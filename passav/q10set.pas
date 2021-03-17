$TITLE Q10SET - quick pass set expression evaluation
module q10set;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM p10cg.typ
$SYSTEM pastal.inc
$SYSTEM pasifu.inc
$SYSTEM q10gen.inc
$SYSTEM p10cgu.inc
$SYSTEM p10opc.inc
$SYSTEM q10exp.inc
$SYSTEM q10cll.inc
$SYSTEM pasmth.inc
$SYSTEM q10dsc.typ
$SYSTEM q10dsc.inc
  
(* set descriptor type.  There are three formats:
  
  "z" format: null set.
  
      nargs = 0
      other fields unused
  
  "o" format: singleton or range set.
  
      nargs = 2
      arg[1] = addr descriptor of lower bound
      arg[2] = addr descriptor of upper bound
      lwb_exp = pointer to integral-valued expression tuple for lowerbound
                  (must be operand of a genset tuple).
      upb_exp = pointer to similar tuple for upper bound.
  
  "l" format:
  
      nargs = 3
      arg[1] = addr descriptor for location of set
      arg[2] = addr descriptor for lower bound
      arg[3] = addr descriptor for length
      upb_exp = pointer to set-valued expression tuple whose value
                          is represented by this descriptor.  *)
  
type
  set_desc = packed record
    nargs: 0..3;
    lwb_exp,
    upb_exp: expr;
    arg: array [1..3] of addr_desc
  end;
  
function do_set_fetch (exp: expr; ctxt_lwb, ctxt_len: set_range): set_desc; forward;
  
(* Sets whose lengths are not constant (known at compile time), but could exceed 30 words,
   will be handled in dynamic rather than static temporaries.  *)
  
public const
  set_dynamic_cutover: set_range = 1080; (*  30 * 36  *)
  
public var
  rtime_upb: addr_desc;
  stemps_dynamic: boolean;
 
var
  have_freed_temp: boolean;
  freed_temp: set_desc;
  ltz_check_of_rtime_upb_reqd: boolean;
$PAGE mask tables

type rmask_table = array[0..36] of machine_word;

const rmasks: rmask_table :=
     (  000000000000B,
        000000000001B,
        000000000003B,
        000000000007B,
        000000000017B,
        000000000037B,
        000000000077B,
        000000000177B,
        000000000377B,
        000000000777B,
        000000001777B,
        000000003777B,
        000000007777B,
        000000017777B,
        000000037777B,
        000000077777B,
        000000177777B,
        000000377777B,
        000000777777B,
        000001777777B,
        000003777777B,
        000007777777B,
        000017777777B,
        000037777777B,
        000077777777B,
        000177777777B,
        000377777777B,
        000777777777B,
        001777777777B,
        003777777777B,
        007777777777B,
        017777777777B,
        037777777777B,
        077777777777B,
        177777777777B,
        377777777777B,
        -1              );

type lmask_table = array[0..18] of integer;

const lmasks: lmask_table :=
     (  000000B,
        400000B,
        600000B,
        700000B,
        740000B,
        760000B,
        770000B,
        774000B,
        776000B,
        777000B,
        777400B,
        777600B,
        777700B,
        777740B,
        777760B,
        777770B,
        777774B,
        777776B,
        777777B  );
$PAGE set_free, l_format_arg, o_format_arg
  
(* SET FREE frees each of the addr_desc's used in a set descriptor.  *)
  
public procedure set_free (sdesc: set_desc);
  
var
  j: 1..3;
  
begin
  with sdesc do
    for j := 1 to nargs do
      free (arg[j])
end;
  
  
(* L FORMAT ARG generates an L-format argument list for a runtime call.  *)
  
public procedure l_format_arg (arg_1, arg_2, arg_3: addr_desc);
  
begin
  gen_rm (arg, 0, arg_1);
  gen_rm (arg, 0, arg_addr (arg_2));
  gen_rm (arg, 0, arg_addr (arg_3))
end;
  
  
(* O FORMAT ARG generates an O-format argument list for a runtime call.  *)
  
public procedure o_format_arg (arg_1, arg_2: addr_desc);
  
  function non_immed (arg: addr_desc): addr_desc;
    begin
      if arg.immediate then begin
	non_immed := absolute_reference;
	non_immed.reloc := gen_cint (arg.offset)
      end
      else non_immed := arg
    end;
  
begin
  gen_rm (arg, 0, non_immed (arg_1));
  gen_rm (arg, 0, non_immed (arg_2))
end;
$PAGE set_elem_reference
(* SET ELEM REFERENCE extracts or replaces a desired element from a set.  The number of the 
   desired element is assumed to already be residing in register ELEM REG.  The contents
   of ELEM REG will be replaced by a byte pointer for the corresponding element
   of the set, which will be loaded into register RESULT REG.  *)
  
public procedure set_elem_reference (var set_loc: set_desc;
				       elem_reg: registers;
				       result_reg: registers;
				       minval,
				       maxval:   int_type;
				       opcode:       opc_range);
  
var
  reg: registers;
  test_upb, test_lwb: boolean;
  set_len: set_range;
  
begin
  assert ((opcode = ldb) or (opcode = dpb)); (* the only things handled properly *)
  
  (* first, ensure that the set's addr_desc isn't indirect, as adjusting
     a byte pointer whose address portion uses indirection won't work
     (except when the adjustment doesn't cross into the next word).  *)
  
  if set_loc.arg[1].indirect then begin
    free (set_loc.arg[1]);
    reg := get_reg (36);
    gen_rm (movei, reg, set_loc.arg[1]);
    set_loc.arg[1] := absolute_reference;
    set_loc.arg[1].index := reg  (* form 0(reg) *)
  end;
  
  (* Normalize the desired element number to the set's lower bound, and
     perform any required range checking.  *)
  
  if set_loc.arg[2].offset > 0 then
    gen_ri (subi, elem_reg, set_loc.arg[2].offset); (* normalize to set's lwb *)
  test_upb := not aconstp (set_loc.arg[3], set_len)
		     orif ((maxval - set_loc.arg[2].offset) >= set_len);
  test_lwb := minval < set_loc.arg[2].offset;
  if test_lwb or test_upb then (* must check it before doing adjbp *) begin
    if opcode = ldb then
      gen_ri (movei, result_reg, 0); (* in case range check fails *)
    if test_lwb then
      if test_upb then
	gen_ri (cai+ltc, elem_reg, 0)
      else (* test for negative only *)
	gen_ri (cai+gec, elem_reg, 0);
    if test_upb then
      gen_rm (cam+ltc, elem_reg, set_loc.arg[3]);
    gen (jrst, 0, 0, 3, dot)
  end;
  
  set_loc.arg[1] := increment_addr (set_loc.arg[1], 0, 0, 1);
    if prog_options.ki_code_opt then
      begin gen_rt(jsr,0,rt_inst_sml);
        gen (adj_bp, elem_reg, 0, 0, gen_bptr (set_loc.arg[1])) 
      end
    else gen (adjbp, elem_reg, 0, 0, gen_bptr (set_loc.arg[1]));
  gen_rr (opcode, result_reg, elem_reg)
end;
$PAGE sets_compatible, longset_join
(* SETS COMPATIBLE determines if two sets have identical lowerbounds and lengths.  *)
  
function sets_compatible (first_set, second_set: set_desc): boolean;
begin
  sets_compatible := (first_set.arg[2].offset = second_set.arg[2].offset) and
                               addr_equal (first_set.arg[3], second_set.arg[3])
end;
  
  
  
(* LONGSET JOIN performs union, intersection, or difference on long sets of
   compatible lower bound and length using an in-line loop to avoid the overhead
   of a runtime call.  *)
  
procedure longset_join (operation:       opc_range;
                        target, source:  addr_desc;
                        bit_width:       addr_desc);
  
var
  reg: registers;
  const_width: set_range;
  
begin
  reg := get_reg (36);
  gen_rm (movei, reg, target);
  if aconstp (bit_width, const_width) then
    gen_ri (hrli, reg, 1000000b - ((const_width + 35) div 36))
  else begin
    (* we should only be here for a dynamic temp, in which case we (should) know
       that the width is of the form "1(reg)". *)
    with bit_width do
      assert ((offset = 1) and immediate and (reloc.kind = absolute_sc));
    gen (movni, 0, bit_width.index, 36, none);
    gen_ri (idivi, 0, 36);
    gen_rr (hrl, reg, 0)
  end;
  gen_rm (movei, 1, source);
  gen_rx (move, 0, 1);
  gen_rx (operation, 0, reg);
  gen_ri (addi, 1, 1);
  gen (aobjn, reg, 0, -3, dot);
  decr_reg_usages (reg)
end;
$PAGE force_out_of_reg, internal_set_free
  
(* FORCE OUT OF REG takes a set descriptor as input, determines whether the described
   set resides in registers, and if so forces the set into a temporary and updates
   the set descriptor accordingly.  *)
  
public procedure force_out_of_reg (var sdesc: set_desc);
  
var
  reg: registers;
  
begin
  if is_register (sdesc.arg[1]) then (* cannot be in reg for runtime call *) begin
    reg := sdesc.arg[1].offset;
    sdesc.arg[1] := get_temp ((sdesc.arg[3].offset + 35) div 36);
    store (reg, sdesc.arg[1], sdesc.arg[3].offset, left_aligned);
    decr_reg_usages (reg)
  end
end;
  
  
(* INTERNAL SET FREE intercepts frees of temporaries that could be reused.  For example,
   a union of two operands both in (dynamic or static) temps would put the result into one
   of them, and free the other.  The next time a temp must be allocated, the previously
   "freed" one can be reused.  This game is only played within routines that are only
   reachable from set_fetch, not within routines externally reachable.  I.e. make_set
   and the routines it calls just use set_free.  *)
  
procedure internal_set_free (sdesc: set_desc);
  
begin
  if (sdesc.nargs = 3) andif (sdesc.arg[1].reloc.kind = temp_sc) andif not have_freed_temp then begin
    freed_temp := sdesc;
    have_freed_temp := true
  end
  else
    set_free (sdesc)
end;
$PAGE allocate_temp
  
(* ALLOCATE TEMP obtains static or dynamic space on the stack for a temporary. *)
  
procedure allocate_temp (var temp_addr, length_addr: addr_desc);
  
var
  reg: registers;
  
begin
  if have_freed_temp then begin
    if stemps_dynamic then begin
      assert (freed_temp.arg[3].index = rtime_upb.offset);
      assert (freed_temp.arg[3].offset = 1);
      length_addr := freed_temp.arg[3]
    end
    else
      assert (addr_equal (freed_temp.arg[3], length_addr));
    temp_addr := freed_temp.arg[1];
    have_freed_temp := false;
    return  (* <--- return *)
  end;
  
  if stemps_dynamic then begin
    if ltz_check_of_rtime_upb_reqd (* less than zero check required *) then begin
      if is_register (rtime_upb) then
        gen_rm (cam+gec, rtime_upb.offset, int_value (0))
      else begin
        free (rtime_upb);
        reg := get_reg (36);
        gen_rm (skip+gec, reg, rtime_upb); (* test it and load it *)
        rtime_upb := reg_addr (reg)
      end;
      gen_ri (movei, rtime_upb.offset, 0);
      ltz_check_of_rtime_upb_reqd := false (* only need to do it once *)
    end
    else if not is_register (rtime_upb) then begin
      reg := load_addr (rtime_upb, left_aligned, 36);
      rtime_upb := reg_addr (reg)
    end;
    temp_addr := alc_dynamic_temp (rtime_upb, 1, 1);
    length_addr := duplicate_addr (rtime_upb);
    (* Have upb in reg; the length is upb + 1 since sets in dynamic temps are zero-based.
       Construct addr_desc for the immediate value 1(reg).  *)
    with length_addr do begin
      index := offset;
      offset := 1;
      immediate := true;
      reloc.kind := absolute_sc
    end
  end
  else
    temp_addr := get_temp ((length_addr.offset + 35) div 36)
end;
$PAGE left_mask
(* LEFT MASK masks a register (pair) zeroing all but the leftmost "n" bits.
   The precision ("p") denotes the number of left aligned bits which may be
   nonzero (i.e. any remaining bits are known to be zero);  a value of p less
   than 36 denotes a single register versus a pair.  *)

public procedure left_mask (reg: registers; p: bit_range; n: bit_range);
  
var word: pdp10word;
  
begin
  if p <= n then
    return                              (* bits to zero are already zero *)

  else if n = 0 then                    (* zero register (pair) *)
    clear (reg)

  else if p > 36 then begin             (* part of mask applies to all of one register *)
    if n <= 36 then begin
      gen_ri (setz, reg+1, 0);
      left_mask (reg, 36, n);
    end
    else
      left_mask (reg+1, p-36, n-36)
  end

  else begin
    if (n >= 18) then
      gen_ri (andcmi, reg, rmasks[36-n])
    else if (p > 17) then begin (* must zero low order bits too *)
      word.value := rmasks [36-n];
      gen (andcm, reg, 0, 0, gen_cword (word, setword))
    end
    else
      gen_ri (tlz, reg, rmasks[18-n])
  end
end;
$PAGE convert_set
(* CONVERT SET performs lowerbound and length conversions on sets.  *)

procedure convert_set (var sdesc: set_desc;
                           ctxt_lwb, ctxt_len: set_range);

var
  reg, old_reg:                 registers;
  target_regsize,
  source_regsize, regsize,
  real_source_length, slen:     set_range;
  temp_addr, length_addr:       addr_desc;
  word_offset:                  unit_range;

begin
  length_addr := int_value (ctxt_len); 
  
  (* try and perform some lower bound conversions at compile time *)
  
  if aconstp (sdesc.arg[3], slen) andif (sdesc.arg[2].offset < ctxt_lwb) then begin
    word_offset := (ctxt_lwb - sdesc.arg[2].offset) div 36;
    if word_offset > 0 then begin
      sdesc.arg[1] := increment_addr (sdesc.arg[1], word_offset, 0, 36);
      sdesc.arg[2].offset := sdesc.arg[2].offset + (36 * word_offset);
      sdesc.arg[3].offset := sdesc.arg[3].offset - (36 * word_offset)
    end
  end;
  
  (* long *)
  
  if (ctxt_len > 72)  orif  (not aconstp (sdesc.arg[3], slen) orif (slen > 72)) then begin
    allocate_temp (temp_addr, length_addr);
    if not stemps_dynamic andif aconstp (sdesc.arg[3], slen) andif
         ((ctxt_lwb = sdesc.arg[2].offset) and (ctxt_len = slen)) then
      do_blt (sdesc.arg[1], duplicate_addr (temp_addr), int_value ((ctxt_len + 35) div 36))
    else begin
      force_out_of_reg (sdesc);  (* runtime cannot accept arg in reg *)
      gen_rt (pushj, sb, rt_smv_ll);
      l_format_arg (temp_addr, int_value (ctxt_lwb), length_addr);
      l_format_arg (sdesc.arg[1], sdesc.arg[2], sdesc.arg[3]);
      set_free (sdesc)
    end;
    sdesc.arg[1] := temp_addr
  end
  
  (* short *)
  
  else begin
    target_regsize := ngm (ctxt_len, 36);
    source_regsize := ngm (sdesc.arg[3].offset, 36);
    regsize := target_regsize;          (* this is size of register to load initially *)

    if source_regsize > target_regsize then     (* 72 -> 36 *)
        regsize := 72;          (* must load 2 words then shift *)

    if is_register (sdesc.arg[1]) then
      old_reg := sdesc.arg[1].offset
    else
      old_reg := noreg;
    if (old_reg = noreg) orif
       ((regsize > 36) and ((regdesc[old_reg].associate = noreg) and
                            (regdesc[old_reg + 1].uses_remaining > 0))) then begin
      (* either the source isn't in a register yet, or else it is but I need
         two and the next one upwards isn't available.  *)
      free (sdesc.arg[1]);
      reg := get_reg (regsize);
      if source_regsize <= 36 then
        gen_rm (move, reg, sdesc.arg[1])
      else
        gen_rm (dmove, reg, sdesc.arg[1])
    end
    else if (regsize > 36) and (regdesc[old_reg].associate = noreg) then begin
      tag_reg (old_reg, old_reg + 1); (* need 2nd register and next one is available *)
      regdesc[old_reg + 1].uses_remaining := 1;
      reg := old_reg
    end
    else
      reg := old_reg; (* its just fine where its at *)
  
    if source_regsize < regsize         (* 36 -> 72, must extend *)
      then gen_ri (setz, reg+1, 0);
    left_shift (reg, regsize, ctxt_lwb - sdesc.arg[2].offset);  (* make lwb's the same *)
  
    (* determine real length of source, and if necessary mask out bits that lie
       above the upper bound of the target.  *)
  
    with sdesc.upb_exp^.desc do
      real_source_length := max (0, min (target_regsize, set_length + set_lwb - ctxt_lwb));
    left_mask (reg, real_source_length, ctxt_len);

    if regsize > target_regsize then    (* 72 -> 36, 2 words loaded, 1 word result *)
      free_and_disassociate (reg+1);
    sdesc.arg[1] := reg_addr (reg)
  end;
  
  sdesc.arg[2] := int_value (ctxt_lwb);
  sdesc.arg[3] := length_addr
end;
$PAGE genset_op_bounds
  
(* GENSET OP BOUNDS determines the range of possible values for an integral valued
   expression tuple occurring as an operand of a GENSET tuple.  *)
  
procedure genset_op_bounds (    int_valued_exp: expr;
                            var min_val, max_val: int_type;
                            var fixed: boolean);

procedure worst_case (var tmin, tmax: int_type);
  begin
    if int_valued_exp^.desc.signed then
      tmin := minimum (int_type)
    else
      tmin := 0;
    if int_valued_exp^.desc.int_prec >= int_bits (maximum (int_type)) then
      tmax := maximum (int_type)
    else
      tmax := 2**int_valued_exp^.desc.int_prec - 1
  end;
  
var
  temp_min, temp_max: int_type;

begin
  fixed := false; (* assume expression isn't constant *)
  with int_valued_exp^ do
    case opcode of

      cst_ref: begin
        min_val := cst_val.ival;
        max_val := cst_val.ival;
        fixed := true
      end;
      ident_ref:
        with id_sym^.type_desc^ do begin
          min_val := minval;
          max_val := maxval
        end;
      func_call_op:
        with subr^.desc.base^.return_type^ do begin
          min_val := minval;
          max_val := maxval
        end;
      field_ref:
        with field_sym^.type_desc^ do begin
          min_val := minval;
          max_val := maxval
        end;
      ptr_ref:
        with base_ptr^.desc.base^.target_type^ do begin
          min_val := minval;
          max_val := maxval
        end;
      array_ref: 
        with base_array^.desc.base^.element_type^ do begin
          min_val := minval;
          max_val := maxval
        end;
      buffer_ref:
        with base_file^.desc.base^.component_type^ do begin
          min_val := minval;
          max_val := maxval
        end;
      sclcvt_op: begin
	genset_op_bounds (operand[1], min_val, max_val, fixed);
	worst_case (temp_min, temp_max);
	min_val := max (min_val, temp_min);
	max_val := min (max_val, temp_max)
      end;
    ers:
	worst_case (min_val, max_val)
    end (* case *);
   
end (* genset_op_bounds *);
$PAGE gen_singleton_set
(* GEN SINGLETON SET generates a set containing a single element.  *)

procedure gen_singleton_set (var sdesc: set_desc;
                                 ctxt_lwb, ctxt_len: set_range);

var
  reg, idx_reg:         registers;
  temp_addr,
  length_addr:           addr_desc;
  const_elem,
  minval, maxval:       int_type;
  check_required, fixed: boolean;

procedure set_check (reg: registers);
  begin
    if (maxval <= ctxt_lwb + ctxt_len - 1) then
      gen_rm (cam+gec, reg, int_value (ctxt_lwb))
    else begin
      if (minval < ctxt_lwb) then
        gen_rm (cam+ltc, reg, int_value (ctxt_lwb));
      gen_rm (cam+lec, reg, int_value (ctxt_lwb + ctxt_len - 1))
    end
  end;
  
begin
  length_addr := int_value (ctxt_len);
  if ctxt_len > 72 then begin
    allocate_temp (temp_addr, length_addr);
    gen_rt (pushj, sb, rt_smv_lo);
    l_format_arg (temp_addr, int_value (ctxt_lwb), length_addr);
    o_format_arg (sdesc.arg[1], sdesc.arg[2]);
    set_free (sdesc);
    sdesc.arg[1] := temp_addr
  end
  
  else (* short *) begin
    if aconstp (sdesc.arg[1], const_elem) then begin (* constant element *)
      reg := get_reg (ctxt_len);
      const_elem := const_elem - ctxt_lwb;
      if (const_elem < 0) or (const_elem >= ctxt_len) then begin
        if ctxt_len > 36 then
          gen_rr (setzb, reg, reg+1)
        else
          gen_ri (movei, reg, 0)
      end
      else if const_elem >= 36 then begin
        gen_ri (movei, reg, 0);
        if const_elem >= 54 then
          gen_ri (movei, reg+1, 2**(71-const_elem))
        else
          gen_ri (hrlzi, reg+1, 2**(53-const_elem))
      end
      else (* const_elem < 36 *) begin
        if ctxt_len > 36 then
          gen_ri (movei, reg+1, 0);
        if const_elem >= 18 then
          gen_ri (movei, reg, 2**(35-const_elem))
        else
          gen_ri (hrlzi, reg, 2**(17-const_elem))
      end
    end
  
    else begin (* not constant element *)
      free (sdesc.arg[2]);
      genset_op_bounds (sdesc.lwb_exp, minval, maxval, fixed);
      check_required := (ctxt_lwb > minval) or (maxval > ctxt_lwb + ctxt_len - 1);
      if ctxt_len <= 36 then begin
        reg := load_addr (sdesc.arg[1], left_aligned, 36);
        if check_required then begin
          set_check (reg);
	  gen_ri (hrrei, reg, ctxt_lwb - 1)
        end;
        if prog_options.overlay_opt then begin
          gen_rt (add, reg, rt_mask_ss);
          gen (move, reg, reg, -ctxt_lwb, none)
        end
        else
          gen (move, reg, reg, -ctxt_lwb, relrt (rt_mask_ss));
      end

      else (* ctxt_len > 36 *) begin
        if check_required then begin
          idx_reg := load_addr (sdesc.arg[1], left_aligned, 36);
          gen_rr (movn, 1, idx_reg)
        end
        else begin
          gen_rm (movn, 1, sdesc.arg[1]);
          free (sdesc.arg[1])
        end;
        reg := get_reg (72);
        if prog_options.overlay_opt then
          genind (dmove, reg, 0, 0, relrt (rt_mask_sd1))
        else
          gen_rt (dmove, reg, rt_mask_sd1);
        gen (lshc, reg, 1, ctxt_lwb, none);
        if check_required then begin
          set_check (idx_reg);
          gen_rr (setzb, reg, reg + 1);
          decr_reg_usages (idx_reg)
        end
      end
    end;
    sdesc.arg[1] := reg_addr(reg)
  end (* short *);
  
  sdesc.nargs := 3;
  sdesc.arg[2] := int_value (ctxt_lwb);
  sdesc.arg[3] := length_addr
end;
$PAGE set_ce
(* SET CE is a utility procedure for get_range_set.  It creates a set of the form
   [cstlwb..(exp+cstoffset)] converted to some specified lwb and length. *)

function set_ce (ctxt_lwb, ctxt_len: set_range; 
                 cstlwb:        set_range;      
                 maddr:         addr_desc;
                 upb_exp:       expr;
                 cstoffset:     int_type): registers; 
var
  idx_reg: registers;
  minval, maxval: int_type;
  fixed: boolean;

begin
  genset_op_bounds (upb_exp, minval, maxval, fixed);
  if (minval < cstlwb) or (maxval > (ctxt_lwb + ctxt_len - 1)) then begin
    idx_reg := load_addr (maddr, left_aligned, 36);
    gen_rr (movn, 1, idx_reg);
    if minval < cstlwb then begin       (* force exp to be in [cstlwb .. upb] *)
      gen_rm (cam+gec, idx_reg, int_value (cstlwb));
      gen_ri (movei, 1, - (cstlwb - 1)) (* desired result will be null set *)
    end;
    if maxval > ctxt_lwb + ctxt_len - 1 then begin
      gen_rm (cam+lec, idx_reg, int_value (ctxt_lwb + ctxt_len - 1));
      gen_ri (movei, 1, -(ctxt_lwb + ctxt_len - 1))
    end;
    decr_reg_usages (idx_reg)
  end
  else begin
    gen_rm (movn, 1, maddr);
    free (maddr)
  end;
  
  set_ce := get_reg (ctxt_len);
  if ctxt_len <= 36 then begin  (* create cstlwb..exp 1's *)
    gen_ri (seto, set_ce, 0);
    gen (lsh, set_ce, 1, 35+cstlwb-cstoffset, none)
  end
  else (* double word case *) begin
    gen_ri (setob, set_ce, set_ce+1);   (* mask is all ones *)
    gen (lshc, set_ce, 1, 71+cstlwb-cstoffset, none)
  end;
  right_shift (set_ce, ctxt_len, cstlwb-ctxt_lwb)       (* insert lwb..cstlwb-1 0's *)
end;
$PAGE half_set
(* HALF SET is a helper for gen_range_set to form short constant sets *)

procedure half_set (lwb, upb: int_type; reg: registers);

begin
  if (upb < 0) or (lwb >= 36) then (* null *)
    gen_ri (movei, reg, 0)
  
  else if lwb <= 0 then begin (* left justified *)
    if upb >= 35 then
      gen_ri (seto, reg, 0)
    else if upb < 18 then
      gen_ri (hrlzi, reg, lmasks[upb+1])
    else
      gen_ri (hrroi, reg, lmasks[upb-17])
  end
  
  else if upb >= 35 then begin (* right justified *)
    if lwb < 18 then
      gen_ri (hrloi, reg, rmasks[18-lwb])
    else
      gen_ri (movei, reg, rmasks[36-lwb])
  end
  
  else if lwb >= 18 then (* within right half *)
    gen_ri (movei, reg, rmasks[ upb-lwb+1 ] * (2**(35-upb)) )
 
  else if upb < 18 then (* within left half *)
    gen_ri (hrlzi, reg, rmasks[ upb-lwb+1 ] * (2**(17-upb)) )
 
  else begin (* across halves *)
    gen_ri (hrloi, reg, rmasks[18-lwb]);
    gen_ri (andcmi, reg, rmasks[35-upb])
  end
end;
$PAGE gen_range_set
(* GEN RANGE SET constructs a set of the form [op1..op2].  *)

procedure gen_range_set (var sdesc: set_desc;
                             ctxt_lwb, ctxt_len: set_range);

var
  reg, reg2, mreg:      registers;
  mlen:                 set_range;
  const_lwb, const_upb: int_type;
  word:                 pdp10word;
  temp_addr, length_addr: addr_desc;

begin
  length_addr := int_value (ctxt_len);
  if ctxt_len > 72 then begin
    allocate_temp (temp_addr, length_addr);
    gen_rt (pushj, sb, rt_smv_lo);
    l_format_arg (temp_addr, int_value (ctxt_lwb), length_addr);
    o_format_arg (sdesc.arg[1], sdesc.arg[2]);
    set_free (sdesc);
    sdesc.arg[1] := temp_addr
  end
  
  else if aconstp (sdesc.arg[1], const_lwb) (* constant lwb *) then begin
    if aconstp (sdesc.arg[2], const_upb) (* constant upb *) then begin
      const_lwb := max (0, const_lwb - ctxt_lwb);
      const_upb := min (const_upb - ctxt_lwb, ctxt_len - 1);
      reg := get_reg (ctxt_len);
      if (const_lwb = 0) and (const_upb = 71) then 
        gen_ri (setob, reg, reg+1)
      else begin
        half_set (const_lwb, const_upb, reg);
        if ctxt_len > 36 then
          half_set (const_lwb - 36, const_upb - 36, reg + 1)
      end;
    end
    else (* constant lwb, variable upb *)
      reg := set_ce (ctxt_lwb, ctxt_len, max (ctxt_lwb, const_lwb),
                     sdesc.arg[2], sdesc.upb_exp, 0);
      sdesc.arg[1] := reg_addr (reg)
  end
  
  else if aconstp (sdesc.arg[2], const_upb) (* constant upb (variable lwb) *) then begin
    reg := set_ce (ctxt_lwb, ctxt_len, ctxt_lwb,
                   sdesc.arg[1], sdesc.lwb_exp, -1);    (* get [lwb..op1-1] mask *)

    (* Form set by performing reg := (not reg) * [lwb..min (const_lwb, upb)] *)

    mreg := reg;
    mlen := max (0, min (ctxt_len, const_upb - ctxt_lwb + 1));
    if ctxt_len > 36 then       (* handle two word case *)
      if mlen <= 36 then
        gen_ri (setz, mreg+1, 0)        (* second word masked out *)
      else begin
        gen_ri (setca, mreg, 0); (* mask of 1st word all ones *)
        mreg := reg + 1;
        mlen := mlen - 36
      end;
    if mlen = 36 then
      gen_ri (setca, mreg, 0)
    else if mlen >= 18 then
      gen_ri (andcbi, mreg, rmasks[36-mlen])
    else (* mlen < 18 *) begin
      word.value := rmasks [36-mlen];
      gen (andcb, reg, 0, 0, gen_cword (word, setword))
    end;
    sdesc.arg[1] := reg_addr (reg)
  end
  
  else begin                            (* variable lwb and upb *)
    reg := set_ce(ctxt_lwb, ctxt_len, ctxt_lwb, sdesc.arg[1], sdesc.lwb_exp, -1); (* not [op1..upb] *)
    reg2 := set_ce(ctxt_lwb, ctxt_len, ctxt_lwb, sdesc.arg[2], sdesc.upb_exp, 0); (* [lwb..op2] *)
    gen_rr (andca, reg, reg2);
    if ctxt_len > 36 then
      gen_rr (andca, reg+1, reg2+1);
    decr_reg_usages (reg2);
    sdesc.arg[1] := reg_addr (reg)
  end;
  
  sdesc.nargs := 3;
  sdesc.arg[2] := int_value (ctxt_lwb);
  sdesc.arg[3] := length_addr
end;
$PAGE make_set
  
(* MAKE SET is given a set descriptor, a lower bound, and a length.  A set
   with the desired value and bounds is created (or converted).  *)
  
public procedure make_set (var sdesc: set_desc;
                           ctxt_lwb, ctxt_len: set_range;
                           force_temp: boolean);
const
  zero_pdp10word: pdp10word := ('F', 0);
  
var
  const_len: set_range;
  counter: int_type;
  
begin
  with sdesc do
  
    if nargs = 0 then begin
      (* note that this case is only utilized by callers who have to have an
         L format descriptor even if the set is null, which is to say none of
         the routines in this module (which take advantage of null subexpressions
         to avoid operations.)  *)
      sdesc.arg[1] := gen_cst (0);
      counter := ctxt_len - 36;
      while counter > 0 do begin
        gen_word (cst_area, zero_pdp10word, setword);  (* additonal word *)
	counter := counter - 36
      end;
      sdesc.arg[2] := int_value (0);
      sdesc.arg[3] := int_value (0);
      nargs := 3
    end
  
    else if nargs = 2 then
      if addr_equal (arg[1], arg[2]) then
        gen_singleton_set (sdesc, ctxt_lwb, ctxt_len)
      else
        gen_range_set (sdesc, ctxt_lwb, ctxt_len)
  
    else if nargs = 3 then begin
      if (ctxt_lwb <> sdesc.arg[2].offset)
       orif
         ((ctxt_len <= 72) andif
           ((ngm (sdesc.arg[3].offset, 36) <> ngm (ctxt_len, 36))
              orif ((ctxt_len mod 36 > 0) andif (sdesc.upb_exp^.desc.set_length > ctxt_len))))
       orif
         (((ctxt_len > 72) and force_temp) andif
           ((sdesc.arg[1].reloc.kind <> temp_sc)
                        orif
            (aconstp (sdesc.arg[3], const_len) andif
                (stemps_dynamic or (const_len <> ctxt_len))))) then
        convert_set (sdesc, ctxt_lwb, ctxt_len)
    end
  
    else
      assert (false)
end;
$PAGE clear_set
  
(* CLEAR SET generates code to null_out a set.  *)
  
procedure clear_set (lhs: expr; var laddr: addr_desc);
  
var
  set_size, i: int_type;
  temp_addr: addr_desc;
  
begin
  set_size := (lhs^.desc.set_length + 35) div 36;
  
  if (set_size > 0) and (set_size <= 3) then begin
    gen_rm (setzm, 0, laddr);
    for i := 2 to set_size do begin
      laddr := increment_addr (laddr, 1, 0, 36);
      gen_rm (setzm, 0, laddr)
    end;
    free (laddr)
  end
  
  else if set_size = 4 then begin
    gen_ri (setzb, 0, 1);
    gen_rm (dmovem, 0, laddr);
    laddr := increment_addr (laddr, 2, 0, 36);
    gen_rm (dmovem, 0, laddr);
    free (laddr)
  end
  
  else if set_size >= 5 then begin
    gen_rm (setzm, 0, laddr);
    temp_addr := increment_addr (duplicate_addr (laddr), 1, 0, 36);
    do_blt (laddr, temp_addr, int_value (set_size - 1))
  end
end;
$PAGE shape_set
  
(* SHAPE SET is passed a set expression tuple.  It propagates lwb and length
   information upwards from the leaves of the expression tree, storing the
   information in the type descriptor fields of the expression tuples. 
  
   For (sub)expressions like [I..J] the length information is worst case, based on the
   declared range (in this case) of J.  The set_cst_len fields of the nodes' type
   descriptors are used to propagate whether a set expression has the indicated length,
   or if the length is variable and the indicated length is only a maximum.  *)
  
public procedure shape_set (    exp: expr;
                            var shaped_lwb, shaped_len: set_range);
  
var
  temp_lwb, temp_len, upb_plus_1, temp_upb_plus_1: set_range;
  i: oper_range;
  fixed: boolean;
  
procedure shape_gen_set_operand (    int_valued_exp: expr;
                                 var lwb, len: set_range;
                                 var fixed: boolean);
  
  var
    min_val, max_val: int_type;
  
  begin
    genset_op_bounds (int_valued_exp, min_val, max_val, fixed);
    if (min_val > set_upb_limit) orif
       (max_val < 0) orif (max_val < min_val) then (* null set *) begin
      lwb := 0;
      len := 0;
      fixed := true
    end
    else begin
      lwb := max (0, min_val);
      len := min (set_upb_limit, max_val) - lwb + 1;
    end
  end;
  
  
begin (* shape_set *)
  with exp^ do begin
    case opcode of
  
      cst_ref,
      ident_ref,
      field_ref,
      buffer_ref,
      ptr_ref,
      array_ref,
      func_call_op: begin (* note: only sets handled here *)
        shaped_lwb := desc.set_lwb;  (* node already properly marked *)
        shaped_len := desc.set_length;
        desc.set_cst_len := true
      end;
  
      setcvt_op: begin
        shape_set (operand[1], shaped_lwb, shaped_len);
        desc.set_cst_len := operand[1]^.desc.set_cst_len
      end;
  
      gen_set_op: begin
        if upperbound (operand) = 0 then begin
          shaped_lwb := 0;
          shaped_len := 0;
          desc.set_cst_len := true
        end
        else if upperbound (operand) = 1 then
          shape_gen_set_operand (operand[1], shaped_lwb, shaped_len, fixed)
        else (* upperbound (operand) = 2 *) begin
          shape_gen_set_operand (operand[1], shaped_lwb, shaped_len, fixed);
          shape_gen_set_operand (operand[2], temp_lwb, temp_len, fixed);
          if (temp_lwb + temp_len) > shaped_lwb then begin
            shaped_len := temp_lwb + temp_len - shaped_lwb;
            desc.set_cst_len := fixed
          end
          else (* null set *) begin
            shaped_lwb := 0;
            shaped_len := 0;
            desc.set_cst_len := true
          end
        end
      end;
  
      diff_op: begin
        shape_set (operand[1], shaped_lwb, shaped_len);
        shape_set (operand[2], temp_lwb, temp_len);
        desc.set_cst_len := operand[1]^.desc.set_cst_len
      end;
  
      both_op: begin
        shape_set (operand[1], temp_lwb, temp_len);
        shape_set (operand[2], shaped_lwb, shaped_len);
        upb_plus_1 := min (shaped_lwb + shaped_len, temp_lwb + temp_len);
	desc.set_cst_len := (operand[1]^.desc.set_cst_len andif 
					      (upb_plus_1 = temp_lwb + temp_len))
				orif
			    (operand[2]^.desc.set_cst_len andif
					      (upb_plus_1 = shaped_lwb + shaped_len));
        shaped_lwb := max (shaped_lwb, temp_lwb);
        if upb_plus_1 > shaped_lwb then
          shaped_len := upb_plus_1 - shaped_lwb
        else (* null set *) begin
          shaped_len := 0;
          shaped_lwb := 0;
          desc.set_cst_len := true
        end
      end;
  
      union_op: begin
        shaped_lwb := set_upb_limit;
        upb_plus_1 := 0;
        for i := 1 to upperbound (operand) do begin
          shape_set (operand[i], temp_lwb, temp_len);
          if temp_len > 0 (* non null *) then begin
            shaped_lwb := min (shaped_lwb, temp_lwb);
            temp_upb_plus_1 := temp_lwb + temp_len;
            if temp_upb_plus_1 > upb_plus_1 then begin
              upb_plus_1 := temp_upb_plus_1;
              desc.set_cst_len := operand[i]^.desc.set_cst_len
            end
            else if (temp_upb_plus_1 = upb_plus_1) andif operand[i]^.desc.set_cst_len then
              desc.set_cst_len := true
          end
        end;
        if upb_plus_1 > shaped_lwb then
          shaped_len := upb_plus_1 - shaped_lwb
        else begin
          shaped_len := 0;
          shaped_lwb := 0;
          desc.set_cst_len := true
        end
      end;
  
      others:
        assert (false)
    end (* case *);
  
    desc.set_lwb := shaped_lwb;
    desc.set_length := shaped_len
  end (* with *);
end (* shape_set *);
$PAGE do_set_difference
  
(* DO SET DIFFERENCE compiles a set difference expression tuple, and returns a
   set descriptor for its value.  *)
  
function do_set_difference (diff_exp: expr;
                            ctxt_lwb, ctxt_len: set_range): set_desc;
  
var
  first_op, second_op: set_desc;
  reg: registers;
  min_val, max_val: integer;
  fixed: boolean;
  
begin
  with diff_exp^ do begin
  
    first_op := do_set_fetch (operand[1], ctxt_lwb, ctxt_len);
    second_op := do_set_fetch (operand[2], ctxt_lwb, ctxt_len);
  
    
    (* if the first operand's range is disjoint from the contextual range, do_set_fetch
       will return the null set for it.  *)
    if (desc.set_length = 0) orif (first_op.nargs = 0) then begin
      do_set_difference.nargs := 0; (* result is unavoidably null *)
      do_set_difference.upb_exp := diff_exp;
      internal_set_free (first_op);
      internal_set_free (second_op)
    end
  
    else if (second_op.nargs = 0) orif
            (operand[1]^.desc.set_lwb >= 
                  (operand[2]^.desc.set_lwb + operand[2]^.desc.set_length)) orif
            (operand[2]^.desc.set_lwb >= 
                  (operand[1]^.desc.set_lwb + operand[1]^.desc.set_length)) then begin
      do_set_difference := first_op; (* second op is irrelevant to result *)
      internal_set_free (second_op)
    end
  
  
    else begin (* have to really do it *)
      make_set (first_op, ctxt_lwb, ctxt_len, true (* force into temporary *));
      do_set_difference := first_op;
      do_set_difference.upb_exp := diff_exp;
      (* update shaped length of 2nd operand to prevent make_set from masking any excess
         bits that are above the contextual upper bound.  They can't affect the result. Note that this 
         is only a concern if the 2nd op's set already exists.  If it doesn't, make_set will cause
         its creation, which will take place within context.  *)
      if second_op.nargs = 3 then with second_op.upb_exp^.desc do
        set_length := ctxt_lwb + ctxt_len - set_lwb;
  
      if ctxt_len <= 72 then begin
	make_set (second_op, ctxt_lwb, ctxt_len, false);
        reg := load_addr (first_op.arg[1], left_aligned, ctxt_len);
        do_set_difference.arg[1] := reg_addr(reg); (* update set descriptor *)
        gen_rm (andcm, reg, second_op.arg[1]);
        if ctxt_len > 36 then begin
          second_op.arg[1] := increment_addr (second_op.arg[1], 1, 0, 36);
          gen_rm (andcm, reg + 1, second_op.arg[1])
        end;
        (* update shaped length of expression to prevent make_set from later thinking
           it needs to mask out any excess high bits.  Make_set would have masked any
           such bits from the first operand that were beyond the contextual upper
           bound, so the result is guaranteed to be free of them.  *)
        diff_exp^.desc.set_length := ctxt_lwb + ctxt_len - diff_exp^.desc.set_lwb
      end
  
      else if (second_op.nargs = 2) andif addr_equal (second_op.arg[1], second_op.arg[2]) then begin
	genset_op_bounds (second_op.lwb_exp, min_val, max_val, fixed);
	reg := load_addr (second_op.arg[1], left_aligned, 36);
	second_op.arg[1] := reg_addr (reg);
	gen_ri (movei, 1, 0);
	set_elem_reference (first_op, reg, 1, min_val, max_val, dpb)
      end
      else begin
	make_set (second_op, ctxt_lwb, ctxt_len, false);
	if sets_compatible (first_op, second_op) then (* compatible long sets *)
	  longset_join (andcam, first_op.arg[1], second_op.arg[1], first_op.arg[3])

	else (* incompatible long set *) begin
	  force_out_of_reg (second_op);  (* runtime cannot accept arg residing in regs *)
	  gen_rt (pushj, sb, rt_sdf_ll);
	  l_format_arg (first_op.arg[1], first_op.arg[2], first_op.arg[3]);
	  l_format_arg (second_op.arg[1], second_op.arg[2], second_op.arg[3])
	end
      end;
      internal_set_free (second_op)
    end
  
  end (* with *);
end (* do_set_difference *);
$PAGE reverse_if_helpful
  
(* REVERSE IF HELPFUL is used by the intersection and union routines to take advantage of
   the commutativity of their operators.  Both routines will force their first
   operand into registers or temporaries.  This routine will determine if one of the
   operands, but not the other,  will have to be loaded anyway for some sort of conversion,
   and will make that one the first operand.  Thus if one operand can be left alone
   and just accessed from its present location, an unnecessary load to registers or
   temporaries is avoided.  Note that the union_op expr tuple is n-ary, so this game is
   actually played only between its first two (non-null variable) operands.  *)
  
procedure reverse_if_helpful (var first_op, second_op: set_desc;
                              ctxt_lwb, ctxt_len: set_range;
			      put_singleton_last: boolean);
  
var
  temp_op: set_desc;
  const_len: set_range;
  
begin
  (* do_union may want to play a special game with singleton
     sets by utilizing adjbp *)
  
  if put_singleton_last then begin
    if (second_op.nargs = 2) andif addr_equal (second_op.arg[1], second_op.arg[2]) then return;
    if (first_op.nargs = 2) andif addr_equal (first_op.arg[1], first_op.arg[2]) then begin
      temp_op := first_op;
      first_op := second_op;
      second_op := temp_op;
      return
    end
  end;
  
  (* now the more normal cases: *)
  
  if first_op.nargs <> 3 then return;	(* <---  will have to be created anyway *)
  
  if (second_op.nargs <> 3)  (* and first_op.nargs = 3 *)
  
   orif
  
   (* in remaining cases, we know that both set_desc's are the 3-arg form, and so both sets
      actually exist - it only remains to be seen if one, but not the other, would
      require conversion before use  *)
     ((second_op.arg[2].offset <> ctxt_lwb) and (first_op.arg[2].offset = ctxt_lwb))
   orif
     ((ctxt_len <= 72) andif
        ((is_register (second_op.arg[1]) andif not is_register (first_op.arg[1]))
           orif
         (ngm (second_op.arg[3].offset, 36) <> ngm (ctxt_len, 36))
           orif
         ((ctxt_len mod 36 > 0) andif (second_op.upb_exp^.desc.set_length > ctxt_len))))
   orif
     ((ctxt_len > 72) andif
        (((second_op.arg[1].reloc.kind = temp_sc) and (first_op.arg[1].reloc.kind <> temp_sc))
                        orif
         (aconstp (second_op.arg[3], const_len) andif
                (stemps_dynamic or (const_len <> ctxt_len))))) then begin
    temp_op := first_op;
    first_op := second_op;
    second_op := temp_op
  end
end;
$PAGE do_intersection
  
(* DO INTERSECTION compiles a set intersection expression tuple, and returns
   a set descriptor for its value.  *)
  
function do_intersection (intersect_exp: expr;
                          ctxt_lwb, ctxt_len: set_range): set_desc;
  
var
  first_op, second_op: set_desc;
  reg: registers;
  
begin
  with intersect_exp^ do begin
  
    first_op := do_set_fetch (operand[1], ctxt_lwb, ctxt_len);
    second_op := do_set_fetch (operand[2], ctxt_lwb, ctxt_len);
  
    (* if the expression's shaped range is disjoint from the contextual range, it must
       be the case that do_set_fetch will return null for at least one of the operands. *)
    if (desc.set_length = 0) orif (first_op.nargs = 0) orif (second_op.nargs = 0) then begin
      do_intersection.nargs := 0; (* result is unavoidably null *)
      internal_set_free (first_op);
      internal_set_free (second_op)
    end
  
    else begin (* have to really do it *)
      (* Cut down the shaped upperbounds of the operands to the contextual upper
         bound.  This ensures that make_set won't try to mask excess high bits of the
         operands.  If any masking is really required, it will be done on the result
         when the result's user applies make_set to it.  Note that this is only a concern if
         the sets already exist (nargs = 3).  If a set doesn't exist yet, make_set will 
         cause its creation, which will be done within context.  *)
      if first_op.nargs = 3 then with first_op.upb_exp^.desc do
        set_length := ctxt_lwb + ctxt_len - set_lwb;
      if second_op.nargs = 3 then with second_op.upb_exp^.desc do
        set_length := ctxt_lwb + ctxt_len - set_lwb;
  
      reverse_if_helpful (first_op, second_op, ctxt_lwb, ctxt_len, false);
  
      make_set (first_op, ctxt_lwb, ctxt_len, true (* force into temporary *));
      do_intersection := first_op;
      make_set (second_op, ctxt_lwb, ctxt_len, false);
  
      if ctxt_len <= 72 then begin
        reg := load_addr (first_op.arg[1], left_aligned, ctxt_len);
        do_intersection.arg[1] := reg_addr (reg); (* update set descriptor *)
        gen_rm (anda, reg, second_op.arg[1]);
        if ctxt_len > 36 then begin
          second_op.arg[1] := increment_addr (second_op.arg[1], 1, 0, 36);
          gen_rm (anda, reg + 1, second_op.arg[1])
        end
      end

      else if sets_compatible (first_op, second_op) then (* compatible long sets *)
        longset_join (andm, first_op.arg[1], second_op.arg[1], first_op.arg[3])
  
      else (* incompatible long set *) begin
        force_out_of_reg (second_op); (* runtime cannot accept arg residing in regs *)
        gen_rt (pushj, sb, rt_sin_ll);
        l_format_arg (first_op.arg[1], first_op.arg[2], first_op.arg[3]);
        l_format_arg (second_op.arg[1], second_op.arg[2], second_op.arg[3])
      end;
      internal_set_free (second_op)
    end
  
  end (* with *);
  do_intersection.upb_exp := intersect_exp
end (* do_intersection *);
$PAGE do_union
  
(* DO UNION compiles a set union expression tuple, and returns a set descriptor
   for its value.  *)
  
function do_union (union_exp: expr;
                   ctxt_lwb, ctxt_len: set_range): set_desc;
  
type
  set_word_array = ^array [1..*] of pdp10word;
  
var
  i, num_nonconst:      oper_range;
  j, real_upb_plus_1, max_const_len, min_val, max_val:    int_type;
  temp_descr:           set_desc;
  set_word:             set_word_array;
  reg:                  registers;
  have_const, fixed:    boolean;
  const_addr:           addr_desc;
  const_len,
  const_lwb, const_upb: set_range;
  
procedure do_or (reg: registers; cset_word: pdp10word);
  begin
    with cset_word do
      if value <> 0 then begin
        if value = -1 then gen_ri (seto, reg, 0)
        else if lh = 0 then gen_ri (iori, reg, rh)
        else if rh = 0 then gen_ri (tlo, reg, lh)
        else if lh = 777777B then
          gen_ri (orcmi, reg, -(value + 1))
        else
          gen_rm (ior, reg, gen_cst (value))
      end
  end;
  
function gen_cst_set (set_word: set_word_array; word_count: int_type): addr_desc;
  
  var
    cons_def: def;
  
  begin
    cons_def := make_def (constant_def);
    mark_def (cst_area, cons_def);
    for j := 1 to word_count do
      gen_word (cst_area, set_word^[j], setword);
    gen_cst_set := absolute_reference;
    gen_cst_set.reloc := reldef (cons_def)
  end;
  
procedure load_set (reg: registers; cset_word: pdp10word);
  begin
    with cset_word do
      if lh = 0 then gen_ri (movei, reg, rh)
      else if lh = 777777b then gen_ri (hrroi, reg, rh)
      else if rh = 0 then gen_ri (hrlzi, reg, lh)
      else if rh = 777777b then gen_ri (hrloi, reg, lh)
      else gen_rm (move, reg, gen_cst (value))
  end;
  
  
begin
  with union_exp^ do begin
    have_const := false;
    num_nonconst := 0;
    real_upb_plus_1 := 0;
    max_const_len := 0;
  
    for i := 1 to upperbound (operand) do begin
      temp_descr := do_set_fetch (operand[i], ctxt_lwb, ctxt_len);
  
      if (temp_descr.nargs = 2) andif
            (aconstp (temp_descr.arg[1], const_lwb) and aconstp (temp_descr.arg[2], const_upb)) then begin
        if not have_const then begin
          have_const := true;
          new (set_word, (ctxt_len + 35) div 36);
          for j := 1 to ((ctxt_len + 35) div 36) do
            set_word^[j].value := 0
        end;
        for j := max (0, const_lwb - ctxt_lwb) to
                 min (const_upb - ctxt_lwb, ctxt_len - 1) do
          set_word^[ (j div 36) + 1 ].bits [j mod 36] := true;
        max_const_len := max (const_upb + 1, max_const_len);
        real_upb_plus_1 := max ( min (ctxt_lwb + ctxt_len, const_upb),
                                 real_upb_plus_1) (* constants are created within context *)
      end
  
      else if temp_descr.nargs > 0 then (* nonconstant *) begin
        if temp_descr.nargs = 3 then (* set exists already *)
	  with temp_descr.upb_exp^.desc do begin
	    real_upb_plus_1 := max (set_lwb + set_length, real_upb_plus_1);
	    set_length := ctxt_lwb + ctxt_len - set_lwb  (* defer masking to result *)
	  end
        else with operand[i]^.desc do (* set will be created (within context) *)
          real_upb_plus_1 := max ( min (set_lwb + set_length, ctxt_lwb + ctxt_len), real_upb_plus_1);
        num_nonconst := num_nonconst + 1;
        if num_nonconst = 1 then
          do_union := temp_descr
        else begin
          if num_nonconst = 2 then begin
            reverse_if_helpful (do_union, temp_descr, ctxt_lwb, ctxt_len, (ctxt_len > 72));
            make_set (do_union, ctxt_lwb, ctxt_len, true (* force into temporary *));
            if ctxt_len <= 72 (* short set *) then begin
              reg := load_addr (do_union.arg[1], left_aligned, ctxt_len);
              do_union.arg[1] := reg_addr (reg) (* update set descriptor *)
            end
          end;
          if ctxt_len <= 72 (* short set *) then begin
	    make_set (temp_descr, ctxt_lwb, ctxt_len, false);
            gen_rm (ior, reg, temp_descr.arg[1]);
            if ctxt_len > 36 then begin
              temp_descr.arg[1] := increment_addr (temp_descr.arg[1], 1, 0, 36);
              gen_rm (ior, reg + 1, temp_descr.arg[1])
            end
          end
	  else if (temp_descr.nargs = 2) andif addr_equal (temp_descr.arg[1], temp_descr.arg[2]) then begin
	    genset_op_bounds (temp_descr.lwb_exp, min_val, max_val, fixed);
	    reg := load_addr (temp_descr.arg[1], left_aligned, 36);
	    temp_descr.arg[1] := reg_addr (reg);
	    gen_ri (movei, 1, 1);
	    set_elem_reference (do_union, reg, 1, min_val, max_val, dpb)
	  end
	  else begin
	    make_set (temp_descr, ctxt_lwb, ctxt_len, false);
	    if sets_compatible (do_union, temp_descr) then
	      longset_join (iorm, do_union.arg[1], temp_descr.arg[1], do_union.arg[3])
	    else (* incompatible long set *) begin
	      force_out_of_reg (temp_descr);  (* runtime cannot accept arg residing in regs *)
	      gen_rt (pushj, sb, rt_sun_ll);
	      l_format_arg (do_union.arg[1], do_union.arg[2], do_union.arg[3]);
	      l_format_arg (temp_descr.arg[1], temp_descr.arg[2], temp_descr.arg[3])
	    end
	  end;
          internal_set_free (temp_descr)
        end
      end (* nonconstant *);
    end (* for *);
  
    (* recompute shaped length of expression using only the shaped upb's of those operands
       not ignored because they were null (after shaping) - operands not or'ed into
       the result can't contribute bits, so none of their bits could possibly require
       masking later *)
    union_exp^.desc.set_length := real_upb_plus_1 - union_exp^.desc.set_lwb;
  

    if not stemps_dynamic then
      max_const_len := ctxt_len;
    if have_const then begin
      if num_nonconst = 0 then begin
        do_union.nargs := 3;
        if ctxt_len > 72 then
          do_union.arg[1] := gen_cst_set (set_word, (max_const_len + 35) div 36)
        else (* short set *) begin
          reg := get_reg (ctxt_len);
          do_union.arg[1] := reg_addr (reg);
          load_set (reg, set_word^[1]);
          if ctxt_len > 36 then
            load_set (reg + 1, set_word^[2])
        end;
        do_union.arg[2] := int_value (ctxt_lwb);
        do_union.arg[3] := int_value (max_const_len)
      end
      else (* num_nonconst > 0 *) begin
        if num_nonconst = 1 then begin
          make_set (do_union, ctxt_lwb, ctxt_len, true (* force into temporary *));
          if ctxt_len <= 72 then (* short set *) begin
            reg := load_addr (do_union.arg[1], left_aligned, ctxt_len);
            do_union.arg[1] := reg_addr (reg)  (* update set descriptor *)
          end
        end;
        if ctxt_len <= 72 (* short set *) then begin
          do_or (reg, set_word^[1]);
          if ctxt_len > 36 then
            do_or (reg + 1, set_word^[2])
        end
        else (* long set *) begin
          const_addr := gen_cst_set (set_word, (max_const_len + 35) div 36);
          if aconstp (do_union.arg[3], const_len) andif
            ((do_union.arg[2].offset = ctxt_lwb) and (const_len = max_const_len)) then
            longset_join (iorm, do_union.arg[1], const_addr, int_value (max_const_len))
          else (* incompatible long sets *) begin
            gen_rt (pushj, sb, rt_sun_ll);
            l_format_arg (do_union.arg[1], do_union.arg[2], do_union.arg[3]);
            l_format_arg (const_addr, int_value (ctxt_lwb), int_value (max_const_len))
          end
        end
      end;
      dispose (set_word)
    end
  
    else (* no constant part *)
      if num_nonconst = 0 (* no nonconstant part either *) then
        do_union.nargs := 0 (* "z" format *)
  
  end (* with *);
  
  (* if anything was actually done for this tuple, then update the set descriptor's
     pointer to the tuple where the set it describes REALLY came from (for later tests
     to see if masking is necessary).  *)
  
  if (num_nonconst <> 1) or have_const then
    do_union.upb_exp := union_exp
end (* do_union *);
$PAGE do_set_fetch
  
(* DO SET FETCH is the set analog of DO FETCH, performing the minimum of work
   necessary to produce a set_desc describing the evaluated set. Creation of an
   actual set is avoided if a descriptor of its value will suffice.  Context
   information originating from the end-user of the set (possibly improved by "shaping")
   is passed along as the set is recursively evaluated.  *)
  
function do_set_fetch (* (exp: expr; ctxt_lwb, ctxt_len: set_range): set_desc *);
  
var
  lwb_immed, upb_immed: boolean;
  length: int_type;
  const_lwb, const_upb: set_range;
  
begin
  with exp^, do_set_fetch do begin
  
    (* evaluate the expression *)
  
    case opcode of
  
      cst_ref,
      ident_ref,
      field_ref,
      buffer_ref,
      ptr_ref,
      array_ref,
      func_call_op: begin  (* note: only sets handled here *)
	if opcode <> func_call_op then
	  arg[1] := fetch_fullword (exp)
	else begin
	  if desc.set_length <= 72 (* short set *) then
	    arg[1] := scl_function_call (exp)
	  else (* long set *) begin
	    arg[1] := get_temp ((max (ctxt_len, desc.set_length) + 35) div 36);
	    pas_call (exp, addr_return_value, nil, arg[1])
	  end
	end;
        arg[2] := int_value (desc.set_lwb);
        length := min (ctxt_lwb + ctxt_len, desc.set_lwb + desc.set_length) - desc.set_lwb;
        if (length <= 0) or (ctxt_lwb >= desc.set_lwb + desc.set_length) then begin
          nargs := 0; (* "z" format *)
          free (arg[1])
        end
        else begin
          nargs := 3; (* "l" format *)
          arg[3] := int_value (length)
        end;
        upb_exp := exp
      end;
  
      setcvt_op: begin
        do_set_fetch := do_set_fetch (operand[1], ctxt_lwb, ctxt_len);
        desc.set_lwb := operand[1]^.desc.set_lwb;
        desc.set_length := operand[1]^.desc.set_length
      end;

      gen_set_op: begin
        if upperbound (operand) = 0 (* null set *) then
          nargs := 0 (* "z" format *)
        else (* upperbound (operand) = 1 or 2 *) begin
          nargs := 2; (* "o" format *)
          arg[1] := argument (operand[1]);
          lwb_exp := operand[1];
          if upperbound (operand) = 1 then begin (* singleton set *)
            arg[2] := duplicate_addr (arg[1]);
            upb_exp := lwb_exp
          end
          else (* range set *) begin
            arg[2] := argument (operand[2]);
            upb_exp := operand[2]
          end;

          (* force bounds to be within context *)

          if ctxt_len > 0 then begin
            lwb_immed := aconstp (arg[1], const_lwb);
            upb_immed := aconstp (arg[2], const_upb);
            if lwb_immed andif (const_lwb < ctxt_lwb) then
              arg[1].offset := ctxt_lwb;
            if upb_immed andif (const_upb > (ctxt_lwb + ctxt_len - 1)) then
              arg[2].offset := ctxt_lwb + ctxt_len - 1
          end;

          (* detect null set *)

          if (ctxt_len = 0) orif (desc.set_length = 0) orif 
             (ctxt_lwb >= desc.set_lwb + desc.set_length) orif
             (desc.set_lwb >= ctxt_lwb + ctxt_len) orif
             (lwb_immed andif (arg[1].offset > (ctxt_lwb + ctxt_len - 1))) orif
             (upb_immed andif (arg[2].offset < ctxt_lwb)) then begin
            nargs := 0;
            free (arg[1]);
            free (arg[2])
          end
        end (* noper = 1 or 2 *)
      end;


      union_op:
        do_set_fetch := do_union (exp, ctxt_lwb, ctxt_len);

      both_op:
        do_set_fetch := do_intersection (exp, ctxt_lwb, ctxt_len);

      diff_op:
        do_set_fetch := do_set_difference (exp, ctxt_lwb, ctxt_len);

      others:
        assert (false)
    end
  end
end;
$PAGE find_runtime_upb
  
(* FIND RUNTIME UPB traverses the tree for a set-valued expression which the
   shaper has determined to have a variable length, and which set_fetch has decided
   could potentially exceed the SET_DYNAMIC_CUTOVER.  An addr_desc is returned for a
   location which will, at runtime, contain the actual upper bound of the set (in bits).
  
   This procedure also determines if the actual upper bound at runtime could be negative,
   and thus must be tested to see that it is not less than zero before use in calculations
   of how much to adjust the stack.  A negative value would result in adjusting the
   stack backwards, with unfortunate results.  *)
  
function find_runtime_upb (set_exp: expr; var ltz_check_required: boolean): addr_desc;
  
var
  first_op, second_op: addr_desc;
  reg: registers;
  i: oper_range;
  const_upb: set_range;
  min_val, max_val: int_type;
  fixed, ltzchk_2: boolean;
  
begin
  with set_exp^ do begin
 
    if desc.set_cst_len then
      find_runtime_upb := int_value (desc.set_length + desc.set_lwb - 1)
  
    else
      case opcode of
  
        setcvt_op,
        diff_op:
          find_runtime_upb := find_runtime_upb (operand[1], ltz_check_required);
  
        gen_set_op: begin
          operand [upperbound (operand)]^.usage_count :=
	    operand [upperbound (operand)]^.usage_count + 1; (* keep fetch happy *)
          find_runtime_upb := fetch_fullword (operand [upperbound (operand)]);
          genset_op_bounds (operand[upperbound (operand)], min_val, max_val, fixed);
          ltz_check_required := min_val < 0
        end;
  
        both_op: begin
          first_op := find_runtime_upb (operand[1], ltz_check_required);
          second_op := find_runtime_upb (operand[2], ltzchk_2);
          if not aconstp (first_op, const_upb) then begin
            reg := load_addr (first_op, left_aligned, 36);
            gen_rm (cam+ltc, reg, second_op);
            gen_rm (move, reg, second_op);
            free (second_op)
          end
          else begin
            reg := load_addr (second_op, left_aligned, 36);
            gen_ri (cam+ltc, reg, 36);
            gen_rm (move, reg, first_op);
            free (first_op)
          end;
          find_runtime_upb := reg_addr (reg);
          ltz_check_required := ltz_check_required or ltzchk_2
        end;
  
        union_op: begin
          first_op := find_runtime_upb (operand[1], ltz_check_required);
          reg := load_addr (first_op, left_aligned, 36);
          for i := 2 to upperbound (operand) do begin
            second_op := find_runtime_upb (operand[i], ltzchk_2);
            gen_rm (cam+gtc, reg, second_op);
            gen_rm (move, reg, second_op);
            free (second_op);
            ltz_check_required := ltz_check_required and ltzchk_2
          end;
          find_runtime_upb := reg_addr (reg)
        end;
  
        others:
          assert (false)
      
      end (* case *);
  end (* with *);
end (* find_runtime_upb *);
$PAGE set_fetch
  
(* SET FETCH is passed a set expression tuple and returns a set descriptor for the
   value of the tuple.  The set value may not actually be created if it can be
   represented by a "o" or "z" format set descriptor.
  
   The caller provides the tightest context information it can via the arguments
   ctxt_lwb and ctxt_len.  A psuedo shaping pass is made over the expression tree
   to propagate lwb and length information up from the leaves of the tree, and to
   improve on the provided context if possible.  *)
  
public function set_fetch (set_exp: expr;
                           ctxt_lwb, ctxt_len: set_range): set_desc;
  
var
  shaped_lwb, shaped_len: set_range;
  
begin
  
  (* propagate lwb and length information up from the leaves of the expression tree *)
  
  shape_set (set_exp, shaped_lwb, shaped_len);
  
  (* use best combination of information possible *)
  
  if ctxt_lwb > shaped_lwb then begin
    shaped_len := max (0, shaped_len - (ctxt_lwb - shaped_lwb));
    shaped_lwb := ctxt_lwb
  end;
  if (shaped_lwb + shaped_len) > (ctxt_lwb + ctxt_len) then
    shaped_len := max (0, ctxt_lwb + ctxt_len - shaped_lwb);
  if shaped_len = 0 then
    shaped_lwb := 0; (* keep null set reps. consistent: len=lwb=0 *)
  
  if not set_exp^.desc.set_cst_len andif (shaped_len > set_dynamic_cutover) then begin
    stemps_dynamic := true;
    shaped_lwb := 0; (* zero-base the sets in dynamic temps to limit
                        conversion problems *)
    rtime_upb := find_runtime_upb (set_exp, ltz_check_of_rtime_upb_reqd);
  end
  else
    stemps_dynamic := false;
  
  (* generate code required to evaluate the set *)
  
  have_freed_temp := false; (* within this module we'll try to reuse temps *)
  set_fetch := do_set_fetch (set_exp, shaped_lwb, shaped_len);
  if have_freed_temp then begin
    set_free (freed_temp);
    have_freed_temp := false
  end
 
end;
$PAGE set_assignment
  
(* SET ASSIGNMENT generates code for assignment of set values.  *)
  
public procedure set_assignment (assign_tpl: tuple);
  
var
  rhs_desc: set_desc;
  laddr: addr_desc;
  reg: registers;
  const_lwb, const_len: set_range;
  
begin
  with assign_tpl^ do begin
    rhs_desc := set_fetch (rhs, lhs^.desc.set_lwb, lhs^.desc.set_length);
    laddr := fetch (lhs);
    
    if rhs_desc.nargs = 0 (* "z" format *) then
      if laddr.mode = fw then
        clear_set (lhs, laddr)
      else begin
        gen_ri (movei, 1, 0);
        store (1, laddr, lhs^.desc.set_length, left_aligned);
        free (laddr)
      end
  
    else if lhs^.desc.set_length <= 72 (* short set *) then begin
      make_set (rhs_desc, lhs^.desc.set_lwb, lhs^.desc.set_length, false);
      reg := load_addr (rhs_desc.arg[1], left_aligned, lhs^.desc.set_length);
      store (reg, laddr, lhs^.desc.set_length, left_aligned);
      decr_reg_usages (reg);
      free (laddr)
    end
  
    else (* long set *) begin
      if rhs_desc.nargs <> 3 then
        make_set (rhs_desc, lhs^.desc.set_lwb, lhs^.desc.set_length, false);
      if (aconstp (rhs_desc.arg[2], const_lwb) andif (const_lwb = lhs^.desc.set_lwb))
              and
         (aconstp (rhs_desc.arg[3], const_len) andif
	   ( (rhs^.desc.set_length = lhs^.desc.set_length) or
		((lhs^.desc.set_length mod 36 = 0) and
		 (rhs^.desc.set_length > lhs^.desc.set_length)) )) then
        do_blt (rhs_desc.arg[1], laddr, int_value ((lhs^.desc.set_length + 35) div 36))
      else begin
        force_out_of_reg (rhs_desc); (* runtime cannot accept arg residing in regs *)
        gen_rt (pushj, sb, rt_smv_ll);
        l_format_arg (laddr, int_value (lhs^.desc.set_lwb), int_value (lhs^.desc.set_length));
        l_format_arg (rhs_desc.arg[1], rhs_desc.arg[2], rhs_desc.arg[3]);
        set_free (rhs_desc);
        free (laddr)
      end
    end;
    if stemps_dynamic then
      free (rtime_upb)
  end
end (* set_assignment *);
$PAGE set_parameter
(* SET PARAMETER evaluates set parameters. *)

public procedure set_parameter (exp: expr;      (* parameter expression *)
                                parm: param_desc;       (* type node for the parameter *)
                                var reg: registers; (* if short form, reg for the parameter *)
                                short_form: boolean);   (* if all parameters in registers *)

var
  set_loc: set_desc;
  lwb, len, const_len: set_range;
  force_to_temp: boolean;
  reg2: registers;
  temp_addr: addr_desc;

begin
  lwb := parm.parm_type^.set_element_type^.minval;
  len := parm.parm_type^.set_element_type^.maxval - lwb + 1;
  set_loc := set_fetch (exp, lwb, len);
  
  (* it will only be necessary to force the argument into a temp if its a value
     param (since var params MUST be of exactly the right type), and even then
     only if the set could have been produced smaller than the formal is expecting,
     or its bigger and the formal doesn't end on a word boundary.  *)
  
  force_to_temp := ((parm.parm_kind = values) and (set_loc.nargs = 3))
					  andif
		   (not aconstp (set_loc.arg[3], const_len) orif
			((set_loc.upb_exp^.desc.set_length <> len) and
			   not ((len mod 36 = 0) and (len < set_loc.upb_exp^.desc.set_length)) ));
  make_set (set_loc, lwb, len, force_to_temp);
  if (force_to_temp and aconstp (set_loc.arg[3], const_len) and (set_loc.arg[1].reloc.kind = temp_sc))
			  andif
     ((len mod 36 <> 0) and (len < set_loc.upb_exp^.desc.set_length)) then begin
    temp_addr := increment_addr (duplicate_addr (set_loc.arg[1]), len div 36, 0, 36);
    reg2 := get_reg (36);
    gen_rm (move, reg2, temp_addr);
    left_mask (reg2, 36, len - (len div 36) * 36);
    gen_rm (movem, reg2, temp_addr);
    free (temp_addr)
  end;
  if stemps_dynamic then
    free (rtime_upb);
  set_free (set_loc);
  if not short_form then begin  (* get reg for result *)
    if (parm.parm_kind = vars) orif p_b_address (parm.parm_type)
      then reg := get_reg (36)
      else reg := get_reg (len);
  end
  else (* reg already assigned if shortform *)
    if (len > 36) andif (parm.parm_kind <> vars) andif not p_b_address (parm.parm_type) then
      tag_reg (reg, reg+1); (* indicate that we're using a pair, not just one *)
  
  if not is_register (set_loc.arg[1]) orif (set_loc.arg[1].offset <> reg) then begin
    if (parm.parm_kind = vars) orif p_b_address (parm.parm_type) then
      gen_rm (movei, reg, set_loc.arg[1])
    else do_move (reg, set_loc.arg[1], left_aligned, len);
  end;
end (* set_parameter *).
   rn