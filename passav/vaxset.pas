$TITLE VAXSET - vax set expression evaluation
module vaxset;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM vaxgen.inc
$SYSTEM vaxcgu.inc
$SYSTEM vaxexp.inc
$SYSTEM vaxutl.inc
$SYSTEM vaxopc.inc
$SYSTEM pascv.inc
$SYSTEM pasmth.inc
$SYSTEM vaxstr.inc
$SYSTEM vaxcll.inc
$SYSTEM ptmcon.inc
  
(* set descriptor type.  There are three formats:
  
  "z" format: null set.
  
      nargs = 0
      other fields UNUSED
  
  "o" format: singleton or range set.
  
      nargs = 2
      arg[1] = addr descriptor of lower bound
      arg[2] = addr descriptor of upper bound
      lwb_exp = pointer to integral-valued expression tuple for lowerbound
                  (must be operand of a genset tuple).
      upb_exp = pointer to similar tuple for upper bound.
      in_temp = UNUSED.
  
  "l" format:
  
      nargs = 3
      arg[1] = addr descriptor for location of set
      arg[2] = addr descriptor for lower bound in BYTES
      arg[3] = addr descriptor for length in BYTES
      lwb_exp,
      upb_exp = UNUSED.
      in_temp = true if set is in a temporary, false otherwise.  *)
  
type
  set_desc = packed record
    nargs: 0..3;
    in_temp: boolean;
    lwb_exp,
    upb_exp: expr;
    arg: array [1..3] of addr_desc
  end;
  
  set_byte_array = ^array [1..*] of packed array [0..7] of boolean;
  
var
  have_freed_temp: boolean;
  freed_temp: set_desc;
$PAGE arithmetic utilities
  
(* The following routines perform simple manipulations that occur with great
   frequency within the set module.  These functions are defined so that the
   calculations utilizing them will be more readable by virtue of the mnemonic
   names.  *)
  
  
function len_bytes (lwb_bit, len_bits: set_range): set_range;
  begin
    if len_bits = 0 then
      len_bytes := 0
    else
      len_bytes := ngm (len_bits + lwb_bit mod bits_per_byte, bits_per_byte) div bits_per_byte
  end;
  
function byte_num (bit_num: set_range): set_range;
  begin
    byte_num := bit_num div bits_per_byte
  end;
  
function upb (lwb_arg, len_arg: set_range): int_type;
  begin
    upb := lwb_arg + len_arg - 1
  end;
  
function len (upb_arg, lwb_arg: set_range): set_range;
  begin
    len := max (upb_arg - lwb_arg + 1, 0)
  end;
  
  
(* SETB SIZE returns the size in bytes of a set given an expr tuple.  Basing modulo 8
   is taken into account.  *)
  
public function setb_size (exp: expr): set_range;
  
  begin
    with exp^.desc do
      setb_size := len_bytes (set_lwb, set_length)
  end;
$PAGE genset_op_bounds
  
(* GENSET OP BOUNDS determines the range of possible values for an integral valued
   expression tuple occurring as an operand of a GENSET tuple.  *)
  
public procedure genset_op_bounds (    int_valued_exp: expr;
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
      others:
	worst_case (min_val, max_val)
    end (* case *);
   
end (* genset_op_bounds *);
$PAGE shape_set
  
(* SHAPE SET is passed a set expression tuple.  It propagates lwb and length
   information upwards from the leaves of the expression tree, storing the
   information in the type descriptor fields of the expression tuples. 
  
   For (sub)expressions like [I..J] the length information is worst case, based on the
   declared ranges of I and J.  The set_cst_len fields of the nodes' type
   descriptors are used to propagate whether a set expression has the indicated length,
   or if the length is variable and the indicated length is only a maximum.  *)
  
public procedure shape_set (    exp: expr;
                            var shaped_lwb, shaped_len: set_range);
  
var
  temp_lwb, temp_len, upper_bound, temp_upper_bound: set_range;
  i: oper_range;
  fixed: boolean;
  
procedure shape_gen_set_operand (    int_valued_exp: expr;
                                 var op_lwb, op_len: set_range;
                                 var fixed: boolean);
  var
    min_val, max_val: int_type;
  
  begin
    genset_op_bounds (int_valued_exp, min_val, max_val, fixed);
    assert (min_val <= set_upb_limit);
    if (max_val < 0) orif (max_val < min_val) then (* null set *) begin
      op_lwb := 0;
      op_len := 0;
      fixed := true
    end
    else begin
      op_lwb := max (0, min_val);
      op_len := len (min (set_upb_limit, max_val), op_lwb);
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
          if upb (temp_lwb, temp_len) >= shaped_lwb then begin
            shaped_len := len (upb (temp_lwb, temp_len), shaped_lwb);
            desc.set_cst_len := fixed (* depends on upperbound of range *)
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
        upper_bound := min (upb (shaped_lwb, shaped_len), upb (temp_lwb, temp_len));
	desc.set_cst_len := (operand[1]^.desc.set_cst_len andif 
					      (upper_bound = upb (temp_lwb, temp_len)))
				orif
			    (operand[2]^.desc.set_cst_len andif
					      (upper_bound = upb (shaped_lwb, shaped_len)));
        shaped_lwb := max (shaped_lwb, temp_lwb);
        if upper_bound >= shaped_lwb then
          shaped_len := len (upper_bound, shaped_lwb)
        else (* null set *) begin
          shaped_len := 0;
          shaped_lwb := 0;
          desc.set_cst_len := true
        end
      end;
  
      union_op: begin
        shaped_lwb := set_upb_limit;
        upper_bound := 0;
        for i := 1 to upperbound (operand) do begin
          shape_set (operand[i], temp_lwb, temp_len);
          if temp_len > 0 (* non null *) then begin
            shaped_lwb := min (shaped_lwb, temp_lwb);
            temp_upper_bound := upb (temp_lwb, temp_len);
            if temp_upper_bound > upper_bound then begin
              upper_bound := temp_upper_bound;
              desc.set_cst_len := operand[i]^.desc.set_cst_len
            end
            else if (temp_upper_bound = upper_bound) andif operand[i]^.desc.set_cst_len then
              desc.set_cst_len := true
          end
        end;
        if upper_bound >= shaped_lwb then
          shaped_len := len (upper_bound, shaped_lwb)
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
$PAGE set_free, set_temporary
  
(* SET FREE frees each of the addr_desc's used in a set descriptor.  *)
  
public procedure set_free (sdesc: set_desc);
  
var
  j: 1..3;
  
begin
  with sdesc do
    for j := 1 to nargs do
      free (arg[j])
end;
 
  
  
(* SET TEMPORARY returns a set_descriptor for space allocated on the stack
   to be used for a temporary.  *)
  
public function set_temporary (ctxt_lwbbit: set_range; len_addr: addr_desc): set_desc;
  
begin
  if have_freed_temp then begin
    set_temporary := freed_temp;
    assert (adr_equal (set_temporary.arg[3], len_addr));
    have_freed_temp := false
  end
  else with set_temporary do begin
    nargs := 3;
    arg[2] := int_value (byte_num (ctxt_lwbbit)); (* lowerbound byte *)
    arg[3] := duplicate_addr (len_addr);
    arg[1] := get_temp (len_addr, vax_byte);
    arg[1].byte_size := vax_byte;
    in_temp := true
  end
end (* set_temporary *);
$PAGE gen_c5
  
(* GEN C5 is a convenience for emitting movc5's and cmpc5's.  It also does some optimization. *)
  
public procedure gen_c5 (opc: opc_range; slen, sloc, tlen, tloc: addr_desc);
  
  var
    regs_saved, used_regs: set_of_registers;
    temp_slen, temp_tlen: addr_desc;
    source_len, target_len: set_range;
    slen_const, tlen_const: boolean;
  
  begin
    if opc = movc5 then
      used_regs := [r0..r5]
    else if opc = cmpc5 then
      used_regs := [r0..r3]
    else
      assert (false);
    slen_const := aconstp (slen, source_len);
    tlen_const := aconstp (tlen, target_len) andif
			((target_len in [1,2,4]) or ((opc = movc5) and (target_len = 8)));
    temp_slen := cvt_word (duplicate_addr (slen), unsigned_value);
    temp_tlen := cvt_word (duplicate_addr (tlen), unsigned_value);
  
    (* Lengths are equal. *)
  
    if adr_equal (slen, tlen) then begin
      if slen_const andif ((opc = cmpc5) and (source_len in [1,2,4])) then
	case source_len of
	  1: gen2 (cmpb, sloc, tloc);
	  2: gen2 (cmpw, sloc, tloc);
	  4: gen2 (cmpl, sloc, tloc)
	end
      else if slen_const andif ((opc = movc5) and (source_len in [0,1,2,4,8])) then
	case source_len of
	  0: ;
	  1: gen2 (movb, sloc, tloc);
	  2: gen2 (movw, sloc, tloc);
	  4: gen2 (movl, sloc, tloc);
	  8: gen2 (movq, sloc, tloc)
	end
      else begin
	regs_saved := save_regs (used_regs);
	gen3 (opc - (movc5 - movc3), temp_slen, sloc, tloc);
	restore_regs (regs_saved);
	mark_regs_used (used_regs)
      end
    end
  
    (* Lengths are not equal. *)
  
    else if (slen_const andif (source_len = 0)) and (opc = cmpc5) and tlen_const then
      case target_len of
	1: gen1 (tstb, tloc);
	2: gen1 (tstw, tloc);
	4: gen1 (tstl, tloc)
      end
    else if (slen_const andif (source_len = 0)) and (opc = movc5) and tlen_const then
      case target_len of
	1: gen1 (clrb, tloc);
	2: gen1 (clrw, tloc);
	4: gen1 (clrl, tloc);
	8: gen1 (clrq, tloc)
      end
    else begin
      regs_saved := save_regs (used_regs);
      gen5 (opc, temp_slen, sloc, typ_int_value (0, vax_byte), temp_tlen, tloc);
      restore_regs (regs_saved);
      mark_regs_used (used_regs)
    end;
  
    free (temp_slen);
    free (temp_tlen);
  end;
$PAGE prep_addr, make_incrementable
  
(* PREP ADDR loads the given address into a register, and updates the addr_desc
   accordingly.  This is used to prepare an addr_desc so nasty things can be done to it.  *)
  
procedure prep_addr (var addr: addr_desc);
  var
    reg, primary_reg, index_reg: registers;
    new_addr: addr_desc;
    usages: int_type;
  begin
    regs_used (addr, primary_reg, index_reg);
    usages := regdesc [primary_reg].uses_remaining;
    reg := move_address (addr);
    new_addr := absolute_reference;
    new_addr.register := reg;
    if (usages > regdesc [primary_reg].uses_remaining) and (usages > 1) then begin
      update_usages (new_addr, usages - 1);
      update_usages (addr, 1 - usages)
    end;
    addr := new_addr
  end;
  
  
(* INCREMENTABLE determines whether offset_addr could increment the given address
   without having to load it into a register first.  *)
  
function incrementable (addr: addr_desc): boolean;
  begin
    incrementable := is_disp_mode (addr) orif is_symbol_addr (addr)
  end;
$PAGE set_move
  
(* SET MOVE moves a set to a new location, taking into account different starting
   bytes and byte lengths of the source and target sets.  WARNING - this routine may modify
   the form of to_desc.arg[1] (although the new form will point to the same place).   *)
 
public procedure set_move (from_desc: set_desc; var to_desc: set_desc);
  
var
  target_addr, dup_addr: addr_desc;
  target_length, cleared_length, source_length: int_type;
  defn1, defn2: def;
  
begin
  assert (from_desc.nargs = 3);
  assert (from_desc.arg[2].offset >= to_desc.arg[2].offset);
  
  (* Fixed-length context. *)
  
  if aconstp (to_desc.arg[3], target_length) then begin
    if to_desc.arg[2].offset < from_desc.arg[2].offset then begin
      cleared_length := min (target_length, from_desc.arg[2].offset - to_desc.arg[2].offset);
      gen_c5 (movc5, int_value (0), stack_top_reference, int_value (cleared_length), to_desc.arg[1]); (* clear *)
      if not incrementable (to_desc.arg[1]) then
        prep_addr (to_desc.arg[1]); (* change form if necessary *)
      dup_addr := duplicate_addr (to_desc.arg[1]);
      target_addr := offset_addr (dup_addr, int_value (cleared_length));
      target_length := target_length - cleared_length
    end
    else
      target_addr := duplicate_addr (to_desc.arg[1]);
    (* note - if target length is constant, then the source length must be also *)
    assert (aconstp (from_desc.arg[3], source_length));
    if target_length <= from_desc.arg[3].offset then
      gen_c5 (movc5, int_value (target_length), from_desc.arg[1],
		     int_value (target_length), target_addr)
    else if (source_length = 1) and (target_length = 2) then
      gen2 (movzbw, from_desc.arg[1], target_addr)
    else if (source_length = 1) and (target_length = 4) then
      gen2 (movzbl, from_desc.arg[1], target_addr)
    else if (source_length = 2) and (target_length = 4) then
      gen2 (movzwl, from_desc.arg[1], target_addr)
    else
      gen_c5 (movc5, int_value (source_length), from_desc.arg[1],
		     int_value (target_length), target_addr);
    free (target_addr)
  end
  
  (* Variable-length context. *)
  
  else if to_desc.arg[2].offset < from_desc.arg[2].offset then begin
    cleared_length := from_desc.arg[2].offset - to_desc.arg[2].offset;
    defn1 := make_def (local_def);
    defn2 := make_def (local_def);
    gen2 (cmpl, to_desc.arg[3], int_value (cleared_length)); (* target really that long? *)
    gen_branch (bgeq, defn1);
    (* Target's not that long - clear what there is and bail out. *)
    gen_c5 (movc5, int_value (0), stack_top_reference, to_desc.arg[3], to_desc.arg[1]);
    gen_branch (brb, defn2);
    (* Clear the unused part of the target. *)
    mark_def (code_area, defn1);
    gen_c5 (movc5, int_value (0), stack_top_reference, int_value (cleared_length), to_desc.arg[1]); (* clear *)
    gen3 (subl3, int_value (cleared_length), to_desc.arg[3], r0_addr);
    
    if not incrementable (to_desc.arg[1]) then
      prep_addr (to_desc.arg[1]); (* change form if necessary *)
    dup_addr := duplicate_addr (to_desc.arg[1]);
    target_addr := offset_addr (dup_addr, int_value (cleared_length));
    gen_c5 (movc5, from_desc.arg[3], from_desc.arg[1], r0_addr, target_addr);
    free (target_addr);
    mark_def (code_area, defn2)
  end
  
  else
    gen_c5 (movc5, from_desc.arg[3], from_desc.arg[1], to_desc.arg[3], to_desc.arg[1]);
  
  set_free (from_desc)
end (* set_move *);
$PAGE gen_cst_set
  
(* GEN CST SET emits a set constant in the constant area. *)
  
function gen_cst_set (set_byte: set_byte_array;
		      const_base, byte_count, first_byte: int_type): set_desc;
  
var
  cr: code;
  cons_def: def;
  bit: 0..7;
  j: set_range;
  
begin
  cons_def := make_def (constant_def);
  mark_def (cst_area, cons_def);
  for j := first_byte to byte_count do begin
    new (cr,setbyte);
    for bit := 0 to 7 do
      cr^.setval [bit] := set_byte^[j][bit];
    gen_emit (cst_area, cr)
  end;
  with gen_cst_set do begin
    nargs := 3;
    arg[1] := absolute_reference;
    arg[1].reloc := reldef (cons_def);
    arg[1].byte_size := vax_byte;
    arg[2] := int_value (byte_num (const_base) + first_byte - 1);
    a := int_value (len (byte_count, first_byte));
    in_temp := false
  end
end (* gen_cst_set *);
$PAGE prep_bitreference
  
(* PREP BITREFERENCE determines what range checking of the element value will be required,
   and adjusts the element value accoring to the context lowerbound to produce a relative
   bit position.  *)
  
public procedure prep_bitreference (element: addr_desc; var bit_position: addr_desc;
				    min_val, max_val: int_type; target_desc: set_desc;
				    var defn: def);
  
var
  ctxt_base, elem_val, temp: set_range;
  test_low, test_high, set_cond_code: boolean;
  
begin
  ctxt_base := target_desc.arg[2].offset * bits_per_byte; (* basing modulo 8 *)
  test_low := min_val < ctxt_base;
  test_high := not aconstp (target_desc.arg[3], temp) orif
	       (max_val > upb (ctxt_base, temp * bits_per_byte));

  set_cond_code := false;
  if aconstp (element, elem_val) then
    bit_position := int_value (elem_val - ctxt_base)
  else if ctxt_base > 0 then begin
    gen3 (subl3, int_value (ctxt_base), element, r0_addr);
    bit_position := r0_addr;
    set_cond_code := true (* subl3 sets condition codes by r0's value *)
  end
  else if not is_register (element) and test_low then begin
    gen2 (movl, element, r0_addr); (* seems beneficial to load it *)
    bit_position := r0_addr;
    set_cond_code := true (* movl sets condition codes by r0's value *)
  end
  else
    bit_position := element; (* just use it where its at *)

  if (test_high or test_low) and (defn = nil) then
    defn := make_def (local_def);
  if test_low then begin
    if not set_cond_code then
      gen1 (tstl, bit_position);
    gen_branch (blss, defn)
  end;
  if test_high then begin
    if aconstp (target_desc.arg[3], temp) then
      gen2 (cmpl, bit_position, int_value (temp * bits_per_byte))
    else begin
      gen3 (mull3, int_value (bits_per_byte), target_desc.arg[3], r1_addr);
      gen2 (cmpl, bit_position, r1_addr)
    end;
    gen_branch (bgeq, defn)
  end;
end (* prep_bitreference *);
$PAGE emit_insv
  
(* EMIT INSV takes a descriptor for a singleton set and, after emitting instructions to perform
   any necessary bounds checking, generates an insv instruction to insert the given
   bit value into the target set.  *)
  
public procedure emit_insv (bit_value: int_type; source_desc: set_desc;
			    target_desc: set_desc);
  
var
  bit_position, temp_addr, dup_addr: addr_desc;
  elem_val: set_range;
  fixed: boolean;
  defn: def;
  min_val, max_val: int_type;
  
begin
  assert (source_desc.nargs = 2);
  genset_op_bounds (source_desc.lwb_exp, min_val, max_val, fixed);
  defn := nil;
  prep_bitreference (source_desc.arg[2], bit_position, min_val, max_val, target_desc, defn);

  if not aconstp (bit_position, elem_val) orif
     ((byte_num (elem_val) > 0) and (elem_val <= max_literal) andif
				     not incrementable (target_desc.arg[1])) then
    (* do it the simple way *)
    gen4 (insv, int_value (bit_value), bit_position, typ_int_value (1, vax_byte), target_desc.arg[1])
  else begin
    (* try to be clever *)
    dup_addr := duplicate_addr (target_desc.arg[1]);
    temp_addr := offset_addr (dup_addr, int_value (byte_num (elem_val)));
    if bit_value = 0 then
      gen2 (bicb2, typ_int_value (2**(elem_val mod bits_per_byte), vax_byte), temp_addr)
    else
      gen2 (bisb2, typ_int_value (2**(elem_val mod bits_per_byte), vax_byte), temp_addr);
    free (temp_addr)
  end;
  
  if defn <> nil then
    mark_def (code_area, defn)
end (* emit_insv *);
$PAGE gen_set
  
(* GEN SET creates a set, within context, given an "o" format set descriptor. *)
  
public procedure gen_set (source_desc: set_desc; var target_desc: set_desc);
  
type
  size_table = array [1..4] of vax_type;
const
  table: size_table := (vax_byte, vax_word, vax_word (* fill *), vax_long);
  
var
  constant, in_line: boolean;
  low, high, ctxt_base, const_base, const_bytelength, target_len: set_range;
  temp_target: addr_desc;
  immed_val, j: int_type;
  bit: 0..7;
  set_byte: set_byte_array;
  set_val: record
    case boolean of
      true: (int: int_type);
      false: (bits: packed array [0..35] of boolean)
    end;
  
begin
  assert (source_desc.nargs = 2);
  constant := aconstp (source_desc.arg[1], low) andif aconstp (source_desc.arg[2], high);
  in_line := constant and (aconstp (target_desc.arg[3], target_len) andif (target_len in [1,2,4]));
  
  (* Singleton set. *)
  
  if adr_equal (source_desc.arg[1], source_desc.arg[2]) then begin
    if not in_line then begin
      gen_c5 (movc5, int_value (0), stack_top_reference, target_desc.arg[3], target_desc.arg[1]); (* clear *)
      emit_insv (1, source_desc, target_desc)
    end
    else begin
      immed_val := 2**(low - (target_desc.arg[2].offset * bits_per_byte));
      temp_target := target_desc.arg[1];
      temp_target.byte_size := table [target_len];
      move_immediate (immed_val, temp_target)
    end
  end
  
  (* Range set. *)
  
  else begin
    ctxt_base := target_desc.arg[2].offset * bits_per_byte;
    if in_line then begin
      set_val.int := 0;
      for j := low - ctxt_base to high - ctxt_base do
	set_val.bits [35 - j] := true;
      temp_target := target_desc.arg[1];
      temp_target.byte_size := table [target_len];
      move_immediate (set_val.int, temp_target)
    end
  
    else if constant then (* length nonconst or not 1,2 or 4 *) begin
      if (low - ctxt_base) < 128 then
	const_base := ctxt_base
      else
	const_base := byte_num (low) * bits_per_byte; (* save space at expense of code *)
      const_bytelength := len (byte_num (high), byte_num (const_base));
      new (set_byte, const_bytelength);
      for j := 1 to const_bytelength do
        for bit := 0 to 7 do
	  set_byte^[j][bit] := false;
      for j := low - const_base to high - const_base do
	set_byte^[byte_num(j) + 1][j mod bits_per_byte] := true;
      set_move (gen_cst_set (set_byte, const_base, const_bytelength, 1), target_desc);
      dispose (set_byte)
    end
  
    else begin
      push_value (duplicate_addr (target_desc.arg[3]), unsigned_value);
      push_address (duplicate_addr (target_desc.arg[1]));
      push_value (int_value (ctxt_base), unsigned_value);
      push_value (duplicate_addr (source_desc.arg[2]), unsigned_value);
      push_value (duplicate_addr (source_desc.arg[1]), unsigned_value);
      gen_rt (5, rt_genset)
    end
  end;
  set_free (source_desc)
end (* gen_set *);
$PAGE clip_bits
  
(* CLIP BITS removes extraneous bits from the beginning of the first byte of a set,
   and from the end of the last byte.   Its intended for use on fixed length sets
   such as the lefthand side of a set assignment, or a set-valued parameter.  *)
  
procedure clip_bits (set_addr: addr_desc;
		     source_lwb, source_length, target_lwb, target_length: set_range);
  
type
  mask_table = array [1..7] of int_type;
const
  lmask: mask_table := (#H01, #H03, #H07, #H0f, #H1f, #H3f, #H7f);
  hmask: mask_table := (#H80, #Hc0, #He0, #Hf0, #Hf8, #Hfc, #Hfe);
  
var
  temp_addr: addr_desc;
  low_mask, high_mask: int_type;
  
begin
  if source_lwb < target_lwb then
    low_mask := target_lwb mod bits_per_byte
  else
    low_mask := 0;
  if low_mask > 0 then
    gen2 (bicb2, typ_int_value (lmask [low_mask], vax_byte), set_addr);
  
  if upb (source_lwb, source_length) >
      upb (target_lwb, target_length) then
    high_mask := 7 - upb (target_lwb, target_length) mod bits_per_byte
  else
    high_mask := 0;
  if high_mask > 0 then begin
    temp_addr := duplicate_addr (set_addr);
    if len_bytes (target_lwb, target_length) > 1 then
      temp_addr := offset_addr (temp_addr, int_value (len_bytes (target_lwb, target_length) - 1));
    gen2 (bicb2, typ_int_value (hmask [high_mask], vax_byte), temp_addr);
    free (temp_addr)
  end
end (* clip_bits *);
$PAGE set_fetch
  
(* SET FETCH is passed a set expression tuple and returns a set descriptor for the
   value of the tuple.  The set value may not actually be created if it can be
   represented by a "o" or "z" format set descriptor.
  
   The caller provides the tightest context information it can via the arguments
   ctxt_lwbbit and ctxt_lenbits.  A psuedo shaping pass is made over the expression tree
   to propagate lwb and length information up from the leaves of the tree, and to
   improve on the provided context if possible.
  
   SET FETCH calls its recursive child DO SET FETCH to traverse the expression tree
   and evaluate the set.  *)
  
public function set_fetch (set_exp: expr;
                           ctxt_lwbbit, ctxt_lenbits: set_range;
			   desired_loc: addr_desc; force_fixed: boolean): set_desc;
  
function rtime_bytelength: addr_desc; forward;
$PAGE do_set_fetch - in set_fetch
  
(* DO SET FETCH is the set analog of DO FETCH, performing the minimum of work
   necessary to produce a set_desc describing the evaluated set. Creation of an
   actual set is avoided if a descriptor of its value will suffice.  Context
   information originating from the end-user of the set (possibly improved by "shaping")
   is passed along as the set is recursively evaluated.  *)
  
function do_set_fetch (exp: expr;
		       ctxt_lwbbit, ctxt_lenbits: set_range;
		       desired_loc: addr_desc): set_desc;
  
  (* First, some conveniences: *)
  
  procedure make_temp_set (var set_operand: set_desc);
    var
      temp_set: set_desc;
    begin
      assert (set_operand.nargs > 0);
      temp_set := set_temporary (ctxt_lwbbit, rtime_bytelength);
      if set_operand.nargs <> 3 then
	gen_set (set_operand, temp_set)
      else
	set_move (set_operand, temp_set);
      set_operand := temp_set
    end;
  
  function desired: set_desc;
    begin
      with desired do begin
	nargs := 3;
	arg[1] := desired_loc;
	arg[2] := int_value (byte_num (ctxt_lwbbit));
	arg[3] := duplicate_addr (rtime_bytelength);
	in_temp := false
      end
    end;
  
  function singleton (set_op: set_desc): boolean;
    begin
      singleton := (set_op.nargs = 2) andif adr_equal (set_op.arg[1], set_op.arg[2])
    end;
  
  procedure prep_operand (var set_op: set_desc; leave_singleton: boolean);
    var
      temp: set_range;
    begin
      assert (set_op.nargs > 0);  (* if null gets here someone screwed up *)
      if leave_singleton andif singleton (set_op) then
        return;
      if (set_op.nargs <> 3) orif (not aconstp (rtime_bytelength, temp) and not set_op.in_temp) then
        make_temp_set (set_op)
    end;
  
  procedure reverse_ops (var first_op, second_op: set_desc);
    var
      temp: set_desc;
    begin
      temp := first_op;
      first_op := second_op;
      second_op := temp
    end;
  
  procedure choose_instr (len_addr: addr_desc; var emit_loop: boolean;
			  var vtype: vax_type; var defn: def);
    var
      len_is_const: boolean;
      const_len: set_range;
    begin
      len_is_const := aconstp (len_addr, const_len);
      if len_is_const andif (const_len in [1,2,4]) then begin
	case const_len of
	  1: vtype := vax_byte;
	  2: vtype := vax_word;
	  4: vtype := vax_long
	end;
	emit_loop := false
      end
      else begin
	if len_is_const then begin
	  if const_len mod 4 = 0 then begin
	    const_len := const_len div 4;
	    vtype := vax_long
	  end
	  else if const_len mod 2 = 0 then begin
	    const_len := const_len div 2;
	    vtype := vax_word
	  end
	  else
	    vtype := vax_byte;
	  gen2 (movl, int_value (const_len - 1), r0_addr)
	end
	else begin
	  gen3 (subl3, int_value (1), len_addr, r0_addr);
	  vtype := vax_byte
	end;
	defn := make_def (local_def);
	mark_def (code_area, defn);
	emit_loop := true
      end
    end;
  
  function in_context (arg_set: set_desc;  ctxt_lwbbit: set_range): boolean;
    begin
      in_context := (arg_set.arg[2].offset = byte_num (ctxt_lwbbit)) 
		       and adr_equal (arg_set.arg[3], rtime_bytelength)
    end;
  
  function indexed_addr (var addr: addr_desc; x_reg: registers): addr_desc;
    begin
      assert (indexable (addr));
      indexed_addr := addr; (* note: not duplicated *)
      indexed_addr.index := x_reg
    end;
  
  procedure internal_set_free (sdesc: set_desc);
    begin
      if (sdesc.nargs = 3) andif (sdesc.in_temp and not have_freed_temp) then begin
	freed_temp := sdesc;
	have_freed_temp := true
      end
      else
	set_free (sdesc)
    end;
$PAGE do_set_difference - in do_set_fetch (in set_fetch)
  
(* DO SET DIFFERENCE compiles a set difference expression tuple, and returns a
   set descriptor for its value.  *)
  
function do_set_difference: set_desc;
  
var
  first_op, second_op: set_desc;
  target: addr_desc;
  defn: def;
  vtype: vax_type;
  emit_loop, reuse_first_op, reuse_second_op: boolean;
  
begin
  with exp^ do begin
  
    first_op  := do_set_fetch (operand[1], ctxt_lwbbit, ctxt_lenbits, no_preference);
    second_op := do_set_fetch (operand[2], ctxt_lwbbit, ctxt_lenbits, no_preference);
  
    (* First, see if we can avoid performing the operation. *)
  
    if (desc.set_length = 0) orif (first_op.nargs = 0) then begin
      do_set_difference.nargs := 0; (* result is unavoidably null *)
      desc.set_lwb := 0;
      desc.set_length := 0;
      internal_set_free (first_op);
      internal_set_free (second_op);
      return  (* <-- return *)
    end;
  
    if (second_op.nargs = 0) orif
	 (operand[1]^.desc.set_lwb > 
	       upb (operand[2]^.desc.set_lwb, operand[2]^.desc.set_length)) orif
	 (operand[2]^.desc.set_lwb > 
	       upb (operand[1]^.desc.set_lwb, operand[1]^.desc.set_length)) then begin
      do_set_difference := first_op; (* second op is irrelevant to result *)
      internal_set_free (second_op)
    end
  
    (* have to really do it *)
  
    else begin
   
      (* Prepare the operands. *)
  
      prep_operand (first_op, false (* convert singletons *));
      prep_operand (second_op, true (* we'll special case singleton *));
      reuse_first_op := false; (* so we'll know what to free later *)
      reuse_second_op := false;
  
      (* Second operand is singleton. *)
  
      if second_op.nargs < 3 then begin
	if first_op.in_temp then begin
	  reuse_first_op := true;
	  do_set_difference := first_op
	end
	else if adr_equal (desired_loc, no_preference) then
	  do_set_difference := set_temporary (ctxt_lwbbit, rtime_bytelength)
	else
	  do_set_difference := desired;
	if not adr_equal (first_op.arg[1], do_set_difference.arg[1]) then begin
	  set_move (first_op, do_set_difference);
	  reuse_first_op := true (* prevent excess free *)
        end;
	emit_insv (0, second_op, do_set_difference)
      end
  
      (* Second operand is not singleton. *)
  
      else begin
	if not in_context (first_op, ctxt_lwbbit) then
	  make_temp_set (first_op);
	if (not first_op.in_temp andif not adr_equal (first_op.arg[1], desired_loc) andif
			  not in_context (second_op, ctxt_lwbbit)) then
	  make_temp_set (second_op);

	(* Choose destination. *)

	if not adr_equal (desired_loc, no_preference) then
	  if in_context (second_op, ctxt_lwbbit) orif
	     adr_equal (first_op.arg[1], desired_loc) then 
	    do_set_difference := desired
	  else (* that leaves first_op in_temp, second_op not in_context *) begin
	    reuse_first_op := true;
	    do_set_difference := first_op
	  end
	else (* no desired location *)
	  if first_op.in_temp then begin
	    reuse_first_op := true;
	    do_set_difference := first_op
	  end
	  else if second_op.in_temp then begin
	    reuse_second_op := true;
	    do_set_difference := second_op
	  end
	  else
	    do_set_difference := set_temporary (ctxt_lwbbit, rtime_bytelength);

	(* Decide on single instruction (or prepare for loop), then emit the actual code. *)

	choose_instr (second_op.arg[3], emit_loop, vtype, defn);

	if adr_equal (first_op.arg[1], do_set_difference.arg[1]) then begin
	  (* Go with 2-operand form, implicitly targeting back into first_op. *)
	  target := duplicate_addr (first_op.arg[1]);
	  if first_op.arg[2].offset < second_op.arg[2].offset then
	    target := offset_addr (target,
				   int_value (second_op.arg[2].offset - first_op.arg[2].offset));
	  if emit_loop then begin
	    gen2 (typ_opc (bicl2, vtype), indexed_addr (second_op.arg[1], r0), indexed_addr (target, r0));
	    gen2 (sobgeq, r0_addr, branch_addr (defn))
	  end
	  else
	    gen2 (typ_opc (bicl2, vtype), second_op.arg[1], target);
	  free (target)
	end

	else begin
	  (* Utilize 3-operand form. *)
	  if emit_loop then begin
	    gen3 (typ_opc (bicl3, vtype), indexed_addr (second_op.arg[1], r0), indexed_addr (first_op.arg[1], r0),
			    indexed_addr (do_set_difference.arg[1], r0));
	    gen2 (sobgeq, r0_addr, branch_addr (defn))
	  end
	  else
	    gen3 (typ_opc (bicl3, vtype), second_op.arg[1], first_op.arg[1], do_set_difference.arg[1])
	end
      end (* not singleton *);
  
      if not reuse_first_op then
	internal_set_free (first_op);
      if not reuse_second_op then
	internal_set_free (second_op)
    end (* have to really do it *);
  
    desc.set_lwb := operand [1]^.desc.set_lwb;
    desc.set_length := operand [1]^.desc.set_length
  end (* with *);
end (* do_set_difference *);
$PAGE do_intersection - in do_set_fetch (in set_fetch)
  
(* DO INTERSECTION compiles a set intersection expression tuple, and returns
   a set descriptor for its value.  *)
  
function do_intersection: set_desc;
  
var
  temp_expr: expr;
  first_op, second_op: set_desc;
  reuse_first_op, emit_loop, length_const: boolean;
  first_loc, second_loc, destination_loc, loop_length, temp_addr, dup_addr: addr_desc;
  loop_lwb, low_empty, loop_upb, high_empty: set_range;
  vtype: vax_type;
  defn: def;
  
begin
  with exp^ do begin
  
    first_op  := do_set_fetch (operand[1], ctxt_lwbbit, ctxt_lenbits, no_preference);
    second_op := do_set_fetch (operand[2], ctxt_lwbbit, ctxt_lenbits, no_preference);
  
    (* First, see if we can avoid performing the operation. *)
  
    if (desc.set_length = 0) orif (first_op.nargs = 0) orif (second_op.nargs = 0) then begin
      do_intersection.nargs := 0; (* result is unavoidably null *)
      desc.set_lwb := 0;
      desc.set_length := 0;
      internal_set_free (first_op);
      internal_set_free (second_op);
      return  (* <-- return *)
    end;
  
  
    (* Have to really do it. *)
  
    prep_operand (first_op, false (* convert singletons *));
    prep_operand (second_op, false);
  
    (* Reverse the operands if helpful in simplifying the possibilities.  At this
       point, each operand is a real set, but each may be
	  (1) in a temp (which implies in_context)
	  (2) in_context but not in a temp
	  (3) not in_context (lwb may be higher than context, upb might be lower).
       We can reduce the 9 combinations to 6 by reversing (if necessary) to ensure
	  (1) if only one is in a temp, its first_op
	  (2) if only one is not in_context, its second_op.        *)
  
    if (second_op.in_temp and not first_op.in_temp) orif
       (in_context (second_op, ctxt_lwbbit) and
	  not in_context (first_op, ctxt_lwbbit)) then begin
      reverse_ops (first_op, second_op);
      temp_expr := operand[1];
      operand[1] := operand[2];
      operand[2] := temp_expr
    end;
  
    (* Choose destination. *)
  
    reuse_first_op := false;
    if not adr_equal (desired_loc, no_preference) then
      do_intersection := desired
    else if first_op.in_temp then begin
      do_intersection := first_op;
      reuse_first_op := true
    end
    else
      do_intersection := set_temporary (ctxt_lwbbit, rtime_bytelength);
    if adr_equal (second_op.arg[1], do_intersection.arg[1]) then (* second_op equals desired location? *)
      reverse_ops (first_op, second_op); (* so we can take advantage of it *)
  
    (* Determine width of actual intersection operation.  If its not full
       contextual width, clear the portions of the destination outside the
       width and adjust the addresses accordingly.  *)
  
    first_loc := duplicate_addr (first_op.arg[1]);
    second_loc := duplicate_addr (second_op.arg[1]);
    destination_loc := duplicate_addr (do_intersection.arg[1]);
  
    if in_context (second_op, ctxt_lwbbit) then
      loop_length := second_op.arg[3]
    else begin
      loop_lwb := max (first_op.arg[2].offset, second_op.arg[2].offset);
      low_empty := loop_lwb - do_intersection.arg[2].offset;
      if low_empty > 0 then begin
	if not reuse_first_op orif  (* don't clear space already null *)
	   (byte_num (operand[1]^.desc.set_lwb) < loop_lwb) then
	  gen_c5 (movc5, int_value (0), stack_top_reference,
			 int_value (low_empty), destination_loc); (* clear *)
	first_loc := offset_addr (first_loc, int_value (loop_lwb - first_op.arg[2].offset));
	second_loc := offset_addr (second_loc, int_value (loop_lwb - second_op.arg[2].offset));
	destination_loc := offset_addr (destination_loc, int_value (low_empty))
      end;
      loop_upb := min (upb (first_op.arg[2].offset, first_op.arg[3].offset),
		       upb (second_op.arg[2].offset, second_op.arg[3].offset));
      loop_length := int_value (len (loop_upb, loop_lwb));
      high_empty := upb (do_intersection.arg[2].offset, do_intersection.arg[3].offset) - loop_upb;
      if (high_empty > 0) andif
	 (not reuse_first_op orif  (* don't clear space already null *)
	    (byte_num (upb (operand[1]^.desc.set_lwb, operand[1]^.desc.set_length))
		    > loop_upb)) then begin
        dup_addr := duplicate_addr (destination_loc);
        temp_addr := offset_addr (dup_addr, loop_length);
	gen_c5 (movc5, int_value (0), stack_top_reference,
		       int_value (high_empty), temp_addr); (* clear *)
	free (temp_addr)
      end
    end;
  
    (* Decide on single instruction (or prepare for loop), then emit the actual code. *)
  
    choose_instr (loop_length, emit_loop, vtype, defn);
  
    if adr_equal (first_loc, destination_loc) then
      (* Go with 2-operand form, implicitly targeting back into first_op. *)
      if emit_loop then begin
	gen2 (typ_opc (mcoml, vtype), indexed_addr (second_loc, r0), r1_addr);
	gen2 (typ_opc (bicl2, vtype), r1_addr, indexed_addr (first_loc, r0));
	gen2 (sobgeq, r0_addr, branch_addr (defn))
      end
      else begin
	gen2 (typ_opc (mcoml, vtype), second_loc, r1_addr);
	gen2 (typ_opc (bicl2, vtype), r1_addr, first_loc)
      end
    else
      (* Utilize 3-operand form. *)
      if emit_loop then begin
	gen2 (typ_opc (mcoml, vtype), indexed_addr (second_loc, r0), r1_addr);
	gen3 (typ_opc (bicl3, vtype), r1_addr, indexed_addr (first_loc, r0), indexed_addr (destination_loc, r0));
	gen2 (sobgeq, r0_addr, branch_addr (defn))
      end
      else begin
	gen2 (typ_opc (mcoml, vtype), second_loc, r1_addr);
	gen3 (typ_opc (bicl3, vtype), r1_addr, first_loc, destination_loc)
      end;
  
    free (first_loc);
    free (second_loc);
    free (destination_loc);
    internal_set_free (second_op);
    if not reuse_first_op then
      internal_set_free (first_op);
  
    desc.set_lwb := max (operand[1]^.desc.set_lwb, operand[2]^.desc.set_lwb);
    desc.set_length := len (min (upb (operand[1]^.desc.set_lwb, operand[1]^.desc.set_length),
			         upb (operand[2]^.desc.set_lwb, operand[2]^.desc.set_length)),
                            desc.set_lwb)
  end (* with *);
end (* do_intersection *);
$PAGE do_union - in do_set_fetch (in set_fetch)
  
(* DO UNION compiles a set union expression tuple, and returns a set descriptor
   for its value.  *)
  
function do_union: set_desc;
  
var
  i, num_const, num_nonconst: oper_range;
  current_op, const_op: set_desc;
  set_byte: set_byte_array;
  j, real_lwb, real_upb,
  alloc_len, const_bytelength, const_base, const_lwb, const_upb, first_const_elem: set_range;
  bit: 0..7;
  
procedure perform_or (var second_op: set_desc; last_op: boolean);
  
  var
    vtype: vax_type;
    destination: set_desc;
    target: addr_desc;
    emit_loop, reuse_do_union: boolean;
    defn: def;
  
  begin
  
    (* Prepare the operands.  *)
  
    if singleton (do_union) andif not singleton (second_op) then
      reverse_ops (second_op, do_union); (* cater to special casing of singleton operand *)
    prep_operand (do_union, false (* convert singletons *));
    prep_operand (second_op, true (* we'll special case singleton *));
    reuse_do_union := false; (* so we'll know what to free later *)
  
    (* Second operand is singleton. *)
  
    if second_op.nargs < 3 then begin
      if do_union.in_temp then begin
        reuse_do_union := true;
	destination := do_union
      end
      else if (not last_op) orif adr_equal (desired_loc, no_preference) then
        destination := set_temporary (ctxt_lwbbit, rtime_bytelength)
      else
	destination := desired;
      if not adr_equal (do_union.arg[1], destination.arg[1]) then begin
	set_move (do_union, destination); (* frees do_union *)
	reuse_do_union := true (* prevent excess free *)
      end;
      emit_insv (1, second_op, destination)
    end
  
    (* Second operand is not singleton. *)
  
    else begin
      if not in_context (do_union, ctxt_lwbbit) andif not second_op.in_temp then
	make_temp_set (do_union)
      else if not do_union.in_temp andif not in_context (second_op, ctxt_lwbbit) then
	make_temp_set (second_op);

      if (second_op.in_temp and not do_union.in_temp) orif
	 (in_context (second_op, ctxt_lwbbit) andif
	      not in_context (do_union, ctxt_lwbbit)) then
	reverse_ops (second_op, do_union);

      (* Choose the destination. *)

      if last_op andif (not adr_equal (desired_loc, no_preference)) andif
	 in_context (second_op, ctxt_lwbbit) then
	destination := desired
      else if do_union.in_temp then begin
	destination := do_union;
	reuse_do_union := true
      end
      else
	destination := set_temporary (ctxt_lwbbit, rtime_bytelength);

      (* Decide on single instruction (or prepare for loop), then emit the actual union code. *)

      choose_instr (second_op.arg[3], emit_loop, vtype, defn);

      if adr_equal (second_op.arg[1], destination.arg[1]) then
	reverse_ops (second_op, do_union); 
      if adr_equal (do_union.arg[1], destination.arg[1]) then begin
	(* Go with 2-operand form, implicitly targeting back into do_union. *)
	target := duplicate_addr (do_union.arg[1]);
	if do_union.arg[2].offset < second_op.arg[2].offset then
	  target := offset_addr (target,
				 int_value (second_op.arg[2].offset - do_union.arg[2].offset));
	if emit_loop then begin
	  gen2 (typ_opc (bisl2, vtype), indexed_addr (second_op.arg[1], r0), indexed_addr (target, r0));
	  gen2 (sobgeq, r0_addr, branch_addr (defn))
	end
	else
	  gen2 (typ_opc (bisl2, vtype), second_op.arg[1], target);
	free (target)
      end

      else begin
	(* Utilize 3-operand form. *)
	if emit_loop then begin
	  gen3 (typ_opc (bisl3, vtype), indexed_addr (second_op.arg[1], r0), indexed_addr (do_union.arg[1], r0),
							      indexed_addr (destination.arg[1], r0));
	  gen2 (sobgeq, r0_addr, branch_addr (defn))
	end
	else
	  gen3 (typ_opc (bisl3, vtype), second_op.arg[1], do_union.arg[1], destination.arg[1])
      end
    end (* not singleton *);
  
    internal_set_free (second_op);
    if not reuse_do_union then
      internal_set_free (do_union);
    do_union := destination
  end (* perform_or *);
  
  
begin
  with exp^ do begin
    num_const := 0;
    const_bytelength := 0;
    num_nonconst := 0;
    real_upb := 0;
    real_lwb := set_upb_limit;
  
    (* Process the operands. *)
  
    for i := 1 to upperbound (operand) do begin
      current_op := do_set_fetch (operand[i], ctxt_lwbbit, ctxt_lenbits, no_preference);
      if current_op.nargs > 0 then with operand[i]^ do begin
	real_lwb := min (desc.set_lwb, real_lwb);
	real_upb := max (upb (desc.set_lwb, desc.set_length), real_upb)
      end;
  
      (* Sift out the constants and combine them (compile-time). *)
  
      if (current_op.nargs = 2) andif (aconstp (current_op.arg[1], const_lwb) and
				       aconstp (current_op.arg[2], const_upb)) then begin
        if const_bytelength = 0 then begin (* first constant operand *)
	  const_base := byte_num (ctxt_lwbbit) * bits_per_byte;  (* basing modulo 8 *)
	  if aconstp (rtime_bytelength, alloc_len) then
	    first_const_elem := 0 (* stick with ctxt_lower bound *)
	  else begin
	    alloc_len := len_bytes (ctxt_lwbbit, ctxt_lenbits);
	    first_const_elem := const_lwb - const_base
	  end;
          new (set_byte, alloc_len);
          for j := 1 to alloc_len do
	    for bit := 0 to 7 do
	      set_byte^[j][bit] := false;
	  const_op := current_op (* remember first in case its only one *)
        end;
	num_const := num_const + 1;
        for j := const_lwb - const_base to const_upb - const_base do
          set_byte^[byte_num (j) + 1] [j mod bits_per_byte] := true;
        const_bytelength := max (byte_num (const_upb - const_base) + 1, const_bytelength);
	first_const_elem := min (const_lwb - const_base, first_const_elem)
      end
  
      (* Combine nonconstant operands (run-time). *)
  
      else if current_op.nargs > 0 then begin
        num_nonconst := num_nonconst + 1;
        if num_nonconst = 1 then
          do_union := current_op
        else
	  perform_or (current_op, i = upperbound (operand) (* last operand? *))
      end
    end (* for *);
  
    (* Finish up by combining resultant constant (if any) and nonconstant (if any). *)

    if num_const > 0 then begin
      if (num_const > 1) orif not singleton (const_op) then
	const_op := gen_cst_set (set_byte, ctxt_lwbbit,
				 const_bytelength, byte_num (first_const_elem) + 1);
      if num_nonconst = 0 then
	do_union := const_op
      else
	perform_or (const_op, true);
      dispose (set_byte)
    end;
  
    if real_upb >= real_lwb then begin
      desc.set_lwb := real_lwb;
      desc.set_length := len (real_upb, real_lwb)
    end
    else begin
      do_union.nargs := 0;
      desc.set_lwb := 0;
      desc.set_length := 0
    end
  end (* with *);
end (* do_union *);
$PAGE do_set_fetch - body (in set_fetch)
  
var
  ctxt_upbbit,
  ctxt_lwbbyte, set_lwb_byte, ctxt_upb_byte, set_upb_byte,
  desc_upb, const_lwb, const_upb: set_range;
  
begin
  ctxt_upbbit := upb (ctxt_lwbbit, ctxt_lenbits);
  with exp^, do_set_fetch do begin
  
    (* Evaluate the expression. *)
  
    case opcode of
  
      cst_ref,
      ident_ref,
      field_ref,
      buffer_ref,
      ptr_ref,
      array_ref,
      func_call_op: begin (* note: only sets handled here *)
        arg[1] := fetch (exp, no_preference);
	assert (not is_register (arg[1]));
	if (ctxt_upbbit < desc.set_lwb) or
	   (upb (desc.set_lwb, desc.set_length) < ctxt_lwbbit) then begin
	  nargs := 0; (* "z" format *)
	  desc.set_lwb := 0;
	  desc.set_length := 0;
	  free (arg[1])
	end
	else (* not null *) begin
	  nargs := 3; (* "l" format *)
	  ctxt_lwbbyte := byte_num (ctxt_lwbbit);
	  set_lwb_byte := byte_num (desc.set_lwb);
	  (* ensure that actual lowerbound byte is no lower than contextual
	     lowerbound byte *)
	  if set_lwb_byte < ctxt_lwbbyte then begin
	    desc.set_length := desc.set_length - (ctxt_lwbbyte * bits_per_byte - desc.set_lwb);
	    desc.set_lwb := ctxt_lwbbyte * bits_per_byte;
	    arg[1] := offset_addr (arg[1], int_value (ctxt_lwbbyte - set_lwb_byte))
	  end;
	  arg[2] := int_value (byte_num (desc.set_lwb)); (* lowerbound byte *)
	  ctxt_upb_byte := byte_num (ctxt_upbbit);
	  set_upb_byte := byte_num (upb (desc.set_lwb, desc.set_length));
	  arg[3] := int_value (len (min (ctxt_upb_byte, set_upb_byte), arg[2].offset));
	  if set_upb_byte > ctxt_upb_byte then
	    desc.set_length := len (((ctxt_upb_byte + 1) * bits_per_byte - 1), desc.set_lwb)
	end;
	in_temp := (opcode = func_call_op) andif in_context (do_set_fetch, ctxt_lwbbit)
      end;
  
      setcvt_op: begin
        do_set_fetch := do_set_fetch (operand[1], ctxt_lwbbit, ctxt_lenbits, desired_loc);
        desc.set_lwb := operand[1]^.desc.set_lwb;
        desc.set_length := operand[1]^.desc.set_length
      end;

      gen_set_op: begin
        if upperbound (operand) = 0 (* null set *) then begin
          nargs := 0; (* "z" format *)
	  desc.set_lwb := 0;
	  desc.set_length := 0
	end
        else (* upperbound (operand) = 1 or 2 *) begin
          nargs := 2; (* "o" format *)
          arg[1] := fetch_fullword (operand[1]);
          lwb_exp := operand[1];
          if upperbound (operand) = 1 then begin (* singleton set *)
            arg[2] := duplicate_addr (arg[1]);
            upb_exp := lwb_exp
          end
          else begin (* range set *)
            arg[2] := fetch_fullword (operand[2]);
            upb_exp := operand[2]
          end;

          (* detect null set *)

          if (ctxt_lenbits = 0) orif (desc.set_length = 0) orif 
             (ctxt_lwbbit > upb (desc.set_lwb, desc.set_length)) orif
             (desc.set_lwb > ctxt_upbbit)  then begin
            nargs := 0;
	    desc.set_lwb := 0;
	    desc.set_length := 0;
            free (arg[1]);
            free (arg[2])
          end
	  else begin
            if aconstp (arg[1], const_lwb) andif (const_lwb < ctxt_lwbbit) then begin
              arg[1].offset := ctxt_lwbbit;
	      lwb_exp^.cst_val.ival := ctxt_lwbbit;
	      desc.set_length := desc.set_length - (ctxt_lwbbit - desc.set_lwb);
	      desc.set_lwb := ctxt_lwbbit
	    end;
            if aconstp (arg[2], const_upb) andif (const_upb > ctxt_upbbit) then begin
              arg[2].offset := ctxt_upbbit;
	      upb_exp^.cst_val.ival := ctxt_upbbit;
	      desc.set_length := len (ctxt_upbbit, desc.set_lwb)
	    end;
	    desc_upb := upb (desc.set_lwb, desc.set_length);
	    desc.set_lwb := max (desc.set_lwb, byte_num (ctxt_lwbbit) * bits_per_byte);
	    desc.set_length := len (min (desc_upb, (byte_num (ctxt_upbbit) + 1) * bits_per_byte - 1),
				    desc.set_lwb)
	  end
        end (* noper = 1 or 2 *);
      end;


      union_op:
        do_set_fetch := do_union;

      both_op:
        do_set_fetch := do_intersection;

      diff_op:
        do_set_fetch := do_set_difference;

      others:
        assert (false)
    end
  end
end (* do_set_fetch *);
$PAGE set_fetch - body
  
var
  shaped_lwb, shaped_len: set_range;
  rt_bytelength, rtime_upb, target, temp: addr_desc;
  defn: def;
  formed_rt_bytelength, ltz_check_reqd: boolean;
  
  
function rtime_bytelength (* : addr_desc *);
  
  begin
    if not formed_rt_bytelength then begin
      if byte_num (shaped_lwb) > 0 then
	if is_register(rtime_upb) then
	  gen2 (subl2, int_value (byte_num (shaped_lwb) * bits_per_byte), rtime_upb)
	else begin
	  temp := get_typ_reg_addr (vax_long);
	  gen3 (subl3, int_value (byte_num (shaped_lwb) * bits_per_byte), rtime_upb, temp);
	  rtime_upb := temp
	end;
      if ltz_check_reqd then begin
	if is_register (rtime_upb) then
	  gen1 (tstl, rtime_upb)
	else begin
	  temp := get_typ_reg_addr (vax_long);
	  gen2 (movl, rtime_upb, temp);
	  rtime_upb := temp
	end;
	defn := make_def (local_def);
	gen_branch (bgeq, defn);
	gen1 (clrl, rtime_upb);
	mark_def (code_area, defn)
      end;
      if is_register (rtime_upb) then
	gen2 (divl2, int_value (bits_per_byte), rtime_upb)
      else begin
	temp := get_typ_reg_addr (vax_long);
	gen3 (divl3, int_value (bits_per_byte), rtime_upb, temp);
	rtime_upb := temp
      end;
      gen1 (incl, rtime_upb);
      rt_bytelength := rtime_upb;
      formed_rt_bytelength := true
    end;
    rtime_bytelength := rt_bytelength
  end (* rtime_bytelength *);
  
  
function find_runtime_upb (set_exp: expr; var ltz_check_required: boolean): addr_desc;
  
  var
    first_op, second_op: addr_desc;
    reg: registers;
    i: oper_range;
    min_val, max_val: int_type;
    fixed, ltzchk_2: boolean;
    defn: def;

  begin
    with set_exp^ do begin

      if desc.set_cst_len then
	find_runtime_upb := int_value (upb (desc.set_lwb, desc.set_length))

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
	    ltz_check_required := min_val < byte_num (shaped_lwb) * bits_per_byte
	  end;

	  both_op: begin
	    first_op := find_runtime_upb (operand[1], ltz_check_required);
	    second_op := find_runtime_upb (operand[2], ltzchk_2);
	    reg := load_addr (first_op, signed_value);
	    defn := make_def (local_def);
	    gen_rm (cmpl, reg, second_op);
	    gen_branch (bleq, defn);
	    gen_mr (movl, second_op, reg);
	    mark_def (code_area, defn);
	    free (second_op);
	    find_runtime_upb := reg_addr (reg);
	    ltz_check_required := ltz_check_required or ltzchk_2
	  end;

	  union_op: begin
	    first_op := find_runtime_upb (operand[1], ltz_check_required);
	    reg := load_addr (first_op, signed_value);
	    for i := 2 to upperbound (operand) do begin
	      second_op := find_runtime_upb (operand[i], ltzchk_2);
	      defn := make_def (local_def);
	      gen_rm (cmpl, reg, second_op);
	      gen_branch (bgeq, defn);
	      gen_mr (movl, second_op, reg);
	      mark_def (code_area, defn);
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
  
begin
  
  (* propagate lwb and length information up from the leaves of the expression tree *)
  
  shape_set (set_exp, shaped_lwb, shaped_len);
  
  (* use best combination of information possible *)
  
  if ctxt_lwbbit > shaped_lwb then begin
    shaped_len := max (0, shaped_len - (ctxt_lwbbit - shaped_lwb));
    shaped_lwb := ctxt_lwbbit
  end;
  if (shaped_lwb + shaped_len) > (ctxt_lwbbit + ctxt_lenbits) then
    shaped_len := max (0, ctxt_lwbbit + ctxt_lenbits - shaped_lwb);
  if shaped_len = 0 then
    shaped_lwb := 0; (* keep null set reps. consistent: len=lwb=0 *)
  
  target := no_preference;
  if (force_fixed or set_exp^.desc.set_cst_len) orif
     (len_bytes (shaped_lwb, shaped_len) <= 128) then begin
    rt_bytelength := int_value (len_bytes (shaped_lwb, shaped_len));
    formed_rt_bytelength := true;
    if (byte_num (shaped_lwb) = byte_num (ctxt_lwbbit)) andif
       (rt_bytelength.offset = len_bytes (ctxt_lwbbit, ctxt_lenbits)) then
      target := desired_loc
  end
  else begin
    rtime_upb := find_runtime_upb (set_exp, ltz_check_reqd);
    formed_rt_bytelength := false (* defer conversion until bytelength required *)
  end;
  
  (* generate code required to evaluate the set *)
  
  have_freed_temp := false; (* within this module we'll try to reuse temps *)
  
  set_fetch := do_set_fetch (set_exp, shaped_lwb, shaped_len, target);
  
  if have_freed_temp then begin
    set_free (freed_temp);
    have_freed_temp := false
  end;
  if formed_rt_bytelength then
    free (rt_bytelength)
  else
    free (rtime_upb) (* never did have to calculate length from upb *)
 
end;
$PAGE set_assignment
  
(* SET ASSIGNMENT generates code for assignment of set values.  *)
  
public procedure set_assignment (assign_tpl: tuple);
  
var
  rhs_desc, lhs_desc: set_desc;
  
begin
  with assign_tpl^ do begin
    with lhs_desc do begin
      nargs := 3;
      arg[1] := fetch (lhs, no_preference);
      arg[2] := int_value (byte_num (lhs^.desc.set_lwb));
      arg[3] := int_value (setb_size (lhs))
    end;
    rhs_desc := set_fetch (rhs, lhs^.desc.set_lwb, lhs^.desc.set_length, lhs_desc.arg[1], true);
    
    if rhs_desc.nargs = 0 (* "z" format *) then
      gen_c5 (movc5, int_value (0), stack_top_reference, lhs_desc.arg[3], lhs_desc.arg[1]) (* clear *)
  
    else begin
      if rhs_desc.nargs <> 3 then
	gen_set (rhs_desc, lhs_desc)
      else if not adr_equal (lhs_desc.arg[1], rhs_desc.arg[1]) then  (* isn't where I wanted it? *)
	set_move (rhs_desc, lhs_desc);
      clip_bits (lhs_desc.arg[1], rhs^.desc.set_lwb, rhs^.desc.set_length,
				  lhs^.desc.set_lwb, lhs^.desc.set_length)
    end;
  
    set_free (lhs_desc)
  end
end (* set_assignment *);
$PAGE set_parameter
  
(* SET PARAMETER evaluates set parameters. *)
  
public function set_parameter (actual_expr: expr; parm_kind: sym_kind;
			       ctxt_lwbbit, ctxt_upbbit: set_range): addr_desc;
  
var
  ctxt_lenbits, ctxt_len_bytes: set_range;
  set_loc, temp_set: set_desc;
  clip_required: boolean;
  
begin
  ctxt_lenbits := len (ctxt_upbbit, ctxt_lwbbit);
  ctxt_len_bytes := len_bytes (ctxt_lwbbit, ctxt_lenbits);
  set_loc := set_fetch (actual_expr, ctxt_lwbbit, ctxt_lenbits, no_preference, true);
  
  if parm_kind = values then begin
    if set_loc.nargs = 0 then begin
      set_loc := set_temporary (ctxt_lwbbit, int_value (ctxt_len_bytes));
      gen_c5 (movc5, int_value (0), stack_top_reference, int_value (ctxt_len_bytes), set_loc.arg[1]) (* clear *)
    end
    else begin
      if set_loc.nargs <> 3 then begin
	temp_set := set_temporary (ctxt_lwbbit, int_value (ctxt_len_bytes));
	gen_set (set_loc, temp_set);
	set_loc := temp_set
      end
      else begin
        with actual_expr^.desc do
	  clip_required := (set_lwb < ctxt_lwbbit) orif (upb (set_lwb, set_length) > ctxt_upbbit);
	if (set_loc.arg[2].offset > byte_num (ctxt_lwbbit)) orif
	   (set_loc.arg[3].offset < ctxt_len_bytes) orif
	   (not set_loc.in_temp andif clip_required) then begin
	  temp_set := set_temporary (ctxt_lwbbit, int_value (ctxt_len_bytes));
	  set_move (set_loc, temp_set);
	  set_loc := temp_set
	end
      end;
      with actual_expr^.desc do
	clip_bits (set_loc.arg[1], set_lwb, set_length, ctxt_lwbbit, ctxt_lenbits)
    end
  end;
  
  assert (set_loc.arg[1].byte_size = vax_byte);
  set_parameter := set_loc.arg[1]
end (* set parameter *).
    2(LH‡