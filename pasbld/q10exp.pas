$TITLE Q10EXP - quick pass expression evaluation and register manipulation
module q10exp;
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE ptmcon.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$system q10dsc.typ
$INCLUDE pasifu.inc
$INCLUDE p10cgu.inc
$INCLUDE p10opc.inc
$INCLUDE q10cmp.inc
$INCLUDE q10cll.inc
$INCLUDE q10gen.inc
$INCLUDE pasmth.inc
$INCLUDE pascv.inc
$INCLUDE q10set.inc
$INCLUDE q10dsc.inc
$INCLUDE pastal.inc
$PAGE global data and forward declarations
public var
  regdesc: reg_descriptor;      (* for all registers *)
  with_base: registers;         (* lowest register in use by withs *)
  maxwith_regs: registers;	(* maximum number of with-regs that had to be 
				   saved at any one time *)
  savewithaddr: addr_desc;	(* descriptor for locations of block of temps for
				   saving of with-regs.  The actual location will
				   filled in by COMPILE_BODY.  *)

public function load (exp: expr; size: bit_range): registers; forward;
public function load_addr (addr: addr_desc; align: data_alignment; size: bit_range): registers; forward;
public function fetch (exp: expr): addr_desc  options special(coercions); forward;
public function fetch_fullword (exp: expr): addr_desc; forward;
public function argument (exp: expr): addr_desc; forward;
public function is_register (maddr: addr_desc): boolean; forward;
  
type
    op_attrs =
        (  commutative,                 (* operands may be exchanged *)
           mem_form,                    (* opcM may be generated *)
           double       );              (* must aply inst twice to process 2 word operands *)
    op_attr_set = set of op_attrs;
  
public function do_binary_op
        ( opcode: opc_range;            (* instruction to emit *)
          operand1, operand2: expr;     (* left and right operands *)
          attributes: op_attr_set       (* properties of instruction to generate *)
                ): registers; forward;  (* register containing result returned *)
  
static var
  with_table: array [2..14b] of packed record
		save_inst: code; (* ptr to instr. copying with-reg to temp *)
		restored:  boolean (* flag indicating it temp used to restore with-reg *)
	      end;
$PAGE clr_rv
public procedure clr_rv;
begin end;
$PAGE precision, alignment
(* PRECISION returns the bit size of an arithmetic value, i.e. integer (or other
   scalar) or real. *)

public function precision (exp: expr): bit_range;
 begin
  if exp^.desc.kind = reals
    then begin
      if exp^.desc.precision > srealprec
        then precision := 2 * bits_per_unit
        else precision := bits_per_unit;
    end
    else precision := min (exp^.desc.int_prec, bits_per_unit);
 end;



(* ALIGNMENT returns the data alignment type for an expression ("exp"). *)

public function alignment (exp: expr): data_alignment;
 begin
  case exp^.desc.kind of
    ints, bools, chars, scalars, pointers:
      if exp^.desc.signed
        then alignment := signed_value
        else alignment := unsigned_value;
    reals:
      alignment := right_aligned;               (* always occupies full words *)
    arrays, records:
      alignment := left_unpadded;
    others:
      alignment := left_aligned
  end;
 end;
$PAGE reg_init
(* REG INIT initializes all registers descriptors to a free state. *)

public procedure reg_init;
 var reg: registers;
 begin
  for reg := minimum (registers) to maximum (registers) do begin
    with regdesc[reg] do begin
      associate := noreg;
      uses_remaining := 0
    end;
  end;
  with_base := 15B;              (* withs will be allocated from 14b down *)
  maxwith_regs := 0;	(* keep track of max number of with-regs that had
			   to be saved at any one time so COMPILE_BODY will
			   know how large a block of temps to allocate *)
  savewithaddr := absolute_reference;
  with savewithaddr do begin
    index := sp;
    reloc.kind := def_sc;
    reloc.reldef := make_def (local_def)
  end
 end;
$PAGE decr_reg_usages
(* DECR_REG_USAGES decrements the usage count for a register and
   any associates.  *)

public procedure decr_reg_usages (reg: registers);

begin
  if (reg <> noreg) andif (reg < 15B) then
    with regdesc[reg] do begin
      assert (uses_remaining > 0);
      uses_remaining := uses_remaining - 1;
      if associate <> noreg then
        with regdesc[associate] do begin
	  assert (uses_remaining > 0);
          uses_remaining := uses_remaining - 1;
          if uses_remaining <= 0 then begin
            assert (regdesc [associate].associate <> noreg);
            regdesc [associate].associate := noreg;
            associate := noreg
          end
        end;
      if (uses_remaining <= 0) andif (associate <> noreg) then begin
        assert (regdesc [associate].associate <> noreg);
        regdesc [associate].associate := noreg;
        associate := noreg
      end
    end
end;
$PAGE free_and_disassociate
(* FREE AND DISASSOCIATE is similar to decr_reg_usages, but is used where the
   intention is to release only one of a pair of registers, not both.  *)
  
public procedure free_and_disassociate (reg: registers);
  
begin
  if (reg <> noreg) andif (reg < 15B) then
    with regdesc[reg] do begin
      uses_remaining := uses_remaining - 1;
      assert (uses_remaining <> maximum (usage_range) );
      if (uses_remaining <= 0) andif (associate <> noreg) then begin
        assert (regdesc [associate].associate <> noreg); (* should be a symmetric relationship *)
        regdesc [associate].associate := noreg;
        associate := noreg
      end
    end
end;
$PAGE tag_reg
(* TAG REG associates two registers *)

public procedure tag_reg (reg1, reg2: registers);

begin
  regdesc[reg1].associate := reg2;
  regdesc[reg2].associate := reg1;
end;
$PAGE regs_used
(* REGS USED returns the register used by an address descriptor, if any. *)

public function regs_used (maddr: addr_desc): registers;

begin
  if (maddr.reloc.kind = register_sc) andif
      ( (maddr.mode <> byte) orif (maddr.index = 0) ) then
    regs_used := maddr.offset
  else if (maddr.index > 1) then
    regs_used := maddr.index
  else
    regs_used := noreg
end;
$PAGE free
(* FREE releases any index register associated with an ADDR_DESC.  *)

public procedure free (maddr: addr_desc);
  
begin
  decr_reg_usages (regs_used (maddr));
end;
$PAGE clear
(* CLEAR zeros a specified register (pair). *)

public procedure clear (reg: registers);
  
begin
  if regdesc[reg].associate = 0
    then gen_ri (setz, reg, 0)
    else gen_rr (setzb, reg, reg+1);    (* assume contiguous *)
end;
$PAGE get_reg
(* GET REG allocates a register or register pair (depending on the value of "p");
   the register allocated is returned.  *)

public function get_reg (p: bit_range): registers;

var   reg: registers;

begin
  for reg := 2 to 14B - ord (p > bits_per_unit) do
    if (regdesc[reg].uses_remaining = 0) andif
       ( (p <= bits_per_unit) orif (regdesc[reg + 1].uses_remaining = 0) ) then begin
      regdesc[reg].uses_remaining := 1;
      if p > bits_per_unit then begin
        regdesc[reg + 1].uses_remaining := 1;
        tag_reg (reg, reg+1)
      end
      else regdesc[reg].associate := noreg;
      get_reg := reg;
      return;           (* <--- short-circuit return *)
    end;
  writeln (tty, '?Register overflow at location ', cv_source_id (cur_source));
  assert (false)
end;
$PAGE reg_addr
(* REG_ADDR is passed a register and returns an address descriptor
   for the register.  *)

public function reg_addr ( reg: registers ): addr_desc;

begin
  reg_addr := reg_addr_desc;
  reg_addr.offset := reg;
end;
$PAGE incr_reg_usages
(* INCR_REG_USAGES increments the USES_REMAINING field for a register
   and any associated registers.  *)

public procedure incr_reg_usages ( reg: registers; additional_uses: usage_range );

begin
  if (reg <> noreg) andif (reg < 15B) then
    with regdesc[reg] do begin
      uses_remaining := uses_remaining + additional_uses;
      if associate <> noreg then
        with regdesc[associate] do 
          uses_remaining := uses_remaining + additional_uses
    end;
end;
$PAGE with_start
(* WITH START initializes a with-descriptor for a new with construct *)

public procedure with_start (with_tpl: tuple)  options special(coercions);
  
var
  addr_ref: addr_ptr;
  i: usage_range;
  save_loc,
  maddr: addr_desc;
  reg: registers;

begin
  
  maddr := fetch (with_tpl);    (* get addressibility for record *)
  free (maddr);
  
  if maddr.indirect orif
      ((maddr.index <> noreg) and (maddr.index < with_base)) then begin

    (* load address into an index register *)

    reg := regs_used (maddr);
    if (reg <> noreg) and (reg < 15B) then
      for i := 1 to with_tpl^.usage_count do
        decr_reg_usages (reg);
    with_base := with_base - 1;  (* allocate a register ... *)
    save_loc := savewithaddr; (* ... and figure out where to save it *)
    save_loc.offset := with_base - 14b;
    if regdesc[with_base].uses_remaining <> 0 then begin
      writeln (tty, '?WITH register overflow at location ', cv_source_id (cur_source));
      assert (false)
    end;
    regdesc[with_base].associate := noreg;
    regdesc[with_base].uses_remaining := with_tpl^.usage_count;
  
    gen_rm (movei, with_base, maddr); (* load with-reg ... *)
    gen_rm (movem, with_base, save_loc); (* ... and save it *)
    with with_table [with_base] do begin
      restored := false; (* so we can tell later if save is necessary *)
      save_inst := code_area.last (* so we can delete it if its not *)
    end;
  
    dispose (with_tpl^.result);
    new (addr_ref);
    addr_ref^ := absolute_reference;
    addr_ref^.index := with_base;
    with_tpl^.result := ptr (ord (addr_ref))
  end
end;
$PAGE with_end, with_restore
(* WITH END terminates a with construct *)

public procedure with_end (with_tpl: tuple);
  
begin
  free ( fetch (with_tpl) );
  if (with_base < 15B) andif (regdesc [with_base].uses_remaining = 0) then begin
    if not with_table [with_base].restored then
      with_table [with_base].save_inst^.kind := nullcode; (* delete the save *)
    with_base := with_base + 1
  end
end;
  
  
  
(* WITH RESTORE reloads the active with registers from the temporaries where WITH START
   saved them.  This is necessary after procedure and function calls, and at the
   beginning of handler clauses.  *)
  
public procedure with_restore;
 
var
  index: registers;
  save_loc: addr_desc;
  
begin
  if with_base > 14b then
    return;  (* no withs require restoration *)
  maxwith_regs := max (maxwith_regs, 15b-with_base);
  save_loc := savewithaddr;
  save_loc.offset := with_base - 14b;
  index := with_base;
  if not odd (index) then begin
    gen_rm (move, index, save_loc);
    with_table [index].restored := true;
    index := index + 1;
    save_loc.offset := save_loc.offset + 1
  end;
  while index < 14b do begin
    gen_rm (dmove, index, save_loc);
    with_table [index].restored := true;
    with_table [index+1].restored := true;
    index := index + 2;
    save_loc.offset := save_loc.offset + 2
  end
end;
$PAGE is_register
(* IS REGISTER returns a flag indicating whether or not an address ("maddr")
   designates a register. *)

public function is_register (* maddr: addr_desc): boolean *);
  
begin
  is_register := (maddr.reloc.kind = register_sc) andif
                 (maddr.mode <> byte) andif
                 (not ( maddr.immediate or maddr.indirect ) ) andif
                 (maddr.offset > 1);            (* if value in 0 or 1, copy it *)
end;
$PAGE left_shift, right_shift
(* LEFT SHIFT emits code to shift a register ("reg") or register pair (determined
   by the precision "p") left "n" bits. *)

public procedure left_shift (reg: registers; p: bit_range; n: int_type);
  
begin
  if n <> 0 then begin
    if p <= bits_per_unit
      then gen_ri (lsh, reg, n)
      else gen_ri (lshc, reg, n);
  end;
end;



(* RIGHT SHIFT emits code to shift a register ("reg") or register pair (determined
   by the precision "p") right "n" bits. *)

public procedure right_shift (reg: registers; p: bit_range; n: int_type);
  
begin
  if n <> 0 then begin
    if p <= bits_per_unit
      then gen_ri (lsh, reg, -n)
      else gen_ri (lshc, reg, -n);
  end;
end;
$PAGE do_move
(* DO MOVE moves the contents of a memory location ("mem"), with any "alignment",
   into a register or register pair ("reg") depending on the specified precision ("p"). *)

public procedure do_move (reg: registers; mem: addr_desc; alignment: data_alignment; p: bit_range);
 begin
  case mem.mode of
    fw:     if p <= bits_per_unit
              then gen_rm (move, reg, mem)
              else gen_rm (dmove, reg, mem);

    lhw:    if (alignment = signed_value) and mem.has_sign
              then gen_rm (hlre, reg, mem)
            else if alignment = left_aligned
              then gen_rm (hllz, reg, mem)
            else if alignment = left_unpadded
              then gen_rm (hll, reg, mem)
            else gen_rm (hlrz, reg, mem);

    rhw:    if (alignment = signed_value) and mem.has_sign
              then gen_rm (hrre, reg, mem)
            else if alignment = left_aligned
              then gen_rm (hrlz, reg, mem)
            else if alignment = left_unpadded
              then gen_rm (hrl, reg, mem)
            else gen_rm (hrrz, reg, mem);

    byte:   begin
              gen_rm (ldb, reg, mem);           (* load right justified *)
              if alignment in [left_aligned, left_unpadded] then
                left_shift (reg, bits_per_unit, 36 - mem.slice_size)
	      else if (alignment = signed_value) and mem.has_sign then begin
		assert (mem.slice_size = 18);
		gen_rx (hrrei, reg, reg)
	      end
            end;

    slice:  if alignment in [left_aligned, left_unpadded] then begin (* load, shift, and mask *)
	      gen_rm (move, reg, mem);
	      left_shift (reg, bits_per_unit - mem.slice_offset, mem.slice_offset);
	      if alignment = left_aligned then
		left_mask (reg, bits_per_unit, mem.slice_size);
	    end
	    else begin
	      gen (ldb, reg, 0, 0, gen_bptr (mem));
	      if (alignment = signed_value) and mem.has_sign then begin
		assert (mem.slice_size = 18);
		gen_rx (hrrei, reg, reg)
	      end
	    end
  end;
 end;
$PAGE do_move_op, non_immed_move
(* DO MOVE OP generates an instruction for that class of operations which can be
   done in a moving instruction, e.g. abs, arithmetic negation;  "opc" gives the
   opcode, "exp" the node of the operator
   being compiled (not the operand), and "p" gives the precision of the operand. *)

function do_move_op ( opc: opc_range; exp: expr; p: bit_range ): registers;

 var
   maddr: addr_desc;

 begin
  maddr := fetch_fullword (exp^.operand[1]);
  free (maddr); (* permit reuse of index register *)
  do_move_op := get_reg ( p );
  gen_rm (opc, do_move_op, maddr);
 end;


(* NON_IMMED_MOVE is identical to DO_MOVE_OP except that an immediate
   instruction will not be generated.  It is useful for opcodes which
   do not have an immediate variation (e.g. FIXR ).  *)

function non_immed_move ( opc: opc_range; exp: expr; p: bit_range ): registers;

 var
   maddr: addr_desc;
   temp_reloc: rel_syllable;

 begin
  maddr := fetch_fullword (exp^.operand[1]);
  free (maddr); (* permit reuse of index register *)
  non_immed_move := get_reg ( p );
  
  if maddr.immediate then begin                 (* immediate values will be generated *)
    temp_reloc := gen_cint ( maddr.offset );    (* in the constant area *)
    maddr := absolute_reference;
    maddr.reloc := temp_reloc;
  end;
  gen_rm (opc, non_immed_move, maddr);
 end;
$PAGE rt_call
(* RT CALL generates a runtime routine call for a n (n <= 4) operand function, given
   "exp" the operator node, and "rts" the
   single precision runtime routine.  The n operands are fetched, freed and passed
   to the runtime routine.  A result register is allocated, and tagged as holding
   "exp".   It is assumed that a double precision routine's symbol is the successor
   of the single precision routine's symbol. *)

function rt_call (exp: expr; rts: rt_symbol ): registers;

 var
   reg: registers;
   mem: array[1..4] of addr_desc;
   p: bit_range;
   i: 0..5;
  immed_val: unit_range;

 begin
  with exp^ do begin

    for i := 1 to upperbound (operand) do begin       (* fetch the operands *)
      mem[i] := fetch_fullword (operand[i]);
      if mem[i].immediate then begin
        immed_val := mem[i].offset;
        mem[i] := absolute_reference;
        mem[i].reloc := gen_cint ( immed_val );
      end;
    end;

    p := precision (exp);       (* allocate the result register *)
    if p <= bits_per_unit  (* call the appropriate runtime routine *)
      then gen_rt (pushj, sb, rts)
      else gen_rt (pushj, sb, succ (rts));

    for i := 1 to upperbound (operand) do begin       (* free the operands, before alloc of result reg *)
      free (mem[i]);
    end;

    reg := get_reg (p);

    for i := 1 to upperbound (operand) do
      gen_rm (arg, reg, mem[i]);

    rt_call := reg;
  end (* with *) ;
 end;
$PAGE coerce_to_double
(* COERCE_TO_DOUBLE takes an address descriptor for a value of
   size <= bits_per_unit bits.  It generates code (if necessary) to load the
   value into the first register of a register pair.  It is used
   for single to double real conversions and for DIV and MOD
   operations.  *)

public function coerce_to_double ( addr: addr_desc; align: data_alignment ): registers;

var
  reg: registers;
  result_reg: registers;

begin
  if is_register ( addr ) then begin            (* address is a register *)
    reg := addr.offset;
    if regdesc[reg + 1].uses_remaining = 0 then begin           (* next reg is free, allocate it *)
      regdesc[reg + 1].uses_remaining := 1;
      tag_reg ( reg, reg + 1 );
      result_reg := reg;
    end
    else begin                                  (* next reg not free, allocate a reg pair and *)
      ( addr );                            (* move value to first reg of pair *)
      result_reg := get_reg ( 2 * bits_per_unit );
      gen_rr ( move, result_reg, reg );
    end;
  end
  else begin                                    (* value not in a reg *)
    free ( addr );                              (* allocate a reg pair and move value to *)
    result_reg := get_reg ( 2 * bits_per_unit );        (* first reg *)
    do_move ( result_reg, addr, align, bits_per_unit );
  end;

  coerce_to_double := result_reg;

end  (* proc coerce_to_double *);
$PAGE do_float_op
(* DO FLOAT OP compiles a "float_op" given the operator node ("exp").  *)

function do_float_op ( exp: expr ): addr_desc;

  var
    reg: registers;
    prec_op1, prec_exp: bit_range;

begin
  prec_exp := precision (exp);
  
  if exp^.operand[1]^.desc.kind in [bools, ints, chars, scalars] then begin     (* integer -> real *)
    if prec_exp > bits_per_unit then
      do_float_op := reg_addr (rt_call ( exp, pred(rt_d_float) ))
    else
      do_float_op := reg_addr (non_immed_move ( fltr, exp, bits_per_unit ))
  end

  else begin                            (* real to real precision conversion *)
    prec_op1 := precision (exp^.operand[1]);    (* get bit sizes *)

    if prec_op1 < prec_exp then begin           (* single --> double conversion *)
      reg := coerce_to_double ( fetch (exp^.operand[1]), alignment (exp^.operand[1]) );
      gen_ri ( setz, reg + 1, 0 );
      do_float_op := reg_addr (reg)
    end
    else if prec_op1 > prec_exp then    (* double --> single conversion *)
      do_float_op := reg_addr (rt_call (exp, rt_d_to_s))
    else
      do_float_op := fetch (exp^.operand[1])
  end
end;
$PAGE round_or_trunc
(* ROUND_OR_TRUNC generates the code for ROUND_OPs and TRUNC_OPs. *)

function round_or_trunc ( exp: expr ): registers;

var
  reg: registers;

begin 
  with exp^ do begin
    if opcode = trunc_op then begin             (* real to int truncation *)
      if precision ( operand[1] ) = 2 * bits_per_unit
        then reg := rt_call ( exp, rt_d_trunc )
        else reg := non_immed_move ( fix, exp, bits_per_unit );
    end
    else begin                                  (* round operation *)
      if upperbound (operand) = 1 then begin                  (* round real to integer *)
        if precision ( operand[1] ) = 2 * bits_per_unit
          then reg := rt_call ( exp, rt_d_round )
          else reg := non_immed_move ( fixr, exp, bits_per_unit );
      end
      else begin                                (* real to real round *)
        reg := rt_call ( exp, rt_r_rnd2 );
      end;
    end  (* round case *) ;
  end  (* with *) ;

  round_or_trunc := reg;

end  (* proc round_or_trunc *) ;
$PAGE do_min_max_op
(* DO MIN MAX OP compiles code for integer or real, min or max operators, given
   the operator node ("exp") for the operator, the comparision subopcode ("cmp"),
   and the precision ("p") of the operands. *)

function do_min_max_op (exp: expr; cmp: cmp_codes; p: bit_range): registers;

 var
   reg: registers;
   mem: addr_desc;
   i: index_range;

 begin
  with exp^ do begin

    (* Load the first operand into a destroyable register.  As we compare this value
       against each successive operand, if the value of the operand is found to be
       less (greater) than the first, the register is loaded with the value of the
       operand compared against. *)

    mem := fetch (operand[1]);
    reg := load_addr (mem, alignment(operand[1]), p);

    (* Process each operand and compare with the min/max value to date. *)

    for i := 2 to upperbound (operand) do begin
      if p <= bits_per_unit
        then mem := fetch_fullword ( operand[ i ] )
        else mem := argument ( operand[ i ] );  (* must be direct addr, so offset can *)
                                                (* be incremented below *)


      if p <= bits_per_unit                                (* generate a compare and skip *)
        then gen_rm (cam+cmp, reg, mem)

      else begin                                (* double precision compare required *)
        if cmp = ltc
          then gen_rm (cam+gec, reg, mem)
          else gen_rm (cam+lec, reg, mem);
        gen (jrst, 0, 0, 4, dot);
        gen_rm (cam+nec, reg, mem);
        mem.offset := mem.offset + 1;
        gen_rm (cam+cmp, reg+1, mem);
        mem.offset := mem.offset - 1;
      end;

      do_move (reg, mem, signed_value, p);
      free (mem);
    end (* for *) ;

    do_min_max_op := reg;
  end (* with *) ;
 end;
$PAGE iconstp
(* ICONSTP is a predicate which indicates if its argument has a constant value.
   It is assumed that the argument has an ordinal type.  If the argument does
   have a constant value, its value is returned. *)

public function iconstp (exp: expr; var val: int_type): boolean;
  
var
  texp: expr;

begin
  texp := exp;
  while texp^.opcode = sclcvt_op do     (* skip conversion operators *)
    texp := texp^.operand[1];
  if (texp^.opcode = cst_ref) andif (texp^.cst_val.kind = scalar_cst) then begin
    iconstp := true;
    val := texp^.cst_val.ival;
  end
  else iconstp := false;
end;
$PAGE aconstp
(* ACONSTP serves the same function as ICONSTP, except that it accepts
   as a parameter an ADDR DESC insead of an EXPR.       *)

public function aconstp (maddr: addr_desc; var val: int_type): boolean;

begin
  if (maddr.index = 0) andif (maddr.reloc.kind = absolute_sc) then begin
    aconstp := true;
    val := maddr.offset;
  end
  else aconstp := false;
end;
$PAGE duplicate_addr
(* DUPLICATE_ADDR increases the usage count by one on any register used
   by the parameter ADDR.  It then returns ADDR as its function value.  *)
  
public function duplicate_addr ( addr: addr_desc ): addr_desc;
  
begin
  incr_reg_usages ( regs_used (addr), 1 );
  duplicate_addr := addr;
end  (* proc duplicate_addr *) ;
$PAGE get_bounds
(* GET BOUNDS returns an approximation of the bounds of a scalar value. *)

public procedure get_bounds (exp: expr; var minval, maxval: int_type);
  
var base: typ;
  
begin
  if iconstp (exp, minval)
    then maxval := minval
  else begin
    base := exp^.desc.base;             (* base type of scalar *)

    if not (base^.kind in [scalars, bools, ints, chars]) then begin
      minval := 0;                      (* must be coerced pointer *)
      maxval := 777777B;
    end

    else begin
      minval := base^.minval;
      maxval := base^.maxval;
    end;
  end;
end;
$PAGE int_value
(* INT VALUE returns an addr_desc for any integer value.  If possible, the
   value is made immediate, otherwise a constant word is omitted, and its
   address returned. *)

public function int_value ( val: int_type ): addr_desc;
 
begin
  int_value := absolute_reference;      (* init most fields to default *)
  if (val >= 0) and (val <= 777777B)
    then begin
      int_value.immediate := true;
      int_value.offset := halfword (val);
    end
  else int_value.reloc := gen_cint (val);
end;
$PAGE expr_size
(* EXPR SIZE calculates the size in bits of a value given its expression type
   descriptor.   *)
  
public function expr_size ( expr_desc: expr_type_desc ): bit_range;

begin
  with expr_desc do begin

    if kind in [bools,ints,chars,scalars] then
      expr_size := int_prec
    else if kind in [pointers,files] then
      expr_size := 18
    else if kind in [procs,funcs] then
      expr_size := bits_per_unit
    else if kind = reals then begin (* expr_type_desc prec. takes precedence
				       over base type node's prec. *)
      if precision <= srealprec then
	expr_size := bits_per_unit
      else
	expr_size := 2 * bits_per_unit
    end
    else if ( base <> nil ) andif ( kind <> sets ) then
      expr_size := base^.base_size
    else if kind = sets then
      expr_size := set_length
    else if kind = strings then begin
      expr_size := ( ( str_length + chars_per_unit - 1 ) div chars_per_unit )
                     * bits_per_unit;
      if str_kind = varying then
        expr_size := expr_size + bits_per_unit
    end
    else
      assert ( false )                         (* what the hell is it? *)
  
  end
end  (* proc expr_size *) ;
$PAGE load_addr
(* LOAD ADDR loads a value described by an address descriptor into a register
   (if it is not already in a register).  The size of the value and alignment
   information are also specified.  The number of the register (or first register
   in a pair) is returned.  Note that the alignment information may cause code
   to be generated to sign extend or left justify the value.  *)
  
function load_addr (* ( addr: addr_desc; align: data_alignment; size: bit_range ): registers  *) ;

begin
  assert ( size <= 2 * bits_per_unit );
  if is_register ( addr ) then begin
    load_addr := addr.offset
  end 
  else begin
    free ( addr );
    load_addr := get_reg ( size );
    do_move ( load_addr, addr, align, size );
  end;
end  (* proc load_addr *) ;
$PAGE load
(* LOAD loads a value described by an expression class tuple into a register.  The
   size of the value to be loaded is also specified as a parameter.  LOAD evaluates
   the expression by calling FETCH and then loads the result into a register (if
   it is not already loaded).  Note that the value may be sign extended or left
   justified when loaded.  *)
  
function load (* ( exp: expr; size: bit_range ): registers *) ;

var
  addr: addr_desc;

begin
  addr := fetch ( exp );
  
  if is_register ( addr ) then begin (* already in a register? *)
    load := addr.offset;        (* no additional work required *)
    if regdesc [load].uses_remaining > 1 then begin (* will there be later uses of this value? *)
      load := get_reg (bits_per_unit);
      gen_rm (move, load, addr); (* make copy *)
      free (addr)
    end
  end
  else
    load := load_addr (addr, alignment (exp), size)
end;
$PAGE nond_load
(* NONDestructive LOAD is similar to LOAD, but the caller stipulates that the
   value in the returned register will not be destroyed.  If FETCH has saved
   an addr_descriptor on the heap, it will be "updated" to reflect the availability
   of the value in the returned register.  Later FETCHs will then be able to
   access the value from that register.  *)
  
public function nond_load ( exp: expr; size: bit_range ): registers  options special(coercions);

var
  addr_ref: addr_ptr;
  addr: addr_desc;
  i: usage_range;
  reg: registers;

begin
  (* suppose that the usage count on the expr tuple is N ...  *)
  addr := fetch ( exp );
  (* then now the usage count is N-1  -  ADDR constitutes the other usage *)
  
  if is_register ( addr ) then
    nond_load := addr.offset    (* no additional work required *)
  
  else if exp^.usage_count = 0 then  (* this is the only usage? *)
    nond_load := load_addr (addr, alignment (exp), size)
  
  else begin    (* fetch saved copy of addr for later uses *)
    (* we are going to put the value addressed by ADDR into a register.  The
       uses_remaining for that new register will be set to N: LOAD will set the
       uses to one, and then we'll explicitly increment by N-1 below.
  
       If ADDR uses registers, then their uses must be decremented by N to account
       for the N uses being transfered to the new register.  LOAD will FREE the
       ADDR, accounting for one of the uses, and we will explicitly decrement
       by N-1.  Note that all of the decrementing is done by the time LOAD calls
       GET_REG so the same register ADDR was using can be reused, unless it had
       additional uses originating from somewhere else.  *)
    reg := regs_used (addr);
    for i := 1 to exp^.usage_count do
      decr_reg_usages (reg);
    nond_load := load_addr (addr, alignment (exp), size);
    dispose (exp^.result);  (* can't just modify it due to type problems *)
    new (addr_ref);
    addr_ref^ := reg_addr_desc;
    addr_ref^.offset := nond_load;
    exp^.result := ptr (ord (addr_ref) );
    incr_reg_usages (nond_load, exp^.usage_count)
  end
end;
$PAGE copy_load
(* COPY LOAD loads an expression tuple value into a register.
   If the value is already in a register, it will be copied to a new reg
   if the register has more than one remaining use or if it is a
   WITH register. *)

public function copy_load ( exp: expr ): registers;

var
  addr: addr_desc;

begin
  addr := fetch ( exp );
  if not is_register (addr) orif (addr.offset >= with_base) orif
    (regdesc[addr.offset].uses_remaining > 1) then begin
      free (addr);
      copy_load := get_reg (bits_per_unit);
      gen_rm (move, copy_load, addr);
    end
  else copy_load := addr.offset;
end;
$PAGE load_reg
public procedure load_reg (reg: registers; exp: expr);

var wsize: bit_range;
    maddr: addr_desc;

begin
  maddr := fetch (exp);
  free (maddr);
  wsize := expr_size (exp^.desc);
  if (wsize > bits_per_unit) andif (wsize <= 2 * bits_per_unit) then begin
    regdesc[reg+1].uses_remaining := 1;
    tag_reg (reg, reg+1);
  end
  else regdesc[reg].associate := noreg;
  regdesc[reg].uses_remaining := 1;
  if not is_register (maddr) orif (maddr.offset <> reg) then (* could accidentally be in right reg *)
    do_move (reg, maddr, alignment (exp), wsize);
end;
$PAGE increment_addr
(* INCREMENT ADDR is passed an address descriptor, word and bit offsets from the
   address, and a field width.  It returns an address descriptor for a field of the
   specified width at the specified offset from the base address.  Note that
   INCREMENT ADDR transforms the address passed in into the return value address,
   any registers used in BASE ADDR may be freed and BASE ADDR must be regarded as
   invalid after INCREMENT ADDR returns.  *)
  
public function increment_addr ( base_addr: addr_desc;
                                 word_offset: unit_range;
                                 bits_offset: bit_offset;
                                 component_width: bit_range ): addr_desc;

var
  reg: registers;

begin
  assert (not ( (base_addr.mode = slice) andif (base_addr.slice_offset <> 0) ));
  assert (not ( (base_addr.mode in [lhw,rhw]) or base_addr.immediate ));
  assert (not (is_register (base_addr) andif (word_offset <> 1) andif 
                (regdesc [base_addr.offset].associate <> base_addr.offset + 1) ));
  
  increment_addr := base_addr;
  with increment_addr do begin

    (* Update the address.  Three cases are distinguished: (1) an address
       of the form @reg is asserted not to happen; (2) all other indirect addresses will
       cause the address to be loaded into a register and an address of the form 
       WORD_OFFSET(REG) is generated, (3) all non-indirect addresses simply have
       WORD_OFFSET added to the offset field of the instruction.  Note that
       not all address modes are considered legal inputs to this routine.  *)

    if word_offset <> 0 then begin
      assert ( not ((reloc.kind = register_sc) and indirect) );  (* Case 1: '@reg' *)
      if indirect then begin            (* Case 2: other indirect addressses *)
        free ( increment_addr );
        reg := get_reg ( bits_per_unit );
        gen_rm ( movei, reg, increment_addr );
        increment_addr := absolute_reference;
        index := reg;
        offset := word_offset;
      end
      else                                      (* Case 3: non-indirect address *)
        offset := offset + word_offset

    end;

    (* set mode of address descriptor. *)

    if (component_width >= bits_per_unit) or (component_width = 0)
      then mode := fw
    else if (component_width = 18) andif (bits_offset = 0)
      then mode := lhw
    else if (component_width = 18) andif (bits_offset = 18)
      then mode := rhw
    else mode := slice;
    slice_size := component_width;              (* always set slice fields because packed *)
    slice_offset := bits_offset;                (* array accesses always use byte ptrs *)

  end
end  (* proc increment_addr *) ;
$PAGE const_addr
(* CONST ADDR returns an address descriptor for the value described by a CST REF
   expression tuple.  The constant NULL LOCATION is returned for subroutine constants.  *)
 
function const_addr ( exp: expr ): addr_desc;

var
  scalar_value: integer;

begin
  with exp^.cst_val do
    
    if kind in [scalar_cst,ptr_cst] then begin  (* pointer and scalar constants *)
      if kind = ptr_cst
        then scalar_value := int_nil
        else scalar_value := ival;
      const_addr := int_value ( scalar_value );
    end

    else begin                                  (* all other constants *)
      const_addr := null_location;
      if kind <> subr_cst then begin
        const_addr.reloc := gen_cnode ( valp );
        (* if user expects a nonvarying constant, but gen_cnode generated the constant with
           a length word, skip over it.  *)
        if (kind = string_cst) andif
                ((exp^.desc.str_kind = nonvarying) and valp^.str_varying_ref) then
          const_addr := increment_addr (const_addr, 1, 0, bits_per_unit)
      end
      else begin                                (* subroutine constant *)
        with const_addr.reloc do begin
          kind := def_sc;
          reldef := get_def ( subr_def, blkp^.number );
        end;
      end;
    end;

end  (* proc const_addr *) ;
$PAGE pass_by_byte_ptr
(* PASS_BY_BYTE_PTR is passed a symbol node and returns a boolean
   value.  The return value is TRUE if the symbol is that of a
   formal parameter which is passed via a byte pointer.  Flexible,
   non-varying string value parameters have a byte pointer passed
   rather than an address.  *)

function pass_by_byte_ptr ( id_sym: sym ): boolean;

begin
  with id_sym^.type_desc^ do

    pass_by_byte_ptr := (id_sym^.kind = values) andif
                        ( flexible ) andif
                        ( kind = strings ) andif
                        ( str_kind = nonvarying );
end;
$PAGE param_addr
(* PARAM_ADDR is passed a symbol node of kind VARS or VALUES and
   whose DCL_CLASS field has the value PARAMETER_SC.  An address
   descriptor record addressing the parameter is returned.
   Note that:
        1. if the symbol is a formal parameter which is a flexible or
           generic array, then the address is that of the start of the 
           array, not the address of the bounds words.
        2. if the symbol is of a flexible, nonvarying string value
           parameter then the address is of a byte pointer which
           points 1 character before the start of the string.  *)

function param_addr ( id_sym: sym ): addr_desc;

var
  id_block: blk;
  reg: registers;
  level_difference: level_index;
  i: level_index;
  new_reg: registers;

begin
  id_block := id_sym^.block;

  reg := sp;
  if id_block <> cur_block then begin           (* Non-local id, must chase static chain *)
    level_difference := cur_block^.apparent_level - id_block^.apparent_level;
    if level_difference <> 0 then begin
      reg := get_reg ( bits_per_unit );
      gen ( hlrz, reg, sp, 1, none );
      for i := 2 to level_difference do
        gen ( hlrz, reg, reg, 1, none );
    end;
  end;

  (* If the parameters require more than 6 words, then a pointer to
     a block containing the parameters is passed.  *)

  if (id_sym^.block^.subr_sym^.type_desc^.parmlist_size > 6) and
     (id_sym <> id_sym^.block^.return_sym) then begin   (* not fcn return value *)
    if reg = sp
      then new_reg := get_reg ( bits_per_unit )
      else new_reg := reg;
    gen ( move, new_reg, reg, id_sym^.block^.parm_list_base, none );
    reg := new_reg;
  end;

  param_addr := parm_reference;
  param_addr.reloc.relsym := id_sym;
  param_addr.index := reg;

  if pass_by_byte_ptr ( id_sym ) 
    then param_addr.mode := byte
  else if passed_by_address ( id_sym )
    then param_addr.indirect := true;

end  (* proc param_addr *);
$PAGE local_addr
(* LOCAL_ADDR is passed a symbol node of kind CONSTS, VARS or VALUES
   and whose DCL_CLASS field has the value LOCAL_SC.  An address
   descriptor record addressing the local variable is returned. *)

function local_addr ( id_sym: sym ): addr_desc;

var
  id_block: blk;
  reg: registers;
  level_difference: level_index;
  i: level_index;

begin
  id_block := id_sym^.block;

  reg := sp;
  if id_block <> cur_block then begin           (* Non-local id, must chase static chain *)
    level_difference := cur_block^.apparent_level - id_block^.apparent_level;
    if level_difference <> 0 then begin
      reg := get_reg ( bits_per_unit );
      gen ( hlrz, reg, sp, 1, none );
      for i := 2 to level_difference do
        gen ( hlrz, reg, reg, 1, none );
    end;
  end;

  local_addr := null_location;                  (* construct addr record to return *)
  with local_addr do begin
    index := reg;
    reloc.kind := local_sc;
    reloc.relsym := id_sym;
  end;

end  (* proc local_addr *) ;
$PAGE ident_addr
(* IDENT ADDR generates an address descriptor record for an IDENT REF tuple.  Note
   that code may be generated to make the identifier addressible, e.g. non-local
   variables.  Also note that the mode of the address descriptor returned will always
   be 'FW'.  *)
  
function ident_addr ( exp: expr ): addr_desc;

begin
  with exp^ do begin
    assert (id_sym^.kind in [consts, vars, values, conditions]); (* must hold to access dcl_class *)
    if id_sym^.kind = conditions then
      assert (id_sym^.dcl_class in [static_sc, external_sc]);
    
    case id_sym^.dcl_class of                   

      constant_sc: begin
	ident_addr := null_location;
	ident_addr.reloc := gen_cval (id_sym^.init_value)
      end;

      local_sc:
	ident_addr := local_addr ( id_sym );

      parameter_sc:
	ident_addr := param_addr ( id_sym );

      static_sc: begin
	ident_addr := null_location;
	with ident_addr do begin
	  reloc.kind := static_sc;
	  reloc.relsym := id_sym
	end
      end;

      external_sc: begin
	ident_addr := null_location;
	with ident_addr do begin
	  reloc.kind := external_sc;
	  reloc.relsym := id_sym;
	  if (prog_options.overlay_opt and (id_sym^.kind <> conditions)) andif 
		((id_sym^.kind <> consts) orif
		    not (id_sym^.type_desc^.kind in [procs, funcs])) then
	      indirect := true;
	end
      end;

      others:
	assert ( false )                        (* Cannot happen !?! *)

    end;
  end
end  (* proc ident_addr *);
$PAGE field_addr
(* FIELD ADDR takes a FIELD REF tuple and returns an address descriptor for the
   referenced field.  The operand of the FIELD REF tuple is evaluated in the process.  *)
  
function field_addr ( field_ref_exp: expr ): addr_desc;

var
  base_addr: addr_desc;
  words_offset: unit_range;
  bits_offset: bit_offset;

begin
  with field_ref_exp^ do begin

    base_addr := fetch ( base_rec ); (* get base address of record *)

    words_offset := field_sym^.fld_offset div bits_per_unit; (* calculate field's address *)
    bits_offset := field_sym^.fld_offset mod bits_per_unit;  (*     "        "       "    *)
    
    field_addr := increment_addr ( base_addr,
                                   words_offset, bits_offset,
                                   field_sym^.fld_width );
    field_addr.has_sign := alignment (field_ref_exp) = signed_value
  end
end;
$PAGE deref_addr
(* DEREF ADDR generates an address descriptor record for a PTR_REF tuple.  The
   operand of the PTR REF tuple is evaluated in the process. *)
  
function deref_addr ( deref_expr: expr ): addr_desc;

begin

  deref_addr := argument ( deref_expr^.base_ptr );
  with deref_addr do begin
    indirect := true;
   
    (* convert form @REG to form 0(REG) *)
  
    if (reloc.kind = register_sc) and not immediate then begin
      index := offset;
      offset := 0;
      indirect := false;
      reloc.kind := absolute_sc
    end;
  
    (* immediate and indirect cancel each other - should only arise if a constant
       is coerced into a pointer and dereferenced *)
  
    if immediate and indirect then begin
      immediate := false;
      indirect := false;
      assert (reloc.kind = absolute_sc)  (* verify that this only happens when I think it does *)
    end
  end

end  (* proc deref_addr *) ;
$PAGE file_deref
(* FILE DEREF generates code for a file variable dereference *)

function file_deref (exp: expr): addr_desc;

var
  reg: registers;

begin
  reg := load (exp^.base_file, bits_per_unit);
  file_deref := absolute_reference;
  with file_deref do begin
    indirect := true;
    index := reg
  end
end;
$PAGE is_generic
(* IS_GENERIC takes an expression tuple whose expression type
   descriptor record has kind array or string.  It returns 
   true if the kind is array and the array is generic. *)

function is_generic ( exp: expr ): boolean;

begin
  with exp^.desc do
    is_generic := ( kind = arrays ) andif ( base^.generic )
end;
$PAGE dynamic_flex
(* DYNAMIC_FLEX is passed an expression tuple whose associated
   type is array or string.  It returns TRUE if the expression
   tuple represents a flexible array or string which has been
   dynamically allocated on the heap with an upperbound determined
   at run time.  FALSE is returned for flexible array or string
   formal parameters.  Thus this routine identifies flexible
   arrays or strings which have the upperbound word physically
   preceding the array or string.  *)

public function dynamic_flex ( array_expr: expr ): boolean;

begin
  with array_expr^ do
    dynamic_flex :=
              ( (opcode = ptr_ref) or (opcode = field_ref) ) andif
              ( ( (desc.kind = strings) andif (desc.str_flex) ) or
                ( (desc.kind = arrays ) andif (desc.base^.flexible) ) )
end  (* proc dynamic_flex *) ;
$PAGE get_lbound
(* GET_LBOUND is passed an expression tuple whose associated type
   is string or *** non-generic *** array.  It returns the constant
   lower bound of the string or array.  *)

function get_lbound ( exp: expr ): integer;

begin
  with exp^.desc do begin
    if kind = strings
      then get_lbound := 1
      else get_lbound := base^.index_type^.minval;
  end
end  (* proc get_lbound *);
$PAGE lower_bound
(* LOWER_BOUND is passed both the expression tuple for an array or
   string **and** an address descriptor record for the base of the
   array or string.  It returns an address descriptor for the
   lower bound of the array or string.  Unless the array or string
   is generic, the address descriptor will simply be an immediate
   reference or a reference to a constant in memory.  For a generic
   array, the address will be that of the lower bound word.  *)

public function lower_bound ( array_expr: expr; array_addr: addr_desc ): addr_desc;

begin

  (* Non_generic array - return immediate address or address of
     constant in constant area.  *)

  if not is_generic ( array_expr ) then
    lower_bound := int_value ( get_lbound (array_expr) )

  (* Generic array.  Array_expr must be an ident_ref tuple for a
     formal parameter - so we can make the following strong
     assumptions about its address.  The address of the array 
     must be of the form '@offset(idx)'.  The address of the
     lower bound is then 'offset-2(idx)'.  *)

  else begin
    assert ( array_expr^.opcode = ident_ref );
    lower_bound := duplicate_addr( array_addr );
    lower_bound.indirect := false;
    lower_bound.offset := lower_bound.offset - 2
  end
end;
$PAGE upper_bound
(* UPPER_BOUND is passed both the expression tuple for an array
   or string and an address descriptor for the base of the array
   or string (actually for the descriptor word for dynamic flexible
   array or strings).  Unless the array or string is flexible, the
   address descriptor will simply be an immediate reference or a
   reference to a constant in memory.  For a flexible array the
   address will be that of the descriptor word containing the
   upper bound.  *)

public function upper_bound ( array_expr: expr; array_addr: addr_desc ): addr_desc;

begin
  with array_expr^ do
    
    (* If array/string is not flexible then create an address
       descriptor for the constant upper bound.  *)

    if ( (desc.kind = strings) andif (not desc.str_flex) ) orif
       ( not desc.base^.flexible ) then
      if desc.kind = strings then
	upper_bound := int_value (desc.str_length)
      else
        upper_bound := int_value (desc.base^.index_type^.maxval)

    else begin
      
      (* If opcode is ident_ref, then we have a formal parameter. 
         The upper bound is in the parameter word preceeding the 
         address of the array.  The address of the first element of the
         array sould be of the form: '@offset(reg)'  (since the array
         is a parameter).  *)

      if opcode = ident_ref then begin
        upper_bound := duplicate_addr( array_addr );
        with upper_bound do begin
          assert (indirect  orif (mode = byte));
          mode := fw;
          indirect := false;
          offset := offset - 1;
        end;
      end

      (* PTR_REF or FIELD_REF tuple.  Flex object is on the heap - the
         upperbound is in the word preceding the first word of the 
         array - which is just the address passed in.  *)

      else if (opcode = ptr_ref) or (opcode = field_ref) then begin
        upper_bound := duplicate_addr( array_addr );
        upper_bound.mode := fw;
      end

      else
        assert ( false )                        (* can't happen - right ? *)

    end;
end  (* proc upper_bound *) ;
$PAGE do_dim_op
(* DO DIM OP generates code for a DIM_OP typle and returns an address
   descriptor for the result.  DIM_OP tuples are generated only when the
   dimension is not known at compile time - when a flex array or string is
   involved.  They are generated both for calls to intrinsic function DIMENSION
   and for aggregate assignment compatability checks involving flex objects.  *)

function do_dim_op ( exp: expr ): addr_desc;

var
  array_expr: expr;
  array_addr: addr_desc;
  lower_addr: addr_desc;
  reg: registers;
  cons_part: integer;

begin

  array_expr := exp^.operand[1];
  array_addr := fetch ( array_expr );
  do_dim_op := upper_bound ( array_expr, array_addr );
  free (array_addr);

  if is_generic ( array_expr ) then begin (* generic array - neither bound known *)
    reg := load_addr (do_dim_op, unsigned_value, bits_per_unit);
    lower_addr := lower_bound ( array_expr, array_addr );
    gen_rm ( sub, reg, lower_addr );
    free (lower_addr);
    gen_ri ( addi, reg, 1 );
    do_dim_op := reg_addr (reg)
  end

  else begin (* non_generic array - lower bound known *)
    cons_part := get_lbound ( array_expr ) - 1;
    if cons_part <> 0 then begin
      reg := load_addr (do_dim_op, unsigned_value, bits_per_unit);
      if cons_part > 0
        then gen_ri ( subi, reg, cons_part )
      else if cons_part < 0
        then gen_ri ( addi, reg, -cons_part );
      do_dim_op := reg_addr (reg)
    end
  end
end;
$PAGE address_op
(* ADDRESS OP generates code for an ADDR_OP expression tuple. An address
descriptor for the address formed is returned. *)

function address_op (exp: expr): addr_desc;

const
  flex_arr_desc_size = 36;
  flex_str_desc_size = 36;

var
  string_desc: str_desc;
  addr: addr_desc;

begin
  with exp^ do begin

    (* Fetch the address of the operand and increment the address past
       any flex string or array descriptor words. *)

    if operand[1]^.desc.kind = strings then begin
      string_desc := fetchstring (operand[1], no_length);
      addr := duplicate_addr (string_desc.base_addr);
      if string_desc.base_is_bound then (* if flex, skip bounds word *)
        addr := increment_addr (addr, flex_str_desc_size, 0, bits_per_unit);
      free_string (string_desc);
    end
    else begin
      addr := fetch (operand[1]);
      if dynamic_flex (operand[1]) then
        addr := increment_addr (addr, flex_arr_desc_size, 0, bits_per_unit);
    end;

    (* Move the address of the operand to a register and return
       the address of the register *)

    assert (not is_register (addr));
    assert (not addr.immediate);
    free (addr);
    address_op := reg_addr (get_reg (bits_per_unit));
    gen_rm (movei, address_op.offset, addr);
  end;
end;
$PAGE case_op
(* CASE OP generates code for an upper or lower case conversion of a
   character. *)

function case_op (addr: addr_desc;      (* addr of character *)
                  case_code: str_translation) (* conversion to be performed *)
                    : addr_desc;        (* addr of result *)

var
  reg: registers;

begin
  reg := load_addr (addr, right_aligned, bits_per_unit);
  if case_code = lower_trans then begin
    gen_ri (cai+ltc, reg, ord ('A'));
    gen_ri (cai+lec, reg, ord ('Z'));
    gen_ri (cai+awc, reg, 0);
    gen_ri (addi, reg, ord ('a') - ord ('A'));
  end
  else begin
    gen_ri (cai+ltc, reg, ord ('a'));
    gen_ri (cai+lec, reg, ord ('z'));
    gen_ri (cai+awc, reg, 0);
    gen_ri (subi, reg, ord ('a') - ord ('A'));
  end;
  case_op := reg_addr (reg);
end;
$PAGE unpacked_array_addr
(* UNPACKED_ARRAY_ADDR is passed an ARRAY_REF tuple corresponding
   to an array whose element size is a single word or greater.
   An address descriptor for the referenced element is returned.  *)

function unpacked_array_addr ( array_ref_exp: expr ): addr_desc;

var
  array_typ: typ;
  index_is_constant: boolean;
  const_part: integer;
  base_addr, index_addr: addr_desc;
  elem_size: bit_range;
  reg: registers;
  words_per_elem: unit_range;
  lower_bound: integer;

begin
  with array_ref_exp^ do begin

    (* A few preliminaries first - set up some info we'll need later.  *)

    array_typ := base_array^.desc.base;
    elem_size := array_typ^.element_size;
    index_addr := fetch (index_val);
    index_is_constant := iconstp (index_val, const_part);
    if index_is_constant then
      free (index_addr);
    base_addr := fetch ( base_array );
    words_per_elem := (elem_size + bits_per_unit - 1) div bits_per_unit;
    
    (* Generic arrays require special handling because the lowerbound
       is not known at compile time.  Note that a generic array reference
       must be a simple identifier which is a formal parameter.  *)

    if array_typ^.generic then begin

      (* generic array with a constant index.  The constant index is
         folded in as the offset portion of the final address.  *)

      if index_is_constant then begin
        reg := get_reg ( bits_per_unit );
        gen ( movn, reg, base_addr.index, base_addr.offset - 2,
              base_addr.reloc );
        if words_per_elem <> 1 
          then gen_ri ( imuli, reg, words_per_elem );
        gen ( add, reg, base_addr.index, base_addr.offset, base_addr.reloc );
        unpacked_array_addr := absolute_reference;
        with unpacked_array_addr do begin
          index := reg;
          offset := const_part * words_per_elem;
        end;
        free (base_addr);
      end

      (* Generic array with variable index.  *)

      else begin
        reg := load_addr ( index_addr, alignment (index_val), bits_per_unit );
        gen ( sub, reg, base_addr.index, base_addr.offset - 2,
              base_addr.reloc );
        if words_per_elem <> 1
          then gen_ri ( imuli, reg, words_per_elem );
        gen ( add, reg, base_addr.index, base_addr.offset, base_addr.reloc );
        unpacked_array_addr := absolute_reference;
        unpacked_array_addr.index := reg;
      end;
      free (base_addr);
    end

    (* Non_generic array - lower bound is known at compile time.  *)

    else begin
      lower_bound := array_typ^.index_type^.minval;
    
      (* Non_generic array with constant index.  *)

      if index_is_constant then begin
        const_part := (const_part - lower_bound) * words_per_elem;
        if dynamic_flex ( base_array )          (* compensate for upperbound word, *)
          then const_part := const_part + 1;    (* if present *)
        unpacked_array_addr := increment_addr ( base_addr, const_part,
            0, elem_size );
      end

      (* Non-generic array with variable index.  *)

      else begin
         reg := load_addr ( index_addr, alignment (index_val), bits_per_unit );
         if words_per_elem <> 1 
           then gen_ri ( imuli, reg, words_per_elem );
        if ( base_addr.index = 0 ) andif 
           ( not (base_addr.indirect or base_addr.immediate) ) andif
           ( base_addr.reloc.kind <> register_sc ) then begin
          unpacked_array_addr := base_addr;
        end
        else begin
	  if base_addr.indirect then begin
	    base_addr.indirect := false;
	    gen_rm (add, reg, base_addr)
	  end
	  else
	    gen_rm ( addi, reg, base_addr );
          unpacked_array_addr := absolute_reference;
        end;
        with unpacked_array_addr do begin
          index := reg;
          offset := offset - lower_bound * words_per_elem;
          if dynamic_flex ( base_array )        (* compensate for upperbound word, *)
            then offset := offset + 1;          (* if present *)
          free (base_addr);
        end;
      end;
    end  (* non_generic else clause *) ;
  end  (* with *) ;
end  (* proc unpacked_array_addr *) ;
$PAGE packed_array_addr
(* PACKED_ARRAY_ADDR is passed an ARRAY_REF tuple corresponding
   to an array whose element size is less than a single word.
   An address descriptor for the referenced element is returned.  *)

function packed_array_addr ( array_ref_exp: expr ): addr_desc;

var
  array_typ: typ;
  index_is_constant: boolean;
  const_part: integer;
  base_addr, index_addr: addr_desc;
  elem_size: bit_range;
  reg: registers;
  elems_per_word: elem_sizes;
  byte_ptr_reloc: rel_syllable;
  word_offset: unit_range;
  bits_offset: bit_offset;
  lower_bound: integer;
  index_value: integer;
  generic: boolean;
  lwr_bnd_addr: addr_desc;
  base_reg: registers;

begin
  with array_ref_exp^ do begin

    (* First some preliminaries - fetch some info we'll need later. *)

    array_typ := base_array^.desc.base;
    elem_size := array_typ^.element_size;
    index_addr := fetch (index_val);
    index_is_constant := iconstp (index_val, index_value);
    if not index_is_constant then index_value := 0;
    elems_per_word := bits_per_unit div elem_size;
    generic := array_typ^.generic;
    if generic
      then lower_bound := 0
      else lower_bound := array_typ^.index_type^.minval;
    

    (* if the index is not constant then load it into a register. *)

    if not index_is_constant
      then reg := load_addr ( index_addr, alignment (index_val), bits_per_unit )
      else free (index_addr);

    (* If the array is generic, then either generate an instruction
       to subtract the lower bound from the index ( if the index is
       not constant) or load the lowerbound (if the index was constant).  *)

    base_addr := fetch ( base_array );
    if generic then begin
      if index_is_constant then begin
        reg := get_reg ( bits_per_unit );
        gen ( movn, reg, base_addr.index, base_addr.offset - 2, base_addr.reloc );
      end
      else begin
        gen ( sub, reg, base_addr.index, base_addr.offset - 2, base_addr.reloc );
      end;
    end;

    (* Adjust the base address by any constant components of the index.  *)

    const_part := index_value - lower_bound;
    if dynamic_flex ( base_array )              (* compensate for upperbound word, *)
      then const_part := const_part + elems_per_word;   (* if present *)
    if const_part <> 0 then begin
      if const_part > 0 then begin
        word_offset := const_part div elems_per_word;
        bits_offset := (const_part mod elems_per_word) * elem_size;
      end
      else begin
        word_offset := (const_part - elems_per_word + 1) div elems_per_word;
        bits_offset := ( elems_per_word + ((const_part + 1)
                         mod elems_per_word) - 1) * elem_size;
      end;
    end
    else begin
      word_offset := 0;
      bits_offset := 0;
    end;
  
    base_addr := increment_addr ( base_addr, word_offset, bits_offset,
                                  elem_size );

    (* Calcuate the elements address.  Unless both the index and the lower
       bound are known at compile time, an ADJBP instruction will be
       emitted.  *)


    if not (index_is_constant andif not generic) then begin

      (* If the base address is indirect then we must load the *address*
         into a reg first - the ADJBP instruction does not work with
         indirect addresses.  *)

      if base_addr.indirect then begin
        free ( base_addr );
        base_reg := get_reg ( bits_per_unit );
        gen_rm ( movei, base_reg, base_addr );
        with base_addr do begin
          indirect := false;
          mode := slice;
          offset := 0;
          reloc := none;
          index := base_reg;
          slice_size := elem_size;
        end;
      end;

      base_reg := regs_used ( base_addr );      (* associate any reg used by the byte *)
      if (base_reg <> noreg) then
        if (base_reg < with_base) then
          tag_reg ( reg, base_reg )     (* pointer, with the reg containing *)
                                                (* the byte pointer, so regs referred to by *)
                                                (* byte pointer will be freed *)
        else
          decr_reg_usages (base_reg);  (* with-reg, or stack pointer *)
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
	    gen (adj_bp, reg, 0, 0, gen_bptr ( base_addr ) ) 
	  end
        else gen (adjbp, reg, 0, 0, gen_bptr ( base_addr ) );
      packed_array_addr := reg_addr_desc;
      with packed_array_addr do begin
        mode := byte;
        offset := reg;
        slice_size := elem_size;        (* needed for shift of packed sets *)
      end;
    end
    else begin
      packed_array_addr := base_addr;
    end;

    packed_array_addr.has_sign := alignment (array_ref_exp) = signed_value;

    (* If the element has multiple uses it is the operand of a check
        tuple and must be loaded here to avoid loss of the byte pointer
        and/or multiple loads. *)

    if usage_count > 1 then begin
      free (packed_array_addr);
      reg := get_reg (elem_size);
      do_move (reg, packed_array_addr, alignment (array_ref_exp), elem_size);
      packed_array_addr := reg_addr (reg);
    end;
  end  (* with *) ;
end  (* proc packed_array_addr *) ;
$PAGE array_addr
(* ARRAY ADDR generates an address descriptor for an ARRAY REF or SUBSTR REF tuple.  The operands
   of the ARRAY REF tuple are evaluated in the process, thus generating any code
   necessary to make the array elements addressable.  *)
  
public function array_addr ( array_ref_exp: expr ): addr_desc;

var
  reg, reg2: registers;
  base_desc: str_desc;
  base_addr, temp_addr: addr_desc;
  cst_index_value: char_range;
  base_reg: registers;

begin

  if array_ref_exp^.opcode = substr_ref then with array_ref_exp^ do begin
    base_desc := fetchstring (base_string, actual_length);
    base_addr := skip_len_words (base_desc);
    free_string (base_desc);
    if base_addr.indirect then begin
      reg := load (substr_index, bits_per_unit);
      free (base_addr);
      reg2 := get_reg (bits_per_unit);
      gen_rm (movei, reg2, base_addr);
      gen_ri (hrli, reg2, 440700b);
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
            gen_rr (adj_bp, reg, reg2) 
	  end
        else gen_rr (adjbp, reg, reg2);
      decr_reg_usages (reg2);
      array_addr := reg_addr (reg);
      array_addr.mode := byte;
    end
    else if (base_addr.mode <> byte) andif (substr_index^.result = nil) andif
      iconstp (substr_index, cst_index_value) then 
        array_addr := increment_addr (base_addr, (cst_index_value - 1) div 5,
                ((cst_index_value - 1) mod 5) * 7, 7)
    else if base_addr.mode <> byte then begin
      reg := load (substr_index, bits_per_unit);
      gen_ri (subi, reg, 1);
      base_addr.slice_offset := 0;
      base_addr.slice_size := 7;
      temp_addr := absolute_reference;
      temp_addr.reloc := gen_bptr (base_addr);
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
            gen_rm (adj_bp, reg, temp_addr) 
	  end
        else gen_rm (adjbp, reg, temp_addr);
      array_addr := reg_addr (reg);
      array_addr.mode := byte;
      base_reg := regs_used (base_addr);
      if base_reg <> noreg then
        if base_reg < with_base then
          tag_reg (reg, base_reg)
        else
          decr_reg_usages (base_reg)
    end
    else begin
      reg := load (substr_index, bits_per_unit);
      free (base_addr);
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
	    gen_rm (adj_bp, reg, base_addr) 
	  end
        else gen_rm (adjbp, reg, base_addr);
      array_addr := reg_addr (reg);
      array_addr.mode := byte;
    end;
    if base_desc.trans_code <> no_trans then
      array_addr := case_op (array_addr, base_desc.trans_code)
    else if array_ref_exp^.usage_count > 1 then begin

      (* An element with multiple uses is the operand of a check tuple
         which must be loaded here to avoid loss of the byte pointer
         and/or multiple loads. *)

      free (array_addr);
      reg := get_reg (bits_per_unit);
      do_move (reg, array_addr, right_aligned, 7);
      array_addr := reg_addr (reg);
    end
  end
  else if array_ref_exp^.base_array^.desc.base^.element_size >= bits_per_unit
    then array_addr := unpacked_array_addr ( array_ref_exp )
    else array_addr := packed_array_addr ( array_ref_exp );

end  (* proc array_addr *) ;
$PAGE do_binary_op
(* DO BINARY OP performs a binary operation, given two operands whose size is
   two words or less.  Note that double word operands are assumed to have an
   opcode which takes double operands, i.e. instructions are not applied twice.  *)
  
function do_binary_op (* (opcode: opc_range; operand1: expr; operand2: expr;
                        attributes: op_attr_set): registers *);

var
  op1_addr,
  op2_addr,
  temp_addr: addr_desc;
  op1,
  op2: expr;
  reg: registers;

begin
  op1_addr := fetch_fullword (operand1);
  op2_addr := fetch_fullword (operand2);

  (* Commute operands if permitted and useful. *)

  if (commutative in attributes) andif
             not is_register (op1_addr) andif
             (is_register (op2_addr) andif (regdesc [op2_addr.offset].uses_remaining = 1)) then begin
    temp_addr := op1_addr;
    op1_addr := op2_addr;
    op2_addr := temp_addr;
    op1 := operand2;
    op2 := operand1;
  end
  else begin
    op1 := operand1;
    op2 := operand2;
  end;

  (* Load the first operand into a reg and generate the operation.  Note that since
     the first operand was FETCH'ed, not LOAD'ed, there is the danger that it is
     already in a register that has multiple uses.  That must be handled explicitly.  *)

  reg := load_addr (op1_addr, alignment (op1), expr_size (op1^.desc));
  if regdesc [reg].uses_remaining > 1 then begin
    free (op1_addr);
    reg := get_reg (expr_size (op1^.desc));
    do_move (reg, op1_addr, alignment (op1), expr_size (op1^.desc))
  end;
  free (op2_addr);                            (* free any regs associated w/ 2nd operand *)
  gen_rm (opcode, reg, op2_addr);            (* generate the operation *)
  do_binary_op := reg;

end  (* proc do_binary_op *);
$PAGE single_op_addr
(* SINGLE_OP_ADDR generates code for a binary operation involving
   single word operands and returns an address descriptor for the result.
   Note: Currently the operations handled by this routine are integer
   addition, subtraction and multiplication.  *)

function single_op_addr ( exp: expr ): addr_desc;

type
  sb_record = packed record                     (* table mapping tuple opcodes into *)
    opcode: opc_range;                          (* instruction opcodes and attributes *)
    attr: op_attr_set
  end;

  sb_array = packed array [iadd_op..imul_op] of sb_record;

const
  sb: sb_array = ( (add, [commutative]),
                   (sub, []),
                   (imul, [commutative])
                 );

var
  reg: registers;

begin
  with exp^ do begin
    reg := do_binary_op ( sb[opcode].opcode, operand[1], operand[2],
                          sb[opcode].attr );
    single_op_addr := reg_addr ( reg );
  end;
end  (* proc single_op_addr *) ;
$PAGE real_op
(* REAL_OP is passed an expression tuple corresponding to real
   valued binary operation.  It generates code to perform the 
   operation and returns an address descriptor for the result.  *)

function real_op ( exp: expr ): addr_desc;

type
  real_op_array = packed array [radd_op..rdiv_op] of opc_range;
  r_attr_array = packed array [radd_op..rdiv_op] of op_attr_set;

const
  single_ops : real_op_array = ( fadr, fsbr, fmpr, fdvr );
  double_ops: real_op_array = ( dfad, dfsb, dfmp, dfdv );
  real_attrs: r_attr_array = ([commutative],[],[commutative],[]);

var
  instr_opcode: opc_range;
  reg: registers;

begin
  with exp^ do begin
    if operand[ 1 ]^.desc.precision <= srealprec 
      then instr_opcode := single_ops[ opcode ]
      else instr_opcode := double_ops[ opcode ];
    
    reg := do_binary_op ( instr_opcode, operand[ 1 ], operand[ 2 ],
                          real_attrs[ opcode ] );
    real_op := reg_addr ( reg );

  end  (* with *) ;
end  (* proc real_op *) ;
$PAGE idiv_or_imod
(* IDIV_OR_IMOD is passed an IDIV_OP or IMOD_OP tuple.  It generates
   the code for the operation and returns an address descriptor for
   the result.  *)

function idiv_or_imod ( exp: expr ): addr_desc;

var
  op1_addr: addr_desc;
  op2_addr: addr_desc;
  op1_reg: registers;
  reg1: registers;

begin
  with exp^ do begin

    op1_addr := fetch ( operand[ 1 ] );
    op2_addr := fetch_fullword ( operand[ 2 ] );

    (* Make sure we have a register PAIR available for the divide.  *)

    reg1 := coerce_to_double ( op1_addr, alignment ( operand[1] ) );

    (* generate the divide and free the unused member of the register pair. *)

    free ( op2_addr );
    gen_rm ( intdiv, reg1, op2_addr );

    if opcode = idiv_op then begin              (* DIV operation *)
      idiv_or_imod := reg_addr ( reg1 );
      free_and_disassociate ( reg1 + 1 );
    end
    else begin                                  (* MOD operation *)
      idiv_or_imod := reg_addr ( reg1 + 1 );
      free_and_disassociate ( reg1 );
    end;

  end  (* with *) ;
end  (* proc idiv_or_imod *) ;
$PAGE real_unary_ops
(* REAL_UNARY_OPS generates code for the real valued unary
   operations RABS_OP and RNEG_OP.  An address descriptor for the
   result of the operation is returned.  *)

function real_unary_ops ( exp: expr ): addr_desc;

var
  size: bit_range;
  reg: registers;
  instr_opcode: opc_range;

begin
  with exp^ do begin
    size := expr_size ( desc );
    if opcode = rabs_op then begin              (* absolute value operator *)
      if size = bits_per_unit then begin        (* single precision *)
        reg := do_move_op ( movm, exp, bits_per_unit );
      end
      else begin                                (* double precision *)
        reg := load ( operand[1], 2 * bits_per_unit );
        gen ( jump + gec, reg, 0, 2, dot );
        gen_rr ( dmovn, reg, reg );
      end;
    end

    else if opcode = rneg_op then begin         (* negation operator *)
      if size = bits_per_unit
        then instr_opcode := movn
        else instr_opcode := dmovn;
      reg := do_move_op (instr_opcode, exp, size );
    end;

    real_unary_ops := reg_addr ( reg );         (* function return value *)

  end  (* with *) ;
end  (* proc real_unary_ops *) ;
$PAGE do_fetch
(* DO FETCH is passed an expression class tuple.  It caals the necessary
   routines to generate just enough code to make the result of the
   expression addressible on the target machine.  Normall, FETCH or
   FETCHSTRING should be called to generate code for an expression tuple
   since they contain the logic necessary to handle tuples with multiple
   uses. *)

public function do_fetch ( exp: expr ): addr_desc;

var
  addr: addr_desc;
  reg, reg1: registers;
  string_desc: str_desc;

begin
  with exp^ do begin

    case opcode of

      cst_ref:
        do_fetch := const_addr ( exp );

      ident_ref:
        do_fetch := ident_addr ( exp );

      field_ref:
        do_fetch := field_addr ( exp );

      ptr_ref:
        do_fetch := deref_addr ( exp );

      array_ref:
        do_fetch := array_addr ( exp );

      buffer_ref:
        do_fetch := file_deref ( exp );

      substr_ref: begin
        assert ( desc.kind = chars );
        do_fetch := array_addr (exp);
      end;

      iadd_op,
      isub_op,
      imul_op:
        do_fetch := single_op_addr ( exp );

      idiv_op,
      imod_op:
        do_fetch := idiv_or_imod ( exp );

      radd_op,
      rsub_op,
      rmul_op,
      rdiv_op:
        do_fetch := real_op ( exp );

      sclcvt_op:
        do_fetch := do_fetch (operand[1]);

      expii_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_exp_ii ) );

      expri_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_exp_ri ) );

      exprr_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_exp_rr ) );

      ineg_op:
        do_fetch := reg_addr ( do_move_op ( movn, exp, bits_per_unit ) );

      rneg_op:
        do_fetch := real_unary_ops ( exp );

      iabs_op:
        do_fetch := reg_addr ( do_move_op ( movm, exp, bits_per_unit ) );

      rabs_op:
        do_fetch := real_unary_ops ( exp );

      float_op:
        do_fetch := do_float_op ( exp );

      trunc_op:
        do_fetch := reg_addr ( round_or_trunc ( exp ) );

      sqrt_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_sqrt ) );

      ln_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_ln ) );

      log_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_log ) );

      exp_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_exp ) );

      sin_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_sin ) );

      arcsin_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_asin ) );

      sinh_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_sinh ) );

      cos_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_cos ) );

      arccos_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_acos ) );

      cosh_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_cosh ) );

      tan_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_tan ) );

      tanh_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_tanh ) );

      cotan_op:
        do_fetch := reg_addr ( rt_call ( exp, rt_r_ctan ) );

      addr_op:
        do_fetch := address_op (exp);

      lwb_op: begin
	addr := fetch (operand[1]);
        do_fetch := lower_bound (operand[1], addr);
	free (addr)
      end;
  
      upb_op:
	if operand[1]^.desc.kind = strings then begin
	  string_desc := fetchstring (operand[1], max_length);
	  free (string_desc.base_addr);
	  if string_desc.text_valid then
	    free (string_desc.text_addr);
	  do_fetch := string_desc.len_addr;
	end
	else begin
	  addr := fetch (operand[1]);
	  do_fetch := upper_bound (operand[1], addr);
	  free (addr)
	end;

      length_op: begin
        string_desc := fetchstring (operand[1], actual_length);
        free (string_desc.base_addr);
        if string_desc.text_valid then
          free (string_desc.text_addr);
        do_fetch := string_desc.len_addr;
      end;

      lwc_op,
      upc_op: begin     (* case conversion for chars only *)
        assert (desc.kind = chars);
        do_fetch := fetch (operand[1]);
        if opcode = lwc_op
          then do_fetch := case_op (do_fetch, lower_trans)
          else do_fetch := case_op (do_fetch, upper_trans);
      end;

      dim_op:
        do_fetch := do_dim_op ( exp );

      round_op:
        do_fetch := reg_addr ( round_or_trunc ( exp ) );

      arctan_op: begin
        if upperbound (operand) = 1
          then do_fetch := reg_addr ( rt_call ( exp, rt_r_atan ) )
          else do_fetch := reg_addr ( rt_call ( exp, rt_r_atn2 ) );
      end;

      imin_op,
      rmin_op:
        do_fetch := reg_addr ( do_min_max_op ( exp, ltc, expr_size ( exp^.desc ) ) );

      imax_op,
      rmax_op:
        do_fetch := reg_addr ( do_min_max_op ( exp, gtc, expr_size ( exp^.desc )) );

      index_op:
        do_fetch := do_index_op (exp);

      search_op,
      verify_op:
        do_fetch := str_search_verify (exp);
  
      alc_temp_op:
	do_fetch := get_temp ((exp^.desc.base^.base_size + 35) div 36);

      agg_val:
        assert (false); (* fetch should have seen non-nil result field *)
 
      func_call_op:
        if not p_b_address (exp^.desc.base) then
          do_fetch := scl_function_call (exp)
        else begin
          do_fetch := get_temp ((exp^.desc.base^.base_size + 35) div bits_per_unit);
          pas_call (exp, addr_return_value, nil, do_fetch)
        end;

      ile_op..filne_op,
      and_op, or_op, in_op, bnot_op, odd_op:
        do_fetch := reg_addr (load_bool (exp, false));

      new_op,
      extent_op: begin
        addr := argument (operand[1]);
        free (addr);
        reg := get_reg (bits_per_unit);
        if opcode = extent_op
          then gen_rt (pushj, sb, rt_extent)
          else gen_rt (pushj, sb, rt_new);
        gen_rm (arg, reg, arg_addr (addr));
        do_fetch := reg_addr (reg)
      end;

      open_op,
      rewrite_op,
      reset_op,
      update_op:
        do_fetch := rt_open_call (exp);

      eof_op,
      eoln_op,
      eopage_op: begin
        do_fetch := absolute_reference;
        do_fetch.index := load (operand[1], bits_per_unit);
        do_fetch.mode := slice;
        do_fetch.offset := 3;
        do_fetch.slice_size := 1;
        if opcode = eof_op then
          do_fetch.slice_offset := 0
        else if opcode = eoln_op then
          do_fetch.slice_offset := 1
        else if opcode = eopage_op then
          do_fetch.slice_offset := 2
      end;

      filesize_op: begin
        do_fetch := argument (operand[1]);
        gen_rt (pushj, sb, rt_file_size);
        gen_rm (arg, 0, arg_addr (do_fetch));
        free (do_fetch);
        reg := get_reg (bits_per_unit);
        gen_rr (move, reg, 1);
        do_fetch := reg_addr (reg);
      end;

      cursor_op: begin
        reg := load (operand[1], bits_per_unit);
        do_fetch := absolute_reference;
        do_fetch.index := reg;
        do_fetch.offset := 4;
      end;

      extstatus_op: begin
        gen_rt (pushj, sb, rt_extstatus);
        reg := get_reg (bits_per_unit);
        gen_rr (move, reg, 1);
        do_fetch := reg_addr (reg);
      end;

      iostatus_op: begin
        if operand[1] <> nil then begin
          addr := argument (operand[1]);
          gen_rt (pushj, sb, rt_iostatus);
          gen_rm (arg, 0, arg_addr (addr));
          free (addr);
        end
        else gen_rt (pushj, sb, rt_iostat_last);
        reg := get_reg (bits_per_unit);
        gen_rr (move, reg, 1);
        do_fetch := reg_addr (reg);
      end;

      masked_op,
      pending_op: begin
	reg := get_reg (bits_per_unit);
	addr := fetch (operand[1]);
	free (addr);
	if opcode = masked_op
	  then gen_rt (pushj, sb, rt_masked)
	  else gen_rt (pushj, sb, rt_pending);
	gen_rm (arg, reg, addr);
	do_fetch := reg_addr (reg)
      end;
  
      mathstatus_op,
      prgmstatus_op,
      spclstatus_op,
      exiostatus_op: begin
	if opcode = mathstatus_op then
	  gen_rt (pushj, sb, rt_stat_math)
	else if opcode = prgmstatus_op then
	  gen_rt (pushj, sb, rt_stat_program)
	else if opcode = spclstatus_op then
	  gen_rt (pushj, sb, rt_stat_special)
	else
	  gen_rt (pushj, sb, rt_stat_exio);
	reg := get_reg (bits_per_unit);
	gen_rr (move, reg, 1);
	do_fetch := reg_addr (reg)
      end;
  
      random_op: begin
        if upperbound (operand) = 1 then begin
          addr := fetch (operand[1]);
          free (addr);
          if desc.precision > srealprec
            then reg := get_reg (2 * bits_per_unit)
            else reg := get_reg (bits_per_unit);
          gen_rt (pushj, sb, rt_rand_set);
          gen_rm (arg, reg, addr);
        end
        else begin
          if desc.precision > srealprec
            then reg := get_reg (2 * bits_per_unit)
            else reg := get_reg (bits_per_unit);
          gen_rt (pushj, sb, rt_rand);
          gen_rr (arg, reg, 0);
        end;
        do_fetch := reg_addr (reg);
      end;

      time_op: begin
	gen_rt (pushj, sb, rt_time);
	reg := get_reg (bits_per_unit);
	gen_rr (move, reg, 1);
	do_fetch := reg_addr (reg);
      end;

      runtime_op: begin
	gen_rt (pushj, sb, rt_runtime);
	reg := get_reg (bits_per_unit);
	gen_rr (move, reg, 1);
	do_fetch := reg_addr (reg);
      end;

      others: begin
        assert (false);
        do_fetch := reg_addr (get_reg (bits_per_unit));
      end

    end  (* case *) ;
  end (* with *);
end (* do_fetch *);
$PAGE fetch
(* FETCH is passed an expression class tuple and returns an address
   descriptor for the result of the expression.  Any code necessary
   to make the expression value addressable via an address descriptor
   is generated.   *)
  
public function fetch (* ( exp: expr ): addr_desc *);   (* forward declared *)

var
  addr_ref: addr_ptr;
  addr: addr_desc;
  reg: registers;

begin
  with exp^ do begin

    (* If the FRE field is non-zero this tuple is an operand of a
       check tuple, which must be evaluated now. *)

    if ref_fre <> 0 then
      do_check (exp);

    (* If the RESULT field is not NIL then we have a multiple use
       expression which has already been evaluated.  In this case
       RESULT points to an address descriptor for the expression
       result, so we simply copy and return that address descriptor. *)

    if result <> nil then begin
      addr_ref := ptr ( ord ( result ) );
      fetch := addr_ref^;
    end
    else fetch := do_fetch (exp);

    (* Check for tuples with multiple uses.  When a tuple has multiple
       uses a copy of the address descriptor is made on the heap and
       the RESULT field of the expr tuple is st to point to the copy.
       That copy is returned for subsequent fetches of the expression.
       In addition any registers used by the address descriptor are
       marked as locked so they will not be freed after their
       initial use.  However if the expression value actually resides
       in a register, then locking the register is not sufficient -
       a use of the value may destroy it.  In that case the register
       is not locked.  Instead code is generated to copy the
       register into a temporary.  Then the address descriptor
       created on the heap is set to reference the temp.  *)

    if (usage_count > 1) andif (result = nil) then begin (* first of multiple uses *)
      new ( addr_ref );
      addr_ref^ := fetch;
      incr_reg_usages ( regs_used(fetch), usage_count - 1);
      result := ptr ( ord ( addr_ref ) );
    end;

    (* Decrement the usage count for the expression.  If the usage
       count drops to zero and we have an address descriptor on the
       heap, then dispose of the address descriptor.  *)

    usage_count := usage_count - 1;
    assert ( (usage_count >= 0) and (usage_count <> maximum(usage_range)) );
    if (usage_count = 0) andif (result <> nil) then  (* last of multiple uses *)
      dispose ( result );
  end  (* with *) ;
end  (* proc fetch *) ;
$PAGE fetch_fullword
(* FETCH FULLWORD is passed an expression class tuple and returns an address
   descriptor for the result of the expression.  The address descriptor is
   guaranteed to be of mode 'FW'.  FETCH FULLWORD is intended to be used when
   a fullword value is required.  FETCH FULLWORD first calls FETCH.  If the
   address descriptor returned is not mode 'FW', then the expression value is
   loaded into a register.  *)
  
public function fetch_fullword (* ( exp: expr ): addr_desc *) ;

var
  addr: addr_desc;
  reg: registers;

begin
  addr := fetch ( exp );

  if addr.mode <> fw then begin
    reg := load_addr ( addr, alignment ( exp ), expr_size ( exp^.desc ) );
    addr := reg_addr ( reg );
  end;
  fetch_fullword := addr;

end (* proc fetch_fullword *) ;
$PAGE argument
(* ARGUMENT is passed an expression class tuple and returns an address descriptor
   for the result of the expression.  The address descriptor is guaranteed to be
   of mode 'FW' and guaranteed not to be an indirect address.  If necessary, the
   result of the expression is loaded into a register in order to guarantee
   direct addressing of a fullword.  Temps are not generated by this routine;  thus
   the expression must yield a value of size 72 bits or less.  *)
  
public function argument (* ( exp: expr ): addr_desc *) ;

var
  reg: registers;

begin

  argument := fetch_fullword ( exp );
 
  if argument.indirect then begin
    free (argument);
    reg := get_reg (expr_size (exp^.desc));
    gen_rm (movei, reg, argument);
    argument := absolute_reference;
    argument.index := reg;      (* datum is 0(reg), not reg itself *)
  end;

end  (* proc argument *) .
     Í