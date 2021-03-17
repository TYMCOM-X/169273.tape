$WIDTH=100
$LENGTH=55
$TITLE VAXEXP.PAS, last modified 1/18/84, zw
MODULE vaxexp OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- VAX expression evaluation and register manipulation*)

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM PASIST.INC
$SYSTEM PASPT.TYP
$SYSTEM PASIF.TYP
$SYSTEM VAXCG.TYP
$SYSTEM PASIFU.INC
$SYSTEM VAXCGU.INC
$SYSTEM VAXOPC.INC
$SYSTEM VAXGEN.INC
$SYSTEM VAXUTL.INC
$SYSTEM UTLMTH.INC
$SYSTEM VAXSTR.INC
$SYSTEM VAXSET.INC
$SYSTEM VAXCLL.INC
$SYSTEM VAXCMP.INC
$SYSTEM PASTAL.INC
$SYSTEM PTMCON.INC
$SYSTEM VAXIO.INC

$PAGE global data and forward declarations

PUBLIC VAR
    regdesc: reg_descriptor; (* for all registers *)
    max_reg: registers; (* highest numbered reg usable in current proc *)
    reg_use_by_block: ^reg_set_vector;

PUBLIC FUNCTION load (exp: expr): registers;

FORWARD;

PUBLIC FUNCTION copy_load ( exp: expr ): registers;

FORWARD;

PUBLIC FUNCTION load_addr (addr: addr_desc; align: data_alignment): registers;

FORWARD;

PUBLIC FUNCTION fetch (exp: expr; targ_addr: addr_desc )
  : addr_desc options special(coercions);

FORWARD;

PUBLIC FUNCTION fetch_fullword (exp: expr): addr_desc;

FORWARD;

PUBLIC FUNCTION mem_fetch (exp: expr): addr_desc;

FORWARD;

PUBLIC FUNCTION argument (exp: expr): addr_desc;

FORWARD;

PUBLIC FUNCTION is_register (maddr: addr_desc): boolean;

FORWARD;

TYPE
    op_attrs = (commutative, mem_form, double);
    op_attr_set = set OF op_attrs;
$PAGE exp_init

PUBLIC PROCEDURE exp_init;
(*performs all one-time-only initialization for this module.
  In particular, it allocates and initializes the register use vector. *)

VAR
    i: index_range;

BEGIN
  new ( reg_use_by_block, blk_number );
  for i := 1 to blk_number DO
    reg_use_by_block^[ i ] := [];
END;
$PAGE alignment

PUBLIC FUNCTION alignment (exp: expr): data_alignment;
(*returns the data alignment type for an expression ("exp"). *)

BEGIN
  CASE exp^.desc.kind OF
    ints, bools, chars, scalars, pointers:
      IF exp^.desc.signed THEN
	alignment := signed_value
      ELSE
	alignment := unsigned_value;
    reals:
      alignment := right_aligned; (* always occupies full words *)
    arrays, records:
      alignment := right_aligned;
    others:
      alignment := right_aligned
  END;
END;
$PAGE reg_init

PUBLIC PROCEDURE reg_init;
(* initializes all registers descriptors to a free state. *)

VAR
    reg: registers;

BEGIN
  for reg := minimum (registers) to maximum (registers) DO BEGIN
    with regdesc[reg] DO BEGIN
      associate := noreg;
      uses_remaining := 0;
      with_reg := false;
    END;
  END;
  IF (cur_block^.parm_list.first = nil) AND
  (*current proc has no parameters*)
  ((cur_block^.return_sym = nil) ORIF
  (* and is not a function, or,*)
  (not passed_by_address(cur_block^.return_sym)) )
  (* AP not used to addr fcn ret value *)
  THEN
    max_reg := ap (* then we use AP as a general register *)
  ELSE
    max_reg := ap - 1;
END;
$PAGE decr_reg_usages

PUBLIC PROCEDURE decr_reg_usages (reg: registers);
(*decrements the usage count for a register and
  any associates.  *)

BEGIN
  IF (reg > 1) AND (reg <= max_reg) THEN
    with regdesc[reg] DO BEGIN
      uses_remaining := uses_remaining - 1;
      assert ( uses_remaining <> maximum( usage_range ) );
      (* would occur if 0 decremented *)
      IF associate <> noreg THEN BEGIN
	with regdesc[associate] DO BEGIN
	  uses_remaining := uses_remaining - 1;
	  IF uses_remaining <= 0 THEN
	    associate := noreg;
	END;
      END;
      IF uses_remaining <= 0 THEN
	associate := noreg;
    END;
END;
$PAGE tag_reg

PUBLIC PROCEDURE tag_reg (reg1, reg2: registers);
(* associates two registers *)

BEGIN
  regdesc[reg1].associate := reg2;
  regdesc[reg2].associate := reg1;
END;
$PAGE is_immediate

PUBLIC FUNCTION is_immediate ( addr: addr_desc ): boolean;
(* returns true if parameter ADDR describes an
   immediate value (of either a scalar or real type).  *)

BEGIN
  is_immediate := (addr.addr_mode = other) ANDIF addr.immediate;
END;
$PAGE is_disp_mode

PUBLIC FUNCTION is_disp_mode ( addr: addr_desc ): boolean;
(* passed an address descriptor and returns TRUE
   if the address descriptor describes a VAX diplacement mode
   address.  Note that FALSE is returned if the address is
   indexed or indirect.  *)

BEGIN
  is_disp_mode := (addr.addr_mode = other ) ANDIF (addr.index = noreg) ANDIF
    (addr.register <> noreg) ANDIF (not addr.immediate) ANDIF
      (not addr.indirect) ANDIF (addr.reloc.kind <> register_sc);
END;
$PAGE is_symbol_addr

PUBLIC FUNCTION is_symbol_addr ( addr: addr_desc ): boolean;
(* passed an address descriptor and returns
   TRUE if the address descriptor describes the address of a
   static symbol.  FALSE is returned if the address is indexed.  *)

BEGIN
  is_symbol_addr := ( addr.addr_mode = other ) ANDIF ( not addr.immediate )
    ANDIF ( not addr.indirect ) ANDIF ( addr.register = noreg ) ANDIF
      ( addr.index = noreg );
END;
$PAGE indexable

PUBLIC FUNCTION indexable ( addr: addr_desc ): boolean;
(* returns true if the address passed in may be indexed
   and returns false otherwise.  *)

BEGIN
  indexable := ( not is_immediate ( addr ) ) ANDIF ( addr.index = noreg ) ANDIF
    ( not is_register ( addr ) );
END;
$PAGE regs_used

PUBLIC PROCEDURE regs_used (maddr: addr_desc;
  VAR primary_reg, index_reg: registers );
  (* returns the registers used by an address descriptor, if any. *)

BEGIN
  primary_reg := noreg;
  with maddr DO BEGIN
    index_reg := index;
    IF not is_immediate ( maddr ) THEN
      primary_reg := register;
  END;
END;
$PAGE free

PUBLIC PROCEDURE free (maddr: addr_desc);
(* decrements the usage counts of any registers associated
 with an ADDR_DESC   (except with base registers).      *)

VAR
    base_reg: registers;
    index_reg: registers;

BEGIN
  regs_used ( maddr, base_reg, index_reg );
  decr_reg_usages ( base_reg );
  decr_reg_usages ( index_reg );
END;
$PAGE vax_type_size

PUBLIC FUNCTION vax_type_size ( vtype: vax_type ): align_range;
(* is passed a VAX_TYPE and returns the number
   of bits required to represent the VAX_TYPE.  *)

TYPE
    vax_size_array = packed array [vax_type] OF align_range;

CONST
    vtype_size: vax_size_array = ( 8 , 16 , 32 , 64 , 32 , 64 );

BEGIN
  vax_type_size := vtype_size[ vtype ];
END;
$PAGE mark_regs_used

PUBLIC PROCEDURE mark_regs_used ( regs_used: set_of_registers );
(* adds a given set of registers to the list of registers
   used in the current block.  *)

BEGIN
  with cur_block^ DO
    reg_use_by_block^[ number] := reg_use_by_block^[ number] + (regs_used - [r0,
      r1,fp,sp,pc]);
END;
$PAGE get_reg

PUBLIC FUNCTION get_reg (p: bit_range): registers;
(* allocates a register or register pair (depending on the value of "p");
   the register allocated is returned.  *)

VAR
    reg: registers;

BEGIN
  for reg := (max_reg - ord( p > bits_per_reg ) ) downto 2 DO BEGIN
    IF (regdesc[reg].uses_remaining = 0) ANDIF ( (p <= bits_per_reg) ORIF
      (regdesc[reg + 1].uses_remaining = 0) ) THEN BEGIN
	regdesc[reg].uses_remaining := 1;
	IF p > bits_per_reg THEN BEGIN (* allocating a register pair *)
	  regdesc[reg + 1].uses_remaining := 1;
	  tag_reg ( reg, reg + 1 );
	  with cur_block^ DO
	    reg_use_by_block^[ number] := reg_use_by_block^[ number] + [reg, reg
	      + 1];
	END
	ELSE BEGIN
	  regdesc[reg].associate := noreg;
	  with cur_block^ DO
	    reg_use_by_block^[ number] := reg_use_by_block^[ number] + [reg];
	END;
	get_reg := reg;
	return; (* <--- short-circuit return *)
      END;
  END; (* for *)
  fatal_error ('Register overflow');
END;
$PAGE get_vax_reg

PUBLIC FUNCTION get_vax_reg ( vtype: vax_type ): registers;
(* is a wrapper for GET_REG.  It permits the caller
   to specify the register size via a VAX_TYPE.  *)

BEGIN
  get_vax_reg := get_reg ( vax_type_size ( vtype ) );
END;
$PAGE incr_reg_usages

PROCEDURE incr_reg_usages ( reg: registers; additional_uses: usage_range );
(* increments the USES_REMAINING field for a register
   and any associated registers.  *)

BEGIN
  IF (reg > 1) AND (reg <= max_reg) THEN BEGIN
    with regdesc[reg] DO BEGIN
      uses_remaining := uses_remaining + additional_uses;
      IF associate <> noreg THEN BEGIN
	with regdesc[associate] DO
	  uses_remaining := uses_remaining + additional_uses;
      END;
    END;
  END;
END;
$PAGE update_usages

PUBLIC PROCEDURE update_usages ( addr: addr_desc; count: usage_range );
(* UPDATE_USAGES increments the usage counts for all the registers
   used by a given address descriptor (parameter ADDR).  The usage
   counts are incremented by the value of parameter COUNT.  *)

VAR
    reg1, reg2: registers;

BEGIN
  regs_used ( addr, reg1, reg2 );
  incr_reg_usages ( reg1, count );
  incr_reg_usages ( reg2, count );
END;
$PAGE with_start

PUBLIC PROCEDURE with_start (with_tpl: tuple) options special(coercions);
(* initializes a with-descriptor for a new with construct *)

VAR
    addr_ref: addr_ptr;
    i: usage_range;
    reg: registers;
    maddr: addr_desc;

BEGIN
  maddr := fetch (with_tpl, no_preference); (* get addressibility for record *)
  free (maddr);
  IF maddr.indirect OR (maddr.index <> noreg) OR (maddr.register <> noreg) AND
    (maddr.register < fp) AND not regdesc[maddr.register].with_reg THEN BEGIN (* load address into a register *)
      IF (maddr.register <> noreg) AND (maddr.register < fp) THEN
	for i := 1 to with_tpl^.usage_count DO
	  decr_reg_usages (maddr.register);
      IF index <> noreg THEN BEGIN
	assert ((index < fp) AND not regdesc [maddr.index].with_reg);
	for i := 1 to with_tpl^.usage_count DO
	  decr_reg_usages (maddr.index)
      END;
      reg := get_reg (bits_per_unit);
      regdesc[ reg ].with_reg := true;
      incr_reg_usages (reg, with_tpl^.usage_count - 1);
      (* get_reg gave it 1 already *)
      gen2 (movab, maddr, reg_addr (reg));
      dispose (with_tpl^.result);
      new (addr_ref);
      addr_ref^ := absolute_reference;
      addr_ref^.register := reg;
      with_tpl^.result := ptr (ord (addr_ref))
    END
END;
$PAGE is_register

PUBLIC FUNCTION is_register (* maddr: addr_desc): boolean *);
(* returns a flag indicating whether or not an address ("maddr")
   designates a register. *)

BEGIN
  is_register := ( maddr.addr_mode = other ) ANDIF ( not maddr.immediate ) ANDIF
    ( maddr.reloc.kind = register_sc ) ANDIF ( maddr.register > 1 ); (* if value in 0 or 1, copy it *)
END;
$PAGE expr_size

PUBLIC FUNCTION expr_size ( exp: expr ): bit_range;
(* calculates the size in bits of a value given an expression
   tuple for the value.  *)

BEGIN
  with exp^.desc DO BEGIN
    IF kind in [bools,ints,chars,scalars] THEN
      expr_size := int_prec
    ELSE IF kind in [pointers,files] THEN
      expr_size := bits_per_address
    ELSE IF kind in [procs,funcs] THEN
      expr_size := 2 * bits_per_address
    ELSE IF ( base <> nil ) ANDIF ( kind <> sets ) THEN
      expr_size := base^.base_size
    ELSE IF kind = sets THEN
      expr_size := setb_size ( exp ) * bits_per_byte
    ELSE IF kind = strings THEN BEGIN
      expr_size := str_length * char_size;
      IF str_kind = varying THEN
	expr_size := expr_size + str_lw_width;
    END
    ELSE
      assert ( false ); (* what is it? *)
  END;
END;
$PAGE unpacked_vax_type

FUNCTION packed_vax_type ( tnode: typ): vax_type;

FORWARD;

PUBLIC FUNCTION unpacked_vax_type ( tnode: typ ): vax_type;
(* passed a TYPE_NODE pointer and returns the
   corresponding VAX type.  The type passed in is assumed not to be
   a component of a packed structure.  *)

BEGIN
  CASE tnode^.kind OF
    bools, chars:
      unpacked_vax_type := vax_byte;
    scalars:
      unpacked_vax_type := packed_vax_type (tnode);
    ints, pointers, files:
      unpacked_vax_type := vax_long;
    reals:
      IF tnode^.precision <= srealprec THEN
	unpacked_vax_type := vax_float
      ELSE
	unpacked_vax_type := vax_double;
    procs, funcs:
      unpacked_vax_type := vax_quad;
      (* sets, strings, records, and arrays are all set to vax_byte *)
      (* note that CMPC and MOVC expect byte type addresses *)
    sets, strings, records, arrays:
      unpacked_vax_type := vax_byte;
    others:
      assert ( false ) (* what is it ??? *)
  END
END;
$PAGE packed_vax_type

FUNCTION packed_vax_type ( tnode: typ ): vax_type;
(* is passed a type node and returns the corresponding
   VAX_TYPE.  The type passed in is assumed to be a component of a
   packed structure.  *)

BEGIN
  with tnode^ DO BEGIN
    IF kind in [scalars, ints] THEN BEGIN
      IF base_size <= bits_per_byte THEN
	packed_vax_type := vax_byte
      ELSE IF base_size <= 2 * bits_per_byte THEN
	packed_vax_type := vax_word
      ELSE
	packed_vax_type := vax_long;
    END
    ELSE
      packed_vax_type := unpacked_vax_type ( tnode );
  END
END;
$PAGE expr_vax_type

PUBLIC FUNCTION expr_vax_type ( exp: expr ): vax_type;
(* is passed an expression tuple and returns the
   corresponding VAX_TYPE.  Note that the expression value is
   assumed to be unpacked.  *)

BEGIN
  with exp^.desc DO BEGIN
    IF (kind = strings) OR (kind = sets) THEN
      expr_vax_type := vax_byte
    ELSE IF kind = reals THEN BEGIN (* precision in expr_type_node overrides *)
      IF precision <= srealprec (* that in type node *)
      THEN
	expr_vax_type := vax_float
      ELSE
	expr_vax_type := vax_double
    END
    ELSE IF kind = scalars THEN
      expr_vax_type := unpacked_vax_type ( base^.base_type )
    ELSE
      expr_vax_type := unpacked_vax_type ( base );
  END;
END;
$PAGE rt_value_call

FUNCTION rt_value_call ( exp: expr; rt_sym: rt_symbol; targ_addr: addr_desc )
  : addr_desc;
  (* generates a call to a runtime routine whose parameters
     are ALL passed by value.  An address descriptor is returned describing
     the location of the runtime routine's result.  Parameter EXP is the
     operator tuple corresponding to the runtime routine and parameter
     RT_SYM is the ssingle precision runtime routine's enumeration symbol.
     The corresponding double precision routine's symbol is assumed to
     be the successor of the single precision routine's symbol.  Finally,
     the runtime routine is assumed to be a function returning a value in
     R0 or R0 and R1.  *)

TYPE
    param_ary = array [ 1..* ] OF addr_desc;

CONST
    arg_bits := 32; (* bits per argument word *)

VAR
    temp_addr: addr_desc;
    arg_count: 0..255;
    i: 0..255;
    params : ^param_ary;
    result_vtype: vax_type;
    reg: registers;
    vtype: vax_type;

BEGIN
  with exp^ DO BEGIN
  (* Fetch all the operands before pushing any. this is to accomodate
     the user who has any dynamic temporaries in the parameter list. *)
    arg_count := 0;
    new ( params, upperbound (operand) );
    for i := upperbound (operand) downto 1 DO BEGIN
      IF operand[ i ] <> nil THEN BEGIN
      (* if there is only one parameter, try simple targetting to SP *)
	IF upperbound (operand) = 1 THEN BEGIN
	  temp_addr := push_reference;
	  temp_addr.byte_size := expr_vax_type ( operand [ 1 ] );
	  IF temp_addr.byte_size in [ vax_byte , vax_word ] THEN
	    temp_addr.byte_size := vax_long;
	  params^[1] := fetch ( operand [ 1 ] , temp_addr );
	  IF not adr_equal ( params^[1] , temp_addr ) THEN
	    push_value ( params^[1] , alignment(operand [ 1 ] ) )
	END
	ELSE
	  params^[i] := fetch ( operand[ i ] , no_preference );
	IF params^[ i ].byte_size in [vax_byte, vax_word, vax_long] THEN
	  vtype := vax_long
	ELSE
	  vtype := params^[ i ].byte_size;
	arg_count := arg_count + (vax_type_size(vtype) div arg_bits);
      END;
    END;
    (* Now push the parameters, since any dynamic temporarys will have been
       created and are already upon the stack. *)
    IF upperbound (operand) <> 1 THEN
      for i := upperbound (operand) downto 1 DO
	push_value ( params^[ i ] , alignment ( operand [ i ] ) );
    dispose ( params ); (* Free that storage *)
    (* Generate the call *)
    result_vtype := expr_vax_type ( exp );
    IF result_vtype = vax_double THEN
      gen_rt ( arg_count, succ ( rt_sym ) )
    ELSE
      gen_rt ( arg_count, rt_sym );
      (* Move the result (in R0 or R0 and R1) to a non-volatile register. *)
    IF not adr_equal(targ_addr,no_preference) AND
      (targ_addr.byte_size=result_vtype) THEN BEGIN
	temp_addr := r0_addr;
	temp_addr.byte_size := result_vtype;
	store ( temp_addr , targ_addr , alignment ( exp ) );
	rt_value_call := targ_addr
      END
      ELSE BEGIN
	reg := load_addr ( typ_reg_addr(R0, result_vtype), alignment(exp) );
	rt_value_call := typ_reg_addr ( reg, result_vtype );
      END;
  END
END;
$PAGE rt_ref_call

FUNCTION rt_ref_call ( exp: expr; rt_sym: rt_symbol; targ_addr: addr_desc )
  : addr_desc;
  (* generates a call to a runtime routine whose parameters
     are ALL passed by address.  An address descriptor is returned
     describing the location of the runtime routine's result.  Parameter
     EXP is the operator tuple corresponding to the runtime routine and
     parameter RT_SYM is the single precision variation of the runtime
     routine.  The enumeration symbol for the corresponding double precision
     routine is assumed to be the successor of the single precision
     symbol.  The routine is assumed to be a function returning a value in
     R0 or R0 and R1.  It is also assumed that the runtime routine has at
     most 2 parameters.  Finally, it is assumed that the runtime routine
     ignores its argument count (the argument count may be increased
     to include temporaries which will then be automatically popped from
     the stack upon return).  *)

CONST
    arg_bits := 32; (* bits per argument word *)

VAR
    arg_count: 0..255;
    temp_addr: addr_desc;
    op1: addr_desc;
    op2: addr_desc;
    vtype1: vax_type;
    vtype2: vax_type;
    result_vtype: vax_type;
    reg: registers;

BEGIN
  with exp^EGIN
    arg_count := upperbound (operand); (* count of argument longwords, *)
    (* initially set to number of parameters *)
    (* Only 1 parameter - if its in a reg then push it onto the stack.
	   Push the parameter address onto the stack.  *)
    IF upperbound (operand) = 1 THEN BEGIN
      op1 := fetch_fullword ( operand[ 1 ] );
      vtype1 := op1.byte_size;
      IF is_register ( op1 ) OR is_immediate ( op1 ) THEN BEGIN
	push_value ( op1, alignment ( operand[ 1 ] ) );
	push_value ( sp_addr, unsigned_value );
	arg_count := arg_count + vax_type_size ( vtype1 ) div arg_bits;
      END
      ELSE
	push_address ( op1 );
    END
    (* Two parameters - if either parameter is in a register then
       push it onto the stack.  The address of the parameters are
       then pushed onto the stack in reverse order.  *)
    ELSE IF upperbound (operand) = 2 THEN BEGIN
      op1 := fetch_fullword ( operand[ 1 ] );
      op2 := fetch_fullword ( operand[ 2 ] );
      vtype1 := op1.byte_size;
      vtype2 := op2.byte_size;
      IF is_register ( op1 ) OR is_immediate ( op1 ) THEN BEGIN
      (* first param is in a reg *)
	push_value ( op1, alignment(operand[ 1 ]) );
	(* push 1st param, update its *)
	op1 := stack_top_reference; (* address and increment arg *)
	op1.byte_size := vtype1; (* word count *)
	arg_count := arg_count + (vax_type_size(vtype1) div arg_bits);
	IF is_register ( op2 ) OR is_immediate ( op2 ) THEN BEGIN
	(* both params are in regs *)
	  push_value (op2, alignment(operand[2]) ); (* push 2nd param, *)
	  op2 := stack_top_reference; (* update both addresses and *)
	  op2.byte_size := vtype2; (* argument word count *)
	  op1.offset := (vax_type_size(vtype2) + bits_per_address)
	    div bits_per_byte;
	  arg_count := arg_count + (vax_type_size(vtype2) div arg_bits);
	END
	ELSE
	  op1.offset := bits_per_address div bits_per_byte;
      END
      ELSE IF is_register( op2 ) OR is_immediate ( op2 ) THEN BEGIN
      (* Only 2nd param in reg *)
	push_value (op2, alignment(operand[ 2 ]) );
	op2 := stack_top_reference;
	op2.byte_size := vtype2;
	arg_count := arg_count + vax_type_size ( vtype2 ) div arg_bits;
      END;
      push_address ( op2 );
      push_address ( op1 );
    END (* end 2 operand case *)
    ELSE
      assert ( false ); (* not prepared for > 2 operands !!! *)
      (* Generate the call to the routine. *)
    result_vtype := expr_vax_type ( exp );
    IF result_vtype = vax_double THEN
      gen_rt ( arg_count, succ ( rt_sym ) )
    ELSE
      gen_rt ( arg_count, rt_sym );
      (* Move the result from R0 or R0 and R1 to a non-volatile register
	 or the prefered target location, if possible. *)
    IF not adr_equal(targ_addr,no_preference) AND
      (targ_addr.byte_size=result_vtype) THEN BEGIN
	temp_addr := r0_addr;
	temp_addr.byte_size := result_vtype;
	store ( temp_addr , targ_addr , alignment ( exp ) );
	rt_ref_call := targ_addr
      END
      ELSE BEGIN
	reg := load_addr ( typ_reg_addr(R0, result_vtype), alignment(exp) );
	rt_ref_call := typ_reg_addr ( reg, result_vtype );
      END;
  END
END;
$PAGE float_or_trunc_op

FUNCTION float_or_trunc_op ( exp: expr; targ_addr: addr_desc ): addr_desc;
(* generates code for FLOAT_OP and TRUNC_OP
   expression tuples (parameter EXP).  An address descriptor
   for the result is returned.  *)

TYPE
    opc_array = packed array [ vax_type ] OF opc_range;

CONST
    cvt_opcodes : opc_array = (cvtlb, cvtlw, cvtbl, 0, cvtlf, cvtld);

VAR
    result_vtype: vax_type;
    operand_addr: addr_desc;
    opc: opc_range;
    reg: registers;

BEGIN
  with exp^ DO BEGIN
  (* Determine the VAX_TYPE of the operand and of the result.
     Fetch address of operand.  *)
    IF adr_equal ( targ_addr , no_preference ) THEN
      result_vtype := expr_vax_type ( exp )
    ELSE
      result_vtype := targ_addr.byte_size;
    operand_addr := fetch ( operand[ 1 ] , no_preference );
    (* Get correct opc.  Float_ops are generated whenever two real
       values have different precisions - it may not be necessary to
       generate code.  *)
    IF result_vtype = operand_addr.byte_size THEN
      float_or_trunc_op := operand_addr (* No conversion ness. *)
    ELSE BEGIN
      assert ( cvt_opcodes[result_vtype] <> 0 ); (* QUAD WORDS ??? *)
      opc := typ_opc ( cvt_opcodes [ result_vtype ] , operand_addr.byte_size );
      free ( operand_addr ); (* Make it available again *)
      (* If it is possible to fulfill the target request, do so. If not, then
		get a register of the appropriate size and move the result there. *)
      IF adr_equal ( targ_addr , no_preference ) THEN
	float_or_trunc_op := typ_reg_addr( get_vax_reg(result_vtype)
	  , result_vtype )
      ELSE
	float_or_trunc_op := targ_addr;
	(* Now move the ressult into the target area. *)
      gen2 ( opc , operand_addr , float_or_trunc_op )
    END
  END
END;
$PAGE do_round_op

FUNCTION do_round_op ( exp: expr; targ_addr: addr_desc ): addr_desc;
(* generates the code for a ROUND_OP expression tuple.
   An address descriptor for the result is returned. *)

VAR
    reg: registers;
    operand_vtype: vax_type;
    addr: addr_desc;

BEGIN
  with exp^ DO BEGIN
    operand_vtype := expr_vax_type ( operand[ 1 ] );
    IF upperbound (operand) = 1 THEN BEGIN (* real to integer round *)
      addr := fetch ( operand[ 1 ] , no_preference );
      free ( addr );
      reg := get_vax_reg ( vax_long );
      gen_mr ( typ_opc(cvtrfl, operand_vtype), addr, reg );
      do_round_op := reg_addr ( reg ); (* always return longword result *)
    END
    ELSE
      do_round_op := rt_value_call(exp,rt_r_rnd2,targ_addr);
      (* real to real *)
  END
END;
$PAGE do_min_max_op

FUNCTION do_min_max_op (exp: expr; max_op: boolean): addr_desc;
(* compiles code for integer or real, min or max operators, given
   the operator node ("exp") for the operator.  Parameter MAX_OP is TRUE
   if the operator is IMAX_OP or RMAX_OP and FALSE for a IMIN_OP or
   RMIN_OP.  *)

VAR
    reg: registers;
    mem: addr_desc;
    i: index_range;
    vtype: vax_type;
    local_label: def;

BEGIN
  with exp^ DO BEGIN
  (* Load the first operand into a destroyable register.  As we compare this
     value against each successive operand, if the value of the operand is
     found to be less (greater) than the first, the register is loaded with
     the value of the operand compared against. *)
    reg := copy_load ( operand[ 1 ] );
    vtype := expr_vax_type ( exp );
    do_min_max_op := typ_reg_addr ( reg, vtype );
    (* Process each operand and compare with the min/max value to date. *)
    for i := 2 to upperbound (operand) DO BEGIN
      mem := fetch_fullword ( operand[ i ] );
      gen2( typ_opc(cmpl,vtype), mem, do_min_max_op );
      local_label := make_def ( local_def );
      IF max_op THEN
	gen_branch ( bleq, local_label ) (* maximum *)
      ELSE
	gen_branch ( bgeq, local_label ); (* minimum *)
      store ( mem, do_min_max_op, signed_value );
      free ( mem );
      mark_def ( code_area, local_label ); (* define local_label *)
    END
  END
END;
$PAGE iconstp

PUBLIC FUNCTION iconstp (exp: expr; VAR val: int_type): boolean;
(* a predicate which indicates if its argument has a constant value.
   It is assumed that the argument has an ordinal type.  If the argument does
   have a constant value, its value is returned. *)

VAR
    texp: expr;

BEGIN
  IF exp^.opcode = sclcvt_op THEN
    texp := exp^.operand[1]
  ELSE
    texp := exp;
  IF (texp^.opcode = cst_ref) ANDIF (texp^.cst_val.kind = scalar_cst) THEN BEGIN
    iconstp := true;
    val := texp^.cst_val.ival;
  END
  ELSE
    iconstp := false;
END;
$PAGE aconstp

PUBLIC FUNCTION aconstp (maddr: addr_desc; VAR val: int_type): boolean;
(* serves the same function as ICONSTP, except that it accepts
   as a parameter an ADDR DESC insead of an EXPR.       *)

BEGIN
  aconstp := is_immediate ( maddr ) ANDIF
    not ( maddr.byte_size in [vax_float, vax_double] );
  IF aconstp THEN
    val := maddr.offset;
END;
$PAGE duplicate_addr

PUBLIC FUNCTION duplicate_addr ( addr: addr_desc ): addr_desc;
(* increases the usage counts by one on all registers
   used by parameter ADDR.  It then returns ADDR as its function value. *)

VAR
    base_reg: registers;
    index_reg: registers;

BEGIN
  regs_used ( addr, base_reg, index_reg );
  incr_reg_usages ( base_reg, 1 );
  incr_reg_usages ( index_reg, 1 );
  duplicate_addr := addr;
END;
$PAGE load_addr

PUBLIC FUNCTION load_addr ( addr: addr_desc; align: data_alignment ):registers;
(* loads an object into a register, given its address
   (parameter ADDR) and its required alignment (parameter
   ALIGNMENT).  The register loaded is returned as the function
   return value.
   Note that:
     1. a longword value already in a register will not be copied,
     2. parameter ADDR is freed by this routine.
     3. byte and word values will be converted to longwords
	during the move.  *)

VAR
    vtype: vax_type;
    dest_addr: addr_desc;

BEGIN
  IF is_register ( addr ) AND not ( addr.byte_size in [vax_byte, vax_word] )
    THEN
      load_addr := addr.register
    ELSE BEGIN
      free ( addr );
      IF addr.byte_size in [vax_byte,vax_word] THEN
	vtype := vax_long
      ELSE
	vtype := addr.byte_size;
      load_addr := get_vax_reg ( vtype );
      dest_addr := typ_reg_addr ( load_addr, vtype );
      store ( addr, dest_addr, align );
    END;
END;
$PAGE load

FUNCTION load ( exp: expr ): registers ;
(* *)

VAR
    addr: addr_desc;

BEGIN
  addr := fetch ( exp , no_preference );
  IF is_register ( addr ) AND not ( addr.byte_size in [vax_byte, vax_word] )
    THEN
      load := addr.register
    ELSE
      load := load_addr ( addr, alignment ( exp ) );
END;
$PAGE copy_load

PUBLIC FUNCTION copy_load ( exp: expr ): registers ;
(* loads an expression tuple value into a register.
   If the value is already in a register, it will be copied to a
   new reg if the register has more than one remaining use or if
   it is a WITH register.  Note that byte and word values are
   converted to longword values when loaded.  *)

VAR
    addr: addr_desc;
    primary_reg: registers;
    index_reg: registers;
    must_load: boolean;
    dest_addr: addr_desc;

BEGIN
  addr := fetch ( exp , no_preference ); (* fetch address of value *)
  regs_used ( addr, primary_reg, index_reg );
  must_load := not is_register ( addr ) ORIF
  (* must load in value not in reg, or, *)
  (addr.byte_size in [vax_byte, vax_word]) ORIF
  (* byte or word value, or, *)
  (regdesc[primary_reg].uses_remaining > 1);
  (* if uses remain after load *)
  IF must_load THEN BEGIN
    free ( addr );
    copy_load := get_vax_reg ( expr_vax_type ( exp ) );
    dest_addr := typ_reg_addr ( copy_load, expr_vax_type ( exp ) );
    store ( addr, dest_addr, alignment ( exp ) );
  END
  ELSE
    copy_load := addr.register; (* no load necessary *)
END;
$PAGE move_address

PUBLIC FUNCTION move_address ( addr: addr_desc ): registers;
(* load an address into a register.  ADDR is an address
   descriptor representing the address; the function return value
   is the number of the register containing the address.  Parameter
   ADDR is freed by this routine.  *)

VAR
    opcode: opc_range;

BEGIN
  assert ( not (is_immediate(addr) OR is_register(addr)) );
  free ( addr );
  move_address := get_reg ( bits_per_address );
  opcode := typ_opc ( moval, addr.byte_size );
  gen_mr ( opcode, addr, move_address );
END;
$PAGE cvt_word

PUBLIC FUNCTION cvt_word ( addr: addr_desc; align: data_alignment ): addr_desc;
(* converts a value to VAX_TYPE VAX_WORD.  Parameter ADDR
   is the address of the value.  Parameter ALIGN is the alignment of
   the value.  The function return value is an address descriptor
   for the converted value.  *)

VAR
    addr_value: int_type;

BEGIN
  IF addr.byte_size = vax_word THEN
    cvt_word := addr
    (* already a word, just return ADDR *)
  ELSE IF ( aconstp ( addr, addr_value ) ) OR (* if constant, or, *)
  ( (addr.byte_size = vax_long) AND (* unsigned longword, and, *)
  (addr.index = noreg) AND (* not indexed, then *)
  ( align = unsigned_value ) ) THEN BEGIN (* just change BYTE_SIZE *)
    cvt_word := addr; (* field of ADDR *)
    cvt_word.byte_size := vax_word;
  END
  ELSE BEGIN (* convert by moving to a register *)
    free ( addr );
    cvt_word := typ_reg_addr ( get_vax_reg ( vax_word ), vax_word );
    store ( addr, cvt_word, align );
  END;
END;
$PAGE cvt_long

PUBLIC FUNCTION cvt_long ( addr: addr_desc; alignment: data_alignment )
  : addr_desc;
  (* is passed the address and alignment of an object whose
     length is assumed to be either 1, 2 or 4 bytes.  If the BYTE_SIZE
     field of the address descriptor is VAX_BYTE or VAX_WORD, then the
     object is converted to longword length.  Parameter ADDR is 'used'
     by this routine and thus is invalid upon return unless it was
     'duplicated' via a call to DUPLICATE_ADDR.  *)

VAR
    reg: registers;

BEGIN
  IF addr.byte_size <> vax_long THEN BEGIN
    assert ( addr.byte_size in [vax_byte, vax_word] );
    IF is_immediate ( addr ) THEN BEGIN (* if an immediate value, then *)
      cvt_long := addr; (* simply change BYTE_SIZE *)
      cvt_long.byte_size := vax_long;
    END
    ELSE BEGIN (* otherwise, load it into a reg *)
      free ( addr );
      cvt_long := reg_addr ( get_reg ( bits_per_reg ) );
      store ( addr, cvt_long, alignment );
    END;
  END
  ELSE
    cvt_long := addr; (* already  a longword *)
END;
$PAGE cvt_quad

FUNCTION cvt_quad ( long_addr: addr_desc ): addr_desc;
(* converts a longword value to a quadword value.  Parameter
   LONG_ADDR is freed by this routine.  *)

VAR
    is_reg: boolean;
    reg: registers;
    result_reg: registers;
    count_addr: addr_desc;

BEGIN
  assert ( long_addr.byte_size = vax_long ); (* we really do want a longword *)
  IF is_immediate ( long_addr ) THEN BEGIN (* if immediate value, just *)
    cvt_quad := long_addr; (* change BYTE_SIZE field *)
    cvt_quad.byte_size := vax_quad;
  END
  ELSE BEGIN (* move (if necessary) and sign extend value *)
    is_reg := is_register ( long_addr );
    IF is_reg THEN
      reg := long_addr.register;
    free ( long_addr );
    result_reg := get_vax_reg ( vax_quad ); (* get a reg pair *)
    cvt_quad := typ_reg_addr ( result_reg, vax_quad );
    (* The conversion may be done in any of 3 ways, in each case
       however, an arithemetic shift right of 32 bits is used.  *)
    count_addr := typ_int_value ( -32, vax_byte );
    IF is_reg ANDIF ( result_reg = reg ) (* lower result reg is orginal reg *)
    THEN
      gen3 ( ashl, count_addr, reg_addr(reg), reg_addr(reg + 1) )
    ELSE IF is_reg ANDIF (result_reg = reg - 1)
    (* upper result reg is original reg *)
    THEN
      gen3 ( ashq, count_addr, cvt_quad, cvt_quad )
    ELSE BEGIN (* must move original value to reg pair *)
      gen_mr ( movl, long_addr, result_reg );
      gen3 ( ashl, count_addr, reg_addr(result_reg), reg_addr(result_reg+1) );
    END;
  END
END;
$PAGE scalar_convert

FUNCTION scalar_convert ( exp: expr ): addr_desc;
(* generates code, if necessary, for SCLCVT_OP expression
   tuples.  Parameter EXP is the SCLCVT_OP tuple; an address
   descriptor for the coverted value is returned as the
   function return value.  *)

VAR
    dest_type: type_kind;
    source_alignment: data_alignment;
    dest_alignment: data_alignment;
    dest_addr: addr_desc;
    dest_vtype: vax_type;

BEGIN
(* Fetch the value to be converted; determine the alignment of both
   the orginal and the converted values.  *)
  dest_type := exp^.desc.kind;
  scalar_convert := fetch ( exp^.operand[ 1 ] , no_preference );
  source_alignment := alignment ( exp^.operand[ 1 ] );
  dest_alignment := alignment ( exp );
  (* For conversions to integer types, we insure that
     the value conforms to the alignment specified in the result's
     expression type descriptor record.  *)
  IF dest_type = ints THEN BEGIN
    IF ( source_alignment in [unsigned_value, right_aligned] ) ANDIF
      ( dest_alignment = signed_value ) AND
	( scalar_convert.byte_size <> vax_long ) THEN BEGIN
	  free ( scalar_convert );
	  dest_addr := reg_addr ( get_reg ( bits_per_integer ) );
	  store ( scalar_convert, dest_addr, source_alignment );
	  scalar_convert := dest_addr;
	END;
  END
  (* If the result is a boolean, char or enumerated type then coerce the
     value to the proper size.  *)
  ELSE IF dest_type in [bools, chars, scalars] THEN BEGIN
    dest_vtype := expr_vax_type ( exp );
    IF scalar_convert.byte_size <> dest_vtype THEN BEGIN
      IF (dest_vtype < scalar_convert.byte_size) ANDIF
	(source_alignment = unsigned_value) ANDIF (scalar_convert.index = noreg)
	  THEN
	    scalar_convert.byte_size := dest_vtype
	  ELSE BEGIN
	    free ( scalar_convert );
	    dest_addr := typ_reg_addr (get_vax_reg(dest_vtype), dest_vtype);
	    store ( scalar_convert, dest_addr, unsigned_value );
	    scalar_convert := dest_addr;
	  END;
    END;
  END
  (* if the result is a file or pointer, then simply convert the source
     to a longword value.  *)
  ELSE IF dest_type in [pointers, files] THEN BEGIN
    IF scalar_convert.byte_size <> vax_long THEN BEGIN
      free ( scalar_convert );
      dest_addr := reg_addr ( get_reg ( bits_per_address ) );
      store ( scalar_convert, dest_addr, source_alignment );
      scalar_convert := dest_addr;
    END;
  END
  ELSE
    assert ( false ); (* what is it?!? *)
END;
$PAGE load_frame_ptr

FUNCTION load_frame_ptr ( id_sym: sym ): registers;
(* chases the static chain for non-local references.
   Parameter ID_SYM is the symbol node pointer for the
   non_local symbol.  The function return
   value is the number of the register containing the appropriate
   frame pointer (or for parameters, the argument block pointer).  *)

CONST
    parent_addr: addr_desc := (vax_long,other,false,false,noreg,fp,-4,(
      absolute_sc));
    chain_addr: addr_desc := (vax_long,auto_dec,false,false,noreg,noreg,0,(
      absolute_sc));

VAR
    reg: registers;
    level_difference: level_index;
    chase_addr: addr_desc;
    is_parameter: boolean;
    i: level_index;
    value_func_sym: boolean;

BEGIN
  with id_sym^ DO BEGIN
    is_parameter := id_sym^.dcl_class = parameter_sc;
    (* TRUE if parameter or fcn return value *)
    value_func_sym := (id_sym = id_sym^.block^.return_sym) ANDIF
    (* TRUE if fcn return value  *)
    ( not passed_by_address ( id_sym ) );
    (* and stored as local variable *)
    IF is_parameter AND not value_func_sym THEN
      reg := ap
    ELSE
      reg := fp;
    IF block <> cur_block THEN BEGIN
    (* non-local reference, must chase static chain *)
      level_difference := cur_block^.apparent_level - block^.apparent_level;
      IF level_difference <> 0 THEN BEGIN
	reg := get_reg ( bits_per_address );
	gen_mr ( movl, parent_addr, reg );
	chase_addr := chain_addr;
	chase_addr.register := reg;
	for i := 2 to level_difference DO
	  gen_mr ( movl, chase_addr, reg );
	IF is_parameter AND not value_func_sym THEN BEGIN
	(* if parameter, load the saved *)
	  chase_addr := absolute_reference; (* argument pointer *)
	  chase_addr.register := reg;
	  chase_addr.offset := stk_ap_offset;
	  gen_mr ( movl, chase_addr, reg );
	END;
      END;
    END;
    load_frame_ptr := reg;
  END
END;
$PAGE param_addr

FUNCTION param_addr ( id_sym: sym ): addr_desc;
(* is passed a symbol node of kind VARS or VALUES and
   whose DCL_CLASS field has the value PARAMETER_SC.  An address
   descriptor record addressing the parameter is returned. *)

VAR
    reg: registers;

BEGIN
  reg := load_frame_ptr ( id_sym );
  param_addr := parm_reference;
  with param_addr DO BEGIN
    reloc.relsym := id_sym;
    register := reg;
    IF id_sym^.type_desc^.packable THEN
      byte_size := packed_vax_type ( id_sym^.type_desc )
    ELSE
      byte_size := unpacked_vax_type ( id_sym^.type_desc );
  END;
  param_addr.indirect := passed_by_address ( id_sym );
END;
$PAGE local_addr

FUNCTION local_addr ( id_sym: sym ): addr_desc;
(* is passed a symbol node of kind CONSTS, VARS or VALUES
   and whose DCL_CLASS field has the value LOCAL_SC.  An address
   descriptor record addressing the local variable is returned. *)

VAR
    reg: registers;

BEGIN
  reg := load_frame_ptr ( id_sym );
  local_addr := null_location; (* construct addr record to return *)
  with local_addr DO BEGIN
    register := reg;
    reloc.kind := local_sc;
    reloc.relsym := id_sym;
    IF id_sym^.type_desc^.packable THEN
      byte_size := packed_vax_type ( id_sym^.type_desc )
    ELSE
      byte_size := unpacked_vax_type ( id_sym^.type_desc );
  END;
END;
$PAGE static_addr

FUNCTION static_addr ( id_sym: sym ): addr_desc;
(* is passed a symbol node of kind CONSTS, VARS or VALUES
   and whose storage class is STATIC_SC.  It returns an address
   descriptor record which addresses the symbols storage location. *)

BEGIN
  static_addr := null_location;
  with static_addr DO BEGIN
    reloc.kind := static_sc;
    reloc.relsym := id_sym;
    IF id_sym^.kind = conditions THEN
      byte_size := vax_long
    ELSE IF id_sym^.type_desc^.packable THEN
      byte_size := packed_vax_type ( id_sym^.type_desc )
    ELSE
      byte_size := unpacked_vax_type ( id_sym^.type_desc );
  END;
END;
$PAGE external_addr

FUNCTION external_addr ( id_sym: sym ): addr_desc;
(* is passed a symbol node of kind CONSTS, VARS or VALUES
   and whose storage class is EXTERNAL_SC.  It returns an address
   descriptor record which addresses the symbol's storage location. *)

BEGIN
  external_addr := null_location;
  with external_addr DO BEGIN
    reloc.kind := external_sc;
    reloc.relsym := id_sym;
    IF id_sym^.kind = conditions THEN
      byte_size := vax_long
    ELSE IF id_sym^.type_desc^.packable THEN
      byte_size := packed_vax_type ( id_sym^.type_desc )
    ELSE
      byte_size := unpacked_vax_type ( id_sym^.type_desc );
      (* If overlay option is in effect, then we must reference all
	 externals indirectly, i.e. through transfer vectors. *)
    IF ( prog_options.overlay_opt AND ( id_sym^.kind <> conditions ) ) ANDIF
      ( ( id_sym^.kind <> consts ) ORIF
	not ( id_sym^.type_desc^.kind in [procs, funcs] ) ) THEN
	  indirect := true
  END;
END;
$PAGE ident_addr

FUNCTION ident_addr ( exp: expr ): addr_desc;
(* *)

BEGIN
  with exp^ DO BEGIN
    assert ( id_sym^.kind in [consts,vars,values,conditions] );
    (* must hold to access storage class field *)
    IF id_sym^.kind = conditions THEN
      assert (id_sym^.dcl_class in [static_sc, external_sc]);
    CASE id_sym^.dcl_class OF
      local_sc:
	ident_addr := local_addr ( id_sym );
      parameter_sc:
	ident_addr := param_addr ( id_sym );
      static_sc:
	ident_addr := static_addr ( id_sym );
      external_sc:
	ident_addr := external_addr ( id_sym );
      constant_sc: BEGIN
	ident_addr := null_location;
	ident_addr.reloc := gen_cval ( id_sym^.init_value, id_sym^.type_desc );
	IF id_sym^.type_desc^.packable THEN
	  ident_addr.byte_size := packed_vax_type ( id_sym^.type_desc )
	ELSE
	  ident_addr.byte_size := unpacked_vax_type ( id_sym^.type_desc );
      END;
      others:
	assert ( false ) (* Cannot happen !?! *)
    END
  END
END;
$PAGE binary_addr

FUNCTION binary_addr ( opcode: opc_range; op1: addr_desc; op2: addr_desc;
  attributes: op_attr_set; targ_addr:addr_desc): addr_desc;
  (* generates code for a binary operation.
     This routine serves the same purpose as DO_BINARY_OP except this
     routine takes ADDR_DESC records as the operand parameters rather
     than expression tuples.  Parameters OP1_ADDR and OP2_ADDR
     are freed by this routine.
     The result is always left in a register and the register number is
     the function return value.  This routine makes several assumptions.
     First, parameter OPCODE must be the longword variation.
     Routine TYP_OPC is called to convert the opcode to the proper
     variation.  Second, this routine assumes that both 2 and 3
     operand variations of the instruction exist.  The 2 operand
     opcode must be passed in; the 3 operand opcode is assumed
     to equal the 2 operand opcode plus 1.
     Third, the BYTE_SIZE fields of the two operand parameters must
     have the same value.  Finally, note that,
     if the operation is not commutative, the second operand to
     this routine is generated as the FIRST operand of the
     instruction.  *)

VAR
    op1_addr, op2_addr, temp_addr: addr_desc;
    commute: boolean;
    both_in_regs: boolean;
    op2_reg_greater: boolean;
    result_reg: addr_desc;
    vtype: vax_type;
    opc: opc_range;

BEGIN
  op1_addr := op1; (* local copies so we can swap them *)
  op2_addr := op2;
  assert ( op1_addr.byte_size = op2_addr.byte_size );
  (* Commute operands if permitted and useful. *)
  both_in_regs := is_register ( op1_addr ) AND is_register ( op2_addr );
  IF both_in_regs THEN
    op2_reg_greater := op2_addr.register > op1_addr.register;
  commute := ( commutative in attributes ) ANDIF ( ( not is_register(op1_addr)
    AND is_register(op2_addr) ) ORIF ( both_in_regs ANDIF (* try to use higher numbered *)
    (op2_reg_greater OR (op1_addr.register > max_reg)) ) );
    (* reg for result *)
  IF commute THEN BEGIN
    temp_addr := op1_addr;
    op1_addr := op2_addr;
    op2_addr := temp_addr;
  END;
  (* Allocate the result register. *)
  vtype := op1_addr.byte_size; (*assume result type is same as first operand*)
  IF is_register ( op1_addr ) ANDIF (* If the second operand OF THE *)
  ( regdesc[ op1_addr.register ].uses_remaining = 1 ) THEN BEGIN
    free ( op1_addr ); (* INSTRUCTION is in a register, then *)
    (* free it 1st so we can use 2 opernad instruction *)
    IF adr_equal(targ_addr,no_preference) OR ( targ_addr.byte_size <> vtype )
      THEN
	binary_addr := typ_reg_addr( get_vax_reg(vtype) , vtype )
      ELSE
	binary_addr := targ_addr;
    free ( op2_addr );
  END
  ELSE BEGIN
    free ( op1_addr );
    free ( op2_addr );
    IF adr_equal(targ_addr,no_preference) OR ( targ_addr.byte_size <> vtype )
      THEN
	binary_addr := typ_reg_addr ( get_vax_reg(vtype) , vtype )
      ELSE
	binary_addr := targ_addr;
  END;
  opc := typ_opc ( opcode, vtype ); (* get correct opcode variation *)
  (* generate the instruction *)
  IF is_register(op1_addr) ANDIF is_register(binary_addr) ANDIF
    (op1_addr.register=binary_addr.register) THEN
      gen2 ( opc, op2_addr, op1_addr )
    ELSE
      gen3 ( opc + 1, op2_addr, op1_addr, binary_addr );
END;
$PAGE increment_addr

PUBLIC FUNCTION increment_addr ( base_addr: addr_desc; byte_offset: unit_range )
  : addr_desc;
  (* is given a base address (parameter BASE_ADDR)
     and a byte offset from the base (parameter BYTE_OFFSET).
     It returns the address of the byte at the given offset.
     The BYTE_SIZE field of the address descriptor returned
     must be set by the caller - this routine does not initialize
     it.  Parameter BASE_ADDR is 'used' by this routine and thus
     is no longer valid upon return unless it was previously
     'duplicated' via a call to DUPLICATE_ADDR.  *)

VAR
    reg: registers;

BEGIN
  IF byte_offset <> 0 THEN BEGIN
    assert ( not is_register ( base_addr ) );
    assert ( not is_immediate ( base_addr ) );
  END;
  increment_addr := base_addr;
  with increment_addr DO BEGIN
  (* Three cases are distinguished.  If the VAX addressing mode of
     BASE_ADDR is displacement then BYTE_OFFSET is simply added
     to the offset field.  If the address is of a static symbol,
     then the offset is added to the OFFSET field, producing an
     address of the form 'SYMBOL+BYTE_OFFSET'.
     In all other cases, the address is
     simply moved to a register and the address returned is
     'BYTE_OFFSET(REG)'.  *)
    IF is_disp_mode ( increment_addr ) THEN BEGIN (* Case 1 *)
      offset := offset + byte_offset;
    END
    ELSE IF is_symbol_addr ( increment_addr ) THEN BEGIN (* Case 2 *)
      offset := offset + byte_offset;
    END
    ELSE IF (byte_offset <> 0) ORIF (base_addr.index <> noreg) THEN BEGIN
      reg := move_address ( increment_addr ); (* move base addr to reg *)
      increment_addr := absolute_reference;
      register := reg;
      offset := byte_offset;
    END;
  END
END;
$PAGE const_addr

FUNCTION const_addr ( exp: expr ): addr_desc;
(* *)

VAR
    scalar_value: integer;

BEGIN
  with exp^.cst_val DO BEGIN
    IF kind in [scalar_cst,ptr_cst] THEN BEGIN (* pointer and scalar constants *)
      IF kind = ptr_cst THEN
	scalar_value := int_nil
      ELSE
	scalar_value := ival;
      const_addr := int_value ( scalar_value );
    END
    ELSE BEGIN (* all other constants *)
      const_addr := null_location;
      IF kind = subr_cst THEN
	const_addr := def_addr ( get_def ( subr_def, blkp^.number ) )
      ELSE IF (kind = set_cst) ANDIF (dimension (valp^.set_val) = 0) THEN
	const_addr := int_value ( 0 )
      ELSE
	const_addr.reloc := gen_cnode ( valp, exp^.desc.base );
    END;
    const_addr.byte_size := expr_vax_type ( exp );
    (* Hack - PASS1 says varying string constants are non-varying in
       EXPR_TYPE_DESCs.  *)
    IF (kind = string_cst) ANDIF ( (exp^.desc.str_kind = nonvarying) AND
      (valp^.str_varying_ref) ) THEN BEGIN
	const_addr := increment_addr ( const_addr, str_lw_width div
	  bits_per_byte );
	const_addr.byte_size := vax_byte;
      END;
  END (* with *);
END (* proc const_addr *);
$PAGE offset_addr

PUBLIC FUNCTION offset_addr ( VAR addr: addr_desc; offset: addr_desc )
  : addr_desc;
  (* OFFSET_ADDR is given a base address (parameter ADDR) and an address
     descriptor for a byte offset from the address (parameter OFFSET).  It
     returns an address descriptor for the location OFFSET bytes from
     ADDR, emitting any code necessary to calculate the returned address.
     Parameter OFFSET **MUST** be a longword value.  This routine will
     always return an address of the form 'n(REG)'; it does not attempt
     to form a VAX indexed mode address.
     NOTE:
       The BYTE_SIZE field of the address descriptor returned is not
     set since this routine has no knowledge of the object whose address
     is being formed.  Thus the caller must set the BYTE_SIZE field upon
     return from this routine.  *)

VAR
    offset_value: unit_range;
    temp_addr: addr_desc;

BEGIN
  assert ( not (is_register(addr) OR is_immediate(addr)) );
  assert ( offset.byte_size = vax_long );
  (* If the offset is a constant, then simply call increment_addr.  *)
  IF aconstp ( offset, offset_value ) THEN BEGIN
    offset_addr := increment_addr ( addr, offset_value );
  END
  (* The offset is not known at compile time.  We generate code to add it
     to the base address at runtime.  Several cases are distinguished based
     on the form of the base address.  *)
  ELSE BEGIN
  (* If the base address is indirect and not indexed, then a pointer
     to the addressed object is in memory and we can add the pointer
     and the offset, yielding an address of the form: '(REG)'.  *)
    IF addr.indirect AND (addr.index = noreg) THEN BEGIN
      addr.indirect := false;
      addr.byte_size := vax_long;
      offset_addr := absolute_reference;
      temp_addr := binary_addr(addl2, addr, offset, [commutative], no_preference
	);
      offset_addr.register := temp_addr.register;
    END
    (* If the address is of the form 'n(REG)', then add the contents
       of REG to the offset, placing the result in a register.  Then an
       address of the form 'n(RESULT_REG)' will be returned.  *)
    ELSE IF is_disp_mode ( addr ) THEN BEGIN
      offset_addr := absolute_reference;
      temp_addr := binary_addr ( addl2, reg_addr(addr.register)
	, offset, [commutative], no_preference );
      offset_addr.register := temp_addr.register;
      offset_addr.reloc := addr.reloc;
      offset_addr.offset := addr.offset;
    END
    (* If all else fails, then move the base address to a register and
       add the offset to that register.  The address returned will be
       of the form: '(REG)'.  *)
    ELSE BEGIN
      offset_addr := reg_addr ( move_address ( addr ) );
      free ( offset );
      gen2 ( addl2, offset, offset_addr );
      offset_addr.reloc := none;
    END;
  END (* else *);
END (* proc offset_addr *);
$PAGE field_addr

FUNCTION field_addr ( field_ref_exp: expr )
  : addr_desc options special(coercions);
  (* *)

VAR
    base_addr: addr_desc;
    byte_offset: unit_range;

BEGIN
  with field_ref_exp^ DO BEGIN
  (* Get base address of record.  *)
    base_addr := fetch ( base_rec , no_preference );
    (* Calculate fields address.  *)
    byte_offset := field_sym^.fld_offset div bits_per_byte;
    field_addr := increment_addr ( base_addr, byte_offset );
    IF base_rec^.desc.base^.packable THEN
      field_addr.byte_size := packed_vax_type ( field_sym^.type_desc )
    ELSE
      field_addr.byte_size := unpacked_vax_type ( field_sym^.type_desc );
  END (* with *);
END (* proc field_addr *);
$PAGE deref_addr

FUNCTION deref_addr ( exp: expr ): addr_desc;
(* DEREF_ADDR is passed a pointer valued expression tuple and returns
   an address descriptor for the target of the pointer.
   This routine has no type information about the target;
   therefore the caller must set the BYTE_SIZE field of the
   address descriptor returned.  *)

VAR
    reg: registers;

BEGIN
(* Fetch address of pointer being dereferenced.  *)
  deref_addr := fetch ( exp , no_preference );
  assert ( deref_addr.byte_size = vax_long );
  with deref_addr DO BEGIN
  (* If the base pointer is in a register, then simply return
     the address '0(reg)'.  *)
    IF is_register ( deref_addr ) THEN BEGIN
      reloc := none;
      offset := 0;
      index := noreg;
    END
    (* If the address of the pointer is of the form 'n(reg)',
       then simply return the address '@n(reg)'.  *)
    ELSE IF is_disp_mode ( deref_addr ) THEN BEGIN
      indirect := true;
    END
    (* Addresses of the form 'REL_SYMBOL' are converted to the
       form '@REL_SYMBOL'.  *)
    ELSE IF is_symbol_addr ( deref_addr ) THEN BEGIN
      indirect := true;
    END
    (* For all other addresses, the pointer is loaded into a
       register and the address '0(reg)' is returned.  *)
    ELSE BEGIN
      reg := load_addr ( deref_addr, unsigned_value );
      deref_addr := absolute_reference;
      deref_addr.register := reg;
    END;
  END (* with *);
END (* proc deref_addr *);
$PAGE file_deref

FUNCTION file_deref ( exp: expr ): addr_desc;
(* FILE_DEREF generates code for a file variable dereference.  *)

VAR
    reg: registers;

BEGIN
  reg := load ( exp^.base_file );
  file_deref := absolute_reference;
  with file_deref DO BEGIN
    indirect := true;
    register := reg;
    byte_size := expr_vax_type ( exp );
  END;
END;
$PAGE dynamic_flex

PUBLIC FUNCTION dynamic_flex ( array_expr: expr ): boolean;
(* DYNAMIC_FLEX is passed an expression tuple whose associated
   type is array or string.  It returns TRUE if the expression
   tuple represents a flexible array or string which has been
   dynamically allocated on the heap with an upperbound determined
   at run time.  FALSE is returned for flexible array or string
   formal parameters.  Thus this routine identifies flexible
   arrays or strings which have the upperbound word physically
   preceding the array or string.  *)

BEGIN
  with array_expr^ DO BEGIN
    dynamic_flex := ( ( (opcode = ptr_ref) OR (opcode = field_ref) ) ANDIF
      ( ( (desc.kind = strings) ANDIF (desc.str_flex) ) OR
	( (desc.kind = arrays ) ANDIF (desc.base^.flexible) ) ) );
  END;
END (* proc dynamic_flex *);
$PAGE get_lbound

FUNCTION get_lbound ( exp: expr ): integer;
(* GET_LBOUND is passed an expression tuple whose associated type
   is string or array.  It returns the constant
   lower bound of the string or array.  *)

BEGIN
  with exp^.desc DO BEGIN
    IF kind = strings THEN
      get_lbound := 1
    ELSE
      get_lbound := base^.index_type^.minval;
  END
END (* proc get_lbound *);
$PAGE get_ubound

FUNCTION get_ubound ( exp: expr ): integer;
(* GET_UBOUND is given an expression tuple whose associated type
   is a *** non-flexible *** array or string.  It returns the
   constant upper bound of the array or string.  *)

BEGIN
  with exp^.desc DO BEGIN
    IF kind = strings THEN
      get_ubound := str_length
    ELSE
      get_ubound := base^.index_type^.maxval;
  END;
END (* proc get_ubound *);
$PAGE upper_bound

PUBLIC FUNCTION upper_bound ( array_expr: expr; array_addr: addr_desc )
  : addr_desc;
  (* UPPER_BOUND is passed both the expression tuple for an array
     or string and an address descriptor for the base of the array
     or string (actually for the descriptor word for dynamic flexible
     array or strings).  Unless the array or string is flexible, the
     address descriptor will simply be an immediate reference.
     For a flexible array or string the
     address will be that of the descriptor word containing the
     upper bound.  If the array or string is flexible then the
     base address is duplicated; thus ARRAY_ADDR is still valid
     on return from this routine. *)

VAR
    addr: addr_desc;

BEGIN
  with array_expr^ DO BEGIN

  (* If array/string is not flexible then create an address
     descriptor for the constant upper bound.  *)
    IF ( (desc.kind = strings) ANDIF (not desc.str_flex) ) ORIF
      ( not desc.base^.flexible ) THEN BEGIN
	addr := int_value ( get_ubound ( array_expr ) );
      END
      (* The array or string is flexible.  The opcode should be one
	 of the following:
	   i. an ident_ref tuple for a formal parameter;
	  ii. a ptr_ref tuple for a flex array or string on the heap, or,
	 iii. a field_ref tuple for a record on the heap whose last field
	      is a flex array or string.  *)
      ELSE BEGIN

      (* If dynamic flex, then upperbound is in descriptor word preceeding
	 array or string.  *)

	IF (opcode = ptr_ref) OR (opcode = field_ref) THEN BEGIN
	  addr := duplicate_addr ( array_addr );
	END
	(* If opcode is ident_ref, then we have a formal parameter.  The
	   upperbound is the argument preceeding the address of the array.
	   The address of the first element of the array should be of the
	   form: '@offset(reg)' ; then the address of the upperbound is
	   'offset-4(reg)'.  *)
	ELSE IF opcode = ident_ref THEN BEGIN
	  addr := duplicate_addr ( array_addr );
	  assert ( addr.indirect );
	  addr.indirect := false;
	  addr.offset := addr.offset - 4;
	END
	ELSE BEGIN
	  assert ( false );
	END;
      END (* else *);

      (* For strings an address descriptor with vax_type VAX_WORD is returned;
	 for arrays vax_type VAX_LONG is returned.  *)
    IF desc.kind = strings THEN
      addr.byte_size := vax_word
    ELSE
      addr.byte_size := vax_long;
    upper_bound := addr;
  END (* with *);
END (* proc upper_bound *);
$PAGE bound_op

FUNCTION bound_op ( exp: expr ): addr_desc;
(* BOUND_OP generates code for a LWB_OP or UPB_OP tuple.  Note
   that these tuples are generated only for flexible arrays or
   strings.  Both tuples take a single operand - an expression
   tuple for the array or string whose bound is desired.  *
   Note that a word length result is returned for strings and a
   longword for arrays.  *)

VAR
    array_expr: expr;
    addr: addr_desc;
    string_desc: str_desc;

BEGIN
  array_expr := exp^.operand[ 1 ];
  IF array_expr^.desc.kind = strings THEN BEGIN (* strings *)
    string_desc := fetchstring ( array_expr, max_length );
    IF exp^.opcode = lwb_op THEN
      bound_op := int_value ( 1 )
    ELSE
      bound_op := duplicate_addr ( string_desc.len_addr );
    free_string ( string_desc );
  END
  ELSE BEGIN (* arrays *)
    addr := fetch ( array_expr , no_preference );
    IF exp^.opcode = lwb_op THEN
      bound_op := int_value ( get_lbound ( array_expr ) )
    ELSE
      bound_op := upper_bound ( array_expr, addr );
    free ( addr ); (* upperbound does not free the array's address *)
  END;
  IF array_expr^.desc.kind = strings THEN
    bound_op.byte_size := vax_word;
END (* proc bound_op *);
$PAGE do_dim_op

FUNCTION do_dim_op ( exp: expr ): addr_desc;
(* DO_DIM_OP generates code for a DIM_OP tuple (parameter EXP) and
   returns an address descriptor for the result.  DIM_OP tuples
   are generated only when the dimension is not known at compile
   time - when a flexible array or string is involved.  DIM_OP
   tuples have one operand - an expr tuple for the array or
   string whose dimension is desired.  They are generated both for
   calls to intrinsic function DIMENSION and for aggregate assignment
   compatability checks involving flex arrays.  The length of the
   result returned is derived from the length of the upperbound
   descriptor for the array or string.  *)

VAR
    array_expr: expr;
    array_addr: addr_desc;
    upper_addr: addr_desc;
    reg: registers;
    cons_part: integer;
    vtype: vax_type;
    opcode: opc_range;
    const_addr: addr_desc;

BEGIN
(* Calculate the address of the array and of its upperbound word. *)
  array_expr := exp^.operand[1];
  array_addr := fetch ( array_expr , no_preference );
  upper_addr := upper_bound ( array_expr, array_addr );
  free ( array_addr );
  (* Calculate upperbound - (lowerbound - 1)  *)
  cons_part := 1 - get_lbound ( array_expr );
  do_dim_op := add_constant ( upper_addr, cons_part, no_preference );
END (* proc do_dim_op *);
$PAGE do_new_op

FUNCTION do_new_op ( exp: expr; targ_addr: addr_desc ): addr_desc;
(* DO_NEW_OP generates code for a NEW_OP expression tuple.  The
   operand to a NEW_OP tuple is the size in bytes of the block of
   memory desired.  The result of a NEW_OP tuple
   evaluation is a pointer to the allocated block.  DO_NEW_OP
   returns an address descriptor for the pointer value.  *)

VAR
    reg: registers;
    size_addr: addr_desc;

BEGIN
(* Fetch the size of the area to be allocated. Since the size will be
   put onto the stack anyway, pass the stack id as the prefered target. *)
  size_addr := fetch ( exp^.operand [ 1 ] , push_reference );
  (* If the size addr returned is not a push reference, then push the value. *)
  IF not adr_equal ( size_addr , push_reference ) THEN
    push_value ( size_addr , alignment ( exp^.operand [ 1 ] ) )
  ELSE
    free ( size_addr );
  gen_rt ( 1, rt_new );
  (* If prefered targget is valid then move result there, else to a non-
     volatile register. *)
  IF adr_equal ( targ_addr, no_preference ) OR
    (targ_addr.byte_size <> vax_long ) THEN
      do_new_op := reg_addr ( load_addr ( r0_addr, alignment ( exp ) ) )
    ELSE BEGIN
      do_new_op := targ_addr;
      store ( r0_addr , targ_addr , alignment ( exp ) )
    END
END (* proc do_new_op *);
$PAGE address_op

FUNCTION address_op ( exp: expr ): addr_desc;
(* ADDRESS_OP generates code for an ADDR_OP expression tuple.
   Parameter EXP is the ADDR_OP tuple.  An address descriptor for
   the address formed is returned as the function return value.  *)

VAR
    string_desc: str_desc;
    addr: addr_desc;

BEGIN
  with exp^ DO BEGIN
  (* Fetch the address of the operand.  The addresses returned are
     incremented past any flex string or array descriptor bytes.  *)
    IF operand[ 1 ]^.desc.kind = strings THEN BEGIN (* strings *)
      string_desc := fetchstring ( operand[ 1 ], no_length );
      addr := duplicate_addr ( string_desc.base_addr );
      IF string_desc.base_is_bound THEN BEGIN (* if flex, skip bounds word *)
	addr := increment_addr ( addr, flex_str_desc_size div bits_per_byte );
	addr.byte_size := vax_byte;
      END;
      free_string ( string_desc );
    END
    ELSE BEGIN (* everything except strings *)
      addr := fetch ( operand [ 1 ] , no_preference );
      IF dynamic_flex ( operand[ 1 ] ) THEN BEGIN (* if dynamic flex, skip bounds word *)
	addr := increment_addr ( addr, flex_arr_desc_size div bits_per_byte );
	addr.byte_size := vax_byte;
      END;
    END;
    (* Move the address of the operand to a register and return the
       address of the register.  *)
    assert ( not is_register ( addr ) );
    assert ( not is_immediate ( addr ) );
    free ( addr );
    address_op := reg_addr ( get_reg ( bits_per_address ) );
    gen2 ( typ_opc ( moval, addr.byte_size ), addr, address_op );
  END (* with *);
END (* proc address_op *);
$PAGE case_op

FUNCTION case_op ( addr: addr_desc; case_code: str_translation ): addr_desc;
(* CASE_OP generates code for an upper or lower case conversion of a
   character.  Parameter ADDR is an address descriptor for the character
   to be translated.  Parameter CASE_CODE indicates whether an upper
   or lower case conversion is required.  This routine does not handle
   case conversions of strings.  Parameter ADDR is freed
   by this routine.  *)

VAR
    a,z: char;
    opcode: opc_range;
    a_addr, z_addr: addr_desc;
    out_label: def;
    delta_addr: addr_desc;

BEGIN
(* Set the comparison limits and the translation opcode.  *)
  IF case_code = lower_trans THEN BEGIN (* lowercase op *)
    a := 'A';
    z := 'Z';
    opcode := addb2;
  END
  ELSE BEGIN (* uppercase op *)
    a := 'a';
    z := 'z';
    opcode := subb2;
  END;
  a_addr := typ_int_value ( ord ( a ), vax_byte );
  z_addr := typ_int_value ( ord ( z ), vax_byte );
  (* Emit code to move the char to a register, compare it against the
     limits of upper or lower case alphabetics, and if within the limits,
     then translate the character.  *)
  free ( addr );
  case_op := typ_reg_addr ( get_reg ( char_size ), vax_byte );
  store ( addr, case_op, unsigned_value ); (* copy to reg *)
  gen2 ( cmpb, case_op, a_addr ); (* compare against lower limit of alphabetics *)
  out_label := make_def ( local_def );
  gen_branch ( blssu, out_label ); (* branch out if < *)
  gen2 ( cmpb, case_op, z_addr ); (* compare to upper limit of alphabetics *)
  gen_branch ( bgtru, out_label ); (* branch if greater *)
  delta_addr := typ_int_value ( ord ( 'a' ) - ord ( 'A' ), vax_byte );
  gen2 ( opcode, delta_addr, case_op ); (* translate *)
  mark_def ( code_area, out_label ); (* define the label *)
END (* proc case_op *);
$PAGE array_addr

PUBLIC FUNCTION array_addr ( exp: expr ): addr_desc;
(* ARRAY_ADDR  generates an address descriptor for a subscripted
   reference.  Parameter EXP may be either an ARRAY_REF tuple or
   a SUBSTR_REF tuple whose SUBSTR_LENGTH field has the constant
   value 1 (SUBSTR_REF tuples are generated for subscripted
   strings).  *)

VAR
    checking_on: boolean;
    desc_bytes: 0..4;
    dyna_flex: boolean;
    elem_size: bit_range;
    lower_bound: int_type;
    index_addr: addr_desc;
    base_addr: addr_desc;
    upb_addr: addr_desc;
    base_desc: str_desc;
    elem_vax_type: vax_type;
    lwb_addr: addr_desc;
    bytes_per_elem: unit_range;
    result_reg: registers;
    result_addr: addr_desc;
    size: addr_desc;
    index_in: addr_desc;
    result_offset: 0..4;
    index_value: int_type;
    byte_offset: unit_range;
    index_const: boolean;
    length_context: str_len_context;
    use_index_mode: boolean;
    base_adjustment: int_type;
    index_align: data_alignment;

BEGIN
  with exp^ DO BEGIN
    checking_on := chk_sub_opt in cur_block^.semantic_options;
    desc_bytes := 0; (* will count bytes of any length or *)
    (* bound words preceding array *)

    (* Set up some info we'll need later - first, we deal with
	   subscripted *arrays*.  *)
    IF opcode = array_ref THEN BEGIN
      dyna_flex := dynamic_flex ( base_array );
      IF dyna_flex THEN
	desc_bytes := flex_arr_desc_size div bits_per_byte;
      with base_array^.desc.base^ DO BEGIN
	elem_size := element_size;
	lower_bound := index_type^.minval;
      END;
      index_addr := fetch_fullword ( index_val );
      index_align := alignment ( index_val );
      base_addr := fetch ( base_array , no_preference );
      upb_addr := upper_bound ( base_array, base_addr );
      free ( upb_addr );
    END

    (* Get setup info for subscripted strings. *)
    ELSE IF opcode = substr_ref THEN BEGIN
      dyna_flex := dynamic_flex ( base_string );
      IF dyna_flex THEN
	desc_bytes := flex_str_desc_size div bits_per_byte;
      IF base_string^.desc.str_kind = varying THEN
	desc_bytes := desc_bytes + ( str_lw_width div bits_per_byte );
      elem_size := char_size;
      lower_bound := 1;
      index_addr := fetch_fullword ( substr_index );
      index_align := alignment ( substr_index );
      index_const := aconstp ( index_addr, index_value ); (* only need length if *)
      IF index_const OR (* INDEX instruction used *)
      ( (base_string^.desc.str_kind = varying) AND not checking_on) THEN
	length_context := no_length
      ELSE
	length_context := actual_length;
      base_desc := fetchstring ( base_string, length_context );
      base_addr := duplicate_addr ( base_desc.base_addr ) ;
      IF not index_const AND (* do not generate code if were not going *)
      ( not indexable ( base_addr ) OR checking_on ) THEN BEGIN
	upb_addr := cvt_long ( duplicate_addr ( base_desc.len_addr )
	  , unsigned_value ); (* to use the upperbound *)
	free ( upb_addr )
      END;
      free_string ( base_desc );
    END
    ELSE BEGIN
      assert ( false );
    END;
    (* Get info which is calculated the same way whether we have
       a string or an array. *)

    IF (opcode = array_ref) ANDIF (base_array^.desc.base^.packable) THEN
      elem_vax_type := packed_vax_type ( base_array^.desc.base^.element_type )
    ELSE
      elem_vax_type := expr_vax_type ( exp );
    lwb_addr := int_value ( lower_bound );
    bytes_per_elem := elem_size div bits_per_byte;
    free ( lwb_addr );
    (* Now we generate the indexing code! *)
    (* First, the easy case - a constant subscript.  *)
    IF aconstp ( index_addr, index_value ) THEN BEGIN
      byte_offset := desc_bytes + (index_value - lower_bound) * bytes_per_elem;
      array_addr := increment_addr ( base_addr, byte_offset );
    END
    (* Index is not constant. *)
    ELSE BEGIN
    (* If possible we generate a VAX index mode address.  If subscript
       checking is on we use the INDEX instruction; otherwise an add or
       subtract is used.  *)
      use_index_mode := ( indexable ( base_addr ) ) AND
	( (bytes_per_elem in [1,2,4]) ORIF ( (bytes_per_elem = 8) ANDIF
	  not dyna_flex ) ) ANDIF
	    ( bytes_per_elem * bits_per_byte = vax_type_size(elem_vax_type) );
      IF use_index_mode THEN BEGIN
	base_adjustment := -lower_bound + (desc_bytes div bytes_per_elem);
	IF checking_on THEN BEGIN (* checking on - use INDEX instr *)
	  free ( index_addr );
	  result_addr := reg_addr ( get_reg ( bits_per_address ) );
	  size := int_value ( 1 );
	  index_in := int_value ( base_adjustment );
	  gen6 ( index, index_addr, lwb_addr, upb_addr, size, index_in,
	    result_addr );
	END
	ELSE BEGIN (* checking off - use add or subtract *)
	  result_addr := add_constant ( index_addr, base_adjustment,
	    no_preference );
	  IF not is_register ( result_addr ) THEN
	    result_addr := reg_addr( load_addr(result_addr,index_align) );
	END;
	array_addr := base_addr;
	array_addr.index := result_addr.register;
      END
      (* We can't generate an indexed address.  Use the index instruction
	 to generate the *byte* offset from the base address.  If possible
	 we incorporate the desc bytes into the INDEX calculation here. *)
      ELSE BEGIN
	free ( index_addr );
	result_reg := get_reg ( bits_per_address );
	result_addr := reg_addr ( result_reg );
	size := int_value ( bytes_per_elem );
	IF (desc_bytes mod bytes_per_elem) = 0 THEN BEGIN
	  result_offset := 0;
	  index_in := int_value ( (desc_bytes div bytes_per_elem)
	    - lower_bound );
	END
	ELSE BEGIN
	  result_offset := desc_bytes;
	  index_in := int_value ( -lower_bound );
	END;
	gen6 ( index, index_addr, lwb_addr, upb_addr, size, index_in,
	  result_addr );

	  (* Offset the base address by the byte offset contained in
	     RESULT_REG plus the constant offset RESULT_OFFSET.  *)
	array_addr := offset_addr ( base_addr, result_addr );
	array_addr.offset := array_addr.offset + result_offset;
      END (* else *);
    END (* else *);

    (* Set the VAX_TYPE of the element. *)
    array_addr.byte_size := elem_vax_type;
    IF (opcode = substr_ref) ANDIF (* if base object was a string and case *)
    (base_desc.trans_code <> no_trans) (* ops were pending then upper/lower case the *)
    THEN
      array_addr := case_op ( array_addr, base_desc.trans_code ); (* resulting char *)
  END (* with *);
END (* proc array_addr *);
$PAGE do_binary_op

PUBLIC FUNCTION do_binary_op ( opcode: opc_range; operand1: expr;
  operand2: expr; attributes: op_attr_set; targ_addr: addr_desc ): addr_desc;
  (* DO_BINARY_OP generates code for a binary operation.
     The two operands (parameters OPERAND1 and OPERAND2) are fetched
     as longwords and then routine BINARY_ADDR is called to generate
     the binary operation instruction.  The number of the register
     containing the result is returned as the function value.  Users
     of this routine should note the assumptions made by routine
     BINARY_ADDR.  *)

VAR
    op1_addr: addr_desc;
    op2_addr: addr_desc;

BEGIN
  op1_addr := fetch_fullword ( operand1 );
  op2_addr := fetch_fullword ( operand2 );
  do_binary_op := binary_addr ( opcode, op1_addr, op2_addr, attributes,
    targ_addr );
END (* proc do_binary_op *);
$PAGE binary_op_addr

FUNCTION binary_op_addr ( exp: expr; targ_addr: addr_desc ): addr_desc;
(* BINARY_OP_ADDR generates code for simple binary operations
   and returns an address descriptor for the result.
   Note: Currently the operations handled by this routine are integer
   and real addition, subtraction, multiplication and division.  *)

TYPE
    sb_record = packed RECORD (* table mapping tuple opcodes into *)
      opcode: opc_range; (* instruction opcodes and attributes *)
      attr: op_attr_set
    END;
    sb_array = packed array [iadd_op..rdiv_op] OF sb_record;

CONST
    sb: sb_array = ( (addl2, [commutative]), (subl2, []), (mull2, [commutative])
      , (divl2, []), ( 0 , []), (* imod_op, illegal in this routine *)
      (addl2, [commutative]), (subl2, []), (mull2, [commutative]), (divl2, []) )
	;

VAR
    scalar_value: int_type;
    index: 0..2;
    addr: addr_desc;

BEGIN
  with exp^ DO BEGIN

  (* Integer addition or subtraction with a constant operand is done
     via function ADD_CONSTANT to take advantage of its special case
     checks.  *)
    index := 0;
    IF (opcode = iadd_op) OR (opcode = isub_op) THEN BEGIN
      IF iconstp ( operand[ 1 ], scalar_value ) AND (opcode = iadd_op) THEN
	index := 2 (* index of non-constant operand *)
      ELSE IF iconstp ( operand[ 2 ], scalar_value ) THEN
	index := 1
    END;
    IF index <> 0 THEN BEGIN (* constant operand is present *)
      addr := fetch_fullword ( operand[ index ] );
      IF opcode = isub_op THEN
	scalar_value := -scalar_value;
      binary_op_addr := add_constant ( addr, scalar_value, targ_addr );
    END
    (* If neither operand is constant then simply call DO_BINARY_OP. *)
    ELSE BEGIN
      binary_op_addr := do_binary_op ( sb[opcode].opcode, operand[1], operand[2]
	, sb[opcode].attr , targ_addr );
    END;
  END;
END (* proc binary_op_addr *);
$PAGE do_imod_op

FUNCTION do_imod_op ( exp: expr ): addr_desc;
(* DO_IMOD_OP is passed an IMOD_OP expression tuple.  It generates
   code for the operation and returns an address descriptor for
   the result.  *)

VAR
    op1_addr: addr_desc;
    op2_addr: addr_desc;

BEGIN
  with exp^ DO BEGIN
    op1_addr := fetch_fullword ( operand[1] );
    op1_addr := cvt_quad ( op1_addr ); (* dividend must be quad word *)
    op2_addr := fetch_fullword ( operand[2] );
    free ( op1_addr );
    free ( op2_addr );
    do_imod_op := reg_addr ( get_reg ( bits_per_integer ) );
    gen4 ( ediv, op2_addr, op1_addr, reg_addr ( r0 ), do_imod_op );
  END (* with *);
END (* proc do_imod_op *);
$PAGE do_neg_op

FUNCTION do_neg_op ( exp: expr; targ_addr: addr_desc ): addr_desc;
(* DO_NEG_OP is passed an INEG_OP or RNEG_OP tuple and returns
   an address descriptor for the result of the operation.  *)

VAR
    result_vtype : vax_type;
    vtype: vax_type;
    maddr : addr_desc;
    temp_addr : addr_desc;
    opc : opc_range;

BEGIN
(* If prefered target is given, fetch the results. If no target is given
   then fetch a fullword. *)
  IF adr_equal ( targ_addr , no_preference ) THEN BEGIN
    maddr := fetch_fullword ( exp^.operand [ 1 ] );
    free ( maddr );
    opc := typ_opc ( mnegl , maddr.byte_size );
    do_neg_op := typ_reg_addr ( get_vax_reg(maddr.byte_size),maddr.byte_size);
  END
  ELSE BEGIN
    maddr := fetch ( exp^.operand [ 1 ] , no_preference );
    free ( maddr );

    (* IF the sizes of the target and operand do not match, convert
       the source to the proper size then negate. *)
    IF targ_addr.byte_size <> maddr.byte_size THEN BEGIN
      temp_addr := maddr;
      maddr := typ_reg_addr ( get_vax_reg(targ_addr.byte_size)
	,targ_addr.byte_size);
      free ( maddr );
      store ( temp_addr , maddr , alignment ( exp ) );
    END;
    opc := typ_opc ( mnegl , targ_addr.byte_size );
    do_neg_op := targ_addr;
  END;
  (* move the results of the negation to the target or whatever *)
  gen2 ( opc , maddr , do_neg_op );
END (* proc do_neg_op *);
$PAGE do_abs_op

FUNCTION do_abs_op ( exp: expr ): addr_desc;
(* DO_ABS_OP generates code for an IABS_OP or RABS_OP expression
   tuple.  An address descriptor for the result is returned.  *)

VAR
    vtype: vax_type;
    reg: registers;
    local_label: def;

BEGIN
  vtype := expr_vax_type ( exp );
  reg := copy_load ( exp^.operand[1] ); (* load value *)
  do_abs_op := typ_reg_addr ( reg, vtype ); (* set result location *)
  gen1 ( typ_opc ( tstl, vtype ), do_abs_op ); (* test value *)
  local_label := make_def ( local_def );
  gen_branch ( bgeq, local_label ); (* if ge, skip negate instr *)
  gen2 ( typ_opc ( mnegl, vtype ), do_abs_op, do_abs_op ); (* negate value *)
  mark_def ( code_area, local_label ); (* define skip label *)
END (* proc do_abs_op *);
$PAGE do_fetch

PUBLIC FUNCTION do_fetch ( exp: expr; targ_addr: addr_desc ): addr_desc;
(* DO_FETCH is passed an expression class tuple.  It calls the necessary
   routines to generate just enough code to make the result of the
   expression addressible on the target machine.  Normally FETCH or
   FETCHSTRING should be called to generate code for an expression tuple
   since they contain the logic necessary to handle tuples with multiple
   uses.  *)

VAR
    addr: addr_desc;
    reg: registers;
    string_desc: str_desc;
    addr2: addr_desc;

CONST
    eof_eoln_offset = 34; (* offset in file block to eof/eoln bits *)
    fileb_cursor_offset = 12; (* offset in file block of cursor longword *)

BEGIN
  with exp^ DO BEGIN
    CASE opcode OF
      cst_ref:
	do_fetch := const_addr ( exp );
      ident_ref:
	do_fetch := ident_addr ( exp );
      field_ref:
	do_fetch := field_addr ( exp );
      ptr_ref: BEGIN
	do_fetch := deref_addr ( base_ptr );
	do_fetch.byte_size := expr_vax_type ( exp );
      END;
      array_ref:
	do_fetch := array_addr ( exp );
      substr_ref: (* subsripted strings appear as SUBSTR_REFs *)
      BEGIN (* with constant length 1 *)
	assert ( desc.kind = chars );
	do_fetch := array_addr ( exp );
      END;
      buffer_ref:
	do_fetch := file_deref ( exp );
      iadd_op, isub_op, imul_op, idiv_op, radd_op, rsub_op, rmul_op, rdiv_op:
	do_fetch := binary_op_addr ( exp , targ_addr );
      imod_op:
	do_fetch := do_imod_op ( exp );
      ineg_op, rneg_op:
	do_fetch := do_neg_op ( exp , targ_addr );
      iabs_op, rabs_op:
	do_fetch := do_abs_op ( exp );
      sclcvt_op:
	do_fetch := scalar_convert ( exp );
      expii_op:
	do_fetch := rt_value_call( exp, rt_exp_ii , targ_addr );
      expri_op:
	do_fetch := rt_value_call( exp, rt_exp_ri , targ_addr );
      exprr_op:
	do_fetch := rt_value_call( exp, rt_exp_rr , targ_addr );
      trunc_op, float_op:
	do_fetch := float_or_trunc_op ( exp , targ_addr );
      sqrt_op:
	do_fetch := rt_ref_call ( exp, rt_r_sqrt, targ_addr );
      ln_op:
	do_fetch := rt_ref_call ( exp, rt_r_ln, targ_addr );
      log_op:
	do_fetch := rt_ref_call ( exp, rt_r_log, targ_addr );
      exp_op:
	do_fetch := rt_ref_call ( exp, rt_r_exp, targ_addr );
      sin_op:
	do_fetch := rt_ref_call ( exp, rt_r_sin, targ_addr );
      arcsin_op:
	do_fetch := rt_ref_call ( exp, rt_r_asin, targ_addr );
      sinh_op:
	do_fetch := rt_ref_call ( exp, rt_r_sinh, targ_addr );
      cos_op:
	do_fetch := rt_ref_call ( exp, rt_r_cos, targ_addr );
      arccos_op:
	do_fetch := rt_ref_call ( exp, rt_r_acos, targ_addr );
      cosh_op:
	do_fetch := rt_ref_call ( exp, rt_r_cosh, targ_addr );
      tan_op:
	do_fetch := rt_ref_call ( exp, rt_r_tan, targ_addr );
      tanh_op:
	do_fetch := rt_ref_call ( exp, rt_r_tanh, targ_addr );
      cotan_op:
	do_fetch := rt_value_call ( exp, rt_r_ctan , targ_addr );
      addr_op:
	do_fetch := address_op ( exp );
      lwb_op, upb_op:
	do_fetch := bound_op ( exp );
      length_op: BEGIN
	string_desc := fetchstring ( operand[ 1 ], actual_length );
	free ( string_desc.base_addr );
	IF string_desc.text_valid THEN
	  free ( string_desc.text_addr );
	do_fetch := string_desc.len_addr;
      END;
      lwc_op, upc_op: BEGIN (* case conversion op - chars only!! *)
	assert ( desc.kind = chars );
	do_fetch := fetch ( operand[ 1 ] , no_preference );
	IF opcode = lwc_op THEN
	  do_fetch := case_op ( do_fetch, lower_trans )
	ELSE
	  do_fetch := case_op ( do_fetch, upper_trans );
      END;
      dim_op:
	do_fetch := do_dim_op ( exp );
      extent_op:
	do_fetch := rt_value_call ( exp, rt_extent , targ_addr );
      filesize_op:
	do_fetch := rt_value_call ( exp, rt_file_size , targ_addr );
      round_op:
	do_fetch := do_round_op ( exp , targ_addr );
      arctan_op: BEGIN
	IF upperbound (operand) = 1 THEN
	  do_fetch := rt_ref_call ( exp, rt_r_atan, targ_addr )
	ELSE
	  do_fetch := rt_ref_call ( exp, rt_r_atn2, targ_addr );
      END;
      imin_op, rmin_op:
	do_fetch := do_min_max_op ( exp, false );
      imax_op, rmax_op:
	do_fetch := do_min_max_op ( exp, true );
      index_op:
	do_fetch := do_index_op ( exp );
      search_op, verify_op:
	do_fetch := str_search_verify ( exp );
      ALC_TEMP_OP: BEGIN
	assert ( ( exp^.desc.base^.base_size mod byte_size ) = 0 );
	DO_FETCH := GET_TEMP ( INT_VALUE(EXP^.DESC.BASE^.BASE_SIZE Div BYTE_SIZE
	  ) , VAX_LONG );
      END;
      func_call_op:
	do_fetch := scl_function_call (exp , targ_addr );
      ile_op..filne_op, and_op, or_op, in_op, bnot_op, odd_op:
	do_fetch := reg_addr (load_bool (exp, false));
      new_op:
	do_fetch := do_new_op ( exp , targ_addr );
      open_op, rewrite_op, reset_op, update_op:
	do_fetch := rt_open_call ( exp );
      eof_op, eoln_op, eopage_op: BEGIN
	reg := load (operand[1]);
	addr := absolute_reference;
	addr.register := reg;
	addr.offset := eof_eoln_offset;
	do_fetch := reg_addr ( reg );
	IF opcode = eopage_op THEN
	  addr2 := int_value ( 2 )
	ELSE
	  addr2 := int_value ( ord ( opcode = eoln_op ) );
	gen4 (extzv, addr2, int_value (1), addr, do_fetch);
      END;
      cursor_op: BEGIN
	reg := load ( operand[ 1 ] );
	do_fetch := absolute_reference;
	do_fetch.register := reg;
	do_fetch.offset := fileb_cursor_offset;
      END;
      iostatus_op: BEGIN
	IF operand[ 1 ] = nil (* EXP^.NOPERS is always one! *)
	THEN
	  do_fetch := rt_value_call ( exp, rt_iostat_last , targ_addr )
	ELSE
	  do_fetch := rt_value_call ( exp, rt_iostatus , targ_addr );
      END;
      extstatus_op:
	do_fetch := rt_value_call ( exp, rt_extstatus , targ_addr );
      masked_op:
	do_fetch := rt_ref_call (exp, rt_masked, targ_addr);
      pending_op:
	do_fetch := rt_ref_call (exp, rt_pending, targ_addr);
      mathstatus_op:
	do_fetch := rt_value_call (exp, rt_stat_math, targ_addr);
      prgmstatus_op:
	do_fetch := rt_value_call (exp, rt_stat_program, targ_addr);
      spclstatus_op:
	do_fetch := rt_value_call (exp, rt_stat_special, targ_addr);
      exiostatus_op:
	do_fetch := rt_value_call (exp, rt_stat_exio, targ_addr);
      random_op: BEGIN
	IF upperbound (operand) = 0 THEN
	  do_fetch := rt_value_call ( exp, rt_rand , targ_addr )
	ELSE
	  do_fetch := rt_value_call ( exp, rt_rand_set , targ_addr );
      END;
      date_op: BEGIN
	DO_FETCH := GET_TEMP ( INT_VALUE ( 9 ) , VAX_BYTE );
	push_address ( duplicate_addr ( do_fetch ) );
	gen_rt ( 1, rt_date );
      END;
      time_op:
	do_fetch := rt_value_call ( exp, rt_time , targ_addr );
      runtime_op:
	do_fetch := rt_value_call ( exp, rt_runtime , targ_addr );
      others:
	assert ( false )
    END (* case *);
  END (* with *);
END (* proc do_fetch *);
$PAGE fetch

PUBLIC FUNCTION fetch ( exp: expr; targ_addr: addr_desc ): addr_desc;
(* FETCH is the central routine for generating code for expression class
   tuples.  FETCH is passed an expression class tuple and returns an
   address descriptor for the result of the expression.  FETCH itself
   is concerned primarily with handling tuples with multiple uses.
   DO_FETCH is called to actually generate the code to evaluate the
   expression.  Note that FETCHSTRING should be called to make string
   valued expressions addressible.  *)

VAR
    addr_ref: addr_ptr;
    reg1, reg2: registers;

BEGIN
  with exp^ DO BEGIN
  (* If the RESULT field is not NIL then we have a multiple use
     expression which has already been evaluated.  In this case
     RESULT points to an address descriptor for the expression
     result, so we simply copy and return that address descriptor. *)
    IF result <> nil THEN BEGIN
      addr_ref := ptr ( ord ( result ) );
      fetch := addr_ref^;
    END
    ELSE
      fetch := do_fetch ( exp , targ_addr );
      (* Check for tuples with multiple uses.  When a tuple has multiple
	 uses a copy of the address descriptor is made on the heap and
	 the RESULT field of the expr tuple is set to point to the copy.
	 That copy is returned for subsequent fetches of the expression.
	 In addition any registers used by the address descriptor
	 have their usage counts set to the usage count of the
	 expression tuple so they will not be freed after their
	 initial use.  *)
    IF (usage_count > 1) ANDIF (result = nil) THEN BEGIN (* first of multiple uses *)
      new ( addr_ref );
      addr_ref^ := fetch;
      update_usages ( fetch, usage_count - 1 );
      result := ptr ( ord ( addr_ref ) );
    END;
    (* Decrement the usage count for the expression.  If the usage
       count drops to zero and we have an address descriptor on the
       heap, then dispose of the address descriptor. *)
    usage_count := usage_count - 1;
    assert ( (usage_count >= 0) AND (usage_count <> maximum(usage_range)) );
    IF (usage_count = 0) ANDIF (result <> nil) (* last of multiple uses *)
    THEN
      dispose ( addr_ref );
  END (* with *);
END (* proc fetch *);
$PAGE fetch_fullword

PUBLIC FUNCTION fetch_fullword ( exp: expr ): addr_desc;
(* *)

VAR
    addr: addr_desc;
    reg: registers;

BEGIN
  addr := fetch ( exp , no_preference );
  IF addr.byte_size in [vax_byte, vax_word] THEN BEGIN
    IF is_immediate ( addr ) THEN BEGIN
      addr.byte_size := vax_long;
    END
    ELSE BEGIN
      reg := load_addr ( addr, alignment ( exp ) );
      addr := reg_addr ( reg );
    END;
  END;
  fetch_fullword := addr;
END (* proc fetch_fullword *);
$PAGE nonindexed_fetch
(* --- not used --- we're keeping it around as it might be handy sometime ---
public function nonindexed_fetch  ( exp: expr ): addr_desc  ;
(* NONINDEXED_FETCH fetches an expression tuple and returns an address
   which is guaranteed not to be indexed.  *)
var
  reg: registers;
begin
  nonindexed_fetch := fetch ( exp , no_preference );

  if  nonindexed_fetch.index <> noreg  then begin
    reg := move_address ( nonindexed_fetch );
    nonindexed_fetch := absolute_reference;
    nonindexed_fetch.register := reg;
  end;
end  (* proc nonindexed_fetch *)  ;
*)
$PAGE mem_fetch

PUBLIC FUNCTION mem_fetch ( exp: expr ): addr_desc;
(* MEM_FETCH fetches an expression tuple and returns an address
   which is guaranteed to be a memory address, i.e., not an
   immediate value or a register.  *)

VAR
    addr: addr_desc;
    value: int_type;
    bit_size: set_range;
    temp_size: unit_range;

BEGIN
(* Fetch value.  *)
  addr := fetch ( exp , no_preference );
  IF aconstp ( addr, value ) THEN
    mem_fetch := gen_cst ( value, addr.byte_size )
    (* If in a register, then move to a temp.  *)
  ELSE IF is_register ( addr ) THEN BEGIN
    temp_size := ngm ( expr_size(exp), bits_per_reg ) div byte_size;
    MEM_FETCH := GET_TEMP ( INT_VALUE ( TEMP_SIZE ) , VAX_LONG );
    mem_fetch.byte_size := addr.byte_size;
    free ( addr );
    store ( addr, mem_fetch, alignment(exp) );
  END
  (* Otherwise, leave it alone. *)
  ELSE
    mem_fetch := addr;
END (* proc mem_fetch *);
$PAGE argument

PUBLIC FUNCTION argument ( exp: expr ): addr_desc ;
(* *)

VAR
    reg: registers;

BEGIN
  argument := fetch_fullword ( exp );
  IF argument.indirect ORIF ( argument.index <> noreg ) THEN BEGIN
    reg := move_address ( argument );
    argument := absolute_reference;
    argument.register := reg;
  END;
END.
    K@5H