$TITLE VAXEXP -  expression evaluation and register manipulation
module vaxexp;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM pasifu.inc
$SYSTEM vaxcgu.inc
$SYSTEM vaxopc.inc
$SYSTEM vaxgen.inc
$SYSTEM vaxutl.inc
$SYSTEM pasmth.inc
$SYSTEM vaxstr.inc
$SYSTEM vaxset.inc
$SYSTEM vaxcll.inc
$SYSTEM vaxcmp.inc
$SYSTEM pastal.inc
$SYSTEM ptmcon.inc
$SYSTEM vaxio.inc
$PAGE global data and forward declarations

public var
  regdesc: reg_descriptor;	(* for all registers *)
  max_reg: registers;			(* highest numbered reg usable in current proc *)
  reg_use_by_block: ^reg_set_vector;


public function load (exp: expr): registers; forward;
public function copy_load ( exp: expr ): registers; forward;
public function load_addr (addr: addr_desc; align: data_alignment): registers; forward;
public function fetch (exp: expr; targ_addr: addr_desc ): addr_desc
  options special(coercions); forward;
public function fetch_fullword (exp: expr): addr_desc; forward;
public function mem_fetch (exp: expr): addr_desc; forward;
public function argument (exp: expr): addr_desc; forward;
public function is_register (maddr: addr_desc): boolean; forward;
type
    op_attrs = (commutative, mem_form, double);
    op_attr_set = set of op_attrs;
$PAGE exp_init

(* EXP_INIT performs all one-time-only initialization for this module.
   In particular, it allocates and initializes the register use vector. *)

public procedure exp_init;

var
  i: index_range;

begin
  new ( reg_use_by_block, blk_number );
  for i := 1 to blk_number do
    reg_use_by_block^[ i ] := [];
end;
$PAGE alignment

(* ALIGNMENT returns the data alignment type for an expression ("exp"). *)

public function alignment (exp: expr): data_alignment;
 begin
  case exp^.desc.kind of
    ints, bools, chars, scalars, pointers:
      if exp^.desc.signed
	then alignment := signed_value
	else alignment := unsigned_value;
    reals:
      alignment := right_aligned;		(* always occupies full words *)
    arrays, records:
      alignment := right_aligned;
    others:
      alignment := right_aligned
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
      uses_remaining := 0;
      with_reg := false;
    end;
  end;
  if (cur_block^.parm_list.first = nil) and	(* if current proc has no parameters *)
     ( (cur_block^.return_sym = nil) orif	(* and is not a function, or, *)
       (not passed_by_address(cur_block^.return_sym)) )	(* AP not used to addr fcn ret value *)
    then max_reg := ap				(* then we use AP as a general register *)
    else max_reg := ap - 1;
 end;
$PAGE decr_reg_usages
(* DECR_REG_USAGES decrements the usage count for a register and
   any associates.  *)

public procedure decr_reg_usages (reg: registers);

 begin
  if (reg > 1) and (reg <= max_reg) then
  with regdesc[reg] do begin
    uses_remaining := uses_remaining - 1;
    assert ( uses_remaining <> maximum( usage_range ) );	(* would occur if 0 decremented *)
    if associate <> noreg then begin
      with regdesc[associate] do begin
	uses_remaining := uses_remaining - 1;
	if uses_remaining <= 0  then associate := noreg;
      end;
    end;
    if uses_remaining <= 0 then associate := noreg;
  end;
 end;
$PAGE tag_reg
(* TAG REG associates two registers *)

public procedure tag_reg (reg1, reg2: registers);

begin
  regdesc[reg1].associate := reg2;
  regdesc[reg2].associate := reg1;
end;
$PAGE is_immediate

(* IS_IMMEDIATE returns true if parameter ADDR describes an
   immediate value (of either a scalar or real type).  *)

public function is_immediate ( addr: addr_desc ): boolean;

begin
  is_immediate := (addr.addr_mode = other) andif addr.immediate;
end  (* proc is_immediate *) ;

$PAGE is_disp_mode

(* IS_DISP_MODE is passed an address descriptor and returns TRUE
   if the address descriptor describes a VAX diplacement mode
   address.  Note that FALSE is returned if the address is
   indexed or indirect.  *)

public function is_disp_mode ( addr: addr_desc ): boolean;

begin
  is_disp_mode := (addr.addr_mode = other ) andif
		  (addr.index = noreg) andif
		  (addr.register <> noreg) andif
		  (not addr.immediate) andif
		  (not addr.indirect)  andif
		  (addr.reloc.kind <> register_sc);
end  (* proc is_disp_mode *) ;
$PAGE is_symbol_addr

(* IS_SYMBOL_ADDR is passed an address descriptor and returns
   TRUE if the address descriptor describes the address of a 
   static symbol.  FALSE is returned if the address is indexed.  *)

public function is_symbol_addr ( addr: addr_desc ): boolean;

begin
  is_symbol_addr := ( addr.addr_mode = other ) andif
		    ( not addr.immediate ) andif
		    ( not addr.indirect ) andif
		    ( addr.register = noreg ) andif
		    ( addr.index = noreg );
end  (* proc is_symbol_addr *) ;
$PAGE indexable

(* INDEXABLE returns true if the address passed in may be indexed
   and returns false otherwise.  *)

public function indexable ( addr: addr_desc ): boolean;

begin

  indexable := ( not is_immediate ( addr ) ) andif
	       ( addr.index = noreg ) andif
	       ( not is_register ( addr ) );

end  (* proc indexable *) ;
$PAGE regs_used
(* REGS USED returns the registers used by an address descriptor, if any. *)

public procedure regs_used (maddr: addr_desc; var primary_reg, index_reg: registers );

begin
  primary_reg := noreg;
  with maddr do begin
    index_reg := index;
    if not is_immediate ( maddr )
      then primary_reg := register;
  end;
end;
$PAGE free
(* FREE decrements the usage counts of any registers associated
 with an ADDR_DESC   (except with base registers).	*)

public procedure free (maddr: addr_desc);

var
  base_reg: registers;
  index_reg: registers;

begin
  regs_used ( maddr, base_reg, index_reg );
  decr_reg_usages ( base_reg );
  decr_reg_usages ( index_reg );
end;
$PAGE vax_type_size

(* VAX_TYPE_SIZE is passed a VAX_TYPE and returns the number
   of bits required to represent the VAX_TYPE.  *)

public function vax_type_size ( vtype: vax_type ): align_range;

type
  vax_size_array = packed array [vax_type] of align_range;

const
  vtype_size: vax_size_array = ( 8 , 16 , 32 , 64 , 32 , 64 );

begin
  vax_type_size := vtype_size[ vtype ];
end  (* proc vax_type_size *) ;
$PAGE mark_regs_used

(* MARK_REGS_USED adds a given set of registers to the list of registers
   used in the current block.  *)

public procedure mark_regs_used ( regs_used: set_of_registers );

begin
  with cur_block^ do
    reg_use_by_block^[ number] := reg_use_by_block^[ number] + (regs_used - [r0,r1,fp,sp,pc]);
end  (* proc mark_regs_used *) ;
$PAGE get_reg
(* GET REG allocates a register or register pair (depending on the value of "p");
   the register allocated is returned.	*)

public function get_reg (p: bit_range): registers;

 var   reg: registers;

 begin
  for reg := (max_reg - ord( p > bits_per_reg ) ) downto 2 do begin
    if (regdesc[reg].uses_remaining = 0) andif
       ( (p <= bits_per_reg) orif (regdesc[reg + 1].uses_remaining = 0) ) then begin
      regdesc[reg].uses_remaining := 1;
      if p > bits_per_reg then begin		(* allocating a register pair *)
        regdesc[reg + 1].uses_remaining := 1;
        tag_reg ( reg, reg + 1 );
        with cur_block^ do 
          reg_use_by_block^[ number] := reg_use_by_block^[ number] + [reg, reg + 1];
      end
      else begin
        regdesc[reg].associate := noreg;
        with cur_block^ do reg_use_by_block^[ number] := reg_use_by_block^[ number] + [reg];
      end;
      get_reg := reg;
      return;		(* <--- short-circuit return *)
    end;
  end (* for *);
  fatal_error ('Register overflow');
 end;
$PAGE get_vax_reg

(* GET_VAX_REG is a wrapper for GET_REG.  It permits the caller
   to specify the register size via a VAX_TYPE.  *)

public function get_vax_reg ( vtype: vax_type ): registers;

begin
  get_vax_reg := get_reg ( vax_type_size ( vtype ) );
end  (* proc get_vax_reg *) ;
$PAGE incr_reg_usages

(* INCR_REG_USAGES increments the USES_REMAINING field for a register
   and any associated registers.  *)

procedure incr_reg_usages ( reg: registers; additional_uses: usage_range );

begin
  if (reg > 1) and (reg <= max_reg) then begin
    with regdesc[reg] do begin
      uses_remaining := uses_remaining + additional_uses;
      if associate <> noreg then begin
        with regdesc[associate] do 
          uses_remaining := uses_remaining + additional_uses;
      end;
    end;
  end;
end  (* proc incr_reg_usages *) ;
$PAGE update_usages

(* UPDATE_USAGES increments the usage counts for all the registers
   used by a given address descriptor (parameter ADDR).  The usage
   counts are incremented by the value of parameter COUNT.  *)

public procedure update_usages ( addr: addr_desc; count: usage_range );

var
  reg1, reg2: registers;

begin
  regs_used ( addr, reg1, reg2 );
  incr_reg_usages ( reg1, count );
  incr_reg_usages ( reg2, count );
end  (* proc update_usages *) ;
$PAGE with_start
(* WITH START initializes a with-descriptor for a new with construct *)

public procedure with_start (with_tpl: tuple)  options special(coercions);
var
  addr_ref: addr_ptr;
  i: usage_range;
  reg: registers;
  maddr: addr_desc;

begin
  maddr := fetch (with_tpl, no_preference);	(* get addressibility for record *)
  free (maddr);
  if maddr.indirect or (maddr.index <> noreg) or
      (maddr.register <> noreg) and (maddr.register < fp)
				and not regdesc[maddr.register].with_reg then begin

  (* load address into a register *)

    if (maddr.register <> noreg) and (maddr.register < fp) then
      for i := 1 to with_tpl^.usage_count do
	decr_reg_usages (maddr.register);
    if index <> noreg then begin
      assert ((index < fp) and not regdesc [maddr.index].with_reg);
      for i := 1 to with_tpl^.usage_count do
	decr_reg_usages (maddr.index)
    end;
    reg := get_reg (bits_per_unit);
    regdesc[ reg ].with_reg := true;
    incr_reg_usages (reg, with_tpl^.usage_count - 1); (* get_reg gave it 1 already *)
    gen2 (movab, maddr, reg_addr (reg));
    dispose (with_tpl^.result);
    new (addr_ref);
    addr_ref^ := absolute_reference;
    addr_ref^.register := reg;
    with_tpl^.result := ptr (ord (addr_ref))
  end
end (* with_start *);  
$PAGE is_register
(* IS REGISTER returns a flag indicating whether or not an address ("maddr")
   designates a register. *)

public function is_register (* maddr: addr_desc): boolean *);
 begin
  is_register := ( maddr.addr_mode = other ) andif
		 ( not maddr.immediate ) andif
		 ( maddr.reloc.kind = register_sc ) andif
		 ( maddr.register > 1 );	(* if value in 0 or 1, copy it *)
 end;
$PAGE expr_size

(* EXPR SIZE calculates the size in bits of a value given an expression
   tuple for the value.  *)
  
public function expr_size ( exp: expr ): bit_range;

begin
  with exp^.desc do begin

    if kind in [bools,ints,chars,scalars]
      then expr_size := int_prec
    else if kind in [pointers,files]
      then expr_size := bits_per_address
    else if kind in [procs,funcs]
      then expr_size := 2 * bits_per_address
    else if ( base <> nil ) andif ( kind <> sets ) 
      then expr_size := base^.base_size
    else if kind = sets 
      then expr_size := setb_size ( exp ) * bits_per_byte
    else if kind = strings then begin
      expr_size := str_length * char_size;
      if str_kind = varying
        then expr_size := expr_size + str_lw_width;
    end
    else begin
      assert ( false );				(* what the hell is it? *)
    end;
  
  end;
end  (* proc expr_size *) ;
$PAGE unpacked_vax_type
(* UNPACKED_VAX_TYPE is passed a TYPE_NODE pointer and returns the 
   corresponding VAX type.  The type passed in is assumed not to be
   a component of a packed structure.  *)

function packed_vax_type ( tnode: typ): vax_type; forward;

public function unpacked_vax_type ( tnode: typ ): vax_type;

begin
  case tnode^.kind of

    bools,
    chars:
      unpacked_vax_type := vax_byte;

    scalars:
      unpacked_vax_type := packed_vax_type (tnode);

    ints,
    pointers,
    files:
      unpacked_vax_type := vax_long;

    reals:
      if tnode^.precision <= srealprec
	then unpacked_vax_type := vax_float
	else unpacked_vax_type := vax_double;

    procs,
    funcs:
      unpacked_vax_type := vax_quad;

    sets, (* sets, strings, records, and arrays are *)
    strings,					(* all set to vax_byte *)
    records,					(* note that CMPC and MOVC expect byte *)
    arrays:					(* type addresses *)
      unpacked_vax_type := vax_byte;

    others:
      assert ( false )			(* what is it ??? *)

  end  (* case *) ;
end  (* proc unpacked_vax_type *) ;
$PAGE packed_vax_type

(* PACKED_VAX_TYPE is passed a type node and returns the corresponding
   VAX_TYPE.  The type passed in is assumed to be a component of a
   packed structure.  *)

function packed_vax_type (* ( tnode: typ ): vax_type *);

begin
  with tnode^ do begin
    if kind in [scalars, ints] then begin
      if base_size <= bits_per_byte
	then packed_vax_type := vax_byte
      else if base_size <= 2 * bits_per_byte
        then packed_vax_type := vax_word
      else packed_vax_type := vax_long;
    end
    else packed_vax_type := unpacked_vax_type ( tnode );
  end  (* with *) ;
end  (* proc packed_vax_type *) ;
$PAGE expr_vax_type

(* EXPR_VAX_TYPE is passed an expression tuple and returns the
   corresponding VAX_TYPE.  Note that the expression value is
   assumed to be unpacked.  *)

public function expr_vax_type ( exp: expr ): vax_type;

begin
  with exp^.desc do begin
    if (kind = strings) or (kind = sets)
      then expr_vax_type := vax_byte
    else if kind = reals then begin	(* precision in expr_type_node overrides *)
      if precision <= srealprec	(* that in type node *)
        then expr_vax_type := vax_float
	else expr_vax_type := vax_double
    end
    else if kind = scalars
      then expr_vax_type := unpacked_vax_type ( base^.base_type )
    else expr_vax_type := unpacked_vax_type ( base );
  end;
end  (* proc expr_vax_type *) ;
$PAGE rt_value_call

(* RT_VALUE_CALL generates a call to a runtime routine whose parameters
   are ALL passed by value.  An address descriptor is returned describing
   the location of the runtime routine's result.  Parameter EXP is the
   operator tuple corresponding to the runtime routine and parameter
  RT_SYM is the ssingle precision runtime routine's enumeration symbol.
   The corresponding double precision routine's symbol is assumed to
   be the successor of the single precision routine's symbol.  Finally,
   the runtime routine is assumed to be a function returning a value in
   R0 or R0 and R1.  *)

function rt_value_call ( exp: expr; rt_sym: rt_symbol; targ_addr: addr_desc ): addr_desc;

type param_ary = array [ 1..* ] of addr_desc;

const
  arg_bits := 32;	(* bits per argument word *)

var
  temp_addr: addr_desc;
  arg_count: 0..255;
  i: 0..255;
  params : ^param_ary;
  result_vtype: vax_type;
  reg: registers;
  vtype: vax_type;

begin
  with exp^ do begin
    
    (* Fetch all the operands before pushing any. this is to accomodate
       the user who has any dynamic temporaries in the parameter list. *)

    arg_count := 0;
    new ( params, upperbound (operand) );
    for i := upperbound (operand) downto 1 do begin
      if operand[ i ] <> nil then begin
	(* if there is only one parameter, try simple targetting to SP *)
	if upperbound (operand) = 1
	  then begin
	    temp_addr := push_reference;
	    temp_addr.byte_size := expr_vax_type ( operand [ 1 ] );
	    if temp_addr.byte_size in [ vax_byte , vax_word ]
	      then temp_addr.byte_size := vax_long;
	    params^[1] := fetch ( operand [ 1 ] , temp_addr );
	    if not adr_equal ( params^[1] , temp_addr )
	      then push_value ( params^[1] , alignment(operand [ 1 ] ) )
	  end
	else params^[i] := fetch ( operand[ i ] , no_preference );
	if params^[ i ].byte_size in [vax_byte, vax_word, vax_long]
	  then vtype := vax_long
	  else vtype := params^[ i ].byte_size;
	arg_count := arg_count + (vax_type_size(vtype) div arg_bits);
      end;
    end;

    (* Now push the parameters, since any dynamic temporarys will have been
       created and are already upon the stack. *)

    if upperbound (operand) <> 1 then
      for i := upperbound (operand) downto 1 do
	push_value ( params^[ i ] , alignment ( operand [ i ] ) );
    dispose ( params );		(* Free that storage *)

    (* Generate the call *)

    result_vtype := expr_vax_type ( exp );
    if result_vtype = vax_double
      then gen_rt ( arg_count, succ ( rt_sym ) )
      else gen_rt ( arg_count, rt_sym );

    (* Move the result (in R0 or R0 and R1) to a non-volatile register. *)

    if not adr_equal(targ_addr,no_preference) and (targ_addr.byte_size=result_vtype)
      then begin
	temp_addr := r0_addr;
	temp_addr.byte_size := result_vtype;
	store ( temp_addr , targ_addr , alignment ( exp ) );
	rt_value_call := targ_addr
      end
    else begin
      reg := load_addr ( typ_reg_addr(R0, result_vtype), alignment(exp) );
      rt_value_call := typ_reg_addr ( reg, result_vtype );
    end;

  end  (* with *) ;
end  (* proc rt_value_call *) ;
$PAGE rt_ref_call

(* RT_REF_CALL generates a call to a runtime routine whose parameters
   are ALL passed by address.  An address descriptor is returned
   describing the location of the runtime routine's result.  Parameter
   EXP is the operator tuple corresponding to the runtime routine and
   parameter RT_SYM is the single precision variation of the runtime
   routine.  The enumeration symbol for the corresponding double precision
   routine is assumed to be the successor of the single precision 
   symbol.  The routine is assumed to be a fun returning a value in
   R0 or R0 and R1.  It is also assumed that the runtime routine has at
   most 2 parameters.  Finally, it is assumed that the runtime routine
   ignores its argument count (the argument count may be increased
   to include temporaries which will then be automatically popped from
   the stack upon return).  *)

function rt_ref_call ( exp: expr;  rt_sym: rt_symbol; targ_addr: addr_desc ): addr_desc;

const
  arg_bits := 32;	(* bits per argument word *)

 var
  arg_count: 0..255;
  temp_addr: addr_desc;
  op1: addr_desc;
  op2: addr_desc;
  vtype1: vax_type;
  vtype2: vax_type;
  result_vtype: vax_type;
  reg: registers;

begin
  with exp^ do begin
    arg_count := upperbound (operand);	(* count of argument longwords, *)
						(* initially set to number of parameters *)
    
    (* Only 1 parameter - if its in a reg then push it onto the stack.
       Push the parameter address onto the stack.  *)

    if upperbound (operand) = 1 then begin
      op1 := fetch_fullword ( operand[ 1 ] );
      vtype1 := op1.byte_size;
      if is_register ( op1 ) or is_immediate ( op1 ) then begin
	push_value ( op1, alignment ( operand[ 1 ] ) );
        push_value ( sp_addr, unsigned_value );
	arg_count := arg_count + vax_type_size ( vtype1 ) div arg_bits;
      end
      else push_address ( op1 );
    end

    (* Two parameters - if either parameter is in a register then
       push it onto the stack.  The address of the parameters are
       then pushed onto the stack in reverse order.  *)

    else if upperbound (operand) = 2 then begin
      op1 := fetch_fullword ( operand[ 1 ] );
      op2 := fetch_fullword ( operand[ 2 ] );
      vtype1 := op1.byte_size;
      vtype2 := op2.byte_size;

      if is_register ( op1 ) or is_immediate ( op1 ) then begin	(* first param is in a reg *)
	push_value ( op1, alignment(operand[ 1 ]) );	(* push 1st param, update its *)
	op1 := stack_top_reference;	(* address and increment arg *)
	op1.byte_size := vtype1;	(* word count *)
	arg_count := arg_count + (vax_type_size(vtype1) div arg_bits);

	if is_register ( op2 ) or is_immediate ( op2 ) then begin	(* both params are in regs *)
	  push_value (op2, alignment(operand[2]) );	(* push 2nd param, *)
	  op2 := stack_top_reference;	(* update both addresses and *)
	  op2.byte_size := vtype2;	(* argument word count *)
	  op1.offset := (vax_type_size(vtype2) + bits_per_address) div bits_per_byte;
	  arg_count := arg_count + (vax_type_size(vtype2) div arg_bits);
	end
	else op1.offset := bits_per_address div bits_per_byte;

      end
      else if is_register( op2 ) or is_immediate ( op2 ) then begin	(* Only 2nd param in reg *)
	push_value (op2, alignment(operand[ 2 ]) );
	op2 := stack_top_reference;
	op2.byte_size := vtype2;
	arg_count := arg_count + vax_type_size ( vtype2 ) div arg_bits;
      end;

      push_address ( op2 );
      push_address ( op1 );
    end	(* end 2 operand case *)
    else assert ( false );	(* not prepared for > 2 operands !!! *)

    (* Generate the call to the routine. *)

    result_vtype := expr_vax_type ( exp );
    if result_vtype = vax_double
      then gen_rt ( arg_count, succ ( rt_sym ) )
      else gen_rt ( arg_count, rt_sym );

    (* Move the result from R0 or R0 and R1 to a non-volatile register
       or the prefered target location, if possible. *)

    if not adr_equal(targ_addr,no_preference) and (targ_addr.byte_size=result_vtype)
      then begin
	temp_addr := r0_addr;
	temp_addr.byte_size := result_vtype;
	store ( temp_addr , targ_addr , alignment ( exp ) );
	rt_ref_call := targ_addr
      end
    else begin
      reg := load_addr ( typ_reg_addr(R0, result_vtype), alignment(exp) );
      rt_ref_call := typ_reg_addr ( reg, result_vtype );
    end;

  end  (* with *) ;
end  (* proc rt_ref_call *) ;
$PAGE float_or_trunc_op

(* FLOAT_OR_TRUNC_OP generates code for FLOAT_OP and TRUNC_OP
   expression tuples (parameter EXP).  An address descriptor
   for the result is returned.  *)

function float_or_trunc_op ( exp: expr; targ_addr: addr_desc ): addr_desc;

type opc_array = packed array [ vax_type ] of opc_range;

const cvt_opcodes : opc_array = (cvtlb, cvtlw, cvtbl, 0, cvtlf, cvtld);

var
  result_vtype: vax_type;
  operand_addr: addr_desc;
  opc: opc_range;
  reg: registers;

begin
  with exp^ do begin

    (* Determine the VAX_TYPE of the operand and of the result.
       Fetch address of operand.  *)

    if adr_equal ( targ_addr , no_preference )
      then result_vtype := expr_vax_type ( exp )
    else result_vtype := targ_addr.byte_size;

    operand_addr := fetch ( operand[ 1 ] , no_preference );

    (* Get correct opc.  Float_ops are generated whenever two real
       values have different precisions - it may not be necessary to
       generate code.  *)

    if result_vtype = operand_addr.byte_size
      then float_or_trunc_op := operand_addr	(* No conversion ness. *)
    else begin
      assert ( cvt_opcodes[result_vtype] <> 0 );	(* QUAD WORDS ??? *)
      opc := typ_opc ( cvt_opcodes [ result_vtype ] , operand_addr.byte_size );

      free ( operand_addr );		(* Make it available again *)

      (* If it is possible to fulfill the target request, do so. If not,
	 then get a register of the appropriate size and move the result there. *)

      if adr_equal ( targ_addr , no_preference )
	then float_or_trunc_op := typ_reg_addr( get_vax_reg(result_vtype), result_vtype )
      else float_or_trunc_op := targ_addr;

      (* Now move the ressult into the target area. *)

      gen2 ( opc , operand_addr , float_or_trunc_op )

    end  (* else *) ;
  end  (* with *) ;
end  (* proc float_or_trunc_op *) ;
$PAGE do_round_op

(* DO_ROUND_OP generates the code for a ROUND_OP expression tuple.
   An address descriptor for the result is returned. *)

function do_round_op ( exp: expr; targ_addr: addr_desc ): addr_desc;

var
  reg: registers;
  operand_vtype: vax_type;
  addr: addr_desc;

begin
  with exp^ do begin
    operand_vtype := expr_vax_type ( operand[ 1 ] );
    if upperbound (operand) = 1 then begin	(* real to integer round *)
      addr := fetch ( operand[ 1 ] , no_preference );
      free ( addr );
      reg := get_vax_reg ( vax_long );
      gen_mr ( typ_opc(cvtrfl, operand_vtype), addr, reg );
      do_round_op := reg_addr ( reg );	(* always return longword result *)
    end
    else do_round_op := rt_value_call(exp,rt_r_rnd2,targ_addr); (* real to real *)
  end  (* with *) ;
end  (* proc do_round_op *) ;
$PAGE do_min_max_op
(* DO MIN MAX OP compiles code for integer or real, min or max operators, given
   the operator node ("exp") for the operator.  Parameter MAX_OP is TRUE 
   if the operator is IMAX_OP or RMAX_OP and FALSE for a IMIN_OP or
   RMIN_OP.  *)

function do_min_max_op (exp: expr; max_op: boolean): addr_desc;

 var
   reg: registers;
   mem: addr_desc;
   i: index_range;
   vtype: vax_type;
   local_label: def;

 begin
  with exp^ do begin

    (* Load the first operand into a destroyable register.  As we compare this value
       against each successive operand, if the value of the operand is found to be
       less (greater) than the first, the register is loaded with the value of the
       operand compared against. *)

    reg := copy_load ( operand[ 1 ] );
    vtype := expr_vax_type ( exp );
    do_min_max_op := typ_reg_addr ( reg, vtype );

    (* Process each operand and compare with the min/max value to date. *)

    for i := 2 to upperbound (operand) do begin
      mem := fetch_fullword ( operand[ i ] );
      gen2( typ_opc(cmpl,vtype), mem, do_min_max_op );
      local_label := make_def ( local_def );
      if max_op
	then gen_branch ( bleq, local_label )	(* maximum *)
	else gen_branch ( bgeq, local_label );	(* minimum *)
      store ( mem, do_min_max_op, signed_value );
      free ( mem );
      mark_def ( code_area, local_label );	(* define local_label *)
    end  (* for *) ;

  end  (* with *) ;
end  (* proc do_min_max_op *) ;
$PAGE iconstp
(* ICONSTP is a predicate which indicates if its argument has a constant value.
   It is assumed that the argument has an ordinal type.  If the argument does
   have a constant value, its value is returned. *)

public function iconstp (exp: expr; var val: int_type): boolean;
 var texp: expr;
 begin
  if exp^.opcode = sclcvt_op
    then texp := exp^.operand[1]
    else texp := exp;
  if (texp^.opcode = cst_ref) andif (texp^.cst_val.kind = scalar_cst) then begin
    iconstp := true;
    val := texp^.cst_val.ival;
  end
  else iconstp := false;
 end;
$PAGE aconstp
(* ACONSTP serves the same function as ICONSTP, except that it accepts
   as a parameter an ADDR DESC insead of an EXPR.	*)

public function aconstp (maddr: addr_desc; var val: int_type): boolean;

begin
  aconstp := is_immediate ( maddr ) andif
	     not ( maddr.byte_size in [vax_float, vax_double] );
  if aconstp then val := maddr.offset;
end;
$PAGE duplicate_addr

(* DUPLICATE_ADDR increases the usage counts by one on all registers
   used by parameter ADDR.  It then returns ADDR as its function value. *)

public function duplicate_addr ( addr: addr_desc ): addr_desc;

var
  base_reg: registers;
  index_reg: registers;

begin
  regs_used ( addr, base_reg, index_reg );
  incr_reg_usages ( base_reg, 1 );
  incr_reg_usages ( index_reg, 1 );
  duplicate_addr := addr;
end  (* proc duplicate_addr *) ;
$PAGE load_addr

(* LOAD_ADDR loads an object into a register, given its address
   (parameter ADDR) and its required alignment (parameter
   ALIGNMENT).  The register loaded is returned as the function
   return value.
   Note that:
     1. a longword value already in a register will not be copied,
     2. parameter ADDR is freed by this routine.
     3. byte and word values will be converted to longwords 
        during the move.  *)

public function load_addr (* ( addr: addr_desc; align: data_alignment ):registers *) ;

var
  vtype: vax_type;
  dest_addr: addr_desc;

begin
  if is_register ( addr ) and not ( addr.byte_size in [vax_byte, vax_word] ) then begin
    load_addr := addr.register
  end 
  else begin
    free ( addr );
    if addr.byte_size in [vax_byte,vax_word]
      then vtype := vax_long
      else vtype := addr.byte_size;
    load_addr := get_vax_reg ( vtype );
    dest_addr := typ_reg_addr ( load_addr, vtype );
    store ( addr, dest_addr, align );
  end;
end  (* proc load_addr *) ;
$PAGE load

function load (* ( exp: expr ): registers *) ;

var
  addr: addr_desc;

begin
  addr := fetch ( exp , no_preference );
  if is_register ( addr ) and not ( addr.byte_size in [vax_byte, vax_word] )
    then load := addr.register
    else load := load_addr ( addr, alignment ( exp ) );
end  (* proc load *) ;
$PAGE copy_load

(* COPY_LOAD loads an expression tuple value into a register.
   If the value is already in a register, it will be copied to a
   new reg if the register has more than one remaining use or if
   it is a WITH register.  Note that byte and word values are
   converted to longword values when loaded.  *)

public function copy_load (* ( exp: expr ): registers  *) ;

var
  addr: addr_desc;
  primary_reg: registers;
  index_reg: registers;
  must_load: boolean;
  dest_addr: addr_desc;

begin
  addr := fetch ( exp , no_preference );	(* fetch address of value *)
  regs_used ( addr, primary_reg, index_reg );
  must_load := not is_register ( addr ) orif	(* must load in value not in reg, or, *)
	       (addr.byte_size in [vax_byte, vax_word]) orif	(* byte or word value, or, *)
	       (regdesc[primary_reg].uses_remaining > 1);	(* if uses remain after load *)

  if must_load then begin
    free ( addr );
    copy_load := get_vax_reg ( expr_vax_type ( exp ) );
    dest_addr := typ_reg_addr ( copy_load, expr_vax_type ( exp ) );
    store ( addr, dest_addr, alignment ( exp ) );
  end
  else copy_load := addr.register;	(* no load necessary *)
end  (* proc copy_load *) ;
$PAGE move_address

(* MOVE_ADDRESS loads an address into a register.  ADDR is an address
   descriptor representing the address; the function return value
   is the number of the register containing the address.  Parameter
   ADDR is freed by this routine.  *)

public function move_address ( addr: addr_desc ): registers;

var
  opcode: opc_range;

begin
  assert ( not (is_immediate(addr) or is_register(addr)) );
  free ( addr );
  move_address := get_reg ( bits_per_address );
  opcode := typ_opc ( moval, addr.byte_size );
  gen_mr ( opcode, addr, move_address );
end  (* proc move_address *) ;
$PAGE cvt_word

(* CVT_WORD converts a value to VAX_TYPE VAX_WORD.  Parameter ADDR
   is the address of the value.  Parameter ALIGN is the alignment of
   the value.  The function return value is an address descriptor
   for the converted value.  *)

public function cvt_word ( addr: addr_desc; align: data_alignment ): addr_desc;

var
  addr_value: int_type;

begin
  if addr.byte_size = vax_word then begin	(* already a word, just return ADDR *)
    cvt_word := addr;
  end
  else if ( aconstp ( addr, addr_value ) ) or	(* if constant, or, *)
          ( (addr.byte_size = vax_long) and	(* unsigned longword, and, *)
	    (addr.index = noreg) and		(* not indexed, then *)
	    ( align = unsigned_value ) ) then begin	(* just change BYTE_SIZE *)
    cvt_word := addr;	(* field of ADDR *)
    cvt_word.byte_size := vax_word;
  end
  else begin	(* convert by moving to a register *)
    free ( addr );
    cvt_word := typ_reg_addr ( get_vax_reg ( vax_word ), vax_word );
    store ( addr, cvt_word, align );
  end;
end  (* proc cvt_word *) ;
$PAGE cvt_long

(* CVT_LONG is passed the address and alignment of an object whose
   length is assumed to be either 1, 2 or 4 bytes.  If the BYTE_SIZE
   field of the address descriptor is VAX_BYTE or VAX_WORD, then the
   object is converted to longword length.  Parameter ADDR is 'used'
   by this routine and thus is invalid upon return unless it was
   'duplicated' via a call to DUPLICATE_ADDR.  *)

public function cvt_long ( addr: addr_desc; alignment: data_alignment ): addr_desc;

var
  reg: registers;

begin
  if addr.byte_size <> vax_long then begin
    assert ( addr.byte_size in [vax_byte, vax_word] );
    if is_immediate ( addr ) then begin		(* if an immediate value, then *)
      cvt_long := addr;				(* simply change BYTE_SIZE *)
      cvt_long.byte_size := vax_long;
    end
    else begin					(* otherwise, load it into a reg *)
      free ( addr );
      cvt_long := reg_addr ( get_reg ( bits_per_reg ) );
      store ( addr, cvt_long, alignment );
    end;
  end
  else cvt_long := addr;	(* already  a longword *)
end  (* proc cvt_long *) ;
$PAGE cvt_quad

(* CVT_QUAD converts a longword value to a quadword value.  Parameter
   LONG_ADDR is freed by this routine.  *)

function cvt_quad ( long_addr: addr_desc ): addr_desc;

var
  is_reg: boolean;
  reg: registers;
  result_reg: registers;
  count_addr: addr_desc;

begin
  assert ( long_addr.byte_size = vax_long );	(* we really do want a longword *)
  if is_immediate ( long_addr ) then begin	(* if immediate value, just *)
    cvt_quad := long_addr;	(* change BYTE_SIZE field *)
    cvt_quad.byte_size := vax_quad;
  end
  else begin	(* move (if necessary) and sign extend value *)
    is_reg := is_register ( long_addr );
    if is_reg then reg := long_addr.register;
    free ( long_addr );
    result_reg := get_vax_reg ( vax_quad );	(* get a reg pair *)
    cvt_quad := typ_reg_addr ( result_reg, vax_quad );

    (* The conversion may be done in any of 3 ways, in each case
       however, an arithemetic shift right of 32 bits is used.  *)

    count_addr := typ_int_value ( -32, vax_byte );
    if is_reg andif ( result_reg = reg ) 	(* lower result reg is orginal reg *)
      then gen3 ( ashl, count_addr, reg_addr(reg), reg_addr(reg + 1) )
    else if is_reg andif (result_reg = reg - 1) 	(* upper result reg is original reg *)
      then gen3 ( ashq, count_addr, cvt_quad, cvt_quad )
    else begin	(* must move original value to reg pair *)
      gen_mr ( movl, long_addr, result_reg );
      gen3 ( ashl, count_addr, reg_addr(result_reg), reg_addr(result_reg+1) );
    end;

  end  (* else *) ;
end  (* proc cvt_quad *) ;
$PAGE scalar_convert

(* SCALAR_CONVERT generates code, if necessary, for SCLCVT_OP expression
   tuples.  Parameter EXP is the SCLCVT_OP tuple; an address
   descriptor for the coverted value is returned as the
   function return value.  *)

function scalar_convert ( exp: expr ): addr_desc;

var
  dest_type: type_kind;
  source_alignment: data_alignment;
  dest_alignment: data_alignment;
  dest_addr: addr_desc;
  dest_vtype: vax_type;

begin
  (* Fetch the value to be converted; determine the alignment of both
     the orginal and the converted values.  *)

  dest_type := exp^.desc.kind;
  scalar_convert := fetch ( exp^.operand[ 1 ] , no_preference );
  source_alignment := alignment ( exp^.operand[ 1 ] );
  dest_alignment := alignment ( exp );

  (* For conversions to integer types, we insure that
     the value conforms to the alignment specified in the result's
     expression type descriptor record.  *)

  if dest_type = ints then begin
    if ( source_alignment in [unsigned_value, right_aligned] ) andif
       ( dest_alignment = signed_value ) and
       ( scalar_convert.byte_size <> vax_long ) then begin
      free ( scalar_convert );
      dest_addr := reg_addr ( get_reg ( bits_per_integer ) );
      store ( scalar_convert, dest_addr, source_alignment );
      scalar_convert := dest_addr;
    end;
  end

  (* If the result is a boolean, char or enumerated type then coerce the
     value to the proper size.  *)

  else if dest_type in [bools, chars, scalars] then begin
    dest_vtype := expr_vax_type ( exp );
    if scalar_convert.byte_size <> dest_vtype then begin
      if (dest_vtype < scalar_convert.byte_size) andif
         (source_alignment = unsigned_value) andif
         (scalar_convert.index = noreg) then begin
        scalar_convert.byte_size := dest_vtype;
      end
      else begin
	free ( scalar_convert );
	dest_addr := typ_reg_addr (get_vax_reg(dest_vtype), dest_vtype);
	store ( scalar_convert, dest_addr, unsigned_value );
	scalar_convert := dest_addr;
      end;
    end;
  end

  (* if the result is a file or pointer, then simply convert the source
     to a longword value.  *)

  else if dest_type in [pointers, files] then begin
    if scalar_convert.byte_size <> vax_long then begin
      free ( scalar_convert );
      dest_addr := reg_addr ( get_reg ( bits_per_address ) );
      store ( scalar_convert, dest_addr, source_alignment );
      scalar_convert := dest_addr;
    end;
  end

  else begin
    assert ( false );	(* what is it?!? *)
  end;

end  (* proc scalar_convert *) ;
$PAGE load_frame_ptr

(* LOAD_FRAME_PTR chases the static chain for non-local references.
   Parameter ID_SYM is the symbol node pointer for the 
   non_local symbol.  The function return
   value is the number of the register containing the appropriate
   frame pointer (or for parameters, the argument block pointer).  *)

function load_frame_ptr ( id_sym: sym ): registers;

const
  parent_addr: addr_desc := (vax_long,other,false,false,noreg,fp,-4,(absolute_sc));
  chain_addr: addr_desc := (vax_long,auto_dec,false,false,noreg,noreg,0,(absolute_sc));

var
  reg: registers;
  level_difference: level_index;
  chase_addr: addr_desc;
  is_parameter: boolean;
  i: level_index;
  value_func_sym: boolean;

begin
  with id_sym^ do begin
    is_parameter := id_sym^.dcl_class = parameter_sc;	(* TRUE if parameter or fcn return value *)
    value_func_sym := (id_sym = id_sym^.block^.return_sym) andif	(* TRUE if fcn return value  *)
		      ( not passed_by_address ( id_sym ) );	(* and stored as local variable *)
    if is_parameter and not value_func_sym then reg := ap else reg := fp;
    if block <> cur_block then begin		(* non-local reference, must chase static chain *)
      level_difference := cur_block^.apparent_level - block^.apparent_level;
      if level_difference <> 0 then begin
	reg := get_reg ( bits_per_address );
	gen_mr ( movl, parent_addr, reg );
	chase_addr := chain_addr;
	chase_addr.register := reg;
	for i := 2 to level_difference do
	  gen_mr ( movl, chase_addr, reg );
  	if is_parameter and not value_func_sym then begin	(* if parameter, load the saved *)
	  chase_addr := absolute_reference;	(* argument pointer *)
	  chase_addr.register := reg;
	  chase_addr.offset := stk_ap_offset;	
	  gen_mr ( movl, chase_addr, reg );
	end;
      end;
    end;
    load_frame_ptr := reg;
  end  (* with *) ;
end  (* proc load_frame_ptr *) ;
$PAGE param_addr
(* PARAM_ADDR is passed a symbol node of kind VARS or VALUES and
   whose DCL_CLASS field has the value PARAMETER_SC.  An address
   descriptor record addressing the parameter is returned. *)

function param_addr ( id_sym: sym ): addr_desc;

var
  reg: registers;

begin
  reg := load_frame_ptr ( id_sym );

  param_addr := parm_reference;
  with param_addr do begin
    reloc.relsym := id_sym;
    register := reg;
    if id_sym^.type_desc^.packable
      then byte_size := packed_vax_type ( id_sym^.type_desc )
      else byte_size := unpacked_vax_type ( id_sym^.type_desc );
  end;

  param_addr.indirect := passed_by_address ( id_sym );

end  (* proc param_addr *);
$PAGE local_addr
(* LOCAL_ADDR is passed a symbol node of kind CONSTS, VARS or VALUES
   and whose DCL_CLASS field has the value LOCAL_SC.  An address
   descriptor record addressing the local variable is returned. *)

function local_addr ( id_sym: sym ): addr_desc;

var
  reg: registers;

begin
  reg := load_frame_ptr ( id_sym );

  local_addr := null_location;			(* construct addr record to return *)
  with local_addr do begin
    register := reg;
    reloc.kind := local_sc;
    reloc.relsym := id_sym;
    if id_sym^.type_desc^.packable
      then byte_size := packed_vax_type ( id_sym^.type_desc )
      else byte_size := unpacked_vax_type ( id_sym^.type_desc );
  end;

end  (* proc local_addr *) ;
$PAGE static_addr

(* STATIC_ADDR is passed a symbol node of kind CONSTS, VARS or VALUES
   and whose storage class is STATIC_SC.  It returns an address
   descriptor record which addresses the symbols storage location. *)

function static_addr ( id_sym: sym ): addr_desc;

begin
  static_addr := null_location;
  with static_addr do begin
    reloc.kind := static_sc;
    reloc.relsym := id_sym;
    if id_sym^.kind = conditions then
      byte_size := vax_long
    else if id_sym^.type_desc^.packable then
      byte_size := packed_vax_type ( id_sym^.type_desc )
    else
      byte_size := unpacked_vax_type ( id_sym^.type_desc );
  end;
end  (* proc static_addr *);
$PAGE external_addr

(* EXTERNAL_ADDR is passed a symbol node of kind CONSTS, VARS or VALUES
   and whose storage class is EXTERNAL_SC.  It returns an address
   descriptor record which addresses the symbol's storage location. *)

function external_addr ( id_sym: sym ): addr_desc;

begin
  external_addr := null_location;
  with external_addr do begin
    reloc.kind := external_sc;
    reloc.relsym := id_sym;
    if id_sym^.kind = conditions then
      byte_size := vax_long
    else if id_sym^.type_desc^.packable then
      byte_size := packed_vax_type ( id_sym^.type_desc )
    else
      byte_size := unpacked_vax_type ( id_sym^.type_desc );

    (* If overlay option is in effect, then we must reference all
       externals indirectly, i.e. through transfer vectors. *)

    if ( prog_options.overlay_opt and ( id_sym^.kind <> conditions ) ) andif
          ( ( id_sym^.kind <> consts ) orif
	   not ( id_sym^.type_desc^.kind in [procs, funcs] ) )
      then indirect := true

  end;
end;
$PAGE ident_addr

function ident_addr ( exp: expr ): addr_desc;

begin
  with exp^ do begin
    assert ( id_sym^.kind in [consts,vars,values,conditions] );	(* must hold to access storage class field *)
    if id_sym^.kind = conditions then
      assert (id_sym^.dcl_class in [static_sc, external_sc]);
    
    case id_sym^.dcl_class of			
      
      local_sc:
        ident_addr := local_addr ( id_sym );

      parameter_sc:
        ident_addr := param_addr ( id_sym );

      static_sc:
        ident_addr := static_addr ( id_sym );

      external_sc:
        ident_addr := external_addr ( id_sym );

      constant_sc:
	begin
	  ident_addr := null_location;
	  ident_addr.reloc := gen_cval ( id_sym^.init_value, id_sym^.type_desc );
	  if id_sym^.type_desc^.packable
	    then ident_addr.byte_size := packed_vax_type ( id_sym^.type_desc )
	    else ident_addr.byte_size := unpacked_vax_type ( id_sym^.type_desc );
	end;

      others:
        assert ( false )			(* Cannot happen !?! *)

    end  (* case *) ;
  end  (* with *) ;
end  (* proc ident_addr *) ;
$PAGE binary_addr

(* BINARY_ADDR generates code for a binary operation.  
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


function binary_addr ( opcode: opc_range; op1: addr_desc; 
	op2: addr_desc; attributes: op_attr_set; targ_addr:addr_desc): addr_desc;

var
  op1_addr, op2_addr, temp_addr: addr_desc;
  commute: boolean;
  both_in_regs: boolean;
  op2_reg_greater: boolean;
  result_reg: addr_desc;
  vtype: vax_type;
  opc: opc_range;

begin
  op1_addr := op1;  (* local copies so we can swap them *)
  op2_addr := op2;
  assert ( op1_addr.byte_size = op2_addr.byte_size );

  (* Commute operands if permitted and useful. *)

  both_in_regs := is_register ( op1_addr ) and is_register ( op2_addr );
  if both_in_regs
    then op2_reg_greater := op2_addr.register > op1_addr.register;

  commute := ( commutative in attributes ) andif
	     ( ( not is_register(op1_addr) and is_register(op2_addr) ) orif
	       ( both_in_regs andif 	(* try to use higher numbered *)
		 (op2_reg_greater or (op1_addr.register > max_reg)) ) );
							(* reg for result *)

  if commute then begin
    temp_addr := op1_addr;
    op1_addr := op2_addr;
    op2_addr := temp_addr;
  end;

  (* Allocate the result register. *)

  vtype := op1_addr.byte_size;	(* assume result type is same as first operand *)
  if is_register ( op1_addr ) andif	(* If the second operand OF THE *)
     ( regdesc[ op1_addr.register ].uses_remaining = 1 ) then begin
    free ( op1_addr );	(* INSTRUCTION is in a register, then *)
			(* free it 1st so we can use 2 opernad instruction *)
    if adr_equal(targ_addr,no_preference) or ( targ_addr.byte_size <> vtype )
      then binary_addr := typ_reg_addr( get_vax_reg(vtype) , vtype )
    else binary_addr := targ_addr;
    free ( op2_addr );
  end
  else begin
    free ( op1_addr );
    free ( op2_addr );
    if adr_equal(targ_addr,no_preference) or ( targ_addr.byte_size <> vtype )
      then binary_addr := typ_reg_addr ( get_vax_reg(vtype) , vtype )
    else binary_addr := targ_addr;
  end;

  opc := typ_opc ( opcode, vtype );	(* get correct opcode variation *)

  (* generate the instruction *)

  if is_register(op1_addr) andif is_register(binary_addr) andif (op1_addr.register=binary_addr.register)
    then gen2 ( opc, op2_addr, op1_addr )
    else gen3 ( opc + 1, op2_addr, op1_addr, binary_addr );
  
end  (* proc binary_addr *) ;
$PAGE increment_addr

(* INCREMENT_ADDR is given a base address (parameter BASE_ADDR)
   and a byte offset from the base (parameter BYTE_OFFSET).
   It returns the address of the byte at the given offset.
   The BYTE_SIZE field of the address descriptor returned
   must be set by the caller - this routine does not initialize
   it.  Parameter BASE_ADDR is 'used' by this routine and thus
   is no longer valid upon return unless it was previously
   'duplicated' via a call to DUPLICATE_ADDR.  *)

public function increment_addr ( base_addr: addr_desc; 
			  byte_offset: unit_range ): addr_desc;

var
  reg: registers;

begin
  if byte_offset <> 0 then begin
    assert ( not is_register ( base_addr ) );
    assert ( not is_immediate ( base_addr ) );
  end;
 
  increment_addr := base_addr;
  with increment_addr do begin

    (* Three cases are distinguished.  If the VAX addressing mode of
       BASE_ADDR is displacement then BYTE_OFFSET is simply added
       to the offset field.  If the address is of a static symbol,
       then the offset is added to the OFFSET field, producing an
       address of the form 'SYMBOL+BYTE_OFFSET'.
       In all other cases, the address is
       simply moved to a register and the address returned is
       'BYTE_OFFSET(REG)'.  *)

    if is_disp_mode ( increment_addr ) then begin	(* Case 1 *)
      offset := offset + byte_offset;
    end
    else if is_symbol_addr ( increment_addr ) then begin	(* Case 2 *)
      offset := offset + byte_offset;
    end
    else if (byte_offset <> 0) orif (base_addr.index <> noreg) then begin
      reg := move_address ( increment_addr );	(* move base addr to reg *)
      increment_addr := absolute_reference;
      register := reg;
      offset := byte_offset;
    end;

  end  (* with *) ;
end  (* proc increment_addr *) ;
$PAGE const_addr

function const_addr ( exp: expr ): addr_desc;

var
  scalar_value: integer;

begin
  with exp^.cst_val do begin
    
    if kind in [scalar_cst,ptr_cst] then begin	(* pointer and scalar constants *)
      if kind = ptr_cst
        then scalar_value := int_nil
        else scalar_value := ival;
      const_addr := int_value ( scalar_value );
    end

    else begin					(* all other constants *)
      const_addr := null_location;
      if kind = subr_cst
        then const_addr := def_addr ( get_def ( subr_def, blkp^.number ) )
      else if (kind = set_cst) andif (dimension (valp^.set_val) = 0)
	then const_addr := int_value ( 0 )
      else const_addr.reloc := gen_cnode ( valp, exp^.desc.base );
    end;
    const_addr.byte_size := expr_vax_type ( exp );

    (* Hack - PASS1 says varying string constants are non-varying in
       EXPR_TYPE_DESCs.  *)

    if (kind = string_cst) andif
       ( (exp^.desc.str_kind = nonvarying) and (valp^.str_varying_ref) ) then begin
      const_addr := increment_addr ( const_addr, str_lw_width div bits_per_byte );
      const_addr.byte_size := vax_byte;
    end;

  end  (* with *) ;
end  (* proc const_addr *) ;
$PAGE offset_addr

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

public function offset_addr ( var addr: addr_desc; offset: addr_desc ): addr_desc;

var
  offset_value: unit_range;
  temp_addr: addr_desc;

begin
  assert ( not (is_register(addr) or is_immediate(addr)) );
  assert ( offset.byte_size = vax_long );

  (* If the offset is a constant, then simply call increment_addr.  *)

  if aconstp ( offset, offset_value ) then begin
    offset_addr := increment_addr ( addr, offset_value );
  end

  (* The offset is not known at compile time.  We generate code to add it
     to the base address at runtime.  Several cases are distinguished based
     on the form of the base address.  *)

  else begin

    (* If the base address is indirect and not indexed, then a pointer
       to the addressed object is in memory and we can add the pointer
       and the offset, yielding an address of the form: '(REG)'.  *)

    if addr.indirect and (addr.index = noreg) then begin
      addr.indirect := false;
      addr.byte_size := vax_long;
      offset_addr := absolute_reference;
      temp_addr := binary_addr(addl2, addr, offset, [commutative], no_preference);
      offset_addr.register := temp_addr.register;
    end

    (* If the address is of the form 'n(REG)', then add the contents
       of REG to the offset, placing the result in a register.  Then an
       address of the form 'n(RESULT_REG)' will be returned.  *)

    else if is_disp_mode ( addr ) then begin
      offset_addr := absolute_reference;
      temp_addr := binary_addr ( addl2, reg_addr(addr.register),
				offset, [commutative], no_preference );
      offset_addr.register := temp_addr.register;
      offset_addr.reloc := addr.reloc;
      offset_addr.offset := addr.offset;
    end

    (* If all else fails, then move the base address to a register and
       add the offset to that register.  The address returned will be
       of the form: '(REG)'.  *)

    else begin
      offset_addr := reg_addr ( move_address ( addr ) );
      free ( offset );
      gen2 ( addl2, offset, offset_addr );
      offset_addr.reloc := none;
    end;

  end  (* else *) ;
end  (* proc offset_addr *) ;
$PAGE field_addr

function field_addr ( field_ref_exp: expr ): addr_desc  options special(coercions);

var
  base_addr: addr_desc;
  byte_offset: unit_range;

begin
  with field_ref_exp^ do begin

    (* Get base address of record.  *)

    base_addr := fetch ( base_rec , no_preference );

    (* Calculate fields address.  *)

    byte_offset := field_sym^.fld_offset div bits_per_byte;
    
    field_addr := increment_addr ( base_addr, byte_offset );

    if base_rec^.desc.base^.packable
      then field_addr.byte_size := packed_vax_type ( field_sym^.type_desc )
      else field_addr.byte_size := unpacked_vax_type ( field_sym^.type_desc );

  end (* with *) ;
end  (* proc field_addr *);
$PAGE deref_addr

(* DEREF_ADDR is passed a pointer valued expression tuple and returns
   an address descriptor for the target of the pointer.
   This routine has no type information about the target;
   therefore the caller must set the BYTE_SIZE field of the
   address descriptor returned.  *)

function deref_addr ( exp: expr ): addr_desc;

var
  reg: registers;

begin

  (* Fetch address of pointer being dereferenced.  *)

  deref_addr := fetch ( exp , no_preference );
  assert ( deref_addr.byte_size = vax_long );

  with deref_addr do begin

    (* If the base pointer is in a register, then simply return
       the address '0(reg)'.  *)

    if is_register ( deref_addr ) then begin
      reloc := none;
      offset := 0;
      index := noreg;
    end

    (* If the address of the pointer is of the form 'n(reg)', 
       then simply return the address '@n(reg)'.  *)

    else if is_disp_mode ( deref_addr ) then begin
      indirect := true;
    end

    (* Addresses of the form 'REL_SYMBOL' are converted to the
       form '@REL_SYMBOL'.  *)

    else if is_symbol_addr ( deref_addr ) then begin
      indirect := true;
    end

    (* For all other addresses, the pointer is loaded into a
       register and the address '0(reg)' is returned.  *)

    else begin
      reg := load_addr ( deref_addr, unsigned_value );
      deref_addr := absolute_reference;
      deref_addr.register := reg;
    end;

  end  (* with *) ;
end  (* proc deref_addr *) ;
$PAGE file_deref

(* FILE_DEREF generates code for a file variable dereference.  *)

function file_deref ( exp: expr ): addr_desc;

var
  reg: registers;

begin
  reg := load ( exp^.base_file );
  file_deref := absolute_reference;
  with file_deref do begin
    indirect := true;
    register := reg;
    byte_size := expr_vax_type ( exp );
  end;
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
  with array_expr^ do begin
    dynamic_flex :=
      ( ( (opcode = ptr_ref) or (opcode = field_ref) ) andif
        ( ( (desc.kind = strings) andif (desc.str_flex) ) or
          ( (desc.kind = arrays ) andif (desc.base^.flexible) ) ) );
  end;	
end  (* proc dynamic_flex *) ;
$PAGE get_lbound
(* GET_LBOUND is passed an expression tuple whose associated type
   is string or array.  It returns the constant
   lower bound of the string or array.  *)

function get_lbound ( exp: expr ): integer;

begin
  with exp^.desc do begin
    if kind = strings
      then get_lbound := 1
      else get_lbound := base^.index_type^.minval;
  end
end  (* proc get_lbound *);
$PAGE get_ubound	

(* GET_UBOUND is given an expression tuple whose associated type
   is a *** non-flexible *** array or string.  It returns the 
   constant upper bound of the array or string.  *)

function get_ubound ( exp: expr ): integer;

begin
  with exp^.desc do begin
    if kind = strings
      then get_ubound := str_length
      else get_ubound := base^.index_type^.maxval;
  end;
end  (* proc get_ubound *) ;
$PAGE upper_bound

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

public function upper_bound ( array_expr: expr; array_addr: addr_desc ): addr_desc;

var
  addr: addr_desc;

begin
  with array_expr^ do begin
    
    (* If array/string is not flexible then create an address
       descriptor for the constant upper bound.  *)

    if ( (desc.kind = strings) andif (not desc.str_flex) ) orif
       ( not desc.base^.flexible ) then begin
      addr := int_value ( get_ubound ( array_expr ) );
    end

    (* The array or string is flexible.  The opcode should be one
       of the following:
         i. an ident_ref tuple for a formal parameter;
        ii. a ptr_ref tuple for a flex array or string on the heap, or,
       iii. a field_ref tuple for a record on the heap whose last field
	    is a flex array or string.  *)

    else begin
    
      (* If dynamic flex, then upperbound is in descriptor word preceeding
	 array or string.  *)
      
      if (opcode = ptr_ref) or (opcode = field_ref)  then begin
	addr := duplicate_addr ( array_addr );
      end

      (* If opcode is ident_ref, then we have a formal parameter.  The
	 upperbound is the argument preceeding the address of the array.
	 The address of the first element of the array should be of the
	 form: '@offset(reg)' ; then the address of the upperbound is
	 'offset-4(reg)'.  *)

      else if opcode = ident_ref then begin
	addr := duplicate_addr ( array_addr );
	assert ( addr.indirect );
	addr.indirect := false;
 	addr.offset := addr.offset - 4;
      end
      else begin
	assert ( false );
      end;

    end  (* else *) ;
    
    (* For strings an address descriptor with vax_type VAX_WORD is returned;
       for arrays vax_type VAX_LONG is returned.  *)

    if desc.kind = strings
      then addr.byte_size := vax_word
      else addr.byte_size := vax_long;
    upper_bound := addr;
  end  (* with *) ;
end  (* proc upper_bound *) ;
$PAGE bound_op
(* BOUND_OP generates code for a LWB_OP or UPB_OP tuple.  Note 
   that these tuples are generated only for flexible arrays or
   strings.  Both tuples take a single operand - an expression
   tuple for the array or string whose bound is desired.  *
   Note that a word length result is returned for strings and a
   longword for arrays.  *)

function bound_op ( exp: expr  ): addr_desc;

var
  array_expr: expr;
  addr: addr_desc;
  string_desc: str_desc;

begin
  array_expr := exp^.operand[ 1 ];
  if array_expr^.desc.kind = strings then begin	(* strings *)
    string_desc := fetchstring ( array_expr, max_length );
    if exp^.opcode = lwb_op
      then bound_op := int_value ( 1 )
      else bound_op := duplicate_addr ( string_desc.len_addr );
    free_string ( string_desc );
  end
  else begin	(* arrays *)
    addr := fetch ( array_expr , no_preference );
    if exp^.opcode = lwb_op
      then bound_op := int_value ( get_lbound ( array_expr ) )
      else bound_op := upper_bound ( array_expr, addr );
    free ( addr );	(* upperbound does not free the array's address *)
  end;
  if array_expr^.desc.kind = strings
    then bound_op.byte_size := vax_word;
end  (* proc bound_op *) ;
$PAGE do_dim_op
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

function do_dim_op ( exp: expr ): addr_desc;

var
  array_expr: expr;
  array_addr: addr_desc;
  upper_addr: addr_desc;
  reg: registers;
  cons_part: integer;
  vtype: vax_type;
  opcode: opc_range;
  const_addr: addr_desc;

begin

  (* Calculate the address of the array and of its upperbound word. *)


  array_expr := exp^.operand[1];
  array_addr := fetch ( array_expr , no_preference );
  upper_addr := upper_bound ( array_expr, array_addr );
  free ( array_addr );

  (* Calculate upperbound - (lowerbound - 1)  *)

  cons_part := 1 - get_lbound ( array_expr );
  do_dim_op := add_constant ( upper_addr, cons_part, no_preference );
end  (* proc do_dim_op *) ;
$PAGE do_new_op
(* DO_NEW_OP generates code for a NEW_OP expression tuple.  The
   operand to a NEW_OP tuple is the size in bytes of the block of
   memory desired.  The result of a NEW_OP tuple
   evaluation is a pointer to the allocated block.  DO_NEW_OP
   returns an address descriptor for the pointer value.  *)

function do_new_op ( exp: expr; targ_addr: addr_desc ): addr_desc;

var
  reg: registers;
  size_addr: addr_desc;

begin

  (* Fetch the size of the area to be allocated. Since the size will be
     put onto the stack anyway, pass the stack id as the prefered target. *)

  size_addr := fetch ( exp^.operand [ 1 ] , push_reference );

  (* If the size addr returned is not a push reference, then push the value. *)

  if not adr_equal ( size_addr , push_reference )
    then push_value ( size_addr , alignment ( exp^.operand [ 1 ] ) )
  else free ( size_addr );

  gen_rt ( 1, rt_new );

  (* If prefered targget is valid then move result there, else to a non-
     volatile register. *)

  if adr_equal ( targ_addr, no_preference ) or (targ_addr.byte_size <> vax_long )
    then do_new_op := reg_addr ( load_addr ( r0_addr, alignment ( exp ) ) )
  else begin
    do_new_op := targ_addr;
    store ( r0_addr , targ_addr , alignment ( exp ) )
  end
end  (* proc do_new_op *);
$PAGE address_op

(* ADDRESS_OP generates code for an ADDR_OP expression tuple.
   Parameter EXP is the ADDR_OP tuple.  An address descriptor for
   the address formed is returned as the function return value.  *)

function address_op ( exp: expr ): addr_desc;

var
  string_desc: str_desc;
  addr: addr_desc;

begin
  with exp^ do begin

    (* Fetch the address of the operand.  The addresses returned are
       incremented past any flex string or array descriptor bytes.  *)

    if operand[ 1 ]^.desc.kind = strings then begin	(* strings *)
      string_desc := fetchstring ( operand[ 1 ], no_length );
      addr := duplicate_addr ( string_desc.base_addr );
      if string_desc.base_is_bound then begin	(* if flex, skip bounds word *)
        addr := increment_addr ( addr, flex_str_desc_size div bits_per_byte );
        addr.byte_size := vax_byte;
      end;
      free_string ( string_desc );
    end

    else begin	(* everything except strings *)
      addr := fetch ( operand [ 1 ] , no_preference );
      if dynamic_flex ( operand[ 1 ] ) then begin	(* if dynamic flex, skip bounds word *)
	addr := increment_addr ( addr, flex_arr_desc_size div bits_per_byte );
	addr.byte_size := vax_byte;
      end;
    end;

    (* Move the address of the operand to a register and return the
       address of the register.  *)

    assert ( not is_register ( addr ) );
    assert ( not is_immediate ( addr ) );
    free ( addr );
    address_op := reg_addr ( get_reg ( bits_per_address ) );
    gen2 ( typ_opc ( moval, addr.byte_size ), addr, address_op );

  end  (* with *) ;
end  (* proc address_op *) ;
$PAGE case_op

(* CASE_OP generates code for an upper or lower case conversion of a
   character.  Parameter ADDR is an address descriptor for the character
   to be translated.  Parameter CASE_CODE indicates whether an upper
   or lower case conversion is required.  This routine does not handle
   case conversions of strings.  Parameter ADDR is freed
   by this routine.  *)

function case_op ( addr: addr_desc; case_code: str_translation ): addr_desc;

var
  a,z: char;
  opcode: opc_range;
  a_addr, z_addr: addr_desc;
  out_label: def;
  delta_addr: addr_desc;

begin

  (* Set the comparison limits and the translation opcode.  *)

  if case_code = lower_trans then begin	(* lowercase op *)
    a := 'A';
    z := 'Z';
    opcode := addb2;
  end
  else begin	(* uppercase op *)
    a := 'a';
    z := 'z';
    opcode := subb2;
  end;

  a_addr := typ_int_value ( ord ( a ), vax_byte );
  z_addr := typ_int_value ( ord ( z ), vax_byte );

  (* Emit code to move the char to a register, compare it against the
     limits of upper or lower case alphabetics, and if within the limits,
     then translate the character.  *)

  free ( addr );
  case_op := typ_reg_addr ( get_reg ( char_size ), vax_byte );
  store ( addr, case_op, unsigned_value );	(* copy to reg *)
  gen2 ( cmpb, case_op, a_addr );	(* compare against lower limit of alphabetics *)
  out_label := make_def ( local_def );
  gen_branch ( blssu, out_label );	(* branch out if < *)
  gen2 ( cmpb, case_op, z_addr );	(* compare to upper limit of alphabetics *)
  gen_branch ( bgtru, out_label );	(* branch if greater *)
  delta_addr := typ_int_value ( ord ( 'a' ) - ord ( 'A' ), vax_byte );
  gen2 ( opcode, delta_addr, case_op );	(* translate *)
  mark_def ( code_area, out_label );	(* define the label *)

end  (* proc case_op *) ;
$PAGE array_addr

(* ARRAY_ADDR  generates an address descriptor for a subscripted
   reference.  Parameter EXP may be either an ARRAY_REF tuple or
   a SUBSTR_REF tuple whose SUBSTR_LENGTH field has the constant
   value 1 (SUBSTR_REF tuples are generated for subscripted 
   strings).  *)

public function array_addr ( exp: expr ): addr_desc;

var
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


begin
  with exp^ do begin
    
    checking_on := chk_sub_opt in cur_block^.semantic_options;
    desc_bytes := 0;				(* will count bytes of any length or *)
						(* bound words preceding array *)
    
    (* Set up some info we'll need later - first, we deal with
       subscripted *arrays*.  *)

    if opcode = array_ref then begin
      dyna_flex := dynamic_flex ( base_array );
      if dyna_flex
        then desc_bytes := flex_arr_desc_size div bits_per_byte;
      with base_array^.desc.base^ do begin
	elem_size := element_size;
	lower_bound := index_type^.minval;
      end;
      index_addr := fetch_fullword ( index_val );
      index_align := alignment ( index_val );
      base_addr := fetch ( base_array , no_preference );
      upb_addr := upper_bound ( base_array, base_addr );
      free ( upb_addr );
    end
    
    (* Get setup info for subscripted strings. *)

    else if opcode = substr_ref then begin
      dyna_flex := dynamic_flex ( base_string );
      if dyna_flex then desc_bytes := flex_str_desc_size div bits_per_byte;
      if base_string^.desc.str_kind = varying
	then desc_bytes := desc_bytes + ( str_lw_width div bits_per_byte );
      elem_size := char_size;
      lower_bound := 1;
      index_addr := fetch_fullword ( substr_index );
      index_align := alignment ( substr_index );
      index_const := aconstp ( index_addr, index_value );	(* only need length if *)
      if index_const or	(* INDEX instruction used *)
	 ( (base_string^.desc.str_kind = varying) and not checking_on)
	then length_context := no_length
	else length_context := actual_length;
      base_desc := fetchstring ( base_string, length_context );
      base_addr := duplicate_addr ( base_desc.base_addr ) ;
      if not index_const and	(* do not generate code if were not going *)
	 ( not indexable ( base_addr ) or checking_on )
	then begin
	  upb_addr := cvt_long ( duplicate_addr ( base_desc.len_addr ),
			    unsigned_value );	(* to use the upperbound *)
	  free ( upb_addr )
	end;
      free_string ( base_desc );
    end
    else begin
      assert ( false );
    end;

    (* Get info which is calculated the same way whether we have
       a string or an array. *)
 
    if (opcode = array_ref) andif (base_array^.desc.base^.packable)
      then elem_vax_type := packed_vax_type ( base_array^.desc.base^.element_type )
      else elem_vax_type := expr_vax_type ( exp );
    lwb_addr := int_value ( lower_bound );	
    bytes_per_elem := elem_size div bits_per_byte;
    free ( lwb_addr );

    (* Now we generate the indexing code! *)

    (* First, the easy case - a constant subscript.  *)

    if aconstp ( index_addr, index_value ) then begin
      byte_offset := desc_bytes + (index_value - lower_bound) * bytes_per_elem;
      array_addr := increment_addr ( base_addr, byte_offset );
    end

    (* Index is not constant. *)

    else begin

      (* If possible we generate a VAX index mode address.  If subscript
	 checking is on we use the INDEX instruction; otherwise an add or
	 subtract is used.  *)

      use_index_mode := ( indexable ( base_addr ) ) and
			( (bytes_per_elem in [1,2,4]) orif
			  ( (bytes_per_elem = 8) andif not dyna_flex ) ) andif
			( bytes_per_elem * bits_per_byte = vax_type_size(elem_vax_type) );

      if use_index_mode then begin
	base_adjustment := -lower_bound + (desc_bytes div bytes_per_elem);

	if checking_on then begin	(* checking on - use INDEX instr *)
	  free ( index_addr );
	  result_addr := reg_addr ( get_reg ( bits_per_address ) );
	  size := int_value ( 1 );
	  index_in := int_value ( base_adjustment );
	  gen6 ( index, index_addr, lwb_addr, upb_addr, size, index_in, result_addr );
	end

	else begin	(* checking off - use add or subtract *)
	  result_addr := add_constant ( index_addr, base_adjustment, no_preference );
	  if not is_register ( result_addr ) 
	    then result_addr := reg_addr( load_addr(result_addr,index_align) );
	end;

	array_addr := base_addr;
	array_addr.index := result_addr.register;
      end

      (* We can't generate an indexed address.  Use the index instruction
	 to generate the *byte* offset from the base address.  If possible
	 we incorporate the desc bytes into the INDEX calculation here. *)

      else begin
	free ( index_addr );
	result_reg := get_reg ( bits_per_address );
	result_addr := reg_addr ( result_reg );
	size := int_value ( bytes_per_elem );
	if (desc_bytes mod bytes_per_elem) = 0 then begin
	  result_offset := 0;
	  index_in := int_value ( (desc_bytes div bytes_per_elem) - lower_bound );
        end
        else begin
	  result_offset := desc_bytes;
	  index_in := int_value ( -lower_bound );
  	end;
	
	gen6 ( index, index_addr, lwb_addr, upb_addr, size, index_in, result_addr );
        
	(* Offset the base address by the byte offset contained in
	   RESULT_REG plus the constant offset RESULT_OFFSET.  *)

	array_addr := offset_addr ( base_addr, result_addr );
	array_addr.offset := array_addr.offset + result_offset;
      end  (* else *) ;

    end  (* else *) ;
    
    (* Set the VAX_TYPE of the element. *)

    array_addr.byte_size := elem_vax_type;

    if (opcode = substr_ref) andif	(* if base object was a string and case *)
      (base_desc.trans_code <> no_trans) 	(* ops were pending then upper/lower case the *)
      then array_addr := case_op ( array_addr, base_desc.trans_code );	(* resulting char *)

  end  (* with *) ;
end  (* proc array_addr *) ;
$PAGE do_binary_op

(* DO_BINARY_OP generates code for a binary operation.  
   The two operands (parameters OPERAND1 and OPERAND2) are fetched 
   as longwords and then routine BINARY_ADDR is called to generate
   the binary operation instruction.  The number of the register
   containing the result is returned as the function value.  Users
   of this routine should note the assumptions made by routine
   BINARY_ADDR.  *)


public function do_binary_op  ( opcode: opc_range; operand1: expr; operand2: expr;
		attributes: op_attr_set; targ_addr: addr_desc ): addr_desc;

var
  op1_addr: addr_desc;
  op2_addr: addr_desc;

begin
  op1_addr := fetch_fullword ( operand1 );
  op2_addr := fetch_fullword ( operand2 );

  do_binary_op := binary_addr ( opcode, op1_addr, op2_addr, attributes, targ_addr );
end  (* proc do_binary_op *) ;
$PAGE binary_op_addr

(* BINARY_OP_ADDR generates code for simple binary operations
   and returns an address descriptor for the result.
   Note: Currently the operations handled by this routine are integer
   and real addition, subtraction, multiplication and division.  *)

function binary_op_addr ( exp: expr; targ_addr: addr_desc ): addr_desc;

type
  sb_record = packed record			(* table mapping tuple opcodes into *)
    opcode: opc_range;				(* instruction opcodes and attributes *)
    attr: op_attr_set
  end;

  sb_array = packed array [iadd_op..rdiv_op] of sb_record;

const
  sb: sb_array = ( (addl2, [commutative]),
		   (subl2, []),
		   (mull2, [commutative]),
		   (divl2, []),
		   ( 0   , []),		(* imod_op, illegal in this routine *)
		   (addl2, [commutative]),
		   (subl2, []),
		   (mull2, [commutative]),
		   (divl2, [])
		 );

var
  scalar_value: int_type;
  index: 0..2;
  addr: addr_desc;

begin
  with exp^ do begin
    
    (* Integer addition or subtraction with a constant operand is done
       via function ADD_CONSTANT to take advantage of its special case
       checks.  *)

    index := 0;
    if (opcode = iadd_op) or (opcode = isub_op) then begin
      if iconstp ( operand[ 1 ], scalar_value ) and (opcode = iadd_op)
        then index := 2	(* index of non-constant operand *)
      else if iconstp ( operand[ 2 ], scalar_value )
	then index := 1
    end;
      
    if index <> 0 then begin	(* constant operand is present *)
      addr := fetch_fullword ( operand[ index ] );
      if opcode = isub_op then scalar_value := -scalar_value;
      binary_op_addr := add_constant ( addr, scalar_value, targ_addr );
    end

    (* If neither operand is constant then simply call DO_BINARY_OP. *)

    else begin
      binary_op_addr := do_binary_op ( sb[opcode].opcode, operand[1], operand[2],
			    sb[opcode].attr , targ_addr );
    end;
  end;
end  (* proc binary_op_addr *) ;
$PAGE do_imod_op

(* DO_IMOD_OP is passed an IMOD_OP expression tuple.  It generates
   code for the operation and returns an address descriptor for
   the result.  *)

function do_imod_op ( exp: expr ): addr_desc;

var
  op1_addr: addr_desc;
  op2_addr: addr_desc;

begin
  with exp^ do begin
    op1_addr := fetch_fullword ( operand[1] );
    op1_addr := cvt_quad ( op1_addr );	(* dividend must be quad word *)
    op2_addr := fetch_fullword ( operand[2] );

    free ( op1_addr );
    free ( op2_addr );

    do_imod_op := reg_addr ( get_reg ( bits_per_integer ) );
    gen4 ( ediv, op2_addr, op1_addr, reg_addr ( r0 ), do_imod_op );

  end  (* with *) ;
end  (* proc do_imod_op *) ;
$PAGE do_neg_op

(* DO_NEG_OP is passed an INEG_OP or RNEG_OP tuple and returns
   an address descriptor for the result of the operation.  *)

function do_neg_op ( exp: expr; targ_addr: addr_desc ): addr_desc;

var
  result_vtype : vax_type;
  vtype: vax_type;
  maddr : addr_desc;
  temp_addr : addr_desc;
  opc : opc_range;

begin

  (* If prefered target is given, fetch the results. If no target is given
     then fetch a fullword. *)

  if adr_equal ( targ_addr , no_preference )
    then begin
      maddr := fetch_fullword ( exp^.operand [ 1 ] );
      free ( maddr );
      opc := typ_opc ( mnegl , maddr.byte_size );
      do_neg_op := typ_reg_addr ( get_vax_reg(maddr.byte_size),maddr.byte_size);
    end
  else begin
    maddr := fetch ( exp^.operand [ 1 ] , no_preference );
    free ( maddr );
    
    (* IF the sizes of the target and operand do not match, convert
       the source to the proper size then negate. *)

    if targ_addr.byte_size <> maddr.byte_size
      then begin
	temp_addr := maddr;
	maddr := typ_reg_addr ( get_vax_reg(targ_addr.byte_size),targ_addr.byte_size);
	free ( maddr );
	store ( temp_addr , maddr , alignment ( exp ) );
      end;
    opc := typ_opc ( mnegl , targ_addr.byte_size );
    do_neg_op := targ_addr;
  end;

  (* move the results of the negation to the target or whatever *)

  gen2 ( opc , maddr , do_neg_op );

end  (* proc do_neg_op *) ;
$PAGE do_abs_op

(* DO_ABS_OP generates code for an IABS_OP or RABS_OP expression
   tuple.  An address descriptor for the result is returned.  *)

function do_abs_op ( exp: expr ): addr_desc;

var
  vtype: vax_type;
  reg: registers;
  local_label: def;

begin
  vtype := expr_vax_type ( exp );
  reg := copy_load ( exp^.operand[1] );	(* load value *)
  do_abs_op := typ_reg_addr ( reg, vtype );	(* set result location *)
  gen1 ( typ_opc ( tstl, vtype ), do_abs_op );	(* test value *)

  local_label := make_def ( local_def );
  gen_branch ( bgeq, local_label );	(* if ge, skip negate instr *)
  gen2 ( typ_opc ( mnegl, vtype ), do_abs_op, do_abs_op );	(* negate value *)
  mark_def ( code_area, local_label );	(* define skip label *)

end  (* proc do_abs_op *) ;
$PAGE do_fetch

(* DO_FETCH is passed an expression class tuple.  It calls the necessary
   routines to generate just enough code to make the result of the
   expression addressible on the target machine.  Normally FETCH or
   FETCHSTRING should be called to generate code for an expression tuple
   since they contain the logic necessary to handle tuples with multiple
   uses.  *)

public function do_fetch ( exp: expr; targ_addr: addr_desc ): addr_desc;

var
  addr: addr_desc;
  reg: registers;
  string_desc: str_desc;
  addr2: addr_desc;

const
  eof_eoln_offset = 34;		(* offset in file block to eof/eoln bits *)
  fileb_cursor_offset = 12;	(* offset in file block of cursor longword *)

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
	begin
	  do_fetch := deref_addr ( base_ptr );
	  do_fetch.byte_size := expr_vax_type ( exp );
	end;

      array_ref:
	do_fetch := array_addr ( exp );

      substr_ref:	(* subsripted strings appear as SUBSTR_REFs *)
	begin	(* with constant length 1 *)
	  assert ( desc.kind = chars );
	  do_fetch := array_addr ( exp );
	end;

      buffer_ref:
	do_fetch := file_deref ( exp );

      iadd_op,
      isub_op,
      imul_op,
      idiv_op,
      radd_op,
      rsub_op,
      rmul_op,
      rdiv_op:
	do_fetch := binary_op_addr ( exp , targ_addr );

      imod_op:
	do_fetch := do_imod_op ( exp );

      ineg_op,
      rneg_op:
	do_fetch := do_neg_op ( exp , targ_addr );

      iabs_op,
      rabs_op:
	do_fetch := do_abs_op ( exp );

      sclcvt_op:
	do_fetch := scalar_convert ( exp );

      expii_op:
	do_fetch := rt_value_call( exp, rt_exp_ii , targ_addr );

      expri_op:
	do_fetch := rt_value_call( exp, rt_exp_ri , targ_addr );

      exprr_op:
	do_fetch := rt_value_call( exp, rt_exp_rr , targ_addr );

      trunc_op,
      float_op:
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

      lwb_op,
      upb_op:
	do_fetch := bound_op ( exp );

      length_op:
	begin
	  string_desc := fetchstring ( operand[ 1 ], actual_length );
	  free ( string_desc.base_addr );
	  if string_desc.text_valid then free ( string_desc.text_addr );
	  do_fetch := string_desc.len_addr;
	end;

      lwc_op,
      upc_op:
	begin	(* case conversion op - chars only!! *)
	  assert ( desc.kind = chars );
	  do_fetch := fetch ( operand[ 1 ] , no_preference );
	  if opcode = lwc_op
	    then do_fetch := case_op ( do_fetch, lower_trans )
	    else do_fetch := case_op ( do_fetch, upper_trans );
	end;

      dim_op:
	do_fetch := do_dim_op ( exp );

      extent_op:	
	do_fetch := rt_value_call ( exp, rt_extent , targ_addr );

      filesize_op:
	do_fetch := rt_value_call ( exp, rt_file_size , targ_addr );

      round_op:
	do_fetch := do_round_op ( exp , targ_addr );

      arctan_op:
	begin
	  if upperbound (operand) = 1
	    then do_fetch := rt_ref_call ( exp, rt_r_atan, targ_addr )
	    else do_fetch := rt_ref_call ( exp, rt_r_atn2, targ_addr );
	end;

      imin_op,
      rmin_op:
	do_fetch := do_min_max_op ( exp, false );

      imax_op,
      rmax_op:
	do_fetch := do_min_max_op ( exp, true );

      index_op:
	do_fetch := do_index_op ( exp );

      search_op,
      verify_op:
	do_fetch := str_search_verify ( exp );

      ALC_TEMP_OP: begin
	assert ( ( exp^.desc.base^.base_size mod byte_size ) = 0 );
	DO_FETCH := GET_TEMP ( INT_VALUE(EXP^.DESC.BASE^.BASE_SIZE Div BYTE_SIZE ) , VAX_LONG );
      end;

      func_call_op:
	do_fetch := scl_function_call (exp , targ_addr );

      ile_op..filne_op,
      and_op,
      or_op,
      in_op,
      bnot_op,
      odd_op:
	do_fetch := reg_addr (load_bool (exp, false));

      new_op:
	do_fetch := do_new_op ( exp , targ_addr );

      open_op,
      rewrite_op,
      reset_op,
      update_op:
	do_fetch := rt_open_call ( exp );

      eof_op,
      eoln_op,
      eopage_op: begin
	reg := load (operand[1]);
	addr := absolute_reference;
	addr.register := reg;
  	addr.offset := eof_eoln_offset;
	do_fetch := reg_addr ( reg );
	if opcode = eopage_op
	  then addr2 := int_value ( 2 )
	  else addr2 := int_value ( ord ( opcode = eoln_op ) );
	gen4 (extzv, addr2, int_value (1), addr, do_fetch);
      end;

      cursor_op:
	begin
	  reg := load ( operand[ 1 ] );
	  do_fetch := absolute_reference;
	  do_fetch.register := reg;
	  do_fetch.offset := fileb_cursor_offset;
	end;

      iostatus_op:
	begin
	  if operand[ 1 ] = nil	(* EXP^.NOPERS is always one! *)
	    then do_fetch := rt_value_call ( exp, rt_iostat_last , targ_addr )
	    else do_fetch := rt_value_call ( exp, rt_iostatus , targ_addr );
	end;

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
  
      random_op:
	begin
	  if upperbound (operand) = 0
	    then do_fetch := rt_value_call ( exp, rt_rand , targ_addr )
	    else do_fetch := rt_value_call ( exp, rt_rand_set , targ_addr );
	end;

      date_op:
	begin
	  DO_FETCH := GET_TEMP ( INT_VALUE ( 9 ) , VAX_BYTE );
	  push_address ( duplicate_addr ( do_fetch ) );
	  gen_rt ( 1, rt_date );
	end;

      time_op:
	do_fetch := rt_value_call ( exp, rt_time , targ_addr );

      runtime_op:
	do_fetch := rt_value_call ( exp, rt_runtime , targ_addr );

      others:
	assert ( false )

    end  (* case *) ;

  end  (* with *) ;
end  (* proc do_fetch *) ;
$PAGE fetch

(* FETCH is the central routine for generating code for expression class
   tuples.  FETCH is passed an expression class tuple and returns an
   address descriptor for the result of the expression.  FETCH itself
   is concerned primarily with handling tuples with multiple uses.
   DO_FETCH is called to actually generate the code to evaluate the
   expression.  Note that FETCHSTRING should be called to make string
   valued expressions addressible.  *)

public function fetch (* ( exp: expr; targ_addr: addr_desc ): addr_desc *);	(* forward declared *)

var
  addr_ref: addr_ptr;
  reg1, reg2: registers;

begin
  with exp^ do begin

    (* If the RESULT field is not NIL then we have a multiple use
       expression which has already been evaluated.  In this case
       RESULT points to an address descriptor for the expression
       result, so we simply copy and return that address descriptor. *)

    if result <> nil then begin
      addr_ref := ptr ( ord ( result ) );
      fetch := addr_ref^;
    end
    else fetch := do_fetch ( exp , targ_addr );


    (* Check for tuples with multiple uses.  When a tuple has multiple
       uses a copy of the address descriptor is made on the heap and
       the RESULT field of the expr tuple is set to point to the copy.
       That copy is returned for subsequent fetches of the expression.
       In addition any registers used by the address descriptor 
       have their usage counts set to the usage count of the
       expression tuple so they will not be freed after their
       initial use.  *)

    if (usage_count > 1) andif (result = nil) then begin (* first of multiple uses *)
      new ( addr_ref );
      addr_ref^ := fetch;
      update_usages ( fetch, usage_count - 1 );
      result := ptr ( ord ( addr_ref ) );
    end;

    (* Decrement the usage count for the expression.  If the usage
       count drops to zero and we have an address descriptor on the
       heap, then dispose of the address descriptor. *)

    usage_count := usage_count - 1;
    assert ( (usage_count >= 0) and (usage_count <> maximum(usage_range)) );
    if (usage_count = 0) andif (result <> nil)	(* last of multiple uses *)
      then dispose ( addr_ref );
  end  (* with *) ;
end  (* proc fetch *) ;
$PAGE fetch_fullword

public function fetch_fullword (* ( exp: expr ): addr_desc *) ;

var
  addr: addr_desc;
  reg: registers;

begin
  addr := fetch ( exp , no_preference );

  if addr.byte_size in [vax_byte, vax_word] then begin
    if is_immediate ( addr ) then begin
      addr.byte_size := vax_long;
    end
    else begin
      reg := load_addr ( addr, alignment ( exp ) );
      addr := reg_addr ( reg );
    end;
  end;
  fetch_fullword := addr;

end (* proc fetch_fullword *)  ;
(* --- not used --- we're keeping it around as it might be handy sometime --- 
$PAGE nonindexed_fetch

(* NONINDEXED_FETCH fetches an expression tuple and returns an address
   which is guaranteed not to be indexed.  *)

public function nonindexed_fetch  ( exp: expr ): addr_desc  ;

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

(* MEM_FETCH fetches an expression tuple and returns an address
   which is guaranteed to be a memory address, i.e., not an
   immediate value or a register.  *)

public function mem_fetch (*  ( exp: expr ): addr_desc  *)  ;

var
  addr: addr_desc;
  value: int_type;
  bit_size: set_range;
  temp_size: unit_range;

begin

  (* Fetch value.  *)

  addr := fetch ( exp , no_preference );


  if aconstp ( addr, value ) then
    mem_fetch := gen_cst ( value, addr.byte_size )

  (* If in a register, then move to a temp.  *)

  else if is_register ( addr ) then begin
    temp_size := ngm ( expr_size(exp), bits_per_reg ) div byte_size;
    MEM_FETCH := GET_TEMP ( INT_VALUE ( TEMP_SIZE ) , VAX_LONG );
    mem_fetch.byte_size := addr.byte_size;
    free ( addr );
    store ( addr, mem_fetch, alignment(exp) );
  end

  (* Otherwise, leave it alone. *)

  else mem_fetch := addr;

end  (* proc mem_fetch *) ;
$PAGE argument

public function argument (* ( exp: expr ): addr_desc *) ;

var
  reg: registers;

begin

  argument := fetch_fullword ( exp );
 
  if argument.indirect orif ( argument.index <> noreg ) then begin
    reg := move_address ( argument );
    argument := absolute_reference;
    argument.register := reg;
  end;

end  (* proc argument *)  .
e@lo