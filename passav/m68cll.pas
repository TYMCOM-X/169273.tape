$TITLE M68000 procedure/function calling routines
module m68cll options check;
$PAGE includes and declarations
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68cgu.inc
$SYSTEM m68utl.inc
$SYSTEM m68exp.inc
$SYSTEM pastal.inc
$SYSTEM m68gen.inc
$SYSTEM m68str.inc
$SYSTEM pasmth.inc
$SYSTEM m68set.inc

type
  parameter_handler = procedure ( op_desc; boolean );
$PAGE pascal_call
(* PASCAL_CALL generates code to call a PASCAL procedure or function. The parameter
   "node" is the call_op or func_call_op node for which code is to be generated;
   "quick" is true only if the routine has a quick calling sequence;
   and "rv_addr" is the address of a location for the return value of function's
   (not applicable if rv_addr.mode is null_mode). *)

procedure pascal_call (
	    node: expr;
	    quick: boolean;
	    rv_addr: op_desc );

const
  static_link = 20;	(* Offset to parent frame pointer. *)
  bytes_per_long = bits_per_long div bits_per_byte;
  bytes_per_address = bits_per_address div bits_per_byte;

var

(* IDX is the index into the ARGLIST array of parameter expressions and
   is used by many of the internal routines to locate the current parameter
   information. *)

  idx: parm_index;
  mask, i, regs_saved, levels_dif: integer;
  reg_mask, save_location, routine, temp_desc: op_desc;
  reg, first_reg: registers;
  areg: addr_regs;
  routine_fetched: boolean;
  arg_block: op_desc;
  skip_label: def;

  (* DOUBLE_REALS is a count of the number of double precision real
     parameters encountered in evaluation of the argument list.  It
     is used to determine the number of DFPOP calls required after
     their evaluation. *)

  double_reals: integer;
$PAGE fetch_parameter - in pascal_call
(* FETCH_PARAMETER just fetches the current parameter, passing its operand
   descriptor(s) to a caller-supplied routine.  MODES is either STACK_MODES
   (if the parameter is to be pushed immediately onto the stack) or the null set. *)

procedure fetch_parameter (
		modes: addr_mode_set;
		process_parameter: parameter_handler );

var
  parm_sym_kind: sym_kind;
  parm_type: typ;
  parm_type_kind: type_kind;
  parm_bit_width: bit_range;
  parm_alignment: align_range;
  i, levels_dif: integer;
  base_desc, temp_desc1, temp_desc2: op_desc;
  pass_by_address: boolean;
  required_size: op_size_set;
  parm_size: op_sizes;

begin 
  with node^ do begin
    parm_sym_kind := subr^.desc.base^.params[idx].parm_kind;
    parm_type := subr^.desc.base^.params[idx].parm_type;
    parm_type_kind := parm_type^.kind;
    if parm_type_kind = strings then
      str_parameter ( arglist[idx], parm_sym_kind, parm_type, process_parameter )
    else if parm_type_kind = sets then
      set_parameter ( arglist[idx], parm_sym_kind, parm_type, process_parameter )
    else if parm_type_kind in [procs, funcs] then begin
      base_desc := fetch ( arglist[idx], control_modes + modes, immediate_mode, any_size, true );

      (* If the parameter is a subroutine variable we need only pass its
	 address; otherwise a temporary must be allocated and initialized
	 with the parent stack pointer and the routine's address. *)

      if not ( ( arglist[idx]^.opcode <> cst_ref ) andif
	       ( ( arglist[idx]^.opcode <> ident_ref ) orif
		 ( arglist[idx]^.id_sym^.kind <> consts ) ) ) then begin

	temp_desc1 := get_temp ( int_desc ( 8, size_long, false ), false );
	temp_desc1.value_size := size_long;
	temp_desc1 := increment_addr ( temp_desc1, 4 );
	base_desc := ref_descriptor ( base_desc );
	gen_mm ( move_opc, base_desc, temp_desc1 );
	free_desc ( base_desc );
	temp_desc1 := increment_addr ( temp_desc1, -4 );
	if ( base_desc.cst_part.kind = external_sc ) orif
	   ( arglist[idx]^.cst_val.blkp^.apparent_level <= 2 ) then begin	(* no parent frame *)
	  gen_m ( clr_opc, temp_desc1 );
	end
	else begin
	  levels_dif := cur_block^.apparent_level - arglist[idx]^.cst_val.blkp^.apparent_level;
	  if levels_dif = -1 then
	    gen_rm ( move_opc, fp, temp_desc1 )
	  else begin	(* must chase static chain *)
	    temp_desc2 := descriptors[displacement_mode];
	    temp_desc2.reg := fp;
	    temp_desc2.value_size := size_long;
	    temp_desc2.cst_part.offset := static_link;
	    if levels_dif > 0 then begin
	      areg := get_areg ();
	      for i := 1 to levels_dif do begin
		gen_mr ( move_opc, temp_desc2, areg );
		temp_desc2.reg := areg;
	      end;
	      free_reg ( areg );
	    end;
	    gen_mm ( move_opc, temp_desc2, temp_desc1 );
	  end;
	end;
	base_desc := temp_desc1;
      end;
      process_parameter ( base_desc, true );
    end (* if proc/func *)
    else if parm_type_kind = arrays then begin
      base_desc := fetch ( arglist[idx], control_modes + modes, immediate_mode, any_size, false );
      temp_desc2 := duplicate_desc ( base_desc );
      if dynamic_flex ( arglist[idx] ) then
	temp_desc2 := increment_addr ( temp_desc2, arr_upb_bytes );
      process_parameter ( temp_desc2, true );
      if parm_type^.flexible then
	process_parameter ( coerce ( upper_bound ( arglist[idx], base_desc ), all_modes, immediate_mode, long_only, false ), false );
      free_desc ( base_desc );
    end
    else begin
      pass_by_address := ( parm_sym_kind = vars ) or p_b_address ( parm_type );
      if pass_by_address then
	required_size := any_size
      else begin
	alc_data ( parm_type, parm_bit_width, parm_alignment );
	if parm_bit_width <= bits_per_byte then
	  parm_size := size_byte
	else if parm_bit_width <= bits_per_word then
	  parm_size := size_word
	else if parm_bit_width <= bits_per_long then
	  parm_size := size_long
	else parm_size := size_double;
	required_size := at_least[parm_size];
      end;
      if ( arglist[idx]^.desc.kind = reals ) andif
	 ( arglist[idx]^.desc.precision = maximum ( prec_type ) ) then begin
	base_desc := dfetch ( arglist[idx], descriptors[null_mode], false );
	double_reals := double_reals + 1;
      end
      else base_desc := fetch ( arglist[idx], nonstack_modes + modes, immediate_mode, required_size, true );
      if not pass_by_address then
	conform_size ( base_desc, parm_size );
      process_parameter ( base_desc, pass_by_address );
    end;
  end (* with *)
end (* fetch_parameter *);
$PAGE block_parameters
(* BLOCK_PARAMETERS evaluates the parameter list for a call to a quick
   routine, building an argument block in the caller's stack frame.
   Since we don't have to worry about dynamic temporaries on the stack,
   the parameters are merely evaluated in reverse order and placed in
   the argument block immediately by PLACE_PARAMETER. *)

procedure block_parameters;

var
  i: integer;
$PAGE place_parameter - in block_parameters
(* PLACE_PARAMETER places an parameter into the argument block for a quick
   routine call. *)

procedure place_parameter ( parameter: op_desc; by_addr: boolean );

const
  sizes: array[op_sizes] of integer := ( 1, 2, 4, 8, 0 );

var
  temp_desc, null_desc: op_desc;
  flag: boolean;

begin
  if by_addr then begin
    arg_block := increment_addr ( arg_block, -bytes_per_address  );
    arg_block.value_size := size_long;
    temp_desc := ref_descriptor ( parameter );
    gen_mm ( move_opc, temp_desc, arg_block );
    free_desc ( temp_desc );
  end
  else begin
    arg_block := increment_addr ( arg_block, - sizes[parameter.value_size] );
    arg_block.value_size := parameter.value_size;
    if parameter.value_size = size_double then begin
      blk_move ( int_desc ( 8, no_size, true ),
		 false,
		 parameter,
		 arg_block,
		 null_desc,
		 flag );
      free_desc ( null_desc );
    end
    else begin
      if parameter.value_size = size_byte then
	arg_block := increment_addr ( arg_block, - sizes[size_byte] );
      gen_mm ( move_opc, parameter, arg_block );
      free_desc ( parameter );
    end;
  end;
end;
$PAGE block_parameters - body
begin
  with node^ do begin
    arg_block := descriptors[displacement_mode];
    arg_block.reg := fp;

    (* If this is a function call, the base of the argument list is given
       by the RETURN_SYM for the function; for a procedure, it is the
       ITEM_ADDR of the first parameter symbol. *)

    if opcode = func_call_op then
      arg_block.cst_part.offset := subr^.cst_val.blkp^.return_sym^.item_addr
    else if upperbound ( arglist ) > 0 then begin
      arg_block.cst_part.offset := subr^.cst_val.blkp^.parm_list.first^.item_addr;

      (* If the parameter is flexible then this offset does not include
	 space for the descriptor bound. Adjust accordingly. *)

      if subr^.desc.base^.params[1].parm_type^.flexible then
	if subr^.desc.base^.params[1].parm_type^.kind = arrays
	  then arg_block.cst_part.offset := arg_block.cst_part.offset - arr_upb_bytes
	  else arg_block.cst_part.offset := arg_block.cst_part.offset - str_upb_bytes;
    end;
    arg_block.cst_part.offset := arg_block.cst_part.offset + subr^.desc.base^.parmlist_size;
    for idx := upperbound ( arglist ) downto 1 do begin
      fetch_parameter ( stack_modes, place_parameter );
    end;
  end;
end;
$PAGE stack_parameters - in pascal_call
(* STACK_PARAMETERS evaluates the parameter list for a call to a non-quick
   routine.  Since parameters are passed on the stack, evaluation of
   parameters involving dynamic temporaries is done before the parameters
   are actually pushed onto the stack.  Those which are "pre-fetched" in
   this manner are fielded by HOLD_PARAMETER, and eventually everything
   is pushed by PUSH_PARAMETER. *)

procedure stack_parameters;

var
  i: integer;

  (* PARAMETERS is a pointer to an array of records describing entries in
     the parameter list which have been evaluated out of sequence to avoid
     allocation of dynamic temps in the midst of the parameter list.
     Two descriptors are allocated for each parameter since some (e. g.,
     flexible and procedure/function-valued parameters) may generate more
     than one distinct entry in the parameter list. *)

  parameters: ^ array[1..*] of record
    entries: 0..2;	(* Number of descriptors entered for this parameter. *)
    entry: array[1..2] of record	(* Actual operand descriptors. *)
      op: op_desc;
      by_address: boolean
    end
  end;
$PAGE hold_parameter - in stack_parameters
(* HOLD_PARAMETER places an operand descriptor for the current parameter
   into the PARAMETERS array so that it may eventually be pushed onto the stack. *)

procedure hold_parameter ( parameter: op_desc; by_addr: boolean );

begin
  with parameters^[idx] do begin
    assert ( entries < maximum ( entries ) );
    entries := succ ( entries );
    with entry[entries] do begin
      op := parameter;
      by_address := by_addr;
    end;
  end;
end (* hold_parameter *);
$PAGE push_parameter - in stack_parameters
(* PUSH_PARAMETER pushes a parameter or its address onto the stack. *)

procedure push_parameter ( parameter: op_desc; by_addr: boolean );

begin
  if by_addr
    then pusha ( parameter )
    else push ( parameter, no_size );
end (* push_parameter *);
$PAGE mbd_str - in stack_parameters
(* MBD_STR examines a parameter expression and returns TRUE if and only
   if the expression represents a flexible string passed as a value
   parameter, since this will almost certainly result in the allocation
   of a dynamic temporary. *)

function mbd_str ( node: expr; index: oper_range ): boolean;

var
  base_expr: expr;
  case_code: str_translation;

begin
  mbd_str := false;	(* until known otherwise *)
  with node^.subr^.desc.base^.params[index] do begin
    if ( parm_kind = values ) andif ( parm_type^.kind = strings ) then begin
      str_case_ops ( node^.arglist[index], base_expr, case_code, false );
      mbd_str := ( base_expr^.desc.kind = strings ) andif
		 ( base_expr^.desc.str_flex );
    end;
  end (* with *);
end (* mbd_str *);
$PAGE mbd - in stack_parameters
(* MBD (May Be Dynamic) is a recursive function used to examine an
   expression in the internal form. It returns a Boolean value of FALSE
   if and only if it can be determined that evaluation of the expression
   will NOT generate dynamic temporaries on the stack.	*)

function mbd ( e: expr ): boolean;

var i: oper_range;
$PAGE mbd_set - in mbd
(* MBD_SET examines an expression tree for a set and returns TRUE if its
   evaluation may generate dynamic temporaries. *)

function mbd_set ( e: expr ): boolean;

begin
  with e^ do begin

    (* For the moment, treat sets without a constant lowerbound and
       dimension as involving dynamic temps. *)

    mbd_set := not ( desc.set_cst_lwb and desc.set_cst_len );
  end (* with *);
end (* mbd_set *);
$PAGE mbd - body
begin
  mbd := false;	(* Until known otherwise *)
  with e^ do begin

    (* If the expression has already been evaluated (e. g., for check
       tuples), any dynamic temps involved will already have been
       generated and no further examination of the expression tree
       is necessary. *)

    if result <> nil then return;	(* <--- Short circuit test. *)

    if desc.kind = sets then begin
      mbd := mbd_set ( e );
    end
    else if opcode = cat_op then begin	(* special case this *)

      (* If any operand of a concatenation tuple is flexible, then the
	 cat_op tuple will be marked as flex and we need not check each
	 operand in that case.  However, even if this test fails we must
	 still check each operand since they may involve dynamic temps
	 in other ways. *)

      mbd := desc.str_flex;
    end;

    (* If none of the above special cases suceeded, continue the check. *)

    if not mbd then
      case opcode of

      field_ref:
	mbd := mbd ( base_rec );

      ptr_ref:
	mbd := mbd ( base_ptr );

      array_ref:
	mbd := mbd ( base_array ) orif
	       mbd ( index_val );

      buffer_ref:
	mbd := mbd ( base_file );

      func_call_op: begin
	for i := 1 to upperbound ( arglist ) do begin	(* Examine arguments. *)
	  mbd := mbd_str ( e, i ) orif mbd ( arglist[i] );
	exit if mbd;
	end;
	if not mbd then	(* Reference to routine itself may do it. *)
	  mbd := mbd ( subr );
      end;

      nary_op..last_nary_op: begin	(* All those with general operand lists. *)
	for i := 1 to upperbound ( operand ) do begin	(* Examine operands. *)
	  mbd := ( operand[i] <> nil ) andif mbd ( operand[i] );
	exit if mbd;
	end;
      end;

      others:	(* Should be ok. *)

    end (* case *);
  end (* with *);
end (* may be dynamic *);
$PAGE stack_parameters - body
begin
  with node^ do begin

    if upperbound ( arglist ) > 0 then begin

      (* Allocate the array to hold parameter operand descriptors until
	 dynamic temps have been allocated. *)

      new ( parameters, upperbound ( arglist ) );
      for idx := 1 to upperbound ( arglist ) do	(* Initialize *)
	parameters^[idx].entries := 0;

      (* Examine each parameter expression, evaluating it if there is any
	 possibility of its generating dynamic temporaries. *)

      for idx := 1 to upperbound ( arglist ) - 1 do begin
	if mbd_str ( node, idx ) orif mbd ( arglist[idx] ) then
	  fetch_parameter ( [], hold_parameter );
      end;

      (* Check the expression tuple for the routine itself, just in case
	 it involves dynamic temps.  Otherwise, delay fetching it until
	 the parameters have been pushed. *)

      if mbd ( subr ) then begin
	routine := fetch ( subr, control_modes, immediate_mode, any_size, true );
	routine_fetched := true;
      end;

      (* Make a second pass over the argument list, fetching those not
	 already evaluated, and pushing everything onto the stack. *)

      for idx := upperbound ( arglist ) downto 1 do
	with parameters^[idx] do begin
	  if entries = 0 then
	    fetch_parameter ( stack_modes, push_parameter )
	  else begin
	    for i := 1 to entries do
	      with entry[i] do
		push_parameter ( op, by_address );
	  end;
	end;

      dispose ( parameters );
    end (* if parameters to be pushed *);
  end;
end (* stack_parameters *);
$PAGE pascal_call - body
begin (* pascal_call *)
  with node^ do begin
    routine_fetched := false;
    double_reals := 0;
    if quick
      then block_parameters
      else stack_parameters;

    if not routine_fetched then
      routine := fetch ( subr, control_modes, immediate_mode, any_size, true );

    (* If this is a function call, push the address of the location
       for the return value. *)

    if rv_addr.mode <> null_mode then begin
      if quick then begin
	areg := get_areg ();
	gen_mm ( lea_opc, rv_addr, reg_desc ( areg, size_long, true ) );
	arg_block.value_size := size_long;
	arg_block.extended_size := no_size;
	arg_block := increment_addr ( arg_block, -bytes_per_address );
	gen_rm ( move_opc, areg, arg_block );
	free_reg ( areg );
      end
      else begin
	pusha ( duplicate_desc ( rv_addr ) );
      end;
    end;

    (* Free up any double real temporaries allocated during parameter
       evaluation. *)

    for i := 1 to double_reals do
      dfpop;

    (* Save any allocated registers. *)

    mask := 0;
    regs_saved := 0;
    for reg := minimum ( registers ) to pred ( bp ) do begin
      if uses_remaining ( reg ) <> 0 then begin
	if regs_saved = 0 then
	  first_reg := reg;
	regs_saved := regs_saved + 1;
	mask := mask + 2 ** ord ( reg );
      end;
    end;
    if regs_saved > 0 then begin
      save_location := get_temp ( int_desc ( 4 * regs_saved, size_long, true ), false (* byte cnt *));
      if regs_saved = 1 then
	gen_rm ( move_opc, first_reg, save_location )
      else begin
	reg_mask := int_desc ( mask, size_word, false );

	(* Save all registers as longwords. *)

	save_location.value_size := size_long;
	save_location.extended_size := no_size;
	gen_mm ( movem_opc, reg_mask, save_location );
      end;
    end;

    (* Check for a subroutine variable. *)

    if ( subr^.opcode <> cst_ref ) and
       ( ( subr^.opcode <> ident_ref ) orif
	 ( subr^.id_sym^.kind <> consts ) ) then begin
      routine.value_size := size_long;

      (* Test the staink at runtime; if it zero then we are calling
	 a level 1 routine and should NOT push a frame pointer. *)

      skip_label := def_create ( local_def );
      gen_m ( tst_opc, routine );
      gen_bcc ( eq_cc, skip_label );
      push ( duplicate_desc ( routine ), size_long );	(* push the static link if non-zero *)
      gen_def ( code_area, skip_label );

      (* Increment the descriptor for the routine to indicate its address,
	 and prepare it for the call. *)

      routine := deref_descriptor ( increment_addr ( routine, 4 ), size_long, true );
    end
    else if not quick and ( subr^.opcode = cst_ref ) andif ( subr^.cst_val.blkp^.level > 2 ) then begin

      (* Must push the static link. *)

      levels_dif := cur_block^.apparent_level - subr^.cst_val.blkp^.apparent_level;
      if levels_dif = -1 then	(* Caller is parent *)
	push ( reg_desc ( fp, size_long, true ), size_long )
      else begin
	temp_desc := descriptors[displacement_mode];
	temp_desc.reg := fp;
	temp_desc.value_size := size_long;
	temp_desc.cst_part.offset := static_link;
	if levels_dif > 0 then begin
	  areg := get_areg (  );
	  for i := 1 to levels_dif do begin
	    gen_mr ( move_opc, temp_desc, areg );
	    temp_desc.reg := areg;
	  end;
	end;
	push ( temp_desc, size_long );
      end;
    end (* if static link necessary *);

    gen_m ( jsr_opc, routine );	(* Finally, the call. *)
    free_desc ( routine );

    (* Restore saved registers. *)

    if regs_saved > 0 then begin
      if regs_saved = 1
	then gen_mr ( move_opc, save_location, first_reg )
	else gen_mm ( movem_opc, save_location, reg_mask );
      free_desc ( save_location );
    end;

  end (* with *);
end (* pascal_call *);
$PAGE fortran_call
(* FORTRAN_CALL generates a call to a fortran procedure or function. The parameter
   "node" is the call_op or func_call_op node. *)

procedure fortran_call ( node: expr; return_value: op_desc );

begin

  (* There should be none, so we just ... *)

  assert ( false );

end (* fortran_call *);
$PAGE quick_routine
(* QUICK_ROUTINE returns a Boolean value indicating whether or not
   a specified routine has a "quick" calling sequence. *)

function quick_routine ( subr: expr ): boolean;

begin
  quick_routine :=
    ( subr^.opcode = cst_ref ) andif
    ( subr^.cst_val.blkp^.owner <> subr^.cst_val.blkp );
end (* quick_routine *);
$PAGE procedure_call
(* PROCEDURE CALL compiles code for a call operator "node". *)

public procedure procedure_call ( node: tuple );

begin
  if node^.subr^.desc.base^.fortran_call
    then fortran_call ( node, descriptors[null_mode] )
    else pascal_call ( node, quick_routine ( node^.subr ), descriptors[null_mode] );
end;
$PAGE function_call
(* FUNCTION CALL compiles code for a func_call_op "node" whose result
   may be of any size (scalar or otherwise). *)

public function function_call ( node: expr ): op_desc;

var
  a, b, c, d: integer;
  gruesome: boolean;

begin
  if ( node^.desc.kind = reals ) andif
     ( node^.desc.precision = maximum ( prec_type ) ) then
    function_call := dftemp
  else begin
    size_of (node^.subr^.desc.base^.return_type, false, a, b, c, d, gruesome);
    assert (not gruesome and (a = 0) and (c = 1) and (d = 0));
    function_call := get_temp ( int_desc ( b, size_word, true ), false (* byte cnt *) );
  end;
  ops_expr_type_desc ( function_call, node^.desc, false );
  if node^.desc.base^.fortran_call
    then fortran_call ( node, function_call )
    else pascal_call ( node, quick_routine ( node^.subr ), function_call )
end (* function_call *).
    T@!