$TITLE M68EXP - M68000 expression evaluation

module m68exp options check;
$PAGE includes
$SYSTEM pascal
$SYSTEM pasist
$SYSTEM ptmcon
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68gen
$SYSTEM pastal
$SYSTEM m68utl
$SYSTEM m68cgu
$SYSTEM m68cll
$SYSTEM m68cmp
$SYSTEM m68str
$SYSTEM m68io
$PAGE forward declarations
public function fetch ( e : expr;
			allowed_modes : addr_mode_set;
			preferred_mode : addr_mode;
			allowed_sizes : op_size_set;
			sign_required : boolean
						) : op_desc
  options special (coercions);
forward;

public function dfetch ( source : expr; dest : op_desc; expr_uses_ok : boolean ) : op_desc;
forward;

public procedure dec_expr_usage (e: expr); forward;
$PAGE guard
(*  GUARD is used to protect a previously computed operand during the
    computation of a subsequent operand.  It is called with an operand
    descriptor which might be on the stack, and returns a "guard key".
    A subsequent call to UNGUARD with the same guard key will return
    an operand descriptor which may be used to access the same operand.
    Usually, UNGUARD will return the same operand which was passed to
    GUARD.  However, if the guarded operand was on the stack, and a
    dynamic temporary has since been allocated (as indicated by a call
    to STK_BASH), then the operand will have been moved into a data register
    for safety, and UNGUARD will return a descriptor for the register.  *)

type
    guard_record = record
	op : op_desc;
	next : guard_key;
    end;

var guard_stack : guard_key;


public function guard ( op : op_desc ) : guard_key;

begin
  new (guard);
  guard^.op := op;
  guard^.next := guard_stack;
  guard_stack := guard;
end (* guard *);
$PAGE unguard
(*  UNGUARD will return a descriptor for an operand which was guarded with
    a call to GUARD.  *)

public function unguard ( key : guard_key ) : op_desc;

begin
  assert (key = guard_stack);
  unguard := key^.op;
  guard_stack := key^.next;
  dispose (key);
end (* unguard *);
$PAGE grd_init
(*  GRD INIT initializes the guard stack at the beginning of compilation
    of a single routine.  *)

public procedure grd_init;

begin
  guard_stack := nil;
end (* grd_init *);
$PAGE grd_term
(*  GRD TERM is called at the end of compilation for each statement.  It
    checks that nothing has been left on the guard stack.  *)

public procedure grd_term;

begin
  assert (guard_stack = nil);
end (* grd_term *);
$PAGE loadr
(*  LOADR will load a relocatable value into a specified register, using the
    most efficient instruction to load at least the specified size.  It
    returns an operand descriptor for the loaded register.

    ***  NOTE  ***

    We assume that it is ok to load the entire register if necessary, so that
    we can use a MOVEQ even if only a byte value is to be loaded.  This is
    the whole point in using LOADR, rather than simply generating a MOVE
    in-line.  *)

public function loadr ( r : registers;
			rval : reloc_value;
			size : op_sizes;
			sign_required : boolean
						) : op_desc;

var rdesc : op_desc;

begin
  rdesc := rel_desc (rval, no_size, sign_required or (r in addr_reg_set));
  assert (rdesc.value_size <= size);
  loadr := reg_desc (r, size, rdesc.signed_value or (rdesc.value_size < size));

  if (loadr.mode = dreg_mode) and (rdesc.value_size = size_byte) then begin
    if (size = size_byte) or (rdesc.signed_value) then
      loadr.value_size := size_long;	(* we can use MOVEQ *)
    if rdesc.signed_value then
      loadr.extended_size := size_long;	(* extended sign bit will be correct *)
  end;

  if (loadr.mode = areg_mode) and (rdesc.value_size <= size_word) then
    loadr.value_size := size_word;	(* MOVE.W will suffice *)

  gen_mm (move_opc, rdesc, loadr);
  loadr.value_size := size;
  loadr.known_positive := rdesc.known_positive;
end (* loadr *);
$PAGE loadi
(*  LOADI will load an integer value into a specified register, using the
    most efficient instruction to load at least the specified size.  It
    returns an operand descriptor for the loaded register.

    ***  NOTE  ***

    We assume that it is ok to load the entire register if necessary, so that
    we can use a MOVEQ even if only a byte value is to be loaded.  This is
    the whole point in using LOADI, rather than simply generating a MOVE
    in-line.  *)

public function loadi ( r : registers;
			ival : integer;
			size : op_sizes;
			sign_required : boolean
						) : op_desc;

begin
  loadi := loadr (r, (ival, absolute_sc), size, sign_required);
end;
$PAGE make_index_long
(*  MAKE INDEX LONG takes an indexed operand descriptor, and forces it to
    have a long (rather than a word) indexing mode.  *)

procedure make_index_long ( var op : op_desc );

begin
  assert (op.mode in indexed_modes);
  if op.mode in [index_w_mode, pc_index_w_mode] then begin
    if op.index_reg in data_reg_set then
      gen_r (ext_opc, op.index_reg, size_long);
    if op.mode = index_w_mode
      then op.mode := index_l_mode
      else op.mode := pc_index_l_mode;
  end;
end (* make_index_long *);
$PAGE coerce
(*  COERCE is called with an operand descriptor which may or may not
    have the required address mode and size.  It returns an operand
    descriptor for the same value which does have the required mode and
    size.  Coerce transforms its input descriptor--that is, it transfers one
    use from its input descriptor to its output descriptor.  *)

public function coerce ( desc : op_desc;
			 allowed_modes : addr_mode_set;
			 preferred_mode : addr_mode;
			 allowed_sizes : op_size_set;
			 sign_required : boolean
						 ) : op_desc;


const
    a_modes : addr_mode_set =
	areg_relative_modes + register_modes;

    coerces_to : array [addr_mode] of addr_mode_set =
      ( register_modes,				(* areg_mode *)
	register_modes,				(* dreg_mode *)
	a_modes,				(* indirect_mode *)
	register_modes + [predecrement_mode],	(* predecrement_mode *)
	register_modes + [postincrement_mode],	(* postincrement_mode *)
	a_modes,				(* displacement_mode *)
	a_modes,				(* index_w_mode *)
	a_modes,				(* index_l_mode *)
	a_modes + [abs_w_mode, abs_l_mode],	(* abs_w_mode *)
	a_modes + [abs_l_mode],			(* abs_l_mode *)
	a_modes + pc_relative_modes,		(* pc_displacement_mode *)
	a_modes + [pc_index_w_mode, pc_index_l_mode], (* pc_index_w_mode *)
	a_modes + [pc_index_l_mode],		(* pc_index_l_mode *)
	register_modes + [immediate_mode],	(* immediate_mode *)
	[ ] );					(* null_mode *)

    coerce_sizes : array [op_sizes] of op_size_set =
      ( [size_byte, size_word, size_long],	(* size_byte *)
	[size_word, size_long],			(* size_word *)
	[size_long],				(* size_long *)
	[size_double],				(* size_double *)
	[no_size] );				(* no_size *)
$PAGE
var cd : op_desc;
    target_modes : addr_mode_set;
    target_sizes : op_size_set;
    min_size : op_sizes;
    best_mode : addr_mode;
    r : registers;
    rdesc : op_desc;

begin
  if (desc.mode in allowed_modes) and
     (desc.value_size in allowed_sizes) and
     (desc.signed_value >= sign_required) then begin
    coerce := desc;
    return;	(* <---- Got it right the first time. *)
  end;

  (*  Decide what size result we need, and what ultimate modes are
      possible, considering (1) the mode we have to start with,
      (2) the modes we are allowed to end up with, and (3) whether
      a size conversion will be necessary.  *)

  cd := desc;
  if cd.extended_size = no_size then
    cd.extended_size := cd.value_size;
  target_modes := coerces_to[cd.mode] * allowed_modes;
  target_sizes := coerce_sizes[cd.value_size] * allowed_sizes;
  if cd.signed_value < sign_required then	(* Must expand to provide sign bit *)
    target_sizes := target_sizes - [cd.value_size];
  if not (cd.value_size in target_sizes) then	(* Can't expand in memory *)
    target_modes := target_modes * [dreg_mode, areg_mode, immediate_mode];
  if target_modes = [areg_mode] then	(* Can't have a byte-size addr reg *)
    target_sizes := target_sizes - [size_byte];
  if target_sizes = [size_byte] then
    target_modes := target_modes - [areg_mode];
  if target_sizes - [size_double, no_size] = [] then	(* Can't load a double-word *)
    target_modes := target_modes * memory_modes;

  assert (target_modes <> []);
  assert (target_sizes <> []);

  for min_size := size_byte to no_size do
    exit if min_size in target_sizes;
$PAGE
  (*  If mode conversion is necessary, generate the code to do it.  *)

  if not (cd.mode in target_modes) then begin
    if preferred_mode in target_modes
      then best_mode := preferred_mode
      else best_mode := null_mode;


    (***************************************

	Converting an absolute word address to a long address is trivial.  *)

    if abs_l_mode in target_modes then begin
      assert (cd.mode = abs_w_mode);
      cd.mode := abs_l_mode;
    end


    (***************************************

	Coerce (An) to 0(An) or 0(An) to An if possible.  *)

    else if (cd.mode = indirect_mode) and (displacement_mode in target_modes) then
      cd.mode := displacement_mode

    else if ( (cd.mode = displacement_mode) andif
	      (cd.cst_part.kind = absolute_sc) andif
	      (cd.cst_part.offset = 0) ) and
	    (indirect_mode in target_modes) then
      cd.mode := indirect_mode


    (***************************************

	Put the result in a register if that is preferred, or if there is
	no alternative.  *)

    else if (best_mode in register_modes) or (target_modes - register_modes = []) then begin

      (*  Get the result register.  *)

      free_desc (cd);
      if (best_mode = areg_mode) or not (dreg_mode in target_modes) then
	r := get_areg ()
      else if not (areg_mode in target_modes) then
	r := get_dreg ()
      else
	r := get_xreg ();

      if cd.mode = immediate_mode then
	cd := loadr (r, cd.cst_part, min_size, sign_required)

      else begin
	rdesc := reg_desc (r, cd.value_size, true);	(* Assume simple load. *)
	if rdesc.mode = dreg_mode then
	  rdesc.signed_value := cd.signed_value;
	if (rdesc.mode = areg_mode) and
	   ( (cd.value_size = size_byte) or (not cd.signed_value) ) then begin
	  cd := coerce (duplicate_desc (cd), [dreg_mode], dreg_mode, word_min, true);
	  rdesc.value_size := cd.value_size;
	  rdesc.signed_value := true;
	end;
	if (rdesc.mode = dreg_mode) and
	   (cd.value_size <> min_size) and
	   (not cd.signed_value) and
	   (not r_in_desc (r, cd)) then
	  rdesc := loadi (r, 0, cd.value_size, true);
	gen_mm (move_opc, cd, rdesc);
	cd := rdesc;
      end;
    end


    (***************************************

	Turn a PC_INDEXED_W descriptor into a PC_INDEXED_L descriptor, or
	a PC_DISPLACEMENT descriptor into a PC_INDEXED descriptor, if a
	pc-relative mode is preferred, or if there is no alternative.  *)

    else if (best_mode in pc_relative_modes) or
	    (target_modes - register_modes - pc_relative_modes = []) then begin
      if cd.mode = pc_displacement_mode then begin
	cd.index_reg := get_xreg ();	(* Get an index register. *)
	if (best_mode = pc_index_l_mode) or
	   not (pc_index_w_mode in target_modes) then begin
	  rdesc := loadr (cd.index_reg, cd.cst_part, size_long, true);
	  cd.mode := pc_index_l_mode;
	end
	else begin
	  rdesc := loadr (cd.index_reg, cd.cst_part, size_word, true);
	  cd.mode := pc_index_w_mode;
	end;
	cd.cst_part := abs_zero;
      end

      else (* word index => long index *) begin
	assert ((cd.mode = pc_index_w_mode) and (pc_index_l_mode in target_modes));
	if cd.index_reg in data_reg_set then
	  gen_r (ext_opc, cd.index_reg, size_long);
	cd.mode := pc_index_l_mode;
      end;
    end


    (**************************************

	Turn any memory operand into some kind of address register relative
	operand.  *)

    else begin
      assert (cd.mode in control_modes);
      target_modes := target_modes * areg_relative_modes;
      assert (target_modes <> []);

      free_desc (cd);

      (*  Coerce (An) or d(An) to 0(An,Xi).  *)

      if (cd.mode in [indirect_mode, displacement_mode]) and
	      (target_modes * indexed_modes <> []) then begin
	use_reg (cd.reg, 1);
	cd.index_reg := get_xreg ();
	if (best_mode = index_l_mode) or
	   not (index_w_mode in target_modes) then begin
	  rdesc := loadr (cd.index_reg, cd.cst_part, size_long, true);
	  cd.mode := index_l_mode;
	end
	else begin
	  rdesc := loadr (cd.index_reg, cd.cst_part, size_word, true);
	  cd.mode := index_w_mode;
	end;
	cd.cst_part := abs_zero;
      end

      (*  Coerce d(An,Xi.W) to d(An,Xi.L).  *)

      else if (cd.mode = index_w_mode) and
	      ( (best_mode = index_l_mode) or
		(target_modes = [index_l_mode]) ) then begin
	if cd.index_reg in data_reg_set then
	  gen_r (ext_opc, cd.index_reg, size_long);
	cd.mode := index_l_mode;
      end

      (*  Coerce arbitrary operand to (An) with an LEA, then coerce further
	  if necessary.  *)

      else begin
	r := get_areg ();
	gen_mm (lea_opc, cd, reg_desc (r, size_long, true));
	cd.mode := indirect_mode;
	cd.reg := r;
	cd.index_reg := d0;
	cd.cst_part := abs_zero;
	if not (indirect_mode in target_modes) then
	  cd := coerce (cd, target_modes, best_mode, target_sizes, sign_required);
      end;
    end;
  end;

  assert (cd.mode in allowed_modes);
$PAGE
  (*  Now we have a descriptor with an allowed addressing mode.  The next
      step is to coerce its size if it isn't as long as it ought to be.  *)

  if not (cd.value_size in target_sizes) then begin
    assert (cd.mode in [dreg_mode, areg_mode, immediate_mode]);
    assert (cd.value_size < min_size);

    (*  Actual data extension is only necessary if the data are in a data
	register, and the register hasn't already been extended.  *)

    if (cd.mode = dreg_mode) and (cd.extended_size < min_size) then begin

      (*  Extend signed data with sign-extend.  *)

      if cd.signed_value or (cd.value_size <> cd.extended_size) then begin
	if cd.extended_size = size_byte then
	  gen_r (ext_opc, cd.reg, size_word);
	if min_size = size_long then
	  gen_r (ext_opc, cd.reg, size_long);
      end

      (*  Extend unsigned data with zeroes.  *)

      else begin
	if cd.value_size = size_word then begin
	  gen_r (swap_opc, cd.reg, size_word);
	  gen_r (clr_opc, cd.reg, size_word);
	  gen_r (swap_opc, cd.reg, size_word);
	end
	else begin
	  gen_ir (and_opc, #hff, cd.reg, size_word);
	  if min_size = size_long then
	    gen_r (ext_opc, cd.reg, size_long);
	end;
      end;
    end;

    cd.value_size := min_size;
    cd.extended_size := max (cd.extended_size, cd.value_size);
    cd.signed_value := true;
  end;

  assert (sign_required <= cd.signed_value);

  coerce := cd;
  coerce.known_positive := desc.known_positive;
end (* coerce *);
$PAGE make_postincr, pincr_free
(*  MAKE POSTINCR takes a descriptor and transforms it into postincrement mode.  *)

public function make_postincr (desc: op_desc): op_desc;

var areg: addr_regs;

begin
  if desc.mode = postincrement_mode then
    make_postincr := desc
  else begin
    make_postincr := coerce (desc, [indirect_mode], indirect_mode, any_size, false);
    if (uses_remaining (make_postincr.reg) > 1) or (make_postincr.reg in reserved_registers) then begin
      free_desc (make_postincr);
      areg := get_areg;
      gen_rr (move_opc, make_postincr.reg, areg, size_long);
      make_postincr.reg := areg
    end;
    make_postincr.mode := postincrement_mode
  end
end (* make_postincr *);


(* PINCR FREE determines if the descriptor could be converted to postincrement without
   instructions. *)

public function pincr_free (op: op_desc): boolean;

begin
  with op do
    pincr_free := (mode = postincrement_mode) or
		  ( ( (mode = indirect_mode) or
		      ( (mode = displacement_mode)
			      and (cst_part.kind = absolute_sc) and (cst_part.offset = 0) ) )
			  andif
		    (not (reg in reserved_registers) and (uses_remaining (reg) <= 1)) )
end;
$PAGE ref_descriptor
(*  REF DESCRIPTOR returns a descriptor which represents the address of the
    value represented by its input descriptor.  The returned descriptor will
    always be signed.  RefDescriptor transforms its input descriptor--
    that is, it transfers one use from its input descriptor to its
    output descriptor.  *)

public function ref_descriptor ( op : op_desc ) : op_desc;

begin
  free_desc (op);

  (*  Compute a descriptor for the address of the input descriptor.  *)

  case op.mode of

    abs_w_mode,
    abs_l_mode :			(* Addr <xxxx> = <#xxxx> *)
       begin
	ref_descriptor := descriptors[immediate_mode];
	ref_descriptor.cst_part := op.cst_part;
	if op.mode = abs_w_mode
	  then ref_descriptor.value_size := size_word
	  else ref_descriptor.value_size := size_long;
      end;

    indirect_mode :			(* Addr <(An)> = <An> *)
      begin
	use_reg (op.reg, 1);
	ref_descriptor := reg_desc (op.reg, size_long, true);
      end;

    others :				(* LEA op,An; then Addr <op> = <An> *)
      begin
	ref_descriptor := reg_desc (get_areg (), size_long, true);
	gen_mm (lea_opc, op, ref_descriptor);
      end;

  end;
end (* ref_descriptor *);
$PAGE deref_descriptor
(*  DEREF DESCRIPTOR returns a descriptor which represents the value
    located at the address represented by its input descriptor.  The
    size and sign of the returned value must be specified.  DerefDescriptor
    transforms its input operand--that is, it transfers one use from its
    input operand to its output operand.  *)

public function deref_descriptor ( op : op_desc;
				   size : op_sizes;
				   signed : boolean
						    ) : op_desc;

var c_op : op_desc;

begin

  (*  For accessing, we must have a signed word or long value which is
      immediate or is in an address register.  *)

  c_op := coerce (op, [areg_mode, immediate_mode], immediate_mode, word_min, true);

  (*  The value at address <An> is <(An)>; the value at address <#xxxx> is
      <xxxx>.  *)

  case c_op.mode of
    areg_mode :
      begin
	derscriptor := descriptors[indirect_mode];
	deref_descriptor.reg := c_op.reg;
      end;
    immediate_mode :
      begin
	if c_op.value_size <= size_word
	  then deref_descriptor := descriptors[abs_w_mode]
	  else deref_descriptor := descriptors[abs_l_mode];
	deref_descriptor.cst_part := c_op.cst_part;
      end;
  end;

  (*  Fill in the value attributes.  *)

  deref_descriptor.value_size := size;
  deref_descriptor.extended_size := size;
  deref_descriptor.signed_value := signed;
end (* deref_descriptor *);
$PAGE increment_addr
(*  INCREMENT ADDR takes an operand descriptor, frees it, and returns a new
    descriptor which has the same size attributes as the original one, but
    with an address DELTA bytes greater.  IncrementAddr transforms its
    input operand--that is, it transfers one use from its input operand
    to its output operand.  *)

public function increment_addr ( op : op_desc; delta : integer ) : op_desc;

var offset_plus : integer;
    offset_size : op_sizes;

begin
  assert (op.mode in control_modes);
  increment_addr := op;
  if delta = 0 then
    return;

  with increment_addr do begin
    offset_plus := cst_part.offset + delta;
    offset_size := int_size (offset_plus, true);

    (*  DELTA can be added directly to the constant offset in an absolute
	address.  *)

    if mode in [abs_w_mode, abs_l_mode] then
      cst_part.offset := offset_plus

    (*  A DISPLACEMENT or PC_DISPLACEMENT operand may be adjusted by adding DELTA
	to its constant offset, provided the result still fits in a word.  (An
	INDIRECT operand may be treated as a DISPLACEMENT operand with an offset
	of zero.)  *)

    else if (mode in [indirect_mode, displacement_mode, pc_displacement_mode]) and
	    (offset_size <= size_word) then begin
      if mode = indirect_mode then
	mode := displacement_mode;
      cst_part.offset := offset_plus;
      if (mode = displacement_mode) and
	 (offset_plus = 0) and
	 (cst_part.kind = absolute_sc) then
	mode := indirect_mode;
    end

    (*  An indexed operand may be adjusted by adding DELTA to its constant
	offset, provided that its constant part is absolute, and that the
	adjusted offset still fits in a byte.  *)

    else if (mode in indexed_modes) and
	    (cst_part.kind = absolute_sc) and
	    (offset_size = size_byte) then
      cst_part.offset := offset_plus

    (*  If there are no uses remaining on the index register, an indexed
	operand may always be adjusted by adding DELTA to the index register.  *)

    else if (mode in indexed_modes) andif (uses_remaining (index_reg) = 1) then begin
      make_index_long (increment_addr);
      cst_part.offset := offset_plus;
      gen_mr (add_opc, rel_desc (cst_part, size_long, true), index_reg);
      cst_part := abs_zero;
    end

    (*  So long as DELTA fits in a word, any operand may be adjusted by loading
	its address into an address register An, and returning <DELTA(An)>.  *)

    else if int_size (delta, true) <= size_word then begin
      free_desc (op);
      mode := displacement_mode;
      reg := get_areg ();
      index_reg := d0;
      cst_part := (delta, absolute_sc);
      gen_mm (lea_opc, op, reg_desc (reg, size_long, true));
    end

    (*  If DELTA will not fit in a word, and the operand is <d(An)> (treating
	<(An)> as <0(An)>) then it may be adjusted by loading DELTA+d into a
	register Xi, and returning <0(An,Xi.L)>.  *)

    else if mode in [indirect_mode, displacement_mode] then begin
      mode := index_l_mode;
      index_reg := get_xreg ();
      cst_part.offset := offset_plus;
      gen_mr (move_opc, rel_desc (cst_part, size_long, true), index_reg);
      cst_part := abs_zero;
    end

    (*  All else failing, load the address of the operand into a register Am,
	add DELTA to Am, and return <(Am)>.  *)

    else begin
      free_desc (op);
      mode := indirect_mode;
      reg := get_areg ();
      index_reg := d0;
      cst_part := abs_zero;
      gen_mm (lea_opc, op, reg_desc (reg, size_long, true));
      gen_ir (add_opc, delta, reg, size_long);
    end;

  end (* with increment_addr *);
end (* increment_addr *);
$PAGE select_index_mode
(*  SELECT INDEX MODE is called with a register-mode operand descriptor and a
    base mode which is INDIRECT_MODE, DISPLACEMENT_MODE, or PC_DISPLACEMENT_MODE.
    It returns the proper mode for indexing the base mode with the described
    register.  *)

function select_index_mode ( rd : op_desc; base_mode : addr_mode ) : addr_mode;

begin
  assert (rd.mode in register_modes);
  assert (base_mode in [indirect_mode, displacement_mode, pc_displacement_mode]);
  if base_mode = pc_displacement_mode then begin
    if rd.value_size = size_long
      then select_index_mode := pc_index_l_mode
      else select_index_mode := pc_index_w_mode;
  end
  else begin
    if rd.value_size = size_long
      then select_index_mode := index_l_mode
      else select_index_mode := index_w_mode;
  end;
end (* select_index_mode *);
$PAGE offset_addr
(*  OFFSET ADDR takes a pair of operand descriptors, frees them, and returns
    a new descriptor which has the same size attributes as the first one,
    but with an address incremented by the value of the second one.  OffsetAddr
    transforms OP and consumes DELTA--that is, it transfers one use from OP
    to its output operand, and frees DELTA.  *)

public function offset_addr ( op : op_desc; delta : op_desc ) : op_desc options assembly;

var cst_delta : integer;
    delta_op : op_desc;
    byte_offset : boolean;
    xr : registers;

begin

  (*  The simplest case:  if DELTA is an integer constant, this simply reduces
      to INCREMENT_ADDR.  *)

  if aconstp (delta, cst_delta) then begin
    offset_addr := increment_addr (op, cst_delta);
    return;				(* <---- Early exit in constant case.  *)
  end;

  (*  To do anything interesting, we must have an address register relative
      or pc relative operand to work with.  *)

  offset_addr := coerce (op, areg_relative_modes, indirect_mode, any_size, false);

  with offset_addr do begin
    byte_offset := (cst_part.kind = absolute_sc) and
		   (int_size (cst_part.offset, true) = size_byte);

    (*  If the base address is not indexed, its displacement can be represented
	in a byte, and DELTA is in a register, then return an address indexed
	by the DELTA register.  *)

    if not (mode in indexed_modes) and
       (delta.mode in register_modes) and
       byte_offset then begin
      delta_op := coerce (delta, all_modes, null_mode, word_min, true);
      mode := select_index_mode (delta_op, mode);
      index_reg := delta_op.reg;
    end

    (*  If the address register has no more uses, then simply add DELTA to it.  *)

    else if uses_remaining (reg) = 1 then begin
      delta_op := coerce (delta, all_modes, dreg_mode, word_min, true);
      gen_mr (add_opc, delta_op, reg);
      free_desc  (delta_op);
    end

    (*  If the base address is indexed, and the index register has no more
	uses, then simply add DELTA to it.  *)

    else if (mode in indexed_modes) andif (uses_remaining (index_reg) = 1) then begin
      delta_op := coerce (delta, all_modes, dreg_mode, long_only, true);
      make_index_long (offset_addr);
      gen_mr (add_opc, delta_op, index_reg);
      free_desc (delta_op);
    end

    (*  If the displacement is in a register, then get the base address in an
	address register and return an indexed reference.  *)

    else if delta.mode in register_modes then begin
      offset_addr := coerce (offset_addr, [indirect_mode], indirect_mode, any_size, false);
      delta_op := coerce (delta, all_modes, dreg_mode, word_min, true);
      mode := select_index_mode (delta_op, mode);
      index_reg := delta_op.reg;
    end

    (*  If the base address is already indexed, then copy the index register
	to a new register and add DELTA to the new index register.  *)

    else if mode in indexed_modes then begin
      if (mode = index_w_mode) and (index_reg in data_reg_set) then begin
	xr := get_xreg ();
	gen_rr (move_opc, index_reg, xr, size_word);
	if xr in data_reg_set then
	  gen_r (ext_opc, xr, size_long);
	delta_op := coerce (delta, all_modes, dreg_mode, long_only, true);
	gen_mr (add_opc, delta_op, xr);
	free_desc (delta_op)
      end
      else begin
	delta_op := coerce (delta, register_modes, dreg_mode, long_only, true);
	if uses_remaining (delta_op.reg) = 1 then
	  xr := delta_op.reg
	else begin
	  xr := get_xreg ();
	  gen_mr (move_opc, delta_op, xr);
	  free_desc (delta_op)
	end;
	gen_rr (add_opc, index_reg, xr, size_long);
      end;
      free_reg (index_reg);
      index_reg := xr;
      if mode = index_w_mode then
	mode := index_l_mode;
    end

    (*  If the base address is not indexed, then allocate an index register,
	load DELTA into it, and change the address mode to indexed.  It may
	also be necessary to add the displacement from the base address into
	the index register.  *)

    else begin
      delta_op := coerce (delta, register_modes, dreg_mode, word_min, true);
      mode := select_index_mode (delta_op, mode);
      index_reg := delta_op.reg;
      if not byte_offset then begin
	gen_mm (add_opc, rel_desc (cst_part, delta_op.value_size, true), delta_op);
	cst_part := abs_zero;
      end;
    end;

  end (* with offset_addr *);
end (* offset_addr *);
$PAGE pushi
(*  PUSHI generates code to push an integer value on the stack with a
    specified size.  *)

public procedure pushi ( i : integer; size : op_sizes );

var push_desc, source_desc : op_desc;

begin
  assert (size in reg_sizes);
  push_desc := (predecrement_mode, size, size, true, true, sp, d0, abs_zero);
  if i = 0 then
    gen_m (clr_opc, push_desc)
  else if int_size (i, true) = size_byte then begin
    source_desc := loadi (get_dreg (), i, size, true);
    gen_mm (move_opc, source_desc, push_desc);
    free_desc (source_desc);
  end
  else if size = size_long then
    gen_m (pea_opc, deref_descriptor (int_desc (i, no_size, true), no_size, true))
  else
    gen_mm (move_opc, int_desc (i, no_size, false), push_desc);
end (* pushi *);
$PAGE push
(*  PUSH generates code to push an operand on the stack.  The size of the
    value to be placed on the stack may be specified; if it is NO_SIZE,
    then the inherent size of the operand will be used.  Push frees its
    input operand.  *)

public procedure push ( op : op_desc; size : op_sizes );

var p_op, push_desc : op_desc;

begin
  if (op.mode = postincrement_mode) andif (op.reg = sp) andif
     ( (size = no_size) or (op.value_size = size) ) then
    (* the operand is already on the stack! *)
  else begin
    if size = no_size then
      p_op := op
    else
      p_op := coerce (op, all_modes, null_mode, [size], false);
    assert (p_op.value_size <> no_size);
    push_desc := (predecrement_mode, p_op.value_size, no_size,
		  p_op.signed_value, false, sp, d0, abs_zero);
    if (p_op.mode = immediate_mode) andif (p_op.cst_part.kind = absolute_sc) then
      pushi (p_op.cst_part.offset, p_op.value_size)
    else if p_op.value_size <= size_long then
      gen_mm (move_opc, p_op, push_desc)
    else begin
      assert (p_op.value_size = size_double);
      p_op.value_size := size_long;
      push_desc.value_size := size_long;
      p_op := increment_addr (p_op, 4);	(* Push the low-order half first. *)
      gen_mm (move_opc, p_op, push_desc);
      p_op := increment_addr (p_op, -4);
      gen_mm (move_opc, p_op, push_desc);
    end;
    free_desc (p_op);
  end;
end (* push *);
$PAGE pusha
(*  PUSHA generates code to push the address of an operand on the stack, and
    frees the operand descriptor.  *)

public procedure pusha ( op : op_desc );

begin
  gen_m (pea_opc, op);
  free_desc (op);
end (* pusha *);
$PAGE stk_bash
(*  STK BASH is the most important part of the GUARD/UNGUARD package.  It
    must be called before any dynamic temporary is allocated on the stack.
    Its effect is to traverse the guarded operand stack, finding any
    operands which are on the stack and popping them into static temps.  *)

public procedure stk_bash;

var traverse : guard_key;
    temp_op, temp_dest : op_desc;

const
    size_size : array [op_sizes] of 0..8 = (1, 2, 4, 8, 0);

begin
  traverse := guard_stack;
  while traverse <> nil do begin
    with traverse^ do begin
      if (op.mode = postincrement_mode) andif (op.reg = sp) then begin
	temp_op := get_temp (int_desc (size_size [op.value_size], no_size, false), false (* byte cnt *));
	temp_op.known_positive := op.known_positive;
	if op.value_size <= size_long then
	  gen_mm (move_opc, op, temp_op)
	else begin
	  temp_op.value_size := size_double;
	  temp_dest := temp_op;
	  temp_dest.value_size := size_long;
	  op.value_size := size_long;
	  gen_mm (move_opc, op, temp_dest);
	  temp_dest := increment_addr (duplicate_desc (temp_dest), 4);
	  gen_mm (move_opc, op, temp_dest);
	end;
	op := temp_op;
      end;
      traverse := next;
    end (* with traverse^ *);
  end (* while traverse <> nil *);
end (* stk_bash *);
$PAGE copy_dreg
(*  COPY DREG ensures that a given operand value will be in a data register
    that may safely be trashed.  CopyDreg transforms its input operand--
    that is, it transfers one use from its input operand to its output
    operand.  *)
public function copy_dreg ( source_desc : op_desc ) : op_desc;

begin

  (*  If the operand isn't in a data register, then copy it into one --
      the resulting register must be unique.  *)

  if source_desc.mode <> dreg_mode then
    copy_dreg := coerce (source_desc, [dreg_mode], dreg_mode,
			 any_size, source_desc.signed_value)

  (*  If the operand is in a data register which has only one remaining use,
      then we can trash it with impunity.  *)

  else if uses_remaining (source_desc.reg) = 1 then
    copy_dreg := source_desc

  (*  If the operand is in a data register which has multiple remaining uses,
      we must copy it into another register.  *)

  else begin
    copy_dreg := source_desc;
    copy_dreg.reg := get_dreg ();
    free_desc (source_desc);
    if copy_dreg.extended_size <> no_size then
      copy_dreg.value_size := copy_dreg.extended_size;
    gen_mm (move_opc, source_desc, copy_dreg);
    copy_dreg.value_size := source_desc.value_size;
  end;
end (* copy_dreg *);
$PAGE power_of_two
(*  POWER OF TWO will determine whether the integer X is a positive power of
    two.  If it is, it will return the value N such that X = 2**N.  *)

function power_of_two ( x : integer; var n : integer ) : boolean;

var y : integer;

begin
  n := 0;
  y := 1;
  while (n <= 30) and (y < x) do begin
    n := n + 1;
    y := y + y;
  end;
  power_of_two := (x = y);
end (* power_of_two *);
$PAGE force_word_size
(*  FORCE WORD SIZE is called with an operand descriptor for an expression
    which is known to have an intrinsic size of word or byte.  It returns
    a descriptor of size word for the expression.  If the expression's size
    is word or byte, this simply requires a coercion.  If its size is long,
    the expression must have its address incremented (if it has a control
    mode), or be loaded into a register, and be marked as having size word.
    This routine tyransforms its input descriptor -- that is, it transfers
    one use from the input operand to the output operand.  *)

function force_word_size ( op : op_desc ) : op_desc;

begin
  if op.value_size in word_max then
    force_word_size := coerce (op, all_modes, null_mode, word_only, false)
  else begin
    assert (op.value_size = size_long);
    if op.mode in control_modes then
      force_word_size := increment_addr (op, 2)
    else
      force_word_size := coerce (op, register_modes, dreg_mode, any_size, false);
    force_word_size.value_size := size_word;
  end;
end (* force_word_size *);
$PAGE mul_cst
(*  MUL CST takes a descriptor for an integer value and a positive integer
    constant.  It frees the input descriptor, and returns a descriptor
    representing the value of the input descriptor, multiplied by the
    integer constant.  Possible cases for the return value are:
    (1) If the input operand is constant, then the returned operand
    will be as well.  (2) If the multiplier is one, the input operand
    will be returned unmodified.  (3) If the multiplication can be
    performed in a register, then the returned operand will be a
    minimum-size data register.  (4) If a call to the runtime integer
    multiplication routine is required, the the returned operand will
    be <(SP)+>.  The OP_SIZE and OP_SIGNED parameters specify known constraints
    on the OP parameter.  For example, a variable declared "var x : 1..100"
    will have a size_word operand descriptor, but OP_SIZE will be size_byte.
    RSLT_SIZE specifies a constraint, if one is known, for the result of the
    multiplication; if it is no_size, then it may be ignored.  *)

public function mul_cst ( op : op_desc;
			  factor : integer;
			  op_size : op_sizes;
			  op_signed : boolean;
			  rslt_size : op_sizes
					       ) : op_desc;

var cst_op : integer;			(* the operand value, if constant *)
    work_size : op_sizes;		(* tentative result size for computation *)
    mul_sizes : op_size_set;		(* possible result sizes *)
    shift_count : integer;		(* computed shift count *)
    shift_op : op_desc;			(* the shift count operand *)
    need_sign : boolean;		(* true if op bit 15 = sign bit *)
    multiplier : op_desc;		(* to be multiplied with data reg *)

begin
  assert (factor >= 0);

  need_sign := op_signed and not op.known_positive;

  (*  If the operand is constant, perform the computation at compile time.  *)

  if aconstp (op, cst_op) then
    mul_cst := int_desc (cst_op * factor, no_size, false)

  (*  Anything times zero is zero.  *)

  else if factor = 0 then
    mul_cst := int_desc (0, no_size, false)

  (*  If the multiplier is one, simply return the operand.  *)

  else if factor = 1 then
    mul_cst := op

  (*  If the multiplier is two, add the operand to itself.  *)

  else if factor = 2 then begin
    work_size := min (succ (op_size), rslt_size, size_long);
    mul_cst := coerce (op, [dreg_mode], dreg_mode, at_least [work_size], false);
    mul_cst := copy_dreg (mul_cst);
    mul_cst.value_size := work_size;
    gen_mm (add_opc, mul_cst, mul_cst);
    if rslt_size = no_size then
      mul_cst.signed_value := true;	(* Otherwise, will be set on return. *)
  end

  (*  If the multiplier is a power of two, then the multiplication can
      be performed with a shift.  *)

  else if power_of_two (factor, shift_count) then begin
    if (op_size = size_byte) and (shift_count <= 8)
      then work_size := size_word
      else work_size := size_long;
    work_size := min (work_size, rslt_size);
    mul_cst := coerce (op, [dreg_mode], dreg_mode, at_least [work_size], true);
    mul_cst := copy_dreg (mul_cst);
    mul_cst.value_size := work_size;
    if shift_count <= 8
      then shift_op := int_desc (shift_count, size_byte, true)
      else shift_op := loadi (get_dreg (), shift_count, size_byte, true);
    gen_mm (asl_opc, shift_op, mul_cst);
    free_desc (shift_op);
    if rslt_size = no_size then begin
      if (op.value_size = size_byte) and (shift_count = 8) then
	mul_cst.signed_value := op.signed_value
      else
	mul_cst.signed_value := true;
    end;
  end

  (*  If the multiplier and the operand can each be represented in a
      word, then the hardware multiply instruction can be used.  *)

  else if ( (op_size <= size_word) andif
	    (int_size (factor, true) <= size_word) )or
	  ( (rslt_size <= size_word) andif
	    (int_size (factor, true) <= size_word) ) then begin
    if op_size > size_word then
      need_sign := true;
    if int_size (factor, true) = size_byte then begin
      mul_cst := loadi (get_dreg (), factor, size_word, need_sign);
      multiplier := force_word_size (op);
    end
    else begin
      mul_cst := copy_dreg (force_word_size (op));
      multiplier := int_desc (factor, size_word, need_sign);
    end;
    if need_sign
      then gen_mm (muls_opc, multiplier, mul_cst)
      else gen_mm (mulu_opc, multiplier, mul_cst);
    free_desc (multiplier);
    mul_cst.value_size := size_long;
    mul_cst.extended_size := size_long;
    mul_cst.signed_value := true;
  end

  (*  All else failing, use the software 32-bit integer multiply.  *)

  else begin
    push (op, size_long);
    pushi (factor, size_long);
    gen_rt (rt_imul);
    mul_cst := pop_long;
  end;

  mul_cst.known_positive := op.known_positive;
end (* mul_cst *);
$PAGE load_frame_pointer
(*  LOAD FRAME POINTER chases the static chain for non-local references.
    ACCESS_BLOCK is the block whose frame pointer is to be made accessible.
    The return value will be the number of an address register which will
    contain the frame pointer for that block.  *)

public function load_frame_pointer ( access_block : blk ) : addr_regs;

var levels, i : integer;
    access : op_desc;

const
    static_link = 20;

begin
  levels := cur_block^.apparent_level - access_block^.apparent_level;
  if levels = 0 then
    load_frame_pointer := fp
  else begin
    load_frame_pointer := get_areg ();
    access := descriptors[displacement_mode];
    access.reg := fp;
    access.cst_part.offset := static_link;
    access.value_size := size_long;
    for i := 1 to levels do begin
      gen_mr (move_opc, access, load_frame_pointer);
      access.reg := load_frame_pointer;
    end;
  end;
end (* load_frame_pointer *);
$PAGE upper_bound
(*  UPPER BOUND is passed an expression tuple for an array  or string and,
    optionally, an operand descriptor for the base of the array or string
    (for a dynamic flexible array or string, it is for the hidden length
    word).  For non-flexible references, an immediate operand is returned.
    For flexible references, an operand descriptor for the hidden length
    word is returned.  If a non-null operand descriptor is passed, it will
    be duplicated so that it will still be valid on return.  *)

public function upper_bound ( exp : expr; array_addr : op_desc ) : op_desc;

var
  str_desc: string_descriptor;

begin
  with exp^ do begin
    
    (* If array/string is not flexible then create an address
       descriptor for the constant upper bound.  *)

    if ( (desc.kind = strings) andif (not desc.str_flex) ) orif
       ( not desc.base^.flexible ) then begin
      with exp^.desc do
	if kind = strings
	  then upper_bound := int_desc ( str_length, size_byte, false )
	  else upper_bound := int_desc ( base^.index_type^.maxval, size_byte, true );
      if array_addr.mode = null_mode then
	dec_expr_usage (exp)
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
	if array_addr.mode = null_mode then begin
	  if desc.kind = strings then begin
	    str_desc := fetchstring (exp, max_length, no_limit);
	    upper_bound := duplicate_desc (str_desc.len_addr);
	    free_string (str_desc);
	  end
	  else upper_bound := fetch (exp, memory_modes, null_mode, any_size, false)
	end
	else
	  upper_bound := duplicate_desc (array_addr);
      end

      (* If opcode is ident_ref, then we have a formal parameter.  The
	 upperbound is the argument preceeding the address of the array.
	 The address of the first element of the array should be of the
	 form: 'offset(reg)' ; then the address of the upperbound is
		'offset-str_upb_bytes(reg)' for strings, and
		'offset-arr_upb_bytes(reg)' for arrays.	*)

      else if opcode = ident_ref then begin
	if array_addr.mode = null_mode then
	  dec_expr_usage (exp);
	upper_bound := descriptors[displacement_mode];
	upper_bound.cst_part := sym_reloc (id_sym);
	upper_bound.reg := load_frame_pointer (id_sym^.block);
	if desc.kind = strings
	  then upper_bound := increment_addr ( upper_bound, -str_upb_bytes )
	  else upper_bound := increment_addr ( upper_bound, -arr_upb_bytes );
      end
      else begin
	assert ( false );
      end;


      (* For strings an address descriptor with type SIZE_WORD is returned;
	 for arrays type SIZE_LONG is returned.  *)

      if desc.kind = strings then begin
	upper_bound.value_size := size_word;
	upper_bound.extended_size := size_word;
	upper_bound.signed_value := false;
	upper_bound.known_positive := true;
      end
      else begin
	upper_bound.value_size := size_long;
	upper_bound.extended_size := size_long;
	upper_bound.signed_value := true;
	upper_bound.known_positive := false;
      end;
    end  (* if flexible *) ;
  end  (* with exp^ *) ;
end (* upper_bound *);
$PAGE calc_width
(*  CALC_WIDTH computes the size of a variable of a specified type with,
    optionally, a specified upper bound parameter.  It transfers one use
    from the UPB descriptor to its result descriptor.  *)

public function calc_width ( agg_type : typ; upb : op_desc ) : op_desc;

var a, b, c, d : integer;
    grotesque : boolean;

begin
  size_of (agg_type, false, a, b, c, d, grotesque);
  assert ( (not grotesque) and (c = 1) );

  (*  WIDTH = (A * upb) + B.  *)

  if a <> 0 then begin
    calc_width := mul_cst (upb, a, size_long, true, no_size);
    if b <> 0 then begin
      calc_width := copy_dreg (calc_width);
      if b > 0
	then gen_im (add_opc, b, calc_width)
	else gen_im (sub_opc, -b, calc_width);
    end;
  end
  else begin
    assert (upb.mode = null_mode);
    calc_width := int_desc (b, no_size, false);
  end;
end (* calc_width *);
$PAGE do_fetch
(*  DO FETCH contains the actual expression fetching logic for FETCH.  It is
    called with an expression tuple and a collection of attributes, and returns
    an operand descriptor for the expression which satisfies the attributes.
    DO_FETCH is only a selector routine for a very large collection of routines
    which fetch particular kinds of expressions.  All of these routines are
    local to DO_FETCH, and make non-local references to its parameters, in
    order to avoid interminable duplication of the DO_FETCH parameter list.  *)

public function do_fetch ( e : expr;
		    allowed_modes : addr_mode_set;
		    preferred_mode : addr_mode;
		    allowed_sizes : op_size_set;
		    sign_required : boolean
					    ) : op_desc;
$PAGE exprsize - in do_fetch
(*  EXPRSIZE returns the operand size required for a specified scalar, real,
    or pointer expression.  *)

function exprsize ( e : expr ) : op_sizes;

begin
  with e^.desc do begin
    case kind of
      ints, bools, chars, scalars, pointers, files :
	if int_prec <= 8 then
	  exprsize := size_byte
	else if int_prec <= 16 then
	  exprsize := size_word
	else
	  exprsize := size_long;
      reals :
	if precision = srealprec then
	  exprsize := size_long
	else if precision = maximum (prec_type) then
	  exprsize := size_double
	else
	  assert (false);
    end;
  end;
end (* exprsize *);
$PAGE exprsigned
(*  EXPRSIGNED returns true if the expression E, represented in size
    EXPRSIZE(E), will have room for a sign bit, and false otherwise.
    I.e., if OP is a descriptor for E, and OP.VALUE_SIZE = EXPRSIZE(E),
    then OP.SIGNED_VALUE = EXPRSIGNED(E).  E must be scalar or integer.  *)

function exprsigned ( e : expr ) : boolean;

begin
  with e^.desc do begin
    assert (kind in [ints, bools, chars, scalars]);
    exprsigned := signed or ( (int_prec <> 8) and (int_prec <> 16) );
  end;
end (* exprsigned *);
$PAGE signed_exprsize
(*  SIGNED EXPRSIZE is the same as EXPRSIZE, except that it returns the operand
    size required for a specified scalar, subject to the requirement that the
    size include room for a sign bit.  E must be integer or scalar.  *)

function signed_exprsize ( e : expr ) : op_sizes;

var signed_prec : align_range;

begin
  with e^.desc do begin
    assert (kind in [ints, bools, chars, scalars]);
    signed_prec := int_prec + ord (not (signed));
    if signed_prec <= 8 then
      signed_exprsize := size_byte
    else if signed_prec <= 16 then
      signed_exprsize := size_word
    else
      signed_exprsize := size_long;
  end;
end (* signed_exprsize *);
$PAGE push_two_long - in do_fetch
(*  PUSH TWO LONG takes two longword operands, OP1 and OP2, and pushes them
    on the stack in that order if COMMUTATIVE is false, or in either order
    if COMMUTATIVE is true.  This is trivial unless COMMUTATIVE is false,
    OP2 is already on the stack, and OP1 is not, in which case OP2 must be
    moved to make room for OP1.  *)

procedure push_two_long ( op1, op2 : op_desc; commutative : boolean );

var op2a : op_desc;

begin
  if op2.mode <> postincrement_mode then begin
    push (op1, size_long);
    push (op2, size_long);
  end

  else if op1.mode = postincrement_mode then begin
    if op1.value_size = size_long then
      push (op2, size_long)
    else begin
      op2a := coerce (op2, nonstack_modes, null_mode, long_only, true);
      push (op1, size_long);
      push (op2a, size_long);
    end;
  end

  else begin
    if (op2.value_size = size_long) and commutative then
      push (op1, size_long)
    else begin
      op2a := coerce (op2, nonstack_modes, null_mode, long_only, true);
      push (op1, size_long);
      push (op2a, size_long);
    end;
  end;
end (* push_two_long *);
$PAGE fetch_ident_ref
(*  FETCH IDENT REF computes an operand descriptor for an IDENT_REF expression
    tuple.  *)

procedure fetch_ident_ref;

var
  sd: def;

begin
  with e^, id_sym^ do begin
    case kind of

      (*  Variables and parameters may always be accessed with an operand of
	  the form d(An).  The displacement is simply the symbol address field;
	  the address register is the static base for external and static
	  symbols, and the appropriate frame pointer for local and parameter
	  symbols.  (A parameter which has been passed by address is actually
	  the address of the parameter, and must be dereferenced.)
	  Conditions are always accessed as a displacement off A5.  *)

      vars,
      values,
      conditions :
	begin
	  do_fetch := descriptors[displacement_mode];
	  do_fetch.cst_part := sym_reloc (id_sym);
	  case dcl_class of
	    static_sc,
	    external_sc : begin
	      do_fetch.reg := bp;
	      do_fetch.cst_part.offset := -32768
	    end;
	    local_sc :
	      do_fetch.reg := load_frame_pointer (block);
	    parameter_sc :
	      begin
		do_fetch.reg := load_frame_pointer (block);
		if passed_by_address (id_sym) then begin
		  do_fetch.value_size := size_long;
		  do_fetch := deref_descriptor (do_fetch, no_size, false);
		end;
	      end;
	  end;
	end;

      (*  Named constants are emitted in the code section.  External ones
	  must be accessed with a long absolute address.  Local constants
	  may be accessed as d(PC); however, if the displacement is more
	  than 16 bits, FixBranches will have to change the descriptor to
	  a long absolute address.  *)

      consts :
	begin
	  case dcl_class of
	    external_sc :
	      begin
		do_fetch := descriptors[abs_l_mode];
		do_fetch.cst_part := sym_reloc (id_sym);
	      end;
	    constant_sc :
              if init_value.kind = subr_cst then begin
		(* Note - this can only happen due to "address (proc or func)" - in all
		   other cases procedures and functions are referenced by cst_ref's, so
		   see fetch_cst_ref for a comment explaining the setup.  *)
                sd := def_lookup (subr_def, init_value.blkp^.number);
                do_fetch := rel_desc (def_reloc (sd), size_double, true);
		do_fetch.extended_size := no_size;
		if sd^.defined then
		  do_fetch.mode := pc_displacement_mode
		else
		  do_fetch.mode := abs_l_mode
	      end
	      else begin
		do_fetch := descriptors[pc_displacement_mode];
		do_fetch.cst_part := gen_cval (init_value, type_desc);
	      end;
	  end;
	end;

    end;

    (*  Set the operand size and sign, according to its type, for a variable,
	parameter, or constant.  *)

    if kind in [vars, values, consts] then
      ops_type (do_fetch, type_desc, false)
    else
      do_fetch.value_size := size_word;

  end (* with e^, id_sym^ *);
end (* fetch_ident_ref *);
$PAGE fetch_field_ref
(*  FETCH FIELD REF computes an operand descriptor for a FIELD_REF expression
    tuple.  *)

procedure fetch_field_ref;

begin
  with e^ do begin

    (*  Get the base address of the record.  *)

    do_fetch := fetch (base_rec, control_modes, indirect_mode, any_size, false);

    (*  Increment the record address by the offset of the field within it.  *)

    do_fetch := increment_addr (do_fetch, field_sym^.fld_offset div bits_per_byte);

    (*  Set the field size attributes.  *)

    ops_type (do_fetch, field_sym^.type_desc, base_rec^.desc.base^.packable);

    (* Watch out for slack allocation. *)

    if (do_fetch.value_size in word_max) and (field_sym^.fld_width = bits_per_long) then
      do_fetch.value_size := size_long
    else if (do_fetch.value_size = size_byte) and (field_sym^.fld_width = bits_per_word) then
      do_fetch.value_size := size_word
  end;
end (* fetch_field_ref *);
$PAGE fetch_ptr_ref
(*  FETCH PTR REF computes an operand descriptor for a PTR_REF expression
    tuple.  *)

procedure fetch_ptr_ref;

begin
  with e^ do begin

    (*  Fetch the pointer expression that is to be dereferenced.  *)

    do_fetch := fetch (base_ptr, [areg_mode, immediate_mode], immediate_mode,
		       word_min, true);

    (*  Dereference it.  *)

    do_fetch := deref_descriptor (do_fetch, no_size, false);

    (*  Set the operand size attributes.  *)

    ops_expr_type_desc (do_fetch, desc, false);

  end;
end (* fetch_ptr_ref *);
$PAGE fetch_array_ref
(*  FETCH ARRAY REF computes an operand descriptor for an ARRAY_REF expression
    tuple.  *)

procedure fetch_array_ref;

var elem_size : integer;		(* element size in bytes *)
    offset : integer;			(* offset of array[0] from array base address *)
    index : op_desc;			(* index expression operand *)
    cst_index : integer;		(* index expression value, if constant *)

begin
  with e^ do begin
    with base_array^.desc.base^ do begin
      elem_size := element_size div bits_per_byte;
      offset := - elem_size * index_type^.minval;
    end;
    if dynamic_flex (base_array) then
      offset := offset + arr_upb_bytes;

    (*  Fetch the base array and the index expression.  *)

    index := fetch (index_val, all_modes, immediate_mode, any_size, false);
    do_fetch := fetch (base_array, control_modes, indirect_mode, any_size, false);

    (*  If the index expression is constant, we can access the desired
	element directly by incrementing the array base address.  *)

    if aconstp (index, cst_index) then
      do_fetch := increment_addr (do_fetch, offset + (cst_index * elem_size))

    (*  Otherwise, it will be necessary to multiply the index value by the
	element size and use it to adjust the array address.  *)

    else begin
      do_fetch := increment_addr (do_fetch, offset);
      index := mul_cst (index, elem_size, exprsize (index_val), index_val^.desc.signed, no_size);
      do_fetch := offset_addr (do_fetch, index);
    end (* nonconstant index *);

    ops_expr_type_desc (do_fetch, desc, base_array^.desc.base^.packable);
  end (* with e^ *);
end (* fetch_array_ref *);
$PAGE fetch_substr_ref
(*  FETCH SUBSTR REF computes an operand descriptor for a SUBSTR REF expression
    tuple.  We will assume that the substring reference was generated for a
    string[index] reference, i.e., that it is really a subscript reference.  *)

procedure fetch_substr_ref;

var index : op_desc;			(* index expression operand *)
    cst_index : integer;		(* index expression value, if constant *)
    str_desc: string_descriptor;	(* base string expression value *)

begin
  with e^ do begin
    assert (desc.kind = chars);

    (*  The code here is very similar to that for FETCH_ARRAY_REF, but
	much simpler.  *)

    (*  Fetch the base string and the index expression.  *)

    index := fetch (substr_index, all_modes, immediate_mode, any_size, false);
    str_desc := fetchtranslated ( base_string, actual_length );
    do_fetch := skip_len_words ( str_desc );	(* want address of text itself *)
    free_string ( str_desc );

    (*  If the index expression is constant, we can access the desired character
	directly.  *)

    if aconstp (index, cst_index) then
      do_fetch := increment_addr (do_fetch, cst_index - 1)


    (*  Otherwise, use OFFSET_ADDR to adjust the base address.  *)

    else begin
      do_fetch := offset_addr (do_fetch, index);
      do_fetch := increment_addr (do_fetch, -1);
    end;

    (* The first pass always supplies a substring length in the IF,
       even though that length must be a constant 1 for a character.
       In order to keep the usage_counts straight we must decrement
       that count here even though the tuple is unused. *)

    dec_expr_usage (substr_length);
  end (* with e^ *);

  ops_type (do_fetch, type_char, false);
  do_fetch.extended_size := no_size;
end (* fetch_substr_ref *);
$PAGE fetch_buffer_ref
(*  FETCH BUFFER REF computes an operand descriptor for a BUFFER_REF expression
    tuple.  *)

procedure fetch_buffer_ref;

begin
  with e^ do begin

    (*  Fetch the file expression.  *)

    do_fetch := fetch (base_file, [areg_mode, immediate_mode], immediate_mode,
		       word_min, true);

    (*  Dereference once to get the file block; then dereference the first
	word of the file block to get the buffer.  *)

    do_fetch := deref_descriptor (do_fetch, size_long, true);
    do_fetch := deref_descriptor (do_fetch, no_size, false);

    (*  Set the operand size attributes.  *)

    ops_expr_type_desc (do_fetch, desc, false);

  end;
end (* fetch_buffer_ref *);
$PAGE fetch_cst_ref
(*  FETCH CST REF computes an operand descriptor for a CST_REF expression
    tuple.  *)

procedure fetch_cst_ref;

var sd : def;

begin
  with e^, cst_val do begin

    (*  Unless we must have a memory reference, we can generate an immediate
	reference for a scalar or pointer constant.  *)

    if not (allowed_modes <= control_modes) and
       not (preferred_mode in pc_relative_modes + [abs_w_mode, abs_l_mode]) and
       (kind in [scalar_cst, ptr_cst]) then begin
      if kind = ptr_cst
	then do_fetch := int_desc (int_nil, no_size,  false)
	else do_fetch := int_desc (ival, no_size, false);
    end

    (*  For a subroutine constant, we want to return a descriptor which
	accesses the starting instruction of the subroutine (NOT the
	address of the starting instruction).  This allows us to return
	a pc-relative reference for a subroutine.  Obviously, the
	subroutine call and subroutine variable assignment routines must
	understand this.  *)

    else if kind = subr_cst then begin
      sd := def_lookup (subr_def, blkp^.number);
      do_fetch := rel_desc (def_reloc (sd), no_size, true);
      if sd^.defined then
	do_fetch.mode := pc_displacement_mode
      else
	do_fetch.mode := abs_l_mode;
    end

    (*  Generate an immediate zero word for a null set constant.  *)

    else if (kind = set_cst) andif (dimension (valp^.set_val) = 0) then begin
      do_fetch := int_desc (0, no_size, true);
      do_fetch.value_size := no_size
    end

    (*  Anything else must be generated in the constant area.  *)

    else begin
      do_fetch := descriptors[pc_displacement_mode];
      do_fetch.cst_part := gen_cval (cst_val, desc.base);
      if desc.kind = reals then begin
	if desc.precision = maximum (prec_type)
	  then do_fetch.value_size := size_double
	  else do_fetch.value_size := size_long;
      end
      else
	ops_expr_type_desc (do_fetch, desc, false);

      (*  If the generated constant is a varying string, and the reference is
	  nonvarying, then increment over the (constant) length word.  *)

      if (kind = string_cst) andif
	 ( (desc.str_kind = nonvarying) and (valp^.str_varying_ref) ) then
	do_fetch := increment_addr (do_fetch, str_lw_bytes);
    end;

  end (* with e^, cst_val *);
end (* fetch_cst_ref *);
$PAGE fetch_addr_op
(*  FETCH ADDR OP returns a descriptor for the address of its operand.  *)

procedure fetch_addr_op;

var str_addr : string_descriptor;
    addr : op_desc;

begin
  if e^.operand[1]^.desc.kind = strings then begin
    str_addr := fetchstring (e^.operand[1], no_length, no_limit);
    addr := duplicate_desc (str_addr.base_addr);
    if str_addr.base_is_bound then
      addr := increment_addr (addr, str_upb_bytes);
    free_string (str_addr);
  end

  else begin
    addr := fetch (e^.operand [1], control_modes, null_mode, any_size, false);
    if dynamic_flex (e^.operand [1]) then
      addr := increment_addr (addr, arr_upb_bytes);
  end;

  do_fetch := ref_descriptor (addr);
end (* fetch_addr_op *);
$PAGE fetch_temp
(*  FETCH TEMP returns a descriptor for a static or dynamic temporary.
    The temporary is for a value of the type described by the type
    descriptor of the ALC_TEMP tuple.  If there is also an upper bound
    parameter, it will be the single operand of the ALC_TEMP tuple.  *)

procedure fetch_temp;

var
  width, upb: op_desc;

begin
  if e^.operand[1] = nil then
    upb := descriptors [null_mode]
  else
    upb := fetch (e^.operand[1], all_modes, null_mode, any_size, false);
  width := calc_width (e^.desc.base, upb);
  do_fetch := get_temp (width, false)
end (* fetch_temp *);
$PAGE fetch_int_add_sub - in do_fetch
(*  FETCH INT ADD SUB generates code to perform an integer addition or
    subtraction, and returns an operand descriptor for the result.  OPC
    is the generic opcode for the operation (AddOpc or SubOpc).  *)

procedure fetch_int_add_sub ( opc : generic_opcodes );

var e_size : op_sizes;
    op1, op2, t_op : op_desc;
    op1_const, op2_const, t_op_const : boolean;
    op1_val, op2_val, t_op_val : integer;
    op1_guard : guard_key;
    op2_modes : addr_mode_set;

begin
  e_size := exprsize (e);
  op1 := fetch (e^.operand[1], all_modes, null_mode, at_least [e_size], false);
  op1_guard := guard (op1);
  if op1.mode = postincrement_mode
    then op2_modes := nonstack_modes
    else op2_modes := all_modes;
  op2 := fetch (e^.operand[2], op2_modes, null_mode, at_least [op1.value_size], false);
  op1 := unguard (op1_guard);
  op1_const := aconstp (op1, op1_val);
  op2_const := aconstp (op2, op2_val);

  if ( (opc = add_opc) and op1_const ) andif
     ( (op1_val >= 1) and (op1_val <= 8) ) then begin
    t_op := op1;  op1 := op2;  op2 := t_op;
    t_op_const := op1_const;  op1_const := op2_const;  op2_const := t_op_const;
    t_op_val := op1_val;  op1_val := op2_val;  op2_val := t_op_val;
  end;

  if op1_const and op2_const then begin
    if opc = add_opc
      then do_fetch := int_desc (op1_val + op2_val, no_size, false)
      else do_fetch := int_desc (op1_val - op2_val, no_size, false);
  end

  else if op2_const andif
     ( ( ( (op2_val >= -128) and (op2_val <= 127) ) or
	 ( (op2_val >= 0) and (op2_val <= 255) and (e_size = size_byte) ) ) and
       not ( (op2_val >= 1) and (op2_val <= 8) ) ) then begin
    op2 := coerce (op2, [dreg_mode], dreg_mode, [op2.value_size], false);
    gen_mm (opc, op1, op2);
    if opc = sub_opc then
      gen_m (neg_opc, op2);
    free_desc (op1);
    do_fetch := op2;
  end

  else if (op1.mode = dreg_mode) andif (uses_remaining (op1.reg) = 1) then begin
    op1 := coerce (op1, [dreg_mode], dreg_mode, [op2.value_size], false);
    gen_mm (opc, op2, op1);
    free_desc (op2);
    do_fetch := op1;
  end

  else if ( (op2.mode = dreg_mode) andif (uses_remaining (op2.reg) = 1) ) and
	  (op2.value_size = op1.value_size) then begin
    gen_mm (opc, op1, op2);
    if opc = sub_opc then
      gen_m (neg_opc, op2);
    free_desc (op1);
    do_fetch := op2;
  end

  else begin
    op1 := copy_dreg (coerce (op1, [dreg_mode], dreg_mode, [op2.value_size], false));
    gen_mm (opc, op2, op1);
    free_desc (op2);
    do_fetch := op1;
  end;

  if do_fetch.mode <> immediate_mode then begin
    do_fetch.known_positive := op1.known_positive and op2.known_positive and (opc = add_opc);
    if not do_fetch.known_positive then
      do_fetch.extended_size := do_fetch.value_size;
    do_fetch.value_size := e_size;
    do_fetch.signed_value := exprsigned (e);
  end;
end (* fetch_int_add_sub *);
$PAGE fetch_int_mul - in do_fetch
(*  FETCH INT MUL generates code to perform an integer multiplication, and
    returns an operand descriptor for the result.  *)

procedure fetch_int_mul;

var e1, e2 : expr;
    e1_size, e2_size, e_size : op_sizes;
    op1, op2 : op_desc;
    op1_guard : guard_key;
    cval : integer;
    need_sign : boolean;
    opc : generic_opcodes;

begin
  e1 := e^.operand[1];
  e2 := e^.operand[2];
  e1_size := exprsize (e1);
  e2_size := exprsize (e2);
  e_size := exprsize (e);
  op1_guard := guard (fetch (e1, all_modes, null_mode, any_size, false));
  op2 := fetch (e2, all_modes, null_mode, any_size, false);
  op1 := unguard (op1_guard);

  (*  If either operand is a positive constant, we can just use the "multiply
      integer by positive constant" procedure.  *)

  if aconstp (op1, cval) andif (cval >= 0) then
    do_fetch := mul_cst (op2, cval, e2_size, e2^.desc.signed, e_size)
  else if aconstp (op2, cval) andif (cval >= 0) then
    do_fetch := mul_cst (op1, cval, e1_size, e1^.desc.signed, e_size)

  (*  We can use a hardware multiply if both operands fit in a word, and
      either (1) both operands are positive -- then we can use an unsigned
      multiply -- or (2) both operands are representable as signed words --
      then we can use a signed multiply.  *)

  else if (e1_size <= size_word) and (e2_size <= size_word) and
	  ( (op1.known_positive and op2.known_positive) or
	    ( (signed_exprsize (e1) <= size_word) and
	      (signed_exprsize (e2) <= size_word) ) ) then begin
    need_sign := not (op1.known_positive and op2.known_positive);
    op1 := force_word_size (op1);
    op2 := force_word_size (op2);
    if need_sign
      then opc := muls_opc
      else opc := mulu_opc;
    if (op1.mode = dreg_mode) andif (uses_remaining (op1.reg) = 1) then begin
      gen_mm (opc, op2, op1);
      free_desc (op2);
      do_fetch := op1;
    end
    else if (op2.mode = dreg_mode) andif (uses_remaining (op2.reg) = 1) then begin
      gen_mm (opc, op1, op2);
      free_desc (op1);
      do_fetch := op2;
    end
    else begin
      op1 := copy_dreg (op1);
      gen_mm (opc, op2, op1);
      free_desc (op2);
      do_fetch := op1;
    end;
    do_fetch.extended_size := size_long;
  end

  (*  All else failing, use the runtime 32-bit integer multiply.  *)

  else begin
    push_two_long (op1, op2, true);
    gen_rt (rt_imul);
    do_fetch := pop_long;
  end;

  if do_fetch.mode = dreg_mode then begin
    do_fetch.value_size := e_size;
    do_fetch.signed_value := exprsigned (e);
  end;
  do_fetch.known_positive := op1.known_positive and op2.known_positive;
end (* fetch_int_mul *);
$PAGE fetch_int_div_mod
(*  FETCH INT DIV MOD generates code to perform an integer division or modulus,
    and returns an operand descriptor for the result.  *)

procedure fetch_int_div_mod;

var e2 : expr;
    e_size, quotient_size : op_sizes;
    op2_modes : addr_mode_set;
    op1, op2 : op_desc;
    quo_bit_size, dividend, divisor, shift : integer;
    skip : def;
    op1_guard : guard_key;
    mod_size : op_sizes;
    mod_trunc : boolean;
    divisor_signed_word, divisor_positive,
    quotient_signed_word, quotient_positive : boolean;

begin
  e_size := exprsize (e);
  e2 := e^.operand[2];
  op1 := fetch (e^.operand[1], all_modes, dreg_mode, any_size, false);
  op1_guard := guard (op1);
  if op1.mode = postincrement_mode
    then op2_modes := nonstack_modes
    else op2_modes := all_modes;
  op2 := fetch (e2, op2_modes, null_mode, any_size, false);
  op1 := unguard (op1_guard);
  divisor_signed_word := (op2.value_size = size_byte) or (op2.signed_value);
  divisor_positive := op2.known_positive;
  if e^.opcode = idiv_op then begin
    quotient_size := e_size;
    quotient_signed_word := (signed_exprsize (e) <= size_word);
    quotient_positive := not e^.desc.signed
  end
  else begin
    quo_bit_size := e^.operand[1]^.desc.int_prec + ord (e^.operand[2]^.desc.signed);
    if quo_bit_size <= 16 then
      quotient_size := size_word
    else
      quotient_size := size_long;
    quotient_positive := e^.operand[1]^.desc.signed = e^.operand[2]^.desc.signed;
    quotient_signed_word := (quo_bit_size + ord (quotient_positive)) <= 16
  end;

  if aconstp (op1, dividend) and aconstp (op2, divisor) then begin
    if e^.opcode = idiv_op
      then do_fetch := int_desc (dividend div divisor, no_size, false)
      else do_fetch := int_desc (dividend mod divisor, no_size, false);
  end

  (*  If the divisor is a constant power of two, we can do a division with
      a shift, or a remainder with an "and" unless the dividend could be
      negative.  *)

  else if aconstp (op2, divisor) andif
     power_of_two (divisor, shift) andif
     ( op1.known_positive or (e^.opcode = idiv_op) ) then begin
    if divisor = 1 then begin
      if e^.opcode = idiv_op then
	do_fetch := op1
      else begin
	do_fetch := int_desc (0, no_size, false);
	free_desc (op1);
      end;
    end
    else begin
      do_fetch := copy_dreg (op1);
      if e^.opcode = idiv_op then begin
	if ( (shift >= 8) and (do_fetch.value_size = size_byte) ) or
	   ( (shift >= 16) and (do_fetch.value_size = size_word) ) then begin
	  free_desc (do_fetch);
	  do_fetch := int_desc (0, no_size, false);
	end
	else begin
	  if shift <= 8
	    then op2 := int_desc (shift, size_byte, true)
	    else op2 := loadi (get_dreg (), shift, size_byte, true);
	  if (not do_fetch.known_positive) and do_fetch.signed_value then begin
	    skip := def_create (local_def);
	    gen_m (tst_opc, do_fetch);
	    gen_bcc (pl_cc, skip);
	    gen_im (add_opc, divisor - 1, do_fetch);
	    gen_def (code_area, skip);
	    gen_mm (asr_opc, op2, do_fetch);
	  end
	  else
	    gen_mm (lsr_opc, op2, do_fetch);
	  free_desc (op2);
	  if not ( (do_fetch.value_size = size_word) and (shift = 8) ) then
	    do_fetch.signed_value := true;
	  if do_fetch.extended_size = no_size then
	    do_fetch.extended_size := do_fetch.value_size;
	  if shift >= 24 then
	    do_fetch.value_size := size_byte
	  else if shift >= 16 then
	    do_fetch.value_size := size_word
	  else if (shift >= 8) and (do_fetch.value_size = size_word) then
	    do_fetch.value_size := size_byte;
	end;
      end

      else (* e^.opcode = imod_op *) begin
	if shift <= 8 then
	  mod_size := size_byte
	else if shift <= 16 then
	  mod_size := size_word
	else
	  mod_size := size_long;
	mod_trunc := (shift = 8) or (shift = 16);
	if (mod_size < do_fetch.value_size) or
	   ( (mod_size = do_fetch.value_size) and (not mod_trunc) ) then begin
	  if mod_trunc then begin
	    do_fetch.value_size := mod_size;
	    do_fetch.extended_size := mod_size;
	    do_fetch.signed_value := false;
	  end
	  else begin
	    do_fetch.value_size := max (mod_size, size_word);
	    do_fetch.extended_size := do_fetch.value_size;
	    do_fetch.signed_value := true;
	    gen_im (and_opc, divisor - 1, do_fetch);
	    do_fetch.value_size := mod_size;
	  end;
	end;
      end;
    end;
  end

  (*  We can use a hardware divide instruction if the divisor and the
      quotient can both be represented in words.  *)

  else if (exprsize (e2) <= size_word) and (quotient_size <= size_word) and
	  ( ( op1.known_positive and (e^.opcode = idiv_op) ) or
	    ( (divisor_positive or quotient_signed_word) and
	      (quotient_positive or divisor_signed_word) ) ) then begin
    do_fetch := coerce (op1, [dreg_mode], dreg_mode, long_only, true);
    do_fetch := copy_dreg (do_fetch);
    op2 := force_word_size (op2);
    if divisor_signed_word and quotient_signed_word
      then gen_mm (divs_opc, op2, do_fetch)
      else gen_mm (divu_opc, op2, do_fetch);
    free_desc (op2);
    do_fetch.value_size := e_size;
    do_fetch.extended_size := size_word;
    do_fetch.signed_value := exprsigned (e);
    if e^.opcode = imod_op then begin
      if (allowed_sizes * word_max = [ ]) and
	 (do_fetch.value_size = size_word) and
	 (not do_fetch.signed_value) then begin
	gen_m (clr_opc, do_fetch);
	do_fetch.extended_size := size_long;
      end;
      do_fetch.value_size := size_word;
      gen_m (swap_opc, do_fetch);
      do_fetch.value_size := e_size;
    end;
  end

  (*  In the most general case, we must call the runtime routine.  *)

  else begin
    push_two_long (op1, op2, false);
    if e^.opcode = idiv_op
      then gen_rt (rt_idiv)
      else gen_rt (rt_imod);
    do_fetch := pop_long;
  end;

  do_fetch.known_positive := op1.known_positive and op2.known_positive;
end (* fetch_int_div_mod *);
$PAGE fetch_int_neg_abs
(*  FETCH INT NEG ABS generates code for a unary negation or absolute value
    operation on an integer operand.  *)

procedure fetch_int_neg_abs;

var skip : def;
    esize : op_sizes;

begin
  esize := exprsize (e);
  do_fetch := fetch (e^.operand[1], [dreg_mode, postincrement_mode], null_mode,
		     at_least [esize], false);
  if (do_fetch.mode = dreg_mode) or (esize < do_fetch.value_size) then
    do_fetch := copy_dreg (do_fetch);
  if do_fetch.mode = postincrement_mode then
    do_fetch.mode := indirect_mode;

  if e^.opcode = iabs_op then begin
    if not do_fetch.known_positive then begin
      skip := def_create (local_def);
      gen_m (tst_opc, do_fetch);
      gen_bcc (pl_cc, skip);
      gen_m (neg_opc, do_fetch);
      gen_def (code_area, skip);
    end;
  end
  else
    gen_m (neg_opc, do_fetch);

  if do_fetch.mode = indirect_mode then
    do_fetch.mode := postincrement_mode;

  do_fetch.extended_size := do_fetch.value_size;
  do_fetch.value_size := esize;
  do_fetch.signed_value := exprsigned (e);
  do_fetch.known_positive := (not e^.desc.signed);
end (* fetch_int_neg_abs *);
$PAGE fetch_rtcall
(*  FETCH RTCALL generates code to fetch zero, one, or two longword (real or
    integer) operands, push them on the stack, and call a runtime routine.
    It returns a descriptor for a stack-top reference returned by the runtime
    routine.  RTS is the runtime symbol for the routine to be called.
    RSLT_DESC is the stack-top descriptor to be returned.  *)

procedure fetch_rtcall ( rts : rt_symbol; commutative : boolean; rslt_desc : op_desc );

var e1, e2 : expr;
    op1, op2 : op_desc;
    op1_guard : guard_key;

begin
  if exprsize (e) = size_double then
    do_fetch := dfetch (e, descriptors [null_mode], true (* fetch will take care of count *))

  else begin
    if (upperbound (e^.operand) <> 0) andif (e^.operand[1] <> nil) then begin
      e1 := e^.operand[1];
      push (fetch (e1, all_modes, null_mode, long_only, true), size_long);

      if upperbound (e^.operand) > 1 then begin
	e2 := e^.operand[2];
	op1_guard := guard (pop_long);
	op2 := fetch (e2, all_modes, null_mode, long_only, true);
	op1 := unguard (op1_guard);
	push_two_long (op1, op2, commutative);
      end;
    end;

    gen_rt (rts);
    do_fetch := rslt_desc;
  end;
end (* fetch_rtcall *);
$PAGE fetch_exc_call
(*  FETCH EXC CALL generates call for a MASKED or PENDING runtime call.  *)

procedure fetch_exc_call ( rts : rt_symbol );

begin
  pusha (fetch (e^.operand[1], control_modes, null_mode, any_size, false));
  gen_rt (rts);
  do_fetch := pop_bool;
end (* fetch_exc_call *);
$PAGE fetch_real_neg_abs
(*  FETCH REAL NEG ABS generates code for a unary negate or absolute value
    operation on a real operand, and returns a descriptor for the result.
    OPC is the generic opcode for the operation that will manipulate the
    sign bit (BchgOpc or BclrOpc).  *)

procedure fetch_real_neg_abs (abs_op: boolean);

var op : op_desc;
    lab: def;

const
    stack_op : op_desc =
	(displacement_mode, size_byte, size_byte, false, false, sp, d0, (3, absolute_sc));

begin
  if exprsize (e) = size_double then
    do_fetch := dfetch (e, descriptors [null_mode], true (* fetch will take care of count *))

  else begin
    assert (exprsize (e^.operand[1]) = size_long);
    do_fetch := fetch (e^.operand[1], [dreg_mode, postincrement_mode], null_mode,
		       long_only, true);
    if do_fetch.mode = dreg_mode then begin
      do_fetch := copy_dreg (do_fetch);
      op := do_fetch;
    end
    else
      op := stack_op;
    if abs_op then
      gen_mm (bclr_opc, int_desc (7, no_size, false), op)
    else begin
      lab := def_create (local_def);
      gen_m (tst_opc, op);
      gen_bcc (eq_cc, lab);
      gen_mm (bchg_opc, int_desc (7, no_size, false), op);
      gen_def (code_area, lab)
    end
  end;
end (* fetch_real_neg_abs *);
$PAGE fetch_float
(*  FETCH FLOAT generates code for an integer->real or double->single
    conversion.  *)

procedure fetch_float;

var e1 : expr;

begin
  if exprsize (e) = size_double then
    do_fetch := dfetch (e, descriptors [null_mode], true (* fetch will take care of count *))

  else begin
    e1 := e^.operand[1];
    if e1^.desc.kind = reals then begin
      if exprsize (e1) = size_long then
	do_fetch := fetch (e1, allowed_modes, preferred_mode, allowed_sizes, sign_required)
      else begin
	pusha (dfetch (e1, descriptors [null_mode], false));
	dfpop;
	gen_rt (rt_dsingle);
	do_fetch := pop_long;
      end;
    end

    else begin
      assert (e1^.desc.kind = ints);
      push (fetch (e1, all_modes, null_mode, long_only, true), size_long);
      gen_rt (rt_ffloat);
      do_fetch := pop_long;
    end;
  end;
end (* fetch_float *);
$PAGE fetch_min_max
(*  FETCH MIN MAX generates code to evaluate an integer or single-precision
    real minimum or maximum operator.  *)

procedure fetch_min_max;

var op : op_desc;
    i : integer;
    lab : def;
    cond : condition_codes;

begin
  if exprsize (e) = size_double then
    do_fetch := dfetch (e, descriptors [null_mode], true (* fetch will take care of count *))

  else begin
    if (e^.opcode = imin_op) or (e^.opcode = rmin_op)
      then cond := lt_cc
      else cond := gt_cc;
    do_fetch := fetch (e^.operand[1], [dreg_mode], dreg_mode, long_only, true);
    for i := 2 to upperbound (e^.operand) do begin
      op := fetch (e^.operand[i], [dreg_mode], dreg_mode, long_only, true);
      if e^.desc.kind = ints then
	gen_mm (cmp_opc, op, do_fetch)
      else begin
	push (duplicate_desc (do_fetch), size_long);
	push (duplicate_desc (op), size_long);
	gen_rt (rt_fcmp);
      end;
      lab := def_create (local_def);
      gen_bcc (cond, lab);
      gen_mm (move_opc, op, do_fetch);
      gen_def (code_area, lab);
      free_desc (op);
    end;
  end;
end (* fetch_min_max *);
$PAGE fetch_double_op
(*  FETCH DOUBLE OP generates code for an operator (ROUND or TRUNC) which
    takes a double-precision real operand and returns an integer result.  *)

procedure fetch_double_op ( rts : rt_symbol );

begin
  pusha (dfetch (e^.operand[1], descriptors [null_mode], false));
  dfpop;
  gen_rt (rts);
  do_fetch := pop_long;
end (* fetch_double_op *);
$PAGE fetch_length
(*  FETCH LENGTH computes the length of a string expression.  *)

procedure fetch_length;

var str_desc : string_descriptor;

begin
  str_desc := fetchstring ( e^.operand[1], actual_length, no_limit );
  do_fetch := duplicate_desc ( str_desc.len_addr );
  free_string ( str_desc );
end (* fetch_length *);
$PAGE fetch_case_conversion
(* FETCH CASE CONVERSION upper- or lower-cases a character. *)

procedure fetch_case_conversion;

var
  skip_label: def;

begin
  assert (e^.desc.kind = chars);
  do_fetch := fetch ( e^.operand[1], allowed_modes, preferred_mode, allowed_sizes, sign_required );
  do_fetch.value_size := size_byte;
  do_fetch.extended_size := size_byte;
  do_fetch.signed_value := true;
  do_fetch.known_positive := true;
  do_fetch := copy_dreg ( do_fetch );
  skip_label := def_create ( local_def );
  if e^.opcode = upc_op then begin
    gen_im ( cmp_opc, ord ('a'), do_fetch );
    gen_bcc ( lt_cc, skip_label );
    gen_im ( cmp_opc, ord ('z'), do_fetch );
    gen_bcc ( gt_cc, skip_label );
    gen_im ( sub_opc, ord ('a') - ord ('A'), do_fetch );
  end
  else begin
    gen_im ( cmp_opc, ord ('A'), do_fetch );
    gen_bcc ( lt_cc, skip_label );
    gen_im ( cmp_opc, ord ('Z'), do_fetch );
    gen_bcc ( gt_cc, skip_label );
    gen_im ( add_opc, ord ('a') - ord ('A'), do_fetch );
  end;
  gen_def ( code_area, skip_label );
end (* fetch_case_conversion *);
$PAGE fetch_scalar_conversion
(* FETCH SCALAR CONVERSION performs scalar conversions. *)

procedure fetch_scalar_conversion;

begin
  do_fetch := fetch ( e^.operand[1], allowed_modes, preferred_mode, allowed_sizes, sign_required );
end (* fetch_scalar_conversion *);
$PAGE fetch_bound_op
(*  FETCH BOUND OP returns a descriptor for the upper bound, lower bound, or
    dimension of an array or string.  *)

procedure fetch_bound_op;

var lwb : integer;

begin
  with e^ do begin
    if operand[1]^.desc.kind = strings then
      lwb := 1
    else
      lwb := operand[1]^.desc.base^.index_type^.minval;

    case opcode of

      lwb_op :
	do_fetch := int_desc (lwb, no_size, false);

      upb_op :
	do_fetch := upper_bound (operand[1], descriptors[null_mode]);

      dim_op :
	begin
	  do_fetch := upper_bound (operand[1], descriptors[null_mode]);
	  lwb := lwb - 1;
	  if lwb <> 0 then begin
	    do_fetch := copy_dreg (do_fetch);
	    if lwb < 0
	      then gen_im (add_opc, - lwb, do_fetch)
	      else gen_im (sub_opc, + lwb, do_fetch);
	  end;
	end;
    end (* case opcode *);
  end (* with e^ *);
end (* fetch_bound_op *);
$PAGE do_fetch - main routine
begin
  case e^.opcode of

    ident_ref :
      fetch_ident_ref;

    field_ref :
      fetch_field_ref;

    ptr_ref :
      fetch_ptr_ref;

    array_ref :
      fetch_array_ref;

    substr_ref :
      fetch_substr_ref;

    buffer_ref :
      fetch_buffer_ref;

    cst_ref :
      fetch_cst_ref;

    addr_op :
      fetch_addr_op;

    iadd_op :
      fetch_int_add_sub (add_opc);

    isub_op :
      fetch_int_add_sub (sub_opc);

    imul_op :
      fetch_int_mul;

    idiv_op,
    imod_op :
      fetch_int_div_mod;

    ineg_op,
    iabs_op :
      fetch_int_neg_abs;

    radd_op :
      fetch_rtcall (rt_fadd, true, pop_long);

    rsub_op :
      fetch_rtcall (rt_fsub, false, pop_long);

    rmul_op :
      fetch_rtcall (rt_fmul, true, pop_long);

    rdiv_op :
      fetch_rtcall (rt_fdiv, false, pop_long);

    sqrt_op :
      fetch_rtcall (rt_fsqt, false, pop_long);

    ln_op :
      fetch_rtcall (rt_fln, false, pop_long);

    log_op :
      fetch_rtcall (rt_flog, false, pop_long);

    exp_op :
      fetch_rtcall (rt_fexp, false, pop_long);

    sin_op :
      fetch_rtcall (rt_fsin, false, pop_long);

    cos_op :
      fetch_rtcall (rt_fcos, false, pop_long);

    tan_op :
      fetch_rtcall (rt_ftan, false, pop_long);

    cotan_op :
      fetch_rtcall (rt_fctn, false, pop_long);

    arcsin_op :
      fetch_rtcall (rt_fasin, false, pop_long);

    arccos_op :
      fetch_rtcall (rt_facos, false, pop_long);

    arctan_op :
      if upperbound (e^.operand) = 1
	then fetch_rtcall (rt_fatan, false, pop_long)
	else fetch_rtcall (rt_fatan2, false, pop_long);

    sinh_op :
      fetch_rtcall (rt_fsinh, false, pop_long);

    cosh_op :
      fetch_rtcall (rt_fcosh, false, pop_long);

    tanh_op :
      fetch_rtcall (rt_ftanh, false, pop_long);

    exprr_op :
      fetch_rtcall (rt_fpwr, false, pop_long);

    expii_op :
      fetch_rtcall (rt_ipwr, false, pop_long);

    expri_op :
      fetch_rtcall (rt_fpwri, false, pop_long);

    rneg_op :
      fetch_real_neg_abs (false);

    rabs_op :
      fetch_real_neg_abs (true);

    round_op :
      if upperbound (e^.operand) = 2 then
	fetch_rtcall (rt_fround2, false, pop_long)
      else if exprsize (e^.operand[1]) = size_long then
	fetch_rtcall (rt_fround, false, pop_long)
      else
	fetch_double_op (rt_dround);

    trunc_op :
      if exprsize (e^.operand[1]) = size_long then
	fetch_rtcall (rt_ftrunc, false, pop_long)
      else
	fetch_double_op (rt_dtrunc);

    float_op :
      fetch_float;

    imin_op,
    imax_op,
    rmin_op,
    rmax_op :
      fetch_min_max;

    random_op :
      if upperbound (e^.operand) = 0
	then fetch_rtcall (rt_frandom, false, pop_long)
	else fetch_rtcall (rt_franset, false, pop_long);

    length_op :
      fetch_length;

    sclcvt_op:
      fetch_scalar_conversion;

    lwb_op,
    upb_op,
    dim_op :
      fetch_bound_op;

    lwc_op, upc_op:
      fetch_case_conversion;

    index_op :
      do_fetch := str_index_op (e);

    search_op,
    verify_op :
      do_fetch := str_search_verify (e);

    ile_op..filne_op,
    bnot_op, or_op, and_op,
    in_op,
    odd_op :
      do_fetch := fetchboolean (e, false, true (* fetch will take care of usage count *));

    func_call_op :
      do_fetch := function_call (e);

    new_op :
      fetch_rtcall (rt_new, false, pop_long);

    extent_op :
      fetch_rtcall (rt_extent, false, pop_long);

    open_op,
    reset_op,
    rewrite_op,
    update_op :
      do_fetch := rt_open_call (e);

    alc_temp_op :
      fetch_temp;

    eof_op :
      fetch_rtcall (rt_eof, false, pop_bool);

    eoln_op :
      fetch_rtcall (rt_eoln, false, pop_bool);

    eopage_op :
      fetch_rtcall (rt_eopage, false, pop_bool);

    cursor_op :
      fetch_rtcall (rt_cursor, false, pop_long);

    filesize_op :
      fetch_rtcall (rt_filesize, false, pop_long);

    iostatus_op :
      if e^.operand[1] <> nil then
	fetch_rtcall (rt_filestatus, false, pop_word)
      else
	fetch_rtcall (rt_genstatus, false, pop_word);

    extstatus_op :
      fetch_rtcall (rt_extstatus, false, pop_word);

    masked_op :
      fetch_exc_call (rt_masked);

    pending_op :
      fetch_exc_call (rt_pending);

    mathstatus_op :
      fetch_rtcall (rt_mathstatus, false, pop_word);

    exiostatus_op :
      fetch_rtcall (rt_exiostatus, false, pop_word);

    prgmstatus_op :
      fetch_rtcall (rt_progstatus, false, pop_word);

    spclstatus_op :
      fetch_rtcall (rt_spclstatus, false, pop_word);

    time_op :
      begin
	gen_rt (rt_time);
	do_fetch := pop_long;
      end;

    runtime_op :
      begin
	gen_rt (rt_runtime);
	do_fetch := pop_long;
    end;

  end (* case e^.opcode *);

  (*  The individual fetch sub-routines are not guaranteed to fulfill the
      required attributes.  Therefore, do a final coercion to make sure.  *)

  do_fetch := coerce (do_fetch, allowed_modes, preferred_mode, allowed_sizes, sign_required);
end (* do_fetch *);
$PAGE dec_expr_usage
(* DEC EXPR USAGE decrements the usage count for an expression tuple.  IF this is the
   last use, and the expression has a saved descriptor on the heap, its freed. *)

procedure dec_expr_usage (* e: expr *); 	(* forward declared *)

begin
  with e^ do begin
    assert (usage_count >= 1);
    usage_count := usage_count - 1;
    if (usage_count = 0) and (result <> nil) then begin
      dispose (result);
      result := nil
    end
  end
end (* dec_expr_usage *);
$PAGE fetch
(*  FETCH will return an operand descriptor which may be used to access the
    value of a specified expression tuple.  *)

function fetch (* forward declared *);

var saved_desc : saved_op_ptr;
    modes : addr_mode_set;

begin
  with e^ do begin
    assert (usage_count >= 1);

    (*  A non-zero REF_FRE field indicates a deferred check operation.  *)

    if ref_fre <> 0 then
      do_check (e);

    (*  If the expression has been fetched previously, then its RESULT
	field will have a pointer to a previously created descriptor.
	Otherwise, DO_FETCH will construct a new descriptor.  *)

    if result <> nil then begin
      with saved_op_ptr (result)^ do begin
	if indirect then
	  fetch := deref_descriptor (op, size, signed)
	else
	  fetch := op;
	fetch := coerce (fetch, allowed_modes, preferred_mode, allowed_sizes, sign_required);
      end;
    end
    else begin
      if usage_count = 1 then
	modes := allowed_modes
      else if allowed_modes * register_modes <> [] then
	modes := allowed_modes * (register_modes + [immediate_mode])

      else
	modes := allowed_modes * nonstack_modes;
      fetch := do_fetch (e, modes, preferred_mode, allowed_sizes, sign_required);

      (*  If this is a multiple-use expression, then we must save the fetched
	  operand descriptor so that it will still be available the next time
	  the expression is fetched.  *)

      if usage_count <> 1 then begin
	use_desc (fetch, usage_count - 1);
	new (saved_desc);
	saved_desc^ := (fetch, false);
	result := expr (saved_desc);
      end;
    end;

    (*  Decrement the usage count for the expression.  If this is the last use,
	and the expression has a saved descriptor on the heap, then free it.  *)

    dec_expr_usage (e)

  end (* with e^ *);
end (* fetch *);
$PAGE dfetch
(*  DFETCH is called with a double-precision floating-point expression
    tuple and an operand descriptor.  If the mode of the descriptor is
    not null, it stores the value of the expression in the location
    indicated by the descriptor, frees the destination descriptor, and
    returns a null descriptor; otherwise, it returns a descriptor for
    the location of the value of the expression, allocating a temporary
    for it if necessary.  *)

function dfetch (* forward declared *);
$PAGE push_destination
(*  PUSH DESTINATION checks DEST.  If it is null, then DFETCH is set to a
    temporary, whose address is pushed on the stack.  Otherwise, the address
    of DEST is pushed on the stack, and DEST is freed.   *)

procedure push_destination;

begin
  if dest.mode = null_mode then begin
    dfetch := dftemp ();
    gen_m (pea_opc, dfetch);
  end
  else begin
    dfetch := descriptors [null_mode];
    gen_m (pea_opc, dest);
    free_desc (dest);
  end;
end (* push_destination *);
$PAGE dfetch_operator
(*  DFETCH OPERATOR will fetch one or two double-precision real operands,
    push their addresses on the stack, push the result address on the
    stack, and call a specified runtime math routine.  *)

procedure dfetch_operator ( rts : rt_symbol );

var op : array [1..2] of op_desc;
    i, n : 1 .. 2;

begin
  with source^ do begin
    n := upperbound (operand);
    if (n = 2) andif (operand[1] = operand[2]) then begin
      dec_expr_usage (operand[2]); (* since we won't fetch it *)
      op[1] := dfetch (operand[1], descriptors[null_mode], false);
      pusha (duplicate_desc (op[1]));
      pusha (op[1]);
      dfpop;
    end
    else begin
      for i := 1 to n do
	op[i] := dfetch (operand[i], descriptors[null_mode], false);
      for i := 1 to n do
	pusha (op[i]);
      for i := 1 to n do
	dfpop;
    end;
  end;

  push_destination;
  gen_rt (rts);
end (* dfetch_operator *);
$PAGE dfetch_r_i
(*  DFETCH R I will generate code for an operator that takes a double-precision
    real operand and an integer operand, and returns a double-precision real
    result.  It is used for D**I and ROUND(D,I).  *)

procedure dfetch_r_i ( rts : rt_symbol );

var op1, op2 : op_desc;

begin
  op1 := dfetch (source^.operand[1], descriptors [null_mode], false);
  op2 := fetch (source^.operand[2], nonstack_modes, null_mode, any_size, false);
  pusha (op1);
  dfpop;
  push (op2, size_long);
  push_destination;
  gen_rt (rts);
end (* dfetch_r_i *);
$PAGE dfetch_abs
(*  DFETCH ABS generates code for an absolute value operation on a double-precision
    real operand. *)

procedure dfetch_abs;

var op : op_desc;

begin
  if dest.mode = null_mode then
    dfetch := dftemp ()
  else
    dfetch := dest;
  op := dfetch (source^.operand[1], duplicate_desc (dfetch), false);
  op := increment_addr (duplicate_desc (dfetch), 6);
  op.value_size := size_byte;	(* Required by bit instruction *)
  op.extended_size := size_byte;
  gen_mm (bclr_opc, int_desc (0, no_size, false), op);
  free_desc (op);
  if dest.mode <> null_mode then
    free_desc (dfetch);
end (* dfetch_abs *);
$PAGE dfetch_neg
(* DFETCH NEG generates code for a unary negate operation on a double-precision
   real operand.  *)

procedure dfetch_neg;

var op : op_desc;
    lab: def;

begin
  if dest.mode = null_mode then
    dfetch := dftemp ()
  else
    dfetch := dest;
  op := dfetch (source^.operand[1], duplicate_desc (dfetch), false);
  op := increment_addr (duplicate_desc (dfetch), 7); (* jump over to exponent byte *)
  op.value_size := size_byte;	(* Required by bit instruction *)
  op.extended_size := size_byte;
  gen_m (tst_opc, op);
  lab := def_create (local_def);
  gen_bcc (eq_cc, lab); (* can't negate zero *)
  op := increment_addr (op, -1); (* backup to byte with sign in it *)
  gen_mm (bchg_opc, int_desc (0, no_size, false), op);
  free_desc (op);
  gen_def (code_area, lab);
  if dest.mode <> null_mode then
    free_desc (dfetch);
end (* dfetch_neg *);
$PAGE dfetch_min_max
(*  DFETCH MIN MAX generates code for a double--precision real minimum or
    maximum operation.  *)

procedure dfetch_min_max;

var temp, op : op_desc;
    i : integer;
    cond : condition_codes;
    lab : def;
    after : boolean;

begin
  if source^.opcode = rmin_op
    then cond := lt_cc
    else cond := gt_cc;
  dfetch := dftemp ();
  op := dfetch (source^.operand[1], duplicate_desc (dfetch), false);
  for i := 2 to upperbound (source^.operand) do begin
    op := dfetch (source^.operand [i], descriptors [null_mode], false);
    dfpop;
    pusha (duplicate_desc (dfetch));
    pusha (duplicate_desc (op));
    gen_rt (rt_dcmp);
    lab := def_create (local_def);
    gen_bcc (cond, lab);
    after := false;
    blk_move (int_desc (8, no_size, false), false, op, duplicate_desc (dfetch), temp, after);
    free_desc (temp);
    gen_def (code_area, lab);
  end;
  if dest.mode <> null_mode then begin
    after := false;
    blk_move (int_desc (8, no_size, false), false, dfetch, dest, temp, after);
    free_desc (temp);
    dfpop;
  end;
end (* dfetch_min_max *);
$PAGE dfetch_float
(*  DFETCH FLOAT generates code for an integer->real or single->double
    conversion.  *)

procedure dfetch_float;

var e1 : expr;

begin
  e1 := source^.operand[1];
  if e1^.desc.kind = reals then begin
    if e1^.desc.precision = maximum (prec_type) then
      dfetch := dfetch (e1, dest, false)
    else begin
      push (fetch (e1, all_modes, null_mode, long_only, true), size_long);
      push_destination;
      gen_rt (rt_fdouble);
    end;
  end

  else begin
    assert (e1^.desc.kind = ints);
    push (fetch (e1, all_modes, null_mode, long_only, true), size_long);
    push_destination;
    gen_rt (rt_dfloat);
  end;
end (* fetch_float *);
$PAGE dfetch - main routine
var rslt_desc : op_desc;
    after, texpr_uses_ok : boolean;

begin
  assert (source^.result = nil);
  assert (source^.usage_count = 1);
  assert ( (source^.desc.kind = reals) andif
	   (source^.desc.precision = maximum (prec_type)) );
  texpr_uses_ok := expr_uses_ok; (* copy I can change *)
  case source^.opcode of

    first_data_ref..last_data_ref, func_call_op :
      begin
	assert (not texpr_uses_ok);
	dfetch := fetch (source, all_modes, null_mode, double_only, true);
	texpr_uses_ok := true; (* fetch took care of it *)
	if dest.mode <> null_mode then begin
	  after := false;
	  blk_move (int_desc (8, no_size, false), false, dfetch, dest, rslt_desc, after);
	  free_desc (rslt_desc);
	  dfetch := descriptors [null_mode];
	  if source^.opcode = func_call_op then
	    dfpop;
	end
	else begin
	  if source^.opcode <> func_call_op then
	    dfpush_dummy;
	end;
      end;


    radd_op :
      dfetch_operator (rt_dadd);

    rsub_op :
      dfetch_operator (rt_dsub);

    rmul_op :
      dfetch_operator (rt_dmul);

    rdiv_op :
      dfetch_operator (rt_ddiv);

    sqrt_op :
      dfetch_operator (rt_dsqt);

    ln_op :
      dfetch_operator (rt_dln);

    log_op :
      dfetch_operator (rt_dlog);

    exp_op :
      dfetch_operator (rt_dexp);

    sin_op :
      dfetch_operator (rt_dsin);

    cos_op :
      dfetch_operator (rt_dcos);

    tan_op :
      dfetch_operator (rt_dtan);

    cotan_op :
      dfetch_operator (rt_dctn);

    arcsin_op :
      dfetch_operator (rt_dasin);

    arccos_op :
      dfetch_operator (rt_dacos);

    arctan_op :
      if upperbound (source^.operand) = 1
	then dfetch_operator (rt_datan)
	else dfetch_operator (rt_datan2);

    sinh_op :
      dfetch_operator (rt_dsinh);

    cosh_op :
      dfetch_operator (rt_dcosh);

    tanh_op :
      dfetch_operator (rt_dtanh);

    exprr_op :
      dfetch_operator (rt_dpwr);

    expri_op :
      dfetch_r_i (rt_dpwri);

    round_op :
      dfetch_r_i (rt_dround2);

    rneg_op :
      dfetch_neg;

    rabs_op :
      dfetch_abs;

    float_op :
      dfetch_float;

    rmin_op, rmax_op :
      dfetch_min_max;

    random_op :
      if upperbound (source^.operand) = 0
	then dfetch_operator (rt_drandom)
	else dfetch_operator (rt_dranset);

  end (* case source^.opcode *);

  if not texpr_uses_ok then
    dec_expr_usage (source)

end (* dfetch *);
$PAGE with_start
(* WITH START obtains the address of a 'withed' record, and saves a descriptor
   for use in references to fields of that record.  *)

public procedure with_start ( with_rec : expr;
			      var num_withs_saved, max_withs_saved: unit_range;
			      savedwith_desc: op_desc )  options special(coercions);

var
  with_desc,
  address_desc,
  save_loc: op_desc;
  saved_desc: saved_op_ptr;

begin
  with_desc := fetch (with_rec, memory_modes, indirect_mode, any_size, false);
  free_desc (with_desc); (* remove reg uses corresponding to start_with *)

  (* The usage_count on the expr tuple and the uses_remaining on any involved
     registers now both reflect the uses remaining for the with record. *)

  if (with_desc.mode in indexed_modes) or
     ((with_desc.mode in areg_relative_modes) andif not (with_desc.reg in [BP, FP])) then begin
    address_desc := ref_descriptor (with_desc); (* frees with_desc *)
    use_desc (with_desc, - (with_rec^.usage_count - 1)); (* take care of rest of uses *)
    num_withs_saved := num_withs_saved + 1;
    max_withs_saved := max (max_withs_saved, num_withs_saved);
    save_loc := savedwith_desc;
    save_loc.cst_part.offset := (num_withs_saved - 1) * 4;
    gen_mm (move_opc, address_desc, save_loc);
    free_desc (address_desc);
    dispose (with_rec^.result);
    new (saved_desc);
    saved_desc^ := (save_loc, true, with_desc.value_size, with_desc.signed_value);
    with_rec^.result := expr (saved_desc)
  end
end (* with_start *);
$PAGE with_end
(* WITH END terminates a with construct. *)

public procedure with_end ( with_rec : expr;
			    var num_withs_saved: unit_range )  options special(coercions);

begin
  if saved_op_ptr (with_rec^.result)^.indirect then
    num_withs_saved := num_withs_saved - 1;
  free_desc (fetch (with_rec, all_modes, null_mode, any_size, false))
end (* with_end *).
 OC[