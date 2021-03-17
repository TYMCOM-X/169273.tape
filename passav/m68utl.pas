$TITLE M68UTL - M68000 utility functions

module m68utl options check;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68gen
$SYSTEM m68exp
$PAGE register allocation
static var

    (*  Register allocation is controlled through the USAGE COUNTS array,
	which indicates the number of remaining uses for each register.  *)

    usage_counts: array [registers] of pos_int;
$PAGE reg_init
(*  REG INIT is called at the beginning of code generation for each routine
    to initialize the usage counts (to zero).  *)

public procedure reg_init;

var r: registers;

begin
  for r := minimum (registers) to maximum (registers) do
    usage_counts [r] := 0;
end;
$PAGE use_reg
(*  USE REG adjusts the usage count of a register by a specified value.  *)

public procedure use_reg ( r: registers; delta: integer );

begin
  if not (r in reserved_registers) then
    usage_counts [r] := usage_counts [r] + delta;
end;
$PAGE free_reg
(*  FREE REG will decrement the usage count of a register.  *)

public procedure free_reg ( r: registers );

begin
  if not (r in reserved_registers) then
    usage_counts [r] := usage_counts [r] - 1;
end;
$PAGE uses_remaining
(*  USES REMAINING returns the usage count of a register.  *)

public function uses_remaining ( r : registers ) : integer;

begin
  uses_remaining := usage_counts [r];
end;
$PAGE get_areg
(*  GET AREG will return the number of an available address register.  *)

public function get_areg (): addr_regs;

var r: addr_regs;
    found: boolean;

begin
  found := false;
  for r := minimum (addr_regs) to maximum (addr_regs) do
    exit if (usage_counts [r] = 0) and not (r in reserved_registers) do
      found := true;
  if found then begin
    get_areg := r;
    usage_counts [r] := 1;
  end
  else
    fatal_error ('A-Register overflow');
end;
$PAGE get_dreg
(*  GET DREG will return the number of an available data register.  *)

public function get_dreg (): data_regs;

var r: data_regs;
    found: boolean;

begin
  found := false;
  for r := minimum (data_regs) to maximum (data_regs) do
    exit if (usage_counts [r] = 0) and not (r in reserved_registers) do
      found := true;
  if found then begin
    get_dreg := r;
    usage_counts [r] := 1;
  end
  else
    fatal_error ('D-Register overflow');
end;
$PAGE get_xreg
(*  GET XREG will return the number of any available register.  It will
    always return a data register in preference to an address register.  *)

public function get_xreg (): registers;

var r: registers;
    found: boolean;

begin
  found := false;
  for r := minimum (registers) to maximum (registers) do
    exit if (usage_counts [r] = 0) and not (r in reserved_registers) do
      found := true;
  if found then begin
    get_xreg := r;
    usage_counts [r] := 1;
  end
  else
    fatal_error ('Register overflow');
end;
$PAGE reg_term
(*  REG TERM is called at the end of code generation for a routine, to make
    sure the code generator has been cleaning up properly after itself.  *)

public procedure reg_term;

var r: registers;

begin
  for r := minimum (registers) to maximum (registers) do
    assert (usage_counts [r] = 0);
end;
$PAGE use_desc
(*  USE DESC will call USE REG for each register in an operand descriptor.  *)

public procedure use_desc ( d : op_desc; delta : integer );

begin
  with d do begin
    if mode in reg_use_modes then
      use_reg (reg, delta);
    if mode in indexed_modes then
      use_reg (index_reg, delta);
  end;
end;
$PAGE duplicate_desc
(*  DUPLICATE DESC takes an operand descriptor and returns a copy of it,
    having first incremented its usage count.  *)

public function duplicate_desc ( d : op_desc ) : op_desc;

begin
  assert (not (d.mode in stack_modes));
  use_desc (d, 1);
  duplicate_desc := d;
end;
$PAGE free_desc
(*  FREE DESC will call FREE REG for each register in an operand descriptor.  *)

public procedure free_desc ( d : op_desc );

begin
  with d do begin
    if mode in reg_use_modes then
      free_reg (reg);
    if mode in indexed_modes then
      free_reg (index_reg);
  end;
end;
$PAGE r_in_desc
(*  R IN DESC is a predicate which tests whether a specified register is
    used in a specified descriptor.  *)

public function r_in_desc ( r : registers; d : op_desc ) : boolean;

begin
  r_in_desc :=
    ( (d.mode in reg_use_modes) andif (r = d.reg) ) or
    ( (d.mode in indexed_modes) andif (r = d.index_reg) );
end;
$PAGE int_size
(*  INT SIZE will return the smallest operand size which can be used to
    represent a specified integer value, with or without a sign bit,
    depending on the SIGN_REQUIRED parameter.  *)

public function int_size ( ival : integer; sign_required : boolean ) : op_sizes;

begin
  if sign_required then begin
    if (ival >= minimum (byte)) and (ival <= maximum (byte)) then
      int_size := size_byte
    else if (ival >= minimum (word)) and (ival <= maximum (word)) then
      int_size := size_word
    else
      int_size := size_long
  end
  else begin
    if (ival >= minimum (byte)) and (ival <= maximum (uns_byte)) then
      int_size := size_byte
    else if (ival >= minimum (word)) and (ival <= maximum (uns_word)) then
      int_size := size_word
    else
      int_size := size_long
  end;
end;
$PAGE conform_size
(* CONFORM_SIZE accepts an operand descriptor and a desired size.  If the
   value_size of the descriptor is greater than that desired and the value
   is not constant or in a register then the operand descriptor must be
   adjusted accordingly. *)

public procedure conform_size (var desc: op_desc; desired_size: op_sizes);

begin
  assert ((desired_size <> no_size) and (desc.value_size <> no_size));
  if desc.value_size > desired_size then begin
    desc := coerce (desc, nonstack_modes, null_mode, any_size, false);
    while desc.value_size > desired_size do begin
      if not (desc.mode in register_modes + [immediate_mode]) then
	case desc.value_size of
	  size_word: desc := increment_addr (desc, 1);
	  size_long: desc := increment_addr (desc, 2);
	  size_double: assert (false) (* should have been a float_op *)
	end;
      desc.value_size := pred (desc.value_size)
    end
  end;
end (* conform_size *);
$PAGE size range tables
public const

    uns_min : array [size_byte..size_word] of integer =
	( minimum (uns_byte), minimum (uns_word) );

    uns_max : array [size_byte..size_word] of integer =
	( maximum (uns_byte), maximum (uns_word) );

    sgn_min : array [size_byte..size_long] of integer =
	( minimum (byte), minimum (word), minimum (longword) );

    sgn_max : array [size_byte..size_long] of integer =
	( maximum (byte), maximum (word), maximum (longword) );
$PAGE reg_desc
(*  REG DESC returns an operand descriptor for a specified register.  *)

public function reg_desc ( reg: registers; size: op_sizes; signed : boolean ): op_desc;

begin
  if reg in data_reg_set
    then reg_desc := descriptors[dreg_mode]
    else reg_desc := descriptors[areg_mode];
  reg_desc.reg := reg;
  reg_desc.value_size := size;
  if reg_desc.mode = dreg_mode then begin
    reg_desc.extended_size := size;
    reg_desc.signed_value := signed;
  end
  else (* reg_desc.mode = areg_mode *) begin
    reg_desc.extended_size := size_long;
    assert (signed);
    assert (size <> size_byte);
  end;
  assert (not ( (size = size_long) and (not signed) ));
end;
$PAGE int_desc
(*  INT DESC returns an operand descriptor for an integer constant.  If no
    size is specified, a minimum size will be used.  *)

public function int_desc ( i : integer; size : op_sizes; sign_required : boolean ) : op_desc;

begin
  int_desc := descriptors[immediate_mode];
  with int_desc do begin
    cst_part.offset := i;
    value_size := int_size (i, sign_required);

    (*  If a size has been specified, make sure that it can be satisfied.  *)

    if size <> no_size then begin
      assert (value_size <= size);
      value_size := size;
    end;

    (*  Determine whether the specified value, represented in the
	indicated size, has a valid sign bit.  *)

    signed_value := (i >= sgn_min [value_size]) and
		    (i <= sgn_max [value_size]);
    known_positive := (i >= 0);
  end;
end;
$PAGE rel_desc
(*  REL DESC returns an operand descriptor for a relocatable value.  If no
    size is specified, the size will be determined from the kind of the
    relocatable value.  *)

public function rel_desc ( rel : reloc_value; size : op_sizes; sign_required : boolean ) : op_desc;

begin
  if rel.kind = absolute_sc then
    rel_desc := int_desc (rel.offset, size, sign_required)
  else begin
    rel_desc := descriptors[immediate_mode];
    rel_desc.cst_part := rel;
    if size <> no_size then
      rel_desc.value_size := size
    else begin
      case rel.kind of
	code_sc,
	external_sc,
	def_sc,
	runtime_sc:
	  rel_desc.value_size := size_long;
	static_sc,
	local_sc,
	parameter_sc:
	  rel_desc.value_size := size_word;
      end;
    end;
  end;
end;
$PAGE def_reloc
(*  DEF RELOC returns a reloc_value record for a specified definition record.  *)

public function def_reloc ( d: def ): reloc_value;

begin
  def_reloc := (0, def_sc, d);
end;
$PAGE sym_reloc
(*  SYM RELOC returns a reloc_value record for a specified symbol.  *)

public function sym_reloc ( s : sym ) : reloc_value;

begin
  sym_reloc.offset := 0;
  sym_reloc.kind := s^.dcl_class;
  sym_reloc.relsym := s;
end;
$PAGE rt_reloc
(*  RT RELOC returns a reloc_value record for a runtime symbol.  *)

public function rt_reloc ( rt : rt_symbol ) : reloc_value;

begin
  rt_reloc := (0, runtime_sc, rt);
end;
$PAGE aconstp
(*  ACONSTP tests whether an operand descriptor represents an absolute
    immediate value, and returns the value if it does.  *)

public function aconstp ( op : op_desc; var val : integer ) : boolean;

begin
  aconstp := (op.mode = immediate_mode) andif (op.cst_part.kind = absolute_sc);
  if aconstp then
    val := op.cst_part.offset;
end;
$PAGE ops_type
(*  OPS TYPE is called to set the size and sign information in an
    operand descriptor in accordance with a specified type.  The flag
    PACKED_CONTEXT is true if the operand occurs in a packed context (i.e.,
    as an element of a packed aggregate).  *)

public procedure ops_type ( var op : op_desc;
				 op_type : typ;
				 packed_context : boolean );

var
  loc_op_type: typ;

begin
  loc_op_type := op_type; (* local copy we can change *)
  while loc_op_type^.kind = indirect_type do
    loc_op_type := loc_op_type^.actual_type;
  assert (loc_op_type <> nil);
  op.signed_value := true;			(* It almost always is. *)
  with loc_op_type^ do begin
    case kind of

      (*  Characters and booleans are always packed in a byte.  *)

      bools,
      chars :
	begin
	  op.value_size := size_byte;
	  op.known_positive := true;
	end;

      (*  Scalars may be minimally packed in a byte or word, but always
	  occupy a word when unpacked.  A byte-length scalar with between
	  129 and 256 values must be unsigned.  *)

      scalars :
	begin
	  if (packable or packed_context) and (maxval <= maximum (uns_byte)) then begin
	    op.value_size := size_byte;
	    op.signed_value := (maxval <= maximum (byte));
	  end
	  else
	    op.value_size := size_word;
	  op.known_positive := true;
	end;

      (*  Integers may be represented in a byte, word, or longword when
	  packed, but require a longword when unpacked.  *)

      ints :
	begin
	  if packable or packed_context then begin
	    if base_size <= bits_per_byte then
	      op.value_size := size_byte
	    else if base_size <= bits_per_word then
	      op.value_size := size_word
	    else
	      op.value_size := size_long;
	    op.signed_value := (minval >= sgn_min[op.value_size]) and
			       (maxval <= sgn_max[op.value_size]);
	  end
	  else
	    op.value_size := size_long;
	  op.known_positive := (minval >= 0);
	end;


      (*  Pointers always require a longword.  *)

      pointers,
      files :
	op.value_size := size_long;

      (*  A single precision real may be stored in a longword.  Double
	  precision reals require a double-word.  *)

      reals :
	if base_size <= bits_per_float
	  then op.value_size := size_long
	  else op.value_size := size_double;

      (*  Procedure and function values always require two longwords --
	  one for the saved static link and one for the code pointer.  *)

      procs,
      funcs :
	op.value_size := size_double;

      (*  The size required for an aggregate is unpredictable; therefore,
	  aggregates are always marked as having no size.  *)

      sets,
      strings,
      records,
      arrays :
	op.value_size := no_size;

    end (* case kind *);
  end (* with loc_op_type^ *);
end (* ops_type *);
$PAGE ops_expr_type_desc
(* OPS EXPR TYPE DESC is a wrapper for OP TYPE.  It sets the size and sign
   information in an operand descriptor given an expr_type_desc.  This routine
   encapsulates knowledge about when the base typ pointer can be used to
   reference the desired type information. *)

public procedure ops_expr_type_desc ( var op : op_desc;
				      type_desc: expr_type_desc;
				      packed_context : boolean );

begin
  if (type_desc.kind <> sets) and (type_desc.base <> nil) then
    ops_type (op, type_desc.base, packed_context)
  else begin
    assert (type_desc.kind in [sets, strings]);
    op.value_size := no_size
  end
end (* ops_expr_type_desc *);
$PAGE dynamic_flex
(*  DYNAMIC FLEX is passed an array or string expression tuple.  It returns
    TRUE if the tuple is on the heap and is flexible.  It may be used to
    determine whether the address of an array or string must be incremented
    to get over a hidden length word to the actual data.  *)

public function dynamic_flex ( e : expr ) : boolean;

begin
  with e^, desc do begin
    dynamic_flex := ( ( (opcode = ptr_ref) or (opcode = field_ref) ) andif
		      ( ( (kind = strings) andif str_flex ) or
			( (kind = arrays) andif base^.flexible ) ) );
  end;
end (* dynamic_flex *);
$PAGE first_sequence, next_sequence, best_sequence
(* FIRST SEQUENCE, NEXT SEQUENCE and BEST SEQUENCE compare the lengths and running times 
   of code sequences, and determine which is preferable according to time-space tradeoff
   heuristics. *)

var
  best_seq, bests_size, bests_cycles: integer;


(* Determine if a longer sequence's speed justifies the extra space.  The percentage
   savings in cycles must be double the loss in space, or else at least 150 cycles
   must be saved per byte spent.  *)

function better (largers_size, largers_cycles, smallers_size, smallers_cycles: integer): boolean;
  begin
    better := (2.0 * (largers_size - smallers_size) / smallers_size  <=
		    (smallers_cycles - largers_cycles) / smallers_cycles )
				orif
	     ( (smallers_cycles - largers_cycles) / (largers_size - smallers_size)  >= 150 )
  end;


public procedure first_sequence (seq, size_in_bytes, cycles: integer);

begin
  best_seq := seq;
  bests_size := size_in_bytes;
  bests_cycles := cycles
end;


public procedure next_sequence (new_seq, new_size, new_cycles: integer);

begin
  if ((new_size = bests_size) and (new_cycles < bests_cycles))  or
     ((new_size > bests_size) and better (new_size, new_cycles, bests_size, bests_cycles))  or
     ((new_size < bests_size) and
			  not better (bests_size, bests_cycles, new_size, new_cycles)) then begin
    best_seq := new_seq;
    bests_size := new_size;
    bests_cycles := new_cycles
  end
end;



public function best_sequence (var predicted_size, predicted_cycles: integer): integer;

begin
  best_sequence := best_seq;
  predicted_size := bests_size;
  predicted_cycles := bests_cycles
end;
public procedure jbrl options special;
var p: ^ integer;
begin
  p := ptr (#o44);
  writeln (tty,'.JBREL = ',p^:6:o); break;
end.
  