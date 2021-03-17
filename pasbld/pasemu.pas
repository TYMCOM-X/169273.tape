$TITLE PASEMU -- New Pascal Expression Manipulation Utilities

module pasemu;


$HEADER pasemu.hdr
$PAGE declarations
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasutl.inc
$SYSTEM pastal.inc
$SYSTEM pasval.inc
$SYSTEM pasifu.inc
$SYSTEM pasesu.inc
$SYSTEM pasxrf.typ
$SYSTEM pasesm.inc
$SYSTEM pasmth.inc
$SYSTEM paserr.inc

$INCLUDE pasemu.typ
$PAGE with_list

(*  Whenever a WITH statement is encountered, PushWith is called with the
    record reference for the statement.  PopWith is called when the WITH
    statement is completed.  These routines maintain the 'with' list, which
    contains a pointer to the record reference expression for each current
    with statement.  *)


public var
  with_list: wl_type; (* Initialized to nil in Body. *)



(*  DupRef is a function which returns 'true' iff the record type of the
    most recent entry in the with list is the same as the record type of
    some other entry in the with list.  *)

function dup_ref: boolean;
var w: wl_type;
begin
  w := with_list^.last;
  while (w <> nil) andif
    (w^.with_ref^.desc.base <> with_list^.with_ref^.desc.base) do
      w := w^.last;
  dup_ref := (w <> nil);
end (* dup_ref *);
$PAGE push_with & pop_with

public procedure push_with ( ref: expr );

var
    w: wl_type;
    fsym: sym;

begin
  w := with_list;
  new (with_list);
  with with_list^ do begin
    with_ref := ref;
    last := w;
    if not dup_ref then begin
      fsym := with_ref^.desc.base^.field_list;          (* Push the field list onto *)
      while fsym <> nil do                      (*   the scope chain. *)
        with fsym^ do begin
          scopechain := name^.scopechain;
          name^.scopechain := fsym;
          fsym := next;
        end;
    end;
  end (* with with_list^ *);
end (* push_with *);


public procedure pop_with ( var ref: expr );
var w: wl_type;
begin
  if not dup_ref then
    pop_scope (with_list^.with_ref^.desc.base^.field_list);
  w := with_list;
  ref := with_list^.with_ref;
  with_list := with_list^.last;
  dispose (w);
end (* pop_with *);
$PAGE bad_expr

(*  BadExpr is a function which returns a newly created expression node
    reflecting an erroneous expression.  *)


public function bad_expr: expr;

begin
  new (bad_expr, nary_op, nary_op, 0);
  initexpr (bad_expr,nil);
  emit (bad_expr);
end (* bad_expr *);
$PAGE fbuffer

(*  Fbuffer creates a buffer reference with a specified base file.  *)


public function fbuffer ( base_file: expr ): expr;

begin
  new (fbuffer, buffer_ref, buffer_ref);
  initexpr (fbuffer, base_file^.desc.base^.component_type);
  fbuffer^.base_file := base_file;
  emit (fbuffer);
end (* fbuffer *);
$PAGE new_ident

(*  NewIdent creates and initializes an IDENT_REF operator referencing a
    specified symbol. *)


public function new_ident ( symbol: sym ): expr;

 begin
  new (new_ident, ident_ref, ident_ref);
  initexpr (new_ident, symbol^.type_desc);
  new_ident^.id_sym := symbol;
  emit (new_ident);
 end;
$PAGE op1, op2, op3
(* OP N creates a tree node with *n* operands.  The "opc"ode and "result_type"
   are given. *)

public function op1 (opc: tuple_opcodes; result_type: typ; o1: expr): expr;
 begin
  new (op1, nary_op, nary_op, 1);
  initexpr (op1, result_type);
  with op1^ do begin
    opcode := opc;
    operand[1] := o1;
  end;
  emit (op1);
 end;

public function op2 (opc: tuple_opcodes; result_type: typ; o1, o2: expr): expr;
 begin
  new (op2, nary_op, nary_op, 2);
  initexpr (op2, result_type);
  with op2^ do begin
    opcode := opc;
    operand[1] := o1;
    operand[2] := o2;
  end;
  emit (op2);
 end;

public function op3 (opc: tuple_opcodes; result_type: typ; o1, o2, o3: expr): expr;
 begin
  new (op3, nary_op, nary_op, 3);
  initexpr (op3, result_type);
  with op3^ do begin
    opcode := opc;
    operand[1] := o1;
    operand[2] := o2;
    operand[3] := o3;
  end;
  emit (op3);
 end;
$PAGE arr_bound

(*  ArrBound takes a bound operator (LWB, UPB, or DIM), an array expression,
    and an index type.  It returns an expression representing the specified
    bound of the index type.  If possible, this will be a constant expression.
    If the index type is generic, or it is flexible and the operator is UPB
    or DIM, then the bound must be computed at runtime.  In this case, a bound
    computation expression is returned, with the array expression as its
    operand.  (Note that a flexible or generic index type can only come from
    a bound function call with a dimension of 1.)  *)


public function arr_bound ( op: tuple_opcodes; arg: expr; index: typ ): expr;

var
    rslt_type: typ;
    rslt_val: int_type;

begin
  if op = dim_op
    then rslt_type := type_int
    else rslt_type := index^.base_type;
  with index^ do begin
    if generic or (flexible and (op <> lwb_op)) then
      arr_bound := op1 (op, rslt_type, arg)
    else begin
      if op = lwb_op then
        rslt_val := minval
      else if op = upb_op then
        rslt_val := maxval
      else
        rslt_val := maxval - minval + 1;
      arr_bound := cst_expr (cst_scalar (rslt_val), rslt_type);
    end;
  end (* with index^ *);
end (* arr_bound *);
$PAGE new_substr

(*  NewSubstr is called with three expressions:  Str, a string-compatible
    expression; Ind, an integer value; and Len, which may be an integer
    value or nil.  It returns a new substr_ref node, with the specified
    string and index, and with the length, if specified, or only two
    operands if not.  NewSubstr also determines the type of the new
    expression on the basis of its operands, and checks constant index
    and length values against the size of the string.  The Ptn parameter
    is the parse node to print a warning message on if they don't check. *)

public function new_substr ( str, ind, slen: expr; is_string: boolean; ptn: parse_node ): expr;

var
    indval: integer; (* The index expression value. *)
    lenval: integer; (* The length expression value. *)
    indcst: boolean; (* True if the index expression is constant. *)
    lencst: boolean; (* True if the length expression is constant. *)
    len: expr;          (* Copy of input argument slen *)
    temp: expr;
    chk: expr;

begin
  new (new_substr, substr_ref, substr_ref);

  with str^.desc do begin
    indcst := constp (ind);
    if indcst
      then indval := ind^.cst_val.ival
      else indval := 1;                 (* innocuous value for static range checks *)

    len := slen;
    if len = nil then begin             (* derive length expression *)
      if (str_kind = nonvarying) and (not str_flex) and indcst
        then begin                      (* have a constant length *)
          lencst := true;
          lenval := str_length - indval + 1;
          len := cst_expr (cst_scalar (lenval), type_int);
        end
        else begin                      (* must generate length expression *)
          lencst := false;
          lenval := 0;                  (* innocuous value *)
          len := op1 (length_op, type_int, str);
          len := op2 (iadd_op, type_int, len, cst_expr (cst_scalar (1), type_int));
          len := op2 (isub_op, type_int, len, ind);
        end;
    end
    else (* len <> nil *) begin         (* check for constant length *)
      lencst := constp (len);
      if lencst
        then lenval := len^.cst_val.ival
        else lenval := 0;
    end;

    if (ind^.desc.kind = unknown_type) or
       ((len <> nil) andif (len^.desc.kind = unknown_type)) then begin
      initexpr (new_substr, nil);
      return;
    end;

    if ( chk_str_opt in cur_block^.semantic_options ) andif
       ( ( (indval < 1) or (lenval < 0) or (indval+lenval > str_length+1) ) orif
         ( (str_kind = varying) or str_flex ) orif
         not ( ( indcst and lencst ) or
               ( (lencst or (slen = nil) ) andif
                 ( (ind^.opcode in [first_data_ref..last_data_ref]) or
                   (ind^.opcode = func_call_op) ) andif
                 ( ind^.desc.base^.minval >= 1 ) andif
                 ( ind^.desc.base^.maxval+lenval <= str_length+1 ) ) ) ) then begin
      if (str_kind = varying) or str_flex
        then temp := op1 (length_op, type_int, str)
        else temp := cst_expr (cst_scalar (str_length), type_int);
      if is_string
        then chk := op3 (str_range_chk, nil, ind, len, temp)
        else chk := op3 (sub_range_chk, nil, ind, cst_expr (cst_scalar (1), type_int), temp);
    end;

    if (indval < 1) or (lenval < 0) or (indval+lenval > str_length+1) then
      err_node (err_str_range, ptn);

    if not is_string then
      initexpr (new_substr, type_char)
    else if lencst then
      initstr (new_substr, false, lenval )
    else
      initstr (new_substr, true, str_length);
  end (* with str^.desc *);

  with new_substr^ do begin
    base_string := str;
    substr_index := ind;
    substr_length := len;
  end (* with new_substr^ *);

  emit (new_substr);
end (* new_substr *);
$PAGE typename

(*  Typename is a utility function which is useful in printing certain
    error messages.  It takes a type node, and returns a pointer to the
    name node for the type, if there is one, or nil otherwise.  *)


public function typename ( type_desc: typ ): nam;

begin
  with type_desc^ do
    if type_id <> nil then
      typename := type_id^.name
    else
      typename := nil;
end (* typename *);
$PAGE op_real

(*  OpReal takes an expression tree designating a real or integer expression,
    and returns a tree for an equivalent real expression of some precision.
    If the argument is real and of the required precision, the input argument
    is returned without transformation.  Otherwise, a float_op is introduced to
    effect the conversion. *)

public function op_real ( op: expr; required_precision: prec_type ): expr;

begin
  if (op^.desc.kind = reals) andif (op^.desc.precision = required_precision) then
    op_real := op
  else if op^.opcode = float_op then begin
    op_real := op;
    op_real^.desc.precision := required_precision;
  end
  else begin
    new (op_real,float_op,float_op,1);
    initexpr (op_real,type_real);
    with op_real^ do begin
      desc.precision := required_precision;
      operand[1] := op;
    end;
    emit (op_real);
  end;
end (* op_real *);
$PAGE pcomponent
PUBLIC FUNCTION pcomponent(ref: expr): BOOLEAN;
(*test function which checks whether a reference is an element
  of a packed structure and is not of a structured type itself*)
BEGIN
  WITH ref^ DO pcomponent := (opcode = substr_ref) ORIF
   (NOT(desc.kind IN [records, arrays, strings]) ANDIF
    (((opcode = field_ref) ANDIF base_rec^.desc.base^.packable) ORIF
     ((opcode = array_ref) ANDIF base_array^.desc.base^.packable))) ORIF
   ((opcode = ident_ref) ANDIF
    (id_sym^.kind = values) ANDIF
    (desc.kind = strings) ANDIF
    (desc.str_kind = nonvarying) ANDIF
    desc.str_flex);
END;
$PAGE tag_scan
(*  TAG SCAN will take a type and a NEW or SIZE argument list.  If the type
    is a variant record type, it will scan the argument list, taking tag
    values from it to return a particular variant.  In this case, it will
    return the selected variant or tag type node, along with a list of the
    tag fields processed and their values, which may be used to set the
    tag values after a NEW call.  Otherwise, TagScan will simply return
    the input type and an empty list.

    If the input type is a flexible array or string type, or a record whose
    field list ends with a flexible array string type, then the argument list
    must end with an actual upperbound expression.  A pointer to this expression
    will also be returned.  *)

public procedure tag_scan ( t: typ; (* the input type *)
			    args: parse_node; (* the NEW or SIZE argument list *)
			    list_tags: boolean; (* create the tag value list? *)
			    var selected: typ; (* the selected type or variant *)
			    var l: tag_value_list; (* the tag values to be set *)
			    var flex_count: expr ); (* the actual upperbound expression *)
$PAGE pick_variant - in tag_scan
(*  PICK VARIANT scans the list of variants under a tag and selects the variant
    identified by a label.  If no variant is found, the tag node itself is
    returned.  

    Since:

      (1) there may be multiple variant nodes with the same fieldlist
          (as in:   RED, BLUE..GREEN: (FIELD: INTEGER) ), and
      (2) the FldVariant pointer in each entry of a variant's fieldlist
          points to the *first* variant node (for "RED" in the above case), and
      (3) the fact that all the FldVariant pointers in a variant's fieldlist
	  have the same value is used in Rtsize to find the last field of a
	  variant (for checking for flexible fields),

    the variant returned is that actually indicated by the fieldlist,
    if one exists. *)

function pick_variant ( tag: typ; labval: val ): typ;

var variant: typ;

begin
  pick_variant := tag;                  (* if no variant found, tag gives correct size *)
  variant := tag^.first_variant;        (* scan list of variants for one with matching label *)
  while variant <> nil do begin
    with variant^ do begin
      if others_var                     (* others variant gives size, if no others found *)
        then pick_variant := variant
      else if (minlab <= labval.ival) and (labval.ival <= maxlab)
        then begin                      (* this variant applies *)

          (* If this variant has a field list and those fields indicate a
             variant other than this one, return that variant instead. *)

          if field_list <> nil
            then pick_variant := field_list^.fld_variant
            else pick_variant := variant;
          return;                       (* <---- exit here *)
        end;
      variant := next_variant;
    end
  end (* while *) ;
end (* pick_variant *);
$PAGE check_flex - in tag_scan
(*  CHECK FLEX takes a type and a SIZE or NEW argument list remainder.  It
    determines whether the type is a flexible array or record type.  If so,
    it takes a count expression from the argument list, returning it in the
    FlexCount parameter.  *)

procedure check_flex ( t: typ; (* the type to be checked *)
		       var a: parse_node; (* the argument list remainder *)
		       var flex_count: expr ); (* the upperbound expression *)

var parm_type: typ;

begin
  if t = nil then begin (* bad type, don't bother checking *)
    a := nil; (* suppress further errors *)
    return; (* <---- exit with bad type *)
  end;

  if not (t^.flexible and (t^.kind in [arrays, strings])) then
    return; (* <---- uninteresting type, return early *)

  (*  The type is flexible.  That means there must be an actual upper bound
      parameter, or we will have an error.  *)

  if a = nil then begin
    err_node (err_flx_upb_required, args); (* print an error message *)
    return; (* <---- no upperbound argument, return early *)
  end;

  if t^.kind = arrays
    then parm_type := t^.index_type (* array index type *)
    else parm_type := type_non_neg; (* strings are indexed by integers *)

  flex_count := copy_semantics (a, parm_type, true); (* evaluate the upperbound expression *)

  a := a^.next; (* remove the upperbound argument *)
end (* check_flex *);
$PAGE tag_scan - main routine
var a: parse_node; (* traverses the argument list *)
    tag: typ; (* the tag field for a variant record *)
    labval: val; (* the constant value of a tag expression *)
    dt: typ; (* a dummy, for the result type of a Constant call *)
    next_l, last_l: tag_value_list;
    field: sym; (* for scanning a field list *)

begin
  a := args^.next; (* the first arg is the type or reference *)
  selected := t;
  l := nil;
  flex_count := nil;
  if t^.kind = records then begin

    while a <> nil do begin (* step through the tag list *)
      if selected^.kind = tags
	then tag := nil (* variant has no field list, no subvariants *)
	else tag := selected^.variant_tag; (* take this variant's case tag *)
      exit if tag = nil; (* nothing more to select *)
      exit if tag^.tag_type = nil do (* ill-defined tag type *)
	a := nil; (*   - suppress further errors *)
      exit if not constant (a, tag^.tag_type, labval, dt) do (* bad tag value *)
	a := nil; (*   - suppress further errors *)
      selected := pick_variant (tag, labval); (* find the selected variant *)
      if list_tags and (tag^.tag_field <> nil) then begin
	new (next_l);
	next_l^.tag := tag^.tag_field;
	next_l^.labval := labval;
	next_l^.next := l;
	l := next_l;
      end;
      a := a^.next;
    end (* while a <> nil *);
$PAGE
    (*  Now find the last field (if any) of the selected variant, and
	check if it is a flexible array or string.  *)

    with selected^ do begin
      if (kind <> tags) andif
	 (field_list <> nil) andif
	 (field_list^.fld_variant = selected) then begin
	field := field_list;
	while (field^.next <> nil) andif (field^.next^.fld_variant = selected) do
	  field := field^.next;
	check_flex (field^.type_desc, a, flex_count);
	if flex_count <> nil then begin
	  new (next_l);
	  next_l^.tag := field;
	  next_l^.next := l;
	  l := next_l;
	end;
      end;
    end;

  end

  (*  If the type isn't a record, it might be a flex array or string.  *)

  else
    check_flex (selected, a, flex_count);

  (*  We should have eaten all the arguments by now.  If not, give an error.  *)

  if a <> nil then
    err_node (err_flex_type_required, a)

  (*  If we have eaten all the arguments, but still have a possibly
      flexible record or variant type, then that's an error, too.  *)

  else with selected^ do begin
    if (kind in [records, variants]) andif (variant_tag <> nil) andif flexible then
      err_node (err_flx_record, args);
  end;
$PAGE
  (*  Finally, reverse the tag value list so it is in the expected order
      (i.e., first tag first).  *)

  last_l := nil;
  while l < do begin
    next_l := l^.next;
    l^.next := last_l;
    last_l := l;
    l := next_l;
  end;
  l := last_l;
end (* tag_scan *);
$PAGE rtsize
(*  RTSIZE is a function which will take a type (possibly a variant
    or tag type node) and a flexible dimension parameter (possibly nil), and
    will return an expression which will compute the type size at runtime.
    Refer to the comments for procedure SizeOf in module PASTAL for further
    explanation of the algorithms used here.  *)

public function rtsize ( data_type: typ; rt_dim: expr; alloc_size: boolean ): expr;

    function pos_const ( n: integer ): expr;
    begin
      pos_const := cst_expr (cst_scalar (n), type_non_neg);
    end;

var a, b, c, d: integer;
    grotesque: boolean;
    x1, x2, x3: expr;

begin
  size_of (data_type, alloc_size, a, b, c, d, grotesque);

  if not grotesque then begin

    (*  Rtsize = (RtDim * A + B) div C.  *)

    if a = 0 then
      x1 := pos_const (b)
    else begin
      if a = 1
	then x1 := rt_dim
	else x1 := op2 (imul_op, type_non_neg, rt_dim, pos_const (a));
      if b > 0 then
	x1 := op2 (iadd_op, type_non_neg, x1, pos_const (b))
      else if b < 0 then
	x1 := op2 (isub_op, type_non_neg, x1, pos_const (-b));
    end;
    if c <> 1 then
      x1 := op2 (idiv_op, type_non_neg, x1, pos_const (c));
  end
$PAGE
  else (* grotesque *) begin

    (*  Rtsize = [ ((RtDim - D) div (A div B)) * A +
		   ((RtDim - D) mod (A div B)) * B + C ] div ByteSize.  *)

    if d = 0
      then x3 := rt_dim
      else x3 := op2 (isub_op, type_non_neg, rt_dim, pos_const (d));
    x1 := op2 (idiv_op, type_non_neg, x3, pos_const (a div b));
    x1 := op2 (imul_op, type_non_neg, x1, pos_const (a));
    x2 := op2 (imod_op, type_non_neg, x3, pos_const (a div b));
    x2 := op2 (imul_op, type_non_neg, x2, pos_const (b));
    x1 := op2 (iadd_op, type_non_neg, x1, x2);
      if c > 0 then
	x1 := op2 (iadd_op, type_non_neg, x1, pos_const (c))
      else if c < 0 then
	x1 := op2 (isub_op, type_non_neg, x1, pos_const (-c));
    x1 := op2 (idiv_op, type_non_neg, x1, pos_const (byte_size));
  end;

  rtsize := x1;
end (* rtsize *).
    ,@Hï