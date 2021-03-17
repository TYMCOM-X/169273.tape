$TITLE pasesm -- Pascal Expression Semantics

module pasesm;

$HEADER pasesm.hdr
$PAGE declarations
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasutl.inc
$SYSTEM pasval.inc
$SYSTEM pascgr.inc
$SYSTEM pasifu.inc
$SYSTEM pascmp.inc
$SYSTEM pasesu.inc
$SYSTEM pasemu.inc
$SYSTEM pa1xrf.inc
$SYSTEM pasglb.inc
$SYSTEM pasref.inc
$SYSTEM passfc.inc
$SYSTEM paserr.inc
$SYSTEM pa1fld.inc
$SYSTEM pastal.inc


public function copy_semantics ( ptn: parse_node; target: typ;
                                 must_check: boolean ): expr;  forward;

public function validate_call ( call: expr; actual_args, call_node: parse_node ): expr;  forward;

public function constant ( ptn: parse_node; reqd_type: typ;
                           var rslt: val; var rslt_type: typ ): boolean;  forward;
$PAGE ck_arg_count
(*  CkArgCount will count the number of arguments in a parse tree argument
    list, and check that it is within the range [min_count..max_count].  If
    it is, CkArgCount will return true.  If there are too many arguments,
    an error message will be printed on the first excess argument.  If there
    are too few arguments, an error message will be printed on the last
    argument or, if there are no arguments, on the function name.  *)


public function ck_arg_count ( arg_list: parse_node;
                               min_count, max_count: parm_range;
                               subr_id: parse_node
                             ): boolean;
var
    nargs: parm_range;
    args, last_arg: parse_node;

begin
  nargs := 0;
  last_arg := subr_id;
  args := arg_list;
  while (args <> nil) and (nargs <> max_count) do begin
    nargs := nargs + 1;
    last_arg := args;
    args := args^.next;
  end;
  if nargs < min_count then
    err_node (err_arg_missing,last_arg);
  if args <> nil then
    err_node (err_arg_extra,args);
  ck_arg_count := (nargs >= min_count) and (args = nil);
end (* ck_arg_count *);
$PAGE var_error
(*  VarError will print a message to the effect that a specified expression
    is not a variable.  *)

procedure var_error ( msg: err_codes; e: expr; ptn: parse_node );

type names = array [sym_kind] of string [20];

const kind_str: names =
     (  'label', 'field name', 'type name', 'constant', '', 'value parameter',
        'for loop index', 'standard procedure', 'standard function',
	'condition', ''  );

var e1: expr;

begin
  e1 := e; (* Find the base expression. *)
  while (e1 <> nil) andif not (e1^.opcode in [ident_ref, cst_ref]) do begin
    case e1^.opcode of
      field_ref: e1 := e1^.base_rec;
      array_ref: e1 := e1^.base_array;
      substr_ref:  e1 := e1^.base_string;
      others: e1 := nil
    end;
  end;

  if e1 = nil then
    err_op (msg, 'The expression', ptn)
  else if e1^.opcode = cst_ref then
    err_op (msg, 'A constant', ptn)
  else
    err_op (msg, 'A ' || kind_str[e1^.id_sym^.kind], ptn);
end (* var_error *);
$PAGE base_semantics
(*  BaseSemantics takes a parse tree and produces an equivalent expression
    tree.  BaseSemantics is the same as ValSemantics (see the next page),
    except that ValSemantics assumes a value context, and BaseSemantics
    allows specification of any context.  *)

public function base_semantics ( ptn: parse_node; expr_context: xrf_class ): expr;

begin
  base_semantics := ref_semantics (ptn, expr_context);          (* Start with a reference. *)
  with base_semantics^, desc do begin

    if kind = unknown_type then (* Erroneous expression tree. *)
      (* no action *)

    else if (opcode = ident_ref) andif
      (id_sym^.kind in [labels,types,conditions]) then begin   (* Wrong kind of symbol. *)
	err_node (err_symb_kind,ptn);
	initexpr (base_semantics,nil);
      end

    else if kind = procs then begin               (* A procedure is being used like a function. *)
      err_node (err_rhs_proc,ptn);
      initexpr (base_semantics,nil);
    end

    else if kind = funcs then begin     (* Function or condition returning a value *)
      if (opcode = ident_ref) andif (id_sym^.kind = std_funcs) then (* Standard function. *)
	base_semantics := std_fcall ( id_sym, nil, expr_context, ptn )
      else              (* User defined function or condition, assume f() *)
	base_semantics := validate_call ( base_semantics, nil, ptn );
    end;
  end (* with *);

  with base_semantics^, desc do begin
    if (kind = sets) andif (base <> nil)   (* Replace a base pointer to a set by a pointer *)
      andif (base^.kind = sets) then            (*   to its maximal subrange element type. *)
	base := base^.set_element_type^.base_type;
  end (* with *);
end (* base_semantics *);
$PAGE val_semantics
(*  ValSemantics takes a parse tree and produces an equivalent expression
    tree representing a value.  Unlike RefSemantics, ValSemantics will
    deprocedure a function name to obtain a value.  ValSemantics will not
    accept a parse tree representing a label, a type, or a procedure, since
    these do not have values per se.  *)

public function val_semantics ( ptn: parse_node ): expr;

begin
  val_semantics := base_semantics (ptn, value_ctxt);
end;
$PAGE trgt_semantics
(*  TrgtSemantics takes a parse tree and produces an equivalent expression
    tree, which is guaranteed to be "assignable to."  *)


public function trgt_semantics ( ptn: parse_node ): expr;

begin
  trgt_semantics := ref_semantics (ptn, mod_ctxt);      (* Start with a reference. *)
  if trgt_semantics^.desc.kind = unknown_type then              (* Erroneous expression tree. *)
    (* no action *)
  else if not assignable (trgt_semantics) then begin (* Not a legal lhs. *)
    var_error (err_not_variable, trgt_semantics, ptn);
    initexpr (trgt_semantics, nil);
  end;
end (* trgt_semantics *);
$PAGE cond_semantics
(*  CondSemantics takes a parse tree and produces an expression tree
    for an IdentRef tuple whose symbol is a condition name.  *)

public function cond_semantics ( ptn: parse_node;
				 user_required, maskable_required: boolean ): expr;

begin
  cond_semantics := ref_semantics (ptn, ref_ctxt);
  with cond_semantics^ do begin

    if (opcode <> ident_ref) orif (id_sym^.kind <> conditions) then begin
      if desc.kind <> unknown_type then begin
	err_node (err_cond_name, ptn);
	initexpr (cond_semantics, nil);
      end
    end

    else if user_required and id_sym^.standard then begin
      err_node (err_cond_user, ptn);
      initexpr (cond_semantics, nil);
    end

    else if maskable_required and not id_sym^.maskable then begin
      err_node (err_cond_maskable, ptn);
      initexpr (cond_semantics, nil);
    end;
  end;
end (* cond_semantics *);
$PAGE subr_semantics
(*  SubrSemantics is the same as RefSemantics, except that a reference to
    the return symbol of a containing function will be replaced by a reference
    to the function itself.  This is used in contexts in which a reference to
    a procedure or function is needed.  *)

public function subr_semantics ( ptn: parse_node ): expr;
begin
  with ptn^ do begin
    if (sym = ident) andif
      (name^.scopechain <> nil) andif
      (name^.scopechain^.kind = vars) andif
      (name^.scopechain^.block^.return_sym = name^.scopechain) then begin
        with name^.scopechain^.scopechain^ do
          subr_semantics := cst_expr (init_value, type_desc);
        xrf_use (name^.scopechain^.scopechain, source, value_ctxt);
      end
    else
      subr_semantics := ref_semantics (ptn, value_ctxt);
  end;
end (* subr_semantics *);
$PAGE count_elems
(*  CountElems is called with the definition chain for an aggregate expression,
    and returns the number of elements in the expression.  *)

function count_elems ( list: parse_node ): int_type;

var elem: parse_node;

begin
  elem := list;
  count_elems := 0;
  while elem <> nil do begin
    count_elems := count_elems + 1;
    elem := elem^.next;
  end;
end (* count_elems *);
$PAGE new_aggregate
(*  NewAggregate is called with a type and an element count, and returns an
    aggregate expression node of the specified type, with room for the
    specified number of elements, but as yet containing no elements.  *)

function new_aggregate ( trgt_type: typ; size: int_type; var nopers: integer ): expr;

begin
  new (new_aggregate,agg_val,agg_val,size);
  initexpr (new_aggregate,trgt_type);
  nopers := 0;
end (* new_aggregate *);
$PAGE new_oper
(*  NewOper is called with a partially constructed aggregate expression node
    and with an array element or a field value to be added to the node.  *)

procedure new_oper ( node: expr; oper: expr; var nopers: integer );

begin
  with node^ do begin
    nopers := nopers + 1;
    operand [nopers] := oper;
    if operand[nopers]^.desc.kind = unknown_type then
      desc.kind := unknown_type;
  end;
end (* new_oper *);
$PAGE record_semantics
(*  RecordSemantics is called with a parse node for a <paren expr> and with
    a type node for a record type.  It returns an AGG_VAL expression tree
    node for an expression of that type, which has been obtained from the
    parenthesized expression.  *)

function record_semantics ( ptn: parse_node; trgt_type: typ ): expr;

var
    def: parse_node; (* Points to successive elements of the
                        value list, as they are evaluated. *)
    cur_var: typ; (* The current record/variant; updated
                     after processing each tag value. *)
    cur_tag: sym; (* The tag field (if any) in the current
                     record/variant. *)
    fields: sym; (* Points to successive symbols in the record
                    field list as they are used. *)
    nopers: integer; (* Count of field values set so far. *)
$PAGE record_semantics - select_next_variant
  (*  When SelectNextVariant is called, the value of 'def' should be a tag
      value.  The 'variant_tag' pointer of the 'cur_var' record or variant
      type node points to the tag description.  SelectNextVariant evaluates
      the tag value as a constant, and then scans the variant list for the tag
      until it finds the correct variant or the list is exhausted.  It then
      sets 'cur_var' to the selected variant, 'cur_tag' to the tag symbol for
      the next sub-variant level (if any), and 'fields' to the field list for
      the variant.  *)

  procedure select_next_variant;

  var
      tag_vars: typ;                            (* Scans the tag variant list. *)
      tvalue: val;                              (* Keeps the actual tag field value. *)
      ttype: typ;                               (* Needed as a result field for Constant. *)

begin
  with cur_var^.variant_tag^ do begin
    cur_var := nil;                             (* Unless we find a variant to select. *)
    fields := nil;
    if constant (def,tag_type,tvalue,ttype) then begin
      new_oper ( record_semantics, cst_expr (tvalue, ttype), nopers );
      tag_vars := first_variant;                (* Get the variant chain to scan. *)
      while tag_vars <> nil do                  (* Scan it. *)
        with tag_vars^, tvalue do begin
          exit if others_var orif ((minlab <= ival) and (ival <= maxlab))
            do begin                            (* The tag matches, so select this variant. *)
              if field_list = nil
                then cur_var := tag_vars
                else cur_var := field_list^.fld_variant;    (* The "master" variant. *)
              if variant_tag = nil
                then cur_tag := nil
                else cur_tag := variant_tag^.tag_field;
              fields := field_list;
            end;
          tag_vars := next_variant;
        end (* while tag_vars <> nil *);
      def := def^.next;
    end
    else (* not constant *) begin               (* A tag value must be a constant. *)
      record_semantics^.desc.kind := unknown_type;
      def := nil;
    end;
  end (* with cur_var^.variant_tag^ *);
end (* select_next_variant *);
$PAGE record_semantics - main routine
begin
  def := ptn^.defn;
  record_semantics := new_aggregate (trgt_type, count_elems(def), nopers);
  cur_var := trgt_type;                 (* The dominating record or variant. *)
  with trgt_type^ do begin
    if variant_tag = nil
      then cur_tag := nil
      else cur_tag := variant_tag^.tag_field;   (* The next variant tag, if any. *)
    fields := field_list;                       (* The field list for the record or variant. *)
  end;

  (*  Process each field value in turn.  *)

  while def <> nil do
    if (fields = nil) orif (fields^.fld_variant <> cur_var) then
      if (cur_var <> nil) andif (cur_var^.variant_tag <> nil) then
        select_next_variant                     (* Undiscriminated union tag value. *)
      else begin
        err_node (err_fld_extra,def);   (* Field list exhausted, more values to store. *)
        def := nil;
      end
    else if fields = cur_tag then               (* Discriminated union tag value. *)
      select_next_variant
    else begin                          (* Evaluate an ordinary field value. *)
      new_oper ( record_semantics, copy_semantics (def, fields^.type_desc, true), nopers );
      fields := fields^.next;           (* On to the next field and value. *)
      def := def^.next;
    end;

  if ((fields <> nil) andif (fields^.fld_variant = cur_var)) orif
    ((cur_var <> nil) andif (cur_var^.variant_tag <> nil)) then
      err_node (err_fld_missing,ptn);   (* Omitted field value. *)
  emit (record_semantics);
  record_semantics := fold (record_semantics, ptn); (* make constant if possible *)
end (* record_semantics *);
$PAGE array_semantics
(*  ArraySemantics is called with a parse node for a <paren expr> and with
    a type node for an array type.  It returns an AGG_VAL expression tree
    node for an expression of that type, which has been obtained from the
    parenthesized expression.

    The unique problem of ArraySemantics is that the elements of a multi-
    dimensional array may be listed in a single list (last subscript varying
    fastest).  This situation is recognized if the array elements are arrays
    themselves, and there are more values in the <paren expr> than there are
    elements in the (top-level) array.

    Therefore, ArraySemantics first determines how many dimensions of
    array structure are represented by the parenthesized list.  Then the
    aggregate expression is built by a recursive procedure.  *)


function array_semantics ( ptn: parse_node; trgt_type: typ ): expr;
$PAGE array_semantics - build_array
(*  BuildArray is called to build the aggregate expression for an array.  The
    'level' parameter indicates the number of array dimensions which have been
    expanded into a single row-major list.  If 'level' is 1, each value in the
    list is an element of the top-level array, and BuildArray simply calls Copy-
    Semantics to evaluate them; If 'level' is 2, the list is two-dimensional,
    with each value representing an element of a second-level array (which is
    itself an element of the top-level array), and BuildArray calls itself to
    obtain each sub-array; and so on recursively.  'Def' is the value list,
    or what is left of it (as BuildArray uses the values, it advances 'def'
    through the list).  'List_size' is the actual number of elements in the
    list; if it is too small, then the aggregate must be short-allocated.
    'Trgt_type' is the type of the array to be constructed.  *)

function build_array ( level: int_type; var def: parse_node;
                      list_size: int_type; trgt_type: typ ): expr;

var nopers: integer;
    array_size: int_type; (* The number of elements in the array. *)
    i: int_type;
    def1: parse_node;

begin
  def1 := def;
  with trgt_type^.index_type^ do
    array_size := maxval - minval + 1;
  array_size := min (array_size, list_size);
  build_array := new_aggregate ( trgt_type, array_size, nopers );
  for i := 1 to array_size do begin
    if level = 1 then begin
      new_oper ( build_array, copy_semantics (def, trgt_type^.element_type, true), nopers );
      def := def^.next;
    end
    else
      new_oper ( build_array, build_array (level-1, def, list_size, trgt_type^.element_type), nopers );
  exit if build_array^.desc.kind = unknown_type;
  end;
  emit (build_array);
  build_array := fold (build_array, def1); (* make constant if possible *)
end (* build_array *);
$PAGE array_semantics - main routine
var nopers: integer;
    def: parse_node; (* Used for stepping through the value list. *)
    list_size: int_type; (* The number of elements in the value list. *)
    array_size: int_type; (* The number of elements in the array. *)
    array_levels: int_type; (* The number of dimensions listed as one. *)
    elem_type: typ; (* Used to trace the sub_arrays of the array. *)


begin

  def := ptn^.defn;
  list_size := count_elems ( def );

  (*  If the array is flexible or generic, then any length list is ok.  *)

  if trgt_type^.flexible then begin
    array_semantics := build_array (1, def, list_size, dcl_array (trgt_type, list_size));
    alloc_type (array_semantics^.desc.base);
    return; (* <---- Return with allocated flex array. *)
  end;


  (*  See how many dimensions are needed to allow for this many elements.  *)

  array_levels := 1;
  with trgt_type^ do begin
    with index_type^ do
      array_size := maxval - minval + 1;
    elem_type := element_type;
  end;

  while (array_size < list_size) and
        ( (elem_type <> nil) andif (elem_type^.kind = arrays) ) do begin
    array_levels := array_levels + 1;
    with elem_type^ do begin
      with index_type^ do
        array_size := array_size * (maxval-minval+1);
      elem_type := element_type;
    end;
  end;

  (*  If the array size (to n dimensions) matches the list size, then create
      the array expression.  Otherwise, print some sort of error message.  *)

  if (list_size >= array_size) orif (array_levels = 1) then
    array_semantics := build_array ( array_levelf, list_size, trgt_type )
  else begin
    array_semantics := new_aggregate (nil, 0, nopers);
    emit (array_semantics);
  end;

  if list_size > array_size then
    err_node (err_elem_extra, def);

  if list_size < array_size then begin
    if array_levels > 1
      then err_node (err_row_major, ptn)
      else err_node (err_elem_missing, ptn);
  end;
end (* array_semantics *);
$PAGE scl_limits
(*  SCL LIMITS returns the limits of a scalar expression.  Assuming that there
    have been no errors in the computation of the expression, its value is
    guaranteed to be between the lower and upper bounds returned by this
    procedure.  *)

public procedure scl_limits (src: expr; var lo_lim, hi_lim: machine_word);

var offset: machine_word;

begin
  with src^ do begin

    (*  The limits of a constant expression are obviously just its value.  *)

    if constp (src) then begin
      lo_lim := cst_val.ival;
      hi_lim := cst_val.ival;
    end

    (*  If the source expression is a data reference or a function result value,
	we can assume that its type bounds are valid.  *)

    else if opcode in [first_data_ref..last_data_ref, func_call_op] then begin
      lo_lim := desc.base^.minval;
      hi_lim := desc.base^.maxval;
    end

    (*  We can make the same assumption for a scalar convert op, since
	checking is done when a scalar conversion is performed.  However,
	we can go farther in this case, and take the result type bounds or
	argument bounds, whichever are tighter.  *)

    else if opcode = sclcvt_op then begin
      scl_limits (operand[1], lo_lim, hi_lim);
      lo_lim := max (lo_lim, desc.base^.minval);
      hi_lim := min (hi_lim, desc.base^.maxval);
    end

    (*  If the source expression has a non-integer scalar type and an iadd_op
	or isub_op opcode, then we can assume that it is the result of a PRED
	or SUCC call applied to a value of that type.  After checking that the
	second operand is in fact a constant, we can still compute good bounds.  *)

    else if (desc.kind <> ints) andif
	    ((opcode = iadd_op) or (opcode = isub_op)) andif
	    constp (operand[2]) then begin
      if opcode = iadd_op
	then offset := operand[2]^.cst_val.ival
	else offset := - operand[2]^.cst_val.ival;
      lo_lim := desc.base^.minval + offset;
      hi_lim := desc.base^.maxval + offset;
    end

    (*  Any other operator which produces a non-integer scalar result
	must be some boolean operator whose result type is guaranteed.
	(Not strictly true--an operand of an and_op might be an invalid
	sclcvt_op--but we'll cross our fingers and hope for the best.)  *)

    else if desc.kind = bools then begin
      lo_lim := type_bool^.minval;
      hi_lim := type_bool^.maxval;
    end

    (*  In the worst case, we have no useful limit information.  *)

    else begin
      lo_lim := minimum (lo_lim);
      hi_lim := maximum (hi_lim);
    end;
  end;
end (* scl_limits *);
$PAGE chk_scalar
(*  ChkScalar is called with a scalar type and a scalar expression node.  If
    it is possible for the expression value to fall outside the bounds of the
    scalar type, then a runtime value check is generated.

    A special case occurs if the target type is flexible.  The only time a
    value is tested against a flexible range is when it is used as the
    upperbound in a NEW or SIZE call; in these cases, the value is allowed
    to be one smaller than the range lower bound (for an empty array), and
    has no upper limit.  *)

public procedure chk_scalar ( target: typ; source: expr; source_node: parse_node );

var chk: tuple;
    lwb_expr, upb_expr: expr;
    lwb, upb, lwlimit, uplimit: machine_word;
    check_lwb, check_upb: boolean;

begin
  with target^ do begin
    if flexible
      then lwb := minval - 1
      else lwb := minval;
    upb := maxval;
  end;

  scl_limits (source, lwlimit, uplimit);

  check_lwb := (lwlimit < lwb);
  check_upb := (uplimit > upb);
 
  (*  If the source expression cannot possibly be in range, then we can
      print a compile-time warning.  *)

  if (lwlimit > upb) or (uplimit < lwb) then
    err_node (err_scl_range, source_node);

  if check_lwb or check_upb then begin

    if check_lwb then
      lwb_expr := cst_expr (cst_scalar (lwb), type_int)
    else
      lwb_expr := nil;

    if check_upb then
      upb_expr := cst_expr (cst_scalar (upb), type_int)
    else
      upb_expr := nil;

    chk := op3 (val_range_chk, nil, source, lwb_expr, upb_expr)

  end
end (* chk_scalar *);
$PAGE cvt_semantics
(*  CvtSemantics takes a semanticated value expression and a target type,
    and attempts to produce an expression representing the conversion of
    the input expression to that type.  It returns the resulting expression,
    and a flag indicating success or failure.  CvtSemantics encapsulates the
    conversion logic of CopySemantics.  The MustCheck flag indicates whether a
    value range check operator should be emitted for scalar type conversions.  *)

public function cvt_semantics ( src: expr; target: typ;
                                source_node: parse_node;
                                must_check: boolean; var success: boolean ): expr;
$PAGE adj_string - in cvt_semantics
(*  AdjString will generate a string conversion operator, if necessary,
    to force its operand to have the specified string type.  *)

function adj_string ( op: expr; str_type: typ ): expr;

begin
  with op^, str_type^ do
    if (desc.kind = strings) andif
       (desc.str_kind = str_kind) andif
       (desc.str_flex = flexible) andif
       ( flexible orif (desc.str_length = str_length) )
      then adj_string := op
      else adj_string := op1 (strcvt_op, str_type, op);
end (* adj_string *);
$PAGE adj_set - in cvt_semantics
(*  AdjSet will generate a set conversion operator to force its operand to
    have a specified type.  The desired set type is the base type of the set
    conversion operator.  Note this peculiarity: unlike all other RHS set
    expressions, a setcvt_op will have as its desc.base the actual set type,
    not the maximal subrange of its element type. *)

function adj_set ( op: expr; set_type: typ ): expr;

begin
  with op^ do begin
    if (desc.set_cst_lwb and desc.set_cst_len) andif
       (desc.set_lwb = set_type^.set_element_type^.minval) andif
       (desc.set_length = set_type^.base_size)
      then adj_set := op
      else adj_set := op1 (setcvt_op, set_type, op);
  end;
end (* adj_set *);
$PAGE cvt_semantics - main routine
var chk: tuple;

begin
  success := true; (* unless proved otherwise *)

  with target^, src^ do begin

    if (kind = reals) and (desc.kind in [reals, ints])  (* int or real of any precision to real *)
      then cvt_semantics := op_real (src, (* trgt *) precision)

    else if ([kind, desc.kind] <= [scalars, ints, bools, chars])
      andif (base_type = desc.base^.base_type) then begin
        if must_check and (chk_val_opt in cur_block^.semantic_options) then
          chk_scalar (target, src, source_node);
      end

    else if (kind = strings) and (desc.kind in [strings, chars])
      then cvt_semantics := adj_string (src, target)

    else if ([kind, desc.kind] <= [sets])
      andif ( ((set_element_type = nil) or (desc.base = nil)) orif
              (set_element_type^.base_type = desc.base^.base_type) )
        then
          cvt_semantics := adj_set (src, target)

    else begin
      cvt_semantics := src; (* default *)
      success := (kind = desc.kind) andif equivtypes (desc.base, target);
    end;
  end (* with target^, src^.desc *);
end (* cvt_semantics *);
$PAGE copy_semantics
(* COPY SEMANTICS takes a parse tree representing an expression and a type
   denoting the type required for the expression.  The tree is processed and
   if the type is not that which is required, conversion is attempted. An 
   error is reported if it is not possible.  Note well:  this routine embodies
   the rules of implicit conversion (i.e. assignment). *)


public function copy_semantics (* ptn: parse_node; target: typ; must_check: boolean ): expr*);

var
    type_name: nam;
    trgt_type: typ;
    ok: boolean;

begin
  trgt_type := target; (* Resolve any type indirection. *)

  while (trgt_type <> nil) andif
    (trgt_type^.kind = indirect_type) do
      trgt_type := trgt_type^.actual_type;

  if trgt_type = nil then                       (* No type specified--take whatever we can get. *)
    copy_semantics := val_semantics (ptn)

  else if trgt_type^.kind in [procs,funcs] then (* Go for a subr name. *)
    copy_semantics := subr_semantics (ptn)

  else if (trgt_type^.kind = records) andif
    (ptn^.sym = paren_expr) then (* Get a record aggregate value. *)
      copy_semantics := record_semantics (ptn,trgt_type)

  else if (trgt_type^.kind = arrays) andif
    (ptn^.sym = paren_expr) then (* Get an array aggregate value. *)
      copy_semantics := array_semantics (ptn,trgt_type)

  else                                          (* An ordinary type--get a simple value. *)
    copy_semantics := val_semantics (ptn);

  (* Check that the value obtained is compatible with the trgt type; if there is
     not an exact match, implicit conversions are introduced. *)

  with copy_semantics^ do begin
    if (desc.kind = unknown_type) or            (* Fall out if we have a default match. *)
      ((trgt_type = nil) orif (trgt_type^.kind = unknown_type)) then
        return;

    copy_semantics := cvt_semantics (copy_semantics, trgt_type, ptn, must_check, ok);

    if not ok then begin                (* types don't match, and conversion not possible *)
      desc.kind := unknown_type;
      type_name := typename (trgt_type);
      if type_name <> nil then
        err_op (err_cvt_named,type_name^.text,ptn)
      else
        err_node (err_cvt_unnamed,ptn);
    end;
  end (* with copy_semantics^ *);
end (* copy_semantics *);
$PAGE check_arg_bounds
(*  CheckArgBounds takes the type node for a formal parameter and the expression
    node for its actual parameter.  If the actual is flexible and the formal
    is not, or the actual is generic and the formal is not, then a runtime
    bounds check must be performed.  *)

procedure check_arg_bounds ( parm_type: typ; arg: expr );

var
    check_lwb, check_upb: boolean;
    comparison: int_type;
    chk_op: tuple_opcodes;
    check: tuple;

begin
  if parm_type = nil then return;
  if (arg^.desc.kind = unknown_type) or (arg^.desc.base = nil) then return;

  with arg^.desc.base^ do begin
    check_lwb := generic and not parm_type^.generic;
    check_upb := flexible and not parm_type^.flexible;
  end;

  if not (check_lwb or check_upb) then return;

  if parm_type^.kind = strings then (* check upper, not lower *)
    check := op2 (compat_chk, nil,
                    op1 (upb_op, type_int, arg),
                    cst_expr (cst_scalar (parm_type^.str_length),  type_int))

  else (* parm_type^.kind = arrays *) begin
    with parm_type^.index_type^ do begin
      if check_lwb and not check_upb then begin
        comparison := minval;
        chk_op := lwb_op;
      end
      else if check_upb and not check_lwb then begin
        comparison := maxval;
        chk_op := upb_op;
      end
      else (* check_lwb and check_upb *) begin
        comparison := maxval - minval + 1;
        chk_op := dim_op;
      end;
    end (* with parm_type^.index_type^ *);

    check := op2 (compat_chk, nil,
                    arr_bound (chk_op, arg, arg^.desc.base^.index_type),
                    cst_expr (cst_scalar (comparison), parm_type^.index_type) );
  end (* if parm_type^.kind = arrays *);
end (* check_arg_bounds *);
$PAGE validate_call
(*  ValidateCall is called with an expression tree node, representing a
    reference to a subroutine, and a portion of a definition chain (from
    a <func qualifier> parse tree node) containing the argument expressions.
    It confirms that the argument list is legal for the subroutine, and
    builds a subr call expression node with the given subr reference and
    argument list.  *)


public function validate_call (* call: expr; actual_args, call_node: parse_node): expr *);

var
    args: parse_node;                           (* Follows the argument list as it is used. *)
    i: parm_range; (* Indexes the parameter descriptors. *)
    type_name: nam;                             (* Used for an error message. *)
    cur_arg: expr;                              (* Points to current actual parm when scanning arglist *)
    have_error: boolean;                        (* Flags if error found in call. *)

begin
  if prog_options.global_opt and in_body then begin
    if call^.opcode = cst_ref then
      writeln (glob_file, ' ', call^.cst_val.blkp^.subr_sym^.name^.text)
    else if call^.opcode = ident_ref then begin
      with call^.id_sym^ do begin
	if (kind = consts) and (dcl_class = external_sc) then begin
	  if init_value.kind = no_value
	    then write (glob_file, ' ')
	    else write (glob_file, '+');
	  with name^ do
	    writeln (glob_file, text)
	end;
      end;
    end;
  end;

  xrf_write (call_xrf);

  (*  If the subroutine being called isn't a parameter, then record the call.  *)

  if (call^.opcode <> ident_ref) orif
    (call^.id_sym^.dcl_class <> parameter_sc) then
      p_calls_q (cur_block, rep_block(call));

  with call^.desc.base^ do begin
    new (validate_call, call_op, call_op, upperbound (params));
    initexpr ( validate_call, return_type );
    with validate_call^ do begin
      case kind of
        procs: opcode := call_op;
        funcs: opcode := func_call_op
      end;
      subr := call; (* The actual subr reference. *)
    end;

    have_error := not ck_arg_count (actual_args, upperbound (params), upperbound (params), call_node);
    args := actual_args;                        (* Get the (unsemanticated) argument list. *)
    i := 1;
    while (args <> nil) and (i <= upperbound (params)) do begin
      with params[i] do begin                   (* Process a parameter. *)
        while (parm_type <> nil) andif (parm_type^.kind = indirect_type) do
          parm_type := parm_type^.actual_type; (* Get the true parameter type. *)
        if (parm_type <> nil) andif (parm_type^.kind = unknown_type) then
          ck_unknown_type (parm_type);

        if parm_kind = values then                      (* Value parameter. *)
          cur_arg := copy_semantics(args,parm_type,true)

        else begin                              (* Var parameter. *)
          cur_arg := ref_semantics(args, var_parm_ctxt);
          with cur_arg^ do begin                (* check that operation is valid *)

            if not equivtypes (desc.base, parm_type) then begin (* Wrong type argument. *)
              type_name := typename (parm_type);
              if type_name <> nil then
                err_op (err_bind_named,type_name^.text,args)
              else
                err_node (err_bind_unnamed,args);
              have_error := true;
            end

            else if not assignable (cur_arg) then begin
              var_error (err_bind_var, cur_arg, args); (* The argument isn't a var. *)
              have_error := true;
            end

            else if pcomponent (cur_arg) then begin (* Can't bind a packed element. *)
              err_node (err_bind_packable,args);
              have_error := true;
            end;
          end (* with cur_arg^ *);
        end (* var parameter *);
        if cur_arg^.desc.kind = unknown_type then
          have_error := true;

        if chk_com_opt in cur_block^.semantic_options then
          check_arg_bounds (parm_type, cur_arg);

        (*  A subroutine parameter is (a) "called" by the routine that passes it,
            and (b) bound to the subr class of its type.  *)

	if (cur_arg^.desc.kind in [procs, funcs]) and not have_error then begin
          p_calls_q (cur_block, rep_block(cur_arg));
          p_calls_q (subr_class(parm_type), rep_block(cur_arg));
        end;
        validate_call^.arglist [i] := cur_arg;
        args := args^.next;
        i := i + 1;
      end (* with params[i] *);
    end (* while args <> nil *);
  end (* with call^.desc.base^ *);

  if have_error then
    validate_call^.desc.kind := unknown_type;
  emit (validate_call);
  xrf_write (end_xrf);
end (* validate_call *);
$PAGE constant
(*  Constant takes a parse tree (of an expression) and an (optional)
    desired type.  It returns parameters representing the (constant)
    value and type of the expression.  It also returns a boolean value
    indicating whether it was able to compute such a value.  *)


public function constant
(*                  ( ptn: parse_node; (* The parse tree to be evaluated. *)
                      reqd_type: typ; (* The required type of the value. *)
                      var rslt: val; (* The constant value of the expression. *)
                      var rslt_type: typ (* The actual type of the value. *)
                    ) : boolean *);             (* True if a good value was found. *)

var rslt_expr: expr;

begin
  rslt_expr := fold (copy_semantics (ptn, reqd_type, true), ptn);
  constant := constp (rslt_expr);
  if constant then
    with rslt_expr^, desc do begin
      rslt := cst_val;
      if reqd_type <> nil then
        rslt_type := reqd_type
      else begin
        case rslt.kind of
          string_cst:
            rslt_type := dcl_string (length(cst_val.valp^.str_val),nonvarying,false);
          set_cst:
            rslt_type := dcl_set (base);
          real_cst:
            with rslt.valp^ do
              rslt_type := dcl_real (real_val, real_val, real_prec);
          scalar_cst:
            rslt_type := base^.base_type;
          others:
            rslt_type := base
        end;
	alloc_type (rslt_type);
      end;
    end
  else begin
    if rslt_expr^.desc.kind <> unknown_type then
      err_node (err_not_constant,ptn);
    rslt.kind := no_value;
    rslt_type := nil;
  end;
end (* constant *).
  yJ+Ù