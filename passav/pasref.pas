$TITLE pasref -- Pascal Reference Semantics
$LENGTH 42

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S R E F                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This is the Reference Semantics  module.  It  contains
        only  a  single  entry  point.  Logically,  PASREF  could  be
        grouped with PASESM; it is a separate module because  of  its
        size and importance.
     
     ENTRY POINTS:
     
        ref_semantics
                    is  the  primary  routine  for  the evaluation of
                    expression   semantics.    Almost    all    other
                    expression    semantics    routines    rely    on
                    ref_semantics.  Ref_semantics is  called  with  a
                    parse   tree   representing  an  expression,  and
                    returns an intermediate form expression tree  for
                    the  same  expression.  Ref_semantics performs as
                    few   transformations   as   possible   on    the
                    expression;  for  example, a function name is not
                    changed into a function  call  unless  the  parse
                    tree  contains  an  explicit <function qualifier>
                    node.
     
     ---------------------------------------------------------------- *)
$PAGE declarations


$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE ptmcon.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE pasutl.inc
$SYSTEM pasval.inc
$INCLUDE pascmp.inc
$INCLUDE pasifu.inc
$INCLUDE pasesu.inc
$INCLUDE pasemu.inc
$INCLUDE pa1xrf.inc
$INCLUDE pasesm.inc
$INCLUDE passfc.inc
$INCLUDE paserr.inc
$INCLUDE pa1fld.inc




public function ref_semantics ( ptn: parse_node; how_used: xrf_class ): expr;
$PAGE set_union_desc

(* SetUnionDesc computes a descriptor for the union of two sets whose types are
   given by the descriptors set1 and set2. *)

function set_union_desc (set1, set2: expr_type_desc): expr_type_desc;
 var desc: expr_type_desc;
 begin
  if (set1.kind = unknown_type) and (set2.kind = unknown_type) then begin
    desc.kind := unknown_type;
    desc.base := nil;
  end

  else if ((set1.set_cst_len) and (set1.set_length = 0))
    then desc := set2                   (* [] + set2 = set2 *)

  else if ((set2.set_cst_len) and (set2.set_length = 0))
    then desc := set1                   (* vice versa *)

  else begin                            (* union of two, valid, nonnull sets *)
    desc.kind := sets;
    desc.base := set1.base;
    desc.set_cst_lwb := set1.set_cst_lwb and set2.set_cst_lwb;
    desc.set_lwb := min (set1.set_lwb, set2.set_lwb);
    desc.set_cst_len := set1.set_cst_len and set2.set_cst_len;
    desc.set_length := max ( set1.set_lwb + set1.set_length - 1,
                             set2.set_lwb + set2.set_length - 1 ) - desc.set_lwb + 1;
  end;

  set_union_desc := desc;
 end;
$PAGE set_both_desc

(* SetDiffDesc computes a descriptor for the intersection of two sets whose types are
   given by the descriptors set1 and set2. *)

function set_both_desc (set1, set2: expr_type_desc): expr_type_desc;
 var desc: expr_type_desc;
 begin
  if (set1.kind = unknown_type) and (set2.kind = unknown_type) then begin
    desc.kind := unknown_type;
    desc.base := nil;
  end

  else if ((set1.set_cst_len) and (set1.set_length = 0))
    then begin                          (* [] * set2 = [] *)
      desc := set1;
      desc.base := set2.base;           (* get kind of elements *)
    end

  else if ((set2.set_cst_len) and (set2.set_length = 0))
    then begin                          (* vice versa *)
      desc := set2;
      desc.base := set1.base;
    end

  else begin                            (* both of two, valid, nonnull sets *)
    desc.kind := sets;
    desc.base := set1.base;
    desc.set_cst_lwb := set1.set_cst_lwb and set2.set_cst_lwb;
    desc.set_lwb := max (set1.set_lwb, set2.set_lwb);
    desc.set_cst_len := set1.set_cst_len and set2.set_cst_len;
    desc.set_length := max (0, min ( set1.set_lwb + set1.set_length - 1,
                                     set2.set_lwb + set2.set_length - 1 ) - desc.set_lwb + 1);
  end;

  set_both_desc := desc;
 end;
$PAGE expr_ident
(*  ExprIdent is called with an identifier parse tree node, and returns
    a WITH REF expression tree node, if the identifier is a field of a record
    in a 'with' scope, or an IDENT REF node if it is an ordinary symbol.
    If the name is undeclared, then ExprIdent generates an error message,
    inserts a dummy 'var' symbol with that name in the symbol table, and returns
    an ERROR EXPR node.  *)


function expr_ident: expr;

var
    dummy_symbol: sym;                          (* Created for an undeclared name. *)
    this_field: sym;                            (* Scans the field symbols in a single scopechain. *)
    w: wl_type; (* Scans the with list. *)
    best_with: wl_type;                 (* The deepest known 'with' var containing a field. *)
    best_field: sym;                            (* The field it contains. *)

begin
  with ptn^.name^  do

    (*  If the symbol is undeclared, then we want to print an error message
        and create a dummy symbol table entry so that we won't repeat the
        error message every time we see a reference to this symbol.  *)

    if scopechain = nil then begin
      err_node (err_undeclared_name,ptn);
      expr_ident := bad_expr();
      dummy_symbol := declare (ptn,vars,nil);   (* Create a dummy 'var' symbol *)
      with dummy_symbol^ do begin               (*   for the undeclared name. *)
        dcl_class := local_sc;
        public_dcl := false;
      end;
    end


    (*  If the symbol is declared and there is at least one active WITH
        statement having a field with this name, then we must check each
        distinct field symbol which is on this scope chain (i.e., which
        has this name and has been opened by a WITH statement), and find
        the one with the deepest record in the 'with' list.  *)

    else if scopechain^.kind = fields then begin
      best_with := nil; (* We know we can do better than nothing. *)
      this_field := scopechain; (* Scans all the fields with this name. *)
      while (this_field <> nil) andif (this_field^.kind = fields) do begin
        w := with_list; (* Find a 'with' with this field. *)
        while (w <> best_with) andif
          (w^.with_ref^.desc.base <> this_field^.fld_record) do
            w := w^.last;
        if w <> best_with then begin (* We've found a better 'with' reference. *)
          best_field := this_field;
          best_with := w;
        end;
        this_field := this_field^.scopechain;
      end (* while *);

      new (expr_ident, field_ref, field_ref); (* Create a 'field ref' node for the field. *)
      initexpr (expr_ident, best_field^.type_desc);
      emit (expr_ident);
      with expr_ident^ do begin
        base_rec := best_with^.with_ref;
        field_sym := best_field;
        xrf_with (base_rec, field_sym, ptn^.source, how_used);
      end;
    end


    (*  The name is a plain, ordinary symbol, so we simply want to return
        an id reference to it.  *)

    else begin
      new (expr_ident,ident_ref,ident_ref);
      initexpr (expr_ident,scopechain^.type_desc);
      emit (expr_ident);
      with expr_ident^ do begin
        if scopechain^.kind = std_procs then    (* A standard procedure or function has a nil *)
          desc.kind := procs;                   (*   type pointer, but isn't of 'unknown_type'. *)
        if scopechain^.kind = std_funcs then
          desc.kind := funcs;
        id_sym := scopechain;
        xrf_use (id_sym, ptn^.source, how_used);
        if id_sym^.block <> cur_block           (* note a non-local reference *)
          then if id_sym^.kind in [consts, vars, values, for_inds]
            then id_sym^.non_local_use := true;

        (*  If the identifier is a constant name, then fold it right away, unless
            this is a reference context.  If this is a reference context, then
            we certainly aren't interested in the value of the constant, and
            we might want to know the symbol later for the cross reference file.  *)

        with id_sym^ do
          if (kind = consts) and (dcl_class <> external_sc) and (how_used <> ref_ctxt) then
            expr_ident := cst_expr (init_value, type_desc);
      end;
    end;
end (* expr_ident *);
$PAGE expr_const
(*  ExprConst is called with an integer, real, or string constant parse
    tree node, and returns a CST REF expression tree node.  *)

function expr_const: expr;

begin
  with ptn^ do
    case sym of

      intconst:
        expr_const := cst_expr (value,type_int);

      stringconst:
        if value.kind = scalar_cst then
          expr_const := cst_expr (value,type_char)
        else
          expr_const := cst_expr (value,nil);

      realconst:
        expr_const := cst_expr (value,type_real);

      nilsy:
        expr_const := cst_expr (cst_nil,type_ptr)

    end (* case ptn^.sym *);
end (* expr_const *);
$PAGE check_array_subscript
(* CheckArraySubscript performs subscript range checking.  If the subscript can be
   validated at compile time, it is; otherwise, a sub_range_check operator is
   emitted to cause a runtime check. *)

procedure check_array_subscript (subscripted_ref: expr; subscript: parse_node);
 var
  idx_type: typ;                        (* type of index of array *)
  check_lwb, check_upb: boolean; (* true if runtime check needed *)
  lwb_expr, upb_expr: expr;
  lwb, upb, lwlimit, uplimit: machine_word;
  chk: tuple;                           (* the check operator *)

begin
  with subscripted_ref^ do begin
    idx_type := base_array^.desc.base^.index_type;
    lwb := idx_type^.minval;
    upb := idx_type^.maxval;

    scl_limits (index_val, lwlimit, uplimit);

    check_lwb := (lwlimit < lwb) or idx_type^.generic;
    check_upb := (uplimit > upb) or idx_type^.flexible;

    if (uplimit < lwb) or (lwlimit > upb) then
      err_node (err_sub_range, subscript);

    if check_lwb or check_upb then begin

      if check_lwb then
	lwb_expr := arr_bound (lwb_op, base_array, idx_type)
      else
	lwb_expr := nil;
 
      if check_upb then
	upb_expr := arr_bound (upb_op, base_array, idx_type)
      else
	upb_expr := nil;
 
      chk := op3 (sub_range_chk, nil, index_val, lwb_expr, upb_expr)

    end
  end (* with *) ;
end (* check_array_subscripts *);
$PAGE expr_array
(*  ExprArray takes an <array qualifier> parse tree node.  If its base
    array node is of array type, it returns an ARRAY REF expression tree
    node.  If the base array node is of string type, it returns a SUBSTR
    REF expression tree node.  If there are semantic errors in the array
    qualifier, ExprArray generates an error message and returns an ERROR
    EXPR node instead.

    <array qualifier>
        |
    <base array> -- <subscript 1> -- .. -- <subscript n>  (n >= 1)

    <subscript> ::= <expression>  |  ":"
                                      |
                                     <expression> -- <expression>  *)


function expr_array: expr;

var
    def: parse_node;                            (* Follows the definition chain of Ptn. *)
    temp_ref: expr;                             (* Temporary storage for the result expression. *)
    subscripted: boolean;                       (* Set true when an array reference is evaluated. *)
    substrung: boolean;                         (* Set true when a substring reference is evaluated. *)
    index: int_type; (* Used for constant subscript checking. *)
    idx_type: typ; (* Used for constant subscript checking. *)


begin
  subscripted := false;                         (* No array subscripts evaluated yet. *)
  substrung := false;                           (* No substring references yet, either. *)
  def := ptn^.defn;                             (* Get the base array or string expression. *)
  expr_array := base_semantics (def,how_used);          (* Evaluate it. *)
  xrf_write (index_xrf);
  def := def^.next;                             (* Get the first subscript or string index. *)
  while def <> nil do begin                     (* Process all the subscripts and indices. *)

    (*  If the base expression is an array, then this must be a simple
        array reference.  All that we have to worry about is whether
        the subscript type is appropriate for this array type.  (In
        particular, a ":" subscript is never appropriate for an array
        reference.  *)

    if expr_array^.desc.kind = arrays then begin
      new (temp_ref,array_ref,array_ref);       (* Create the array reference node. *)
      temp_ref^.base_array := expr_array;       (* Make it the return node. *)
      expr_array := temp_ref;
      initexpr (expr_array,nil);                (* Until we know the reference is ok. *)
      with expr_array^, base_array^.desc.base^ do
        if def^.sym = colon then                (* Colon is only legal for substrings. *)
          err_node (err_sub_length,def)
        else begin
          index_val := copy_semantics (def,index_type,false);   (* Evaluate the index. *)
          if index_val^.desc.kind <> unknown_type then begin
            initexpr (expr_array,element_type);
            if chk_sub_opt in cur_block^.semantic_options then
              check_array_subscript (expr_array, def)
            else if constp (expr_array^.index_val) then begin
              index := expr_array^.index_val^.cst_val.ival;
              idx_type := expr_array^.base_array^.desc.base^.index_type;
              if (index < idx_type^.minval) or (index > idx_type^.maxval) then
                err_node (err_sub_range, def);
            end;
          end;
        end;
      emit (expr_array);
      subscripted := true;                      (* We have processed an array reference. *)
    end


    (*  If the base expression is a string, and we haven't already
        evaluated a substring reference, then that's what this must
        be.  *)

    else if (expr_array^.desc.kind = strings) and (not substrung) then begin
      if def^.sym = colon then begin (* [Index:Length] specified. *)
        expr_array := new_substr ( expr_array,
                                   copy_semantics (def^.defn, type_int,false),
                                   copy_semantics (def^.defn^.next, type_int,false),
                                   true,
                                   def );
      end
      else begin (* [Index] specified; default length is one. *)
        expr_array := new_substr ( expr_array,
                                   copy_semantics (def,type_int,false),
                                   cst_expr (cst_scalar(1), type_int),
                                   false,
                                   def );
      end;
      substrung := true;                        (* We have processed a substring reference. *)
    end


    (*  We have an index that we shouldn't have.  The base expression may
        not have been an array or a string at all; or it may have been an
        array, but we've gotten more subscripts than it had dimensions; or
        we may have an extra subscript on a substring reference.  *)

    else begin
      if expr_array^.desc.kind <> unknown_type then begin
        if def^.sym = colon then                (* A colon--base should have been a string. *)
          err_node (err_not_string,ptn^.defn)
        else if subscripted then                (* Base was an array, but too many subscripts. *)
          err_node (err_sub_count,def)
        else if substrung then                  (* Only one subscript per string, please. *)
          err_node (err_str_count,def)
        else                                    (* The original base expression type was bad. *)
          err_node (err_not_array,ptn^.defn);
        initexpr (expr_array,nil);              (* Return the base as an error node. *)
      end;
      if def^.sym = colon then begin            (* Evaluate and discard an unused *)
        temp_ref := val_semantics(def^.defn);   (*   index:length pair. *)
        temp_ref := val_semantics(def^.defn^.next);
      end
      else                                      (* Evaluate and discard an unused subscript. *)
        temp_ref := val_semantics(def);
    end;


    def := def^.next;
  end (* while def <> nil *);
  xrf_write (end_xrf);
end (* expr_array *);
$PAGE expr_field
(*  ExprField takes a <field qualifier> parse tree node.  It returns a FIELD
    REF expression tree node.  If the field does not match the record type,
    it will generate an error message and return an ERROR EXPR node.

    <field qualifier>
        |
    <base record> [ -- <identifier> ] (may be missing, if field name omitted)  *)

function expr_field: expr;

var
    def: parse_node;                            (* Follows the definition chain of Ptn. *)

begin
  new (expr_field,field_ref,field_ref);         (* Create the field reference node. *)
  initexpr (expr_field,nil);                    (* By default, until we known everything's ok. *)
  with expr_field^ do begin
    field_sym := nil;                           (* By default, until we look up the field name. *)
    def := ptn^.defn;                           (* Get the record expression parse tree *)
    base_rec := base_semantics (def,how_used);          (*   and evaluate it. *)
    with base_rec^.desc do

      if kind = records then begin              (* At least the base is a record. *)
        def := def^.next;                       (* Get the field name. *)
        if def <> nil then begin                (* Search the recorld list for the name. *)
          field_sym := (*base_rec^.desc.*)base^.field_list;
          with def^ do
            while (field_sym <> nil) andif (field_sym^.name <> name) do
              field_sym := field_sym^.next;
          if field_sym <> nil then begin                (* We have a valid reference. *)
            initexpr (expr_field,field_sym^.type_desc);
            xrf_use (field_sym,def^.source,field_xrf);
          end
          else                                  (* The name is not a field of this record. *)
            err_node (err_invalid_field,def);
        end (* def <> nil *);
      end

      else if kind = unknown_type then          (* Error in the record expression. *)
        (* no action *)

      else                                      (* The base isn't a record-- *)
        err_node (err_bad_record,def);          (*   print an error message. *)
  end (* with expr_field^ *);
  emit (expr_field);
end (* expr_field *);
$PAGE expr_ptr
(*  ExprPtr takes a <pointer qualifier> parse tree node.  It returns a PTR REF
    expression tree node.  If the parse tree node is not of pointer type, then
    it generates an error message and returns an ERROR EXPR node instead.

    <pointer qualifier>
        |
    <base pointer>  *)


function expr_ptr: expr;
var base_expr, chk: tuple;
begin
  expr_ptr := nil; (* Until properly set. *)
  base_expr := val_semantics (ptn^.defn); (* Get the pointer reference. *)
  with base_expr^.desc do begin
    if base = type_ptr then (* We can't dereference NIL or a PTR var. *)
      err_node (err_ptr_deref, ptn)
    else if kind = pointers then begin (* No problems. *)
      new (expr_ptr, ptr_ref, ptr_ref);
      initexpr (expr_ptr, base^.target_type);
      expr_ptr^.base_ptr := base_expr;
      xrf_write (deref_xrf);
      xrf_use (base^.heap_class, ptn^.source, how_used);
      if chk_poi_opt in cur_block^.semantic_options then
        chk := op1 (ptr_chk, nil, base_expr);
      emit (expr_ptr);
    end
    else if kind = files then begin (* No problems. *)
      if base^.file_kind = binaryfile then
        err_node (err_bin_buffer, ptn)
      else begin
        if chk_fil_opt in cur_block^.semantic_options then
          chk := op1 (file_chk, nil, base_expr);
        expr_ptr := fbuffer (base_expr);
        xrf_write (fileblk_xrf);
        xrf_use (base^.file_class, ptn^.source, how_used);
      end;
    end
    else if kind = unknown_type then (* Error in pointer expression. *)
      (* no action *)
    else (* The base isn't a pointer. *)
      err_node (err_not_pointer, ptn^.defn);
  end (* with base_expr^.desc *);
  if expr_ptr = nil
    then expr_ptr := bad_expr (); (* Not a valid expression. *)
end (* expr_ptr *);
$PAGE expr_func
(*  ExprFunc takes a <function qualifier> parse tree node, and returns a
    SUBR REF expression tree node.  If the actual parameters don't match
    up with the formal parameters, then it generates an error message and
    returns an ERROR EXPR node instead.

    <func qualifier>
        |
    <subr name> -- <expression 1> -- .. -- <expression n>  (n >= 0)  *)


function expr_func: expr;

var
    tnode: parse_node;
    subr_val: expr;

begin
  expr_func := nil;
  with ptn^ do begin
    subr_val := subr_semantics (defn);          (* Get the function name. *)
    with subr_val^, desc do

      if kind = unknown_type then (* Bad expression -- error already flagged. *)
        (* no action *)

      else if (kind in [chars,bools,scalars,pointers]) and (* Scalar type transfer function? *)
        ((opcode = ident_ref) andif (id_sym^.kind = types)) then
          expr_func := std_fcall (id_sym, defn, how_used, defn)

      else if (opcode = ident_ref) andif
        (id_sym^.kind in [labels,types,conditions]) then (* Not allowed. *)
          err_node (err_symb_kind,ptn)

      else if kind = funcs then                   (* Evaluate the function call. *)
        if (opcode = ident_ref) andif (id_sym^.kind = std_funcs)
          then expr_func := std_fcall (id_sym,defn^.next,how_used,defn)
          else expr_func := validate_call ( subr_val, defn^.next, ptn )

      else if kind = procs then                   (* A procedure is used like a function. *)
        err_node (err_rhs_proc,ptn)

      else if (kind = arrays) and (defn^.next <> nil) then begin (* Really an array ref. *)
        err_node (err_sub_brackets,ptn);
        expr_func := expr_array ();
      end

      else                                      (* The function name isn't a function name. *)
        err_node (err_not_subr,ptn);
  end;

  if expr_func = nil then
    expr_func := bad_expr ();
end (* expr_func *);
$PAGE expr_binop
(*  ExprBinop takes an <addop>, <mulop>, or <relop> parse tree node, and
    returns an appropriate binary op ref expression tree node.

    <bin op>
        |
    <expression> -- <expression>  *)


function expr_binop: expr;
$PAGE bop_select - in expr_binop

(*  BopSelect takes a lexical operator and the type kinds of its two arguments,
    and returns the specific operaotr called for.  BopSelect checks only the
    type kinds, not the types themselves; for example, a relational test between
    variables of two different scalar types would have to be detected by the
    scalar relational operator code.  If a compatibility error is detected,
    the universal NARY OP is returned.  An operand of unknown type is not a
    compatibility error, however; so the calling routine must check for this case
    separately.  This means that we only return NARY OP if there is actually
    of an unacceptable type; and that if one operand is of unknown type, then we
    will still check if the type of the other operand is compatible with the
    operator.  *)


function bop_select ( op: operators; lkind, rkind: type_kind ): tuple_opcodes;

var kinds: set of type_kind;

type
    relops = array [leop..neop] of tuple_opcodes;
    boolops = array [andop..orifop] of tuple_opcodes;

const
    scalar_relop: relops = ( ile_op,    ilt_op,    igt_op,    ige_op,    ieq_op,    ine_op   );
    real_relop:   relops = ( rle_op,    rlt_op,    rgt_op,    rge_op,    req_op,    rne_op   );
    string_relop: relops = ( sle_op,    slt_op,    sgt_op,    sge_op,    seq_op,    sne_op   );
    set_relop:    relops = ( setle_op,  nary_op,   nary_op,   setge_op,  seteq_op,  setne_op );
    ptr_relop:    relops = ( nary_op,   nary_op,   nary_op,   nary_op,   ptreq_op,  ptrne_op );
    file_relop:   relops = ( nary_op,   nary_op,   nary_op,   nary_op,   fileq_op,  filne_op );

    bool_op: boolops = ( and_op, andif_op, nary_op, nary_op, or_op, orif_op );


begin
  bop_select := nary_op;                        (* Default, in case of incompatibilities. *)
  kinds := [lkind,rkind] - [unknown_type];      (* If both kinds are unknown, then some opcode
                                                   will be selected, whatever the operator, and
                                                   no error message will be printed.  If just
                                                   kind is unknown, then this will still check
                                                   check the type of the other operand. *)
  case op of 

    plus:
      if kinds <= [ints] then bop_select := iadd_op
      else if kinds <= [reals, ints] then bop_select := radd_op
      else if kinds <= [sets] then bop_select := union_op;

    minus:
      if kinds <= [ints] then bop_select := isub_op
      else if kinds <= [reals, ints] then bop_select := rsub_op
      else if kinds <= [sets] then bop_select := diff_op;

    mul:
      if kinds <= [ints] then bop_select := imul_op
      else if kinds <= [reals, ints] then bop_select := rmul_op
      else if kinds <= [sets] then bop_select := both_op;

    rdiv:
      if kinds <= [reals, ints] then bop_select := rdiv_op;

    idiv:
      if kinds <= [ints] then bop_select := idiv_op;

    imod:
      if kinds <= [ints] then bop_select := imod_op;


    expon:
      if kinds <= [reals, ints] then
        if rkind = ints then
          if lkind = ints then
            bop_select := expii_op
          else
            bop_select := expri_op
        else
          bop_select := exprr_op;

    catop:
      if kinds <= [chars, strings] then bop_select := cat_op;

    leop, ltop, gtop, geop, neop, eqop:
      if kinds <= [ints, bools, chars, scalars] then bop_select := scalar_relop [op]
      else if kinds <= [reals, ints] then bop_select := real_relop [op]
      else if kinds <= [strings, chars] then bop_select := string_relop [op]
      else if kinds <= [sets] then bop_select := set_relop [op]
      else if kinds <= [pointers] then bop_select := ptr_relop [op]
      else if kinds <= [files] then bop_select := file_relop [op];

    inop:
      if (lkind in [ints,bools,chars,scalars,unknown_type]) and
        (rkind in [sets,unknown_type]) then
          bop_select := in_op;

    andop, orop, andifop, orifop:
      if kinds <= [bools] then
        bop_select := bool_op [op]

  end (* case op *);
end (* bop_select *);
$PAGE op_semantics - in expr_binop

(*  OpSemantics takes an expression node representing some binary operator,
    verifies the specific compatibility of its operands, if necessary (for
    example, that two pointer types have the same target), and sets up the
    result type information in the node.  A compatibility error is indicated
    by setting the operator to the ubiquitous NARY OP.  *)


procedure op_semantics ( op: expr );

var
    str_length: char_range;
    temp_prec: prec_type;

const
    max_prec_log = 7; (* This is somewhere in the vicinity of
                         log2 (maximum (int_prec) ).  If the integer
                         precision of an exponent is this big, then
                         the integer precision of the result will be
                         the maximum possible integer precision. *)

 function real_prec (op: expr): prec_type;      (* return minimum real precision of operand *)
  begin
   if op^.desc.kind = reals
     then real_prec := op^.desc.precision
     else real_prec := type_real^.precision     (* this is minimum precision for an integer *)
  end;


begin
  with op^ do
    case opcode of

      (* In processing the integer operators to determine the apparent precision
         of the result, we assume that the operand types are valid.  The
         precision computations for integer expressions are discussed in
         a separate note, "Precision of Integer Arithmetic Expressions".  *)

      iadd_op:
        begin
          initexpr (op, type_int);
          desc.signed := operand[1]^.desc.signed or operand[2]^.desc.signed;
          desc.int_prec :=
            min ( max ( operand[1]^.desc.int_prec - ord (operand[1]^.desc.signed),
                        operand[2]^.desc.int_prec - ord (operand[2]^.desc.signed) )
                    + 1 + ord (desc.signed),
                  int_prec_limit );
        end;

      isub_op:
        begin
          initexpr (op, type_int);
          desc.signed := true;
          desc.int_prec :=
            min ( max ( operand[1]^.desc.int_prec - ord (operand[1]^.desc.signed),
                        operand[2]^.desc.int_prec - ord (operand[2]^.desc.signed) )
                    + 1 + ord (desc.signed),
                  int_prec_limit );
        end;

      imul_op:
        begin
          initexpr (op, type_int);
          desc.signed := operand[1]^.desc.signed or operand[2]^.desc.signed;
          desc.int_prec :=
            min ( operand[1]^.desc.int_prec + operand[2]^.desc.int_prec,
                  int_prec_limit );
        end;

      idiv_op:
        begin
          initexpr (op, type_int);
          desc.signed := operand[1]^.desc.signed or operand[2]^.desc.signed;
          desc.int_prec :=
            min ( operand[1]^.desc.int_prec + ord (operand[2]^.desc.signed),
                  int_prec_limit );
        end;

      imod_op:
        begin
          initexpr (op, type_int);
          desc.signed := operand[1]^.desc.signed;
          desc.int_prec :=
            min ( min ( operand[1]^.desc.int_prec - ord (operand[1]^.desc.signed),
                        operand[2]^.desc.int_prec - ord (operand[2]^.desc.signed) )
                    + ord (desc.signed),
                  int_prec_limit );
        end;

      expii_op:
        begin
          initexpr (op, type_int);
          desc.signed := operand[1]^.desc.signed;
          if operand[2]^.desc.int_prec > max_prec_log then
            desc.int_prec := int_prec_limit
          else
            desc.int_prec :=
              min ( (operand[1]^.desc.int_prec - ord (operand[1]^.desc.signed))
                      * (2 ** (operand[2]^.desc.int_prec - ord(operand[2]^.desc.signed))
                           - 1)
                       + ord (desc.signed),
                    int_prec_limit );
        end;

      radd_op, rsub_op, rmul_op, rdiv_op, exprr_op:
        begin
          initexpr (op, type_real);
          desc.precision := max (real_prec (operand[1]), real_prec (operand[2]));
          operand[1] := op_real (operand[1], desc.precision);
          operand[2] := op_real (operand[2], desc.precision);
        end;

      expri_op:
        begin
          initexpr (op,type_real);
          op^.desc.precision := operand[1]^.desc.precision;
        end;

      ptreq_op, ptrne_op, fileq_op, filne_op,
      setle_op, setge_op, seteq_op, setne_op:
        if equivtypes (operand[1]^.desc.base,operand[2]^.desc.base) then
          initexpr (op,type_bool)
        else
          opcode := nary_op;

      ile_op, ilt_op, igt_op, ige_op, ieq_op, ine_op, in_op:
        if equivtypes (operand[1]^.desc.base^.base_type,operand[2]^.desc.base^.base_type) then
          initexpr (op,type_bool)
        else
          opcode := nary_op;

      rle_op, rlt_op, rgt_op, rge_op, req_op, rne_op:
        begin
          temp_prec := max (real_prec (operand[2]), real_prec (operand[1]));
          operand[1] := op_real (operand[1],temp_prec);
          operand[2] := op_real (operand[2],temp_prec);
          initexpr (op,type_bool);
        end;

      sle_op, slt_op, sgt_op, sge_op, seq_op, sne_op:
        initexpr (op, type_bool);

      diff_op:
        if equivtypes (operand[1]^.desc.base,operand[2]^.desc.base)
          then op^.desc := operand[1]^.desc     (* result contained in 1st set *)
          else opcode := nary_op;

      union_op:
        begin
          if equivtypes (operand[1]^.desc.base, operand[2]^.desc.base)
            then op^.desc := set_union_desc (operand[1]^.desc, operand[2]^.desc)
            else opcode := nary_op;
        end;

      both_op:
        begin
          if equivtypes (operand[1]^.desc.base, operand[2]^.desc.base)
            then op^.desc := set_both_desc (operand[1]^.desc, operand[2]^.desc)
            else opcode := nary_op;
        end;

      or_op, orif_op, and_op, andif_op:
        initexpr (op,type_bool);

      cat_op:
        begin
          str_length := min ( maximum (char_range),
                              operand[1]^.desc.str_length + operand[2]^.desc.str_length );
          if (operand[1]^.desc.str_kind = varying) or (operand[2]^.desc.str_kind = varying) or
             (operand[1]^.desc.str_flex) or (operand[2]^.desc.str_flex) then
              initstr (op,true,str_length)
            else
              initstr (op,false,str_length);
        end

    end (* case opcode *);
end (* op_semantics *);
$PAGE expr_binop - main routine

var
    op_sym: string [5];
    left, right: parse_node;
    l_kind, r_kind: type_kind;
    l_err, r_err: boolean;
    temp: expr;
    i, opcnt1, opcnt2: 0..4095;

type los_type = array [operators] of string [5];
const lex_op_syms: los_type :=
      ( '*', '/', 'DIV', '**', 'MOD', 'AND', 'ANDIF', '+', '-', 'OR',
        'ORIF', '||', '<=', '<', '>', '>=', '=', '<>', 'IN',
        'READ', 'WRITE', 'RDLN', 'WRTLN', 'RDRN', 'WRTRN',
        'GETST', 'PUTST', '*NOP*' );

begin
  new (expr_binop, nary_op, nary_op, 2); (* Create the operator node. *)
  with expr_binop^, ptn^ do begin
    op_sym := lex_op_syms [op];
    left := ptn^.defn;                          (* Get the left operand. *)
    operand[1] := val_semantics (left);
    l_kind := operand[1]^.desc.kind;
    right := left^.next;                        (* Get the right operand. *)
    operand[2] := val_semantics (right);
    r_kind := operand[2]^.desc.kind;


    (*  Select the proper intermediate form operator for this lexical operator
        and these operands.  *)

    opcode := bop_select (op,l_kind,r_kind);

    if opcode = nary_op then begin              (* Operator/operand compatibility error. *)
      l_err := (bop_select (op,l_kind,unknown_type) = nary_op);   (* Is left op bad? *)
      if l_err then
        err_op (err_operands_bad,op_sym,left);
      r_err := (bop_select (op,unknown_type,r_kind) = nary_op);   (* Is right op bad? *)
      if r_err and ((not l_err) or (l_kind <> r_kind)) then
        err_op (err_operands_bad,op_sym,right);
      if not (l_err or r_err) then
        err_op (err_incompatible_types,op_sym,right);
    end;

    if (l_kind = unknown_type) or (r_kind = unknown_type) then
      opcode := nary_op;                        (* BopSelect might not have marked this one. *)

    (*  Check the compatibility of the operands, and set the type information
        for this expression.  *)

    if opcode = nary_op then
      initexpr (expr_binop,nil)
    else begin
      op_semantics (expr_binop);
      if opcode = nary_op then begin            (* Operand/operand compatibility error. *)
        err_op (err_incompatible_types,op_sym,right);
        initexpr (expr_binop,nil);
      end;
    end;

    (* Transform certain binary operators into nary operators. *)

    if (opcode = union_op) or (opcode = cat_op) then begin
      if operand[1]^.opcode = opcode
	then opcnt1 := upperbound (operand[1]^.operand)
        else opcnt1 := 1;
      if operand[2]^.opcode = opcode
	then opcnt2 := upperbound (operand[2]^.operand)
        else opcnt2 := 1;
      if (opcnt1 + opcnt2) > 2 then begin       (* alloc new node only if necessary *)
        new (temp, nary_op, nary_op, opcnt1 + opcnt2);
        temp^.opcode := opcode;
        temp^.desc := desc;
        if operand[1]^.opcode = opcode
          then for i := 1 to opcnt1 do temp^.operand[i] := operand[1]^.operand[i]
          else temp^.operand[1] := operand[1];
        if operand[2]^.opcode = opcode
          then for i := 1 to opcnt2 do temp^.operand [i+opcnt1] := operand[2]^.operand[i]
          else temp^.operand[opcnt1+1] := operand[2];
        dispose (expr_binop);
        expr_binop := temp;
      end;
    end;
  end;
  emit (expr_binop);
end (* expr_binop *);
$PAGE expr_signop
(*  ExprSignop takes a <sign op> parse tree node, and returns the appro-
    priate neg op expression tree node, if the sign is '-', or the argument
    of the sign op, if the sign is '+'.  A negative constant will be folded
    here, rather than being deferred to the Fold operation.

    <sign op>
        |
    ["+" | "-"] -- <expression>  *)


function expr_signop: expr;

var
    def: parse_node;

begin
  def := ptn^.defn;
  if def^.op = plus then begin                  (* Unary "+" op. *)
    expr_signop := val_semantics (def^.next);   (* No operator needed. *)
    with expr_signop^.desc do
      if not (kind in [ints,reals,unknown_type]) then begin
        err_op (err_operands_bad,'+',def^.next);
        initexpr (expr_signop,nil);
      end;
  end


  else begin                                    (* Unary "-" op. *)
    new (expr_signop,nary_op,nary_op,1); (* Create the unary "-" operator node. *)
    initexpr (expr_signop,nil);                 (* Default, until we know a true type. *)
    with expr_signop^ do begin
      operand[1] := val_semantics (def^.next);  (* Get the operand[1]. *)
        with operand[1]^.desc do
          case kind of
            unknown_type:
              (* no action *);
            ints:  begin
              initexpr (expr_signop,type_int);
              desc.int_prec := operand[1]^.desc.int_prec +
                               ord (not operand[1]^.desc.signed);
              opcode := ineg_op;
            end;
            reals:  begin
              initexpr (expr_signop,type_real);
              desc.precision := operand[1]^.desc.precision;
              opcode := rneg_op;
            end;
            others:
              err_op (err_operands_bad,'-',def^.next)
          end (* kind *);
    end (* with expr_signop^ *);
    emit (expr_signop);
    if expr_signop^.operand[1]^.opcode = cst_ref then    (* Fold a negative constant. *)
      expr_signop := fold (expr_signop, def^.next);
  end (* unary "-" op *);
end (* expr_signop *);
$PAGE expr_notop
(*  Expr_notop takes a <not op> parse tree node, and returns a bnot op
    expression tree node.

    <not op>
        |
    <expression>  *)


function expr_notop: expr;

begin
  new (expr_notop,bnot_op,bnot_op,1);                           (* Create the bnot operator node. *)
  initexpr (expr_notop,nil);                    (* Default, until we know it's legal. *)
  with expr_notop^ do begin
    operand[1] := val_semantics (ptn^.defn);    (* Get the boolean expression. *)
    with operand[1]^.desc do
      if kind = bools then                      (* No problems. *)
        initexpr (expr_notop,type_bool)
      else if kind = unknown_type then          (* Error in boolean expression. *)
        (* no action *)
      else                                      (* The operand[1] isn't a boolean expression-- *)
        err_op (err_operands_bad,'NOT',ptn^.defn);  (*   print an error message. *)
  end (* with expr_notop^ *);
  emit (expr_notop);
end (* expr_notop *);
$PAGE expr_paren
(*  ExprParen takes a <paren expr> parse tree node.  If it contains a
    single expression, ExprParen simply returns that expression.
    Otherwise, ExprParen prints an error message and returns an
    ERROR EXPR node.

    <paren expr>
        |
    <expression 1> -- ... -- <expression n>  (n >= 1)  *)


function expr_paren: expr;

var
    def: parse_node;                            (* Follows the definition list for Ptn. *)

begin
  def := ptn^.defn;
  if def = nil then begin                       (* The expression was "()". *)
    err_node (err_no_expression,ptn);
    expr_paren := bad_expr();
  end
  else begin                                    (* There is a parenthesized expression. *)
    expr_paren := val_semantics (def);
    if def^.next <> nil then begin                      (* The paren expr is a list. *)
      err_node (err_list_found,ptn);
      initexpr (expr_paren,nil);
    end;
  end;
end (* expr_paren *);
$PAGE expr_set
(*  ExprSet takes a <set expr> parse tree node, and returns an appropriate
    expression tree to build the specified set.

    <set expr>
        |
    <element 1> -- ... -- <element n>  (n >= 0)

    <element> ::= <expression>  |  ".."
                                     |
                                   <expression> -- <expression>  *)


function expr_set: expr;

var
    def: parse_node;                            (* Scans the set element list. *)
    base_type: typ;                             (* Records the set element type. *)
    i, opcnt: 0..4095;
$PAGE element - in expr_set

    (*  SetElement evaluates one of the expressions in a set expression, and
        checks that it is a scalar value compatible with any other elements
        in the set expression.  When the first element is encountered, the
        global "base_type" is set to record the type of the elements of the
        set expression.  A descriptor ("sdesc") for a set containing the
        single element specified is built and returned. *)

    function set_element ( elem: parse_node; var sdesc: expr_type_desc ): expr;
    begin
      set_element := val_semantics (elem);
      with set_element^ do begin
        if desc.kind in [bools,ints,chars,scalars] then begin
          desc.base := desc.base^.base_type;
          if base_type = nil then
            base_type := desc.base
          else if desc.base <> base_type then begin
            err_node (err_set_compat,elem);
            initexpr (set_element,nil);
          end;
        end
        else if desc.kind <> unknown_type then begin
          err_node (err_set_not_scalar,elem);
          initexpr (set_element,nil);
        end;

        sdesc.kind := sets;                     (* compute set descriptor *)
        sdesc.base := base_type;
        if desc.kind = unknown_type
          then sdesc.kind := unknown_type       (* no valid info *)
        else if opcode = cst_ref                (* constant singleton set *)
          then begin
            sdesc.set_cst_lwb := true;
            sdesc.set_lwb := cst_val.ival;
            sdesc.set_cst_len := true;
            sdesc.set_length := 1;
          end
          else begin                            (* nonconstant singleton set *)
            sdesc.set_cst_lwb := true;  (* take 0..maximum (element) as element type of set *)
            sdesc.set_lwb := 0;
            sdesc.set_cst_len := (desc.kind <> ints);   (* most scalars are of limited size *)
            sdesc.set_length := min (base_type^.maxval + 1, set_upb_limit);
          end;
      end (* with *) ;
    end (* set_element *);
$PAGE prim_set - in expr_set

    (*  PrimSet takes a parse node for one of the entries in a set expression
        (either "expr" or "expr..expr"), and returns a GEN SET OP  for it.  *)

    function prim_set ( entry: parse_node ): expr;
    var op1_sdesc, op2_sdesc: expr_type_desc;
    begin
      if entry^.sym = elipsis then begin
        new (prim_set,gen_set_op,gen_set_op,2);
        initexpr (prim_set,nil);                (* If there are any problems, then no type. *)
        with prim_set^ do begin
          operand[1] := set_element (entry^.defn, op1_sdesc);
          if operand[1]^.desc.kind <> unknown_type then begin
            operand[2] := set_element (entry^.defn^.next, op2_sdesc);
            if operand[2]^.desc.kind <> unknown_type then begin
              (* compute range as [op1..*] and [*..op2] *)

              op1_sdesc.set_cst_len := op1_sdesc.set_cst_lwb;
              op1_sdesc.set_length := set_upb_limit - op1_sdesc.set_lwb;
              op2_sdesc.set_length := op2_sdesc.set_lwb + op2_sdesc.set_length;
              op2_sdesc.set_lwb := 0;
              desc := set_both_desc (op1_sdesc, op2_sdesc);
            end;
          end
          else
            operand[2] := val_semantics (entry^.defn^.next);
        end;
      end
      else (* entry^.sym <> ellipsis *) begin
        new (prim_set,gen_set_op,gen_set_op,1);
        initexpr (prim_set,nil);
        with prim_set^ do begin
          operand[1] := set_element (entry, op1_sdesc);
          if operand[1]^.desc.kind <> unknown_type then
            desc := op1_sdesc;
        end;
      end;
      emit (prim_set);
    end (* prim_set *);
$PAGE expr_set - main routine

begin
  def := ptn^.defn;                             (* The first element of the set. *)
  if def = nil then begin                       (* The set is empty. *)
    new (expr_set, gen_set_op, gen_set_op, 0);
    with expr_set^ do begin
      initexpr (expr_set, nil);
      desc.kind := sets;
      desc.set_lwb := 0;
      desc.set_cst_lwb := true;
      desc.set_length := 0;
      desc.set_cst_len := true;
    end;
    emit (expr_set);
    return;     (* <---- exit here with null set *)
  end;

  opcnt := 0;           (* find number of elements *)
  while def <> nil do begin
    def := def^.next;
    opcnt := opcnt + 1;
  end;

  def := ptn^.defn;
  base_type := nil;                             (* Set base type is unknown, so far. *)

  if opcnt = 1
    then expr_set := prim_set (def)

  else begin            (* opcnt > 1 *)
    new (expr_set, union_op, union_op, opcnt);
    with expr_set^ do begin
      operand[1] := prim_set (def);
      desc := operand[1]^.desc;
      def := def^.next;
      for i := 2 to opcnt do begin
        operand[i] := prim_set (def);
        desc := set_union_desc (desc, operand[i]^.desc);
        def := def^.next;
      end;
    end;
    emit (expr_set);
  end;
end (* expr_set *);
$PAGE ref_semantics - main routine

begin
  assert (ptn <> nil);
  case ptn^.sym of
    ident:
      ref_semantics := expr_ident ();
    intconst,
    realconst,
    stringconst,
    nilsy:
      ref_semantics := expr_const ();
    not_op:
      ref_semantics := expr_notop ();
    sign_op:
      ref_semantics := expr_signop ();
    mulop,
    addop,
    relop,
    powerop:
      ref_semantics := expr_binop ();
    array_qualifier:
      ref_semantics := expr_array ();
    field_qualifier:
      ref_semantics := expr_field ();
    ptr_qualifier:
      ref_semantics := expr_ptr ();
    func_qualifier:
      ref_semantics := expr_func ();
    paren_expr:
      ref_semantics := expr_paren ();
    set_expr:
      ref_semantics := expr_set ();
    others:
      begin
        err_node (err_bad_expr, ptn);
        ref_semantics := bad_expr ();
      end
  end (* case ptn^.sym *);
end (* ref_semantics *).
   @C