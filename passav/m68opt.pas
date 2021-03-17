$TITLE M68OPT - MC68000 Code Generator Assignment Optimization

module m68opt options check;
$PAGE includes
$SYSTEM pascal
$SYSTEM pasist
$SYSTEM ptmcon
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasifu
$PAGE constrain_precisions
(*  CONSTRAIN PRECISIONS determines the machine-specific precisions at which
    arithmetic operations should be performed.  The precision required for
    an operation depends both on the precisions of its operands and on its
    context.  For example, "A := B + C": if A, B, and C are all 16-bit signed
    integers, then one might expect to require 32-bit addition, since 16-bit
    addition might overflow; but if the result will not be checked, then
    overflow may be ignored, since the result will be stored in a 16-bit
    variable in any case.

    The general algorithm is a backward scan of all nodes in the I/F.  As
    each node of interest is processed, two actions may be performed:
    (1) determine the precision of that operation using any contextual
    information available, and (2) set the context for children of the
    node.  Step (2) is performed by setting the result field of the child
    to point to the parent node.

    In the case of a node which has multiple uses, the referencing node
    with the most appropriate constraint is chosen.  This arbitration is
    performed as a referencing node attempts to set itself as the context
    node.  *)

public procedure constrain_precisions;

const
    broad_contexts : set of tuple_opcodes =
      [ ile_op, ige_op, ilt_op, igt_op, ieq_op, ine_op,
	sub_range_chk, str_range_chk, val_range_chk, compat_chk ];
$PAGE rmode - in constrain_precisions
(*  RMODE computes the machine precision for real operations and operands.
    There are only two choices--single and double precision.  *)

function rmode ( e : expr ) : prec_type;

begin
  if e^.desc.precision <= srealprec
    then rmode := srealprec
    else rmode := maximum (prec_type);
end;
$PAGE context_mode - in constrain_precisions
(*  CONTEXT MODE returns the contextually derived precision determined by
    a context tuple "context".  This has presumably been obtained from the
    result field of some expression.  *)

function context_mode ( context : tuple ) : align_range;

begin
  if (context = nil) orif (context^.opcode in broad_contexts) then
    context_mode := int_prec_limit
  else
    context_mode := context^.desc.int_prec;
end;
$PAGE context_sign - in constrain_precisions
(*  CONTEXT SIGN returns the contextually derived sign determined by a context
    tuple "context".  This has presumably been obtained from the result field
    of some expression.  *)

function context_sign ( context : tuple ) : boolean;

begin
  if (context = nil) orif (context^.opcode in broad_contexts) then
    context_sign := true
  else
    context_sign := context^.desc.signed;
end;
$PAGE set_context - in constrain_precisions
(*  SET CONTEXT specifies that "context" is a context node for expression "e".
    If another context has already been specified for "e", then the broadest
    context is selected.  *)

procedure set_context ( e : expr; context : tuple );

begin
  if (e^.result = nil) orif (context_mode (context) > context_mode (e^.result)) then
    e^.result := context;
end;
$PAGE constrain_int_prec - in constrain_precisions
(*  CONSTRAIN INT PREC is called with an expression tuple and a context tuple.
    It sets the integer precision and the signed flag in the expression's
    type descriptor to the minimum of the intrinsic values for the expression
    and the constrained values for the context.  *)

procedure constrain_int_prec ( e : expr; context : tuple );

begin
  with e^.desc do begin
    if signed and not context_sign (context) then begin
      signed := false;
      int_prec := int_prec - 1;
    end;
    int_prec := min (int_prec, context_mode (context));
  end;
end;
$PAGE constrain_precisions - main routine
var t : tuple;
    i : integer;

begin
  t := t_chain^.final_tuple;
  while t <> nil do begin
    with t^ do begin
      case opcode of

	(*  Record the current statement for debugging purposes.  *)

	start_stmt :
	  cur_source :=  stmt_source;

	(*  The precision of the rhs of an assignment may safely be constrained
	    to the precision of the lhs.  (This is not necessarily true if
	    checking is enabled; however, in this case, the check tuple context
	    will override the assignment context.)  *)

	assign_op :
	  set_context (rhs, lhs);

	(*  It is never useful to apply a contextual constraint to a simple
	    data reference.  *)

	first_data_ref..last_data_ref,
	func_call_op :
	  case desc.kind of
	    ints, bools, chars, scalars :
	      constrain_int_prec (t, nil);
	    reals :
	      desc.precision := rmode (t);
	    others :
	      (* ok *);
	  end;

	(*  The precision of an integer arithmetic expression is constrained by
	    its context and its intrinsic precision.  *)

	iadd_op, isub_op, imul_op, idiv_op, imod_op, ineg_op, iabs_op,
	expii_op, imin_op, imax_op :
	  constrain_int_prec (t, result);

	(*  Only intrinsic constraints apply to real expressions, since a float
	    operation is always generated when a real size conversion is needed.  *)

	radd_op, rsub_op, rmul_op, rdiv_op, rneg_op, rabs_op, rmin_op, rmax_op,
	sqrt_op, float_op, ln_op, log_op, exp_op, sin_op, arcsin_op, sinh_op,
	cos_op, arccos_op, cosh_op, tan_op, tanh_op, cotan_op, random_op,
	arctan_op, expri_op, exprr_op :
	  desc.precision := rmode (t);

	(*  Comparison and range check operations do not constrain their operands.
	    Since the rhs of an assignment is normally constrained to have the
	    same precision as the lhs, the fact that the rhs is being checked
	    must suppress that constraint.  (In the absence of checking, we can
	    assume that the rhs value is legal, since it won't be detected if
	    it isn't anyways.  However, if checking is on, we must compute the
	    rhs to its full precision, since we must detect overflow if it
	    occurs.  *)

	ile_op, ilt_op, igt_op, ige_op, ieq_op, ine_op :
	  begin
	    constrain_int_prec (t, nil);
	    set_context (operand [1], t);
	    set_context (operand [2], t);
	  end;

	sub_range_chk, str_range_chk, val_range_chk, compat_chk :
	  for i := 1 to upperbound (operand) do begin
	    if operand [i] <> nil then
	      set_context (operand [i], t);
	  end;

	(*  A scalar convert operator may be constrained like any arithmetic
	    expression; furthermore, it constrains its operand.  *)

	sclcvt_op :
	  begin
	    constrain_int_prec (t, result);
	    set_context (operand [1], t);
	  end;

	(*  For boolean operators, all constraints are pretty much irrelevant,
	    except for the basic machine size constraints.  *)

	or_op, and_op, orif_op, andif_op, gen_orif_op, gen_andif_op, bnot_op,
	rle_op, rlt_op, rgt_op, rge_op, req_op, rne_op,
	sle_op, slt_op, sgt_op, sge_op, seq_op, sne_op,
	setle_op, setge_op, seteq_op, setne_op,
	ptreq_op, ptrne_op, fileq_op, filne_op,
	in_op, odd_op,
	eoln_op, eof_op, eopage_op,
	masked_op, pending_op :
	  constrain_int_prec (t, nil);

	 (*  Pointer and file expressions are already set properly.  *)

	addr_op, new_op, in_str_op, out_str_op,
	open_op, reset_op, rewrite_op, update_op :
	  (* ok *);

	(*  We have several integer operators with intrinsic precisions.  *)

	runtime_op, lwb_op, upb_op, dim_op, trunc_op,
	extent_op, cursor_op, filesize_op,
	iostatus_op, extstatus_op,
	time_op,
	mathstatus_op, prgmstatus_op, spclstatus_op, exiostatus_op :
	  constrain_int_prec (t, nil);

	(*  "Round" is a special case--it returns either an integer or a real.  *)

	round_op :
	  if desc.kind = reals then
	    desc.precision := rmode (t)
	  else
	    constrain_int_prec (t, nil);

	(*  String search functions either return a simple 16-bit value,
	    or are determined by their third operand and their context.  *)

	search_op, index_op, verify_op :
	  if upperbound (operand) = 2 then
	    desc.int_prec := 16
	  else begin
	    desc.int_prec := max (16, operand [3]^.desc.int_prec);
	    constrain_int_prec (t, result);
	    set_context (operand [3], t);
	  end;

	(*  The string length function always returns a 16-bit value.  *)

	length_op :
	  desc.int_prec := 16;

	(*  Any character occupies a single byte.  *)

	lwc_op, upc_op :
	  constrain_int_prec (t, nil);

	others :
	  if (opcode >= first_expr) and (opcode <= last_expr) then
	    assert (not (desc.kind in [ints, bools, chars, scalars, reals]));

      end (* case opcode *);

      t := prev;
    end (* with t^ *);
  end (* while t <> nil *);

  clr_rslt;				(* Set the result fields back to nil. *)
end (* constrain_int_precisions *);
$PAGE const_match
(*  CONST MATCH takes two VAL nodes, returning true if they represent the same
    constant value.  Record, array and set constants are only checked to see
    if they have the same value pointers.  *)


function const_match ( c1, c2: val ): boolean;

begin
  if c1.kind <> c2.kind then
    const_match := false
  else begin
    case c1.kind of

      scalar_cst:
        const_match := (c1.ival = c2.ival);

      real_cst:
        const_match :=
          (c1.valp^.real_val = c2.valp^.real_val) andif
          (c1.valp^.real_prec = c2.valp^.real_prec);

      string_cst:
        const_match :=
          (length (c1.valp^.str_val) = length (c2.valp^.str_val)) andif
          (c1.valp^.str_varying_ref = c2.valp^.str_varying_ref) andif
	  (c1.valp^.str_val = c2.valp^.str_val);

      set_cst,
      array_cst,
      record_cst:
        const_match := (c1.valp = c2.valp);

      ptr_cst:
        const_match := true; (* The only pointer constant is NIL. *)

      subr_cst:
        const_match := (c1.blkp = c2.blkp);

      no_value:
        const_match := true;

    end (* case *);
  end;
end (* const_match *);
$PAGE tpl_match
(*  TPL MATCH compares two reference expressions to determine if they have the
    same opcode and operands.  *)


function tpl_match ( t, t1: expr ): boolean;

begin
  with t^ do
    if t1^.opcode = opcode then begin
      case opcode of

        cst_ref:
          tpl_match := const_match (cst_val, t1^.cst_val);

        ident_ref:
          tpl_match := (t1^.id_sym = id_sym);

        field_ref:
          tpl_match :=
	    tpl_match (t1^.base_rec, base_rec) andif
            (t1^.field_sym = field_sym);

        ptr_ref:
          tpl_match := tpl_match (t1^.base_ptr, base_ptr);

        buffer_ref:
          tpl_match := tpl_match (t1^.base_file, base_file);

        array_ref:
          tpl_match :=
            tpl_match (t1^.base_array, base_array) andif
            tpl_match (t1^.index_val, index_val);

        substr_ref:
          tpl_match :=
            tpl_match (t1^.base_string, base_string) andif
            tpl_match (t1^.substr_index, substr_index) andif
            tpl_match (t1^.substr_length, substr_length);

        others:
          tpl_match := false;

      end (* case kind *);
    end
    else (* t1^.opcode <> opcode *) begin
      tpl_match := false;
    end;
end (* tpl_match *);
$PAGE circ_fields
(*  CIRC FIELDS is called with a pair of field symbols, Fa and Fb, for fields
    in the same record type.  The question is where (1) Fa and Fb are the same
    field, or (2) Fa is the tag field of a discriminated union which Fb is in
    one of the variants of, or (3) Fa and Fb are in different variants of some
    undiscriminated union.  *)


function circ_fields ( fa, fb: sym ): boolean;

var
    a_tag: typ; (* The variant tag below the rec/var or Fa. *)
    a_var: typ; (* Scans up the Fa variant chain. *)
    b_var: typ; (* Scans up the Fb variant chain. *)

begin

  (*  See if Fa and Fb are the same fields.  *)

  if fa = fb then begin
    circ_fields := true;
    return;
  end;

  (*  See if Fb is a field in a variant of which Fa is the tag field.  *)

  a_tag := fa^.fld_variant^.variant_tag;
  if (a_tag <> nil) andif (a_tag^.tag_field = fa) then begin

    (*  Fa is a tag field.  The question now is whether Fb is in a variant
        controlled by Fa.  The question is actually recursive:  is Fb in a
        variant ... in a variant controlled by Fa?  *)

    b_var := fb^.fld_variant;
    while (b_var^.kind = variants) andif
      (b_var^.tag <> a_tag) do
        b_var := b_var^.tag^.tag_recvar;
    if b_var^.kind = variants then begin
      circ_fields := true;
      return;
    end;
  end;

  (*  See if Fa and Fb are fields in distinct variants controlled by the same
      undiscriminated union.  *)

  a_var := fa^.fld_variant;
  b_var := fb^.fld_variant;
  if (a_var^.kind = variants) and (b_var^.kind = variants) then begin

    (*  Fa and Fb are both fields in variants.  Could they be fields
        of different variants of an undiscriminated union?  *)

    while a_var^.kind = variants do begin
      if a_var^.tag^.tag_field = nil then begin

        (*  We have found an undiscriminated union with a variant containing
            Fa.  Now scan to see if Fb is in a variant of the same undiscri-
            minated union.  *)

        b_var := fb^.fld_variant;
        while (b_var^.kind = variants) andif
          (b_var^.tag <> a_var^.tag) do
            b_var := b_var^.tag^.tag_recvar;

        if b_var^.kind = variants then begin

(*=====>  Common undiscriminated union found for Fa and Fb!!!
          See if Fa and Fb are in different variants.
          Return in any case.  *)

          circ_fields := (a_var <> b_var);
          return;
        end;

      end (* undiscriminated union containing Fa *);
      a_var := a_var^.tag^.tag_recvar;
    end (* while a_var^.kind = variants *);
  end (* if Fa and Fb are both in variants *);

  (*  None of the above.  *)

  circ_fields := false;

end (* circ_fields *);
$PAGE distinct_indices
(*  DISTINCT INDICES returns true if (1) Ia and Ib are distinct constants, or
    (2) Ia = Ib + C (C <> 0), or (3) Ib = Ia + C (C <> 0), or (4) Ia = X + C1
    and Ib = X + C2 (C1 <> C2), where X is any expression.  *)


function distinct_indices ( ia, ib: expr ): boolean;


var
    a_base, b_base: expr; (* The non-constant term from Ia/Ib. *)
    a_cst, b_cst: integer; (* The constant term from Ia/Ib. *)

  procedure sum_test ( i: expr; var base: expr; var cst: integer );
  begin
    with i^ do begin
      if (opcode = iadd_op) andif (operand[1]^.opcode = cst_ref) then begin
        base := operand [2];
        cst := operand[1]^.cst_val.ival;
      end
      else if (opcode = iadd_op) andif (operand[2]^.opcode = cst_ref) then begin
        base := operand [1];
        cst := operand[2]^.cst_val.ival;
      end
      else if (opcode = isub_op) andif (operand[2]^.opcode = cst_ref) then begin
        base := operand [1];
        cst := - operand[2]^.cst_val.ival;
      end
      else if (opcode = cst_ref) andif (cst_val.kind = scalar_cst) then begin
	base := nil;
	cst := cst_val.ival;
      end
      else begin
	base := i;
	cst := 0;
      end;
    end (* with i^ *);
  end (* sum_test *);

begin
  sum_test (ia, a_base, a_cst);
  sum_test (ib, b_base, b_cst);

  if a_base = nil then
    distinct_indices := (b_base = nil) andif (a_cst <> b_cst)
  else
    distinct_indices := (b_base <> nil) andif tpl_match (a_base, b_base) andif
			(a_cst <> b_cst);
end (* distinct_indices *);
$PAGE circumscribes
(*  CIRCUMSCRIBES tests whether two tuples satisfy the "circumscribes" relation
    as defined in CIN-#5.  *)


function circumscribes ( a, b: tuple ): boolean;

begin
  with a^ do begin
    if opcode <> b^.opcode then
      circumscribes := false
    else begin
      case opcode of

        ident_ref:
          circumscribes :=
            (id_sym = b^.id_sym);

        field_ref:
          circumscribes :=
            circumscribes (base_rec, b^.base_rec) andif
            circ_fields (field_sym, b^.field_sym);

        ptr_ref:
          circumscribes :=
            (base_ptr^.desc.base^.heap_class = b^.base_ptr^.desc.base^.heap_class);

        buffer_ref:
          circumscribes :=
            (base_file^.desc.base^.file_class = b^.base_file^.desc.base^.file_class);

        array_ref:
          circumscribes :=
            circumscribes (base_array, b^.base_array) andif
            not distinct_indices (index_val, b^.index_val);

        substr_ref:
          circumscribes :=
            circumscribes (base_string, b^.base_string) andif not
              ( (desc.kind = chars) andif (b^.desc.kind = chars) andif
                distinct_indices (substr_index, b^.substr_index) )

      end (* case opcode *);
    end (* a^.opcode = b^.opcode *);
  end (* with a^ *);
end (* circumscribes *);
$PAGE chk_overlap
(*  CHK OVERLAP is a function which takes the left and right hand side
    expressions from an assignment, and determines whether a temporary
    location is necessary to perform the assignment safely.  An overlap
    is only indicated for string, set and aggregate assignments.  *)

public function chk_overlap (left, right: expr ): boolean;
$PAGE may_be_undiscriminated - in chk_overlap
(*  MAY BE UNDISCRIMINATED takes two non-simple references, and tests whether
    there is a possibility that the two references could be to fields (or
    components) in distinct variants of an undiscriminated union in the same
    record.  *)

function may_be_undiscriminated ( ref1, ref2: expr ): boolean;

const ref_stk_size = 20;

type ref_stack = record
        top: 0 .. ref_stk_size;
        stack: array [1..ref_stk_size] of expr
     end;
$PAGE stack_ref - in may_be_undiscriminated - in chk_overlap
(*  STACK REF creates a stack containing the nested references making up a
    fully qualified reference, down to the first occurrence of a "primitive"
    reference--a constant, ientifier, pointer or buffer reference, or a temporary.  *)

procedure stack_ref ( ref: expr; var stack: ref_stack; var too_complex: boolean );

var loop_statustacking, stack_full, stacking_aborted, finished );
    r: expr;

begin
  r := ref;
  stack.top := 0;
  loop_status := stacking;
  while loop_status = stacking do begin
    if stack.top = ref_stk_size then
      loop_status := stack_full
    else begin
      stack.top := stack.top + 1;
      stack.stack[stack.top] := r;
      with r^ do begin
        case opcode of
          cst_ref, ident_ref, ptr_ref, buffer_ref, alc_temp_op:
            loop_status := finished;
          field_ref:
            r := base_rec;
          array_ref:
            r := base_array;
	  func_call_op:		(* All bets are off without a MUCH more detailed analysis *)
	    loop_status := stacking_aborted
        end;
      end;
    end;
  end (* while loop_status = stacking *);

  too_complex := (loop_status in [stack_full, stacking_aborted]);
end (* stack_ref *);
$PAGE may_be_undiscriminated - main routine - in chk_overlap
var stk1, stk2: ref_stack;
    r1, r2: expr;
    loop_status: ( comparing, undiscriminated, ok );
    ofl1, ofl2: boolean;
    base_match: boolean;

begin
  stack_ref (ref1, stk1, ofl1);
  stack_ref (ref2, stk2, ofl2);
  if ofl1 or ofl2 then begin (* Expression too complex, assume the worst. *)
    may_be_undiscriminated := true;
    return;
  end;
  
  r1 := stk1.stack[stk1.top];
  stk1.top := stk1.top - 1;
  r2 := stk2.stack[stk2.top];
  stk2.top := stk2.top - 1;

  if r1^.opcode <> r2^.opcode then
    base_match := false
  else begin
    case r1^.opcode of
      ident_ref:
        base_match := (r1^.id_sym = r2^.id_sym);
      ptr_ref:
        base_match := (r1^.base_ptr^.desc.base^.heap_class = r2^.base_ptr^.desc.base^.heap_class);
      buffer_ref:
        base_match := (r1^.base_file^.desc.base^.file_class = r2^.base_file^.desc.base^.file_class);
      alc_temp_op:
	base_match := (r1 = r2);
    end;
  end;

  if base_match
    then loop_status := comparing (* Something in common. *)
    else loop_status := ok; (* Nothing in common. *)

  while loop_status = comparing do begin
    if (stk1.top = 0) or (stk2.top = 0) then
      loop_status := ok (* Match failed. *)
    else begin
      r1 := stk1.stack[stk1.top];
      stk1.top := stk1.top - 1;
      r2 := stk2.stack[stk2.top];
      stk2.top := stk2.top - 1;

      if r1^.opcode <> r2^.opcode then
        loop_status := ok (* Match failed. *)
      else if r1^.opcode = field_ref then begin
        if r1^.field_sym = r2^.field_sym then
          (* matches so far, keep comparing *)
        else if circ_fields (r1^.field_sym, r2^.field_sym) then
          loop_status := undiscriminated
        else
          loop_status := ok; (* Distinct or discriminated fields. *)
      end
      else begin
	assert (r1^.opcode = array_ref);
	if distinct_indices (r1^.index_val, r2^.index_val) then
	  loop_status := ok;
      end;
    end (* if neither stack is empty *);
  end (* while loop_status = comparing *);

  may_be_undiscriminated := (loop_status = undiscriminated);
end (* may_be_undiscriminated *);
$PAGE rec_circumscribes
(*  REC CIRCUMSCRIBES tests whether an assignment to L can change the value of R.  *)

function rec_circumscribes ( l, r : expr ) : boolean;

var i : integer;

begin
  if circumscribes (l, r) then
    rec_circumscribes := true
  else begin
    rec_circumscribes := false;
    with r^ do begin
      case opcode of
	first_nnary_op..last_nnary_op :
	  for i := 1 to upperbound (operand) do begin
	    rec_circumscribes := rec_circumscribes (l, operand [i]);
	  exit if rec_circumscribes;
	  end;
	first_sbinary_op..last_snary_op :
	  (* false *);
	func_call_op :
	  rec_circumscribes := true;
	cst_ref,
	ident_ref,
	ptr_ref,
	buffer_ref :
	  (* false *);
	array_ref :
	  rec_circumscribes := rec_circumscribes (l, base_array) orif
			       rec_circumscribes (l, index_val);
	field_ref :
	  rec_circumscribes := rec_circumscribes (l, base_rec);
	substr_ref :
	  rec_circumscribes := rec_circumscribes (l, base_string) orif
			       rec_circumscribes (l, substr_index) orif
			       rec_circumscribes (l, substr_length);
      end (* case opcode *);
    end (* with r^ *);
  end (* if not circumscribes (l, r) *);
end (* rec_circumscribes *);
$PAGE str_overlaps - in chk_overlap
(*  STR OVERLAPS is called with the left and right hand side expressions of a
    string assignment, and determines whether the assignment must be performed
    using a temporary.  A temporary is necessary if some part of the lhs
    string appears as part of the rhs string, except at the very beginning,
    or if the rhs string contains a function call which can use or modify the
    lhs string value.  *)

function str_overlaps ( l, r: expr ): boolean;


    function str_ovlay ( l, r: expr ): boolean;
      forward;
$PAGE str_ovl - in str_overlaps - in chk_overlap
(*  STR OVL is the base function for the computation of StrOverlaps.  It also
    provides the base for the helper function StrOvlay.  StrOvl checks whether
    two string expressions "overlap" or are "overlaid" in some respect.  It is
    parameterized with a function OvlPrim which determines its value when none
    of its other rules apply.

    StrOvl (l, r, OvlPrim) ==
        r = substr (r', ...)      => StrOvl (l, r', OvlPrim)
        r = uppercase (r')        => StrOvl (l, r', OvlPrim)
        r = lowercase (r')        => StrOvl (l, r', OvlPrim)
        r = string_convert (r')   => StrOvl (l, r', OvlPrim)
        l = substr (l', 1, ...)   => StrOvl (l', r, OvlPrim)
        r = r1 || ... || rn       => StrOvl (l, r1, OvlPrim) or
                                     StrOvlay (l, r2) ... or StrOvlay (l, rn)
        l = substr (l', ...)      => StrOvlay (l', r)
        else                      => OvlPrim (l, r)                             *)

type ovl_fn = function ( expr; expr ): boolean;


function str_ovl ( l, r: expr; ovl_prim: ovl_fn ): boolean;

var l1, r1: expr;
    i: oper_range;

label 100;

begin
  l1 := l;
  r1 := r;

100:

  with r1^ do begin
    if opcode in [func_call_op, sclcvt_op] then begin
      str_ovl := true;
      return;
    end;

    if opcode = substr_ref then begin
      r1 := base_string;
      goto 100;
    end;

    if opcode in [lwc_op, upc_op, strcvt_op] then begin
      r1 := operand[1];
      goto 100;
    end;
  end;

  with l1^ do begin
    if opcode = substr_ref then begin
      if (substr_index^.opcode = cst_ref) andif
         (substr_index^.desc.kind = ints) andif
         (substr_index^.cst_val.ival = 1) then begin
        l1 := base_string;
        goto 100;
      end;
    end;
  end;

  with r1^ do begin
    if opcode = cat_op then begin
      for i := 2 to upperbound (operand) do begin
        str_ovl := str_ovlay (l1, operand[i]);
        if str_ovl then
          return;
      end;
      r1 := operand[1];
      goto 100;
    end;
  end;

  if l1^.opcode = substr_ref
    then str_ovl := str_ovlay (l1^.base_string, r1)
    else str_ovl := ovl_prim (l1, r1);
end (* str_ovl *);
$PAGE str_ovlay - in str_overlaps - in chk_overlap
(*  STR OVLAY is a helper function for StrOverlaps.  It is essentially the same
    as StrOverlaps, except that two strings are said to be "overlaid" if they
    can have anything in common, while they do not "overlap" unless the common
    portion comes at the head of the lhs string.  *)

function str_ovlay (* l, r: expr ): boolean *);

begin
  str_ovlay := str_ovl (l, r, rec_circumscribes);
end;
$PAGE str_overlaps - main routine - in chk_overlap
begin
  str_overlaps := str_ovl (l, r, may_be_undiscriminated);
end;
$PAGE set_agg_overlaps - in chk_overlap
(*  SET AGG OVERLAPS is called with the left and right hand side expressions of
    a set or aggregate assignment, and determines whether the assignment must
    be performed using a temporary.  A temporary is necessary if the lhs
    expression, or some component of it, occurs in the rhs expression, or if
    the rhs expression contains a function call which can use or modify the
    lhs expression value.

    SetAggOverlaps (l, r) ==
        r = func_call (...)  => true
        r = gen_set (...)    => false
        r = set_cvt (r')     => SetOverlaps (l, r')
        r = r1 + ... + rn    => Circumscribes (r, l) ... or
                                Circumscribes (rn, l)
        r = r1 - r2          => Circumscribes (r, l) or Circumscribes (r2, l)
        r = r1 * r2          => Circumscribes (r, l) or Circumscribes (r2, l)
        r = (r1, ..., rn)    => true
        else                 => MayBeUndiscriminated (l, r)                     *)

function set_agg_overlaps ( l, r: expr ): boolean;

var i: oper_range;

begin
  if r^.opcode in [first_data_ref..last_data_ref] then
    set_agg_overlaps := may_be_undiscriminated (l, r)
  else
    set_agg_overlaps := rec_circumscribes (l, r);
end (* set_agg_overlaps *);
$PAGE chk_overlap - main routine
begin
  case left^.desc.kind of

    strings:
      chk_overlap := str_overlaps (left, right);

    sets, arrays, records:
      chk_overlap := set_agg_overlaps (left, right);

    others:
      chk_overlap := false
  end;
end (* chk_overlap *);
$PAGE chk_recursion
(*  CHK RECURSION is called with an assignment tuple.  It sets the Lrecursive and
    Rrecursive flags in the tuple if the first or second operands of the
    right-hand side expression match the left-hand side expression.  *)

public procedure chk_recursion ( a: tuple );

var r: expr;

begin
  with a^ do begin
    r := rhs;
    while r^.opcode in [setcvt_op, strcvt_op, sclcvt_op] do
      r := r^.operand[1];

    case r^.opcode of

      ineg_op,
      rneg_op,
      bnot_op,
      iabs_op,
      rabs_op,
      isub_op,
      diff_op,
      cat_op:
        if tpl_match (lhs, r^.operand[1]) then
          lrecursive := true;

      iadd_op,
      and_op,
      or_op,
      union_op,
      both_op:
        if tpl_match (lhs, r^.operand[1]) then
          lrecursive := true
        else if tpl_match (lhs, r^.operand[2]) then
          rrecursive := true;

      substr_ref:
        if tpl_match (lhs, r^.base_string) andif
           (r^.substr_index^.opcode = cst_ref) andif
           (r^.substr_index^.cst_val.ival = 1) then
          lrecursive := true;

      others:
        (* not recursive *);

    end (* case r^.opcode *);
  end (* with a^ *);
end (* chk_recursion *).
  DhQ;