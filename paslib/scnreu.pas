$TITLE scnreu - SCANNR Regular Expression Utilities
$LENGTH 43

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N R E U                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  12 June 1978
     
     PURPOSE:   This  module  manipulates  regular   expressions.   A
        regular expression is represented as a linked tree structure,
        where nodes may represent literal expressions  or  functional
        combinations of expressions.
     
     ENTRY POINTS:
     
        lambda      is  a public variable containing a pointer to the
                    regular expression representing the null string.
     
        phi         is a public variable containing a pointer to  the
                    regular expression representing the empty set.
     
        anychar     is  a public variable containing a pointer to the
                    regular  expression   representing   any   single
                    character.
     
        anystring   is  a public variable containing a pointer to the
                    regular expression representing any string.
     
        initre      is called  to  initialize  the  'lambda',  'phi',
                    'anychar',  and  'anystring'  regular  expression
                    constants.  It must be called before the rest  of
                    this module is used.
     
        clearre     frees   the   'lambda',   'phi',  'anychar',  and
                    'anystring' regular expression constants.
     
        literal     is called with a  data  element,  and  returns  a
                    literal  regular expression with the data element
                    as its literal value.
     
        lit_range   is called with  a  pair  of  data  elements,  and
                    returns  a literal range regular expression whose
                    endpoints are the literal values  represented  by
                    the data elements.
     
        iterate     is  called  with  a  regular  expression "X", and
                    returns the regular expression "X *".
     
        negate      is called with an  operand,  and  returns  a  not
                    operator applied to the operand.
     
        conjoin     is called with a pair of operands, and returns an
                    and operator applied to the operands.
     
        alternate   is called with a pair of operands, and returns an
                    or operator applied to the operands.
     
        catenate    is  called with a pair of operands, and returns a
                    concatenation operator applied to the operands.
     
        derivative  is called with a regular expression  and  a  data
                    element  (representing  a  literal  symbol),  and
                    returns  a  regular  expression  which   is   the
                    derivative  of  the input expression with respect
                    to the literal.
     
        re_compare  is called with a pair of regular expressions, and
                    returns  a  relation  code indicating whether the
                    first is less than, similar to  or  greater  than
                    the second.  Similarity is a weaker relation than
                    equality  of  regular  expressions.  Two  regular
                    expressions are defined to be similar if they are
                    both phi or lambda, if they are the same  literal
                    or  literal  range,  or  if  they  have  the same
                    operator and similar operands.  The  ordering  on
                    regular  expressions  is  purely  arbitrary,  but
                    useful.
     
        re_null     is a predicate which takes a regular  expression,
                    and  returns  true if the null string (lambda) is
                    in the set of strings denoted by the expression.
     
        litval      is called with a regular expression and  an  data
                    element variable.  If the regular expression is a
                    simple  literal  expression,  Litval   sets   the
                    variable  to the literal value, and returns true.
                    Otherwise, litval returns false and the  variable
                    is unchanged.
     
        lithead     is  called  with  a  regular expression, two data
                    element  variables,  and  a  regular   expression
                    variable.  If  the regular expression begins with
                    a literal or a literal range,  Lithead  sets  the
                    variables  to the minimum and maximum elements of
                    the  range  and  the  remainder  of  the  regular
                    expression, and returns true.  Otherwise, Lithead
                    returns false.
     
     NOTES:  These routines apply  various  simplification  rules  as
        they create their results.  Since all regular expressions are
        created by the routines in this module, this means  that  all
        regular  expressions  are  guaranteed  to  be  in  simplified
        (canonical) form.
     
        A single regular expression may be an operand of  any  number
        of  other regular expressions.  (That is, regular expressions
        are really represented as  directed  acyclic  graphs,  rather
        than  as  trees.)   Consequently,  a regular expression, once
        created, must never be modified.
     
        The following conditional compilation switches are defined:
     
        CHAR controls the  basic  data  type  (data_element)  of  the
             regular expression type.  It is used in SCNRE.TYP.
     
        DUMP  controls  whether  code is compiled for dumping regular
             expressions.  If DUMP is not  enabled,  then  the  PrtRe
             routine will be a no-op.
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$INCLUDE scannr.typ
$INCLUDE scnre.typ

$INCLUDE scnrea

$IF dump
$INCLUDE scnnam
$INCLUDE scnlst
$INCLUDE fio
$ENDIF


public var
    lambda   : reg_exp; (* Matches the null string. *)
    phi      : reg_exp; (* Doesn't match any string. *)
    anychar  : reg_exp; (* Matches any single-character string. *)
    anystring: reg_exp; (* Matches any string. *)


function binary ( op: reg_ops; left, right: reg_exp ): reg_exp;  forward;
$PAGE literal

(*  Literal is called with a data element, and returns a literal regular
    expression for that value.  *)

public function literal ( n: data_element ): reg_exp;

begin
  literal := new_re ( literal_op, false ); (* Literals don't contain lambda. *)
  literal^.lit_val := n; (* Set the literal value. *)
end (* literal *);
$PAGE lit_range

(*  LitRange is called with a pair of data elements, and returns a regular
    expression representing the range with the two data elements as its
    end-points.  *)

public function lit_range ( min, max: data_element ): reg_exp;

begin
  lit_range := new_re ( lit_range_op, false ); (* Literals don't contain lambda. *)
  with lit_range^ do begin (* Fill in the range bounds. *)
    min_lit := min;
    max_lit := max;
  end;
end (* lit_range *);
$PAGE iterate

(*  Iterate is called with a regular expression, and returns a new regular
    expression representing its star closure.  *)

public function iterate ( op: reg_exp ): reg_exp;

begin

  if op = lambda then
    iterate := lambda (*  lambda *  =  lambda  *)

  else if op = phi then
    iterate :=  phi (*  phi *  =  phi  *)

  else begin
    iterate := new_re ( star_op, true ); (* Star closures always contain lambda. *)
    iterate^.operand := use_re (op); (* Set the operand. *)
  end;

end (* iterate *);
$PAGE negate

(*  Negate is called with a regular expression, and returns a new regular
    expression representing its complement.  *)

public function negate ( op: reg_exp ): reg_exp;

begin
  negate := new_re ( not_op, not op^.l_in ); (* Negation contains lambda iff operand doesn't. *)
  negate^.operand := use_re (op); (* Set the operand. *)
end (* negate *);
$PAGE conjoin

(*  Conjoin is called with a pair of regular expressions, and returns a new
    regular expression representing their conjunction.  *)

public function conjoin ( left, right: reg_exp ): reg_exp;

begin

  if (left = phi) or (right = phi) then begin
    conjoin := phi; (*  phi & X  =  X & phi  = phi  *)
    test_re (left);
    test_re (right);
  end

  else if left = lambda then begin
    if right^.l_in
      then conjoin := lambda (*  lambda & (lambda | X)  =  lambda  *)
      else conjoin := phi; (*  lambda & (lambda' & X)  =  phi  *)
    test_re (right);
  end

  else if right = lambda then begin
    if left^.l_in
      then conjoin := lambda (*  (lambda | X) & lambda  =  lambda  *)
      else conjoin := phi; (*  (lambda' & X) & lambda  =  phi  *)
    test_re (left);
  end

  else
    conjoin := binary ( and_op, left, right );

end (* conjoin *);
$PAGE alternate

(*  Alternate is called with a pair of regular expressions, and returns a new
    regular expression representing their alternation.  *)

public function alternate ( left, right: reg_exp ): reg_exp;

begin

  if left = phi then
    alternate := right (*  phi | X  =  X  *)

  else if right = phi then
    alternate := left (*  X | phi  =  X  *)

  else
    alternate := binary ( or_op, left, right );

end (* alternate *);
$PAGE catenate

(*  Catenate is called with a pair of regular expressions, and returns a new
    regular expression representing their concatenation.  *)

public function catenate ( left, right: reg_exp ): reg_exp;

var
    leftv: reg_exp; (* Scans the left operand expression. *)
    l_in_right: boolean; (* True if the right operand includes lambda. *)
    cat: reg_exp; (* Used in building an expression. *)

begin

  if (left = phi) or (right = phi) then begin
    catenate := phi; (*  X phi  =  phi X  =  phi  *)
    test_re (left);
    test_re (right);
  end

  else if left = lambda then
    catenate := right (*  lambda X  =  X  *)

  else if right = lambda then
    catenate := left (*  X lambda  =  X  *)

  else begin

    (*  The canonical form for concatenated regular expressions is:

	    cat
	    / \
	   r1  cat
	       / \	or    (r1 . (r2 . ... (rn-1 . rn) ... ) )
	      r2  cat
		    .
		     .
		      cat
		      / \
		   rn-1  rn

	If the left operand of this call is itself concatenated, then we
	must create a copy of the left operand (since it may be in use
	elsewhere) with the right operand concatenated onto its tail.  I.e.,

	    CAT [ (r1 . (r2 . ... (rn-1 . rn) ... ) ) , s ] =>
		(r1 . (r2 . ... (rn-1 . (rn . s) ) ... ) )

	The algorithm is an iterative realization of the function:

	    CAT[r,s]  = (r . s)			 , if r.op <> cat
		      = (r.left . CAT[r.right,s]), if r.op  = cat	    *)

    leftv := left;
    l_in_right := right^.l_in;
    catenate := new_re ( cat_op, leftv^.l_in and l_in_right );
    cat := catenate;
    while leftv^.reg_op = cat_op do
      with cat^ do begin
	leftop := use_re (leftv^.leftop);
	leftv := leftv^.rightop;
	rightop := use_re ( new_re ( cat_op, leftv^.l_in and l_in_right ) );
	cat := rightop;
      end;
    with cat^ do begin
      leftop := use_re (leftv);
      rightop := use_re (right);
    end;
    test_re (left); (* Operand may be copied. *)
  end;

end (* catenate *);
$PAGE initre

(*  InitRE is called to initialize the 'lambda' and 'phi' constant regular
    expressions.  *)

public procedure initre;

begin
  lambda := use_re ( new_re ( lambda_op, true ) );
  phi := use_re ( new_re ( phi_op, false ) );
  anychar := use_re ( lit_range ( minimum (data_element), maximum (data_element) ) );
  anystring := use_re ( iterate ( anychar ) );
end (* initre *);
$PAGE clearre

(*  Clearre is called to free 'lambda' and 'phi' again.  *)

public procedure clearre;

begin
  free_re (lambda);
  free_re (phi);
  free_re (anychar);
  free_re (anystring);
end (* clearre *);
$PAGE derivative

(*  Derivative takes a regular expression and a data element, and returns the
    derivative of the regular expression with respect to the data element.  *)

public function derivative ( re: reg_exp; a: data_element ): reg_exp;

begin
  with re^ do
    case reg_op of

      lambda_op,
      phi_op:
	derivative := phi;

      literal_op:
	if lit_val = a
	  then derivative := lambda
	  else derivative := phi;

      lit_range_op:
	if (min_lit <= a) and (a <= max_lit)
	  then derivative := lambda
	  else derivative := phi;

      star_op:
	derivative := catenate ( derivative (operand, a), re );

      cat_op:  begin
	derivative := catenate ( derivative (leftop, a), rightop );
	if leftop^.l_in then
	  derivative := alternate ( derivative, derivative (rightop, a) );
      end;

      or_op:
	derivative := alternate ( derivative (leftop, a), derivative (rightop, a) );

      and_op:
	derivative := conjoin ( derivative (leftop, a), derivative (rightop, a) );

      not_op:
	derivative := negate ( derivative (operand, a) )

    end (* case reg_op *);
end (* derivative *);
$PAGE re_compare

(*  ReCompare takes a pair of regular expressions, and returns a 'relation'
    code indicating the relation between them.  This relation may be 'lss'
    (less than), 'eql' (similar to), or 'gtr' (greater than).  Note that
    the 'similar to' relation is not the same as equality of regular
    expressions.  The ordering is arbitrary, but makes it possible to
    define the canonical form for binary operators, which in turn makes
    it possible to efficiently test regular expressions for similarity.  *)

public function re_compare ( re1v, re2v: reg_exp ): relation;


    (*  LitComp compares two data elements.  *)

    function lit_comp ( lit1, lit2: data_element ): relation;
    begin
      if lit1 < lit2 then
	lit_comp := lss
      else if lit1 > lit2 then
	lit_comp := gtr
      else
	lit_comp := eql;
    end (* lit_comp *);


var
    re1, re2: reg_exp;

label
    100 (* Compare re1 and re2 *);

begin
  re1 := re1v;
  re2 := re2v;

100 (* Compare re1 and re2 *):

  if re1 = re2 then
    re_compare := eql (* An expression is similar to itself. *)

  else
    if re1^.reg_op < re2^.reg_op then
      re_compare := lss
    else if re1^.reg_op > re2^.reg_op then
      re_compare := gtr
    else (* Same operators:  expressions may be similar. *)
      case re1^.reg_op of

	star_op,
	not_op: begin
	  re1 := re1^.operand;
	  re2 := re2^.operand;
	  goto 100; (* Compare the operands. *)
	end;

	or_op,
	and_op,
	cat_op:  begin
	  re_compare := re_compare (re1^.leftop, re2^.leftop); (* Compare the left operands. *)
	  if re_compare = eql then begin
	    re1 := re1^.rightop;
	    re2 := re2^.rightop;
	    goto 100; (* If the same, compare the right operands. *)
	  end;
	end;

	literal_op:
	  re_compare := lit_comp (re1^.lit_val, re2^.lit_val); (* Compare literal symbols. *)

	lit_range_op:  begin
	  re_compare := lit_comp (re1^.min_lit, re2^.min_lit); (* Compare min literals. *)
	  if re_compare = eql then
	    re_compare := lit_comp (re1^.max_lit, re2^.max_lit); (* If the same, compare max literals. *)
	end

    end (* case reg_op *);
end (* re_compare *);
$PAGE binary

(*  Binary is the function which actually creates a regular expression with
    a "&" or "|" operator.  It is called with the operator to be used and a
    pair of regular expressions, and returns the regular expression which is
    the operator applied to the two operands.

    'And' and 'or' regular expressions have the canonical form:

	op
	/\
       r1 op
	  /\      or    (r1 . (r2 . ... (rn-1 . rn) ... ) )
	 r2 op
	     .
	      .
	       op
	       /\
	    rn-1 rn

    There is also the requirement that r1 < r2 .. < rn, where the ordering is
    that defined by the Compare function.  If one or both of the arguments to
    Binary contain the operator already, then a completely new expression,
    containing all the operands of each argument expression, is created.  A
    merge procedure is necessary to create the new expression in canonical
    form.   For example,

	(a . (c . e) ) . (b . (c . d) )  =>  (a . (b . (c . (d . e) ) ) )    *)


function binary (* op: reg_ops; left, right: reg_exp ): reg_exp *);

var
    leftv, rightv: reg_exp; (* For traversing the argument expressions. *)
    leftval, rightval: reg_exp; (* Single left and right operands. *)
    result: reg_exp; (* Used in constructing the result. *)
    rel: relation; (* Used in comparing operands. *)

begin
  if left = right then begin
    binary := left; (*  X op X  =  X  *)
    return; (* <---- *)
  end;

  rel := re_compare (left, right);
  if rel = eql then begin
    binary := left;
    test_re (right);
    return; (* <---- *)
  end;

  if (left^.reg_op <> op) and (right^.reg_op <> op) then begin
    binary := new_re (op, false);
    if rel = lss then begin
      binary^.leftop := use_re (left);
      binary^.rightop := use_re (right);
    end
    else begin
      binary^.leftop := use_re (right);
      binary^.rightop := use_re (left);
    end;
    if op = and_op
      then binary^.l_in := left^.l_in and right^.l_in
      else binary^.l_in := left^.l_in or right^.l_in;
    return;	(* <---- *)
  end;

(*  The first phase of Binary performs the merge procedure, leaving an
    inverted tree structure:

	op		op			 op
	/\		/\			 /\
       a  op	OP     b  op	    =>	       op  e
	  /\		  /\		       /\
	 c  e		 c  d		     op  d
					     /\
					   op  c
					   /\
	op  b
					 /\
				      nil  a				*)

  rel := eql; (* Forces a "get left op" and a "get right op". *)
  result := nil; (* The bottom of the inverted tree. *)
  leftv := left;
  rightv := right;

  loop
    if rel <> gtr then (* Get the next left operand. *)
      if leftv = nil then
	leftval := nil
      else with leftv^ do
	if reg_op <> op then begin
	  leftval := leftv;
	  leftv := nil;
	end
	else (* reg_op = op *) begin
	  leftval := leftop;
	  leftv := rightop;
	end;

    if rel <> lss then (* Get the next right operand. *)
      if rightv = nil then
	rightval := nil
      else with rightv^ do
	if reg_op <> op then begin
	  rightval := rightv;
	  rightv := nil;
	end
	else (* reg_op = op *) begin
	  rightval := leftop;
	  rightv := rightop;
	end;

  exit if (leftval = nil) and (rightval = nil);

    if leftval = nil then
      rel := gtr (* No left op -- use the right. *)
    else if rightval = nil then
      rel := lss (* No right op -- use the left. *)
    else 
      rel := re_compare ( leftval, rightval ); (* Compare the two operands. *)

    binary := new_re ( op, false ); (* Create a new result tree node. *)
    with binary^ do begin
      leftop := result; (* Hang the rest of the tree on it. *)
      if rel = lss
	then rightop := leftval (* Hang the new operand on it. *)
	else rightop := rightval;
    end;
    result := binary;
  end (* loop *);

(*  The second phase of Binary reverses the tree pointers, propagates the
    correct l_in values up the tree, sets the reference counts in the
    operand nodes, and disposes of the unused (nil . a) node:

		op		op
		/\		/\
	      op  e	       a  op
	      /\		  /\
	    op  d		 b  op
	    /\	      =>	    /\
	  op  c			   c  op
	  /\			      /\
	op  b			     d  e
	/\
     nil  a								*)

  binary := result^.rightop; (* Get the last element. *)
  while result^.leftop <> nil do begin
    result^.rightop := use_re (binary); (* The right part so far. *)
    binary := result;
    with binary^ do begin
      result := leftop; (* The rest of the tree. *)
      leftop := use_re ( result^.rightop ); (* Copy another operand. *)
      if op = and_op (* Set the 'lambda_in' flag. *)
	then l_in := leftop^.l_in and rightop^.l_in
	else l_in := leftop^.l_in or  rightop^.l_in;
    end;
  end;

  del_re (result); (* Discard the redundant pointer. *)
  test_re (left); (* The operands may have been copied. *)
  test_re (right);

end (* binary *);
$PAGE re_null

(*  ReNull tests whether lambda is in the set of strings denoted by a regular
    expression.  This is just a wrapper for the L_In field, since other
    modules don't have access to the fields of a regular expression record.  *)

public function re_null ( re: reg_exp ): boolean;

begin
  re_null := re^.l_in;
end;
$PAGE litval

(*  Only the SCNREA and SCNREU routines are permitted to look at the fields
    of a regular expression node.  Unfortunately, the parser semantic routines
    sometimes need the literal value of a literal regular expression, either
    to construct a literal range expression, or to record the minimum and max-
    imum literal values used.  Therefore, Litval will return a boolean value
    indicating whether a regular expression is a literal, and will store its
    value in a data element variable if it is.  *)

public function litval ( re: reg_exp; var result: data_element ): boolean;

begin
  with re^ do begin
    litval := (reg_op = literal_op);
    if litval then
      result := lit_val;
  end;
end (* litval *);
$PAGE lithead

(*  Lithead is similar in function and justification to Litval.  However,
    Lithead determines whether a regular expression has the form "X alpha",
    where X is a literal or literal range, and alpha is any regular expression.
    If so, then Lithead returns the minimum and maximum range values, and the
    expression alpha.  *)

public function lithead ( re: reg_exp;
			  var min_result, max_result: data_element;
			  var remainder: reg_exp ): boolean;

var r: reg_exp;

begin
  with re^ do begin
    if reg_op = cat_op then begin
      r := leftop;
      remainder := rightop;
    end
    else begin
      r := re;
      remainder := lambda;
    end;
  end (* with re^ *);

  with r^ do begin
    if reg_op = literal_op then begin
      lithead := true;
      min_result := lit_val;
      max_result := lit_val;
    end
    else if reg_op = lit_range_op then begin
      lithead := true;
      min_result := min_lit;
      max_result := max_lit;
    end
    else
      lithead := false;
  end (* with r^ *);
end (* lithead *);
$PAGE print_regular_expression

(*  PRINT REGULAR EXPRESSION is called with a regular expression pointer,
    and prints the regular expression in external form.  The Paren flag
    indicates whether parentheses are to be printed before and after the
    regular expression.  *)

$IF dump

procedure print_regular_expression ( re: reg_exp; paren: boolean );

const
    pr: array [reg_ops] of 1..6 = (* Operator Priorities *)
     (  6, 6, 6, 1, 5, 5, 4, 2, 3  );

    opname: array [reg_ops] of string [5] = (* Operator Names *)
     (  '^', '#', '', '', '*', '''', ' ', ' | ', ' & '  );


var re1: reg_exp;
    paren1: boolean;
    paren_count: number;

begin
  re1 := re;
  paren1 := paren;
  paren_count := 0;
  while re1 <> nil do begin
    if paren1 then begin
      fio_write (listfb, '(');
      paren_count := paren_count + 1;
    end;
    with re1^ do begin
      case reg_op of

	lambda_op, phi_op:
	  begin
	    fio_write (listfb, opname [reg_op]);
	    re1 := nil;
	  end;

	literal_op:
	  begin
	    prt_symbol (lit_val, '');
	    re1 := nil;
	  end;

	lit_range_op:
	  begin
	    prt_symbol (min_lit, '');
	    fio_write (listfb, '..');
	    prt_symbol (max_lit, '');
	    re1 := nil;
	  end;

	cat_op, and_op, or_op:
	  begin
	    print_regular_expression (leftop, (pr[leftop^.reg_op] <= pr[reg_op]));
	    fio_write (listfb, opname[reg_op]);
	    re1 := rightop;
	    paren1 := (pr[rightop^.reg_op] < pr[reg_op]);
	  end;

	star_op, not_op:
	  begin
	    print_regular_expression (operand, (pr[operand^.reg_op] < pr[reg_op]));
	    fio_write (listfb, opname[reg_op]);
	    re1 := nil;
	  end;

      end (* case reg_op *);
    end (* with re1^ *);
  end (* while re1 <> nil *);

  while paren_count <> 0 do begin
    fio_write (listfb, ')');
    paren_count := paren_count - 1;
  end;
end (* print_regular_expression *);

$ENDIF
$PAGE prt_re

(*  PRT RE prints a regular expression on the listing file.  It is merely a
    publicly accessible wrapper for PrintRegularExpression.  *)

public procedure prt_re ( re: reg_exp );

begin
$IF dump  print_regular_expression (re, false);
end.
  ?|"á