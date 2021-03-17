$TITLE DEBBOL - Pascal Debugger boolean expression evauation routines.
  
$HEADER debbol

module debbo$  options nocheck, special (word);

$INCLUDE debug.typ
  
$INCLUDE debref.inc
$INCLUDE deblex.inc
$PAGE eval_rel_expr
(* EVAL_REL_EXPR evaluates a relational expression.  DESC1 and DESC2 
   describe values on the left and right hand sides respectively of the
   relational operator specified by parameter RELOP.  The boolean
   result of evaluating the relational expression will be returned as
   the function value on a successful return.  *)

function eval_rel_expr (var desc1, desc2:	descriptor;
			    relop:		token_kind;
			var status:		status_code): boolean;

  type
    relationship = (less, equal, greater, incomparable);

  const
    constituent_relations: packed array [relops] of set of relationship :=
       ([equal], [less,greater], [less,equal], [less], [greater,equal], [greater], []);

    legalops: packed array [desc_kinds] of set of relops := 
      ( [eqop..gtop],				(* scalar_dt *)
	[eqop..gtop],				(* int_dt *)
	[eqop..gtop],				(* char_dt *)
	[eqop..gtop],				(* real_dt *)
	[eqop, neop, leop, geop],			(* set_dt *)
	[eqop, neop],				(* pointer_dt *)
	[],					(* array_dt *)
	[eqop, neop],				(* file_dt *)
	[eqop..gtop],				(* string_dt *)
	[],					(* record_dt *)
	[eqop, neop],				(* subr_dt *)
	[eqop..gtop],				(* substr_dt *)
	[],					(* slice_dt *)
	[] );					(* unknown_dt *)
$PAGE get_relationship - in eval_rel_expr
(* GET_RELATIONSHIP is passed descriptors for two objects.  It checks
   them for type compatability.  If the two objects are compatable
   then it compares the two objects.  The result of the comparison is
   an element of enumerated type relationship and is returned in
   parameter RELATION.  *)

procedure get_relationship (var desc1, desc2:	descriptor;
			    var relation:	relationship;
			    var status:		status_code);

  const
    address_dts: set of desc_kinds := [pointer_dt, file_dt, subr_dt];

  var
    int_value1, int_value2:	machine_word;
    ch_index, slen1, slen2:	char_range;
    ch1, ch2:			char;
    real1, real2:		real_type;
    in_set1, in_set2,
    equalset, subset, superset: boolean;
    elem_index:		  	set_range;
    desc_kind:	  		desc_kinds;

  begin

    (* Check descriptors for type compatability.  Note that we allow
       files, pointers and subroutines to be compared to address-radix
       constants.  Also, unlike the compiler, we allow comparison
       of arbitrary subroutine variables.  *)

    if not (
       ( (desc1.dkind in address_dts) and (desc2.dkind = int_dt) and (desc2.user_const) )  or
       ( (desc2.dkind in address_dts) and (desc1.dkind = int_dt) and (desc1.user_const) )  or
       ( (desc1.dkind = subr_dt) and (desc2.dkind = subr_dt) )
	   ) then
      if not (comp$types (desc1, desc2) or comp$types (desc2, desc1)) then begin
	status := type_incompatability;
	if status in severe then return		(* <-- error return *)
      end;

    desc_kind := desc1.dkind;
    if (desc_kind = int_dt) and (desc2.dkind = real_dt) then
      desc_kind := real_dt;

    case desc_kind of

      scalar_dt,
      int_dt,
      pointer_dt,
      file_dt,
      subr_dt:
	begin
	  if (desc1.dkind in address_dts) or (desc2.dkind in address_dts) then begin
	    int_value1 := ext$address (desc1, status);
	    int_value2 := ext$address (desc2, status)
	  end
	  else begin
	    int_value1 := ext$scalar (desc1, status);
	    int_value2 := ext$scalar (desc2, status)
	  end;
	  if int_value1 < int_value2 then      relation := less
	  else if int_value1 = int_value2 then relation := equal
	  else	    			       relation := greater
	end;

      char_dt,
      string_dt,
      substr_dt:
	begin
	  relation := equal;
	  slen1 := ext$lenstr (desc1);
	  slen2 := ext$lenstr (desc2);
	  for ch_index := 1 to max (slen1, slen2) do begin
	    if ch_index <= slen1 then
	      ch1 := ext$strchar (desc1, ch_index)
	    else
	      ch1 := ' ';
	    if ch_index <= slen2 then
	      ch2 := ext$strchar (desc2, ch_index)
	    else
	      ch2 := ' ';
	  exit if ch1 <> ch2 do
	    if ch1 < ch2 then
	      relation := less
	    else
	      relation := greater
	  end
	end;

      real_dt:
	begin
	  real1 := ext$real (desc1, status);
	  real2 := ext$real (desc2, status);
	  if real1 < real2 then	     relation := less
	  else if real1 > real2 then relation := greater
	  else	    		     relation := equal
	end;

      set_dt:
	begin
	  equalset := true;
	  subset := true;
	  superset := true;

	  (* Examine each element of the union of the two sets, until all elements
	     have been compared or the sets are known to be incomparable. *)

	  for elem_index := min (desc1.set_min, desc2.set_min) to
		   		max (desc1.set_max, desc2.set_max) do begin
	   exit if not (equalset or subset or superset);
	    in_set1 := ext$setelem (desc1, elem_index);
	    in_set2 := ext$setelem (desc2, elem_index);

	    equalset := equalset and (in_set1 = in_set2);
	    subset := subset and (in_set1 <= in_set2);
	    superset := superset and (in_set1 >= in_set2)
	  end;

	  if equalset then	relation := equal
	  else if subset then	relation := less
	  else if superset then	relation := greater
	  else	    		relation := incomparable
	end;

      others: (* array_dt, record_dt, slice_dt, unknown_dt *)
	status := relop_invalid

    end
  end (* get_relationship *);
$PAGE set_membership_test - in eval_rel_expr
(* SET_MEMBERSHIP_TEST takes a descriptor for a scalar type, ELEM_DESC,
   and a descriptor for a set, SET_DESC.  It returns true if the scalar
   described by ELEM_DESC is an element of the set described by SET_DESC
   (if parameter STATUS is not set to an error code).  *)

function set_membership_test (var elem_desc: descriptor;
				  set_desc: descriptor;
			      var status: status_code): boolean;

  var
    elem_inttyp: inttyp;
    elem_value:	 set_range;
    temp_status: status_code;

  begin
    set_membership_test := false;
    if set_desc.dkind <> set_dt then begin
      status := type_incompatability;
      return (* <-- return *)
    end;
    elem_inttyp := set_desc.dtype.set_element_type;
    if elem_inttyp = ord (nil) then	(* empty set *)
      return; (* <-- return *)

    temp_status := success;
    check$type (elem_desc, elem_inttyp, temp_status);
    if temp_status in [above_range, below_range] then (* language says result false in this case *)
      return (* <-- return *)
    else if temp_status = type_incompatability then
      status := ill_set_elem_type (* we can be more specific in this case *)
    else if temp_status <> success then (* not one we know what to do with? *)
      status := temp_status;
    if status in severe then return; (* <-- error return *)

    if elem_desc.dkind in [string_dt, substr_dt] then	(* coerce strings to chars *)
      elem_value := ord (ext$strchar (elem_desc, 1))
    else
      elem_value := ext$scalar (elem_desc, status);

    set_membership_test := ext$setelem (set_desc, elem_value)
  end  (* set_membership_test *);
$PAGE eval_rel_expr - body

  var
    relation: relationship;

  begin
    if relop = inop then			(* 'IN' operator *)
      eval_rel_expr := set_membership_test (desc1, desc2, status)
    else if not (relop in legalops [desc1.dkind]) or not (relop in legalops [desc2.dkind]) then
      status := relop_invalid
    else begin
      get_relationship (desc1, desc2, relation, status);
      if status in severe then return; (* <-- error return *)
      eval_rel_expr := relation in constituent_relations [relop]
    end
  end (* eval_rel_expr *);
$PAGE eval$bool$expr
(* EVAL$BOOL$EXPR parses and evaluates a boolean expression.
  
   The scanner's CURSOR should point to the beginning of the first token
   of the boolean expression on entry.  On a successful return it will point
   to the first character following the boolean expression.  Parameter
   SCOPE_STACK is used in evaluating program data references.  On a successful
   return, RESULT is set to true if the expression evaluates to true, and is
   set to false otherwise.  A boolean expression has the syntax:
  
	<bool expr> ::= <term> [ <relop> <term> ]
	<relop>     ::= IN | = | <> | <= | < | >= | >			*)

public procedure eval$bool$expr (var lex_scan_rec:	lex_scan_rec_type;
				     scope_stack:	scope_type;
				 var result:		boolean;
				 var status:		status_code);
$PAGE set_boolean, extract_boolean - in eval$bool$expr
(* SET_BOOLEAN builds a descriptor DESC to describe the boolean value
   of parameter BOOL_VAL.  *)

procedure set_boolean (var desc:	descriptor;
			   bool_val:	boolean);

  begin
    mak$scalar$desc (desc, scope_stack.displays [1].prog_blk^.boolpoint, ord (bool_val))
  end;




(* EXTRACT_BOOLEAN returns the value of the boolean stored in parameter
   DESC.  If DESC does not describe a boolean or the boolean value is not
   0 or 1 then STATUS is set to an error code.  *)

function extract_boolean (    desc:	descriptor): boolean;

  var
    ord_bool: int_type;

  begin
    extract_boolean := false; (* result sometimes used before status checked *)
    if desc.dtype.kind = bools then begin
      ord_bool := ext$scalar (desc, status);
      if not (ord_bool in [0, 1]) then
	status := illegal_value
      else
	extract_boolean := (ord_bool = ord (true))
    end
    else
      status := not_boolean
  end (* extract_boolean *);
$PAGE primitive_expr - in eval$bool$expr
(* PRIMITIVE_EXPR parses and evaluates a primitive expression within
   a boolean expression.  On a successful return, parameter DESC is set
   to the result of the primitive expression's evaluation.  A primitive
   expression has the syntax:
  
	<prim expr> ::= <constant> | <reference> | '(' <bool expr> ')'
  
   On a successful return, the scanner's CURSOR points to the first character
   beyond the end of the primitive expression.  *)

procedure primitive_expr (var desc:	descriptor);

  var
    prev_cursor: cursor_range;
    result: boolean;

  begin
    prev_cursor := lex_scan_rec.cursor;
    lex$scan (lex_scan_rec, status);
    if status in severe then return; (* <-- error return *)

    if lex_scan_rec.tkind = lparent then begin		(* Paranthesized boolean expression *)
      eval$bool$expr (lex_scan_rec, scope_stack, result, status);
      if status in severe then return; (* <-- error return *)
      set_boolean (desc, result);
      lex$scan (lex_scan_rec, status);
      if status in severe then return; (* <-- error return *)
      if lex_scan_rec.tkind <> rparent then
	status := rparen_expected
    end
    else begin					(* constant or reference *)
      lex_scan_rec.cursor := prev_cursor;
      get$descriptor (lex_scan_rec, scope_stack, desc, status)
    end
  end (* primitive_expr *);
$PAGE unary_expr - in eval$bool$expr
(* UNARY_EXPR parses and evaluates a unary expression within a
   boolean expression.  On a successful return, parameter DESC is
   set to the result of the unary expression's evaluation.  A unary
   expression has the syntax:
  
	<unary expr>: := [ 'NOT' ] <primitive expr>
  
   On a successful return, the scanner's CURSOR points to the first
   character beyond the end of the unary expression.  *)

procedure unary_expr (var desc:	descriptor);

  var
    prev_cursor: cursor_range;
    not_flag:	 boolean;

  begin
    prev_cursor := lex_scan_rec.cursor;
    lex$scan (lex_scan_rec, status);	(* check for 'NOT' *)
    if status in severe then return; (* <-- error return *)
    not_flag := lex_scan_rec.tkind = notsy;
    if not not_flag then
      lex_scan_rec.cursor := prev_cursor;

    primitive_expr (desc);		(* parse and evaluate the primitive expr *)
    if status in severe then return; (* <-- error return *)

    if not_flag then			(* complement value if 'NOT' parsed *)
      set_boolean (desc, not extract_boolean (desc))
  end (* unary_expr *);
$PAGE factor - in eval$bool$expr
(* FACTOR parses and evaluates a factor of a boolean expression.
   On a successful return parameter DESC is set to the result of the
   factor's evaluation.  A factor has the syntax:
  
	<factor>: := <unary expr> [ 'and' <unary expr> ]*
  
   On a successful return, the scanner's CURSOR will point to the first
   char beyond the end of the factor.  *)

procedure factor (var desc:	descriptor);

  var
    prev_cursor: cursor_range;
    bool_value:  boolean;

  begin
    unary_expr (desc);			(* parse and evaluate first unary expression *)
    if status in severe then return; (* <-- error return *)

    (* Parse and evaluate any additional unary expressions.  *)

    loop
      prev_cursor := lex_scan_rec.cursor;
      lex$scan (lex_scan_rec, status);
      if status in severe then return; (* <-- error return *)
    exit if lex_scan_rec.tkind <> andop;
      bool_value := extract_boolean (desc);
      if status in severe then return; (* <-- error return *)
      unary_expr (desc);
      if status in severe then return; (* <-- error return *)
      bool_value := bool_value and extract_boolean (desc);
      if status in severe then return; (* <-- error return *)
      set_boolean (desc, bool_value)
    end;

    lex_scan_rec.cursor := prev_cursor
  end (* factor *);
$PAGE term - in eval$bool$expr
(* TERM parses and evaluates a term in a boolean expression.
   On a successful return parameter DESC will describe the result
   of the term's evaluation.  A term has the syntax:
  
	<term>: := <factor> [ 'OR' <factor> ]*
   
   On a successful return, the scanner's CURSOR will point to the
   first char beyond the end of the factor.  *)

procedure term (var desc:	descriptor);

  var
    prev_cursor:	cursor_range;
    bool_value:	boolean;

  begin
    factor (desc);			(* Parse and evaluate initial factor *)
    if status in severe then return; (* <-- error return *)

    (* Parse and evaluate any additional factors. *)

    loop
      prev_cursor := lex_scan_rec.cursor;
      lex$scan (lex_scan_rec, status);
      if status in severe then return; (* <-- error return *)
    exit if lex_scan_rec.tkind <> orop;
      bool_value := extract_boolean (desc);
      if status in severe then return; (* <-- error return *)
      factor (desc);
      if status in severe then return; (* <-- error return *)
      bool_value := bool_value or extract_boolean (desc);
      if status in severe then return; (* <-- error return *)
      set_boolean (desc, bool_value)
    end;

    lex_scan_rec.cursor := prev_cursor
  end (* term *);
$PAGE eval$bool$expr - body
var
  desc1, (* first <term> *)
  desc2:	descriptor; (* second <term> *)
  prev_cursor:	cursor_range;
  relop:	token_kind;

begin
  
  term (desc1);			(* parse and evaluate first term *)
  if status in severe then return; (* <-- error return *)

  prev_cursor := lex_scan_rec.cursor; (* in case we don't find relop *)
  lex$scan (lex_scan_rec, status);
  if status in severe then return; (* <-- error return *)

  with lex_scan_rec do
    if (tkind >= minimum (relops)) and
       (tkind <= maximum (relops)) then begin	(* <term> relop <term> *)
      relop := tkind; (* remember it while working on 2nd term *)
      term (desc2);			(* parse and evaluate second term *)
      if status in severe then return; (* <-- error return *)
      result := eval_rel_expr (desc1, desc2, relop, status)
    end
    else begin					(* <term> *)
      cursor := prev_cursor; (* wasn't a relation op, so backup *)
      result := extract_boolean (desc1)
    end
  
end (* eval$bool$expr *).
    