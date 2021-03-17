$TITLE PASANL.PAS, last modified 5/11/84, zw
PROGRAM pasanl OPTIONS special(word),  storage(6000);
(*TYM-Pascal Compiler Syntax/Semantic Analysis*)
$HEADER PASANL.HDR
$INCLUDE pascal.inc
$INCLUDE pasfil.inc
$INCLUDE pasist.inc
$INCLUDE ptmcon.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE paslex.inc
$INCLUDE paserr.inc
$INCLUDE pascfm.inc
$INCLUDE passw.inc
$INCLUDE pasopd.inc
$INCLUDE corout.inc
$INCLUDE pasopn.inc
$INCLUDE pasrdr.inc
$INCLUDE pasenv.inc
$INCLUDE pascgr.inc
$INCLUDE pa1xrf.inc
$INCLUDE pasglb.inc
$INCLUDE pasblk.inc
$INCLUDE pastal.inc
$INCLUDE pasalc.inc
$INCLUDE pasifu.inc
$INCLUDE pascv.inc
$INCLUDE pa1dmp.inc
$INCLUDE pasdmp.inc
$INCLUDE prgdir.inc
$INCLUDE TIMUTL.inc
$INCLUDE paslog.inc
$INCLUDE infpac.inc
$INCLUDE tmpnam.inc
$SYSTEM RUNUTL.INC
EXTERNAL
FUNCTION fileblock ( VAR TEXT ): filblock;
EXTERNAL
PROCEDURE dmp_ptree ( parse_node ); (* in PA1DMP *)
$INCLUDE pasbnf.sem
$INCLUDE pasbnf.tab
STATIC VAR (* parser state information *)
ic, (* address of current pminst *)
nextic: pmaddr; (* address of next pminst, changed by recovery routines*)
savedtok, (* flags next symbol in savedsym due to insert token *)
needtok: BOOLEAN; (* flags scan required on next terminal recognition*)
savedsym, (*contains next symbol saved by inserttoken *)
tokensym: symbols; (* token read by scanner *)
defnbase, (* dummy node to which we chain everything *)
lastnode, (* last node chained to list *)
resume_node: parse_node; (* new "last node" after return *)
(* Parse stack *)
CONST
maxpmstack = 200; (* increased from 100  7/24/82 DWS *)
VAR
pmstackptr: 0..maxpmstack;
pmstack: ARRAY [1..maxpmstack] OF RECORD
  retic: pmaddr; (* ic of calling instruction *)
  src: source_id; (* position in text of construct *)
  col: line_index;
  src_not_set: BOOLEAN; (* flags whether above is valid *)
  marker: parse_node (* last node pushed prior to recognition call *)
END;
PUBLIC VAR
ptree: parse_node;
(*    procedure to initialize syntax analyzer    *)
PUBLIC
PROCEDURE initparse;
BEGIN
  needtok:= TRUE;
  nextic := minpmaddr;
  NEW (defnbase); (* define dummy node for base of tree *)
  WITH defnbase^ DO BEGIN
    sym := badsymbol;
    dummy := TRUE;
    next := NIL;
    defn := NIL
  END;
  pmstackptr:= 1;
  pmstack[1].src_not_set := FALSE; (* suppress recording of source info in dummy level *)
  pmstack[1].marker := defnbase;
  resume_node := defnbase;
  ptree := NIL;
END (*initparse*);
(* procedure to shut down the parser *)
PUBLIC
PROCEDURE endparse;
BEGIN
  DISPOSE (defnbase)
END;
CONST
dcldelim: symbol_set := (* symbols delimiting declarations *)
[ labelsy, constsy, typesy, varsy, functionsy, proceduresy, externalsy,
  publicsy, staticsy, beginsy, endsy, exceptionsy ];
stmtdelim: symbol_set := (* symbols delimiting statements *)
[ beginsy, ifsy, casesy, repeatsy, whilesy, forsy, withsy, loopsy, exitsy,
  endsy, untilsy, gotosy, semicolon, otherssy, eofsy ,
(*  + dcldelim  *)
labelsy, constsy, typesy, varsy, functionsy, proceduresy, externalsy, publicsy
  , staticsy, beginsy, endsy, exceptionsy ];
exprdelim: symbol_set := (* symbols delimiting expressions *)
[ addop, relop, mulop, notsy, rparent, rbracket, dosy, tosy, downtosy, ofsy,
  thensy, comma, elipsis, precsy ,
(*  + stmtdelim  *)
beginsy, ifsy, casesy, repeatsy, whilesy, forsy, withsy, loopsy, exitsy, endsy
  , untilsy, gotosy, semicolon, otherssy, eofsy ,
(*  + dcldelim  *)
labelsy, constsy, typesy, varsy, functionsy, proceduresy, externalsy, publicsy
  , staticsy, beginsy, endsy, exceptionsy ];
(*    the syntactical analyzer    *)
TYPE
parseret = (parsecomplete,parsestackoflo,parseerror);
FUNCTION parse ( VAR code: parseret): parse_node;
LABEL
100, (* instruction decode *)
200, (* instruction fetch + subr return *)
300; (* cooroutine return to caller or parse *)
  PROCEDURE generate ( action: pmsemop );
  FORWARD;
  PROCEDURE forcetrue; (* force at branch of current symbol *)
  BEGIN
    nextic:= pmtable[ic].at;
    needtok := FALSE
  END (*forcenext*);
  PROCEDURE forcefalse; (* force af branch of current symbol *)
  BEGIN
    nextic:= pmtable[ic].af; (* doing this on error can have interesting results *)
    needtok:= FALSE
  END (*forcefalse*);
  PROCEDURE forceloop; (* force re-parse of current symbol *)
  BEGIN
    nextic:= ic;
    needtok := FALSE
  END;
  PROCEDURE inserttoken(insertion: symbols); (*insert a symbol*)
  BEGIN
    IF NOT savedtok THEN BEGIN (*don't cream previously saved token*)
      savedsym:= tokensym;
      savedtok:= TRUE (*save current token and flag it*)
    END;
    tokensym:= insertion; (*insert the token*)
    needtok:= FALSE (*and make sure the parser doesn't skip it*)
  END;
  PROCEDURE skip; (* skip the current symbol *)
  BEGIN
    IF savedtok THEN BEGIN
      tokensym := savedsym;
      savedtok := FALSE
    END
    ELSE tokensym := scan;
    needtok := FALSE (* make sure parser sees this symbol *)
  END;
  PROCEDURE skipuntil(targets: symbol_set); (*find desired token(s)*)
  BEGIN
    IF tokensym <> eofsy THEN skip; (* always skip at least one symbol *)
    WHILE NOT (tokensym IN targets) DO skip;
    needtok:= FALSE (*make sure parser sees this symbol*)
  END;
  PROCEDURE err_after (code: err_codes);
  BEGIN
    err_print (code, last_token.source, '', last_token.column + last_token.
      LENGTH)
  END;
  PROCEDURE err_skip_next (code: err_codes; targets: symbol_set);
  BEGIN
    err_token (code, token);
    skipuntil (targets + [eofsy]); (* don't skip past EOF ever! *)
    IF token.sym = eofsy THEN forcefalse
    ELSE forcetrue
  END;
  PROCEDURE err_last_skip_next (code: err_codes; targets: symbol_set);
  BEGIN
    err_after (code);
    skipuntil (targets + [eofsy]);
    IF token.sym = eofsy THEN forcefalse
    ELSE forcetrue
  END;
  PROCEDURE err_insert (code: err_codes; insertion: symbols);
  BEGIN
    err_token (code, token);
    inserttoken (insertion);
    forceloop
  END;
  PROCEDURE err_last_insert (code: err_codes; insertion: symbols);
  BEGIN
    err_after (code);
    inserttoken (insertion);
    forceloop
  END;
  PROCEDURE find_loop (targets: symbol_set);
  VAR
  saved_token: token_type;
  BEGIN
    saved_token := token;
    skipuntil (targets + stmtdelim + [eofsy]); (* see if we can find what caller wants *)
    IF token.sym IN targets (* found target, preceding was junk *)
    THEN err_token (err_bad_clause, saved_token);
    (* Retry failing construct. If a target was found, then parse should continue
	 normally having skipped the junk. If a target was not found, then same
	 error will occur, but this time at an Ending symbol. *)
    IF token.sym = eofsy THEN forcefalse
    ELSE forceloop
  END;
  PROCEDURE insert_nonterminal ( nt: symbols );
  BEGIN
    generate (attachnt); (* collect excess stacked nodes *)
    lastnode^.sym := nt; (* may not have right type *)
    lastnode^.dummy := TRUE;
    generate (pmtable[ic].sg); (* chain into tree as it should be, if no
						       action specified, nonexistent nosemop case
						       will be taken. *)
  END;
  PROCEDURE fake_qualifier ( nt: symbols );
  BEGIN
    generate (attachqual);
    lastnode^.sym := nt;
    lastnode^.dummy := TRUE;
  END;
  PROCEDURE err_missing_expr (code: err_codes; targets: symbol_set);
  BEGIN
    err_last_skip_next (code, targets);
    insert_nonterminal (exprtree)
  END;
  PROCEDURE generate (* action: pmsemop *);
  VAR
  node, a, b, op: parse_node;
  BEGIN
    CASE action OF
    (* ATTACH pushes a terminal symbol on the parse tree stack. *)
      attach: BEGIN
	NEW (node);
	node^ := token; (* get info from current token *)
	node^.sym := tokensym; (* in case of an inserted token *)
	WITH node^ DO BEGIN
	  defn := NIL;
	  next := NIL;
	END;
	lastnode^.next := node; (* chain to end of defn *)
	lastnode := node;
      END;
      (* ATTACH NT pushes a nonterminal symbol node onto the parse tree stack.
	     The definition of the nonterminal is made up of all symbol pushed
	     *after* the recognition "call" for the nonterminal.  The action is:
	     marker[sp+1] -> ..., new NT => marker[sp+1] -> NT (...).  The parens
	     enclose the definition chain. *)
      attachnt: BEGIN
	NEW (node);
	WITH node^ DO BEGIN (* construct nonterminal info *)
	  sym := pmtable[ic].sym;
	  WITH pmstack[pmstackptr+1] DO BEGIN
	    dummy := src_not_set;
	    source := src;
	    column := col;
	    defn := marker^.next;
	    marker^.next := node; (* replace the end of the list *)
	  END;
	  next := NIL;
	END;
	lastnode := node;
      END;
      (* ATTACH QUALifier pushes a nonterminal node whose definition is all nodes
	     pushed on the parse tree stack after the recognition call of the *superior*
	     nonterinal symbols.  This is used to define postfix operations such as
	     <array qualifier>.  Action:  marker[sp] -> ..., new NT => NT (...)       *)
      attachqual: BEGIN
	NEW (node);
	WITH node^ DO BEGIN (* construct nonterminal info *)
	  sym := pmtable[ic].sym;
	  WITH pmstack[pmstackptr] DO BEGIN
	    dummy := src_not_set;
	    source := src;
	    column := col;
	    defn := marker^.next;
	    marker^.next := node; (* replace the end of the list *)
	  END;
	  next := NIL;
	END;
	lastnode := node;
      END;
      (* INFIX OP is called to construct an operator subtree, using the top three
	     nodes on the stack.  It is called upon recognition of the second operand,
	     and appends the operands to the operator's definition chain.  The subtree
	     thus built is pushed. Action:  marker[sp] -> A -> OP -> B -> ... =>
	     marker[sp] -> OP (A -> B -> ...).   Note that this means that there
	     may be n right-hand operands, and it is used as such to chain together
	     the represenation of subranges with PREC attributes. *)
      infixop: BEGIN
	WITH pmstack[pmstackptr] DO BEGIN
	  a := marker^.next; (* collect nodes *)
	  op := a^.next;
	  b := op^.next;
	  a^.next := b; (* mark operation defn *)
	  op^.defn := a;
	  marker^.next := op; (* replace triple with op *)
	  op^.next := NIL;
	  lastnode := op; (* last node on chain *)
	END
      END;
      (* ATTACH OPTion chains a set of suboptions to an option identifier.  This
	     action is performed *only* after a recognition call of <option>.  The
	     effect is: marker[sp+1] -> a -> ... => marker [sp+1] -> a (...).
	     "*Option" is recognized as a special case:
	     marker[sp+1] -> * -> a -> ... => marker[sp+1] -> * (a (...)). *)
      attachopt: BEGIN
	WITH pmstack [pmstackptr+1] DO BEGIN
	  WITH marker^.next^ DO BEGIN
	    IF sym = starsy THEN BEGIN (* push option under star *)
	      next^.defn := next^.next;
	      next^.next := NIL;
	    END;
	    defn := next; (* put suboptions on defn chain *)
	    next := NIL;
	  END;
	  lastnode := marker^.next;
	END;
      END;
      (* RETURN TERM attaches a terminal symbol and then returns it as a parse
	     tree, all by itself. *)
      returnterm: BEGIN
	resume_node := lastnode;
	generate (attach);
	GOTO 300;
      END;
      (* RETURN NT is creates a nonterminal with it definition and returns the
	     parse tree. *)
      returnnt: BEGIN
	generate (attachnt); (* do it the easy way *)
	resume_node := pmstack[pmstackptr+1].marker;
	GOTO 300
      END;
      (* RETURN QUAL performs an attachqual and then returns the parse tree. *)
      returnqual: BEGIN
	generate (attachqual);
	resume_node := pmstack[pmstackptr].marker;
	GOTO 300;
      END;
      (* RETURN SUBR is used to construct and return a subr_decl node. The unusual
	     feature of this action is that it is called in the middle of the production
	     for <subr decl>.  This is necessary because we want to return the header
	     information for the subroutine before constructing the parse tree(s) for
	     the body.  This operation is invoked *only* in the SG or FG of the
	     recognition of FORWARD in <subr declaration>.  The parse tree returned
	     as the definition of the <subr decl> node includes everything processed
	     since the start of the production *containing* the <subr declaration>
	     reference; this forces inclusion of a PUBLIC or STATIC keyword if there
	     is one.  Note that care is taken to check if FORWARD is present, and to
	     append it to the declaration if it is. *)
      returnsubr: BEGIN
	IF token.sym = forwardsy THEN generate (attach); (* push FORWARD on the stack if present *)
	NEW (node);
	WITH node^ DO BEGIN
	  sym := subr_decl;
	  WITH pmstack[pmstackptr-1] DO BEGIN
	    dummy := src_not_set;
	    source := src;
	    column := col;
	    defn := marker^.next;
	    marker^.next := node;
	  END;
	  next := NIL;
	END;
	lastnode := node;
	resume_node := pmstack[pmstackptr-1].marker;
	GOTO 300;
      END;
      (************* Syntactic Error Recovery Actions **************)
      (* EOF not found after end of program *)
      extratext: BEGIN
	error (err_following_text); (* halt compilation *)
	STOP
      END;
      (* program name not found in PROGRAM statement, just continue *)
      progidexp: err_last_skip_next (err_prog_id_exp, stmtdelim + dcldelim);
      (* semicolon not found when expected *)
      semiexp: err_last_insert (err_semi_expected, semicolon);
      (* body of main program not found *)
      progexp: BEGIN
	IF token.sym = eofsy THEN err_token (err_prog_exp, last_token) (* no body at all *)
	ELSE err_insert (err_no_begin, beginsy); (* assume he forgot begin *)
      END;
      (* no dot terminating program *)
      dotexp: BEGIN
	err_token (err_dot_exp, last_token);
	skipuntil ([eofsy]); (* if not semicolon instead, it is a bizarre error *)
	forcetrue (* so skip semi or scan 'til end *)
      END;
      (* Missing declaration following VAR, CONST, etc. *)
      declexp: BEGIN
	err_after (err_no_declaration);
	forcetrue;
      END;
      (* Missing declaration following PUBLIC, EXTERNAL, etc. *)
      declrequired: BEGIN
	err_last_skip_next (err_no_declaration, dcldelim);
	fake_qualifier (declaration);
      END;
      (* Missing label id, <intconst>, in LABEL declaration. *)
      labelidexp: err_skip_next (err_no_label, [comma, semicolon] + dcldelim);
      (* <declarations> ::= <declaration> production fails. If this is the end
	     of a list of declarations, then everything is okay. Otherwise, it is
	     likely that the user has blown a declaration -- if we allow the
	     production to fail, we will skip all subsequent declarations until we
	     see the body of the procedure or program; so try to recover at the start
	     of the next declaration. *)
      nodecl: BEGIN
	IF NOT (token.sym IN [beginsy, period, endsy, eofsy])
	  THEN err_skip_next (err_no_declaration, dcldelim)
      END;
      (* Semicolon not found between declarations.  If this is the last declaration,
	     i.e. "." or END (in the case of a data module) follows, then this is okay.
	     Otherwise, there is an error, and we must try to recover. *)
      noending: BEGIN
	IF NOT (token.sym IN [period, endsy])
	  THEN err_last_insert (err_semi_expected, semicolon);
      END;
      (* Missing type declaration in VAR, CONST, parameter, or return value
	     declarations. *)
      typeexp: BEGIN
	err_last_skip_next (err_no_type_decl, [becomes, relop (* = *),
	  semicolon, rparent] + dcldelim);
	insert_nonterminal (type_decl);
      END;
      (* Missing expression giving value of a constant *)
      initvalexp: err_missing_expr (err_no_const_val, [semicolon] + dcldelim);
      (* Missing colon in declarations, case labels, statement labels, on prefixes.
	     Often an equals will be used in its place in a declaration; so if we find
	     an '=' we skip to the next token and continue.  Otherwise, the most likely
	     case is that it has been forgotten. *)
      colonexp: BEGIN
	IF (token.sym = relop) ANDIF (token.op = eqop) THEN BEGIN
	  err_token (err_colon_exp, token); (* mark error at the '=' *)
	  skip;
	  forcetrue (* continue with following token *)
	END
	ELSE err_last_insert (err_colon_exp, colon)
      END;
      (* Missing identifier after PROCEDURE or FUNCTION. *)
      subrnameexp: BEGIN
	err_last_skip_next (err_subr_id_exp, [lparent, colon, semicolon] +
	  stmtdelim + dcldelim);
	generate (attach); (* attach dummy id node with nil name *)
	lastnode^.dummy := TRUE;
	lastnode^.sym := ident;
	lastnode^.name := NIL
      END;
      (* Missing subroutine body. This is an error if the subroutine is not
	     external. The forward case is caught by an explicit production. *)
      subrbodyexp: BEGIN
	err_token (err_subr_body_exp, last_token); (* as body is optional, parse will continue *)
	insert_nonterminal (null_stmt);
	resume_node := pmstack[pmstackptr+1].marker;
	GOTO 300;
      END;
      (* Missing parameter declaration within a parameter list *)
      parmdeclexp: err_skip_next (err_parm_decl_exp, [semicolon, rparent,
	colon] + dcldelim);
      (* Missing identifier in identifier list, scalar type declaration, on
	     statement, or variant part declaration. *)
      idexp: err_last_skip_next (err_id_expected, [comma, rparent, colon,
	semicolon, ofsy, dosy] + dcldelim);
      (* <relop> found when '=' is required. Check if the operator is in fact
	     equals, and if so, continue. Otherwise, force a false return from the
	     <equals> ::= <relop> production. *)
      chkeq: BEGIN
	IF token.op <> eqop THEN forcefalse
      END;
      (* <mulop> found when '*' expected. Process as for chkeq above. *)
      chkstar: BEGIN
	IF token.op <> mul THEN forcefalse
      END;
      (* <addop> found when <sign> expected. Check that op is '+' or '-' *)
      chksign: BEGIN
	IF NOT (token.op IN [plus, minus]) THEN forcefalse
	ELSE generate (attach)
      END;
      (* Equals expected. A colon is often used in its place in declarations;
	     if a colon is found, warn programmer and continue. Otherwise, guess
	     that it has been omitted. *)
      eqexp: BEGIN
	IF token.sym = colon THEN BEGIN
	  err_token (err_eq_expected, token); (* mark error at the ':' *)
	  skip;
	  forcetrue (* coe with next token *)
	END
	ELSE BEGIN (* would normally insert, but eq is a relop *)
	  err_token (err_eq_expected, last_token);
	  forcetrue
	END
      END;
      (* Left bracket expected *)
      lbracketexp: err_insert (err_lbracket_exp, lbracket);
      (* Index type specification missing within an array declaration *)
      idxtypeexp: BEGIN
	err_skip_next (err_no_idx_type, [comma, rbracket] + dcldelim);
	insert_nonterminal (type_decl);
      END;
      (* missing OF in record case clause or case statement *)
      ofexp: err_last_insert (err_of_expected, ofsy);
      (* No semicolon ending a field declaration has been seen. This is acceptable
	     if the declaration is the last in the list. *)
      nofieldsemi: BEGIN
	IF NOT (token.sym IN [endsy, rparent])
	  THEN err_last_insert (err_semi_expected, semicolon)
      END; (* resume parse on AT branch *)
      (* At least one variant not found within record case clause. *)
      novariant: err_skip_next (err_no_variant, [endsy] + dcldelim);
      (* No case label found. If at the END of the case, this is okay.  Otherwise
	     assume that the label has been fouled up or forgotten altogether, and
	     try to recover to parse the member statement. *)
      nocaselab: BEGIN
	IF NOT (token.sym IN [endsy, rparent]) THEN BEGIN (* used in record variants too *)
	  IF token.sym IN ([colon, lparent] + stmtdelim)
	    THEN err_token (err_case_label_exp, token) (* no apparent label *)
	  ELSE BEGIN (* garbage *)
	    err_token (err_bad_case_label, token);
	    skipuntil ([colon] + stmtdelim) (* try to get something recognizable *)
	  END;
	  IF token.sym <> colon THEN inserttoken (colon); (* so we can continue *)
	  forcetrue (* continue parse with ':' *)
	END
      END;
      (* Left parenthesis expected. *)
      lparenexp: err_insert (err_lparent_exp, lparent);
      (* No statement following ";" (or another statement) in a statement list.
	     If not a valid delimiter, assume that we have trash for a statement and
	     skip until the next statement can be found. *)
      nostatement: BEGIN
	IF NOT (token.sym IN [endsy, untilsy, exceptionsy, semicolon] +
	  dcldelim) THEN err_skip_next (err_bad_stmt, stmtdelim)
      END;
      (* No semicolon found after a statement in a statement list. If this is the
	     end of the list, okay; otherwise, error. *)
      nosemi: BEGIN
	IF NOT (token.sym IN [endsy, untilsy, exceptionsy]) THEN BEGIN
	  IF ((last_token.source.file_no = token.source.file_no) AND
	    (last_token.source.page_no = token.source.page_no) AND
	    (last_token.source.line_no = token.source.line_no)) AND
	    NOT (token.sym IN stmtdelim) THEN BEGIN (* assume both part of malformed statement *)
	    err_token (err_stmt_end, token);
	    skipuntil (stmtdelim); (* look for real end *)
	    forceloop
	  END
	  ELSE err_last_insert (err_semi_expected, semicolon)
	END (* assume statement break *)
      END;
      (* no right hand side of assignment statement *)
      norhs: err_missing_expr (err_no_rhs, stmtdelim);
      (* not even one handler found in handler clause *)
      nohandler: err_skip_next (err_no_handler, stmtdelim);
      (* no predicate expression following IF or EXIT IF *)
      noifpred: err_missing_expr (err_no_if_pred, [thensy, elsesy, dosy] +
	stmtdelim);
      (* THEN not found in IF statement *)
      thenexp: err_last_insert (err_then_exp, thensy);
      (* ELSE not found after IF-THEN, check for semicolon-ELSE *)
      chksemielse: IF token.sym = semicolon THEN BEGIN
	skip; (* see what comes next *)
	IF token.sym = elsesy THEN BEGIN
	  err_token (err_semi_before_else, last_token);
	  forceloop; (* ignore the semicolon *)
	END
	ELSE inserttoken (semicolon); (* restore the semicolon *)
      END;
      (* FOR index missing *)
      foridxexp: err_skip_next (err_for_idx_exp, [becomes, tosy, downtosy,
	dosy] + stmtdelim);
      (* ':=' missing from FOR statement *)
      becomesexp: BEGIN
	IF token.sym IN ([tosy, downtosy, dosy] + stmtdelim)
	  THEN err_skip_next (err_for_init_exp, [tosy, downtosy, dosy] +
	  stmtdelim)
	ELSE find_loop ([becomes, tosy, downtosy, dosy])
      END;
      (* initialization expression missing in FOR *)
      forinitexp: err_missing_expr (err_for_init_exp, [tosy, downtosy, dosy] +
	stmtdelim);
      (* TO or DOWNTO not found in FOR statement *)
      forwayexp: BEGIN
	IF token.sym IN ([dosy] + stmtdelim)
	  THEN err_skip_next (err_for_limit_exp, [dosy] + stmtdelim)
	ELSE find_loop ([tosy, downtosy, dosy])
      END;
      (* missing terminal expression in FOR statement *)
      forfinexp: err_missing_expr (err_for_limit_exp, [dosy] + stmtdelim);
      (* missing DO in FOR or WHILE statement *)
      doexp: BEGIN
	IF token.sym IN stmtdelim THEN err_insert (err_do_exp, dosy)
	ELSE find_loop ([dosy])
      END;
      (* missing UNTIL clause at end of REPEAT loop *)
      untilexp: err_insert (err_until_exp, untilsy);
      (* missing predicate in UNTIL clause *)
      untilpredexp: err_missing_expr (err_no_until_pred, stmtdelim);
      (* missing predicate after WHILE *)
      whilepredexp: err_missing_expr (err_no_while_pred, [dosy] + stmtdelim);
      (* At least one case not found in case statement *)
      nocase: err_skip_next (err_no_case, stmtdelim);
      (* Missing statement after case label.  It is acceptable that there be no
	     statement.  However, here we check that in fact we have a null statement
	     instead of garbage.  In the former case, we return allowing the parser
	     to continue because a <statement> is optional.  In the latter, we skip
	     until we see something reasonable. *)
      stmtincaseexp: BEGIN
	IF NOT (token.sym IN [endsy, semicolon])
	  THEN err_skip_next (err_no_stmt, stmtdelim);
	generate (returnnt);
      END;
      (* There is no semicolon delimiting member statement in a CASE. This is
	     in error if the statement is not the last one in the case. *)
      nocasedelim: BEGIN
	IF token.sym <> endsy THEN err_last_insert (err_semi_expected,
	  semicolon)
      END;
      (* No reference after WITH *)
      nowithref: err_last_skip_next (err_no_with_ref, [dosy, comma] +
	stmtdelim);
      (* No label after GOTO. *)
      labelexp: err_last_skip_next (err_no_target_label, stmtdelim);
      (* END statement expected *)
      endstmtexp: err_insert (err_end_exp, endsy);
      (* missing IF in EXIT IF clause *)
      ifexp: err_insert (err_if_exp, ifsy);
      (* missing operand to +, *, AND, NOT, etc. *)
      operandexp: err_missing_expr (err_no_operand, exprdelim + stmtdelim);
      (* Missing expression after "(", ",", BY, etc. *)
      expressionexp: err_missing_expr (err_no_expression, exprdelim +
	stmtdelim);
      (* right parenthesis expected *)
      rparenexp: err_insert (err_no_rparent, rparent);
      (* right bracket expected *)
      rbracketexp: err_insert (err_no_rbracket, rbracket);
      (* missing <range> or <rangelist> *)
      rangeexp: err_skip_next (err_range_exp, [comma, colon, rbracket, elipsis
	] + stmtdelim);
      (* missing upper bound after '..' *)
      upbexp: err_missing_expr (err_upb_exp, [comma, rbracket, elipsis, colon]
	+ stmtdelim);
      (* No field identifier after "." *)
      fieldexp: err_last_skip_next (err_no_field, exprdelim + stmtdelim);
      (* Have an '=' instead of ':='. Just give a warning. *)
      eqinstead: err_token (err_eq_instead, token) (* BNF is such that parse continues normally *)
    END (* case action *);
  END;
BEGIN
(* init tree pointers to build new construct *)
  lastnode := resume_node;
  lastnode^.next := NIL; (* don't hang on to (nonexistent) old tree *)
  (* continue with indicated next pm instruction *)
  200: IF nextic >= minpmaddr THEN BEGIN (* pm goto *)
    ic:= nextic;
    GOTO 100 (* back to decode *)
  END;
  (* some kind of pm subroutine return *)
  ic := pmstack[pmstackptr].retic;
  pmstackptr:= pmstackptr-1; (* back to call instruction *)
  (* take at or af of this (calling) instruction according to return *)
  IF pmstackptr <= 0 THEN BEGIN (* back to head of parse *)
    IF nextic <> succrt THEN BEGIN (* error not recovered from *)
      parse := NIL;
      code:= parseerror;
      RETURN (* stop the parse *)
    END
    ELSE BEGIN (* return normally *)
      parse := NIL;
      code:= parsecomplete;
      RETURN
    END
  END
  ELSE WITH pmtable[ic] DO BEGIN (* some kind of return *)
    IF nextic = succrt THEN BEGIN
      nextic:= at;
      IF sg<>nosemop THEN generate(sg)
    END
    ELSE IF nextic = failrt THEN BEGIN
      nextic:= af;
      IF fg<>nosemop THEN generate(fg)
    END
    ELSE BEGIN (* nextic = err_rt *)
      code:= parseerror;
      RETURN
    END;
    GOTO 200 (* decide where to go next *)
  END; (* some kind of return *)
  (*  decode current pm instruction op code  *)
  100: WITH pmtable[ic] DO BEGIN
    IF sym < nonterminal THEN BEGIN
      IF needtok THEN BEGIN (*must scan new symbol*)
	IF savedtok THEN BEGIN
	  tokensym := savedsym;
	  savedtok := FALSE
	END
	ELSE tokensym := scan;
	needtok:= FALSE
      END;
      IF tokensym = sym THEN BEGIN (*recognition*)
	nextic:= at;
	needtok:= TRUE; (*symbol has been recognized*)
	IF sg<>nosemop THEN generate(sg);
      END
      ELSE BEGIN (*no recognition*)
	nextic:= af;
	IF fg<>nosemop THEN generate(fg) (*may change nextic*)
      END;
      GOTO 200 (* go see what to do next *)
    END
    ELSE BEGIN (* non-terminal symbol -- perform pm subroutine call *)
      IF pmstackptr < maxpmstack THEN pmstackptr:= pmstackptr+1
      ELSE BEGIN (* stack overflow *)
	code:= parsestackoflo;
	parse := NIL;
	RETURN
      END;
      WITH pmstack[pmstackptr] DO BEGIN
	retic := ic;
	marker := lastnode;
	IF needtok THEN BEGIN (* get next token *)
	  IF savedtok THEN BEGIN
	    tokensym := savedsym;
	    savedtok := FALSE
	  END
	  ELSE tokensym := scan;
	  needtok := FALSE
	END;
	src := token.source;
	col := token.column;
	src_not_set := token.dummy;
      END;
      ic:= ntaddr;
      GOTO 100 (* go decode opcode *)
    END
  END; (* decoding *)
  (*  Return current parse tree to caller.  Assumes that this is done only
	on an SG call, with nextic properly set. *)
  300: parse := lastnode;
  code := parsecomplete;
END (*parse*);
(* DEL PTREE disposes of a specified parse tree. *)
PUBLIC
PROCEDURE del_ptree ( pt: parse_node );
VAR
cur, next: parse_node;
BEGIN
  IF pt <> NIL THEN BEGIN
    cur := pt^.defn;
    WHILE cur <> NIL DO BEGIN
      next := cur^.next;
      del_ptree (cur);
      cur := next;
    END;
    DISPOSE (pt);
  END;
END (* del_ptree *);
(* GET PTREE will dispose of any existing existing parse tree, and will then
   call the parser to get a new parse tree.  Any parser error will be reported
   here.  Any parse subtree which is to be kept across a call to this routine
   should be cut out of the current parse tree, to keep it from being deleted.
   The root of the fetched parse tree will be in the global variable PTREE. *)
PUBLIC
PROCEDURE get_ptree;
VAR
perr: parseret;
BEGIN
  IF ptree <> NIL THEN del_ptree (ptree);
  ptree := parse (perr);
  CASE perr OF
    parsestackoflo: error (err_parse_overflow);
    parseerror: error (err_pr_error);
    parsecomplete:
  END;
  IF switch (cur_block^.dump_switches, 'PT') THEN dmp_ptree (ptree);
END (* get_ptree *);
(*  CHECK TYPES is called at the end of an environment compilation to examine
    all the type identifiers defined in the root block, and print error messages
    if any of them are undefined.  *)
PROCEDURE check_types;
VAR
ts: sym;
BEGIN
  ts := root_block^.type_list.first;
  WHILE ts <> NIL DO BEGIN
    WITH ts^.type_desc^ DO BEGIN
      IF (kind = unknown_type) ANDIF (type_id <> NIL) ANDIF
	(type_id^.name <> NIL) THEN BEGIN
	err_print (err_type_warning, declaration, type_id^.name^.TEXT, 0);
	declaration := null_source;
      END;
    END;
    ts := ts^.next;
  END;
END (* check_types *);
EXTERNAL VAR
abort: co_routine; (* action to perform on fatal error *)
PROCEDURE do_pass_1;
VAR
dtime: dtime_ext;
CONST
reader_stack_size = 2000;
abort_stack_size = 10;
LABEL
100 (* abort *);
  PROCEDURE abt_pass1;
  BEGIN
    detach; (* after creation *)
    fin_source := cur_source; (* record line where reading aborted *)
    GOTO 100;
  END;
BEGIN (* do_pass_1 *)
  ;
  dtime := dc_ext (daytime ());
  cdatesym^.init_value.valp^.str_val [1:9] := SUBSTR (dtime, 1, 9);
  ctimesym^.init_value.valp^.str_val [1:8] := SUBSTR (dtime, 11, 8);
  all_opts := [ ];
  cur_block := root_block;
  ext_block := NIL;
  lex_block := NIL;
  blk_number := 0;
  max_level := 0;
  heap_chain := NIL;
  sym_vl_number := vl_base;
  sym_nvl_number := nvl_base;
  vl_list := vll_base;
  err_count := 0;
  warnings := 0;
  linect := 0;
  inclct := 0;
  elf_status := unopened;
  elf_open;
  df_status := unopened; (* tell PASDMP to open on first reference *)
  xrf_init;
  lex_init;
  tal_init;
  alc_init;
  initparse;
  ch_init;
  ch_open (FALSE, TRUE);
  IF prog_options.global_opt THEN glob_init;
  abort := create (abt_pass1, abort_stack_size);
  reader := create (read_input_lines, reader_stack_size);
  semantics;
  IF switch (root_block^.dump_switches, 'NAMES') THEN dump_name_table;
  IF switch (root_block^.dump_switches, 'ROOTST') THEN BEGIN
    dmpblock (root_block);
    dmpstable (root_block);
  END;
  finish := (NOT env_compilation) AND ( (max_severity = 0) OR
    ((max_severity = 1) AND prog_options.finish_opt) );
  endparse;
  IF env_compilation THEN check_types;
  IF finish THEN BEGIN
    fin_graph; (* includes the quick block analysis *)
    allocate_storage; (* gives addresses to all var, value and const symbols *)
  END;
  100 (* abort *): DISPOSE (abort);
  DISPOSE (reader);
  xrf_close;
  elf_close;
  dmp_close;
  ch_close;
  IF prog_options.global_opt THEN glob_term;
END (* do_pass_1 *);
(* NEXT PASS saves the heap and, if the runoff flag is set, initiates the next
   pass. *)
PROCEDURE next_pass;
VAR
next: PACKED ARRAY [1..6] OF CHAR;
BEGIN
  opts_listing := ( list_file <> '' ) ANDIF ( src_selected ORIF
    ([symbols_opt, xref_opt, calls_opt] * all_opts <> []) ORIF
    ( prog_options.code_opt ANDIF ([assembly_opt, map_opt] * all_opts <> [])
    ANDIF prog_options.banner_opt ) );
  IF (rel_file = '') AND ( (list_file = '') OR NOT (assembly_opt IN all_opts)
    ) THEN prog_options.code_opt := FALSE;
  quick := NOT have_optimizer OR ( have_checkout AND
    ( ( prog_options.quick_opt = opt_is_on ) OR
    ( ( prog_options.quick_opt = opt_is_auto ) AND
    NOT ( optimize_opt IN all_opts ) ) ) );
  IF finish AND (prog_options.code_opt OR (prog_options.dump_switches <> NIL))
    THEN BEGIN
    IF quick THEN BEGIN
      IF opts_listing OR (err_count <> 0) THEN next := 'PASLST'
      ELSE next := tmprefix || 'CCG';
    END
    ELSE next := 'PASOPT'; (*tmprefix || 'SHP';*)
  END
  ELSE IF opts_listing OR (err_count <> 0) THEN next := 'PASLST'
  ELSE next := 'PASCAL';
  log_record.no_lines := linect;
  log_record.no_incl_lines := inclct;
  log_record.no_errors := err_count - warnings;
  log_record.date_and_time := root_block^.children^.comp_dtime;
  log_record.alloc_strategy := prog_options.alloc_mode;
  log_record.opt_debug := prog_options.debug_opt;
  log_record.opt_check := ([MINIMUM (checklist) .. MAXIMUM (checklist)
    ] * all_opts <> []);
  log_record.opt_main := (root_block^.children^.kind = program_blk);
  log_record.opt_overlay := prog_options.overlay_opt;
  log_record.opt_source := src_selected;
  log_record.opt_special := ([MINIMUM (speciallist) .. MAXIMUM (speciallist)
    ] * all_opts <> []);
  log_record.opt_terse := prog_options.terse_opt;
  log_record.opt_trace := (trace_opt IN all_opts);
  log_record.opt_xref := prog_options.global_opt;
  log_record.lowseg_size := 0;
  log_record.highseg_size := 0;
  log_record.ki10:=prog_options.ki_code_opt;
  IF next = 'PASCAL' THEN log_write
  ELSE IF NOT wrpas(tempname ('PAS')) THEN STOP;
  IF runoff <> 0 THEN BEGIN
    IF NOT runprg(next || prgm_dir (), 1) THEN BEGIN
      REWRITE (TTY);
      WRITELN (TTY, '?Unable to run ', next)
    END
  END;
END;
(*  SAVE ENVIRONMENT will write the current environment to a specified file.  *)
PROCEDURE save_environment;
VAR
save_main_file_name: FILE_NAME;
BEGIN
  save_main_file_name:=main_file;
  main_file:='';
  cmd_clear;
  pop_switches (default_options.switches, NIL);
  pop_switches (default_options.dump_switches, NIL);
  IF NOT wrpas('.ENV ' || rel_file) THEN STOP;
  main_file:=save_main_file_name;
END;
VAR
start_time: INTEGER;
segstuff: segrecd;
BEGIN
  IF NOT rdpas(tempname ('PA0'), FALSE) THEN BEGIN
    REWRITE (TTY);
    WRITELN ('?Compiler temporary file PA0 lost');
    STOP;
  END;
  REWRITE (TTY);
  start_time := RUNTIME;
  IF open_search (INPUT, '.PAS ' || main_file) THEN ;
  log_record.FILE_NAME := fileblock (INPUT);
  log_record.run_time := start_time;
  IF prog_options.names_opt THEN BEGIN
    WRITELN (TTY, '[Compiling ', FILENAME (INPUT), ']');
    BREAK;
  END;
  root_block^.semantic_options := prog_options.semantic_options;
  root_block^.dump_switches := prog_options.dump_switches;
  do_pass_1;
  IF env_compilation AND ( (max_severity = 0) OR ( (max_severity = 1) AND
    prog_options.finish_opt) ) AND (rel_file <> '') THEN BEGIN
    save_environment;
    REWRITE (TTY);
  END;
  IF prog_options.statistics_opt THEN BEGIN
    seginfo (segstuff);
    WRITELN (TTY, '[Pass 1: ', (RUNTIME - start_time)
      / 1000.0: 8: 3, ' seconds, ', (segstuff.lowlen+511)
      DIV 512: 3, '+', (segstuff.highlen+511) DIV 512: 3, 'P]');
  END;
  next_pass;
END (* pass1 *).
   2ZRX