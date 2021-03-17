$TITLE PASANL.PAS, last modified 1/31/84, zw
MODULE pasanl;
(*TYM-Pascal compiler -- syntax/semantics analysis*)

$HEADER PASANL.HDR

$SYSTEM TYPUTL.TYP
$SYSTEM CMDUTL.INC
$SYSTEM TIMUTL.INC
$SYSTEM SRCUTL.INC
$SYSTEM FLGUTL.INC
$SYSTEM SRCUTL.INC
$SYSTEM PASCAL.INC
$SYSTEM PASENV.INC
$SYSTEM PASIST.INC
$SYSTEM PASIF.TYP
$SYSTEM PASLEX.INC
$SYSTEM PASERR.INC
$SYSTEM PASCMD.typ
$SYSTEM PASRDR.INC
$SYSTEM PASCGR.INC
$SYSTEM PA1XRF.INC
$SYSTEM PASGLB.INC
$SYSTEM PASBLK.INC
$SYSTEM PASTAL.INC
$SYSTEM PASALC.INC
$SYSTEM PASIFU.INC
$SYSTEM PASCV.INC
$SYSTEM PA1DMP.INC
$SYSTEM PASDMP.INC
$SYSTEM PASLOG.INC
$SYSTEM INFPAC.INC
$INCLUDE PASANL.TYP
$INCLUDE PASANL.TAB

STATIC VAR
ic, (*address of current pminst*)
nextic: pmaddr; (*address of next pminst, changed by recovery routines*)
savedtok, (*flags next symbol in savedsym due to insert token*)
needtok: boolean; (*flags scan required on next terminal recognition*)
savedsym, (*contains next symbol saved by inserttoken*)
tokensym: symbols; (*token read by scanner*)
defnbase, (*dummy node to which we chain everything*)
lastnode, (*last node chained to list*)
resume_node: parse_node; (*new "last node" after return*)

(*parse stack*)
CONST maxpmstack = 200;
VAR
pmstackptr: 0 .. maxpmstack;
pmstack: ARRAY [1 .. maxpmstack] OF RECORD
  retic: pmaddr; (*ic of calling instruction*)
  src: source_id; (*position in text of construct*)
  col: line_index;
  src_not_set: BOOLEAN; (*flags whether above is valid*)
  marker: parse_node (*last node pushed prior to recognition call*)
END;

PUBLIC VAR ptree: parse_node;

$PAGE initparse, endparse

PUBLIC PROCEDURE initparse;
(*initialize syntax analyzer*)
BEGIN
  needtok := TRUE;
  nextic := minpmaddr;
  NEW(defnbase); (*define dummy node for base of tree*)
  WITH defnbase^ DO BEGIN
    sym := badsymbol; dummy := TRUE; next := NIL; defn := NIL
  END;
  pmstackptr:= 1;
  (*suppress recording of source info in dummy level*)
  pmstack[1].src_not_set := FALSE; pmstack[1].marker := defnbase;
  resume_node := defnbase;
  ptree := NIL;
END;

PUBLIC PROCEDURE endparse;
(*shut down the parser*)
BEGIN
  DISPOSE(defnbase)
END;

$PAGE parser symbol set constants

CONST
  DclDelim: symbol_set := (*symbols delimiting declarations*)
    [labelsy, constsy, typesy, varsy, functionsy, proceduresy,
      externalsy, publicsy, staticsy, beginsy, endsy, exceptionsy];
  StmtDelim: symbol_set := (*symbols delimiting statements*)
    [ beginsy, ifsy, casesy, repeatsy, whilesy, forsy, withsy,
      loopsy, exitsy, endsy, untilsy, gotosy, semicolon,
      otherssy, eofsy,
    (* + dcldelim *)
      labelsy, constsy, typesy, varsy, functionsy, proceduresy,
      externalsy, publicsy, staticsy, beginsy, endsy, exceptionsy];
  ExprDelim: symbol_set := (*symbols delimiting expressions*)
    [ addop, relop, mulop, notsy, rparent, rbracket, dosy, tosy,
      downtosy, ofsy, thensy, comma, elipsis, precsy,
    (* + stmtdelim *)
      beginsy, ifsy, casesy, repeatsy, whilesy, forsy, withsy,
      loopsy, exitsy, endsy, untilsy, gotosy, semicolon,
      otherssy, eofsy,
    (* + dcldelim *)
      labelsy, constsy, typesy, varsy, functionsy, proceduresy,
      externalsy, publicsy, staticsy, beginsy, endsy, exceptionsy];

$PAGE parse

TYPE parseret = (parsecomplete,parsestackoflo,parseerror);

FUNCTION PARSE(VAR code: parseret): parse_node;
(*syntactical analyzer*)
LABEL
  100, (*instruction decode*)
  200, (*instruction fetch + subr return*)
  300; (*cooroutine return to caller or parse*)

PROCEDURE generate(action: pmsemop ); FORWARD;

$PAGE error recovery routines in parse

PROCEDURE forcetrue;
(*force at branch of current symbol*)
BEGIN
  nextic:= pmtable[ic].at; needtok := FALSE
END;

PROCEDURE forcefalse;
(*force af branch of current symbol*)
BEGIN
  (*doing this on error can have interesting results*)
  nextic:= pmtable[ic].af; needtok:= FALSE
END;

PROCEDURE forceloop;
(*force re-parse of current symbol*)
BEGIN
  nextic:= ic; needtok := FALSE
END;

PROCEDURE inserttoken(insertion: symbols);
(*insert a symbol*)
BEGIN
  IF NOT savedtok THEN BEGIN (*don't cream previously saved token*)
    savedsym := tokensym;
    savedtok := TRUE (*save current token and flag it*)
  END;
  tokensym := insertion; (*insert the token*)
  needtok := FALSE (*and make sure the parser doesn't skip it*)
END;

PROCEDURE skip;
(*skip the current symbol*)
BEGIN
  IF savedtok THEN BEGIN tokensym := savedsym; savedtok := FALSE END
  ELSE tokensym := scan;
  needtok := FALSE (*make sure parser sees this symbol*)
END;

PROCEDURE skipuntil(targets: symbol_set);
(*find desired token(s)*)
BEGIN
  IF tokensym <> eofsy THEN skip; (*always skip at least one symbol*)
  WHILE NOT (tokensym IN targets) DO skip;
  needtok := FALSE (*make sure parser sees this symbol*)
END;

$PAGE syntactic error recovery

PROCEDURE err_after(code: err_codes);
BEGIN
  err_print(code, last_token.source, '', last_token.column + last_token.length)
END;

PROCEDURE err_skip_next(code: err_codes; targets: symbol_set);
BEGIN
  err_token(code, token);
  skipuntil(targets + [eofsy]); (*don't skip past EOF ever!*)
  IF token.sym = eofsy THEN forcefalse ELSE forcetrue
END;

PROCEDURE err_last_skip_next(code: err_codes; targets: symbol_set);
BEGIN
  err_after(code);
  skipuntil(targets + [eofsy]);
  IF token.sym = eofsy THEN forcefalse ELSE forcetrue
END;

PROCEDURE err_insert(code: err_codes; insertion: symbols);
BEGIN
  err_token(code, token);
  inserttoken(insertion);
  forceloop
END;

PROCEDURE err_last_insert(code: err_codes; insertion: symbols);
BEGIN
  err_after(code);
  inserttoken(insertion);
  forceloop
END;

PROCEDURE find_loop(targets: symbol_set);
VAR saved_token: token_type;
BEGIN
  saved_token := token;
  skipuntil(targets + StmtDelim + [eofsy]); (*try to find what caller wants*)
  IF token.sym IN targets (*found target, preceding was junk*)
  THEN err_token(err_bad_clause, saved_token);
  (*Retry failing construct. If a target was found, then parse should continue
    normally having skipped the junk. If a target was not found, then same
    error will occur, but this time at an Ending symbol.*)
  IF token.sym = eofsy THEN forcefalse ELSE forceloop
END;

PROCEDURE insert_nonterminal(nt: symbols);
BEGIN
  generate(attachnt); (*collect excess stacked nodes*)
  lastnode^.sym := nt; (*may not have right type*)
  lastnode^.dummy := TRUE;
  generate (pmtable[ic].sg) (*chain into tree as it should be, if no
                              action specified, nonexistent nosemop case
                              will be taken.*)
END;

PROCEDURE fake_qualifier(nt: symbols);
BEGIN
  generate(attachqual);
  lastnode^.sym := nt;
  lastnode^.dummy := TRUE
END;

PROCEDURE err_missing_expr(code: err_codes; targets: symbol_set);
BEGIN
  err_last_skip_next(code, targets);
  insert_nonterminal(exprtree)
END;

$PAGE generate in parse

PROCEDURE generate(action: pmsemop);
VAR node, a, b, op: parse_node;
BEGIN
  CASE action OF
    attach: (*pushes a terminal symbol on the parse tree stack.*)
      BEGIN
        NEW(node);
        node^ := token; (*get info from current token*)
        node^.sym := tokensym; (*in case of an inserted token*)
        WITH node^ DO BEGIN defn := NIL; next := NIL END;
        lastnode^.next := node; (*chain to end of defn*)
        lastnode := node;
      END;
    attachnt: (*pushes a nonterminal symbol node onto the parse tree stack.
      The definition of the nonterminal is made up of all symbol pushed
      *after* the recognition "call" for the nonterminal.  The action is:
      marker[sp+1] -> ..., new NT => marker[sp+1] -> NT (...).  The parens
      enclose the definition chain.*)
      BEGIN
        NEW(node);
        WITH node^ DO BEGIN (*construct nonterminal info*)
          sym := pmtable[ic].sym;
          WITH pmstack[pmstackptr + 1] DO BEGIN
            dummy := src_not_set; source := src; column := col;
            defn := marker^.next;
            marker^.next := node; (*replace the end of the list*)
          END;
          next := NIL
        END;
        lastnode := node
      END;
    attachqual: (*pushes a nonterminal node whose definition is all nodes
      pushed on the parse tree stack after the recognition call of the
      *superior* nonterinal symbols.  This is used to define postfix
      operations such as <array qualifier>.
      Action:  marker[sp] -> ..., new NT => NT (...)*)
      BEGIN
        NEW(node);
        WITH node^ DO BEGIN (*construct nonterminal info*)
          sym := pmtable[ic].sym;
          WITH pmstack[pmstackptr] DO BEGIN
            dummy := src_not_set; source := src; column := col;
            defn := marker^.next;
            marker^.next := node (*replace the end of the list*)
          END;
          next := NIL
        END;
        lastnode := node;
      end;

$PAGE

    infixop: (*called to construct an operator subtree, using the top three
      nodes on the stack.  It is called upon recognition of the second operand,
      and appends the operands to the operator's definition chain.  The subtree
      thus built is pushed. Action:  marker[sp] -> A -> OP -> B -> ... =>
      marker[sp] -> OP (A -> B -> ...).   Note that this means that there
      may be n right-hand operands, and it is used as such to chain together
      the represenation of subranges with PREC attributes.*)
      BEGIN
        WITH pmstack[pmstackptr] DO BEGIN
          a := marker^.next; (*collect nodes*)
          op := a^.next; b := op^.next;
          a^.next := b; (*mark operation defn*)
          op^.defn := a;
          marker^.next := op; (*replace triple with op*)
          op^.next := nil;
          lastnode := op (*last node on chain*)
        END
      END;
    attachoption: (*chains a set of subopts to an option identifier.  This
      action is performed *only* after a recognition call of <option>.  The
      effect is: marker[sp+1] -> a -> ... => marker [sp+1] -> a (...).
      "*option" is recognized as a special case:
      marker[sp+1] -> * -> a -> ... => marker[sp+1] -> * (a (...)).*)
      BEGIN
        WITH pmstack[pmstackptr + 1] DO BEGIN
          WITH marker^.next^ DO BEGIN
            IF sym = starsy THEN BEGIN (*push option under star*)
              next^.defn := next^.next; next^.next := NIL
            END;
            defn := next; (*put subopts on defn chain*)
            next := NIL
          END;
          lastnode := marker^.next
        END
      END;
    returnterm: (*attaches a terminal symbol and then returns it as a parse
      tree, all by itself.*)
      BEGIN
        resume_node := lastnode; generate(attach); GOTO 300
      END;
    returnnt: (*creates a nonterminal with it definition and returns the
      parse tree.*)
      BEGIN
        generate(attachnt); (*do it the easy way*)
        resume_node := pmstack[pmstackptr + 1].marker;
        GOTO 300
      END;

$PAGE

    returnqual: (*performs an attachqual and then returns the parse tree.*)
      BEGIN
        generate(attachqual);
        resume_node := pmstack[pmstackptr].marker;
        GOTO 300
      END;
    returnsubr: (*used to construct and return a subr_decl node. The unusual
      feature of this action is that it is called in the middle of the
      production for <subr decl>.  This is necessary because we want to
      return the header information for the subroutine before constructing
      the parse tree(s) for the body.  This operation is invoked *only* in
      the SG or FG of the recognition of FORWARD in <subr declaration>.  The
      parse tree returned as the definition of the <subr decl> node includes
      everything processed since the start of the production *containing* the
      <subr declaration> reference; this forces inclusion of a PUBLIC or
      STATIC keyword if there is one.  Note that care is taken to check if
      FORWARD is present, and to append it to the declaration if it is.*)
      BEGIN
	(*push FORWARD on the stack if present*)
        IF token.sym = forwardsy THEN generate(attach);
        NEW(node);
        WITH node^ DO BEGIN
          sym := subr_decl;
          WITH pmstack[pmstackptr - 1] DO BEGIN
            dummy := src_not_set; source := src; column := col;
            defn := marker^.next; marker^.next := node
          END;
          next := NIL
        END;
        lastnode := node;
        resume_node := pmstack[pmstackptr - 1].marker;
        GOTO 300
      END;

$PAGE syntactic error recovery actions in generate in parse

    extratext: (*EOF not found after end of program*)
      BEGIN
        error(err_following_text); (*halt compilation*)
        STOP
      END;
    progidexp: (*program name not found in PROGRAM statement, just continue*)
      err_last_skip_next(err_prog_id_exp, StmtDelim + DclDelim);
    semiexp: (*semicolon not found when expected*)
      err_last_insert(err_semi_expected, semicolon);
    progexp: (*body of main program not found*)
      BEGIN
        IF token.sym = eofsy
        THEN err_token(err_prog_exp, last_token) (*no body at all*)
        ELSE err_insert(err_no_begin, beginsy) (*assume he forgot begin*)
      END;
    dotexp: (*no dot terminating program*)
      BEGIN
        err_token(err_dot_exp, last_token);
        skipuntil([eofsy]); (*if not semicolon instead, it is a bizarre error*)
        forcetrue (*so skip semi or scan 'til end*)
      END;
    declexp: (*Missing declaration following VAR, CONST, etc.*)
      BEGIN err_after(err_no_declaration); forcetrue END;
    declrequired: (*Missing declaration following PUBLIC, EXTERNAL, etc.*)
      BEGIN
        err_last_skip_next(err_no_declaration, DclDelim);
        fake_qualifier(declaration)
      END;
    labelidexp: (*Missing label id, <intconst>, in LABEL declaration.*)
      err_skip_next(err_no_label, [comma, semicolon] + DclDelim);
    nodecl: (*<declarations> ::= <declaration> production fails. If this is
      the end of a list of declarations, then everything is okay. Otherwise,
      it is likely that the user has blown a declaration -- if we allow the 
      production to fail, we will skip all subsequent declarations until we
      see the body of the procedure or program; so try to recover at the start
      of the next declaration.*)
      BEGIN
        IF NOT (token.sym in [beginsy, period, endsy, eofsy])
	THEN err_skip_next(err_no_declaration, DclDelim)
      END;
    noending: (*Semicolon not found between declarations.  If this is the last
      declaration, i.e. "." or END (in the case of a data module) follows, then
      this is okay. Otherwise, there is an error, and we must try to recover.*)
      BEGIN
        IF NOT (token.sym in [period, endsy])
	THEN err_last_insert(err_semi_expected, semicolon)
      END;

$PAGE

    typeexp: (*Missing type declaration in VAR, CONST, parameter, or return
      value declarations.*)
      BEGIN
        err_last_skip_next(err_no_type_decl,
	  [becomes, relop (*=*), semicolon, rparent] + DclDelim);
        insert_nonterminal(type_decl)
      END;
    initvalexp: (*Missing expression giving value of a constant*)
      err_missing_expr(err_no_const_val, [semicolon] + DclDelim);
    colonexp: (*Missing colon in declarations, case labels, statement labels,
      on prefixes. Often an equals will be used in its place in a declaration;
      so if we find an '=' we skip to the next token and continue.  Otherwise,
      the most likely case is that it has been forgotten.*)
      BEGIN
        IF (token.sym = relop) ANDIF (token.op = eqop) THEN BEGIN
          err_token (err_colon_exp, token); (*mark error at the '='*)
          skip; forcetrue (*continue with following token*)
          END
          ELSE err_last_insert(err_colon_exp, colon)
      END;
     subrnameexp: (*Missing identifier after PROCEDURE or FUNCTION.*)
      BEGIN
        err_last_skip_next(err_subr_id_exp,
	  [lparent, colon, semicolon] + StmtDelim + DclDelim);
        generate(attach); (*attach dummy id node with nil name*)
        lastnode^.dummy := true; lastnode^.sym := ident;
        lastnode^.name := NIL
      END;
    subrbodyexp: (*Missing subroutine body. This is an error if the subroutine
      is not external. The forward case is caught by an explicit production.*)
      BEGIN
        err_token(err_subr_body_exp, last_token);
	(*body is optional, parse will continue*)
        insert_nonterminal(null_stmt);
        resume_node := pmstack[pmstackptr + 1].marker;
        GOTO 300
      END;
    parmdeclexp: (*Missing parameter declaration within a parameter list*)
      err_skip_next(err_parm_decl_exp, [semicolon, rparent, colon] + DclDelim);
    idexp: (*Missing identifier in identifier list, scalar type declaration,
      on statement, or variant part declaration.*)
      err_last_skip_next (err_id_expected,
        [comma, rparent, colon, semicolon, ofsy, dosy] + DclDelim);
    chkeq: (*<relop> found when '=' is required. Check if the operator is in
      fact equals, and if so, continue. Otherwise, force a false return from
      the <equals> ::= <relop> production.*)
      BEGIN
        IF token.op <> eqop THEN forcefalse
      END;

$PAGE

    chkstar: (*<mulop> found when '*' expected. Process as for chkeq above.*)
      BEGIN
        IF token.op <> mul THEN forcefalse
      END;
    chksign: (*<addop> found when <sign> expected. Check that op is '+', '-'*)
      BEGIN
        IF NOT (token.op IN [plus, minus]) THEN forcefalse
        ELSE generate(attach)
      END;
    eqexp: (*Equals expected. A colon is often used in its place in
      declarations; if a colon is found, warn programmer and continue.
      Otherwise, guess that it has been omitted.*)
      BEGIN
        IF token.sym = colon THEN BEGIN
          err_token(err_eq_expected, token); (*mark error at the ':'*)
          skip; forcetrue (*continue with next token*)
        END
        ELSE BEGIN (*would normally insert, but eq is a relop*)
          err_token(q_expected, last_token);
          forcetrue
        END
      END;
    lbracketexp: (*Left bracket expected*)
      err_insert(err_lbracket_exp, lbracket);
    idxtypeexp: (*Index type specification missing in an array declaration*)
      BEGIN
        err_skip_next(err_no_idx_type, [comma, rbracket] + DclDelim);
        insert_nonterminal(type_decl)
      END;
    ofexp: (*missing OF in record case clause or case statement*)
      err_last_insert(err_of_expected, ofsy);
    nofieldsemi: (*No semicolon ending a field declaration has been seen.
      This is acceptable if the declaration is the last in the list.*)
      BEGIN
        IF NOT (token.sym IN [endsy, rparent])
        THEN err_last_insert(err_semi_expected, semicolon)
      END; (*resume parse on AT branch*)
    novariant: (*At least one variant not found within record case clause.*)
      err_skip_next(err_no_variant, [endsy] + DclDelim);

$PAGE

    nocaselab: (*No case label found. If at the END of the case, this is okay.
      Otherwise assume that the label has been fouled up or forgotten
      altogether, and try to recover to parse the member statement.*)
      BEGIN
        IF NOT (token.sym IN [endsy, rparent]) THEN BEGIN
	  (*used in record variants too*)
          IF token.sym IN ([colon, lparent] + StmtDelim)
          THEN err_token(err_case_label_exp, token) (*no apparent label*)
          ELSE BEGIN (*garbage*)
            err_token(err_bad_case_label, token);
            skipuntil([colon] + StmtDelim)
	    (*try to get something recognizable*)
          END;
          IF token.sym <> colon THEN inserttoken(colon);
	  (*so we can continue*)
          forcetrue (*continue parse with ':'*)
        END
      END;
    lparenexp: (*Left parenthesis expected.*)
      err_insert(err_lparent_exp, lparent);
    nostatement: (*No statement following ";" (or another statement) in a
      statement list. If not a valid delimiter, assume that we have trash
      for a statement and skip until the next statement can be found.*)
      BEGIN
        IF NOT (token.sym IN
	  [endsy, untilsy, exceptionsy, semicolon] + dcldelim)
	THEN err_skip_next(err_bad_stmt, StmtDelim)
      END;
    nosemi: (*No semicolon found after a statement in a statement list. If
      this is the end of the list, okay; otherwise, error.*)
      BEGIN
        IF NOT (token.sym IN [endsy, untilsy, exceptionsy]) THEN BEGIN
          IF ((last_token.source.file_no = token.source.file_no)
            AND (last_token.source.page_no = token.source.page_no)
            AND (last_token.source.line_no = token.source.line_no))
            and not (token.sym in StmtDelim)
          THEN BEGIN (*assume both part of malformed statement*)
            err_token(err_stmt_end, token);
            skipuntil(StmtDelim); (*look for real end*)
            forceloop
          END
          ELSE err_last_insert(err_semi_expected, semicolon)
        END (*assume statement break*)
      END;
    norhs: (*no right hand side of assignment statement*)
      err_missing_expr(err_no_rhs, StmtDelim);
    nohandler: (*not even one handler found in handler clause*)
      err_skip_next(err_no_handler, StmtDelim);

$PAGE

    noifpred: (*no predicate expression following IF or EXIT IF*)
      err_missing_expr(err_no_if_pred, [thensy, elsesy, dosy] + StmtDelim);
    thenexp: (*THEN not found in IF statement*)
      err_last_insert(err_then_exp, thensy);
    chksemielse: (*ELSE not found after IF-THEN, check for semicolon-ELSE*)
      IF token.sym = semicolon THEN BEGIN
        skip; (*see what comes next*)
        IF token.sym = elsesy THEN BEGIN
          err_token(err_semi_before_else, last_token);
          forceloop (*ignore the semicolon*)
        END
        ELSE inserttoken(semicolon) (*restore the semicolon*)
      END;
    foridxexp: (*FOR index missing*)
      err_skip_next(err_for_idx_exp,
        [becomes, tosy, downtosy, dosy] + StmtDelim);
    becomesexp: (*':=' missing from FOR statement*)
      BEGIN
        IF token.sym IN ([tosy, downtosy, dosy] + StmtDelim)
        THEN err_skip_next(err_for_init_exp,
	  [tosy, downtosy, dosy] + StmtDelim)
        ELSE find_loop([becomes, tosy, downtosy, dosy])
      END;
    forinitexp: (*initialization expression missing in FOR*)
      err_missing_expr(err_for_init_exp, [tosy, downtosy, dosy] + StmtDelim);
    forwayexp: (*TO or DOWNTO not found in FOR statement*)
      BEGIN
        IF token.sym IN ([dosy] + StmtDelim)
        THEN err_skip_next(err_for_limit_exp, [dosy] + StmtDelim)
        ELSE find_loop([tosy, downtosy, dosy])
      END;
    forfinexp: (*missing terminal expression in FOR statement*)
      err_missing_expr(err_for_limit_exp, [dosy] + stmtdelim);
    doexp: (*missing DO in FOR or WHILE statement*)
      BEGIN
        IF token.sym IN StmtDelim
        THEN err_insert(err_do_exp, dosy)
        ELSE find_loop([dosy])
      END;
    untilexp: (*missing UNTIL clause at end of REPEAT loop*)
      err_insert(err_until_exp, untilsy);
    untilpredexp: (*missing predicate in UNTIL clause*)
      err_missing_expr(err_no_until_pred, StmtDelim);
    whilepredexp: (*missing predicate after WHILE*)
      err_missing_expr(err_no_while_pred, [dosy] + StmtDelim);
    nocase: (*At least one case not found in case statement*)
      err_skip_next(err_no_case, StmtDelim);

$PAGE

    stmtincaseexp: (*Missing statement after case label.  It is acceptable
      that there be no statement.  However, here we check that in fact we
      have a null statement instead of garbage.  In the former case, we return
      allowing the parser to continue because a <statement> is optional.  In
      the latter, we skip until we see something reasonable.*)
      BEGIN
        IF NOT (token.sym IN [endsy, semicolon])
	THEN err_skip_next(err_no_stmt, StmtDelim);
        generate(returnnt)
      END;
    nocasedelim: (*There is no semicolon delimiting member statement in a
      CASE. This is error if the statement is not the last one in the case.*)
      BEGIN
        IF token.sym <> endsy
        THEN err_last_insert(err_semi_expected, semicolon)
      END;
    nowithref: (*No reference after WITH*)
      err_last_skip_next(err_no_with_ref, [dosy, comma] + StmtDelim);
    labelexp: (*No label after GOTO.*)
      err_last_skip_next(err_no_target_label, StmtDelim);
    endstmtexp: (*END statement expected*)
      err_insert(err_end_exp, endsy);
    ifexp: (*missing IF in EXIT IF clause*)
      err_insert(err_if_exp, ifsy);
    operandexp: (*missing operand to +, *, AND, NOT, etc.*)
      err_missing_expr(err_no_operand, ExprDelim + StmtDelim);
    expressionexp: (*Missing expression after "(", ",", BY, etc.*)
      err_missing_expr(err_no_expression, ExprDelim + StmtDelim);
    rparenexp: (*right parenthesis expected*)
      err_insert(err_no_rparent, rparent);
    rbracketexp: (*right bracket expected*)
      err_insert(err_no_rbracket, rbracket);
    rangeexp: (*missing <range> or <rangelist>*)
      err_skip_next(err_range_exp,
        [comma, colon, rbracket, elipsis] + StmtDelim);
    upbexp: (*missing upper bound after '..'*)
      err_missing_expr(err_upb_exp,
        [comma, rbracket, elipsis, colon] + StmtDelim);
    fieldexp: (*No field identifier after "."*)
      err_last_skip_next(err_no_field, ExprDelim + StmtDelim);
    eqinstead: (*Have an '=' instead of ':='. Just give a warning.*)
      err_token(err_eq_instead, token);
    OTHERS: (*BNF is such that parse continues normally*)
  END
END;

$PAGE parse body

BEGIN
  (*init tree pointers to build new construct*)
  lastnode := resume_node;
  lastnode^.next := NIL; (*don't hang on to (nonexistent) old tree*)
  200: (*continue with indicated next pm instruction*)
  IF nextic >= minpmaddr THEN BEGIN (*pm goto*)
    ic:= nextic; GOTO 100 (*back to decode*)
  END;
  (*some kind of pm subroutine return*)
  ic := pmstack[pmstackptr].retic;
  pmstackptr := pmstackptr - 1; (*back to call instruction*)
  (*take at or af of this (calling) instruction according to return*)
  IF pmstackptr <= 0 THEN BEGIN (*back to head of parse*)
    IF nextic <> succrt THEN BEGIN (*error not recovered from*)
      parse := nil; code:= parseerror; RETURN
    END
    ELSE BEGIN (*return normally*)
      parse := NIL; code:= parsecomplete; RETURN
    END
  END
  ELSE WITH pmtable[ic] DO BEGIN (*some kind of return*)
    IF nextic = succrt THEN BEGIN
      nextic:= at;
      IF sg <> nosemop THEN generate(sg)
    END
    ELSE IF nextic = failrt THEN BEGIN
      nextic:= af;
      IF fg <> nosemop THEN generate(fg)
    END
    ELSE BEGIN (*nextic = err_rt*)
      code:= parseerror; RETURN
    END;
    GOTO 200 (*decide where to go next*)
  END; (*some kind of return*)
  (*decode current pm instruction op code*)
  100: WITH pmtable[ic] DO BEGIN
    IF sym < nonterminal THEN BEGIN
      IF needtok THEN BEGIN (*must scan new symbol*)
        IF savedtok THEN BEGIN
          tokensym := savedsym; savedtok := FALSE
        END
        ELSE tokensym := scan;
        needtok:= FALSE
      END;
      IF tokensym = sym THEN BEGIN (*recognition*)
        nextic:= at; needtok:= TRUE; (*symbol has been recognized*)
        IF sg <> nosemop THEN generate(sg);
      END
      ELSE BEGIN (*no recognition*)
        nextic:= af;
        IF fg <> nosemop THEN generate(fg) (*may change nextic*)
      END;
      GOTO 200 (*go see what to do next*)
    END
    ELSE BEGIN (*non-terminal symbol -- perform pm subroutine call*)
      IF pmstackptr < maxpmstack THEN pmstackptr := pmstackptr + 1
      ELSE BEGIN (*stack overflow*)
        code:= parsestackoflo; parse := nil; RETURN
      END;
      WITH pmstack[pmstackptr] DO BEGIN
        retic := ic; marker := lastnode;
        IF needtok THEN BEGIN (*get next token*)
          IF savedtok THEN BEGIN tokensym := savedsym; savedtok := FALSE END
          ELSE tokensym := scan;
          needtok := FALSE
        END;
        src := token.source; col := token.column; src_not_set := token.dummy
      END;
      ic:= ntaddr;
      GOTO 100 (*go decode opcode*)
    END
  END; (*decoding*)
  (*Return current parse tree to caller.  Assumes that this is done only
    on an SG call, with nextic properly set.*)
  300: parse := lastnode; code := parsecomplete;
END;

$PAGE del_ptree, get_ptree

PUBLIC PROCEDURE del_ptree(pt: parse_node); (*should be VAR??*)
(*disposes of a specified parse tree*)
VAR cur, next: parse_node;
BEGIN
  IF pt <> NIL THEN BEGIN
    cur := pt^.defn;
    WHILE cur <> NIL DO BEGIN
      next := cur^.next; del_ptree(cur); cur := next
    END;
    DISPOSE(pt)
  END
END;

PUBLIC PROCEDURE get_ptree;
(*Dispose of any existing existing parse tree, and then call the parser
  to get a new parse tree.  Any parser error will be reported here.  Any
  parse subtree which is to be kept across a call to this routine should 
  be cut out of the current parse tree, to keep it from being deleted.
  The root of the fetched parse tree will be in the global variable PTREE.*)
VAR perr: parseret;
BEGIN
  IF ptree <> NIL THEN del_ptree(ptree);
  ptree := parse(perr);
  CASE perr OF
    parsestackoflo: error(err_parse_overflow);
    parseerror: error(err_pr_error);
    parsecomplete: (*no error*)
  END;
  IF sw(cur_block^.dump_flags, 'PT') THEN dmp_ptree(ptree)
END;

$PAGE check_types

PROCEDURE check_types;
(*This is called at the end of an environment compilation to examine
  all the type identifiers defined in the root block, and print
  error messages if any of them are undefined.*)
VAR ts: sym;
BEGIN
  ts := root_block^.type_list.first;
  WHILE ts <> NIL DO BEGIN
    WITH ts^.type_desc^ DO BEGIN
      IF (kind = unknown_type) ANDIF (type_id <> NIL) ANDIF
        (type_id^.name <> nil) 
      THEN BEGIN
	err_print(err_type_warning, declaration, type_id^.name^.text, 0);
        declaration := null_source
      END
    END;
    ts := ts^.next
  END
END;

$PAGE do_pass_1

PROCEDURE do_pass_1;
(*TYM-Pascal compiler first pass*)
VAR dtime: dtime_ext;
CONST reader_stack_size = 2000;
LABEL 100; (* abort *)
PROCEDURE abt_pass1; (* record line where reading aborted *)
BEGIN detach; fin_source := cur_source; GOTO 100 END;
BEGIN
  dtime := dc_ext(DAYTIME());
  cdatesym^.init_value.valp^.str_val[1:9] := SUBSTR(dtime, 1, 9);
  ctimesym^.init_value.valp^.str_val[1:8] := SUBSTR(dtime, 11, 8);
  semopts := [];
  cur_block := root_block;
  ext_block := NIL; lex_block := NIL;
  blk_number := 0; max_level := 0;
  heap_chain := NIL;
  sym_vl_number := vl_base; sym_nvl_number := nvl_base;
  vl_list := vll_base;
  err_count := 0; warnings := 0;
  linect := 0; inclct := 0;
  elf_status := unopened; elf_open;
  df_status := unopened; (* tell PASDMP to open on first reference *)
  xrf_init; lex_init; tal_init; alc_init;
  initparse;
  ch_init; ch_open(FALSE, TRUE);
  IF glbopts.global_option THEN glob_init;
  abort := abt_pass1; (*procedure to do the actual abort*)
  reader := create(read_input_lines, reader_stack_size);
  semantics; (*do the actual first pass analysis*)
  IF sw(root_block^.dump_flags, 'NAMES') THEN dump_name_table;
  IF sw(root_block^.dump_flags, 'ROOTST') THEN BEGIN
    dmpblock(root_block); dmpstable(root_block)
  END;
  finish := (NOT env_compilation) AND ((max_severity = 0) OR
    ((max_severity = 1) AND glbopts.finish_option));
  endparse;
  IF env_compilation THEN check_types;
  IF finish THEN BEGIN
    fin_graph; (* includes the quick block analysis *)
    allocate_storage; (* gives addresses to all var, value and const symbols *)
  END;
  100: (*abort*)
  DISPOSE(reader);
  xrf_close; elf_close; dmp_close; ch_close;
  IF glbopts.global_option THEN glob_term;
END;

PROCEDURE write_environment(environment: file_name);
(*write the current environment to a specified file*)
VAR save-srcfil: file_name;
BEGIN
  save_srcfil := srcfil;
  srcfil := '';
  cmdclr;
  popsw(defopts.flags, NIL);
  popsw(defopts.dump_flags, NIL);
  assume(wrpas('.ENV ' || environment),
    '?Unable to write environment to file ' || environment);
  srcfil := save_srcfil
END;

PROCEDURE next_pass;
(*initiate next pass*)
VAR next: file_name;
BEGIN
  lstopts := (lstfil <> '') ANDIF (srcsel ORIF
    ([symbols_option, xref_option, calls_option] * semopts <> []) ORIF
    (glbopts.code_option ANDIF glbopts.banner_option ANDIF
      ([assembly_option, map_option] * semopts <> []));
  IF (relfil = '') AND ((lstfil = '') OR NOT (assembly_option IN semopts))
  THEN glbopts.code_option := FALSE;
  quick := NOT have_optionimizer OR (have_checkout AND
    ((glbopts.quick_option = option_is_on) OR
    ((glbopts.quick_option = option_is_auto) AND
      NOT (optionimize_option IN semopts))));
  IF finish AND (glbopts.code_option OR (glbopts.dump_flags <> NIL))
  THEN BEGIN
    IF quick THEN BEGIN
      IF opts_listing OR (err_count <> 0) THEN next := 'PASLST'
      ELSE next := tmprefix || 'CCG'
    END
    ELSE next := tmprefix || 'SHP'
  END
  ELSE IF opts_listing OR (err_count <> 0) THEN next := 'PASLST'
  ELSE next := 'PASCAL';
  log_record.no_lines := linect;
  log_record.no_incl_lines := inclct;
  log_record.no_errors := err_count - warnings;
  log_record.date_and_time := root_block^.children^.comp_dtime;
  log_record.alloc_strategy := glbopts.alloc_mode;
  log_record.option_debug := glbopts.debug_option;
  log_record.option_check :=
    ([MINIMUM(checklist) .. MAXIMUM(checklist)] * semopts <> []);
  log_record.option_main := (root_block^.children^.kind = program_blk);
  log_record.option_overlay := glbopts.overlay_option;
  log_record.option_source := src_selected;
  log_record.option_special :=
    ([MINIMUM(speciallist) .. MAXIMUM(speciallist)] * semopts <> []);
  log_record.option_terse := glbopts.terse_option;
  log_record.option_trace := (trace_option in semopts);
  log_record.option_xref := glbopts.global_option;
  log_record.lowseg_size := 0;
  log_record.highseg_size := 0;
  log_record.ki10:=glbopts.ki_code_option;
  chain(next)
END;

$PAGE write_environment, main block

VAR start_time: INTEGER;

EXTERNAL FUNCTION fileblock(VAR TEXT): filblock; (*where is this defined??*)

BEGIN
  start_time := RUNTIME;
  unchain;
  assume(opnsrc(glbopts.search_list, '.PAS ' || main_file),
    '?Lost source file.');
  logrcd.file_name := fileblock(INPUT);
  logrcd.run_time := start_time;
  IF glbopts.names_option THEN dpymsg('[Compiling ' || FILENAME(INPUT) || ']');
  root_block^.semantic_opts := glbopts.semantic_opts;
  root_block^.dump_flags := glbopts.dump_flags;
  do_pass_1;
  IF env_compilation AND ((max_severity = 0) OR
    ((max_severity = 1) AND glbopts.finish_option)) AND (relfil <> '')
  THEN write_environment(relfil);
  IF glbopts.statistics_option THEN dpystat('PASANL', start_time);
  logrcd.run_time := RUNTIME - logrcd.run_time
  next_pass
END.
 ,rœ