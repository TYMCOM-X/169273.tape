module pasanl;

(* ****************************************************************************


                   PASCAL   Parse   Tree   Constructor


   LL(1) analysis (top-down, no-backup) package.  Assumes the existence of two
   external routines, "scan", the lexical analyzer, and "generate", the 
   semantic processor.  The LL(1) compiler generator creates three include
   files: "grammar.sym" which defines a scalar type naming all terminal symbols
   and designated nonterminal symbols; "grammar.sem" which defines a scalar
   type giving the semantic operation codes that are passed to generate; and
   "grammar.tab" which contains the parsing machine tables an its associated
   definitions.

   Format of parsing machine tables:

   sym => defines the symbol to be recognized.  If it is a terminal symbol
          (i.e. sym < nonterminal), sym is matched against the symbol class
          of the current token from scan.  If it is a nonterminal symbol, the
          pm "subroutine" defining the nonterminal is called by stacking the
          address of the current pm instruction and continuing execution at
          ntaddr.

   at =>  gives the address at which to continue the parse if the recognition
          operation is successful.  Address less than minpmaddr indicate
          a "return" from a pm subroutine.

   af =>  gives the continuation address if the recoginition operation fails.
          The case af = err_rt causes immediate parse termination unless the
          semantic operation takes error recovery action.

   sg =>  gives the semantic operation to perform if recognition is successful.
          The value "nosemop" implies no operation.

   fg =>  gives the semantic operation to perform if recognition fails. The
          operation may take recovery action on errors via the error routines
          defined below, which alter the flow of pm interpretation so that
          the err_rt action is not taken, or which skip or insert terminal
          symbol.

   Parse tree construction:

   The tree to be constructed consists of nodes and their definition chains.
   A stack of selected, recognized symbols is kept as a linear chain of nodes;
   Substructure is created by the semantic operations.  Typically, they pop
   certain nodes off of the stack and make them the definition of a new node
   which is then pushed on the stack.  Determination of the nodes to pop is
   aided by remembering "marker" nodes at each nonterminal symbol recognition
   call.  For example, the definition of many nonterminals is simply all nodes
   pushed after the recognition call.


   **************************************************************************** *)
$PAGE includes
$include pascal.inc
$include pasist.inc
$include paspt.typ
$include paslex.inc
$include paserr.inc
$include passw.inc


external procedure dmp_ptree ( parse_node ); (* in PA1DMP *)
$PAGE parsing declarations and parsing machine table
$include pasbnf.sem
$include pasbnf.tab
$PAGE parser state information

static var    (* parser state information *)
    ic,                                         (* address of current pminst *)
    nextic: pmaddr;                             (* address of next pminst, changed by recovery routines*)
    savedtok,                                   (* flags next symbol in savedsym due to insert token *)
    needtok: boolean;                           (* flags scan required on next terminal recognition*)
    savedsym,                                   (*contains next symbol saved by inserttoken *)
    tokensym: symbols;                          (* token read by scanner *)
    defnbase,                                   (* dummy node to which we chain everything *)
    lastnode,                   (* last node chained to list *)
    resume_node: parse_node;                    (* new "last node" after return *)

(* Parse stack *)

const maxpmstack = 200;		(* increased from 100  7/24/82 DWS *)
var
  pmstackptr: 0..maxpmstack;
  pmstack: array [1..maxpmstack] of
      record
        retic: pmaddr;                          (* ic of calling instruction *)
        src: source_id;                 (* position in text of construct *)
        col: line_index;
        src_not_set: boolean;                   (* flags whether above is valid *)
        marker: parse_node                      (* last node pushed prior to recognition call *)
      end;



public var ptree: parse_node;
$PAGE initparse, endparse
(*    procedure to initialize syntax analyzer    *)

public procedure initparse;
begin
  needtok:= true;
  nextic := minpmaddr;
  new (defnbase);       (* define dummy node for base of tree *)
  with defnbase^ do begin
    sym := badsymbol;
    dummy := true;
    next := nil;
    defn := nil
  end;
  pmstackptr:= 1;
  pmstack[1].src_not_set := false;                      (* suppress recording of source info in dummy level *)
  pmstack[1].marker := defnbase;
  resume_node := defnbase;
  ptree := nil;
end (*initparse*);


(* procedure to shut down the parser *)

public procedure endparse;
 begin
  dispose (defnbase)
 end;

$PAGE constants
const
  DclDelim: symbol_set :=               (* symbols delimiting declarations *)
        [ labelsy, constsy, typesy, varsy, functionsy, proceduresy,
          externalsy, publicsy, staticsy, beginsy, endsy, exceptionsy   ];

  StmtDelim: symbol_set :=              (* symbols delimiting statements *)
        [ beginsy, ifsy, casesy, repeatsy, whilesy, forsy, withsy,
          loopsy, exitsy, endsy, untilsy, gotosy, semicolon,
          otherssy, eofsy                                           ,
        (*  + dcldelim  *)
          labelsy, constsy, typesy, varsy, functionsy, proceduresy,
          externalsy, publicsy, staticsy, beginsy, endsy, exceptionsy   ];

  ExprDelim: symbol_set :=              (* symbols delimiting expressions *)
        [ addop, relop, mulop, notsy, rparent, rbracket, dosy, tosy,
          downtosy, ofsy, thensy, comma, elipsis, precsy             ,
        (*  + stmtdelim  *)
          beginsy, ifsy, casesy, repeatsy, whilesy, forsy, withsy,
          loopsy, exitsy, endsy, untilsy, gotosy, semicolon,
          otherssy, eofsy                                           ,
        (*  + dcldelim  *)
          labelsy, constsy, typesy, varsy, functionsy, proceduresy,
          externalsy, publicsy, staticsy, beginsy, endsy, exceptionsy   ];
$PAGE parse
(*    the syntactical analyzer    *)

type parseret = (parsecomplete,parsestackoflo,parseerror);

function parse ( var code: parseret): parse_node;
  label
    100,                                        (* instruction decode *)
    200,                                        (* instruction fetch + subr return *)
    300;                                        (* cooroutine return to caller or parse *)

  procedure generate ( action: pmsemop ); forward;
$PAGE error recovery routines

procedure forcetrue;                    (* force at branch of current symbol *)
 begin
  nextic:= pmtable[ic].at;
  needtok := false
 end (*forcenext*);


procedure forcefalse;                   (* force af branch of current symbol *)
 begin
  nextic:= pmtable[ic].af;                      (* doing this on error can have interesting results *)
  needtok:= false
 end (*forcefalse*);


procedure forceloop;                    (* force re-parse of current symbol *)
 begin
  nextic:= ic;
  needtok := false
 end;


procedure inserttoken(insertion: symbols);    (*insert a symbol*)
 begin
  if not savedtok then begin                    (*don't cream previously saved token*)
    savedsym:= tokensym;
    savedtok:= true                             (*save current token and flag it*)
  end;
  tokensym:= insertion;                         (*insert the token*)
  needtok:= false                               (*and make sure the parser doesn't skip it*)
 end;


procedure skip;                         (* skip the current symbol *)
 begin
  if savedtok then begin
    tokensym := savedsym;
    savedtok := false
  end
  else tokensym := scan;
  needtok := false                              (* make sure parser sees this symbol *)
 end;


procedure skipuntil(targets: symbol_set);  (*find desired token(s)*)
 begin
  if tokensym <> eofsy then
    skip; (* always skip at least one symbol *)
  while not (tokensym in targets) do skip;
  needtok:= false                               (*make sure parser sees this symbol*)
 end;
$PAGE syntactic error recovery
procedure err_after (code: err_codes);
 begin
  err_print (code, last_token.source, '', last_token.column + last_token.length)
 end;


procedure err_skip_next (code: err_codes; targets: symbol_set);
 begin
  err_token (code, token);
  skipuntil (targets + [eofsy]);                (* don't skip past EOF ever! *)
  if token.sym = eofsy 
    then forcefalse
    else forcetrue
 end;


procedure err_last_skip_next (code: err_codes; targets: symbol_set);
 begin
  err_after (code);
  skipuntil (targets + [eofsy]);
  if token.sym = eofsy
    then forcefalse
    else forcetrue
 end;


procedure err_insert (code: err_codes; insertion: symbols);
 begin
  err_token (code, token);
  inserttoken (insertion);
  forceloop
 end;


procedure err_last_insert (code: err_codes; insertion: symbols);
 begin
  err_after (code);
  inserttoken (insertion);
  forceloop
 end;


procedure find_loop (targets: symbol_set);
 var saved_token: token_type;
 begin
  saved_token := token;
  skipuntil (targets + StmtDelim + [eofsy]);            (* see if we can find what caller wants *)
  if token.sym in targets                       (* found target, preceding was junk *)
    then err_token (err_bad_clause, saved_token);

  (* Retry failing construct. If a target was found, then parse should continue
     normally having skipped the junk. If a target was not found, then same
     error will occur, but this time at an Ending symbol. *)

  if token.sym = eofsy
    then forcefalse
    else forceloop
 end;


procedure insert_nonterminal ( nt: symbols );
 begin
  generate (attachnt);                          (* collect excess stacked nodes *)
  lastnode^.sym := nt;                          (* may not have right type *)
  lastnode^.dummy := true;
  generate (pmtable[ic].sg);                    (* chain into tree as it should be, if no
                                                   action specified, nonexistent nosemop case
                                                   will be taken. *)
 end;


procedure fake_qualifier ( nt: symbols );
 begin
  generate (attachqual);
  lastnode^.sym := nt;
  lastnode^.dummy := true;
 end;


procedure err_missing_expr (code: err_codes; targets: symbol_set);
 begin
  err_last_skip_next (code, targets);
  insert_nonterminal (exprtree)
 end;
$PAGE generate
procedure generate (* action: pmsemop *) ;

 var node, a, b, op: parse_node;

 begin
  case action of

    (* ATTACH pushes a terminal symbol on the parse tree stack. *)

    attach:
      begin
        new (node);
        node^ := token;                         (* get info from current token *)
        node^.sym := tokensym;                  (* in case of an inserted token *)
        with node^ do begin
          defn := nil;
          next := nil;
        end;
        lastnode^.next := node;                 (* chain to end of defn *)
        lastnode := node;
      end;

    (* ATTACH NT pushes a nonterminal symbol node onto the parse tree stack.
       The definition of the nonterminal is made up of all symbol pushed
       *after* the recognition "call" for the nonterminal.  The action is:
       marker[sp+1] -> ..., new NT => marker[sp+1] -> NT (...).  The parens
       enclose the definition chain. *)

    attachnt:
      begin
        new (node);
        with node^ do begin                     (* construct nonterminal info *)
          sym := pmtable[ic].sym;
          with pmstack[pmstackptr+1] do begin
            dummy := src_not_set;
            source := src;
            column := col;
            defn := marker^.next;
            marker^.next := node;               (* replace the end of the list *)
          end;
          next := nil;
        end;
        lastnode := node;
      end;

    (* ATTACH QUALifier pushes a nonterminal node whose definition is all nodes
       pushed on the parse tree stack after the recognition call of the *superior*
       nonterinal symbols.  This is used to define postfix operations such as
       <array qualifier>.  Action:  marker[sp] -> ..., new NT => NT (...)       *)

    attachqual:
      begin
        new (node);
        with node^ do begin                     (* construct nonterminal info *)
          sym := pmtable[ic].sym;
          with pmstack[pmstackptr] do begin
            dummy := src_not_set;
            source := src;
            column := col;
            defn := marker^.next;
            marker^.next := node;               (* replace the end of the list *)
          end;
          next := nil;
        end;
        lastnode := node;
      end;

    (* INFIX OP is called to construct an operator subtree, using the top three
       nodes on the stack.  It is called upon recognition of the second operand,
       and appends the operands to the operator's definition chain.  The subtree
       thus built is pushed. Action:  marker[sp] -> A -> OP -> B -> ... =>
       marker[sp] -> OP (A -> B -> ...).   Note that this means that there
       may be n right-hand operands, and it is used as such to chain together
       the represenation of subranges with PREC attributes. *)

    infixop:
      begin
        with pmstack[pmstackptr] do begin
          a := marker^.next;                            (* collect nodes *)
          op := a^.next;
          b := op^.next;
          a^.next := b;                                 (* mark operation defn *)
          op^.defn := a;
          marker^.next := op;                           (* replace triple with op *)
          op^.next := nil;
          lastnode := op;                               (* last node on chain *)
        end
      end;

    (* ATTACH OPTion chains a set of suboptions to an option identifier.  This
       action is performed *only* after a recognition call of <option>.  The
       effect is: marker[sp+1] -> a -> ... => marker [sp+1] -> a (...).
       "*Option" is recognized as a special case:
       marker[sp+1] -> * -> a -> ... => marker[sp+1] -> * (a (...)). *)

    attachopt:
      begin
        with pmstack [pmstackptr+1] do begin
          with marker^.next^ do begin
            if sym = starsy then begin          (* push option under star *)
              next^.defn := next^.next;
              next^.next := nil;
            end;
            defn := next;                       (* put suboptions on defn chain *)
            next := nil;
          end;
          lastnode := marker^.next;
        end;
      end;

    (* RETURN TERM attaches a terminal symbol and then returns it as a parse
       tree, all by itself. *)

    returnterm:
      begin
        resume_node := lastnode;
        generate (attach);
        goto 300;
      end;

    (* RETURN NT is creates a nonterminal with it definition and returns the
       parse tree. *)

    returnnt:
      begin
        generate (attachnt);                            (* do it the easy way *)
        resume_node := pmstack[pmstackptr+1].marker;
        goto 300
      end;

    (* RETURN QUAL performs an attachqual and then returns the parse tree. *)

    returnqual:
      begin
        generate (attachqual);
        resume_node := pmstack[pmstackptr].marker;
        goto 300;
      end;

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

    returnsubr:
      begin
        if token.sym = forwardsy then generate (attach);        (* push FORWARD on the stack if present *)
        new (node);
        with node^ do begin
          sym := subr_decl;
          with pmstack[pmstackptr-1] do begin
            dummy := src_not_set;
            source := src;
            column := col;
            defn := marker^.next;
            marker^.next := node;
          end;
          next := nil;
        end;
        lastnode := node;
        resume_node := pmstack[pmstackptr-1].marker;
        goto 300;
      end;

    (************* Syntactic Error Recovery Actions **************)

    (* EOF not found after end of program *)

    extratext:
      begin
        error (err_following_text);                     (* halt compilation *)
        stop
      end;

    (* program name not found in PROGRAM statement, just continue *)

    progidexp:
      err_last_skip_next (err_prog_id_exp, StmtDelim + DclDelim);

    (* semicolon not found when expected *)

    semiexp:
      err_last_insert (err_semi_expected, semicolon);

    (* body of main program not found *)

    progexp:
      begin
        if token.sym = eofsy
          then err_token (err_prog_exp, last_token)     (* no body at all *)
          else err_insert (err_no_begin, beginsy);      (* assume he forgot begin *)
      end;

    (* no dot terminating program *)

    dotexp:
      begin
        err_t(err_dot_exp, last_token);
        skipuntil ([eofsy]);                    (* if not semicolon instead, it is a bizarre error *)
        forcetrue                               (* so skip semi or scan 'til end *)
      end;

    (* Missing declaration following VAR, CONST, etc. *)

    declexp:
      begin
	err_after (err_no_declaration);
	forcetrue;
      end;

    (* Missing declaration following PUBLIC, EXTERNAL, etc. *)

    declrequired:
      begin
        err_last_skip_next (err_no_declaration, DclDelim);
        fake_qualifier (declaration);
      end;

    (* Missing label id, <intconst>, in LABEL declaration. *)

    labelidexp:
      err_skip_next (err_no_label, [comma, semicolon] + DclDelim);

    (* <declarations> ::= <declaration> production fails. If this is the end
       of a list of declarations, then everything is okay. Otherwise, it is
       likely that the user has blown a declaration -- if we allow the 
       production to fail, we will skip all subsequent declarations until we
       see the body of the procedure or program; so try to recover at the start
       of the next declaration. *)

    nodecl:
      begin
        if not (token.sym in [beginsy, period, endsy, eofsy]) then
          err_skip_next (err_no_declaration, DclDelim)
      end;

    (* Semicolon not found between declarations.  If this is the last declaration,
       i.e. "." or END (in the case of a data module) follows, then this is okay.
       Otherwise, there is an error, and we must try to recover. *)

    noending:
      begin
        if not (token.sym in [period, endsy]) then
          err_last_insert (err_semi_expected, semicolon);
      end;

    (* Missing type declaration in VAR, CONST, parameter, or return value
       declarations. *)

    typeexp:
      begin
        err_last_skip_next (err_no_type_decl, [becomes, relop (* = *), semicolon, rparent] + DclDelim);
        insert_nonterminal (type_decl);
      end;

    (* Missing expression giving value of a constant *)

    initvalexp:
      err_missing_expr (err_no_const_val, [semicolon] + DclDelim);

    (* Missing colon in declarations, case labels, statement labels, on prefixes.
       Often an equals will be used in its place in a declaration; so if we find
       an '=' we skip to the next token and continue.  Otherwise, the most likely
       case is that it has been forgotten. *)

    colonexp:
      begin
        if (token.sym = relop) andif (token.op = eqop)
          then begin
            err_token (err_colon_exp, token);   (* mark error at the '=' *)
            skip; forcetrue                     (* continue with following token *)
          end
          else err_last_insert (err_colon_exp, colon)
      end;

    (* Missing identifier after PROCEDURE or FUNCTION. *)

     subrnameexp:
      begin
         err_last_skip_next (err_subr_id_exp, [lparent, colon, semicolon] + StmtDelim + DclDelim);
        generate (attach);                      (* attach dummy id node with nil name *)
        lastnode^.dummy := true;
        lastnode^.sym := ident;
        lastnode^.name := nil
      end;

    (* Missing subroutine body. This is an error if the subroutine is not
       external. The forward case is caught by an explicit production. *)

    subrbodyexp:
      begin
        err_token (err_subr_body_exp, last_token);              (* as body is optional, parse will continue *)
        insert_nonterminal (null_stmt);
        resume_node := pmstack[pmstackptr+1].marker;
        goto 300;
      end;

    (* Missing parameter declaration within a parameter list *)

    parmdeclexp:
      err_skip_next (err_parm_decl_exp, [semicolon, rparent, colon] + DclDelim);

    (* Missing identifier in identifier list, scalar type declaration, on
       statement, or variant part declaration. *)

    idexp:
       err_last_skip_next (err_id_expected,
                           [comma, rparent, colon, semicolon, ofsy, dosy] + DclDelim);

    (* <relop> found when '=' is required. Check if the operator is in fact
       equals, and if so, continue. Otherwise, force a false return from the
       <equals> ::= <relop> production. *)

    chkeq:
      begin
        if token.op <> eqop then forcefalse
      end;

    (* <mulop> found when '*' expected. Process as for chkeq above. *)

    chkstar:
      begin
        if token.op <> mul then forcefalse
      end;

    (* <addop> found when <sign> expected. Check that op is '+' or '-' *)

    chksign:
      begin
        if not (token.op in [plus, minus]) then forcefalse
        else generate (attach)
      end;

    (* Equals expected. A colon is often used in its place in declarations;
       if a colon is found, warn programmer and continue. Otherwise, guess
       that it has been omitted. *)

    eqexp:
      begin
        if token.sym = colon
          then begin
            err_token (err_eq_expected, token); (* mark error at the ':' *)
            skip; forcetrue                     (* continue with next token *)
          end
          else begin                            (* would normally insert, but eq is a relop *)
            err_token (err_eq_expected, last_token);
            forcetrue
          end
      end;

    (* Left bracket expected *)
    lbracketexp:
      err_insert (err_lbracket_exp, lbracket);

    (* Index type specification missing within an array declaration *)

    idxtypeexp:
      begin
        err_skip_next (err_no_idx_type, [comma, rbracket] + DclDelim);
        insert_nonterminal (type_decl);
      end;

    (* missing OF in record case clause or case statement *)

    ofexp:
      err_last_insert (err_of_expected, ofsy);

    (* No semicolon ending a field declaration has been seen. This is acceptable
       if the declaration is the last in the list. *)

    nofieldsemi:
      begin
        if not (token.sym in [endsy, rparent])
          then err_last_insert (err_semi_expected, semicolon)
      end;                                      (* resume parse on AT branch *)

    (* At least one variant not found within record case clause. *)

    novariant:
      err_skip_next (err_no_variant, [endsy] + DclDelim);

    (* No case label found. If at the END of the case, this is okay.  Otherwise
       assume that the label has been fouled up or forgotten altogether, and
       try to recover to parse the member statement. *)

    nocaselab:
      begin
        if not (token.sym in [endsy, rparent]) then begin       (* used in record variants too *)
          if token.sym in ([colon, lparent] + StmtDelim)
            then err_token (err_case_label_exp, token)  (* no apparent label *)
            else begin                                  (* garbage *)
              err_token (err_bad_case_label, token);
              skipuntil ([colon] + StmtDelim)           (* try to get something recognizable *)
            end;
          if token.sym <> colon then inserttoken (colon);       (* so we can continue *)
          forcetrue                                     (* continue parse with ':' *)
        end
      end;

    (* Left parenthesis expected. *)

    lparenexp:
      err_insert (err_lparent_exp, lparent);


    (* No statement following ";" (or another statement) in a statement list.
       If not a valid delimiter, assume that we have trash for a statement and
       skip until the next statement can be found. *)

    nostatement:
      begin
        if not (token.sym in [endsy, untilsy, exceptionsy, semicolon] + dcldelim) then
          err_skip_next (err_bad_stmt, StmtDelim)
      end;

    (* No semicolon found after a statement in a statement list. If this is the
       end of the list, okay; otherwise, error. *)

    nosemi:
      begin
        if not (token.sym in [endsy, untilsy, exceptionsy]) then begin
          if ((last_token.source.file_no = token.source.file_no)
             and (last_token.source.page_no = token.source.page_no)
             and (last_token.source.line_no = token.source.line_no))
		and not (token.sym in StmtDelim)
            then begin                          (* assume both part of malformed statement *)
              err_token (err_stmt_end, token);
              skipuntil (StmtDelim);            (* look for real end *)
              forceloop
            end
            else err_last_insert (err_semi_expected, semicolon)
        end                                     (* assume statement break *)
      end;

    (* no right hand side of assignment statement *)

    norhs:
      err_missing_expr (err_no_rhs, StmtDelim);

    (* not even one handler found in handler clause *)

    nohandler:
      err_skip_next (err_no_handler, StmtDelim);

    (* no predicate expression following IF or EXIT IF *)

    noifpred:
      err_missing_expr (err_no_if_pred, [thensy, elsesy, dosy] + StmtDelim);

    (* THEN not found in IF statement *)

    thenexp:
      err_last_insert (err_then_exp, thensy);

    (* ELSE not found after IF-THEN, check for semicolon-ELSE *)

    chksemielse:
      if token.sym = semicolon then begin
        skip; (* see what comes next *)
        if token.sym = elsesy then begin
          err_token (err_semi_before_else, last_token);
          forceloop; (* ignore the semicolon *)
        end
        else
          inserttoken (semicolon); (* restore the semicolon *)
      end;

    (* FOR index missing *)

    foridxexp:
      err_skip_next (err_for_idx_exp, [becomes, tosy, downtosy, dosy] + StmtDelim);

    (* ':=' missing from FOR statement *)

    becomesexp:
      begin
        if token.sym in ([tosy, downtosy, dosy] + StmtDelim)
          then err_skip_next (err_for_init_exp, [tosy, downtosy, dosy] + StmtDelim)
          else find_loop ([becomes, tosy, downtosy, dosy])
      end;

    (* initialization expression missing in FOR *)

    forinitexp:
      err_missing_expr (err_for_init_exp, [tosy, downtosy, dosy] + StmtDelim);

    (* TO or DOWNTO not found in FOR statement *)

    forwayexp:
      begin
        if token.sym in ([dosy] + StmtDelim)
          then err_skip_next (err_for_limit_exp, [dosy] + StmtDelim)
          else find_loop ([tosy, downtosy, dosy])
      end;

    (* missing terminal expression in FOR statement *)

    forfinexp:
      err_missing_expr (err_for_limit_exp, [dosy] + stmtdelim);

    (* missing DO in FOR or WHILE statement *)

    doexp:
      begin
        if token.sym in StmtDelim
          then err_insert (err_do_exp, dosy)
          else find_loop ([dosy])
      end;

    (* missing UNTIL clause at end of REPEAT loop *)

    untilexp:
      err_insert (err_until_exp, untilsy);

    (* missing predicate in UNTIL clause *)

    untilpredexp:
      err_missing_expr (err_no_until_pred, StmtDelim);

    (* missing predicate after WHILE *)

    whilepredexp:
      err_missing_expr (err_no_while_pred, [dosy] + StmtDelim);

    (* At least one case not found in case statement *)

    nocase:
      err_skip_next (err_no_case, StmtDelim);

    (* Missing statement after case label.  It is acceptable that there be no
       statement.  However, here we check that in fact we have a null statement
       instead of garbage.  In the former case, we return allowing the parser
       to continue because a <statement> is optional.  In the latter, we skip
       until we see something reasonable. *)

    stmtincaseexp:
      begin
        if not (token.sym in [endsy, semicolon]) then
          err_skip_next (err_no_stmt, StmtDelim);
        generate (returnnt);
      end;

    (* There is no semicolon delimiting member statement in a CASE. This is
       in error if the statement is not the last one in the case. *)

    nocasedelim:
      begin
        if token.sym <> endsy
          then err_last_insert (err_semi_expected, semicolon)
      end;

    (* No reference after WITH *)

    nowithref:
      err_last_skip_next (err_no_with_ref, [dosy, comma] + StmtDelim);

    (* No label after GOTO. *)

    labelexp:
      err_last_skip_next (err_no_target_label, StmtDelim);

    (* END statement expected *)

    endstmtexp:
      err_insert (err_end_exp, endsy);

    (* missing IF in EXIT IF clause *)

    ifexp:
      err_insert (err_if_exp, ifsy);

    (* missing operand to +, *, AND, NOT, etc. *)

    operandexp:
      err_missing_expr (err_no_operand, ExprDelim + StmtDelim);

    (* Missing expression after "(", ",", BY, etc. *)

    expressionexp:
      err_missing_expr (err_no_expression, ExprDelim + StmtDelim);

    (* right parenthesis expected *)

    rparenexp:
      err_insert (err_no_rparent, rparent);

    (* right bracket expected *)

    rbracketexp:
      err_insert (err_no_rbracket, rbracket);

    (* missing <range> or <rangelist> *)

    rangeexp:
      err_skip_next (err_range_exp, [comma, colon, rbracket, elipsis] + StmtDelim);

    (* missing upper bound after '..' *)

    upbexp:
      err_missing_expr (err_upb_exp, [comma, rbracket, elipsis, colon] + StmtDelim);

    (* No field identifier after "." *)

    fieldexp:
      err_last_skip_next (err_no_field, ExprDelim + StmtDelim);

    (* Have an '=' instead of ':='. Just give a warning. *)

    eqinstead:
      err_token (err_eq_instead, token)         (* BNF is such that parse continues normally *)

  end (* case action *) ;
 end;
$PAGE parse mainline
begin
  (* init tree pointers to build new construct *)

  lastnode := resume_node;
  lastnode^.next := nil;        (* don't hang on to (nonexistent) old tree *)

  (* continue with indicated next pm instruction *)

200:
  if nextic >= minpmaddr then begin             (* pm goto *)
    ic:= nextic;
    goto 100                                    (* back to decode *)
  end;

  (* some kind of pm subroutine return *)

  ic := pmstack[pmstackptr].retic;
  pmstackptr:= pmstackptr-1;                    (* back to call instruction *)

  (* take at or af of this (calling) instruction according to return *)

  if pmstackptr <= 0 then begin                 (* back to head of parse *)
    if nextic <> succrt then begin              (* error not recovered from *)
      parse := nil;
      code:= parseerror;
      return                                    (* stop the parse *)
    end
    else begin                                  (* return normally *)
      parse := nil;
      code:= parsecomplete;
      return
    end
  end
  else with pmtable[ic] do begin                (* some kind of return *)
    if nextic = succrt then begin
      nextic:= at;
      if sg<>nosemop then generate(sg)
    end
    else if nextic = failrt then begin
      nextic:= af;
      if fg<>nosemop then generate(fg)
    end
    else begin  (* nextic = err_rt *)
      code:= parseerror;
      return
    end;
    goto 200                                    (* decide where to go next *)
  end;                                          (* some kind of return *)

  (*  decode current pm instruction op code  *)

100:
  with pmtable[ic] do begin
    if sym < nonterminal then begin
      if needtok then begin                     (*must scan new symbol*)
        if savedtok then begin
          tokensym := savedsym;
          savedtok := false
        end
        else tokensym := scan;
        needtok:= false
      end;
      if tokensym = sym then begin              (*recognition*)
        nextic:= at;
        needtok:= true;                         (*symbol has been recognized*)
        if sg<>nosemop then generate(sg);
      end
      else begin                                (*no recognition*)
        nextic:= af;
        if fg<>nosemop then generate(fg)        (*may change nextic*)
      end;
      goto 200                                  (* go see what to do next *)
    end
    else begin                                  (* non-terminal symbol -- perform pm subroutine call *)
      if pmstackptr < maxpmstack then
        pmstackptr:= pmstackptr+1
      else begin                                (* stack overflow *)
        code:= parsestackoflo;
        parse := nil;
        return
      end;
      with pmstack[pmstackptr] do begin
        retic := ic;
        marker := lastnode;
        if needtok then begin                   (* get next token *)
          if savedtok then begin
            tokensym := savedsym;
            savedtok := false
          end
          else tokensym := scan;
          needtok := false
        end;
        src := token.source;
        col := token.column;
        src_not_set := token.dummy;
      end;
      ic:= ntaddr;
      goto 100                                  (* go decode opcode *)
    end
  end;                                          (* decoding *)

  (*  Return current parse tree to caller.  Assumes that this is done only
      on an SG call, with nextic properly set. *)

300:
  parse := lastnode;
  code := parsecomplete;
end (*parse*);
$PAGE del_ptree
(* DEL PTREE disposes of a specified parse tree. *)

public procedure del_ptree ( pt: parse_node );
 var cur, next: parse_node;
 begin
  if pt <> nil then begin
    cur := pt^.defn;
    while cur <> nil do begin
      next := cur^.next;
      del_ptree (cur);
      cur := next;
    end;
    dispose (pt);
  end;
 end (* del_ptree *);
$PAGE get_ptree
(* GET PTREE will dispose of any existing existing parse tree, and will then
   call the parser to get a new parse tree.  Any parser error will be reported
   here.  Any parse subtree which is to be kept across a call to this routine
   should be cut out of the current parse tree, to keep it from being deleted.
   The root of the fetched parse tree will be in the global variable PTREE. *)

public procedure get_ptree;
 var perr: parseret;
 begin
  if ptree <> nil then
    del_ptree (ptree);
  ptree := parse (perr);
  case perr of
    parsestackoflo: error (err_parse_overflow);
    parseerror:     error (err_pr_error);
    parsecomplete:
  end;
  if switch (cur_block^.dump_switches, 'PT') then
    dmp_ptree (ptree);
 end (* get_ptree *).
    f k