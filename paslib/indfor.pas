$OPTIONS SPECIAL
module INDFOR;
$LENGTH 44
$TITLE INDFOR Formatting module for INDENT
(*$Y20
$HEADER INDFOR.HDR
*)
$PAGE GLOBALS and other declarations

(*$X10
$OPTIONS NOSOURCE
*)
$INCLUDE INDENT.TYP
(*$X10
$OPTIONS SOURCE
*)

(*$X10
$OPTIONS NOSOURCE
*)
$INCLUDE INDFOR.INC
(*$X10
$OPTIONS SOURCE
*)

public var
  TOP_FLAG: STK_FLAG := ONE_FLAG;	(* set by INDSTK, last operated on *)
  SAFIRST,				(* pointers to list of standalone *)
  SALAST: SAPTR;			(* comments for WRITER *)

var
  PRO_LEVEL,				(* current lexical level of proced. *)
  PREV_IND: LINE_IDX;			(* retains rel. ind. reference col *)
  FIX_NEXT,
  FIRST_ON_LINE: boolean;		(* FOR_COM sets, used for rel. ind. *)

const
  SECKEYWDS: TOKSET :=
    [BEGINSY, TYPESY, CONSTSY, VARSY, LABELSY, PUBLICSY, EXTERNALSY,
     PROCSY, FUNCTSY, INITPROCSY, STATICSY];

  NONSTMT: TOKSET :=			(* simple statememt terminators *)
    [BEGINSY, LOOPSY, REPEATSY, SEMICOLONSY, FORSY, WHILESY, ENDSY,
     WITHSY, DOSY, IFSY, THENSY, ELSESY, CASESY, EXITSY, UNTILSY];
$PAGE NON_BLANK	define for_com and helper

procedure FOR_COM;

(* FOR_COM is the bottom of the chain of routines used in INDFOR to
   prepare text for WRITER, and performs the actual 'resume' to WRITER.
   FOR_COM will not return to its caller until THIS_TOKEN is not a
   comment-like token (OPEN_COM, CONT_COM, CLOSE_COM, SA_COM, DIRECTIVE).
   Thus, the rest of the FORMAT routines see a comment only as NEXT_
   TOKEN. Relative indentation is completely preserved in stand-alone
   comments; multi-line imbedded comments are force-aligned to the
   first non-blank character of the first line of the comment; 
   compiler directives are always placed in column one. *)

  var
    OFFSET: LINE_IDX;			(* local for FOR_COM *)
    SATMP: SAPTR;			(* temp for making SARECs *)



  function NON_BLANK: LINE_IDX;

  (* NON_BLANK helps FOR_COM determine the indentation of continuation
     lines of multi-line imbedded comments, by finding the first non-
     blank character in the comment. If there are none, then NON_BLANK
     returns QUANTUM. *)
    
    begin
    if CUREND - CURSTART = 1 then
      NON_BLANK := QUANTUM		(* only two characters, no start *)
    else begin				(* find non_blank *)
      NON_BLANK := 2;
      while LINE[CURSTART + NON_BLANK] = ' ' do
	NON_BLANK := NON_BLANK + 1
      end
    end (* function NON_BLANK *);
$PAGE FOR_COM 	jams comments through format
  begin					(* code for FOR_COM *)
    loop				(* no exit, will do return within *)
      COM_IND := OUT_IND;		(* except if directive coming *)
      if FIRST_ON_LINE then
	LIN_INDENT := OUT_IND;		(* save indent for this line *)
      FIRST_ON_LINE := (EOL_CT > 0);	(* see if next token is first *)
      if not (THIS_TOKEN in [SA_COM,DIRECTIVE]) then
	resume (WRITER);		(* write out current anyway *)

      case THIS_TOKEN of

	DIRECTIVE: begin
	  OUT_IND := 0;
	  COM_IND := CUR_IND;		(* comment before directive *)
	  resume (WRITER);
	  if not (THIS_TOKEN in [SA_COM,DIRECTIVE]) then return
	  end (* DIRECTIVE *);

	OPEN_COM: 
	  OFFSET := TOK_IND + NON_BLANK + LIN_INDENT;

	CONT_COM, CLOSE_COM:
	  OUT_IND := OFFSET;

	SA_COM: begin
	  OFFSET := ORIG_IND;		(* remember first, get others relative *)

	  repeat
	    new (SATMP: (CUREND - CURSTART + 1));
	    if SALAST = nil then
	      SAFIRST := SATMP		(* get link to new record *)
	    else SALAST^.SANEXT := SATMP;
	    SALAST := SATMP;		(* and make it new last one *)

	    with SALAST^ do		(* initialize it *)
	      begin
	      SALEN := CUREND - CURSTART + 1;
	      SATEXT[1: SALEN] := substr (LINE, CURSTART, SALEN);
	      SAEOL := EOL_CT;
	      SANEXT := nil;
	      SAOFFSET := ORIG_IND - OFFSET (* offset relative to first line *)
	      end;

	    resume (READER)		(* get another token *)
	  until THIS_TOKEN <> SA_COM;
	if THIS_TOKEN <> DIRECTIVE then return
	end (* SA_COM *);

	others: return
      end (* case *)
    end 
  end (* procedure FOR_COM *);
$PAGE FOR_REL two routines to select final token indentation
procedure FOR_NO_REL;

(* FOR_NO_REL resumes WRITER with the current token through FOR_COM, 
   enforcing the current indentation CUR_IND. In addition, it marks the
   current line as a reference for any subsequent relative indentation
   (see FOR_SAV_REL below). The reference indentation is recorded in
   PREV_IND as the difference of input and output indentation of the
   current line. *)

  begin
  OUT_IND := CUR_IND;
  FIX_NEXT := true;
  if FIRST_ON_LINE then
    PREV_IND := OUT_IND			(* get outind of line containing *)
  else PREV_IND := LIN_INDENT;		(* the reference token *)
  PREV_IND := PREV_IND - ORIG_IND;	(* get diff. between inp&out *)
  FOR_COM
  end (* procedure FOR_NO_REL *);



procedure FOR_SAV_REL;

(* FOR_SAV_REL also resumes WRITER through FOR_COM like FOR_NO_REL, but
   preserves relative indentation of lines while enforcing the minimum.
   Since we know that OUT_IND is used by WRITER only for the first token
   on a line, we can ignore TOK_IND, for it is zero. Every call to 
   FOR_NO_REL above sets PREV_IND to be the number of columns that the
   current line was shifted to the right. To indent relative to the last
   FOR_NO_REL call, merely add this offset to the original indentation 
   of the line. *)

  begin
  if FIRST_ON_LINE then begin
    if FIX_NEXT then begin
      if CUR_IND > ORIG_IND + PREV_IND then
	PREV_IND := CUR_IND - ORIG_IND;
      FIX_NEXT := false
      end;
    OUT_IND := max (CUR_IND, ORIG_IND + PREV_IND)
    end;
  FOR_COM
  end (* procedure FOR_SAV_REL *);
$PAGE GO_PAST helper for semantic formatting routines

procedure GO_PAST( SKIP_TOKEN: TOKENTYPE;
		   FORMAL: COROUTINE);

(* GO_PAST calls the formal parameter routine (either FOR_NO_REL
   or FOR_SAV_REL) until it sees the desired tokentype SKIP_TOKEN,
   and then performs one more formal call. THIS_TOKEN is thus set
   to the token following the SKIP_TOKEN. *)

  begin
  while THIS_TOKEN <> SKIP_TOKEN do
    FORMAL;
  FORMAL
  end (* procedure GO_PAST *);
$PAGE FOR_TYPE formats a general type statement
procedure FOR_REC_BODY; forward;	(* read below *)
procedure FOR_PRO_DECL; forward;

procedure FOR_TYPE;

(* FOR_TYPE performs indentation for a type statement, i.e. what one
   would find on the right hand sides of (1) equal sign in type declar-
   ations; (2) colon in var and const declarations, and procedure
   headings. In the case of a record definition, FOR_TYPS is
   mutually recursive with FOR_REC_BODY in the following manner:
   FOR_TYPE eats the RECORD token, and then calls FOR_REC_BODY to 
   indent the rest of the definition; FOR_REC_BODY calls FOR_TYPE
   for the type statement in each field declaration. 
   In a similar fashion, FOR_TYPE can call FOR_PRO_DECL to format
   a procedure header in a formal parameter definition; if a
   procedure header contains parameters, their types will be formatted
   by FOR_TYPE. *)

  var
    ORIGL,				(* for saving calling ind *)
    KWL: LINE_IDX;			(* and 'record' keyword ind *)

  begin
  ORIGL := CUR_IND;			(* save current level on stack *)
    loop

      case THIS_TOKEN of

	OPENPARENSY:
	  begin
	  FOR_SAV_REL;			(* write paren, keep rel. *)
	  GO_PAST(CLOSEPARENSY,FOR_SAV_REL)
	  end (* OPENPARENSY *);

	ENDSY,
	ASSIGNSY,			(* may be var or typed const *)
	CLOSEPARENSY,
	SEMICOLONSY: return;		(* let caller deal with final token *)

	RECORDSY:
	  begin
	  if 3 in OPTIONS then		(* get offset from current *)
	    KWL := TOK_IND		(* to apply after writing *)
	  else KWL := 0;		(* the record word *)
	  FOR_NO_REL;
	  CUR_IND := LIN_INDENT + KWL + QUANTUM; (* indent a quantum
					   after line containing rec *)
	  KWL := CUR_IND - QUANTUM;	(* save quantum less for end *)
	  FOR_REC_BODY;			(* process record body *)

	  while not (THIS_TOKEN in SECKEYWDS + [ENDSY]) do
	    FOR_NO_REL;			(* safely get rid of extra stuff *)
	  CUR_IND := KWL;		(* restore ind. to 'keyword' level *)

	  if THIS_TOKEN = ENDSY then
	    FOR_NO_REL;			(* got the end token *)

	  CUR_IND := ORIGL		(* replace original indentation *)
	  end (* RECORDSY *);

	PROCSY,
	FUNCTSY: FOR_PRO_DECL;		(* routine type definition *)

	others: FOR_SAV_REL

      end (* case *)
    end (* loop *)
  end (* procedure FOR_TYPE *);
$PAGE FOR_REC_BODY formats body of a record declaration

procedure FOR_REC_BODY;

(* FOR_REC_BODY formats the body of a record declaration, i.e. everything
   after the keyword 'record'. Since this 'body' is the same structure
   that follows the open-paren of a variant part definition, FOR_REC_BODY
   will call itself after the said open-paren. Also, FOR_REC_BODY will
   call FOR_TYPE for formatting everything after a colon, just like a
   var declaration. *)

  var
    ORIGL,				(* save caller's indent *)
    LASTVAR: LINE_IDX;			(* and ind. of variant labels *)

  begin
  ORIGL := CUR_IND;			(* save indent now *)
  while not (THIS_TOKEN in SECKEYWDS + [ENDSY,CLOSEPARENSY,CASESY]) do
    begin				(* format fixed part *)
    GO_PAST(COLONSY,FOR_NO_REL);	(* get past colon *)
    CUR_IND := CUR_IND + QUANTUM;	(* tick, this is cont. line *)
    FOR_TYPE;				(* get type statement *)
    CUR_IND := ORIGL;			(* restore *)
    if THIS_TOKEN = SEMICOLONSY then
      FOR_NO_REL			(* if ')' or 'end', no sem *)
    end (* while fixed part *);

  if THIS_TOKEN = CASESY then
    begin				(* format variant part *)
    GO_PAST(OFSY,FOR_NO_REL);		(* case header, est. rel. ind. *)
    CUR_IND := CUR_IND + QUANTUM;	(* variant labels go in a quantum *)
    LASTVAR := CUR_IND;			(* from the fixed part *)
      repeat				(* for each variant *)
	GO_PAST(COLONSY, FOR_NO_REL);	(* eat colon after variant selector *)
	CUR_IND := CUR_IND + QUANTUM;	(* tick a quantum after colon *)

	GO_PAST(OPENPARENSY, FOR_NO_REL); (* skip junk if any *)
	if not FIRST_ON_LINE then	(* est. min. at first decl. *)
	  CUR_IND := LIN_INDENT + TOK_IND; (* unless first on line *)

	FOR_REC_BODY;			(* recurse, go get it! *)
	GO_PAST(CLOSEPARENSY,FOR_NO_REL); (* match the parens *)

	if THIS_TOKEN = SEMICOLONSY then
	  FOR_NO_REL;			(* eat sems if any *)
	CUR_IND := LASTVAR		(* back to level of last variant *)
      until THIS_TOKEN in SECKEYWDS + [ENDSY,CLOSEPARENSY];

      (* Either ')' or 'end' will terminate a record body, depending
         upon the variant part nesting. Could also be a record def.
	 within a variant part, for that matter! *)

    CUR_IND := ORIGL			(* back to pre-variant ind. lev. *)
    end (* variant part *)
  end (* procedure FOR_REC_BODY *);
$PAGE FOR_TYPE_DECL format a type declaration section

procedure FOR_TYPE_DECL;

(* FOR_TYPE DECL processes a section beginning with the keyword
   'type', and continues to process declarations until another
   section keyword is seen. *)

  var
    ORIGL: LINE_IDX;			(* to save ind on stack *)

  begin
  FOR_NO_REL;				(* jam 'type' thru, est. rel. *)
  ORIGL := CUR_IND;			(* save caller's indeet. *)
  if FIRST_ON_LINE then
    CUR_IND := CUR_IND + QUANTUM	(* tick a q if new line *)
  else CUR_IND := LIN_INDENT + TOK_IND; (* else line up to first decl *)

  while not (THIS_TOKEN in SECKEYWDS) do
    begin				(* do a type decl. *)
    FOR_NO_REL;				(* put ident., est. rel. ind. *)
    CUR_IND := CUR_IND + QUANTUM;	(* tick q for contin. lines *)
    GO_PAST(EQUALSY,FOR_SAV_REL);	(* get past equals, preserve *)
    FOR_TYPE;				(* do the definition *)
    CUR_IND := CUR_IND - QUANTUM;	(* ready for next one *)
    FOR_SAV_REL;			(* dump the semicolon *)
    end;

  CUR_IND := ORIGL			(* restore original indent *)
  end (* procedure FOR_TYPE_DECL *);
$PAGE FOR_VAR_DECL format a var declaration section

procedure FOR_VAR_DECL;

(* FOR_VAR_DECL deals with a var declaration section in much the same
   manner as FOR_TYPE_DECL deals with type declarations. Initialization
   of the var is not formatted, but relative indentation is preserved. *)

  var
    ORIGL: LINE_IDX;			(* for stacking indentation *)

  begin
  FOR_NO_REL;				(* get rid of var *)
  ORIGL := CUR_IND;			(* save current ind for restore later *)
  if FIRST_ON_LINE then
    CUR_IND := CUR_IND + QUANTUM
  else CUR_IND := LIN_INDENT + TOK_IND; (* get ind for rest of section *)

  while not (THIS_TOKEN in SECKEYWDS) do
    begin				(* get a decl. *)
    FOR_NO_REL;				(* put ident., estab. rel. ind. *)
    while THIS_TOKEN <> COLONSY do	(* id list goes on min ind. *)
      FOR_NO_REL;			(* for sect. -- continuation
					   lines only for type stmt *)
    CUR_IND := CUR_IND + QUANTUM;	(* and the colon too *)
    FOR_SAV_REL;			(* put colon--cont. lines will
					   now be indented a q *)
    FOR_TYPE;				(* format its type *)

    while not (THIS_TOKEN in SECKEYWDS + [SEMICOLONSY]) do
      FOR_SAV_REL;			(* get to the semi *)
    CUR_IND := CUR_IND - QUANTUM;	(* ready for next decl *)
    if THIS_TOKEN = SEMICOLONSY then
      FOR_NO_REL			(* write out semi if there *)
    end (* while *);

  CUR_IND := ORIGL
  end (* procedure FOR_VAR_DECL *);
$PAGE FOR_CONST format const section

procedure FOR_CONST;

(* FOR_CONST is a lot like FOR_VAR_DECL, except that const decls. don't
   necessarily have to have a type associated with them. *)

  var
    ORIGL: LINE_IDX;			(* for stacking indentation *)

  begin
  FOR_NO_REL;				(* eat const *)
  ORIGL := CUR_IND;			(* save current for later *)
  if FIRST_ON_LINE then
    CUR_IND := CUR_IND + QUANTUM
  else CUR_IND := LIN_INDENT + TOK_IND;

  while not (THIS_TOKEN in SECKEYWDS) do
    begin
    FOR_NO_REL;				(* put ident, est. rel. ind. *)
    CUR_IND := CUR_IND + QUANTUM;	(* contin. lines indent a q *)
    while not (THIS_TOKEN in SECKEYWDS +
	       [COLONSY,ASSIGNSY,EQUALSY,SEMICOLONSY]) do
      FOR_NO_REL;			(* get to type or value(if there) *)

    if THIS_TOKEN = COLONSY then
      begin
      FOR_SAV_REL;			(* a colon --> typed const *)
      FOR_TYPE				(* get the type *)
      end (* typed const *);

    if THIS_TOKEN <> SEMICOLONSY then
      FOR_SAV_REL;			(* put out equals or assign if one *)
    while not (THIS_TOKEN in SECKEYWDS + [SEMICOLONSY]) do
      FOR_SAV_REL;			(* get to semi *)
    CUR_IND := CUR_IND - QUANTUM;	(* end -- no longer a cont. line *)
    if THIS_TOKEN = SEMICOLONSY then
      FOR_SAV_REL;			(* if semi, put it out *)
    end (* const section *);

  CUR_IND := ORIGL			(* restore to before section *)
  end (* procedure FOR_CONST *);
$PAGE FOR_LABEL trivial routine to format label decls

procedure FOR_LABEL;

(* FOR_LABEL works in the same spirit as the other declaration
   routines. Since label sections are always terminated by the
   first semicolon, this routine is trivial. *)

  var
    ORIGL: LINE_IDX;

  begin
  FOR_NO_REL;				(* write out label token *)
  ORIGL := CUR_IND;			(* save current indent *)
  if FIRST_ON_LINE then
    CUR_IND := CUR_IND + QUANTUM
  else CUR_IND := LIN_INDENT + TOK_IND;

  while not (THIS_TOKEN in SECKEYWDS + [SEMICOLONSY]) do
    FOR_SAV_REL;			(* get up to the semi *)

  CUR_IND := ORIGL;			(* and restore original indent *)
  FOR_SAV_REL				(* put out the semi *)
  end (* procedure FOR_LABEL *);
$PAGE FOR_PRO_DECL formats procedure/function headers

procedure FOR_PRO_DECL;

(* FOR_PRO_DECL formats procedure and/or function headings both as actual
   declarations and as formal types. Since FOR_TYPE can call FOR_PRO_DECL
   to format a formal type, it is possible to format such monsters as:
       procedure FOO(FORMAL:procedure(procedure();var char);var CH: char);
   even though the compiler won't take it. *)

  var
    FUNCT: boolean;			(* to remember if doing function *)
    ORIGL: LINE_IDX;			(* to remember former indent *)

  begin
  FUNCT := (THIS_TOKEN = FUNCTSY);	(* remember if function for later *)
  FOR_NO_REL;				(* put proc/funct keyword *)
  ORIGL := CUR_IND;			(* save original *)
  CUR_IND := LIN_INDENT + QUANTUM;	(* opt.(4)--one q beyond proc line *)
  if not (THIS_TOKEN in [OPENPARENSY, SEMICOLONSY]) then
    FOR_NO_REL;				(* put name if any *)

  if THIS_TOKEN = OPENPARENSY then
    begin				(* get a (null) parameter list *)
    if not (4 in OPTIONS) then		(* must line to first par *)
      begin
      if (EOL_CT > 0) then
	begin				(* paren at end, ind. a quantum *)
	CUR_IND := LIN_INDENT + TOK_IND + QUANTUM; (* one q after paren *)
	FOR_NO_REL			(* get rid of paren! *)
	end
      else begin			(* line up under first par *)
	FOR_NO_REL;			(* get rid of paren *)
	CUR_IND := TOK_IND + LIN_INDENT (* we know not first on line *)
	end
      end
    else FOR_NO_REL;			(* already set ind., write paren *)

    while not (THIS_TOKEN in SECKEYWDS + [CLOSEPARENSY]
		- [VARSY,PROCSY,FUNCTSY]) do
      begin				(* process rest of par list *)
      if THIS_TOKEN in [ETC, VARSY] then (* the normal things in proc heads *)
	FOR_NO_REL;			(* then write out at zero relative *)
      FOR_TYPE;				(* ident before doesn't matter *)
      if THIS_TOKEN = SEMICOLONSY then
	FOR_NO_REL			(* junk semi if there *)
      end (* within parens *);

    if THIS_TOKEN = CLOSEPARENSY then
      FOR_NO_REL			(* write paren, not other keyword! *)
    end (* (null) parameter list *);

  if FUNCT then
    FOR_TYPE;				(* get fval type, colon don't matter *)
  CUR_IND := ORIGL			(* restore *)
  end (* procedure FOR_PRO_DECL *);
$PAGE FOR_BODY format procedure/program body
procedure FOR_BODY;

(* FOR_BODY processes a body of code from 'begin' to 'end', including
   the terminal semicolon or period. FOR_BODY uses the INDSTK stack,
   and thus does not recurse. With the exception of 'if..then', a
   compound statement header (CSH) will place at most one marker on
   the stack. Terminators ('end','until', and semicolon) pop the stack
   until an appropriate place marker is found.  *)

var
  KWLEVEL,				(* for keeping ind. of CSH keywd *)
  B_LEVEL,				(* 'begin' count, to find end *)
  P_LEVEL: LINE_IDX;			(* openbracket/openparen count *)

  NOBEGIN,				(* true if BEGIN part of prev. CSH *)
  ELSE_SE		(* true if IF is really ELSE IF *)
  STMT: boolean;			(* true if within simple stmt *)

label 2,3;

  begin
  STK_INIT;				(* clear stack ... *)
  MARK(ONE_FLAG);			(* and save cur. ind. for end of body *)
  NOBEGIN := false;			(* first begin is CSH itself *)
  STMT := false;			(* not currently simple stmt *)
  B_LEVEL := 0;				(* no 'begin' et. al. seen yet *)
  P_LEVEL := 0;				(* zero nesting of paren/brack *)

  while not (THIS_TOKEN in SECKEYWDS - [BEGINSY]) do
    begin				(* for safety, kick out if new sect. *)
    if STMT and (THIS_TOKEN in NONSTMT) then
      begin				(* control res. word, must restore... *)
      POP_UNTIL ([ONE_FLAG]);		(* restore--we know one_flag is there *)
      STMT := false
      end;

    case THIS_TOKEN of

      BEGINSY: begin

      (* there are two kinds of begin. The first is when the begin is a
	 CSH by itself, that is, when NOBEGIN is false, or the begin
	 is first on the line and option 5 is in effect. We line up
	 the current indentation to it (if option 2 and NOBEGIN, then
	 we're already in a quantum for the terminator and body). *)

	if (not NOBEGIN) or 
	   (FIRST_ON_LINE and (5 in OPTIONS)) then
	  begin					(* begin is CSH *)
	  if not NOBEGIN then begin		(* CSH to line up to *)
	    if not FIRST_ON_LINE then
	      CUR_IND := LIN_INDENT + TOK_IND
	    end
	  else				(* CSH because of INDENT (5) *)
	    if FIRST_ON_LINE and
	       not (2 in OPTIONS) then
	      CUR_IND := CUR_IND + QUANTUM; (* tick if not ticked already *)
	  FOR_NO_REL			(* and write out slammed to min *)
	  end
	else FOR_SAV_REL;		(* else if cont., just write out *)

	if 2 in OPTIONS then
	  MARK (MANY_FLAG)
	else PUSH (MANY_FLAG);
 	NOBEGIN := false;
	B_LEVEL := B_LEVEL + 1;
	P_LEVEL := 0
      end (* BEGIN *);

      LOOPSY, REPEATSY: begin
	MARK (ONE_FLAG);		(* record for terminator after 'end *)
	if not FIRST_ON_LINE then
	  CUR_IND := LIN_INDENT + TOK_IND; (* ind. up to kwd if not first *)
	FOR_NO_REL;
	if 2 in OPTIONS then		(* tick a q now, put on stack *)
	  CUR_IND := CUR_IND + QUANTUM; (* to line terminator correctly *)
	MARK (MANY_FLAG);		(* record ind. for terminator *)

	if not (2 in OPTIONS) then	(* indent(2) means we already ticked *)
	  CUR_IND := CUR_IND + QUANTUM; (* otherwise tick after keyword *)

	NOBEGIN := false;
	B_LEVEL := B_LEVEL + 1;
	P_LEVEL := 0
	end (* LOOP, REPEAT *);

      FORSY, WHILESY, WITHSY: begin	(* all take a 'do' *)
	NOBEGIN := true;
	if not FIRST_ON_LINE then
	  NEW_LEVEL (ONE_FLAG)
	else MARK (ONE_FLAG);		(* one-flag, restored on semi *)
	FOR_NO_REL;			(* write out keyword *)
	KWLEVEL := CUR_IND;		(* and remember where *)
	if 2 in OPTIONS then
	  CUR_IND := CUR_IND + QUANTUM; (* INDENT(2)--> ind. CSH. cont. line *)

	GO_PAST (DOSY,FOR_SAV_REL);	(* get corresponding 'do' *)
	if THIS_TOKEN <> BEGINSY then
	  CUR_IND := KWLEVEL + QUANTUM  (* simple stmt. one q from kwd *)
	end (* FOR, WHILE, WITH *);

      EXITSY: begin
	NOBEGIN := false;		(* begin after exit is like CSH alone *)
	MARK(ONE_FLAG);			(* save indent *)
	if FIRST_ON_LINE and not
	  (2 in OPTIONS) then		(* deindent if first, and not IND(2)*)
	  CUR_IND := CUR_IND - QUANTUM;

	FOR_NO_REL;
	KWLEVEL := CUR_IND;		(* remember kwd level for subords *)
	if 2 in OPTIONS then
	  CUR_IND := CUR_IND + QUANTUM;	(* INDENT(2)--> CSH cont. line *)

	while not (THIS_TOKEN in [DOSY,SEMICOLONSY,ENDSY,UNTILSY]) do
	  FOR_SAV_REL;			(* get to terminator *)
	if THIS_TOKEN = DOSY then
	  FOR_SAV_REL;			(* now ready for either stmt or... *)
	CUR_IND := KWLEVEL + QUANTUM	(* semi; ind. q past kwd *)

	(* note: if the next token is one of the terminators, we know
	   that it will cause the stack to be popped, and previous
	   indentation to be restored. A simple statement, howover,
	   will be indented one q past the 'exit' keyword, and any
	   continuation lines of THAT will be further indented. *)

	end (* EXIT *);

      IFSY: begin			(* if statements are hard *)
	ELSE_SEEN := false;
	if not FIRST_ON_LINE then
	  CUR_IND := TOK_IND + LIN_INDENT; (* mark level at the 'if' *)
	3:				(* come here from 'else' *)
	MARK (THEN_FLAG);		(* stack indentation of kwd *)
	NOBEGIN := true;
	KWLEVEL := CUR_IND;
	FOR_NO_REL;

	(* the CSH keyword is now written out, and its indentation stacked.
	   Indent continuation lines if necessary, and get the 'then' *)

	if 2 in OPTIONS then
	  CUR_IND := CUR_IND + QUANTUM; (* CSH cont. line indented a q *)

	while THIS_TOKEN <> THENSY do	(* get that then *)
	  FOR_SAV_REL;

	(* now we have to decide where to write out the 'then', and
	   what to put on the stack for the 'else' *)

	CUR_IND := KWLEVEL;		(* back to start of CSH *)
	if FIRST_ON_LINE then		(* only matters if first *)
	  begin
	  if (1 in OPTIONS) then
	    begin
	    if ELSE_SEEN then
	      PUSH (THEN_FLAG)		(* ELSE IF -- stack the KW indent. *)
	    else begin
	      CUR_IND := CUR_IND + QUANTUM;
	      MARK (THEN_FLAG)		(* otherwise tick before stacking *)
	      end;
	    KWLEVEL := CUR_IND
	    end
	  else if 2 in OPTIONS then	(* make it look like indented...*)
	    PUSH (THEN_FLAG)		(* CSH continuation line *)
	  else MARK (THEN_FLAG);	(* otherwise put 'if' ind. on stack *)

	  FOR_NO_REL;			(* write out the 'then *)
	  end
	else begin			(* not first, stack 'if' indent *)
	  MARK (THEN_FLAG);
	  FOR_NO_REL
	  end;

	if (THIS_TOKEN <> BEGINSY) or
	   (2 in OPTIONS) then
	  CUR_IND := KWLEVEL + QUANTUM
	else CUR_IND := KWLEVEL
      end (* IF *);

      ELSESY: begin

	(* First get the appropriate indentation for the else. It lines
	   up with the 'if' normally. If INDENT(1), then it lines up to
	   the 'then' if the 'then' was first on line, else to the 'if'.
	   We stacked both values above, so we pick up either the first
	   or the second. *)

	ELSE_SEEN := true;		(* for 'else if' processing *)
	POP_UNTIL( [THEN_FLAG] );	(* get level of 'then' *)
	KWLEVEL := CUR_IND;		(* save it *)
	POP_UNTIL( [THEN_FLAG] );	(* get level of 'if' *)

	if NEXT_TOKEN = IFSY then
	  goto 3;			(* level all set, go to it. *)

	MARK (ONE_FLAG);		(* record indent of 'if' for semi *)
	NOBEGIN := true;
	if 1 in OPTIONS then		(* we should back off to the 'then' *)
	  CUR_IND := KWLEVEL;		(* which we saved above *)

	FOR_NO_REL;
	if (THIS_TOKEN <> BEGINSY)
	or (2 in OPTIONS) then		(* decide whether to indent next *)
	  CUR_IND := CUR_IND + QUANTUM
      end (* ELSE *);

      CASESY: begin
	NOBEGIN := false;
	MARK (ONE_FLAG);		(* so terminator after 'end' can pop back *)
	if not FIRST_ON_LINE then
	  CUR_IND := LIN_INDENT + TOK_IND; (* get keyword ind. level *)
	FOR_NO_REL;
	KWLEVEL := CUR_IND;		(* remember kw ind *)
	B_LEVEL := B_LEVEL + 1;		(* tick begin nesting level *)
	if 2 in OPTIONS then
	  CUR_IND := CUR_IND + QUANTUM;	(* indent CSH cont. lines *)
	MARK (CASE_FLAG);		(* indent level for terminator *)
	GO_PAST (OFSY, FOR_SAV_REL);	(* get past the 'of' *)
	CUR_IND := KWLEVEL + QUANTUM	(* indent labelled stmts *)
	end (* CASE *);

      ENDSY: begin
	NOBEGIN := false;
	POP_UNTIL( [MANY_FLAG,CASE_FLAG] ); (* back to begin, case, or loop *)
	FOR_NO_REL;			(* write 'end' at stacked ind. *)
	B_LEVEL := max(B_LEVEL - 1, 0)
	end (* END *);

      UNTILSY: begin			(* like end, but can have cont. lines *)
	NOBEGIN := false;		(* 'until' doesn't take a 'begin' *)
	POP_UNTIL( [MANY_FLAG] );	(* 'case' can't end in 'until' *)
	KWLEVEL := CUR_IND;		(* save restoring indentation *)
	FOR_NO_REL;			(* write it out *)
	B_LEVEL := max (B_LEVEL - 1, 0); (* dec begin nesting level *)
	if 2 in OPTIONS then
	  CUR_IND := CUR_IND + QUANTUM;	(* ind. cont. lines of UNTIL *)
	while not (THIS_TOKEN in [ENDSY,ELSESY,SEMICOLONSY,UNTILSY]) do
	  FOR_SAV_REL;			(* write out rest of UNTIL *)
	CUR_IND := KWLEVEL		(* restore previous ind *)
	end (* UNTIL *);

      SEMICOLONSY: begin
	FOR_NO_REL;			(* write it out *)
	NOBEGIN := false;
	POP_WHILE( [ONE_FLAG,THEN_FLAG] );
	IF B_LEVEL = 0 then return	(* last semi on procedure *)
	end (* SEMICOLON *);

      PERIODSY: begin			(* could be last thing in file! *)
	FOR_NO_REL;			(* write it out *)
	if B_LEVEL = 0 then return
	end (* PERIOD *);

      COLONSY: begin
	FOR_NO_REL;			(* write it out *)
	if (TOP_FLAG = CASE_FLAG) and
	  (P_LEVEL = 0) then		(* we have a case label at last *)
	  PUSH (ONE_FLAG)		(* disable case label hunt, tick a
					q for stmts., save for semi at end *)
	end (* COLON *);

      OPENPARENSY,OPENBRACKSY: begin
	FOR_SAV_REL;			(* write it out *)
	P_LEVEL := P_LEVEL + 1		(* and keep track of nesting level *)
	end (* OPENPAREN,OPENBRACKET *);

      CLOSEPARENSY,CLOSEBRACKSY: begin
	FOR_SAV_REL;
	P_LEVEL := max(P_LEVEL - 1, 0)
	end (* CLOSEPAREN,CLOSEBRACKET *);

      NUMERIC: if not STMT then
	if (TOP_FLAG <> CASE_FLAG) then
	  begin				(* yick -- declared label *)
	  MARK (ONE_FLAG);		(* save current ind. on stack *)
	  CUR_IND := PRO_LEVEL * QUANTUM; (* return to indent. of enclosing *)
	  GO_PAST (COLONSY, FOR_NO_REL); (* get to colon *)
	  POP_UNTIL ( [ONE_FLAG] );	(* back to saved indent *)
	  end
	else goto 2			(* not STMT -- must be case label *)
      else FOR_SAV_REL;			(* STMT -- normal numeric *)

      OTHERS: begin
	2:if TOP_FLAG = CASE_FLAG then	(* looking for case label *)
	  while THIS_TOKEN <> COLONSY do
	    FOR_NO_REL			(* get the colon and kick out *)
	else if not STMT then
	  begin				(* first word of simple stmt *)
	  if FIRST_ON_LINE then		(* must save current indent *)
	    MARK (ONE_FLAG)
	  else NEW_LEVEL(ONE_FLAG);	(* if not first, line cont.lines...*)
	  FOR_NO_REL;			(* put out first token *)
	  CUR_IND := CUR_IND + QUANTUM; (* a q past start of simple stmt. *)
	  STMT := true			(* and keep from ticking later *)
	  end
	else FOR_SAV_REL		(* and just write out others *)
	end (* OTHERS*)
      end (* case *)
    end (* while *)
  end (* procedure FOR_BODY *);
$PAGE INDFOR entry point for FORMAT

public procedure INDFOR;

(* INDFOR is the name of the coroutine that is CREATEd in the
   environment FORMAT. By looking at the current token, INDFOR
   decides which formatting routine to call. There is no exit 
   from the loop, since READER will DETACH when we're done, and
   the wrapper in INDENT will return, destroying the instantiation. *)

var
  EXT_FLAG: boolean;			(* true if 'external' seen prev. *)
  SEEN_EXTERN: boolean;			(* true if EXTERN/FORTRAN should not
					   cause a deindentation *)

  begin					(* a few initializations *)

  FIRST_ON_LINE := true;
  FIX_NEXT := false;
  SEEN_EXTERN := true;
  EXT_FLAG := false;
  CUR_IND := 0;
  PREV_IND := 0;
  LIN_INDENT := 0;
  OUT_IND := 0;
  PRO_LEVEL := 0;
  TOP_FLAG := CASE_FLAG;		(* init. for SPLITR *)
  SAFIRST := nil;			(* no comments yet *)
  SALAST := nil;

  detach;				(* end initializations *)

    loop
      case THIS_TOKEN of

	TYPESY: begin
	  FOR_TYPE_DECL;
	  EXT_FLAG := false
	  end (* TYPESY *);

	LABELSY: begin
	  FOR_LABEL;
	  EXT_FLAG := false
	  end (* LABELSY *);

	VARSY: begin
	  FOR_VAR_DECL;
	  EXT_FLAG := false
	  end (* VARSY *);

	CONSTSY: begin
	  FOR_CONST;
	  EXT_FLAG := false
	  end (* CONSTSY *);

	STATICSY, PUBLICSY: begin
	  FOR_NO_REL;
	  EXT_FLAG := false
	  end (* STATICSY,PUBLICSY *);

	EXTERNALSY: begin
	  FOR_NO_REL;
	  EXT_FLAG := true
	  end (* EXTERNALSY *);

	PROCSY, FUNCTSY: begin
	  if not EXT_FLAG then		(* tick level after decl *)
	    PRO_LEVEL := PRO_LEVEL + 1;
	  FOR_PRO_DECL;
	  CUR_IND := max (QUANTUM * PRO_LEVEL, 0);
	  SEEN_EXTERN := EXT_FLAG;	(* be on the lookout for psuedobody *)
	  EXT_FLAG := false
	  end (* PROCSY,FUNCTSY *);

	FORTRANSY, EXTERNSY, FORWARDSY: begin
	  FOR_SAV_REL;			(* write out the token *)
	  if not SEEN_EXTERN then
	    begin			(* this is the psuedo-body *)
	    PRO_LEVEL := max (PRO_LEVEL - 1, 0);
	    CUR_IND := PRO_LEVEL * QUANTUM
	    end;
	  SEEN_EXTERN := true
	end (* FORTRANSY, EXTERNSY *);

	BEGINSY: begin
	  PRO_LEVEL := max (PRO_LEVEL - 1, 0);
	  CUR_IND := PRO_LEVEL * QUANTUM;
	  FOR_BODY;
	  SEEN_EXTERN := true;		(* don't deindent on psuedobody *)
	  TOP_FLAG := CASE_FLAG;
	  EXT_FLAG := false
	  end (* BEGINSY *);

	OTHERS: FOR_SAV_REL

      end
    end
  end (* INDFOR and module *).
   c Tñ