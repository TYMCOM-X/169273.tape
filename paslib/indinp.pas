module INDINP;
$TITLE INDINP Input module for INDENT
$LENGTH 44
(*$Y20
$HEADER INDINP.HDR
*)
$PAGE GLOBAL declarations and vars

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
$INCLUDE INDINP.INC
(*$X10
$OPTIONS SOURCE
*)

type
  SYM_TEXT = packed array[1..2] of char;
  RES_TEXT = packed array[1..10] of char;
  SYM_SUBRANGE =  '!' .. '_' ;		(* interesting characters for symbols *)
  SYM_L_TYPE = array [1..MAX_SYM] of SYM_TEXT;
  SYM_X_TYPE = array [1..MAX_SYM] of SYMBOLS;
  SYM_TAB_TYPE = array[SYM_SUBRANGE] of SYMBOLS;
  SYM_SUB_SET = set of SYM_SUBRANGE;
  SYM_TOKEN = array[SYMBOLS] of TOKENTYPE;
  SCAN_TEXT = string[LINLEN];

const
  EOL_CH: char := chr(1);		(* SOH as endofline character *)
  MAX_RES_WD := 62;			(* number of reserved words + 10 *)

  SYM_LIST: SYM_L_TYPE := (
    ':=', '**', '(*', '*)', '!!', '<>', '<=', '>=', '..'   );

  SYM_LIST_SYM: SYM_X_TYPE := (
    ASSI_OP, EXP_OP, OPEN_COM_OP, CLOSE_COM_OP, CONCAT_OP, NE_OP,
    LE_OP, GE_OP, ELLIPS_OP
  );

  VALIDFIRSTS: SYM_SUB_SET :=
    [':', '*', '(', '!', '<', '>', '.'];

public const				(* so mainline can see it *)
  SYM_TABLE: SYM_TAB_TYPE := (
    NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD,  (* '!', '"', '#', and '$' *)
    OPEN_COM_OP,			(* '%' and we need a \ here *)
    NO_GOOD, NO_GOOD,			(* '&' and '''' *)
    LPAREN_OP, RPAREN_OP,		(* '(' and ')' *)
    MUL_OP, ADD_OP, COMMA_OP,		(* '*', '+', and ',' *)
    SUB_OP, PERIOD_OP,			(* '-' and '.' *)
    DIV_OP,				(* '/' *)
    NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, (* the digits 0-9 *)
    NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD,
    COLON_OP, SEMI_OP,			(* ':' and ';' *)
    LT_OP, EQ_OP, GT_OP,		(* '<', '=', and '>' *)

    (* following are 28 NO_GOODS for letters, '@' and '?' *)

    NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD,
    NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD,
    NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD,
    NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD, NO_GOOD,

    LBRACK_OP, CLOSE_COM_OP, RBRACK_OP, (*  '[', % '\', and ']' *)
    UPARR_OP, NO_GOOD			(* '^' and '_' *)
  );

  SYM_TO_TOKEN: SYM_TOKEN := (		(* table of symbols to tokens *)
    COLONSY, EQUALSY, SYMSY, SYMSY, SYMSY, SYMSY, SYMSY,
    SYMSY, CLOSEPARENSY, OPENPARENSY, CLOSEBRACKSY, OPENBRACKSY, SEMICOLONSY,
    SYMSY, SYMSY, SYMSY, COMMASY, PERIODSY, SYMSY, SYMSY, SYMSY,
    SYMSY, SYMSY, SYMSY, ASSIGNSY, SYMSY
  );


var
  NEXT_TEXT: SCAN_TEXT;			(* text scanned by lookahead *)
  NEXT_SYM: SYMBOLS;			(* symbol type of next-token if symb *)
  CH: char;				(* the current char. being scanned *)
  NEST_LEVEL,				(* comment nesting level *)
  NEXT_TOK_LEN,				(* length of token to replace TOK_LEN *)
  NEXT_EOL_CT,				(* and to replace EOL_CT *)
  NEXT_LIN_IND,				(* holds blank count until next scan *)
  NEXT_ORIG_IND: LINE_IDX;		(* and ORIG_IND *)
  LAST_CH: char;			(* for PUT_BACK *)

public var
  NEXT_PAGE: boolean;			(* true if NEXT_TOKEN is '$PAGE' *)
$PAGE GET_CH input character getter
procedure GET_CH;

(* GET_CH gets the next character in the input stream and stores it
   in ch. EOL_CH is returned if the line contains no non_blank
   characters. The leading blanks are stripped off a new line, and
   the number of them is placed in ORIG_IND if non_blank. *)

var
  T_IND: LINE_IDX;			(* temp for counting leading blanks *)

  begin
  if LAST_CH <> EOL_CH then
    begin
    CH := LAST_CH;
    LAST_CH := EOL_CH;
    return
    end;
    if CH = EOL_CH then			(* at eoln last time, read new line *)
      begin
      readln(INFILE);
      CH := ' ';			(* init to blank, we'll know if a... *)
      T_IND := 0;			(* non_blank found *)
      while not eoln(INFILE) do
	begin
	read (INFILE, CH);		(* get source character *)
      exit if CH <> ' ';		(* found a non_blank *)
	T_IND := T_IND + 1		(* else tick leading blank count *)
	end;

      (* now, if CH is blank, then eoln was reached without finding
	 a non-blank char, so return EOL_CH. if not blank, then we
	 return the char. EOLN may be high, but we'll return that
	 next time. *)

      if CH = ' ' then
	CH := EOL_CH
      else NEXT_LIN_IND := T_IND	(* CH already set, stomp ORIG_IND *)
      end

    else if eoln(INFILE) then		(* eoln leftover, return EOL_CH *)
      CH := EOL_CH

    else begin				(* first char after EOL_CH string *)
      read(INFILE, CH);			(* get the character *)
      if NEXT_LIN_IND >= 0 then		(* if this is first request after... *)
	NEXT_ORIG_IND := NEXT_LIN_IND;  (* char after EOL_CH, then scan starts *)
      NEXT_LIN_IND := -1		(* so set real orig_ind for this line *)
      end
  end (* procedure GET_CH *);


procedure PUT_BACK;

(* PUT_BACK is a helper for GET_CH. When CH is PUT_BACK, the next call to
   GET_CH will again return CH. This is useful for scanning ellipsis. *)

  begin
  LAST_CH := CH
  end;
$PAGE GET_COM comment processor
procedure GET_COM;

(* GET_COM is called to process the text of a comment. It is assumed that
   at the time of the call, NEST_LEVEL (the nesting level of comments) is
   non-zero, and that the current scanner character CH has not yet been
   processed. *)

  begin
    repeat				(* until done with this line *)
      if (CH = '\') or
	((CH = '*') and (INFILE^ = ')')) then
	begin				(* sneak a lookahead to see close *)
	if CH <> '\' then GET_CH;	(* must be closeparen--toss it *)
	GET_CH;				(* toss the stop-comment marker *)
	NEXT_TEXT := NEXT_TEXT || '*)';
	NEST_LEVEL := NEST_LEVEL - 1;

	(* now we've seen a close_comment mark. If comment-level
	   is zero, there may be another open-com on the line, which should
	   be included in the original comment. Thus, if nesting is zero,
	   let's eat up those intervening blanks before checking for
	   an open-comment mark. *)

	if NEST_LEVEL = 0 then
	  while CH = ' ' do		(* if not blank, then that's text *)
	    begin
	    NEXT_TEXT := NEXT_TEXT || ' ';
	    GET_CH
	    end
	end;

      (* now check for open-comment, not unlike close-comment above *)

      if (CH = '%') or
	((CH = '(') and (INFILE^ = '*')) then
	begin				(* got it *)
	if CH <> '%' then GET_CH;	(* toss openparen *)
	GET_CH;				(* toss percentsign or star *)
	NEXT_TEXT := NEXT_TEXT || '(*';
	NEST_LEVEL := NEST_LEVEL + 1
	end

      else if (NEST_LEVEL > 0) 		(* not open or close operator *)
	and (CH <> EOL_CH) then begin   (* and not end-of-line -- put it out *)
	NEXT_TEXT := NEXT_TEXT || CH;
	GET_CH
      end

    until (NEST_LEVEL = 0) or (CH = EOL_CH)

  (* now we're either at end of line with a possibly zero nesting count,
     or end of comment with real text following. We're done. *)

  end (* procedure GET_COM *);
$PAGE TABLES for reserved word lookup routine
type
  RES_TYPE = array[1..MAX_RES_WD] of RES_TEXT; (* for lex. table *)
  RES_TABL_TYPE = array[1..MAX_RES_WD] of TOKENTYPE; (* and assoc. token *)
  RES_XL_TYPE = array[0..10] of 1..MAX_RES_WD; (* length of token into table *)


var
  RES_WORD: RES_TYPE := (

  (* The reserved word table. To look up a token, insert it into the
     table following the last reserved word with the same length. When
     a match is made (always will), the entry in RES_XLATE (below) will
     give the corresponding token type. *)

    '', 'OF', 'TO', 'IN', 'IF', 'OR', 'DO', 'BY',
    '', 'VAR', 'END', 'AND', 'FOR', 'SET', 'DIV', 'MOD', 'NOT',
    '', 'TYPE', 'THEN', 'ELSE', 'EXIT', 'WITH', 'LOOP', 'ORIF',
	'CASE', 'GOTO', 'FILE', 'STOP',
    '', 'CONST', 'BEGIN', 'WHILE', 'UNTIL', 'LABEL', 'ANDIF',
	'ARRAY', 'ALGOL',
    '', 'STATIC', 'REPEAT', 'PASCAL', 'MODULE', 'EXTERN', 'PACKED',
	'RECORD', 'PUBLIC', 'DOWNTO', 'OTHERS', 'STRING', 'RETURN',
    '', 'FORTRAN', 'FORWARD', 'PROGRAM',
    '', 'FUNCTION', 'EXTERNAL',
    '', 'PROCEDURE',
    '', (* no reswords of length ten *)
    ''
  );

const
  RES_XLATE: RES_TABL_TYPE := (

  (* The token types corresponding to each entry in the lex table above *)

    ETC, OFSY, TOSY, INSY, IFSY, ORSY, DOSY, BYSY,
    ETC, VARSY, ENDSY, ANDSY, FORSY, SETSY, DIVSY, MODSY, NOTSY,
    ETC, TYPESY, THENSY, ELSESY, EXITSY, WITHSY, LOOPSY, ORIFSY,
	 CASESY, GOTOSY, FILESY, STOPSY,
    ETC, CONSTSY, BEGINSY, WHILESY, UNTILSY, LABELSY, ANDIFSY,
	 ARRAYSY, ALGOLSY,
    ETC, STATICSY, REPEATSY, PASCALSY, MODULESY, EXTERNSY, PACKEDSY,
	 RECORDSY, PUBLICSY, DOWNTOSY, OTHERSSY, STRINGSY, RETURNSY,
    ETC, FORTRANSY, FORWARDSY, MODULESY,
    ETC, FUNCTSY, EXTERNALSY,
    ETC, PROCSY,
    ETC, ETC );

  RES_XREF: RES_XL_TYPE := (

  (* Where to place a token of length INDEX into the RES_WORD table. *)

    1, 1, 9, 18, 30, 39, 52, 56, 59, 61, 62
  );
$PAGE WORD_LOOKUP looks up alpha text to see if reserved word.
public function WORD_LOOKUP (IDENT: SCAN_TEXT): TOKENTYPE;

(* WORD_LOOKUP uses the above tables to identify an alphanumeric string
   as either a reserved word or an ETC. It checks all reserved words of
   the same length as the search goal. By placing the goal as the last
   entry in the reserved word table, a match is guaranteed. *)

var
  I, LENG: LINE_IDX;			(* to avoid complex expressions *)

  begin
  WORD_LOOKUP := ETC;			(* set now in case IDENT bad length *)
  LENG := length(IDENT);
  if (LENG > 1) and (LENG < 11) then
    begin				(* length OK, do the lookup *)
    RES_WORD[RES_XREF[LENG]] := IDENT;

    (* IDENT now in table at proper place. The following FOR loop will
       never terminate normally, as the last is guaranteed to match. *)

    for I := RES_XREF[LENG - 1] + 1  to RES_XREF[LENG] do
      exit if RES_WORD[I] = IDENT do
	WORD_LOOKUP := RES_XLATE[I]
    end
  end (* function WORD_LOOKUP *);
$PAGE SYM_LOOKUP tries to identify two-character special symbols
public function SYM_LOOKUP (SYMB: SCAN_TEXT): SYMBOLS;

(* SYM_LOOKUP attempts to locate SYMB in the two-character special
   symbol table SYM_LIST. If the symbol is found, the corresponding
   symbol type in SYM_LIST_SYM is returned as function value, otherwise
   the value NO_GOOD is returned. Unlike the reserved word lookup
   above, an exhaustive search is employed, since there are only
   8 two-character symbols which this routine must identify. *)

var
  I: LINE_IDX;				(* for loop index *)

  begin
  SYM_LOOKUP := NO_GOOD;		(* set now in case not found *)
  if length(SYMB) = 2 then		(* don't bother otherwise *)
    for I := 1 to MAX_SYM do
      exit if SYM_LIST[I] = SYMB do
	SYM_LOOKUP := SYM_LIST_SYM[I]
  end (* function SYM_LOOKUP *);
$PAGE CHANGE_CASE little routine to perform case conversions
procedure CHANGE_CASE (HOW: CASE_TYPE);

(* CHANGE_CASE alters the case of NEXT_TEXT (being assembled by SCAN_NEXT
   below) according to HOW. Thus, if SCAN_NEXT identifies a reserved word,
   its case is changed according to the user's specification, and similarly
   for text words. *)

  begin
  if HOW <> NO_CHANGE then
    begin				(* NO_CHANGE is the default *)
    if HOW = CAP then
      NEXT_TEXT := uppercase(NEXT_TEXT)
    else begin				(* either DECAP or MIXED *)
      NEXT_TEXT := lowercase (NEXT_TEXT); (* LC the whole thing anyway *)
      if HOW = MIXED then		(* then just cap the first letter *)
	NEXT_TEXT[1] := uppercase(NEXT_TEXT[1])
      end
    end
  end (* procedure CHANGE_CASE *);
$PAGE SCAN_NEXT the token scanner
procedure SCAN_NEXT;

(* SCAN_NEXT is the token scanner for INDENT. It recognizes all PASCAL
   reserved words and special symbols. TOK_LEN is set in SCAN_NEXT;
   trailing blanks are appended onto the token. SCAN_NEXT operates 
   on NEXT_TOKEN, NEXT_TOK_LEN, and NEXT_EOL_CT; in other words, the
   next token is fully scanned before the other coroutines are resumed
   with the current token.

   State is preserved with respect to comments through NEST_LEVEL; if
   non-zero, the scanner is working on a comment that has not yet been
   closed, and continues doing so until it has been closed. The text
   of the scanned token is returned in NEXT_TEXT (including trailing
   blanks), and the caller should enforce BEFORE and AFTER spacing. *)

  label 5;

  begin
  NEXT_TEXT := '';
  NEXT_TOKEN := SYMSY;			(* preset *)
  NEXT_SYM := NO_GOOD;
  NEXT_PAGE := false;

  (* since the previous scan included trailing blanks, it is assumed
     at this point that CH is the first character of the next token,
     so the initial caller should perform a GET_CH. Also, GET_CH sets
     ORIG_IND when reading in a new line, removing leading blanks. *)

  if NEST_LEVEL = 0 then		(* if 0, perform scan *)

    case uppercase (CH) of

      'A'..'Z': begin			(* program text or reserved word *)
	while uppercase(CH) in ['A'..'Z','0'..'9','_','$'] do
	  begin				(* tack on all alphanumerics *)
	  NEXT_TEXT := NEXT_TEXT || CH;
	  GET_CH
	  end;				(* now have entire token *)

	NEXT_TOKEN := WORD_LOOKUP ( uppercase (NEXT_TEXT) );

	(* we now know if reserved word. Note that original case has
	   been preserved. Now perform appropriate case conversion. *)

	if NEXT_TOKEN = ETC then
	  CHANGE_CASE(TEXT_CASE)
	else CHANGE_CASE(WORD_CASE)
      end (* alphanumeric token *);

      '0'..'9','.': begin
	NEXT_TEXT := CH;
	GET_CH;
	NEXT_TOKEN := NUMERIC;
	if (NEXT_TEXT <> '.') or
	   (CH in ['0'..'9']) then
	  while uppercase (CH) in ['0'..'9','E','.'] do
	    begin
	    if uppercase(CH) = 'E' then
	      begin
	      NEXT_TEXT := NEXT_TEXT || CH;
	      GET_CH;
	      if CH in ['+','-'] then
		begin
		NEXT_TEXT := NEXT_TEXT || CH;
		GET_CH
		end
	      end

	    else if CH = '.' then
	      begin
	      GET_CH;
	      if CH = '.' then
		begin
		PUT_BACK;
		goto 5
		end
	      else NEXT_TEXT := NEXT_TEXT || '.'
	      end
	    else begin
	      NEXT_TEXT := NEXT_TEXT || CH;
	      GET_CH
	      end
	  end
	else begin
	  if CH = '.' then
	    begin
	    NEXT_TOKEN := ELLIPSISY;
	    NEXT_SYM := ELLIPS_OP;
	    NEXT_TEXT := '..';
	    GET_CH
	    end
	  else begin
	    NEXT_TOKEN := PERIODSY;
	    NEXT_SYM := PERIOD_OP
	    end
	  end;
      5:end (* numeric or dot *);

      '''': begin			(* literal *)
	repeat
	  repeat
	    NEXT_TEXT := NEXT_TEXT || CH;
	    GET_CH
	  until (CH = '''') or (CH = EOL_CH);
	  exit if CH = EOL_CH;
	  NEXT_TEXT := NEXT_TEXT || CH;
	  GET_CH
	until CH <> '''';
	NEXT_TOKEN := LITERALSY
      end (* literal *);

      '$': begin
	NEXT_TOKEN := DIRECTIVE;
	NEXT_TEXT := CH;
	GET_CH;
	while (CH <> ' ') and (CH <> EOL_CH) do
	  begin				(* capitalize directive keyword *)
	  NEXT_TEXT := NEXT_TEXT || uppercase(CH);
	  GET_CH
	  end;

	NEXT_PAGE := ( NEXT_TEXT = '$PAGE' );

	(* now just get rest of directive line *)

	while (CH <> EOL_CH) do
	  begin
	  NEXT_TEXT := NEXT_TEXT || CH;
	  GET_CH
	  end
      end (* directive *);

      OTHERS: begin
	NEXT_TEXT := CH;
	GET_CH;
	if NEXT_TEXT = '|' then NEXT_TEXT := '!';
	if CH = '|' then CH := '!';

	(* all interesting characters will now appear in the subrange 
	   SYM_SUBRANGE of char. Check it before trying to identify. *)

	if (NEXT_TEXT[1] >= '!') and
	   (NEXT_TEXT[1] <= '_') then
	  begin
	  if NEXT_TEXT[1] in VALIDFIRSTS then
	    begin
	    NEXT_SYM := SYM_LOOKUP (NEXT_TEXT || CH);
	    if NEXT_SYM <> NO_GOOD then
	      begin
	      if CH = '!' then begin	(* reconvert concat to vert. bars *)
		NEXT_TEXT := '||';
		GET_CH
		end
	      else begin
		NEXT_TEXT := NEXT_TEXT || CH;
		GET_CH
		end
	      end
	    else NEXT_SYM := SYM_TABLE[NEXT_TEXT[1]]
	    end
	  else NEXT_SYM := SYM_TABLE[NEXT_TEXT[1]]
	  end
	else NEXT_SYM := NO_GOOD;

	if NEXT_SYM <> OPEN_COM_OP then
	  NEXT_TOKEN := SYM_TO_TOKEN[NEXT_SYM]
	else begin
	  if NEXT_TEXT = '%' then
	    NEXT_TEXT := '(*';
	  NEST_LEVEL := 1;
	  GET_COM;
	  if (EOL_CT > 0) and		(* if comment starts line... *)
	  ((NEST_LEVEL > 0 ) or		(* and either isn't closed yet... *)
	   (CH = EOL_CH)) then		(* or end of line reached while proc *)
	    NEXT_TOKEN := SA_COM	(* then it's a stand-alone com. line *)
	  else NEXT_TOKEN := OPEN_COM	(* else it's an embedded *)
	  end
      end (* others *)
    end (* case and then *)

  else begin				(* still within comment *)
    GET_COM;
    if THIS_TOKEN <> SA_COM then
      begin
      if NEST_LEVEL = 0 then
	NEXT_TOKEN := CLOSE_COM
      else NEXT_TOKEN := CONT_COM
      end
    else NEXT_TOKEN := SA_COM
    end;

  NEXT_TOK_LEN := length (NEXT_TEXT);
  while CH = ' ' do			(* attach trailing blanks *)
    begin
    NEXT_TEXT := NEXT_TEXT || CH;
    GET_CH
    end;

  NEXT_EOL_CT := 0;
  if CH = EOL_CH then
    repeat
      NEXT_EOL_CT := NEXT_EOL_CT + 1;
    exit if eof(INFILE);		(* last line in file, don't loop *)
      GET_CH
    until CH <> EOL_CH
  end (* procedure SCAN_NEXT *);
$PAGE CHECK_BEFORE checks for spaces before tokens
procedure CHECK_BEFORE;

(* CHECK_BEFORE enforces the user directive BEFORE(...) by insuring that
   the text of THIS_TOKEN is followed by at least one space when THIS_TOKEN
   is in the set selected by the user. Since the number of possible
   before/after tokens is too large ti fit into one scalar type, there
   is a scalar type for reserved words (and other things), and one for
   operators. *)

  begin
  if EOL_CT = 0 then			(* don't bother if THIS_TOKEN first *)
    begin
    if (NEXT_TOKEN in TOK_BEFORES) or
       (NEXT_SYM in SYM_BEFORES) then   (* then it requires *)

      if CUREND - CURSTART < TOK_LEN then  (* no space at all, put one *)
	begin
	CUREND := CUREND + 1;
	LINE[CUREND] := ' '
	end
    end
  end (* procedure CHECK_BEFORE *);
$PAGE CHECK_AFTER checks for spaces after tokens
procedure CHECK_AFTER;

(* CHECK_AFTER performs a similar function for the AFTER directive. 
   In this case, THIS_TOKEN is examined. *)

  begin
  if EOL_CT = 0 then			(* don't bother if last on line *)
    if (THIS_TOKEN in TOK_AFTERS) or
       (NEXT_SYM in SYM_AFTERS) then
      if NEXT_TEXT [ length(NEXT_TEXT) ] <> ' ' then (* last one not blank *)
	NEXT_TEXT := NEXT_TEXT || ' '
  end (* procedure CAFTER *);
$PAGE INDINP entry into coroutine READER
public procedure INDINP;

(* INDINP is the coroutine living in the environment READER, performing
   all program text input operations for INDENT.  READER is the only co-
   routine which will detach (except at initialization); it will do so
   after the token preceding the input end of file has been processed
   by WRITER.   INDINP is responsible for enforcing BEFORE and AFTER
   spacing.  As mentioned above, INDINP works with a full look-ahead,
   completely processing NEXT_TOKEN when resuming SPLITR with THIS_TOKEN.
   Therefore, an extra input buffer is required -- input cannot be
   read directly into LINE. SCAN_NEXT reads NEXT_TOKEN into NEXT_TEXT,
   and INDINP places this text into line before resuming to SPLITR. *)

  begin					(* initialize *)
  THIS_TOKEN := ETC;  NEXT_TOKEN := ETC;
  CURSTART := 1;  CUREND := 1;
  LINE := ' ';
  TOK_LEN := 0;  ORIG_IND := 0;  EOL_CT := 1;
  TOK_IND := 0;
  CH := EOL_CH;				(* to force GET_CH to read line *)
  LAST_CH := EOL_CH;			(* no put back yet *)
  NEST_LEVEL := 0;
  NEXT_LIN_IND := -1;

  detach;

  GET_CH;				(* read in first char to start off *)

    loop
      if eof(INFILE) then
	begin				(* got eof on last NEXT_TOKEN scan *)
	EOL_CT := 1;			(* only one end of line to follow *)
	resume(SPLITR);			(* it's now THIS_TOKEN -- send it *)
	if THIS_TOKEN in [SA_COM, DIRECTIVE] then
	  begin				(* must clear out buffered comment *)
	  THIS_TOKEN := OPEN_COM;	(* will slip through WRITER *)
	  CUREND := CURSTART;		(* length zero *)
	  EOL_CT := 0;			(* no extra cr/lf *)
	  OUT_IND := 0;			(* align to left margin *)
	  COM_IND := 0;
	  resume (WRITER)		(* go directly to WRITER *)
	  end;
	detach				(* and we're all done *)
	end;

      SCAN_NEXT;			(* process NEXT_TOKEN *)
      CHECK_BEFORE;			(* make sure THIS_TOKEN has trailing
					   space if NEXT_TOKEN needs leading *)

      (* at this point, THIS_TOKEN and NEXT_TOKEN are ready. *)

      if EOL_CT > 0 then
	begin				(* reset line index *)
	resume (SPLITR);
	CUREND := 0
	end
      else resume (SPLITR);

      (* now place NEXT_TOKEN as THIS_TOKEN. *)

      THIS_TOKEN := NEXT_TOKEN;
      EOL_CT := NEXT_EOL_CT;
      ORIG_IND := NEXT_ORIG_IND;
      TOK_LEN := NEXT_TOK_LEN;

      (* check 'after' spacing on the new THIS_TOKEN now. *)

      CHECK_AFTER;

      (* now set up new start and stop indices, and put text into LINE *)

      CURSTART := CUREND + 1;
      TOK_IND := CURSTART - 1;
      LINE[CURSTART: length(NEXT_TEXT)] := NEXT_TEXT;
      CUREND := CURSTART + length(NEXT_TEXT) - 1
    end (* loop *)
  end (* procedure INDINP and module *).
    RO;VH