$TITLE PASLEX.PAS, last modified 5/11/84, zw
MODULE paslex OPTIONS special(word);
(*TYM-Pascal Compiler Lexical Scanner*)
$HEADER PASLEX.HDR
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE paserr.inc
$INCLUDE pasfil.inc
$INCLUDE pasutl.inc
$INCLUDE pasval.inc
$INCLUDE corout.inc
$INCLUDE pasrdr.inc
$PAGE declarations
PUBLIC VAR (* information available to rest of parse *)
token: token_type; (* last token scanned *)
last_token: token_type; (* previous token *)
STATIC VAR
lindex: line_index; (* line scanning cursor *)
$PAGE local definitions
(* Lexical analysis state information and type definitions. *)

CONST
max_comment_level = 8;
STATIC VAR
comment_level: 0..max_comment_level; (* current depth of nested comments *)
comment_start: ARRAY [0..max_comment_level] OF source_id;
(* line at which each level of comment begins *)
$PAGE lexicon

$INCLUDE pasrw.inc
$PAGE lex_init
(* LEX INIT initializes the lexical analysis state information. *)
PUBLIC
PROCEDURE lex_init;
BEGIN
  line := '  '; (* prime next line to get line on first call *)
  lindex := 1;
  literal_line := '';
  WITH token DO BEGIN
    sym := badsymbol; (* field tested on entry to scan *)
    dummy := FALSE; (* no text for token *)
    source := cur_source; (* file 0, page 0, line 0 *)
    next := NIL;
    defn := NIL;
  END;
  last_token := token;
  comment_level := 0;
END;
$PAGE scan_number
(* SCAN NUMBER parses an integer or real number, and sets up the token
   information for what it sees. *)

PROCEDURE scan_number;

  PROCEDURE scan_digits;
  BEGIN
    lindex := lindex + VERIFY (SUBSTR (line, lindex), ['0'..'9']) - 1;
  END;
VAR
ndigits: int_type;
BEGIN
  token.sym := intconst; (* assume integer to start with *)
  scan_digits; (* get integer part, if any *)
  ndigits := lindex - token.column;
  IF line[lindex] = 'B' THEN BEGIN (* octal constant: nnnnnnB *)
    token.value := cst_int (SUBSTR (line, token.column, ndigits)
      , 8, token.column);
    lindex := lindex + 1;
  END
  ELSE BEGIN (* decimal integer or real constant *)
    IF (line[lindex] = '.') (* check if there is a decimal point *)
    AND (line[lindex+1] <> '.') THEN BEGIN (* but not an elipsis *)
      token.sym := realconst;
      lindex := lindex + 1;
      scan_digits; (* scan fraction *)
      ndigits := lindex - token.column - 1;
    END;
    IF line[lindex] = 'E' THEN BEGIN (* has an exponent field *)
      token.sym := realconst;
      lindex := lindex + 1;
      IF line[lindex] IN ['+', '-'] THEN lindex := lindex + 1;
      scan_digits;
    END;
    WITH token DO BEGIN
      IF sym = intconst THEN value := cst_int (SUBSTR (line, column, ndigits)
	, 10, column)
      ELSE value := cst_real (SUBSTR (line, column, lindex - column), ndigits)
	;
    END;
  END;
END;
$PAGE scan
(* SCAN reads the next token of input, bypassing white space and comments
   and fills in the corresponding information in the public var TOKEN. *)
PUBLIC
FUNCTION scan: symbols;
LABEL
100 (* eof *),
200 (* reserved word found *);
(* Entered when an EOF is encountered on the master file. This sets
     the token info to eof and forces an exit of scan. *)

  PROCEDURE set_eof;
  BEGIN
  (* leave "cur_source" at logical end of main file for other errors *)
    token.dummy := TRUE; (* eof token has no text, anyway *)
    token.sym := eofsy;
    IF comment_level > 0 THEN (* comment unclosed *)
    err_print (err_unclosed_comment, comment_start[comment_level], '', 0);
    GOTO 100 (* EOFABORT *)
  END;
  (* Procedure to read a new logical line into the line buffers. Filters and
       processes directive lines including $INCLUDE. Returns if a new line can
       be found. Calls SETEOF if there is no remaining text. *)

  PROCEDURE next_line;
  BEGIN
    IF end_of_file THEN set_eof; (* does not return *)
    call (reader); (* fetch new line *)
    lindex := 1;
    token.source := cur_source;
  END;
  (* PUSH COMMENT enters a level of comment nesting by incrementing the
       comment level counter and recording the statement at which the comment
       starts *)

  PROCEDURE push_comment;
  BEGIN
    IF comment_level = max_comment_level THEN BEGIN
      error (err_comments_too_deep);
      STOP
    END;
    comment_level := comment_level + 1;
    comment_start [comment_level] := cur_source
  END;
$PAGE scan mainline
VAR
l: line_index;
idx: int_type;
word: rwordtype;
TEXT: line_string;
radix: int_type;
BEGIN
  last_token := token; (* save for error recovery *)
  token.dummy := FALSE; (* assume token has text *)
  IF token.sym <> eofsy THEN BEGIN (* once in eof state, stay there *)
    LOOP (* until good symbol found *)
      ;
      (*  Ignore white space and comments.  *)
      LOOP
	LOOP
	  l := VERIFY (SUBSTR (line, lindex), [' ']);
	  EXIT IF l <> 0;
	  next_line
	END;
	lindex := lindex + l - 1;
	EXIT IF (line [lindex] <> '(') OR (line[lindex+1] <> '*');
	push_comment;
	lindex := lindex + 2;
	IF (lindex <= LENGTH (line)) ANDIF (line[lindex] = '$')
	  THEN err_print (err_cmt_dollar_option, cur_source, '$', lindex);
	REPEAT (* until back at starting level *)
	  LOOP (* search for comment delims *)
	    l := SEARCH (SUBSTR (line, lindex), ['(','*']);
	    EXIT IF l <> 0;
	    next_line
	  END;
	  lindex := lindex + l - 1; (* position at first delim *)
	  IF (line[lindex] = '(') AND (line[lindex+1] = '*') THEN BEGIN
	    push_comment;
	    lindex := lindex + 2;
	    IF (lindex <= LENGTH (line)) ANDIF (line[lindex] = '$')
	      THEN err_print (err_cmt_dollar_option, cur_source, '', lindex);
	  END
	  ELSE IF (line[lindex] = '*') AND (line[lindex + 1] = ')') THEN BEGIN
	    comment_level := comment_level - 1;
	    lindex := lindex + 2;
	  END
	  ELSE lindex := lindex + 1 (* not comment start or stop, reconsider next char *)
	UNTIL comment_level = 0
      END;
      token.column := lindex; (* remember where token appears *)
      token.LENGTH := 1; (* set in case of errors *)
      CASE line [lindex] OF
	'A'..'Z','_','$': (* identifier or reserved word *)
	BEGIN
	  l := VERIFY (SUBSTR (line, lindex), ['A'..'Z','_','$','0'..'9'])
	    - 1;
	  lindex := lindex + l;
	  IF l < UPPERBOUND (frw) THEN BEGIN
	    word := SUBSTR (line, lindex-l, l); (* pad and align for quick comparison *)
	    FOR idx := frw[l] TO frw[l+1]-1 DO IF rw[idx] = word THEN BEGIN
	      token.sym := rsy[idx];
	      IF rop[idx] <> noop THEN token.op := rop[idx];
	      GOTO 200
	    END
	  END; (* exit if too long, or not found in RW list *)
	  token.sym := ident;
	  token.name := entername (SUBSTR (line, lindex - l, l));
	  200:
	END;
	'''': BEGIN (* character string constant *)
	  token.sym := stringconst;
	  lindex := lindex + 1;
	  TEXT := '';
	  LOOP
	    l := INDEX (SUBSTR (line, lindex), '''');
	    IF l <> 0 THEN BEGIN (* found second quote *)
	      TEXT := TEXT || SUBSTR (literal_line, lindex, l-1);
	      lindex := lindex + l
	    END
	    ELSE BEGIN (* unbalanced quote *)
	      token.sym := badsymbol;
	      err_print (err_unbalanced_quote, cur_source, '', LENGTH (line));
	      lindex := LENGTH (line) (* force fallout *)
	    END;
	    EXIT IF line[lindex] <> ''''; (* exit if not doubled quote *)
	    TEXT := TEXT || '''';
	    lindex := lindex + 1
	  END;
	  IF LENGTH (TEXT) = 1 THEN token.value := cst_scalar (ORD (TEXT[1]))
	  ELSE token.value := cst_string (TEXT)
	END;
	'0'..'9': scan_number; (* entered in other places too *)
	'.': BEGIN
	  IF line[lindex+1] = '.' THEN BEGIN
	    token.sym := elipsis;
	    lindex := lindex + 2
	  END
	  ELSE IF line[lindex+1] IN ['0'..'9'] THEN scan_number (* starts with a decimal point *)
	  ELSE BEGIN
	    token.sym := period;
	    lindex := lindex + 1
	  END
	END;
	'#': BEGIN
	  lindex := lindex + 1;
	  CASE line[lindex] OF
	    'B': radix := 2;
	    'O': radix := 8;
	    'D': radix := 10;
	    'H': radix := 16;
	    OTHERS: radix := 0
	  END;
	  IF radix = 0 THEN token.sym := badsymbol
	  ELSE BEGIN
	    token.sym := intconst;
	    lindex := lindex + VERIFY (SUBSTR (line, lindex + 1)
	      , ['0'..'9', 'A'..'F']);
	    token.value := cst_int (SUBSTR (line, token.column + 2, lindex -
	      token.column - 2), radix, token.column);
	  END;
	END;
	':': BEGIN
	  lindex := lindex + 1;
	  IF line[lindex] = '=' THEN BEGIN
	    token.sym := becomes;
	    lindex := lindex + 1
	  END
	  ELSE token.sym := colon
	END;
	'<','>': BEGIN
	  token.sym := relop;
	  token.op := sop[line[lindex]];
	  lindex := lindex + 1;
	  IF line[lindex] = '=' THEN BEGIN (* case of >= <= *)
	    IF token.op = ltop THEN token.op := leop
	    ELSE token.op := geop;
	    lindex := lindex + 1
	  END
	  ELSE IF (line [lindex] = '>') AND (token.op = ltop) THEN BEGIN
	    token.op := neop;
	    lindex := lindex + 1
	  END
	END;
	'*': BEGIN
	  lindex := lindex + 1;
	  IF line[lindex] = '*' THEN BEGIN
	    token.sym := powerop;
	    token.op := expon; (* case of ** *)
	    lindex := lindex + 1;
	  END
	  ELSE IF line[lindex] = ')' THEN error (err_unopened_comment)
	  ELSE BEGIN
	    token.sym := mulop;
	    token.op := mul;
	  END;
	END;
	'|': BEGIN
	  lindex := lindex + 1;
	  IF line[lindex] = '|' THEN BEGIN (* concatenation operator *)
	    token.sym := addop;
	    token.op := catop;
	    lindex := lindex + 1
	  END
	  ELSE BEGIN (* | not allowed *)
	    token.sym := badsymbol;
	  END
	END;
	OTHERS: (* fetch data from table *)
	BEGIN
	  token.sym := ssy [line[lindex]];
	  IF sop[line[lindex]] <> noop THEN token.op := sop [line[lindex]];
	  lindex := lindex + 1
	END
      END; (* of case on line character *)
      EXIT IF token.sym <> badsymbol; (* of loop to get symbol *)
      err_print (err_bad_token, token.source, SUBSTR (line, token.column,
	token.LENGTH), token.column)
    END
  END; (* of if on eofsy *)
  token.LENGTH := lindex - token.column; (* record length *)
  (* EOFABORT *)
  100: (* on abort, token.sym has been set *)
  scan := token.sym
END.
  