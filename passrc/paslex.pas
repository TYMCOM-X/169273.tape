$WIDTH=100
$LENGTH=55
$TITLE PASLEX.PAS, last modified 3/26/84, zw
MODULE paslex;
(*TYM-Pascal compiler -- lexical scanner*)

$HEADER PASLEX.HDR

$PAGE system modules
$SYSTEM UTLTYP.TYP
$SYSTEM PASCAL.INC
$SYSTEM DTIME.TYP
$SYSTEM PASSPF.TYP
$SYSTEM PASIST.TYP
$SYSTEM PASSY.TYP
$SYSTEM PASPT.TYP
$SYSTEM PASERR.INC
$SYSTEM PASUTL.INC
$SYSTEM PASVAL.INC
$SYSTEM PASRDR.INC

$INCLUDE PASLEX.TYP

$PAGE definitions
CONST max_comment_level = 8;
TYPE
comment_depth: 0 .. max_comment_depth;
comment_start: ARRAY [0 .. max_comment_depth] OF source_position;
VAR
source_cursor: source_line_cursor;

PUBLIC VAR
token: token_type; (*The token just scanned.*)
lsttkn: token_type; (*The previous token.*)

EXCEPTION end_of_file;

$PAGE lexbgn, next_line
PUBLIC PROCEDURE lexbgn;
(*Set up the lexical analysis state information.*)
BEGIN
  srclin := '  ';
  litline := '';
  source_cursor := 1;
  comment_depth := 0;
  WITH token DO BEGIN
    sym := badsymbol;
    dummy := no;
    source := srcpos;
    next := NIL;
    defn := NIL
  END;
  lsttkn := token
END;

PROCEDURE next_line;
(*Read next source line.  Signals end_of_file otherwise.*)
BEGIN
  IF NOT srceof THEN BEGIN (*Set up to scan next line.*)
    rdsrc;
    source_cursor := 1;
    token.source := srcpos
  END
  ELSE BEGIN (*End of source file.*)
    (*The srcpos is at the logical end of the main file for any other errors*)
    token.dummy := yes; (*The eof token has no text, anyway.*)
    token.sym := eofsy;
    IF comment_depth > 0
    THEN errprt(err_unclosed_comment, comment_start[comment_depth], '', 0);
    SIGNAL(end_of_file)
  END
END;

$PAGE skip_spaces, scan_comments
PROCEDURE skip_spaces;
(*Skip over any spaces.*)
BEGIN
  LOOP
    l := VERIFY(SUBSTR(srclin, source_cursor), [' ']);
    EXIT IF l <> 0;
    next_line
  END;
  source_cursor := source_cursor + l - 1
END;

PROCEDURE scan_comments;
(*Skip over comments.*)
PROCEDURE begin_comment;
(*Begin a comment.*)
BEGIN
  comment_depth := comment_depth + 1;
  IF comment_depth <= max_comment_depth
  THEN comment_start[comment_depth] := srcpos
  ELSE error(err_comments_too_deep)
  source_cursor := source_cursor + 2;
  IF srclin[source_cursor] = '$'
  THEN err_print(err_cmt_dollar_option, cur_source, '', source_cursor)
END;
PROCEDURE end_comment;
(*End a comment.*)
BEGIN
  comment_depth := comment_depth - 1;
  source_cursor := source_cursor + 2
END;
BEGIN
  REPEAT (*Repeat until back at starting depth.*)
    LOOP (*Search for comment markers.*)
      l := SEARCH(SUBSTR(line, source_cursor), ['(','*']);
      EXIT IF l <> 0;
      next_line
    END;
    source_cursor := source_cursor + l - 1; (*Position at first marker.*)
    IF (srclin[source_cursor] = '(') AND (srclin[source_cursor + 1] = '*')
    THEN begin_comment
    ELSE IF (line[source_cursor] = '*') and (line[source_cursor + 1] = ')')
    THEN end_comment
    END
    ELSE source_cursor := source_cursor + 1
  UNTIL comment_depth = 0
END;

$PAGE next_token, scan_digits, scan_integer
PROCEDURE next_token;
(*Proceed to next token.  Ignore white space and comments.*)
VAR l: INTEGER;
BEGIN
  LOOP
    skip_spaces;
    IF (srclin[source_cursor] = '*') ANDIF (srclin[source_cursor] = ')')
    THEN error(err_unopened_comment);
    EXIT IF (srclin[source_cursor]<>'(') ANDIF (srclin[source_cursor+1]<>'*');
    scan_comments;
  END
END;

PROCEDURE scan_digits(digits: char_set);
(*Skip over digits in specified character set.*)
BEGIN
  source_cursor := source_cursor +
    VERIFY(SUBSTR(srclin, source_cursor), digits) - 1
END;

PROCEDURE scan_integer;
(*Scan an integer machine-word constant.*)
VAR radix: integer;
BEGIN
  source_cursor := source_cursor + 1;
  CASE srclin[source_cursor] OF
    'B' : radix := 2;
    'O': radix := 8;
    'D': radix := 10;
    'H': radix := 16;
    OTHERS: radix := 0
  END;
  IF radix = 0 THEN token.sym := badsymbol
  ELSE BEGIN
    token.sym := mwdconst;
    CASE radix OF
      2: scan_digits(['0', '1']);
      8: scan_digits(['0' .. '7']);
      10: scan_digits(['0' .. '9']);
      16: scan_digits(['0'..'9', 'A'..'F']);
    END;
    token.value := cstmwd(SUBSTR(srclin, token.column + 2,
      source_cursor - token.column - 2), radix, token.column)
  END
END;

$PAGE scan_number
PROCEDURE scan_number;
(*Scan an integer or real number and set up token.*)
VAR ndigits: INTEGER;
BEGIN
  token.sym := intconst; (*assume integer to start with*)
  scan_digits(['0' .. '9']); (*get integer part, if any*)
  ndigits := source_cursor - token.column;
  IF srclin[source_cursor] = 'B' THEN BEGIN (*octal constant: nnnnnnB*)
    token.value :=
      cstint(SUBSTR(line, token.column, ndigits), 8, token.column);
    source_cursor := source_cursor + 1
  END
  ELSE BEGIN (*decimal integer or real constant*)
    IF (line[source_cursor] = '.') (*check if there is a decimal point*)
	AND (line[source_cursor + 1] <> '.')
    THEN BEGIN (*but not an elipsis*)
      token.sym := realconst; source_cursor := source_cursor + 1;
      scan_digits(['0' .. '9']); (*scan fraction*)
      ndigits := source_cursor - token.column - 1
    END;
    IF line[source_cursor] = 'E' THEN BEGIN (*has an exponent field*)
      token.sym := realconst;
      source_cursor := source_cursor + 1;
      IF line[source_cursor] IN ['+', '-']
      THEN source_cursor := source_cursor + 1;
      scan_digits(['0' .. '9'])
    END;
    WITH token DO BEGIN
      IF sym = intconst
      THEN value := cstint(SUBSTR(line, column, ndigits), 10, column)
      ELSE value :=
        cstnum(SUBSTR(line, column, source_cursor - column), ndigits)
    END
  END
END;

$PAGE scan_keyword, scan_string
PROCEDURE scan_keyword;
(*Scan an itentifier or a reserved word.*)
EXCEPTION reserved_word_found;
VAR l: source_line_cursor; i: INTEGER; word: rwordtype;
BEGIN
  l := VERIFY(SUBSTR(srclin,source_cursor), ['A'..'Z','_','$','0'..'9']) - 1;
  source_cursor := source_cursor + l;
  IF l < UPPERBOUND(frw) THEN BEGIN (*It might be a reserved word.*)
    word := SUBSTR(srclin, source_cursor - l, l);
    FOR i := frw[l] TO frw[l + 1] - 1 DO IF rw[i] = word THEN SIGNAL(found)
  END;
  token.sym := ident;
  token.name := newnam(SUBSTR(srclin, source_cursor - l, l));
  EXCEPTION
  found: BEGIN
    token.sym := rsy[i];
    IF rop[i] <> noop THEN token.op := rop[i];
  END
END;

PROCEDURE scan_string;
(*Scan a character string constant.*)
VAR
l: source_line_cursor;
text: source_line_string;
BEGIN
  token.sym := stringconst;
  text := '';
  source_cursor := source_cursor + 1;
  REPEAT
    l := INDEX(SUBSTR(srclin, source_cursor), '''');
    IF l <> 0 THEN BEGIN (*Found a second quote.*)
      text := text || SUBSTR(litlin, source_cursor, l - 1);
      source_cursor := source_cursor + l
    END
    ELSE BEGIN (*Quote is unbalanced.*)
      token.sym := badsymbol;
      err_print(err_unbalanced_quote, cur_source, '', LENGTH(litlin));
    END;
    IF srclin[source_cursor] = ''''; THEN BEGIN (*Found double quote.*)
      text := text || '''';
      source_cursor := source_cursor + 1
    END
  UNTIL token.sym = badsymbol ORIF (srclin[source_cursor] <> '''');
  IF LENGTH(text) = 1 THEN token.value := cstsca(ORD(text[1]))
  ELSE token.value := cststr(text)
END;

$PAGE scan_dot, scan_colon, scan_relop
PROCEDURE scan_dot;
(*Scan elipsis, number or period.*)
BEGIN
  IF line[source_cursor+1] = '.' THEN BEGIN
    token.sym := elipsis;
    source_cursor := source_cursor + 2
  END
  ELSE IF line[source_cursor+1] IN ['0'..'9'] THEN scan_number
  ELSE BEGIN
    token.sym := period;
    source_cursor := source_cursor + 1
  END
END;

PROCEDURE scan_colon;
(*Scan a colon, ":" or ":=".*)
BEGIN
  source_cursor := source_cursor + 1;
  IF srclin[source_cursor] = '=' THEN BEGIN
    token.sym := becomes;
    source_cursor := source_cursor + 1
  END
  ELSE token.sym := colon
END;

PROCEDURE scan_relop;
(*Scan a relational operator "<", ">", "<>", ">=" or "<=".*)
BEGIN
  token.sym := relop;
  token.op := sop[srclin[source_cursor]];
  source_cursor := source_cursor + 1;
  IF srclin[source_cursor] = '=' THEN BEGIN
    IF token.op = ltop THEN token.op := leop ELSE token.op := geop;
    source_cursor := source_cursor + 1
  END
  ELSE IF (srclin[source_cursor] = '>') AND (token.op = ltop) THEN BEGIN
    token.op := neop; source_cursor := source_cursor + 1
  END
END;

$PAGE scan_star, scan_concatenation, scan_operator
PROCEDURE scan_star;
(*Scan a star which is mulop, "*", or powerop, "**".*)
BEGIN
  source_cursor := source_cursor + 1;
  IF line[source_cursor] = '*' THEN BEGIN
    token.sym := powerop;
    token.op := expon;
    source_cursor := source_cursor + 1
  END
  ELSE BEGIN
    token.sym := mulop;
    token.op := mul
  END
END;

PROCEDURE scan_concatenation;
(*Scan concatenation symbol "||".*)
BEGIN
  source_cursor := source_cursor + 1;
  IF line[source_cursor] = '|' THEN BEGIN
    token.sym := addop;
    token.op := catop;
    source_cursor := source_cursor + 1
  END
  ELSE token.sym := badsymbol
END;

PROCEDURE scan_operator;
(*Scan a simple operator via the ssy/sop tables.*)
BEGIN
  token.sym := ssy[srclin[source_cursor]];
  IF sop[srclin[source_cursor]] <> noop
  THEN token.op := sop[srclin[source_cursor]];
  source_cursor := source_cursor + 1
END;

$PAGE scan
PUBLIC FUNCTION scan: symbols;
(*Scan the next token from the source input.*)
BEGIN
  lsttkn := token;
  token.dummy := no;
  IF token.sym <> eofsy THEN BEGIN
    REPEAT
      next_token;
      token.column := source_cursor;
      token.length := 1;
      CASE srclin[source_cursor] OF
        'A'..'Z','_','$': scan_keyword;
        '''': scan_string;
        '0'..'9': scan_number;
	'#': scan_integer;
        '.': scan_dot;
        ':': scan_colon;
        '<','>': scan_relop;
        '*': scan_star;
        '|': scan_concatenation;
        OTHERS: scan_operator;
      END;
      IF token.sym = badsymbol
      THEN errprt(err_bad_token, token.source,
        SUBSTR(srclin, token.column, token.length), token.column)
    UNTIL token.sym <> badsymbol
  END;
  token.length := source_cursor - token.column;
  scan := token.sym
  EXCEPTION
  end_of_file: scan := token.sym
END.
   