$WIDTH=100
$LENGTH=55
$TITLE FMTPAS.PAS, last modified 10/17/83, zw

PROGRAM fmtpas;
(*PASCAL source file formatter*)

$SYSTEM pasutl

CONST
    stk_size = 1000;
    ind_quantum = 2;
    nul = chr(0); (* ascii nul *)
    dollar_flag = chr(1); (* ascii soh *)
    tab = chr(9); (* ascii ht *)
    cmt_brk = chr(10); (* ascii lf *)
    eol = chr(13); (* ascii cr *)
    eof_ch = chr(28); (* ascii fs *)
    sym_max_text = -500;
    max_text = 500;
    maxrw = 42;
    max_rw_len = 10;

TYPE
    string10 = string [10];
    text_index = 0 .. max_text;
    token_kind = ( beginsy, ofsy, loopsy, repeatsy, whilesy, endsy, untilsy,
      dosy, colonsy, exitsy, semicolonsy, thensy, elsesy, commentsy, lparensy,
	rparensy, ifsy, procedursy, functionsy, constsy, typesy, varsy, recordsy
	  , labelsy, commasy, publicsy, externalsy, staticsy, forwardsy,
	    externsy, fortransy, orsy, andsy, orifsy, andifsy, casesy, eofsy,
	      initprocsy, pascalsy, dollarsy, lbracketsy, rbracketsy, etc );
    space_codes = ( space, nospace, comment );
    stack_flags = ( one_flag, many_flag, then_flag );
    flag_set = set OF stack_flags;
    sym_index = sym_max_text .. max_text; (* Symmetric text index type. *)

$PAGE variables

VAR
    nsp: sym_index; (* Number of saved spaces. *)
    col: sym_index; (* Current output line column number. *)
    out_line: string [max_text]; (* Current output line. *)
    split_token: boolean; (* Can split line after current token. *)
    split_col: text_index; (* Latest split symbol location. *)
    cmt_indent: sym_index;
    (*  Variables for the output module.  *)
    spacing: space_codes;
    token_text: string [max_text];
    break_flag: boolean;
    cmt_column: text_index;
    cmt_level: 0 .. 10000;
    (*  Variables for the analyzer module.  *)
    this_token: token_kind;
    next_token: token_kind;
    (*  Variables just for us.  *)
    next_text: string [max_text];
    nspaces: text_index;
    nlines: 0 .. 10000;
    cur_column: text_index;
    saved_char: char;
    key_tokens: set OF token_kind := [procedursy,initprocsy,functionsy,labelsy,
      constsy,typesy, varsy, beginsy,staticsy,externalsy,publicsy,eofsy];
    stack: array [1..stk_size] OF packed RECORD
      flag: stack_flags;
      ind: text_index
    END;
    stack_ptr: 0 .. stk_size;
    indentation: text_index;
    stack_empty: boolean;
    nrw: array [1..11] OF 1..43 := ( 1, 2, 7, 11, 19, 26, 33, 36, 39, 41, 43 );
    rw: array [1..42] OF string [10] := ( '', 'OF', 'DO', 'IF', 'OR', '', 'END',
      'VAR', 'AND', '', 'LOOP', 'EXIT', 'THEN', 'ELSE', 'TYPE', 'ORIF', 'CASE',
	'', 'BEGIN', 'WHILE', 'UNTIL', 'CONST', 'LABEL', 'ANDIF', '', 'REPEAT',
	  'RECORD', 'PUBLIC', 'STATIC', 'EXTERN', 'PASCAL', '', 'FORWARD',
	    'FORTRAN', '', 'FUNCTION', 'EXTERNAL', '', 'PROCEDURE', '',
	      'INITPROCED', '' );
    rwsym: array [1..42] OF token_kind := ( etc, ofsy, dosy, ifsy, orsy, etc,
      endsy, varsy, andsy, etc, loopsy, exitsy, thensy, elsesy, typesy, orifsy,
	casesy, etc, beginsy, whilesy, untilsy, constsy, labelsy, andifsy, etc,
	  repeatsy, recordsy, publicsy, staticsy, externsy, pascalsy, etc,
	    forwardsy, fortransy, etc, functionsy, externalsy, etc, procedursy,
	      etc, initprocsy, etc );

$PAGE stkinit, mark, push, pop_while, pop_until

(*stkinit -- set up stack*)

PROCEDURE stkinit;

BEGIN
  indentation := 0;
  stack_ptr := 0;
  stack_empty := true;
END;

(*mark -- push flag onto stack*)

PROCEDURE mark(f: stack_flags);

BEGIN
  IF stack_ptr = stk_size THEN
    error('stack overflow')
  ELSE BEGIN
    stack_ptr := stack_ptr + 1;
    WITH stack[stack_ptr] DO BEGIN
      flag := f;
      ind := indentation
    END;
    stack_empty := false
  END
END;

(*push -- push flag, increment indentation*)

PROCEDURE push(f: stack_flags);

BEGIN
  mark(f);
  indentation := indentation + ind_quantum;
END;

(*pop_while -- pop stack while flag is at top of stack*)

PROCEDURE pop_while ( f: flag_set );

BEGIN
  WHILE (stack_ptr <> 0) ANDIF (stack[stack_ptr].flag in f) DO BEGIN
    indentation := stack[stack_ptr].ind;
    stack_ptr := stack_ptr - 1;
  END;
  stack_empty := (stack_ptr = 0)
END;

(*pop_until -- pop stack until flag is popped*)

PROCEDURE pop_until(f: flag_set);

BEGIN
  WHILE (stack_ptr <> 0) ANDIF NOT (stack[stack_ptr].flag IN f) DO
    stack_ptr := stack_ptr - 1;
  chkerr(stack_ptr = 0, 'stack underflow');
  indentation := stack[stack_ptr].ind;
  stack_ptr := stack_ptr - 1;
  stack_empty := (stack_ptr = 0)
END;

$PAGE put_string

(*put_string -- write string to OUTPUT file*)

PROCEDURE put_string ( s: string );

VAR
    len: text_index; (*len = length(s)*)
    i: text_index; (*i indexes through the string.*)
    ch: char; (*ch = s[i]*)
    rem: 0 .. 7; (*rem is the distance from the last tab stop.*)
    ctr: text_index;

BEGIN
  FOR i := 1 TO LENGTH(s) DO BEGIN
    ch := s[i];
    IF ch = ' ' THEN BEGIN
      IF nsp = col THEN
	nsp := nsp + 1
      ELSE
	out_line := out_line || ' ';
      col := col + 1
    END
    ELSE IF ch = eol THEN BEGIN
      col := indentation;
      nsp := indentation;
      break_flag := false;
      len := LENGTH(out_line);
      WHILE (len <> 0) ANDIF (out_line [len] = ' ') DO
	len := len - 1;
      WRITELN(OUTPUT, out_line: len);
      out_line := '';
      split_col := 0
    END
    ELSE IF ch = cmt_brk THEN BEGIN
      col := cmt_indent;
      nsp := cmt_indent;
      len := LENGTH(out_line);
      WHILE (len <> 0) ANDIF (out_line [len] = ' ') DO
	len := len - 1;
      WRITELN(OUTPUT, out_line: len);
      out_line := '';
      split_col := 0;
      split_token := false
    END
    ELSE IF ch <> eof_ch THEN BEGIN
      IF col < 0 THEN
	col := 0;
      IF nsp < 0 THEN
	nsp := 0;
      rem := col mod 8;
      IF (nsp = col) AND (nsp > rem) THEN BEGIN
	FOR ctr := 1 TO (nsp - rem + 7) DIV 8 DO
	  out_line := out_line || tab;
	nsp := rem
      END;
      FOR ctr := 1 TO nsp DO
	out_line := out_line || ' ';
      nsp := 0;
      col := col + 1;
      out_line := out_line || ch
    END
  END
END;

$PAGE new_line, break_line, get_char, put_back, lookup

(*new_line -- start a new line*)

PROCEDURE new_line;

BEGIN
  put_string(eol)
END;

(*break_line -- force next token to appear on a new line*)

PROCEDURE break_line;

BEGIN
  break_flag := true
END;

(*get_char -- retrieve next input character*)

FUNCTION get_char(VAR c: CHAR): CHAR;

BEGIN
  IF saved_char <> nul THEN BEGIN
    c := saved_char;
    saved_char := nul
  END
  ELSE IF EOF(INPUT) THEN
    c := eof_ch
  ELSE IF EOLN(INPUT) THEN BEGIN
    c := eol;
    READLN(INPUT);
    cur_column := 0
  END
  ELSE BEGIN
    READ(INPUT, c);
    cur_column := cur_column + 1;
    IF (c = '$') AND (cur_column = 1) THEN
      c := dollar_flag
  END;
  get_char := c;
END;

(*put_back -- put back character to be returned next by get_char*)

PROCEDURE put_back ( c: char );

BEGIN
  saved_char := c
END;

(*lookup -- return token kind of a reserved word*)

FUNCTION lookup ( s: string10 ): token_kind;

VAR
    i: 1 .. maxrw;
    len: 1 .. 10;

BEGIN
  len := length(s);
  IF len > max_rw_len THEN
    lookup := etc
  ELSE BEGIN
    rw[nrw[len + 1] - 1] := s;
    i := nrw[len];
    WHILE rw[i] <> s DO
      i := i + 1;
    lookup := rwsym[i];
  END
END;

$PAGE get_next

(*get_next -- scan the next token from the input*)

PROCEDURE get_next;

VAR
    build_buffer: packed array [1..max_text] OF char;
    count: text_index;
    c: char;

BEGIN
  nlines := 0;
  nspaces := 0;
  REPEAT
    IF get_char(c) = eol THEN
      nlines := nlines + 1
    ELSE IF c = ' ' THEN
      nspaces := nspaces + 1
  UNTIL NOT (c IN [eol, ' ']);
  next_text := c;
  CASE uppercase(c) OF
    'A'..'Z','_': BEGIN
      count := 1;
      build_buffer [1] := c;
      WHILE UPPERCASE(get_char(c)) in ['A'..'Z','0'..'9','_'] DO BEGIN
	count := count + 1;
	build_buffer[count] := c
      END;
      put_back (c);
      next_text := substr(build_buffer,1,count);
      next_token := lookup(uppercase(next_text))
    END;
    '0'..'9','.': BEGIN
      count := 1;
      build_buffer [1] := c;
      WHILE get_char(c) in ['0'..'9','.'] DO BEGIN
	count := count + 1;
	build_buffer[count] := c
      END;
      IF uppercase(c) = 'E' THEN BEGIN
	count := count + 1;
	build_buffer[count] := c;
	IF get_char(c) in ['+','-'] THEN BEGIN
	  count := count + 1;
	  build_buffer[count] := c
	END
	ELSE
	  put_back (c);
	WHILE get_char(c) in ['0'..'9','.'] DO BEGIN
	  count := count + 1;
	  build_buffer[count] := c
	END
      END;
      put_back(c);
      next_text := substr(build_buffer, 1, count);
      next_token := etc
    END;
    '''': BEGIN
      count := 0;
      REPEAT
	count := count + 1;
	build_buffer[count] := '''';
	REPEAT
	  count := count + 1;
	  build_buffer[count] := get_char(c);
	  chkerr(c = eol, 'eol in quated string')
	UNTIL (c = '''') OR err;
      UNTIL (get_char(c) <> '''') OR err;
      put_back(c);
      next_text := substr(build_buffer, 1, count);
      next_token := etc;
    END;

$PAGE
    ':': BEGIN
      IF get_char(c) = '=' THEN BEGIN
	next_text := ':=';
	next_token := etc
      END
      ELSE BEGIN
	next_token := colonsy;
	put_back(c)
      END
    END;
    ',':
      next_token := commasy;
    ';':
      next_token := semicolonsy;
    '(': BEGIN
      IF get_char(c) = '*' THEN BEGIN
	next_token := commentsy;
	cmt_column := cur_column - 2;
      END
      ELSE BEGIN
	next_token := lparensy;
	put_back(c);
      END
    END;
    ')':
      next_token := rparensy;
    '<': BEGIN
      next_token := etc;
      IF get_char(c) = '=' THEN
	next_text := '<='
      ELSE IF c = '>' THEN
	next_text := '<>'
      ELSE
	put_back(c);
    END;
    '>': BEGIN
      next_token := etc;
      IF get_char(c) = '=' THEN
	next_text := '>='
      ELSE
	put_back(c);
    END;
    '|': BEGIN
      next_token := etc;
      IF get_char(c) = '|' THEN
	next_text := '||'
      ELSE
	put_back(c);
    END;
    '[':
      next_token := lbracketsy;
    ']':
      next_token := rbracketsy;
    dollar_flag:
      next_token := dollarsy;
    eof_ch:
      next_token := eofsy;
    others:
      next_token := etc
  END;
  IF next_token in [commentsy, dollarsy] THEN
    spacing := comment
  ELSE IF (nspaces <> 0) OR (nlines <> 0) THEN
    spacing := space
  ELSE
    spacing := nospace;
END;

$PAGE getinit, get_token

(*getinit -- set up the input module*)

PROCEDURE getinit;

BEGIN
  saved_char := nul;
  cur_column := 0;
  cmt_level := 0;
  get_next
END;

(*get_token -- advance token input*)

PROCEDURE get_token;

BEGIN
  this_token := next_token;
  token_text := next_text;
  get_next;
END;

$PAGE get_comment

(*get_comment -- load next line of comment, if any, into text buffer*)

FUNCTION get_comment: BOOLEAN;

VAR
    build_buffer: packed array [1..max_text] OF char;
    count: text_index;
    c: char;
    i: text_index;


  PROCEDURE scan_comment;

  LABEL
      100; (*scan line*)

  BEGIN
    IF cmt_level = 0 THEN BEGIN
      count := 2;
      build_buffer[1:2] := '(*';
      cmt_level := 1;
    END
    ELSE
      count := 0;
    100: (*scan line*)
      WHILE cmt_level <> 0 DO BEGIN
      EXIT IF get_char(c) = eol;
	count := count + 1;
	build_buffer[count] := c;
	IF c = '(' THEN BEGIN
	  IF get_char(c) = '*' THEN BEGIN
	    count := count + 1;
	    build_buffer [count] := '*';
	    cmt_level := cmt_level + 1;
	  END
	  ELSE
	    put_back(c)
	END
	ELSE IF c = '*' THEN BEGIN
	  IF get_char(c) = ')' THEN BEGIN
	    count := count + 1;
	    build_buffer[count] := ')';
	    cmt_level := cmt_level - 1;
	  END
	  ELSE
	    put_back (c)
	END
	ELSE IF c = dollar_flag THEN
	  build_buffer[count] := '$'
	ELSE IF c = eof_ch THEN
	  error('eof in comment')
      END;
    IF cmt_level = 0 THEN BEGIN
      get_next;
      IF next_token in [commentsy,dollarsy] THEN BEGIN
	IF nlines = 0 THEN BEGIN
	  FOR i := count + 1 TO count + nspaces DO
	    build_buffer[i] := ' ';
	  build_buffer [count+nspaces+1:2] := '(*';
	  count := count + nspaces + 2;
	  cmt_level := 1;
	  GOTO 100 (*scan line*)
	END
	ELSE
	  nlines := nlines - 1;
      END
    END;
    token_text := substr(build_buffer, 1, count)
  END;

$PAGE

BEGIN (*get comment*)
  get_comment := (next_token IN [commentsy, dollarsy]);
  IF NOT get_comment THEN BEGIN
    FOR count := 1 TO nlines - 1 DO
      new_line;
    IF nlines <> 0 THEN
      break_line;
    nlines := 0
  END
  ELSE IF nlines <> 0 THEN BEGIN
    nlines := nlines - 1;
    token_text := ''
  END
  ELSE IF next_token = dollarsy THEN BEGIN
    count := 1;
    build_buffer[1] := '$';
    WHILE get_char(c) <> eol DO BEGIN
      count := count + 1;
      build_buffer[count] := c;
    END;
    get_next;
    token_text := SUBSTR(build_buffer, 1, count)
  END
  ELSE
    scan_comment
END;

$PAGE put_comment, putinit, put_token

(*put_comment -- output text of comment*)

PROCEDURE put_comment;

BEGIN
  IF next_token = commentsy THEN
    cmt_indent := indentation
  ELSE
    cmt_indent := 0;
  WHILE get_comment ANDIF (token_text = '') DO
    put_string(cmt_brk);
  IF cmt_level <> 0 THEN
    cmt_indent := col - cmt_column
  ELSE IF next_token = commentsy THEN
    cmt_indent := indentation
  ELSE
    cmt_indent := 0;
  put_string (token_text);
  WHILE get_comment AND NOT err DO BEGIN
    put_string(cmt_brk);
    put_string (token_text);
    IF cmt_level = 0 THEN BEGIN
      IF next_token = commentsy THEN
	cmt_indent := indentation
      ELSE
	cmt_indent := 0
    END
  END;
END;

(*putinit -- set up output module*)

PROCEDURE putinit;

BEGIN
  nsp := 0;
  col := 0;
  out_line := '';
  split_col := 0;
  break_flag := false;
  IF spacing = comment THEN
    put_comment;
END;

(*put_token -- write text of current token followed by spacing*)

PROCEDURE put_token;

VAR
    temp_out: string [max_text];
    i: text_index;
    split_token: boolean;

BEGIN
  IF break_flag THEN
    put_string(eol);
  IF col + length(token_text) > 79 THEN BEGIN
    IF split_col = 0 THEN
      split_col := length(out_line) + 1;
    temp_out := substr(out_line,split_col);
    out_line := substr(out_line,1,split_col-1);
    push(one_flag);
    put_string(eol);
    i := verify(temp_out,[' ']);
    IF i <> 0 THEN
      put_string(substr(temp_out,i));
  END;
  IF NOT (this_token IN [commentsy, etc]) THEN
    token_text := UPPERCASE(token_text);
  put_string (token_text);
  split_token := this_token IN [rparensy,semicolonsy,orsy,andsy,orifsy,andifsy];
  CASE spacing OF
    nospace:
      ;
    space:
      put_string (' ');
    comment: BEGIN
      put_string (' ');
      put_comment
    END
  END;
  IF split_token THEN
    split_col := length(out_line) + 1
END;

$PAGE format_hdr

(*format_hdr -- format procedure or function header*)

PROCEDURE format_hdr(ext_flag: boolean);

VAR
    plevel: text_index;

BEGIN
  plevel := 0;
  REPEAT
    get_token;
    CASE this_token OF
      lparensy: BEGIN
	plevel := plevel + 1;
	put_token
      END;
      rparensy: BEGIN
	chkerr(plevel = 0, 'extra right paren in header');
	plevel := plevel - 1;
	put_token;
      END;
      eofsy:
	error('eof in header');
      others:
	put_token
    END;
  UNTIL ((plevel = 0) AND (this_token = semicolonsy)) OR err;
  IF NOT err THEN BEGIN
    IF ext_flag ANDIF (next_token in [fortransy,pascalsy]) THEN
      REPEAT
	chkerr(next_token = eofsy, 'eof in header');
	get_token;
	put_token
      UNTIL (this_token = semicolonsy) OR err;
    pop_while([one_flag])
  END
END;

$PAGE format_labels, format_consts

(*format_labels -- format labels*)

PROCEDURE format_labels;

BEGIN
  get_token;
  put_token;
  push(one_flag);
  push(one_flag);
  break_line;
  REPEAT
    get_token;
    CASE this_token OF
      commasy: BEGIN
	put_token;
	break_line
      END;
      eofsy:
	error('eof in labels section');
      others:
	put_token
    END
  UNTIL (this_token = semicolonsy) OR err;
  pop_while([one_flag])
END;

(*format_consts -- cormat constants*)

PROCEDURE format_consts;

BEGIN
  get_token;
  put_token;
  push(then_flag);
  push(then_flag);
  break_line;
  WHILE NOT (next_token IN key_tokens) AND NOT err DO BEGIN
    get_token;
    put_token;
    IF this_token = semicolonsy THEN BEGIN
      pop_while([one_flag]);
      break_line
    END
  END;
  pop_while([then_flag])
END;

$PAGE format_decls

(*format_decls -- format declarations*)

PROCEDURE format_decls;

VAR
    case_level: 0 .. 100;
    plevel: array [0..100] OF text_index;
    case_flag: boolean;

BEGIN
  get_token;
  put_token;
  push(then_flag);
  push(then_flag);
  break_line;
  case_level := 0;
  plevel[0] := 0;
  case_flag := false;
  WHILE NOT (next_token IN key_tokens) DO BEGIN
    get_token;
    CASE this_token OF
      recordsy: BEGIN
	put_token;
	push (many_flag);
	break_line
      END;
      casesy: BEGIN
	put_token;
	case_flag := true
      END;
      ofsy: BEGIN
	put_token;
	IF case_flag THEN BEGIN
	  push(many_flag);
	  break_line;
	  case_level := case_level + 1;
	  plevel[case_level] := 0;
	  case_flag := false
	END;
      END;
      lparensy: BEGIN
	put_token;
	IF (case_level <> 0) ANDIF (plevel [case_level] = 0) THEN BEGIN
	  push(many_flag);
	  break_line
	END;
	plevel[case_level] := plevel[case_level] + 1;
      END;
      rparensy: BEGIN
	put_token;
	IF plevel[case_level] = 0 THEN BEGIN
	  pop_until ([many_flag]);
	  IF case_level <> 0 THEN
	    case_level := case_level - 1;
	END;
	plevel[case_level] := plevel[case_level] - 1;
	IF (case_level <> 0) ANDIF (plevel[case_level] = 0) THEN
	  pop_until([many_flag]);
      END;
      endsy: BEGIN
	IF case_level <> 0 THEN
	  pop_until ([many_flag]);
	pop_until ([many_flag]);
	break_line;
	put_token;
	case_level := 0;
	plevel [0] := 0;
      E      semicolonsy: BEGIN
	put_token;
	pop_while ([one_flag]);
	break_line
      END;
      others:
	put_token
    END
  END;
  pop_while([then_flag]);
END;

$PAGE format_body

(*format_body -- format procedure, function or program up to 'end'*)

PROCEDURE format_body;

VAR
    begin_level: text_index;
    plevel: text_index;

BEGIN
  begin_level := 0;
  plevel := 0;
  REPEAT
    get_token;
    CASE this_token OF
      beginsy, ofsy, loopsy, repeatsy: BEGIN
	put_token;
	push(many_flag);
	break_line;
	begin_level := begin_level + 1
      END;
      endsy, untilsy: BEGIN
	pop_until([many_flag]);
	break_line;
	put_token;
	begin_level := begin_level - 1
      END;
      dosy, colonsy: BEGIN
	put_token;
	IF (next_token <> beginsy) AND (plevel = 0) THEN BEGIN
	  push(one_flag);
	  break_line
	END;
      END;
      exitsy: BEGIN
	pop_until([many_flag]);
	put_token;
	push(many_flag)
      END;
      semicolonsy: BEGIN
	put_token;
	pop_while ([one_flag,then_flag]);
	break_line
      END;
      thensy: BEGIN
	put_token;
	IF next_token = beginsy THEN
	  mark (then_flag)
	ELSE BEGIN
	  push(then_flag);
	  break_line
	END
      END;
      elsesy: BEGIN
	pop_until ([then_flag]);
	break_line;
	put_token;
	IF not (next_token in [ifsy,beginsy]) THEN BEGIN
	  push (one_flag);
	  break_line
	END;
      END;
      lparensy, lbracketsy: BEGIN
	put_token;
	plevel := plevel + 1
      END;
      rparensy, rbracketsy: BEGIN
	put_token;
	IF plevel <> 0 THEN
	  plevel := plevel - 1
      END;
      eofsy:
	error('eof in procedure/function/program body');
      others:
	put_token
    END;
  UNTIL (begin_level = 0) OR err;
  get_token;
  put_token
END;

$PAGE format, main block

(*format -- examine keys, invoke routines to format various parts*)

PROCEDURE format;

VAR
    proc_level: text_index;
    no_break_flag: boolean;
    ext_flag: boolean;

BEGIN
  proc_level := 0;
  no_break_flag := false;
  ext_flag := false;
  WHILE (next_token <> eofsy) AND NOT err DO BEGIN
    IF not no_break_flag THEN BEGIN
      new_line;
      new_line;
    END;
    CASE next_token OF
      procedursy, initprocsy, functionsy: BEGIN
	IF not ext_flag THEN BEGIN
	  IF proc_level <> 0 THEN BEGIN
	    push (many_flag);
	    break_line;
	  END;
	  proc_level := proc_level + 1;
	END;
	format_hdr(ext_flag);
	ext_flag := false;
	no_break_flag := false;
      END;
      labelsy: BEGIN
	format_labels;
	ext_flag := false;
	no_break_flag := false;
      END;
      constsy: BEGIN
	format_consts;
	ext_flag := false;
	no_break_flag := false;
      END;
      typesy, varsy: BEGIN
	format_decls;
	ext_flag := false;
	no_break_flag := false;
      END;
      beginsy: BEGIN
	format_body;
	IF proc_level <> 0 THEN
	  proc_level := proc_level - 1;
	IF proc_level <> 0 THEN
	  pop_until([many_flag]);
	ext_flag := false;
	no_break_flag := false;
      END;
      semicolonsy: BEGIN
	get_token;
	put_token;
	no_break_flag := false;
	break_line;
      END;
      others: BEGIN
	get_token;
	put_token;
	IF this_token = externalsy THEN
	  ext_flag := true
	ELSE IF this_token in [forwardsy,externsy] THEN BEGIN
	  IF proc_level <> 0 THEN
	    proc_level := proc_level - 1;
	  IF proc_level <> 0 THEN
	    pop_until ([many_flag]);
	END;
	no_break_flag := true;
      END
    END
  END;
  new_line;
  chkerr(proc_level <> 0, 'incomplete function or procedure')
END;

$PAGE main block

BEGIN
  WHILE start('FMTPAS', '') DO BEGIN
    getinit;
    stkinit;
    putinit;
    format
  END
END.
@
¨