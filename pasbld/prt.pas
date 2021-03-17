$TITLE prt.pas, last modified 5/8/84, zw
PROGRAM pretty;
(*TYM-Pascal Pretty Printer*)
$HEADER PRT.HDR

$SYSTEM VERSIO.INC
$INCLUDE PRTRW.TYP

CONST
nul = CHR(0);
dollar_flag = CHR(1);
tab = CHR(9);
cmt_brk = CHR(10);
eol = CHR(13);
eof_ch = CHR(28);
stack_size = 500;
ind_quantum = 2;
max_text = 500;
output_line_size = 78;

TYPE
text_index = 0 .. max_text;
sym_index = -max_text .. max_text; (*symmetric text index type.*)
space_codes = ( space, nospace, comment );
stack_flags = ( one_flag, many_flag, then_flag );
flag_set = SET OF stack_flags;
string20 = STRING[20];

VAR
(*variables for the output module.*)
spacing: space_codes;
token_text: STRING[max_text];
comment_column: text_index;
comment_level: 0 .. 1000;
(*variables for the analyzer module.*)
this_token: token_kind;
next_token: token_kind;
(*variables just for us.*)
next_text: STRING[max_text];
nspaces: text_index;
nlines: INTEGER;
cur_column: text_index;
saved_char: CHAR;
stack: ARRAY[1..stack_size] OF PACKED RECORD
  flag: stack_flags;
  ind: text_index
END;
stack_ptr: 0 .. stack_size;
indentation: text_index;
stack_empty: BOOLEAN;
nsp: sym_index; (*number of saved spaces.*)
col: sym_index; (*current output line column number.*)
comment_indent: sym_index;
out_line: STRING[max_text]; (*current output line.*)
split_token: BOOLEAN; (*can split line after current token.*)
split_col: text_index; (*latest split symbol location.*)
break_flag: BOOLEAN;
key_tokens: SET OF token_kind := [proceduresy,functionsy,labelsy, constsy,
  typesy, varsy, beginsy,staticsy,externalsy,publicsy,endfsy];

LABEL
100; (*restart*)

PROCEDURE error( msg: STRING );
BEGIN
  WRITELN(TTY,msg);
  GOTO 100
END;

PROCEDURE put_string( s: STRING );
(*put_string is used to write a string to the output file. spaces
written to the output file are replaced by tabs where appropriate,
and are deleted at the ends of lines.*)
VAR
len: text_index; (*len = length(s)*)
i: text_index; (*i indexes through the string.*)
ch: CHAR; (*ch = s[i]*)
rem: 0 .. 7; (*rem is the distance from the last tab stop.*)
ctr: text_index;

  PROCEDURE output_line(indentation: text_index);
  BEGIN
    len := LENGTH(out_line);
    WHILE(len <> 0) ANDIF(out_line[len] = ' ') DO len := len - 1;
    WRITELN(OUTPUT,out_line:len);
    out_line := '';
    split_col := 0;
    col := indentation;
    nsp := indentation
  END;
BEGIN
  FOR i := 1 TO LENGTH(s) DO BEGIN
    ch := s[i];
    IF ch = ' ' THEN BEGIN
      IF nsp = col THEN nsp := nsp + 1
      ELSE out_line := out_line || ' ';
      col := col + 1
    END
    ELSE IF ch = eol THEN BEGIN
      output_line(indentation);
      break_flag := FALSE
    END
    ELSE IF ch = cmt_brk THEN BEGIN
      output_line(comment_indent);(*???*)
      split_token := FALSE
    END
    ELSE IF ch <> eof_ch THEN BEGIN
      IF col < 0 THEN col := 0;
      IF nsp < 0 THEN nsp := 0;
      rem := col MOD 8;
      IF(nsp = col) AND(nsp > rem) THEN BEGIN
	FOR ctr := 1 TO(nsp - rem + 7) DIV 8 DO out_line := out_line || tab;
	nsp := rem
      END;
      FOR ctr := 1 TO nsp DO out_line := out_line || ' ';
      nsp := 0;
      col := col + 1;
      out_line := out_line || ch
    END
  END
END;

PROCEDURE new_line;
(*new_line begins a new output line.*)
BEGIN
  put_string(eol)
END;

PROCEDURE break_line;
(*break_line forces the next token to appear on a new line.*)
BEGIN
  break_flag := TRUE
END;

FUNCTION get_char( VAR c: CHAR ): CHAR;
(*get_char will return the next input character, both as its return
value and in its parameter. if a character has been saved by put_back,
then that character will be returned; otherwise, a character will be
read.*)
BEGIN
  IF saved_char <> nul THEN BEGIN
    c := saved_char;
    saved_char := nul
  END
  ELSE IF EOF(INPUT) THEN c := eof_ch
  ELSE IF EOLN(INPUT) THEN BEGIN
    c := eol;
    READLN(INPUT);
    cur_column := 0
  END
  ELSE BEGIN
    READ(INPUT,c);
    cur_column := cur_column + 1;
    IF(c = '$') AND(cur_column = 1) THEN c := dollar_flag
  END;
  get_char := c
END;

PROCEDURE put_back( c: CHAR );
(*put_back will save its parameter character in the saved character
buffer, so that it can be returned by get_char again.*)
BEGIN
  saved_char := c
END;

FUNCTION lookup( s: string20 ): token_kind;
(*lookup returns the token kind of a reserved word, or 'etc' for
anything else.*)
VAR
i: 1 .. maxrw;
len: 1 .. 20;
BEGIN
  len := LENGTH(s);
  IF len > max_rw_len THEN BEGIN
    lookup := etc;
    RETURN
  END;
  rw[nrw[len+1]-1] := s;
  i := nrw[len];
  WHILE rw[i] <> s DO i := i + 1;
  lookup := rwsym[i]
END;

PROCEDURE get_next;
(*get_next will pick up the next token from the input, setting next_text,
next_token, and spacing. if the next input token is a comment, then
nlines will be the number of new-lines preceding it.*)
VAR
build_buffer: PACKED ARRAY[1..max_text] OF CHAR;
count: text_index;
c: CHAR;
LABEL
100;

  PROCEDURE get_identifier;
  BEGIN
    count := 1;
    build_buffer[1] := c;
    WHILE UPPERCASE(get_char(c)) IN ['A'..'Z','0'..'9','_'] DO BEGIN
      count := count + 1;
      build_buffer[count] := c
    END;
    put_back(c);
    next_text := SUBSTR(build_buffer,1,count);
    next_token := lookup(UPPERCASE(next_text));
    IF next_token <> etc THEN next_text := UPPERCASE(next_text)
    ELSE next_text := LOWERCASE(next_text)
  END;

  PROCEDURE get_number;
  BEGIN
    count := 1;
    build_buffer[1] := c;
    WHILE get_char(c) IN ['0'..'9','.'] DO BEGIN
      count := count + 1;
      build_buffer[count] := c
    END;
    IF UPPERCASE(c) = 'e' THEN BEGIN
      count := count + 1;
      build_buffer[count] := c;
      IF get_char(c) IN ['+','-'] THEN BEGIN
	count := count + 1;
	build_buffer[count] := c
      END
      ELSE put_back(c);
      WHILE get_char(c) IN ['0'..'9','.'] DO BEGIN
	count := count + 1;
	build_buffer[count] := c
      END
    END;
    put_back(c);
    next_text := SUBSTR(build_buffer,1,count);
    next_token := etc
  END;

  PROCEDURE get_string;
  BEGIN
    count := 0;
    REPEAT
      count := count + 1;
      build_buffer[count] := '''';
      REPEAT
	count := count + 1;
	build_buffer[count] := get_char(c);
	IF c = eol THEN error('end-of-line in quoted string');
      UNTIL c = '''';
    UNTIL get_char(c) <> '''';
    put_back(c);
    next_text := SUBSTR(build_buffer,1,count);
    next_token := etc
  END;

  PROCEDURE get_colon;
  BEGIN
    IF get_char(c) = '=' THEN BEGIN
      next_text := ':=';
      next_token := etc
    END
    ELSE BEGIN
      next_token := colonsy;
      put_back(c)
    END
  END;

  PROCEDURE get_paren;
  BEGIN
    IF get_char(c) = '*' THEN BEGIN
      next_token := commentsy;
      comment_column := cur_column - 2
    END
    ELSE BEGIN
      next_token := lparensy;
      put_back(c)
    END
  END;

  PROCEDURE get_less;
  BEGIN
    next_token := etc;
    IF get_char(c) = '=' THEN next_text := '<='
    ELSE IF c = '>' THEN next_text := '<>'
    ELSE put_back(c)
  END;

  PROCEDURE get_greater;
  BEGIN
    next_token := etc;
    IF get_char(c) = '=' THEN next_text := '>='
    ELSE put_back(c)
  END;

  PROCEDURE get_concat;
  BEGIN
    next_token := etc;
    IF get_char(c) = '|' THEN next_text := '||'
    ELSE put_back(c)
  END;
BEGIN
  nlines := 0;
  nspaces := 0;
  LOOP (*find a non-blank character.*)
    IF get_char(c) = eol THEN nlines := nlines + 1
    ELSE IF c = ' ' THEN nspaces := nspaces + 1
    ELSE GOTO 100
  END;
  100: next_text := c;
  CASE UPPERCASE(c) OF
    'A'..'Z','_': get_identifier;
    '0'..'9','.': get_number;
    '''': get_string;
    ':': get_colon;
    ',': next_token := commasy;
    ';': next_token := semicolonsy;
    '(': get_paren;
    ')': next_token := rparensy;
    '<': get_less;
    '>': get_greater;
    '|': get_concat;
    '[': next_token := lbracketsy;
    ']': next_token := rbracketsy;
    dollar_flag: next_token := dollarsy;
    eof_ch: next_token := endfsy;
    OTHERS: next_token := etc
  END;
  IF next_token IN [commentsy,dollarsy] THEN spacing := comment
  ELSE IF(nspaces <> 0) OR(nlines <> 0) THEN spacing := space
  ELSE spacing := nospace
END;

FUNCTION get_comment: BOOLEAN;
(*get_comment will load the next line of comment text into the
token text buffer. if there is no more comment text, it will
return false.*)
VAR
build_buffer: PACKED ARRAY[1..max_text] OF CHAR;
count: text_index;
c: CHAR;
i: text_index;
LABEL
100; (*scan line*)
BEGIN
  get_comment :=(next_token IN [commentsy,dollarsy]);
  IF NOT get_comment THEN BEGIN
    IF nlines <> 0 THEN break_line;
    nlines := 0;
    RETURN
  END;
  IF nlines <> 0 THEN BEGIN
    nlines := 0;
    token_text := '';
    RETURN
  END;
  IF next_token = dollarsy THEN BEGIN
    count := 1;
    build_buffer[1] := '$';
    WHILE NOT (get_char(c) IN [eol, ' ']) DO BEGIN
      count := count + 1;
      build_buffer[count] := c
    END;
    build_buffer := UPPERCASE(build_buffer);
    IF c = ' ' THEN REPEAT
      count := count + 1;
      build_buffer[count] := c
    UNTIL get_char(c)= eol;
    get_next;
    token_text := SUBSTR(build_buffer,1,count);
    RETURN
  END;
  IF comment_level = 0 THEN BEGIN (*first line of this comment.*)
    count := 2;
    build_buffer[1:2] := '(*';
    comment_level := 1
  END
  ELSE count := 0; (*subsequent line of this comment.*)
  100: (*scan line*)
  WHILE comment_level <> 0 DO BEGIN
    EXIT IF get_char(c) = eol;
    count := count + 1;
    build_buffer[count] := c;
    IF c = '(' THEN BEGIN
      IF get_char(c) = '*' THEN BEGIN
        count := count + 1;
        build_buffer[count] := '*';
        comment_level := comment_level + 1
      END
      ELSE put_back(c)
    END
    ELSE IF c = '*' THEN BEGIN
      IF get_char(c) = ')' THEN BEGIN
        count := count + 1;
        build_buffer[count] := ')';
        comment_level := comment_level - 1
      END
      ELSE put_back(c)
    END
    ELSE IF c = dollar_flag THEN build_buffer[count] := '$'
    ELSE IF c = eof_ch THEN error('eof in comment')
  END;
  IF comment_level = 0 THEN BEGIN
    get_next;
    IF next_token IN [commentsy,dollarsy] THEN IF nlines = 0 THEN BEGIN
      FOR i := count+1 TO count+nspaces DO build_buffer[i] := ' ';
      build_buffer[count+nspaces+1:2] := '(*';
      count := count + nspaces + 2;
      comment_level := 1;
      GOTO 100
    END
    ELSE nlines := 0
  END;
  token_text := SUBSTR(build_buffer,1,count)
END;

PROCEDURE get_token;
(*get_token is the primary input routine. it copies the data about the
next token(if there is any) for the current token, and then gets data
for the next token.*)
BEGIN
  this_token := next_token;
  token_text := next_text;
  get_next
END;

PROCEDURE mark( f: stack_flags );
(*mark will push a specified flag on top of the stack.*)
BEGIN
  IF stack_ptr = stack_size THEN error('overflow: program too complicated');
  stack_ptr := stack_ptr + 1;
  WITH stack[stack_ptr] DO BEGIN
    flag := f;
    ind := indentation
  END;
  stack_empty := FALSE
END;

PROCEDURE push( f: stack_flags );
(*push is the same as mark, except that it also increments the
current indentation level.*)
BEGIN
  mark(f);
  indentation := indentation + ind_quantum
END;

PROCEDURE pop_while( f: flag_set );
(*pop_while will pop the stack as long as a specified flag is at the
top of the stack.*)
BEGIN
  WHILE(stack_ptr <> 0) ANDIF(stack[stack_ptr].flag IN f) DO BEGIN
    indentation := stack[stack_ptr].ind;
    stack_ptr := stack_ptr - 1
  END;
  stack_empty :=(stack_ptr = 0)
END;

PROCEDURE pop_until( f: flag_set );
(*pop_until will pop the stack until a specified symbol is popped
off the top of the stack.*)
BEGIN
  WHILE(stack_ptr <> 0) ANDIF NOT(stack[stack_ptr].flag IN f)
    DO stack_ptr := stack_ptr - 1;
  IF stack_ptr = 0 THEN error('underflow: bad program syntax');
  indentation := stack[stack_ptr].ind;
  stack_ptr := stack_ptr - 1;
  stack_empty :=(stack_ptr = 0)
END;

PROCEDURE put_comment;
(*put_comment will output the entire text of a comment.*)
BEGIN
  IF next_token = commentsy THEN comment_indent := indentation (*comment*)
  ELSE comment_indent := 0; (*dollar line*)
  WHILE get_comment ANDIF(token_text = '') DO put_string(cmt_brk);
(*???  IF comment_level <> 0 THEN comment_indent := col - comment_column
  ELSE IF next_token = commentsy THEN comment_indent := indentation
  ELSE comment_indent := 0; *)
  IF next_token = commentsy THEN BEGIN
    IF comment_level <> 0 THEN comment_indent := col - comment_column
    ELSE comment_indent := indentation
  END
  ELSE comment_indent := 0; (*dollar line*)
  put_string(token_text);
  WHILE get_comment DO BEGIN
    put_string(cmt_brk);
    put_string(token_text);
    IF comment_level = 0 THEN BEGIN
      IF next_token = commentsy THEN comment_indent := indentation (*comment*)
      ELSE comment_indent := 0; (*dollar line*)
    END
  END
END;

PROCEDURE put_token;
(*put_token writes the text of the current token to the output, and
follows it with the appropriate spacing. depending on the context,
this may be nothing, a single space, or a complete comment.*)
VAR
temp_out: STRING[max_text];
i: text_index;
split_token: BOOLEAN;
BEGIN
  IF break_flag THEN put_string(eol);
  IF col + LENGTH(token_text) > output_line_size THEN BEGIN
    IF split_col = 0 THEN split_col := LENGTH(out_line) + 1;
    temp_out := SUBSTR(out_line,split_col);
    out_line := SUBSTR(out_line,1,split_col-1);
    push(one_flag);
    put_string(eol);
    pop_until([one_flag]);
    i := VERIFY(temp_out,[' ']);
    IF i <> 0 THEN put_string(SUBSTR(temp_out,i))
  END;
  put_string(token_text);
  split_token := (this_token IN [rparensy,semicolonsy,orsy,andsy,orifsy,
    andifsy]) ;
  CASE spacing OF
    nospace:; (*no spacing*)
    space: put_string(' ');
    comment: BEGIN
      put_string(' ');
      put_comment
    END
  END;
  IF split_token THEN split_col := LENGTH(out_line) + 1
END;

PROCEDURE format_hdr( ext_flag: BOOLEAN );
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
	IF plevel = 0 THEN error('extra right paren in header');
	plevel := plevel - 1;
	put_token
      END;
      endfsy: error('eof in procedure or function header');
      OTHERS: put_token
    END;
  UNTIL(plevel = 0) AND(this_token = semicolonsy);
  IF ext_flag ANDIF(next_token = fortransy) THEN REPEAT
    IF next_token = endfsy THEN error('eof in procedure or function header');
    get_token;
    put_token;
  UNTIL this_token = semicolonsy;
  pop_while([one_flag])
END;

PROCEDURE format_labels;
BEGIN
  get_token;
  put_token;
  mark(one_flag);
  mark(one_flag);
  break_line;
  REPEAT
    get_token;
    CASE this_token OF
      commasy: BEGIN
	put_token;
	break_line
      END;
      endfsy: error('eof in labels section');
      OTHERS: put_token
    END;
  UNTIL this_token = semicolonsy;
  pop_while([one_flag])
END;

PROCEDURE format_consts;
BEGIN
  get_token;
  put_token;
  mark(then_flag);
  mark(then_flag);
  break_line;
  WHILE NOT(next_token IN key_tokens) DO BEGIN
    get_token;
    put_token;
    IF this_token = semicolonsy THEN BEGIN
      pop_while([one_flag]);
      break_line
    END
  END;
  pop_while([then_flag])
END;

PROCEDURE format_decls;
VAR
case_level: 0 .. 100;
plevel: ARRAY[0..100] OF text_index;
case_flag: BOOLEAN;
BEGIN
  get_token;
  put_token;
  mark(then_flag);
  mark(then_flag);
  break_line;
  case_level := 0;
  plevel[0] := 0;
  case_flag := FALSE;
  WHILE NOT(next_token IN key_tokens) DO BEGIN
    get_token;
    CASE this_token OF
      recordsy: BEGIN
	put_token;
	push(many_flag);
	break_line
      END;
      casesy: BEGIN
	put_token;
	case_flag := TRUE
      END;
      ofsy: BEGIN
	put_token;
	IF case_flag THEN BEGIN
	  push(many_flag);
	  break_line;
	  case_level := case_level + 1;
	  plevel[case_level] := 0;
	  case_flag := FALSE
	END
      END;
      lparensy: BEGIN
	put_token;
	IF(case_level <> 0) ANDIF(plevel[case_level] = 0) THEN BEGIN
	  push(many_flag);
	  break_line
	END;
	plevel[case_level] := plevel[case_level] + 1
      END;
      rparensy: BEGIN
	put_token;
	IF plevel[case_level] = 0 THEN BEGIN
	  pop_until([many_flag]);
	  IF case_level <> 0 THEN case_level := case_level - 1
	END;
	plevel[case_level] := plevel[case_level] - 1;
	IF(case_level <> 0) ANDIF(plevel[case_level] = 0)
	  THEN pop_until([many_flag])
      END;
      endsy: BEGIN
	IF case_level <> 0 THEN pop_until([many_flag]);
	pop_until([many_flag]);
	break_line;
	put_token;
	case_level := 0;
	plevel[0] := 0
      END;
      semicolonsy: BEGIN
	put_token;
	pop_while([one_flag]);
	break_line
      END;
      OTHERS: put_token
    END
  END;
  pop_while([then_flag])
END;

PROCEDURE format_body;
(*format_body is called when the next symbol is a (zero-level)
begin. it performs the formatting of the body of a procedure
or program, up through the symbol following the matching (zero-
level) end.*)
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
      semicolonsy: BEGIN
	put_token;
	pop_while([one_flag,then_flag]);
	break_line
      END;
      thensy: BEGIN
	put_token;
	mark(then_flag)
      END;
      elsesy: BEGIN
	pop_until([then_flag]);
	break_line;
	put_token;
      END;
      lparensy, lbracketsy: BEGIN
	put_token;
	plevel := plevel + 1
      END;
      rparensy, rbracketsy: BEGIN
	put_token;
	IF plevel <> 0 THEN plevel := plevel - 1
      END;
      endfsy: error('eof in procedudy');
      OTHERS: put_token
    END
  UNTIL begin_level = 0;
  get_token;
  put_token
END;

PROCEDURE format;
(*format examines selected key words and invokes the primary formatting
routines to format the various declaration and statement parts of
the program.*)
VAR
proc_level: text_index;
no_break_flag: BOOLEAN;
ext_flag: BOOLEAN;
BEGIN
  proc_level := 0;
  no_break_flag := FALSE;
  ext_flag := FALSE;
  WHILE next_token <> endfsy DO BEGIN
    IF NOT no_break_flag THEN new_line;
    CASE next_token OF
      proceduresy, functionsy: BEGIN
	IF NOT ext_flag THEN BEGIN
	  IF proc_level <> 0 THEN push(many_flag);
	  proc_level := proc_level + 1
	END;
	break_line;
	format_hdr(ext_flag);
	ext_flag := FALSE;
	no_break_flag := FALSE
      END;
      labelsy: BEGIN
	IF (proc_level = 0) AND NOT no_break_flag THEN new_line;
	format_labels;
	ext_flag := FALSE;
	no_break_flag := FALSE
      END;
      constsy: BEGIN
	IF (proc_level = 0) AND NOT no_break_flag THEN new_line;
	format_consts;
	ext_flag := FALSE;
	no_break_flag := FALSE
      END;
      typesy, varsy: BEGIN
	IF (proc_level = 0) AND NOT no_break_flag THEN new_line;
	format_decls;
	ext_flag := FALSE;
	no_break_flag := FALSE
      END;
      beginsy: BEGIN
	IF (proc_level = 0) AND NOT no_break_flag THEN new_line;
	format_body;
	IF proc_level <> 0 THEN proc_level := proc_level - 1;
	IF proc_level <> 0 THEN pop_until([many_flag]);
	ext_flag := FALSE;
	no_break_flag := FALSE
      END;
      semicolonsy: BEGIN
	get_token;
	put_token;
	no_break_flag := FALSE;
	break_line
      END;
      OTHERS: BEGIN
	get_token;
	put_token;
	IF this_token = externalsy THEN ext_flag := TRUE
	ELSE IF this_token = forwardsy THEN BEGIN
	  IF proc_level <> 0 THEN proc_level := proc_level - 1;
	  IF proc_level <> 0 THEN pop_until([many_flag])
	END;
	no_break_flag := TRUE
      END
    END
  END;
  new_line;
  IF proc_level <> 0 THEN error('incomplete function or procedure')
END;

PROCEDURE getiofiles;
(*getiofiles is a routine for reading command lines. it assumes
that the calling program does input and output on the standard
files input and output, that the command lines have the format:
<output file>=<input file>
and that an empty command line indicates a request to terminate
the program.*)
VAR
line: STRING[80];
ind: 0 .. 80;
LABEL
100;
BEGIN
  100: WRITE(TTY,'*');
  BREAK;
  READLN(TTY);
  READ(TTY, line);
  IF line = '' THEN STOP;
  ind := SEARCH(line,['=']);
  IF ind = 0 THEN BEGIN
    WRITELN(TTY,'<output file>=<input file>');
    GOTO 100
  END;
  RESET(INPUT,'.PAS '||SUBSTR(line,ind+1));
  IF EOF(INPUT) THEN BEGIN
    WRITELN(TTY,'file ',SUBSTR(line,ind+1),' empty or missing');
    GOTO 100
  END;
  REWRITE(OUTPUT,'.PAS '||SUBSTR(line,1,ind-1));
  IF NOT EOF(OUTPUT) THEN BEGIN
    WRITELN(TTY,'bad output file ',SUBSTR(line,1,ind-1));
    GOTO 100
  END
END;

PROCEDURE putinit;
BEGIN
  nsp := 0;
  col := 0;
  out_line := '';
  split_col := 0;
  break_flag := FALSE;
  IF spacing = comment THEN put_comment
END;

PROCEDURE stackinit;
BEGIN
  indentation := 0;
  stack_ptr := 0;
  stack_empty := TRUE
END;

PROCEDURE getinit;
BEGIN
  saved_char := nul; (*no characters saved by put_back.*)
  cur_column := 0;
  comment_level := 0;
  get_next; (*get the first token or comment.*)
END;

BEGIN
  REWRITE(TTY);
  OPEN(TTY);
  WRITELN(TTY,'TYM-Pascal Pretty Printer, Version ', version());
  WRITELN(TTY);
  100: LOOP
    getiofiles;
    getinit;
    stackinit;
    putinit;
    format;
    CLOSE(OUTPUT)
  END
END.
p@	LÑ