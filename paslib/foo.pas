$WIDTH=100
$LENGTH=55
$TITLE FOO.PAS, last modified 12/19/83, zw
PROGRAM foo;
(*friendly on-line organizer*)

(*this is a test of TYM-Pascal features*)

$HEADER FOO.HDR

$PAGE exception declarations

EXCEPTION
abort, (*general error condition*)
done, (*program exit*)
fatal; (*fatal error condition*)
$ENABLE trap_abort
$DISABLE trap_fatal
$DISABLE trap_attention

$PAGE constant declarations

CONST
foo_banner = 'friendly on-line organizer, version 1.0';
foo_prompt = 'ready for command';
number_of_foo_command_options = 16; number_of_foo_command_routines = 14;
default_page_size = 66; (*tty forms*)
default_line_size = 80; (*tty forms*)
default_wrap_position = -1; (*right wrap*)
default_left_margin = 0;
default_right_margin = 0;
default_top_margin = 0;
default_bottom_margin = 0;
turn_lpt_on = CHR(27) || CHR(91) || CHR(53) || CHR(105); (*VT100/LA50*)
turn_lpt_off = CHR(27) || CHR(91) || CHR(52) || CHR(105); (*VT100/LA50*)
temp_name = '###FOO.TMP';
output_line_size = 132; (*maximum number of characters per output line*)
output_page_size = 132; (*maximum number of lines per output page*)
command_line_size = 80; (*maximum number of characters per command line*)
option_word_size = 20; (*maximum number of characters in an option word*)
terminal_name = 'TTY:'; (*file name for the terminal*)
printer_name = 'LPT:'; (*file name for the printer*)
temp_name = 'TMP:'; (*file name for the general purpose temporary file*)
null_name = 'NUL:'; (*file name for the null file*)
terminal_end_of_file = 'EOF'; (*signals end of terminal input*)
terminal_fatal_error = 'ABORT'; (*signals fatal condition from terminal input*)
default_nset: (*numeric characters for token scanner*)
  SET OF CHAR = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
default_delimiter_set: (*delimiter characters for token scanner*)
  SET OF CHAR = [' ', CHR(9)];
default_quote_set: (*quote characters for token scanner*)
  SET OF CHAR = ['''', '"'];
default_comment_set: (*comment characters for token scanner*)
  SET OF CHAR = ['$', '!', ';'];
default_operator_set: (*operator characters for token scanner*)
  SET OF CHAR = [',', '.', '/', '?', ';', ':', '\', '|',
  '[', ']', '{', '}', '=', '+', '-', ')', '(', '*', '&', '^', '%',
  '$', '#', '@', '!'];

$PAGE type declarations

TYPE
positive_integer = 0 .. MAXIMUM(INTEGER);
ordinal_integer = 1 .. MAXIMUM(positive_integer);
command_line = STRING[command_line_size];
output_line = STRING[output_line_size];
output_width = 0 .. output_line_size;
output_height = 0 .. output_page_size;
option_word = STRING[option_word_size];
option_record = RECORD word: option_word; code: positive_integer END;
option_list_argument = ARRAY[*] OF option_record;
string_argument = STRING[*];
string_pointer = ^STRING[*];
token_type = (numeric_token, literal_token, operator_token, word_token);
token_pointer = ^token_record;
token_record = RECORD
  next: token_pointer;
  CASE kind: token_type OF
  numeric_token: (numeric_value: string_pointer);
  literal_token: (literal_value: string_pointer);
  operator_token: (operator_value: CHAR);
  word_token: (word_value: string_pointer)
END;
command_routine = PROCEDURE(token_pointer);
routine_list_argument = ARRAY[1 .. *] OF command_routine;
output_file = RECORD (*output file state information*)
  internal_file: TEXT; (*output file, set to NILF if not open*)
  external_file: FILE_NAME; (*name of output file*)
  page_number: positive_integer; (*of last character written*)
  line_number: positive_integer; (*in current page of last character written*)
  line_cursor: output_width; (*line position of last character written*)
  line_size: output_width; (*current line size -- paper size*)
  page_size: output_height; (*current page size -- paper size*)
  left_margin: output_width; (*space used by left margin*)
  right_margin: output_width; (*space used by right margin*)
  top_margin: output_height; (*space used by top margin*)
  bottom_margin: output_height; (*space used by bottom margin*)
  wrap_position: -1 .. output_line_size; (*0=no wrap, -1=right wrap*)
  insert_page_mark: PROCEDURE(VAR output_file);
  insert_left_margin: PROCEDURE(VAR output_file);
  insert_right_margin: PROCEDURE(VAR output_file);
  insert_top_margin: PROCEDURE(VAR output_file);
  insert_bottom_margin: PROCEDURE(VAR output_file)
END;

$PAGE variable declarations

VAR (*initial values established in procedure set_up_initial_conditions*)
foo_options: ARRAY [1 .. number_of_foo_command_options] OF option_record;
foo_routines: ARRAY [1 .. number_of_foo_command_routines] OF command_routine;
terminal_is_on: positive_integer := 0; (*terminal is on if greater than zero*)
terminal_is_printer: BOOLEAN := FALSE;
terminal_was_printer: BOOLEAN := FALSE;
output_name: FILE_NAME := '';
input_name: FILE_NAME := '';
saved_files = RECORD (*when terminal is turned on*)
  output_file, input_file, command_source: TEXT
END;
command_source: TEXT := NILF;
comment_set: SET OF CHAR := default_comment_set;
delimiter_set: SET OF CHAR := default_delimiter_set;
nset: SET OF CHAR := default_nset;
quote_set: SET OF CHAR := default_quote_set;
operator_set: SET OF CHAR := default_operator_set;
encryption_key: string_pointer := NIL;
encryption_matrix: ARRAY [CHAR, CHAR] OF CHAR;
encryption_matrix_loaded: BOOLEAN := FALSE;

$PAGE dispose_file, dispose_string, dispose_token

PROCEDURE dispose_file(VAR f: TEXT);
(*close a text file, set to NILF*)
BEGIN
  MASK(ATTENTION);
  IF (f <> NILF) ANDIF (f <> TTY) ANDIF (f <> TTYOUTPUT) THEN CLOSE(f);
  f := NILF;
  UNMASK(ATTENTION)
END;

PROCEDURE dispose_string(VAR s: string_pointer);
(*dispose of a string, set to NIL*)
BEGIN
  MASK(ATTENTION);
  IF s <> NIL THEN DISPOSE(s);
  s := NIL;
  UNMASK(ATTENTION)
END;

PROCEDURE dispose_token(VAR t: token_pointer);
(*dispose of a token list, set to NIL*)
VAR tmp: token_pointer;
BEGIN
  MASK(ATTENTION);
  WHILE t <> NIL DO BEGIN
    tmp := t; t := tmp^.next; tmp^.next := NIL;
    CASE tmp^.kind OF
      numeric_token: dispose_string(tmp^.code);
      literal_token: dispose_string(tmp^.literal_value);
      operator_token: (*do nothing*);
      word_token: dispose_string(tmp^.word)
    END;
    DISPOSE(tmp)
  END;
  UNMASK(ATTENTION)
END;

$PAGE write_string, write_line, write_message

PROCEDURE write_string(s: string_argument);
(*write a string to output*)
BEGIN
  IF OUTPUT = NILF THEN RETURN;
  WRITE(s);
  IF OUTPUT = TTYOUTPUT THEN BREAK
END;

PROCEDURE write_line(l: string_argument);
(*write a line of output*)
BEGIN
  IF OUTPUT = NILF THEN RETURN;
  WRITELN(l);
  IF OUTPUT = TTYOUTPUT THEN BREAK
END;

PROCEDURE write_message(m: string_argument);
(*write a message line*)
BEGIN
  write_line(''); write_line(m);
END;

$PAGE output_nothing, attach_output, open_output, close_output, ready_output

PROCEDURE output_nothing(VAR f: output_file);
(*null output procedure*)
BEGIN
END;

PROCEDURE attach_output(VAR f: output_file; t: TEXT);
(*set up initial status for non-NILF output file*)
BEGIN
  WITH f DO BEGIN
    internal_file := n; external_file := FILENAME(internal_file);
    page_number := 0; line_number := 0; line_cursor := 0;
    page_size := 66; line_size := 80;
    wrap_position := -1;
    left_margin := 0; right_margin := 0;
    top_margin := 0; bottom_margin := 0;
    insert_page_mark := output_nothing;
    insert_left_margin := output_nothing;
    insert_right_margin := output_nothing;
    insert_top_margin := output_nothing;
    insert_bottom_margin := output_nothing;
  END
END;

PROCEDURE open_output(VAR f: output_file; n: FILE_NAME);
(*open an output file with initial status*)
VAR t: TEXT;
BEGIN
  REWRITE(t, n); attach_output(f, t);
END;

PROCEDURE close_output(VAR f: output_file);
(*temporarily close an output file*)
BEGIN
  dispose_file(f.internal_file)
END;

PROCEDURE ready_output(VAR f: output_file);
(*insure that the output file is open*)
BEGIN
  WITH f DO IF internal_file = NILF
  THEN REWRITE(internal_file, external_file, [PRESERVE])
END;

$PAGE flush_output_page, flush_output_line, fill_output_page, fill_output_line

PROCEDURE flush_output_page(VAR f: output_file);
(*write output page buffer to output file*)
VAR saved_output: TEXT;
BEGIN
  ready_output(f); saved_output := OUTPUT;
  BEGIN
    OUTPUT := f.internal_file;
    FOR i := 1 TO f.page_size DO wrlin(f.page_buffer[i]);
    OUTPUT := saved_output
    EXCEPTION ALLCONDITIONS: BEGIN OUTPUT := saved_output; SIGNAL END
  END
END;

PROCEDURE flush_output_line(VAR f: output_file);
(*send output line buffer to output page buffer*)
BEGIN
  WITH f DO page_buffer[page_cursor] := line_buffer
END;

PROCEDURE fill_output_page(VAR f: output_file);
(*fill remainder of output page buffer with blanks*)
BEGIN
  output_vertical_tab(f, f.page_size)
END;

PROCEDURE fill_output_line(VAR f: output_file);
(*fill remainder of line buffer with blanks*)
BEGIN
  output_horizontal_tab(f, f.line_size)
END;

$PAGE finish_output_file, output_new_page, output_new_line

PROCEDURE finish_output_file(VAR f: output_file);
(*finish with output file*)
BEGIN
  fill_output_page(f); flush_output_page(f); close_output_file(f)
END;

PROCEDURE output_new_page(VAR f: output_file);
(*finish previous page, insert page mark, start next page*)
BEGIN
  fill_output_page(f); flush_output_page(f); f.insert_page_mark(f)
END;

PROCEDURE output_new_line(VAR f: output_file);
(*finish previous line, insert line mark, start next line*)
BEGIN
  fill_output_line(f); flush_output_line(f)
END;

$PAGE fio_nskip
(* FIO NSKIP is equivalent to N calls to fio_skip, except that it will never
   write beyond the top of a new page.  Fio_nskip will also test whether there
   are M lines remaining on the current page, and will force a page skip if
   there are not. *)

public procedure fio_nskip ( var fb: output_file; nskip, nleft: integer );
var i: integer;

begin
  with fb do begin
    if (line_cursor = 1) andif (page_size <> 0) andif (line_number > page_size)
      then fio_page (fb)
      else begin
	line_cursor := 1;
	if (page_size = 0) orif (line_number + nskip + nleft <= page_size) then begin
	  for i := 1 to nskip do
	    writeln (fil);
	  line_number := line_number + nskip;
	end
	else
	  line_number := page_size + 1;
      end;
  end;
end;
$PAGE fio_tab
(* FIO TAB moves the output to a specified line_cursor position.  If the target line_cursor
   lies before the current line_cursor position, a new line is started before tabbing. *)

public procedure fio_tab (var fb: output_file; tabcol: output_width);

 var
   i, tcol: output_width;

const
    ht = chr (011b);

 begin
  with fb do begin
    tcol := min (tabcol, line_size);

    if line_cursor > tcol then fio_skip (fb);

    if line_cursor = tcol then return;

    if (line_cursor = 1) andif (page_size <> 0) andif (line_number > page_size)
      then fio_page (fb);

    for i := 1 to (((tcol - 1) div 8) - ((line_cursor - 1) div 8)) do
      write (fil, ht);
    write (fil, ' ': min ((tcol - 1) mod 8, tcol - line_cursor));
    line_cursor := tcol;
  end;
 end;
$PAGE fio_write
(* FIO WRITE outputs a string to the file.  Page line_size and length limitations are
   checked for and enforced. *)

public procedure fio_write (var fb: output_file; str: packed array [1..*] of char);

var idx, l: integer;

begin
  with fb do begin
    if (line_cursor = 1) andif (page_size <> 0) andif (line_number > page_size)
      then fio_page (fb);

    if (length (str) > line_size - line_cursor + 1) and
       (length (str) <= line_size - wrap_position + 1) and
       (wrap_position <> 0) then
      fio_tab (fb, wrap_position);

    idx := 1;
    loop
      l := min (length (str) - idx + 1, line_size - line_cursor + 1);
      write (fil, substr (str, idx, l));
      idx := idx + l;
      line_cursor := line_cursor + l;
    exit if (idx > length(str)) or (wrap_position = 0);
      fio_tab (fb, wrap_position);
    end;
  end (* with fb *);
end;
$PAGE fio_line
(* FIO LINE writes a string to a file and skips to a new line.  Page line_size and
   length limitations are applied. *)

public procedure fio_line (var fb: output_file; str: packed array [1..*] of char);
 begin
  fio_write (fb, str);
  fio_skip (fb);
 end;
$PAGE fio_space
(* FIO SPACE emits a specified number of spaces to the output file. *)

public procedure fio_space ( var fb: output_file; spaces: output_width);

begin
  fio_tab (fb, fb.line_cursor+spaces);
end.

$PAGE printeron, printeroff, printerstr, printerlin

PROCEDURE printeron;
(*turn on line printer*)
VAR saved_output: TEXT;
BEGIN
  MASK(ATTENTION);
  IF NOT terminal_is_printer THEN BEGIN
    saved_output := OUTPUT; OUTPUT := terminalOUTPUT;
    write_string(turn_printer_on);
    OUTPUT := saved_output;
    terminal_is_printer := TRUE
  END;
  UNMASK(ATTENTION)
END;

PROCEDURE printeroff;
(*turn off line printer*)
VAR saved_output: TEXT;
BEGIN
  MASK(ATTENTION);
  IF terminal_is_printer THEN BEGIN
    saved_output := OUTPUT; OUTPUT := terminalOUTPUT;
    write_string(turn_printer_off);
    OUTPUT := saved_output;
    terminal_is_printer := FALSE
  END;
  UNMASK(ATTENTION)
END;

PROCEDURE printerstr(str: string_argument);
(*write a string to the line printer*)
VAR saved_output: TEXT;
BEGIN
  saved_output := OUTPUT;
  BEGIN
    OUTPUT := terminalOUTPUT;
    IF terminal_is_printer THEN write_string(str)
    ELSE BEGIN
      printeron;
      BEGIN
        write_string(str); printeroff
	EXCEPTION ALLCONDITIONS: BEGIN printeroff; SIGNAL END
      END
    END;
    OUTPUT := saved_output
    EXCEPTION ALLCONDITIONS: BEGIN OUTPUT := saved_output; SIGNAL END
  END
END;

PROCEDURE printerlin(lin: string_argument);
(*write a line to the line printer*)
VAR saved_output: TEXT;
BEGIN
  saved_output := OUTPUT;
  BEGIN
    OUTPUT := terminalOUTPUT;
    IF terminal_is_printer THEN write_line(lin)
    ELSE BEGIN
      printeron;
      BEGIN
        write_line(lin); printeroff
	EXCEPTION ALLCONDITIONS: BEGIN printeroff; SIGNAL END
      END
    END;
    OUTPUT := saved_output;
    EXCEPTION ALLCONDITIONS: BEGIN OUTPUT := saved_output; SIGNAL END
  END
END;

$PAGE opnterminal, terminalon, terminaloff

PROCEDURE opnterminal;
(*open the terminal for input and output*)
BEGIN
  MASK(ATTENTION);
  REWRITE(terminalOUTPUT); OPEN(terminal); terminal_is_on := 0;
  terminal_is_printer := TRUE; printeroff; terminal_was_printer := FALSE;
  UNMASK(ATTENTION)
END;

PROCEDURE terminalon;
(*temporarily make output go to terminal/printer*)
BEGIN
  MASK(ATTENTION);
  IF terminal_is_on < 1 THEN BEGIN
    terminal_was_printer := terminal_is_printer;
    IF terminal_is_printer THEN printeroff;
    saved_output := OUTPUT; saved_input := INPUT;
    saved_command_source := command_source;
    OUTPUT := terminalOUTPUT; INPUT := terminal; command_source := terminal
  END;
  terminal_is_on := terminal_is_on + 1;
  UNMASK(ATTENTION)
END;

PROCEDURE terminaloff;
(*restore output after terminalon*)
BEGIN
  MASK(ATTENTION);
  IF terminal_is_on > 0 THEN BEGIN
    terminal_is_on := terminal_is_on - 1;
    IF terminal_is_on < 1 THEN BEGIN
      IF terminal_was_printer THEN printeron;
      OUTPUT := saved_output; INPUT := saved_input;
      command_source := saved_command_source
    END
  END;
  UNMASK(ATTENTION)
END;

$PAGE terminalstr, terminallin, terminalmsg

PROCEDURE terminalstr(str: string_argument);
(*write a string to the terminal*)
BEGIN
  terminalon;
  BEGIN
    write_string(str)
    EXCEPTION ALLCONDITIONS: BEGIN terminaloff; SIGNAL END
  END;
  terminaloff
END;

PROCEDURE terminallin(lin: string_argument);
(*write a line to the terminal*)
BEGIN
  terminalon;
  BEGIN
    write_line(lin)
    EXCEPTION ALLCONDITIONS: BEGIN terminaloff; SIGNAL END
  END;
  terminaloff
END;

PROCEDURE terminalmsg(msg: string_argument);
(*write a message to the terminal*)
BEGIN
  terminalon;
  BEGIN
    write_message(msg)
    EXCEPTION ALLCONDITIONS: BEGIN terminaloff; SIGNAL END
  END;
  terminaloff
END;

$PAGE errmsg, err, chk, chkterminal

PROCEDURE errmsg(msg: string_argument);
(*write an error message to the terminal*)
BEGIN
  terminalmsg(msg)
END;

FUNCTION askterminal(question: string_argument; options: option_list_argument): ordinal_integer; FORWARD;

PROCEDURE err(msg: string_argument);
(*signal an error--abort, continue or fatal*)
CONST options: ARRAY[1 .. 5] OF option_record =
  (('ABORT', 1), ('CONTINUE', 2), ('FATAL', 3),
  ('YES', 1), ('NO', 2));
EXCEPTION continue;
BEGIN
  MASK(ATTENTION);
  BEGIN
    errmsg(msg);
    CASE askterminal('abort after error?', options) OF
      1: SIGNAL(abort);
      2: SIGNAL(continue);
      3: SIGNAL(fatal)
    END
    EXCEPTION ALLCONDITIONS: BEGIN UNMASK(ATTENTION); SIGNAL END
  END
  EXCEPTION continue: errmsg('continue with error')
END;

PROCEDURE chk(cond: BOOLEAN; msg: string_argument);
(*check condition, error if FALSE*)
BEGIN
  IF NOT cond THEN err(msg)
END;

PROCEDURE chkterminal(msg: string_argument);
(*check that command source is the terminal*)
BEGIN
  chk(command_source = terminal, msg)
END;

$PAGE rdlin, solicit

FUNCTION rdlin(VAR lin: string_argument): BOOLEAN;
(*try to read the next line of input (file or terminal)*)
BEGIN
  (*note that "lin" is not set if end of file*)
  rdlin := (INPUT <> NILF) ANDIF NOT EOF;
  IF rdlin THEN BEGIN
    IF INPUT = terminal THEN BEGIN READLN; READ(lin) END
    ELSE READLN(lin)
  END;
  IF lin = terminal_fatal_error THEN SIGNAL(fatal);
  IF lin = terminal_end_of_file THEN rdlin := FALSE
END;

PROCEDURE solicit(question: string_argument; VAR response: string_argument);
(*solicit a response to a question from the command source*)
BEGIN
  IF command_source = terminal THEN BEGIN
    terminalon;
    BEGIN
      terminalmsg(question); IF NOT rdlin(response) THEN response := ''
      EXCEPTION ALLCONDITIONS: BEGIN terminaloff; SIGNAL END
    END;
    terminaloff
  END
  ELSE err('currently, only "terminal:" is allowed as a command source');

$PAGE newstr, unquote

FUNCTION newstr(str: string_argument): string_pointer;
(*allocate a new string*)
BEGIN
  MASK(ATTENTION);
  NEW(newstr, LENGTH(str)); newstr^ := str;
  UNMASK(ATTENTION)
END;

FUNCTION unquote(str: string_argument): string_pointer;
(*remove layer of quotes from string, if any*)
VAR i, j: positive_integer; tmp: string_pointer;
BEGIN
  NEW(tmp, LENGTH(str));
  BEGIN
    tmp^ := '';
    IF str <> '' THEN BEGIN
      IF NOT (str[1] IN quote_set) THEN tmp^ := str
      ELSE BEGIN
        IF str[LENGTH(str)] = str[1] THEN j := LENGTH(str) - 1
        ELSE j := LENGTH(str);
        FOR i := 2 TO j
        DO IF (NOT (str[i] IN quote_set)) ORIF (str[i - 1] IN quote_set)
        THEN tmp^ := tmp^ || str[i]
      END
    END
    EXCEPTION ALLCONDITIONS: BEGIN dispose_string(tmp); SIGNAL END
  END;
  unquote := tmp
END;

$PAGE intlen, explen, declen, codelen

FUNCTION intlen(str: string_argument): positive_integer;
(*return length of integer token in string*)
BEGIN
  (*change the codeeric base by altering the "nset"*)
  intlen := VERIFY(str, nset, LENGTH(str) + 1) - 1;
END;

FUNCTION explen(str: string_argument): positive_integer;
(*return length of exponent token in string*)
BEGIN
  (*exponent must begin with "E"*)
  (*the "+" or "-" before the integer part is optional*)
  IF (str <> '') ANDIF (str[1] IN ['E', 'e']) THEN BEGIN
    IF (LENGTH(str) > 1) ANDIF (str[2] IN ['+', '-']) THEN explen := 2
    ELSE explen := 1;
    explen := explen + intlen(SUBSTR(str, explen + 1))
  END
  ELSE explen := 0
END;

FUNCTION declen(str: string_argument): positive_integer;
(*return length of decimal token in string*)
BEGIN
  (*decimal must begin with a period*)
  IF (str <> '') ANDIF (str[1] = '.')
  THEN declen := 1 + intlen(SUBSTR(str, 2))
  ELSE declen := 0
END;

FUNCTION codelen(str: string_argument): positive_integer;
(*return length of codeeric token in string*)
BEGIN
  (*integer or real codeeric strings*)
  codelen := intlen(str);
  codelen := codelen + declen(SUBSTR(str, codelen + 1));
  codelen := codelen + explen(SUBSTR(str, codelen + 1))
END;

$PAGE grblen, litlen, tknlen

FUNCTION grblen(str: string_argument): positive_integer;
(*return length of initial garbage in string*)
BEGIN
  (*garbage is a series of delimiter characters, see "delimiter_set"*)
  IF str = '' THEN grblen := 0
  ELSE grblen := VERIFY(str, delimiter_set, 1) - 1
END;

FUNCTION litlen(str: string_argument; ch: CHAR): positive_integer;
(*return length of literal token in string with given quote character*)
BEGIN
  (*returned length includes quote characters*)
  (*literal terminated by quote character or end of string*)
  (*two consecutive quote characters will not terminate a literal*)
  IF str = '' THEN litlen := 0
  ELSE BEGIN
    litlen := INDEX(str, ch, LENGTH(str));
    IF ((litlen + 1) <= LENGTH(str)) ANDIF (str[litlen + 1] = ch) THEN BEGIN
      IF (litlen + 2) > LENGTH(str) THEN litlen := LENGTH(str)
      ELSE litlen := litlen + litlen(SUBSTR(str, litlen + 2), ch) + 1
    END
  END
END;

FUNCTION tknlen(str: string_argument): positive_integer;
(*return length of initial token in string*)
BEGIN
  (*a token is a series of non-delimiter characters, see "delimiter_set"*)
  (*a token can also be a number, see "nset"*)
  (*a token can also be an operator character, see "operator_set"*)
  (*a token can also be a quoted literal, see "quote_set"*)
  IF str = '' THEN tknlen := 0
  ELSE BEGIN
    IF str[1] IN nset THEN tknlen := codelen(str)
    ELSE IF str[1] IN operator_set THEN tknlen := 1
    ELSE IF str[1] IN quote_set THEN tknlen := litlen(SUBSTR(str, 2), str[1]) + 1
    ELSE tknlen := SEARCH(str, delimiter_set + operator_set + quote_set, LENGTH(str) + 1) - 1
  END
END;

$PAGE scntkn

FUNCTION scntkn(str: string_argument): token_pointer;
(*scan a list of tokens from a string*)
VAR bgntkn, endtkn: positive_integer; tkn, tmp: token_pointer;
BEGIN
  (*bgntkn is the character position of the first character in the token*)
  (*endtkn is the character position of the last character in the token*)
  tkn := NIL;
  BEGIN
    bgntkn := 1; scntkn := NIL;
    WHILE bgntkn <= LENGTH(str) DO BEGIN
      bgntkn := bgntkn + grblen(SUBSTR(str, bgntkn));
      IF bgntkn <= LENGTH(str) THEN BEGIN
        IF str[bgntkn] IN comment_set THEN endtkn := LENGTH(str)
        ELSE endtkn := bgntkn + tknlen(SUBSTR(str, bgntkn)) - 1;
	MASK(ATTENTION);
        IF tkn = NIL THEN BEGIN NEW(tkn); tmp := tkn END
        ELSE BEGIN NEW(tmp^.next); tmp := tmp^.next END;
        tmp^.next := NIL;
	UNMASK(ATTENTION);
        IF str[bgntkn] IN nset THEN BEGIN
          tmp^.kind := numeric_token;
	  tmp^.code := newstr(SUBSTR(str, bgntkn, endtkn - bgntkn + 1))
        END
        ELSE IF str[bgntkn] IN (quote_set + comment_set) THEN BEGIN
          tmp^.kind := literal_token;
          tmp^.literal_value := newstr(SUBSTR(str, bgntkn, endtkn - bgntkn + 1))
        END
        ELSE IF str[bgntkn] IN operator_set THEN BEGIN
          tmp^.kind := operator_token; tmp^.opr := str[bgntkn]
        END
        ELSE BEGIN
          tmp^.kind := word_token;
          tmp^.word := newstr(SUBSTR(str, bgntkn, endtkn - bgntkn + 1))
        END;
        bgntkn := endtkn + 1
      END
    END
    EXCEPTION ALLCONDITIONS: BEGIN dispose_token(tkn); SIGNAL END
  END;
  scntkn := tkn
END;

$PAGE abbrev, optcode, tknopt

FUNCTION abbrev(s1, s2: string_argument): BOOLEAN;
(*return TRUE if s1 is equal to or an abbreviation of s2*)
BEGIN
  abbrev := (LENGTH(s1) <= LENGTH(s2)) ANDIF
    (UPPERCASE(s1) = UPPERCASE(SUBSTR(s2, 1, LENGTH(s1))))
END;

FUNCTION optcode(opt: string_argument; options: option_list_argument): positive_integer;
(*return option number or zero if no match*)
VAR i: positive_integer;
BEGIN
  optcode := 0;
  FOR i := 1 TO UPPERBOUND(options)
  DO EXIT IF abbrev(opt, options[i].word) DO optcode := options[i].code
END;

FUNCTION tknopt(tkn: token_pointer; options: option_list_argument): positive_integer;
(*return option index of first token or zero*)
BEGIN
  IF tkn <> NIL THEN CASE tkn^.kind OF
    numeric_token: tknopt := optcode(tkn^.code^, options);
    literal_token: tknopt := 0;
    operator_token: tknopt := optcode(tkn^.opr, options);
    word_token: tknopt := optcode(tkn^.word^, options);
  END
  ELSE tknopt := 0
END;

$PAGE wrtkn, wroptions, terminaloptions

PROCEDURE wrtkn(tkn: token_pointer);
(*write token list to output*)
VAR tmp: token_pointer;
BEGIN
  tmp := tkn;
  WHILE tmp <> NIL DO WITH tmp^ DO BEGIN
    CASE typ OF
      numeric_token: write_line(code^);
      literal_token: write_line(lit^);
      operator_token: write_line(opr);
      word_token: write_line(word^)
    END;
    tmp := next
  END
END;

PROCEDURE wroptions(options: option_list_argument);
(*display options*)
VAR i: positive_integer;
BEGIN
  FOR i := 1 TO UPPERBOUND(options) DO write_line(options[i].word)
END;

PROCEDURE terminaloptions(msg: string_argument; options: option_list_argument);
(*write message and display options to terminal*)
BEGIN
  terminalon;
  BEGIN
    write_message(msg); wroptions(options);
    EXCEPTION ALLCONDITIONS: BEGIN terminaloff; SIGNAL END
  END;
  terminaloff
END;

$PAGE query, command

FUNCTION query(question: string_argument; options: option_list_argument; VAR tkn: token_pointer): ordinal_integer;
(*ask a question, return number of option recognized, tkn points to response*)
VAR i: positive_integer; opt: option_word;
BEGIN
  REPEAT
    IF tkn = NIL THEN BEGIN solicit(question, opt); tkn := scntkn(opt) END;
    i := tknopt(tkn, options);
    IF i = 0 THEN BEGIN
      chkterminal('invalid response to "' || question || '"');
      terminaloptions('options are: ', options); dispose_token(tkn)
    END
    ELSE query:= i
  UNTIL i > 0
END;

PROCEDURE command(prompt: string_argument; routines: routine_list_argument; options: option_list_argument);
(*command line processor--dispatch to command routine*)
VAR tkn: token_pointer; optcode: positive_integer;
BEGIN
  tkn := NIL; optcode := query(prompt, options, tkn);
  BEGIN
    routines[optcode](tkn^.next)
    EXCEPTION
$IF trap_attention
    ATTENTION: BEGIN dispose_token(tkn); SIGNAL(abort) END;
$ENDIF
    OTHERS: BEGIN dispose_token(tkn); SIGNAL END
  END;
  dispose_token(tkn)
END;

$PAGE ask, askterminal

FUNCTION ask(question: string_argument; options: option_list_argument): ordinal_integer;
(*ask a question, return number of option recognized*)
VAR tkn: token_pointer;
BEGIN
  tkn := NIL;
  BEGIN
    REPEAT
      dispose_token(tkn); ask := query(question, options, tkn);
      IF tkn^.next <> NIL THEN BEGIN
        chkterminal('no arguments allowed for "' || question || '"');
        errmsg('no arguments allowed for "' || question || '"')
      END
    UNTIL tkn^.next = NIL;
    dispose_token(tkn)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_token(tkn); SIGNAL END
  END
END;

FUNCTION askterminal(question: string_argument; options: option_list_argument): ordinal_integer;
(*ask a question to terminal, return number of option recognized*)
BEGIN
  terminalon;
  BEGIN
    askterminal := ask(question, options)
    EXCEPTION ALLCONDITIONS: BEGIN terminaloff; SIGNAL END
  END;
  terminaloff
END;

$PAGE chkarg, chklit, chkin, chkout, chkio, chkpass

PROCEDURE chkarg(nam: string_argument; args: token_pointer; codeargs: positive_integer);
(*check that command has correct number of arguments*)
VAR tmp: token_pointer; i: positive_integer;
BEGIN
  tmp := args;
  FOR i := 1 TO codeargs DO BEGIN
    chk(tmp <> NIL, 'too few arguments supplied for ' || nam || ' command');
    tmp := tmp^.next
  END;
  chk(tmp = NIL, 'too many arguments supplied for ' || nam || ' command')
END;

PROCEDURE chklit(nam: string_argument; arg: token_pointer);
(*check that argument is a literal*)
BEGIN
  chk(((arg <> NIL) ANDIF (arg^.kind = literal_token)),
    nam || ' command requires a literal argument')
END;

PROCEDURE chkin(nam: string_argument);
(*check that input has been established, tell how to terminate if terminal*)
BEGIN
  chk(input_name <> '', nam || ' command requires input');
  IF INPUT = terminal
  THEN BEGIN
    terminalmsg('"' || terminal_end_of_file || '" terminates input to ' || nam || ' command')
  END
END;

PROCEDURE chkout(nam: string_argument);
(*check that output has been established*)
BEGIN
  chk(output_name <> '', nam || ' command requires output');
  IF OUTPUT = terminalOUTPUT THEN terminalmsg(nam || ' command output follows')
END;

PROCEDURE chkio(nam: string_argument);
(*check that input and output have been established*)
BEGIN
  chkin(nam); chkout(nam)
END;

PROCEDURE chkpass(nam: string_argument);
(*check that an encryption key/password has been established*)
BEGIN
  chk(encryption_key <> NIL, nam || ' command requires password')
END;

$PAGE load_encryption_matrix, encrypt

PROCEDURE load_encryption_matrix;
(*load global encryption translation matrix*)
VAR c, c1, c2: CHAR; minp, maxp: CHAR; p: SET OF CHAR;
BEGIN
  IF encryption_matrix_loaded THEN RETURN;
  MASK(ATTENTION);
  p := [' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'f', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1',
    '2', '3', '4', '5', '6', '7', '8', '9', '!', '@', '#', '$', '%', '^',
    '&', '*', '(', ')', '_', '-', '+', '=', '{', '[', '}', ']', '|', '\',
    ':', ';', '"' , '''', '<', ',', '>', '.', '?', '/', '~', '`'];
  FOR minp := MINIMUM(CHAR) TO MAXIMUM(CHAR) DO EXIT IF minp IN p;
  FOR maxp := MAXIMUM(CHAR) DOWNTO MINIMUM(CHAR) DO EXIT IF maxp IN p;
  FOR c1 := MINIMUM(CHAR) TO MAXIMUM(CHAR)
  DO FOR c2 := MINIMUM(CHAR) TO MAXIMUM(CHAR)
  DO encryption_matrix[c1, c2] := c1;
  c := minp;
  REPEAT c := SUCC(c) UNTIL c IN p;
  FOR c2 := minp TO maxp DO IF c2 IN p
  THEN FOR c1 := maxp DOWNTO minp DO IF c1 IN p THEN BEGIN
    encryption_matrix[c1, c2] := c;
    IF c1 <> minp THEN REPEAT
      IF c = maxp THEN c := minp ELSE c := SUCC(c)
    UNTIL c IN p
  END;
  UNMASK(ATTENTION);
  encryption_matrix_loaded := TRUE
END;

PROCEDURE encrypt(VAR str: string_argument; key: string_argument; VAR keycsr: positive_integer);
(*encrypt a string using specified key and cursor*)
VAR strcsr: positive_integer;
BEGIN
  IF key = '' THEN RETURN;
  load_encryption_matrix;
  FOR strcsr := 1 TO LENGTH(str) DO BEGIN
    IF (keycsr > LENGTH(key)) ORIF (keycsr < 1) THEN keycsr := 1;
    str[strcsr] := encryption_matrix[str[strcsr], key[keycsr]];
    keycsr := keycsr + 1
  END
END;

$PAGE open_input, open_output

PROCEDURE open_input(filnam: string_argument);
(*open input file*)
BEGIN
  chk(terminal_is_on < 1, 'can not open input when terminal is on');
  input_name := UPPERCASE(filnam); dispose_file(INPUT); INPUT := NILF;
  IF input_name = terminal_name THEN INPUT := terminal
  ELSE IF input_name = printer_name THEN err('can not input from line printer')
  ELSE IF input_name = temp_name THEN RESET(INPUT, temp_name)
  ELSE IF input_name <> '' THEN RESET(INPUT, input_name)
  EXCEPTION ALLCONDITIONS: BEGIN
    dispose_file(INPUT); INPUT := NILF; input_name := ''; SIGNAL
  END
END;

PROCEDURE open_output(filnam: string_argument);
(*open output file*)
BEGIN
  chk(terminal_is_on < 1, 'can not open output while terminal is on');
  IF output_name = printer_name THEN printeroff;
  output_name := UPPERCASE(filnam); dispose_file(OUTPUT); OUTPUT := NILF;
  IF output_name = terminal_name THEN OUTPUT := terminalOUTPUT
  ELSE IF output_name = printer_name THEN BEGIN printeron; OUTPUT := terminalOUTPUT END
  ELSE IF output_name = temp_name THEN REWRITE(OUTPUT, temp_name)
  ELSE IF output_name <> '' THEN REWRITE(OUTPUT, output_name)
  EXCEPTION ALLCONDITIONS: BEGIN
    dispose_file(OUTPUT); OUTPUT := NILF; output_name := ''; SIGNAL
  END
END;

$PAGE input_cmd, output_cmd, status_cmd

PROCEDURE input_cmd(arg: token_pointer);
(*set up input*)
VAR filnam: string_pointer;
BEGIN
  filnam := NIL;
  BEGIN
    chkarg('INPUT', arg, 1); chklit('INPUT', arg);
    filnam := unquote(arg^.literal_value^); open_input(filnam^); dispose_string(filnam)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_string(filnam); SIGNAL END
  END;
  EXCEPTION
  ALLCONDITIONS: BEGIN open_input(''); SIGNAL END
END;

PROCEDURE output_cmd(arg: token_pointer);
(*set up output*)
VAR filnam: string_pointer;
BEGIN
  filnam := NIL;
  BEGIN
    chkarg('OUTPUT', arg, 1); chklit('OUTPUT', arg);
    filnam := unquote(arg^.literal_value^); open_output(filnam^); dispose_string(filnam)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_string(filnam); SIGNAL END
  END
  EXCEPTION
  ALLCONDITIONS: BEGIN open_output(''); SIGNAL END
END;

PROCEDURE status_cmd(args: token_pointer);
(*status of input and output*)
BEGIN
  chkarg('STATUS', args, 0);
  IF output_name = '' THEN terminalmsg('no output file')
  ELSE BEGIN
    terminalmsg('output file is "' || output_name || '"');
    IF OUTPUT = NILF THEN terminalmsg('lost output file')
  END;
  IF input_name = '' THEN terminalmsg('no input file')
  ELSE BEGIN
    terminalmsg('input file is "' || input_name || '"');
    IF (INPUT = NILF) ORIF EOF THEN terminalmsg('end of input file')
  END
END;

$PAGE scratch_cmd, restore_cmd, pipe_cmd

PROCEDURE scratch_cmd(args: token_pointer);
(*clear input and output*)
BEGIN
  chkarg('SCRATCH', args, 0); open_input(''); open_output('')
  EXCEPTION
  ALLCONDITIONS: BEGIN open_input(''); open_output(''); SIGNAL END
END;

PROCEDURE restore_cmd(args: token_pointer);
(*restore input*)
BEGIN
  chkarg('RESTORE', args, 0); open_input(input_name)
  EXCEPTION ALLCONDITIONS: BEGIN open_input(''); SIGNAL END
END;

PROCEDURE pipe_cmd(arg: token_pointer);
(*prepare output as input and output, argument specifies new output*)
VAR filnam: string_pointer;
BEGIN
  input_name := output_name; filnam := NIL;
  IF arg = NIL THEN chkarg('PIPE', arg, 0)
  ELSE BEGIN
    chkarg('PIPE', arg, 1); chklit('PIPE', arg);
    filnam := unquote(arg^.literal_value^); output_name := filnam^; dispose_string(filnam)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_string(filnam); SIGNAL END
  END;
  dispose_file(OUTPUT); open_input(input_name); open_output(output_name)
  EXCEPTION
  ALLCONDITIONS: BEGIN open_input(''); open_output(''); SIGNAL END
END;

$PAGE exit_cmd, copy_cmd, scan_cmd, type_cmd

PROCEDURE exit_cmd(args: token_pointer);
(*terminate program execution*)
BEGIN
  chkarg('EXIT', args, 0); SIGNAL(done)
END;

PROCEDURE copy_cmd(args: token_pointer);
(*copy input to output*)
VAR lin: output_line;
BEGIN
  chkarg('COPY', args, 0); chkio('COPY');
  BEGIN
    WHILE rdlin(lin) DO write_line(lin);
    dispose_file(INPUT)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_file(INPUT); SIGNAL END
  END
END;

PROCEDURE scan_cmd(args: token_pointer);
(*scan tokens from input to output*)
VAR lin: output_line; tkn: token_pointer;
BEGIN
  tkn := NIL; chkarg('SCAN', args, 0); chkio('SCAN');
  BEGIN
    WHILE rdlin(lin) DO BEGIN tkn := scntkn(lin); wrtkn(tkn); dispose_token(tkn) END;
    dispose_file(INPUT)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_file(INPUT); dispose_token(tkn); SIGNAL END
  END
END;

PROCEDURE type_cmd(args: token_pointer);
(*type input to terminal*)
VAR lin: output_line;
BEGIN
  chkarg('TYPE', args, 0); chkin('TYPE');
  terminalmsg('text of ' || input_name || ' follows');
  BEGIN
    WHILE rdlin(lin) DO terminallin(lin);
    dispose_file(INPUT)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_file(INPUT); SIGNAL END
  END
END;

$PAGE list_cmd, password_cmd, encrypt_cmd

PROCEDURE list_cmd(args: token_pointer);
(*list input to line printer*)
VAR lin: output_line;
BEGIN
  chkarg('LIST', args, 0); chkin('LIST');
  terminalmsg('listing text of ' || input_name);
  BEGIN
    WHILE rdlin(lin) DO printerlin(lin);
    dispose_file(INPUT)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_file(INPUT); SIGNAL END
  END
END;

PROCEDURE password_cmd(arg: token_pointer);
(*set encryption key/password*)
BEGIN
  chkarg('PASSWORD', arg, 1); chklit('PASSWORD', arg);
  dispose_string(encryption_key);
  encryption_key := unquote(arg^.literal_value^)
END;

PROCEDURE encrypt_cmd(args: token_pointer);
(*encrypt input to output*)
VAR lin: output_line; keycsr: positive_integer;
BEGIN
  chkarg('ENCRYPT', args, 0); chkio('ENCRYPT'); chkpass('ENCRYPT');
  keycsr := 1;
  BEGIN
    WHILE rdlin(lin) DO BEGIN
      encrypt(lin, encryption_key^, keycsr); write_line(lin);
    END;
    dispose_file(INPUT)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_file(INPUT); SIGNAL END
  END
END;

$PAGE check_page_break, page_cmd

FUNCTION check_page_break(VAR logbrk: BOOLEAN; lin: string_argument; lincnt: positive_integer);
VAR tkn: token_pointer;
BEGIN (*check for a page break for line just read from INPUT*)
  logbrk := FALSE;
  IF SEARCH(lin, ['$']) = 1 THEN BEGIN
    tkn := scntkn(SUBSTR(lin, 2));
    IF tkn <> NIL THEN BEGIN
      logbrk := (tkn^.kind = word_token) ANDIF (tkn^.word^ = 'PAGE');
      dispose_token(tkn)
      EXCEPTION ALLCONDITIONS: BEGIN dispose_token(tkn); SIGNAL END
    END
  END
  check_page_break := EOPAGE OR logbrk OR ((lincnt MOD (page_size - footsz)) = 0)
END;

PROCEDURE page_cmd(args: token_pointer);
(*interpret page marks into blank lines, insert headings and footings*)
VAR lincnt, phypagcnt, logpagcnt: positive_integer; logbrk: BOOLEAN; lin: output_line;
PROCEDURE page_break(page_action: SET OF (heading, footing));
BEGIN (*perform a page break with heading and/or footing*)
  phypagcnt := phypagcnt + 1; IF logical_break THEN logpagcnt := logpagcnt + 1;
  IF footing IN page_action THEN BEGIN
    FOR lincnt := lincnt + 1 TO page_size - footsz - headsz DO write_line('');
    write_line(''); write_line('')
  END;
  IF heading IN page_action THEN BEGIN write_line(''); write_line('') END;
  lincnt := 0
END;
BEGIN
  chkarg('PAGE', args, 0); chkio('PAGE');
  BEGIN
    phypagcnt := 0; logpagcnt := 0;
    break_page([heading]);
    WHILE rdlin(lin) DO BEGIN
      IF check_page_break(logbrk, lincnt) THEN break_page([heading, footing]);
      IF NOT (logbrk) THEN BEGIN write_line(lin); lincnt := lincnt + 1 END
    END;
    break_page([footing]);
    dispose_file(INPUT)
    EXCEPTION ALLCONDITIONS: BEGIN dispose_file(INPUT); SIGNAL END
  END
END;

$PAGE set_up_initial_conditions, main

PROCEDURE set_up_initial_conditions(banner: string_argument);
(*set up terminal I/O, display banner*)
BEGIN
  MASK(ATTENTION);
  options := (('EXIT', 1), ('INPUT', 2), ('OUTPUT', 3),
    ('STATUS', 4), ('SCRATCH', 5), ('RESTORE', 6),
    ('PIPE', 7), ('COPY', 8), ('SCAN', 9), ('TYPE', 10),
    ('LIST', 11), ('QUIT', 1), ('STOP', 1), ('PASSWORD', 12),
    ('ENCRYPT', 13), ('PAGE', 14));
  routines := (exit_cmd, input_cmd, output_cmd,
    status_cmd, scratch_cmd, restore_cmd,
    pipe_cmd, copy_cmd, scan_cmd, type_cmd,
    list_cmd, password_cmd, encrypt_cmd,
    page_cmd);
  INPUT := NILF; input_name := ''; OUTPUT := NILF; output_name := '';
  opnterminal; command_source := terminal;
  terminalmsg(banner);
  load_encryption_matrix;
  UNMASK(ATTENTION)
  END;

BEGIN
  set_up_initial_conditions(banner);
  LOOP
    BEGIN
      command(prompt, routines, options)
      EXCEPTION
      done: BEGIN IF terminal_is_printer THEN printeroff; SIGNAL END;
$IF trap_abort
      abort: errmsg('restarting');
$ENDIF
$IF trap_fatal
      fatal: errmsg('fatal error encountered--restarting');
$ENDIF
$IF trap_attention
      ATTENTION: errmsg('use "EXIT" to terminate program');
$ENDIF
    END
  END
  EXCEPTION
  done: terminalmsg('normal program termination');
  abort: errmsg('can not restart');
  fatal: errmsg('fatal error encountered');
  ATTENTION: errmsg('operation interrupted');
  OTHERS: EXCEPTION_MESSAGE
END.
    @b 