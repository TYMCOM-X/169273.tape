MODULE setup;
(*set up TTY, INPUT and OUTPUT files*)

$SYSTEM err
$SYSTEM dbg
$SYSTEM ask
$SYSTEM copy

$INCLUDE setup.typ

PUBLIC PROCEDURE tty_on; FORWARD;
(*direct INPUT and OUTPUT to TTY*)

PUBLIC PROCEDURE tty_off; FORWARD;
(*restore INPUT and OUTPUT to state before tty_on*)

PUBLIC PROCEDURE close_io; FORWARD;
(*close INPUT and OUTPUT files, except TTY*)

PUBLIC PROCEDURE open_io(i_nam, o_nam: STRING[*]); FORWARD;
(*open INPUT and OUTPUT files*)

PUBLIC PROCEDURE rd_lin(VAR lin: STRING[*]); FORWARD;
(*read line of text from INPUT*)

PUBLIC PROCEDURE rd_cmd(prompt: STRING[*]; VAR lin: STRING[*]); FORWARD;
(*read command line from INPUT, prompt if TTY*)

PUBLIC PROCEDURE arg(n: INTEGER; VAR val: STRING[*]); FORWARD;
(*return the nth argument from the command line*)

PUBLIC FUNCTION set_up(name: STRING[*]): BOOLEAN; FORWARD;
(*set up TTY, INPUT and OUTPUT files*)

PUBLIC VAR end_input: BOOLEAN;
(*set TRUE if at end of INPUT file*)

CONST
stk_siz = 5;
eof_mrk = '#';

VAR
initial_debug: BOOLEAN := FALSE;
cold_start: BOOLEAN := TRUE;
lin: STRING[80] := '';
stk: ARRAY[1 .. stk_siz] OF RECORD
  stk_input, stk_output: TEXT; stk_end_input: BOOLEAN
END;
stk_ptr: 0 .. stk_siz;

PROCEDURE rd_lin(VAR lin: STRING[*]);
BEGIN (*read line of text from INPUT*)
  end_input := end_input OR EOF;
  IF end_input THEN lin := ''
  ELSE BEGIN
    IF INPUT = TTY THEN BEGIN READLN; READ(lin) END
    ELSE READLN(lin);
    end_input := (lin = eof_mrk);
    IF end_input THEN lin := ''
  END
END;

PROCEDURE rd_cmd(prompt: STRING[*]; VAR lin: STRING[*]);
BEGIN (*read command line from INPUT, prompt if TTY*)
  IF INPUT = TTY THEN BEGIN WRITE(TTYOUTPUT, prompt); BREAK(TTYOUTPUT) END;
  rd_lin(lin);
  IF lin <> '' THEN lin := SUBSTR(lin, VERIFY(lin, [' '], 1));
END;

PROCEDURE arg(n: INTEGER; VAR val: STRING[*]);
VAR argnum, argpos, newargpos, arglen: INTEGER;
BEGIN (*return the nth argument from the command line*)
  argnum := 0; argpos := 0;
  REPEAT
    newargpos := argpos + INDEX(SUBSTR(lin, argpos + 1), '/', 0);
    EXIT IF newargpos <= argpos DO argnum := 0;
    argnum := argnum + 1; argpos := newargpos
    UNTIL argnum >= n;
  IF (n > 0) ANDIF (argnum = n) THEN BEGIN
    arglen := INDEX(SUBSTR(lin, argpos + 1), '/', 0) - 1;
    IF arglen < 0 THEN arglen := LENGTH(lin) - argpos;
    IF arglen > UPPERBOUND(val) THEN arglen := UPPERBOUND(val);
    val := SUBSTR(lin, argpos + 1, arglen)
    END
  ELSE val := ''
END;

PROCEDURE pop_io;
BEGIN (*pop INPUT and OUTPUT files from stack*)
  IF stk_ptr = MINIMUM(stk_ptr) THEN BEGIN
    INPUT := TTY; end_input := FALSE; OUTPUT := TTYOUTPUT
  END
  ELSE BEGIN
    WITH stk[stk_ptr] DO BEGIN
      INPUT := stk_input; end_input := stk_end_input; OUTPUT := stk_output
    END;
    stk_ptr := PRED(stk_ptr)
  END
END;

PROCEDURE push_io;
BEGIN (*push INPUT and OUTPUT files onto stack*)
  IF stk_ptr = MAXIMUM(stk_ptr) THEN sgnl_err('file stack overflow')
  ELSE BEGIN
    stk_ptr := SUCC(stk_ptr);
    WITH stk[stk_ptr] DO BEGIN
      stk_input := INPUT; stk_end_input := end_input; stk_output := OUTPUT
    END
  END
END;

PROCEDURE close_io;
BEGIN (*close INPUT and OUTPUT files, except TTY*)
  IF INPUT <> TTY THEN CLOSE(INPUT);
  IF OUTPUT <> TTYOUTPUT THEN CLOSE(OUTPUT);
  pop_io
END;

PROCEDURE open_io(i_nam, o_nam: STRING[*]);
BEGIN (*open INPUT and OUTPUT files*)
  err := FALSE; push_io;
  IF (o_nam = '') OR (UPPERCASE(o_nam) = 'TTY:') THEN OUTPUT := TTYOUTPUT
  ELSE BEGIN
    REWRITE(OUTPUT, o_nam, [RETRY]);
    IF IOSTATUS <> IO_OK THEN sgnl_err('output file: ' || o_nam)
  END;
  IF (i_nam = '') OR (UPPERCASE(i_nam) = 'TTY:') THEN INPUT := TTY
  ELSE BEGIN
    RESET(INPUT, i_nam, [RETRY]);
    IF IOSTATUS <> IO_OK THEN sgnl_err('input file: ' || i_nam)
    ELSE end_input := EOF;
  END;
  IF err THEN close_io;
END;

PROCEDURE tty_on;
BEGIN (*direct INPUT and OUTPUT to TTY*)
  open_io('', '')
END;

PROCEDURE tty_off;
BEGIN (*restore INPUT and OUTPUT to state before tty_on*)
  close_io
END;

PROCEDURE do_continue(name: STRING[*]);
VAR resp: INTEGER;
BEGIN (*continue from any error*)
  IF err THEN ask_tty('Continue ' || name || '?', ('YES', 'NO'), resp)
  ELSE resp := 1;
  IF resp <> 1 THEN STOP;
END;

PROCEDURE do_start(name: STRING[*]);
BEGIN (*cold or warm start*)
  IF NOT cold_start THEN do_continue(name);
  CLOSE; OPEN(TTY); INPUT := TTY; REWRITE(TTYOUTPUT); OUTPUT := TTYOUTPUT;
  end_input := FALSE; open_err; IF dbg OR initial_debug THEN open_dbg;
  stk_ptr := MINIMUM(stk_ptr);
  IF cold_start THEN BEGIN
    WRITELN(TTY, name); cold_start := FALSE; initial_debug := dbg
  END
END;

PROCEDURE dpy_info(name: STRING[*]);
BEGIN (*display information to TTY*)
  WRITELN(TTY, 'Program: ', name);
  WRITELN(TTY, 'Respond with one of "?", "HELP", "EXIT" or:');
  WRITELN(TTY, '<input file> = <output file> <arg1> <arg2> ...')
END;

PROCEDURE open_files(VAR files_open: BOOLEAN);
VAR opos, olen, ipos, ilen: INTEGER;
BEGIN (*get file names from non-null command line, try to open files*)
  opos := 1; olen := INDEX(lin, '=', 0) - opos;
  IF olen < 0 THEN ipos := 1 ELSE ipos := olen + 2;
  ilen := INDEX(lin, '/', LENGTH(lin) + 1) - ipos;
  IF olen < 0 THEN olen := 0; IF ilen < 0 THEN ilen := 0;
  openio(SUBSTR(lin, opos, olen), SUBSTR(lin, ipos, ilen));
  files_open := NOT err; lin := SUBSTR(lin, i_pos + i_len)
END;

PROCEDURE auto_debug;
BEGIN (*set debug flag if initially set*)
  IF initial_debug THEN set_dbg
END;

FUNCTION set_up(name: STRING[*]): BOOLEAN;
VAR do_exit, files_open: BOOLEAN;
BEGIN (*set up TTY, INPUT and OUTPUT files*)
  do_start(name);
  files_open := FALSE; do_exit := FALSE;
  REPEAT
    WRITELN; rd_cmd('*', lin);
    IF end_input OR (lin = '') THEN do_exit := TRUE
    ELSE IF abbrev(lin, '?') THEN dpy_info(name)
    ELSE IF abbrev(lin, 'HELP') THEN copy(name || '.HLP', 'TTY:')
    ELSE IF abbrev(lin, 'EXIT') THEN do_exit := TRUE
    ELSE open_files(files_open);
    IF files_open THEN auto_debug;
    IF do_exit THEN CLOSE
  UNTIL files_open OR do_exit;
  set_up := NOT do_exit
END.
 