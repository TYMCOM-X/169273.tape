$WIDTH=100
$LENGTH=55
$TITLE UTLIO.PAS, last modified 2/6/84, zw
MODULE utlio;
(*TYM-Pascal input/output utility*)

$HEADER UTLIO.HDR

$INCLUDE UTLIO.TYP

$PAGE definitions

CONST limit_open_input_files := 3;

TYPE
file_list_pointer = ^file_list_node;
file_list_node = RECORD
  nam: FILE_NAME; linnum: INTEGER; next: file_list_pointer
END;
file_stack_pointer = ^file_stack_node;
file_stack_node = RECORD
  fil: TEXT; nam: FILE_NAME; next: file_stack_pointer
END;

VAR
tty_is_open: BOOLEAN := FALSE; (*implies that tty i/o is open*)
input_is_open: BOOLEAN := FALSE; (*implies that input file is open*)
output_is_open: BOOLEAN := FALSE; (*implies that output file is open*)
tty_on_stack: INTEGER := 0; (*count of calls to ttyon*)
tty_ok_stack: INTEGER := 0; (*count of calls to ttyok(FALSE)*)
saved: RECORD (*a place to save things while tty is on*)
  input, output: TEXT; linnum, tty_on_stack: INTEGER;
END;
file_list: file_list_pointer := NIL; (*for remembering input file positions*)
file_stack: file_stack_pointer := NIL; (*for stacking input files*)
io_is_ready: BOOLEAN := FALSE; (*flag un-initialized execution*)

PUBLIC PROCEDURE assume(cond: BOOLEAN; msg: STRING[*]); FORWARD;

$PAGE linnum, bymsg, rdyio

PUBLIC VAR
linnum: INTEGER (*count of lines read from input*)
  := 0;
byemsg: STRING[80] (*second part of byebye message*)
  := '? abnormal program exit, ready for stack dump';

PUBLIC PROCEDURE rdyio;
(*ready input/output utility*)
BEGIN
  CLOSE;
  INPUT := NILF;
  input_is_open := FALSE;
  OUTPUT := NILF;
  output_is_open := FALSE;
  tty_on_stack := 0;
  tty_ok_stack := 0;
  io_is_ready := TRUE
END;

$PAGE clstty, opntty, ttyoff, ttyon

PUBLIC PROCEDURE clstty;
(*close terminal input and output*)
BEGIN
  CLOSE(TTY); CLOSE(TTYOUTPUT);
  tty_is_open := FALSE
END;

PUBLIC PROCEDURE opntty;
(*open terminal for input and output*)
BEGIN
  OPEN(TTY); REWRITE(TTYOUTPUT);
  tty_is_open := TRUE
END;

PUBLIC PROCEDURE ttyoff;
(*restore input/output after ttyon*)
BEGIN
  IF tty_on_stack > 0 THEN BEGIN (*tty is currently on*)
    tty_on_stack := tty_on_stack - 1; (*unstack ttyon call*)
    IF tty_on_stack < 1 THEN BEGIN (*end of stack, turn tty off*)
      INPUT := saved.input; OUTPUT := saved.output; linnum := saved.linnum
    END
  END
END;

PUBLIC PROCEDURE ttyon;
(*temporarily turn input/output to terminal*)
BEGIN
  assume(io_is_ready, '? terminal I/O not ready');
  assume(tty_ok_stack = 0, '? not ok for tty to be on');
  IF tty_on_stack < 1 THEN BEGIN (*was not on before*)
    saved.input := INPUT; saved.output := OUTPUT; saved.linnum := linnum;
    opntty; (*make sure tty is open*)
    INPUT := TTY; OUTPUT := TTYOUTPUT;
  END;
  tty_on_stack := tty_on_stack + 1 (*stack ttyon call*)
END;

$PAGE ttyok

PUBLIC PROCEDURE ttyok(ok: BOOLEAN);
(*is it ok for tty to be on?*)
BEGIN
  IF ok THEN BEGIN (*ok for tty to be on*)
    IF tty_ok_stack > 0 THEN BEGIN (*was not ok for tty to be on before*)
      tty_ok_stack := tty_ok_stack - 1; (*unstack ttyok() call*)
      IF tty_ok_stack < 1 THEN BEGIN (*end of stack*)
        IF saved.tty_on_stack > 0 THEN BEGIN (*was on before--restore*)
          tty_on_stack := 0; ttyon; (*force tty on*)
	  tty_on_stack := saved.tty_on_stack (*restore tty on stack*)
        END
      END
    END
  END
  ELSE BEGIN (*not ok for tty to be on*)
    IF tty_ok_stack < 1 THEN BEGIN (*end of stack--was ok to be on before*)
      IF tty_on_stack > 0 THEN BEGIN (*is on now--turn off*)
        saved.tty_on_stack := tty_on_stack; (*remember tty on stack*)
	tty_on_stack := 1; ttyoff; (*force tty off*)
      END
    END;
    tty_ok_stack := tty_ok_stack + 1 (*stack ttyok() call*)
  END
END;

$PAGE wrstr, wrlin, rdlin, skprd

PUBLIC PROCEDURE wrnum(n: REAL; a, b: INTEGER);
(*write real number to output*)
BEGIN
  WRITE(n: a: b); IF OUTPUT = TTYOUTPUT THEN BREAK
END;

PUBLIC PROCEDURE wrint(i: INTEGER; a: INTEGER);
(*write integer to output*)
BEGIN
  WRITE(i: a); IF OUTPUT = TTYOUTPUT THEN BREAK
END;

PUBLIC PROCEDURE wrspc(n: INTEGER);
(*write n spaces to output*)
BEGIN
  WRITE(' ': n); IF OUTPUT = TTYOUTPUT THEN BREAK
END;

PUBLIC PROCEDURE wrstr(str: STRING[*]);
(*write string to output*)
BEGIN
  WRITE(str); IF OUTPUT = TTYOUTPUT THEN BREAK
END;

PUBLIC PROCEDURE wrlin(lin: STRING[*]);
(*write line to output*)
BEGIN
  WRITELN(lin); IF OUTPUT = TTYOUTPUT THEN BREAK
END;

PUBLIC FUNCTION rdlin(VAR lin: STRING[*]): BOOLEAN;
(*try to read line from input -- file or terminal*)
BEGIN
  assume(io_is_ready, '? input not ready');
  rdlin := (INPUT <> NILF) ANDIF NOT EOF(INPUT);
  IF rdlin THEN BEGIN
    IF INPUT <> TTY THEN READLN(lin)
    ELSE BEGIN READLN; READ(lin) END;
    linnum := linnum + 1
  END
END;

PUBLIC FUNCTION skprd(n: INTEGER): BOOLEAN;
(*try to skip n lines of input*)
VAR i: INTEGER; lin: STRING[1];
BEGIN
  skprd := TRUE; FOR i := 1 TO n DO EXIT IF NOT rdlin(lin) DO skprd := FALSE
END;

$PAGE ask, asktty, ttymsg, byebye, assume

PUBLIC PROCEDURE ask(q: STRING[*]; VAR a: STRING[*]);
(*ask question, get answer*)
PROCEDURE prompt(q: STRING[*]);
BEGIN wrlin(''); wrstr(q); BREAK END;
BEGIN
  IF (INPUT = TTY) AND (OUTPUT = TTYOUTPUT) THEN prompt(q);
  IF NOT rdlin(a) THEN a := ''
END;

PUBLIC PROCEDURE asktty(q: STRING[*]; VAR a: STRING[*]);
(*ask terminal a question, get answer*)
BEGIN
  ttyon; ask(q, a); ttyoff
END;

PUBLIC PROCEDURE ttymsg(msg: STRING[*]);
(*send message to terminal*)
BEGIN
  ttyon; wrlin(''); wrlin(msg); ttyoff
END;

PUBLIC PROCEDURE byebye(msg: STRING[*]);
(*display message and halt*)
BEGIN
  rdyio; ttymsg(msg); IF byemsg <> '' THEN ttymsg(byemsg); STOP
END;

PUBLIC PROCEDURE assume(cond: BOOLEAN; msg: STRING[*]);
(*assume condition is true else go byebye*)
BEGIN
  IF NOT cond THEN byebye(msg)
END;

$PAGE clsfil, opnfil

PUBLIC PROCEDURE clsfil(VAR fil: TEXT);
(*close text file*)
BEGIN
  IF fil <> NILF  THEN BEGIN
    IF (fil <> TTY) ANDIF (fil <> TTYOUTPUT) THEN CLOSE(fil);
    fil := NILF
  END
END;

PUBLIC FUNCTION opnfil(VAR fil: TEXT; nam: FILE_NAME; mode: opn_mod): BOOLEAN;
(*try to open text file for specified access*)
BEGIN
  IF UPPERCASE(nam) = 'TTY:' THEN BEGIN
    opntty; opnfil := TRUE;
    IF mode = read_mode THEN fil := TTY ELSE fil := TTYOUTPUT
  END
  ELSE BEGIN
    CASE mode OF
      read_mode: RESET(fil, nam);
      write_mode: REWRITE(fil, nam);
      append_mode: REWRITE(fil, nam, [PRESERVE])
    END;
    opnfil := IOSTATUS = IO_OK
  END;
  IF NOT opnfil THEN fil := NILF
END;

$PAGE clsout, opnout

PUBLIC PROCEDURE clsout;
(*close output file*)
BEGIN
  assume(io_is_ready, '? output not ready');
  ttyok(FALSE);
  IF output_is_open THEN BEGIN
    assume(OUTPUT <> NILF, '? can not close null output');
    clsfil(OUTPUT); output_is_open := FALSE
  END;
  OUTPUT := NILF;
  ttyok(TRUE)
END;

PUBLIC FUNCTION opnout(nam: FILE_NAME; new_fil: BOOLEAN): BOOLEAN;
(*try to open output file*)
VAR mode: ARRAY [BOOLEAN] OF opn_mod;
BEGIN
  ttyok(FALSE);
  clsout;
  mode[TRUE] := write_mode; mode[FALSE] := append_mode;
  opnout := opnfil(OUTPUT, nam, mode[new_fil]); output_is_open := opnout;
  IF NOT output_is_open THEN OUTPUT := NILF;
  ttyok(TRUE)
END;

$PAGE clsin, opnin

PUBLIC PROCEDURE clsin;
(*close input file, record position in file list*)
VAR tmp: file_list_pointer; nam: FILE_NAME;
BEGIN
  assume(io_is_ready, '? input not ready');
  ttyok(FALSE);
  IF input_is_open THEN BEGIN
    assume(INPUT <> NILF, '? null input file');
    IF INPUT <> TTY THEN BEGIN (*record line position*)
      nam := FILENAME(INPUT); tmp := file_list;
      WHILE tmp <> NIL DO BEGIN (*search for file in file list*)
        EXIT IF tmp^.nam = nam; tmp := tmp^.next
      END;
      IF tmp = NIL THEN BEGIN (*insert new entry in file list*)
        NEW(tmp); tmp^.nam := nam; tmp^.next := file_list; file_list := tmp
      END;
      tmp^.linnum := linnum (*remember current line position within file*)
    END;
    clsfil(INPUT); input_is_open := FALSE
  END;
  INPUT := NILF;
  ttyok(TRUE)
END;

PUBLIC FUNCTION opnin(nam: FILE_NAME; new_fil: BOOLEAN): BOOLEAN;
(*try to open input file*)
VAR tmp: file_list_pointer; tmp_nam: FILE_NAME;
BEGIN
  ttyok(FALSE);
  clsin; input_is_open := opnfil(INPUT, nam, read_mode);
  linnum := 0; (*reset line counter*)
  IF input_is_open THEN BEGIN
    IF NOT new_fil AND (INPUT <> TTY) THEN BEGIN
      assume(INPUT <> NILF, '? can not re-open null input file');
      tmp_nam := FILENAME(INPUT); tmp := file_list; 
      WHILE tmp <> NIL DO BEGIN (*search for file in file list*)
        EXIT IF tmp^.nam = tmp_nam; tmp := tmp^.next
      END;
      assume(tmp <> NIL, '? file not in list: "' || tmp_nam || '"');
      assume(skprd(tmp^.linnum), '? lost lines in file: "' || tmp_nam || '"')
    END
  END
  ELSE INPUT := NILF;
  opnin := input_is_open;
  ttyok(TRUE)
END;

$PAGE popin, pushin

PUBLIC FUNCTION popin: BOOLEAN;
(*close current input file and try to pop to previous input file*)
VAR next: file_stack_pointer;
BEGIN
  ttyok(FALSE);
  clsin;
  popin := file_stack <> NIL;
  IF popin THEN BEGIN (*there is a previous input*)
    INPUT := file_stack^.fil; input_is_open := INPUT <> NILF;
    IF NOT input_is_open THEN assume(opnin(file_stack^.nam, FALSE),
      '? can not pop open input file: "' || file_stack^.nam || '"');
    next := file_stack^.next; DISPOSE(file_stack); file_stack := next;
  END;
  ttyok(TRUE)
END;

PUBLIC FUNCTION pushin(fil: FILE_NAME): BOOLEAN;
(*try to push current input, open new file*)
VAR tmp: file_stack_pointer; i: INTEGER;
BEGIN
  ttyok(FALSE);
  IF input_is_open THEN BEGIN (*there is an input file to push*)
    assume(INPUT <> NILF, '? can not push null input file');
    NEW(tmp); tmp^.next := file_stack; file_stack := tmp;
    tmp^.fil := INPUT; INPUT := NILF; input_is_open := FALSE;
    IF tmp^.fil = TTY THEN tmp^.nam := 'TTY:'
    ELSE tmp^.nam := FILENAME(tmp^.fil);
    FOR i := 1 TO limit_open_input_files DO BEGIN
      EXIT IF tmp = NIL; tmp := tmp^.next
    END;
    IF tmp <> NIL THEN clsfil(tmp^.fil)
  END;
  pushin := opnin(fil, TRUE);
  IF NOT pushin AND (file_stack <> NIL)
  THEN assume(popin, '? lost file stack');
  ttyok(TRUE)
END.
