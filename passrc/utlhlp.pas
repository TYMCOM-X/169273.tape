$WIDTH=100
$LENGTH=55
$TITLE UTLHLP.PAS, last modified 1/9/84, zw
MODULE utlhlp;
(*"help" utility*)

$HEADER UTLHLP.HDR

$SYSTEM CMDUTL.INC

CONST mrk = '$TOPIC'; lin_siz = 80;
TYPE str = PACKED ARRAY [1 .. *] OF CHAR;

$PAGE makhlp

PUBLIC PROCEDURE makhlp(inam, onam: FILE_NAME);
VAR fil: FILE OF *; i, j, len: INTEGER; dir: ^ARRAY[1..*] OF cmd_lookup_record;
  lin: STRING[lin_siz]; txt: PACKED ARRAY[1 .. lin_siz] OF CHAR;
FUNCTION is_tpc(lin: STRING[*]): BOOLEAN;
BEGIN
;WRITELN(TTY, 'lin = "', lin, '"'); BREAK(TTY);
  is_tpc := (lin <> '') ANDIF
    (UPPERCASE(SUBSTR(lin, 1, MIN(LENGTH(lin), LENGTH(mrk)))) = mrk)
;IF is_tpc THEN WRITELN(TTY, 'is_tpc = TRUE')
ELSE WRITELN(TTY, 'is_tpc = FALSE');
BREAK(TTY);
END;
PROCEDURE get_tpc(VAR tpc: PACKED ARRAY[1 .. *] OF CHAR; lin: STRING[*]);
VAR pos, len: INTEGER;
BEGIN
;WRITELN(TTY, 'lin = "', lin, '"'); BREAK(TTY);
  IF NOT is_tpc(lin) THEN tpc := ''
  ELSE BEGIN
    pos := VERIFY(SUBSTR(lin, LENGTH(mrk) + 1), [' ']) + LENGTH(mrk);
;WRITELN(TTY, 'pos = ', pos); BREAK(TTY);
    len := SEARCH(SUBSTR(lin, pos) || ' ', [' ']) - 1;
;WRITELN(TTY, 'len = ', len); BREAK(TTY);
    tpc := UPPERCASE(SUBSTR(lin, pos, MIN(len, UPPERBOUND(tpc))))
  END
;WRITELN(TTY, 'tpc = "', tpc, '"'); BREAK(TTY);
END;
BEGIN
  RESET(INPUT, inam); i := 0;
  WHILE NOT EOF DO BEGIN READLN(lin); IF is_tpc(lin) THEN i := i + 1 END;
;WRITELN(TTY, 'number of topics = ', i); BREAK(TTY);
  CLOSE(INPUT); RESET(INPUT, inam); REWRITE(fil, onam, [SEEKOK]); NEW(dir, i);
  WRITE(fil, i); j := CURSOR(fil); WRITE(fil, dir^: SIZE(dir^, i)); i := 0;
  REPEAT READLN(lin) UNTIL EOF OR is_tpc(lin);
  WHILE NOT EOF DO BEGIN
    IF is_tpc(lin) THEN BEGIN
      WRITE(fil, -1);
      i := i + 1; get_tpc(dir^[i].text, lin); dir^[i].code := CURSOR(fil);
      WRITELN(TTYOUTPUT, 'topic: ', dir^[i].text); BREAK(TTYOUTPUT);
      dir^[i].abbrev := UPPERBOUND(dir^[i].text)
    END
    ELSE BEGIN
      txt := lin; len := LENGTH(lin);
      WRITE(fil, len, txt: SIZE(str, len)) 
    END;
    READLN(lin)
  END;
  WRITERN(fil, j, dir^: SIZE(dir^, UPPERBOUND(dir^)));
  CLOSE(INPUT); CLOSE(fil); DISPOSE(dir)
END;

$PAGE hlp

PUBLIC FUNCTION hlp(cmd: STRING[*]; VAR idx: INTEGER; nam: FILE_NAME): BOOLEAN;
VAR fil: FILE OF *; i, j: INTEGER; dir: ^ARRAY[1..*] OF cmd_lookup_record;
PROCEDURE dpy_msg(pos: INTEGER);
VAR len: INTEGER; txt: PACKED ARRAY [1 .. lin_siz] OF CHAR;
BEGIN
  READRN(fil, pos, len);
  WHILE len > -1 DO BEGIN
    READ(fil, txt: SIZE(str, len)); WRITELN(SUBSTR(txt, 1, len));
    READ(fil, len)
  END
END;
PROCEDURE hlp_tpc;
VAR code: INTEGER; tkn: STRING[10];
BEGIN
  IF cmd_lookup(cmd, idx, ['A'..'Z'], dir^, code) THEN dpy_msg(code)
  ELSE IF cmd_token(cmd, idx, ['A'..'Z'], tkn) THEN
    WRITELN('%No help available on: ' || tkn)
  ELSE BEGIN
    WRITELN('%HELP syntax is: HELP[:] [<topic> | ( <topic> [,<topic> ...] )]');
    WRITELN('[Use "HELP *" to see all topics.]')
  END
END;
BEGIN
  RESET(fil, nam, [SEEKOK]); hlp := (IOSTATUS = IO_OK);
  IF hlp THEN BEGIN
    REWRITE(OUTPUT, 'TTY:');
    READ(fil, i); NEW(dir, i); READ(fil, dir^: SIZE(dir^, i));
    IF cmd_check_punct(cmd, idx, ':') THEN;
    IF cmd_check_punct(cmd, idx, '*') THEN
      FOR i := 1 TO UPPERBOUND(dir^) DO dpy_msg(dir^[i].code)
    ELSE IF cmd_check_punct(cmd, idx, '(') THEN BEGIN
      REPEAT hlp_tpc UNTIL NOT cmd_check_punct(cmd, idx, ',');
      IF NOT cmd_check_punct(cmd, idx, ')') THEN
	WRITELN('%HELP syntax expects a ")" after the last topic')
      END
    ELSE hlp_tpc;
    DISPOSE(dir); OUTPUT := NILF
  END;
  CLOSE(fil)
END.
