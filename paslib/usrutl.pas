$WIDTH=100
$LENGTH=55
$TITLE USRUTL.PAS, last modified 10/26/83, zw

MODULE usrutl;
(*TYM-Pascal User Utility*)

$HEADER USRUTL.DOC

$PAGE modules and declarations

$SYSTEM DBGUTL.MOD

$INCLUDE USRUTL.DEC

PUBLIC VAR prognm: FILE_NAME := '';

$PAGE ttystr, ttylin

PUBLIC PROCEDURE ttystr(str: STRING[*]);
BEGIN
  WRITE(TTYOUTPUT, str);
  BREAK(TTYOUTPUT)
END;

PUBLIC PROCEDURE ttylin(lin: STRING[*]);
BEGIN
  WRITELN(TTYOUTPUT, lin);
  BREAK(TTYOUTPUT)
END;

$PAGE rdlin

PUBLIC FUNCTION rdlin(VAR line: STRING[*]): BOOLEAN;
BEGIN
  IF EOF(INPUT) OR (line = '.EOF') THEN line := '.EOF'
  ELSE BEGIN
    IF INPUT = TTY THEN BEGIN
      READLN(INPUT);
      READ(INPUT, line)
    END
    ELSE BEGIN
      READ(INPUT, line);
      READLN(INPUT)
    END
  END;
  rdlin := line <> '.EOF'
END;

$PAGE resume, start

PUBLIC PROCEDURE resume(prognam: FILE_NAME);
BEGIN
  CLOSE;
  OPEN(TTY);
  INPUT := TTY;
  REWRITE(TTYOUTPUT);
  OUTPUT := TTYOUTPUT;
  prognm := prognam;
  IF dbg THEN debug
END;

PUBLIC PROCEDURE start(prognam: FILE_NAME; version: STRING[*]);
BEGIN
  resume(prognam);
  ttylin(prognam || ' ' || version)
END.
