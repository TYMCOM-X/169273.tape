PROGRAM filnam;

VAR
name: FILE_NAME;
tmp: FILE OF *;

BEGIN
  OPEN(TTY);
  REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT, 'FILE NAME VERIFICATION');
  REPEAT
    WRITE(TTYOUTPUT, 'NAME: ');
    BREAK(TTYOUTPUT);
    READLN(TTY);
    READ(TTY, name);
    IF name <> '' THEN BEGIN
      RESET(tmp, name);
      IF FILENAME(tmp) = '' THEN REWRITE(tmp, name);
      WRITELN(TTYOUTPUT, FILENAME(tmp));
      CLOSE(tmp)
    END
  UNTIL name = ''
END.
  