PROGRAM args;
  (*echo command line arguments*)

EXTERNAL FUNCTION star(prognam, hlpfil: STRING[*]): BOOLEAN;
EXTERNAL FUNCTION arg(n: INTEGER; VAR val: STRING[*]): BOOLEAN;

VAR val: STRING[10];
VAR n: INTEGER;

BEGIN
  WHILE star('ARG', '') DO BEGIN
    n := 1;
    WHILE arg(n, val) DO BEGIN
      WRITELN('arg #', n:0, ' is "', val, '"');
      n := n + 1
      END
    END
  END.
  