PROGRAM chrcnt;
  (*count characters in standard input*)

$SYSTEM TOOLS

VAR c: CHAR;
VAR nc: INTEGER;

BEGIN
  setup('CHARACTER COUNT');
  WHILE getio DO BEGIN
    nc := 0;
    WHILE getc(c) DO nc := nc + 1;
    putdec(nc, 1);
    putc(newline)
    END
  END.
  