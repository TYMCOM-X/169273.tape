PROGRAM wrdcnt;
  (*count words in standard input*)

$SYSTEM TOOLS

VAR c: CHAR;
VAR wc: INTEGER;
VAR inword: BOOLEAN;

BEGIN
  setup('WORD COUNT');
  WHILE getio DO BEGIN
    wc := 0;
    inword := FALSE;
    WHILE getc(c) DO
      IF (c = blank) OR (c = newline) OR (c = tab) THEN inword := FALSE
      ELSE IF NOT inword THEN BEGIN
        inword := TRUE;
        wc := wc + 1
        END;
    putdec(wc, 1);
    putc(newline)
    END
  END.
 