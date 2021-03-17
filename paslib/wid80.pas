$WIDTH=100
$LENGTH=55
$TITLE WID80.PAS, last modified 10/14/83, zw

PROGRAM wid80;
(*set VT102/LA50 for 80 character lines*)

$SYSTEM pasutl

PROCEDURE out_chrs(chrs: ARRAY[1 .. *] OF INTEGER);

VAR
    idx: INTEGER;

BEGIN (*output the list of characters, given their numbers*)
  FOR idx := 1 TO UPPERBOUND(chrs) DO
    wrstr(CHR(chrs[idx]))
END;

BEGIN
  IF start('WID80', ';') THEN BEGIN
    out_chrs((27, 91, 63, 51, 108)); (*set VT100 to 80 chars per line*)
    lpton;
    out_chrs((27, 91, 49, 119)); (*set LA50 to 80 chars per line*)
    lptoff
  END
END.
