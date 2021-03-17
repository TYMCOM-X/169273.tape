$TITLE RUNTST.PAS, last modified 4/9/84, zw
PROGRAM runtst;
(*TYM-Pascal test for RUNUTL*)

$HEADER RUNUTL.HDR

$INCLUDE RUNUTL.INC

PROCEDURE error;
BEGIN
  REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT, 'TEST FAILS')
END;

BEGIN
  REWRITE(TTYOUTPUT);
  CASE runoff OF
    0: BEGIN
      WRITELN(TTYOUTPUT, 'TEST OF RUNUTL');
      IF NOT runprg('RUNTST', 1) THEN error
    END;
    1: IF NOT runprg('RUNTST', 2) THEN error;
    2: IF NOT runprg('RUNTST', 3) THEN error;
    3: IF NOT runprg('RUNTST', 4) THEN error;
    4: IF NOT runprg('RUNTST', 5) THEN error;
    5: BEGIN
      IF runprg('TSTRUN', 0) THEN error;
      REWRITE(TTYOUTPUT);
      WRITE(TTYOUTPUT, 'TEST SUCCEEDS')
    END;
  END
END.
