$TITLE FLGTST.PAS, last modified 4/11/84, zw
PROGRAM flgtst;
(*TYM-Pascal test for FLGUTL*)

$HEADER FLGUTL.HDR

$SYSTEM TYPUTL.TYP
$INCLUDE FLGUTL.INC

VAR
flags: flag_pointer := NIL;
flag_marker: flag_pointer;
flag_file: binary_file;

PROCEDURE error;
BEGIN
  WRITELN(TTYOUTPUT, 'TEST FAILS');
  STOP
END;

BEGIN
  OPEN(TTY);
  REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT, 'TEST OF FLGUTL');
  IF flag(flags, 'FLAG1') THEN error;
  setflg(flags, 'FLAG1', yes);
  IF NOT flag(flags, 'FLAG1') THEN error;
  IF flag(flags, 'FLAG2') THEN error;
  setflg(flags, 'FLAG2', yes);
  IF NOT flag(flags, 'FLAG2') THEN error;
  IF NOT flag(flags, 'FLAG1') THEN error;
  flag_marker := flags;
  setflg(flags, 'FLAG1', no);
  IF flag(flags, 'FLAG1') THEN error;
  delflg(flags, flag_marker);
  IF NOT flag(flags, 'FLAG1') THEN error;
  REWRITE(flag_file, 'FLGTST.TMP');
  stflgs(flags, flag_file);
  CLOSE(flag_file);
  IF flag(flags, 'FLAG1') THEN error;
  RESET(flag_file, 'FLGTST.TMP');
  ldflgs(flags, flag_file);
  SCRATCH(flag_file);
  IF NOT flag(flags, 'FLAG2') THEN error;
  IF NOT flag(flags, 'FLAG1') THEN error;
  WRITELN(TTYOUTPUT, 'TEST SUCCEEDS');
END.
 