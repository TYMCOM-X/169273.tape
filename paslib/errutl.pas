$WIDTH=100
$LENGTH=55
$TITLE ERRUTL.PAS, last modified 10/26/83, zw

MODULE errutl;
(*Error Utility*)

$HEADER ERRUTL.DOC

$PAGE modules and declarations

$SYSTEM USRUTL.MOD

$INCLUDE ERRUTL.DEC

PUBLIC VAR err: BOOLEAN := FALSE;

$PAGE error, fatal

PUBLIC PROCEDURE error(msg: STRING[*]);
BEGIN
  ttylin('error: ' || msg);
  err := TRUE
END;

PUBLIC PROCEDURE fatal(msg: STRING[*]);
BEGIN
  ttylin('fatal: ' || msg);
  STOP
END;

$PAGE chkerr, chkftl, assume

PUBLIC PROCEDURE chkerr(cond: BOOLEAN; msg: STRING[*]);
BEGIN
  IF cond THEN error(msg)
END;

PUBLIC PROCEDURE chkftl(cond: BOOLEAN; msg: STRING[*]);
BEGIN
  IF cond THEN fatal(msg)
END;

PUBLIC PROCEDURE assume(cond: BOOLEAN; msg: STRING[*]);
BEGIN
  chkftl(NOT cond, msg)
END.

