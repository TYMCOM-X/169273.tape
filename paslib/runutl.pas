$WIDTH=100
$LENGTH=55
$TITLE RUNUTL.PAS, last modified 10/26/83, zw

MODULE runutl;
(*Program Execution Utility*)

$HEADER RUNUTL.DOC

$PAGE modules and declarations

$SYSTEM USRUTL.MOD

$INCLUDE RUNUTL.DEC

$PAGE run

PUBLIC PROCEDURE run(prognam: FILE_NAME);
BEGIN
  ttylin('');
  ttylin('Please run: ' || prognam);
  CLOSE;
  STOP
END.
  