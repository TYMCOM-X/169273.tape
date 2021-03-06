$WIDTH=100
$LENGTH=55
$TITLE UTLCHK.PAS, last modified 12/8/83, zw
MODULE utlchk;
(*error check*)

$HEADER utlchk.hdr

PUBLIC PROCEDURE check(cond: BOOLEAN; msg: STRING[*]);
(*check that condition is true, else error*)
BEGIN
  IF NOT cond THEN BEGIN
    REWRITE(TTYOUTPUT);
    WRITELN(TTYOUTPUT, 'check: ', msg);
    BREAK(TTYOUTPUT);
    TRACE;
$IFNOT production
    WRITELN(TTYOUTPUT, 'proceeding after error check...');
    BREAK(TTYOUTPUT);
$ENDIF
$IF production
    WRITELN(TTYOUTPUT, 'Please record the stack dump and give it to');
    WRITELN(TTYOUTPUT, 'a software maintenance person.  Thank you.');
    BREAK(TTYOUTPUT);
    STOP;
$ENDIF
  END
END.
   