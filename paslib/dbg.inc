$IFNOT dbginc

$SYSTEM setup.typ
$SYSTEM ask.typ
$SYSTEM err.typ
$SYSTEM copy.typ

$INCLUDE dbg.typ

EXTERNAL PROCEDURE open_dbg;
(*open debug log file*)

EXTERNAL PROCEDURE set_dbg;
(*set debug flag according to user's decision*)

EXTERNAL PROCEDURE b(msg: STRING[*]);
(*display message to OUTPUT file, begin block*)

EXTERNAL PROCEDURE e(msg: STRING[*]);
(*end block, display message to OUTPUT file*)

EXTERNAL PROCEDURE d(msg: STRING[*]);
(*display message to OUTPUT file*)

EXTERNAL VAR dbg: BOOLEAN;
(*debug flag set TRUE if debugging*)

$ENABLE dbginc
$ENDIF
 