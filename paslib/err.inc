$IFNOT errinc

$SYSTEM setup.typ
$SYSTEM ask.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ

$INCLUDE err.typ

EXTERNAL PROCEDURE open_err;
(*open error log file*)

EXTERNAL PROCEDURE barf(msg: STRING[*]);
(*display message and STOP*)

EXTERNAL PROCEDURE sgnl_err(msg: STRING[*]);
(*display message to OUTPUT file and TTY, set error flag*)

EXTERNAL VAR err: BOOLEAN;
(*error flag set TRUE when error signaled*)

$ENABLE errinc
$ENDIF
  