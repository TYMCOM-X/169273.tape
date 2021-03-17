MODULE dbg;
(*debugging routines*)

$SYSTEM setup
$SYSTEM ask

$INCLUDE dbg.typ

PUBLIC PROCEDURE open_dbg; FORWARD;
(*open debug log file*)

PUBLIC PROCEDURE set_dbg; FORWARD;
(*set debug flag according to user's decision*)

PUBLIC PROCEDURE b(msg: STRING[*]); FORWARD;
(*display message to OUTPUT file, begin block*)

PUBLIC PROCEDURE e(msg: STRING[*]); FORWARD;
(*end block, display message to OUTPUT file*)

PUBLIC PROCEDURE d(msg: STRING[*]); FORWARD;
(*display message to OUTPUT file*)

PUBLIC VAR dbg: BOOLEAN := FALSE;
(*debug flag set TRUE if debugging*)

CONST dbg_fil_nam = 'DBG.LOG';

VAR
ind: INTEGER := 0;
dbg_fil: TEXT := NILF;

PROCEDURE open_dbg;
BEGIN (*open debug log file*)
  RESET(dbg_fil, dbg_fil_nam, [RETRY]);
  IF IOSTATUS = IO_OK THEN BEGIN
    CLOSE(dbg_fil); REWRITE(dbg_fil, dbg_fil_nam, [PRESERVE])
  END
  ELSE dbg_fil := NILF
END;

PROCEDURE set_dbg;
VAR resp: INTEGER;
BEGIN (*set debug flag according to user's decision*)
  ask_tty('Debug?', ('YES', 'NO'), resp); dbg := resp IN [1];
  IF dbg THEN d('[debugging]')
END;

PROCEDURE b(msg: STRING[*]);
BEGIN (*display message to OUTPUT file, begin block*)
  IF dbg THEN BEGIN d(msg); ind := SUCC(ind) END
END;

PROCEDURE e(msg: STRING[*]);
BEGIN (*end block, display message to OUTPUT file*)
  IF dbg THEN BEGIN ind := PRED(ind); d(msg) END
END;

PROCEDURE d(msg: STRING[*]);
BEGIN (*display message to OUTPUT file*)
  IF dbg THEN BEGIN
    tty_on; WRITELN(' ': ind * 2, msg); BREAK; tty_off;
    IF dbg_fil <> NILF THEN WRITELN(dbg_fil, ' ': ind * 2, msg)
  END
END.
