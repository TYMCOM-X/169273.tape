$TITLE edt.PAS, last modified 2/16/84, zw
PROGRAM edt OPTIONS STORAGE(3072);
(*driver program for line editor*)
$SYSTEM VERSIO.INC
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qederr.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

VAR buffer: qbuffer; (*working buffer*)

BEGIN
  OPEN(TTY, [ASCII]); REWRITE(TTYOUTPUT);
  WRITELN(TTY, 'TYM-Pascal Text Editor, Version ', version());
  WRITELN(TTY);
  TTY^ := cr; (*initialize fake line end*)
  qinitexec(buffer); (*init buffer and editor parameters*)
  REPEAT
    qedcl(buffer, [MINIMUM(qedcmds) .. MAXIMUM(qedcmds)])
  UNTIL (NOT buffer.changes) ORIF query('Unwritten changes, OK')
END.
   