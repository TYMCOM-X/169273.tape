$PAGE editread
(*   +--------------------------------------------------------------+
     |                                                              |
     |                      E D I T R E A D                         |
     |                      - - - - - - - -                         |
     |                                                              |
     +--------------------------------------------------------------+


     STARTED: 29-Jul-77

     NOTE: This is now a stub that simply calls QREAD.

     CHANGES: Previous line editing removed and a call to QREAD inserted.
              P. Lee, 7/3/79

     ---------------------------------------------------------------- *)
(* All code in the old EDITREAD version 1.5 are preserved in comments.*)
module editread
  options special;
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

(*
external function lineedit ( oldline: qstring ): qstring;
*)

public function editread (oldline: qstring): qstring;

begin

  editread := qread;

end.

(* The following is QED 1.5 EDITREAD code   .   
  label 100;
 var newline: packed array[1..qstringlen] of char;  (* PACKED FOR SPEED *)
     nll: qstringidx;

 begin
  nll := 0;					(* HAVE NO CHARS YET *)
  if eoln (tty) then readln (tty);		(* IF AT END OF LINE, GET NEW LINE *)
  if eof (tty) then begin			(* ERRONEOUS CONDITION *)
    open (tty, '');				(* MUST REOPEN IT *)
    readln (tty)
  end;
  while not eoln (tty) do begin			(* READ EACH CHARACTER *)
    if nll < length (newline) then begin	(* ROOM LEFT IN BUFFER *)
      nll := nll + 1;
      newline [nll] := tty^;
      get (tty)
    end
    else goto 100				(* HAVE OVERFLOWED BUFFER, RETURN WHAT WE HAVE *)
  end;

  if eoln(tty) andif (tty^=chr(7) (*CONTROL-G*)) then begin
    if nll>0 then begin
      writeln(tty); break;
      editread:= substr(newline,1,nll);		(*TO AVOID RUMORED BUG PASSING SUBSTR*)
      editread:= lineedit(editread)
    end
    else editread:= lineedit(oldline);
    return
  end;

 100:
  editread := substr (newline, 1, nll);		(* RETURN VARYING STRING *)
 end.
*)
