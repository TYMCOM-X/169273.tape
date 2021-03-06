$PAGE qmarkmatch
module qmarkpas
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
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc
public procedure qmarkmatch
(	var buffer: qbuffer;			(* working buffer *)
	mark: spred;				(* markstring to search for *)
	sect_name: spred;			(* in conjunction with mark *)
	start: qlineno;				(* where to start looking *)
	var fno, lno: qlineno;			(* limits of the marked section *)
        backward: boolean;                      (* backward search flag *)
	wrap: boolean;				(* wrap/no-wrap around flag *)
	var err: qerrcode);			(* error report *)

var
  lineno: qlineno;
  line: qstring;
  increment: -1..1;

begin
  lineno := start;
  if backward then increment := -1
              else increment := 1;
  loop
    line := qgetline (buffer, lineno, err);
    if err <> qok then return;
  exit if spredmatch (line, mark, err) andif spredmatch (line, sect_name, err);
    if err <> qok then return;
    lineno := lineno + increment;
    if (lineno > buffer.hbound) and wrap then lineno := buffer.lbound
      else if (lineno < buffer.lbound) and wrap then lineno := buffer.hbound;
    if lineno = start then
    begin
      err := qnomark;
      return
    end
  end;
  fno := lineno;
  if lineno <> buffer.hbound then
  begin
    loop
      lineno := lineno + 1;
      line := qgetline (buffer, lineno, err);
      if err <> qok then return;
    exit if spredmatch (line, mark, err) do lineno := lineno - 1;
    exit if lineno = buffer.hbound;
      if err <> qok then return
    end;
  end;
  lno := lineno
end.						(* qmarkmatch *)
   