$PAGE qsplitlines
module qsplitpas
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
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc
public function qsplitlines			(* split selected lines into smaller ones *)
(	var buffer: qbuffer;			(* working buffer *)
	lineno: qlineno;			(* line to split *)
	pat: spattern;				(* where to split each line *)
	opts: split_opt_set;			(* various action-controllers *)
	var cnt: qlineno;			(* number of splits done, i.e. number of new lines *)
	var err: qerrcode			(* error report *)
		): boolean;			(* set true if a match for pat found *)

label 1;

var
  pos,
  len:		qstringidx;			(* for SPATMATCH *)
  source:	qstring;			(* text of lines to be split *)
  tempstr:	qstring;			(* temporary for building lines *)
  doit,
  didit,
  numberit:	boolean;			(* conditionals *)
  newlineno:	qlineno;			(* line counters *)
  idx:		qstringidx;			(* position at which to search for pat *)

begin
  cnt := 0;
  err := qok;
  didit := false;
  qsplitlines := false;
  newlineno := lineno;
  idx := 1;
  source := qgetline (buffer, lineno, err);
  if err <> qok then return;
  numberit := (number_splitop in opts);
  loop
    tempstr := substr (source, idx);
    doit := spatmatch (tempstr, pat, pos, len, err);
  exit if (not doit) or (err <> qok);
    qsplitlines := true;
    if numberit
      then begin
	 writeln (tty, lineno:5);
	numberit := false
      end;
    if confirm_splitop in opts
      then begin
	writeln (tty, substr (source, 1, idx + pos - 1 - 1), '\', substr (source, idx + pos - 1, len),
	  '\', substr (source, idx + pos - 1 + len));
	doit := query ('OK')
      end;
    if doit
      then begin

	(* we take care here to complete this single split operation, so
	   that in the event of a escape being issued to the next
	   confirmation prompt, everything is as it should be up to the
	   point that the escape is issued *)

	if delete_splitop in opts
	  then tempstr := substr (source, 1, idx + pos - 1 - 1)
	  else tempstr := substr (source, 1, idx + pos - 1 + len - 1);
	source := substr (source, idx + pos - 1 + len);
	qaddline (buffer, newlineno, source, err);
	if err <> qok then return;
	qmodline (buffer, newlineno, tempstr, err);
	if err <> qok then return;
	didit := true;
	newlineno := newlineno + 1;
	cnt := cnt + 1;
	idx := 1
      end
      else idx := idx + pos - 1 + len;
    if not ((all_splitop in opts) and (pat.stype in [simple, token]))
      then goto 1
  end;						(* loop *)
1:
  if (print_splitop in opts) and didit
    then qlistlines (buffer, lineno, newlineno, ttyoutput, true, false, true, err);
end.						(* qsplitlines *)
