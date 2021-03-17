$page QJOINLINES
module qjoin
  options special;
public procedure qjoinlines			(* turn two (or more) lines into one *)
(	var buffer: qbuffer;			(* working buffer *)
	first,
	last: qlineno;				(* range of lines to join *)
	contmark: qstring;			(* string to replace CRs with *)
	var err: qerrcode);			(* error report *)

var
  source:	qstring;			(* text of lines to be joined *)
  result:	qstring;			(* line formed by joining others *)
  lineno:	qlineno;			(* counter to step through lines joined *)

begin
  if first >= last
    then begin
      err := qjointoofew;
      return
    end;
  result := qgetline (buffer, first, err);
  if err <> qok then return;
  for lineno := (first + 1) to last do
  begin
    source := qgetline (buffer, lineno, err);
    if err <> qok then return;
    if length (result || source) > qstringlen
      then begin
	err := qlnlong;
	return
      end;
    result := result || contmark || source
  end;
  qmodline (buffer, first, result, err);
  if err <> qok then return;
  qdellines (buffer, first + 1, last, err)
end.						(* qjoinlines *)
