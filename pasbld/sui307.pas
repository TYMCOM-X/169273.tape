
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 307*)
(*TEST 6.9.4-14, CLASS=QUALITY*)
(* This program attempts to perform recursive I/O using the
  same file for the second I/O action.
  The semantics of write are not sufficiently well-defined to
  establish what should occur. It depends on evaluation orders,
  etc., which is why this test is in the quality section. *)
program t6p9p4d14;
function a(i:integer):integer;
begin
   writeln(i);
   a:=i;
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #307');
   writeln(a(1));
   writeln('RECURSIVE I/O ALLOWED USING THE SAME FILE...6.9.4-14');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

