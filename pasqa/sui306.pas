
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 306*)
(*TEST 6.9.4-13, CLASS=CONFORMANCE*)
(* This program attempts to perform recursive I/O using a
  different file for the second I/O action. *)
program t6p9p4d13;
var
   f:text;
function a(i:integer):integer;
begin
   writeln(f,i);
   a:=i;
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #306');
   rewrite(f);
   writeln(a(1));
   writeln(' RECURSIVE I/O ALLOWED USING DIFFERENT FILES');
   writeln(' PASS...6.9.4-13, RECURSIVE I/O');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

