
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  49*)
(*TEST 6.2.2-9, CLASS=DEVIANCE*)
(* This program attempts to assign a value to a function outside
  the bounds of the function. The compiler deviates if the
  program prints DEVIATES. *)
program t6p2p2d9;
var
   i:integer;
function f1:integer;
begin
   f1:=6
end;
function f2(i:integer):integer;
begin
   f2:=i;
   f1:=5
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #049');
   i:=f1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   i:=f2(2);
   writeln(' DEVIATES...6.2.2-9, FUNCTION')
end.
  