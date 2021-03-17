
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 234*)
(*TEST 6.8.2.2-1, CLASS=IMPLEMENTATIONDEFINED*)
(* This program determines whether selection of a variable involving
  the indexing of an array occurs before or after the evaluation
  of the expression in an assignment statement. *)
program t6p8p2p2d1;
var
   i : integer;
   a : array[1..3] of integer;
function sideeffect(var i:integer) : integer;
begin
   i:=i+1;
   sideeffect:=i
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #234');
   writeln(' TEST OF BINDING ORDER (A[I] := EXP)');
   i:=1;
   a[1]:=0;
   a[2]:=0;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   a[i]:=sideeffect(i);
   if a[1]=2 then
      writeln(' SELECTION THEN EVALUATION...6.8.2.2-1')
   else
      if a[2]=2 then
         writeln(' EVALUATION THEN SELECTION...6.8.2.2-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

