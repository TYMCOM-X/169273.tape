
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  32*)
(*TEST 6.2.1-1, CLASS=CONFORMANCE*)
(* This program includes a sample of each declaration
  part in its minimal form.  Every possibility is covered elsewhere
  in the validation suite, but the test is made here. *)
program t6p2p1d1;
label
   1;
const
   one = 1;
type
   small = 1..3;
var
   tiny : small;
procedure p(var x : small);
begin
   x:=1
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #032');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   goto 1;
1: p(tiny);
   if (tiny = one) then
      writeln(' PASS...6.2.1-1')
end.
