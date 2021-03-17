
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 161*)
(*TEST 6.6.3.5-2, CLASS=DEVIANCE*)
(* This test checks functional compatibility in that function
  types are required to be identical. The compiler deviates if the
  program compiles and prints DEVIATES. *)
program t6p6p3p5d2;
type
   natural=0..maximum(integer);
var
   k:integer;
function actual(i:natural):natural;
begin
   actual:=i
end;
procedure p(function formal(i:natural):integer);
begin
   k:=formal(10)
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #161');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   p(actual);
   writeln(' DEVIATES...6.6.3.5-2, FUNC TYPES NOT IDENTICAL')
end.
