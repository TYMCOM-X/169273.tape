
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 141*)
(*TEST 6.6.1-6, CLASS=DEVIANCE*)
(* This program tests the compilers actions for a procedure declared
  as forward, but no matching subsequent procedure declaration
  for the forward procedure occurs. *)
program t6p6p1d6;
var
   c : integer;
procedure two(var b : integer);
   forward;
procedure one(var a : integer);
begin
   a:=a+1;
   if a = 1 then two(a)
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #141');
   c:=0;
   one(c)
   writeln(' DEVIATES...6.6.1-6, FORWARD PROCEDURE');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
