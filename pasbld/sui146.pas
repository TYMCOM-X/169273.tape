
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 146*)
(*TEST 6.6.2-5, CLASS=DEVIANCE*)
(* The Pascal Standard specifies that at least one assignment
  statement which assigns a value to the function identifier
  must occur in the function block.
  Does the compiler permit a function declaration with
  no assignment to the function identifier?
  The compiler deviates if it does. *)
program t6p6p2d5;
var
   a : integer;
function illegal(var b : integer) : integer;
var
   x : integer;
begin
   x:=b*2
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #146');
   a:=2;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   a:=illegal(a);
   writeln(' DEVIATES...6.6.2-5')
end.
