
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 104*)
(*TEST 6.4.4-3, CLASS=DEVIANCE*)
(* Pointers to items in the stack are not allowed.
  The ^ symbol is not permitted to act as an operator
  giving the reference to a variable. The compiler deviates
  if the program compiler and prints DEVIATES. *)
program t6p4p4d3;
var
   p: ^integer;
   x:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #104');
   x:=10;
   p:=^x;
   writeln(' DEVIATES...6.4.4-3, POINTER')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    