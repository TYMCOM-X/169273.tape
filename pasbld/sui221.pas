
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 221*)
(*TEST 6.7.2.2-9, CLASS=DEVIANCE*)
(* The unary operator plus can clearly only be applied to numeric
  operands. Hence this program should fail to compile.
  The compiler deviates if the program compiles and prints
  DEVIATES. *)
program t6p7p2p2d9;
const
   capa = 'A';
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #221');
   writeln(+capa);
   writeln(' DEVIATES...6.7.2.2-9, UNARY OPERATOR')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  