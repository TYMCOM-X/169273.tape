
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number   3*)
(*TEST 6.1.2-2, CLASS=DEVIANCE*)
(* This test checks that reserved words cannot in fact be redefined.
  The compiler deviates if the program compiles and prints DEVIATES*)
program t6p1p2d2;
var
   thing:(var,string);
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #003');
   thing:=string;
   writeln(' DEVIATES...6.1.2-2, RESERVED WORDS')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 