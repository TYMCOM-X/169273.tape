
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  13*)
(*TEST 6.1.5-6, CLASS=DEVIANCE*)
(* The Pascal standard allows equivalence of upper and
  lower-case letters in names and reserved words only.
  Will the compiler accept 'e' as equivalent to 'E'?
  It should not. The test is not relevant
  to one-case processors. *)
program t6p1p5d6;
var
   i : real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #013');
   i:=123e2;
   writeln(' DEVIATES...6.1.5-6')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    