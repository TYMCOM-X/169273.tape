
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  10*)
(*TEST 6.1.5-3, CLASS=DEVIANCE*)
(* The number productions specified in the Pascal Standard
  clearly state that a decimal point must be preceded by
  a digit sequence.
  The compiler deviates if the program compiles, in which case
  the program will print 'DEVIATES', or if one of the cases is
  accepted.
  The compiler conforms if all the cases are rejected. *)
program t6p1p5d3;
const
   r = .123;
var
   i : real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #010');
   i:=.123;
   i:=-.123;
   writeln(' DEVIATES...6.1.5-3');
end.
