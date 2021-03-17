
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  11*)
(*TEST 6.1.5-4, CLASS=DEVIANCE*)
(* The number productions specified in the Pascal Standard
  clearly state that a decimal point must be followed by
  a digit sequence.
  The compiler deviates if the program compiles, in which case
  the program will print 'DEVIATES'.
  The compiler conforms if the program fails to compile. *)
program t6p1p5d4;
var
   i : real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #011');
   i:=0123.;
   writeln(' DEVIATES...6.1.5-4');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

