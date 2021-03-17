
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 190*)
(*TEST 6.6.6.2-5, CLASS=ERRORHANDLING*)
(* This program causes an error to occur as a negative
  argument is used for the sqrt function.
  The error should be detected at run-time. *)
program t6p6p6p2d5;
var
   m    : real;
   i, j : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #190');
   i:=256;
   j:=i*2;
   j:=j-257;
   m:=sqrt(j-i);
   writeln(' ERROR NOT DETECTED...6.6.6.2-5')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

