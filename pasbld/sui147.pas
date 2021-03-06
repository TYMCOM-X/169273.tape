
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 147*)
(*TEST 6.6.2-6, CLASS=ERRORHANDLING*)
(* The Pascal Standard states that the result of a function will
  be the last value assigned to its identifier. If no assignment
  occurs then the result is undefined.
  This program contains a function with an assignment to its
  identifier, however the assignment is never executed. An error
  should occur during execution. *)
program t6p6p2d6;
var
   radius ,
   circlearea : real;
function area(a : real) : real;
var
   x : real;
begin
   if a > 0 then x:=3.1415926*a*a
            else area:=0
end;
begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #147');
   radius:=2;
   circlearea:=area(radius);
   writeln(' ERROR NOT DETECTED...6.6.2-6')
end.
  