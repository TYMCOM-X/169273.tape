
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 313*)
(*TEST 6.10-3, CLASS=DEVIANCE*)
(* This program checks that the default file output is
  implicitly declared at the program level by attempting to
  redefine it. The file input should be identical, of course.
  The test should not compile. *)
program t6p10d3;
var
   output:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #313');
   output:=1;
   writeln(' DEVIATES...6.10-3, OUTPUT REDEFINED')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 