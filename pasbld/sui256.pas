
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 256*)
(*TEST 6.8.3.7-2, CLASS=CONFORMANCE*)
(* This test checks that a loop containing no statements
  is executed until the expression is true.
  The compiler fails if the program does not compile or the
  program prints FAIL. *)
program t6p8p3p7d2;
var
  a:integer;
function bool : boolean;
begin
   a:=a+1;
   bool := a>=5;
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #256');
   a:=0;
   repeat
   until bool;
   if (a=5) then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      writeln(' PASS...6.8.3.7-2, EMPTY REPEAT')
   else
      writeln(' FAIL...6.8.3.7-2, EMPTY REPEAT');
end.
