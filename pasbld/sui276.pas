
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 276*)
(*TEST 6.8.3.9-17, CLASS=ERRORHANDLING*)
(* This test checks the type of error produced when two nested
  for statements use the same control variable. *)
program t6p8p3p9d17;
var
   i,j:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #276');
   j:=0;
   for i:=1 to 10 do
      for i:=1 to 10 do
         j:=j+1;
   writeln(' ERROR NOT DETECTED...6.8.3.9-17, FOR');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   