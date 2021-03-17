
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 265*)
(*TEST 6.8.3.9-6, CLASS=ERRORHANDLING*)
(* This program uses a for statement control variable after a
  for loop which is not entered. The control variable should be
  undefined after the for statement. *)
program t6p8p3p9d6;
var
   i,j,k,m:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #265');
   i:=100;
   k:=1;
   m:=0;
   j:=10;
   for i:=j to k do
   begin
      m:=m+1;
   end;
   writeln(' THE VALUE OF I =',i);
   writeln(' ERROR NOT DETECTED...6.8.3.9-6, FOR');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
