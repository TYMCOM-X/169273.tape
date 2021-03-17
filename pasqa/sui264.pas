
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 264*)
(*TEST 6.8.3.9-5, CLASS=ERRORHANDLING*)
(* This test checks that the use of a for statement control
  variable after the completion of the for statement, and
  without an intervening assignment is detected. *)
program t6p8p3p9d5;
var
   i,j,k,m:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #264');
   i:=100;
   j:=1;
   k:=10;
   m:=0;
   for i:=j to k do
   begin
      m:=m+1;
   end;
   writeln('  THE VALUE OF I =',i);
   writeln(' ERROR NOT DETECTED...6.8.3.9-5, FOR');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
