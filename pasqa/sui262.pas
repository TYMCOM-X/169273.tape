
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 262*)
(*TEST 6.8.3.9-3, CLASS=DEVIANCE*)
(* This test checks that an error is produced when an
  assignment is made to a for statement control variable.
  The compiler deviates if the program compiles and prints
  DEVIATES. *)
program t6p8p3p9d3;
var
   i,j:integer;
procedure nasty (var n:integer);
begin
   n:=n+1;
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #262');
   j:=0;
   for i:=1 to 10 do
   begin
      j:=j+1;
      nasty(i);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   end;
   writeln(' DEVIATES...6.8.3.9-3, FOR');
end.
