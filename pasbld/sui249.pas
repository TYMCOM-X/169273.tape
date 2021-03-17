
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 249*)
(*TEST 6.8.3.5-9, CLASS=DEVIANCE*)
(* This test checks that the compiler detects that case-constants
  and the case-index are of different types.
  The compiler deviates if the program compiles and the program
  prints deviates. *)
program t6p8p3p5d9;
var
   i,counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #249');
   counter:= 0;
   for i:= 1 to 4 do
      case i of
      1: counter:=counter+1;
      2.0: counter:=counter+1;
      3: counter:=counter+1;
      4e0: counter:=counter+1;
      end;
   if counter=4 then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      writeln(' DEVIATES...6.8.3.5-9, CASE CONSTANTS')
   else
      writeln(' FAILS...6.8.3.5-9, CASE CONSTANTS');
end.
