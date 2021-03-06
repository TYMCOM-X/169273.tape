
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 158*)
(*TEST 6.6.3.4-1, CLASS=CONFORMANCE*)
(* This program tests that procedures may be passed to other
  procedures and functions as parameters.
  The compiler fails if the program does not compile and run. *)
program t6p6p3p4d1;
var
   i : integer;
procedure a(procedure b);
   begin
      write(' PASS');
      b
   end;
procedure c;
   begin
      write('.')
   end;
function d(procedure b) : integer;
   begin
      b;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      d:=2
   end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #158');
   a(c);
   i:=d(c);
   if i=2 then
      writeln('.6.6.3.4-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 