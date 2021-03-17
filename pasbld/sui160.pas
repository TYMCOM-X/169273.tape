
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 160*)
(*TEST 6.6.3.5-1, CLASS=CONFORMANCE*)
(* Similarly to 6.6.3.4-1, this program tests that functions may
  be passed to procedures and functions as parameters.
  The compiler fails if the program does not compile and run. *)
program t6p6p3p5d1;
var
   j : integer;
procedure a(function b : integer);
   var
      i : integer;
   begin
      i:=b;
      write(' PASS')
   end;
function c : integer;
   begin
      c:=2
   end;
function d(function b : integer) : integer;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   begin
      d:=b
   end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #160');
   a(c);
   j:=d(c);
   if j=2 then
      writeln('...6.6.3.5-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

