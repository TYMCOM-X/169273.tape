
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  48*)
(*TEST 6.2.2-8, CLASS=CONFORMANCE*)
(* It is possible to declare a function but not assign a value
  to that function at that level. This program assigns a value
  to a function from within a function within the function.
  The compiler fails if the program does not compile or it prints
  FAIL. *)
program t6p2p2d8;
var
   j,k:integer;
function f1(i:integer):integer;
   function f2(i:integer):integer;
      function f3(i:integer):integer;
      begin
         f3:=1;
         f1:=i
      end;
   begin
      f2:=f3(i)
   end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

begin
   j:=f2(i)
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #048');
   k:=f1(5);
   if (k=5) then
      writeln(' PASS...6.2.2-8, FUNCTION')
   else
      writeln(' FAIL...6.2.2-8, FUNCTION')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

