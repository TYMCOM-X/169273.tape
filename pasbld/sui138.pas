
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 138*)
(*TEST 6.6.1-2, CLASS=CONFORMANCE*)
(* This program tests the implementation of FORWARD declaration,
  recursive activation, and multilevel referencing of a var
  parameter in procedures.
  The compiler fails if the program will not compile. *)
program t6p6p1d2;
var
   c : integer;
procedure one(var a : integer);
   forward;
procedure two(var b : integer);
begin
   b:=b+1;
   one(b)
end;
procedure one;
begin
   a:=a+1;
   if a = 1 then two(a)

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #138');
   c:=0;
   one(c);
   if c = 3 then
      writeln(' PASS...6.6.1-2')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  