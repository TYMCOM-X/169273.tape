
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  46*)
(*TEST 6.2.2-6, CLASS=CONFORMANCE*)
(* As for the other conformance tests in this section,
  it is possible to redefine a field-name of a record within
  the same scope as this record.
  The compiler also fails if the program does not compile. *)
program t6p2p2d6;
var
   j : integer;
   x : record
         j:integer
       end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #046');
   j:=1;
   x.j:=2;
   with x do
      j:=3;
   if (j=1) and (x.j=3) then writeln(' PASS...6.2.2-6')
   else  writeln(' FAIL...6.2.2-6')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
