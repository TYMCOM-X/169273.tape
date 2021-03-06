
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 290*)
(*TEST 6.9.2-3, CLASS=CONFORMANCE*)
(* This test checks that integers and reals are read correctly
  from a file. The compiler fails if the program does not
  compile or the program prints FAIL. *)
program t6p9p2d3;
var
   f:text;
   i,j:integer;
   r,s:real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #290');
   (* Internal (compile-time conversions) and run-time conversions
     should result in the same value, hence justifying the
     equality tests on real numbers. *)
   rewrite(f);
   writeln(f,' 123 123.456 5 123E6 ');
   reset(f);
   read(f,i,r,j,s);
   if(i=123)and(r=123.456) and (j=5) and (s=123E6) then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      writeln(' PASS...6.9.2-3, READ')
   else
   begin
      if (i=123) and (j=5) then
         writeln(' FAIL...6.9.2-3, READ REAL CONVERSIONS')
      else
         writeln(' FAIL...6.9.2-3, READ')
   end;
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 