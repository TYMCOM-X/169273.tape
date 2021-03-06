
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 288*)
(*TEST 6.9.2-1, CLASS=CONFORMANCE*)
(* This test checks that a single read statement with many
  variables is equivalent to many read statements containing
  one variable each. The compiler fails if the program does not
  compile or the program prints FAIL. *)
program t6p9p2d1;
var
   f:text;
   a,b,c,d,e:integer;
   a1,b1,c1,d1,e1:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #288');
   rewrite(f);
   writeln(f,' 1 2 3 4 5 ');
   reset(f);
   read(f,a,b,c,d,e);
   reset(f);
   read(f,a1);
   read(f,b1);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   read(f,c1);
   read(f,d1);
   read(f,e1);
   if(a=a1) and (b=b1) and (c=c1) and (d=d1) and (e=e1) then
      writeln(' PASS...6.9.2-1, READ')
   else
      writeln(' FAIL...6.9.2-1, READ');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 