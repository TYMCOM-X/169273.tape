
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 297*)
(*TEST 6.9.4-4, CLASS=CONFORMANCE*)
(* This program checks that real numbers are correctly written to
  text files. The compiler fails if the program does not compile
  or the program prints FAIL. *)
program t6p9p4d4;
var
   f:text;
   a:packed array [1..26] of char;
   b:packed array [1..24] of char;
   i:integer;
   counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #297');
   rewrite(f);
   counter:=0;
   writeln(f,0.0:6,1.0:6,1.0:10);
   reset(f);
   for i:=1 to 26 do
      read(f,a[i]);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if (a='     0.0     1.0 1.000E+00') then
      counter:=counter+1;
   rewrite(f);
   writeln(f,0.0:4:1,1.0:6:1,-1.0:6:1,123.456:7:3);
   reset(f);
   for i:=1 to 24 do
      read(f,b[i]);
   if (b=' 0.0   1.0  -1.0 123.456') then
      counter:=counter+1;
   if (counter=2) then
      writeln(' PASS...6.9.4-4, WRITE REALS')
   else
      writeln(' FAIL...6.9.4-4, WRITE REALS');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

