
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 281*)
(*TEST 6.8.3.10-2, CLASS=CONFORMANCE*)
(* This test checks that a field identifier is correctly
  identified when a with statement is invoked.
  The compiler fails if the program does not compile or the
  program prints FAILS. *)
program t6p8p3p10d2;
var
   r:record
       i,j:integer
     end;
   i:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #281');
   i:=10;
   with r do
      i:=5;
   if (i=10) and (r.i=5) then
      writeln(' PASS 6.8.3.10-2, WITH')
   else

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      writeln(' FAIL 6.8.3.10-2, WITH');
end.
  