
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 266*)
(*TEST 6.8.3.9-7, CLASS=CONFORMANCE*)
(* This test checks that extreme values may be used in a for
  loop. This will break a simply implemented for loop.
  In some compilers the succ test may fail at the last increment
  and cause wraparound(overflow) - leading to an infinite loop. *)
program t6p8p3p9d7;
var
   i,j:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #266');
   j:=0;
   for i:= (maximum(integer)-10) to maximum(integer) do
     j:=j+1;
   for i:= (-maximum(integer)+10) downto -maximum(integer) do
      j:=j+1;
   if j = 22 then
      writeln(' PASS...6.8.3.9-7, FOR LOOP')
   else
      writeln(' FAIL...6.8.3.9-7, FOR LOOP');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
