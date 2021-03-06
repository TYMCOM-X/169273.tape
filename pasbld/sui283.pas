
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 283*)
(*TEST 6.8.3.10-4, CLASS=CONFORMANCE*)
(* This test checks that the selection of a variable in the
  record-variable-list is performed before the component
  statement is executed. The compiler fails if the program
  does not compile or the program prints FAIL. *)
program t6p8p3p10d4;
var
   a:array[1..2] of record
                      i,j:integer
                    end;
   k:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #283');
   a[2].i:=5;
   k:=1;
   with a[k] do
   begin
      j:=1;
      k:=2;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      i:=2
   end;
   if (a[2].i=5) and (a[1].i=2) then
      writeln(' PASS...6.8.3.10-4, WITH')
   else
      writeln(' FAIL...6.8.3.10-4, WITH');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    