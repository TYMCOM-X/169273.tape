
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 282*)
(*TEST 6.8.3.10-3, CLASS=CONFORMANCE*)
(* This test checks that the record-variable-list is evaluated
  in the correct order. The compiler fails if the program does
  not compile or the program prints FAILS. *)
program t6p8p3p10d3;
var
   r1:record
        i,j,k:integer
      end;
   r2:record
        i,j:integer
      end;
   r3:record
        i:integer
      end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #282');
   with r1 do
   begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      i:=0;
      j:=0;
      k:=0
   end;
   with r2 do
   begin
      i:=0;
      j:=0
   end;
   with r3 do
      i:=0;
   with r1,r2,r3 do
   begin
      i:=5;
      j:=6;
      k:=7
   end;
   if(r1.i=0) and (r1.j=0) and (r2.i=0) and (r1.k=7)
      and (r2.j=6) and (r3.i=5) then
      writeln(' PASS 6.8.3.10-3, WITH EVALUATION')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   else
      writeln(' FAIL 6.8.3.10-3, WITH EVALUATION');
end.
  