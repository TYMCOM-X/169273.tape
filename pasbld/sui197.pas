
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 197*)
(*TEST 6.6.6.3-1, CLASS=CONFORMANCE*)
(* This program checks the implementation of the transfer
  functions trunc and round.
  The compiler fails if the program does not compile and run. *)
program t6p6p6p3d1;
var
   i,
   truncstatus,
   roundstatus : integer;
   j : real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #197');
   truncstatus:=0;
   roundstatus:=0;
   if (trunc(3.7)=3) and (trunc(-3.7)=-3) then
      truncstatus:=truncstatus+1
   else
      writeln(' FAIL...6.6.6.3-1 : TRUNC');
   if (round(3.7)=4) and (round(-3.7)=-4) then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      roundstatus:=roundstatus+1
   else
      writeln(' FAIL...6.6.6.3-1 : ROUND');
   j:=0;
   for i:=-333 to 333 do
   begin
      j:=j+i div 100;
      if j<0 then
         if (trunc(j-0.5)=round(j)) then
            begin
               truncstatus:=truncstatus+1;
               roundstatus:=roundstatus+1
            end
         else
            writeln(' FAIL...6.6.6.3-1 : TRUNC ROUND')
      else
         if (trunc(j+0.5)=round(j)) then
            begin
               truncstatus:=truncstatus+1;
               roundstatus:=roundstatus+1

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

            end
         else
            writeln(' FAIL...6.6.6.3-1 : TRUNC ROUND')
   end;
   if (truncstatus=668) and (roundstatus=668) then
      writeln(' PASS...6.6.6.3-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 