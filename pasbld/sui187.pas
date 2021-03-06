
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 187*)
(*TEST 6.6.6.2-2, CLASS=CONFORMANCE*)
(* This program tests the implementation of the arithmetic
  function sqr. Both real and integer expressions are used.
  The compiler fails if the program does not compile and run. *)
program t6p6p6p2d2;
var
   i,counter : integer;
   variable : real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #187');
   counter := 0;
   for i:= -10 to 10 do
   begin
      if sqr(i) = i*i then
         counter := counter + 1;
   end;
   variable := -10.3;
   while (variable < 10.3) do
   begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      if (sqr(variable) = variable*variable) then
         counter := counter+1;
      variable := variable + 0.9;
   end;
   if (counter = 44) then
      writeln(' PASS...6.6.6.2-2')
   else
      writeln(' FAIL...6.6.6.2-2:SQR')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   