
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 213*)
(*TEST 6.7.2.2-1, CLASS=CONFORMANCE*)
(* This program checks the operation of the operators + - and *.
  The compiler fails if the program does not compile, or the
  program states that this is so. *)
program t6p7p2p2d1;
var
   i, x, y , counter : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #213');
   counter := 0;
   for x := -10 to 10 do
   begin
      if (succ(x)=x+1) then
         counter := counter+1;
      if (pred(x) = x-1) then
         counter := counter+1;
      if (x*x=sqr(x)) then
         counter:= counter+1;
   end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if (counter=63) then
      writeln(' PASS...6.7.2.2-1')
   else
      writeln(' FAIL...6.7.2.2-1')
end.
   