
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 203*)
(*TEST 6.6.6.4-3, CLASS=CONFORMANCE*)
(* This program tests the function pred only. The user is
  refered to tests 6.4.2.2-4 and 6.4.2.3-2 for tests of
  succ.
  The compiler fails if the program does not compile and run. *)
program t6p6p6p4d3;
type
   colourtype = (red,orange,yellow,green,blue);
var
   colour : colourtype;
   counter: integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #203');
   counter:=0;
   colour:=blue;
   colour:=pred(colour);
   colour:=pred(colour);
   colour:=pred(succ(colour));
   if colour=yellow then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      counter:=1
   else
      writeln(' FAIL...6.6.6.4-3 : COLOUR');
   if pred(-10)=-11 then
      counter:=counter+1
   else
      writeln(' FAIL...6.6.6.4-3 : -VE NUMBERS');
   if counter=2 then
      writeln(' PASS...6.6.6.4-3')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   