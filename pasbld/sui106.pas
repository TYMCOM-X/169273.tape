
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 106*)
(*TEST 6.4.5-2, CLASS=DEVIANCE*)
(* This program simply tests that the compiler does not deviate
  from the Standard in the case of subranges of the same host
  being treated as identical.
  The program should fail to compile/execute if the compiler
  conforms. *)
program t6p4p5d2;
type
   colour = (red,pink,orange,yellow,green,blue);
   subone = red..yellow;
   subtwo = pink..blue;
var
   colour1 : subone;
   colour2 : subtwo;
procedure test(var col1 : subone);
begin
   writeln(' DEVIATES...6.4.5-2')
end;
begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #106');
   (* Although colour1 and colour2 are compatible (i.e. subone and
     subtwo are compatible), they are not identical, and the call
     to TEST should fail. *)
   colour2:=pink;
   test(colour2)
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 