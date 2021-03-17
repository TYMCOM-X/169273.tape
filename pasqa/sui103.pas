
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 103*)
(*TEST 6.4.4-2, CLASS=DEVIANCE*)
(* This program tests the diagnostic that should be produced by
  the compiler if the type to which a pointer points is not
  found. *)
program t6p4p4d2;
var
   pointer1 : ^real;
   pointer2 : ^rekord;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #103');
   new(pointer1);
   pointer1:=nil;
   new(pointer2);
   pointer2:=nil;
   writeln(' DEVIATES...6.4.4-2')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

