
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 176*)
(*TEST 6.6.5.3-2, CLASS=CONFORMANCE*)
(* This program tests that new and dispose operate as described
  in the Standard, however the undefinition of the pointer
  variable by dispose is not tested.
  The compiler fails if the program does not compile
  and run to completion. *)
program t6p6p5p3d2;
var
   ptr : ^integer;
   i   : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #176');
   for i:=1 to 10 do
   begin
      new(ptr);
      ptr^:=i;
      dispose(ptr)
   end;
   writeln(' PASS...6.6.5.3-2')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
  