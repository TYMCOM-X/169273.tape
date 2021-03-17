
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 109*)
(*TEST 6.4.5-5, CLASS=DEVIANCE*)
(* Again, this test is similar to 6.4.5-4, except that deviance
  for pointers is tested.
  Although the two pointers in this example point to the same
  type, they are not identical.
  The compiler conforms if the program does not compile/execute. *)
program t6p4p5d5;
type
   rekord = record
               a : integer;
            end;
   ptrone = ^rekord;
   ptrtwo = ^rekord;
var
   ptrtorec    : ptrone;
   ptrtorectoo : ptrtwo;
procedure test(var ptr : ptrone);
begin
   writeln(' DEVIATES...6.4.5-5')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #109');
   new(ptrtorectoo);
   ptrtorectoo:=nil;
   test(ptrtorectoo)
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

