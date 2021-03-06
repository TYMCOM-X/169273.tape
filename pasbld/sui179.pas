
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 179*)
(*TEST 6.6.5.3-5, CLASS=ERRORHANDLING*)
(* This program causes an error to occur as a variable
  which is currently an actual variable parameter is
  refered to by the pointer parameter of dispose.
  The error should be detected by the compiler or
  at run-time. *)
program t6p6p5p3d5;
var
   ptr : ^integer;
procedure error(a:integer);
   var
      x : integer;
   begin
      x:=a*2;
      dispose(ptr)
   end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #179');
   new(ptr);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   ptr^:=6;
   error(ptr^);
   writeln(' ERROR NOT DETECTED...6.6.5.3-5')
end.
    