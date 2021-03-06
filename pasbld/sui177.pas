
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 177*)
(*TEST 6.6.5.3-3, CLASS=ERRORHANDLING*)
(* This program causes an error to occur as the pointer
  parameter of dispose is nil. The error should be detected
  by the compiler or at run-time. *)
program t6p6p5p3d3;
type
   rekord = record
            a : integer;
            b : boolean
           end;
var
   ptr : ^rekord;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #177');
   ptr:=nil;
   dispose(ptr);
   writeln(' ERROR NOT DETECTED...6.6.5.3-3')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    