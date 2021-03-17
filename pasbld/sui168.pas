
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 168*)
(*TEST 6.6.5.2-1, CLASS=ERRORHANDLING*)
(* This program causes an error to occur, as eof(f) does
  not yield true prior to execution of a put on the file f.
  The error should be detected at compile-time or run-time. *)
program t6p6p5p2d1;
var
   fyle : text;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #168');
   rewrite(fyle);
   writeln(fyle,'ABC');
   reset(fyle);      (* eof is false and f^='A' *)
   put(fyle);        (* causes an error *)
   writeln(' ERROR NOT DETECTED...6.6.5.2-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

