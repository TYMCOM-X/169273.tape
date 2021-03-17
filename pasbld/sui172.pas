
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 172*)
(*TEST 6.6.5.2-5, CLASS=CONFORMANCE*)
(* This program checks that a rewrite on the file f sets
  eof to be true. *)
program t6p6p5p2d5;
var
   fyle : text;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #172');
   rewrite(fyle);
   if eof(fyle) then
      writeln(' PASS...6.6.5.2-5')
   else
      writeln(' FAIL...6.6.5.2-5')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

