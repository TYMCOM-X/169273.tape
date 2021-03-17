
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 100*)
(*TEST 6.4.3.5-3, CLASS=CONFORMANCE*)
(* This program tests if an end of line marker is inserted at the
  end of the line, if not explicitly done in the program.
  The structure of a text file requires a closing linemarker.
  Conforming compilers will either insert the linemarker, or
  report a run-time error. *)
program t6p4p3p5d3;
var
   file1 : text;
   chare : char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #100');
   rewrite(file1);
   write(file1,'A');
   reset(file1);
   get(file1);
   if eoln(file1) then writeln(' PASS...6.4.3.5-3')
                  else writeln(' FAIL...6.4.3.5-3')
end.
