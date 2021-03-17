
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  16*)
(*TEST 6.1.7-1, CLASS=CONFORMANCE*)
(* Character strings consisting of a single character
  are the constants of the standard type char. This
  program simply tests that these are permitted by
  the compiler.
  The compiler fails if the program will not compile. *)
program t6p1p7d1;
const
   one = '1';
   two = '2';
var
   twotoo : char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #016');
   if (one <> two) and (two = '2') then
      begin
      twotoo:='2';
      if twotoo = two then
         writeln(' PASS...6.1.7-1')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      else
         writeln(' FAIL...6.1.7-1')
      end
   else
      writeln(' FAIL...6.1.7-1')
end.
