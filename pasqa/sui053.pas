
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  53*)
(*TEST 6.3-3, CLASS=DEVIANCE*)
(* This program checks that signed strings are not permitted.
  Note that minus may have a worse effect than plus. *)
program t6p3d3;
const
   stars = '****';
   plusstars = + stars;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #053');
   writeln(' DEVIATES...6.3-3')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

