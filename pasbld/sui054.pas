
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  54*)
(*TEST 6.3-4, CLASS=DEVIANCE*)
(* This program checks that signed scalars are not permitted.
  Note than minus may have a worse effect than plus. *)
program t6p3d4;
const
   truth = true;
   plustruth = + truth;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #054');
   writeln(' DEVIATES...6.3-4')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  