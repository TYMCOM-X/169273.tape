
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 231*)
(*TEST 6.7.2.5-3, CLASS=DEVIANCE*)
(* This test checks that file comparisons are not allowed.
  The semantics of this situation are particularly ill-defined,
  and not within standard Pascal. The compiler deviates if the
  program compiles and prints DEVIATES. *)
program t6p7p2p5d3;
var
   f:text;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #231');
   rewrite(f);
   if f=output then
      writeln(' FAIL1...6.7.2.5-3, CONTENTS COMPARED')
   else
      writeln(' FAIL2...6.7.2.5-3, DESCRIPTORS COMPARED')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 