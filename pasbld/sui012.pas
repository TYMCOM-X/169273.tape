
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  12*)
(*TEST 6.1.5-5, CLASS=DEVIANCE*)
(* Spaces in numbers are forbidden by the Pascal Standard
  This includes spaces around '.' and 'E'. The compiler
  deviates if ONE or MORE of the cases below are accepted.
  The compiler conforms if ALL cases are rejected. *)
program t6p1p5d5;
const
   one   = 1 234;
   two   = 0 .1234;
   three = 0. 1234;
   four  = 1234 E2;
   five  = 1234E 2;
   six   = 1234E- 2;
   seven = 1234E+ 2;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #012');
   writeln(' DEVIATES...6.1.5-5')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 