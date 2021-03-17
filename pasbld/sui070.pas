
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  70*)
(*TEST 6.4.2.4-2, CLASS=DEVIANCE*)
(* This program tests to see if real constants are permitted
  in a subrange declaration. The Pascal Standard states that
  a subrange definition must be of a subrange of another ordinal
  type. This rules out real constants in the definition. *)
program t6p4p2p4d2;
type
   wiregauge = 0.001..0.2;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #070');
   writeln(' DEVIATES...6.4.2.4-2')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

