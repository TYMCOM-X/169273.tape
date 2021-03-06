
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  72*)
(*TEST 6.4.3.1-1, CLASS=DEVIANCE*)
(* The Pascal Standard states that only structured types may be
  PACKED (array, set, file and record types).
  This program tests this point. The compiler conforms if
  the program will not compile. *)
program t6p4p3p1d1;
type
   switch = packed(on_,off);
   state  = packed(high,low,invalid);
   decade   = packed 0..10;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #072');
   writeln(' DEVIATES...6.4.3.1-1 : IMPROPER USE OF PACKED')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    