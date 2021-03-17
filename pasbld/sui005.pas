
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number   5*)
(*TEST 6.1.3-1, CLASS=CONFORMANCE*)
(* The Pascal Standard permits identifiers to be of any length
  This test will simply print out 'PASS' if the compiler accepts
  identifiers of lengths up to 70 characters. *)
program t6p1p3d1;
const
   i10iiiiiii = 10;
   i20iiiiiiiiiiiiiiiii = 20;
   i30iiiiiiiiiiiiiiiiiiiiiiiiiii = 30;
   i40iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii = 40;
   i50iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii = 50;
   i60iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii = 60;
i70iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
                                                                = 70;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #005');
   if i10iiiiiii + i20iiiiiiiiiiiiiiiii +
      i30iiiiiiiiiiiiiiiiiiiiiiiiiii +
      i40iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii +

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      i50iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii +
      i60iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii +
i70iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
      <> 280 then
      writeln(' FAIL...6.1.3-1')
   else
      writeln(' PASS...6.1.3-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

