
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  67*)
(*TEST 6.4.2.3-1, CLASS=CONFORMANCE*)
(* This program checks the possible syntax productions for
  enumerated types, as specified by the Pascal Standard.
  The compiler fails if the program does not compile. *)
program t6p4p2p3d1;
type
   singularitytype = (me);
   switch          = (on_,off);
   maritalstatus   = (married,divorced,widowed,single);
   colour          = (red,pink,orange,yellow,green);
   cardsuit        = (heart,diamond,spade,club);
var
   i : singularitytype;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #067');
   i:=me;
   writeln(' PASS...6.4.2.3-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 