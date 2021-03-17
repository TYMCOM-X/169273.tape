
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  93*)
(*TEST 6.4.3.4-1, CLASS=CONFORMANCE*)
(* This program simply tests that set types as described in the
  Pascal Standard are permitted.
  The compiler fails if the program will not compile. *)
program t6p4p3p4d1;
type
   colour   = (red,blue,pink,green,yellow);
   setone   = set of colour;
   settwo   = set of blue..green;
   setthree = set of boolean;
   setfour  = set of 1..10;
   setfive  = set of 0..3;
   setsix   = set of (heart,diamond,spade,club);
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #093');
   writeln(' PASS...6.4.3.4-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

