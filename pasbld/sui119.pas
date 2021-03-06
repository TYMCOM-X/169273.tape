
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 119*)
(*TEST 6.4.6-2, CLASS=CONFORMANCE*)
(* This test is similar to 6.4.6-1, except that it tests the use
  of assignment compatibility in actual and formal parameters.
  The compiler fails if the program does not compile. *)
program t6p4p6d2;
type
   colour = (red,pink,yellow,green);
   subcol1 = yellow..green;
   subcol2 = set of colour;
   subcol3 = set of pink..green;
var
   a        : integer;
   b        : real;
   colour1  : colour;
   colour2  : pink..green;
   colour3  : set of colour;
   colour4  : set of yellow..green;
procedure compat(i : integer; j : real;
                 col1 : colour; col2 : subcol1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                 col3 : subcol2; col4 : subcol3);
begin
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #119');
   compat(2,2.4,yellow,yellow,[pink],[pink]);
   a:=2;
   b:=3.1;
   colour1:=pink;
   colour2:=green;
   colour3:=[yellow];
   colour4:=[yellow];
   compat(a,b,colour1,colour2,colour3,colour4);
   compat(a,a,colour2,colour2,colour4,colour4);
   writeln(' PASS...6.4.6-2')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   