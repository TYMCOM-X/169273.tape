
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 132*)
(*TEST 6.5.3.2-1, CLASS=ERRORHANDLING*)
(* This test is similar to 6.4.5-6, except that a two
  dimensional array is used. This may present some problems to
  particular implementations. *)
program t6p5p3p2d1;
var
   urray : array[1..10,1..10] of integer;
   i     : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #132');
   i:=3;
   urray[i*2,i*4]:=0;
   writeln(' ERROR NOT DETECTED...6.5.3.2-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

