
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 124*)
(*TEST 6.4.6-7, CLASS=ERRORHANDLING*)
(* Similarly for 6.4.6-4, if two types are compatible set types,
  and any of the members of the set expression E (of type T2)
  is not in the closed interval specified by the base-type of the
  type T1, an error occurs.
  Again, does the compiler detect this. *)
program t6p4p6d7;
type
   colour = (red,pink,orange,yellow,green,blue);
   subone = red..orange;
   subtwo = pink..yellow;
var
   setone : set of subone;
   settwo : set of subtwo;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #124');
   settwo:=[pink,yellow];
   setone:=settwo;                     (* should be an error *)
   writeln(' ERROR NOT DETECTED...6.4.6-7')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
