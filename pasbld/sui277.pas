
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 277*)
(*TEST 6.8.3.9-18, CLASS=QUALITY*)
(* This test checks that the undefined state of a for-statement
  controlled variable when the loop is left has one or both of
  the following properties:
    (a) Range checks are not omitted on these variables in the
        supposition that its value is permissible, or
    (b) the value of the variable is in range of its type (in
        this specific implementation).
  This test is not relevant if the use of the variable is
  prohibited. *)
program t6p8p3p9d18;
type
   t=(red,green,blue,pink);
var
   i,j,k:t;
   m:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #277');
   (* i is a finite scalar. *)

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   i:=green;
   j:=red;
   k:=pink;
   m:=0;
   for i:=j to k do
   begin
      m:=m+1;
   end;
   writeln(' THE UNDEFINED ORDINAL VALUE OF I IS ',ord(i));
   writeln(' ERROR NOT DETECTED');
   write(' ITS SYMBOLIC VALUE IS ');
   (* A possible omission of the range check on the case statement
     may be disastrous if a wild jump occurs. *)
   case i of
   red: writeln('RED');
   green: writeln('GREEN');
   blue: writeln('BLUE');
   pink: writeln('PINK');
   end;
   writeln(' JUST IN CASE THE RANGE ISNT CHECKED');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
  