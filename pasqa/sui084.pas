
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  84*)
(*TEST 6.4.3.3-5, CLASS=ERRORHANDLING*)
(* The Pascal Standard states that if a change of variant occurs
  (by assigning a value associated with a  variant to the
  tag-field), then the fields associated with the previous variants
  cease to exist. This program causes the error to occur. *)
program t6p4p3p3d5;
type
   two = (a,b);
var
   variant : record
               case tagfield:two of
                  a: (m:integer);
                  b: (n:integer);
             end;
   i : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #084');
   variant.tagfield:=a;
   variant.m:=1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   i:=variant.n;     (*illegal*)
   writeln(' ERROR NOT DETECTED...6.4.3.3-5')
end.
