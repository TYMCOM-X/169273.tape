
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  86*)
(*TEST 6.4.3.3-7, CLASS=ERRORHANDLING*)
(* This test is similar to 6.4.3.3-5, except that no tagfield is
  used.
  Variant changes occur implicitly as a result of
  assignment to fields. The fields associated with the new
  variant come into existence with undefined values. *)
program t6p4p3p3d7;
type
   two = (a,b);
var
   variant : record
               case two of
                  a : (m : integer);
                  b : (n : integer);
             end;
   i : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #086');
   variant.m:=2;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   i:=variant.n;     (* illegal *)
   writeln(' ERROR NOT DETECTED...6.4.3.3-7')
end.
    