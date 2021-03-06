
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  85*)
(*TEST 6.4.3.3-6, CLASS=ERRORHANDLING*)
(* The program causes an error by accessing a field with
  an undefined value. The undefinition arises because
  when a change of variant occurs, those fields associated with
  the new variant come into existence with undefined
  values. *)
program t6p4p3p3d6;
type
   two = (a,b);
var
   variant : record
               case tagfield:two of
                  a : (m : integer;
                       l : integer);
                  b : (n : integer;
                       o : integer)
             end;
   i : integer;
begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #085');
   variant.tagfield:=a;
   variant.m:=1;
   variant.l:=1;
   variant.tagfield:=b;
   variant.n:=1;
   i:=variant.o;     (* illegal *)
   writeln(' ERROR NOT DETECTED...6.4.3.3-6')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  