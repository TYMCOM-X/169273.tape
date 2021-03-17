
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 118*)
(*TEST 6.4.6-1, CLASS=CONFORMANCE*)
(* This program tests that all assignment compatible types as
  described by the Pascal Standard, are permitted by this compiler.
  This program tests only those uses in assignment statements.
  All cases have been tested elsewhere, but are included here
  together for consistency.
  The compiler fails if one or more of the cases below
  are rejected. *)
program t6p4p6d1;
type
   colour = (red,pink,yellow);
   rekord = record
               a : integer;
               b : boolean
            end;
var
   i     : integer;
   j     : real;
   col1  : colour;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   col2  : pink..yellow;
   col3  : set of colour;
   col4  : set of red..pink;
   urray1   : array[1..6] of integer;
   urray2   : array[1..4] of integer;
   record1  : rekord;
   record2  : rekord;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #118');
   i:=2;
   j:=i;
   col1:=yellow;
   col2:=col1;
   col3:=[pink];
   col4:=col3;
   urray2[1]:=0;
   urray1[6]:=urray2[1];
   record1.a:=2;
   record1.b:=true;
   record2:=record1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   writeln(' PASS...6.4.6-1')
end.
