
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 121*)
(*TEST 6.4.6-4, CLASS=ERRORHANDLING*)
(* The Pascal standard says that if the two types in an assignment
  compatibility test (T1 and T2) are compatible ordinal types
  and the value of the expression E which is of type T2 is not in
  the closed interval specified by the type T1, an error occurs.
  Does this compiler detect this. *)
program t6p4p6d4;
type
   subrange = 0..5;
var
   i : subrange;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #121');
   i:=5;
   i:=i*2;    (* error *)
   writeln(' ERROR NOT DETECTED...6.4.6-4')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  