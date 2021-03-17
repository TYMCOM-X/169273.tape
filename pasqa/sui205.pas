
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 205*)
(*TEST 6.6.6.4-5, CLASS=ERRORHANDLING*)
(* This program causes an error to occur as the function PRED
  is applied to the first value of an ordinal type.
  The error should be detected by the compiler or at run-time. *)
program t6p6p6p4d5;
type
   enumerated = (first,second,third,fourth,last);
var
   ordinal : enumerated;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #205');
   ordinal:=first;
   ordinal:=pred(ordinal);
   writeln(' ERROR NOT DETECTED...6.6.6.4-5, PRED')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

