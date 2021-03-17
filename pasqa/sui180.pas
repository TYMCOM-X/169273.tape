
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 180*)
(*TEST 6.6.5.3-6, CLASS=ERRORHANDLING*)
(* This program causes an error to occur as a variable which
  is an element of the record-variable-list of a with
  statement is refered to by the pointer parameter of
  dispose. *)
program t6p6p5p3d6;
type
   subrange = 0..9999;
   rekord   = record
               name : packed array[1..15] of char;
               employeeno : subrange
              end;
var
   ptr : ^rekord;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #180');
   new(ptr);
   with ptr^ do
   begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      name:='HARRY M. MULLER';
      employeeno:=9998;
      dispose(ptr)
   end;
   writeln(' ERROR NOT DETECTED...6.6.5.3-6')
end.
