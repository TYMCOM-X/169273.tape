
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  94*)
(*TEST 6.4.3.4-2, CLASS=IMPLEMENTATIONDEFINED*)
(* This program tests if a set of char is permitted by the
  compiler. *)
program t6p4p3p4d2;
var
   s : set of char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #094');
   s:=[';',' ','9','z'];
   if ([';',' ','9','z'] <= s) then
      writeln(' IMPLEMENTATION ALLOWS SET OF CHAR')
   else
      writeln(' IMPLEMENTATION DOES NOT ALLOW SET OF CHAR')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 