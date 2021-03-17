
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 174*)
(*TEST 6.6.5.2-7, CLASS=ERRORHANDLING*)
(* This test is similar to 6.6.5.2-6, except that the
  buffer variable is an element of the record variable list
  of a with statement.
  The error should be detected by the compiler or at
  run-time. *)
program t6p6p5p2d7;
type
   sex   = (male,female,notgiven);
   socialsecuritynumber = 0..10000;
   rekord = record
               a : socialsecuritynumber;
               b : sex
            end;
var
   fyle : file of rekord;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #174');
   rewrite(fyle);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   with fyle^ do
   begin
      a:=9999;
      b:=notgiven;
      put(fyle)
   end;
   writeln(' ERROR NOT DETECTED...6.6.5.2-7')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

