
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  82*)
(*TEST 6.4.3.3-3, CLASS=CONFORMANCE*)
(* The Pascal Standard permits the declaration of an empty record,
  this empty record serves little purpose, and for this reason
  some compilers will not allow it to be used.
  The compiler fails if the program does not compile. *)
program t6p4p3p3d3;
type
   statuskind  = (defined,undefined);
   emptykind   = record end;
var
   empty : emptykind;
   number: record
            case status:statuskind of
               defined  : (i : integer);
               undefined: (e : emptykind)
            end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #082');
   with number do

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   begin
      status:=defined;
      i:=7
   end;
   writeln(' PASS...6.4.3.3-3')
end.
