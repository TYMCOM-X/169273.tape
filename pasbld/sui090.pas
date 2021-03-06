
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  90*)
(*TEST 6.4.3.3-11, CLASS=DEVIANCE*)
(* This program is similar to 6.4.3.3-3, except here, the
  empty record is assigned a value. This should not be possible.
  The program conforms if the program does not compile or run. *)
program t6p4p3p3d11;
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
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #090');
   with number do
   begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      status:=undefined;
      e:=666
   end;
   writeln(' PASS...6.4.3.3-11')
end.
  