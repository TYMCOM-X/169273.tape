
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 128*)
(*TEST 6.4.6-11, CLASS=DEVIANCE*)
(* This program tests the latter half of the statement in
  6.4.6-10.
  The compiler conforms if the program does not compile. *)
program t6p4p6d11;
type
   rekord = record
               f : text;
               a : integer
            end;
var
   record1 : rekord;
   record2 : rekord;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #128');
   record1.a:=1;
   reset(record1.f);
   rewrite(record2.f);
   writeln(record1.f);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   record2:=record1;;
   writeln(' DEVIATES...6.4.6-11')
end.
  