
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  81*)
(*TEST 6.4.3.3-2, CLASS=CONFORMANCE*)
(* The Pascal Standard states that the occurrence of a field
  identifier within the identifier list of a record section is
  its defining occurence as a field identifier for the record
  type in which the record section occurs.
  This should allow redefinition of a field identifier in another
  type declaration.
  The compiler fails if the program does not compile. *)
program t6p4p3p3d2;
type
   a     = record
            realpart : real;
            imagpart : real
           end;
   realpart = (notimaginary,withbody,withsubstance);
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #081');
   writeln(' PASS...6.4.3.3-2')
end.
   