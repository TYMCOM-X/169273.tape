
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 284*)
(*TEST 6.8.3.10-5, CLASS=CONFORMANCE*)
(* This test checks that the selection of a variable in the
  record-variable-list is performed before the component
  statement is executed. The compiler fails if the program
  does not compile or the program prints FAIL. *)
program t6p8p3p10d5;
type
   pointer = ^recordtype;
   recordtype = record
                  data:integer;
                  link:pointer
                end;
var
   counter:integer;
   p,q:pointer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #284');
   counter:=0;
   new(p);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   p^.data:=0;
   new(q);
   q^.data:=1;
   q^.link:=nil;
   p^.link:=q;
   q:=p;
   with q^ do
   begin
      q:=link;
      if (data=0) and (q^.data=1) then
         counter:=counter+1;
   end;
   with p^ do
   begin
      p:=link;
      (* The first record now has no reference, so it could
        be deleted prematurely. *)
      if (data=0) and (p^.data=1) then
         counter:=counter+1;
   end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if counter=2 then
         writeln(' PASS...6.8.3.10-5, WITH')
      else
          writeln(' FAIL...6.8.3.10-5, WITH');
end.
   