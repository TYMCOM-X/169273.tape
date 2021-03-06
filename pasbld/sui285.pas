
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 285*)
(*TEST 6.8.3.10-6, CLASS=CONFORMANCE*)
(* This test checks that the order of evaluation of the
  record-variable-list in a with statement is correctly
  implemented. The compiler fails if the program prints FAIL. *)
program t6p8p3p10d6;
type
   pp = ^ptr;
   ptr = record
           i:integer;
           link:pp;
         end;
var
   p,q,r : pp;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #285');
   new(p);
   p^.i := 0;
   new(q);
   q^.i := 0;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   p^.link := q;
   new(r);
   r^.i := 0;
   r^.link := nil;
   q^.link := r;
   with p^, link^, link^ do
   begin
      i:=5;
   end;
   if ((r^.i=5) and (q^.i=0) and (p^.i=0)) then
      writeln('PASS...6.8.3.10-6, WITH')
   else
      writeln(' FAIL...6.8.3.10-6, WITH');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 