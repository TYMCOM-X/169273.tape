
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 280*)
(*TEST 6.8.3.10-1, CLASS=CONFORMANCE*)
(* This program checks the implementation of the with statement.
  The compiler fails if the program does not compile or it
  compiles and prints FAILS. *)
program t6p8p3p10d1;
var
   r1:record
        a,b:integer
      end;
   r2:record
        c,d:integer
      end;
   r3:record
        e,f:integer
      end;
   counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #280');
   counter:=0;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   with r1 do
      a:=5;
   with r1,r2,r3 do
   begin
      e:=a;
      c:=a
   end;
   with r2 do
      if c=5 then
         counter:=counter+1;
   if r2.c=5 then
      counter:=counter+1;
   if counter=2 then
      writeln(' PASS 6.8.3.10-1, WITH')
   else
      writeln(' FAIL 6.8.3.10-1, WITH');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  