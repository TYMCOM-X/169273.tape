
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 233*)
(*TEST 6.8.2.1-1, CLASS=CONFORMANCE*)
(* Does the compiler allow all the possible empty clauses?
  The compiler fails if the program does not compile and print
  PASS. *)
program t6p8p2p1d1;
var
   b:boolean;
   r1:record
       x:real;
       a:integer;   (*1*)
       end;
   r2:record
       case b:boolean of
       true:(
             c:real;
             d:char;   (*2*)
            );
       false:
             (e:integer);   (*3*)

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

       end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #233');
   b:=true;
  if b then;   (*4*)
   if b then else;   (*5*)
   repeat
      b:= not b;   (*6*)
   until b;
   while b do
   begin
      b:=not b;   (*7*)
   end;
   with r1 do;   (*8*)
  r1.a:=1;
   case r1.a of
   0:  b:=false;
   1:  ;   (*9*)
   2:  b:=true;   (*10*)
   end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   writeln(' PASS...6.8.2.1-1, EMPTY STATEMENT');   (*11*)
end.
