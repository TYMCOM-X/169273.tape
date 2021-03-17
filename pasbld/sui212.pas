
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 212*)
(*TEST 6.7.1-2, CLASS=CONFORMANCE*)
(* This program tests that the precedence of the arithmetic
  operators is as described by the Pascal Standard.
  The compiler fails if the program does not compile, or
  the program states that this is so. *)
program t6p7p1d2;
var
   a,b,c,d,e,f,g : integer;
   h,i,j,k,l,m,n : real;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #212');
   a:=1;
   b:=2;
   c:=3;
   d:=4;
   e:=5;
   f:=a-b+c-d;
   g:=e-d div b*c;
   h:=1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   i:=2;
   j:=3;
   k:=4;
   l:=5;
   m:=h/i*j/k;
   n:=l+k/i-3*j;
   if (f=-2) and (g=-1) and (n=-2) and
      ((m<0.38) and (m>0.37)) then
      writeln(' PASS...6.7.1-2')
   else
      writeln(' FAIL...6.7.1-2')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

