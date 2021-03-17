
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  88*)
(*TEST 6.4.3.3-9, CLASS=QUALITY*)
(* Note this program relies on the compiler deviating for tests
  6.4.3.3-5 to 6.4.3.3-8.
  If the compiler conforms for these tests, this program will
  not compile/run.
  The method of storage for fields of variants may differ,
  depending on the method of definition.
  Programmers should not rely on the VALUES of fields under
  one variant still being accessible from another. However, the
  relationships between the two variants in this example may be
  determined by the output of the program. *)
program t6p4p3p3d9;
type
   two = (a,b);
var
   variant : record
               case tagfield : two of
                  a: (i,j,k : integer);
                  b: (l : integer;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                      m : integer;
                      n : integer)
             end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #088');
   variant.tagfield:=a;
   variant.i:=1;
   variant.j:=2;
   variant.k:=3;
   variant.tagfield:=b;
   if (variant.l=1) and (variant.m=2) then
      writeln(' EXACT CORRELATION-- I:L  J:M  K:N')
   else
   if (variant.l=3) and (variant.m=2) then
      writeln(' REVERSE CORRELATION -- I:N  J:M  K:L')
   else
      writeln(' UNKNOWN CORRELATION - lmn are:',
               variant.l,variant.m,variant.n)
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

