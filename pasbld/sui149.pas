
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 149*)
(*TEST 6.6.3.1-1, CLASS=CONFORMANCE*)
(* This program tests that parameters as described by the Pascal
  Standard are permitted by the compiler, especially long
  identifier lists. A parameter list with 30 elements is thought
  long enough to test most applications using procedure/function
  parameter lists. This test occurs elsewhere in the suite, but
  is included here for consistency.
  The compiler fails if the program does not compile. *)
program t6p6p3p1d1;
type
   colour   = (red,orange,yellow,green,blue,brown);
   subrange = red..blue;
   rekord   = record
               a : integer
              end;
   ptrtype  = ^rekord;
var
   a,b,c,d,e,f,g,h,i,j,
   k,l,m,n,o,p,q,r,s,t : integer;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   colone : colour;
   coltwo : colour;
   colthree : colour;
   u,v,w,x : real;
   y,z : boolean;
   ptr : ptrtype;
procedure testone(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,
                  l1,m1,n1,o1,p1,q1,r1,s1,t1 : integer;
                  colourone : subrange;
                  colourtwo,colourthree : colour;
                  u1,v1,w1,x1 : real;
                  y1,z1 : boolean;
                  ptr : ptrtype);
begin
   write(' PASS')
end;
procedure testtwo(var a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,
                  l1,m1,n1,o1,p1,q1,r1,s1,t1 : integer;
                  var colourone : subrange;
                  var colourtwo,colourthree : colour;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                  var u1,v1,w1,x1 : real;
                  var y1,z1 : boolean;
                  var ptr : ptrtype);
begin
   writeln('...6.6.3.1-1')
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #149');
   a:=0; b:=0; c:=0; d:=0; e:=0; f:=0; g:=0;
   h:=0; i:=0; j:=0; k:=0; l:=0; m:=0; n:=0;
   o:=0; p:=0; q:=0; r:=0; s:=0; t:=0;
   colone:=orange;
   coltwo:=brown;
   colthree:=red;
   u:=0; v:=0; w:=0; x:=0;
   y:=true;
   z:=false;
   new(ptr);
   testone(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,
           colone,coltwo,colthree,u,v,w,x,y,z,ptr);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   testtwo(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,
           colone,coltwo,colthree,u,v,w,x,y,z,ptr);
end.
 