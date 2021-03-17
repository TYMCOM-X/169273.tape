
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 195*)
(*TEST 6.6.6.2-10, CLASS=QUALITY*)
(* This test checks the implementation of the ln function. *)
program t6p6p6p2d10;
var
(*     data required
         none
      other subprograms in this package
         machar -  as for sqrtest
         ran(k) - as for sqrtest
      standard subprograms required
         abs, ln, sqrt
                                                                      *)
   i , ibeta , iexp ,  irnd , it , i1 , j , k , k1 , machep ,
      iy , maxexp , minexp , n , negep , ngrd : integer ;
   a , albeta , b , beta , d , del , eight , eps , epsneg , half , r5 ,
      r6 , r7 , tenth , w , x , xl , xmax , xmin , xn , x1 , y , z ,zz:
      real ;
procedure machar (var ibeta , it , irnd , ngrd , machep , negep , iexp,
  minexp , maxexp : integer ; var eps , epsneg , xmin , xmax : real ) ;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

var
(*     This subroutine is intended to determine the characteristics
      of the floating-point arithmetic system that are specified
      below.  The first three are determined according to an
      algorithm due to M. Malcolm, CACM 15 (1972), pp. 949-951,
      incorporating some, but not all, of the improvements
      suggested by M. Gentleman and S. Marovich, CACM 17 (1974),
      pp. 276-277.  The version given here is for single precision.
      Latest revision - October 1, 1976.
      Author - W. J. Cody
               Argonne National Laboratory
      Revised for Pascal - R. A. Freak
                           University of Tasmania
                           Hobart
                           Tasmania
      ibeta    is the radix of the floating-point representation
      it       is the number of base ibeta digits in the floating-point
                  significand
      irnd     =  0 if the arithmetic chops,
                  1 if the arithmetic rounds

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      ngrd     =  0 if  irnd=1, or if  irnd=0  and only  it  base ibeta
                    digits participate in the post normalization shift
                    of the floating-point significand in multiplication
                  1 if  irnd=0  and more than  it  base  ibeta  digits
                    participate in the post normalization shift of the
                    floating-point significand in multiplication
      machep   is the exponent on the smallest positive floating-point
                  number  eps such that  1.0+eps <> 1.0
      negeps   is the exponent on the smallest positive fl. pt. no.
                  negeps such that  1.0-negeps <> 1.0, except that
                  negeps is bounded below by  it-3
      iexp     is the number of bits (decimal places if ibeta = 10)
                  reserved for the representation of the exponent of
                  a floating-point number
      minexp   is the exponent of the smallest positive fl. pt. no.
                  xmin
      maxexp   is the exponent of the largest finite floating-point
                  number  xmax
      eps      is the smallest positive floating-point number such
                  that  1.0+eps <> 1.0. in particular,

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                  eps = ibeta**machep
      epsneg   is the smallest positive floating-point number such
                  that  1.0-eps <> 1.0  (except that the exponent
                  negeps is bounded below by it-3).  in particular
                  epsneg = ibeta**negep
      xmin     is the smallest positive floating-point number.  in
                  particular,  xmin = ibeta ** minexp
      xmax     is the largest finite floating-point number.  in
                  particular   xmax = (1.0-epsneg) * ibeta ** maxexp
                  note - on some machines  xmax  will be only the
                  second, or perhaps third, largest number, being
                  too small by 1 or 2 units in the last digit of
                  the significand.
                                                                    *)
   i , iz , j , k , mx : integer ;
   a , b , beta , betain , betam1 , one , y , z , zero : real ;
   underflo : boolean;
begin
   irnd := 1 ;
   one := ( irnd );

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   a := one + one ;
   b := a ;
   zero := 0.0 ;
(*
      determine ibeta,beta ala Malcolm
                                                                    *)
   while ( ( ( a + one ) - a ) - one = zero ) do begin
      a := a + a ;
   end ;
   while ( ( a + b ) - a = zero ) do begin
      b := b + b ;
   end ;
   ibeta := trunc ( ( a + b ) - a );
   beta := ( ibeta );
   betam1 := beta - one ;
(*
      determine irnd,ngrd,it
                                                                    *)
   if ( ( a + betam1 ) - a = zero ) then irnd := 0 ;
   it := 0 ;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   a := one ;
   repeat begin
      it := it + 1 ;
      a := a * beta ;
   end until ( ( ( a + one ) - a ) - one <> zero ) ;
(*
      determine negep, epsneg
                                                                    *)
   negep := it + 3 ;
   a := one ;
   for i := 1 to negep do begin
      a := a / beta ;
   end ;
   while ( ( one - a ) - one = zero ) do begin
      a := a * beta ;
      negep := negep - 1 ;
   end ;
   negep := - negep ;
   epsneg := a ;
(*

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      determine machep, eps
                                                                    *)
   machep := negep ;
   while ( ( one + a ) - one = zero ) do begin
      a := a * beta ;
      machep := machep + 1 ;
   end ;
   eps := a ;
(*
      determine ngrd
                                                                    *)
   ngrd := 0 ;
   if(( irnd = 0) and((( one + eps) * one - one) <> zero)) then
   ngrd := 1 ;
(*
      determine iexp, minexp, xmin
      loop to determine largest i such that
          (1/beta) ** (2**(i))
      does not underflow
      exit from loop is signall by an underflow

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                                                                    *)
   i := 0 ;
   betain := one / beta ;
   z := betain ;
   underflo := false;
   repeat begin
      y := z ;
      z := y * y ;
(*
      check for underflow
                                                                    *)
      if ( ( z * one = zero ) or ( abs ( z )> y ) ) then begin
         underflo := true;
      end else begin
         i := i + 1 ;
      end;
   end until underflo ;
   k := 1 ;
(*
      determine k such that (1/beta)**k does not underflow

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      first set k = 2 ** i
                                                                    *)
   for j := 1 to i do begin
      k := k + k ;
   end ;
   iexp := i + 1 ;
   mx := k + k ;
   if ( ibeta = 10 ) then begin
(*
      for decimal machines only                                     *)
      iexp := 2 ;
      iz := ibeta ;
      while ( k >= iz ) do begin
         iz := iz * ibeta ;
         iexp := iexp + 1 ;
      end ;
      mx := iz + iz - 1 ;
   end;
   underflo := false;
   repeat begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(*
      loop to construct xmin
      exit from loop is signalled by an underflow
                                                                    *)
      xmin := y ;
      y := y * betain ;
      if ( ( ( y * one ) = zero ) or ( abs ( y )> xmin ) )
      then begin
         underflo := true;
      end else begin
         k := k + 1 ;
      end;
   end until underflo ;
   minexp := - k ;
(*  determine maxexp, xmax
                                                                    *)
   if ( ( mx <= k + k - 3 ) and ( ibeta <> 10 ) ) then begin
      mx := mx + mx ;
      iexp := iexp + 1 ;
   end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   maxexp := mx + minexp ;
(*  adjust for machines with implicit leading
   bit in binary significand and machines with
   radix point at extreme right of significand
                                                                    *)
   i := maxexp + minexp ;
   if ( ( ibeta = 2 ) and ( i = 0 ) ) then maxexp := maxexp - 1 ;
   if ( i > 20 ) then maxexp := maxexp - 3 ;
   xmax := one - epsneg ;
   if ( xmax * one <> xmax ) then xmax := one - beta * epsneg ;
   xmax := ( xmax * betain * betain * betain ) / xmin ;
   i := maxexp + minexp + 3 ;
   if  ( i > 0 ) then begin
      for j := 1 to i do begin
         xmax := xmax * beta ;
      end ;
   end;
end;
function random : real ;
(*     random number generator - based on algorithm 266

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

       by Pike and Hill (modified by Hansson)
       collected Alg. from CACM.
      This subprogram is intended for use on computers with
       fixed point wordlength of at least 29 bIts.  it is
       best if the floating point significand has at most
       29 bits. *)
(*     The quality of the random numbers is not important.
      If recoding is needed for small wordlength computers,
      even returning a constant value or zero is possible. *)
(*     The value iy is global, and is initialized in the driver *)
begin
   iy := (iy*125) mod 2796203;
   random := ( iy )/ 2796203.0e0 ;
end;
procedure printtestrun (n:integer; lb,ub:real;
                        big,small : integer;
                        mean,maxerror,xmaxerror,rmserror:real);
begin
   writeln(' ':5,n:4,' RANDOM ARGUMENTS WERE TESTED FROM THE INTERVAL')
      ;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   writeln(' ':10,'(',lb:15,',',ub:15,')');
   writeln;
   writeln(' ':5,'THE RESULT WAS TOO LARGE',big:5,' TIMES, AND');
   writeln(' ':10,'TOO SMALL',small:5,' TIMES');
   writeln;
   if (mean <> 0.0) then begin
      writeln(' ':5,'MEAN RELATIVE ERROR =',mean:15,'=',
         IBETA:4,' ** ',LN(ABS(mean))/ALBETA:7:2);
   end;
   if (maxerror<> 0.0) then begin
      writeln(' ':5,'THE MAXIMUM RELATIVE ERROR OF',maxerror:15,'=',
         IBETA:4,' ** ',LN(ABS(maxerror))/ALBETA:7:2);
      writeln(' ':10,'OCCURRED FOR X =',xmaxerror:15);
   end;
   if (rmserror <> 0.0) then begin
      writeln(' ':5,'ROOT-MEAN-SQUARE RELATIVE ERROR =',rmserror:15,
         '=',IBETA:4,' ** ',LN(ABS(rmserror))/ALBETA:7:2);
   end;
   writeln;
end;   (* OF PRINT TEST RUN *)

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

function sign(a1 , a2 : real) : real;
begin
  if (a2 < 0) then
    sign := -abs(a1)
  else
    sign := abs(a1)
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #195');
   iy := 100001;
   machar ( ibeta , it , irnd , ngrd , machep , negep , iexp , minexp ,
     maxexp , eps , epsneg , xmin , xmax );
   beta := ( ibeta );
   albeta := ln ( beta );
   j := it div 3 ;
   a := 1.0 ;
   for i := 1 to j do begin
      a := a / beta ;
   end ;
   n := 2000 ;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   xn := ( n );
   b := 1.0 + a ;
   a := 1.0 - a ;
   d := 1.0 + sqrt ( eps );
   half := 0.5 ;
   eight := 8.0 ;
   tenth := 0.1 ;
   i1 := 0 ;
(*
      random argument accuracy tests
                                                                      *)
   for j := 1 to 4 do begin
      case j of
      1:
         begin
            write(' TEST OF LN(X) VS TAYLOR SERIES EXPANSION');
            writeln(' OF LN(1+Y)');
         end;
      2:
         begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

            writeln(' TEST OF LN(X) VS LN(17X/16)-LN(17/16)');
            writeln;
            a := sqrt ( half );
            b := 15.0 / 16.0 ;
            end;
      3:
         begin
            writeln(' TEST OF LN(X) VS LN(11X/10)-LN(11/10)');
            writeln;
            a := sqrt ( tenth );
            b := 0.9;
         end;
      4:
         begin
            writeln(' TEST OF LN(X*X) VS 2 * LN(X)');
            writeln;
            a := 16.0 ;
            b := 240.0 ;
         end;
      end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      del := ( b - a ) / xn ;
      k := 0 ;
      k1 := 0 ;
      x1 := 0.0 ;
      r5 := 0.0 ;
      r6 := 0.0 ;
      r7 := 0.0 ;
      xl := a ;
      for i := 1 to n do begin
         x := del * random * d + xl ;
         case j of
         1:
            begin
               y := ( x - 0.5 ) - half ;
               z := ln ( x );
               zz := 1.0 / 3.0 ;
               zz := y * ( zz - y / 4.0 ) ;
               zz := ( zz - half ) * y * y + y ;
            end;
         2:

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

            begin
               x := ( x + 8.0 ) - eight ;
               y := x + x / 16.0 ;
               z := ln ( x );
               zz := ln ( y )- 7.7746816434842581e-5 ;
               zz := zz - 31.0 / 512.0 ;
            end;
         3:
            begin
               x := ( x + 8.0 ) - eight ;
               y := x + x * tenth ;
               z := ln ( x )/ ln(10) ;
               zz := ln ( y )/ ln(10) - 3.7706015822504075e-4 ;
               zz := zz - 21.0 / 512.0 ;
            end;
         4:
            begin
               z := ln ( x * x );
               zz := ln ( x );
               zz := zz + zz ;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

            end;
         end;
         w := ( z - zz ) / z ;
         z := sign ( w , z );
         if ( z > 0.0 ) then k := k + 1 ;
         if ( z < 0.0 ) then k1 := k1 + 1 ;
         r5 := r5 + w ;
         w := abs ( w );
         if ( w > r6 ) then begin
            r6 := w ;
            x1 := x ;
         end;
         r7 := r7 + w * w ;
         xl := xl + del ;
      end ;
      r5 := r5 / xn ;
      r7 := sqrt ( r7 / xn );
      printtestrun(n,a,b,k,k1,r5,r6,x1,r7);
   end ;
(*

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      special tests
                                                                      *)
   writeln(' THE IDENTITY LN(X) = - LN(1/X) WILL BE TESTED');
   writeln;
   writeln('        X        F(X) + F(1/X)');
   writeln;
   for i := 1 to 5 do begin
      x := random ;
      x := x + x + 15.0 ;
      y := 1.0 / x ;
      z := ln ( x )+ ln ( y );
      writeln(x:15,z:15);
   end ;
   writeln;
   writeln(' TEST OF SPECIAL ARGUMENTS');
   writeln;
   x := 1.0 ;
   y := ln ( x );
   writeln(' LN(1.0) = ', y:15);
   writeln;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   x := xmin ;
   y := ln ( x );
   writeln(' LN(XMIN) = LN(', x:15, ') = ', y:15);
   writeln;
   x := xmax ;
   y := ln ( x );
   writeln(' LN(XMAX) = LN(', x:15, ') = ', y:15);
   writeln;
(*
      Test 6.6.6.2-4 checks that an error is produced
      when  ln is called with a negative argument.
                                                                 *)
   writeln(' THIS CONCLUDES THE TESTS');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   