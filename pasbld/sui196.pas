
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 196*)
(*TEST 6.6.6.2-11, CLASS=IMPLEMENTATIONDEFINED*)
(* This program determines some of the characteristics of the
  floating-point arithmetic system of the host machine.
  If the program fails or the printed results do not agree
  with the known data for the machine then the program
  should be checked because some of the assumptions made
  about floating-point arithmetic may be invalid for that
  machine. *)
program t6p6p6p2d11;
(* If the results from this test are not in conformity with
  the known data for the implementation,  then all copies of
  MACHAR should be replaced by an equivalent that assigns
  the appropriate values to the parameters. *)
var
   eps , epsneg , xmax , xmin : real;
   ibeta , iexp , irnd , it , machep , maxexp , minexp , negep , ngrd :
   integer;
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
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #196');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   machar ( ibeta , it , irnd , ngrd , machep , negep , iexp , minexp ,
      maxexp , eps , epsneg , xmin , xmax );
   writeln(' OUTPUT FROM MACHAR');
   writeln;
   writeln;
   writeln(' BETA =',ibeta:5);
   writeln;
   writeln('    T =',it:5);
   writeln;
   writeln('  RND =',irnd:5);
   writeln;
   writeln(' NGRD =',ngrd:5);
   writeln;
   writeln(' MACHEP =',machep:5);
   writeln;
   writeln(' NEGEP =',negep:5);
   writeln;
   writeln(' IEXP =',iexp:5);
   writeln;
   writeln(' MINEXP =',minexp:5);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   writeln;
   writeln(' MAXEXP =',maxexp:5);
   writeln;
   writeln(' EPS =',eps:15);
   writeln;
   writeln(' EPSNEG =',epsneg:15);
   writeln;
   writeln(' XMIN =',xmin:15);
   writeln;
   writeln(' XMAX =',xmax:15);
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  