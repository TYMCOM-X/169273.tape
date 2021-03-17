program tst106;

 var
   s: -10e4..10e4 prec 5;
   d: -10e4..10e4 prec 12;
   i: 0..64;
   ch: char;

begin
 s := random;
 s := random (d);

(*
 ch := lowercase (ch);
 ch := uppercase (ch);
*)

 s := abs (s);
 d := abs (d);

 i := trunc (s);
 i := trunc (d);

 i := round (s);
 i := round (d);

 i := max (i, i);
 s := max (s, s);
 d := min (d, d);
 d := max (d, d);

 s := max (i, s);
 s := max (i, s, d);

 s := round (s, i);
 d := round (d, 3);

 d := d * s;
 d := d * i;
 s := s * i;
 d := i;
 s := i;
 s := d;
 d := s;
 s := sqrt (s);
 d := sqrt (d);

 s := ln (s);
 d := ln (d);

 d := arctan (d);
 d := arctan (d, d);

 i := i ** i;
 s := s ** i;
 d := d ** i;
 s := s ** s;
 d := d ** d;
end.
