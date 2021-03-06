program tst103 options dumpif, nocheck, dumpst;

 type matrix = array [1..6, 2..8] of 1.0..12.0 prec 12;
      mat2   = array [-4..5, 2..3, 0..6] of 1.0..24.0 prec 12;

 var i, j, k, l: 0..16000;
     a: matrix;
     b: mat2;
     r: 1.0..12.0 prec 12;

 begin
  i := (j - 3 - k) * 2;
  j := (j * 4) + (l * 2);
  k := (3 + 4) div 5;
  l := (i + 3) * j;
  i := (j + 3) + (k - 3);
  j := ((k * 4) - 12) - (3 + (i * 2));
  k := (-j * 2) + (4 - (l * 2));

  l := 3 * 4;
  i := 2 * (4 + (k * 7));
  j := (i * 3) * (k * 2);
  k := ((i * -2) * (3 + (i * 17))) - j;
  l := abs (i * -12);

  r := a [j-3, k+4];
  b [(j*2), j-k, i+3-l+2] := a [3, i-12];
  b [i, j, k] := a [j, k];
  a [i, j] := r;
end.
  