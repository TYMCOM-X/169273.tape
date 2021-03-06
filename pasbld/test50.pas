program test50 options dump;

type color = (red, green, yellow, blue, violet, orange);

type
  t1 = 1..4;
  t2 = 1.0 .. 4;
  t3 = 1.0 .. 4.0;
  t4 = 1..4 prec 12;
  t5 = 1..5.0 prec 17;
  t6 = 1..2 prec 20;
  t7 = 1..2 prec -1;

var
  r1: real;
  r2: t2;
  r3: t3;
  r4: t4;
  r5: t5;

  i: integer;
  b: boolean;

begin
  r1 := r4 + i+ 2.345;
  b := i < r3;
  writeln (tty, r4);
  stop
end.
    