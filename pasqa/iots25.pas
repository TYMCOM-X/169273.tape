program iots25;

type
  singlereal = -1.0e23 .. 1.0e23 prec 6;
  doublereal = -1.0e23 .. 1.0e23 prec 17;

var
  f, g: text;
  r: singlereal;
  d: doublereal;

begin
  open (f, 'tty:'); rewrite (g, 'tty:');
  read (f, r:12); read (f, r:12:6);
  read (f, r:12:6:e);  read (f, r:12:6:f)
end.
