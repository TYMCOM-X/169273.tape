program iots22;

var
  f: text;
  r: real;

begin
  rewrite (f, 'TTY:');
  r := 2.00001;
  repeat
    r := r * 2.0;
    writeln (f, 'G[', r, ']  F[', r:14:7:f, ']  E[', r:12:7:e, ']' );
  until (r > 1.0) and (r < 2.3)
end.
   