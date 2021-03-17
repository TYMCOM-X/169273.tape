program iots37;

var
  f, g: text;
  b: packed array [1..200] of char;
  i: 0..200;

begin
  open (f, 'FOO.INP');
  get (f);
  rewrite (g, 'FOO.OUT');

  while not eof (f) do begin
    i := 1;
    while not eoln (f) do begin
      b[i] := f^; i := i+1; get (f) end;
    writeln (g, b:i-1);
    readln (f)
    end;
  close (f);
  close (g);
  rewrite (f, 'TTY:');
  f^ := '&';
  put (f);
  close (f)
end.
