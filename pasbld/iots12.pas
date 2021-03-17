program iotst5;

var
  f, g: text;
  s: string[200];

begin
  open (f, 'FOO.INP');
  get (f);
  rewrite (g, 'FOO.OUT');

  while not eof (f) do begin
    read (f, s);  writeln (g, s);
    readln (f)
    end;
  close (f);
  close (g);
  rewrite (f, 'TTY:');
  f^ := '&';
  put (f);
  close (f)
end.
