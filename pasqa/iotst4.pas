program iotst4;

var
  f, g: text;

begin
  open (f, 'FOO.INP');
  rewrite (g, 'FOO.OUT');
  loop
    get (f);
  exit if eof (f);
    while eoln (f) do begin
      readln (f);
      writeln (g)
      end;
    g^ := f^;
    put (g)
  end;
  close
end.
    