program iotst2;

var
  f,g,h: text;

begin
  rewrite (f, 'TTY:');
  f^ := 'H';
  put (f);
  f^ := 'i'; put (f);
  f^ := ' '; put (f);
  f^ := 'Y'; put (f);
  f^ := 'o'; put (f);
  f^ := 'u'; put (f);
  f^ := chr (15b); put (f);
  f^ := chr (12b); put (f);
  close (f)
end.
    