program iotst3;

public var
  f, g: text;

begin
  open (f, 'FOO2.TMP');
  rewrite (g, 'FOO.OUT');
  loop
    get (f);
  exit if eof (f);
    if eoln (f) then begin
      g^ := chr (15b); put (g);
      g^ := chr (12b)
      end
    else g^ := f^;
    put (g)
  end;
  close
end.
