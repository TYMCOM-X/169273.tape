program invert;

var
  f, g: text;
  h, j: file of integer;
  i, len: integer;
  filename: string[60];

begin
  open (f, 'tty:'); rewrite (g, 'tty:');
  write (g, 'File to invert: '); break (g); readln (f);
  read (f, filename);
  open (h, filename, [randio]);
  write (g, 'Output: '); break (g); readln (f);
  read (f, filename);
  rewrite (j, filename);
  len := extent (h);
  repeat
    seek (h, len); j^ := h^; put (j); len := len -1
  until len = 0;
  close
end.
