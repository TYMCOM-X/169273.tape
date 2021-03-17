$options sp, noch, main
var ch: char;
begin
  rewrite (tty);
  open (input, 't.pas');
  writeln (tty,filename (input));
  break (tty);
  while not eof (input) do begin
    readln;
    while not eoln (input) do begin
      read (ch);
      write (tty,ch);
    end;
    writeln(tty);
  end;
end.
