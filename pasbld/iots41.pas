program iots41;

var
  s: string[200];

begin
  open(tty); rewrite (ttyoutput, [control]);
  loop
    write (tty, 'Enter line: '); break (tty); readln (tty);
    read (tty, s);
    writeln (tty, 'ok.');
  exit if s = '';
    writeln (tty, s)
  end
end.
  