program iots51;

const
  a = false;
  b = true;

var
  w: integer;

begin
  rewrite (ttyoutput); open (tty);
  loop
    write (tty, 'Width: '); break (tty); readln (tty);
    read (tty, w);
    write (tty, '[', a:w, '] [', b:w, '] [', (a or b):w, '] [', (a and b):w, ']');
    writeln (tty, '[', a, '] [', b, '] [', (a or b), '] [', (a and b), ']');
  end
end.
 