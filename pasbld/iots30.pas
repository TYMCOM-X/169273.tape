program iots30;

var
  f, g: text;
  r: -1.0e23 .. 1.0e23  prec 16;
  i: -10000 .. 10000;

procedure writeit (r: -1.0e23 .. 1.0e23 prec 16);
  var i: 1..20;

  begin
    for i := 1 to 5 do
      writeln (g, '[', r:24:i*4:e, '] [', r:22:i*4, '] [', r:22:i*4, ']');
    break (g);
  end;

begin
  rewrite (g, 'tty:');
  open (f, 'tty:');
  r := 2.0;
    loop
      if eoln (f) then begin
	write (g, 'Enter real number(s):'); break (g); readln (f)
	end;
      read (f, r:10);
    exit if r = 0.0;
      writeit (r);
      writeit (1.0/r);
    end
end.
 