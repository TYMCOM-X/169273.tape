program iots15;

var
  f, g: text;
  i: -10000..10000;
  s1, s2: string[40];
  p1: string[80];

begin
  open (f, 'TTY:');
  rewrite (g, 'TTY:');
    loop
      write (g, 'Enter number:');
      break (g);
      readln (f);
      read (f, i);
      s1 := ''; s2 := '';
      p1 := '';
      repeat
	s2 := s1;
	s1 := '';
	putstring (s1, s2, '[', i, ']');
	p1 := '123456789012345678901234567890';
	p1 := p1 || p1;
	putstring (substr (p1, 2, length (s1) + 3), s2, '[', i, ']' );
	writeln (g, s1:40:l, ':', length (s1):3, '.');
	writeln (g, p1);
      until s1 = s2;
    exit if i = 0
    end
end.
 