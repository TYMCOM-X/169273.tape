program iots14;

var
  f, g: text;
  i: -10000..10000;
  s1, s2: string[40];

begin
  open (f, 'TTY:');
  rewrite (g, 'TTY:');
    loop
      write (g, 'Enter number:');
      break (g);
      readln (f);
      read (f, i);
      s1 := ''; s2 := '';
      repeat
	s2 := s1;
	putstring (s1, s2, '[', i, ']');
	writeln (g, s1:40:l, ':', length (s1):3, '.');
      until s1 = s2;
    exit if i = 0
    end
end.
   