program iots16;

var
  f, g: text;
  s1, s2: string[40];

begin
  open (f, 'tty:');
  rewrite (g, 'Tty:');
  s1 := '1234567890'
  s1 := s1 || s1;
  s1 := s1 || s1;
  write (g, 'Enter string:');
  break (g); readln (f); read (f, s2);
  putstring (substr (s1, 2, 20), '[', length(s2), ']', s2 );
  writeln (g, s1)
end.
 