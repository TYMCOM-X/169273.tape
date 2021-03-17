program iots35;

type
  littlearray = array [1..200] of integer;

var
  f, g: text;
  starfile: file of *;
  readerin: littlearray;
  filename: string[50];
  width, i: 0..10000;

begin
  open (f, 'tty:'); rewrite (g, 'tty:');
  write (g, 'Enter star file name to create: '); break (g); readln (f);
  read (f, filename);
  if filename = '' then stop;
  rewrite (starfile, filename);
    loop
      for i := 1 to 200 do readerin [i] := 0;
      write (g, 'Enter width, 0 to stop: '); break (g); readln (f);
      read (f, width);
    exit if width = 0;
      for i := 1 to width do readerin[i] := width - i + 1;
      write (starfile, readerin);
      write (starfile, readerin:width )
    end;
  close
end.
