program test;
begin
  rewrite (tty); open (tty);
  loop begin
    write (tty,'???? '); break; readln (tty);
  exit if eoln (tty);
    exception allconditions: begin
	writeln (tty,exception_message);
    end;
  end end;
end.
