program test55 options dump;

 function fact (x: integer): integer;
  begin
   if x <= 1 then fact := 1
     else fact := x * 1
  end;

begin
 writeln (tty, fact (4));
end.
 