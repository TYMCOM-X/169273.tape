program ichtst;

procedure dropnum (n: integer);
var i, j, k: integer;
begin
  i := n;
  j := 32;
  for k := 1 to 6 do begin
    if i >= j
      then write ('1')
      else write ('0');
    i := i mod j;
    j := j div 2;
  end;
  writeln;
end;

var i, j: integer;
begin
  rewrite (output,'iching.txt');
  for i := 0 to 63 do begin
    dropnum (i);
    writeln ('  This is number',i);
    writeln ('  (Or should be)');
    writeln;
  end;
end.
    