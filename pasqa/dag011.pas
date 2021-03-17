program dag011;

var i, j, k, l, m, n: integer;

procedure fish;
begin
  j := 0;
end;

begin
  if i = j then begin
    i := j;
    k := i;
    i := 0;
  end
  else begin
    i := j;
    fish;
    k := i;
    i := 0;
  end;
end.
