program aggass;
var a : array [1..3] of char;
    aa : array [1..3,1..3] of char;
    c : char;
begin
  a := (c, 'a', 'b');
  aa := ( (c, 'a', 'b'), a, a );
end.
