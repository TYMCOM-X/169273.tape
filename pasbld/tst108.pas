program tst108;

type
  color = (red, blue, white, black, yellow, orange, green, purple, mustard);

type
  smallset = set of 0..12;
  smallset2 = set of 10..24;
  mediumset = set of 1..71;
  oddset = set of 34..80;
  bigset = set of 3..255;
  colorset = set of color;
  charset = set of char;

var
  ss: smallset;
  ss2: smallset2;
  ms: mediumset;
  os: oddset;
  bs: bigset;
  cs: colorset;
  chs: charset;
  str: string[24];
  ascii: set of ' '..'''';

  i,j: lowerbound (bs)..upperbound (bs);
  k: lowerbound (ss)..upperbound (ss);
  bint: integer;
  flag: boolean;


  function setfun (a1: smallset2; a2: bigset): bigset;
   begin
    setfun := a1 + a2;
   end;


begin
  bs := bs;
  bs := [];
  bs := [i];
  bs := [i..j];

  ss := bs;
  os := bs;

  bs := bs * bs;
  bs := bs - ss;
  bs := bs + ms;

  bs := ms + [i..j];
  bs := (ms + ss) - [i];

  bs := setfun (ss, bs);
  ss := setfun (ss2, bs);

  if bs = bs then
  if ss = bs then
  if bs <= [i..j] then
  if not (bs <= [i..j]) then
  if [i..k, j] <= [k..bint] then
  if bs <> bs then 
  if k in bs then
  if not (k in bs) then ;

  i := verify ('abcdef', chs);
  i := verify (substr (str, i, j), ['A'..'Z']);
  j := search (uppercase (str), ascii);
end.
 