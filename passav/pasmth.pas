$TITLE PASMTH -- Miscellaneous Mathematical Functions

module pasmth;

$HEADER pasmth.hdr
$PAGE ngm
(*  NGM returns the "next greater multiple".  Ngm(a,b) is defined as the
    smallest n, such that n >= a and n | b (n is divisible by b).  *)

public function ngm ( a, b: integer ): integer;

begin
  ngm := ((a+b-1) div b) * b;
end;
$PAGE nlm
(*  NLM returns the "next lower multiple".  Nlm(a,b) is defined as the
    smallest n, such that n <= a and n | b (n is divisible by b).  *)

public function nlm ( a, b: integer ): integer;

begin
  nlm := (a div b) * b;
end;
$PAGE gcd
(*  GCD (a,b) is the greatest common divisor of A and B.  *)

public function gcd ( a, b: integer ): integer;

var c: integer;

begin
  gcd := a;
  c := b;
  while c <> gcd do begin
    if c > gcd
      then c := c - gcd
      else gcd := gcd - c;
  end;
end (* gcd *);
$PAGE int_bits
(*  IntBits calculates the number of bits required to represent an integer
    in twos-complement notation.  The formula for this is

        int_bits (n)    = ceil (log2 (x + 1) )          , if n > 0
                        = 1                             , if n = 0
                        = ceil (log2 (abs (x) ) ) + 1   , if n < 0      *)

public function int_bits ( x: integer ): integer;

var t: integer;

begin
  if x >= 0 then begin
    int_bits := 1;
    t := x;
  end
  else begin
    int_bits := 2;
    t := - (x + 1);
  end;
  while t > 1 do begin
    t := t div 2;
    int_bits := int_bits + 1;
  end;
end (* int_bits *).
