external string procedure cvHexS( integer toHex );
COMMENT
! Returns a string of hex digits for the unsigned argument passed in
! same conventions for length as CVS and CVOS.
;

external integer routine cvHex( string AsciiHex; integer start(0) );
COMMENT
! Like CVO, but (1) first character of the string must be a hex digit.
! (2) If more than 36 bits are specified, only returns the low order 36.
! (3) !SKIP! is set to the break character (-1 if end-of-string).
! (4) value is derived by shifting out "start".
;

