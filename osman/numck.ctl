; Get rid of first 3 columns of every line
R TYPE
NUMCK.CBL=NUMCK.IBM/COL:4:135/EXIT
; Get rid of card sequence numbers at end of every line
MUNG NONUM.TEC,NUMCK.CBL
; Get rid of leading blanks in comment lines (asterisk)
MUNG NOSPC.TEC,NUMCK.CBL
; Get a listing file
COMPILE/LIST NUMCK.CBL
DIRECT NUMCK.*
    