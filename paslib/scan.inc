$IFNOT scaninc

$SYSTEM ask.typ
$SYSTEM err.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ
$SYSTEM setup.typ
$SYSTEM bufio.typ

$INCLUDE scan.typ

EXTERNAL PROCEDURE scan;
(*scan token from INPUT file*)

EXTERNAL VAR tkn: tkn_typ;
(*current token set by scan*)

EXTERNAL VAR tkn_chr_pos, tkn_lin_pos: INTEGER;
(*character and line position of current token*)

$ENABLE scaninc
$ENDIF
