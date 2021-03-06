$IFNOT parseinc

$SYSTEM ask.typ
$SYSTEM err.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ
$SYSTEM setup.typ
$SYSTEM bufio.typ
$SYSTEM scan.typ
$SYSTEM sym.typ
$SYSTEM grmr.typ

$INCLUDE parse.typ

EXTERNAL PROCEDURE st_tre(tre: par_tre_typ);
(*store parse tree to OUTPUT file*)

EXTERNAL PROCEDURE parse
(str: sym_str_typ; grmr: grmr_typ; VAR par_tre: par_tre_typ);
(*parse string using grammar, output parse tree*)

$ENABLE parseinc
$ENDIF
    