$IFNOT grmrinc

$SYSTEM ask.typ
$SYSTEM err.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ
$SYSTEM setup.typ
$SYSTEM bufio.typ
$SYSTEM scan.typ
$SYSTEM sym.typ

$INCLUDE grmr.typ

EXTERNAL PROCEDURE ld_grmr(VAR grmr: grmr_typ);
(*load grammar from INPUT*)

EXTERNAL PROCEDURE st_grmr(grmr: grmr_typ);
(*store grammar to OUTPUT*)

$ENABLE grmrinc
$ENDIF
 