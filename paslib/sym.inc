$IFNOT syminc

$SYSTEM ask.typ
$SYSTEM err.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ
$SYSTEM setup.typ
$SYSTEM bufio.typ
$SYSTEM scan.typ

$INCLUDE sym.typ

EXTERNAL PROCEDURE mak_sym(nam: nam_typ);
(*make symbol -- insert name into symbol table*)

EXTERNAL FUNCTION lkp_sym(nam: nam_typ; VAR sym: sym_typ): BOOLEAN;
(*lookup symbol given name, return TRUE if found*)

EXTERNAL VAR sym_nam: sym_tab_typ;
(*global symbol table -- valid symbols have non-null names*)

EXTERNAL PROCEDURE ld_sym(VAR sym: sym_typ);
(*load symbol from INPUT file*)

EXTERNAL PROCEDURE st_sym(sym: sym_typ);
(*store symbol to OUTPUT file*)

EXTERNAL PROCEDURE ld_str(VAR sym_str: sym_str_typ);
(*load symbol string from INPUT file*)

EXTERNAL PROCEDURE st_str(sym_str: sym_str_typ);
(*store symbol string to OUTPUT file*)

EXTERNAL PROCEDURE ld_set(VAR sym_set: sym_set_typ);
(*load symbol set from INPUT file*)

EXTERNAL PROCEDURE st_set(sym_set: sym_set_typ);
(*store symbol set to OUTPUT file*)

$ENABLE syminc
$ENDIF
 