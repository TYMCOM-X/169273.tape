$IFNOT parsetyp

TYPE
par_tre_typ = ^par_tre_rcd;
par_tre_rcd = RECORD sym: sym_typ; next: par_tre_typ; prod: par_tre_typ END;
(*parse tree nodes are lists of symbols -- multi-forked trees*)

CONST nul_par_tre: par_tre_typ = NIL;
(*null parse tree*)

$ENABLE parsetyp
$ENDIF
   