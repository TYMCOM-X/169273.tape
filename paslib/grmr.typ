$IFNOT grmrtyp

TYPE

prod_typ = ^prod_rcd;
prod_rcd = RECORD str: sym_str_typ; next: prod_typ END;
(*productions are linked lists of symbol strings*)

prod_set_typ = ARRAY[sym_typ] OF prod_typ;
(*production sets have production lists for each non-terminal symbol*)

grmr_typ = RECORD
  non_terms: sym_set_typ; terms: sym_set_typ;
  prods: prod_set_typ; start: sym_typ
END;
(*context-free grammars follow standard mathematical form*)

CONST nul_prod: prod_typ = NIL;
(*null production*)

$ENABLE grmrtyp
$ENDIF
  