$IFNOT symtyp

CONST
num_syms = 10;
nam_len = 20;
end_nam = 'END'; end_sym = 1;

TYPE

sym_typ = 1 .. num_syms; nam_typ = STRING[nam_len];
(*symbols represented internally as numbers, externally as names*)

sym_tab_typ = ARRAY[sym_typ] OF nam_typ;
(*symbol table -- cross-refrence of symbol names and numbers*)

sym_str_typ = ^sym_str_rcd;
sym_str_rcd = RECORD sym: sym_typ; next: sym_str_typ END;
(*symbol strings are linked lists of symbol*)

sym_set_typ = ARRAY[sym_typ] OF BOOLEAN;
(*symbol sets have TRUE entries if indexing symbols are members*)

CONST (*null structures -- array constants must have num_syms entries*)
nul_sym_str: sym_str_typ = NIL;
nul_sym_tab: sym_tab_typ = (end_nam, '', '', '', '', '', '', '', '', '');
nul_sym_set: sym_set_typ = (FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, FALSE);

$ENABLE symtyp
$ENDIF
