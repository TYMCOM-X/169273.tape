$IFNOT scantyp

CONST tkn_siz = 20;

TYPE tkn_typ = STRING[tkn_siz];

CONST
new_tkn: tkn_typ = '';
end_tkn: tkn_typ = CHR(0);

$ENABLE scantyp
$ENDIF
   