(*utlscn.typ, last modified 5/5/83, zw*)
$IFNOT utlscntyp

CONST key_len = 20;
TYPE
  key_idx = one .. key_len;
  key = STRING [key_len];

$ENABLE utlscntyp
$ENDIF
(*end of utlscn.typ*)
   