$IFNOT asktyp

CONST
  ask_wrd_len = 20;

TYPE
  ask_wrd = STRING[ask_wrd_len];
  ask_lst = ARRAY[*] OF ask_wrd;

$ENABLE asktyp
$ENDIF
   