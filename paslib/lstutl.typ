(*lstutl.typ, last modified 9/19/83, zw*)

$IFNOT lstutltyp

TYPE
    listflags = SET OF (listodd, listeven, listlegal, listdouble, listbreak,
      listheader, listwrap, listlogical, listgo, listclean);

$ENABLE lstutltyp
$ENDIF
