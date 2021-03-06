$PAGE PASEMU.TYP, last modified 1/13/84, zw
$IFNOT pasemutyp

(*SYSTEM PASIST.TYP*)
(*SYSTEM PASIF.TYP*)

TYPE (*expression manipulation structures*)
wl_type = ^wl_node;
wl_node = RECORD
  with_ref: expr; (*record reference expression*)
  last: wl_type (*with list link*)
END;
tag_value_list = ^tag_val_node;
tag_val_node = RECORD
  tag: sym; (*tagged symbol*)
  labval: val; (*tagged value*)
  next: tag_value_list
END;

$ENABLE pasemutyp
$ENDIF
  