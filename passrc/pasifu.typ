$PAGE PASIFU.TYP, last modified 1/13/84, zw
$IFNOT pasifutyp

(*SYSTEM PASIST.TYP*)

(*if_file_index used only in PASIF module.  A variable of this
  type must be declared in PASDAT so type is declared here*)

TYPE (*intermediate form file index*)
if_file_index = ^if_file_node;
if_file_node = PACKED RECORD
  next: if_file_index;
  block: blk;
  cursor: INTEGER;
END;

$ENABLE pasifutyp
$ENDIF
  