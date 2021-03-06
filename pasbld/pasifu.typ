$PAGE PASIFU.TYP, last modified 5/11/84
(*  The IfFileIndex type is used only in the PASIF module.  However, a
    variable of this type must be declared in PASENV so that it can be
    preserved between passes, so the type is declared here.  *)

TYPE
if_file_index = ^ if_file_node;
if_file_node = PACKED RECORD
  next: if_file_index;
  block: blk;
  CURSOR: INTEGER;
END;
    