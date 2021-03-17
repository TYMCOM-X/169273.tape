(* DEBBRK.INC - external declarations for public routines of the Pascal
   Debugger module DEBBRK.  *)

external procedure get$mod$name  (^prog_block; var id_string; var status_code);
external procedure info$stmt     (stmt_block_ptr; var source_id_record; var status_code);
external procedure get$prog$ptr  (mod_string; var ^prog_block; var status_code);
external procedure get$file$ptr  (^prog_block; file_string; ext_file_range;
				  var file_block_ptr; var status_code);
external procedure find$stmt$blk (augmntd_source_id; var stmt_block_ptr; var status_code);
external procedure clr$all$brkpts;
external procedure clr$brkpt     (brkpt_range; var status_code);
external procedure set$brkpt     (augmntd_source_id; cmd_str; var brkpt_range; var status_code);
