type
  unit_range = integer;	(* addressing units in memory *)
  bit_range =  unit_range;			(* bits in memory *)
  char_range = unit_range;			(* chars in memory *)
  align_range = 0..128;				(* possible bit alignments *)

  prec_type = 1..16;				(* possible real precisions *)

  set_range = 0 .. #o777777;

  id_range = 0 .. #o777777; (* for numbering symbols, tuples, etc. *)
  index_range = 0 .. 4095; (* for numbering scope and control flow blocks *)

  parm_range = 0 .. 1023; (* for counting subroutine parameters *)
  parm_index = 1 .. maximum (parm_range);

  deb_file_index = 0 .. #o377777; (* index into debugger symbol table file *)
