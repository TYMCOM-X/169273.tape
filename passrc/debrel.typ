(* DEBREL.TYP - type definitions for structures found in Pascal
   rel files compiled with the debug option.  File PASCAL.INC must
   be included before this include file.  *)

type
  half_word = 0..777777b;

  (* "Pointers" to symbol table nodes stored in .rel file structures below,
     and in symbol table nodes in .deb files, are actually integer offsets
     within the .deb file where the node pointed to begins.  The following
     synonyms for HALF_WORD are used to document what kind of node a
     given offset "points" to.  The public routine DEREF$PTR will accept one
     of these offsets and return a pointer to the location in core where
     it will have loaded the referenced node (see DEBSYM).  *)
  
  intblk = half_word;				(* offset of a BLOCK_NODE - converted to
 						   a BLK by DEREF$PTR *)
  inttyp = half_word;				(* offset of a TYPE_NODE - converted to a TYP *)
  intval = half_word;				(* offset of a VALUE_NODE - converted
						   to a VAL_PTR  *)
  intsym = half_word;				(* offset of a SYMBOL_NODE - converted to a SYM *)
  intnam = half_word;				(* offset of a NAME_NODE - converted to a NAM *)
$PAGE statement, page, and file blocks
type
  stmt_kinds = (assignment_stmt, if_stmt, for_stmt, loop_stmt, while_stmt,
	        case_stmt, with_stmt, goto_stmt, io_stmt, return_stmt,
		stop_stmt, exit_clause, untilsy, repeat_stmt,
		end_keyword, proc_call);
  
  stmt_block = packed record		(* word following jsp 1,stmt. *)
		 previous_stmt: ^stmt_block;	(* ^page_block for first stmt *)
                 stmt_kind: stmt_kinds;		(* code for source stmt kind *)
		 line_number: 0..37777b	(* remaining 14 bits for line no *)
	      end;
  stmt_block_ptr = ^stmt_block;  (* all references outside this file will be to
				    stmt_block_ptr, not to stmt_block *)

  page_block = packed record	
		 previous_page: ^page_block;	(* ^file_block for first page in file *)
                 block_id: half_word;		(* MUST be zero - distinguishes page blocks
						   from stmt and file blocks *)
		 last_stmt: ^stmt_block;		(* to last stmt in page *)
		 page_number: half_word;
		 subtitle: string[80]		(* exact length allocated; all alphanumeric
						   and underscore characters following $PAGE up
						   to first character which is not an
						   alphanumeric or underscore.  *)
	      end;
  page_block_ptr = ^page_block;  (* all references outside this file will be to
				    page_block_ptr, not to page_block *)

  file_block = packed record			
		 previous_file: ^file_block;	(* NIL for first file *)
		 last_page: ^page_block;		(* block for last page in file *)
                 program_blk: ^prog_block;	(* pointer to program block *)
		 file_number: half_word;
		 file_name: string[80]		(* exact length allocated; extension
						   is not included.  *)
	      end;
  file_block_ptr = ^file_block;  (* all references outside this file will be to
				    file_block_ptr, not to file_block *)
$PAGE program and procedure blocks
  file_name_string = string[ 80 ];

  prog_block = packed record			(* one per rel file *)
		    last_file: ^file_block;	(* backlink through "previous_file" fields *)
                    prog_filler: half_word;	(* unused *)
                    comp_date: int_type;		(* day and time of compilation - internal
							   day/time from day/time package *)
		    hiseg_base: half_word;	(* code relocation base *)
		    lowseg_base: half_word;	(* static relocation base *)
		    intpoint: inttyp;		(* intptr *)
		    realpoint: inttyp;		(* realptr *)
		    charpoint: inttyp;		(* charptr *)
		    boolpoint: inttyp;		(* boolptr *)
		    symfile_name: file_name_string	(* name of .REL file.  *)
		  end;

  procedure_block = packed record		
		       pflev: half_word;	(* static level of objects declared WITHIN routine *)
		       block_node: intblk;	(* symbol table offset of block node for
 						   the procedure or zero *)
		       prog_blk: ^prog_block;	(* pointer to program block, always present!!!  *)
						(* if not DEBUG, a zero word replaces prog block *)
                       first_stmt: stmt_block_ptr;	(* pointer to statement block for first
						   statement of procedure or NIL *)
		       proc_name: string[80]	(* exact length allocated.  *)
		     end;

  procblklink = packed record			(* word following call to PNTRY. at start
						   of a procedure's code *)
    filler1: half_word;
    proc_blk: ^procedure_block			(* ptr to proc block for the procedure *)
  end;

  (* A module record is the first item emitted by the compiler into the REL file.  *)

  r50word = 0..37777777777b;			(* 32 bits of info *)
  module_record = record
    r50_name: r50word;				(* module name in radix 50 *)
    program_block: prog_block			(* program block for the module if debug;  if not
						   debug, a word containing zero will be present.  *)
  end;
