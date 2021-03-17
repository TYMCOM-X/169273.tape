(* The debugger's SYMBOL TABLE is a pointer linked structure consisting
   of the name table, the lexically ordered tree of block nodes, symbol nodes
   within (sic) the block, type nodes, and value (constant) nodes. 

   Note: Files DEBREL.TYP and PASCAL.INC must be included before this include file.  *)

const
  level0blk = 0;			(* first record written to .DEB file is the root
					   or level 0 block node *)
  max_string_len := 130;		(* max length string user may enter; used to dimension
					   value node fields; derived from cmd line length
					   permitted by debugger *)
  max_set_size := 936;			(* max length (in bits) of user entered sets;
					   derived from MAX_STRING_LEN *)
  max_set_offset := 935;		(* max offset w/in user entered set; used to
					   dimension value node set variant *)

type
  nam = ^name_node;
  blk = ^block_node;
  sym = ^symbol_node;
  typ = ^type_node;



(* STORAGE CLASS designates the logical area of storage in which a data item
   may reside.  The class as declared (e.g. parameter) is kept in the symbol
   table. *)

  storage_class = 
     (	local_sc,				(* local to a block *)
	parameter_sc,				(* parameter to a block *)
	static_sc,				(* static to a single compiled module *)
        constant_sc,				(* constant storage of a compilation *)
	external_sc,				(* relative to an external name *)
	dynamic_sc, 				(* represents an entire heap class *)
        fileblk_sc,
        opt_sc,
	code_sc,				(* within code area of a compilation *)
	absolute_sc,				(* constant address *)
	temp_sc,				(* relative to temporary area for a block *)
	register_sc,				(* item is a register *)
	runtime_sc,				(* runtime symbol *)
	def_sc,				(* internal label *)
	self_rel_sc,			(* self relocating *)
	unallocated   );			(* data not allocated *)



(* The NAME TABLE contains the text of all <identifier>s seen during a
   compilation.  It is organized as an unbalanced binary tree. 
   NAME_NODEs for labels and named types are not written to the
   .DEB file.  *)

  name_node =
      packed record
	alink, zlink: intnam;			(* branches of tree; name < current is on alink *)
	scopechain: intsym;			(* points to 1st symbol in scope with this name;
						   chains through sym^.scopechain *)
	len: line_index;			(* length of name, as allocated *)
	text: packed array[1..40] of char	(* text of name: must be referenced as
						   substr (nam^.text, 1, nam^.len)  *)
      end;






(* VALUE NODES are used to represent constant values.  In order to save storage,
   simple scalar values are stored in a VAL record instead of a pointer to a 
   value node. The val record is used in all referencing structures. 
   VALs and VALUE_NODEs are written to the .DEB file only if the 
   ALLOCATED field of the constant's SYMBOL_NODE is set to false. 
   However, VAL nodes of kind SUBR_CST and ALLOC_CST are never
   written to the .DEB file. *)

  value_kind = ( scalar_cst, real_cst, string_cst, set_cst, ptr_cst, array_cst,
		 record_cst, subr_cst, no_value (* indicates no assigned value *),
		alloc_cst (* indicates a value which has been allocated in pass 4 *) );

  val_ptr = ^ value_node;

  val =
      packed record
	case kind: value_kind of
	  scalar_cst:
	    (   ival: machine_word  );		(* ordinal values of a scalar value *)
	  real_cst,
	  string_cst,
	  set_cst:
	    (  valp: intval  );		(* more complex value, described below *)
	  ptr_cst:
	    ( ); (* the only pointer constant is NIL *)
	  subr_cst:
	    (  blkp: intblk  ); (* ptr to block node of subr name *)
	  alloc_cst:
	    (  defp: intval  ) (* really a definition pointer, set in pass 4 *)
      end;

  value_node =
      packed record
	case kind: value_kind of

	  (* the following two variants each have a field declared as an array [1..1] to convince
	     the compiler to allow use of the address function on those fields  *)
	  scalar_cst:				(* for full word consts *)
	    (  scalar_val: array [1..1] of machine_word );

	  real_cst:
	    (  real_prec: prec_type;		(* apparent precision *)
	       real_val: array [1..1] of real_type  );		(* converted value *)

	  string_cst:
	    (  str_len: char_range;		(* length of string *)
	       str_varying_ref: boolean;	(* initially false, set if length field is needed *)
	       str_val: packed array[1..max_string_len] of char  );	(* text of string; must be referenced as:
							   substr (val^.str_val, 1, val^.str_len)  *)

	  set_cst:
	    (  set_origin: int_type;		(* implicit offset of 1st bit *)
	       set_len: bit_range;		(* number of elements allocated *)
	       set_val: packed array[0..max_set_offset] of boolean  )   (* set_len elements allocated *)
      end;

(* SYMBOL NODES contain information about all objects declared in the
   program or created by the compiler.  Symbol nodes of kind LABELS,
   TYPES, FOR_INDS, STD_PROCS and STD_FUNCS are not written to the
   .DEB file.  Procedures and functions have symbol nodes of
   kind CONSTS.  *)

  sym_kind = ( labels, fields, types, consts, vars, values, for_inds,
	       std_procs, std_funcs, conditions, blocks );
  fld_range = 1..2047;

  symbol_node =
      packed record
	name: intnam;				(* if user symbol, ptr to name in name table; if compiler
						   symbol, nil. *)
	block: intblk;				(* block name appears in; nil for fields *)
	next: intsym;				(* next symbol of similar kind: for fields, linked off
						   record/variant; for others: next parm, var, etc.
						   linked off block *)
	type_desc: inttyp;				(* object data type, nil for labels, standard subrs *)
	case kind: sym_kind of

	  types:  ();				(* no further information *)

	  fields:
	    (  fld_offset: bit_range;		(* offset of field from start of record *)
	       fld_width: bit_range;		(* allocated width in bits *)
	       fld_number: fld_range;		(* logical offset of field, for handling record consts *)
	       fld_record: inttyp;			(* record containing field *)
	       fld_variant: inttyp );		(* record/first variant of which field is a
						   member of the fixed part *)

	  consts, vars, values, for_inds, conditions:
	    (  dcl_class: storage_class;	(* storage class as declared, e.g. static, parm, local *)
	       public_dcl: boolean;		(* item declared as public *)
	       allocated: boolean;		(* const allocated in memory *)
	       standard: boolean;
	       maskable: boolean;
	       item_addr: unit_range;		(* offset from start of class, 
						   start address for proc of func *)
	      case boolean of (* on value of "allocated" *)
	        true: ();
	        false: (init_value: val)     );	(* initial value for static/public vars; value of consts;
						   unused in all other cases. *)

	  blocks:
	    (  ) (* used as a cross-reference code only *)
      end;

  sym_list =					(* chain of symbol nodes *)
      packed record
	first: intsym;
	last: intsym
      end;


(* TYPE NODES contain information describing user declared and built-in
   data types. *)

  type_kind = ( scalars, bools, chars, ints, reals, sets, pointers,
		files, strings, arrays, records, variants, tags,
		procs, funcs, unknown_type,
		indirect_type );

  string_kind = ( varying, nonvarying );

  type_node =
      packed record
        size: bit_range;			(* size of data item in bits *)
        djwalignment: align_range;			(* required alignment of data *)
	packable: boolean;			(* data may be minimally packed *)
	flexible: boolean;			(* type has '*' bound *)
	generic: boolean;			(* array [*] of ... (flexible is also true) *)
	case kind: type_kind of

	  bools, ints, chars, scalars:		(* basic types + subranges of enumerated types *)
	    (  base_type: inttyp;			(* type over which interval is defined, may loop *)
	       minval, maxval: machine_word;	(* ordinal value of range limits *)
	       case type_kind of		(* below only non-nil for maximal subrange *)
		 bools, scalars:
		   (  cst_list: sym_list  ) );	(* chain thru sym^.next of enumerated values,
						   sublist of blk^.id_list *)

	  reals:				(* subranges of real *)
	    (  precision: prec_type;		(* precision, in decimal digits *)
 	      rminval, rmaxval: real_type );	(* limits of range *)

	  sets:
	    (  set_element_type: inttyp  );	(* type of elements of set *)
						(* NIL for the empty set *)

	  pointers:
	    (  target_type: inttyp	);		(* type of data pointed to by target *)

	  arrays:
	    (  element_size: bit_range;		(* size of elements of the array *)
	       element_type: inttyp;		(* type of element of the array *)
	       index_type: inttyp     );		(* type of subscripts *)

	  files:
	    (  component_type: inttyp  ;);		(* type of data in file *)

	  strings:
	    (  str_kind: string_kind;		(* varying or non-varying *)
	       str_length: char_range  );	(* maximum length of varying string or complete
						   length of non-varying string *)

	  records, variants:			(* size gives max size including all inferior variants *)
	    (  field_list: intsym;			(* first field in fixed part of record or variant
						   (fixed part ends with change of fld_variant ptr);
						   for records, this is start of chain of all fields *)
	       variant_tag: inttyp;		(* variable part tag *)
	       case type_kind of
		 variants:
		   (  tag: inttyp;			(* superior tag of which this is a variant *)
		      next_variant: inttyp;	(* chain of variants for a given tag *)
		      others_var: boolean;	(* true => others variant *)
		      minlab, maxlab: int_type    );	(* case label range, min = max for non-range *)
		 records:
		   (  )  );

	  tags:					(* size is for variants with () field lists *)
	    (  tag_field: intsym;			(* field holding value of tag; if nil then 
						   this is an undiscrimated union *)
	       tag_type: inttyp;			(* data type of tag *)
	       tag_recvar: inttyp;			(* record or variant containing tag *)
	       first_variant: inttyp  );		(* chain thru typ^.next_variant of variants *)

	  procs, funcs:
	    (  return_type: inttyp;		(* type of return value, if function; else nil *)
	       nparms: parm_range;		(* number of parameters *)
	       fortran_call: boolean;	(* use fortran calling sequence *)
	       parmlist_size: unit_range  );	(* aggregate size of parameter list including descriptors *)

	  unknown_type, indirect_type:		(* forward referenced types, before/after definition *)
	    (  actual_type: inttyp	)		(* type node forward referenced; this is
						   nil, if referenced before defined. *)
      end;



(* BLOCK NODES represent levels of lexical scope and contain information about
   all symbols declared in that level. *)

  block_kind =
     (  root_blk, program_blk, module_blk, subr_blk, data_blk, handler_blk,
	class_blk, extern_blk  );

  level_index = 0 .. 63;

  (*  The BLOCK_NODE for level 0 is the first record written to
      the .DEB file.  EXTERN_BLK nodes and CLASS_BLK nodes are not
      written to the .DEB file. *)

  block_node =
      packed record
	parent: intblk;				(* containing block *)
	peer: intblk;				(* next block within same parent *)
	children: intblk;				(* chain thru blk^.peer of contained blocks *)
	level: level_index;				(* level 0 contains standard names *)
	return_sym: intsym;			(* if function, dummy symbol addressing return value *)
	parm_list: sym_list;			(* chain thru sym^.next of parameters *)
	id_list: sym_list;			(* chain thru sym^.next of all other names except fields *)
	semantic_options: set_of_options;	(* check, debug, map, symbols, dump, special,
						   and optimize permitted *)
	parm_list_base: unit_range; (* offset of parm list pointer if > 6 words *)
	case kind: block_kind of

	  root_blk: ();				(* imaginary block containing standard defs *)

	  program_blk, module_blk, data_blk:		(* level 1, main program, or just decls *)
	    (  id: intnam;				(* compilation id *)
	       start_addr: unit_range;		(* start of mainline, for program_blk only *)
	       comp_dtime: int_type);		(* day-time of compilation, placed in program block also,
						   internal day/time format of day/time package *)

	  subr_blk:				(* actual procedures and functions *)
	    (  subr_sym: intsym	);		(* procedure or function id *)

      end;
 