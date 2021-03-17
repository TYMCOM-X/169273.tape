(* The debugger's SYMBOL TABLE is a pointer linked structure consisting
   of the name table, the lexically ordered tree of block nodes, symbol nodes
   within (sic) the block, type nodes, and value (constant) nodes. 

   Note: Files PASCAL.INC and PASADR.TYP must be included before this
   include file.  *)


(* The NAME TABLE contains the text of all <identifier>s seen during a
   compilation.  It is organized as an unbalanced binary tree. 
   NAME_NODEs for labels and named types are not written to the
   .DEB file.  *)

type

  deb_name_node =
      packed record
	alink, zlink: nam;			(* branches of tree; name < current is on alink *)
	scopechain: sym;			(* points to 1st symbol in scope with this name;
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


  deb_value_node =
      packed record
	case kind: value_kind of

	  scalar_cst:				(* for full word consts *)
	    (  scalar_val: int_type );

	  real_cst:
	    (  real_prec: prec_type;		(* apparent precision *)
	       real_val: real_type  );		(* converted value *)

	  string_cst:
	    (  str_len: char_range;		(* length of string *)
	       str_varying_ref: boolean;	(* initially false, set if length field is needed *)
	       str_val: packed array[1..1] of char  );	(* text of string; must be referenced as:
							   substr (val^.str_val, 1, val^.str_len)  *)

	  set_cst:
	    (  set_origin: int_type;		(* implicit offset of 1st bit *)
	       set_len: bit_range;		(* number of elements allocated *)
	       set_val: packed array[0..0] of boolean  );   (* set_len elements allocated *)

	  array_cst, record_cst:
	    (  struc_type: typ;			(* array or record type; nil until known *)
	       n_elems: unit_range;		(* number of elements in array, fields in record *)
	       elem_vals: array[1..1] of val  )	(* ptrs to element values, only n_elems allocated *)
      end;

(* SYMBOL NODES contain information about all objects declared in the
   program or created by the compiler.  Symbol nodes of kind LABELS,
   TYPES, FOR_INDS, STD_PROCS and STD_FUNCS are not written to the
   .DEB file.  Procedures and functions have symbol nodes of
   kind CONSTS.  *)


  deb_symbol_node =
      packed record
	name: nam;				(* if user symbol, ptr to name in name table; if compiler
						   symbol, nil. *)
	block: blk;				(* block name appears in; nil for fields *)
	next: sym;				(* next symbol of similar kind: for fields, linked off
						   record/variant; for others: next parm, var, etc.
						   linked off block *)
	type_desc: typ;				(* object data type, nil for labels, standard subrs *)
	case kind: sym_kind of

	  types:  ();				(* no further information *)

	  fields:
	    (  fld_offset: bit_range;		(* offset of field from start of record *)
	       fld_width: bit_range;		(* allocated width in bits *)
	       fld_number: fld_range;	(* logical offset of field, for handling record consts *)
	       fld_record: typ;			(* record containing field *)
	       fld_variant: typ  );		(* record/first variant of which field is a
						   member of the fixed part *)

	  consts, vars, values, for_inds, conditions:
	    (  dcl_class: storage_class;	(* storage class as declared, e.g. static, parm, local *)
	       public_dcl: boolean;		(* item declared as public *)
	       allocated: boolean;		(* const allocated in memory *)
	       standard: boolean; (* language-defined condition *)
	       maskable: boolean; (* may be masked (standard conditions only) *)
	       item_addr: unit_range;		(* offset from start of class, 
						   start address for proc of func *)
	       case boolean of (* on value of "allocated" *)
		 true: ();
		 false: (
		   init_value: val)	          );	(* initial value for static/public vars; value of consts;
						   unused in all other cases. *)

	  blocks:
	    (  ) (* used as a cross-reference code only *)
      end;

(* TYPE NODES contain information described user declared and builtin
   data types. *)

  deb_type_node =		(* version of type_node for debugger *)
      packed record
        size: bit_range;			(* size of data item in bits *)
        alignment: align_range;			(* required alignment of data *)
	packable: boolean;			(* data may be minimally packed *)
	flexible: boolean;			(* type has '*' bound *)
	generic: boolean;			(* array [*] of ... (flexible is also true) *)
	case kind: type_kind of

	  bools, ints, chars, scalars:		(* basic types + subranges of enumerated types *)
	    (  base_type: typ;			(* type over which interval is defined, may loop *)
	       minval, maxval: int_type;	(* ordinal value of range limits *)
	       case type_kind of		(* below only non-nil for maximal subrange *)
		 bools, scalars:
		   (  cst_list: sym_list  ) );	(* chain thru sym^.next of enumerated values,
						   sublist of blk^.id_list *)

	  reals:				(* subranges of real *)
	    (  precision: prec_type;	(* precision, in decimal digits *)
	       rminval, rmaxval: real_type  );	(* limits of range *)

	  sets:
	    (  set_element_type: typ  );	(* type of elements of set *)

	  pointers:
	    (  target_type: typ	);		(* type of data pointed to by target *)

	  arrays:
	    (  element_size: bit_range;		(* size of elements of the array *)
	       element_type: typ;		(* type of element of the array *)
	       index_type: typ     );		(* type of subscripts *)

	  files:
	    (  component_type: typ  );		(* type of data in file *)

	  strings:
	    (  str_kind: string_kind;		(* varying or non-varying *)
	       str_length: char_range  );	(* maximum length of varying string or complete
						   length of non-varying string *)

	  records, variants:			(* size gives max size including all inferior variants *)
	    (  field_list: sym;			(* first field in fixed part of record or variant
						   (fixed part ends with change of fld_variant ptr);
						   for records, this is start of chain of all fields *)
	       variant_tag: typ;		(* variable part tag *)
	       case type_kind of
		 variants:
		   (  tag: typ;			(* superior tag of which this is a variant *)
		      next_variant: typ;	(* chain of variants for a given tag *)
		      others_var: boolean;	(* true => others variant *)
		      minlab, maxlab: int_type  )  );	(* case label range, min = max for non-range *)

	  tags:					(* size is for variants with () field lists *)
	    (  tag_field: sym;			(* field holding value of tag; if nil then 
						   this is an undiscrimated union *)
	       tag_type: typ;			(* data type of tag *)
	       tag_recvar: typ;			(* record or variant containing tag *)
	       first_variant: typ  );		(* chain thru typ^.next_variant of variants *)

	  procs, funcs:
	    (  return_type: typ;		(* type of return value, if function; else nil *)
	       nparms: parm_range;		(* number of parameters *)
	       fortran_call: boolean;	(* use fortran calling sequence *)
	       parmlist_size: unit_range  )	(* aggregate size of parameter list including descriptors *)

      end;



  (*  The BLOCK_NODE for level 0 is the first record written to
      the .DEB file.  EXTERN_BLK nodes and CLASS_BLK nodes are not
      written to the .DEB file. *)

  deb_block_node =		(* version of block_node for debugger *)
      packed record
	parent: blk;				(* containing block *)
	peer: blk;				(* next block within same parent *)
	children: blk;				(* chain thru blk^.peer of contained blocks *)
	level: level_index;				(* level 0 contains standard names *)
	return_sym: sym;			(* if function, dummy symbol addressing return value *)
	parm_list: sym_list;			(* chain thru sym^.next of parameters *)
	id_list: sym_list;			(* chain thru sym^.next of all other names except fields *)
	semantic_options: set_of_options;	(* check, debug, map, symbols, dump, special,
						   and optimize permitted *)
	parm_list_base: unit_range; (* offset of parm list pointer if > 6 words *)
	case kind: block_kind of

	  root_blk: ();				(* imaginary block containing standard defs *)

	  program_blk, module_blk, data_blk:		(* level 1, main program, or just decls *)
	    (  id: nam;				(* compilation id *)
	       start_addr: unit_range;		(* start of mainline, for program_blk only *)
	       comp_dtime: dtime_int);		(* time of compilation, placed in program block also,
						   internal day/time format of day/time package *)

	  subr_blk:				(* actual procedures and functions *)
	    (  subr_sym: sym	)		(* procedure or function id *)
      end;

