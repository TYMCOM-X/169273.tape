type
    tpl_block = packed array [0..*] of tuple;

    tpl_vector = ^ tpl_block; (* Dynamically created tuple vectors. *)

    index_block = packed array [0..*] of index_range;

    index_vector = ^ index_block; (* Dynamically created block index vectors. *)


(*  A Formal Reference Expression, or FRE, represents a class of reference
    expressions in the program.  A particular FRE may represent either a
    variable symbol (identifier, heap class or file class), an arbitrary
    array element or substring of another FRE, or a specified field of
    another FRE.

    When Reif's algorithms are applied, FREs are used instead of simple
    variables.  This provides a way of treating structured variables.  *)


type

(*  fre = id_range; *)				(* FREs are referenced by their id numbers.
						   This type definition is in PASIF.TYP.  *)

    fre_kind = (sym_fre, elem_fre, field_fre );

    fre_node = packed record
	case kind: fre_kind of
	  sym_fre:
	    ( id_sym: sym ); (* The reference symbol. *)
	  elem_fre:
	    ( base_fre: fre ); (* The FRE number of the base array or string. *)
	  field_fre:
	    ( record_fre: fre; (* The FRE number of the base record. *)
	      field_sym: sym ) (* The referenced field in the record. *)
    end (* fre_node *);

    fre_block = array [0..*] of fre_node;

    fre_vector = ^ fre_block;


type
    tpl_list = ^ tpl_list_node;

    tpl_list_node = packed record
	tpl: tuple;
	next: tpl_list
    end;

    tpl_list_blk = packed array [0..*] of tpl_list;

    tpl_list_vec = ^ tpl_list_blk;


type
    ref_c_tree = ^ ref_c_node;

    ref_c_node = packed record
	left, right: ref_c_tree;
	r_terminal: boolean;
	ref_count: index_range;
	tpl: tuple
    end;

    ref_c_block = array [index_range] of ref_c_tree;

    ref_c_vector = ^ ref_c_block;
