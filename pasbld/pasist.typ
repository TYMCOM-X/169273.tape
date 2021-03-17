$PAGE PASIST.TYP, last modified 5/11/84, zw
(* The compiler's INTERNAL SYMBOL TABLE is pointer linked structure consisting
   of the name table, the lexically order tree of block nodes, symbol nodes
   within (sic) the block, type nodes, and value (constant) nodes. *)

TYPE
nam = ^name_node;
blk = ^block_node;
sym = ^symbol_node;
typ = ^type_node;
$PAGE storage_class

(* STORAGE CLASS is an attribute of a data symbol (i.e., var, value parm, or
   constant), or, in Pass 4, of an address. *)
storage_class = ( local_sc, (* local to a block *)
parameter_sc, (* parameter to a block *)
static_sc, (* static to a single compiled module *)
constant_sc, (* constant storage of a compilation *)
external_sc, (* relative to an external name *)
(* The following storage classes are used to distinguish the dummy
	   "pseudo-symbols" which are created to represent certain information
	   for optimization. *)
dynamic_sc, (* represents the targets of a class of pointers *)
fileblk_sc, (* represents the file statuses of a class of files *)
opt_sc, (* miscellaneous optimization symbols *)
(* The following storage classes are introduced to represent relocation
	   attributes of addresses during code generation. *)
code_sc, (* within code area of a compilation *)
absolute_sc, (* constant address *)
temp_sc, (* relative to temporary area for a block *)
register_sc, (* item is a register *)
runtime_sc, (* runtime symbol *)
def_sc, (* internal label *)
self_rel_sc, (* self relocating *)
unallocated ); (* data not allocated *)
$PAGE name_node

(* The NAME TABLE contains the text of all <identifier>s seen during a
   compilation.  It is organized as an unbalanced binary tree. *)
name_node = PACKED RECORD
  alink, zlink: nam; (* branches of tree; name < current is on alink *)
  scopechain: sym; (* points to 1st symbol in scope with this name;
						     chains through sym^.scopechain *)
  visited: BOOLEAN; (* for debugger file creation walk *)
  file_loc: deb_file_index; (* relocation into debugger file *)
  TEXT: PACKED ARRAY [1..*] OF CHAR (* text of name *)
END;
$PAGE value_node

(* VALUE NODES are used to represent constant values.  In order to save storage,
   simple scalar values are stored in a VAL record instead of a pointer to a
   value node. The val record is used in all referencing structures. *)
value_kind = ( scalar_cst, real_cst, string_cst, set_cst, ptr_cst, array_cst,
  record_cst, subr_cst, no_value (* indicates no assigned value *), alloc_cst (* indicates a value which has been allocated in pass 4 *)
  );
val_ptr = ^ value_node;
val = PACKED RECORD
  CASE kind: value_kind OF
    scalar_cst: (
      ival: machine_word ); (* ordinal values of a scalar value *)
    real_cst, string_cst, set_cst, array_cst, record_cst: (
      valp: val_ptr ); (* more complex value, described below *)
    ptr_cst: (
      ); (* the only pointer constant is NIL *)
    subr_cst: (
      blkp: blk ); (* ptr to block node of subr name *)
    alloc_cst: (
      defp: val_ptr ) (* really a definition pointer, set in pass 4 *)
END;
value_node = PACKED RECORD
  def_addr: val_ptr; (* really a DEF pointer in pass 4 *)
  CASE kind: value_kind OF
    scalar_cst: (* for full word consts *)
    (
      scalar_val: machine_word );
    real_cst: (
      real_prec: prec_type; (* apparent precision *)
      real_val: real_type ); (* converted value *)
    string_cst: (
      str_varying_ref: BOOLEAN; (* initially false, set if length field is needed *)
      str_val: PACKED ARRAY [1..*] OF CHAR ); (* text of string *)
    set_cst: (
      set_origin: set_range; (* implicit offset of 1st bit *)
      set_val: PACKED ARRAY [0..*] OF BOOLEAN ); (* set element values *)
    array_cst, record_cst: (
      struc_type: typ; (* array or record type; nil until known *)
      elem_vals: ARRAY [1..*] OF val ) (* ptrs to element values *)
END;
$PAGE symbol_node

(* SYMBOL NODES contain information about all objects declared in the
   program or created by the compiler. *)
sym_kind = ( labels, fields, types, consts, vars, values, for_inds, std_procs,
  std_funcs, conditions, blocks );
fld_range = 1..2047;
symbol_node = PACKED RECORD
  name: nam; (* if user symbol, ptr to name in name table; if
						     compiler symbol, nil. *)
  block: blk; (* block name appears in; nil for fields *)
  next: sym; (* next symbol of similar kind: for fields, linked
						     off record/variant; for others: next parm,
						     var, etc. linked off block *)
  scopechain: sym; (* next symbol in scope with same name *)
  type_desc: typ; (* object data type, nil for labels, standard subrs *)
  id_number: id_range; (* among vars/labels or other symbols *)
  visited: BOOLEAN; (* for debugger file creation walk *)
  file_loc: deb_file_index; (* relocation into debugger file *)
  CASE kind: sym_kind OF
    types: (
      ); (* no further information *)
    labels: (
      lab_defined: BOOLEAN; (* implies seen in body of block *)
      lab_nonlocal_use: BOOLEAN; (* target of nonlocal goto *)
      lab_declaration: source_id ); (* source line of label declaration *)
    fields: (
      fld_number: fld_range; (* logical offset of field, for handling record consts *)
      fld_record: typ; (* record containing field *)
      fld_variant: typ; (* record/first variant of which field is a
							 member of the fixed part *)
      fld_offset: bit_range; (* offset of field from start of record *)
      fld_width: bit_range ); (* allocated width in bits *)
    consts, vars, values, for_inds, conditions: (
      dcl_class: storage_class; (* storage class as declared, e.g. static, parm, local *)
      public_dcl: BOOLEAN; (* item declared as public *)
      abnormal_use: BOOLEAN; (* item not candidate for CSE *)
      non_local_use: BOOLEAN; (* item accessed non locally *)
      parm_use: BOOLEAN; (* item used as a parameter *)
      allocated: BOOLEAN; (* item referenced in memory *)
      standard: BOOLEAN; (* language-defined condition *)
      maskable: BOOLEAN; (* may be masked (standard conditions only) *)
      item_addr: unit_range; (* offset from start of class *)
      init_value: val ); (* initial value for static/public vars; value of
						       consts; unused in all other cases. *)
    std_procs, std_funcs: (
      std_pf_code: std_pr_fun ); (* code to dispatch to p/f handler *)
    blocks: (
      ) (* used as a cross-reference code only *)
END;
sym_list = (* chain of symbol nodes *)
PACKED RECORD
  first: sym;
  last: sym
END;
(* VL nodes are used to construct a simple linked list of all the variable and
   label symbols in the program. *)
vl_link = ^ vl_node;
vl_node = PACKED RECORD
  last: vl_link;
  symbol: sym
END;
$PAGE type_node

(* TYPE NODES contain information described user declared and builtin
   data types. *)
type_kind = ( scalars, bools, chars, ints, reals, sets, pointers, files,
  strings, arrays, records, variants, tags, procs, funcs, unknown_type,
  indirect_type );
string_kind = ( varying, nonvarying );
file_modes = (* kinds of files, for OPEN and family *)
( textfile, (* TEXT *)
typedfile, (* FILE OF <some type> *)
binaryfile, (* FILE OF * *)
anyfile ); (* invalid file type; compatible with all *)
param_desc = PACKED RECORD
  parm_type: typ;
  parm_kind: sym_kind (* vars or values *)
END;
type_node = PACKED RECORD
  base_size: bit_range; (* minimal size of data item in bits *)
  type_id: sym; (* type identifier, if named type; nil otherwise *)
  visited: BOOLEAN; (* for debugger file creation walk *)
  file_loc: deb_file_index; (* relocation into debugger file *)
  packable: BOOLEAN; (* data may be minimally packed *)
  flexible: BOOLEAN; (* type has '*' bound *)
  generic: BOOLEAN; (* array [*] of ... (flexible is also true) *)
  CASE kind: type_kind OF
    bools, ints, chars, scalars: (* basic types + subranges of enumerated types *)
    (
      base_type: typ; (* type over which interval is defined, may loop *)
      minval, maxval: machine_word; (* ordinal value of range limits *)
      CASE type_kind OF (* below only non-nil for maximal subrange *)
	bools, scalars: (
	  cst_list: sym_list ) ); (* chain thru sym^.next of enumerated values,
						       sublist of blk^.id_list *)
    reals: (* subranges of real *)
    (
      precision: prec_type; (* precision, in decimal digits *)
      rminval, rmaxval: real_type ); (* limits of range *)
    sets: (
      set_element_type: typ ); (* type of elements of set *)
    pointers: (
      target_type: typ; (* type of data pointed to by target *)
      heap_class: sym ); (* represents vars accessible to this type *)
    arrays: (
      element_alignment: align_range; (* required element alignment *)
      element_type: typ; (* type of element of the array *)
      index_type: typ; (* type of subscripts *)
      element_size: bit_range ); (* size of elements of the array *)
    files: (
      file_kind: file_modes; (* text, typed, binary, any *)
      component_type: typ; (* data in file, if not binary or any *)
      file_class: sym; (* represents all files with this type *)
      comp_size: bit_range ); (* in bits *)
    strings: (
      str_kind: string_kind; (* varying or non-varying *)
      str_length: char_range ); (* maximum length of varying string or complete
						       length of non-varying string *)
    records, variants: (* size gives max size including all inferior variants *)
    (
      field_list: sym; (* first field in fixed part of record or variant
							 (fixed part ends with change of fld_variant ptr);
							 for records, this is start of chain of all fields *)
      variant_tag: typ; (* variable part tag *)
      CASE type_kind OF
	records: (
	  record_alignment: align_range ); (* minimal alignment for record *)
	variants: (
	  tag: typ; (* superior tag of which this is a variant *)
	  next_variant: typ; (* chain of variants for a given tag *)
	  others_var: BOOLEAN; (* true => others variant *)
	  minlab, maxlab: machine_word ) ); (* case label range, min = max for non-range *)
    tags: (* size is for variants with () field lists *)
    (
      tag_field: sym; (* field holding value of tag; if nil then
							 this is an undiscrimated union *)
      tag_type: typ; (* data type of tag *)
      tag_recvar: typ; (* record or variant containing tag *)
      first_variant: typ ); (* chain thru typ^.next_variant of variants *)
    procs, funcs: (
      fortran_call: BOOLEAN; (* use fortran calling sequence *)
      class_block: blk; (* subr class for subr vars/parms of this type *)
      return_type: typ; (* type of return value, if function; else nil *)
      parmlist_size: unit_range; (* aggregate size of parameter list including descriptors *)
      params: PACKED ARRAY [1..*] OF param_desc );
    unknown_type, indirect_type: (* forward referenced types, before/after definition *)
    (
      actual_type: typ; (* type node forward referenced; this is
							 nil, if referenced before defined. *)
      declaration: source_id ) (* line where type is first referred to *)
END;
$PAGE block_node

(* BLOCK NODES represent levels of lexical scope and contain information about
   all symbols declared in that level. *)
block_kind = ( root_blk, program_blk, module_blk, subr_blk, data_blk,
  class_blk, extern_blk );
call_link = ^ call_link_node;
level_index = 0 .. 63;
(*  Subroutine classes are chained as siblings of the root block, and are
      accessible through links in the subroutine type nodes.  If there is
      an <extern> block, it will be chained as a level 2 procedure (under
      the program or module), and will be accessible through the global link
      'ext_block'.  *)
block_node = PACKED RECORD
  parent: blk; (* containing block *)
  peer: blk; (* next block within same parent *)
  children: blk; (* chain thru blk^.peer of contained blocks *)
  visited: BOOLEAN; (* for debugger file creation walk *)
  file_loc: deb_file_index; (* relocation into debugger file *)
  number: index_range; (* unique block number *)
  level: level_index; (* level 0 contains standard names *)
  apparent_level: level_index; (* level accounting for quick blocks *)
  max_call_level: level_index; (* deepest apparent_level from which block is called *)
  hndlr_depth: level_index; (* max nesting of handled compound statements *)
  recursive: BOOLEAN; (* subroutine can call itself *)
  class_bound: BOOLEAN; (* subroutine is bound to a class *)
  return_sym: sym; (* if function, dummy symbol addressing return value *)
  parm_list: sym_list; (* chain thru sym^.next of parameters *)
  label_list: sym_list; (* chain thru sym^.next of labels within this block *)
  type_list: sym_list; (* chain thru sym^.next of all type ids *)
  id_list: sym_list; (* chain thru sym^.next of all other names except fields *)
  semantic_options: set_of_options; (* check, debug, map, symbols, dump, special,
						     and optimize permitted *)
  dump_switches: switch_ptr; (* from dump ( ) *)
  owner: blk; (* block containing this blk's frame *)
  calls: call_link; (* chain thru call_link^.next_call of blocks called *)
  downward_call_thread: blk; (* DFST order threads for call tree *)
  upward_call_thread: blk;
  lex_thread: blk; (* block ordering by decreasing level numbers *)
  pos_local_size: unit_range; (* size of pos offset local storage *)
  neg_local_size: unit_range; (* size of neg offset local storage *)
  pos_stk_begin: unit_range; (* owner frame offset for pos storage *)
  neg_stk_begin: unit_range; (* owner frame offset for neg storage *)
  pos_stk_end: unit_range; (* owner frame last word + 1 for pos storage *)
  neg_stk_end: unit_range; (* owner frame last word + 1 for neg storage *)
  parm_list_base: unit_range; (* offset of parm list pointer if > 6 words *)
  CASE kind: block_kind OF
    root_blk: (
      ); (* imaginary block containing standard defs *)
    program_blk, module_blk, data_blk: (* level 1, main program, or just decls *)
    (
      id: nam; (* compilation id *)
      start_addr: unit_range; (* start of mainline, for program_blk only *)
      comp_dtime: dtime_int ); (* day/time of compilation *)
    subr_blk: (* actual procedures and functions *)
    (
      subr_sym: sym; (* procedure or function id *)
      forward_dcl: BOOLEAN; (* indicates body not yet given *)
      declaration: source_id ); (* location of the declaration *)
    class_blk: (* block representing a subr var/parm type *)
    (
      class_type: typ ); (* the subr type of the class *)
    extern_blk: (
      ) (* block represents all external procedures *)
END;
(*  The blocks which are called from a given block are represented by a
    right-threaded binary tree structure (see Knuth, volume 1, pp 319-325).
    The ordering on the tree is:  p precedes q if level(p) > level(q), or
    if level(p) = level(q) and number(p) < number(q).  *)
call_link_node = PACKED RECORD
  called_subr: blk; (* block node for a called subr *)
  r_terminal: BOOLEAN; (* true if rlink is a thread; false for a link *)
  llink, rlink: call_link (* the tree structure links *)
END;
   