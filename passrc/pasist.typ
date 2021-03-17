$PAGE PASIST.TYP, last modified 3/27/84, zw
$IFNOT pasisttyp

(*SYSTEM UTLTYP.TYP*)
(*SYSTEM UTLSW.TYP*)
(*SYSTEM UTLSRC.TYP*)
(*SYSTEM PASCAL.TYP*)
(*SYSTEM DTIME.TYP*)

(*The compiler's INTERNAL SYMBOL TABLE is pointer linked structure consisting
  of the name table, the lexically order tree of block nodes, symbol nodes
  within (sic) the block, type nodes, and value (constant) nodes.*)

TYPE
name_pointer = ^name_node;
symbol_pointer = ^symbol_node;
value_pointer = ^value_node;
type_pointer = ^type_node;
block_pointer = ^block_node;
block_vector = ARRAY[0 .. *] OF blk;
symbol_list = PACKED RECORD (*chain of symbol nodes*)
  first: symbol_pointer;
  last: symbol_pointer
END;
(*VL nodes are used to construct a simple linked list of all the variable
and label symbols in the program.*)
vl_link = ^vl_node;
vl_node = PACKED RECORD
  last: vl_link;
  symbol: symbol_pointer
END;


$PAGE storage_class
(*The storage class is an attribute of a data symbol (i.e., var, value parm,
  or constant), or, in Pass 4, of an address.*)
storage_class = ( local_sc, (*local to a block*)
  parameter_sc, (*parameter to a block*)
  static_sc, (*static to a single compiled module*)
  constant_sc, (*constant storage of a compilation*)
  external_sc, (*relative to an external name*)
  (*The following storage classes are used to distinguish the dummy
    "pseudo-symbols" which are created to represent certain information
    for optimization.*)
  dynamic_sc, (*represents the targets of a class of pointers*)
  fileblk_sc, (*represents the file statuses of a class of files*)
  opt_sc, (*miscellaneous optimization symbols*)
  (*The following storage classes are introduced to represent relocation
    attributes of addresses during code generation.*)
  code_sc, (*within code area of a compilation*)
  absolute_sc, (*constant address*)
  temp_sc, (*relative to temporary area for a block*)
  register_sc, (*item is a register*)
  runtime_sc, (*runtime symbol*)
  def_sc, (*internal label*)
  self_rel_sc, (*self relocating*)
  unallocated ); (*data not allocated*)

$PAGE name_node
(*The name table contains the text of all <identifier>s seen during a
  compilation.  It is organized as an unbalanced binary tree.*)
name_node = PACKED RECORD
  alink, zlink: name_pointer; (*branches of tree; name < current is on alink*)
  scopechain: symbol_pointer; (*points to 1st symbol in scope with this
    name; chains through sym^.scopechain*)
  visited: yn; (*for debugger file creation walk*)
  file_loc: debug_index; (*relocation into debugger file*)
  text: PACKED ARRAY [1..*] OF CHAR (*text of name*)
END;

$PAGE value_node
(*Value nodes are used to represent constant values.  In order to save
  storage, simple scalar values are stored in a simple_value record instead
  of a pointer to a value node. The simple_value record is used in all 
  referencing structures.*)
value_kind = (scalar_cst, real_cst, string_cst, set_cst, ptr_cst,
  integer_cst, array_cst, record_cst, subr_cst, machine_cst,
  no_value, (*indicates no assigned val*)
  alloc_cst); (*indicates a value which has been allocated in pass 4*)
simple_value = PACKED RECORD
  CASE kind: value_kind OF
    scalar_cst: (sval: positive_integer);
    integer_cst: (ival: integer);
    machine_cst, real_cst, string_cst, set_cst, array_cst, record_cst:
      (complex: value_pointer);
    ptr_cst: (); (*The only pointer constant is NIL.*)
    subr_cst: (blkp: block_pointer); (*The block node of the subroutine name.*)
    alloc_cst: (defp: value_pointer) (*This is a definition set in pass 4.*)
END;
value_node = PACKED RECORD
  def_addr: value_pointer; (*This is a definition pointer in pass 4.*)
  CASE kind: value_kind OF
    scalar_cst: (scalar_value: positive_integer);
    integer_cst: (integer_value: integer);
    machine_cst: (machine_value: PACKED ARRAY [1 .. *] OF 0 .. 1);
    real_cst: (
      real_precision: precision_type;
      real_value: real
    string_cst: (
      length_needed: yn;
      string_value: PACKED ARRAY [1 .. *] OF CHAR);
    set_cst: (
      set_offset: set_range;
      set_value: PACKED ARRAY [0 .. *] OF yn);
    array_cst, record_cst: (
      structure: type_pointer; (*array or record type; nil until known*)
      element_values: ARRAY [1 .. *] OF simple_value)
END;

$PAGE symbol_node
(*Symbol nodes contain information about all objects declared in the
  program or created by the compiler.*)
symbol_kind = (labels, fields, types, consts, vars, values, for_inds,
  std_procs, std_funcs, conditions, blocks);
fld_range = 1..2047;
symbol_node = PACKED RECORD
  name: name_pointer; (*Actual name in symbol table or NIL.*)
  block: block_pointer; (*The block the name appears in or NIL for fields.*)
  next_symbol: sym; (*next symbol of similar kind: for fields, linked
    off record/variant; for others: next parm, var, etc. linked off block*)
  next_scope: symbol_pointer; (*The next symbol in this scope with same name.*)
  data_type: type_pointer; (*object data type, nil for labels, standard subrs*)
  number: identifier_range; (*among vars/labels or other symbols*)
  visited: yn; (*for debugger file creation walk*)
  file_loc: debug_index; (*relocation into debugger file*)
  CASE kind: symbol_kind OF
    types: (); (*no further information*)
    labels: (
      lab_defined: yn; (*implies seen in body of block*)
      lab_nonlocal_use: yn; (*target of nonlocal goto*)
      lab_declaration: source_position ); (*position of label declaration*)
    fields: (
      field_number: field_range; (*logical offset of field, for record consts*)
      field_record: typ; (*record containing field*)
      field_variant: typ; (*record/first variant of which field is a
        member of the fixed part*)
      field_offset: bit_range; (*offset of field from start of record*)
      field_width: bit_range ); (*allocated width in bits*)
    consts, vars, values, for_inds, conditions: (
      dcl_class: storage_class; (*storage class as declared, e.g. static,
        parm, local*)
      public_dcl: yn; (*item declared as public*)
      abnormal_use: yn; (*item not candidate for CSE*)
      non_local_use: yn; (*item accessed non locally*)
      parm_use: yn; (*item used as a parameter*)
      allocated: yn; (*item referenced in memory*)
      standard: yn; (*language-defined condition*)
      maskable: yn; (*may be masked (standard conditions only)*)
      item_addr: unit_range; (*offset from start of class*)
      init_value: val ); (*initial value for static/public vars; value of
        consts; unused in all other cases.*)
    std_procs, std_funcs: (
      std_pf_code: std_pr_fun ); (*code to dispatch to p/f handler*)
    blocks: () (*used as a cross-reference code only*)
END;

$PAGE type_nodes
(*Type nodes contain information describing user and builtin data types.*)
type_kind = (scalars, bools, chars, ints, reals, sets, pointers, files,
  strings, arrays, records, variants, tags, procs, funcs, unknown_type,
  indirect_type);
string_kind = (varying, nonvarying);
file_modes = (*kinds of files, for OPEN, etc.*)
  (textfile, (*TEXT*)
   typedfile, (*FILE OF <some type>*)
   binaryfile, (*FILE OF **)
   anyfile ); (*invalid file type; compatible with all*)
param_desc = PACKED RECORD
  parm_type: type_pointer;
  parm_kind: symbol_kind (*vars or values*)
END;
type_node = PACKED RECORD
  base_size: bit_range; (*minimal size of data item in bits*)
  type_name: symbol_pointer; (*type identifier, if named type; nil otherwise*)
  visited: yn; (*for debugger file creation walk*)
  file_loc: debug_index; (*relocation into debugger file*)
  packable: yn; (*data may be minimally packed*)
  flexible: yn; (*type has '*' bound*)
  generic: yn; (*array [*] of ... (flexible is also true)*)
  CASE kind: type_kind OF
    bools, ints, chars, scalars: ( (*subranges of enumerated types*)
      base_type: type_pointer; (*over which interval is defined, may loop*)
      minval, maxval: simple_value; (*ordinal value of range limits*)
      CASE type_kind OF (*below only non-nil for maximal subrange*)
        bools, scalars: (cst_list: symbol_list)); (*chain thru sym^.next of
	  enumerated values, sublist of block^.id_list*)
    reals: ( (*subranges of real*)
      precision: precision_type; (*precision, in decimal digits*)
      rminval, rmaxval: real); (*limits of range*)
    sets: (set_element_type: type_pointer); (*type of elements of set*)
    pointers: (
      target_type: type_pointer; (*type of data pointed to by target*)
      heap_class: symbol_pointer); (*represents vars accessible to this type*)
    arrays: (
      element_alignment: alignment_range; (*required element alignment*)
      element_type: type_pointer; (*type of element of the array*)
      index_type: type_pointer; (*type of subscripts*)
      element_size: bit_range); (*size of elements of the array*)
    files: (
      file_kind: file_modes; (*text, typed, binary, any*)
      component_type: type_pointer; (*data in file, if not binary or any*)
      file_class: symbol_pointer; (*represents all files with this type*)
      component_size: bit_range ); (*in bits*)
    strings: (
      str_kind: string_kind; (*varying or non-varying*)
      str_length: character_range ); (*maximum length of varying string or
        complete length of non-varying string*)
    records, variants: ( (*size is max size including all inferior variants*)
      field_list: symbol_pointer; (*first field in fixed part of record or
        variant (fixed part ends with change of field_variant ptr); for
	records, this is start of chain of all fields*)
      variant_tag: type_pointer; (*variable part tag*)
      CASE type_kind OF
        records: (record_alignment: alignment_range ); (*minimal alignment*)
        variants: (
          tag: type_pointer; (*superior tag of which this is a variant*)
          next_variant: type_pointer; (*chain of variants for a given tag*)
          others_var: yn; (*true => others variant*)
          minlab, maxlab: integer)); (*case label range, min = max
            for non-range*)
    tags: ( (*size is for variants with () field lists*)
      tag_field: symbol_pointer; (*field holding value of tag; if nil then
        this is an undiscrimated union*)
      tag_type: type_pointer; (*data type of tag*)
      tag_recvar: type_pointer; (*record or variant containing tag*)
      first_variant: type_pointer); (*chain typ^.next_variant of variants*)
    procs, funcs: (
      fortran_call: yn; (*use fortran calling sequence*)
      class_block: block_pointer; (*subr class for subr vars/parms of type*)
      return_type: type_pointer; (*type of return value if function else nil*)
      parmlist_size: unit_range; (*aggregate size of parameter list
        including descriptors*)
      params: PACKED ARRAY [1 .. *] OF param_desc );
    unknown_type, indirect_type: ( (*forward referenced types, before/after
	definition*)
      actual_type: type_pointer; (*type node forward referenced; this is
        nil, if referenced before defined.*)
    declaration: source_position) (*line where type first referred to*)
END;

$PAGE block nodes
(*Block nodes represent levels of lexical scope and contain information
  about all symbols declared in that level.*)
block_kind = ( root_blk, program_blk, module_blk, subr_blk, data_blk,
  class_blk, extern_blk );
call_link = ^ call_link_node;
level_index = 0 .. 63;
(*Subroutine classes are chained as siblings of the root block, and are
  accessible through links in the subroutine type nodes.  If there is
  an <extern> block, it will be chained as a level 2 procedure (under
  the program or module), and will be accessible through the global link
  'ext_block'.*)
block_node = PACKED RECORD
  parent: block_pointer; (*containing block*)
  peer: block_pointer; (*next block within same parent*)
  children: block_pointer; (*chain thru blk^.peer of contained blocks*)
  visited: yn; (*for debugger file creation walk*)
  file_loc: debug_index; (*relocation into debugger file*)
  number: index_range; (*unique block number*)
  level: level_index; (*level 0 contains standard names*)
  apparent_level: level_index; (*level accounting for quick blocks*)
  max_call_level: level_index; (*deepest apparent_level from which block
    is called*)
  hndlr_depth: level_index; (*max nesting of handled compound statements*)
  recursive: yn; (*subroutine can call itself*)
  class_bound: yn; (*subroutine is bound to a class*)
  return_sym: symbol_pointer; (*if function, dummy symbol addr return value*)
  parm_list: symbol_list; (*chain thru sym^.next of parameters*)
  label_list: symbol_list; (*chain thru sym^.next of labels within this
    block*)
  type_list: symbol_list; (*chain thru sym^.next of all type ids*)
  id_list: symbol_list; (*chain thru sym^.next of all other names except
    fields*)
  semantic_options: set_of_options; (*check, debug, map, symbols, dump,
    special, and optimize permitted*)
  dump_switches: switch_ptr; (*from dump ( )*)
  owner: block_pointer; (*block containing this blk's frame*)
  calls: call_link; (*chain thru call_link^.next_call of blocks called*)
  downward_call_thread: block_pointer; (*DFST order threads for call tree*)
  upward_call_thread: block_pointer
  lex_thread: block_pointer; (*block ordering by decreasing level numbers*)
  pos_local_size: unit_range; (*size of pos offset local storage*)
  neg_local_size: unit_range; (*size of neg offset local storage*)
  pos_stk_begin: unit_range; (*owner frame offset for pos storage*)
  neg_stk_begin: unit_range; (*owner frame offset for neg storage*)
  pos_stk_end: unit_range; (*owner frame last word + 1 for pos storage*)
  neg_stk_end: unit_range; (*owner frame last word + 1 for neg storage*)
  parm_list_base: unit_range; (*offset of parm list pointer if >6 words*)
  CASE kind: block_kind OF
    root_blk: (); (*imaginary block containing standard defs*)
    program_blk, module_blk, data_blk: ( (*level 1 main program or just decls*)
      id: name_pointer; (*compilation id*)
      start_addr: unit_range; (*start of mainline, for program_blk only*)
      comp_dtime: dtime_int ); (*day/time of compilation*)
    subr_blk: ( (*actual procedures and functions*)
      subr_sym: symbol_pointer; (*procedure or function id*)
      forward_dcl: yn; (*indicates body not yet given*)
      declaration: source_position); (*location of the declaration*)
    class_blk: ( (*block representing a subr var/parm type*)
      class_type: type_pointer ); (*the subr type of the class*)
    extern_blk: () (*block represents all external procedures*)
END;
(*The blocks which are called from a given block are represented by a
  right-threaded binary tree structure (see Knuth, volume 1, pp 319-325).
  The ordering on the tree is:  p precedes q if level(p) > level(q), or
  if level(p) = level(q) and number(p) < number(q).*)
call_link_node = PACKED RECORD
  called_subr: block_pointer; (*block node for a called subr*)
  r_terminal: yn; (*true if rlink is a thread; false for a link*)
  llink, rlink: call_link (*the tree structure links*)
END;

$ENABLE pasisttyp
$ENDIF
    