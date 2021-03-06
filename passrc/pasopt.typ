$PAGE PASOPT.TYP, last modified 1/13/84, zw
$IFNOT pasopttyp

(*SYSTEM PASADR.TYP*)
(*SYSTEM PASIST.TYP*)
(*SYSTEM PASIF.TYP*)

(*A Formal Reference Expression, or FRE, represents a class of reference
  expressions in the program.  A particular FRE may represent either a
  variable symbol (identifier, heap class or file class), an arbitrary
  array element or substring of another FRE, or a specified field of
  another FRE.
  When Reif's algorithms are applied, FREs are used instead of simple
  variables.  This provides a way of treating structured variables.  *)

TYPE (*structures for the intermediate form optimizer*)
tpl_block = PACKED ARRAY [0 .. *] OF tuple;
tpl_vector = ^tpl_block; (*dynamically created tuple vectors*)
index_block = PACKED ARRAY [0 .. *] OF index_range;
index_vector = ^index_block; (*dynamically created block index vectors*)
(*fre = id_range;*)     (*FREs are referenced by their id numbers.
     This type definition is in PASIF.TYP*)
fre_kind = (sym_fre, elem_fre, field_fre );
fre_node = PACKED RECORD
  CASE kind: fre_kind OF
    sym_fre: (id_sym: sym); (*reference symbol*)
    elem_fre: (base_fre: fre); (*FRE number of the base array or string*)
    field_fre: (record_fre: fre; (*FRE number of the base record*)
      field_sym: sym) (*referenced field in the record*)
END;
fre_block = ARRAY [0 .. *] OF fre_node;
fre_vector = ^fre_block;
tpl_list = ^tpl_list_node;
tpl_list_node = PACKED RECORD tpl: tuple; next: tpl_list END;
tpl_list_blk = PACKED ARRAY [0 .. *] OF tpl_list;
tpl_list_vec = ^tpl_list_blk;
ref_c_tree = ^ref_c_node;
ref_c_node = PACKED RECORD
  left, right: ref_c_tree;
  r_terminal: BOOLEAN;
  ref_count: index_range;
  tpl: tuple
END;
ref_c_block = ARRAY [index_range] OF ref_c_tree;
ref_c_vector = ^ref_c_block;

$ENABLE pasopttyp
$ENDIF
  