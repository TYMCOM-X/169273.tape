external var
  code_area: code_list;			(* chain of code records for code area *)
  static_area: code_list;		(* chain for static area image *)
  cst_area: code_list;			(* chain for constant pool area *)
  fp_area: code_list;			(* chain of page and file blocks for debugger *)
  bptr_area: code_list;			(* chain for byte pointers *)
  blt_area: code_list;			(* chain for BLT control words *)

  loc_code: code_address;			(* location counter for code area *)
  loc_static: code_address;		(* location counter for static area *)
  loc_cst: code_address;			(* location counter for constant area *)
  def_lists: array [def_types] of def;		(* start of chains of internal definitions *)

type rts_name_array = array[rt_init..rt_last_used] of packed array[1..6] of char;
external var
  rts_name: rts_name_array;
  rw_request: array [rt_int_read..rt_bool_write] of boolean;

external procedure gen_init;
external procedure gen_term;
external function make_def (dtype: def_types): def;
external function get_def (dtype: def_types; defid: id_range): def;
external function get_extern (name: pdp10word): def;
external procedure def_value (d: def; value: code_address; relocatable: boolean);
external procedure del_def_list (dtype: def_types);
external function get_offset (intdef: def; off: int_type): def;
external function relrt (rtsym: rt_symbol): rel_syllable;
external function reldef (intdef: def): rel_syllable;
external procedure gen_emit (var area_list: code_list; cr: code);
external procedure gen_origin (var arealist: code_list; loc: unit_range);
external procedure mark_def (var area_list: code_list; intdef: def);
external procedure gen_source (srcid: source_id);
external procedure gen
  ( opc: opc_range; acc, index: registers; offset: int_type; rel: rel_syllable );
external procedure genind
  ( opc: opc_range; acc, index: registers; offset: int_type; rel: rel_syllable );
external procedure gen_lab (opc: opc_range; acc: registers; lab: tuple);
external procedure gen_rt (opc: opc_range; acc: registers; rtsym: rt_symbol);
external procedure gen_rl (opc: opc_range; acc: registers; lab: tuple);
external procedure gen_rr (opc: opc_range; acc: registers; operand: registers);
external procedure gen_rx (opc: opc_range; acc: registers; operand: registers);
external procedure gen_ri (opc: opc_range; acc: registers; operand: int_type);
external procedure gen_rm (opc: opc_range; acc: registers; operand: addr_desc);
external function gen_bptr (mem: addr_desc): rel_syllable;
external function gen_blt (source_offset: code_address; source_reloc: rel_syllable;
			   dest_offset: code_address; dest_reloc: rel_syllable): rel_syllable;
external procedure gen_xwd
  ( lhv: int_type; lhrel: rel_syllable; rhv: int_type; rhrel: rel_syllable );
external procedure gen_string (str: string);
external procedure gen_word (var area_list: code_list; word: pdp10word; wkind: code_types);
external procedure gen_val (var area_list: code_list; value: val);
external procedure gen_node (var area_list: code_list; value: val_ptr);
external function gen_cval (value: val): rel_syllable;
external function gen_cnode (value: val_ptr): rel_syllable;
external function gen_cword (cons_word: pdp10word; cons_code: code_types): rel_syllable;
external function gen_cint (int_cons: int_type): rel_syllable;
external procedure wr_code (var area_list: code_list; var ic: unit_range);
external function halfword (i: int_type): unit_range;
external function word_size (exp: expr): unit_range;
external procedure gen_cmt (var area_list: code_list; text: string);

type
    block_vector = array [index_range] of blk;

external var
    blk_list: ^ block_vector;

external function gen_cst (int_type): addr_desc;
