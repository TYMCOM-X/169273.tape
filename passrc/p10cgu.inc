$PAGE P10CGU.INC, last modified 1/16/84, zw
$IFNOT p10cguinc

$SYSTEM PASCAL.TYP
$SYSTEM PASIST.TYP
$SYSTEM P10CGU.TYP

EXTERNAL VAR
code_area: code_list;
static_area: code_list;
cst_area: code_list;
hbt_area: code_list;
fp_area: code_list;
bptr_area: code_list;
blt_area: code_list;
loc_code: code_address;
loc_static: code_address;
loc_cst: code_address;
loc_hbt: code_address;
def_lists: ARRAY[def_types] OF def;
rts_name: rts_name_array;
rw_request: ARRAY[rt_int_read .. rt_bool_write] OF BOOLEAN;
blk_list: ^ block_vector;

EXTERNAL PROCEDURE gen_init;
EXTERNAL PROCEDURE gen_term;
EXTERNAL FUNCTION word_size(expr): unit_range;
EXTERNAL FUNCTION make_def(def_types): def;
EXTERNAL FUNCTION get_def(def_types; id_range): def;
EXTERNAL FUNCTION get_extern(pdp10word): def;
EXTERNAL PROCEDURE def_value(def; code_address; BOOLEAN);
EXTERNAL PROCEDURE del_def_list(def_types);
EXTERNAL FUNCTION get_offset(def; code_offset): def;
EXTERNAL FUNCTION relrt(rt_symbol): rel_syllable;
EXTERNAL FUNCTION reldef(def): rel_syllable;
EXTERNAL PROCEDURE gen_emit(VAR code_list; code);
EXTERNAL PROCEDURE gen_origin(VAR code_list; unit_range);
EXTERNAL PROCEDURE set_origin(VAR code_list; unit_range);
EXTERNAL PROCEDURE mark_def(VAR code_list; def);
EXTERNAL PROCEDURE def_temp(val_desc);
EXTERNAL PROCEDURE gen_source(source_id; source_index);
EXTERNAL PROCEDURE gen_cmt(VAR code_list; STRING[*]);
EXTERNAL PROCEDURE gen_asm_label(VAR code_list; STRING[6]);
EXTERNAL FUNCTION halfword(INTEGER): code_address;
EXTERNAL PROCEDURE gen
 (opc_range; registers; registers; INTEGER; rel_syllable);
EXTERNAL PROCEDURE genind
 (opc_range; registers; registers; INTEGER; rel_syllable);
EXTERNAL PROCEDURE gen_rr(opc_range; registers; registers);
EXTERNAL PROCEDURE gen_rx(opc_range; registers; registers);
EXTERNAL PROCEDURE gen_ri(opc_range; registers; INTEGER);
EXTERNAL PROCEDURE gen_rm(opc_range; registers; addr_desc);
EXTERNAL PROCEDURE gen_rt(opc_range; registers; rt_symbol);
EXTERNAL PROCEDURE gen_rl(opc_range; registers; tuple);
EXTERNAL PROCEDURE gen_word(VAR code_list; pdp10word; code_types);
EXTERNAL PROCEDURE gen_string(STRING[*]);
EXTERNAL FUNCTION addr_equal(addr_desc; addr_desc): BOOLEAN;
EXTERNAL FUNCTION gen_blt
 (code_address; rel_syllable; code_address; rel_syllable): rel_syllable;
EXTERNAL FUNCTION gen_bptr(addr_desc): rel_syllable;
EXTERNAL PROCEDURE btmp_offsets;
EXTERNAL PROCEDURE gen_xwd(INTEGER; rel_syllable; INTEGER; rel_syllable);
EXTERNAL PROCEDURE gen_vnode(VAR code_list; val_ptr);
EXTERNAL PROCEDURE gen_val(VAR code_list; val);
EXTERNAL FUNCTION gen_cnode(val_ptr): rel_syllable;
EXTERNAL FUNCTION gen_cval(val): rel_syllable;
EXTERNAL FUNCTION gen_cword(pdp10word; code_types): rel_syllable;
EXTERNAL FUNCTION gen_cint(int_type): rel_syllable;
EXTERNAL FUNCTION gen_cst(int_type): addr_desc;
EXTERNAL PROCEDURE wr_code(VAR code_list; VAR code_address; BOOLEAN);

$ENABLE p10cguinc
$ENDIF

