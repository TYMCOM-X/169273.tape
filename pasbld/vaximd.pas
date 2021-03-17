$TITLE VAXIMD.PAS, last modified 5/14/84, zw
MODULE vaximd;
(*TYM-Pascal compiler VAX/VMS machine-dependant declarations*)
$SYSTEM ENVUTL.INC
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM pasini.inc
$SYSTEM ptmimd.typ
public const
    mword_size: integer = 32;
    max_real: real_type = 1.7e+38;
    max_char: integer = 177b;
    fnamsize: integer = 125;
    fnamextern: extname = 'PAX_FIO.FNAME';
    trnamextern: extname = 'TRACE';
    condnamextern: condnames = ( 'PAX_EXC.MATH', 'PAX_EXC.IO', 'PAX_EXC.USER',
	'PAX_EXC.ATTN', 'PAX_EXC.STOR', 'PAX_EXC.STK', 'PAX_EXC.SPEC' );
$PAGE target machine constants
$INCLUDE ptmcon.inc
$PAGE sys_ppf
(*  SYS PPF will enter the system-dependent predefined procedures and functions
    into the initial symbol table.  *)
public procedure sys_ppf;
begin
  predef ( 'SPY', 'SPY', consts, prtyp0 ( nil ) ); (* prints trace of most recently executed stmts *)
end;
$PAGE init_tal_tables
(*  INIT TAL TABLES initializes the type allocation tables used by PASTAL.  *)
external var tal_tables: environment;
procedure init_tal_tables options special(coercions);
$INCLUDE pastal.prm
$PAGE
type
    x_packing_table = array [packing_contexts] of integer;
static var
    r_8_8_8     : rule_node := ( nil, 8, 8, 8 );
    r_16_8_8    : rule_node := ( nil, 16, 8, 8 );
    r_16_8_32   : rule_node := ( nil, 16, 8, 32 );
    r_16_16_16  : rule_node := ( nil, 16, 16, 16 );
    r_all_8_8   : rule_node := ( nil, maximum (bit_range), 8, 8 );
    r_all_8_16  : rule_node := ( nil, maximum (bit_range), 8, 16 );
    r_all_16_8  : rule_node := ( nil, maximum (bit_range), 16, 8 );
    r_all_16_16 : rule_node := ( nil, maximum (bit_range), 16, 16 );
    r_all_16_32 : rule_node := ( nil, maximum (bit_range), 16, 32 );
    r_all_32_8  : rule_node := ( nil, maximum (bit_range), 32, 8 );
    r_all_32_32 : rule_node := ( nil, maximum (bit_range), 32, 32 );
    rules: array [1..10] of rule;
procedure make_rules;
begin
  new (rules [1]);	(* Rule 1 - *)
  rules [1]^ := r_all_8_8;	(*    0..*:  8, 8 *)
  new (rules [2]);	(* Rule 2 - *)
  rules [2]^ := r_all_8_16;	(*    0..*:  8, 16 *)
  new (rules [3]);	(* Rule 3 - *)
  rules [3]^ := r_all_32_8;	(*    0..*:  32, 8 *)
  new (rules [4]);	(* Rule 4 - *)
  rules [4]^ := r_all_32_32;	(*    0..*:  32, 32 *)
  new (rules [5]);	(* Rule 5 - *)
  rules [5]^ := r_8_8_8;	(*    0..8:  8, 8 *)
  new (rules [5]^.next);
  rules [5]^.next^ := r_16_16_16;	(*   9..16:  16, 16 *)
  rules [5]^.next^.next := rules [4];	(*   17..*:  32, 32 *)
  new (rules [6]);	(* Rule 6 - *)
  rules [6]^ := r_16_8_8;	(*   0..16:  8, 8 *)
  rules [6]^.next := rules [3];	(*   17..*:  32, 8 *)
 
  new (rules [7]);	(* Rule 7 - *)
  rules [7]^ := r_16_8_32;	(*   0..16:  8, 32 *)
  rules [7]^.next := rules [4];	(*   17..*:  32, 32 *)
  new (rules [8]);	(* Rule 8 - *)
  rules [8]^ := r_all_16_8;	(*   0..*:  16, 8 *)
  new (rules [9]);	(* Rule 9 - *)
  rules [9]^ := r_all_16_16;	(*   0..*:  16, 16 *)
  new (rules [10]);	(* Rule 10 - *)
  rules [10]^ := r_all_16_32;	(*   0..*:  16, 32 *)
end (* make_rules *);
$PAGE
static var
    x_efw: bit_range := 32;
    x_efa: bit_range := 8;
    x_str_lw_width: bit_range := 16;
    x_str_char_size: bit_range := 8;
    x_real_base_size: array [prec_type] of bit_range :=
      ( 32, 32, 32, 32, 32, 32, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64 );
    x_pointer_base_size: bit_range := 32;
    x_file_base_size: bit_range := 32;
    x_subr_base_size: bit_range := 64;
    x_allocation_tables: array [type_kind] of x_packing_table :=
      ( ( 6, 5, 5, 5, 7 ),	(* scalars *)
	( 1, 1, 1, 1, 7 ),	(* bools *)
	( 1, 1, 1, 1, 7 ),	(* chars *)
	( 6, 4, 4, 4, 4 ),	(* unsigned integers *)
	( 3, 4, 4, 4, 4 ),	(* reals *)
	( 1, 1, 1, 1, 1 ),	(* sets *)
	( 3, 4, 4, 4, 4 ),	(* pointers *)
	( 3, 4, 4, 4, 4 ),	(* files *)
	( 1, 1, 1, 1, 1 ),	(* non-varying strings *)
	( 1, 1, 1, 1, 1 ),	(* arrays *)
	( 1, 1, 1, 1, 1 ),	(* records *)
	( 1, 1, 1, 1, 1 ),	(* variants *)
        ( 1, 1, 1, 1, 1 ),	(* tags *)
	( 3, 4, 4, 4, 4 ),	(* procs *)
	( 3, 4, 4, 4, 4 ),	(* funcs *)
	( 5, 5, 5, 5, 5 ),	(* unknown_type *)
	( 5, 5, 5, 5, 5 ) );	(* indirect_type *)
    x_packed_scalar_rules: array [scalars..chars] of x_packing_table :=
      ( ( 6, 5, 5, 5, 7 ), (* scalars *)
	( 1, 1, 1, 1, 7 ), (* bools *)
	( 1, 1, 1, 1, 7 ) ); (* chars *)
    x_integer_rules: array [boolean (* signed *), boolean (* packed *)] of x_packing_table :=
    ( ( ( 6, 4, 4, 4, 4 ), (* unsigned / unpacked *)
	( 6, 5, 5, 5, 7 ) ), (* unsigned /   packed *)
      ( ( 6, 4, 4, 4, 4 ), (*   signed / unpacked *)
	( 6, 5, 5, 5, 7 ) ) ); (*   signed /   packed *)
    x_var_string_rules: x_packing_table :=
	( 1, 2, 2, 2, 2 );	(* varying strings *)
    x_arr_desc_rules: x_packing_table :=
	( 3, 4, 4, 4, 4 );	(* array descriptors *)
    x_str_desc_rules: x_packing_table :=
	( 8, 9, 9, 9, 10 );	(* string descriptors *)
    x_pl_base: bit_range := 32;
    x_rv_addr_loc: rv_loc_type := rv_at_start;
    x_rv_value_loc: rv_loc_type := rv_nowhere;
    x_a_prm_size: bit_range := 32;
    x_a_prm_alignment: align_range := 32;
    x_pba_types: set of type_kind := [arrays, strings, records, sets];
    x_pbv_limit: bit_range := 64;
    x_pba_retsym: boolean := false;
$PAGE
$INCLUDE pastal.ini
$PAGE init_alc_tables
(*  INIT ALC TABLES initializes the storage allocation tables used by PASALC.  *)
external var alc_tables: environment;
procedure init_alc_tables options special(coercions);
$PAGE
$INCLUDE pasalc.prm
$PAGE
static var
    x_alc_area_descs: array [area_class] of area_desc :=
      ( ( 0, true ), (* initialized static *)
	( 0, true ), (* uninitialized static *)
	( 0, true ), (* condition cells *)
	( 0, false), (* positive stack area - unused *)
	( 8, false) ); (* negative stack area - local variables *)
    x_loc_area: local_areas := neg_stack_area;
    x_retsym_loc: array [boolean (* passed by address *)] of rv_loc_type :=
      ( ( false, false, true ), ( true, false, true ) );
    x_parmlist_loc: array [boolean (* size <= limit *)] of pl_loc_type :=
      ( ( neg_stack_area, true ), ( neg_stack_area, true ) );
    x_pl_size_limit: unit_range := 0;
    x_parm_ptr_size: unit_range := 4;
    x_basic_alignment: align_range := 4;
    x_cond_size: unit_range := 4;
    x_cond_alignment: align_range := 4;
$PAGE
$INCLUDE pasalc.ini
$PAGE tm_constants
(*  TM_CONSTANTS will initialize the PTMCON variables to values which
    characterize the target machine.  *)
public procedure tm_constants;
begin
  tmprefix := 'VAX';
  ttyiname := 'PAX_INPUT';
  ttyoname := 'PAX_OUTPUT';
  rel_extension := 'OBJ';
  have_checkout := true;
  have_optimizer := false;
  radix := hex_radix;
  adr_width := 8;
  srealprec := 6;
  set_lwb_limit := 0;
  set_upb_limit := 32767;
  set_size_limit := 32768;
  set_lquantum := 8;
  set_uquantum := 8;
  set_lbase := maximum (integer);
  set_ubase := minimum (integer);
  byte_size := 8;
  int_prec_limit := 32;
  qbl_allowed := false;
  init_tal_tables;
  init_alc_tables;
end (* tm_constants *);
$PAGE dcl_stt_constants
(* DCL SCONSTS will declare the elements of the scalar status types.  *)
public procedure dcl_sconsts;
begin
  stt_constant ('IO_OK'  , stat_io, 0);
  stt_constant ('IO_NOVF', stat_io, 1);
  stt_constant ('IO_POVF', stat_io, 2);
  stt_constant ('IO_DGIT', stat_io, 3);
  stt_constant ('IO_GOVF', stat_io, 4);
  stt_constant ('IO_INTR', stat_io, 5);
  stt_constant ('IO_REWR', stat_io, 6);
  stt_constant ('IO_EOF' , stat_io, 7);
  stt_constant ('IO_OUTF', stat_io, 8);
  stt_constant ('IO_INPF', stat_io, 9);
  stt_constant ('IO_SEEK', stat_io, 10);
  stt_constant ('IO_ILLC', stat_io, 11);
  stt_constant ('IO_NEMP', stat_io, 12);
  stt_constant ('IO_OPNF', stat_io, 13);
  stt_constant ('MATH_OK',		stat_math, 0);
  stt_constant ('MATH_FLT_UND',		stat_math, 1);
  stt_constant ('MATH_FLT_OVF',		stat_math, 2);
  stt_constant ('MATH_INT_OVF',		stat_math, 3);
  stt_constant ('MATH_ZERO_DIVIDE',	stat_math, 4);
  stt_constant ('MATH_ARG_ARCSIN',	stat_math, 5);
  stt_constant ('MATH_ARG_ARCCOS',	stat_math, 6);
  stt_constant ('MATH_SQRT_NEG',	stat_math, 7);
  stt_constant ('MATH_BAD_LOG',		stat_math, 8);
  stt_constant ('MATH_BAD_ARG',		stat_math, 9);
  stt_constant ('MATH_LOST_SIGN',	stat_math, 10);
  stt_constant ('PROGRAM_OK',		stat_program, 0);
  stt_constant ('PROGRAM_ASSERTION',	stat_program, 1);
  stt_constant ('PROGRAM_CASE',		stat_program, 2);
  stt_constant ('PROGRAM_COMPATIBILITY',stat_program, 3);
  stt_constant ('PROGRAM_FILE',		stat_program, 4);
  stt_constant ('PROGRAM_POINTER',	stat_program, 5);
  stt_constant ('PROGRAM_SUBSTRING',	stat_program, 6);
  stt_constant ('PROGRAM_SUBSCRIPT',	stat_program, 7);
  stt_constant ('PROGRAM_VALUE',	stat_program, 8);
  stt_constant ('SPECIAL_OK',		stat_special, 0);
  stt_constant ('SPECIAL_ILL_MEM_REF',	stat_special, 1);
  stt_constant ('SPECIAL_ILL_INST',	stat_special, 2);
  stt_constant ('SPECIAL_NEW_NEG',	stat_special, 3);
  stt_constant ('SPECIAL_DISP_PTR',	stat_special, 4);
  stt_constant ('SPECIAL_DISP_TWICE',	stat_special, 5);
  stt_constant ('SPECIAL_SUCC_PRED',	stat_special, 6);
  stt_constant ('SPECIAL_VAX_INDEX',	stat_special, 7)
end.
   