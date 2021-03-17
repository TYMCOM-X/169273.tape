$TITLE M68IMD.PAS, last modified 5/11/84, zw
MODULE m68imd options special(word), check;
(*TYM-Pascal compiler motorola 68000 machine-dependent declarations*)
$SYSTEM ENVUTL.INC
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM pasini.inc
$SYSTEM ptmimd.typ
public const
    mword_size: integer = 32;
    max_real: real_type = 1.7e+38;
    max_char: integer = #o177;
    fnamsize: integer = 36;
    fnamextern: extname = 'I.FILNAM';
    trnamextern: extname = 'M.TRACE';
    exnamextern: extname = 'E.STR';
    condnamextern: condnames = ( 'E.CMATH', 'E.CIO', 'E.CPROG', 'E.CATTN',
				 'E.CHEAP', 'E.CSTACK', 'E.CSPEC' );
$PAGE target machine constants
$INCLUDE ptmcon.inc
$PAGE sys_ppf
(*  SYS PPF will enter the system-dependent predefined procedures and functions
    into the initial symbol table.  *)
public procedure sys_ppf;
begin
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
    r_8_8_16    : rule_node := ( nil, 8, 8, 16 );
    r_all_8_8   : rule_node := ( nil, maximum (bit_range), 8, 8 );
    r_all_8_16  : rule_node := ( nil, maximum (bit_range), 8, 16 );
    r_all_16_16 : rule_node := ( nil, maximum (bit_range), 16, 16 );
    r_all_32_16 : rule_node := ( nil, maximum (bit_range), 32, 16 );
    rules: array [1..5] of rule;
procedure make_rules;
begin
  new (rules [1]);				(* Rule 1 - *)
  rules [1]^ := r_all_8_8;			(*    0..*:  8, 8 *)
  new (rules [2]);				(* Rule 2 - *)
  rules [2]^ := r_all_16_16;			(*    0..*:  16, 16 *)
  new (rules [3]);				(* Rule 3 - *)
  rules [3]^ := r_8_8_8;			(*    0..8:  8, 8 *)
  rules [3]^.next := rules [2];			(*    9..*:  16, 16 *)
  new (rules [4]);				(* Rule 4 - *)
  rules [4]^ := r_8_8_16;			(*    0..8:  8, 16 *)
  rules [4]^.next := rules [2];			(*    9..*:  16, 16 *)
  new (rules [5]);				(* Rule 5 - *)
  rules [5]^ := r_all_32_16;			(*    0..*:  32, 16 *)
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
      ( ( 3, 2, 2, 2, 2 ),			(* scalars *)
	( 1, 1, 1, 1, 2 ),			(* bools *)
	( 1, 1, 1, 1, 2 ),			(* chars *)
	( 3, 5, 5, 5, 5 ),			(* unsigned integers *)
	( 2, 2, 2, 2, 2 ),			(* reals *)
	( 2, 2, 2, 2, 2 ),			(* sets *)
	( 2, 2, 2, 2, 2 ),			(* pointers *)
	( 2, 2, 2, 2, 2 ),			(* files *)
	( 2, 2, 2, 2, 2 ),			(* non-varying strings *)
	( 2, 2, 2, 2, 2 ),			(* arrays *)
	( 2, 2, 2, 2, 2 ),			(* records *)
	( 2, 2, 2, 2, 2 ),			(* variants *)
	( 2, 2, 2, 2, 2 ),			(* tags *)
	( 2, 2, 2, 2, 2 ),			(* procs *)
	( 2, 2, 2, 2, 2 ),			(* funcs *)
	( 2, 2, 2, 2, 2 ),			(* unknown_type *)
	( 2, 2, 2, 2, 2 ) );			(* indirect_type *)
    x_packed_scalar_rules: array [scalars..chars] of x_packing_table :=
      ( ( 3, 3, 3, 3, 2 ),			(* scalars *)
	( 1, 1, 1, 1, 2 ),			(* bools *)
	( 1, 1, 1, 1, 2 ) );			(* chars *)
    x_integer_rules: array [boolean (* signed *), boolean (* packed *)] of x_packing_table :=
    ( ( ( 3, 5, 5, 5, 5 ),			(* unsigned / unpacked *)
	( 3, 3, 3, 3, 4 ) ),			(* unsigned /   packed *)
      ( ( 3, 5, 5, 5, 5 ),			(*   signed / unpacked *)
	( 3, 3, 3, 3, 4 ) ) );			(*   signed /   packed *)
    x_var_string_rules: x_packing_table :=
	( 2, 2, 2, 2, 2 );			(* varying strings *)
    x_arr_desc_rules: x_packing_table :=
	( 5, 5, 5, 5, 5 );			(* array descriptors *)
    x_str_desc_rules: x_packing_table :=
	( 2, 2, 2, 2, 2 );			(* string descriptors *)
    x_pl_base: bit_range := 0;
    x_rv_addr_loc: rv_loc_type := rv_at_start;
    x_rv_value_loc: rv_loc_type := rv_at_start;
    x_a_prm_size: bit_range := 32;
    x_a_prm_alignment: align_range := 16;
    x_pba_types: set of type_kind := [arrays, strings, records, sets];
    x_pbv_limit: bit_range := 32;
    x_pba_retsym: boolean := true;
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
      ( ( 0, true ),				(* initialized static *)
	( 0, true ),				(* uninitialized static *)
	( 0, true ),				(* condition cells *)
	( 0, false),				(* positive stack area - unused *)
	( 0, false) );				(* negative stack area - local variables *)
    x_loc_area: local_areas := neg_stack_area;
    x_retsym_loc: array [boolean (* passed by address *)] of rv_loc_type :=
      ( ( true, false, true ), ( true, false, true ) );
    x_parmlist_loc: array [boolean (* size <= limit *)] of pl_loc_type :=
      ( ( neg_stack_area, true ), ( neg_stack_area, true ) );
    x_pl_size_limit: unit_range := 0;
    x_parm_ptr_size: unit_range := 0;
    x_basic_alignment: align_range := 2;
    x_cond_size: unit_range := 2;
    x_cond_alignment: align_range := 2;
$PAGE
$INCLUDE pasalc.ini
$PAGE tm_constants
(*  TM_CONSTANTS will initialize the PTMCON variables to values which
    characterize the target machine.  *)
public procedure tm_constants;
begin
  tmprefix := 'M68';
  ttyiname := '#CN';
  ttyoname := '#CN';
  rel_extension := 'RO';
  have_checkout := true;
  have_optimizer := false;
  radix := hex_radix;
  adr_width := 8;
  srealprec := 6;
  set_lwb_limit := 0;
  set_upb_limit := 32767;
  set_size_limit := 32768;
  set_lquantum := 16;
  set_uquantum := 16;
  set_lbase := maximum (integer);
  set_ubase := minimum (integer);
  byte_size := 8;
  int_prec_limit := 32;
  qbl_allowed := true;
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
  