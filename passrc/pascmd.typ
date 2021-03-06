$PAGE PASCMD.TYP, last modified 4/4/84, zw
$IFNOT pascmdtyp

(*SYSTEM UTLKEY.TYP*)
(*SYSTEM PASCAL.INC*)

TYPE
immediate_command = (environment_command, target_command);
command_option =		(*  CMD  BLK  NO  *)
  (option_terse,		(*   X            *)
   option_verbose,		(*   X            *)
   option_length,		(*   X            *)
   option_width,		(*   X            *)
   option_enable,		(*   X            *)
   option_disable,		(*   X            *)
   option_quick,		(*   X         X  *)
   option_statistics,		(*   X         X  *)
   option_names,		(*   X         X  *)
   option_code,			(*   X         X  *)
   option_finish,		(*   X         X  *)
   option_source,		(*   X         X  *)
   option_search,		(*   X         X  *)
   option_list_system,		(*   X         X  *)
   option_errors,		(*   X         X  *)
   option_banner,		(*   X         X  *)
   option_kicode,		(*   X         X  *)
   option_check,		(*   X    X    X  *)
   option_trace,		(*   X    X    X  *)
   option_quick_blocks,		(*   X    X    X  *)
   option_map,			(*   X    X    X  *)
   option_symbols,		(*   X    X    X  *)
   option_calls,		(*   X    X    X  *)
   option_assembly,		(*   X    X    X  *)
   option_xref,			(*   X    X    X  *)
   option_optimize,		(*   X    X    X  *)
   option_special,		(*   X    X    X  *)
   option_overlay,		(*   X    X    X  *)
   option_mainseg,		(*   X    X    X  *)
   option_debug,		(*   X    X    X  *)
   option_underflow,		(*   X    X    X  *)
   option_masking,		(*   X    X    X  *)
   option_global,		(*   X    X    X  *)
   option_standard,		(*   X    X    X  *)
   option_dump,			(*   X    X    X  *)
   option_allocate,		(*   X    X       *)
   option_storage,		(*   X    X       *)
   option_fortran,		(*        X       *)
   option_run,			(*                *)
   option_runoffset,		(*                *)
   option_help,			(*                *)
   option_exit);		(*                *)
command_options_set = SET OF command_option;

$PAGE check_suboption, special_suboption, immediate_commands IN PASCMD.TYP
check_suboption = (option_check_assert, option_check_case,
  option_check_array, option_check_variant, option_check_file,
  option_check_number, option_check_pointer, option_check_string,
  option_check_subscript, option_check_scalar);
special_suboption = (option_special_coercion, option_special_pointer,
  option_special_word);

CONST
immediate_commands:
  ARRAY [1 .. ORD(MAXIMUM(immediate_command)) + 1] OF key_record =
  (('ENVIRONMENT', 3, ORD(environment_command)),
   ('TARGET', 3, ORD(target_command)));

$PAGE command_options in PASCMD.TYP
command_options: ARRAY [1 .. ORD(MAXIMUM(command_option)) + 1] OF key_record =
  (('ALLOCATE', 2, ORD(option_allocate)),
   ('ASSEMBLY', 1, ORD(option_assembly)),
   ('BANNER', 3, ORD(option_banner)),
   ('CALLS', 2, ORD(option_calls)),
   ('CHECK', 2, ORD(option_check)),
   ('CODE', 3, ORD(option_code)),
   ('DEBUG', 3, ORD(option_debug)),
   ('DISABLE', 3, ORD(option_disable)),
   ('DUMP', 4, ORD(option_dump)),
   ('ENABLE', 2, ORD(option_enable)),
   ('ERRORS', 3, ORD(option_errors)),
   ('EXIT', 4, ORD(option_exit)),
   ('FINISH', 3, ORD(option_finish)),
   ('FORTRAN', 7, ORD(option_fortran)),
   ('GLOBAL', 4, ORD(option_global)),
   ('HELP', 1, ORD(option_help)),
   ('KICODE', 2, ORD(option_kicode)),
   ('LENGTH', 3, ORD(option_length)),
   ('LSYSTEM', 4, ORD(option_list_system)),
   ('MAINSEG', 4, ORD(option_mainseg)),
   ('MAP', 3, ORD(option_map)),
   ('MASKING', 4, ORD(option_masking)),
   ('NAMES', 4, ORD(option_names)),
   ('OPTIMIZE', 3, ORD(option_optimize)),
   ('OVERLAY', 2, ORD(option_overlay)),
   ('QBLOCKS', 3, ORD(option_quick_blocks)),
   ('QUICK', 1, ORD(option_quick)),
   ('RUN', 3, ORD(option_run)),
   ('RUNOFFSET', 6, ORD(option_runoffset)),
   ('SEARCH', 3, ORD(option_search)),
   ('SOURCE', 1, ORD(option_source)),
   ('SPECIAL', 2, ORD(option_special)),
   ('STANDARD', 4, ORD(option_standard)),
   ('STATISTICS', 4, ORD(option_statistics)),
   ('STORAGE', 3, ORD(option_storage)),
   ('SYMBOLS', 3, ORD(option_symbols)),
   ('TERSE', 1, ORD(option_terse)),
   ('TRACE', 2, ORD(option_trace)),
   ('UNDERFLOW', 5, ORD(option_underflow)),
   ('VERBOSE', 1, ORD(option_verbose)),
   ('WIDTH', 3, ORD(option_width)),
   ('XREF', 1, ORD(option_xref)));

$PAGE line_options, block_options, no_options, auto_options in PASCMD.TYP
line_options: command_options_set = [option_terse .. option_storage];
block_options: command_options_set = [option_check .. option_fortran];
no_options: command_options_set = [option_quick .. option_dump];
auto_options: command_options_set = [option_source, option_quick];

from_command_option:
  ARRAY [option_trace .. option_optimize] OF semantic_option =
  (trace_option, quick_blocks_option, map_option, symbols_option, calls_option,
   assembly_option, xref_option, optimize_option) ;

to_command_option: ARRAY [semantic_option] OF command_option =
  (option_check, option_check, option_check, option_check, option_check,
   option_check, option_check, option_check, option_check, option_check,
   option_special, option_special, option_special, option_map, option_symbols,
   option_calls, option_assembly, option_xref, option_trace,
   option_quick_blocks, option_optimize);

$PAGE check_suboptions in PASCMD.TYP
check_suboptions: ARRAY [1 .. ORD(MAXIMUM(check_suboption))+1] OF key_record =
  (('ASSERTIONS', 3, ORD(option_check_assert)),
   ('CASES', 3, ORD(option_check_case)),
   ('COMPATIBILITY', 3, ORD(option_check_array)),
   ('FIELDS', 3, ORD(option_check_variant)),
   ('FILES', 3, ORD(option_check_file)),
   ('INPUT', 3, ORD(option_check_number)),
   ('POINTERS', 3, ORD(option_check_pointer)),
   ('STRINGS', 3, ORD(option_check_string)),
   ('SUBSCRIPTS', 3, ORD(option_check_subscript)),
   ('VALUES', 3, ORD(option_check_scalar)));

from_check_suboption: ARRAY [check_suboption] OF check_semantic_option =
  (check_assert_option, check_case_option, check_array_option,
   check_variant_option, check_file_option, check_number_option,
   check_pointer_option, check_string_option, check_subscript_option,
   check_scalar_option);

to_check_suboption: ARRAY [check_semantic_option] OF check_suboption =
  (option_check_assert, option_check_case, option_check_array,
    option_check_variant, option_check_file, option_check_number,
    option_check_pointer, option_check_string, option_check_subscript,
    option_check_scalar);

$PAGE special_options in PASCMD.TYP
special_suboptions:
  ARRAY [1 .. ORD(MAXIMUM(special_suboption)) + 1] OF key_record=
  (('COERCIONS', 3, ORD(option_special_coercion)),
   ('PTR', 3, ORD(option_special_pointer)),
   ('WORD', 3, ORD(option_special_word)));

from_special_suboption: ARRAY [special_suboption] OF special_semantic_option =
  (special_coercion_option, special_pointer_option, special_word_option);

to_special_suboption: ARRAY [special_semantic_option] OF special_suboption =
  (option_special_coercion, option_special_pointer, option_special_word);

$ENABLE pascmdtyp
$ENDIF
 