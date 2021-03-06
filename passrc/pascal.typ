$PAGE PASCAL.TYP, last modified 4/3/84, zw
$IFNOT pascaltyp

(*SYSTEM UTLTYP.TYP*)
(*SYSTEM UTLSRC.TYP*)
(*SYSTEM UTLLST.TYP*)
(*SYSTEM UTLSW.TYP*)
(*SYSTEM UTLKEY.TYP*)
(*SYSTEM PASADR.TYP*)

TYPE target_code = (kl10, ki10);
CONST target_names: ARRAY[1 .. 3] OF key_record =
  (('KI10', 2, ORD(ki10)), ('KL10', 2, ORD(kl10)), ('PDP10', 2, ORD(kl10)));

CONST
current_version = 'TYM-Pascal, Version 2';
system_directory = '(PASSRC)';
initial_target = kl10;
command_program = system_directory || 'PASCMD';
analysis_program = system_directory || 'PASANL';
optimizing_program = system_directory || 'PASOPT';
listing_program = system_directory || 'PASLST';
code_generation_program: ARRAY[target_code] OF RECORD qcg, ocg: file_name END =
  ((system_directory || 'PKLCCG', system_directory || 'PKLOCG'),
   (system_directory || 'PKICCG', system_directory || 'PKIOCG'));
target_environment_file: ARRAY[target_code] OF file_name =
  (system_directory || 'PKLINI.ENV',
   system_directory || 'PKIINI.ENV');
runtime_library_file: ARRAY[target_code] OF file_name =
  (system_directory || 'PKLLIB.REL',
   system_directory || 'PKILIB.REL');
global_environment_file = '###GLB.ENV';
current_environment_file = '###CUR.ENV';
load_save_file = '###SAV.TMP';
command_file = '###TMP.CMD';
cross_reference_file = '###TMP.XRF';
error_log_file = '###TMP.ERR';
error_message_file = system_directory || 'PASERR.TXT';
message_file = system_directory || 'PASMSG.TXT';
help_file = system_directory || 'PASCAL.HLP';
log_file = system_directory || 'PASCAL.LOG';

$PAGE semantic_option in PASCAL.TYP
TYPE
semantic_option = (
  check_assert_option, (*check assertions*)
  check_case_option, (*check case statements*)
  check_array_option, (*check array compatibility*)
  check_variant_option, (*check field variant tags*)
  check_file_option, (*check file references and dereferences*)
  check_number_option, (*check input integers and reals*)
  check_pointer_option, (*check nil pointer dereferences*)
  check_string_option, (*check substring bounds*)
  check_subscript_option, (*check array subscripts*)
  check_scalar_option, (*check scalar assignments*)
  special_coercion_option, (*allow ADDRESS and pointer coercion functions*)
  special_pointer_option, (*allow use of type PTR*)
  special_word_option, (*allow use of type target_WORD*)
  map_option, (*generate line/object code mapping*)
  symbols_option, (*list declared symbols*)
  calls_option, (*list called procedures*)
  assembly_option, (*list generated code*)
  xref_option, (*generate cross-reference*)
  trace_option, (*emit trace information*)
  quick_blocks_option, (*allow quick block analysis*)
  optimize_option); (*optimize code, overridden by debug*)
check_semantic_option = check_assert_option .. check_scalar_option;
special_semantic_option = special_coercion_option .. special_word_option;
option_mode = (option_is_off, option_is_auto, option_is_on);

$PAGE command_option_record in PASCAL.TYP
command_option_record = RECORD
  semantic_options: SET OF semantic_option; (*default semantic options*)
  banner_option: yes_no; (*banner pages desired?*)
  code_option: yes_no; (*generate code?*)
  debug_option: yes_no; (*use debugger?*)
  errors_option: yes_no; (*print an errors listing?*)
  finish_option: yes_no; (*finish compilation after warnings?*)
  global_option: yes_no; (*global cross reference desired?*)
  list_system_option: yes_no; (*list $SYSTEM files?*)
  mainseg_option: yes_no; (*code is for main sharable overlay?*)
  mask_option: yes_no; (*allow MASK(ATTENTION) to work?*)
  names_option: yes_no; (*print file name at start of compilation?*)
  overlay_option: yes_no; (*code will be overlaid?*)
  quick_option: option_mode; (*use quick code generator?*)
  source_option: option_mode; (*listing desired?*)
  statistics_option: yes_no; (*stats desired after compilation?*)
  terse_option: yes_no; (*short error messages?*)
  underflow_option: yes_no; (*signal MATHERROR on floating underflows?*)
  page_length: list_length_range; (*zero => pagination???*)
  page_width: list_width_range; (*columns per listing line*)
  alloc_mode: 0 .. 99; (*dyes_noamic allocation strategy code*)
  storage: unit_range; (*stack size*)
  switches: switch_pointer; (*from enable/disable*)
  dump_switches: switch_pointer; (*from dump()*)
  search_list: source_search_list (*from search()*)
END;

$ENABLE pascaltyp
$ENDIF
  