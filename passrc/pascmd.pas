$TITLE PASCMD.PAS, last modified 4/3/84, zw
PROGRAM pascmd;
(*TYM-Pascal compiler -- command line processor/dispatcher*)

$PAGE system declarations
$SYSTEM TYPUTL.TYP
$SYSTEM FIOUTL.INC
$SYSTEM RUNUTL.INC
$SYSTEM HLPUTL.INC
$SYSTEM SWTUTL.INC
$SYSTEM CMDUTL.INC
$SYSTEM TCRUTL.INC
$SYSTEM SRCUTL.INC
$SYSTEM KEYUTL.INC
$SYSTEM PASCAL.INC
$SYSTEM PASENV.INC
$SYSTEM PASLOG.INC

$INCLUDE PASCMD.TYP

$PAGE definitions

CONST prompt = '*'; (*prompt used in read_command*)

TYPE command_error = (source_file_error, excess_text_error,
  equals_expected_error, immediate_command_error, indirect_file_error,
  auto_option_error, no_option_error, option_expected_error,
  command_option_error, standard_option_error, help_option_error,
  allocation_option_error, storage_option_error, debug_optimize_error,
  overlay_mainseg_error, special_option_error, check_option_error,
  kicode_option_error, width_option_error, length_option_error,
  target_command_error, switch_expected_error, value_expected_error,
  file_name_error, paren_expected_error, load_environment_error);

CONST command_error_message: ARRAY [command_error] OF STRING[60] =
  ('?Can not access source file.',
   '?Excess text follows command.',
   '?"=" expected before source file name.',
   '?Immediate command expected.',
   '?Can not access indirect command file.',
   '?Command option does not accept "AUTO".',
   '?Command option does not accept "NO".',
   '?Command option expected.',
   '?Option is not allowed on command line.',
   '?The "STANDARD" option has not been implemented yet.',
   '?No "HELP" is available, sorry.',
   '?Allocation code is out of range.',
   '?Storage stack size is out of range.',
   '?"DEBUG" and "OPTIOMIZE" are incompatable options.',
   '?"OVERLAY" and "MAINSEG" are incompatable options.',
   '?"SPECIAL" suboption expected.',
   '?"CHECK" suboption expected.',
   '?Can not use "KICODE" option with current target machine.',
   '?Page width value is out of range.',
   '?Page length value is out of range.',
   '?Target machine code expected.',
   '?Switch name expected.',
   '?Integer value expected.',
   '?File name expected.',
   '?")" expected to end list.',
   '?Can not load environment.');

VAR
command_line: command_line_string;
command_cursor: command_line_cursor;
done: yes_no; (*if /EXIT*)
program_to_run: file_name; (*from /RUN*)
program_offset: integer; (*from /RUNOFFSET*)

EXCEPTION abort_command;

$PAGE signal_error, read_command, skip_spaces, etc.
PROCEDURE signal_error(code: command_error);
(*Reports an error in a command line and signals "abort_command".*)
BEGIN
  cmderr(command_line, command_cursor, command_error_message[code]);
  SIGNAL(abort_command)
END;

PROCEDURE read_command;
(*Get the next command line from the saved list or the terminal.*)
BEGIN
  rdcmd(command_line, command_cursor)
END;

PROCEDURE skip_spaces;
(*Skip past any spaces.*)
BEGIN
  skpspc(command_line, command_cursor)
END;

FUNCTION scan_end_of_line: yes_no;
(*Try to scan the end of the line.*)
BEGIN
  skip_spaces;
  scan_end_of_line := chkeol(command_line, command_cursor)
END;

PROCEDURE check_end_of_line;
(*Check for end of line.  Error if excess text.*)
BEGIN
  IF NOT scan_end_of_line THEN signal_error(excess_text_error)
END;

FUNCTION scan_character(ch: CHAR): yes_no;
(*Try to scan the specified character.*)
BEGIN
  skip_spaces;
  scan_character := chkchr(command_line, command_cursor, ch)
END;

FUNCTION scan_paren: yes_no;
BEGIN
  IF scan_character(':') ORIF scan_character('=') THEN scan_paren := no
  ELSE scan_paren := scan_character('(')
END;

PROCEDURE check_paren(paren: yes_no);
BEGIN
  IF paren AND NOT scan_character(')') THEN signal_error(paren_expected_error)
END;

$PAGE scan_key, scan_file_name, scan_number, scan_switch_list, etc.
FUNCTION scan_key
  (chars: char_set; list: key_list; VAR code: integer): yes_no;
VAR key: string_argument;
BEGIN
  scan_key := scnkey(command_line, command_cursor, chars, key) ANDIF
    lkpkey(key, list, code)
END;

FUNCTION scan_file_name: file_name;
BEGIN
  skip_spaces;
  IF NOT scnfil(command_line, command_cursor, scan_file_name)
  THEN signal_error(file_name_error)
END;

FUNCTION scan_number(default: INTEGER): INTEGER;
(*Scan a number.  If nothing, default if default >= 0 else error.*)
VAR paren: boolean; digits: STRING[11];
BEGIN
  paren := scan_paren;
  skip_spaces;
  IF NOT scnkey(command_line, command_cursor, ['0' .. '9'], digits) THEN BEGIN
    IF default >= 0 THEN scan_number := default
    ELSE signal_error(value_expected_error)
  END
  ELSE GETSTRING(digits, scan_number);
  check_paren(paren)
END;

PROCEDURE scan_switch_list(VAR list: switch_pointer; enable: yes_no);
(*Parse and add names to switch list, enable as specified.*)
VAR name: switch_name; paren: yes_no;
BEGIN
  paren := scan_paren;
  REPEAT
    skip_spaces;
    IF NOT scnkey(command_line, command_cursor, ['A'..'Z', '0'..'9', '_'],
      name)
    THEN signal_error(switch_expected_error);
    list := enasw(list, name, enable)
  UNTIL NOT paren ORIF NOT scan_character(',');
  check_paren(paren)
END;

PROCEDURE scan_file_list(VAR list: source_search_list);
(*Parse and add names to search list.*)
VAR paren: yes_no;
BEGIN
  paren := scan_paren;
  REPEAT
    newsrch(list, scan_file_name)
  UNTIL NOT paren ORIF NOT scan_character(',');
  check_paren(paren)
END;

$PAGE load_environment, new_target
PROCEDURE load_environment(name: file_name);
(*Load the specified environment file.  Re-load previous if load fails.*)
VAR save_file: binary_file; save_options: command_option_record;
  save_usecmd: yes_no; save_log: log_record;
BEGIN
  IF NOT wrpas(current_environment_file)
  THEN byebye('?Unable to write temporary environment.');
  REWRITE(save_file, load_save_file);
  stcmd(save_file);
  stsw(defopts.switches, save_file);
  stsw(defopts.dump_switches, save_file);
  stsrch(defopts.search_list, save_file);
  CLOSE(save_file);
  save_options := defopts;
  save_usecmd := usecmd;
  save_log := logrcd;
  IF rdpas(name, no) THEN BEGIN
    dpymsg('[' || keystr(target_names, ORD(target)) || ' environment ' ||
      name || ']');
    defopts := save_options;
    usecmd := save_usecmd;
    logrcd := save_log;
    RESET(save_file, load_save_file);
    ldcmd(save_file);
    defopts.switches := ldsw(save_file);
    defopts.dump_switches := ldsw(save_file);
    defopts.search_list := ldsrch(save_file);
    SCRATCH(save_file)
  END
  ELSE BEGIN
    IF NOT rdpas(current_environment_file, TRUE)
    THEN byebye('?Unable to reload environment.');
    signal_error(load_environment_error)
  END
END;

PROCEDURE new_target;
(*Establish a new target machine.*)
VAR code: integer;
BEGIN
  IF scan_key(['A'..'Z', '0'..'9'], target_names, code)
  THEN BEGIN
    target := target_code(code);
    load_environment(target_environment_file[target])
  END
  ELSE signal_error(target_command_error)
END;

$PAGE make_mode, do_length_option, do_width_option, etc.
FUNCTION make_mode(yes_flag, auto_flag: yes_no): option_mode;
(*Determine the option mode.*)
BEGIN
  IF auto_flag THEN make_mode := option_is_auto
  ELSE IF yes_flag THEN make_mode := option_is_on
  ELSE make_mode := option_is_off
END;

PROCEDURE do_length_option;
VAR value: integer;
BEGIN
  value := scan_number(-1); 
  WITH glbopts DO BEGIN
    IF (value < MINIMUM(page_length)) OR (value > MAXIMUM(page_length))
    THEN signal_error(length_option_error);
    page_length := value
  END
END;

PROCEDURE do_width_option;
VAR value: integer;
BEGIN
  value := scan_number(-1);
  WITH glbopts DO BEGIN
    IF (value < MINIMUM(page_width)) OR (value > MAXIMUM(page_width))
    THEN signal_error(width_option_error);
    page_width := value;
  END
END;

PROCEDURE do_search_option(yes_flag: yes_no);
BEGIN
  zapsrch(glbopts.search_list);
  IF yes_flag THEN scan_file_list(glbopts.search_list)
END;

PROCEDURE do_kicode_option(yes_flag: yes_no);
BEGIN
  IF NOT (target IN [kl10, ki10]) THEN signal_error(kicode_option_error);
  IF yes_flag THEN target := ki10 ELSE target := kl10
END;

$PAGE do_check_option
PROCEDURE do_check_option(yes_flag: yes_no);
(*Process CHECK sub-options.*)
TYPE check_set = SET OF check_semantic_option;
CONST check_all: check_set =
  [MINIMUM(check_semantic_option) .. MAXIMUM(check_semantic_option)];
VAR option_set: check_set; code: integer; paren: yes_no;
BEGIN
  option_set := check_all;
  IF scan_end_of_line THEN RETURN
  ELSE IF scan_character(',') ORIF scan_character('/')
  THEN command_cursor := command_cursor - 1
  ELSE BEGIN
    option_set := [];
    paren := scan_paren;
    REPEAT
      IF NOT scan_key(['A' .. 'Z'], check_suboptions, code)
      THEN signal_error(check_option_error);
      option_set :=
        option_set + [from_check_suboption[check_suboption(code)]]
    UNTIL NOT paren ORIF NOT scan_character(',');
    check_paren(paren)
  END;
  WITH glbopts DO IF yes_flag
  THEN semantic_options := semantic_options - check_all + option_set
  ELSE semantic_options := semantic_options - option_set
END;

PROCEDURE do_semantic_options(yes_flag: yes_no; code: integer);
BEGIN
  WITH glbopts DO BEGIN
    IF (command_option(code) = option_optimize) AND yes_flag AND debug_option
    THEN signal_error(debug_optimize_error);
    IF yes_flag
    THEN semantic_options :=
      semantic_options + [from_command_option[command_option(code)]]
    ELSE semantic_options :=
      semantic_options - [from_command_option[command_option(code)]]
  END
END;

$PAGE do_special_option, do_overlay_option, do_mainseg_option
PROCEDURE do_special_option(yes_flag: yes_no);
(*Process SPECIAL sub-options.*)
TYPE special_set = SET OF special_semantic_option;
CONST special_all: special_set =
  [MINIMUM(special_semantic_option) .. MAXIMUM(special_semantic_option)];
VAR option_set: special_set; code: integer; paren: yes_no;
BEGIN
  option_set := special_all;
  IF scan_end_of_line THEN RETURN
  ELSE IF scan_character(',') ORIF scan_character('/')
  THEN command_cursor := command_cursor - 1
  ELSE BEGIN
    option_set := [];
    paren := scan_paren;
    REPEAT
      IF NOT scan_key(['A' .. 'Z'], special_suboptions, code)
      THEN signal_error(special_option_error);
      option_set :=
        option_set + [from_special_suboption[special_suboption(code)]]
    UNTIL NOT paren ORIF NOT scan_character(',');
    check_paren(paren)
  END;
  WITH glbopts DO IF yes_flag
  THEN semantic_options := semantic_options - special_all + option_set
  ELSE semantic_options := semantic_options - option_set
END;

PROCEDURE do_overlay_option(yes_flag: yes_no);
BEGIN
  IF yes_flag AND glbopts.mainseg_option
  THEN signal_error(overlay_mainseg_error);
  glbopts.overlay_option := yes_flag
END;

PROCEDURE do_mainseg_option(yes_flag: yes_no);
BEGIN
  IF yes_flag AND glbopts.overlay_option
  THEN signal_error(overlay_mainseg_error);
  glbopts.mainseg_option := yes_flag
END;

$PAGE do_debug_option, do_storage_option, do_allocate_option, etc.
PROCEDURE do_debug_option(yes_flag: yes_no);
BEGIN
  IF yes_flag AND (optimize_option IN glbopts.semantic_options)
  THEN signal_error(debug_optimize_error);
  glbopts.debug_option := yes_flag
END;

PROCEDURE do_storage_option;
VAR value: integer;
BEGIN
  value := scan_number(-1);
  WITH glbopts DO BEGIN
    IF (value < MINIMUM(storage)) OR (value > MAXIMUM(storage))
    THEN signal_error(storage_option_error);
    storage := value
  END
END;

PROCEDURE do_allocate_option;
VAR value: integer;
BEGIN
  value := scan_number(-1);
  WITH glbopts DO BEGIN
    IF (value < MINIMUM(alloc_mode)) OR (value > MAXIMUM(alloc_mode))
    THEN signal_error(allocation_option_error);
    alloc_mode := value
  END
END;

PROCEDURE do_run_option;
VAR paren: yes_no;
BEGIN
  paren := scan_paren;
  program_to_run := 'SYS: ' || scan_file_name;
  check_paren(paren)
END;

PROCEDURE do_help_option;
BEGIN
  IF NOT help(command_line, command_cursor, help_file)
  THEN signal_error(help_option_error)
END;

$PAGE do_option
PROCEDURE do_option(code: integer; is_yes, is_auto: yes_no);
BEGIN
  CASE command_option(code) OF
    option_terse: glbopts.terse_option := is_yes;
    option_verbose: glbopts.terse_option := NOT is_yes;
    option_length: do_length_option;
    option_width: do_width_option;
    option_search: do_search_option(is_yes);
    option_enable: scan_switch_list(glbopts.switches, TRUE);
    option_disable: scan_switch_list(glbopts.switches, FALSE);
    option_dump: scan_switch_list(glbopts.dump_switches, is_yes);
    option_source: glbopts.source_option := make_mode(is_yes, is_auto);
    option_list_system: glbopts.list_system_option := is_yes;
    option_errors: glbopts.errors_option := is_yes;
    option_quick: glbopts.quick_option := make_mode(is_yes, is_auto);
    option_banner: glbopts.banner_option := is_yes;
    option_kicode: do_kicode_option(is_yes);
    option_code: glbopts.code_option := is_yes;
    option_finish: glbopts.finish_option := is_yes;
    option_statistics: glbopts.statistics_option := is_yes;
    option_names: glbopts.names_option := is_yes;
    option_global: glbopts.global_option := is_yes;
    option_check: do_check_option(is_yes);
    option_trace .. option_optimize: do_semantic_options(is_yes, code);
    option_special: do_special_option(is_yes);
    option_overlay: do_overlay_option(is_yes);
    option_mainseg: do_mainseg_option(is_yes);
    option_debug: do_debug_option(is_yes);
    option_masking: glbopts.mask_option := is_yes;
    option_underflow: glbopts.underflow_option := is_yes;
    option_storage: do_storage_option;
    option_allocate: do_allocate_option;
    option_standard: signal_error(standard_option_error);
    option_exit: done := true;
    option_run: do_run_option;
    option_runoffset: program_offset := scan_number(1);
    option_help: do_help_option;
    OTHERS: signal_error(command_option_error)
  END
END;

$PAGE process_options, process_options_command, process_command_file
PROCEDURE process_options;
(*Process command line options.*)
VAR code: integer; is_yes, is_auto: yes_no; save_cursor: command_line_cursor;
BEGIN
  REPEAT
    is_yes := yes;
    is_auto := no;
    save_cursor := command_cursor;
    IF scan_key(['A' .. 'Z'], (('NO', 2, 1), ('AUTO', 4, 2)), code) THEN BEGIN
      save_cursor := command_cursor;
      CASE code OF
        1: is_yes := no;
        2: is_auto := yes
      END
    END;
    command_cursor := save_cursor;
    IF NOT scan_key(['A'..'Z'], command_options, code)
    THEN signal_error(option_expected_error);
    IF NOT is_yes AND NOT (command_option(code) IN no_options)
    THEN signal_error(no_option_error);
    IF is_auto AND NOT (command_option(code) IN auto_options)
    THEN signal_error(auto_option_error);
    do_option(code, is_yes, is_auto)
  UNTIL NOT (scan_character(',') ORIF scan_character('/'))
END;

PROCEDURE process_options_command;
(*Process command to set new default options.*)
BEGIN
  process_options;
  check_end_of_line;
  defopts := glbopts
END;

PROCEDURE process_command_file;
(*Process an "@" command file.*)
VAR command_file: file_name;
BEGIN
  command_file := scan_file_name;
  check_end_of_line;
  IF NOT cmdfil('.CCL' || command_file) ANDIF
    NOT cmdfil('.CMD' || command_file) ANDIF
    NOT cmdfil('.COM' || command_file)
  THEN signal_error(indirect_file_error)
END;

$PAGE process_compilation_command
PROCEDURE process_compilation_command;
(*Process actual compilation command.*)
BEGIN
  relfil := ''; 
  lstfil := ''; 
  lstexp := no;
  IF scan_character('=') THEN BEGIN (*=<source>*)
    srcfil := scan_file_name
  END
  ELSE IF scan_character(',') THEN BEGIN (*,[<list>]=<source>*)
    IF scan_character('=') THEN BEGIN (*,=<source>*)
      srcfil := scan_file_name
    END
    ELSE BEGIN (*,<list>=<source>*)
      lstfil := scan_file_name;
      lstexp := true;
      IF NOT scan_character('=') THEN signal_error(equals_expected_error);
      srcfil := scan_file_name
    END
  END
  ELSE BEGIN
    srcfil := scan_file_name;
    IF scan_character('=') THEN BEGIN (*<rel>=<source>*)
      relfil := srcfil;
      srcfil := scan_file_name
    END
    ELSE IF scan_character(',') THEN BEGIN (*<rel>,[<list>]=<source>*)
      relfil := srcfil;
      IF scan_character('=') THEN BEGIN (*<rel>,=<source>*)
        srcfil := scan_file_name
      END
      ELSE BEGIN (*<rel>,<list>=<source>*)
        lstfil := scan_file_name;
	lstexp := yes;
        IF NOT scan_character('=') THEN signal_error(equals_expected_error);
        srcfil := scan_file_name;
      END
    END
    ELSE BEGIN (*<source>*)
    (*This seems to be dependant upon the PDP10/TYMCOM-10 environment.*)
      relfil := SUBSTR(srcfil, SEARCH(srcfil, [':', ')']) + 1);
      relfil :=
        SUBSTR(relfil, 1, SEARCH(relfil, ['[', '.'], LENGTH(relfil) + 1) - 1);
      lstfil := relfil
    END
  END;
  IF scan_character('/') THEN process_options;
  check_end_of_line;
  IF opnsrc(glbopts.search_list, '.PAS ' || srcfil)
  THEN assume(popin, '?All is lost.')
  ELSE signal_error(source_file_error)
END;

$PAGE process_immediate_command, process_compiler_command
PROCEDURE process_immediate_command;
(*Read and execute a ":" command. *)
VAR command_code: integer;
BEGIN
  IF scan_key(['A' .. 'Z'], immediate_commands, command_code)
  THEN CASE immediate_command(command_code) OF
    environment_command: load_environment(scan_file_name);
    target_command: new_target;
  END
  ELSE signal_error(immediate_command_error);
  check_end_of_line
END;

PROCEDURE process_compiler_command;
(*Read and process TYM-Pascal compiler command lines.*)
VAR was_error: yes_no;
BEGIN
  REPEAT
    done := no;
    program_to_run := '';
    program_offset := 0;
    srcfil := '';
    was_error := no;
    glbopts := defopts;
    BEGIN
      read_command;
      IF done ORIF scan_end_of_line THEN done := TRUE
      ELSE IF scan_character('@') THEN process_command_file
      ELSE IF scan_character(':') THEN process_immediate_nd
      ELSE IF scan_character('/') THEN process_options_command
      ELSE process_compilation_command
      EXCEPTION
      abort_command: was_error := yes
    END
  UNTIL NOT was_error ANDIF
    (done OR (program_to_run <> '') OR (srcfil <> ''));
END;

$PAGE delete_temporary_files, read_tcr, read_command_file
PROCEDURE delete_temporary_files;
(*Delete TYM-Pascal compiler temporary files.*)
BEGIN
  delfil(global_environment_file);
  delfil(current_environment_file);
  delfil(command_file);
  delfil(load_save_file);
  delfil(cross_reference_file);
  delfil(error_log_file)
END;

FUNCTION read_tcr: yes_no;
(*Try to read tcr -- dependant upon the PDP10/TYMCOM-10 environment*)
VAR buffer: tcr_buffer; buffer_length: tcr_buffer_length;
BEGIN
  buffer_length := 0;
  read_tcr := tcrfun('PAS', tcr_rf, 0, buffer_length);
  IF read_tcr THEN BEGIN
    NEW(buffer, buffer_length);
    read_tcr := tcrfun('PAS', tcr_df, ORD(buffer), buffer_length)
  END;
  IF read_tcr THEN BEGIN
    logrcd.was_tcr := yes;
    read_tcr := cmdtcr('TMP:PAS', buffer)
  END
END;

FUNCTION read_command_file: yes_no;
(*Try to get commands from command file.*)
BEGIN
  read_command_file := read_tcr;
  IF NOT read_command_file THEN BEGIN
    read_command_file := cmdfil(command_file);
    IF read_command_file THEN BEGIN
      logrcd.was_command_file := yes;
      delfil(command_file)
    END
  END
END;

$PAGE start_compiler, main block
PROCEDURE start_compiler;
(*Start/Restart the TYM-Pascal compiler.*)
BEGIN
  dpymsg(current_version);
  IF rdpas(target_environment_file[target], FALSE) THEN BEGIN
    glbopts := defopts;
    newlog;
    IF NOT usecmd ORIF NOT read_command_file THEN dpyfil(message_file)
  END
  ELSE byebye('?Unable to read initial environment.')
END;

BEGIN
  IF NOT rdpas(global_environment_file, TRUE) THEN start_compiler;
  process_compiler_command;
  IF done THEN BEGIN (*This is the normal compiler termination.*)
    delete_temporary_files
  END
  ELSE IF program_to_run <> '' THEN BEGIN (*Run the specified program.*)
    delete_temporary_files;
    IF NOT runprg(program_to_run, program_offset)
    THEN byebye('?Unable to run program: ' || program_to_run)
  END
  ELSE BEGIN (*Chain to the next compiler program.*)
    IF wrpas(global_environment_file) THEN chain(analysis_program)
    ELSE byebye('?Unable to write global environment.')
  END
END.
    n@
ê