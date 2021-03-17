$TITLE PAS.PAS, last modified 5/11/84, zw
PROGRAM pascmd OPTIONS special(word);
(*TYM-Pascal Compiler Command Line Processor*)
$SYSTEM TIMUTL.INC
$SYSTEM RUNUTL.INC
$SYSTEM pascal
$SYSTEM ptmcon
$SYSTEM pasfil
$SYSTEM pasist
$SYSTEM paslog
$SYSTEM passw
$SYSTEM pasenv
$SYSTEM pascfm
$SYSTEM pasopn
$SYSTEM pasopd
$SYSTEM prgdir
$SYSTEM tmpnam
$SYSTEM versio
$SYSTEM tmpcor
EXTERNAL VAR
auto_startup: BOOLEAN;
(*TRUE => terminate at end of control file*)
EXTERNAL
FUNCTION user_ppn: machine_word;
(*READ COMMAND LINE reads and processes command lines. It performs one of
four actions: (1)In case of a syntactic or other error in the command line,
it reads another line. (2) In the case of a blank line(a quit command), it
halts compilation. (3)In the case of a global option setting, the setting
is performed, and another line read.(4)When a full compilation line is read,
it returns to start the compilation.*)

PROCEDURE read_command_line;
CONST
prompt = '*';
VAR
cmd_line: STRING [256];
(*command line as read*)
clidx: INTEGER;
(*scanning cursor for cmd_line*)
exit_flag: BOOLEAN;
(*if /EXIT*)
run_file_name: FILE_NAME;
(*from /RUN*)
run_offset: INTEGER;
(*from /RUNOFFSET*)
LABEL
1 (*abort*);
(*exit from cmd_error into command loop*)
(*CMD ERROR reports an error in a command line and returns to the command loop
by means of a nonlocal goto.*)
TYPE
error_codes =(bad_syntax, bad_source_file, line_too_long, bad_indirect_file,
  bad_check_opt, bad_special_opt, bad_option, not_cmd_option, bad_width,
  bad_length, bad_alloc, opt_expected, paren_expected, val_expected,
  fid_expected, sw_expected, debug_optimize, imd_expected, bad_tm_code,
  overlay_mainseg, defid_expected, no_help_file);

  PROCEDURE cmd_error(error: error_codes);
  VAR
  cmd_file: cmd_stack;
  BEGIN
    cmd_file := cmd_name_stack;
    WHILE cmd_file <> NIL DO BEGIN
      WRITELN(TTY, ' ':LENGTH(prompt), 'In file: ', cmd_file^.cmd_file_name);
      cmd_file := cmd_file^.next
    END;
    IF cmd_name_stack <> NIL THEN WRITELN(TTY, ' ':LENGTH(prompt), cmd_line);
    WRITELN(TTY, ' ':LENGTH(prompt)+clidx-1, '^');
    CASE error OF
      bad_syntax: WRITELN(TTY, '?Command line syntax error.');
      bad_source_file: WRITELN(TTY, '?Cannot open source file.');
      line_too_long: WRITELN(TTY, '?Command line too long.');
      bad_indirect_file: WRITELN(TTY, '?Cannot open indirect command file.');
      bad_check_opt: WRITELN(TTY, '?Invalid CHECK option.');
      bad_special_opt: WRITELN(TTY, '?Invalid SPECIAL option.');
      bad_option: WRITELN(TTY, '?Invalid option.');
      not_cmd_option: WRITELN(TTY, '?Option not valid from command line.');
      bad_width: WRITELN(TTY, '?Illegal page width .');
      bad_length: WRITELN(TTY, '?Illegal page length.');
      bad_alloc: WRITELN(TTY, '?Illegal ALLOC code.');
      opt_expected: WRITELN(TTY, '?Option keyword expected.');
      paren_expected: WRITELN(TTY,
	'?Option value must be enclosed in parentheses.');
      val_expected: WRITELN(TTY, '?Option subfield expected.');
      fid_expected: WRITELN(TTY, '?Filename expected.');
      sw_expected: WRITELN(TTY, '?Switch expected.');
      debug_optimize: WRITELN(TTY,
	'?DEBUG and OPTIMIZE are incompatible options.');
      imd_expected: WRITELN(TTY, '?Immediate command expected.');
      bad_tm_code: WRITELN(TTY, '?Invalid target machine code.');
      overlay_mainseg: WRITELN(TTY,
	'?OVERLAY and MAINSEG are incompatible options');
      defid_expected: WRITELN(TTY, '?Filename default expected');
      no_help_file: WRITELN(TTY, '?No help information is available')
    END;
    WRITELN(TTY);
    BREAK(TTY);
    GOTO 1;
    (*try again*)
  END;
  (*cmd_error*)
  (*WRAPUP deletes any outstanding temporary files.*)

  PROCEDURE wrapup;
  VAR
  temp_file: TEXT;
  BEGIN
    RESET(temp_file, tempname('PA0'));
    SCRATCH(temp_file);
    RESET(temp_file, tempname('XRF'));
    SCRATCH(temp_file);
    RESET(temp_file, tempname('ERR'));
    SCRATCH(temp_file)
  END;
  (* GET_LINE returns a command line from the saved list or the tty. *)

  PROCEDURE get_line;
  VAR
  cmd_next: pending_command;
  stk_next: cmd_stack;
  too_long: BOOLEAN;
  BEGIN
    WHILE(cmd_list <> NIL)ANDIF(cmd_list^.EOF)DO BEGIN
      cmd_next := cmd_list^.next;
      DISPOSE(cmd_list);
      cmd_list := cmd_next;
      stk_next := cmd_name_stack^.next;
      DISPOSE(cmd_name_stack);
      cmd_name_stack := stk_next
    END;
    IF cmd_list = NIL THEN BEGIN
      IF auto_startup THEN BEGIN (*Command file finished - terminate.*)
	wrapup;
	STOP
      END
      ELSE BEGIN
	cmd_getline(prompt, cmd_line, clidx);
	cmd_line := UPPERCASE(cmd_line)
      END
    END
    ELSE (*cmd_list <> nil*)BEGIN
      cmd_line := UPPERCASE(cmd_list^.TEXT);
      too_long := cmd_list^.too_long;
      cmd_next := cmd_list^.next;
      DISPOSE(cmd_list);
      cmd_list := cmd_next;
      IF too_long THEN cmd_error(line_too_long)
    END
  END (*get_line*);

  FUNCTION get_file_name: FILE_NAME;
  BEGIN
    IF NOT cmd_file_name(cmd_line, clidx, TRUE, get_file_name)
      THEN cmd_error(fid_expected)
  END;
  (*NUMERIC VALUE returns the value of a numeric parameter. Default is
  the value to be returned if the parameter is omitted. If Default is
  negative, then an error message will be printed if the parameter is
  omitted.*)

  FUNCTION numeric_value(default: INTEGER): INTEGER;
  VAR
  punct:(paren, colon, none);
  BEGIN
    IF cmd_check_punct(cmd_line, clidx, ':')ORIF
      cmd_check_punct(cmd_line, clidx, '=')THEN punct := colon
    ELSE IF cmd_check_punct(cmd_line, clidx, '(')THEN punct := paren
    ELSE punct := none;
    IF NOT cmd_number(cmd_line, clidx, FALSE, numeric_value)THEN BEGIN
      IF(default >= 0)AND(punct = none)THEN numeric_value := default
      ELSE cmd_error(val_expected)
    END;
    IF(punct = paren)ANDIF NOT cmd_check_punct(cmd_line, clidx, ')')
      THEN cmd_error(paren_expected)
  END;

  FUNCTION switchlist(head: switch_ptr; enable: BOOLEAN): switch_ptr;
  VAR
  switch_name: switch_string;
  paren: BOOLEAN;
  BEGIN
    switchlist := head;
    IF cmd_check_punct(cmd_line, clidx, ':')ORIF
      cmd_check_punct(cmd_line, clidx, '=')THEN paren := FALSE
    ELSE paren := cmd_check_punct(cmd_line, clidx, '(');
    REPEAT
      IF NOT cmd_token(cmd_line, clidx, ['A'..'Z', '0'..'9', '_'], switch_name
	)THEN cmd_error(sw_expected);
      switchlist := enable_switch(switchlist, switch_name, enable);
    UNTIL NOT paren ORIF NOT cmd_check_punct(cmd_line, clidx, ',');
    IF paren ANDIF NOT cmd_check_punct(cmd_line, clidx, ')')
      THEN cmd_error(paren_expected)
  END;

  FUNCTION filelist: search_ptr;
  VAR
  current, last: search_ptr;
  paren: BOOLEAN;
  tempname: FILE_NAME;
  BEGIN
    filelist := NIL;
    IF cmd_check_punct(cmd_line, clidx, ':')ORIF
      cmd_check_punct(cmd_line, clidx, '=')THEN paren := FALSE
    ELSE paren := cmd_check_punct(cmd_line, clidx, '(');
    REPEAT
      IF NOT cmd_file_name(cmd_line, clidx, FALSE, tempname)
	THEN cmd_error(defid_expected);
      NEW(current, LENGTH(tempname));
      current^.next := NIL;
      current^.name := tempname;
      IF filelist = NIL THEN filelist := current
      ELSE last^.next := current;
      last := current;
    UNTIL NOT paren ORIF NOT cmd_check_punct(cmd_line, clidx, ',');
    IF paren AND NOT cmd_check_punct(cmd_line, clidx, ')')
      THEN cmd_error(paren_expected)
  END;

  PROCEDURE sea_save(list: search_ptr; VAR f: FILE OF * );
  VAR
  ll: search_ptr;
  len: INTEGER;
  BEGIN
    ll := list;
    WHILE ll <> NIL DO BEGIN
      len := LENGTH(ll^.name);
      WRITE(f, len, ll^: SIZE(ll^, len));
      ll := ll^.next
    END;
    len := 0;
    WRITE(f, len)
  END;

  FUNCTION sea_load(VAR f: FILE OF * ): search_ptr;
  VAR
  sea, last_sea: search_ptr;
  len: INTEGER;
  BEGIN
    sea_load := NIL;
    LOOP
      READ(f, len);
      EXIT IF len = 0;
      NEW(sea, len);
      READ(f, sea^: SIZE(sea^, len));
      sea^.next := NIL;
      IF sea_load = NIL THEN sea_load := sea
      ELSE last_sea^.next := sea;
      last_sea := sea
    END
  END;
  (*TMNAME will return the target machine name associated with a particular
  target machine code.*)

  FUNCTION tmname(prefix: parm_string): parm_string;
  TYPE
  target_machines =(tm_p10, tm_vax, tm_m68, tm_none);
  tm_list = ARRAY [target_machines] OF RECORD
    prefix: fstr_3;
    name: STRING [10]
  END;
  CONST
  tm_names: tm_list =(('P10', 'PDP 10'),('VAX', 'VAX-11') ,('M68', 'M68000')
    ,(' ', ''));
  VAR
  tm_idx: target_machines;
  BEGIN
    tmname := '';
    FOR tm_idx := MINIMUM(target_machines)TO MAXIMUM(target_machines)
      DO EXIT IF tm_names[tm_idx].prefix = prefix DO tmname := tm_names[tm_idx
      ].name
  END;
  (*LOAD ENVIRONMENT will load a specified environment file. If the load
  fails, it will print an error message and attempt to reload the old
  environment.*)

  PROCEDURE load_environment(env_file: FILE_NAME; prefix: fstr_3);
  VAR
  save_file: FILE OF *;
  save_options: command_options;
  save_startup: BOOLEAN;
  save_log: log_file_record;
  BEGIN
    IF NOT wrpas(tempname('PA0')) THEN STOP;
    REWRITE(save_file, tempname('PSF'));
    cmd_save(save_file);
    sw_save(default_options.switches, save_file);
    sw_save(default_options.dump_switches, save_file);
    sea_save(default_options.search_list, save_file);
    CLOSE(save_file);
    save_options := default_options;
    save_startup := auto_startup;
    save_log := log_record;
    IF rdpas('.ENV ' ||env_file, FALSE)THEN BEGIN
      REWRITE(TTY);
      IF env_name = NIL THEN WRITELN(TTY, '[', tmname(tmprefix)
	, ' initial environment]')
      ELSE WRITELN(TTY, '[', tmname(tmprefix)
	, ' environment ', env_name^.TEXT, ' created ', SUBSTR(dc_ext(
	env_dtime), 1, 15), ']');
      default_options := save_options;
      auto_startup := save_startup;
      log_record := save_log;
      RESET(save_file, tempname('PSF'));
      cmd_restore(save_file);
      default_options.switches := sw_load(save_file);
      default_options.dump_switches := sw_load(save_file);
      default_options.search_list := sea_load(save_file);
      SCRATCH(save_file)
    END
    ELSE BEGIN
      REWRITE(TTY);
      IF prefix = '' THEN WRITELN(TTY, '?Unable to load environment ',
	env_file)
      ELSE WRITELN(TTY, '?Unable to load ', tmname(prefix)
	, ' initial environment');
      IF NOT rdpas(tempname('PA0'), FALSE)THEN BEGIN
	REWRITE(TTY);
	WRITELN(TTY, '?Unable to reload old environment');
	STOP
      END;
      REWRITE(TTY)
    END;
    OPEN(TTY)
  END (*load_environment*);
  (*PROCESS IMMEDIATE COMMAND reads and executes a ":" command.*)

  PROCEDURE process_immediate_command;
  VAR
  new_prefix: parm_string;
  imd_idx: INTEGER;
  BEGIN
    IF NOT cmd_lookup(cmd_line, clidx, ['A'..'Z'], opdict_imd_cmd_table,
      imd_idx)THEN cmd_error(imd_expected);
    CASE imd_commands(imd_idx)OF
      imd_environment: load_environment(get_file_name(), '');
      imd_target: BEGIN
	IF NOT cmd_token(cmd_line, clidx, ['A'..'Z', '0'..'9'], new_prefix)
	  ORIF(tmname(new_prefix)= '')THEN cmd_error(bad_tm_code);
	load_environment(new_prefix || 'INI.ENV' || prgm_dir(), new_prefix)
      END
    END (*case*);
    IF NOT cmd_eol(cmd_line, clidx)THEN cmd_error(bad_syntax)
  END (*process_immediate_command*);

  PROCEDURE check_sub_options(positive: BOOLEAN);
  TYPE
  check_set = SET OF checklist;
  CONST
  check_all: check_set = [ MINIMUM(checklist).. MAXIMUM(checklist)];
  VAR
  opt_set: check_set;
  sub_opt: INTEGER;
  paren: BOOLEAN;
  BEGIN
    IF cmd_eol(cmd_line, clidx)THEN opt_set := check_all
    ELSE IF cmd_check_punct(cmd_line, clidx, ',')ORIF
      cmd_check_punct(cmd_line, clidx, '/')THEN BEGIN
      opt_set := check_all;
      clidx := clidx - 1;
      (*leave the punctuation*)
    END
    ELSE BEGIN
      opt_set := [];
      IF cmd_check_punct(cmd_line, clidx, ':')ORIF
	cmd_check_punct(cmd_line, clidx, '=')THEN paren := FALSE
      ELSE paren := cmd_check_punct(cmd_line, clidx, '(');
      REPEAT
	IF NOT cmd_lookup(cmd_line, clidx, ['A'..'Z'], opdcot_chk_opt_table,
	  sub_opt)THEN cmd_error(bad_check_opt);
	opt_set := opt_set + [opdmtc_map_to_chk_opt [chk_opts(sub_opt)]];
      UNTIL NOT paren ORIF NOT cmd_check_punct(cmd_line, clidx, ',');
      IF paren ANDIF NOT cmd_check_punct(cmd_line, clidx, ')')
	THEN cmd_error(paren_expected)
    END;
    WITH prog_options DO IF positive THEN semantic_options := semantic_options
      - check_all + opt_set
    ELSE semantic_options := semantic_options - opt_set
  END;

  PROCEDURE special_sub_options(positive: BOOLEAN);
  TYPE
  special_set = SET OF speciallist;
  CONST
  special_all: special_set = [ MINIMUM(speciallist).. MAXIMUM(speciallist)];
  VAR
  opt_set: special_set;
  sub_opt: INTEGER;
  paren: BOOLEAN;
  BEGIN
    IF cmd_eol(cmd_line, clidx)THEN opt_set := special_all
    ELSE IF cmd_check_punct(cmd_line, clidx, ',')ORIF
      cmd_check_punct(cmd_line, clidx, '/')THEN BEGIN
      opt_set := special_all;
      clidx := clidx - 1;
      (*leave the punctuation*)
    END
    ELSE BEGIN
      opt_set := [];
      IF cmd_check_punct(cmd_line, clidx, ':')ORIF
	cmd_check_punct(cmd_line, clidx, '=')THEN paren := FALSE
      ELSE paren := cmd_check_punct(cmd_line, clidx, '(');
      REPEAT
	IF NOT cmd_lookup(cmd_line, clidx, ['A'..'Z'], opdsot_sp_opt_table,
	  sub_opt)THEN cmd_error(bad_special_opt);
	opt_set := opt_set + [opdmts_map_to_sp_opt [sp_opts(sub_opt)]];
      UNTIL NOT paren ORIF NOT cmd_check_punct(cmd_line, clidx, ',');
      IF paren ANDIF NOT cmd_check_punct(cmd_line, clidx, ')')
	THEN cmd_error(paren_expected)
    END;
    WITH prog_options DO IF positive THEN semantic_options := semantic_options
      + opt_set
    ELSE semantic_options := semantic_options - opt_set
  END;

  PROCEDURE do_help;
  VAR
  help_file: FILE OF *;
  index_cursor, index_entries: INTEGER;
  INDEX: ^ ARRAY [1..*] OF cmd_lookup_record;
  i: INTEGER;
  code: INTEGER;
  token: STRING [10];

    PROCEDURE print_message(code: INTEGER);
    TYPE
    str = PACKED ARRAY [1..*] OF CHAR;
    VAR
    len: INTEGER;
    TEXT: PACKED ARRAY [1..80] OF CHAR;
    BEGIN
      READRN(help_file, code, len);
      WHILE len <> -1 DO BEGIN
	READ(help_file, TEXT: SIZE(str, len));
	WRITELN(TTY, SUBSTR(TEXT, 1, len));
	READ(help_file, len)
      END
    END;
  BEGIN
    RESET(help_file, 'PASCAL.HLP' || prgm_dir(), [SEEKOK]);
    IF iostatus <> IO_OK THEN BEGIN
      CLOSE(help_file);
      cmd_error(no_help_file)
    END;
    READ(help_file, index_cursor, index_entries);
    NEW(INDEX, index_entries);
    READRN(help_file, index_cursor, INDEX^: SIZE(INDEX^, index_entries));
    IF cmd_check_punct(cmd_line, clidx, ':')THEN BEGIN
      IF cmd_check_punct(cmd_line, clidx, '*')THEN BEGIN
	FOR i := 1 TO index_entries DO print_message(INDEX^[i].code)
      END
      ELSE IF cmd_check_punct(cmd_line, clidx, '(')THEN BEGIN
	REPEAT
	  IF cmd_lookup(cmd_line, clidx, ['A'..'Z'], INDEX^, code)
	    THEN print_message(code)
	  ELSE IF cmd_token(cmd_line, clidx, ['A'..'Z', '0'..'9', '_'], token
	    )THEN WRITELN(TTY, '%No information available on ', token)
	  ELSE BEGIN
	    print_message(INDEX^[1].code);
	    DISPOSE(INDEX);
	    CLOSE(help_file);
	    cmd_error(bad_syntax)
	  END;
	UNTIL NOT cmd_check_punct(cmd_line, clidx, ',');
	IF NOT cmd_check_punct(cmd_line, clidx, ')')
	  THEN cmd_error(paren_expected)
      END
      ELSE IF cmd_lookup(cmd_line, clidx, ['A'..'Z'], INDEX^, code)
	THEN print_message(code)
      ELSE IF cmd_token(cmd_line, clidx, ['A'..'Z', '0'..'9', '_'], token)
	THEN WRITELN(TTY, '%No information available on ', token)
      ELSE BEGIN
	print_message(INDEX^[1].code);
	DISPOSE(INDEX);
	CLOSE(help_file);
	cmd_error(bad_syntax)
      END
    END
    ELSE print_message(INDEX^[1].code);
    DISPOSE(INDEX);
    CLOSE(help_file)
  END;
  (*SCAN OPTIONS is called to process command line options. The scanning cursor
  points to the character past the '/' when entered. On exit, prog_options
  contain the derived options.*)

  PROCEDURE scan_options;
  VAR
  ix: INTEGER;
  option_ix: option_scalar;
  positive, automatic: BOOLEAN;
  val: INTEGER;
  stemp: search_ptr;
  BEGIN
    WITH prog_options DO BEGIN
      REPEAT (*at least one option expected at start*)
	cmd_skip_blanks(cmd_line, clidx);
	IF SUBSTR(cmd_line, clidx, MIN(LENGTH(cmd_line)-clidx+1, 2))
	  = 'NO' THEN BEGIN
	  positive := FALSE;
	  automatic := FALSE;
	  clidx := clidx + 2
	END
	ELSE IF SUBSTR(cmd_line, clidx, MIN(LENGTH(cmd_line)-clidx+1, 4))
	  = 'AUTO' THEN BEGIN
	  positive := TRUE;
	  automatic := TRUE;
	  clidx := clidx + 4
	END
	ELSE BEGIN
	  positive := TRUE;
	  automatic := FALSE
	END;
	IF NOT cmd_lookup(cmd_line, clidx, ['A'..'Z'], opdotb_option_table, ix
	  )THEN cmd_error(opt_expected);
	option_ix := option_scalar(ix);
	IF(NOT positive AND NOT(option_ix IN opdnoo_no_options))OR
	  (automatic AND NOT(option_ix IN opdauo_auto_options))
	  THEN cmd_error(bad_option);
	CASE option_ix OF
	  opt_terse: terse_opt := TRUE;
	  opt_verbose: terse_opt := FALSE;
	  opt_length: BEGIN
	    val := numeric_value(-1);
	    IF(val < 4)OR(val > 255)THEN cmd_error(bad_length);
	    page_length := val
	  END;
	  opt_width: BEGIN
	    val := numeric_value(-1);
	    IF(val < 20)OR(val > 255)THEN cmd_error(bad_width);
	    page_width := val
	  END;
	  opt_search: BEGIN
	    WHILE search_list <> NIL DO BEGIN
	      stemp := search_list;
	      search_list := search_list^.next;
	      DISPOSE(stemp)
	    END;
	    IF positive THEN search_list := filelist()
	  END;
	  opt_enable: switches := switchlist(switches, TRUE);
	  opt_disable: switches := switchlist(switches, FALSE);
	  opt_dump: dump_switches := switchlist(dump_switches, positive);
	  opt_source: IF automatic THEN source_opt := opt_is_auto
	  ELSE IF positive THEN source_opt := opt_is_on
	  ELSE source_opt := opt_is_o	  opt_lsystem: lsys_opt := positive;
	  opt_errors: errors_opt := positive;
	  opt_quick: IF automatic THEN quick_opt := opt_is_auto
	  ELSE IF positive THEN quick_opt := opt_is_on
	  ELSE quick_opt := opt_is_off;
	  opt_banner: banner_opt := positive;
	  opt_kicode: ki_code_opt := positive;
	  opt_code: code_opt := positive;
	  opt_finish: finish_opt := positive;
	  opt_statistics: statistics_opt := positive;
	  opt_names: names_opt := positive;
	  opt_global: global_opt := positive;
	  opt_check: check_sub_options(positive);
	  opt_trace..opt_optimize: BEGIN
	    IF(option_ix = opt_optimize)AND positive AND
	      debug_opt THEN cmd_error(debug_optimize);
	    IF positive THEN semantic_options := semantic_options + [
	      opdmto_map_to_optionlist [option_ix]]
	    ELSE semantic_options := semantic_options - [
	      opdmto_map_to_optionlist [option_ix]]
	  END;
	  opt_special: special_sub_options(positive);
	  opt_overlay: BEGIN
	    IF positive AND mainseg_opt THEN cmd_error(overlay_mainseg);
	    overlay_opt := positive
	  END;
	  opt_mainseg: BEGIN
	    IF positive AND overlay_opt THEN cmd_error(overlay_mainseg);
	    mainseg_opt := positive
	  END;
	  opt_debug: BEGIN
	    IF positive AND(optimize_opt IN semantic_options)
	      THEN cmd_error(debug_optimize);
	    debug_opt := positive
	  END;
	  opt_masking: masking_opt := positive;
	  opt_underflow: underflow_opt := positive;
	  opt_storage: storage := numeric_value(-1);
	  opt_alloc: BEGIN
	    val := numeric_value(-1);
	    IF val > 99 THEN cmd_error(bad_alloc);
	    alloc_mode := val
	  END;
	  opt_standard: WRITELN(TTY,
	    '%The STANDARD option has not been implemented');
	  opt_exit: exit_flag := TRUE;
	  opt_run: IF cmd_check_punct(cmd_line, clidx, ':')
	    THEN run_file_name := get_file_name()
	  ELSE cmd_error(fid_expected);
	  opt_runoffset: run_offset := numeric_value(1);
	  opt_help: do_help;
	  OTHERS: cmd_error(not_cmd_option)
	END (*case*);
      UNTIL NOT(cmd_check_punct(cmd_line, clidx, ',')ORIF
	cmd_check_punct(cmd_line, clidx, '/'))
    END (*with*)
  END;
VAR
cmd_file_name: FILE_NAME;
cmd_file: TEXT;
mf_idx: INTEGER;

  PROCEDURE get_main_file;
  BEGIN
    cmd_skip_blanks(cmd_line, clidx);
    mf_idx := clidx;
    main_file := get_file_name()
  END;
BEGIN (*read_command_line*)
  1 (*abort*): (*reenter command loop after error*)
  REPEAT
    prog_options := default_options;
    (*initialize options from defaults*)
    get_line;
    clidx := 1;
    (*start parse at beginning of line*)
    exit_flag := FALSE;
    run_file_name := '';
    run_offset := 0;
    main_file := '';
    IF cmd_eol(cmd_line, clidx)THEN BEGIN (*blank line => stop*)
      wrapup;
      STOP
    END;
    IF cmd_check_punct(cmd_line, clidx, '@')THEN BEGIN
      cmd_file_name := get_file_name();
      IF NOT cmd_eol(cmd_line, clidx)THEN cmd_error(bad_syntax);
      RESET(cmd_file, '.CCL ' || cmd_file_name);
      IF iostatus <> IO_OK THEN RESET(cmd_file, '.CMD ' || cmd_file_name);
      IF iostatus <> IO_OK THEN cmd_error(bad_indirect_file);
      rd_cmd_file(cmd_file);
      CLOSE(cmd_file)
    END
    ELSE IF cmd_check_punct(cmd_line, clidx, ':')
      THEN process_immediate_command
    ELSE IF cmd_check_punct(cmd_line, clidx, '/')THEN BEGIN
      scan_options;
      IF NOT cmd_eol(cmd_line, clidx)THEN cmd_error(bad_syntax);
      default_options := prog_options
    END
    ELSE BEGIN (*have files to compile*)
      rel_file := '';
      list_file := '';
      list_explicit := FALSE;
      IF cmd_check_punct(cmd_line, clidx, '=')THEN (*=source*)
      get_main_file
      ELSE IF cmd_check_punct(cmd_line, clidx, ',')THEN BEGIN (*,[list]=source*)
	IF cmd_check_punct(cmd_line, clidx, '=')THEN (*,=source*)
	get_main_file
	ELSE BEGIN (*,list=source*)
	  list_file := get_file_name();
	  list_explicit := TRUE;
	  IF NOT cmd_check_punct(cmd_line, clidx, '=')
	    THEN cmd_error(bad_syntax);
	  get_main_file
	END
      END
      ELSE BEGIN
	get_main_file;
	IF cmd_check_punct(cmd_line, clidx, '=')THEN BEGIN (*rel=source*)
	  rel_file := main_file;
	  get_main_file
	END
	ELSE IF cmd_check_punct(cmd_line, clidx, ',')THEN BEGIN (*rel,[list]=source*)
	  rel_file := main_file;
	  IF cmd_check_punct(cmd_line, clidx, '=')THEN (*rel,=source*)
	  get_main_file
	  ELSE BEGIN (*rel,list=source*)
	    list_file := get_file_name();
	    list_explicit := TRUE;
	    IF NOT cmd_check_punct(cmd_line, clidx, '=')
	      THEN cmd_error(bad_syntax);
	    get_main_file
	  END
	END
	ELSE BEGIN (*source*)
	  rel_file := SUBSTR(main_file, SEARCH(main_file, [':', ')'])+ 1);
	  rel_file := SUBSTR(rel_file, 1, SEARCH(rel_file, ['[', '.'], LENGTH(
	    rel_file)+ 1)- 1);
	  list_file := rel_file
	END
      END;
      IF cmd_check_punct(cmd_line, clidx, '/')THEN scan_options;
      (*get options for *this* compilation*)
      IF NOT cmd_eol(cmd_line, clidx)THEN cmd_error(bad_syntax);
      IF NOT open_search(INPUT, '.PAS ' || main_file)THEN BEGIN
	clidx := mf_idx;
	(*report error at file name*)
	cmd_error(bad_source_file)
      END
    END (*file case*);
    IF exit_flag THEN BEGIN
      wrapup;
      STOP
    END;
    IF run_file_name <> '' THEN BEGIN
      wrapup;
      IF NOT runprg('SYS: ' || run_file_name, run_offset)THEN
    END;
  UNTIL main_file <> ''
END (*read_command_line*);
(* STARTUP initializes the compiler permanent data the first time Pass 1 is
invoked. This involves writing a version message, initializing the log
record skeleton, determining whether the compiler is being run from the
terminal or from another program, and reading the auto-startup command
file in the latter case. *)

PROCEDURE startup OPTIONS special(coercions);
VAR
jbver: ^ INTEGER;
buffer: tmp_buffer;
len: tmpcor_length;
tcr: BOOLEAN;
cmd_file: TEXT;
BEGIN
  REWRITE(TTY);
  WRITELN(TTY, 'TYM-Pascal Compiler, Version ', version());
  WRITELN(TTY);
  IF NOT rdpas('P10INI.ENV' || prgm_dir(), FALSE)THEN BEGIN
    REWRITE(TTY);
    WRITELN(TTY, '?Unable to load initial environment');
    STOP
  END;
  (* Prepare the standard log record skeleton. *)
  jbver := PTR(137b);
  log_record.version := jbver^;
  log_record.users_ppn := user_ppn();
  log_record.opt_auto_run := runoff > 0;
  log_record.opt_tmpcor := FALSE;
  log_record.opt_hash := FALSE;
  auto_startup := runoff > 0;
  IF auto_startup THEN BEGIN
    len := 0;
    tcr := tmpcor('PAS', tmpcor_rf, 0, len);
    (*Look for a tmpcor file.*)
    IF tcr THEN BEGIN
      NEW(buffer, len);
      (*If its there, read and delete it.*)
      tcr := tmpcor('PAS', tmpcor_df, ORD(buffer), len)
    END;
    IF tcr THEN BEGIN (*Read commands from tmpcor file.*)
      log_record.opt_tmpcor := TRUE;
      rd_tmpcor('TMP:PAS', buffer, len)
    END
    ELSE BEGIN
      RESET(cmd_file, tempname('PAS'));
      IF NOT EOF(cmd_file)THEN BEGIN
	log_record.opt_hash := TRUE;
	(*Read commands from temp file.*)
	rd_cmd_file(cmd_file);
	SCRATCH(cmd_file)
      END
      ELSE (*No commands -- forget auto-startup.*)
      auto_startup := FALSE
    END
  END (*if auto_startup*)
END (*startup*);

BEGIN
  IF NOT rdpas(tempname('PA0'), FALSE)THEN BEGIN
    startup;
    IF NOT wrpas(tempname('PA0')) THEN STOP
  END;
  OPEN(TTY);
  REWRITE(TTY);
  read_command_line;
  (*Process a command.*)
  prog_options.switches := enable_switch(prog_options.switches, tmprefix, TRUE
    );
  IF NOT wrpas(tempname('PA0')) THEN STOP;
  (*Now off to Pass 1 to compile a module.*)
  IF NOT switch(prog_options.dump_switches, 'MANUAL')THEN BEGIN
    IF NOT runprg('PASANL' || prgm_dir(), 1)THEN BEGIN
      REWRITE(TTY);
      WRITELN(TTY, '?Unable to run PASANL')
    END
  END
END.
    u A^