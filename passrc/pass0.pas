$WIDTH=100
$LENGTH=55
$TITLE PASS0.PAS, last modified 1/10/84, zw
PROGRAM pass0 OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- command line processor/dispatcher*)

(*all programs in the TYM-Pascal compiler chain here when done*)

$PAGE system declarations

$SYSTEM PASCAL.INC
$SYSTEM PASENV.INC
$SYSTEM PTMCON.INC
$SYSTEM PASFIL.INC
$SYSTEM PASIST.INC
$SYSTEM PASLOG.INC
$SYSTEM UTLHLP.INC
$SYSTEM UTLSW.INC
$SYSTEM PASCFM.INC
$SYSTEM UTLOPN.INC
$SYSTEM PASOPD.INC
$SYSTEM UTLPDR.INC
$SYSTEM DTIME.INC
$SYSTEM RUN.INC
$SYSTEM PASLOG.INC
$SYSTEM TMPCOR.INC

$PAGE definitions

CONST prompt = '*'; (*prompt used in get_cmd*)

TYPE errs =
  (bad_syn, bad_src_fil, lin_too_big, bad_ind_fil, bad_chk_opt,
  bad_spc_opt, bad_opt, not_cmd_opt, bad_wid, bad_len, bad_alc,
  opt_exp, prn_exp, val_exp, fid_exp, sw_exp, dbg_opt, imd_exp,
  bad_tm_cod, ovl_msg, defid_exp, no_hlp, no_std);

VAR
cmd: STRING[256]; (*command line*)
idx: INTEGER; (*scanning cursor for cmd*)
done: BOOLEAN;	(*if /EXIT*)
run_file: FILE_NAME; (*from /RUN*)
run_offset: INTEGER; (*from /RUNOFFSET*)

EXCEPTION abort;

(*where are these externals defined???*)
EXTERNAL VAR
auto_run: BOOLEAN; (* TRUE => use auto-startup command file *)
auto_startup: BOOLEAN; (* TRUE => terminate at end of control file *)

$PAGE skp_grb, chk_eol, chk_chr

PROCEDURE skp_grb;
(*skip over garbage (blanks) in command line*)
BEGIN
  cmd_skip_blanks(cmd, idx)
END;

FUNCTION chk_eol: BOOLEAN;
(*return TRUE if at end of command line*)
BEGIN
  chk_eol := cmd_eol(cmd, idx)
END;

FUNCTION chk_chr(ch: CHAR): BOOLEAN;
(*return TRUE if specified character next in command line, skip over it*)
BEGIN
  chk_chr := cmd_check_punct(cmd, idx, ch)
END;

$PAGE show_command_files, show_command_line, err

PROCEDURE show_command_files;
(*show command files in stack*)
VAR tmp: cmd_stack;
BEGIN
  tmp := cmd_name_stack;
  IF tmp <> NIL THEN BEGIN
    ttymsg('error in command file ' || tmp^.cmd_file_name); tmp := tmp^.next
  END;
  WHILE tmp <> NIL DO BEGIN
    ttymsg('from command file ' || tmp^.cmd_file_name); tmp := tmp^.next
  END
END;

PROCEDURE show_command_line;
(*show current command line, with pointer at current token*)
BEGIN
  REWRITE(TTYOUTPUT);
  IF cmd_name_stack <> NIL THEN WRITELN(TTYOUTPUT, ' ': LENGTH(prompt), cmd);
  WRITELN(TTYOUTPUT, ' ': LENGTH(prompt) + idx - 1, '^');
  CLOSE(TTYOUTPUT)
END;

$PAGE err

PROCEDURE err(code: errs);
(*reports an error in a command line and signals "abort"*)
BEGIN
  show_command_files; show_command_line;
  CASE code OF
    bad_syn: ttymsg('?command line syntax error');
    bad_src_fil: ttymsg('?cannot open source file');
    lin_too_big: ttymsg('?command line too long');
    bad_ind_fil: ttymsg('?cannot open indirect command file');
    bad_chk_opt: ttymsg('?invalid CHECK option');
    bad_spc_opt: ttymsg('?invalid SPECIAL option');
    bad_opt: ttymsg('?invalid option');
    not_cmd_opt: ttymsg('?option not valid from command line');
    bad_wid: ttymsg('?illegal page width');
    bad_len: ttymsg('?illegal page length');
    bad_alc: ttymsg('?illegal ALLOC code');
    opt_exp: ttymsg('?option keyword expected');
    prn_exp: ttymsg('?value must be enclosed in parentheses');
    val_exp: ttymsg('?option subfield expected');
    fid_exp: ttymsg('?filename expected');
    sw_exp: ttymsg('?switch expected');
    dbg_opt: ttymsg('?DEBUG and OPTIMIZE are incompatible');
    imd_exp: ttymsg('?immediate command expected');
    bad_tm_cod: ttymsg('?invalid target machine code');
    ovl_msg: ttymsg('?OVERLAY and MAINSEG are incompatible');
    defid_exp: ttymsg('?filename default expected');
    no_hlp: ttymsg('?no help is available');
    no_std: ttymsg('?option STANDARD not implemented')
  END;
  ttymsg(''); SIGNAL(abort)
END;

$PAGE get_cmd

PROCEDURE get_cmd;
(*get a command line from the saved list or the terminal*)
VAR nxt_cmd: pending_command; stk_next: cmd_stack; too_long: BOOLEAN;
BEGIN
  WHILE (cmd_list <> NIL) ANDIF (cmd_list^.eof) DO BEGIN
    nxt_cmd := cmd_list^.next; DISPOSE(cmd_list);
    cmd_list := nxt_cmd; stk_next := cmd_name_stack^.next;
    DISPOSE(cmd_name_stack); cmd_name_stack := stk_next
  END;
  IF cmd_list = NIL THEN BEGIN (*get command from terminal*)
    IF auto_startup THEN done := TRUE
    ELSE BEGIN
      OPEN(TTY); REWRITE(TTYOUTPUT);
      cmd_getline(prompt, cmd, idx); cmd := UPPERCASE(cmd);
      CLOSE(TTY); CLOSE(TTYOUTPUT)
    END
  END
  ELSE BEGIN (*get command from list*)
    cmd := UPPERCASE(cmd_list^.text);
    too_long := cmd_list^.too_long; nxt_cmd := cmd_list^.next;
    DISPOSE(cmd_list); cmd_list := nxt_cmd;
    IF too_long THEN err(lin_too_big)
  END
END;

$PAGE get_fil_nam, get_num

FUNCTION get_fil_nam: FILE_NAME;
BEGIN
  IF NOT cmd_file_name(cmd, idx, true, get_fil_nam) THEN err(fid_exp)
END;

FUNCTION get_num(default: INTEGER): INTEGER;
(*returns the value of a numeric parameter*)
(*"default" is the value to be returned if the parameter is omitted.
  If "default" is negative, then an error message will be printed if
  the parameter is omitted.*)
VAR punct: (paren, colon, none);
BEGIN
  IF chk_chr(':') ORIF chk_chr('=') THEN punct := colon
  ELSE IF chk_chr('(') THEN punct := paren
  ELSE punct := none;
  IF NOT cmd_number(cmd, idx, FALSE, get_num) THEN BEGIN
    IF (default >= 0) AND (punct = none) THEN get_num := default
    ELSE err(val_exp)
  END;
  IF (punct = paren) ANDIF NOT chk_chr(')') THEN err(prn_exp)
END;

$PAGE switch_list, fil_lst

FUNCTION switch_list(head: sw_ptr; enable: BOOLEAN): sw_ptr;
VAR switch_name: sw_str; paren: BOOLEAN;
BEGIN
  switch_list := head;
  IF chk_chr(':') ORIF chk_chr('=') THEN paren := FALSE
  else paren := chk_chr('(');
  REPEAT
    IF NOT cmd_token(cmd, idx, ['A'..'Z', '0'..'9', '_'], switch_name)
    THEN err(sw_exp);
    switch_list := enasw(switch_list, switch_name, enable)
  UNTIL NOT paren ORIF NOT chk_chr(',');
  IF paren ANDIF NOT chk_chr(')')
  THEN err(prn_exp)
END;

FUNCTION fil_lst(lst: srch_lst): srch_lst;
(*parse and add names to search list*)
VAR paren: BOOLEAN; nam: FILE_NAME;
BEGIN
  fil_lst := lst;
  IF chk_chr(':') ORIF chk_chr('=') THEN paren := FALSE
  ELSE paren := chk_chr('(');
  REPEAT
    IF NOT cmd_file_name(cmd, idx, FALSE, nam) THEN err(defid_exp);
    fil_lst := newsrch(fil_lst, nam);
  UNTIL NOT paren ORIF NOT chk_chr(',');
  IF paren AND NOT chk_chr(')') THEN err(prn_exp)
END;

$PAGE tm_nam

FUNCTION tm_nam(prefix: STRING[*]): parm_string;
(*will return the target machine name associated with a particular
  target machine code. *)
VAR tmidx: tm;
BEGIN
  tm_nam := '';
  FOR tmidx := MINIMUM(tm) TO MAXIMUM(tm)
  DO EXIT IF tmnams[tmidx].prefix = prefix DO tm_nam := tmnams[tmidx].nam
END;

$PAGE load_env

PROCEDURE load_env(env_file: FILE_NAME; prefix: fstr_3);
(*load specified environment file*)
(*If the load fails, it will print an error message and attempt to
  reload the old environment.*)
VAR save_file: FILE OF *; save_options: command_options;
  save_auto: BOOLEAN; save_startup: BOOLEAN; save_log: log_file_record;
BEGIN
  IF NOT wrpas(pas_tmp)
  THEN byebye('?unable to write environment to temporary file ' || pas_tmp);
  REWRITE(save_file, lde_tmp);
  cmd_save(save_file);
  wrsw(default_options.switches, save_file);
  wrsw(default_options.dump_switches, save_file);
  wrsrch(default_options.search_list, save_file);
  CLOSE(save_file);
  save_options := default_options; save_auto := auto_run;
  save_startup := auto_startup; save_log := log_record;
  IF rdpas('.ENV ' || env_file, FALSE) THEN BEGIN
    IF env_name = NIL
    THEN ttymsg('[' || tm_nam(tmprefix) || ' initial environment]')
    ELSE ttymsg('[' || tm_nam(tmprefix) || ' environment ' ||
      env_name^.text || ' created ' || SUBSTR(dc_ext(env_dtime),1,15) || ']');
    default_options := save_options; auto_run := save_auto;
    auto_startup := save_startup; log_record := save_log;
    RESET(save_file, lde_tmp);
    cmd_restore(save_file);
    default_options.switches := rdsw(save_file);
    default_options.dump_switches := rdsw(save_file);
    default_options.search_list := rdsrch(save_file);
    SCRATCH(save_file)
  END
  ELSE BEGIN
    IF prefix = '' THEN ttymsg('?unable to load environment ' || env_file)
    ELSE ttymsg('?unable to load initial envoronment ' || tm_nam(prefix));
    IF NOT rdpas(pas_tmp, FALSE)
    THEN byebye('?unable to reload old environment');
  END
END;

$PAGE check_sub_options

PROCEDURE check_sub_options(set_opt_on: BOOLEAN);
(*process CHECK sub-options*)
TYPE check_set = SET OF checklist;
CONST check_all: check_set = [MINIMUM(checklist)..MAXIMUM(checklist)];
VAR opt_set: check_set; sub_opt: INTEGER; paren: BOOLEAN;
BEGIN
  IF chk_eol() THEN opt_set := check_all
  ELSE IF chk_chr(',') ORIF chk_chr('/')
  THEN BEGIN opt_set := check_all; idx := idx - 1 END
  ELSE BEGIN
    opt_set := [];
    IF chk_chr(':') ORIF chk_chr('=') THEN paren := FALSE
    ELSE paren := chk_chr('(');
    REPEAT
      IF NOT cmd_lookup(cmd, idx, ['A'..'Z'], opdcot_chk_opt_table, sub_opt)
      THEN err(bad_chk_opt);
      opt_set := opt_set + [opdmtc_map_to_chk_opt[chk_opts(sub_opt)]]
    UNTIL NOT paren ORIF NOT chk_chr(',');
    IF paren ANDIF NOT chk_chr(')') THEN err(prn_exp)
  END;
  WITH prog_options
  DO IF set_opt_on
  THEN semantic_options := semantic_options - check_all + opt_set
  ELSE semantic_options := semantic_options - opt_set
END;

$PAGE special_sub_options

PROCEDURE special_sub_options(set_opt_on: BOOLEAN);
(*process SPECIAL sub-options*)
TYPE special_set = SET OF speciallist;
CONST special_all: special_set = [MINIMUM(speciallist)..MAXIMUM(speciallist)];
VAR opt_set: special_set; sub_opt: INTEGER; paren: BOOLEAN;
BEGIN
  IF chk_eol() THEN opt_set := special_all
  ELSE IF chk_chr(',') ORIF chk_chr('/')
  THEN BEGIN opt_set := special_all; idx := idx - 1 END
  ELSE BEGIN
    opt_set := [];
    IF chk_chr(':') ORIF chk_chr('=') THEN paren := FALSE
    ELSE paren := chk_chr('(');
    REPEAT
      IF NOT cmd_lookup(cmd, idx, ['A'..'Z'], opdsot_sp_opt_table, sub_opt)
      THEN err (bad_spc_opt);
      opt_set := opt_set + [opdmts_map_to_sp_opt [sp_opts (sub_opt)]];
    UNTIL NOT paren ORIF NOT chk_chr(',');
    IF paren ANDIF NOT chk_chr(')') THEN err(prn_exp)
  END;
  WITH prog_options 
  DO IF set_opt_on
  THEN semantic_options := semantic_options + opt_set
  ELSE semantic_options := semantic_options - opt_set
END;

$PAGE process_options

PROCEDURE process_options;
(*process command line options*)
(*The scanning cursor points to the character past the '/' when entered.
  On exit, prog_options contain the derived options.*)
VAR ix, val: INTEGER; opt_ix: option_scalar; set_opt_on, set_opt_auto: BOOLEAN;
BEGIN
  WITH prog_options DO BEGIN
    REPEAT (*at least one option expected at start*)
      skp_grb;
      IF SUBSTR(cmd, idx, MIN(length(cmd)-idx+1, 2)) = 'NO' THEN BEGIN
	set_opt_on := FALSE; set_opt_auto := FALSE; idx := idx + 2
      END
      ELSE IF SUBSTR(cmd, idx, MIN(length(cmd)-idx+1, 4)) = 'AUTO' THEN BEGIN
	set_opt_on := TRUE; set_opt_auto := TRUE; idx := idx + 4
      END
      ELSE BEGIN set_opt_on := TRUE; set_opt_auto := FALSE END;
      IF NOT cmd_lookup(cmd, idx, ['A'..'Z'], opdotb_option_table, ix)
      THEN err(opt_exp);
      opt_ix := option_scalar(ix);
      IF (NOT set_opt_on AND NOT (opt_ix IN opdnoo_no_options)) OR
	(set_opt_auto AND NOT (opt_ix IN opdauo_auto_options))
      THEN err(bad_opt);
      CASE opt_ix OF
	opt_terse: terse_opt := TRUE;
	opt_verbose: terse_opt := FALSE;
	opt_length: BEGIN
	  val := get_num(-1); IF (val < 4) OR (val > 255) THEN err(bad_len);
	  page_length := val
	END;
	opt_width: BEGIN
	  val := get_num(-1); IF (val < 20) OR (val > 255) THEN err(bad_wid);
	  page_width := val;
	END;
	opt_search: BEGIN
	  zapsrch(search_list);
	  IF set_opt_on THEN search_list := fil_lst(search_list)
	END;
	opt_enable: switches := switch_list(switches, TRUE);
	opt_disable: switches := switch_list(switches, FALSE);
	opt_dump: dump_switches := switch_list(dump_switches, set_opt_on);

$PAGE

	opt_source: BEGIN
	  IF set_opt_auto THEN source_opt := opt_is_auto
	  ELSE IF set_opt_on THEN source_opt := opt_is_on
	  ELSE source_opt := opt_is_off
	END;
	opt_lsystem: lsys_opt := set_opt_on;
	opt_errors: errors_opt := set_opt_on;
	opt_quick: BEGIN
	  IF set_opt_auto THEN quick_opt := opt_is_auto
	  ELSE IF set_opt_on THEN quick_opt := opt_is_on
	  ELSE quick_opt := opt_is_off
	END;
	opt_banner: banner_opt := set_opt_on;
	opt_kicode: ki_code_opt := set_opt_on;
	opt_code: code_opt := set_opt_on;
	opt_finish: finish_opt := set_opt_on;
	opt_statistics: statistics_opt := set_opt_on;
	opt_names: names_opt := set_opt_on;
	opt_global: global_opt := set_opt_on;
	opt_check: check_sub_options(set_opt_on);
	opt_trace .. opt_optimize: BEGIN
	  IF (opt_ix = opt_optimize) AND set_opt_on AND debug_opt
	  THEN err(dbg_opt);
	  IF set_opt_on
	  THEN semantic_options :=
	    semantic_options + [opdmto_map_to_optionlist[opt_ix]]
	  ELSE semantic_options :=
	    semantic_options - [opdmto_map_to_optionlist[opt_ix]]
	END;
	opt_special: special_sub_options(set_opt_on);
	opt_overlay: BEGIN
	  IF set_opt_on AND mainseg_opt THEN err(ovl_msg);
	  overlay_opt := set_opt_on
	END;
	opt_mainseg: BEGIN
	  IF set_opt_on AND overlay_opt THEN err(ovl_msg);
	  mainseg_opt := set_opt_on
	END;
	opt_debug: BEGIN
	  IF set_opt_on AND (optimize_opt IN semantic_options)
	  THEN err(dbg_opt);
	  debug_opt := set_opt_on
	END;
	opt_masking: masking_opt := set_opt_on;
	opt_underflow: underflow_opt := set_opt_on;
	opt_storage: storage := get_num(-1);

$PAGE

	opt_alloc: BEGIN
	  val := get_num(-1); IF val > 99 THEN err(bad_alc);
	  alloc_mode := val
	END;
	opt_standard: err(no_std);
	opt_exit: done := true;
	opt_run: BEGIN
	  IF chk_chr(':') THEN run_file := get_fil_nam ()
	  ELSE err(fid_exp)
	END;
	opt_runoffset: run_offset := get_num(1);
	opt_help: IF NOT hlp(cmd, idx, pdr(hlp_txt)) THEN err(no_hlp);
	others: err(not_cmd_opt)
      END;
    UNTIL NOT (chk_chr(',') ORIF chk_chr('/'));
  END
END;

$PAGE do_options_command, do_command_file, do_immediate_command

PROCEDURE do_options_command;
(*set new default options*)
BEGIN
  process_options; IF NOT chk_eol() THEN err(bad_syn);
  default_options := prog_options
END;

PROCEDURE do_command_file;
(*process an "@" command file*)
VAR nam: FILE_NAME; fil: TEXT;
BEGIN
  nam := get_fil_nam(); IF NOT chk_eol() THEN err(bad_syn);
  RESET(fil, '.CCL ' || nam);
  IF IOSTATUS <> IO_OK THEN RESET(fil, '.CMD ' || nam);
  IF IOSTATUS <> IO_OK THEN RESET(fil, '.COM ' || nam);
  IF IOSTATUS <> IO_OK THEN err(bad_ind_fil);
  rd_cmd_file(fil); CLOSE(fil)
END;

PROCEDURE do_immediate_command;
(*reads and executes a ":" command. *)
VAR new_prefix: parm_string; imd_idx: INTEGER;
BEGIN
  IF NOT cmd_lookup(cmd, idx, ['A'..'Z'], opdict_imd_cmd_table, imd_idx)
  THEN err(imd_exp);
  CASE imd_commands(imd_idx) OF
    imd_environment: load_env(get_fil_nam(), '');
    imd_target: BEGIN
      IF NOT cmd_token(cmd, idx, ['A'..'Z', '0'..'9'], new_prefix) ORIF
	(tm_nam(new_prefix) = '')
      THEN err(bad_tm_cod);
      load_env(pdr(new_prefix || 'INI.ENV'), new_prefix)
    END
  END;
  IF NOT chk_eol() THEN err(bad_syn)
END;

$PAGE do_compilation_command

PROCEDURE do_compilation_command;
(*process files to compile*)
VAR mfidx: INTEGER;
PROCEDURE get_main_file;
BEGIN skp_grb; mfidx := idx; main_file := get_fil_nam() END;
BEGIN
  rel_file := ''; list_file := ''; list_explicit := false;
  IF chk_chr('=') THEN get_main_file (*=<source>*)
  ELSE IF chk_chr(',') THEN BEGIN (*,[<list>]=<source>*)
    IF chk_chr('=') THEN get_main_file (*,=source*)
    ELSE BEGIN (*,<list>=<source>*)
      list_file := get_fil_nam (); list_explicit := true;
      IF NOT chk_chr('=') THEN err(bad_syn);
      get_main_file
    END
  END
  ELSE BEGIN
    get_main_file;
    IF chk_chr('=') THEN BEGIN (*<rel>=<source>*)
      rel_file := main_file; get_main_file
    END
    ELSE IF chk_chr(',') THEN BEGIN (*<rel>,[<list>]=<source>*)
      rel_file := main_file;
      IF chk_chr('=') THEN get_main_file (*<rel>,=<source>*)
      ELSE BEGIN (*<rel>,<list>=<source>*)
        list_file := get_fil_nam(); list_explicit := true;
        IF NOT chk_chr('=') THEN err(bad_syn);
        get_main_file;
      END
    END
    ELSE BEGIN (*<source>*)
    (*this seems to be dependant upon PDP10 environment*)
      rel_file := SUBSTR(main_file, SEARCH(main_file, [':', ')']) + 1);
      rel_file := SUBSTR(rel_file, 1,
        SEARCH(rel_file, ['[', '.'], LENGTH(rel_file) + 1) - 1);
      list_file := rel_file;
    END
  END;
  IF chk_chr('/') THEN process_options;
  IF NOT chk_eol() THEN err(bad_syn);
  IF NOT opnsrc(prog_options.search_list, INPUT, '.PAS ' || main_file)
  THEN BEGIN idx := mfidx; err(bad_src_fil) END
  ELSE CLOSE(INPUT)
END;

$PAGE process_command 

PROCEDURE process_command;
(*read and process command lines*)
VAR was_err: BOOLEAN;
BEGIN
  REPEAT
    done := false; main_file := ''; run_file := ''; run_offset := 0;
    was_err := FALSE; prog_options := default_options;
    BEGIN
      get_cmd; idx := 1;
      IF done ORIF chk_eol() THEN done := TRUE
      ELSE IF chk_chr('@') THEN do_command_file
      ELSE IF chk_chr(':') THEN do_immediate_command
      ELSE IF chk_chr('/') THEN do_options_command
      ELSE do_compilation_command
      EXCEPTION
      abort: was_err := TRUE
    END
  UNTIL NOT was_err ANDIF (done OR (run_file <> '') OR (main_file <> ''));
END;

$PAGE zaptmp, inimsg, inicmd

PROCEDURE zap_tmp;
(*deletes any outstanding temporary files*)
VAR tmp: TEXT;
BEGIN
  RESET(tmp, pas_tmp); SCRATCH(tmp);
  RESET(tmp, ini_tmp); SCRATCH(tmp);
  RESET(tmp, xrf_tmp); SCRATCH(tmp);
  RESET(tmp, err_tmp); SCRATCH(tmp);
  RESET(tmp, lde_tmp); SCRATCH(tmp)
END;

PROCEDURE dpy_fil(nam: FILE_NAME);
(*display text file to terminal*)
VAR lin: STRING[80]; fil: TEXT;
BEGIN
ET(fil, nam); REWRITE(TTYOUTPUT);
  IF IOSTATUS = IO_OK THEN WHILE NOT EOF(fil) DO BEGIN
    READLN(fil, lin); WRITELN(TTYOUTPUT, lin)
  END;
  CLOSE(fil); CLOSE(TTYOUTPUT)
END;

PROCEDURE initial_commands;
(*get initial commands from tmpcor or tmpfil, if any*)
VAR buf: tmp_buffer; len: tmpcor_length; tcr: BOOLEAN; fil: TEXT;
BEGIN
  len := 0;
  (*tmpcor is dependant upon the PDP10 environment*)
  tcr := tmpcor('PAS', tmpcor_rf, 0, len); (*look for a tmpcor file*)
  IF tcr THEN BEGIN (*if its there, read and delete it*)
    NEW(buf, len); tcr := tmpcor('PAS', tmpcor_df, ORD(buf), len)
  END;
  IF tcr THEN BEGIN (*read commands from tmpcor file*)
    log_record.opt_tmpcor := TRUE; rd_tmpcor('TMP:PAS', buf, len)
  END
  ELSE BEGIN (*no tmpcor, check for tmpfil*)
    RESET(fil, cmd_tmp);
    IF NOT EOF(fil) THEN BEGIN (*read commands from temp file*)
      log_record.opt_hash := TRUE; rd_cmd_file(fil); SCRATCH(fil);
    END
    ELSE auto_startup := FALSE (*no commands -- forget auto-startup*)
  END
END;

$PAGE start_compiler, main program

PROCEDURE start_compiler;
(*start up the compiler*)
VAR tmp: FILE OF *;
BEGIN
  RESET(tmp, pas_tmp);
  IF (IOSTATUS = IO_OK) ANDIF (NOT EOF(tmp)) THEN BEGIN (*continue*)
    CLOSE(tmp);
    IF NOT rdpas(ini_tmp, TRUE)
    THEN byebye('?lost initial environment file ' || ini_tmp)
  END
  ELSE BEGIN (*first time through--set up initial environment*)
    CLOSE(tmp);
    ttymsg(current_version);
    IF NOT rdpas(pdr(env_fil), FALSE)
    THEN byebye('?unable to load initial environment ' || pdr(env_fil));
    prog_options := default_options;
    newlog;
    auto_startup := auto_run;
    IF auto_startup THEN initial_commands;
    IF NOT auto_startup THEN dpy_fil(pdr(msg_txt));
    ttymsg('')
  END
END;

BEGIN
  start_compiler;
  process_command;
  prog_options.switches := enasw(prog_options.switches, tmprefix, TRUE);
  IF done THEN zap_tmp
  ELSE IF run_file <> '' THEN BEGIN
    zap_tmp;
    run('SYS: ' || run_file, run_offset);
    byebye('?unable to run SYS: ' || run_file)
  END
  ELSE BEGIN
    IF NOT wrpas(ini_tmp)
    THEN byebye('?unable to write initial environment to file ' || ini_tmp)
    ELSE chain('PASANL')
  END
END.
b@C