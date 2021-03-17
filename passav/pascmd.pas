$TITLE PASCMD -- Pascal Compiler Command Line Processor

program pascmd;
$PAGE declarations
$SYSTEM pascal
$SYSTEM ptmcon
$SYSTEM pasfil
$SYSTEM pasist
$SYSTEM paslog
$SYSTEM passw
$SYSTEM pasdat
$SYSTEM pascfm
$SYSTEM pasopn
$SYSTEM pasopd
$SYSTEM prgdir
$SYSTEM dtime
$SYSTEM tmpnam

external var
    auto_run: boolean; (* TRUE => use auto-startup command file *)
    auto_startup: boolean; (* TRUE => terminate at end of control file *)

external procedure run (packed array [1..*] of char; integer);
$PAGE read_command_line 
(* READ COMMAND LINE reads and processes command lines.  It performs one of
   four actions:  (1) In case of a syntactic or other error in the command line,
   it reads another line.  (2)  In the case of a blank line (a quit command), it
   halts compilation.  (3) In the case of a global option setting, the setting
   is performed, and another line read. (4) When a full compilation line is read,
   it returns to start the compilation. *)

procedure read_command_line;

const 
    prompt = '*';

var cmd_line: string [256];     (* command line as read *)
    clidx: integer; (* scanning cursor for cmd_line *)
    exit_flag: boolean;	(* if /EXIT *)
    run_file_name: file_name;	(* from /RUN *)
    run_offset: integer;	(* from /RUNOFFSET *)

label 1 (* abort *) ;           (* exit from cmd_error into command loop *)
$PAGE cmd_error
(* CMD ERROR reports an error in a command line and returns to the command loop
   by means of a nonlocal goto. *)

type error_codes =
       ( bad_syntax, bad_source_file, line_too_long, bad_indirect_file,
	 bad_check_opt, bad_special_opt, bad_option, not_cmd_option, bad_width,
	 bad_length, bad_alloc, opt_expected, paren_expected, val_expected,
	 fid_expected, sw_expected, debug_optimize, imd_expected,
	 bad_tm_code, overlay_mainseg, defid_expected, no_help_file  );


procedure cmd_error (error: error_codes);
var cmd_file: cmd_stack;
begin
  cmd_file := cmd_name_stack;
  while cmd_file <> nil do begin
    writeln (tty, ' ':length(prompt), 'In file: ', cmd_file^.cmd_file_name);
    cmd_file := cmd_file^.next;
  end;
  if cmd_name_stack <> nil then
    writeln (tty, ' ':length(prompt), cmd_line);
  writeln (tty, ' ':length (prompt)+clidx-1, '^');

  case error of
    bad_syntax:
      writeln (tty, '?Command line syntax error.');
    bad_source_file:
      writeln (tty, '?Cannot open source file.');
    line_too_long:
      writeln (tty, '?Command line too long.');
    bad_indirect_file:
      writeln (tty, '?Cannot open indirect command file.');
    bad_check_opt:
      writeln (tty, '?Invalid CHECK option.');
    bad_special_opt:
      writeln (tty, '?Invalid SPECIAL option.');
    bad_option:
      writeln (tty, '?Invalid option.');
    not_cmd_option:
      writeln (tty, '?Option not valid from command line.');
    bad_width:
      writeln (tty, '?Illegal page width .');
    bad_length:
      writeln (tty, '?Illegal page length.');
    bad_alloc:
      writeln (tty, '?Illegal ALLOC code.');
    opt_expected:
      writeln (tty, '?Option keyword expected.');
    paren_expected:
      writeln (tty, '?Option value must be enclosed in parentheses.');
    val_expected:
      writeln (tty, '?Option subfield expected.');
    fid_expected:
      writeln (tty, '?Filename expected.');
    sw_expected:
      writeln (tty, '?Switch expected.');
    debug_optimize:
      writeln (tty, '?DEBUG and OPTIMIZE are incompatible options.');
    imd_expected:
      writeln (tty, '?Immediate command expected.');
    bad_tm_code:
      writeln (tty, '?Invalid target machine code.');
    overlay_mainseg:
      writeln (tty, '?OVERLAY and MAINSEG are incompatible options');
    defid_expected:
      writeln (tty, '?Filename default expected');
    no_help_file:
      writeln (tty, '?No help information is available');
  end;
  writeln (tty);
  break (tty);
  goto 1; (* try again *)
end; (* cmd_error *)
$PAGE wrapup
(* WRAPUP deletes any outstanding temporary files. *)

procedure wrapup;

var
  temp_file: text;
  
begin
  reset (temp_file, tempname ('PA0'));
  scratch (temp_file);
  reset (temp_file, tempname ('XRF'));
  scratch (temp_file);
  reset (temp_file, tempname ('ERR'));
  scratch (temp_file);
end;
$PAGE get_line
(*  GET_LINE returns a command line from the saved list or the tty.  *)

procedure get_line;

var cmd_next: pending_command;
    stk_next: cmd_stack;
    too_long: boolean;

begin
  while (cmd_list <> nil) andif (cmd_list^.eof) do begin
    cmd_next := cmd_list^.next;
    dispose (cmd_list);
    cmd_list := cmd_next;
    stk_next := cmd_name_stack^.next;
    dispose (cmd_name_stack);
    cmd_name_stack := stk_next;
  end;

  if cmd_list = nil then begin
    if auto_startup then begin (* Command file finished - terminate. *)
      wrapup;
      stop;
    end

    else begin
      cmd_getline (prompt, cmd_line, clidx);
      cmd_line := uppercase (cmd_line);
    end;
  end

  else (* cmd_list <> nil *) begin
    cmd_line := uppercase (cmd_list^.text);
    too_long := cmd_list^.too_long;
    cmd_next := cmd_list^.next;
    dispose (cmd_list);
    cmd_list := cmd_next;
    if too_long then
      cmd_error (line_too_long);
  end;
end (* get_line *);
$PAGE get_file_name
function get_file_name: file_name;

begin
  if not cmd_file_name (cmd_line, clidx, true, get_file_name) then
    cmd_error (fid_expected);
end;
$PAGE numeric_value
(* NUMERIC VALUE returns the value of a numeric parameter.  Default is
   the value to be returned if the parameter is omitted.  If Default is
   negative, then an error message will be printed if the parameter is
   omitted. *)

function numeric_value (default: integer): integer;

var punct: (paren, colon, none);

begin
  if cmd_check_punct (cmd_line, clidx, ':') orif cmd_check_punct (cmd_line, clidx, '=') then
    punct := colon
  else if cmd_check_punct (cmd_line, clidx, '(') then
    punct := paren
  else
    punct := none;
  if not cmd_number (cmd_line, clidx, false, numeric_value) then begin
    if (default >= 0) and (punct = none) then
      numeric_value := default
    else
      cmd_error (val_expected);
  end;
  if (punct = paren) andif not cmd_check_punct (cmd_line, clidx, ')') then
    cmd_error (paren_expected);
end;
$PAGE switchlist
function switchlist (head: switch_ptr; enable: boolean): switch_ptr;

var switch_name: switch_string;
    paren: boolean;

begin
  switchlist := head;
  if cmd_check_punct (cmd_line, clidx, ':') orif cmd_check_punct (cmd_line, clidx, '=')
    then paren := false
    else paren := cmd_check_punct (cmd_line, clidx, '(');
  repeat
    if not cmd_token (cmd_line, clidx, ['A'..'Z', '0'..'9', '_'], switch_name) then
      cmd_error (sw_expected);
    switchlist := enable_switch (switchlist, switch_name, enable);
  until not paren orif not cmd_check_punct (cmd_line, clidx, ',');
  if paren andif not cmd_check_punct (cmd_line, clidx, ')') then
    cmd_error (paren_expected);
end;
$PAGE filelist
function filelist: search_ptr;

var current, last: search_ptr;
    paren: boolean;
    tempname: file_name;

begin
  filelist := nil;
  if cmd_check_punct (cmd_line, clidx, ':') orif cmd_check_punct (cmd_line, clidx, '=')
    then paren := false
    else paren := cmd_check_punct (cmd_line, clidx, '(');
  repeat
    if not cmd_file_name (cmd_line, clidx, false, tempname) then
      cmd_error (defid_expected);
    new (current, length (tempname));
    current^.next := nil;
    current^.name := tempname;
    if filelist = nil
      then filelist := current
      else last^.next := current;
    last := current;
  until not paren orif not cmd_check_punct (cmd_line, clidx, ',');
  if paren and not cmd_check_punct (cmd_line, clidx, ')') then
    cmd_error (paren_expected);
end;
$PAGE sea_save
procedure sea_save ( list: search_ptr; var f: file of * );

var ll: search_ptr;
    len: integer;

begin
  ll := list;
  while ll <> nil do begin
    len := length (ll^.name);
    write (f, len, ll^: size (ll^, len));
    ll := ll^.next;
  end;
  len := 0;
  write (f, len);
end;
$PAGE sea_load
function sea_load ( var f: file of * ): search_ptr;

var sea, last_sea: search_ptr;
    len: integer;

begin
  sea_load := nil;
  loop
    read (f, len);
  exit if len = 0;
    new (sea, len);
    read (f, sea^: size (sea^, len));
    sea^.next := nil;
    if sea_load = nil
      then sea_load := sea
      else last_sea^.next := sea;
    last_sea := sea;
  end;
end;
$PAGE tmname
(* TMNAME will return the target machine name associated with a particular
   target machine code. *)

function tmname ( prefix: parm_string ): parm_string;

type
    target_machines = ( tm_p10, tm_vax, tm_m68, tm_none );
    tm_list = array [target_machines] of record
        prefix: fstr_3;
        name: string [10]
    end;

const
    tm_names: tm_list =
    ( ( 'P10', 'PDP 10' ),
      ( 'VAX', 'VAX-11' ),
      ( 'M68', 'M68000' ),
      ( '   ', '' ) );

var tm_idx: target_machines;

begin
  tmname := '';
  for tm_idx := minimum (target_machines) to maximum (target_machines) do
    exit if tm_names[tm_idx].prefix = prefix do
      tmname := tm_names[tm_idx].name;
end;
$PAGE load_environment
(* LOAD ENVIRONMENT will load a specified environment file.  If the load
   fails, it will print an error message and attempt to reload the old
   environment. *)

procedure load_environment ( env_file: file_name; prefix: fstr_3 );

var save_file: file of *;
    save_options: command_options;
    save_auto: boolean;
    save_startup: boolean;
    save_log: log_file_record;

begin
  dat_save (tempname ('PA0'));
  rewrite (save_file, tempname ('PSF'));
  cmd_save (save_file);
  sw_save (default_options.switches, save_file);
  sw_save (default_options.dump_switches, save_file);
  sea_save (default_options.search_list, save_file);
  close (save_file);
  save_options := default_options;
  save_auto := auto_run;
  save_startup := auto_startup;
  save_log := log_record;
  if dat_get ('.ENV ' ||env_file, false) then begin
    rewrite (tty);
    if env_name = nil
      then writeln (tty, '[', tmname (tmprefix), ' initial environment]')
      else writeln (tty, '[', tmname (tmprefix), ' environment ', env_name^.text,
                         ' created ', substr (dc_ext (env_dtime), 1, 15), ']');
    default_options := save_options;
    auto_run := save_auto;
    auto_startup := save_startup;
    log_record := save_log;
    reset (save_file, tempname ('PSF'));
    cmd_restore (save_file);
    default_options.switches := sw_load (save_file);
    default_options.dump_switches := sw_load (save_file);
    default_options.search_list := sea_load (save_file);
    scratch (save_file)
  end

  else begin
    rewrite (tty);
    if prefix = ''
      then writeln (tty, '?Unable to load environment ', env_file)
      else writeln (tty, '?Unable to load ', tmname (prefix), ' initial environment');
    if not dat_get (tempname ('PA0'), false) then begin
      rewrite (tty);
      writeln (tty, '?Unable to reload old environment');
      stop;
    end;
    rewrite (tty);
  end;
  open (tty);
end (* load_environment *);
$PAGE process_immediate_command
(* PROCESS IMMEDIATE COMMAND reads and executes a ":" command. *)

procedure process_immediate_command;

var new_prefix: parm_string;
    imd_idx: integer;

begin
  if not cmd_lookup (cmd_line, clidx, ['A'..'Z'], opdict_imd_cmd_table, imd_idx) then
    cmd_error (imd_expected);
  case imd_commands (imd_idx) of

    imd_environment:
      load_environment (get_file_name (), '');

    imd_target:
      begin
	if not cmd_token (cmd_line, clidx, ['A'..'Z', '0'..'9'], new_prefix) orif
	   (tmname (new_prefix) = '') then
	  cmd_error (bad_tm_code);
        load_environment (new_prefix || 'INI.ENV' || prgm_dir (), new_prefix);
      end

  end (* case *);

  if not cmd_eol (cmd_line, clidx) then
    cmd_error (bad_syntax);
end (* process_immediate_command *);
$PAGE check_sub_options
procedure check_sub_options (positive: boolean);

type
    check_set = set of checklist;

const
    check_all: check_set = [ minimum(checklist) .. maximum(checklist) ];

var opt_set: check_set;
    sub_opt: integer;
    paren: boolean;

begin
  if cmd_eol (cmd_line, clidx) then
    opt_set := check_all
  else if cmd_check_punct (cmd_line, clidx, ',') orif cmd_check_punct (cmd_line, clidx, '/') then begin
    opt_set := check_all;
    clidx := clidx - 1; (* leave the punctuation *)
  end
  else begin
    opt_set := [];
    if cmd_check_punct (cmd_line, clidx, ':') orif cmd_check_punct (cmd_line, clidx, '=')
      then paren := false
      else paren := cmd_check_punct (cmd_line, clidx, '(');
    repeat
      if not cmd_lookup (cmd_line, clidx, ['A'..'Z'], opdcot_chk_opt_table, sub_opt) then
	cmd_error (bad_check_opt);
      opt_set := opt_set + [opdmtc_map_to_chk_opt [chk_opts (sub_opt)]];
    until not paren orif not cmd_check_punct (cmd_line, clidx, ',');
    if paren andif not cmd_check_punct (cmd_line, clidx, ')') then cmd_error (paren_expected);
  end;

  with prog_options do
    if positive
      then semantic_options := semantic_options - check_all + opt_set
      else semantic_options := semantic_options - opt_set;
end;
$PAGE special_sub_options
procedure special_sub_options (positive: boolean);

type
    special_set = set of speciallist;

const
    special_all: special_set = [ minimum(speciallist) .. maximum(speciallist) ];

var opt_set: special_set;
    sub_opt: integer;
    paren: boolean;

begin
  if cmd_eol (cmd_line, clidx) then
    opt_set := special_all
  else if cmd_check_punct (cmd_line, clidx, ',') orif cmd_check_punct (cmd_line, clidx, '/') then begin
    opt_set := special_all;
    clidx := clidx - 1; (* leave the punctuation *)
  end
  else begin
    opt_set := [];
    if cmd_check_punct (cmd_line, clidx, ':') orif cmd_check_punct (cmd_line, clidx, '=')
      then paren := false
      else paren := cmd_check_punct (cmd_line, clidx, '(');
    repeat
      if not cmd_lookup (cmd_line, clidx, ['A'..'Z'], opdsot_sp_opt_table, sub_opt) then
	cmd_error (bad_special_opt);
      opt_set := opt_set + [opdmts_map_to_sp_opt [sp_opts (sub_opt)]];
    until not paren orif not cmd_check_punct (cmd_line, clidx, ',');
    if paren andif not cmd_check_punct (cmd_line, clidx, ')') then cmd_error (paren_expected);
  end;

  with prog_options do
    if positive
      then semantic_options := semantic_options + opt_set
      else semantic_options := semantic_options - opt_set;
end;
$PAGE do_help
procedure do_help;
var help_file: file of *;
    index_cursor, index_entries: integer;
    index: ^ array [1..*] of cmd_lookup_record;
    i: integer;
    code: integer;
    token: string [10];

  procedure print_message (code: integer);
  type
      str = packed array [1..*] of char;
  var len: integer;
      text: packed array [1..80] of char;
  begin
    readrn (help_file, code, len);
    while len <> -1 do begin
      read (help_file, text: size (str, len));
      writeln (tty, substr (text, 1, len));
      read (help_file, len);
    end;
  end;

begin
  reset (help_file, 'PASCAL.HLP' || prgm_dir (), [seekok]);
  if iostatus <> io_ok then begin
    close (help_file);
    cmd_error (no_help_file);
  end;
  read (help_file, index_cursor, index_entries);
  new (index, index_entries);
  readrn (help_file, index_cursor, index^: size (index^, index_entries));

  if cmd_check_punct (cmd_line, clidx, ':') then begin
    if cmd_check_punct (cmd_line, clidx, '*') then begin
      for i := 1 to index_entries do
	print_message (index^[i].code);
    end
    else if cmd_check_punct (cmd_line, clidx, '(') then begin
      repeat
	if cmd_lookup (cmd_line, clidx, ['A'..'Z'], index^, code) then
	  print_message (code)
	else if cmd_token (cmd_line, clidx, ['A'..'Z', '0'..'9', '_'], token) then
	  writeln (tty, '%No information available on ', token)
	else begin
	  print_message (index^[1].code);
	  dispose (index);
	  close (help_file);
	  cmd_error (bad_syntax);
	end;
      until not cmd_check_punct (cmd_line, clidx, ',');
      if not cmd_check_punct (cmd_line, clidx, ')') then
	cmd_error (paren_expected);
    end
    else if cmd_lookup (cmd_line, clidx, ['A'..'Z'], index^, code) then
      print_message (code)
    else if cmd_token (cmd_line, clidx, ['A'..'Z', '0'..'9', '_'], token) then
      writeln (tty, '%No information available on ', token)
    else begin
      print_message (index^[1].code);
      dispose (index);
      close (help_file);
      cmd_error (bad_syntax);
    end
  end
  else
    print_message (index^[1].code);
  dispose (index);
  close (help_file);
end;
$PAGE scan_options
(* SCAN OPTIONS is called to process command line options.  The scanning cursor
   points to the character past the '/' when entered.  On exit, prog_options
   contain the derived options. *)

procedure scan_options;

var ix: integer;
    option_ix: option_scalar;
    positive, automatic: boolean;
    val: integer;
    stemp: search_ptr;

begin
  with prog_options do begin
    repeat                        (* at least one option expected at start *)
      cmd_skip_blanks (cmd_line, clidx);
      if substr (cmd_line, clidx, min (length(cmd_line)-clidx+1, 2)) = 'NO' then
	begin
	positive := false;
	automatic := false;
	clidx := clidx + 2;
      end
      else if substr (cmd_line, clidx, min (length(cmd_line)-clidx+1, 4)) = 'AUTO' then
	begin
	positive := true;
	automatic := true;
	clidx := clidx + 4;
      end
      else begin
	positive := true;
	automatic := false;
      end;

      if not cmd_lookup (cmd_line, clidx, ['A'..'Z'], opdotb_option_table, ix) then
	cmd_error (opt_expected);

      option_ix := option_scalar (ix);

      if ( not positive and not (option_ix in opdnoo_no_options) ) or
	 ( automatic and not (option_ix in opdauo_auto_options) )
	then cmd_error (bad_option);

      case option_ix of

	opt_terse:
	  terse_opt := true;

	opt_verbo	  terse_opt := false;

	opt_length:
	  begin
	    val := numeric_value (-1);
	    if (val < 4) or (val > 255) then cmd_error (bad_length);
	    page_length := val;
	  end;

	opt_width:
	  begin
	    val := numeric_value (-1);
	    if (val < 20) or (val > 255) then cmd_error (bad_width);
	    page_width := val;
	  end;

	opt_search:
	  begin
	    while search_list <> nil do begin
	      stemp := search_list;
	      search_list := search_list^.next;
	      dispose (stemp);
	    end;
	    if positive then
	      search_list := filelist ();
	  end;

	opt_enable:
	  switches := switchlist (switches, true);

	opt_disable:
	  switches := switchlist (switches, false);

	opt_dump:
	  dump_switches := switchlist (dump_switches, positive);

	opt_source:
	  if automatic then
	    source_opt := opt_is_auto
	  else if positive then
	    source_opt := opt_is_on
	  else
	    source_opt := opt_is_off;

	opt_lsystem:
	  lsys_opt := positive;

	opt_errors:
	  errors_opt := positive;

	opt_quick:
	  if automatic then
	    quick_opt := opt_is_auto
	  else if positive then
	    quick_opt := opt_is_on
	  else
	    quick_opt := opt_is_off;

	opt_banner:
	  banner_opt := positive;

	opt_kicode:
	  ki_code_opt := positive;

	opt_code:
	  code_opt := positive;

	opt_finish:
	  finish_opt := positive;

	opt_statistics:
	  statistics_opt := positive;

	opt_names:
	  names_opt := positive;

	opt_global:
	  global_opt := positive;

	opt_check:
	  check_sub_options (positive);

	opt_trace..opt_optimize:
	  begin
	    if (option_ix = opt_optimize) and positive and debug_opt then
	      cmd_error (debug_optimize);
	    if positive
	      then semantic_options := semantic_options + [opdmto_map_to_optionlist [option_ix]]
	      else semantic_options := semantic_options - [opdmto_map_to_optionlist [option_ix]];
	  end;

	opt_special:
	  special_sub_options (positive);

	opt_overlay:
	  begin
	    if positive and mainseg_opt then
	      cmd_error (overlay_mainseg);
	    overlay_opt := positive;
	  end;

	opt_mainseg:
	  begin
	    if positive and overlay_opt then
	      cmd_error (overlay_mainseg);
	    mainseg_opt := positive;
	  end;

	opt_debug:
	  begin
	    if positive and (optimize_opt in semantic_options) then
	      cmd_error (debug_optimize);
	    debug_opt := positive;
	  end;

	opt_masking:
	  masking_opt := positive;

	opt_underflow:
	  underflow_opt := positive;

	opt_storage:
	  storage := numeric_value (-1);

	opt_alloc:
	  begin
	    val := numeric_value (-1);
	    if val > 99 then
	      cmd_error (bad_alloc);
	    alloc_mode := val;
	  end;

	opt_standard:
	  writeln (tty, '%The STANDARD option has not been implemented');

	opt_exit:
	  exit_flag := true;

	opt_run:
	  if cmd_check_punct (cmd_line, clidx, ':') then
	    run_file_name := get_file_name ()
	  else
	    cmd_error (fid_expected);

	opt_runoffset:
	  run_offset := numeric_value (1);

	opt_help:
	  do_help;

	others:
	  cmd_error (not_cmd_option)

      end (* case *) ;

    until not (cmd_check_punct (cmd_line, clidx, ',') orif cmd_check_punct (cmd_line, clidx, '/'));
  end (* with *) ;
end;
$PAGE read_command_line - main routine
var cmd_file_name: file_name;
    cmd_file: text;
    mf_idx: integer;

  procedure get_main_file;
  begin
    cmd_skip_blanks (cmd_line, clidx);
    mf_idx := clidx;
    main_file := get_file_name ();
  end;

begin (* read_command_line *)

1 (* abort *) :         (* reenter command loop after error *)
  repeat
    prog_options := default_options;    (* initialize options from defaults *)
    get_line;
    clidx := 1;                     (* start parse at beginning of line *)

    exit_flag := false;
    run_file_name := '';
    run_offset := 0;
    main_file := '';

    if cmd_eol (cmd_line, clidx) then begin	(* blank line => stop *)
      wrapup;
      stop;
    end;

    if cmd_check_punct (cmd_line, clidx, '@') then begin
      cmd_file_name := get_file_name ();
      if not cmd_eol (cmd_line, clidx) then cmd_error (bad_syntax);
      reset (cmd_file, '.CCL ' || cmd_file_name);
      if iostatus <> io_ok then
	reset (cmd_file, '.CMD ' || cmd_file_name);
      if iostatus <> io_ok then
        cmd_error (bad_indirect_file);
      rd_cmd_file (cmd_file);
      close (cmd_file);
    end

    else if cmd_check_punct (cmd_line, clidx, ':') then
      process_immediate_command

    else if cmd_check_punct (cmd_line, clidx, '/') then begin
      scan_options;
      if not cmd_eol (cmd_line, clidx) then cmd_error (bad_syntax);
      default_options := prog_options;
    end

    else begin      (* have files to compile *)
      rel_file := '';
      list_file := '';
      list_explicit := false;
      if cmd_check_punct (cmd_line, clidx, '=') then (* =source *)
        get_main_file
      else if cmd_check_punct (cmd_line, clidx, ',') then begin (* ,[list]=source *)
        if cmd_check_punct (cmd_line, clidx, '=') then (* ,=source *)
          get_main_file
        else begin (* ,list=source *)
          list_file := get_file_name ();
          list_explicit := true;
          if not cmd_check_punct (cmd_line, clidx, '=') then cmd_error (bad_syntax);
          get_main_file;
        end
      end
      else begin
        get_main_file;
        if cmd_check_punct (cmd_line, clidx, '=') then begin (* rel=source *)
          rel_file := main_file;
          get_main_file;
        end
        else if cmd_check_punct (cmd_line, clidx, ',') then begin (* rel,[list]=source *)
          rel_file := main_file;
          if cmd_check_punct (cmd_line, clidx, '=') then (* rel,=source *)
            get_main_file
          else begin (* rel,list=source *)
            list_file := get_file_name ();
            list_explicit := true;
            if not cmd_check_punct (cmd_line, clidx, '=') then cmd_error (bad_syntax);
            get_main_file;
          end
        end
        else begin (* source *)
	  rel_file := substr (main_file, search (main_file, [':', ')']) + 1);
          rel_file := substr (rel_file, 1,
                              search (rel_file, ['[', '.'], length (rel_file) + 1) - 1);
          list_file := rel_file;
        end
      end;

      if cmd_check_punct (cmd_line, clidx, '/') then scan_options;   (* get options for *this* compilation *)
      if not cmd_eol (cmd_line, clidx) then cmd_error (bad_syntax);
      if not open_search (input, '.PAS ' || main_file) then begin
        clidx := mf_idx;              (* report error at file name *)
        cmd_error (bad_source_file);
      end;
    end (* file case *) ;

    if exit_flag then begin
      wrapup;
      stop;
    end;

    if run_file_name <> '' then begin
      wrapup;
      run ('SYS: ' || run_file_name, run_offset);
    end;
  until main_file <> '';
end (* read_command_line *);
$PAGE pascmd - main program
begin
  if not dat_get (tempname ('PA0'), false) then begin
    rewrite (tty);
    writeln (tty, '?Compiler temporary file PA0 lost');
    stop;
  end;

  open (tty);
  rewrite (tty);

  read_command_line; (* Process a command. *)

  prog_options.switches := enable_switch (prog_options.switches, tmprefix, true);
  dat_save (tempname ('PA0')); (* Now off to Pass 1 to compile a module. *)

  if not switch (prog_options.dump_switches, 'MANUAL') then begin
    run ('PASANL' || prgm_dir (), 1);
    rewrite (tty);
    writeln (tty, '?Unable to run PASANL');
  end;
end (* pascmd *).
    
kË