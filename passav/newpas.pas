$TITLE NEWPAS -- Pascal Compiler Driver Program

program newpas;
$PAGE declarations

$OPTIONS special, nocheck

$INCLUDE pascal.inc
$INCLUDE ptmcon.inc
$INCLUDE pasfil.inc
$INCLUDE paslog.inc
$INCLUDE pasist.inc
$INCLUDE passw.inc
$INCLUDE pasdat.inc
$INCLUDE pascmd.inc
$INCLUDE pasopd.inc
$INCLUDE versio.inc
$INCLUDE prgdir.inc
$INCLUDE dtime.inc
$INCLUDE delren.inc
$INCLUDE tmpnam.inc
$INCLUDE run.inc
$INCLUDE tmpcor.inc

external var
    auto_run: 0 .. 1; (* 0 = run from terminal
                         1 = run from another program *)
    auto_startup: boolean; (* TRUE => terminate at end of control file *)

var cmd_file: text;

external function fileblock ( var text ): filblock;
external function user_ppn: integer;

var save_file: text;
$PAGE read_command_line 
(* READ COMMAND LINE reads and processes command lines.  It performs one of
   four actions:  (1) In case of a syntactic or other error in the command line,
   it reads another line.  (2)  In the case of a blank line (a quit command), it
   halts compilation.  (3) In the case of a global option setting, the setting
   is performed, and another line read. (4) When a full compilation line is read,
   it returns to start the compilation. *)

procedure read_command_line;

  const prompt = '*';

  var  cmd_file_name: file_id;
       cmd_line: line_string;     (* command line as read *)
       clidx: line_index; (* scanning cursor for cmd_line *)
       err_idx: line_index;     (* point in line at which to mark error *)
       mf_idx: line_index;                      (* start of main file name *)

  label 1 (* abort *) ;           (* exit from cmd_error into command loop *)
$PAGE cmd_error - in read_command_line
  (* CMD ERROR reports an error in a command line and returns to the command loop
     by means of a nonlocal goto. *)

  type error_codes =
         ( bad_syntax, bad_source_file, line_too_long, bad_indirect_file,
           bad_check_opt, bad_special_opt, bad_option, not_cmd_option, bad_width,
           bad_length, bad_alloc, opt_expected, paren_expected, val_expected,
           fid_expected, invalid_fid, sw_expected, debug_optimize, imd_expected,
           bad_tm_code, overlay_mainseg  );


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
    writeln (tty, ' ':length (prompt)+err_idx-1, '^');

    case error of
      bad_syntax:
        writeln (tty, '?Command line syntax error.');
      bad_source:
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
      invalid_fid:
        writeln (tty, '?Malformed filename.');
      sw_expected:
        writeln (tty, '?Switch expected.');
      debug_optimize:
        writeln (tty, '?DEBUG and OPTIMIZE are incompatible options.');
      imd_expected:
        writeln (tty, '?Immediate command expected.');
      bad_tm_code:
        writeln (tty, '?Invalid target machine code.');
      overlay_mainseg:
	writeln (tty, '?OVERLAY and MAINSEG are incompatible options')
    end;
    writeln (tty);
    break (tty);
    goto 1; (* try again *)
  end; (* cmd_error *)
$PAGE terminate - in read_command_line

(* TERMINATE deletes any outstanding temporary files and terminates the compiler. *)

procedure terminate;

begin
  del_file (tempname ('PA0'));
  stop;
end;
$PAGE get_line - in read_command_line

  (*  GET_LINE returns a command line from the saved list or the tty.  *)

  procedure get_line;

  var
      cmd_next: pending_command;
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
      if auto_startup then (* Command file finished - terminate. *)
        terminate

      else begin
        write (tty, prompt);
        break (tty);
        readln (tty);
        cmd_line := '';
        while not eoln (tty) do begin
          if length (cmd_line) > upperbound (cmd_line) - 2
            then cmd_error (line_too_long)
            else cmd_line := cmd_line || tty^;
          get (tty);
        end;
      end;
    end

    else (* cmd_list <> nil *) begin
      cmd_line := substr (cmd_list^.text, 1, cmd_list^.len);
      too_long := cmd_list^.too_long;
      cmd_next := cmd_list^.next;
      dispose (cmd_list);
      cmd_list := cmd_next;
      if too_long then
        cmd_error (line_too_long);
    end;
  end (* get_line *);
$PAGE parsing utilities - in read_command_line

  procedure skip_blanks;
  begin
    while (clidx <= length (cmd_line)) andif (cmd_line[clidx] <= ' ')
      do clidx := clidx + 1;
    err_idx := clidx;                   (* normally err at last nonblank *)
  end;

  function checkeol: boolean;
  begin
    skip_blanks;
    checkeol := (clidx > length (cmd_line));
  end;

  function checkpunct (ch: char): boolean;
  begin
    skip_blanks;
    if (clidx <= length (cmd_line)) andif (ch = cmd_line [clidx])
      then begin
        checkpunct := true;
        clidx := clidx + 1;
      end
      else checkpunct := false;
  end;

  function filename: file_name;
    var l: line_index;
  begin
    filename := '';
    skip_blanks;
    if clidx > length (cmd_line)
      then cmd_error (fid_expected);
    l := clidx;
    if not pr_file_id (cmd_line, clidx, filename) then begin
      if l = clidx
        then cmd_error (fid_expected)
        else cmd_error (invalid_fid);
    end;
  end;

$PAGE number, numeric_option - in read_command_line

  function number: integer;
  begin
    if checkeol then cmd_error (val_expected);
    number := 0;
    while (clidx <= length (cmd_line)) andif (cmd_line[clidx] in ['0'..'9']) do begin
      number := (number * 10) + index ('0123456789', cmd_line[clidx]) - 1;
      clidx := clidx + 1;
    end;
  end;



  function numeric_option: integer;
    var l: line_index;
        paren: boolean;
  begin
    if checkpunct (':') orif checkpunct ('=')
      then paren := false
      else paren := checkpunct ('(');
    skip_blanks;
    l := clidx;                   (* remember start of number *)
    numeric_option := number ();
    if paren andif not checkpunct (')') then cmd_error (paren_expected);
    err_idx := l;                 (* caller wants error at number *)
  end;
$PAGE identifier, switchlist - in read_command_line

  function identifier: parm_string;
  begin
    if checkeol then cmd_error (sw_expected);
    identifier := '';
    while (clidx <= length (cmd_line))
      andif (uppercase (cmd_line[clidx]) in ['A'..'Z', '0'..'9', '_']) do begin
      identifier := identifier || uppercase (cmd_line [clidx]);
      clidx := clidx + 1;
    end;
    if identifier = '' then cmd_error (sw_expected);
  end;



  function switchlist (head: switch_ptr; enable: boolean): switch_ptr;
    var switch_name: switch_string;
        paren: boolean;
  begin
    switchlist := head;
    if checkpunct (':') orif checkpunct ('=')
      then paren := false
      else paren := checkpunct ('(');
    repeat
      switch_name := identifier ();
      switchlist := enable_switch (switchlist, switch_name, enable);
    until not paren orif not checkpunct (',');
    if paren andif not checkpunct (')') then cmd_error (paren_expected);
  end;
$PAGE tmname - in read_command_line
(* TMNAME will return the target machine name associated with a particular
   target machine code. *)

function tmname ( prefix: parm_string ): parm_string;

type
    target_machines = ( tm_p10, tm_vax, tm_none );
    tm_list = array [target_machines] of record
        prefix: fstr_3;
        name: string [10]
    end;

const
    tm_names: tm_list =
    ( ( 'P10', 'PDP 10' ),
      ( 'VAX', 'VAX 11' ),
      ( '   ', '' ) );

var tm_idx: target_machines;

begin
  tmname := '';
  for tm_idx := minimum (target_machines) to maximum (target_machines) do
    exit if tm_names[tm_idx].prefix = prefix do
      tmname := tm_names[tm_idx].name;
end;
$PAGE load_environment - in read_command_line
(* LOAD ENVIRONMENT will load a specified environment file.  If the load
   fails, it will print an error message and attempt to reload the old
   environment. *)

procedure load_environment ( env_file: file_id; prefix: fstr_3 );

var save_options: command_options;
    save_auto: 0 .. 2;
    save_startup: boolean;
    save_log: log_file_record;

begin
  dat_save (tempname ('PA0'));
  rewrite (save_file, tempname ('PSF'));
  cmd_save (save_file);
  sw_save (default_options.switches, save_file);
  sw_save (default_options.dump_switches, save_file);
  close (save_file);
  save_options := default_options;
  save_auto := auto_run;
  save_startup := auto_startup;
  save_log := log_record;
  if dat_get ('.ENV ' ||env_file, false) then begin
    if env_name = nil
      then writeln (tty, '[', tmname (tmprefix), ' initial environment]')
      else writeln (tty, '[', tmname (tmprefix), ' environment ',
                         substr (env_name^.text, 1, env_name^.len),
                         ' created ', substr (dc_ext (env_dtime), 1, 15), ']');
    default_options := save_options;
    auto_run := save_auto;
    auto_startup := save_startup;
    log_record := save_log;
    reset (save_file, tempname ('PSF'));
    cmd_load (save_file);
    default_options.switches := sw_load (save_file);
    default_options.dump_switches := sw_load (save_file);
    close (save_file);
    del_file (tempname ('PSF'));
  end

  else begin
    if prefix = ''
      then writeln (tty, '?Unable to load environment ', env_file)
      else writeln (tty, '?Unable to load ', tmname (prefix), ' initial environment');
    if not dat_get (tempname ('PA0'), false) then begin
      writeln (tty, '?Unable to reload old environment');
      stop;
    end;
  end;
end (* load_environment *);
$PAGE process_immediate_command - in read_command_line
(* PROCESS IMMEDIATE COMMAND reads and executes a ":" command. *)

procedure process_immediate_command;

var new_prefix: parm_string;
    imd_idx: imd_commands;

begin
  skip_blanks;
  if not lookup_imd_commands (cmd_line, clidx, opdict_imd_cmd_table,
                              maximum (imd_commands), imd_idx)
    then cmd_error (imd_expected);
  case imd_idx of

    imd_environment:
      load_environment (filename (), '');

    imd_target:
      begin
        new_prefix := identifier ();
        if tmname (new_prefix) = '' then
          cmd_error (bad_tm_code);
        load_environment (new_prefix || 'INI.ENV' || prgm_dir (), new_prefix);
      end

  end (* case *);

  if not checkeol then cmd_error (bad_syntax);
end (* process_immediate_command *);
$PAGE check_sub_options - in read_command_line

  procedure check_sub_options (positive: boolean);
    type
      check_set = set of checklist;
    const
      check_all: check_set = [ minimum(checklist) .. maximum(checklist) ];
    var
      opt_set: check_set;
      sub_opt: chk_opts;
      paren: boolean;
  begin
    if checkeol then
      opt_set := check_all
    else if checkpunct (',') orif checkpunct ('/') then begin
      opt_set := check_all;
      clidx := clidx - 1; (* leave the punctuation *)
    end
    else begin
      opt_set := [];
      if checkpunct (':') orif checkpunct ('=')
        then paren := false
        else paren := checkpunct ('(');
      repeat
        skip_blanks;
        if not lookup_check_opts (cmd_line, clidx, opdcot_chk_opt_table,
                                  maximum (chk_opts), sub_opt) then
          cmd_error (bad_check_opt);
        opt_set := opt_set + [opdmtc_map_to_chk_opt [sub_opt]];
      until not paren orif not checkpunct (',');
      if paren andif not checkpunct (')') then cmd_error (paren_expected);
    end;

    with prog_options do
      if positive
        then semantic_options := semantic_options - check_all + opt_set
        else semantic_options := semantic_options - opt_set;
  end;
$PAGE special_sub_options - in read_command_line

  procedure special_sub_options (positive: boolean);
    type
      special_set = set of speciallist;
    const
      special_all: special_set = [ minimum(speciallist) .. maximum(speciallist) ];
    var
      opt_set: special_set;
      sub_opt: sp_opts;
      paren: boolean;
  begin
    if checkeol then
      opt_set := special_all
    else if checkpunct (',') orif checkpunct ('/') then begin
      opt_set := special_all;
      clidx := clidx - 1; (* leave the punctuation *)
    end
    else begin
      opt_set := [];
      if checkpunct (':') orif checkpunct ('=')
        then paren := false
        else paren := checkpunct ('(');
      repeat
        skip_blanks;
        if not lookup_special_opts (cmd_line, clidx, opdsot_sp_opt_table,
                                  maximum (sp_opts), sub_opt) then
          cmd_error (bad_special_opt);
        opt_set := opt_set + [opdmts_map_to_sp_opt [sub_opt]];
      until not paren orif not checkpunct (',');
      if paren andif not checkpunct (')') then cmd_error (paren_expected);
    end;

    with prog_options do
      if positive
        then semantic_options := semantic_options + opt_set
        else semantic_options := semantic_options - opt_set;
  end;
$PAGE scan_options
  (* SCAN OPTIONS is called to process command line options.  The scanning cursor
     points to the character pass the '/' when entered.  On exit, prog_options
     contain the derived options. *)

  procedure scan_options;

    var
      option_ix: option_scalar;
      positive, automatic: boolean;
      val: integer;

  begin
    with prog_options do begin
      repeat                        (* at least one option expected at start *)
        skip_blanks;
        if uppercase (substr (cmd_line, clidx, min (length(cmd_line)-clidx+1, 2))) = 'NO' then
          begin
          positive := false;
          automatic := false;
          clidx := clidx + 2;
        end
        else if uppercase (substr (cmd_line, clidx, min (length(cmd_line)-clidx+1, 4))) = 'AUTO' then
          begin
          positive := true;
          automatic := true;
          clidx := clidx + 4;
        end
        else begin
          positive := true;
          automatic := false;
        end;

        if not lookup_options (cmd_line, clidx, opdotb_option_table, maximum (option_scalar), option_ix)
          then cmd_error (opt_expected);

        if ( not positive and not (option_ix in opdnoo_no_options) ) or
           ( automatic and not (option_ix in opdauo_auto_options) )
          then cmd_error (bad_option);

        case option_ix of

          opt_terse:
            terse_opt := true;

          opt_verbose:
            terse_opt := false;

          opt_length:
            begin
              val := numeric_option ();
              if (val < 4) or (val > 255) then cmd_error (bad_length);
              page_length := val;
            end;

          opt_width:
            begin
              val := numeric_option ();
              if (val < 20) or (val > 255) then cmd_error (bad_width);
              page_width := val;
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

	  opt_library:
	    library_opt := positive;

          opt_check:
            check_sub_options (positive);

          opt_trace..opt_optimize:
            begin
              if (option_ix = opt_optimize) and positive and debug_opt then
                cmd_error (debug_optimize);
              if positive
                then semantic_options := semantic_ns + [opdmto_map_to_optionslist [option_ix]]
                else semantic_options := semantic_options - [opdmto_map_to_optionslist [option_ix]];
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

          opt_storage:
            storage := numeric_option ();

          opt_alloc:
            begin
              val := numeric_option ();
              if val > 99 then
                cmd_error (bad_alloc);
              alloc_mode := val;
            end;

          opt_standard:
            writeln (tty, '%The STANDARD option has not been implemented');

          opt_exit:
            terminate;

	  opt_ki_code:
	    ki_code_opt:=true;

          others:
            cmd_error (not_cmd_option)

        end (* case *) ;

      until not (checkpunct (',') orif checkpunct ('/'));
    end (* with *) ;
  end;
$PAGE read_command_line - main routine

begin (* read_command_line *)
1 (* abort *) :         (* reenter command loop after error *)
  loop (* until compilation command found *)
    prog_options := default_options;    (* initialize options from defaults *)
    get_line;
    clidx := 1;                     (* start parse at beginning of line *)
    err_idx := 1;

  if checkeol () then terminate; (* blank line => stop *)

    if checkpunct ('@') then begin
      cmd_file_name := filename ();
      if not checkeol then cmd_error (bad_syntax);
      if not open_file (cmd_file, cmd_file_name, 'CCL', input_mode, []) andif
         not open_file (cmd_file, cmd_file_name, 'CMD', input_mode, []) then
        cmd_error (bad_indirect_file);
      readln (cmd_file);
      rd_cmd_file (cmd_file);
      close (cmd_file);
    end

    else if checkpunct (':') then
      process_immediate_command

    else if checkpunct ('/') then begin
      scan_options;
      if not checkeol then cmd_error (bad_syntax);
      default_options := prog_options;
    end

    else begin      (* have files to compile *)
      rel_file := '';
      list_file := '';
      list_explicit := false;
      if checkpunct ('=') then (* =source *)
        main_file := filename ()
      else if checkpunct (',') then begin (* ,[list]=source *)
        if checkpunct ('=') then (* ,=source *)
          main_file := filename ()
        else begin (* ,list=source *)
          list_file := filename ();
          list_explicit := true;
          if not checkpunct ('=') then cmd_error (bad_syntax);
          main_file := filename ();
        end
      end
      else begin
        main_file := filename ();
        if checkpunct ('=') then begin (* rel=source *)
          rel_file := main_file;
          main_file := filename ();
        end
        else if checkpunct (',') then begin (* rel,[list]=source *)
          rel_file := main_file;
          if checkpunct ('=') then (* rel,=source *)
            main_file := filename ()
          else begin (* rel,list=source *)
            list_file := filename ();
            list_explicit := true;
            if not checkpunct ('=') then cmd_error (bad_syntax);
            main_file := filename ();
          end
        end
        else begin (* source *)
	  rel_file := substr (main_file, search (main_file, [':', ')']) + 1);
          rel_file := substr (rel_file, 1,
                              search (rel_file, ['[', '.'], length (rel_file) + 1) - 1);
          list_file := rel_file;
        end
      end;

      if checkpunct ('!') then begin (* run specified program *)
        del_file (tempname ('PA0'));
        run ('SYS:' || main_file, 1);
      end;

      mf_idx := err_idx;        (* main file was last parsed *)

      if checkpunct ('/') then scan_options;   (* get options for *this* compilation *)
      if not checkeol then cmd_error (bad_syntax);
      if not open_file (input, main_file, 'PAS', input_mode, [])  then begin
        err_idx := mf_idx;              (* report error at filename *)
        cmd_error (bad_source);
      end;
      return;                   (* to compile the files *)
    end (* file case *) ;
  end (* loop *)
end (* read_command_line *);
$PAGE startup
(*  STARTUP initializes the compiler permanent data the first time Pass 1 is
    invoked.  This involves writing a version message, initializing the log
    record skeleton, determining whether the compiler is being run from the
    terminal or from another program, and reading the auto-startup command
    file in the latter case.  *)

procedure startup;

var jbver: ^ integer;
    buffer: tmp_buffer;
    len: tmpcor_len;
    tcr: boolean;

begin
  if not dat_get ('P10INI.ENV' || prgm_dir (), false) then begin
    writeln (tty, '?Unable to load initial environment');
    stop;
  end;
  writeln (tty, 'Pascal, Version ', version ());
  writeln (tty);

  (*  Prepare the standard log record skeleton.  *)

  jbver := ptr (137b);
  log_record.version := jbver^;
  log_record.users_ppn := user_ppn ();
  log_record.opt_auto_run := (auto_run = 1);
  log_record.opt_tmpcor := false;
  log_record.opt_hash := false;

  auto_startup := (auto_run = 1);
  if auto_startup then begin
    len := 0;
    tcr := tmpcor ('PAS', tmpcor_rf, 0, len); (* Look for a tmpcor file. *)

    if tcr then begin
      new (buffer: len); (* If its there, read and delete it. *)
      tcr := tmpcor ('PAS', tmpcor_df, ord (buffer), len);
    end;

    if tcr then begin (* Read commands from tmpcor file. *)
      log_record.opt_tmpcor := true;
      rd_tmpcor ('TMP:PAS', buffer, len);
    end

    else if open_file (cmd_file, tempname ('PAS'), '', input_mode, []) then begin
      log_record.opt_hash := true; (* Read commands from temp file. *)
      readln (cmd_file);
      rd_cmd_file (cmd_file);
      close (cmd_file);
      del_file (tempname ('PAS'));
    end

    else (* No commands -- forget auto-startup. *)
      auto_startup := false;

  end (* if auto_startup *);
end (* startup *);
$PAGE newpas - main program
begin
  open (tty);
  rewrite (tty);

(*$X0                   (* Initial startup. *)
  startup;
*)

(*$X6                   (* Recycled from a subsequent pass. *)
  if not dat_get (tempname ('PA0'), false) then begin
    writeln (tty, '?Compiler temporary file PA0 lost');
    stop;
  end;
*)

  read_command_line; (* Process a command. *)

  dat_save (tempname ('PA0')); (* Now off to Pass 1 to compile a module. *)
  if not switch (prog_options.dump_switches, 'MANUAL') then begin
    close (ttyoutput);
    run (tmprefix || 'DRV' || prgm_dir (), 1);
    writeln (tty, '?Unable to run Pass 1');
  end;
end (* newpas *).
   ~-X!