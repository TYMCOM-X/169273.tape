$TITLE PASCAL -- Pascal Compiler, Startup Program

program pascal;
$PAGE declarations
$SYSTEM pascal
$SYSTEM pasist
$SYSTEM pasdat
$SYSTEM pascfm
$SYSTEM versio
$SYSTEM prgdir
$SYSTEM dtime
$SYSTEM paslog
$SYSTEM tmpnam
$SYSTEM run
$SYSTEM tmpcor

external var
    auto_run: boolean; (* TRUE => use auto-startup command file *)
    auto_startup: boolean; (* TRUE => terminate at end of control file *)

var cmd_file: text;

external function user_ppn: machine_word;
$PAGE startup
(*  STARTUP initializes the compiler permanent data the first time Pass 1 is
    invoked.  This involves writing a version message, initializing the log
    record skeleton, determining whether the compiler is being run from the
    terminal or from another program, and reading the auto-startup command
    file in the latter case.  *)

procedure startup
  options special(coercions);

var jbver: ^ integer;
    buffer: tmp_buffer;
    len: tmpcor_length;
    tcr: boolean;
    cmd_file: text;

begin
  if not dat_get ('P10INI.ENV' || prgm_dir (), false) then begin
    rewrite (tty);
    writeln (tty, '?Unable to load initial environment');
    stop;
  end;

  rewrite (tty);
  writeln (tty, 'TYM-Pascal, Version ', version ());
  writeln (tty);

  (*  Prepare the standard log record skeleton.  *)

  jbver := ptr (137b);
  log_record.version := jbver^;
  log_record.users_ppn := user_ppn ();
  log_record.opt_auto_run := auto_run;
  log_record.opt_tmpcor := false;
  log_record.opt_hash := false;

  auto_startup := auto_run;
  if auto_startup then begin
    len := 0;
    tcr := tmpcor ('PAS', tmpcor_rf, 0, len); (* Look for a tmpcor file. *)

    if tcr then begin
      new (buffer, len); (* If its there, read and delete it. *)
      tcr := tmpcor ('PAS', tmpcor_df, ord (buffer), len);
    end;

    if tcr then begin (* Read commands from tmpcor file. *)
      log_record.opt_tmpcor := true;
      rd_tmpcor ('TMP:PAS', buffer, len);
    end

    else begin
      reset (cmd_file, tempname ('PAS'));
      if not eof (cmd_file) then begin
	log_record.opt_hash := true; (* Read commands from temp file. *)
	rd_cmd_file (cmd_file);
	scratch (cmd_file);
      end

      else (* No commands -- forget auto-startup. *)
	auto_startup := false;
    end;

  end (* if auto_startup *);
end (* startup *);
$PAGE pascal - main program
begin
  startup;
  dat_save (tempname ('PA0'));
  run ('PASCMD' || prgm_dir (), true);
  rewrite (tty);
  writeln (tty, '?Unable to run PASCMD');
end.
 