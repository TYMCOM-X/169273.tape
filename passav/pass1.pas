$TITLE PASS1 - driver for compiler first pass

program pass1
  options storage (6000);
$PAGE declarations
$INCLUDE pascal.inc
$INCLUDE pasfil.inc
$INCLUDE pasist.inc
$INCLUDE ptmcon.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE paslex.inc
$INCLUDE paserr.inc
$INCLUDE pascfm.inc
$INCLUDE passw.inc
$INCLUDE pasopd.inc
$INCLUDE corout.inc
$INCLUDE pasopn.inc
$INCLUDE pasrdr.inc
$INCLUDE pasdat.inc
$INCLUDE pascgr.inc
$INCLUDE pa1xrf.inc
$INCLUDE pasglb.inc
$INCLUDE pasanl.inc
$INCLUDE pasblk.inc
$INCLUDE pastal.inc
$INCLUDE pasalc.inc
$INCLUDE pasifu.inc
$INCLUDE pascv.inc
$INCLUDE pa1dmp.inc
$INCLUDE pasdmp.inc
$INCLUDE prgdir.inc
$INCLUDE dtime.inc
$INCLUDE paslog.inc
$INCLUDE infpac.inc
$INCLUDE tmpnam.inc
$INCLUDE run.inc

external var
  auto_run: 0 .. 1; (* 0 = manual, 1 = automatic *)

external function fileblock ( var text ): filblock;
$PAGE check_types
(*  CHECK TYPES is called at the end of an environment compilation to examine
    all the type identifiers defined in the root block, and print error messages
    if any of them are undefined.  *)

procedure check_types;

var ts: sym;

begin
  ts := root_block^.type_list.first;
  while ts <> nil do begin
    with ts^.type_desc^ do begin
      if (kind = unknown_type) andif
         (type_id <> nil) andif
         (type_id^.name <> nil) then begin
	err_print (err_type_warning, declaration, type_id^.name^.text, 0);
        declaration := null_source;
      end;
    end;
    ts := ts^.next;
  end;
end (* check_types *);
$PAGE do_pass_1

external var abort: environment;        (* action to perform on fatal error *)

procedure do_pass_1;

  var dtime: dtime_ext;

  const
    reader_stack_size = 2000;
    abort_stack_size = 10;

  label 100 (* abort *) ;

  procedure abt_pass1;
  begin
    detach; (* after creation *)
    fin_source := cur_source;   (* record line where reading aborted *)
    goto 100;
  end;

begin (* do_pass_1 *);
  dtime := dc_ext (daytime ());
  cdatesym^.init_value.valp^.str_val [1:9] := substr (dtime, 1, 9);
  ctimesym^.init_value.valp^.str_val [1:8] := substr (dtime, 11, 8);

  all_opts := [ ];
  cur_block := root_block;
  ext_block := nil;
  lex_block := nil;
  blk_number := 0;
  max_level := 0;
  heap_chain := nil;
  sym_vl_number := vl_base;
  sym_nvl_number := nvl_base;
  vl_list := vll_base;
  err_count := 0;
  warnings := 0;
  linect := 0;
  inclct := 0;
  elf_status := unopened;
  elf_open;
  df_status := unopened;      (* tell PASDMP to open on first reference *)
  xrf_init;
  lex_init;
  tal_init;
  alc_init;
  initparse;
  ch_init;
  ch_open (false, true);
  if prog_options.global_opt then
    glob_init;

  abort := create (abt_pass1, abort_stack_size);
  reader := create (read_input_lines, reader_stack_size);

  semantics;

  if switch (root_block^.dump_switches, 'NAMES') then
    dump_name_table;
  if switch (root_block^.dump_switches, 'ROOTST') then begin
    dmpblock (root_block);
    dmpstable (root_block);
  end;

  finish := (not env_compilation) and
	    ( (max_severity = 0) or
	      ((max_severity = 1) and prog_options.finish_opt) );

  endparse;
  if env_compilation then
    check_types;
  if finish then begin
    fin_graph; (* includes the quick block analysis *)
    allocate_storage; (* gives addresses to all var, value and const symbols *)
  end;

100 (* abort *):

  dispose (abort);
  dispose (reader);
  xrf_close;
  elf_close;
  dmp_close;
  ch_close;
  if prog_options.global_opt then
    glob_term;
end (* do_pass_1 *);
$PAGE next_pass
(* NEXT PASS saves the heap and, if the AUTO_RUN flag is set, initiates the next
   pass. *)

procedure next_pass;

var
  next: packed array [1..6] of char;

begin
  opts_listing := ( list_file <> '' ) andif
                  ( src_selected orif
                    ([symbols_opt, xref_opt, calls_opt] * all_opts <> []) orif
                    ( prog_options.code_opt andif
                      ([assembly_opt, map_opt] * all_opts <> []) andif
                      prog_options.banner_opt ) );

  if (rel_file = '') and ( (list_file = '') or not (assembly_opt in all_opts) ) then
    prog_options.code_opt := false;

  quick := not have_optimizer or
           ( have_checkout and
             ( ( prog_options.quick_opt = opt_is_on ) or
               ( ( prog_options.quick_opt = opt_is_auto ) and not ( optimize_opt in all_opts ) ) ) );

  if finish and (prog_options.code_opt or (prog_options.dump_switches <> nil)) then begin
    if quick then begin
      if opts_listing or (err_count <> 0) then
        next := 'PASLST'
      else
        next := tmprefix || 'CCG';
    end
    else
      next := tmprefix || 'SHP';
  end
  else if opts_listing or (err_count <> 0) then
    next := 'PASLST'
  else
    next := 'PASCMD';

  log_record.no_lines := linect;
  log_record.no_incl_lines := inclct;
  log_record.no_errors := err_count - warnings;
  log_record.date_and_time := root_block^.children^.comp_dtime;
  log_record.alloc_strategy := prog_options.alloc_mode;
  log_record.opt_debug := prog_options.debug_opt;
  log_record.opt_check :=
    ([minimum (checklist) .. maximum (checklist)] * all_opts <> []);
  log_record.opt_main := (root_block^.children^.kind = program_blk);
  log_record.opt_overlay := prog_options.overlay_opt;
  log_record.opt_source := src_selected;
  log_record.opt_special :=
    ([minimum (speciallist) .. maximum (speciallist)] * all_opts <> []);
  log_record.opt_terse := prog_options.terse_opt;
  log_record.opt_trace := (trace_opt in all_opts);
  log_record.opt_xref := prog_options.global_opt;
  log_record.lowseg_size := 0;
  log_record.highseg_size := 0;
  log_record.ki10:=prog_options.ki_code_opt;

  if next = 'PASCMD'
    then log_write
    else dat_save (tempname ('PAS'));

  if auto_run <> 0 then begin
    run (next || prgm_dir (), true);
    rewrite (tty);
    writeln (tty, '?Unable to run ', next);
  end;
end;
$PAGE save_environment
(*  SAVE ENVIRONMENT will write the current environment to a specified file.  *)

procedure save_environment;

var	save_main_file_name: file_name;

begin
  save_main_file_name:=main_file;
  main_file:='';
  cmd_clear;
  pop_switches (default_options.switches, nil);
  pop_switches (default_options.dump_switches, nil);
  dat_save ('.ENV ' || rel_file);
  main_file:=save_main_file_name;
end;
$PAGE pass1 - main program
var start_time: integer;
    segstuff: segrecd;

begin
  if not dat_get (tempname ('PA0'), false) then begin
    rewrite (tty);
    writeln ('?Compiler temporary file PA0 lost');
    stop;
  end;

  rewrite (tty);

  start_time := runtime;
  if open_search (input, '.PAS ' || main_file) then ;

  log_record.file_name := fileblock (input);
  log_record.run_time := start_time;

  if prog_options.names_opt then begin
    writeln (tty, '[Compiling ', filename (input), ']');
    break;
  end;

  root_block^.semantic_options := prog_options.semantic_options;
  root_block^.dump_switches := prog_options.dump_switches;

  do_pass_1;

  if env_compilation and
     ( (max_severity = 0) or
       ( (max_severity = 1) and prog_options.finish_opt) ) and
     (rel_file <> '') then begin
    save_environment;
    rewrite (tty);
  end;

  if prog_options.statistics_opt then begin
    seginfo (segstuff);
    writeln (tty, '[Pass 1: ', (runtime - start_time) / 1000.0: 8: 3, ' seconds, ',
                  (segstuff.lowlen+511) div 512: 3, '+',
                  (segstuff.highlen+511) div 512: 3, 'P]');
  end;

  next_pass;
end (* pass1 *).
   