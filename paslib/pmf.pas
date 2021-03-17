$TITLE pmf -- pascal macro facility main program (pmf)
$LENGTH 43

program pmf;

$INCLUDE pmf.typ
$PAGE  pmfcmd.inc file
$INCLUDE pmfcmd.inc
$PAGE  pmfinp.inc file
$INCLUDE pmfinp.inc
$PAGE  pmfput.inc file
$INCLUDE pmfput.inc
$PAGE  pmfdef.inc file
$INCLUDE pmfdef.inc
$PAGE  pmfexp,inc file 
$INCLUDE pmfexp.inc
$PAGE  pmfscn.inc file
$INCLUDE pmfscn.inc
$PAGE  pmferr.inc file 
$INCLUDE pmferr.inc


external procedure run( packed array [1..*] of char; integer);

$PAGE  pmf: main body declarations

var
  status: ( null,                             (* No processing done yet. *)
	    success,                          (* Last file processed successfully. *)
	    failure                           (* Error in last file. *)
		    ) := null;

  run_flag: boolean := false;             (* True if running another program from PMF *)

  run_file : sym_string;                  (* String file name *)

  runoffset : integer;                    (* Offset when running another pgm from PMF *)

label
(* start *) 100,
(* continue *) 200;


public procedure err_exit;

begin
  status := failure;
  goto (* continue *) 200;
end;


$PAGE  pmf:main body code begins
begin
  rewrite (tty);
  writeln (tty,'PMF, Version 3.0  November 2, 1981' );

  (* start *) 100:

  command;                                      (* Process a command line from the tty. *)

  with cmd_options do begin
    if (exit_switch in option_list) then begin                      (* All done. *)
      break;
      if run_flag
	then begin
          if run_file = ''
            then begin
	      writeln(tty,'% NO RUN FILE SPECIFIED');
              stop;
            end
            else begin
              close (* all files - run call will not close open files *);
              run( run_file, runoffset );
            end;
        end
	else stop;
  end;

  if(process_switch in option_list) then begin           (* Input/output files specified. *)
    if (macro_switch in option_list) then begin          (* Perform macro processing. *)
      inpinit;                                (* Initialize everything. *)
      putinit;
      definit;
      expinit;
      if (lib_switch in option_list) then begin                   (* Load a library file. *)
	lib_file_id := switch_values [lib_switch];
	if not lib_load() then begin
	  writeln (tty,'% BAD LIBRARY FILE ',lib_file_id);
	  status := null;
	  goto (* continue *) 200;
	end;
      end;
      if not open_files() then begin          (* Input or output file error. *)
	status := null;
	goto (* continue *) 200;
      end;
      if not (input_switch in option_list) then          (* No input file, so flag an eof. *)
	put_back (eof_ch);
      scanner;                                (* Do the macro processing. *)
      status := success;                      (* It worked! *)
    end (* macro processing *);
  end (* if sw(process) *);
end (* with cmd_options *);
$PAGE
  (* continue *)   200:

  with cmd_options do begin
    if (save_switch in option_list) and (status <> null) then begin (* Save a library file. *)
      lib_file_id := switch_values [save_switch];
      num_args := 1;                            (* So SAVE won't try to save a text parameter. *)
      if not lib_save() then
	writeln (tty,'% BAD LIBRARY FILE ',lib_file_id);
    end;

    if status <> null
      then close (output);

    if run_switch in option_list
      then begin
	run_flag := true;
        run_file := switch_values[ run_switch ];
	getstring( switch_values[ runoffset_switch ], runoffset );
      end;

    if (dump_switch in option_list) and (status = failure) then     (* Do error traceback. *)
      err_dump;
  end;

  status := null;  (* Set for next time around *)

goto (* start *) 100;
end (* pmf *).
