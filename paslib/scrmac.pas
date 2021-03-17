$TITLE SCRMAC Program

program scrmac;

$INCLUDE cmdutl.inc[31024,320156]

$INCLUDE tempfi.inc[31024,320156]

external procedure run ( packed array [1..*] of char; integer);

$IF P10
$INCLUDE infpac.inc[31024,320156]
$ENDIF

$IF VAX
$INCLUDE image.inc[31024,320157]
$ENDIF

$PAGE const, type, and var global declarations
const
  cmd_line_max   = 120;



type
  fname    = string[40];
  opt_line = string[80];
  pgms     = (pmf,scribe);

  command_line = record
                   input_file  : fname;
                   output_file : fname;
                   pgm_opts    : array [ pgms ] of opt_line
                 end;

  error_codes = (err_fname, err_opt_list, err_rt_paren, err_pmf_input);


var
  status     : (get_cmd, pmf_cmds, scribe_cmds, run_pmf, error, exit_pgm);

  cmd_rec    : command_line;
  cmd_line   : string[cmd_line_max];

  pgm_names  : array [ 1..2 ] of cmd_lookup_record  (* Table used by CMD_LOOKUP *)
		     := (('PMF',1,1), ('SCRIBE',1,2));

  current_pmf    : fname;
  current_scribe : fname;
  current_scrmac : fname;
  set_scrmac_lib : string[cmd_line_max];

  error_code : error_codes;
$PAGE set_error
  (* SET_ERROR sets the ERROR_CODE to the specified error
     and sets STATUS to ERROR.                            *)

  procedure set_error( err : error_codes );
    begin
      error_code := err;
      status := error;
    end (* set_error *);
$PAGE initialize_scrmac
(* INITIALIZE_SCRMAC opens the terminal for input and output, writes
   the current version number, and sets STATUS to GET_CMD. It also
   sets up the versions of PMF and SCRIBE to use; they are assumed
   to reside in the same account as SCRMAC.                           *)

  procedure initialize_scrmac;

    const scrmac_version = 'SCRMAC, Version 2.0  November 2, 1981';

    var
$IF P10   info : jobrec;
$IF VAX
          vax_file   : file_name;
          vax_acct   : string[80];
          error_flag : image_error;
$ENDIF

    begin
      open( tty );
      rewrite( tty );
      writeln( tty );
      writeln( tty, scrmac_version );
      break;
      status := get_cmd;

      (* Get the account number of SCRMAC (different for P10 and VAX )  *)
$IF P10
      jobinfo(info);
      current_pmf := 'PMF' || info.progdir;
      current_scribe := 'SCRIBE' || info.progdir;
      current_scrmac := 'SCRMAC' || info.progdir;
      set_scrmac_lib := '/SET:LIB:SCRMAC' || info.progdir;
$ENDIF
$IF VAX
      image_file_name( vax_file, error_flag );
      vax_acct := substr( vax_file, 1, index( vax_file, ']' ) );
      current_pmf := vax_acct || 'PMF';
      current_scribe := vax_acct || 'SCRIBE';
      current_scrmac := vax_acct || 'SCRMAC';
      set_scrmac_lib :=  '/SET:LIB:' || vax_acct || 'SCRMAC';
$ENDIF
    end (* initialize_scrmac *);
$PAGE get_option_lists
(* GET_OPTION_LISTS parses the option lists for PMF and SCRIBE from
   the command line. If a '/' is not found at the beginning of the
   option list, one is inserted. Option lists are of the form:

		  /<PMF | SCRIBE>:( <option_list> )                  *)

  procedure get_option_lists( var ind : 1.. cmd_line_max );

    var pgm_typ     : pgms;             (* index into pgm option list array *)
        pgm_ptr     : integer;          (* pointer into lookup table *)
        save_ind    : 1..cmd_line_max;  (* used to save IND's value *)
        found_right : boolean;          (* flag found right paren *)
        opt_list    : opt_line;         (* holds part of option line *)

    begin

      (* Start option lists empty *)
      cmd_rec.pgm_opts[ pmf ] := '';
      cmd_rec.pgm_opts[ scribe ] := '';

      (* get options until there are no more or an error is found *)
      while (status <> error) andif (not cmd_eol( cmd_line, ind))  do begin

        (* IF expression checks for proper syntax option list syntax *)
	if not(cmd_check_punct( cmd_line, ind, '/') andif
	      (cmd_lookup( cmd_line, ind, ['A'..'Z'], pgm_names, pgm_ptr) andif
	      (cmd_check_punct( cmd_line, ind, ':') andif
	       cmd_check_punct( cmd_line, ind, '(') )))
	  then set_error( err_opt_list )
	  else with cmd_rec  do begin
	    if pgm_ptr = 1  then pgm_typ := pmf
			    else pgm_typ := scribe;
	    found_right := false;

            (* get list until matching right paren found *)
            while (not found_right) and (status <> error)  do begin
	      save_ind := ind;
	      if not cmd_string( cmd_line, ind, ')', opt_list)
                then set_error( err_rt_paren )
		else if index(substr(cmd_line,save_ind,(ind-save_ind)),'(') = 0
		  then found_right := true
                  else opt_list := opt_list || ')';
	      pgm_opts[ pgm_typ ] := pgm_opts[ pgm_typ ] || opt_list;
	    end (* while not found_right *);

            (* make sure first character of option list is "/" *)
	    if (length(pgm_opts[ pgm_typ ]) > 0) andif
	       (pgm_opts[ pgm_typ ][1] <> '/')
	      then pgm_opts[ pgm_typ ] := '/' || pgm_opts[ pgm_typ ];

	  end (* else with *);

        end (* while *);
  end (* get_option_lists *);
$PAGE get_cmd_line
(* GET_CMD_LINE will get the command line from the terminal and parse it
   into an input file, an output file (if given), PMF program options,
   and SCRIBE program options.  Command lines are of the form:

   [ <output file> = ] <input file> [ /<program name>:(<option list>) ]...

   If the command line (CMD_LINE) is null, STATUS will be set to EXIT_PGM.   *)

  procedure get_cmd_line;

    const prompt = '*';
    var   ind    : 1..cmd_line_max ;
          ptr    : integer;


    begin
      cmd_getline( prompt, cmd_line, ind);
      if cmd_line = ''
        then status := exit_pgm
        else with cmd_rec  do begin
          status := succ( status );

          (* get the input and output file names *)
          if not cmd_file_name(cmd_line, ind, true, output_file)
            then set_error( err_fname )
            else if not cmd_check_punct( cmd_line, ind, '=')
              then begin
                input_file := output_file;
                ptr := index( input_file, '.' );
                if ptr <> 0
                  then output_file := substr( input_file, 1, (ptr-1) );
              end (* then begin *)
              else if not cmd_file_name( cmd_line, ind, true, input_file)
		then set_error( err_fname );

          (* get the option lists for PMF and SCRIBE *)
          get_option_lists( ind );

        end (* with cmd_rec *)
    end (* get_cmd_line *);
$PAGE write_pmf_cmds
(* WRITE_PMF_CMDS will check to see that the PMF input file exists.
   If the input file does not exist, then STATUS will  be set to
   ERROR, and an error code will be assigned. If it does, then the
   proper PMF command lines are written to the ###PMF. TMP file in
   the form

	  "<###INT.TMP>=<input file>[<option list>]",

   along with the necessary commands to run SCRIBE from PMF.       *)

  procedure write_pmf_cmds;

    const set_nopascal_switch = '/SET:NOPASCAL';
    var   cmd_file : text;

    begin
      with cmd_rec  do begin
        reset( input, '.PMF ' || input_file );
        if eof( input )
          then set_error( err_pmf_input )
          else begin
            rewrite( cmd_file, temp_file_name('PMF'));
            writeln( cmd_file, set_scrmac_lib );
            writeln( cmd_file, set_nopascal_switch );
            write( cmd_file, temp_file_name('INT'), '=', input_file );
            writeln( cmd_file, pgm_opts[ pmf] );
            writeln( cmd_file, '/RUN:', current_scribe);
            writeln( cmd_file, '/EXIT');
            close( cmd_file );
            status := succ(status);
          end (* else begin *);
      end (* with *);
    end (* write_pmf_cmds *);
$PAGE write_scribe_cmds
(* WRITE_SCRIBE_CMDS will write a command line to the file ###SCR.TMP
   which will run SCRIBE and then return to the SCRMAC program. 
   The form of the command line written to ###SCR.TMP is

     <output file>=<###INT.TMP>/[<option list>]                       *)


  procedure write_scribe_cmds;

    var cmd_file : text;

    begin
      with cmd_rec  do begin
	rewrite( cmd_file, temp_file_name('SCR'));
        write( cmd_file, output_file, '=', temp_file_name('INT') );
        writeln( cmd_file, pgm_opts[ scribe ] );
	writeln( cmd_file, '/RUN:', current_scrmac);
	writeln( cmd_file, '/EXIT');
	close( cmd_file );
	status := succ(status);
      end (* with *);
    end (* write_scribe_cmds *);
$PAGE run_pmf_pgm
  (* RUN_PMF_PGM closes any open files and then runs PMF *)

  procedure run_pmf_pgm;
    begin
      close   (* All files *);
      run( current_pmf, 0);
    end (* run_pmf_pgm *);
$PAGE disp_error
(* DISP_ERROR will display the appropiate error message and
   set STATUS to GET_CMD to try another command line.       *)

  procedure disp_error;

    begin
      case error_code of

        err_fname     :  writeln( tty,'% ILLEGAL FILE NAME SYNTAX');

        err_opt_list  :  writeln( tty,'% ILLEGAL OPTION LIST SYNTAX');

        err_rt_paren  :  writeln( tty,'%  OPTIONS MUST BE ENCLOSED IN PARENTHESIS');

        err_pmf_input :  writeln( tty,'% BAD SCRMAC INPUT FILE');

      end (* case *);

      status := get_cmd;

    end (* disp_error *);
$PAGE scrmac main body
  begin (* scrmac main program body *)

    (* initialize program *)
    initialize_scrmac;

    (* Pick procedure according to program status. *)
    (* Normal status flow is:                      *)
    (*   get_cmd, pmf_cmds, scribe_cmds, run_pmf.  *)
    (* If an error occures, STATUS is set to error *)
    (* and then is reset to GET_CMD in DISP_ERROR. *)

    while status <> exit_pgm

      do case status of

	get_cmd     : get_cmd_line;

	pmf_cmds    : write_pmf_cmds;

	scribe_cmds : write_scribe_cmds;

	run_pmf     : run_pmf_pgm;

	error       : disp_error;

      end (* case *);
end (* scrmac *).
   