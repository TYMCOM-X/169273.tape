$TITLE DOC.PAS, last modified 4/27/84, zw
PROGRAM doc;
$SYSTEM CMDUTL.INC
$SYSTEM TEMPFI.INC
$SYSTEM RUNUTL.INC
$SYSTEM VERSIO.INC
$SYSTEM INFPAC.INC
(*
$IF VAX
$SYSTEM image.inc
$ENDIF
*)
const
  cmd_line_max   = 120;
type
  fname    = string[40];
  opt_line = string[80];
  pgms     = (mac,txt);
  command_line = record
                   input_file  : fname;
                   output_file : fname;
                   pgm_opts    : array [pgms] of opt_line
                 end;
  error_codes = (err_fname, err_opt_list, err_rt_paren, err_mac_input);
var
  status     : (get_cmd, mac_cmds, txt_cmds, run_mac, error, exit_pgm);
  cmd_rec    : command_line;
  cmd_line   : string[cmd_line_max];
  pgm_names  : array [1..2] of cmd_lookup_record  (* Table used by CMD_LOOKUP *)
		     := (('mac',1,1), ('txt',1,2));
  current_mac    : fname;
  current_txt : fname;
  current_doc : fname;
  set_doc_lib : string[cmd_line_max];
  error_code : error_codes;
  (* SET_ERROR sets the ERROR_CODE to the specified error
     and sets STATUS to ERROR.                            *)
  procedure set_error(err : error_codes);
    begin
      error_code := err;
      status := error;
    end (* set_error *);
(* INITIALIZE_DOC opens the terminal for input and output, writes
   the current version number, and sets STATUS to GET_CMD. It also
   sets up the versions of mac and txt to use; they are assumed
   to reside in the same account as doc.                           *)
  procedure initialize_doc;
    var
$IF P10   info : jobrec;
$IF VAX
          vax_file   : file_name;
          vax_acct   : string[80];
          error_flag : image_error;
$ENDIF
    begin
      open(tty);
      rewrite(tty);
      writeln(tty, 'TYM-Pascal Document Processor, Version ', version());
      writeln(tty);
      break;
      status := get_cmd;
      (* Get the account number of doc (different for P10 and VAX)  *)
$IF P10
      jobinfo(info);
      current_mac := 'mac' || info.progdir;
      current_txt := 'txt' || info.progdir;
      current_doc := 'DOC' || info.progdir;
      set_doc_lib := '/SET:LIB:DOC' || info.progdir;
$ENDIF
$IF VAX
      image_file_name(vax_file, error_flag);
      vax_acct := substr(vax_file, 1, index(vax_file, ']'));
      current_mac := vax_acct || 'mac';
      current_txt := vax_acct || 'txt';
      current_doc := vax_acct || 'DOC';
      set_doc_lib :=  '/SET:LIB:' || vax_acct || 'DOC';
$ENDIF
    end (* initialize_doc *);
(* GET_OPTION_LISTS parses the option lists for mac and txt from
   the command line. If a '/' is not found at the beginning of the
   option list, one is inserted. Option lists are of the form:
		  /<mac | txt>:(<option_list>)                  *)
  procedure get_option_lists(var ind : 1.. cmd_line_max);
    var pgm_typ     : pgms;             (* index into pgm option list array *)
        pgm_ptr     : integer;          (* pointer into lookup table *)
        save_ind    : 1..cmd_line_max;  (* used to save IND's value *)
        found_right : boolean;          (* flag found right paren *)
        opt_list    : opt_line;         (* holds part of option line *)
    begin
      (* Start option lists empty *)
      cmd_rec.pgm_opts[mac] := '';
      cmd_rec.pgm_opts[txt] := '';
      (* get options until there are no more or an error is found *)
      while (status <> error) andif (not cmd_eol(cmd_line, ind))  do begin
        (* IF expression checks for proper syntax option list syntax *)
	if not(cmd_check_punct(cmd_line, ind, '/') andif
	      (cmd_lookup(cmd_line, ind, ['A'..'Z'], pgm_names, pgm_ptr) andif
	      (cmd_check_punct(cmd_line, ind, ':') andif
	       cmd_check_punct(cmd_line, ind, '('))))
	  then set_error(err_opt_list)
	  else with cmd_rec  do begin
	    if pgm_ptr = 1  then pgm_typ := mac
			    else pgm_typ := txt;
	    found_right := false;
            (* get list until matching right paren found *)
            while (not found_right) and (status <> error)  do begin
	      save_ind := ind;
	      if not cmd_string(cmd_line, ind, ')', opt_list)
                then set_error(err_rt_paren)
		else if index(substr(cmd_line,save_ind,(ind-save_ind)),'(') = 0
		  then found_right := true
                  else opt_list := opt_list || ')';
	      pgm_opts[pgm_typ] := pgm_opts[pgm_typ] || opt_list;
	    end (* while not found_right *);
            (* make sure first character of option list is "/" *)
	    if (length(pgm_opts[pgm_typ]) > 0) andif
	       (pgm_opts[pgm_typ][1] <> '/')
	      then pgm_opts[pgm_typ] := '/' || pgm_opts[pgm_typ];
	  end (* else with *);
        end (* while *);
  end (* get_option_lists *);
(* GET_CMD_LINE will get the command line from the terminal and parse it
   into an input file, an output file (if given), mac program options,
   and txt program options.  Command lines are of the form:
   [<output file> =] <input file> [/<program name>:(<option list>)]...
   If the command line (CMD_LINE) is null, STATUS will be set to EXIT_PGM.   *)
  procedure get_cmd_line;
    const prompt = '*';
    var   ind    : 1..cmd_line_max ;
          ptr    : integer;
    begin
      cmd_getline(prompt, cmd_line, ind);
      if cmd_line = ''
        then status := exit_pgm
        else with cmd_rec  do begin
          status := succ(status);
          (* get the input and output file names *)
          if not cmd_file_name(cmd_line, ind, true, output_file)
            then set_error(err_fname)
            else if not cmd_check_punct(cmd_line, ind, '=')
              then begin
                input_file := output_file;
                ptr := index(input_file, '.');
                if ptr <> 0
                  then output_file := substr(input_file, 1, (ptr-1));
              end (* then begin *)
              else if not cmd_file_name(cmd_line, ind, true, input_file)
		then set_error(err_fname);
          (* get the option lists for mac and txt *)
          get_option_lists(ind);
        end (* with cmd_rec *)
    end (* get_cmd_line *);
(* WRITE_mac_CMDS will check to see that the mac input file exists.
   If the input file does not exist, then STATUS will  be set to
   ERROR, and an error code will be assigned. If it does, then the
   proper mac command lines are written to the ###mac. TMP file in
   the form
	  "<###INT.TMP>=<input file>[<option list>]",
   along with the necessary commands to run txt from mac.       *)
  procedure write_mac_cmds;
    const set_nopascal_switch = '/SET:NOPASCAL';
    var   cmd_file : text;
    begin
      with cmd_rec  do begin
        reset(input, '.mac ' || input_file);
        if eof(input)
          then set_error(err_mac_input)
          else begin
            rewrite(cmd_file, temp_file_name('mac'));
            writeln(cmd_file, set_doc_lib);
            writeln(cmd_file, set_nopascal_switch);
            write(cmd_file, temp_file_name('INT'), '=', input_file);
            writeln(cmd_file, pgm_opts[mac]);
            writeln(cmd_file, '/RUN:', current_txt);
            writeln(cmd_file, '/EXIT');
            close(cmd_file);
            status := succ(status);
          end (* else begin *);
      end (* with *);
    end (* write_mac_cmds *);
(* WRITE_txt_CMDS will write a command line to the file ###SCR.TMP
   which will run txt and then return to the doc program. 
   The form of the command line written to ###SCR.TMP is
     <output file>=<###INT.TMP>/[<option list>]                       *)
  procedure write_txt_cmds;
    var cmd_file : text;
    begin
      with cmd_rec  do begin
	rewrite(cmd_file, temp_file_name('SCR'));
        write(cmd_file, output_file, '=', temp_file_name('INT'));
        writeln(cmd_file, pgm_opts[txt]);
	writeln(cmd_file, '/RUN:', current_doc);
	writeln(cmd_file, '/EXIT');
	close(cmd_file);
	status := succ(status);
      end (* with *);
    end (* write_txt_cmds *);
  (* RUN_mac_PGM closes any open files and then runs mac *)
  procedure run_mac_pgm;
    begin
      close   (* All files *);
      IF NOT runprg(current_mac, 0) THEN;
    end (* run_mac_pgm *);
(* DISP_ERROR will display the appropiate error message and
   set STATUS to GET_CMD to try another command line.       *)
  procedure disp_error;
    begin
      case error_code of
        err_fname     : writeln(tty,'% ILLEGAL FILE NAME SYNTAX');
        err_opt_list  : writeln(tty,'% ILLEGAL OPTION LIST SYNTAX');
        err_rt_paren  : writeln(tty,'% OPTIONS MUST BE ENCLOSED IN PARENTHESIS');
        err_mac_input : writeln(tty,'% BAD DOC INPUT FILE');
      end (* case *);
      status := get_cmd;
    end (* disp_error *);
begin (* doc main program body *)
    (* initialize program *)
    initialize_doc;
    (* Pick procedure according to program status. *)
    (* Normal status flow is:                     *)
    (*   get_cmd, mac_cmds, txt_cmds, run_mac.  *)
    (* If an error occures, STATUS is set to error *)
    (* and then is reset to GET_CMD in DISP_ERROR. *)
    while status <> exit_pgm
      do case status of
	get_cmd     : get_cmd_line;
	mac_cmds    : write_mac_cmds;
	txt_cmds : write_txt_cmds;
	run_mac     : run_mac_pgm;
	error       : disp_error;
      end (* case *);
end (* doc *).
  