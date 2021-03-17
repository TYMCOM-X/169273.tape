$TITLE pmfcmd -- pmf command line processor
$LENGTH 43

module command_module;

$INCLUDE pmf.typ


$include tempfi.inc

$PAGE module variable declarations
const
  line_size = 256;
  max_options = 11;
  max_indirect_nesting = 6;

type
  line_index = 0 .. line_size;

  option_index = 0 .. max_options;

  cursor = packed record
		    loc: line_index;
		    len: line_index
		  end;

var
  cmd_line: string [line_size];             (* Current command line. *)
  in_file_id: cursor;                            (* Input file name location in line. *)
  out_file_id: cursor;                       (* Output file name location in line. *)
  options_in_line: array [1..max_options] of record   (* Option switches in the line:  *)
					       name: cursor;                        (* Option name location. *)
					       val: cursor                          (* Option value location. *)
					     end;
  option_count: 0 .. max_options;              (* Number option switches in the line. *)

  ind_file : array [1..max_indirect_nesting] of text; (* Indirect command files *)
  ind_file_index : 0..max_indirect_nesting := 0; (* Index to indirect files *)
  indirect_input: boolean := false;              (* True if reading from a command file. *)
  auto_indirect_file : boolean := false;         (* True if ###PMF.TMP file used *)

public var
  cmd_options: option_record;
$PAGE get_a_line
(*  GET_A_LINE will read a text line from the specified file
    into 'cmd_line'.  *)

procedure get_a_line ( var cmd_file: text );

begin
  cmd_line := '';
  while not eoln(cmd_file) do begin
    if cmd_file^ <> ' ' then
      cmd_line := cmd_line || uppercase(cmd_file^);
    get (cmd_file);
  end;
end (* get_a_line *);





(* ASSIGN_INDIRECT_INPUT will open the indirect file if it is valid
   or write appropiate error messages if the indirect file name is
   invalid.                                                         *)

procedure assign_indirect_input;


  begin
    if ind_file_index = max_indirect_nesting
      then  writeln(tty,'% MAXIMUM NESTED INDIRECT FILES EXCEEDED',
			' - COMMAND IGNORED')
      else begin
	reset(ind_file[ind_file_index+1], '.CMD ' || substr(cmd_line,2));
	if not eof(ind_file[ind_file_index+1])
	  then begin
	    ind_file_index := ind_file_index + 1;
	    indirect_input := true;
	  end
	  else writeln(tty,'% BAD INDIRECT FILE ',substr(cmd_line,2),
			   ' - COMMAND IGNORED');
      end (* else begin *);
  end (* assign_indirect_input *);
$PAGE read_cmd_line
(*  READ_CMD_LINE will get a command line from the current command file
    (tty or indirect).  '@File' commands will be processed.  *)


procedure read_cmd_line;

  label 100;

begin
100:
  if indirect_input
    then begin
      get_a_line (ind_file[ind_file_index]);
      if not auto_indirect_file
	then writeln (tty,'>',cmd_line);
      break;
      readln (ind_file[ind_file_index]);
      if eof(ind_file[ind_file_index])
	then begin
          if (ind_file_index = 1) andif (auto_indirect_file = true)
	    then scratch(ind_file[ind_file_index])
	    else close(ind_file[ind_file_index]);
	  ind_file_index := ind_file_index - 1;
	  if ind_file_index = 0
	    then indirect_input := false;
	end (* then begin *)
	else if cmd_line = ''  then goto 100;
    end (* indirect_input *)

    else (* not indirect_input *) begin
      rewrite (tty);
      write (tty,'*');
      break;
      reset (tty);
      get_a_line (tty);
      if cmd_line = ''
	then cmd_line := '/EXIT';
    end (* not indirect input *);

    if cmd_line [1] = '@'
      then begin
	assign_indirect_input();
	goto 100;
      end;
end (* read_cmd_line *);
$PAGE scan_cmd_line
(*  SCAN_CMD_LINE will scan the text string in 'cmd_line', which
    is assumed to have the format:

  [[<output file>=]<input file>][/<option>[:<value>]]...

    It sets up string descriptors in 'out_file_id', 'in_file_id', and
    'options_in_line [i]' (.name and .val) for i = 1 to 'option_count'.  *)

procedure scan_cmd_line;

  var ind: line_index;

  procedure copy_to_delim ( var str: cursor; delims: char_set );

  begin
    str.loc := ind + 1;
    ind := ind + search(substr(cmd_line,ind+1),delims+[' '],length(cmd_line)-ind);
    str.len := ind - str.loc;
  end;

begin
  cmd_line := cmd_line || ' ';
  ind := 0;
  out_file_id.len := 0;
  option_count := 0;
  copy_to_delim (in_file_id,['=','/']);
  if cmd_line [ind] = '=' then begin
    out_file_id := in_file_id;
    copy_to_delim (in_file_id,['/']);
  end;
  while (ind <> length(cmd_line)) and (option_count <> max_options) do begin
    option_count := option_count + 1;
    with options_in_line [option_count] do begin
      copy_to_delim (name,[':','/']);
      if cmd_line [ind] = ':' then
	copy_to_delim (val,['/'])
      else begin
	val.loc := ind;
	val.len := 0;
      end;
    end;
  end;
end (* scan_cmd_line *);
$PAGE option data initialization

var
  saved_options: option_record :=
    ( [macro_switch,pascal_switch], ('','','',''), '' );


procedure reset_options;

begin
  cmd_options := saved_options;
  if (in_file_id.len <> 0) or (out_file_id.len <> 0) then
    cmd_options.option_list := cmd_options.option_list + [process_switch];
  if (in_file_id.len <> 0) then
    cmd_options.option_list := cmd_options.option_list + [input_switch];
end (* reset_options *);
$PAGE lookup_option
(*  LOOKUP_OPTION returns the option switch whose name matches the
    indexed option name from the command line.  'Neg' will be set to
    'true' if the option name is prefixed by 'NO'.  *)

function lookup_option ( ind: option_index; var neg: boolean ): option_switches;

  type
    opt_name_array = array [macro_switch..exit_switch] of string [10];

(* In opt_name the order is relevant so RUN is recognized before RUNOFFSET *)

  const
    opt_name: opt_name_array =
      ( 'MACRO', 'PASCAL', 'LIB', 'SAVE', 'RUN',
				  'RUNOFFSET', 'DUMP', 'SET', 'EXIT' );

  var
    opt: option_switches;
    true_name: string [10];

begin
  with options_in_line[ind].name do begin
    if len = 0 then begin
      lookup_option := bad_option;
      return;
    end;
    neg := (len > 2) andif (substr(cmd_line,loc,2) = 'NO');
    if neg
      then true_name := substr(cmd_line,loc+2,len-2)
      else true_name := substr(cmd_line,loc,len);
  end;
  opt := macro_switch;
  while (opt <> bad_option) andif
    ( (length(true_name) > length(opt_name[opt])) orif
      (true_name <> substr(opt_name[opt],1,length(true_name))) ) do
    opt := succ(opt);
  lookup_option := opt;
end (* lookup_option *);
$PAGE parse_option
(*  PARSE_OPTION parses the option name and value indicated
    by 'options_in_line [ind]'.  If they are good, then it updates
    the specified option record and returns 'true'.  Otherwise,
    it prints an error message and returns 'false'.  *)

function parse_option ( ind: option_index; var opt: option_record ): boolean;

  var
    negated: boolean;
    switch: option_switches;

begin
  parse_option := true;
  with opt, options_in_line [ind] do begin
    switch := lookup_option (ind,negated);
    option_list := option_list + [switch];
    if switch = bad_option then begin
      writeln (tty,'% UNKNOWN OPTION /',substr(cmd_line,name.loc,name.len));
      parse_option := false;
    end
    else if negated then
      option_list := option_list - [switch]
    else if switch in [lib_switch..runoffset_switch] then begin
      switch_values [switch] := substr(cmd_line,val.loc,val.len);
      if  switch = run_switch then begin
        option_list := option_list + [runoffset_switch];
        switch_values[ runoffset_switch ] := '0';  (* Default value *)
      end;
    end
    else if switch = dump_switch then
      dmp_list := substr(cmd_line,val.loc,val.len)
    else if switch = set_switch then begin
      name.loc := val.loc;
      with val do
	while (len <> 0) andif (cmd_line [loc] <> ':') do begin
	  loc := loc + 1;
	  len := len - 1;
	end;
      name.len := val.loc - name.loc;
      if val.len <> 0 then begin
	val.loc := val.loc + 1;
	val.len := val.len - 1;
      end;
      parse_option := parse_option (ind,saved_options);
    end;
  end;
end (* parse_option *);
$PAGE open_files
(*  OPEN_FILES will open the PMF main processing input and output
    files.  OPEN_FILES returns a flag indicating whether there
    were any errors in the file-opening process.  *)

public function open_files: boolean;

  var
    extension: string [5];

begin
  open_files := true;

  if in_file_id.len <> 0 then
    with in_file_id do begin
      if macro_switch in cmd_options.option_list
	then extension := '.MAC '
	else extension := '.PAS ';
      reset (input,extension||substr(cmd_line,loc,len),[ascii]);
      if eof(input) then begin
	writeln (tty,'% BAD INPUT FILE ',substr(cmd_line,loc,len));
	open_files := false;
	return;
      end;
    end;

  if not (macro_switch in cmd_options.option_list) then
    return;

  if out_file_id.len <> 0 then
    with out_file_id do begin
      rewrite (output,'.PAS '||substr(cmd_line,loc,len),[ascii]);
      if not eof(output) then begin
	writeln (tty,'% BAD OUTPUT FILE ',substr(cmd_line,loc,len));
	open_files := false;
	return;
      end;
    end
  else if in_file_id.len <> 0 then
    with in_file_id do
      rewrite (output,substr(cmd_line,loc,len)||'.TMP[]',[ascii]);
end (* open_files *);
$PAGE command
(*  COMMAND is the driver routine for the PMF command line processor.
    It directs the reading and parsing of command lines and the storing
    of the extracted information for the use of the rest of the system.  *)

public procedure command;

  var
    i: option_index;

  label 100;

begin

  if ind_file_index = 0
    then begin
      reset( ind_file[ind_file_index+1], temp_file_name('PMF'));
      if not eof( ind_file[ind_file_index+1] )
	then begin
          auto_indirect_file := true;
	  ind_file_index := ind_file_index + 1;
	  indirect_input := true;
	end;
    end;

100:

  read_cmd_line;

  scan_cmd_line;
  if option_count = max_options then begin
    writeln (tty,'% TOO MANY OPTIONS');
    goto 100;
  end;

  reset_options;
  for i := 1 to option_count do
    if not parse_option (i,cmd_options) then goto 100;

end (* command & command_module *).
  