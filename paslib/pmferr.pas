$TITLE pmferr -- error message printing and dump routines
$LENGTH 43

module error_module;

$INCLUDE pmf.typ
$PAGE pmfstk.typ
$INCLUDE pmfstk.typ
$PAGE pmfinp.inc
$INCLUDE pmfinp.inc
$PAGE pmfput.inc
$INCLUDE pmfput.inc
$PAGE pmfstk.inc
$INCLUDE pmfstk.inc
$PAGE pmfcmd.inc
$INCLUDE pmfcmd.inc

external procedure err_exit;
$PAGE utilities
procedure dump_args;                            (* Print the arguments of the current macro. *)

  var
    i: arg_index;
    ind: string_index;

begin
  writeln (tty,'  ARGUMENTS:');
  for i := 1 to num_args do begin
    write (tty,i:3,':  "');
    ind := 1;
    while (ind <= 60) andif (arg_char(i,ind) <> etx) do begin
      if arg_char(i,ind) <> eol then
	write (tty,arg_char(i,ind))
      else begin
	writeln (tty,'"');
	write (tty,'      "');
      end;
      ind := ind + 1;
    end;
    if arg_char(i,ind) = etx
      then writeln (tty,'"')
      else writeln (tty,'...');
  end;
end (* dump_args *);


procedure prt_name;                             (* Print the name of the macro at the stack top. *)

begin
  with call_ptr^.def^ do
    if length(name_text) <= 40
      then writeln (tty,substr(name_text,1,length(name_text)))
      else writeln (tty,substr(name_text,1,40),'...');
end (* exp_dump *);



$PAGE error
public procedure error ( code: error_codes );

begin
  case code of
    er_tok_len:
      writeln (tty,'% TOKEN TOO LONG');
    er_lit_len:
      writeln (tty,'% LITERAL STRING TOO LONG');
    er_cmt_len:
      writeln (tty,'% COMMENT LINE TOO LONG');
    er_lit_eol:
      writeln (tty,'% END OF LINE IN LITERAL STRING');
    er_cmt_eof:
      writeln (tty,'% END OF FILE IN COMMENT');
    er_arg_eof:
      begin
	exp_call;
	write (tty,'% END OF FILE IN ARG LIST FOR ');
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
      end;
    er_bad_sym:
      begin
	write (tty,'% BAD SYMBOL IN ARG LIST FOR ');
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
      end;
    er_und_mac:
      begin
	write (tty,'% UNDEFINED MACRO NAME IN CALL TO ');
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
      end;
    er_few_arg:
      begin
	write (tty,'% TOO FEW ARGUMENTS IN CALL TO ');
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
      end;
    er_inp_ofl:
      writeln (tty,'% INPUT STACK OVERFLOW');
    er_tmp_ofl:
      writeln (tty,'% TEMP STACK OVERFLOW');
    er_arg_ind:
      begin
	write (tty,'? PMF FAILURE-BAD ARG INDEX IN CALL TO ');
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
	trace;
      end;
    er_bad_num:
      begin
	write (tty,'% BAD NUMERIC EXPRESSION IN ARG LIST FOR ');
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
      end;
    er_lib_fil:
      begin
	write (tty,'% BAD LIBRARY FILE IN CALL TO ');
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
      end;
    er_str_eof:
      if eof(input) then
	writeln (tty,'% END OF FILE IN QUOTED TEXT')
      else begin
	write (tty,'% UNMATCHED STRING QUOTES IN ARGUMENT TO ');
	exp_call;
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
      end;
    er_err_mac:
      begin
	write (tty,'% CALL TO ');
	begin
	  prt_name;
	  dump_args;
	  end_call;
	end;
      end
  end (* case *);
  if line_no = 0 then
    writeln (tty,'  IN /LIB TEXT')
  else begin
    writeln (tty,'  IN INPUT LINE ',abs(line_no):5,':');
    write (tty,'  ',substr(line_text,1,line_len));
    if not eof(input) then
      while not eoln(input) do begin
	write (tty,input^);
	get (input);
      end;
    writeln (tty);
    if line_no < 0 then write (tty,' ');
    writeln (tty,' ':line_len+1,'^');
  end;
  err_exit;                                     (* Back to the main-line. *)
end (* error *);
$PAGE err_dump
(*  ERR_DUMP interprets a dump descriptor list and dumps macro call
    stack entries according to it.  A dump descriptor has the format:

   <s ! d> <[-]count ! [-]'name[<+!->offset]>

    S means skip (discard) stack entries, D means dump (print) them,
    A means dump them with their arguments.
    COUNT means do for the top n entries, -COUNT means do down to but
    not including the n-th entry from the bottom.  'NAME means do down
    to but not including the top-most entry with the given name, -'NAME
    means do down to but not including the bottom-most entry with the
    given name.  +OFFSET or -OFFSET mean do to the entry n entries
    above or below the named entry, respectively.  *)

public procedure err_dump;

  var
    i: string_index;                            (* Indexes through the dump descriptor list. *)
    cptr: call_pointer;                         (* Used to scan the call stack. *)
    count: integer;                             (* Records the COUNT descriptor value. *)
    total: integer;                             (* Used as a general counter. *)
    dump_flag: char;                            (* The 'S', 'D', or 'A' from the descriptor. *)
    neg_flag: boolean;                          (* Indicates a descriptor preceded by '-'. *)









$PAGE find_name
  (*  FIND_NAME processes a 'NAME dump descriptor.  *)

  procedure find_name;

    var
      i1: string_index;                           (* The location of the name in the dump list. *)
      l1: string_index;                           (* The length of the name. *)

  begin

    while not (cmd_options.dmp_list [i] in alphas) do
      i := i + 1;                             (* Find the start of the name. *)
    i1 := i;

    while not (cmd_options.dmp_list [i] in [' '..'_']-alphanumerics) do
      i := i + 1;                           (* Find the end of the name. *)
    l1 := i - i1;
    count := 0;
    total := 0;                                   (* Total will count the current stack depth. *)
    cptr := call_ptr;                             (* Start scanning from the stack top. *)
    repeat                                        (* Until name found or end of stack. *)
      while (cptr <> nil) andif                   (* Find an occurrence of the name. *)
	(substr(cmd_options.dmp_list,i1,l1) <>
	  substr(cptr^.def^.name_text,1,length(cptr^.def^.name_text)))  do begin
	total := total + 1;
	cptr := cptr^.down;
      end;
      if cptr <> nil then begin                   (* Name found in stack. *)
	count := total;                           (* Record its location. *)
	total := total + 1;                       (* Ready to look for another. *)
	cptr := cptr^.down;
      end;
    until (not neg_flag) or (cptr = nil);


    while not (cmd_options.dmp_list [i] in ['S','D','A','+','-','/']) do
      i := i + 1;
    if cmd_options.dmp_list [i] in ['+','-'] then begin       (* Compute the offset. *)
      neg_flag := (cmd_options.dmp_list [i] = '-');

      while not (cmd_options.dmp_list [i] in ['S','D','A','0'..'9','/']) do
	i := i + 1;

      begin
	total := 0;
	while cmd_options.dmp_list [i] in ['0'..'9'] do begin
	  total := total*10 + (ord(cmd_options.dmp_list[i])-ord('0'));
	  i := i + 1;
	end;
      end;
      if neg_flag
	then count := count + total
	else count := count - total;
    end;
    neg_flag := false;                            (* Named indices are never treated as '-'. *)
  end (* find_name *);
$PAGE dump_top
  (*  DUMP_TOP prints data about the top-most call stack entry, according
      to the 'dump_flag', and then removes the entry from the stack.  *)

  procedure dump_top;

  begin
    if call_ptr = chn_collect then begin          (* Macro was collecting arguments. *)
      exp_call;                                   (* Convert it to an expanding entry. *)
      if dump_flag <> 'S' then begin              (* Print the name. *)
	writeln (tty);
	write (tty,'  COLLECTING ARGUMENTS FOR ');
	prt_name;
      end;
      if dump_flag = 'A' then                     (* Print the arguments. *)
	dump_args;
    end
    else                                          (* Macro was being expanded. *)
      if dump_flag <> 'S' then begin              (* Print the name. *)
	writeln (tty);
	write (tty,'  EXPANDING ');
	prt_name;
      end;
    end_call;                                     (* Remove the call stack entry. *)
  end (* dump_top *);
$PAGE err_dump main procedure
begin
  if call_ptr = nil then return;                (* Nothing to dump. *)
  if cmd_options.dmp_list = ''
    then cmd_options.dmp_list := 'D-/'                      (* Default -- dump everything. *)
    else cmd_options.dmp_list := cmd_options.dmp_list || '/';           (* Mark the list end. *)
  i := 1;                                       (* Start scanning the dump list. *)

  while not (cmd_options.dmp_list [i] in ['S','D','A','/']) do
    i := i + 1;
  while cmd_options.dmp_list [i] <> '/' do begin
    dump_flag := cmd_options.dmp_list [i];
    i := i + 1;

    while not (cmd_options.dmp_list [i] in ['S','D','A','-','''','0'..'9','/']) do
      i := i + 1;
    neg_flag := (cmd_options.dmp_list [i] = '-');
    if neg_flag then
      while not (cmd_options.dmp_list [i] in ['S','D','A','''','0'..'9','/']) do
	i := i + 1;
    if cmd_options.dmp_list [i] = ''''
      then find_name                            (* Macro name descriptor. *)
      else
	begin
	  count := 0;
	  while cmd_options.dmp_list [i] in ['0'..'9'] do begin
	    count := count*10 + (ord(cmd_options.dmp_list[i])-ord('0'));
	    i := i + 1;
	  end;
	end;                                (* Absolute count descriptor. *)
    total := 0;                                 (* Count the total number of call stack entries. *)
    cptr := call_ptr;
    while cptr <> nil do begin
      cptr := cptr^.down;
      total := total + 1;
    end;
    if neg_flag then
      if total >= count
	then count := total - count
	else count := 0
    else
      if count > total then
      count := total;
    for total := 1 to count do
      dump_top;

    while not (cmd_options.dmp_list [i] in ['S','D','A','/']) do
      i := i + 1;
  end;
end (* err_dump *).
