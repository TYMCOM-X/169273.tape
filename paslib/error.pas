$OPTIONS SPECIAL, NOCHECK

$LENGTH=44

$TITLE Error Compiler

program error;

$PAGE declarations

const
    prefix := 'err_';
    warning := 1;
    error := 2;
    fatal_error := 3;
    max_string_length := 130;
    min_user_number := 0;
    max_user_number := 9999;
    open_comment := '(* ';
    close_comment := ' *)';
    tab := chr (11b);
    comma := ',';

type
    scalar_string = string[30]; (* maximum length of scalar_id *)
    string_range = 1..max_string_length; (* max legnth of error message line *)
    level_range = warning..fatal_error;
    user_range = min_user_number..max_user_number;
    line_pointer = ^error_message; (* link lines of error message *)
    error_message = packed record
      next_line: line_pointer; (* link multiple line messages *)
      line_length: string_range; (* length of line *)
      line: packed array[string_range] of char (* text of message *)
    end;
    error_indicator = ^error_record; (* link entries in tree *)
    error_record = packed record
      lesser_errors: error_indicator; (* subtree of lesser user numbers *)
      greater_errors: error_indicator; (* subtree of greater user numbers *)
      next_error: error_indicator; (* chain entries as declared *)
      message: line_pointer; (* to first line of error message *)
      scalar_id: scalar_string; (* scalar identifier *)
      level: level_range; (* severity level *)
      user_number: user_range (* user error number *)
    end;

var
    head_of_tree: error_indicator; (* head of unbalanced binary tree *)
    first_error: error_indicator; (* first scalar encountered *)
    last_error: error_indicator; (* last scalar *)
    error_count: integer; (* global count of scalars declared *)






label
    1; (* non-local goto into mainline on errors *)
$PAGE in_error

    (* routine "in_error" is called when an error condition is noted.
       the parameter determines the message to be printed to the terminal
       before jumping back into the mainline.               *)






type
    err_code = (nosource,bad_format);

procedure in_error (error_code: err_code);

begin
  case error_code of
    nosource:
      writeln (tty,'Can''t find source file.  Try again.');
    bad_format:
      writeln (tty,'Bad input file format.')
  end;
  goto 1;
end;

$PAGE get_source_file

(* routine "get_source_file" reads a filename from the terminal
   and attempts to open the input file.  calls in_error if file not found *)






procedure get_source_file;

const
    prompt := 'File: ';
    default_extension := '.TXT ';

var
    filename: string;
    ch: char;

begin
  writeln (tty);
  write (tty,prompt);
  break (tty);
  readln (tty);
  if eoln (tty) then
    stop; (*allow graceful abort *)
  filename := '';
  repeat
    read (tty,ch);
    filename := filename || ch;
  until eoln (tty);
  reset (input,default_extension || filename);
  if eof (input) (* cannot find file? *)
  then
    in_error (nosource);
end;
$PAGE enter_error

(* routine "enter_error" accepts a pointer to a record describing
   an error entry to be linked into the scalar tree.  if the user number
   is multiply defined a warning is printed to tty. *)






procedure enter_error (new_error: error_indicator);

var
    err: error_indicator; (* used in tree search *)





begin
  if head_of_tree = nil then
    head_of_tree := new_error (* first scalar *)
  else begin
    err := head_of_tree;
    while err <> nil do
      with err^ do begin
	if new_error^.user_number < user_number then begin
	  err := lesser_errors; (* check left subtree *)
	  if err = nil then
	    lesser_errors := new_error;
	end
	else if new_error^.user_number > user_number then begin
	  err := greater_errors; (* check right subtree *)
	  if err = nil then
	    greater_errors := new_error
	end
	else begin
	  writeln (tty);
	  writeln (tty,'Warning: user_number ',user_number:4,
	    ' is multiply defined.');
	  break;
	  err := greater_errors (* enter anyway *)
	end
      end (* while, with *);
  end; (* tree search *)
  if first_error = nil then
    first_error := new_error
  else
    last_error^.next_error := new_error;
  last_error := new_error;
  with new_error^ do begin
  (* complete misc. info *)
    next_error := nil;
    message := nil;
    lesser_errors := nil;
    greater_errors := nil;
  end;
  error_count := error_count + 1;
end; (* enter_error *)
$PAGE read_entries

(* routine "read_entries" reads scalar descriptions from the open input
   file, recording the data and calling "enter_error" to build the tree. *)





procedure read_entries;

(* local routine to initialize relevant global variables *)









  procedure initialize_globals;

  begin
    head_of_tree := nil;
    first_error := nil;
    last_error := nil;
    error_count := 0;
  end;

  (* "make_string" converts the user number to a four-character string
     for errors with no scalar assigned *)


type
    string4 = packed array[1..4] of char;


  function make_string (number: user_range): string4;

  var
      val: user_range;
      index: 1..4;

  begin
    val := abs (number);
    for index := 4 downto 1 do begin
      make_string[index] := chr ( (val mod 10) + ord('0'));
      val := val div 10;
    end;
  end;

type
    set_of_char = set of char;

var
    scalar: scalar_string; (* name of scalar *)
    current_line, last_line: line_pointer; (* for multiple line message *)
    error_number: user_range; (* user error number *)
    ch: char;
    current_error: error_indicator;
    severity: level_range;
    error_line: string[max_string_length];

static var
    scalar_set: set_of_char := ['A'..'Z','_','0'..'9'];

begin
  initialize_globals;
  while not eof (input) do begin (* scan input file *)
    if not eoln (input) then begin (* have scalar description *)
      read (ch);
      scalar := '';
      if ch = '-' then (* scalar_id omitted, skip *)
	read (ch)
      else begin
	while uppercase (ch) in scalar_set do begin
	  scalar := scalar || ch;
	  read (ch);
	end;
      end;
      if ch = comma then
	read (ch)
      else
	in_error (bad_format);
      new (current_error);
      with current_error^ do begin
	read (severity);
	level := severity;
	if input^ = comma then
	  read (ch)
	else
	  in_error (bad_format);
	read (error_number);
	user_number := error_number;
	enter_error (current_error); (* link in tree *)
	if scalar = '' then (* id omitted *)
	  scalar_id := make_string (user_number)
	else
	  scalar_id := scalar;
	readln; (* first line of message, if any *)
	current_line := nil;
	last_line := nil;
	while not eoln do begin (* message terminated by blank line *)
	  error_line := '';
	  repeat
	    read (ch);
	    error_line := error_line || ch
	  until eoln;
	  new (current_line:length(error_line));
	  with current_line^ do begin
	    next_line := nil;
	    line_length := length (error_line);
	    line[1:line_length] := error_line;
	  end;
	  if last_line = nil then
	    message := current_line
	  else
	    last_line^.next_line := current_line;
	  last_line := current_line;
	  readln; (* next line of message *)
	end; (* multiple line read *)
      end; (* with current_error *)
    end; (* if not eoln *)
    readln;
  end; (* while not eof *)
  close (input);
end; (* read_entries *)
$PAGE write_codes

(* routine "write_codes" produces the scalar type definition for
       "err_codes", declaring the scalars as read. *)



procedure write_codes;

const
    type_file := 'PASERR.TYP';

var
    err: error_indicator;
    i: integer;

begin
  rewrite (output,type_file);
  writeln ('type');
  writeln (' ':4,'err_codes = (');
  writeln (' ':6,open_comment,'scalar_id',tab,tab,'user_number',close_comment) ;
  err := first_error; (* sequential list as declared *)
  while err <> nil do
    with err^ do begin
      write (tab,prefix,scalar_id);
      i := 8 (* first tab *)+ 4 (* prefix *)+ length (scalar_id);
      (* number columns used so far *)
      while i < 32 do begin
      (* tab out to align comments *)
	write (tab);
	i := i + 8;
      end;
      write (open_comment,user_number:4,close_comment);
      if next_error <> nil then
	writeln (comma)
      else
	writeln (');');
      err := next_error;
    end; (* while, with *)
  writeln;
  close (output);
end; (* write_codes *)
$PAGE write_summary

(* routine "write_summary" prints to the terminal any data collected on the
   error messages *)


procedure write_summary;

begin
  writeln (tty,error_count:4,' error messages compiled.');
  break;
end;
$PAGE write_tables_etc

(* routine "write_tables_etc" produces a table of error numbers and severity levels
       for the error listing routines as well as an ordered listing of messages. *)



var
    listing: text; (* list of error messages *)
    pasmsg: text; (* input for error routines *)


procedure write_tables_etc;

const
    left_paren := '(';
    right_paren := ')';
    table_file := 'PASERR.TAB[,]';
    message_file := 'PASERR.TXT[,]';
    list_file := 'ERRORS.LST[,]';

var
    not_first_error: boolean; (* false for first scalar written after header *)
    line_ptr: line_pointer;

$PAGE write_entry

    (* recursive routine "write_entry" writes data on one scalar
	     during the tree walk. *)





  procedure write_entry (err: error_indicator);

  begin
    if err <> nil then
      with err^ do begin
	write_entry (lesser_errors); (* tree walk *)
	if not_first_error then
	  writeln (comma)
	else
	  not_first_error := true;
	write (tab,left_paren,user_number:4,comma,level:2,right_paren);
	write (tab,open_comment,scalar_id,close_comment);
	write (listing,user_number:4,' ');
	write (pasmsg,user_number:4,' ');
	line_ptr := message;
	if line_ptr = nil then
	  writeln (listing)
	else begin
	  with line_ptr^ do begin
	    writeln (listing,line:line_length);
	    writeln (pasmsg,line:line_length);
	  end;
	  line_ptr := line_ptr^.next_line;
	  while line_ptr <> nil do
	    with line_ptr^ do begin
	      writeln (listing,' ':5,line:line_length);
	      writeln (pasmsg,' ':5,line:line_length);
	      line_ptr := next_line;
	    end;
	end;
	writeln (pasmsg);
	write_entry (greater_errors);
      end; (* with *)
  end; (* write_entry *)


$PAGE write_tables_etc body

begin (* write_tables *)
  rewrite (output,table_file);
  rewrite (listing,list_file);
  rewrite (pasmsg,message_file);
  writeln (listing,' Error messages compiled on ',date:9);
  writeln (listing);
  writeln (pasmsg);
  writeln;
  writeln ('type');
  writeln (' ':4,'user_range = 0..9999;');
  writeln (' ':4,'level_range = 0..3;');
  writeln (' ':4,'error_table= array[err_codes] of packed record');
  writeln (' ':6,'user_number: user_range;');
  writeln (' ':6,'level: level_range');
  writeln (' ':4,'end;');
  writeln;
  writeln ('public const');
  writeln ('    err_table: error_table := (');
  not_first_error := false;
  write_entry (head_of_tree);
  writeln (');');	(* close constant initialization *)
  close (output);
  close (listing);
  close (pasmsg);
end; (* write_tables_etc *)
$PAGE PASERR body

begin
  rewrite (tty);
  open (tty);
  1: (* target of nonlocal goto from in_error after errors *)
    get_source_file;
  read_entries;
  write_codes;
  write_tables_etc;
  write_summary;
end. (* paserr mainline *)
  