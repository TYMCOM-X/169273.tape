program stat;

$IF P10
$SYSTEM pascal
$SYSTEM pasfil
$ENDIF

const
$IF P10  num_bytes = 4;
$IF M68  num_bytes = 256;

type
  one_byte    = 0..2 ** 8 - 1;
  two_bytes   = 0..2 ** 16 - 1;
  three_bytes = 0..2 ** 24 - 1;
  four_bytes  = 0..2 ** 32 - 1;
  input_array   = packed array [1..num_bytes] of one_byte;
  string_10 = string[10];
  ESDID_range = 17..255;
  ESDID_ptr = ^ESDID_link_record;
  ESDID_link_record = record
    link_next : ESDID_ptr;
    link_name : string_10;
    link_count : 0..maximum (integer);
  end;
  ESDID_array_record = record
    array_name : string_10;
    array_count : 0..maximum (integer);
  end;

var
  eof_flag : boolean          (* end-of-file on input_file? *);
  tty_flag : boolean          (* output to terminal or file? *);
  id_flag : boolean           (* identification record encountered yet? *);
  end_flag : boolean          (* end record encountered yet? *);
  current_ESDID : ESDID_range (* ESDID count for XREF's *);
  byte_ptr : integer          (* which byte of input_record to get next *);
  byte_count : one_byte       (* number of bytes remaining in current logical record *);
  input_record : input_array  (* current num_bytes bytes read in from the input file *);
  input_file : file of input_array;
$IF P10  output_file : file_block;
$IF M68  output_file : text;
  ESDID_array : array [ESDID_range] of ESDID_array_record;
  ESDID_list_head : ESDID_ptr;
$PAGE array_init
procedure array_init;

begin
  for current_ESDID := minimum (ESDID_range) to maximum (ESDID_range) do begin
    with ESDID_array[current_ESDID] do begin
      array_name := '';
      array_count := 0;
    end (* with ESDID_array[current_ESDID] *);
  end (* for *);
  current_ESDID := minimum (ESDID_range);
end (* array_init *);
$PAGE initialize
procedure initialize;

begin
  open (tty);
  rewrite (tty);
  eof_flag := false;
  tty_flag := true;
  id_flag := false;
  end_flag := false;
  byte_ptr := 1;
  array_init;
  new (ESDID_list_head);
  with ESDID_list_head^ do begin
    link_next := nil;
    link_name := '';
    link_count := 0;
  end (* with ESDID_list_head *);
end (* initialize *);
$PAGE write_string
procedure write_string (input_string : string[*]);

begin
  if tty_flag then
    writeln (tty, input_string)
  else
$IF P10  fio_line (output_file, input_string);
$IF M68  writeln (output_file, input_string);
end (* write_string *);
$PAGE convert_decimal
function convert_decimal (input_number : integer) : string_10;

begin
  putstring (convert_decimal, input_number:0);
end (* convert_decimal *);
$PAGE page_skip
procedure page_skip;

begin
$IF P10  fio_page (output_file);
$IF M68  page (output_file);
end (* page_skip *);
$PAGE write_header
$IF P10
procedure write_header (var fb : file_block);

begin
  fio_line (fb, 'M68000 Runtime call statistics for file: ' || 
	    filename (input_file) || 
	    '         Page ' || 
	    convert_decimal (fb.pageno));
  fio_skip (fb);
end (* write_header *);
$ENDIF
$PAGE warning
procedure warning (input_warning_message : string[*]);

begin
  writeln (tty, '*** Warning: ' || input_warning_message);
  if not tty_flag then begin
    page_skip;
    write_string ('*** Warning: ' || input_warning_message);
  end;
end (* warning *);
$PAGE error
procedure error (input_error_message : string[*]);

begin
  writeln (tty, '*** Error: ' || input_error_message);
  if not tty_flag then begin
    page_skip;
    write_string ('*** Error: ' || input_error_message);
  end;
  stop;
end (* error *);
$PAGE check_flags
procedure check_eof_flag;

begin
  if eof_flag then
    error ('Unexpected EOF.');
end (* check_eof_flag *);

(* ------------------------------------------------------------ *)

procedure check_id_flag;

begin
  if not id_flag then
    warning ('Record encountered before Identification Record.');
end (* check_id_flag *);

(* ------------------------------------------------------------ *)

procedure check_end_flag;

begin
  if end_flag then
    warning ('Record encountered after End Record.');
end (* check_end_flag *);
$PAGE read_one_byte
procedure read_one_byte (var output_one_byte : one_byte);

var
  local_counter : 1..num_bytes;

begin
  output_one_byte := input_record[byte_ptr];
  byte_ptr := byte_ptr + 1;
  if byte_ptr = num_bytes + 1 then begin
    if eof (input_file) then begin
      for local_counter := 1 to num_bytes do
        input_record[local_counter] := 0;
      eof_flag := true;
    end
    else
      read (input_file, input_record);
    byte_ptr := 1;
  end;
end (* read_one_byte *);
$PAGE read_two_bytes
procedure read_two_bytes (var output_two_bytes : two_bytes);

var
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof_flag;
  output_two_bytes := local_one_byte;
  read_one_byte (local_one_byte);
  check_eof_flag;
  output_two_bytes := output_two_bytes * 256 + local_one_byte;
end (* read_two_bytes *);
$PAGE read_three_bytes
procedure read_three_bytes (var output_three_bytes : three_bytes);

var
  local_counter : 1..2;
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof_flag;
  output_three_bytes := local_one_byte;
  for local_counter := 1 to 2 do begin
    read_one_byte (local_one_byte);
    check_eof_flag;
    output_three_bytes := output_three_bytes * 256 + local_one_byte;
  end;
end (* read_three_bytes *);
$PAGE read_four_bytes
procedure read_four_bytes (var output_four_bytes : four_bytes);

var
  local_counter : 1..3;
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof_flag;
  output_four_bytes := local_one_byte;
  for local_counter := 1 to 3 do begin
    read_one_byte (local_one_byte);
    check_eof_flag;
    output_four_bytes := output_four_bytes * 256 + local_one_byte;
  end;
end (* read_four_bytes *);
$PAGE read_string
procedure read_string (input_length : one_byte;
                       var output_string : string[*]);

var
  local_counter : one_byte;
  local_one_byte : one_byte;

begin
  output_string := '';
  for local_counter := 1 to input_length do begin
    read_one_byte (local_one_byte);
    check_eof_flag;
    if local_one_byte > ord (maximum (char)) then
      local_one_byte := ord (' ');
    output_string := output_string || chr (local_one_byte);
  end;
end (* read_string *);
$PAGE process_identification_record
procedure process_identification_record;

var
  local_string : string[255];

begin
  read_string (byte_count - 1, local_string);
  check_eof_flag;
  id_flag := true;
  end_flag := false;
  array_init;
end (* process_identification_record *);
$PAGE process_ESD_record
procedure process_ESD_record;

var
  local_one_byte : one_byte;
  local_four_bytes : four_bytes;
  local_string : string_10;

begin
  check_id_flag;
  check_end_flag;
  read_one_byte (local_one_byte);
  check_eof_flag;
  case local_one_byte div 16 of

    2 : begin
	  read_four_bytes (local_four_bytes);
	end;

    4 : begin
	  read_string (10, local_string);
	  read_four_bytes (local_four_bytes);
	end;

    7 : begin
	  read_string (10, local_string);
          ESDID_array[current_ESDID].array_name := local_string;
          current_ESDID := current_ESDID + 1;
	end;

    others : error ('Illegal ESD record type.');

  end (* case *);
end (* process_ESD_record *);
$PAGE get_bits
function get_bits (input_byte : one_byte;
		   input_last_bit : 0..7;
		   input_number_bits : 1..8) : one_byte;

var
  local_counter : 0..7;
  local_bit : 0..1;
  local_byte : one_byte;

begin
  get_bits := 0;
  local_byte := input_byte;
  for local_counter := 0 to 7 do begin
    local_bit := local_byte mod 2;
    local_byte := local_byte div 2;
    if (local_counter >= input_last_bit) and
       (local_counter < input_last_bit + input_number_bits) then
      get_bits := get_bits + (local_bit * 2 ** (local_counter - input_last_bit));
  end (* for *);
end (* get_bits *);
$PAGE decrement_check_byte_count
procedure decrement_check_byte_count (input_amount : integer);

begin
  byte_count := byte_count - input_amount;
  if byte_count < 0 then
    error ('Incorrect Text Record byte count.');
end (* decrement_check_byte_count *);
$PAGE process_absolute_code
procedure process_absolute_code (input_counter : 0..32);

var
  local_two_bytes : two_bytes;

begin
  read_two_bytes (local_two_bytes);
  decrement_check_byte_count (2);
end (* process_absolute_code *);
$PAGE process_relocation_data
procedure process_relocation_data (input_counter : 0..32);

var
  local_counter : 0..7;
  local_num_ESDID : 0..7;
  local_offset_length : 0..4;
  local_one_byte : one_byte;
  local_two_bytes : two_bytes;
  local_three_bytes : three_bytes;
  local_four_bytes : four_bytes;

begin
  read_one_byte (local_one_byte);
  check_eof_flag;
  decrement_check_byte_count (1);
  local_num_ESDID := get_bits (local_one_byte, 5, 3);
  local_offset_length := get_bits (local_one_byte, 0, 3);
  for local_counter := 1 to local_num_ESDID do begin
    read_one_byte (local_one_byte);
    check_eof_flag;
    decrement_check_byte_count (1);
    if local_one_byte >= minimum (ESDID_range) then
      ESDID_array[local_one_byte].array_count := 
	ESDID_array[local_one_byte].array_count + 1;
  end (* for *);
  case local_offset_length of
    0 : ;
    1 : read_one_byte (local_one_byte);
    2 : read_two_bytes (local_two_bytes);
    3 : read_three_bytes (local_three_bytes);
    4 : read_four_bytes (local_four_bytes);
  end (* case *);
  decrement_check_byte_count (local_offset_length);
end (* process_relocation_data *);
$PAGE process_text_record
procedure process_text_record;

var
  local_counter : 0..32;
  local_map : array [1..4] of one_byte;
  local_ESDID : one_byte;

begin
  check_id_flag;
  check_end_flag;
  decrement_check_byte_count (1);
  for local_counter := 1 to 4 do begin
    read_one_byte (local_map[local_counter]);
    check_eof_flag;
    decrement_check_byte_count (1);
  end (* for *);
  read_one_byte (local_ESDID);
  check_eof_flag;
  decrement_check_byte_count (1);
  local_counter := 0;
  while (local_counter < 32) and
	 (byte_count > 0) do begin
    if get_bits (local_map[ (local_counter div 8) + 1], 
		 7 - (local_counter mod 8), 1) = 0 then
      process_absolute_code (local_counter)
    else
      process_relocation_data (local_counter);
    local_counter := local_counter + 1;
  end (* while *);
  if byte_count <> 0 then
    error ('Incorrect Text Record byte count.');
end (* process_text_record *);
$PAGE add_ESDID_link
procedure add_ESDID_link;

var
  local_index : ESDID_range;
  local_prev_ptr : ESDID_ptr;
  local_curr_ptr : ESDID_ptr;

begin
  local_index := minimum (ESDID_range);
  while (local_index < current_ESDID) andif
        (ESDID_array[local_index].array_name <> '') do begin
    local_prev_ptr := ESDID_list_head;
    local_curr_ptr := local_prev_ptr^.link_next;
    while (local_curr_ptr <> nil) andif
          (local_curr_ptr^.link_name < ESDID_array[local_index].array_name) do begin
      local_prev_ptr := local_curr_ptr;
      local_curr_ptr := local_prev_ptr^.link_next;
    end (* inner while *);
    if local_curr_ptr = nil then begin
      new (local_curr_ptr);
      with local_curr_ptr^, ESDID_array[local_index] do begin
        link_next := nil;
        link_name := array_name;
        link_count := array_count;
      end (* with local_curr_ptr^, ESDID_array[local_index] *);
      local_prev_ptr^.link_next := local_curr_ptr;
    end
    else begin
      if local_curr_ptr^.link_name = ESDID_array[local_index].array_name then
	local_curr_ptr^.link_count := 
	  local_curr_ptr^.link_count + ESDID_array[local_index].array_count
      else begin
        new (local_curr_ptr);
        with local_curr_ptr^, ESDID_array[local_index] do begin
          link_next := local_prev_ptr^.link_next;
          link_name := array_name;
          link_count := array_count;
        end (* with local_curr_ptr^, ESDID_array[local_index] *);
        local_prev_ptr^.link_next := local_curr_ptr;
      end (* inner else *);
    end (* outer else *);
    local_index := local_index + 1;
  end (* outer while *);
end (* add_ESDID_link *);
$PAGE process_end_record
procedure process_end_record;

var
  local_one_byte : one_byte;
  local_four_bytes : four_bytes;

begin
  check_id_flag;
  read_one_byte (local_one_byte);
  check_eof_flag;
  if local_one_byte < 17 then
    read_four_bytes (local_four_bytes);
  end_flag := true;
  add_ESDID_link;
end (* process_end_record *);
$PAGE process_input
procedure process_input;

var
  local_one_byte : one_byte;

begin
  read_one_byte (byte_count);
  while not eof_flag do begin
    if byte_count > 0 then begin
      read_one_byte (local_one_byte);
      check_eof_flag;
      case local_one_byte of
	ord ('1') : process_identification_record;
	ord ('2') : process_ESD_record;
	ord ('3') : process_text_record;
	ord ('4') : process_end_record;
	others    : error ('Illegal record type.');
      end (* case *);
    end (* if *);
    read_one_byte (byte_count);
  end (* while *);
  if not id_flag then
    warning ('Missing Identification Record.');
  if not end_flag then
    warning ('Missing End Record.');
end (* process_input *);
$PAGE print_ESDID_statistics
procedure print_ESDID_statistics;

var
  local_ptr : ESDID_ptr;
  local_num_names : integer;
  local_num_calls : integer;

begin
  write_string ('Runtime Statistics:');
  write_string ('');
  write_string ('Name        Calls');
  write_string ('----------  -----');
  write_string ('');
  local_num_names := 0;
  local_num_calls := 0;
  local_ptr := ESDID_list_head^.link_next;
  while local_ptr <> nil do begin
    with local_ptr^ do begin
      write_string (link_name || '  ' || convert_decimal (link_count));
      local_num_names := local_num_names + 1;
      local_num_calls := local_num_calls + link_count;
    end (* with local_ptr^ *);
    local_ptr := local_ptr^.link_next;
  end (* while *);
  write_string ('');
  write_string ('');
  write_string ('Total number of names = ' || convert_decimal (local_num_names));
  write_string ('Total number of calls = ' || convert_decimal (local_num_calls));
  write_string ('Average calls/name = ' ||
                convert_decimal (local_num_calls div local_num_names));
end (* print_ESDID_statistics *);
$PAGE get_file_names
procedure get_file_names;

var
  input_file_name : file_name;
  output_file_name : file_name;

begin
  writeln (tty, 'M68000 Runtime call statistics program, Version 1.0');
  write (tty, 'Input file: ');
  break (tty);
  readln (tty);
  read (tty, input_file_name);
  if input_file_name = '' then
    stop;  (* <--- No input file *)
  reset (input_file, '.RO ' || input_file_name, [retry]);
  if iostatus (input_file) <> io_ok then
    error ('Bad input file.');
  write (tty, 'Output file: ');
  break (tty);
  readln (tty);
  read (tty, output_file_name);
  if output_file_name <> ' ' then begin
$IF P10  fio_open (output_file, '.LS ' || output_file_name);
$IF M68  rewrite (output_file, '.LS ' || output_file_name);
    if iostatus <> io_ok then
      error ('Bad output file.');
    tty_flag := false;
$IF P10
    with output_file do begin
      page_header := write_header;
      width := 120;
      plength := 42;
    end (* with output_file *);
    page_skip;
$ENDIF
$IF M68
    write_string ('M68000 Runtime call statistics for file: ' ||
                  filename (input_file));
    write_string ('');
$ENDIF
  end;
  read (input_file, input_record);
end (* get_file_names *);
$PAGE main
begin
  initialize;
  get_file_names;
  process_input;
  print_ESDID_statistics;
end.
