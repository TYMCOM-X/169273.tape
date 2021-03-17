$TITLE M68DMP - M68000 Object File Analyzer
program m68dmp;

$SYSTEM pascal
$SYSTEM pasfil

type
  one_byte    = 0..2 ** 8 - 1;
  two_bytes   = 0..2 ** 16 - 1;
  three_bytes = 0..2 ** 24 - 1;
  four_bytes  = 0..2 ** 32 - 1;
  fake_word   = packed array [1..4] of one_byte;
  string_10 = string[10];
  ESDID_range = 17..255;
$IF STATS
  ESDID_record = record
    name : string_10;
    count : 0..maximum (integer);
  end;
$ENDIF

var
  tty_flag : boolean          (* output to terminal or file? *);
  id_flag : boolean           (* identification record encountered yet? *);
  end_flag : boolean          (* end record encountered yet? *);
  current_ESDID : ESDID_range (* ESDID count for XREF's *);
  byte_ptr : integer          (* which byte of input_word to get next *);
  byte_count : integer        (* number of bytes remaining in current logical record *);
  skip_count : integer        (* number of zero-length records skipped *);
  input_word : fake_word      (* current four bytes read in from the input file *);
  input_file : file of fake_word;
  output_file : file_block;
$IF STATS
  ESDID_array : array [ESDID_range] of ESDID_record;
$ENDIF
$PAGE initialize
(* INITIALIZE - opens tty and initializes global variables. *)

procedure initialize;

$IF STATS
var
  local_index : ESDID_range;
$ENDIF

begin
  open (tty);
  rewrite (tty);
  tty_flag := true;
  id_flag := false;
  end_flag := false;
  current_ESDID := minimum (ESDID_range);
  byte_ptr := 1;
  skip_count := 0;
$IF STATS
  for local_index := minimum (ESDID_range) to maximum (ESDID_range) do begin
    with ESDID_array[local_index] do begin
      name := '';
      count := 0;
    end (* with ESDID_array[local_index] *);
  end (* for *);
$ENDIF
end (* initialize *);
$PAGE write_string
(* WRITE_STRING - writes a string to either the terminal or the output file. *)

procedure write_string (input_string : string[*]);

begin
  if tty_flag then
    writeln (tty, input_string)
  else
    fio_line (output_file, input_string);
end (* write_string *);
$PAGE convert_one_byte
(* CONVERT_ONE_BYTE - converts a one-byte value to hexadecimal characters. *)

function convert_one_byte (input_one_byte : one_byte) : string_10;

begin
  putstring (convert_one_byte, input_one_byte:2:h)
end (* convert_one_byte *);
$PAGE convert_two_bytes
(* CONVERT_TWO_BYTES - converts a two-byte value to hexadecimal characters. *)

function convert_two_bytes (input_two_bytes : two_bytes) : string_10;

begin
  putstring (convert_two_bytes, input_two_bytes:4:h)
end (* convert_two_bytes *);
$PAGE convert_three_bytes
(* CONVERT_THREE_BYTES - converts a three-byte value to hexadecimal characters. *)

function convert_three_bytes (input_three_bytes : three_bytes) : string_10;

begin
  putstring (convert_three_bytes, input_three_bytes:6:h)
end (* convert_three_bytes *);
$PAGE convert_four_bytes
(* CONVERT_FOUR_BYTES - converts a four-byte value to hexadecimal characters. *)

function convert_four_bytes (input_four_bytes : four_bytes) : string_10;

begin
  putstring (convert_four_bytes, input_four_bytes:8:h)
end (* convert_four_bytes *);
$PAGE convert_decimal
function convert_decimal (input_number : integer;
                          input_size : 0..10) : string_10;

begin
  putstring (convert_decimal, input_number:input_size);
end (* convert_decimal *);
$PAGE write_header
(* WRITE_HEADER - is called by fio_page in the fio package to write
                  a page header at the top of each new page. *)

procedure write_header (var fb : file_block);

begin
  fio_line (fb, 'M68000 Object File Analysis for file: ' || 
	    filename (input_file) || 
	    '         Page ' || 
	    convert_decimal (fb.pageno, 0));
  fio_skip (fb);
end (* write_header *);
$PAGE warning
(* WARNING - writes a warning message to the terminal, and possibly
             to the output file. *)

procedure warning (input_warning_message : string[*]);

begin
  writeln (tty, '*** Warning: ' || input_warning_message);
  if not tty_flag then begin
    fio_page (output_file);
    write_string ('*** Warning: ' || input_warning_message);
  end;
end (* warning *);
$PAGE error
(* ERROR - writes an error message to the terminal, and possibly to the
           output file, then closes all open files and stops execution. *)

procedure error (input_error_message : string[*]);

begin
  writeln (tty, '*** Error: ' || input_error_message);
  if not tty_flag then begin
    fio_page (output_file);
    write_string ('*** Error: ' || input_error_message);
  end;
  close;
  stop;
end (* error *);
$PAGE check_eof
(* CHECK_EOF - checks to see if an unexpected end-of-file condition has
               been encountered on the input file, which is an error. *)

procedure check_eof;

begin
  if eof (input_file) then
    error ('Unexpected EOF.');
end (* check_eof *);
$PAGE read_one_byte
(* READ_ONE_BYTE - gets the next byte from input_word, as indicated by
                   byte_ptr.  If necessary, it reads a new four-byte
                   input_word and resets byte_ptr to 1. *)

procedure read_one_byte (var output_one_byte : one_byte);

begin
  output_one_byte := input_word[byte_ptr];
  byte_ptr := byte_ptr + 1;
  if byte_ptr = 5 then begin
    if eof (input_file) then
      input_word := (0, 0, 0, 0)
    else
      read (input_file, input_word);
    byte_ptr := 1;
  end;
end (* read_one_byte *);
$PAGE read_two_bytes
(* READ_TWO_BYTES - calls read_one_byte twice and combines these two
                    separate bytes into a single two-byte value. *)

procedure read_two_bytes (var output_two_bytes : two_bytes);

var
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof;
  output_two_bytes := local_one_byte;
  read_one_byte (local_one_byte);
  check_eof;
  output_two_bytes := output_two_bytes * 256 + local_one_byte;
end (* read_two_bytes *);
$PAGE read_three_bytes
(* READ_THREE_BYTES - calls read_one_byte three times and combines these
		      three separate bytes into a single three-byte value. *)

procedure read_three_bytes (var output_three_bytes : three_bytes);

var
  local_counter : one_byte;
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof;
  output_three_bytes := local_one_byte;
  for local_counter := 1 to 2 do begin
    read_one_byte (local_one_byte);
    check_eof;
    output_three_bytes := output_three_bytes * 256 + local_one_byte;
  end;
end (* read_three_bytes *);
$PAGE read_four_bytes
(* READ_FOUR_BYTES - calls read_one_byte four times and combines these
                     four separate bytes into a single four-byte value. *)

procedure read_four_bytes (var output_four_bytes : four_bytes);

var
  local_counter : one_byte;
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof;
  output_four_bytes := local_one_byte;
  for local_counter := 1 to 3 do begin
    read_one_byte (local_one_byte);
    check_eof;
    output_four_bytes := output_four_bytes * 256 + local_one_byte;
  end;
end (* read_four_bytes *);
$PAGE read_string
(* READ_STRING - reads in a given number of bytes, converting each one
                 into a character, and concatenating them into a string. *)

procedure read_string (input_length : one_byte;
                       var output_string : string[*]);

var
  local_counter : one_byte;
  local_one_byte : one_byte;

begin
  output_string := '';
  for local_counter := 1 to input_length do begin
    read_one_byte (local_one_byte);
    check_eof;
    output_string := output_string || chr (local_one_byte);
  end;
end (* read_string *);
$PAGE motorola_format_dump
procedure motorola_format_dump;

var
  local_code_string : string[100];
  local_char_string : string[25];
  local_char : char;
  local_segment_number : one_byte;
  local_char_number : integer;
  local_one_byte : one_byte;
  local_file_name : file_name;

begin
  local_code_string := '';
  local_char_string := '';
  local_segment_number := 0;
  local_char_number := 0;
  repeat
    if local_char_number mod 256 = 0 then begin
      write_string (local_code_string || local_char_string);
      write_string ('');
      local_code_string := '    SN=$' || 
			   convert_one_byte (local_segment_number); 
      local_char_string := '      ' ||
			   convert_decimal (local_segment_number, 0);
      local_segment_number := local_segment_number + 1;
    end;
    if local_char_number mod 16 = 0 then begin
      write_string (local_code_string || local_char_string);
      local_code_string := convert_one_byte (local_char_number mod 256) || '    ';
      local_char_string := '     ';
    end;
    if local_char_number mod 16 = 8 then
      local_code_string := local_code_string || ' ';
    read_one_byte (local_one_byte);
    local_code_string := local_code_string || convert_one_byte (local_one_byte) || ' ';
    local_char := '.';
    if chr (local_one_byte) in [' '..'}'] then
      local_char := chr (local_one_byte);
    local_char_string := local_char_string || local_char;
    local_char_number := local_char_number + 1;
  until eof (input_file);
  for local_char_number := 1 to 4 do begin
    read_one_byte (local_one_byte);
    local_code_string := local_code_string || convert_one_byte (local_one_byte) || ' ';
    local_char := '.';
    if chr (local_one_byte) in [' '..'}'] then
      local_char := chr (local_one_byte);
    local_char_string := local_char_string || local_char;
  end (* for loop *);
  write_string (local_code_string || local_char_string);
  local_file_name := filename (input_file);
  close (input_file);
  reset (input_file, local_file_name);
  if iostatus (input_file) <> io_ok then
    error ('Cannot re-open input file.');
  read (input_file, input_word);
  byte_ptr := 1;
  if not tty_flag then
    fio_page (output_file);
end (* motorola_format_dump *);
$PAGE process_identification_record
(* PROCESS_IDENTIFICATION_RECORD - breaks an identification record up into
				   it's component fields. *)

procedure process_identification_record;

type
  string_211 = string[211];

var
  local_one_byte : one_byte;
  local_two_bytes : two_bytes;
  local_string : string_211;

begin
  if end_flag then
    warning ('Record encountered after End Record.');
  write_string ('');
  write_string ('Identification record:');
  read_string (10, local_string);
  write_string ('   Module name = ''' || local_string || '''');
  read_one_byte (local_one_byte);
  check_eof;
  write_string ('   Module version number = ' || 
		convert_decimal (local_one_byte, 0));
  read_one_byte (local_one_byte);
  check_eof;
  write_string ('   Module revision number = ' || 
		convert_decimal (local_one_byte, 0));
  read_string (1, local_string);
  write_string ('   Language processor = ''' || local_string || '''');
  read_string (4, local_string);
  write_string ('   Source file volume name = ''' || local_string || '''');
  read_two_bytes (local_two_bytes);
  write_string ('   Source file user number = ' || 
		convert_decimal (local_two_bytes, 0));
  read_string (8, local_string);
  write_string ('   Source file catalog name = ''' || local_string || '''');
  read_string (8, local_string);
  write_string ('   Source file file name = ''' || local_string || '''');
  read_string (2, local_string);
  write_string ('   Source file extension = ''' || local_string || '''');
  local_string := '  :  :  ';
  read_one_byte (local_one_byte);
  check_eof;
  putstring (substr (local_string, 1, 2), local_one_byte:2:h);
  read_one_byte (local_one_byte);
  check_eof;
  putstring (substr (local_string, 4, 2), local_one_byte:2:h);
  read_one_byte (local_one_byte);
  check_eof;
  putstring (substr (local_string, 7, 2), local_one_byte:2:h);
  write_string ('   Module creation time = ' || local_string);
  local_string := '  /  /  ';
  read_one_byte (local_one_byte);
  check_eof;
  putstring (substr (local_string, 1, 2), local_one_byte:2:h);
  read_one_byte (local_one_byte);
  check_eof;
  putstring (substr (local_string, 4, 2), local_one_byte:2:h);
  read_one_byte (local_one_byte);
  check_eof;
  putstring (substr (local_string, 7, 2), local_one_byte:2:h);
  write_string ('   Module creation date = ' || local_string);
  read_string (byte_count - 44, local_string);
  write_string ('   Module description = ''' || local_string || '''');
  id_flag := true;
end (* process_identification_record *);
$PAGE process_ESD_record
(* PROCESS_ESD_RECORD - breaks an ESD record into it's component fields, 
                        after using the left digit of the first byte to
                        determine the record subtype. *)

procedure process_ESD_record;

type
  string_10 = string[10];

var
  local_one_byte : one_byte;
  local_four_bytes : four_bytes;
  local_string : string_10;

begin
  if not id_flag then
    warning ('Record encountered before Identification Record.');
  if end_flag then
    warning ('Record encountered after End Record.');
  write_string ('');
  read_one_byte (local_one_byte);
  check_eof;
  case local_one_byte div 16 of

    2 : begin
	  write_string ('Standard relocatable section:');
	  write_string ('   Section = $' || 
			convert_one_byte (local_one_byte mod 16) ||
			'  (' || 
			convert_decimal (local_one_byte mod 16, 0) || 
                        ')');
	  read_four_bytes (local_four_bytes);
	  write_string ('   Size = $' || 
			convert_four_bytes (local_four_bytes) || 
			'  (' || 
			convert_decimal (local_four_bytes, 0) || 
                        ')');
	end;

    4 : begin
	  write_string ('Relocatable XDEF:');
	  write_string ('   Section = $' || 
			convert_one_byte (local_one_byte mod 16) ||
			'  (' || 
			convert_decimal (local_one_byte mod 16, 0) || 
                        ')');
	  read_string (10, local_string);
	  write_string ('   XDEF = ''' || local_string || '''');
	  read_four_bytes (local_four_bytes);
	  write_string ('   Address = $' || 
			convert_four_bytes (local_four_bytes));
	end;

    7 : begin
	  write_string ('Absolute XREF:');
	  read_string (10, local_string);
	  write_string ('   XREF = ''' || local_string || '''');
          write_string ('   ESDID = $' ||
			convert_one_byte (current_ESDID) ||
			'  (' || 
			convert_decimal (current_ESDID, 0) || 
                        ')');
$IF STATS
          ESDID_array[current_ESDID].name := local_string;
$ENDIF
          current_ESDID := current_ESDID + 1;
	end;

    others : error ('Illegal ESD record type.');

  end (* case *);
end (* process_ESD_record *);
$PAGE get_bits
(* GET_BITS - returns the value of the given bit range for a given byte, 
              with the bits numbered in Motorola fashion (7 6 5 4 3 2 1 0). *) 

function get_bits (input_byte : one_byte;
		   input_last_bit : 0..7;
		   input_number_bits : one_byte) : one_byte;

var
  local_counter : one_byte;
  local_bit : one_byte;
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
(* DECREMENT_CHECK_BYTE_COUNT - subtracts a given amount from the number of
                                bytes remaining in the current logical
                                record, and signals an error if the count
                                goes negative. *)

procedure decrement_check_byte_count (input_amount : integer);

begin
  byte_count := byte_count - input_amount;
  if byte_count < 0 then
    error ('Incorrect Text Record byte count.');
end (* decrement_check_byte_count *);
$PAGE process_absolute_code
(* PROCESS_ABSOLUTE_CODE - writes out the two bytes of absolute code that
                           make up an entry in a text record. *)

procedure process_absolute_code (input_counter : 0..32);

var
  local_two_bytes : two_bytes;

begin
  read_two_bytes (local_two_bytes);
  decrement_check_byte_count (2);
  write_string ('   (' ||
		convert_decimal (input_counter + 1, 2) ||
		') Absolute code: $' ||
		convert_two_bytes (local_two_bytes));
end (* process_absolute_code *);
$PAGE process_relocation_data
(* PROCESS_RELOCATION_DATA - breaks a relocation data entry from a text
                             record up into it's component fields. *)

procedure process_relocation_data (input_counter : 0..32);

var
  local_counter : one_byte;
  local_num_ESDID : one_byte;
  local_data_size : one_byte;
  local_offset_length : one_byte;
  local_one_byte : one_byte;
  local_two_bytes : two_bytes;
  local_three_bytes : three_bytes;
  local_four_bytes : four_bytes;

begin
  write_string ('   (' ||
                convert_decimal (input_counter + 1, 2) ||
                ') Relocation data:');
  read_one_byte (local_one_byte);
  check_eof;
  decrement_check_byte_count (1);
  local_num_ESDID := get_bits (local_one_byte, 5, 3);
  local_data_size := get_bits (local_one_byte, 3, 1) + 1;
  local_offset_length := get_bits (local_one_byte, 0, 3);
  write_string ('           Flag byte = $' || 
		convert_one_byte (local_one_byte) ||
		' [Number ESDIDs = ' ||
		convert_decimal (local_num_ESDID, 0) ||
		', Data size = ' ||
		convert_decimal (local_data_size, 0) ||
		' word(s), Offset length = ' ||
		convert_decimal (local_offset_length, 0) ||
                ' byte(s)]');
  for local_counter := 1 to local_num_ESDID do begin
    read_one_byte (local_one_byte);
    check_eof;
    decrement_check_byte_count (1);
    write_string ('           ESDID = $' || 
		  convert_one_byte (local_one_byte) ||
		  '  (' || 
		  convert_decimal (local_one_byte, 0) || 
		  ')');
$IF STATS
    if local_one_byte >= minimum (ESDID_range) then
      ESDID_array[local_one_byte].count := ESDID_array[local_one_byte].count + 1;
$ENDIF
 (* for *);
  case local_offset_length of
    0 : ;
    1 : read_one_byte (local_one_byte);
    2 : read_two_bytes (local_two_bytes);
    3 : read_three_bytes (local_three_bytes);
    4 : read_four_bytes (local_four_bytes);
    others : error ('Illegal offset length.');
  end (* case *);
  decrement_check_byte_count (local_offset_length);
  case local_offset_length of
    0 : ;
    1 : write_string ('           Offset = $' || 
		      convert_one_byte (local_one_byte));
    2 : write_string ('           Offset = $' || 
		      convert_two_bytes (local_two_bytes));
    3 : write_string ('           Offset = $' || 
		      convert_three_bytes (local_three_bytes));
    4 : write_string ('           Offset = $' || 
		      convert_four_bytes (local_four_bytes));
    others : error ('Illegal offset length.');
  end (* case *);
end (* process_relocation_data *);
$PAGE convert_binary
(* CONVERT_BINARY - converts a byte into printable characters that
                    represent it's bit pattern. *)

function convert_binary (input_one_byte : one_byte) : string_10;

const
  bit_char : array [0..1] of char = ('0', '1');

var
  local_counter : 0..7;

begin
  convert_binary := '';
  for local_counter := 7 downto 0 do begin
    convert_binary := convert_binary ||
                      bit_char[get_bits (input_one_byte, local_counter, 1)];
    if local_counter = 4 then
      convert_binary := convert_binary || ' ';
  end (* for loop *);
end (* convert_binary *);
$PAGE process_text_record
(* PROCESS_TEXT_RECORD - breaks up a text record into it's component fields
                         using the four bytes of bit maps read in to 
                         determine the type of each of the next 1 to 32
                         sub-records.  *)

procedure process_text_record;

var
  local_counter : 0..32;
  local_map : array [1..4] of one_byte;
  local_ESDID : one_byte;

begin
  if not id_flag then
    warning ('Record encountered before Identification Record.');
  if end_flag then
    warning ('Record encountered after End Record.');
  decrement_check_byte_count (1);
  write_string ('');
  write_string ('Text record:');
  for local_counter := 1 to 4 do begin
    read_one_byte (local_map[local_counter]);
    check_eof;
    decrement_check_byte_count (1);
    write_string ('   Map byte = $' || 
		  convert_one_byte (local_map[local_counter]) ||
                  '  (' ||
                  convert_binary (local_map[local_counter]) ||
                  ')');
  end (* for *);
  read_one_byte (local_ESDID);
  check_eof;
  decrement_check_byte_count (1);
  write_string ('   ESDID = $' || 
		convert_one_byte (local_ESDID) ||
		'  (' || 
		convert_decimal (local_ESDID, 0) || 
		')');
  write_string ('');
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
  write_string ('');
  if byte_count <> 0 then
    error ('Incorrect Text Record byte count.');
end (* process_text_record *);
$PAGE process_end_record
(* PROCESS_END_RECORD - breaks up an end record into it's component fields. *)

procedure process_end_record;

var
  local_one_byte : one_byte;
  local_four_bytes : four_bytes;

begin
  if not id_flag then
    warning ('Record encountered before Identification Record.');
  write_string ('');
  write_string ('End record:');
  read_one_byte (local_one_byte);
  check_eof;
  write_string ('   Starting execution section = $' || 
		convert_one_byte (local_one_byte) ||
		'  (' || 
		convert_decimal (local_one_byte, 0) ||
		')');
  if local_one_byte < 17 then begin
    read_four_bytes (local_four_bytes);
    write_string ('   Starting execution address = $' || 
		  convert_four_bytes (local_four_bytes));
  end;
  end_flag := true;
end (* process_end_record *);
$PAGE process_input
(* PROCESS_INPUT - loops until end-of-file is encountered on the input file, 
                   determining the type of each logical record and then
                   processing it. *)

procedure process_input;

var
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  while not eof (input_file) do begin
    byte_count := local_one_byte;
    if byte_count = 0 then
      skip_count := skip_count + 1
    else begin
      if skip_count > 0 then begin
        write_string ('');
        write_string (convert_decimal (skip_count, 0) || 
		      ' zero-length record(s) skipped');
        skip_count := 0;
      end;
      read_one_byte (local_one_byte);
      check_eof;
      case local_one_byte of
	ord ('1') : process_identification_record;
	ord ('2') : process_ESD_record;
	ord ('3') : process_text_record;
	ord ('4') : process_end_record;
	others    : error ('Illegal record type.');
      end (* case *);
    end (* else *);
    read_one_byte (local_one_byte);
  end (* while *);
  if skip_count > 0 then begin
    write_string ('');
    write_string (convert_decimal (skip_count, 0) || 
		  ' zero-length record(s) skipped');
  end;
  if not id_flag then
    warning ('Missing Identification Record.');
  if not end_flag then
    warning ('Missing End Record.');
end (* process_input *);
$PAGE print_ESDID_statistics
(* PRINT_ESDID_STATISTICS - prints out the statistics collected above
                            that count the number of static calls to
                            each runtime call. *)

$IF STATS
procedure print_ESDID_statistics;

var
  local_index : ESDID_range;

begin
  local_index := minimum (ESDID_range);
  fio_page (output_file);
  write_string ('Runtime Statistics:');
  write_string ('');
  write_string ('Name        Calls');
  write_string ('----------  -----');
  write_string ('');
  while (local_index <= maximum (ESDID_range)) andif
        (ESDID_array[local_index].name <> '') do begin
    with ESDID_array[local_index] do 
      write_string (name || '  ' || convert_decimal (count, 0));
    local_index := local_index + 1;
  end (* while *);
end (* print_ESDID_statistics *);
$ENDIF
$PAGE get_file_names
(* GET_FILE_NAMES - prompts user for input file name, and either output
                    file name or tty.  Writes out a heading and reads in
                    the first input_word. *)

procedure get_file_names;

var
  input_file_name : file_name;
  output_file_name : file_name;

begin
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
    fio_open (output_file, '.LS ' || output_file_name);
    if iostatus <> io_ok then
      error ('Bad output file.');
    with output_file do begin
      page_header := write_header;
      width := 120;
      plength := 42;
    end (* with output_file *);
    fio_page (output_file);
    tty_flag := false;
  end;
  read (input_file, input_word);
end (* get_file_names *);
$PAGE main
begin
  initialize;
  get_file_names;
  motorola_format_dump;
  process_input;
$IF STATS
  print_ESDID_statistics;
$ENDIF
  close;
end.
8@Ð