$TITLE M68REL - Rel file emitter routines
module m68rel options check, special (word);
$PAGE includes
$SYSTEM pascal
$SYSTEM pasfil
$SYSTEM pasist
$SYSTEM ptmcon
$SYSTEM tmpnam
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68emu
$SYSTEM m68cgu
$PAGE types
type
  one_byte    = 0..2 ** 8 - 1;
  two_bytes   = 0..2 ** 16 - 1;
  three_bytes = 0..2 ** 24 - 1;
  four_bytes  = 0..2 ** 32 - 1;
  rel_word    = packed array [1..4] of one_byte;

  XREF_range = 16..255;

  string_2  = string[2];
  string_8  = string[8];

  ESDID_array = array [1..7] of one_byte;

  packed_bytes = packed record
    fill :   0..15;
    byte_1 : one_byte;
    byte_2 : one_byte;
    byte_3 : one_byte;
    byte_4 : one_byte;
  end;

  rel_record = record
    rel_num_ESDID : 0..7;
    rel_long_flag : boolean;
    rel_offset_length : 0..4;
    rel_ESDID_array : ESDID_array;
    rel_offset : four_bytes;
  end;

  obj_record = record
    case obj_relocatable_flag : boolean of
      true  : (obj_relocatable : rel_record);
      false : (obj_absolute : two_bytes);
  end;

  ESDID_record = record
    next : ^ESDID_record;
    name : string_8;
    ESDID : XREF_range;
  end;
$PAGE declarations
var
  obj_byte_count : one_byte;
  obj_curr_entry : 1..33;
  obj_odd_flag : boolean;
  obj_array : array [1..32] of obj_record;

  tempf : file of rel_word;
  relf : file of rel_word;
  rel_var : rel_word;
  rel_byte_ptr : 1..4;
  rel_byte_count : integer;

  current_ESDID : XREF_range;
  ESDID_list_head : ^ESDID_record;
  current_section_number : 0..15;
$PAGE write_one_byte
(* WRITE_ONE_BYTE - puts the given byte into rel_var, maintains the
                    index into rel_var, and writes out rel_var to
                    either relf or tempf when necessary.  This is the
                    main procedure that writes to these files. *)

procedure write_one_byte (var out_file : file of rel_word;
                          input_byte : one_byte);

begin
  rel_var[rel_byte_ptr] := input_byte;
  rel_byte_count := rel_byte_count + 1;
  if rel_byte_ptr = 4 then begin
    write (out_file, rel_var);
    rel_var := (0, 0, 0, 0);
    rel_byte_ptr := 1;
  end
  else
    rel_byte_ptr := rel_byte_ptr + 1;
end (* write_one_byte *);
$PAGE write_two_bytes
(* WRITE_TWO_BYTES - calls procedure write_one_byte twice to write
                     out a two_byte value. *)

procedure write_two_bytes (var out_file : file of rel_word;
                           input_two_bytes : two_bytes);

var
  int_bytes : record
    case boolean of
      true  : (int : four_bytes);
      false : (bytes : packed_bytes);
  end;

begin
  int_bytes.int := input_two_bytes;
  with int_bytes.bytes do begin
    write_one_byte (out_file, byte_3);
    write_one_byte (out_file, byte_4);
  end (* with int_bytes.bytes *);
end (* write_two_bytes *);
$PAGE write_three_bytes
(* WRITE_THREE_BYTES - calls procedure write_one_byte three times to
                       write out a three_byte value. *)

procedure write_three_bytes (var out_file : file of rel_word;
                             input_three_bytes : three_bytes);

var
  int_bytes : record
    case boolean of
      true  : (int : four_bytes);
      false : (bytes : packed_bytes);
  end;

begin
  int_bytes.int := input_three_bytes;
  with int_bytes.bytes do begin
    write_one_byte (out_file, byte_2);
    write_one_byte (out_file, byte_3);
    write_one_byte (out_file, byte_4);
  end (* with int_bytes.bytes *);
end (* write_three_bytes *);
$PAGE write_four_bytes
(* WRITE_FOUR_BYTES - calls procedure write_one_byte four times to 
                      write out a four_byte value. *)

procedure write_four_bytes (var out_file : file of rel_word;
			    input_four_bytes : four_bytes);

var
  int_bytes : record
    case boolean of
      true  : (int : four_bytes);
      false : (bytes : packed_bytes);
  end;

begin
  int_bytes.int := input_four_bytes;
  with int_bytes.bytes do begin
    write_one_byte (out_file, byte_1);
    write_one_byte (out_file, byte_2);
    write_one_byte (out_file, byte_3);
    write_one_byte (out_file, byte_4);
  end (* with int_bytes.bytes *);
end (* write_four_bytes *);
$PAGE write_string
(* WRITE_STRING - converts each character in the input string into
                  it's one_byte hexadecimal value, and then calls
                  procedure write_one_byte to actually write it out. *)

procedure write_string (var out_file : file of rel_word;
			input_string : packed array [1..*] of char);

var
  local_string_ptr : integer;

begin
  for local_string_ptr := 1 to length (input_string) do
    write_one_byte (out_file, ord (input_string[local_string_ptr]) );
end (* write_string *);
$PAGE flush_file_buffer
(* FLUSH_FILE_BUFFER - zero-fills the rest of rel_var, and writes it out. *)

procedure flush_file_buffer (var out_file : file of rel_word);

begin
  while rel_byte_ptr <> 1 do
    write_one_byte (out_file, 0);
end (* flush_file_buffer *);
$PAGE set_bits
(* SET_BITS - sets the specified bits in the given byte to the
              input_contents.  It uses the Motorola bit numbering
              convention of 7, 6, 5, 4, 3, 2, 1, 0. *)

procedure set_bits (var byte : one_byte;
                    input_last_bit : 0..7;
                    input_contents : one_byte);

begin
  byte := byte + (input_contents * (2 ** input_last_bit) );
end;
$PAGE emit_identification_record
(* EMIT_IDENTIFICATION_RECORD - combines the given information, along
                                with the translated date and time, into
                                an object module identification record. *)

procedure emit_identification_record (input_module_name : string_8;
				      input_file_name : string_8;
				      input_extension : string_2);

type
  month_string = string[3];
  date_string = string[9];

const
  local_months : array [1..12] of month_string = 
    ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');

var
  local_time : integer;
  local_seconds : one_byte;
  local_minutes : one_byte;
  local_hours : one_byte;
  local_date : date_string;
  local_day : one_byte;
  local_year : one_byte;
  local_month_index : one_byte;

begin
  write_one_byte (relf, 44);
  write_string (relf, '1');
  write_string (relf, substr (input_module_name || '          ', 1, 10) );
  write_one_byte (relf, 0);
  write_one_byte (relf, 0);
  write_string (relf, 'P');
  write_string (relf, '    ');
  write_two_bytes (relf, 0);
  write_string (relf, '        ');
  write_string (relf, substr (input_file_name || '        ', 1, 8) );
  write_string (relf, substr (input_extension || '  ', 1, 2) );
  local_time := time;
  local_time := local_time div 1000;
  local_seconds := local_time mod 60;
  local_time := local_time div 60;
  local_minutes := local_time mod 60;
  local_time := local_time div 60;
  local_hours := local_time mod 24;
  write_one_byte (relf, ( (local_hours div 10) * 16) + (local_hours mod 10) );
  write_one_byte (relf, ( (local_minutes div 10) * 16) + (local_minutes mod 10) );
  write_one_byte (relf, ( (local_seconds div 10) * 16) + (local_seconds mod 10) );
  local_date := date;
  local_month_index := 1;
  while (local_month_index < 13) andif
	 (local_months[local_month_index] <> substr (local_date, 4, 3) ) do
    local_month_index := local_month_index + 1;
  assert (local_month_index <> 13);
  write_one_byte (relf, ( (local_month_index div 10) * 16) + (local_month_index mod 10) );
  getstring (substr (local_date, 1, 2), local_day);
  write_one_byte (relf, ( (local_day div 10) * 16) + (local_day mod 10) );
  getstring (substr (local_date, 8, 2), local_year);
  write_one_byte (relf, ( (local_year div 10) * 16) + (local_year mod 10) );
end (* emit_identification_record *);
$PAGE emit_section_record
(* EMIT_SECTION_RECORD - emits a section header record for a
			 standard relocatable section. *)

procedure emit_section_record (input_section : 0..15;
			       input_size : four_bytes);

begin
  write_one_byte (relf, 6);
  write_string (relf, '2');
  write_one_byte (relf, #h20 + input_section);
  write_four_bytes (relf, input_size);
end (* emit_section_record *);
$PAGE emit_XDEF_record
(* EMIT_XDEF_RECORD - emits an external symbol definition. *)

procedure emit_XDEF_record (input_section : 0..15;
			    input_XDEF : string_8;
			    input_address : four_bytes);

begin
  write_one_byte (relf, 16);
  write_string (relf, '2');
  write_one_byte (relf, #h40 + input_section);
  write_string (relf, substr (input_XDEF||'          ', 1, 10) );
  write_four_bytes (relf, input_address);
end (* emit_XDEF_record *);
$PAGE emit_XREF_record
(* EMIT_XREF_RECORD - emits an external symbol reference. *)

procedure emit_XREF_record (input_XREF : string_8);

begin
  write_one_byte (relf, 12);
  write_string (relf, '2');
  write_one_byte (relf, #h70);
  write_string (relf, substr (input_XREF||'          ', 1, 10) );
end (* emit_XREF_record *);
$PAGE emit_text_record
(* EMIT_TEXT_RECORD - emits an actual object code text record.  The
                      array obj_array contains up to 32 entries, as
                      indicated by obj_curr_entry, each of which
                      can either be absolute code or relocation 
                      information.  *)

procedure emit_text_record;

var
  local_counter : integer;
  local_inner_counter : integer;
  local_one_byte : one_byte;
  local_map : array [1..4] of one_byte;

begin
  write_one_byte (tempf, obj_byte_count);
  write_string (tempf, '3');
  local_map := (0, 0, 0, 0);
  for local_counter := 0 to obj_curr_entry - 2 do
    set_bits (local_map[ (local_counter div 8) + 1], 
	      7 - (local_counter mod 8), 
	      ord (obj_array[local_counter + 1].obj_relocatable_flag) );
  for local_counter := 1 to 4 do
    write_one_byte (tempf, local_map[local_counter]);
  write_one_byte (tempf, current_section_number + 1);
  for local_counter := 1 to obj_curr_entry - 1 do begin
    with obj_array[local_counter] do begin
      if obj_relocatable_flag then begin
	with obj_relocatable do	begin
	  local_one_byte := 0;
	  set_bits (local_one_byte, 5, rel_num_ESDID);
	  set_bits (local_one_byte, 3, ord (rel_long_flag) );
	  set_bits (local_one_byte, 0, rel_offset_length);
	  write_one_byte (tempf, local_one_byte);
	  for local_inner_counter := 1 to rel_num_ESDID do
	    write_one_byte (tempf, rel_ESDID_array[local_inner_counter]);
	  case rel_offset_length of
	    0 : ;
	    1 : write_one_byte (tempf, rel_offset);
	    2 : write_two_bytes (tempf, rel_offset);
	    3 : write_three_bytes (tempf, rel_offset);
	    4 : write_four_bytes (tempf, rel_offset);
	  end (* case statement *);
	end (* with obj_relocatable *);
      end
      else
	 write_two_bytes (tempf, obj_absolute);
    end (* with obj_array[local_counter] *);
  end (* for loop *);
  obj_byte_count := 6;
  obj_curr_entry := 1;
  obj_odd_flag := false;
end (* emit_text_record *);
$PAGE emit_end_record
(* EMIT_END_RECORD - emits an end record. *)

procedure emit_end_record (input_section : 0..17;
			   input_address : four_bytes);

begin
  if input_section = 17 then
    write_one_byte (relf, 2)
  else
    write_one_byte (relf, 6);
  write_string (relf, '4');
  write_one_byte (relf, input_section);
  if input_section <> 17 then
    write_four_bytes (relf, input_address);
end (* emit_end_record *);
$PAGE absolute_one_byte
(* ABSOLUTE_ONE_BYTE - puts one byte into the absolute data field of
                       an obj_record.  If this fills up the data field
                       of this record, then the record is put into the
                       array to be passed to emit_text_reocrd. *)

procedure absolute_one_byte (input_number : integer);

var
  local_number : integer;

begin
  local_number := input_number;
  if local_number < 0 then
    local_number := local_number + #h100;
  assert ((local_number <= maximum (one_byte)) and (local_number >= 0));
  with obj_array[obj_curr_entry] do begin
    if obj_odd_flag then begin
      obj_absolute := obj_absolute + local_number;
      obj_curr_entry := obj_curr_entry + 1;
      obj_byte_count := obj_byte_count + 2;
      if (obj_curr_entry = 33) or
         (obj_byte_count >= 244) then
        emit_text_record;
      obj_odd_flag := false;
    end
    else begin
      obj_relocatable_flag := false;
      obj_absolute := local_number * 256;
      obj_odd_flag := true;
    end;
  end (* with obj_array[obj_curr_entry] *);
end (* absolute_one_byte *);
$PAGE absolute_two_bytes
(* ABSOLUTE_TWO_BYTES - breaks a two byte field into two one_byte units,
                        then calls absolute_one_byte to insert them into
                        the absolute data field of an obj_record. *)

procedure absolute_two_bytes (input_number : integer);

var
  int_bytes : record
    case boolean of
      true  : (int : integer);
      false : (bytes : packed_bytes);
  end;

begin
  assert ((input_number >= minimum (word)) and
          (input_number <= maximum (uns_word)));
  with int_bytes do begin
    int := input_number;
    with bytes do begin
      absolute_one_byte (byte_3);
      absolute_one_byte (byte_4);
    end (* with bytes *);
  end (* with int_bytes *);
end (* absolute_two_bytes *);
$PAGE absolute_four_bytes
(* ABSOLUTE_FOUR_BYTES - breaks a four byte field into four one_byte units,
			 then calls absolute_one_byte to insert them into
			 the absolute data field of an obj_record. *)

procedure absolute_four_bytes (input_number : integer);

var
  int_bytes : record
    case boolean of
      true  : (int : integer);
      false : (bytes : packed_bytes);
  end;

begin
  int_bytes.int := input_number;
  with int_bytes.bytes do begin
    absolute_one_byte (byte_1);
    absolute_one_byte (byte_2);
    absolute_one_byte (byte_3);
    absolute_one_byte (byte_4);
  end (* with int_bytes.bytes *);
end (* absolute_four_bytes *);
$PAGE get_ESDID
(* GET_ESDID - returns the value of the ESDID of the given external
               symbol.  If the symbol is already on the external list,
               then it's ESDID is taken from the existing entry,
               otherwise a new entry is created with the next available
               ESDID used. *)

function get_ESDID (input_name : string_8) : XREF_range;

var
  prev_ESDID_ptr : ^ESDID_record;
  curr_ESDID_ptr : ^ESDID_record;
  local_new_ESDID : ^ESDID_record;

begin
  prev_ESDID_ptr := ESDID_list_head;
  curr_ESDID_ptr := prev_ESDID_ptr^.next;
  while (curr_ESDID_ptr <> nil) andif
        (curr_ESDID_ptr^.name <> input_name) do begin
    prev_ESDID_ptr := curr_ESDID_ptr;
    curr_ESDID_ptr := prev_ESDID_ptr^.next;
  end (* while *);
  if curr_ESDID_ptr = nil then begin
    assert (current_ESDID < maximum (XREF_range));
    current_ESDID := current_ESDID + 1;
    new (local_new_ESDID);
    with local_new_ESDID^ do begin
      next := nil;
      name := input_name;
      ESDID := current_ESDID;
    end (* with local_new_ESDID^ *);
    prev_ESDID_ptr^.next := local_new_ESDID;
    get_ESDID := current_ESDID;
  end
  else
    get_ESDID := curr_ESDID_ptr^.ESDID;
end (* get_ESDID *);
$PAGE relocatable_entry
(* RELOCATABLE_ENTRY - actually fills up a relocatable entry 
		       for a text record. *)

procedure relocatable_entry (input_num_ESDID : 0..7;
			     input_long_flag : boolean;
			     input_array : ESDID_array;
			     input_offset : integer);

var
  local_offset : integer;

begin
  assert (not obj_odd_flag);
  with obj_array[obj_curr_entry] do begin
    obj_relocatable_flag := true;
    with obj_relocatable do begin
      rel_num_ESDID := input_num_ESDID;
      rel_long_flag := input_long_flag;
      rel_ESDID_array := input_array;
      local_offset := input_offset;
      if local_offset > 0 then begin
        assert (local_offset < #h80000000);
        rel_offset := local_offset;
        rel_offset_length := 1;
        if rel_offset >= #h80 then
          rel_offset_length := 2;
        if rel_offset >= #h8000 then
          rel_offset_length := 3;
        if rel_offset >= #h800000 then
          rel_offset_length := 4;
      end
      else begin
	if local_offset < -#h800000 then
	  local_offset := local_offset + #h100000000;
	if local_offset < -#h8000 then
	  local_offset := local_offset + #h1000000;
	if local_offset < -#h80 then
	  local_offset := local_offset + #h10000;
	if local_offset < 0 then
	  local_offset := local_offset + #h100;
	rel_offset := local_offset;
	rel_offset_length := 0;
	if rel_offset > 0 then
	  rel_offset_length := 1;
	if rel_offset > maximum (one_byte) then
	  rel_offset_length := 2;
	if rel_offset > maximum (two_bytes) then
	  rel_offset_length := 3;
	if rel_offset > maximum (three_bytes) then
	  rel_offset_length := 4;
      end;
      obj_curr_entry := obj_curr_entry + 1;
      obj_byte_count := obj_byte_count + 1 + rel_num_ESDID +
			rel_offset_length;
      if (obj_curr_entry = 33) or
	 (obj_byte_count >= 244) then
	emit_text_record;
    end (* with obj_relocatable *);
  end (* with obj_array[obj_curr_entry] *);
end (* relocatable_entry *);
$PAGE obj_set_up
(* OBJ_SET_UP - fills up the relocatable fields of an obj_record. *)

procedure obj_set_up (input_reloc : reloc_value;
		      input_long_flag : boolean;
		      input_pc_relative_flag : boolean;
		      input_pc : code_address);

var
  local_offset : integer;
  local_def : def;
  local_reloc : reloc_value;
  local_relocatable_flag : boolean;
  local_num_ESDID : 0..7;
  local_ESDID_array : ESDID_array;

begin
  local_offset := 0;
  local_reloc := input_reloc;
  while local_reloc.kind = def_sc do begin
    local_offset := local_offset + local_reloc.offset;
    local_def := local_reloc.reldef;
    if local_def^.defined then
      local_reloc := local_def^.defval
    else begin
      def_forward (local_def, input_pc,
		   input_long_flag, local_offset);
      local_reloc := abs_zero;
      local_offset := 0;
    end (* else *);
  e while *);
  local_reloc.offset := local_reloc.offset + local_offset;
  local_relocatable_flag := true;
  with local_reloc do begin
    case kind of

      absolute_sc :
	begin
	  local_offset := offset;
	  local_relocatable_flag := false;
	end;

      code_sc :
	begin
	  local_offset := offset;
	  local_num_ESDID := 1;
	  local_ESDID_array[1] := section_numbers[relsect] + 1;
	end;

      external_sc :
	begin
	  local_offset := offset;
	  local_num_ESDID := 1;
          if relsym^.init_value.kind = no_value then
	    local_ESDID_array[1] := get_ESDID (relsym^.name^.text)
          else
            local_ESDID_array[1] := get_ESDID (relsym^.init_value.valp^.str_val);
          if relsym^.kind in [vars, conditions] then begin
	    local_num_ESDID := 2;
	    local_ESDID_array[2] := get_ESDID ('M.ST_OFF');
          end;
	end;

      static_sc :
	begin
	  local_offset := offset + stc_offset (relsym);
	  local_num_ESDID := 2;
	  local_ESDID_array[1] := section_numbers[static_section] + 1;
	  local_ESDID_array[2] := get_ESDID ('M.ST_OFF');
	end;

      local_sc :
	begin
	  local_offset := offset + relsym^.item_addr;
	  local_relocatable_flag := false;
	end;

      parameter_sc :
	begin
	  local_offset := offset + relsym^.item_addr;
	  local_relocatable_flag := false;
	end;

      runtime_sc :
	begin
	  local_offset := offset;
	  local_num_ESDID := 1;
	  local_ESDID_array[1] := get_ESDID (rts_name[relrt]);
	end;

    end (* case *);
    if local_relocatable_flag and
       input_pc_relative_flag then begin
      assert (kind = code_sc);
      local_offset := local_offset - input_pc;
      local_relocatable_flag := false;
    end;
  end (* with local_reloc *);
  if local_relocatable_flag then
    relocatable_entry (local_num_ESDID, input_long_flag, 
		       local_ESDID_array, local_offset)
  else begin
    if input_long_flag then
      absolute_four_bytes (local_offset)
    else
      absolute_two_bytes (local_offset);
  end;
end (* obj_set_up *);
$PAGE extension_word
(* EXTENSION_WORD - determines what type of extension word or words
                    is needed, based on the addressing mode, and then
                    generates it. *)

procedure extension_word (input_array : array [1..5] of uns_word;
			  var var_length : 1..5;
			  input_operand : op_desc;
                          var var_pc : code_address);

const
  local_indexed_modes : addr_mode_set =
    [index_w_mode, index_l_mode, pc_index_w_mode, pc_index_l_mode];
  local_word_modes : addr_mode_set =
    [displacement_mode, abs_w_mode];

begin
  with input_operand do begin
    if mode in local_indexed_modes then begin
      var_length := var_length + 1;
      absolute_two_bytes (input_array[var_length]);
      var_pc := var_pc + 2;
    end;
    if mode in local_word_modes then begin
      var_length := var_length + 1;
      obj_set_up (cst_part, false, false, var_pc);
      var_pc := var_pc + 2;
    end;
    if mode = abs_l_mode then begin
      var_length := var_length + 2;
      obj_set_up (cst_part, true, false, var_pc);
      var_pc := var_pc + 4;
    end;
    if mode = pc_displacement_mode then begin
      var_length := var_length + 1;
      obj_set_up (cst_part, false, true, var_pc);
      var_pc := var_pc + 2;
    end;
    if mode = immediate_mode then begin
      obj_set_up (cst_part, value_size = size_long, false, var_pc);
      if value_size = size_long then begin
	var_length := var_length + 2;
        var_pc := var_pc + 4;
      end
      else begin
	var_length := var_length + 1;
        var_pc := var_pc + 2;
      end;
    end;
  end (* with input_operand *);
end (* extension_word *);
$PAGE process_inst_code
(* PROCESS_INST_CODE - emits a code_record of kind inst_code. *)

procedure process_inst_code (input_code_ptr : code);

var
  local_array : array [1..5] of uns_word;
  local_counter : 1..5;
  local_print_length : 1..5;
  local_absolute_length : 1..5;
  local_op_1_flag : boolean;
  local_op_2_flag : boolean;
  local_pc : code_address;

begin
  local_pc := input_code_ptr^.addr;
  with input_code_ptr^.inst do begin
    get_code (input_code_ptr, local_array,
              local_print_length, local_absolute_length,
              local_op_1_flag, local_op_2_flag);
    for local_counter := 1 to local_absolute_length do begin
      absolute_two_bytes (local_array[local_counter]);
      local_pc := local_pc + 2;
    end;
    local_counter := local_absolute_length;
    if local_op_1_flag then
      extension_word (local_array, local_counter,
		      operands[1], local_pc);
    if local_op_2_flag and
       not ((opcode in [bcc_p_opc, bsr_p_opc]) and
            (operands[2].value_size = size_byte)) then
      extension_word (local_array, local_counter,
		      operands[2], local_pc);
  end (* with input_code_ptr^.inst *);
end (* process_inst_code *);
$PAGE process_byte_code
(* PROCESS_BYTE_CODE - emits a code_record of kind byte_code. *)

procedure process_byte_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    assert (not pc_relative);
    absolute_one_byte (relval.offset);
  end (* with input_code_ptr^ *);
end (* process_byte_code *);
$PAGE process_word_code
(* PROCESS_WORD_CODE - emits a code_record of kind word_code. *)

procedure process_word_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    obj_set_up (relval, false, pc_relative, addr);
  end (* with input_code_ptr^ *);
end (* process_word_code *);
$PAGE process_long_code
(* PROCESS_LONG_CODE - emits a code_record of kind long_code. *)

procedure process_long_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    obj_set_up (relval, true, pc_relative, addr);
  end (* with input_code_ptr^ *);
end (* process_long_code *);
$PAGE process_string_code
(* PROCESS_STRING_CODE - emits a code_record of kind string_code. *)

procedure process_string_code (input_code_ptr : code);

var
  local_counter : integer;

begin
  with input_code_ptr^ do begin
    for local_counter := 1 to length (strval) do
      absolute_one_byte (ord (strval[local_counter]));
  end (* with input_code_ptr^ *);
end (* process_string_code *);
$PAGE process_len_code
(* PROCESS_LEN_CODE - emits a code_record of kind len_code. *)

procedure process_len_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    absolute_two_bytes (lenval);
  end (* with input_code_ptr^ *);
end (* process_len_code *);
$PAGE process_set_code
(* PROCESS_SET_CODE - emits a code_record of kind set_code. *)

procedure process_set_code (input_code_ptr : code);

var
  local_num_words : integer;
  local_bit_number : integer;
  local_inner_counter : 0..15;
  local_outer_counter : integer;
  local_set_value : two_bytes;

begin
  with input_code_ptr^ do begin
    local_num_words := dimension (setval) div 16;
    for local_outer_counter := 1 to local_num_words do begin
      local_set_value := 0;
      for local_inner_counter := 0 to 15 do begin
        local_bit_number := (local_outer_counter - 1) * 16 +
                            local_inner_counter;
        local_set_value := (local_set_value * 2) +
			   ord (setval[local_bit_number]);
      end (* inner for loop *);
      absolute_two_bytes (local_set_value);
    end (* outer for loop *);
  end (* with input_code_ptr^ *);
end (* process_set_code *);
$PAGE process_sreal_code
(* PROCESS_SREAL_CODE - emits a code_record of kind sreal_code. *)

procedure process_sreal_code (input_code_ptr : code);

var
  local_int : integer;

begin
  with input_code_ptr^ do begin
    cvt_sreal (realval, local_int, true);
    absolute_four_bytes (local_int);
  end (* with input_code_ptr^ *);
end (* process_sreal_code *);
$PAGE process_dreal_code
(* PROCESS_DREAL_CODE - emits a code_record of kind dreal_code. *)

procedure process_dreal_code (input_code_ptr : code);

var
  local_int_1 : integer;
  local_int_2 : integer;

begin
  with input_code_ptr^ do begin
    cvt_dreal (realval, local_int_1, local_int_2);
    absolute_four_bytes (local_int_1);
    absolute_four_bytes (local_int_2);
  end (* with input_code_ptr^ *);
end (* process_dreal_code *);
$PAGE process_dtime_code
(* PROCESS_DTIME_CODE - emits a code_record of kind dtime_code. *)

procedure process_dtime_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    absolute_four_bytes (dtimeval.d);
  end (* with input_code_ptr^ *);
end (* process_dtime_code *);
$PAGE process_block_code
(* PROCESS_BLOCK_CODE - emits a code_record of kind block_code. *)

procedure process_block_code (input_code_ptr : code);

var
  local_size : integer;
  local_array : ESDID_array;

begin
  with input_code_ptr^ do begin
    if blocksize < 1 then 
      return; (* <--- No bytes to emit *)
    local_size := blocksize;
    if odd (addr) then begin
      absolute_one_byte (0);
      local_size := local_size - 1;
    end;
    relocatable_entry (0, false, local_array, 
		       (local_size div 2) * 2);
    if odd (local_size) then
      absolute_one_byte (0);
  end (* with input_code_ptr^ *);
end (* process_block_code *);
$PAGE process_fill_code
(* PROCESS_FILL_CODE - emits a code_record of kind fill_code. *)

procedure process_fill_code;

begin
  absolute_one_byte (0);
end (* process_fill_code *);
$PAGE emit_code_record
(* EMIT_CODE_RECORD - determines the kind of the given code_record,
                      and then calls the appropriate routine to emit it. *)

procedure emit_code_record (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    case kind of
      inst_code      : process_inst_code (input_code_ptr);
      byte_code      : process_byte_code (input_code_ptr);
      word_code      : process_word_code (input_code_ptr);
      long_code      : process_long_code (input_code_ptr);
      string_code    : process_string_code (input_code_ptr);
      len_code       : process_len_code (input_code_ptr);
      set_code       : process_set_code (input_code_ptr);
      sreal_code     : process_sreal_code (input_code_ptr);
      dreal_code     : process_dreal_code (input_code_ptr);
      dtime_code     : process_dtime_code (input_code_ptr);
      block_code     : process_block_code (input_code_ptr);
      def_code       : ;
      pc_assert_code : ;
      source_code    : ;
      comment_code   : ;
      asm_label_code : ;
      fill_code      : process_fill_code;
      align_code     : ;
    end (* case *);
  end (* with input_code_ptr^ *);
end (* emit_code_record *);
$PAGE rel_init
(* REL_INIT - sets up the rel file (relf), sets up the temporary
              rel file (tempf), and initializes the ESDID linked list. *)

public procedure rel_init;

begin
  rel_byte_ptr := 1;
  rel_var := (0, 0, 0, 0);
  if rel_file = '' then
    return; (* <--- No rel-file *)
  rewrite (relf, '.' || rel_extension || ' ' || rel_file);
  if iostatus (relf) <> io_ok then begin
    writeln (tty, '?Rel file open error');
    rel_file := '';
    return;
  end;
  rel_file := substr (main_file, search (main_file, [':', ')']) + 1);
  rel_file := substr (rel_file, 1, search (rel_file, ['[', '.'], length (rel_file) + 1) - 1);
  rel_file := filename (relf);

  (* open the temporary file *)

  rewrite (tempf, tempname ('TRO'));
  if iostatus (tempf) <> io_ok then begin
    writeln (tty, '?Temp .RO file open error');
    rel_file := '';
    return;
  end;

  (* initialize the ESDID linked list head *)

  current_ESDID := minimum (XREF_range);
  new (ESDID_list_head);
  with ESDID_list_head^ do begin
    next := nil;
    name := '';
    ESDID := minimum (XREF_range);
  end (* with ESDID_list_head^ *);
end (* rel_init *);
$PAGE rel_emit
(* REL_EMIT - emits the rel file records for the given code_list. *)

public procedure rel_emit (input_list : code_list;
                           input_section : rel_sections);

var
  local_code_ptr : code;

begin
  current_section_number := section_numbers[input_section];
  obj_byte_count := 6;
  obj_curr_entry := 1;
  obj_odd_flag := false;
  local_code_ptr := input_list.first;
  while local_code_ptr <> nil do begin
    emit_code_record (local_code_ptr);
    local_code_ptr := local_code_ptr^.next;
  end (* while *);
  if obj_odd_flag then
    absolute_one_byte (0);
  if obj_curr_entry > 1 then
    emit_text_record;
end (* rel_emit *);
$PAGE rel_patch
public procedure rel_patch (input_patch_pc : code_address;
                            input_curr_pc : code_address;
                            input_long_flag : boolean;
                            input_reloc : reloc_value;
                            input_patch : integer);

var
  local_array : ESDID_array;
  local_reloc : reloc_value;

begin
  if rel_file = '' then
    return; (* <--- No rel file *)
  obj_byte_count := 6;
  obj_curr_entry := 1;
  obj_odd_flag := false;
  relocatable_entry (0, input_long_flag, local_array, 
		     input_patch_pc - input_curr_pc);
  local_reloc := input_reloc;
  local_reloc.offset := local_reloc.offset + input_patch;
  obj_set_up (local_reloc, input_long_flag, false, input_patch_pc);
  if input_long_flag then
    relocatable_entry (0, input_long_flag, local_array, 
		       input_curr_pc - input_patch_pc - 4)
  else
    relocatable_entry (0, input_long_flag, local_array, 
		       input_curr_pc - input_patch_pc - 2);
  if obj_odd_flag then
    absolute_one_byte (0);
  if obj_curr_entry > 1 then
    emit_text_record;
end (* rel_patch *);
$PAGE rel_fatal
(* REL_FATAL - scratches the rel file. *)

public procedure rel_fatal;

begin
  if rel_file <> '' then
    scratch (relf);
end (* rel_fatal *);
$PAGE rel_term
(* REL_TERM - emits an end record, and, if necessary, zero-fills the
              end of the last object record. *)

public procedure rel_term (input_start_addr : def;
                           input_size : array [rel_sections] of code_address)
options special (coercions);

var
  local_sym : sym;
  local_ESDID_ptr : ^ESDID_record;
  local_rel_word : rel_word;
  local_address : code_address;
  local_def : def;
  local_section_number : 0..15;
  local_file_name : file_name;

begin
  flush_file_buffer (tempf);

  (* emit identification record *)

  emit_identification_record (root_block^.children^.id^.text,
                              rel_file, rel_extension);

  (* emit XDEF records *)

  if input_start_addr <> nil then begin
    with input_start_addr^ do begin
      assert (defined);
      assert (defval.kind = code_sc);
      emit_XDEF_record (section_numbers[defval.relsect], 
			'M.START', defval.offset);
    end (* with input_start_addr^ *);
  end;
  local_sym := root_block^.children^.id_list.first;
  while local_sym <> nil do begin
    with local_sym^ do begin
      if (kind in [vars, consts, conditions]) andif public_dcl then begin 
        if kind = consts then begin
          if init_value.kind = subr_cst then
            local_def := def_lookup (subr_def, init_value.blkp^.number)
          else
            local_def := def (init_value.defp);
          with local_def^ do begin
            assert (defined);
            assert (defval.kind = code_sc);
	    local_section_number := section_numbers[defval.relsect];
	    local_address := defval.offset;
          end (* with local_def^ *);
	end
	else (* kind in [vars, conditions ] *) begin
	  local_section_number := section_numbers[static_section];
	  local_address := stc_offset (local_sym);
	end;
        emit_XDEF_record (local_section_number, name^.text, local_address);
      end (* if public_dcl *);
      local_sym := next;
    end (* with local_sym^ *);
  end (* while *);

  (* emit XREF records *)

  local_ESDID_ptr := ESDID_list_head^.next;
  while local_ESDID_ptr <> nil do begin
    emit_XREF_record (local_ESDID_ptr^.name);
    local_ESDID_ptr := local_ESDID_ptr^.next;
  end (* while *);

  (* emit section headers *)

  if input_size[static_section] > 0 then
    emit_section_record (section_numbers[static_section], 
			 input_size[static_section]);
  if input_size[code_section] > 0 then
    emit_section_record (section_numbers[code_section],
			 input_size[code_section]);

  (* emit text records *)

  local_file_name := filename (tempf);
  close (tempf);
  reset (tempf, local_file_name);
  flush_file_buffer (relf);
  while not eof (tempf) do begin
    read (tempf, local_rel_word);
    write (relf, local_rel_word);
  end (* while *);
  flush_file_buffer (relf);

  (* emit end record *)

  emit_end_record (17,0);

  (* zero-fill the last record until it's a multiple of 256 *)

  while rel_byte_count mod 256 <> 0 do
    write_one_byte (relf, 0);

  (* close the files *)

  close (relf);
  scratch (tempf);
end (* rel_term *).
w <Ž