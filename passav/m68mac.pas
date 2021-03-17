$TITLE M68MAC - Routines for Assembly Listing
module m68mac options check, special (word);
$PAGE includes
$SYSTEM pascal
$SYSTEM pasfil
$SYSTEM pasist
$SYSTEM ptmcon
$SYSTEM dtime
$SYSTEM pascv
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68emu
$PAGE locals
type
  string_2  = string[2];
  string_3  = string[3];
  string_5  = string[5];
  string_8  = string[8];
  string_10 = string[10];
  string_12 = string[12];
  string_20 = string[20];
  string_40 = string[40];

  ESDID_record = record
    next : ^ESDID_record;
    name : string_8;
  end;

const
  pc_column      = 1;
  code_column    = 10;
  label_column   = 23;
  opcode_column  = 34;
  operand_column = 44;
  comment_column = 80;

var
  curr_section : rel_sections;
  ESDID_list_head : ^ESDID_record;
  label_string : string_8;
  label_addr : code_address;
$PAGE init_output
(* INIT_OUTPUT - sets up the listing file for I/O *)

public procedure init_output;

begin
  case lf_status of
    now_open    : ;
    unopened    : fio_open (listfb, '.LST ' || list_file);
    prev_opened : fio_reopen (listfb);
  end (* case *);
  with listfb do begin
    if not eof (file_var) then begin
      writeln (tty, '?Cannot open list file: ' || list_file);
      list_file := '';
      return;
    end;
    new_page := fio_eject;
    page_header := fio_nop;
    width := prog_options.page_width;
    plength := prog_options.page_length;
  end (* with listfb *);
  lf_status := now_open;
end (* init_output *);
$PAGE cv_hex
(* CV_HEX - converts an integer value to a string giving its hexadecimal
            representation. *)

function cv_hex (input_number : integer;
                 input_length : 0..8) : string_8;

begin
  if input_length > 0 then
    putstring (cv_hex, input_number:input_length:h)
  else begin
    putstring (cv_hex, input_number:8:h);
    cv_hex := substr (cv_hex, min (9-input_length, verify (cv_hex, ['0'], 8)))
  end
end (* cv_hex *);
$PAGE force_old_label
(* FORCE_OLD_LABEL - prints an empty line with the current 
		     LABEL_STRING value, if it is not blank, 
		     and clears it.  *)

procedure force_old_label;

begin
  if label_string <> '' then begin
    fio_write (listfb, cv_hex (label_addr, 8));
    fio_tab (listfb, label_column);
    fio_line (listfb, label_string);
    label_string := '';
  end;
end (* force_old_label *);
$PAGE init_line
(* INIT_LINE - prints the address of the current instruction, 
	       the outstanding label (if there is one), 
	       and the instruction opcode. *)

procedure init_line (input_addr : code_address; 
                     input_code : string_12;
		     input_opcode : string_8);

begin
  fio_write (listfb, cv_hex (input_addr, 8));
  fio_tab (listfb, code_column);
  fio_write (listfb, input_code);
  if label_string <> '' then begin
    assert (label_addr = input_addr);
    fio_tab (listfb, label_column);
    fio_write (listfb, label_string);
    label_string := '';
  end;
  fio_tab (listfb, opcode_column);
  fio_write (listfb, input_opcode);
  fio_tab (listfb, operand_column);
end (* init_line *);
$PAGE write_opcode
(* WRITE_OPCODE - converts a given instruction's specific_opcode
                  into printable characters and writes it out on
                  the listing, taking into account the CC portion
                  of bcc, dbcc, and scc instructions, and the
                  .W and .L extensions. *)

procedure write_opcode (input_code_ptr : code);

const
  opcodes : packed array [specific_opcodes] of string_5 =
    ('ADD',   'ADDI',  'ADDA',  'ADD',
     'ADDQ',  'AND',   'ANDI',  'AND',
     'ASL',   'ASL',   'ASL',   'ASR',
     'ASR',   'ASR',   'B',     'BCHG',
     'BCHG',  'BCHG',  'BCHG',  'BCLR',
     'BCLR',  'BCLR',  'BCLR',  'BSET',
     'BSET',  'BSET',  'BSET',  'BSR',
     'BTST',  'BTST',  'BTST',  'BTST',
     'CLR',   'CMPI',  'CMPA',  'CMP',
     'CMPM',  'DB',    'DIVS',  'DIVU',
     'EOR',   'EORI',  'EXG',   'EXG',
     'EXG',   'EXT',   'JMP',   'JSR',
     'LEA',   'LINK',  'LSL',   'LSL',
     'LSL',   'LSR',   'LSR',   'LSR',
     'MOVEQ', 'MOVEA', 'MOVE',  'MOVEM',
     'MOVEM', 'MULS',  'MULU',  'NEG',
     'NOP',   'NOT',   'OR',    'ORI',
     'OR',    'PEA',   'ROL',   'ROL',
     'ROL',   'ROR',   'ROR',   'ROR',
     'RTS',   'S',     'SUB',   'SUBI',
     'SUBA',  'SUB',   'SUBQ',  'SWAP',
     'TST',   'UNLK');

  cond_codes : packed array [condition_codes] of string_2 =
    ('T', 'F', 'HI','LS','CC','CS','NE','EQ',
     'VC','VS','PL','MI','GE','LT','GT','LE');

  extent_array : packed array [op_sizes] of string_2 =
    ('.S', '.L', '.L', '.L', '');

  size_array : packed array [op_sizes] of string_2 =
    ('.B', '.W', '.L', '.L', '');

  cc_opcodes : set of specific_opcodes =
    [bcc_p_opc, dbcc_dw_opc, scc_m_opc];

  extent_opcodes : set of specific_opcodes =
    [bcc_p_opc, bsr_p_opc];

  no_extension_opcodes : set of specific_opcodes =
    [dbcc_dw_opc, link_aw_opc, move_bd_opc, nop_x_opc, rts_x_opc, unlk_a_opc,
     muls_md_opc, mulu_md_opc, divs_md_opc, divu_md_opc, swap_d_opc, lea_ma_opc,
     pea_m_opc, scc_m_opc, jmp_m_opc, jsr_m_opc];

  size_1_opcodes : set of specific_opcodes =
    [add_ma_opc, cmp_ma_opc, move_ma_opc, movem_mw_opc, sub_ma_opc];

  size_2_opcodes : set of specific_opcodes =
    [minimum (specific_opcodes)..maximum (specific_opcodes)] -
    (no_extension_opcodes + extent_opcodes + size_1_opcodes);

begin
  with input_code_ptr^.inst do begin
    if (opcode = bcc_p_opc) andif (ccode = t_cc) then
      fio_write (listfb, 'BRA')
    else begin
      fio_write (listfb, opcodes[opcode]);
      if opcode in cc_opcodes then
	fio_write (listfb, cond_codes[ccode]);
    end;
    if opcode in extent_opcodes then
      fio_write (listfb, extent_array[operands[2].value_size]);
    if opcode in size_1_opcodes then
      fio_write (listfb, size_array[operands[1].value_size]);
    if opcode in size_2_opcodes then
      fio_write (listfb, size_array[operands[2].value_size]);
  end (* with input_code_ptr^.inst *);
end (* write_opcode *);
$PAGE sym_name_text
(* SYM_NAME_TEXT - returns the name of a symbol. *)

function sym_name_text (input_sym : sym) : string_20;

begin
  with input_sym^ do begin
    if name <> nil then
      sym_name_text := name^.text
    else begin
      if kind in [vars, labels] then
        sym_name_text := 'V.' || cv_int (id_number)
      else
        sym_name_text := 'C.' || cv_int (id_number);
    end (* else *);
  end (* with input_sym^ *);
end (* sym_name_text *);
$PAGE def_name_text
(* DEF_NAME_TEXT - returns the name of a symbolic definition. *)

function def_name_text (input_def_ptr : def) : string_20;

const
  local_codes : array [def_class] of string[2] =
    ('S', 'B', 'T', 'ST', 'W', 'C', 'X', 'L', 'K', 'H');

begin
  with input_def_ptr^ do begin
    def_name_text := local_codes[kind] || '.';
    if kind in local_def_classes then
      def_name_text := def_name_text || cv_int (cur_block^.number) || '.';
    def_name_text := def_name_text || cv_int (index);
  end (* with input_def_ptr^ *);
end (* def_name_text *);
$PAGE add_ESDID
(* ADD_ESDID - checks the list of external symbols to see if the
               given symbol is on the list.  If it is, nothing else
               happens, but if it is not on the list, a new entry
               is created with the new symbol. *)

procedure add_ESDID (input_name : string_8);

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
    new (local_new_ESDID);
    with local_new_ESDID^ do begin
      next := nil;
      name := input_name;
    end (* with local_new_ESDID^ *);
    prev_ESDID_ptr^.next := local_new_ESDID;
  end;
end (* add_ESDID *);
$PAGE reloc_text
(* RELOC_TEXT - returns an operand field string representation for a
		relocatable value.  Also, if ANNOTATE is true, returns
		an informative comment string in ANNOTATION. *)

function reloc_text (input_reloc : reloc_value;
		     annotate : boolean;
		     var annotation : string_40) : string_20;

var
  b : blk;

begin
  with input_reloc do begin
    case kind of

      absolute_sc :
        begin
	  reloc_text := cv_int (offset);
	  if annotate then begin
	    annotation := annotation || '  $' || cv_hex (offset, 0);
	  end;
        end;

      code_sc :
        begin
          case relsect of
            static_section : reloc_text := 'STATIC.';
            code_section   : reloc_text := 'CODE.';
          end (* case *);
        end;

      external_sc :
        begin
          if relsym^.init_value.kind = no_value then
	    reloc_text := relsym^.name^.text
          else
            reloc_text := relsym^.init_value.valp^.str_val;
          add_ESDID (reloc_text);
	  if annotate then
	    annotation := annotation || '  ' || sym_name_text (relsym);
        end;

      static_sc :
        begin
	  reloc_text := 'STATIC.+' || cv_int (stc_offset (relsym));
	  if annotate then
	    annotation := annotation || '  ' || sym_name_text (relsym);
        end;

      local_sc,
      parameter_sc :
        begin
	  reloc_text := cv_int (relsym^.item_addr);
	  if annotate then
	    annotation := annotation || '  ' || sym_name_text (relsym);
        end;

      def_sc :
        begin
	  reloc_text := def_name_text (reldef);
	  if annotate then begin
	    if reldef^.kind = subr_def then begin
	      b := root_block;
	      while b^.number <> reldef^.index do
		b := b^.downward_call_thread;
	      annotation := annotation || '  ' || sym_name_text (b^.subr_sym);
	      if reldef^.defined then
		annotation := annotation || ' = ' ||
			      reloc_text (reldef^.defval, false, annotation);
	    end
	    else if reldef^.defined and (reldef^.kind in comment_def_classes) then
	      annotation := annotation || '  ' || reloc_text (reldef^.defval, false, annotation);
	  end;
        end;

      runtime_sc :
        begin
	  reloc_text := rts_name[relrt];
          add_ESDID (reloc_text);
        end;

    end (* case *);
    if (kind <> absolute_sc) and (offset <> 0) then begin
      if offset > 0
	then reloc_text := reloc_text || '+'
	else reloc_text := reloc_text || '-';
      if kind = code_sc
	then reloc_text := reloc_text || '$' || cv_hex (abs (offset), 0)
	else reloc_text := reloc_text || cv_int (abs (offset));
    end;
  end (* with input_reloc *);
end (* reloc_text *);
$PAGE process_inst_code
(* PROCESS_INST_CODE - writes out a code_record of kind inst_code,
                       which involves writing out the opcode, and
                       0, 1, or 2 operand fields. *)

procedure process_inst_code (input_code_ptr : code);

const
  reg_array : array [registers] of string_2 =
    ('D0','D1','D2','D3','D4','D5','D6','D7',
     'A0','A1','A2','A3','A4','A5','A6','A7');

  prefix : array [addr_mode] of string_2 =
    ('', '', '(', '-(', '(', '(', '(', '(', '', '', '(', '(', '(', '', '');

  suffix : array [addr_mode] of string_3 =
    ('', '', ')', ')', ')+', ')', '.W)', '.L)', '', '', ')', '.W)', '.L)', '', '');

var
  local_operand : 1..2;
  local_comma_flag : boolean;
  local_comment_string : string_40;
  local_array : array [1..5] of uns_word;
  local_print_length : 1..5;
  local_absolute_length : 1..5;
  local_op_1_flag : boolean;
  local_op_2_flag : boolean;
  local_counter : 1..5;

begin
  with input_code_ptr^, inst do begin
    fio_write (listfb, cv_hex (input_code_ptr^.addr, 8));
    get_code (input_code_ptr, local_array,
              local_print_length, local_absolute_length,
              local_op_1_flag, local_op_2_flag);
    fio_tab (listfb, code_column);
    for local_counter := 1 to min (local_print_length, 3) do
      fio_write (listfb, cv_hex (local_array[local_counter], 4));
    if label_string <> '' then begin
      assert (label_addr = addr);
      fio_tab (listfb, label_column);
      fio_write (listfb, label_string);
      label_string := '';
    end;
    fio_tab (listfb, opcode_column);
    write_opcode (input_code_ptr);
    fio_tab (listfb, operand_column);
    local_comma_flag := false;
    local_comment_string := '';
    for local_operand := 1 to 2 do begin
      with operands[local_operand] do begin
        if (local_comma_flag) and
           (mode <> null_mode) then
          fio_write (listfb, ',');
        if mode <> null_mode then
	  local_comma_flag := true;
        if mode = immediate_mode then
          fio_write (listfb, '#');
        if mode in cst_use_modes then
	  fio_write (listfb, reloc_text (cst_part, true, local_comment_string));
        fio_write (listfb, prefix[mode]);
        if mode in reg_use_modes then
          fio_write (listfb, reg_array[reg]);
        if mode in pc_relative_modes then
          fio_write (listfb, 'PC');
        if mode in indexed_modes then
          fio_write (listfb, ',' || reg_array[index_reg]);
        fio_write (listfb, suffix[mode]);
      end (* with *);
    end (* for loop *);
    if local_comment_string <> '' then begin
      fio_tab (listfb, comment_column);
      fio_write (listfb, local_comment_string);
    end;
    fio_skip (listfb);
    if local_print_length > 3 then begin
      fio_tab (listfb, code_column);
      for local_counter := 4 to local_print_length do
	fio_write (listfb, cv_hex (local_array[local_counter], 4));
      fio_skip (listfb);
    end;
  end (* with input_code_ptr^.inst *);
end (* process_inst_code *);
$PAGE process_byte_code
(* PROCESS_BYTE_CODE - writes out a code_record of kind byte_code,
                       which involves writing out a DC.B and a
                       relocatable operand field. *)

procedure process_byte_code (input_code_ptr : code);

var
  local_comment_string : string_40;

begin
  with input_code_ptr^ do begin
    if relval.kind = absolute_sc then
      init_line (addr, cv_hex (relval.offset, 2), 'DC.B')
    else
      init_line (addr, '00', 'DC.B');
    local_comment_string := '';
    fio_write (listfb, reloc_text (relval, true, local_comment_string));
    if local_comment_string <> '' then begin
      fio_tab (listfb, comment_column);
      fio_write (listfb, local_comment_string);
    end;
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_byte_code *);
$PAGE process_word_code
(* PROCESS_WORD_CODE - writes out a code_record of kind word_code,
                       which involves writing out a DC.W and a
                       relocatable operand field. *)

procedure process_word_code (input_code_ptr : code);

var
  local_comment_string : string_40;

begin
  with input_code_ptr^ do begin
    if relval.kind = absolute_sc then
      init_line (addr, cv_hex (relval.offset, 4), 'DC.W')
    else
      init_line (addr, '0000', 'DC.W');
    local_comment_string := '';
    fio_write (listfb, reloc_text (relval, true, local_comment_string));
    if pc_relative then
      fio_write (listfb, '-*');
    if local_comment_string <> '' then begin
      fio_tab (listfb, comment_column);
      fio_write (listfb, local_comment_string);
    end;
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_word_code *);
$PAGE process_long_code
(* PROCESS_LONG_CODE - writes out a code_record of kind long_code,
                       which involves writing out a DC.L and a
                       relocatable operand field. *)

procedure process_long_code (input_code_ptr : code);

var
  local_comment_string : string_40;

begin
  with input_code_ptr^ do begin
    if relval.kind = absolute_sc then
      init_line (addr, cv_hex (relval.offset, 8), 'DC.L')
    else
      init_line (addr, '00000000', 'DC.L');
    local_comment_string := '';
    fio_write (listfb, reloc_text (relval, true, local_comment_string));
    if pc_relative then
      fio_write (listfb, '-*');
    if local_comment_string <> '' then begin
      fio_tab (listfb, comment_column);
      fio_write (listfb, local_comment_string);
    end;
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_long_code *);
$PAGE process_string_code
(* PROCESS_STRING_CODE - writes out a code_record of kind string_code,
                         which involves writing out a DC.B and the
                         string constant enclosed in quotes. *)

procedure process_string_code (input_code_ptr : code);

var
  local_string : string_12;
  local_counter : 1..6;

begin
  with input_code_ptr^ do begin
    if length (strval) = 0 then
      return; (* <--- No string to write out *)
    local_string := '';
    for local_counter := 1 to min (6, length (strval)) do
      local_string := local_string || 
		      cv_hex (ord (strval[local_counter]), 2);
    init_line (addr, local_string, 'DC.B');
    fio_write (listfb, '''' || strval || '''');
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_string_code *);
$PAGE process_len_code
(* PROCESS_LEN_CODE - writes out a code_record of kind len_code,
                      which involves writing out a DC.W and the decimal
                      value of the length word, converted to printable
                      characters. *)

procedure process_len_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    init_line (addr, cv_hex (lenval, 4), 'DC.W');
    fio_write (listfb, cv_int (lenval));
    fio_tab (listfb, comment_column);
    fio_write (listfb, 'String length word');
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_len_code *);
$PAGE process_set_code
(* PROCESS_SET_CODE - writes out a code_record of kind set_code,
                      which involves writing out a DC.W and a string
                      of 0's and 1's corresponto the false's and
                      true's in the setval boolean array. *)

procedure process_set_code (input_code_ptr : code);

const
  bit : array [boolean] of char = ('0', '1');

var
  local_num_words : integer;
  local_bit_number : integer;
  local_inner_counter : 0..15;
  local_outer_counter : integer;
  local_addr : code_address;
  local_operand : string_20;
  local_set_value : uns_word;

begin
  with input_code_ptr^ do begin
    local_num_words := dimension (setval) div 16;
    local_addr := addr;
    for local_outer_counter := 1 to local_num_words do begin
      local_operand := '';
      local_set_value := 0;
      for local_inner_counter := 0 to 15 do begin
        local_bit_number := (local_outer_counter - 1) * 16 + 
			    local_inner_counter;
        local_operand := local_operand || 
			 bit[setval[local_bit_number]];
        local_set_value := (local_set_value * 2) +
                           ord (setval[local_bit_number]);
      end (* inner for loop *);
      init_line (local_addr, cv_hex (local_set_value, 4), 'DC.W');
      fio_write (listfb, '%' || local_operand);
      fio_skip (listfb);
      local_addr := local_addr + 2;
    end (* outer for loop *);
  end (* with input_code_ptr^ *);
end (* process_set_code *);
$PAGE process_sreal_code
(* PROCESS_SREAL_CODE - writes out a code_record of kind sreal_code,
                        which involves writing out a DC.L and the
                        hexadecimal representation of the real value. *)

procedure process_sreal_code (input_code_ptr : code);

var
  local_int : integer;

begin
  with input_code_ptr^ do begin
    cvt_sreal (realval, local_int, false);
    init_line (addr, cv_hex (local_int, 8), 'DC.L');
    fio_write (listfb, '$' || cv_hex (local_int, 8));
    fio_tab (listfb, comment_column + 2);
    fio_write (listfb, cv_real (realval));
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_sreal_code *);
$PAGE process_dreal_code
(* PROCESS_DREAL_CODE - writes out a code_record of kind dreal_code,
                        which involves writing out a pair of DC.L's
                        containing the upper and lower words of the
                        hexadecimal representation of the real value. *)

procedure process_dreal_code (input_code_ptr : code);

var
  local_int_1 : integer;
  local_int_2 : integer;

begin
  with input_code_ptr^ do begin
    cvt_dreal (realval, local_int_1, local_int_2);
    init_line (addr, cv_hex (local_int_1, 8), 'DC.L');
    fio_write (listfb, '$' || cv_hex (local_int_1, 8));
    fio_skip (listfb);
    init_line (addr, cv_hex (local_int_2, 8), 'DC.L');
    fio_write (listfb, '$' || cv_hex (local_int_2, 8));
    fio_tab (listfb, comment_column + 2);
    fio_write (listfb, cv_real (realval));
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_dreal_code *);
$PAGE process_dtime_code
(* PROCESS_DTIME_CODE - writes out a code_record of kind dtime_code,
                        which involves writing out a DC.L and the 
                        hexadecimal representation of the time value. *)

procedure process_dtime_code (input_code_ptr : code);

var
  local_chars : string_12;

begin
  with input_code_ptr^ do begin
    local_chars := cv_hex (dtimeval.d, 8);
    if length (local_chars) > 8 then
      local_chars := substr (local_chars, length (local_chars) - 7, 8);
    init_line (addr, local_chars, 'DC.L');
    fio_write (listfb, '$' || local_chars);
    fio_tab (listfb, comment_column);
    fio_write (listfb, 'Date-time');
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_dtime_code *);
$PAGE process_block_code
(* PROCESS_BLOCK_CODE - writes out a code_record of kind block_code,
                        which involves writing out a DS.B and the
                        decimal representation of the blocksize.
                        This kind of code_record is used to reserve
                        a block of uninitialized memory. *)

procedure process_block_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    if blocksize < 1 then
      return; (* <--- No bytes to reserve *)
    init_line (addr, cv_hex (blocksize, 8), 'DS.B');
    fio_write (listfb, cv_int (blocksize));
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_block_code *);
$PAGE process_def_code
(* PROCESS_DEF_CODE - writes out a code_record of kind def_code,
                      which involves writing out the label associated
                      with this def_record in the label field, since
                      this is the definition of this label. *)

procedure process_def_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    force_old_label;
    label_string := def_name_text (defname);
    label_addr := addr;
  end (* with input_code_ptr^ *);
end (* process_def_code *);
$PAGE process_source_code
(* PROCESS_SOURCE_CODE - writes out a code_record of kind source_code,
                         which involves writing out a comment line
                         containing the file-page/line (statement)
                         information for the next line. *)

procedure process_source_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    fio_tab (listfb, label_column);
    fio_write (listfb, '*');
    fio_tab (listfb, comment_column);
    fio_write (listfb, 'LINE ');
    fio_write (listfb, cv_source_id (stmtid));
    if stmtindex > 1 then
      fio_write (listfb, ' (' || cv_int (stmtindex) || ')');
    fio_skip (listfb);
  end (* with input_code_ptr^ *);
end (* process_source_code *);
$PAGE process_comment_code
(* PROCESS_COMMENT_CODE - writes out a code_record of kind comment_code,
			  which involves writing out a comment block
			  containing the comment text. *)

procedure process_comment_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    fio_tab (listfb, label_column);
    fio_line (listfb, '*');
    fio_tab (listfb, label_column);
    fio_write (listfb, '*');
    fio_tab (listfb, opcode_column);
    fio_line (listfb, substr (cmtext, 1, length (cmtext)));
    fio_tab (listfb, label_column);
    fio_line (listfb, '*');
  end (* with input_code_ptr^ *);
end (* process_comment_code *);
$PAGE process_asm_label_code
(* PROCESS_ASM_LABEL_CODE - writes out a code_record of kind asm_label_code,
			    which involves writing out an XDEF directive for
			    the given label, followed by the definition of
			    the label itself, by writing it in the label
			    field. *)

procedure process_asm_label_code (input_code_ptr : code);

begin
  with input_code_ptr^ do begin
    if defd_by_pos then
      force_old_label;
    fio_tab (listfb, opcode_column);
    fio_write (listfb, 'XDEF');
    fio_tab (listfb, operand_column);
    fio_line (listfb, labtext);
    if defd_by_pos then begin
      label_string := labtext;
      label_addr := addr
    end
    else begin
      fio_tab (listfb, label_column);
      fio_write (listfb, labtext);
      fio_tab (listfb, opcode_column);
      fio_write (listfb, 'EQU');
      fio_tab (listfb, operand_column);
      fio_line (listfb, 'STATIC.+' || cv_int (static_offset))
    end
  end (* with input_code_ptr^ *);
end (* process_asm_label_code *);
$PAGE process_fill_code
(* PROCESS_FILL_CODE - writes out a code_record of kind fill_code,
                       which involves writing out a DS.B of length 1
                       as a fill byte to resume word alignment. *)

procedure process_fill_code (input_code_ptr : code);

begin
  init_line (input_code_ptr^.addr, '00000001', 'DS.B');
  fio_write (listfb, '1');
  fio_tab (listfb, comment_column);
  fio_line (listfb, 'Fill byte');
end (* process_fill_code *);
$PAGE list_code
(* LIST_CODE - determines the kind of the given code_record, and then
	       calls the appropriate routine to write it out. *)

procedure list_code (input_code_ptr : code);

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
      def_code       : process_def_code (input_code_ptr);
      pc_assert_code : ;
      source_code    : process_source_code (input_code_ptr);
      comment_code   : process_comment_code (input_code_ptr);
      asm_label_code : process_asm_label_code (input_code_ptr);
      fill_code      : process_fill_code (input_code_ptr);
      align_code     : ;
    end (* case *);
  end (* with input_code_ptr^ *);
end (* list_code *);
$PAGE mac_init
(* MAC_INIT - writes out the IDNT directive at the top of the listing,
              followed by a comment containing the time and date of
              the compile, and then then SECTION definitions for the
              static and code areas. *)

public procedure mac_init (input_pc : array [rel_sections] of code_address);

begin
  if list_file = '' then
    return;
  init_output;

  (* initialize the ESDID linked list head *)

  new (ESDID_list_head);
  with ESDID_list_head^ do begin
    next := nil;
    name := '';
  end (* with ESDID_list_head^ *);

  (* write out the header for the listing *)

  fio_page (listfb);
  fio_tab (listfb, label_column);
  fio_write (listfb, substr (root_block^.children^.id^.text || '          ',
                             1, 10));
  fio_tab (listfb, opcode_column);
  fio_write (listfb, 'IDNT');
  fio_tab (listfb, operand_column);
  fio_line (listfb, '0,0');
  fio_tab (listfb, label_column);
  fio_line (listfb, '*');
  fio_tab (listfb, label_column);
  fio_write (listfb, '*');
  fio_tab (listfb, opcode_column);
  fio_line (listfb, 'MC68000 Checkout Code Generator,  ' || 
		    ns_d2 (extr_date (daytime)) || '  ' || 
		    lowercase (ns_t1 (extr_time (daytime))));
  fio_tab (listfb, label_column);
  fio_line (listfb, '*');
  fio_tab (listfb, code_column);
  fio_write (listfb, cv_hex (section_numbers[static_section], 8));
  fio_tab (listfb, opcode_column);
  fio_write (listfb, 'SECTION');
  fio_tab (listfb, operand_column);
  fio_line (listfb, cv_int (section_numbers[static_section]));
  fio_tab (listfb, code_column);
  fio_write (listfb, cv_hex (input_pc[static_section], 8));
  fio_tab (listfb, label_column);
  fio_write (listfb, 'STATIC.');
  fio_tab (listfb, opcode_column);
  fio_write (listfb, 'EQU');
  fio_tab (listfb, operand_column);
  fio_line (listfb, '*');
  fio_tab (listfb, code_column);
  fio_write (listfb, cv_hex (section_numbers[code_section], 8));
  fio_tab (listfb, opcode_column);
  fio_write (listfb, 'SECTION');
  fio_tab (listfb, operand_column);
  fio_line (listfb, cv_int (section_numbers[code_section]));
  fio_tab (listfb, code_column);
  fio_write (listfb, cv_hex (input_pc[code_section], 8));
  fio_tab (listfb, label_column);
  fio_write (listfb, 'CODE.');
  fio_tab (listfb, opcode_column);
  fio_write (listfb, 'EQU');
  fio_tab (listfb, operand_column);
  fio_line (listfb, '*');
  curr_section := code_section;
end (* mac_init *);
$PAGE mac_list
(* MAC_LIST - writes out the assembly listing for a given code list. *)

public procedure mac_list (input_list : code_list;
                           input_section : rel_sections);

var
  local_code_ptr : code;

begin
  if list_file = '' then
    return;
  init_output;
  if input_section <> curr_section then begin
    fio_tab (listfb, code_column);
    fio_write (listfb, cv_hex (section_numbers[input_section], 8));
    fio_tab (listfb, opcode_column);
    fio_write (listfb, 'SECTION');
    fio_tab (listfb, operand_column);
    fio_line (listfb, cv_int (section_numbers[input_section]));
    curr_section := input_section;
  end;
  local_code_ptr := input_list.first;
  label_string := '';
  while local_code_ptr <> nil do begin
    list_code (local_code_ptr);
    local_code_ptr := local_code_ptr^.next;
  end (* while *);
  force_old_label;
end (* mac_list *);
$PAGE mac_term
(* MAC_TERM - writes out a comment at the end of the listing containing
              the sizes of the code, const, and static areas, followed
              by the END directive. *)

public procedure mac_term (input_code_size : unit_range;
                           input_const_size : unit_range;
                           input_static_size : unit_range);

var
  local_ESDID_ptr : ^ESDID_record;

begin
  if list_file = '' then
    return;
  init_output;

  (* emit XREF records *)

  local_ESDID_ptr := ESDID_list_head^.next;
  while local_ESDID_ptr <> nil do begin
    fio_tab (listfb, opcode_column);
    fio_write (listfb, 'XREF');
    fio_tab (listfb, operand_column);
    fio_line (listfb, local_ESDID_ptr^.name);
    local_ESDID_ptr := local_ESDID_ptr^.next;
  end (* while *);

  (* write out the end of the listing *)

  fio_tab (listfb, label_column);
  fio_line (listfb, '*');
  fio_tab (listfb, label_column);
  fio_write (listfb, '*');
  fio_tab (listfb, opcode_column);
  fio_line (listfb, 'Code area:      ' ||
		    cv_hex (input_code_size, 8) ||
                    ' bytes (' ||
		    cv_int (input_code_size) ||
		    ' decimal)');
  fio_tab (listfb, label_column);
  fio_write (listfb, '*');
  fio_tab (listfb, opcode_column);
  fio_line (listfb, 'Constant area:  ' ||
		    cv_hex (input_const_size, 8) ||
                    ' bytes (' ||
		    cv_int (input_const_size) ||
		    ' decimal)');
  fio_tab (listfb, label_column);
  fio_write (listfb, '*');
  fio_tab (listfb, opcode_column);
  fio_line (listfb, 'Static area:    ' ||
		    cv_hex (input_static_size, 8) ||
                    ' bytes (' ||
		    cv_int (input_static_size) ||
		    ' decimal)');
  fio_tab (listfb, label_column);
  fio_line (listfb, '*');
  fio_tab (listfb, code_column);
  fio_write (listfb, '00000000');
  fio_tab (listfb, opcode_column);
  fio_line (listfb, 'END');
  fio_close (listfb);
  lf_status := prev_opened;
end (* mac_term *).
E@?ø