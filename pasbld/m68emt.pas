$TITLE M68EMT - Main routines for M68000 Code Emitter
module m68emt options check, special (word);
$PAGE includes
$SYSTEM pascal
$SYSTEM pasfil
$SYSTEM pasist
$SYSTEM ptmcon
$SYSTEM pasmap
$SYSTEM pascv
$SYSTEM tmpnam
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68cgu
$SYSTEM m68pho
$SYSTEM m68rel
$SYSTEM m68mac
$SYSTEM passw
$PAGE location counters
var
  section_pc : array [rel_sections] of code_address;
$PAGE emt_init
(* EMT_INIT - initializes the location counters for each section to 0,
              and then checks to see which initialization routines
              need to be called, based on the options specified. *)

public procedure emt_init;

begin
  section_pc := (0, 0);
  if switch (prog_options.dump_switches, 'PEEP') then
    peep_init;
  if rel_file <> '' then
    rel_init;
  if assembly_opt in all_opts then
    mac_init (section_pc);
  if map_opt in all_opts then
    map_init;
end (* emt_init *);
$PAGE remove_instruction
(* REMOVE INACTIVE INSTR unchains and disposes of inactive code records. *)

public procedure remove_instruction (var cdl: code_list;
				     var c: code);

var
  ctemp: code;

begin
  with c^ do begin
    if next = nil then
      cdl.last := prev
    else
      next^.prev := prev;
    assert (prev <> nil);
    prev^.next := next;
    ctemp := c;
    c := prev;
  end;
  dispose (ctemp)
end (* remove_instruction *);
$PAGE compute_addresses
(*  COMPUTE ADDRESSES will scan a code list.  It will (1) insert any fill
    bytes that are needed to force proper alignment of data records, (2) provisionally
    set the address field of each code record to the current program counter,
    (3) resolve span dependent instruction lengths (see Szymanski, CACM 21:4) and
    assign final addresses to code records and symbolic definitions for labels,
    (4) validate any PC_ASSERT records in the code list, (5) delete any
    inactive code records, and (6) return the total number of bytes needed
    for the code list.  *)

procedure compute_addresses ( var cdl : code_list;
			      cur_section : rel_sections;
			      var list_size : unit_range );

var c,
    c_insert,
    first_sdi,
    last_sdi : code;
    pc : code_address;
    sdi_count : unit_range;
$PAGE force_alignment - in compute_addresses
(*  FORCE ALIGNMENT is called whenever a code record is processed which
    requires word alignment.  It generates a fill byte, if necessary,
    before the first non-data record (if any) immediately preceding the
    current data record (the one which requires alignment).  *)

procedure force_alignment;

var
  fill : code;

begin
  if c_insert = nil then
    c_insert := c;

  (*  Generate a fill byte if necessary.  *)

  if odd (pc) then begin
    new (fill, fill_code);
    fill^.next := c_insert;
    fill^.prev := c_insert^.prev;
    if c_insert^.prev = nil then
      cdl.first := fill
    else
      c_insert^.prev^.next := fill;
    c_insert^.prev := fill;
    fill^.addr := pc;
    pc := pc + 1;
  end;
end (* force_alignment *);
$PAGE fill_in_addresses - in compute_addresses
(*  FILL IN ADDRESSES will store the current PC in the address field of each
    code record from the first of the non-data records preceding the
    current record, up through the current record.  *)

procedure fill_in_addresses;

begin
  if c_insert = nil then
    c_insert := c;
  loop
    with c_insert^ do begin
      addr := pc;
      if kind = def_code then begin
	assert (not defname^.defined and (defname^.provisional_address = -1));
	defname^.provisional_address := pc
      end;
  exit if c_insert = c;
      c_insert := next;
    end;
  end;
  c_insert := nil;
end (* fill_in_addresses *);
$PAGE chain_sdi - in compute_addresses
(* CHAIN SDI threads together the span dependent instructions in preparation for
   resolving their lengths. *)

procedure chain_sdi;

begin
  if last_sdi = nil then
    first_sdi := c
  else
    last_sdi^.next_sdi := c;
  c^.next_sdi := nil;
  last_sdi := c;
  sdi_count := sdi_count + 1;
end (* chain_sdi *);
$PAGE compute_addresses - main routine
const
  non_data_codes: set of code_record_kind =
			   [def_code, pc_assert_code, source_code, comment_code, asm_label_code];
  aligned_data_codes: set of code_record_kind =
			   [inst_code, word_code, long_code, len_code, set_code, string_code,
			    sreal_code, dreal_code, dtime_code, align_code];
  basic_size: array [code_record_kind] of unit_range =
			   (2, 1, 2, 4, 0, 2, 0, 4, 8, 4, 0, 1, 0, 0, 0, 0, 0, 0);
  op_size: array [addr_mode] of 0..4 =
			   (0, 0, 0, 0, 0, 2, 2, 2, 2, 4, 2, 2, 2, 2, 0);
  quick_ops: set of specific_opcodes =
			   [add_qm_opc, asl_qd_opc, asr_qd_opc, lsl_qd_opc, lsr_qd_opc,
			    move_bd_opc, rol_qd_opc, ror_qd_opc, sub_qm_opc];

var
  iop : 1 .. 2;
  sdi_list: ^array [1..*] of packed record
      span,
      minimum_span: integer;
      long: 0..4;
      sdi_operand: 1..2;
      ext_word_offset: 2..4;
      sdi_ptr: code
    end;
  i, j: unit_range;
  nochange, byte_exceeded, word_exceeded: boolean;
  increment, minimum_distance: integer;
  labref: ^label_reference_record;

begin
  pc := section_pc [cur_section];
  gen_align (cdl);			(* Force even words in each code list. *)

  sdi_count := 0; (* count of span dependent instructions *)
  first_sdi := nil; (* thread through next_sdi field of code records *)
  last_sdi := nil;

  (* Make first pass over the code list assigning minimal addresses for each item. *)

  c := cdl.first;
  c_insert := nil;
  while c <> nil do begin
    if not c^.active then
      remove_instruction (cdl, c)
    else if c^.kind in non_data_codes then begin
      (* Cleanup after peepholer. *)
      if (c^.kind = def_code) andif (c^.ref_list <> nil) then
	repeat
	  labref := c^.ref_list;
	  c^.ref_list := c^.ref_list^.next_ref;
	  dispose (labref)
	until c^.ref_list = nil;
      if c_insert = nil then
	c_insert := c;
    end
    else with c^ do begin
      if kind in aligned_data_codes then
	force_alignment;
      fill_in_addresses;
      pc := pc + basic_size [kind];
      case kind of			(* Do special size computations. *)
	inst_code :
	  begin
	    if inst.operands[1].mode = pc_displacement_mode then begin
	      assert (inst.operands[2].mode <> pc_displacement_mode);
	      chain_sdi (* pc-relative data refs (to const areas) *)
	    end
	    else if (inst.operands[2].mode = pc_displacement_mode) and
		    (inst.opcode <> dbcc_dw_opc) then begin
	      assert ((inst.operands[1].mode = null_mode) or
		      (inst.opcode in [btst_dm_opc, btst_wm_opc]));
	      chain_sdi (* bcc, bsr, pea *)
	    end;
	    if inst.opcode in [bcc_p_opc, bsr_p_opc] then
	      inst.operands[2].value_size := size_byte
	      (* 'basic_size' will do *)
	    else begin
	      for iop := 1 to 2 do
		with inst.operands [iop] do begin
		  pc := pc + op_size [mode];
		  if (mode = immediate_mode) and (value_size = size_long) and
		     not (inst.opcode in quick_ops) then
		    pc := pc + 2
		end;
	      if inst.opcode in quick_ops then
		pc := pc - 2
	    end
	  end;
	string_code:	pc := pc + upperbound (strval);
	set_code:	pc := pc + (dimension (setval) div bits_per_byte);
	block_code:	pc := pc + blocksize;
	others:	(* basic size is ok *);
      end
    end;
    c := c^.next
  end (* while c <> nil *);

  list_size := pc - section_pc [cur_section]; (* actually, minimal size at this point *)


  (* Build table of span dependent instructions. *)

  new (sdi_list, sdi_count); (* allocate table for span dependent instructions *)

  c := first_sdi;
  for i := 1 to sdi_count do begin (* fill in sdi table *)
    with c^.inst, sdi_list^[i] do begin
      sdi_operand := 1 + ord (opcode in [bcc_p_opc, bsr_p_opc, pea_m_opc, btst_dm_opc, btst_wm_opc]);
      (* Spans are calculated relative to the extension word for the PC-relative
         operand, so we need to know how far after the instruction's address that is. *)
      if opcode <> btst_wm_opc then
	ext_word_offset := 2
      else
	ext_word_offset := 4;
      assert ((operands[sdi_operand].mode = pc_displacement_mode) and
	      (operands[sdi_operand].cst_part.kind = def_sc));
      with operands[sdi_operand].cst_part do
	if reldef^.defined then begin
	  assert (reldef^.defval.kind = code_sc);
	  minimum_span := offset + reldef^.defval.offset - (c^.addr + ext_word_offset)
	end
	else begin
	  assert ((offset = 0) and (reldef^.provisional_address >= 0));
	  minimum_span := reldef^.provisional_address - (c^.addr + ext_word_offset)
	end;
      if (abs (minimum_span) <= maximum (byte) div 3) or
	 (not (opcode in [bcc_p_opc, bsr_p_opc]) and
		  (abs (minimum_span) <= maximum (word) div 3)) then begin
	span := 0; (* resolved - in worst case byte still suffices *)
	if minimum_span = 0 then begin (* catch branch to immediately following label *)
	  assert (opcode = bcc_p_opc);
	  opcode := nop_x_opc; (* 0-disp. short branch wouldn't work *)
	  operands[2].mode := null_mode
	end
      end
      else
	span := minimum_span; (* remains to be resolved *)
      long := 0; (* bytes by which instruction length increased *)
      sdi_ptr := c (* pointer to the span dependent instr. itself *)
    end;
    c := c^.next_sdi
  end;

  (* Iterate over the span dependent instructions until all are exactly
     as long as they have to be. *)

  repeat
    nochange := true;
    for i := 1 to sdi_count do
      with sdi_list^[i], sdi_ptr^.inst do begin
	byte_exceeded := (span < minimum (byte)) or (maximum (byte) < span);
	word_exceeded := (span < minimum (word)) or (maximum (word) < span);
	if ((opcode in [bcc_p_opc, bsr_p_opc]) and
	    (operands[2].value_size = size_byte) and byte_exceeded)
		orif
	   ((opcode <> bcc_p_opc) and word_exceeded) then begin
	  pc := sdi_ptr^.addr; (* original (minimum) address of sdi *)
	  if opcode in [bcc_p_opc, bsr_p_opc] then
	    operands[2].value_size := succ (operands[2].value_size);
	  if word_exceeded then begin
	    assert (opcode <> bcc_p_opc);
	    if opcode = bsr_p_opc then
	      opcode := jsr_m_opc;
	    operands[sdi_operand].mode := abs_l_mode
	  end;
	  long := long + 2; (* record by how much instr. was lengthened *)
	  if (opcode = bcc_p_opc) or word_exceeded then
	    span := 0; (* mark this table entry as resolved *)
	  for j := 1 to sdi_count do (* look for branches whose span includes sdi i *)
	    if sdi_list^[j].span <> 0 then begin
	      minimum_distance := pc - (sdi_list^[j].sdi_ptr^.addr + sdi_list^[j].ext_word_offset);
	      if minimum_distance >= 0 then begin (* j before i *)
		if sdi_list^[j].minimum_span > minimum_distance then (* forward jump beyond i? *)
		  sdi_list^[j].span := sdi_list^[j].span + long
	      end
	      else if minimum_distance < 0 then (* j after i *)
		if sdi_list^[j].minimum_span < minimum_distance then
		  sdi_list^[j].span := sdi_list^[j].span - long
	    end;
	  nochange := false
	end
      end
  until nochange;


  (* Make a final pass over the entire code list bumping up the previously
     assigned addresses according to the increases from minimum size made above.
     The final values of def_code code records can now be assigned also. *)

  increment := 0;
  c := cdl.first;
  i := 0; (* index of last sdi passed *)

  while c <> nil do begin
    c^.addr := c^.addr + increment;
    if c^.kind = pc_assert_code then
      assert (c^.addr = c^.loc)
    else if c^.kind = def_code then
      def_assign (c^.defname, (c^.addr, code_sc, cur_section))
    else if (c^.kind = source_code) andif
	    (map_opt in cur_block^.semantic_options) and
	    (c^.stmtindex = 1) then
      map_write (c^.stmtid, c^.addr);
    if (i < sdi_count) andif (sdi_list^[i+1].sdi_ptr = c) then begin (* reached next sdi? *)
      i := i + 1;
      increment := increment + sdi_list^[i].long
    end;
    c := c^.next
  end;
  assert (i = sdi_count);
  dispose (sdi_list);

  list_size := list_size + increment; (* final size of code list *)
  section_pc [cur_section] := section_pc [cur_section] + list_size (* final value of pc after list *)

end (* compute_addresses *);
$PAGE purge_code_list
(*  PURGE CODE LIST releases the space used by a code list, and restores the
    list to its initial state.  *)

procedure purge_code_list ( var cdl : code_list );

var
  c1 : code;

begin
  while cdl.first <> nil do begin
    c1 := cdl.first;
    cdl.first := cdl.first^.next;
    dispose (c1);
  end;
  cdl.last := nil;
end (* purge_code_list *);
$PAGE emit_code
(* EMIT_CODE - is the only entry point into the emitter section of
               the code generator for the outside world.  It emits
               the code for a given code_list, writes out the assembly
               listing, if necessary, and empties the code_list. *)

public procedure emit_code (var list : code_list;
                            input_section : rel_sections;
                            input_options : set_of_options;
                            var code_size : unit_range;
			    instructions: boolean);

begin
  if (list.first <> list.last) or
     ((list.first <> nil) andif (list.first^.kind <> comment_code)) then begin
    (* The code list isn't empty or just a comment, so ... *)
    if instructions and switch (cur_block^.dump_switches, 'PEEP') then
      peephole_optimization (list);
    compute_addresses (list, input_section, code_size);
    if assembly_opt in cur_block^.semantic_options then
      mac_list (list, input_section);
    if rel_file <> '' then
      rel_emit (list, input_section)
  end
  else
    code_size := 0;
  purge_code_list (list);
end (* emit_code *);
$PAGE emt_fixup
public procedure emt_fixup (input_pc : code_address;
                            input_long_flag : boolean;
                            input_reloc : reloc_value;
                            input_patch : integer);

begin
  rel_patch (input_pc, section_pc[code_section], input_long_flag, 
	     input_reloc, input_patch);
end (* emt_fixup *);
$PAGE convert_map
procedure convert_map;

type
  map_record = record
    srcid : source_id;
    address : code_address;
  end;

var
  map_file : file of map_record;
  map_text : text;
  map_entry : map_record;
  curr_page : -1 .. max_page_no;

begin
  reset (map_file, tempname ('MAP'));
  if not eof (map_file) then begin
    rewrite (map_text, '.MAP ' || list_file);
    write (map_text, 'Module ');
    with root_block^.children^ do begin
      if id = nil then
	writeln (map_text, '??????')
      else
	writeln (map_text, id^.text);
    end (* with root_block^.children^ *);

    curr_page := -1;
    read (map_file, map_entry);
    while not eof (map_file) do begin
      with map_entry do begin
	if curr_page <> srcid.page_no then begin
	  curr_page := srcid.page_no;
	  writeln (map_text, cv_radix (address, adr_width), ' ',
			     cv_radix (curr_page, 3));
	end;
      end (* with map_entry *);
      read (map_file, map_entry);
    end;
  end;

  close (map_text);
end (* convert_map *);
$PAGE emt_term
(* EMT_TERM - calls the appropriate wrap-up routines, based on the
              options specified. *)

public procedure emt_term (input_start_addr : def;
                           input_code_size : unit_range;
                           input_const_size : unit_range;
                           input_static_size : unit_range);

begin
  if switch (prog_options.dump_switches, 'PEEP') then
    peep_term (assembly_opt in all_opts);
  if rel_file <> '' then
    rel_term (input_start_addr, section_pc);
  if assembly_opt in all_opts then
    mac_term (input_code_size, input_const_size, input_static_size);
  if (map_opt in all_opts) or
     (switch (prog_options.dump_switches, 'MAP')) then begin
(*    map_close;*)
    if switch (prog_options.dump_switches, 'MAP') then
      convert_map;
    map_print;
  end;
end (* emt_term *);
$PAGE emt_fatal
public procedure emt_fatal;

begin
  rel_fatal;
end (* emt_fatal *).
    