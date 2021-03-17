$TITLE p10rel - relocatable file emission
$LENGTH 43

module p10rel;

(*  -----
-----  *)
$PAGE declarations

$INCLUDE pascal.inc
$INCLUDE ptmcon.inc
$INCLUDE pasist.inc
$INCLUDE pasfil.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$INCLUDE p10cgu.inc
$INCLUDE pascv.inc
$INCLUDE p10gen.inc
$INCLUDE prgdir.inc

type
    left_right = ( left, right );
    binary_file = file of pdp10word;


var relf: binary_file;
type
    record_word = record
        word: pdp10word;
        next: ^ record_word
    end;

var
    cur_record: ^ record_word; (* The record being constructed. *)
    last_word: ^ record_word; (* The last word in the current record. *)
    cur_rel_word: ^ record_word; (* The latest relocation word in the record. *)
    cur_rec_count: code_address; (* The word count for the current record. *)

type
    str6 = packed array [1..6] of char;

var
    writing_code: boolean; (* True if CurRecord is a code record. *)
    last_address: code_address; (* If WritingCode, the last address written. *)
    code_count: code_address; (* The number of code words in this record. *)
$PAGE Link-10 item types

type
    item_type = 0 .. 37b;

const
    no_rec = 0;
    code_rec = 1;
    symbol_rec = 2;
    hiseg_rec = 3;
    entry_rec = 4;
    end_rec = 5;
    name_rec = 6;
    start_rec = 7;
    int_request_rec = 10b;
    polish_rec = 11b;
    lib_request_rec = 17b;

    def_global_sym = 1;
    def_local_sym = 2;
    req_global_sym = 14b;

    polish_byte = 0;
    polish_global = 2;
    polish_add = 3;
    polish_sub = 4;
    polish_str = 777777b;
    polish_stl = 777776b;
$PAGE rel_record

(*  RelRecord is called with a rel-file record type.  It wraps any record which
    is currently being constructed, and starts a new one of the specified type.  *)


procedure rel_record ( link_item: item_type );

var
    next_word: ^ record_word;

begin
  if cur_record <> nil then begin
    cur_record^.word.rh := cur_rec_count;
    while cur_record <> nil do begin
      relf^ := cur_record^.word;
      if cur_rec_count <> 0 then
        put (relf);
      next_word := cur_record^.next;
      dispose (cur_record);
      cur_record := next_word;
    end;
  end;

  if link_item <> no_rec then begin
    new (cur_record);
    with cur_record^ do begin
      word.lh := link_item;
      word.rh := 0;
      next := nil;
    end;
    last_word := cur_record;
    cur_rec_count := 0;
  end;
  writing_code := (link_item = code_rec);
end (* rel_record *);
$PAGE rel_word

(*  RelWord takes a PDP 10 word and a pair of relocation flags, and adds the
    word to the current record.  *)


procedure rel_word ( word: pdp10word; lrel, rrel: boolean );

var
    new_word: ^ record_word;
    rel_number: 1 .. 18;

begin
  rel_number := (cur_rec_count mod 18) + 1;
  cur_rec_count := cur_rec_count + 1;
  if rel_number = 1 then begin (* Insert a relocation word. *)
    new (new_word);
    last_word^.next := new_word;
    new_word^.next := nil;
    last_word := new_word;
    cur_rel_word := new_word;
  end;
  cur_rel_word^.word.rel_byte [rel_number] := 2 * ord (lrel) + ord (rrel);
  new (new_word);
  last_word^.next := new_word;
  new_word^.word := word;
  new_word^.next := nil;
  last_word := new_word;
end (* rel_word *);
$PAGE rel_int & rel_xwd

(*  RelInt is a wrapper for RelWord.  Instead of a PDP 10 word, it takes an
    integer value and turns it into a PDP 10 word.  *)


procedure rel_int ( x: int_type; rel: boolean );

var word: pdp10word;

begin
  word.value := x;
  rel_word (word, false, rel);
end (* rel_int *);




(*  RelXwd is a wrapper for RelWord.  Instead of a PDP 10 word, it takes two
    half-word integer values and constructs a PDP 10 word out of them.  *)


procedure rel_xwd ( lh: code_address; lrel: boolean;
                    rh: code_address; rrel: boolean );

var word: pdp10word;

begin
  word.lh := lh;
  word.rh := rh;
  rel_word (word, lrel, rrel);
end (* rel_xwd *);
$PAGE radix50

(*  Radix50 takes a four-bit code and a six-character symbol, and constructs
    a PDP 10 word out of them, translating the symbol into 32-bit radix-50
    format.  *)


public function radix50 ( code: int_type; sym: str6 ): pdp10word;

type
    rep_array = packed array [' ' .. '_'] of 0..50b;

const
    rep_50: rep_array =
     (    0,   0,   0,   0,   46b, 47b,   0,   0,     0,   0,   0,   0,     0,   0, 45b,   0,
         1b,  2b,  3b,  4b,    5b,  6b,  7b, 10b,   11b, 12b,   0,   0,     0,   0,   0,   0,
          0, 13b, 14b, 15b,   16b, 17b, 20b, 21b,   22b, 23b, 24b, 25b,   26b, 27b, 30b, 31b,
        32b, 33b, 34b, 35b,   36b, 37b, 40b, 41b,   42b, 43b, 44b,   0,     0,   0,   0, 47b  );

var
    i: 1 .. 6;
    symval: int_type;

begin
  symval := 0;
  for i := 1 to 6 do
    if sym [i] <> ' ' then
      symval := symval * 50b + rep_50 [sym [i]];
  radix50.code50 := code;
  radix50.sym50 := symval;
end (* radix_50 *);
$PAGE rel_50 & rel_sixbit

(*  Rel50 is a wrapper for RelWord.  Instead of a PDP 10 word, it takes a four-
    bit code and a six-character symbol name, and constructs a radix-50 PDP 10
    word from them.  *)


procedure rel_50 ( code: int_type; sym: str6 );

begin
  rel_word (radix50 (code, sym), false, false);
end (* rel_50 *);




(*  RelSixbit is a wrapper for RelWord.  Instead of a PDP 10 word, it takes a
    six-character symbol.  The emitted word is the symbol, converted to SIXBIT
    code.  *)


procedure rel_sixbit ( sym: str6 );

var
    word: pdp10word;
    i: 1 .. 6;

begin
  for i := 1 to 6 do
    word.sixbit [i] := ord (sym [i]) - ord (' ');
  rel_word (word, false, false);
end (* rel_sixbit *);
$PAGE rel_init

(*  RelInit opens the relocatable binary output file and emits those record
    which should be emitted at the start of the file, and which do not require
    processing of the intermediate form.  *)


public procedure rel_init;

var
    block: blk;
    symbols: sym;

begin
  if rel_file = '' then return; (* <---- no rel file *)
  rewrite (relf, '.' || rel_extension || ' ' || rel_file);
  if not eof (relf) then begin (* Bad rel file. *)
    writeln (tty, '?Unable to write rel file ', rel_file);
    rel_file := ''; (* Suppress any further relfile activity. *)
    return;
  end;
  rel_file := filename (relf); (* Save actual file name. *)
  cur_record := nil;

  (*  Write an ENTRY record, giving the name of each public variable, constant,
      procedure, or function in the module.  *)

  rel_record (entry_rec);

  symbols := root_block^.children^.id_list.first;
  while symbols <> nil do begin
    with symbols^ do begin
      if (kind in [vars, consts, conditions]) andif public_dcl then
	rel_50 (0, name^.text);
      symbols := next;
    end;
  end (* while symbols <> nil *);

  (*  Write a NAME record for the module.  *)

  rel_record (name_rec);
  with root_block^.children^ do
    rel_50 (0, id^.text);

  (*  Write a HISEG record for the module.  *)

  rel_record (hiseg_rec);
  rel_xwd (400000b, false, 400000b, false);

end (* rel_init *);
$PAGE back_chain

(*  BackChain is called with a definition node and the location of a reference
    to that definition.  It adds the referencing location to the left or right
    back-chain of the definition node, returning its new value.  *)


procedure back_chain ( d: def; lr: left_right; ic: code_address;
                      var rslt_value: code_address; var rslt_rel: boolean );

begin
  with d^ do begin
    if lr = left then begin
      rslt_value := lbacklink;
      lbacklink := ic;
    end
    else begin
      rslt_value := rbacklink;
      rbacklink := ic;
    end;
  end;
  rslt_rel := (rslt_value <> 0);
end (* back_chain *);
$PAGE offset_value

(*  OffsetValue is called with a relocatable value and a signed offset.  If the
    value plus the offset yields a positive value in the same relocation area
    as the original value, then this sum is returned.  Otherwise, back-chaining
    must be applied.  *)


procedure offset_value ( value: code_address; offset: int_type;
                         lr: left_right; ic: code_address;
                         var rslt_value: code_address; var rslt_rel: boolean );

var
    svalue: int_type;
    offset_base: def;

begin
  svalue := value + offset;
  if (svalue >= 0) andif
     ( (svalue < 400000b) = (value < 400000b) ) then begin
    rslt_value := halfword (svalue);
    rslt_rel := true;
  end
  else
    if value <= 400000b
      then back_chain (get_offset (low_base, svalue), lr, ic, rslt_value, rslt_rel)
      else back_chain (get_offset (high_base, svalue - 400000b), lr, ic, rslt_value, rslt_rel);
end (* offset_value *);
$PAGE relocate

(*  Relocate is called with a half-word value and its relocation syllable.
    It computes a half-word offset and relocation flag to represent the
    value in the rel file.  Back-chaining is handled by this routine.  *)


procedure relocate ( value: code_address; rel: rel_syllable;
                     lr: left_right; ic: code_address;
                     var rslt_value: code_address; var rslt_rel: boolean );

var
    svalue: int_type;
    sym_addr: code_address;
    ext_name: pdp10word;

begin
  if value <= 377777b
    then svalue := value
    else svalue := value - 1000000b;

  with rel do begin
    case kind of

      register_sc,
      absolute_sc,
      temp_sc:
        begin
          rslt_value := value;
          rslt_rel := false;
        end;

      runtime_sc:
        begin
          ext_name := radix50 (req_global_sym, rts_name [relrtsym]);
          back_chain (get_offset (get_extern (ext_name), svalue), lr, ic, rslt_value, rslt_rel);
        end;

      parameter_sc,
      local_sc:
        begin
          if relsym <> nil then
            svalue := svalue + relsym^.item_addr;
          rslt_value := halfword (svalue);
          rslt_rel := false;
        end;

      static_sc:
	with relsym^ do begin
	  if kind = conditions then
	    sym_addr := item_addr + size_init + size_uninit
	  else begin
	    assert (kind = vars);
	    if init_value.kind = no_value
	      then sym_addr := item_addr + size_init
	      else sym_addr := item_addr;
	  end;
	  offset_value (sym_addr, svalue, lr, ic, rslt_value, rslt_rel);
	end;

      external_sc:
        begin
          if relsym^.init_value.kind = no_value then
	    ext_name := radix50 (req_global_sym, relsym^.name^.text)
          else
	    ext_name := radix50 (req_global_sym, relsym^.init_value.valp^.str_val);
          back_chain (get_offset (get_extern (ext_name), svalue), lr, ic, rslt_value, rslt_rel);
        end;

      def_sc:
        with reldef^ do begin
          if defined then begin
            if relocatable then
              offset_value (addr, svalue, lr, ic, rslt_value, rslt_rel)
            else begin
              rslt_value := halfword (addr + svalue);
              rslt_rel := false;
            end;
          end
          else
            back_chain (get_offset (rel.reldef, svalue), lr, ic, rslt_value, rslt_rel);
        end;

      self_rel_sc:
        offset_value (ic, svalue, lr, ic, rslt_value, rslt_rel);

      others:
        (* no action, for now *)

    end (* case kind *);
  end (* with rel *);
end (* relocate *);
$PAGE rel_code

(*  RelCode takes a PDP 10 word, an address, and a pair of relocation syllables,
    and generates a code record word.  *)


public procedure rel_code ( word: pdp10word; var ic: code_address;
                            lreloc, rreloc: rel_syllable );

var
    lh, rh: code_address;
    lrel, rrel: boolean;

begin
  if rel_file <> '' then begin (* Don't bother if no rel file. *)
    if not writing_code orif (last_address <> ic - 1) orif (code_count = 180) then begin
      rel_record (code_rec);
      rel_int (ic, true);
      code_count := 1;
    end;
    last_address := ic;
    code_count := code_count + 1;

    relocate (word.lh, lreloc, left, ic, lh, lrel);
    relocate (word.rh, rreloc, right, ic, rh, rrel);
    rel_xwd (lh, lrel, rh, rrel);
  end (* if rel_file <> '' *);
  ic := ic + 1; (* Increment ic in any case. *)
end (* rel_code *);
$PAGE local_request

(*  LocalRequest is called with a definition node.  If the definition address can be
    emitted in a local request (i.e., does not require a Polish fixup), then a
    local request word is emitted.  *)


procedure local_request ( d: def );

var addr_val: code_address;

begin
  with d^ do begin
    if not fixup_required then begin
      if deftype = offset_def
        then addr_val := halfword (offset + reldef^.addr)
        else addr_val := addr;
      if rbacklink <> 0 then
        rel_xwd (rbacklink, true, addr_val, relocatable);
      if lbacklink <> 0 then begin
        rel_int (-1, false);
        rel_xwd (lbacklink, true, addr_val, relocatable);
      end;
    end;
  end;
end (* local_request *);
$PAGE local_polish

(*  LocalPolish is called with a definition node.  If a Polish fixup record is
    required for the definition address, then one is generated.  *)


procedure local_polish ( d: def );

var
    lbyte: code_address;
    lrel: boolean;

begin
  with d^ do begin
    if fixup_required andif ( (rbacklink <> 0) or (lbacklink <> 0) ) then begin
      rel_record (polish_rec);
      rel_xwd (polish_sub, false, polish_byte, false);
      rel_xwd (reldef^.addr, true, polish_byte, false);
      lbyte := - offset;
      lrel := false;
      if rbacklink <> 0 then begin
        rel_xwd (lbyte, lrel, polish_str, false);
        lbyte := rbacklink;
        lrel := true;
      end;
      if lbacklink <> 0 then begin
        rel_xwd (lbyte, lrel, polish_stl, false);
        lbyte := lbacklink;
        lrel := true;
      end;
      rel_xwd (lbyte, lrel, 0, false);
    end;
  end;
end (* local_polish *);
$PAGE global_polish

(*  GlobalPolish is called with an offset or left-backchained external definition
    node.  It generates a Polish fixup record for the external symbol.  *)


procedure global_polish ( d: def );

begin
  with d^ do begin
    if (deftype = offset_def) and ( (rbacklink <> 0) or (lbacklink <> 0) ) then begin
      rel_record (polish_rec);
      if offset >= 0
        then rel_xwd (polish_add, false, polish_global, false)
        else rel_xwd (polish_sub, false, polish_global, false);
      rel_word (reldef^.ext_name, false, false);
      rel_xwd (polish_byte, false, abs (offset), false);
      if rbacklink <> 0 then
        rel_xwd (polish_str, false, rbacklink, true);
      if lbacklink <> 0 then
        rel_xwd (polish_stl, false, lbacklink, true);
    end

    else if (deftype <> offset_def) and (lbacklink <> 0) then begin
      rel_record (polish_rec);
      rel_xwd (polish_global, false, ext_name.lh, false);
      rel_xwd (ext_name.rh, false, polish_stl, false);
      rel_xwd (lbacklink, true, 0, false);
    end;
  end;
end (* global_polish *);
$PAGE rel_end

(*  RelEnd emits any necessary fixup records (global requests and Polish fixups),
    the public SYMBOL records, the LIBRARY REQUEST records, the START ADDRESS
    record if this is a program, and the END record.  It then closes the rel
    file.  *)


public procedure rel_end ( start_addr: def; lowseg_fin, highseg_fin: code_address )
  options special(coercions);

var
    true_highseg_fin: code_address;
    d, d1: def;
    vl: vl_link;
    emit_symbols: boolean;
    rw_rt: rt_symbol;
    dt: def_types;
    b: blk;
    s: sym;
    libdir: integer;

begin
  if rel_file = '' then return; (* <---- no rel file *)

  (*  Emit the SYMBOL records.  *)

  rel_record (symbol_rec);

  (*  If this is a main program, define the program name and START. symbols.  *)

  if root_block^.children^.kind = program_blk then begin
    rel_50 (def_global_sym, root_block^.children^.id^.text);
    rel_int (start_addr^.addr, true);
    rel_50 (def_global_sym, 'START.');
    rel_int (start_addr^.addr, true);
  end;

  (*  Public variable and constant definition.  Normally, these are simply
      defined as public symbols.  However, if this is an overlay compilation,
      then it is necessary to emit an indirect word in the high segment for 
      each, and to define the public symbols as the addresses of the indirect
      words.  The following loop is executed with EmitSymbols = True only
      (the normal case), or = False, then = True.  *)

  for emit_symbols := (not prog_options.overlay_opt) to true do begin
    true_highseg_fin := highseg_fin;
    if emit_symbols then
      rel_record (symbol_rec)
    else begin
      rel_record (code_rec);
      rel_int (highseg_fin, true);
    end;

    (*  Define all public variables.  *)

    vl := vl_list;
    while vl <> nil do begin
      with vl^ do begin
	with symbol^ do begin
	  if (kind = vars) andif public_dcl andif
	     not (dcl_class in [dynamic_sc, fileblk_sc, opt_sc]) then begin
	    if emit_symbols then
	      rel_50 (def_global_sym, name^.text);
	    if emit_symbols and prog_options.overlay_opt then
	      rel_int (true_highseg_fin, true)
	    else begin
	      if init_value.kind = no_value
		then rel_int (item_addr + size_init, true)
		else rel_int (item_addr, true);
	    end;
	    true_highseg_fin := true_highseg_fin + 1;
	  end;
	end;
	vl := last;
      end;
    end (* while vl <> nil *);

    (*  Define all public constants.  *)

    s := root_block^.children^.id_list.first;
    while s <> nil do begin
      with s^ do begin
	if (kind = consts) andif public_dcl andif (init_value.kind <> subr_cst) then begin
	  if emit_symbols then
	    rel_50 (def_global_sym, name^.text);
	  if emit_symbols and prog_options.overlay_opt then
	    rel_int (true_highseg_fin, true)
	  else begin
	    d := def (init_value.defp);
	    with d^ do begin
	      if deftype = offset_def
		then rel_int (offset + reldef^.addr, relocatable)
		else rel_int (addr, relocatable);
	    end;
	  end;
	  true_highseg_fin ue_highseg_fin + 1;
	end;
	s := next;
      end;
    end (* while s <> nil *);
  end (* for emit_symbols *);

  (*  Define all public exceptions.  (These are their own indirect words, so
      no special overlay processing is needed.  *)

  s := root_block^.children^.id_list.first;
  while s <> nil do begin
    with s^ do begin
      if (kind = conditions) and public_dcl then begin
	rel_50 (def_global_sym, name^.text);
	rel_int (item_addr + size_init + size_uninit, true);
      end;
      s := next;
    end;
  end (* while s <> nil *);

  (*  Define all public subroutines.  *)

  d := def_lists [subr_def];
  while d <> nil do begin
    with d^ do begin
      with blk_list^[defnumber]^ do begin
        if (kind = subr_blk) andif subr_sym^.public_dcl then begin
	  rel_50 (def_global_sym, subr_sym^.name^.text);
          rel_int (d^.addr, d^.relocatable);
        end;
      end;
      d := next;
    end;
  end (* while d <> nil *);

  (*  Request all non-offset, right-backchained external symbols.  *)

  d := def_lists [extern_def];
  while d <> nil do begin
    with d^ do begin
      if rbacklink <> 0 then begin
        rel_word (ext_name, false, false);
        rel_int (rbacklink, true);
      end;
      d := next;
    end;
  end (* while d <> nil *);

  (*  If this is a main compilation, request the selected allocator.  *)

  if root_block^.children^.kind = program_blk then begin
    rel_50 (req_global_sym, 'ALC' || cv_int (prog_options.alloc_mode) || '.');
    rel_int (0, false);
  end;

  (*  Emit any requested i/o symbols.  *)

  for rw_rt := rt_int_read to rt_bool_write do
    if rw_request [rw_rt] then begin
      rel_50 (req_global_sym, rts_name [rw_rt]);
      rel_int (0, false);
    end;

  (*  Request the special "no attention masking", "no underflow exceptions",
      and "allconditions required" symbols as appropriate.  *)

  if not prog_options.masking_opt then begin
    rel_50 (req_global_sym, 'NMASK.');
    rel_int (0, false);
  end;
  if not prog_options.underflow_opt then begin
    rel_50 (req_global_sym, 'NUFLO.');
    rel_int (0, false);
  end;
  if allc_required then begin
    rel_50 (req_global_sym, 'EX.ALL');
    rel_int (0, false);
  end;

  (*  Emit the LOCAL REQUEST back-chains.  *)

  rel_record (int_request_rec);
  for dt := sym_def to code_def do begin
    d := def_lists [dt];
    while d <> nil do begin
      local_request (d);
      d1 := d^.first_offset;
      while d1 <> nil do begin
        local_request (d1);
        d1 := d1^.next;
      end;
      d := d^.next;
    end (* while d <> nil *);
  end (* for dt *);

  (*  Emit the POLISH back-chain records for any local requests requiring fixup.  *)

  for dt := sym_def to code_def do begin
    d := def_lists [dt];
    while d <> nil do begin
      local_polish (d);
      d1 := d^.first_offset;
      while d1 <> nil do begin
        local_polish (d1);
        d1 := d1^.next;
      end;
      d := d^.next;
    end (* while d <> nil *);
  end (* for dt *);

  (*  Emit the POLISH back-chain records for any offset or left-backlinked
      global requests.  *)

  d := def_lists [extern_def];
  while d <> nil do begin
    with d^ do begin
      if lbacklink <> 0 then
        global_polish (d);
      d1 := first_offset;
      while d1 <> nil do begin
        global_polish (d1);
        d1 := d1^.next;
      end;
      d := next;
    end;
  end (* while d <> nil *);

  (*  Emit the LIBRARY REQUEST and START ADDRESS records if this is a
      main program.  *)

  if root_block^.children^.kind = program_blk then begin
    rel_record (lib_request_rec);
	if prog_options.ki_code_opt then
	    rel_sixbit ('PAILIB') 
	  else
	    rel_sixbit ('PASLIB');
    prgm_ppn (libdir);
    rel_int (libdir, false);
    rel_sixbit ('DSK');

    rel_sixbit ('FORLIB');
    rel_int (libdir, false);
    rel_sixbit ('DSK');

    rel_record (start_rec);
    rel_int (start_addr^.addr, true);
  end;

  (*  Emit the END record.  *)

  rel_record (end_rec);
  if prog_options.overlay_opt
    then rel_int (true_highseg_fin, true)
    else rel_int (highseg_fin, true);
  rel_int (lowseg_fin, true);

  rel_record (no_rec);
  close (relf);
end (* rel_end *).
   :@Gë