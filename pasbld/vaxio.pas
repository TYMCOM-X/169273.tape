$TITLE VAXIO - VAX input/output routines

module vaxio;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM pasifu.inc
$SYSTEM vaxopc.inc
$SYSTEM vaxgen.inc
$SYSTEM vaxutl.inc
$SYSTEM pasmth.inc
$SYSTEM pascv.inc
$SYSTEM vaxstr.inc
$SYSTEM vaxset.inc
$SYSTEM vaxexp.inc
$SYSTEM vaxcgu.inc
$SYSTEM ptmcon.inc
$PAGE declarations

static var

  getputstring: boolean;	(* true if in sequence of reads/writes
				   to/from strings.	*)

  abinary_file : boolean;	(* true if writing and/or reading from a binary file *)

  clear_stack: boolean;		(* true if hidden parameters to be cleared from stack *)

  rt_entry: rt_symbol;		(* runtime entry point if GETPUTSTRING true *)
  get_put_str_desc: str_desc;
  file_arg_addr : addr_desc;
  getput_addr_desc : addr_desc;	(* Keeping track of get/put as the process
				   progresses. *)
$PAGE rt_open_call
(* RT OPEN CALL generates code for an open, reset, rewrite, or update op,
   and tags the expression node with the register in which it has been
   loaded.	*)

public function rt_open_call (node: expr): addr_desc;

var
  str_addr: str_desc;
  user_opt_set: set_desc;
  reg: registers;
  addr_ref: addr_ptr;
  ix, low_val, high_val: int_type;
  parmsize: parm_range;
  rts: rt_symbol;
  comp_options: record
    case boolean of
      true:  (opt_val: int_type);
      false: (opt_set: packed array[0..35] of boolean)
  end;
  open_mode: file_modes;
  open_size: bit_range;
  should_push_component_size : boolean;
  target_set: set_desc;

const	(* specific entries in the options set *)
  max_user_opt :=	35;
  min_user_opt :=	30;
  is_internal_name :=	29;
  is_packed_file :=	28;
  is_typed_file :=	27;
  is_binary_file :=	26;
  is_output_file :=	25;
  is_input_file :=	24;

begin
  comp_options.opt_val := 0;	(* sets all bits false *)
  should_push_component_size := false;	(* used to cope with dynamic temps *)
  with node^ do begin
    open_mode := desc.base^.file_kind;
    if open_mode = textfile then begin
      parmsize := 3;
      case opcode of
	open_op: rts := rt_open;
	rewrite_op: rts := rt_rewrite;
	reset_op: rts := rt_reset
      end;
    end
    else if open_mode = binaryfile then begin
      parmsize := 3;
      rts := rt_open_binary;
      comp_options.opt_set[is_binary_file] := true;
    end
    else begin	(* must be typed *)
      parmsize := 4;	(* include component size *)
      rts := rt_open_typed;
      open_size := (desc.base^.comp_size + byte_size - 1) div byte_size;
      should_push_component_size := true;
      comp_options.opt_set[is_typed_file] := true;
    end;
    comp_options.opt_set[is_internal_name] := operand[ 1 ] = nil;
    comp_options.opt_set[is_packed_file] := desc.base^.packable;
    case opcode of
      reset_op, open_op: comp_options.opt_set[is_input_file] := true;
      rewrite_op: comp_options.opt_set[is_output_file] := true;
      update_op: begin
	comp_options.opt_set[is_output_file] := true;
	comp_options.opt_set[is_input_file] := true;
      end
    end;

    (* get the users options set *)

    user_opt_set := set_fetch (operand[3], 0, bits_per_unit, no_preference, true);

    (* now get the filename string. *)

    if operand[ 1 ] <> nil
      then str_addr := fetchtranslated ( operand[ 1 ], actual_length )
      else str_addr := fetchtranslated ( operand[ 2 ], actual_length );

    if should_push_component_size
      then push_value ( int_value(open_size) , unsigned_value );

    if user_opt_set.nargs = 0 then
      push_value (int_value (comp_options.opt_val), unsigned_value)
    else if (user_opt_set.nargs = 2) andif aconstp (user_opt_set.arg[1], low_val) andif
            aconstp (user_opt_set.arg[2], high_val) then begin
      for ix := low_val to high_val do
        comp_options.opt_set [35 - ix] := true;
      push_value (int_value (comp_options.opt_val), unsigned_value)
    end
    else begin
      if user_opt_set.nargs = 2 then begin
	gen1 (clrl, push_reference);
        with target_set do begin
	  nargs := 3;
	  arg[1] := stack_top_reference;
	  arg[2] := int_value (0);
	  arg[3] := int_value (bytes_per_unit)
	end;
	if adr_equal (user_opt_set.arg[1], user_opt_set.arg[2]) then begin
	  emit_insv (1, user_opt_set, target_set);
	  set_free (user_opt_set)
	end
        else
	  gen_set (user_opt_set, target_set)
      end
      else begin
	gen2 (movzbl, user_opt_set.arg[1], push_reference);
        set_free (user_opt_set)
      end;
      gen2 (bisl2, int_value (comp_options.opt_val), stack_top_reference)
    end;

    push_value (duplicate_addr (str_addr.len_addr), unsigned_value);
    push_address (skip_len_words (str_addr));
    free_string (str_addr);
    gen_rt (parmsize, rts);

    (* Since the pointer to the file block is returned in register R0,
       move it to a more permanent register. *)

    reg := get_reg (bits_per_unit);
    gen_rr (movl, r0, reg);

    (* Return the register. *)

    rt_open_call := reg_addr (reg);

  end (* with *);
end (* rt_open_call *);
$PAGE free_file

(* FREE_FILE fetches the 'file variable' expression associated with an
   I/O operation.  This routine is useful when the file variable 
   expression must be fetched and immediately freed, simply to keep
   register usage counts correct.  A simple 'FREE ( FETCH ( EXP ) )'
   will not work since the 'file' variable may in fact be a IN_STR_OP
   or OUT_STR_OP (i.e., the I/O operation may be a GET/PUT STRING
   operation)  *)

public procedure free_file ( exp: expr );

begin
  with exp^ do begin

    if (opcode = in_str_op) or (opcode = out_str_op)
      then free_string ( fetchstring ( exp, no_length ) )
      else free ( fetch ( exp , no_preference ) );

  end;
end  (* proc free_file *) ;
$PAGE read_write_call

public procedure read_write_call (node: tuple);

var
  file_addr, temp_addr, item_addr, width_addr, temp_width_addr: addr_desc;
  string_addr: str_desc;
  have_field_width: boolean;
  width_value: int_type;
  parmsize: parm_range;
  mask: packed record
    case integer of
      1: (int: integer);
      2: (left_hw: 0..3777777b;	(* leftmost 20 bits *)
	  right_hw: 0..177777b);	(* rightmost 16 bits *)
      3: (bits: packed array[0..35] of boolean)
  end;
  check_input: boolean;
  cons_def: def;
  range_block: addr_desc;
  item_type_kind: type_kind;

const
  is_left_justified := 16;
  has_field_width := 15;
  is_octal := 14;
  is_hexadecimal := 13;
  has_real_field_width := 12;
  is_e_format := 11;
  is_f_or_e_format := 10;
  has_subrange_limits := 9;

begin
  with node^ do begin
    if rw_mode = binaryrw then begin
      file_addr := fetch ( rw_file , no_preference );
      item_addr := fetch ( rw_item , no_preference );
      width_addr:= fetch ( rw_width , no_preference );
      push_value ( file_addr , unsigned_value);	(* addr of file block *)
      if dynamic_flex (rw_item) then
	item_addr := increment_addr (item_addr, flex_arr_desc_size div bits_per_byte);
      push_mem_addr ( item_addr , alignment(rw_item) );
      push_value ( width_addr , unsigned_value);
      if opcode = read_op
	then gen_rt (3, rt_read_binary)
	else gen_rt (3, rt_write_binary);
    end
    else begin
      mask.int := 0;
      if rw_width <> nil then begin
	width_addr := fetch (rw_width, no_preference );
	have_field_width := true;
      end
      else have_field_width := false;

      (* The following code fetches the operands for the i/o operation.
	 This is done in this convoluted place because the routine was not
	 originally designed to handle dynamic temps. This entire routine
	 should be re-worked some day so that it is smaller and more readable *)

      if rw_item^.desc.kind <> strings
	then item_addr := fetch ( rw_item , no_preference );

      if rw_item^.desc.kind = reals
	then if rw_precision <> nil
	       then temp_width_addr := fetch ( rw_precision , no_preference )
	     else temp_width_addr := int_value ( 0 )
      else if rw_item^.desc.kind = strings
	then if opcode = read_op
	       then string_addr := fetchstring ( rw_item, max_length )
	     else string_addr := fetchtranslated ( rw_item , actual_length );


      (* If integer or real text file read and CHECK(INPUT) option specified,
	 then generate a lower and upper bound block and push pointer
	 to the block onto the stack.  *)

      check_input := (chk_inp_opt in cur_block^.semantic_options) and
		     (opcode = read_op) and
		     (rw_mode in [decimalrw, octalrw, hexrw, realrw, fixedrw, floatrw]);
      item_type_kind := rw_item^.desc.kind;

      (* Suppress input check if subrange limits are minimum(integer/real)
	 and maximum(integer/real).  *)

      if check_input then begin
	with rw_item^.desc.base^ do begin
          if item_type_kind = ints then
	    check_input := (minval <> minimum (longword)) or (maxval <> maximum (longword))
	  else if item_type_kind = reals then
	    check_input := (rminval <> type_real^.rminval) or
			   (rmaxval <> type_real^.rmaxval);
 	end;
      end;

      (* Push the length and address of the source, or file var *)
      if getputstring
	then begin
	  push_address ( duplicate_addr ( getput_addr_desc ) );
	  parmsize := 3;
	end
      else if not abinary_file
	then begin
	  push_value ( duplicate_addr ( file_arg_addr ), unsigned_value );
	  parmsize := 3;
	end
      else parmsize := 2;	(* as is usually the case *)
      if check_input then begin
	cons_def := make_def ( constant_def );
	mark_def ( cst_area, cons_def );
	range_block := absolute_reference;
	range_block.reloc := reldef ( cons_def );

	push_address ( range_block );
	parmsize := parmsize + 1;

	with rw_item^.desc.base^ do begin
	  if item_type_kind = ints then begin
	    gen_longword ( cst_area, minval );
	    gen_longword ( cst_area, maxval );
	  end
	  else if kind = reals then begin
	    gen_real ( cst_area, precision, rminval );
	    gen_real ( cst_area, precision, rmaxval );
	  end
	  else assert ( false );
	end;
      end;

      case rw_item^.desc.kind of

	ints: begin
	  if opcode = read_op
	    then push_address (item_addr)
	    else push_value (item_addr, alignment (rw_item));
	  mask.left_hw := 0;
	end;

	reals: begin
	  if have_field_width then begin
	    parmsize := parmsize + 1;
	    push_value (width_addr, unsigned_value);
	  end;
	  if rw_item^.desc.precision > srealprec then begin
	    push_mem_addr ( item_addr, alignment( rw_item ) );
	    mask.left_hw := 2;
	  end
	  else begin
	    mask.left_hw := 1;
	    if opcode = read_op
	      then push_address (item_addr)
	      else push_value (item_addr, signed_value);
	  end;
	  have_field_width := true;
	  width_addr := temp_width_addr;	(* Saved from above for dyn temps *)
	end (* reals *);

	strings: begin
	  push_value (duplicate_addr (string_addr.len_addr), unsigned_value);
	  if opcode = read_op then begin
	    push_address (skip_desc_word (string_addr));
	    if string_addr.type_desc.str_kind = varying
	      then mask.left_hw := 3
	      else mask.left_hw := 4;
	  end
	  else begin
	    push_address (skip_len_words (string_addr));
	    mask.left_hw := 4;
	  end;
	  free_string (string_addr);
	  parmsize := parmsize + 1;
	end;

	chars: begin
	  push_value (int_value (1), unsigned_value);
	  push_mem_addr ( item_addr, alignment( rw_item) );
	  parmsize := parmsize + 1;
	  mask.left_hw := 4;
	end;

	bools: begin
	  push_value (item_addr, unsigned_value);
	  mask.left_hw := 5;
	end
      end (* case *);

      (* Set info bits in mask for runtime *)

      with mask do begin
	bits[is_left_justified] := (rw_mode = leftrw);
	bits[has_field_width] := (rw_width <> nil) and (rw_item^.desc.kind <> reals);
	bits[is_octal] := (rw_mode = octalrw);
	bits[is_hexadecimal] := (rw_mode = hexrw);
	bits[has_real_field_width] := (rw_item^.desc.kind = reals) and (rw_width <> nil);
	bits[is_e_format] := (rw_mode = floatrw);
	bits[is_f_or_e_format] := (rw_mode = fixedrw) or
	  ( (rw_mode = floatrw) and (rw_precision <> nil) );
	bits[has_subrange_limits] := check_input;
      end;
      if not have_field_width then
	push_value (int_value (mask.int), unsigned_value)
      else if aconstp (width_addr, width_value) then
	push_value (int_value (mask.int + width_value), unsigned_value)
      else begin	(* merge with word on stack *)
	push_value (width_addr, unsigned_value);
	item_addr := stack_reference;
	item_addr.offset := 2;
	gen2 (bisw2, typ_int_value (mask.left_hw, vax_word), item_addr);
      end;
      if getputstring then
	gen_rt (parmsize, rt_entry)
      else if opcode = read_op
	then gen_rt (parmsize, rt_read)
        else gen_rt (parmsize, rt_write);
      
      free_file ( rw_file );		(* free file variable so reg usage *)

    end (* if not binary io *)
  end (* with node *)
end (* read_write_call *);
$PAGE io_begins
(* IO BEGINS accepts a tuple identifying the source or target of a 
   read, write, getstring or putstring. Except for binary files, the argument
   is evaluated and pushed on the stack for subsequent i/o operations. *)

public procedure io_begins (node: tuple);

var
  temp_addr , start_addr: addr_desc;
  str_addr : str_desc;
  temp_length: char_range;

begin
  with node^.file_arg^ do begin
    
    (* For GETSTRINGs the source string is first copied to a temp,
       then the address of the temp is passed to the runtime.  *)

    if opcode = in_str_op then begin	(* getstring *)
      str_addr := fetchstring (node^.file_arg, actual_length);
      get_put_str_desc := copy_string ( str_addr );	(* copy source string to temp *)
      rt_entry := rt_getstring;
      getputstring := true;
    end

    (* For PUTSTRINGs we allocate a temp for the destination string
       and push the temps address onto the stack.  
       Code will be emitted by IO_ENDS to copy the temp to the
       destination string.  *)

    else if opcode = out_str_op then begin	(* putstring *)
      str_addr := fetchstring (node^.file_arg, max_length);
     
      (* Allocate and initialize the temp.  *)

      temp_length := str_addr.max_len;
      get_put_str_desc := str_temp( str_addr.len_addr, temp_length,
				    str_addr.type_desc.str_kind );
      get_put_str_desc.type_desc := str_addr.type_desc;
      if operand[1]^.desc.str_kind = varying then begin
	rt_entry := rt_put_varying_string;
	gen1 ( clrw, get_put_str_desc.base_addr );
      end
      else rt_entry := rt_put_fixed_string;
      getputstring := true;
      free_string ( str_addr );
    end

    (* For text, typed and binary files save filevar in file_arg_addr *)

    else begin				(* TEXT , TYPED and BINARY files *)
      getputstring := false;
      file_arg_addr := fetch ( node^.file_arg , no_preference );
      free ( file_arg_addr );		(* Don't forget the usage counts *)
      abinary_file := desc.base^.file_kind = binaryfile;
    end;


    (* due to the vax runtime the length word of the source is being modigied
       and as such must be saved, or updated, for the next call. The following
       register will be used to keep track of the string index *)

    if getputstring
      then begin
	getput_addr_desc := get_temp ( int_value(8) , vax_long );

	(* Store the length word into the second part of the temp *)

	getput_addr_desc.offset := getput_addr_desc.offset + 4;
	store ( get_put_str_desc.len_addr , getput_addr_desc , unsigned_value );
	getput_addr_desc.offset := getput_addr_desc.offset - 4;

	if rt_entry = rt_getstring
	  then temp_addr := skip_len_words ( get_put_str_desc )
	else temp_addr := skip_desc_word ( get_put_str_desc );

	(* Move address of the string itself into the static temp *)

	gen2 ( movaw , temp_addr , getput_addr_desc );

	free ( temp_addr );
      end
  end (* with *);
end (* io_begins *);
$PAGE io_ends
(* IO ENDS frees hidden arguments to runtime i/o routines after a
   sequence of read/write calls.  For a PUTSTRING it must also terminate
   fixed length putstrings and copy the temp used to the destination 
   string.  *)

public procedure io_ends ( node: tuple );

var
  dest_desc: str_desc;
  length_word_addr: addr_desc;
  dummy: addr_desc;

  (* FREE_BOTH_STRS is passed as a parameter to MOVE_STRING and is used
     to free the string descriptors for the temp and the destination
     string during a PUTSTRING operation.  *)

  procedure free_both_strs;
    begin
      free_string ( get_put_str_desc );
      free_string ( dest_desc );
    end;

begin
  if getputstring
    then begin
      if rt_entry = rt_put_fixed_string
	then begin
	  push_address ( duplicate_addr ( getput_addr_desc ) );
	  gen_rt ( 1 , rt_put_last_string )
	end;
      free ( getput_addr_desc )
    end;

  (* If the operation was a PUTSTRING, we must copy the temporary
     string to the destination string.  Otherwise we simply free
     the string descriptors or address descriptors used, so that all
     usage counts work out correctly.  *)

  if getputstring and
     ( (rt_entry = rt_put_fixed_string) or (rt_entry = rt_put_varying_string) ) then begin
    dest_desc := fetchstring ( node^.file_arg, max_length );	(* fetch dest string *)
    
    (* Copy length word also if varying string.  *)

    if dest_desc.type_desc.str_kind = varying then begin
      length_word_addr := skip_desc_word ( dest_desc );
      gen2 ( movw, get_put_str_desc.base_addr, length_word_addr );
      free ( length_word_addr );
    end;
    
    (* Now copy the temp string to the PUTSTRING destination string. *)

    move_string ( dest_desc, get_put_str_desc, false, free_both_strs, false, dummy );
  end
  else if getputstring and (rt_entry = rt_getstring) then begin
    free_string ( get_put_str_desc );
    free_file ( node^.file_arg );
  end
  else free_file ( node^.file_arg );
end (* io_ends *);
$PAGE rt_io_call
(* RT IO CALL generates a call to a runtime IO routine, the file parameter
   to which is already on the stack. The parameter is cleared in the call. *)

public procedure rt_io_call (rts: rt_symbol);

begin

  (* the address of the file parameter. Dynamic temps used within
     the parameter list will render the old-style of pushing the file parameter
     first and then relying on it being at the top of the stack invalid. *)

  push_value ( duplicate_addr ( file_arg_addr ) , unsigned_value );
  gen_rt (1, rts);
  clear_stack := false;	(* done in call *)
end.
    Y Sí