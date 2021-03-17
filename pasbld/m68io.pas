$TITLE M68IO - M68000 I/O Routines
module m68io options check, special (word);
$PAGE includes
$SYSTEM pascal
$SYSTEM pasist
$SYSTEM ptmcon
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68gen
$SYSTEM m68cgu
$SYSTEM m68set
$SYSTEM m68str
$SYSTEM m68utl
$SYSTEM m68exp
$PAGE locals
var
  getputstring_flag : boolean;
  file_arg_addr : op_desc;
  getputstring_desc: string_descriptor;
  rt_routine: rt_symbol;
  get_put_block: op_desc;
$PAGE rt_open_call
public function rt_open_call (input_file : expr) : op_desc;

var
  local_open_mode : file_modes;
  local_rts : rt_symbol;
  local_flags : packed record
    case boolean of
      true  : (word : uns_word;
               fill : 0..2**20 - 1);
      false : (bit : packed array [0..15] of boolean);
  end;
  local_string_addr : string_descriptor;
  local_opt_set : set_desc;
  local_target_set : set_desc;
  local_op_desc : op_desc;
  local_element_size : uns_word;
  local_counter : 0..15;
  local_low_value : 0..15;
  local_high_value : 0..15;

begin
  local_flags.word := 0;
  with input_file^ do begin

    (* get the filename string *)

    if operand[1] = nil then
      local_string_addr := fetchtranslated (operand[2], actual_length)
    else
      local_string_addr := fetchtranslated (operand[1], actual_length);

    (* get the users option set *)

    local_opt_set := set_fetch (operand[3], 0, bits_per_word,
                                no_preference, true);

    (* get the runtime symbol and set some of the option flags *)

    local_open_mode := desc.base^.file_kind;
    if local_open_mode = textfile then begin
      case opcode of
        open_op    : local_rts := rt_open_text;
        rewrite_op : local_rts := rt_rewrite_text;
        reset_op   : local_rts := rt_reset_text;
      end;
    end
    else begin
      if local_open_mode = typedfile then begin
        local_flags.bit[12] := true;
        local_rts := rt_open_typed;
        local_element_size := (desc.base^.comp_size + bits_per_byte - 1)
                              div bits_per_byte;
      end
      else begin
        local_flags.bit[12] := true;
        local_flags.bit[13] := true;
        local_rts := rt_open_binary;
      end;
    end;
    if operand[1] <> nil then
      local_flags.bit[10] := true;
    local_flags.bit[11] := desc.base^.packable;
    if opcode in [rewrite_op, update_op] then
      local_flags.bit[14] := true; (* output file *)
    if opcode in [open_op, reset_op, update_op] then
      local_flags.bit[15] := true; (* input file *)

    (* push the element size, for typed files only *)

    if local_open_mode = typedfile then
      pushi (local_element_size, size_word);

    (* push the filename string pointer and length *)

    pushstring (local_string_addr, nonvarying);
    pushlength (local_string_addr);

    (* free the string descriptor *)

    free_string (local_string_addr);

    (* set and push option word, and free the set descriptor, 
       if necessary *)

    with local_opt_set do begin
      if nargs = 0 then
        pushi (local_flags.word, size_word)
      else begin
        if nargs = 2 then begin
          if aconstp (arg[1], local_low_value) and
             aconstp (arg[2], local_high_value) then begin
            for local_counter := local_low_value to local_high_value do
              local_flags.bit[local_counter] := true;
            pushi (local_flags.word, size_word);
          end
          else begin
            pushi (0, size_word);
            with local_target_set do begin
              nargs := 3;
	      in_temp := false;
              arg[1] := stack_top_word;
              arg[2] := int_desc (0, size_long, true);
              arg[3] := int_desc (1, size_long, true);
            end (* with local_target_set *);
            if ops_equal (arg[1], arg[2]) then begin
              emit_bit_inst (1, local_opt_set, local_target_set);
              set_free (local_opt_set);
            end
            else
              genset (local_opt_set, local_target_set);
	    gen_im (or_opc, local_flags.word, stack_top_word);
          end;
        end
        else begin
          local_op_desc := duplicate_desc (arg[1]);
          local_op_desc.value_size := size_word;
          push (local_op_desc, size_word);
          set_free (local_opt_set);
	  gen_im (or_opc, local_flags.word, stack_top_word);
        end;
      end;
    end (* with local_opt_set *);

    (* generate the call to the runtime routine *)

    gen_rt (local_rts);

    (* return the result descriptor *)

    rt_open_call := pop_long;
  end (* with input_file^ *);
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
      then free_string ( fetchstring ( exp, no_length, no_limit ) )
      else free_desc ( fetch ( exp, all_modes, immediate_mode, any_size, false ) );

  end;
end  (* proc free_file *) ;
$PAGE rt_io_call
public procedure rt_io_call (input_file : expr;
                             input_rts : rt_symbol);

var
  local_op_desc : op_desc;

begin
  local_op_desc := fetch (input_file, nonstack_modes + stack_modes,
                          postincrement_mode, any_size, true);
  push (local_op_desc, size_long);
  gen_rt (input_rts);
end (* rt_io_call *);
$PAGE rt_seek_call
public procedure rt_seek_call (input_file : expr;
                               input_index : expr);

var
  local_file_addr : op_desc;
  local_index_addr : op_desc;

begin

  (* fetch all parameters *)

  local_file_addr := fetch (input_file, nonstack_modes,
                            immediate_mode, any_size, true);
  local_index_addr := fetch (input_index, nonstack_modes,
                             immediate_mode, any_size, true);

  (* push all parameters *)

  push (local_index_addr, size_long);
  push (local_file_addr, size_long);

  (* generate the runtime call *)

  gen_rt (rt_seek);
end (* rt_seek_call *);
$PAGE io_begins
public procedure io_begins (input_node : tuple);

var
  areg: addr_regs;
  str_desc: string_descriptor;
  temp_desc: op_desc;

  (* FREE_RHS is passed as a parameter to MOVE_STRING to free the source
     string being copied to a temporary. *)

  procedure free_rhs;

  begin
    free_string ( str_desc );
  end;

begin
  with input_node^.file_arg^ do begin
    if opcode = in_str_op then begin

      (* getstring *)

      str_desc := fetchstring ( input_node^.file_arg, actual_length, no_limit );
      init_string_descriptor ( getputstring_desc );
      with getputstring_desc do begin
	type_desc := str_desc.type_desc;
	max_len := str_desc.max_len;
	len_addr := duplicate_desc ( str_desc.len_addr );

	(* Allocate a temporary into which the source string will be copied. *)

	if type_desc.str_flex
	  then base_addr := get_temp ( duplicate_desc ( len_addr ), false )
	  else base_addr := get_temp ( int_desc ( max_len, size_word, false ), false );
	text_valid := true;
	text_addr := duplicate_desc ( base_addr );
      end (* with *);
      move_string ( getputstring_desc, str_desc, false, free_rhs, false, false );

      (* Allocate and initialize the runtime's control block. *)

      get_put_block := get_temp ( int_desc ( 8, size_long, false ), false );
      get_put_block := increment_addr ( get_put_block, 6 );
      get_put_block.value_size := size_word;
      gen_mm ( move_opc, getputstring_desc.len_addr, get_put_block );
      get_put_block := increment_addr ( get_put_block, -2 );
      gen_m ( clr_opc, get_put_block );
      get_put_block := increment_addr ( get_put_block, -4 );
      get_put_block.value_size := size_long;
      areg := get_areg ();
      gen_mm ( lea_opc, getputstring_desc.base_addr, reg_desc ( areg, size_long, true ) );
      gen_rm ( move_opc, areg, get_put_block );
      free_reg ( areg );
      rt_routine := rt_getstring;
      getputstring_flag := true;
    end
    else if opcode = out_str_op then begin

      (* putstring *)

      str_desc := fetchstring ( input_node^.file_arg, max_length, no_limit );

      (* Allocate a temporary for the putstring, in case the target string
	 overlaps. *)

      init_string_descriptor ( getputstring_desc );
      with getputstring_desc do begin
	type_desc := str_desc.type_desc;
	max_len := str_desc.max_len;
	if type_desc.str_flex then begin
	  if type_desc.str_kind = varying
	    then base_addr := str_varying_dynamic_temp ( str_desc.len_addr )
	    else base_addr := get_temp ( duplicate_desc ( str_desc.len_addr ), false );
	end
	else begin
	  if type_desc.str_kind = varying
	    then base_addr := get_temp ( int_desc ( str_desc.max_len + 2, size_word, false ), false )
	    else base_addr := get_temp ( int_desc ( str_desc.max_len, size_word, false ), false );
	end;
	if type_desc.str_kind = varying then begin	(* Init length to zero *)
	  getputstring_desc.len_addr := duplicate_desc ( getputstring_desc.base_addr );
	  getputstring_desc.len_addr.value_size := size_word;
	  temp_desc := skip_desc_word ( getputstring_desc );
	  gen_m ( clr_opc, temp_desc );
	  free_desc ( temp_desc );
	  rt_routine := rt_putstring_varying;
	end
	else begin
	  getputstring_desc.len_addr := duplicate_desc ( str_desc.len_addr );
	  rt_routine := rt_putstring_fixed;
	end;
      end;

      (* Allocate the runtime's control block and initialize it. *)

      get_put_block := get_temp ( int_desc ( 8, size_long, false ), false );
      get_put_block := increment_addr ( get_put_block, 6 );
      get_put_block.value_size := size_word;
      if str_desc.type_desc.str_flex
	then gen_mm ( move_opc, str_desc.len_addr, get_put_block )
	else gen_im ( move_opc, str_desc.max_len, get_put_block );
      get_put_block := increment_addr ( get_put_block, -2 );
      gen_m ( clr_opc, get_put_block );
      get_put_block := increment_addr ( get_put_block, -4 );
      areg := get_areg ();
      gen_mm ( lea_opc, getputstring_desc.base_addr, reg_desc ( areg, size_long, true ) );
      get_put_block.value_size := size_long;
      gen_rm ( move_opc, areg, get_put_block );
      free_reg ( areg );
      free_string ( str_desc );
      getputstring_flag := true;
    end
    else begin

      (* file i/o *)

      getputstring_flag := false;
      file_arg_addr := fetch (input_node^.file_arg,
			      nonstack_modes,
			      immediate_mode, any_size, true);
      free_desc (file_arg_addr);
    end;
  end (* with input_node^.file_arg^ *);
end (* io_begins *);
$PAGE io_ends
public procedure io_ends (input_node : tuple);

var
  str_desc: string_descriptor;
  length_word_addr, len_desc: op_desc;

  (* FREE_BOTH_STRINGS is passed as a parameter to MOVE_STRING to free
     the string descriptors for the temporary and destination strings
     in a PUTSTRING operation. *)

  procedure free_both_strings;
  begin
    free_string ( getputstring_desc );
    free_string ( str_desc );
  end;

begin
  if getputstring_flag then begin

    (* If this was a putstring to a fixed string, call the runtime to
       pad the target. *)

    if rt_routine = rt_putstring_fixed then begin
      pusha ( get_put_block );
      gen_rt ( rt_putstring_pad );
    end;
    if rt_routine = rt_getstring then begin
      free_string ( getputstring_desc );
      free_file ( input_node^.file_arg );
    end
    else begin

      (* For a putstring we must copy the temporary to the target string. *)

      str_desc := fetchstring ( input_node^.file_arg, max_length, no_limit );
      if str_desc.type_desc.str_kind = varying then begin	(* must copy length word *)
	length_word_addr := skip_desc_word ( str_desc );
	len_desc := skip_desc_word ( getputstring_desc );
	gen_mm ( move_opc, len_desc, length_word_addr );
	free_desc ( len_desc );
	free_desc ( length_word_addr );
      end;
      move_string ( str_desc, getputstring_desc, false, free_both_strings, false, false );
    end
  end
  else begin

    (* file code *)

    free_file ( input_node^.file_arg );
  end;
end (* io_ends *);
$PAGE check_desc
procedure check_desc (var var_desc : op_desc;
                      input_desc : expr_type_desc);

var
  local_desc : op_desc;
  local_size : 1..4;

begin

  (* Double precision real numbers always have a base type of REAL, which
     confuses OPS_TYPE into believing they are SIZE_LONG. Since no action
     by CHECK_DESC should be necessary anyway, we merely assert that such
     reals are where they should be. *)

  if (input_desc.kind = reals) andif (input_desc.precision > srealprec) then begin
    assert (not (var_desc.mode in register_modes + [immediate_mode] + stack_modes));
  end
  else begin
    ops_type (local_desc, input_desc.base, false);
    var_desc := coerce (var_desc, nonstack_modes, var_desc.mode,
			[local_desc.value_size], local_desc.signed_value);
    if var_desc.mode in register_modes + [immediate_mode] then begin
      if var_desc.mode in register_modes then begin
	case var_desc.value_size of
	  size_byte : local_size := 1;
	  size_word : local_size := 2;
	  size_long : local_size := 4;
	end (* case *);
	local_desc := get_temp (int_desc (local_size, no_size, true),
				false);
	local_desc.value_size := var_desc.value_size;
	gen_mm (move_opc, var_desc, local_desc);
      end
      else begin
	assert (var_desc.cst_part.kind = absolute_sc);
	local_desc := gen_cst (var_desc.cst_part.offset,
			       local_desc.value_size);
      end;
      free_desc (var_desc);
      var_desc := local_desc;
    end;
  end;
end (* check_desc *);
$PAGE read_write_call
public procedure read_write_call (input_node : tuple) options assembly;

var
  local_flags : packed record
    case boolean of
      true  : (word : uns_word;
               fill : 0..2**20-1);
      false : (bit : packed array [0..15] of boolean);
  end;
  local_string_addr : string_descriptor;
  local_item_addr : op_desc;
  local_width_addr : op_desc;
  local_prec_addr : op_desc;
  local_def_ptr : def;
  local_chkblk_op: op_desc;
  local_check_input_flag : boolean;

begin
  local_flags.word := 0;
  with input_node^ do begin
    if rw_mode = binaryrw then begin

      (* binary read/write *)

      (* fetch all parameters *)

      local_item_addr := fetch (rw_item, nonstack_modes,
                                immediate_mode, any_size, true);
      local_width_addr := fetch (rw_width, nonstack_modes,
				 immediate_mode, any_size, true);

      (* process all parameters *)

      push (local_width_addr, size_long);
      check_desc (local_item_addr, rw_item^.desc);
      pusha (local_item_addr);
      push (duplicate_desc (file_arg_addr), size_long);

      (* generate the runtime call *)

      if opcode = read_op then
        gen_rt (rt_read_binary)
      else
	gen_rt (rt_write_binary);
    end
    else begin

      (* text or typed read/write *)

      (* fetch all parameters *)

      if rw_item^.desc.kind = strings then begin
        if opcode = read_op then
          local_string_addr := fetchstring (rw_item, max_length, no_limit)
        else
          local_string_addr := fetchtranslated (rw_item, actual_length);
      end
      else begin
        if (rw_item^.desc.kind = reals) andif
           (rw_item^.desc.precision > srealprec) then
          local_item_addr := dfetch (rw_item, descriptors[null_mode], false)
        else
	  local_item_addr := fetch (rw_item, nonstack_modes,
				    immediate_mode, any_size, true);
      end;
      if rw_width <> nil then
        local_width_addr := fetch (rw_width, nonstack_modes,
				   immediate_mode, any_size, true);
      if rw_precision <> nil then begin
        local_prec_addr := fetch (rw_precision, nonstack_modes,
				  immediate_mode, any_size, true);
	if rw_mode = realrw then begin

	  (* TEMP FIX --- take precision from item, not RW_PRECISION *)

	  free_desc ( local_prec_addr );
	  local_prec_addr := int_desc ( rw_item^.desc.precision, size_long, false );
	end;
      end;

      (* process subrange limits, if any *)

      local_check_input_flag := false;
      if (chk_inp_opt in cur_block^.semantic_options) and
	 (opcode = read_op) and
	 (rw_mode in [decimalrw, octalrw, hexrw,
		      realrw, fixedrw, floatrw]) then
        local_check_input_flag := true;

      if local_check_input_flag then begin
        with rw_item^.desc do begin
          if kind = ints then begin
            if (base^.minval = minimum (longword)) and
               (base^.maxval = maximum (longword)) then
              local_check_input_flag := false;
          end
          else begin
	    if (base^.minval = type_real^.rminval) and
	       (base^.maxval = type_real^.rmaxval) then
	      local_check_input_flag := false;
          end;
        end (* with rw_item^.desc *);
      end;

      if local_check_input_flag then begin
	local_def_ptr := def_create (const_def);
	gen_def (cst_area, local_def_ptr);
        with rw_item^.desc do begin
          if kind = ints then begin
            gen_long (cst_area, (base^.minval, absolute_sc), false);
            gen_long (cst_area, (base^.maxval, absolute_sc), false);
          end
          else begin
            if rw_item^.desc.precision > srealprec then begin
	      gen_dreal (cst_area, base^.minval);
	      gen_dreal (cst_area, base^.maxval);
	    end
	    else begin
	      gen_sreal (cst_area, base^.minval);
	      gen_sreal (cst_area, base^.maxval);
            end;
          end;
        end (* with rw_item^.desc *);
	local_chkblk_op := descriptors [pc_displacement_mode];
	local_chkblk_op.cst_part := dloc (local_def_ptr);
	pusha (local_chkblk_op)
      end;

      (* process field width, if any *)

      if rw_width <> nil then
        push (local_width_addr, size_long);

      (* process string length word, if any *)

      if rw_item^.desc.kind = strings then
        pushlength (local_string_addr)
      else if rw_item^.desc.kind = chars then	(* fake length word *)
	pushi (1, size_word);

      (* process precision, for reals only *)

      if rw_precision <> nil then
        push (local_prec_addr, size_long)
      else if (rw_item^.desc.kind = reals) andif (rw_mode = floatrw) then
	pushi (0, size_long);	(* Supply dummy precision to be ignored *)

      (* process option word *)

      case rw_item^.desc.kind of

        ints :
          begin
            local_flags.bit[9] := (rw_mode = hexrw);
            local_flags.bit[10] := (rw_mode = octalrw);
          end;

        reals:
          begin
            if rw_item^.desc.precision > srealprec then
              local_flags.bit[14] := true
            else
              local_flags.bit[15] := true;
            if rw_mode = fixedrw then
              local_flags.bit[7] := true;
            if rw_mode = floatrw then begin
              local_flags.bit[8] := true;
              if rw_precision <> nil then
                local_flags.bit[7] := true;
            end;
          end;

        strings :
          begin
            if local_string_addr.type_desc.str_kind = varying then begin
	      local_flags.bit[14] := true;
	      local_flags.bit[15] := true;
            end
            else
              local_flags.bit[13] := true;
          end;

        chars :
          begin
            local_flags.bit[13] := true;
          end;

        bools :
          begin
            local_flags.bit[13] := true;
            local_flags.bit[15] := true;
          end;

      end (* case *);
      local_flags.bit[12] := (rw_mode = leftrw);
      local_flags.bit[11] := (rw_width <> nil);
      local_flags.bit[6] := local_check_input_flag;
      pushi (local_flags.word, size_word);

      (* process item address *)

      with rw_item^.desc do begin
        if kind = strings then begin
          if (opcode = read_op) and
             (local_string_addr.type_desc.str_kind = varying) then
            pushstring (local_string_addr, varying)
          else
            pushstring (local_string_addr, nonvarying);
          free_string (local_string_addr);
        end
        else begin
          check_desc (local_item_addr, rw_item^.desc);
          pusha (local_item_addr);
        end;
      end (* with rw_item^.desc *);

      if (rw_item^.desc.kind = reals) andif
         (rw_item^.desc.precision > srealprec) then
        dfpop;

      if getputstring_flag then begin
	pusha ( get_put_block );
	gen_rt ( rt_routine );
      end
      else begin
	push (duplicate_desc (file_arg_addr), size_long);
	if opcode = read_op then
	  gen_rt (rt_read_text)
	else
	  gen_rt (rt_write_text);
      end;
    end;
    free_file ( rw_file );
  end (* with input_node^ *);
end (* read_write_call *).
c  yì