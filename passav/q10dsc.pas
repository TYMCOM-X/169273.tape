$TITLE Q10DSC - quick pass code generator string and io routines.

module q10dsc;

$PAGE includes and forward declarations
$SYSTEM pascal.inc
$INCLUDE ptmcon.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM p10cg.typ
$SYSTEM q10dsc.typ
$SYSTEM p10opc.inc
$SYSTEM p10cgu.inc
$SYSTEM q10exp.inc
$SYSTEM q10gen.inc
$SYSTEM q10set.inc
$system q10cll.inc
  

public function fetchstring ( exp: expr; length_context: str_len_context ): str_desc
  options special(coercions); forward;

const
  bits_per_char = 7;
$PAGE runtime operator tables

type
    mvop_table = array [ boolean, (* padded/unpadded move *)
                         f_format..r_format, (* F, X, or R destination *)
                         c_format..x_format, (* C, F, or X source *)
                         str_translation ] (* direct, uppercase, lowercase *)
                    of rt_symbol;

const cs_move: mvop_table =
  ( ( ( ( rt_cm_fc,     rt_cmu_fc,      rt_cml_fc ),
        ( rt_cm_ff,     rt_cmu_ff,      rt_cml_ff ),
        ( rt_cm_fx,     rt_cmu_fx,      rt_cml_fx ) ),
      ( ( rt_cm_xc,     rt_cmu_xc,      rt_cml_xc ),
        ( rt_cm_xf,     rt_cmu_xf,      rt_cml_xf ),
        ( rt_cm_xx,     rt_cmu_xx,      rt_cml_xx ) ),
      ( ( rt_cm_rc,     rt_cmu_rc,      rt_cml_rc ),
        ( rt_cm_rf,     rt_cmu_rf,      rt_cml_rf ),
        ( rt_cm_rx,     rt_cmu_rx,      rt_cml_rx ) ) ),

    ( ( ( rt_cmp_fc,    rt_cmpu_fc,     rt_cmpl_fc ),
        ( rt_cmp_ff,    rt_cmpu_ff,     rt_cmpl_ff ),
        ( rt_cmp_fx,    rt_cmpu_fx,     rt_cmpl_fx ) ),
      ( ( rt_cmp_xc,    rt_cmpu_xc,     rt_cmpl_xc ),
        ( rt_cmp_xf,    rt_cmpu_xf,     rt_cmpl_xf ),
        ( rt_cmp_xx,    rt_cmpu_xx,     rt_cmpl_xx ) ),
      ( ( rt_cmp_rc,    rt_cmpu_rc,     rt_cmpl_rc ),
        ( rt_cmp_rf,    rt_cmpu_rf,     rt_cmpl_rf ),
        ( rt_cmp_rx,    rt_cmpu_rx,     rt_cmpl_rx ) ) ) );

type binop_table =              (* op <- table [leftop_fmt, rightop_fmt] *)
        array [c_format..x_format, c_format..x_format] of rt_symbol;

const cs_compare: binop_table :=
     (  rt_stop,        rt_cp_cf,       rt_cp_cx,       (* cc compare is scalar op *)
        rt_cp_fc,       rt_cp_ff,       rt_cp_fx,
        rt_cp_xc,       rt_cp_xf,       rt_cp_xx   );

const cs_index: binop_table :=
     (  rt_cix_cc,      rt_cix_cf,      rt_cix_cx,
        rt_cix_fc,      rt_cix_ff,      rt_cix_fx,
        rt_cix_xc,      rt_cix_xf,      rt_cix_xx   );
$PAGE arg_addr
(* ARG ADDR is passed an address descriptor which is to be used in a
   runtime argument which will be referenced with an immediate instruction.
   Immediate references are made absolute, register references made
   0(reg), and all others made indirect. This should NOT be used for
   all arguments, only those for which the runtime expects this format. *)

public function arg_addr (addr: addr_desc): addr_desc;

begin
  if addr.immediate then begin
    arg_addr := addr;
    arg_addr.immediate := false;
  end
  else if is_register (addr) then begin
    arg_addr := absolute_reference;
    arg_addr.index := addr.offset;
  end
  else begin
    assert (not addr.indirect);
    arg_addr := addr;
    arg_addr.indirect := true;
  end;
end (* arg_addr *);
$PAGE free_string

(* FREE_STRING frees all of the address descriptors contained within
   a string descriptor (parameter DESC).  *)

public procedure free_string ( desc: str_desc );

begin
  with desc do begin
    free ( base_addr );
    if len_context <> no_length then free ( len_addr );
    if text_valid then free ( text_addr );
  end;
end  (* proc free_string *) ;
$PAGE is_str_temp

(* IS_STR_TEMP returns true if parameter DESC describes a string
   temporary and returns false otherwise.
   Note well:
        This routine only works if the RELOC.KIND field of the base
        address has the value temp_sc.  In a concatenation, moves after
        the first one are to the 'interior' of the temp and the
        relocation kind is absolute_sc.  Thus this routine should not
        be called from CONCAT_DESC or any routine which it calls,
        directly or indirectly.  *)

function is_str_temp ( desc: str_desc ): boolean;

begin
  is_str_temp := desc.base_addr.reloc.kind = temp_sc;
end  (* proc is_str_temp *) ;
$PAGE init_str_desc

(* INIT_STR_DESC initializes some fields of a string
   descriptor record (parameter DESC).  Only those fields for
   which reasonable defaults can be supplied are initialized.  *)

procedure init_str_desc ( var desc: str_desc );

begin
  with desc do begin
    base_is_bound := false;
    len_context := actual_length;
    text_valid := false;
    trans_code := no_trans;
  end  (* with *) ;
end  (* proc init_str_desc *) ;
$PAGE alc_dynamic_temp
(* ALC DYNAMIC TEMP allocates a dynamic temporary on the stack, given
   its "size" as an addr_desc and an optional constant offset, plus
   the bit width of each element (e. g., 7 for chars, 1 for sets) *)

public function alc_dynamic_temp (
        len: addr_desc;
        const_len: integer;
        bits_per_element: bit_range
                ): addr_desc;

var
  len_reg, loc_reg: registers;

begin
  len_reg := get_reg (72);
  if is_register (len) then
    gen (movei, len_reg, len.offset, const_len + (36 div bits_per_element) - 1, none)
  else begin
    gen_rm (move, len_reg, len);
    gen_ri (addi, len_reg, const_len + (36 div bits_per_element) - 1)
  end;
  gen_ri (idivi, len_reg, 36 div bits_per_element);
  free_and_disassociate (len_reg + 1);
  loc_reg := get_reg (36);
  gen (movei, loc_reg, sb, 1, none);
    if prog_options.ki_code_opt then
      begin gen_rt(jsr,0,rt_inst_sml);
	gen (adj_sp, sb, len_reg, 0, none) 
      end
    else gen (adjsp, sb, len_reg, 0, none);
  decr_reg_usages (len_reg);
  alc_dynamic_temp := temp_reference;
  alc_dynamic_temp.index := loc_reg;
  reset_needed := true;
end;
$PAGE alloc_str_temp

(* ALLOC_STR_TEMP allocates a string temporary of length LEN.
   A string descriptor describing the temp is returned.  *)

(* This routine does not yet generate dynamic temporaries. *)

function alloc_str_temp ( len: char_range; is_varying: boolean ): str_desc;

begin
  init_str_desc ( alloc_str_temp );
  with alloc_str_temp do begin
    if is_varying
      then base_addr := get_temp ((len+4) div 5 + 1)
      else base_addr := get_temp ((len+4) div 5);
    max_len := len;
  end;
end  (* proc alloc_str_temp *) ;
$PAGE skip_desc_word

(* SKIP_DESC_WORD takes a string descriptor and returns an address
   descriptor for the location following any bound's word preceeding
   the string (i.e., the start of the text for nonvarying strings and
   the length word address for varying strings).  The TEXT_ADDR field
   of the string descriptor may be set by this routine.  *)

public function skip_desc_word ( var desc: str_desc ): addr_desc;

var
  is_varying: boolean;

begin
  with desc do begin
    is_varying := (type_desc.kind = strings) andif (type_desc.str_kind = varying);

    if text_valid then begin    (* if address of text present, then *)
      skip_desc_word := duplicate_addr ( text_addr );   (* back up 2 bytes *)
      if is_varying
        then skip_desc_word := increment_addr ( skip_desc_word, -1, 0, 36);
    end

    else begin  (* text addr invalid, use base address *)
      skip_desc_word := duplicate_addr ( base_addr );

      if base_is_bound then begin       (* if flex, increment addr past bounds word *)
        skip_desc_word := increment_addr ( skip_desc_word, 1, 0, 36);

        text_valid := true;     (* set TEXT_ADDR field, since we can calcualte *)
        text_addr := duplicate_addr ( skip_desc_word ); (* address w/o generating code *)
        if is_varying
          then text_addr := increment_addr (text_addr, 1, 0, 36);
        
        (* If the string descriptor describes the actual length of a
           varying string, then update the LEN_ADDR field also.  The new
           address should describe the same location as the old one, but
           may be in a more efficient, shorter format.   *)

        if (len_context = actual_length) and (type_desc.str_kind = varying) then begin
          free ( len_addr );                    (* free old length address *)
          len_addr := duplicate_addr ( skip_desc_word );        (* set it to new, hopefully better, form *)
        end;
      end;
    end;

  end  (* with *) ;
end  (* proc skip_desc_word *) ;
$PAGE skip_len_words

(* SKIP_LEN_WORDS takes a string descriptor and returns an address
   descriptor for the byte beginning the text of the string.  Note 
   that parameter DESC must be a VAR parameter because the TEXT_VALID
   and TEXT_ADDR fields of the string descriptor may be updated.  *)

public function skip_len_words ( var desc: str_desc ): addr_desc;

var
  increment: 0..4;

begin
  with desc do begin
  
    (* If the TEXT_ADDR field is already valid then simply duplicate
       and return it.  *)

    if text_valid then begin
      skip_len_words := duplicate_addr ( text_addr );
    end

    (* The TEXT_ADDR field is not valid.  We must increment the base
       address by the length of any bound or length words.  The
       TEXT_ADDR field will then be set.  *)

    else begin
      if base_is_bound  (* flex string ? *)
        then increment := 1
        else increment := 0;

      if (type_desc.kind = strings) andif       (* varying string? *)
         (type_desc.str_kind = varying)
        then increment := increment + 1;

      skip_len_words := duplicate_addr ( base_addr );
      if increment > 0 then
        skip_len_words := increment_addr ( skip_len_words, increment, 0, 36 );

      text_valid := true;
      text_addr := duplicate_addr ( skip_len_words );

      (* If the string descriptor describes the actual length of a varying
         string, then we change the length address in the descriptor.  The
         new address should describe the same location as the old one, but
         may be in a more efficient, shorter format.  *)

      if (len_context = actual_length) and (type_desc.kind = strings) andif
         (type_desc.str_kind = varying) then begin
        free ( len_addr );
        len_addr := duplicate_addr ( skip_len_words );
        len_addr := increment_addr (len_addr, -1, 0, 36);
      end;
    end  (* else *) ;

  end  (* with *) ;
end  (* proc skip_len_words *) ;
$PAGE prep_string
(* PREP STRING accepts a string descriptor and assures that the length
   and address fields are addressible as require in the runtime argument
   descriptors.  Specifically,

   (1) the length must not be indirect, and
   (2) the "text_addr" must be valid for "f" format descriptors.

   The argument descriptor format is also determined and returned. *)

procedure prep_string (var str: str_desc; var format: str_desc_format);

var
  taddr: addr_desc;

begin
  with str do begin
    if (base_addr.mode = byte) orif text_valid andif (text_addr.mode = byte) then
      format := x_format
    else if type_desc.kind = chars
      then format := c_format
      else format := f_format;
    if (format = f_format) and not text_valid then begin
      taddr := skip_len_words (str);
      free (taddr);
    end;
    if len_addr.indirect then
      len_addr := reg_addr (load_addr (len_addr, unsigned_value, 36));
  end;
end (* prep_string *);
$PAGE gen_str_desc, gen_str_coded
(* GEN STR DESC emits a runtime argument descriptor for a string which
   must have been prepared by PREP_STRING. GEN_STR_CODED is identical but
   allows specification of a datum register for the descriptor. *)

procedure gen_str_coded (datum: registers; str: str_desc; format: str_desc_format);

begin
  with str do begin
    case format of
      r_format:
        (* no descriptor needed *);
      c_format:
        gen_rm (arg, datum, base_addr);
      x_format, f_format: begin
        if format = x_format then begin
          if text_valid
            then gen_rm (arg, datum, text_addr)
            else gen_rm (arg, datum, base_addr)
        end
        else begin
          assert (text_valid);
          gen_rm (arg, datum, text_addr);
        end;
        gen_rm (arg, 0, arg_addr (str.len_addr));
      end (* x and f formats *)
    end (* case on format *)
  end (* with *)
end (* gen_str_coded *);

procedure gen_str_desc (str: str_desc; format: str_desc_format);

begin
  gen_str_coded (0, str, format);
end;
$PAGE move_string

(* MOVE_STRING moves a string specified by one string descriptor 
   (parameter RHS_DESC) to a location specified by a second string
   descriptor (parameter LHS_DESC).  Parameter PADDED is a boolean
   which indicates whether or not the result string should be padded
   with blanks if the lhs string is larger than the rhs string.
   Parameter FREE_PROC is a procedure parameter which will be called
   immediately before the string instruction is emitted.  It is 
   generally used to permit the caller to free all or some of
   the address descriptors in RHS_DESC and/or LHS_DESC.
   Parameter REMAINDER is a boolean which indicates whether or not
   the target string is the remainder of a previous string move (hence
   an "r_format" descriptor will be issued). *)

procedure move_string (
        var lhs_desc: str_desc; (* target string *)
        var rhs_desc: str_desc; (* source string *)
        padded: boolean;        (* if target to be padded *)
        free_proc: free_procedure; (* to be called sometime *)
        remainder: boolean;     (* if target is remainder of previous move *)
        suppress_null_move: boolean);   (* if true, ok to ignore null varying string move *)
                                (* Note: first move of concatenation must be made *)

var
  dest_format, source_format: str_desc_format;
  rts: rt_symbol;
  rhs_len: char_range;

begin

  (* Check for move of a null string to a varying string and suppress it
     if indicated. *)

  if suppress_null_move andif ((lhs_desc.type_desc.str_kind = varying) orif not padded)
    andif (rhs_desc.type_desc.kind = strings)
    andif (aconstp (rhs_desc.len_addr, rhs_len)) andif (rhs_len = 0) then begin
      free_proc;
  end
  else begin

    (* Determine the runtime argument descriptors which will be generated. *)

    prep_string (rhs_desc, source_format);
    if remainder
      then dest_format := r_format
      else prep_string (lhs_desc, dest_format);

    (* Performed requrested frees. *)

    free_proc;

    (* Choose a runtime entry point. *)

    rts := cs_move[padded,dest_format,source_format,rhs_desc.trans_code];

    (* Perform the move through the runtime. *)

    gen_rt (pushj, 17b, rts);

    (* Emit the argument descriptor(s). *)

    gen_str_desc (lhs_desc, dest_format);
    gen_str_desc (rhs_desc, source_format);

  end (* non-trivial move *);

end  (* proc move_string *) ;
$PAGE concat_desc

(* CONCAT_DESC generates code for a CAT_OP expression tuple.  Parameter
   EXP is the CAT_OP expression tuple.  Parameter CASE_CODE indicates
   any upper or lower case conversions which should be done during the
   concatenation move.  Concatenations are always done by moving the
   operand strings to a temporary.  A string descriptor for the resulting
   temporary is returned as the function return value.  *)

function concat_desc ( exp: expr; case_code: str_translation ): str_desc;

type
  str_desc_array = array [1..*] of str_desc;

var
  op_descs: ^str_desc_array;
  temp_length: char_range;
  i: oper_range;
  result_base: addr_desc;
  len_loaded: boolean;
  const_part: char_range;
  reg: registers;
  total_len_addr: addr_desc;
  new_base_addr: addr_desc;
  const_addr: addr_desc;
  op: str_desc;
  use_dynamic_temp: boolean;


  (* The following procedure is passed as a parameter to MOVE_STRING
     to free the desired strings before the runtime call. *)

  procedure concat_free;
    begin
      free_string ( op_descs^[ i ] );   (* free rhs string *)
    end;

  procedure check_length (laddr: addr_desc);

  var
    const_len: char_range;

  begin
    if aconstp (laddr, const_len) then
      const_part := const_part + const_len      (* accumulate *)
    else begin
      if len_loaded then        (* accumulate at runtime *)
        gen_rm (add, total_len_addr.offset, laddr)
      else begin
        total_len_addr := reg_addr (get_reg (36));
        gen_rm (move, total_len_addr.offset, laddr);
        len_loaded := true;
      end;
    end;
  end;

begin
  with exp^ do begin

    (* We first fetch each operand, so we can determine an upperbound
       on the size of the result.  We then allocate the temp and
       initialize the string descriptor for the result of the
       concatenation.  *)

    new ( op_descs, upperbound (operand) );
    temp_length := 0;
    use_dynamic_temp := false;
    for i := 1 to upperbound (operand) do begin
      op := fetchstring ( operand[ i ], actual_length );
      if case_code <> no_trans then op.trans_code := case_code;
      op_descs^[i] := op;
      if op.type_desc.str_flex
        then use_dynamic_temp := true
        else temp_length := temp_length + op.max_len;
    end;

    (* Now scan the operands, accumulating the total length. *)

    len_loaded := false;
    const_part := 0;

    for i := 1 to upperbound (operand) do
      check_length (op_descs^[i].len_addr);

    (* If any operand is flex we must allocate a dynamic temporary. *)

    if use_dynamic_temp then begin
      init_str_desc (concat_desc);
      with concat_desc do begin
        base_addr := alc_dynamic_temp (total_len_addr, const_part, bits_per_char);
        if const_part <> 0 then begin
          gen_ri (addi, total_len_addr.offset, const_part);
          const_part := 0;
        end;
        len_addr := duplicate_addr (total_len_addr);
      end;
    end
    else begin  (* not dynamic temp *)
      concat_desc := alloc_str_temp (temp_length, true);
      with concat_desc do begin
        len_addr := int_value (temp_length);   end;
    end;
    with concat_desc do begin
      result_base := duplicate_addr (base_addr);
      type_desc := desc;
    end;

    for i := 1 to upperbound (operand) do begin

      (* move the operand string *)

      move_string (concat_desc, op_descs^[i], false, concat_free, i > 1, i > 1);
    end;

    (* Generate code to add the consant part of the total length into
       the non-constant portion of the total, if necessary. Set the
       length field of the result string descriptor and reset the base
       address of the result. *)

    if len_loaded then begin
      if const_part <> 0 then
        gen_ri (addi, total_len_addr.offset, const_part)
    end
    else total_len_addr := int_value (const_part);

    free_string (concat_desc);
    init_str_desc (concat_desc);

    with concat_desc do begin
      len_addr := total_len_addr;
      base_addr := result_base; (* restore base addr of temp *)
    end;
    dispose ( op_descs );

  end  (* with *) ;
end  (* proc concat_desc *) ;
$PAGE substr_desc

(* SUBSTR_DESC generates code for SUBSTR_REF expression tuples.
   Parameter EXP is the SUBSTR_REF tuple.  Parameter CASE_CODE indicates
   any upper or lower case conversions inherited from higher nodes in the
   expression tree.  A string descriptor describing the resulting substring
   is returned.  *)

function substr_desc (exp: expr; case_code: str_translation): str_desc;

var
  base_desc: str_desc;
  index_reg, reg: registers;
  length_value, index_value: char_range;
  index_const: boolean;
  index_addr, taddr: addr_desc;

begin
  init_str_desc (substr_desc);      (* initialize the result str_desc *)

  (* If a substr check is involved, do it now. *)

  if exp^.substr_index^.ref_fre <> 0 then  (* marked as operand of substr check *)
    do_check (exp^.substr_index);

  with substr_desc do begin
    base_desc := fetchstring (exp^.base_string, actual_length);  (* fetch str_desc for base string *)
    len_addr := fetch_fullword (exp^.substr_length);        (* set susbstr length *)
    if aconstp (len_addr, length_value)
      then max_len := length_value
      else max_len := base_desc.max_len;

    if not base_desc.text_valid then begin
      taddr := skip_len_words (base_desc);
      free (taddr);
    end;

    index_addr := fetch (exp^.substr_index);
    index_const := iconstp (exp^.substr_index, index_value);
    if index_const then
      free (index_addr); (* won't need it - value is used directly *)
  
    if base_desc.text_addr.mode <> byte then begin
      if not cst_addr (base_desc.text_addr) then begin
	if index_const then begin
	  free (base_desc.text_addr);
	  reg := get_reg (36);
	  gen_rm (movei, reg, base_desc.text_addr);
	  if index_value <= 6 then
	    gen_ri (hrli, reg, 440700b - (index_value - 1) * 70000b)
	  else begin
	    taddr := absolute_reference;
	    taddr.reloc := gen_blt (440700b - ((index_value - 1) mod 5) * 70000b, none,
		      (index_value - 1) div 5, none);
	    gen_rm (add, reg, taddr);
	  end;
	  text_addr := reg_addr (reg);
	end
	else begin
	  index_reg := load_addr (index_addr, alignment (exp^.substr_index), 36);
	  gen_ri (subi, index_reg, 1);
	  free (base_desc.text_addr);
	  reg := get_reg (36);
	  gen_rm (movei, reg, base_desc.text_addr);
	  gen_ri (hrli, reg, 440700b);
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
	    gen_rr (adj_bp, index_reg, reg) 
	  end
	else gen_rr (adjbp, index_reg, reg);
	  decr_reg_usages (reg);
	  text_addr := reg_addr (index_reg);
	end
      end (* if text addr indirect *)
      else begin      (* emit constant byte pointer *)
	if index_const then begin
	  if index_value mod 5 = 1
	    then taddr := increment_addr (base_desc.text_addr,
		       ((index_value - 1) div 5) - 1, 28, 7)
	    else taddr := increment_addr (base_desc.text_addr,
		       (index_value - 2) div 5, ((index_value - 2) mod 5) * 7, 7);
	  text_addr := absolute_reference;
	  text_addr.reloc := gen_bptr (taddr);
	end
	else begin
	  taddr := increment_addr (base_desc.text_addr, -1, 21, 7);
	  text_addr := absolute_reference;
	  text_addr.reloc := gen_bptr (taddr);
	  reg := load_addr (index_addr, alignment (exp^.substr_index), 36);
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
	    gen_rm (adj_bp, reg, text_addr) 
	  end
	else gen_rm (adjbp, reg, text_addr);
	  text_addr := reg_addr (reg);
	end;
	free (taddr);
      end;
      base_desc.text_valid := false;  (* freed in above cases *)
    end
    else begin        (* already have byte pointer *)
      if index_const then begin
	reg := get_reg (36);
	gen_ri (hrrei, reg, index_value - 1);
      end
      else begin
	reg := load_addr (index_addr, alignment (exp^.substr_index), 36);
	gen_ri (subi, reg, 1);
      end;
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
            gen_rm (adj_bp, reg, base_desc.text_addr) 
	  end
        else gen_rm (adjbp, reg, base_desc.text_addr);
      text_addr := reg_addr (reg);
    end;
    text_valid := true;
    text_addr.mode := byte;
    type_desc := exp^.desc;   (* set type info *)
    base_addr := duplicate_addr (text_addr);

    if case_code = no_trans   (* set case conversion code *)
      then trans_code := base_desc.trans_code
      else trans_code := case_code;

  end  (* with *) ;

  free_string (base_desc);  (* free base string *)
end  (* proc substr_desc *) ;
$PAGE get_max_length

(* GET_MAX_LENGTH is passed a string valued expression tuple and an
   address descriptor for the base of the string (if the string is
   a dynamic flex string, then STR_ADDR should point to the bounds
   word; for a non-dynamic-flex varying string, STR_ADDR should
   point to the length word).  STR_EXPR must be an entire string
   reference operator (e.g., not a substring, concatenation or
   case conversion tuple) - this routine does not word for arbitrary
   string valued expression tuples.  The return value will be an
   address descriptor for the maximum length of the string.  *)

function get_max_length ( str_expr: expr; str_addr: addr_desc ): addr_desc;

begin
  with str_expr^ do begin
    if desc.kind = chars        (* if type is char, length is one *)
      then get_max_length := int_value ( 1 )
    else if desc.str_flex       (* if flex string, *)
      then get_max_length := upper_bound ( str_expr, str_addr ) (* then get upper bound *)
    else get_max_length := int_value ( desc.str_length );
                                                (* else maximum is constant *)
  end  (* with *) ;
end  (* proc get_max_length *) ;
$PAGE get_actual_length

(* GET_ACTUAL_LENGTH is passed a string valued expression tuple and
   an address descriptor for the base of the string (if the string
   is a dynamic-flex string, then STR_ADDR should point to the bounds
   word; for a non-dynamic-flex varying string, STR_ADDR should point
   to the length word).  STR_EXPR must be an entire string reference
   operator (e.g., not a substring, concatenation or case conversion
   tuple) - this routine does not work for arbitrary string valued
   expression tuples.  The return value will be an address descriptor
   for the actual length of the string. *)

function get_actual_length ( str_expr: expr; str_addr: addr_desc ): addr_desc;

begin
  with str_expr^ do begin
    if desc.kind = chars        (* if char, length is one *)
      then get_actual_length := int_value ( 1 )
    else if desc.str_kind = varying then begin  (* if varying string, return length word address *)
      get_actual_length := duplicate_addr ( str_addr );
      if dynamic_flex ( str_expr )      (* if flex, skip bounds word *)
        then get_actual_length := increment_addr ( get_actual_length, 1, 0, 36);
    end
    else if desc.str_flex       (* if non-varying flex, then get bounds word *)
      then get_actual_length := upper_bound ( str_expr, str_addr )
    else get_actual_length := int_value ( desc.str_length );
                                                (* if non-flex fixed string, length is constant *)

  end  (* with *) ;
end  (* proc get_actual_length *) ;
$PAGE get_case_ops

(* GET_CASE_OPS is passed a string valued expression tuple (parameter
   EXP).  If the tuple opcode is LWC_OP, UPC_OP or STRCVT_OP, then
   parameter BASE_EXPR will be set to the first descendent tuple whose
   opcode is not one of those three;  otherwise BASE_EXPR will be set
   to the vale of EXP.  CASE_CODE is set to reflect the kind of the 
   first case conversion operator encountered; if none is encountered
   it will be set to the value NO_TRANS.  *)


procedure get_case_ops ( exp: expr;  var base_expr: expr;
                         var case_code: str_translation );

begin
  base_expr := exp;
  while base_expr^.opcode = strcvt_op do        (* skip string convert ops *)
    base_expr := base_expr^.operand[ 1 ];
  case_code := no_trans;

  if ( base_expr^.opcode = lwc_op ) orif        (* if case op is present, *)
     ( base_expr^.opcode = upc_op ) then begin  (* then set parameter CASE_CODE *)
    if base_expr^.opcode = lwc_op               (* to reflect which conversion *)
      then case_code := lower_trans
      else case_code := upper_trans;

    repeat      (* skip any subsequent case or type *)
      base_expr := base_expr^.operand[ 1 ]      (* conversion operators *)
    until (base_expr^.opcode <> strcvt_op) and
          (base_expr^.opcode <> lwc_op) and
          (base_expr^.opcode <> upc_op);
  end;

end  (* proc get_case_ops *) ;
$PAGE date_desc
(* DATE DESC emits a call to the fortran DATE routine, returning
   a string descriptor for the result. *)

function date_desc (exp: expr; case_code: str_translation): str_desc;

const
  one_arg: val := (scalar_cst, -1000000b);      (* [-1,,0] *)

begin
  date_desc := alloc_str_temp (exp^.desc.str_length, false);
  with date_desc do begin
    gen_rr (push, sb, sp);
    gen (push, sb, 0, 0, gen_cval (one_arg));
    gen_rm (movei, 1, base_addr);
    gen_rr (push, sb, 1);
    gen_rx (movei, sp, sb);
    gen_rt (pushj, sb, rt_date);
      if prog_options.ki_code_opt then
	begin gen_rt(jsr,0,rt_inst_sml);
	  gen_ri (adj_sp, sb, -2) 
	end
      else gen_ri (adjsp, sb, -2);
    gen_rr (pop, sb, sp);
    len_addr := int_value (exp^.desc.str_length);
    type_desc := exp^.desc;
    trans_code := case_code;
  end;
end;
$PAGE get_str_desc

(* GET_STR_DESC is given a string (or possibly character) valued
   expression tuple (parameter ORIGINAL_EXP) and returns a string
   descriptor for the value.  A parameter (CONTEXT) is also passed
   in which indicates whether an address descriptor for the length
   of the string is desired and, if so, whether the actual length
   or the maximum length is desired.  *)

function get_str_desc ( original_exp: expr; context: str_len_context ): str_desc;

var
  exp: expr;
  case_code: str_translation;
  val_record: val;
  char_value: 0..255;

begin

  (* we first process any LWC_OPs, UPC_OPs, or STRCVT_OPs.  CASE_CODE
     will be set to indicate any case conversions necessary.  *)

  get_case_ops ( original_exp, exp, case_code );

  (* Get the descriptor.  *)

  case exp^.opcode of

    cat_op:     (* concatenation operator *)
      begin
        get_str_desc := concat_desc ( exp, case_code ); 
        if context = no_length then free ( get_str_desc.len_addr );
      end;

    substr_ref: (* substring operator *)
      begin
        get_str_desc := substr_desc ( exp, case_code );
        if context = no_length then free ( get_str_desc.len_addr );
      end;

    date_op: begin
      get_str_desc := date_desc (exp, case_code);
      if context = no_length then free (get_str_desc.len_addr);
    end;

    others:     (* an entire string reference *)
      begin
        with get_str_desc do begin
          if exp^.desc.kind = chars then begin
            assert (exp^.usage_count = 1);
            base_addr := fetch_fullword (exp);
            exp^.usage_count := 1 (* both fetch and fetch_string will decrement it *)
          end
          else if exp^.opcode = func_call_op then begin
            get_str_desc := alloc_str_temp (exp^.desc.str_length, exp^.desc.str_kind = varying);
            pas_call (exp, addr_return_value, nil, get_str_desc.base_addr);
          end
          else if exp^.desc.kind = strings then
            base_addr := do_fetch ( exp ) (* avoiding fetch as fetch_string handles multy uses *)
          else assert (false); (* only kinds should be chars or strings *)

          base_is_bound := ( exp^.desc.kind <> chars ) andif
                           ( dynamic_flex ( exp ) );

          if aconstp ( base_addr, char_value )  (* BASE_ADDR may actually be *)
            then base_addr := gen_cst ( char_value );   (* an immediate character value. If so, *)
                                                (* we generate it in the constant area, so *)
                                                (* we can always legally use BASE_ADDR as *)
                                                (* an address access mode operand *)

          text_valid := false;
          type_desc := exp^.desc;
          trans_code := case_code;
          len_context := context;

          if context = actual_length
            then len_addr := get_actual_length ( exp, base_addr )
          else if context = max_length
            then len_addr := get_max_length ( exp, base_addr );

          (* If varying, dynamic flex string then we can set the TEXT_ADDR
             field without generating any additional code.  *)

          if (context = actual_length) and ( base_is_bound ) and
             (type_desc.str_kind = varying) then begin
            text_valid := true;
            text_addr := duplicate_addr ( len_addr );
            text_addr := increment_addr (text_addr, 1, 0, 36);
          end;

          with exp^.desc do begin
            if kind = chars
              then max_len := 1
            else max_len := str_length;
            assert ( max_len <= maximum ( char_range ) );
          end;

        end  (* with get_str_desc *) ;
      end  (* string reference case *)

  end  (* case *) ;
end  (* proc get_str_desc *) ;
$PAGE fetchstring

(* FETCHSTRING is the counterpart of FETCH for fetching string values.
   Parameter EXP is the string valued expression tuple being fetched.
   Parameter CONTEXT indicates the kind of string length which should
   be set in the string descriptor returned.  Most of the logic in this
   routine is concerned with handling expression nodes with multiple
   uses.  For multi-use string valued expression tuples to be evaluated
   correctly, fetchstring must be called for each use.  *)

public function fetchstring (* exp: expr; length_context: str_len_context ): str_desc *) ;  (* forward declared *)

var
  desc_ref: ^str_desc;

begin
  with exp^ do begin

    (* If the FRE field is non-zero this tuple is an operand of a
       check tuple, which must be evaluated now. *)
  
    if ref_fre <> 0 then
      do_check (exp);
  
    (* if the RESULT field of the expression tuple is not NIL, then
     the expression tuple has been previously fetched.  The RESULT
     field is then a pointer to the string descriptor which was
     previously fetched.  *)

    if result <> nil then begin
      desc_ref := ptr ( ord ( result ) );
      fetchstring := desc_ref^;
      assert ( ( length_context = no_length ) or ( length_context = fetchstring.len_context ) );
    end

    (* This expression tuple has not been previously fetched.  We call
       GET_STR_DESC to actually construct the string descriptor.  *)

    else fetchstring := get_str_desc ( exp, length_context );

    (* If this is the first of multiple uses of this expression tuple, then
       make a copy of the string descriptor on the heap and store a pointer
       to it in the RESULT field of the expression tuple.  Increase the usage
       counts on any registers used in the address descriptors contained in
       the string descriptor.  The register usae counts are increased to 
       reflect the number of remaining uses of the expression tuple.  *)

    if ( usage_count > 1 ) andif ( result = nil ) then begin
      new ( desc_ref );
      desc_ref^ := fetchstring;
      incr_reg_usages ( regs_used (fetchstring.base_addr), usage_count - 1 );
      if fetchstring.len_context <> no_length
        then incr_reg_usages ( regs_used (fetchstring.len_addr), usage_count - 1 );
      if fetchstring.text_valid
        then incr_reg_usages ( regs_used (fetchstring.text_addr), usage_count - 1 );
      result := ptr ( ord ( desc_ref ) );
    end;

    (* Decrement the number of remaining uses for this expression tuple.
       If the usage count drops to zero and we have a copy of the string
       descriptor on the heap, then dispose of the heap copy.  *)

    usage_count := usage_count - 1;
    assert ( usage_count <> maximum ( usage_range ) );
    if ( usage_count = 0 ) andif ( result <> nil ) 
      then dispose ( desc_ref );

  end  (* with *) ;
end  (* proc fetchstring *) ;
$PAGE do_translation

(* DO_TRANSLATION performs any upper or lower case conversions pending
   for a string.  Parameter DESC is a string descriptor for the 
   string.  The function return value is a string descriptor for the
   translated string.  The LEN_CONTEXT field of DESC must not be set
   to NO_LENGTH, since the length of the input string is required
   for the translation.  Parameter DESC is freed by this routine.
   The kind of the string, varying or nonvarying, is preserved when
   a translation occurs. *)

function do_translation ( var desc: str_desc;  force_to_temp: boolean ): str_desc;

var
  dest_desc: str_desc;
  dummy_addr: addr_desc;
  is_varying: boolean;
  len_word: addr_desc;
  temp_len_word: addr_desc;

  (* FREE_RHS is a procedure which is passed as an actual parameter
     to MOVE_STRING.  It frees the rhs string before the register save
     which preceeds the string instruction.  *)

  procedure free_rhs;
    begin
      free_string ( desc );
    end;

begin

  with desc do begin

    (* If the string requires upper or lower casing then allocate a temporary
       and move and translate the string to the temporary.  *)

    if (trans_code <> no_trans) or force_to_temp then begin
      assert ( len_context <> no_length );

      is_varying := ( type_desc.kind = strings ) andif
                    ( type_desc.str_kind = varying );
      if type_desc.str_flex then begin  (* use dynamic temp *)
        init_str_desc (dest_desc);
        if is_varying then begin
          dest_desc.base_addr := alc_dynamic_temp (len_addr, chars_per_unit, bits_per_char);
          dest_desc.len_addr := duplicate_addr (dest_desc.base_addr);
        end
        else begin
          dest_desc.base_addr := alc_dynamic_temp (len_addr, 0, bits_per_char);
          dest_desc.len_addr := duplicate_addr (len_addr);
        end
      end
      else begin
        dest_desc := alloc_str_temp (max_len, is_varying);
        dest_desc.len_addr := duplicate_addr (len_addr);
      end;

      dest_desc.len_context := len_context;     (* init the temp *)
      dest_desc.type_desc := type_desc;

      if is_varying then begin  (* if varying strings, then copy length word *)
        len_word := skip_desc_word ( desc );
        temp_len_word := skip_desc_word ( dest_desc );
        gen_rm (move, 1, len_word);
        gen_rm (movem, 1, temp_len_word);
        free ( len_word );
        free ( temp_len_word );
      end;

      move_string ( dest_desc, desc, false, free_rhs, false, true );

      do_translation := dest_desc;
    end

    else do_translation := desc;
 
  end  (* with *) ;
end  (* proc do_translation *) ;
$PAGE fetchtranslated

(* FETCHTRANSLATED fetches a string, given a string valued expression
   tuple.  It serves the same purpose as FETCHSTRING except that it
   guarantees that the resulting string has no pending case conversions.
   This routine calls FETCHSTRING; if the fetched string does require
   case conversion, then DO_TRANSLATION is called to translate the 
   string by copying it to a temporary.
   Parameter EXP is the expression tuple for the string 
   being fetched.  Parameter CONTEXT indicates the type of string
   length desired.  CONTEXT may not be no_length, since a length
   is required for the move.  The kind of the string, varying or
   nonvarying, is preserved when a translation occurs.  *)

public function fetchtranslated ( exp: expr;  context: str_len_context ): str_desc;


begin

  fetchtranslated := fetchstring ( exp, context );      (* fetch the untranslated string *)

  fetchtranslated := do_translation ( fetchtranslated, false );        (* translate if necessary *)
end  (* proc fetchtranslated *) ;
$PAGE do_index_op

(* DO_INDEX_OP generates code for intrinsic function INDEX.  Parameter
   INDEX_OP_EXPR is the INDEX_OP expression tuple.  An address
   descriptor is returned for the INDEX return value.  *)

public function do_index_op ( index_op_expr: expr ): addr_desc;

var
  source_desc, object_desc: str_desc;
  source_format, object_format: str_desc_format;
  regs_saved: set_of_registers;
  reg: registers;

begin
  with index_op_expr^ do begin

    (* Fetch the source and object strings and calculate the start of 
       the text of each. *)

    source_desc := fetchtranslated ( operand[ 1 ], actual_length );
    object_desc := fetchtranslated ( operand[ 2 ], actual_length );

    prep_string (source_desc, source_format);
    prep_string (object_desc, object_format);

    if upperbound (operand) = 3 then
      reg := copy_load (operand[3]);

    gen_rt (pushj, sb, cs_index [source_format, object_format]);
    gen_str_desc (source_desc, source_format);
    gen_str_desc (object_desc, object_format);

    free_string (object_desc);
    free_string (source_desc);

    (* Copy the result, if necessary. *)

    if upperbound (operand) = 2 then begin
      reg := get_reg (36);
      gen_rr (move, reg, 1);    (* simple copy *)
    end
    else begin  (* copy if nonzero *)
      gen_rr (skip+eqc, 0, 1);
      gen_rr (move, reg, 1);
    end;

    do_index_op := reg_addr (reg);
  end  (* with *) ;
end  (* proc do_index_addr *) ;
$PAGE str_search_verify

(* STR_SEARCH_VERIFY generates code for intrinsic functions SEARCH and
   VERIFY.  Calls to runtime routines are generated for either routine.
   Parameter EXP is the SEARCH_OP or VERIFY_OP expression tuple.  An 
   address descriptor for the function result is returned.  *)

public function str_search_verify ( exp: expr ): addr_desc;

type
  sv_rt_symbol_array = packed array [
        boolean,        (* true ==> verify *)
        boolean,        (* true ==> o-format set *)
        no_trans..upper_trans,  (* string translation pending *)
        c_format..x_format      (* string format *)
                ] of rt_symbol;

const
  sv_entry_point: sv_rt_symbol_array := (
    rt_sr_cl,   rt_sr_fl,       rt_sr_xl,       (* search operators *)
    rt_sru_cl,  rt_sru_fl,      rt_sru_xl,
    rt_sr_co,   rt_sr_fo,       rt_sr_xo,
    rt_sru_co,  rt_sru_fo,      rt_sru_xo,
    rt_vf_cl,   rt_vf_fl,       rt_vf_xl,       (* verify operators *)
    rt_vfu_cl,  rt_vfu_fl,      rt_vfu_xl,
    rt_vf_co,   rt_vf_fo,       rt_vf_xo,
    rt_vfu_co,  rt_vfu_fo,      rt_vfu_xo);

var
  string_desc: str_desc;
  string_format: str_desc_format;
  rts: rt_symbol;
  search_set: set_desc;

begin
  with exp^ do begin
    string_desc := fetchstring (operand[1], actual_length);
    if string_desc.trans_code = lower_trans then
      string_desc := do_translation (string_desc, false);
    search_set := set_fetch (operand[2], operand[2]^.desc.set_lwb,
        operand[2]^.desc.set_length);
    if search_set.nargs = 0 then begin  (* search with empty set ? *)
      if upperbound (operand) = 3 then
        str_search_verify := reg_addr (load (operand[3], expr_size (operand[3]^.desc)))
      else begin
        str_search_verify := reg_addr (get_reg (36));
        gen_ri (movei, str_search_verify.offset, 0);
      end;
      free_string (string_desc);
    end
    else begin
      prep_string (string_desc, string_format);
      force_out_of_reg (search_set);  (* if set is in regs, push it out to a temp *)
      if upperbound (operand) = 3 then
        str_search_verify := reg_addr (copy_load (operand[3]));
      rts := sv_entry_point [
                opcode = verify_op,
                search_set.nargs <> 3,  (* o-format *)
                string_desc.trans_code,
                string_format];
      gen_rt (pushj, sb, rts);
      gen_str_desc (string_desc, string_format);
      if search_set.nargs = 2 then
        o_format_arg (search_set.arg[1], search_set.arg[2])
      else
        l_format_arg (search_set.arg[1], search_set.arg[2], search_set.arg[3]);
      free_string (string_desc);
      set_free (search_set);
      if upperbound (operand) = 3 then begin
        gen_rr (skip+eqc, 0, 1);
        gen_rr (move, str_search_verify.offset, 1);
      end
      else begin
        str_search_verify := reg_addr (get_reg (36));
        gen_rr (move, str_search_verify.offset, 1);
      end;
    end;
  end (* with *);
end  (* proc str_search_verify *) ;
$PAGE str_compare

(* STR_COMPARE compares two strings.  Parameter EXP is the string
   comparison expression tuple.  This routine merely emits a string
   comparison instruction.  Execution of the instruction will set
   register 1 to the result of the comparison. It is the responsibility
   of the caller to utilize this result. *)

public procedure str_compare ( exp: expr );

var
  lhs, rhs: str_desc;
  lhs_format, rhs_format: str_desc_format;
  lhs_text_addr, rhs_text_addr: addr_desc;
  regs_saved: set_of_registers;

begin
  
  (* Fetch both strings.  *)

  lhs := fetchtranslated ( exp^.operand[ 1 ], actual_length );
  rhs := fetchtranslated ( exp^.operand[ 2 ], actual_length );

  lhs_text_addr := skip_len_words ( lhs );
  rhs_text_addr := skip_len_words ( rhs );

  prep_string (lhs, lhs_format);
  prep_string (rhs, rhs_format);

  (* Emit the runtime call. *)

  gen_rt (pushj, sb, cs_compare[lhs_format,rhs_format]);

  gen_str_desc (lhs, lhs_format);
  gen_str_desc (rhs, rhs_format);

  free_string ( lhs );
  free_string ( rhs );
  free ( lhs_text_addr );
  free ( rhs_text_addr );

end  (* proc str_compare *) ;
$PAGE update_length

(* UPDATE_LENGTH updates the length word of a varying string.  Parameter
   LHS_DESC is a string descriptor for the (varying) string whose length
   word is to be updated.  Parameter RHS_DESC is a string descriptor for
   the string whose length is to be used in the update.  Note that the
   LHS_DESC may have its TEXT_ADDR field set, and, thus LHS_DESC must
   be a VAR parameter.  *)

procedure update_length ( var lhs_desc: str_desc; rhs_desc: str_desc );

var
  length_word_addr: addr_desc;
  length_valid: boolean;
  length_addr: addr_desc;
  len_value: char_range;

begin
  with lhs_desc do begin

    (* Calculate the address of the target string's length word.  *)

    length_word_addr := skip_desc_word ( lhs_desc );


    (* In general we must update the length word with:
         MIN ( upperbound ( lhs ), length ( rhs ) ) 
       However, in 3 special cases we can determine which value to use at
       compile time.  The 3 special cases are:
         1. The RHS string is actually a char; we update with the constant
            value one.
         2. Neither string has the STR_FLEX flag set in its EXPR_TYPE_DESC,
            and, upperbound ( lhs ) >= upperbound ( rhs );  we update with
            length ( rhs ).
         3. Neither string has the STR_FLEX flag set in its EXPR_TYPE_DESC,
            the rhs string is nonvarying and length ( rhs ) >=
            upperbound ( lhs ); we update with upperbound ( lhs ).  *)

    length_valid := false;         (* assume no special cases apply *)
    if rhs_desc.type_desc.kind = chars then begin       (* Case 1: Rhs is a char *)
      length_addr := int_value ( 1 );
      length_valid := true;
    end

    else if not (type_desc.str_flex or  (* if neither flex flag set, then *)
                 rhs_desc.type_desc.str_flex) then begin        (* check for cases 2 and 3 *)
      
      if type_desc.str_length >= rhs_desc.type_desc.str_length then begin       (* Case 2 *)
        length_addr := duplicate_addr ( rhs_desc.len_addr );
        length_valid := true;
      end

      else if ( rhs_desc.type_desc.str_kind = nonvarying ) andif        (* Case 3 *)
              ( rhs_desc.type_desc.str_length >= type_desc.str_length ) then begin
        length_addr := int_value ( type_desc.str_length );
        length_valid := true;
      end;
    end;

    (* if any of the above special cases applied, the we simply move
       the length determined above to the lhs length word.  *)


    if length_valid andif aconstp (length_addr, len_value) andif (len_value = 0) then begin
      gen_rm (setzm, 0, length_word_addr)
    end
    else begin
      if length_valid then begin
        gen_rm (move, 1, length_addr);
        free ( length_addr );
      end

      (* None of the special cases applied - we must emit code to compute
         at run time:  MIN ( upperbound ( lhs ), length ( rhs ) )  *)

      else begin
        gen_rm (move, 1, rhs_desc.len_addr);
        gen_rm (cam+lec, 1, len_addr);
        gen_rm (move, 1, len_addr);
      end;

      gen_rm (movem, 1, length_word_addr);
    end (* non-zero length *);
    free ( length_word_addr );

  end  (* with *) ;
end  (* proc update_length *) ;
$PAGE str_assignment

(* STR_ASSIGNMENT performs a string assignment.  NODE is the ASSIGN_OP
   tuple for the assignment.  *)

public procedure str_assignment ( node: tuple );

var
  rhs_desc: str_desc;
  lhs_desc: str_desc;
  temp_desc: str_desc;
  padded: boolean;
  dummy_addr: addr_desc;
  const_len: char_range;


  (* The following two procs are passes as parameters to MOVE_STRING
     to do the desired frees. *)

  procedure free_rhs_only;
    begin
      free_string ( rhs_desc );
    end;

  procedure free_both_sides;
    begin
      free_string ( rhs_desc );
      free_string ( lhs_desc );
    end;

begin
  with node^ do begin
  
    (* Get string descriptors for both the left and right hand sides.  *)

    rhs_desc := fetchstring ( rhs, actual_length );
    lhs_desc := fetchstring ( lhs, max_length );

    (* If the lhs and rhs strings may overlap, the rhs string is
       first copied to a temp.  *)

    if overlaps andif           (* lhs and rhs strings may overlap, *)
       not is_str_temp ( rhs_desc ) then begin  (* rhs is not a temporary *)
      if not aconstp (rhs_desc.len_addr, const_len) then begin (* use dynamic temp *)
        init_str_desc (temp_desc);
        temp_desc.base_addr := alc_dynamic_temp (rhs_desc.len_addr, 0, bits_per_char);
      end
      else temp_desc := alloc_str_temp ( const_len, false );
      with temp_desc, type_desc do begin
        len_addr := duplicate_addr ( rhs_desc.len_addr );
        type_desc := rhs_desc.type_desc;
        if (kind = strings) andif (str_kind = varying) then begin
          str_kind := nonvarying;       (* since no length word precedes copied string *)
          str_flex := true;     (* but length is not known at compile time *)
        end
	else if kind = chars then begin
	  kind := strings;
	  str_kind := nonvarying;
	  str_flex := false;
	  str_length := 1
	end;
      end;
      move_string ( temp_desc, rhs_desc, false, free_rhs_only, false, true );
      rhs_desc := temp_desc;
    end;

    (* if lhs string is varying, update length word.  *)

    if lhs_desc.type_desc.str_kind = varying    (* note that lhs cannot be a char *)
      then update_length ( lhs_desc, rhs_desc );
    
    (* finally move the rhs string to the lhs string.  *)

    padded := lhs_desc.type_desc.str_kind = nonvarying;
    move_string ( lhs_desc, rhs_desc, padded, free_both_sides, false, true );

  end  (* with *) ;
end  (* proc str_assignment *) ;
$PAGE var_parameter

(* VAR_PARAMETER generates the argument word(s) for a VAR string
   parameter.  Parameter ACTUAL_EXPR is the expression tuple for the
   actual parameter;  FORMAL_SYM is the symbol node for the formal
   parameter.  *)

procedure var_parameter (
        actual_expr: expr;      (* parameter expression *)
        forml_type: typ;        (* parameter as declared *)
        reg: registers);        (* target register for parameter *)

var
  actual_desc: str_desc;
  param_addr: addr_desc;
  r: registers;

begin

  (* Fetch the actual parameter.  *)

  actual_desc := fetchstring ( actual_expr, max_length );

  (* Get the address of the string, skipping any bound word *)

  param_addr := skip_desc_word (actual_desc);

  (* If the formal is flex then load the upperbound of the string. *)

  if forml_type^.flexible then begin

    (* Careful not to overwrite a necessary index register. *)

    r := regs_used (param_addr);
    if r = reg then begin       (* load address before length *)
      gen_rm (movei, reg+1, param_addr);
      gen_rm (move, reg, actual_desc.len_addr);
    end
    else begin
      gen_rm (move, reg, actual_desc.len_addr);
      gen_rm (movei, reg+1, param_addr);
    end;
  end
  else gen_rm (movei, reg, param_addr);
  
  free_string ( actual_desc );
  free (param_addr);

end  (* proc var_parameter *) ;
$PAGE value_parameter

(* VALUE_PARAMETER generates code for string parameters passed by value.
   It both copies the string to a temporary, if required, and generates
   the argument word(s) in the registers.  Parameter ACTUAL_EXPR is the
   expression tuple for the actual parameter;  FORMAL_SYM is the symbol
   node for the formal parameter.  REG is the target register.  *)

procedure value_parameter (
        actual_expr: expr;
        forml_type: typ;
        reg: registers);

var
  r: registers;
  taddr: addr_desc;
  uncoerced_expr: expr;
  actual_kind: string_kind;
  formal_kind: string_kind;
  actual_is_flex: boolean;
  formal_is_flex: boolean;
  actual_bound: char_range;
  formal_bound: char_range;
  no_copy: boolean;
  actual_desc: str_desc;
  bound_addr: addr_desc;
  temp_length: char_range;
  temp_desc: str_desc;
  length_word_addr: addr_desc;
  padded: boolean;
  dummy_addr: addr_desc;
  param_addr: addr_desc;

  (* FREE_ACTUAL is passed as a procedure parameter to MOVE_STRING.
     It frees the rhs string just before the move.  *)

  procedure free_actual;
    begin
      free_string ( actual_desc );
    end;

begin

  (* First, we determine some info we will need later:  (1) the kind
      (varying or nonvarying) of the formal and of the actual, (2)
     whether or not either is flex, and, (3) a compile time bound
     on the length of both the formal and actual.  *)

  if actual_expr^.opcode = strcvt_op    (* don't take type info for the actual *)
    then uncoerced_expr := actual_expr^.operand[ 1 ]    (* from a STRCVT_op!! *)
    else uncoerced_expr := actual_expr;

  if uncoerced_expr^.desc.kind = chars
    then actual_kind := nonvarying
    else actual_kind := uncoerced_expr^.desc.str_kind;
  assert ( forml_type^.kind = strings );
  formal_kind := forml_type^.str_kind;

  actual_is_flex := ( uncoerced_expr^.desc.kind = strings ) andif
                    ( uncoerced_expr^.desc.str_flex );
  formal_is_flex := forml_type^.flexible;

  if uncoerced_expr^.desc.kind = chars
    then actual_bound := 1
    else actual_bound := uncoerced_expr^.desc.str_length;
  formal_bound := forml_type^.str_length;
  actual_desc := fetchstring (actual_expr, max_length);

  (* Determine whether or not a copy to a temporary is
        required. A copy IS necessary if the actual is a character,
        but is NOT necessary under the following circumstances:
        1. no STRCVT_OP is present, or, 
        2. the formal parameter is a nonvarying flex, or,
        3. the kinds of the formal and actual are the same and one of
           the following holds:
           a. the formal is flex, or, 
           b. neither the formal nor the actual is flex and either
              ( i. ) both are nonvarying and length(actual) >= length(formal),
		     and we don't have a byte pointer, or,
              ( ii. ) both are varying and upperbound(actual) <= 
                      upperbound(formal).                               *)

  no_copy := ( uncoerced_expr^.desc.kind  <> chars ) andif
             (( actual_expr^.opcode <> strcvt_op ) or
             ( formal_is_flex and ( formal_kind = nonvarying ) ) or
             ( ( actual_kind = formal_kind ) and
               ( ( formal_is_flex ) or
                 ( not formal_is_flex and not actual_is_flex and
                   ( ( ( actual_kind = nonvarying ) and
                       ( formal_kind = nonvarying ) and
		       ( actual_desc.base_addr.mode <> byte ) and
                       ( actual_bound >= formal_bound ) ) or
                     ( ( actual_kind = varying ) and
                       ( formal_kind = varying ) and
                       ( actual_bound <= formal_bound ) ) ) ) ) ) );

  (* A bit of chicanery here - we may need both the length and the
     upperbound of the actual.  Since the length is easily found if the bound
     is known, we request the bound when we fetch the actual.  We
     then copy the bound and then set the descriptor's length to the
     actual length.  *)

  if no_copy 
    then actual_desc := do_translation ( actual_desc, false );
  bound_addr := actual_desc.len_addr;
  if actual_kind = nonvarying
    then actual_desc.len_addr := duplicate_addr ( actual_desc.len_addr )
    else actual_desc.len_addr := skip_desc_word ( actual_desc );
  actual_desc.len_context := actual_length;

  (* If a copy is necessary, then allocate a temp and copy the
     actual parameter to the temp.  *)

  if not no_copy then begin

    (* Allocate and initialize the temporary.  *)

    if formal_is_flex
      then temp_length := actual_desc.max_len
      else temp_length := formal_bound;
  
    if actual_is_flex and formal_is_flex then begin
      init_str_desc (temp_desc);
      temp_desc.base_addr := alc_dynamic_temp (actual_desc.len_addr,
        chars_per_unit * ord (formal_kind = varying), bits_per_char);
    end
    else temp_desc := alloc_str_temp ( temp_length, formal_kind = varying );
    with temp_desc.type_desc do begin
      base := nil;
      kind := strings;
      str_kind := formal_kind;
      str_flex := formal_is_flex;
      str_length := formal_bound;
    end;

    (* Set the LEN_ADDR field of the string descriptor for the temp.
       In addition, if the kind of the formal is varying, then first
       set the temporary's length word.  *)

    if formal_kind = varying then begin (* formal is varying *)
      if formal_is_flex then begin      (* update length word *)
        length_word_addr := temp_desc.base_addr;
        gen_rm (move, 1, actual_desc.len_addr);
        gen_rm (movem, 1, length_word_addr);
      end
      else begin
        temp_desc.len_addr := int_value ( formal_bound );
        update_length ( temp_desc, actual_desc );
      end;

      temp_desc.len_addr := duplicate_addr ( temp_desc.base_addr );     (* set LEN_ADDR field *)
    end

    else begin  (* formal is nonvarying *)
      if formal_is_flex
        then temp_desc.len_addr := duplicate_addr ( actual_desc.len_addr )
        else temp_desc.len_addr := int_value ( formal_bound );
    end;

    (* Copy the actual parameter to the temp.  Set ACTUAL_DESC to
       the temp's string descriptor.  *)

    padded := formal_kind = nonvarying;
    move_string ( temp_desc, actual_desc, padded, free_actual, false, true );
    actual_desc := temp_desc;
  end  (* if *) ;

  (* Load the parameters. *)

  if formal_kind = nonvarying
    then param_addr := skip_len_words ( actual_desc )
    else param_addr := skip_desc_word ( actual_desc );

  (* only one kind of formal accepts a byte pointer -  a nonvarying flex *)
  assert ( (param_addr.mode <> byte) or
	   ((formal_kind = nonvarying) and (formal_is_flex)) );
  if formal_is_flex then begin
    if formal_kind =  nonvarying
      then taddr := actual_desc.len_addr
      else taddr := bound_addr;
    r := regs_used (param_addr);
    if r = reg then begin
      if param_addr.mode = byte then
        gen_rm (move, reg+1, param_addr)
      else begin
        gen_rm (movei, reg+1, param_addr);
        if formal_kind = nonvarying then
          gen_ri (hrli, reg+1, 440700b);
      end;
      gen_rm (move, reg, taddr);
    end
    else begin
      gen_rm (move, reg, taddr);
      if param_addr.mode = byte then
        gen_rm (move, reg+1, param_addr)
      else begin
        gen_rm (movei, reg+1, param_addr);
        if formal_kind = nonvarying then
          gen_ri (hrli, reg+1, 440700b);
      end;
    end;
  end
  else begin
    if param_addr.mode = byte
      then gen_rm (move, reg, param_addr)
      else gen_rm (movei, reg, param_addr);
  end;

  (* Clean up the act.  *)

  free_string ( actual_desc );
  free ( bound_addr );
  free (param_addr);

end  (* proc value_parameter *) ;
$PAGE str_parameter

(* STR_PARAMETER generates code to push the argument word(s) for
   a string parameter onto the stack.  Any type coercions required
   for value parameters are done also.  Parameter ACTUAL_EXPR is the
   expression tuple for the actual parameter; FORMAL_SYM is the symbol
   node for the formal parameter.  *)

public procedure str_parameter ( actual_expr: expr; parm: param_desc; reg: registers );

begin
  
  if parm.parm_kind = vars
    then var_parameter ( actual_expr, parm.parm_type, reg )
    else value_parameter ( actual_expr, parm.parm_type, reg );

end  (* proc str_parameter *) ;
$PAGE io_begins, io_ends

static var
  original_target,
  str_file: str_desc;           (* string descriptor for get/put *)
  getputstring: boolean;        (* during sequence of get/put strings *)
  str_addr: addr_desc;  (* to temps *)
  str_bptr: boolean;            (* if str_addr is byte pointer to string *)

public procedure io_begins (node: tuple);

var
  reg: registers;
  str_len: addr_desc;
  temp_addr: addr_desc;
  temp_desc: str_desc;

begin
  with node^.file_arg^ do begin
    if opcode = in_str_op then begin
      str_file := fetchtranslated (operand[1], actual_length);
      if not is_str_temp (str_file) then
        str_file := do_translation (str_file, true); (* utilize trans. routine to make temp. copy *)
      getputstring := true;
      str_addr := get_temp (2);
      str_len := str_addr;
      str_len.offset := str_len.offset + 1;
      temp_addr := skip_len_words (str_file);
      if temp_addr.mode <> byte then begin
        gen_rm (movei, 1, temp_addr);
        gen_ri (hrli, 1, 440700b);
        gen_rm (movem, 1, str_addr)
      end
      else if is_register (temp_addr) then
        gen_rm (movem, temp_addr.offset, str_addr)
      else begin
        gen_rm (move, 1, temp_addr);
        gen_rm (movem, 1, str_addr);
      end;
      free (temp_addr);
      str_bptr := true;
      gen_rm (move, 1, str_file.len_addr);
      gen_rm (movem, 1, str_len);
      free_string (str_file);
    end
    else if opcode = out_str_op then with original_target do begin
      original_target := fetchstring (operand[1], max_length);
      getputstring := true;
      str_addr := get_temp (2); (* allocate 2-word cell *)
      str_len := str_addr;
      str_len.offset := str_len.offset + 1;
      gen_rm (move, 1, len_addr);
      gen_rm (movem, 1, str_len); (* fill countdown word *)
      if type_desc.str_flex then begin (* use dynamic temp *)
	init_str_desc (str_file);
	str_file.base_addr := alc_dynamic_temp (len_addr, chars_per_unit (* room for possible length *), bits_per_char);
      end
      else
	str_file := alloc_str_temp (max_len, true (* make room for possible length *) );
      if operand[1]^.desc.str_kind = varying then
	str_file.len_addr := duplicate_addr (str_file.base_addr)
      else
        str_file.len_addr := duplicate_addr (len_addr);
      str_file.len_context := len_context;
      str_file.type_desc := type_desc;
  
      if operand[1]^.desc.str_kind = varying then begin
        if text_valid andif (text_addr.mode = byte) then begin
          str_bptr := true;
          if not is_register (text_addr) then begin
            gen_rm (move, 1, text_addr);
            gen_rm (movem, 1, str_addr);
          end
          else gen_rm (movem, text_addr.offset, str_addr);
        end
        else begin
          temp_addr := skip_desc_word (str_file);
          gen_rm (movei, 1, temp_addr);
          gen_rx (setzm, 0, 1);
          gen_rm (movem, 1, str_addr);
          free (temp_addr);
          str_bptr := false;
        end;
      end
      else begin
        temp_addr := skip_desc_word (str_file);
        if temp_addr.mode = byte then begin
          if not is_register (temp_addr) then begin
            gen_rm (move, 1, temp_addr);
            gen_rm (movem, 1, str_addr);
          end
          else gen_rm (movem, temp_addr.offset, str_addr);
        end
        else begin
          gen_rm (movei, 1, temp_addr);
          gen_ri (hrli, 1, 440700b);
          gen_rm (movem, 1, str_addr);
        end;
        str_bptr := true;
        free (temp_addr);
      end;
    end
    else begin  (* must be to file *)
      reg := nond_load (node^.file_arg, 36);
      decr_reg_usages (reg);
      getputstring := false;
    end;
  end;
end (* io_begins *);

public procedure io_ends (node: tuple);

var
  addr: addr_desc;
  
  procedure free_both;
    begin
      free_string (str_file);
      free_string (original_target)
    end;

begin
  with node^.file_arg^ do begin
    if getputstring then begin
      if opcode = out_str_op then begin
        if operand[1]^.desc.str_kind = nonvarying then begin
          gen_rt (pushj, sb, rt_wr_dnn);
          gen_rm (arg, 0, str_addr);
        end;
        if original_target.type_desc.str_kind = varying then
	  update_length (original_target, str_file);
	move_string (original_target, str_file, false, free_both, false, true)
      end
    end
    else begin
      addr := fetch (node^.file_arg);
      free (addr);
    end;
  end;
end;
$PAGE rt_open_call
(* RT OPEN CALL generates code for an open, reset, rewrite or update op, and
   returns an address descriptor to the register in which it has been loaded. *)

public function rt_open_call ( node: expr ): addr_desc;

var
     file_str_desc: str_desc;
     str_format: str_desc_format;
     iname: 1..2;
     open_mode: file_modes;
     open_size: bit_range;
     temp_addr, options_addr: addr_desc;
     options_set: set_desc;
     bits, reg: registers;
     rts: rt_symbol;

 begin
  with node^ do begin

   if operand[1] <> nil
     then iname := 1
     else iname := 2;
   open_mode := desc.base^.file_kind;
   if desc.base^.packable
     then open_size := desc.base^.comp_size
     else open_size := (desc.base^.comp_size + byte_size - 1) div byte_size;

    (* Fetch the operands. *)

    file_str_desc := fetchstring (operand[iname], actual_length);
    options_set := set_fetch (operand[3], operand[3]^.desc.set_lwb, operand[3]^.desc.set_length);
    if options_set.nargs = 0 then begin
      options_addr := reg_addr (get_reg (36));
      gen_ri (movei, options_addr.offset, 0);
    end
    else begin
      make_set (options_set, 0, operand[3]^.desc.set_lwb + operand[3]^.desc.set_length, true);
      options_addr := duplicate_addr (options_set.arg[1]);
      set_free (options_set)
    end;

    (* Insure the string is byte pointer addressible *)

    prep_string (file_str_desc, str_format);
    temp_addr := skip_len_words (file_str_desc);
    free (temp_addr);
    if file_str_desc.text_addr.mode <> byte then begin
      free (file_str_desc.text_addr);
      if file_str_desc.text_addr.indirect then begin
        reg := get_reg (36);
        gen_rm (movei, reg, file_str_desc.text_addr);
        gen_ri (hrli, reg, 440700b);
        file_str_desc.text_addr := reg_addr (reg);
      end
      else begin
        file_str_desc.text_addr := increment_addr (file_str_desc.text_addr, -1, 28, 7);
        temp_addr := absolute_reference;
        temp_addr.reloc := gen_bptr (file_str_desc.text_addr);
        file_str_desc.text_addr := temp_addr;
      end;
      file_str_desc.text_addr.mode := byte;
      str_format := x_format;
    end;

    (* Compute the control bits. *)

    if open_mode = textfile then begin
      case opcode of
        open_op:    rts := rt_open;
        rewrite_op: rts := rt_rewrite;
        reset_op:   rts := rt_reset
      end;
      bits := 0;
    end
    else begin
      if open_mode = binaryfile
        then rts := rt_open_binary
        else rts := rt_open_typed;
      case opcode of
        open_op,
        reset_op:   bits := 8;
        rewrite_op: bits := 4;
        update_op:  bits := 12
      end;
      if desc.base^.packable then
        bits := bits + 2;
    end;
    if iname = 2 then
      bits := bits + 1;

    (* Call the runtime routine. *)

    gen_rt (pushj, sb, rts);
    gen_ri (arg, bits, open_size);
    gen_str_desc (file_str_desc, str_format);
    gen_rm (arg, 0, options_addr);

    free_string (file_str_desc);
    free (options_addr);

    (* The file control block address is in register 1.  Move it into some other
       register, which will be the return register for the operation. *)

    reg := get_reg (36);
    gen_rr (move, reg, 1);
    rt_open_call := reg_addr (reg);
  end (* with node^ *);
 end;
$PAGE read_write_call
(* READ WRITE CALL will generate a runtime routine call for a read_op or
   write_op tuple. *)

public procedure read_write_call ( op: tuple );

 var
    fil: expr;
    datum_code: registers; (* acc field of datum descriptor *)
    format_code: registers; (* acc field of first format word *)
    string_io: boolean;
    item_prec: bit_range;
    io_str_desc: str_desc;
    item_format: str_desc_format;
    cval: int_type;
    io_item, io_width, io_file, io_precision, range_block: addr_desc;
    cons_def: def;

 type
    rt_sym_list = array [boolean, boolean] of rt_symbol;

 const
    fd_rts: rt_sym_list = ( rt_wr_fdn, rt_rd_fdn, rt_wr_fdr, rt_rd_fdr );
    sv_rts: rt_sym_list = ( rt_wr_svn, rt_wr_svn, rt_wr_svr, rt_wr_svr );
    ss_rts: rt_sym_list = ( rt_wr_ssn, rt_rd_ssn, rt_wr_ssr, rt_rd_ssr );

 type
    format_list = array [rd_wr_mode] of registers;

 const
    format_table: format_list = ( 0, 0, 0, 0, 8, 4, 0, 8, 12, 1, 0 );

 type
    req_list = array [0..7, boolean] of rt_symbol;

 const
    req_sym: req_list =
     (  rt_int_write,   rt_int_read,    rt_real_write,  rt_real_read,
        rt_real_write,  rt_real_read,   rt_xstr_write,  rt_xstr_read,
        rt_fstr_write,  rt_fstr_read,   rt_cstr_write,  rt_cstr_read,
        rt_strv_read,   rt_strv_read,   rt_bool_write,  rt_bool_write    );

	(* Note that "write varying string" and "read boolean" are impossible
	   combinations. *)


 begin
  with op^ do begin

    if rw_mode = binaryrw then begin
      io_file := argument (rw_file);
      io_item := argument (rw_item); (* item is *address* of data area *)
      if aconstp (io_item, cval) then
        io_item := gen_cst (cval);
      if (rw_item^.desc.kind in [arrays, strings]) andif dynamic_flex (rw_item) then
	io_item := increment_addr (io_item, 1, 0, 36);
      io_width := argument (rw_width);
      if opcode = read_op
        then gen_rt (pushj, sb, rt_read_binary)
        else gen_rt (pushj, sb, rt_write_binary);
      gen_rm (arg, 0, arg_addr (io_file));
      gen_rm (arg, 0, io_item);
      gen_rm (arg, 0, arg_addr (io_width));
      free (io_width);
      free (io_item);
      free (io_file);
    end

    else if rw_mode = imagerw then begin
      io_file := argument (rw_file);
      io_item := fetch_fullword (rw_item);
      if aconstp (io_item, cval) then
        io_item := gen_cst (cval);
      if opcode = read_op
        then gen_rt (pushj, sb, rt_read_image)
        else gen_rt (pushj, sb, rt_write_image);
      gen_rm (arg, 0, arg_addr (io_file));
      gen_rm (arg, 0, io_item);
      free (io_file);
      free (io_item);
    end

    else (* formatted i/o *) begin
      string_io := (rw_mode in [leftrw, rightrw]);
      if (rw_item^.desc.kind = reals) andif (rw_item^.desc.precision > srealprec)
        then item_prec := 72
        else item_prec := 36;

      if not rw_old_file then begin
        if not getputstring then
          io_file := argument (rw_file);
      end;

      if string_io then begin
	if opcode = read_op then
	  io_str_desc := fetchtranslated (rw_item, max_length)
	else
	  io_str_desc := fetchtranslated (rw_item, actual_length);
        prep_string (io_str_desc, item_format);
      end
      else begin
        io_item := fetch_fullword (rw_item);
        if aconstp (io_item, cval) then
          io_item := gen_cst (cval)
      end;
      if rw_precision <> nil then
        io_precision := argument (rw_precision);
      if rw_width <> nil then
        io_width := argument (rw_width);

      if getputstring then begin
        if str_bptr
          then gen_rt (pushj, sb, ss_rts [rw_old_file, (opcode = read_op)])
          else gen_rt (pushj, sb, sv_rts [rw_old_file, (opcode = read_op)])
      end
      else
        gen_rt (pushj, sb, fd_rts [rw_old_file, (opcode = read_op)]);

      if not rw_old_file then begin
        if getputstring then begin
          gen_rm (arg, 0, str_addr);
        end
        else begin
          gen_rm (arg, 0, arg_addr (io_file));
          free (io_file);
        end
      end;

      case rw_mode of
        booleanrw:
          datum_code := 7;
        decimalrw, octalrw, hexrw:
          datum_code := 0;
        realrw, fixedrw, floatrw:
          if item_prec = 36
            then datum_code := 1
            else datum_code := 2;
        leftrw, rightrw: begin
          case item_format of
            c_format: datum_code := 5;
            f_format:
              if (opcode = read_op) andif (rw_item^.desc.kind = strings) andif
                 (rw_item^.desc.str_kind = varying) then begin
                datum_code := 6;
                io_str_desc.text_addr.offset := io_str_desc.text_addr.offset - 1;
                if io_str_desc.len_context <> max_length then begin
		  assert (false); (* npr254/fr 305 SHOULD prevent us being here *)
                  if io_str_desc.len_context = actual_length then
                    free (io_str_desc.len_addr);
                  if io_str_desc.base_is_bound
                    then io_str_desc.len_addr := duplicate_addr (io_str_desc.base_addr)
                    else io_str_desc.len_addr := int_value (io_str_desc.max_len);
		  io_str_desc.len_context := max_length;
		end
	      end
	      else datum_code := 4;
            x_format: datum_code := 3
          end;
        end
      end (* case *);

      rw_request [req_sym [datum_code, (opcode = read_op)]] := true;

      format_code := format_table [rw_mode];
      if rw_width <> nil then
        format_code := format_code + 2;

      if (opcode = read_op) and (datum_code <= 2) (* integer, real, double *)
             andif (chk_inp_opt in cur_block^.semantic_options) then
        datum_code := datum_code + 8; (* mark it to be checked *)
  
      if string_io then begin
        gen_str_coded (datum_code, io_str_desc, item_format);
        free_string (io_str_desc);
      end
      else begin
        gen_rm (arg, datum_code, io_item);
        free (io_item);
      end;

      if datum_code >= 8 (* marked to be checked *) then begin
        cons_def := make_def (constant_def);
        mark_def (cst_area, cons_def);
        range_block := absolute_reference;
        range_block.reloc := reldef (cons_def);
        gen_rm (arg, 0, range_block);
        if datum_code = 8 (* integer *) then begin
	  gen_word (cst_area, ('F', rw_item^.desc.base^.minval), fullword);
	  gen_word (cst_area, ('F', rw_item^.desc.base^.maxval), fullword);
        end
        else (* real or double *) begin
	  gen_word (cst_area, ('R', rw_item^.desc.base^.rminval), realword);
	  gen_word (cst_area, ('R', rw_item^.desc.base^.rmaxval), realword);
        end;
      end;

      if rw_precision <> nil then begin
        gen_rm (arg, format_code, arg_addr (io_precision));
        free (io_precision);
        format_code := 0;
      end
      else if rw_mode = floatrw then begin
        gen_ri (arg, 6, 0);
        format_code := 0
      end;

      if rw_width <> nil then begin
        gen_rm (arg, format_code, arg_addr (io_width));
        free (io_width);
      end
      else if format_code <> 0 then
        gen_ri (arg, format_code, 0);
  
    end (* if formatted i/o *);
  end (* with op^ *);
 end.
   Bç