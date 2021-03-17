$TITLE M68STR - M68000 code generator string routines.

module m68str options check;
$PAGE includes and forward declarations
$SYSTEM pascal.inc
$SYSTEM ptmcon.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68cgu.inc
$SYSTEM m68utl.inc
$SYSTEM m68exp.inc
$SYSTEM m68gen.inc
$SYSTEM m68set.inc
$system m68cll.inc
  

public function fetchstring (
		     exp: expr;
		     length_context: str_len_context;
		     temp_limit: char_range )
			  : string_descriptor; forward;

function fetch_string (
		exp: expr;
		context_tuple: tuple;
		var optimized: boolean;
		length_context: str_len_context;
		temp_limit: char_range )
			: string_descriptor options special(coercions); forward;

public const
  no_limit: char_range := maximum ( char_range );
$PAGE runtime operator tables

type
  mvop_table = array [ boolean, (* padded/unpadded move *)
		       c_format..f_format, (* C or F source *)
		       f_format..r_format, (* F or R destination *)
		       str_translation ] (* direct, uppercase, lowercase *)
                    of rt_symbol;

  binop_table =              (* op <- table [leftop_fmt, rightop_fmt] *)
        array [c_format..f_format, c_format..f_format] of rt_symbol;

const
  move_routines: mvop_table := (
  ( ( ( rt_mv_cf,	rt_mv_cfu,	rt_mv_cfl ),
      ( rt_mv_cr,	rt_mv_cru,	rt_mv_crl ) ),
    ( ( rt_mv_ff,	rt_mv_ffu,	rt_mv_ffl ),
      ( rt_mv_fr,	rt_mv_fru,	rt_mv_frl ) ) ),
  ( ( ( rt_mv_cfp,	rt_mv_cfpu,	rt_mv_cfpl ),
      ( rt_mv_crp,	rt_mv_crpu,	rt_mv_crpl ) ),
    ( ( rt_mv_ffp,	rt_mv_ffpu,	rt_mv_ffpl ),
      ( rt_mv_frp,	rt_mv_frpu,	rt_mv_frpl ) ) ) );

  concat_routines: mvop_table := (
  ( ( ( rt_ct_cf,	rt_ct_cfu,	rt_ct_cfl ),
      ( rt_mv_cr,	rt_mv_cru,	rt_mv_crl ) ),
    ( ( rt_ct_ff,	rt_ct_ffu,	rt_ct_ffl ),
      ( rt_mv_fr,	rt_mv_fru,	rt_mv_frl ) ) ),
  ( ( ( rt_ct_cfp,	rt_ct_cfpu,	rt_ct_cfpl ),
      ( rt_mv_crp,	rt_mv_crpu,	rt_mv_crpl ) ),
    ( ( rt_ct_ffp,	rt_ct_ffpu,	rt_ct_ffpl ),
      ( rt_mv_frp,	rt_mv_frpu,	rt_mv_frpl ) ) ) );

  comparison_routines: binop_table := (
    ( rt_stop,		rt_cp_cf ),
    ( rt_cp_fc,		rt_cp_ff ) );

  index_routines: binop_table := (
    ( rt_stop,		rt_ix_cf ),
    ( rt_ix_fc,		rt_ix_ff ) );

  search_verify_routines: packed array [
	boolean,	(* true for verify, false for search *)
	c_format..f_format,	(* string format *)
	boolean,	(* false = o_format set, true = l_format set *)
	no_trans..upper_trans] of rt_symbol := (
    ( ( ( rt_sr_co,	rt_sr_cou ),
	( rt_sr_cl,	rt_sr_clu ) ),
      ( ( rt_sr_fo,	rt_sr_fou ),
	( rt_sr_fl,	rt_sr_flu ) ) ),
    ( ( ( rt_vf_co,	rt_vf_cou ),
	( rt_vf_cl,	rt_vf_clu ) ),
      ( ( rt_vf_fo,	rt_vf_fou ),
	( rt_vf_fl,	rt_vf_flu ) ) ) );
$PAGE free_string
(* FREE_STRING frees all of the address descriptors contained within
   a string descriptor (parameter DESC).  *)

public procedure free_string ( desc: string_descriptor );

begin
  with desc do begin
    free_desc ( base_addr );
    if len_context <> no_length then
      free_desc ( len_addr );
    if text_valid then
      free_desc ( text_addr );
  end;
end  (* free_string *) ;
$PAGE str_desc_size
(* STR_DESC_SIZE fills in the size and signed value fields for an
   operand descriptor related to strings.  In particular, string
   lengths are always set with:

	VALUE_SIZE := SIZE_WORD

   while operand descriptors for string text are always set with:

	VALUE_SIZE := SIZE_LONG

   and characters are set with:

	VALUE_SIZE := SIZE_BYTE

   In all cases EXTENDED_SIZE is set to NO_SIZE, SIGNED_VALUE to
   FALSE, and KNOWN_POSITIVE  to FALSE.	*)

public procedure str_desc_size ( var op: op_desc; size: op_sizes );

begin
  assert ( size in [size_byte, size_word, size_long] );
  with op do begin
    value_size := size;
    extended_size := no_size;
    signed_value := false;
    known_positive := false;
  end;
end (* str_desc_size *);
$PAGE str_varying_dynamic_temp
(* STR_VARYING_DYNAMIC_TEMP allocates a dynamic temporary for a varying string,
   given its current length.  The address descriptor returned is
   for the length word. *)

public function str_varying_dynamic_temp ( len: op_desc ): op_desc;

var
  temp_desc: op_desc;

begin
  temp_desc := reg_desc ( get_dreg (), size_word, false );
  gen_mm ( move_opc, len, temp_desc );
  gen_im ( add_opc, str_lw_bytes, temp_desc );
  str_varying_dynamic_temp := get_temp ( temp_desc, false (* byte cnt *) );
  str_varying_dynamic_temp.value_size := size_word;
  str_varying_dynamic_temp.extended_size := no_size;
  str_varying_dynamic_temp.signed_value := false;
end (* str_varying_dynamic_temp *);
$PAGE init_string_descriptor
(* INIT_STRING_DESCRIPTOR initializes some fields of a string
   descriptor record (parameter DESC).  Only those fields for
   which reasonable defaults can be supplied are initialized.  *)

public procedure init_string_descriptor ( var desc: string_descriptor );

begin
  with desc do begin
    base_is_bound := false;
    len_context := actual_length;
    text_valid := false;
    trans_code := no_trans;
  end  (* with *) ;
end  (* init_string_descriptor *) ;
$PAGE skip_desc_word
(* SKIP_DESC_WORD takes a string descriptor and returns an address
   descriptor for the location following any bound's word preceeding
   the string (i.e., the start of the text for nonvarying strings and
   the length word address for varying strings).  The TEXT_ADDR field
   of the string descriptor may be set by this routine.  *)

public function skip_desc_word ( var desc: string_descriptor ): op_desc;

var
  is_varying: boolean;

begin
  with desc do begin
    is_varying := (type_desc.kind = strings) andif (type_desc.str_kind = varying);

    if text_valid then begin	(* if address of text present, then *)
      skip_desc_word := duplicate_desc ( text_addr );	(* back up 2 bytes *)
      if is_varying
	then skip_desc_word := increment_addr ( skip_desc_word, - str_lw_bytes );
    end

    else begin	(* text addr invalid, use base address *)
      skip_desc_word := duplicate_desc ( base_addr );

      if base_is_bound then begin	(* if flex, increment addr past bounds word *)
	skip_desc_word := increment_addr ( skip_desc_word, str_upb_bytes );

	text_valid := true;	(* set TEXT_ADDR field, since we can calculate *)
	text_addr := duplicate_desc ( skip_desc_word );	(* address w/o generating code *)
	str_desc_size ( text_addr, size_long );
	if is_varying
	  then text_addr := increment_addr ( text_addr, str_lw_bytes );
        
	(* If the string descriptor describes the actual length of a
	   varying string, then update the LEN_ADDR field also.  The new
	   address should describe the same location as the old one, but
	   may be in a more efficient, shorter format.   *)

	if ( len_context = actual_length ) and ( type_desc.str_kind = varying ) then begin
	  free_desc ( len_addr );			(* free old length address *)
	  len_addr := duplicate_desc ( skip_desc_word );	(* set it to new, hopefully better, form *)
	  str_desc_size ( len_addr, size_word );
	end;
      end;
    end;

    if type_desc.kind = chars then
      str_desc_size ( skip_desc_word, size_byte )
    else if is_varying
      then str_desc_size ( skip_desc_word, size_word )
      else str_desc_size ( skip_desc_word, size_long );

  end  (* with *) ;
end  (* skip_desc_word *) ;
$PAGE skip_len_words
(* SKIP_LEN_WORDS takes a string descriptor and returns an address
   descriptor for the byte beginning the text of the string.  Note 
   that parameter DESC must be a VAR parameter because the TEXT_VALID
   and TEXT_ADDR fields of the string descriptor may be updated.  *)

public function skip_len_words ( var desc: string_descriptor ): op_desc;

var
  increment: 0 .. str_upb_bytes + str_lw_bytes;

begin
  with desc do begin
  
    (* If the TEXT_ADDR field is already valid then simply duplicate
       and return it.  *)

    if text_valid then begin
      skip_len_words := duplicate_desc ( text_addr );
    end

    (* The TEXT_ADDR field is not valid.  We must increment the base
       address by the length of any bound or length words.  The
       TEXT_ADDR field will then be set.  *)

    else begin
      if base_is_bound	(* flex string ? *)
	then increment := str_upb_bytes
 	else increment := 0;

      if ( type_desc.kind = strings ) andif	(* varying string? *)
	 ( type_desc.str_kind = varying )
	then increment := increment + str_lw_bytes;

      skip_len_words := duplicate_desc ( base_addr );
      skip_len_words := increment_addr ( skip_len_words, increment );

      text_valid := true;
      text_addr := duplicate_desc ( skip_len_words );
      if type_desc.kind = chars
	then str_desc_size ( text_addr, size_byte )
	else str_desc_size ( text_addr, size_long );

      (* If the string descriptor describes the actual length of a varying
	 string, then we change the length address in the descriptor.  The
	 new address should describe the same location as the old one, but
	 may be in a more efficient, shorter format.  *)

      if (len_context = actual_length) and (type_desc.kind = strings) andif
	 (type_desc.str_kind = varying) then begin
	free_desc ( len_addr );
	len_addr := duplicate_desc ( skip_len_words );
	len_addr := increment_addr ( len_addr, - str_lw_bytes );
	str_desc_size ( len_addr, size_word );
      end;
    end  (* else *) ;

    if type_desc.kind = chars
      then str_desc_size ( skip_len_words, size_byte )
      else str_desc_size ( skip_len_words, size_long );
  end  (* with *) ;
end  (* skip_len_words *) ;
$PAGE pushstring, pushlength
(* PUSHSTRING pushes a string parameter address onto the stack for a runtime call.
   It insures that characters are addressible as strings, and does NOT
   free the string parameter itself.  PUSHLENGTH just pushes the length
   word.  *)

public procedure pushstring ( var desc: string_descriptor; kind: string_kind );

var
  temp_desc: op_desc;

begin
  if ( desc.type_desc.kind = chars) andif not ( desc.base_addr.mode in control_modes ) then begin
    temp_desc := get_temp ( int_desc ( 1, size_word, false ), false );
    temp_desc.value_size := size_byte;
    gen_mm ( move_opc, desc.base_addr, temp_desc );
  end
  else if kind = varying
    then temp_desc := skip_desc_word ( desc )
    else temp_desc := skip_len_words ( desc );
  pusha ( temp_desc );
end (* pushstring *);

public procedure pushlength ( desc: string_descriptor );

begin
  push ( duplicate_desc ( desc.len_addr ), size_word );
end (* pushlength *);
$PAGE string_rt_parameter
(* STRING_RT_PARAMETER pushes a string runtime parameter onto the stack
   and returns the format of the parameter. *)

function string_rt_parameter ( var desc: string_descriptor ): string_descriptor_format;

var
  temp_desc: op_desc;

begin
  with desc do begin
    if type_desc.kind = chars then begin	(* push the character itself *)
      temp_desc := duplicate_desc ( base_addr );
      push ( temp_desc, size_byte );
      string_rt_parameter := c_format;
    end
    else begin	(* must push address and length *)
      pushstring ( desc, nonvarying );
      pushlength ( desc );
      string_rt_parameter := f_format;
    end;
  end (* with *);
end (* string_rt_parameter *);
$PAGE move_string
(* MOVE_STRING moves a string specified by one string descriptor 
   (parameter RHS_DESC) to a location specified by a second string
   descriptor (parameter LHS_DESC).  Parameter PADDED is a boolean
   which indicates whether or not the result string should be padded
   with blanks if the lhs string is larger than the rhs string.
   Parameter FREE_PROC is a procedure parameter which will be called
   immediately before the string instruction is emitted.  It is 
   generally used to permit the caller to free_desc all or some of
   the address descriptors in RHS_DESC and/or LHS_DESC.
   Parameter REMAINDER is a boolean which indicates whether or not
   the target string is the remainder of a previous string move (hence
   an "r_format" descriptor will be issued). *)

public procedure move_string (
        var lhs_desc: string_descriptor; (* target string *)
        var rhs_desc: string_descriptor; (* source string *)
        padded: boolean;        (* if target to be padded *)
        free_proc: free_procedure; (* to be called sometime *)
        remainder: boolean;     (* if target is remainder of previous move *)
        suppress_null_move: boolean);   (* if true, ok to ignore null varying string move *)
                                (* Note: first move of concatenation must be made *)

var
  lhs_format, rhs_format: string_descriptor_format;
  rts: rt_symbol;
  rhs_length: char_range;

begin

  (* If possible, suppress null move to a varying string. *)

  if suppress_null_move andif
    ( ( lhs_desc.type_desc.str_kind = varying ) orif not padded ) andif
    ( rhs_desc.type_desc.kind = strings ) andif
    ( aconstp ( rhs_desc.len_addr, rhs_length ) ) andif
    ( rhs_length = 0 ) then begin
      free_proc;
  end
  else begin
    if remainder
      then lhs_format := r_format
      else lhs_format := string_rt_parameter ( lhs_desc );
    rhs_format := string_rt_parameter ( rhs_desc );

    (* Perform the requested frees. *)

    free_proc;

    (* Choose a runtime entry point. *)

    rts := move_routines[padded, rhs_format, lhs_format, rhs_desc.trans_code];

    (* Call the runtime to make the move. *)

    gen_rt ( rts );
  end;
end  (* move_string *) ;
$PAGE concat_desc
(* CONCAT_DESC generates code for a CAT_OP expression tuple.  Parameter
   EXP is the CAT_OP expression tuple.  Parameter CASE_CODE indicates
   any upper or lower case conversions which should be done during the
   concatenation move.  Concatenations are always done by moving the
   operand strings to a temporary.  A string descriptor for the resulting
   temporary is returned as the function return value.   TEMP_LIMIT
   specifies an upper bound for any static temporary used in the
   concatenation.  If this limit is less than the constant NO_LIMIT
   then it will be used whether or not dynamic temps are involved
   in the concatenation. *)

function concat_desc (
		exp: expr;
		case_code: str_translation;
		temp_limit: char_range )
			 : string_descriptor;

type
  string_descriptor_array = array [1..*] of string_descriptor;

var
  op_descs: ^string_descriptor_array;
  temp_length, len_value: char_range;
  i: oper_range;
  result_base: op_desc;
  len_loaded: boolean;
  const_part: char_range;
  total_len_addr, temp_len_addr: op_desc;
  const_addr: op_desc;
  op: string_descriptor;
  have_dynamic_operand: boolean;


  (* The following procedure is passed as a parameter to MOVE_STRING
     to free the desired strings before the runtime call. *)

  procedure concat_free;
    begin
      free_string ( op_descs^[ i ] );   (* free rhs string *)
    end;

  procedure check_length ( laddr: op_desc );

  var
    const_len: char_range;

  begin
    if aconstp ( laddr, const_len ) then
      const_part := const_part + const_len      (* accumulate *)
    else begin
      if len_loaded then        (* accumulate at runtime *)
	gen_mm ( add_opc, laddr, total_len_addr )
      else begin
	total_len_addr := reg_desc ( get_dreg (), size_word, false );
	str_desc_size ( total_len_addr, size_word );
	gen_mm ( move_opc, laddr, total_len_addr );
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
    have_dynamic_operand := false;
    len_loaded := false;
    const_part := 0;
    for i := 1 to upperbound ( operand ) do begin
      if temp_limit < no_limit
	then op := fetchstring ( operand[ i ], actual_length, max ( 0, temp_limit - const_part ) )
	else op := fetchstring ( operand[ i ], actual_length, no_limit );
      if case_code <> no_trans then op.trans_code := case_code;
      op_descs^[i] := op;
      if op.type_desc.str_flex then
	have_dynamic_operand := true
      else if aconstp ( op.len_addr, len_value )
	then temp_length := temp_length + len_value
	else temp_length := temp_length + op.max_len;
      check_length ( op.len_addr );
    end;

    (* If any operand is flex and no bound was specified on the size
       of the temp to be use, then a dynamic temp must be allocated. *)

    init_string_descriptor ( concat_desc );
    if have_dynamic_operand andif ( temp_limit = no_limit ) then begin
      assert ( len_loaded );
      if const_part <> 0 then
	gen_im ( add_opc, const_part, total_len_addr );
      temp_len_addr := duplicate_desc ( total_len_addr );
    end
    else begin
      if have_dynamic_operand
	then temp_length := temp_limit
	else temp_length := min ( temp_length, temp_limit );
      temp_len_addr := int_desc ( temp_length, size_word, false );
      if len_loaded then begin
	if const_part <> 0 then
	  gen_im ( add_opc, const_part, total_len_addr )
      end
      else total_len_addr := duplicate_desc ( temp_len_addr );
    end;
    with concat_desc do begin
      result_base := get_temp ( duplicate_desc ( temp_len_addr ), false (* byte cnt *) );
      str_desc_size ( result_base, size_long );
      base_addr := duplicate_desc ( result_base );
      len_addr := temp_len_addr;
      text_valid := true;
      text_addr := duplicate_desc ( base_addr );
      type_desc := desc;
    end;

    for i := 1 to upperbound (operand) do begin

      (* move the operand string *)

      move_string (concat_desc, op_descs^[i], false, concat_free, i > 1, i > 1);
    end;

    (* Set the length field of the result string descriptor and reset the base
       address of the result. *)

    free_string ( concat_desc );
    init_string_descriptor ( concat_desc );

    with concat_desc do begin
      len_addr := total_len_addr    base_addr := result_base; (* restore base addr of temp *)
      text_valid := true;
      text_addr := duplicate_desc ( base_addr );
      if have_dynamic_operand and ( temp_limit = no_limit )
	then max_len := no_limit
	else max_len := temp_length;
    end;
    dispose ( op_descs );

  end  (* with *) ;
end  (* concat_desc *) ;
$PAGE substring_descriptor
(* SUBSTR_DESC generates code for SUBSTR_REF expression tuples.
   Parameter EXP is the SUBSTR_REF tuple.  Parameter CASE_CODE indicates
   any upper or lower case conversions inherited from higher nodes in the
   expression tree.  A string descriptor describing the resulting substring
   is returned.  *)

function substring_descriptor ( exp: expr; case_code: str_translation ): string_descriptor;

var
  base_desc: string_descriptor;
  length_value, index_value: char_range;
  index_desc, temp_desc: op_desc;

begin
  init_string_descriptor ( substring_descriptor );
  with substring_descriptor do begin
    base_desc := fetchstring ( exp^.base_string, actual_length, no_limit );
    len_addr := fetch ( exp^.substr_length, nonstack_modes, immediate_mode, word_min, false );
    if ( len_addr.value_size = size_long ) andif not ( len_addr.mode in register_modes + [immediate_mode] ) then
      len_addr := increment_addr ( len_addr, 2 );
    str_desc_size ( len_addr, size_word );
    if aconstp ( len_addr, length_value )
      then max_len := length_value
      else max_len := base_desc.max_len;
    if not base_desc.text_valid then begin	(* must have address of start of string *)
      temp_desc := skip_len_words ( base_desc );
      free_desc ( temp_desc );
    end;

    (* Fetch the index value and increment the base address accordingly. *)

    index_desc := fetch ( exp^.substr_index, nonstack_modes, immediate_mode, word_min, false );
    if aconstp ( index_desc, index_value ) then begin
      if index_value = 1
	then base_addr := duplicate_desc ( base_desc.text_addr )
        else base_addr := increment_addr ( duplicate_desc ( base_desc.text_addr ), index_value - 1 )
    end
    else begin
      temp_desc := copy_dreg ( index_desc );
      gen_im ( sub_opc, 1, temp_desc );
      base_addr := offset_addr ( duplicate_desc ( base_desc.text_addr ), temp_desc );
    end;
    text_addr := duplicate_desc ( base_addr );
    text_valid := true;
    type_desc := exp^.desc;
    if case_code = no_trans
      then trans_code := base_desc.trans_code
      else trans_code := case_code;
    if type_desc.kind = chars then begin
      str_desc_size ( base_addr, size_byte );
      str_desc_size ( text_addr, size_byte );
    end;
  end (* with *);
  free_string ( base_desc );
end (* substring_descriptor *);
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

function get_max_length ( str_expr: expr; str_addr: op_desc ): op_desc;

begin
  with str_expr^ do begin
    if desc.kind = chars then
      get_max_length := int_desc ( 1, size_word, false )
    else if desc.str_flex then
      get_max_length := upper_bound ( str_expr, str_addr )
    else get_max_length := int_desc ( desc.str_length, size_word, false );
  end;
  str_desc_size ( get_max_length, size_word );
end  (* get_max_length *) ;
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

function get_actual_length ( str_expr: expr; str_addr: op_desc ): op_desc;

begin
  with str_expr^ do begin
    if desc.kind = chars then
      get_actual_length := int_desc ( 1, size_word, false )
    else if desc.str_kind = varying then begin	(* return length word address *)
      get_actual_length := duplicate_desc ( str_addr );
      if dynamic_flex ( str_expr ) then	(* Skip bounds word if flex. *)
	get_actual_length := increment_addr ( get_actual_length, str_upb_bytes );
    end
    else if desc.str_flex	(* if non-varying flex then get bounds word *)
      then get_actual_length := upper_bound ( str_expr, str_addr )
      else get_actual_length := int_desc ( desc.str_length, size_word, false );
	    (* if non-flex and fixed, length is constant. *)
  end (* with *);
  str_desc_size ( get_actual_length, size_word );
end (* get_actual_length *);
$PAGE str_case_ops
(* STR_CASE_OPS is passed a string valued expression tuple (parameter
   EXP).  If the tuple opcode is LWC_OP, UPC_OP or STRCVT_OP, then
   parameter BASE_EXPR will be set to the first descendent tuple whose
   opcode is not one of those three;  otherwise BASE_EXPR will be set
   to the vale of EXP.  CASE_CODE is set to reflect the kind of the 
   first case conversion operator encountered; if none is encountered
   it will be set to the value NO_TRANS. 

   USE_EXPRS is a caller-supplied value indicating whether skipped tuples
   are to be considered "used" by this call, in which case their usage
   counts are to be decremented.  Some of the cases in which this is an issue,
   and their underlieing motives, are:

   (1) GET_STRING_DESCRIPTOR cares only for the base string expression and
       whether or not case conversion is necessary.  Intervening STRCVT_OP,
       UPC_OP, and LWC_OP tuples should be consumed here but ONLY if they
       are not the first tuple on the chain, since FETCH_STRING handles
       that itself.

   (2) VALUE_PARAMETER and M68CLL@MBD_STR are more concerned with
       context information and will fetch the initial string expression
       anyway, so skipped tuples will not be used here.

   (3) MOVE_STRING_INLINE makes a subsequent decision whether the expression
       should be fetched or folded, in which case it handles the usage 
       counts internally.

   This change was required by the deletion of the IF on a statement by
   statement basis as well as the decision not to always fetch string
   expressions. *)

public procedure str_case_ops ( exp: expr;  var base_expr: expr;
				var case_code: str_translation;
				use_exprs: boolean );

begin
  base_expr := exp;
  if base_expr^.opcode = strcvt_op then begin	(* Skip string conversions *)
    loop
      base_expr := base_expr^.operand[ 1 ];
    exit if base_expr^.opcode <> strcvt_op;
      if use_exprs then
	dec_expr_usage ( base_expr );
    end;
  end;
  case_code := no_trans;

  if ( base_expr^.opcode = lwc_op ) orif        (* if case op is present, *)
     ( base_expr^.opcode = upc_op ) then begin  (* then set parameter CASE_CODE *)
    if base_expr^.opcode = lwc_op               (* to reflect which conversion *)
      then case_code := lower_trans
      else case_code := upper_trans;

    repeat      (* skip any subsequent case or type *)
      if use_exprs andif ( base_expr <> exp ) then
	dec_expr_usage ( base_expr );
      base_expr := base_expr^.operand[ 1 ]      (* conversion operators *)
    until ( base_expr^.opcode <> strcvt_op ) and
          ( base_expr^.opcode <> lwc_op ) and
          ( base_expr^.opcode <> upc_op );
  end;

end (* str_case_ops *);
$PAGE date_desc
(* DATE DESC emits a call to the fortran DATE routine, returning
   a string descriptor for the result. *)

function date_desc (exp: expr; case_code: str_translation): string_descriptor;

const
  date_length = 9;	(* length of date string returned by runtime *)

begin
  init_string_descriptor ( date_desc );
  date_desc.len_addr := int_desc ( date_length, size_word, false );
  str_desc_size ( date_desc.len_addr, size_word );
  date_desc.base_addr := get_temp ( date_desc.len_addr, false );
  str_desc_size ( date_desc.base_addr, size_long );
  pusha ( duplicate_desc ( date_desc.base_addr ) );
  gen_rt ( rt_date );
  with date_desc, type_desc do begin
    text_valid := true;
    text_addr := duplicate_desc ( base_addr );
    base := nil;
    kind := strings;
    str_flex := false;
    str_kind := nonvarying;
    str_length := date_length;
  end;
end (* date_desc *);
$PAGE get_string_descriptor
(* GET_STRING_DESCRIPTOR is given a string (or possibly character) valued
   expression tuple (parameter ORIGINAL_EXP) and returns a string
   descriptor for the value.  A parameter (CONTEXT) is also passed
   in which indicates whether an address descriptor for the length
   of the string is desired and, if so, whether the actual length
   or the maximum length is desired.  TEMP_LIMIT is used in some cases
   to establish an upperbound on statically allocated temporaries. *)

function get_string_descriptor (
		original_exp: expr;
		context: str_len_context;
		temp_limit: char_range )
			 : string_descriptor;

var
  exp: expr;
  case_code: str_translation;

begin

  (* we first process any LWC_OPs, UPC_OPs, or STRCVT_OPs.  CASE_CODE
     will be set to indicate any case conversions necessary.  *)

  str_case_ops ( original_exp, exp, case_code, true );

  (* Get the descriptor.  *)

  case exp^.opcode of

    cat_op:     (* concatenation operator *)
      begin
        get_string_descriptor := concat_desc ( exp, case_code, temp_limit ); 
        if context = no_length
	  then free_desc ( get_string_descriptor.len_addr )
	  else get_string_descriptor.len_context := context;
      end;

    substr_ref: (* substring operator *)
      begin
        get_string_descriptor := substring_descriptor ( exp, case_code );
        if context = no_length
	  then free_desc ( get_string_descriptor.len_addr )
	  else get_string_descriptor.len_context := context;
      end;

    date_op: begin
      get_string_descriptor := date_desc (exp, case_code);
      if context = no_length then
	free_desc (get_string_descriptor.len_addr);
    end;

    in_str_op,	(* getstring *)
    out_str_op:	(* putstring *)
      begin
	get_string_descriptor := fetchstring ( exp^.operand[1], context, temp_limit );
      end;

    others:     (* an entire string reference *)
      begin
        with get_string_descriptor do begin
	  base_is_bound := (exp^.desc.kind <> chars) andif
			   ( dynamic_flex ( exp ) );
	  base_addr := do_fetch ( exp, nonstack_modes, indirect_mode, any_size, false );
	  text_valid := false;
	  type_desc := exp^.desc;
	  if type_desc.kind <> chars then begin
	    if base_is_bound orif ( type_desc.str_kind = varying )
	      then str_desc_size ( base_addr, size_word )
	      else str_desc_size ( base_addr, size_long );
	  end
	  else str_desc_size ( base_addr, size_byte );
	  trans_code := case_code;
	  len_context := context;
	  if context = actual_length then
	    len_addr := get_actual_length (exp, base_addr)
	  else if context = max_length then
	    len_addr := get_max_length (exp, base_addr);

	  (* If this is a varying, dynamic flex string then then TEXT_ADDR
	     field can be set without generating additional code. *)

	  if ( context = actual_length ) and base_is_bound and
	    ( type_desc.str_kind = varying ) then begin
	    text_valid := true;
	    text_addr := duplicate_desc ( len_addr );
	    str_desc_size ( text_addr, size_long );
	    text_addr := increment_addr ( text_addr, str_lw_bytes );
	  end;

	  with exp^.desc do begin
	    if kind = chars then
	      max_len := 1
	    else if str_flex
	      then max_len := maximum ( char_range )
	      else max_len := str_length;
	  end;

        end  (* with get_string_descriptor *) ;
      end  (* string reference case *)

  end  (* case *) ;

  (* If the base expr tuple filtered through STR_CASE_OPS is not the same as
     that passed in by FETCH_STRING, then its usage_count will not
     have been decremented by DO_FETCH or by STR_CASE_OPS, nor will
     FETCH_STRING do so.  Therefore it must be explicitely done here. *)

  if exp <> original_exp then
    dec_expr_usage ( exp );
end (* get_string_descriptor *);
$PAGE fetch_string
(* FETCH_STRING is the counterpart of FETCH for fetching string values.
   Parameter EXP is the string valued expression tuple being fetched.
   Parameter CONTEXT_TUPLE, if not nil, is a string assignment tuple
   from which recursion and overlapping may be determined.
   Parameter CONTEXT indicates the kind of string length which should
   be set in the string descriptor returned.  Most of the logic in this
   routine is concerned with handling expression nodes with multiple
   uses.  For multi-use string valued expression tuples to be evaluated
   correctly, fetch_string must be called for each use.  TEMP_LIMIT 
   establishes an upperbound on some statically allocated temporaries. *)

function fetch_string (*
		exp: expr;
		context_tuple: tuple;
		var optimized: boolean;
		length_context: str_len_context;
		temp_limit: char_range ): string_descriptor *) ;  (* forward declared *)

var
  desc_ref: ^string_descriptor;

begin
  optimized := false;
  with exp^ do begin

    (* If the RESULT field of the expression tuple is non-NIL then the
       expression has been previously fetched and a string descriptor
       saved on the heap. *)

    if result <> nil then begin
      desc_ref := ptr ( result );
      fetch_string := desc_ref^;
      assert ( length_context in [no_length, fetch_string.len_context] );
    end
    else fetch_string := get_string_descriptor ( exp, length_context, temp_limit );

    (* If this is the first of multiple uses of this expression then make
       a copy of the string descriptor on the heap and store a pointer to
       it in the RESULT field.  Increment the usage counts of any registers
       involved by the remaining uses of this expression tuple. *)

    if ( usage_count > 1 ) andif ( result = nil ) then begin
      new ( desc_ref );
      desc_ref^ := fetch_string;
      use_desc ( fetch_string.base_addr, usage_count - 1 );
      if fetch_string.len_context <> no_length then
	use_desc ( fetch_string.len_addr, usage_count - 1 );
      if fetch_string.text_valid then
	use_desc ( fetch_string.text_addr, usage_count - 1 );
      result := ptr ( desc_ref );
    end;

    (* Decrement the usage count for the expression.  If it reaches zero
       and there is a copy of the string descriptor on the heap, dispose it. *)

    dec_expr_usage (exp)
  end (* with *);
end (* fetch_string *);
$PAGE fetchstring
(* FETCHSTRING is just a wrapper for FETCH_STRING and is called whenever
   no context tuple is present or required, especially from other modules
   in the code generator. *)

function fetchstring (*
		exp: expr;
		length_context: str_len_context;
		temp_limit: char_range )
			: string_descriptor *); (* forward declared *)

var
  optimized: boolean;

begin
  fetchstring := fetch_string ( exp, nil, optimized, length_context, temp_limit );
end (* fetchstring *);
$PAGE do_translation
(* DO_TRANSLATION performs any upper or lower case conversions pending
   for a string.  Parameter DESC is a string descriptor for the 
   string.  The function return value is a string descriptor for the
   translated string.  The LEN_CONTEXT field of DESC must not be set
   to NO_LENGTH, since the length of the input string is required
   for the translation.  Parameter DESC is freed by this routine.
   The kind of the string, varying or nonvarying, is preserved when
   a translation occurs. *)

function do_translation ( var desc: string_descriptor;  force_to_temp: boolean ): string_descriptor;

  (* FREE_RHS is a procedure which is passed as an actual parameter
     to MOVE_STRING.  It frees the rhs string before the register save
     which preceeds the string instruction.  *)

  procedure free_rhs;
    begin
      free_string ( desc );
    end;

var
  dest_desc: string_descriptor;
  is_varying: boolean;

begin
  with desc do begin

    (* If no translation is required and we're not forcing a copy then just
       return the parameter itself. *)

    if ( trans_code = no_trans ) and not force_to_temp then
      do_translation := desc
    else begin

      assert ( len_context <> no_length );
      is_varying := ( type_desc.kind = strings ) andif
		    ( type_desc.str_kind = varying );
      init_string_descriptor ( dest_desc );
      if type_desc.str_flex then begin	(* just use dynamic temp *)
	if is_varying then begin
	  dest_desc.base_addr := str_varying_dynamic_temp ( len_addr );
	  dest_desc.len_addr := duplicate_desc ( dest_desc.base_addr );
	end
	else begin
	  dest_desc.base_addr := get_temp ( duplicate_desc ( len_addr ), false (* byte cnt *) );
	  dest_desc.len_addr := duplicate_desc ( len_addr );
	end
      end (* if flex *)
      else begin
	if is_varying
	  then dest_desc.base_addr := get_temp ( int_desc ( max_len + str_lw_bytes, size_word, false ),
				       false (* byte cnt *) )
	  else dest_desc.base_addr := get_temp ( int_desc ( max_len, size_word, false ), false (* byte cnt *) );
	if is_varying
	  then dest_desc.len_addr := duplicate_desc ( dest_desc.base_addr )
	  else dest_desc.len_addr := duplicate_desc ( len_addr );
      end;
      if is_varying
	then str_desc_size ( dest_desc.base_addr, size_word )
	else str_desc_size ( dest_desc.base_addr, size_long );
      str_desc_size ( dest_desc.len_addr, size_word );
      dest_desc.type_desc := type_desc;

      (* If a varying string, move the length word. *)

      if is_varying then
	gen_mm ( move_opc, len_addr, dest_desc.len_addr );

      move_string ( dest_desc, desc, false, free_rhs, false, true );
      do_translation := dest_desc;
    end (* non-trivial case *)
  end (* with *)
end (* do_translation *);
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

public function fetchtranslated ( exp: expr;  context: str_len_context ): string_descriptor;


begin

  fetchtranslated := fetchstring ( exp, context, no_limit );      (* fetch the untranslated string *)

  fetchtranslated := do_translation ( fetchtranslated, false );        (* translate if necessary *)
end (* fetchtranslated *);
$PAGE str_function
(* STR_FUNCTION is a utility for SEARCH, VERIFY, and INDEX, returning an
   operand descriptor for the result of the function call. *)

function str_function ( exp: expr; alternate_desc: op_desc ): op_desc;

var
  skip_label: def;

begin
  str_function := descriptors[postincrement_mode];
  str_function.reg := sp;
  str_function.value_size := size_word;
  if upperbound ( exp^.operand ) = 3 then begin
    skip_label := def_create ( local_def );
    if alternate_desc.value_size > size_word then begin

      (* Cannot leave result on the stack since we have to coerce it. *)

      str_function := coerce ( str_function, [dreg_mode], dreg_mode, [alternate_desc.value_size], false );
      str_function := copy_dreg ( str_function );

      (* Assume that the conversion set the condition codes as well. *)

      gen_bcc ( ne_cc, skip_label );
      gen_mm ( move_opc, alternate_desc, str_function );
    end
    else begin

      (* Both operands are word-sized, so we leave the result on the stack. *)

      assert ( alternate_desc.value_size = size_word );
      str_function.mode := indirect_mode;
      gen_m ( tst_opc, str_function );
      gen_bcc ( ne_cc, skip_label );
      gen_mm ( move_opc, alternate_desc, str_function );
      str_function.mode := postincrement_mode;
    end;
    gen_def ( code_area, skip_label );
    free_desc ( alternate_desc );
  end;
end (* str_function *);
$PAGE do_index_op
(* STR_INDEX_OP generates code for intrinsic function INDEX.  Parameter
   INDEX_OP_EXPR is the INDEX_OP expression tuple.  An address
   descriptor is returned for the INDEX return value.  *)

public function str_index_op ( index_op_expr: expr ): op_desc;

var
  str1_desc, str2_desc: string_descriptor;
  str1_format, str2_format: string_descriptor_format;
  alternate_desc: op_desc;

begin
  with index_op_expr^ do begin
    str1_desc := fetchtranslated ( operand[1], actual_length );
    str2_desc := fetchtranslated ( operand[2], actual_length );
    if upperbound ( operand ) = 3 then
      alternate_desc := fetch ( operand[3], nonstack_modes, immediate_mode, word_min, true );
    str2_format := string_rt_parameter ( str2_desc );
    free_string ( str2_desc );
    str1_format := string_rt_parameter ( str1_desc );
    free_string ( str1_desc );
    gen_rt ( index_routines [str1_format, str2_format] );
    str_index_op := str_function ( index_op_expr, alternate_desc );
  end;
end (* str_index_op *);
$PAGE str_search_verify
(* STR_SEARCH_VERIFY generates code for intrinsic functions SEARCH and
   VERIFY.  Calls to runtime routines are generated for either routine.
   Parameter EXP is the SEARCH_OP or VERIFY_OP expression tuple.  An 
   address descriptor for the function result is returned.  *)

public function str_search_verify ( exp: expr ): op_desc;

var
  str_addr: string_descriptor;
  str_format: string_descriptor_format;
  set_addr: set_desc;
  alternate_desc: op_desc;
  skip_label: def;

begin
  with exp^ do begin
    str_addr := fetchstring ( operand[1], actual_length, no_limit );
    if str_addr.trans_code = lower_trans then
      str_addr := do_translation ( str_addr, false );
    set_addr := set_fetch ( operand[2], operand[2]^.desc.set_lwb, operand[2]^.desc.set_length, no_preference, false );
    if upperbound ( operand ) = 3 then
      alternate_desc := fetch ( operand[3], nonstack_modes, immediate_mode, word_min, false );
    if set_addr.nargs = 0 then begin

      (* Special case search/verify with a known null set parameter. *)

      if opcode = search_op then begin

	(* By definition:

	     SEARCH (STRING, []) = 0, and
	     SEARCH (STRING, [], X) = X		*)

	if upperbound ( operand ) = 3
	  then str_search_verify := alternate_desc
	  else str_search_verify := int_desc ( 0, size_word, true );
      end
      else begin

	(* Verify is somewhat more complicated. By definition:

	     VERIFY (STRING, []) = 0 if length (string) = 0, and
	     VERIFY (STRING, []) = 1 if length (string) > 0, and
	     VERIFY (STRING, [], X) = X if length (string) = 0, and
	     VERIFY (STRING, [], X) = 1 if length (string) > 0.	*)

	str_search_verify := pop_word;
	str_search_verify.mode := predecrement_mode;
	if upperbound ( operand ) = 3 then begin
	  assert ( alternate_desc.value_size >= size_word );
	  str_search_verify.value_size := alternate_desc.value_size;
	  gen_im ( move_opc, 1, str_search_verify );
	  gen_m ( tst_opc, str_addr.len_addr );
	  skip_label := def_create ( local_def );
	  gen_bcc ( ne_cc, skip_label );
	  str_search_verify.mode := indirect_mode;
	  gen_mm ( move_opc, alternate_desc, str_search_verify );
	  gen_def ( code_area, skip_label );
	  str_search_verify.mode := postincrement_mode;
	  free_desc ( alternate_desc );
	end
	else begin
	  gen_im ( move_opc, 1, str_search_verify );
	  gen_m ( tst_opc, str_addr.len_addr );
	  skip_label := def_create ( local_def );
	  gen_bcc ( ne_cc, skip_label );
	  str_search_verify.mode := indirect_mode;
	  gen_m ( clr_opc, str_search_verify );
	  str_search_verify.mode := postincrement_mode;
	end;
      end;
      free_string ( str_addr );
    end (* null set case *)
    else begin
      if set_addr.nargs = 2 then begin
	push ( set_addr.arg[1], size_long );
	push ( set_addr.arg[2], size_long );
      end
      else begin	(* l-format set *)
	pusha ( set_addr.arg[1] );
	push ( set_addr.arg[2], size_long );
	push ( set_addr.arg[3], size_long );
      end;
      str_format := string_rt_parameter ( str_addr );
      gen_rt ( search_verify_routines [opcode = verify_op, str_format, set_addr.nargs = 3, str_addr.trans_code] );
      free_string ( str_addr );
      str_search_verify := str_function ( exp, alternate_desc );
    end;
  end (* with *);
end (* str_search_verify *);
$PAGE str_compare
(* STR_COMPARE compares two strings.  Parameter EXP is the string
   comparison expression tuple.  This routine merely emits a string
   comparison instruction.  Execution of the instruction will set
   the condition codes to the result of the comparison. It is the responsibility
   of the caller to utilize this result. *)

public procedure str_compare ( lhs_expr, rhs_expr: expr );

var
  lhs_desc, rhs_desc, temp_desc: string_descriptor;
  lhs_format, rhs_format: string_descriptor_format;

begin
  lhs_desc := fetchtranslated ( lhs_expr, actual_length );
  rhs_desc := fetchtranslated ( rhs_expr, actual_length );
  rhs_format := string_rt_parameter ( rhs_desc );
  free_string ( rhs_desc );
  lhs_format := string_rt_parameter ( lhs_desc );
  free_string ( lhs_desc );
  gen_rt ( comparison_routines[lhs_format, rhs_format] );
end (* str_compare *);
$PAGE update_length
(* UPDATE_LENGTH updates the length word of a varying string.  Parameter
   LHS_DESC is a string descriptor for the (varying) string whose length
   word is to be updated.  Parameter RHS_DESC is a string descriptor for
   the string whose length is to be used in the update.  Note that the
   LHS_DESC may have its TEXT_ADDR field set, and, thus LHS_DESC must
   be a VAR parameter.  *)

procedure update_length ( var lhs_desc: string_descriptor; rhs_desc: string_descriptor );

var
  length_addr, length_word_addr: op_desc;
  dreg: data_regs;
  length_valid: boolean;
  skip_label: def;
  rhs_len, lhs_len: char_range;

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

    length_valid := false; (* in case no special cases apply *)
    if rhs_desc.type_desc.kind = chars then begin	(* Case 1: Rhs is a char *)
      length_addr := int_desc ( 1, size_word, false );
      length_valid := true;
    end

    else if not ( type_desc.str_flex or	(* if neither flex flag set, then *)
		 rhs_desc.type_desc.str_flex ) then begin	(* check for cases 2 and 3 *)
      
      if type_desc.str_length >= rhs_desc.type_desc.str_length then begin	(* Case 2 *)
	length_addr := duplicate_desc ( rhs_desc.len_addr );
	length_valid := true;
      end

      else if ( rhs_desc.type_desc.str_kind = nonvarying ) andif	(* Case 3 *)
	      ( rhs_desc.type_desc.str_length >= type_desc.str_length ) then begin
	length_addr := int_desc ( type_desc.str_length, size_word, false );
	length_valid := true;
      end;
    end;

    (* if any of the above special cases applied, the we simply move
       the length determined above to the lhs length word.  *)


    if length_valid then begin
      gen_mm ( move_opc, length_addr, length_word_addr );
      free_desc ( length_addr );
    end

    (* None of the special cases applied - we must emit code to compute
       at run time:  MIN ( upperbound ( lhs ), length ( rhs ) )  *)

    else begin

      (* In some cases both the upperbound ( lhs ) and length ( rhs )
	 may be constant ( e. g., a nonvarying string passed as a flex
	 varying string.  In that case use the minimum. *)

      if aconstp ( len_addr, lhs_len ) andif aconstp ( rhs_desc.len_addr, rhs_len ) then begin
	gen_mm ( move_opc, int_desc ( min ( lhs_len, rhs_len ), size_word, false ), length_word_addr );
      end
      else begin
	assert ( rhs_desc.len_addr.value_size = size_word );
	assert ( len_addr.value_size = size_word );
	dreg := get_dreg ();
	gen_mr ( move_opc, rhs_desc.len_addr, dreg );	(* load rhs len *)
	gen_mr ( cmp_opc, len_addr, dreg );	(* compare with upb length *)
	skip_label := def_create ( local_def );
	gen_bcc ( le_cc, skip_label );
	gen_mr ( move_opc, len_addr, dreg );
	gen_def ( code_area, skip_label );
	gen_rm ( move_opc, dreg, length_word_addr );
	free_reg ( dreg );
      end
    end;

    free_desc ( length_word_addr );

  end  (* with *) ;
end (* update_length *);
$PAGE str_assignment
(* STR_ASSIGNMENT performs a string assignment.  NODE is the ASSIGN_OP
   tuple for the assignment.  *)

public procedure str_assignment ( node: tuple );

const
  max_const_len = 10;	(* Max length of constant string moved inline. *)

var
  rhs_desc: string_descriptor;
  lhs_desc: string_descriptor;
  case_code: str_translation;

  (* The following proc is passed as a parameter to MOVE_STRING
     to do the desired frees. *)

  procedure free_both_sides;
    begin
      free_string ( rhs_desc );
      free_string ( lhs_desc );
    end;
$PAGE move_constant - in str_assignment
(* MOVE_CONSTANT moves a constant string inline. *)

procedure move_constant ( cst_string: string[*] );

var
  next_increment, next_char, move_len, actual_len: char_range;
  str_chars: packed array[1..max_const_len+str_lw_bytes] of char;
  areg: addr_regs;
  i: integer;
  temp_desc1, temp_desc2: op_desc;

  function string_ival ( start, len: char_range ): machine_word;

  var i: char_range;

  begin
    string_ival := 0;
    for i := start to start + len - 1 do
      string_ival := string_ival * #H100 + ord ( str_chars [ i ] );
  end;

begin
  with node^ do begin

    (* First, calculate the number of characters to be moved, constrained
       by the type and length of target (possibly padding with a terminal
       blank for more efficient code). *)

    actual_len := length ( cst_string );
    if lhs^.desc.str_kind = varying then begin
      if lhs^.desc.str_length < actual_len then begin
	actual_len := lhs^.desc.str_length;
	move_len := actual_len;
      end
      else if odd ( actual_len )
	then move_len := succ ( actual_len )
	else move_len := actual_len;
      move_len := move_len + str_lw_bytes;	(* move length word, also *)
      str_chars := chr ( actual_len div #H100 ) ||
		   chr ( actual_len mod #H100 ) ||
		   cst_string;
    end
    else begin
      move_len := lhs^.desc.str_length;
      if odd ( move_len ) then
	move_len := succ ( move_len );
      actual_len := move_len;
      str_chars := cst_string;
    end;
    if case_code = upper_trans then
      str_chars := uppercase ( str_chars )
    else if case_code = lower_trans then
      str_chars := lowercase ( str_chars );

    (* Fetch the target of the assignment. *)

    lhs_desc := fetchstring ( lhs, max_length, no_limit );

    (* Special case VERY short moves. *)

    temp_desc1 := skip_desc_word ( lhs_desc );
    temp_desc1.extended_size := no_size;
    case move_len of

      0: ;	(* Could possibly happen *)

      1: begin
	temp_desc1.value_size := size_byte;
	gen_im ( move_opc, string_ival ( 1, 1 ), temp_desc1 );
      end;

      2, 3: begin
	temp_desc1.value_size := size_word;
	gen_im ( move_opc, string_ival ( 1, 2 ), temp_desc1 );
	if move_len = 3 then begin
	  temp_desc1 := increment_addr ( temp_desc1, 2 );
	  temp_desc1.value_size := size_byte;
	  gen_im ( move_opc, string_ival ( 3, 1 ), temp_desc1 );
	end;
      end;

      4: begin
	temp_desc1.value_size := size_long;
	gen_im ( move_opc, string_ival ( 1, 4 ), temp_desc1 );
      end;

      others: begin
	free_desc ( temp_desc1 );
	areg := get_areg ();
	temp_desc2 := reg_desc ( areg, size_long, true );
	gen_mm ( lea_opc, temp_desc1, temp_desc2 );
	temp_desc1 := temp_desc2;
	temp_desc1.value_size := size_long;
	temp_desc1.extended_size := no_size;
	temp_desc1.mode := postincrement_mode;
	next_char := 1;
	while next_char <= move_len do begin
	  if move_len = next_char then begin	(* move last character *)
	    temp_desc1.value_size := size_byte;
	    next_increment := 1;
	  end
	  else if move_len - next_char < 3 then begin
	    temp_desc1.value_size := size_word;
	    next_increment := 2;
	  end
	  else begin
	    next_increment := 4;
	  end;
	  gen_im ( move_opc, string_ival ( next_char, next_increment ), temp_desc1 );
	  next_char := next_char + next_increment;
	end (* while *);
      end (* others *)

    end (* case *);
    free_desc ( temp_desc1 );
    free_string ( lhs_desc );
  end (* with *);
end (* move_constant *);
$PAGE move_string_inline
(* MOVE_STRING_INLINE attempts to emit an inline assignment, returning a Boolean
   value of TRUE if and only if it has done so. *)

function move_string_inline: boolean;

var
  source_desc, dest_desc, temp_desc1, temp_desc2, counter: op_desc;
  base_expr, temp_expr: expr;
  loop_label: def;
  last_word, done: boolean;
  register_mask: machine_word;

  function use_movem: boolean;

  begin
    use_movem := false;
  end;

begin
  move_string_inline := false;
  with node^ do begin

    (* The most obvious case is the assignment of a short constant string,
       provided the following conditions are met:

	 (1) The target of the assignment must not be flex.

	 (2) The target string must not be a substring since we may move the
	     length word along with characters.

	 (3) The target must be varying or, if non-varying, have a maximum
	     length <= MAX_CONST_LEN.

	 (4) The constant to be moved must be <= MAX_CONST_LEN or <= the
	     length of the target if it is nonvarying.

       First we track down the actual right hand side, ignoring
       string conversion tuples.	*)

    str_case_ops ( rhs, base_expr, case_code, false );
    if ( not lhs^.desc.str_flex ) andif
       ( lhs^.opcode <> substr_ref ) andif
       ( base_expr^.opcode = cst_ref ) andif
       ( ( lhs^.desc.str_kind = varying ) orif
	 ( lhs^.desc.str_length <= max_const_len ) ) andif
       ( ( base_expr^.cst_val.kind = scalar_cst ) orif
	 ( base_expr^.cst_val.kind = string_cst ) andif
	 ( ( upperbound (base_expr^.cst_val.valp^.str_val ) <= max_const_len ) orif
	   ( lhs^.desc.str_length <= max_const_len ) orif
	   ( lhs^.desc.str_kind = nonvarying ) andif
	   ( upperbound ( base_expr^.cst_val.valp^.str_val ) <= lhs^.desc.str_length ) ) ) then begin

	(* Since we are eliminating the fetch of the source string in order
	   to avoid an unnecessary allocation in the constant area, we must
	   check the usage counts of all the tuples passed over by 
	   GET_CASE_OPS, "using" each tuple in the chain. *)

	temp_expr := rhs;
	loop
	  dec_expr_usage ( temp_expr );
	exit if temp_expr = base_expr;
	  temp_expr := temp_expr^.operand[ 1 ];
	end;
	if base_expr^.cst_val.kind = scalar_cst
	  then move_constant ( chr ( base_expr^.cst_val.ival ) )
	  else move_constant ( base_expr^.cst_val.valp^.str_val );
      move_string_inline := true;
    end (* constant case *)
    else begin	(* Must fetch the strings in non-constant cases. *)
      if lhs^.desc.str_flex
	then rhs_desc := fetch_string ( rhs, node, done, actual_length, no_limit )
        else rhs_desc := fetch_string ( rhs, node, done, actual_length, lhs^.desc.str_length );

      (* If somehow the assignment was performed during the fetch of the
	 rhs, as in a recursive concatenation, then return as if the
	 assignment were performed inline. *)

      if done then begin
	free_string ( rhs_desc );
	move_string_inline := true;
	return;
      end;
      lhs_desc := fetchstring ( lhs, max_length, no_limit );

      (* The assignment cannot be emitted inline if any of the following hold:

	 (1) The source requires upper- or lower-casing.

	 (2) The target is a substring.

	 (3) The source is not a directly addressible string (e. g.,
	     it is a substring or concatenation.

	 (4) Either the source or target is flex.	*)

      if ( rhs_desc.trans_code = no_trans ) andif
	 ( lhs^.opcode <> substr_ref ) andif
	 ( base_expr^.opcode <> substr_ref ) andif
	 ( base_expr^.opcode <> cat_op ) andif
	 ( not lhs_desc.type_desc.str_flex ) andif
	 ( ( rhs_desc.type_desc.kind = chars ) orif
	   ( rhs_desc.type_desc.kind = strings ) andif
	   ( not rhs_desc.type_desc.str_flex ) ) then begin

	(* First, check for varying-string := char. *)

	if ( rhs_desc.type_desc.kind = chars ) andif
	   ( lhs_desc.type_desc.str_kind = varying ) then begin

	  (* Stupid, but check for string[0]. *)

	  temp_desc1 := duplicate_desc ( lhs_desc.base_addr );
	  str_desc_size ( temp_desc1, size_word );
	  if lhs_desc.type_desc.str_length = 0 then
	    gen_m ( clr_opc, temp_desc1 )
	  else begin
	    gen_im ( move_opc, 1, lhs_desc.base_addr );
	    temp_desc1 := increment_addr ( temp_desc1, str_lw_bytes );
	    temp_desc1.value_size := rhs_desc.base_addr.value_size;
	    gen_mm ( move_opc, rhs_desc.base_addr, temp_desc1 );
	  end;
	  free_desc ( temp_desc1 );
	  free_both_sides;
	  move_string_inline := true;
	end

	(* Use BLK_MOVE for:

	     non-varying-string := non-varying-string

	   where length (lhs) <= length (rhs).	*)

	else if ( lhs_desc.type_desc.str_kind = nonvarying ) andif
	  ( rhs_desc.type_desc.kind = strings ) andif
	  ( rhs_desc.type_desc.str_kind = nonvarying ) andif
	  ( lhs_desc.max_len <= rhs_desc.max_len ) then begin

	  blk_move ( int_desc ( lhs_desc.max_len, size_word, false ),
		     false,
		     duplicate_desc ( rhs_desc.base_addr ),
		     duplicate_desc ( lhs_desc.base_addr ),
		     temp_desc1,
		     last_word );
	  free_desc ( temp_desc1 );
	  free_both_sides;
	  move_string_inline := true;
	end

	(* For varying-string := varying-string, with the upperbound
	   of the source <= upperbound of the target, use MOVEM or set up
	   an inline loop. *)

	else if ( lhs_desc.type_desc.str_kind = varying ) andif
	  ( rhs_desc.type_desc.kind = strings ) andif
	  ( rhs_desc.type_desc.str_kind = varying ) andif
	  ( rhs_desc.max_len <= lhs_desc.max_len ) then begin

	  (* May be able to use MOVEM for a short, varying string. *)

	  if use_movem then begin
	    temp_desc1 := rhs_desc.base_addr;
	    temp_desc1.value_size := size_long;
	    gen_mm ( movem_opc, int_desc ( register_mask, size_word, false ), temp_desc1 );
	    temp_desc1 := lhs_desc.base_addr;
	    temp_desc1.value_size := size_long;
	    gen_mm ( movem_opc, temp_desc1, int_desc ( register_mask, size_word, false ) );
	  end
	  else begin	(* use loop *)
	    counter := copy_dreg ( duplicate_desc ( rhs_desc.len_addr ) );
	    free_string ( rhs_desc );
	    source_desc := reg_desc ( get_areg (), size_long, true );
	    gen_mm ( lea_opc, rhs_desc.len_addr, source_desc );
	    free_string ( lhs_desc );
	    dest_desc := reg_desc ( get_areg (), size_long, true );
	    gen_mm ( lea_opc, lhs_desc.base_addr, dest_desc );
	    gen_im ( add_opc, 1, counter );
	    gen_im ( lsr_opc, 1, counter );
	    source_desc.value_size := size_word;
	    source_desc.extended_size := no_size;
	    source_desc.mode := postincrement_mode;
	    dest_desc.value_size := size_word;
	    dest_desc.extended_size := no_size;
	    dest_desc.mode := postincrement_mode;
	    loop_label := def_create ( local_def );
	    gen_def ( code_area, loop_label );
	    gen_mm ( move_opc, source_desc, dest_desc );
	    gen_dbcc ( f_cc, counter.reg, loop_label );
	    free_desc ( counter );
	    free_desc ( source_desc );
	    free_desc ( dest_desc );
	  end;
	  move_string_inline := true;
	end;
      end;
    end;
  end (* with *);
end (* move_string_inline *);
$PAGE str_assignment - body
begin
  with node^ do begin

    (* In some cases it is more efficient to move a string inline.
       Do so if possible, otherwise go through a runtime call. *)

    if not move_string_inline then begin

      (* String descriptors for lhs and rhs have been fetched already. *)

      if lhs_desc.type_desc.str_kind = varying then	(* set length word *)
	update_length ( lhs_desc, rhs_desc );
      move_string ( lhs_desc, rhs_desc, lhs_desc.type_desc.str_kind = nonvarying,
		    free_both_sides, false, true );
    end (* out of line move *)
  end (* with *);
end (* str_assignment *);
$PAGE str_parameter
(* STR_PARAMETER generates code to push the argument word(s) for
   a string parameter onto the stack.  Any type coercions required
   for value parameters are done also.  Parameter ACTUAL_EXPR is the
   expression tuple for the actual parameter; FORMAL_TYPE is the type
   node for the parameter;  ADD_PARAMETER is the routine to be called
   to place operand descriptors in the argument list. *)

public procedure str_parameter (
		   actual_expr: expr;
		   parm_kind: sym_kind;
		   formal_type: typ;
		   add_parameter: procedure (op_desc; boolean) );

$PAGE var_parameter - in str_parameter
(* VAR_PARAMETER generates the argument word(s) for a VAR string parameter. *)

procedure var_parameter;

var
  actual_desc: string_descriptor;
  param_desc: op_desc;

begin
  actual_desc := fetchstring ( actual_expr, max_length, no_limit );
  param_desc := skip_desc_word ( actual_desc );
  add_parameter ( param_desc, true );
  if formal_type^.flexible then begin	(* pass the length as well *)
    add_parameter ( coerce ( duplicate_desc ( actual_desc.len_addr ), all_modes, immediate_mode, word_only, false ), false );
  end;
  free_string ( actual_desc );
end (* var_parameter *);
$PAGE value_parameter - in str_parameter
(* VALUE_PARAMETER generates code for string parameters passed by value.
   It both copies the string to a temporary, if required, and generates
   the argument word(s) in the argument block. *)

procedure value_parameter;

var
  actual_desc: string_descriptor;
  uncoerced_expr, final_expr: expr;
  case_code: str_translation;
  actual_kind: string_kind;
  formal_kind: string_kind;
  actual_is_flex: boolean;
  formal_is_flex: boolean;
  actual_bound: char_range;
  formal_bound: char_range;
  no_copy: boolean;
  bound_addr: op_desc;
  temp_length: char_range;
  temp_desc: string_descriptor;
  length_word_addr: op_desc;
  padded: boolean;
  param_addr: op_desc;

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
  str_case_ops ( uncoerced_expr, final_expr, case_code, false );

  if uncoerced_expr^.desc.kind = chars
    then actual_kind := nonvarying
    else actual_kind := uncoerced_expr^.desc.str_kind;
  assert ( formal_type^.kind = strings );
  formal_kind := formal_type^.str_kind;

  actual_is_flex := ( uncoerced_expr^.desc.kind = strings ) andif
                    ( uncoerced_expr^.desc.str_flex );
  formal_is_flex := formal_type^.flexible;

  if uncoerced_expr^.desc.kind = chars
    then actual_bound := 1
    else actual_bound := uncoerced_expr^.desc.str_length;
  formal_bound := formal_type^.str_length;
  actual_desc := fetchstring ( actual_expr, max_length, formal_bound );

  (* Determine whether or not a copy to a temporary is
        required. A copy IS necessary if the actual is a character,
        but is NOT necessary under the following circumstances:
        1. no STRCVT_OP is present, or, 
        2. the formal parameter is a nonvarying flex, or,
        3. the kinds of the formal and actual are the same and one of
           the following holds:
           a. the formal is flex, or, 
           b. neither the formal nor the actual is flex and either
              ( i. ) both are nonvarying and length(actual) >= length(formal), or,
              ( ii. ) both are varying and upperbound(actual) <= 
                      upperbound(formal).                               *)

  no_copy := ( uncoerced_expr^.desc.kind  <> chars ) andif
	     ( final_expr^.opcode <> substr_ref ) andif
             ( ( actual_expr^.opcode <> strcvt_op ) or
               ( formal_is_flex and ( formal_kind = nonvarying ) ) or
             ( ( actual_kind = formal_kind ) and
               ( ( formal_is_flex ) or
                 ( not formal_is_flex and not actual_is_flex and
                   ( ( ( actual_kind = nonvarying ) and
                       ( formal_kind = nonvarying ) and
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
    then actual_desc.len_addr := duplicate_desc ( actual_desc.len_addr )
    else actual_desc.len_addr := skip_desc_word ( actual_desc );
  actual_desc.len_context := actual_length;

  (* If a copy is necessary, then allocate a temp and copy the
     actual parameter to the temp.  *)

  if not no_copy then begin

    (* Allocate and initialize the temporary.  *)

    if formal_is_flex then
      temp_length := actual_desc.max_len
    else if ( not actual_is_flex ) and ( formal_kind = varying )
      then temp_length := min ( formal_bound, actual_bound )
      else temp_length := formal_bound;
  
    init_string_descriptor (temp_desc);
    if actual_is_flex and formal_is_flex then begin
      if formal_kind = varying
        then temp_desc.base_addr := str_varying_dynamic_temp ( actual_desc.len_addr )
	else temp_desc.base_addr := get_temp ( duplicate_desc ( actual_desc.len_addr ), false );
    end
    else if formal_kind = nonvarying
      then temp_desc.base_addr := get_temp ( int_desc ( temp_length, size_word, false ), false )
      else temp_desc.base_addr := get_temp ( int_desc ( temp_length + str_lw_bytes, size_word, false ), false );

    str_desc_size ( temp_desc.base_addr, size_word );
    with temp_desc.type_desc do begin
      base := nil;
      kind := strings;
      str_kind := formal_kind;
      str_flex := false;
      str_length := temp_length;
    end;
    if formal_kind = varying then begin	(* set length *)
      if formal_is_flex then begin
	gen_mm ( move_opc, actual_desc.len_addr, temp_desc.base_addr );
      end
      else begin
	temp_desc.len_addr := int_desc ( temp_length, max ( size_word, int_size ( temp_length, false ) ), false );
	update_length ( temp_desc, actual_desc );
      end;
      temp_desc.len_addr := duplicate_desc ( temp_desc.base_addr );
    end
    else begin
      if formal_is_flex
	then temp_desc.len_addr := duplicate_desc ( actual_desc.len_addr )
	else temp_desc.len_addr := int_desc ( formal_bound, size_word, false );
      str_desc_size ( temp_desc.base_addr, size_long );
    end;

    (* Copy the actual parameter to the temp.  Set ACTUAL_DESC to
       the temp's string descriptor.  *)

    padded := formal_kind = nonvarying;
    move_string ( temp_desc, actual_desc, padded, free_actual, false, true );
    actual_desc := temp_desc;
  end  (* if *) ;

  (* Move the parameters. *)

  if formal_kind = nonvarying then begin
    param_addr := skip_len_words ( actual_desc );

    (* If the formal type is flex and the actual is a varying string,
       then set the bound addr to the actual length of the string
       in case we're just passing the address of the text of the varying
       string in order to avoid a copy to a temp. *)

    if formal_is_flex andif ( actual_kind = varying ) then begin
      free_desc ( bound_addr );
      bound_addr := duplicate_desc ( actual_desc.len_addr );
    end;
  end
  else param_addr := skip_desc_word ( actual_desc );

  free_string ( actual_desc );
  add_parameter ( param_addr, true );
  if formal_is_flex
    then add_parameter ( coerce ( bound_addr, all_modes, immediate_mode, word_only, false ), false )
    else free_desc ( bound_addr );

end (* value_parameter *);
$PAGE str_parameter - body
begin
  if parm_kind = vars
    then var_parameter
    else value_parameter
end (* str_parameter *).
  @•