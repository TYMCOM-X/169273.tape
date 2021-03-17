$TITLE VAXSTR - VAX code generator string related routines.

module vaxstr;

$PAGE includes and forward declarations
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM vaxopc.inc
$SYSTEM vaxcgu.inc
$SYSTEM vaxexp.inc
$SYSTEM vaxutl.inc
$SYSTEM vaxgen.inc
$SYSTEM vaxset.inc
$SYSTEM vaxcll.inc

public function fetchstring ( exp: expr; length_context: str_len_context ): str_desc
  options special(coercions); forward;
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
$PAGE str_temp

(* STR_TEMP allocates a string temporary of length LEN.
   A string descriptor describing the temp is returned.
   If the value is <= 128 allocate a static temp. Otherwise
   if the address descriptor is immediate then allocate 
   a static temp, else allocate a dynamic temp.
   If parameter STR_KIND is VARYING then two additional bytes will
   be allocated for the string length word.  *)

public function str_temp ( DESC : ADDR_DESC;  len: char_range;
 			   str_kind: string_kind): str_desc;

var
  temp_length: char_range;
  temp_len_addr: addr_desc;

begin
  init_str_desc ( str_temp );
  with str_temp do begin
    temp_length := len;
    if aconstp ( desc, temp_length ) orif
       ( len <= 128 ) then begin		(* static temp *)
      if str_kind = varying then temp_length := temp_length + str_lw_bytes;
      temp_len_addr := int_value ( temp_length );
    end
    else begin					(* dynamic temp *)
      if str_kind = varying
	then temp_len_addr := add_constant ( duplicate_addr(desc),
    					     str_lw_bytes, no_preference )
	else temp_len_addr := duplicate_addr ( desc );
    end;
    base_addr := get_temp ( temp_len_addr, vax_word );
    free ( temp_len_addr );
    LEN_ADDR := DUPLICATE_ADDR ( DESC );
    max_len := len;
  end;
end  (* proc str_temp *) ;
$PAGE skip_desc_word

(* SKIP_DESC_WORD takes a string descriptor and returns an address
   descriptor for the location following any bound's word preceeding
   the string (i.e., the start of the text for nonvarying strings and
   the length word address for varying strings).  The TEXT_ADDR field
   of the string descriptor may be set by this routine.  The BYTE_SIZE
   field of the address descriptor returned will be set to VAX_WORD.  *)

public function skip_desc_word ( var desc: str_desc ): addr_desc;

var
  is_varying: boolean;

begin
  with desc do begin
    is_varying := (type_desc.kind = strings) andif (type_desc.str_kind = varying);

    if text_valid then begin	(* if address of text present, then *)
      skip_desc_word := duplicate_addr ( text_addr );	(* back up 2 bytes *)
      if is_varying
	then skip_desc_word := increment_addr ( skip_desc_word, 
				   -str_lw_width div bits_per_byte );
    end

    else begin	(* text addr invalid, use base address *)
      skip_desc_word := duplicate_addr ( base_addr );

      if base_is_bound then begin	(* if flex, increment addr past bounds word *)
	skip_desc_word := increment_addr ( skip_desc_word,
			   flex_str_desc_size div bits_per_byte );

	text_valid := true;	(* set TEXT_ADDR field, since we can calucalte *)
	text_addr := duplicate_addr ( skip_desc_word );	(* address w/o generating code *)
	if is_varying
	  then text_addr := increment_addr ( text_addr, str_lw_width div bits_per_byte );
	text_addr.byte_size := vax_byte;
        
	(* If the string descriptor describes the actual length of a
	   varying string, then update the LEN_ADDR field also.  The new
	   address should describe the same location as the old one, but
	   may be in a more efficient, shorter format.   *)

	if (len_context = actual_length) and (type_desc.str_kind = varying) then begin
	  free ( len_addr );			(* free old length address *)
	  len_addr := duplicate_addr ( skip_desc_word );	(* set it to new, hopefully better, form *)
	  len_addr.byte_size := vax_word;
	end;
      end;
    end;

    skip_desc_word.byte_size := vax_word;

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
      if base_is_bound	(* flex string ? *)
	then increment := flex_str_desc_size div bits_per_byte
 	else increment := 0;

      if (type_desc.kind = strings) andif	(* varying string? *)
	 (type_desc.str_kind = varying)
        then increment := increment + str_lw_width div bits_per_byte;

      skip_len_words := duplicate_addr ( base_addr );
      skip_len_words := increment_addr ( skip_len_words, increment );
      skip_len_words.byte_size := vax_byte;

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
	len_addr := increment_addr ( len_addr, -str_lw_width div bits_per_byte );
	len_addr.byte_size := vax_word;
      end;
    end  (* else *) ;

  end  (* with *) ;
end  (* proc skip_len_words *) ;
$PAGE save_remainder

(* SAVE_REMAINDER is called after a string instruction has been
   generated.  It saves the contents of one of the registers used
   by the string instruction.  This routine must be called after 
   the string instruction is generated but before the registers used
   by the instruction are restored.  The content of the register
   is assumed to be a string address.  Thus, the address descriptor
   returned is for the base address of the string.  *)

function save_remainder ( remainder_reg: registers ): addr_desc;

begin

  (* If the register is not already allocated then simply allocate it
     and return the address '(REG)'.  *)

  if regdesc[ remainder_reg ].uses_remaining = 0 then begin
    regdesc[ remainder_reg ].uses_remaining := 1;
    save_remainder := absolute_reference;
    save_remainder.register := remainder_reg;
  end

  (* Register was previously allocated, move its contents to a temp
     and return the address @<temp addr>.  *)

  else begin
    SAVE_REMAINDER := GET_TEMP ( INT_VALUE(BITS_PER_ADDRESS Div BITS_PER_BYTE),VAX_LONG );
    gen_rm ( movl, remainder_reg, save_remainder );
    save_remainder.indirect := true;
  end;

  save_remainder.byte_size := vax_byte;

end  (* proc save_remainder *) ;
$PAGE save_regs

(* SAVE_REGS saves a set of registers on the stack.  Only registers which
   are between R2 and SP inclusively and are currently allocated will
   be saved.  The set of registers which are in fact saved is returned
   as the function value.  *)

public function save_regs ( reg_set: set_of_registers ): set_of_registers;

var
  mask: 0..177777b;
  i: registers;

begin
  mask := 0;
  save_regs := [];
  for i := r2 to sp do begin
    if ( i in reg_set ) andif 	(* in the specified set, and *)
       ( regdesc[ i ].uses_remaining > 0 ) then begin 	(* currently allocated *)
      mask := mask + ( 2 ** i );
      save_regs := save_regs + [ i ];
    end;
  end;

  if mask <> 0 
    then gen1 ( pushr, typ_int_value ( mask, vax_word ) );

end  (* proc save_regs *) ;
$PAGE restore_regs

(* RESTORE_REGS restores a specified set of registers (parameter
   REG_SET) from the top of the stack.   *)

public procedure restore_regs ( reg_set: set_of_registers );

var
  mask: 0..177777b;
  i: registers;

begin
  mask := 0;
  if reg_set <> [] then begin
    for i := r2 to sp do begin
      if ( i in reg_set ) 	(* in specified set *)
	then mask := mask + ( 2 ** i );
    end;
  end;

  if mask <> 0 
    then gen1 ( popr, typ_int_value ( mask, vax_word ) );
end  (* proc restore_regs *) ;
$PAGE gen_move_char

(* GEN_MOVE_CHAR generates a string move where the right hand side 
   string is either the null string or a single character.  The
   parameters are identical to those of MOVE_STRING except
   parameter PADDED is omitted here.  *)

procedure gen_move_char ( var lhs_desc, rhs_desc: str_desc;
    free_proc: free_procedure; remainder: boolean; var remainder_addr: addr_desc );

var
  rhs_const: boolean;
  rhs_length: char_range;
  source_addr: addr_desc;
  dest_addr: addr_desc;

begin
  rhs_const := aconstp ( rhs_desc.len_addr, rhs_length );

  if rhs_length = 1 then begin
    source_addr := skip_len_words ( rhs_desc );
    free ( source_addr );
  end;

  if (rhs_length = 1) or remainder then begin
    dest_addr := skip_len_words ( lhs_desc );
    if remainder then remainder_addr := duplicate_addr ( dest_addr );
    free ( dest_addr );
  end;

  free_proc;

  if rhs_length = 1
    then store ( source_addr, dest_addr, unsigned_value );

  if remainder then begin
    remainder_addr := increment_addr ( remainder_addr, rhs_length );
    remainder_addr.byte_size := vax_byte;
  end;

end  (* proc gen_move_char *) ;
$PAGE gen_movc3

(* GEN_MOVC3 does a string move using a MOVC3 instruction.  The
   parameters are identical to those of MOVE_STRING.  *)

procedure gen_movc3 ( var lhs_desc, rhs_desc: str_desc; padded: boolean;
    free_proc: free_procedure;  remainder: boolean; var remainder_addr: addr_desc );

var
  length_addr: addr_desc;
  regs_saved: set_of_registers;
  source_addr: addr_desc;
  dest_addr: addr_desc;

begin

  (* Indicate R0 - R5 are used by the current block; determine which
     length to use in the move.  *)

  mark_regs_used ( [r0..r5] );

  if padded
    then length_addr := duplicate_addr ( lhs_desc.len_addr )
  else if (lhs_desc.type_desc.kind = chars) orif
     ( lhs_desc.type_desc.str_kind = nonvarying )	(* must be temp *)
    then length_addr := duplicate_addr ( rhs_desc.len_addr )
  else length_addr := skip_desc_word ( lhs_desc );

  (* Get start address for the text of both strings;  free the
     instruction operands;  save any of R0 - R5 which are currently
     in  use.  *)

  source_addr := skip_len_words ( rhs_desc );
  dest_addr := skip_len_words ( lhs_desc );

  free ( source_addr );
  free ( dest_addr );
  free ( length_addr );
  free_proc;

  regs_saved := save_regs ( [r0..r5] );

  (* Generate the instruction.  *)

  gen3 ( movc3, length_addr, source_addr, dest_addr );

  (* Save the start address of any subsequent concatenating moves
     to the lhs string and restore any regs saved.  *)

  if remainder 
    then remainder_addr := save_remainder ( r3 );
  restore_regs ( regs_saved );

end  (* proc gen_movc3 *) ;
$PAGE gen_movc5

(* GEN_MOVC5 does a string move using a MOVC5 instruction.  The 
   parameters are identical to those of MOVE_STRING.  *)

procedure gen_movc5 ( var lhs_desc, rhs_desc: str_desc; padded: boolean;
    free_proc: free_procedure; remainder: boolean; var remainder_addr: addr_desc );

var
  source_addr: addr_desc;
  dest_addr: addr_desc;
  regs_saved: set_of_registers;

begin
  mark_regs_used ( [r0..r5] );	(* indicate r0-r5 used in current block *)

  source_addr := skip_len_words ( rhs_desc );	(* get start of text *)
  dest_addr := skip_len_words ( lhs_desc );	(* addresses *)

  free ( source_addr );
  free ( dest_addr );

  free_proc;
  regs_saved := save_regs ( [r0..r5] );	(* save if currently in use *)

  gen5 ( movc5, rhs_desc.len_addr, source_addr, fill_addr,
	        lhs_desc.len_addr, dest_addr );

  if remainder
    then remainder_addr := save_remainder ( r3 );
  restore_regs ( regs_saved );

end  (* proc gen_movc5 *) ;
$PAGE gen_movtc

(* GEN_MOVTC does a string move with upper or lower case conversion
   using a MOVTC instruction.  The parameters are identical to those
   of MOVE_STRING.   *)

procedure gen_movtc ( var lhs_desc, rhs_desc: str_desc; padded: boolean;
    free_proc: free_procedure; remainder: boolean; var remainder_addr: addr_desc );

var
  source_len: addr_desc;
  dest_len: addr_desc;
  source_addr: addr_desc;
  dest_addr: addr_desc;
  vector_addr: addr_desc;
  table_addr: addr_desc;
  regs_saved: set_of_registers;

begin
  mark_regs_used ( [r0..r5] );	(* indicate r0-r5 used in current block *)

  (* determine length operands to use in the move.  *)

  if not padded and (lhs_desc.type_desc.kind = strings) andif
     (lhs_desc.type_desc.str_kind = nonvarying) then begin	(* i.e., a string temp *)
    source_len := duplicate_addr ( rhs_desc.len_addr );
    dest_len := duplicate_addr ( source_len );
  end
  else if not padded and (lhs_desc.type_desc.kind = strings) andif
	  (lhs_desc.type_desc.str_kind = varying) then begin
    source_len := skip_desc_word ( lhs_desc );
    dest_len := duplicate_addr ( source_len );
  end
  else begin
    source_len := duplicate_addr ( rhs_desc.len_addr );
    dest_len := duplicate_addr ( lhs_desc.len_addr );
  end;

  (* calculate source and destination addresses *)

  source_addr := skip_len_words ( rhs_desc );
  dest_addr := skip_len_words ( lhs_desc );

  (* Upper/lower case conversion tables are accessed via a transfer
     vector in a position independent manner.  The runtime symbols
     address the transfer vector.  The transfer vector entry is the
     self-relative byte displacement to the start of the table.  *)

  if rhs_desc.trans_code = lower_trans
    then vector_addr := make_rt_addr ( rt_lower_table )
  else if rhs_desc.trans_code = upper_trans
    then vector_addr := make_rt_addr ( rt_upper_table )
  else begin
    assert ( false );
  end;
  vector_addr.byte_size := vax_long;
  gen_mr ( movl, vector_addr, r0 );	(* move self-relative displacement to r0 *)
  
  table_addr := vector_addr;	(* index vector address by displacement *)
  table_addr.index := r0;
  table_addr.byte_size := vax_byte;

  (* Free the instruction operands before the register save, then
     generate the instruction.  *)
  free ( source_addr );
  free ( dest_addr );
  free ( source_len );
  free ( dest_len );
  free ( table_addr );
  free_proc;
  regs_saved := save_regs ( [r0..r5] );

  gen6 ( movtc, source_len, source_addr, fill_addr, table_addr,
	        dest_len, dest_addr );

  (* Save the start address within the lhs string of any subsequent
     concatenation moves, if specified; restore any saved regs. *)

  if remainder
    then remainder_addr := save_remainder ( r5 );
  restore_regs ( regs_saved );

end (* proc gen_movtc *) ;
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
   the address of the location one byte beyond the final destination
   string should be saved (for use in subsequent string moves, e.g.,
   concatenations).  If parameter REMAINDER is true then parameter
   REMAINDER_ADDR will be set to the address of the byte following
   the destination string.  The LEN_ADDR field of the string descriptors 
   is assumed to be set to the maximum possible length for the lhs string,
   and to the actual length for the rhs string.  *)

public procedure move_string ( var lhs_desc: str_desc; var rhs_desc: str_desc; padded: boolean;
    free_proc: free_procedure; remainder: boolean;  var remainder_addr: addr_desc );

var
  lhs_const: boolean;
  rhs_const: boolean;
  lhs_length: char_range;
  rhs_length: char_range;
  use_movb: boolean;
  use_movc3: boolean;

begin

  (* Set flags indicating whether the length of the strings are known
     at compile time.  Set flags indicating the type of move instruction
     to use.  *)

  lhs_const := aconstp ( lhs_desc.len_addr, lhs_length );
  rhs_const := aconstp ( rhs_desc.len_addr, rhs_length );

  use_movb := ( rhs_const ) andif ( rhs_length <= 1 ) and	(* null string or *)
	      ( not padded ) and	(* single char to move *)
	      ( rhs_desc.trans_code = no_trans );

  use_movc3 := ( not use_movb ) and
	       ( rhs_desc.trans_code = no_trans ) and
	       ( ( not padded ) or
	         ( ( rhs_const and lhs_const ) andif
		   ( rngth >= lhs_length ) ) );

  (* Generate the proper move instruction.  *)

  if use_movb
    then gen_move_char ( lhs_desc, rhs_desc, free_proc,
			 remainder, remainder_addr )
  else if use_movc3
    then gen_movc3 ( lhs_desc, rhs_desc, padded, free_proc, remainder, remainder_addr )
  else if ( rhs_desc.trans_code = no_trans )
    then gen_movc5 ( lhs_desc, rhs_desc, padded, free_proc, remainder, remainder_addr )
  else gen_movtc ( lhs_desc, rhs_desc, padded, free_proc, remainder, remainder_addr );
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
  const_len: char_range;
  reg: registers;
  total_len_addr: addr_desc;
  new_base_addr: addr_desc;
  const_addr: addr_desc;


  (* The following procedure is passed as a parameter to MOVE_STRING
     to free the desired strings before the registers used by the
     instruction are saved.  *)

  procedure concat_free;
    begin
      free_string ( op_descs^[ i ] );	(* free rhs string *)
      FREE ( CONCAT_DESC.BASE_ADDR );	(* Free address *)
      If CONCAT_DESC.TEXT_VALID
	Then Free ( CONCAT_DESC.TEXT_ADDR );
      concat_desc.text_valid := false;
    end;

begin
  with exp^ do begin

    (* We first fetch each operand, so we can determine an upperbound
       on the size of the result.  We then allocate the temp and
       initialize the string descriptor for the result of the
       concatenation.  *)

    new ( op_descs, upperbound (operand) );
    temp_length := 0;
    LEN_LOADED := False;	(* All lengths constatn, so far *)
    CONST_PART := 0;		(* Lengths of all of the constant parts *)

    (* For each operand calculate the actal length, either at runtime
       for dynamic strings, or constants for the rest. If dynamic
       perform the add, else keep a running total and add it in at the
       end of the loop. *)

    For I := 1 to Upperbound (OPERAND) Do Begin
      (* Fetch the operand and store into the array for later reference *)
      OP_DESCS^[I] := FETCHSTRING ( OPERAND[I] , ACTUAL_LENGTH );

      If CASE_CODE <> NO_TRANS
	Then OP_DESCS^[I].TRANS_CODE := CASE_CODE;

      (* Calculate the maximum length of the concatenated string *)

      TEMP_LENGTH := TEMP_LENGTH + OP_DESCS^[I].MAX_LEN;

      (* If the string is of constant length add  in the length, else runtim *)

      If ACONSTP ( OP_DESCS^[I].LEN_ADDR , CONST_LEN )
	THen CONST_PART := CONST_PART + CONST_LEN
      Else Begin
	Assert ( OP_DESCS^[i].LEN_ADDR.BYTE_SIZE = VAX_WORD );
	If LEN_LOADED		(* Notthe first dynamic string *)
	  Then GEN2 ( ADDW2, OP_DESCS^[i].LEN_ADDR, TOTAL_LEN_ADDR )
	Else Begin
	  TOTAL_LEN_ADDR := TYP_REG_ADDR( GET_VAX_REG ( VAX_WORD ), VAX_WORD );
	  STORE ( OP_DESCS^[i].LEN_ADDR, TOTAL_LEN_ADDR, UNSIGNED_VALUE );
	  LEN_LOADED := True		(* From now on just add in thelengths *)
	End;
      End
    End;		(* The for loop *)

    (* If there was a dynamic part add in the length of the constatn part.
       If no dynamic part then the length is the length of the constant part *)

    If LEN_LOADED
      Then Begin
	If CONST_PART <> 0	(* Don't waste time adding zero *)
	  Then GEN2 ( ADDW2 , TYP_INT_VALUE ( CONST_PART , VAX_WORD ) , TOTAL_LEN_ADDR )
      End
    Else TOTAL_LEN_ADDR := TYP_INT_VALUE ( CONST_PART , VAX_WORD );

    (* Now get the string for the temp *)

    CONCAT_DESC := STR_TEMP ( TOTAL_LEN_ADDR , TEMP_LENGTH, NONVARYING );
    Free ( TOTAL_LEN_ADDR );
    CONCAT_DESC.TYPE_DESC := DESC;	(* Initialize desc feild *)

    (* Save the base address cause the move strings below will change it *)

    RESULT_BASE := DUPLICATE_ADDR ( CONCAT_DESC.BASE_ADDR );

    (* Now move the strings into the resultant temp *)

    For I := 1 to Upperbound (OPERAND) Do Begin
      MOVE_STRING ( CONCAT_DESC, OP_DESCS^[i], False, CONCAT_FREE, I<>Upperbound (OPERAND), NEW_BASE_ADDR );
      CONCAT_DESC.BASE_ADDR := NEW_BASE_ADDR	(* Update next moves dest addr *)
    End;

    (* Resethe base addr of the result *)

    CONCAT_DESC.BASE_ADDR := RESULT_BASE;

    (* Clean up the temp used to hold the operands *)

     Dispose ( OP_DESCS );

  end  (* with *) ;
end  (* proc concat_desc *) ;
$PAGE substr_desc

(* SUBSTR_DESC generates code for SUBSTR_REF expression tuples.
   Parameter EXP is the SUBSTR_REF tuple.  Parameter CASE_CODE indicates
   any upper or lower case conversions inherited from higher nodes in the
   expression tree.  A string descriptor describing the resulting substring
   is returned.  *)

function substr_desc ( exp: expr; case_code: str_translation ): str_desc;

var
  base_desc: str_desc;
  index_addr: addr_desc;
  length_value: char_range;
  start_addr: addr_desc;

begin
  with exp^ do begin
    init_str_desc ( substr_desc );	(* initialize the result str_desc *)
    
    (* If the substring's length is known to be one, then we calculate
       the substrings base address using the array indexing routine.  *)

    if ( substr_length <> nil ) andif
       ( substr_length^.opcode = cst_ref ) andif
       ( substr_length^.cst_val.ival = 1 ) then begin
      with substr_desc do begin
        base_addr := array_addr ( exp );
        len_addr := typ_int_value ( 1, vax_word );
        max_len := 1;
        type_desc := exp^.desc;
        trans_code := case_code;
      end;
    end

    (* Substring is NOT known to be of length one.  We simply fetch
       the substring length.  The substring start address is found by
       fetching the base string, and adding (substring_index - 1).  *)

    else begin
      with substr_desc do begin
        base_desc := fetchstring ( base_string, actual_length );	(* fetch str_desc for base string *)
	index_addr := fetch_fullword ( substr_index );	(* fetch start index *)
	len_addr := fetch ( substr_length , no_preference );
	len_addr := cvt_word ( len_addr, unsigned_value );

	if aconstp ( len_addr, length_value )	(* set max length *)
	  then max_len := length_value
	  else max_len := base_desc.max_len;

	start_addr := skip_len_words ( base_desc );	(* calc substr start address *)
	base_addr := offset_addr ( start_addr, index_addr );
	base_addr := increment_addr ( base_addr, -1 );

	type_desc := exp^.desc;	(* set type info *)

	if case_code = no_trans 	(* set case conversion code *)
	  then trans_code := base_desc.trans_code
	  else trans_code := case_code;

      end  (* with *) ;
    
      free_string ( base_desc );	(* free base string *)

    end  (* else *) ;

  end  (* with *) ;
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
   address descriptor for the maximum length of the string.  The
   BYTE_SIZE field of the address descriptor returned will be set
   to VAX_WORD.  *)

function get_max_length ( str_expr: expr; str_addr: addr_desc ): addr_desc;

begin
  with str_expr^ do begin
    if desc.kind = chars	(* if type is char, length is one *)
      then get_max_length := typ_int_value ( 1, vax_word )
    else if desc.str_flex	(* if flex string, *)
      then get_max_length := upper_bound ( str_expr, str_addr )	(* then get upper bound *)
    else get_max_length := typ_int_value ( desc.str_length, vax_word );
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
   for the actual length of the string.  The BYTE_SIZE field of the
   address descriptor returned will be set to VAX_WORD.  *)

function get_actual_length ( str_expr: expr; str_addr: addr_desc ): addr_desc;

begin
  with str_expr^ do begin
    if desc.kind = chars 	(* if char, length is one *)
      then get_actual_length := typ_int_value ( 1, vax_word )
    else if desc.str_kind = varying then begin	(* if varying string, return length word address *)
      get_actual_length := duplicate_addr ( str_addr );
      if dynamic_flex ( str_expr )	(* if flex, skip bounds word *)
	then get_actual_length := increment_addr ( get_actual_length,
				flex_str_desc_size div bits_per_byte );
      get_actual_length.byte_size := vax_word;
    end
    else if desc.str_flex	(* if non-varying flex, then get bounds word *)
      then get_actual_length := upper_bound ( str_expr, str_addr )
    else get_actual_length := typ_int_value ( desc.str_length, vax_word );
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
  while base_expr^.opcode = strcvt_op do	(* skip string convert ops *)
    base_expr := base_expr^.operand[ 1 ];
  case_code := no_trans;

  if ( base_expr^.opcode = lwc_op ) orif	(* if case op is present, *)
     ( base_expr^.opcode = upc_op ) then begin	(* then set parameter CASE_CODE *)
    if base_expr^.opcode = lwc_op		(* to reflect which conversion *)
      then case_code := lower_trans
      else case_code := upper_trans;

    repeat	(* skip any subsequent case or type *)
      base_expr := base_expr^.operand[ 1 ]	(* conversion operators *)
    until (base_expr^.opcode <> strcvt_op) and
	  (base_expr^.opcode <> lwc_op) and
	  (base_expr^.opcode <> upc_op);
  end;

end  (* proc get_case_ops *) ;
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
  char_value: 0..Maximum ( CHAR_RANGE );
  temp_addr: addr_desc;

begin

  (* we first process any LWC_OPs, UPC_OPs, or STRCVT_OPs.  CASE_CODE
     will be set to indicate any case conversions necessary.  *)

  get_case_ops ( original_exp, exp, case_code );

  (* Get the descriptor.  *)

  case exp^.opcode of

    cat_op:	(* concatenation operator *)
      begin
	get_str_desc := concat_desc ( exp, case_code );	
        if context = no_length then free ( get_str_desc.len_addr );
	get_str_desc.len_context := context;
      end;

    substr_ref:	(* substring operator *)
      begin
	get_str_desc := substr_desc ( exp, case_code );
	if context = no_length then free ( get_str_desc.len_addr );
	get_str_desc.len_context := context;
      end;
    
    (* IN_STR_OP and OUT_STR_OPs are treated somewhat specially here.
       They are in fact TEXT valued rather than string valued.
       The actual string involved is the value of OPERAND[1] of the
       IN_STR_OP or OUT_STR_OP.  However, because the in_str_op or
       out_str_op has the multiple usages associated with it, it is
       convenient for the code generator to treat those ops as if 
       they are string valued.  Thus here we recursively fetch the
       string value of OPERAND[1].  *)

    in_str_op,					(* getstring *)
    out_str_op:					(* putstring *)
      get_str_desc := get_str_desc ( exp^.operand[ 1 ], context );

    others:	(* an entire string reference *)
      begin
	with get_str_desc do begin
	  base_is_bound := ( exp^.desc.kind <> chars ) andif
			   ( dynamic_flex ( exp ) );

          base_addr := do_fetch ( exp , no_preference);

	  (* duplicate logic of vaxexp@mem_fetch, so we can always legally use
	     BASE_ADDR as an address access mode operand (we don't just use mem_fetch
	     as it in turn uses fetch, which plays with usage counts, interfering
	     with our doing that "ourselves" in fetchstring)   *)

	  if aconstp (base_addr, char_value) then
	    base_addr := gen_cst (char_value, vax_byte)
	  else if is_register (base_addr) then begin
	    temp_addr := get_temp (int_value (1), vax_byte);
	    temp_addr.byte_size := vax_byte;
	    free (base_addr);
	    store (base_addr, temp_addr, alignment (exp));
	    base_addr := temp_addr
	  end;

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
	    text_addr := increment_addr ( text_addr, str_lw_width div bits_per_byte );
	    text_addr.byte_size := vax_byte;
	  end;

	  with exp^.desc do begin
	    if kind = chars
	      then max_len := 1
            else if str_flex 
	      then max_len := Maximum ( CHAR_RANGE )
	    else max_len := str_length;
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
      update_usages ( fetchstring.base_addr, usage_count - 1 );
      if fetchstring.len_context <> no_length
	then update_usages ( fetchstring.len_addr, usage_count - 1 );
      if fetchstring.text_valid
	then update_usages ( fetchstring.text_addr, usage_count - 1 );
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

function do_translation ( var desc: str_desc ): str_desc;

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

    if trans_code <> no_trans then begin
      assert ( len_context <> no_length );

      is_varying := ( type_desc.kind = strings ) andif
		    ( type_desc.str_kind = varying );
      if is_varying
        then dest_desc := str_temp ( DESC.LEN_ADDR , max_len, varying )
        else dest_desc := str_temp ( DESC.LEN_ADDR , max_len, nonvarying );

      dest_desc.len_context := len_context;	(* init the temp *)
      dest_desc.type_desc := type_desc;

      if is_varying then begin	(* if varying strings, then copy length word *)
	len_word := skip_desc_word ( desc );
	temp_len_word := skip_desc_word ( dest_desc );
	store ( len_word, temp_len_word, unsigned_value );
	free ( len_word );
      	free ( temp_len_word );
      end;

      move_string ( dest_desc, desc, false, free_rhs, false, dummy_addr );

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

  fetchtranslated := fetchstring ( exp, context );	(* fetch the untranslated string *)

  fetchtranslated := do_translation ( fetchtranslated );	(* translate if necessary *)
end  (* proc fetchtranslated *) ;
$PAGE calc_index

(* CALC_INDEX is a helper for DO_INDEX_OP.  It emits the code to
   calculate the starting index of the matched substring.  Parameters
   SOURCE_LENGTH and OBJECT_LENGTH are address descriptors for  the
   lengths of the source and object strings.  REMAINDER_ADDR is an
   address descriptor for the value computed and left in R2 by the
   MATCHC instruction, i.e., the number of characters remaining in 
   the source string following the matched substring.  RESULT_ADDR
   is an address descriptor for the location in which the result of
   the calculation should be left.  Parameters SOURCE_LENGTH and
   OBJECT_LENGTH  are freed by this routine.
   The calculation performed is:
	source_length - object_length - remainder + 1		*)

procedure calc_index ( source_length, object_length, remainder_addr,
		       result_addr: addr_desc );

var
  source_const, object_const: boolean;
  source_value, object_value: char_range;
  const_part: integer;
  const_addr: addr_desc;

begin

  (* Determine which components of the expression to be evaluated are
     known at compile time and accumulate them.  *)

  source_const := aconstp ( source_length, source_value );
  object_const := aconstp ( object_length, object_value );

  const_part := 1;
  if source_const then const_part := const_part + source_value;
  if object_const then const_part := const_part - object_value;
  const_addr := typ_int_value ( const_part, vax_word );

  (* Evaluate the expression.  *)

  if source_const then begin
    if object_const then begin	(* source and object lengths constant *)
      gen3 ( subw3, remainder_addr, const_addr, result_addr );
    end
    else begin	(* Only source length is constant *)
      gen3 ( addw3, remainder_addr, object_length, result_addr );
      gen3 ( subw3, result_addr, const_addr, result_addr );
    end;
  end

  else begin	(* source is not constant *)
    gen3 ( subw3, remainder_addr, source_length, result_addr );
    if object_const then begin	(* source not constant, object is constant *)
      add2_constant ( result_addr, const_part );
    end
    else begin	(* neither length constant *)
      gen2 ( subw2, object_length, result_addr );
      add2_constant ( result_addr, 1 );
    end;
  end;

  (* Free all the operand address descriptors.  *)

  free ( source_length );
  free ( object_length );

end  (* proc calc_index *) ;
$PAGE do_index_op

(* DO_INDEX_OP generates code for intrinsic function INDEX.  Parameter
   INDEX_OP_EXPR is the INDEX_OP expression tuple.  An address
   descriptor is returned for the INDEX return value.  *)

public function do_index_op ( index_op_expr: expr ): addr_desc;

var
  result_addr: addr_desc;
  source_desc, object_desc: str_desc;
  source_addr, object_addr: addr_desc;
  source_length, object_length: addr_desc;
  regs_saved: set_of_registers;
  reg: registers;
  remainder_addr: addr_desc;
  out_label: def;
  default_value: int_type;

begin
  with index_op_expr^ do begin

    (* Move the default value to a register.  *)

    if upperbound (operand) = 2 
      then result_addr := reg_addr ( load_addr(int_value( 0 ), unsigned_value) )
      else result_addr := reg_addr ( copy_load ( operand[ 3 ] ) );

    (* Fetch the source and object strings and calculate the start of 
       the text of each.  The string descriptors may be freed now, except
       for the string lengths.  The lengths are used to calculate the result
       returned when a match occurs.  *)

    source_desc := fetchtranslated ( operand[ 1 ], actual_length );
    object_desc := fetchtranslated ( operand[ 2 ], actual_length );
    source_addr := skip_len_words ( source_desc );
    object_addr := skip_len_words ( object_desc );
    source_length := duplicate_addr ( source_desc.len_addr );
    object_length := duplicate_addr ( object_desc.len_addr );

    free_string ( source_desc );
    free_string ( object_desc );
    free ( source_addr );
    free ( object_addr );

    (* Save any registers which are currently allocated and are used by
       the MATCHC instruction;  generate the instruction.  *)

    regs_saved := save_regs ( [r0..r3] );
    gen4 ( matchc, object_length, object_addr, source_length, source_addr );

    (* We need the value left in r2 by the MATCHC instruction.  If R2
       was previously in use, move its value to R0.  The move is done
       via PUSHR and POPR instructions because they do not change the
       condition codes set by the MATCHC.  After saving R2, if necessary,
       restore any regs which were saved before the emission of the
       MATCHC instruction.  *)

    if regdesc[ r2 ].uses_remaining > 0 then begin
      gen1 ( pushr, typ_int_value ( 4, vax_word ) );
      gen1 ( popr, typ_int_value ( 1, vax_word ) );
      reg := r0;
    end
    else reg := r2;
    remainder_addr := typ_reg_addr ( reg, vax_word );

    restore_regs ( regs_saved );

    (* If no match was found (condition code Z clear), then we
       branch out of the INDEX code; otherwise we calculate the
       position of the first character matched.  *)

    out_label := make_def ( local_def );
    gen_branch ( bnequ, out_label );

    result_addr.byte_size := vax_word;	(* calculation done with word operands *)
    calc_index ( source_length, object_length, remainder_addr, result_addr );
    
    (* If the default value may not fit in a word, then we must convert
       the index calculation to a longword.  *)

    if ( upperbound (operand) = 3 ) andif 	(* explicit default *)
       ( ( not iconstp ( operand[ 3 ], default_value ) ) orif
         ( ( default_value < minimum ( word ) ) or
	   ( default_value > maximum ( word ) ) ) )
      then result_addr := cvt_long ( result_addr, unsigned_value );

    (* Define the branch label and set the return value.  *)

    mark_def ( code_area, out_label );
    do_index_op := result_addr;

  end  (* with *) ;
end  (* proc do_index_addr *) ;
$PAGE str_search_verify

(* STR_SEARCH_VERIFY generates code for intrinsic functions SEARCH and
   VERIFY.  Calls to runtime routines are generated for either routine.
   Parameter EXP is the SEARCH_OP or VERIFY_OP expression tuple.  An 
   address descriptor for the function result is returned.  *)

public function str_search_verify ( exp: expr ): addr_desc;

type
  sv_rt_symbol_array = packed array [boolean, boolean, boolean] of rt_symbol;
						(* 1st dim: true => SEARCH *)
						(* 2nd dim: true => no uppercasing of string *)
						(* 3rd dim: true => L format set descriptor used *)
const
  sv_entry_point: sv_rt_symbol_array :=
	( ( ( rt_vfu_fo, rt_vfu_fl ),
	    ( rt_vf_fo,  rt_vf_fl  ) ),
          ( ( rt_sru_fo, rt_sru_fl ),
	    ( rt_sr_fo,  rt_sr_fl  ) ) );

var
  l_format: boolean;
  char_set, set_temp: set_desc;
  len_bytes: int_type;
  temp_addr: addr_desc;
  string_desc: str_desc;
  rt_sym: rt_symbol;
  out_label: def;

begin
  with exp^ do begin

    (* If a default is explicitly specified, then load it into a
       register.  *)

    if upperbound (operand) = 3
      then str_search_verify := reg_addr ( copy_load ( operand[ 3 ] ) );

    (* FETCH the string operand.  If a lower case conversion is pending,
       then we must do the case conversion by copying the string to a temp.
       Upper case conversions are handled by the runtime routine.
       This is fetched before the call to str_search_verify because a
       dynamic temp may be created in either case due to the nature of sets
       and strings. The string length and start will be pushed later *)

    string_desc := fetchstring ( operand[ 1 ], actual_length );
    if string_desc.trans_code = lower_trans
      then string_desc := do_translation ( string_desc );

    (* Generate the argument words for the set operand.  *)

    char_set := set_fetch (operand[2], operand[2]^.desc.set_lwb, operand[2]^.desc.set_length,
			   no_preference, true);
    if char_set.nargs = 0 then begin
      push_value (int_value (0), unsigned_value);
      push_value (int_value (1), unsigned_value) (* result is [1..0] or null set *)
    end
    else if char_set.nargs = 2 then begin
      push_value (char_set.arg[2], unsigned_value);
      push_value (char_set.arg[1], unsigned_value)
    end
    else begin
      assert (aconstp (char_set.arg[3], len_bytes));
      if char_set.arg[2].offset > 0 then begin
	len_bytes := len_bytes + char_set.arg[2].offset;
	set_temp := set_temporary (0, int_value (len_bytes));
	set_move (char_set, set_temp);
	char_set := set_temp
      end;
      with operand[2]^.desc do
	set_length := set_length + (set_lwb mod bits_per_byte) + ((set_lwb div bits_per_byte) * bits_per_byte);
      push_value (int_value (operand[2]^.desc.set_length), unsigned_value);
      push_address (char_set.arg[1])
    end;
    l_format := char_set.nargs = 3;

    (* Push the address of the start of the string's text onto the stack and
       push the length of the string onto the stack.  *)

    push_address ( skip_len_words ( string_desc ) );	(* push text address *)
    push_value ( duplicate_addr ( string_desc.len_addr ), unsigned_value );	(* push length *)

    (* Generate the runtime call.  *)

    rt_sym := sv_entry_point[ opcode = search_op, string_desc.trans_code <> upper_trans,
			      l_format ];
    gen_rt ( 4, rt_sym );

    (* If an explicit default was given then we must test the result
       returned, and, it it is non-zero, then move it to the result
       register.  If a default value was not explicitly given, then
       simply move the runtime routine's result to the result register.  *)

    if upperbound (operand) = 3 then begin
      gen1 ( tstl, r0_addr );
      out_label := make_def ( local_def );
      gen_branch ( beql, out_label );
      gen2 ( movl, r0_addr, str_search_verify );
      mark_def ( code_area, out_label );
    end
    else begin	(* no explicit default specified *)
      str_search_verify := reg_addr ( get_reg ( bits_per_reg ) );
      gen2 ( movl, r0_addr, str_search_verify );
    end;

    (* Free the string descriptor.  *)

    free_string ( string_desc );

  end  (* with *) ;
end  (* proc str_search_verify *) ;
$PAGE str_compare

(* STR_COMPARE compares two strings.  Parameter EXP is the string
   comparison expression tuple.  This routine merely emits a string
   comparison instruction.  Execution of the instruction will set
   the condition codes.  It is the callee's responsibility to generate
   code to utilize the condition code setting.  *)

public procedure str_compare ( exp: expr );

var
  lhs, rhs: str_desc;
  lhs_const, rhs_const: boolean;
  lhs_length, rhs_length: char_range;
  lhs_text_addr, rhs_text_addr: addr_desc;
  regs_saved: set_of_registers;

begin
  
  (* Fetch both strings.  *)

  lhs := fetchtranslated ( exp^.operand[ 1 ], actual_length );
  rhs := fetchtranslated ( exp^.operand[ 2 ], actual_length );

  (* Determine if either string is of constant length and calculate
     the starting addresses of the text of each string.  *)

  lhs_const := aconstp ( lhs.len_addr, lhs_length );
  rhs_const := aconstp ( rhs.len_addr, rhs_length );

  lhs_text_addr := skip_len_words ( lhs );
  rhs_text_addr := skip_len_words ( rhs );

  (* Mark regs R0-R3 as used by the current procedure;  free the strings
     and address descriptors used before we save any regs used by the
     instruction.  *)

  mark_regs_used ( [r0..r3] );
  free_string ( lhs );
  free_string ( rhs );
  free ( lhs_text_addr );
  free ( rhs_text_addr );
  regs_saved := save_regs ( [r0..r3] );

  (* Generate a CMPC3 if both strings are known at compile time to
     be of the same length; otherwise generate a CMPC5.  *)

  if ( lhs_const and rhs_const ) andif
     ( lhs_length = rhs_length )
    then gen3 ( cmpc3, lhs.len_addr, lhs_text_addr, rhs_text_addr )
    else gen5 ( cmpc5, lhs.len_addr, lhs_text_addr, fill_addr,
		       rhs.len_addr, rhs_text_addr );

  restore_regs ( regs_saved );	(* restore any regs actually saved  *)

end  (* proc str_compare *) ;
$PAGE copy_string

(* COPY_STRING copies a given string (parameter SOURCE_DESC) to a
   temporary.  A string descriptor for the temporary is returned.
   Note that the length word of a varying string will NOT be copied.
   Parameter SOURCE_DESC is freed by this routine.  *)

public function copy_string ( var source_desc: str_desc ): str_desc;

var
  dummy: addr_desc;

  (* FREE_SOURCE is passed as a parameter to MOVE_STRING to free the
     source descriptor before the string move.  *)

  procedure free_source;
    begin
      free_string ( source_desc );
    end;

begin

  (* Allocate the temp and initialize its string descriptor.  *)

  COPY_STRING := STR_TEMP ( SOURCE_DESC.LEN_ADDR , SOURCE_DESC.MAX_LEN, nonvarying );
  with copy_string, type_desc do begin
    type_desc := source_desc.type_desc;
    if (kind = strings) andif (str_kind = varying) then begin
      str_kind := nonvarying;	(* since length word not copied *)
      str_flex := true;		(* since length not known at compile time *)
    end;
  end;

  (* Move string to temp. *)

  move_string ( copy_string, source_desc, false, free_source, false, dummy );

end  (* proc copy_string *) ;
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
  skip_label: def;

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
      length_addr := typ_int_value ( 1, vax_word );
      length_valid := true;
    end

    else if not (type_desc.str_flex or	(* if neither flex flag set, then *)
		 rhs_desc.type_desc.str_flex) then begin	(* check for cases 2 and 3 *)
      
      if type_desc.str_length >= rhs_desc.type_desc.str_length then begin	(* Case 2 *)
	length_addr := duplicate_addr ( rhs_desc.len_addr );
	length_valid := true;
      end

      else if ( rhs_desc.type_desc.str_kind = nonvarying ) andif	(* Case 3 *)
	      ( rhs_desc.type_desc.str_length >= type_desc.str_length ) then begin
	length_addr := typ_int_value ( type_desc.str_length, vax_word );
	length_valid := true;
      end;
    end;

    (* if any of the above special cases applied, the we simply move
       the length determined above to the lhs length word.  *)


    if length_valid then begin
      store ( length_addr, length_word_addr, unsigned_value );
      free ( length_addr );
    end

    (* None of the special cases applied - we must emit code to compute
       at run time:  MIN ( upperbound ( lhs ), length ( rhs ) )  *)

    else begin
      assert ( rhs_desc.len_addr.byte_size = vax_word );
      assert ( len_addr.byte_size = vax_word );
      gen2 ( movw, rhs_desc.len_addr, length_word_addr );
      gen2 ( cmpw, length_word_addr, len_addr );
      skip_label := make_def ( local_def );
      gen_branch ( blequ, skip_label );
      gen2 ( movw, len_addr, length_word_addr );
      mark_def ( code_area, skip_label );
    end;

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
  padded: boolean;
  dummy_addr: addr_desc;


  (* The following proc is passed as a parameter to MOVE_STRING
     to do the desired frees before the registers used by the instruction
     are saved.   *)

  procedure free_both;
    begin
      free_string ( rhs_desc );
      free_string ( lhs_desc );
    end;

begin
  with node^ do begin
  
    (* Get string descriptors for both the left and right hand sides.  *)

    rhs_desc := fetchstring ( rhs, actual_length );
    lhs_desc := fetchstring ( lhs, max_length );

    (* if lhs string is varying, the update length word.  *)

    if lhs_desc.type_desc.str_kind = varying 	(* note that lhs cannot be a char *)
      then update_length ( lhs_desc, rhs_desc );
    
    (* finally move the rhs string to the lhs string.  *)

    padded := lhs_desc.type_desc.str_kind = nonvarying;
    move_string ( lhs_desc, rhs_desc, padded, free_both, false, dummy_addr );

  end  (* with *) ;
end  (* proc str_assignment *) ;
$PAGE var_parameter

(* VAR_PARAMETER generates the argument word(s) for a VAR string
   parameter. The address descriptor is put onto a    parameter. The addr_desc for the argument is saved into an array. This
   is used to insure proper handling of dynamic temps. When all parameters for
   the current routine have been fetched, VAXCLL will push the parameters
   from the array. Parameter ACTUAL_EXPR is the expression tuple for the
   actual parameter;  FORMAL_TYPE is the type node for the formal
   parameter.  *)

procedure var_parameter ( actual_expr: expr; formal_type: typ ;
			var params:^param_ary; var param_ary_idx: parm_index );

var
  actual_desc: str_desc;
  param_addr: addr_desc;

begin

  (* Fetch the actual parameter.  *)

  actual_desc := fetchstring ( actual_expr, max_length );

  (* Push the address of the string onto the stack, skipping
     the upperbound word if present. (Remember the word actually gets stored
     in an array in VAXCLL until all parameters have been fetched, cause
     of dynamic temporaries, whereupon all parameters will be pushed. *)

  param_addr := skip_desc_word ( actual_desc );
  param_addr.byte_size := vax_byte;
  add_to_param_ary( param_addr , unsigned_value, false , params , param_ary_idx );

  (* If the formal is flex then push the upperbound of the string
     onto the stack, so to speak. (actually store it until all are fetched.. *)

  if formal_type^.flexible
    then add_to_param_ary( duplicate_addr(actual_desc.len_addr), unsigned_value, true ,
		params , param_ary_idx );
  
  free_string ( actual_desc );

end  (* proc var_parameter *) ;
$PAGE value_parameter

(* VALUE PARAMETER generates an ADDR_DESC for string parameters passed
   by value. This address descriptor will be pushed onto a stack and
   stored there until all parameters have been fetched. This is to 
   insure the proper handling of dynamic temporaries. The array whii
   have the paraemeters removed and pushed immediatly before the routine
   is called ( see VAXCLL ).
   It both copies the string to a temporary, if required, and generates
   the argument word(s) . Parameter ACTUAL_EXPR is the
   expression tuple for the actual parameter;  FORMAL_TYPE is the type
   node for the formal parameter.  *)

procedure value_parameter ( actual_expr: expr; formal_type: typ;
			var params:^param_ary; var param_ary_idx:parm_index );

var
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
  temp_len_addr: addr_desc;
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

  if actual_expr^.opcode = strcvt_op	(* don't take type info for the actual *)
    then uncoerced_expr := actual_expr^.operand[ 1 ]	(* from a STRCVT_op!! *)
    else uncoerced_expr := actual_expr;

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

  (* Determine whether or not a copy to a temporary is
     required.  A copy is NOT necessary under the following
     circumstances:
	1. no STRCVT_OP is present, or, 
	2. the formal parameter is a nonvarying flex, or,
	3. the kinds of the formal and actual are the same and one of
	   the following holds:
	   a. the formal is flex, or, 
	   b. neither the formal nor the actual is flex and either
	      ( i. ) both are nonvarying and length(actual) >= length(formal), or,
	      ( ii. ) both are varying and upperbound(actual) <= 
		      upperbound(formal).				*)

  no_copy := ( actual_expr^.opcode <> strcvt_op ) or
	     ( formal_is_flex and ( formal_kind = nonvarying ) ) or
	     ( ( actual_kind = formal_kind ) and
	       ( ( formal_is_flex ) or
	         ( not formal_is_flex and not actual_is_flex and
		   ( ( ( actual_kind = nonvarying ) and
		       ( formal_kind = nonvarying ) and
		       ( actual_bound >= formal_bound ) ) or
		     ( ( actual_kind = varying ) and
		       ( formal_kind = varying ) and
		       ( actual_bound <= formal_bound ) ) ) ) ) );

  (* A bit of chicanery here - we may need both the length and the
     upperbound of the actual.  Since the length is easily found if the bound
     is known, we request the bound when we fetch the actual.  We
     then copy the bound and then set the descriptor's length to the
     actual length.  *)

  if no_copy 
    then actual_desc := fetchtranslated ( actual_expr, max_length )
    else actual_desc := fetchstring ( actual_expr, max_length );
  bound_addr := actual_desc.len_addr;
  if actual_kind = nonvarying
    then actual_desc.len_addr := duplicate_addr ( actual_desc.len_addr )
    else actual_desc.len_addr := skip_desc_word ( actual_desc );
  actual_desc.len_context := actual_length;

  (* If a copy is necessary, then allocate a temp and copy the
     actual parameter to the temp.  *)

  if not no_copy then begin

    (* Allocate and initialize the temporary.  *)

    if formal_is_flex or
       ( (formal_kind = varying) and (actual_desc.max_len < formal_bound) )
      then temp_length := actual_desc.max_len
      else temp_length := formal_bound;
    if formal_is_flex and actual_is_flex
      then temp_len_addr := actual_desc.len_addr
      else temp_len_addr := typ_int_value ( temp_length, vax_word );
  
    TEMP_DESC := STR_TEMP ( temp_len_addr , TEMP_LENGTH, formal_kind );
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

    if formal_kind = varying then begin	(* formal is varying *)
      if formal_is_flex then begin	(* update length word *)
        length_word_addr := temp_desc.base_addr;
	length_word_addr.byte_size := vax_word;
	store ( actual_desc.len_addr, length_word_addr, unsigned_value );
      end
      else begin
	update_length ( temp_desc, actual_desc );
      end;
      free ( temp_desc.len_addr );
      temp_desc.len_addr := duplicate_addr ( temp_desc.base_addr );
    end
    else if formal_is_flex then begin
      free ( temp_desc.len_addr );
      temp_desc.len_addr := duplicate_addr ( actual_desc.len_addr );
    end;

    (* Copy the actual parameter to the temp.  Set ACTUAL_DESC to
       the temp's string descriptor.  *)

    padded := formal_kind = nonvarying;
    move_string ( temp_desc, actual_desc, padded, free_actual, false, dummy_addr );
    actual_desc := temp_desc;
  end  (* if *) ;

  (* Push the parameter's address onto the stack.  *)

  if formal_kind = nonvarying
    then param_addr := skip_len_words ( actual_desc )
    else param_addr := skip_desc_word ( actual_desc );
  param_addr.byte_size := vax_byte;

  (* Due to dynamic temporaries, this value may not be pushed directly
     onto the stack, rather it must be stored until all the parameters have
     been fetched, then push them. *)

  add_to_param_ary ( param_addr , unsigned_value , false , params , param_ary_idx );

  (* if the formal is flex then push the upperbound of the actual
     onto the stack, so to speak. Actually add it to the parameter array
     and VAXCLL will push the addr_desc after it has fetched them all *)

  if formal_is_flex then begin
    if formal_kind = nonvarying
      then add_to_param_ary(duplicate_addr(actual_desc.len_addr),unsigned_value, true ,
		params , param_ary_idx )
    else add_to_param_ary (duplicate_addr(bound_addr), unsigned_value, true ,
		params , param_ary_idx );
  end;

  (* Clean up the act.  *)

  free_string ( actual_desc );
  free ( bound_addr );

end  (* proc value_parameter *) ;
$PAGE str_parameter

(* STR_PARAMETER generates code to fetch argument word(s) for a string
   parameter, but because of dynamic temps, does not generate code to
   directly push the arguments onto the stack, but calls a routine which
   stores the arguments into an array. Immediately before generating the
   code to call the procedure, VAXCLL will generate code to push each of the
   parameters stored in the array. ( The arguments cannot be directly pushed
   onto the stack until all arguments have been fetched, or any dynamic
   temp created by a call to fetch will cause the parameter to get lost
   within the dynamic tmp ).
   Any type coercions required
   for value parameters are done also.  Parameter ACTUAL_EXPR is the
   expression tuple for the actual paramter; PARM_KIND is the kind
   of the formal (in particular, VAR or value); PARM_TYPE is the
   type of the formal.  *)

public procedure str_parameter ( actual_expr: expr; parm_kind: sym_kind; parm_type: typ ;
		var params:^param_ary; var param_ary_idx: parm_index );

begin
  
  if parm_kind = vars
    then var_parameter ( actual_expr, parm_type , params , param_ary_idx )
    else value_parameter ( actual_expr, parm_type , params , param_ary_idx );

end  (* proc str_parameter *) .
I v