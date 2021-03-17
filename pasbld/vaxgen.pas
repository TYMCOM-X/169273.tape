$TITLE VAXGEN - VAX quick pass code generator

module vaxgen;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasfil.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM vaxcg.typ
$SYSTEM vaxcgu.inc
$SYSTEM vaxmac.inc
$SYSTEM vaxopc.inc
$SYSTEM pasifu.inc
$SYSTEM vaxexp.inc
$SYSTEM vaxcll.inc
$SYSTEM vaxcmp.inc
$SYSTEM vaxstr.inc
$system vaxset.inc
$SYSTEM vaxrel.inc
$SYSTEM vaxcsp.inc
$SYSTEM vaxdeb.inc
$SYSTEM pasjmp.inc
$SYSTEM pastal.inc
$SYSTEM passw.inc
$SYSTEM pa2dmp.inc
$SYSTEM pasdmp.inc
$system vaxutl.inc
$system vaxio.inc
$system pasmth.inc
$system pascv.inc
$SYSTEM pasmap.inc
$system ptmcon.inc
$PAGE global variables

public var

  p4_error_level: error_levels;	(* max severity of any pass4 error *)

static var

  temp_base: unit_range;	(* top of temps in local stack frame *)

  temp_max: unit_range;		(* offset of max allocated temps *)

  stack_fixup: code;		(* for resolution of stack frame size *)


  ANY_DYNAMICS : Boolean;	(* Any dynamic temporaries for this statement ? *)

  DYN_DEF : DEF;		(* Definition record used to clean up the
				   stack because of dynamic temps *)

type
  attr_array = array[psect_type] of set of psect_attributes;

public const
  psectattrs: attr_array := (
	(* static_psect *)
	[longword_aligned,concatenate,position_independent,readable,writeable,relocatable],
	(* code psect *)
	[longword_aligned,concatenate,executable,position_independent,shareable,readable,relocatable]);

public var
  psect_id: array[psect_type] of byte;
$PAGE restriction_error

(* RESTRICTION_ERROR is called to print an error message whenever an
   unimplemented feature is encountered. *)

public procedure restriction_error ( error_message: string );

begin

  (* Write error message and current source id to the terminal. *)

  writeln ( tty, 'Error at location ', cv_source_id ( cur_source ) );
  writeln ( tty, '   ', error_message );

  (* Set global error flag to SEVERE_ERRORS.  This flag is used by
     VAXREL to determine whether or not any existing object file
     should be superseded.  *)

  p4_error_level := severe_errors;	(* no object file will be produced *)

end  (* proc restriction_error *) ;
$PAGE fatal_error

(* FATAL_ERROR is called in the event of an unrecoverable error in
   PASS4.  (Currently this means either register overflow or a single
   procedure so large it requires branch displacements larger than
   16 bits).  *)

public procedure fatal_error ( message: string );

begin
  (* Write an error message to the terminal *)

  writeln ( tty, '?Fatal error at ', cv_source_id ( cur_source ) );
  writeln ( tty, '   - ', message );
  break ( tty );

  (* If an object file is being written then close and delete it
     (leaving any previous version intact).  *)

  p4_error_level := fatals;
  rel_end ( nil, 0, 0 );

  stop;
end  (* proc fatal_error *) ;
$PAGE get_temp
public function get_temp (desc: addr_desc; vax_alignment: vax_type): addr_desc;

(* Allocate a temporary of the specified size and alignment in the local stack frame. *)

type
  size_array = array[vax_type] of bit_range;

const
  vax_size: size_array := (1, 2, 4, 4, 4, 4);

Var TEMP_DESC : ADDR_DESC;	(* Must have SP into a longword register *)
    LEN : INT_TYPE;		(* For return from ACONSTP, size of static *)

begin

  (* IF the address descriptor is IMMEDIATE then allocate a static temp, else
     allocate a dynamic temp *)

  If ACONSTP ( DESC , LEN )
    Then Begin
      get_temp := temp_reference;
      temp_base := ngm (abs(cur_block^.neg_stk_end) + temp_base + LEN ,
		   vax_size[vax_alignment]) + cur_block^.neg_stk_end;
      temp_max := max (temp_max, temp_base);
      get_temp.offset := cur_block^.neg_stk_end - temp_base;
    End
  Else Begin
    ANY_DYNAMICS := True;	(* Set the static flag so the stack may be reset *)
    GET_TEMP := GET_TYP_REG_ADDR ( VAX_LONG );	(* REG fr the temp *)

    (* Now fix the stack pointer using address descriptor passed to 
       this routine. It contains the size of the temp. SUBL2 <size>,SP *)

    TEMP_DESC := CVT_LONG ( DUPLICATE_ADDR ( DESC ) , UNSIGNED_VALUE );
    GEN2 ( SUBL2 , TEMP_DESC , SP_ADDR );
    Free ( TEMP_DESC );

    (* Now generate address of temp into a register, MOVAB (SP),Rn *)

    GEN2 ( MOVAB , STACK_TOP_REFERENCE , GET_TEMP );

    (* Change return value so it is (Rn) and not Rn *)

    GET_TEMP.OFFSET := 0;		(* Insurance *)
    GET_TEMP.RELOC.KIND := ABSOLUTE_SC
  End
end;
$PAGE kill_temps

(* KILL_TEMPS resets the staic temporary offset, thus effectively
   deallocating any active static temps.  *)

public procedure kill_temps;

begin
  temp_base := 0;	(* no temps allocated *)
end;
$PAGE ovly_emit_indirect_words
(* This routine generates the code records for the indirect words used to
   access public consts and vars in overlay compilations. This routine is called
   from gen_code in vaxgen, after constant pooling has beeen done. *)

procedure ovly_emit_indirect_words
  options special(coercions);

var symbol : sym;

begin

  (* generate a comment code record to indicate that this is the beginning
     of the indirect words for overlay compilations *)

  gen_cmt ( cst_area , 'Indirect words for an overlay compilation.' );

  (* Walk the symbol nodes looking for candidates for the indirect words *)

  symbol := root_block^.children^.id_list.first;

  while symbol <> nil do
    with symbol^ do begin
      if public_dcl andif
      ((kind = vars ) or ((kind=consts) andif ( init_value.kind = alloc_cst)))
	then genindirect_word ( cst_area, symbol );

      symbol := next
    end;		(* with symbol^ do *)

end;		(* ovly_emit_indirect_words *)
$PAGE bb_start, reset_stack

(* BB_START performs processing which must be done on entry to
   each basic block (i.e., at each LABLEL_NODE tuple).  In
   particular it generates an instruction on entry to each basic
   block which deallocates any active dynamic temps.  If the label
   is the target of a non_local goto or the label is already
   known to be the target of a branch out of a block which
   allocated dynamic temps, then the code record is simply
   generated normally.  Otherwise, the deallocation may in fact be
   unnecessary so the KIND of the code record is set to NULLCODE.
   If the deallocation turns out to be necessary, then at the end
   of some successor basic block the kind of the code record
   will be reset to INSTRUCTION.  The flag indicating whether
   dynamic temps were allocated in the current basic block is 
   always reset by this routine.  *)
  
  
procedure reset_stack;
  var
    restore_addr : addr_desc;
  begin
    restore_addr := fp_addr;
    restore_addr.reloc.kind := def_sc;
    if dyn_def = nil			(* static cell for defn ptr *)
      then dyn_def := make_def ( temp_size_def );
    restore_addr.reloc.reldef := dyn_def;
    gen2 ( moval, restore_addr, sp_addr )
  end;
  
  
procedure bb_start ( label_node_tuple: tuple )  options special(coercions);

begin

  (* Reset the flag indicating whether dynamic temps have been allocated
     in the current basic block.  *)

  any_dynamics := false;

  (* Generate an instruction to deallocate dynamic temps.  Note
     that the stack offset in the restore address is a forward
     reference.  The value is represented here by a definition 
     record and resolved at the end of the compilation of the
     current routine. *)

  reset_stack;

  (* If the basic block is either the target of a non-local goto
     or is already known to be the successor of a block which uses
     dynamic temps then leave the code record alone; otherwise mark
     it as KIND NULLCODE.  Note that we overload the DOM_SON field
     of the LABEL_NODE and use it as a boolean flag indicating
     whether or not the block is the target of a branch from a block
     using dynamic temps.  This field is only used when the jump
     tuple is processed before the LABEL_NODE tuple.  The IDOM
     field of the LABEL_NODE tuple is also overloaded; it is
     coerced to point to the code record allocated.  *)

  with label_node_tuple^ do begin
    idom := ptr ( ord ( code_area.last ) );
    if ( (label_sym = NIL) orif (not label_sym^.lab_nonlocal_use) ) and
       ( dom_son = nil ) then
      code_area.last^.kind := nullcode;
  end  (* with *) ;

end  (* proc bb_start *) ;
$PAGE bb_end

(* BB_END does processing necessary at the end of a basic block.
   In particular, if dynamic temps were used in the basic block
   terminated by the jump, then the label node for the basic
   block being branched to by the jump is flagged as requiring
   dynamic temp deallocation.  This 'flagging' is done in one of
   two ways.  If the label node for the target has already been
   processed then the IDOM field of the LABEL_NODE has been 
   coerced to point to the code record for the deallocation
   instruction;  in this case we set the KIND of that code record
   to INSTRUCTION (it was NULLCODE).  If the LABEL_NODE for the
   target has not yet been processed, then we flag the LABEL_NODE
   by setting its DOM_SON field to a non-nil value.  *)

procedure bb_end ( jump_tuple: tuple )  options special(coercions);

var
  code_ptr: code;

begin
  if any_dynamics then begin
    with jump_tuple^.jump_to^ do begin
      assert ( opcode = label_node );
      if idom = nil then begin
	dom_son := jump_tuple 		(* set to any non-nil value *)
      end
      else begin
	code_ptr := ptr ( ord(idom) );
	code_ptr^.kind := instruction;
      end;
    end  (* with *);
  end  (* if *) ;
end  (* proc bb_end *) ;
$PAGE emit_code
(* EMIT CODE takes a code list.  It will write it to the rel file, and to
   the macro listing if assembly_opt is in the options_list parameter. *)

procedure emit_code ( var area_list: code_list; (* the code area to be written *)
		      var ic: unit_range; (* the address to write the code at *)
		      psect: byte;	(* psect id of the code area *)
		      options_list: set_of_options ); (* to control listing *)

 begin
  if assembly_opt in options_list then
    mac_list (area_list, ic );
  wr_code (area_list, ic, psect);
 end;
$PAGE init_static
(* INIT STATIC generates code for the initialized part of the static area, and
   for the declared public constants.  Symbols are processed in the order:
	Symbols within the id list of a block
	Blocks in downward_call_thread order
   This is the order in which they are processed during storage allocation. *)

procedure init_static (static_size: unit_range)  options special(coercions);

 var
   block: blk;
   symbols: sym;
   reld: rel_syllable;
   need_origin: boolean;
   id: byte;
   static_counter, actual_size, tsize: bit_range;
   talign: align_range;

 begin

  if static_size > 0 then	(* init psect *)
    gen_origin (static_area, static_psect);
  need_origin := root_block^.children^.kind = data_blk;
  static_counter := 0;	(* Track size of emitted constants *)
  block := root_block;

  while block <> nil do begin
    with block^ do begin
      symbols := id_list.first;
      while symbols <> nil do begin
	with symbols^ do begin
	  if (kind = vars) andif (dcl_class = static_sc) andif
	     (init_value.kind <> no_value) then begin

	    (* Force alignment *)

	    alc_data (type_desc, tsize, talign);
	    skip_bytes (static_area, static_counter, ngm (static_counter, talign));
	    assert ((item_addr * bits_per_byte) = static_counter); (* verify assumption *)
	    gen_val (static_area, init_value, type_desc);
	    if (init_value.kind = string_cst) andif (init_value.valp^.str_varying_ref) then begin

	      (* Pad out to full declared length *)

	      actual_size := str_lw_width + length (init_value.valp^.str_val) * char_size;
	      skip_bytes ( static_area, actual_size , type_desc^.base_size );
	    end;
	    static_counter := static_counter + tsize;
	  end
	  else if (kind = consts) andif (init_value.kind <> subr_cst) andif
		  ( public_dcl or
		    ( prog_options.debug_opt and
		      (dcl_class <> external_sc) ) ) then begin
	    if need_origin then begin
	      gen_origin (cst_area, code_psect);
	      need_origin := false;
	    end;
	    reld := gen_cval (init_value, type_desc);
	    init_value.kind := alloc_cst;
	    init_value.defp := val_ptr (reld.reldef);
	  end;
	  symbols := next;
	end;
      end (* while symbols <> nil *);
      block := downward_call_thread;
    end;
  end (* while block <> nil *);

  skip_bytes (static_area, static_counter, ngm (static_counter, bits_per_unit));
  block := root_block;
  while block <> nil do begin
    with block^ do begin
      symbols := id_list.first;
      while symbols <> nil do begin
	with symbols^ do begin
	  if (kind = conditions) andif (dcl_class = static_sc) then begin
	    alc_data (type_desc, tsize, talign);
	    skip_bytes (static_area, static_counter, ngm (static_counter, talign));
	    assert (((item_addr + size_init) * bits_per_byte) = static_counter);
	    genindirect_word (static_area, symbols);
	    static_counter := static_counter + bits_per_address
	  end;
	  symbols := next;
	end;
      end (* while symbols <> nil *);
      block := downward_call_thread;
    end;
  end (* while block <> nil *);
  emit_code (static_area, loc_static, psect_id [static_psect], prog_options.semantic_options);
  assert (ngm (loc_static, bytes_per_unit) = (size_init + size_cond));
  if (static_size > loc_static) and (assembly_opt in prog_options.semantic_options) then
    mac_pad (static_size - loc_static);  (* put .BLKB for unitialized static area into macro listing *)
 end;
$PAGE store
(* STORE moves a value to a destination, with truncation or
   expansion if necessary. Both source and destination must be
   no larger than a quadword. "Alignment" parameter determines
   if sign extension should be performed.	*)

public procedure store (source, dest: addr_desc; alignment: data_alignment);
var
  op: opc_range;
  cval: int_type;
  new_source: addr_desc;

type
  ops_array = packed array[vax_type,vax_type,data_alignment] of opc_range;

const
  convert_ops: ops_array := (

	(* byte destination *)	(

	(movb,		movb,		movb),	(* byte source *)
	(movb,		cvtwb,		movb),	(* word source *)
	(movb,		cvtlb,		movb),	(* long source *)
	(halt,		halt,		halt),	(* quad source *)
	(cvtfb,		cvtfb,		cvtfb),	(* real source *)
	(cvtdb,		cvtdb,		cvtdb)),(* dblr source *)

	(* word destination *)	(

	(movzbw,	cvtbw,		movzbw),(* byte source *)
	(movw,		movw,		movw),	(* word source *)
	(movw,		cvtlw,		movw),	(* long source *)
	(halt,		halt,		halt),	(* quad source *)
	(cvtfw,		cvtfw,		cvtfw),	(* real source *)
	(cvtdw,		cvtdw,		cvtdw)),(* dblr source *)

	(* long destination *)	(

	(movzbl,	cvtbl,		movzbl),(* byte source *)
	(movzwl,	cvtwl,		movzwl),(* word source *)
	(movl,		movl,		movl),	(* long source *)
	(halt,		halt,		halt),	(* quad source *)
	(cvtfl,		cvtfl,		cvtfl),	(* real source *)
	(cvtdl,		cvtdl,		cvtdl)),(* dblr source *)

	(* quad destination *)	(

	(halt,		halt,		halt),	(* byte source *)
	(halt,		halt,		halt),	(* word source *)
	(halt,		halt,		halt),	(* long source *)
	(movq,		movq,		movq),	(* quad source *)
	(halt,		halt,		halt),	(* real source *)
	(halt,		halt,		halt)),	(* dblr source *)

	(* single real destination *)	(

	(cvtbf,		cvtbf,		cvtbf),	(* byte source *)
	(cvtwf,		cvtwf,		cvtwf),	(* word source *)
	(cvtlf,		cvtlf,		cvtlf),	(* long source *)
	(halt,		halt,		halt),	(* quad source *)
	(movf,		movf,		movf),	(* real source *)
	(cvtdf,		cvtdf,		cvtdf)),(* dblr source *)

	(* double real destination *)	(

	(cvtbd,		cvtbd,		cvtbd),	(* byte source *)
	(cvtwd,		cvtwd,		cvtwd),	(* word source *)
	(cvtld,		cvtld,		cvtld),	(* long source *)
	(halt,		halt,		halt),	(* quad source *)
	(cvtfd,		cvtfd,		cvtfd),	(* real source *)
	(movd,		movd,		movd)));(* dblr source *)

begin
  if adr_equal (source, dest) then
    return;
  if aconstp (source, cval) then
    move_immediate (cval, dest)
  else begin
    
    (* if the source is unsigned, indexed and larger than the
       destination then we first move the source to a volatile 
       register so that the index calculation is correct.  *)

    if (source.index <> noreg) and
       (alignment <> signed_value) and
       (vax_type_size(source.byte_size) > vax_type_size(dest.byte_size)) then begin
      gen2(typ_opc(movl,source.byte_size), source, r0_addr);
      new_source := r0_addr;
      new_source.byte_size := source.byte_size;
    end
    else new_source := source;

    op := convert_ops [dest.byte_size, new_source.byte_size, alignment];
    assert (op <> halt);
    gen2 (op, new_source, dest);
  end;
end;
$PAGE scalar_assignment
(* SCALAR ASSIGNMENT generates code for assignments of scalar values. *)

procedure scalar_assignment (tpl: tuple);

 var
   laddr, raddr: addr_desc;

 begin
  with tpl^ do begin
    laddr := fetch ( lhs , no_preference );
    raddr := fetch ( rhs , laddr );

    (* check to see if the fetch was able to fulfill the target request.
       If so, just free the address descriptor. If not, then move the
       result into the left hand side. *)

    if not adr_equal ( raddr , laddr )
      then begin
	store ( raddr , laddr , alignment ( rhs ) );
	free ( laddr );
      end;
    free ( raddr );
  end;
end (* scalar_assignment *);
$PAGE proc_func_assignment
(* PROC FUNC ASSIGNMENT generates assignment to procedure and function variables. *)

procedure proc_func_assignment (tpl: tuple);

var
  target, source, addr: addr_desc;
  i, levels_dif: int_type;
  no_parent: boolean;
  reg: registers;

begin
  with tpl^ do begin
    source := fetch (rhs,no_preference);

    (* If the source is another procedure/function variable it can simply be
       moved to the destination. *)

    if (source.reloc.kind in [local_sc,parameter_sc,static_sc]) orif
      (source.reloc.kind = external_sc) andif (source.reloc.relsym^.kind <> consts) then begin
	target := fetch (lhs,no_preference);
	gen2 (movq, source, target);
    end
    else begin  target := argument (lhs);	(* must not be indirect *)

      (* Determine if the routine has a parent. *)

      no_parent := (source.reloc.kind = external_sc) orif (rhs^.cst_val.blkp^.apparent_level <= 2);

      if target.index <> noreg then begin	(* load address *)
	free (target);
	reg := get_reg (bits_per_unit);
	gen_mr (movaq, target, reg);
	target := absolute_reference;
	target.register := reg;
      end;

      (* Move the routine's address. *)

      gen2 (moval, source, off_addr (target, bytes_per_unit));

      if no_parent then
	gen1 (clrl, target)
      else begin
	levels_dif := cur_block^.apparent_level - rhs^.cst_val.blkp^.apparent_level;
	if levels_dif = -1 then
	  gen2 (movl, fp_addr, target)
	else begin
	  addr := absolute_reference;
	  addr.register := fp;
	  addr.offset := -bytes_per_unit;
	  if levels_dif = 0 then
	    gen2 (movl, addr, target)
	  else begin
	    gen2 (movl, addr, r0_addr);
	    addr := push_reference;
	    addr.register := r0;
	    for i := 1 to levels_dif - 1 do
	      gen2 (movl, addr, r0_addr);
	    gen2 (movl, addr, target);
	  end;
	end;
      end;
    end;
    free (target);
    free (source);
  end (* with *);
end (* proc_func_assignment *);
$PAGE block_move

(* BLOCK_MOVE moves a specified number of bytes of uninterpreted
   data from one location to another.  Paramerer LEN_ADDR is an 
   address descriptor for the length in bytes of the block;
   SOURCE_ADDR is the base address of the source and DEST_ADDR
   is the base address of the destination.  

   Note that:
     1. This routine does not FREE any address descriptors,
     2. Unless the length is constant and in the set [0,1,2,4,8]
	a MOVC3 instruction is generated.  This instruction
	assumes a data type of BYTE for it source and destination
	addresses, i.e., do not pass in an immediate or indexed
	source or destination address unless its BYTE_SIZE field
	is VAX_BYTE.
     3. Similarly, the BYTE_SIZE field of the address descriptor
	for the length of the block must be VAX_WORD if the operand
	is an immediate or indexed.  *)

procedure block_move ( len_addr, source_addr, dest_addr: addr_desc );

var
  length_value: unit_range;
  regs_saved: set_of_registers;

begin

  (* If possible a two operand move instruction is used.  *)

  if aconstp ( len_addr, length_value ) andif
     ( length_value in [0, 1, 2, 4, 8] ) then begin
    
    case length_value of
      0:	;
      1:	gen2 ( movb, source_addr, dest_addr );
      2:	gen2 ( movw, source_addr, dest_addr );
      4:	gen2 ( movl, source_addr, dest_addr );
      8:	gen2 ( movq, source_addr, dest_addr )
    end  (* case *) ;

  end

  (* Otherwise use the MOVC3 *)

  else begin
    regs_saved := save_regs ( [R0..R5] );
    gen3 ( movc3, len_addr, source_addr, dest_addr );
    restore_regs ( regs_saved );
    mark_regs_used ( [R0..R5] );
  end;
end  (* proc block_move *) ;
$PAGE agg_assignment
(* AGG ASSIGNMENT generates code to assign one record or array to another. *)

procedure agg_assignment (tpl: tuple);

var
  raddr, laddr, upb: addr_desc;
  i: int_type;
  byte_offset, elem_size: unit_range;
  field: sym;
  field_offset: unit_range;

begin
  with tpl^ do begin
    raddr := fetch (rhs,no_preference);
    laddr := fetch (lhs,no_preference);

    (* Record assignment.  *)

    if rhs^.desc.kind = records then begin
      if rhs^.desc.base^.flexible then begin	(* ends with flex structure *)
	field := rhs^.desc.base^.field_list;
	while field^.next <> nil do	(* find last (flex) field *)
	  field := field^.next;
	field_offset := field^.fld_offset div bits_per_byte;
	upb := increment_addr (duplicate_addr (raddr), field_offset);
	if field^.type_desc^.kind = strings then begin
	  if field^.type_desc^.str_kind = varying then
	    field_offset := field_offset + (str_lw_width div bits_per_byte);
	  gen3 (addw3, int_value (field_offset + flex_str_desc_size div bits_per_byte),
			upb, r1_addr);
	end
	else begin
	  byte_offset := field^.type_desc^.index_type^.minval - 1;
	  elem_size := field^.type_desc^.element_size div bits_per_byte;
	  if byte_offset <> 0 then begin
	    if byte_offset > 0
	      then gen3 (subl3, int_value (byte_offset), upb, r1_addr)
	      else gen3 (addl3, int_value (-byte_offset), upb, r1_addr);
	    if elem_size > 1 then
	      gen2 (mull2, int_value (elem_size), r1_addr)
	  end
	  else if elem_size > 1 then
	    gen3 (mull3, int_value (elem_size), upb, r1_addr)
	  else gen2 (movl, upb, r1_addr);
	  byte_offset := field_offset + flex_arr_desc_size div bits_per_byte;
	  gen2 (addl2, int_value (byte_offset), r1_addr);
	end;
	block_move ( r1_addr, raddr, laddr);
	free (upb);
      end
      else block_move ( typ_int_value (ngm (rhs^.desc.base^.base_size, bits_per_byte)
		 div bits_per_byte, vax_word), raddr, laddr)
    end

    (* Array assignment.  *)

    else begin	(* array assignment *)
      if dynamic_flex (lhs) then begin	(* target on heap *)
	laddr := increment_addr ( laddr, flex_arr_desc_size div byte_size );	(* skip upb word *)
	upb := upper_bound (rhs, raddr);	(* upb of rhs *)
	if dynamic_flex (rhs) orif (rhs^.opcode = ident_ref) andif
	 (rhs^.desc.base^.flexible) then begin	(* on heap or parameter *)
	  if dynamic_flex ( rhs ) then
	    raddr := increment_addr ( raddr, flex_arr_desc_size div byte_size );
	  byte_offset := rhs^.desc.base^.index_type^.minval - 1;
	  elem_size := rhs^.desc.base^.element_size div bits_per_byte;
	  if byte_offset <> 0 then begin	(* correct by lowerbound *)
	    if byte_offset > 0
	      then gen3 (subl3, int_value (byte_offset), upb, r1_addr)
	      else gen3 (addl3, int_value (-byte_offset), upb, r1_addr);
	    if elem_size > 1 then
	      gen2 (mull2, int_value (elem_size), r1_addr);
	  end
	  else if elem_size > 1 then
	    gen3 (mull3, int_value (elem_size), upb, r1_addr)
	  else gen2 (movl, upb, r1_addr);
	  block_move ( r1_addr, laddr, raddr);
	end
	else begin	(* fixed upb array *)
	  block_move ( typ_int_value (rhs^.desc.base^.base_size div bits_per_byte, vax_word), raddr, laddr);
	end;
	free (upb);
      end (* target flexible *)
      else begin	(* array with non-flex lhs *)
	if dynamic_flex (rhs)
	  then raddr := increment_addr ( raddr, flex_arr_desc_size div byte_size );
	block_move ( typ_int_value (lhs^.desc.base^.base_size div bits_per_byte, vax_word), raddr, laddr);
      end;
    end (* array assignment *);
    free (raddr);
    free (laddr);
  end;
end;
$PAGE expand_agg_constructor
(* EXPAND_AGG_CONSTRUCTOR inserts tuples after an agg_val expr tuple to
   assign the given fields or elements into a statically allocated temporary.
   The result field of the agg_val is left pointing to a copy of the addr_desc 
   for the temp (left on the heap by FETCH).  When the agg_val is eventually
   fetched, the temp's addr_desc will be returned.  The temp will have the desired
   contents at runtime, as the assign_op stmt tuples emitted by this routine will
   precede the reference to the agg_val, and therefore the code generated by
   those assign_ops will be executed before the code for the agg_val's user.  *)
  
  
procedure expand_agg_constructor (var agg_val_tpl: expr);
  
var
  previous_tpl,
  alc_temp_tpl,
  component_tpl,
  index_tpl,
  lhs_upb_tpl, rhs_upb_tpl: expr;
  temp_addr: addr_desc;
  i: oper_range;
  index_type,
  comp_type,
  cur_var,
  var_list: typ;
  tag_val: int_type;
  fields,
  cur_tag: sym;
  
procedure init_expr (var node: expr; node_type: typ);
  
  begin
    with node^ do begin
      desc.base := node_type;
      with node_type^ do begin
	desc.kind := kind;
	case kind of
	  bools, chars, ints, scalars: begin
	    desc.signed := minval < 0;
	    desc.int_prec := base_size
	  end;
	  reals:
	    desc.precision := precision;
	  strings: begin
	    desc.str_kind := str_kind;
	    desc.str_length := str_length;
	    desc.str_flex := flexible
	  end;
	  sets: begin
	    desc.set_cst_lwb := true;
	    desc.set_lwb := set_element_type^.minval;
	    desc.set_cst_len := true;
	    desc.set_length := base_size - desc.set_lwb mod bits_per_byte
	  end
	end
      end;
      result := nil;
      ref_fre := 0;
      usage_count := 1
    end
  end (* init_expr *);
  
procedure emit_assignment (lhs_tpl, rhs_tpl: expr);
  var
    assign_tpl: tuple;
  begin
    new (assign_tpl, assign_op);
    with assign_tpl^ do begin
      must_store := true;
      lrecursive := false;
      rrecursive := false;
      overlaps := false;
      lhs := lhs_tpl;
      rhs := rhs_tpl
    end;
    emit (assign_tpl)
  end;
  
begin (* expand_agg_constructor *)
  t_set (agg_val_tpl); (* tuples will be inserted after the agg_val *)
  
  new (alc_temp_tpl, alc_temp_op, alc_temp_op, 0); (* tuple to allocate target temp *)
  with alc_temp_tpl^ do begin
    desc := agg_val_tpl^.desc;
    result := nil;
    ref_fre := 0;
    usage_count := upperbound (agg_val_tpl^.operand) + 2 (* n opers + agg_val + below *)
  end;
  emit (alc_temp_tpl); (* chain tuple into I/F *)
  temp_addr := fetch ( alc_temp_tpl , no_preference );	(* fetch obtains addr_desc for temp,
					and put copy on heap *)
  agg_val_tpl^.result := alc_temp_tpl^.result; (* this "invisible" connection won't show in I/F dump *)
  
  if agg_val_tpl^.desc.kind = arrays then begin
    index_type := agg_val_tpl^.desc.base^.index_type;
    comp_type := agg_val_tpl^.desc.base^.element_type;
    for i := 1 to upperbound (agg_val_tpl^.operand) do begin
      new (index_tpl, cst_ref, cst_ref); (* constant index of element i *)
      init_expr (index_tpl, index_type);
      index_tpl^.cst_val.ival := index_type^.minval + i - 1;
      emit (index_tpl);
      new (component_tpl, array_ref, array_ref);
      init_expr (component_tpl, comp_type);
      with component_tpl^ do begin
	base_array := alc_temp_tpl;
	index_val := index_tpl
      end;
      emit (component_tpl);
      emit_assignment (component_tpl, agg_val_tpl^.operand[i]) (* tuple to assign element into temp *)
    end
  end
  
  else (* kind = records *) begin
    cur_var := agg_val_tpl^.desc.base;
    if cur_var^.variant_tag = nil then
      cur_tag := nil
    else
      cur_tag := cur_var^.variant_tag^.tag_field;
    fields := cur_var^.field_list;
  
    for i := 1 to upperbound (agg_val_tpl^.operand) do begin
     exit if fields = nil;
      if fields^.fld_variant = cur_var then begin
	new (component_tpl, field_ref, field_ref);
	init_expr (component_tpl, fields^.type_desc);
	with component_tpl^ do begin
	  base_rec := alc_temp_tpl;
	  field_sym := fields
	end;
	emit (component_tpl); (* reference field of temp *)
	emit_assignment (component_tpl, agg_val_tpl^.operand[i]); (* assign field into temp *)
	if (fields^.type_desc^.kind in [arrays, strings]) and fields^.type_desc^.flexible then begin
	  new (lhs_upb_tpl, upb_op, upb_op, 1);
	  init_expr (lhs_upb_tpl, fields^.type_desc^.index_type);
	  lhs_upb_tpl^.operand[1] := component_tpl;
	  component_tpl^.usage_count := component_tpl^.usage_count + 1;
	  emit (lhs_upb_tpl);
	  new (rhs_upb_tpl, upb_op, upb_op, 1);
	  init_expr (rhs_upb_tpl, fields^.type_desc^.index_type);
	  rhs_upb_tpl^.operand[1] := agg_val_tpl^.operand[i];
	  with agg_val_tpl^.operand[i]^ do
	    usage_count := usage_count + 1;
	  emit (rhs_upb_tpl);
	  emit_assignment (lhs_upb_tpl, rhs_upb_tpl) (* assign field's upb into temp *)
	end
      end;
      if (fields = cur_tag) or (fields^.fld_variant <> cur_var) then begin
        tag_val := agg_val_tpl^.operand[i]^.cst_val.ival; (* must be a scalar constant *)
        var_list := cur_var^.variant_tag^.first_variant; (* the variant list *)
        while (var_list <> nil) andif not ( var_list^.others_var orif
              ( (var_list^.minlab <= tag_val) and (tag_val <= var_list^.maxlab) ) ) do
          var_list := var_list^.next_variant;
        if var_list = nil then (* no variant selected - no more fields *)
          fields := nil
        else begin (* select the new variant *)
          fields := var_list^.field_list;
          if fields <> nil then
            cur_var := fields^.fld_variant; (* if fields = nil, it doesn't matter *)
          if cur_var^.variant_tag = nil
            then cur_tag := nil (* no sub-variants *)
            else cur_tag := cur_var^.variant_tag^.tag_field; (* the sub-variant tag field *)
        end;
      end
      else
        fields := fields^.next;
    end (* for i *);
  end
  
end (* expand_agg_constructor *);
$PAGE save_ap

(* SAVE_AP returns a boolean value indicating whether or not
   the argument pointer will be saved upon entry to the routine
   corresponding to parameter BLOCK. *)

function save_ap ( block: blk ): boolean;

begin
  with block^ do begin
    save_ap := (kind = subr_blk) and
	       (children <> nil) and
	       ( (parm_list.first <> nil) or
		 ( (return_sym <> nil) andif passed_by_address(return_sym) ) );
  end;
end  (* func save_ap *) ;
$PAGE prologue
(* PROLOGUE emits the entry sequence of a procedure or function. *)

procedure prologue (var addr: def);

var
  tb: def;

begin
  with cur_block^ do begin

    if prog_options.debug_opt or (trace_opt in cur_block^.semantic_options)
      then tb := trace_block	(* mark its address *)
      else tb := nil;

    (* mark the start address *)

    addr := get_def (subr_def, number);
    mark_def (code_area, addr);

    (* Emit the register save mask *)

    gen_mask (cur_block);

    (* Set the static link. *)

    if apparent_level <= 2
      then gen1 (clrl, push_reference)	(* no parent *)
      else gen1 (pushl, r0_addr);
    
    (* Set the traceblock pointer. *)

    if tb = nil
      then gen1 ( clrl, push_reference )
      else gen1 (pushal, def_addr (tb));

    (* if this block has children AND parameters, then save the
       parameter list address for use by inner routines. *)

    if save_ap ( cur_block ) then gen1 ( pushl, ap_addr );

    (* Allocate stack frame (size resolved later). *)

    gen2 (subl2, int_value (0), sp_addr);

    (* mark for fixup of allocated stack size *)

    stack_fixup := code_area.last;
    if kind = program_blk then
      gen1 (jsb, rt_addr (rt_main_frame));
      
    (* allocate handler state blocks *)
  
    neg_stk_end := neg_stk_end - 8 * hndlr_depth;
  end;
  reg_init;	(* mark all regs as free *)
  temp_max := 0;	(* no temps allocated yet *)
  DYN_DEF := Nil;	(* Pointer to save SP fixup value for dynamic temps *)
  ANY_DYNAMICS := False;	(* True if dynamic temps were used *)
end;
$PAGE case_jump
(* CASE JUMP generates code for a case jump operator and following jump in operators.
   The parameter "tpl" points to the case operator on entry; on return, it is
   set to the last jump in operator. *)

procedure case_jump (var tpl: tuple);

 var
   jmp: tuple;
   i: int_type;
   addr: addr_desc;
   d: def;

 begin
  with tpl^ do begin
    addr := fetch_fullword (cond);
    bb_end ( tpl );

    (* If next^.opcode <> jump_in_op, then there are no cases
	so we do nothing. *)

    if next^.opcode = jump_in_op then begin	(* 1 case at least *)

      (* Generate the CASE instruction. *)

      gen3 (casel, addr, int_value (low_cntrl), int_value (high_cntrl-low_cntrl));

      (* offset of from start of displacement list *)

      d := make_def (local_def);
      mark_def (code_area, d);

      jmp := next;

      (* For each iteration of the following loop, we generate the displacements
	 for a label range of the form 'n..m:' and any displacements from
	 'm' to the start of the next explicitly given range.  *)

      loop		(* over jump_in_op's, assume at least one *)

	(* Generate displacements for a label of the form 'i..j'. *)

	bb_end ( jmp );
	for i := jmp^.low_cntrl to jmp^.high_cntrl do
	  gen_displacement (d, get_def (label_def, jmp^.jump_to^.block_order_no), 2 * byte_size);
      exit if jmp^.next^.opcode <> jump_in_op;
	jmp := jmp^.next;

	(* If there is a gap between the last label range and the next one,
	   then generate displacements to the 'others' case till the gap
	   is filled. *)

	for i := jmp^.prev^.high_cntrl + 1 to jmp^.low_cntrl - 1 do
	  gen_displacement (d, get_def (label_def, tpl^.jump_to^.block_order_no), 2 * byte_size);
      end;

      if jump_to <> nil	(* at end of table gen branch to 'others' case, if present *)
	then gen_branch ( brb, get_def ( label_def, jump_to^.block_order_no ) );

      tpl := jmp;			(* advance to last jump in op *)
    end;		(* If ... = jump_in_op then begin *)
  end (* with *) ;
  free (addr);
 end;
$PAGE cond_handler_stmts
(* COND HNDLR STMTS generates code for the statement tuples associated with
   condition handling.  *)
  
procedure cond_handler_stmts (var tpl: tuple);
 
var
  addr: addr_desc;
  jump: tuple;
  defn: def;
  
begin
  with tpl^ do
    case opcode of
 
      set_handler_op, rst_handler_op: begin
	if hndlr_tuple <> nil then
	  push_address (def_addr (get_def (hnd_tab_def, hndlr_tuple^.block_order_no)))
	else
	  push_value (int_value (0), signed_value);
	if opcode = set_handler_op then
	  gen_rt (1, rt_set_handler)
	else
	  gen_rt (1, rt_rst_handler)
      end;
 
      signal_op: begin
	addr := fetch_fullword (cond_parm);
	push_address (addr);
	gen_rt (1, rt_signal)
      end;
 
      mask_op: begin
	addr := fetch_fullword (cond_parm);
	push_address (addr);
	gen_rt (1, rt_mask)
      end;
 
      unmask_op: begin
	addr := fetch_fullword (cond_parm);
	push_address (addr);
	gen_rt (1, rt_unmask)
      end;
 
      resignal_op:
	gen_rt (0, rt_resignal);
 
      exmessage_op:
	gen_rt (0, rt_exmessage);
 
      hndlr_jump_op: begin
  
	(* handler common entry code *)
  
	reset_stack; 
	addr := r0_addr;
	addr.reloc := (absolute_sc);
	gen1 (jmp, addr); (* jmp  0(r0) *)
  
	(* handler branch table *)
  
	mark_def (code_area, get_def (hnd_tab_def, jump_from^.block_order_no)); (* satisfy set_handler *)
	gen_mask (nil);
	gen1 (jsb, rt_addr (rt_exc_vaxcond));
  
	defn := make_def (local_def);
	mark_def (code_area, defn); (* mark loc of pointer to encompassing hbt *)
	if jump_from^.in_handler = nil then
	  gen_word (code_area, 0) (* no outer handler *)
	else
	  gen_displacement (defn, get_def (hnd_tab_def, jump_from^.in_handler^.block_order_no),
			    2 * byte_size);
 
	gen_word (code_area, cur_block^.neg_stk_end + (high_cntrl - 1) * 8); (* hsb's offset *)
  
	defn := make_def (local_def);
	mark_def (code_area, defn); (* mark loc of pointer to common code *)
	gen_displacement (defn, get_def (label_def, jump_from^.block_order_no), 2 * byte_size);
  
	jump := tpl;
	while jump^.next^.opcode = jump_cond_op do begin
	  jump := jump^.next;
	  addr := fetch_fullword (jump^.cond);
	  assert (addr.reloc.kind in [static_sc, external_sc]);
	  defn := make_def (local_def);
	  mark_def (code_area, defn); (* mark loc of condition *)
	  if addr.reloc.kind = static_sc then
	    gen_displacement (defn, get_ext_or_stat (static_def, addr.reloc.relsym), 4 * byte_size)
	  else if (* addr.reloc.kind = external_sc and *) addr.reloc.relsym^.standard then 
	    genindirect_word (code_area, addr.reloc.relsym)
	  else (* .kind = external_sc and not relsym^.standard *)
	    gen_displacement (defn, get_ext_or_stat (extern_def, addr.reloc.relsym), 4 * byte_size);
	  defn := make_def (local_def);
	  mark_def (code_area, defn); (* mark loc of pointer to handler *)
	  gen_displacement (defn, get_def (label_def, jump^.jump_to^.block_order_no),
			    2 * byte_size);
	end;
  
	if jump_to^.next^.opcode = resignal_op then begin
	  gen_longword (code_area, -1);
	  gen_word (code_area, 0)
	end
	else begin
	  if low_cntrl = 0 then
	    gen_longword (code_area, -1)
	  else
	    gen_longword (code_area, -2);
	  assert (jump_to^.opcode = label_node);
	  defn := make_def (local_def);
	  mark_def (code_area, defn); (* mark loc of pointer to others or allcond. handler *)
	  gen_displacement (defn, get_def (label_def, jump_to^.block_order_no), 2 * byte_size)
	end;
	tpl := jump; (* advance to last jump_cond_op *)
      end;
  
      others:
	assert (false)
 
    end (* case opcode *);
end (* cond_handler_stmts *);
$PAGE rt_seek_call
(* RT SEEK CALL generates a call to the runtime file random access routine. *)

procedure rt_seek_call ( skfil: expr; ind: expr );

 begin
  push_value ( fetch ( skfil , no_preference ) , alignment ( skfil ) );
  push_value (fetch (ind,no_preference), alignment (ind));
  gen_rt (1, rt_seek);
 end;
$PAGE value_check
 (* VALUE_CHECK generates code for value (subrange) check tuples.
    Value check tuples have 3 operands: the value, the low limit
    and the high limit.  The low and high limits are constants,
    though this code does not depend on it.  Currently no value
    check ops are generated for real subranges, though this
    code does not make that assumption either.  *)

procedure value_check ( value_chk_op: tuple );

var
  value_addr, low_addr, high_addr: addr_desc;
  vtype: vax_type;
  err_call_label, low_ok_label, high_ok_label: def;
  low_value, high_value: int_type;

begin
  with value_chk_op^ do begin

    assert ( upperbound (operand) = 3 );
    value_addr := fetch_fullword ( operand[1] );	(* fetch operands *)
    vtype := value_addr.byte_size;	(* we assume all operands are *)
    if operand[2] <> nil then begin
      low_addr := fetch_fullword ( operand[2] );
      assert ( vtype = low_addr.byte_size )	(* of the same VAX_TYPE *)
    end;
    if operand[3] <> nil then begin
      high_addr := fetch_fullword ( operand[3] );
      assert ( vtype = high_addr.byte_size )
    end;

    if operand[2] <> nil then begin
      if aconstp (low_addr, low_value) andif (low_value = 0)
	then gen1 ( tstl, value_addr )
	else gen2 ( typ_opc (cmpl, vtype), value_addr, low_addr );	(* cmp value to low limit *)
      low_ok_label := make_def ( local_def );
      gen_branch ( bgeq, low_ok_label );	(* if value >= low limit, branch *)
      if operand[3] <> nil then begin
	err_call_label := make_def ( local_def );
	mark_def ( code_area, err_call_label )
      end;
      gen1 ( jsb, make_rt_addr ( rt_val_chk ) );	(* gen call to error routine *)
      mark_def (code_area, low_ok_label)
    end;
    if operand[3] <> nil then begin
      if aconstp (high_addr, high_value) andif (high_value = 0)
	then gen1 ( tstl, value_addr )
	else gen2 ( typ_opc (cmpl, vtype), value_addr, high_addr );	(* cmp value to high limit *)
      if operand[2] <> nil then
	gen_branch ( bgtr, err_call_label )	(* if value > high limit, branch *)
      else begin
	high_ok_label := make_def (local_def);
	gen_branch (bleq, high_ok_label);
	gen1 (jsb, make_rt_addr (rt_val_chk));
	mark_def (code_area, high_ok_label)
      end;
    end;
    
    free ( value_addr );
    if operand[2] <> nil then
      free ( low_addr );
    if operand[3] <> nil then
      free ( high_addr );

  end (* with *) ;
end  (* proc value_check *) ;
$PAGE pointer_check

(* POINTER_CHECK generates code for a pointer or file check tuple.
   Tests for NIL and for zero are generated.  *)

procedure pointer_check ( ptr_check_tpl: tuple );

var
  ptr_addr: addr_desc;
  nil_cmp_label: def;
  error_call_label: def;
  error_routine_addr: addr_desc;
  nil_addr: addr_desc;

begin
  if ptr_check_tpl^.opcode = ptr_chk then begin	(* pointer check *)
    error_routine_addr := make_rt_addr ( rt_ptr_chk );
    nil_addr := int_value ( int_nil );
  end
  else begin	(* file check *)
    error_routine_addr := make_rt_addr ( rt_fil_chk );
    nil_addr := int_value ( int_nilf );
  end;

  ptr_addr := fetch ( ptr_check_tpl^.operand[ 1 ] , no_preference );
  gen1 ( tstl, ptr_addr );	(* test pointer *)
  nil_cmp_label := make_def ( local_def );
  gen_branch ( bnequ, nil_cmp_label );	(* if not 0, branch to NIL test *)
  error_call_label := make_def ( local_def );
  mark_def ( code_area, error_call_label );
  gen1 ( jsb, error_routine_addr );	(* gen call to error routine *)
  mark_def ( code_area, nil_cmp_label );
  gen2 ( cmpl, nil_addr, ptr_addr );	(* compare ptr to NIL *)
  gen_branch ( beqlu, error_call_label );	(* if equal, branch to error call *)
  free ( ptr_addr );
end (* proc pointer_check *) ;
$PAGE subscript_check

(* SUBSCRIPT_CHECK generates a subscript range check.  The INDEX
   instruction is used for subscript calculations unless the 
   subscript value is a constant.  Since the INDEX instruction
   automatically does a range check, an explicit check is generated
   by this routine only if the index value is constant.  In addition,
   PASS1 does a range check if the bounds are known at compile time.
   However the compiler will only generate a warning message when it
   finds a range error.  Thus this routine generates code only if the 
   index is constant and one of the following is true: the upperbound
   of the range is not known at compile time, or, the (constant) index
   is outside the limits of a constant bound.  *)

procedure subscript_check ( sub_check_op: tuple );

var
  index_value: int_type;
  lwb_is_constant: boolean;
  lwb_value: int_type;
  upb_is_constant: boolean;
  upb_value: int_type;
  upb_addr: addr_desc;
  upb_vtype: vax_type;
  index_error: boolean;
  index_addr: addr_desc;
  ok_label: def;
  lwb_addr: addr_desc;

begin
  with sub_check_op^ do begin

    (* Generate the check only if the index is constant.  *)

    if iconstp ( operand[1], index_value ) then begin

      (* Get the value of the (necessarily constant) lower bound and
	 determine if the upperbound is constant.  If it is, then
	 get its value also.  *)

      if operand[2] <> nil then begin
	lwb_is_constant := iconstp ( operand[2], lwb_value );
	assert ( lwb_is_constant )	(* no generic arrays !! *)
      end;
      if operand[3] <> nil then begin
	upb_is_constant := iconstp ( operand[3], upb_value );
	upb_addr := fetch ( operand[3] , no_preference );
	upb_vtype := upb_addr.byte_size
      end;

      (* If we detect a range error at compile time, then simply generate a call
	 to the error routine.  We'll check for the index below the lower bound,
	 or above the upper bound, or if the index is into a string as is outside
	 the hardware imposed limits.  *)

      index_error :=
	((operand[2] <> nil) andif (index_value < lwb_value))	
		or
	((operand[3] <> nil) andif
		     ((upb_is_constant) andif (index_value > upb_value)) or
		     ((upb_vtype = vax_word) and
			 ((index_value < 1) or (index_value > max_str_length))));

      if index_error then begin
	gen1 ( jsb, make_rt_addr ( rt_sub_chk ) );
      end

      (* The upperbound is not known at compile time.  Emit code to
	 check it at runtime.  *)

      else if operand[3] <> nil then begin
	index_addr := typ_int_value ( index_value, upb_vtype );
	gen2 ( typ_opc ( cmpl, upb_vtype ), index_addr, upb_addr );
						  (* compare the index to the upperbound *)
	ok_label := make_def ( local_def );
	if upb_vtype = vax_word
	  then gen_branch ( blequ, ok_label )	(* string lengths are unsigned *)
	  else gen_branch ( bleq, ok_label );	(* array bounds are signed *)
	gen1 ( jsb, make_rt_addr ( rt_sub_chk ) );	(* generate the error call *)
	mark_def ( code_area, ok_label );
      end  (* else *) ;

      if operand[3] <> nil then
	free ( upb_addr );

    end

    (* If we do not generate an explicit check, we still fetch the
       operands and free them so that the register usages are
       correct.  This simply means that some code is generated earlier
       than it otherwise would be.  *)

    else begin
      index_addr := fetch ( operand[1] , no_preference );
      free ( index_addr );
      if operand[2] <> nil then begin
	lwb_addr := fetch ( operand[2] , no_preference );
	free ( lwb_addr )
      end;
      if operand[3] <> nil then begin
	upb_addr := fetch ( operand[3] , no_preference );
	free ( upb_addr )
      end
    end;
  end  (* with *) ;
end  (* proc subscript_check *) ;
$PAGE compatability_check

(* COMPATABILITY_CHECK generates code for a compatability check
   (COMPAT_CHK) tuple.  Compatability checks are generated for
   array assignments when one of the arrays is flexible, and, when
   a flex array or string is passed as a VAR parameter and the type
   of the formal is not flex.  A COMPAT_CHK tuple has two parameters:
   the upperbound or dimension of each array.  *)

procedure compatability_check ( tpl: tuple );

var
  bound1: addr_desc;
  bound2: addr_desc;
  skip_label: def;

begin
  with tpl^ do begin

    bound1 := fetch_fullword ( operand[ 1 ] );
    bound2 := fetch_fullword ( operand[ 2 ] );

    gen2 ( cmpl, bound1, bound2 );
    skip_label := make_def ( local_def );
    gen_branch ( beql, skip_label );
    gen1 ( jsb, make_rt_addr ( rt_cmp_chk ) );
    mark_def ( code_area, skip_label );

    free ( bound1 );
    free ( bound2 );
  end  (* with *) ;
end  (* proc compatability_check *) ;
$PAGE substring_check

(* SUBSTRING_CHECK generates code for substring check tuples.  Three
   tests are generated:
   	1. length ( substring ) >= 0,
	2. start_index > 0,
	3. start_index + length ( substring ) - 1 <= length ( base_string ).
   The substring check tuple has three operands: the starting index of the
   substring, the length of the substring and the length of the base
   string.  *)

procedure substring_check ( check_tuple: tuple );

var
  substr_const: boolean;
  substr_value: char_range;
  index_const: boolean;
  index_value: char_range;
  base_const: boolean;
  base_value: char_range;
  emit_test1: boolean;
  emit_test2: boolean;
  emit_test3: boolean;
  error_call_label: def;
  error_routine_addr: addr_desc;
  substr_addr: addr_desc;
  index_addr: addr_desc;
  calc_length: addr_desc;
  base_addr: addr_desc;
  out_label: def;

begin
  with check_tuple^ do begin

    (* Determine if any of the three tests can be done at compile
       time.  *)

    substr_const := iconstp ( operand[ 2 ], substr_value );
    index_const := iconstp ( operand[ 1 ], index_value );
    base_const := iconstp ( operand[ 3 ], base_value );

    emit_test1 := (not substr_const) orif (substr_value < 0);
    emit_test2 := (not index_const) orif (index_value <= 0);
    emit_test3 := emit_test1 or emit_test2 or
		  (not (substr_const and index_const and base_const) orif
		    ( (index_value + substr_value - 1) > base_value) );

    if emit_test1 or emit_test2 or emit_test3
      then error_call_label := make_def ( local_def );
    error_routine_addr := make_rt_addr ( rt_str_chk );

    (* Emit the test for LENGTH( substring ) >= 0.  *)

    if emit_test1 then begin
      substr_addr := fetch_fullword ( operand[ 2 ] );
      gen1 ( tstl, substr_addr );
      gen_branch ( blss, error_call_label );
    end;

    (* Emit the test for start_index > 0.  *)

    if emit_test2 then begin
      index_addr := fetch_fullword ( operand[ 1 ] );
      gen1 ( tstl, index_addr );
      gen_branch ( bleq, error_call_label );
    end;

    (* Calculate (start_index + LENGTH( substring ) - 1) for use in the
       third and final check.  Constant components of the expression are
       accumulated at compile time.  *)

    if emit_test3 then begin
      if substr_const and index_const then begin
	calc_length := int_value ( substr_value + index_value - 1 );
      end
      else if substr_const then begin
	calc_length := add_constant ( index_addr, substr_value - 1, no_preference );
      end
      else if index_const then begin
	calc_length := add_constant ( substr_addr, index_value - 1, no_preference );
      end
      else begin
	free ( substr_addr );
	free ( index_addr );
	calc_length := reg_addr ( get_reg ( bits_per_integer ) );
	gen3 ( addl3, substr_addr, index_addr, calc_length );
	add2_constant ( calc_length, -1 );
      end;

      (* Generate code to compare the length calculated above to the
	 length of the base string.  *)

      base_addr := fetch_fullword ( operand[ 3 ] );
      gen2 ( cmpl, calc_length, base_addr );
      out_label := make_def ( local_def );
      gen_branch ( bleq, out_label );
      mark_def ( code_area, error_call_label );
      gen1 ( jsb, error_routine_addr );
      mark_def ( code_area, out_label );

      free ( base_addr );
      free ( calc_length );
    end  (* if *);

  end  (* with *) ;
end  (* proc substring_check *);
$PAGE get_next_action
(* GET NEXT ACTION scans the IF from a given tuple and returns the first tuple
   which is not a "start_stmt" operator. *)

function get_next_action (tpl: tuple): tuple;
 begin
   get_next_action := tpl^.next;
   while get_next_action^.opcode = start_stmt do begin
     get_next_action := get_next_action^.next;
   end;
 end;
$PAGE compile_body
(* COMPILE BODY compiles the body of the current block. *)

procedure compile_body ( var begin_addr: def )  options special(coercions);

 var
   tpl: tuple;				(* scanning cursor for compilation *)
   lab: tuple;
   d, d1: def;
   op: opc_range;
   reg: registers;
   i: int_type;
   mem, addr: addr_desc;
   addrp: addr_ptr;
  ret_type: typ;
  ret_size: bit_range;
  align: align_range;
  first_jump: tuple;

 begin
  rd_tuples;				(* fetch the intermediate form *)

  (* If original IF dump requested, do it. *)

  if switch (cur_block^.dump_switches, 'IFM0') then
    dmptuples ('ORIGINAL INTERMEDIATE FORM FOR BLOCK $');

  make_basic_blocks;	(* explode booleans and supply labels *)

  (* Dump IF after make_basic_blocks, if requested. *)

  if switch (cur_block^.dump_switches, 'IFM') then
    dmptuples ('INTERMEDIATE FORM AFTER MAKE_BASIC_BLOCKS FOR BLOCK $');



  prologue (begin_addr);		(* entry sequence *)

  tpl := t_chain^.first_label;
  while tpl <> nil do begin
    with tpl^ do begin
      case opcode of

	assign_op:
	  begin
	    case lhs^.desc.kind of
	      bools, ints, scalars, pointers, files, reals:
		scalar_assignment (tpl);
	      chars:
		if rhs^.desc.kind = strings
		  then str_assignment (tpl)
		  else scalar_assignment (tpl);
	      strings:
		str_assignment (tpl);
	      sets:
		set_assignment (tpl);
	      procs, funcs:
		proc_func_assignment (tpl);
	      arrays, records:
		agg_assignment (tpl) 
	    end (* case *) ;
	  end;

        start_with:
	  with_start (with_rec);
	
	end_with:
	  free (fetch (with_rec, no_preference)); (* last usage of the with_rec expr *)

	call_op:
	  procedure_call (tpl);

	label_node:
	  begin
	    d := get_def (label_def, block_order_no);
	    mark_def (code_area, d);
	    if (label_sym <> nil) andif (label_sym^.lab_nonlocal_use) then begin
	      d := get_def (sym_def, label_sym^.id_number);
	      mark_def (code_area, d);
	    end;
	    bb_start ( tpl );
	  end;

	jump_op:
	  begin
	    bb_end ( tpl );
	    if jump_to <> get_next_action (tpl)	(* generate only if useful *)
	      then gen_branch (brb, get_def (label_def, jump_to^.block_order_no));
	  end;

	jump_t_op, jump_f_op:
	  begin
	    first_jump := tpl;
	    tpl := tpl^.next;		(* skip to alternate jump op *)
	    lab := get_next_action (tpl);	(* this may be label node *)
	    if opcode = jump_t_op
	      then test_and_jump (cond, jump_to, tpl^.jump_to, lab)
	      else test_and_jump (cond, tpl^.jump_to, jump_to, lab);
	    bb_end ( first_jump );
	    bb_end ( tpl );
	  end;

	case_jump_op:
	  case_jump (tpl);

	goto_op:
	  begin
	    d := get_def (sym_def, target_lab^.id_number);	(* get label definition *)
	    if target_lab^.block^.owner = cur_block^.owner then
	      gen_branch (brb, d)
	    else if target_lab^.block^.kind = program_blk then begin (* label is in main program *)
	      gen2 (moval, def_addr (d), r0_addr);
	      gen1 ( jmp, make_rt_addr ( rt_uw_prg ) );
	    end
	    else begin (* label is in a containing routine *)
	      gen2 (moval, def_addr (d), r0_addr);
	      move_immediate (cur_block^.apparent_level - target_lab^.block^.apparent_level, r1_addr);
	      gen1 ( jmp, make_rt_addr ( rt_uw_rtn ) );
	    end;
	  end;

        gen_jump_op:
	  begin
	    bb_end ( tpl );
	    tpl := tpl^.next;	(* presumably the jump_to label *)
	    lab := tpl;		(* remember the label node *)
	    tpl := tpl^.next;	(* this should be the gen_xxxif operator *)
	    reg := get_reg (bits_per_unit);	(* get a register in which to load the value *)
	    case tpl^.opcode of
	      gen_andif_op: move_immediate (1, reg_addr (reg));
	      gen_orif_op: gen1 (clrl, reg_addr (reg))
	    end;
	    d1 := make_def (local_def);
	    gen_branch (brb, d1);
	    d := get_def (label_def, lab^.block_order_no);
	    mark_def (code_area, d);
	    bb_start ( lab );
	    case tpl^.opcode of
	      gen_andif_op: gen1 (clrl, reg_addr (reg));
	      gen_orif_op: move_immediate (1, reg_addr (reg))
	    end;
	    mark_def (code_area, d1);
	    new (addrp);
	    addrp^ := reg_addr (reg);
	    tpl^.result := ptr (ord (addrp));	(* chain to orif/andif node *)
	  end;

        dispose_op:
	  begin
	    push_value (fetch (dptrarg,no_preference), unsigned_value);
	    gen_rt (1, rt_dispose);
	  end;

	start_stmt:
	  begin
	    kill_temps;			(* reset stack if required *)
	    cur_source := stmt_source;	(* for debugging *)
	    gen_source (stmt_source);	(* comment for assembly listing *)
	    if (* first_stmt_on_line andif *) prog_options.debug_opt then begin
	      stmt_block (stmt_source, stmt_kind);
	    end;
	  end;

	stop_op:
	  gen1 (jmp, rt_addr (rt_stop));

	return_op:
	  begin
	    if (cur_block^.return_sym <> nil) (* is a function *) andif
	      not passed_by_address (cur_block^.return_sym) then begin
		mem := temp_reference;
		mem.reloc.kind := local_sc;
		mem.reloc.relsym := cur_block^.return_sym;
		ret_type := cur_block^.return_sym^.type_desc;
		mem.byte_size := unpacked_vax_type ( ret_type );
		if ret_type^.kind = sets then begin
		  alc_data ( ret_type, ret_size, align );
		  if ret_size > bits_per_unit
		    then mem.byte_size := vax_quad;
		end;
		addr := r0_addr;
		if not (mem.byte_size in [vax_byte, vax_word])
		  then addr.byte_size := mem.byte_size;
		store ( mem, addr, unsigned_value );
	    end;
	    gen_opcode (ret);
	  end;

	abort_op:
	  gen1 ( jsb, make_rt_addr(rt_ass_chk) );

	case_abort_op:
	  gen1 ( jsb, make_rt_addr(rt_case_chk) );
 
	set_handler_op,
	rst_handler_op,
	signal_op,
	mask_op,
	unmask_op,
	resignal_op,
	exmessage_op,
	hndlr_jump_op,
	jump_cond_op:
	  cond_handler_stmts (tpl);

	get_op:
	  begin
	    (*A fetch can be used since PUT/GET are not IN_STR_OP or OUT_STR_OP *)
	    push_value( fetch( file_arg, no_preference), unsigned_value );
	    if file_arg^.desc.base^.file_kind = textfile
	      then gen_rt (1, rt_get_char)
	      else gen_rt (1, rt_get);
	  end;

	put_op:
	  begin
	    (* Fetch can be used cause PUT is not IN_STR_OP or OUT_STR_OP *)
	    push_value( fetch( file_arg, no_preference), unsigned_value );
	    if file_arg^.desc.base^.file_kind = textfile
	      then gen_rt (1, rt_put_char)
	      else gen_rt (1, rt_put);
	  end;

	readln_op:
	  begin
	    rt_io_call (rt_readln);
	    free ( fetch ( file_arg , no_preference ) );	(* fetch and free file variable so *)
						(* reg usave counts are correct *)
	  end;

	writeln_op:
	  begin
	    rt_io_call (rt_writeln);
	    free ( fetch ( file_arg , no_preference ) );	(* fetch and free file variable so *)
						(* reg usage counts are correct *)
	  end;

	page_op:
	  begin
	    rt_io_call (rt_page);
	    free ( fetch ( file_arg , no_preference ) );
	  end;

	clear_op:
	  begin
	    rt_io_call (rt_clear);
	    free ( fetch ( file_arg , no_preference ) );
	  end;

	break_op:
	  begin
	    rt_io_call (rt_break);
	    free ( fetch ( file_arg , no_preference ) );
	  end;

	empty_op:
	  begin
	    rt_io_call (rt_empty_text);
	    free ( fetch ( file_arg , no_preference ) );
	  end;

	close_op:
	  begin
	    rt_io_call (rt_close);
	    free ( fetch ( file_arg , no_preference ) );
	  end;

	scratch_op:
	  begin
	    rt_io_call (rt_scratch);
	    free ( fetch ( file_arg , no_preference ) );
	  end;

	start_io_op:
	  io_begins (tpl);

	end_io_op:
	  io_ends ( tpl );

	read_op:
	  read_write_call (tpl);

	write_op:
	  read_write_call (tpl);

	seek_op:
	  rt_seek_call ( seek_file , seek_index);

	close_all_op:
	  begin
	    gen_rt (0, rt_close_all);
	  end;

        val_range_chk:
	  value_check ( tpl );

	file_chk,
        ptr_chk:
	  pointer_check ( tpl );

        sub_range_chk:
	  subscript_check ( tpl );

	str_range_chk:
	  substring_check ( tpl );

	compat_chk:
	  compatability_check ( tpl );
	agg_val:
	  expand_agg_constructor (tpl); (* insert assignments into a temp *)

	others:
	  (* forget it *)

      end (* case *) ;
    end (* with *) ;
    tpl := tpl^.next;	(* this must be outside the with *)
  end;

  (* Clean up debug stuff. *)

  if prog_options.debug_opt
    then blk_end;

  (* Resolve allocated stack size and suppress allocation if zero. *)

  i := ngm (cur_block^.neg_stk_begin + temp_max - cur_block^.neg_stk_end, bytes_per_unit);
  if dyn_def <> nil then begin  	(* If dynamics must fix up definition record for SP *)
    DYN_DEF^.DEFINED := True;
    DYN_DEF^.ADDR := -i + cur_block^.neg_stk_begin
  end;
  (* 1st pass always allocates 4 bytes for save of ap - if we're really saving
     it, pushl from prologue took care of it.  If we're not, and no locals or
     temps follow it (whose offsets we can't screw up) then dispense with the space. *)
  if save_ap ( cur_block ) or (i = 4)
    then i := i - 4;
  if i > 0
    then stack_fixup^.operands[1].offset := i
    else stack_fixup^.kind := nullcode;	(* To ignore operation *)

  (* Verify that all registers were freed.  *)

  for i := 2 to max_reg do
    assert ( regdesc[ i ].uses_remaining = 0 );

  fix_branches (code_area, loc_code);

  emit_code (code_area, loc_code, psect_id [code_psect], cur_block^.semantic_options);


  (* Dump the final IF , if requested *)

  if switch (cur_block^.dump_switches, 'FINAL') then begin
    reclaim;
    dmptuples ('FINAL INTERMEDIATE FORM FOR BLOCK $')
  end;

  del_def_list (label_def);
  del_def_list (local_def);
  del_tuples;			(* get rid of IF for this block *)
 end;
$PAGE compile_subr
(* COMPILE SUBR generates code for the body of a procedure or function.  It is
   assumed that cur_block points to the block node for the subroutine on entry. *)

procedure compile_subr;

 var
   entryaddr: def;

 begin
  gen_cmt (code_area, 'Begin subroutine ' || cur_block^.subr_sym^.name^.text);

  compile_body (entryaddr);				(* compile the body of the subroutine *)
  cur_block^.subr_sym^.item_addr := entryaddr^.addr;	(* resolve entry address of routine *)
 end;
$PAGE compile_main
(* COMPILE MAIN generates code for the body of a main program.  It is assumed that
   cur_block points to the program's block node at entry. *)

procedure compile_main (var startaddr: def);

begin
  gen_cmt (code_area, 'Begin program ' || cur_block^.id^.text);

  compile_body (startaddr);	(* generate code for body of block *)
end;
$PAGE gen_code
(* GEN CODE is the driving program for the code generator.  It directs initialization,
   generation, and termination. *)

public procedure gen_code ( var code_size, const_size, static_size: unit_range );

var
  last_cst: code;
  startaddr: def; (* start address of main program, nil otherwise *)
  debfile: text;

begin
  cur_block := root_block^.children;	(* set immediately in case of assertion failure *)
  cur_source := (0, 0, 0);		(*  "       "      "   "    "     "        "    *)
 
  startaddr := nil;			(* init globals in this module *)
  p4_error_level := no_errors;	(* no pass4 errors so far *)
  gen_init;	(* and others *)
  exp_init;
  if assembly_opt in all_opts
    then mac_header;
  if map_opt in all_opts
    then map_init;
  rel_init;
  if prog_options.debug_opt then
    deb_init;

  (* ASsign psect id's. *)

  static_size := size_init + size_cond + size_uninit;
  if static_size <> 0 then begin
    psect_id[static_psect] := 0;
    psect_id[code_psect] := 1;
  end
  else (* no static storage *)
    psect_id[code_psect] := 0;
  init_static (static_size);

  (* If not a datamodule, identify the code psect *)

  if root_block^.children^.kind <> data_blk
    then gen_origin (code_area, code_psect);

  (* Compile the constituent blocks in reverse depth first order. *)

  cur_block := root_block;		(* look for end of call chain *)
  while cur_block^.downward_call_thread <> nil do cur_block := cur_block^.downward_call_thread;

  while cur_block <> root_block do begin	(* compile in reverse order *)
    case cur_block^.kind of 
      program_blk:    compile_main (startaddr);
      subr_blk:	      compile_subr
    end;
    cur_block := cur_block^.upward_call_thread;
  end;
  cur_block := root_block^.children; (* in case of later assertion failure *)

  (* Final cleanup *)

  loc_cst := loc_code; (* normally, consts go at end of code area *)
  pool_constants (loc_cst);
  emit_code (cst_area, loc_cst, psect_id [code_psect], prog_options.semantic_options);
  if prog_options.debug_opt then begin
    fp_blocks;	(* dumps page and file blocks in code area *)
    emit_code (code_area, loc_cst, psect_id [code_psect], prog_options.semantic_options);
  end;

  (* If this is an overlay compilation generate the indirect word used to access
     public vars and constants *)

  if prog_options.overlay_opt then begin
    loc_cst := ngm ( loc_cst , bytes_per_unit );	(* must longword align *)
    ovly_emit_indirect_words;
    emit_code ( cst_area, loc_cst, psect_id[code_psect], prog_options.semantic_options )
  end;

  code_size := loc_code;
  const_size := loc_cst - loc_code;
  if assembly_opt in all_opts then
    mac_end (startaddr, code_size, const_size, static_size);	(* terminate assembly listing *)
  if map_opt in all_opts 
    then map_print;
  rel_end (startaddr, code_size + const_size, static_size);
  gen_term;	(* termination routine for module VAXCGU *)

  (* Close the dump file, if necessary. *)

  dmp_close;
end.
} Ð