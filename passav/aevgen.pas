$TITLE AEVGEN - AEV quick pass code generator

module aevgen;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasfil.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM aevcg.typ
$SYSTEM aevcgu.inc
$SYSTEM aevmac.inc
$SYSTEM aevopc.inc
$SYSTEM pasifu.inc
$SYSTEM aevexp.inc
$SYSTEM aevcll.inc
$SYSTEM aevcmp.inc
$SYSTEM aevstr.inc
$system aevset.inc
$SYSTEM aevrel.inc
$SYSTEM aevcsp.inc
$SYSTEM aevdeb.inc
$SYSTEM pasjmp.inc
$SYSTEM pastal.inc
$SYSTEM passw.inc
$SYSTEM pa2dmp.inc
$SYSTEM pasdmp.inc
$system aevutl.inc
$system aevio.inc
$system pasmth.inc
$system pascv.inc
$SYSTEM pasmap.inc
$system ptmcon.inc
$PAGE global variables

public var

  startaddr: def;			(* start address of main program, nil otherwise *)

  low_base: def;	(* direct static area relocatable base *)

  vlow_base: def;	(* virtual static area relocatable base *)

  high_base: def;	(* code/constant area relocatable base *)

  p4_error_level: error_levels;	(* max severity of any pass4 error *)

static var

  temp_base: unit_range;	(* top of temps in local stack frame *)

  temp_max: unit_range;		(* offset of max allocated temps *)

  stack_fixup: code;		(* for resolution of stack frame size *)


  ANY_DYNAMICS : Boolean;	(* Any dynamic temporaries for this statement ? *)

  DYN_DEF : DEF;		(* Definition record used to clean up the
				   stack because of dynamic temps *)
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

  writeln ( tty, 'Fatal error at ', cv_source_id ( cur_source ) );
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
$PAGE bb_start

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

procedure bb_start ( label_node_tuple: tuple )  options special(coercions);

var
  restore_addr : addr_desc;

begin

  (* Reset the flag indicating whether dynamic temps have been allocated
     in the current basic block.  *)

  any_dynamics := false;

  (* Generate an instruction to deallocate dynamic temps.  Note
     that the stack offset in the restore address is a forward
     reference.  The value is represented here by a definition 
     record and resolved at the end of the compilation of the
     current routine. *)

  restore_addr := fp_addr;
  restore_addr.reloc.kind := def_sc;
  if dyn_def = nil			(* static cell for defn ptr *)
    then dyn_def := make_def ( temp_size_def );
  restore_addr.reloc.reldef := dyn_def;
  gen2 ( moval, restore_addr, sp_addr );

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
$PAGE rslv_dyn_def
Procedure RSLV_DYN_DEF ( N : INT_TYPE );

(* Resolve the definition record for the dynamic temporaries. *)

Begin
  If DYN_DEF <> Nil
    Then Begin
      DYN_DEF^.DEFINED := True;
      DYN_DEF^.ADDR := - ( N + 12 )
    End
End;
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

procedure init_static  options special(coercions);

 var
   block: blk;
   symbols: sym;
   reld: rel_syllable;
   need_origin: boolean;
   id: byte;
   static_counter, actual_size, tsize: bit_range;
   talign: align_range;
   static_size: unit_range;

 begin

  static_size := size_init + size_uninit + size_cond;
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

  emit_code (static_area, loc_static, psect_id [static_psect], prog_options.semantic_options);
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

begin
end;
$PAGE scalar_assignment
(* SCALAR ASSIGNMENT generates code for assignments of scalar values. *)

procedure scalar_assignment (tpl: tuple);

 var
   laddr, raddr: addr_desc;

 begin
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
  end (* with *);
end (* proc_func_assignment *);
$PAGE block_move

(* BLOCK_MOVE moves a specified number of bytes of uninterpreted
   data from one location to another.  Paramerer LEN_ADDR is an 
   address descriptor for the length in bytes of the block;
   SOURCE_ADDR is the base address of the source and DEST_ADDR
   is the base address of the destination.   *)

procedure block_move ( len_addr, source_addr, dest_addr: addr_desc );

var
  length_value: unit_range;
  regs_saved: set_of_registers;

begin
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
	  lhs_upb_tpl^.operand[1] :=onent_tpl;
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

    (* Allocate stack frame (size resolved later). *)

    gen_rm (save, r0, absolute_reference);

    (* mark for fixup of allocated stack size *)

    stack_fixup := code_area.last;

    (* Set up entry through runtime. *)

    gen_rt (jsr, rt_enter);
    gen_word (code_area, - argblocksize - 3);
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

    if next^.opcode = jump_in_op	(* 1 case at least *)
      then begin

	(* Generate the CASE instruction. *)

	gen3 (typ_opc (casel, addr.byte_size), addr, int_value (low_cntrl),
	  int_value (high_cntrl-low_cntrl));

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
	    gen_address (get_def (label_def, jmp^.jump_to^.block_order_no), 2 * byte_size);
	exit if jmp^.next^.opcode <> jump_in_op;
	  jmp := jmp^.next;

	  (* If there is a gap between the last label range and the next one,
	     then generate displacements to the 'others' case till the gap
	     is filled. *)

	  for i := jmp^.prev^.high_cntrl + 1 to jmp^.low_cntrl - 1 do
	    gen_address (get_def (label_def, tpl^.jump_to^.block_order_no), 2 * byte_size);
	end;

	if jump_to <> nil	(* at end of table gen branch to 'others' case, if present *)
	  then gen_branch ( brb, get_def ( label_def, jump_to^.block_order_no ) );

	tpl := jmp;			(* advance to last jump in op *)
      end;		(* If ... = jump_in_op then begin *)
  end (* with *) ;
  free (addr);
 end;
$PAGE rt_seek_call
(* RT SEEK CALL generates a call to the runtime file random access routine. *)

procedure rt_seek_call ( skfil: expr; ind: expr );

 begin
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
  value_addr: addr_desc;
  low_addr: addr_desc;
  high_addr: addr_desc;
  vtype: vax_type;
  cmp_opcode: opc_range;
  high_cmp_label: def;
  err_call_label: def;
  low_value: int_type;
  high_value: int_type;
  low_is_minint: boolean;
  high_is_maxint: boolean;
  low_const: boolean;
  high_const: boolean;

begin
  with value_chk_op^ do begin
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
	
	(* END_WITHs - The expression for the with  record is not fetched and freed
	   because with regs are (currently) handled outside the normal
	   register allocation/deallocation mechanism.  *)

	end_with:
	  with_end;

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
	      then gen_branch (get_def (label_def, jump_to^.block_order_no));
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
	      gen_branch (d)
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
	      gen_andif_op: gen_rr (sub + carry1 + shift_left + skp, reg, reg);
	      gen_orif_op: gen_rr (sub + skp, reg, reg)
	    end;
	    d := get_def (label_def, lab^.block_order_no);
	    mark_def (code_area, d);
	    bb_start ( lab );
	    case tpl^.opcode of
	      gen_andif_op: gen_ri (lef, reg, 0);
	      gen_orif_op: gen_ri (lef, reg, 1)
	    end;
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
	  end;

	abort_op:
	  gen1 ( jsb, make_rt_addr(rt_ass_chk) );

	case_abort_op:
	  gen1 ( jsb, make_rt_addr(rt_case_chk) );

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
  RSLV_DYN_DEF ( I );	(* If dynamics must fix up definition record for SP *)
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

procedure compile_main;

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
  debfile: text;
begin
  startaddr := nil;			(* init globals in this module *)
  p4_error_level := no_errors;	(* no pass4 errors so far *)
  gen_init;	(* and others *)
  exp_init;
  low_base := make_def (code_def);
  def_value (low_base, loc_static, true);
  vlow_base := make_def (code_def);
  def_value (vlow_base, loc_static, true);
  high_base := make_def (code_def);
  def_value (high_base, loc_code, true);
  if assembly_opt in all_opts
    then mac_header;
  if map_opt in all_opts
    then map_init;
  rel_init;
  if prog_options.debug_opt then
    deb_init;

  init_static;

  (* If not a datamodule, identify the code psect *)

  if root_block^.children^.kind <> data_blk
    then gen_origin (code_area, code_psect);
  (* Compile the constituent blocks in reverse depth first order. *)

  cur_block := root_block;	(* look for end of call chain *)
  while cur_block^.downward_call_thread <> nil do cur_block := cur_block^.downward_call_thread;

  while cur_block <> root_block do begin	(* compile in reverse order *)
    case cur_block^.kind of 
      program_blk:    compile_main;
      subr_blk:	      compile_subr
    end;
    cur_block := cur_block^.upward_call_thread;
  end;

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
   ;5iu