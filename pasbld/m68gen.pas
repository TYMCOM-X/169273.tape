$TITLE M68GEN - MC68000 quick pass code generator

module m68gen options check;
$PAGE includes
$SYSTEM pasdmp.inc
$SYSTEM pa2dmp.inc
$SYSTEM pascal.inc
$SYSTEM ptmcon.inc
$SYSTEM pasfil.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM pasifu.inc
$SYSTEM m68opt.inc
$SYSTEM m68cgu.inc
$SYSTEM m68utl.inc
$SYSTEM m68exp.inc
$SYSTEM m68str.inc
$SYSTEM m68set.inc
$SYSTEM m68io.inc
$SYSTEM m68cmp.inc
$SYSTEM m68cll.inc
$SYSTEM m68emt.inc
$SYSTEM pasjmp.inc
$SYSTEM passw.inc
$SYSTEM pasmth.inc
$SYSTEM pastal.inc
$SYSTEM pascv.inc
$PAGE declarations

var

  temp_start, (* start of temps in local stack frame *)

  temp_base, (* top of temps in local stack frame *)

  temp_max: unit_range; (* maximum attained by temp_base over procedure *)

  save_sp_inst: code; (* loc in block's code list of instruction 
			 to save SP's contents (for restoration after
			 dynamic temps)  *)

  save_sp_desc: op_desc; (* descriptor for static temporary used to
			    save SP's contents *)

  dyn_in_cur_bb: boolean; (* flag indicating whether any dynamic temporaries
			     have been allocated in the current basic block *)

  num_withs_saved: unit_range; (* number of with record addresses stacked in
				  a statically allocated save area, at the current
				  time.  *)

  max_withs_saved: unit_range; (* maximum attained by num_withs_saved during
				  compilation of current block. *)

  savewith_desc: op_desc; (* descriptor for the first longword of the
			      statically allocated save area.  *)
$PAGE fatal_error
(* FATAL ERROR is called in the event of an unrecoverable error in the
   code generator.  *)

public procedure fatal_error (message: packed array [1..*] of char);

var
  n: nam;

begin

  trace;

  (* Write an error message to the terminal. *)

  if cur_block^.kind = subr_blk then
    n := cur_block^.subr_sym^.name
  else
    n := cur_block^.id;
  writeln (tty, '?Fatal error in block ' || n^.text || ' at ' ||
			cv_source_id (cur_source));
  writeln (tty, '   - ', message);
  break (tty);

  emt_fatal; (* blow away current rel file leaving previous one *)
  stop

end (* fatal_error *);
$PAGE get_temp
(* GET TEMP allocates a temporary of the specified size.  If the size requested
   is constant, the temporary will be statically allocated and have a duration of
   one source statement.  KILL TEMPS will release the space allocated here when
   the next start_statement tuple is encountered.  If the size is nonconstant,
   the temporary will by dynamically allocated on top of the stack, and will
   have a duration of one basic block. 

   The returned op_desc will be of the form -n(SP) if static, and (An) (where
   An contains a copy of SP's contents after allocation) if dynamic.   *)

public function get_temp (size: op_desc; len_in_words: boolean): op_desc;

var
  len: integer;
  areg: addr_regs;
  temp_size: op_desc;

begin

  (* Statically allocated temporary. *)

  if aconstp (size, len) then begin
    if len_in_words then
      len := len + len; (* convert to bytes *)
    temp_base := ngm (temp_base + len, 2);
    assert (temp_base <= 32767); (* stack frame offsets are 16 bit signed *)
    temp_max := max (temp_max, temp_base);
    get_temp := descriptors [displacement_mode];
    (* If size in [1,2,4], set value_size field.  Otherwise leave as no_size. *)
    if len = 1 then
      get_temp.value_size := size_byte
    else if len = 2 then
      get_temp.value_size := size_word
    else if len = 4 then
      get_temp.value_size := size_long;
    get_temp.reg := fp;
    get_temp.cst_part.offset := -temp_base (* minus since allocating toward lower addrs *)
  end

  (* Dynamically allocated temporary. *)

  else begin
    stk_bash;			(* force save of any operands on the stack *)
    dyn_in_cur_bb := true; (* we'll need to know at end of basic block *)
    temp_size := coerce (size, data_modes, dreg_mode, long_only, false);

    if chk_stk_opt in cur_block^.semantic_options then begin
      push (temp_size, size_long); (* MOVE.L	<temp_size>,-(SP) *)
      if len_in_words then
	gen_rt (rt_dyntemp_w) (* JSR		rt_dyntemp_w *)
      else
	gen_rt (rt_dyntemp_b); (* JSR		rt_dyntemp_b *)
      areg := get_areg;
      gen_rr (move_opc, sp, areg, size_long) (* MOVEA.L	SP,An *)
    end

    else (* inline *) begin
      temp_size := copy_dreg (temp_size);
      free_desc (temp_size);
      if len_in_words then
	gen_ir (asl_opc, 1, temp_size.reg, size_long)  (* ASL.L		#1,Dn *)
      else begin
	gen_ir (add_opc, 1, temp_size.reg, size_long); (* ADDQ.L	#1,Dn *)
	gen_ir (bclr_opc, 0, temp_size.reg, size_long) (* BCLR.L	#0,Dn *)
      end;
      gen_rr (sub_opc, temp_size.reg, sp, size_long); (* SUBA.L	Dn,SP *)
      areg := get_areg;
      gen_rr (move_opc, sp, areg, size_long) (* MOVEA.L	SP,An *)
    end;
    get_temp := descriptors [indirect_mode];
    get_temp.reg := areg
  end
end (* get_temp *);
$PAGE kill_temps
(* KILL TEMPS resets the starting offset for static temporaries, effectively
   deallocating any active static temporaries.  *)

procedure kill_temps;

begin
  temp_base := temp_start
end;
$PAGE double floating temporary stack
(* Double-precision floating-point calculations use static temporaries
   for their intermediate results.  DFSTACK is a stack of temporaries
   used in a single statement, and is manipulated by DFTEMP, DFPOP,
   DFPUSH_DUMMY, and DFTERM. *)

type
    dflist = ^ dflist_record;
    dflist_record = record
	next, prev : dflist;
	dummy_count : 0 .. maximum (integer);
	desc : op_desc;
    end;

var dfstack : dflist;
    dftop : dflist;
    dfcurrent : dflist;

static var
    dfbase : dflist_record;
$PAGE dfinit
(*  DFINIT initializes the information for keeping track of double-precision
    floating-point temporaries within statements.  *)

procedure dfinit  options special (coercions);

begin
  dfstack := address (dfbase);
  dftop := dfstack;
  dfcurrent  := dfstack;
  dfcurrent^.dummy_count := 0;
end (* dfinit *);
$PAGE dftemp
(* DFTEMP will return a descriptor for a double-word temporary that is not
   now in use for anything else. *)

public function dftemp () : op_desc;

begin
  if dfcurrent = dftop then begin
    dftemp := get_temp (int_desc (4, no_size, false), true (* word count *));
    dftemp.value_size := size_double;
    dftemp.extended_size := size_double;
    new (dfcurrent);
    dfcurrent^ := (nil, dftop, 0, dftemp);
    dftop^.next := dfcurrent;
    dftop := dfcurrent;
  end

  else begin
    dfcurrent := dfcurrent^.next;
    dftemp := dfcurrent^.desc;
  end;
end (* dftemp *);
$PAGE dfpush_dummy
(*  DFPUSH DUMMY indicates that a double-precision floating-point variable
    has been fetched, so the next DFPOP should not free an allocated DF
    temporary.  *)

public procedure dfpush_dummy;

begin
  dfcurrent^.dummy_count := dfcurrent^.dummy_count + 1;
end (* dfpush_dummy *);
$PAGE dfpop
(* DFPOP indicates that the most recently allocated DF temporary is now
   available again. *)

public procedure dfpop;

begin
  with dfcurrent^ do begin
    if dummy_count = 0 then
      dfcurrent := prev
    else
      dummy_count := dummy_count - 1;
  end;
end (* dfpop *);
$PAGE dfterm
(* DFTERM is called at the end (actually the beginning) of each statement,
   and discards the current DF stack. *)

procedure dfterm;

begin
  assert ( (dfcurrent = dfstack) and (dfcurrent^.dummy_count = 0) );
  while dftop <> dfstack do begin
    dfcurrent := dftop^.prev;
    dispose (dftop);
    dftop := dfcurrent;
  end;
end (* dfterm *);
$PAGE compute_quick_dominators
(*  COMPUTE QUICK DOMINATORS computes the immediate dominator of each block
    in the symbol table.  The immediate dominator of a non-quick block is
    always NIL.  The immediate dominator of a quick block B is the dominator
    of B which doesn't dominate any other dominator of B.  A block B1 is a
    dominator of a block B if, whenever B is active, B1 must be, too.

    The LEX_THREAD field of each block is set to point to its immediate
    dominator.  The PARM_LIST_BASE is used as an internal flag; at the
    conclusion of this routine, it will be zero.  *)

procedure compute_quick_dominators;

var b, c, d : blk;
    cc : call_link;

begin

  (*  Initialize the immediate dominator pointers and mark flags.  *)

  b := root_block;
  while b <> nil do begin
    b^.lex_thread := nil;
    b^.parm_list_base := 0;
    b := b^.downward_call_thread;
  end;

  (*  Process each block in turn.  *)

  b := root_block;
  while b <> nil do begin

    (*  Process each routine C with the same owner as B (other than
	the owner of B itself) which is called by B.  *)

    cc := b^.calls;
    while cc <> nil do begin
      c := cc^.called_subr;
      if (c^.owner = b^.owner) and (c <> b^.owner) then begin

	(*  If B is the first caller of C to be processed, set B to
	    be the (tentative) immediate dominator of C.  *)

	if c^.lex_thread = nil then
	  c^.lex_thread := b

	(*  Otherwise, the immediate dominator of C must be an existing
	    dominator of C which is also a dominator of B.  *)

	else begin

	  (*  First, mark B and all its dominators.  *)

	  d := b;
	  while d <> nil do begin
	    d^.parm_list_base := 1;
	    d := d^.lex_thread;
	  end;

	  (*  Now find an existing dominator of C which is also
	      a dominator of B (i.e., which has been marked).  *)

	  d := c^.lex_thread;
	  while d^.parm_list_base <> 1 do
	    d := d^.lex_thread;

	  (*  Set the immediate dominator of C to be the common dominator,
	      and unmark all the marked blocks.  *)

	  c^.lex_thread := d;
	  d := b;
	  while d <> nil do begin
	    d^.parm_list_base := 0;
	    d := d^.lex_thread;
	  end;
	end (* if C already has a dominator *);
      end (* if B and C have the same owner *);

      cc := cc^.rlink;
    end (* while cc <> nil *);
    b := b^.downward_call_thread;
  end (* while b <> nil *);
end (* compute_quick_dominators *);
$PAGE compute_parm_area_sizes
(*  COMPUTE PARM AREA SIZES will set the PARM_LIST_BASE field of each routine
    the total space required for the parameter blocks of the routines which
    are immediately dominated by that routine.  *)

procedure compute_parm_area_sizes;

var b : blk;

begin
  b := root_block;
  while b <> nil do begin
    with b^ do begin
      if (kind = subr_blk) and (lex_thread <> nil) then
	lex_thread^.parm_list_base := lex_thread^.parm_list_base +
				      subr_sym^.type_desc^.parmlist_size;
      b := downward_call_thread;
    end;
  end;
end (* compute_parm_area_sizes *);
$PAGE offset_symbols
(*  OFFSET SYMBOLS adds an offset, DELTA, to the ITEM_ADDR field of every
    local symbol in a list, LIST.  *)

procedure offset_symbols ( list : sym; delta : unit_range );

var s : sym;

begin
  s := list;
  while s <> nil do begin
    with s^ do begin
      if (kind in [vars, values]) andif (dcl_class in [local_sc, parameter_sc]) then
	item_addr := item_addr + delta;
      s := next;
    end;
  end;
end (* offset_symbols *);
$PAGE relocate_stack_frames
(*  RELOCATE STACK FRAMES will recompute the addresses for all local variables
    and parameters in quick blocks.  In effect, it repeats the action of the
    the first-pass storage allocator, allowing space for parameter blocks as
    well as for variables.  *)

procedure relocate_stack_frames;

var b, last : blk;
    pbase : unit_range;
    cc : call_link;

begin

  (*  First, go through all the routines, saving their stack begins in their
      stack end fields and zeroing their stack begin fields.  Also, set LAST
      to the last routine in the downward call list.  *)

  b := root_block;
  while b <> nil do begin
    with b^ do begin
      neg_stk_end := neg_stk_begin;
      neg_stk_begin := 0;
      last := b;
      b := downward_call_thread;
    end;
  end;

  (*  Next, compute the locations of individual stack sub-frames, and relocate
      variable and parameter symbols.  *)

  b := root_block;
  while b <> nil do begin
    with b^ do begin

      (*  If the block is quick, relocate its variables and parameters.  If
	  it is non-quick, just relocate the parameters.  *)

      if owner = b then begin
	if level <= 2
	  then pbase := 20
	  else pbase := 24;
      end
      else begin
	pbase := lex_thread^.parm_list_base - subr_sym^.type_desc^.parmlist_size;
	lex_thread^.parm_list_base := pbase;
      end;
      offset_symbols (id_list.first, neg_stk_begin - neg_stk_end);
      offset_symbols (parm_list.first, pbase);
      offset_symbols (return_sym, pbase);

      (*  Set the bounds for the stack sub-frame for this routine, with the
	  PARM_LIST_BASE set to the starting address for contained parameter
	  lists.  *)

      neg_stk_end := neg_stk_begin - neg_local_size - parm_list_base;
      parm_list_base := neg_stk_begin - neg_local_size;

      (*  Any routines which are called by this routine must have sub-frames
	  which begin after the end of this routine's sub-frame.  *)

      cc := calls;
      while cc <> nil do begin
	with cc^ do begin
	  if (called_subr^.owner = owner) and
	     (called_subr <> owner) and
	     (called_subr <> b) then
	    called_subr^.neg_stk_begin := min (called_subr^.neg_stk_begin, neg_stk_end);
	  cc := rlink;
	end;
      end;

      b := downward_call_thread;
    end (* with b^ *);
  end (* while b <> nil *);

  (*  Now run back up the list, computing the true NEG_STK_END for each
      owner routine.  *)

  b := last;
  while b <> nil do begin
    with b^ do begin
      cc := calls;
      while cc <> nil do begin
	with cc^ do begin
	  if called_subr^.owner = owner then
	    neg_stk_end := min (neg_stk_end, called_subr^.neg_stk_end);
	  cc := rlink;
	end;
      end;
      b := upward_call_thread;
    end;
  end;
end (* relocate_stack_frames *);
$PAGE get_next_action
(* GET NEXT ACTION scans the IF from a given tuple and returns the first tuple
   which is not a "start_stmt" operator.  *)

function get_next_action (tpl: tuple): tuple;

begin
  get_next_action := tpl^.next;
  while get_next_action^.opcode = start_stmt do
    get_next_action := get_next_action^.next
end;
$PAGE bb_start, reset_stack
(* BB START performs processing which must be done on entry to
   each basic block (i.e., at each LABEL_NODE tuple).  In particular
   it generates an instruction on entry to each basic block which
   deallocates any active dynamic temps.  If the label is the target
   of a nonlocal goto or the label is already known to be the target of a
   branch out of a block which allocated dynamic temps, then the code
   record is simply generated normally.  Otherwise, the deallocation
   may in fact be unnecessary so the code record is generated, but is not "activated".
   If the deallocation turns out to be necessary, then at the end of some
   basic block which this one succeeds the instruction will be activated.
  
   The flag indicating whether dynamic temps were allocated in the
   current basic block is always reset by this routine.  *)

procedure reset_stack (really_required: boolean);

begin
  gen_mr (move_opc, save_sp_desc, sp);
  if really_required then
    save_sp_inst^.active := true (* ensure saving instr. is activated *)
  else
    code_area.last^.active := false (* deactivate reset instr. for time being *)
end (* reset_stack *);



procedure bb_start (label_node_tuple: tuple)  options special(coercions);

begin

 (* Reset the flag indicating whether dynamic temps have been allocated 
    in the current basic block.  *)

  dyn_in_cur_bb := false;

  (* The reset instruction will be left active and the SP saving instruction
     will be activated if the basic block is the target of a non-local
     goto, or it is already known to be the successor of a block which uses
     dynamic temps.

     Note that we overload the DOM_SON field of the LABEL_NODE and use it as
     a boolean flag indicating whether or not the block is the target of a
     branch from a block using dynamic temps.  This field is only used when
     the jump tuple is processed before the LABEL_NODE tuple.  The IDOM
     field of the LABEL_NODE is also overloaded; it is coerced to point to
     the reset code record allocated.  *)

  with label_node_tuple^ do begin
    assert (opcode = label_node);

    (* Generate an instruction to deallocate dynamic temps.   *)

    reset_stack ( ( (label_sym <> nil) andif (label_sym^.lab_nonlocal_use) ) or
		  (dom_son <> nil) );
    idom := tuple (code_area.last);
  end (* with *);

end (* bb_start *);
$PAGE bb_end
(* BB END does processing necessary at the end of a basic block. In
   particular, if dynamic temps were used in the basic block terminated
   by the jump, then the label node for the basic block being branched
   to by the jump is flagged as requiring dynamic temp deallocation
   (stack top reset).  This "flagging" is done in one of two ways.  If
   the label node for the target has already been processed then the
   IDOM field of the LABEL_NODE has been coerced to point to the code
   record for the stack reset instruction; in this case we just make sure
   it's "activated".  If the LABEL_NODE for the target has not yet been
   processed, then we flag the LABEL_NODE by setting its DOM_SON field
   to any non-nil value.  *)

procedure bb_end (jump_tuple: tuple)  options special(coercions);

begin
  if dyn_in_cur_bb then begin
    with jump_tuple^.jump_to^ do begin
      assert (opcode = label_node); (* should be start of successor block *)
      if idom = nil then (* label_node not processed yet *)
        dom_son := jump_tuple (* set to any non-nil value *)
      else begin
	code (idom)^.active := true; (* activate reset instruction *)
	save_sp_inst^.active := true (* and ensure saving inst active too *)
      end
    end (* with *);
  end
end (* bb_end *);
$PAGE init_static
(* INIT STATIC generates code for the initialized part of the static area, and
   fordeclared public constants.  Symbols are processed in the order:
	symbols within the id list of a block
	blocks in downward_call_thread order
   This is the order in which they are processed during storage allocation. *)

procedure init_static (var static_size, public_const_size: unit_range)  options special(coercions);

var
  block: blk;
  symbols: sym;
  reld: reloc_value;
  size_contribution: unit_range;
  init_static_area,
  cond_area,
  uninit_static_area: code_list;

begin

  (* We cannot tell until link time how big the static area will really be,
     but we can at least get an immediate indication of whether that portion of
     the static area that this module defines is impossibly big. *)

  assert ((size_init + size_cond + size_uninit) <= 65536);

  (* Prepare the various area lists. *)

  cdl_init (init_static_area);
  gen_cmt  (init_static_area, 'Initialized static area');
  cdl_init (cond_area);
  gen_cmt  (cond_area, 'Condition cells');
  cdl_init (uninit_static_area);
  gen_cmt  (uninit_static_area, 'Unitialized static area');
  gen_block (uninit_static_area, size_uninit); (* show size of area *)
  gen_cmt (cst_area, 'Public constant area');

  (* Traverse the symbol table, constructing the initialized static, public
     constant, and condition cell code lists in parallel. *)

  block := root_block;

  while block <> nil do begin
    with block^ do begin
      symbols := id_list.first;
      while symbols <> nil do begin
        with symbols^ do

	  if (kind = vars) andif (dcl_class = static_sc) andif
	     (init_value.kind <> no_value) then begin
	    gen_pc_assert (init_static_area, item_addr); (* "assert" we are where we expect *)
	    if public_dcl then
	      gen_asm_label (init_static_area, name^.text, true, 0);
	    gen_val (init_static_area, init_value, type_desc); (* the initial value *)
	    (* pad varying strings out to full declared length *)
	    if (init_value.kind = string_cst) andif (init_value.valp^.str_varying_ref) then 
	      gen_block (init_static_area, ngm (type_desc^.str_length, 2) -
					      ngm (length (init_value.valp^.str_val), 2))
	  end

	  else if (kind = vars) andif (dcl_class = static_sc) andif
	          (init_value.kind = no_value) and public_dcl then 
	    gen_asm_label (uninit_static_area, name^.text, false, size_init + size_cond + item_addr)

	  else if (kind = consts) andif (init_value.kind <> subr_cst) andif
		  ( public_dcl or
		    ( prog_options.debug_opt and (dcl_class <> external_sc) ) ) then begin
	    if public_dcl then
	      gen_asm_label (cst_area, name^.text, true, 0);
	    reld := gen_cval (init_value, type_desc);
	    init_value.kind := alloc_cst;
	    init_value.defp := val_ptr (reld.reldef)
	  end

	  else if (kind = conditions) andif (dcl_class = static_sc) then begin
	    gen_pc_assert (cond_area, size_init + item_addr); (* "assert" that we are where we expect *)
	    if public_dcl then
	      gen_asm_label (cond_area, name^.text, true, 0);
	    gen_word (cond_area, (#H7fff, absolute_sc), false) 
	  end;

	symbols := symbols^.next
      end (* while symbols <> nil *);
      block := downward_call_thread
    end (* with *);
  end (* while block <> nil *);

  emit_code (init_static_area, static_section, prog_options.semantic_options, size_contribution, false);
  assert (size_contribution = size_init);
  emit_code (cond_area, static_section, prog_options.semantic_options, size_contribution, false);
  assert (size_contribution = size_cond);

  static_size := size_init + size_cond + size_uninit;
  gen_pc_assert (uninit_static_area, static_size); (* double check the final size *)
  emit_code (uninit_static_area, static_section, prog_options.semantic_options, size_contribution, false);
  assert (size_contribution = size_uninit);

  emit_code (cst_area, code_section, prog_options.semantic_options, public_const_size, false)

end (* init_static *);
$PAGE scalar_assignment
(* SCALAR ASSIGNMENT generates code for assignments of scalar values. *)

procedure scalar_assignment (tpl: tuple);

var
  ldesc,
  rdesc: op_desc;
  rh_value: integer;

begin

  (* If the source is smaller then the destination, fetch can bear the burden of
     extending it.  If the destination is the smaller, however, truncation is up 
     to us (except for single-real := double-real, where a float_op will be in the IF).
     There is no sign-constraint on the source--the size constraint is enough.  *)

  ldesc := fetch (tpl^.lhs, memory_alterable_modes, null_mode, any_size, false);
  assert (ldesc.value_size in [size_byte..size_double]);

  if (ldesc.value_size = size_double) and (tpl^.lhs^.desc.kind = reals) then begin
    rdesc := dfetch (tpl^.rhs, ldesc, false);
  end

  else begin
    rdesc := fetch (tpl^.rhs, all_modes, null_mode, at_least [ldesc.value_size], false);
    if aconstp (rdesc, rh_value) andif ( (rh_value >= -128) and (rh_value <= 127) ) then
      rdesc := copy_dreg (rdesc);
    assert (ldesc.value_size <= rdesc.value_size);

    conform_size (rdesc, ldesc.value_size);

    if ldesc.value_size in reg_sizes then
      gen_mm (move_opc, rdesc, ldesc)
    else (* must be double-word subr type *) begin
      assert (ldesc.value_size = size_double);
      ldesc.value_size := size_long; (* have to move one longword at a time *)
      rdesc.value_size := size_long;
      gen_mm (move_opc, rdesc, ldesc); (* first longword *)
      ldesc := increment_addr (ldesc, 4);
      rdesc := increment_addr (rdesc, 4);
      gen_mm (move_opc, rdesc, ldesc) (* and the second longword *)
    end;

    free_desc (ldesc);
    free_desc (rdesc)
  end (* not double real *);
end (* scalar_assignment *);
$PAGE bst_mv_seq
(* BST MV SEQ selects the best code sequence for moving a vector of words. *)

type
  mv_sequences = (no_mv_code, single_move, rt_mv_call, postincr_simple_moves,
		  word_mv_loop, longword_mv_loop, two_longword_mv_loop,
		  movem_by_words, movem_by_longs);

public function bst_mv_seq (    n: integer;
				source_indirect, target_indirect: boolean;
			    var bests_size, bests_cycles: integer;
			    var result_loc_after_and_indirect: boolean): integer;

var
  reg: registers;
  avail_regs, num_indirect: integer;
  if_n_odd: 0..1;

begin
  if n = 0 then begin
    bst_mv_seq := ord (no_mv_code);
    bests_size := 0;
    bests_cycles := 0
  end
  else begin
    num_indirect := ord (source_indirect) + ord (target_indirect);

    (* Take numbers for runtime call as a starting point, then see if anything else is better. *)

    if_n_odd := ord (odd (n)); (* coef for terms applied just if n odd *)
    first_sequence (ord (rt_mv_call),
		    18                      - 2 * num_indirect,
		    336 + round (12.25 * n) - 4 * num_indirect); (* note - wrong if variable len *)

    if n > 0 (* constant length *) then begin
      next_sequence (ord (postincr_simple_moves),
		     8 + 2 * ((n+1) div 2)        - 4 * num_indirect,
		     16 + 11 * n  + 2 * if_n_odd  - 8 * num_indirect);
      next_sequence (ord (single_move),
		     6 * ((n + 1) div 2)    - 2 * num_indirect,
		     15 * n + 6 * if_n_odd  - 4 * num_indirect);
      next_sequence (ord (word_mv_loop),
		     16          - 4 * num_indirect,
		     24 + 23 * n - 8 * num_indirect);
      next_sequence (ord (longword_mv_loop),
		     16 + 2 * if_n_odd            - 4 * num_indirect,
		     24 + 16 * n  - 3 * if_n_odd  - 8 * num_indirect);
      next_sequence (ord (two_longword_mv_loop),
		     18 + (((n mod 4) + 1) div 2) * 2	- 4 * num_indirect,
		     24 + 54 * (n div 4) + 13 * (n mod 4) 
			    - 4 * (n mod 4) div 2   	- 8 * num_indirect);
      avail_regs := 0;
      for reg := minimum (registers) to maximum (registers) do
	if not (reg in reserved_registers) then
	  avail_regs := avail_regs + ord (uses_remaining (reg) = 0);
      if (n div 2) <= avail_regs then
	next_sequence (ord (movem_by_longs),
		       12 + 6 * if_n_odd           - 2 * num_indirect,
		       28 + 9 * n + 12 * if_n_odd  - 4 * num_indirect);
      if n <= avail_regs then
	next_sequence (ord (movem_by_words),
		       12         - 2 * num_indirect,
		       28 + 9 * n - 4 * num_indirect)
    end;
    bst_mv_seq := best_sequence (bests_size, bests_cycles) (* and the answer is ... *)
  end;
  
  (* Signal whether result_loc from blk_move would be pointing after last word
     cleared and would be indirect. *)

  result_loc_after_and_indirect :=
	not (bst_mv_seq in [ord (rt_mv_call), ord (movem_by_words), ord (movem_by_longs)])
		and
	(not (bst_mv_seq in [ord (single_move), ord (no_mv_code)]) or target_indirect)
end (* bst_mv_seq *);
$PAGE blk_move
(* BLK MOVE moves a specified number of words of uninterpreted data from one memory
   location to another.  SOURCE_LOC and LEN_DESC are freed.  A usage is transfered from DEST_LOC
   to RESULT_LOC.  If the user has a further need for DEST_LOC, duplicate_desc should
   be used, and if he has no further need of RESULT_LOC, it should be freed.  RESULT_LOC
   may refer to the same place that DEST_LOC did, or it may point to the word after
   the last word filled.  AFTER_LAST_WORD specifies which.

   Note that:
	1. It is assumed that an even number of bytes can be moved.  If the
	   given length is odd, that length plus one will be moved.
	2. At least initially, this routine is only expected to be used to move
	   non-scalar objects, such as records, arrays, and sets (but not, in general,
	   strings).  Therefore, the value_sizes of the source and destination are
	   irrelevant.  The value_size of RESULT_LOC will be set to that of DEST_LOC. *)

public procedure blk_move (    len_desc: op_desc;
			       len_in_words: boolean;
			       source_loc, dest_loc: op_desc;
			   var result_loc: op_desc;
			   var after_last_word: boolean);

const
  default_size: array [mv_sequences] of op_sizes := (no_size, size_long, no_size, size_long,
					     size_word, size_long, size_long, size_word, size_long);

var
  temp_len, temp_source: op_desc;
  loop_label: def;
  reg: registers;
  loop_count, remainder,
  n, avail_regs, mask,
  bests_size, bests_cycles: integer;
  best_seq: mv_sequences;
  result_loc_after_and_indirect: boolean;

(* Set source and destination value_size's. *)

procedure set_sizes (size: op_sizes);
  begin
    temp_source.value_size := size;
    temp_source.extended_size := size;
    result_loc.value_size := size;
    result_loc.extended_size := size
  end;

begin
  assert ((source_loc.mode in memory_modes) and (dest_loc.mode in memory_alterable_modes));

  if aconstp (len_desc, n) then begin
    assert (n >= 0);
    if not len_in_words then
      n := (n + 1) div 2
  end
  else
    n := -1; (* signifies variable length *)

  (* Select the best code sequence. *)

  best_seq := mv_sequences (bst_mv_seq (n,
					pincr_free (source_loc), pincr_free (dest_loc),
					bests_size, bests_cycles, result_loc_after_and_indirect));

  (* Generate the chosen code sequence. *)

  temp_source := source_loc; (* local copies we can modify *)
  result_loc := dest_loc;
  if not (best_seq in [rt_mv_call, movem_by_words, movem_by_longs]) and pincr_free (result_loc) then
    result_loc := make_postincr (result_loc);
  set_sizes (default_size [best_seq]); (* make an educated guess at the sizes *)

  case best_seq of

    no_mv_code: ;

    single_move: begin
      assert (n in [1, 2]);
      if n = 1 then
        set_sizes (size_word);
      gen_mm (move_opc, temp_source, result_loc)
    end;

    rt_mv_call: begin
      pusha (duplicate_desc (temp_source));
      pusha (duplicate_desc (result_loc));
      if n >= 0 (* i.e. constant *) then
	pushi (n, size_long)
      else
	push (len_desc, size_long);
      if len_in_words or (n >= 0) then
	gen_rt (rt_move_block_wcnt)
      else
	gen_rt (rt_move_block_bcnt)
    end;

    postincr_simple_moves: begin
      temp_source := make_postincr (temp_source);
      result_loc := make_postincr (result_loc);
      while n >= 2 do begin
	gen_mm (move_opc, temp_source, result_loc);
	n := n - 2
      end;
      if n > 0 then begin
	set_sizes (size_word);
	gen_mm (move_opc, temp_source, result_loc)
      end
    end;

    word_mv_loop, longword_mv_loop, two_longword_mv_loop: begin
      temp_source := make_postincr (temp_source);
      result_loc := make_postincr (result_loc);
      if best_seq = word_mv_loop then begin
	loop_count := n;
	remainder := 0
      end
      else if best_seq = longword_mv_loop then begin
	loop_count := n div 2;
	remainder := n mod 2
      end
      else (* two_longword_mv_loop *) begin
	loop_count := n div 4;
	remainder := n mod 4
      end;
      temp_len := loadi (get_dreg, loop_count - 1, size_word, true);
      loop_label := def_create (local_def);
      gen_def (code_area, loop_label);
      gen_mm (move_opc, temp_source, result_loc);
      if best_seq = two_longword_mv_loop then
	gen_mm (move_opc, temp_source, result_loc);
      gen_dbcc (f_cc, temp_len.reg, loop_label);
      free_desc (temp_len);
      if remainder >= 2 then begin
	gen_mm (move_opc, temp_source, result_loc); (* sizes already long *)
	remainder := remainder - 2
      end;
      if remainder > 0 then begin
	set_sizes (size_word);
	gen_mm (move_opc, temp_source, result_loc)
      end
    end;

    movem_by_words, movem_by_longs: begin
      mask := 0;
      avail_regs := 0;
      for reg := minimum (registers) to maximum (registers) do begin
	if not (reg in reserved_registers) andif (uses_remaining (reg) = 0) then begin
	  mask := mask + 2 ** ord(reg);
	  avail_regs := avail_regs + 1
	end;
       exit if ((best_seq = movem_by_longs) and (avail_regs = n div 2)) or (avail_regs = n);
	if reg = maximum (registers) then assert (false)
      end;
      gen_mm (movem_opc, temp_source, int_desc (mask, size_word, false));
      gen_mm (movem_opc, int_desc (mask, size_word, false), result_loc);
      if (best_seq = movem_by_longs) and odd (n) then begin
	temp_source := increment_addr (temp_source, (n-1) * 2);
	result_loc := increment_addr (result_loc, (n-1) * 2);
	set_sizes (size_word);
	gen_mm (move_opc, temp_source, result_loc)
      end
    end

  end;

  free_desc (temp_source);
  if result_loc.mode = postincrement_mode then begin
    after_last_word := true;
    result_loc.mode := indirect_mode (* keep life simple *)
  end
  else
    after_last_word := false;
  result_loc.value_size := dest_loc.value_size; (* for want of a better choice *)
  result_loc.extended_size := dest_loc.extended_size;
  (* Verify accuracy of bst_mv_seq's prediction. *)
  assert (result_loc_after_and_indirect = after_last_word)
end (* blk_move *);
$PAGE agg_assignment
(* AGG ASSIGNMENT generates code to assign one record or array to another. *)

procedure agg_assignment (tpl: tuple);

var
  ldesc,
  rdesc,
  final_loc,
  upb_desc,
  width_desc: op_desc;
  field: sym;
  after_last_word: boolean;

begin
  with tpl^ do begin
    rdesc := fetch (rhs, memory_modes, indirect_mode, any_size, false);
    ldesc := fetch (lhs, memory_alterable_modes, indirect_mode, any_size, false);

    (* Determine number of bytes to be copied, taking advantage of fact that
       both sides must have the same length.  *)

    if not rhs^.desc.base^.flexible then (* rhs has fixed size *)
      width_desc := int_desc ((rhs^.desc.base^.base_size + 7) div 8, no_size, true)
    else if not lhs^.desc.base^.flexible then (* lhs has fixed size *)
      width_desc := int_desc ((lhs^.desc.base^.base_size + 7) div 8, no_size, true)

    else if rhs^.desc.kind = records then begin (* (flex) record assignment *)
      field := lhs^.desc.base^.field_list;
      while field^.next <> nil do
        field := field^.next;
      upb_desc := increment_addr (duplicate_desc (ldesc), field^.fld_offset div bits_per_byte);
      if field^.type_desc^.kind = strings then
	upb_desc.value_size := size_word
      else
	upb_desc.value_size := size_long;
      width_desc := calc_width (lhs^.desc.base, upb_desc)
    end

    else (* rhs^.desc.kind = arrays *) begin (* (flex) array assignment *)
      upb_desc := upper_bound (lhs, ldesc);
      width_desc := calc_width (lhs^.desc.base, upb_desc)
    end;
    
    if rhs^.desc.kind = arrays then begin
      if dynamic_flex (lhs) then
	ldesc := increment_addr (ldesc, 4); (* advance to start of data *)
      if dynamic_flex (rhs) then
	rdesc := increment_addr (rdesc, 4) (* advance to start of data *)
    end;

    blk_move (width_desc, false (* byte_count *), rdesc, ldesc, final_loc, after_last_word);
    free_desc (final_loc)
  end
end (* agg_assignment *);
$PAGE proc_func_assignment
(* PROC FUNC ASSIGNMENT generates assignments to procedure and function
   variables. *)

procedure proc_func_assignment (tpl: tuple);

var
  areg: addr_regs;
  source_desc, dest_desc, result_loc: op_desc;
  after_last_word: boolean;

begin
  with tpl^ do begin
    source_desc := fetch (rhs, memory_modes, null_mode, any_size, false);
    dest_desc := fetch (lhs, memory_alterable_modes, null_mode, double_only, false);
    dest_desc.value_size := size_long; (* we'll set it one longword at a time *)

    (* If the source is a procedure/function variable it can simply be
       moved to the destination.  *)

    if (rhs^.opcode <> cst_ref) and
       ((rhs^.opcode <> ident_ref) orif (rhs^.id_sym^.kind <> consts)) then begin
      blk_move (int_desc (8, no_size, true), false (* len in bytes *),
		source_desc, dest_desc,
		result_loc, after_last_word);
      free_desc (result_loc)
    end
    else begin

      (* If the routine has no parent, we just clear the static link portion
	 of the destination.  *)

      if (source_desc.cst_part.kind = external_sc) orif
	 (rhs^.cst_val.blkp^.subr_sym^.block^.level <= 1) then
	gen_m (clr_opc, dest_desc)
      else begin
	areg := load_frame_pointer (rhs^.cst_val.blkp^.parent);
	gen_rm (move_opc, areg, dest_desc);
	free_reg (areg)
      end;
      dest_desc := increment_addr (dest_desc, 4);
      source_desc := ref_descriptor (source_desc); (* we want the address of it *)
      gen_mm (move_opc, source_desc, dest_desc);
      free_desc (source_desc);
      free_desc (dest_desc)
    end;
  end
end (* proc_func_assignment *);
$PAGE prologue
(* PROLOGUE emits the entry sequence of a procedure or function. *)

procedure prologue (    prog_block: def;
		    var frame_size: def);

var
  target: def;
  procedure_name: nam;
  frame_size_desc: op_desc;
  next_link: call_link;
  plist_size: unit_range;

begin

  (* Entry code for nonquick blocks. *)

  if cur_block^.owner = cur_block then begin
    if cur_block^.kind = subr_blk then
      plist_size := cur_block^.subr_sym^.type_desc^.parmlist_size
    else
      plist_size := 0;
    if cur_block^.level > 2 then
      plist_size := plist_size + 4;	(* space for static link *)
    pushi (plist_size + 16, size_long);

    (* Procedure block. *)

    if prog_options.debug_opt or (trace_opt in cur_block^.semantic_options) then begin
      target := def_create (local_def); (* allocate label *)
      gen_call (target); (* bsr to the label *)
      gen_long (code_area, def_reloc (prog_block), false); (* address of program block *)
      if prog_options.debug_opt then
	gen_word (code_area, abs_zero, false) (* !!! temporary !!! *)
      else
	gen_word (code_area, abs_zero, false); (* 0 => not debug *)
      if cur_block^.kind = subr_blk then
	procedure_name := cur_block^.subr_sym^.name
      else (* program, module, or data block *)
	procedure_name := cur_block^.id;
      gen_len (code_area, length (procedure_name^.text));
      gen_string (code_area, procedure_name^.text);
      gen_def (code_area, target) (* now insert the label *)
    end
    else (* notrace *)
      pushi (0, size_long);
    frame_size := def_lookup (temp_size_def, cur_block^.number); (* unknown until end of body *)
    frame_size_desc := rel_desc (def_reloc (frame_size), size_word, true);
    if chk_stk_opt in cur_block^.semantic_options then begin
      frame_size_desc.cst_part.offset := -4; (* additional 4 bytes in this case *)
      gen_mm (move_opc, frame_size_desc, reg_desc (d0, size_long, true));
      gen_rt (rt_entry)
    end
    else begin
      pushi (0, size_long);
      gen_rm (link_opc, fp, frame_size_desc)
    end;
  end

  (* No entry code required for quick blocks. *)

  else
    frame_size := nil;


  reg_init; (* mark all registers as free *)

  (* temp_start  records where we start allocating temps for this block - takes into account
                 quick-blocking
     temp_base   is advanced as temps are needed within a statement, and is reset
                 to temp_start at the start of each statement
     temp_max    keeps track of the highest value reached by temp_base i.e. the
                 requirements of the "worst" statement.  *)

  temp_start := abs (cur_block^.neg_stk_end);
  next_link := cur_block^.calls;
  while next_link <> nil do begin
    if cur_block^.owner = next_link^.called_subr^.owner then
      temp_start := max (temp_start, abs (next_link^.called_subr^.neg_stk_end));
    next_link := next_link^.rlink
  end;
  assert (not odd (temp_start)); (* we expect word alignment *)
  temp_start := temp_start + (cur_block^.hndlr_depth * 10);

  temp_base := temp_start;
  temp_max := temp_start;

  (* Emit instruction to save SP so that it can be restored after basic blocks
     that have allocated dynamic temps by changing it.  Initially, the instruction
     will be marked as not active.  If its actually required it will be activated.
     The location in which to store SP is left undefined at this point.  If its
     actually required, COMPILE_BODY will allocate the location.  *)

  save_sp_desc := descriptors [displacement_mode];
  save_sp_desc.value_size := size_long;
  save_sp_desc.reg := fp;
  save_sp_desc.cst_part := def_reloc (def_lookup (stackptr_save_def, cur_block^.number));

  gen_rm (move_opc, sp, save_sp_desc);
  save_sp_inst := code_area.last; (* remember where the instruction is *)
  save_sp_inst^.active := false; (* make it "invisible" for now *)
  
  (* Initialize information required for saving of WITH record addresses,
     and for allocation of the save area.  *)

  num_withs_saved := 0; (* count of number of save locs currently in use *)
  max_withs_saved := 0; (* high water mark for num_withs_saved *)
  savewith_desc := descriptors [displacement_mode];
  savewith_desc.value_size := size_long;
  savewith_desc.reg := fp;
  savewith_desc.cst_part := def_reloc (def_lookup (with_save_def, cur_block^.number));

  dfinit;
  grd_init;
end (* prologue *);
$PAGE expand_aggregates
(*  EXPAND AGGREGATES pre-processes the i/f for a routine, replacing
    operations involving aggregate constructors (AGG_VAL tuples) by
    multiple assignments.  An assignment of a constructor to a variable
    may simply be expanded into multiple component assignments; in all
    other cases, a temporary is allocated, the components of the constructor
    are assigned to components of the temporary, and references to the
    constructor are replaced by referecnes to the temporary.  *)

procedure expand_aggregates;
$PAGE initexpr - in expand_aggregates
(*  INIT EXPR is called with an expression tree node and a type record, and
    sets up the type information in the node in accordance with the record.  *)

procedure initexpr ( node : expr; node_type : typ );

var nt : typ;

begin
  nt := node_type; (* If this is an indirect type node, *)
  while (nt <> nil) andif (*   find the actual type. *)
    (nt^.kind = indirect_type) do
      nt := nt^.actual_type;
  assert (nt <> nil);
  with node^ do begin
    result := nil;
    ref_fre := 0;
    context := valx;
    blk_input_tuple := false;
    copy_tuple := false;
    killed_tuple := false;
    desc.base := nt;
    with nt^ do begin
      desc.kind := kind;
      case kind of
        bools, ints, chars, scalars:
          begin
            desc.signed := minval < 0;
            desc.int_prec := base_size;
          end;
	files, pointers:
	  begin
	    desc.signed := false;
	    desc.int_prec := base_size;
	  end;
        reals:
          desc.precision := precision;
        strings:
          begin
            desc.str_kind := str_kind;
            desc.str_length := str_length;
            desc.str_flex := flexible;
          end;
        sets:
          begin
            if nt^.set_element_type = nil then
              desc.base := nil;
            desc.set_cst_lwb := true;
            desc.set_lwb := set_element_type^.minval;
            desc.set_cst_len := true;
            desc.set_length := set_element_type^.maxval - set_element_type^.minval + 1;
          end;
	arrays, records, procs, funcs:
	  (* no special descriptor *);
      end (* case kind *);
    end (* nt <> nil *);
  end (* with node^ *);
end (* initexpr *);
$PAGE cst_expr - in expand_aggregates
(*  CST EXPR returns a CST_REF tuple with a specified scalar value and type.  *)

function cst_expr ( n : integer; t : typ ) : expr;

begin
  new (cst_expr, cst_ref, cst_ref);
  initexpr (cst_expr, t);
  cst_expr^.cst_val := (scalar_cst, n);
  emit (cst_expr);
end (* cst_expr *);
$PAGE field_expr - in expand_aggregates
(*  FIELD EXPR returns a FIELD_REF tuple referring to a specified field in
    a specified record expression.  *)

function field_expr ( fld : sym; rec : expr ) : expr;

begin
  new (field_expr, field_ref, field_ref);
  initexpr (field_expr, fld^.type_desc);
  field_expr^.base_rec := rec;
  field_expr^.field_sym := fld;
  emit (field_expr);
end (* field_expr *);
$PAGE upb_expr - in expand_aggregates
(*  UPB EXPR returns an UPB_OP tuple referring to the upper bound of a
    specified expression which is, presumably, a trailing flex array or
    string field in some record.  *)

function upb_expr ( ref : expr ) : expr;

begin
  new (upb_expr, upb_op, upb_op, 1);
  if ref^.desc.kind = arrays
    then initexpr (upb_expr, ref^.desc.base^.index_type)
    else initexpr (upb_expr, type_non_neg);
  upb_expr^.operand[1] := ref;
  emit (upb_expr);
end (* upb_expr *);
$PAGE agg_temp - in expand_aggregates
(*  AGG TEMP allocates a temporary to hold a specified aggregate constructor.
    It creates an ALC_TEMP tuple whose type descriptor is the same as that
    for the constructor.  In most cases, the size required for the temporary
    is completely determined by the type descriptor, and the operand of the
    ALC_TEMP tuple is nil.  The only exception occurs when the aggregate is
    being assigned to a record with a trailing flex string field.   In this
    case, the DEST argument must be the variable the aggregate is being
    assigned to, and the operand of the ALC_TEMP tuple will be a reference
    to the upper bound of the last field of this record variable.  *)

function agg_temp ( agg, dest : expr ) : expr;

var fld : sym;

begin
  new (agg_temp, alc_temp_op, alc_temp_op, 1);
  initexpr (agg_temp, agg^.desc.base);

  if agg^.desc.base^.flexible then begin
    assert (agg^.desc.kind = records);
    assert (dest <> nil);
    fld := dest^.desc.base^.field_list;
    assert (fld <> nil);
    while fld^.next <> nil do
      fld := fld^.next;
    assert (fld^.fld_variant = dest^.desc.base);
    agg_temp^.operand[1] := upb_expr (field_expr (fld, dest));
  end
  else
    agg_temp^.operand[1] := nil;

  emit (agg_temp);
end (* agg_temp *);
$PAGE assign - in expand_aggregates
(*  ASSIGN generates an assignment tuple to assign RHS_EXPR to LHS_EXPR.  *)

procedure assign ( lhs_expr, rhs_expr : expr );

var tpl : tuple;

begin
  new (tpl, assign_op);
  with tpl^ do begin
    must_store := true;
    lrecursive := false;
    rrecursive := false;
    overlaps := true;
    lhs := lhs_expr;
    rhs := rhs_expr;
  end;
  emit (tpl);
end (* assign *);
$PAGE assign_agg_val - in expand_aggregates
(*  ASSIGN AGG VAL generates code to assign each component of the aggregate
    constructor RHS to the corresponding component of the aggregate reference
    LHS.  DEST is the ultimate destination of the aggregate, while LHS is
    the immediate destination; LHS may be the same as DEST, or it may be an
    intermediate temporary.  *)

procedure assign_agg_val ( lhs, rhs, dest : expr );

var index_type, comp_type : typ;
    i : oper_range;
    component, upb : expr;
    cur_var, var_list : typ;
    cur_tag, fields : sym;
    tag_val : integer;

begin

  (*  For an array constructor, simply generate assignments LHS[i] :=
      RHS[i], for each i from 1 to the number of components.  *)

  if rhs^.desc.kind = arrays then begin
    index_type := rhs^.desc.base^.index_type;
    comp_type := rhs^.desc.base^.element_type;
    for i := 1 to upperbound (rhs^.operand) do begin
      new (component, array_ref, array_ref);
      initexpr (component, comp_type);
      component^.base_array := lhs;
      component^.index_val := cst_expr (index_type^.minval + i - 1, index_type);
      emit (component);
      assign (component, rhs^.operand[i]);
    end;
  end

  (*  For a record constructor, generate assignments LHS.Fi := RHS[i] for
      each selected field Fi.  *)

  else begin
    assert (rhs^.desc.kind = records);
    cur_var := rhs^.desc.base;	(* the record type *)
    if cur_var^.variant_tag = nil then
      cur_tag := nil	(* no variants *)
    else
      cur_tag := cur_var^.variant_tag^.tag_field;	(* the variant tag field *)
    fields := cur_var^.field_list;
    for i := 1 to upperbound (rhs^.operand) do begin

      (*  If the field list is exhausted, then any remaining operands must be
	  selectors for undiscriminated unions, so they can be discarded.  *)

      exit if fields = nil;

      (*  If the field belongs to the current variant, then assign the
	  current rhs component to it.  *)

      if fields^.fld_variant = cur_var then begin
	component := field_expr (fields, lhs);

	(*  An assignment to a trailing flex field of a temporary must also
	    set the hidden upper bound word for the field, so that when the
	    temporary is copied to its ultimate destination, the upper bound
	    word will have the correct value.  *)

	if fields^.type_desc^.flexible and (lhs <> dest) then begin
	  assert (fields^.type_desc^.kind in [arrays, strings]);
	  assert (dest <> nil);
	  assign (upb_expr (component), upb_expr (field_expr (fields, dest)));
	end;
	assign (component, rhs^.operand[i]);
      end;

      (*  If the field is the variant tag, then the current operand must
	  be used to select a discriminated union variant.  If the field
	  doesn't belong to the current variant, then the current operand
	  must be used to select an undiscriminated union variant.  The
	  code is the same in either case.  *)

      if (fields = cur_tag) or (fields^.fld_variant <> cur_var) then begin
	tag_val := rhs^.operand[i]^.cst_val.ival;
	var_list := cur_var^.variant_tag^.first_variant;
	while (var_list <> nil) andif not ( var_list^.others_var orif
	      ( (var_list^.minlab <= tag_val) and (tag_val <= var_list^.maxlab) ) ) do
	  var_list := var_list^.next_variant;
	if var_list = nil then	(* no variant selected - no more fields *)
	  fields := nil
	else begin	(* select the new variant *)
	  fields := var_list^.field_list;
	  if fields <> nil then begin
	    cur_var := fields^.fld_variant;	(* if fields = nil, it doesn't matter *)
	    if cur_var^.variant_tag = nil then
	      cur_tag := nil	(* no sub-variants *)
	    else
	      cur_tag := cur_var^.variant_tag^.tag_field;	(* the sub-variant tag field *)
	  end;
	end;
      end
      else
	fields := fields^.next;
    end (* for i *);
  end;

end (* assign_agg_val *);
$PAGE exp_agg_val - in expand_aggregates
(*  EXP AGG VAL is called with an AGG_VAL tuple, and returns an ALC_TEMP
    tuple for an aggregate of the correct size, to whose components the
    components of the aggregate constructor have been assigned.  DEST, if
    it is not nil, is the reference expression to which the aggregate will
    ultimately be assigned.  *)

function exp_agg_val ( agg, dest : expr ) : expr;

begin
  t_set (agg);
  exp_agg_val := agg_temp (agg, dest);
  assign_agg_val (exp_agg_val, agg, dest);
end (* exp_agg_val *);
$PAGE expand_aggregates - main routine
var t, t1 : tuple;
    i : oper_range;

begin
  t := t_chain^.final_tuple;
  while t <> nil do begin
    with t^ do begin
      t1 := prev;

      (*  An aggregate constructor can occur (1) on the rhs of an assignment,
	  (2) as an actual value parameter, or (3) as the base record whose
	  last field is being referenced for an upper bound check.  In cases
	  (1) and (2), the constructor will be expanded into assignments; in
	  case (3), the last field will simply be selected.  *)

      if (opcode = assign_op) andif (rhs^.opcode = agg_val) then begin
	if chk_overlap (lhs, rhs) then begin
	  rhs := exp_agg_val (rhs, lhs);
	  t1 := prev;
	end
	else begin
	  t_set (t);
	  assign_agg_val (lhs, rhs, lhs);
	  dechain (t);
	  t1 := if_cursor;
	end;
      end

      else if (opcode = call_op) or (opcode = func_call_op) then begin
	for i := 1 to upperbound (arglist) do begin
	  if arglist[i]^.opcode = agg_val then
	    arglist[i] := exp_agg_val (arglist[i], nil);
	end;
	t1 := prev;
      end

      else if (opcode = upb_op) andif
	      (operand[1]^.opcode = field_ref) andif
	      (operand[1]^.base_rec^.opcode = agg_val) then begin
	assert (operand[1]^.field_sym^.next = nil);
	operand[1] := operand[1]^.base_rec^.operand[upperbound(operand[1]^.base_rec^.operand)];
      end;
    end;
    t := t1;
  end (* while t <> nil *);
  reclaim;
end (* expand_aggregates *);
$PAGE case_jump
(* CASE JUMP generates code for a case jump operator and following jump in
   operators.  The parameter TPL points to the case operator on entry; on
   return it is set to the last jump in operator. 

   The generic code sequence we'll generate is (assuming selector expression in Rn):

		SUB	low_cntrl,Rn		; normalize
		BLT	<others case>
		CMP	high_cntrl-low_cntrl,Rn	; don't overshoot table
		BGT	<others case>
		ADD	Rn,Rn			; double - two bytes per table entry
		MOVE.W	TABLE(Rn.W),Rn
		JMP	TABLE(Rn.W)
	TABLE:	offset from TABLE to first case's code
		offset from TABLE to second case's code
		 -
		 -
		 -					*)

procedure case_jump (var tpl: tuple);

var
  selector_op,
  indexed_from_table: op_desc;
  others_def,
  table: def;
  i,
  entry_offset: integer;
  min_size: op_sizes;

begin
  with tpl^ do begin
    
    (* We're going to subract low_cntrl from the selector expression to
       normalize it (so low_cntrl will correspond to offset zero in the
       table).  Then, we'll test the result to see that its within
       0..high_cntrl - low_cntrl.  To know what size to request of fetch,
       we must determine the maximal size among
	  1. the operands and result of the subtraction
	  2. the operands of the range tests		*)

    if ( (cond^.desc.base^.minval - low_cntrl) < minimum (word) )   or
       ( (cond^.desc.base^.maxval - low_cntrl) > maximum (word) )  then
      min_size := size_long (* potential size of difference mandates long *)
    else
      min_size := size_word; (* indexing we'll do later mandates at least word *)

    (* That takes care of the difference.  Fetch will implicitly take care
       of the minuend (the selector_op), so that leaves the subtrahend: *)

    min_size := max (min_size, int_size (low_cntrl, true));

    (* As for the first test, either the subtraction already set the codes, or
       we do a TST, which can accept any size.  The second test is just a CMP
       to the length of the table, which had better not be too big for 16 bits: *)

    assert ((high_cntrl - low_cntrl) <= (maximum (word) div 2));

    selector_op := fetch (cond, [dreg_mode], dreg_mode, [size_long, min_size], true);
    selector_op := copy_dreg (selector_op); (* so we can normalize, then doubld *)

    (* Lets pretend a new basic block starts here.  If the "previous" basic block
       used any dynamic temps, we will reset the stack now.  If we did not play this
       game, the basic block would end with the jumps to the individual (and 'others') cases,
       and the code for every one of those cases would begin by resetting the stack.  *)

    if dyn_in_cur_bb then begin
      reset_stack (true);
      dyn_in_cur_bb := false
    end;

    if next^.opcode <> jump_in_op then begin
      free_desc (selector_op);
      return (* <--- no non-others cases *)
    end;

    others_def := def_lookup (label_def, jump_to^.block_order_no);
    if low_cntrl <> 0 then
      gen_im (sub_opc, low_cntrl, selector_op)
    else
      gen_m (tst_opc, selector_op);
    gen_bcc (lt_cc, others_def);
    gen_im (cmp_opc, high_cntrl - low_cntrl, selector_op);
    gen_bcc (gt_cc, others_def)
  end;

  (* Code to jump to the selected case. *)

  selector_op.value_size := size_word;
  gen_mm (add_opc, selector_op, selector_op); (* table entries are 2 bytes each *)
  table := def_create (local_def);

  indexed_from_table := descriptors [pc_index_w_mode];
  indexed_from_table.value_size := size_word;
  indexed_from_table.index_reg := selector_op.reg;
  indexed_from_table.cst_part := def_reloc (table);

  gen_mm (move_opc, indexed_from_table, selector_op); (*	MOVE.W	TABLE(Xi.w),Xi *)
  gen_m (jmp_opc, indexed_from_table); (*	JMP	TABLE(Xi.w) *)
  free_desc (selector_op);

  (* Generate the case table.  For each iteration of the following loop, we
     generate the entries for a label range of the form "n..m", and entries
     for the gap from "m" to the start of the next explicitly given range.  *)

  gen_def (code_area, table); (* TABLE:	*)
  tpl := tpl^.next;
  entry_offset := 0;

  loop (* over jump_in_op's - assume at least one *)
    for i := tpl^.low_cntrl to tpl^.high_cntrl do begin
      gen_word (code_area,
		(entry_offset, def_sc, def_lookup (label_def, tpl^.jump_to^.block_order_no)),
		true);
      entry_offset := entry_offset + 2
    end;
  exit if tpl^.next^.opcode <> jump_in_op;
    tpl := tpl^.next;
    for i := tpl^.prev^.high_cntrl + 1 to tpl^.low_cntrl - 1 do begin
      gen_word (code_area,
		(entry_offset, def_sc, others_def),
		true); (* goto 'others' for unspecified cases *)
      entry_offset := entry_offset + 2
    end
  end
end (* case_jump *);
$PAGE cond_handler_stmts
(* COND HANDLER STMTS generates code for the statement tuples associated
   with exceptional condition handling. *)

procedure cond_handler_stmts (var tpl: tuple);

var
  clause_addr, table_op: op_desc;
  table: def;
  jump: tuple;
  entry_offset: integer;

begin
  with tpl^ do begin
    case opcode of

      set_handler_op,
      rst_handler_op:
	begin
	  if hndlr_tuple <> nil then begin
	    table_op := descriptors [pc_displacement_mode];
	    table_op.cst_part := def_reloc (def_lookup (hnd_tab_def, hndlr_tuple^.block_order_no));
	    pusha (table_op)
	  end
	  else
	    pushi (0, size_long);
	  if opcode = set_handler_op then
	    gen_rt (rt_set_handler)
	  else
	    gen_rt (rt_restore_handler);
	end;

      signal_op:
	begin
	  pusha (fetch (cond_parm, control_modes, null_mode, any_size, false));
	  gen_rt (rt_signal);
	end;

      resignal_op:
	gen_rt (rt_resignal);

      mask_op:
	begin
	  pusha (fetch (cond_parm, control_modes, null_mode, any_size, false));
	  gen_rt (rt_mask);
	end;

      unmask_op:
	begin
	  pusha (fetch (cond_parm, control_modes, null_mode, any_size, false));
	  gen_rt (rt_unmask);
	end;

      hndlr_jump_op:
	begin

	  (*  Generate the handler common entry code.  *)

	  reset_stack (true);
	  table := def_lookup (hnd_tab_def, jump_from^.block_order_no);
	  clause_addr := descriptors [pc_index_w_mode];
	  clause_addr.index_reg := a0;
	  clause_addr.cst_part := def_reloc (table);
	  gen_m (jmp_opc, clause_addr);

	  (*  Generate the handler branch table.  *)

	  gen_def (code_area, table);

	  (*  First, the common entry code address.  *)

	  gen_long (code_area,
		    def_reloc (def_lookup (label_def, jump_from^.block_order_no)), false);

	  (*  Next, the "next outer handler" branch table address.  *)

	  if jump_from^.in_handler = nil then
	    gen_long (code_area, abs_zero, false)
	  else
	    gen_long (code_area, def_reloc (def_lookup (hnd_tab_def,
			  jump_from^.in_handler^.block_order_no)), false);

	  (*  Next, the handler state block location in the stack frame.  *)

	  gen_word (code_area,
		    (- temp_base + (high_cntrl - 1) * 10, absolute_sc),
		    false);

	  (*  Now generate the condition address/handler address pairs.  *)

	  jump := tpl^.next;
	  entry_offset := 12;
	  while jump^.opcode = jump_cond_op do begin
	    gen_word (code_area, sym_reloc (jump^.cond^.id_sym), false);

	    (* This constitutes a use of JUMP^.COND, so ... *)

	    dec_expr_usage (jump^.cond);
	    gen_word (code_area,
		      (entry_offset, def_sc, def_lookup (label_def, jump^.jump_to^.block_order_no)),
		      true);
	    entry_offset := entry_offset + 4;
	    jump := jump^.next;
	  end;

	  (*  Finally, generate the OTHERS or ALLCONDITIONS entry at the
	      end of the table.  *)

	  if low_cntrl = 0 then
	    gen_word (code_area, (-1, absolute_sc), false)
	  else
	    gen_word (code_area, (-2, absolute_sc), false);
	  if jump_to^.next^.opcode = resignal_op then
	    gen_word (code_area, abs_zero, false)
	  else
	    gen_word (code_area,
		      (entry_offset, def_sc, def_lookup (label_def, jump_to^.block_order_no)),
		      true);

	  tpl := jump^.prev;
	end;

    end (* case opcode *);
  end (* with tpl^ *);
end (* cond_handler_stmts *);
$PAGE value_check
(* VALUE_CHECK generates code for value and subscript range check tuples. *)

procedure value_check (value_chk_op: tuple; rts: rt_symbol);

var
  value, limit : op_desc;
  err_label : def;
  label_err_call : boolean;

  procedure simple_check ( cc : condition_codes );
  var
    ok_label : def;
  begin
    if cc = t_cc then
      (* check ok *)
    else begin
      if cc <> f_cc then begin
	ok_label := def_create (local_def);
	gen_bcc (cc, ok_label);
      end;
      if label_err_call then begin
	err_label := def_create (local_def);
	gen_def (code_area, err_label);
      end;
      gen_rt (rts);
      if cc <> f_cc then
	gen_def (code_area, ok_label);
    end;
  end;

  procedure second_check ( cc : condition_codes );
  begin
    if cc = f_cc then
      (* check ok *)
    else
      gen_bcc (cc, err_label);
  end;

begin
  with value_chk_op^ do begin
    value := fetch (operand[1], all_modes, null_mode, any_size, false);
    err_label := nil;
    if operand[2] <> nil then begin
      limit := fetch (operand[2], all_modes, null_mode, any_size, false);
      label_err_call := (operand[3] <> nil);
      compare_integer_ops (value, limit, gec, simple_check);
      free_desc (limit);
    end;
    if operand[3] <> nil then begin
      limit := fetch (operand[3], all_modes, null_mode, any_size, false);
      label_err_call := false;
      if err_label = nil then
	compare_integer_ops (value, limit, lec, simple_check)
      else
	compare_integer_ops (value, limit, gtc, second_check);
      free_desc (limit);
    end;
    free_desc (value);
  end;
end (* value_check *);
$PAGE pointer_check
(* POINTER_CHECK generates code for a pointer or file check tuple. *)

procedure pointer_check (ptr_check_tpl: tuple; rts: rt_symbol; bad_value: integer);

var
  pointer: op_desc;
  ok_label: def;

begin

  (* Even though NIL or NILF is passed in, we're exploiting our knowledge
     that they're both zero.  If they weren't, we would properly check the
     pointer against zero in addition to the check for NIL or NILF.  But,
     rather than put in a bunch of extra code (knowing its just for show),
     we'll settle for:  *)

  assert (bad_value = 0);

  pointer := fetch (ptr_check_tpl^.operand[1], data_modes, null_mode, any_size, false);

  gen_im (cmp_opc, bad_value, pointer);
  ok_label := def_create (local_def);
  gen_bcc (ne_cc, ok_label); (* skip around runtime call *)
  gen_rt (rts); (* call to signal the check's failure *)
  gen_def (code_area, ok_label);

  free_desc (pointer)

end (* pointer_check *);
$PAGE substring_check
(* SUBSTRING_CHECK generates code for substring check tuples.  Three
   tests are generated:

   	1. length (substring) >= 0,
	2. start_index > 0,
	3. start_index + length (substring) - 1 <= length (base_string)
	 or, as we'll actually do it:
	   length (substring) <= length (base_string) - start_index + 1

   The substring check tuple has three operands: the starting index of the
   substring, the length of the substring and the length of the base
   string.  Note that the check tuple should not have been generated if it is
   known at compile time to be useless, but it WILL be generated if its known
   at compile time that the check will fail.  *)

procedure substring_check (str_chk_tpl: tuple);

var
  index_const,
  substr_len_const,
  base_len_const:     boolean;
  index_value,
  substr_len_value,
  base_len_value:     char_range;
  index_op,
  substr_len_op,
  base_len_op,
  permissible_len_op: op_desc;
  size:		      op_sizes;
  out_label,
  error_call_label:   def;

begin

  (* Fetch with loosest possible constraints. *)

  index_op :=      fetch (str_chk_tpl^.operand[1], data_modes, dreg_mode, any_size, true);
  substr_len_op := fetch (str_chk_tpl^.operand[2], data_modes, dreg_mode, any_size, true);
  base_len_op :=   fetch (str_chk_tpl^.operand[3], data_modes, dreg_mode, any_size, true);

  (* Find out whats known at compile time.  *)

  index_const :=  aconstp (index_op, index_value);
  substr_len_const := aconstp (substr_len_op, substr_len_value);
  base_len_const :=   aconstp (base_len_op, base_len_value);

  error_call_label := def_create (local_def);

  (* Emit the test for length (substring) >= 0.  *)

  if (not substr_len_const) orif (substr_len_value < 0) then begin
    gen_m (tst_opc, substr_len_op);
    gen_bcc (lt_cc, error_call_label)
  end;

  (* Emit the test for start_index > 0.  *)

  if (not index_const) orif (index_value <= 0) then begin
    gen_m (tst_opc, index_op);
    gen_bcc (le_cc, error_call_label)
  end;

  (* Calculate (length (base_string) - start_index + 1) for use in the final check.  *)

  size := max (index_op.value_size, substr_len_op.value_size, base_len_op.value_size);

  index_op :=      coerce (index_op,      data_modes, dreg_mode, [size], true);
  substr_len_op := coerce (substr_len_op, data_modes, dreg_mode, [size], true);
  base_len_op :=   coerce (base_len_op,   data_modes, dreg_mode, [size], true);

  if base_len_const and index_const then
    permissible_len_op := int_desc (base_len_value - index_value + 1, size, true)
  else if base_len_const and (base_len_value < sgn_max [size]) then begin
    permissible_len_op := loadi (get_dreg, base_len_value + 1, size, true);
    gen_mm (sub_opc, index_op, permissible_len_op);
    free_desc (index_op)
  end
  else if index_const then begin
    permissible_len_op := copy_dreg (base_len_op);
    gen_im (sub_opc, index_value - 1, permissible_len_op)
  end
  else begin
    (* If length (base_string) is in a register we can clobber, we'll do the
       work there.  Otherwise, we load it into a fresh register and use that. *)
    permissible_len_op := copy_dreg (base_len_op);
    gen_mm (sub_opc, index_op, permissible_len_op);
    free_desc (index_op);
    gen_im (add_opc, 1, permissible_len_op)
  end;

  (* Generate code to compare the substring's ending position calculated above
     to the length of the base string.  *)

  out_label := def_create (local_def);
  (* If only one operand is immediate, it must be the source operand. *)
  if (permissible_len_op.mode = immediate_mode) and not substr_len_const then begin
    (* set cc's by substr_len - permissible_len *)
    gen_mm (cmp_opc, permissible_len_op, substr_len_op);
    gen_bcc (le_cc, out_label) (* skip over rt call if ok *)
  end
  else begin
    (* set cc's by permissible_len - substr_len *)
    gen_mm (cmp_opc, substr_len_op, permissible_len_op);
    gen_bcc (ge_cc, out_label) (* skip over rt call if ok *)
  end;
  gen_def (code_area, error_call_label);
  gen_rt (rt_str_chk); (* call to signal the check's failure *)
  gen_def (code_area, out_label);

  free_desc (substr_len_op);
  free_desc (permissible_len_op);

end  (* substring_check *);
$PAGE compatibility_check
(* COMPATIBILITY CHECK generates code for compatibility check tuples.
   Compatability checks are generated for array assignments when one of
   the arrays is flexible, or when a flex array or string is passed as
   a VAR parameter and the type of the formal is not flex.  *)

procedure compatibility_check (compat_chk_tpl: tuple);

  procedure check_equality ( cc : condition_codes );
  var
    skip_label : def;
  begin
    if cc = t_cc then
      (* check ok *)
    else begin
      if cc <> f_cc then begin
	skip_label := def_create (local_def);
	gen_bcc (cc, skip_label);
      end;
      gen_rt (rt_cmp_chk);
      if cc <> f_cc then
	gen_def (code_area, skip_label);
    end;
  end;

var
  bound1,
  bound2: op_desc;

begin

  (* The logic is very simple.  Fetch the operands; compare them for
     equality; if not equal, signal a compatibility error. *)

  bound1 := fetch (compat_chk_tpl^.operand[1], data_modes, dreg_mode, any_size, true);
  bound2 := fetch (compat_chk_tpl^.operand[2], data_modes, dreg_mode, any_size, true);

  compare_integer_ops (bound1, bound2, eqc, check_equality);

  free_desc (bound1);
  free_desc (bound2)
end (* compatibility_check *);
$PAGE perform_check
(* PERFORM CHECK executes a check tuple. *)

procedure perform_check (check_tuple: tuple);

begin
  case check_tuple^.opcode of

    val_range_chk:
      value_check (check_tuple, rt_val_chk);

    file_chk:
      pointer_check (check_tuple, rt_fil_chk, int_nilf);

    ptr_chk:
      pointer_check (check_tuple, rt_ptr_chk, int_nil);

    sub_range_chk:
      value_check (check_tuple, rt_sub_chk);

    str_range_chk:
      substring_check (check_tuple);

    compat_chk:
      compatibility_check (check_tuple);

  end;
end (* perform_check *);
$PAGE do_check
(* DO CHECK evaluates a check tuple which had previously been deferred.
   The check tuple is cleverly hidden as the result field of the operand. *)

public procedure do_check (exp: expr);

var
  tpl: tuple;

begin
  tpl := exp^.result;
  exp^.result := nil; (* clear the deferred check information *)
  exp^.ref_fre := 0;
  perform_check (tpl);
end (* do_check *);
$PAGE attach_check_op
(* ATTACH CHECK OP attaches a check tuple to one of its operands, to be
   evaluated when the operand is fetched.  If the operand has already been
   fetched, then tyhe check is performed now. *)

procedure attach_check_op (check_tpl: tuple; operand: expr);

begin
  if operand^.result <> nil then (* already evaluated, do the check *)
    perform_check (check_tpl)
  else begin
    operand^.result := check_tpl;
    operand^.ref_fre := 1; (* mark pending check *)
    assert (operand^.usage_count > 1); (* useless otherwise *)
  end;
end (* attach_check_op *);
$PAGE compile_body
(* COMPILE BODY compiles the body of the current block.  *)

procedure compile_body (    prog_block: def;
			var code_contribution: unit_range;
			var const_contribution: unit_range)  options special(coercions);

var
  tpl, (* scanning cursor for compilation *)
  temp_cursor, (* for disposing tuples *)
  label_chain, label_end, (* for preserving label_nodes until end of block *)
  lab,
  first_jump: tuple;
  d,
  d1,
  frame_size: def; (* frame size definition if nonquick, nil otherwise *)
  op: op_desc;
  op_ptr: saved_op_ptr;
  dreg: data_regs;
  nonlocal_fp : addr_regs;
  dump_final: boolean;

begin
  rd_tuples; (* fetch the intermediate form *)

  (* If original IF dump requested, do it. *)

  if switch (cur_block^.dump_switches, 'IFM0') then
    dmptuples ('ORIGINAL INTERMEDIATE FORM FOR BLOCK $');

  make_basic_blocks; (* explode booleans and supply labels *)
  constrain_precisions;	(* compute machine integer and real precisions *)
  expand_aggregates;	(* expand AGG_VAL's into assignments *)

  (* Dump IF after make_basic_blocks, if requested. *)

  if switch (cur_block^.dump_switches, 'IFM') then
    dmptuples ('INTERMEDIATE FORM AFTER MAKE_BASIC_BLOCKS FOR BLOCK $');

  (* Note whether we will be dumping the final intermediate form.  If so, we'll
     have to suppress the business of deleting I/F after every statement, while
     preserving the labels on another chain.  *)

  dump_final := switch (cur_block^.dump_switches, 'FINAL'); (* going to be dumping final form? *)

  prologue (prog_block, frame_size); (* entry sequence *)

  tpl := t_chain;
  label_chain := nil; (* we'll hold onto labels until end of block *)
  while tpl <> nil do begin
    with tpl^ do begin
      case opcode of

	start_block,
	end_block:  ; (* no action required *)

	assign_op:
	  begin
	    overlaps := chk_overlap (lhs, rhs);
	    chk_recursion (tpl);
	    case lhs^.desc.kind of
	      bools, ints, chars, scalars, pointers, files, reals:
	        scalar_assignment (tpl);
	      strings:
		str_assignment (tpl);
	      sets:
		set_assignment (tpl);
	      procs, funcs:
		proc_func_assignment (tpl);
	      arrays, records:
		agg_assignment (tpl)
	    end
	  end;

	start_with:
	  with_start (with_rec, num_withs_saved, max_withs_saved, savewith_desc);

	end_with:
	  with_end (with_rec, num_withs_saved);

	call_op:
	  procedure_call (tpl);

	label_node:
	  begin
	    d := def_lookup (label_def, block_order_no);
	    gen_def (code_area, d); (* define location of label *)
	    if (label_sym <> nil) andif (label_sym^.lab_nonlocal_use) then begin
	      d := def_lookup (sym_def, label_sym^.id_number);
	      gen_def (code_area, d) (* user-defined label symbol with nonlocal use *)
	    end;
	    bb_start (tpl); (* start of new basic block *)
	    (* Move this tuple from normal chain to special label chain. *)
	    if not dump_final then begin
	      if prev = nil then
		t_chain := next
	      else
		prev^.next := next;
	      if next <> nil then
		next^.prev := prev;
	      if label_chain = nil then
		label_chain := tpl
	      else
		label_end^.next := tpl;
	      label_end := tpl
	    end
	  end;

	jump_op:
	  begin
	    bb_end (tpl); (* end of basic block *)
	    if jump_to <> get_next_action (tpl) then (* generate only if useful *)
	      gen_bcc (t_cc, def_lookup (label_def, jump_to^.block_order_no))
	  end;

	jump_t_op, jump_f_op:
	  begin
	    first_jump := tpl;
	    tpl := tpl^.next; (* skip to alternate jump op *)
	    lab := get_next_action (tpl); (* this may be label node *)
	    if opcode = jump_t_op
	      then test_and_jump (cond, jump_to, tpl^.jump_to, lab)
	      else test_and_jump (cond, tpl^.jump_to, jump_to, lab);
	    bb_end (first_jump);
	    bb_end (tpl)
	  end;

	case_jump_op:
	  case_jump (tpl);

	goto_op:
	  begin
	    d := def_lookup (sym_def, target_lab^.id_number); (* get label definition *)
	    if target_lab^.block^.kind = program_blk then begin
	      gen_mr (move_opc, rel_desc (def_reloc (d), size_long, true), a0);
	      gen_rt (rt_uw_prg)
	    end
	    else begin
	      nonlocal_fp := load_frame_pointer (target_lab^.block);
	      if nonlocal_fp <> fp then
		gen_rr (move_opc, nonlocal_fp, fp, size_long);
	      free_reg (nonlocal_fp);
	      gen_jump (d)
	    end
	  end;

	gen_jump_op:
	  begin
	    bb_end (tpl);
	    tpl := tpl^.next; (* presumably the jump_to label *)
	    lab := tpl; (* remember the label node *)
	    tpl := tpl^.next; (* this should be the gen_xxxif operator *)
	    dreg := get_dreg; (* get a register in which to load the value *)
	    case tpl^.opcode of
	      gen_andif_op: op := loadi (dreg, 1, size_byte, true);
	      gen_orif_op:  op := loadi (dreg, 0, size_byte, true)
	    end;
	    d1 := def_create (local_def);
	    gen_bcc (t_cc, d1);
	    d := def_lookup (label_def, lab^.block_order_no);
	    gen_def (code_area, d); (* define short-circuit label *)
	    bb_start (lab);
	    case tpl^.opcode of
	      gen_andif_op: op := loadi (dreg, 0, size_byte, true);
	      gen_orif_op:  op := loadi (dreg, 1, size_byte, true)
	    end;
	    gen_def (code_area, d1);
	    new (op_ptr);
	    op_ptr^ := (op, false);
	    tpl^.result := expr (op_ptr);
	  end;

	dispose_op:
	  begin
	    op := fetch (dptrarg, nonstack_modes, null_mode, long_only, false);
	    push (op, size_long);
	    gen_rt (rt_dispose)
	  end;

	start_stmt:
	  begin
	    kill_temps; (* reset stack if required *)
	    dfterm; (* also, reset the DF stack *)
	    grd_term; (* also, reset the guard stack *)
	    cur_source := stmt_source; (* for debugging *)
	    gen_source (code_area, stmt_source, stmt_index); (* comment for assembly listing *)
	    if not dump_final then
	      repeat
		temp_cursor := tpl^.prev;
		tpl^.next^.prev := tpl^.prev;
		tpl^.prev^.next := tpl^.next;
		dispose (tpl);
		tpl := temp_cursor
	      until (tpl^.opcode = start_block) orif
		    ( (tpl^.opcode in [first_expr..last_expr, first_chk_op..last_chk_op]) andif
		      (tpl^.usage_count <> 0) )
	  end;

	stop_op:
	  gen_rt (rt_stop);

	return_op:
	  begin
	    if cur_block <> cur_block^.owner then begin (* quick? *)
	      if dyn_in_cur_bb then
		reset_stack (true);
	      gen_opcode (rts_opc)
	    end
	    else
	      gen_rt (rt_return)
	  end;

	abort_op:
	  gen_rt (rt_ass_chk);

	case_abort_op:
	  gen_rt (rt_case_chk);

	set_handler_op,
	rst_handler_op,
	signal_op,
	mask_op,
	unmask_op,
	resignal_op,
	hndlr_jump_op,
	jump_cond_op: 
	  cond_handler_stmts (tpl);

	start_io_op:
	  io_begins (tpl);
	end_io_op:
	  io_ends (tpl);

	get_op:
	  if file_arg^.desc.base^.file_kind = textfile
	    then rt_io_call (file_arg, rt_get_text)
	    else rt_io_call (file_arg, rt_get_typed);
	put_op:
	  if file_arg^.desc.base^.file_kind = textfile
	    then rt_io_call (file_arg, rt_put_text)
	    else rt_io_call (file_arg, rt_put_typed);
	readln_op:
	  rt_io_call (file_arg, rt_readln);
	writeln_op:
	  rt_io_call (file_arg, rt_writeln);
	page_op:
	  rt_io_call (file_arg, rt_page);
	clear_op:
	  rt_io_call (file_arg, rt_clear);
	break_op:
	  case file_arg^.desc.base^.file_kind of
	    textfile : rt_io_call (file_arg, rt_break_text);
	    typedfile : rt_io_call (file_arg, rt_break_typed);
	    binaryfile : rt_io_call (file_arg, rt_break_binary);
	  end;
	empty_op:
	  case file_arg^.desc.base^.file_kind of
	    textfile : rt_io_call (file_arg, rt_empty_text);
	    typedfile : rt_io_call (file_arg, rt_empty_typed);
	    binaryfile : rt_io_call (file_arg, rt_empty_binary);
	  end;
	close_op:
	  rt_io_call (file_arg, rt_close);
	scratch_op:
	  rt_io_call (file_arg, rt_scratch);

	read_op,
	write_op:
	  read_write_call (tpl);

	seek_op:
	  rt_seek_call (seek_file, seek_index);

	close_all_op:
	  gen_rt (rt_close_all);

	val_range_chk,
	file_chk,
	ptr_chk,
	sub_range_chk:
	  attach_check_op (tpl, operand[1]);

	str_range_chk:
	  attach_check_op (tpl, operand[2]);

	compat_chk:
	  begin
	    lab := operand[1]^.operand[1];	(* back over dim_op, lwb_op, or upb_op *)
	    if not (lab^.usage_count > 1) then begin
	      assert (lab^.opcode = field_ref);	(* must be for flex record *)
	      lab := lab^.base_rec;
	    end;
	    attach_check_op (tpl, lab);
	  end;

	first_expr..last_expr: 
	  (*  will be fetched later *)

      end (* case *);
    end (* with *);
    tpl := tpl^.next; (* ready for next time around while loop *)

  end (* while *);

  (* If any reset of SP was required (because of dynamic temp allocation, condition
     handler, or nonlocal goto) supply temp location for saving its contents. *)

  if save_sp_inst^.active then begin
    temp_max := temp_max + 4;
    def_assign (save_sp_desc.cst_part.reldef, (-temp_max, absolute_sc))
  end;

  (* Allocate WITH record address save area. *)

  temp_max := temp_max + 4 * max_withs_saved;
  assert (num_withs_saved = 0); (* ensure it came out right *)
  def_assign (savewith_desc.cst_part.reldef, (-temp_max, absolute_sc));

  (* Finish up temporary allocation and resolve stack frame size. *)

  cur_block^.neg_stk_end := -temp_max;
  if frame_size <> nil then
    def_assign (frame_size, (-temp_max, absolute_sc));

  (* Verify that all registers were freed. *)

  reg_term;

  (* Emit the code lists. *)

  emit_code (cst_area, code_section, cur_block^.semantic_options, const_contribution, false);
  emit_code (code_area, code_section, cur_block^.semantic_options, code_contribution, true);

  (* Dump the final IF, if requested *)

  if dump_final then begin
    reclaim;
    dmptuples ('FINAL INTERMEDIATE FORM FOR BLOCK $')
  end;

  def_purge; (* reinit classes not used across blocks *)
  if not dump_final then
    assert ((t_chain^.opcode = start_block) and
	    (t_chain^.next^.opcode in [return_op, stop_op, goto_op]) and
	    (t_chain^.next^.next^.opcode = end_block));
  del_tuples; (* get rid of IF for this block *)
  if not dump_final then begin
    t_chain := label_chain;
    label_end^.next := nil;
    del_tuples
  end
end (* compile_body *);
$PAGE compile_subr
(* COMPILE SUBR generates code for the body of a procedure or function.  It is
   assumed that cur_block points to the block node for the subroutine on entry. *)

procedure compile_subr (    prog_block: def;
			var code_contribution, const_contribution: unit_range);

var
  entryaddr: def;

begin
  gen_cmt (code_area, 'Begin subroutine ' || cur_block^.subr_sym^.name^.text);
  gen_cmt (cst_area, 'Constant area for subroutine ' || cur_block^.subr_sym^.name^.text);
  with cur_block^.subr_sym^ do
    if public_dcl then
      gen_asm_label (code_area, name^.text, true, 0);
  entryaddr := def_lookup (subr_def, cur_block^.number);
  gen_def (code_area, entryaddr);

  compile_body (prog_block,
		code_contribution, const_contribution) (* compile the body of the subroutine *)
end (* compile_subr *);
$PAGE compile_main
(* COMPILE MAIN generates code for the body of a main program.  It is assumed
   that cur_block points to the program's block node at entry.  *)

procedure compile_main (var startaddr: def;
			    prog_block: def;
			var code_contribution, const_contribution: unit_range);

begin
  gen_cmt (code_area, 'Begin program ' || cur_block^.id^.text);
  gen_cmt (cst_area, 'Constant area for program ' || cur_block^.id^.text);
  gen_asm_label (code_area, 'M.START', true, 0);
  startaddr := def_lookup (subr_def, cur_block^.number);
  gen_def (code_area, startaddr);

  compile_body (prog_block,
		code_contribution, const_contribution) (* generate code for body of block *)
end (* compile_main *);
$PAGE gen_code
(* GEN CODE is the driving program for the code generator.  It is called
   by PASS4 to compile an entire module.  It directs initialization,
   generation, and termination.  *)

public procedure gen_code (var code_size, const_size, static_size: unit_range);

var
  startaddr: def; (* start address of main program, nil otherwise *)
  code_contribution,
  const_contribution: unit_range;
  prog_block: def; (* location of program block if debug or trace *)
  obj_file: file_name;

begin
  cur_block := root_block^.children; (* set immediately in case of assertion failure *)
  cur_source := (0, 0, 0);           (*  "       "      "   "   "      "       "     *)

  (* Initialization. *)

  startaddr := nil; (* start address of mainline *)
  def_init; (* initialize def_record data structures *)
  cdl_init (code_area);
  cdl_init (cst_area);
  emt_init; (* all initialization for code list emission *)

  compute_quick_dominators;
  compute_parm_area_sizes;
  relocate_stack_frames;

  (* Program block. *)

  if prog_options.debug_opt or (trace_opt in all_opts) then begin
    prog_block := def_create (code_def);
    gen_def (code_area, prog_block);
    gen_dtime (code_area, cur_block^.comp_dtime);
    if prog_options.debug_opt then
      gen_word (code_area, (0, code_sc, static_section), false)
    else
      gen_word (code_area, abs_zero, false);
    obj_file := substr (rel_file, search (rel_file, [':']) + 1); (* discard device name *)
    obj_file := substr (obj_file, 1, search (obj_file, ['.'], length (obj_file) + 1) - 1); (* keep name *)
    gen_string (code_area, substr (obj_file || '      ', 1, 6));
    emit_code (code_area, code_section, prog_options.semantic_options, code_size, false)
  end
  else
    code_size := 0; (* initialize count of total code bytes *)
  init_static (static_size, const_size); (* generate init. statics and public consts *)

  (* Compile the constituent blocks in reverse depth first order. *)

  cur_block := root_block; (* look for end of call chain *)
  while cur_block^.downward_call_thread <> nil do
    cur_block := cur_block^.downward_call_thread;

  while cur_block <> root_block do begin (* compile in reverse order *)
    if cur_block^.kind in [program_blk, subr_blk] then begin
      if cur_block^.kind = program_blk then
	compile_main (startaddr, prog_block, code_contribution, const_contribution)
      else if cur_block^.kind = subr_blk then
	compile_subr (prog_block, code_contribution, const_contribution);
      code_size := code_size + code_contribution;
      const_size := const_size + const_contribution
    end;
    cur_block := cur_block^.upward_call_thread;
  end;
  cur_block := root_block^.children; (* in case of later assertion failure *)

  (* Termination. *)

  emt_term (startaddr, code_size, const_size, static_size); (* finish up code list emission *)
  def_term; (* cleanup def_record data structures *)
  dmp_close (* close the dump file, if necessary *)
end (* gen_code *).
 Dc*.