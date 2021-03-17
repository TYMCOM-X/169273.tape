$TITLE P10GEN - code generator
$LENGTH 42

module p10gen;
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE ptmcon.inc
$INCLUDE pasfil.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$INCLUDE p10cgu.inc
$INCLUDE p10mac.inc
$INCLUDE pasmap.inc
$INCLUDE p10opc.inc
$INCLUDE pasifu.inc
$INCLUDE p10exp.inc
$INCLUDE p10cmp.inc
$INCLUDE p10cll.inc
$INCLUDE p10dsc.inc
$INCLUDE p10rel.inc
$INCLUDE p10csp.inc
$INCLUDE p10deb.inc
$INCLUDE pasmth.inc
$PAGE global variables

public var
  cur_bb: tuple;                (* from compile_body, label_node heading the current basic 
                                   block being processed. *)

  low_base: def; (* relocatable 0 *)

  high_base: def; (* relocatable 400000b *)

  prg_blk_def: def;     (* location of program block in debug *)

const

  prg_blk_size := 6;    (* 5 words data + module word *)

static var

  hsb_addr: addr_desc; (* descriptor for statically allocated temporary
			  area in block's stack frame where handler state
			  blocks will be located *)
  overlaid: boolean; (* true if compilation under /OVERLAY *)
$PAGE emit_code
(* EMIT CODE takes a code list.  It will write it to the rel file, and to
   the macro listing if assembly_opt is in the options_list parameter. *)

public procedure emit_code ( var area_list: code_list; (* the code area to be written *)
                             var ic: unit_range; (* the address to write the code at *)
                             options_list: set_of_options ); (* to control listing *)

 begin
  if assembly_opt in options_list then
    mac_list (area_list, ic);
  wr_code (area_list, ic, true);
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
   cr: code;

 begin
  gen_origin (static_area, loc_static);

  gen_asm_label (static_area, 'STATC.');
  block := root_block;
  while block <> nil do begin
    with block^ do begin
      symbols := id_list.first;
      while symbols <> nil do begin
        with symbols^ do begin
          if (kind = vars) andif (dcl_class = static_sc) andif
             (init_value.kind <> no_value) then begin
            gen_origin (static_area, item_addr);
            gen_val (static_area, init_value);
          end
          else if (kind = consts) andif (init_value.kind <> subr_cst) andif
                  ( public_dcl or
                    ( prog_options.debug_opt and
                      (dcl_class <> external_sc) ) ) then begin
            reld := gen_cval (init_value);
            init_value.kind := alloc_cst;
            init_value.defp := val_ptr (reld.reldef); (* coerce *)
          end
	  else if (kind = conditions) andif (dcl_class = static_sc) then begin
	    gen_origin (static_area, item_addr + size_init + size_uninit);
	    new (cr, halfwords);
	    with cr^ do begin
	      xwd.lh := 0;
	      xwd.rh := 0;
	      lreloc := none;
	      rreloc.kind := static_sc;
	      rreloc.relsym := symbols; (* word's own address *)
	    end;
	    gen_emit (static_area, cr);
	  end;
          symbols := next;
        end;
      end (* while symbols <> nil *);
      block := downward_call_thread;
    end;
  end (* while block <> nil *);

  gen_origin (static_area, loc_static + size_init); (* start of uninit. static *)
  emit_code (static_area, loc_static, prog_options.semantic_options);
  if (size_uninit > 0) and (assembly_opt in prog_options.semantic_options) then
    mac_pad (size_uninit) (* put BLOCK for uninit. static into macro listing *)
 end;
$PAGE rst_stack, kill_temps
(* In order to release stack space allocated to a dynamic temporary, a special
   operator "reset_stk_op" is inserted before either the start of the next
   statement or the last jump group in the statement.  This indicates that the
   stack end must be reset before the next statement or the jump is executed.
   In addition, the stack is reset at all labels which are the targets of non-
   local goto's;  this insures that the stack is properly unwound if the goto
   comes from a quick internal procedure.

   A dynamic temporary is introduced with an alc_temp_op.  However, due to folding
   the size operand may be a constant.  In such a case, a reqular temporary is
   introduced and the stack need not be reset.  The global flags below help
   keep track of when a reset is required.

   To reset the stack, a temporary which holds the initial value of the stack
   end pointer (reg=sb=17B) is created in the prologue.  It is marked as un-
   allocated, so that if there are no reset operations, it will be deleted. *)

var
  dynamic_temps_allocated: boolean;     (* alc_temp with non-constant size processed *)
  reset_needed: boolean;                (* set to above when reset_stk_op processed *)
  saved_sb: val_desc;                   (* temp holding initial stack end pointer *)


(* RST STACK generates code to reset the end of the current stack frame.  This
   marks the "saved_sb" temporary as used. *)

procedure rst_stack;
 begin
  saved_sb^.allocate := true;           (* "used" indicator *)
  gen_rm (move, sb, saved_sb^.loc);     (* fetch the initial stack end *)
  reset_needed := false;
 end;


(* FREE TEMPS is called at those points where a reset stack may be required.  If
   the operation is actually needed, it is generated. *)

public procedure kill_temps;
 begin
  if reset_needed then rst_stack;
 end;
$PAGE do_movem
(* DO MOVEM moves the contents of a register or register pair ("reg") into
   a memory location ("maddr").  The precision "p" denotes whether a single
   register or a pair is to be moved.  "Alignment" gives the data alignment
   requirements of the value in the register;  it is assumed to be compatible
   with that of the target. *)

procedure do_movem (maddr: addr_desc; reg: registers; p: bit_range; alignment: data_alignment);
 begin
  case maddr.mode of
    fw:     if p > 36
              then gen_rm (dmovem, reg, maddr)
            else if regdesc[reg]^.loc.mode = fw
              then gen_rm (movem, reg, maddr)
            else if regdesc[reg]^.signed                (* rhw -> fw *)
              then gen_rm (hrrem, reg, maddr)
              else gen_rm (hrrzm, reg, maddr);

    lhw:    if alignment = left_aligned
              then gen_rm (hllm, reg, maddr)
              else gen_rm (hrlm, reg, maddr);

    rhw:    if alignment = left_aligned
              then gen_rm (hlrm, reg, maddr)
              else gen_rm (hrrm, reg, maddr);

    byte:   begin
              if alignment = left_aligned then begin
                clr_reg (reg);
                right_shift (reg, 36, 36 - maddr.slice_size);
              end;
              gen_rm (dpb, reg, maddr);
            end;

    slice:  begin
              if alignment = left_aligned then begin
                clr_reg (reg);
                right_shift (reg, 36, 36 - maddr.slice_size);
              end;
              gen (dpb, reg, 0, 0, gen_bptr (maddr));
            end
  end;
 end;
$PAGE scalar_assignment
(* SCALAR ASSIGNMENT generates code for assignments of scalar values. *)

procedure scalar_assignment (tpl: tuple);

 var
   r: registers;
   maddr: addr_desc;
   scalar_val: int_type;
   opc: opc_range;

 begin
  with tpl^ do begin
    prepare (anyreg, rhs);
    prepare (anyreg, lhs);
    maddr := locate (lhs);
    if (maddr.mode in [fw, lhw, rhw]) andif iconstp (rhs, scalar_val) andif
      ((scalar_val = 0) orif (scalar_val = -1)) then begin
        case maddr.mode of
          fw:
            if scalar_val = 0
              then opc := setzm
              else opc := setom;
          lhw:
            if scalar_val = 0
              then opc := hrrzs
              else opc := hrros;
          rhw:
            if scalar_val = 0
              then opc := hllzs
              else opc := hllos
        end;
        gen_rm (opc, 0, maddr);
    end
    else begin
      r := load (anyreg, rhs, rhs^.desc.int_prec);
      lock (r);
      if rhs^.desc.signed
        then do_movem (maddr, r, 36, signed_value)
        else do_movem (maddr, r, 36, unsigned_value);
      unlock (r);
    end;
    free (rhs);
    free (lhs);
  end (* with *) ;
 end;
$PAGE real_assignment
(* REAL ASSIGNMENT generates code for assignments of real values. *)

procedure real_assignment (tpl: tuple);

 var
   r: registers;
   maddr: addr_desc;
   regsize: bit_range;

 begin
  with tpl^ do begin
    prepare (anyreg, rhs);
    prepare (anyreg, lhs);
    if rhs^.desc.precision > srealprec
      then regsize := 72
      else regsize := 36;
    r := load (anyreg, rhs, regsize);
    lock (r);
    maddr := locate (lhs);
    do_movem (maddr, r, regsize, right_aligned);
    unlock (r);
    free (rhs);
    free (lhs);
  end (* with *) ;
 end;
$PAGE set_assignment
(* SET ASSIGNMENT generates code for assignments of set values *)

procedure set_assignment (tpl: tuple);

 var
   r: registers;
   maddr: addr_desc;

 begin
  with tpl^ do begin
    if lhs^.opcode = desc_ref
      then ls_move (lhs, rhs)
    else begin
      prepare (anyreg, rhs);
      prepare (anyreg, lhs);
      if lhs^.desc.set_length > 0 then begin
	r := load (anyreg, rhs, lhs^.desc.set_length);
	lock (r);
	maddr := locate (lhs);
	do_movem (maddr, r, lhs^.desc.set_length, left_aligned);
	unlock (r)
      end;
      free (rhs);
      free (lhs);
    end;
  end (* with *) ;
 end;
$PAGE cst_addr
(* CST ADDR returns true if the described memory address is completely
   known at compile time. *)

public function cst_addr (mem: addr_desc): boolean;
 begin
  with mem do
    cst_addr := immediate and (index = noreg) and (reloc.kind <> temp_sc)
 end;
$PAGE do_blt
(* DO BLT generates the code necessary to move multiple memory words with a BLT
   instruction.  It takes expressions representing the source and destination
   addresses for the move, and an expression representing the number of "units"
   to be moved.  An integer parameter represents the number of "units" in a
   memory word. *)

public procedure do_blt (source, dest, width: expr; density: elem_sizes );



var 
    src_addr, dest_addr: addr_desc;
    r, r1: registers;
    n_words: align_range;
    width_known: boolean;

begin

  (*  If both the source and destination addresses are known at compile
      time, the BLT control word may be generated as a constant and
      loaded.  Otherwise, it must be constructed.  *)

  prepare (anyreg, dest);
  prepare (anyreg, source);
  dest_addr := locate (dest);

  if cst_addr (dest_addr) then begin (* destination address known *)
    src_addr := locate (source); (* no mem_lock needed on constant address *)
    if cst_addr (src_addr) then begin (* constant BLT word *)
      r := get_reg (anyreg, 36);
      gen (move, r, 0, 0, gen_blt (src_addr.offset, src_addr.reloc, dest_addr.offset, dest_addr.reloc));
      free (dest);
    end
    else begin (* construct BLT word *)
      mem_lock (src_addr);
      r := dload (anyreg, dest, 36);
      mem_unlock (src_addr);
      gen_rm (hrl, r, src_addr);
    end
  end
  else begin (* destination address unknown - construct BLT word *)
    r := dload (anyreg, dest, 36);
    lock (r);
    src_addr := locate (source);
    unlock (r);
    gen_rm (hrl, r, src_addr);
  end;
  free (source);

  (*  Once the BLT control word is in register r, several different code
      sequences may be generated:

      If the length and the destination address are both known:
          BLT    r, dest+len-1

      If the length is known, and the destination address is "nd(rd)":
          BLT    r, nd+len-1(rd)

      If the length is known, and the destination address is totally unknown:
          HRRZI  r1, 0(r)
          BLT    r, len-1(r1)

      If the length is unknown, and the destination address is known:
          MOVE   r1, len
          JUMPLE r1, .+2
          BLT    r, dest-1(r1)

      If neither the length nor the destination address is known:
          MOVE   r1, len
          JUMPLE r1, .+3
          ADDI   r1, -1(r)
          BLT    r, 0(r1)                                               *)


  width_known := iconstp (width, n_words);

  if width_known then begin
    if n_words = 0 then return; (* <----  no code for zero-length copy *)
    n_words := (n_words + density - 1) div density;
  end;

  if cst_addr (dest_addr) then begin
    if width_known then begin

      (*  The destination address and width are both known.  The BLT control
          word is in register r.  We modify dest_addr to refer to the last
          address for the transfer, and then perform the BLT.  *)

      free (width);
      dest_addr.immediate := false;
      dest_addr.offset := dest_addr.offset + n_words - 1;
      gen_rm (blt, r, dest_addr);
    end
    else begin

      (*  The destination address is known, but the width isn't.  We must get
          the width in a register and make sure it is greater than zero.  We
          decrement the destination address and give it the length register as
          an index; this gives the address of the last destination word.  *)

      prepare (anyreg, width);
      if density = 1 then begin
        r1 := load (anyreg, width, 36);
        free (width);
      end
      else begin
        r1 := prep_operand (width, anyreg, 36, 72);
        gen_ri (addi, r1, density-1);
        gen_ri (idivi, r1, density);
        free_reg (regdesc[r1]);
      end;
      gen (jump+lec, r1, 0, 2, dot);
      dest_addr.immediate := false;
      dest_addr.offset := dest_addr.offset - 1;
      dest_addr.index := r1;
      gen_rm (blt, r, dest_addr);
    end
  end
  else begin
    if width_known then begin

      (*  The width is known, but the destination address is not.  If the
          destination address is immediate, however, it may still be modified
          to refer to the last address for the transfer.  Otherwise, it must
          be copied from the BLT word.  *)

      free (width);
      if usage (dest) <> 0 then
        dest_addr := locate (dest); (* where is it now? *)
      if dest_addr.immediate andif
         ( (usage (dest) <> 0) or
           (dest_addr.index = sp) or
           (dest_addr.reloc.kind = temp_sc) ) then begin
        dest_addr.immediate := false;
        dest_addr.offset := dest_addr.offset + n_words - 1;
        gen_rm (blt, r, dest_addr);
      end
      else begin
        if (usage (dest) <> 0) andif is_register (dest_addr) then
          r1 := dest_addr.offset
        else begin
          r1 := get_reg (anyreg, 36);
          gen_rx (hrrzi, r1, r);
          if usage (dest) = 0
            then free_reg (regdesc[r1])
            else tag_reg (r1, dest);
        end;
        gen (blt, r, r1, n_words-1, none);
      end;
    end
    else begin

      (*  Neither the destination nor the width is known.  We have to load
          almost *everything*.  *)

      if density = 1 then
        r1 := dload (anyreg, width, 36)
      else begin
        r1 := prep_operand (width, anyreg, 36, 72);
        gen_ri (addi, r1, density-1);
        gen_ri (idivi, r1, density);
      end;
      free_reg (regdesc[r1]);
      gen (jump+lec, r1, 0, 3, dot);
      gen (addi, r1, r, -1, none);
      gen (blt, r, r1, 0, none);
    end;
  end;

  unlock (r);
  free_reg (regdesc[r]);
 end (* do_blt *);
$PAGE agg_assignment
(* AGG ASSIGNMENT generates code for assignments of array and record values. *)

procedure agg_assignment (tpl: tuple);

 type mask_array = packed array [0..17] of 000000b .. 777777b;

 const mask: mask_array =
     (  000000b, 400000b, 600000b,  700000b, 740000b, 760000b,
        770000b, 774000b, 776000b,  777000b, 777400b, 777600b,
        777700b, 777740b, 777760b,  777770b, 777774b, 777776b  );

 var
    maddr: addr_desc;
    r: registers;
    agg_width: align_range;

 begin
  with tpl^ do begin

    if rhs^.opcode = func_call_op then begin

      (* An aggregate function call may be compiled as one of:
            (a)  mem_ref := func_call_op
            (b)  desc_ref := func_call_op
         In case (a) , the mem_ref must be converted to an addr_ref and
         passed as the return value address for the function.  In case (b),
         only the address from the desc_ref is interesting. *)

      if lhs^.opcode = desc_ref then begin (* case (b) *)
        pas_call (rhs, lhs^.operand[1]);
        free (lhs^.operand[1]);
        free (lhs^.operand[2]);
      end
      else begin (* case (a)  *)
        lhs^.opcode := addr_ref;
        pas_call (rhs, lhs);
        free (lhs);
        lhs^.opcode := mem_ref; (* just like it used to be *)
      end;
      free (rhs);
    end

    else (* rhs^.opcode <> func_call_op *) begin

      (*  An aggregate assignment may have any of the following forms:
            (a)  mem_ref := mem_ref (1 word)
            (b)  mem_ref := mem_ref (2 words)
            (c)  desc_ref := desc_ref
          Case (a) can be done simply with a MOVE, MOVEM, and case (b) can be
          done with a DMOVE, DMOVEM.  Finally, case (c) requires construction
          of a BLT, which allows for lots of ingenuity.  *)

      if lhs^.opcode <> desc_ref then begin
        prepare (anyreg, lhs);
        prepare (anyreg, rhs);
	agg_width := ngm (lhs^.desc.base^.base_size, 36);
	if agg_width > 0 then begin
	  r := load (anyreg, rhs, agg_width);
	  lock (r);
	  maddr := locate (lhs);
	  unlock (r);
	  do_movem (maddr, r, agg_width, left_aligned)
	end;

        free (lhs);
        free (rhs);
      end

      else (* descriptor assignment *) begin

        (*  The assignment has the form DESC (lhs, len) := (rhs,len).
            Lhs and rhs are addr_ref's for the destination and source.
            The same len is used on both sides; it is the number of words
            to be moved.  This length expression is freed once by "do_blt",
            so it must be freed once here, too.  *)

        free (rhs^.operand[2]);
        do_blt (rhs^.operand[1], lhs^.operand[1], lhs^.operand[2], 1);

      end (* descriptor assignment *);
    end (* if rhs^.opcode <> func_call_op *);
  end (* with tpl^ *);
 end;
$PAGE prologue
(* PROLOGUE establishes the register state at block entry.  Specifically, it scans
   the list of tuples preceding the first label and performs the following actions:
   (1) tags the display_op as residing in reqister 16B (sp), (2) tags all parameters
   as residing in the registers in which they are past, and (3) queues stores
   of the parameter values to temporaries (in case they must be displaced from
   their registers). *)

procedure prologue;

 var
   tpl: tuple;
   reg: registers;
   vdesc: val_desc;
   cur_rs: reg_state;

 begin
  tpl := t_chain^.next;
  reg := 2;                     (* number of reg in which to place next parm *)
  while tpl^.opcode <> label_node do begin
    with tpl^ do begin
      case opcode of

        display_op:                     (* must be level 0 display *)
          begin
            vdesc := get_value_desc (tpl);
	    vdesc^.generated := true;
            tag_reg (get_reg (sp, 36), tpl);
	    regdesc[sp]^.loc.mode := rhw;
          end;

        eval_op:                        (* argument is a parameter *)
          begin
            vdesc := get_value_desc (rhs);
            with vdesc^ do begin
              loc := parm_reference;
              loc.offset := rhs^.item.offset;
              loc.reloc.relsym := rhs^.item.sym_name;
            end;
            tag_reg (get_reg (reg, word_size (rhs) * 36), rhs);
            store_reg (reg, vdesc^.loc);
            reg := reg + regdesc[reg]^.group_size;
            free (rhs);
            if (usage (rhs) = 0) and (rhs^.item.index <> nil) then
              free (rhs^.item.index);
          end

      end (* case *) ;
      tpl := next;
    end (* with *) ;
  end (* while *) ;

  (* Save a copy of the stack end pointer for use in the release stack space
     allocated to dynamic temporaries. *)

  saved_sb := get_temp (1);                     (* get space for a pointer *)
  saved_sb^.allocate := false;                  (* until actually used *)
  saved_sb^.free_at_stmt := false;              (* global lifetime *)
  saved_sb^.global := true;
  gen_rm (movem, sb, saved_sb^.loc);            (* this is deleted if saved_sb not allocated *)

  (* The registers in which the parameters arrive are not used by the current
     routine, unless they are reallocated at some later point.  The following
     adjusts the register usage information to reflect this fact. *)

  regs_used := [];              (* none of above allocations are 'uses' *)

  (* Set register state of initial basic block. *)

  rs_save (cur_rs);
  bb_merge (tpl, cur_rs);       (* tpl is label node of 1st basic block *)

  (* Initialize descriptor for stack locations where handler state blocks will reside. *)

  hsb_addr := absolute_reference;
  with hsb_addr do begin
    index := sp;
    reloc.kind := def_sc;
    reloc.reldef := make_def (local_def);
  end;
 end;
$PAGE case_jump
(* CASE JUMP generates code for a case jump operator and following jump in operators.
   The parameter "tpl" points to the case operator on entry; on return, it is
   set to the last jump in operator. *)

procedure case_jump (var tpl: tuple);

 var
   reg: registers;
   tv, tvoff: def;
   jmp: tuple;
   i: int_type;

 begin
  with tpl^ do begin
    prepare (anyreg, cond);
    if next^.opcode <> jump_in_op then begin (* no cases! *)
      free (cond);
      bb_end;
      if jump_to <> next then
        gen_rl (jrst, 0, jump_to);
    end

    else (* next^.opcode = jump_in_op *) begin
      reg := load (anyreg, cond, 18);
      free (cond);
      lock (reg);
      bb_end;                   (* save regs, free dynamic temps at end of basic block *)
      unlock (reg);
      gen_rm (cam+ltc, reg, int_value (low_cntrl));
      gen_rm (cam+lec, reg, int_value (high_cntrl));
      gen_rl (jrst, 0, jump_to);

      genind (jrst, 0, reg, 1 - low_cntrl, dot);

      jmp := next;
      loop              (* over jump_in_op's, assume at least one *)
        for i := jmp^.low_cntrl to jmp^.high_cntrl do
          gen_rl (arg, 0, jmp^.jump_to);
      exit if jmp^.next^.opcode <> jump_in_op;
        jmp := jmp^.next;
        for i := jmp^.prev^.high_cntrl + 1 to jmp^.low_cntrl - 1 do
          gen_rl (arg, 0, tpl^.jump_to);        (* goto 'others' for unspecified cases *)
      end;
      tpl := jmp;                       (* advance to last jump in op *)
    end;
  end (* with *) ;
 end;
$PAGE hnd_jump
(* HND JUMP generates the handler branch table for a handler jump operator and
   its following jump cond operators.  The parameter "tpl" points to the handler
   operator on entry; on return, it is set to the last jump in the list. *)

procedure hnd_jump (var tpl: tuple);

 var
   lab: tuple;
   cr: code;
   cond_addr: addr_desc;
   jmp: tuple;
   area: code_list;

 begin
  rst_stack;
  genind (jrst, 0, 0, 0, none); (* jrst @0 *)
  lab := tpl^.jump_from;
  if overlaid
    then area := hbt_area
    else area := code_area;
  mark_def (area, get_def (hnd_tab_def, lab^.block_order_no)); (* ref'd by set_handler_op *)

  new (cr, halfwords); (* first word of handler branch table *)
  with cr^ do begin
    xwd.lh := 0;
    xwd.rh := 0;
    lreloc := reldef (get_def (label_def, lab^.block_order_no));
    if lab^.in_handler = nil
      then rreloc := none
      else rreloc := reldef (get_def (hnd_tab_def, lab^.in_handler^.block_order_no));
  end;
  gen_emit (area, cr);

  new (cr, halfwords); (* second word of handler branch table *)
  with cr^ do begin
    xwd.lh := 0;
    lreloc := none;
    xwd.rh := (tpl^.high_cntrl - 1) * 2;
    rreloc := hsb_addr.reloc;
  end;
  gen_emit (area, cr);

  jmp := tpl;
  while jmp^.next^.opcode = jump_cond_op do begin
    jmp := jmp^.next;
    new (cr, halfwords);
    prepare (anyreg, jmp^.cond);
    cond_addr := locate (jmp^.cond);
    free (jmp^.cond);
    with cr^ do begin
      xwd.lh := cond_addr.offset;
      lreloc := cond_addr.reloc;
      xwd.rh := 0;
      rreloc := reldef (get_def (label_def, jmp^.jump_to^.block_order_no));
    end;
    gen_emit (area, cr);
  end;

  new (cr, halfwords); (* last word of handler branch table *)
  with cr^ do begin
    lreloc := none;
    assert (tpl^.jump_to^.opcode = label_node);
    if tpl^.jump_to^.next^.opcode = resignal_op then begin
      xwd.lh := 777777b;
      xwd.rh := 0;
      rreloc := none;
    end
    else begin
      if tpl^.low_cntrl = 0
	then xwd.lh := 777777b
	else xwd.lh := 777776b;
      rreloc := reldef (get_def (label_def, tpl^.jump_to^.block_order_no));
    end;
  end;
  gen_emit (area, cr);

  bb_end;
  tpl := jmp; (* advance to last jump_cond_op *)
  if overlaid
    then hbt_area := area
    else code_area := area;
 end;
$PAGE rt_cond_call
(* RT COND CALL generates a runtime routine call for one of the simple
   exception routines, given "rts", the runtime routine code, and "cond",
   the condition argument. *)

procedure rt_cond_call ( rts: rt_symbol; cond: expr );

 var
   cond_addr: addr_desc;

 begin
  prepare (anyreg, cond);
  clr_rv;
  cond_addr := locate (cond);
  free (cond);
  gen_rt (pushj, sb, rts);
  gen_rm (arg, 0, cond_addr);
 end;
$PAGE rt_io_call
(* RT IO CALL generates a runtime routine call for one of the simple i/o
   routines, given "op", the runtime statement tuple, and "rts", the runtime
   routine code.  If there are both explicit and implicit argument forms of
   a call, it is assumed that the code for the implicit call is the successor
   of the code for the explicit call. *)

procedure rt_io_call ( op: tuple; rts: rt_symbol );

 var
    f: expr;
    mem: addr_desc;

 begin
  with op^ do begin
    if old_file then begin
      clr_rv;
      gen_rt (pushj, sb, succ (rts));
    end

    else begin
      f := file_arg;
      if (f^.opcode = io_var_str_op) or (f^.opcode = io_fix_str_op) then
        f := f^.operand[1];
      prep_direct (anyreg, f);
      clr_rv;
      mem := argument (f);
      free (f);
      gen_rt (pushj, sb, rts);
      gen_rm (arg, 0, mem);
    end;
  end;
 end;
$PAGE rt_seek_call
(* RT SEEK CALL generates a call to the runtime file random access routine,
   using the specified file and index arguments. *)

procedure rt_seek_call ( fil, ind: expr );

 var fil_mem, ind_mem: addr_desc;

 begin
  prep_direct (anyreg, fil);
  prepare (anyreg, ind);
  clr_rv;
  fil_mem := argument (fil);
  mem_lock (fil_mem);
  ind_mem := fetch_direct (ind, 36);
  mem_unlock (fil_mem);
  free (fil);
  free (ind);
  gen_rt (pushj, sb, rt_seek);
  gen_rm (arg, 0, fil_mem);
  gen_rm (arg, 0, ind_mem);
 end;
$PAGE value_check
(* VALUE CHECK compiles a three-operand range check operator ("tpl") and
   branches to the runtime error routine ("rts") if the first operand is
   not within the range designated by the other two. *)

procedure value_check (tpl: tuple; rts: rt_symbol);

 var
   reg: registers;
   i: 1..3;
   low_limit, high_limit: addr_desc;

 begin
  with tpl^ do begin
    for i := 1 to 3 do          (* compile the subtrees of the operators *)
      if operand[i] <> nil then
	prepare (anyreg, operand[i]);

    reg := load (anyreg, operand[1], 36);       (* will do compare in register *)
    lock (reg);
    if operand[2] <> nil then begin
      low_limit := fetch (operand[2], 36);
      if low_limit.reloc.kind = register_sc
	then lock (low_limit.offset)
      else if low_limit.index <> noreg
	then lock (low_limit.index);
    end;
  
    if operand[3] <> nil then
      high_limit := fetch (operand[3], 36);

    unlock (reg);                       (* free the operands *)
    if operand[2] <> nil then begin
      if low_limit.reloc.kind = register_sc
	then unlock (low_limit.offset)
      else if low_limit.index <> noreg
	then unlock (low_limit.index);
    end;

    for i := 1 to 3 do
      if operand [i] <> nil then
	free (operand [i]);

    if operand[2] <> nil then
      if operand[3] <> nil then
	gen_rm (cam+ltc, reg, low_limit)
      else
	gen_rm (cam+gec, reg, low_limit);
    if operand[3] <> nil then
      gen_rm (cam+lec, reg, high_limit);
    gen_rt (jsp, 1, rts);
  end (* with *) ;
 end;
$PAGE compat_check
(* COMPAT CHECK compiles a compatibility check ("tpl") and branches to the
   runtime compatibility error routine if the two arguments are not equal. *)

procedure compat_check ( tpl: tuple );

 var
   reg: registers;
   adr: addr_desc;

 begin
  with tpl^ do begin
    prepare (anyreg, operand[1]);
    prepare (anyreg, operand[2]);

    reg := load (anyreg, operand[1], 36);
    lock (reg);
    adr := fetch (operand[2], 36);

    unlock (reg);
    free (operand[1]);
    free (operand[2]);

    gen_rm (cam+eqc, reg, adr);
    gen_rt (jsp, 1, rt_cmp_chk);
  end (* with tpl^ *);
 end;
$PAGE pointer_check
(* POINTER CHECK compiles a ptr_chk tuple ("tpl"). *)

procedure pointer_check (tpl: tuple; rts: rt_symbol);
 var reg: registers;
 begin
  with tpl^ do begin
    prepare (anyreg, operand[1]);
    reg := load (anyreg, operand[1], 36);
    free (operand[1]);
    gen (jump+eqc, reg, 0, 2, dot);
    gen_ri (cai+nec, reg, 377777B);
    gen_rt (jsp, 1, rts);
  end (* with *) ;
 end;
$PAGE string_range_check
(* STRING RANGE CHECK checks to see if a substring reference is within limits.
   That is it compiles str_range_chk (i, l, len) to check for the condition:
   1 <= i <= i+l <= len+1. *)

procedure string_range_check (tpl: tuple);

 var
   i_reg, l_reg: registers;
   len: addr_desc;

 begin
   with tpl^ do begin
    prepare (anyreg, operand[1]);                       (* prepare to access arguments *)
    prepare (anyreg, operand[2]);
    prepare (anyreg, operand[3]);

    i_reg := load (anyreg, operand[1], 36);     (* fetch the operands *)
    free (operand[1]);
    lock (i_reg);
    l_reg := load (anyreg, operand[2], 36);
    free (operand[2]);
    lock (l_reg);
    len := fetch (operand[3], 36);
    free (operand[3]);
    unlock (i_reg);
    unlock (l_reg);

    gen (jump+ltc, l_reg, 0, 5, dot); (* generate the check *)
    gen (jump+lec, i_reg, 0, 4, dot);
    gen (movei, 1, i_reg, -1, none);
    gen_rr (add, 1, l_reg);
    gen_rm (cam+lec, 1, len);
    gen_rt (jsp, 1, rt_str_chk);
  end;
 end;
$PAGE alc_temps
(* ALC TEMPS allocates addresses for the temporary variables in the current
   block.  Each value descriptor on the vdesc_list which is marked as "allocate"
   must be given an address relative to the stack frame of the owner of the
   current block.  Value descriptors fall into three classes:

   FREE_AT_STMT:  These last only for the duration of a single statement,
        and are allocated following the highest address used by any other
        block with the same owner which is called in this statement.

   ~FREE_AT_STMT and ~GLOBAL:  These last for the duration of a basic block,
        and are allocated following the highest address used in any statement
        within the basic block.  (If there are multiple basic blocks in a
        single statement, they are treated as a single basic block.)

   GLOBAL:  These last for the duration of the block, and are allocated
        following the last address used for any basic block within the block.

   If "stack_frame" is not nil, then it points to a definition node whose value
   must be defined as the total stack frame size for this block. *)


procedure alc_temps ( stack_frame: def );

 var
   code_ptr: code; (* used to step through the code area *)
   block_ptr: code; (* records the start of the current basic block *)
   stmt_ptr: code; (* records the start of the current stmt *)

   end_stmt: boolean; (* true if the end of a statement has been reached *)
   end_block: boolean; (* true if the end of a basic block has been reached *)
   block_next: boolean; (* true if statement end is also block end *)
$PAGE next_code - in alc_temps
(* NEXT CODE advances the code pointer. *)

procedure next_code;
 begin

  if end_stmt then begin
    end_stmt := false;
    if block_next then begin
      end_block := true;
      block_next := false;
    end
    else
      stmt_ptr := code_ptr;
  end

  else if end_block then begin
    end_block := false;
    block_ptr := code_ptr;
    stmt_ptr := code_ptr;
  end

  else begin
    code_ptr := code_ptr^.next;
    while (code_ptr <> nil) andif
          not (code_ptr^.kind in [instruction, deftemp]) do begin
      with code_ptr^ do begin
        if kind = source then
          end_stmt := true
        else if (kind = defmark) andif (defname^.deftype = label_def) then
          block_next := true;
        code_ptr := next;
      end;
    end;
    if code_ptr = nil then begin
      end_stmt := true;
      block_next := true;
    end;
    block_next := block_next and end_stmt;
  end;
 end;
$PAGE init_code, reset_stmt, reset_block - in alc_temps
(* INIT CODE initializes the code pointer.  It leaves EndStmt and EndBlock false. *)

procedure init_code;
 begin
  code_ptr := code_area.first;
  while not ( (code_ptr^.kind = source) or
              ( (code_ptr^.kind = defmark) andif (code_ptr^.defname^.deftype = label_def) ) ) do
    code_ptr := code_ptr^.next;
  while not (code_ptr^.kind in [instruction, deftemp]) do
    code_ptr := code_ptr^.next;
  stmt_ptr := code_ptr;
  block_ptr := code_ptr;
  end_stmt := false;
  end_block := false;
  block_next := false;
 end;


(* RESET STMT resets the code pointer back to the start of the current statement. *)

procedure reset_stmt;
 begin
  code_ptr := stmt_ptr;
  end_block := false;
  end_stmt := false;
 end;


(* RESET BLOCK resets the code pointer back to the start of the current basic block. *)

procedure reset_block;
 begin
  code_ptr := block_ptr;
  stmt_ptr := code_ptr;
  end_block := false;
  end_stmt := false;
 end;
$PAGE alc_temps - main routine

 var
   global_stk_begin, block_stk_begin, stmt_stk_begin: code_address;
   vald: val_desc;
   called_block: blk;

 begin
  global_stk_begin := cur_block^.pos_stk_end;
  init_code;
  while code_ptr <> nil do begin
    block_stk_begin := cur_block^.pos_stk_end;
    while not end_block do begin
      stmt_stk_begin := cur_block^.pos_stk_end;
      while not end_stmt do begin
        with code_ptr^ do begin
          if (kind = instruction) andif (inst.opcode = pushj) andif
             (reloc.kind = def_sc) andif (reloc.reldef^.deftype = subr_def) then begin
            called_block := blk_list^ [reloc.reldef^.defnumber];
            if called_block^.owner = cur_block^.owner then
              stmt_stk_begin := max (stmt_stk_begin, called_block^.pos_stk_end);
          end;
        end;
        next_code;
      end (* while not end_stmt *);

      reset_stmt;
      while not end_stmt do begin
        if code_ptr^.kind = deftemp then
          with code_ptr^.tempname^ do
            if allocate and free_at_stmt then begin
              code_ptr^.tempname^.loc.offset := stmt_stk_begin;
              stmt_stk_begin := stmt_stk_begin + code_ptr^.tempname^.size;
            end;
        next_code;
      end;
      block_stk_begin := max (block_stk_begin, stmt_stk_begin);
      next_code;
    end (* while not end_block *);

    reset_block;
    while not end_block do begin
      if not end_stmt andif (code_ptr^.kind = deftemp) then
        with code_ptr^.tempname^ do
          if allocate and not free_at_stmt and not global then begin
            code_ptr^.tempname^.loc.offset := block_stk_begin;
            block_stk_begin := block_stk_begin + code_ptr^.tempname^.size;
          end;
      next_code;
    end;
    global_stk_begin := max (global_stk_begin, block_stk_begin);
    next_code;
  end (* while code_ptr <> nil *);

  (* Allocate the global temporaries. *)

  vald := vdesc_list;
  while vald <> nil do begin
    with vald^ do begin
      if allocate and global then begin
        loc.offset := global_stk_begin;
        global_stk_begin := global_stk_begin + size;
      end;
      vald := next;
    end;
  end;

  (* Allocate the handler state blocks. *)

  if cur_block^.hndlr_depth > 0 then begin
    def_value (hsb_addr.reloc.reldef, global_stk_begin, false);
    global_stk_begin := global_stk_begin + 2 * cur_block^.hndlr_depth;
  end;
  cur_block^.pos_stk_end := global_stk_begin;
  if stack_frame <> nil then
    def_value (stack_frame, cur_block^.pos_stk_end, false);

  (* Replace all temp references in instructions by absolute stack offsets. *)

  code_ptr := code_area.first;
  while code_ptr <> nil do begin
    with code_ptr^ do begin
      if (kind = instruction) andif (reloc.kind = temp_sc) then begin
        inst.index := sp;
        if inst.offset <= 3777777b
          then inst.offset := inst.offset + reloc.relval^.loc.offset
          else inst.offset := inst.offset - 1000000b + reloc.relval^.loc.offset;
        reloc.kind := absolute_sc;
      end
      else if (kind = bytepointer) andif (bpreloc.kind = temp_sc) then begin
        bptr.index := sp;
        if bptr.bpoffset <= 377777b
          then bptr.bpoffset := bptr.bpoffset + bpreloc.relval^.loc.offset
          else bptr.bpoffset := bptr.bpoffset - 1000000b + bpreloc.relval^.loc.offset;
        bpreloc.kind := absolute_sc;
      end;
      code_ptr := next;
    end;
  end (* while code_ptr <> nil *);

  (* Replace all temp references in the byte pointer area by absolute stack offsets. *)

  btmp_offsets;
 end (* alc_temps *);
$PAGE cleanup
(* CLEANUP scans the code area for the current block and removes references to
   unused temporaries and parameter locations. *)

procedure cleanup;

 var
   cr: code;                            (* code scanning cursor *)
   next_cr: code;
   last_cr: code;


 function used (rel: rel_syllable): boolean;    (* determines if location referenced is allocated *)
  begin
   case rel.kind of

     temp_sc:
       used := rel.relval^.allocate;    (* if storage assigned to the value *)

     parameter_sc:
       used := (rel.relsym = nil) orif  (* if parm base ptr *)
               (rel.relsym^.allocated) orif     (* if referenced in memory *)
               prog_options.debug_opt; (* if in debug mode *)

     others:
       used := true

   end (* case *) ;
  end;


 begin
  cr := code_area.first;
  last_cr := nil;
  while cr <> nil do begin
    next_cr := cr^.next;                (* remember following one in case cr^ deleted *)
    if ((cr^.kind = instruction) andif not used (cr^.reloc)) orif
       ((cr^.kind = deftemp) andif not cr^.tempname^.allocate)
      then begin                        (* delete spurious instruction *)
        if last_cr = nil        (* unchain the record *)
          then code_area.first := next_cr
          else last_cr^.next := next_cr;
        dispose (cr);                   (* delete the code record *)
      end
      else last_cr := cr;
    cr := next_cr;                      (* iteration step *)
  end;
 end;
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
(* COMPILE BODY compiles the body of the current block.  "Stack_frame" is the
   definition node (if any) which must be defined in alc_temps as the stack
   frame size of the block. *)

procedure compile_body ( stack_frame: def );

 var
   tpl: tuple;                          (* scanning cursor for compilation *)
   lab: tuple;
   d: def;
   reg: registers;
   mem: addr_desc;

 begin
  rd_tuples;                            (* fetch the intermediate form *)
  reg_reset (t_chain^.last_label^.block_order_no);

  prologue;             (* fetch parameters *)

  dynamic_temps_allocated := false;     (* initialize flags for rst_stack, kill_temps *)
  reset_needed := false;

  tpl := t_chain^.first_label;
  cur_bb := nil;
  while tpl <> nil do begin
    with tpl^ do begin
      case opcode of

        assign_op:
          begin
            case lhs^.desc.kind of
              bools, chars, ints, scalars, pointers, files, procs, funcs:
                scalar_assignment (tpl);
              reals:
                real_assignment (tpl);
              strings:
                str_assignment (tpl);
              sets:
                set_assignment (tpl);
              arrays, records:
                agg_assignment (tpl)
            end (* case *) ;
          end;

        eval_op:                                (* evaluate operand for its side effects *)
          begin
            prepare (anyreg, rhs);
            case rhs^.desc.kind of
              bools, ints, chars, scalars, pointers:
                reg := load (anyreg, rhs, rhs^.desc.int_prec);
              reals:
                if rhs^.desc.precision > srealprec
                  then reg := load (anyreg, rhs, 72)
                  else reg := load (anyreg, rhs, 36);
              sets:
                if rhs^.desc.set_length <= 72
                  then reg := load (anyreg, rhs, rhs^.desc.set_length)
            end;
            free (rhs);
          end;

        start_with:
          (* no action *);

        call_op:
          procedure_call (tpl);

        label_node:
          begin
            cur_bb := tpl;      (* tag current basic block *)
            bb_start;           (* establish register state at entry *)
            d := get_def (label_def, block_order_no);
            mark_def (code_area, d);
            if (label_sym <> nil) andif (label_sym^.lab_nonlocal_use) then begin
              d := get_def (sym_def, label_sym^.id_number);
              mark_def (code_area, d);
              rst_stack;                        (* must always reset stack on return from non-local goto *)
            end;
          end;

        jump_op:
          begin
            bb_end;             (* save regs/register state at exit; reset stack if needed *)
            if jump_to <> next  (* generate only if useful *)
              then gen_rl (jrst, 0, jump_to);
          end;

        jump_t_op, jump_f_op:
          begin
            tpl := tpl^.next;           (* skip to alternate jump op *)
            lab := get_next_action (tpl);       (* this may be label node *)
            if opcode = jump_t_op
              then test_and_jump (cond, jump_to, tpl^.jump_to, lab)
              else test_and_jump (cond, tpl^.jump_to, jump_to, lab);
          end;

        case_jump_op:
          case_jump (tpl);

        goto_op:
          begin
            d := get_def (sym_def, target_lab^.id_number);      (* get label definition *)
            if target_lab^.block^.owner = cur_block^.owner then begin
              gen (jrst, 0, 0, 0, reldef (d));
              if target_frame <> nil then
                free (target_frame);
            end
            else if target_lab^.block^.kind = program_blk then begin (* label is in main program *)
              gen_rt (pushj, sb, rt_uw_prg);
              gen (jrst, 0, 0, 0, reldef (d));
            end
            else begin (* label is in a containing routine *)
              prepare (anyreg, target_frame);   (* get sp of frame of subr containing label *)
              reg := load (anyreg, target_frame, 36);
              free (target_frame);
              gen_rt (pushj, sb, rt_uw_rtn);    (* unwind and jump *)
              gen (jump+awc, reg, 0, 0, reldef (d));    (* reg is arg to unwind *)
            end;
          end;

        (* Here we process the sequence of operators denoting generation of
           a boolean value for an andif/orif operator.  It is assumed that
           they remain in the order in which they appear in the original
           intermediate form:

                        gen_jump    1
                     1: label
                        gen_andif   A,B

           The label is reached from the short-circuit tests on A and B. *)

        gen_jump_op:
          begin
            bb_end;     (* save reg state at end gen_jump block *)
            tpl := tpl^.next;   (* presumably the jump_to label *)
            cur_bb := tpl;
            bb_start;   (* get entry register state at label *)
            tpl := tpl^.next;   (* this should be the gen_xxxif operator *)
            reg := get_reg (anyreg, 36);        (* get a register in which to load the value *)
            case tpl^.opcode of
              gen_andif_op:  gen (skip+awc, reg, 0, 0, gen_cint (1));
              gen_orif_op:   gen_ri (tdza, reg, reg);
	      others:        assert (false)
            end;
            d := get_def (label_def, cur_bb^.block_order_no);
            mark_def (code_area, d);
            case tpl^.opcode of
              gen_andif_op:  gen_ri (setz, reg, 0);
              gen_orif_op:   gen_ri (movei, reg, 1)
            end;
            tag_reg (reg, tpl);         (* remember where it is *)
          end;

        sub_range_chk:
          value_check (tpl, rt_sub_chk);

        str_range_chk:
          string_range_check (tpl);

        val_range_chk:
         value_check (tpl, rt_val_chk);

        ptr_chk:
          pointer_check (tpl, rt_ptr_chk);

        file_chk:
          pointer_check (tpl, rt_fil_chk);

        compat_chk:
          compat_check (tpl);

        dispose_op:
          begin
            prep_direct (anyreg, dptrarg);
            clr_rv;
            mem := argument (dptrarg);
            free (dptrarg);
            gen_rt (pushj, sb, rt_dispose);
            gen_rm (arg, 0, mem);
          end;

        start_stmt:
          begin
            kill_temps;                 (* reset stack if required *)
            cur_source := stmt_source;  (* for debugging *)
            gen_source (stmt_source, stmt_index);       (* comment for assembly listing *)
            if (stmt_index = 1) and prog_options.debug_opt then begin
              stmt_block (stmt_source, stmt_kind);
            end;
          end;

        stop_op:
          gen_rt (jrst, 0, rt_stop);

        return_op:
          begin
            if cur_block <> cur_block^.owner    (* if quick *)
              then gen_ri (popj, sb, 0)
              else gen_rt (jrst, 0, rt_return);
          end;

        abort_op:
          gen_rt (jsp, 1, rt_ass_chk);

        case_abort_op:
          gen_rt (jsp, 1, rt_cas_chk);

        (* Here we determine when it is necessary to release dynamic temporaries
           (see rst_stack, kill_temps).  Note that the check on the alc_temp_op
           could be done in p10exp, but is not in order to keep the flag local
           to this module. *)

        alc_temp_op:
          begin
            if not ((tpl^.operand[1]^.opcode = immed_ref) andif (tpl^.operand[1]^.item.index = nil))
              then dynamic_temps_allocated := true;
          end;

        reset_stk_op:
          begin
            reset_needed := dynamic_temps_allocated;    (* flag reset when reset done *)
            dynamic_temps_allocated := false;
          end;

	set_handler_op, rst_handler_op:
	  begin
	    if opcode = set_handler_op
	      then gen_rt (pushj, sb, rt_set_handler)
	      else gen_rt (pushj, sb, rt_rst_handler);
	    if hndlr_tuple = nil
	      then gen_ri (arg, 0, 0)
	      else gen (arg, 0, 0, 0, reldef (get_def (hnd_tab_def, hndlr_tuple^.block_order_no)));
	  end;

	signal_op:
	  rt_cond_call (rt_signal, cond_parm);

	mask_op:
	  rt_cond_call (rt_mask, cond_parm);

	unmask_op:
	  rt_cond_call (rt_unmask, cond_parm);

	resignal_op:
	  gen_rt (pushj, sb, rt_resignal);

	exmessage_op:
	  gen_rt (pushj, sb, rt_exmessage);

	hndlr_jump_op:
	  hnd_jump (tpl);

        get_op:
          if file_arg^.desc.base^.file_kind = textfile
            then rt_io_call (tpl, rt_get_char)
            else rt_io_call (tpl, rt_get);

        put_op:
          if file_arg^.desc.base^.file_kind = textfile
            then rt_io_call (tpl, rt_put_char)
            else rt_io_call (tpl, rt_put);

        readln_op:
          rt_io_call (tpl, rt_rd_lnn);

        writeln_op:
          rt_io_call (tpl, rt_wr_lnn);

        page_op:
          rt_io_call (tpl, rt_page);

        clear_op:
          rt_io_call (tpl, rt_clear);

        break_op:
          rt_io_call (tpl, rt_break);

        empty_op:
          if file_arg^.desc.base^.file_kind = textfile
            then rt_io_call (tpl, rt_empty_text)
            else rt_io_call (tpl, rt_empty);

        close_op:
          rt_io_call (tpl, rt_close);

        scratch_op:
          rt_io_call (tpl, rt_scratch);

        end_io_op:      (* only passed through for putstring calls *)
          rt_io_call (tpl, rt_wr_dnn);

        read_op:
          read_write_call (tpl);

        write_op:
          read_write_call (tpl);

        seek_op:
          rt_seek_call (seek_file, seek_index);

        close_all_op:
          begin
            clr_rv;
            gen_rt (pushj, sb, rt_close_all);
          end;

        others:
          (* forget it *)

      end (* case *) ;
    end (* with *) ;
    tpl := tpl^.next;   (* this must be outside the with *)
  end;

  cleanup;                                      (* remove non-mandatory stores *)
  alc_temps (stack_frame); (* allocate temporary storage *)
  reg_use_by_block^[cur_block^.number] := regs_used; (* save register use info *)
  if prog_options.debug_opt then
    blk_end;    (* clean up debug info *)
  if hbt_area.first <> nil then begin
    set_origin (hbt_area, loc_hbt);
    set_origin (code_area, loc_code);
    wr_code (hbt_area, loc_hbt, false); (* define the handler addresses *)
  end;
  emit_code (code_area, loc_code, cur_block^.semantic_options);
  if hbt_area.first <> nil then
    emit_code (hbt_area, loc_hbt, cur_block^.semantic_options);
  temp_reset;
  del_tuples;                   (* get rid of IF for this block *)
 end;
$PAGE compile_subr
(* COMPILE SUBR generates code for the body of a procedure or function.  It is
   assumed that cur_block points to the block node for the subroutine on entry. *)

procedure compile_subr;

 var
   tb, entryaddr: def;
   lev: level_index;
   quick: boolean;
   stack_frame: def;

 begin
  gen_cmt (code_area, 'Begin subroutine ' || cur_block^.subr_sym^.name^.text);

  quick := (cur_block^.owner <> cur_block);     (* entry sequence differs for quick/nonquick *)

  if not quick then begin
    if prog_options.debug_opt or (trace_opt in cur_block^.semantic_options)
      then tb := trace_block (cur_block)        (* emit the trace control block *)
      else tb := nil;

    if (not quick) and (cur_block^.level > 2) then (* may be called by parent *)
      gen (hrli, sp, sp, 0, none); (* set static = dynamic link *)
  end;

  with cur_block^.subr_sym^ do
    if public_dcl then
      gen_asm_label (code_area, name^.text);
  entryaddr := get_def (subr_def, cur_block^.number);   (* define the entry point *)
  mark_def (code_area, entryaddr);

  if not quick then begin               (* generate full entry sequence *)
    gen_rt (jsp, 1, rt_entry);
    if tb <> nil        (* trace or debug in effect *)
      then gen_xwd (0, none, 0, reldef (tb))
      else gen_xwd (0, none, int_nil, none);
    stack_frame := get_def (temp_size_def, cur_block^.number);
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
    	    gen (adj_sp, sb, 0, 0, reldef (stack_frame)) 
	  end
    	else gen (adjsp, sb, 0, 0, reldef (stack_frame)) 
  end
  else
    stack_frame := nil;

  if cur_block^.return_sym <> nil       (* if function, mark return symbol *)
    then cur_block^.return_sym^.allocated := true;      (* to prevent deletion of ref'ing code records *)


  compile_body (stack_frame);                           (* compile the body of the subroutine *)
 end;
$PAGE compile_main
(* COMPILE MAIN generates code for the body of a main program.  It is assumed that
   cur_block points to the program's block node at entry. *)

procedure compile_main (var startaddr: def);

  var
   tb: def;
   stack_frame: def;

begin
  gen_cmt (code_area, 'Begin program ' || cur_block^.id^.text);

  if prog_options.debug_opt or (trace_opt in prog_options.semantic_options) 
    then tb := trace_block (cur_block)
    else tb := nil;

  startaddr := get_def (subr_def, cur_block^.number);
  mark_def (code_area, startaddr);

  gen_ri (jfcl, 0, 0);
  gen_ri (movei, sp, prog_options.storage);
  gen_rt (jsp, 7, rt_init);
  if tb <> nil
    then gen_xwd (0, none, 0, reldef (tb))
    else gen_xwd (0, none, int_nil, none);
  stack_frame := get_def (temp_size_def, cur_block^.number);
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
	    gen (adj_sp, sb, 0, 0, reldef (stack_frame)) 
	  end
	else gen (adjsp, sb, 0, 0, reldef (stack_frame));

  compile_body (stack_frame);   (* generate code for body of block *)
end;
$PAGE gen_code
(* GEN CODE is the driving program for the code generator.  It directs initialization,
   generation, and termination. *)

public procedure gen_code ( var code_size, const_size, static_size: unit_range );

var loc_fp: code_address;
  lowseg_break: unit_range;
  startaddr: def; (* start address of main program, nil otherwise *)
  temp_file: text;

begin
  cur_block := root_block^.children;	(* set immediately in case of assertion failure *)
  cur_source := (0, 0, 0);		(*  "      "       "   "   "      "        "    *)

  lowseg_break := size_init + size_uninit + size_cond;
  overlaid := prog_options.overlay_opt or prog_options.mainseg_opt;
  startaddr := nil;                     (* init globals in this module *)
  gen_init;     (* and others *)
  low_base := make_def (code_def);
  def_value (low_base, loc_static, true);
  high_base := make_def (code_def);
  def_value (high_base, loc_code, true);
  loc_hbt := lowseg_break;
  reg_init;
  if assembly_opt in all_opts then
    mac_header ('Optimizing compiler');
  if map_opt in all_opts then
    map_init;
  rel_init;
  init_static;

  (* First word is the module word for the debugger.  Emit it here even if this
     is a data module and doesn't contain any procedures. *)

  gen_origin (code_area, loc_code);
  deb_init; (* drop radix 50 module name, reserve prog block *)
  emit_code (code_area, loc_code, prog_options.semantic_options);

  (* Compile the constituent blocks in reverse depth first order. *)

  cur_block := root_block;      (* look for end of call chain *)
  while cur_block^.downward_call_thread <> nil do cur_block := cur_block^.downward_call_thread;

  while cur_block <> root_block do begin        (* compile in reverse order *)
    case cur_block^.kind of 
      program_blk:    compile_main (startaddr);
      subr_blk:       compile_subr
    end;
    cur_block := cur_block^.upward_call_thread;
  end;
  cur_block := root_block^.children;	(* in case of assertion failure *)

  (* Final cleanup *)

  pool_constants;
  if overlaid then begin
    loc_cst := loc_hbt; (* emit consts in lowseg for overlay compilation *)
    set_origin (cst_area, loc_cst);
  end
  else
    loc_cst := loc_code; (* normally, consts go at end of code area *)
  emit_code (cst_area, loc_cst, prog_options.semantic_options);
  emit_code (bptr_area, loc_cst, prog_options.semantic_options);
  emit_code (blt_area, loc_cst, prog_options.semantic_options);
  if prog_options.debug_opt then begin
    if overlaid
      then loc_fp := loc_code
      else loc_fp := loc_cst;
    gen_origin (code_area, loc_fp);
    fp_blocks;  (* dump page and file blocks in code area *)
    emit_code (code_area, loc_fp, prog_options.semantic_options);
    deb_stable; (* dump symbol table *)
    prog_block;
  end
  else if rel_file <> '' then begin
    reset (temp_file, rel_file || '.DEB[,]');
    scratch (temp_file)
  end;

  if prog_options.debug_opt then begin
    if overlaid
      then code_size := loc_fp - 400000b
      else code_size := (loc_fp - loc_cst) + (loc_code - 400000b);
  end
  else
    code_size := loc_code - 400000b;
  if overlaid
    then const_size := loc_cst - lowseg_break
    else const_size := loc_cst - loc_code;
  static_size := lowseg_break;

  if assembly_opt in all_opts then
    mac_end (startaddr, code_size, const_size, static_size);    (* terminate assembly listing *)
  if overlaid and prog_options.debug_opt then
    rel_end (startaddr, loc_cst, loc_fp)
  else if overlaid and not prog_options.debug_opt then
    rel_end (startaddr, loc_cst, loc_code)
  else if not overlaid and prog_options.debug_opt then
    rel_end (startaddr, lowseg_break, loc_fp)
  else
    rel_end (startaddr, lowseg_break, loc_code);
  reg_term;
  gen_term;
  if map_opt in all_opts then
    map_print;
end.
  *6mj