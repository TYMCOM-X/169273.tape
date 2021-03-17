$TITLE p10cll - Subroutine Call Utilities
$LENGTH 42

module p10cll;
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$INCLUDE p10cgu.inc
$INCLUDE p10opc.inc
$INCLUDE p10exp.inc
$PAGE get_regs_used
(* GET REGS USED returns an estimate of the registers used by a subroutine as
   designated by its location ("subrdef").   The algorithm used takes advantage
   of the fact that blocks are compiled in reverse depth first order.  If a
   subroutine is called which is deeper in the order than the current block,
   then it has already been compiled, and its register usage is known.  If the
   block called preceeds the current block, then its usage is not known and it
   is assumed that all registers are used for the sake of safety.  Note that this
   requires us to return only those registers *used so far* by the current block;
   this is because only those registers may need to be stored.  This is desirable
   because it keeps the set of used registers for the current block as small as
   possible. *)

function get_regs_used (subrdef: rel_syllable): set_of_registers;

 var
   blknum: index_range;
   blkp: blk;

 begin
  if subrdef.kind = external_sc
    then get_regs_used := [0..15B]      (* externals can clobber any reg *)

  else begin
    blknum := subrdef.reldef^.defnumber;        (* get id of block called *)
    if blknum = cur_block^.number
      then get_regs_used := regs_allocated              (* backward loop, assume all used *)
    else begin                                  (* search list for block *)
      blkp := cur_block^.downward_call_thread;
      loop
      exit if blkp = nil
        do get_regs_used := [0..15B];
      exit if blkp^.number = blknum
        do get_regs_used := reg_use_by_block^[blkp^.number];
        blkp := blkp^.downward_call_thread;
      end;
    end;
  end;
 end;
$PAGE pas_call
(* PAS CALL generates code to call a pascal procedure or function.  The parameter
   "node" is the call_op or func_call_op node for which code is to be generated;
   and "rv_addr" is a pointer to a location in which an aggregate valued function
   is to place its result.  Neither node nor rv_value are freed. *)

public procedure pas_call ( node: expr; rv_addr: expr );

 var
   subr_type: typ;
   called_blk: blk;
   saddr_def: rel_syllable;
   saddr_offset: int_type;
   pl_temp: val_desc;
   pl_loc: addr_desc;
   i: parm_range;
   reg: registers;
   display_loc: addr_desc;
   rv_reg: registers;

 begin
  with node^ do begin
    subr_type := subr^.desc.base;               (* info about parameter list *)

    (* When the parameter list occupies 6 words or less, the parameters are passed
       in registers 2-7. *)

    if subr_type^.parmlist_size <= 6 then begin
      reg := 2;
      for i := 1 to upperbound (arglist) do begin              (* compile argument sub-trees *)
        prepare (reg, arglist[i]);
        reg := reg + word_size (arglist[i]);
      end;

      if rv_addr <> nil then prepare (reg, rv_addr);    (* access the return value address if any *)
      prepare (anyreg, subr);                   (* if actual subr, immediate address is result
                                                   else if formal, subr value is loaded *)

      reg := 2;                                 (* load parameters into registers *)
      for i := 1 to upperbound (arglist) do begin
        load_reg (reg, arglist[i], word_size (arglist[i]) * 36);
        lock (reg);
        reg := reg + regdesc[reg]^.group_size;
        free (arglist[i]);
      end;
      rv_reg := reg;                            (* one past last parm reg *)
    end

    (* When there are more than 6 words in the parameter list, generate the argument
       list in memory and pass a pointer to the start of the list in register 2. *)

    else if upperbound (subr_type^.params) <> 0 then begin
      pl_temp := get_temp (subr_type^.parmlist_size);   (* get a place to store the argument list *)
      pl_temp^.allocate := true;                (* must have storage for temp *)
      pl_loc := pl_temp^.loc;                   (* addr of base of temp *)

      prepare (anyreg, subr);                   (* as in above case *)
      if rv_addr <> nil then prepare (anyreg, rv_addr);

      for i := 1 to upperbound (arglist) do begin              (* copy arguments to arglist *)
        prepare (anyreg, arglist[i]);
        reg := load (anyreg, arglist[i], word_size (arglist[i]) * 36);
        if regdesc[reg]^.group_size = 1
          then gen_rm (movem, reg, pl_loc)
          else gen_rm (dmovem, reg, pl_loc);
        pl_loc.offset := pl_loc.offset + regdesc[reg]^.group_size;
        free (arglist[i]);
      end;

      clr_reg (2);              (* pass ptr to parm list in 2 *)
      gen_rm (movei, 2, pl_temp^.loc);
      lock (2);
      rv_reg := 3;                              (* one past last parm reg *)
    end

    else rv_reg := 2;                           (* no parameters, place rv_addr in first parm reg *)

    (* If the subroutine is a function returning an aggregate value, the address
       where the function should place its result is placed in the first register
       after the registers used by the parameter list. *)

    if rv_addr <> nil then begin
      load_reg (rv_reg, rv_addr, 36);
      lock (rv_reg);
    end;

    (* Generate code for the actual call.  In the first case, we have a direct
       call to a subroutine constant - either a local or external subroutine id. *)

    if subr^.opcode = desc_ref then begin       (* operand is addr of subr *)
      with subr^.operand[1]^.item do begin      (* get definition and offset of entry point *)
        if class = external_sc then begin
          saddr_offset := 0;
          saddr_def.kind := external_sc;
          saddr_def.relsym := sym_name;
        end

        (* If the called procedure is non-quick and not at the top level, then
           the display must be updated.  This is done by branching to an offset
           before the procedure entry, to execute the static chain chasing code. *)

        else (* class <> external *) begin      (* declared in this compilation *)
          called_blk := sym_name^.init_value.blkp;
          saddr_def.kind := def_sc;
          saddr_def.reldef := get_def (subr_def, called_blk^.number);
          if (called_blk^.level <= 2) or (called_blk^.owner <> called_blk) then
            saddr_offset := 0 (* top-level or quick call *)
	  else if called_blk^.parent^.owner = cur_block^.owner then
	    saddr_offset := -1 (* set static = dynamic link *)
	  else begin
	    saddr_offset := 0;
	    assert (upperbound (subr^.operand) = 2);
	    prepare (anyreg, subr^.operand[2]); (* load static link in r16 LH *)
	    display_loc := locate (subr^.operand[2]);
	    do_move (sp, display_loc, left_unpadded, 18);
	  end;
        end;
      end;
      free (subr);
      free (subr^.operand[1]);
      if upperbound (subr^.operand) = 2 then
	free (subr^.operand[2]);

      clr_rv;                                   (* clear the return value slot if occupied *)
      kill_regs (get_regs_used (saddr_def));    (* save any regs in use which are killed by the called subr *)
      gen (pushj, sb, 0, saddr_offset, saddr_def);      (* generate the call *)
    end

    (* Indirect call thru a subroutine value.  The display for the called subroutine
       must be loaded, and the subroutine value called. *)

    else begin
      reg := load (anyreg, subr, 36);
      free (subr);

      clr_rv;                           (* clear return value slot *)
      kill_regs ([0..15B]);             (* assume all killed in lieu of better info *)

      gen_rr (hll, sp, reg);		(* load display *)
      gen_rx (pushj, sb, reg);                  (* call 0(reg) *)
    end;

    (* Release any registers or temporaries involved in the calling sequence. *)

    if subr_type^.parmlist_size <= 6 then begin
      for i := 2 to 2 + subr_type^.parmlist_size - 1
        do unlock (i);
    end
    else begin
      unlock (2);
    end;
    if rv_addr <> nil then unlock (rv_reg);
  end (* with *) ;
 end;
$PAGE for_call
(* FORTRAN CALL generates a call to a fortran procedure or function.  The parameter
   "node" is the call_op or func_call_op node. *)

procedure for_call (node: expr);

 var
   pl_temp: val_desc;
   pl_loc: addr_desc;
   i: parm_range;
   mem: addr_desc;
   saddr_def: rel_syllable;
   reg: registers;

 begin
  with node^ do begin

    (* For a call to an external fortran routine, a parameter list of the following
       form is constructed:

                        -n,,0
                16 ->   0,,parm 1 addr
                        0,,parm 2 addr
                         ...
                        0,,parm n addr

       Pointer values in the parameter list are assumed to refer to var parameters,
       and such values are placed in the parameter list as parm n addr's.  Values
       of other data types are assumed to be value parameters, and an address is
       also past, with a copy in storage created if necessary.  It is assumed that
       the fortran subroutine does not modify these parameters. *)

    pl_temp := get_temp (upperbound (arglist)+1);              (* get place to put addresses *)
    pl_temp^.allocate := true;          (* the temp must be allocated *)
    pl_loc := pl_temp^.loc;

    gen_ri (hrlzi, 1, -upperbound (arglist));          (* generate count word *)
    gen_rm (movem, 1, pl_loc);
    pl_loc.offset := pl_loc.offset + 1;

    for i := 1 to upperbound (arglist) do begin                (* fill in address words *)
      prepare (anyreg, arglist[i]);             (* compile sub-trees *)

      if arglist[i]^.desc.kind = pointers then begin    (* pass address of var parameter *)
        reg := load (anyreg, arglist[i], 36);
        gen_rm (movem, reg, pl_loc);
        free (arglist[i]);
      end

      else begin                                (* pass value parameter *)
        mem := fetch_direct (arglist[i], word_size (arglist[i]) * 36);
        if is_register (mem)
          then begin                            (* must get a copy in register *)
            clr_reg (mem.offset);       (* makes copy; clear since reg cannot surive ext call *)
            mem := locate (arglist[i]);
          end
          else begin                    (* already in memory, get address *)
            if usage (arglist[i]) = 1   (* free accessing reg's unless used again *)
              then consume (arglist[i]);
          end;
        gen_rm (movei, 1, mem); (* get the address *)
        gen_rm (movem, 1, pl_loc);      (* store in parm list *)
      end;

      pl_loc.offset := pl_loc.offset + 1;
    end (* for *) ;

    for i := 1 to upperbound (arglist) do begin        (* free value parameters and their temps *)
      if arglist[i]^.desc.kind <> pointers
        then free (arglist[i]);
    end;

    (* Generate the call *)

    saddr_def.kind := external_sc;
    saddr_def.relsym := subr^.operand[1]^.item.sym_name;
    free (subr);
    free (subr^.operand[1]);

    clr_rv;                                             (* clear the return value slot *)
    kill_regs ([0..15B]);                       (* must assume external proc kills all registers *)
    gen_rr (push, sb, sp);              (* 16 is used as arg list pointer *)
    pl_loc := pl_temp^.loc;
    pl_loc.offset := pl_loc.offset + 1;
    gen_rm (movei, sp, pl_loc);
    gen (pushj, sb, 0, 0, saddr_def);   (* generate the call *)
    gen_rr (pop, sb, sp);               (* restore 16, saved in gen_arg_list *)

  end (* with *) ;
 end;
$PAGE procedure_call
(* PROCEDURE CALL compiles code for a call operator "node". *)

public procedure procedure_call (node: tuple);
 begin
  if node^.subr^.desc.base^.fortran_call
    then for_call (node)
    else pas_call (node, nil);
 end;
$PAGE scl_function_call
(* SCL FUNCTION CALL compiles code for a func_call_op "node" whose result is
   of size two words or less, e.g. scalars and reals. *)

public procedure scl_function_call (node: expr);
 var vdesc: val_desc;
 begin

  (* A Pascal subroutine leaves its return value on the stack in a location which
     is just beyond the end of the stack frame of the calling subroutine.  This
     location is termed the 'return value slot'.  The contents of the slot must
     be specially managed to insure that the value is not destroyed before it
     can be used, or moved;  this is the purpose of the "tag_rv" call. *)

  if not node^.subr^.desc.base^.fortran_call then begin
    pas_call (node, nil);
    if node^.subr^.opcode <> desc_ref then
      tag_rv (node, nil)
    else if node^.subr^.operand[1]^.item.class = external_sc then
      tag_rv (node, nil)
    else
      tag_rv (node, node^.subr^.operand[1]^.item.sym_name^.init_value.blkp);
  end

  (* A fortran subroutine leaves its return value in register 0 (or 0-1 pair).
     As we assume that these registers are unused, the value is immediately
     stored in a temporary location. *)

  else begin
    for_call (node);
    vdesc := get_value_desc (node);     (* assign a temporary for the result *)
    with vdesc^ do begin
      loc := temp_reference;
      loc.reloc.relval := vdesc;
      vdesc^.allocate := true;          (* location must exist *)
      vdesc^.free_at_stmt := true;      (* short lifetime *)
      def_temp (vdesc);
      if size = 1       (* store the value *)
        then gen_rm (movem, 0, loc)
        else gen_rm (dmovem, 0, loc);
    end;
  end;
 end.
    