$TITLE P10EXP - expression evaluation and register manipulation
$LENGTH 42
module p10exp;
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE ptmcon.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$INCLUDE pasifu.inc
$INCLUDE p10cgu.inc
$INCLUDE p10opc.inc
$INCLUDE p10cmp.inc
$INCLUDE p10cll.inc
$INCLUDE p10gen.inc
$INCLUDE p10dsc.inc
$INCLUDE pasmth.inc
$PAGE global data and forward declarations

type
  reg_set_vector = array [0..*] of set_of_registers;

public var
  regdesc: array[registers] of reg_desc;        (* descriptors for all registers *)
  rv_value: expr;                       (* value current in return value slot *)
  regs_used: set_of_registers;          (* registers altered by this routine *)
  regs_allocated: set_of_registers;     (* registers used but not altered; this and above differ
                                           only for incoming parameter registers *)

  reg_use_by_block: ^ reg_set_vector;
  vdesc_list: val_desc;                 (* list of currently active value descriptors *) 
  temp_id_counter: id_range;            (* for assigning names to internal temps *)


(* The register state list is a list of reg_state's at entry to each block
   computed from the output states of all predecessor nodes not including
   those with greater basic block numbers. *)

type rs_list_type = array [0..*] of reg_state_ptr;
var rs_list: ^ rs_list_type;


public procedure do_move (reg: registers; mem: addr_desc;
                          alignment: data_alignment; p: bit_range); forward;
public procedure prepare (treg: reg_selector; exp: expr); forward;
public procedure prep_direct (treg: reg_selector; exp: expr); forward;
procedure prepare_mem_ref (treg: reg_selector; exp: expr) options special(coercions); forward;
public function load (treg: reg_selector; exp: expr; p: bit_range): registers; forward;
public procedure load_reg (treg: registers; exp: expr; p: bit_range); forward;
public function fetch (exp: expr; p: bit_range): addr_desc; forward;
public function fetch_direct (exp: expr; p: bit_range): addr_desc; forward;
public function argument (arg: expr): addr_desc; forward;
$PAGE precision
(* PRECISION returns the bit size of an arithmetic value, i.e. integer (or other
   scalar) or real. *)

function precision (exp: expr): bit_range;
 var answer: bit_range;
 begin
  case exp^.desc.kind of
    reals:
      if exp^.desc.precision > srealprec
	then answer := 72
	else answer := 36;
    ints, bools, chars, scalars, pointers, files:
      answer := exp^.desc.int_prec;
    sets:
      if exp^.desc.set_length <= 36 then
        answer := 36
      else if exp^.desc.set_length <= 72 then
        answer := 72
      else
        assert (false);
    arrays, records:
      with exp^.desc.base^ do begin
	assert (not flexible);
        if base_size <= 36 then
          answer := 36
        else if base_size <= 72 then
          answer := 72
        else
          assert (false);
      end;
    unknown_type: (* must be a condition cell reference *)
      precision := 36;
    procs, funcs:
      precision := 36;
    others:
      assert (false)
  end (* case exp^.desc.kind *);
  precision := answer;

 end;
$PAGE alignment

(* ALIGNMENT returns the data alignment type for an expression ("exp"). *)

function alignment (exp: expr): data_alignment;
 begin
  case exp^.desc.kind of
    ints, bools, chars, scalars, pointers, files:
      if exp^.desc.signed
        then alignment := signed_value
        else alignment := unsigned_value;
    reals:
      alignment := right_aligned;               (* always occupies full words *)
    arrays, records:
      alignment := left_unpadded;
    others:
      alignment := left_aligned
  end;
 end;
$PAGE usage
(* USAGE returns the apparent number of remaining uses to an expr.  If the
   expression is a reference to a variable, then an extra use is added if the
   variable is live on exit from the block. *)

type usage_cnt = 0..4095;

public function usage (e: expr): usage_cnt  options special(coercions);
 var vdesc: val_desc; 
 begin
  vdesc := val_desc (e^.result);  (* coerce *)
  if vdesc <> nil then (* get status from descriptor *)
    usage := vdesc^.uses
  else if e^.ref_fre <> 0 then (* descriptor has been released *)
    usage := 0
  else (* not yet ref'ed, all uses remain *)
    usage := e^.usage_count;
end;
$PAGE reg_init, reg_term
(* REG INIT creates the register descriptor array *)

public procedure reg_init;

 var
   reg: registers;
   rd: reg_desc;
   i: index_range;

 begin
  for reg := maximum (registers) downto minimum (registers) do begin
    new (rd);
    rd^.loc := reg_addr_desc;
    rd^.loc.offset := reg;
    regdesc [reg] := rd;
  end;
  new (reg_use_by_block, blk_number);
  for i := 1 to blk_number do
    reg_use_by_block^[i] := [];
  rs_list := nil;
  vdesc_list := nil;
 end;


(* REG TERM cleans up storage used by the register allocator.  It is called at
   the end of code generation. *)

public procedure reg_term;
 var reg: registers;
 begin
  for reg := minimum (registers) to maximum (registers)
    do dispose (regdesc[reg]);
  dispose (reg_use_by_block);
  if rs_list <> nil then dispose (rs_list);
 end;
$PAGE reset_reg_descs, reg_reset
(* RESET REG DESCS initializes all registers descriptors to a free state. *)

procedure reset_reg_descs;
 var reg: registers;
 begin
  for reg := minimum (registers) to maximum (registers) do begin
    with regdesc[reg]^ do begin
      group_size := 1;
      value_size := 1;
      contents := nil;
      free := true;
      locked := false;
      succ_locked := false;
      signed := false;
    end;
  end;
 end;


(* REG RESET initializes the register allocator at the start of a block. *)

public procedure reg_reset ( n_blocks: index_range );
 var i: index_range;
 begin
  reset_reg_descs;
  temp_id_counter := 1;
  rv_value := nil;

  regs_allocated := [];
  regs_used := [];

  if rs_list <> nil then dispose (rs_list);
  new (rs_list, n_blocks);
  for i := 1 to n_blocks do rs_list^[i] := nil;
 end;
$PAGE temp_reset
(* TEMP RESET is called at the end of a routine to dispose of any value descriptors
   which are still on the list.  If any of these descriptors have a non-zero
   usage count, a warning message is printed. *)

public procedure temp_reset;
 var vdesc: val_desc; 
     vd_errors: int_type;
 begin
  vd_errors := 0;
  while vdesc_list <> nil do begin
    with vdesc_list^ do begin
      if uses <> 0 then begin
        vd_errors := vd_errors + 1;
        write (tty, 'VDESC ');
        if internal
          then write (tty, '%', temp_id:4)
          else write (tty, '$', op^.nodeid:4);
        writeln (tty, ':', uses:3, ' USES');
      end;
      vdesc := next;
    end;
    dispose (vdesc_list);
    vdesc_list := vdesc;
  end;
  if vd_errors <> 0 then begin
    writeln (tty, '?', vd_errors:4, ' unfreed value descriptors in block ',
                  cur_block^.number:3);
    assert (false)
  end;
 end;
$PAGE store_reg
(* STORE REG stores the contents of a register ("reg") in memory ("mem"). *)

PUBLIC PROCEDURE store_reg(reg: registers; mem: addr_desc);
VAR vdesc: val_desc;
BEGIN
  IF regdesc[reg]^.value_size = 2 THEN gen_rm(dmovem, reg, mem)
  ELSE IF regdesc[reg]^.loc.mode = rhw THEN BEGIN
    IF regdesc[reg]^.signed THEN gen_rm(hrrem, reg, mem)
    ELSE gen_rm(hrrzm, reg, mem)
  END
  ELSE gen_rm(movem, reg, mem);
  vdesc := regdesc[reg]^.contents;
  IF vdesc <> NIL THEN WITH vdesc^ DO BEGIN
    IF loc.reloc.kind = temp_sc THEN is_dirty := FALSE
  END
END;
$PAGE save_reg
(* SAVE REG insures that the contents of a register can be retrieved from
   memory at some later point in the program.  If the value is not an
   (immediate) constant, and if it has not been previously stored, then value
   is assigned a temporary location and a non-mandatory store emitted.
   Only need save the value if it does not have a memory address from which
   we can refetch the value.  If it is necessary to save the value, then it
   will be assigned a memory address (a temporary location) suppressing
   any further saves unless the value becomes dirty. *)

PROCEDURE save_reg(sreg: registers);
VAR vdesc: val_desc; 
BEGIN
  vdesc := regdesc[sreg]^.contents;
  IF vdesc <> NIL THEN WITH vdesc^ DO BEGIN
    IF loc.reloc.kind = unallocated THEN BEGIN
      loc := temp_reference;
      IF (not internal) ANDIF (op^.opcode = display_op) THEN loc.mode := rhw;
      loc.reloc.relval := vdesc; def_temp(vdesc);
      store_reg(sreg, loc)
    END
    ELSE IF (loc.reloc.kind = temp_sc) AND is_dirty THEN store_reg(sreg, loc)
  END
END;
$PAGE free_reg
(* FREE REG releases a register group for future allocation.  The registers to
   free are derived from "reg" and the group size of that register.  Here only
   the fields of the register descriptor relating to allocation are reset.  The
   fields describing the value present are not reset, as the value may remain
   in use momentarily - for example, in a move of the register to another.  *)

public procedure free_reg (reg: reg_desc);
 var r: registers;
 begin
  r := reg^.loc.offset;
  with regdesc[r]^ do begin
    free := true;
    if contents <> nil then begin
      contents^.reg := nil;
      contents := nil;
    end;
    if group_size = 2 then begin
      with regdesc[r+1]^ do begin
        free := true;
        if contents <> nil then begin
          contents^.reg := nil;
          contents := nil;
        end;
        group_size := 1;
      end;
    end;
    group_size := 1;
  end;
 end;
$PAGE clr_reg, kill_regs
(* CLR REG insures that a register is free.  If there is a value in the register,
   then the value is forcibly evicted. *)

public procedure clr_reg (reg: registers);
 begin
  if not regdesc[reg]^.free then begin
    save_reg (reg);
    free_reg (regdesc[reg]);
  end;
 end;



(* KILL REGS clears all registers designated by a set ("regs"), and marks those
   registers as being used by the current block. *)

public procedure kill_regs ( regs: set_of_registers );
 var reg: registers;
 begin
  for reg := 2 to 14b do begin
    if reg in regs then clr_reg (reg);
  end;
  regs_used := regs_used + regs;
 end;
$PAGE reg_is_free
(* REG IS FREE determines whether a register "reg" of specified size
   "regsize" is free and unlocked. *)

function reg_is_free (reg: registers; regsize: bit_range): boolean;
 begin
  reg_is_free := (regdesc[reg]^.free and not regdesc[reg]^.locked) and
    ((regsize <= 36) or (regdesc[reg+1]^.free and not regdesc[reg+1]^.locked));
 end;
$PAGE reg_is_available
(* REG IS AVAILABLE returns a flag which indicates if a register ("reg") of
   suitable size ("regsize") is currently available.  No register past R14
   is ever available (R15 is reserved for the PDA pointer, R16 for the
   frame pointer, and R17 for the stack pointer).  Other registers are
   available if they are free (see above). *)

function reg_is_available (reg: registers; regsize: bit_range): boolean;
 begin
  if (reg >= #o15) or ((reg = #o14) and (regsize > 36)) then
    reg_is_available := false
  else
    reg_is_available := reg_is_free (reg, regsize);
 end;
$PAGE lock, unlock
(* LOCK sets the lock bit for a register.  This prohibits the register from
   being allocated even if free, or from being displaced (e.g. saved) in
   order to allocate another value to the same register if not free. *)

public procedure lock (reg: registers);
 begin
  regdesc[reg]^.locked := true;
  if regdesc[reg]^.group_size = 2 then begin
    regdesc[reg+1]^.locked := true;
    regdesc[reg]^.succ_locked := true;
  end;
 end;



(* UNLOCK simply turns the locked bit off. *)

public procedure unlock (reg: registers);
 begin
  regdesc[reg]^.locked := false;
  if regdesc[reg]^.succ_locked then begin
    regdesc[reg+1]^.locked := false;
    regdesc[reg]^.succ_locked := false;
  end;
 end;
$PAGE mem_lock, mem_unlock
(* MEM LOCK locks a register which is part of some memory address. *)

public procedure mem_lock (mem: addr_desc);
 begin
  if mem.reloc.kind = register_sc
    then lock (mem.offset)
  else if mem.index <> noreg
    then lock (mem.index);
 end;



(* MEM UNLOCK unlocks a register which is part of some memory address *)

public procedure mem_unlock (mem: addr_desc);
 begin
  if mem.reloc.kind = register_sc
    then unlock (mem.offset)
  else if mem.index <> noreg
    then unlock (mem.index);
 end;
$PAGE clear
(* CLEAR zeros a specified register (pair). *)

public procedure clear (reg: registers);
 begin
  if regdesc[reg]^.group_size = 1
    then gen_ri (setz, reg, 0)
    else gen_rr (setzb, reg, reg+1);
 end;
$PAGE select_single_reg_for_free
(* SELECT SINGLE REGISTER FOR FREE chooses a single word register to store to
   make room for some other register allocation.

   The rules for selecting a register to save are:  (1) Never displaced a locked
   register regardless of whether or not it is free (see "lock/unlock").  By
   convention, there must be sufficient unlocked registers at the time a get_reg
   call is made to make the allocation.  (2) Of the remaining registers, select
   the one with the smallest usage count on the assumption that it is the least
   likely to be used in the future.  Ties are resolved by bias toward the preferred
   register ("treg").

   It is assumed that when this routine is called, there are no single free
   registers, i.e. that all registers are in use.  Thus we need not check
   for free registers while scanning the array of registers. *)

function select_single_reg_for_free (treg: reg_selector): registers;
 
 var
   r: registers;
   min_uses: usage_cnt;

 begin
  min_uses := maximum (usage_cnt);
  r := 2;
  while r <= 14b do begin               (* examine each group of registers *)
    with regdesc[r]^ do begin
      if not locked then begin  (* don't alloc a locked reg *)
        if (contents^.uses < min_uses) orif     (* take one with least uses *)
           ((contents^.uses = min_uses) and (r = treg)) (* or in case of tie the preferred *)
          then begin
            min_uses := contents^.uses;
            select_single_reg_for_free := r;
          end;
      end;
      r := r + group_size;              (* ignore 2nd word of pairs *)
    end;
  end (* while *) ;
  assert (min_uses <> maximum (usage_cnt));
 end;
$PAGE select_double_reg_for_free
(* SELECT DOUBLE REG FOR FREE chooses a double word register to save in order to
   make room for another allocation.

   The rules applied are the same as for the single register case, except that
   we must account for the existent of noncontiquous, single free registers, and
   handle size related problems. *)

function select_double_reg_for_free (treg: reg_selector): registers;

 var
   min_uses: usage_cnt; (* minimum usage count encountered *)
   r: registers;        (* scanning cursor *)
   uses: usage_cnt;     (* apparent usage cnt for reg [r] *)
   pair_uses: usage_cnt;        (* usage of pair [r-1, r] *)
   last_uses: usage_cnt;                (* usage count of reg [r-1] *)
   use_last: boolean;                   (* permits formation of group with last reg *)

 begin
  min_uses := maximum (usage_cnt);
  use_last := false;

  r := 2;
  while r <= 14b do begin               (* scan the register groups *)
    with regdesc[r]^ do begin
      if locked                         (* don't alloc locked reg *)
        then use_last := false

      else if group_size = 1 then begin
        if free                         (* derive apparent usage *)
          then uses := 0
          else uses := contents^.uses;
        if use_last then begin          (* pair with previous register *)
          pair_uses := uses + last_uses;
          if (pair_uses < min_uses) or ((pair_uses = min_uses) and ((r-1) = treg))
            then begin                  (* pair is candidate *)
              select_double_reg_for_free := r - 1 ;
              min_uses := pair_uses;
            end;
        end;
        use_last := true;               (* try to pair with next reg *)
        last_uses := uses;
      end

      else begin        (* double reg, alloc by itself *)
        uses := contents^.uses;
        if (uses < min_uses) or ((uses = min_uses) and (r = treg))
          then begin                    (* have a candidate *)
            select_double_reg_for_free := r;
            min_uses := uses;
          end;
        use_last := false;              (* don't try to pair a double reg *)
      end;

      r := r + group_size;
    end (* with *) ;
  end (* while *) ;
  assert (min_uses <> maximum (usage_cnt));
 end;
$PAGE get_reg
(* GET REG allocates a register or register pair (depending on the value of "p");
   a register preference may be specified ("treg"), and this routine will attempt
   to allocate the desired register.  The register actually allocated is returned. *)

public function get_reg (treg: reg_selector; p: bit_range): registers;

 label 100;                             (* go here with reg = register to allocate *)
 var   reg, last_avl: registers;

 begin

  (* If the caller has specified a preferred register, check if it can be
     allocated.  If so, directly allocate that register. *)

  if (treg <> anyreg) and (treg < 16) then begin
    if reg_is_free (treg, p) then begin
      reg := treg;
      goto 100;                         (* <---- branch to allocation point *)
    end;
  end;

  (* Search the list of registers available for allocation to user variables for
     a free register (pair).  If found, return after marking the registers as
     in use.  *)

  if p <= 36
    then last_avl := 14b
    else last_avl := 13b;
  for reg := 2 to last_avl do begin
    if reg_is_free (reg, p) then goto 100;      (* <---- branch to allocation point *)
  end;

  (* No register available, must free up a register in order to make space for
     this allocation. *)

  if p <= 36
    then begin
      reg lect_single_reg_for_free (treg);
      clr_reg (reg);
    end
    else begin
      reg := select_double_reg_for_free (treg);
      clr_reg (reg);
      clr_reg (reg + 1);
    end;

100:

  (* When we reach here, "reg" contains the number of the register to allocate.
     Mark the register (pair) as allocated, and update the register use set. *)

  regdesc[reg]^.free := false;
  regs_used := regs_used + [reg];
  if p > 36 then begin
    regdesc[reg]^.group_size := 2;
    regdesc[reg+1]^.free := false;
    regs_used := regs_used + [reg+1];
  end;
  regs_allocated := regs_allocated + regs_used;
  get_reg := reg;
 end;
$PAGE is_register
(* IS REGISTER returns a flag indicating whether or not an address ("maddr")
   designates a register. *)

public function is_register (maddr: addr_desc): boolean;
 begin
  is_register := (maddr.reloc.kind = register_sc) andif
                 (maddr.mode <> byte) andif
                 (not ( maddr.immediate or maddr.indirect ) ) andif
                 (maddr.offset > 1);            (* if value in 0 or 1, copy it *)
 end;
$PAGE get_temp
(* GET TEMP generates a new value descriptor for an internal temporary of a
   designated size ("temp_size"), and notes the introduction of the temporary
   by emitting a definition of it. *)

public function get_temp (temp_size: unit_range): val_desc; 
 begin
  new (get_temp);
  with get_temp^ do begin
    next := vdesc_list;                 (* chain to list of value descriptors *)
    prev := nil;
    if vdesc_list <> nil
      then vdesc_list^.prev := get_temp;
    vdesc_list := get_temp;
    size := temp_size;
    reg := nil;
    loc_index := nil;
    loc := temp_reference;
    loc.reloc.relval := get_temp;
    allocate := true;
    global := false;
    generated := false;
    uses := 0;                  (* no information about its lifetime *)
    free_at_stmt := true;
    internal := true;
    temp_id := temp_id_counter; (* assign identifier for listing *)
    temp_id_counter := temp_id_counter + 1;
  end;
  def_temp (get_temp);
 end;
$PAGE get_value_desc
(* GET VALUE DESC returns the value descriptor for an expression node ("exp").
   If one alreadly exists, it is returned;  otherwise, one is created and
   initialized with "generated" false to indicate that the node has not yet
   been processed.  If the "ref_fre" field of the expression is non-zero, it
   indicates that the expression has had a value descriptor previously, and that
   all uses of it have already been accounted for. *)

public function get_value_desc (exp: expr): val_desc  options special(coercions); 
 begin
  if exp^.result <> nil then begin      (* alreadly has one *)
    get_value_desc := val_desc (exp^.result);  (* coerce *)
    return;
  end

  else begin                            (* must create one *)
    assert (exp^.ref_fre = 0);     (* expression already processed once *)
    new (get_value_desc);
    with get_value_desc^ do begin
      next := vdesc_list;       (* chain to list *)
      prev := nil;
      if vdesc_list <> nil
        then vdesc_list^.prev := get_value_desc;
      vdesc_list := get_value_desc;
      reg := nil;
      size := word_size (exp);
      loc_index := nil;
      loc := null_location;
      uses := exp^.usage_count;
      generated := false;
      allocate := false;
      global := false;
      free_at_stmt := false;
      internal := false;                (* record that value desc is for expr *)
      op := exp;
    end;
    exp^.result := expr (get_value_desc);
    return;
  end
 end;
$PAGE free
(* FREE marks the end of a value use.  The usage count of the value is decremented,
   and when it reaches zero, the value descriptor and any resources (registers,
   etc.) tied to the value are released.  Note that if the value is a memory
   operand which involves a register in the address calculation then the usage
   count of the index value is tied to the memory value;  therefore, the index
   is also freed.   If the value descriptor must be examined during temporary and
    global register allocation, then the descriptor is not deleted.  The "result"
   field of the expression is reset to nil, and the "ref_fre" field is set to 1.
   This signifies that the expression has been prepared and freed, and that any
   subsequent attempt to prepare it would be erroneous. *)

public procedure free (exp: expr);
 var vdesc: val_desc; 
 begin
  vdesc := get_value_desc (exp);        (* create a desc for this even if not yet generated as
                                           there may remain non-implicit uses *)
  with vdesc^ do begin
    if loc_index <> nil then free (loc_index^.op);
    assert (uses <> 0);
    uses := uses - 1;
    if uses = 0 then begin
      if reg <> nil then free_reg (reg);
      if not (allocate or global or (loc.reloc.kind = temp_sc)) then begin
        (******* free temp ********)
        if prev = nil                   (* slice out from chain *)
          then vdesc_list := next
          else prev^.next := next;
        if next <> nil
          then next^.prev := prev;
        dispose (vdesc);
        exp^.result := nil;
        exp^.ref_fre := 1;
        if exp = rv_value                       (* if this is in the return value slot *)
          then rv_value := nil;         (* ... then mark it as free *)
      end;
    end;
  end;
 end;
$PAGE detach_expr
(* DETACH EXPR disassociates an index or base expression (the "loc_index") from the
   value descriptor of a referencing expression.  It is used when ref is no longer
   to be referenced with a memory address involving the loc_index.  It compensates
   for the adjusted usage count set up by "attach" and frees the value.  Note that
   it is assumed that the loc_index is non-nil. *)

procedure detach_expr (ref: val_desc); 
 var idx: val_desc; 
 begin
  with ref^ do begin
    idx := loc_index;           (* adjust usage counts of index's indexes *)
    while idx <> nil do begin
      idx^.uses := idx^.uses - uses + 1;
      idx := idx^.loc_index;
    end;
    free (loc_index^.op);                       (* account for reference thru "ref" *)
    loc_index := nil;
  end (* with *) ;
 end;
$PAGE attach
(* ATTACH associates the value descriptor of an index or base expression ("aux")
   with the value descriptor of a referencing expression ("ref").  Since aux must
   last until the referencing expression is used, and since it may be used as
   many times as the referencing expression, the usage count of aux is increased
   by the usage count of ref.  The two usage counts are then decremented in concert
   until the last reference, or until the association is broken by a call to "detach_expr". *)

procedure attach (ref: val_desc; aux: val_desc); 
 var idx: val_desc; 
 begin
  with ref^ do begin
    if aux = loc_index then return;             (* this value is already attached; this check
                                                   avoids problems with multiple calls to prepare *)
    loc_index := aux;
    idx := aux;         (* adjust usage counts of index exprs *)
    while idx <> nil do begin
      idx^.uses := idx^.uses + uses - 1;
      idx := idx^.loc_index;
    end;
  end (* with *) ;
 end;
$PAGE consume
(* CONSUME disassociates a value ("exp") from any registers (including index
   registers) currently associated with it.  This is used to "prefree" registers
   for allocation in situations where destructive loads are desired.  As this
   can leave the value without any location, one of two constraints must hold for
   this routine to be used:  (1) a new location is to be set for the value, or
   (2) there are no uses of the value which remain unaccounted for, i.e. no more
   "locate"s will be performed on the value. *)

public procedure consume (exp: expr)  options special(coercions);
 var vdesc: val_desc; 
 begin
  vdesc := val_desc (exp^.result);  (* coerce *)
  with vdesc^ do begin
    if reg <> nil then free_reg (reg);
    if (loc.mode = byte) or (* if loc_index is byte pointer *)
       not (loc.index in [0, sp, sb]) then begin (* or not easy to get *)
      if loc_index <> nil then detach_expr (vdesc);
      loc := null_location;
    end;
  end;
 end;
$PAGE tag_loc
(* TAG LOC records the fact that an expr ("exp") is available at in a particular
   memory location ("mloc").  An expression ("aux_index") involved in deriving
   mloc should be specified in order to account uses of the index expression.  This
   routine is used for mem_ref's, immed_ref's and certain cases of addr_ref's
   where the address can be a memory operand. *)

public procedure tag_loc (mloc: addr_desc; exp: expr; aux_index: expr)  options special(coercions);
 var vdesc: val_desc; 
 begin
  vdesc := val_desc (exp^.result);               (* get its descriptor *)  (* coerce *)
  with vdesc^ do begin
    loc := mloc;                                (* record the address *)
    loc.has_sign := alignment (exp) = signed_value;
    if aux_index <> nil (* remember index or base expression *)
      then attach (vdesc, val_desc (aux_index^.result));  (* coerce *)
  end (* with *) ;
 end;
$PAGE tag_reg
(* TAG REG records the fact that an expr ("exp") has been loaded in a particular
   register ("reg") by linking the register and value descriptors. *)

public procedure tag_reg (treg: registers; exp: expr);
 var vdesc: val_desc; 
 begin
  vdesc := get_value_desc (exp);
  with vdesc^ do begin

    (* If the value is already associated with the specified register, then we
       can just return.  If the value is associated with some other register,
       then that register will no longer be referenced, so we can free it. *)

    if reg <> nil then begin
      if reg^.loc.offset = treg
        then return (* <---- reg already tagged *)
        else free_reg (reg);
    end;

    (* Set the linkage between the value descriptor and the register descriptor. *)

    with regdesc[treg]^ do begin
      contents := vdesc;
      value_size := vdesc^.size;
      loc.mode := fw;
      signed := (alignment (exp) = signed_value);
    end;
    reg := regdesc[treg];

    (* If we just loaded the value of a variable component, then we must break
       the association between the value and the memory location to avoid future
       references to the possibly modified location.  This also discards any
       registers or temporaries assigned to an indexing value. *)

    if (loc.mode = byte) or not (loc.index in [0, sp, sb]) then begin
      if loc_index <> nil then detach_expr (vdesc);
      loc := null_location;
    end;

    (* If the expression tagged is the value currently in the return value slot,
       then the slot is marked as empty since the value is now available elsewhere. *)

   if rv_value = exp then begin
     rv_value := nil;
     loc := null_location;
   end;

  end (* with *) ;
 end;
$PAGE tag_rv
(* TAG RV records that an expression ("exp") is the return value of a function.
   For the PDP-10, quick functions place their result at a location relative
   to the current stack frame.  Non-quick functions place their result at a
   location relative to their own stack frame, which amounts to a location
   relative to the end of the current stack frame.  In either case, such a
   result might be wiped out by any other call.  This routine sets the "loc"
   of the value descriptor for "exp" to address the return value location for
   block "func" ("func" may be nil for an external or subr-var function call),
   and records that there a function return value is in use.  The routine
   "clr_rv" must be called before any operation which may perform a call or
   alter the length of the current stack frame.

   The routines "free" and "tag_loc" aid in keeping track of the contents of
   the return value slot, by marking it as unused when the value has no remaining
   uses or the value is moved elsewhere. *)


public procedure tag_rv (exp: expr; func: blk)  options special(coercions);
 const
    rv_reference: addr_desc := (fw, false, false, false, sb, 5, 0, 0, (absolute_sc));
    qrv_reference: addr_desc := (fw, false, false, false, sp, 0, 0, 0, (local_sc, nil));
 var vdesc: val_desc; 
 begin
  vdesc := val_desc (exp^.result);  (* coerce *)
  if (func = nil) orif (func^.owner = func) then (* non-quick *)
    vdesc^.loc := rv_reference
  else begin (* quick *)
    vdesc^.loc := qrv_reference;
    vdesc^.loc.reloc.relsym := func^.return_sym;
  end;
  rv_value := exp;
 end;
$PAGE clr_rv
(* CLR RV insures that the value (if any) in the return value location is
   available in some other place. *)

public procedure clr_rv;
 var reg: registers;
 begin
  if rv_value = nil then return;        (* no current return value *)
  case rv_value^.desc.kind of
    ints, bools, chars, scalars, pointers, files:
      reg := load (anyreg, rv_value, 36);
    reals:
      if rv_value^.desc.precision > srealprec
        then reg := load (anyreg, rv_value, 72)
        else reg := load (anyreg, rv_value, 36)
  end;

  (* The load calls will "tag_reg" the result, which will set rv_value to
     nil, and set the location of the value to null. *)

 end;
$PAGE rs_save
(* RS SAVE saves the current register state.  Summary information is copied from
   the register descriptors into "rs". *)

public procedure rs_save ( var rs: reg_state );
 var reg: registers;
 begin
  for reg := minimum (registers) to maximum (registers) do begin
    if regdesc[reg]^.contents = nil
      then begin
        rs[reg].contents := nil;
        rs[reg].size := 1;
      end
      else begin
        rs[reg].contents := regdesc[reg]^.contents^.op;
        rs[reg].size := regdesc[reg]^.group_size;
      end;
  end;
 end;
$PAGE rs_load
(* RS LOAD loads a previously saved register state ("rs").  The actions taken
   here parallel the functions of get_reg and tag_reg -- with the important
   difference that registers marked here as being loaded are not added to the
   reg_used set.  That will have been done at the time the value was initially
   loaded.  The point is to avoid marking unaltered parameter registers as used. *)

procedure rs_load ( rs: reg_state )  options special(coercions);

 var
   reg, r: registers;
   vdesc: val_desc; 
   exp: expr;

begin
  reset_reg_descs;                      (* mark all as free *)
  reg := minimum (registers);
  while reg <= maximum (registers) do begin
    if rs[reg].contents <> nil then begin
      with regdesc[reg]^ do begin
        group_size := rs[reg].size;     (* mark reg as in use *)
        free := false;
        if group_size = 2       (* tag the second register *)
          then regdesc[reg+1]^.free := false;
        exp := rs[reg].contents;        (* link the register and value descriptors *)
        vdesc := val_desc (exp^.result);  (* coerce *)
        contents := vdesc;
        vdesc^.reg := regdesc[reg];
        value_size := vdesc^.size;      (* record attributes of value in reg *)
        signed := (alignment (exp) = signed_value);
	if exp^.opcode = display_op
	  then loc.mode := rhw
	  else loc.mode := fw;
      end;
    end;
    reg := reg + rs[reg].size;
  end;
 end;
$PAGE merge_states, bb_merge
(* MERGE STATES merges one register state ("rs2") into another ("rs1");  only
   those entries which are the same are preserved; otherwise the entries are
   marked as free. *)

procedure merge_states ( var rs1: reg_state; rs2: reg_state );
 var reg: registers;
 begin
  for reg := minimum (registers) to maximum (registers) do begin
    if rs1[reg].contents <> rs2[reg].contents then begin
      rs1[reg].contents := nil;
      rs1[reg].size := 1;
    end;
  end;
 end;



(* BB MERGE folds a specified register state ("rs") into the entry state for
   a designated block. *)

public procedure bb_merge ( bb: tuple; rs: reg_state );
 var rsp: reg_state_ptr;
 begin
  if rs_list^[bb^.block_order_no] = nil
    then begin                  (* first state, init to "rs" *)
      new (rsp);
      rsp^ := rs;
      rs_list^[bb^.block_order_no] := rsp;
    end
    else merge_states (rs_list^[bb^.block_order_no]^, rs);
 end;
$PAGE bb_end
(* BB END is called just before exiting a basic block.  It performs four actions:
   (1) It insures that all values generated in the block and live on exit to the
   block have been saved.  This guarentees that there is a store in a block which
   dominates all successive uses of the value.  (2) It records the exit register
   state.  The state is folded in to the entry register state of successor blocks
   except those which are loop headers (see bb_start).  (3) It releases stack
   space allocated to any dynamic temporaries generated by the last statement in
   the basic block.  (4) Marks the value descriptors of live on exit values as
   "global" to prevent their deletion on last use.  Otherwise, the node might be
   deleted before all blocks the value reaches (through entries in reg_states)
   are encountered.

   WARNING:  This routine may call "prepare" to process expressions which
   are exported from this block.  Therefore, any registers or memory locations
   which may still be necessary for end-of-block processing (e.g., jump test
   expressions) must be locked before the call to "bb_end". *)

public procedure bb_end  options special(coercions);

 var
   scan: tuple;
   cur_rs: reg_state;
   jmp, lab: tuple;
   vdesc: val_desc; 
   mem: addr_desc;
   r: registers;

 begin

  (* There may be expressions in this basic block which are never used in this
     block, but are used in later, dominated blocks.  Such expressions must be
     evaluated now.  We may recognize unused expressions by the fact that the
     "result" field (i.e. the value descriptor pointer) is nil, or the value
     descriptor is not marked "generated", and that the "ref_fre" field is
     zero.  Ref_fre is set when a tuple is freed for the last time. *)

  if cur_bb = t_chain^.last_label
    then scan := t_chain^.final_tuple^.prev
    else scan := cur_bb^.downward_thread^.prev;
  while (first_jump_op <= scan^.opcode) and (scan^.opcode <= last_jump_op) do
    scan := scan^.prev; (* find the last true tuple of the block *)
  while scan <> cur_bb do begin
    if ( (scan^.opcode >= first_expr) and (scan^.opcode <= last_expr) and
         (scan^.opcode <> set_op) and (scan^.opcode <> desc_ref) and
	 (scan^.desc.kind <> strings) and
         (scan^.opcode <> io_fix_str_op) and (scan^.opcode <> io_var_str_op) ) andif
       ( scan^.ref_fre = 0 ) then begin
      vdesc := val_desc (scan^.result);  (* coerce *)
      if (vdesc = nil) orif not vdesc^.generated then begin
        prepare (anyreg, scan);
        r := load (anyreg, scan, precision (scan));
      end;
    end;
    scan := scan^.prev;
  end;

  (* preserve return value slot, just in case *)

  clr_rv;

  (* find any live, prepared tuples which aren't in registers and aren't
     easily accessible, and load them now *)

  vdesc := vdesc_list;
  while vdesc <> nil do begin
    with vdesc^ do begin
      if (not internal) and (reg = nil) and not (loc.index in [0, sp, sb]) then begin
        prepare_mem_ref (anyreg, op); (* there may be a register involved *)
        if reg = nil then begin
          mem := loc;
          consume (op);
          r := get_reg (anyreg, size * 36);
          do_move (r, mem, alignment (op), size * 36);
          tag_reg (r, op);
        end;
      end;
      vdesc := next;
    end;
  end;

  rs_save (cur_rs);                     (* get summary of current register state *)

  jmp := cur_bb^.outward_jumps;         (* scan list of successors *)
  while (jmp <> nil) andif ((jump_op <= jmp^.opcode) and (jmp^.opcode <= case_jump_op)) do begin
    lab := jmp^.jump_to;
    if lab^.block_order_no > cur_bb^.block_order_no
      then bb_merge (lab, cur_rs);
    jmp := jmp^.next;
  end;

  (* Process live-on-exit values:  mark them as global, and presave their values
     if they are only in registers. *)

  vdesc := vdesc_list;
  while vdesc <> nil do begin
    with vdesc^ do begin
      global := global or (uses <> 0);  (* set true only for *live* value nodes *)
      if reg <> nil then save_reg (reg^.loc.offset);
      vdesc := next;
    end;
  end;

  (* Release dynamically allocated temporaries. *)

  kill_temps;                   (* checks if any are present and frees them *)
 end;
$PAGE bb_start
(* BB START establishes the register state on entry to a basic block.  Note
   that there are two kinds of basic blocks:  blocks which are reached only
   by forward edges and blocks which are reached via one forward edge and
   one or more back edges.  The latter, loop headers, are always preceded by
   a preheader node (courtesy pasjmp) which receive forward edges intended
   for the loop header.

   To establish the register state of forward convergent blocks, the exit register
   state of all predecessors is merged.  This is done in bb_end as each predecessor
   is exited and depends on ordered traversal of the basic blocks.

   For the present, the register state on entry to a loop header is taken as
   empty.  This means that all registers must be cleared as a loop header is
   entered.  Code to do so will end up in the preheader block. *)

public procedure bb_start;
 var vdesc: val_desc; 
 begin

  (* Establish contents of registers on entry to block. *)

  vdesc := vdesc_list;  (* don't know where values are until rs_load *)
  while vdesc <> nil do begin
    vdesc^.reg := nil;
    vdesc := vdesc^.next;
  end;

  rs_load (rs_list^ [cur_bb^.block_order_no]^);
  dispose (rs_list^[cur_bb^.block_order_no]);   (* no longer needed *)
  rs_list^[cur_bb^.block_order_no] := nil;

  (* Check if this block is a loop header.  It suffices to examine the source
     of the first inward jump as in the case of a loop header, the last inward
     jump is the single forward edge. *)

  if (cur_bb^.inward_jumps <> nil) andif
     (cur_bb^.inward_jumps^.jump_from^.block_order_no >= cur_bb^.block_order_no)
    then kill_regs (regs_allocated * [0..14b]); (* don't kill the sp *)

 end;
$PAGE mark_as_allocated
(* MARK AS ALLOCATED is called with the value descriptor of an expression.  If
   the value descriptor indicates a reference to a temporary or a parameter
   symbol, then the referenced item will be marked to prevent instructions
   referring to it from being deleted during code cleanup. *)

procedure mark_as_allocated ( vdesc: val_desc ); 
 begin
  with vdesc^ do begin
    case loc.reloc.kind of
      temp_sc:
        loc.reloc.relval^.allocate := true;
      parameter_sc:
        if loc.reloc.relsym <> nil then
          loc.reloc.relsym^.allocated := true;
      others:
        (* no action *)
    end;
  end;
 end;
$PAGE locate
(* LOCATE determines the location where a particular value ("exp") can be found
   in register, temporary or memory space.  An addr_desc for the location is
   returned.  The descriptor will show "unallocated" if the location has not
   been previously established. *)

public function locate (exp: expr): addr_desc  options special(coercions);
 var vdesc: val_desc; 
 begin
  vdesc := val_desc (exp^.result);  (* coerce *)
  if vdesc = nil
    then locate := null_location
  else if vdesc^.reg <> nil
    then locate := vdesc^.reg^.loc

  (* If we must take a memory address, and that address involves an index or
     base expression, then we must make sure that the index is accessible before
     we return the address.  This is done by repreparing the address, which will
     load any values not directly available.   This has an important side effect
     on the function of "prep_direct", in that it insures that the indirect pointer
     will be loaded. *)

  else begin
    if vdesc^.loc_index <> nil                  (* there may be a register involved *)
      then prepare_mem_ref (anyreg, exp);
    locate := vdesc^.loc;
    mark_as_allocated (vdesc);
  end;
 end;
$PAGE left_shift, right_shift
(* LEFT SHIFT emits code to shift a register ("reg") or register pair (determined
   by the precision "p") left "n" bits. *)

procedure left_shift (reg: registers; p: bit_range; n: int_type);
 begin
  if n <> 0 then begin
    if p <= 36
      then gen_ri (lsh, reg, n)
      else gen_ri (lshc, reg, n);
  end;
 end;



(* RIGHT SHIFT emits code to shift a register ("reg") or register pair (determined
   by the precision "p") right "n" bits. *)

public procedure right_shift (reg: registers; p: bit_range; n: int_type);
 begin
  if n <> 0 then begin
    if p <= 36
      then gen_ri (lsh, reg, -n)
      else gen_ri (lshc, reg, -n);
  end;
 end;
$PAGE mask tables

type rmask_table = array[0..36] of machine_word;

const rmasks: rmask_table :=
     (  000000000000B,
        000000000001B,
        000000000003B,
        000000000007B,
        000000000017B,
        000000000037B,
        000000000077B,
        000000000177B,
        000000000377B,
        000000000777B,
        000000001777B,
        000000003777B,
        000000007777B,
        000000017777B,
        000000037777B,
        000000077777B,
        000000177777B,
        000000377777B,
        000000777777B,
        000001777777B,
        000003777777B,
        000007777777B,
        000017777777B,
        000037777777B,
        000077777777B,
        000177777777B,
        000377777777B,
        000777777777B,
        001777777777B,
        003777777777B,
        007777777777B,
        017777777777B,
        037777777777B,
        077777777777B,
        177777777777B,
        377777777777B,
        -1              );

type lmask_table = array[0..18] of integer;

const lmasks: lmask_table :=
     (  000000B,
        400000B,
        600000B,
        700000B,
        740000B,
        760000B,
        770000B,
        774000B,
        776000B,
        777000B,
        777400B,
        777600B,
        777700B,
        777740B,
        777760B,
        777770B,
        777774B,
        777776B,
        777777B  );
$PAGE left_mask
(* LEFT MASK masks a register (pair) zeroing all but the leftmost "n" bits.
   The precision ("p") denotes the number of left aligned bits which may be
   nonzero (i.e. any remaining bits are known to be zero);  a value of p less
   than 36 denotes a single register versus a pair.  *)

procedure left_mask (reg: registers; p: bit_range; n: bit_range);
 var word: pdp10word;
 begin
  if p <= n
    then return                         (* bits to zero are already zero *)

  else if n = 0 then begin                      (* zero register (pair) *)
    clear (reg);
  end

  else if p > 36 then begin             (* part of mask applies to all of one register *)
    if n <= 36
      then begin
        gen_ri (setz, reg+1, 0);
        left_mask (reg, 36, n);
      end
    else left_mask (reg+1, p-36, n-36);
  end

  else begin
    if (n >= 18)
      then gen_ri (andcmi, reg, rmasks[36-n])
    else if (p > 17)
      then begin        (* must zero low order bits too *)
        word.value := rmasks [36-n];
        gen (andcm, reg, 0, 0, gen_cword (word, setword));
      end
    else gen_ri (tlz, reg, rmasks[18-n]);
  end;
 end;
$PAGE do_move
(* DO MOVE moves the contents of a memory location ("mem"), with any "alignment",
   into a register or register pair ("reg") depending on the specified precision
   ("p"). *)

public procedure do_move (*reg: registers; mem: addr_desc;
                           alignment: data_alignment; p: bit_range*);
 begin
  if p = 0 then begin
    clear (reg);
    return
  end;
  case mem.mode of
    fw:     if p <= 18 then
	      if alignment = signed_value
		then gen_rm (move, reg, mem)
	      else if alignment = left_aligned
		then gen_rm (hllz, reg, mem)
	      else if alignment = left_unpadded
		then gen_rm (hll, reg, mem)
	      else gen_rm (hrrz, reg, mem)
	    else if p <= 36
              then gen_rm (move, reg, mem)
	    else gen_rm (dmove, reg, mem);

    lhw:    if (alignment = signed_value) and mem.has_sign
              then gen_rm (hlre, reg, mem)
            else if alignment = left_aligned
              then gen_rm (hllz, reg, mem)
            else if alignment = left_unpadded
              then gen_rm (hll, reg, mem)
            else gen_rm (hlrz, reg, mem);

    rhw:    if (alignment = signed_value) and mem.has_sign
              then gen_rm (hrre, reg, mem)
            else if alignment = left_aligned
              then gen_rm (hrlz, reg, mem)
            else if alignment = left_unpadded
              then gen_rm (hrl, reg, mem)
            else gen_rm (hrrz, reg, mem);

    byte:   begin
              gen_rm (ldb, reg, mem);           (* load right justified *)
              if alignment in [left_aligned, left_unpadded] then
                left_shift (reg, 36, 36 - mem.slice_size)
	      else if (alignment = signed_value) and mem.has_sign then begin
		assert (mem.slice_size = 18);
		gen_rx (hrrei, reg, reg)
	      end
            end;

    slice:  if alignment in [left_aligned, left_unpadded] then begin (* load, shift, and mask *)
	      gen_rm (move, reg, mem);
	      left_shift (reg, 36 - mem.slice_offset, mem.slice_offset);
	      if alignment = left_aligned then
		left_mask (reg, 36, mem.slice_size);
	    end
	    else begin
	      gen (ldb, reg, 0, 0, gen_bptr (mem));
	      if (alignment = signed_value) and mem.has_sign then begin
		assert (mem.slice_size = 18);
		gen_rx (hrrei, reg, reg)
	      end
	    end
  end;
 end;
$PAGE move_reg
(* MOVE REG moves the contents of one register to another.  Size conversion is
   performed (e.g. single to double) with left truncation.   Note that if the
   source register is a rhw, then the value is sign extended to prevent having
   to do the operation in the future.  It is assumed that "src" is tagged as
   containing some value (the assignment is unchaned by this routine), and that
   "dest" has already been allocated.  None of the value information is propagated
   from src to dest, because the result may be a converted value whose properties
   is known only to the caller. *)

procedure move_reg (dest: registers; src: registers);
 var exp: expr;
 begin
  if regdesc[src]^.loc.mode = fw then begin     (* single or double word move *)
    if (regdesc[src]^.value_size = 2) and (regdesc[dest]^.group_size = 2)
      then gen_rr (dmove, dest, src)
      else gen_rr (move, dest, src);
  end (* dw, fw move *) 

  else begin                            (* rhw values, sign extend in move *)
    if regdesc[src]^.signed
      then gen_rr (hrres, dest, src)
      else gen_rr (hrrzs, dest, src)
  end;

  regdesc[src]^.loc.mode := fw;         (* both must be fw after this operation *)
  regdesc[dest]^.loc.mode := fw;
 end;
$PAGE extend_reg
(* EXTEND REG sign extends a register.  It is assumed that the register contains
   a halfword value, and has been tagged as containing some value.  On return,
   the register will be marked as having a full word value. *)

procedure extend_reg (reg: registers);
 var exp: expr;
 begin
  if regdesc[reg]^.signed
    then gen_rx (hrrei, reg, reg)
    else gen_rx (hrrzi, reg, reg);
  regdesc[reg]^.loc.mode := fw;
 end;
$PAGE do_inst
(* DO INST generates two operand instructions of the register-memory class
   (e.g. add, fad, and, etc.).  In preparing operands, this routine duplicates
   the actions of "load" and "fetch".  For example, it loads index register
   and performs sign extension.  However, the order in which the steps are
   performed are changed in order to subsume those steps into operations
   required to generate the instruction.  The primary problem that this routine
   deals with is that of multi-use operands contained in registers that will
   be destroyed by the instruction.  *)

type
  inst_opt_list =
      (  commutative,                   (* operands may be exchanged *)
         mem_form,                      (* opcM may be generated *)
         double      );                 (* must apply inst twice to process 2 word operands *)
  inst_opt_set = set of inst_opt_list;

public function do_inst
      ( opc_group: opc_range;                   (* instruction to emit *)
        treg: reg_selector;             (* prefered target register *)
        oper1, oper2: expr;             (* left and right operands *)
        regsize: bit_range;             (* size of register in which to perform operation *)
        opsize: bit_range;              (* size to use for operands *)
        inst_options: inst_opt_set      (* properties of instruction to generate *)
                        ): registers;   (* register containing result returned *)

 var
   opc: opc_range;                      (* particular form of opc_group to emit *)
   loc1: addr_desc;                             (* location of operand 1 *)
   r: registers;                                (* register in which op1 has been loaded *)
   extend_op1: boolean;                 (* implies operand 1 requires sign extension *)
   expand_op1: boolean;                 (* implies single precision expansion to double *)
   loc2: addr_desc;                     (* location of operand 2 *)
   r2: registers;                       (* location of op2 if in reg, else "noreg" *)
   extend_op2: boolean;                 (* implies register operand 2 requires sign extension *)
   rx2: registers;                      (* a register involved in accessing op2 *)
   rr: registers;                       (* register where result was placed *)
   reverse: boolean;                    (* indicates operands may be reversed *)
   result_to_r2: boolean;               (* indicates mem form of operator used *)
   op1, op2, temp: expr;
   temploc: addr_desc;

 begin
  op1 := oper1; (* use local in case we want to swap *)
  op2 := oper2;
  if (opsize > 36) and (double in inst_options) (* compile sub-trees of operands *)
    then begin  (* 2 applications of instruction required *)
      prep_direct (treg, op1);  (* cannot indirect *)
      prep_direct (anyreg, op2);
    end
    else begin  (* one application of (double) instruction required *)
      prepare (treg, op1);
      prepare (anyreg, op2);
    end;

  loc1 := locate (op1);         (* get current addressing info *)
  if loc1.reloc.kind = register_sc      (* don't displace op1's regs to fetch op2 *)
    then r := loc1.offset
    else r := loc1.index;       (* if any *)
  lock (r);
  loc2 := locate (op2);
  unlock (r);

  (* For commutative operators, check if the reverse form is "better" than the
     original form (i.e. op2 op op1 instead of op1 op op2).  The idea is to choose
     the operand to be loaded into a register which will be odestroyed by the
     operation. *)

  if (op1 <> op2) and (commutative in inst_options) then begin
    reverse := (usage (op2) = 1) and		(* never kill op2 if it has more uses *)
	       not (is_register (loc2) andif
		    (loc2.offset in [0, sp, sb])) and	(* DON'T KILL SP OR FP !!! *)

    (* If one of the two operands has one remaining use and the other has several,
       then the single use operand is selected, as its value is of no use after
       the current operation. *)

	       ( (usage (op1) > 1) or

    (* If both have single uses, then select an operand which has to be loaded
       into a register in order for its value to be accessed. *)

		 (loc2.mode <> fw) or

    (* If both have single uses, select the one which is in a register to
       be the left operand.  This avoids an extra load. *)

		 (is_register (loc2) and not is_register (loc1)) );

    if reverse then begin                       (* swap the operands *)
      temp := op1; op1 := op2; op2 := temp;
      temploc := loc1; loc1 := loc2; loc2 := temploc;
    end;
  end (* commutative *) ;

  (* Load operand 1 into a register.  If it is already in a register, then that
     register is used.  Determine if the value requires sign extension, but
     defer the operation. *)

  if is_register (loc1) then begin      (* value already in register *)
    r := loc1.offset;
    extend_op1 := (opsize = 36) and (loc1.mode <> fw);
    expand_op1 := (regsize = 72) and (regdesc[r]^.group_size <> 2);
  end

  else begin                                    (* not in reg, must load *)
    consume (op1);                              (* permit an index register to be allocated *)
    if usage (op1) = 1
      then r := get_reg (treg, regsize) (* register may be used for result *)
      else r := get_reg (anyreg, opsize);       (* register will have other uses *)
    do_move (r, loc1, alignment (op1), opsize); (* load it *)
    tag_reg (r, op1);                           (* remember that op1 now here *)
    loc1 := locate (op1);                       (* get register address *)
    extend_op1 := false;                        (* do_move always sign extends *)
  end;


  (* Make operand 2 available as a memory operand.  If it is already in a register,
     then that register may be used.  Determine if such a register requires sign
     extension, but defer that operation. *)

  loc2 := locate (op2);
  if op2 = op1 then begin (* use same register for both ops *)
    loc2 := loc1;
    r2 := r;
    extend_op2 := false; (* only needs to be done once *)
  end

  else if is_register (loc2) then begin (* already in register *)
    r2 := loc2.offset;
    extend_op2 := (opsize = 36) and (loc2.mode <> fw);
  end

  else begin
    r2 := noreg;        (* remember that op2 is not in a register *)
    extend_op2 := false;
    if (loc2.mode <> fw) and not ((loc2.mode = rhw) and (opsize = 18)) then begin
      lock (r);
      consume (op2);                    (* permit use of index register *)
      if (mem_form in inst_options) and (usage (op1) > 1) and (usage (op2) = 1)
        then r2 := get_reg (treg, opsize)       (* register may be used as target *)
        else r2 := get_reg (anyreg, opsize);
      do_move (r2, loc2, alignment (op2), opsize);      (* load operand 2 *)
      tag_reg (r2, op2);
      loc2 := locate (op2);             (* get register address *)
      unlock (r);
    end;
  end;


  (* Now determine how to perform the operation.  What is at issue is that the
     instruction will destroy its destination operand.  If the destination is
     not free at the end of the operation, then a copy must be performed in
     order to get a register that can be destroyed.  The following observations
     are considered in determining what to do.  (1) The choice of destination operand
     is possible if mem_form allowed - that is, <opcM r,m> can be generated instead
     of <opc r,m> thus destroying op2.  This is only used if op2 appears in a
     register.  (2) A copy may be required in order to do a sign extension for
     certain cases of scalar operations;  if so, this copy can effect a copy
     required to sidestep the destination. *)

  opc := opc_group;                     (* use primary form unless changed below *)
  result_to_r2 := false;

  free (op1);                           (* if possible, free the registers tied to the operands *)
  free (op2);

  if r2 = r then rx2 := noreg (* find any special reg associated with op2 *)
  else if r2 <> noreg then rx2 := r2
  else if loc2.reloc.kind = register_sc then rx2 := loc2.offset
  else rx2 := loc2.index;               (* this may be "noreg" *)

  if reg_is_available (r, regsize) then begin        (* put result in place of op1 *)
    rr := get_reg (r, regsize);         (* allocate r to result *)
  end

  else if extend_op1 or expand_op1 then begin           (* extend and copy op1 at same time *)
    lock (rx2);                         (* don't overwrite any reg important to op2 *)
    rr := get_reg (treg, regsize);
    unlock (rx2);
    move_reg (rr, r);
    extend_op1 := false;        (* just did *)
    loc1.mode := fw;
  end

  else if (mem_form in inst_options) and ((r2 <> noreg) andif reg_is_available (r2, opsize)) then begin
    rr := get_reg (r2, opsize);
    opc := opc_group + memc;
    result_to_r2 := true;
  end

  else if (mem_form in inst_options) and extend_op2 then begin  (* note extend_op2 => op2 in register *)
    lock (r);   (* don't copy into r *)
    rr := get_reg (treg, opsize);
    unlock (r);
    move_reg (rr, r2);  (* failure of prev case insure's r not free *)
    extend_op2 := false;                (* just did *)
    loc2.mode := fw;
    opc := opc_group + memc;
  end

  else begin                            (* no alternative but to copy *)
    lock (rx2);
    rr := get_reg (treg, regsize);
    unlock (rx2);
    if r <> rr then move_reg (rr, r);
  end;


  (* Perform any final preparation required on the operands:  sign extension, and
     for operand 2, changing a register reference into a faster indexed reference
     when possible. *)

  if extend_op1 then begin
    extend_reg (r);
    loc1.mode := fw;
  end;

  if (r2 <> noreg) and not result_to_r2 then begin (* op2 may be an index form reference *)
    if (op2^.desc.kind in [scalars, pointers, ints, bools, chars]) andif
       ((not op2^.desc.signed) and (op2^.desc.int_prec = 18)) then begin
      loc2 := immediate_reference;      (* implicit extension gives full word *)
      loc2.index := r2;
      extend_op2 := false;
    end

    else if (opsize = 18) and ((loc1.mode <> fw) or (loc2.mode <> fw)) then begin
      loc2 := immediate_reference;      (* eighteen bits suffice, if rhw result anyway *)
      loc2.index := r2;
      if op2^.desc.signed then loc2.mode := rhw;
    end
  end;

  if extend_op2 then begin
    extend_reg (r2);                    (* must be in register if flag true *)
    loc2.mode := fw;                    (* record extension *)
  end;


  (* Generate the instruction, and determine whether or not the result requires
     sign extension.  This check applies only to scalar operations, but only
     those may involve rhw values. *)

  if result_to_r2
    then gen_rr (opc, r, r2)            (* opcM r,r2 *)
    else gen_rm (opc, rr, loc2);        (* use rr as reg, since r may have been moved *)
  if (opsize > 36) and (double in inst_options) then begin      (* process second word *)
    if rr = r2
      then gen_rr (opc, r+1, r2+1)
      else begin
        loc2.offset := loc2.offset + 1;
        gen_rm (opc, rr+1, loc2);
      end;
   end;

  if (loc1.mode = rhw) or (loc2.mode = rhw)
    then regdesc[rr]^.loc.mode := rhw;

  do_inst := rr;        (* return the register in which result appears *)
 end;
$PAGE do_move_op
(* DO MOVE OP generates an instruction for that class of operations which can be
   done in a moving instruction, e.g. abs, arithmetic negation;  "opc" gives the
   opcode, "treg" the preferred target register, "exp" the node of the operator
   being compiled (not the operand), and "p" gives the precision of the operand.
   "Imd_ok" indicates whether the operator can accept an immediate operand. *)

procedure do_move_op (opc: opc_range;
                      treg: registers;
                      exp: expr;
                      p: bit_range;
                      imd_ok: boolean);

 var
   maddr: addr_desc;
   reg: registers;

 begin
  prepare (treg, exp^.operand[1]);
  if imd_ok
    then maddr := fetch (exp^.operand[1], p)
    else maddr := fetch_direct (exp^.operand[1], p);
  free (exp^.operand[1]);               (* permit reuse of registers assigned to op1 *)
  reg := get_reg (treg, p);
  gen_rm (opc, reg, maddr);
  tag_reg (reg, exp);
 end;
$PAGE prep_operand
(* PREP OPERAND loads a value into a destroyable register and performs sign
   extension and size conversion if required.  One use of the operand is freed.
   The logic closely follows that of "do_inst" except that only one operand is
   involved. *)

public function prep_operand
     (  op: expr;                       (* the operand to load *)
        treg: reg_selector;             (* prefered target register *)
        srcsize: bit_range;             (* the precision of the operand *)
        dstsize: bit_range              (* the precision of the result *)
                             ): registers;      (* the register loaded *)

 var
   reg: registers;
   result: registers;
   loc: addr_desc;

 begin
  prepare (treg, op);                   (* compile the sub-trees of the operand *)

  (* Get the operand in some register, from which it will be moved to a register
     of appropriate size if necessary. *)

  loc := locate (op);                   (* where is operand now *)
  if is_register (loc)
    then reg := loc.offset              (* is in a register *)

  else begin                            (* not in a register, must load *)
    consume (op);                       (* permit index register to be reallocated *)
    if usage (op) = 1
      then reg := get_reg (treg, dstsize)       (* register may be used for result *)
      else reg := get_reg (anyreg, srcsize);    (* register will have other uses *)
    do_move (reg, loc, alignment (op), min (dstsize, srcsize)); (* load it *)
    tag_reg (reg, op);                  (* remember where it is *)
  end;

  (* It is necessary to move the operand if (1) the value has remaining uses,
     (2) sign extension is required, or (3) the destination size is different
     from the source size and the register required for extension is not
     available *)

  free (op);                            (* release reg if possible *)

  if (srcsize = 36) and (regdesc[reg]^.loc.mode <> fw) then begin       (* sign extension required *)
    result := get_reg (treg, dstsize);          (* try to move to target in process *)
    if result = reg
      then extend_reg (result)
      else move_reg (result, reg);              (* extends as it moves *)
  end

  else if reg_is_available (reg, dstsize) then begin         (* checks for size, remaining uses *)
    result := get_reg (reg, dstsize);           (* allocate result in place of operand *)
  end

  else begin                                    (* must move *)
    result := get_reg (treg, dstsize);
    move_reg (result, reg);
  end;

  prep_operand := result;                       (* return register allocated *)
 end;
$PAGE dload
(* DLOAD loads a value of specified size into a free register.  It is used to
   fetch an operand of an operator which will destroy the value in performing
   the operation.  Its calling sequence is the same as "load", and has the same
   function as "prep_operand" except that no size conversion is performed. *)

public function dload (treg: reg_selector; exp: expr; p: bit_range): registers;
 begin
  dload := prep_operand (exp, treg, p, p);
 end;
$PAGE rt_call
(* RT CALL generates a runtime routine call for a n (n <= 4) operand function, given
   "treg" the preferred result register; "exp" the operator node; and "rts" the
   single precision runtime routine.  The n operands are fetched, freed and past
   to the runtime routine.  A result register is allocated, and tagged as holding
   "exp".   It is assumed that a double precision routine's symbol is the successor
   of the single precision routine's symbol. *)

procedure rt_call ( treg: reg_selector; exp: expr; rts: rt_symbol );

 var
   reg: registers;
   mem: array[1..4] of addr_desc;
   p: bit_range;
   i: 0..5;

 begin
  p := precision (exp); (* determine the result size *)
  with exp^ do begin
    for i := 1 to upperbound (operand) do     (* compile the subtrees *)
      prepare (anyreg, operand[i]);
    clr_rv;
    for i := 1 to upperbound (operand) do begin       (* fetch the operands *)
      mem[i] := fetch_direct (operand[i], max (precision (operand[i]), 36));
      mem_lock (mem[i]);
    end;

    for i := 1 to upperbound (operand) do begin       (* free the operands, before alloc of result reg *)
      mem_unlock (mem[i]);
      free (operand[i]);
    end;
    reg := get_reg (treg, p);

    if p <= 36  (* call the appropriate runtime routine *)
      then gen_rt (pushj, sb, rts)
      else gen_rt (pushj, sb, succ (rts));

    for i := 1 to upperbound (operand) do
      gen_rm (arg, reg, mem[i]);
    tag_reg (reg, exp); (* remember where we put it *)
  end (* with *) ;
 end;
$PAGE rt_status_call
(* RT STATUS CALL generates a runtime call to a 0-operand status routine
   which returns a value in register 1. *)

procedure rt_status_call ( treg: reg_selector; exp: expr; rts: rt_symbol );

 var
   reg: registers;

 begin
  clr_rv;
  gen_rt (pushj, sb, rts);
  reg := get_reg (treg, 36);
  gen_rr (move, reg, 1);
  tag_reg (reg, exp);
 end;
$PAGE do_float_op
(* DO FLOAT OP compiles a "float_op" given the operator node ("exp") and 
   a preferred target register ("treg").

   Note that in the case where the operator is of the form "real*n = float (real*n)",
   a move may be generated.  The reason is that the code generator works on the
   assumption that all nodes in the tree are unique values with *unique* storage
   locations -- the move preserves this requirement. *)

procedure do_float_op (treg: reg_selector; exp: expr);

  var
    reg: registers;
    p1, pr: bit_range;

begin
  with exp^ do begin
    if operand[1]^.desc.kind in [bools, ints, chars, scalars] then begin
      if precision (exp) = 36
        then do_move_op (fltr, treg, exp, 36, false) (* integer -> single real *)
        else rt_call (treg, exp, pred (rt_d_float)); (* integer -> double real *)
    end

    else begin                          (* real to real precision conversion *)
      p1 := precision (operand[1]);     (* get bit sizes *)
      pr := precision (exp);

      if p1 < pr then begin (* single -> double *)
        reg := prep_operand (operand[1], treg, p1, pr);
        gen_ri (setz, reg+1, 0);
        tag_reg (reg, exp);
      end
      else (* double -> single *)
        rt_call (treg, exp, rt_d_to_s);
    end;
  end (* with *) ;
end;
$PAGE do_min_max_op, dirty_value
(* DIRTY VALUE markes the value of an expression as being "dirty".  Any
   attempt to save a register associated with the expression will cause
   a STORE REG to take place to a temporary, even if the temporary was
   allocated previously. *)

PUBLIC PROCEDURE dirty_value(exp: expr);
VAR vdesc: val_desc;
BEGIN
  vdesc := get_value_desc(exp);
  IF vdesc <> NIL THEN WITH vdesc^ DO BEGIN
    IF loc.reloc.kind = temp_sc THEN is_dirty := TRUE
  END
END;
(* DO MIN MAX OP compiles code for integer or real, min or max operators,
   given the operator node ("exp") for the operator, the comparision
   subopcode ("cmp"), and the precision ("p") of the operands.  A register
   preference ("treg") may be specified.  Load the first operand into a
   destroyable register.  As we compare this value against each successive
   operand, if the value of the operand is found to be less (greater) than
   the first, the register is loaded with the value of the operand compared
   against.  In order to keep track of the register as the other operands
   are evaluated (possibly displacing the register), the min/max node is
   tagged as being located in the register, and the node's value is reloaded
   before each compare.  If it was not displaced, then no code is generated
   for the reload. *)

PROCEDURE do_min_max_op
(treg: reg_selector; exp: expr; cmp: cmp_codes; p: bit_range);
VAR reg: registers; mem: addr_desc; i: index_range;
BEGIN
  WITH exp^ DO BEGIN
    reg := dload(treg, operand[1], p);
    (* Process each operand and compare with the min/max value to date. *)
    FOR i := 2 TO UPPERBOUND(operand) DO BEGIN
      tag_reg(reg, exp);
      IF p = 72 THEN prep_direct(anyreg, operand[i])
      ELSE prepare(anyreg, operand[i]);
      reg := load(treg, exp, p);
      lock(reg); mem := fetch(operand[i], p); free(operand[i]); unlock(reg);
      IF p = 36 THEN gen_rm(cam+cmp, reg, mem)
      ELSE BEGIN
        IF cmp = ltc THEN gen_rm(cam+gec, reg, mem)
        ELSE gen_rm(cam+lec, reg, mem);
        gen(jrst, 0, 0, 4, dot);
        gen_rm(cam+nec, reg, mem); mem.offset := mem.offset + 1;
        gen_rm(cam+cmp, reg+1, mem); mem.offset := mem.offset - 1;
      END;
      do_move(reg, mem, signed_value, p); dirty_value(exp)
    END;
    tag_reg(reg, exp)
  END
END;
$PAGE iconstp
(* ICONSTP is a predicate which indicates if its argument has a constant value.
   It is assumed that the argument has an ordinal type.  If the argument does
   have a constant value, its value is returned. *)

public function iconstp (exp: expr; var val: int_type): boolean;
 begin
  iconstp := false;
  with exp^ do begin
    if item.index = nil then begin
      if opcode = immed_ref then begin
        iconstp := true;
        val := item.offset;
      end
      else if (opcode = mem_ref) andif
              (item.class = constant_sc) andif
              (item.cstref^.kind = scalar_cst) then begin
        iconstp := true;
        val := item.cstref^.scalar_val;
      end;
    end;
  end;
 end;
$PAGE get_bounds
(* GET BOUNDS returns an approximation of the bounds of a scalar value. *)

procedure get_bounds (exp: expr; var minval, maxval: int_type);
 var base: typ;
 begin
  if iconstp (exp, minval)
    then maxval := minval
  else begin
    base := exp^.desc.base;             (* base type of scalar *)
    if (exp^.opcode = mem_ref) andif (exp^.item.class in [local_sc, parameter_sc, static_sc, external_sc])
      then base := exp^.item.sym_name^.type_desc;       (* try to get subrange info *)

    if not (base^.kind in [scalars, bools, ints, chars]) then begin
      minval := 0;                      (* must be coerced pointer *)
      maxval := 777777B;
    end

    else begin
      minval := base^.minval;
      maxval := base^.maxval;
    end;
  end;
 end;
$PAGE do_setcvt_op
(* DO SETCVT OP compiles a "setcvt_op" given the operator ("node") and a
   preferred target register ("treg").  The conversion is assumed to yield
   a set of 72 bits or less.

   See the note in "do_float_op" about why a move is always generated. *)

procedure do_setcvt_op (treg: reg_selector; exp: expr);

 var
   reg: registers;
   tregsize, opregsize, regsize: set_range;
   tlen, tlwb, oplen, oplwb: set_range;

 begin
  with exp^ do begin
    if operand[1]^.opcode = desc_ref            (* must convert long set *)
      then reg := ls_setcvt (treg, desc, operand[1])
    else begin
      tlen := desc.set_length;
      tlwb := desc.set_lwb;
      oplen := operand[1]^.desc.set_length;
      oplwb := operand[1]^.desc.set_lwb;

      tregsize := ngm (tlen, 36);
      opregsize := ngm (oplen, 36);
      regsize := tregsize;              (* this is size of register to load initially *)

      if (opregsize = 72) and (tregsize = 36) then begin
        if ((tlwb + tlen - 1) <= (oplwb + 36 - 1))      (* if extracting only from first word *)
          then oplen := 36
          else regsize := 72;           (* must load 2 words then shift *)
      end;

      reg := prep_operand (operand[1], treg, opregsize, regsize);
      if (regsize = 72) and (opregsize < regsize)            (* 36 -> 72, must extend *)
        then gen_ri (setz, reg+1, 0);
      if min (opregsize, regsize) > 0 then (* avoid shifting a cleared reg *)
	left_shift (reg, regsize, tlwb - oplwb);  (* make lwb's the same *)
      oplen := max (0, min (tregsize, oplen + (oplwb - tlwb))); (* get no of nonzero bits after shift *)
      left_mask (reg, oplen, tlen);     (* remove extraneous bits *)

      if regsize > tregsize then begin  (* 72 -> 36, 2 words loaded, 1 word result *)
        free_reg (regdesc[reg]);        (* get rid of extra reg *)
        reg := get_reg (reg, 36);
      end;
    end;
  end (* with *) ;
  tag_reg (reg, exp);
 end;
$PAGE int_value
(* INT VALUE returns an addr_desc for any integer value.  If possible, the
   value is made immediate, otherwise a constant word is omitted, and its
   address returned. *)

public function int_value ( val: int_type ): addr_desc;
 begin
  int_value := absolute_reference;      (* init most fields to default *)
  if (val >= 0) and (val <= 777777B)
    then begin
      int_value.immediate := true;
      int_value.offset := halfword (val);
    end
  else int_value.reloc := gen_cint (val);
 end;
$PAGE set_check
(* SET CHECK determines if a runtime check must be generated to see if a value
   is within the bounds of a set.  If so true is returned, and code producing
   a skip if in bounds is generated;  otherwise false is returned and no code
   is generated. *)

function set_check ( opreg: registers; op: expr; lwb, upb: set_range ): boolean;
 var minval, maxval: int_type;
 begin
  get_bounds (op, minval, maxval);
  set_check := not ((lwb <= minval) and (maxval <= upb));
  if set_check then begin               (* op not a priori in bounds *)
    if (maxval <= upb)          (* only lwb out of bounds *)
      then gen_rm (cam+gec, opreg, int_value (lwb))
      else begin                        (* upb (and maybe lwb) out of bounds *)
        if (minval < lwb)
          then gen_rm (cam+ltc, opreg, int_value (lwb));
        gen_rm (cam+lec, opreg, int_value (upb));
      end;
  end;
 end;
$PAGE gen_singleton_set
(* GEN SINGLETON SET generates a set containing a single element.  "Exp" is the
   1 operand gen_set_op describing the set to be generated.  The register in
   which the result is replaced is tagged. *)

procedure gen_singleton_set (treg: reg_selector; exp: expr);

 var
   reg, opreg: registers;
   upb: set_range;
  cval: int_type;

 begin
  with exp^ do begin
    prepare (anyreg, operand[1]);       (* load the element index *)
    if iconstp (operand[1], cval) then begin
      reg := get_reg (treg, desc.set_length);
      cval := cval - desc.set_lwb;
      free (operand[1]);
      if (cval < 0) or (cval >= desc.set_length) then begin
        if desc.set_length > 36 then
          gen_rr (setzb, reg, reg+1)
        else gen_ri (movei, reg, 0)
      end
      else begin
        if cval >= 36 then begin
          gen_ri (movei, reg, 0);
          if cval >= 54 then
            gen_ri (movei, reg+1, 2**(71-cval))
          else gen_ri (hrlzi, reg+1, 2**(53-cval))
        end
        else begin
          if desc.set_length > 36 then
            gen_ri (movei, reg+1, 0);
          if cval >= 18 then
            gen_ri (movei, reg, 2**(35-cval))
          else gen_ri (hrlzi, reg, 2**(17-cval));
        end;
      end;
    end (* if constant element *)

    else begin
      upb := desc.set_lwb + desc.set_length - 1;        (* save upperbound for later used *)

      if desc.set_length <= 36 then begin
	if prog_options.overlay_opt then begin
	  reg := dload (treg, operand[1], 36);
	  if set_check (reg, operand[1], desc.set_lwb, upb) then
	    gen_ri (hrrei, reg, desc.set_lwb - 1);
	  gen_rt (add, reg, rt_mask_ss);
	  gen (move, reg, reg, -desc.set_lwb, none);
	end
	else begin
	  opreg := load (anyreg, operand[1], 36);
	  free (operand[1]);
	  reg := get_reg (treg, 36);
	  if set_check (opreg, operand[1], desc.set_lwb, upb)
	    then gen_rr (tdza, reg, reg);
	  gen (move, reg, opreg, -desc.set_lwb, relrt (rt_mask_ss));
	end;
      end

      else (* set_length > 36 *) begin
	opreg := load (anyreg, operand[1], 36);
        lock (opreg);
        reg := get_reg (treg, 72);
        unlock (opreg);
        free (operand[1]);
	if prog_options.overlay_opt
	  then genind (dmove, reg, 0, 0, relrt (rt_mask_sd1))
	  else gen_rt (dmove, reg, rt_mask_sd1);
        gen_rr (movn, 1, opreg);
        gen (lshc, reg, 1, desc.set_lwb, none);
        if set_check (opreg, operand[1], desc.set_lwb, upb)
          then gen_rr (setzb, reg, reg+1);
      end
    end
  end (* with *) ;
  tag_reg (reg, exp);
 end;
$PAGE set_ce
(* SET CE is a utility procedure for get_range_set.  It creates a set of the form
   [cstlwb..(exp+cstoffset)] converted to some specified lwb and length. *)

function set_ce
     (  treg: reg_selector;             (* register preference *)
        desc: expr_type_desc;           (* gives lwb and length of set to generate *)
        cstlwb: set_range;      (* limits of 1 bits to turn on *)
        exp: expr;
        cstoffset: int_type
          ): registers;                 (* register in which set is placed *)

 var
   reg, idxreg: registers;
   minval, maxval: int_type;
   upb: set_range;

 begin
  upb := desc.set_lwb + desc.set_length - 1;    (* save set upper bound *)

  idxreg := load (anyreg, exp, 36);
  gen_rr (movn, 1, idxreg);

  get_bounds (exp, minval, maxval);
  if minval < cstlwb then begin (* force exp to be in [cstlwb .. upb] *)
    gen_rm (cam+gec, idxreg, int_value (cstlwb));
    gen_ri (movei, 1, - (cstlwb - 1));
  end;
  if maxval > upb then begin
    gen_rm (cam+lec, idxreg, int_value (upb));
    gen_ri (movei, 1, -upb);
  end;

  free (exp);                           (* allocate register for set *)
  reg := get_reg (treg, desc.set_length);

  if desc.set_length <= 36 then begin   (* create cstlwb..exp 1's *)
    gen_ri (seto, reg, 0);
    gen (lsh, reg, 1, 35 + cstlwb - cstoffset, none);
  end
  else (* double word case *) begin
    gen_ri (setob, reg, reg+1); (* mask is all ones *)
    gen (lshc, reg, 1, 71+cstlwb-cstoffset, none);
  end;
  right_shift (reg, desc.set_length, cstlwb-desc.set_lwb);      (* insert lwb..cstlwb-1 0's *)

  set_ce := reg;                        (* return reg where result placed *)
 end;
$PAGE half_set
(* HALF SET is another helper for gen_range_set, to form short constant sets *)

procedure half_set (lwb, upb: int_type; reg: registers);

begin
  if (upb < 0) or (lwb >= 36) then gen_ri (movei, reg, 0)
  else if lwb <= 0 then begin
    if upb >= 35 then gen_ri (seto, reg, 0)
    else if upb < 18 then gen_ri (hrlzi, reg, lmasks[upb+1])
    else gen_ri (hrroi, reg, lmasks[upb-17]);
  end
  else if upb >= 35 then begin
    if lwb < 18 then gen_ri (hrloi, reg, rmasks[18-lwb])
    else gen_ri (movei, reg, rmasks[36-lwb]);
  end
  else if lwb >= 18 then gen_ri (movei, reg, rmasks[upb-lwb+1]*(2**(35-upb)))
  else if upb < 18 then gen_ri (hrlzi, reg, rmasks[upb-lwb+1]*(2**(17-upb)))
  else begin
    gen_ri (hrloi, reg, rmasks[18-lwb]);
    gen_ri (andcmi, reg, rmasks[35-upb]);
  end;
end;
$PAGE gen_range_set
(* GEN RANGE SET constructs a set of the form [op1..op2] for set of dimension
   72 or less.  "Exp" is a 2 operand gen_set_op; "treg" is a register preference.
   The result register is tagged. *)

procedure gen_range_set (treg: reg_selector; exp: expr);

 var
   reg, reg2: registers;
   mreg: registers;
   mlen: set_range;
   cval, dval: int_type;
   word: pdp10word;

 begin
  with exp^ do begin
    if iconstp (operand[1], cval)
      then begin
        free (operand[1]);
        if iconstp (operand[2], dval) then begin
          free (operand[2]);
          cval := max (0, cval - desc.set_lwb);
          dval := min (dval - desc.set_lwb, desc.set_length - 1);
          reg := get_reg (anyreg, desc.set_length);
          if cval > dval then
            if desc.set_length > 36 then
              gen_ri (setzb, reg, reg+1)
            else gen_ri (movei, reg, 0)
          else if (cval = 0) and (dval = 71) then gen_ri (setob, reg, reg+1)
          else begin
            half_set (cval, dval, reg);
            if desc.set_length > 36 then
              half_set (cval - 36, dval - 36, reg + 1);
          end;
        end
        else begin
          prepare (anyreg, operand[2]);
          reg := set_ce (treg, desc, max (desc.set_lwb, cval), operand[2], 0);
        end;
      end
    else if iconstp (operand[2], cval)
      then begin
        free (operand[2]);
        prepare (anyreg, operand[1]);
        reg := set_ce (treg, desc, desc.set_lwb, operand[1], -1);       (* get [lwb..op1-1] mask *)

        (* Form set by performing reg := (not reg) * [lwb..min (cval, upb)] *)

        mreg := reg;
        mlen := max (0, min (desc.set_length, cval - desc.set_lwb + 1));
        if desc.set_length > 36 then begin      (* handle two word case *)
          if mlen <= 36
            then gen_ri (setz, mreg+1, 0)       (* second word masked out *)
            else begin  (* mask of 1st word all ones *)
              gen_ri (setca, mreg, 0);
              mreg := reg + 1;
              mlen := mlen - 36;
            end;
        end;
        if mlen = 36
          then gen_ri (setca, mreg, 0)
        else if mlen >= 18
          then gen_ri (andcbi, mreg, rmasks[36-mlen])
        else (* mlen <= 18 *) begin
          word.value := rmasks [36-mlen];
          gen (andcb, reg, 0, 0, gen_cword (word, setword));
        end;
      end
    else begin                          (* nonconstants for lwb, upb *)
      prepare (anyreg, operand[1]);
      prepare (anyreg, operand[2]);
      reg := set_ce (treg, desc, desc.set_lwb, operand[1], -1); (* not [op1..upb] *)
      lock (reg);
      reg2 := set_ce (anyreg, desc, desc.set_lwb, operand[2], 0);       (* [lwb..op2] *)
      unlock (reg);
      gen_rr (andca, reg, reg2);
      if desc.set_length > 36 then gen_rr (andca, reg+1, reg2+1);
      free_reg (regdesc[reg2]);
    end;

    tag_reg (reg, exp);
  end (* with *) ;
 end;
$PAGE do_union
(* DO UNION performs the union of short sets, attempting to fold
   constant sets where possible.        *)

procedure do_union (treg: registers; exp: expr);

var
    temp_expr: expr;    (* used to rearrange operands *)
    i, top: int_type;   (* top will be number of non-constant operands *)
    set1, set2: pdp10word;      (* bits are set for constants *)
    mem: addr_desc;
    reg: registers;

(* CONSTANT SET determines if a set expression is a constant range and
   if so, sets the appropriate bits in SET1 and/or SET2. If a constant
   set is found the tuple and its operands are freed.   *)

function constant_set (cexp: expr): boolean;

var
    lwb, upb, i: int_type;
    vdesc: val_desc; 

begin
  constant_set := false;
  with cexp^ do begin
    if (opcode = gen_set_op) andif ((upperbound (operand) = 0) orif (iconstp (operand[1], lwb)
      andif ((upperbound (operand) = 1) orif iconstp (operand[2], upb)))) then begin
        constant_set := true;
        if upperbound (operand) > 0 then begin
          if upperbound (operand) = 1 then upb := lwb;        (* single element *)

          (* Set corresponding bits in set words. Be sure to correct by and
             limit to the lwb and length of the UNION tuple, since it may
             differ from that of the gen_set_op.        *)

          for i := max (0, lwb - exp^.desc.set_lwb) to min (upb - exp^.desc.set_lwb, exp^.desc.set_length - 1) do begin
            if i >= 36 then set2.bits[i-36] := true
            else set1.bits[i] := true;
          end;
        end;
        vdesc := get_value_desc (cexp);
        vdesc^.generated := true;
        if vdesc^.uses = 1 then (* last use of gen_set_op ==> free operands *)
          for i := 1 to upperbound (operand) do
            free (operand[i]);
        free (cexp);
      end       (* if constant set *)
  end (* with *);
end (* constant_set *);

(* DO OR attempts to perform an OR of a constant into a register with
   an immediate instruction.    *)

procedure do_or (reg: registers; cset: pdp10word);

begin
  with cset do if value <> 0 then begin
    if value = -1 then gen_ri (seto, reg, 0)
    else if lh = 0 then gen_ri (iori, reg, rh)
    else if rh = 0 then gen_ri (tlo, reg, lh)
    else if lh = 777777b then gen_ri (orcmi, reg, - (value + 1))
    else gen (ior, reg, 0, 0, gen_blt (lh, none, rh, none));
  end;
end (* do_or *);

(* LOAD_SET generates immediate (if possible) instructions to load a set into 
    a specified registers.      *)

procedure load_set (reg: registers; cset: pdp10word);

begin
  with cset do begin
    if lh = 0 then gen_ri (movei, reg, rh)
    else if lh = 777777b then gen_ri (hrroi, reg, rh)
    else if rh = 0 then gen_ri (hrlzi, reg, lh)
    else if rh = 777777b then gen_ri (hrloi, reg, lh)
    else gen (move, reg, 0, 0, gen_blt (lh, none, rh, none));
  end;
end (* load_set *);

begin (* do_union *)
  with exp^ do begin

  (* First, rearrange the operands to put non-constant sets at the
     head of the list.  *)

    top := upperbound (operand);
    set1.value := 0;    (* initially, no constants *)
    set2.value := 0;
    i := 1;
    repeat
      if constant_set (operand[i]) then begin   (* also stuffs bits in set1 and set2 *)
        while (top >= i) andif ((top = i) orif constant_set (operand[top])) do
          top := top - 1;
        if top > i then (* swap *) begin
          temp_expr := operand[i];
          operand[i] := operand[top];
          operand[top] := temp_expr;
          top := top - 1;
        end;
      end;
      i := i + 1;
    until i > top;

    (* extreme case: constant set has all elements, so union is superfluous *)

    if (set1.value = -1) andif ((desc.set_length <= 36) orif (set2.value = -1)) then begin
      for i := 1 to top do free (operand[i]);
      reg := get_reg (treg, desc.set_length);
      if desc.set_length > 36 then
        gen_rr (setob, reg, reg+1)
      else gen_ri (seto, reg, 0);
    end
    else begin  (* work to be done *)
      if top >= 2 then begin    (* two or more non-constant sets *)
        reg := do_inst (ior, treg, operand[1], operand[2], desc.set_length, desc.set_length, [double, mem_form, commutative]);
        for i := 3 to top do begin      (* merge in rest *)
          tag_reg (reg, exp);   (* save intermediate value while getting operand *)
          if desc.set_length > 36 then
            prep_direct (anyreg, operand[i])
          else prepare (anyreg, operand[i]);
          reg := load (treg, exp, desc.set_length);     (* reload if displaced *)
          lock (reg);   (* make operand addressible *)
          mem := fetch (operand[i], desc.set_length);
          free (operand[i]);
          unlock (reg);
          gen_rm (ior, reg, mem);       (* perform union *)
          if desc.set_length > 36 then begin
            mem.offset := mem.offset + 1;
            gen_rm (ior, reg+1, mem);
          end;
        end (* for remaining operands *);
      end (* if two or more non-constant sets *)
      else if top = 1 then begin        (* single non-constant set *)
        prepare (treg, operand[1]);
        reg := load (treg, operand[1], desc.set_length);        (* get set *)
        free (operand[1]);
        reg := get_reg (reg, desc.set_length);  (* was freed by free *)
      end (* if any non-constant sets *)
      else (* constant set only *) reg := get_reg (treg, desc.set_length);

      if top >= 1 then begin    (* merge constants with loaded set *)
        do_or (reg, set1);
        if desc.set_length > 36 then
          do_or (reg+1, set2);
      end
      else begin
        if (set1.value = 0) and (set2.value= 0) then begin      (* empty set only *)
          if desc.set_length > 36 then
            gen_rr (setzb, reg, reg+1)
          else gen_ri (setz, reg, 0);
        end
        else begin      (* load constant set *)
          load_set (reg, set1);
          if desc.set_length > 36 then
            load_set (reg+1, set2);
        end;
      end (* if constant set only *)
    end (* if not all elements of set *)
  end (* with exp^ *);
  tag_reg (reg, exp);
end (* do_union *);
$PAGE form_address
(* FORM ADDRESS is a helper procedure for "prep_mem_ref".  It folds in the index and
   symbolic relocation information for an address descriptor "maddr" given the
   referencing "item".  At the time this is called, it is assumed that the
   maddr mode and offset fields have been filled in with appropriate values for
   the reference being made.  In particular, it is important that item.offset not
   be consulted for the offset; the reason is that the caller has calculated the
   correct *word* offset when the reference is a byte reference. *)

procedure form_address (var maddr: addr_desc; item: mem_addr_desc);

 var
   idxaddr: addr_desc;
   idxreg: registers;

 begin
  maddr.indirect := false;
  maddr.immediate := false;

  if item.index = nil then
    idxreg := 0 (* no index expression; no index register *)

  else begin
    prepare (anyreg, item.index); (* get the index expression address *)

    (* If the item reference is of the form n[x], with no symbolic name (an
       absolute reference), then some optimization of the reference may be
       possible. *)

    if (item.class = absolute_sc) and (item.index^.desc.kind = pointers) then begin
      idxaddr := locate (item.index);

      (* If the item reference is of the form 0[x], and "x" is a directly
         addressable full-word pointer in memory, then an indirect address
         may be generated.  This is not applied if the pointer is in a register,
         since indexed addressing is faster than indirect addressing. *)

      if (maddr.offset = 0) and
         (idxaddr.mode = fw) and (idxaddr.reloc.kind <> register_sc) and
         (not (idxaddr.indirect or idxaddr.immediate))
        then begin                      (* address can be indirect *)
          maddr.index := idxaddr.index;         (* merge idx addr into maddr *)
          maddr.offset := idxaddr.offset;       (*   and return indirect idx addr *)
          maddr.reloc := idxaddr.reloc;
          maddr.indirect := true;
          return;                               (* <---- exit here for indirect address *)
        end;

      (* If the index is an immediate temporary reference, then it will ultimately
         be compiled as m(16), so we can offset it by "n" and make it direct to
         obtain the final address. *)

      if idxaddr.immediate and (idxaddr.reloc.kind = temp_sc)
        then begin (* adjust temp address *)
          maddr.index := idxaddr.index;         (* offset maddr by idx addr *)
          maddr.offset := maddr.offset + idxaddr.offset; (*   and return direct temp addr *)
          maddr.reloc := idxaddr.reloc;
          return; (* <---- exit here for temp reference *)
        end;
    end;

    idxreg := load (anyreg, item.index, 18); (* general index expression *)
  end;

  (* At this point, "idxreg" contains the number of the index register to use
     (which may be zero).  Fill in symbolic relocation information required. *)

  maddr.index := idxreg;
  maddr.reloc.kind := item.class;
  case item.class of
    absolute_sc:  ;                             (* no relocation *)
    constant_sc: begin
      maddr.reloc := gen_cnode (item.cstref);
      if (item.cstref^.kind = string_cst) andif (item.cstref^.str_varying_ref) then
        maddr.offset := maddr.offset + 1;       (* offset to start of text *)
    end;
    code_sc:      begin                         (* must be subr id *)
                    maddr.reloc.kind := def_sc;
                    maddr.reloc.reldef :=
                      get_def (subr_def, item.sym_name^.init_value.blkp^.number);
                  end;
    others:       maddr.reloc.relsym := item.sym_name
  end;
 end;
$PAGE prepare_mem_ref
(* PREPARE MEM REF performs the function of "prepare" for all cases of mem_ref's
   immed_ref's and addr_ref's.  When the operand can be a memory reference, the
   result is noted by a tag_loc;  when it must be placed in a register, the result
   is noted with a tag_reg.  This routine is used by both prepare and locate;  the
   latter uses it to reprepare instances of memory references to insure that any
   index or base expression is properly accessible. *)

procedure prepare_mem_ref (* treg: reg_selector; exp: expr *) ;

 var
   mem: addr_desc;
   off: int_type;
   vreg, idxreg, bpreg: registers;
   p: 0..63;
   vdesc: val_desc; 
   bpreloc: rel_syllable;

 begin
  with exp^ do begin

    (* If the address is packed, convert the bit offset in "item.offset" into a
       word offset in "off" and a bit offset within the word in "p". *)

    if item.pack then begin
      if item.offset >= 0 then begin
        off := item.offset div 36;
        p := item.offset mod 36;
      end
      else begin
        off := (item.offset - 35) div 36;
        p := 35 + ((item.offset + 1) mod 36);
      end;
    end;

    case opcode of

      (* Two forms of IMMED REFs are generated by the shaping code:
              n         an immediate constant
              n[x]      an implicit add of "x" by a constant (n <> 0)           *)

      immed_ref:
        begin
          mem := immediate_reference;   (* init all fields *)
          if desc.signed
            then mem.mode := rhw                        (* only have 18 bits of full value *)
            else mem.mode := fw;                        (* implicit extension gives full word result *)
          mem.offset := halfword (item.offset);
          if item.index <> nil then begin       (* get index value *)
            prepare (anyreg, item.index);
            mem.index := load (anyreg, item.index, 18);
          end;
          tag_loc (mem, exp, item.index);                       (* remember how to address it *)
        end;

      (* Any one of three forms of MEM REFs may be generated by the shaping code:
              sym+n[x]          direct symbol reference (sym,n,x may be null)
              sym+n.m[x]        packed reference (sym,n,x may be null)
              0.0(b)            byte access, b is byte pointer expression         *)

      mem_ref:
        begin
          if item.base <> nil then begin                (* case 3 - byte access *)
            prepare (anyreg, item.base);                (* get the byte pointer as a memory operand *)
            mem := fetch (item.base, 36);
            mem.mode := byte;                   (* tag for ldb, dpb *)
            mem.slice_size := item.size;        (* need to know in case of left_aligned data *)
            tag_loc (mem, exp, item.base);
          end

          else begin                                    (* symbolic references, cases 1,2 *)
            if not item.pack    (* select between cases 1 and 2 *)
              then begin                                (* full word access *)
                off := item.offset;
                mem.mode := fw;
              end
              else begin                                (* packed field access *)
                if (item.size <> 18) orif ((item.offset mod 18) <> 0)
                  then mem.mode := slice                (* generate byte pointer on access *)
                else if (item.offset mod 36) = 0        (* halfword access *)
                  then mem.mode := lhw
                  else mem.mode := rhw;
                mem.slice_size := item.size;    (* remember byte alignment info *)
                mem.slice_offset := p;
              end;
            mem.offset := halfword (off);
            form_address (mem, item);           (* fold in index and relocation *)
            tag_loc (mem, exp, item.index);
          end;

          (* If the copy bit is set for the memory reference, then the value of
             the designated location may not continue to hold the same value
             throughout all remaining references to the tuple.  Therefore, it is
             necessary to copy the value immediately. *)

          if exp^.copy_tuple then begin
            vdesc := val_desc (exp^.result);  (* coerce *)
            mark_as_allocated (vdesc);
            with vdesc^ do begin
              consume (exp);                    (* copy into register *)
              vreg := get_reg (treg, size * 36);
              do_move (vreg, mem, alignment (exp), size * 36);
              tag_reg (vreg, exp);
              if loc_index <> nil then detach_expr (vdesc);  (* mark the value as unstored *)
              loc := null_location;
            end;
          end;
        end;

      (* Three forms of ADDR REFs may be produced by the shaping code:
              sym+n[x]          symbolic full word address (sym,n,x may be null)
              sym+n.m[x]        symbolic full word address
              0[x](b)           byte pointer b, adjusted by x                     *)

      addr_ref:
        begin
          if item.base <> nil then begin                (* must offset byte pointer *)
            prepare (anyreg, item.base);
            bpreg := dload (anyreg, item.index, 36);
            lock (bpreg);
            mem := locate (item.base);
            unlock (bpreg);
	if prog_options.ki_code_opt then
	  begin gen_rt(jsr,0,rt_inst_sml);
            gen_rm (adj_bp, bpreg, mem)          (* index it *)
	  end
	else gen_rm (adjbp, bpreg, mem);         (* index it *)
            free (item.base);
            tag_reg (bpreg, exp);                       (* result is now in this register *)
          end

          else if not item.pack then begin              (* get full word address *)
            mem.mode := fw;
            mem.offset := halfword (item.offset);
            form_address (mem, item);           (* fill in index and symbolic relocation *)
            if mem.indirect
              then mem.indirect := false                (* directly reference pointer, indirect is slower *)
              else mem.immediate := true;               (* generate address as immediate operand *)
            tag_loc (mem, exp, item.index);
          end

          else (* item.packed *) begin          (* generate a byte pointer *)

            if item.index <> nil then begin     (* create byte pointer at run time *)
              mem.offset := halfword (off);
              mem.mode := fw;                   (* this part of address is full word *)
              form_address (mem, item);         (* add index and symbolic relocation *)
              if mem.indirect
                then mem.indirect := false      (* as above *)
                else mem.immediate := true;
              free (item.index);                (* free a register allocated to the index *)
              bpreg := get_reg (anyreg, 36);    (* get a register for the byte pointer *)
              do_move (bpreg, mem, unsigned_value, 36); (* move in the address *)
              p := 36 - p - item.size;          (* compute position field of byte pointer *)
              gen_ri (hrli, bpreg, (p*10000b) + (item.size*100b)); (* move in p and s *)
              tag_reg (bpreg, exp);
            end

            else (* item.index = nil *) begin   (* a constant byte pointer will do *)
              mem.mode := slice;                (* create a slice address for the byte *)
              mem.slice_size := item.size;
              mem.slice_offset := p;
              mem.offset := halfword (off);
              form_address (mem, item);
              bpreloc := gen_bptr (mem);        (* create the byte pointer *)
              mem := absolute_reference;        (* now make the byte pointer addressable *)
              mem.reloc := bpreloc;
              tag_loc (mem, exp, nil);
            end;
          end;
        end

    end (* case *) ;
  end (* with *) ;
 end;
$PAGE prepare
(* PREPARE is used to prepare an operand ("exp") for future reference.  The idea
   is to make the operand addressible with at most one load at the time of the
   actual reference.  By convention, all the operands of an operator are prepared
   in this manner before any one is loaded or fetched.  The one load heuristic
   insures that it is possible to access the operands without displacing another.
   Essentially, this involves computing all the subtrees of an operator.  In the
   case of a memory reference, the address is partially calculated, and "locate"
   is used at the time of the reference to complete the addressing information.
   In the case of some applicative operator such as iadd, the action required is
   to compile code for the expression, and note the register assigned. A register
   preference, "treg" may be specified. *)

public procedure prepare (* treg: reg_selector; exp: expr *);

 var
   vdesc: val_desc; 
   mem: addr_desc;
   off: int_type;
   reg, reg2: registers;
   rts: rt_symbol;
   temp: val_desc; 
   i: oper_range;

 begin

  (* Get/create a value descriptor for "exp".  If the "generated" bit is set, then
     it has previously been processed by this routine, and the descriptor tells
     how to access the value.  If not, then the node must be compiled. *)

  vdesc := get_value_desc (exp);
  if vdesc^.generated then return;      (* <---- short circuit return *)
  vdesc^.generated := true;     (* will be, when this routine is done *)


  (* Prepare the value for access. *)

  with exp^ do begin
    case opcode of

      mem_ref, immed_ref, addr_ref:
        prepare_mem_ref (treg, exp);            (* prepares memory accesses *)

	desc_ref: (* unaffected by prepare *)
	  ;

      (* Display operators reference the stack frame pointer of some parent's
         stack frame. *)

      display_op:
        begin
          if nlevels = 0 then begin     (* referencing current stack frame *)
            mem := immediate_reference; (* 0(16) immediate *)
            mem.index := sp;
	    mem.mode := rhw;
	    tag_loc (mem, exp, nil);
          end

          else if nlevels = 1 then begin        (* referencing first parent *)
            mem := absolute_reference;  (* 1(16) lhw *)
            mem.mode := lhw;
            mem.offset := 1;
            mem.index := sp;
	    tag_loc (mem, exp, nil);
          end

          else begin                    (* some grand parent *)
            reg := get_reg (treg, 36);
            gen (hlrz, reg, sp, 1, none);
            for off := 1 to nlevels - 1 do
	      gen (hlrz, reg, reg, 1, none);
	    tag_reg (reg, exp);
	    regdesc[reg]^.loc.mode := rhw;
          end;
        end;

      (* All others are expression which must be computed and placed in a register. *)

      iadd_op:
        begin
          reg := do_inst (add, treg, operand[1], operand[2], 36, desc.int_prec, [commutative, mem_form]);
          tag_reg (reg, exp);
        end;

      isub_op:
        begin
          reg := do_inst (sub, treg, operand[1], operand[2], 36, desc.int_prec, [mem_form]);
          tag_reg (reg, exp);
        end;

      imul_op:
        begin
          reg := do_inst (imul, treg, operand[1], operand[2], 36, desc.int_prec, [commutative, mem_form]);
          tag_reg (reg, exp);
        end;

      idiv_op:
        begin
          reg := do_inst (intdiv, treg, operand[1], operand[2], 72, 36, [mem_form]);
          if regdesc[reg]^.group_size = 2 then begin            (* discard modulus *)
            free_reg (regdesc[reg]);            (* discard double reg *)
            reg := get_reg (reg, 36);           (* reclaim single reg *)
          end;
          tag_reg (reg, exp);
        end;

      imod_op:
        begin
          if (treg = 2) or (treg = anyreg)
            then reg := do_inst (intdiv, anyreg, operand[1], operand[2], 72, 36, [])
            else reg := do_inst (intdiv, treg-1, operand[1], operand[2], 72, 36, []);
          free_reg (regdesc[reg]);              (* discard double reg *)
          reg := get_reg (reg+1, 36);           (* reclaim remainder part *)
          tag_reg (reg, exp);
        end;

      radd_op:
        begin
          if desc.precision > srealprec
            then reg := do_inst (dfad, treg, operand[1], operand[2], 72, 72, [commutative])
            else reg := do_inst (fadr, treg, operand[1], operand[2], 36, 36, [commutative, mem_form]);
          tag_reg (reg, exp);
        end;

      rsub_op:
        begin
          if desc.precision > srealprec
            then reg := do_inst (dfsb, treg, operand[1], operand[2], 72, 72, [])
            else reg := do_inst (fsbr, treg, operand[1], operand[2], 36, 36, [mem_form]);
          tag_reg (reg, exp);
        end;

      rmul_op:
        begin
          if desc.precision > srealprec
            then reg := do_inst (dfmp, treg, operand[1], operand[2], 72, 72, [commutative])
            else reg := do_inst (fmpr, treg, operand[1], operand[2], 36, 36, [commutative, mem_form]);
          tag_reg (reg, exp);
        end;

      rdiv_op:
        begin
          if desc.precision > srealprec
            then reg := do_inst (dfdv, treg, operand[1], operand[2], 72, 72, [])
            else reg := do_inst (fdvr, treg, operand[1], operand[2], 36, 36, [mem_form]);
          tag_reg (reg, exp);
        end;

      iabs_op:
        do_move_op (movm, treg, exp, 36, true);

      ineg_op:
        do_move_op (movn, treg, exp, 36, true);

      rabs_op:
        begin
          if desc.precision <= srealprec
            then do_move_op (movm, treg, exp, 36, true)
            else begin
              reg := dload (treg, operand[1], 72);
              gen (jump+gec, reg, 0, 2, dot);
              gen_rr (dmovn, reg, reg);
              tag_reg (reg, exp);
            end;
        end;

      rneg_op:
        begin
          if desc.precision > srealprec
            then do_move_op (dmovn, treg, exp, 72, false)
            else do_move_op (movn, treg, exp, 36, true);
        end;

      imin_op:
        do_min_max_op (treg, exp, ltc, 36);

      imax_op:
        do_min_max_op (treg, exp, gtc, 36);

      rmin_op:
        do_min_max_op (treg, exp, ltc, precision (exp));

      rmax_op:
        do_min_max_op (treg, exp, gtc, precision (exp));

      float_op:
        do_float_op (treg, exp);        (* too many cases to do inline *)

      trunc_op:
        if precision (operand[1]) = 36
          then do_move_op (fix, treg, exp, 36, false)
          else rt_call (treg, exp, rt_d_trunc);

      round_op:
        begin
          if upperbound (operand) = 1 then begin
            if precision (operand[1]) = 36
              then do_move_op (fixr, treg, exp, 36, false)
              else rt_call (treg, exp, rt_d_round);
          end
          else
            rt_call (treg, exp, rt_r_rnd2);
        end;

      expii_op:
        rt_call (treg, exp, rt_exp_ii);

      expri_op:
        rt_call (treg, exp, rt_exp_ri);

      exprr_op:
        rt_call (treg, exp, rt_exp_rr);

      sqrt_op:
        rt_call (treg, exp, rt_r_sqrt);

      ln_op:
        rt_call (treg, exp, rt_r_ln);

      log_op:
        rt_call (treg, exp, rt_r_log);

      exp_op:
        rt_call (treg, exp, rt_r_exp);

      sin_op:
        rt_call (treg, exp, rt_r_sin);

      arcsin_op:
        rt_call (treg, exp, rt_r_asin);

      sinh_op:
        rt_call (treg, exp, rt_r_sinh);

      cos_op:
        rt_call (treg, exp, rt_r_cos);

      arccos_op:
        rt_call (treg, exp, rt_r_acos);

      cosh_op:
        rt_call (treg, exp, rt_r_cosh);

      tan_op:
        rt_call (treg, exp, rt_r_tan);

      tanh_op:
        rt_call (treg, exp, rt_r_tanh);

      cotan_op:
        rt_call (treg, exp, rt_r_ctan);

      arctan_op:
        begin
          if upperbound (operand) = 1
            then rt_call (treg, exp, rt_r_atan)
            else rt_call (treg, exp, rt_r_atn2);
        end;

      new_op:
        begin
          prep_direct (treg, operand[1]);
          clr_rv;
          mem := argument (operand[1]);
          free (operand[1]);
          reg := get_reg (treg, 36);
          gen_rt (pushj, sb, rt_new);
          gen_rm (arg, reg, mem);
          tag_reg (reg, exp);
        end;

      alc_temp_op:                      (* generate a dynamic temporary *)
        begin
          if (operand[1]^.opcode = immed_ref) and (operand[1]^.item.index = nil) then begin
            temp := get_temp (operand[1]^.item.offset); (* fixed size, don't fiddle with stack *)
            mem := temp^.loc;           (* result is addr of temp *)
            mem.immediate := true;
            tag_loc (mem, exp, nil);
            free (operand[1]);
          end

          else begin                    (* flexible case, extend stack to alloc *)
            clr_rv;                     (* about to consume past the end of the stack *)
            prepare (anyreg, operand[1]);
            reg := get_reg (treg, 36);  (* allocate result register *)
            gen (movei, reg, sb, 1, none);      (* move address of bottom of stack *)
            lock (reg);                 (* don't release to access operand *)
            mem := fetch (operand[1], 18);      (* get size of temp *)
            if mem.immediate
              then begin                        (* operand of adjsp is normally E, no need to make it immediate *)
                mem.immediate := false;
		if prog_options.ki_code_opt then
		  begin gen_rt(jsr,0,rt_inst_sml);
                    gen_rm (adj_sp, sb, mem) 
		  end
                else gen_rm (adjsp, sb, mem) 
              end
              else begin                        (* must get value in reg to access *)
                reg2 := load (anyreg, operand[1], 18);
		if prog_options.ki_code_opt then
		  begin gen_rt(jsr,0,rt_inst_sml);
                    gen_rx (adj_sp, sb, reg2) 
		  end
                else gen_rx (adjsp, sb, reg2) 
              end;
            free (operand[1]);
            unlock (reg);
            tag_reg (reg, exp);
          end;
        end;

      lwc_op:                           (* only character processed here *)
        begin
          reg := dload (treg, operand[1], 36);
          gen_ri (cai+ltc, reg, ord ('A'));
          gen_ri (cai+lec, reg, ord ('Z'));
          gen_ri (skip+awc, 0, 0);
          gen_ri (addi, reg, 40B);
          tag_reg (reg, exp);
        end;

      upc_op:
        begin
          reg := dload (treg, operand[1], 36);
          gen_ri (cai+ltc, reg, ord ('a'));
          gen_ri (cai+lec, reg, ord ('z'));
          gen_ri (skip+awc, 0, 0);
          gen_ri (subi, reg, 40B);
          tag_reg (reg, exp);
        end;

      (* All set operations which are given to this routine for processing
         are short sets. *)

      setcvt_op:
        do_setcvt_op (treg, exp);

      union_op:
        do_union (treg, exp);

      diff_op:
        begin
          reg := do_inst (andcm, treg, operand[1], operand[2], desc.set_length, desc.set_length, [mem_form, double]);
          tag_reg (reg, exp);
        end;

      both_op:
        begin
          reg := do_inst (anda, treg, operand[1], operand[2], desc.set_length, desc.set_length, [mem_form, double, commutative]);
          tag_reg (reg, exp);
        end;

      gen_set_op:
        begin
          if (upperbound (operand) = 0) or (desc.set_length = 0) then begin  (* [], clear the register *)
            reg := get_reg (treg, desc.set_length);
	    for i := 1 to upperbound (operand) do begin
	      prepare (anyreg, operand[i]);
	      free (operand[i])
	    end;
            clear (reg);
            tag_reg (reg, exp);
          end

          else if upperbound (operand) = 1    (* [e], add one element to the set *)
            then gen_singleton_set (treg, exp)

          else (* if upperbound (operand) = 2 then *)
            gen_range_set (treg, exp);  (* [e1..e2], create set of range of elements *)
        end;

      random_op:
        begin
          if upperbound (operand) = 0 then begin
            clr_rv;
            reg := get_reg (treg, precision (exp));
            gen_rt (pushj, sb, rt_rand);
            gen_ri (arg, reg, 0);
          end
          else begin
            prepare (treg, operand[1]);
            clr_rv;
            mem := fetch_direct (operand[1], precision (operand[1]));
            free (operand[1]);
            reg := get_reg (treg, precision (exp));
            gen_rt (pushj, sb, rt_rand_set);
            gen_rm (arg, reg, mem);
          end;
          tag_reg (reg, exp);
        end;

      subr_var_op:
        begin
          prepare (anyreg, operand[1]);
          if upperbound (operand) = 2 then prepare (anyreg, operand[2]);
          reg := dload (treg, operand[1], 36);
          if upperbound (operand) = 2 then begin              (* move display to lhw *)
            lock (reg);
	    mem := locate (operand[2]);
            free (operand[2]);
            unlock (reg);
	    do_move (reg, mem, left_unpadded, 18);
          end;
          tag_reg (reg, exp);
        end;

      index_op:                         (* call string module to compile these *)
        str_index_op (treg, exp);

      verify_op:
        str_verify_op (treg, exp);

      search_op:
        str_search_op (treg, exp);

      str_comp_op:
        str_compare_op (treg, exp);

      ile_op..filne_op,                 (* all comparisions *)
      and_op, or_op, in_op, bnot_op, odd_op:
        reg := load_bool (treg, exp, false);    (* evalute the boolean; result reg is tagged *)

      func_call_op:                     (* simple values only *)
        scl_function_call (exp);        (* records where value placed *)

      time_op:
	rt_status_call (treg, exp, rt_time);

      runtime_op:
	rt_status_call (treg, exp, rt_runtime);

      iostatus_op:
        begin
          if operand[1] = nil then begin
            clr_rv;
            gen_rt (pushj, sb, rt_iostat_last);
          end
          else begin
            prep_direct (treg, operand[1]);
            clr_rv;
            mem := argument (operand[1]);
            free (operand[1]);
            gen_rt (pushj, sb, rt_iostatus);
            gen_rm (arg, 0, mem);
          end;
          reg := get_reg (treg, 36); (* result is in register 1 *)
          gen_rr (move, reg, 1);
          tag_reg (reg, exp);
        end;

      extstatus_op:
	rt_status_call (treg, exp, rt_extstatus);

      mathstatus_op:
	rt_status_call (treg, exp, rt_stat_math);

      prgmstatus_op:
	rt_status_call (treg, exp, rt_stat_program);

      spclstatus_op:
	rt_status_call (treg, exp, rt_stat_special);

      exiostatus_op:
	rt_status_call (treg, exp, rt_stat_exio);

      masked_op:
	rt_call (treg, exp, rt_masked);

      pending_op:
	rt_call (treg, exp, rt_pending);

      filesize_op:
        begin
          prep_direct (treg, operand[1]);
          clr_rv;
          mem := argument (operand[1]);
          free (operand[1]);
          gen_rt (pushj, sb, rt_file_size);
          gen_rm (arg, 0, mem);
          reg := get_reg (treg, 36);
          gen_rr (move, reg, 1);
          tag_reg (reg, exp);
        end;

      extent_op:
        begin
          prep_direct (treg, operand[1]);
          clr_rv;
          mem := argument (operand[1]);
          free (operand[1]);
          gen_rt (pushj, sb, rt_extent);
          gen_rm (arg, 0, mem);
          reg := get_reg (treg, 36);
          gen_rr (move, reg, 1);
          tag_reg (reg, exp);
        end;

      open_op, reset_op, rewrite_op, update_op:
        rt_open_call (treg, exp);

      gen_andif_op, gen_orif_op:
	(* no action *); (* processed by CompileBody *)

      others:
	assert (false)

    end (* case *) ;
  end (* with treg, exp^ *) ;
 end;
$PAGE prep_direct
(* PREP DIRECT performs essentially the same function as "prepare".  However,
   unlike "prepare", it will never generate an indirect address.  This is
   important in at least two situations:

   (1)  Sometimes a double-word operation must be implemented as two single-
        word operations.  For example, set union is implemented with logical
        or instructions, and there is no double-word logical or.  In this case,
        the indirect address of the first word would be useless for accessing
        the second word.

   (2)  Many runtime routines take arguments which reflect the value (rather
        than the location) of an i/f operand tuple.  Examples are the file block
        address in an i/o call and the string length in various string calls.
        These direct arguments are created with the "argument" function, rather
        than with "fetch_direct".  If the actual argument value is in a memory
        location, then an indirect reference to that location will be generated.
        Thus, an expression which will be processed by "argument" must not be an
        indirect reference already. *)

public procedure prep_direct (* treg: reg_selector; exp: expr *);

 var vdesc: val_desc; 
     r: registers;
     maddr, mem: addr_desc;

 begin

  (* "Prepare" will create an indirect reference only for a mem_ref expression
     (never for an addr_ref or immed_ref) which has an index part.  All other
     expressions may be prepared normally. *)

  if (exp^.opcode = mem_ref) andif (exp^.item.index <> nil) then begin
    vdesc := get_value_desc (exp);
    if not vdesc^.generated then begin

      (* If the expression hasn't already been prepared, then it is sufficient
         to make sure the index expression is available in a register, since
         "prepare" will create an indexed reference rather than an indirect
         one whenever possible. *)

      prepare (anyreg, exp^.item.index);
      r := load (anyreg, exp^.item.index, 36);
      prepare (treg, exp);
    end

    else (* vdesc^.generated *) begin
      maddr := locate (exp);
      if maddr.indirect then begin

        (* If the expression has already been prepared, and it is an indirect
           address, then we must load the address into a register and change
           the reference to an indexed reference.  I.e., if the expression is
           now "@X", then we generate "move r, X" and change the expression to
           "0(r)".  In the process, we must dissociate the expression from
           any index register it may currently be using. *)

        if vdesc^.loc_index <> nil then
          detach_expr (vdesc); (* dissociate fromm index register *)
        mem := absolute_reference; (* create the new reference *)
        mem.index := get_reg (anyreg, 36); (* make it an index reference *)
        maddr.indirect := false; (* we want to load the *address* *)
        do_move (mem.index, maddr, unsigned_value, 36); (* load it *)
        tag_loc (mem, exp, nil); (* note the new address *)
      end;
    end;
  end

  else (* normal case *) prepare (treg, exp);
 end;
$PAGE prep_set_addr
(* The first operand of a long set desc_ref tuple evaluates to the word address
   of the set.  When the set is to be processed with a runtime call, this is
   what we want.  However, to process a set in-line, we need to be able to treat
   it as a memory operand.  PREP SET ADDR may be called with such a set address
   tuple, and will return a memory reference for the set itself.

   If the set address is an immediate reference, then the set memory reference
   may be obtained simply by marking the address as direct.  If the set address
   is direct, then we must load it into a register and use "0(reg)" as the memory
   reference.  By preparing the set address with "prep_direct", we can make sure
   that it is never indirect. *)

public function prep_set_addr (treg: reg_selector; exp: expr): addr_desc;

 var maddr, mem: addr_desc;

 begin
  prep_direct (treg, exp);
  maddr := locate (exp);
  if maddr.immediate then begin
    maddr.immediate := false;
    prep_set_addr := maddr;
  end
  else begin
    mem := absolute_reference; (* create the memory reference *)
    mem.index := get_reg (anyreg, 36); (* make it an indexed reference *)
    do_move (mem.index, maddr, unsigned_value, 36); (* load it *)
    prep_set_addr := mem;
  end;
 end;
$PAGE load
(* LOAD loads a value ("exp") into a register.  A register preference may be
   specified ("treg"), and if reasonable, the value will be placed in that
   register - though if the value is already in a register, it will not be
   moved.  The size of the desired result is specified ("p"), and in the case
   of integer values, sign extension is performed when necessary.  The register
   loaded is returned. *)

public function load (* treg: reg_selector; exp: expr; p: bit_range): registers *) ;

 var
   reg: registers;
   maddr: addr_desc;

 begin

  (* At this point, we assume that "exp" has been "prepare"ed for access.  Thus
     its location can be obtained from the value descriptor.  If the value is in
     memory, then it is loaded; otherwise the register in which it resides is
     used directly. *)

  maddr := locate (exp);
  assert (not addr_equal (maddr, null_location));
  if is_register (maddr)
   then reg := maddr.offset
   else begin                                   (* must load value *)
     consume (exp);                             (* permit an index register to be allocate for load *)
     reg := get_reg (treg, p);                  (* now allocate reg *)
     do_move (reg, maddr, alignment (exp), p);
     tag_reg (reg, exp);                        (* remember that exp is in this register *)
     if exp^.opcode = display_op then
       regdesc[reg]^.loc.mode := rhw;
   end;

  (* If this is an integer expression, then the value must be sign extended if
     the value has halfword precision, is not extended, and the target precision
     is a fullword.  If so, the value is extended in place. *)

  if (p = 36) and (regdesc[reg]^.loc.mode <> fw)        (* only scalars have other than fw mode *)
    then extend_reg (reg);

  load := reg;                                  (* return the number of the register loaded *)
 end;
$PAGE load_reg
(* LOAD REG loads a value ("exp") into a specific register ("treg").  If the target
   register is loaded with another value, that value is saved in memory, before
   "exp" is loaded.  The size of the desired result ("p") is specified, and in
   the case of simple values, sign extension is performed when required. *)

public procedure load_reg (* treg: registers; exp: expr; p: bit_range *);

 var
   reg: registers;
   maddr: addr_desc;
   is_reg: boolean;

 begin
  maddr := locate (exp);        (* find out where value currently is *)
  is_reg := is_register (maddr);

  (* If the value is already in the specified register, then the only action which
     may be required is sign extension. *)

  if not (is_reg andif (maddr.offset = treg)) then begin
    consume (exp);                      (* free any regs not needed after move *)
    clr_reg (treg);                     (* save any values occupying the target regs *)
    if p > 36 then clr_reg (treg+1);
    reg := get_reg (treg, p);           (* allocate the target reg *)

    if is_reg                           (* move value to target *)
      then move_reg (treg, maddr.offset)
      else do_move (treg, maddr, alignment (exp), p);
    tag_reg (treg, exp);                (* remember that we put it there *)
  end;

  (* If this is an integer expression, then the value must be sign extended if
     (1) the value has halfword precision, (2) is not currently extended, and (3)
     the target precision is a fullword. If so, the value is extended in place. *)

  if (p = 36) and (regdesc[treg]^.loc.mode <> fw)       (* only scalars have other than fw mode *)
    then extend_reg (reg);
 end;
$PAGE fetch
(* FETCH generates an addr_desc for a value ("exp") which is to be used as the
   memory operand of an instruction.  It insures that the operand is addressible.
   This may entail one of three actions:  (1) Loading an index expression into an
   index register.  (2) Loading operands with a byte address into a register so
   that the value can be accessed.  (3) Sign extending halfword scalar values
   when a full word result is required ("p" gives the bit precision needed for the
   operand).  *)

public function fetch (* exp: expr; p: bit_range): addr_desc *) ;

 var
   reg: registers;
   is_reg: boolean;                     (* true if addr is register *)

 begin

  (* Obtain the location of "exp" from its value descriptor.  If the value is
     a reference to a byte of memory, then the value must be loaded into a
     register in order to be accessed by the instruction.  However, if the
     value is in the rhw and an 18 bit operand is required, then the word can
     be referenced directly, at the penalty of trashing the lhw of the result. *)

  fetch := locate (exp);        (* this loads the index register *)
  is_reg := is_register (fetch);        (* remember if address is register addr *)

  if not is_reg then begin      (* check if load required *)
    if (fetch.mode <> fw) and not ((fetch.mode = rhw) and (p = 18)) then begin
      consume (exp);                    (* permit load of index reg *)
      reg := get_reg (anyreg, 36);
      do_move (reg, fetch, alignment (exp), 36);
      tag_reg (reg, exp);       (* record that the value is in the register *)
      fetch := locate (exp);                    (* get register address *)
      is_reg := true;
    end;
  end;

  (* If this is a scalar valued expression, then compatibility of the expression
     with the desired result size must be checked, and the value extended if
     necessary.  Note that since referencing a register as an index register is
     faster than referencing it as a memory location, an attempt is made to turn
     a register reference into immediate indexed reference. *)

  if exp^.desc.kind in [bools, ints, scalars, chars] then begin
    if is_reg then begin        (* deal with value in register *)
      reg := fetch.offset;
      if (not exp^.desc.signed) and (exp^.desc.int_prec = 18) then begin
        fetch := immediate_reference;   (* implicit extension gives full word *)
        fetch.index := reg;
      end

      else if p = 18 then begin (* eighteen bits suffice *)
        fetch := immediate_reference;
        fetch.index := reg;
        if exp^.desc.signed then fetch.mode := rhw;
      end

      else if (fetch.mode <> fw) and (p = 36) then begin
        extend_reg (reg);
        fetch.mode := fw;
      end;
    end         (* register case *)
  end;
 end;
$PAGE fetch_direct
(* FETCH DIRECT performs the same function as "fetch" except that an immediate
   reference is not returned. *)

public function fetch_direct (* exp: expr; p: bit_range): addr_desc *) ;
 var reg: registers;
     constval: val;
 begin
  fetch_direct := fetch (exp, p);
  with fetch_direct do begin
    if immediate then begin
      if (index = 0) and (reloc.kind <> temp_sc) and (mode = fw) then begin
        constval.kind := scalar_cst; (* generate a constant word reference *)
        constval.ival := offset;
        immediate := false;
        offset := 0;
        reloc := gen_cval (constval);
      end
      else begin (* must load an immediate computed value *)
        consume (exp);                  (* permit load of index register *)
        reg := get_reg (anyreg, 36);
        do_move (reg, fetch_direct, alignment (exp), 36);
        tag_reg (reg, exp);
        fetch_direct := locate (exp);
      end;
    end;
  end;
 end;
$PAGE argument
(* ARGUMENT creates a runtime argument descriptor for the value represented by
   some tuple.  The routine can be used with any halfword argument, which will
   be loaded by a runtime routine with "movei reg, @descword".  Thus, any of
   the following argument forms may be generated:

        ARG     const           for a constant argument
        ARG     const(reg)      for arg = const + reg
        ARG     0(reg)          for an argument in a register
        ARG     @location       for an argument somewhere in memory

   This routine is commonly used to pass a length word argument (since these are
   frequently immediate).  It is also used to pass an address when the address
   has been created from an addr_ref tuple in the i/f.  For example, both the
   byte pointer address and the string length arguments in an X-format string
   argument descriptor are created here.

   Note that expressions which will be passed to this routine must be prepared
   with prep_direct, not with prepare.  This is because prepare can generate
   indirect addresses, and there is no way to pass a doubly indirect address as
   an argument.  For example, the value at location X can be passed with "ARG @X",
   but there is no way to pass the value at location @X. *)

public function argument (* arg: expr ): addr_desc *);

 var reg: registers;

 begin
  prep_direct (anyreg, arg); (* make sure it's direct *)
  argument := fetch (arg, 18); (* get the argument address *)

  (* If the argument is a constant or a register plus a constant, it will be
     marked as immediate.  The immediate reference is implicit in the runtime
     load, however, so it may be suppressed here. *)

  if argument.immediate then
    argument.immediate := false

  (* Otherwise, we have the address of the argument.  If the argument is not a
     fullword, we must load it into a register.  We can then pass 0(reg) as the
     argument descriptor. *)

  else if argument.mode <> fw then begin
    reg := load (anyreg, arg, 18);
    argument := absolute_reference;
    argument.index := reg;
  end

  (* If the argument is already in a register, we can simply used 0(reg) for the
     argument descriptor. *)

  else if argument.reloc.kind = register_sc then begin
    reg := argument.offset;
    argument := absolute_reference;
    argument.index := reg;
  end

  (* All else failing, we are reduced to making an indirect reference to the
     memory location containing the argument.  To do this, we change our
     current address into @addr. *)

  else
    argument.indirect := true;

 end.
   -
$^