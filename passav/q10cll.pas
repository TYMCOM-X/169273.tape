$TITLE q10cll - Subroutine Call Utilities (Quick Pass)

module q10cll;
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$system q10dsc.typ
$INCLUDE p10cgu.inc
$INCLUDE p10opc.inc
$INCLUDE q10exp.inc
$INCLUDE pastal.inc
$system q10dsc.inc
$system q10gen.inc
$system q10set.inc
$PAGE save_regs
(* SAVE REGS saves any allocated expression registers prior to 
   parameter list evaluation, returning the addr of the temporary
   used for the save. The saved registers are freed. *)

const
    free_reg: reg_status := (0, 0);

function save_regs: addr_desc;

var
    reg: registers;
    maddr, mem: addr_desc;
    temp_size: code_address;

begin

  (* first scan registers to determine required temp size *)

  reg := 2;
  temp_size := 0;
  while reg < with_base do with regdesc[reg] do begin
    if uses_remaining <> 0 then begin
      if (reg+1 < with_base) andif (regdesc[reg+1].uses_remaining <> 0) then begin
        temp_size := temp_size + 2;
        reg := reg + 1;
      end
      else temp_size := temp_size + 1;
    end;
    reg := reg + 1;
  end;

  (* get the temp and save the regs *)

  maddr := get_temp (temp_size);
  mem := maddr;
  reg := 2;
  while reg < with_base do with regdesc[reg] do begin
    if uses_remaining <> 0 then begin
      if (reg+1 < with_base) andif (regdesc[reg+1].uses_remaining <> 0) then begin
        gen_rm (dmovem, reg, mem);
        regdesc[reg] := free_reg;
        regdesc[reg+1] := free_reg;
        mem.offset := mem.offset + 2;
        reg := reg + 1;
      end
      else begin
        gen_rm (movem, reg, mem);
        mem.offset := mem.offset + 1;
        regdesc[reg] := free_reg;
      end;
    end;
    reg := reg + 1
  end;

  (* return addr of temps *)

  save_regs := maddr;
end;
$PAGE merge_with_regs
(* MERGE WITH REGS updates the saved register states since with register
   usage counts may have changed in evaluation of parameter expressions. *)

procedure merge_with_regs (var old_state: reg_descriptor; new_state: reg_descriptor);

var
  reg: registers;

begin
  for reg := with_base to 14b do
    old_state[reg].uses_remaining := new_state[reg].uses_remaining
end;
$PAGE restore_regs
(* RESTORE REGS reloads the expression registers saved by SAVE REGS.  *)

procedure restore_regs (prev_regs: reg_descriptor; reg_temp: addr_desc);

var
  reg: registers;
  maddr: addr_desc;

begin
  reg := 2;
  maddr := reg_temp;
  while reg < with_base do with prev_regs[reg] do begin
    if uses_remaining <> 0 then begin
      if (reg+1 < with_base) andif (prev_regs[reg+1].uses_remaining <> 0) then begin
        gen_rm (dmove, reg, maddr);
        maddr.offset := maddr.offset + 2;
        reg := reg + 1;
      end
      else begin (* single reg *)
        gen_rm (move, reg, maddr);
        maddr.offset := maddr.offset + 1;
      end;
    end;
    reg := reg + 1;
  end;

  (* restore with index registers *)

  with_restore;

  (* restore registers' state *)

  regdesc := prev_regs;
end;
$PAGE pf_access
(* PF ACCESS loads into a register the address of a procedure or function,
   along with its parent frame pointer, if any. *)

public procedure pf_access ( routine: tuple; var reg: registers; allocated: boolean );

var
  source, taddr: addr_desc;
  levels_dif, i: -1..maximum (level_index);

begin
  source := fetch (routine);
  free (source);
  if not allocated then (* allocate result register *)
    reg := get_reg (bits_per_unit);

  (* If the source is a procedure/function variable it can simply be
     loaded into a register. *)

  if (routine^.opcode <> cst_ref) and ( (routine^.opcode <> ident_ref) orif
					      (routine^.id_sym^.kind <> consts) ) then
      gen_rm (move, reg, source)
  else begin

    (* If the routine has no parent then merely load its address. *)

    if (source.reloc.kind = external_sc) orif
       (routine^.cst_val.blkp^.subr_sym^.block^.level <= 1) then
      gen_rm (movei, reg, source)
    else begin  (* must load its parent pointer *)
      levels_dif := cur_block^.apparent_level - routine^.cst_val.blkp^.apparent_level;
      if levels_dif = -1 then   (* called by its parent *)
        gen_rr (hrlz, reg, sp)
      else begin
        taddr := absolute_reference;
        taddr.index := sp;
        taddr.offset := 1;
        if levels_dif = 0 then  (* same parent as caller *)
          gen_rm (hllz, reg, taddr)
        else begin
          gen_rm (hlrz, reg, taddr);
          taddr.index := reg;
          for i := 1 to levels_dif - 1 do       (* chase static chain *)
            gen_rm (hlrz, reg, taddr);
          gen_rm (hllz, reg, taddr);
        end;
      end;
      gen_rm (hrri, reg, source);
    end;
  end;
end;
$PAGE pas_call
(* PAS CALL generates code to call a pascal procedure or function.  The parameter
   "node" is the call_op or func_call_op node for which code is to be generated.
   Parameter "ret_type" indicates whether the routine is a structured 
   function and, if so, whether the result is to be placed in "rv_expr"
   (as in aggregate functions) or in "rv_addr" (as for string temporaries). *)

type
  return_type = (no_return_value, expr_return_value, addr_return_value);

public procedure pas_call (
        node: expr;             (* routine to be called *)
        ret_type: return_type;  (* selects either or neither of *)
        rv_expr: expr;          (* an expression result destination *)
        rv_addr: addr_desc);    (* or an allocated temporary *)

 var
   subr_type: typ;
   saddr_def: rel_syllable;
   saddr, taddr: addr_desc;
   reg_temp: addr_desc;      (* addr of saved registers *)
   prevregs: reg_descriptor;
   pl_temp, pl_loc: addr_desc;
   i: parm_range;
   parm: typ;
   reg: registers;
   rv_reg: registers;
$PAGE get_parameter
(* GET PARAMETER does the dirty work of fetching a parameter or its
   address.     *)

procedure get_parameter (exp: expr;     (* the parameter expression *)
        parm: param_desc;               (* the declared parameter *)
        short_form: boolean;    (* if all parameters in registers *)
        var reg: registers;     (* reg in which to load it (if short) *)
        var loc: addr_desc);    (* load of parameter temp if not short *)

type
    dynamic_allocation = (bounded, flexible, generic);

  function parm_allocation (t: typ): dynamic_allocation;
  begin
    if t^.generic then
      parm_allocation := generic
    else if t^.flexible then
      parm_allocation := flexible
    else parm_allocation := bounded;
  end;

var
    mem: addr_desc;
    r: registers;
    dest_allocation, source_allocation: dynamic_allocation;
    on_heap: boolean;
    rsize: registers;


begin
  if parm.parm_type^.kind = strings then begin
    if short_form then
      if parm.parm_type^.flexible
        then reg := get_reg (72)
        else reg := get_reg (36);
    str_parameter (exp, parm, reg);
    if parm.parm_type^.flexible
      then rsize := 2
      else rsize := 1;
  end
  else if parm.parm_type^.kind = arrays then begin      (* might be flex/generic *)
    dest_allocation := parm_allocation (parm.parm_type);
    source_allocation := parm_allocation (exp^.desc.base);

    (* make the expression addressible *)

    mem := fetch (exp);
    free (mem);

    (* determine if a flexible array/string on the heap *)

    on_heap := dynamic_flex (exp);

    (* must perform different actions depending on the types of the
       expected and actual parameter types *)

    case dest_allocation of
      bounded: begin    (* an honest array/string *)
        case source_allocation of
          bounded:      (* just pass its address *)
            gen_rm (movei, reg, mem);
          flexible: begin       (* must extract address *)
            if on_heap then begin       (* must correct addr desc *)
              if mem.indirect then begin
                gen_rm (movei, reg, mem);       (* address on heap *)
                gen_ri (addi, reg, 1);          (* skip upb word *)
              end
              else begin
                mem.offset := mem.offset + 1;   (* skip upb word *)
                gen_rm (movei, reg, mem);
              end;
            end (* if on the heap *)
            else begin
              mem.indirect := false;
              gen_rm (move, reg, mem);  (* pick up actual address *)
            end
          end;
          generic:      (* address already available *) begin
            mem.indirect := false;
            gen_rm (move, reg, mem)
          end
        end;
        rsize := 1;
      end (* bounded parameter type *);

      flexible: begin
        case source_allocation of
          bounded: begin
            gen_rm (movei, reg+1, mem); (* addr *)
            gen_rm (move, reg, upper_bound (exp, mem)); (* and const upb *)
          end;
          flexible, generic: begin      (* pass upb as well as address *)
            if on_heap then begin
              if mem.indirect then begin
                gen_rm (movei, reg+1, mem);
                gen_ri (addi, reg+1, 1);
              end
              else begin
                mem.offset := mem.offset + 1;
                gen_rm (movei, reg+1, mem);
              end;
              gen (move, reg, reg+1, -1, none); (* get stored upb *)
            end
            else begin
              mem.indirect := false;
              mem.offset := mem.offset - 1;
              gen_rm (dmove, reg, mem);
            end;
          end (* flexible parameter type *)
        end (* case on actual type *);
        rsize := 2;
      end (* required flexible *);

    generic: begin      (* must pass lwb, upb, and addr *)
      case source_allocation of
        bounded: begin  (* constant lwb and upb *)
          gen_rm (movei, reg+2, mem);   (* get address first *)
          gen_rm (move, reg, lower_bound (exp, mem));
          gen_rm (move, reg+1, upper_bound (exp, mem));
        end;
        flexible: begin
          if on_heap then begin
            if mem.indirect then begin
              gen_rm (movei, reg+2, mem);
              gen_ri (addi, reg+2, 1);
            end
            else begin
              mem.offset := mem.offset + 1;
              gen_rm (movei, reg+2, mem);
            end;
            gen (move, reg+1, reg+2, -1, none); (* stored upb wourd *)
          end
          else begin    (* pick up upb and addr at same time *)
            mem.indirect := false;
            mem.offset := mem.offset - 1;
            gen_rm (dmove, reg+1, mem);
          end;
          gen_rm (move, reg, lower_bound (exp, mem));
        end (* flexible to generic conversion *);
        generic: begin  (* lwb, upb and addr already known - just load them *)
          mem.indirect := false;
          if mem.index = reg then begin (* careful of index register *)
            mem.offset := mem.offset - 1;
            gen_rm (dmove, reg+1, mem); (* get upb and addr *)
            mem.offset := mem.offset - 1;
            gen_rm (move, reg, mem);    (* and lwb *)
          end
          else begin
            mem.offset := mem.offset - 2;
            gen_rm (move, reg, mem);    (* get lwb *)
            mem.offset := mem.offset + 1;
            gen_rm (dmove, reg+1, mem); (* and upb and addr *)
          end;
        end (* generic case *)
      end (* case on actual type *);
      rsize := 3;
     end (* generic type required *)
    end (* case on required type *)
  end (* if array or string *)
  else begin    (* everything else *)
    if parm.parm_type^.kind = sets then begin
      set_parameter (exp, parm, reg, short_form);
      rsize := 1 + ord (regdesc[reg].associate <> noreg);
    end
    else if parm.parm_type^.kind in [procs, funcs] then begin
      pf_access (exp, reg, short_form);
      rsize := 1;
    end
    else if (parm.parm_kind = vars) orif p_b_address (parm.parm_type) then begin
      mem := fetch (exp);
      free (mem);
      if not short_form then
        reg := get_reg (36);
      gen_rm (movei, reg, mem);
      rsize := 1;
    end
    else begin
      rsize := word_size (exp);
      if short_form then
        load_reg (reg, exp)
      else begin
        reg := load (exp, rsize*36);
        decr_reg_usages (reg);
      end;
    end;
  end;
  if short_form then begin      (* assure that parameter regs are allocated *)
    for r := reg to reg+rsize-1 do
      regdesc[r].uses_remaining := 1;
    reg := reg + rsize;
  end
  else begin    (* store in temp *)
    case rsize of
      1: begin
        gen_rm (movem, reg, loc);
        loc.offset := loc.offset + 1;
      end;
      2: begin
        gen_rm (dmovem, reg, loc);
        loc.offset := loc.offset + 2;
      end;
      3: begin
        gen_rm (dmovem, reg, loc);
        loc.offset := loc.offset + 2;
        gen_rm (movem, reg+2, loc);
        loc.offset := loc.offset + 1;
      end
    end;
    for r := reg to reg + rsize - 1 do
      regdesc[r] := free_reg;
  end;
end;
$PAGE pas_call - body 
 var lev, levels_dif: integer;

 begin
  prevregs := regdesc;  (* previous register state *)
  reg_temp := save_regs;
  with node^ do begin
    subr_type := subr^.desc.base;               (* info about parameter list *)

    (* When the parameter list occupies 6 words or less, the parameters are passed
       in registers 2-7. *)

    reg := 2;                                   (* load parameters into registers *)
    if subr_type^.parmlist_size <= 6 then begin
      for i := 1 to upperbound (arglist) do begin
        get_parameter (arglist[i], subr_type^.params[i], true, reg, pl_loc);
      end;
      rv_reg := reg;                            (* one past last parm reg *)
    end

    (* When there are more than 6 words in the parameter list, generate the argument
       list in memory and pass a pointer to the start of the list in register 2. *)

    else if upperbound (subr_type^.params) <> 0 then begin
      pl_loc := get_temp (subr_type^.parmlist_size);    (* get a place to store the argument list *)
      pl_temp := pl_loc;

      for i := 1 to upperbound (arglist) do begin              (* copy arguments to arglist *)
        get_parameter (arglist[i], subr_type^.params[i], false, reg, pl_loc);
      end;

      gen_rm (movei, 2, pl_temp);
      rv_reg := 3;                              (* one past last parm reg *)
    end

    else rv_reg := 2;                           (* no parameters, place rv_addr in first parm reg *)

    (* If the subroutine is a function returning an aggregate value, the address
       where the function should place its result is placed in the first register
       after the registers used by the parameter list. *)

    if ret_type = expr_return_value then begin
      taddr := fetch_fullword (rv_expr);
      free (taddr);
      gen_rm (movei, rv_reg, taddr);
    end
    else if ret_type = addr_return_value then begin
      gen_rm (movei, rv_reg, rv_addr);
    end;

    (* Generate code for the actual call.  In the first case, we have a direct
       call to a subroutine constant - either a local or external subroutine id. *)

    saddr := fetch (subr);
    free (saddr);

    merge_with_regs (prevregs, regdesc);

    (* check for subroutine variable *)

    if (subr^.opcode <> cst_ref) and ( (subr^.opcode <> ident_ref) orif
				       (subr^.id_sym^.kind <> consts) ) then begin
      gen_rm (move, 1, saddr);  (* get the subroutine variable *)
      gen_rr (hll, sp, 1);
      gen_rx (pushj, sb, 1);    (* actual call *)
    end
    else begin  (* must be an actual address *)
      if (saddr.reloc.kind <> external_sc) andif   (* if local to this module *)
         (subr^.cst_val.blkp^.level > 2) andif   (* and not level 1 *)
         (subr^.cst_val.blkp^.owner = subr^.cst_val.blkp) then begin (* and not quick *)
	levels_dif := cur_block^.apparent_level - subr^.cst_val.blkp^.apparent_level;
	if levels_dif = -1 then (* called by parent *)
	  saddr.offset := saddr.offset - 1
	else if levels_dif = 0 then (* called by sibling *)
	  gen (hll, sp, sp, 1, none)
	else begin (* called by nephew *)
	  gen (hlrz, 1, sp, 1, none);
	  for lev := 1 to levels_dif - 1 do
	    gen (hlrz, 1, 1, 1, none);
	  gen (hll, sp, 1, 1, none);
	end;
      end;
      gen_rm (pushj, sb, saddr);
    end;

  (* restore saved registers *)

  restore_regs (prevregs, reg_temp);
  end (* with *) ;
 end;
$PAGE for_call
(* FORTRAN CALL generates a call to a fortran procedure or function.  The parameter
   "node" is the call_op or func_call_op node. *)

procedure for_call (node: expr);

 var
   pl_temp: addr_desc;
   pl_loc: addr_desc;
   i: parm_range;
   mem, mem2: addr_desc;
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
       also passed, with a copy in storage created if necessary.  It is assumed that
       the fortran subroutine does not modify these parameters. *)

    pl_temp := get_temp (upperbound (arglist)+1);              (* get place to put addresses *)
    pl_loc := pl_temp;

    gen_ri (hrlzi, 1, -upperbound (arglist));          (* generate count word *)
    gen_rm (movem, 1, pl_loc);
    pl_loc.offset := pl_loc.offset + 1;

    for i := 1 to upperbound (arglist) do begin                (* fill in address words *)

      if arglist[i]^.desc.kind = pointers then begin    (* pass address of var parameter *)
        reg := load (arglist[i], 36);
        gen_rm (movem, reg, pl_loc);
   decr_reg_usages (reg);
      end

      else begin                                (* pass value parameter *)
        mem := fetch_fullword (arglist[i]);
        free (mem);
        if is_register (mem)
          then begin                            (* must get a copy in register *)
            mem2 := get_temp (word_size (arglist[i]));
            if word_size (arglist[i]) > 1
              then gen_rm (dmovem, mem.offset, mem2)
              else gen_rm (movem, mem.offset, mem2);
            mem := mem2;
          end;
        gen_rm (movei, 1, mem); (* get the address *)
        gen_rm (movem, 1, pl_loc);      (* store in parm list *)
      end;

      pl_loc.offset := pl_loc.offset + 1;
    end (* for *) ;

    (* Generate the call *)

    saddr_def.kind := external_sc;
    saddr_def.relsym := subr^.id_sym;
    gen_rr (push, sb, sp);              (* 16 is used as arg list pointer *)
    if node^.opcode <> func_call_op then        (* save PDA pointer for fortran procedures *)
      gen_rr (push, sb, 15b);
    pl_temp.offset := pl_temp.offset + 1;
    gen_rm (movei, sp, pl_temp);
    gen (pushj, sb, 0, 0, saddr_def);   (* generate the call *)
    if node^.opcode <> func_call_op then
      gen_rr (pop, sb, 15b);    (* restore 15 *)
    gen_rr (pop, sb, sp);               (* restore 16, saved in gen_arg_list *)

  end (* with *) ;
 end;
$PAGE procedure_call
(* PROCEDURE CALL compiles code for a call operator "node". *)

public procedure procedure_call (node: tuple);
var taddr: addr_desc;
 begin
  if node^.subr^.desc.base^.fortran_call
    then for_call (node)
    else pas_call (node, no_return_value, nil, taddr);
 end;
$PAGE scl_function_call
(* SCL FUNCTION CALL compiles code for a func_call_op "node" whose result is
   of size two words or less, e.g. scalars and reals. *)

public function scl_function_call (node: expr): addr_desc;
var size: int_type;
    reg: registers;
    taddr: addr_desc;
 begin

  (* A Pascal subroutine leaves its return value on the stack in a location which
     is just beyond the end of the stack frame of the calling subroutine.  This
     location is termed the 'return value slot'.  The contents of the slot must
     be specially managed to insure that the value is not destroyed before it
     can be used or moved.      *)

  size := word_size (node);
  if not node^.subr^.desc.base^.fortran_call then begin
    pas_call (node, no_return_value, nil, taddr);
    taddr := absolute_reference;
    if (node^.subr^.opcode = cst_ref) andif (node^.subr^.cst_val.blkp^.owner <> node^.subr^.cst_val.blkp) then begin
      taddr.index := sp;
      taddr.offset := node^.subr^.cst_val.blkp^.return_sym^.item_addr;
    end
    else begin
      taddr.index := sb;
      taddr.offset := 5;
    end;
    scl_function_call := reg_addr (get_reg (size * 36));
    if size > 1
      then gen_rm (dmove, scl_function_call.offset, taddr)
      else gen_rm (move, scl_function_call.offset, taddr);
  end

  (* A fortran subroutine leaves its return value in register 0 (or 0-1 pair).
     As we assume that these registers are unused, the value is immediately
     stored in a temporary location. *)

  else begin
    for_call (node);
    scl_function_call := reg_addr (get_reg (size*36));
    if size > 1
      then gen_rm (dmovem, 0, scl_function_call)
      else gen_rm (movem, 0, scl_function_call);
  end;
 end.
  3A­