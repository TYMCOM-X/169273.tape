$TITLE p10cmp - comparision and jump code generation
$LENGTH 42

module p10cmp;

$PAGE includes
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE ptmcon.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$INCLUDE p10cgu.inc
$INCLUDE p10exp.inc
$INCLUDE p10opc.inc
$INCLUDE p10dsc.inc
$PAGE opcode mapping tables

type maptype = array [cmp_codes] of cmp_codes;

(* Table of the inverse of each opcode, e.g. maps "<" to ">=" *)

const inverse: maptype := 
        (  awc,    (* nvc *)
           gec,    (* ltc *)
           nec,    (* eqc *)
           gtc,    (* lec *)
           nvc,    (* awc *)
           ltc,    (* gec *)
           eqc,    (* nec *)
           lec   );        (* gtc *)

(* Table of the reverse of each opcode, i.e. the corresponding comparision if
   the operands are switched. *)

const reverse: maptype :=
        (  nvc, (* nvc *)
           gtc, (* ltc *)
           eqc, (* eqc *)
           gec, (* lec *)
           awc, (* awc *)
           lec, (* gec *)
           nec, (* nec *)
           ltc  );      (* gtc *)

(* Table of comparision subopcodes for each IF opcode *)

type subopctype = array[ile_op..filne_op] of cmp_codes;

const subopcodes: subopctype :=
        (  lec,    (* ile_op *)
           ltc,    (* ilt_op *)
           gtc,    (* igt_op *)
           gec,    (* ige_op *)
           eqc,    (* ieq_op *)
           nec,    (* ine_op *)
           lec,    (* rle_op *)
           ltc,    (* rlt_op *)
           gtc,    (* rgt_op *)
           gec,    (* rge_op *)
           eqc,    (* req_op *)
           nec,    (* rne_op *)
           lec,    (* sle_op *)
           ltc,    (* slt_op *)
           gtc,    (* sgt_op *)
           gec,    (* sge_op *)
           eqc,    (* seq_op *)
           nec,    (* sne_op *)
           lec,    (* setle_op *)
           gec,    (* setge_op *)
           eqc,    (* seteq_op *)
           nec,    (* setne_op *)
           eqc,    (* ptreq_op *)
           nec, (* ptrne_op *)
           eqc, (* fileq_op *)
           nec  ); (* filne_op *)
$PAGE bit masks

type bmask_table = array[0..17] of integer;     (* bmask[n] mask for bit n *)

const bmasks: bmask_table :=
     (  400000B,
        200000B,
        100000B,
        040000B,
        020000B,
        010000B,
        004000B,
        002000B,
        001000B,
        000400B,
        000200B,
        000100B,
        000040B,
        000020B,
        000010B,
        000004B,
        000002B,
        000001B  );
$PAGE reverse_compare
(* REVERSE COMPARE examines the operands of a comparision and decides which
   should be the register operand and which should be the memory operand.  The
   choice is reflected by the returned flag which implies that the original
   order should be reversed.  It is assumed that the left operand (op1) is
   normally placed in the register, and the right operand (op2), referenced in
   memory.  The choice is based on heuristics intended to reduce the number of
   register loads. *)

function reverse_compare (op1, op2: expr): boolean;

  var
    addr1, addr2: addr_desc;
    use1, use2: usage_cnt;

begin

  (* If one of the operands is a byte reference, then it will have to be loaded
     into a register in order to perform the comparision; therefore, it is a
     logical choice to be the register operand. *)

  addr1 := locate (op1);
  addr2 := locate (op2);

  if addr1.mode <> fw then begin
    reverse_compare := false;
    return;
  end
  else if addr2.mode <> fw then begin
    reverse_compare := true;
    return;
  end;

  (* Select for the memory operand, one which is an immediate reference and
     therefore does not need to be loaded. *)

  if addr1.immediate then begin
    reverse_compare := true;
    return;
  end
  else if addr2.immediate then begin
    reverse_compare := false;
    return;
  end;

  (* Select as the register operand, one which is already in a register *)

  if is_register (addr1) then begin
    reverse_compare := false;
    return;
  end
  else if is_register (addr2) then begin
    reverse_compare := true;
    return;
  end;

  (* Select as the register operand, one which has multiple uses and is therefore
     likely to be used again. *)

  if usage (op1) > 1 then begin
    reverse_compare := false;
    return;
  end
  else if usage (op2) > 1 then begin
    reverse_compare := true;
    return;
  end;

  (* One choice seems no better than another, use original order *)

  reverse_compare := false;
end;
$PAGE compare_with_zero
(* COMPARE WITH ZERO examines an operand of an arithmetic conditional the relation
   to be tested for, and determines if the compare can be done with zero.  This
   includes direct comparisions with zero, and special cases of compares with
   1 and -1.  A flag is returned indicating whether or not a zero compare can be
   performed.  If the flag is true, the relation subopcode is also returned, and
   the constant operand is freed. *)

function compare_with_zero
     (  op1: expr;                      (* test is of form op1 :: op2 *)
        cmp: cmp_codes;                 (* original relational operator: op1 :: x *)
        var tcmp: cmp_codes             (* operator for zero comparision: op2 :: 0 *)
                ): boolean;

 var val: int_type;

 begin
  with op1^ do begin    (* test for a constant value *)
    if (opcode = immed_ref) andif (item.index = nil)
      then val := item.offset
    else if (opcode = mem_ref) andif (item.class = constant_sc)
        andif (item.cstref^.kind = scalar_cst)
      then val := item.cstref^.scalar_val
    else begin
      compare_with_zero := false;       (* <---- exit if op1 not constant *)
      return;
    end;
  end (* with *) ;

  if val = 0
    then tcmp := reverse [cmp]
  else if (val = 1) and (cmp = gtc)
    then tcmp := lec
  else if (val = 1) and (cmp = lec)
    then tcmp := gtc
  else if (val = -1) and (cmp = ltc)
    then tcmp := gec
  else if (val = -1) and (cmp = gec)
    then tcmp := ltc
  else begin
    compare_with_zero := false;         (* <---- exit if special case do not apply *)
    return;
  end;

  free (op1);                           (* count off a use *)
  compare_with_zero := true;
 end;
$PAGE in_long_set
(* IN LONG SET emits inline code to test for a constant in a long set
   or a runtime call otherwise. *)

function in_long_set (test: expr; negate: boolean; treg: registers): registers;

var
    reg: registers;
    maddr: addr_desc;
    cval, ix: int_type;

begin
  with test^ do begin
    if operand[2]^.desc.set_cst_lwb and operand[2]^.desc.set_cst_len and
       iconstp (operand[1], cval) andif (cval >= operand[2]^.desc.set_lwb) andif
       (cval < operand[2]^.desc.set_length + operand[2]^.desc.set_lwb) then begin
      if treg = noreg then begin
	maddr := prep_set_addr (anyreg, operand[2]^.operand[1]);
	reg := 1;
      end
      else begin
	maddr := prep_set_addr (treg, operand[2]^.operand[1]);
	reg := get_reg (treg, 36);
      end;
      maddr.offset := maddr.offset + (cval - operand[2]^.desc.set_lwb) div 36;
      cval := (cval - operand[2]^.desc.set_lwb) mod 36;
      if cval < 18
	then gen_ri (hrlzi, reg, bmasks[cval])
	else gen_ri (hrrzi, reg, bmasks[cval-18]);
      if negate
	then gen_rm (tdne, reg, maddr)
	else gen_rm (tdnn, reg, maddr);
      free (operand[1]);
      for ix := 1 to 3 do
	free (operand[2]^.operand[ix]);
      free (operand[2]);
      if treg <> noreg then begin
	gen_ri (tdza, reg, reg);
	gen_ri (movei, reg, 1)
      end
      else bb_end;
    end

    else (* not constant value *) begin
      if treg = noreg then
        reg := get_reg (anyreg, 36)
      else
	reg := get_reg (treg, 36);
      ls_in_operator (rt_in_vl, operand[1], operand[2], reg);
      if treg = noreg then begin
	lock (reg);
	bb_end;
	unlock (reg);
        if negate
          then gen_ri (skipe, 0, reg)
	  else gen_ri (skipn, 0, reg);
	free_reg (regdesc [reg])
      end
      else
        if negate then
          gen_ri (andcai, reg, 1);
    end;
  end (* with test^ *);
  in_long_set := reg;
end;
$PAGE test_and_skip
(* TEST AND SKIP performs an arbtrary logical test (comparision, in, boolean test,
   etc.) generating a skip if the condition is either true or false.  If the
   flag "branching" is true, then code is being generated for a jump_op; if such
   is the case, the routine "bb_end" will be called to perform any actions
   required on exit to the basic block. *)

procedure test_and_skip
  (  cond: expr;                     (* boolean valued predicate -- the test *)
     negate: boolean;             (* implies that skip condition is (not test) *)
     branching: boolean  );     (* this is code for a jump_op *)


  (* These routine contains four local procedures that generate various
     cases of skips:

        compare_single

        compare_double

        compare_zero_skip

        compare_arith

     All examine the global parameter "branching". *)
$PAGE compare_single - in test_and_skip
(* COMPARE SINGLE performs an arithmetic comparision of two values of precision
   36 or less.  If the comparision is true, a skip is generated. *)

procedure compare_single
  (  cmp: cmp_codes;         (* kind of comparision to perform *)
     op1, op2: expr  );               (* values to compare *)

  var
    tcmp: cmp_codes;              (* (possibly reversed) cmp *)
    reg: registers;               (* the register and memory operands *)
    mem: addr_desc;

begin
  prepare (anyreg, op1);        (* make the operands addressible *)
  prepare (anyreg, op2);

  if reverse_compare (op1, op2) (* select register and memory operands *)
    then begin
      tcmp := reverse [cmp];
      reg := load (anyreg, op2, 36);    (* value in reg must be full word *)
      lock (reg);
      mem := fetch (op1, 36);
    end
    else begin
      tcmp := cmp;
      reg := load (anyreg, op1, 36);
      lock (reg);
      mem := fetch (op2, 36);
    end;

  mem_lock (mem);
  free (op1); (* count off a use of each operand *)
  free (op2);
  if branching then
    bb_end;
  unlock (reg);
  mem_unlock (mem);

  gen_rm (cam+tcmp, reg, mem);      (* camxx reg,mem *)
end;
$PAGE compare_double - in test_and_skip
(* COMPARE DOUBLE performs and arithmetic comparision of two values of precision
   72 (i.e. a double word).  The code emitted will skip if the condition is true. *)

procedure compare_double
  (  cmp: cmp_codes;         (* kind of comparision to perform *)
     op1, op2: expr    );                  (* values to compare *)

  var
    tcmp: cmp_codes;              (* (possibly reversed) cmp *)
    reg: registers;               (* the register and memory operands *)
    mem: addr_desc;

begin
  prep_direct (anyreg, op1);        (* make the operands addressible *)
  prep_direct (anyreg, op2);

  if reverse_compare (op1, op2) (* select register and memory operands *)
    then begin
      tcmp := reverse [cmp];
      reg := load (anyreg, op2, 72);
      lock (reg);
      mem := fetch (op1, 72);
    end
    else begin
      tcmp := cmp;
      reg := load (anyreg, op1, 72);
      lock (reg);
      mem := fetch (op2, 72);
    end;

  mem_lock (mem);
  free (op1);         (* count off a use of each operand *)
  free (op2);
  if branching then
    bb_end;
  unlock (reg);
  mem_unlock (mem);

  case tcmp of (* generate the comparison *)

    eqc:
      begin
        gen_rm (cam+nec, reg, mem);
        mem.offset := mem.offset + 1;
        gen_rm (cam+eqc, reg+1, mem);
      end;

    nec:
      begin
        gen_rm (cam+nec, reg, mem);
        mem.offset := mem.offset + 1;
        gen_rm (cam+eqc, reg+1, mem);
        gen (jrst, 0, 0, 2, dot);
      end;

    lec, ltc, gec, gtc:
      begin
        if tcmp in [lec, ltc]
          then gen_rm (cam+gec, reg, mem)
          else gen_rm (cam+lec, reg, mem);
        gen (jrst, 0, 0, 4, dot);
        gen_rm (cam+nec, reg, mem);
        mem.offset := mem.offset + 1;
        gen_rm (cam+tcmp, reg+1, mem);
      end

  end (* case *) ;
end;
$PAGE compare_zero_skip - in test_and_skip
(* COMPARE ZERO SKIP compares a single precision scalar value with zero and skips
   if a specified relation is satisfied (exp :: 0). *)

procedure compare_zero_skip
     (  cmp: cmp_codes;         (* the relation to test for *)
        exp: expr       );                      (* the value to test *)

var
  mem: addr_desc;
  reg: registers;

begin
  prepare (anyreg, exp);                (* get the value *)
  mem := fetch_direct (exp, 36);
  mem_lock (mem);

  if is_register (mem) orif (usage (exp) = 1) then
    reg := 0
  else begin
    reg := get_reg (anyreg, 36);
    tag_reg (reg, exp);
  end;
  free (exp);

  lock (reg);
  if branching then
    bb_end;
  unlock (reg);
  mem_unlock (mem);

  gen_rm (skip+cmp, reg, mem);
end;
$PAGE compare_arith - in test_and_skip
(* COMPARE ARITH performs an arithmetic comparision of two values.  If a
   specified condition is satistied, the code will cause a skip to occur. *)

procedure compare_arith
  (  cmp: cmp_codes;         (* kind of comparision to perform *)
     op1, op2: expr;                  (* values to compare *)
     p: bit_range         );           (* precision of value *)

var tcmp: cmp_codes;
begin
  if p <= 36 then begin         (* 1 word comparision *)
    if compare_with_zero (op1, cmp, tcmp)
      then compare_zero_skip (tcmp, op2)
    else if compare_with_zero (op2, reverse [cmp], tcmp)
      then compare_zero_skip (tcmp, op1)
    else compare_single (cmp, op1, op2);
  end

  else begin                    (* double word comparision *)
    compare_double (cmp, op1, op2)
  end;
end;
$PAGE test_and_skip - main routine

  var
    cmp: cmp_codes;
    reg: registers;

begin
  with cond^ do begin
    case opcode of

      mem_ref, immed_ref, func_call_op:
        ;

      ile_op..ine_op:
        begin
          cmp := subopcodes [opcode];
          if negate then cmp := inverse [cmp];
          compare_arith (cmp, operand[1], operand[2], desc.int_prec);
        end;

      rle_op..rne_op:
        begin
          cmp := subopcodes [opcode];
          if negate then cmp := inverse [cmp];
          if operand[1]^.desc.precision > srealprec
            then compare_arith (cmp, operand[1], operand[2], 72)
            else compare_arith (cmp, operand[1], operand[2], 36);
        end;

      ptreq_op, ptrne_op, fileq_op, filne_op:
        begin
          cmp := subopcodes [opcode];
          if negate then cmp := inverse [cmp];
          compare_arith (cmp, operand[1], operand[2], 18);
        end;

      odd_op:
        begin
          prepare (anyreg, operand[1]);
          reg := load (anyreg, operand[1], 18);
          free (operand[1]);
          if branching then begin
            lock (reg);
            bb_end;
            unlock (reg);
          end;
          if negate
            then gen_ri (trne, reg, 1)
            else gen_ri (trnn, reg, 1);
        end;

      sle_op..sne_op:
        begin   (* operand is "sign" of comparision *)
          cmp := subopcodes [opcode];
          if negate then cmp := inverse [cmp];
          compare_zero_skip (cmp, operand[1]);
        end;

      setle_op, setge_op:       (* only need process long sets *)
        begin
	  if branching then
	    reg := get_reg (anyreg, 36)
	  else
	    reg := 1;
          if opcode = setge_op
            then ls_operator (rt_sle_ll, operand[2], operand[1], reg)
            else ls_operator (rt_sle_ll, operand[1], operand[2], reg);
	  if branching then begin
	    lock (reg);
	    bb_end;
	    unlock (reg);
	    free_reg (regdesc [reg])
	  end;
          if negate then cmp := nec else cmp := eqc;
          gen_rr (skip+cmp, 0, reg);      (* operator places 0 in reg if true *)
        end;

      seteq_op, setne_op:
        begin
          cmp := subopcodes [opcode];
          if negate then cmp := inverse [cmp];
          if operand[1]^.opcode <> desc_ref then begin  (* short compare *)
            compare_arith (cmp, operand[1], operand[2], operand[1]^.desc.set_length);
          end
          else begin                            (* long compare *)
	    if branching then
	      reg := get_reg (anyreg, 36)
	    else
	      reg := 1;
            ls_operator (rt_seq_ll, operand[1], operand[2], reg);
	    if branching then begin
	      lock (reg);
	      bb_end;
	      unlock (reg);
	      free_reg (regdesc [reg])
	    end;
            gen_rr (skip+cmp, 0, reg);    (* operator leaves 0 in reg if sets equal *)
          end;
        end
    end (* case *) ;
  end (* with *) ;
end;
$PAGE bool_compare_zero
(* BOOL COMPARE ZERO generates a boolean value for a comparision ("cmp") of
   some integer value ("exp") against zero.  The methods vary for each comparision
   but all involve insidious bit pattern hacks. *)

function bool_compare_zero ( treg: reg_selector; cmp: cmp_codes; exp: expr ): registers;

 var
   reg: registers;
   mem: addr_desc;

 begin
  case cmp of

    ltc:
      begin
        reg := dload (treg, exp, 36);
        gen_ri (lsh, reg, -35);
      end;

    lec:
      begin
        reg := dload (treg, exp, 36);
        gen_ri (subi, reg, 1);
        gen_ri (lsh, reg, -35);
      end;

    gtc:
      begin
        prepare (treg, exp);
        mem := fetch (exp, 36);
        free (exp);
        reg := get_reg (treg, 36);
        gen_rm (movn, reg, mem);
        gen_ri (lsh, reg, -35);
      end;

    gec:
      begin
        reg := dload (treg, exp, 36);
        gen_ri (lsh, reg, -35);
        gen_ri (andcai, reg, 1);
      end;

    eqc:
      begin
        reg := dload (treg, exp, 36);
        gen_rr (tdzn, reg, reg);
        gen_ri (movei, reg, 1);
      end;

    nec:
      begin
        prepare (anyreg, exp);
        mem := fetch_direct (exp, 36);
        free (exp);
        reg := get_reg (treg, 36);
        gen_rip+eqc, reg, mem);
        gen_ri (movei, reg, 1);
      end

  end (* case *) ;
  bool_compare_zero := reg;
 end;
$PAGE bool_skip_compare, bool_single_compare
(* BOOL SKIP COMPARE evaluates a boolean test by generating a skip-load one-
   load zero sequence.  This returns the register in which the result is placed. *)

function bool_skip_compare (treg: reg_selector; test: expr; negate: boolean): registers;
 var reg: registers;
 begin
  test_and_skip (test, not negate, false);
  reg := get_reg (treg, 36);
  gen (skip+awc, reg, 0, 0, gen_cint (1));
  gen_ri (setz, reg, 0);
  bool_skip_compare := reg;
 end;



(* BOOL SINGLE COMPARE performs a "test" on single word values, special casing
   compares with zero.  "Negate" indicates a negative test. *)

function bool_single_compare (treg: reg_selector; test: expr; negate: boolean): registers;
 var cmp, tcmp: cmp_codes;
 begin
  with test^ do begin
    cmp := subopcodes [opcode];
    if negate then cmp := inverse [cmp];
    if compare_with_zero (operand[1], cmp, tcmp)
      then bool_single_compare := bool_compare_zero (treg, tcmp, operand[2])
    else if compare_with_zero (operand[2], reverse [cmp], tcmp)
      then bool_single_compare := bool_compare_zero (treg, tcmp, operand[1])
    else bool_single_compare := bool_skip_compare (treg, test, negate);
  end (* with *) ;
 end;
$PAGE ld_bool_prepared
(* LD BOOL PREPARED loads a prepared boolean value into a register. *)

function ld_bool_prepared ( treg: reg_selector; test: expr; negate: boolean ): registers;

begin
  if negate then begin
    ld_bool_prepared := dload (treg, test, 18);
    gen_ri (andcai, ld_bool_prepared, 1);
  end
  else
    ld_bool_prepared := load (treg, test, 36);
end;
$PAGE load_bool
(* LOAD BOOL evaluates a boolean expression and leaves the result in a register.
   A register preference may be specified, and the boolean expression may be
   negated. *)

public function load_bool (treg: reg_selector; test: expr; negate: boolean): registers;

  var
    reg, reg1: registers;
    mem1, mem2: addr_desc;
    cmp, tcmp: cmp_codes;
    len: set_range;
    opc: opc_range;
    cval: int_type;
    vdesc: val_desc;
    prev_prepared: boolean;

begin
  with test^ do begin
    case opcode of

      mem_ref, immed_ref, func_call_op, gen_andif_op, gen_orif_op,
      masked_op, pending_op:
        begin
          prepare (treg, test);
          reg := ld_bool_prepared (treg, test, negate);
        end;

      bnot_op:
        begin
          vdesc := get_value_desc (operand[1]);
          prev_prepared := vdesc^.generated;
          if prev_prepared
            then reg := ld_bool_prepared (treg, operand[1], not negate)
            else reg := load_bool (treg, operand[1], not negate);
          if (not negate) and
             ( prev_prepared or
               (operand[1]^.opcode = mem_ref) or
               (operand[1]^.opcode = immed_ref) or
               (operand[1]^.opcode = func_call_op) or
               (operand[1]^.opcode = gen_andif_op) or
               (operand[1]^.opcode = gen_orif_op) ) then
            (* operand[1] was "dload"ed; no "free" needed *)
          else
            free (operand[1]);
        end;

      odd_op:
        begin
          prepare (treg, operand[1]);
          reg := dload (treg, operand[1], 18);
          if negate
            then gen_ri (andcai, reg, 1)
            else gen_ri (andi, reg, 1);
        end;

      or_op:
        begin
          reg := do_inst (ior, treg, operand[1], operand[2], 18, 18, [commutative, mem_form]);
          if negate then gen_ri (andcai, reg, 1);
        end;

      and_op:
        begin
          reg := do_inst (anda, treg, operand[1], operand[2], 18, 18, [commutative, mem_form]);
          if negate then gen_ri (andcai, reg, 1);
        end;

      orif_op, andif_op:
        ;                               (* should not reach code generator *)

      ile_op..ine_op:
        reg := bool_single_compare (treg, test, negate);

      sle_op..sne_op:
        begin
          opc := subopcodes[opcode];
          if negate then opc := inverse[opc];
          reg := bool_compare_zero (treg, opc, operand[1]);
        end;

      seteq_op, setne_op:
        begin
          if operand[1]^.desc.set_length <= 36
            then reg := bool_single_compare (treg, test, negate)
            else reg := bool_skip_compare (treg, test, negate);
        end;

      setle_op, setge_op:
        begin
          if operand[1]^.desc.set_length > 72   (* long set compare *)
            then reg := bool_skip_compare (treg, test, negate)
          else begin                    (* short set compare *)
            if opcode = setle_op then opc := andcm else opc := andca;
            len := operand[1]^.desc.set_length; (* both op's converted to same length *)
            reg := do_inst (opc, treg, operand[1], operand[2], len, len, [mem_form, double]);
            if len > 36 then begin                              (* check for both words zero *)
              gen_rr (ior, reg, reg+1);
              free_reg (regdesc[reg]);          (* need only one word result *)
              reg := get_reg (reg, 36);
            end;
            if negate           (* generate a boolean result *)
              then gen (jump+eqc, reg, 0, 2, dot)
              else gen_rr (tdzn, reg, reg);
            gen_ri (movei, reg, 1);
          end;
        end;

      in_op:
        begin
          if operand[2]^.opcode = desc_ref      (* long set case *)
            then reg := in_long_set (test, negate, treg)

          else if (operand[2]^.opcode = set_op) then begin      (* must be 2 operand form set_op *)
            prepare (anyreg, operand[1]);
            prepare (anyreg, operand[2]^.operand[1]);
            prepare (anyreg, operand[2]^.operand[2]);
            reg1 := load (anyreg, operand[1], 36);
            lock (reg1);
            mem1 := fetch (operand[2]^.operand[1], 36);
            mem_lock (mem1);
            mem2 := fetch (operand[2]^.operand[2], 36);
            unlock (reg1);
            mem_unlock (mem1);
            free (operand[1]);
            free (operand[2]^.operand[1]);
            free (operand[2]^.operand[2]);
            reg := get_reg (treg, 36);
            gen_rm (cam+ltc, reg1, mem1);
            gen_rm (cam+lec, reg1, mem2);
            if negate
              then begin
                gen (skip+awc, reg, 0, 0, gen_cint (1));
                gen_ri (setz, reg, 0);
              end
              else begin
                gen_ri (tdza, reg, reg);
                gen_ri (movei, reg, 1);
              end;
          end

          else if iconstp (operand[1], cval)    (* const in short set *)
            andif (operand[2]^.desc.set_lwb <= cval)
            andif (cval <= (operand[2]^.desc.set_lwb + operand[2]^.desc.set_length - 1)) then begin
            prepare (anyreg, operand[2]);
            reg1 := load (anyreg, operand[2], operand[2]^.desc.set_length);
            cval := cval - operand[2]^.desc.set_lwb;    (* index of element to test *)
            if cval >= 36 then begin                    (* in second register *)
              cval := cval - 36;
              reg1 := reg1 + 1;
            end;
            free (operand[1]);
            free (operand[2]);
            if cval <= 17
              then if not negate
                then gen_ri (tlnn, reg1, bmasks [cval])
                else gen_ri (tlne, reg1, bmasks [cval])
              else if not negate
                then gen_ri (trnn, reg1, bmasks [cval-18])
                else gen_ri (trne, reg1, bmasks [cval-18]);
            reg := get_reg (treg, 36);
            gen_ri (tdza, reg, reg);
            gen_ri (movei, reg, 1);
          end

          else begin                            (* x in short set *)
            prepare (anyreg, operand[1]);
            prepare (anyreg, operand[2]);
            reg1 := load (anyreg, operand[1], operand[1]^.desc.int_prec);
            lock (reg1);
            reg := dload (anyreg, operand[2], operand[2]^.desc.set_length);
            unlock (reg1);
            if operand[1]^.desc.int_prec > 18   then begin      (* if range check not implicit in shift *)
              gen_rm (cam+ltc, reg1, int_value (operand[2]^.desc.set_lwb));
              gen_rm (cam+lec, reg1, int_value (operand[2]^.desc.set_lwb +
                                               operand[2]^.desc.set_length - 1));
              clear (reg);                      (* zero reg1 so that set has no elements *)
            end;
            if operand[2]^.desc.set_length <= 36
              then gen (lsh, reg, reg1, -operand[2]^.desc.set_lwb - 35, none)
              else gen (lshc, reg, reg1, -operand[2]^.desc.set_lwb - 35, none);
            free (operand[1]);
            free_reg (regdesc[reg]);    (* alloc *single* register for boolean result *)
            reg := get_reg (reg, 36);
            if negate   (* isolate selected bit *)
              then gen_ri (andcai, reg, 1)
              else gen_ri (andi, reg, 1);
          end;
        end;

      others:
        reg := bool_skip_compare (treg, test, negate)

    end (* case *) ;
  end (* with *) ;

  if not negate then
    tag_reg (reg, test);                        (* mark test as residing in reg *)
  load_bool := reg;
end;
$PAGE bool_value
(* BOOL VALUE returns a flag indicating whether or not an expression ("exp") is
   a boolean value reference.  True is returned if (1) the node is a boolean
   variable, or (2) the node is a multi-use boolean expression.  This is used
   to determine when to compute (i.e. load) the value of the expression when
   it is encountered.  In the case of boolean expression, the multi-use criteria
   is applied since the value of single-use nodes may be tested implicitly via
   skip/jump instructions, and since evaluation of a multi-use node at first
   encounter is assumed by the optimizer.  Note that it is assumed that the node
   is (or has been converted to) type boolean. *)

function bool_value (exp: expr): boolean;
 begin
  bool_value := (exp^.opcode = mem_ref) orif
                (exp^.opcode = immed_ref) orif
                (exp^.opcode = func_call_op) orif
		(exp^.opcode = masked_op) orif
		(exp^.opcode = pending_op) orif
                (exp^.usage_count > 1);
 end;
$PAGE test_zero_jump
(* TEST ZERO JUMP compares a single precision value with zero (exp :: 0), and if
   a specified condtion ("cmp") is met, jumps to a specified label ("loc"),
   otherwise execution continues at the following location.  There are two code
   sequences that can be used: a load-jump or skip-jrst.  Either requires the
   same number of instructions -- unless the value is already loaded, in which
   case the jump sequence is better.  If not, the skip-jrst case is generally
   preferable as it does not destroy a register. *)

procedure test_zero_jump (cmp: cmp_codes; exp: expr; loc: tuple);

 var
   mem: addr_desc;
   reg: registers;

 begin
  prepare (anyreg, exp);
  mem := fetch_direct (exp, 36);                        (* get the value as a full width operand *)

  (* Load the operand into a register if it has more than one remaining use
     on the assumption that it will be used at a latter time. *)

  if usage (exp) > 1 then begin
    reg := load (anyreg, exp, 36);
    free (exp);
    lock (reg);
    bb_end;
    unlock (reg);
    gen_rl (jump+cmp, reg, loc);
  end

  else if is_register (mem) then begin  (* use a jump *)
    free (exp);
    lock (mem.offset);
    bb_end;
    unlock (mem.offset);
    gen_rl (jump+cmp, mem.offset, loc);
  end

  else begin                            (* use a skip, so as to save a register *)
    free (exp);
    mem_lock (mem);
    bb_end;
    mem_unlock (mem);
    gen_rm (skip+inverse [cmp], 0, mem);
    gen_rl (jrst, 0, loc);
  end;
 end;
$PAGE test_single_and_jump
(* TEST SINGLE AND JUMP evaluates a "test" on single word values and jumps to
   "tloc" or "floc" depending on whether the test is true or false.  "Skip_cond"
   indicates on what condition a jump should occur and on what condition a the
   code should continue.  Skip_cond = true => jump if false; false => jump if
   true. *)

procedure test_single_and_jump (test: expr; skip_cond: boolean; tloc, floc: tuple);
 var tcmp: cmp_codes;
 begin
  with test^ do begin
    if compare_with_zero (operand[1], subopcodes [opcode], tcmp)
      then begin
        if skip_cond
          then test_zero_jump (inverse [tcmp], operand[2], floc)
          else test_zero_jump (tcmp, operand[2], tloc);
      end
    else if compare_with_zero (operand[2], reverse [subopcodes [opcode]], tcmp)
      then begin
        if skip_cond
          then test_zero_jump (inverse [tcmp], operand[1], floc)
          else test_zero_jump (tcmp, operand[1], tloc);
      end
    else begin
      test_and_skip (test, not skip_cond, true);
      if skip_cond
        then gen_rl (jrst, 0, floc)
        else gen_rl (jrst, 0, tloc);
    end;
  end (* with *) ;
 end;
$PAGE test_and_jump
(* TEST AND JUMP generates code to perform an arbitrary test and jump to
   specified locations on the outcome of the test. *)

public procedure test_and_jump
  (  test: expr;                     (* the test to evaluate *)
     tloc: tuple;                    (* label node to goto if test true *)
     floc: tuple;                    (* label node to goto if test false *)
     nloc: tuple   );                (* label node following jump, if known; else nil *)

  var
    skip_cond: boolean;                  (* skip on state *skip_cond* gotos to nloc (if <> nil) *)
    tcmp: cmp_codes;
    reg, reg2: registers;
    mem1, mem2: addr_desc;
    len: set_range;
    opc: opc_range;
    cval: int_type;

begin

  (* Select the condition in which we 'fall through' to the nloc label. This
     will be used to bias the sense of the jumps performed. *)

  if tloc = nloc then skip_cond := true
  else if floc = nloc then skip_cond := false
  else skip_cond := true;              (* nloc is neither tloc or floc, arbitrary selection okay *)

  (* If the node to be tested is a boolean value (variable or multi-use expression),
     then load the value and test it against zero. *)

  if bool_value (test) then begin
    if skip_cond
      then test_zero_jump (eqc, test, floc)     (* jump if false, continue if true *)
      else test_zero_jump (nec, test, tloc);
  end

  (* If this is not a value, then perform the indicated comparision. *)

  else begin
    free (test);
    with test^ do begin
      case opcode of

        ile_op..ine_op, ptreq_op, ptrne_op, fileq_op, filne_op:
          test_single_and_jump (test, skip_cond, tloc, floc);

        seteq_op, setne_op:
          begin
            tcmp := subopcodes [opcode];
            if (operand[1]^.opcode = desc_ref)
              then begin
		reg := get_reg (anyreg, 36);
                ls_operator (rt_seq_ll, operand[1], operand[2], reg);  (* leaves 0 in reg if equal *)
		lock (reg);
		bb_end;
		unlock (reg);
		free_reg (regdesc [reg]);
                if skip_cond
                  then gen_rl (jump+inverse[tcmp], reg, floc)
                  else gen_rl (jump+tcmp, reg, tloc);
              end
            else if operand[1]^.desc.set_length <= 36
              then test_single_and_jump (test, skip_cond, tloc, floc)
            else begin
              test_and_skip (test, not skip_cond, true);
              if skip_cond
                then gen_rl (jrst, 0, floc)
                else gen_rl (jrst, 0, tloc);
            end;
          end;

        slt_op, sle_op, sne_op, seq_op, sge_op, sgt_op:
          begin (* operand is str_comp_op giving "sign" of comparision *)
            if skip_cond
              then test_zero_jump (inverse [subopcodes [opcode]], operand[1], floc)
              else test_zero_jump (subopcodes [opcode], operand[1], tloc);
          end;

        setle_op, setge_op:
          begin
            if operand[1]^.opcode = desc_ref then begin (* long set case *)
              test_and_skip (test, not skip_cond, true);
              if skip_cond
                then gen_rl (jrst, 0, floc)
                else gen_rl (jrst, 0, tloc);
            end
            else begin                  (* short set case *)
              if opcode = setle_op then opc := andcm else opc := andca;
              len := operand[1]^.desc.set_length;       (* following yields zero if test true *)
              reg := do_inst (opc, anyreg, operand[1], operand[2], len, len, [mem_form, double]);
              free_reg (regdesc[reg]);
              lock (reg);
              bb_end;
              unlock (reg);
              if len > 36               (* must check both words *)
                then gen_rl (jump+nec, reg+1, floc);
              if skip_cond
                then gen_rl (jump+nec, reg, floc)
                else gen_rl (jump+eqc, reg, tloc);
            end;
          end;

        in_op:
          begin
	    if (operand[2]^.opcode = gen_set_op) and (upperbound (operand[2]^.operand) = 0) then begin
	      prepare (anyreg, operand[1]);
	      prepare (anyreg, operand[2]);
	      free (operand[1]);
	      free (operand[2]);
	      bb_end;
	      if skip_cond
		then gen_rl (jrst, 0, floc)
		else gen_rl (jrst, 0, tloc)
	    end
  
            else if operand[2]^.opcode = desc_ref then begin (* long set case *)
              reg := in_long_set (test, not skip_cond, noreg);
              if skip_cond
                then gen_rl (jrst, 0, floc)
                else gen_rl (jrst, 0, tloc);
            end

            else if (operand[2]^.opcode = set_op) then begin    (* x in [op1..op2] *)
              prepare (anyreg, operand[1]);
              prepare (anyreg, operand[2]^.operand[1]);
              prepare (anyreg, operand[2]^.operand[2]);
              reg := load (anyreg, operand[1], 36);
              lock (reg);
              mem1 := fetch (operand[2]^.operand[1], 36);
              mem_lock (mem1);
              mem2 := fetch (operand[2]^.operand[2], 36);
              free (operand[1]);
              free (operand[2]^.operand[1]);
              free (operand[2]^.operand[2]);
              mem_lock (mem2);
              bb_end;
              unlock (reg);
              mem_unlock (mem1);
              mem_unlock (mem2);
              gen_rm (cam+ltc, reg, mem1);
              gen_rm (cam+lec, reg, mem2);
              gen_rl (jrst, 0, floc);
              skip_cond := true;                (* can only do it this one way *)
            end

            else if iconstp (operand[1], cval)  (* const in short set *)
              andif (operand[2]^.desc.set_lwb <= cval)
              andif (cval <= (operand[2]^.desc.set_lwb + operand[2]^.desc.set_length - 1)) then begin
              prepare (anyreg, operand[2]);
              reg := load (anyreg, operand[2], operand[2]^.desc.set_length);
              cval := cval - operand[2]^.desc.set_lwb;  (* index of element to test *)
              if cval >= 36 then begin                  (* in second register *)
                cval := cval - 36;
                reg := reg + 1;
              end;
              free (operand[1]);
              free (operand[2]);
              lock (reg);
              bb_end;
              unlock (reg);
              if cval <= 17
                then if skip_cond
                  then gen_ri (tlnn, reg, bmasks [cval])
                  else gen_ri (tlne, reg, bmasks [cval])
                else if skip_cond
                  then gen_ri (trnn, reg, bmasks [cval-18])
                  else gen_ri (trne, reg, bmasks [cval-18]);
              if skip_cond
                then gen_rl (jrst, 0, floc)
                else gen_rl (jrst, 0, tloc);
            end

            else begin                          (* x in short set *)
              prepare (anyreg, operand[1]);
              prepare (anyreg, operand[2]);
              reg := load (anyreg, operand[1], operand[1]^.desc.int_prec);
              lock (reg);
              reg2 := dload (anyreg, operand[2], operand[2]^.desc.set_length);
              unlock (reg);
              if operand[1]^.desc.int_prec > 18 then begin      (* if range check not implicit in shift *)
                gen_rm (cam+ltc, reg, int_value (operand[2]^.desc.set_lwb));
                gen_rm (cam+lec, reg, int_value (operand[2]^.desc.set_lwb +
                                                 operand[2]^.desc.set_length - 1));
                clear (reg2);                   (* zero reg so that set has no elements *)
              end;
              if operand[2]^.desc.set_length <= 36
                then gen (lsh, reg2, reg, -operand[2]^.desc.set_lwb, none)
                else gen (lshc, reg2, reg, -operand[2]^.desc.set_lwb, none);
              free (operand[1]);
              free_reg (regdesc[reg2]);
              lock (reg2);
              bb_end;
              unlock (reg2);
              if skip_cond
                then gen_rl (jump+gec, reg2, floc)
                else gen_rl (jump+ltc, reg2, tloc);
            end;
          end;

        others:
          begin
            test_and_skip (test, not skip_cond, true);
            if skip_cond
              then gen_rl (jrst, 0, floc)
              else gen_rl (jrst, 0, tloc);
          end

      end (* case *) ;
    end (* with *) ;
  end (* else *) ;

  if skip_cond and (tloc <> nloc)               (* if true case falls through *)
    then gen_rl (jrst, 0, tloc);                (* then branch to true label if not alrealy there *)
end.
   K^dV]