$TITLE q10cmp - comparision and jump code generation
$LENGTH 42

module q10cmp;

$PAGE includes
$INCLUDE pascal.inc
$INCLUDE ptmcon.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE p10cg.typ
$INCLUDE p10cgu.inc
$INCLUDE q10exp.inc
$INCLUDE p10opc.inc
$system q10dsc.typ
$system q10dsc.inc
$system q10set.inc
$system q10gen.inc
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
           eqc,    (* setle_op *)
           eqc,    (* setge_op *)
           eqc,    (* seteq_op *)
           nec,    (* setne_op *)
           eqc,    (* ptreq_op *)
           nec, (* ptrne_op *)
           eqc, (* fileq_op *)
           nec  ); (* filne_op *)
$PAGE reverse_compare
(* REVERSE COMPARE examines the operands of a comparision and decides which
   should be the register operand and which should be the memory operand.  The
   choice is reflected by the returned flag which implies that the original
   order should be reversed.  It is assumed that the left operand (op1) is
   normally placed in the register, and the right operand (op2), referenced in
   memory.  The choice is based on heuristics intended to reduce the number of
   register loads. *)

function reverse_compare (addr1, addr2: addr_desc): boolean;

begin

  (* If one of the operands is a byte reference, then it will have to be loaded
     into a register in order to perform the comparision; therefore, it is a
     logical choice to be the register operand. *)

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

  (* One choice seems no better than another, use original order *)

  reverse_compare := false;
end;
$PAGE const_comparison
(* CONST COMPARISON returns true just when C1 stands in the specified relation to C2. *)
  
  
function const_comparison (c1: int_type; cmp: cmp_codes; c2: int_type): boolean;
  
begin
  const_comparison := false;
  case cmp of
    nvc: ;
    ltc: if c1 < c2  then const_comparison := true;
    eqc: if c1 = c2  then const_comparison := true;
    lec: if c1 <= c2 then const_comparison := true;
    awc: const_comparison := true;
    gec: if c1 >= c2 then const_comparison := true;
    nec: if c1 <> c2 then const_comparison := true;
    gtc: if c1 > c2  then const_comparison := true;
    others: assert (false)
  end
end;
$PAGE compare_with_zero
(* COMPARE WITH ZERO examines an operand of an arithmetic conditional the relation
   to be tested for, and determines if the compare can be done with zero.  This
   includes direct comparisions with zero, and special cases of compares with
   1 and -1.  A flag is returned indicating whether or not a zero compare can be
   performed.  If the flag is true, the relation subopcode is also returned, and
   the constant operand is freed. *)

function compare_with_zero (op1: expr;          (* test is of form op1 :: op2 *)
                            cmp: cmp_codes;                     (* original relational operator: op1 :: x *)
                            var tcmp: cmp_codes         (* operator for zero comparision: op2 :: 0 *)
                                               ): boolean;

 var val: int_type;

 begin
  if not iconstp (op1, val) then begin  (* not a constant *)
    compare_with_zero := false;
    return;     (* <---- return since not a const *)
  end;

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

  compare_with_zero := true;
 end;
$PAGE prep_sets
(* PREP SET checks the lowerbound and length of the two sets under comparison
   and establishes limits for the operation. *)

procedure prep_sets (    op:             tuple_opcodes;
                         l_op, r_op:     expr;
                     var l_set, r_set:   set_desc;
                     var lwb, len:       set_range);

var
  temp1_lwb, temp1_len, temp2_lwb, temp2_len: set_range;
  
begin
  shape_set (l_op, temp1_lwb, temp1_len);
  shape_set (r_op, temp2_lwb, temp2_len);
  if op = setle_op then begin
    lwb := temp1_lwb;
    len := temp1_len;
    temp2_len := temp1_len
  end
  else if op = setge_op then begin
    lwb := temp2_lwb;
    len := temp2_len;
    temp1_len := temp2_len
  end
  else begin
    lwb := min (temp1_lwb, temp2_lwb);
    temp1_len := temp1_lwb + temp1_len - lwb;
    temp2_len := temp2_lwb + temp2_len - lwb;
    len := max (temp1_len, temp2_len);
    if len <= 72 then (* short *) begin
      temp1_len := len;
      temp2_len := len
    end;
  end;
  if (not l_op^.desc.set_cst_len and not r_op^.desc.set_cst_len) andif
      ((l_op^.desc.set_length > set_dynamic_cutover) and (r_op^.desc.set_length > set_dynamic_cutover)) then begin
    len := len + lwb;
    temp1_len := temp1_len + lwb;
    temp2_len := temp2_len + lwb;
    lwb := 0 (* avoid conversions - both will be built with
                zero basing anyway, so just accept it *)
  end;
  
  l_set := set_fetch (l_op, lwb, temp1_len);
  make_set (l_set, lwb, temp1_len, false);
  if stemps_dynamic then
    free (rtime_upb);
  r_set := set_fetch (r_op, lwb, temp2_len);
  make_set (r_set, lwb, temp2_len, false);
  if stemps_dynamic then
    free (rtime_upb)
end;
$PAGE test_and_skip
(* TEST AND SKIP performs an arbtrary logical test (comparision, in, boolean test,
   etc.) generating a skip if the condition is either true or false.  If the
   flag "branching" is true, then code is being generated for a jump_op; if such
   is the case, the routine "bb_end" will be called to perform any actions
   required on exit to the basic block. *)

procedure test_and_skip (cond: expr; (* boolean valued predicate -- the test *)
                         negate: boolean; (* implies that skip condition is (not test) *)
                         branching: boolean  ); (* this is code for a jump_op *)


  (* These routine contains three local procedures that generate various
     cases of skips:

        compare_single

        compare_double

        compare_arith

     All examine the global parameter "branching". *)
$PAGE compare_single - in test_and_skip
(* COMPARE SINGLE performs an arithmetic comparision of two values of precision
   36 or less.  If the comparision is true, a skip is generated. *)

procedure compare_single (cmp: cmp_codes;         (* kind of comparision to perform *)
                          op1, op2: expr  ); (* values to compare *)

  var
    tcmp: cmp_codes;              (* (possibly reversed) cmp *)
    reg: registers;               (* the register and memory operands *)
    addr1, addr2, mem: addr_desc;

begin
  addr1 := fetch_fullword (op1);
  addr2 := fetch_fullword (op2);

  if reverse_compare (addr1, addr2) (* select register and memory operands *)
    then begin
      tcmp := reverse [cmp];
      reg := load_addr (addr2, alignment (op2), 36);    (* value in reg must be full word *)
      mem := addr1;
      free (addr1);
    end
    else begin
      tcmp := cmp;
      reg := load_addr (addr1, alignment (op1), 36);
      mem := addr2;
      free (addr2);
    end;

  if branching then
    bb_end;

  gen_rm (cam+tcmp, reg, mem);      (* camxx reg,mem *)
  decr_reg_usages (reg);
end;
$PAGE compare_double - in test_and_skip
(* COMPARE DOUBLE performs and arithmetic comparision of two values of precision
   72 (i.e. a double word).  The code emitted will skip if the condition is true. *)

procedure compare_double (cmp: cmp_codes;         (* kind of comparision to perform *)
                          op1, op2: expr    ); (* values to compare *)

  var
    tcmp: cmp_codes;              (* (possibly reversed) cmp *)
    reg: registers;               (* the register and memory operands *)
    addr1, addr2, mem: addr_desc;

begin
  addr1 := argument (op1);
  addr2 := argument (op2);      (* make the operands directly addressible *)

  if reverse_compare (addr1, addr2) (* select register and memory operands *)
    then begin
      tcmp := reverse [cmp];
      reg := load_addr (addr2, alignment (op2), 72);
      mem := addr1;
      free (addr1);
    end
    else begin
      tcmp := cmp;
      reg := load_addr (addr1, alignment (op1), 72);
      mem := addr2;
      free (addr2);
    end;

  if branching then
    bb_end;

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
  decr_reg_usages (reg);
end;
$PAGE compare_arith - in test_and_skip
(* COMPARE ARITH performs an arithmetic comparision of two values.  If a
   specified condition is satistied, the code will cause a skip to occur. *)

procedure compare_arith (cmp: cmp_codes;         (* kind of comparision to perform *)
                         op1, op2: expr;                  (* values to compare *)
                         p: bit_range); (* precision of value *)

var
  tcmp: cmp_codes;
  mem: addr_desc;
  val1, val2: int_type;
  
begin
  if p <= 36 then begin         (* 1 word comparision *)
    if iconstp (op1, val1) andif iconstp (op2, val2) then begin
      mem := fetch (op1);
      free (mem);
      mem := fetch (op2);
      free (mem);
      if branching then
        bb_end;
      if const_comparison (val1, cmp, val2) then
	gen_rr (trna, 0, 0)
    end
    else if compare_with_zero (op1, cmp, tcmp) then begin
      mem := fetch_fullword (op2);
      free (mem);
      if branching then
        bb_end;
      gen_rm (skip+tcmp, 0, mem)
    end
    else if compare_with_zero (op2, reverse [cmp], tcmp) then begin
      mem := fetch_fullword (op1);
      free (mem);
      if branching then
        bb_end;
      gen_rm (skip+tcmp, 0, mem)
    end
    else
      compare_single (cmp, op1, op2);
  end

  else                    (* double word comparision *)
    compare_double (cmp, op1, op2)
end;
$PAGE test_and_skip - main routine

  var
    cmp: cmp_codes;
    reg, reg2: registers;
    taddr: addr_desc;
    lhs_set, rhs_set: set_desc;
    lwb, len: set_range;
    rts: rt_symbol;
    opc: opc_range;

begin
  with cond^ do begin
    if opcode <> odd_op then begin
      cmp := subopcodes [opcode];
      if negate then
        cmp := inverse [cmp]
    end;

    case opcode of

      ile_op..ine_op:
          compare_arith (cmp, operand[1], operand[2], desc.int_prec);

      rle_op..rne_op:
          if operand[1]^.desc.precision > srealprec
            then compare_arith (cmp, operand[1], operand[2], 72)
            else compare_arith (cmp, operand[1], operand[2], 36);

      ptreq_op, ptrne_op, fileq_op, filne_op:
          compare_arith (cmp, operand[1], operand[2], 18);

      odd_op:
        begin
          reg := load (operand[1], 18);
          if branching then
            bb_end;
          if negate
            then gen_ri (trne, reg, 1)
            else gen_ri (trnn, reg, 1);
          decr_reg_usages (reg);
        end;

      sle_op..sne_op:
        begin   (* operand is "sign" of comparision *)
          str_compare (cond);
          if branching then
            bb_end;
          gen_rr (skip+cmp, 0, 1);
        end;

      setle_op, setge_op, seteq_op, setne_op: begin
        prep_sets (opcode, operand[1], operand[2], lhs_set, rhs_set, lwb, len);
  
        if len <= 72 then begin (* both short sets *)
          if not is_register (lhs_set.arg[1]) then begin
            free (lhs_set.arg[1]);
            reg := get_reg (len);
            do_move (reg, lhs_set.arg[1], left_aligned, len);
            lhs_set.arg[1] := reg_addr (reg);
          end
          else reg := lhs_set.arg[1].offset;
          if rhs_set.arg[1].indirect orif (rhs_set.arg[1].mode <> fw) then begin
            reg2 := load_addr (rhs_set.arg[1], left_aligned, operand[2]^.desc.set_length);
            rhs_set.arg[1] := reg_addr (reg2);
          end;
          if (opcode = seteq_op) orif (opcode = setne_op) then begin
            if branching then
              bb_end;
            if len <= 36 then
              gen_rm (cam + cmp, reg, rhs_set.arg[1])
            else begin
              taddr := rhs_set.arg[1];
              gen_rm (cam + nec, reg, taddr);
              taddr.offset := taddr.offset + 1;
              gen_rm (cam + eqc, reg + 1, taddr);
              if cmp = nec then
                gen (jrst, 0, 0, 2, dot);
            end
          end
          else (* opcode = setle_op or setge_op *) begin
            if opcode = setle_op
              then opc := andcm
              else opc := andca;
            gen_rm (opc, reg, rhs_set.arg[1]);
            if len > 36 then begin
              taddr := rhs_set.arg[1];
              taddr.offset := taddr.offset + 1;
              gen_rm (opc, reg + 1, taddr);
              gen_rr (ior, reg, reg + 1);
            end;
            if branching then
              bb_end;
            if negate
              then gen (jump + nec, reg, 0, 2, dot)
              else gen (jump + eqc, reg, 0, 2, dot);
          end
        end (* short sets *)
 
        else begin
          if (opcode = setle_op) or (opcode = setge_op) then
            rts := rt_sle_ll
          else (* opcode = seteq_op or setne_op *)
            rts := rt_seq_ll;
          force_out_of_reg (lhs_set); (* if short, it might be in a reg *)
          force_out_of_reg (rhs_set);
          gen_rt (pushj, sb, rts);
	  if opcode = setge_op then begin (* == reversed setle_op *)
	    l_format_arg (rhs_set.arg[1], rhs_set.arg[2], rhs_set.arg[3]);
	    l_format_arg (lhs_set.arg[1], lhs_set.arg[2], lhs_set.arg[3]);
	  end
	  else begin
	    l_format_arg (lhs_set.arg[1], lhs_set.arg[2], lhs_set.arg[3]);
	    l_format_arg (rhs_set.arg[1], rhs_set.arg[2], rhs_set.arg[3]);
	  end;
          if branching then
            bb_end;
          gen_rr (skip + cmp, 0, 1)
        end;
        set_free (lhs_set);
        set_free (rhs_set)
      end;

      others:
        assert (false)
    end (* case *);
  end (* with *);
end;
$PAGE bool_compare_zero
(* BOOL COMPARE ZERO generates a boolean value for a comparision ("cmp") of
   some integer value ("exp") against zero.  The methods vary for each comparision
   but all involve insidious bit pattern hacks. *)

function bool_compare_zero ( cmp: cmp_codes; exp: expr ): registers;

var
  reg: registers;
  mem: addr_desc;

begin
  case cmp of

    ltc:
      begin
        reg := load (exp, 36);
        gen_ri (lsh, reg, -35);
      end;

    lec:
      begin
        reg := load (exp, 36);
        gen_ri (subi, reg, 1);
        gen_ri (lsh, reg, -35);
      end;

    gtc:
      begin
        mem := fetch_fullword (exp);
        free (mem);
        reg := get_reg (36);
        gen_rm (movn, reg, mem);
        gen_ri (lsh, reg, -35);
      end;

    gec:
      begin
        reg := load (exp, 36);
        gen_ri (lsh, reg, -35);
        gen_ri (andcai, reg, 1);
      end;

    eqc:
      begin
        reg := load (exp, 36);
        gen_rr (tdzn, reg, reg);
        gen_ri (movei, reg, 1);
      end;

    nec:
      begin
        mem := fetch_fullword (exp);
        free (mem);
        r get_reg (36);
        gen_rm (skip+eqc, reg, mem);
        gen_ri (movei, reg, 1);
      end

  end (* case *) ;
  bool_compare_zero := reg;
end;
$PAGE load_bool
(* LOAD BOOL evaluates a boolean expression and leaves the result in a register.
   A register preference may be specified, and the boolean expression may be
   negated. *)

public function load_bool (test: expr; negate: boolean): registers;

  var
    reg, reg2: registers;
    opc: opc_range;
    cval, minval, maxval: int_type;
    set_loc: set_desc;
    set_lwb, set_len: set_range;
    test_upb, test_lwb: boolean;
    cmp, tcmp: cmp_codes;

begin
  case test^.opcode of

    cst_ref, ident_ref, field_ref, array_ref, ptr_ref, buffer_ref, gen_andif_op, gen_orif_op,
      eof_op, eoln_op, eopage_op, masked_op, pending_op, func_call_op:
      begin
        reg := load (test, 36);
        if negate then gen_ri (andcai, reg, 1);
      end;

    bnot_op:
      begin
        reg := load_bool (test^.operand[1], not negate);
      end;

    odd_op:
      begin
        reg := load (test^.operand[1], 18);
        if negate
          then gen_ri (andcai, reg, 1)
          else gen_ri (andi, reg, 1);
      end;

    or_op:
      begin
        reg := do_binary_op (ior, test^.operand[1], test^.operand[2], [commutative, mem_form]);
        if negate then gen_ri (andcai, reg, 1);
      end;

    and_op:
      begin
        reg := do_binary_op (anda, test^.operand[1], test^.operand[2], [commutative, mem_form]);
        if negate then gen_ri (andcai, reg, 1);
      end;

    orif_op, andif_op:
      assert (false);                           (* should not reach code generator *)

    ile_op..ine_op:
      with test^ do begin
        cmp := subopcodes [test^.opcode];
        if negate then
          cmp := inverse [cmp];
        if compare_with_zero (test^.operand[1], cmp, tcmp) then
          reg := bool_compare_zero (tcmp, test^.operand[2])
        else if compare_with_zero (test^.operand[2], reverse [cmp], tcmp) then
          reg := bool_compare_zero (tcmp, test^.operand[1])
        else begin
          test_and_skip (test, not negate, false);
          reg := get_reg (36);
          gen (skip+awc, reg, 0, 0, gen_cint (1));
          gen_ri (setz, reg, 0)
        end
      end;

    sle_op..sne_op:
      begin
        opc := subopcodes [test^.opcode];
        if negate then opc := inverse[opc];
        str_compare (test);
        reg := get_reg (36);
        gen_rr (skip+opc, 0, 1);
        gen_rr (tdza, reg, reg);
        gen_ri (movei, reg, 1);
      end;

    seteq_op, setne_op, setle_op, setge_op: begin
      test_and_skip (test, negate, false);
      reg := get_reg (36);
      gen_rr (tdza, reg, reg);
      gen_ri (movei, reg, 1);
    end;

    in_op: begin
      get_bounds (test^.operand[1], minval, maxval);
      set_lwb := max (0, minval);
      if (maxval - set_lwb) >= set_size_limit then
	set_len := set_size_limit (* if max_val = 377777777777b and lwb = 0 adding 1 goes neg. *)
      else
	set_len := max (maxval - set_lwb + 1, 0);
      set_loc := set_fetch (test^.operand[2], set_lwb, set_len);

      if set_loc.nargs = 0 then begin (* null set *)
        reg := get_reg (36);
        gen_ri (movei, reg, ord (negate))
      end

      else if set_loc.nargs < 3 then begin      (* range or singleton set *)
        reg2 := load (test^.operand[1], 36);
        if addr_equal (set_loc.arg[1], set_loc.arg[2]) then (* singleton set *) begin
          decr_reg_usages (reg2);
          reg := get_reg (36);
          if negate
            then gen_rm (cam+nec, reg2, set_loc.arg[1])
            else gen_rm (cam+eqc, reg2, set_loc.arg[1]);
          gen_rr (tdza, reg, reg);
          gen_ri (movei, reg, 1);
        end
        else (* range set *) begin
          gen_rm (cam+ltc, reg2, set_loc.arg[1]);
          gen_rm (cam+lec, reg2, set_loc.arg[2]);
          decr_reg_usages (reg2);
          reg := get_reg (36);
          if negate then
            gen (skip+awc, reg, 0, 0, gen_cint (1))
          else
            gen_rr (tdza, reg, reg);
          gen_ri (movei, reg, ord (not negate));
        end;
      end

      else if iconstp (test^.operand[1], cval) then begin       (* const in some set *)
        cval := cval - set_loc.arg[2].offset;
        if is_register (set_loc.arg[1]) then begin
          reg := set_loc.arg[1].offset;
          if cval >= 36 then begin
            cval := cval - 36;
            free_and_disassociate (reg);
            reg := reg + 1;
          end
          else if regdesc[reg].associate <> noreg then
            free_and_disassociate (reg+1);
          if cval < 35 then
            gen_ri (lsh, reg, cval-35); (* move desired bit to right *)
          if negate
            then gen_ri (andcai, reg, 1)
            else gen_ri (andi, reg, 1);
          incr_reg_usages (reg, 1);     (* so it won't be freed with the set *)
        end (* if set in register *)
        else begin
          set_loc.arg[1] := increment_addr (set_loc.arg[1], cval div 36, cval mod 36, 1);
          reg := get_reg (36);
          do_move (reg, set_loc.arg[1], right_aligned, 1);
          if negate then
            gen_ri (andcai, reg, 1);
        end
      end (* if const in set *)

      else begin
        reg2 := load (test^.operand[1], 36);
        if aconstp (set_loc.arg[3], set_len) andif (set_len <= 72) then begin
          if not is_register (set_loc.arg[1]) then
            set_loc.arg[1] := reg_addr (load_addr (set_loc.arg[1],
                                                   alignment (test^.operand[2]),
                                                   set_len));
          reg := set_loc.arg[1].offset;
          incr_reg_usages (reg, 1);     (* so it won't be freed with the set *)
          if test^.operand[1]^.desc.int_prec > 8 then begin    (* if range check not implicit in shift *)
            gen_rm (cam+ltc, reg2, set_loc.arg[2]);
            gen_rm (cam+lec, reg2, int_value (set_len + set_loc.arg[2].offset - 1));
            clear (reg);
          end;
          if set_len <= 36 then
            gen (lsh, reg, reg2, -set_loc.arg[2].offset - 35, none)
          else begin
            gen (lshc, reg, reg2, -set_loc.arg[2].offset - 35, none);
            free_and_disassociate (reg+1);
          end;
          if negate
            then gen_ri (andcai, reg, 1)
            else gen_ri (andi, reg, 1);
        end
        else begin
          if stemps_dynamic then
            free (rtime_upb);
          reg := get_reg (36);
	  set_elem_reference (set_loc, reg2 (* desired elem *), reg (* loc for result *), minval, maxval, ldb);
          if negate
            then gen_ri (andcai, reg, 1);
        end;
        decr_reg_usages (reg2)
      end;
      set_free (set_loc)
    end;

    others: begin
      test_and_skip (test, not negate, false);
      reg := get_reg (36);
      gen (skip+awc, reg, 0, 0, gen_cint (1));
      gen_ri (setz, reg, 0)
    end
  end (* case *) ;

  load_bool := reg;
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
  int: int_type;

begin
  mem := fetch_fullword (exp);                  (* get the value as a full width operand *)
  bb_end;
  
  if iconstp (exp, int) then begin
    if const_comparison (int, cmp, 0) then
      gen_rl (jrst, 0, loc)
  end
  
  else if is_register (mem) then     (* use a jump *)
    gen_rl (jump+cmp, mem.offset, loc)

  else begin                            (* use a skip, so as to save a register *)
    gen_rm (skip+inverse [cmp], 0, mem);
    gen_rl (jrst, 0, loc);
  end;
  free (mem);
end;
$PAGE test_single_and_jump
(* TEST SINGLE AND JUMP evaluates a "test" on single word values and jumps to
   "tloc" or "floc" depending on whether the test is true or false.  "Skip_cond"
   indicates on what condition a jump should occur and on what condition a the
   code should continue.  Skip_cond = true => jump if false; false => jump if
   true. *)

procedure test_single_and_jump (test: expr; skip_cond: boolean; tloc, floc: tuple);
  
var
  tcmp: cmp_codes;
  mem: addr_desc;
  val1, val2: int_type;
  
begin
  with test^ do begin
    if iconstp (operand[1], val1) andif iconstp (operand[2], val2) then begin
      mem := fetch (operand[1]);
      free (mem);
      mem := fetch (operand[2]);
      free (mem);
      if const_comparison (val1, subopcodes [opcode], val2) then begin
	if not skip_cond then
	  gen_rl (jrst, 0, tloc) (* doing nothing would drop into floc *)
      end
      else begin
        if skip_cond then
	  gen_rl (jrst, 0, floc) (* doing nothing would drop into tloc *)
      end
    end
    else if compare_with_zero (operand[1], subopcodes [opcode], tcmp)
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

public procedure test_and_jump (test: expr;  (* the test to evaluate *)
                                tloc: tuple;  (* label node to goto if test true *)
                                floc: tuple;  (* label node to goto if test false *)
                                nloc: tuple );  (* label node following jump, if known; else nil *)
  
  var
    skip_cond, test_upb, test_lwb: boolean; (* skip on state *skip_cond* gotos to nloc (if <> nil) *)
    tcmp: cmp_codes;
    reg: registers;
    set_lwb, set_len: set_range;
    cval, minval, maxval: int_type;
    set_loc: set_desc;
 
  procedure jump_to_label (floc_op, tloc_op: opc_range; reg: registers);
    begin
      if skip_cond then
        gen_rl (floc_op, reg, floc)
      else
        gen_rl (tloc_op, reg, tloc)
    end;

begin

  (* Select the condition in which we 'fall through' to the nloc label. This
     will be used to bias the sense of the jumps performed. *)

  if tloc = nloc then
    skip_cond := true
  else if floc = nloc then
    skip_cond := false
  else
    skip_cond := true  (* nloc is neither tloc or floc, arbitrary selection okay *);

  (* If the node to be tested is a boolean value (variable or multi-use expression),
     then load the value and test it against zero. *)

  if (test^.opcode = cst_ref)      orif
     (test^.opcode = ident_ref)    orif (test^.opcode = field_ref) orif
     (test^.opcode = array_ref)    orif (test^.opcode = buffer_ref) orif
     (test^.opcode = func_call_op) orif (test^.opcode = eof_op) orif
     (test^.opcode = masked_op)    orif (test^.opcode = pending_op) orif
     (test^.opcode = eoln_op)      orif (test^.opcode = eopage_op) then begin
    if skip_cond
      then test_zero_jump (eqc, test, floc)     (* jump if false, continue if true *)
      else test_zero_jump (nec, test, tloc);
  end

  (* If this is not a value, then perform the indicated comparision. *)

  else
    case test^.opcode of

      ile_op..ine_op, ptreq_op, ptrne_op:
        test_single_and_jump (test, skip_cond, tloc, floc);

      slt_op, sle_op, sne_op, seq_op, sge_op, sgt_op:
        begin   (* operand is str_comp_op giving "sign" of comparision *)
          tcmp := subopcodes [test^.opcode];
          str_compare (test);
          bb_end;
          if skip_cond then
            gen_rr (skip+tcmp, 0, 1)
          else
            gen_rr (skip+inverse[tcmp], 0, 1);
          jump_to_label (jrst, jrst, 0)
        end;

      in_op: begin
        get_bounds (test^.operand[1], minval, maxval);
        set_lwb := max (0, minval);
	if (maxval - set_lwb) >= set_size_limit then
	  set_len := set_size_limit (* if maxval = 377777777777b & lwb = 0 adding 1 goes neg. *)
	else
	  set_len := max (0, maxval - set_lwb + 1);
        set_loc := set_fetch (test^.operand[2], set_lwb, set_len);

        if set_loc.nargs = 0 then begin (* null set *)
          bb_end;
          if skip_cond then
            gen_rl (jrst, 0, floc)
        end

        else if set_loc.nargs < 3 then begin    (* range or singleton set *)
          reg := load (test^.operand[1], 36);
          bb_end;
          if addr_equal (set_loc.arg[1], set_loc.arg[2]) then (* singleton set *) begin
            if skip_cond
              then gen_rm (cam+eqc, reg, set_loc.arg[1])
              else gen_rm (cam+nec, reg, set_loc.arg[1]);
          end
          else (* range set *) begin
            gen_rm (cam+ltc, reg, set_loc.arg[1]);
            gen_rm (cam+lec, reg, set_loc.arg[2]);
            skip_cond := true;
          end;
          jump_to_label (jrst, jrst, 0);
          decr_reg_usages (reg);
        end

        else if iconstp (test^.operand[1], cval) then begin     (* const in some set *)
          cval := cval - set_loc.arg[2].offset;
          if is_register (set_loc.arg[1]) then begin    
            bb_end;
            reg := set_loc.arg[1].offset;
            if cval >= 36 then begin
              cval := cval - 36;
              reg := reg + 1;
            end;
            if cval = 0 then
              jump_to_label (jump+gec, jump+ltc, reg)
            else begin
              if cval <= 17 then
                if skip_cond
                  then gen_ri (tlnn, reg, 2** (17-cval))
                  else gen_ri (tlne, reg, 2** (17-cval))
              else if skip_cond
                then gen_ri (trnn, reg, 2** (17 - (cval-18)))
                else gen_ri (trne, reg, 2** (17 - (cval-18)));
              jump_to_label (jrst, jrst, 0)
            end
          end (* if set in register *)
          else (* set not in register *) begin
            set_loc.arg[1] := increment_addr (set_loc.arg[1], cval div 36, cval mod 36, 1);
            do_move (1, set_loc.arg[1], right_aligned, 1);
            bb_end;
            jump_to_label (jump+eqc, jump+nec, 1)
          end
        end (* if const in set *)

        else begin
          reg := load (test^.operand[1], 36);
          if aconstp (set_loc.arg[3], set_len) andif (set_len <= 72) then begin
            if not is_register (set_loc.arg[1]) then
              set_loc.arg[1] := reg_addr (load_addr (set_loc.arg[1],
                                                     alignment (test^.operand[2]),
                                                     set_len));
            if test^.operand[1]^.desc.int_prec > 8 then begin  (* if range check not implicit in shift *)
              gen_rm (cam+ltc, reg, set_loc.arg[2]);
              gen_rm (cam+lec, reg, int_value (set_len + set_loc.arg[2].offset - 1));
              clear (set_loc.arg[1].offset);
            end;
            if set_len <= 36
              then gen (lsh, set_loc.arg[1].offset, reg, -set_loc.arg[2].offset, none)
              else gen (lshc, set_loc.arg[1].offset, reg, -set_loc.arg[2].offset, none);
            bb_end;
            jump_to_label (jump+gec, jump+ltc, set_loc.arg[1].offset);
          end
          else begin
            if stemps_dynamic then
              free (rtime_upb);
	    set_elem_reference (set_loc, reg (* desired elem *), 1 (* loc for result *), minval, maxval, ldb);
            bb_end;
            jump_to_label (jump+eqc, jump+nec, 1);
          end;
          decr_reg_usages (reg)
        end;
        set_free (set_loc)
      end;

      others:
        begin
          test_and_skip (test, not skip_cond, true);
          jump_to_label (jrst, jrst, 0)
        end

    end (* case & else *);

  if skip_cond and (tloc <> nloc)               (* if true case falls through *)
    then gen_rl (jrst, 0, tloc);                (* then branch to true label if not already there *)
end.
 ,)2	