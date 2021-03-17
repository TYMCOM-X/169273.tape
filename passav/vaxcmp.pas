$TITLE vaxcmp - comparision and jump code generation

module vaxcmp;
$PAGE includes
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE vaxcg.typ
$INCLUDE vaxcgu.inc
$INCLUDE vaxexp.inc
$INCLUDE vaxopc.inc
$INCLUDE vaxutl.inc
$system vaxstr.inc
$system vaxset.inc
$system vaxgen.inc
$system pasmth.inc
$system ptmcon.inc
$PAGE opcode mapping tables

const	(* comparison conditions *)
	nvc = 0;
	ltc = 1;
	eqc = 2;
	lec = 3;
	awc = 4;
	gec = 5;
	nec = 6;
	gtc = 7;

type cmp_codes = nvc..gtc;

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
	(  nvc,	(* nvc *)
	   gtc,	(* ltc *)
	   eqc,	(* eqc *)
	   gec,	(* lec *)
	   awc,	(* awc *)
	   lec,	(* gec *)
	   nec,	(* nec *)
	   ltc  );	(* gtc *)

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
$PAGE branch tables

type branch_table = packed array[cmp_codes, data_alignment] of opc_range;

const branch_codes: branch_table := (
	(halt,	halt,	halt),	(* nvc *)
	(blssu,	blss,	blss),	(* ltc *)
	(beqlu,	beql,	beql),	(* eqc *)
	(blequ,	bleq,	bleq),	(* lec *)
	(brb,	brb,	brb),	(* awc *)
	(bgequ,	bgeq,	bgeq),	(* gec *)
	(bnequ,	bneq,	bneq),	(* nec *)
	(bgtru,	bgtr,	bgtr));	(* gtc *)
$PAGE compare_with_zero
(* COMPARE WITH ZERO examines an operand of an arithmetic conditional the relation
   to be tested for, and determines if the compare can be done with zero.  This
   includes direct comparisions with zero, and special cases of compares with
   1 and -1.  A flag is returned indicating whether or not a zero compare can be
   performed.  If the flag is true, the relation subopcode is also returned. *)

function compare_with_zero
     (	op1: expr;			(* test is of form op1 :: op2 *)
	cmp: cmp_codes;			(* original relational operator: op1 :: x *)
	var tcmp: cmp_codes		(* operator for zero comparision: op2 :: 0 *)
		): boolean;

 var val: int_type;

 begin
  if not iconstp (op1, val) then begin	(* not a constant *)
    compare_with_zero := false;
    return;	(* <---- return since not a const *)
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
    compare_with_zero := false;		(* <---- exit if special cases do not apply *)
    return;
  end;

  compare_with_zero := true;
 end;
$PAGE fetch_operands
(* FETCH OPERANDS insures that two quantities can be directly compared.
   In particular, comparison of two unsigned values may require zero extension
   of one; comparison of two signed values may require sign extension of one;
   and comparison of a signed value and an unsigned value requires zero
   extension of the unsigned quantity and possibly sign extension of the other
   operand. The alignment of the two operands is returned, and 
   an ADDR_DESC for the final location of each operand is returned in a
   formal parameter. *)

function fetch_operands (op1, op2: expr; var loc1, loc2: addr_desc): data_alignment;

var
  align1, align2: data_alignment;

  function convert (loc: addr_desc; mode: vax_type;
	 reg_size: unit_range; align: data_alignment): addr_desc;
  (* CONVERT is a helper for FETCH_OPERANDS and moves an operand to a
     register, with a specified alignment and size. *)


  begin
    free (loc);	(* possibly reuse resources *)
    convert := reg_addr (get_reg (reg_size));
    convert.byte_size := mode;
    store (loc, convert, align);
  end;

begin (* fetch_operands *)

  (* Make operands addressible. *)

  loc1 := fetch (op1, no_preference );
  loc2 := fetch (op2, no_preference );

  (* Determine their alignment. *)

  align1 := alignment (op1);
  align2 := alignment (op2);

  (* If one is a single precision real and the other a double, the 
     single must be converted. *)

  if [loc1.byte_size,loc2.byte_size] <= [vax_float, vax_double] then begin
    if loc1.byte_size <> loc2.byte_size then
      if loc1.byte_size = vax_float
	then loc1 := convert (loc1, vax_double, 2*bits_per_unit, right_aligned)
	else loc2 := convert (loc2, vax_double, 2*bits_per_unit, right_aligned);
  end (* real case *)
  else begin

    (* If one is signed and the other not, the unsigned value must be zero
       extended unless already a longword. *)

    if [align1,align2] = [signed_value,unsigned_value] then (* sneaky *) begin
      if (align1 = unsigned_value) andif (loc1.byte_size < vax_long) then
	loc1 := convert (loc1, max (succ (loc1.byte_size), loc2.byte_size), bits_per_unit, unsigned_value)
      else if (align2 = unsigned_value) andif (loc2.byte_size < vax_long) then
	loc2 := convert (loc2, max (succ (loc2.byte_size), loc1.byte_size), bits_per_unit, unsigned_value);
      align1 := signed_value;
      align2 := signed_value;
    end;

    (* Both must now have the same byte_size. *)

    if loc1.byte_size <> loc2.byte_size then
      if loc1.byte_size < loc2.byte_size
	then loc1 := convert (loc1, loc2.byte_size, bits_per_unit, align1)
	else loc2 := convert (loc2, loc1.byte_size, bits_per_unit, align2);
  end (* neither operand real *);
  fetch_operands := align1;
end (* fetch_operands *);
$PAGE compare_zero_branch
(* COMPARE ZERO BRANCH compares a scalar value with zero and branches
   if a specified relation is satisfied. *)

procedure compare_zero_branch (cmp: cmp_codes; op: expr; branch_loc: def);

var
  mem: addr_desc;

begin
  mem := fetch (op, no_preference );
  free (mem);
  gen1 (typ_opc (tstl, mem.byte_size), mem);
  gen_branch (branch_codes [cmp, alignment (op)], branch_loc);
end;
$PAGE bool_negate
(* BOOL NEGATE complements a boolean value in a register. *)

procedure bool_negate (reg: registers);

begin
  gen3 (bicl3, reg_addr (reg), int_value (1), reg_addr (reg));
end;
$PAGE compare_constants

(* COMPARE_CONSTANTS compares a scalar constant (parmameter VALUE1) with
   a second constant (VALUE2).  If a given relationship (parameter CMP)
   holds between them then a branch to a specified location (parameter LOC)
   is generated.  *)

procedure compare_constants ( cmp: cmp_codes; value1: int_type;
    value2: int_type; loc: def );

type
  relation_set = set of cmp_codes;
  relation_array = array [cmp_codes] of relation_set;

const
  constituent_relations: relation_array :=
      ( [],				(* nvc *)
	[ ltc ],			(* ltc *)
	[ eqc ],			(* eqc *)
	[ ltc, eqc ],			(* lec *)
	[ ltc, eqc, gtc ],		(* awc *)
	[ eqc, gtc ],			(* gec *)
	[ ltc, gtc ],			(* nec *)
	[ gtc ] );			(* gtc *)
var
  relation: cmp_codes;

begin

  (* Determine relationship of first value to second. *)

  if value1 < value2
    then relation := ltc
  else if value1 = value2
    then relation := eqc
  else relation := gtc;

  (* If the relation passed as a parameter holds then generate an
      unconditional branch to the specified location.  *)

  if relation in constituent_relations[ cmp ]
    then gen_branch ( brb, loc );

end  (* proc compare_constants *) ;
$PAGE compare_arith
(* COMPARE ARITH performs an arithmetic comparison of two values. If a
   specified condition is satisfied a branch will be taken to a specified location. *)

procedure compare_arith (cmp: cmp_codes; op1, op2: expr; branch_loc: def);

var
  tcmp: cmp_codes;
  loc1, loc2: addr_desc;
  align: data_alignment;
  value1: int_type;
  value2: int_type;

begin
  if compare_with_zero (op1, cmp, tcmp) then
    compare_zero_branch (tcmp, op2, branch_loc)
  else if compare_with_zero (op2, reverse [cmp], tcmp) then
    compare_zero_branch (tcmp, op1, branch_loc)
  else begin
    align := fetch_operands (op1, op2, loc1, loc2);
    if aconstp ( loc1, value1 ) and aconstp ( loc2, value2 ) then begin
      compare_constants ( cmp, value1, value2, branch_loc );
    end
    else begin
      gen2 (typ_opc (cmpl, loc1.byte_size), loc1, loc2);
      gen_branch (branch_codes [cmp, align], branch_loc);
      free (loc1);
      free (loc2);
    end;
  end;
end;
$PAGE succ_or_fail_branch, rangeset
  
(* SUCC OR FAIL BRANCH emits the specified branch instruction choosing the target
   according to the given condition.  *)
  
procedure succ_or_fail_branch (op: opc_range; eqc: boolean; var eqc_target, nec_target: def);
  
  begin
    if eqc then begin
      if eqc_target = nil then
	eqc_target := make_def (local_def);
      gen_branch (op, eqc_target)
    end
    else begin
      if nec_target = nil then
	nec_target := make_def (local_def);
      gen_branch (op, nec_target)
    end
  end;
  
  
  
(* RANGESET allocates a set temporary and builds the range set in it.  *)
  
procedure rangeset (set_op: expr; (* Expr tuple range_set is value of.   Only used
					  here to get at type info in the expr_type_desc.  *)
			 var range_set: set_desc; (* "O" format set descriptor. *)
			 ctxt_base: set_range); (* Contextual lower bound to be used. *)
  
  var
    temp_set: set_desc;
    temp: addr_desc;
    max_len, const_upb: set_range;
  defn: def;
  
  begin
    with set_op^.desc do
      max_len := ((set_lwb + set_length - 1) - ctxt_base) div bits_per_byte + 1;
  
    (* Allocate temporary. *)
  
    if aconstp (range_set.arg[2], const_upb) then (* constant lenth *)
      temp_set := set_temporary (ctxt_base,
				 int_value ((const_upb - ctxt_base) div bits_per_byte + 1))
    else if max_len <= 128 then (* variable, but use worst case as if constant *)
      temp_set := set_temporary (ctxt_base, int_value (max_len))
    else begin (* bite the bullet and use variable length *)
      temp := get_typ_reg_addr (vax_long);
      if ctxt_base = 0 then
	gen2 (movl, range_set.arg[2], temp)
      else
	gen3 (subl3, int_value (ctxt_base), range_set.arg[2], temp);
      defn := make_def (local_def);
      gen_branch (bgeq, defn);
      gen1 (clrl, temp);
      mark_def (code_area, defn);
      gen2 (divl2, int_value (bits_per_byte), temp);
      gen1 (incl, temp);
      temp_set := set_temporary (ctxt_base, temp);
      free (temp)
    end;
  
    (* Build the range in the new temporary. *)
  
    gen_set (range_set, temp_set);
    range_set := temp_set
  end (* rangeset *);
$PAGE set_equality

(* SET EQUALITY generates code for seteq_op and setne_op. *)

procedure set_equality (test: expr; negate: boolean; var failure_loc: def);

var
  cmp: cmp_codes;
  temp_expr: expr;
  first_set, second_set, temp_set: set_desc;
  const_bytelen, ctxt_base, const_upb: set_range;
  temp: addr_desc;
  success_loc, defn: def;
  
procedure failure_branch (op_if_eqc, op_if_nec: opc_range);
  begin
    if cmp = eqc then
      gen_branch (op_if_eqc, failure_loc)
    else
      gen_branch (op_if_nec, failure_loc)
  end;
  
begin
  
  (* Invert the operation so that failure_loc will actually be just that. *)
  
  cmp := subopcodes [test^.opcode];
  if not negate then
    cmp := inverse [cmp];
  
  first_set  := set_fetch (test^.operand[1], 0, set_size_limit, no_preference, false);
  second_set := set_fetch (test^.operand[2], 0, set_size_limit, no_preference, false);
  success_loc := nil; (* so I can tell if it was ever required *)
  
  (* Make the matrix of possibilities triangular. *)
  
  if (first_set.nargs < second_set.nargs) or
     (((first_set.nargs = 3) and (second_set.nargs = 3)) andif
	  (first_set.arg[2].offset < second_set.arg[2].offset)) then begin
    temp_set   := first_set;  temp_expr        := test^.operand[1];
    first_set  := second_set; test^.operand[1] := test^.operand[2];
    second_set := temp_set;   test^.operand[2] := temp_expr
  end;
  
  (* Emit code for first_set "cmp" second_set, branching to failure_loc if false,
     falling through otherwise.  *)
  
  case second_set.nargs of
  
    (* First, take second_set null (first_set.nargs in [0, 2, 3]). *)

    0: case first_set.nargs of
	 0: (* [] = [] *)
	   if cmp = nec then
	     gen_branch (brb, failure_loc);
	 2:
	   if not adr_equal (first_set.arg[1], first_set.arg[2]) then begin (* [x..y] = [] *)
	     gen2 (cmpl, first_set.arg[1], first_set.arg[2]);
	     failure_branch (bleq, bgtr)
	   end
	   else if cmp = eqc then (* [x] = [] *)
	     gen_branch (brb, failure_loc);
	 3: begin (* <set> = [] *)
	   gen_c5 (cmpc5, int_value (0), stack_top_reference (* something short *),
			  first_set.arg[3], first_set.arg[1]);
	   failure_branch (bneq, beql)
	 end
       end;

    (* Second_set range or singleton (first_set.nargs in [2, 3]). *)

    2: begin
      if first_set.nargs < 3 then begin (* [w..x] = [y..z] *)
	if not adr_equal (first_set.arg[1], first_set.arg[2]) and
	   not adr_equal (second_set.arg[1], second_set.arg[2]) then begin
	  defn := make_def (local_def);
	  gen2 (cmpl, first_set.arg[1], first_set.arg[2]);
	  gen_branch (bleq, defn);
	  gen2 (cmpl, second_set.arg[1], second_set.arg[2]);
	  succ_or_fail_branch (bgtr, cmp = eqc, success_loc, failure_loc);
	  mark_def (code_area, defn)
	end;
	gen2 (cmpl, first_set.arg[1], second_set.arg[1]);
	if adr_equal (first_set.arg[1], first_set.arg[2]) and
	   adr_equal (second_set.arg[1], second_set.arg[2]) then (* both singleton *)
	  failure_branch (bneq, beql)
	else begin
	  succ_or_fail_branch (bneq, cmp = eqc, failure_loc, success_loc);
	  gen2 (cmpl, first_set.arg[2], second_set.arg[2]);
	  failure_branch (bneq, beql)
	end
      end
      else begin (* <set> = [x..y] *)
	ctxt_base := first_set.arg[2].offset * bits_per_byte;
	if ctxt_base > test^.operand[2]^.desc.set_lwb then begin (* x could be below ctxt_base *)
	  (* range could be empty (if its not singleton) *)
	  if not adr_equal (second_set.arg[1], second_set.arg[2]) then begin
	    gen2 (cmpl, second_set.arg[1], second_set.arg[2]);
	    defn := make_def (local_def);
	    gen_branch (bgtr, defn)
	  end;
	  gen2 (cmpl, second_set.arg[1], int_value (ctxt_base));
	  succ_or_fail_branch (blss, cmp = eqc, failure_loc, success_loc);
	  if not adr_equal (second_set.arg[1], second_set.arg[2]) then (* did we emit empty check? *)
	    mark_def (code_area, defn)
	end;
	rangeset (test^.operand[2], second_set, ctxt_base);
	gen_c5 (cmpc5, first_set.arg[3], first_set.arg[1], second_set.arg[3], second_set.arg[1]);
	failure_branch (bneq, beql)
      end
    end;

    (* Second_set is a set (and so first_set must be also). *)

    3: begin
      if first_set.arg[2].offset > second_set.arg[2].offset then begin
	if aconstp (first_set.arg[3], const_bytelen) then
	  temp := int_value (const_bytelen + (first_set.arg[2].offset - second_set.arg[2].offset))
	else begin
	  temp := get_typ_reg_addr (vax_long);
	  gen3 (addl3, first_set.arg[3],
		       int_value (first_set.arg[2].offset - second_set.arg[2].offset), temp)
	end;
	temp_set := set_temporary (second_set.arg[2].offset * bits_per_byte, temp);
	free (temp);
	set_move (first_set, temp_set);
	first_set := temp_set
      end;
      gen_c5 (cmpc5, first_set.arg[3], first_set.arg[1], second_set.arg[3], second_set.arg[1]);
      failure_branch (bneq, beql)
    end
  end (* case second_set.nargs *);
  
  if success_loc <> nil then
    mark_def (code_area, success_loc);
  set_free (first_set);
  set_free (second_set)
end (* set_equality *);
$PAGE set_inclusion
  
(* SET INCLUSION generates code for setle_op and setge_op. *)
  
procedure set_inclusion (test: expr; negate: boolean; var success_loc: def);
  
type
  set_op_table = array [1..4] of opc_range;
const
  diff_2op: set_op_table := (bicb2, bicw2, halt, bicl2);
  diff_3op: set_op_table := (bicb3, bicw3, halt, bicl3);
  
var
  temp_expr: expr;
  first_set, second_set, temp_set: set_desc;
  bit_position, loop_len_addr: addr_desc;
  failure_loc, defn: def;
  min_val, max_val: int_type;
  ctxt_base, const_upb, first_len, second_len, loop_len: set_range;
  fixed: boolean;
  
procedure success_branch (op_if_not_negate, op_if_negate: opc_range);
  begin
    if not negate then
      gen_branch (op_if_not_negate, success_loc)
    else
      gen_branch (op_if_negate, success_loc)
  end;
  
function index_addr (var addr: addr_desc; x_reg: registers): addr_desc;
  begin
    index_addr := addr;
    index_addr.index := x_reg
  end;
  
begin
  if test^.opcode = setge_op then begin
    temp_expr := test^.operand[1];
    test^.operand[1] := test^.operand[2];
    test^.operand[2] := temp_expr
  end;
  failure_loc := nil; (* so I can tell if it was ever required *)
  first_set := set_fetch (test^.operand[1], 0, set_size_limit, no_preference, false);
  second_set := set_fetch (test^.operand[2], test^.operand[1]^.desc.set_lwb,
					     test^.operand[1]^.desc.set_length, no_preference, false);
  
  (* Emit code for first_set <= second_set, branching to success_loc if true,
     falling through otherwise. *)
  
  case first_set.nargs of
  
    (* First_set null. *)
  
    0: (* [] <= []      if not negate then
	gen_branch (brb, success_loc);
 
    (* First_set singleton or range. *)
  
    2:
      if adr_equal (first_set.arg[1], first_set.arg[2]) then begin (* first_set singleton *)
	case second_set.nargs of
	  0: (* [x] <= [] *)
	    if negate then
	      gen_branch (brb, success_loc);
	  2:
	    if adr_equal (second_set.arg[1], second_set.arg[2]) then begin (* [x] <= [y] *)
	      gen2 (cmpl, first_set.arg[1], second_set.arg[1]);
	      success_branch (beql, bneq)
	    end
	    else begin (* [x] <= [y..z] *)
	      gen2 (cmpl, first_set.arg[1], second_set.arg[1]);
	      succ_or_fail_branch (blss, not negate, failure_loc, success_loc);
	      gen2 (cmpl, first_set.arg[1], second_set.arg[2]);
	      success_branch (bleq, bgtr)
	    end;
	  3: begin (* [x] <= <set> *)
	    genset_op_bounds (first_set.lwb_exp, min_val, max_val, fixed);
	    if not negate then begin
	      prep_bitreference (first_set.arg[1], bit_position, min_val, max_val,
				 second_set, failure_loc);
	      gen_bb (bbs, bit_position, second_set.arg[1], success_loc)
	    end
	    else begin
	      prep_bitreference (first_set.arg[1], bit_position, min_val, max_val,
				 second_set, success_loc);
	      gen_bb (bbc, bit_position, second_set.arg[1], success_loc)
	    end
	  end
	end (* case second_set.nargs *);
      end
      else begin (* first_set range *)
	case second_set.nargs of

	  0: begin (* [x..y] <= [] *)
	    gen2 (cmpl, first_set.arg[1], first_set.arg[2]);
	    success_branch (bgtr, bleq)
	  end;

	  2:
	    if adr_equal (second_set.arg[1], second_set.arg[2]) then begin (* [x..y] <= [z] *)
	      gen2 (cmpl, first_set.arg[1], first_set.arg[2]);
	      succ_or_fail_branch (bgtr, not negate, success_loc, failure_loc);
	      succ_or_fail_branch (blss, not negate, failure_loc, success_loc);
	      gen2 (cmpl, first_set.arg[1], second_set.arg[1]);
	      success_branch (beql, bneq)
	    end
	    else begin (* [w..x] <= [y..z] *)
	      gen2 (cmpl, first_set.arg[1], first_set.arg[2]);
	      succ_or_fail_branch (bgtr, not negate, success_loc, failure_loc);
	      gen2 (cmpl, first_set.arg[1], second_set.arg[1]);
	      succ_or_fail_branch (blss, not negate, failure_loc, success_loc);
	      gen2 (cmpl, first_set.arg[2], second_set.arg[2]);
	      success_branch (bleq, bgtr)
	    end;

	  3: begin (* [x..y] <= <set> *)
	    ctxt_base := second_set.arg[2].offset * bits_per_byte;
	    if ctxt_base > test^.operand[1]^.desc.set_lwb then begin (* x could be below ctxt_base *)
	      gen2 (cmpl, first_set.arg[1], first_set.arg[2]);
	      succ_or_fail_branch (bgtr, not negate, success_loc, failure_loc);
	      gen2 (cmpl, first_set.arg[1], int_value (ctxt_base));
	      succ_or_fail_branch (blss, not negate, failure_loc, success_loc)
	    end;
	    rangeset (test^.operand[1], first_set, ctxt_base);
	    (* Calculate [x..y] - <set>, then see if its null. *)
	    if aconstp (first_set.arg[3], first_len) and aconstp (second_set.arg[3], second_len) then
	      loop_len_addr := int_value (min (first_len, second_len))
	    else begin
	      gen2 (movl, first_set.arg[3], r0_addr);
	      gen2 (cmpl, r0_addr, second_set.arg[3]);
	      defn := make_def (local_def);
	      gen_branch (blss, defn);
	      gen2 (movl, second_set.arg[3], r0_addr);
	      mark_def (code_area, defn);
	      loop_len_addr := r0_addr
	    end;
	    if aconstp (loop_len_addr, loop_len) andif (loop_len in [1,2,4]) then
	      gen2 (diff_2op [loop_len], second_set.arg[1], first_set.arg[1])
	    else begin
	      if aconstp (loop_len_addr, loop_len) then
		gen2 (movl, int_value (loop_len - 1), r0_addr)
	      else
		gen1 (decl, r0_addr);
	      defn := make_def (local_def);
	      mark_def (code_area, defn);
	      gen2 (bicb2, index_addr (second_set.arg[1], r0), index_addr (first_set.arg[1], r0));
	      gen2 (sobgeq, r0_addr, branch_addr (defn))
	    end;
	    gen_c5 (cmpc5, int_value (0), stack_top_reference, first_set.arg[3], first_set.arg[1]);
	    success_branch (beql, bneq)
	  end
	end (* case second_set.nargs *);
      end;
  
    (* First_set is a set. *)
  
    3:
      if second_set.nargs = 0 then begin (* <set> <= [] *)
	gen_c5 (cmpc5, int_value (0), stack_top_reference, first_set.arg[3], first_set.arg[1]);
	success_branch (beql, bneq)
      end
      else begin (* <set> <= [x] or [x..y] or <set> *)
	temp_set := set_temporary (first_set.arg[2].offset * bits_per_byte, first_set.arg[3]);
	if second_set.nargs < 3 then
	  gen_set (second_set, temp_set)
	else
	  set_move (second_set, temp_set);
	second_set := temp_set;
	if aconstp (first_set.arg[3], loop_len) andif (loop_len in [1,2,4]) then begin
	  gen3 (diff_3op [loop_len], second_set.arg[1], first_set.arg[1], r1_addr);
	  success_branch (beql, bneq)
	end
	else begin
	  if aconstp (first_set.arg[3], loop_len) then
	    gen2 (movl, int_value (loop_len - 1), r0_addr)
	  else
	    gen3 (subl3, int_value (1), first_set.arg[3], r0_addr);
	  defn := make_def (local_def);
	  mark_def (code_area, defn);
	  gen3 (bicb3, index_addr (second_set.arg[1], r0), index_addr (first_set.arg[1], r0),
		       r1_addr);
	  succ_or_fail_branch (bneq, not negate, failure_loc, success_loc);
	  gen2 (sobgeq, r0_addr, branch_addr (defn));
	  if not negate then
	    gen_branch (brb, success_loc)
	end
      end
  
  end (* case first_set.nargs *);
  
  if failure_loc <> nil then
    mark_def (code_area, failure_loc);
  set_free (first_set);
  set_free (second_set)
end (* set_inclusion *);
$PAGE test_and_branch
(* TEST AND BRANCH evaluates a boolean expression and branches to a specified
   location if the boolean expression evaluates to TRUE.  *)

procedure test_and_branch (test: expr; negate: boolean; var branch_loc: def);

var
  cmp: cmp_codes;
  reg: registers;
  mem: addr_desc;

begin
  case test^.opcode of
    ile_op..ine_op, rle_op..rne_op, ptreq_op, ptrne_op, fileq_op, filne_op: begin
      cmp := subopcodes [test^.opcode];
      if negate then cmp := inverse [cmp];
      compare_arith (cmp, test^.operand[1], test^.operand[2], branch_loc);
    end;

    odd_op: begin
      mem := fetch (test^.operand[1], no_preference );
      gen2 (typ_opc (bitl, mem.byte_size), int_value (1), mem);
      if negate
	then gen_branch (beql, branch_loc)
	else gen_branch (bneq, branch_loc);
      free (mem);
    end;

    sle_op..sne_op: begin
      str_compare (test);
      cmp := subopcodes [test^.opcode];
      if negate then cmp := inverse [cmp];
      gen_branch (branch_codes [cmp, unsigned_value], branch_loc);
    end;

    seteq_op, setne_op:
      set_equality (test, negate, branch_loc);

    setle_op, setge_op:
      set_inclusion (test, negate, branch_loc);

    others: assert (false)
  end
end (* test_and_branch *);
$PAGE bool_compare_zero
(* BOOL COMPARE ZERO generates a boolean value for a comparision ("cmp") of
   some integer value ("exp") against zero. *)

function bool_compare_zero ( cmp: cmp_codes; exp: expr ): registers;

 var
   reg: registers;
   mem: addr_desc;
   d: def;

 begin
  case cmp of

    nec: begin
      reg := load (exp);
      d := make_def (local_def);
      gen_branch (beql, d);
      move_immediate (1, reg_addr (reg));
      mark_def (code_area, d);
    end;

    others: begin
      reg := get_reg (bits_per_unit);
      gen1 (clrl, reg_addr (reg));
      mem := fetch (exp, no_preference );
      gen1 (typ_opc (tstl, mem.byte_size), mem);
      d := make_def (local_def);
      gen_branch (branch_codes[ inverse[cmp], alignment(exp) ], d);
      gen1 (incl, reg_addr (reg));
      mark_def (code_area, d);
      free (mem);
    end

  end (* case *) ;
  bool_compare_zero := reg;
 end;
$PAGE bool_long_compare, bool_single_compare
(* BOOL LONG COMPARE evaluates a boolean test and returns the register
   in which the result is placed. *)

function bool_long_compare (test: expr; negate: boolean): registers;

var
  reg: registers;
  d1: def;

begin
  reg := get_reg (bits_per_unit);
  gen1 (clrl, reg_addr (reg)); (* preload false *)
  d1 := make_def (local_def);
  test_and_branch (test, not negate, d1); (* note inversion to make label success loc *)
  gen1 (incl, reg_addr (reg)); (* change to true *)
  mark_def (code_area, d1); (* target to leave result false *)
  bool_long_compare := reg;
end;

(* BOOL SINGLE COMPARE performs a test on single word values, special casing
   compares with zero. The test may be negated. *)

function bool_single_compare (test: expr; negate: boolean): registers;

var
  cmp, tcmp: cmp_codes;

begin
  with test^ do begin
    cmp := subopcodes [opcode];
    if negate then cmp := inverse [cmp];
    if compare_with_zero (operand[1], cmp, tcmp) then
      bool_single_compare := bool_compare_zero (tcmp, operand[2])
    else if compare_with_zero (operand[2], reverse [cmp], tcmp) then
      bool_single_compare := bool_compare_zero (tcmp, operand[1])
    else bool_single_compare := bool_long_compare (test, negate);
  end;
end;
$PAGE load_bool
(* LOAD BOOL evaluates a boolean expression and leaves the result in a register.
   The boolean expression may be negated. *)

public function load_bool (test: expr; negate: boolean): registers;

  var
    reg: registers;
    fixed: boolean;
    temp_addr, mem, bit_position: addr_desc;
    min_val, max_val: int_type;
    defn1, defn2: def;
    set_lwb, set_len: set_range;
    set_loc: set_desc;

begin
  case test^.opcode of

    cst_ref, ident_ref, field_ref, gen_orif_op, gen_andif_op,
      ptr_ref, array_ref, buffer_ref, eof_op, eoln_op, eopage_op, func_call_op: begin
	reg := load (test);
	if negate then bool_negate (reg);
      end;

    bnot_op:
	reg := load_bool (test^.operand[1], not negate);

    odd_op:
      begin
	reg := load (test^.operand[1]);
	if negate
	  then bool_negate (reg)
	  else gen2 (bicl2, int_value (-2), reg_addr (reg));
      end;

    or_op:
      begin
	temp_addr := do_binary_op (bisl2, test^.operand[1], test^.operand[2],
	      [commutative,mem_form], no_preference );
	reg := temp_addr.register;
	if negate then bool_negate ( reg );
      end;

    and_op:
      begin
	mem := fetch (test^.operand[1], no_preference );
	reg := load (test^.operand[2]);
	free (mem);
	gen2 (typ_opc (mcoml, mem.byte_size), mem, r1_addr);
	gen2 (bicb2, r1_addr, reg_addr (reg));
	if negate then bool_negate (reg);
      end;

    orif_op, andif_op:
      assert (false);

    ile_op..ine_op:
      reg := bool_single_compare (test, negate);

    in_op: begin
      genset_op_bounds (test^.operand[1], min_val, max_val, fixed);
      set_lwb := max (0, min_val);
      if (max_val - set_lwb) >= set_size_limit then
	set_len := set_size_limit
      else
	set_len := max (0, max_val - set_lwb + 1);
      set_loc := set_fetch (test^.operand[2], set_lwb, set_len, no_preference, false);
      reg := get_reg (bits_per_unit);

      if set_loc.nargs = 0 then (* x in [] *)
	move_immediate (ord (negate), reg_addr (reg))

      else if set_loc.nargs < 3 then begin (* range or singleton *)
	mem := fetch_fullword (test^.operand[1]);
	defn1 := make_def (local_def);
	if adr_equal (set_loc.arg[1], set_loc.arg[2]) then begin (* x in [y] *)
	  gen1 (clrl, reg_addr (reg)); (* preload false *)
	  gen2 (cmpl, mem, set_loc.arg[1]);
	  if negate then
	    gen_branch (beql, defn1)
	  else
	    gen_branch (bneq, defn1);
	  gen1 (incl, reg_addr (reg)) (* change to true *)
	end
	else begin (* x in [y..z] *)
	  move_immediate (ord (negate), reg_addr (reg)); (* assume its not in *)
	  gen2 (cmpl, mem, set_loc.arg[1]);
	  gen_branch (blss, defn1); (* bail out if that assumption's correct *)
	  gen2 (cmpl, mem, set_loc.arg[2]);
	  gen_branch (bgtr, defn1); (*  "    "  "   "        "          "    *)
	  if negate then
	    gen1 (clrl, reg_addr (reg))
	  else
	    gen1 (incl, reg_addr (reg))
	end;
	mark_def (code_area, defn1);
	free (mem)
      end

      else begin (* x in <set> *)
	mem := fetch_fullword (test^.operand[1]);
	defn1 := nil;
	prep_bitreference (mem, bit_position, min_val, max_val, set_loc, defn1);
	gen4 (extzv, bit_position, typ_int_value (1, vax_byte), set_loc.arg[1], reg_addr (reg));
	if defn1 <> nil then begin (* range checking code was emitted? *)
	  defn2 := make_def (local_def);
	  gen_branch (brb, defn2);
	  mark_def (code_area, defn1);
	  gen1 (clrl, reg_addr (reg));
	  mark_def (code_area, defn2)
	end;
	if negate then
	  bool_negate (reg);
	free (mem)
      end;

      set_free (set_loc)
    end (* in_op *);

    others:
      reg := bool_long_compare (test, negate)

  end (* case *);

  load_bool := reg;
end;
$PAGE bool_value
(* BOOL VALUE returns a flag indicating whether or not an expression ("exp") is
   a boolean value reference.  Note that it is assumed that the node
   is or has been converted to type boolean.  Note also, that other tuples having
   boolean values are ignored here so that TEST AND JUMP can take advantage of the
   fact that they will have to be loaded, and thus a tst instruction isn't necessary.
   See eoln, eof, etc.  *)

function bool_value (exp: expr): boolean;
 begin
  bool_value := (exp^.opcode = cst_ref) orif
		(exp^.opcode = ident_ref) orif
		(exp^.opcode = field_ref) orif
		(exp^.opcode = array_ref) orif
		(exp^.opcode = buffer_ref) orif
		(exp^.opcode = ptr_ref) orif
	        (exp^.opcode = func_call_op);
 end;
$PAGE test_zero_jump
(* TEST ZERO JUMP compares a single precision value with zero (exp :: 0), and if
   a specified condtion ("cmp") is met, branches to a specified label ("loc"),
   otherwise execution continues at the following location.   *)

procedure test_zero_jump (cmp: cmp_codes; exp: expr; loc: def);

 var
   mem: addr_desc;
   reg: registers;
   value: int_type;

 begin
  mem := fetch (exp, no_preference );			(* get the value  *)
  if aconstp ( mem, value ) then begin	(* fold test if possible *)
    compare_constants ( cmp, value, 0, loc );
  end
  else begin
    gen1 (typ_opc (tstl, mem.byte_size), mem);
    gen_branch (branch_codes[ cmp, alignment (exp)], loc);
    free (mem);
  end;
 end;
$PAGE test_single_and_jump
(* TEST SINGLE AND JUMP evaluates a "test" on single word values and jumps to
   "tloc" or "floc" depending on whether the test is true or false.  "Jump_if_false"
   indicates on what condition a jump should occur and on what condition a the
   code should continue.  Jump_if_false = true => jump if false; false => jump if
   true. *)

procedure test_single_and_jump (test: expr; jump_if_false: boolean; var tloc, floc: def);
 var tcmp: cmp_codes;
 begin
  with test^ do begin
    if compare_with_zero (operand[1], subopcodes [opcode], tcmp)
      then begin
	if jump_if_false
	  then test_zero_jump (inverse [tcmp], operand[2], floc)
	  else test_zero_jump (tcmp, operand[2], tloc);
      end
    else if compare_with_zero (operand[2], reverse [subopcodes [opcode]], tcmp)
      then begin
	if jump_if_false
	  then test_zero_jump (inverse [tcmp], operand[1], floc)
	  else test_zero_jump (tcmp, operand[1], tloc);
      end
    else begin
      if jump_if_false
	then test_and_branch (test, jump_if_false, floc)
	else test_and_branch (test, jump_if_false, tloc);
    end;
  end (* with *) ;
 end;
$PAGE test_and_jump
(* TEST AND JUMP generates code to perform an arbitrary test and branch to
   specified locations on the outcome of the test. *)

public procedure test_and_jump
  (  test: expr;                     (* the test to evaluate *)
     true_loc: tuple;                    (* label node to goto if test true *)
     false_loc: tuple;                    (* label node to goto if test false *)
     next_loc: tuple   );                (* label node following branch, if known; else nil *)

  var
    jump_if_false, (* biases sense of tests and jumps *)
    fixed: boolean;
    tloc, floc: def;
    mem, bit_position: addr_desc;
    set_lwb, set_len: set_range;
    min_val, max_val: int_type;
    set_loc: set_desc;

begin

  (* Select the condition in which we 'fall through' to the next_loc label. This
     will be used to bias the sense of the jumps performed. *)

  jump_if_false := false_loc <> next_loc;

  (* Get the definition records for the branch targets. *)

  tloc := get_def (label_def, true_loc^.block_order_no);
  floc := get_def (label_def, false_loc^.block_order_no);

  (* If the node to be tested is a boolean value (variable or multi-use expression),
     then load the value and test it against zero. *)

  if bool_value (test) then
    if jump_if_false
      then test_zero_jump (eqc, test, floc)
      else test_zero_jump (nec, test, tloc)

  (* If this is not a value, then perform the indicated comparision. *)

  else
    case test^.opcode of

      ile_op..ine_op, ptreq_op, ptrne_op:
	test_single_and_jump (test, jump_if_false, tloc, floc);

      slt_op, sle_op, sne_op, seq_op, sge_op, sgt_op:
	begin	(* operand is str_comp_op giving "sign" of comparision *)
	  str_compare (test);	(* leaves condition codes set *)
	  if jump_if_false
	    then gen_branch (branch_codes [inverse [subopcodes [test^.opcode]], unsigned_value], floc)
	    else gen_branch (branch_codes [subopcodes [test^.opcode], unsigned_value], tloc);
	end;

      eof_op, eoln_op, eopage_op, masked_op, pending_op: begin
	(* NOTE - it is ASSUMED here that the last instruction emitted because
	   of the fetch will properly set the condition codes.  That is why these
	   tuples are handled here rather than allowing bool_value to cause there
	   handling by test_zero_jump, earlier. *)
	mem := fetch (test, no_preference );
	if jump_if_false
	  then gen_branch (beql, floc)
	  else gen_branch (bneq, tloc);
	free (mem);
      end;

      in_op: begin
	genset_op_bounds (test^.operand[1], min_val, max_val, fixed);
	set_lwb := max (0, min_val);
	if (max_val - set_lwb) >= set_size_limit then
	  set_len := set_size_limit
	else
	  set_len := max (0, max_val - set_lwb + 1);
	set_loc := set_fetch (test^.operand[2], set_lwb, set_len, no_preference, false);

	if set_loc.nargs = 0 then begin (* x in [] *)
	  if jump_if_false then
	    gen_branch (brb, floc)
	end

	else if set_loc.nargs < 3 then begin (* range or singleton *)
	  mem := fetch_fullword (test^.operand[1]);
	  if adr_equal (set_loc.arg[1], set_loc.arg[2]) then begin (* x in [y] *)
	    gen2 (cmpl, mem, set_loc.arg[1]);
	    if jump_if_false then
	      gen_branch (bneq, floc)
	    else
	      gen_branch (beql, tloc)
	  end
	  else begin (* x in [y..z] *)
	    gen2 (cmpl, mem, set_loc.arg[1]);
	    gen_branch (blss, floc);
	    gen2 (cmpl, mem, set_loc.arg[2]);
	    if jump_if_false then
	      gen_branch (bgtr, floc)
	    else
	      gen_branch (bleq, tloc)
	  end;
	  free (mem)
	end

	else begin  (* x in <set> *)
	  mem := fetch_fullword (test^.operand[1]);
	  prep_bitreference (mem, bit_position, min_val, max_val, set_loc, floc);
	  if jump_if_false then
	    gen_bb (bbc, bit_position, set_loc.arg[1], floc)
	  else
	    gen_bb (bbs, bit_position, set_loc.arg[1], tloc);
	  free (mem)
	end;

	set_free (set_loc)
      end (* in_op *);

      others:
	begin
	  if jump_if_false
	    then test_and_branch (test, jump_if_false, floc)
	    else test_and_branch (test, jump_if_false, tloc);
	end

    end (* case *);

  if jump_if_false and (true_loc <> next_loc)		(* if true case falls through *)
    then gen_branch (brb, tloc)	(* then branch to true label if not already there *)
end.
   4F7w