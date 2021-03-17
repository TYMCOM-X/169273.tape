$TITLE M68CMP - M68000 Boolean Expression Code

module m68cmp options check;
$PAGE includes
$SYSTEM pascal
$SYSTEM pasist
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68utl
$SYSTEM m68cgu
$SYSTEM m68exp
$SYSTEM m68str
$SYSTEM m68set
$SYSTEM m68gen
$SYSTEM ptmcon.inc
$SYSTEM pasmth.inc


public function fetchboolean ( test_expr : expr; negate, expr_uses_ok : boolean ): op_desc;
  forward;
$PAGE relations

(*  OP_REL[OP] is the relation tested by the tuple opcode OP.  *)

const
    op_rel : array [ile_op..filne_op] of relations =
      ( lec,	ltc,	gtc,	gec,	eqc,	nec,	(* ile_op .. ine_op *)
	lec,	ltc,	gtc,	gec,	eqc,	nec,	(* rle_op .. rne_op *)
	lec,	ltc,	gtc,	gec,	eqc,	nec,	(* sle_op .. sne_op *)
	lec,	gec,	eqc,	nec,			(* setle_op .. setne_op *)
	eqc,	nec,					(* ptreq_op .. ptrne_op *)
	eqc,	nec );					(* fileq_op .. filne_op *)


(*  "X REL Y" == "Y REVERSE[REL] X".  *)

const
    reverse : array [relations] of relations =
      ( gtc, gec, eqc, nec, lec, ltc, fc, tc );


(*  "X COND_INVERSE[FALSE,REL] Y" == "X REL Y".
    "X COND_INVERSE[TRUE,REL] Y" == NOT "X REL Y".  *)

const
    cond_inverse : array [boolean, relations] of relations =
      ( ( ltc, lec, eqc, nec, gec, gtc, fc, tc ),
	( gec, gtc, nec, eqc, ltc, lec, tc, fc ) );
$PAGE condition code mappings
(*  SGN_CMP_COND[REL] is the condition code which will cause a branch if
    X REL Y, and X and Y are signed integers which have been compared with
    "CMP Y,X", or X and Y are strings or reals which have been compared
    with a string or real runtime comparison call (PUSH X, PUSH Y, JSR
    COMPARE).  *)

const
    sgn_cmp_cond : array [relations] of condition_codes =
      ( lt_cc, le_cc, eq_cc, ne_cc, ge_cc, gt_cc, f_cc, t_cc );


(*  UNS_CMP_COND[REL] is the condition code which will cause a branch if
    X REL Y, and X and Y are unsigned integers which have been compared
    with "CMP Y,X".  *)

const
    uns_cmp_cond : array [relations] of condition_codes =
      ( cs_cc, ls_cc, eq_cc, ne_cc, cc_cc, hi_cc, f_cc, t_cc );


(*  SGN_TST_COND[REL] is the condition code which will cause a branch if
    X REL 0, and X is a signed integer which has been tested with "TST X".  *)

const
    sgn_tst_cond : array [relations] of condition_codes =
      ( lt_cc, le_cc, eq_cc, ne_cc, ge_cc, gt_cc, f_cc, t_cc );


(*  UNS_TST_COND[REL] is the condition code which will cause a branch if
    X REL 0, and X is an unsigned integer which has been tested with "TST X".  *)

const
    uns_tst_cond : array [relations] of condition_codes =
      ( f_cc, eq_cc, eq_cc, ne_cc, t_cc, ne_cc, f_cc, t_cc );
$PAGE compare_with_zero
(*  COMPARE WITH ZERO considers an operand of an integer comparison and
    the relation to be tested, and determines whether the comparison can
    be done with a TST (i.e. a comparison to zero).   OP is the operand
    and REL is the relation to be tested.  If COMPARE WITH ZERO returns
    TRUE, then the sequence "TST X; branch if REL1" will be equivalent
    to the sequence "CMP OP,X; branch if REL".  *)

function compare_with_zero ( op : op_desc;
			     rel : relations;
			     var rel1 : relations
						  ) : boolean;

var op_val : integer;

begin
  compare_with_zero := true;

  if not aconstp (op, op_val) then
    compare_with_zero := false			(* op is not constant *)

  else if op_val = 0 then			(* X REL 0 *)
    rel1 := rel

  else if (op_val = 1) and (rel = ltc) then	(* X LT 1 == X LE 0 *)
    rel1 := lec

  else if (op_val = 1) and (rel = gec) then	(* X GE 1 == X GT 0 *)
    rel1 := gtc

  else if (op_val = -1) and (rel = gtc) then	(* X GT -1 == X GE 0 *)
    rel1 := gec

  else if (op_val = -1) and (rel = lec) then	(* X LE -1 == X LT 0 *)
    rel1 := ltc

  else						(* no special case fits *)
    compare_with_zero := false;
end (* compare_with_zero *);
$PAGE compare_constants
(*  COMPARE CONSTANTS compares two integer constants, and then invokes an
    action procedure with either a T_CC or F_CC condition code, depending
    on whether the constants satisfy a specified relation.  *)

procedure compare_constants ( cst1, cst2 : integer;
			      rel : relations;
			      action : action_proc );

var test_succeeds : boolean;

begin
  if cst1 < cst2 then
    test_succeeds := (rel in [ltc, lec, nec])
  else if cst1 = cst2 then
    test_succeeds := (rel in [lec, eqc, gec])
  else
    test_succeeds := (rel in [nec, gec, gtc]);

  if test_succeeds
    then action (t_cc)
    else action (f_cc);
end (* compare_constants *);
$PAGE test
(*  TEST tests an operand and performs a specified action based on whether
    the operand has a specified relation to zero.  *)

procedure test ( op : op_desc; rel : relations; action : action_proc );

var val : integer;

begin

  (*  If the operand is constant, fold the test.  *)

  if aconstp (op, val) then
    compare_constants (val, 0, rel, action)

  (*  Otherwise, test the operand and generate a conditional branch.  *)

  else begin
    gen_m (tst_opc, op);
    if op.signed_value
      then action (sgn_tst_cond [rel])
      else action (uns_tst_cond [rel])
  end
end (* test *);
$PAGE compare_integer_ops
(*  COMPARE INTEGER OPS is called with a pair of descriptors, a relation, and an action;
    it performs the action, based on whether the two integers satisfy the relation.  *)

public procedure compare_integer_ops ( var op1, op2 : op_desc;
					   rel : relations; action : action_proc );

var cst1, cst2 : integer;
    relc : relations;
    compare_signed : boolean;

begin

  (*  If both operands are constant, the test can be completed at compile time.  *)

  if aconstp (op1, cst1) andif aconstp (op2, cst2) then
    compare_constants (cst1, cst2, rel, action)

  (*  Check whether the comparison can be reduced to a test on one operand.  *)

  else if compare_with_zero (op1, reverse [rel], relc) then
    test (op2, relc, action)

  else if compare_with_zero (op2, rel, relc) then
    test (op1, relc, action)

  (*  No, we really have to compare the two values.  *)

  else begin

    (*  First, make sure that they are the same size.  *)

    if op1.value_size < op2.value_size then
      op1 := coerce (op1, all_modes, null_mode, [op2.value_size], false);
    if op2.value_size < op1.value_size then
      op2 := coerce (op2, all_modes, null_mode, [op1.value_size], false);

    (*  If both operands are signed, we can use a signed compare.  If both
	are unsigned or known to be positive, we can use an unsigned compare.
	Otherwise, we must extend both operands and do a signed compare.  *)

    if op1.signed_value and op2.signed_value then
      compare_signed := true
    else if (not op1.signed_value or op1.known_positive) and
	    (not op2.signed_value or op2.known_positive) then
      compare_signed := false
    else begin
      op1 := coerce (op1, all_modes, null_mode, [succ (op1.value_size)], true);
      op2 := coerce (op2, all_modes, null_mode, [succ (op2.value_size)], true);
      compare_signed := true;
    end;


    (*  A comparison to a constant between -128 and 127 can always be
	improved by getting the constant in a register with a MOVEQ.  *)

    if aconstp (op1, cst1) andif ( (cst1 >= -128) and (cst1 <= 127) ) then
      op1 := copy_dreg (op1);
    if aconstp (op2, cst2) andif ( (cst2 >= -128) and (cst2 <= 127) ) then
      op2 := copy_dreg (op2);
    (*  The only valid comparison modes are "CMP OP,REG" and "CMPI #N,OP".  *)

    if (op2.mode = immediate_mode) or (op1.mode in register_modes) then begin
      gen_mm (cmp_opc, op2, op1);
      relc := rel;
    end
    else if (op1.mode = immediate_mode) or (op2.mode in register_modes) then begin
      gen_mm (cmp_opc, op1, op2);
      relc := reverse [rel];
    end
    else begin
      op1 := copy_dreg (op1);
      gen_mm (cmp_opc, op2, op1);
      relc := rel;
    end;

    (*  Generate the conditional branch.  *)

    if compare_signed
      then action (sgn_cmp_cond [relc])
      else action (uns_cmp_cond [relc]);
  end;
end (* compare_integer_ops *);
$PAGE compare_integers
(*  COMPARE INTEGERS is called with a pair of integer expressions, a
    relation, and an action; it performs the action, based on whether the
    two expressions satisfy the relation.  In fact, this routine is a wrapper for
    COMPARE INTEGER OPS - this routine allows the caller to supply expressions
    rather than op_descs.  *)

procedure compare_integers ( exp1, exp2 : expr; rel : relations; action : action_proc );

var op1, op2 : op_desc;
    op1_guard : guard_key;
    op2_modes : addr_mode_set;

begin

  (* Fetch the expressions. *)

  op1 := fetch (exp1, all_modes, null_mode, any_size, false);
  op1_guard := guard (op1);
  if op1.mode = postincrement_mode
    then op2_modes := nonstack_modes
    else op2_modes := all_modes;
  op2 := fetch (exp2, op2_modes, null_mode, any_size, false);
  op1 := unguard (op1_guard);

  (* Now do the work. *)

  compare_integer_ops (op1, op2, rel, action);
  free_desc (op1);
  free_desc (op2)
end (* compare_integers *);
$PAGE compare_reals
(*  COMPARE REALS is called with a pair of real expressions, a
    relation, and an action; it performs the action, based on whether the
    two expressions satisfy the relation.  *)

procedure compare_reals ( exp1, exp2 : expr; rel : relations; action : action_proc );

var op1, op2 : op_desc;
    op1_guard : guard_key;
    relc : relations;

begin
  if exp1^.desc.precision = maximum (prec_type) then begin
    pusha (dfetch (exp1, descriptors [null_mode], false));
    pusha (dfetch (exp2, descriptors [null_mode], false));
    dfpop;
    dfpop;
    gen_rt (rt_dcmp);
    action (sgn_cmp_cond [rel]);
  end

  else begin
    push (fetch (exp1, all_modes, null_mode, long_only, true), size_long);
    op1_guard := guard (pop_long);
    op2 := fetch (exp2, all_modes, null_mode, long_only, true);
    op1 := unguard (op1_guard);
    relc := rel;
    if op2.mode <> postincrement_mode then begin
      push (op1, size_long);
      push (op2, size_long);
    end
    else if op1.mode = postincrement_mode then
      push (op2, size_long)
    else begin
      push (op1, size_long);
      relc := reverse [rel];
    end;
    gen_rt (rt_fcmp);
    action (sgn_cmp_cond [relc]);
  end;
end (* compare_reals *);
$PAGE compare_strings
(*  COMPARE STRINGS is called with a pair of string expressions, a
    relation, and an action; it performs the action, based on whether the
    two expressions satisfy the relation.  *)

procedure compare_strings ( exp1, exp2 : expr; rel : relations; action : action_proc );

begin
  str_compare (exp1, exp2);
  action (sgn_cmp_cond [rel]);
end (* compare_strings *);
$PAGE complement
(*  COMPLEMENT takes a boolean expression and returns its complement.  *)

function complement ( op : op_desc ) : op_desc;

var bval : integer;

begin
  if aconstp (op, bval) then
    complement := int_desc (1 - bval, no_size, false)
  else begin
    complement := copy_dreg (op);
    complement.value_size := size_byte;
    gen_im (eor_opc, 1, complement);
  end;
end (* complement *);
$PAGE and_or_test
(*  AND OR TEST evaluates an AND or OR tuple, returning a descriptor for a
    boolean value.  *)

function and_or_test ( exp1, exp2 : expr; opc : tuple_opcodes; negate : boolean ) : op_desc;

var op1, op2 : op_desc;
    op1_guard : guard_key;
    op2_modes : addr_mode_set;
    de_morgan : boolean;
    and_or : tuple_opcodes;
    bval : integer;

const
    fetchable_ops : set of tuple_opcodes =
      [ first_data_ref..last_data_ref, func_call_op, sclcvt_op,
	eof_op, eopage_op, eoln_op, masked_op, pending_op];

begin
  de_morgan := negate and
	       not ( (exp1^.opcode in fetchable_ops) and (exp2^.opcode in fetchable_ops) );
  op1 := fetchboolean (exp1, de_morgan, false);
  op1_guard := guard (op1);
  op2 := fetchboolean (exp2, de_morgan, false);
  op1 := unguard (op1_guard);
  if de_morgan
    then and_or := tuple_opcodes (ord (and_op) + ord (or_op) - ord (opc))
    else and_or := opc;

  (*  Check for a constant operand.  *)

  if aconstp (op1, bval) then begin
    if ( (and_or = and_op) and (bval = 1) ) or ( (and_or = or_op) and (bval = 0) ) then
      and_or_test := op2
    else begin
      free_desc (op2);
      if and_or = and_op
	then and_or_test := int_desc (0, no_size, false)
	else and_or_test := int_desc (1, no_size, false);
    end;
  end

  else if aconstp (op2, bval) then begin
    if ( (and_or = and_op) and (bval = 1) ) or ( (and_or = or_op) and (bval = 0) ) then
      and_or_test := op1
    else begin
      free_desc (op1);
      if and_or = and_op
	then and_or_test := int_desc (0, no_size, false)
	else and_or_test := int_desc (1, no_size, false);
    end;
  end

  (*  If a computation must be performed, then the destination must be
      in a unique data register.  *)

  else begin
    if (op1.mode = dreg_mode) andif (uses_remaining (op1.reg) = 1) then
      and_or_test := op1
    else if (op2.mode = dreg_mode) andif (uses_remaining (op2.reg) = 1) then begin
      and_or_test := op2;
      op2 := op1;
    end
    else begin
      and_or_test := copy_dreg (op2);
      op2 := op1;
    end;

    if and_or = and_op then begin
      gen_mr (and_opc, op2, and_or_test.reg);
      and_or_test.value_size := size_byte;
      and_or_test.extended_size := max (and_or_test.extended_size, op2.value_size);
    end
    else begin
      gen_mr (or_opc, op2, and_or_test.reg);
      and_or_test.value_size := size_byte;
      and_or_test.extended_size := min (and_or_test.extended_size, op2.value_size);
    end;
    free_desc (op2);
  end;

  if negate and not de_morgan then
    and_or_test := complement (and_or_test);
end (* and_or_test *);
$PAGE set_in_test
(* SET IN TEST evaluates an in_op expression in either a boolean or branching
   context.  Note that this routine implicitly knows what the ACTION procedure will
   do.  It's used as a convenience, not as an abstraction technique.  You cannot
   change the effect of this routine simply by changing ACTION.  *)

procedure set_in_test (bool_ctxt: boolean; var bool_result: op_desc;
		       exp1, exp2: expr; var floc: def;
		       negate: boolean; action: action_proc);

var
  mem, bit_pos, byte_loc: op_desc;
  fixed: boolean;
  set_loc: set_desc;
  skip_label: def;
  min_val, max_val: integer;
  set_lwb, set_len: set_range;

  procedure f_branch (cc: condition_codes);
    begin
      gen_bcc (cc, floc)
    end;

begin

  (* Determine what we've got. *)

  gset_op_bounds (exp1, min_val, max_val, fixed);
  set_lwb := max (0, min_val);
  if (max_val - set_lwb) >= set_size_limit then
    set_len := set_size_limit
  else
    set_len := max (0, max_val - set_lwb + 1);
  set_loc := set_fetch (exp2, set_lwb, set_len, no_preference, false);

  (* Perform the test. *)

  mem := fetch (exp1, data_modes, dreg_mode, reg_sizes, false);
  if set_loc.nargs = 0 then begin (* x in [] *)
    free_desc (mem);
    action (sgn_tst_cond [cond_inverse [negate, fc]])
  end

  else if set_loc.nargs < 3 then begin (* range or singleton *)
    if ops_equal (set_loc.arg[1], set_loc.arg[2]) then (* x in [y] *)
      compare_integer_ops (mem, set_loc.arg[1], cond_inverse [negate, eqc], action)
    else begin (* x in [y..z] *)
      if bool_ctxt then begin
	floc := def_create (local_def);
	bool_result := loadi (get_dreg, ord (negate), size_byte, true) (* assume its not in *)
      end;
      compare_integer_ops (mem, set_loc.arg[1], ltc, f_branch);
      if not bool_ctxt then
	compare_integer_ops (mem, set_loc.arg[2], cond_inverse [negate, lec], action)
      else begin
	compare_integer_ops (mem, set_loc.arg[2], gtc, f_branch);
	bool_result := loadi (bool_result.reg, ord (not negate), size_byte, true);
	gen_def (code_area, floc)
      end
    end;
    free_desc (mem);
    set_free (set_loc)
  end

  else begin (* x in <set> *)
    if bool_ctxt then
      floc := nil;
    prep_bitreference (mem, exp1, set_loc, bit_pos, byte_loc, floc);
    gen_mm (btst_opc, bit_pos, byte_loc);
    free_desc (bit_pos);
    free_desc (byte_loc);
    action (sgn_tst_cond [cond_inverse [negate, nec]]);
    if bool_ctxt and (floc <> nil) then begin (* range checking code was emitted? *)
      skip_label := def_create (local_def);
      gen_bcc (t_cc, skip_label);
      gen_def (code_area, floc);
      bool_result := loadi (bool_result.reg, ord (negate), size_byte, true);
      gen_def (code_area, skip_label)
    end
  end
end (* set_in_test *);
$PAGE bst_null_test_seq
(* BST NULL TEST SEQ selects the best code sequence for testing that a set is empty. *)

type null_test_sequences = (simple_null_tests, postincr_null_tests,
			    word_null_loop, longword_null_loop, variable_word_null_loop);


function bst_null_test_seq (    n: integer;
				source_indirect: boolean;
			    var bests_size, bests_cycles: integer): integer;

begin
  assert ((n = -1) or (n > 0));
  if n < 0 (* variable length *) then begin
    bst_null_test_seq := ord (variable_word_null_loop);
    bests_size := 16  - 4 * ord (source_indirect)
    (* bests_cycles undefined *)
  end
  else begin
    first_sequence (ord (postincr_null_tests),
		    6 + 4 * ((n-1) div 2)		           - 4 * ord (source_indirect),
		    8 + 6 * n + 2 * (n mod 2) + 10 * ((n-1) div 2) - 8 * ord (source_indirect));
    next_sequence  (ord (simple_null_tests),
		    4 + 6 * ((n-1) div 2)		       - 2 * ord (source_indirect),
		    8 * n + 4 * (n mod 2) + 10 * ((n-1) div 2) - 4 * ord (source_indirect));
    next_sequence  (ord (word_null_loop),
		    12		- 4 * ord (source_indirect),
		    16 + 18 * n - 8 * ord (source_indirect));
    next_sequence  (ord (longword_null_loop),
		    12 + 4 * (n mod 2)		- 4 * ord (source_indirect),
		    16 + 11 * n + 7 * (n mod 2) - 8 * ord (source_indirect));
    bst_null_test_seq := best_sequence (bests_size, bests_cycles)
  end
end (* bst_null_test_seq *);
$PAGE test_null
(* TEST NULL generates code to test a set to seit is empty.  The given
   set_desc is not freed, but it IS ASSUMED that the caller won't be doing
   anything more with this set other than to free it.  The sequences generated
   may jump out to the provided label if the set is not null, but the primary
   result is to cause the condition codes to be set when execution falls
   out the bottom of the generated sequence.  *)

procedure test_null (var source_set: set_desc; notnull_label: def);

const
  default_size: array [null_test_sequences] of op_sizes := (size_long, size_long,
							    size_word, size_long, size_word);

var
  best_seq: null_test_sequences;
  n, bests_size, bests_cycles: integer;
  remainder: 0..1;
  loop_label: def;

begin

  (* Select the best code sequence. *)

  if not aconstp (source_set.arg[3], n) then
    n := -1 (* signifies variable length *)
  else
    assert (n > 0);
  best_seq := null_test_sequences (bst_null_test_seq (n, pincr_free (source_set.arg[1]),
						      bests_size, bests_cycles));


  (* Generate the selected sequences. *)

  if pincr_free (source_set.arg[1]) or (best_seq <> simple_null_tests) then
    source_set.arg[1] := make_postincr (source_set.arg[1]);
  source_set.arg[1].value_size := default_size [best_seq];

  case best_seq of

    simple_null_tests, postincr_null_tests:
      while n > 0 do begin
        if n = 1 then
	  source_set.arg[1].value_size := size_word;
	gen_m (tst_opc, source_set.arg[1]);
	n := n - 2;
	if n > 0 then begin
	  gen_bcc (ne_cc, notnull_label);
	  if source_set.arg[1].mode <> postincrement_mode then
	    source_set.arg[1] := increment_addr (source_set.arg[1], 4)
	end
      end;

    word_null_loop, longword_null_loop, variable_word_null_loop: begin
      remainder := 0;
      if best_seq = word_null_loop then
	source_set.arg[3] := loadi (get_dreg, n - 1, size_word, true)
      else if best_seq = longword_null_loop then begin
	source_set.arg[3] := loadi (get_dreg, (n div 2) - 1, size_word, true);
	remainder := n mod 2
      end
      else begin
	source_set.arg[3] := copy_dreg (coerce (source_set.arg[3], data_modes, dreg_mode, word_min, true));
	gen_im (sub_opc, 1, source_set.arg[3])
      end;
      loop_label := def_create (local_def);
      gen_def (code_area, loop_label);
      gen_m (tst_opc, source_set.arg[1]);
      gen_dbcc (ne_cc, source_set.arg[3].reg, loop_label);
      if remainder > 0 then begin
	gen_bcc (ne_cc, notnull_label);
	source_set.arg[1].value_size := size_word;
	gen_m (tst_opc, source_set.arg[1])
      end
    end
  end;
  if source_set.arg[1].mode = postincrement_mode then
    source_set.arg[1].mode := indirect_mode
end (* test_null *);
$PAGE test_set_equality
(* TEST SET EQUALITY generates code to test whether one set is equal to another.
   The given set_desc's are not freed, but it IS ASSUMED that the caller won't be
   doing anything more with them other than to free them.  The sequences generated
   may jump out to the provided label if the sets are not equal, but the primary
   result is to cause the condition codes to be set when execution falls out
   the bottom of the generated sequence.  *)

procedure test_set_equality (var first_set, second_set: set_desc; notequal_label: def);

type
  equality_test_sequences = (simple_equal_tests, postincr_equal_tests, equal_rt_call,
			     word_equal_loop, longword_equal_loop, variable_word_equal_loop);
  strategy_choices = (composite_test, resort_to_runtime);

const
  default_size: array [equality_test_sequences] of op_sizes = (size_long, size_long, no_size,
							       size_word, size_long, size_word);

var
  bst_equal_seq: equality_test_sequences;
  best_strategy: strategy_choices;
  cst_first_len, cst_second_len, temp_len: set_range;
  num_indirect: 0..2;
  if_n_odd, remainder: 0..1;
  bests_size, bests_cycles, null_tests_size, null_tests_cycles: set_range;
  n, bst_null_seq_ord: integer;
  temp_set: set_desc;
  loop_label: def;
  dreg: data_regs;

begin
  if aconstp (first_set.arg[3], cst_first_len) and aconstp (second_set.arg[3], cst_second_len) then begin
    if cst_first_len < cst_second_len then begin
      temp_set := first_set;    temp_len := cst_first_len;
      first_set := second_set;  cst_first_len := cst_second_len;
      second_set := temp_set;   cst_second_len := temp_len
    end;
    n := cst_second_len;
    assert (n > 0);
    if_n_odd := ord (odd (n)); (* coef for terms applied just if n odd *)
    num_indirect := ord (pincr_free (first_set.arg[1]) or (cst_first_len > cst_second_len))
		  + ord (pincr_free (second_set.arg[1]));
    first_sequence (ord (postincr_equal_tests),
		    10 + 4 * ((n-1) div 2)                          - 4 * num_indirect,
		    16 + 10 * n + 2 * if_n_odd + 10 * ((n-1) div 2) - 8 * num_indirect);
    next_sequence  (ord (simple_equal_tests),
		    8 + 10 * ((n-1) div 2)                      - 2 * num_indirect,
		    17 * n + 10 * if_n_odd + 10 * ((n-1) div 2) - 4 * num_indirect);
    next_sequence  (ord (word_equal_loop),
		    16          - 4 * num_indirect,
		    24 + 22 * n - 8 * num_indirect);
    next_sequence  (ord (longword_equal_loop),
		    16 + 4 * if_n_odd           - 4 * num_indirect,
		    24 + 15 * n + 22 * if_n_odd - 8 * num_indirect);
    bst_equal_seq := equality_test_sequences (best_sequence (bests_size, bests_cycles));
    if cst_first_len > cst_second_len then begin
      if not pincr_free (first_set.arg[1]) then begin
	bests_size := bests_size + 4; (* account for the assumed extra LEA *)
	bests_cycles := bests_cycles + 8;
	num_indirect := num_indirect - 1 (* readjust for resort_to_runtime calc. *)
      end;
      bst_null_seq_ord := bst_null_test_seq (cst_first_len - cst_second_len, true,
					     null_tests_size, null_tests_cycles);
      first_sequence (ord (composite_test),
		      bests_size + null_tests_size + 2 (* for BNE *),
		      bests_cycles + null_tests_cycles + 10 (* for BNE *));
      next_sequence  (ord (resort_to_runtime),
		      22                                                   - 2 * num_indirect,
		      340 + 15 * n + 11 * (cst_first_len - cst_second_len) - 4 * num_indirect);
      best_strategy := strategy_choices (best_sequence (bests_size, bests_cycles));
      if best_strategy = resort_to_runtime then
	bst_equal_seq := equal_rt_call
      else
        first_set.arg[1] := make_postincr (first_set.arg[1])
    end
  end
  else if not ops_equal (first_set.arg[3], second_set.arg[3]) then
    bst_equal_seq := equal_rt_call
  else (* equal variable length *)
    bst_equal_seq := variable_word_equal_loop;


  (* Generate the selected sequences. *)

  if (bst_equal_seq <> equal_rt_call) then begin
    if pincr_free (first_set.arg[1]) or (bst_equal_seq <> simple_equal_tests) then
      first_set.arg[1] := make_postincr (first_set.arg[1]);
    if pincr_free (second_set.arg[1]) or (bst_equal_seq <> simple_equal_tests) then
      second_set.arg[1] := make_postincr (second_set.arg[1])
  end;
  first_set.arg[1].value_size := default_size [bst_equal_seq];
  second_set.arg[1].value_size := default_size [bst_equal_seq];

  case bst_equal_seq of

    simple_equal_tests: begin
      assert (n <= 2); (* if the numbers work out as I expect *)
      if n = 1 then begin
	first_set.arg[1].value_size := size_word;
	second_set.arg[1].value_size := size_word
      end;
      dreg := get_dreg;
      gen_mr (move_opc, first_set.arg[1], dreg);
      gen_mr (cmp_opc, second_set.arg[1], dreg);
      free_reg (dreg)
    end;

    postincr_equal_tests:
      while n > 0 do begin
	if n = 1 then begin
	  first_set.arg[1].value_size := size_word;
	  second_set.arg[1].value_size := size_word
	end;
	gen_mm (cmp_opc, first_set.arg[1], second_set.arg[1]);
	n := n - 2;
	if n > 0 then
	  gen_bcc (ne_cc, notequal_label)
      end;

    equal_rt_call: begin
      pusha (duplicate_desc (first_set.arg[1]));
      push (duplicate_desc (first_set.arg[3]), size_long);
      pusha (duplicate_desc (second_set.arg[1]));
      push (duplicate_desc (second_set.arg[3]), size_long);
      gen_rt (rt_set_equality)
    end;

    word_equal_loop, longword_equal_loop, variable_word_equal_loop: begin
      remainder := 0;
      if bst_equal_seq = word_equal_loop then
	first_set.arg[3] := loadi (get_dreg, n - 1, size_word, true)
      else if bst_equal_seq = longword_equal_loop then begin
	first_set.arg[3] := loadi (get_dreg, (n div 2) - 1, size_word, true);
	remainder := n mod 2
      end
      else begin
	first_set.arg[3] := copy_dreg (coerce (first_set.arg[3], data_modes, dreg_mode, word_min, true));
	gen_im (sub_opc, 1, first_set.arg[3])
      end;
      loop_label := def_create (local_def);
      gen_def (code_area, loop_label);
      gen_mm (cmp_opc, first_set.arg[1], second_set.arg[1]);
      gen_dbcc (ne_cc, first_set.arg[3].reg, loop_label);
      if remainder > 0 then begin
	gen_bcc (ne_cc, notequal_label);
	first_set.arg[1].value_size := size_word;
        second_set.arg[1].value_size := size_word;
	gen_mm (cmp_opc, first_set.arg[1], second_set.arg[1])
      end
    end
  end;

  if first_set.arg[1].mode = postincrement_mode then
    first_set.arg[1].mode := indirect_mode; (* keep life simple *)
  if second_set.arg[1].mode = postincrement_mode then
    second_set.arg[1].mode := indirect_mode;
  if not (bst_equal_seq in [equal_rt_call, variable_word_equal_loop]) andif
     (cst_first_len > cst_second_len) then begin
    gen_bcc (ne_cc, notequal_label);
    first_set.arg[3] := int_desc (cst_first_len - cst_second_len, no_size, true);
    test_null (first_set, notequal_label)
  end
end (* test_set_equality *);
$PAGE test_inclusion
(* TEST INCLUSION generates code to test whether one set is a subset of another.
   The given set_desc's are not freed, but it IS ASSUMED that the caller won't be
   doing anything more with them other than to free them.  The sequences generated
   may jump out to the provided label if the subset relation doesn't hold, but the
   primary result is to cause the condition codes to be set when execution falls out the
   bottom of the generated sequence.  *)

procedure test_inclusion (var first_set, second_set: set_desc; notincluded_label: def);

type
  incl_test_sequences = (simple_incl_tests, postincr_incl_tests, incl_rt_call,
			 word_incl_loop, longword_incl_loop);

const
  default_sizes: array [incl_test_sequences] of op_sizes := (size_long, size_long,
							     no_size, size_word, size_long);

var
  bst_incl_seq: incl_test_sequences;
  n, bests_size, bests_cycles: integer;
  loop_count: set_range;
  num_indirect: 0..2;
  loop_label: def;
  temp_len: op_desc;

procedure actual_incl_seq;
  var
    dreg: data_regs;
  begin
    dreg := get_dreg;
    gen_mr (move_opc, second_set.arg[1], dreg);
    gen_r (not_opc, dreg, second_set.arg[1].value_size);
    gen_mr (and_opc, first_set.arg[1], dreg);
    free_reg (dreg)
  end;

begin

  (* Select the best code sequence. *)

  assert (ops_equal (first_set.arg[3], second_set.arg[3]));
  if not aconstp (first_set.arg[3], n) then
    bst_incl_seq := incl_rt_call
  else begin
    assert (n > 0);
    num_indirect := ord (pincr_free (first_set.arg[1])) + ord (pincr_free (second_set.arg[1]));
    first_sequence (ord (postincr_incl_tests),
		    14 + 8 * ((n-1) div 2)                           - 4 * num_indirect,
		    16 + 16 * n + 4 * (n mod 2) + 10 * ((n-1) div 2) - 8 * num_indirect);
    next_sequence  (ord (simple_incl_tests),
		    10 + 12 * ((n-1) div 2)                     - 2 * num_indirect,
		    20 * n + 8 * (n mod 2) + 10 * ((n-1) div 2) - 4 * num_indirect);
    next_sequence  (ord (incl_rt_call),
		    18           - 2 * num_indirect,
		    340 + 21 * n - 4 * num_indirect);
    next_sequence  (ord (word_incl_loop),
		    20          - 4 * num_indirect,
		    24 + 30 * n - 8 * num_indirect);
    next_sequence  (ord (longword_incl_loop),
		    20 + 8 * (n mod 2)           - 4 * num_indirect,
		    24 + 21 * n + 30 * (n mod 2) - 8 * num_indirect);
    bst_incl_seq := incl_test_sequences (best_sequence (bests_size, bests_cycles))
  end;

  (* Generate the selected sequence. *)

  if bst_incl_seq <> incl_rt_call then begin
    if (bst_incl_seq <> simple_incl_tests) orif pincr_free (first_set.arg[1]) then
      first_set.arg[1] := make_postincr (first_set.arg[1]);
    if (bst_incl_seq <> simple_incl_tests) orif pincr_free (second_set.arg[1]) then
      second_set.arg[1] := make_postincr (second_set.arg[1])
  end;
  first_set.arg[1].value_size := default_sizes [bst_incl_seq];
  second_set.arg[1].value_size := default_sizes [bst_incl_seq];

  case bst_incl_seq of

    simple_incl_tests, postincr_incl_tests: begin
      if n > 2 then begin
	assert ((n <= 3) and (bst_incl_seq <> simple_incl_tests));
	actual_incl_seq;
	gen_bcc (ne_cc, notincluded_label)
      end;
      if odd (n) then begin
	first_set.arg[1].value_size := size_word;
	second_set.arg[1].value_size := size_word
      end;
      actual_incl_seq
    end;

    incl_rt_call: begin
      pusha (duplicate_desc (first_set.arg[1]));
      pusha (duplicate_desc (second_set.arg[1]));
      push (duplicate_desc (first_set.arg[3]), size_long);
      gen_rt (rt_set_inclusion)
    end;

    word_incl_loop, longword_incl_loop: begin
      if bst_incl_seq = word_incl_loop then
	loop_count := n
      else begin
	loop_count := n div 2;
	assert (not odd (n))
      end;
      temp_len := loadi (get_dreg, loop_count - 1, size_word, true);
      loop_label := def_create (local_def);
      gen_def (code_area, loop_label);
      actual_incl_seq;
      gen_dbcc (ne_cc, temp_len.reg, loop_label);
      free_desc (temp_len)
    end
  end
end (* test_inclusion *);
$PAGE set_equality
(* SET EQUALITY evaluates a seteq_op or setne_op in either a boolean or branching
   context.  Note that this routine implicitly knows what the ACTION procedure will
   do.  It's used as a convenience, not as an abstraction technique.  You cannot
   change the effect of this routine simply by changing ACTION.  *)

procedure set_equality (bool_ctxt: boolean; var bool_result: op_desc;
		        exp1, exp2: expr; rel: relations;
			var target_if_rel, fall_out: def; action: action_proc);

var
  first_set, second_set, temp_set: set_desc;
  expr1, expr2, temp_expr: expr;
  desired_rel: relations;
  first_sing, second_sing: boolean;
  equal_label, notequal_label, skip_label: def;
  ctxt_base, ctxt_upb, cst_word_length, range_set_upb: set_range;
  temp: op_desc;
  low, high: integer;

procedure equal (cc: condition_codes);
  begin
    gen_bcc (cc, equal_label)
  end;

procedure notequal (cc: condition_codes);
  begin
    gen_bcc (cc, notequal_label)
  end;

procedure skip (cc: condition_codes);
  begin
    if cc <> f_cc then
      gen_bcc (cc, skip_label)
  end;

begin
  expr1 := exp1;
  expr2 := exp2;
  first_set := set_fetch (expr1, 0, set_size_limit, no_preference, false);
  second_set := set_fetch (expr2, 0, set_size_limit, no_preference, false);

  (* Make matrix of possibilities triangular. *)

  if (first_set.nargs < second_set.nargs) or
     (((first_set.nargs = 3) and (second_set.nargs = 3)) andif
           (first_set.arg[2].cst_part.offset < second_set.arg[2].cst_part.offset)) then begin
    temp_set := first_set;   temp_expr := expr1;
    first_set := second_set; expr1 := expr2;
    second_set := temp_set;  expr2 := temp_expr
  end;

  (* Emit code for first_set "rel" second_set. *)

  case second_set.nargs of

    (* First, take second_set null (first_set.nargs in [0, 2, 3]). *)

    0: case first_set.nargs of
         0:  (* [] = [] *)
	   if rel = eqc then
	     action (sgn_tst_cond [tc])
	   else
	     action (sgn_tst_cond [fc]);
         2: 
	   if not ops_equal (first_set.arg[1], first_set.arg[2]) then begin (* [x..y] = [] *)
	     if rel = eqc then
	       desired_rel := gtc
	     else
	       desired_rel := lec;
	     compare_integer_ops (first_set.arg[1], first_set.arg[2], desired_rel, action)
	   end
	   else (* [x] = [] *)
	     if rel = eqc then
	       action (sgn_tst_cond [fc])
	     else
	       action (sgn_tst_cond [tc]);
         3: begin (* <set> = [] *)
	   if bool_ctxt then
	     notequal_label := def_create (local_def)
	   else if rel = eqc then
	     notequal_label := fall_out
	   else
	     notequal_label := target_if_rel;
	   test_null (first_set, notequal_label);
	   if bool_ctxt then
	     gen_def (code_area, notequal_label);
	   action (sgn_tst_cond [rel])
	 end
       end;

    (* Second_set range or singleton (first_set.nargs in [2, 3]). *)

    2: begin
      first_sing := (first_set.nargs < 3) andif ops_equal (first_set.arg[1], first_set.arg[2]);
      second_sing := (second_set.nargs < 3) andif ops_equal (second_set.arg[1], second_set.arg[2]);
      if first_sing and second_sing then (* [w] = [y] *)
	compare_integer_ops (first_set.arg[1], second_set.arg[1], rel, action)
      else begin
	if bool_ctxt then begin
	  fall_out := def_create (local_def);
	  bool_result := loadi (get_dreg, ord (rel = eqc), size_byte, true);  (* assume sets equal *)
	  equal_label := fall_out;
	  notequal_label := def_create (local_def)
	end
	else if rel = eqc then begin
	  notequal_label := fall_out;
	  equal_label := target_if_rel
	end
	else (* rel = nec *) begin
	  equal_label := fall_out;
	  notequal_label := target_if_rel
	end;
	if first_set.nargs < 3 then begin (* [w..x] = [y..z] *)
	  if not first_sing and not second_sing then begin
	    skip_label := def_create (local_def);
	    compare_integer_ops (first_set.arg[1], first_set.arg[2], lec, skip);
	    compare_integer_ops (second_set.arg[1], second_set.arg[2], gtc, equal);
	    gen_def (code_area, skip_label)
	  end;
	  compare_integer_ops (first_set.arg[1], second_set.arg[1], nec, notequal);
	  if bool_ctxt or (rel = eqc) then
	    compare_integer_ops (first_set.arg[2 - ord (first_sing)],
				 second_set.arg[2 - ord (second_sing)], eqc, equal)
	  else
	    compare_integer_ops (first_set.arg[2 - ord (first_sing)],
				 second_set.arg[2 - ord (second_sing)], nec, notequal)
	end
	else begin (* <set> = [x..y] *)
	  ctxt_base := first_set.arg[2].cst_part.offset * bits_per_word;
	  with expr1^.desc do
	    ctxt_upb := ngm (set_lwb + set_length, bits_per_word) - 1;
	  with expr2^.desc do
	    range_set_upb := set_lwb + set_length - 1;
	  skip_label := nil;
	  if ctxt_base > expr2^.desc.set_lwb then begin (* x could be below ctxt_base *)
	    if not second_sing then begin
	      skip_label := def_create (local_def);
	      compare_integer_ops (second_set.arg[1], second_set.arg[2], gtc, skip)
	    end;
	    temp := int_desc (ctxt_base, no_size, false);
	    compare_integer_ops (second_set.arg[1], temp, ltc, notequal);
	    free_desc (temp)
	  end;
	  if not aconstp (first_set.arg[3], cst_word_length) orif
	     (range_set_upb > ctxt_upb) then begin (* y could be above ctxt_upb *)
	    if (skip_label = nil) and not second_sing then begin
	      skip_label := def_create (local_def);
	      compare_integer_ops (second_set.arg[1], second_set.arg[2], gtc, skip)
	    end;
	    if aconstp (first_set.arg[3], cst_word_length) then
	      temp := int_desc (ctxt_base + cst_word_length * bits_per_word - 1, no_size, false)
	    else begin
	      first_set.arg[3] := coerce (first_set.arg[3], [dreg_mode], dreg_mode,
					  at_least [min (succ (first_set.arg[3].value_size), size_long)],
					  true);
	      temp := copy_dreg (duplicate_desc (first_set.arg[3]));
	      gen_im (asl_opc, 4, temp);
	      if ctxt_base > 0 then
		gen_im (add_opc, ctxt_base - 1, temp)
	      else
		gen_im (sub_opc, 1, temp)
	    end;
	    compare_integer_ops (second_set.arg[2], temp, gtc, notequal);
	    free_desc (temp)
	  end;
	  if skip_label <> nil then
	    gen_def (code_area, skip_label); (* target of null range test *)
	  if aconstp (second_set.arg[1], low) andif (low < ctxt_base) then
	    second_set.arg[1] := int_desc (ctxt_base, no_size, false);
	  if aconstp (second_set.arg[2], high) andif (high > ctxt_upb) then
	    second_set.arg[2] := int_desc (ctxt_upb, no_size, false);
	  temp_set := set_temporary (ctxt_base, first_set.arg[3]);
	  genset (second_set, dupl_set (temp_set));
	  second_set := temp_set;
	  test_set_equality (first_set, second_set, notequal_label);
	  if bool_ctxt or (rel = eqc) then
	    gen_bcc (eq_cc, equal_label)
	  else
	    gen_bcc (ne_cc, notequal_label)
	end;
	if bool_ctxt then begin
	  gen_def (code_area, notequal_label);
	  bool_result := loadi (bool_result.reg, ord (rel = nec), size_byte, true);
	  gen_def (code_area, fall_out)
	end;
      end
    end;

    (* Second_set is a set (and so first_set must be also). *)

    3: begin
      if first_set.arg[2].cst_part.offset > second_set.arg[2].cst_part.offset then begin
	if aconstp (first_set.arg[3], cst_word_length) then
	  temp := int_desc (cst_word_length + (first_set.arg[2].cst_part.offset
						- second_set.arg[2].cst_part.offset), no_size, true)
	else begin
	  temp := copy_dreg (duplicate_desc (first_set.arg[3]));
	  gen_im (add_opc, first_set.arg[2].cst_part.offset - second_set.arg[2].cst_part.offset, temp)
	end;
	temp_set := set_temporary (second_set.arg[2].cst_part.offset * bits_per_word, temp);
	free_desc (temp);
	set_move (first_set, dupl_set (temp_set));
	first_set := temp_set
      end;
      if bool_ctxt then
	notequal_label := def_create (local_def)
      else if rel = eqc then
	notequal_label := fall_out
      else
	notequal_label := target_if_rel;
      test_set_equality (first_set, second_set, notequal_label);
      if bool_ctxt then
	gen_def (code_area, notequal_label);
      action (sgn_tst_cond [rel])
    end
  end (* case second_set.nargs *);

  set_free (first_set);
  set_free (second_set)
end (* set_equality *);
$PAGE set_inclusion
(* SET INCLUSION evaluates a setle_op or setge_op in either a boolean or branching
   context.  Note that this routine implicitly knows what the ACTION procedure
   will do.   It's used as a convenience, not as an abstraction technique.  You
   cannot change the effect of this routine simply by changing ACTION.  *)

procedure set_inclusion (bool_ctxt: boolean; var bool_result: op_desc;
			 exp1, exp2 : expr; negate : boolean;
			 var jump_out_target, fall_out: def; action : action_proc );

var
  first_set, second_set, temp_set: set_desc;
  included_label, notincluded_label, skip_label: def;
  bit_pos, byte_loc, temp_desc: op_desc;
  ctxt_base, ctxt_upb, cst_word_length, range_set_upb: set_range;
  low, high: integer;
  actual_cc: condition_codes;

procedure prologue;

  begin
    if bool_ctxt then begin
      fall_out := def_create (local_def);
      bool_result := loadi (get_dreg, ord (not negate), size_byte, true); (* assume first_set <= second_set *)
      included_label := fall_out;
      notincluded_label := def_create (local_def)
    end
    else if negate then begin
      included_label := fall_out;
      notincluded_label := jump_out_target
    end
    else begin
      notincluded_label := fall_out;
      included_label := jump_out_target
    end;
  end;

procedure epilogue;

  begin
    if bool_ctxt then begin
      gen_def (code_area, notincluded_label);
      bool_result := loadi (bool_result.reg, ord (negate), size_byte, true);
      gen_def (code_area, fall_out)
    end
  end;

procedure included (cc: condition_codes);
  begin
    actual_cc := cc;
    if cc <> f_cc then
      gen_bcc (cc, included_label)
  end;

procedure notincluded (cc: condition_codes);
  begin
    gen_bcc (cc, notincluded_label)
  end;

begin
  first_set := set_fetch (exp1, 0, set_size_limit, no_preference, false);
  second_set := set_fetch (exp2, exp1^.desc.set_lwb, exp1^.desc.set_length, no_preference, false);

  (* Emit code for first_set <= second_set. *)

  case first_set.nargs of

    (* First_set null. *)

    0: (* [] <= anything *)
      action (sgn_tst_cond [cond_inverse [negate, tc]]);

    (* First_set singleton or range. *)

    2:
      if ops_equal (first_set.arg[1], first_set.arg[2]) then begin (* first_set singleton *)
	case second_set.nargs of
	  0: (* [x] <= [] *)
	    action (sgn_tst_cond [cond_inverse [negate, fc]]);
	  2:
	    if ops_equal (second_set.arg[1], second_set.arg[2]) then (* [x] <= [y] *)
	      compare_integer_ops (first_set.arg[1], second_set.arg[1], cond_inverse [negate, eqc],
				   action)
	    else begin (* [x] <= [y..z] *)
	      prologue;
	      compare_integer_ops (first_set.arg[1], second_set.arg[1], nec, notincluded);
	      if bool_ctxt or not negate then
		compare_integer_ops (first_set.arg[1], second_set.arg[2], eqc, included)
	      else
		compare_integer_ops (first_set.arg[1], second_set.arg[2], nec, notincluded);
	      epilogue
	    end;
	  3: begin (* [x] <= <set> *)
	    if bool_ctxt then
	      notincluded_label := nil
	    else if negate then
	      notincluded_label := jump_out_target
	    else
	      notincluded_label := fall_out;
	    prep_bitreference (duplicate_desc (first_set.arg[1]), first_set.lwb_exp,
			       dupl_set (second_set), bit_pos, byte_loc, notincluded_label);
	    gen_mm (btst_opc, bit_pos, byte_loc);
	    free_desc (bit_pos);
	    free_desc (byte_loc);
	    action (sgn_tst_cond [cond_inverse [negate, nec]]);
	    if bool_ctxt and (notincluded_label <> nil) then begin (* range check code was emitted? *)
	      skip_label := def_create (local_def);
	      gen_bcc (t_cc, skip_label);
	      gen_def (code_area, notincluded_label);
	      bool_result := loadi (bool_result.reg, ord (negate), size_byte, true);
	      gen_def (code_area, skip_label)
	    end
	  end
	end (* case second_set.nargs *);
      end
      else begin (* first_set range *)
	case second_set.nargs of

	  0: (* [x..y] <= [] *)
	    compare_integer_ops (first_set.arg[1], first_set.arg[2], cond_inverse [negate, gtc],
				 action);

	  2: begin
	    prologue;
	    if ops_equal (second_set.arg[1], second_set.arg[2]) then begin (* [x..y] <= [z] *)
	      compare_integer_ops (first_set.arg[1], first_set.arg[2], gtc, included);
	      if actual_cc = gt_cc then
		gen_bcc (lt_cc, notincluded_label)
	      else if actual_cc = lt_cc then
		gen_bcc (gt_cc, notincluded_label)
	      else
		compare_integer_ops (first_set.arg[1], first_set.arg[2], ltc, notincluded);
	      if bool_ctxt or not negate then
		compare_integer_ops (first_set.arg[1], second_set.arg[1], eqc, included)
	      else
		compare_integer_ops (first_set.arg[1], second_set.arg[1], nec, notincluded)
	    end
	    else begin (* [w..x] <= [y..z] *)
	      compare_integer_ops (first_set.arg[1], first_set.arg[2], gtc, included);
	      compare_integer_ops (first_set.arg[1], second_set.arg[1], ltc, notincluded);
	      if bool_ctxt or not negate then
		compare_integer_ops (first_set.arg[2], second_set.arg[2], lec, included)
	      else
		compare_integer_ops (first_set.arg[2], second_set.arg[2], gtc, notincluded)
	    end;
	    epilogue
	  end;
	
	  3: begin (* [x..y] <= <set> *)
	    prologue;
	    ctxt_base := second_set.arg[2].cst_part.offset * bits_per_word;
	    with exp2^.desc do
	      ctxt_upb := ngm (set_lwb + set_length, bits_per_word) - 1;
	    with exp1^.desc do
	      range_set_upb := set_lwb + set_length - 1;
	    if ctxt_base > exp1^.desc.set_lwb then begin (* x could be below ctxt_base *)
	      compare_integer_ops (first_set.arg[1], first_set.arg[2], gtc, included);
	      temp_desc := int_desc (ctxt_base, no_size, false);
	      compare_integer_ops (first_set.arg[1], temp_desc, ltc, notincluded);
	      free_desc (temp_desc)
	    end;
	    if not aconstp (second_set.arg[3], cst_word_length) orif
	      (range_set_upb > ctxt_upb) then begin (* y could be above ctxt_upb *)
	      if ctxt_base <= exp1^.desc.set_lwb then (* haven't already emitted null check? *)
		compare_integer_ops (first_set.arg[1], first_set.arg[2], gtc, included);
	      if aconstp (second_set.arg[3], cst_word_length) then
		temp_desc := int_desc (ctxt_base + cst_word_length * bits_per_word - 1, no_size, false)
	      else begin
		second_set.arg[3] := coerce (second_set.arg[3], [dreg_mode], dreg_mode,
					     at_least [min (succ (second_set.arg[3].value_size), size_long)],
					     true);
		temp_desc := copy_dreg (duplicate_desc (second_set.arg[3]));
		gen_im (asl_opc, 4, temp_desc);
		if ctxt_base > 0 then
		  gen_im (add_opc, ctxt_base - 1, temp_desc)
		else
		  gen_im (sub_opc, 1, temp_desc)
	      end;
	      compare_integer_ops (first_set.arg[2], temp_desc, gtc, notincluded);
	      free_desc (temp_desc)
	    end;
	    if aconstp (first_set.arg[1], low) andif (low < ctxt_base) then
	      first_set.arg[1] := int_desc (ctxt_base, no_size, false);
	    if aconstp (first_set.arg[2], high) andif (high > ctxt_upb) then
	      first_set.arg[2] := int_desc (ctxt_upb, no_size, false);
	    temp_set := set_temporary (ctxt_base, second_set.arg[3]);
	    genset (first_set, dupl_set (temp_set));
	    first_set := temp_set;
	    test_inclusion (first_set, second_set, notincluded_label);
	    if bool_ctxt or not negate then
	      gen_bcc (eq_cc, included_label)
	    else
	      gen_bcc (ne_cc, notincluded_label);
	    epilogue
	  end
	end (* case second_set.nargs *);
      end;

    (* First_set is a set. *)

    3: begin
      prologue;
      if second_set.nargs = 0 then begin (* <set> <= [] *)
	test_null (first_set, notincluded_label);
	if bool_ctxt or not negate then
	  gen_bcc (eq_cc, included_label)
	else
	  gen_bcc (ne_cc, notincluded_label)
      end
      else begin (* <set> <= [x] or [x..y] or <set> *)
	if (second_set.nargs < 3) orif
	   not ops_equal (first_set.arg[2], second_set.arg[2]) orif
	   not ops_equal (first_set.arg[3], second_set.arg[3]) then begin
	  temp_set := set_temporary (first_set.arg[2].cst_part.offset * bits_per_word, first_set.arg[3]);
	  if second_set.nargs < 3 then
	    genset (second_set, dupl_set (temp_set))
	  else
	    set_move (second_set, dupl_set (temp_set));
	  second_set := temp_set
	end;
	test_inclusion (first_set, second_set, notincluded_label);
	if bool_ctxt or not negate then
	  gen_bcc (eq_cc, included_label)
	else
	  gen_bcc (ne_cc, notincluded_label)
      end;
      epilogue
    end

  end (* case first_set.nargs *);

  set_free (first_set);
  set_free (second_set)
end (* set_inclusion *);
$PAGE fetchboolean
(*  FETCHBOOLEAN evaluates a boolean expression and makes its value
    available.  It is used when a boolean value is needed as a parameter
    or for an assignment.  The NEGATE parameter, if true, indicates that
    the complement of the expression is actually to be computed.  *)

function fetchboolean (* forward declared *);

var jump_out, fall_out : def;
    texpr_uses_ok : boolean;

    procedure set_result ( cc : condition_codes );
    begin
      if cc = t_cc then
	fetchboolean := int_desc (1, no_size, false)
      else if cc = f_cc then
	fetchboolean := int_desc (0, no_size, false)
      else begin
	fetchboolean := reg_desc (get_dreg (), size_byte, true);
	gen_scc (cc, fetchboolean);
	fetchboolean.extended_size := size_word;
	gen_ir (and_opc, 1, fetchboolean.reg, size_word);
      end;
    end;
  
begin
  texpr_uses_ok := expr_uses_ok; (* copy I can change *)

  with test_expr^ do begin
    case opcode of

      (*  If the boolean expression is one that must be fetched anyways, then
	  fetch it and, if it must be negated, load it into a register and
	  negate it.  *)

      first_data_ref..last_data_ref,
      func_call_op,
      sclcvt_op,
      eof_op, eoln_op, eopage_op,
      masked_op, pending_op,
      gen_andif_op, gen_orif_op :
	begin
	  assert (not texpr_uses_ok);
	  fetchboolean := fetch (test_expr, all_modes, null_mode, any_size, false);
	  texpr_uses_ok := true; (* fetch took care of it *)
	  if negate then
	    fetchboolean := complement (fetchboolean);
	end;

      (*  A NOT operator is simply processed by fetching its operand with
	  the NEGATE flag set.  *)

      bnot_op :
	fetchboolean := fetchboolean (operand [1], not negate, false);

      or_op,
      and_op :
	fetchboolean := and_or_test (operand[1], operand[2], opcode, negate);

      ile_op..ine_op,
      ptreq_op..ptrne_op,
      fileq_op..filne_op :
	compare_integers (operand[1], operand[2],
			  cond_inverse [negate, op_rel [opcode]], set_result);

      rle_op..rne_op :
	compare_reals (operand[1], operand[2],
		       cond_inverse [negate, op_rel [opcode]], set_result);

      sle_op..sne_op :
	compare_strings (operand[1], operand[2],
			 cond_inverse [negate, op_rel [opcode]], set_result);

      seteq_op, setne_op :
	set_equality (true (* boolean context *), fetchboolean, operand[1], operand[2],
		      cond_inverse [negate, op_rel [opcode]], jump_out, fall_out, set_result);

      setle_op :
	set_inclusion (true (* boolean context *), fetchboolean, operand[1], operand[2],
		      negate, jump_out, fall_out, set_result);

      setge_op :
	set_inclusion (true (* boolean context *), fetchboolean, operand[2], operand[1],
		      negate, jump_out, fall_out, set_result);

      in_op :
	set_in_test (true (* boolean context *), fetchboolean, operand[1], operand[2],
		     jump_out, negate, set_result);

      odd_op :
	begin
	  fetchboolean := copy_dreg (fetch (operand [1], [dreg_mode], dreg_mode, any_size, false));
	  fetchboolean.extended_size := max (fetchboolean.extended_size, size_word);
	  fetchboolean.value_size := size_byte;
	  gen_ir (and_opc, 1, fetchboolean.reg, size_word);
	end;

    end (* case opcode *);
  end (* with test_expr^ *);
    
  if not texpr_uses_ok then
    dec_expr_usage (test_expr);

  fetchboolean.signed_value := true;
  fetchboolean.known_positive := true;
end (* fetchboolean *);
$PAGE test_and_jump
(*  TEST AND JUMP generates code to test an arbitrary boolean expression
    and branch depending on the result of the test.  TRUE LOC is the label
    node to be branched to if the test is true, and FALSE LOC is the destination
    label if the test is false.  NEXT LOC is the label node immediately following
    the branch, if that is known, or nil; it is used to determine whether the
    sign of the test may profitably be inverted.  *)

public procedure test_and_jump ( test_expr : expr;
				 true_loc, false_loc, next_loc : tuple );

var jump_if_false : boolean;
    tloc, floc, nloc, jump_out : def;
    mem, result_op : op_desc;

    procedure branch ( cc : condition_codes );
    begin
      if cc <> f_cc then begin
	if jump_if_false
	  then gen_bcc (cc, floc)
	  else gen_bcc (cc, tloc);
	end;
    end;

begin

  (*  If we can fall through to the false destination, then we want to jump
      (to the true destination) when the expr is true.  Otherwise, we will
      jump to the false destination when the expr is false.  *)

  jump_if_false := (false_loc <> next_loc);

  (*  Get the symbolic definitions for the true and false destinations.  *)

  tloc := def_lookup (label_def, true_loc^.block_order_no);
  floc := def_lookup (label_def, false_loc^.block_order_no);
  nloc := def_lookup (label_def, next_loc^.block_order_no);

  with test_expr^ do begin
    case opcode of

      (*  If the boolean expression to be tested is one that must be fetched
	  anyways, then fetch it and test whether it is 0 (false) or 1 (true).  *)

      first_data_ref..last_data_ref,
      func_call_op,
      sclcvt_op,
      eof_op, eoln_op, eopage_op,
      masked_op, pending_op :
	begin
	  mem := fetch (test_expr, all_modes, null_mode, any_size, false);
	  test (mem, cond_inverse [jump_if_false, nec], branch);
	  free_desc (mem)
	end;

      ile_op..ine_op,
      ptreq_op..ptrne_op,
      fileq_op..filne_op : begin
	compare_integers (operand [1], operand [2],
			  cond_inverse [jump_if_false, op_rel [opcode]], branch);
	dec_expr_usage (test_expr) (* since we didn't fetch it *)
      end;

      rle_op..rne_op : begin
	compare_reals (operand [1], operand [2],
		       cond_inverse [jump_if_false, op_rel [opcode]], branch);
	dec_expr_usage (test_expr) (* since we didn't fetch it *)
      end;

      sle_op..sne_op : begin
	compare_strings (operand [1], operand [2],
			 cond_inverse [jump_if_false, op_rel [opcode]], branch);
	dec_expr_usage (test_expr) (* since we didn't fetch it *)
      end;

      seteq_op, setne_op, setle_op, setge_op :
	begin
	  if jump_if_false then
	    jump_out := floc
	  else
	    jump_out := tloc;
	  if (opcode = seteq_op) or (opcode = setne_op) then
	    set_equality (false (* not boolean context *), result_op (* not used *), 
			  operand[1], operand[2],
			  cond_inverse [jump_if_false, op_rel [opcode]], jump_out, nloc, branch)
	  else if opcode = setle_op then
	    set_inclusion (false (* no boolean context *), result_op (* not used *),
			   operand[1], operand[2], jump_if_false, jump_out, nloc, branch)
	  else
	    set_inclusion (false (* no boolean context *), result_op (* not used *),
			   operand[2], operand[1], jump_if_false, jump_out, nloc, branch);
	  dec_expr_usage (test_expr) (* since we didn't fetch it *)
	end;

      in_op : begin
	set_in_test (false (* not boolean context *), result_op (* not used *),
		     operand[1], operand[2], floc, jump_if_false, branch);
	dec_expr_usage (test_expr) (* since we didn't fetch it *)
      end;

      odd_op :
	begin
	  mem := fetch (operand [1], data_modes, null_mode, any_size, false);
	  dec_expr_usage (test_expr); (* since we didn't fetch it *)
	  if mem.mode <> dreg_mode then begin
	    if mem.value_size = size_long then
	      mem := increment_addr (mem, 3)
	    else if mem.value_size = size_word then
	      mem := increment_addr (mem, 1);
	    mem.value_size := size_byte;
	  end;
	  gen_im (btst_opc, 0, mem);
	  free_desc (mem);
	  branch (sgn_tst_cond [cond_inverse [jump_if_false, nec]])
	end;

    end (* case opcode *);
  end (* with test_expr^ *);

  (*  If we generated a conditional branch to the false label, and we won't
      just fall through to the true label, then we must fall through to a
      a branch to the true label.  *)

  if jump_if_false and (true_loc <> next_loc) then
    gen_bcc (t_cc, tloc);
end (* test_and_branch *).
    u Oè