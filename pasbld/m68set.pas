$TITLE M68SET - MC68000 set expression evaluation

module m68set options check;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68gen.inc
$SYSTEM m68utl.inc
$SYSTEM m68cgu.inc
$SYSTEM m68exp.inc
$SYSTEM m68cmp.inc
$SYSTEM pasmth.inc
$SYSTEM ptmcon.inc
  
type
  set_bit_array = ^packed array [0..*] of boolean;
  
public const
  no_preference: op_desc := descriptors [null_mode];
$PAGE arithmetic utilities
(* The following routines perform simple manipulations that occur with great
   frequency within the set module.  These functions are defined so that the
   calculations utilizing them will be more readable by virtue of the mnemonic
   names.  *)
  
  
function len_words (lwb_bit, len_bits: set_range): set_range;
  begin
    if len_bits = 0 then
      len_words := 0
    else
      len_words := ngm (len_bits + lwb_bit mod bits_per_word, bits_per_word) div bits_per_word
  end;
  
function word_num (bit_num: set_range): set_range;
  begin
    word_num := bit_num div bits_per_word
  end;
  
function upb (lwb_arg, len_arg: set_range): integer;
  begin
    upb := lwb_arg + len_arg - 1
  end;
  
function len (upb_arg, lwb_arg: set_range): set_range;
  begin
    len := max (upb_arg - lwb_arg + 1, 0)
  end;
  
  
(* SETW SIZE returns the size in words of a set given an expr tuple.  Basing modulo 16
   is taken into account.  *)
  
function setw_size (exp: expr): set_range;
  
  begin
    with exp^.desc do
      setw_size := len_words (set_lwb, set_length)
  end;
$PAGE ops_equal, dupl_set
(* OPS EQUAL compares two operand descriptors, and returns a boolean
   indicating whether the two are the same.  *)

public function ops_equal (op1, op2: op_desc): boolean;

  function reloc_equal (rel1, rel2: reloc_value): boolean;
    begin
      reloc_equal := (rel1.offset = rel2.offset) and (rel1.kind = rel2.kind);
      if reloc_equal then
	case rel1.kind of
	  absolute_sc: ; (* must be ok *)
	  code_sc: reloc_equal := rel1.relsect = rel2.relsect;
	  external_sc,
	  static_sc,
	  local_sc,
	  parameter_sc: reloc_equal := rel1.relsym = rel2.relsym;
	  def_sc: reloc_equal := rel1.reldef = rel2.reldef;
	  runtime_sc: reloc_equal := rel1.relrt = rel2.relrt
	end
    end (* reloc_equal *);

begin
  ops_equal := (op1.mode = op2.mode) and
	       (op1.value_size = op2.value_size) and
	       (op1.extended_size = op2.extended_size) and
	       (op1.signed_value = op2.signed_value) and
	       (op1.reg = op2.reg) and (op1.index_reg = op2.index_reg) and
	       (reloc_equal (op1.cst_part, op2.cst_part))
end (* ops_equal *);


(* DUPL SET is the set_desc equivalent to duplicate_desc. *)

public function dupl_set (sdesc: set_desc): set_desc;
  var
    j: 1..3;
begin
  dupl_set := sdesc;
  with dupl_set do
    for j := 1 to nargs do
      arg[j] := duplicate_desc (arg[j])
end;
$PAGE gset_op_bounds
(* Genset OP BOUNDS determines the range of possible values for an integral valued
   expression tuple occurring as an operand of a GENSET tuple.  *)
  
public procedure gset_op_bounds (    int_valued_exp: expr;
				   var min_val, max_val: integer;
				   var fixed: boolean);

procedure worst_case (var tmin, tmax: integer);
  begin
    if int_valued_exp^.desc.signed then
      tmin := minimum (integer)
    else
      tmin := 0;
    if int_valued_exp^.desc.int_prec >= int_bits (maximum (integer)) then
      tmax := maximum (integer)
    else
      tmax := 2**int_valued_exp^.desc.int_prec - 1
  end;
  
var
  temp_min, temp_max: integer;

begin
  fixed := false; (* assume expression isn't constant *)
  with int_valued_exp^ do
    case opcode of

      cst_ref: begin
        min_val := cst_val.ival;
        max_val := cst_val.ival;
        fixed := true
      end;
      ident_ref:
        with id_sym^.type_desc^ do begin
          min_val := minval;
          max_val := maxval
        end;
      func_call_op:
        with subr^.desc.base^.return_type^ do begin
          min_val := minval;
          max_val := maxval
        end;
      field_ref:
        with field_sym^.type_desc^ do begin
          min_val := minval;
          max_val := maxval
        end;
      ptr_ref:
        with base_ptr^.desc.base^.target_type^ do begin
          min_val := minval;
          max_val := maxval
        end;
      array_ref: 
        with base_array^.desc.base^.element_type^ do begin
          min_val := minval;
          max_val := maxval
        end;
      buffer_ref:
        with base_file^.desc.base^.component_type^ do begin
          min_val := minval;
          max_val := maxval
        end;
      sclcvt_op: begin
	gset_op_bounds (operand[1], min_val, max_val, fixed);
	worst_case (temp_min, temp_max);
	min_val := max (min_val, temp_min);
	max_val := min (max_val, temp_max)
      end;
      others:
	worst_case (min_val, max_val)
    end (* case *);
   
end (* gset_op_bounds *);
$PAGE shape_set
(* SHAPE SET is passed a set expression tuple.  It propagates lwb and length
   information upwards from the leaves of the expression tree, storing the
   information in the type descriptor fields of the expression tuples. 
  
   For (sub)expressions like [I..J] the length information is worst case, based on the
   declared ranges of I and J.  The set_cst_len fields of the nodes' type
   descriptors are used to propagate whether a set expression has the indicated length,
   or if the length is variable and the indicated length is only a maximum.  *)
  
public procedure shape_set (    exp: expr;
                            var shaped_lwb, shaped_len: set_range);
  
var
  temp_lwb, temp_len, temp_upper_bound: set_range;
  upper_bound: -1..maximum (set_range);
  i: oper_range;
  fixed: boolean;
  
procedure shape_gen_set_operand (    int_valued_exp: expr;
                                 var op_lwb, op_len: set_range;
                                 var fixed: boolean);
  var
    min_val, max_val: integer;
  
  begin
    gset_op_bounds (int_valued_exp, min_val, max_val, fixed);
    assert (min_val <= set_upb_limit);
    if (max_val < 0) orif (max_val < min_val) then (* null set *) begin
      op_lwb := 0;
      op_len := 0;
      fixed := true
    end
    else begin
      op_lwb := max (0, min_val);
      op_len := len (min (set_upb_limit, max_val), op_lwb)
    end
  end;
  
  
begin (* shape_set *)
  with exp^ do begin
    case opcode of
  
      cst_ref,
      ident_ref,
      field_ref,
      buffer_ref,
      ptr_ref,
      array_ref,
      func_call_op: begin (* note: only sets handled here *)
        shaped_lwb := desc.set_lwb;  (* node already properly marked *)
        shaped_len := desc.set_length;
        desc.set_cst_len := true
      end;
  
      setcvt_op: begin
        shape_set (operand[1], shaped_lwb, shaped_len);
        desc.set_cst_len := operand[1]^.desc.set_cst_len
      end;
  
      gen_set_op: begin
        if upperbound (operand) = 0 then begin
          shaped_lwb := 0;
          shaped_len := 0;
          desc.set_cst_len := true
        end
        else if upperbound (operand) = 1 then
          shape_gen_set_operand (operand[1], shaped_lwb, shaped_len, fixed)
        else (* upperbound (operand) = 2 *) begin
          shape_gen_set_operand (operand[1], shaped_lwb, shaped_len, fixed);
          shape_gen_set_operand (operand[2], temp_lwb, temp_len, fixed);
          if upb (temp_lwb, temp_len) >= shaped_lwb then begin
            shaped_len := len (upb (temp_lwb, temp_len), shaped_lwb);
            desc.set_cst_len := fixed (* depends on upperbound of range *)
          end
          else (* null set *) begin
            shaped_lwb := 0;
            shaped_len := 0;
            desc.set_cst_len := true
          end
        end
      end;
  
      diff_op: begin
        shape_set (operand[1], shaped_lwb, shaped_len);
        shape_set (operand[2], temp_lwb, temp_len);
        desc.set_cst_len := operand[1]^.desc.set_cst_len
      end;
  
      both_op: begin
        shape_set (operand[1], temp_lwb, temp_len);
        shape_set (operand[2], shaped_lwb, shaped_len);
        upper_bound := min (upb (shaped_lwb, shaped_len), upb (temp_lwb, temp_len));
	desc.set_cst_len := (operand[1]^.desc.set_cst_len andif 
					      (upper_bound = upb (temp_lwb, temp_len)))
				orif
			    (operand[2]^.desc.set_cst_len andif
					      (upper_bound = upb (shaped_lwb, shaped_len)));
        shaped_lwb := max (shaped_lwb, temp_lwb);
        if upper_bound >= shaped_lwb then
          shaped_len := len (upper_bound, shaped_lwb)
        else (* null set *) begin
          shaped_len := 0;
          shaped_lwb := 0;
          desc.set_cst_len := true
        end
      end;
  
      union_op: begin
        shaped_lwb := set_upb_limit;
        upper_bound := 0;
        for i := 1 to upperbound (operand) do begin
          shape_set (operand[i], temp_lwb, temp_len);
          if temp_len > 0 (* non null *) then begin
            shaped_lwb := min (shaped_lwb, temp_lwb);
            temp_upper_bound := upb (temp_lwb, temp_len);
            if temp_upper_bound > upper_bound then begin
              upper_bound := temp_upper_bound;
              desc.set_cst_len := operand[i]^.desc.set_cst_len
            end
            else if (temp_upper_bound = upper_bound) andif operand[i]^.desc.set_cst_len then
              desc.set_cst_len := true
          end
        end;
        if upper_bound >= shaped_lwb then
          shaped_len := len (upper_bound, shaped_lwb)
        else begin
          shaped_len := 0;
          shaped_lwb := 0;
          desc.set_cst_len := true
        end
      end
    end (* case *);
  
    desc.set_lwb := shaped_lwb;
    desc.set_length := shaped_len
  end (* with *);
end (* shape_set *);
$PAGE set_free, set_temporary
(* SET FREE frees each of the op_desc's used in a set descriptor.  *)
  
public procedure set_free (sdesc: set_desc);
  
var
  j: 1..3;
  
begin
  with sdesc do
    for j := 1 to nargs do
      free_desc (arg[j])
end;
 
  
  
(* SET TEMPORARY returns a set_descriptor for space allocated on the stack
   to be used for a temporary.  *)
  
public function set_temporary (ctxt_lwbbit: set_range; len_addr: op_desc): set_desc;
  
begin
  with set_temporary do begin
    nargs := 3;
    arg[2] := int_desc (word_num (ctxt_lwbbit), no_size, true); (* lowerbound word *)
    arg[3] := duplicate_desc (len_addr);
    arg[1] := get_temp (duplicate_desc (len_addr), true (* word cnt *));
    arg[1].value_size := no_size;
    in_temp := true
  end
end (* set_temporary *);
$PAGE bst_clear_seq
(* BST CLEAR SEQ selects the best code sequence for clearing a vector of words. *)

type
  clr_sequences = (no_clr_code, rt_clr_call, simple_clears,
		   postincr_simple_clears, word_clr_loop, longword_clr_loop);

function bst_clear_seq (    n: integer;
			    target_indirect: boolean;
			var bests_size, bests_cycles: integer;
			var result_loc_after_and_indirect: boolean): integer;

var
  if_n_odd: 0..1;

begin
  if n = 0 then begin
    bst_clear_seq := ord (no_clr_code);
    bests_size := 0;
    bests_size := 0
  end
  else begin
    if_n_odd := ord (odd (n)); (* coef for terms applied just if n odd *)
    first_sequence (ord (rt_clr_call),
		    14                     - 2 * ord (target_indirect),
		    308 + round (8.25 * n) - 4 * ord (target_indirect)); (* note - wrong if variable len *)
    if n > 0 (* constant length *) then begin
      next_sequence (ord (simple_clears),
		     4 * ((n + 1) div 2)    - 2 * ord (target_indirect),
		     13 * n + 4 * if_n_odd  - 4 * ord (target_indirect));
      next_sequence (ord (postincr_simple_clears),
		     4 + 2 * ((n + 1) div 2)    - 4 * ord (target_indirect),
		     8 + 11 * n + 2 * if_n_odd  - 8 * ord (target_indirect));
      next_sequence (ord (word_clr_loop),
		     12          - 4 * ord (target_indirect),
		     16 + 23 * n - 8 * ord (target_indirect));
      next_sequence (ord (longword_clr_loop),
		     12 + 2 * if_n_odd           - 4 * ord (target_indirect),
		     16 + 16 * n - 3 * if_n_odd  - 8 * ord (target_indirect))
    end;
    bst_clear_seq := best_sequence (bests_size, bests_cycles)
  end;

  (* Signal whether result_loc from blk_clear would be pointing after last word
     cleared AND would be indirect.  I.e. whether a subsequent block move count
     on starting off with a postincrementable (for free) address mode.  *)

  result_loc_after_and_indirect := (bst_clear_seq <> ord (rt_clr_call)) and
				   (not (bst_clear_seq in [ord (simple_clears), ord (no_clr_code)])
					 or target_indirect)
end (* bst_clear_seq *);
$PAGE blk_clear
(* BLK CLEAR clears the specified number of words starting at the given address.   A usage
   is transfered from dest_loc to result_loc, and dest_len will be freed.  If the user
   has a further need for dest_loc, duplicate_desc should be used.  If the user has no
   need of result_loc, it should be freed.  Result_loc may refer to the same place that
   dest_loc did, or it may point to the word after the last word cleared.  After_last_word
   specifies which. *)

procedure blk_clear (    dest_loc, dest_len: op_desc;
		     var result_loc: op_desc;
		     var after_last_word: boolean);

const
  default_size: array [clr_sequences] of op_sizes := (no_size, no_size, size_long,
						      size_long, size_word, size_long);

var
  best_seq: clr_sequences;
  n, bests_size, bests_cycles: integer;
  temp_len: op_desc;
  loop_count, remainder: set_range;
  loop_label: def;
  result_loc_after_and_indirect: boolean;

begin
  assert (dest_loc.mode in memory_alterable_modes);

  (* Select the best code sequence. *)

  if not aconstp (dest_len, n) then 
    n := -1 (* signifies variable length *)
  else
    assert (n >= 0);
  best_seq := clr_sequences (bst_clear_seq (n, pincr_free (dest_loc), bests_size, bests_cycles,
					    result_loc_after_and_indirect));

  (* Generate the selected sequence. *)

  result_loc := dest_loc; (* copy we can modify *)
  if (best_seq <> rt_clr_call) and (pincr_free (result_loc) or (best_seq <> simple_clears)) then
    result_loc := make_postincr (result_loc);
  result_loc.value_size := default_size [best_seq]; (* make an educated guess at the size *)

  case best_seq of

    no_clr_code: ;

    rt_clr_call: begin
      pusha (duplicate_desc (result_loc));
      push (dest_len, size_long);
      gen_rt (rt_clear_block)
    end;

    simple_clears: begin
      assert (n <= 4); (* if the numbers work out as I expect ... *)
      if n > 2 then begin
	gen_m (clr_opc, result_loc);
	if result_loc.mode <> postincrement_mode then
	  result_loc := increment_addr (result_loc, 4)
      end;
      if odd (n) then
	result_loc.value_size := size_word;
      gen_m (clr_opc, result_loc);
      if result_loc.mode <> postincrement_mode then
	result_loc := increment_addr (result_loc, 4 - 2 * (n mod 2))
    end;

    postincr_simple_clears: begin
      while n >= 2 do begin
	gen_m (clr_opc, result_loc);
        n := n - 2
      end;
      if n > 0 then begin
	result_loc.value_size := size_word;
	gen_m (clr_opc, result_loc)
      end
    end;

    word_clr_loop, longword_clr_loop: begin
      if best_seq = word_clr_loop then begin
	loop_count := n;
	remainder := 0
      end
      else begin
	loop_count := n div 2;
	remainder := n mod 2
      end;
      temp_len := loadi (get_dreg, loop_count - 1, size_word, true);
      loop_label := def_create (local_def);
      gen_def (code_area, loop_label); (* insert loop label *)
      gen_m (clr_opc, result_loc);
      gen_dbcc (f_cc, temp_len.reg, loop_label);
      free_desc (temp_len);
      if remainder > 0 then begin
        result_loc.value_size := size_word;
	gen_m (clr_opc, result_loc)
      end
    end

  end;

  if result_loc.mode = postincrement_mode then
    result_loc.mode := indirect_mode; (* keep life simple *)
  result_loc.value_size := no_size; (* put it back *)

  after_last_word := best_seq <> rt_clr_call;

  (* Verify accuracy of bst_clear_seq's prediction. *)
  assert (result_loc_after_and_indirect <=
			(after_last_word and (result_loc.mode = indirect_mode)))
end (* blk_clear *);
$PAGE set_move
(* SET MOVE moves a set to a new location, taking into account different starting
   words and word lengths of the source and target sets.  Both given descriptors will be
   freed - dupl_set must be used if either is to be valid upon return.  *)

public procedure set_move (from_desc, to_desc: set_desc);

type
  set_move_sequences = (discrete_clrs_and_move, discrete_with_coerce,
			clear_entire_then_move, mv_and_clr_rt_call);

var
  best_seq:	set_move_sequences;
  after_last_word, target_indirect, result_after_and_indirect:	boolean;
  target_loc, new_target_loc:	op_desc;
  source_length, target_length:	set_range;
  lead_cleared_length, move_length, tail_cleared_length,
  size_wo_coerce, cycles_wo_coerce,
  size_with_coerce, cycles_with_coerce,
  size_clr_entire, cycles_clr_entire,
  best_seq_ordinal, size_in_bytes, cycles, coef:	integer;

procedure discrete_seq (target_pincr_for_free: boolean; var discr_size, discr_cycles: integer);

  begin
    discr_size   := 0;
    discr_cycles := 0;
    target_indirect := target_pincr_for_free; 
    if lead_cleared_length > 0 then begin
      best_seq_ordinal := bst_clear_seq (lead_cleared_length, target_indirect, discr_size, discr_cycles,
					 result_after_and_indirect);
      target_indirect := result_after_and_indirect
    end;
    best_seq_ordinal := bst_mv_seq (move_length, pincr_free (from_desc.arg[1]), target_indirect,
				    size_in_bytes, cycles, result_after_and_indirect);
    discr_size   := discr_size   + size_in_bytes;
    discr_cycles := discr_cycles + cycles;
    target_indirect := result_after_and_indirect;
    if tail_cleared_length > 0 then begin
      best_seq_ordinal := bst_clear_seq (tail_cleared_length, target_indirect,
					 size_in_bytes, cycles, result_after_and_ind);
      discr_size   := discr_size   + size_in_bytes;
      discr_cycles := discr_cycles + cycles
    end
  end;

begin
  assert (from_desc.nargs = 3); (* "l" format *)
  assert (from_desc.arg[2].cst_part.offset >= to_desc.arg[2].cst_part.offset);
  lead_cleared_length := from_desc.arg[2].cst_part.offset
			   - to_desc.arg[2].cst_part.offset;

  (* Select the best sequence. *)

  if not aconstp (to_desc.arg[3], target_length) then begin
    assert (from_desc.arg[1].mode <> immediate_mode); (* will have to pass its address *)
    best_seq := mv_and_clr_rt_call (* variable length --> go with runtime *)
  end
  else begin
    if not aconstp (from_desc.arg[3], source_length) then
      assert (false); (* if target len const, source must be too *)
    assert (lead_cleared_length < target_length);
    assert ((source_length > 0) and (source_length <= target_length - lead_cleared_length));
    move_length := min (source_length, target_length - lead_cleared_length);
    tail_cleared_length := target_length - move_length - lead_cleared_length;

    discrete_seq (pincr_free (to_desc.arg[1]), size_wo_coerce, cycles_wo_coerce);
    discrete_seq (true, size_with_coerce, cycles_with_coerce);
    coef := 0;
    if not pincr_free (to_desc.arg[1]) then
      if to_desc.arg[1].mode <> indirect_mode then
        coef := 4 (* would required lea *)
      else
	coef := 2; (* just needs register copy *)

    best_seq_ordinal := bst_clear_seq (target_length, false, size_clr_entire, cycles_clr_entire,
				       result_after_and_indirect);
    best_seq_ordinal := bst_mv_seq (move_length, pincr_free (from_desc.arg[1]), false,
				    size_in_bytes, cycles, result_after_and_indirect);
    size_clr_entire := size_clr_entire + size_in_bytes + 6;
    cycles_clr_entire := cycles_clr_entire + cycles + 17;
    first_sequence (ord (discrete_clrs_and_move), size_wo_coerce, cycles_wo_coerce);

    next_sequence  (ord (discrete_with_coerce),
		    size_with_coerce + coef, cycles_with_coerce + coef * 2);
    next_sequence  (ord (clear_entire_then_move), size_clr_entire, cycles_clr_entire);
    if from_desc.arg[1].mode <> immediate_mode then
      next_sequence (ord (mv_and_clr_rt_call),
		     22 + 4 * ord (lead_cleared_length > 0) - 2 * ord (pincr_free (from_desc.arg[1]))
							    - 2 * ord (pincr_free (to_desc.arg[1])),
		     336 + round (12.25 * move_length)
			 + round (8.25 * (lead_cleared_length + tail_cleared_length)));
    best_seq := set_move_sequences (best_sequence (size_in_bytes, cycles))
  end;

  (* Generate the selected sequence. *)

  case best_seq of

    discrete_clrs_and_move, discrete_with_coerce: begin
      target_loc := to_desc.arg[1];
      if best_seq = discrete_with_coerce then begin
	target_loc := make_postincr (target_loc);
	target_loc.mode := indirect_mode (* we don't pass around postincr's *)
      end;

      if lead_cleared_length > 0 then begin
	blk_clear (target_loc, int_desc (lead_cleared_length, no_size, true),
		   new_target_loc, after_last_word);
	if after_last_word then
	  target_loc := new_target_loc
	else
	  target_loc := increment_addr (new_target_loc, lead_cleared_length * 2)
      end;
      blk_move (int_desc (move_length, no_size, true), true (* len in words *),
		from_desc.arg[1] (* freed *),
		target_loc,
		new_target_loc, after_last_word);
      if tail_cleared_length > 0 then begin
	if after_last_word then
	  target_loc := new_target_loc
	else
	  target_loc := increment_addr (new_target_loc, move_length * 2);
	blk_clear (target_loc, int_desc (tail_cleared_length, no_size, true),
		   new_target_loc, after_last_word)
      end;
      free_desc (new_target_loc)
    end;

    clear_entire_then_move: begin
      blk_clear (duplicate_desc (to_desc.arg[1]), int_desc (target_length, no_size, true),
		 new_target_loc, after_last_word);
      free_desc (new_target_loc);
      target_loc := increment_addr (to_desc.arg[1], lead_cleared_length * 2);
      blk_move (int_desc (move_length, no_size, true), true (* len in words *),
	        from_desc.arg[1] (* freed *),
		target_loc, new_target_loc, after_last_word);
      free_desc (new_target_loc)
    end;

    mv_and_clr_rt_call: begin
      pusha (from_desc.arg[1]);
      if (lead_cleared_length > 0) or not ops_equal (from_desc.arg[3], to_desc.arg[3]) then
	push (from_desc.arg[3], size_long); (* we'll pass both lengths *)
      pusha (to_desc.arg[1]);
      push (to_desc.arg[3], size_long);
      if lead_cleared_length > 0 then begin
	pushi (lead_cleared_length, size_long);
	gen_rt (rt_setmove_diff_lwb)
      end
      else if ops_equal (from_desc.arg[3], to_desc.arg[3]) then
	gen_rt (rt_move_block_wcnt)
      else
	gen_rt (rt_setmove_same_lwb)
    end

  end
end (* set_move *);
$PAGE gen_cst_set
(* GEN CST SET emits a set constant in the constant area. *)

function gen_cst_set (set_contents: set_bit_array;
		      const_base, word_count, first_word: set_range): set_desc;

var
  cons_def: def;
  j, const_word_len: set_range;
  temp_contents: set_bit_array;

begin
  cons_def := def_create (const_def); (* allocate label *)
  gen_def (cst_area, cons_def); (* insert it into code list *)
  const_word_len := len (word_count, first_word); (* in case we're discarding leading words *)

  (* Make copy of set's contents without leading words to be dropped. *)

  new (temp_contents, const_word_len * bits_per_word - 1);
  for j := 0 to const_word_len * bits_per_word - 1 do
    temp_contents^[j] := set_contents^[j - (first_word - 1) * bits_per_word];
  gen_set (cst_area, temp_contents^); (* emit the constant *)
  dispose (temp_contents);

  (* Construct set descriptor for the constant. *)

  with gen_cst_set do begin
    nargs := 3;
    arg[1] := descriptors [pc_displacement_mode];
    arg[1].cst_part := def_reloc (cons_def);
    arg[2] := int_desc (const_base + first_word - 1, no_size, true);
    arg[3] := int_desc (const_word_len, no_size, true);
    in_temp := false
  end
end (* gen_cst_set *);
$PAGE prep_bitreference
(* PREP BITREFERENCE generates any necessary range checking code, and returns
   op_desc's for the byte and bit within it for the desired element.  The input
   op_desc and set_desc are freed. *)

public procedure prep_bitreference (    element: op_desc; element_exp: expr;
				        target_set: set_desc;
				    var bit_pos, byte_loc: op_desc;
				    var defn: def);

var
  min_val, max_val: integer;
  fixed, test_high, test_low: boolean;
  ctxt_base, targ_len, elem_val: set_range;
  index_reg, temp_reg, elem_reg: op_desc;

begin
  assert (target_set.nargs = 3);

  (* First, lets see what we have to do. *)

  if aconstp (element, elem_val) then begin
    min_val := elem_val;
    max_val := elem_val
  end
  else
    gset_op_bounds (element_exp, min_val, max_val, fixed);
  ctxt_base := target_set.arg[2].cst_part.offset * bits_per_word;
  test_low := min_val < ctxt_base;
  test_high := not aconstp (target_set.arg[3], targ_len) orif
		(max_val > upb (ctxt_base, targ_len * bits_per_word));
  if (test_low or test_high) and (defn = nil) then
    defn := def_create (local_def);

  (* Constant element. *)

  if aconstp (element, elem_val) then begin
    assert (not test_high and not test_low);
    elem_val := elem_val - ctxt_base;
    bit_pos := int_desc (7 - (elem_val mod bits_per_byte), size_byte, true);
    byte_loc := increment_addr (target_set.arg[1], elem_val div bits_per_byte);
    free_desc (target_set.arg[3])
  end

  (* Nonconstant element. *)

  else begin
    elem_reg := copy_dreg (element);
    index_reg := copy_dreg (duplicate_desc (elem_reg));
    if index_reg.signed_value then
      gen_im (asr_opc, 3, index_reg) (* get byte offset *)
    else begin
      gen_im (lsr_opc, 3, index_reg);
      index_reg.signed_value := true
    end;
    if ctxt_base > 0 then
      gen_im (sub_opc, ctxt_base div bits_per_byte, index_reg); (* normalize to lwb of set *)
    if test_low then
      gen_bcc (lt_cc, defn);
    if test_high then begin
      if aconstp (target_set.arg[3], targ_len) then
	gen_im (cmp_opc, targ_len * 2, index_reg)
      else begin
	temp_reg := copy_dreg (target_set.arg[3]); (* runtime word length *)
	gen_mm (add_opc, temp_reg, temp_reg); (* double to get byte length *)
	gen_mm (cmp_opc, temp_reg, index_reg); (* set cc's to byte offset - byte length *)
	free_desc (temp_reg)
      end;
      gen_bcc (ge_cc, defn)
    end;
    bit_pos := loadi (get_dreg, 7, size_byte, true); (* load 7 for mask and for subtraction *)
    elem_reg.value_size := size_byte; (* only care about low 3 bits now *)
    gen_mm (and_opc, bit_pos, elem_reg); (* modulo 8 *)
    gen_mm (sub_opc, elem_reg, bit_pos); (* reverse direction of bit numbering *)
    free_desc (elem_reg);
    byte_loc := offset_addr (target_set.arg[1], index_reg)
  end;

  byte_loc.value_size := size_byte
end (* prep_bitreference *);
$PAGE emit_bit_inst
(* EMIT BIT INST takes descriptors for a singleton set and a target set, and
   sets or clears the singleton bit in the target set.  Both set descriptors
   are freed. *)

public procedure emit_bit_inst (bit_value: 0..1;
				source_set, target_set: set_desc);

var
  bit_pos, byte_loc: op_desc;
  skip_label: def;

begin
  assert ((source_set.nargs = 2) and (target_set.nargs = 3));

  (* Generate code for range checking, and to concoct the byte address
     and bit offset within it. *)

  skip_label := nil;
  free_desc (source_set.arg[2]);
  prep_bitreference (source_set.arg[1], source_set.lwb_exp,
		     target_set, bit_pos, byte_loc, skip_label);

  (* Set or clear the bit. *)

  if bit_value = 0 then
    gen_mm (bclr_opc, bit_pos, byte_loc)
  else
    gen_mm (bset_opc, bit_pos, byte_loc);
  if skip_label <> nil then
    gen_def (code_area, skip_label);

  free_desc (bit_pos);
  free_desc (byte_loc)
end (* emit_bit_inst *);
$PAGE genset
(* GENSET creates a set, within context, given an "o" format set descriptor.  Both given
   descriptors will be freed. *)

public procedure genset (source_desc, target_desc: set_desc);

var
  temp_desc: set_desc;
  low, high, word_span, target_len,
  const_base, const_wordlen: set_range;
  j: integer;
  signbit_pos: 0..16;
  constant_range, constant_len, after_last_word: boolean;
  result_loc: op_desc;
  set_val: record
    case boolean of
      true:  (int: integer);
      false: (bits: packed array [-4..31] of boolean)
    end;
  set_contents: set_bit_array;

begin
  assert (source_desc.nargs = 2);
  constant_range := aconstp (source_desc.arg[1], low) and aconstp (source_desc.arg[2], high);
  if constant_range then
    word_span := len_words (low, len (high, low));
  constant_len := aconstp (target_desc.arg[3], target_len);

  (* First, a caller from outside M68SET might give an empty range. *)

  if constant_range and (word_span = 0) then begin
    blk_clear (target_desc.arg[1], target_desc.arg[3], result_loc, after_last_word);
    free_desc (result_loc)
  end

  (* Maybe we can just make an immediate value. *)

  else if constant_range and (word_span in [1,2]) and constant_len then begin
    const_base := word_num (low);
    if word_span = 1 then
      if target_desc.arg[2].cst_part.offset = (const_base - 1) then begin
	word_span := 2;
	const_base := const_base - 1
      end
      else if constant_len andif
	      (upb (target_desc.arg[2].cst_part.offset, target_len) = const_base + 1) then
	word_span := 2;
    set_val.int := 0;
    signbit_pos := bits_per_word * (2 - word_span);
    for j := low - const_base * bits_per_word  to  high - const_base * bits_per_word do
      set_val.bits [j + signbit_pos] := true;
    if set_val.bits [signbit_pos] then (* mc68000 sign bit on? *)
      for j := -4 to signbit_pos - 1 do
	set_val.bits [j] := true; (* sign extend in dec 10 word *)
    temp_desc.nargs := 3;
    temp_desc.arg[1] := int_desc (set_val.int, no_size, true);
    temp_desc.arg[2] := int_desc (const_base, no_size, true);
    temp_desc.arg[3] := int_desc (word_span, no_size, true);
    set_move (temp_desc, target_desc)
  end

  (* Singleton set, with either the length or the element variable. *)

  else if ops_equal (source_desc.arg[1], source_desc.arg[2]) then begin
    pusha (target_desc.arg[1]);
    push (target_desc.arg[3], size_long);
    push (source_desc.arg[1], size_long);
    free_desc (source_desc.arg[2]);
    if target_desc.arg[2].cst_part.offset > 0 then
      pushi (target_desc.arg[2].cst_part.offset * 2 (* in bytes *), size_long);
    if target_desc.arg[2].cst_part.offset > 0 then
      gen_rt (rt_set_singleton_nonzero_lwb)
    else
      gen_rt (rt_set_singleton_zero_lwb)
  end

  (* Range set, with length or range variable, or range just too big for immediate. *)

  else begin
    if constant_range then (* length variable, or spans more than 2 words *) begin
      const_wordlen := len (word_num (high), word_num (low));
      new (set_contents, const_wordlen * bits_per_word - 1);
      for j := 0 to const_wordlen * bits_per_word - 1 do
	set_contents^[j] := j + word_num (low) * bits_per_word in [low..high];
      set_move (gen_cst_set (set_contents, word_num (low), const_wordlen, 1), target_desc);
      dispose (set_contents)
    end

    else begin
      pusha (target_desc.arg[1]);
      push (target_desc.arg[3], size_long);
      push (source_desc.arg[1], size_long);
      push (source_desc.arg[2], size_long);
      if target_desc.arg[2].cst_part.offset > 0 then
	pushi (target_desc.arg[2].cst_part.offset * 16 (* in bits *), size_long);
      if target_desc.arg[2].cst_part.offset > 0 then
	gen_rt (rt_set_range_nonzero_lwb)
      else
	gen_rt (rt_set_range_zero_lwb)
    end
  end
end (* genset *);
$PAGE clip_calc, clip_bits
(* CLIP BITS removes extraneous bits from the beginning of the first word of
   a set, and from the end of the last word.  It's intended for use on fixed
   length sets such as the lefthand side of a set assignment, or a set-valued 
   parameter.

   CLIP CALC determines how many such bits there are at each end of the set.  *)

procedure clip_calc (    source_lwb, source_length,
			 target_lwb, target_length: set_range;
		     var low_bits_to_mask, high_bits_to_mask: 0..15);

begin
  if source_lwb < target_lwb then
    low_bits_to_mask := target_lwb mod bits_per_word
  else
    low_bits_to_mask := 0;
  if upb (source_lwb, source_length) > upb (target_lwb, target_length) then
    high_bits_to_mask := 15 - upb (target_lwb, target_length) mod bits_per_word
  else
    high_bits_to_mask := 0;
end;


type
  mask_table = array [0..15] of 0..#Hffff;
const
  lmask: mask_table := (#Hffff, #H7fff, #H3fff, #H1fff, #H0fff, #H07ff, #H03ff, #H01ff,
			#H00ff, #H007f, #H003f, #H001f, #H000f, #H0007, #H0003, #H0001);
  hmask: mask_table := (#Hffff, #Hfffe, #Hfffc, #Hfff8, #Hfff0, #Hffe0, #Hffc0, #Hff80,
			#Hff00, #Hfe00, #Hfc00, #Hf800, #Hf000, #He000, #Hc000, #H8000);

procedure clip_bits (set_op: set_desc;
		     low_bits_to_mask, high_bits_to_mask: 0..15);

var
  mask: 0..#Hffff;
  target_loc: op_desc;
  set_len: set_range;

begin
  if not aconstp (set_op.arg[3], set_len) then
    assert (false);
  target_loc := set_op.arg[1]; 
  target_loc.value_size := size_word; (* for andi *)
  if set_len = 1 then begin
    mask := lmask [low_bits_to_mask] - (2 ** 16 - hmask [high_bits_to_mask] - 1);
    if mask <> #Hffff then
      gen_im (and_opc, mask, target_loc)
  end
  else begin
    if low_bits_to_mask > 0 then
      gen_im (and_opc, lmask [low_bits_to_mask], target_loc);
    if high_bits_to_mask > 0 then begin
      target_loc := increment_addr (target_loc, (set_len - 1) * 2);
      gen_im (and_opc, hmask [high_bits_to_mask], target_loc)
    end
  end;
  free_desc (target_loc)
end (* clip_bits *);
$PAGE set_fetch
(* SET FETCH is passed a set expression tuple and returns a set descriptor for the
   value of the tuple.  The set value may not actually be created if it can be
   represented by a "o" or "z" format set descriptor.

   The caller provides the tightest context information it can via the arguments
   ctxt_lwbbit and ctxt_lenbits.  A psuedo shaping pass is made over the expression tree
   to propagate lwb and length information up form the leaves of the tree, and to
   improve on the provided context if possible.

   SET FETCH calls its recursive child DO SET FETCH to traverse the expression tree
   and evaluate the set.  *)

public function set_fetch (set_exp: expr;
			   ctxt_lwbbit, ctxt_lenbits: set_range;
			   desired_loc: op_desc; force_fixed: boolean): set_desc;

function rtime_wordlength: op_desc; forward;
$PAGE do_set_fetch - in set_fetch
(* DO SET FETCH is the set analog of DO_FETCH, performing the minimum of work
   necessary to produce a set_desc describing the evaluated set.  Creation of an
   actual set is avoided if a descriptor of its value will suffice.  Context
   information originating from the end-user of the set (possibly improved by 
   "shaping") is passed along as the set is recursively evaluated.  *)

function do_set_fetch (exp: expr;
		       ctxt_lwbbit, ctxt_lenbits: set_range;
		       desired_loc: op_desc): set_desc;
  
  (* First, some conveniences: *)
  
  procedure make_temp_set (var set_operand: set_desc);
    var
      temp_set: set_desc;
    begin
      assert (set_operand.nargs > 0);
      temp_set := dupl_set (set_temporary (ctxt_lwbbit, rtime_wordlength));
      if set_operand.nargs <> 3 then
	genset (set_operand, temp_set)
      else
	set_move (set_operand, temp_set);
      set_operand := temp_set
    end;

  function desired: set_desc;
    begin
      with desired do begin
	nargs := 3;
	arg[1] := desired_loc;
	arg[2] := int_desc (word_num (ctxt_lwbbit), no_size, true);
	arg[3] := duplicate_desc (rtime_wordlength);
	in_temp := false
      end
    end;
  
  function singleton (set_op: set_desc): boolean;
    begin
      singleton := (set_op.nargs = 2) andif ops_equal (set_op.arg[1], set_op.arg[2])
    end;
  
  procedure prep_operand (var set_op: set_desc; leave_singleton: boolean);
    var
      temp: set_range;
    begin
      assert (set_op.nargs > 0);  (* if null gets here someone screwed up *)
      if leave_singleton andif singleton (set_op) then
        return;
      if (set_op.nargs <> 3) orif (not aconstp (rtime_wordlength, temp) and not set_op.in_temp) then
        make_temp_set (set_op)
    end;
  
  procedure reverse_ops (var first_op, second_op: set_desc);
    var
      temp: set_desc;
    begin
      temp := first_op;
      first_op := second_op;
      second_op := temp
    end;
  
  function in_context (arg_set: set_desc): boolean;
    begin
      in_context := (arg_set.arg[2].cst_part.offset = word_num (ctxt_lwbbit)) 
		       and ops_equal (arg_set.arg[3], rtime_wordlength)
    end;
$PAGE do_set_difference - in do_set_fetch (in set_fetch)
(* DO SET DIFFERENCE compiles a set difference expression tuple, and returns a
   set descriptor for its value.  *)
  
function do_set_difference: set_desc;
  
type
  set_diff_sequences = (simple_diff_sequence, set_diff_rt_call,
			diff_word_loop, diff_longword_loop);

var
  first_op, second_op: set_desc;
  target, temp_len: op_desc;
  n, bests_size, bests_cycles, num_indirect: integer;
  best_seq: set_diff_sequences;
  loop_label: def;
  two_op_form: boolean;
  if_2_op, n_odd: 0..1;
  

procedure generate_actual_diff_seq (size: op_sizes);
  var
    dreg: data_regs;
  begin
    first_op.arg[1].value_size := size;
    second_op.arg[1].value_size := size;
    if not two_op_form then
      target.value_size := size;
    dreg := get_dreg;
    gen_mr (move_opc, second_op.arg[1], dreg);	(*	MOVE	OP2,Dn	  *)
    gen_r (not_opc, dreg, size);		(*	NOT	Dn	  *)
    if two_op_form then
      gen_rm (and_opc, dreg, first_op.arg[1])	(*	AND	Dn,OP1	  *)
    else begin
      gen_mr (and_opc, first_op.arg[1], dreg);	(*	AND	OP1,Dn	  *)
      gen_rm (move_opc, dreg, target)		(*	MOVE	Dn,TARGET *)
    end;
    free_reg (dreg)
  end;

begin
  first_op  := do_set_fetch (exp^.operand[1], ctxt_lwbbit, ctxt_lenbits, no_preference);
  second_op := do_set_fetch (exp^.operand[2], ctxt_lwbbit, ctxt_lenbits, no_preference);

  (* First, see if we can avoid performing the operation. *)

  if (exp^.desc.set_length = 0) orif (first_op.nargs = 0) then begin
    do_set_difference.nargs := 0; (* result is unavoidably null *)
    exp^.desc.set_lwb := 0;
    exp^.desc.set_length := 0;
    set_free (first_op);
    set_free (second_op);
    return  (* <-- return *)
  end;

  if (second_op.nargs = 0) orif
       (exp^.operand[1]^.desc.set_lwb > 
	     upb (exp^.operand[2]^.desc.set_lwb, exp^.operand[2]^.desc.set_length)) orif
       (exp^.operand[2]^.desc.set_lwb > 
	     upb (exp^.operand[1]^.desc.set_lwb, exp^.operand[1]^.desc.set_length)) then begin
    do_set_difference := first_op; (* second op is irrelevant to result *)
    set_free (second_op)
  end

  (* have to really do it *)

  else begin

    (* Prepare the operands. *)

    prep_operand (first_op, false (* convert singletons *));
    prep_operand (second_op, true (* we'll special case singleton *));

    (* Second operand is singleton. *)

    if second_op.nargs < 3 then begin
      if first_op.in_temp then
	do_set_difference := first_op
      else if ops_equal (desired_loc, no_preference) then
	do_set_difference := set_temporary (ctxt_lwbbit, rtime_wordlength)
      else
	do_set_difference := desired;
      if not ops_equal (first_op.arg[1], do_set_difference.arg[1]) then
	set_move (first_op, dupl_set (do_set_difference));
      emit_bit_inst (0, second_op, dupl_set (do_set_difference))
    end

    (* Second operand is not singleton. *)

    else begin
      if not in_context (first_op) then
	make_temp_set (first_op);
      if (not first_op.in_temp andif not ops_equal (first_op.arg[1], desired_loc) andif
			not in_context (second_op)) then
	make_temp_set (second_op);

      (* Choose destination. *)

      if not ops_equal (desired_loc, no_preference) then
	if in_context (second_op) orif ops_equal (first_op.arg[1], desired_loc) then 
	  do_set_difference := desired
	else (* that leaves first_op in_temp, second not in_context *)
	  do_set_difference := dupl_set (first_op)
      else (* no desired location *)
	if first_op.in_temp then
	  do_set_difference := dupl_set (first_op)
	else if second_op.in_temp then
	  do_set_difference := dupl_set (second_op)
	else
	  do_set_difference := set_temporary (ctxt_lwbbit, rtime_wordlength);

      (* Determine the best instruction sequence in the present situation. *)

      two_op_form := ops_equal (first_op.arg[1], do_set_difference.arg[1]);
      if two_op_form then begin
	if first_op.arg[2].cst_part.offset < second_op.arg[2].cst_part.offset then
	  first_op.arg[1] := increment_addr (first_op.arg[1], (second_op.arg[2].cst_part.offset
							    - first_op.arg[2].cst_part.offset) * 2)
      end
      else
	target := duplicate_desc (do_set_difference.arg[1]);

      if not aconstp (second_op.arg[3], n) then
	best_seq := set_diff_rt_call
      else begin
	if_2_op := ord (two_op_form); (* coef for terms applied just for two op form *)
	n_odd := ord (odd (n)); (* coef for terms applied just if n is odd *)
	num_indirect := ord (pincr_free (first_op.arg[1])) + ord (pincr_free (second_op.arg[1]));

	first_sequence (ord (simple_diff_sequence),
			(14  - 4 * if_2_op) * ((n + 1) div 2)               - 2 * num_indirect,
			(29 - 5 * if_2_op) * n + (12 - 3 * if_2_op) * n_odd - 4 * num_indirect);
	next_sequence  (ord (set_diff_rt_call),
			22 - 4 * if_2_op             - 2 * num_indirect,
			340 + (28 - 3 * if_2_op) * n - 4 * num_indirect);
	next_sequence  (ord (diff_word_loop),
			26 - 6 * if_2_op                          - 4 * num_indirect,
			32 - 8 * if_2_op + (39 - 4 * if_2_op) * n - 8 * num_indirect);
	next_sequence  (ord (diff_longword_loop),
			26 - 6 * if_2_op + (8 - 2 * if_2_op) * n_odd         - 4 * num_indirect,
			32 - 8 * if_2_op + (28 - 3 * if_2_op) * n + (1 - 1 * if_2_op) * n_odd
									     - 8 * num_indirect);
	best_seq := set_diff_sequences (best_sequence (bests_size, bests_cycles))
      end;

      case best_seq of

	simple_diff_sequence: begin
	  assert (n in [1..4]); (* if formulas work out as I expect ... *)
	  while n > 0 do begin
	    if n = 1 then
	      generate_actual_diff_seq (size_word)
	    else
	      generate_actual_diff_seq (size_long);
	    n := n - 2;
	    if n > 0 then begin
	      second_op.arg[1] := increment_addr (second_op.arg[1], 4);
	      first_op.arg[1] := increment_addr (first_op.arg[1], 4);
	      if not two_op_form then
		target := increment_addr (target, 4)
	    end
	  end
	end;

	set_diff_rt_call: begin
	  pusha (duplicate_desc (first_op.arg[1]));
	  pusha (duplicate_desc (second_op.arg[1]));
	  if not two_op_form then
	    pusha (duplicate_desc (target));
	  push (duplicate_desc (second_op.arg[3]), size_long);
	  if two_op_form then
	    gen_rt (rt_set_diff2)
	  else
	    gen_rt (rt_set_diff3)
	end;

	diff_word_loop, diff_longword_loop: begin
	  second_op.arg[1] := make_postincr (second_op.arg[1]);
	  first_op.arg[1] := make_postincr (first_op.arg[1]);
	  if not two_op_form then
	    target := make_postincr (target);
	  temp_len := loadi (get_dreg, n div (ord (best_seq = diff_longword_loop) + 1) - 1,
			     size_word, true);
	  loop_label := def_create (local_def);
	  gen_def (code_area, loop_label);
	  if best_seq = diff_word_loop then
	    generate_actual_diff_seq (size_word)
	  else
	    generate_actual_diff_seq (size_long);
	  gen_dbcc (f_cc, temp_len.reg, loop_label);
	  if (best_seq = diff_longword_loop) and odd (n) then
	    generate_actual_diff_seq (size_word);
	  free_desc (temp_len)
	end

      end (* case best_seq *);

      if not two_op_form then
	free_desc (target);
      set_free (first_op);
      set_free (second_op)

    end (* not singleton *);

  end (* have to really do it *);

  exp^.desc.set_lwb := exp^.operand [1]^.desc.set_lwb;
  exp^.desc.set_length := exp^.operand [1]^.desc.set_length
end (* do_set_difference *);
$PAGE gen_union_or_intersection
(* GEN UNION OR INTERSECTION generates the code sequence to carry out a set
   union or intersection operation.  None of the given op_desc's is freed. *)

procedure gen_union_or_intersection (    operation: generic_opcodes;
				     var first_loc, second_loc: op_desc;
					 two_op_form: boolean;
				     var target: op_desc;
					 inter_or_union_length: op_desc);

type
  inter_union_sequences = (simple_sequence, simple_seq_with_leas, runtime_call,
			   word_loop, longword_loop);

var
  temp_len: op_desc;
  n, bests_size, bests_cycles, num_indirect: integer;
  best_seq: inter_union_sequences;
  loop_label: def;
  if_2_op, n_odd: 0..1;

procedure generate_actual_seq (size: op_sizes);
  var
    dreg: data_regs;
  begin
    first_loc.value_size := size;
    second_loc.value_size := size;
    if not two_op_form then target.value_size := size;
    dreg := get_dreg;
    gen_mr (move_opc, second_loc, dreg);	(*	MOVE	OP2,Dn	  *)
    if two_op_form then
      gen_rm (operation, dreg, first_loc)	(*	AND/OR	Dn,OP1	  *)
    else begin
      gen_mr (operation, first_loc, dreg);	(*	AND/OR	OP1,Dn	  *)
      gen_rm (move_opc, dreg, target)		(*	MOVE	Dn,TARGET *)
    end;
    free_reg (dreg)
  end;

begin
  if not aconstp (inter_or_union_length, n) then
    best_seq := runtime_call
  else begin
    if_2_op := ord (two_op_form); (* coef for terms applied just for two op form *)
    n_odd := ord (odd (n)); (* coef for terms applied just if n is odd *)
    num_indirect := ord (pincr_free (first_loc)) + ord (pincr_free (second_loc));

    first_sequence (ord (simple_sequence),
		    (12 - 4 * if_2_op) * ((n + 1) div 2)                     - 2 * num_indirect,
		    (26 - 5 * if_2_op) * n + (11 - 3 * if_2_op) * n_odd      - 4 * num_indirect);
    next_sequence  (ord (simple_seq_with_leas),
		    (12 - 4 * if_2_op) + (6 - 2 * if_2_op) * ((n + 1) div 2) - 4 * num_indirect,
		    (24 - 8 * if_2_op) + (20 - 3 * if_2_op) * n + (5 - if_2_op) * n_odd
									     - 8 * num_indirect);
    next_sequence  (ord (runtime_call),
		    22 - 4 * if_2_op             - 2 * num_indirect,
		    340 + (25 - 3 * if_2_op) * n - 4 * num_indirect);
    next_sequence  (ord (word_loop),
		    24 - 6 * if_2_op                            - 4 * num_indirect,
		    (32 - 8 * if_2_op) + (35 - 4 * if_2_op) * n - 8 * num_indirect);
    next_sequence  (ord (longword_loop),
		    24 - 6 * if_2_op + (6 - 2 * if_2_op) * n_odd                  - 4 * num_indirect,
		    (32 - 8 * if_2_op) + (25 - 3 * if_2_op) * n - if_2_op * n_odd - 8 * num_indirect);
    best_seq := inter_union_sequences (best_sequence (bests_size, bests_cycles))
  end;
 
  case best_seq of

    simple_sequence, simple_seq_with_leas: begin
      assert (n in [1..6]); (* if formulas work out as I expect ... *)
      if best_seq = simple_seq_with_leas then begin
	second_loc := make_postincr (second_loc);
	first_loc := make_postincr (first_loc);
	if not two_op_form then
	  target := make_postincr (target)
      end;
      while n > 0 do begin
	if n = 1 then
	  generate_actual_seq (size_word)
	else
	  generate_actual_seq (size_long);
	n := n - 2;
	if (n > 0) and (best_seq = simple_sequence) then begin
	  second_loc := increment_addr (second_loc, 4);
	  first_loc := increment_addr (first_loc, 4);
	  if not two_op_form then
	    target := increment_addr (target, 4)
	end
      end
    end;

    runtime_call: begin
      pusha (duplicate_desc (first_loc));
      pusha (duplicate_desc (second_loc));
      if not two_op_form then
	pusha (duplicate_desc (target));
      push (duplicate_desc (inter_or_union_length), size_long);
      if two_op_form then
	if operation = and_opc then
	  gen_rt (rt_set_inter2)
	else
	  gen_rt (rt_set_union2)
      else
	if operation = and_opc then
	  gen_rt (rt_set_inter3)
	else
	  gen_rt (rt_set_union3)
    end;

    word_loop, longword_loop: begin
      second_loc := make_postincr (second_loc);
      first_loc := make_postincr (first_loc);
      if not two_op_form then
	target := make_postincr (target);
      temp_len := loadi (get_dreg, n div (ord (best_seq = longword_loop) + 1) - 1,
			 size_word, true);
      loop_label := def_create (local_def);
      gen_def (code_area, loop_label);
      if best_seq = word_loop then
	generate_actual_seq (size_word)
      else
	generate_actual_seq (size_long);
      gen_dbcc (f_cc, temp_len.reg, loop_label);
      if (best_seq = longword_loop) and odd (n) then
	generate_actual_seq (size_word);
      free_desc (temp_len)
    end

  end (* case best_seq *);
end (* gen_union_or_intersection *);
$PAGE do_intersection - in do_set_fetch (in set_fetch)
(* DO INTERSECTION compiles a set intersection expression tuple, and returns
   a set descriptor for its value.  *)

function do_intersection: set_desc;

var
  temp_expr: expr;
  first_op, second_op: set_desc;
  two_op_form, after_last_word: boolean;
  target, new_target, intersect_length, temp_addr: op_desc;
  intersect_lwb, low_empty, intersect_upb, high_empty: set_range;
  
begin
  with exp^ do begin
  
    first_op  := do_set_fetch (operand[1], ctxt_lwbbit, ctxt_lenbits, no_preference);
    second_op := do_set_fetch (operand[2], ctxt_lwbbit, ctxt_lenbits, no_preference);
  
    (* First, see if we can avoid performing the operation. *)
  
    if (desc.set_length = 0) orif (first_op.nargs = 0) orif (second_op.nargs = 0) then begin
      do_intersection.nargs := 0; (* result is unavoidably null *)
      desc.set_lwb := 0;
      desc.set_length := 0;
      set_free (first_op);
      set_free (second_op);
      return  (* <-- return *)
    end;
  
  
    (* Have to really do it. *)
  
    prep_operand (first_op, false (* convert singletons *));
    prep_operand (second_op, false (* convert singletons *));
  
    (* Reverse the operands if helpful in simplifying the possibilities.  At this
       point, each operand is a real set, but each may be
	  (1) in a temp (which implies in_context)
	  (2) in_context but not in a temp
	  (3) not in_context (lwb may be higher than context, upb might be lower).
       We can reduce the 9 combinations to 6 by reversing (if necessary) to ensure
	  (1) if only one is in a temp, its first_op
	  (2) if only one is not in_context, its second_op.        *)
  
    if (second_op.in_temp and not first_op.in_temp) orif
       (in_context (second_op) and not in_context (first_op)) then begin
      reverse_ops (first_op, second_op);
      temp_expr := operand[1];
      operand[1] := operand[2];
      operand[2] := temp_expr
    end;
  
    (* Choose destination. *)
  
    if not ops_equal (desired_loc, no_preference) then
      do_intersection := desired
    else if first_op.in_temp then
      do_intersection := dupl_set (first_op)
    else
      do_intersection := set_temporary (ctxt_lwbbit, rtime_wordlength);
    if ops_equal (second_op.arg[1], do_intersection.arg[1]) then (* second_op is desired location? *)
      reverse_ops (first_op, second_op); (* so we can take advantage of it *)
  
    two_op_form := ops_equal (first_op.arg[1], do_intersection.arg[1]);
    if not two_op_form then
      target := duplicate_desc (do_intersection.arg[1]);
  

    (* Determine width of actual intersection operation.  If its not full
       contextual width, clear the portions of the destination outside the
       width and adjust the addresses accordingly.  *)
  
    if in_context (second_op) then
      intersect_length := second_op.arg[3]
    else begin
      intersect_lwb := max (first_op.arg[2].cst_part.offset, second_op.arg[2].cst_part.offset);
      low_empty := intersect_lwb - do_intersection.arg[2].cst_part.offset;
      if low_empty > 0 then begin
	after_last_word := false;
	if not two_op_form orif  (* don't clear space already null *)
	   (word_num (operand[1]^.desc.set_lwb) < intersect_lwb) then begin
	  if two_op_form then
	    temp_addr := first_op.arg[1]
	  else
	    temp_addr := target;
	  blk_clear (temp_addr, int_desc (low_empty, no_size, true),
		     new_target, after_last_word)
	end;
	if two_op_form and after_last_word then
	  first_op.arg[1] := new_target
	else
	  first_op.arg[1] := increment_addr (first_op.arg[1],
					     (intersect_lwb - first_op.arg[2].cst_part.offset) * 2);
	second_op.arg[1] := increment_addr (second_op.arg[1],
					    (intersect_lwb - second_op.arg[2].cst_part.offset) * 2);
	if not two_op_form then
	  if after_last_word then
	    target := new_target
	  else
	    target := increment_addr (target, low_empty * 2)
      end;
      intersect_upb := min (upb (first_op.arg[2].cst_part.offset, first_op.arg[3].cst_part.offset),
		       upb (second_op.arg[2].cst_part.offset, second_op.arg[3].cst_part.offset));
      intersect_length := int_desc (len (intersect_upb, intersect_lwb), no_size, true);
      high_empty := upb (do_intersection.arg[2].cst_part.offset,
			 do_intersection.arg[3].cst_part.offset) - intersect_upb;
      if (high_empty > 0) andif
	 (not two_op_form orif  (* don't clear space already null *)
	    (word_num (upb (operand[1]^.desc.set_lwb, operand[1]^.desc.set_length))
		    > intersect_upb)) then begin
	if two_op_form then
	  temp_addr := duplicate_desc (first_op.arg[1])
	else
	  temp_addr := duplicate_desc (target);
	blk_clear (increment_addr (temp_addr, intersect_length.cst_part.offset * 2),
		   int_desc (high_empty, no_size, true),
		   new_target, after_last_word);
	free_desc (new_target)
      end
    end;
  

    (* Generate appropriate code sequence. *)

    gen_union_or_intersection (and_opc, first_op.arg[1], second_op.arg[1],
			       two_op_form, target, intersect_length);

    if not two_op_form then
      free_desc (target);
    set_free (first_op);
    set_free (second_op);
  
    desc.set_lwb := max (operand[1]^.desc.set_lwb, operand[2]^.desc.set_lwb);
    desc.set_length := len (min (upb (operand[1]^.desc.set_lwb, operand[1]^.desc.set_length),
			         upb (operand[2]^.desc.set_lwb, operand[2]^.desc.set_length)),
                            desc.set_lwb)
  end (* with *);
end (* do_intersection *);
$PAGE do_union - in do_set_fetch (in set_fetch)
(* DO UNION compiles a set union expression tuple, and returns a set descriptor
   for its value.  *)

function do_union: set_desc;
  
var
  i, num_const, num_nonconst: oper_range;
  current_op, const_op: set_desc;
  set_bits: set_bit_array;
  j, real_lwb, real_upb,
  alloc_len, const_wordlength, const_base, const_lwb, const_upb, first_const_elem: set_range;
  

procedure perform_or (var second_op: set_desc; last_op: boolean);
  
  var
    destination: set_desc;
    two_op_form: boolean;
    target: op_desc;
  
  begin
  
    (* Prepare the operands.  *)
  
    if singleton (do_union) andif not singleton (second_op) then
      reverse_ops (second_op, do_union); (* cater to special casing of singleton operand *)
    prep_operand (do_union, false (* convert singletons *));
    prep_operand (second_op, true (* we'll special case singleton *));
  
    (* Second operand is singleton. *)
  
    if second_op.nargs < 3 then begin
      if do_union.in_temp then
	destination := do_union
      else if (not last_op) orif ops_equal (desired_loc, no_preference) then
        destination := set_temporary (ctxt_lwbbit, rtime_wordlength)
      else
	destination := desired;
      if not ops_equal (do_union.arg[1], destination.arg[1]) then
	set_move (do_union, dupl_set (destination));
      emit_bit_inst (1, second_op, dupl_set (destination))
    end
  
    (* Second operand is not singleton. *)
  
    else begin
      if not in_context (do_union) and not second_op.in_temp and
         not (last_op and ops_equal (desired_loc, second_op.arg[1])) then
	make_temp_set (do_union)
      else if not do_union.in_temp and not in_context (second_op) and
              not (last_op and ops_equal (desired_loc, do_union.arg[1])) then
	make_temp_set (second_op);

      (* Reverse the operands if helpful in simplifying the possibilities. *)

      if (second_op.in_temp and not do_union.in_temp) orif
	 (in_context (second_op) and not in_context (do_union)) then
	reverse_ops (second_op, do_union);

      (* Choose the destination. *)

      if last_op and (not ops_equal (desired_loc, no_preference)) and
	   (in_context (second_op) or not do_union.in_temp) then
	destination := desired
      else if do_union.in_temp then
	destination := dupl_set (do_union)
      else
	destination := set_temporary (ctxt_lwbbit, rtime_wordlength);
      if ops_equal (second_op.arg[1], destination.arg[1]) then
	reverse_ops (second_op, do_union);  (* so we can take advantage of it *)

      two_op_form := ops_equal (do_union.arg[1], destination.arg[1]);
      if not two_op_form then
        target := duplicate_desc (destination.arg[1])
      else if do_union.arg[2].cst_part.offset < second_op.arg[2].cst_part.offset then
	do_union.arg[1] := increment_addr (do_union.arg[1], (second_op.arg[2].cst_part.offset -
							     do_union.arg[2].cst_part.offset) * 2);
  
      (* Generate appropriate code sequence. *)

      gen_union_or_intersection (or_opc, do_union.arg[1], second_op.arg[1],
				 two_op_form, target, second_op.arg[3]);
      if not two_op_form then
	free_desc (target);
      set_free (do_union);
      set_free (second_op)
    end (* not singleton *);

    do_union := destination
  end (* perform_or *);
  
  
begin
  num_const := 0;
  const_wordlength := 0;
  num_nonconst := 0;
  real_upb := 0;
  real_lwb := set_upb_limit;

  (* Process the operands. *)

  for i := 1 to upperbound (exp^.operand) do begin
    current_op := do_set_fetch (exp^.operand[i], ctxt_lwbbit, ctxt_lenbits, no_preference);
    if current_op.nargs > 0 then with exp^.operand[i]^ do begin
      real_lwb := min (exp^.desc.set_lwb, real_lwb);
      real_upb := max (upb (exp^.desc.set_lwb, exp^.desc.set_length), real_upb)
    end;

    (* Sift out the constants and combine them (compile-time). *)

    if (current_op.nargs = 2) andif (aconstp (current_op.arg[1], const_lwb) and
				     aconstp (current_op.arg[2], const_upb)) then begin
      if const_wordlength = 0 then begin (* first constant operand *)
	const_base := word_num (ctxt_lwbbit) * bits_per_word;  (* basing modulo 16 *)
	if aconstp (rtime_wordlength, alloc_len) then
	  first_const_elem := 0 (* stick with contextual lower bound *)
	else begin
	  alloc_len := len_words (ctxt_lwbbit, ctxt_lenbits);
	  first_const_elem := const_lwb - const_base
	end;
	new (set_bits, alloc_len * bits_per_word - 1);
	for j := 1 to alloc_len * bits_per_word - 1 do
	  set_bits^[j] := false;
	const_op := current_op (* remember first in case its only one *)
      end;
      num_const := num_const + 1;
      for j := const_lwb - const_base to const_upb - const_base do
	set_bits^ [j] := true;
      const_wordlength := max (word_num (const_upb - const_base) + 1, const_wordlength);
      first_const_elem := min (const_lwb - const_base, first_const_elem)
    end

    (* Combine nonconstant operands (run-time). *)

    else if current_op.nargs > 0 then begin
      num_nonconst := num_nonconst + 1;
      if num_nonconst = 1 then
	do_union := current_op
      else
	perform_or (current_op, i = upperbound (exp^.operand) (* last operand? *))
    end
  end (* for *);

  (* Finish up by combining resultant constant (if any) and nonconstant (if any). *)

  if num_const > 0 then begin
    if (num_const > 1) orif not singleton (const_op) then
      const_op := gen_cst_set (set_bits, word_num (ctxt_lwbbit),
			       const_wordlength, word_num (first_const_elem) + 1);
    if num_nonconst = 0 then
      do_union := const_op
    else
      perform_or (const_op, true);
    dispose (set_bits)
  end;

  if real_upb >= real_lwb then begin
    exp^.desc.set_lwb := real_lwb;
    exp^.desc.set_length := len (real_upb, real_lwb)
  end
  else begin
    do_union.nargs := 0;
    exp^.desc.set_lwb := 0;
    exp^.desc.set_length := 0
  end
end (* do_union *);
$PAGE do_set_fetch - body (in set_fetch)
  
var
  ctxt_upbbit: -1..maximum (set_range);
  ctxt_lwbword, set_lwb_word, ctxt_upb_word, set_upb_word,
  desc_upb, const_lwb, const_upb: set_range;
  
begin
  ctxt_upbbit := upb (ctxt_lwbbit, ctxt_lenbits);
  
  (* Evaluate the expression. *)
  
  with exp^, do_set_fetch do
    case opcode of
  
      cst_ref,
      ident_ref,
      field_ref,
      buffer_ref,
      ptr_ref,
      array_ref,
      func_call_op: begin (* note: only sets handled here *)
        arg[1] := fetch (exp, memory_modes, indirect_mode, [no_size], false);
	if (ctxt_upbbit < desc.set_lwb) or
	   (upb (desc.set_lwb, desc.set_length) < ctxt_lwbbit) then begin
	  nargs := 0; (* "z" format *)
	  desc.set_lwb := 0;
	  desc.set_length := 0;
	  free_desc (arg[1])
	end
	else (* not null *) begin
	  nargs := 3; (* "l" format *)
	  ctxt_lwbword := word_num (ctxt_lwbbit);
	  set_lwb_word := word_num (desc.set_lwb);
	  (* ensure that actual lowerbound word is no lower than contextual
	     lowerbound word *)
	  if set_lwb_word < ctxt_lwbword then begin
	    desc.set_length := desc.set_length - (ctxt_lwbword * bits_per_word - desc.set_lwb);
	    desc.set_lwb := ctxt_lwbword * bits_per_word;
	    arg[1] := increment_addr (arg[1], (ctxt_lwbword - set_lwb_word) * 2)
	  end;
	  arg[2] := int_desc (word_num (desc.set_lwb), no_size, true); (* lowerbound word *)
	  ctxt_upb_word := word_num (ctxt_upbbit);
	  set_upb_word := word_num (upb (desc.set_lwb, desc.set_length));
	  arg[3] := int_desc (len (min (ctxt_upb_word, set_upb_word), arg[2].cst_part.offset),
			       no_size, true);
	  if set_upb_word > ctxt_upb_word then
	    desc.set_length := len (((ctxt_upb_word + 1) * bits_per_word - 1), desc.set_lwb)
	end;
	in_temp := (opcode = func_call_op) andif in_context (do_set_fetch)
      end;
  
      setcvt_op: begin
        do_set_fetch := do_set_fetch (operand[1], ctxt_lwbbit, ctxt_lenbits, desired_loc);
        desc.set_lwb := operand[1]^.desc.set_lwb;
        desc.set_length := operand[1]^.desc.set_length;
	dec_expr_usage (exp) (* since we didn't use fetch *)
      end;

      gen_set_op: begin
        if upperbound (operand) = 0 (* null set *) then begin
          nargs := 0; (* "z" format *)
	  desc.set_lwb := 0;
	  desc.set_length := 0
	end
        else (* upperbound (operand) = 1 or 2 *) begin
          nargs := 2; (* "o" format *)
          arg[1] := fetch (operand[1], data_modes, dreg_mode, reg_sizes, false);
          lwb_exp := operand[1];
          if upperbound (operand) = 1 then begin (* singleton set *)
            arg[2] := duplicate_desc (arg[1]);
            upb_exp := lwb_exp
          end
          else begin (* range set *)
            arg[2] := fetch (operand[2], data_modes, dreg_mode, reg_sizes, false);
            upb_exp := operand[2]
          end;

          (* detect null set *)

          if (ctxt_lenbits = 0) orif (desc.set_length = 0) orif 
             (ctxt_lwbbit > upb (desc.set_lwb, desc.set_length)) orif
             (desc.set_lwb > ctxt_upbbit) orif
	     (* and, in case the integer expression generation stuff detects
		a constant expression gset_op_bounds didn't ... *)
	     ( (aconstp (arg[1], const_lwb) and aconstp (arg[2], const_upb))
		 andif (const_lwb > const_upb) )  then begin
            nargs := 0;
	    desc.set_lwb := 0;
	    desc.set_length := 0;
            free_desc (arg[1]);
            free_desc (arg[2])
          end
	  else begin
            if aconstp (arg[1], const_lwb) andif (const_lwb < ctxt_lwbbit) then begin
              arg[1].cst_part.offset := ctxt_lwbbit;
	      lwb_exp^.cst_val.ival := ctxt_lwbbit;
	      desc.set_length := desc.set_length - (ctxt_lwbbit - desc.set_lwb);
	      desc.set_lwb := ctxt_lwbbit
	    end;
            if aconstp (arg[2], const_upb) andif (const_upb > ctxt_upbbit) then begin
              arg[2].cst_part.offset := ctxt_upbbit;
	      upb_exp^.cst_val.ival := ctxt_upbbit;
	      desc.set_length := len (ctxt_upbbit, desc.set_lwb)
	    end;
	    desc_upb := upb (desc.set_lwb, desc.set_length);
	    desc.set_lwb := max (desc.set_lwb, word_num (ctxt_lwbbit) * bits_per_word);
	    desc.set_length := len (min (desc_upb, (word_num (ctxt_upbbit) + 1) * bits_per_word - 1),
				    desc.set_lwb)
	  end
        end (* noper = 1 or 2 *);
	dec_expr_usage (exp) (* since we didn't use fetch *)
      end;


      union_op: begin
        do_set_fetch := do_union;
	dec_expr_usage (exp) (* since we didn't use fetch *)
      end;

      both_op: begin
        do_set_fetch := do_intersection;
	dec_expr_usage (exp) (* since we didn't use fetch *)
      end;

      diff_op: begin
        do_set_fetch := do_set_difference;
	dec_expr_usage (exp) (* since we didn't use fetch *)
      end;

    end;
  if do_set_fetch.nargs = 3 then
    assert (do_set_fetch.arg[1].value_size = no_size) (* see that we're consistent *)
end (* do_set_fetch *);
$PAGE set_fetch - body
var
  shaped_lwb, shaped_len: set_range;
  rt_wordlength, rtime_upb, target: op_desc;
  formed_rt_wordlength, ltz_check_reqd: boolean;
  skip_label: def;

function rtime_wordlength (* : op_desc *);

  begin
    if not formed_rt_wordlength then begin
      rt_wordlength := copy_dreg (rtime_upb);
      if rt_wordlength.signed_value then
	gen_im (asr_opc, 4, rt_wordlength)
      else begin
	gen_im (lsr_opc, 4, rt_wordlength);
	rt_wordlength.signed_value := true
      end;
      if word_num (shaped_lwb) > 0 then begin
	if (word_num (shaped_lwb) - 1) > 0 then
	  gen_im (sub_opc, word_num (shaped_lwb) - 1, rt_wordlength)
      end
      else
	gen_im (add_opc, 1, rt_wordlength);
      if ltz_check_reqd then begin
	skip_label := def_create (local_def);
	gen_bcc (gt_cc, skip_label);
	rt_wordlength := loadi (rt_wordlength.reg, 1, rt_wordlength.value_size, true);
	gen_def (code_area, skip_label)
      end;
      formed_rt_wordlength := true
    end;
    rtime_wordlength := rt_wordlength
  end (* rtime_wordlength *);

function find_runtime_upb (set_exp: expr; var ltz_check_required: boolean): op_desc;

  var
    first_op, second_op, temp_op: op_desc;
    i: oper_range;
    min_val, max_val, first_val, second_val: integer;
    fixed, ltzchk_2: boolean;

  procedure prep_and_compare_operands (var first_op, second_op: op_desc; skip_relation: relations);

    procedure skip (cc: condition_codes);
      begin
	gen_bcc (cc, skip_label);
	if skip_relation = lec then
	  assert (cc in [lt_cc, cs_cc, le_cc, ls_cc])
	else (* skip relation = gec *)
	  assert (cc in [gt_cc, hi_cc, ge_cc, cc_cc])
      end;

    var
      size: op_sizes;
      force_signed: boolean;
    begin
      size := max (first_op.value_size, second_op.value_size);
      force_signed := (first_op.signed_value and not first_op.known_positive and not second_op.signed_value)
			or
		      (second_op.signed_value and not second_op.known_positive and not first_op.signed_value);
      if force_signed and
	 ( (not first_op.signed_value and (first_op.value_size = size))
	     or
	   (not second_op.signed_value and (second_op.value_size = size)) ) then
        size := succ (size);
      assert (size in reg_sizes);
      first_op := coerce (first_op, data_modes, dreg_mode, at_least [size], force_signed);
      second_op := coerce (second_op, data_modes, dreg_mode, at_least [size], force_signed);
      if aconstp (first_op, first_val) and (second_op.mode <> dreg_mode) then
	second_op := copy_dreg (second_op);
      if (second_op.mode = dreg_mode) and (first_op.mode <> dreg_mode) then begin
	temp_op := first_op;
	first_op := second_op;
	second_op := temp_op
      end;
      first_op := copy_dreg (first_op);
      skip_label := def_create (local_def);
      compare_integer_ops (first_op, second_op, skip_relation, skip);
      assert ((first_op.value_size = size) and (second_op.value_size = size));
      gen_mm (move_opc, second_op, first_op);
      gen_def (code_area, skip_label);
      free_desc (second_op)
    end;

  begin
    with set_exp^ do begin

      if desc.set_cst_len then
	find_runtime_upb := int_desc (upb (desc.set_lwb, desc.set_length), no_size, true)

      else
	case opcode of

	  setcvt_op,
	  diff_op:
	    find_runtime_upb := find_runtime_upb (operand[1], ltz_check_required);

	  gen_set_op: begin
	    operand [upperbound (operand)]^.usage_count :=
	      operand [upperbound (operand)]^.usage_count + 1; (* keep fetch happy *)
	    find_runtime_upb := fetch (operand [upperbound (operand)], data_modes, dreg_mode,
				       reg_sizes, false);
	    gset_op_bounds (operand[upperbound (operand)], min_val, max_val, fixed);
	    ltz_check_required := min_val < word_num (shaped_lwb) * bits_per_word
	  end;

	  both_op: begin
	    first_op := find_runtime_upb (operand[1], ltz_check_required);
	    second_op := find_runtime_upb (operand[2], ltzchk_2);
	    ltz_check_required := ltz_check_required or ltzchk_2;
	    if aconstp (first_op, first_val) and aconstp (second_op, second_val) then
	      if first_val <= second_val then
		find_runtime_upb := first_op
	      else
		find_runtime_upb := second_op
	    else begin
	      prep_and_compare_operands (first_op, second_op, lec);
	      find_runtime_upb := first_op;
	      find_runtime_upb.signed_value := first_op.signed_value and second_op.signed_value
	    end
	  end;

	  union_op: begin
	    first_op := find_runtime_upb (operand[1], ltz_check_required);
	    for i := 2 to upperbound (operand) do begin
	      second_op := find_runtime_upb (operand[i], ltzchk_2);
	      ltz_check_required := ltz_check_required and ltzchk_2;
	      if aconstp (first_op, first_val) and aconstp (second_op, second_val) then begin
		if first_val < second_val then begin
		  temp_op := first_op;
		  first_op := second_op;
		  second_op := temp_op
	        end
	      end
	      else
		prep_and_compare_operands (first_op, second_op, gec);
	      find_runtime_upb := first_op;
	      find_runtime_upb.signed_value := first_op.signed_value and second_op.signed_value
	    end
	  end;

	end (* case *);
    end (* with *);
  end (* find_runtime_upb *);

begin
  
  (* propagate lwb and length information up from the leaves of the expression tree *)
  
  shape_set (set_exp, shaped_lwb, shaped_len);
  
  (* use best combination of information possible *)
  
  if ctxt_lwbbit > shaped_lwb then begin
    shaped_len := max (0, shaped_len - (ctxt_lwbbit - shaped_lwb));
    shaped_lwb := ctxt_lwbbit
  end;
  if (shaped_lwb + shaped_len) > (ctxt_lwbbit + ctxt_lenbits) then
    shaped_len := max (0, ctxt_lwbbit + ctxt_lenbits - shaped_lwb);
  if shaped_len = 0 then
    shaped_lwb := 0; (* keep null set reps. consistent: len=lwb=0 *)
  
  target := no_preference;
  if (force_fixed or set_exp^.desc.set_cst_len) orif
     (len_words (shaped_lwb, shaped_len) <= 64) then begin
    rt_wordlength := int_desc (len_words (shaped_lwb, shaped_len), no_size, true);
    formed_rt_wordlength := true;
    if (word_num (shaped_lwb) = word_num (ctxt_lwbbit)) andif
       (rt_wordlength.cst_part.offset = len_words (ctxt_lwbbit, ctxt_lenbits)) then
      target := desired_loc
  end
  else begin
    rtime_upb := find_runtime_upb (set_exp, ltz_check_reqd);
    formed_rt_wordlength := false (* defer conversion until wordlength required *)
  end;
  
  (* generate code required to evaluate the set *)
  
  set_fetch := do_set_fetch (set_exp, shaped_lwb, shaped_len, target);
  
  if formed_rt_wordlength then
    free_desc (rt_wordlength)
  else
    free_desc (rtime_upb); (* never did have to calculate length from upb *)
 
end (* set_fetch *);
$PAGE set_assignment
(* SET ASSIGNMENT generates code for assignment of set values. *)

public procedure set_assignment (assign_tpl: tuple);

var
  lhs_desc, rhs_desc: set_desc;
  final_loc: op_desc;
  after_last_word: boolean;
  low_clip, high_clip: 0..15;

begin
  with assign_tpl^ do begin
    with lhs_desc do begin
      nargs := 3;
      in_temp := false;
      arg[1] := fetch (lhs, memory_alterable_modes, indirect_mode, [no_size], false);
      arg[2] := int_desc (word_num (lhs^.desc.set_lwb), no_size, true);
      arg[3] := int_desc (setw_size (lhs), no_size, true)
    end;
    rhs_desc := set_fetch (rhs, lhs^.desc.set_lwb, lhs^.desc.set_length, lhs_desc.arg[1], true);

    if rhs_desc.nargs = 0 (* "z" format *) then begin
      blk_clear (lhs_desc.arg[1], lhs_desc.arg[3], final_loc, after_last_word);
      free_desc (final_loc)
    end

    else begin
      clip_calc (rhs^.desc.set_lwb, rhs^.desc.set_length,
		 lhs^.desc.set_lwb, lhs^.desc.set_length,
		 low_clip, high_clip);
      if (low_clip > 0) or (high_clip > 0) then
	lhs_desc := dupl_set (lhs_desc); (* clip_bits will need it too *)
      if rhs_desc.nargs <> 3 (* "o" format *) then
	genset (rhs_desc, lhs_desc)
      else if not ops_equal (lhs_desc.arg[1], rhs_desc.arg[1]) then (* not where I wanted it? *)
	set_move (rhs_desc, lhs_desc)
      else (* already where I want it *)
	set_free (lhs_desc);
      if (low_clip > 0) or (high_clip > 0) then
	clip_bits (lhs_desc, low_clip, high_clip)
    end
  end
end (* set_assignment *);
$PAGE set_parameter
(* SET PARAMETER evaluates set paramaeters. *)

public procedure set_parameter (actual_expr: expr; parm_kind: sym_kind;
				parm_type: typ; p: procedure (op_desc; boolean));

var
  ctxt_lenbits, ctxt_len_words, low_clip, high_clip: set_range;
  set_loc, temp_set: set_desc;
  result_loc: op_desc;
  after_last_word: boolean;

begin
  assert (parm_type^.kind = sets);
  with parm_type^.set_element_type^ do begin
    ctxt_lenbits := len (maxval (* ctxt_upbbit *), minval (* ctxt_lwbbit *));
    ctxt_len_words := len_words (minval, ctxt_lenbits);
    set_loc := set_fetch (actual_expr, minval, ctxt_lenbits, no_preference, true);

    if parm_kind = values then
      if set_loc.nargs = 0 then begin
	set_loc := set_temporary (minval, int_desc (ctxt_len_words, no_size, true));
	blk_clear (duplicate_desc (set_loc.arg[1]), set_loc.arg[3], result_loc, after_last_word);
	free_desc (result_loc)
      end
      else begin
	clip_calc (actual_expr^.desc.set_lwb, actual_expr^.desc.set_length,
		   minval, ctxt_lenbits,
		   low_clip, high_clip);
	if set_loc.nargs <> 3 then begin
	  temp_set := set_temporary (minval, int_desc (ctxt_len_words, no_size, true));
	  genset (set_loc, dupl_set (temp_set));
	  set_loc := temp_set
	end
	else if (set_loc.arg[2].cst_part.offset > word_num (minval)) orif
	        (set_loc.arg[3].cst_part.offset < ctxt_len_words) orif
		(not set_loc.in_temp and ((low_clip > 0) or (high_clip > 0))) then begin
	  temp_set := set_temporary (minval, int_desc (ctxt_len_words, no_size, true));
	  set_move (set_loc, dupl_set (temp_set));
	  set_loc := temp_set
	end;
	if (low_clip > 0) or (high_clip > 0) then
	  clip_bits (dupl_set (set_loc), low_clip, high_clip)
      end
  end;

  p (set_loc.arg[1] (* freed *), true (* passed by address *))
end (* set_parameter *).
  j@;½