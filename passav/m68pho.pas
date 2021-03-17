$TITLE M68PHO - M68000 peephole optimizer
module m68pho options check;
$PAGE includes
$SYSTEM pascal
$SYSTEM pasfil
$SYSTEM pasist
$SYSTEM ptmcon
$SYSTEM pascv
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM m68cg.typ
$SYSTEM m68utl
$SYSTEM m68cgu
$SYSTEM m68set
$SYSTEM m68emt
$SYSTEM m68mac
$SYSTEM passw
$PAGE types and globals
type
  peephole_techniques =
      ( passes_required,
	blocks_processed,
	unrefd_labels_removed,
	redundant_labels_removed,
	unreachable_coderecs_removed,
	redundant_unc_br_removed,
	unc_branchchaining,
	redundant_cond_br_removed,
	cond_uncond_reverse,
	cond_uncond_redundant_removed,
	c_branchchaining,
	cond_retarget_plus_one,
	xjump_insts_changed,
	case_exc_table_word,
	redundant_tst_removed,
	useless_tst_or_cmp_removed);

  count_array = array [peephole_techniques] of integer;

var
  global_counts: count_array;

const
  technique_descriptions: array [peephole_techniques] of packed array [1..38] of char :=
      ( 'Peepholing passes required:           ',
	'Blocks processed:                     ',
	'Unreferenced labels removed:          ',
	'Redundant labels removed:             ',
	'Unreachable code records removed:     ',
	'Redundant uncond. brs. removed:       ',
	'Unc. branches retargeted from chains: ',
	'Redundant cond. brs. removed:         ',
	'Cond. around uncond. reversed:        ',
	'Cond. to same as uncond. removed:     ',
	'Cond. branches retargeted from chains:',
	'Cond. branches retargeted plus one:   ',
	'Cross-jumping; insts. changed to BRA: ',
	'Case or cond. table word retargeted:  ',
	'Redundant tst removed:                ',
	'Useless tst or cmp removed:           ' );
$PAGE peep_init
(* PEEP INIT performs initialization required by the peephole optimizer. *)

public procedure peep_init;

var
  idx: peephole_techniques;

begin
  for idx := minimum (idx) to maximum (idx) do
    global_counts [idx] := 0
end (* peep_init *);
$PAGE peephole_optimization
(* PEEPHOLE OPTIMIZATION performs a variety of improvements on a code list.  The improvements
   applied to labels and branch instructions are closely patterned after the FINAL
   phase of the optimizing BLISS/11 compiler described by Wulf et al. 

   Many assumptions are involved here.  An attempt has been made to document and
   verify them by the use of assertion statements, especially in build_ref_structure.
   Other important assumptions are:

      - Bcc (and BRA) instructions always have pc_displacement_mode destination
        operands (verified by validate_instruction), never pc_indexed.

      - The kinds of labels referenced by Bcc (and BRA) instructions are always in
	local_def_classes.

      - JMP instructions never have pc_displacement_mode destination operands
	(verified by assertion in translate_opcode).  So, although a JMP may
        reference a local_def_classes label, its not the simple pc_displacement_mode
	reference we could do anything with.      *)


public procedure peephole_optimization (var cdl: code_list);

  const

    (* A branch on condition cc would be taken if and only if a branch on
       reverse_sense [cc] would fall through. *)

    reverse_sense: array [condition_codes] of condition_codes :=
      (	f_cc,		t_cc,		ls_cc,		hi_cc,	
	cs_cc,		cc_cc,		eq_cc,		ne_cc,	
	vs_cc,		vc_cc,		mi_cc,		pl_cc,	
	lt_cc,		ge_cc,		le_cc,		gt_cc  );

    (* If the condition codes are such that a branch on cc would be taken, then
       a branch on a condition in implied_by [cc] would be taken also. *)

    implied_by: array [condition_codes] of set of condition_codes :=
      ( [t_cc],       [t_cc],             [t_cc,hi_cc,cc_cc,ne_cc], [t_cc,ls_cc],
	[t_cc,cc_cc], [t_cc,cs_cc,ls_cc], [t_cc,ne_cc],             [t_cc,eq_cc,ls_cc,le_cc],
	[t_cc,vc_cc], [t_cc,vs_cc],       [t_cc,pl_cc],             [t_cc,mi_cc],
	[t_cc,ge_cc], [t_cc,lt_cc,le_cc], [t_cc,gt_cc,ge_cc],       [t_cc,le_cc] );

    (* If the condition codes are such that a branch on cc would be taken, then 
       a branch on a condition in refuted_by [cc] would NOT be taken. *)

    refuted_by: array [condition_codes] of set of condition_codes :=
      ( [f_cc],       [f_cc],             [f_cc,ls_cc,cs_cc,eq_cc],  [f_cc,hi_cc],
	[f_cc,cs_cc], [f_cc,cc_cc,hi_cc], [f_cc,eq_cc],              [f_cc,ne_cc,hi_cc,gt_cc],
	[f_cc,vs_cc], [f_cc,vc_cc],       [f_cc,mi_cc],              [f_cc,pl_cc],
	[f_cc,lt_cc], [f_cc,ge_cc],       [f_cc,le_cc],              [f_cc,gt_cc] );

    (* Some instructions set the condition codes according to the value of their
       result, just as a tst instruction applied to that result would do. *)

    set_cc_like_tst: set of specific_opcodes :=
      [ and_dm_opc,  and_im_opc,  and_md_opc, 
	eor_dm_opc,  eor_im_opc,
	move_bd_opc, move_mm_opc,
	or_dm_opc,   or_im_opc,   or_md_opc,  
	clr_m_opc,   ext_d_opc,   not_m_opc,   tst_m_opc ];

    (* Some instructions leave the condition codes alone. *)

    leaves_cc_alone: set of specific_opcodes :=
      [ add_ma_opc, bcc_p_opc,  bsr_p_opc,  dbcc_dw_opc,
	exg_aa_opc, exg_da_opc, exg_dd_opc, jmp_m_opc,  jsr_m_opc,
	lea_ma_opc, link_aw_opc, move_ma_opc, movem_mw_opc,
	movem_wm_opc, nop_x_opc, pea_m_opc,  rts_x_opc,
	scc_m_opc,  sub_ma_opc, unlk_a_opc ];

    (* The following is a subset of leaves_cc_alone of instructions nasty
       enough that we can't easily deal with where execution will proceed
       after them. *)

    untouchable: set of specific_opcodes := 
      [ bsr_p_opc,  jmp_m_opc,  jsr_m_opc,  rts_x_opc ];
$PAGE build_ref_structure - in peephole_optimization
(* BUILD REF STRUCTURE scans a code list and constructs a data structure with a
   distinct record for each distinct reference to a label by an instruction operand
   or by a data element (case statement jump tables and the like).  This structure
   allows finding all of the instructions referencing a given label, or given an
   instruction, finding the label it references (if any).  *)

procedure build_ref_structure;


  procedure establish_ref (refing_inst, refed_def_code: code);

    var
      temp_labref: ^label_reference_record;

    begin
      
      (* Create new label-reference record. *)

      new (temp_labref);
      temp_labref^ := (refed_def_code, refing_inst, 0, nil, refed_def_code^.ref_list);
   
      (* Establish linkage from referencing instruction (or data element) to 
	 the label_reference record. *)

      if refing_inst^.kind = inst_code then
	refing_inst^.inst.label_ref := temp_labref
      else
	refing_inst^.label_ref := temp_labref;

      (* Put label-reference record onto (front of) reference-list for the label. *)

      if refed_def_code^.ref_list <> nil then
	refed_def_code^.ref_list^.prev_ref := temp_labref;
      refed_def_code^.ref_list := temp_labref
    end;

  var
    c: code;
    pc_op: 0..2;
    iop: 1..2;

  begin
    
    (* Scan down code list.  A whole raft of assertions are included to verify
       assumptions about how things are "supposed" to be. *)

    c := cdl.first;
    while c <> nil do begin
      with c^ do
	if not active then
	  remove_instruction (cdl, c)

	(* Instruction code record. *)

	else if kind = inst_code then begin
	  assert (inst.label_ref = nil); (* just checking *)
	  pc_op := 0; (* no PC-relative operand seen, so far *)
	  for iop := 1 to 2 do
	    if inst.operands [iop].mode in pc_relative_modes then begin
	      (* On this machine, only one operand of an instruction can be PC-relative. *)
	      assert (pc_op = 0);
	      pc_op := iop;
	      with inst.operands [pc_op].cst_part.reldef^ do
		if kind in local_def_classes then begin
		  (* Check that the def_record has a pointer to a def_code code record,
		     and that that code record correctly points back to the same def_record. *)
		  assert (not defined andif (label_code_rec <> nil) andif
			  (label_code_rec^.kind = def_code) andif
			  (label_code_rec^.defname = inst.operands [pc_op].cst_part.reldef));
		  establish_ref (c, label_code_rec);
		  inst.label_ref^.pc_op := pc_op (* remember which operand it was *)
		end
		else
		  (* Double check that pc-relative instruction operands can only reference
		     five kinds of labels - the three classed as local_def_classes, and ... *)
		  assert (kind in [subr_def, const_def])
	    end
	    else
	      (* Double check that if a non pc-relative operand references a symbolic 
		 definition, its NOT one of the three kinds in local_def_classes.  This
		 ensures that the code above really is finding ALL of the references to
		 those three kinds of labels. *)
	      assert ((inst.operands [iop].cst_part.kind <> def_sc) orif
		      not (inst.operands [iop].cst_part.reldef^.kind in local_def_classes));
	end

	(* Word or longword code record. *)

	else if (kind in [word_code, long_code]) andif
		(relval.kind = def_sc) andif
		(relval.reldef^.kind in local_def_classes) then begin
	  assert (label_ref = nil); (* just checking *)
	  with relval.reldef^ do begin
	    (* Check that the def_record has a pointer to a def_code code record,
	       and that that code record correctly points back to the same def_record. *)
	    assert (not defined andif (label_code_rec <> nil) andif
		    (label_code_rec^.kind = def_code) andif
		    (label_code_rec^.defname = relval.reldef));
	    establish_ref (c, label_code_rec)
	  end
	end;
      c := c^.next
    end (* while *);

    (* In many places within peephole_optimization, we will scan forward from
       a certain kind of code record looking for the next label, etc.  Its
       convenient not to always have to be checking against running off the end
       of the code list, but knowing when thats possible involves assumptions
       about what is at the end of a code list.  Note that we are only concerned
       with lists generated by compile_body, representing the executable code for
       a block, whose I/F must end with one of {return_op, stop_op, goto_op}, and
       an end_block (verified by assertion in compile_body).   It follows that:  *)

    assert ((cdl.last^.kind = inst_code) andif
	    (cdl.last^.inst.opcode in [rts_x_opc, jsr_m_opc, jmp_m_opc]))
  end (* build_ref_structure *);
$PAGE combine_labels - in peephole_optimization
(* COMBINE LABELS removes a redundant label (one that marks the same place in
   the code list as a preceding label) from the code list.  The instructions
   that referenced it are altered to reference the preceding label, and the
   label-reference record list from the removed label are combined with the list
   for the preceding label.  *)

procedure combine_labels (    prec_label: code;
			  var red_label: code);

var
  temp_labref: ^label_reference_record;

begin
  if red_label^.ref_list <> nil then begin

    (* Change the instructions refering to the redundant label to instead
       reference the preceding one. *)

    temp_labref := red_label^.ref_list;
    loop
      with temp_labref^ do begin
	if inst_code_rec^.kind = inst_code then
	  inst_code_rec^.inst.operands [pc_op].cst_part.reldef := prec_label^.defname
	else
	  inst_code_rec^.relval.reldef := prec_label^.defname;
	label_code_rec := prec_label
      end;
     exit if temp_labref^.next_ref = nil;
      temp_labref := temp_labref^.next_ref
    end;

    (* Hook the redundant label's reference list onto the front of that
       of the preceding label. *)

    temp_labref^.next_ref := prec_label^.ref_list;
    prec_label^.ref_list^.prev_ref := temp_labref;
    prec_label^.ref_list := red_label^.ref_list
  end;

  (* Finally, discard the redundant label. *)

  remove_instruction (cdl, red_label)
end (* combine_labels *);
$PAGE next_noncomment, next_noncomment_or_label - in peephole_optimization
(* NEXT NONCOMMENT is given a starting point in a code list, and finds
   the first code record, at or after the starting point, which is not
   merely commentary (i.e. comment_code or source_code kinds).  It will
   also skip labels that are not in local_def_classes.  This is useful since
   the only kind of branches we can readily manipulate are BRA and Bcc's, which
   can only target on labels in those classes.  We generally wish, therfore, to
   treat the other kinds of labels as untouchable and just ignore them.  *)

function next_noncomment (start: code): code;

begin
  next_noncomment := start;
  while (next_noncomment <> nil) andif
	( (next_noncomment^.kind in [comment_code, source_code]) or
	  ((next_noncomment^.kind = def_code) andif
	   not (next_noncomment^.defname^.kind in local_def_classes)) ) do
    next_noncomment := next_noncomment^.next
end (* next_noncomment *);



(* NEXT NONCOMMENT OR LABEL is similar to next_noncomment, except that it skips
   all labels, regardless of their class.  *)

function next_noncomment_or_label (start: code): code;

begin
  next_noncomment_or_label := start;
  while (next_noncomment_or_label <> nil) andif
	(next_noncomment_or_label^.kind in [comment_code, source_code, def_code]) do
    next_noncomment_or_label := next_noncomment_or_label^.next
end (* next_noncomment_or_label *);
$PAGE remove_label_ref - in peephole_optimization
(* REMOVE LABEL REF unchains a label_reference_record from the ref_list of
   the label it is a reference to, and then (optionally) disposes of it.  *)

procedure remove_label_ref (labref: ^label_reference_record;
			    want_dispose: boolean);

begin
  if labref = nil then (* save some checking on part of callers *)
    return;

  with labref^ do begin
    if label_code_rec^.ref_list = labref then begin (* first ref on label's list? *)
      assert (prev_ref = nil); (* just cross checking *)
      label_code_rec^.ref_list := next_ref (* successor is new head of list *)
    end;
    if prev_ref <> nil then
      prev_ref^.next_ref := next_ref; (* unchain *)
    if next_ref <> nil then
      next_ref^.prev_ref := prev_ref; (* unchain *)
  end;

  if want_dispose then
    dispose (labref)
end (* remove_label_ref *);
$PAGE change_target - in peephole_optimization
(* CHANGE TARGET alters a branch instruction or word code record so that its target
   becomes the given def_code code record. *)

procedure change_target (branch_or_word, new_label_code: code;
			 was_old_target: boolean);

var
  labref: ^label_reference_record;

begin
  with branch_or_word^ do begin

    if kind = inst_code then
      labref := inst.label_ref
    else
      labref := label_ref;

    (* Unhook the reference cell from the list of the old target. *)

    if was_old_target then
      remove_label_ref (labref, false);

    (* Update the branch instruction or word code record, and the reference cell to
       point to the new target. *)

    if kind = inst_code then
      inst.operands [2].cst_part := def_reloc (new_label_code^.defname)
    else
      relval.reldef := new_label_code^.defname;
    labref^.label_code_rec := new_label_code;
    labref^.next_ref := new_label_code^.ref_list;

    (* Insert the reference cell at the head of the reference list of the new target. *)

    labref^.prev_ref := nil;
    if new_label_code^.ref_list <> nil then
      new_label_code^.ref_list^.prev_ref := labref;
    new_label_code^.ref_list := labref

  end
end (* change_target *);
$PAGE introduce_label_before - in peephole_optimization
(* INTRODUCE LABEL BEFORE is given a pointer to a code record.  It returns
   a pointer to a def_code code record labeling it.  If there is already
   a suitable label there, a brand new one isn't created.  *)

function introduce_label_before (c: code): code;

var
  temp_c: code;

begin

  (* See if a suitable label is already there. *)

  temp_c := c^.prev;
  while (temp_c <> nil) andif
        ( (temp_c^.kind in [comment_code, source_code]) orif
	  ((temp_c^.kind = def_code) andif not (temp_c^.defname^.kind in local_def_classes)) ) do
    temp_c := temp_c^.prev;

  if (temp_c <> nil) andif (temp_c^.kind = def_code) then begin
    introduce_label_before := temp_c;
    return
  end;

  (* Create a new label and insert it immediately before c. *)

  new (introduce_label_before, def_code);
  with introduce_label_before^ do begin
    defname := def_create (local_def);
    ref_list := nil;
    defname^.label_code_rec := introduce_label_before;
    active := true;
    assert (c^.prev <> nil);
    next := c;
    prev := c^.prev
  end;
  c^.prev^.next := introduce_label_before;
  c^.prev := introduce_label_before

end (* introduce_label_before *);
$PAGE find_following_branch - in peephole_optimization
(* FIND FOLLOWING BRANCH starts from a given def_code code record, and locates the
   first item after it which is not a label or commentary.  If this item is
   the desired kind of branch, a pointer to it is returned.  Otherwise, nil is returned.  *)

function find_following_branch (label_code: code; cc: condition_codes): code;

begin

  find_following_branch := next_noncomment_or_label (label_code^.next);

  with find_following_branch^ do
    if (kind <> inst_code) orif
       ((inst.opcode <> bcc_p_opc) or not (inst.ccode in implied_by [cc])) then
      find_following_branch := nil

end (* find_following_branch *);
$PAGE uncond_branchchaining - in peephole_optimization
(* UNCOND BRANCHCHAINING is given an unconditional branch instruction.  It determines
   if this branch is the start of a chain of uncond. branches, and if so, changes
   the target of every branch in the chain (except the last) to directly reference
   the final target.  *)

procedure uncond_branchchaining (    first_branch: code;
				 var count: integer;
				 var improvements_made: boolean);

var
  next_target_label,
  following_branch,
  temp_branch,
  final_target_label: code;

begin

  (* First, see if we have a branch chain at all. *)

  next_target_label := first_branch^.inst.label_ref^.label_code_rec; (* target def_code *)
  following_branch := find_following_branch (next_target_label, t_cc);
  if following_branch = nil then
    return; (* no branch chain *)

  (* Next, locate the final target label (def_code code record).
  temp_branch := following_branch;
  repeat
    final_target_label := temp_branch^.inst.label_ref^.label_code_rec;
    temp_branch := find_following_branch (final_target_label, t_cc)
  until temp_branch = nil;

  (* Last, retarget each branch in the chain (except the last). *)

  change_target (first_branch, final_target_label, true);
  count := count + 1;
  improvements_made := true; (* signal that another pass should be made *)

  while following_branch^.inst.label_ref^.label_code_rec <> final_target_label do begin
    next_target_label := following_branch^.inst.label_ref^.label_code_rec;
    change_target (following_branch, final_target_label, true);
    count := count + 1;
    following_branch := find_following_branch (next_target_label, t_cc)
  end

end (* uncond_branchchaining *);
$PAGE cond_branchchaining - in peephole_optimization
(* COND BRANCHCHAINING is given a conditional branch instruction.  It determines
   if this branch is the start of a chain of branches such that the condition
   of the initial branch also satisfies the condition of the succeeding ones.
   For example, BEQ  X; ... ; X: BLE  Y; ... ; Y: BRA  Z; ... ; Z:  .  If the 
   conditional branch is the start of such a chain, its target is changed to directly
   reference the final target.

   An additional improvement technique is attempted afterwords, whether or not
   retargetting to bypass a chain is done or not.  If the first instruction
   after the target of the conditional branch is itself a conditional branch,
   and its branch condition is such that it must fall through if the condition
   codes caused the original branch to be taken, then the original branch can
   be retargeted to land after it.  E.g. given  BEQ  X; ...; X: BGT  Y; Z: 
   we can change the BEQ to jump to Z rather than X.   *)

procedure cond_branchchaining (    first_branch: code;
			       var chain_count, plus_one_count: integer;
			       var improvements_made: boolean);

var
  temp_branch,
  final_target_label,
  temp_c,
  new_label: code;

begin

  (* First, see if we have a branch chain at all. *)

  final_target_label := first_branch^.inst.label_ref^.label_code_rec; (* target def_code *)
  temp_branch := find_following_branch (final_target_label, first_branch^.inst.ccode);

  if temp_branch <> nil then begin

    (* Locate the final target label (def_code code record). *)

    repeat
      final_target_label := temp_branch^.inst.label_ref^.label_code_rec;
      temp_branch := find_following_branch (final_target_label, first_branch^.inst.ccode)
    until temp_branch = nil;

    (* Retarget the initial branch. *)

    change_target (first_branch, final_target_label, true);
    chain_count := chain_count + 1;
    improvements_made := true (* signal that another pass should be made *)
  end;

  (* Finally, see if we should actually jump an extra instruction. *)

  temp_c := next_noncomment_or_label (final_target_label^.next);

  with temp_c^ do
    if (kind = inst_code) andif
       ((inst.opcode = bcc_p_opc) and (inst.ccode in refuted_by [first_branch^.inst.ccode])) then begin
      new_label := introduce_label_before (next);
      change_target (first_branch, new_label, true);
      plus_one_count := plus_one_count + 1;
      improvements_made := true (* signal that another pass should be made *)
    end

end (* cond_branchchaining *);
$PAGE cross_jumping - in peephole_optimization
(* CROSS JUMPING carries out the "cross-jumping" optimization described by Wulf et. al.
   The two code sequences are compared.  If the trailing instructions of the secondary
   sequence match those of the primary sequence, they are each replaced by branches
   to the corresponding instructions in the primary sequence.  *)

procedure cross_jumping (    primary_seq_endpoint,
			     secondary_seq_endpoint: code;
			 var count: integer;
			 var improvements_made: boolean);

  function inst_equal (inst1, inst2: instruction): boolean;

    begin
      inst_equal := (inst1.opcode = inst2.opcode) andif
		    (inst1.ccode = inst2.ccode) andif
		    ops_equal (inst1.operands[1], inst2.operands[1]) andif
		    ops_equal (inst1.operands[2], inst2.operands[2])
    end;

  var
    primary,
    secondary,
    new_label: code;

  begin
    primary := primary_seq_endpoint^.prev;
    secondary := secondary_seq_endpoint^.prev;

    loop
      while (primary <> nil) andif
	    (primary^.kind in [comment_code, source_code, def_code]) do
	primary := primary^.prev;
     exit if (primary = nil) orif (primary^.kind <> inst_code);
      while (secondary <> nil) andif
	    (secondary^.kind in [comment_code, source_code, def_code]) do
	secondary := secondary^.prev;
     exit if (secondary = nil) orif (secondary^.kind <> inst_code);
     exit if not inst_equal (primary^.inst, secondary^.inst);
      new_label := introduce_label_before (primary);
      remove_label_ref (secondary^.inst.label_ref, true);
      secondary^.inst := (bcc_p_opc, t_cc, nil, (descriptors [null_mode], descriptors [pc_displacement_mode]));
      secondary^.inst.opcode := bcc_p_opc;
      secondary^.inst.ccode := t_cc;
      secondary^.inst.label_ref := nil;
      secondary^.inst.operands[1] := descriptors [null_mode];
      secondary^.inst.operands[2] := descriptors [pc_displacement_mode];
      new (secondary^.inst.label_ref);
      secondary^.inst.label_ref^ := (nil, secondary, 2, nil, nil);
      change_target (secondary, new_label, false);
      count := count + 1;
      improvements_made := true; (* signal that another pass should be made *)
      primary := primary^.prev;
      secondary := secondary^.prev
    end

  end (* cross_jumping *);
$PAGE cross_jump_all_pairs - in peephole_optimization
(* CROSS JUMP ALL PAIRS is given a list of label_reference_records.  It determines
   which of the referencing instructions are unconditional branches, and feeds
   all possible pairs of them to cross_jumping. *)

procedure cross_jump_all_pairs (    ref_list: ^label_reference_record;
				var count: integer;
				var improvements_made: boolean);

  procedure next_uncond_branch (var ref: ^label_reference_record);
    begin
      while (ref <> nil) andif
	    ( (ref^.inst_code_rec^.kind <> inst_code) orif
	      (ref^.inst_code_rec^.inst.opcode <> bcc_p_opc) orif
	      (ref^.inst_code_rec^.inst.ccode <> t_cc) ) do
        ref := ref^.next_ref
    end;

  var
    primary_ref,
    secondary_ref: ^label_reference_record;

  begin
    primary_ref := ref_list;
    loop
      next_uncond_branch (primary_ref);
     exit if primary_ref = nil;
      secondary_ref := primary_ref^.next_ref;
      loop
        next_uncond_branch (secondary_ref);
       exit if secondary_ref = nil;
	cross_jumping (primary_ref^.inst_code_rec, secondary_ref^.inst_code_rec,
		       count, improvements_made);
	secondary_ref := secondary_ref^.next_ref
      end;
      primary_ref := primary_ref^.next_ref
    end
  end (* cross_jump_all_pairs *);
$PAGE peephole_optimization - body
var
  c,
  temp_c, temp_d: code;
  improvements_made: boolean;
  comment: string [80];
  idx: peephole_techniques;
  local_counts: count_array;

begin
  
  (* Initialize counters to keep track of effectiveness of each technique. *)

  for idx := minimum (idx) to maximum (idx) do
    local_counts [idx] := 0;
  local_counts [blocks_processed] := 1;
  
  (* First, construct the labels vs. references data structure. *)

  build_ref_structure;

  (* Repeatedly scan the code list applying improvement techniques, ceasing when
     no more improvements are possible.  *)

  repeat

    improvements_made := false;
    local_counts [passes_required] := local_counts [passes_required] + 1;
    c := cdl.first;
    while c <> nil do begin

      (* Label. *)

      if (c^.kind = def_code) andif (c^.defname^.kind in local_def_classes) then begin
	if c^.ref_list = nil then begin
	  remove_instruction (cdl, c);
	  improvements_made := true;
	  local_counts [unrefd_labels_removed] := local_counts [unrefd_labels_removed] + 1
	end
	else begin
	  temp_c := c; (* lookahead cursor *)
	  loop
	    temp_c := next_noncomment (temp_c^.next);
	   exit if temp_c^.kind <> def_code;
	    combine_labels (c, temp_c);
	    improvements_made := true;
	    local_counts [redundant_labels_removed] := local_counts [redundant_labels_removed] + 1
	  end;
	  (* Finally, apply the "cross-jumping" optimization to the sequences
	     preceding all possible pairs of unconditional branches to this label. *)
	  cross_jump_all_pairs (c^.ref_list, local_counts [xjump_insts_changed], improvements_made)
	end
      end

      (* Instruction. *)

      else if (c^.kind = inst_code) then begin
	
	(* Unconditional branch. *)

	if (c^.inst.opcode = jmp_m_opc) or 
	   ((c^.inst.opcode = bcc_p_opc) and (c^.inst.ccode = t_cc)) then begin
	  (* First, find the next label.  Anything up until then is unreachable. *)
	  temp_c := c^.next; (* lookahead cursor *)
	  while (temp_c <> nil) andif (temp_c^.kind <> def_code) do begin
	    if not (temp_c^.kind in [source_code, comment_code]) then begin
	      if temp_c^.kind = inst_code then
		remove_label_ref (temp_c^.inst.label_ref, true)
	      else if temp_c^.kind in [byte_code, word_code, long_code] then
		remove_label_ref (temp_c^.label_ref, true);
	      remove_instruction (cdl, temp_c);
	      improvements_made := true;
	      local_counts [unreachable_coderecs_removed] :=
		      local_counts [unreachable_coderecs_removed] + 1
	    end;
	    temp_c := temp_c^.next
	  end;
	  temp_c := next_noncomment (temp_c);
	  if c^.inst.opcode = bcc_p_opc then begin
	    (* Second, see if branch is just to the following label (if there is one). *)
	    if c^.inst.label_ref^.label_code_rec = temp_c then begin
	      remove_label_ref (c^.inst.label_ref, true);
	      remove_instruction (cdl, c);
	      improvements_made := true;
	      local_counts [redundant_unc_br_removed] := local_counts [redundant_unc_br_removed] + 1
	    end
	    else begin
	      (* The branch didn't go away, so look for branch chaining. *)
	      uncond_branchchaining (c, local_counts [unc_branchchaining], improvements_made);
	      (* And, finally, apply the "cross-jumping" optimization to the sequences
		 preceding the branch, and its target label. *)
	      cross_jumping (c^.inst.label_ref^.label_code_rec, c,
			     local_counts [xjump_insts_changed], improvements_made)
	    end
	  end
	end
	
	(* Conditional branch. *)

	else if c^.inst.opcode = bcc_p_opc then begin
	  temp_c := next_noncomment (c^.next);
	  (* First, see if the cond. branch is just to an immediately following
	     label.  *)
	  if (temp_c^.kind = def_code) andif
	     (c^.inst.label_ref^.label_code_rec = temp_c) then begin
	    remove_label_ref (c^.inst.label_ref, true);
	    remove_instruction (cdl, c);
	    improvements_made := true;
	    local_counts [redundant_cond_br_removed] := local_counts [redundant_cond_br_removed] + 1
	  end
	  else begin
	    (* Second, look for (e.g.)  BLT  X;  BRA  Y;  X: --- *)
	    if (temp_c^.kind = inst_code) andif
	       ((temp_c^.inst.opcode = bcc_p_opc) and (temp_c^.inst.ccode = t_cc)) then begin
	      temp_d := next_noncomment (temp_c^.next);
	      if (temp_d^.kind = def_code) andif
		 (c^.inst.label_ref^.label_code_rec = temp_d) then begin
		remove_label_ref (c^.inst.label_ref, true);
		temp_c^.inst.ccode := reverse_sense [c^.inst.ccode];
		remove_instruction (cdl, c);
		improvements_made := true;
		local_counts [cond_uncond_reverse] := local_counts [cond_uncond_reverse] + 1;
		(* We still have a conditional branch (even if not the original),
		   and know where the next following non-commentary item is, so
		   reset the pointers and fall into the next technique. *)
		c := temp_c;
		temp_c := temp_d
	      end
	    end;
	    (* Third, look for (e.g.)  BLT  X;  BRA  X   *)
	    temp_c := next_noncomment_or_label (temp_c);
	    if (temp_c^.kind = inst_code) andif
	       ((temp_c^.inst.opcode = bcc_p_opc) and (temp_c^.inst.ccode = t_cc)) andif
	       (temp_c^.inst.label_ref^.label_code_rec = c^.inst.label_ref^.label_code_rec) then begin
	      remove_label_ref (c^.inst.label_ref, true);
	      remove_instruction (cdl, c);
	      improvements_made := true;
	      local_counts [cond_uncond_redundant_removed] :=
		      local_counts [cond_uncond_redundant_removed] + 1
	    end
	    else
	      (* We've failed to make the conditional branch go away, so look
		 for branch chaining. *)
	      cond_branchchaining (c, local_counts [c_branchchaining],
				   local_counts [cond_retarget_plus_one], improvements_made)
	  end
	end

        (* Test or compare. *)

	else if c^.inst.opcode
			  in [tst_m_opc, cmp_im_opc, cmp_ma_opc, cmp_md_opc, cmp_mm_opc] then begin
	  (* First, see if we have a tst preceded by an instruction that would
	     set the condition codes just as the tst would.  If so, remove the tst. *)
	  if c^.inst.opcode = tst_m_opc then begin
	    temp_c := c^.prev;
	    while (temp_c <> nil) andif
		  (temp_c^.kind in [comment_code, source_code]) do
	      temp_c := temp_c^.prev
	  end;
	  if (c^.inst.opcode = tst_m_opc) andif (temp_c <> nil) andif
	     (temp_c^.kind = inst_code) andif
	     (temp_c^.inst.opcode in set_cc_like_tst) andif
	     ops_equal (c^.inst.operands[2], temp_c^.inst.operands[2]) then begin
	    remove_label_ref (c^.inst.label_ref, true);
	    remove_instruction (cdl, c);
	    improvements_made := true;
	    local_counts [redundant_tst_removed] := local_counts [redundant_tst_removed] + 1
	  end
	  (* Second, see if results of tst or cmp are never utilized. Follow path of
	     execution until the cc's are utilizied, or clobbered, or we have to give up. *)
	  else begin
	    temp_c := next_noncomment_or_label (c^.next);
	    while (temp_c <> nil) andif
		  (temp_c^.kind = inst_code) andif
		  (not (temp_c^.inst.opcode in [bcc_p_opc, scc_m_opc, dbcc_dw_opc]) or
							(temp_c^.inst.ccode = t_cc) ) andif
		  (temp_c^.inst.opcode in leaves_cc_alone - untouchable) do begin
	      if temp_c^.inst.opcode = bcc_p_opc then
		temp_c := temp_c^.inst.label_ref^.label_code_rec;
	      temp_c := next_noncomment_or_label (temp_c^.next)
	    end;
	    if (temp_c <> nil) andif
	       (temp_c^.kind = inst_code) andif
	       not (temp_c^.inst.opcode
				   in [bcc_p_opc, scc_m_opc, dbcc_dw_opc] + untouchable) then begin
	      remove_label_ref (c^.inst.label_ref, true);
	      remove_instruction (cdl, c);
	      improvements_made := true;
	      local_counts [useless_tst_or_cmp_removed] :=
		      local_counts [useless_tst_or_cmp_removed] + 1
	    end
	  end
	end
      end

      (* Word. *)

      else if (c^.kind = word_code) andif
	      (c^.relval.kind = def_sc) andif
	      (c^.relval.reldef^.kind in local_def_classes) then begin
	(* Presumably this is an entry in a case or condition handling table.  If the 
	   target is itself only a branch, retarget the table entry to the
	   branch's target. *)
	temp_c := find_following_branch (c^.relval.reldef^.label_code_rec, t_cc);
	if temp_c <> nil then begin
	  change_target (c, temp_c^.inst.label_ref^.label_code_rec, true);
	  local_counts [case_exc_table_word] := local_counts [case_exc_table_word] + 1;
	  improvements_made := true
	end
      end;

      c := c^.next
    end (* while *);

  until not improvements_made;

  (* Tag comments onto the end of the code list documenting the amounts of
     improvement achieved by each technique. *)

  for idx := minimum (idx) to maximum (idx) do begin
    putstring (comment, technique_descriptions [idx], local_counts [idx]:5);
    gen_cmt (cdl, comment)
  end;

  (* Update global counts of effectiveness of each technique. *)

  for idx := minimum (idx) to maximum (idx) do
    global_counts [idx] := global_counts [idx] + local_counts [idx]

end (* peephole_optimization *);
$PAGE peep_term
(* PEEP TERM finishes up for the peephole optimizer. *)

public procedure peep_term (ass_opt_used: boolean);

var
  idx: peephole_techniques;
  comment: string [80];

begin
  if ass_opt_used and (list_file <> '') then begin
    init_output;
    fio_tab (listfb, 23);
    fio_line (listfb, '*  Peepholing totals:');
    for idx := minimum (idx) to maximum (idx) do begin
      fio_tab (listfb, 23);
      fio_write (listfb, '*');
      fio_tab (listfb, 34);
      putstring (comment, technique_descriptions [idx], global_counts [idx]:5);
      fio_line (listfb, comment)
    end;
  end;

  writeln (ttyoutput);
  for idx := minimum (idx) to maximum (idx) do
    writeln (ttyoutput, technique_descriptions [idx], global_counts [idx]:5)
end (* peep_term *).
  VEAÙ