$TITLE PASOPT.PAS, last modified 5/14/84, zw
PROGRAM pasopt options special(word), storage(4096);
(*TYM-Pascal compiler  Optimization Module*)
$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE ptmcon.inc
$INCLUDE pasfil.inc
$include paspt.typ
$INCLUDE pasif.typ
$include pasenv.inc
$INCLUDE pastal.inc
$INCLUDE paserr.inc
$INCLUDE passet.inc
$INCLUDE passw.inc
$INCLUDE pasifu.inc
$INCLUDE pasjmp.inc
$INCLUDE pasopt.TYP
$INCLUDE pa2fld.inc
$INCLUDE pasesu.inc
$INCLUDE ptmsel.inc
$INCLUDE corout.inc
$INCLUDE pa2dmp.inc
$INCLUDE pasdmp.inc
$INCLUDE pa2xrf.inc
$INCLUDE prgdir.inc
$INCLUDE infpac.inc
$SYSTEM RUNUTL.INC
$INCLUDE tmpnam.inc
public var
    smod: svector; (* The MOD relation. *)
    suse: svector; (* The USE relation. *)
var
(*  Description of the basic block structure of a subroutine.  *)
    n_blocks: index_range; (* The number of basic blocks in this subr. *)
    block_labels: tpl_vector; (* The label nodes of the individual basic blocks. *)
(*  Information from by MakeDominatorTree.  *)
    idom: index_vector; (* The immediate dominator array. *)
    dom_son: index_vector; (* The first-son array. *)
    dom_brother: index_vector; (* The next-son array. *)
    reducible:  boolean;
(*  Formal reference expression inofrmation for global flow analysis.  *)
    n_fre: fre; (* The number of distinct FREs. *)
  io_fre: fre; (* The FRE for IoOpsym. *)
    fre_table: fre_vector; (* The actual array of FRE nodes. *)
    input_set: svector; (* An FRE, i, will be in InputSet[j] if
                           FRE i represents an input tuple to
                           basic block j. *)
    output_set: svector; (* An FRE, i, will be in OutputSet[j] if
                            FRE i represents a variable which is
                            modified in basic block j. *)
    input_tuples: tpl_list_vec; (* The input tuple lists for each basic block. *)
(*  The results of the IDEF computation.  *)
    idef: svector;
    r_set: svector; (* R is precomputed for the weak
                       environment computation. *)
(*  DoSummaryAnalysis performs a summary data flow analysis, as described in
    CIN-#7.  The final result of the analysis is the sets SMOD and SUSE,
    which are left as global variables for use in MakeBasicBlocks (which uses
    the non-local jump information) and in Optimize.  *)
public procedure do_summary_analysis;
var
    vv: svector;
(*  The following variables are used in Propagate.  They are declared here
    to eliminate the need for constant allocation and deallocation of the
    heap storage that they use.  *)
var
    visited, stacked: svector;
    dfnumber, lowlink, sccroot: index_vector;
(*  Propagate applies Algorithm 1A of CIN-#7 for set propagation by depth-
    first search.  MOD_SET and USE_SET are the set vectors to which propa-
    gation is to be applied.  LEVEL is the level of variables which are to
    be propagated.  If LEVEL is 0, then this is the computation of the
    subroutine class information, and no scope restrictions are applied.
    The information about visited nodes is maintained in the set VISITED.
    The set STACKED is used to record whether a node is currently on the
    stack.  The stack itself is kept using the SCCROOT links, since the
    actual SCCROOT of a node is not determined until the node is removed
    from the stack.  *)
procedure propagate ( mod_set, use_set: svector; level: level_index );
var
    s: index_range;
    count: index_range;
(*  If LEVEL is non-zero, then MergeSets has the effect:
        MOD_SET (q) := MOD_SET (q) + (MOD_SET (p) * VV[i])
        USE_SET (q) := USE_SET (q) + (USE_SET (p) * VV[i])
    If LEVEL is zero, then MergeSets has the effect:
        MOD_SET (q) := MOD_SET (q) + MOD_SET (p)
        USE_SET (q) := USE_SET (q) + USE_SET (p)                        *)
procedure merge_sets ( p, q: index_range );
begin
  if level = 0 then begin
    union (mod_set, p, q);
    union (use_set, p, q);
  end
  else begin
    mov_set (vv, level, mod_set, 0);
    intersect (mod_set, p, 0);
    union (mod_set, 0, q);
    mov_set (vv, level, use_set, 0);
    intersect (use_set, p, 0);
    union (use_set, 0, q);
  end;
end (* merge_sets *);
procedure dfs ( b: blk );
var
    c: call_link;
    p, q: index_range;
begin
  p := b^.number;
  add_elem (visited, 0, p); (* Mark p "visited". *)
  count := count + 1;
  dfnumber^ [p] := count;
  lowlink^ [p] := count;
  sccroot^ [p] := s; (* Push p on the stack. *)
  s := p;
  add_elem (stacked, 0, p);
  (*  The call tree for a block is ordered by decreasing level number.  Thus,
      when processing the list of procedures called by a given block, we can
      stop as soon as we find a procedure whose level is not greater than the
      level that we are processing.  *)
  c := b^.calls;
  while (c <> nil) andif ((c^.called_subr^.level > level) orif (level = 0)) do begin
    q := c^.called_subr^.number;
    if not in_set (visited, 0, q) then begin
      dfs (c^.called_subr);
      lowlink^ [p] := min (lowlink^ [p], lowlink^ [q]);
    end
    else if (dfnumber^ [q] < dfnumber^ [p]) andif
      in_set (stacked, 0, q) then
        lowlink^ [p] := min (lowlink^ [p], dfnumber^ [q]);
    if not in_set (stacked, 0, q) then (* Merge the set from q into p. *)
      merge_sets (sccroot^ [q], p);
    c := c^.rlink;
  end;
  if lowlink^ [p] = dfnumber^ [p] then begin (* P is an SCC root. *)
    repeat
      q := s; (* Pop q from stack s. *)
      s := sccroot^ [q];
      del_elem (stacked, 0, q);
      merge_sets (q, p);
      sccroot^ [q] := p;
    until q = p;
  end;
end (* dfs *);
var b: blk;
begin
  s := 0; (* Empty stack. *)
  clr_set (stacked, 0);
  count := 0;
  clr_set (visited, 0); (* All nodes are unvisited. *)
  (*  Since the lex thread is ordered by decreasing level number, all blocks
      whose levels are greater than a specified level will precede any blocks
      whose levels are less than or equal to the specified level in the list.  *)
  b := lex_block;
  while (b <> nil) andif ((b^.level > level) orif (level = 0)) do begin
    if not in_set (visited, 0, b^.number) then
      dfs (b);
    b := b^.lex_thread;
  end;
  (*  Now set the MOD and USE sets for each block to the corresponding sets
      for the SCC root of the block.  *)
  b := lex_block;
  while (b <> nil) andif ((b^.level > level) orif (level = 0)) do begin
    merge_sets (sccroot^ [b^.number], b^.number);
    b := b^.lex_thread;
  end;
end (* propagate *);
var
    i: index_range;
    v: vl_link;
    b: blk;
    c: call_link;
    classmod, classuse: svector;
begin
  if switch (prog_options.dump_switches, 'VLSYMS') then
    dmpvlsyms;
  (*  First we allocate some working storage for the propagate procedure.  *)
  visited := new_svector (0, sym_vl_number);
  stacked := new_svector (0, sym_vl_number);
  new (dfnumber, blk_number);
  new (lowlink, blk_number);
  new (sccroot, blk_number);
  (*  VV is a vector of sets.  It is initialized so that VV[i] is the set of
      variables which are defined in blocks at level i.  *)
  vv := new_svector (max_level, sym_vl_number);
  for i := 0 to max_level do
    clr_set (vv, i);
  v := vl_list;
  while v <> nil do begin
    with v^.symbol^ do begin
      if block <> nil then begin
        if dcl_class in [static_sc, dynamic_sc, fileblk_sc, opt_sc]
          then add_elem (vv, 1, id_number)
          else add_elem (vv, block^.level, id_number);
      end;
    end;
    v := v^.last;
  end;
  if switch (prog_options.dump_switches, 'VV') then
    dmpset (vv, sym_vl_number, 'VARIABLES DECLARED AT LEVELS');
  (*  Create the SMOD and SUSE vectors, and call the cross reference module
      to initialize them to MOD0 and USE0 (that is, to the sets of variables
      which are directly modified by the individual blocks).  *)
  smod := new_svector (blk_number, sym_vl_number);
  suse := new_svector (blk_number, sym_vl_number);
  for i := 0 to blk_number do begin
    clr_set (smod, i);
    clr_set (suse, i);
  end;
  sum_flow (smod, suse);
  if switch (prog_options.dump_switches, 'USEMOD0') then begin
    dmpset (smod, sym_vl_number, 'DIRECTMOD');
    dmpset (suse, sym_vl_number, 'DIRECTUSE');
  end;
  (*  Now remove all references in blocks at level i to variables declared at
      level i.  This is equivalent to replacing MOD0 and USE0 by (MOD0 and
      SCOPE) and (USE0 and SCOPE).  *)
  b := lex_block;
  i := max_level + 1;
  while b <> nil do begin
    if i <> b^.level then begin
      i := b^.level;
      mov_set (vv, i, smod, 0);
      mov_set (vv, i, suse, 0);
    end;
    subtract (smod, 0, b^.number);
    subtract (suse, 0, b^.number);
    b := b^.lex_thread;
  end;
  if switch (prog_options.dump_switches, 'USEMOD1') then begin
    dmpset (smod, sym_vl_number, 'DIRECTMOD AND SCOPE');
    dmpset (suse, sym_vl_number, 'DIRECTUSE AND SCOPE');
  end;
  (*  For each scope level, propagate all information (in the absence of
      subroutine classes) for variables defined at that level.  *)
  for i := 1 to max_level do
    propagate (smod, suse, i);
  if switch (prog_options.dump_switches, 'USEMOD2') then begin
    dmpset (smod, sym_vl_number, 'MOD w/o SUBR CLASSES');
    dmpset (suse, sym_vl_number, 'USE w/o SUBR CLASSES');
  end;
  del_svector (vv);
  (*  If there are any subroutine classes defined, then we must allow for
      their effects.  First define CLASSMOD and CLASSUSE for true subroutines,
      to be [], and for classes, to be the union of the MOD (USE) sets of all
      subroutines which are "called" by the class.  *)
  if root_block^.peer <> nil then begin (* There are classes defined. *)
    classmod := new_svector (blk_number, sym_vl_number);
    classuse := new_svector (blk_number, sym_vl_number);
    for i := 0 to blk_number do begin
      clr_set (classmod, i);
      clr_set (classuse, i);
    end;
    b := root_block^.peer;
    while b <> nil do begin (* Compute CLASSMOD(b) and CLASSUSE(b). *)
      c := b^.calls;
      while c <> nil do begin (* Process each subroutine called by b. *)
        i := c^.called_subr^.number;
        mov_set (smod, i, classmod, 0);
        union (classmod, 0, i);
        mov_set (suse, i, classuse, 0);
        union (classuse, 0, i);
        c := c^.rlink;
      end;
      b := b^.peer;
    end;
    if switch (prog_options.dump_switches, 'USEMOD2') then begin
      dmpset (classmod, sym_vl_number, 'CLASSMOD0');
      dmpset (classuse, sym_vl_number, 'CLASSUSE0');
    end;
    (*  Now propagate the CLASSMOD and CLASSUSE information through the entire
        call graph, including subroutine class nodes, and ignoring lexical
        level information.  *)
    propagate (classmod, classuse, 0);
    if switch (prog_options.dump_switches, 'USEMOD3') then begin
      dmpset (classmod, sym_vl_number, 'FINAL CLASSMOD');
      dmpset (classuse, sym_vl_number, 'FINAL CLASSUSE');
    end;
    (*  Finally, merge the CLASSMOD and CLASSUSE sets back into the SMOD and
        SUSE sets.  *)
    for i := 1 to blk_number do begin
      mov_set (classmod, i, smod, 0);
      union (smod, 0, i);
      mov_set (classuse, i, suse, 0);
      union (suse, 0, i);
    end;
    del_svector (classmod);
    del_svector (classuse);
  end (* root_block^.peer <> nil *);
  clr_set (smod, 0);
  clr_set (suse, 0);
  if switch (prog_options.dump_switches, 'USEMOD') then begin
    dmpset (smod, sym_vl_number, 'FINAL MOD RELATION');
    dmpset (suse, sym_vl_number, 'FINAL USE RELATION');
  end;
  (*  Dispose of the working storage we have acquired.  *)
  del_svector (visited);
  del_svector (stacked);
  dispose (dfnumber);
  dispose (lowlink);
  dispose (sccroot);
end (* do_summary_analysis *);
(*  Effects is called with an intermediate form procedure or function call node.
    It computes, in SMOD(0) and SUSE(0), the unions of SUSE and SMOD for the
    called procedure and for any subroutine parameters in the argument list
    of the call, plus the base symbols of any var parameters.  *)
public procedure effects ( node: tuple );
(*  SubrBlock is called with an expression denoting a subroutine (either the
    subroutine for the call, or a subroutine parameter).  It returns the block
    node representing that subroutine in the call graph.  (See also RepBlock in
    PASCGR, which is used in Pass 1 during call graph construction.  *)
function subr_block ( subr: expr ): blk;
begin
  with subr^ do begin
    if opcode = cst_ref then (* Ordinary subroutine. *)
      subr_block := cst_val.blkp
    else if (opcode = ident_ref) andif
            (id_sym^.kind = consts) andif
            (id_sym^.dcl_class = external_sc) then (* External subroutine. *)
      subr_block := ext_block
    else (* Subr var or parm; class block previously computed. *)
      subr_block := desc.base^.class_block;
  end;
end (* subr_block *);
var
    b: blk;
    p: index_range;
    i: int_type;
    arg: expr;
    id: index_range;
begin
  with node^ do begin
    b := subr_block (subr); (* Get the sets for the called subroutine. *)
    p := b^.number;
    cpy_set (smod, p, 0);
    cpy_set (suse, p, 0);
    for i := 1 to upperbound (arglist) do begin (* Process each argument of the call. *)
      arg := arglist [i];
      if arg^.desc.kind in [procs, funcs] then begin (* Is it a subr parm? *)
        b := subr_block (arglist[i]); (* Yes - account for its effects. *)
        p := b^.number;
        union (smod, p, 0);
        union (suse, p, 0);
      end;
      if arg^.context = varx then (* Var parameter. *)
        while arg <> nil do begin (* Find the base symbol of the argument. *)
          with arg^ do begin
            case opcode of
              ident_ref:
                begin
                  id := id_sym^.id_number;
                  add_elem (smod, 0, id);
                  add_elem (suse, 0, id);
                  arg := nil;
                end;
              ptr_ref:
                begin
                  id := base_ptr^.desc.base^.heap_class^.id_number;
                  add_elem (smod, 0, id);
                  add_elem (suse, 0, id);
                  arg := nil;
                end;
              buffer_ref:
                begin
                  id := base_file^.desc.base^.file_class^.id_number;
                  add_elem (smod, 0, id);
                  add_elem (suse, 0, id);
                  arg := nil;
                end;
              array_ref:
                arg := base_array;
              field_ref:
                arg := base_rec;
              substr_ref:
                arg := base_string
            end (* case opcode *);
          end (* with arg^ *);
        end (* while arg <> nil *);
    end (* for i *);
  end (* with node^ *);
end (* effects *);
(*  CircFields is called with a pair of field symbols, Fa and Fb, for fields
    in the same record type.  The question is where (1) Fa and Fb are the same
    field, or (2) Fa is the tag field of a discriminated union which Fb is in
    one of the variants of, or (3) Fa and Fb are in different variants of some
    undiscriminated union.  *)
function circ_fields ( fa, fb: sym ): boolean;
var
    a_tag: typ; (* The variant tag below the rec/var or Fa. *)
    a_var: typ; (* Scans up the Fa variant chain. *)
    b_var: typ; (* Scans up the Fb variant chain. *)
begin
  (*  See if Fa and Fb are the same fields.  *)
  if fa = fb then begin
    circ_fields := true;
    return;
  end;
  (*  See if Fb is a field in a variant of which Fa is the tag field.  *)
  a_tag := fa^.fld_variant^.variant_tag;
  if (a_tag <> nil) andif (a_tag^.tag_field = fa) then begin
    (*  Fa is a tag field.  The question now is whether Fb is in a variant
        controlled by Fa.  The question is actually recursive:  is Fb in a
        variant ... in a variant controlled by Fa?  *)
    b_var := fb^.fld_variant;
    while (b_var^.kind = variants) andif
      (b_var^.tag <> a_tag) do
        b_var := b_var^.tag^.tag_recvar;
    if b_var^.kind = variants then begin
      circ_fields := true;
      return;
    end;
  end;
  (*  See if Fa and Fb are fields in distinct variants controlled by the same
      undiscriminated union.  *)
  a_var := fa^.fld_variant;
  b_var := fb^.fld_variant;
  if (a_var^.kind = variants) and (b_var^.kind = variants) then begin
    (*  Fa and Fb are both fields in variants.  Could they be fields
        of different variants of an undiscriminated union?  *)
    while a_var^.kind = variants do begin
      if a_var^.tag^.tag_field = nil then begin
        (*  We have found an undiscriminated union with a variant containing
            Fa.  Now scan to see if Fb is in a variant of the same undiscri-
            minated union.  *)
        b_var := fb^.fld_variant;
        while (b_var^.kind = variants) andif
          (b_var^.tag <> a_var^.tag) do
            b_var := b_var^.tag^.tag_recvar;
        if b_var^.kind = variants then begin
(*=====>  Common undiscriminated union found for Fa and Fb!!!
          See if Fa and Fb are in different variants.
          Return in any case.  *)
          circ_fields := (a_var <> b_var);
          return;
        end;
      end (* undiscriminated union containing Fa *);
      a_var := a_var^.tag^.tag_recvar;
    end (* while a_var^.kind = variants *);
  end (* if Fa and Fb are both in variants *);
  (*  None of the above.  *)
  circ_fields := false;
end (* circ_fields *);
(*  This function tests whether two tuples satisfy the 'circumscribes' relation,
    as defined in CIN-#5.  *)
function circumscribes ( a, b: tuple ): boolean;
(*  DistinctIndices returns true if (1) Ia and Ib are distinct constants, or
    (2) Ia = Ib + C (C <> 0), or (3) Ib = Ia + C (C <> 0), or (4) Ia = X + C1
    and Ib = X + C2 (C1 <> C2), where X is any expression.  Since Ia and Ib
    have been folded, we know that neither has the form C1 + C2 or X + 0.  *)
function distinct_indices ( ia, ib: expr ): boolean;
var
    a_base, b_base: expr; (* The non-constant term from Ia/Ib. *)
    a_cst, b_cst: int_type; (* The consterm from Ia/Ib. *)
  function sum_test ( i: expr; var base: expr; var cst: int_type ): boolean;
  begin
    with i^ do begin
      if (opcode = iadd_op) andif (operand[1]^.opcode = cst_ref) then begin
        sum_test := true;
        base := operand [2];
        cst := operand[1]^.cst_val.ival;
      end
      else if (opcode = iadd_op) andif (operand[2]^.opcode = cst_ref) then begin
        sum_test := true;
        base := operand [1];
        cst := operand[2]^.cst_val.ival;
      end
      else if (opcode = isub_op) andif (operand[2]^.opcode = cst_ref) then begin
        sum_test := true;
        base := operand [1];
        cst := - operand[2]^.cst_val.ival;
      end
      else
        sum_test := false;
    end (* with i^ *);
  end (* sum_test *);
begin
  distinct_indices :=
    ( (ia^.opcode = cst_ref) andif (ib^.opcode = cst_ref) andif
      ( (ia^.cst_val.kind <> scalar_cst) orif
	(ib^.cst_val.kind <> scalar_cst) orif
	(ia^.cst_val.ival <> ib^.cst_val.ival) )    ) orif
    ( sum_test (ia, a_base, a_cst) andif
      ( (a_base = ib) orif
        sum_test (ib, b_base, b_cst) andif (a_base = b_base) andif (a_cst <> b_cst) ) ) orif
    ( sum_test (ib, b_base, b_cst) andif (b_base = ia) );
end (* distinct_indices *);
begin
  with a^ do begin
    if opcode <> b^.opcode then
      circumscribes := false
    else begin
      case opcode of
        ident_ref:
          circumscribes :=
            (id_sym = b^.id_sym);
        field_ref:
          circumscribes :=
            circumscribes (base_rec, b^.base_rec) andif
            circ_fields (field_sym, b^.field_sym);
        ptr_ref:
          circumscribes :=
            (base_ptr^.desc.base^.heap_class = b^.base_ptr^.desc.base^.heap_class);
        buffer_ref:
          circumscribes :=
            (base_file^.desc.base^.file_class = b^.base_file^.desc.base^.file_class);
        array_ref:
          circumscribes :=
            circumscribes (base_array, b^.base_array) andif
            not distinct_indices (index_val, b^.index_val);
        substr_ref:
          circumscribes :=
            circumscribes (base_string, b^.base_string) andif not
              ( (desc.kind = chars) andif (b^.desc.kind = chars) andif
                distinct_indices (substr_index, b^.substr_index) )
      end (* case opcode *);
    end (* a^.opcode = b^.opcode *);
  end (* with a^ *);
end (* circumscribes *);
(*  ConstMatch takes two VAL nodes, returning true if they represent the same
    constant value.  Record, array and set constants are only checked to see
    if they have the same value pointers.  *)
function const_match ( c1, c2: val ): boolean;
begin
  if c1.kind <> c2.kind then
    const_match := false
  else begin
    case c1.kind of
      scalar_cst:
        const_match := (c1.ival = c2.ival);
      real_cst:
        const_match :=
          (c1.valp^.real_val = c2.valp^.real_val) andif
          (c1.valp^.real_prec = c2.valp^.real_prec);
      string_cst:
        const_match :=
          (length (c1.valp^.str_val) = length (c2.valp^.str_val)) andif
          (c1.valp^.str_varying_ref = c2.valp^.str_varying_ref) andif
	  (c1.valp^.str_val = c2.valp^.str_val);
      set_cst,
      array_cst,
      record_cst:
        const_match := (c1.valp = c2.valp);
      ptr_cst:
        const_match := true; (* The only pointer constant is NIL. *)
      subr_cst:
        const_match := (c1.blkp = c2.blkp);
      no_value:
        const_match := true
    end (* case *);
  end;
end (* const_match *);
(*  TplMatch compares two expressions to determine if they have the same
    opcode and operands.  *)
function tpl_match ( t, t1: expr ): boolean;
var
    i: int_type; (* Operand index variable. *)
begin
  with t^ do
    if t1^.opcode = opcode then begin
      case opcode of
        cst_ref:
          tpl_match := const_match (t^.cst_val, t1^.cst_val);
        ident_ref:
          tpl_match := (t1^.id_sym = id_sym);
        field_ref:
          tpl_match :=
            (t1^.base_rec = base_rec) andif
            (t1^.field_sym = field_sym);
        ptr_ref:
          tpl_match := (t1^.base_ptr = base_ptr);
        buffer_ref:
          tpl_match := (t1^.base_file = base_file);
        array_ref:
          tpl_match :=
            (t1^.base_array = base_array) andif
            (t1^.index_val = index_val);
        substr_ref:
          tpl_match :=
            (t1^.base_string = base_string) andif
            (t1^.substr_index = substr_index) andif
            (t1^.substr_length = substr_length);
        func_call_op:
          if (t1^.subr = subr) andif
	     (upperbound (t1^.arglist) = upperbound (arglist)) then begin
            tpl_match := true;
            for i := 1 to upperbound (arglist) do
              exit if t1^.arglist[i] <> arglist[i] do
                tpl_match := false;
          end
          else
            tpl_match := false;
        nary_op..bnot_op, (* All the n-ary ops except for the *)
        trunc_op..last_nary_op, (*   convert operators. *)
        first_chk_op .. last_chk_op:
          if (t1^.opcode = opcode) andif
	     (upperbound (t1^.operand) = upperbound (operand)) then begin
            tpl_match := true;
            for i := 1 to upperbound (operand) do
              exit if t1^.operand[i] <> operand[i] do
                tpl_match := false;
          end
          else
            tpl_match := false;
        float_op:
          tpl_match :=
            (t1^.operand[1] = operand[1]) andif
            (t1^.desc.precision = desc.precision);
        sclcvt_op:
          tpl_match :=
            (t1^.operand[1] = operand[1]) andif
            (t1^.desc.signed = desc.signed) andif
            (t1^.desc.int_prec = desc.int_prec);
        strcvt_op:
          tpl_match :=
            (t1^.operand[1] = operand[1]) andif
            (t1^.desc.str_kind = desc.str_kind) andif
            (t1^.desc.str_flex = desc.str_flex) andif
            (t1^.desc.str_length = desc.str_length);
        setcvt_op:
          tpl_match :=
            (t1^.operand[1] = operand[1]) andif
            (t1^.desc.set_cst_lwb = desc.set_cst_lwb) andif
            (t1^.desc.set_cst_len = desc.set_cst_len) andif
            (t1^.desc.set_lwb = desc.set_lwb) andif
            (t1^.desc.set_length = desc.set_length);
        display_op:
          tpl_match := (t1^.nlevels = nlevels);
        addr_ref,
        mem_ref,
        immed_ref:
          if (t1^.item.base = item.base) andif
            (t1^.item.index = item.index) andif
            (t1^.item.offset = item.offset) andif
            (t1^.item.pack = item.pack) andif
            (t1^.item.size = item.size) andif
            (t1^.item.class = item.class) then
              if item.class = constant_sc then
                with item.cstref^, t1^ do begin
                  case kind of
                    scalar_cst:
                      tpl_match := (scalar_val = item.cstref^.scalar_val);
                    real_cst:
                      tpl_match :=
                        (real_val = item.cstref^.real_val) andif
                        (real_prec = item.cstref^.real_prec);
                    string_cst:
                      tpl_match :=
			(length (str_val) = length (item.cstref^.str_val)) andif
                        (str_varying_ref = item.cstref^.str_varying_ref) andif
			(str_val = item.cstref^.str_val);
                    others:
                      tpl_match := (t^.item.cstref = t1^.item.cstref)
                  end (* case *);
                end (* with *)
              else if item.class in [local_sc..code_sc, absolute_sc] then
                tpl_match := (t1^.item.sym_name = item.sym_name)
              else
                tpl_match := true
          else
            tpl_match := false;
        others:
          tpl_match := false
      end (* case kind *);
    end
    else (* t1^.opcode <> opcode *) begin
      tpl_match := false;
    end;
end (* tpl_match *);
(*  CvalCode returns an arbitrary integer representing a constant value.  This
    integer may be used in computing hash functions.  *)
function cval_code ( c: val_ptr ): int_type;
var ilog: int_type;
begin
  with c^ do begin
    case kind of
      scalar_cst:
        cval_code := scalar_val;
      real_cst:
        if real_val = 0.0 then
          cval_code := real_prec
        else begin (* Scale to the range 10^0 .. 10^4. *)
          ilog := trunc ( log ( abs ( real_val) ) );
          cval_code := real_prec + trunc (real_val * (10.0 ** (ilog mod 4 - ilog)));
        end;
      string_cst:
	if str_val = ''
          then cval_code := 0
          else cval_code := length (str_val) + ord (str_val [1]);
      others:
        cval_code := ord (c)
    end (* case kind *);
  end (* with c^ *);
end (* cval_code *);
(*  CstCode returns an arbitrary integer representing a constant value.  This
    integer may be used in computing hash functions.  *)
function cst_code ( c: val ): int_type;
begin
  case c.kind of
    scalar_cst:
      cst_code := c.ival;
    real_cst,
    string_cst:
      cst_code := cval_code (c.valp);
    set_cst,
    array_cst,
    record_cst:
      cst_code := ord (c.valp);
    ptr_cst:
      cst_code := ord (nil);
    subr_cst:
      cst_code := c.blkp^.number;
    no_value:
      cst_code := 0
  end (* case c.kind *);
end (* cst_code *);
(*  The value function returns the "value" of an expression tuple T.  This
    is normally the result field of T.  However, if the result field of T
    points to an assignment tuple, then T is a variable whose value was set
    in an assignment statement, and the right-hand side of the assignment
    tuple is returned.  If that right-hand side tuple is marked as a killed
    tuple, it indicates that it is a reference expression whose value is no
    longer valid, and the tuple is marked as a copy tuple, meaning that a
    temporary must be allocated for it.
    Before the result field of T is returned as the value of T, it is checked
    to see if it is in a "mod" context.  If so, then T is returned, instead of
    its result field.  This check is for cases such as "x := x + 1", where
    without this special test, the lhs x would be returned as the value of the
    rhs x (because left-hand sides are emitted before right-hand sides).  *)
function value ( t: tuple ): tuple;
begin
  if t = nil then
    value := nil
  else with t^ do begin
    assert (result <> nil);
    if result^.opcode = assign_op then begin
      value := result^.rhs;
      if value^.killed_tuple then
        value^.copy_tuple := true;
    end
    else if result^.context = modx then
      value := t
    else
      value := result;
  end (* with t^ *);
end (* value *);
(*  The simple types are distinguished from the aggregate types.  All types
    except strings, arrays, records and sets are simple.  In general, opti-
    mization is only applied to expressions ith simple types.  Non-simple
    types are not subject to value propagation via assignment or to common
    subexpression elimination.  *)
type
    type_kind_set = set of type_kind;
const
    simple_types: type_kind_set =
      [ bools, ints, chars, scalars, reals, pointers, files, procs, funcs ];
(*  The common subexpression elimination algorithm is based on a hash table of
    lists of tuples.  Each unique tuple in the intermediate form is assigned a
    value number (which is stored in its RefFre field).  The hash index of a
    tuple is computed from its opcode, and from the value numbers of its oper-
    ands.  Within one of the hash table lists, tuples are ordered from highest
    to lowest tuple id numbers.  For basic block reduction, only tuples from a
    single basic block will be found in a hash list.  For global reduction,
    tuples from an entire dominator chain of basic blocks may be found in a
    hash list.  Because of the intermediate form ordering, if block A dominates
    block B, then all tuples in block A will have lower tuple id numbers than
    any tuples in block B.
    The CseData structure also contains a list of variable modifications and
    a list of i/o operations for the current chain.  A variable modification
    is described by the modifying tuple, the modified tuple, and (for an
    assignment) the new-value tuple.  An i/o operation is described by the
    i/o operation tuple and the file operand tuple.  *)
const
    cse_hash_size = 223;
    cse_hash_limit = 222; (* = cse_hash_size - 1 *)
type
    cse_hash_index = 0 .. cse_hash_limit;
    cse_mod_list = ^ cse_mod_node;
    cse_mod_node = packed record
        mod_tpl: tuple; (* The modifying tuple *)
        target: tuple; (* The modified or file argument tuple *)
        new_val: tuple; (* For an assignment, the right_hand side tuple *)
        next: cse_mod_list (* The rest of the list *)
    end;
    cse_data = record
        table: array [cse_hash_index] of tpl_list; (* The hash table *)
        mod_list: cse_mod_list; (* Modifications *)
        io_list: cse_mod_list; (* I/o operations *)
        val_number: fre (* The last assigned value number *)
    end;
(*  CseInit initializes a CseData structure, by setting all the pointers to Nil.  *)
procedure cse_init ( var c: cse_data );
var i: cse_hash_index;
begin
  with c do begin
    for i := minimum (i) to maximum (i) do
      table [i] := nil;
    mod_list := nil;
    io_list := nil;
    val_number := 0;
  end;
end (* cse_init *);
(*  CseClear is called with a CseData structure and a basic block id number.
    It removes from the lists in the data structure all entries which point
    to tuples in basic blocks which follow the specified basic block in the
    intermediate form.  If the basic block number is zero, then the lists
    will be cleared completely.  *)
procedure cse_clear ( var c: cse_data; block: index_range );
var
    i: cse_hash_index;
    tnode: tpl_list;
    cnode: cse_mod_list;
    limit: id_range;
begin
  if block = t_chain^.last_label^.block_order_no then
    return;
  if block = 0
    then limit := 0
    else limit := block_labels^[block+1]^.nodeid;
  with c do begin
    for i := minimum (i) to maximum (i) do
      while (table [i] <> nil) andif (table[i]^.tpl^.nodeid > limit) do begin
        tnode := table[i]^.next;
        dispose (table [i]);
        table [i] := tnode;
      end;
    while (mod_list <> nil) andif (mod_list^.mod_tpl^.nodeid > limit) do begin
      cnode := mod_list^.next;
      dispose (mod_list);
      mod_list := cnode;
    end;
    while (io_list <> nil) andif (io_list^.mod_tpl^.nodeid > limit) do begin
      cnode := io_list^.next;
      dispose (io_list);
      io_list := cnode;
    end;
  end (* with c *);
end (* cse_clear *);
(*  NewValue creates a new value number, and assigns it to a specified tuple,
    which it inserts at a specified index in the hash table.  *)
procedure new_value ( var c: cse_data; t: tuple; index: cse_hash_index );
var entry: tpl_list;
begin
  new (entry);
  entry^.tpl := t;
  entry^.next := c.table [index];
  c.table [index] := entry;
  t^.result := t;
  c.val_number := c.val_number + 1;
  t^.ref_fre := c.val_number;
end (* new_value *);
(*  NewValNum is the same as NewValue, except that it does not add the tuple
    to the hash table.  NewValNum is used for tuples such as RandomOps, which
    can never be common.  *)
procedure new_val_num (var c: cse_data; t: tuple );
begin
  t^.result := t;
  c.val_number := c.val_number + 1;
  t^.ref_fre := c.val_number;
end (* new_val_num *);
(*  ChangeEntry ensures that there is a hash table entry whose Tpl is T.
    If there is a matching entry already in the current block, then its
    Tpl field is changed to T; otherwise, one is created.  This is used
    when an assignment to (or modification of) tuple T is processed.  *)
procedure change_entry ( var c: cse_data; t: tuple; block: tuple );
var
    index: cse_hash_index;
    scan: tpl_list;
begin
  with t^ do begin
    case opcode of
      ident_ref:
        index := id_sym^.id_number mod cse_hash_size;
      ptr_ref:
        index := (base_ptr^.ref_fre * ord(opcode)) mod cse_hash_size;
      buffer_ref:
        index := (base_file^.ref_fre * ord(opcode)) mod cse_hash_size;
      field_ref:
        index := ((base_rec^.ref_fre + field_sym^.id_number) * ord(opcode)) mod cse_hash_size;
      array_ref:
        index := ((base_array^.ref_fre + index_val^.ref_fre) * ord(opcode)) mod cse_hash_size;
      substr_ref:
        if substr_length = nil then
          index := ((base_string^.ref_fre + substr_index^.ref_fre) * ord(opcode)) mod cse_hash_size
        else
          index := ((base_string^.ref_fre + substr_index^.ref_fre + substr_length^.ref_fre)
                    * ord(opcode)) mod cse_hash_size;
      upb_op: (* May be on lhs in NEW call. *)
        index := (operand[1]^.ref_fre * ord(opcode)) mod cse_hash_size;
      others:
        assert (false)
    end (* case opcode *);
  end (* with t^ *);
  scan := c.table [index];
  while (scan <> nil) andif
        (scan^.tpl^.nodeid > block^.nodeid) andif
        not tpl_match (t, scan^.tpl) do
    scan := scan^.next;
  if (scan <> nil) andif (scan^.tpl^.nodeid > block^.nodeid)
    then scan^.tpl := t
    else new_value (c, t, index);
end (* change_entry *);
(*  LookupTuple tests whether there is a previous tuple which matches a given
    tuple.  The parameters are T, the tuple to be matched; Index, its hash table
    index; and Limit, the largest node id number of any of its operands.  (Any
    tuple matching T must follow all of the operands of T in the intermediate
    form).  If a match is found, the Result and RefFre fields of T will be
    copied from the matching tuple, and Limit will be changed to the node id
    number of the matching tuple.  Otherwise, the Result field of T will be
    set to T, and its RefFre field will be set to a new value number.  *)
procedure lookup_tuple
                ( var c: cse_data; t: tuple; index: cse_hash_index; var limit: id_range );
var scan: tpl_list;
begin
  if (t^.desc.kind in simple_types) or
     ( (t^.desc.kind in [arrays, records]) and
       (t^.opcode <> agg_val) and
       (t^.opcode <> func_call_op) ) then begin
    scan := c.table [index];
    while (scan <> nil) andif (scan^.tpl^.nodeid > limit) do begin
      if tpl_match (t, scan^.tpl) then begin
        t^.result := scan^.tpl^.result;
        t^.ref_fre := scan^.tpl^.ref_fre;
        limit := scan^.tpl^.nodeid;
        return; (* <----  Exit with matching tuple found. *)
      end;
      scan := scan^.next;
    end;
  end;
  new_value (c, t, index); (* No matching tuple, add to table. *)
end (* lookup_tuple *);
(*  Modification scans the ModList for the current blocks to see whether there
    is an entry for a modification of a tuple which circumscribes tuple T.  If
    there is, Modification returns that ModList entry.  Otherwise, Modification
    returns nil.  Only ModList entries with NodeId numbers greater than Limit
    are considered.  *)
function modification ( var c: cse_data; t: tuple; limit: id_range ): cse_mod_list;
begin
  modification := c.mod_list;
  while (modification <> nil) andif
        (modification^.mod_tpl^.nodeid > limit) andif
        not circumscribes (modification^.target, t) do
    modification := modification^.next;
  if (modification <> nil) andif
     (modification^.mod_tpl^.nodeid <= limit) then
    modification := nil;
end (* modification *);
(*  CheckMod scans the ModTuples list for the current blocks to see whether
    there is an assignment to or a read into a tuple which circumscribes
    tuple T.  In general, such a modifying operation invalidates any previous
    tuple that T may have matched.  However, if the operation is an assign-
    ment, then there are two special cases:
        If the lhs of the assignment matches T, then it is a perfectly good
        match for T, and may be treated like any matched tuple.
        If the rhs of the assignment matches T, and T is not a field or a
        substring, then (due to the definition of circumscribes) either the
        lhs is T, and the assignment reduces to T:=T, or the lhs is not T.
        In either case, T is unaffected by the assignment.  To understand why
        this doesn't apply to fields and substrings, consider that in the
        assignment "a[j]:=a[i]", either a[i] and a[j] are the same element,
        or they are not; but in "substr(s,j,3):=substr(s,i,3)", the rhs may
        be invalidated if j = i-2, i-1, i+1, or i+2.  Similarly, fields in
        distinct variants of an undiscriminated union might overlap. *)
procedure check_mod ( var c: cse_data; t: tuple; index: cse_hash_index; limit: id_range );
var m: cse_mod_list;
begin
  m := modification (c, t, limit);
  if m <> nil then begin
    with m^ do begin
      if new_val <> nil then begin (* Assignment. *)
        if tpl_match (t, target) then begin
          t^.result := target^.result;
          t^.ref_fre := target^.ref_fre;
        end
        else if ((t^.opcode <> field_ref) and (t^.opcode <> substr_ref)) andif
          ( tpl_match (t, new_val) orif
            (tpl_match (t, mod_tpl^.prev) andif (value (mod_tpl^.prev) = new_val)) ) then begin
              t^.result := new_val;
              t^.ref_fre := new_val^.ref_fre;
        end
        else
          new_value (c, t, index)
      end
      else (* new_val = nil *) begin
        new_value (c, t, index);
      end;
    end (* with m^ *);
  end (* if m <> nil *);
end (* check_mod *);
(*  CheckIo scans the IoList to see whether there is an i/o operation on a
    file in the same file class as the Check tuple, and a NodeId number greater
    than Limit.  If so, the tuple T, which matched a previous tuple, has its
    Result field set to itself and its RefFre field set to a new value number.  *)
procedure check_io
                ( var c: cse_data; t, check: tuple; index: cse_hash_index; limit: id_range );
var scan: cse_mod_list;
    class: sym;
begin
  class := check^.desc.base^.file_class;
  scan := c.io_list;
  while (scan <> nil) andif (scan^.mod_tpl^.nodeid >= limit) do begin
    exit if scan^.target^.desc.base^.file_class = class do
      new_value (c, t, index);
    scan := scan^.next;
  end;
end (* check_io *);
(*  NoteMod will record the fact that tuple T modifies tuple Target.  If the
    new value of Target is known, it will be in NewVal; otherwise, NewVal will
    be nil.  *)
procedure note_mod ( var c: cse_data; t, target, new_val: tuple );
var entry: cse_mod_list;
begin
  new (entry);
  entry^.mod_tpl := t;
  entry^.target := target;
  entry^.new_val := new_val;
  entry^.next := c.mod_list;
  c.mod_list := entry;
end (* note_mod *);
(*  NoteIo records the fact the tuple T performs an i/o operation on file
    argument FileArg.  It is assumed that FileArg is really a file, and not
    an in_str_op or an out_str_op.  *)
procedure note_io ( var c: cse_data; t, file_arg: tuple );
var entry: cse_mod_list;
begin
  new (entry);
  entry^.mod_tpl := t;
  entry^.target := file_arg;
  entry^.new_val := nil;
  entry^.next := c.io_list;
  c.io_list := entry;
end (* note_io *);
(*  When an assignment of the form <reference1> := <reference2> is followed by
    an operation which invalidates <reference2>, and optimization replaces a
    subsequent reference to <reference1> by <reference2>, the initial tuple
    for <reference2> must be marked as a copy tuple, so that it will be eval-
    uated once and saved, rather than being loaded from memory whenever it is
    used.
    For example, in the sequence "x:=y; read(y); z:=x+1", the reference to
    x in the last assignment will be replaced by a reference to the original
    y tuple.  Therefore, that tuple must be marked as a copy tuple so that
    we save and retrieve the original value of y, instead of loading its new
    value.
    In general, any change to any part of the <reference2> expression invali-
    dates it.  For example, the expression "a[b[i]]" must be modified if there
    is any change to a, b, i, or to any expression which circumscribes a[b[i]]
    or b[i].
    We keep a list of reference expressions which have occurred on the right-
    hand sides of assignment statements.  With each expression, there is a
    set of all the variables which occur in it, and a list of all component
    subexpressions of the reference expression.  This RhsReference list is
    ordered from higher to lower tuple id numbers.  During basic block opti-
    mization, it will contain tuples from only a single basic block; during
    global optimization, it will contain tuples from an entire dominator
    chain.  When a tuple in the RhsReference list is invalidated, it is added
    to a KillStack list; this enables killed tuples to be unmarked when we
    back up the dominator tree during global optimization.  *)
type
    ref_desc = ^ ref_desc_node;
    ref_desc_node = packed record
        next: ref_desc; (* The link to maintain the chain. *)
        ref: expr; (* The reference expression described by this node. *)
        next_killed: ref_desc; (* Next in the stack of killed tuples. *)
        has_been_copied: boolean; (* True if this node may be ignored. *)
        comp_exprs: tpl_list; (* The component subexpressions of the expression. *)
        comp_vars: svector (* The component variables of the expression. *)
    end;
    rhs_ref_data = record
        references: ref_desc;
        killed_stack: ref_desc;
        listed_tuples: svector; (* Tuples which are in the list. *)
        test_vars: svector (* Used in KillModReferences. *)
    end;
(*  RhsRefInit initializes an RhsRef data structure to nil by setting the
    reference and killed stack lists to Nil.  *)
procedure rhs_ref_init ( var rhs_ref: rhs_ref_data );
begin
  rhs_ref.references := nil;
  rhs_ref.killed_stack := nil;
  rhs_ref.listed_tuples := new_svector (0, t_chain^.final_tuple^.nodeid);
  clr_set (rhs_ref.listed_tuples, 0);
  rhs_ref.test_vars := new_svector (1, sym_vl_number);
end;
(*  RhsRefClear is called with an RhsRefData structure and pointers to entries
    in the KilledStack and References lists of the structure.  Any entries in
    the References list following the specified entry are removed.  Any entries
    in the KilledStack list following the specified entry are moved back to the
    References list.  If the corresponding tuples have been marked as copy tuples,
    then the list entries are marked.  *)
procedure rhs_ref_clear ( var rhs_ref: rhs_ref_data; old_ref, old_kill: ref_desc );
var
    this: ref_desc;
    comp, next_comp: tpl_list;
begin
  with rhs_ref do begin
    (*  Transfer nodes from the killed stack back to the references list.  *)
    while killed_stack <> old_kill do begin
      with killed_stack^ do begin
        if not ref^.copy_tuple then begin
          has_been_copied := false;
          ref^.killed_tuple := false;
        end;
        killed_stack := next_killed;
      end;
    end;
    (*  Delete nodes from the top of the reference list.  *)
    while references <> old_ref do begin
      this := references;
      with this^ do begin
        references := next;
        del_svector (comp_vars);
        comp := comp_exprs;
        while comp <> nil do begin
          next_comp := comp^.next;
          dispose (comp);
          comp := next_comp;
        end;
        del_elem (listed_tuples, 0, ref^.nodeid);
      end;
      dispose (this);
    end;
  end (* with rhs_ref *);
end (* rhs_ref_clear *);
(*  NoteUse is called with a reference expression tuple which has been used as
    the rhs in an assignment tuple.  It adds the tuple to the RhsReferences
    list.  *)
procedure note_use ( var rhs_ref: rhs_ref_data; t: tuple );
var entry: ref_desc;
(*  RecordComponents is called recursively to record all the component variables
    and subexpressions of a reference expression.  Variables are added to the
    CompVars set; subexpressions are added to the CompExprs tuple list.  Both of
    these are components of Entry, a RefDesc which is a local variable of NoteUse.  *)
procedure record_components ( t: tuple );
var component: tpl_list;
begin
  if not assignable (t) then
    return;
  with t^ do begin
    case opcode of
      ident_ref:
        begin
          add_elem (entry^.comp_vars, 0, id_sym^.id_number);
          component := nil;
        end;
      ptr_ref:
        begin
          record_components (base_ptr);
          add_elem (entry^.comp_vars, 0, base_ptr^.desc.base^.heap_class^.id_number);
          component := nil;
        end;
      buffer_ref:
        begin
          record_components (base_file);
          add_elem (entry^.comp_vars, 0, base_file^.desc.base^.file_class^.id_number);
          component := nil;
        end;
      array_ref:
        begin
          record_components (base_array);
          record_components (index_val);
          new (component);
        end;
      field_ref:
        begin
          record_components (base_rec);
          new (component);
        end;
      substr_ref:
        begin
          record_components (base_string);
          record_components (substr_index);
          if substr_length <> nil then
            record_components (substr_length);
          new (component);
        end;
      others:
        component := nil
    end (* case opcode *);
  end (* with t^ *);
  if component <> nil then begin
    component^.tpl := t;
    component^.next := entry^.comp_exprs;
    entry^.comp_exprs := component;
  end;
end (* record_components *);
var k: ref_desc;
begin
  (*  If the tuple T is already in the references list, we've nothing to do.  *)
  if in_set (rhs_ref.listed_tuples, 0, t^.nodeid) then
    return;
  (*  Otherwise, create an node and add it at the head of the list.  *)
  new (entry);
  with entry^ do begin
    next := rhs_ref.references;
    ref := t;
    has_been_copied := false;
    comp_vars := new_svector (0, sym_vl_number);
    clr_set (comp_vars, 0);
    comp_exprs := nil;
  end;
  rhs_ref.references := entry;
  (*  Now add all the component variables and subexpressions of the reference
      to the CompVars set and CompExprs list.  *)
  record_components (t);
  add_elem (rhs_ref.listed_tuples, 0, t^.nodeid);
end (* note_use *);
(*  KillReference may be called with either (a) a tuple whose value has just
    been changed, because it is the lhs reference from an assignment, the
    target of a read, or the string destination of a putstring; or (b) a
    variable symbol id number, which is the id number of the file class of
    a file which has just been used in an i/o operation.  It marks as "killed"
    any reference tuples on the RhsReferences list which are invalidated by
    the change to the reference expression / file class.  *)
procedure kill_reference ( var rhs_ref: rhs_ref_data; reft: tuple; class: id_range );
var
    base_expr: tuple;
    base_id: id_range;
    symbolic: boolean;
    ref_node: ref_desc;
    comp: tpl_list;
begin
  (*  Set BaseId to the symbol number of the base variable of the killed reference.  *)
  if reft = nil then begin
    symbolic := true;
    base_id := class;
  end
  else begin
    if reft^.opcode = upb_op then return;       (* <---- Exit of setting flex upb for "new". *)
    symbolic := (reft^.opcode in [ident_ref, ptr_ref, buffer_ref]);
    base_expr := reft;
    base_id := 0;
    while base_id = 0 do begin
      with base_expr^ do begin
        case opcode of
          ident_ref: base_id := id_sym^.id_number;
          ptr_ref: base_id := base_ptr^.desc.base^.heap_class^.id_number;
          buffer_ref: base_id := base_file^.desc.base^.file_class^.id_number;
          field_ref: base_expr := base_rec;
          array_ref: base_expr := base_array;
          substr_ref: base_expr := base_string
        end;
      end;
    end (* while base_id = 0 *);
  end (* if reft <> nil *);
  (*  Now scan the References list, looking for ones which have this variable as
      a component variable.  *)
  ref_node := rhs_ref.references;
  while ref_node <> nil do begin
    with ref_node^ do begin
      if not has_been_copied andif in_set (comp_vars, 0, base_id) then begin
        (*  The reference is killed if Reft directly invalidates a component variable,
            or circumscribes a component subexpression.  *)
        if symbolic then
          ref^.killed_tuple := true
        else begin
          comp := comp_exprs;
          while (comp <> nil) andif not circumscribes (reft, comp^.tpl) do
            comp := comp^.next;
          ref^.killed_tuple := (comp <> nil);
        end;
        (*  If the reference is killed, add it to the KilledStack.  *)
        if ref^.killed_tuple then begin
          has_been_copied := true;
          next_killed := rhs_ref.killed_stack;
          rhs_ref.killed_stack := ref_node;
        end;
      end;
      ref_node := next;
    end (* with ref_node^ *);
  end (* while ref_node <> nil *);
end (* kill_reference *);
(*  KillModReferences marks as "killed" any reference tuples on the RhsReferences
    list which have component variables which are in a specified set.  *)
procedure kill_mod_references ( var rhs_ref: rhs_ref_data; mod_set: svector );
var ref_node: ref_desc;
begin
  mov_set (mod_set, 0, rhs_ref.test_vars, 1);
  ref_node := rhs_ref.references;
  while ref_node <> nil do begin
    with ref_node^ do begin
      mov_set (comp_vars, 0, rhs_ref.test_vars, 0);
      intersect (rhs_ref.test_vars, 1, 0);
      if not has_been_copied andif not is_empty (rhs_ref.test_vars, 0) then begin
        ref^.killed_tuple := true;
        has_been_copied := true;
        next_killed := rhs_ref.killed_stack;
        rhs_ref.killed_stack := ref_node;
      end;
      ref_node := next;
    end;
  end;
end (* kill_mod_references *);
(*  KillIdef marks as "killed" any reference tuples on the RhsReference list
    which are invalidated by formal reference expressions in the Idef set of
    a specified block.  *)
procedure kill_idef ( var rhs_ref: rhs_ref_data; block: index_range );
  function fcirc ( f: fre; t: tuple ): boolean;
  begin
    with fre_table^[f] do begin
      case t^.opcode of
        ident_ref:   fcirc := (kind = sym_fre) andif (id_sym = t^.id_sym);
        ptr_ref:     fcirc := (kind = sym_fre) andif (id_sym = t^.desc.base^.heap_class);
        buffer_ref:  fcirc := (kind = sym_fre) andif (id_sym = t^.desc.base^.file_class);
        array_ref:   fcirc := (kind = elem_fre) andif fcirc (base_fre, t^.base_array);
        substr_ref:  fcirc := (kind = elem_fre) andif fcirc (base_fre, t^.base_string);
        field_ref:   fcirc := (kind = field_fre) andif fcirc (record_fre, t^.base_rec) andif
                                 circ_fields (field_sym, t^.field_sym)
      end;
    end;
  end (* fcirc *);
var
    f, base_f: fre;
    base_id: id_range;
    ref_node: ref_desc;
    comp: tpl_list;
label 100 (* next fre *);
begin
  for f := 1 to n_fre do begin
    if in_set (idef, block, f) then begin
      base_f := f;
      while fre_table^[base_f].kind <> sym_fre do begin
        with fre_table^[base_f] do begin
          if kind = elem_fre
            then base_f := base_fre
            else base_f := record_fre;
          if in_set (idef, block, base_f) then
            goto 100; (* <---- Base ref in IDEF, already processed. *)
        end (* with *);
      end (* while *);
      base_id := fre_table^[base_f].id_sym^.id_number;
      ref_node := rhs_ref.references;
      while ref_node <> nil do begin
        with ref_node^ do begin
          if not has_been_copied andif in_set (comp_vars, 0, base_id) then begin
            if fre_table^[f].kind = sym_fre then
              ref^.killed_tuple := true
            else begin
              comp := comp_exprs;
              while (comp <> nil) andif not fcirc (f, comp^.tpl) do
                comp := comp^.next;
              ref^.killed_tuple := (comp <> nil);
            end;
            if ref^.killed_tuple then begin
              has_been_copied := true;
              next_killed := rhs_ref.killed_stack;
              rhs_ref.killed_stack := ref_node;
            end;
          end;
          ref_node := next;
        end (* with ref_node^ *);
      end (* while ref_node <> nil *);
    end (* if in_set *);
100 (* next fre *):
  end (* for f *);
end (* kill_idef *);
(*  ChkOverlap is a function which takes the left and right hand side
    expressions from an assignment, and determines whether a temporary
    location is necessary to perform the assignment safely.  An overlap
    is only indicated for string, set and aggregate assignments.  *)
function chk_overlap (left, right: expr ): boolean;
(*  MayBeUndiscriminated takes two non-simple references, and tests whether
    there is a possibility that the two references could be to fields (or
    components) in distinct variants of an undiscriminated union in the same
    record.  *)
function may_be_undiscriminated ( ref1, ref2: expr ): boolean;
const ref_stk_size = 20;
type ref_stack = record
        top: 0 .. ref_stk_size;
        stack: array [1..ref_stk_size] of expr
     end;
(*  StackRef creates a stack containing the nested references making up a
    fully qualified reference, down to the first occurrence of a "primitive"
    reference--a constant, ientifier, pointer or buffer reference.  *)
procedure stack_ref ( ref: expr; var stack: ref_stack; var overflow: boolean );
var loop_status: ( stacking, stack_full, finished );
    r: expr;
begin
  r := ref;
  stack.top := 0;
  loop_status := stacking;
  while loop_status = stacking do begin
    if stack.top = ref_stk_size then
      loop_status := stack_full
    else begin
      stack.top := stack.top + 1;
      stack.stack[stack.top] := r;
      with r^ do begin
        case opcode of
          cst_ref, ident_ref, ptr_ref, buffer_ref:
            loop_status := finished;
          field_ref:
            r := base_rec;
          array_ref:
            r := base_array
        end;
      end;
    end;
  end (* while loop_status = stacking *);
  overflow := (loop_status = stack_full);
end (* stack_ref *);
var stk1, stk2: ref_stack;
    r1, r2: expr;
    loop_status: ( comparing, undiscriminated, ok );
    ofl1, ofl2: boolean;
    base_match: boolean;
begin
  stack_ref (ref1, stk1, ofl1);
  stack_ref (ref2, stk2, ofl2);
  if ofl1 or ofl2 then begin (* Expression too complex, assume the worst. *)
    may_be_undiscriminated := true;
    return;
  end;
  
  r1 := stk1.stack[stk1.top];
  stk1.top := stk1.top - 1;
  r2 := stk2.stack[stk2.top];
  stk2.top := stk2.top - 1;
  if r1^.opcode <> r2^.opcode then
    base_match := false
  else begin
    case r1^.opcode of
      ident_ref:
        base_match := (r1^.id_sym = r2^.id_sym);
      ptr_ref:
        base_match := (r1^.base_ptr^.desc.base^.heap_class = r2^.base_ptr^.desc.base^.heap_class);
      buffer_ref:
        base_match := (r1^.base_file^.desc.base^.file_class = r2^.base_file^.desc.base^.file_class)
    end;
  end;
  if base_match
    then loop_status := comparing (* Something in common. *)
    else loop_status := ok; (* Nothing in common. *)
  while loop_status = comparing do begin
    if (stk1.top = 0) or (stk2.top = 0) then
      loop_status := ok (* Match failed. *)
    else begin
      r1 := stk1.stack[stk1.top];
      stk1.top := stk1.top - 1;
      r2 := stk2.stack[stk2.top];
      stk2.top := stk2.top - 1;
      if r1^.opcode <> r2^.opcode then
        loop_status := ok (* Match failed. *)
      else if r1^.opcode = field_ref then begin
        if r1^.field_sym = r2^.field_sym then
          (* matches so far, keep comparing *)
        else if circ_fields (r1^.field_sym, r2^.field_sym) then
          loop_status := undiscriminated
        else
          loop_status := ok; (* Distinct or discriminated fields. *)
      end
      else
        (* array ref's, keep comparing *);
    end (* if neither stack is empty *);
  end (* while loop_status = comparing *);
  may_be_undiscriminated := (loop_status = undiscriminated);
end (* may_be_undiscriminated *);
(*  ModOrUses is called with a function call tuple and a reference expression
    tuple.  It checks whether the function call can use or modify the reference
    expression.  *)
function mod_or_uses ( call, ref: expr ): boolean;
var ref_base: expr;
    ref_number: id_range;
begin
  (*  Find the base reference symbol id number for the reference expression.  *)
  ref_base := ref;
  ref_number := 0;
  while ref_number = 0 do begin
    with ref_base^ do begin
      case opcode of
        ident_ref:   ref_number := id_sym^.id_number;
        ptr_ref:     ref_number := base_ptr^.desc.base^.heap_class^.id_number;
        buffer_ref:  ref_number := base_file^.desc.base^.file_class^.id_number;
        field_ref:   ref_base := base_rec;
        array_ref:   ref_base := base_array;
        substr_ref:  ref_base := base_string
      end;
    end;
  end (* while ref_number = 0 *);
  effects (call);
  mod_or_uses := in_set (smod, 0, ref_number) or in_set (suse, 0, ref_number);
end (* mod_or_uses *);
(*  StrOverlaps is called with the left and right hand side expressions of a
    string assignment, and determines whether the assignment must be performed
    using a temporary.  A temporary is necessary if some part of the lhs
    string appears as part of the rhs string, except at the very beginning,
    or if the rhs string contains a function call which can use or modify the
    lhs string value.  *)
function str_overlaps ( l, r: expr ): boolean;
    function str_ovlay ( l, r: expr ): boolean;
      forward;
(*  StrOvl is the base function for the computation of StrOverlaps.  It also
    provides the base for the helper function StrOvlay.  StrOvl checks whether
    two string expressions "overlap" or are "overlaid" in some respect.  It is
    parameterized with a function OvlPrim which determines its value when none
    of its other rules apply.
    StrOvl (l, r, OvlPrim) ==
        r = func_call (...)       => r Uses l or r Modifies l
        r = substr (r', ...)      => StrOvl (l, r', OvlPrim)
        r = uppercase (r')        => StrOvl (l, r', OvlPrim)
        r = lowercase (r')        => StrOvl (l, r', OvlPrim)
        r = string_convert (r')   => StrOvl (l, r', OvlPrim)
        l = substr (l', 1, ...)   => StrOvl (l', r, OvlPrim)
        r = r1 || ... || rn       => StrOvl (l, r1, OvlPrim) or
                                     StrOvlay (l, r2) ... or StrOvlay (l, rn)
        l = substr (l', ...)      => StrOvlay (l', r)
        else                      => OvlPrim (l, r)                             *)
type ovl_fn = function ( expr; expr ): boolean;
function str_ovl ( l, r: expr; ovl_prim: ovl_fn ): boolean;
var l1, r1: expr;
    i: oper_range;
label 100;
begin
  l1 := l;
  r1 := r;
100:
  with r1^ do begin
    if opcode = func_call_op then begin
      str_ovl := mod_or_uses (r1, l1);
      return;
    end;
    if opcode = substr_ref then begin
      r1 := base_string;
      goto 100;
    end;
    if (opcode = lwc_op) or (opcode = upc_op) or (opcode = strcvt_op) then begin
      r1 := operand[1];
      goto 100;
    end;
  end;
  with l1^ do begin
    if opcode = substr_ref then begin
      if (substr_index^.opcode = cst_ref) andif
         (substr_index^.desc.kind = ints) andif
         (substr_index^.cst_val.ival = 1) then begin
        l1 := base_string;
        goto 100;
      end;
    end;
  end;
  with r1^ do begin
    if opcode = cat_op then begin
      for i := 2 to upperbound (operand) do begin
        str_ovl := str_ovlay (l1, operand[i]);
        if str_ovl then
          return;
      end;
      r1 := operand[1];
      goto 100;
    end;
  end;
  if l1^.opcode = substr_ref
    then str_ovl := str_ovlay (l1^.base_string, r1)
    else str_ovl := ovl_prim (l1, r1);
end (* str_ovl *);
(*  StrOvlay is a helper function for StrOverlaps.  It is essentially the same
    as StrOverlaps, except that two strings are said to be "overlaid" if they
    can have anything in common, while they do not "overlap" unless the common
    portion comes at the head of the lhs string.  *)
function str_ovlay (* l, r: expr ): boolean *);
begin
  str_ovlay := str_ovl (l, r, circumscribes);
end;
begin
  str_overlaps := str_ovl (l, r, may_be_undiscriminated);
end;
(*  SetAggOverlaps is called with the left and right hand side expressions of
    a set or aggregate assignment, and determines whether the assignment must
    be performed using a temporary.  A temporary is necessary if the lhs
    expression, or some component of it, occurs in the rhs expression, or if
    the rhs expression contains a function call which can use or modify the
    lhs expression value.
    SetAggOverlaps (l, r) ==
        r = func_call (...)  => r Uses l or r Modifies l
        r = gen_set (...)    => false
        r = set_cvt (r')     => SetOverlaps (l, r')
        r = r1 + ... + rn    => Circumscribes (r, l) ... or
                                Circumscribes (rn, l)
        r = r1 - r2          => Circumscribes (r, l) or Circumscribes (r2, l)
        r = r1 * r2          => Circumscribes (r, l) or Circumscribes (r2, l)
        r = (r1, ..., rn)    => true
        else                 => MayBeUndiscriminated (l, r)                     *)
function set_agg_overlaps ( l, r: expr ): boolean;
var i: oper_range;
begin
  with r^ do begin
    case opcode of
      func_call_op:
        set_agg_overlaps := mod_or_uses (r, l);
      gen_set_op:
       set_agg_overlaps := false;
      setcvt_op:
        set_agg_overlaps := set_agg_overlaps (l, operand[1]);
      union_op, both_op, diff_op:
        begin
          for i := 1 to upperbound (operand) do begin
            set_agg_overlaps := circumscribes (l, operand[i]);
          exit if set_agg_overlaps;
          end;
        end;
      agg_val:
        set_agg_overlaps := true;
      others:
        set_agg_overlaps := may_be_undiscriminated (l, r)
    end;
  end;
end (* set_agg_overlaps *);
begin
  case left^.desc.kind of
    strings:
      chk_overlap := str_overlaps (left, right);
    sets, arrays, records:
      chk_overlap := set_agg_overlaps (left, right);
    others:
      chk_overlap := false
  end;
end (* chk_overlap *);
(*  ChkRecursion is called with an assignment tuple.  It sets the Lrecursive and
    Rrecursive flags in the tuple if the first or second operands of the
    right-hand side expression match the left-hand side expression.  *)
procedure chk_recursion ( a: tuple );
var r: expr;
begin
  with a^ do begin
    r := rhs;
    while r^.opcode in [setcvt_op, strcvt_op, sclcvt_op] do
      r := r^.operand[1];
    case r^.opcode of
      ineg_op,
      rneg_op,
      bnot_op,
      iabs_op,
      rabs_op,
      cat_op:
        if tpl_match (lhs, r^.operand[1]) then
          lrecursive := true;
      iadd_op,
      radd_op,
      isub_op,
      rsub_op,
      imul_op,
      rmul_op,
      and_op,
      or_op,
      union_op,
      both_op,
      diff_op:
        if tpl_match (lhs, r^.operand[1]) then
          lrecursive := true
        else if tpl_match (lhs, r^.operand[2]) then
          rrecursive := true;
      idiv_op,
      rdiv_op:
        if tpl_match (lhs, r^.operand[2]) then
          rrecursive := true;
      substr_ref:
        if tpl_match (lhs, r^.base_string) andif
           (r^.substr_index^.opcode = cst_ref) andif
           (r^.substr_index^.cst_val.ival = 1) then
          lrecursive := true;
      others:
        (* not recursive *)
    end (* case r^.opcode *);
  end (* with a^ *);
end (* chk_recursion *);
(*  Reduce is the central optimization routine.  It may be called by either
    ReduceBlocks or ReduceGlobal, to apply common subexpression elimination
    and value propagation to a basic block.  It takes a CseData structure,
    an RhsRefData structure, a variable set vector, and a flag as parameters.
    The flag indicates whether basic block or global optimization is to be
    performed.
    The VarSet parameter contains several interesting sets of variable symbols.
    X is an "active" variable if an identifier reference for it has been seen
    more recently than a subroutine call which could kill its value.  X is a
    "used" variable if a subroutine call which can use its value has occurred
    more recently than the most recent assignment to it.  For basic block opti-
    mization only, X "may be an input" variable if it has never been assigned
    to in the current basic block, nor has a subroutine call ever been encoun-
    tered which could kill its value.
    PotentialInputs is a set of IF tuples, where T is in PotentialInputs if
    T is a tuple which might be a block input tuple, even though it may not
    have been seen in a value context yet.  *)
const
    active = 1;
    used = 2;
    may_be_input = 3;
    basic_var_sets = may_be_input;
    global_var_sets = used;
procedure reduce ( basic_block: tuple; (* The basic block label node. *)
                   var cse: cse_data;
                   var rhs_ref: rhs_ref_data;
                   var cur_text_file: tuple;
                   var_set: svector;
                   potential_inputs: svector;
                   global: boolean );
(*  CheckPrevious is called with the result field of a tuple which has appeared
    as the left-hand side of an assignment or as the target of a read.  If this
    result field is an assignment tuple, then it is a redundant assignment, and
    may be replaced with an eval operation, as long as its left-hand side is not
    an abnormal variable and is not used in a subsequent subroutine call.  *)
procedure check_previous ( t: tuple );
var
    lhs_var: expr;
    lhs_sym: sym;
begin
  with t^ do
      if (opcode = assign_op) andif (not must_store) then begin
      lhs_var := lhs;
      lhs_sym := nil;
      while lhs_sym = nil do begin
        with lhs_var^ do begin
          case opcode of
            ident_ref: lhs_sym := id_sym;
            ptr_ref: lhs_sym := base_ptr^.desc.base^.heap_class;
            buffer_ref: lhs_sym := base_file^.desc.base^.file_class;
            field_ref: lhs_var := base_rec;
            array_ref: lhs_var := base_array;
            substr_ref: lhs_var := base_string
          end;
        end;
      end (* while lhs_sym = nil *);
      if not (lhs_sym^.abnormal_use or in_set (var_set, used, lhs_sym^.id_number)) then begin
        opcode := eval_op; (* Change the assignment to evaluation. *)
        lhs := nil;
      end;
    end (* opcode = assign_op *);
end (* check_previous *);
(*  FilePrevious considers a file argument.  If it is a text file or an i/o
    string argument, and if it must be the same as the last file argument
    in an operation that remembers file arguments, then FilePrevious returns
    true; otherwise, it returns false.  In any case, the IoOpsym pseudo-
    symbol is marked as active, and the global parameter CurTextFile is set
    to the file argument (to be remembered for next time).  *)
function file_previous ( file_arg: tuple ): boolean;
begin
  if (file_arg^.opcode = in_str_op) orif (file_arg^.opcode = out_str_op) orif
     (file_arg^.desc.base^.file_kind = textfile) then begin
    if in_set (var_set, active, io_opsym^.id_number) andif
       (cur_text_file = file_arg) then
      file_previous := true
    else begin
      file_previous := false;
      cur_text_file := file_arg;
      add_elem (var_set, active, io_opsym^.id_number);
    end;
  end
  else
    file_previous := false;
end (* file_previous *);
(*  VarSym is a function which takes a reference expression tuple.  If the
    tuple is an identifier, pointer, or buffer reference, then VarSym returns
    true, with the symbol, heap class, or file class id number in the Id parameter.
    Otherwise, VarSym returns false.  *)
function var_sym ( t: tuple; var id: id_range ): boolean;
begin
  with t^ do begin
    case opcode of
      ident_ref:
        if id_sym^.kind = vars then begin
          id := id_sym^.id_number;
          var_sym := true;
        end
        else
          var_sym := false;
      ptr_ref:
        begin
          id := base_ptr^.desc.base^.heap_class^.id_number;
          var_sym := true;
        end;
      buffer_ref:
        begin
          id := base_file^.desc.base^.file_class^.id_number;
          var_sym := true;
        end;
      others:
        var_sym := false
    end;
  end (* with t^ *);
end (* var_sym *);
(*  Redefine is called with a reference expression which has occurred as an
    operand of a tuple which will change its value.  Redefine performs many
    of the common bookkeeping actions which are required in this case.  *)
procedure redefine ( t: expr );
var ind: id_range;
begin
  change_entry (cse, t, basic_block);   (* The tuple is a brand-new common
                                           subexpression, all by itself. *)
  kill_reference (rhs_ref, t, 0);       (* If this invalidates any previous
                                           right-hand side expressions, we
                                           want to make a note of the fact. *)
  check_previous (t^.result);           (* If a prior assignment to the tuple
                                           has just become redundant, then go
                                           back and change it to an eval. *)
  if var_sym (t, ind) then begin        (* If the tuple is a real or pseudo
                                           symbol reference, then update the
                                           symbol vectors. *)
    add_elem (var_set, active, ind);
    del_elem (var_set, used, ind);
    if not global then
      del_elem (var_set, may_be_input, ind);
  end;
end (* redefine *);
(*  IoOperation is called with a tuple FileArg which has occurred as the file
    argument to an i/o operation T.  IoOperation performs bookkeeping actions
    which are required in this case.  *)
procedure io_operation ( t, file_arg: tuple );
var class: id_range;
begin
  if (file_arg^.opcode <> in_str_op) andif (file_arg^.opcode <> out_str_op) then begin
    class := file_arg^.desc.base^.file_class^.id_number;
    note_io (cse, t, file_arg);                 (* Note the i/o operation for
                                                   future reference. *)
    kill_reference (rhs_ref, nil, class);       (* Note that this invalidates any
                                                   previous right_hand side references
                                                   to buffers with this file class. *)
    if not global then                          (* No buffer reference of this file
                                                   class can be a block input tuple
                                                   hereafter, since this operation
                                                   might have changed it. *)
      del_elem (var_set, may_be_input, class);
  end;
end (* io_operation *);
(*  MarkBaseInputTuples is called with a base array, string, or record tuple
    which has been determined to be a block input tuple.  It and all of its
    base tuples are marked as block input tuples; yea, even unto the n-th
    generation.  *)
procedure mark_base_input_tuples ( t: tuple );
var t1: expr;
begin
  t1 := t;
  while (t1 <> nil) andif not t1^.blk_input_tuple do
    with t1^ do begin
      blk_input_tuple := true;
      case opcode of
        array_ref: t1 := base_array;
        field_ref: t1 := base_rec;
        substr_ref: t1 := base_string;
        others: t1 := nil
      end;
    end;
end (* mark_base_input_tuples *);
var
    t: tuple; (* The block scanning variable. *)
    t_next: tuple; (* The next tuple in the chain. *)
    t_temp: tuple; (* just a working temporary *)
    ind: int_type; (* Operand index variable. *)
    match_limit: fre; (* The value number limit for a backward scan. *)
    cur_source: source_id; (* The current statement loc, for Fold errors. *)
    deletion: boolean; (* Set true when a tuple is dechained. *)
    index: cse_hash_index; (* Temporary for hash index computations. *)
    op_sum: int_type; (* The sum of the operand value numbers. *)
    t_folded: tuple; (* Temporary for folding expressions. *)
    tref: ref_desc; (* Temporary used in disposing of ref lists. *)
begin
  t := basic_block^.next;
  while (t^.opcode <> label_node) and (t^.opcode <> end_block) do begin
    deletion := false;
    with t^ do begin
      t_next := next;
      case opcode of
        start_stmt:
          cur_source := stmt_source;
        (*****  Simple Action Tuples  *****)
        start_with:
          with_rec := value (with_rec);
        jump_op,
        retjump_op,
        jump_t_op,
        jump_f_op,
        jump_in_op,
        gen_jump_op,
        case_jump_op:
          cond := value (cond);
        goto_op:
          target_frame := value (target_frame);
        dispose_op:
          dptrarg := value (dptrarg);
        eval_op:
          rhs := value (rhs);
        (*****  Assignment  *****)
        assign_op:
          begin
            rhs := value (rhs);
            if lhs^.result = rhs^.result then begin
              dechain (t); (* Delete redundant assignment. *)
              deletion := true; (* Flag it for later. *)
            end
            else begin
              overlaps := chk_overlap (lhs, rhs);
              chk_recursion (t);
              redefine (lhs); (* Update the data structures. *)
              if lhs^.desc.kind in simple_types
                then lhs^.result := t (* Assignment could be redundant - remember it. *)
                else lhs^.result := lhs; (* Aggregate - don't record the value. *)
              if (rhs^.desc.kind in simple_types) and assignable (rhs) then
                note_use (rhs_ref, rhs);
              note_mod (cse, t, lhs, rhs);
            end (* if lhs^.result <> rhs^.result *);
          end;
        read_op:
          begin
            rw_file := value (rw_file);
            if not rw_old_file then
              rw_old_file := file_previous (rw_file);
            rw_width := value (rw_width);
            rw_precision := value (rw_precision);
            redefine (rw_item); (* Update the data structures. *)
            rw_item^.result := rw_item;
            note_mod (cse, t, rw_item, nil);
            io_operation (t, rw_file);
          end;
        (*****  Input/Output Operators  *****)
        start_io_op,
        page_op,
        clear_op,
        break_op,
        empty_op,
        close_op,
        scratch_op:
          begin
            file_arg := value (file_arg);
            io_operation (t, file_arg);
          end;
        get_op,
        put_op,
        readln_op,
        writeln_op:
          begin
            file_arg := value (file_arg);
            if not old_file then
              old_file := file_previous (file_arg);
            io_operation (t, file_arg);
          end;
        end_io_op:
          begin
            file_arg := value (file_arg);
            if file_arg^.opcode = out_str_op then
              redefine (file_arg^.operand[1]);
            io_operation (t, file_arg);
          end;
        seek_op:
          begin
            seek_file := value (seek_file);
            seek_index := value (seek_index);
            io_operation (t, seek_file);
          end;
        write_op:
          begin
            rw_file := value (rw_file);
            if not rw_old_file then
              rw_old_file := file_previous (rw_file);
            if rw_mode <> binaryrw then
              rw_item := value (rw_item);
            rw_width := value (rw_width);
            rw_precision := value (rw_precision);
            io_operation (t, rw_file);
          end;
        (*****  Simple Expressions  *****)
        cst_ref:
          begin
            index := abs (cst_code (cst_val)) mod cse_hash_size;
            match_limit := 0;
            lookup_tuple (cse, t, index, match_limit);
          end;
        first_nnary_op..last_nnary_op,
        first_chk_op..last_chk_op: (* Delete redundant check operators. *)
          begin
            match_limit := 0;
            op_sum := 0;
            for ind := 1 to upperbound (operand) do begin
              operand [ind] := value (operand [ind]);
              op_sum := op_sum + operand[ind]^.ref_fre;
              match_limit := max (match_limit, operand[ind]^.nodeid);
            end;
            index := (op_sum * ord(opcode)) mod cse_hash_size;
            lookup_tuple (cse, t, index, match_limit);
            if (result <> t) and (first_chk_op <= opcode) and (opcode <= last_chk_op) then begin
              dechain (t);
              deletion := true;
            end;
          end;
        first_io_func..last_io_func:
          begin
            operand [1] := value (operand[1]);
            ind := operand[1]^.desc.base^.file_class^.id_number;
            index := (operand[1]^.ref_fre * ord(opcode)) mod cse_hash_size;
            if blk_input_tuple
              then match_limit := result^.nodeid
              else match_limit := basic_block^.nodeid;
            match_limit := max (match_limit, operand[1]^.nodeid);
            if in_set (var_set, active, ind) then begin
              lookup_tuple (cse, t, index, match_limit);
              if result <> t then
                check_io (cse, t, base_file, index, match_limit);
            end
            else
              new_value (cse, t, index);
            if result = t then begin
              add_elem (var_set, active, ind);
              if not global andif in_set (var_set, may_be_input, ind) then begin
                blk_input_tuple := true;
                add_elem (potential_inputs, 0, nodeid);
                del_elem (var_set, may_be_input, ind);
              end;
            end;
          end;
	addr_op:
	  new_val_num (cse, t);
        new_op,
        in_str_op,
        gen_andif_op,
        gen_orif_op,
        iostatus_op,
	masked_op,
	pending_op,
        random_op..last_snary_op:
          begin
            for ind := 1 to upperbound (operand) do
              operand [ind] := value (operand [ind]);
            new_val_num (cse, t);
          end;
        out_str_op:
          new_val_num (cse, t);
        (*****  Reference Expressions  *****)
        ident_ref:
          begin
            ind := id_sym^.id_number;
            index := ind mod cse_hash_size;
            if id_sym^.kind <> vars then
              match_limit := 0
            else if blk_input_tuple then
              match_limit := result^.nodeid
            else
              match_limit := basic_block^.nodeid;
            if ((id_sym^.kind <> vars) orif in_set (var_set, active, ind)) and
              not id_sym^.abnormal_use
                then lookup_tuple (cse, t, index, match_limit)
                else new_value (cse, t, index);
            if (result = t) andif (id_sym^.kind = vars) andif
              (context in [valx, basemodx, basevarx]) then begin
                add_elem (var_set, active, ind);
                if not global andif in_set (var_set, may_be_input, ind) andif
                  not id_sym^.abnormal_use then begin
                    if context = valx then
                      blk_input_tuple := true;
                    add_elem (potential_inputs, 0, nodeid);
                    del_elem (var_set, may_be_input, ind);
                end;
            end;
          end;
        ptr_ref:
          begin
	    t_temp := value (base_ptr);
	    if (t_temp^.desc.kind = pointers) andif
	       (t_temp^.desc.base^.target_type = desc.base) then
	      base_ptr := value (base_ptr)
	    else begin
	      if base_ptr^.result^.opcode = assign_op then
		base_ptr^.result^.must_store := true;
	    end;
            ind := base_ptr^.desc.base^.heap_class^.id_number;
            index := (base_ptr^.ref_fre * ord(opcode)) mod cse_hash_size;
            if blk_input_tuple
              then match_limit := result^.nodeid
              else match_limit := basic_block^.nodeid;
            match_limit := max (match_limit, base_ptr^.nodeid);
            if in_set (var_set, active, ind) then begin
              lookup_tuple (cse, t, index, match_limit);
              if result <> t then
                check_mod (cse, t, index, match_limit);
            end
            else
              new_value (cse, t, index);
            if (result = t) andif (context in [valx, basemodx, basevarx]) andif
               (base_ptr^.opcode <> new_op) then begin
              add_elem (var_set, active, ind);
              if not global andif in_set (var_set, may_be_input, ind) then begin
                if context = valx then
                  blk_input_tuple := true;
                add_elem (potential_inputs, 0, nodeid);
                del_elem (var_set, may_be_input, ind);
              end;
            end;
          end;
        buffer_ref:
          begin
            base_file := value (base_file);
            ind := base_file^.desc.base^.file_class^.id_number;
            index := (base_file^.ref_fre * ord(opcode)) mod cse_hash_size;
            if blk_input_tuple
              then match_limit := result^.nodeid
              else match_limit := basic_block^.nodeid;
            match_limit := max (match_limit, base_file^.nodeid);
            if in_set (var_set, active, ind) then begin
              lookup_tuple (cse, t, index, match_limit);
              if result <> t then begin
                check_mod (cse, t, index, match_limit);
                check_io (cse, t, base_file, index, match_limit);
              end;
            end
            else
              new_value (cse, t, index);
            if (result = t) andif (context in [valx, basemodx, basevarx]) then begin
              add_elem (var_set, active, ind);
              if not global andif in_set (var_set, may_be_input, ind) then begin
                if context = valx then
                  blk_input_tuple := true;
                add_elem (potential_inputs, 0, nodeid);
                del_elem (var_set, may_be_input, ind);
              end;
            end;
          end;
        field_ref:
          begin
            base_rec := value (base_rec);
            index := ((base_rec^.ref_fre + field_sym^.id_number) * ord(opcode)) mod cse_hash_size;
            if blk_input_tuple
              then match_limit := result^.nodeid
              else match_limit := basic_block^.nodeid;
            match_limit := max (match_limit, base_rec^.nodeid);
            lookup_tuple (cse, t, index, match_limit);
            if result <> t then
              check_mod (cse, t, index, match_limit)
            else if not global andif
              in_set (potential_inputs, 0, base_rec^.nodeid) andif
              (modification (cse, t, base_rec^.nodeid) = nil) andif
              (context in [valx, basemodx, basevarx]) then begin
                if context = valx then begin
                  blk_input_tuple := true;
                  mark_base_input_tuples (base_rec);
                end;
                add_elem (potential_inputs, 0, nodeid);
            end;
          end;
        array_ref:
          begin
            base_array := value (base_array);
            index_val := value (index_val);
            if blk_input_tuple
              then match_limit := result^.nodeid
              else match_limit := basic_block^.nodeid;
            match_limit := max (match_limit, base_array^.nodeid, index_val^.nodeid);
            index := ((base_array^.ref_fre + index_val^.ref_fre) * ord(opcode)) mod cse_hash_size;
            lookup_tuple (cse, t, index, match_limit);
            if result <> t then
              check_mod (cse, t, index, match_limit)
            else if not global andif
              in_set (potential_inputs, 0, base_array^.nodeid) andif
              (modification (cse, t, base_array^.nodeid) = nil) andif
              (context in [valx, basemodx, basevarx]) then begin
                if context = valx then begin
                  blk_input_tuple := true;
                  mark_base_input_tuples (base_array);
                end;
                add_elem (potential_inputs, 0, nodeid);
            end;
          end;
        substr_ref:
          begin
            base_string := value (base_string);
            substr_index := value (substr_index);
            if blk_input_tuple
              then match_limit := result^.nodeid
              else match_limit := basic_block^.nodeid;
            match_limit := max (match_limit, base_string^.nodeid, substr_index^.nodeid);
            op_sum := base_string^.ref_fre + substr_index^.ref_fre;
            if substr_length <> nil then begin
              substr_length := value (substr_length);
              match_limit := max (match_limit, substr_length^.nodeid);
              op_sum := op_sum + substr_length^.ref_fre;
            end;
            index := (op_sum * ord(opcode)) mod cse_hash_size;
            lookup_tuple (cse, t, index, match_limit);
            if result <> t then
              check_mod (cse, t, index, match_limit)
            else if not global andif
              in_set (potential_inputs, 0, base_string^.nodeid) andif
              (modification (cse, t, base_string^.nodeid) = nil) andif
              (context in [valx, basemodx, basevarx]) then begin
                if context = valx then begin
                  blk_input_tuple := true;
                  mark_base_input_tuples (base_string);
                end;
                add_elem (potential_inputs, 0, nodeid);
            end;
          end;
        (*****  Procedure and Function Calls  *****)
        call_op,
        func_call_op:
          begin
            effects (t);
            mov_set (smod, 0, var_set, 0);
            subtract (var_set, 0, active);
            if not global then
              subtract (var_set, 0, may_be_input);
            mov_set (suse, 0, var_set, 0);
            union (var_set, 0, used);
            kill_mod_references (rhs_ref, smod);
            subr := value (subr);
            op_sum := 0;
            match_limit := 0;
            for ind := 1 to upperbound (arglist) do begin
              if arglist[ind]^.context <> varx then
                arglist [ind] := value (arglist [ind]);
              op_sum := op_sum + arglist[ind]^.ref_fre;
              match_limit := max (match_limit, arglist[ind]^.nodeid);
            end;
            index := (op_sum * ord(opcode)) mod cse_hash_size;
            if (opcode = func_call_op) and is_empty (suse, 0)
              then lookup_tuple (cse, t, index, match_limit)
              else new_value (cse, t, index);
          end;
        others:
      end (* case opcode *);
      if not deletion andif
        (first_expr <= opcode) andif
        (opcode <= last_expr) andif
        (result = t) then begin (* Try to fold an expression. *)
          t_set (t); (* Emit the folded tuple right after T. *)
          t_folded := fold (t, cur_source);
          if t_folded <> t then begin
            if t_folded^.result = nil then begin (* Must be newly created. *)
              if t_folded^.opcode = cst_ref then begin
                index := abs (cst_code (t_folded^.cst_val)) mod cse_hash_size;
                match_limit := 0;
              end
              else if t_folded^.opcode = ineg_op then begin
                index := (operand[1]^.ref_fre * ord(opcode)) mod cse_hash_size;
                match_limit := operand[1]^.nodeid;
              end
              else (* There shouldn't be any other cases. *)
                assert (false);
              lookup_tuple (cse, t_folded, index, match_limit);
            end;
            result := t_folded^.result;
            ref_fre := t_folded^.ref_fre;
          end;
      end;
      t := t_next; (* On to the next tuple. *)
    end (* with t^ *);
  end (* while not end of block *);
end (* reduce *);
(*  ReduceBlocks is the basic block optimization routine.  It reduces basic
    blocks to DAGs, using the Reduce procedure to do the actual work.  *)
procedure reduce_blocks;
var
    block: tuple;
    cse: cse_data;
    rhs_ref: rhs_ref_data;
    file_tpl: tuple;
    var_set: svector;
    pot_inp_set: svector;
begin
  var_set := new_svector (basic_var_sets, sym_vl_number);
  pot_inp_set := new_svector (0, t_chain^.final_tuple^.nodeid);
  clr_set (pot_inp_set, 0);
  cse_init (cse);
  rhs_ref_init (rhs_ref);
  block := t_chain^.first_label;
  while block <> nil do begin
    clr_set (var_set, active);
    clr_set (var_set, used);
    unv_set (var_set, may_be_input);
    file_tpl := nil;
    reduce (block, cse, rhs_ref, file_tpl, var_set, pot_inp_set, false);
    cse_clear (cse, 0);
    rhs_ref_clear (rhs_ref, nil, nil);
    block := block^.downward_thread;
  end;
  del_svector (var_set);
  del_svector (pot_inp_set);
  del_svector (rhs_ref.test_vars);
  del_svector (rhs_ref.listed_tuples);
  reclaim;
end (* reduce_blocks *);
(*  BuildFreTable will process the intermediate form of the current routine,
    establishing the FRE table for the routine.  *)
procedure build_fre_table;
const
    hash_size = 107;
    hash_limit := 106; (* = hash_size - 1 *)
type
    hash_index = 0 .. hash_limit;
    build_list = ^ build_node;
    build_node = packed record
        hash_link: build_list; (* For looking up an FRE during building. *)
        next: build_list; (* The sequential chain of FREs. *)
        entry: fre_node; (* The actual FRE for this node. *)
        number: fre (* The number of the FRE. *)
    end;
    sym_link = ^ sym_link_rec;
    sym_link_rec = packed record
        next: sym_link;
        file_sym: sym
    end;
var
    table: array [hash_index] of build_list; (* The hash table for FRE lookup. *)
    chain: build_list; (* The sequential chain of FREs. *)
    impl_mod: svector; (* See ComputeImplMod. *)
    fre_files: sym_link; (* List of file classes with FREs. *)
(*  FreHash is a function which will take a reference expression tuple, and
    will return the index in the FRE hash array of the bucket to which the
    tuple hashes.  *)
function fre_hash ( t: tuple ): hash_index;
const (* Hashing constants *)
    k_hash = 3;
    k_sym = 0;
    k_elem = 1;
    k_field = 2;
begin
  with t^ do begin
    case opcode of
      ident_ref:
        fre_hash := (k_hash * id_sym^.id_number + k_sym) mod hash_size;
      ptr_ref:
        fre_hash := (k_hash * base_ptr^.desc.base^.heap_class^.id_number + k_sym) mod hash_size;
      buffer_ref:
        fre_hash := (k_hash * base_file^.desc.base^.file_class^.id_number + k_sym) mod hash_size;
      array_ref:
        fre_hash := (k_hash * base_array^.ref_fre + k_elem) mod hash_size;
      substr_ref:
        fre_hash := (k_hash * base_string^.ref_fre + k_elem) mod hash_size;
      field_ref:
        fre_hash := (k_hash * (base_rec^.ref_fre + field_sym^.id_number) + k_field) mod hash_size;
      others (* i/o functions *):
        fre_hash := (k_hash * operand[1]^.desc.base^.file_class^.id_number + k_sym) mod hash_size
    end (* case opcode *);
  end (* with t^ *);
end (* fre_hash *);
(*  FreMatch will test whether an FRE F corresponds to a tuple T.  *)
function fre_match ( f: fre_node; t: tuple ): boolean;
begin
  with t^ do begin
    case opcode of
      ident_ref:  fre_match :=
        (f.kind = sym_fre) andif
        (f.id_sym = id_sym);
      ptr_ref:  fre_match :=
        (f.kind = sym_fre) andif
        (f.id_sym = base_ptr^.desc.base^.heap_class);
      buffer_ref:  fre_match :=
        (f.kind = sym_fre) andif
        (f.id_sym = base_file^.desc.base^.file_class);
      array_ref:  fre_match :=
        (f.kind = elem_fre) andif
        (f.base_fre = base_array^.ref_fre);
      substr_ref:  fre_match :=
        (f.kind = elem_fre) andif
        (f.base_fre = base_string^.ref_fre);
      field_ref:  fre_match :=
        (f.kind = field_fre) andif
        (f.record_fre = base_rec^.ref_fre) andif
        (f.field_sym = field_sym);
      others (* i/o functions *):  fre_match :=
        (f.kind = sym_fre) andif
        (f.id_sym = operand[1]^.desc.base^.file_class)
    end (* case opcode *);
  end (* with t^ *);
end (* fre_match *);
(*  MakeFre will return a new FRE which represents a specified tuple.
    NOTE:  When MakeFre creates a FRE representing a file class, it stores
    the FRE number as the ItemAddr field (yes, really!) of the file class
    symbol, since this provides a place to find it later, and ItemAddr is
    obviously not used as an address for pseudo_symbols.  *)
function make_fre ( t: tuple ): build_list;
var f: sym_link;
begin
  new (make_fre);
  with make_fre^, entry do begin
    n_fre := n_fre + 1;
    number := n_fre;
    case t^.opcode of
      ident_ref:  begin
        kind := sym_fre;
        id_sym := t^.id_sym;
      end;
      ptr_ref:  begin
        kind := sym_fre;
        id_sym := t^.base_ptr^.desc.base^.heap_class;
      end;
      buffer_ref:  begin
        kind := sym_fre;
        id_sym := t^.base_file^.desc.base^.file_class;
        id_sym^.item_addr := n_fre;
        new (f);
        f^.next := fre_files;
        fre_files := f;
        f^.file_sym := id_sym;
      end;
      array_ref:  begin
        kind := elem_fre;
        base_fre := t^.base_array^.ref_fre;
      end;
      substr_ref:  begin
        kind := elem_fre;
        base_fre := t^.base_string^.ref_fre;
      end;
      field_ref:  begin
        kind := field_fre;
        record_fre := t^.base_rec^.ref_fre;
        field_sym := t^.field_sym;
      end;
      others (* i/o functions *):  begin
        kind := sym_fre;
        id_sym := t^.operand[1]^.desc.base^.file_class;
        id_sym^.item_addr := n_fre;
        new (f);
        f^.next := fre_files;
        fre_files := f;
        f^.file_sym := id_sym;
      end
    end (* case t^.opcode *);
  end (* with make_fre^, entry^ *);
end (* make_fre *);
(*  FreNumber returns the FRE number associated with a reference expression.
    A new FRE node will be created, if necessary.  *)
function fre_number ( t: expr ): fre;
var
    f: build_list; (* For looking up and creating FREs. *)
    index: hash_index; (* For indexing the hash table. *)
begin
  (*  If T is a field reference, and its base record is a WITH record, then the
      base record may not have an FRE number yet, since it is not a block input
      tuple.  If this is the case, then we assign it one now.  *)
  with t^ do
    if (opcode = field_ref) andif (base_rec^.ref_fre = 0) then
      base_rec^.ref_fre := fre_number (base_rec);
  (*  Look up the FRE for this tuple in the hash table.  *)
  index := fre_hash (t);
  f := table [index];
  while (f <> nil) andif not fre_match (f^.entry, t) do
    f := f^.hash_link;
  (*  If there is no FRE for this tuple, then create one now.  *)
  if f = nil then begin
    f := make_fre (t);
    f^.next := chain;
    chain := f;
    f^.hash_link := table [index];
    table [index] := f;
  end;
  fre_number := f^.number;
end (* fre_number *);
(*  CreateIoFre will create an FRE entry for the pseudo-symbol IoOpsym, which
    represents remembered i/o operand values.  *)
procedure create_io_fre;
var f: build_list;
    hash: hash_index;
(*  The following constant declarations are copied from FreHash, where they
    ought to have stayed, except that FreHash expects a tuple for its operand,
    and there is no good way to come up with an ident_ref tuple for a pseudo-
    symbol.  Thus, the hash index computation in this routine had better stay
    in line with that in FreHash.  *)
const
    k_hash = 3;
    k_sym = 0;
begin
  new (f);
  with f^ do begin
    n_fre := n_fre + 1;
    number := n_fre;
    io_fre := n_fre;
    entry.kind := sym_fre;
    entry.id_sym := io_opsym;
    next := chain;
    chain := f;
    hash := (k_hash * io_opsym^.id_number + k_sym) mod hash_size;
    hash_link := table [hash];
    table [hash] := f;
  end;
end (* create_io_fre *);
(*  CreateList will scan the reduced intermediate form, computing an FRE for
    each block input tuple.  When it is finished, all the relevant FREs for
    this procedure will have been computed.  *)
procedure create_list;
var t: tuple;
begin
  t := t_chain;
  while t <> nil do begin
    with t^ do begin
      if ( ( (first_data_ref <= opcode) and (opcode <= last_data_ref) ) or
           ( (first_io_func <= opcode) and (opcode <= last_io_func) ) ) and
         (blk_input_tuple) then
        ref_fre := fre_number (t);
      t := next;
    end (* with t^ *);
  end (* while t <> nil *);
  create_io_fre;
end (* create_list *);
(*  MarkAllReferences scans the entire IF, setting the RefFre field of any
    reference expression which matches some FRE which is already in the
    table.  *)
procedure mark_all_references;
var
    t: tuple;
    may_be_fre: boolean;
    index: hash_index;
    f: build_list;
begin
  t := t_chain;
  while t <> nil do begin
    with t^ do begin
      case opcode of
        ident_ref:   may_be_fre := (id_sym^.kind = vars);
        ptr_ref:     may_be_fre := true;
        buffer_ref:  may_be_fre := true;
        array_ref:   may_be_fre := (base_array^.ref_fre <> 0);
        substr_ref:  may_be_fre := (base_string^.ref_fre <> 0);
        field_ref:   may_be_fre := (base_rec^.ref_fre <> 0);
        others:      may_be_fre := ( (first_io_func <= opcode) and
                                     (opcode <= last_io_func) )
      end (* case opcode *);
      if may_be_fre andif (ref_fre = 0) then begin
        index := fre_hash (t);
        f := table [index];
        while (f <> nil) andif not fre_match (f^.entry, t) do
          f := f^.hash_link;
        if f <> nil then
          ref_fre := f^.number;
      end;
      t := next;
    end (* with t^ *);
  end (* while t^.opcode <> end_block *);
end (* mark_all_references *);
(*  ConvertListToTable will take the FRE build list from CreateList, and
    turn it into the proper FRE table.  *)
procedure convert_list_to_table;
var next_chain: build_list;
begin
  new (fre_table, n_fre);
  while chain <> nil do begin
    with chain^ do begin
      fre_table^ [number] := entry;
      next_chain := next;
    end;
    dispose (chain);
    chain := next_chain;
  end;
end (* convert_list_to_chain *);
(*  ComputeImplMod computes the ImplMod sets.  For each FRE, f, ImplMod(f) is
    the set of all those FREs f' such that a change to f must be assumed to
    be a change to f' as well.
    To define ImplMod more rigorously, we must define the relations CONT,
    CIRC, and IDENT.  IDENT is the identity relation:  f IDENT f' iff f = f'.
    CONT is the "contains" relation:  f CONT f' iff the FRE f' has the form
    "element of f" or "field ... of f".  CIRC is related to the "circumscribes"
    relation:  f CIRC f' iff f and f' have the forms "field a of r" and "field
    a' of r", for some FRE r, and a reference of the form r.a would circum-
    scribe a reference of the form r.a' (i.e., f and f' are fields in the same
    record type, and field f circumscribes field f').  Given these relations,
    ImplMod(f) may be defined as the set of FREs f' such that f IMPLMOD f',
    where the relation IMPLMOD is defined as
        IMPLMOD = (IDENT or CIRC) CONT*                                 *)
procedure compute_impl_mod;
var
    i, j: fre;
    base: svector;
    field_list, field_link: index_vector;
begin
  impl_mod := new_svector (n_fre, n_fre);
  base := new_svector (n_fre, n_fre);
  new (field_list, n_fre);
  new (field_link, n_fre);
  for i := 1 to n_fre do begin
    clr_set (impl_mod, i);
    add_elem (impl_mod, i, i);
    field_list^ [i] := 0;
  end;
  (*  Note that if FRE(i) CONTAINS FRE(j), then i < j.  *)
  for i := n_fre downto 1 do begin
    with fre_table^ [i] do begin
      if kind = elem_fre then
        union (impl_mod, i, base_fre)
      else if kind = field_fre then begin
        union (impl_mod, i, record_fre);
        mov_set (impl_mod, i, base, i);
        j := field_list^ [record_fre];
        while j <> 0 do begin
          if circ_fields (field_sym, fre_table^[j].field_sym) then begin
            mov_set (base, j, impl_mod, 0);
            union (impl_mod, 0, i);
          end;
          if circ_fields (fre_table^[j].field_sym, field_sym) then begin
            mov_set (base, i, impl_mod, 0);
            union (impl_mod, 0, j);
          end;
          j := field_link^ [j];
        end;
        field_link^ [i] := field_list^ [record_fre];
        field_list^ [record_fre] := i;
      end (* if kind = field_fre *);
    end;
  end (* for i *);
  del_svector (base);
  dispose (field_list);
  dispose (field_link);
end (* compute_impl_mod *);
(*  FileFre takes the file operand of some i/o operation.  If it is really a
    file, and if an FRE has been assigned to its file class, then the FRE number
    is returned.  Otherwise, zero is returned.  Note the the FRE number for a
    file class is stored as the ItemAddr field of the file class symbol node.  *)
function file_fre ( file_arg: expr ): fre;
var f: sym_link;
    class: sym;
begin
  if (file_arg^.opcode = in_str_op) or (file_arg^.opcode = out_str_op) then
    file_fre := 0
  else begin
    f := fre_files;
    class := file_arg^.desc.base^.file_class;
    while (f <> nil) andif (f^.file_sym <> class) do
      f := f^.next;
    if f = nil
      then file_fre := 0
      else file_fre := class^.item_addr;
  end;
end (* file_fre *);
(*  ComputeInputOutputSets will compute InputSet(b) and OutputSet(b), for each
    basic block b.  An FRE f is in InputSet(b) if there is an input tuple in
    b whose FRE number is f.  An FRE f is in OutputSet(b) if there is a tuple
    in b which modifies some FRE f', and f is in ImplMod(f').  In addition to
    the input and output sets, this routine also establishes the input tuple
    list for each basic block.  *)
procedure compute_input_output_sets;
var
    t: tuple;
    i: fre;
    in_list: tpl_list;
    new_tpl_node: tpl_list;
begin
  input_set := new_svector (n_blocks, n_fre);
  output_set := new_svector (n_blocks, n_fre);
  new (input_tuples, n_blocks);
  t := t_chain^.final_tuple; (* Get the last tuple in the IF. *)
  (*  The input and output sets for a single basic block are accumulated in
      InputSet(0) and ImplMod(0).  When a label node is encountered, these are
      copied into the appropriate entries in the input/output set vectors.  *)
  clr_set (input_set, 0);
  clr_set (impl_mod, 0);
  (*  The input tuple list for a basic block is built from InList.  When a
      label node is encountered, InList is stored in the appropriate entry
      in the InputTuples array.  *)
  in_list := nil;
  while t <> nil do begin
    with t^ do begin
      case opcode of
        label_node:
          begin
            cpy_set (input_set, 0, block_order_no);
            clr_set (input_set, 0);
            mov_set (impl_mod, 0, output_set, block_order_no);
            clr_set (impl_mod, 0);
            input_tuples^ [block_order_no] := in_list;
            in_list := nil;
          end;
        assign_op:
          if lhs^.ref_fre <> 0 then
            union (impl_mod, lhs^.ref_fre, 0);
        first_io_stmt..last_io_stmt:
          begin
            i := file_fre (file_arg);
            if i <> 0 then
              union (impl_mod, i, 0);
            if (opcode >= get_op) and (opcode <= writeln_op) then
              union (impl_mod, io_fre, 0);
            if (opcode = end_io_op) andif
               (file_arg^.opcode = out_str_op) andif
               (file_arg^.operand[1]^.ref_fre <> 0) then
              union (impl_mod, file_arg^.operand[1]^.ref_fre, 0);
          end;
        read_op:
          begin
            if rw_item^.ref_fre <> 0 then
              union (impl_mod, rw_item^.ref_fre, 0);
            i := file_fre (rw_file);
            if i <> 0 then
              union (impl_mod, i, 0);
            union (impl_mod, io_fre, 0);
          end;
        write_op:
          begin
            i := file_fre (rw_file);
            if i <> 0 then
              union (impl_mod, i, 0);
            union (impl_mod, io_fre, 0);
          end;
        seek_op:
          begin
            i := file_fre (seek_file);
            if i <> 0 then
              union (impl_mod, i, 0);
          end;
        call_op, func_call_op:
          begin
            effects (t);
            for i := 1 to n_fre do
              with fre_table^ [i] do
                if (kind = sym_fre) andif in_set (smod, 0, id_sym^.id_number) then
                  union (impl_mod, i, 0);
          end;
        first_data_ref..last_data_ref,
        first_io_func..last_io_func:
          if (blk_input_tuple orif
             ((ref_fre <> 0) andif in_set (input_set, 0, ref_fre))) then begin
            blk_input_tuple := true;
            add_elem (input_set, 0, ref_fre);
            new (new_tpl_node);
            new_tpl_node^.tpl := t;
            new_tpl_node^.next := in_list;
            in_list := new_tpl_node;
          end;
        others:
          (* no action *)
      end (* case opcode *);
      t := prev;
    end (* with t^ *);
  end (* while t^.opcode <> start_block *);
  del_svector (impl_mod);
end (* compute_input_output_sets *);
var index: hash_index;
    f: sym_link;
begin
  n_fre := 0;
  chain := nil;
  fre_files := nil;
  (*  Clear the FRE hash table. *)
  for index := minimum (index) to maximum (index) do
    table [index] := nil;
  create_list;
  mark_all_references;
  convert_list_to_table;
  if switch (cur_block^.dump_switches, 'FRES') then 
    dmpfrtable (fre_table, n_fre);
  compute_impl_mod;
  if switch (cur_block^.dump_switches, 'IMPL_MOD') then
    dmpset (impl_mod, n_fre, 'IMPL_MOD SETS FOR BLOCK $');
  compute_input_output_sets;
  while fre_files <> nil do begin
    f := fre_files^.next;
    dispose (fre_files);
    fre_files := f;
  end;
  if switch (cur_block^.dump_switches, 'IOSETS') then begin
    dmpset (input_set, n_fre, 'BASIC BLOCK INPUT FRE''S FOR BLOCK $');
    dmpset (output_set, n_fre, 'BASIC BLOCK OUTPUT FRE''S FOR BLOCK $');
  end;
  if switch (cur_block^.dump_switches, 'IFM_FRE') then
    dmptuples ('IF FOR BLOCK $, WITH FORMAL REFERENCE EXPRESSIONS');
end (* build_fre_table *);
(*  MakeDominatorTree computes the dominator tree of the program flow graph,
    using the algorithm of Aho and Ullman, Principles of Compiler Design,
    pp 445-446.  The function of the working set NEWD in the algorithm is
    filled by set D(0).  All the dominator tree information is returned in
    three block vectors:
        Idom(i)       is the block which immediately dominates block i.
        DomSon(i)     is the first block on the chain of blocks which
                      are dominated by block i.
        DomBrother(i) links together the chain of blocks which are dominated
                      by Idom(i).
    This routine also checks whether the flow graph is reducible, since the
    check is easier to perform using the dominator set than the dominator
    tree.  A flow graph is reducible iff every back edge in the depth-first
    spanning tree goes from a node to a node which dominates it.  An edge is
    a back edge iff it goes from a higher-numbered node to a lower-numbered
    node in the depth-first ordering.  *)
procedure make_dominator_tree;
var
    d: svector; (* The dominator sets. *)
    i, j: index_range;
    b_pred: tuple;
    change: boolean;
label
    100 (* not reducible *);
begin
  (*  Create the dominator sets, according to the Aho-Ullman algorithm.  *)
  d := new_svector (n_blocks, n_blocks);
  clr_set (d, 1); (* D(n0) := [n0]. *)
  add_elem (d, 1, 1);
  for i := 2 to n_blocks do (* For n in N-[n0], D(n) := N. *)
    unv_set (d, i);
  repeat
    change := false; (* Set if any changes. *)
    for i := 1 to n_blocks do begin
      b_pred := block_labels^[i]^.inward_jumps; (* The first predecessor of block i. *)
      if b_pred = nil then (* Block i has no predecessors. *)
        clr_set (d, 0)
      else begin (* Intersect the dsets of the predecessors. *)
        cpy_set (d, b_pred^.jump_from^.block_order_no, 0);
        b_pred := b_pred^.next_inward_jump;
        while b_pred <> nil do begin
          intersect (d, b_pred^.jump_from^.block_order_no, 0);
          b_pred := b_pred^.next_inward_jump;
        end;
      end;
      add_elem (d, 0, i); (* Every node dominates itself. *)
      if not set_eq (d, 0, i) then begin
        change := true; (* Another iteration is necessary. *)
        cpy_set (d, 0, i);
      end;
    end (* for i := 1 to n_blocks *);
  until not change;
  (*  Check whether the flow graph is reducible.  *)
  reducible := false;
  for i := 1 to n_blocks do begin
    b_pred := block_labels^[i]^.inward_jumps;
    while b_pred <> nil do begin
      j := b_pred^.jump_from^.block_order_no;
      if (j > i) andif not in_set (d, j, i) then
        goto 100 (* not reducible *);
      b_pred := b_pred^.next_inward_jump;
    end;
  end;
  reducible := true;
100 (* not reducible *):
  (*  Now use the dominator set information to build the dominator tree.  The
      key fact is that the immediate dominator of a node is the node in its
      dominator set with the largest block order number, since no node can
      dominate a node which precedes it in the depth-first ordering.  *)
  new (idom, n_blocks);
  new (dom_son, n_blocks);
  new (dom_brother, n_blocks);
  for i := 1 to n_blocks do
    dom_son^ [i] := 0;
  (*  By processing the basic blocks in reverse order in the following loop,
      we guarantee that the children of a node in the dominator tree will
      be ordered by DFST number.  (I.e., for all nodes n, DomBrother(n) > n.)
      This guarantees that during global optimization, the block containing
      a GEN_ANDIF or GEN_ORIF tuple will be processed after the blocks
      containing its operands.  *)
  for i := n_blocks downto 2 do begin
    j := i - 1; (* Find the largest dominator. *)
    while not in_set (d, i, j) do
      j := j - 1;
    idom^ [i] := j;
    dom_brother^ [i] := dom_son^ [j];
    dom_son^ [j] := i;
  end;
  idom^ [1] := 0;
  dom_brother^ [1] := 0;
  del_svector (d); (* We no longer need the set information. *)
  if switch (cur_block^.dump_switches, 'DOM') then
    dmpdominators (idom, dom_son, dom_brother, reducible);
end (* make_dominator_tree *);
(*  ComputeIdef implements Algorithm 4A of Reif, 'Combinatorial Aspects of
    Symbolic Program Analysis', page 4-16 (discussed on pages 4-8 - 4-19).
    Some significant modifications are:
    (1) Rather than explicitly computing the postorder numbering of the control
        flow graph, ComputeIdef calls the recursive routine NodeIdef to do the
        actual processing.  Since NodeIdef first calls itself with each son of
        the current node in the dominator tree, and then processes the current
        node, postorder processing is implicit in the implementation.
    (2) The Reif algorithm runs in almost linear time.  To accomplish this, it
        makes two passes over the dominator tree.  The first pass processes the
        dominator tree with the almost linear UNION-FIND algorithm using path
        compression on balanced trees.  This produces a sequence of LINK and
        EVAL instructions, which can be processed offline in almost linear time,
        also using path compression on balanced trees, using the algorithms of
        Tarjan ('Applications of Path Compression on Balanced Trees').  The
        implementation presented here makes only a single pass over the domi-
        nator tree.  This necessitates online execution of the LINK and EVAL
        instructions, so that while path compression is still applicable,
        balancing is not.  According to Tarjan, this running time of these
        algorithms without balancing is O(m*max(2,log(n**2/m)/log(2*m/n))).
        However, the non-balancing algorithms are MUCH simpler.
    (3) The set of edges Aw is never explicitly computed.  It is just as easy
        to process them each time they are needed as it would be to save them,
        either in a set or in an adjacency list.
    (4) The graph Gw' is never actually created.  The only reason for creating
        it is that we want to process the strongly connected components of Gw
        in the topological order of Gw'.  However, the Tarjan algorithm for
        strongly connected components produces the components of Gw in reverse
        topological order in any case, so a separate topological sort is
        unnecessary.
    (5) Reif defines a helping function R in the weak environment computation.
        Because it is easy to do, we compute R at the same time that we are
        computing Idef.  *)
procedure compute_idef ( set_size: elem_number );
var
    (*  E is used to represent Reif's sets Ew.  It's entries are computed in
        ComputeEw, and it is described there and in the main procedure of
        NodeIdef.  *)
    e: svector;
    (*  The strongly connected components of the graph Gw are represented by
        the variable FirstScc and the link array NextScc, which chain the
        individual components of Gw, and by the link array SccLink, which
        chains the elements of each component.  These are computed by pro-
        cedure Condense, and are described there and in the main procedure
        of NodeIdef.  *)
    first_scc: index_range;
    next_scc: index_vector;
    scc_link: index_vector;
    (*  The vector F and set vector Cc represent the data structured of the
        same names in the Tarjan algorithm for the LINK/EVAL problem.  They
        defined, used, and described in procedure ProcessComponent.  *)
    f: index_vector;
    cc: svector;
    (*  The following variables actually reference dynamically allocated working
        storage for the Condense procedure.  They are declared here, rather than
        in Condense, to save the cost of allocating and deallocating them each
        time Condense is called.  *)
    stacked_nodes: svector;
    marked_nodes: svector;
    dfnumber: index_vector;
    lowlink: index_vector;
(*  NodeIdef is initially called by ComputeIdef with the root node of the
    dominator tree.  It first calls itself with each son in the dominator
    tree of its argument node, and then performs the IDEF processing on
    its argument node.  *)
procedure node_idef ( w: index_range );
(*  For each son n of w, ComputeEw creates the set
        E(n) = { H(m,w) | (m,n) in A, m <> w },
    where A is the set of edges of the flow graph, and H(m,w) is the node
    satisfying the relation
        w -> H(m,w) ->* m .
    ("->" is the relation "immediately dominates".)  *)
procedure compute_ew;
var
    n, m, hmw, m1: index_range;
    m_tpl: tuple;
begin
  (*  Apply the processing to each son n of w.  *)
  n := dom_son^ [w];
  while n <> 0 do begin
    clr_set (e, n);
    (*  Process each edge leading in to node n.  *)
    m_tpl := block_labels^[n]^.inward_jumps;
    while m_tpl <> nil do begin
      m := m_tpl^.jump_from^.block_order_no;
      if m <> w then begin
        (*  The following code implements the FIND(m) operation in the Reif
            algorithm.  The algorithm is the EVAL algorithm with path com-
            pression (but without balancing), from Tarjan, 'Applications of
            Path Compression on Balanced Trees', page 10.  The variable m
            takes the place of variable y and parameter v, m1 takes the
            place of x, hmw takes the place of r, and the set cc(0) takes
            the place of the result variable a.  *)
        if f^ [m] = 0 then
          hmw := m
        else begin
          m1 := 0;
          hmw := f^ [m];
          (*  In this first loop, we work our way up to the root of the tree
              containing m.  In the process, we reverse the f links in all
              the nodes along the path to the root, so that we can process all
              these nodes again in the second (path compression) loop.  *)
          while f^ [hmw] <> 0 do begin
            f^ [m] := m1;
            m1 := m;
            m := hmw;
            hmw := f^ [hmw];
          end;
          (*  In this second loop, we process all the nodes along the path
              from m to the root, setting the f link in each of them to point
              to the root (hmw), and collecting the union of all their cc
              values in cc(0).  *)
          cpy_set (cc, m, 0);
          while m1 <> 0 do begin
            m := f^ [m1];
            union (cc, m1, 0);
            cpy_set (cc, 0, m1);
            f^ [m1] := hmw;
            m1 := m;
          end;
        end;
        (*  End of the FIND operation.  *)
        add_elem ( e, n, hmw );
      end (* m <> w *);
      m_tpl := m_tpl^.next_inward_jump;
    end (* while m_tpl <> nil *);
    n := dom_brother^ [n];
  end (* while n <> 0 *);
end (* compute_ew *);
(*  Condense computes and orders the strongly connected components of the graph
    Gw, whose nodes are the sons of w in the dominator tree and whose edges are
    the edges in Ew.  The results of Condense are in FirstScc, NextScc, and
    SccLink, as described in the comment where Condense is called from NodeIdef.
    The strongly connected components are computed according to the algorithm in
    Aho, Hopcroft and Ullman, 'The Design and Analysis of Computer Algorithms',
    pages 192-193 (discussed on pages 189-195).  The algorithm was originally
    developed in Tarjan, 'Depth-first Search and Linear Graph Algorithms'.  It
    is implicit in the algorithm, although it is not explicitly stated in either
    of these sources, that the order in which the strongly connected components
    are detected by the algorithm is the reverse of the topological order of the
    condensation of the original graph.  Thus, a topological sort is not needed
    to determine the topological ordering of Gw'.
    In this implementation, there is a set MarkedNodes.  A node is marked "old"
    if it is in MarkedNodes, and "new" if it is not.  Nodes which are on the
    stack are also recorded in the set StackedNodes for quick testing.  The
    nicest touch is that the stack of nodes is maintained using the SccLink
    nodes (the top is indicated by StackTop), so that when a component is
    popped from the stack, the nodes in the component are already chained
    together, and it is only necessary to set the link in the last node of
    the component to zero.
    The variables StackedNodes, MarkedNodes, DfNumber, and LowLink refer to
    dynamically allocated data structures for Condense.  Since these would
    be allocated with the same size each time Condense is called, and be
    deallocated each time Condense returns, they are declared, allocated,
    and deallocated in the main procedure of ComputeIdef instead.  *)
procedure condense;
var
    v: index_range;
    count: index_range;
    stack_top: index_range;
procedure searchc ( v: index_range );
var n: index_range;
begin
  add_elem (marked_nodes, 0, v); (* Mark v "old". *)
  dfnumber^ [v] := count;
  count := count + 1;
  lowlink^ [v] := dfnumber^ [v];
  scc_link^ [v] := stack_top; (* Push v on the stack. *)
  stack_top := v;
  add_elem (stacked_nodes, 0, v);
  n := dom_son^ [w];
  while n <> 0 do begin
    if in_set (e, n, v) then (* For each vertex n on L[v] do: *)
      if not in_set (marked_nodes, 0, n) then begin (* If n is marked "new": *)
        searchc (n);
        lowlink^ [v] := min (lowlink^ [v], lowlink^ [n]);
      end
      else begin (* If n is marked "old": *)
        if (dfnumber^ [n] < dfnumber^ [v]) and
          (in_set (stacked_nodes, 0, n)) then
            lowlink^ [v] := min (lowlink^ [v], dfnumber^ [n]);
      end;
    n := dom_brother^ [n];
  end (* while n <> 0 *);
  if lowlink^ [v] = dfnumber^ [v] then begin
    next_scc^ [stack_top] := first_scc; (* Link a new component. *)
    first_scc := stack_top;
    repeat
      n := stack_top; (* Pop n from the top of the stack. *)
      stack_top := scc_link^ [stack_top];
      del_elem (stacked_nodes, 0, n);
    until n = v;
    scc_link^ [v] := 0; (* Unlink the component from the stack. *)
  end;
end (* searchc *);
begin
  (*  Initialize the algorithm.  *)
  count := 1;
  first_scc := 0;
  clr_set (marked_nodes, 0); (* For all v in V do mark v "new". *)
  stack_top := 0; (* Initialize stack to empty. *)
  clr_set (stacked_nodes, 0);
  (*  While there exists a vertex v marked "new" do SEARCHC(v).  *)
  v := dom_son^ [w];
  while v <> 0 do begin
    if not in_set (marked_nodes, 0, v) then
      searchc (v);
    v := dom_brother^ [v];
  end;
end (* condense *);
(*  ProcessComponent computes IDEF(n) for each node n in component s.  (Since
    the nodes in s are strongly connected, they must all have the same IDEF.)
    We know that any node which (1) is dominated by w, (2) is not dominated by
    any node is s, and (3) has an edge leading to a node in s, has already
    been completely processed.  *)
procedure process_component ( s: index_range );
var
    n, m, m1, r: index_range;
    m_tpl: tuple;
begin
  (*  The set Idef(s), which has not yet been evaluated, is used for the working
      set QS of Reif's algorithm.  *)
  clr_set (idef, s);
  (*  S is non-trivial if it contains more than one node, or if it contains
      only a single node n, and there is a path from n to n in the original
      flow graph (which means that (n,n) is in Ew, i.e., n is in E(n)).  If
      s is non-trivial, it means that it is possible to get from any node in
      s to any other node in s without going through w, and that any vari-
      ables which are defined by some node in s must be included in IDEF
      for each node in s.  *)
  if (scc_link^ [s] <> 0) or (in_set (e, s, s)) then begin
    n := s;
    while n <> 0 do begin
      mov_set (output_set, n, idef, 0);
      union (idef, 0, s);
      n := scc_link^ [n];
    end;
  end;
  (*  The key step in the Reif algorithm is:
        for all n in S do
          for all (m,n) in Aw do
            Qs := Qs  U  EVAL(m)  U  OUT(m)  *)
  n := s;
  while n <> 0 do begin (* For all n in S do *)
    m_tpl := block_labels^[n]^.inward_jumps;
    while m_tpl <> nil do begin (* For all (m,n) in Aw do *)
      m := m_tpl^.jump_from^.block_order_no;
      if m <> w then begin
        mov_set (output_set, m, idef, 0); (* Qs := Qs U OUT(m) *)
        union (idef, 0, s);
        (*  The following code implements the EVAL(m) operation.  The algorithm
            is the EVAL algorithm with path compression (but without balancing)
            from Tarjan, 'Applications of Path Compression on Balanced Trees',
            page 10.  Variable m takes the place of parameter v and variable y,
            and m1 takes the place of x.  The set cc(0) takes the place of the
            result variable a.  *)
        if f^ [m] = 0 then
          clr_set (cc, 0)
        else begin
          m1 := 0;
          r := f^ [m];
          (*  In this first loop, we work our way up to the root of the tree
              containing m.  In the process, we reverse the f links in all
              the nodes along the path to the root, so that we can process
              all these nodes again in the second (evaluation and path com-
              pression) loop.  *)
          while f^ [r] <> 0 do begin
            f^ [m] := m1;
            m1 := m;
            m := r;
            r := f^ [r];
          end;
          (*  In this second loop, we process all the nodes along the path
              from m to the root, setting the f link in each of them to point
              to the root (r), and collecting the union of all their cc values
              in cc(0).  *)
          cpy_set (cc, m, 0);
          while m1 <> 0 do begin
            m := f^ [m1];
            union (cc, m1, 0);
            cpy_set (cc, 0, m1);
            f^ [m1] := r;
            m1 := m;
          end;
        end;
        (*  End of the EVAL operation.  *)
        mov_set (cc, 0, idef, 0); (* Qs := Qs U EVAL(m) *)
        union (idef, 0, s);
      end (* m <> w *);
      m_tpl := m_tpl^.next_inward_jump;
    end (* while m_tpl <> nil *);
    n := scc_link^ [n];
  end (* while n <> 0 *);
  (*  The final step.  For each node n in s, link n as a son of w, with
      edge value Qs U OUT(n), and set IDEF(n) to Qs.  The link algorithm
      is also from Tarjan, page 9.  Note that the LINK operation in the
      Reif algorithm is LINK(n,w,Qs), while the we are doing LINK(n,w,
      (Qs U OUT(n)).  Using the LINK operation as defined by Reif, EVAL
      does not really return DEF(H'(m,w,S),m), because OUT(k) is never
      included in a LINK operation if k is the node in a trivial compo-
      nent.  (See pages 4-8 and 4-17,18.)  Our modification remedies this
      omission.  *)
  n := s;
  while n <> 0 do begin
    f^ [n] := w; (* LINK (n,w,Qs U OUT(n)) *)
    mov_set (idef, s, cc, n);
    mov_set (output_set, n, cc, 0);
    union (cc, 0, n);
    cpy_set (idef, s, n); (* IDEF(n) := Qs *)
    n := scc_link^ [n];
  end;
end (* process_component *);
var
    n, s: index_range;
begin
  (*  If w is a leaf node in the dominator tree, we don't need to do anything
      with it.  Otherwise, we must begin by processing all of its sons.  *)
  if dom_son^ [w] <> 0 then begin
    n := dom_son^ [w];
    while n <> 0 do begin
      node_idef (n);
      n := dom_brother^ [n];
    end;
    (***********************************
    Now we are supposed to compute the sets Aw and Ew.  If (m,n) is an edge
    in the flow graph, IDOM(n) = w, and m <> w, then Aw contains the edge
    (m,n), and Ew contains the edge (H(m,w),n), where H(m,w) is the son of
    w in the dominator tree which dominates m (i.e., w -> H(m,w) ->* m,
    where "->" is the "immediately dominates" relation).
    Actually, the edges in Aw can be computed as they are needed, so there
    is no need to create the set Aw.  We use a single vector of sets, E, to
    represent all of the Ew's--if (m,n) is in Ew, then m is in E(n).
    ***********************************)
    compute_ew;
    (***********************************
    Let Gw be the graph (IDOM-1[w],Ew), where IDOM-1[w] is the set of all
    the nodes which are immediately dominated by w.  We are now supposed
    to compute Gw', the condensation of Gw.  We do this by replacing each
    strongly connected component, S, of Gw by a single node which repre-
    sents all of the nodes of S.  Gw' is obviously acyclic.  We then apply
    the processing below to each strongly connected component of Gw, in the
    topological order of Gw'.  Actually, we have no interest in Gw', just
    in its topological ordering, which guarantees that if there is a path
    from a node in component S1 to a node in component S2, then S1 will be
    processed before S2.  Our condensation routine creates a list of the
    strongly connected components of Gw, ordered by the topological order
    of Gw'.  The variable FirstScc indicates the first node in this list;
    subsequent elements are chained together by links in the NextScc array.
    Each node in the Scc list represents a single component; individual
    nodes in the component are chained together by links in the SccLink
    array.
    ***********************************)
    condense;
    (***********************************
    We are supposed to process each strongly connected component S of Gw
    in topological order of Gw'.  This just means stepping through the
    component list that was created by Condense, and processing each com-
    ponent in turn.
    ***********************************)
    s := first_scc;
    while s <> 0 do begin
      process_component (s);
      s := next_scc^ [s];
    end;
  end (* dom_son^ [w] <> 0 *);
  (***********************************
  Reif, in Algorithm 4B (page 4-22) calls for the computation of an array
  of sets R.  Since R is to be computed in a postorder scan of the dominator
  tree, and since we now have all the information we need to compute R(w),
  we might as well do so, and save ourselves another postorder scan later.
  R(w) is defined to be the union, over all the blocks n in IDOM-1[w], of the
  sets (R(n) U IN(n)) - IDEF(n).  It represents the set of all variables which
  are input variables to some block dominated by w, and which are known to have
  the same value on input to that node that they do on exit from w.
  ***********************************)
  clr_set (r_set, w);
  n := dom_son^ [w];
  while n <> 0 do begin
    mov_set (input_set, n, r_set, 0);
    union (r_set, n, 0);
    mov_set (r_set, 0, idef, 0);
    subtract (idef, n, 0);
    mov_set (idef, 0, r_set, 0);
    union (r_set, 0, w);
    n := dom_brother^ [n];
  end;
end (* node_idef *);
var v: index_range;
begin
  (*  Initialize the working storage for the computation.  *)
  e := new_svector (n_blocks, n_blocks);
  new (next_scc, n_blocks);
  new (scc_link, n_blocks);
  (*  Initialize the working storage for Condense.  *)
  new (dfnumber, n_blocks);
  new (lowlink, n_blocks);
  marked_nodes := new_svector (0, n_blocks);
  stacked_nodes := new_svector (0, n_blocks);
  (*  The EVAL and LINK algorithms build a tree, which is represented by the
      array f (the father of each node in the tree) and the set vector cc
      (the set of variables associated with each edge in the tree.  *)
  new (f, n_blocks);
  cc := new_svector (n_blocks, set_size);
  for v := 1 to n_blocks do begin
    f^ [v] := 0;
    clr_set (cc, v);
  end;
  (*  Allocate space for the output set vector IDEF.  *)
  idef := new_svector (n_blocks, set_size);
  r_set := new_svector (n_blocks, set_size);
  (*  Apply the processing algorithm to the dominator tree in postorder.  *)
  node_idef (1);
  (*  Dispose of the working storage.  *)
  del_svector (e);
  dispose (next_scc);
  dispose (scc_link);
  dispose (f);
  del_svector (cc);
  (*  Dispose of the working storage for Condense.  *)
  dispose (dfnumber);
  dispose (lowlink);
  del_svector (marked_nodes);
  del_svector (stacked_nodes);
end (* compute_idef *);
(*  This is a straightforward implementation of Algorithm 4B of Reif,
    'Combinatorial Aspects of Symbolic Program Analysis', page 4-22
    (discussed on pages 4-20 - 4-24).  The intermediate function M in the
    WeakVal computation, which is the intersection of Idef(n) and R(n), is
    computed in R(n), which it replaces.
    There is one error in Reif's algorithm 4B.  If W(X->n) = n, the algorithm
    will not compute this properly, since (due to the definition of R and the
    structure of the WeakVal procedure) node n is only pushed on WS(X) if the
    output value of X from n is used in some node dominated by n--not if it is
    used in node n itself.  Our procedure WeakVal corrects this error.  *)
procedure compute_weak_environment;
type
    stack = ^ stack_node;
    stack_node = packed record
        top: tuple; (* A basic block label tuple. *)
        rest: stack
    end;
    stack_array = array [0..*] of stack;
var
    ws: ^ stack_array;
procedure weak_val ( n: index_range );
var
    m: index_range;
    in_tpl: tpl_list;
    x: fre;
    stack_entry: stack;
begin
  in_tpl := input_tuples^ [n];
  while in_tpl <> nil do begin
    with in_tpl^.tpl^ do
      if in_set (idef, n, ref_fre)
        then result := block_labels^ [n]
        else result := ws^[ref_fre]^.top;
    in_tpl := in_tpl^.next;
  end;
  mov_set (idef, n, r_set, 0);
  intersect (r_set, 0, n);
  for x := 1 to n_fre do
    if in_set (r_set, n, x) then begin
      new (stack_entry);
      stack_entry^.top := block_labels^ [n];
      stack_entry^.rest := ws^ [x];
      ws^ [x] := stack_entry;
    end;
  m := dom_son^ [n];
  while m <> 0 do begin
    weak_val (m);
    m := dom_brother^ [m];
  end;
  for x := 1 to n_fre do
    if in_set (r_set, n, x) then begin
      stack_entry := ws^[x]^.rest;
      dispose (ws^ [x]);
      ws^ [x] := stack_entry;
    end;
end (* weak_val *);
var
    i: index_range;
    root_entry: stack;
    next_tpl: tpl_list;
begin
  new (ws, n_fre);
  new (root_entry);
  root_entry^.top := block_labels^ [1];
  root_entry^.rest := nil;
  for i := 1 to n_fre do
    ws^ [i] := root_entry;
  clr_rslt;
  weak_val (1);
  dispose (root_entry);
  dispose (ws);
  del_svector (r_set);
  if switch (cur_block^.dump_switches, 'WENV') then
    dmpwenv (input_tuples, n_blocks);
  for i := 1 to n_blocks do
    while input_tuples^ [i] <> nil do begin
      next_tpl := input_tuples^[i]^.next;
      dispose (input_tuples^ [i]);
      input_tuples^ [i] := next_tpl;
    end;
end (* compute_weak_environment *);
(*  ReduceGlobal is the global optimization routine.  It applies the Reduce
    procedure to basic blocks of the program, in dominator tree pre-order,
    using recursive procedure RedGblBlk.  *)
procedure reduce_global;
var
    cse: cse_data;
    rhs_ref: rhs_ref_data;
    var_set: svector;
    non_fre_vars: svector;
    file_tpl: tuple;
(*  RedGblBlk calls Reduce to process the basic blocks of the program in
    dominator tree pre-order.  It uses a non-recursive tree walk, with a
    separate stack to keep status information about dominating blocks.  *)
procedure red_gbl_blk;
type
    block_save_data = record
	next: ^ block_save_data;
	active_set: svector;
	input_references, input_killed: ref_desc;
	save_file: tuple;
    end;
var
    save_node: block_save_data;
    save_stack: ^ block_save_data;
    block: index_range;
    pot_inp_set: svector;
begin
  block := 1;			(* The root of the dominator tree. *)
  save_stack := nil;
  while block <> 0 do begin
    loop
      with save_node do begin	(* Save status for block. *)
	active_set := new_svector (0, sym_vl_number);
	mov_set (var_set, active, active_set, 0);
	if in_set (idef, block, io_fre) then
	  del_elem (var_set, active, io_opsym^.id_number);
	input_references := rhs_ref.references;
	input_killed := rhs_ref.killed_stack;
	unv_set (var_set, used);
	kill_mod_references (rhs_ref, non_fre_vars);
	kill_idef (rhs_ref, block);
	save_file := file_tpl;
      end;
      reduce (block_labels^ [block], cse, rhs_ref, file_tpl, var_set, pot_inp_set, true);
    exit if dom_son^[block] = 0;
      block := dom_son^[block];		(* Descend one level in the tree. *)
      save_node.next := save_stack;
      new (save_stack);
      save_stack^ := save_node;
    end;
    loop
      with save_node do begin		(* Restore state information. *)
	mov_set (active_set, 0, var_set, active);
	del_svector (active_set);
	cse_clear (cse, idom^ [block]);
	rhs_ref_clear (rhs_ref, input_references, input_killed);
	file_tpl := save_file;
      end;
    exit if (dom_brother^[block] <> 0) or (idom^[block] = 0);
      block := idom^[block];		(* Back up one level in the tree. *)
      save_node := save_stack^;
      dispose (save_stack);
      save_stack := save_node.next;
    end;
    block := dom_brother^[block];
  end;
end (* red_gbl_blk *);
var f: fre;
begin
  var_set := new_svector (global_var_sets, sym_vl_number);
  clr_set (var_set, active);
  cse_init (cse);
  rhs_ref_init (rhs_ref);
  non_fre_vars := new_svector (0, sym_vl_number);
  unv_set (non_fre_vars, 0);
  for f := 1 to n_fre do
    with fre_table^[f] do
      if kind = sym_fre then
        del_elem (non_fre_vars, 0, id_sym^.id_number);
  file_tpl := nil;
  red_gbl_blk;
  del_svector (var_set);
  del_svector (rhs_ref.test_vars);
  del_svector (rhs_ref.listed_tuples);
  del_svector (non_fre_vars);
  reclaim;
end (* reduce_global *);
(*  Optimize performs the optimizing transformations on the first pass output,
    which has already been converted into basic blocks by MakeBasicBlocks.  *)
public procedure optimize;
var
    i: index_range;
    t: tuple;
begin
  (*  Determine the number of basic blocks in this subroutine.  *)
  n_blocks := t_chain^.last_label^.block_order_no;
  (*  Make a vector of pointers to the label nodes of the basic blocks.  *)
  new (block_labels, n_blocks);
  t := t_chain^.first_label;
  for i := 1 to n_blocks do begin
    block_labels^ [i] := t;
    t := t^.downward_thread;
  end;
  if switch (cur_block^.dump_switches, 'FGRAPH') then
    dmpfgraph (block_labels, n_blocks);
  (*  Reduce the basic blocks of the subroutine to DAGs.  *)
  reduce_blocks;
  if switch (cur_block^.dump_switches, 'DAGS') then
    dmptuples ('BLOCK $ AFTER BASIC BLOCK OPTIMIZATION');
  (*  Compute the dominator tree for the flow graph, representing it in the
      arrays Idom, DomSon, and DomBrother.  *)
  make_dominator_tree;
  if switch (cur_block^.dump_switches, 'NOGLOBAL') then
    return;
  (*  Compute the Formal Reference Expressions for this subroutine.  *)
  build_fre_table;
  (*  Compute the IDEF function for the flow graph.  *)
  compute_idef (n_fre);
  if switch (cur_block^.dump_switches, 'IDEF') then
    dmpset (idef, n_fre, 'IDEF FOR BLOCK $');
  if switch (cur_block^.dump_switches, 'R') then
    dmpset (r_set, n_fre, 'INTERMEDIATE SET R FOR BLOCK $');
  (*  Compute the weak environment.  *)
  compute_weak_environment;
  (*  Perform global optimization.  *)
  reduce_global;
  del_svector (input_set);
  del_svector (output_set);
  dispose (fre_table);
  del_svector (idef);
end (* optimize *);
(*  LowReduce is a modified version of the ReduceBlocks routine, designed to
    eliminate redundant tuples from basic blocks in the low-level intermediate
    form.  This is a much easier task than ReduceBlocks has, since no attempt
    is made to keep track of the values of variables.
    The algorithm is implemented with a hash table.  Each hash table entry
    points to a list of tuples with the same hash index, linked together by
    their Result fields.  The Result field is also used to represent the
    value of a tuple.  The two cases are distinguished by the KilledTuple
    flag, which is true if a tuple is its own value (and is on the hash list),
    and is false if the Result field of the tuple points to its true value.
    The basic blocks of the program are processed in dominator tree pre-order,
    so that at any point, the lists of tuples in the hash table contain tuples
    from the current block and all its dominators.  *)
public procedure low_reduce;
const
    hash_size = 223;
    hash_limit = 222;
type
    hash_index = 0 .. hash_limit;
var
    table: array [hash_index] of tuple;
    val_number: fre;
(*  LowValue returns the "value" of an expression tuple T.  This is analogous
    to the Value function for Reduce, but doesn't need to worry about the
    contexts of expressions.  *)
function low_value ( t: tuple ): tuple;
begin
  if t = nil then
    low_value := nil
  else
    with t^ do
      if killed_tuple orif (result = nil)
        then low_value := t
        else low_value := result;
end (* low_value *);
(*  LowLookup tests whether there is a tuple in the list for hash index I
    which matches tuple T.  The Limit parameter is the largest node id number
    of any of the operands of T; any tuple matching T must follow all of the
    operands of T in the intermediate form.  If a match is found, the result
    and value number of T are set to the matching tuple.  Otherwise, T is
    added to the hash list with a new value number.  *)
procedure low_lookup ( t: tuple; i: hash_index; limit: id_range );
var scan: tuple;
begin
  if t^.desc.kind in simple_types then begin
    scan := table [i];
    while (scan <> nil) andif (scan^.nodeid > limit) do begin
      if tpl_match (t, scan) then begin
        with t^ do begin
          result := scan;
          ref_fre := scan^.ref_fre;
          killed_tuple := false;
        end;
        return; (* <---- Exit with matching tuple found. *)
      end;
      scan := scan^.result;
    end;
  end;
  with t^ do begin (* No match, T is its own value. *)
    result := table [i];
    table [i] := t;
    killed_tuple := true;
    val_number := val_number + 1;
    ref_fre := val_number;
  end;
end (* low_lookup *);
(*  LowRedBlk performs the low-level optimization of a single basic block.  *)
procedure low_red_blk ( first: tuple );
var
    t: tuple; (* To scan the tuple chain. *)
    ind: int_type; (* Operand index variable. *)
    match_limit: id_range; (* The limit for a backward scan. *)
    op_sum: int_type; (* The sum of the operand value numbers. *)
begin
  t := first^.next;
  while (t^.opcode <> label_node) and (t^.opcode <> end_block) do begin
    with t^ do begin
      case opcode of
        start_with:
          with_rec := low_value (with_rec);
        jump_op,
        retjump_op,
        jump_t_op,
        jump_f_op,
        jump_in_op,
        gen_jump_op,
        case_jump_op:
          cond := low_value (cond);
        goto_op:
          target_frame := low_value (target_frame);
        dispose_op:
          dptrarg := low_value (dptrarg);
        eval_op,
        assign_op:
          begin
            lhs := low_value (lhs);
            rhs := low_value (rhs);
          end;
        first_io_stmt..last_io_stmt:
          file_arg := low_value (file_arg);
        read_op,
        write_op:
          begin
            rw_file := low_value (rw_file);
            rw_item := low_value (rw_item);
            rw_width := low_value (rw_width);
            rw_precision := low_value (rw_precision);
          end;
        seek_op:
          begin
            seek_file := low_value (seek_file);
            seek_index := low_value (seek_index);
          end;
        first_nnary_op..last_nnary_op:
          begin
            match_limit := 0;
            op_sum := 0;
            for ind := 1 to upperbound (operand) do begin
              operand [ind] := low_value (operand [ind]);
              match_limit := max (match_limit, operand[ind]^.nodeid);
              op_sum := op_sum + operand[ind]^.ref_fre;
            end;
            low_lookup (t, (op_sum * ord(opcode)) mod hash_size, match_limit);
          end;
        first_snary_op..last_snary_op:
          begin
            for ind := 1 to upperbound (operand) do
              operand [ind] := low_value (operand [ind]);
            killed_tuple := true;
            val_number := val_number + 1;
            ref_fre := val_number;
          end;
        addr_ref,
        mem_ref,
        immed_ref:
          begin
            item.base := low_value (item.base);
            item.index := low_value (item.index);
            if (opcode <> mem_ref) orif
               ( (item.class = parameter_sc) andif
                 ( (item.sym_name = nil) orif
                   (item.sym_name <> item.sym_name^.block^.return_sym) ) ) then begin
              op_sum := 0;
              match_limit := 0;
              if item.base <> nil then begin
                op_sum := item.base^.ref_fre;
                match_limit := item.base^.nodeid;
              end;
              if item.index <> nil then begin
                op_sum := op_sum + item.index^.ref_fre;
                match_limit := max (match_limit, item.index^.nodeid);
              end;
              if item.class = constant_sc then
                op_sum := op_sum + cval_code (item.cstref)
              else if (item.class in [local_sc..code_sc, absolute_sc]) andif
                (item.sym_name <> nil) then
                  op_sum := op_sum + item.sym_name^.id_number;
              low_lookup (t, (op_sum * ord(opcode)) mod hash_size, match_limit);
            end
            else begin
              killed_tuple := true;
              val_number := val_number + 1;
              ref_fre := val_number;
            end;
          end;
        display_op:
          low_lookup (t, (nlevels * ord(opcode)) mod hash_size, 0);
        call_op,
        func_call_op:
          begin
            subr := low_value (subr);
            for ind := 1 to upperbound (arglist) do
              arglist [ind] := low_value (arglist [ind]);
            if opcode = func_call_op then begin
              killed_tuple := true;
              val_number := val_number + 1;
              ref_fre := val_number;
            end;
          end;
        others:
          (* no action *)
      end (* case opcode *);
      t := next;
    end (* with t^ *);
  end (* while not end of block *);
end (* low_reduce *);
(*******************************************************************************
(*  DoReferenceAllocation adds to the intermediate form the information needed
    to perform intelligent register allocation.  Each loop is preceded by a
    FREEZE operator for all tuples which are defined outside the loop and
    referenced in the loop.  Such a loop is followed by a DECREMENT REFCOUNT
    operator for each such reference.  If the depth first ordering includes
    blocks which are not part of the loop between nodes which are part of the
    loop, such inclusions are preceded by PUSH AND DECREMENT operators, and
    followed by POP REFERENCE operators.  We know in advance that the flow
    graph is reducible.  *)
procedure do_reference_allocation;
var
    loop_set: svector;
    pred_set: svector;
    ff: index_vector;
    m: index_range;
    marked: svector;
    ref_lists: ref_c_vector;
    loop_head: index_vector;
    loop_depth: index_range;
(*  NoteReference is called with a tuple which is referred to in some loop.
    If the tuple is not defined in that loop, then NoteReference adds the
    tuple to the reference tree for the outermost loop in which the tuple
    is not defined.  *)
procedure note_reference ( t: tuple );
  function add_ref ( thread: ref_c_tree ): ref_c_tree;
  begin
    new (add_ref);
    with add_ref^ do begin
      tpl := t;
      ref_count := 1;
      r_terminal := true;
      left := nil;
      right := thread;
    end;
  end (* add_ref *);
var
    tree: ref_c_tree;
    i: index_range;
begin
  if (t = nil) orif (t^.nodeid > block_labels^[loop_head^[loop_depth]]^.nodeid) then
    return;
  i := 1;
  while t^.nodeid > block_labels^[loop_head^[i]]^.nodeid do
    i := i + 1;
  tree := ref_lists^ [loop_head^ [i]];
  if tree = nil then begin
    ref_lists^ [loop_head^ [i]] := add_ref (nil);
    return;
  end;
  loop
    with tree^ do begin
      if tpl = t then begin
        ref_count := ref_count + 1;
        return;
      end;
      if tpl^.nodeid > t^.nodeid then begin
        if left = nil then begin
          left := add_ref (tree);
          return;
        end
        else
          tree := left;
      end
      else begin
        if r_terminal then begin
          right := add_ref (right);
          r_terminal := false;
          return;
        end
        else
          tree := right;
      end;
    end (* with tree^ *);
  end (* loop *);
end (* note_reference *);
(*  ProcessLoop finds all references in a specified loop to tuples which are
    not in the loop, and collects them in a reference tree.  *)
procedure process_loop ( loop_number: index_range );
var
    i: index_range;
    t: tuple;
    ind: index_range;
    tr: ref_c_tree;
begin
  loop_depth := loop_depth + 1;
  loop_head^ [loop_depth] := loop_number;
  i := loop_number;
  while not is_empty (loop_set, loop_number) do begin
    if in_set (loop_set, loop_number, i) andif not in_set (marked, 0, i) then
      if (i = loop_number) or is_empty (loop_set, i) then begin
        t := block_labels^[i]^.next;
        while (t^.opcode <> label_node) and (t^.opcode <> end_block) do begin
          with t^ do begin
            case opcode of
              start_with:
                note_reference (with_rec);
              jump_op,
              retjump_op,
              jump_t_op,
              jump_f_op,
              jump_in_op,
              gen_jump_op,
              case_jump_op:
                note_reference (cond);
              eval_op,
              assign_op:
                begin
                  note_reference (lhs);
                  note_reference (rhs);
                end;
              read_op:
                note_reference (rd_target);
              write_op:
                begin
                  note_reference (wr_value);
                  note_reference (wr_field_width);
                  if wr_value^.desc.kind = reals then
                    note_reference (wr_fract_width);
                end;
              nary_op..last_nary_op:
                for ind := 1 to upperbound (operand) do
                  note_reference (operand [ind]);
              call_op,
              func_call_op:
                begin
                  note_reference (subr);
                  for ind := 1 to upperbound (arglist) do
                    note_reference (arglist [ind]);
                end;
              mem_ref,
              addr_ref,
              immed_ref:
                begin
                  note_reference (item.base);
                  note_reference (item.index);
                end
            end (* case opcode *);
            t := next;
          end (* with t^ *);
        end;
      end
      else
        process_loop (i);
      add_elem (marked, 0, i);
      del_elem (loop_set, loop_number, i);
    i := i + 1;
  end (* while loop_set(loop_number) is not empty *);
  loop_depth := loop_depth - 1;
  if ref_lists^ [loop_number] <> nil then begin
    tr := ref_lists^ [loop_number];
    while tr^.left <> nil do
      tr := tr^.left;
    ref_lists^ [loop_number] := tr;
    while tr^.right <> nil do begin
      with tr^ do begin
        if not r_terminal then
          while right^.left <> nil do
            right := right^.left;
        tr := right;
      end;
    end;
  end;
end (* process_loop *);
(*  LoopSearch is called with a node N, which is known to be in the loop of
    node M.  If N is not already in LoopSet(M), then it is added to it, and
    all the incoming edges of node N are processed.  *)
procedure loop_search ( n: index_range );
var
    nn, n1, n2, n3: index_range;
    preds: tuple;
begin
  if in_set (loop_set, m, n) then  return;
  if ff^ [n] = 0 then begin (* N is not yet in any loop. *)
    add_elem (loop_set, m, n);
    preds := block_labels^[n]^.inward_jumps;
    while preds <> nil do begin
      nn := preds^.jump_from^.block_order_no;
      if nn >= m
        then loop_search (nn) (* Nn is also in the loop. *)
        else add_elem (pred_set, m, nn); (* Nn is a predecessor of this loop. *)
      preds := preds^.next_inward_jump;
    end;
    ff^ [n] := m;
  end
  (*  If node N is already in a loop, then that loop must be a subset of the
      loop of M.  Loop sets are maintained using the standard disjoint set
      UNION-FIND data structure with path compression.  *)
  else begin
    n1 := n; (* Find the loop containing N. *)
    n2 := ff^ [n];
    n3 := 0;
    while n2 <> n1 do begin
      ff^ [n1] := n3;
      n3 := n1;
      n1 := n2;
      n2 := ff^ [n2];
    end;
    union (loop_set, n1, m); (* N1 is the head of the loop containing N. *)
    union (pred_set, n1, m);
    for nn := m to n1 - 1 do (* Check each predecessor of the loop N1. *)
      if in_set (pred_set, n1, nn) then
        loop_search (nn);
    while n1 <> 0 do begin (* Add loop N1 to loop M. *)
      ff^ [n1] := m;
      n1 := n3;
      n3 := ff^ [n1];
    end;
  end (* n is in a loop *);
end (* loop_search *);
var i: index_range;
begin
  loop_set := new_svector (n_blocks, n_blocks); (* Compute the loop set of each basic block. *)
  pred_set := new_svector (n_blocks, n_blocks);
  new (ff, n_blocks);
  for m := n_blocks downto 2 do begin (* Note that block 1 is never a loop header. *)
    clr_set (loop_set, m);
    clr_set (pred_set, m);
    ff^ [m] := 0;
    if block_labels^[m]^.inward_jumps^.jump_from^.block_order_no >= m then
      loop_search (m); (* M is a loop header. *)
  end (* for m *);
  del_svector (pred_set);
  dispose (ff);
  if switch (cur_block^.dump_switches, 'LOOPS') then
    dmpset (loop_set, n_blocks, 'LOOPS HEADED BY BASIC BLOCKS IN BLOCK $');
  marked := new_svector (0, n_blocks);
  clr_set (marked, 0);
  new (ref_lists, n_blocks);
  for i := 1 to n_blocks do
    ref_lists^ [i] := nil;
  new (loop_head, n_blocks);
  loop_depth := 0;
  for i := 1 to n_blocks do
    if not in_set (marked, 0, i) andif not is_empty (loop_set, i) then
      process_loop (i);
  if switch (cur_block^.dump_switches, 'REFS') then
    dmprefcounts (ref_lists, n_blocks);
  del_svector (loop_set);
  dispose (loop_head);
  dispose (ref_lists);
end (* do_reference_allocation *);
*******************************************************************************)
var
    index: id_range;
    b: index_range;
    dom_last: id_range;
begin
  (*  First, clear the hash table.  *)
  for index := 0 to hash_limit do
    table [index] := nil;
  val_number := 0;
  (*  Now process the procedure prologue block (the display and parameter
      tuples that precede the first basic block).  *)
  low_red_blk (t_chain);
  (*  We process the basic blocks of the program in dominator tree pre-order.
      To do this, we use the non-recursive pre-order tree walk.  Remember the
      non-recursive pre-order tree walk?  *)
  b := 1;
  loop
    loop
      low_red_blk (block_labels^ [b]);
    exit if dom_son^ [b] = 0;
      b := dom_son^ [b];
    end;
    while (dom_brother^ [b] = 0) and (idom^ [b] <> 0) do
      b := idom^ [b];
  exit if dom_brother^ [b] = 0;
    b := dom_brother^ [b];
    dom_last := block_labels^[idom^[b]+1]^.nodeid;
    for index := 0 to hash_limit do
      while (table [index] <> nil) andif (table[index]^.nodeid > dom_last) do
        table [index] := table[index]^.result;
  end;
  reclaim;
(***  do_reference_allocation;***)
  (*  Dispose of the miscellaneous storage the optimizer has allocated and
      left lying around.  *)
  dispose (idom);
  dispose (dom_son);
  dispose (dom_brother);
  dispose (block_labels);
end (* low_reduce *);
(*  EstablishUsageContexts computes the usage context of each expression tuple
    in the intermediate form.  The usage context of an expression is "val",
    except for the following cases:
    1)  The left-hand side of an assignment statement, the target of a read
        statement, and the i/o string in a putstring call have a "mod" context.
    2)  Any subroutine call argument corresponding to a var parameter has a
        "var" context.
    3)  The record in a WITH statement, and the argument in an ADDR, UPPER-
        BOUND, LOWERBOUND, or DIMENSION function call, have a "ref" context.
    4)  In a component, field, or substring reference, the context of the base
        array, record, or string depends on the context of the reference as a
        whole.  If the reference expression context is "val", "mod", "var", or
        "ref", then the base context will be "val", "basemod", "basevar", or
        "ref", respectively.  The record in a WITH statement, however, never
        has any context other than "ref", regardless of the contexts of any
        references to fields within it.
                                                                        *)
procedure establish_usage_contexts;
(*  PushContext is called with an expression tuple and a context code.  It
    sets the context of the expression.  If the expression is an array
    component, a field, or a substring, then the operation is recursively
    applied to the base array, record, or string (unless the base already
    has a "ref" context, indicating that it is a record in a WITH statement),
    and the base context flag of the base structure is set to true.  *)
procedure push_context ( t: expr; t_context: usage_context );
var
    t1: expr;
    c: usage_context;
type
    bases = array [usage_context] of usage_context;
const
    base_context: bases = ( refx, valx, basemodx, basemodx, basevarx, basevarx );
begin
  if t = nil then return;
  t1 := t;
  c := t_context;
  repeat
    with t1^ do begin
      context := c;
      if opcode = field_ref then
        t1 := base_rec
      else if opcode = array_ref then
        t1 := base_array
      else if opcode = substr_ref then
        t1 := base_string
      else
        t1 := nil;
    end;
    c := base_context [c];
  until (t1 = nil) orif (t1^.context = refx);
end (* push_context *);
var
    t: tuple; (* Scans the IF chain. *)
    ind: int_type; (* Counts subr call arguments. *)
    parm: sym; (* Scans a parameter list. *)
begin
  t := t_chain;
  while t^.opcode <> end_block do
    with t^ do begin
       case opcode of
        start_with:
          push_context (with_rec, refx);
        assign_op:
          push_context (lhs, modx);
        out_str_op:
          push_context (operand[1], modx);
        read_op:
          push_context (rw_item, modx);
        call_op,
        func_call_op:
          for ind := 1 to upperbound (arglist) do
            if subr^.desc.base^.params[ind].parm_kind = vars then
              push_context (arglist [ind], varx);
        upb_op,
        lwb_op,
        dim_op,
        addr_op:
          push_context (operand [1], refx);
        others:
          (* no changes *)
      end (* case opcode *);
      if (first_expr <= opcode) and (opcode <= last_expr) then
        context := valx; (* Default, unless changed later. *)
      t := next;
    end (* with t^ *);
end (* establish_usage_contexts *);
(*  SINGLE USE OPERANDS performs a post_pass over the intermediate form,
    after it has been optimized, shaped, simplified, etc.  It finds all
    expression tuples which only are only referenced once, from a tuple
    in a different basic block, and moves them into the same basic block
    that they are referenced from.  Normally, such tuples are moved right
    after the LabelNode tuple starting the basic block they are put in.
    However, if the LabelNode is immediately followed by a GenAndifOp or
    a GenOrifOp, then the label node and the Gen op constitute a single
    unit, and the tuple is moved after it.  *)
procedure single_use_operands;
var
    t, t_label: tuple;
    i: int_type;
    node_number: int_type;
  (*  TEST MOVE looks at a tuple.  If the tuple is only used once, and it is
      not in the current basic block, then it will be spliced out of its
      current location in the I/F chain and reinserted following the label
      node of the current basic block.  *)
  procedure test_move ( t: tuple );
  begin
    if t = nil then
      return;
    with t^ do begin
      if not ((opcode = gen_andif_op) or (opcode = gen_orif_op)) andif
         (usage_count = 1) andif
         (nodeid < t_label^.nodeid) andif
         (not copy_tuple) then begin
        prev^.next := next;
        next^.prev := prev;
        next := t_label^.next;
        prev := t_label;
        t_label^.next^.prev := t;
        t_label^.next := t;
        nodeid := t_label^.nodeid + 1; (* Prevent subsequent moves *)
      end;
    end;
  end (* test_move *);
begin
  t := t_chain^.final_tuple;
  node_number := t^.nodeid;
  t_label := t_chain^.last_label;
  if ( t_label <> nil ) andif
     ( (t_label^.next^.opcode = gen_andif_op) or
       (t_label^.next^.opcode = gen_orif_op) ) then
    t_label := t_label^.next;
  while t_label <> nil do begin
    with t^ do begin
      case opcode of
        label_node:
          begin
            t_label := t^.upward_thread;
            if ( t_label <> nil ) andif
               ( (t_label^.next^.opcode = gen_andif_op) or
                 (t_label^.next^.opcode = gen_orif_op) ) then
              t_label := t_label^.next;
          end;
        eval_op,
        assign_op:
          begin
            test_move (lhs);
            test_move (rhs);
          end;
        first_jump_op..last_jump_op:
          test_move (cond);
        goto_op:
          test_move (target_frame);
        dispose_op:
          test_move (dptrarg);
	signal_op, mask_op, unmask_op:
	  test_move (cond_parm);
        first_io_stmt..last_io_stmt:
          if not old_file then
            test_move (file_arg);
        read_op, write_op:
          begin
            if not rw_old_file then
              test_move (rw_file);
            test_move (rw_item);
            test_move (rw_width);
            test_move (rw_precision);
          end;
        seek_op:
          begin
            test_move (seek_file);
            test_move (seek_index);
          end;
        mem_ref, addr_ref, immed_ref:
          begin
            test_move (item.index);
            test_move (item.base);
          end;
        call_op, func_call_op:
          begin
            test_move (subr);
            for i := 1 to upperbound (arglist) do
              test_move (arglist[i]);
          end;
        first_nnary_op..last_nnary_op,
        subr_var_op, desc_ref, (* omit gen_andif_op and gen_orif_op *)
        first_sunary_op..last_sunary_op,
        first_snary_op..last_snary_op,
        first_chk_op..last_chk_op:
          for i := 1 to upperbound (operand) do
           test_move (operand[i]);
        others: (* may be safely ignored *)
      end (* case opcode *);
      nodeid := node_number;
      node_number := node_number - 1;
      t := prev;
    end (* with t^ *);
  end (* while t_label <> nil *);
  while t <> nil do begin
    t^.nodeid := node_number;
    node_number := node_number - 1;
    t := t^.prev;
  end;
end (* single_use_operands *);
(*  PrepCode is a driver routine which calls the other intermediate form
    preparation routines for each block of the module.  *)
procedure prep_code;
begin
  if optimize_opt in all_opts then
    do_summary_analysis;
  cur_block := lex_block;
  while cur_block <> nil do begin
    if cur_block^.kind in [program_blk, subr_blk] then begin
      rd_tuples;
      if switch (cur_block^.dump_switches, 'IFM0') then
        dmptuples ('ORIGINAL INTERMEDIATE FORM FOR BLOCK $');
      establish_usage_contexts;
      make_basic_blocks;
      if switch (cur_block^.dump_switches, 'IFM') then
        dmptuples ('INTERMEDIATE FORM FOR BLOCK $');
      if optimize_opt in cur_block^.semantic_options then begin
        optimize;
        if switch (cur_block^.dump_switches, 'OPT') then
          dmptuples ('OPTIMIZED INTERMEDIATE FORM FOR BLOCK $');
      end;
      if not switch (cur_block^.dump_switches, 'NOSHAPE') then begin
        shape;
        if switch (cur_block^.dump_switches, 'SHAPE') then
          dmptuples ('SHAPED INTERMEDIATE FORM FOR BLOCK $');
        if optimize_opt in prog_options.semantic_options then begin
          low_reduce;
          if switch (cur_block^.dump_switches, 'OPTSHAPE') then
            dmptuples ('REDUCED SHAPED INTERMEDIATE FORM FOR BLOCK $');
        end;
        single_use_operands;
      end;
      if switch (cur_block^.dump_switches, 'FINAL') then
        dmptuples ('FINAL INTERMEDIATE FORM FOR BLOCK $');
      wr_tuples;
    end;
    cur_block := cur_block^.lex_thread;
  end;
end (* prep_code *);
(* RUN_P3_OR_P4 saves the heap and, if the runoff flag is set, initiates
   the next pass.  PASS3 is run only if the error count is greater than
   zero or a list file is required; otherwise PASS4 is run. *)
procedure run_p3_or_p4;
var
  next: packed array [1..6] of char;
  temp_file: text;
begin
  finish := (max_severity = 0) or ((max_severity = 1) and prog_options.finish_opt);
  ch_close; (* Must know finish/nofinish decision. *)
  IF NOT wrpas(tempname ('PAS')) THEN STOP;
  if runoff <> 0 then begin
    if opts_listing or (err_count <> 0) then
      next := 'PASLST'
    else begin
      next := tmprefix || 'OCG';
      reset (temp_file, tempname ('ERR'));
      scratch (temp_file);
      reset (temp_file, tempname ('XRF'));
      scratch (temp_file);
    end;
    IF NOT runprg(next || prgm_dir (), 1) THEN BEGIN
      rewrite (tty);
      writeln (tty, '?Unable to run ', next)
    END
  end;
end (* run_p3_or_p4 *);
external var abort: co_routine;
const abort_stack_size = 100;
procedure abt_pass2;
begin
  detach;
  elf_close;
  IF NOT wrpas(tempname ('PAS')) THEN STOP;
  stop;
end;
var start_time: integer;
    segstuff: segrecd;
begin
  start_time := runtime;
  if not rdpas(tempname ('PAS'), true) then begin
    rewrite (tty);
    writeln (tty, '?Compiler temporary file PAS lost');
    stop;
  end;
  rewrite (tty);
  if finish then begin
    elf_open;
    tal_init;
    ch_open (true, true);
    abort := create (abt_pass2, abort_stack_size);
    prep_code; (* prepare for code generation *)
    elf_close;
    dmp_close;
  end;
  if prog_options.statistics_opt then begin
    seginfo (segstuff);
    writeln (tty, '[Pass 2: ', (runtime - start_time) / 1000.0:8:3, ' seconds, ',
                  (segstuff.lowlen+511) div 512:3, '+',
                  (segstuff.highlen+511) div 512:3, 'P]');
  end;
  run_p3_or_p4;
end (* pass2 *).
P|:yq