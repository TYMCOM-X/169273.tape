$TITLE PASCGR -- Pascal Call Graph Module

module pascgr;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S C G R                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This is the call graph module.  It  contains  routines
        to create and manipulate the call graph of a program.
     
     ENTRY POINTS:
     
        p_calls_q   is  a  routine which augments the call graph with
                    an entry indicating the  CALLS  relation  between
                    two specified blocks.
     
        rep_block   returns  the  representative  block  node  of  an
                    expression  which  denotes  a   subroutine.   The
                    representative  block  of  a  subroutine  in this
                    module is the actual block  for  the  subroutine.
                    The   representative   block   for  any  external
                    subroutine   is   the   external    block.    The
                    representative block for a subroutine variable or
                    parameter is the class block node whose  type  is
                    the type of the variable or parameter.
     
        prm_block   returns  the  block for the subroutine class of a
                    subroutine parameter symbol.
     
        fin_graph   will complete the call graph at the  end  of  the
                    first  pass, by adding links between the external
                    block,  public  subroutine  blocks,   and   class
                    blocks.  Quick  block  analysis is also performed
                    at this time.
     
     ---------------------------------------------------------------- *)

$PAGE declarations
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasutl.inc
$SYSTEM pasifu.inc
$SYSTEM pascmp.inc
$SYSTEM paserr.inc
$SYSTEM passw.inc

external procedure dmpcgraph;
$PAGE p_calls_q

(*  NewCall records the relation p CALLS q.  For each block p, all the nodes q
    such that p CALLS q are stored in a right-threaded binary tree, whose root
    is pointed to by the 'calls' field of the block node.  The ordering on the
    tree is:  p precedes q if level(p) > level(q), or if level(p) = level(q)
    and number(p) < number (q).  *)


public procedure p_calls_q ( p, q: blk );

  (*  ADD_CALL is a function which returns a new call tree node, with a
      specified right thread.  *)

  function add_call ( thread: call_link ): call_link;
  begin
    new (add_call);
    with add_call^ do begin
      called_subr := q;
      r_terminal := true;
      llink := nil;
      rlink := thread;
    end;
  end (* add_call *);

var tree: call_link;

begin
  tree := p^.calls;
  if tree = nil then begin
    p^.calls := add_call (nil);
    return;
  end;

  loop
    with tree^ do begin
      if called_subr = q then return;
      if (q^.level > called_subr^.level) orif
        ( (q^.level = called_subr^.level) andif
          (q^.number < called_subr^.number) ) then begin (* q precedes called_subr *)
            if llink = nil then begin
              llink := add_call (tree);
              return;
            end
            else
              tree := llink;
          end
          else begin (* called_subr precedes q *)
            if r_terminal then begin
              rlink := add_call (rlink);
              r_terminal := false;
              return;
            end
            else
              tree := rlink;
          end;
    end (* with tree^ *);
  end (* loop *);
end (* p_calls_q *);
$PAGE subr_class

(*  SubrClass takes a subroutine type node, and returns the block node for the
    subroutine class which includes the specified type.  If the 'class_block'
    field of the type node wasn't set previously, it will be now.  *)


public function subr_class ( t: typ ): blk;

begin
  subr_class := t^.class_block; (* Get the type subr_class. *)
  if subr_class = nil then begin (* The subr_class of this type isn't defined yet. *)
    subr_class := root_block^.peer; (* Is there a subr_class for a compatible type? *)
    while (subr_class <> nil) andif
      not equivtypes (subr_class^.class_type, t) do
        subr_class := subr_class^.peer;
    if subr_class = nil then begin (* No; we will have to define one. *)
      subr_class := new_blk (class_blk, nil);
      subr_class^.class_type := t;
    end;
    t^.class_block := subr_class;
  end (* subr_class = nil *);
end (* subr_class *);
$PAGE rep_block

(*  RepBlock takes an expression which denotes a subroutine.  It returns the
    block node which will represent that subroutine in the call graph.  For
    a subroutine in this module, the representative block is the subroutine
    block.  For any external subroutine, the representative is the external
    block.  For a subroutine variable or parameter, the representative block
    is the class block for the subroutine class of the particular variable
    or parameter.  *)


public function rep_block ( subr: expr ): blk;

begin
  with subr^ do begin
    if opcode = cst_ref then (* Ordinary subroutine. *)
      rep_block := cst_val.blkp

    else if (opcode = ident_ref) andif
      (id_sym^.kind = consts) andif
      (id_sym^.dcl_class = external_sc) then (* External subroutine. *)
        rep_block := ext_block

    else (* Must be a subroutine class. *)
      rep_block := subr_class (desc.base);
  end (* with subr^ *);
end (* rep_block *);
$PAGE fin_graph

(*  FinGraph finishes up the call graph between the first and second passes.
    It performs two distinct operations.  If any external procedures are
    defined, AddLinks adds call graph links to and from the external block.
    LexOrdering sets the global variable LexBlock, and the LexThread link
    in each block, to establish a block chain ordered by decreasing lexical
    level.  *)


public procedure fin_graph;
$PAGE add_links - in fin_graph

(*  AddLinks completes the call graph if an external block has been defined.
    It adds the following links to the call graph:

    -  <extern> calls every public procedure
    -  <extern> calls each subroutine class
    -  each subroutine class calls <extern>                             *)


procedure add_links;

var b: blk;

begin
  if ext_block = nil then
    return;

  b := root_block^.children^.children; (* Get the first top-level procedure. *)
  while b <> nil do (* Check each top-level procedure. *)
    with b^ do begin
      if (kind = subr_blk) andif subr_sym^.public_dcl then
        p_calls_q (ext_block, b); (* If it's public, <extern> calls it. *)
      b := peer;
  end;

  b := root_block^.peer; (* Get the first subroutine class. *)
  while b <> nil do (* Process each subroutine class. *)
    with b^ do begin
      p_calls_q (ext_block, b); (* <extern> calls every class. *)
      p_calls_q (b, ext_block); (* Every class calls <Extern>. *)
      b := peer;
    end;

end (* add_links *);
$PAGE lex_ordering - in fin_graph

(*  LexOrdering produces the LexThread chain, ordered by decreasing lexical
    level.  The start of the chain is in LexBlock.  *)


procedure lex_ordering;

var
    lex_lists: array [level_index] of blk;
    i: level_index;
    b: blk;

begin
  for i := 0 to max_level do
    lex_lists [i] := nil;

  (*  The following code is a non-recursive traversal of the lexical block
      tree.  When it is complete, all the blocks at a given level will have
      been chained together.  *)

  b := root_block;
  i := 0; (* I is always the level of b. *)
  loop
    loop (* Descend to the deepest descendant of b. *)
      b^.lex_thread := lex_lists [i]; (* First add b to its own thread. *)
      lex_lists [i] := b;
    exit if b^.children = nil;
      b := b^.children;
      i := i + 1;
    end;
    while (b^.peer = nil) and (b^.parent <> nil) do begin
      b := b^.parent; (* We want to find a brother or an uncle. *)
      i := i - 1;
    end;
  exit if b^.peer = nil; (* Have we reached the last block at level 0? *)
    b := b^.peer; (* No, so process a sibling. *)
  end;

  (*  Now add a link from the end of each chain to the start of the chain
      at the next higher level.  *)

  for i := max_level downto 1 do begin
    b := lex_lists [i];
    while b^.lex_thread <> nil do (* Find the end of the chain. *)
      b := b^.lex_thread;
    b^.lex_thread := lex_lists [i-1]; (* Chain it to the next list. *)
  end;

  lex_block := lex_lists [max_level];
end (* lex_ordering *);
$PAGE linearize_call_trees - in fin_graph

(*  LinearizeCallTrees modifies the Rlink's in the call trees for the blocks
    of the program, changing them from threaded trees into simple lists,
    linked on the Rlink field.  *)


procedure linearize_call_trees;

var
    b: blk;
    c: call_link;

begin
  b := lex_block;
  while b <> nil do begin
    with b^ do begin
      if calls <> nil then begin
        while calls^.llink <> nil do
          calls := calls^.llink;
        c := calls;
        while c^.rlink <> nil do begin
          with c^ do begin
            if not r_terminal then
              while rlink^.llink <> nil do
                rlink := rlink^.llink;
            c := rlink;
          end (* with c^ *);
        end (* while c^.rlink <> nil *);
      end (* if calls <> nil *);
      b := lex_thread;
    end (* with b^ *);
  end (* while b <> nil *);
end (* linearize_call_tree *);
$PAGE quick_blocks - in fin_graph

(*  QuickBlocks will perform the quick block computation for the program, given
    the call graph.  On return from this routine, the owner and call thread
    fields of each block will have been set.  If a block is inaccessible,
    then a warning message will be printed and the block will be its owner.
    See CIN-#1 for a discussion of quick blocks and a description of the
    algorithms that are used here.  The apparent level and maximum call level
    of each block are also computed.  *)


procedure quick_blocks;

type
    index_set = packed array [0..*] of boolean;

var
    blk_visited: ^ index_set; (* Set of blocks marked "visited". *)
    blk_ordered: ^ index_set; (* Set of completely processed blocks. *)
    thread: blk; (* The last block added to the call thread. *)


(*  Internal subroutines:

        order - performs the depth-first search algorithm
        find_bound_subrs - marks all the subroutines that are bound or
                assigned to subroutine parameters or variables
        non_quick - tests whether a block is a priori not a quick block
        assign_owners - sets the owner fields of all called subroutines
        compute_apparent_level - determines the number of static levels between
                                 each block and the root block, allowing for
                                 quick block effects
        compute_max_call_level - determines the maximum apparent level from
                                 which each subroutine is called
        find_uncalled_subrs - prints warnings for all uncalled subroutines  *)
$PAGE order - in quick_blocks - in fin_graph

(*  Order performs the depth-first search algorithm, starting with a specified
    node in the call graph.  *)


procedure order ( b: blk );

var
    c: call_link;
    csubr: blk;

begin
  with b^ do begin

    blk_visited^[number] := true;

    c := b^.calls;
    while c <> nil do begin
      csubr := c^.called_subr;
      if not blk_visited^[csubr^.number] then
        order (csubr) (* Not yet visited - process it now. *)
      else if not blk_ordered^[csubr^.number] then
        csubr^.recursive := true; (* Still pending - must be able to call itself. *)
      c := c^.rlink;
    end;

    downward_call_thread := thread;
    if thread <> nil then
      thread^.upward_call_thread := b;
    upward_call_thread := nil;
    thread := b;

    blk_ordered^[number] := true;

  end (* with b^ *);
end (* order *);
$PAGE find_uncalled_subrs - in quick_blocks - in fin_graph

(*  FindUncalledSubrs will find any subroutine blocks which have not yet been
    ordered.  Since such subroutines can never be called, a warning message
    will be printed.  Then, just to complete the depth first chain, they will
    be ordered anyway.  Thus, such blocks are ordered as though they were
    called from the root block.  FindUncalledSubrs uses essentially the same
    non-recursive preorder traversal as LexOrdering to scan the blocks.  *)


procedure find_uncalled_subrs;

var b: blk;

begin
  b := root_block;
  loop

    loop
      with b^ do begin
        if (kind = subr_blk)
          andif not blk_visited^[number] then begin
            err_print (err_never_called, declaration, '', 0);
            order (b);
          end;
    exit if children = nil;
        b := children;
      end (* with b^ *);
    end (* loop *);

    while (b^.peer = nil) and (b^.parent <> nil) do
      b := b^.parent;

  exit if b^.peer = nil;
    b := b^.peer;

  end (* loop *);
end (* find_uncalled_subrs *);
$PAGE find_bound_subrs - in quick_blocks - in fin_graph

(*  FindBoundSubrs uses the call graph to find all blocks which are "called"
    by subroutine class blocks.  A subroutine class calls a subroutine only
    if the subroutine is bound or assigned to a parameter or variable of that
    subroutine type.  *)


procedure find_bound_subrs;

var
    b: blk;
    c: call_link;

begin
  b := root_block^.peer; (* The first subroutine class. *)
  while b <> nil do begin
    c := b^.calls;
    while c <> nil do begin
      c^.called_subr^.class_bound := true;
      c := c^.rlink;
    end;
    b := b^.peer;
  end;
end (* find_bound_subrs *);
$PAGE non_quick - in quick_blocks - in fin_graph

(*  NonQuick tests whether a block is a priori not a quick block, by
    virtue of one of the following criteria:

        - this is a debug mode compilation
        - it is not a subroutine block
        - it is public
        - it is bound to a formal subroutine parameter, or assigned to a
	  subroutine variable
        - it contains exception handlers				*)


function non_quick ( b: blk ): boolean;

begin
  with b^ do
    non_quick :=
      not qbl_allowed orif
      prog_options.debug_opt orif
      (kind <> subr_blk) orif
      subr_sym^.public_dcl orif
      not (qblocks_opt in semantic_options) orif
      recursive orif
      class_bound orif
      (hndlr_depth <> 0);
end (* non_quick *);
$PAGE assign_owners - in quick_blocks - in fin_graph

(*  AssignOwners sets the owner field of each subroutine block on the downward
    call thread.  *)

procedure assign_owners;

var
    b: blk;
    c: call_link;

begin
  b := root_block;
  while b <> nil do begin
    if non_quick (b) or (b^.owner = nil) then
      b^.owner := b;
    c := b^.calls;
    while c <> nil do
      with c^.called_subr^ do begin
        if owner = nil then
          owner := b^.owner
        else if owner <> b^.owner then
          owner := c^.called_subr;
        c := c^.rlink;
      end (* while c <> nil *);
    b := b^.downward_call_thread;
  end (* while b <> nil *);
end (* assign_owners *);
$PAGE compute_apparent_level - in quick_blocks - in fin_graph

(* ComputeApparentLevel determines for each block the number of static display
   levels between the block and the root block, accounting for the presence of
   quick blocks and owner blocks (which may or may not be (direct) parents of
   the block.  Note that the computation is performed completely for each block.
   This is required because there is no ordering available which insures that
   a block is processed before both its parent and owner. *)

procedure compute_apparent_level;

 var
   b, refblk: blk;
   levels: level_index;

 begin
  b := root_block;
  while b <> nil do begin

    refblk := b;                                (* count number of levels from b to root *)
    levels := 0;
    if refblk^.kind <> class_blk then begin
      while refblk <> root_block do begin       (* search outward on owner chain *)
        if refblk <> refblk^.owner      (* then refblk is quick *)
          then refblk := refblk^.owner  (* ... and compute level of owner *)
          else begin                    (* real frame, count a level *)
            levels := levels + 1;
            refblk := refblk^.parent;   (* compute level of parent *)
          end;
      end;
    end;
    b^.apparent_level := levels;

    b := b^.downward_call_thread;
  end (* search of call thread *) ;
 end;
$PAGE compute_max_call_level - in quick_blocks - in fin_graph

(* ComputeMaxCallLevel determines for each block, the maximum level of a call to
   that block.  Apparent levels are used in this computation. *)

procedure compute_max_call_level;

 var
   b: blk;
   c: call_link;

 begin
  b := root_block;                      (* iterate over all blocks *)
  while b <> nil do begin

    c := b^.calls;
    while c <> nil do begin
      with c^.called_subr^ do   (* assume max_call_level initialized to 0 *)
        max_call_level := max (max_call_level, b^.apparent_level);
      c := c^.rlink;
    end (* search of called blocks *) ;

    b := b^.downward_call_thread;
  end;
 end;
$PAGE quick_blocks - main routine - in fin_graph

var i: index_range;

begin
  new (blk_visited, blk_number);
  new (blk_ordered, blk_number);
  for i := 1 to blk_number do begin
    blk_visited^[i] := false;
    blk_ordered^[i] := false;
  end;

  thread := nil;

  order (ext_block); (* Process all externally accessible blocks. *)

  order (root_block^.children); (* Process the main program. *)

  find_uncalled_subrs; (* Process any uncalled procedures. *)

  order (root_bl (* Process the root. *)

  dispose (blk_visited);
  dispose (blk_ordered);
  find_bound_subrs;

  assign_owners;
  compute_apparent_level;
  compute_max_call_level;
end (* quick_blocks *);
$PAGE fin_graph - main routine

begin
  lex_ordering;
  if max_severity <= 1 then begin
    add_links;
    linearize_call_trees;
    quick_blocks;
    if switch (prog_options.dump_switches, 'CGRAPH') then
      dmpcgraph;
  end;
end (* fin_graph *).
   J Sx