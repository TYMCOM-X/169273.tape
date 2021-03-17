$TITLE PASJMP
$options enable (6)
$LENGTH 42

module pasjmp;

(* To select quick-pass only routines, ENABLE (6) *)
$PAGE includes
$include pascal.inc
$include pasist.inc
$include paspt.typ
$include pasif.typ
$include pasifu.inc
$INCLUDE passet.inc
$INCLUDE pasopt.inc
$INCLUDE pascgr.inc
$INCLUDE passw.inc
(*$y6
$INCLUDE pa2dmp.inc
 *)
$PAGE next_action
(* NEXT ACTION finds the first imperative operator following a node.  Specifically,
   it ignores commentary actions, such as a start_stmt. *)

function next_action (node: tuple): tuple;
 begin
  next_action := node^.next;
  while (next_action <> nil) andif (next_action^.opcode = start_stmt)
    do next_action := next_action^.next;
 end;
$PAGE get_following_label
(* GET FOLLOWING LABEL looks for a label following a "node".  If one is there, then
   it is returned;  otherwise, a label is created and insert following the node. *)

function get_following_label ( node: tuple ): tuple;    (* returns the label node *)
 var lab: tuple;
 begin
  lab := next_action (node);
  if lab^.opcode <> label_node then begin;
    new (lab, label_node);
    ap_chain (node, lab);                       (* insert in tuple chain *)
    with lab^ do begin
      label_sym := nil;                         (* this is compiler generated label *)
      nodeid := 0;
      inward_jumps := nil;
      outward_jumps := nil;
    end;
  end;
  get_following_label := lab;
 end;
$PAGE append_jump
(* APPEND JUMP is used to add one of the jump operators to the intermediate
   form.  Its adds the jump after a specified node. *)

procedure append_jump
             (  after_node: tuple;              (* append after this node *)
                opc: tuple_opcodes;             (* jump opcode *)
                test: expr;                     (* jump condition, may be nil *)
                target: tuple       );          (* target label of jump *)

 var jump: tuple;
 begin
  new (jump, jump_op);                          (* jump_op form is same for all *)
  ap_chain (after_node, jump);          (* chain into tuple chain *)
  with jump^ do begin
    nodeid := 0;
    opcode := opc;                              (* select correct jump *)
    cond := test;
    jump_to := target;
    low_cntrl := 0; high_cntrl := 0;
    jump_from := nil;
    next_inward_jump := nil;
  end (* with *) ;
 end;
$PAGE remove_label, remove_jump
(* REMOVE LABEL deletes a label node ("lab") from the label list;  "t_chain" as
   always provides the hook for the start of the list. *)

procedure remove_label (lab: tuple);
 begin
  with lab^ do begin
    if upward_thread = nil
      then t_chain^.first_label := downward_thread
      else upward_thread^.downward_thread := downward_thread;
    if downward_thread <> nil
      then downward_thread^.upward_thread := upward_thread;
  end;
 end;



(* REMOVE JUMP deletes a jump operator ("jmp") from the inward_jump list of its
   target label. *)

procedure remove_jump (jmp: tuple);
 var lab, injmp, last_injmp: tuple;
 begin
  lab := jmp^.jump_to;
  injmp := lab^.inward_jumps;   (* search for jmp, and find preceding jump *)
  last_injmp := nil;
  while injmp <> jmp do begin
    if injmp = nil then return;         (* jmp not on inward jump list *)
    last_injmp := injmp;
    injmp := injmp^.next_inward_jump;
  end;
  if last_injmp = nil   (* remove it from chain *)
    then lab^.inward_jumps := jmp^.next_inward_jump     (* is start of chain *)
    else last_injmp^.next_inward_jump := jmp^.next_inward_jump;
 end;
(*$y6   (* Not quick pass *)
$PAGE delete_unused_operators, delete_basic_block
(* DELETE UNUSED OPERATORS is called to delete a list of operators which can never
   be reached during execution.  It deletes all nodes from a specified "node" up
   to, but not including, the following label node (which may be the initial 
   node) or end block node. *)

procedure delete_unused_operators ( node: tuple );
 var tnode, next: tuple;
 begin
  tnode := node;
  while (tnode^.opcode <> label_node) and (tnode^.opcode <> end_block) do begin
    next := tnode^.next;
    if not is_expr (tnode) then begin           (* expr's are deleted by garbage collection *)
      if (jump_op <= tnode^.opcode) and (tnode^.opcode <= case_jump_op)
        then remove_jump (tnode);       (* take care to remove jumps from inward jump lists *)
      dechain (tnode);
    end;
    tnode := next;
  end;
 end;



(* DELETE BASIC BLOCK deletes a basic block.  "Node" is the first node of the block
   and is assumed to be a label node. *)

procedure delete_basic_block ( node: tuple );
 begin
  delete_unused_operators (node^.next);
  dechain (node);
 end;
 *)
$PAGE boolean_jump
(* BOOLEAN JUMP transforms a boolean operator into the equivalent jump form. *)

procedure boolean_jump
      ( test: expr;                     (* the boolean operator *)
        tloc: tuple;                    (* where to go if the condition is true *)
        floc: tuple;                    (* where to go if the condition is false *)
        continue: boolean  );           (* true => falling through leaves control at the
                                           tloc location;  false => floc *)

 var lab: tuple;                        (* inserted label node *)

 begin
  with test^ do begin
    case opcode of

      bnot_op:
        begin
          boolean_jump (operand[1], floc, tloc, not continue);
          dechain (test);
        end;

      and_op, andif_op:
        begin
          lab := get_following_label (operand[1]);      (* evaluation of op1 continues at op2 *)
          boolean_jump (operand[1], lab, floc, true);
          boolean_jump (operand[2], tloc, floc, continue);
          dechain (test);
        end;

      or_op, orif_op:
        begin
          lab := get_following_label (operand[1]);
          boolean_jump (operand[1], tloc, lab, false);
          boolean_jump (operand[2], tloc, floc, continue);
          dechain (test);
        end;

      others:
        begin
          if continue
            then append_jump (test, jump_f_op, test, floc)
            else append_jump (test, jump_t_op, test, tloc);
        end

    end (* case *) ;
  end (* with *) ;
 end;
$PAGE explode_booleans
(* EXPLODE BOOLEANS scans the intermediate form and explodes boolean operators
   into jump form.  There is a strong assumption here, that this processes the
   IF before optimization or shaping has occurred;  this means that operands are
   expected to appear in a regular order.

   A backward scan of the intermediate form is performed into to recognize the
   context in which the operators are used.  In the case where it is used as the
   operand of a jump, it is transformed into condtional jumps to the target and
   continuation of the referencing jump.  

   In the case where the operator yields a boolean result, the action depends on
   the operator.  "And_op" and "or_op" are left as is.  "Andif_op" and "orif_op"
   nodes are expanded into a special form which has the properties of ordering
   the evaluation, and causing generation of a boolean value:

      A andif B    ->        jumpf     A,1
                             jumpf     B,1
                             gen_jump  1
                          1: gen_andif A,B

   The gen_jump operator is interpreted as meaning "load one and jump a location
   following the gen_andif operator";  the gen_andif operator, "load zero and
   continue".  "Orif" is processed in a similar way, but with the jump conditions
   and truth values inverted.  The "gen_" prefix operators differ from the normal
   operators in that they do not constitute a use of their operands; this keeps
   the usage count information accurate.  Their operands are kept for basically
   documentary purposes; and since any boolean operands will be replaced with
   jump logic, they are changed to nil here. *)

procedure explode_booleans;

 var
   node: tuple;                 (* the scanning cursor *)
   lab: tuple;                  (* a generated label *)
   anchor: tuple;               (* node following next node on subsequent iterations *)

 type
   op_set = set of or_op .. bnot_op;

 const
   bool_ops: op_set = [or_op, and_op, orif_op, andif_op, bnot_op];

 begin
  node := t_chain^.final_tuple; (* get end of tuple list *)

  while node <> nil do begin    (* make a backwards scan over the IF *)
    with node^ do begin
      anchor := node;
      case opcode of

        jump_t_op, jump_f_op:
          begin
            if cond^.opcode in bool_ops then begin      (* explode and, or, not *)
             lab := get_following_label (node);         (* get alternative location *)
             if opcode = jump_t_op
               then boolean_jump (cond, jump_to, lab, false)
               else boolean_jump (cond, lab, jump_to, true);
             anchor := node^.next;      (* to keep our place as node is deleted *)
             remove_jump (node);        (* delete the original jump *)
             dechain (node);
            end;        (* boolean jump deletes extraneous op's *)
          end;

        andif_op:
          begin
            opcode := gen_andif_op;                     (* tag with special opcode to merge true/false values *)
            lab := get_following_label (prev);  (* insert label before gen_andif_op *)
            append_jump (operand[2], gen_jump_op, nil, lab);    (* these two are in reverse order of appearance in if *)
            append_jump (operand[2], jump_f_op, operand[2], lab);
            append_jump (operand[1], jump_f_op, operand[1], lab);       (* short-circuit test *)
            if operand[1]^.opcode in bool_ops then
              operand[1] := nil;
            if operand[2]^.opcode in bool_ops then
              operand[2] := nil;
          end;  (* subsequent passes will explode operands *)

        orif_op:
          begin
            opcode := gen_orif_op;                      (* tag with special opcode to merge true/false values *)
            lab := get_following_label (prev);  (* insert label before gen_orif_op *)
            append_jump (operand[2], gen_jump_op, nil, lab);    (* these two are in reverse order of appearance in if *)
            append_jump (operand[2], jump_t_op, operand[2], lab);
            append_jump (operand[1], jump_t_op, operand[1], lab);       (* short-circuit test *)
            if operand[1]^.opcode in bool_ops then
              operand[1] := nil;
            if operand[2]^.opcode in bool_ops then
              operand[2] := nil;
          end

      end (* case *) ;
    end (* with *) ;
    node := anchor^.prev;
  end (* while *) ;
(*Y6  if switch (cur_block^.dump_switches, 'BJMP')
    then dmptuples ('BLOCK $ AFTER BOOLEAN EXPLOSION'); *)
 end;
$PAGE complete_labelling

(* There are two versions of COMPLETE LABELLING. The quick-pass version
   assures that conditional jumps are paired. *)

(*$x6
procedure complete_labelling;

var 
  node, target: tuple;
  jump_opcode: tuple_opcodes;
  lnumber: 0..4095;     (* label renumbering *)

begin
  (* Start the body with a label. *)
  t_chain^.first_label := get_following_label (t_chain);

  (* Locate conditional jumps. *)

  node := t_chain^.first_label;
  lnumber := 0;
  while node^.opcode <> end_block do begin
    case node^.opcode of
      jump_t_op, jump_f_op: begin
        if node^.opcode = jump_t_op
          then jump_opcode := jump_f_op
          else jump_opcode := jump_t_op;
        if node^.next^.opcode <> jump_opcode then begin
          target := get_following_label (node);
          append_jump (node, jump_opcode, node^.cond, target);
        end;
        node := node^.next;     (* skip paired jump *)
      end;
      label_node: begin
        lnumber := lnumber + 1;
        node^.block_order_no := lnumber;
      end
    end;
    node := node^.next;
  end (* while *);
end (* complete_labelling *);   *)

(*$y6
(* COMPLETE LABELLING insures that the tuple list for a program is completely
   labeled.   Specifically:  (1) Every basic block begins with a label node.
   (2) Every basic block ends with some form of terminating operator.  (3) Unused
   nodes have been removed, though there may be unexecutable basic blocks.
   (4) All labels have been threaded together.  (5) The outward jump lists of
   all labels are complete.  This routine inserts and deletes nodes as needed to
   realize these properties. *)


procedure complete_labelling;

 var
   lab: tuple;                  (* in scan, label node starting basic block;
                                   nil'ed when block terminated. *)
   last_lab: tuple;             (* label node starting preceding basic block;
                                   lasts past end of that block *)
   node: tuple;                 (* current node being scanned *)
   target: tuple;
   call_node: tuple;
   jump_opcode: tuple_opcodes;
   labsym: sym;                 (* for searching label list *)
   opt: boolean; (* true if optimization in effect *)


 (* LOOKUP scans the list of tuples for the node corresponding to a particular
    user defined label.  The label is indicated by its symbol node "name"; the
    label tuple is returned. *)

 function lookup (name: sym): tuple;
  begin
   lookup := t_chain;
   while not ((lookup^.opcode = label_node) andif (lookup^.label_sym = name))
     do lookup := lookup^.next;
  end;



 begin

  opt := (optimize_opt in all_opts);

  (* A label is needed at the start of the program.  If there is not one there
     already, create one. *)

  t_chain^.first_label := get_following_label (t_chain);
                                                (* create if not found *)

  (* Scan the list of operators, completing the labeling as described above. *)

  lab := nil;                           (* no label seen yet *)
  last_lab := nil;
  node := t_chain^.first_label;

  while node^.opcode <> end_block do begin
    case node^.opcode of

      (* Label: this is the start of a basic block.  It is necessary to insure
         that the label is preceded by a jump (i.e., that the preceding block
         has been terminated).  In addition, if a label node is preceded by a
         start statement node, we exchange them, so that the start statement
         node follows the label. *)

      label_node:
        begin
          if node^.prev^.opcode = start_stmt then begin
            with node^ do begin
              prev^.next := next;
              next^.prev := prev;
              next := prev;
              prev := prev^.prev;
              next^.prev := node;
              prev^.next := node;
            end;
          end;
          if lab <> nil then begin      (* last block has no jump *)
            append_jump (node^.prev, jump_op, nil, node);
            lab^.outward_jumps := node^.prev (* jump *) ;
          end;
          node^.upward_thread := last_lab;      (* build label thread *)
          node^.downward_thread := nil;
          if last_lab <> nil
            then last_lab^.downward_thread := node;
          lab := node;                  (* remember node starting basic block *)
          last_lab := node;
        end;

      (* Jumps terminate basic blocks.  Build the outward jump list of the block.
         Close the basic block by nil'ing "lab" -- this prevents insertion of 
         jumps, and enables deletion of code which cannot be reached. *)

      jump_op, gen_jump_op:
        begin
          lab^.outward_jumps := node;
          lab := nil;
          delete_unused_operators (node^.next);         (* following can never be executed *)
        end;

      (* Case jumps:  semantication has already produced a fully connected graph
         with no extraneous operators. *)

      case_jump_op:
        begin
          lab^.outward_jumps := node;
          lab := nil;
        end;

      (* Conditional jumps must come in pairs to indicate the alternative ways
         of exiting the block (jump condition true or false).  Semantication only
         creates the first one; add the other if not present.  There can be no
         unused following nodes. *)

      jump_t_op, jump_f_op:
        begin
          lab^.outward_jump := node;
          lab := nil;
          if node^.opcode = jump_t_op           (* get reverse operator *)
            then jump_opcode := jump_f_op
            else jump_opcode := jump_t_op;
          if node^.next^.opcode <> jump_opcode then begin       (* not paired, create extra jump *)
            target := get_following_label (node);       (* must known where to jump to *)
            append_jump (node, jump_opcode, node^.cond, target);
          end;
          node := node^.next;                   (* skip the paired jump *)
        end;

      (* Block exits:  these terminate the subroutine/program in which they
         appear, and thereby the block in which they appear.  Note that goto_op's
         only appear in the IF to indicate a nonlocal goto. *)

      goto_op, stop_op, return_op, abort_op:
        begin
          lab^.outward_jumps := nil;            (* nil => terminal node *)
          lab := nil;
          delete_unused_operators (node^.next); (* following nodes cannot be reached *)
        end;

      (* Subroutine invocations may cause jumps via nonlocal goto's.  In order to
         properly analyze the graph, these edges are needed; therefore, the  special
         "retjump" operators are added, followed by a jump to complete the block, if
         any nonlocal gotos may be performed (directly or indirectly) by the 
         subroutine called.  If summary data flow analysis has been performed,
         then label symbol L will be in SUSE(P) if a call to P can result in a goto
         to L.  Otherwise, we must assume that any subroutine may go to any
         label which is the target of some nonlocal goto. *)

      call_op, func_call_op, signal_op, func_signal_op:
        begin
          call_node := node;    (* place holder *)
          if node^.next^.opcode <>ump_op then begin        (* not already processed *)
            if opt then
              effects (node); (* Compute the side effects of the call. *)
            labsym := cur_block^.label_list.first;              (* scan all labels with nonlocal refs *)
            while labsym <> nil do begin
              if labsym^.lab_nonlocal_use andif
                (not opt orif in_set (smod, 0, labsym^.id_number)) then begin
                  append_jump (node, retjump_op, nil, lookup (labsym));
                  node := node^.next;                   (* advance to the retjump just emitted *)
                end;
              labsym := labsym^.next;
            end (* search *) ;
            if node^.opcode = retjump_op then begin     (* terminate block if retjumps added *)
              target := get_following_label (node);
              append_jump (node, jump_op, nil, target);
              node := node^.next;       (* point to jump_op *)
              lab^.outward_jumps := call_node^.next;    (* the first retjump *)
              lab := nil;
            end
          end
          else begin                    (* retjump's already inserted *)
            while node^.opcode <> jump_op       (* scan to terminating jump_op *)
              do node := node^.next;
            lab^.outward_jumps := call_node^.next;
            lab := nil;
          end;
        end

    end (* case *) ;
    node := node^.next;                         (* iteration step *)
  end (* while *) ;
 end;
  *)
(*$y6   (* Not quick pass code *)
$PAGE complete_graph
(* COMPLETE GRAPH creates the block edge linkages.  When called, it is assumed
   that all label nodes are linked together, that the outward jump lists are
   complete, and that the targets of jumps are filled in. *)

procedure complete_graph;
 var lab, jmp: tuple;

 begin
  (* Walk the list of labels and nil the inward_jump list.  This is to enable
     use of this routine on a previously labeled graph. *)

  lab := t_chain^.first_label;
  repeat                                (* there must be at least one label *)
    lab^.inward_jumps := nil;
    lab := lab^.downward_thread;
  until lab = nil;

  (* The steps required to complete the graph are to append each jump to the
     inward jump list of the target label, and to record the basic block
     (i.e. label) from which it exits. *)

  lab := t_chain^.first_label;          (* for each basic block ... *)
  repeat
    jmp := lab^.outward_jumps;          (* for each jump exiting the block *)
    while (jmp <> nil) do begin
      with jmp^ do begin
        jump_from := lab;               (* record block exited *)
        next_inward_jump := jump_to^.inward_jumps;      (* append to chain *)
        jump_to^.inward_jumps := jmp;
        if (next^.opcode < jump_op) or (next^.opcode > case_jump_op)
          then jmp := nil       (* go on to next jump (if any) *)
          else jmp := next;
      end;
    end (* while *) ;
    lab := lab^.downward_thread;
  until lab = nil;
 end;
$PAGE transfer_jumps
(* TRANSFER JUMPS transfers jumps targeting on a certain label ("lab") to some 
   other label ("new_lab").  This is used by simplify_jumps to handle jumps to
   jumps. *)

procedure transfer_jumps (lab: tuple; new_lab: tuple);
 var jmp, nextjmp: tuple;
 begin
  jmp := lab^.inward_jumps;             (* walk list of all jumps *)
  lab^.inward_jumps := nil;             (* routine should leave list empty *)
  while jmp <> nil do begin
    with jmp^ do begin
      jump_to := new_lab;                       (* point jump at new label *)
      nextjmp := next_inward_jump;                      (* do iteration step before updating chain *)
      next_inward_jump := new_lab^.inward_jumps;
      new_lab^.inward_jumps := jmp;
      jmp := nextjmp;
    end;
  end;
 end;
$PAGE simplify_graph
(* SIMPLIFY GRAPH removes jumps to jumps, by setting the initial jump to target
   on the final label, and deleting the superfluous basic block containing the
   label. *)

procedure simplify_graph;
 var lab, next_lab, jmp: tuple;
 begin
  lab := t_chain^.first_label;
  repeat
    next_lab := lab^.downward_thread;           (* record in case node deleted *)

    jmp := next_action (lab);                   (* check for lab: jump, stmt marks may intervene *)
    if (jmp <> nil) andif (jmp^.opcode = jump_op) then begin
      transfer_jumps (lab, jmp^.jump_to);       (* make incomming jumps point at outward label *)
      remove_label (lab);       (* slice out of label list *)
      delete_basic_block (lab); (* delete the block *)
    end;

    lab := next_lab;
  until lab = nil;
 end;
$PAGE order_graph
(* ORDER GRAPH performs a depth-first ordering of the basic blocks in a program.
   The ordering is indicated by the block number and the downward and upward
   threads.  Any blocks which are unreferenced after the ordering is performed
   are deleted. *)

procedure order_graph;


 (* SEARCH recursively walks the spanning tree of labels (i.e. basic blocks)
    and performs the depth-first ordering by building the threads.  The block
    number is used as a marker, and on initial entry all are assumed to be
    zero.  As all labels are initially on the chain of labels, the label is
    removed from the original chain, before being threaded according to the
    ordering.  Thus, any labels left on the original chain are unreferenced. *)

 var last_visited: tuple;               (* label last processed as we back out *)

 procedure search (lab: tuple);
  var jmp: tuple;
  begin
    with lab^ do begin
      block_order_no := 1;              (* flag this node as visited *)
      remove_label (lab);               (* unchain from original thread *)

      jmp := outward_jumps;
      while jmp <> nil do begin (* process all jmps *)
        if jmp^.jump_to^.block_order_no = 0     (* if unvisited *)
          then search (jmp^.jump_to);
        with jmp^ do    (* get next outward jump (if any) *)
          if (next^.opcode < jump_op) or (next^.opcode > case_jump_op)
            then jmp := nil
            else jmp := next;
      end;

      if last_visited <> nil            (* thread current node onto the chain *)
        then last_visited^.upward_thread := lab
        else t_chain^.last_label := lab;
      downward_thread := last_visited;
      upward_thread := nil;
      last_visited := lab;
    end;
  end;

 var lab, nextlab: tuple;
     number: 0..4095;

 begin
  (* Process the tree, ordering all nodes which may be reached from the start
     node. *)

  lab := t_chain^.first_label;
  while lab <> nil do begin             (* mark all nodes as unvisited *)
    lab^.block_order_no := 0;
    lab := lab^.downward_thread;
  end;
  last_visited := nil;                  (* new chain is initially nil *)
  search (t_chain^.first_label);

  (* Any blocks left on the original chain cannot be reached during execution,
     so they may be deleted. *)

  lab := t_chain^.first_label;
  while lab <> nil do begin
    nextlab := lab^.downward_thread;
    delete_basic_block (lab);
    lab := nextlab;
  end;

  (* Process the new chain, numbering the label nodes on it in order, and attach
     it to the start block node. *)

  t_chain^.first_label := last_visited;
  number := 1;
  lab := last_visited;
  while lab <> nil do begin
    lab^.block_order_no := number;
    number := number + 1;
    lab := lab^.downward_thread;
  end;
 end;
$PAGE order_tuples
(* ORDER TUPLES performs a final scan of the IF, rearranging next/prev links
   so that the order of the basic blocks in the next/prev tuple chain is the
   same as their order in the downward_thread label chain. *)

procedure order_tuples;
 var t, last_t, next_lab: tuple;
 begin
  t := t_chain;
  last_t := nil;
  next_lab := nil;
  loop
    if ( (t^.opcode = label_node) or (t^.opcode = end_block) ) and
      (next_lab <> nil) then begin
        last_t^.next := next_lab;
        next_lab^.prev := last_t;
      end;

  exit if t^.opcode = end_block;

    if t^.opcode = label_node then
      if t^.downward_thread = nil
        then next_lab := t_chain^.final_tuple
        else next_lab := t^.downward_thread;
    last_t := t;
    t := t^.next;
  end;
 end;
$PAGE insert_preheaders
(* INSERT PREHEADERS scans the I/F and inserts preheaders before the headers of
   all loops.  For our purposes, a loop header is any node with an impinging
   edge from a block with follows it in the ordering. *)

procedure insert_preheaders;

 var
   lab, next_lab: tuple;                (* for scanning the input *)
   looplab: tuple;                      (* after splitting the header, node which receives
                                           the backward edges. *)
   jmp, njmp, ljmp: tuple;                      (* for scanning inward jump lists *)
   ojmp: tuple; (* for scanning outward jump lists *)
   number: index_range;                 (* for renumbering basic blocks *)

 begin
  lab := t_chain^.first_label;          (* examine each label node *)
  number := 1;
  while lab <> nil do begin
    lab^.block_order_no := number;
    number := number + 1;
    next_lab := lab^.downward_thread;

    (* Transfer all back edges entering this basic block, to a newly created
       label node ("looplab").  Forward edges will remain targeted on "lab".
       Insert an uncondition jump between the nodes. *)

    looplab := nil;                     (* create when first back edge found *)
    jmp := lab^.inward_jumps;           (* scan inward jumps looking for back edges *)
    ljmp := nil;                        (* preceding forward edge *)
    while jmp <> nil do begin
      njmp := jmp^.next_inward_jump;

      if jmp^.jump_from^.block_order_no < lab^.block_order_no
        then ljmp := jmp        (* have a forward edge *)

      else begin                (* have a back edge *)
        if looplab = nil then begin     (* create if no previous back edges *)
          new (looplab, label_node);
          if t_chain^.last_label = lab then
            t_chain^.last_label := looplab;
          ap_chain (lab, looplab);
          with looplab^ do begin
            label_sym := nil;
            nodeid := 0;
            block_order_no := number;   (* assign it number after lab *)
            number := number + 1;       (* so next label gets correct number *)
            downward_thread := lab^.downward_thread;    (* splice into label list *)
            lab^.downward_thread := looplab;
            upward_thread := lab;
            outward_jumps := lab^.outward_jumps;        (* jumps follow new label *)
            append_jump (lab, jump_op, nil, looplab);   (* insert a jump between lab and looplab *)
            lab^.next^.jump_from := lab;
            lab^.outward_jumps := lab^.next;    (* next == jump just created *)
            inward_jumps := lab^.next;
            ojmp := outward_jumps; (* change jump_from of the outward jumps *)
            while ojmp <> nil do begin
              ojmp^.jump_from := looplab;
              if (ojmp^.next^.opcode < jump_op) or (ojmp^.next^.opcode > case_jump_op)
                then ojmp := nil
                else ojmp := ojmp^.next;
            end;
          end (* with *) ;
        end;

        if ljmp = nil                   (* transfer jmp to looplab *)
          then lab^.inward_jumps := njmp
          else ljmp^.next_inward_jump := njmp;
        jmp^.next_inward_jump := looplab^.inward_jumps;
        looplab^.inward_jumps := jmp;
        jmp^.jump_to := looplab;
      end;

      jmp := njmp;
    end;

    lab := next_lab;                    (* examine next label, skip the one inserted *)
  end;
 end;
 *)
$PAGE make_basic_blocks
(* MAKE BASIC BLOCKS processes the tuple list output from the first pass (that is,
   the labelling is assumed to be incomplete), and builds a complete, simplified,
   and ordered basic block graph. *)

public procedure make_basic_blocks;

 begin
  explode_booleans;
  complete_labelling;

(*$y6   (* pass 2 only code *)

  complete_graph;
  simplify_graph;
  order_graph;
  order_tuples;
  insert_preheaders;

  (* end pass 2 only code *)    *)

  reclaim;
end.
 D@M­