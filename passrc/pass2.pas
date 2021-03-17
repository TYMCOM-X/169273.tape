$WIDTH=100
$LENGTH=55
$TITLE PASS2.PAS, last modified 1/10/84, zw
PROGRAM pass2 OPTIONS STORAGE(4096), SPECIAL(WORD);
(*TYM-Pascal compiler -- high-level optimization, pass 2*)

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM PASENV.INC
$SYSTEM PASIST.INC
$SYSTEM PTMCON.INC
$SYSTEM PASFIL.INC
$SYSTEM PASPT.TYP
$SYSTEM PASIF.TYP
$SYSTEM PASTAL.INC
$SYSTEM PASERR.INC
$SYSTEM UTLSET.INC
$SYSTEM UTLSW.INC
$SYSTEM PASIFU.INC
$SYSTEM PASJMP.INC
$SYSTEM PASOPT.INC
$SYSTEM PTMSEL.INC
$SYSTEM PA2DMP.INC
$SYSTEM PASDMP.INC
$SYSTEM PA2XRF.INC
$SYSTEM INFPAC.INC

$PAGE establish_usage_contexts

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
$PAGE push_context - in establish_usage_contexts

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
$PAGE establish_usage_contexts - main routine

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
$PAGE single_use_operands

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

$PAGE prep_code

PROCEDURE prep_code;
(*driver routine which calls the other intermediate form
  preparation routines for each block of the module.  *)
BEGIN
  IF optimize_opt IN all_opts THEN do_summary_analysis;
  cur_block := lex_block;
  WHILE cur_block <> NIL DO BEGIN
    IF cur_block^.kind IN [program_blk, subr_blk] THEN BEGIN
      rd_tuples;
      IF sw(cur_block^.dump_switches, 'IFM0')
      THEN dmptuples('ORIGINAL INTERMEDIATE FORM FOR BLOCK $');
      establish_usage_contexts;
      make_basic_blocks;
      IF sw(cur_block^.dump_switches, 'IFM')
      THEN dmptuples('INTERMEDIATE FORM FOR BLOCK $');
      IF optimize_opt IN cur_block^.semantic_options THEN BEGIN
        optimize;
        IF sw(cur_block^.dump_switches, 'OPT')
	THEN dmptuples('OPTIMIZED INTERMEDIATE FORM FOR BLOCK $')
      END;
      IF NOT sw(cur_block^.dump_switches, 'NOSHAPE') THEN BEGIN
        shape;
        IF sw(cur_block^.dump_switches, 'SHAPE')
	THEN dmptuples('SHAPED INTERMEDIATE FORM FOR BLOCK $');
        IF optimize_opt IN prog_options.semantic_options THEN BEGIN
          low_reduce;
          IF sw(cur_block^.dump_switches, 'OPTSHAPE')
	  THEN dmptuples('REDUCED SHAPED INTERMEDIATE FORM FOR BLOCK $')
        END;
        single_use_operands
      END;
      IF sw(cur_block^.dump_switches, 'FINAL')
      THEN dmptuples('FINAL INTERMEDIATE FORM FOR BLOCK $');
      wr_tuples
    END;
    cur_block := cur_block^.lex_thread
  END
END;

$PAGE next_pass

PROCEDURE next_pass;
(*initiate the next pass*)
(*PASS3 is run only if the error count is greater than
  zero or a list file is required; otherwise PASS4 is run. *)
VAR next: STRING[6]; tmp: TEXT;
BEGIN
  finish := (max_severity IN [0, 1]) AND prog_options.finish_opt;
  ch_close; (* Must know finish/nofinish decision. *)
  IF opts_listing OR (err_count <> 0) THEN next := 'PASLST'
  ELSE BEGIN
    next := tmprefix || 'OCG';
    RESET(tmp, xrf_tmp); SCRATCH(tmp);
    RESET(tmp, err_tmp); SCRATCH(tmp)
  END;
  chain(next)
END;

$PAGE pass2 - main

VAR start_time: INTEGER; segstuff: segrecd; abt_rdy: BOOLEAN;

PROCEDURE abt_pass2;
BEGIN
  IF abt_rdy THEN BEGIN
    elf_close;
    IF NOT wrpas(pas_tmp) THEN ttymsg('?can not save environment');
    byebye('?compiler pass 2 aborted')
  END
  ELSE abt_rdy := TRUE
END;

BEGIN
  start_time := RUNTIME;
  unchain;
  IF finish THEN BEGIN
    elf_open; tal_init; ch_open(TRUE, TRUE);
    abort := abt_pass2; abt_rdy := FALSE; (*procedure to do actual abort*)
    prep_code; (*prepare for code generation*)
    elf_close; dmp_close
  END;
  IF prog_options.statistics_opt THEN BEGIN
    REWRITE(TTY); seginfo(segstuff);
    WRITE(TTY, '[Pass 2');
    WRITE(TTY, ', ', (RUNTIME - start_time) / 1000.0:8:3, ' seconds');
    WRITE(TTY, ', ', (segstuff.lowlen + 511) DIV 512:3, '+');
    WRITE(TTY, (segstuff.highlen + 511) DIV 512:3, 'pages');
    WRITELN(TTY, ']');
  END;
  next_pass
END.
