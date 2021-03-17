$TITLE P10STR - string code generation
$LENGTH 42

module p10dsc;
$PAGE includes
$include pascal.inc
$include pasist.inc
$INCLUDE ptmcon.inc
$include paspt.typ
$include pasif.typ
$include p10cg.typ
$include p10cgu.inc
$include p10opc.inc
$include p10exp.inc
$include p10gen.inc
$include p10cll.inc
$PAGE string expr types
(* For string operations, the shaping code will generate operands of several
   forms.  For the lhs of an assignment, only two are generated:

         mem_ref        for an unpacked character
         desc_ref       for a string or packed character

   For the rhs of an assignment and the other operators, any one of several forms
   may appear:

         desc_ref       for strings
         upc/lwc_op     for case translated strings and characters
         cat_op         for concatenations of the others
         func_call_op   for a function invocation, yielding either a character
                          or string expression.
         date_op        for a call to the Fortran date routine
         *others*       for an expression which yields a character value; note
                          that the node need not have type character (in the case
                          of the removal of a sclcvt operator).

   Note that the operands of a cat_op will not be another cat_op or a func_call_op
   which yields a string.

   The types below are introduced to indicate which subset of these operators
   various routines are prepared to deal with:

        strop           is used for a desc_ref or character (i.e. nonstring)
                          expression.
        strexpr         includes all string or character expressions.
                                                                                *)

type
  strop = expr;
  strexpr = expr;
$PAGE runtime operator tables

type
    mvop_table = array [ boolean, (* padded/unpadded move *)
                         f_format..r_format, (* F, X, or R destination *)
                         c_format..x_format, (* C, F, or X source *)
                         str_translation ] (* direct, uppercase, lowercase *)
                    of rt_symbol;

const cs_move: mvop_table =
  ( ( ( ( rt_cm_fc,     rt_cmu_fc,      rt_cml_fc ),
        ( rt_cm_ff,     rt_cmu_ff,      rt_cml_ff ),
        ( rt_cm_fx,     rt_cmu_fx,      rt_cml_fx ) ),
      ( ( rt_cm_xc,     rt_cmu_xc,      rt_cml_xc ),
        ( rt_cm_xf,     rt_cmu_xf,      rt_cml_xf ),
        ( rt_cm_xx,     rt_cmu_xx,      rt_cml_xx ) ),
      ( ( rt_cm_rc,     rt_cmu_rc,      rt_cml_rc ),
        ( rt_cm_rf,     rt_cmu_rf,      rt_cml_rf ),
        ( rt_cm_rx,     rt_cmu_rx,      rt_cml_rx ) ) ),

    ( ( ( rt_cmp_fc,    rt_cmpu_fc,     rt_cmpl_fc ),
        ( rt_cmp_ff,    rt_cmpu_ff,     rt_cmpl_ff ),
        ( rt_cmp_fx,    rt_cmpu_fx,     rt_cmpl_fx ) ),
      ( ( rt_cmp_xc,    rt_cmpu_xc,     rt_cmpl_xc ),
        ( rt_cmp_xf,    rt_cmpu_xf,     rt_cmpl_xf ),
        ( rt_cmp_xx,    rt_cmpu_xx,     rt_cmpl_xx ) ),
      ( ( rt_cmp_rc,    rt_cmpu_rc,     rt_cmpl_rc ),
        ( rt_cmp_rf,    rt_cmpu_rf,     rt_cmpl_rf ),
        ( rt_cmp_rx,    rt_cmpu_rx,     rt_cmpl_rx ) ) ) );
$PAGE runtime string concatenation operators
type
    catop_table = array [ boolean, (* padded/unpadded result *)
                          f_format..x_format, (* F or X destination *)
                          c_format..x_format, (* C, F, or X source *)
                          str_translation ] (* direct, uppercase, lowercase *)
                     of rt_symbol;

const cs_cat: catop_table =
  ( ( ( ( rt_cc_fc,     rt_ccu_fc,      rt_ccl_fc ),
        ( rt_cc_ff,     rt_ccu_ff,      rt_ccl_ff ),
        ( rt_cc_fx,     rt_ccu_fx,      rt_ccl_fx ) ),
      ( ( rt_cc_xc,     rt_ccu_xc,      rt_ccl_xc ),
        ( rt_cc_xf,     rt_ccu_xf,      rt_ccl_xf ),
        ( rt_cc_xx,     rt_ccu_xx,      rt_ccl_xx ) ) ),

    ( ( ( rt_ccp_fc,    rt_ccpu_fc,     rt_ccpl_fc ),
        ( rt_ccp_ff,    rt_ccpu_ff,     rt_ccpl_ff ),
        ( rt_ccp_fx,    rt_ccpu_fx,     rt_ccpl_fx ) ),
      ( ( rt_ccp_xc,    rt_ccpu_xc,     rt_ccpl_xc ),
        ( rt_ccp_xf,    rt_ccpu_xf,     rt_ccpl_xf ),
        ( rt_ccp_xx,    rt_ccpu_xx,     rt_ccpl_xx ) ) ) );
$PAGE runtime binary string operators
type
    binop_table = array [ c_format..x_format,
                          c_format..x_format ]
                    of rt_symbol;

const cs_compare: binop_table =
     (  rt_stop,        rt_cp_cf,       rt_cp_cx,       (* cc compare is scalar op *)
        rt_cp_fc,       rt_cp_ff,       rt_cp_fx,
        rt_cp_xc,       rt_cp_xf,       rt_cp_xx   );

const cs_index: binop_table :=
     (  rt_cix_cc,      rt_cix_cf,      rt_cix_cx,
        rt_cix_fc,      rt_cix_ff,      rt_cix_fx,
        rt_cix_xc,      rt_cix_xf,      rt_cix_xx   );
$PAGE implied_argument
(* IMPLIED ARGUMENT splits a string expression into an case translation operation
   and an untranslated string expression (usually a descriptor reference).  The
   untranslated node is returned as the function value, and the translation action
   as the "trans" parameter.  This is used to subsume case translation operations
   into the processing of other operators.

   The case conversion operators which are bypassed by this routine will never be
   prepared.  Therefore, if the "free_ops" flag is true, this routine will free
   all these bypassed operators. *)

function implied_argument ( exp: strexpr; var trans: str_translation; free_ops: boolean ): strexpr;
 begin
  implied_argument := exp;
  trans := no_trans;
  while (implied_argument^.opcode = upc_op) or (implied_argument^.opcode = lwc_op) do begin
    if trans = no_trans then begin      (* outermost translation applies *)
      if implied_argument^.opcode = upc_op
        then trans := upper_trans
        else trans := lower_trans;
    end;
    if free_ops then
      free (implied_argument);
    implied_argument := implied_argument^.operand[1];
  end;
 end;
$PAGE str_format
(* STR FORMAT determines the type of runtime argument descriptor which is to be
   used to address a string as denoted by a desc_ref ("str"). *)

function str_format ( str: strop ): str_desc_format;
 begin
  if (str^.opcode = desc_ref) andif (str^.operand[1]^.desc.int_prec = 36)
    then str_format := x_format (* if byte address string or target character *)
  else if str^.desc.kind <> strings
    then str_format := c_format                 (* operand is character (desc_ref or expression) *)
    else str_format := f_format;                (* aligned address + length *)
 end;
$PAGE pad_str
(*  PAD STR is a predicate which tests whether a padded move operator should be
    used for a string move to a specified destination string.  The answer is yes
    if the destination is a non-varying string.  *)

function pad_str ( str: expr ): boolean;

begin
  with str^.desc do
    pad_str := (kind = strings) andif (str_kind = nonvarying);
end;
$PAGE prep_string
(* PREP STRING prepares a string operand ("str") for reference.  "str" is either
   a desc_ref or an operator yielding a character value.  Note that by convention, a
   desc_ref node, itself, is only a place holder pointing to the address and
   length operands.  These are individually prepared for access.  The descriptor
   is never tagged with a value descriptor as it has no value, and thus never
   needs to be freed.  It is also assumed that there are no multi-use descriptors;
   therefore, the address and length operands have usage_counts reflecting their
   true usage. *)

procedure prep_string ( str: strop );
 begin
  with str^ do begin
    case str_format (str) of

      c_format:
        begin
          if opcode = desc_ref
            then prep_direct (anyreg, operand[1])       (* only the address is needed *)
            else prepare (anyreg, str);         (* mem_ref or other operator *)
        end;

      f_format:
        begin
          prep_direct (anyreg, operand[1]);
          prep_direct (anyreg, operand[2]);
        end;

      x_format:
        begin
          prepare (anyreg, operand[1]); (* will be "fetch"ed, not "argument" *)
          prep_direct (anyreg, operand[2]);
        end

    end;
  end (* with *) ;
 end;
$PAGE free_string
(* FREE STRING frees a string operand.  Specifically, it frees all nodes which
   may be referenced by prep_string above. *)

procedure free_string ( str: strop );
 begin
  if str^.opcode <> desc_ref
    then free (str)
    else begin
      free (str^.operand[1]);
      free (str^.operand[2]);   (* even if c_format, to keep usage count correct *)
    end;
 end;
$PAGE fetch_string
(* FETCH STRING generates a runtime addressing descriptor for a string operand.
   This consists of an address and length (for f_format and x_format) or just
   an address (for c_format).  Both addresses and lengths will be emitted by
   "gen_str_desc" as indirect words following the runtime call;  the runtime
   loads both with "movei r,@descword" so that the values can be, in effect,
   immediate.  The routine returns in "addr" and "len" the address to be placed
   in the indirect words.  In the case of c_format, len is tagged with an
   unallocated address to suppress generation of the descriptor word. *)

procedure fetch_string ( str: strop; var addr: addr_desc; var len: addr_desc );
 begin
  with str^ do begin
    case str_format (str) of

      c_format:
        begin
          if opcode = desc_ref
            then addr := argument (operand[1])
            else addr := fetch_direct (str, 36);
          mem_lock (addr);
          len := null_location;         (* only address required *)
        end;

      f_format:
        begin
          addr := argument (operand[1]);                (* load address of start of string *)
          mem_lock (addr);
          len := argument (operand[2]);
        end;

      x_format:
        begin
          addr := fetch (operand[1], 36);       (* get address of byte pointer *)
          mem_lock (addr);
          len := argument (operand[2]);
        end

    end (* case *) ;
  end (* with *) ;

  mem_lock (len);
 end;
$PAGE gen_str_desc, gen_str_coded
(* GEN STR DESC generates the argument descriptor for a string runtime operand
   given the address and length memory addresses.  These have been previously
   computed by fetch_string. *)

procedure gen_str_desc ( addr: addr_desc; len: addr_desc );
 begin
  gen_rm (arg, 0, addr);
  if len.reloc.kind <> unallocated              (* length is not needed for character *)
    then gen_rm (arg, 0, len);
  mem_unlock (addr);            (* values no longer required *)
  mem_unlock (len);
 end;



(* GEN STR CODED is the same as gen_str_desc, except that it allows the acc
   field of the first argument word to be specified. *)

procedure gen_str_coded ( code: registers; addr: addr_desc; len: addr_desc );
 begin
  gen_rm (arg, code, addr);
  if len.reloc.kind <> unallocated              (* length is not needed for character *)
    then gen_rm (arg, 0, len);
  mem_unlock (addr);            (* values no longer required *)
  mem_unlock (len);
 end;
$PAGE move_string
(* MOVE STRING generates code to move one string ("src") to another ("dest").
   A translation to be applied to the source string ("src_trans") may be
   specified. *)

procedure move_string ( dest: strop; src: strop; src_trans: str_translation );
 var addr1, addr2, len1, len2: addr_desc;
 begin
  prep_string (dest);                   (* compute addr and length from descriptors *)
  prep_string (src);

  clr_rv; (* runtime call kills the return value slot *)

  fetch_string (dest, addr1, len1);
  fetch_string (src, addr2, len2);

  gen_rt (pushj, sb, cs_move [ pad_str (dest), str_format (dest), str_format (src), src_trans ]);

  gen_str_desc (addr1, len1);
  gen_str_desc (addr2, len2);

  free_string (dest);
  free_string (src);
 end;
$PAGE cat_move_strings
(* CAT MOVE STRINGS generates code to move the concatenation of several strings
   ("src", a cat_op) to a target string ("dest").  A translation to be applied to
   each of the operands of the cat_op may be specified ("src_trans"). *)

procedure cat_move_strings ( dest: strop; src: strexpr;
                             src_trans: str_translation; recursive: boolean );

 var
   i: 0..4095;
   arg_i: strop;
   arg_trans: str_translation;
   addr1, addr2, len1, len2, slen: addr_desc;

 begin
  prep_string (dest);                           (* get address and length of operands *)

  with src^ do begin
    for i := 1 to upperbound (operand) do
      prep_string (implied_argument (operand[i], arg_trans, false));

    clr_rv; (* runtime calls kill the return value slot *)

    (* Move the first operand to the target string.  Only during this first move
       is the descriptor for the target specified.  When processing all subsequent
       moves, the target is referenced impliedly with an r_format operator.  If
       the assignment is recursive (i.e., the first operand is the same as the
       destination), then the initial move specifies the first two operands. *)

    if recursive
      then i := 2
      else i := 1;
    arg_i := implied_argument (operand[i], arg_trans, true);    (* get actual operand *)
    if src_trans <> no_trans then
      arg_trans := src_trans;
    fetch_string (dest, addr1, len1);
    fetch_string (arg_i, addr2, len2);
    if recursive then begin
      slen := argument (operand[1]^.operand[2]); (* The operand 1 length. *)
      gen_rt (pushj, sb, cs_cat [ (upperbound (operand) = 2) and pad_str (dest),
                                  str_format (dest), str_format (arg_i), arg_trans ]);
      gen_str_desc (addr1, len1);
      gen_rm (arg, 0, slen);
      mem_unlock (slen);
      gen_str_desc (addr2, len2);
      free_string (operand [1]);
    end
    else begin
      gen_rt (pushj, sb, cs_move [false, str_format (dest), str_format (arg_i), arg_trans]);
      gen_str_desc (addr1, len1);
      gen_str_desc (addr2, len2);
    end;
    free_string (dest);
    free_string (arg_i);

    (*  All but the last of the remaining operands are siimply moved to the
        remainder of the target string.  *)

    for i := i + 1 to upperbound (operand) - 1 do begin
      arg_i := implied_argument (operand[i], arg_trans, true);
      if src_trans <> no_trans then
        arg_trans := src_trans;
      fetch_string (arg_i, addr2, len2);
      gen_rt (pushj, sb, cs_move [false, r_format, str_format (arg_i), arg_trans]);
      gen_str_desc (addr2, len2);
      free_string (arg_i);
    end;

    (* The final operand is also moved to the remainder of the target string.
       However, if the target is a fixed (as opposed to a varying string),
       any remaining character positions after this move must be filled with
       blanks. *)

    if (upperbound (operand) > 2) or (not recursive) then begin
      arg_i := implied_argument (operand[upperbound (operand)], arg_trans, true);
      if src_trans <> no_trans then
        arg_trans := src_trans;
      fetch_string (arg_i, addr2, len2);
      gen_rt (pushj, sb, cs_move [pad_str (dest), r_format, str_format (arg_i), arg_trans]);
      gen_str_desc (addr2, len2);
      free_string (arg_i);
    end;

  end (* with *) ;
  free (src);
 end;
$PAGE move_str_fvalue
(* MOVE STR FVALUE invokes a string valued function ("src") and copies the result
   to a string temporary (denoted by the descriptor "dest"). *)

procedure move_str_fvalue ( dest: strop; src: expr );
 begin
  pas_call (src, dest^.operand[1]);             (* pass address of result *)
  free (src);
  free_string (dest);
 end;
$PAGE move_str_date
(* MOVE STR DATE calls the Fortran DATE subroutine, passing it the address of
   the destination string temporary.  (DateOp's are always assigned to a nine-
   character fixed-length string temporary, which simplifies life.)
   The following code is generated:

        PUSH    17,16           ; save the stack pointer
        PUSH    17,[-1,,0]      ; create the argument list
        PUSH    17,dest_addr
        MOVEI   16,0(17)        ; set the argument list pointer
        PUSHJ   17,DATE##       ; call the Fortran routine
        ADJSP   17,-2           ; pop the argument list
        POP     17,16           ; pop the old stack pointer

                                                                        *)

procedure move_str_date ( dest: strop; src: expr );
 const one_arg: val = (scalar_cst, -1000000b); (* [-1,,0] *)
 var r: registers;
 begin
  gen_rr (push, sb, sp);
  gen    (push, sb, 0, 0, gen_cval (one_arg));
  prepare (anyreg, dest^.operand[1]); (* prepare the destination address *)
  clr_rv; (* clear the return value slot *)
  r := load (anyreg, dest^.operand[1], 36);
  gen_rr (push, sb, r);
  gen_rx (movei, sp, sb);
  gen_rt (pushj, sb, rt_date);
  if prog_options.ki_code_opt then
    begin gen_rt(jsr,0,rt_inst_sml);
      gen_ri (adj_sp, sb, -2) 
    end
  else gen_ri (adjsp, sb, -2);
  gen_rr (pop, sb, sp);
  free (src);
  free_string (dest);
 end;
$PAGE str_index_op
(* STR INDEX OP generates code for an "index_op", and tags the expresion node
   with the register in which it has been loaded. *)

public procedure str_index_op ( treg: reg_selector; node: expr );

 var
   reg: registers;
   addr1, addr2, len1, len2: addr_desc;
   mem: addr_desc;
   signedp: data_alignment;

 begin
  with node^ do begin

    (* Fetch the operands. *)

    prep_string (operand[1]);
    prep_string (operand[2]);
    if upperbound (operand) = 3 then prepare (treg, operand[3]);

    clr_rv; (* runtime call kills the return value slot *)

    fetch_string (operand[1], addr1, len1);
    fetch_string (operand[2], addr2, len2);
    if upperbound (operand) = 3 then mem := fetch (operand[3], desc.int_prec);

    (* Call the index runtime routine.  This will return in register 1 the index
       of the first occurence of operand[2] in operand[1], or zero if the search
       fails. *)

    gen_rt (pushj, sb, cs_instr_format (operand[1]), str_format (operand[2])]);
    gen_str_desc (addr1, len1);
    gen_str_desc (addr2, len2);

    free_string (operand[1]);
    free_string (operand[2]);

    (* Move the result in register 1 to a register whose lifetime is more dependable.
       If there is a third operand, its value must be loaded into the final result
       register if the contents of register 1 is zero. *)

    if upperbound (operand) <> 3 then begin           (* just copy reg 1 *)
      reg := get_reg (treg, 36);
      gen_rr (move, reg, 1);
    end

    else begin                          (* have a third argument *)
      free (operand[3]);                (* discard resources required for operand[3] *)
      reg := get_reg (treg, 36);

      if operand[3]^.desc.signed        (* signed indicator needed below *)
        then signedp := signed_value
        else signedp := unsigned_value;

      if is_register (mem) and (mem.offset = reg)
        then begin
          gen (jump+eqc, 1, 0, 2, dot);
          gen_rr (move, reg, 1);
        end
      else if mem.index = reg
        then begin
          gen (jump+eqc, 1, 0, 2, dot);
          gen_rr (skip+awc, reg, 1);
          do_move (reg, mem, signedp, operand[3]^.desc.int_prec);
        end
        else begin
          gen_rr (skip+nec, reg, 1);
          do_move (reg, mem, signedp, operand[3]^.desc.int_prec);
        end;
    end;

    tag_reg (reg, node);                        (* record where the value is *)
  end (* with *) ;
 end;
$PAGE str_compare_op
(* STR COMPARE OP generates code for a "str_comp_op" and tags the expression
   node with the register in which it has been loaded. *)

public procedure str_compare_op ( treg: reg_selector; node: tuple );

 var
   reg: registers;
   addr1, addr2, len1, len2: addr_desc;

 begin
  with node^ do begin
    prep_string (operand[1]);
    prep_string (operand[2]);

    clr_rv; (* runtime call kills the return value slot *)

    fetch_string (operand[1], addr1, len1);
    fetch_string (operand[2], addr2, len2);

    gen_rt (pushj, sb, cs_compare [str_format (operand[1]), str_format (operand[2])]);

    gen_str_desc (addr1, len1);
    gen_str_desc (addr2, len2);

    free_string (operand[1]);
    free_string (operand[2]);
  end;

  (* The runtime leaves the result -- the "sign" of the comparision -- in register
     1.  Since this register is likely to die on us at any moment, we are forced
     to copy the value to another register if it has more than one remaining use;
     if it has only one use, we assume that the value will be immediately consumed
     by a string comparision operator (sle_op, etc.).  *)

  if node^.usage_count = 1
    then reg := 1
    else begin
      reg := get_reg (treg, 36);
      gen_rr (move, reg, 1);
    end;
  tag_reg (reg, node);
 end;
$PAGE str_assignment
(* STR ASSIGNMENT generates code for the assignment of strings and characters.
   Note that character to character assignments are handled as scalar assignments. *)

public procedure str_assignment ( node: tuple (* the assign_op *) );

 var
   src: expr;
   src_trans: str_translation;

 begin
  with node^ do begin
    src := implied_argument (rhs, src_trans, true);             (* strip translation operators *)
    case src^.opcode of

      cat_op:
        cat_move_strings (lhs, src, src_trans, lrecursive);

      func_call_op:
        move_str_fvalue (lhs, src);             (* src_trans = no_trans since all function calls
                                                   when args of upper/lowercase operators are first
                                                   copied to temps. *)

      date_op:
        move_str_date (lhs, src); (* likewise *)

      others:                           (* desc_ref, or character expression *)
        move_string (lhs, src, src_trans)

    end (* case *) ;
  end (* with *) ;
 end;
$PAGE prep_set
(* PREP SET prepares a set operand ("se") for reference.  "Se" may be either a
   desc_ref, or a gen_set_op (0, 1, or 2) args.  *)

procedure prep_set ( se: expr );
 begin
  with se^ do begin
    case opcode of

      desc_ref:
        begin
          prep_direct (anyreg, operand[1]);
          prep_direct (anyreg, operand[2]);
          prep_direct (anyreg, operand[3]);
        end;

      set_op:
        begin
          if upperbound (operand) > 0 then prepare (anyreg, operand[1]);
          if upperbound (operand) > 1 then prepare (anyreg, operand[2]);
        end;

      others:                           (* short set *)
        prepare (anyreg, se)

    end (* case *) ;
  end (* with *) ;
 end;
$PAGE free_set
(* FREE SET frees the values used to reference a set operand ("se"). *)

procedure free_set ( se: expr );
 begin
  with se^ do begin
    case se^.opcode of

      desc_ref:
        begin
          free (operand[1]);
          free (operand[2]);
          free (operand[3]);
        end;

      set_op:
        begin
          if upperbound (operand) > 0 then free (operand[1]);
          if upperbound (operand) > 1 then free (operand[2]);
        end;

      others:
        free (se)

    end;
  end;
 end;
$PAGE fetch_set
(* FETCH SET generates runtime descriptors for a set operand ("se").  There are
   four kinds of descriptors which may be generated:

     long fmt     arg 1:  addr (set)
                  arg 2:  immed (lwb)
                  arg 3:  immed (len)

     ones fmt     arg 1:  lwb           	for [e1..e2] or [e1]
                  arg 2:  upb or lwb

     zero fmt     no arguments                  for []

     short fmt    addr (set)                    for a short set

   The argument words are generated in the same manner as string address and length
   arguments. *)

type set_desc = (* for processing set argument words *)
        record
          nargs: index_range;   (* number of argument words *)
          arg: array[1..3] of addr_desc (* address to be placed in the words *)
        end;

procedure fetch_set ( se: expr; var dgroup: set_desc );
 begin
  with se^ do begin
    case opcode of

      desc_ref:
        begin
          dgroup.nargs := 3;
          dgroup.arg[1] := argument (operand[1]);
          mem_lock (dgroup.arg[1]);
          dgroup.arg[2] := argument (operand[2]);
          mem_lock (dgroup.arg[2]);
          dgroup.arg[3] := argument (operand[3]);
          mem_lock (dgroup.arg[3]);
        end;

      set_op:
        begin
          if upperbound (operand) = 0
            then dgroup.nargs := 0
          else begin
            dgroup.nargs := 2;
            dgroup.arg[1] := fetch_direct (operand[1], 36);
            mem_lock (dgroup.arg[1]);
            if upperbound (operand) = 1
              then dgroup.arg[2] := dgroup.arg[1]
              else dgroup.arg[2] := fetch_direct (operand[2], 36);
            mem_lock (dgroup.arg[2]);
          end;
        end;

      others:
        begin
          dgroup.nargs := 1;
          dgroup.arg[1] := fetch_direct (se, se^.desc.set_length);
          mem_lock (dgroup.arg[1]);
        end

    end;
  end (* with *) ;
 end;
$PAGE gen_set_desc
(* GEN SET DESC generates the argument descriptor for a set runtime operand
   given a "set_desc" structure given the descriptor words.  These have been
   previously computed by fetch_set. *)

procedure gen_set_desc ( dgroup: set_desc );
 var i: index_range;
 begin
  for i := 1 to dgroup.nargs do begin
    gen_rm (arg, 0, dgroup.arg[i]);
    mem_unlock (dgroup.arg[i]);
  end;
 end;
$PAGE search/verify operator tables

type sv_op_table = array [no_trans..upper_trans, c_format..x_format] of rt_symbol;

const
  srch_l: sv_op_table := ( rt_sr_cl,  rt_sr_fl,  rt_sr_xl,
                           rt_sru_cl, rt_sru_fl, rt_sru_xl );
  srch_o: sv_op_table := ( rt_sr_co,  rt_sr_fo,  rt_sr_xo,
                           rt_sru_co, rt_sru_fo, rt_sru_xo );
  srch_d: sv_op_table := ( rt_sr_cd,  rt_sr_fd,  rt_sr_xd,
                           rt_sru_cd, rt_sru_fd, rt_sru_xd );
  vrfy_l: sv_op_table := ( rt_vf_cl,  rt_vf_fl,  rt_vf_xl,
                           rt_vfu_cl, rt_vfu_fl, rt_vfu_xl );
  vrfy_o: sv_op_table := ( rt_vf_co,  rt_vf_fo,  rt_vf_xo,
                           rt_vfu_co, rt_vfu_fo, rt_vfu_xo );
  vrfy_d: sv_op_table := ( rt_vf_cd,  rt_vf_fd,  rt_vf_xd,
                           rt_vfu_cd, rt_vfu_fd, rt_vfu_xd );
$PAGE sv_generate
(* SV GENERATE generates code for a search or verify operator ("node").  The
   "op_table" denotes the various runtime routines to use for different kinds
   of string arguments.  The string argument may be either a descriptor reference,
   a character expression, or an uppercased descriptor reference.  The set argument
   may be either a descriptor reference, a short set expression (over the range
   ' '..char (ord (' ')+71)), or a gen_set_op with one or two arguments.  "Treg"
   gives a register preference, and the result is tagged. *)

procedure sv_generate ( op_table: sv_op_table; treg: reg_selector; node: expr );

 var
   op1: expr;
   reg: registers;
   addr1, len1: addr_desc;
   arg2desc: set_desc;
   mem: addr_desc;
   signedp: data_alignment;
   str_trans: str_translation;

 begin
  with node^ do begin
   op1 := implied_argument (operand[1], str_trans, true);       (* strip possible uppercase op *)

    (* Fetch the operands. *)

    prep_string (op1);
    prep_set (operand[2]);
    if upperbound (operand) = 3 then prepare (treg, operand[3]);

    clr_rv; (* runtime call kills the return value slot *)

    fetch_string (op1, addr1, len1);
    fetch_set (operand[2], arg2desc);
    if upperbound (operand) = 3 then mem := fetch (operand[3], desc.int_prec);

    (* Call the runtime routine.  This will return in register 1 then result
       defined for the 2 operand form of search or verify. *)

    gen_rt (pushj, sb, op_table [str_trans, str_format (op1)]);
    gen_str_desc (addr1, len1);
    gen_set_desc (arg2desc);

    free_string (op1);
    free_set (operand[2]);

    (* Move the result in register 1 to a register whose lifetime is more dependable.
       If there is a third operand, its value must be loaded into the final result
       register if the contents of register 0 is zero. *)

    if upperbound (operand) <> 3 then begin           (* just copy reg 1 *)
      reg := get_reg (treg, 36);
      gen_rr (move, reg, 1);
    end

    else begin                          (* have a third argument *)
      free (operand[3]);                (* discard resources required for operand[3] *)
      reg := get_reg (treg, 36);

      if operand[3]^.desc.signed        (* signed indicator needed below *)
        then signedp := signed_value
        else signedp := unsigned_value;

      if is_register (mem) and (mem.offset = reg)
        then begin
          gen (jump+eqc, 1, 0, 2, dot);
          gen_rr (move, reg, 1);
        end
      else if mem.index = reg
        then begin
          gen (jump+eqc, 1, 0, 2, dot);
          gen_rr (skip+awc, reg, 1);
          do_move (reg, mem, signedp, operand[3]^.desc.int_prec);
        end
        else begin
          gen_rr (skip+nec, reg, 1);
          do_move (reg, mem, signedp, operand[3]^.desc.int_prec);
        end;
    end;

    tag_reg (reg, node);                        (* record where the value is *)
  end (* with *) ;
 end;
$PAGE str_search_op, str_verify_op
(* STR SEARCH OP generates code for a "search_op", and tags the expression
   "node" with the register in which it has been loaded.  "Treg" is a register
   preference.  *)

public procedure str_search_op ( treg: reg_selector; node: tuple );
 begin
  if node^.operand[2]^.opcode = desc_ref
    then sv_generate (srch_l, treg, node)
  else if node^.operand[2]^.opcode = set_op
    then sv_generate (srch_o, treg, node)
  else sv_generate (srch_d, treg, node);
 end;


(* STR VERIFY OP generates code for a "verify_op".  Its usage is the same as
   for the above. *)
public procedure str_verify_op ( treg: reg_selector; node: tuple );
 begin
  if node^.operand[2]^.opcode = desc_ref
    then sv_generate (vrfy_l, treg, node)
  else if node^.operand[2]^.opcode = set_op
    then sv_generate (vrfy_o, treg, node)
  else sv_generate (vrfy_d, treg, node);
 end;
$PAGE ls_operator
(* LS OPERATOR generates a two operand, set runtime call.  "Op" denotes the
   runtime routine to call;  "arg1" and "arg2" are the operands, and are freed
   upon return.    "Treg" is the place the caller wants the boolean result
   left, and should be noreg for those runtime routines that don't return such results. *)

public procedure ls_operator (op: rt_symbol; arg1, arg2: expr; treg: registers);
 var arg1desc, arg2desc: set_desc;
 begin
  prep_set (arg1);                      (* prepare and fetch the operands *)
  prep_set (arg2);

  clr_rv; (* runtime call kills the return value slot *)

  fetch_set (arg1, arg1desc);
  fetch_set (arg2, arg2desc);

  free_set (arg1);                      (* free the operands *)
  free_set (arg2);

  gen_rt (pushj, sb, op);               (* emit the call to the runtime routines *)

  gen_set_desc (arg1desc);              (* output the argument descriptor words *)
  gen_set_desc (arg2desc);
  
  if (treg <> noreg) and (treg <> 1) then
    gen_rr (move, treg, 1)
 end;
$PAGE ls_in_operator
(* LS IN OPERATOR generates code to test for "arg1" in "arg2".  "Op" designates
   the particular runtime operator to use for the test.    "Treg" is the place
   the caller wants the boolean result left.  *)

public procedure ls_in_operator (op: rt_symbol; arg1, arg2: expr; treg: registers);

 var
   arg1mem: addr_desc;
   arg2desc: set_desc;

 begin
  prepare (anyreg, arg1);               (* prepare and fetch the arguments *)
  prep_set (arg2);

  clr_rv; (* runtime call kills the return value slot *)

  fetch_set (arg2, arg2desc);
  arg1mem := fetch_direct (arg1, 36);

  free_set (arg2);
  free (arg1);

  gen_rt (pushj, sb, op);               (* call the runtime operator *)

  gen_rm (arg, 0, arg1mem);
  gen_set_desc (arg2desc);
  
  if (treg <> noreg) and (treg <> 1) then
    gen_rr (move, treg, 1)
 end;
$PAGE ls_setcvt
(* LS SETCVT converts a long set ("arg1") denoted by a desc_ref to a one or
   two word short set described by "tdesc".  The register in which the converted
   set is placed is returned. *)

public function ls_setcvt (treg: reg_selector; tdesc: expr_type_desc; arg1: expr): registers;

 var
   reg: registers;
   temp: val_desc;
   arg1desc: set_desc;

 begin
  prep_set (arg1);      (* get descriptor for set to convert *)

  clr_rv; (* runtime call kills the return value slot *)

  fetch_set (arg1, arg1desc);
  free_set (arg1);

  temp := get_temp ((tdesc.set_length + bits_per_unit - 1) div bits_per_unit);

  gen_rt (pushj, sb, rt_smv_ll);                (* call set move operator *)
  gen_rm (arg, 0, temp^.loc);                   (* fake desc for target temp *)
  gen_ri (arg, 0, tdesc.set_lwb);
  gen_ri (arg, 0, tdesc.set_length);
  gen_set_desc (arg1desc);                      (* source is set to convert *)

  reg := get_reg (treg, tdesc.set_length);      (* temp has temporary lifetime, so must copy to reg *)
  do_move (reg, temp^.loc, left_aligned, tdesc.set_length);

  ls_setcvt := reg;                             (* return register where set loaded *)
 end;
$PAGE ls_clear
(* LS CLEAR determines if a long set should be "null"ed inline or through
   the runtime. If the set has a constant lowerbound and length it will
   cleared explicitely. *)

procedure ls_clear (lhs, rhs: expr);

var
    set_size, ix: int_type;     (* length in words *)
    maddr: addr_desc;
    reg: registers;

begin
  if lhs^.desc.set_cst_lwb and lhs^.desc.set_cst_len then begin
    set_size := (lhs^.desc.set_length + 35) div 36;
    if set_size <= 4 then begin
      maddr := prep_set_addr (anyreg, lhs^.operand[1]);
    if set_size = 3 then begin  (* clear with SETZM *)
      for ix := 1 to 3 do begin
        gen_rm (setzm, 0, maddr);
        maddr.offset := maddr.offset + 1;
      end;
    end
    else if set_size = 4 then begin     (* clear with dmovems *)
      gen_ri (setzb, 0, 1);
      gen_rm (dmovem, 0, maddr);
      maddr.offset := maddr.offset + 2;
      gen_rm (dmovem, 0, maddr);
    end
    end
    else begin
        prepare (anyreg, lhs^.operand[1]);
        maddr := locate (lhs^.operand[1]);
        if cst_addr (maddr) then begin

          (* address known at compile time *)

        reg := get_reg (anyreg, 36);
        gen (move, reg, 0, 0, gen_blt (1000000b - set_size, none, 0, maddr.reloc));
      end
      else (* must generate address at runtime *) begin
        reg := get_reg (anyreg, 36);
        gen_rm (move, reg, maddr);      (* load set address *)
        gen_ri (hrli, reg, 1000000b - set_size);
      end;
      gen_rx (setzm, 0, reg);
      gen (aobjn, reg, 0, -1, dot);
      free_reg (regdesc[reg]);
    end;
    free_set (lhs);
    free_set (rhs);
  end
  else ls_operator (rt_smv_lz, lhs, rhs, noreg);
end;
$PAGE sets_compatible
(* SETS COMPATIBLE determines if two long sets have identical constant
   lowerbound and length.       *)

function sets_compatible (lhs, rhs: expr): boolean;

begin
  sets_compatible := false;
  if lhs^.desc.set_cst_lwb and rhs^.desc.set_cst_lwb and
    lhs^.desc.set_cst_len and rhs^.desc.set_cst_len and
      (lhs^.desc.set_lwb = rhs^.desc.set_lwb) and
        (lhs^.desc.set_length = rhs^.desc.set_length) then
          sets_compatible := true;
end;
$PAGE lset_move
(* LSET MOVE moves long sets of compatible length and lowerbound *)

procedure lset_move (lhs, rhs: expr);

begin
  do_blt (rhs^.operand[1], lhs^.operand[1], lhs^.operand[3], 36);
  free (lhs^.operand[2]);
  free (rhs^.operand[2]);
  free (rhs^.operand[3]);
end;
$PAGE ls_join
(* LS_JOIN performs union, intersection, or difference on long sets
  of compatible lowerbound and length (to avoid overhead of runtime
  calls. *)

procedure ls_join (operation: opc_range; lhs, rhs: expr);

var
    laddr, raddr: addr_desc;
    set_size: int_type;
    reg: registers;

begin
  set_size := (lhs^.desc.set_length + 35) div 36;       (* words in each set *)
  prep_set (lhs);
  prep_set (rhs);
  laddr := locate (lhs^.operand[1]);
  reg := get_reg (anyreg, 36);
  if cst_addr (laddr) then begin
    gen (move, reg, 0, 0, gen_blt (1000000b - set_size, none, laddr.offset, laddr.reloc));
  end
  else begin
    gen_rm (move, reg, laddr);
    gen_ri (hrli, reg, 1000000b - set_size);
  end;
  lock (reg);
  raddr := locate (rhs^.operand[1]);
  gen_rm (move, 1, raddr);
  unlock (reg);
  gen_rx (move, 0, 1);  (* get a word of source set *)
  gen_rx (operation, 0, reg);   (* join to word in dest set *)
  gen_ri (addi, 1, 1);
  gen (aobjn, reg, 0, -3, dot); (* loop *)
  free_reg (regdesc[reg]);
  free_set (lhs);
  free_set (rhs);
end;
$PAGE ls_move
(* LS MOVE generates code for assignments to long sets (sets addressed by
   descriptors).  There are only certain forms which can be generated.  These
   are documented below. *)

public procedure ls_move (lhs, rhs: expr);
 begin
  case rhs^.opcode of

    desc_ref:                           (* s1 <- s2 *)
      if sets_compatible (lhs, rhs) then
        lset_move (lhs, rhs)
      else ls_operator (rt_smv_ll, lhs, rhs, noreg);

    set_op:                             (* s1 <- [..] *)
      begin
	if upperbound (rhs^.operand) = 0
          then ls_clear (lhs, rhs)
          else ls_operator (rt_smv_lo, lhs, rhs, noreg);
      end;

    union_op:   (* s1 <- s1 + s2 *)
      begin
        if sets_compatible (lhs, rhs)
          then ls_join (iorm, rhs^.operand[1], rhs^.operand[2])
          else ls_operator (rt_sun_ll, rhs^.operand[1], rhs^.operand[2], noreg);
        free_set (lhs);         (* same as op1 *)
        free (rhs);
      end;

    both_op:    (* s1 <- s1 * s2 *)
      begin
        if sets_compatible (lhs, rhs)
          then ls_join (andm, rhs^.operand[1], rhs^.operand[2])
          else ls_operator (rt_sin_ll, rhs^.operand[1], rhs^.operand[2], noreg);
        free_set (lhs);                 (* same as op1 *)
        free (rhs);
      end;

    diff_op:    (* s1 <- s1 - s2 *)
      begin
        if sets_compatible (lhs, rhs)
          then ls_join (andcam, rhs^.operand[1], rhs^.operand[2])
          else ls_operator (rt_sdf_ll, rhs^.operand[1], rhs^.operand[2], noreg);
        free_set (lhs);                 (* same as op1 *)
        free (rhs);
      end;

    func_call_op:       (* s1 <- fset () *)
      begin
        pas_call (rhs, lhs^.operand[1]);        (* pass the address of the target *)
        free_set (lhs);
        free (rhs);
      end

  end (* case *) ;
 end;
$PAGE rt_open_call
(* RT OPEN CALL generates code for an open, reset, rewrite or update op, and
   tags the expression "node" with the register in which it has been loaded.
   "Treg" is a register preference, and "rts" is the symbol for the runtime
   call to actually perform the open. *)

public procedure rt_open_call ( treg: reg_selector; node: expr );

 var iname: 1..2;
     name_addr, name_len: addr_desc;
     option_desc: set_desc;
     open_mode: file_modes;
     open_size: bit_range;
     bits, reg: registers;
     rts: rt_symbol;

 begin
  with node^ do begin
    if operand[1] <> nil
      then iname := 1
      else iname := 2;
    open_mode := desc.base^.file_kind;
    if desc.base^.packable
      then open_size := desc.base^.comp_size
      else open_size := (desc.base^.comp_size + byte_size - 1) div byte_size;

    (* Fetch the operands. *)

    prep_string (operand[iname]);
    prep_set (operand[3]);

    clr_rv; (* runtime call kills the return value slot *)

    fetch_string (operand[iname], name_addr, name_len);
    fetch_set (operand[3], option_desc);

    (* Compute the control bits. *)

    if open_mode = textfile then begin
      case opcode of
        open_op:    rts := rt_open;
        rewrite_op: rts := rt_rewrite;
        reset_op:   rts := rt_reset
      end;
      bits := 0;
    end
    else begin
      if open_mode = binaryfile
        then rts := rt_open_binary
        else rts := rt_open_typed;
      case opcode of
        open_op,
        reset_op:   bits := 8;
        rewrite_op: bits := 4;
        update_op:  bits := 12
      end;
      if desc.base^.packable then
        bits := bits + 2;
    end;
    if iname = 2 then
      bits := bits + 1;

    (* Call the runtime routine. *)

    gen_rt (pushj, sb, rts);
    gen_ri (arg, bits, open_size);
    gen_str_desc (name_addr, name_len);
    gen_set_desc (option_desc);

    free_string (operand[iname]);
    free_set (operand[3]);

    (* The file control block address is in register 1.  Move it into some other
       register, which will be the return register for the operation. *)

    reg := get_reg (treg, 36);
    gen_rr (move, reg, 1);
    tag_reg (reg, node);
  end (* with node^ *);
 end;
$PAGE read_write_call
(* READ WRITE CALL will generate a runtime routine call for a read_op or
   write_op tuple. *)

public procedure read_write_call ( op: tuple );

 var
    fil: expr;
    datum_code: registers; (* acc field of datum descriptor *)
    format_code: registers; (* acc field of first format word *)
    string_io: boolean;
    item_prec: bit_range;
    io_file, io_item, item_len, io_precision, io_width: addr_desc;

 type
    rt_sym_list = array [boolean, boolean] of rt_symbol;

 const
    fd_rts: rt_sym_list = ( rt_wr_fdn, rt_rd_fdn, rt_wr_fdr, rt_rd_fdr );
    sv_rts: rt_sym_list = ( rt_wr_svn, rt_wr_svn, rt_wr_svr, rt_wr_svr );
    ss_rts: rt_sym_list = ( rt_wr_ssn, rt_rd_ssn, rt_wr_ssr, rt_rd_ssr );

 type
    format_list = array [rd_wr_mode] of registers;

 const
    format_table: format_list = ( 0, 0, 0, 0, 8, 4, 0, 8, 12, 1, 0 );

 type
    req_list = array [0..7, boolean] of rt_symbol;

 const
    req_sym: req_list =
     (  rt_int_write,   rt_int_read,    rt_real_write,  rt_real_read,
        rt_real_write,  rt_real_read,   rt_xstr_write,  rt_xstr_read,
        rt_fstr_write,  rt_fstr_read,   rt_cstr_write,  rt_cstr_read,
	rt_strv_read,	rt_strv_read,	rt_bool_write,	rt_bool_write	);

	(* Note that "write varying string" and "read boolean" are impossible
	   combinations. *)


 begin
  with op^ do begin

    if rw_mode = binaryrw then begin
      prep_direct (anyreg, rw_file);
      prep_direct (anyreg, rw_item);
      prep_direct (anyreg, rw_width);
      clr_rv;
      io_file := argument (rw_file);
      mem_lock (io_file);
      io_item := argument (rw_item); (* item is *address* of data area *)
      mem_lock (io_item);
      io_width := argument (rw_width);
      mem_unlock (io_file);
      mem_unlock (io_item);
      free (rw_file);
      free (rw_item);
      free (rw_width);
      if opcode = read_op
        then gen_rt (pushj, sb, rt_read_binary)
        else gen_rt (pushj, sb, rt_write_binary);
      gen_rm (arg, 0, io_file);
      gen_rm (arg, 0, io_item);
      gen_rm (arg, 0, io_width);
    end

    else if rw_mode = imagerw then begin
      prep_direct (anyreg, rw_file);
      prepare (anyreg, rw_item);
      clr_rv;
      io_file := argument (rw_file);
      mem_lock (io_file);
      io_item := fetch_direct (rw_item, 36);
      mem_unlock (io_file);
      free (rw_file);
      free (rw_item);
      if opcode = read_op
        then gen_rt (pushj, sb, rt_read_image)
        else gen_rt (pushj, sb, rt_write_image);
      gen_rm (arg, 0, io_file);
      gen_rm (arg, 0, io_item);
    end

    else (* formatted i/o *) begin
      string_io := (rw_mode in [leftrw, rightrw]);
      if (rw_item^.desc.kind = reals) andif
         (rw_item^.desc.precision > srealprec)
        then item_prec := 72
        else item_prec := 36;

      if not rw_old_file then begin
        fil := rw_file;
        if (fil^.opcode = io_var_str_op) or (fil^.opcode = io_fix_str_op) then
          fil := fil^.operand[1];
      end;

      if not rw_old_file then
        prep_direct (anyreg, fil);
      if string_io
        then prep_string (rw_item)
        else prepare (anyreg, rw_item);
      if rw_precision <> nil then
        prep_direct (anyreg, rw_precision);
      if rw_width <> nil then
        prep_direct (anyreg, rw_width);

      clr_rv;

      if not rw_old_file then begin
        io_file := argument (fil);
        mem_lock (io_file);
      end;
      if string_io then
        fetch_string (rw_item, io_item, item_len)
      else begin
        io_item := fetch_direct (rw_item, item_prec);
        mem_lock (io_item);
      end;
      if rw_precision <> nil then begin
        io_precision := argument (rw_precision);
        mem_lock (io_precision);
      end;
      if rw_width <> nil then
        io_width := argument (rw_width);

      if not rw_old_file then begin
        mem_unlock (io_file);
        free (fil);
      end;
      if not string_io then begin
        mem_unlock (io_item);
        free (rw_item);
      end;
      if rw_precision <> nil then begin
        mem_unlock (io_precision);
        free (rw_precision);
      end;
      if rw_width <> nil then
        free (rw_width);

      if rw_file^.opcode = io_var_str_op then
        gen_rt (pushj, sb, sv_rts [rw_old_file, (opcode = read_op)])
      else if rw_file^.opcode = io_fix_str_op then
        gen_rt (pushj, sb, ss_rts [rw_old_file, (opcode = read_op)])
      else
        gen_rt (pushj, sb, fd_rts [rw_old_file, (opcode = read_op)]);

      if not rw_old_file then
        gen_rm (arg, 0, io_file);

      case rw_mode of
        booleanrw:
          datum_code := 7;
        decimalrw, octalrw, hexrw:
          datum_code := 0;
        realrw, fixedrw, floatrw:
          if item_prec = 36
            then datum_code := 1
            else datum_code := 2;
        leftrw, rightrw:
          case str_format (rw_item) of
            c_format: datum_code := 5;
            f_format:
              if (opcode = read_op) andif (rw_item^.desc.kind = strings) andif
                 (rw_item^.desc.str_kind = varying)
                then datum_code := 6
                else datum_code := 4;
            x_format: datum_code := 3
          end
      end (* case *);

      rw_request [req_sym [datum_code, (opcode = read_op)]] := true;

      format_code := format_table [rw_mode];
      if rw_width <> nil then
        format_code := format_code + 2;
      if (datum_code <= 2) and
         (chk_inp_opt in cur_block^.semantic_options) and
         (opcode = read_op) then
        datum_code := datum_code + 8;

      if string_io then begin
        gen_str_coded (datum_code, io_item, item_len);
        free_string (rw_item);
      end
      else
        gen_rm (arg, datum_code, io_item);

      if datum_code = 8 then begin
        with rw_item^.desc.base^ do begin
          gen (arg, 0, 0, 0, gen_cword (('F', minval), fullword));
          gen_word (cst_area, ('F', maxval), fullword);
        end;
      end
      else if (datum_code = 9) or (datum_code = 10) then begin
        with rw_item^.desc.base^ do begin
          gen (arg, 0, 0, 0, gen_cword (('R', rminval), realword));
          gen_word (cst_area, ('R', rmaxval), realword);
        end;
      end;
      if rw_precision <> nil then begin
        gen_rm (arg, format_code, io_precision);
        format_code := 0;
      end
      else if rw_mode = floatrw then begin
        gen_ri (arg, 6, 0);
        format_code := 0;
      end;

      if rw_width <> nil then
        gen_rm (arg, format_code, io_width)
      else if format_code <> 0 then
        gen_ri (arg, format_code, 0);

    end (* if formatted i/o *);
  end (* with op^ *);
 end.
    1qh„