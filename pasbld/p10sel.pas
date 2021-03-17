$TITLE P10SEL - PDP-10 High-level to Low-level I/F Conversion

module p10sel  options check (assertions);

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         p 1 0 s e l                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This module performs machine specific  translation  of
        the intermediate form.  In particular, it converts references
        to memory access  form  and  determines  the  proper  "shape"
        (length, precision, etc.) of individual operators.
     
     ENTRY POINTS:
     
        shape       invokes  the  translation process.  A walk of the
                    chain of imperative operators is  performed,  and
                    attached  expression tuples transformed by a tree
                    walk.  Unused nodes are reclaimed.
     
     NOTES:  It is assumed that this  operation  is  performed  after
        common   subexpression  have  been  exposed  in  the  machine
        independent form of the IF.  Therefore, care is  taken  never
        to  generate  two nodes for any one node which appears in the
        input.
     
     ---------------------------------------------------------------- *)
$PAGE includes, forward declarations
$include pascal.inc
$INCLUDE ptmcon.inc
$include pasist.inc
$include paspt.typ
$include pasif.typ
$INCLUDE pastal.inc
$include pasifu.inc
$include pasesu.inc
$include pasmth.inc
$INCLUDE passw.inc
$INCLUDE passet.typ
$INCLUDE pasopt.TYP
$INCLUDE pa2dmp.inc


procedure access (ref: expr; node: expr); forward;
function prepare_call (incall: tuple): tuple; forward;
procedure substr_access (ref: expr; node: expr); forward;
function str_reference (str: expr): expr; forward;
function set_reference (se: expr): expr; forward;
function agg_reference (agg, dest_agg: expr): expr; forward;
function expand_short_set_op (tdesc: expr_type_desc; se: expr): expr; forward;
procedure offset_byte_address (saddr: expr); forward;
procedure expand ( first_op, last_op: tuple ); forward;
$PAGE constants
const
  bits_per_unit = 36;
  chars_per_unit = 5;
  flex_arr_desc_size = 36;
  flex_str_desc_size = 36;

const
  pointer_desc: expr_type_desc := (nil, pointers, false, 18);
  file_desc: expr_type_desc := (nil, files, false, 18);
  index_desc: expr_type_desc := (nil, ints, true, 18);
  byte_ptr_desc: expr_type_desc := (nil, pointers, false, 36);
  full_int_desc: expr_type_desc := (nil, ints, true, 36);
  char_desc: expr_type_desc := (nil, chars, false, 18);
  len_desc: expr_type_desc := (nil, ints, false, 18);
  str_1_desc: expr_type_desc := (nil, strings, nonvarying, false, 0);
  set_desc: expr_type_desc := (nil, sets, true, true, minimum (set_range), maximum (set_range));
  ascii_set: expr_type_desc := (nil, sets, true, true, ord (' '), 72);
  io_opt_set: expr_type_desc := (nil, sets, true, true, 0, 6);

const
  var_item: mem_addr_desc := (nil, nil, 0, false, bits_per_unit, absolute_sc, nil);
  parm_item: mem_addr_desc := (nil, nil, 0, false, bits_per_unit, parameter_sc, nil);
  packed_item: mem_addr_desc := (nil, nil, 0, true, 0, absolute_sc, nil);
  const_item: mem_addr_desc := (nil, nil, 0, false, bits_per_unit, constant_sc, nil);
  immed_item: mem_addr_desc := (nil, nil, 0, false, bits_per_unit, absolute_sc, nil);
  temp_item: mem_addr_desc := (nil, nil, 0, false, bits_per_unit, absolute_sc, nil);
  subr_item: mem_addr_desc := (nil, nil, 0, false, bits_per_unit, code_sc, nil);

const (* file control block offsets *)
  file_cursor_offset = 4;
  file_flag_word_offset = 3;

  file_eof_offset = 0; (* bits in flag word *)
  file_eoln_offset = 1;
  file_eopage_offset = 2;

type
  type_set = set of type_kind;

const
  scalar_types: type_set = [ bools, chars, ints, scalars ];
$PAGE mode, best
(* For the PDP-10, we chose either 18 or 36 bit operations.  The primary
   consideration is that 18 bit operations may generate garbage in the left
   half of a register with mixed sign operations, or where the potential
   precision is larger than 18.  Therefore, we only choose 18 bit precision
   when (1) context insures that we will reference only the right half, e.g.
   in indexing, (2) a copy will be performed, so that we can sign extend the
   18 bit result, or (3) the result is naturally unsigned and 18 bits or less. *)



(* MODE computes the natural precision for an operation.  That is no context
   information is availble, and it is assumed that cases (1) or (2) do not
   hold.  Therefore, short (18 bit) operations can only be used in case (3). *)

function mode (node: expr): align_range;
 begin
  with node^ do begin
    if desc.signed
      then mode := 36           (* if signed, must use full word *)
      else (* unsigned *)
        if desc.int_prec <= 18
          then mode := 18       (* naturally a short operation *)
          else mode := 36
  end;
 end;



(* BEST computes a precision for an operations when cases (1) or (2) apply. *)

function best (node: expr): align_range;
 begin
  if node^.desc.int_prec <= 18
    then best := 18
    else best := 36;
 end;
$PAGE rmode
(* RMODE computes the machine precision for real operators/operands. On the
   pdp-10 there are two choices: short and long (7 and 16). *)

function rmode (node: expr): prec_type;
 begin
  if node^.desc.precision <= srealprec
    then rmode := srealprec
    else rmode := maximum (prec_type)
 end;
$PAGE i_context
(* I CONTEXT returns the contextually derived arithmetic precision.  The node which
   determines the i_context is given by "node". *)

function i_context (node: expr): align_range;
 begin
  if node = nil
    then i_context := int_prec_limit     (* there is no constraint *)
    else case node^.opcode of
      ile_op, ige_op, ilt_op, igt_op, ine_op, ieq_op,
      sub_range_chk, str_range_chk, val_range_chk, fld_chk, compat_chk:
        i_context := int_prec_limit;                     (* context does not constrain the value *)

      assign_op:                        (* only is set for rhs *)
        i_context := min (mode (node^.lhs), best (node^.rhs));

      array_ref:
        i_context := index_desc.int_prec;       (* maximum for a index *)

      others:
        i_context := node^.desc.int_prec        (* derived precision of referencing node *)
    end (* case *) ;
 end;
$PAGE sign_context
(* SIGN CONTEXT returns the contextually derived sign constraint.  The node
   which determines the constraint is given by "node". *)

function sign_context ( node: expr ): boolean;
 begin
  if node = nil
    then sign_context := true (* assume signed *)
    else case node^.opcode of
      ile_op, ige_op, ilt_op, igt_op, ine_op, ieq_op,
      sub_range_chk, str_range_chk, val_range_chk, fld_chk, compat_chk:
	sign_context := true; (* context does not constrain sign *)

      assign_op:
	sign_context := node^.lhs^.desc.signed;

      array_ref:
	sign_context := node^.index_val^.desc.signed;

      others:
	sign_context := node^.desc.signed
    end (* case *);
 end;
$PAGE set_i_context
(* SET I CONTEXT is used to record a use of a "node" by some referencing node ("ref")
   with the intent of determining the most appropriate constraint on the result
   to be generated by "node".   The reference is recorded in the "result" field
   of "node".  In the case of conflicts (i.e. multiple uses),  we choose (first)
   a comparision or range check node (which indicates that the value is unbounded),
   and then which ever reference has the smallest precision. *)

procedure set_i_context (node: expr; ref: expr);


  function overrides (op: expr): boolean;       (* predicate testing for compares, range checks *)
   begin
    case op^.opcode of
      ile_op, ige_op, ilt_op, igt_op, ine_op, ieq_op,
      sub_range_chk, str_range_chk, val_range_chk, fld_chk, compat_chk:
        overrides := true;
      others:
        overrides := false
    end
   end;


 begin
  if node^.result = nil
    then node^.result := ref            (* this is the first use so far *)

  else begin                            (* multiple uses *)
    if overrides (node^.result)
      then                              (* if either is compare, it overrides *)
    else if overrides (ref)
      then node^.result := ref
    else if i_context (ref) > i_context (node^.result)  (* pick strictest constraint *)
      then node^.result := ref;
  end;
 end;
$PAGE apply_constraints
(* APPLY CONSTRAINTS determines the machine specific precision at which to
   perform arithmetic operations.

   The general algorithm is a backward scan of all nodes in the IF.  As each
   node of interest is processed, two actions may be performed:  (1) determine
   the precision of that operation using any contextual information available,
   and (2) set the context for children of the node.  Step (2) is performed
   by setting the result field of the child to point to the parent node.

   In the case of a node which has multiple uses, the referencing node with the
   most appropriate constraint is chosen.  Such arbitration is performed as a
   referencing node attempts to set itself as the context node. *)

procedure apply_constraints;

 var
   node: expr;                          (* the scanning cursor *)
   i: 0..4095;
   dbase: typ;

 begin
  node := t_chain^.final_tuple;

  while node <> nil do begin
    with node^ do begin
      case opcode of

        (* In assignment, we can take advantage of two facts:  first, the rhs
           must be within the range of the lhs and therefore its precision can
           be constrained to mode (lhs) -- strictly, this statement is only valid
           if there is no checking enable;  however, if there is, the range check
           constraints will override.  Second, since a copy is to be performed,
           we can sign extend; therefore, best (rhs) is also a constraint. *)

        assign_op:
          set_i_context (rhs, node);

        (* In the case of a direct data reference, use the natural size for the
           item given its declared precision -- because this is the size of the
           register in which it will be loaded. *)

        first_data_ref..last_data_ref,
          func_call_op:
            begin
              dbase := desc.base;
              if desc.kind in scalar_types              (* only works with certain types *)
                then desc.int_prec := mode (node)
              else if desc.kind = pointers
                then desc := pointer_desc
              else if desc.kind = files
                then desc := file_desc
              else if desc.kind = reals
                then desc.precision := rmode (node);
              desc.base := dbase;

              (* In the case of an array reference, the index value is constrained
                 by the size of the machine's index registers. *)

              if opcode = array_ref then set_i_context (index_val, node);
            end;


        (* The precision of the operands cannot be larger than that known to be
           required for the result.  This is a result of the properties of
           modulus 2**n. *)

        iadd_op, isub_op, imul_op, ineg_op, iabs_op, 

        (* The precision of the multiplicand must be <= the desired precision.
           Technically however, the exponent may take on any arbitrarily large
           precision, since the multiplicand may be zero or one.  In those cases,
           the exponent does not matter (0**0 being undefined).  If the multi-
           plicand takes on any other value (e.g. 2), the exponent must have a
           precision <= the result precision less one.  Therefore, we simply
           use the result precision. *)

        expii_op:
          begin
            desc.int_prec := min (mode (node), i_context (result));
	    desc.signed := desc.signed and sign_context (result);
            for i := 1 to upperbound (operand) do set_i_context (operand [i], node);
          end;

        (* Comparisions and range check operators do not constrain their
           operands, as clearly the purpose of the operation is to test the
           operands' natural values. *)

        ile_op, ilt_op, igt_op, ige_op, ieq_op, ine_op:
          begin
            desc.int_prec := mode (node);               (* natural mode for booleans *)
            set_i_context (operand[1], node);
            set_i_context (operand[2], node);
          end;

        sub_range_chk, str_range_chk, val_range_chk, fld_chk, compat_chk:
          begin
            for i := 1 to upperbound (operand) do
	      if operand[i] <> nil then
		set_i_context (operand[i], node);
          end;

        (* The sclcvt_op operator appears in the IF to denote conversions such
           as char or ord.  In check mode, it requires checking; otherwise, it
           is essentially transparent. *)

        sclcvt_op:
          begin
            if desc.kind = pointers
              then desc := pointer_desc         (* strict limit *)
	      else begin
		desc.int_prec := i_context (result);
		desc.signed := sign_context (result);
	      end;
            set_i_context (operand[1], node);
          end;

        (* The following operators do not in any way constraint their operands,
           but are themselves constrained by the referencing operands. *)

        idiv_op, imod_op, imin_op, imax_op:
	  begin
	    desc.int_prec := min (mode (node), i_context (result));
	    desc.signed := desc.signed and sign_context (result);
	  end;

        (* The following operators have essentially self-evident precisions,
           but need, in some cases, to be rounded to a natural machine precision. *)

        or_op, orif_op, and_op, andif_op, in_op, bnot_op,
        rle_op, rlt_op, rgt_op, rge_op, req_op, rne_op,
        sle_op, slt_op, sgt_op, sge_op, seq_op, sne_op,
        setle_op, setge_op, seteq_op, setne_op,
        ptreq_op, ptrne_op, runtime_op:
          desc.int_prec := mode (node);

        lwb_op, upb_op, dim_op:
          desc.int_prec := min (mode (node), 18);       (* cannot exceed index size *)

        addr_op, new_op, in_str_op, out_str_op: (* to set pointer size for this machine *)
          begin
            dbase := desc.base;
            desc := pointer_desc;
            desc.base := dbase;
          end;

        open_op, reset_op, rewrite_op, update_op: (* to set file pointer size for this machine *)
          begin
            dbase := desc.base;
            desc := file_desc;
            desc.base := dbase;
          end;


        (* Real operations need only to be rounded to a natural machine precision. *)

        radd_op, rsub_op, rmul_op, rdiv_op, rneg_op, rabs_op, rmin_op, rmax_op,
        sqrt_op, float_op, ln_op, log_op, exp_op, sin_op, arcsin_op, sinh_op,
        cos_op, arccos_op, cosh_op, tan_op, tanh_op, cotan_op, random_op,
        arctan_op, expri_op, exprr_op:
          desc.precision := rmode (node);

        (* String functions *)

        index_op, search_op, verify_op:
          begin
            if upperbound (operand) = 2
              then desc := len_desc (* at least as good as any contextually derived limit *)
              else begin                                (* failure result specified *)
                desc.int_prec := min (mode (operand[3]), i_context (result));     (* result is unlimited *)
		desc.signed := operand[3]^.desc.signed and sign_context (result);
                set_i_context (operand[3], node);               (* but pass along context *)
              end;
          end;

        lwc_op, upc_op:
          begin
            if operand[1]^.desc.kind <> strings then begin      (* char or scalar converted to char *)
	      desc.signed := desc.signed and sign_context (result);
              set_i_context (operand[1], node);
            end;
          end;

        length_op:
          desc := len_desc;

        (* Real transfer functions:  trunc only generates an integer, round
           generates an integer in the one operand form and a real in the two
           operand form.  The integers are taken as full width values. *)

        trunc_op:
          desc.int_prec := mode (node);

        round_op:
          begin
            if desc.kind = reals
              then desc.precision := rmode (node)
              else desc.int_prec := mode (node);
          end;

        (* For debugging purposes, record the statement being processed. *)

        start_stmt:
          cur_source := stmt_source

      end (* case opcode *) ;

      node := prev;                             (* continue backward scan *)
    end (* with op^ *) ;
  end (* while *) ;
 end;
$PAGE transform
(* TRANSFORM:  The general action of the shaping code is to scan the input IF from
   beginning to end and to expand certain operators.  The transformed result is
   recorded in the result field of the operator.  This routine is used as the
   uniform mechanism to extract the result when an original pointer is to be
   replaced;  that is, usage of the form:  op := transform (op) is assumed.  *)

function transform (op: expr): expr;
 begin
  if op = nil then transform := nil     (* be prepared for degenerate cases *)
  else if op^.result = nil then transform := op
  else transform := op^.result;
 end;
$PAGE transform_operands
(* TRANSFORM OPERANDS replaces all the operands of an IF tuple with their
   "transform"ed versions.  Both expression and imperative tuples are processed. *)

procedure transform_operands (t: expr);
 var i: 0..4095;
 begin
  with t^ do begin
    case opcode of

      field_ref:
        base_rec := transform (base_rec);

      ptr_ref:
        base_rec := transform (base_ptr);

      array_ref:
        begin
          base_array := transform (brray);
          index_val := transform (index_val);
        end;

      substr_ref:
        begin
          base_string := transform (base_string);
          substr_index := transform (substr_index);
          substr_length := transform (substr_length);
        end;

      buffer_ref:
        base_file := transform (base_file);

      mem_ref, addr_ref, immed_ref:
        begin
          item.index := transform (item.index);
          item.base := transform (item.base);
        end;

      assign_op:
        begin
          lhs := transform (lhs);
          rhs := transform (rhs);
        end;

      eval_op:                          (* just evaluate a form *)
        rhs := transform (rhs);

      first_chk_op..last_chk_op,
      first_nary_op..last_nary_op:
        for i := 1 to upperbound (operand) do operand[i] := transform (operand[i]);

      call_op, func_call_op:
        begin
          subr := transform (subr);
          for i := 1 to upperbound (arglist) do arglist[i] := transform (arglist[i]);
        end;

      start_with:
        with_rec := transform (with_rec);

      jump_t_op, jump_f_op, jump_in_op, jump_cond_op, case_jump_op:
        cond := transform (cond);

      goto_op:
        target_frame := transform (target_frame);

      dispose_op:
        dptrarg := transform (dptrarg);

      signal_op, mask_op, unmask_op:
	cond_parm := transform (cond_parm);

      first_io_stmt..last_io_stmt:
        file_arg := transform (file_arg);

      read_op, write_op:
        begin
          rw_file := transform (rw_file);
          rw_item := transform (rw_item);
          rw_width := transform (rw_width);
          rw_precision := transform (rw_precision);
        end;

      seek_op:
        begin
          seek_file := transform (seek_file);
          seek_index := transform (seek_index);
        end

    end (* case *) ;
  end (* with *) ;
 end;
$PAGE init_node
(* INIT NODE initializes a new expr node, with a specified type descriptor, and
   chains it into the tuple list.   The caller determines where in the list it is
   to be chained in order to preserve a tree walk order, and performs a "t_set"
   if needed to insure that it is emitted in the correct position.

   Generally, when processing a node, there is only one "t_set" and that is in
   the main shape loop.  The insertion point is set to the node preceeding that
   to be expanded so that all special operators created in the expansion lie in
   the right place.  In a few cases, operators are patched in out of order; 
   however, routines which do this are responsible for saving and restoring the
   t_setting of its caller. *)

procedure init_node (node: expr; ndesc: expr_type_desc);
 begin
  with node^ do begin
    emit (node);                        (* chain into existing tree *)
    nodeid := 0;
    desc := ndesc;
    usage_count := 1;
    result := nil;
    ref_fre := 0;
    killed_tuple := false;
    copy_tuple := false;
    context := valx;
  end;
 end;
$PAGE const_access
(* CONST ACCESS is used to address a constant value.  It is assumed that a
   memory access (of any kind) is required, and that any immediate references
   have been handled.  This constructs an address relative to a value node which
   will later be assigned a full address.  Note: up until this time, scalars
   have been stored in "val"s and not "value_node"s; so here it is made a full
   value node.  Parameters: "value" is the val to access; "node" is the access
   tree to be filled in with the address. *)

procedure const_access (value: val; node: expr);
 begin
  with node^ do begin
    item := const_item;                 (* unit aligned in constant section *)

    if value.kind = scalar_cst then begin       (* create full value node for this *)
      new (item.cstref, scalar_cst);
      with item.cstref^ do begin
        scalar_val := value.ival;
        def_addr := nil;
      end;
    end

    else item.cstref := value.valp;                     (* copy the node given *)
  end (* with *) ;
 end;
$PAGE int_const, scalar_const
(* INT CONST forms a reference to integer constant.  Either an immed_ref or mem_ref
   is generated depending on the precision required to represent the value. *)

function int_const (value: int_type): expr;
 var sclval: val;
 begin
  if (value < 0) or (value > 377777B) then begin        (* full word constant *)
    new (int_const, mem_ref, mem_ref);
    init_node (int_const, full_int_desc);
    int_const^.desc.signed := (value < 0);
    sclval.kind := scalar_cst;          (* generate val record for value *)
    sclval.ival := value;
    const_access (sclval, int_const);
  end

  else begin                            (* immediate constant *)
    new (int_const, immed_ref, immed_ref);
    init_node (int_const, index_desc);
    with int_const^ do begin
      item := immed_item;
      item.offset := value;
      desc.signed := (value < 0);
    end;
  end;
 end;

(* SCALAR CONST forms a reference to a constant of any scalar type of a pointer
   constant (i.e. nil).  As with the above, the most appropriate representation
   is found. *)

function scalar_const (scalar_type: typ; value: int_type): expr;
 begin
  scalar_const := int_const (value);    (* to form new node *)
  if scalar_type <> nil then begin
    with scalar_const^ do begin (* change from integer to whatever *)
      desc.kind := scalar_type^.kind;
      if desc.kind <> pointers
        then desc.base := scalar_type^.base_type
        else desc.base := scalar_type;
    end;
  end;
 end;
$PAGE op_1, op_2, op_3
(* OP N creates a tree node with *n* operands.  The "opc"ode and "desc"riptor
   are given. *)

function op_1 (opc: tuple_opcodes; desc: expr_type_desc; o1: expr): expr;
 begin
  new (op_1, nary_op, nary_op, 1);
  init_node (op_1, desc);
  with op_1^ do begin
    opcode := opc;
    operand[1] := o1;
  end;
 end;

function op_2 (opc: tuple_opcodes; desc: expr_type_desc; o1, o2: expr): expr;
 begin
  new (op_2, nary_op, nary_op, 2);
  init_node (op_2, desc);
  with op_2^ do begin
    opcode := opc;
    operand[1] := o1;
    operand[2] := o2;
  end;
 end;

function op_3 (opc: tuple_opcodes; desc: expr_type_desc; o1, o2, o3: expr): expr;
 begin
  new (op_3, nary_op, nary_op, 3);
  init_node (op_3, desc);
  with op_3^ do begin
    opcode := opc;
    operand[1] := o1;
    operand[2] := o2;
    operand[3] := o3;
  end;
 end;
$PAGE assign, eval
(* ASSIGN generates an assignment of "s" to "t". *)

procedure assign (t: expr; s: expr);
  var op: tuple;
begin
  new (op, assign_op);
  emit (op);
  with op^ do begin
    nodeid := 0;
    must_store := true; (* all assignments generate in this pass must happen *)
    lhs := t;
    rhs := s;
  end;
end;



(* EVAL generates an "eval_op" and applies it to the expr "exp". *)

procedure eval (exp: expr);
  var op: tuple;
 begin
  new (op, eval_op);
  emit (op);
  with op^ do begin
    nodeid := 0;
    rhs := exp;
    lhs := nil;                 (* dump code likes this to be nil *)
  end;
 end;
$PAGE display
(* DISPLAY (n) creates a display_op node referencing the stack frame *n* static
   levels out from the current frame.   The display for the current frame (0) is
   an obvious common subexpression;  therefore, a "cur_display" is kept and
   returned for n = 0.  It is initialized at the start of "shape". *)

type dlevels = 0..63;
static var cur_display: tuple;

function display (n: dlevels): expr;
 begin
  if n = 0
    then display := cur_display
    else begin
      new (display, display_op, display_op);
      init_node (display, pointer_desc);
      display^.nlevels := n;
    end;
 end;
$PAGE indirect
(* INDIRECT takes an accessing node and makes the address contained in it an
   indirect address.  A new node is created, and the appropriate information
   is copied down.  The new node is then made the index with zero offset of
   the original node. *)

procedure indirect (node: expr);
 var ip: expr;                          (* the "indirect pointer" *)
 begin
  new (ip, mem_ref, mem_ref);           (* create the node and initialize *)
  init_node (ip, pointer_desc);
  ip^.item := node^.item;               (* copy addressing info from original *)
  node^.item := var_item;               (* set original to indirect *)
  node^.item.index := ip;
 end;
$PAGE bitindirect
(* BIT INDIRECT takes an accessing node and makes the address a packed address,
   i.e., on the PDP-10, a byte pointer.  To show this, the following convention
   is used:  the byte pointer is an "addr_ref" operator with the packed bit set;
   the reference to the item addressed by the byte pointer is marked as packed,
   and the *base* field points to the byte pointer.  A new node is created to
   hold the byte address, and this new node made the base with zero offset of
   the original node. *)

procedure bitindirect (node: expr);
 var bp: expr;                                  (* the "byte pointer" *)
 begin
  new (bp, addr_ref, addr_ref);
  init_node (bp, byte_ptr_desc);
  with bp^ do begin
    item := node^.item;                 (* copy address info *)
    if not item.pack                    (* convert to bit offset, if necssary *)
      then item.offset := item.offset * bits_per_unit;
    item.pack := true;
  end;
  with node^ do begin                           (* set original to indirect *)
    item := packed_item;
    item.base := bp;
    item.size := bp^.item.size;
  end;
 end;
$PAGE parm_base, parm_access
(* PARM BASE constructs a mem_ref to the parm base pointer which addresses the
   parameter list block for a given block ("defblock"). *)

function parm_base (defblk: blk): expr;
 begin
  new (parm_base, mem_ref, mem_ref);
  with parm_base^ do begin
    item := parm_item;
    item.offset := defblk^.parm_list_base;
    item.index := display (cur_block^.apparent_level - defblk^.apparent_level);
  end;
  init_node (parm_base, pointer_desc);
 end;



(* PARM ACCESS determines how to address a particular parameter ("parm") and
   enters the information into a caller supplied "node".

   On the PDP-10, up to six words of arguments may be passed in registers to the
   called procedure.  If there are more than six, the parameters are passed in a
   block in memory with a pointer to that block in the first parameter register. *)

procedure parm_access (node: expr; parm: sym);
 begin
  with node^ do begin
    item := parm_item;
    if (parm^.block^.subr_sym^.type_desc^.parmlist_size > 6)
      and (parm <> parm^.block^.return_sym)     (* return symbol not in parm block *)
      then item.index := parm_base (parm^.block)        (* parm found in parameter block *)
      else item.index := display (cur_block^.apparent_level - parm^.block^.apparent_level)
                                        (* parms passed individually *) ;
    item.sym_name := parm;              (* designate the parmeter to be access *)
  end (* with *) ;
 end;
$PAGE parm_descriptor
(* PARM DESCRIPTOR constructs a tree accessing the descriptor of a parameter
   array or string.  "Id" is the parameter symbol whose descriptor is being
   referenced; "offset" is the offset of the descriptor relative to the parameter
   pointer. *)

function parm_descriptor (id: sym; offset: int_type): expr;
 begin
  new (parm_descriptor, mem_ref, mem_ref);
  parm_access (parm_descriptor, id);            (* address the parameter *)
  init_node (parm_descriptor, index_desc);      (* desc's are scalars *)
  with parm_descriptor^ do              (* fetch the descriptor *)
    item.offset := item.offset + offset;
 end;
$PAGE base_access
(* BASE ACCESS is used to address the base of some structure datum denoted by
   the reference tree "ref".  This fills "node" with the address of the ref; but
   it is *expected* that this node will be altered to access some component of
   the datum.  Since the base "ref" has already been processed, the result
   field points to some operator which gives the address of the node. *)

procedure base_access (ref: expr; node: expr);
 var ref_base: expr;
 begin
  ref_base := agg_reference (ref, nil);
  with ref_base^ do begin
    if opcode = desc_ref
      then node^.item := operand[1] (* addr *) ^.item
      else node^.item := item;          (* assume ref is mem_ref or addr_ref *)
  end;
 end;
$PAGE lower_bound
(* LOWER BOUND constructs a tree computing the lower bound of a string or
   array reference ("ref").  This returns a constant if the lower bound is
   known at compile time; otherwise (generic parameter arrays), a mem_ref
   to the hidden descriptor word is returned. *)

function lower_bound (ref: expr): expr;

 var reftype: typ;                              (* type of item referenced *)

 begin
  reftype := ref^.desc.base;

  (* If the type is not generic, then the lowerbound is a constant.  For arrays
     it is the minimum of the index type; for strings, 1, by definition. *)

  if not reftype^.generic then begin
    if reftype^.kind = arrays
      then lower_bound := int_const (reftype^.index_type^.minval)
      else lower_bound := int_const (1);
  end

  (* Otherwise, the datum is a generic array parameter.  The lower bound is in
     the parameter list, 2 locations before the pointer to the item. *)

  else lower_bound := parm_descriptor (ref^.id_sym, -2);

 end;
$PAGE upper_bound
(* UPPER BOUND constructs a tree computing the upper bound of a string or
   array reference ("ref").  This returns a constant if the upper bound is
   known at compile time; otherwise (generic or flexible arrays or strings),
   a mem_ref to the hidden descriptor word is returned. *)

function upper_bound (ref: expr): expr;

 var reftype: typ;

 begin
  reftype := ref^.desc.base;

  (* If the type is not flexible, then the upperbound is a constant.  For arrays
     it is the maximum of the index type; for strings, the maximum length. *)

  if not reftype^.flexible then begin
    if reftype^.kind = arrays
      then upper_bound := int_const (reftype^.index_type^.maxval)
      else upper_bound := int_const (reftype^.str_length);
  end

  (* If the datum is an aggregate constructor, then the upper bound may be
     computed directly from the lower bound and the number of elements. *)

  else if ref^.opcode = agg_val then begin
    if reftype^.generic
      then upper_bound := int_const (upperbound (ref^.operand))
      else upper_bound := int_const (upperbound (ref^.operand) + reftype^.index_type^.minval - 1);
  end

  (* If the datum is a parameter, then the upper bound is in the parameter list,
     one location before the pointer to the datum. *)

  else if (ref^.opcode = ident_ref) then        (* other cases are field and ptr ref *)
    upper_bound := parm_descriptor (ref^.id_sym, -1)

  (* Last case: the item is flexible and not a parameter.  This means that it
     is on the heap somewhere.  The hidden descriptor word is located at the
     first storage location of the array. *)

  else begin
    new (upper_bound, mem_ref, mem_ref);        (* access descriptor at start of item *)
    init_node (upper_bound, index_desc);
    access (ref, upper_bound);          (* no adjustment need, offset = 0 *)
  end;
 end;
$PAGE compute_size
(* COMPUTE SIZE constructs a tree returning the runtime size of a specified
   type with a specified upperbound parameter, if necessary. *)

function compute_size (t: typ; ubound: expr): expr;

var a, b, c, d: integer;
    grotesque: boolean;

begin
  size_of (t, false, a, b, c, d, grotesque); (* compute the runtime size *)
  assert (not grotesque);
  if a = 0 then
    compute_size := int_const (b)
  else begin
    if a = 1
      then compute_size := ubound
      else compute_size := op_2 (imul_op, len_desc, ubound, int_const (a));
    if b > 0 then
      compute_size := op_2 (iadd_op, len_desc, compute_size, int_const (b))
    else if b < 0 then
      compute_size := op_2 (isub_op, len_desc, compute_size, int_const (-b));
  end;
  if c <> 1 then
    compute_size := op_2 (idiv_op, len_desc, compute_size, int_const (c));
end (* compute_size *);
$PAGE ident_access
(* IDENT ACCESS determines how to address a var, value or const symbol denoted
   by an ident_ref node ("ref").  The addressing information is placed in the
   caller supplied "node". *)

procedure ident_access (ref: expr; node: expr);
 var id: sym;
 begin
  id := ref^.id_sym;
  with node^ do begin
    item := var_item;                   (* init as absolute, unit aligned ref *)
    item.class := id^.dcl_class;
    item.sym_name := id;                (* the identifier referenced *)

    case item.class of

      local_sc:                         (* in stack frame of some block *)
        item.index := display (cur_block^.apparent_level - id^.block^.apparent_level);

      parameter_sc:                     (* parameter to some block *)
        begin
          parm_access (node, id);               (* determine where parm value or ptr is *)
          if passed_by_address (id)
            then indirect (node);       (* indirect to data *)
        end;

      external_sc:      (* external name *)
        begin
          if prog_options.overlay_opt and (id_sym^.kind <> conditions) and
	     not ((id^.kind = consts) and    (* not procedure/function id *)
		  (id^.type_desc^.kind in [procs, funcs]))
            then indirect (node);       (* address is really pointer to item *)
        end;

      constant_sc:                              (* reference to a declared const *)
        const_access (id^.init_value, node);

      static_sc:
	(* no action *)

    end (* case *) ;
  end (* with node^ *) ;
 end;
$PAGE field_access
(* FIELD ACCESS determines how to address a (packed) field, denoted by a "field_ref"
   expr, and fills in a caller supplied node with the addressing information.  The
   address of the base record is determined, and the field offset added in.  If the
   field is not then directly addressible, a "byte pointer" is generated. *)

procedure field_access (ref: expr; node: expr);
 var fldid: sym;
 begin
  fldid := ref^.field_sym;
  base_access (ref^.base_rec, node);            (* put address of record in node *)
  with node^ do begin
    item.size := min (bits_per_unit, fldid^.fld_width);
    if (item.size < bits_per_unit) and
       fldid^.fld_record^.packable and
       not (ref^.desc.base^.kind in [arrays, records, strings]) (* watch out for flex arrays,
                                                                   which have size zero *)
      then begin                        (* packed reference *)
        if not item.pack                (* make it a bit offset *)
          then item.offset := item.offset * bits_per_unit;
        item.pack := true;
        item.offset := item.offset + fldid^.fld_offset;
      end
      else begin                        (* unpacked reference *)
        item.offset :=          (* add in an unit offset *)
          item.offset + (fldid^.fld_offset div bits_per_unit);
      end;
  end (* with *) ;
 end;
$PAGE ptr_dereference
(* PTR DEREFERENCE creates accessing information for an object referenced by a
   "ptr_ref" expr, given the reference itself, and a caller supplied node in
   which to place the information.  It cannot be assumed that the base pointer
   is itself a reference, as the result may be a new_op, address, or coerced
   pointer. *)

procedure ptr_dereference (ref: expr; node: expr);
 begin
  with node^ do begin
    item := var_item;
    item.index := transform (ref^.base_ptr);
  end;
 end;
$PAGE buffer_access
(* BUFFER ACCESS creates accessing information for an object referenced by a
   "buffer_ref" expr, given the reference itself, and a caller supplied node
   in which to place the information.  The base_file points to the file block,
   whose first word points to the file component, or buffer, so this is like
   ptr_dereference, with an extra level of indirection. *)

procedure buffer_access (ref: expr; node: expr);
 begin
  with node^ do begin
    item := var_item;
    item.index := transform (ref^.base_file);
  end;
  indirect (node);
 end;
$PAGE unpacked_array_access
(* UNPACKED ARRAY ACCESS performs array subscript calculation on an array reference
   to an unpacked array ("ref").  The result is placed in "node".  For each subscript,
   the offset := ((subscript - lowerbound) * multiplier) is calculated; care is  taken
   to fold obvious constant calculations. *)

procedure unpacked_array_access (ref: expr; node: expr);

 var
  arraytype: typ;       (* type of array being accessed *)
  idxtype: typ; (* type of the index of the array *)

  idx: expr;                    (* expression part of current index *)
  multiplier: int_type;                 (* size of elements of the array in units *)
  lwb: expr;            (* nonconstant lowerbound of generic array *)
  base_idx: expr;               (* index of base array *)

 begin
  with ref^ do begin
    arraytype := base_array^.desc.base;         (* get useful info from ref tree *)
    idxtype := arraytype^.index_type;
    multiplier := arraytype^.element_size div bits_per_unit;

    base_access (base_array, node);
    base_idx := node^.item.index;       (* save for future use *)

    (* If the array is generic, then isolate the address of the virtual origin of
       the array as a separate computation as constant from the entry block. Any
       generic array must be a parameter; therefore it has a base pointer which is
       the pointer to the array.  Subtract (lwb * multiplier) from this, and make
       the result the new base pointer. *)

    if arraytype^.generic then begin
      lwb := lower_bound (base_array);  (* get nonconstant lwb *)
      if multiplier <> 1
        then lwb := op_2 (imul_op, index_desc, lwb, int_const (multiplier));
      base_idx := op_2 (isub_op, pointer_desc, base_idx, lwb);
    end

    (* If the array is not generic, then the lower bound is constant, and may be
       accumulated directly into the constant offset. *)

    else node^.item.offset := node^.item.offset - (idxtype^.minval * multiplier);

    (* Calculate the variable offset (index_val * multiplier).  The lowerbound has
       already been accounted for. *)

    idx := transform (index_val);
    if multiplier <> 1
      then idx := op_2 (imul_op, index_desc, idx, int_const (multiplier));

    (* If the base address already has an index, add it to the offset just computed.
       Special care is taken to separate out the base address pointer, so that a
       multidimensional index appears as ((sub1*size1)+(sub2*size2)+baseaddr) instead
       of ((baseaddr+(sub1*size1))+(sub2*size2)), on the assumption that the former
       form exposes more common subexpressions. *)

    if base_idx <> nil then begin
      with base_idx^ do begin
        if desc.kind = pointers
          then idx :=                           (* index is just base addr *)
            op_2 (iadd_op, index_desc, idx, base_idx)    (* by convention move base ptr to right *)
        else if (opcode = iadd_op) andif (operand[2]^.desc.kind = pointers)
          then idx :=   (* index is (sub + baseaddr) *)
            op_2 ( iadd_op, index_desc,
                     op_2 (iadd_op, index_desc, operand[1], idx),
                     operand[2]                                  )
        else idx := op_2 (iadd_op, index_desc, idx, base_idx);   (* index is simple expr *)
      end (* with *) ;
    end;

    node^.item.index := idx;    (* subscript the base *)
  end (* with ref^ *) ;
 end;
$PAGE add_cst_bit_index
(* ADD CST BIT INDEX adds a constant index to a byte pointer expression. It is
   assumed that the bit size of the packed array being indexed is contained in
   the byte pointer.  The initial alignment is maintained as in the ADJBP 
   instruction. *)

procedure add_cst_bit_index (byteaddr: expr; idx: int_type);

 var
   nelems_per_unit: align_range;                (* number of packed elements per unit *)
   units: int_type;                     (* pos/neg unit offset *)
   bits: int_type;                              (* pos bit offset from start of word *)
   nelems: int_type;                            (* offset in terms of packed elements *)
   offset: align_range;                         (* bit offset of first element in unit *)

 begin
  with byteaddr^ do begin
    units := item.offset div bits_per_unit;     (* split offset into unit/bit *)
    bits := item.offset mod bits_per_unit;
    if bits < 0 then begin              (* bits >= 0 assumed below *)
      bits := bits_per_unit + bits;
      units := units - 1;
    end;
    offset := bits mod item.size;       (* alignment of first element *)

    nelems := idx + (bits div item.size);       (* element offset relative to "units" *)
    nelems_per_unit := bits_per_unit div item.size;
    units := units + (nelems div nelems_per_unit);      (* make nelems pos offset within unit *)
    nelems := nelems mod nelems_per_unit;
    if nelems < 0 then begin
      units := units - 1;
      nelems := nelems_per_unit + nelems;
    end;

    item.offset :=                      (* calculate final offset *)
      (units * bits_per_unit)           (* pos/neg offset to word containing the byte *)
        + (nelems * item.size)          (* pos offset to start of byte *)
        + offset;               (* preserve the original alignment *)
  end (* with *) ;
 end;
$PAGE packed_array_access
(* PACKED ARRAY ACCESS performs the address computation for subscripting a
   packed array reference ("ref"), and stores the result in "node".  The
   computation is logically the same as for an unpacked array, and again 
   obvious constant subexpressions are folded.  However, the result is a byte
   pointer value. *)

procedure packed_array_access (ref: expr; node: expr);

 var
   arraytype: typ;                      (* type of array being referenced *)
   idxtype: typ;                        (* type of the index of the array *)
   lwb: expr;                   (* lower and upper bound expressions *)
   idx: expr;                           (* index expr being built *)
   nelems_per_unit: align_range;        (* number of array elements per storage unit *)

 begin
  with ref^ do begin
    arraytype := base_array^.desc.base; (* get info from ref node *)
    idxtype := arraytype^.index_type;
    nelems_per_unit := bits_per_unit div arraytype^.element_size;

    (* To index a packed array we offset a byte pointer by the computed
       index expression.  The byte pointer is a byte pointer to the first
       element of the base array.  This is constructed with an addr_ref
       operator marked as being a packed ref.  The offseting operation is
       indicated by a packed addr_ref whose index is the computed subscript
       and whose *base* is the byte pointer expression. *)

    base_access (base_array, node);                     (* yields unpacked addr of base array *)
    node^.item.size := arraytype^.element_size; (* set byte size *)
    bitindirect (node);                         (* make addr packed indirect (byte ptr) *)

    (* It is useful to expose the byte pointer to the virtual origin of the 
       array as a common subexpression.  So, the base byte pointer is offset by
       the lower bound. *)

    if arraytype^.generic then begin    (* lower bound is not constant *)
      lwb := lower_bound (base_array);
      node^.item.index := op_1 (ineg_op, index_desc, lwb);       (* make it a packed indexed address *)
      bitindirect (node);
    end

    else (* not generic *) begin        (* lower bound is constant *)
      add_cst_bit_index (node^.item.base, - idxtype^.minval);   (* add to byte pointer *)
    end;

    (* Calculate the index expression, and use it to index the byte pointer to the
       base of the array.  The result is itself a byte pointer used to access the
       actual element of the array. *)

    idx := transform (index_val);
    node^.item.index := idx;
    bitindirect (node);
  end (* with ref^ *) ;
 end;
$PAGE array_access
(* ARRAY ACCESS determines how to address an arbitrary array reference ("ref"),
   and places the accessing computation in "node".  Essentially, this routine
   decides if the reference is packed or unpacked, and dispatches to the case
   specific routines. *)

procedure array_access (ref: expr; node: expr);

 var arraytype: typ;                    (* type of array being subscripted *)
 begin

  (* To test for a packed array, we check both the array packing flag and
     the element size, since the data may not in fact be packable. *)

  arraytype := ref^.base_array^.desc.base;
  if arraytype^.packable and (arraytype^.element_size < bits_per_unit)
    then packed_array_access (ref, node)
    else unpacked_array_access (ref, node);

 end;
$PAGE access
(* ACCESS determines how to address an arbitray reference, and fills in any
   type access node (mem_ref, addr_ref, etc.), except immed_ref, with the
   required information to address the item.  "Ref" is the reference form node
   to address; "node" is the access form node to fill in. *)

procedure access (* ref: expr; node: expr *);
 begin
  case ref^.opcode of
    cst_ref:       const_access (ref^.cst_val, node);
    ident_ref:     ident_access (ref, node);
    field_ref:     field_access (ref, node);
    ptr_ref:       ptr_dereference (ref, node);
    array_ref:     array_access (ref, node);
    substr_ref:    substr_access (ref, node);
    buffer_ref:    buffer_access (ref, node);

    (* If we get a previously processed node, just return the addressing
       information already developed.  This occurs when asking for the
       of a previously processed node.  (The addressing information for a
       desc_ref is in its first operand.) *)

    mem_ref,
    addr_ref:       node^.item := ref^.item;
    desc_ref:       node^.item := ref^.operand[1]^.item
  end (* case *) ;
 end;
$PAGE addr
(* ADDR converts a reference tree ("ref") to a tree giving the address.  The
   result is always an "addr_ref", but may be a packed or unpacked address.
   Even though the reference has already been processed, an address is 
   generated anew for several reasons:  string addresses may reflect unusual
   offsets, and constants may be in immediate form. *)

function addr (ref: expr): expr;

  var
    desc_size: unit_range;

 begin
  new (addr, addr_ref, addr_ref);
  if (ref^.desc.kind in [arrays, records]) andif
     ( (ref^.opcode = agg_val) or (ref^.opcode = func_call_op) )
    then access (agg_reference (ref, nil), addr)
    else access (ref, addr);

  (* If the reference is to a flexible array or string on the heap, then addr
     now points to the length descriptor word.  Therefore, we add the descriptor
     size to find the address of the actual data.  (We can determine whether a
     flex array or string is on the heap by checking the opcode.  A flex ident_ref
     must be a parameter; a flex agg_val is created on the stack; all other flex
     refs must be on the heap. ) *)

  with ref^ do begin
    if ( ( (desc.kind = arrays) andif desc.base^.flexible ) or
         ( (desc.kind = strings) andif desc.str_flex ) ) andif
       ( ( opcode <> ident_ref ) and ( opcode <> agg_val ) ) then begin
      if desc.kind = arrays
	then desc_size := flex_arr_desc_size
	else desc_size := flex_str_desc_size;
      if not addr^.item.pack then
	desc_size := desc_size div bits_per_unit;
      addr^.item.offset := addr^.item.offset + desc_size;
    end (* if flexible heap data *);
  end;

  if addr^.item.pack
    then init_node (addr, byte_ptr_desc)
    else init_node (addr, pointer_desc);
 end;
$PAGE alloc_dynamic_temp, alloc_temp
(* ALLOC DYNAMIC TEMP emits an alc_temp_op to allocate a non fixed size temporary
   on the stack frame of the executing block.  "Ssize" is an expr denoting the
   size to be allocated.  An addr_ref is returned, pointing to the allocated
   storage.

   When we are finished with the temporary, the stack must be contracted.
   The lifetime of such temporaries extends to the end of the statement in
   which they are introduced.  That is, they will not be referenced after the
   end of the statement, and it is difficult to determine if the temporary
   becomes free before the end of the statement.  Therefore, the flag "temp_used"
   is used to signal the need to reset the stack; it is examined in "expand"
   when a "start_stmt" operator is processed. *)

var temp_used: boolean;                 (* initialized at start of "expand" *)


function alloc_dynamic_temp (ssize: expr): expr;
 begin
  new (alloc_dynamic_temp, addr_ref, addr_ref);
  with alloc_dynamic_temp^ do begin
    item := var_item;
    item.index := op_1 (alc_temp_op, pointer_desc, ssize);
  end;
  init_node (alloc_dynamic_temp, pointer_desc);
  temp_used := true;
 end;



(* ALLOC TEMP allocates a fixed sized temporary in the stack frame of the current
   block.  The parameter "ssize" gives the number of storage units to allocate;
   an addr_ref is returned for the allocated storage. *)

function alloc_temp (ssize: unit_range): expr;
 begin
  alloc_temp := alloc_dynamic_temp (int_const (ssize));
 end;
$PAGE set_temp
(* SET TEMP allocates a set temporary of possibly flexible extents, and returns
   an expr giving a descriptor of the size of the temp.  The parameters "slwb"
   and "slen" are expr's giving the lower bound and length of the set temporary
   to be allocated;  "sdesc" is a descriptor. *)

function set_temp (sdesc: expr_type_desc; slwb: expr; slen: expr): expr;

  var
    op, saddr: tuple;
    ssize: unit_range;

begin
  (* If the set has a fixed length, or a reasonable bound on a flexible size,
     then allocate a fixed length temporary to hold the set. *)

  if sdesc.set_cst_len or (sdesc.set_length < (10 * bits_per_unit)) then begin
    ssize := (sdesc.set_length + (bits_per_unit - 1)) div bits_per_unit;
    saddr := alloc_temp (ssize);
  end

  (* Otherwise, must dynamically allocate a temporary of the appropriate size
     on the stack. *)

  else begin
    op := op_2 (iadd_op, len_desc, slen, int_const (bits_per_unit - 1));     (* runtime size calc *)
    op := op_2 (idiv_op, len_desc, op, int_const (bits_per_unit));
    saddr := alloc_dynamic_temp (op);
  end;

  (* Construct the descriptor for the temporary allocated.  Note that the slwb
     and slen exprs are valid even if the set has constant bounds. *)

  set_temp := op_3 (desc_ref, sdesc, saddr, slwb, slen);
end;
$PAGE padded_set_operand
(* PADDED SET OPERAND is used to shape the operands of the set operators.  It
   insures (via a conversion if necessary) that the operand has a specified
   lowerbound; however, the length of the operand may extend up to the next
   full word boundary (of the target length).  That is, extra bits can be left
   in the operand because the caller knows that implicit masking of those bits
   will occur.  Applicable only to short sets. *)

function padded_set_operand ( opdesc: expr_type_desc; op: expr ): expr;

 var
   src: expr;
   len, opreglen: set_range;

 begin
  padded_set_operand := expand_short_set_op (opdesc, op);
  with padded_set_operand^ do begin
    if (opcode = setcvt_op) andif (operand[1]^.opcode <> desc_ref) then begin (* operand needed conversion *)
      src := operand[1];                        (* the converted set *)

      len := src^.desc.set_length;      (* get number of significant bits after shifting *)
      len := max (0, min ( len + (src^.desc.set_lwb - opdesc.set_lwb), 72 ) );
      opreglen := ngm (opdesc.set_length, bits_per_unit);

      if (((opreglen-36) < len) and (len <= opreglen)) then begin       (* if word length okay *)
        if desc.set_lwb = src^.desc.set_lwb
          then padded_set_operand := src        (* no conversion required *)
          else (* setcvt *) desc.set_length := len;     (* just shift required *)
      end

      else begin                        (* word length different *)
        if opreglen < len
          then desc.set_length := opreglen;     (* can allow extra bits up to length of register *)
      end;
    end;
  end (* with *) ;
 end;
$PAGE expand_short_set_op
(* EXPAND SHORT SET OP shapes set operations whose length is constrained to be
   less than some preferred length.  (For the PDP-10, the length of a shiftable
   register pair, 72.)  Operands which have longer lengths are truncated by
   changing their descriptors (desc_ref's) or result type info.  "Tdesc" gives
   the required result type;  "se" is the operand to be processed. *)

function expand_short_set_op (* (tdesc: expr_type_desc; se: expr): expr *) ;

  var
    ndesc: expr_type_desc;
    newse: expr;
    saddr: mem_addr_desc;
    tupb, supb, lwb, len: int_type;
    i: 0..4095;

begin
  newse := se;                  (* default is not to alter original *)
  with se^ do begin
    case opcode of

      (* In the case of a memory reference to a short set, no shaping is required,
         if it is not of the correct short type, it can always be converted to
         the required type in a register. *)

      mem_ref:
	if desc.set_length = 0 then begin (* prevent accessing of (effectively) null object *)
	  new (newse, gen_set_op, gen_set_op, 0);
	  init_node (newse, tdesc)
	end;

      (* In the case of a function call, if the result is a short set, the
         value can be used directly.  If the result is a long set, it must
         first be placed in a long temporary, then shortened. *)

      func_call_op:
        begin
          if desc.set_length > 72       (* recurse to extract portion from desc_ref of result temp *)
            then newse := expand_short_set_op (tdesc, set_reference (se));
        end;

      (* In the case of a memory reference to a long set, we attempt to convert
         the set to a short set containing at least those elements in the
         intersection of its range and the desired range.  Extra elements are
         removed by introduction of a set conversion if necessary.  In essence,
         we attempt to take a slice of the long set.   If the required slice
         crosses two word boundaries, so that it cannot be converted by shifting
         in even a double register, we generate setcvt (desc_ref). *)

      desc_ref:
        begin
          if desc.set_cst_len then begin

            (* Determine the lwb and start address of the section of the original
               set to be used.  Remember that since it has a size > 72, it cannot
               be packed. *)

            if desc.set_lwb >= tdesc.set_lwb then begin
              lwb := desc.set_lwb; (* take slice starting at original origin *)
              saddr := operand[1]^.item;        (* op1 is addr (set) *)
            end
            else begin (* take slice beginning in middle of original set *)
              lwb := desc.set_lwb + (* starting at first preceding unit boundary *)
                ((tdesc.set_lwb - desc.set_lwb) div bits_per_unit) * bits_per_unit;
              saddr := operand[1]^.item; (* address loc -> new lwb *)
              saddr.offset := saddr.offset + ((lwb - desc.set_lwb) div bits_per_unit);
            end;

            (* Compute the length of the slice. *)

            tupb := tdesc.set_lwb + tdesc.set_length - 1;       (* upb of target *)
            supb := desc.set_lwb + desc.set_length - 1;         (* upb of source *)
            if supb < tupb
              then len := max (0, supb - lwb + 1)               (* truncate set at end of original *)
              else begin                                        (* truncate at end of word containing bit tupb *)
                len := max (0, tupb - lwb + 1);
                len := ngm (len, bits_per_unit);
              end;

            (* If the length of the slice is 72 bits or less, then we can replace
               the desc_ref by a mem_ref. *)

            if len <= 72 then begin
              ndesc := tdesc;
              ndesc.set_lwb := lwb;
              ndesc.set_length := len;
              new (newse, mem_ref, mem_ref);
              init_node (newse, ndesc);
              newse^.item := saddr;
            end;
          end;
        end;

      (* In the case of a gen_set_op, it is only necessary to tag the result with the
         type that is desired;  the code generator will insure that the set is generated
         properly, complete with any implied truncation. *)

      gen_set_op:
        begin
          if upperbound (operand) = 0
            then begin                          (* no op0 must generate directly *)
	      new (newse, gen_set_op, gen_set_op, 0);
              init_node (newse, tdesc);
            end
          else if upperbound (operand) = 1
            then newse := op_1 (gen_set_op, tdesc, operand[1])
          else newse := op_2 (gen_set_op, tdesc, operand[1], operand[2]);
        end;

      (* For set operations, the result cannot involve elements outside of the
         intersection of the target set's bounds and the natural bounds of the
         operation.  This observation is applied in the following way:  if the
         natural and target dimensions do not involve the same number of words,
         then the bounds with the shortest length is used;  otherwise, we select
         the bounds in order to minimize conversions (shifting and masking). *)

      union_op, diff_op, both_op:
        begin
	  if ngm (desc.set_length, bits_per_unit) = ngm (tdesc.set_length, bits_per_unit)
            then begin
              if desc.set_lwb = tdesc.set_lwb
                then ndesc := desc      (* result will only differ in length *)
                else ndesc := tdesc;    (* must shift result, generally better to
                                           shift the operands instead. *)
            end
          else if tdesc.set_length < desc.set_length
            then ndesc := tdesc
          else ndesc := desc;
          new (newse, nary_op, nary_op, upperbound (operand));        (* rebuild the operation at the derived precision *)
          newse^.opcode := opcode;

          if opcode = union_op then begin               (* all operands have result precision *)
            for i := 1 to upperbound (operand) do
              newse^.operand[i] := expand_short_set_op (ndesc, operand[i]);
          end

          (* Since A - B == A andcm B, there is an implicit mask of the second
             operand by the first, therefore we can permit it to be longer than
             the first operand. *)

          else if opcode = diff_op then begin   (* only has two operands *)
            newse^.operand[1] := expand_short_set_op (ndesc, operand[1]);
            newse^.operand[2] := padded_set_operand (ndesc, operand[2]);
          end

          (* In intersection, there is again implicit masking of the longer
             operand by the shorter one.  Therefore, we allow the operands to
             extend up to the next full word.  The result length is adjusted to
             the minimum length of the operands. *)

          else (* opcode = both_op *) begin
            newse^.operand[1] := padded_set_operand (ndesc, operand[1]);
            newse^.operand[2] := padded_set_operand (ndesc, operand[2]);
            ndesc.set_length := min ( newse^.operand[1]^.desc.set_length,
                                      newse^.operand[2]^.desc.set_length  );
          end;

          init_node (newse, ndesc);
        end;

        (* For a set conversion, the operand is shaped to the result type of the
           setcvt_op.  If the result of the shaping yields the desired range, the
           conversion is removed, otherwise it must be left to force the code
           generator to perform the conversion.  (The conversion is reintroduced
           by the general check at the end of this routine.) *)

        setcvt_op:
          newse := expand_short_set_op (desc, operand[1])      (* try for implicit conversion *)

        end (* case *);
    end (* with *);

    (* If the above transformations yield a set which is not of the target type,
       then a setcvt_op is introduced to cause the code generator to perform the
       conversion.  Note that it is introduced only when a shift or mask operation
       will actually be required. *)

    if (newse^.desc.set_lwb <> tdesc.set_lwb) or        (* shift required *)
       (newse^.desc.set_length > tdesc.set_length) or   (* masking required *)
       (ngm (newse^.desc.set_length, bits_per_unit) <> ngm (tdesc.set_length, bits_per_unit))
      then newse := op_1 (setcvt_op, tdesc, newse);      (* or truncation required;  note that zero
                                                           filled sets need no conversion. *)

    expand_short_set_op := newse;       (* return possibly altered result *)
  end;
$PAGE implict_set_op
(* IMPLICIT SET OP converts a gen_set_op to a set_op.  The former is used when
   a (short) set is to be generated -- i.e. it represents a set value; the latter
   is used when the operands are to be referenced -- i.e. the set_op is a place
   holder.  One example of the use of a set_op is for "x in [a..b]" where it is
   inefficient to actually generate the set to perform the test. *)

function implicit_set_op (sdesc: expr_type_desc; se: expr): expr;
 var i: index_range;
 begin
  new (implicit_set_op, set_op, set_op, upperbound (se^.operand));
  with implicit_set_op^ do begin
    for i := 1 to upperbound (operand) do operand[i] := se^.operand[i];
  end;
  init_node (implicit_set_op, sdesc);
 end;
$PAGE expand_set_op
(* EXPAND SET OP expands operations of the form t := set expr into a series of
   assignments to the target or temporaries of the same size.   The "target" is
   given as a descriptor, and "se", the set expr, is a possibly transformed
   tree. *)

procedure expand_set_op (target: expr; se: expr);

  var
    temp: expr;
    i: 0..4095;

begin
  (* If the target has an (approximate length) below the preferred set size,
     then the source expression can be evaluated in registers and assigned. *)

  if target^.opcode = mem_ref then begin        (* short set, len <= 72 bits *)
    assign (target, expand_short_set_op (target^.desc, se));
    return;
  end;

  (* Otherwise, the assignment must be compiled as a storage to storage move. *)

  with se^ do begin
    case opcode of

      desc_ref:
        assign (target, se);

      func_call_op:
        begin
          if (desc.set_length <= 72) or (desc.set_lwb <> target^.desc.set_lwb)
				     or (desc.set_length <> target^.desc.set_length)
            then assign (target, set_reference (se))    (* copy to temp before descriptor move *)
            else assign (target, se);           (* return value directly in temp *)
        end;

      mem_ref:                  (* assigning short to long *)
        assign (target, set_reference (se));    (* set_ref generates desc for short set *)

      setcvt_op:
        expand_set_op (target, operand[1]);     (* perform the conversion in the move *)

      gen_set_op:
        assign (target, implicit_set_op (target^.desc, se));

      (* For t := f (op1, op2, ... opn), generate t := op_1; t := f (t, op2), and
         so forth.  Intermediate temporaries are created for the opn as required, i.e.
         where the opn is not a memory reference and involves a different function.
         Note that if the operator yields a short result, we will cause it to
         be evaluated in the registers. *)

      union_op, diff_op, both_op:
        begin
          if (desc.set_length <= 72) and desc.set_cst_len              (* then evaluate in regs and put in storage *)
            then assign (target, set_reference (se))
          else begin                            (* storage to storage operators *)
            expand_set_op (target, operand[1]);
            temp := nil;            (* allocate and reuse temporary if needed *)
            for i := 2 to upperbound (operand) do begin
              if (operand[i]^.opcode = desc_ref) andif (operand[i]^.desc.set_lwb = target^.desc.set_lwb)
                then assign (target, op_2 (opcode, desc, target, operand[i]))   (* assign directly to set *)
              else if (operand[i]^.opcode = mem_ref) andif (operand[i]^.desc.set_lwb = target^.desc.set_lwb)
                then assign (target, op_2 (opcode, desc, target, set_reference (operand[i])))
              else begin
                if temp = nil (* create a temporary if needed *)
                  then temp := set_temp (target^.desc, target^.operand[2], target^.operand[3]);
                expand_set_op (temp, operand[i]);
                assign (target, op_2 (opcode, desc, target, temp));
              end;
            end;
          end;
        end

    end (* case *) ;
  end (* with *) ;
end;
$PAGE set_lowerbound
(* SET LOWERBOUND calculates an apparent lowerbound for a set expression.  That
   is, it derives an origin for a temporary used to hold the expression.  If there
   are flexible sets involved, an a constant bound can be found, the constant
   bound is used despite the fact that a bound computed at runtime might be better.
   A nonconstant bound may be returned by this routine, however, if there is no
   other valid choice.  "Se" is the set expression; an expr tree giving the
   lower bound is returned. *)

function set_lowerbound (se: expr): expr;
  var lwb: expr;
      i: 0..4095;
begin
  with se^ do begin
    if desc.set_cst_lwb
      then lwb := scalar_const (desc.base, desc.set_lwb)        (* obviously constant *)
      else 
        case opcode of

          desc_ref:
            lwb := operand[1];

          gen_set_op:
            lwb := operand[1];

          (* For +, the lower bound is the minimum of the operand lower bounds.
             For *, it is the maximum, since all elements must be in both sets.
             For -, it is the lower bound of the first operand, since all elements
             must be in the first set, but not necessarily in the second. *)

          union_op, both_op:
            begin
              if opcode = union_op
                then new (lwb, imin_op, imin_op, upperbound (operand))
                else new (lwb, imax_op, imax_op, upperbound (operand));
              for i := 1 to upperbound (operand) do
                lwb^.operand [i] := set_lowerbound (operand[i]);
              init_node (lwb, full_int_desc);
              lwb^.desc.kind := desc.base^.kind;  (* patch desc to show correct kind *)
              lwb^.desc.signed := desc.base^.minval < 0;
            end; (* union, both *) 

          diff_op:
            lwb := set_lowerbound (operand[1])

        end (* case *) ;
    set_lowerbound := lwb;              (* return the computed result *)
  end (* with *) ;
end;
$PAGE set_upperbound
(* SET UPPERBOUND calculates an apparent upper bound for a set expression.  Its
   usage and function corresponds to set_lowerbound. *)

function set_upperbound (se: expr): expr;
  var upb: expr;
      i: 0..4095;
begin
  with se^ do begin
    if desc.set_cst_len and desc.set_cst_lwb
      then upb := int_const (desc.set_length + desc.set_lwb - 1)
      else 
        case opcode of

          desc_ref:
            begin
              upb := op_2 (iadd_op, full_int_desc, operand[3], operand[2]);    (* upb := len + lwb - 1 *)
              upb := op_2 (isub_op, full_int_desc, upb, int_const (1));
            end;

          gen_set_op:
            begin
              if upperbound (operand) = 1
                then upb := operand[1]
                else upb := operand[2]
            end;

          (* For +, the upperbound is the maximum of the operand upper bounds.
             For *, it is the minimum.  For -, it the first operand upper bound.
             (See the previous page.) *)

          union_op, both_op:
            begin
              if opcode = union_op
                then new (upb, imax_op, imax_op, upperbound (operand))
                else new (upb, imin_op, imin_op, upperbound (operand));
              for i := 1 to upperbound (operand) do
                upb^.operand[i] := set_upperbound (operand[i]);
              init_node (upb, full_int_desc);
            end;

          diff_op:
            upb := set_upperbound (operand[1])

        end (* case *) ;
    upb^.desc.kind := desc.base^.kind;      (* patch in correct kind *)
    upb^.desc.signed := desc.base^.minval < 0;
    set_upperbound := upb;
  end (* with *) ;
end;
$PAGE set_reference
(* SET REFERENCE generates a descriptor (address, lower bound and length) for
   a set value ("se");  if se denotes an expression, then the expression is
   copied to a temporary, and a descriptor for the temporary is returned. *)

function set_reference (* (se: expr): expr *) ;

  var
    slen, slwb: expr;
    temp, stemp: expr;

begin
  with se^ do begin
    if opcode = desc_ref
      then set_reference := se          (* direct memory reference *)

    else begin          (* must copy and/or generate descriptor *)
      slwb := set_lowerbound (se);      (* get parameters *)
      if desc.set_cst_len
        then slen := int_const (desc.set_length)
        else begin (* must compute length at runtime *)
          slen := op_2 (isub_op, full_int_desc, set_upperbound (se), slwb);
	  slen := op_2 (imax_op, len_desc, slen, int_const (0));
          slen := op_2 (iadd_op, len_desc, slen, int_const (1))
        end;

      (* If we have a short set, then we can make a descriptor for it if it is
         unpacked;  otherwise, it must be copied to an aligned temporary before
         a descriptor can be generated. *)

      if (opcode = mem_ref) andif (not item.pack)       (* can generate desc, otherwise, treat as expression *)
        then set_reference := op_3 (desc_ref, desc, addr (se), slwb, slen)

      else begin                (* some set expression *)
        temp := set_temp (desc, slwb, slen);          (* allocate temp -> desc *)

        if (not desc.set_cst_len) or (desc.set_length > 72)
          then expand_set_op (temp, se)         (* long set operation *)

        else begin                              (* evaluate short expr and store in temp *)
          new (stemp, mem_ref, mem_ref);        (* must copy expression to mem_ref *)
          init_node (stemp, desc);
          stemp^.item := temp_item;
          stemp^.item.index := temp^.operand[1];        (* address is 0(addr(temp)) *)
          assign (stemp, expand_short_set_op (desc, se));
        end;

        set_reference := temp;
      end;
    end;
  end (* with *) ;
end;
$PAGE long_set_operand
(* LONG SET OPERAND generates a descriptor for a set value ("se").  If necessary,
   the set is converted to the lower bound, but not necessarily length, specified
   by "tdesc" and "lwb". *)

function long_set_operand (tdesc: expr_type_desc; lwb: expr; se: expr): expr;
 var len, temp: expr;
 begin
  with se^ do begin
    if desc.set_cst_lwb and (desc.set_lwb = tdesc.set_lwb) then begin
      long_set_operand := set_reference (se);
      return;                           (* <--- exit here if set already has lwb *)
    end;                                (* else continue, performing conversion *)

    len := op_2 (isub_op, full_int_desc, set_upperbound (se), lwb);
    len := op_2 (iadd_op, full_int_desc, len, int_const (1));
    len := op_2 (imax_op, len_desc, len, int_const (0));

    temp := set_temp (tdesc, lwb, len);
    expand_set_op (temp, se);
    long_set_operand := temp;
  end (* with *) ;
 end;
$PAGE str_length
(* STR LENGTH creates a tree which computes the length function.  "Str" is the
   string, and the return value is the tree.  This handles all variations
   of fixed, varying and flexible strings. *)

function str_length (str: expr): expr;

 var
   i: 0..4095;
   slen, soff, sdesc: expr;

 begin
  with str^ do begin
    if desc.kind in scalar_types (* just a character *)
      then str_length := int_const (1)
    else if (desc.str_kind = nonvarying) and (not desc.str_flex) (* fixed length string *)
      then str_length := int_const (desc.str_length)

    else case opcode of (* dispatch to handle other cases *)

      (* If the operand is some kind of reference, then the length has already
         been generated and placed in a descriptor when the str node itself
         was processed.  It is important that this length be used, so that
         length nodes are unique for a given input node. *)

      ident_ref, field_ref, array_ref, ptr_ref, substr_ref:
        str_length := result^.operand[2];       (* result -> desc, op2 is length *)

      (* If the operand is a constant, then the length is also a constant *)

      cst_ref:
        str_length := int_const (length (cst_val.valp^.str_val));

      (* Case translation functions:  length (upper/lowercase(s)) = length(s).
         String conversion: if flexible or varying result, length is from operand. *)

      upc_op, lwc_op, strcvt_op:
        str_length := str_length (operand [1]);

      (* Concatenation: the length is the sum of the length of the strings
         joined together. *)

      cat_op:
        begin
          str_length := str_length (operand[1]);
          for i := 2 to upperbound (operand) do begin
            slen := str_length (operand[i]);
            str_length := op_2 (iadd_op, len_desc, str_length, slen);
          end
        end;

      (* Function call: force evaluation of the call, and get the
         length from the descriptor. *)

      func_call_op:
        str_length := str_length (str_reference (str));

      (* Descriptor reference: the length is the second operand. *)

      desc_ref:
        str_length := operand[2]

    end (* case opcode *) ;
  end (* with *) ;
 end;
$PAGE str_value
(* STR VALUE develops a string operand for use in a context where the result will
   be moved to some well defined target.  Therefore, string expressions need not
   be placed in a temporary, as the target may be used instead.  Thus, this will
   return any of following kinds of nodes:  desc_ref, upc/lwc_op, cat_op,
   and character values/expressions.  String conversions are removed, and string
   functions copied to a temporary of appropriate size.  The parameter "op" is
   the node to be processed, and the transformed result is returned. *)

function str_value (op: expr): expr;
 begin
  str_value := op;      (* net transformation is to remove conversions *)
  while str_value^.opcode = strcvt_op do
    str_value := str_value^.operand[1];
  str_value := transform (str_value);   (* operand of strcvt is not transformed *)
  if (str_value^.opcode = func_call_op) or (str_value^.opcode = date_op) then
    str_value := str_reference (str_value);
 end;
$PAGE str_temp
(* STR TEMP allocates a string (char) temporary of possibly flexible length,
   and returns a descriptor of the temporary created.  "Desc" is the type
   descriptor of the target; "supb" gives the size of the string desired in
   characters. *)

function str_temp (desc: expr_type_desc; supb: expr): expr;

 const str_lw_size = 1;
 var
  saddr, slen: expr;
  op: expr;
  ssize: unit_range;

 begin

  (* If the target is a character, we need only a one word temporary. *)

  if desc.kind in scalar_types (* any scalar can be sclcvt'ed to a char *)
    then saddr := alloc_temp (1)

  (* If the temporary has a fixed length, then it can be allocated directly. *)

  else if not desc.str_flex then begin
    ssize := (desc.str_length + chars_per_unit - 1) div chars_per_unit;
    if desc.str_kind = varying
      then ssize := ssize + str_lw_size;
    saddr := alloc_temp (ssize);
  end

  (* If the temporary has a flexible length, then we must compute the unit size
     of the temporary and make a dynamic allocation. *)

  else begin
    ssize := chars_per_unit - 1;
    if desc.str_kind = varying then (* allow for length word *)
      ssize := ssize + (str_lw_size * chars_per_unit);
    op := op_2 (iadd_op, len_desc, supb, int_const (ssize));
    op := op_2 (idiv_op, len_desc, op, int_const (chars_per_unit));
    saddr := alloc_dynamic_temp (op);
  end;

  (* If the temporary is a varying string, then the address should address
     the first word containing characters and the length should be a mem_ref
     to the length word. *)

  if (desc.kind = strings) andif (desc.str_kind = varying)
    then begin
      new (slen, mem_ref, mem_ref);
      init_node (slen, len_desc);
      slen^.item := saddr^.item;
      saddr^.item.offset := saddr^.item.offset + 1;
    end
    else slen := supb;

  (* Construct the descriptor for the temporary.  Note that slen is correct
     even if the string has a constant length. *)

  str_temp := op_2 (desc_ref, desc, saddr, slen);
 end;
$PAGE vs_move_length
(* VS MOVE LENGTH generates an IF tree to calculate the number of characters to
   move in the assignment of a string ("source") to a varying "target" string.
   The expression "target_upb" is an upper bound on the length of the target
   string; this will be a constant, unless the target string is flexible.

   In general, the move length is "min (upperbound (target), length (source))".
   However, if neither the source nor the target string is flexible, then
   the "min" operation may be eliminated in two cases:

   (1)  If the maximum possible source string size is less than the target
        string upperbound, then the entire source string may always be copied.

   (1a) In the above case, if the source is a function call, then we don't
        know its length, but its length is just a formality anyways, (since
        we only pass an address to the function), so we can use the target
        string length.

   (2)  If the source string is nonvarying, and is longer than the upper bound
        of the target string, then there are enough characters to fill the
        target string to its maximum length. *)

function vs_move_length ( target, source, target_upb: expr ): expr;

 var source_varying: boolean;
     source_bound: char_range;

 label 100 (* cannot optimize *);

 begin
  if target^.desc.str_flex then
    goto 100; (* unbounded target string *)

  with source^.desc do begin
    if kind in scalar_types then begin (* any scalar can be sclcvt'ed to a char *)
      source_varying := false;
      source_bound := 1;
    end
    else (* kind = strings *) begin
      if str_flex then
        goto 100; (* unbounded source string *)
      source_varying := (str_kind = varying);
      source_bound := str_length;
    end;
  end;

  if source_varying and (source_bound > target^.desc.str_length) then
    goto 100; (* source string might be longer or shorter *)

  (* One of the above optimizations is possible. *)

  if (source_bound > target^.desc.str_length) orif (* case (2) above *)
     (source^.opcode = func_call_op) then (* case (1a) above *)
    vs_move_length := target_upb
  else (* case (1) above *)
    vs_move_length := str_length (source);
  return; (* <---- exit early with optimized result *)

 100 (* no optimization possible *):

  vs_move_length := op_2 (imin_op, len_desc, str_length (source), target_upb);
 end;
$PAGE length_word_address
(* LENGTH WORD ADDRESS takes a reference to the address of the text word of
   a varying string, and returns an addr_ref to the length word of the string. *)

function length_word_address ( str_addr: expr ): expr;
 var saved_cursor: tuple;
 begin
  saved_cursor := if_cursor;
  t_set (str_addr);
  new (length_word_address, addr_ref, addr_ref);
  init_node (length_word_address, pointer_desc);
  if str_addr^.opcode = addr_ref then begin
    length_word_address^.item := str_addr^.item;
    length_word_address^.item.offset := length_word_address^.item.offset - 1;
  end
  else begin
    length_word_address^.item := var_item;
    with length_word_address^.item do begin
      index := str_addr;
      offset := -1;
    end;
  end;
  t_set (saved_cursor);
 end;
$PAGE expand_str_move
(* EXPAND STR MOVE generates an assignment of a string expression ("src") to a
   a string temporary ("target").  If the temporary is known to have been
   created with the same length as the source string, then "t_len" will be
   an expression representing that length. *)

procedure expand_str_move ( target: expr; src: expr; t_len: expr );
 var len, tdesc, t_src, mv_len: expr;
 begin

  t_src := src;
  while t_src^.opcode = strcvt_op do (* any conversion is done by the move *)
    t_src := t_src^.operand[1];

  (* If the target is a fixed length string, or a character, then all that is
     required is to perform a simple assignment. *)

  if (target^.desc.kind <> strings) orif (target^.desc.str_kind <> varying) then
    assign (target, t_src)

  (* If the source is a function call, then the length word of the string may
     have to be set, but the function will set it, so a simple assignment is
     still sufficient.  However, the address of the length word of the string,
     rather than its text, must be passed to the function. *)

  else if src^.opcode = func_call_op then begin
    tdesc := op_2 (desc_ref, target^.desc, length_word_address (target^.operand[1]),
                                          target^.operand[2]);
    assign (tdesc, src);
  end

  (* If the target is a varying length string, then we must move the source 
     string to the target and assign the length of the source to the length
     word of the target.  If the temporary length is known to be the same as
     the source length, it will have been passed as "t_len".  Otherwise, the
     number of characters to move must be computed.  Presumably, if a flexible
     varying temporary is created, it will be created at just the right length,
     and "t_len" can be used; thus, we if t_len is nil, we can assume a constant
     upper bound for the target string length. *)

  else begin
    new (len, mem_ref, mem_ref);                (* create a ref to the length word *)
    len^.item := target^.operand[2]^.item;      (* op2 is mem_ref (length word) of temp; we make
                                                    a copy since op2 is for rhs use *)
    if t_len = nil then begin
      mv_len := int_const (target^.desc.str_length);
      mv_len := vs_move_length (target, src, mv_len);   (* get number of chars to move *)
    end
    else
      mv_len := t_len;
    tdesc := op_2 (desc_ref, target^.desc, target^.operand[1], mv_len);
    assign (tdesc, t_src);                      (* move to desc_ref (addr (text), upb) *)
    init_node (len, len_desc);              (* assign no of chars moved to target length word *)
    assign (len, mv_len);
  end;
 end;
$PAGE str_char_temporary
(* STR CHAR TEMPORARY allocates a one_word temporary and assigns a character-
   valued expression to it.  The expression is assigned to a character mem_ref,
   and this function then returns a desc_ref so that the location can be used
   as a one-character string. *)

function str_char_temporary ( str: expr ): expr;

 var temp_addr, temp: expr;

 begin
  temp_addr := alloc_temp (1);
  new (temp, mem_ref, mem_ref);
  init_node (temp, char_desc);
  temp^.item := temp_item;
  temp^.item.index := temp_addr;
  assign (temp, str);
  str_char_temporary := op_2 (desc_ref, char_desc, temp_addr, int_const (1));
 end;
$PAGE str_reference
(* STR REFERENCE generates a descriptor (address and length) for a string or
   character value ("str").  A packed address may be generated in the case of
   a substring reference, or a packed character.  If "str" denotes an expression
   then the expression is copied to a temporary, and a descriptor for the
   temporary is returned. *)

function str_reference (* str: expr): expr *) ; (* the descriptor is the return value *)

 var
  saddr, slen, temp: expr;
  char_val: val;

 begin
  with str^ do begin    (* str assumed to be transformed operand *)
    case opcode of

      (* Memory accesses will have already been expanded when those nodes were
         originally seen in the input.  Thus the result field of string vars
         will be a descriptor;  however, character items will be either immed_ref's
         or mem_ref's and must be processed specially. *)

      immed_ref:
        begin
          new (saddr, addr_ref, addr_ref);
          char_val.kind := scalar_cst;
          char_val.ival := item.offset;
          const_access (char_val, saddr);
          init_node (saddr, pointer_desc);
          str_reference := op_2 (desc_ref, char_desc, saddr, int_const (1));
        end;

      mem_ref:
        begin
          new (saddr, addr_ref, addr_ref);
          if item.pack
            then init_node (saddr, byte_ptr_desc)
            else init_node (saddr, pointer_desc);
          saddr^.item := item;
          if item.base <> nil then
            offset_byte_address (saddr);
          str_reference := op_2 (desc_ref, char_desc, saddr, int_const (1));
        end;


      desc_ref:
        str_reference := str;                   (* return original descriptor *)

      (* In the case of a string expression, it must be copied to an aligned
         temporary, so that we may generate the address of the entire value. *)

      func_call_op:
        if transform (str) <> str then (* call has already been evaluated *)
          str_reference := result
        else begin (* must evaluate the call now *)
          if desc.kind = strings then begin
            slen := int_const (desc.str_length);        (* func's can only return nonflexible strings *)
            temp := str_temp (desc, slen);      (* get a temporary to copy the result to *)
            str_reference := temp;              (* desc returned shows addr (text) + current length *)
            if desc.str_kind = varying then begin       (* must pass address of start of length word *)
              new (saddr, addr_ref, addr_ref);
              init_node (saddr, pointer_desc);
              saddr^.item := temp^.operand[2]^.item;    (* op2 is length word, copy address *)
              temp := op_2 (desc_ref, desc, saddr, slen);
            end;
            assign (temp, str);
          end
          else                          (* character valued function *)
            str_reference := str_char_temporary (str);
          result := str_reference; (* save for future reference *)
        end;

      others:
        begin
          if (opcode = strcvt_op) andif (desc.str_kind = varying)
            then slen := vs_move_length (str, operand[1], int_const (desc.str_length))
            else slen := str_length (str);
          str_reference := str_temp (desc, slen);
          expand_str_move (str_reference, str, slen);
        end

    end (* case *) ;
  end (* with *) ;
 end;
$PAGE sv_set_operand, sv_str_operand
(* SV SET OPERAND shapes the set argument to a search or verify function.  It
   takes a set expression ("se") and shapes it to either (1) a gen_set_op of
   one or two arguments, (2) a set in the range ' '..'g' (a 72 bit set covering
   most of the interesting printable characters) or (3) a long set, referenced
   by descriptor. *)

function sv_set_operand (se: expr): expr;
 begin
  if se^.opcode = gen_set_op
    then begin
      if upperbound (se^.operand) = 0
        then sv_set_operand := expand_short_set_op (ascii_set, se)      (* silly case, avoid failure *)
        else sv_set_operand := implicit_set_op (se^.desc, se)
    end
  else if (ascii_set.set_lwb <= se^.desc.set_lwb) and   (* if set is in range of ascii_set *)
          ((se^.desc.set_lwb + se^.desc.set_length - 1) <=
                    (ascii_set.set_lwb + ascii_set.set_length - 1))
    then sv_set_operand := expand_short_set_op (ascii_set, se)
  else sv_set_operand := set_reference (se);
 end;


(* SV STR OPERAND shaprs the strng argument to a search or verify function.  It
   takes a string expression ("str") and returns either (1) an uppercase operation,
   (2) a character expression, or (3) a descriptor reference. *)

function sv_str_operand (str: expr): expr;
 var base: expr;
 begin
  if str^.desc.kind in scalar_types then
    sv_str_operand := str (* a character is ok *)
  else if str^.opcode = upc_op then begin
    base := str^.operand[1]; (* upc is ok, but operand must be, too *)
    while (base^.opcode = upc_op) or (base^.opcode = lwc_op) do
      base := base^.operand[1];
    sv_str_operand := op_1 (upc_op, str^.desc, sv_str_operand (base));
  end
  else
    sv_str_operand := str_reference (str); (* must convert all others *)
 end;
$PAGE str_address
(* STR ADDRESS is used to form the starting address of a string reference or
   expression ("str") and store the addressing information in "node".  If str
   denotes a string expression, then the expression is stored in a temporary,
   and the address of the temporary returned. *)

procedure str_address (str: expr; node: expr);
 var sdesc: expr;
 begin
  sdesc := str_reference (transform (str));     (* computes address and length *)
  if (sdesc^.operand[1]^.desc.base <> byte_ptr_desc.base) orif
     (sdesc^.operand[1]^.desc.kind <> byte_ptr_desc.kind) orif
     (sdesc^.operand[1]^.desc.signed <> byte_ptr_desc.signed) orif
     (sdesc^.operand[1]^.desc.int_prec <> byte_ptr_desc.int_prec)
    then node^.item := sdesc^.operand[1]^.item  (* copy the addressing info *)
    else begin                                  (* construct a byte address *)
      node^.item := packed_item;
      node^.item.base := sdesc^.operand[1];
      node^.item.size := type_char^.base_size;
    end;
 end;
$PAGE substr_access
(* SUBSTR ACCESS performs the address computation necessary for accessing the
   first character of a substring reference ("ref"), and stores the result in
   "node".  The logic follows that of packed_array_reference, except that this
   is prepared to handle "substr (substr (...), ...)" by combining the two
   index expressions. *)

procedure substr_access (* ref: expr; node: expr *);

 var
   str: expr;                                   (* the string being substrung *)
   idx: expr;                                   (* the evaluated offset *)

 begin
  with ref^ do begin
    str := base_string;
    str_address (str, node);            (* get (packed) addr of base string *)

    (* If the base is an unpacked character (unlikely, but...), then treat the
       character as a byte of width 36. *)

    if (str^.desc.kind in scalar_types) and (not node^.item.pack) then begin
      node^.item.size := bits_per_unit;
      bitindirect (node);
      add_cst_bit_index (node^.item.base, -1);  (* string index is base one *)
    end

    (* If the base string is not a packed reference, then construct a byte
       pointer referencing one character position before the start of the
       string. *)

    else if not node^.item.pack then begin
      node^.item.size := type_char^.base_size;       (* build char byte pointer *)
      bitindirect (node);
      add_cst_bit_index (node^.item.base, -1);  (* compensate for start index *)
    end

    (* If the address is packed, and the base address is of the following form:
       addr 0[idx](base), then we may add the two index expressions. *)

    else if (node^.item.base^.opcode = addr_ref) andif (node^.item.base^.item.index <> nil)
      then node^.item := node^.item.base^.item; (* index is str offset; base is byte ptr *)

    (* Calculate the offset; add to an existing offset in the base (if any); and
       generate a reference indirecting through a byte pointer. *)

    idx := transform (substr_index);
    if node^.item.index <> nil
      then idx := op_2 (iadd_op, len_desc, idx, node^.item.index);
    node^.item.index := idx;
    bitindirect (node);

  end (* with *) ;
 end;
$PAGE str_bptr
(* STR BPTR takes a desc_ref tuple describing a string in storage, and returns
   a byte pointer address tuple for the string (offset back one character, for
   the ILDB with which such addresses are usually attacked). *)

function str_bptr ( sdesc: expr ): expr;

 begin
  str_bptr := sdesc^.operand[1]; (* the address from the string descriptor *)

  if str_bptr^.item.pack or
     ( (str_bptr^.desc.base = byte_ptr_desc.base) andif
       (str_bptr^.desc.kind = byte_ptr_desc.kind) andif
       (str_bptr^.desc.signed = byte_ptr_desc.signed) andif
       (str_bptr^.desc.int_prec = byte_ptr_desc.int_prec) ) then
    return; (* <---- return if already a byte address *)

  new (str_bptr, addr_ref, addr_ref); (* make an address into a byte pointer *)
  init_node (str_bptr, byte_ptr_desc);
  with str_bptr^ do begin
    item := sdesc^.operand[1]^.item;
    item.offset := item.offset * bits_per_unit;
    item.pack := true;
    if sdesc^.desc.kind in scalar_types (* any scalar may be sclcvt'ed to a char *)
      then item.size := bits_per_unit
      else item.size := type_char^.base_size;
  end;
  add_cst_bit_index (str_bptr, -1); (* offset for initial ILDB *)
 end;
$PAGE offset_byte_address

(*  OFFSET BYTE ADDRESS offsets the byte address in a substring reference.
    On entry, the Saddr expression has the form "addr 0(bptr)", where "bptr"
    is "addr 0[idx](base)".  Saddr is replaced by "bptr", the extra level of
    indirection being redundant.

    The resulting byte address points to the first character of a substring.
    This must be offset back one byte, since the first character of a substring
    is loaded by the runtime with an ILDB instruction.  There are two cases to
    be considered.  The base byte pointer may be in memory, or it may be an
    addr_ref which has just been generated.  In the latter case, the bit offset
    in the base pointer may be altered directly.  *)

procedure offset_byte_address (* saddr: expr *);

 var sbp: expr;
     saved_cursor: tuple;

 begin
  saddr^.item := saddr^.item.base^.item;
  sbp := saddr^.item.base; (* the base address *)
  if sbp^.opcode = addr_ref then (* can offset directly *)
    add_cst_bit_index (sbp, -1)
  else begin (* must compute the offset *)
    saved_cursor := if_cursor; (* insert computation out of order *)
    t_set (saddr^.prev);
    saddr^.item.index := op_2 (isub_op, index_desc, saddr^.item.index, int_const (1));
    t_set (saved_cursor);
  end;
 end;
$PAGE is_temporary
(* IS TEMPORARY is called with the right-hand side of a string or set assignment.
   Such an assignment must sometimes be performed via a temporary.  However, if
   the right-hand side has already been evaluated in a temporary (as may be
   necessary, for example, if it is a string-convert op applied to a function
   call), then a second temporary would be redundant.  This function tests
   whether the transformed right-hand side expression is a temporary. *)

function is_temporary (rhs_ref: expr): boolean;

 var ref: expr;

 begin
  if rhs_ref^.opcode <> desc_ref then
    is_temporary := false

  else begin
    ref := rhs_ref^.operand[1];
    while (ref^.opcode = addr_ref) andif
          ( (ref^.item.base <> nil) or (ref^.item.index <> nil) ) do begin
      if ref^.item.base <> nil
        then ref := ref^.item.base
        else ref := ref^.item.index;
    end;
    is_temporary := (ref^.opcode = alc_temp_op);
  end;
 end;
$PAGE expand_exp
(* EXPAND EXP expands expii and expri_op's with positive constant exponents.
   "Op" gives the opcode of the multiplying operator (imul_op or rmul_op);
   "desc" is a descriptor giving the precision and type for the final result;
   "x" is the node being raised to a power (each use increments the usage
   count; and "n" is the power to be computed.  The return value is the tree
   which computes the value.  The expandsion algorithm uses at most one
   register. *)

function expand_exp
  (op: tuple_opcodes; desc: expr_type_desc; x: expr; n: int_type): expr;

 var r: expr;                           (* the result *)
 begin
  if n < 0 then r := nil                (* errorneous case, don't loop *)

  else if n = 0 then                    (* x**0 = 1 *)
    if op = imul_op
      then r := int_const (1)   (* vanilla or chocolate 1 *)
      else r := nil             (***************)

  else if n = 1 then                    (* x**1 = x *)
    r := x

  else if odd (n) then begin            (* x**n = (x**(n-1)) * x *)
    r := expand_exp (op, desc, x, n-1);
    r := op_2 (op, desc, r, x);
  end

  else (* even (n) *) begin     (* x**n = sqr (x ** (n div 2)) *)
    r := expand_exp (op, desc, x, n div 2);
    r := op_2 (op, desc, r, r);
  end;

  expand_exp := r;              (* return result *)
 end;
$PAGE fixed_str_aligned_ref, varying_str_ref
(* These two functions are predicates used in the expansion of strcvt_op in expand
   operator, below.  They return true if the designated not designates a reference
   to a nonvarying or varying data item, respectively.  Note that substr ref's are
   rejected by fixed_str_aligned_ref, because the address cannot designate a packed
   item. *)

function fixed_str_aligned_ref (s: expr): boolean;
 begin
  fixed_str_aligned_ref :=
    (s^.desc.kind = strings) andif              (* check for fixed length, nonvarying string *)
    (s^.desc.str_kind = nonvarying) andif
    (not s^.desc.str_flex) andif
    (s^.opcode = desc_ref) andif                (* memory reference ?? *)
    (not s^.operand[1] (* addr *)^.item.pack);  (* aligned ?? *)
 end;



function varying_str_ref (s: expr): boolean;
 begin
  varying_str_ref :=
   (s^.desc.kind = strings) andif               (* check for fixed max length, varying string *)
   (s^.desc.str_kind = varying) andif
   (not s^.desc.str_flex) andif
   (s^.opcode = desc_ref);                      (* memory reference ?? *)
 end;
$PAGE is_iconstp
(* IS_ICONSTP is a predicate which determines if is operand ("node") represents
   a constant.  It is assumed that the type of node is ints.  If the value is
   a constant, the parameter "c" is set to the value of the constant, else
   it is set to zero. *)

function is_iconstp (node: expr; var c: int_type): boolean;
 begin
  is_iconstp := false;
  c := 0;
  with node^ do begin
    if item.index = nil then begin
      if opcode = immed_ref then begin
        is_iconstp := true;
        c := item.offset;
      end
      else if (opcode = mem_ref) andif
              (item.class = constant_sc) andif
              (item.cstref^.kind = scalar_cst) then begin
        is_iconstp := true;
        c := item.cstref^.scalar_val;
      end
    end
  end (* with *) ;
 end;
$PAGE last_field
(* LAST FIELD returns a FieldRef tuple referring to the last field in a
   record expression with no variants. *)

function last_field (rec: expr): expr;

 var fld_sym: sym;

 begin
  fld_sym := rec^.desc.base^.field_list;
  while fld_sym^.next <> nil do
    fld_sym := fld_sym^.next;
  assert (fld_sym^.fld_variant = rec^.desc.base);
  new (last_field, field_ref, field_ref);
  initexpr (last_field, fld_sym^.type_desc);
  last_field^.base_rec := rec;
  last_field^.field_sym := fld_sym;
  emit (last_field);
 end;
$PAGE assign_agg_val
(* ASSIGN AGG VAL assigns an aggregate constructor to a destination aggregate,
   one component at a time.  The assignments are generated using field and
   array accessing operators, and then re-expanded.  The Dest argument is
   the ultimate destination of the constructed expression, while the Lhs
   argument is the destination of the immediate assignment, and may be
   either the same as Dest, or an intermediate temporary. *)

procedure assign_agg_val (lhs, rhs, dest: expr);

 var start_code: tuple;
     index_type, comp_type: typ;
     i: oper_range;
     component, upb: expr;
     cur_var, var_list: typ;
     cur_tag, fields: sym;
     tag_val: int_type;

 begin
  start_code := if_cursor; (* record the start of the expanded code *)

  if rhs^.desc.kind = arrays then begin
    index_type := rhs^.desc.base^.index_type;
    comp_type := rhs^.desc.base^.element_type;
    for i := 1 to upperbound (rhs^.operand) do begin
      new (component, array_ref, array_ref);
      initexpr (component, comp_type);
      component^.base_array := lhs;
      component^.index_val := scalar_const (index_type, index_type^.minval + i -1);
      emit (component);
      assign (component, rhs^.operand [i]);
    end;
  end

  else (* rhs^.desc.kind = records *) begin
    cur_var := rhs^.desc.base; (* the record type *)
    if cur_var^.variant_tag = nil
      then cur_tag := nil (* no variants *)
      else cur_tag := cur_var^.variant_tag^.tag_field; (* the variant tag field *)
    fields := cur_var^.field_list;
    for i := 1 to upperbound (rhs^.operand) do begin

    (* If the field list is exhausted, then any remaining operands must be
       selectors for undiscriminated unions, so they can be discarded. *)

    exit if fields = nil;

      (* If the field belongs to the current variant, then the current operand
         should be assigned to it. *)

      if fields^.fld_variant = cur_var then begin
        new (component, field_ref, field_ref);
        initexpr (component, fields^.type_desc);
        component^.base_rec := lhs;
        component^.field_sym := fields;
        emit (component);
	with fields^.type_desc^ do begin
	  if (kind in [arrays, strings]) and flexible then begin
	    new (upb, upb_op, upb_op, 1);
	    if kind = arrays
	      then initexpr (upb, index_type)
	      else initexpr (upb, type_non_neg);
	    upb^.operand[1] := component;
	    emit (upb);
	    if dest <> nil then
	      assign (upb, upper_bound (last_field (dest)))
	    else begin
	      assert (rhs^.opcode = agg_val);
	      assign (upb, upper_bound (rhs^.operand[i]));
	    end;
	  end;
	end;
        assign (component, rhs^.operand[i]);
      end;

      (* If the field is the variant tag, then the current operand must be used
         to select a discriminated union variant.  If the field doesn't belong
         to the current variant, then the current operand must be used to select
         an undiscriminated union variant.  The code is the same in either case. *)

      if (fields = cur_tag) or (fields^.fld_variant <> cur_var) then begin
        tag_val := rhs^.operand[i]^.cst_val.ival; (* must be a scalar constant *)
        var_list := cur_var^.variant_tag^.first_variant; (* the variant list *)
        while (var_list <> nil) andif not ( var_list^.others_var orif
              ( (var_list^.minlab <= tag_val) and (tag_val <= var_list^.maxlab) ) ) do
          var_list := var_list^.next_variant;
        if var_list = nil then (* no variant selected - no more fields *)
          fields := nil
        else begin (* select the new variant *)
          fields := var_list^.field_list;
          if fields <> nil then
            cur_var := fields^.fld_variant; (* if fields = nil, it doesn't matter *)
          if cur_var^.variant_tag = nil
            then cur_tag := nil (* no sub-variants *)
            else cur_tag := cur_var^.variant_tag^.tag_field; (* the sub-variant tag field *)
        end;
      end
      else
        fields := fields^.next;
    end (* for i *);
  end;

  expand (start_code^.next, if_cursor^.next); (* expand the assignments into low_level code *)
 end;
$PAGE agg_temp
(* AGG TEMP returns a dynamic temporary of a size appropriate to hold a
   specified aggregate expression.  When the expression is a record with
   a trailing flex string field, then it must be the right-hand side of an
   assignment, and we want to allocate a temp which will be compatible with
   the left-hand side. *)

function agg_temp (source_op, dest_op: expr): expr;

 var op: expr;
     saved_cursor: tuple;
     asize: expr;
     n: integer;

 begin
  saved_cursor := if_cursor;
  with source_op^ do begin
    if desc.kind = arrays then begin
      op := source_op;
      t_set (op^.prev);
      asize := upper_bound (op);
    end
    else if (opcode = agg_val) andif (desc.base^.flexible) andif 
	    (operand[upperbound (operand)]^.desc.kind = strings) then begin
      assert (dest_op <> nil);
      op := dest_op;
      t_set (op^.prev);
      asize := upper_bound (last_field (op));
    end
    else begin
      op := source_op;
      t_set (op^.prev);
      asize := upper_bound (op^.operand[upperbound (op^.operand)]);
    end;
  end;
  asize := compute_size (op^.desc.base, asize);
  agg_temp := alloc_dynamic_temp (asize); (* allocate a temp of the required size *)
  if is_iconstp (asize, n) andif (n <= 2) then begin
    agg_temp^.opcode := mem_ref;
    agg_temp^.desc := op^.desc;
  end
  else
    agg_temp := op_2 (desc_ref, op^.desc, agg_temp, asize);
  t_set (saved_cursor);
 end;
$PAGE agg_reference
(* AGG REFERENCE returns an addressable reference for an aggregate.  The only
   possible aggregate value representations are references, constructors and
   function calls.  References are simply returned.  Constructors and function
   calls are assigned to temporaries, and temporary references are returned.
   It is important to note that (due to the structure of semantic analysis and
   optimization) multiple references to an aggregate constructor or function
   value are possible only if it is the record in a with statement.  The record
   in a with statement is always processed by this routine; therefore, if a
   temporary is needed for an aggregate, it can be created the first time the
   aggregate is processed, and simply referred to thereafter.

   If the aggregate reference occurs on the right-hand side of an assignment
   statement, the DestAgg parameter will be the left-hand side of the assign-
   ment; otherwise, DestAgg will be Nil.  *)

function agg_reference (* agg, dest_agg: expr): expr *);

 var saved_cursor: tuple;

 begin
  if (agg^.result <> nil) andif (agg^.result^.opcode <> agg^.opcode) then
    agg_reference := agg^.result (* tuple already has a temp *)

  else if agg^.opcode = agg_val then begin
    agg_reference := agg_temp (agg, dest_agg);
    saved_cursor := if_cursor;
    t_set (agg);
    assign_agg_val (agg_reference, agg, dest_agg);
    t_set (saved_cursor);
    agg^.result := agg_reference;
  end

  else if agg^.opcode = func_call_op then begin
    agg_reference := agg_temp (agg, dest_agg);
    saved_cursor := if_cursor;
    t_set (agg);
    assign (agg_reference, agg^.result);
    t_set (saved_cursor);
    with agg_reference^ do begin
      if opcode = desc_ref then
        agg^.result := op_2 (desc_ref, desc, operand[1], operand[2])
      else begin
        new (agg^.result, mem_ref, mem_ref);
        agg^.result^.item := item;
        init_node (agg^.result, desc);
      end;
    end;
    agg_reference := agg^.result;
  end

  else
    agg_reference := agg;
 end;
$PAGE expand_io_string
(* EXPAND IO STRING takes an input or output string operator, and replaces it
   with an io varying string operator or an io fixed string operator, depending
   on the string operand.  The resulting operator refers to a two-word temporary,
   which has the string address or byte pointer stored in its first word, and
   the string maximum or current length stored in its second word. *)

procedure expand_io_string (op: expr);

 var str_block, sb_addr, sb_len, str, str_addr, str_len, temp: expr;
     temp_desc: expr_type_desc;

 begin
  str := op^.operand[1];

  str_block := alloc_temp (2); (* allocate the string i/o block *)

  new (sb_addr, mem_ref, mem_ref); (* the first word is the addr/bptr word *)
  sb_addr^.item := str_block^.item;

  new (sb_len, mem_ref, mem_ref); (* the second word is the length word *)
  sb_len^.item := str_block^.item;
  sb_len^.item.offset := sb_len^.item.offset + 1;
  init_node (sb_len, len_desc);

  if op^.opcode = in_str_op then begin

    (*  If the operation is GETSTRING and the source string is not a
	temporary, then allocate a temporary, copy the source string
	to it, and use the temporary as the source for the GETSTRING
	operation.  This will avoid problems with operations like
	"getstring (str, x, str:5, y)".  *)

    str := transform (str);
    if not is_temporary (str) then begin
      str_len := str_length (str);
      temp_desc := str^.desc;
      temp_desc.str_kind := nonvarying;
      if temp_desc.str_flex
	then temp := str_temp (temp_desc, str_len)
	else temp := str_temp (temp_desc, int_const (str^.desc.str_length));
      temp^.operand[2] := str_len;
      assign (temp, str);
      str := temp;
    end;

    (*  The RD.SS i/o call takes a block containing a byte pointer to the
	character preceding the source string, and the string length.  *)

    init_node (sb_addr, byte_ptr_desc);
    str := str_reference (str);
    assign (sb_addr, str_bptr (str));
    assign (sb_len, str^.operand[2]); (* the string length *)
    op^.result := op_1 (io_fix_str_op, file_desc, str_block);
  end

  else begin

    (*  If the operation is PUTSTRING, then we must allocate a varying
	string temporary with the same upper bound as the destination
	string.  We will perform the PUTSTRING operation into this
	temporary, and at its conclusion, we will copy the temporary
	to the target string.  *)

    temp_desc := str^.desc;
    temp_desc.str_kind := varying;
    if str^.opcode = substr_ref
      then str_len := transform (str^.substr_length)
      else str_len := upper_bound (str);
    temp := str_temp (temp_desc, str_len);
    init_node (sb_addr, pointer_desc);
    new (str_addr, addr_ref, addr_ref); (* get address of length word *)
    init_node (str_addr, pointer_desc);
    str_addr^.item := temp^.operand[2]^.item;
    assign (sb_addr, str_addr);
    assign (sb_len, str_len);
    new (str_len, mem_ref, mem_ref);
    init_node (str_len, len_desc);
    str_len^.item := temp^.operand[2]^.item;
    assign (str_len, int_const (0)); (* set the target string to '' *)
    op^.result := op_1 (io_var_str_op, file_desc, str_block);
    op^.result^.result := temp; (* save the temp string for later *)
  end;

 end;
$PAGE expand_operator
(* EXPAND OPERATOR transforms nary operators.  First, it replaces all operand
   references by the expanded form of the operand.  Second, it performs
   operator specific expansions and optimizations.  The result field of the
   input operator node ("op") is set to the transformed result (which may be
   the original operator.) *)

procedure expand_operator (op: expr);

 var i: 0..4095;
     temp: expr;
     sdesc: expr_type_desc;
     ival: int_type;

 begin
  with op^ do begin
    result := op;                       (* result is normally unchanged *)
    case opcode of

      (* It is generally an improvement to move constant operands to the right
         in addition and multiplication. *)

      iadd_op, imul_op:
        begin
          if operand[1]^.opcode = cst_ref then begin    (* move the cst *)
            temp := operand[2];
            operand[2] := operand[1];
            operand[1] := temp;
          end;
          operand [1] := transform (operand[1]);
          operand [2] := transform (operand[2]);
        end;

      (* Exponentiation by a positive constant exponent can be expanded inline,
         by converting it to a series of multiplies. *)

      expii_op:
        begin
          operand[1] := transform (operand[1]);
          operand[2] := transform (operand[2]);
          if is_iconstp (operand[2], ival) andif (ival >= 0)
            then result := expand_exp (imul_op, desc, operand[1], ival);
        end;

      expri_op:
        begin
          operand[1] := transform (operand[1]);
          operand[2] := transform (operand[2]);
          if is_iconstp (operand[2], ival) andif (ival >= 0)
            then result := expand_exp (rmul_op, desc, operand[1], ival);
        end;

      (* Attribute enquiry functions: dispatch to the routines which create
         trees computing the values. *)

      lwb_op:
        result := lower_bound (operand[1]);

      upb_op:
        result := upper_bound (operand[1]);

      dim_op:
        result := op_2 ( iadd_op, index_desc,
                        op_2 ( isub_op, index_desc,
                              upper_bound (operand[1]),
                              lower_bound (operand[1]) ),
                        int_const (1) );

      addr_op:
        result := addr (operand[1]);

      (* Float_op's are introduced in the first pass to convert reals of different
         declared precision;  however, after their internal precision has been
         set, it may be determined that the values have the same precision.  For
         example:  real 3 := real 4 becomes real 7 := float (real 7, 7).  Therefore
         it is useful to remove float_op's which convert a value to its own precision. *)

      float_op:
        begin
          operand [1] := transform (operand [1]);
          if (operand[1]^.desc.kind = reals) andif (operand[1]^.desc.precision = desc.precision)
            then result := operand[1];  (* remove the float op *)
        end;

      (* String functions. *)

      length_op:
        result := str_length (operand[1]);

      index_op:
        begin
          operand[1] := str_reference (transform (operand[1]));
          if operand[2]^.desc.kind in scalar_types (* any scalar may be a char *)
            then operand[2] := transform (operand[2])
            else operand[2] := str_reference (transform (operand[2]));
          if upperbound (operand) = 3
            then operand[3] := transform (operand[3]);
        end;

      search_op, verify_op:
        begin
          operand[1] := sv_str_operand (transform (operand[1]));
          operand[2] := sv_set_operand (transform (operand[2]));
          if upperbound (operand) = 3
            then operand[3] := transform (operand[3]);
        end;

      (* A string comparsion is translated as follows:

           sxx_op (s1, s2)  ->  sxx_op (str_comp_op (s1, s2))

         where the str_comp_op returns the difference of the first two
         distinguishing characters or zero if the strings are identical.
         Thus, the result is negative if s1 < s2; zero, if s1 = s2; and
         positive if s1 > s2.  This form mimics the form of the runtime
         call and, more important, permits optimization of multiple
         compares against the same strings: if s1 < s2 else if s1 >= s2. *)

      sle_op, slt_op, sgt_op, sge_op, seq_op, sne_op:
	result := op_1 (opcode, desc, op_2 (str_comp_op, index_desc,
				str_reference (transform (operand [1])),
				str_reference (transform (operand [2])) ));

      (* A function which returns a string value must appear in an assignment
         operator which designates where the result of the function is to be
         placed.  The operators which accept strings must guarentee that such
         an assignment is made.  Only those below require an explict check; others
         such as index_op, above, force all string expressions, not just functions,
         to be copied. *)

      cat_op:
        begin
          for i := 1 to upperbound (operand) do
            operand[i] := str_value (operand[i]);       (* str_value copies string functions *)
        end;

      upc_op, lwc_op:
        begin
          if desc.kind = chars
            then operand[1] := transform (operand[1])
            else operand[1] := str_value (operand[1]);
        end;

      (* String conversions are subject to optimization by (1) removal of the
         conversion operator when not required, (2) alteration of the actual
         desc_ref, if the operand "looks" like the required string, and (3)
         a change in the target type of the conversion to one which "looks" like
         the required type, but requires less storage.  These transformation are
         performed with insidious knowledge of the way in which they will be used
         in assignments and actual value parameter passing. *)

      strcvt_op:
        begin
          operand[1] := str_value (operand[1]); (* if arg is func, must copy *)
          if ((desc.str_kind = nonvarying) and (desc.str_flex))
            then begin  (* no conversion required, even if packed *)
              if operand[1]^.desc.kind <> strings       (* but char must be promoted *)
                then result := str_reference (operand[1])
                else result := operand[1];
            end
          else if ((desc.str_kind = nonvarying) and (not desc.str_flex)) andif
                  fixed_str_aligned_ref (operand[1]) andif
                  (operand[1]^.desc.str_length >= desc.str_length)
            then result :=                      (* shorten reference to longer nonvarying string *)
                   op_2 (desc_ref, desc, operand[1]^.operand[1], int_const (desc.str_length))
          else if ((desc.str_kind = varying) and (not desc.str_flex)) andif
                  varying_str_ref (operand[1]) andif
                  (operand[1]^.desc.str_length <= desc.str_length)
            then result := operand[1]           (* operand's maxlength is shorter than required,
                                                   thus, also valid VS of a longer maxlength  *)
          else if (desc.str_kind = varying) and (desc.str_flex) andif
                  varying_str_ref (operand [1])
            then result := operand[1]           (* any varying string is okay flex varying string *)
          else if desc.str_kind = varying then begin
            if operand[1]^.desc.kind in scalar_types    (* only need minimum length target *)
              then begin
                desc.str_length := min (desc.str_length, 1);
                desc.str_flex := false;         (* has finite length at compile time *)
              end
            else if not operand[1]^.desc.str_flex
              then begin
                desc.str_length := min (desc.str_length, operand[1]^.desc.str_length);
                desc.str_flex := false;
              end;
          end;
        end;

      (* Set functions *)

      in_op:
        begin
          operand[1] := transform (operand[1]);
          operand[2] := transform (operand[2]);

          if operand[2]^.opcode = gen_set_op
            then begin
              if upperbound (operand[2]^.operand) = 0                 (* x in [] !!!! *)
                then result := scalar_const (type_bool, ord (false))
              else if upperbound (operand[2]^.operand) = 1            (* x in [a] !!!! *)
                then result := op_2 (ieq_op, desc, operand[1], operand[2]^.operand[1])
              else operand[2] := implicit_set_op (operand[2]^.desc, operand[2]);
            end
          else if operand[2]^.desc.set_length <= 72
            then operand[2] := expand_short_set_op (operand[2]^.desc, operand[2])
          else operand[2] := set_reference (operand[2]);
        end;

      (* For the subset relations, we need only consider elements in the range of
         the subset operand, since it the relation necessarily holds for all
         elements outside of the range.  We use this to get the minimum size
         for the operands.  If this range is small enough, the comparision can
         be done in the registers;  otherwise, it must be done as a storage to
         storage comparision. *)

      setle_op, setge_op:
        begin
          if opcode = setle_op
            then begin                  (* get bounds of subset operand *)
              sdesc := operand[1]^.desc;
              temp := set_lowerbound (operand[1]);
            end
            else begin
              sdesc := operand[2]^.desc;
              temp := set_lowerbound (operand[2]);
            end;
          if sdesc.set_length <= 72
            then begin
              operand[1] := expand_short_set_op (sdesc, transform (operand[1]));
              operand[2] := padded_set_operand (sdesc, transform (operand[2]));
            end
            else begin
              operand[1] := long_set_operand (sdesc, temp, transform (operand[1]));
              operand[2] := long_set_operand (sdesc, temp, transform (operand[2]));
            end;
        end;

      (* For the set equality tests, we must consider the union of the ranges of
         the two operands.  Again, the operation may be done in or out of the
         registers. *)

      seteq_op, setne_op:
        begin
          operand[1] := transform (operand[1]);
          operand[2] := transform (operand[2]);

          sdesc := set_desc;                    (* compute desc, giving size *)
          sdesc.base := operand[1]^.desc.base;
          sdesc.set_lwb := min (operand[1]^.desc.set_lwb, operand[2]^.desc.set_lwb);
          sdesc.set_cst_lwb := operand[1]^.desc.set_cst_lwb and operand[2]^.desc.set_cst_lwb;
          sdesc.set_length :=
            max ( operand[1]^.desc.set_lwb + operand[1]^.desc.set_length - 1,
                  operand[2]^.desc.set_lwb + operand[2]^.desc.set_length - 1 )
                    - sdesc.set_lwb + 1;
          sdesc.set_cst_len := operand[1]^.desc.set_cst_len and operand[2]^.desc.set_cst_len;

          if sdesc.set_length <= 72
            then begin
              operand[1] := expand_short_set_op (sdesc, operand[1]);
              operand[2] := expand_short_set_op (sdesc, operand[2]);
            end
            else begin
              temp := op_2 (imin_op, len_desc,
                            set_lowerbound (operand[1]), set_lowerbound (operand[2]));
              operand[1] := long_set_operand (sdesc, temp, operand[1]);
              operand[2] := long_set_operand (sdesc, temp, operand[2]);
            end;
        end;

      setcvt_op:
        begin
          if desc.base^.kind = sets                     (* code assumes element type used *)
            then desc.base := desc.base^.set_element_type^.base_type;
          operand[1] := transform (operand[1]);
        end;

      (* The sclcvt operator performs no action, so it may be removed. *)

      sclcvt_op:
        result := transform (operand[1]);

      agg_val:
        (* do not update the operands now *);

      in_str_op,
      out_str_op:
        expand_io_string (op);

      (* The cursor function simply looks at the cursor word in the file
         control block. *)

      cursor_op:
        begin
          new (result, mem_ref, mem_ref);
          init_node (result, desc);
          result^.item := var_item;
          result^.item.index := transform (operand[1]); (* access the file block *)
          result^.item.offset := file_cursor_offset;
        end;

      (* The eof, eoln, and eopage functions simply look at bits in the flag
         word in the file control block. *)

      eof_op, eoln_op, eopage_op:
        begin
          new (result, mem_ref, mem_ref);
          init_node (result, desc);
          result^.item := packed_item;
          with result^.item do begin
            index := transform (operand[1]);
            size := 1;
            offset := bits_per_unit * file_flag_word_offset;
            case opcode of
              eof_op: offset := offset + file_eof_offset;
              eoln_op: offset := offset + file_eoln_offset;
              eopage_op: offset := offset + file_eopage_offset
            end;
          end;
        end;

      open_op, reset_op, rewrite_op, update_op:
        begin
          if operand[1] <> nil (* expand name reference *)
            then i := 1
            else i := 2;
          operand[i] := str_reference (transform (operand[i]));
          t_set (operand[i]^.prev);
          operand[i]^.operand[1] := str_bptr (operand[i]); (* must be a byte pointer *)
          t_set (prev); (* expand options set *)
          if operand[3] <> nil then
            operand[3] := expand_short_set_op (io_opt_set, transform (operand[3]));
        end;

      (* Most nary operators do not require special expansion, therefore in all
         other cases just update the operand pointers. *)

      others:
        for i := 1 to upperbound (operand) do operand[i] := transform (operand[i])

    end (* case opcode *) ;
  end (* with op^ *) ;
 end;
$PAGE prep_value_parameter
(* PREP VALUE PARAMETER processes a single value parameter and places into the
   argument list a value or pointer and any descriptor words required.  The
   parameters are the same as the local variables of "prepare_call".  Note that
   "parmidx" is advance whenever a descriptor is inserted, but not when the
   parameter (pointer) is inserted. *)


procedure prep_value_parameter
    (outcall: tuple; parm_type: typ; arg: tuple; var parmidx: parm_range);

 var parm_is_ref: boolean;              (* flags parm as reference tuple *)
     temp: expr;
     sarg: expr;
     saddr: expr;
     elemtype: typ;

  (* LOAD BYTE ADDRESS loads a byte pointer to a string in storage whose address
     is given in a descriptor ("sdesc"). *)

(* --- not used --- we're keeping it around as it might be handy sometime ---
  procedure load_byte_address (sdesc: expr);
   var s: expr;
   begin
    s := sdesc^.operand[1];                     (* from address in descriptor *)
    if not s^.item.pack then begin      (* must get a bit address *)
      new (s, addr_ref, addr_ref);      (* generate byte address ref *)
      init_node (s, byte_ptr_desc);
      with s^ do begin
        item := sdesc^.operand[1]^.item;
        item.offset := item.offset * bits_per_unit;
        item.pack := true;
        if sdesc^.desc.kind in scalar_types (* any scalar may be a char *)
          then item.size := bits_per_unit       (* treat aligned character as big byte *)
          else item.size := type_char^.base_size;
      end;
      add_cst_bit_index (s, -1);        (* string lower bound is 1 *)
    end;
    outcall^.arglist[parmidx] := s;     (* pass the byte pointer *)
   end;
*)


 begin
  case parm_type^.kind of

    bools, ints, chars, scalars, pointers, files, reals:
      outcall^.arglist[parmidx] := transform (arg);
                                        (* simply load the value *)

    procs, funcs:
      outcall^.arglist[parmidx] := transform (arg);

    arrays:
      begin
        if parm_type^.generic then begin
          outcall^.arglist[parmidx] := lower_bound (arg);
          parmidx := parmidx + 1;
        end;
        if parm_type^.flexible then begin
          outcall^.arglist[parmidx] := upper_bound (arg);
          parmidx := parmidx + 1;
        end;
        if p_b_address (parm_type)
          then outcall^.arglist[parmidx] := addr (arg)
          else outcall^.arglist[parmidx] := agg_reference (arg, nil);
      end;

    records:
      if p_b_address (parm_type)
        then outcall^.arglist[parmidx] := addr (arg)
        else outcall^.arglist[parmidx] := agg_reference (arg, nil);

    strings:
      begin
        sarg := transform (arg);                (* get transformed version *)

        (* If the transformed argument is not a desc_ref, then it is an expression
           and must be copied to a temporary, before it can be passed.  Note that
           if the argument is of an incorrect type, then a strcvt will have been
           introduced in the first pass to convert it.  However, there is a careful
           conspiracy between this routine and expand_operator which attempts to
           remove strcvt's in cases where the operand string can be made to look
           like the required string without a copy.  In particular, it knows that
           substr references (yielding a packed address) may be passed through
           conversion to a flexible nonvarying string, but not conversion to other
           kinds of strings which require the result to be unpacked.  *)

        temp := str_reference (sarg);           (* forces copy of any expression, required conversion *)

        with parm_type^ do begin
          if flexible then begin        (* handle descriptor words *)
            outcall^.arglist[parmidx] := temp^.operand[2];      (* length *)
            parmidx := parmidx + 1;
          end;
          if str_kind = varying then begin              (* pass address of length word *)
            new (saddr, addr_ref, addr_ref);            (* address in descriptor is addr of text *)
            init_node (saddr, pointer_desc);
            saddr^.item := temp^.operand[1]^.item;
            saddr^.item.offset := saddr^.item.offset - 1;
            if saddr^.item.class = constant_sc then
              saddr^.item.cstref^.str_varying_ref := true;
            outcall^.arglist[parmidx] := saddr;
          end
          else if flexible then (* pass byte pointer to string *)
            outcall^.arglist[parmidx] := str_bptr (temp)
          else (* pass address from descriptor *)
            outcall^.arglist[parmidx] := temp^.operand[1];
        end (* with parm_type^ *);
      end (* string case *) ;

    sets:
      begin
        (* There are two cases:  if the argument is to be passed by address, any
           expressions (including) conversions must be evaluated and assigned to
           temporaries, so that the address of the temporary may be used.  If the
           argument is to be passed by value, the set expression is referenced
           directly as the argument.  In the course of both transformations, note
           that the processing of setcvt_op's is fully aware of how and where the
           setcvt_op is actually needed. *)

        if p_b_address (parm_type)
          then begin                    (* pass by address *)
            sarg := set_reference (transform (arg));
            outcall^.arglist[parmidx] := sarg^.operand[1];
          end
          else                          (* pass by value *)
            outcall^.arglist[parmidx] := expand_short_set_op (arg^.desc, transform (arg));
      end;

    others:
      assert (false)

  end (* case *) ;
 end;
$PAGE prep_var_parameter
(* PREP VAR PARAMETER processes a single var parameter, and inserts into the
   argument a pointer to the parameter and any descriptor words required.  The
   parameters are the same as the local variables of "prepare_call".  Note that
   "parmidx" is advanced when a descriptor is inserted, but not when the address
   of the parameter is inserted. *)

procedure prep_var_parameter
    (outcall: tuple; parm_type: typ; arg: tuple; var parmidx: parm_range);

 begin
  case parm_type^.kind of
    arrays, strings:
      begin
        if parm_type^.generic then begin        (* strings are never generic *)
          outcall^.arglist[parmidx] := lower_bound (arg);
          parmidx := parmidx + 1;
        end;
        if parm_type^.flexible then begin
          outcall^.arglist[parmidx] := upper_bound (arg);
          parmidx := parmidx + 1;
        end;
      end
  end (* case *) ;
  outcall^.arglist[parmidx] := addr (arg);
 end;
$PAGE prepare_call
(* PREPARE CALL processes a subroutine call node, shaping the subr value and
   forming the true argument list (i.e. inserting descriptors, addresses, etc.). *)

function prepare_call (* incall: tuple): tuple *) ;

 var
  parmidx: parm_range;
  outcall: tuple;               (* the transformed output call node *)
  i: parm_range;

 begin
  (* Special descriptor parameters are past for flexible and generic parameters.
     This requires that the parameter list be expanded and the descriptors added
     as if they were value parameters.  Here we perform a prescan to see how
     many parameter slots are required. *)

  parmidx := 0;                         (* and count slots required *)
  with incall^.subr^.desc.base^ do begin
    for i := 1 to upperbound (params) do begin
      parmidx := parmidx
                    + 1                 (* one for the parameter itself *)
                    + ord (params[i].parm_type^.flexible)       (* one for an upper bound *)
                    + ord (params[i].parm_type^.generic);       (* one for a lower bound *)
    end;
  end;

  (* If there are in fact extra descriptor parameters, then we must allocate
     a new call node to hold the extra arguments. *)

  if parmidx <> upperbound (incall^.arglist) then begin
    new (outcall, call_op, call_op, parmidx);
    outcall^.opcode := incall^.opcode;
  end
  else outcall := incall;               (* parmidx = upperbound (arglist), use same node *)

  (* Iterate over all the parameters and process according to their kind.  In
     the calls below, "parmidx" is incremented by the called routine when
     descriptors are inserted. *)

  parmidx := 0;
  with incall^.subr^.desc.base^ do begin
    for i := 1 to upperbound (incall^.arglist) do begin
      parmidx := parmidx + 1;
      if params[i].parm_kind = vars
        then prep_var_parameter (outcall, params[i].parm_type, incall^.arglist[i], parmidx)
        else prep_value_parameter (outcall, params[i].parm_type, incall^.arglist[i], parmidx);
    end;
  end;

   (* Get the subroutine value to call. *)

   outcall^.subr := transform (incall^.subr);
   with outcall^.subr^ do begin
     if opcode = subr_var_op then begin       (* use desc_ref to tag call to subr constant *)
       if upperbound (operand) = 1
	 then outcall^.subr := op_1 (desc_ref, desc, operand[1])
	 else outcall^.subr := op_2 (desc_ref, desc, operand[1], operand[2]);
     end;
   end;
   prepare_call := outcall;             (* return possibly transformed result *)
   if incall <> outcall then begin              (* splice in transformed call node *)
     if incall^.opcode = func_call_op
       then init_node (outcall, incall^.desc)
       else emit (outcall);
   end;
 end;
$PAGE make_variable_descriptor
(* MAKE VARIABLE DESCRIPTOR forms a string descriptor (addr, len) for references
   to string variables (substr_ref and cst_ref are excluded). "Str" is the input
   reference node, and the descriptor generated is returned. *)

function make_variable_descriptor (str: expr): expr;

 var
   parm: boolean;               (* flags str as parameter *)
   saddr, slen: expr;           (* components of descriptor *)

 begin
  new (saddr, addr_ref, addr_ref);              (* get address of base of string/char variable *)
  access (str, saddr);
  init_node (saddr, pointer_desc);

  (* If the variable is a string, we must check if it is flexible, in
     which case there is a hidden descriptor word. *)

  parm := ((str^.opcode = ident_ref) andif (str^.id_sym^.dcl_class = parameter_sc));
  if str^.desc.str_flex and (not parm)          (* flexible non parms are offset *)
    then saddr^.item.offset := saddr^.item.offset + 1;

  (* Fixed length string.  If the variable is a flexible parameter, then the
     parameter pointer (i.e.  saddr^.item.index) is actually a byte pointer
     (offset by -1 characters already, so that no ILDB compensation is needed);
     otherwise, the address is the full word address of the string.  This may
     be a packed address if the string is a non-varying string of less than
     six characters (i.e., less than one word) in a packed record or array;
     in that case, change it to an unpacked, full-word address. *)

  if str^.desc.str_kind = nonvarying then begin
    slen := upper_bound (str);          (* knows how to handle flex cases *)
    if str^.desc.str_flex and (parm andif (str^.id_sym^.kind = values)) then begin
      saddr := saddr^.item.index;               (* use parm *byte* pointer *)
      saddr^.desc := byte_ptr_desc;
    end
    else if saddr^.item.pack and
	   ( (saddr^.desc.base <> byte_ptr_desc.base) orif
	     (saddr^.desc.kind <> byte_ptr_desc.kind) orif
	     (saddr^.desc.signed <> byte_ptr_desc.signed) orif
	     (saddr^.desc.int_prec <> byte_ptr_desc.int_prec) ) then begin
      saddr^.item.pack := false; (* change packed to unpacked full-word addr *)
      saddr^.item.size := bits_per_unit;
      saddr^.item.offset := saddr^.item.offset div bits_per_unit;
    end;
  end

  (* Varying length string.  The address we have points to the length word,
     and the string proper starts one word beyond.  Construct a reference to
     the length word, and adjust the address. *)

  else (* str^.desc.str_kind = varying *) begin
    new (slen, mem_ref, mem_ref);               (* generate reference to address word *)
    slen^.item := saddr^.item;
    init_node (slen, len_desc);
    saddr^.item.offset := saddr^.item.offset + 1;       (* address the text portion of the string *)
  end;

  make_variable_descriptor := op_2 (desc_ref, str^.desc, saddr, slen);
 end;
$PAGE make_substr_descriptor
(* MAKE SUBSTR DESCRIPTOR forms a descriptor for a substr reference.  The input 
   reference is "str", and the descriptor of the substr is returned.  It has
   a packed address.  Note that special care is taken to insure that the
   packed address is offset for use in ILDB loops (see below). *)

function make_substr_descriptor (str: expr): expr;

 var
  saddr, slen: expr;            (* components of the descriptor *)
  sbp: expr;
  saved_cursor: tuple;

 begin
  new (saddr, addr_ref, addr_ref);      (* get address of first char *)
  substr_access (str, saddr);
  init_node (saddr, byte_ptr_desc);     (* saddr = addr 0(bptr), where bptr = addr 0[idx](base) *)

  offset_byte_address (saddr);

  (* The length is given by the third operand of the substr reference.  Note
     that a two operand substr is converted to a three operand form during
     the first pass. *)

  slen := transform (str^.substr_length);
  make_substr_descriptor := op_2 (desc_ref, str^.desc, saddr, slen);
 end;
$PAGE simple_mem_access
(* SIMPLE MEM ACCESS processes the case of a reference to a data item in memory
   whose length is known, and is no more than two words. *)

function simple_mem_access (ref: expr): expr;
 begin
  new (simple_mem_access, mem_ref, mem_ref);
  access (ref, simple_mem_access);
  init_node (simple_mem_access, ref^.desc);

  (* Copy the optimizer-generated usage information from the original node
     into the copy. *)

  simple_mem_access^.ref_fre := ref^.ref_fre;
  simple_mem_access^.context := ref^.context;
  simple_mem_access^.killed_tuple := ref^.killed_tuple;
  simple_mem_access^.copy_tuple := ref^.copy_tuple;
 end;
$PAGE expand_reference
(* EXPAND REFERENCE expands data references of values of all type kinds. That is,
   it transforms reference style nodes (cst_ref, ident_ref, array_ref, etc.) into
   memory access nodes (mem_ref, desc_ref, etc.).  The type of node produced is
   dependent on the particular data type and form of reference.  The "result"
   field of the input reference ("ref") is set to point to the expanded tree. *)

procedure expand_reference (ref: expr);

 var
   node: expr;                          (* expanded reference *)
   saddr, slen: expr;
   size_parm: expr;
    a, b, c, d: integer;
    grotesque: boolean;
   setlwb, setlen: int_type;
   id: sym;
   fldsym: sym;

 begin
  with ref^ do begin
    case desc.kind of

      (* Scalar data are expanded into mem_ref's, except in the case of a cst_ref
         which is small enough to fit in a half word, in which case an immed_ref
         is generated. *)

      scalars, bools, ints, chars:
        begin
          if (opcode = cst_ref) and (desc.int_prec <= 18)
            then node := scalar_const (desc.base, cst_val.ival)
            else node := simple_mem_access (ref);
        end;

      reals:
        node := simple_mem_access (ref);

      pointers, files:
        begin
          if opcode = cst_ref
            then node := scalar_const (type_ptr, 377777b)
            else node := simple_mem_access (ref);
        end;

      (* String references are transformed into desc_ref (addr (ref), length (ref). *)

      strings:
        begin
          if opcode = cst_ref
            then begin                          (* generate descriptor directly *)
              new (saddr, addr_ref, addr_ref);
              const_access (cst_val, saddr);
              init_node (saddr, pointer_desc);
              slen := int_const (desc.str_length);
              node := op_2 (desc_ref, desc, saddr, slen);
            end
          else if opcode = substr_ref
            then node := make_substr_descriptor (ref)
            else node := make_variable_descriptor (ref);
        end;

      (* Set references are of one of two classes:  short sets whose maximum
         length is <= the preferred set size (72 for the PDP-10), and long sets
         whose length is greater than the preferred size.  Short sets are
         represented as memory references, while long sets are represented as
         desc_ref (addr (set), lwb (set), dim (set)). *)

      sets:
        begin

          (* LHS refs use the set type for the desc.base; all others except setcvt
             use the maximal subrange of the element type.  The check below insures
             that this code only sees the maximal subrange. *)

          if (desc.base <> nil) andif (desc.base^.kind = sets)
            then desc.base := desc.base^.set_element_type^.base_type;

          if desc.set_length <= 72 then         (* short set, use mem_ref *)
            node := simple_mem_access (ref)

          else begin                    (* represent long sets with desc_ref *)
            saddr := addr (ref);
            if opcode = cst_ref
              then begin
                setlwb := cst_val.valp^.set_origin;
                setlen := dimension (cst_val.valp^.set_val);
              end
              else begin
                setlwb := desc.set_lwb;
                setlen := desc.set_length;
              end;
            node := op_3 (desc_ref, desc,
                             saddr, scalar_const (desc.base, setlwb), int_const (setlen));
          end;
        end;

      (* Aggregate references (arrays and records) also fall into two classes.
         Aggregates whose size is fixed at compile time, and is no more than
         two words, are represented as memory references.  All other aggregates
         are referenced as desc_ref (addr (aggregate), size (aggregate)).
         Note that the address is of the actual data (not any descriptor words),
         and that the size may be a constant (for a normal aggregate) or an
         expression (for a flexible or generic aggregate). *)

      arrays, records:
        with desc.base^ do begin
          if (base_size <= 72) and not flexible then (* short aggregate, use mem_ref *)
            node := simple_mem_access (ref)
          else begin (* long or flexible aggregate, use desc_ref *)
            if not flexible then (* constant length *)
              size_parm := nil
            else begin (* we must compute the length *)
              if generic then (* the parm to SIZE is the dimension *)
                size_parm := op_2 ( iadd_op, index_desc,
                                   op_2 ( isub_op, index_desc,
                                         upper_bound (ref),
                                         lower_bound (ref) ),
                                   int_const (1) )
              else if kind = arrays then (* the parm to SIZE is the upper bound *)
                size_parm := upper_bound (ref)
              else if variant_tag <> nil then (* cannot take size of flex variant *)
                size_parm := nil
              else begin (* the parm to SIZE is the length word from the
                            the array which is the last field of the record *)
                fldsym := field_list;
                while fldsym^.next <> nil do
                  fldsym := fldsym^.next;
                new (size_parm, mem_ref, mem_ref);
                access (ref, size_parm);
                init_node (size_parm, index_desc);
                size_parm^.item.offset :=
                  size_parm^.item.offset + (fldsym^.fld_offset div bits_per_unit);
              end (* if aggregate is flexible record *);
            end (* if aggregate is flexible *);
            saddr := addr (ref);
            node := op_2 (desc_ref, desc, saddr, compute_size (desc.base, size_parm));
          end (* if not short aggregate *);
        end;

      (* Procedure and function values are expanded into memory references in the
         case of subroutine variables, and subr_var_op's in the case of subroutine
         constants. *)

      procs, funcs:
        begin
          if opcode = cst_ref then begin                (* subr_var (addr (subr), display (parent)) *)
            id := cst_val.blkp^.subr_sym;       (* get address of constant subr id *)
            new (node, addr_ref, addr_ref);
            node^.item := subr_item;
            node^.item.sym_name := id;
            init_node (node, pointer_desc);
            if id^.block^.level <= 1                    (* level 1 blocks have no display *)
              then node := op_1 (subr_var_op, desc, node)
              else node := op_2 (subr_var_op, desc, node,
                                  display (cur_block^.apparent_level - id^.block^.apparent_level));
          end
          else if (opcode = ident_ref) andif (id_sym^.kind = consts) then begin
            new (node, addr_ref, addr_ref);     (* external subr constant *)
            access (ref, node);
            init_node (node, pointer_desc);
            node := op_1 (subr_var_op, desc, node);
          end
          else                          (* mem_ref to subr value *)
            node := simple_mem_access (ref);
        end;

      others:
	begin
	  if (opcode = ident_ref) andif (id_sym^.kind = conditions)
	    then node := simple_mem_access (ref) (* access the condition cell *)
	    else node := ref; (* no transformation applied yet *)
	end

    end (* case type kind *) ;

    result := node;                             (* record the transformed result *)
  end (* with *) ;
 end;
$PAGE assignment
(* ASSIGNMENT shapes assignments of values of all type kinds.  If the original
   assignment op must be removed from the I/F, the flag must_dechain will be
   set. *)

procedure assignment (op: tuple; var must_dechain: boolean);

 var
   temp, src, dest, len, node: expr;
   rhs_func: boolean;

 begin
  must_dechain := false;
  with op^ do begin
    case lhs^.desc.kind of

      bools, ints, scalars, pointers, reals, procs, funcs, files:
        begin
          lhs := transform (lhs);
          rhs := transform (rhs);
        end;

      arrays, records:
        begin
          lhs := agg_reference (lhs, nil);
          if (rhs^.opcode = agg_val) and (not overlaps) then begin
            assign_agg_val (lhs, rhs, lhs); (* expand assignment directly *)
            must_dechain := true;
          end
          else if (rhs^.opcode = func_call_op) and (not overlaps) then
	    rhs := transform (rhs)
          else begin
            rhs := agg_reference (rhs, lhs); (* create temporary if needed *)
            if (rhs^.opcode = desc_ref) and (lhs^.opcode = desc_ref) then begin

              (*  There is no need to compute the length for an array or record
                   move more than once.  If one side is flexible or generic, and
                   the other side isn't, we want to use the length which can be
                   computed more completely at compile time.  *)

              if (lhs^.desc.base^.flexible and not rhs^.desc.base^.flexible) or
                 (lhs^.desc.base^.generic and not rhs^.desc.base^.generic)
                then len := rhs^.operand[2]
                else len := lhs^.operand[2];

              (*  Copy the lhs and rhs descriptors up before the assignment.  *)

              lhs := op_2 (desc_ref, lhs^.desc, lhs^.operand[1], len);
              rhs := op_2 (desc_ref, rhs^.desc, rhs^.operand[1], len);
            end

            else if (rhs^.opcode = desc_ref) and (lhs^.opcode = mem_ref) then begin
              new (node, mem_ref, mem_ref);
              node^.item := rhs^.operand[1]^.item;
              init_node (node, lhs^.desc);
              rhs := node;
            end
            else if (rhs^.opcode = mem_ref) and (lhs^.opcode = desc_ref) then begin
              new (node, mem_ref, mem_ref);
              node^.item := lhs^.operand[1]^.item;
              init_node (node, rhs^.desc);
              lhs := node;
            end;
          end;
        end;

      chars:
        begin
          lhs := transform (lhs);
          rhs := transform (rhs);
        end;

      sets:
        begin
          (* If the set is less than the preferred set size, the operation can
             be done in the registers.  Otherwise, it must be done by storage to
             storage moves.  In the latter case, we must avoid smashing the lhs
             since its value may be used on the rhs. *)

          src := transform (rhs);
          dest := transform (lhs);
          if dest^.opcode = mem_ref             (* short set, len <= 72 *)
            then begin                          (* can assign values in regs *)
              rhs := expand_short_set_op (dest^.desc, src);
              lhs := dest;
            end

          else if (not overlaps) or lrecursive or rrecursive or is_temporary (src)
            then begin                          (* can move in place *)
              expand_set_op (dest, src);
              must_dechain := true;             (* delete original assignment *)
            end

          else begin    (* must introduce temp *)
            temp := set_temp (dest^.desc, dest^.operand[2], dest^.operand[3]);
            expand_set_op (temp, src);
            rhs := temp;
            lhs := dest;
          end;
        end;

      strings:
        begin

          (* Fetch the source operand, and strip any conversion operators.  Conversion
             can be performed in the process of a string move, unless the source
             is a func_call_op which can only be assigned to something of the same
             size as the result.  However, such a form cannot reach here for if
             there is a conversion involved the input IF will contain a conversion
             operator: "lhs := strcvt (func_call)".  This will have been transformed
             into "temp := func_call; lhs := strcvt (temp)" before this point
             is reached. *)

          src := transform (rhs);               (* maintain rhs, lhs as original nodes *)
          while src^.opcode = strcvt_op do src := src^.operand[1];
          dest := transform (lhs);

          (* If the LHS and RHS can overlap in such a way that a left to right
             move cannot work, then we must first assign the RHS to a temporary.
             If the lhs has a constant length, then the temporary we create is
             of that size, otherwise we use the possibly flexible length of the
             source value. *)

          if overlaps and (not lrecursive) and (not is_temporary (src)) then begin
            if src^.desc.kind = chars then
              temp := str_char_temporary (src)
            else if lhs^.desc.str_flex and src^.desc.str_flex then begin
              len := str_length (src); (* source and destination flexible *)
              temp := str_temp (src^.desc, len);
              expand_str_move (temp, src, len);
            end
            else (* at least one side non-flexible *) begin
              if lhs^.desc.str_flex
                then temp := str_temp (src^.desc, int_const (src^.desc.str_length))
                else temp := str_temp (lhs^.desc, int_const (lhs^.desc.str_length));
              expand_str_move (temp, src, nil);
            end;
            src := temp;
          end;

          rhs_func := (src^.opcode = func_call_op) andif
                      (src^.desc.kind = strings) andif
                      (src^.desc.str_kind = varying);

          (* If the lhs is a varying string, then the target length must be
             changed to the upperbound of the string, and the actual length
             of the source string assigned to the length word. *)

          if lhs^.desc.str_kind = varying then begin
            temp := vs_move_length (dest, src, upper_bound (lhs));      (* get no of chars to move *)
            len := dest^.operand[2];            (* the length part *)
            dest := op_2 (desc_ref, dest^.desc, dest^.operand[1], temp); (* target uses upper bound as length *)
            if not rhs_func then begin (* function calls set string length *)
             t_set (op);                                (* insert assignment to length, after string move *)
             assign (len, temp);
            end;
          end;

          rhs := src;   (* reference transformed operands in assign_op *)
          lhs := dest;

          (* If the target is a varying string and the source is a zero length
             string, then the assignment of the text's of the string is extraneous --
             only setting the length word to zero is required. *)

          if ((lhs^.desc.kind = strings) andif (lhs^.desc.str_kind = varying))
            and ((rhs^.desc.kind = strings) andif (rhs^.desc.str_length = 0))
              then must_dechain := true;        (* remove it and reclaim will get it's operands *)

          (* If the source is a varying string function, then the length word
             address rather than the text address of the destination must be
             passed to the function. *)

          if rhs_func then
            lhs^.operand[1] := length_word_address (lhs^.operand[1]);
        end

    end (* case target kind *) ;
  end (* with *) ;
 end;
$PAGE expand
(* EXPAND processes the entire IF in forwards order and performs node specific
   transformations.  In particular, it converts references of ident_ref, etc.
   form into mem_ref, etc. form;  it also transforms calls, strings, sets, and
   aggregate operations into their final imperative form: that is, implied copies
   are made explicit by the introduction of assignments; and much, much more.
   Expand can also be called recursively by other routines (in particular,
   assign_agg_val) to reprocess an already-processed portion of IF. *)

procedure expand (* first_op, last_op: tuple *);

 var
   op, orig_op: tuple;
   i: 0..4095;
   must_dechain: boolean;
   temp, str, saddr: expr;

 begin
  op := first_op;
  temp_used := false;   (* no dynamic temps in existence yet *)
  while op <> last_op do begin
    t_set (op^.prev);                   (* insert expansion of operator before the node *)
    case op^.opcode of

      first_data_ref..last_data_ref:
        expand_reference (op);

      first_nary_op..last_nary_op:
        expand_operator (op);

      start_with,
      end_with:
        op^.with_rec := nil;

      assign_op:        (* omnipresent, omnipotent *)
        begin
          assignment (op, must_dechain);
          if must_dechain then begin (* some assignment ops must be removed *)
            op := op^.next;
            dechain (op^.prev);
            op := op^.prev;
          end;
        end;

      eval_op:                          (* just evaluate a form *)
        op^.rhs := transform (op^.rhs);

      read_op,
      write_op:
        with op^ do begin
          rw_file := transform (rw_file);
          rw_width := transform (rw_width);
          rw_precision := transform (rw_precision);
          case rw_mode of
            binaryrw:
              rw_item := addr (rw_item);
            leftrw, rightrw:
              begin
                temp := str_reference (transform (rw_item));
                if (opcode = read_op) andif (rw_item^.desc.kind = strings) andif
                   (rw_item^.desc.str_kind = varying) then begin
                  (* A read into a varying string requires <addr (length word),
                     upper bound>, unlike other string i/o calls, which require
                     <addr (string), length>. *)
                  t_set (temp^.prev);
                  new (saddr, addr_ref, addr_ref);
                  init_node (saddr, pointer_desc);
                  saddr^.item := temp^.operand[2]^.item; (* the length word *)
                  temp^.operand[1] := saddr;
                  temp^.operand[2] := upper_bound (rw_item);
                end;
                rw_item := temp;
              end;
            others:
              begin
                if rw_item^.desc.kind in [arrays, records]
                  then rw_item := agg_reference (rw_item, nil)
                  else rw_item := transform (rw_item);
              end
          end (* case rw_mode *);
        end;

      (* A start io operator is not relevant to the PDP-10 code generator. *)

      start_io_op:
        begin
          op := op^.prev;
          dechain (op^.next);
        end;

      (* An end io operator is not relevant to the PDP-10 code generator.
	 However, if the io was a putstring, then it is necessary to copy
	 the destination temporary to the true destination string. *)

      end_io_op:
        begin
	  if op^.file_arg^.opcode = out_str_op then begin
	    t_set (op^.prev);
	    temp := transform (transform (op^.file_arg)); (* the temp string *)
	    expand_reference (op^.file_arg^.operand[1]);
	    str := transform (op^.file_arg^.operand[1]); (* the destination string *)
	    if str^.desc.str_kind = nonvarying then
	      assign (str, temp)
	    else begin
	      str^.operand[2] := temp^.operand[2];
	      assign (str, temp);
	      saddr := length_word_address (str^.operand[1]);
	      saddr^.opcode := mem_ref;
	      assign (saddr, temp^.operand[2]);
	    end;
	  end;
	  op := op^.prev;
	  dechain (op^.next);
        end;

      first_chk_op..last_chk_op:
        for i := 1 to upperbound (op^.operand) do op^.operand[i] := transform (op^.operand[i]);

      (* The call operator may be replaced with another operator with more
         arguments (representing descriptors).  If so, the new operator has
         been inserted, but the old one still needs to be deleted. *)

      call_op:
        begin
          orig_op := op;
          op := prepare_call (op);
          if op <> orig_op then begin
            op := orig_op^.next; (* remember the next node to be processed *)
            dechain (orig_op);
            op := op^.prev; (* now op^.next is the next node *)
          end;
        end;

      func_call_op:
        op^.result := prepare_call (op);

      goto_op:  (* fill in display of label to goto *)
        op^.target_frame :=
          display (cur_block^.apparent_level - op^.target_lab^.block^.apparent_level);

      (* If a dynamic temporary was allocated during the last statement, then
         we must emit a "reset_stk_op" to eliminate it before beginning the
         next statement.  If the previous statement is terminated by a jump
         operator, then the reset operator must be inserted before the jump
         to insure that stack is reset before control is transfer elsewhere. *)

      start_stmt:
        begin
          cur_source := op^.stmt_source;        (* keep track of where we are when debugging *)
          if temp_used then begin               (* must emit a reset stack *)
            repeat
              op := op^.prev;                   (* scan backwards for last nonjump, nonlabel *)
            until not ( ((jump_op <= op^.opcode) and (op^.opcode <= case_jump_op)) or
                        (op^.opcode = label_node) );
            t_set (op);                         (* place reset after this node *)
            new (op, reset_stk_op);
            emit (op);
            temp_used := false;
          end;
        end;

      others:
        transform_operands (op)

    end (* case opcode *) ;
    op := op^.next;
  end (* while *) ;
 end;
$PAGE rearrange_retjumps

(* The code generator will try to evaluate all expressions in their own basic
   blocks.  This will cause major problems with a sequence like

        X)      FCALL   something
        X+1)    RETJUMP somewhere
        X+2)    JUMP    X+3
        X+3)    ASSIGN  something := X

   when the function call is one of those whose values cannot be preserved
   between basic blocks.  Therefore, REARRANGE RETJUMPS will scan the
   entire I/F.  Whenever it finds an assignment statement whose rhs is
   a function call op which is followed by a retjump and whose value
   cannot be preserved between basic blocks, it will move the entire
   RETJUMP/JUMP series following the function call so that it now follows
   the assignment.  The function call will then be used in the same basic
   block that it is evaluated in, and there will be no problems. *)

procedure rearrange_retjumps;

var t, r, tn, rn, cur_label: tuple;

begin
  t := t_chain;
  while t <> nil do begin
    if t^.opcode = label_node then
      cur_label := t
    else if (t^.opcode = assign_op) or (t^.opcode = eval_op) then begin
      r := t^.rhs;
      with r^ do begin
        if (opcode = func_call_op) andif
           (next^.opcode = retjump_op) andif
           ( (desc.kind in [arrays, records, strings]) or
             ( (desc.kind = sets) andif (desc.set_length > 72) ) ) then begin

          (* We must move the basic block boundary from just after the function
             call tuple (R) to just after the assignment tuple (T). *)

          tn := r^.next;
          while tn^.opcode <> label_node do
            tn := tn^.next;
	  assert (tn = cur_label);
          rn := tn^.next;
          t^.next^.prev := tn;
          tn^.next^.prev := r;
          r^.next^.prev := t;
          tn^.next := t^.next;
          t^.next := r^.next;
          r^.next := rn;
        end;
      end;
    end;
    t := t^.next;
  end;
end (* rearrange_retjumps *);
$PAGE simplify
(* SIMPLIFY attempts to optimize integer operations in the IF.  It does this by
   performing a forward scan so that leaves are encountered and processed before
   the nodes which use them.  The result field is used to point to the transformed
   version of a node, and as each node which may have operands is encountered, that
   node is patched to point to the transformed operands.

   The optimizations which are performed here try to accomplish five things:
   (1) to propagate upward constant portions of sums;  for example, (j+2)-(k+3)
   becomes (j-k)-1.  This reduces the number of operations and permits constant
   offsets to be subsumed into an operand address.  (2) to fold constant valued
   expressions produced by the shaping code and thus not folded by the optimizer.
   (3) to convert as many constant references to positive values;  for example,
   the sum (-3+i) becomes (i-3).  This generally permits the constant to be an
   immediate operand, thereby saving an extra word for a long negative constant.
   (4) to propagate negations upward;  for example, (-i-2) becomes - (i+2).  A
   negation can most often be subsumed into the referencing operation (by changing
   an add to a subtract, using a move negative in an assignment, etc.) and also
   aids in third case.  (4) to factor constant multiplications;  for example,
   (j*4)+(k*2) becomes ((j*2)+k)+2).  Such expressions occur in multidimensional
   subscripting;  the transformation helps to expose additional common sube-
   expressions.

   We specifically do not try to simplify expressions which are assigned to
   variables.  This can often produce extra code as in the case of:
   "a := b + 1; c := d + 1; e := a + c" which would produce "e := b + d + 2".
   Also, when optimization is in effect, it can introduce errors in the
   program because certain variables (e.g. b, d in the example above) have
   lifetimes longer than the optimizer expects.   To handle this case, a node
   is marked as a "copy_tuple" when it is used as the rhs of an assignment.
   Thereafter, it will not be further simplified.  This is enforced by the
   helper routines splitsum and splitprod, above. *)

procedure simplify;

 var
  op: expr;                     (* IF scanning cursor *)
  e1, e2: expr;                 (* op = (e * cm) + ca *)
  cm1, cm2: int_type;
  ca1, ca2: int_type;
  r, t: expr;           (* temps *)
  rm, tm: int_type;
  numops, i: 0..4095;                   (* for processing operand lists *)


 (* There are three internal functions which generate certain code sequences
    and return the root of the expr tree generated:

      mulc (e, c)  ->  e * c

      addc (e, cm, ca)  ->  (e * cm) + ca

      sub  (e1, cm1, e2, cm2)  ->  (e1 * cm1) - (e2 * cm2)

      add  (e1, cm1, e2, cm2)  ->  (e1 * cm1) + (e2 * cm2)

    All handle obvious special cases, e.g. mulc (e, 1) => e.  Also, by convention,
    if any of the e(n) arguments are nil, then they are assumed to be zero.  The
    philosophy behind passing the constant multipliers to the adding routines is
    that it permits generation of adds or subtracts depending on the signs of the
    multipliers.  All reference "op" to get the result descriptor. *)



 (* SIGN returns -1, 0 or 1 depending on the sign of its argument. *)

 function sign (val: int_type): int_type;
  begin
   if val > 0 then sign := 1
   else if val < 0 then sign := -1
   else sign := 0
  end;
$PAGE splitprod - in simplify
(* SPLITPROD explodes an integer expression into two terms: "epart" and
   "cmultiplier" such that the input tree "e" = epart * cmultiplier.  On
   return epart is always nonnil; cmultiplier is set to 1 if there is no
   constant multiplier directly in the tree.

   Note that because of tree transformations performed in shape, and prior
   simplification of the current node a constant part is always to the right. *)

procedure splitprod (e: expr; var epart: expr; var cmultiplier: int_type);
 var c: int_type;
 begin
  epart := e;                   (* set defaults *)
  cmultiplier := 1;
  with e^ do begin
    if copy_tuple then return;          (* don't simplify evaluated tuple further *)
    if opcode = ineg_op then begin
      epart := operand[1];
      cmultiplier := -1;
    end
    else if (opcode = imul_op) andif is_iconstp (operand[2], c) then begin
      cmultiplier :=  c;
      epart := operand[1];
    end;
  end (* with *) ;
 end;
$PAGE splitsum - in simplify
(* SPLITSUM explodes an integer addition or subtraction into three terms:
   "epart", "caddend" and "cmultiplier" where "e" (the input expression) is the
   same as (epart * cmultiplier) + caddend.  Epart is an expr tree (nil if there
   is no nonconstant part); and caddend and cmultiplier are integer constants
   (0 and 1 are returned if there are no such constant parts). *)

procedure splitsum (e: expr; var epart: expr; var caddend, cmultiplier: int_type);

 var
   t: expr;
   c: int_type;
   enclosing_multiplier: int_type;

 begin
  (* Check for an enclosing product to factor in:  ((epart * cm) + ca) * em gives
     (epart * (cm*em)) + (ca*em) on output.  This helps to propagate inegs up to
     the outermost level of the expression. *)

  splitprod (e, epart, enclosing_multiplier);
  cmultiplier := 1;
  caddend := 0;

  (* Extract the constant addend in epart, if any *)

  with epart^ do begin
    if copy_tuple
      then              (* don't simplify evaluated tuple further *)
    else if opcode = mem_ref then begin
      if (item.class = constant_sc) andif (item.cstref^.kind = scalar_cst) then begin
        epart := nil;
        caddend := item.cstref^.scalar_val;
      end;
    end
    else if opcode = iadd_op then begin
      if is_iconstp (operand[2], caddend)
        then epart := operand[1];
    end
    else if opcode = isub_op then begin
      if is_iconstp (operand[2], c) then begin
        epart := operand[1];
        caddend := - c;
      end
      else if is_iconstp (operand[1], caddend) then begin
        epart := operand[2];
        cmultiplier := -1;
      end
    end
    else if opcode = immed_ref then begin
      epart := item.index;
      caddend := item.offset;
    end;
  end (* with *) ;

  (* Extract constant multiplier in remaining epart *)

  if epart <> nil then begin
    splitprod (epart, t, c);
    epart := t;
    cmultiplier := c * cmultiplier;
  end;

  (* Factor in the enclosing constant multiplier *)

  caddend := caddend * enclosing_multiplier;
  cmultiplier := cmultiplier * enclosing_multiplier;
 end;
$PAGE mulc - in simplify
(* MULC generates a multiply of an expr tree by a constant. *)

function mulc (e: expr; c: int_type; desc: expr_type_desc): expr;
 begin
  if (e = nil) or (c = 0)
    then mulc := int_const (0)
  else if c = 1
    then mulc := e
  else if c = -1
    then mulc := op_1 (ineg_op, desc, e)
  else mulc := op_2 (imul_op, desc, e, int_const (c))
 end;
$PAGE sub, add - in simplify
(* SUB generates a subtract of two expr trees with individual constant multipliers. *)

function sub (e1: expr; cm1: int_type; e2: expr; cm2: int_type; desc: expr_type_desc): expr;
 begin
  if (e1 = e2) and (cm1 = cm2)
    then sub := int_const (0)
    else sub := op_2 (isub_op, desc, mulc (e1, cm1, desc), mulc (e2, cm2, desc));
 end;



(* ADD generates an add of two expr trees with individual constant multipliers. *)

function add (e1: expr; cm1: int_type; e2: expr; cm2: int_type; desc: expr_type_desc): expr;
 begin
  if (e1 = nil) or (cm1 = 0)
    then add := mulc (e2, cm2, desc)
  else if (e2 = nil) or (cm2 = 0)
    then add := mulc (e1, cm1, desc)
  else if (cm1 < 0) and (cm2 < 0)
    then add := op_1 (ineg_op, desc,
                         op_2 (iadd_op, desc, mulc (e1, -cm1, desc), mulc (e2, -cm2, desc)) )
  else if (cm1 < 0)
    then add := sub (e2, cm2, e1, -cm1, desc)
  else if (cm2 < 0)
    then add := sub (e1, cm1, e2, -cm2, desc)
  else add := op_2 (iadd_op, desc, mulc (e1, cm1, desc), mulc (e2, cm2, desc));
 end;
$PAGE addc - in simplify
(* ADDC generates an add of an expr tree (times some constant) and a constant.
   It performs one very special optimization:  if the result of the operation
   is known to be a half word,  an implicit add (i.e. an indexed immediate
   operand) is generated.  Even if the result requires sign extension, the
   move (d <- cst(s)) amounts to a three operand add and thereby permits non-
   destructive adds. *)

function addc (e: expr; cm: int_type; cst: int_type; desc: expr_type_desc): expr;
 var t: expr;
 begin
  if (e = nil) or (cm = 0)
    then addc := int_const (cst)
  else if cst = 0
    then addc := mulc (e, cm, desc)
  else if (op^.desc.int_prec = 18) and (cm > 0)
    then begin
      t := mulc (e, cm, desc);
      new (addc, immed_ref, immed_ref); (* generate an implicit add *)
      init_node (addc, op^.desc);
      with addc^ do begin
        item := immed_item;
        item.offset := cst;
        item.index := t;
      end;
    end
  else begin
    if cst < 0
      then addc := add (e, cm, int_const (-cst), -1, desc)
      else addc := add (e, cm, int_const (cst), 1, desc);
  end;
 end;
$PAGE simplify - body

 begin
  op := t_chain;
  while op <> nil do begin
    with op^ do begin
      transform_operands (op);                  (* replace operands by transformed ones *)
      t_set (prev);             (* put transformations before original node *)
      case opcode of

        iadd_op, isub_op:
          begin
            splitsum (operand[1], e1, ca1, cm1);        (* op = (e * cm) + ca *)
            splitsum (operand[2], e2, ca2, cm2);
            if opcode = isub_op then begin
              ca2 := - ca2;
              cm2 := - cm2;
            end;

            (* If the result is an exactly e1 (+|-) e2, then we set the operands of the
               original node.  Note this may change the operands if ca1 or ca2 <> 0. *)

            if ((ca1 + ca2) = 0)
                  andif ((e1 <> nil) and (e2 <> nil))
                  andif ( (cm1 = 1) and ( ((cm2 = 1) and (opcode = iadd_op)) 
                                            or ((cm2 = -1) and (opcode = isub_op)) ) ) then begin
              operand[1] := e1;
              operand[2] := e2;
            end

            else if (e1 = nil) and (e2 = nil)   (* there are no expr parts => constant sum *)
              then result := int_const (ca1 + ca2)

            else begin
              if ((cm1 mod cm2) = 0) and (e2 <> nil)
                then begin
                  tm := abs (cm2);      (* in add, multipliers have sign of original *)
                  t := add (e1, cm1 div tm, e2, sign (cm2), desc);
                end
              else if ((cm2 mod cm1) = 0) and (e1 <> nil)
                then begin
                  tm := abs (cm1);
                  t := add (e2, cm2 div tm, e1, sign (cm1), desc);
                end
              else begin
                t := add (e1, cm1, e2, cm2, desc);
                tm := 1;
              end;

              splitprod (t, r, rm);             (* force negation upwards *)
              result := addc (r, rm*tm, ca1 + ca2, desc);
            end (* non trivial case *) ;
          end (* iadd, isub *) ;

        imul_op:
          begin
            splitsum (operand[1], e1, ca1, cm1);        (* op = (e * cm) + ca *)
            splitsum (operand[2], e2, ca2, cm2);

            if ((e1 = nil) or (cm1 = 0)) and ((e2 = nil) or (cm2 = 0))
              then result := int_const (ca1 * ca2)
            else if ((e2 = nil) or (cm2 = 0))
              then result := addc (e1, cm1 * ca2, ca1 * ca2, desc)
            else if ((e1 = nil) or (cm1 = 0))
              then result := addc (e2, cm2 * ca1, ca2 * ca1, desc)
            else if (ca1 = 0) and (ca2 = 0)
              then begin
                t := op_2 (imul_op, desc, e1, e2);
                result := mulc (t, cm1 * cm2, desc);
              end
            else if (ca1 = 0)
              then begin
                t := addc (e2, cm2 * cm1, ca2 * cm1, desc);
                splitprod (t, r, rm);
                r := op_2 (imul_op, desc, r, e1);
                result := mulc (r, rm, desc);
              end
            else if (ca2 = 0)
              then begin
                t := addc (e1, cm1 * cm2, ca1 * cm2, desc);
                splitprod (t, r, rm);
                r := op_2 (imul_op, desc, r, e2);
                result := mulc (r, rm, desc);
              end;
          end (* imul case *) ;

        idiv_op:
          begin
            if is_iconstp (operand[1], cm1) andif is_iconstp (operand[2], cm2)
              then result := int_const (cm1 div cm2)
            else begin
              splitprod (operand[1], e1, cm1);
              splitprod (operand[2], e2, cm2);

              if (cm2 <> 0) andif ((cm1 mod cm2) = 0) then
                result := op_2 (idiv_op, desc, mulc (e1, cm1 div cm2, desc), e2)
              else if (cm1 <> 0) andif ((cm2 mod cm1) = 0) then
                result := op_2 (idiv_op, desc, e1, mulc (e2, cm2 div cm1, desc));
            end;
          end;

        ineg_op:
          begin
            splitsum (operand[1], e1, ca1, cm1);
            result := addc (e1, -cm1, -ca1, desc);
          end;

        iabs_op:
          begin
            splitprod (operand[1], e1, cm1);
            t := op_1 (iabs_op, desc, e1);
            result := mulc (t, abs (cm1), desc);
          end;

        mem_ref, addr_ref:
          begin
            if item.index <> nil then begin             (* constant offset may be subsumed into the address *)
              splitsum (item.index, e1, ca1, cm1);
              if ca1 <> 0 then begin

                (* If the item is not packed, then we can add the constant offset
                   directly into the item's address. *)

                if not item.pack then begin
                  if (e1 = nil) or (cm1 = 0)
                    then item.index := nil
                    else item.index := mulc (e1, cm1, item.index^.desc);
                  item.offset := item.offset + ca1;
                end

                (* If the item is a packed reference with no base pointer,
                   then we can add the constant offset in as a bit offset. *)

                else if item.base = nil then begin
                  if (e1 = nil) or (cm1 = 0)
                    then item.index := nil
                    else item.index := mulc (e1, cm1, item.index^.desc);
                  item.offset := item.offset + (ca1 * bits_per_unit);
                end

                (* We have both an index and a base.  The item must be an
                   address reference, since memory references are not allowed
                   to have both.  If the byte pointer (i.e., the base) is an
                   address reference, then we can adjust it by the constant
                   part of the index, unless the byte pointer itself has a
                   non-nil base, in which case we would end up with
                   "addr offset[index](base)", which is forbidden.  (The code
                   generator assumes that an indexed byte pointer does not
                   have any constant offset part.) *)

                else if (* opcode = addr_ref and *)
                        (* item.base <> nil andif *)
                        (item.base^.opcode = addr_ref) andif
                        (item.base^.item.base = nil) then begin
                  new (t, addr_ref, addr_ref);          (* generate new one in case base multi-use *)
                  init_node (t, byte_ptr_desc);
                  t^.item := item.base^.item;
                  add_cst_bit_index (t, ca1);
                  item.base := t;
                  if (e1 = nil) or (cm1 = 0)
                    then result := item.base    (* zero offset and index, must not return addr 0[nil](bptr) *)
                    else item.index := mulc (e1, cm1, item.index^.desc);
                end;
              end

              (* Addr_ref (0[x]) may be transformed into simply "x". *)

              else if (opcode = addr_ref) and (not item.pack) and
                      (item.offset = 0) and (item.class = absolute_sc)
                then result := item.index;
            end

            (* If on processing a preceding pass, a packed indexed array reference
               has its byte address folded to yield a constant bit offset, then it
               is to our advantage to directly reference the array element as a
               slice of a word. *)

            else if (* item.index = nil and *) (item.base <> nil) and (opcode = mem_ref) then begin
              if (item.base^.opcode = addr_ref) andif
                 (item.base^.item.index = nil)
                then item := item.base^.item;
            end;
          end;

        imin_op, imax_op:
          begin
            numops := 0;                        (* count of non-constant operands *)
            if opcode = imin_op
              then tm := maximum (int_type)     (* get boundary value *)
              else tm := minimum (int_type);
            for i := 1 to upperbound (operand) do begin
              splitsum (operand[i], e1, ca1, cm1);
              if (e1 = nil) or (cm1 = 0)
                then begin                      (* constant operand *)
                  if opcode = imin_op
                    then tm := min (tm, ca1)
                    else tm := max (tm, ca1)
                end
                else begin                      (* nonconstant operand, move to next available slot *)
                  numops := numops + 1;
                  operand[numops] := operand[i];
                end;
            end;
            if numops = 0
              then result := scalar_const (desc.base, tm)
            else if numops < upperbound (operand)
              then begin                        (* append folded min/max constant part *)
                numops := numops + 1;
                operand[numops] := scalar_const (desc.base, tm);
              end;
          end;

        assign_op:                              (* suppress simplification of rhs *)
          begin
            if (rhs^.opcode <> mem_ref) and (rhs^.opcode <> desc_ref)   (* only tag expressions *)
              then rhs^.copy_tuple := true;
          end;

        start_stmt:
          cur_source := stmt_source             (* for debugging *)

      end (* case opcode *) ;
      op := next;
    end (* with op^ *) ;
  end (* while *) ;
 end;
$PAGE shape_prologue
(* SHAPE_PROLOGUE generates uses of special values in an imaginary block following
   the start of the intermediate form for a block (i.e. after the start block
   node).  These values include:  the display for the current block, and all
   parameters which will be passed in registers.  The purpose of introducing
   these references is to allow the code generator to find them and tag them
   as residing in certain registers as execution begins. *)

procedure shape_prologue;

 var
   parm: sym;
   node: expr;

 begin
  t_set (t_chain);                      (* place prologue nodes after start_block operator *)

  new (cur_display, display_op, display_op);    (* create common display for current block *)
  init_node (cur_display, pointer_desc);
  cur_display^.nlevels := 0;
  cur_display^.copy_tuple := true;	(* prevent movement into a basic block *)

  if cur_block^.kind = subr_blk then begin

    if cur_block^.subr_sym^.type_desc^.parmlist_size > 6 then
      eval (parm_base (cur_block)) (* parameters passed as block, load parm block pointer *)

    else begin                          (* parms passed in the registers, note each parameter *)
      parm := cur_block^.parm_list.first;
      while parm <> nil do begin
        if parm^.type_desc^.generic     (* reference parameter descriptors *)
          then eval (parm_descriptor (parm, -2));
        if parm^.type_desc^.flexible
          then eval (parm_descriptor (parm, -1));

        (* Reference the parameter:  if it is passed by pointer, then the pointer
           itself is introduced directly as a mem_ref;  otherwise, the parameter
           is introduced by an ident_ref, which will be converted to a memory
           reference of appropriate class by "expand". *)

        if passed_by_address (parm)
          then begin                    (* passed by pointer *)
            new (node, mem_ref, mem_ref);
            parm_access (node, parm);
            if (parm^.kind = values) and (parm^.type_desc^.flexible) and (parm^.type_desc^.kind = strings)
              then init_node (node, byte_ptr_desc)
              else init_node (node, pointer_desc);
          end
          else begin    (* passed by value *)
            new (node, ident_ref, ident_ref);
            initexpr (node, parm^.type_desc);
            node^.id_sym := parm;
            emit (node);
          end;
        eval (node);                    (* reference it in the prologue *)

        parm := parm^.next;
      end (* while *) ;
    end;

    (* If the subroutine is a function returning an aggregate value, then the
       address where the return value is to be placed is passed to the function.
       This pointer is the value denoted by mem_ref (<return sym>). *)

    with cur_block^ do begin
      if (return_sym <> nil) andif passed_by_address (return_sym) then begin
        new (node, mem_ref, mem_ref);
        parm_access (node, return_sym);
        init_node (node, pointer_desc);
        eval (node);
      end;
    end;
  end (* if subr_blk *) ;
 end;
$PAGE shape
(* SHAPE directs conversion of the IF to a form more ammenable for compilation
   on the target machine. *)

public procedure shape;
 begin
  shape_prologue;                             (* generate references to parms, cur display *)


  clr_rslt;                                     (* determine actual precisions *)
  apply_constraints;
  if switch (cur_block^.dump_switches, 'PRECS') then
    dmptuples ('I/F FOR BLOCK $ WITH CONSTRAINED PRECISIONS');

  clr_rslt;                                     (* transform memory references and operators *)
  expand (t_chain, nil);
  reclaim;
  if switch (cur_block^.dump_switches, 'EXPAND') then
    dmptuples ('EXPANDED INTERMEDIATE FORM FOR BLOCK $');

  rearrange_retjumps;

  simplify; (* simplify integer operators *)
  reclaim;
 end.
tdA¦