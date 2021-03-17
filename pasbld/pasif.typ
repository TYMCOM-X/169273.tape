$PAGE PASIF.TYP, last modified 5/11/84, zw
(*  The INTERMEDIATE FORM represents the semanticated program.  The I/F for a
    procedure comprises a doubly-linked chain of tuples.  There are two general
    classes of tuples: typed expression tuples, and untyped statement tuples.
    Expression tuples are retained in the I/F only because they are operands
    of some other tuple; all operands of any tuple must be expression tuples.
    Statement tuples are retained in the I/F implicitly.  *)
$PAGE tuple_opcodes

TYPE
tuple_opcodes = (
(*  E X P R E S S I O N S  *)
(*  ---------------------  *)
(*  Data References  -- Pass I  *)
cst_ref, ident_ref, ptr_ref, field_ref, array_ref, substr_ref, buffer_ref,
(*  Memory References -- Pass II  *)
display_op, mem_ref, addr_ref, immed_ref,
(*  N-ARY OPERATORS  *)
nary_op,
(*  Normal Binary Operators  *)
iadd_op, isub_op, imul_op, idiv_op, imod_op, radd_op, rsub_op, rmul_op,
  rdiv_op, expii_op, expri_op, exprr_op, ile_op, ilt_op, igt_op, ige_op,
  ieq_op, ine_op, rle_op, rlt_op, rgt_op, rge_op, req_op, rne_op, sle_op,
  slt_op, sgt_op, sge_op, seq_op, sne_op, setle_op, setge_op, seteq_op,
  setne_op, ptreq_op, ptrne_op, fileq_op, filne_op, or_op, and_op, orif_op,
  andif_op, in_op, diff_op, both_op, str_comp_op,
(*  Normal Unary Operators  *)
ineg_op, rneg_op, iabs_op, rabs_op, bnot_op, sclcvt_op, strcvt_op, setcvt_op,
  float_op, trunc_op, odd_op, sqrt_op, ln_op, log_op, exp_op, sin_op,
  arcsin_op, sinh_op, cos_op, arccos_op, cosh_op, tan_op, tanh_op, cotan_op,
  length_op, lwc_op, upc_op, lwb_op, upb_op, dim_op, extent_op,
(*  Normal N-ary Operators  *)
gen_set_op, round_op, arctan_op, imin_op, rmin_op, imax_op, rmax_op, index_op,
  verify_op, search_op, agg_val, cat_op, union_op,
(*  Special Binary Operators  *)
desc_ref, subr_var_op, gen_andif_op, gen_orif_op,
(*  Special Unary Operators  *)
new_op, alc_temp_op, addr_op, in_str_op, out_str_op, io_var_str_op,
  io_fix_str_op, eoln_op, eof_op, eopage_op, cursor_op, filesize_op,
  iostatus_op, masked_op, pending_op,
(*  Special N-ary Operators  *)
random_op, set_op, extstatus_op, date_op, time_op, runtime_op, mathstatus_op,
  prgmstatus_op, spclstatus_op, exiostatus_op, open_op, reset_op, rewrite_op,
  update_op,
(*  END N-ARY OPERATORS  *)
func_call_op,
$PAGE
(*  S T A T E M E N T S  *)
(*  -------------------  *)
call_op, start_stmt, start_with, end_with, eval_op, assign_op, dispose_op, nop
  , abort_op, case_abort_op, start_block, end_block, label_node, jump_op,
  retjump_op, jump_t_op, jump_f_op, jump_in_op, jump_cond_op, gen_jump_op,
  hndlr_jump_op, case_jump_op, stop_op, return_op, goto_op, set_handler_op,
  rst_handler_op, signal_op, resignal_op, mask_op, unmask_op, exmessage_op,
  sub_range_chk, str_range_chk, val_range_chk, ptr_chk, file_chk, fld_chk,
  compat_chk, reset_stk_op, start_io_op, end_io_op, get_op, put_op, readln_op,
  writeln_op, page_op, clear_op, break_op, empty_op, close_op, scratch_op,
  read_op, write_op, seek_op, close_all_op );

CONST
first_expr = cst_ref;
last_expr = func_call_op;
first_data_ref = cst_ref;
last_data_ref = buffer_ref;
first_nary_op = nary_op;
last_nary_op = update_op;
first_nbinary_op = iadd_op;
last_nbinary_op = str_comp_op;
first_nunary_op = ineg_op;
last_nunary_op = extent_op;
first_nnary_op = nary_op;
last_nnary_op = union_op;
first_sbinary_op = desc_ref;
last_sbinary_op = gen_orif_op;
first_sunary_op = new_op;
last_sunary_op = pending_op;
first_snary_op = random_op;
last_snary_op = update_op;
first_chk_op = sub_range_chk;
last_chk_op = compat_chk;
first_jump_op = jump_op;
last_jump_op = case_jump_op;
first_io_stmt = start_io_op;
last_io_stmt = scratch_op;
first_io_func = eoln_op;
last_io_func = filesize_op;
$PAGE auxiliary types

TYPE
tuple = ^ tuple_node; (* a TUPLE may point to any tuple node *)
expr = ^ tuple_node; (* an EXPR should only point to expression nodes *)
usage_context = (* possible contexts for an expression: *)
( refx, (* reference to an expr: addr(~) or with ~ do *)
valx, (* the value of an expr is used *)
modx, (* the value of a reference expr is changed *)
basemodx, (* base array, record or string of a mod component *)
varx, (* a reference expr is pased as a var parameter *)
basevarx ); (* base array, record or string of a var component *)
rd_wr_mode = (* formatting and transfer modes for READ and WRITE *)
( binaryrw, (* width parm is number of words to transfer *)
imagerw, (* READ8 and WRITE8 *)
booleanrw, (* boolean - output only *)
decimalrw, (* decimal integer conversion *)
octalrw, (* octal integer conversion *)
hexrw, (* hexadecimal integer conversion *)
realrw, (* G-format real conversion *)
fixedrw, (* F-format real conversion *)
floatrw, (* E-format real conversion *)
leftrw, (* left-justfified string conversion *)
rightrw ); (* right-justified string conversion *)
fre = id_range; (* FREs are used in optimization. *)
usage_range = 0 .. 4095; (* usage counts for expression tuples *)
oper_range = 0 .. 4095; (* for counting operands of tuples *)
$PAGE expr_type_desc
(* An EXPR TYPE DESC is included in every expression tuple, and provides an
   abbreviated description of its result type.  The BASE field is a pointer
   to a type node.  In general, this is the result type of the expression,
   and the type descriptor merely provides a shortcut to some of the more
   useful information in it.  However, there are three special cases:

     -  For a set expression in a value context, the base type is the maximal
	subrange of the set element type, rather than the set type.

     -  For some string expressions, there may not be a type node for the
	result string type.  In this case, the base pointer will be nil, and
	all the result type information must be obtained from the descriptor.

     -  For some real expressions, the base type is simply the standard real
	type, and the actual real precision of the result must be obtained
	from the descriptor. *)
expr_type_desc = PACKED RECORD
  base: typ;
  CASE kind: type_kind OF (* type-specific information *)
    bools, ints, chars, scalars, pointers, files: (
      signed: BOOLEAN; (* indicates value has sign bit *)
      int_prec: align_range ); (* binary precision of result *)
    reals: (
      precision: prec_type ); (* type precision *)
    strings: (
      str_kind: string_kind; (* varying/nonvarying *)
      str_flex: BOOLEAN; (* => flexible type, length is only upper bound *)
      str_length: char_range ); (* string upperbound/length *)
    sets: (
      set_cst_lwb: BOOLEAN; (* => first set element known at compile time *)
      set_cst_len: BOOLEAN; (* => set size known at compile time *)
      set_lwb: set_range; (* first element -- const or lower bound *)
      set_length: set_range ) (* # of elements -- const or upper bound *)
END;
$PAGE mem_addr_desc
(* A MEM ADDR DESC contains all the information for a MEM, ADDR, or IMMED REF
   tuple. *)
mem_addr_desc = PACKED RECORD
  base: expr; (* base address in storage units *)
  INDEX: expr; (* in units of item size *)
  offset: int_type; (* constant offset in bits or units *)
  pack: BOOLEAN; (* true => ref to packed item, offset in bits *)
  SIZE: align_range; (* item size in bits *)
  CASE class: storage_class OF (* kind of item being referenced *)
    constant_sc: (
      cstref: ^ value_node ); (* reference to constant *)
    local_sc, parameter_sc, static_sc, external_sc, code_sc, absolute_sc: (
      sym_name: sym ) (* reference to a declared symbol *)
END;
$PAGE tuple_node - miscellaneous statements
tuple_node = PACKED RECORD
  prev, next: tuple; (* linear chain of operators in block *)
  nodeid: id_range; (* unique per program block *)
  CASE opcode: tuple_opcodes OF
    start_block: (* designates first node of body of a block *)
    (
      block: blk; (* corresponding block node in symbol table *)
      first_label, last_label: tuple; (* first and last label nodes in the block *)
      final_tuple: tuple ); (* the end block tuple for the block *)
    end_block: (
      ); (* terminates the body of a block *)
    start_stmt: (* flags the beginning of a statement in the source *)
    (
      stmt_kind: symbols; (* if, simple, etx. *)
      stmt_index: 0 .. 63; (* statement index in source line *)
      first_on_line: BOOLEAN; (* true for first on source line *)
      stmt_source: source_id ); (* location in text of stmt, for debugger *)
    start_with, (* enter the scope of a with *)
    end_with: (* leave the scope of a with *)
    (
      with_rec: expr ); (* referenced record value *)
    eval_op, (* evaluate an expression *)
    assign_op: (* evaluate and assign an expression *)
    (
      must_store: BOOLEAN; (* assignment is mandatory *)
      lrecursive: BOOLEAN; (* lhs is the first rhs operand *)
      rrecursive: BOOLEAN; (* lhs is the second rhs operand *)
      overlaps: BOOLEAN; (* lhs overlaps an operand *)
      lhs: expr; (* target of assignment; nil for eval *)
      rhs: expr ); (* value to assign or evaluate *)
    dispose_op: (* dispose a variable on the heap *)
    (
      dptrarg: expr );
    nop: (
      ); (* place holder *)
$PAGE tuple_node - flow control statements
    label_node: (* defines a (dummy) label, start of a basic block *)
    (
      block_order_no: index_range; (* DFST ordering, 1 is entry block *)
      label_sym: sym; (* the label defined, nil if just start of block *)
      downward_thread: tuple; (* to block with next highest order number *)
      upward_thread: tuple; (* to block with next lowest order number *)
      inward_jumps: tuple; (* to first jump impinging on this block *)
      outward_jumps: tuple; (* to first jump leaving this block,
							 if nil, then this is terminal edge *)
      in_handler: tuple; (* the handler clause in effect here *)
      idom: tuple; (* immediately dominating block *)
      dom_son: tuple; (* first dominated block *)
      dom_brother: tuple ); (* next dominated block under idom *)
    jump_op, (* unconditional branch *)
    retjump_op, (* virtual jump, indicating nonlocal goto return from subr *)
    jump_t_op, (* jump if condition true *)
    jump_f_op, (* jump if condition false *)
    jump_in_op, (* jump if value in range *)
    jump_cond_op, (* jump if specified condition signaled *)
    gen_jump_op, (* jump to operator to load value of andif/orif *)
    hndlr_jump_op, (* handler clause dispatch *)
    case_jump_op: (* case dispatch *)
    (
      cond: expr; (* condition or selector, nil -> unconditional *)
      jump_from: tuple; (* basic block exited *)
      jump_to: tuple; (* basic block entered *)
      next_inward_jump: tuple; (* next jump entering "to" block *)
      low_cntrl, high_cntrl: machine_word ); (* range of values of computation causing jump *)
    (* For a case_jump_op, the jump_op fields have the following meanings:
	       jump_to      => the 'others' label of the case
	       cond         => the selector expression for the case
	       low_cntrl    => the lowest explicit value in any case label
	       high_cntrl   => the highest explicit value in any case label
	     The case_jump_op is followed by a list of jump_in_ops denoting
	     the individual cases. *)
    stop_op: (
      ); (* halt the program *)
    return_op: (
      ); (* return from subroutine block *)
    abort_op: (
      ); (* abnormal termination for assertion failure *)
    case_abort_op: (
      ); (* abnormal termination for case error *)
    goto_op: (* non local goto *)
    (
      target_lab: sym; (* label to branch to *)
      target_frame: expr ); (* frame containing instance of label *)
$PAGE condition management statements
    set_handler_op, (* establish the current handler *)
    rst_handler_op: (* restore previous handler *)
    (
      hndlr_tuple: tuple ); (* the label on the hndlr_jump_op *)
    signal_op, (* signal a condition *)
    mask_op, (* mask a condition *)
    unmask_op: (* unmask a condition *)
    (
      cond_parm: expr ); (* the condition *)
    resignal_op: (* resignal the last condition *)
    (
      );
    exmessage_op: (* exception_message call *)
    (
      );
$PAGE tuple_node - input/output statements
    start_io_op, (* start of io on a file *)
    end_io_op, (* end of io on a file *)
    get_op, (* read into the file buffer *)
    put_op, (* write out the file buffer *)
    readln_op, (* do GETs until start of line *)
    writeln_op, (* output an end-of-line *)
    page_op, (* output a form-feed *)
    clear_op, (* discard any incomplete operations *)
    break_op, (* complete any pending output operations *)
    empty_op, (* delete the contents of the file *)
    close_op, (* terminate the association *)
    scratch_op: (* close, and purge the file *)
    (
      old_file: BOOLEAN; (* true => implicit file *)
      file_arg: expr ); (* the file the operation applies to *)
    read_op, (* read a (formatted) item *)
    write_op: (* write a (formatted) item *)
    (
      rw_mode: rd_wr_mode; (* the mode of the transfer *)
      rw_old_file: BOOLEAN; (* is the file argument implicit? *)
      rw_file: expr; (* the file or i/o-string argument *)
      rw_item: expr; (* the item to be read/written *)
      rw_width: expr; (* the field_width *)
      rw_precision: expr ); (* for reals, the precision *)
    seek_op: (* random access to a particluar item *)
    (
      seek_file: expr; (* the file to be accessed *)
      seek_index: expr ); (* the item "address" in the file *)
    close_all_op: (
      ); (* close all open files *)
$PAGE tuple_node - data reference expressions
    first_expr..last_expr, first_chk_op..last_chk_op, call_op: (* treated like expr as they have parm lists *)
    (
      context: usage_context; (* how expression is used *)
      blk_input_tuple: BOOLEAN; (* true if the tuple is a basic block input *)
      copy_tuple: BOOLEAN; (* load and save in a temporary *)
      killed_tuple: BOOLEAN; (* ref value doesn't match tuple value *)
      desc: expr_type_desc; (* result type information *)
      result: expr; (* location of result *)
      ref_fre: fre; (* Formal Reference Expression class *)
      usage_count: usage_range; (* number of references to this node in a basic block *)
      CASE tuple_opcodes OF
	cst_ref: (* reference to a constant value *)
	(
	  cst_val: val );
	ident_ref: (* reference to non-field identifier *)
	(
	  id_sym: sym ); (* symbol table entry in scope with id name *)
	field_ref: (* reference to field within record *)
	(
	  base_rec: expr; (* record from which field is selected *)
	  field_sym: sym ); (* symbol table field record with this name *)
	ptr_ref: (* dereference a pointer to get value *)
	(
	  base_ptr: expr ); (* gives ptr value being dereferenced *)
	array_ref: (* indexed reference to element of array *)
	(
	  base_array: expr; (* array value being indexed; is another array_ref,
							     when multiple dimensions are used *)
	  index_val: expr );
	substr_ref: (* substring reference *)
	(
	  base_string: expr; (* the string being selected from *)
	  substr_index: expr; (* the index into the string *)
	  substr_length: expr ); (* the length to extract; nil => remainder *)
	buffer_ref: (* reference to file buffer *)
	(
	  base_file: expr ); (* the file whose buffer it is *)
	display_op: (* stack base for a block *)
	(
	  nlevels: 0..63 ); (* 0 is stack of current, 1 is parent, etc. *)
	mem_ref, (* reference to contents of storage *)
	addr_ref, (* reference to address of storage *)
	immed_ref: (* reference to immediate value *)
	(
	  item: mem_addr_desc ); (* location referenced *)
$PAGE tuple_node - computational expressions
	call_op, (* properly a control operator *)
	func_call_op: (* invoke a function *)
	(
	  subr: expr; (* procedure or function to invoke *)
	  arglist: PACKED ARRAY [1..*] OF expr ); (* list of arguments *)
	nary_op..last_nary_op, (* all simple operators *)
	first_chk_op..last_chk_op: (* + all range check statements *)
	(
	  operand: PACKED ARRAY [1..*] OF expr ) (* each operand *)
	  (* end expression nodes *))
END;
 