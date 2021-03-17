$TITLE passfc -- Pascal Standard Function Call Semantics
$LENGTH 43

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S S F C                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This  module  contains  the  StdFcall  routine,  which
        performs semantic analysis for calls on standard functions.
     
     ENTRY POINTS:
     
        std_fcall   is the function which evaluates standard function
                    call semantics.  It  takes  a  standard  function
                    code,  an arg_list (which is a parse tree chain),
                    and the defining parse tree  node  for  the  call
                    (used  only  for  error messages), and returns an
                    expression tree representing the call.  Note that
                    this   is   not   necessarily   a  function  call
                    expression tree; some standard  functions  return
                    constant values.
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE pasutl.inc
$INCLUDE pasifu.inc
$INCLUDE pascmp.inc
$INCLUDE pasesu.inc
$INCLUDE pasemu.inc
$INCLUDE pa1xrf.inc
$INCLUDE pasglb.inc
$INCLUDE pasref.inc
$INCLUDE pasesm.inc
$INCLUDE pasval.inc
$INCLUDE paserr.inc
$PAGE std_fcall

(*  StdFcall is called with a standard function code and an argument list,
    and returns the appropriate expression tree structures for the call.  *)


public function std_fcall ( func_sym: sym;
                            arg_list: parse_node;
                            context: xrf_class;
                            name_node: parse_node ): expr;


(*  Most standard functions take a bounded number of arguments, the values of
    which are of interest (as opposed to the addresses or types, for example).
    Prior to the evaluation of such a function, a1, a2, and a3 will be set to
    the parse nodes for the arguments and arg1, arg2, and arg3 will be set to
    the expression tree representations for the arguments.  (The excess args
    will be set to nil.)  *)
var
    code: std_func;

    a1, a2, a3: parse_node;
    arg1, arg2, arg3: expr;
$PAGE argument and call descriptions

type call_desc = array [std_func] of packed record
        min_parms: parm_range;
        max_parms: parm_range;
        val_parms: boolean
     end;

const arg_limits: call_desc = (
    (* abs *)           (1, 1, true),
    (* sqr *)           (1, 1, true),
    (* sqrt *)          (1, 1, true),
    (* ln *)            (1, 1, true),
    (* log *)           (1, 1, true),
    (* exp *)           (1, 1, true),
    (* sin *)           (1, 1, true),
    (* arcsin *)        (1, 1, true),
    (* sinh *)          (1, 1, true),
    (* cos *)           (1, 1, true),
    (* arccos *)        (1, 1, true),
    (* cosh *)          (1, 1, true),
    (* tan *)           (1, 1, true),
    (* arctan *)        (1, 2, true),
    (* tanh *)          (1, 1, true),
    (* cotan *)         (1, 1, true),
    (* random *)        (0, 1, true),
    (* min *)           (1, maximum(parm_range), false),
    (* max *)           (1, maximum(parm_range), false),
    (* odd *)           (1, 1, true),
    (* round *)         (1, 2, true),
    (* trunc *)         (1, 1, true),
    (* ord *)           (1, 1, true),
    (* chr *)           (1, 1, true),
    (* succ *)          (1, 1, true),
    (* pred *)          (1, 1, true),
    (* minimum *)       (1, 1, false),
    (* maximum *)       (1, 1, false),
    (* eoln *)          (0, 1, true),
    (* eopage *)        (0, 1, true),
    (* eof *)           (0, 1, true),
    (* cursor *)        (1, 1, true),
    (* iostatus *)      (0, 1, false),
    (* extstatus *)     (0, 0, false),
    (* mathstatus *)	(0, 0, false),
    (* programstatus *)	(0, 0, false),
    (* specialstatus *)	(0, 0, false),
    (* exiostatus *)	(0, 0, false),
    (* date *)          (0, 0, false),
    (* time *)          (0, 0, false),
    (* runtime *)       (0, 0, false),
    (* masked *)	(1, 1, false),
    (* pending *)	(1, 1, false),
    (* length *)        (1, 1, true),
    (* lowercase *)     (1, 1, true),
    (* uppercase *)     (1, 1, true),
    (* substr *)        (2, 3, true),
    (* index *)         (2, 3, true),
    (* verify *)        (2, 3, true),
    (* search *)        (2, 3, true),
    (* lowerbound *)    (1, 2, false),
    (* upperbound *)    (1, 2, false),
    (* dimension *)     (1, 2, false),
    (* size *)          (1, maximum(parm_range), false),
    (* extent *)        (1, 1, true),
    (* address *)       (1, 1, false),
    (* xfer *)          (2, 2, false) );



type
    rop_list = array [fnsqrt..fncotan] of tuple_opcodes;

const
    rops: rop_list =
      ( sqrt_op, ln_op, log_op, exp_op,
        sin_op, arcsin_op, sinh_op,
        cos_op, arccos_op, cosh_op,
        tan_op, arctan_op, tanh_op, cotan_op );


type
    io_op_list = array [fneoln..fneof] of tuple_opcodes;

const
    io_ops: io_op_list := ( eoln_op, eopage_op, eof_op );
$PAGE chk_arg_type

(*  ChkArgType checks an argument to make sure that its type is in a
    specified set of type-kinds, printing a specified error message if
    it is not.  *)

type kind_set = set of type_kind;

const
    num_kinds: kind_set = [ints,reals];
    scl_kinds: kind_set = [ints,bools,chars,scalars];
    str_kinds: kind_set = [strings,chars];

function chk_arg_type ( arg: expr; ptn: parse_node;
                      kinds: kind_set; msg_text: string ): boolean;
begin
  with arg^.desc do
    if kind in kinds then
      chk_arg_type := true
    else begin
      chk_arg_type := false;
      if kind <> unknown_type then
        err_op (err_std_call_arg,msg_text,ptn);
    end;
end (* chk_arg_type *);
$PAGE check_chars

(*  CheckChars checks an argument to make sure that it is a set of
    characters.  *)

function check_chars ( arg: expr; ptn: parse_node ): boolean;

begin
  with arg^.desc do
    if (kind = sets) andif
      ((base = nil) orif (base^.kind = chars)) then
        check_chars := true
    else begin
      check_chars := false;
      if kind <> unknown_type then
        err_op (err_std_call_arg,'a set of characters',ptn);
    end;
end (* check_chars *);
$PAGE real_arg

(*  RealArg checks to make sure that its argument is numeric, and then
    converts it to a real if it is an integer.  *)

function real_arg ( var arg: expr; ptn: parse_node ): boolean;

begin
  real_arg := chk_arg_type (arg,ptn,num_kinds,'numeric');
  if real_arg andif (arg^.desc.kind <> reals) then
    arg := op_real (arg, type_real^.precision);
end (* real_arg *);
$PAGE bad_call

(*  BadCall creates an nary_op tuple with no operands, to take the place of
    a bad function call.  *)

procedure bad_call;

begin
  new (std_fcall, nary_op, nary_op, 0);
  initexpr (std_fcall, nil);
  emit (std_fcall);
end (* bad_call *);
$PAGE make_unary_op

(*  MakeUnaryOp creates a specified unary operator node for the result of
    StdFcall, with 'arg1' as its operand.  *)

procedure make_unary_op ( op: tuple_opcodes; rslt_type: typ );

begin
  new (std_fcall,nary_op,nary_op,1);
  initexpr (std_fcall,rslt_type);
  with std_fcall^ do begin
    opcode := op;
    operand[1] := arg1;
    if (desc.kind = reals) and (arg1 <> nil) then
      desc.precision := arg1^.desc.precision;
  end;
  emit (std_fcall);
end (* make_unary_op *);
$PAGE make_binary_op

(*  MakeBinaryOp creates a specified binary operator node for the result of
    StdFcall, with 'arg1' and 'arg2' as its operands.  *)

procedure make_binary_op ( op: tuple_opcodes; rslt_type: typ );

begin
  new (std_fcall,nary_op,nary_op,2);
  initexpr (std_fcall,rslt_type);
  with std_fcall^ do begin
    opcode := op;
    operand[1] := arg1;
    operand[2] := arg2;
    if (desc.kind = reals) and (arg1 <> nil) then
      desc.precision := arg1^.desc.precision;
  end;
  emit (std_fcall);
end (* make_binary *);
$PAGE make_env_op

(*  MakeEnvOp creates a specified environment inquiry operator node for the
    result of StdFcall.  *)

procedure make_env_op ( op: tuple_opcodes; env_type : typ );

begin
  new (std_fcall,nary_op,nary_op,0);
  if op = date_op
    then initstr (std_fcall,false,9)
    else initexpr (std_fcall,env_type);
  std_fcall^.opcode := op;
  emit (std_fcall);
end (* make_env_op *);
$PAGE make_str_op

(*  MakeStrOp creates a specified string operator node for the result of
    StdFcall, with 'arg1', 'arg2', and 'arg3' as its operands.  *)

procedure make_str_op ( op: tuple_opcodes; rslt_type: typ );

begin
  if arg3 = nil
    then new (std_fcall,nary_op,nary_op,2)
    else new (std_fcall,nary_op,nary_op,3);
  initexpr (std_fcall,rslt_type);
  with std_fcall^ do begin
    opcode := op;
    operand [1] := arg1;
    operand [2] := arg2;
    if arg3 <> nil
      then operand[3] := arg3;
  end;
  emit (std_fcall);
end (* make_str_op *);
$PAGE make_min_max_op

(*  MakeMinMaxOp creates an aggregate operator node for the real or integer
    MIN or MAX call with the given argument list.  *)

procedure make_min_max_op ( int_op, real_op: tuple_opcodes );

var
    a: parse_node;
    arg: expr;
    real_args: boolean;
    max_real_prec: prec_type;
    mode_ok: boolean;
    numeric_mode: boolean;
    base_type: typ;
    n, i: integer;

begin
  n := 0; (* Count the arguments. *)
  a := arg_list;
  i := 0;
  while a <> nil do begin
    n := n + 1;
    a := a^.next;
  end;

  new (std_fcall,agg_val,agg_val,n);(* Create the operator. *)
  with std_fcall^ do begin
    real_args := false;
    max_real_prec := minimum (prec_type);
    a := arg_list;
    for i := 1 to n do begin
      arg := val_semantics (a);
      if i = 1 then begin (* Check for scalar/numeric op. *)
        mode_ok := chk_arg_type (arg, a, num_kinds + scl_kinds, 'scalar or numeric');
        if mode_ok then begin (* Determine mode. *)
          numeric_mode := (arg^.desc.kind in num_kinds);
          base_type := arg^.desc.base;
	  if base_type^.kind in [ints, bools, chars, scalars] then
	    base_type := base_type^.base_type;
        end;
      end
      else if numeric_mode then
        mode_ok := chk_arg_type (arg, a, num_kinds, 'numeric')
      else begin
        mode_ok := equivtypes (base_type, arg^.desc.base^.base_type);
        if not mode_ok then
          if int_op = imin_op
            then err_op (err_incompatible_types, 'MIN', a)
            else err_op (err_incompatible_types, 'MAX', a);
      end;
    exit if (not mode_ok) or (arg^.desc.kind = unknown_type);
      if arg^.desc.kind = reals then begin
        real_args := true;
        max_real_prec := max (max_real_prec, arg^.desc.precision);
      end
      else
        max_real_prec := max (max_real_prec, type_real^.precision);
      operand[i] := arg;
      a := a^.next;
    end (* for i *);

    if a <> nil then
      initexpr (std_fcall, nil)
    else if real_args then begin                (* convert arguments to reals *)
      opcode := real_op;
      for i := 1 to n do
        operand[i] := op_real (operand[i], max_real_prec);
      initexpr (std_fcall, type_real);
      desc.precision := max_real_prec;
    end
    else begin
      opcode := int_op;
      initexpr (std_fcall, base_type^.base_type);
    end;
  end (* with std_fcall^ *);
  emit (std_fcall);
end (* make_min_max_op *);
$PAGE make_bound_op

(*  MakeBoundOp creates an upperbound, lowerbound, or dimension operator,
    as specified.  *)

procedure make_bound_op ( op: tuple_opcodes; name: string );

var
    elem_type: typ;
    array_type: typ;
    rslt_type: typ;
    dim_num: val;
    backup_tuple, tcursor: tuple;

label
    100 (* error *);

begin
  backup_tuple := if_cursor;
  arg1 := ref_semantics (arg_list, ref_ctxt);
  if not chk_arg_type (arg1,arg_list,[arrays,strings,sets],
                     'an array, string, or set') then goto 100;
  if not ( ( (arg1^.opcode = ident_ref) andif (arg1^.id_sym^.kind = types) ) or
           ( arg1^.opcode in [first_data_ref..last_data_ref] - [substr_ref] ) ) then begin
    err_op (err_bnd_ref, name, arg_list);
    goto 100;
  end;
  with arg1^.desc do

    if kind = arrays then begin
      if arg_list^.next = nil then
        dim_num.ival := 1 (* Default dimension. *)
      else
        if not constant (arg_list^.next, type_int, dim_num, rslt_type) then
          goto 100;
      if dim_num.ival < 1 then begin
        err_node (err_dim_less_one,arg_list^.next);
        goto 100;
      end;
      array_type := base;
      while dim_num.ival > 1 do begin (* Get the selected dimension of the array. *)
        array_type := array_type^.element_type;
        if array_type^.kind <> arrays then begin
          err_node (err_dim_too_large,arg_list^.next);
          goto 100;
        end;
        dim_num.ival := dim_num.ival - 1;
      end;
      std_fcall := arr_bound (op, arg1, array_type^.index_type);
    end

    else if kind = strings then begin
      if arg_list^.next <> nil then begin
        err_op (err_dim_specified,'string',arg_list^.next);
        goto 100;
      end;
      if op = lwb_op then (* LOWERBOUND(string) = 1. *)
        std_fcall := cst_expr (cst_scalar(1),type_int)
      else if not str_flex then (* UPPERBOUND/DIMENSION(non-flex string). *)
        std_fcall := cst_expr (cst_scalar(str_length),type_int)
      else (* UPPERBOUND/DIMENSION(flexible string). *)
        make_unary_op (upb_op, type_int);
    end

    else (* kind = sets *) begin
      if arg_list^.next <> nil then begin
        err_op (err_dim_specified,'set',arg_list^.next);
        goto 100;
      end;
      if base^.kind = sets
        then elem_type := base^.set_element_type
        else elem_type := base;
      with elem_type^ do
        if op = lwb_op then
          std_fcall := cst_expr (cst_scalar(minval),base_type)
        else if op = upb_op then
          std_fcall := cst_expr (cst_scalar(maxval),base_type)
        else (* op = dim_op *)
          std_fcall := cst_expr (cst_scalar(maxval-minval+1),type_int);
    end;

  if (arg1^.opcode = ident_ref) andif (* A runtime bound op cannot be applied to a type name. *)
     (arg1^.id_sym^.kind = types) andif
     (std_fcall^.opcode <> cst_ref) then begin
    case op of
      upb_op: err_node (err_upb_type, arg_list);
      lwb_op: err_node (err_lwb_type, arg_list);
      dim_op: err_node (err_dim_type, arg_list)
    end;
    bad_call;
  end;
  if std_fcall^.opcode = cst_ref then begin
    while if_cursor^.prev <> backup_tuple do begin
      tcursor := if_cursor^.prev;
      dechain (tcursor);
    end;
  end;
  return; (* If we've gotten here, we have a good result. *)

100: (* Generate a dummy result after an error. *)

  bad_call;
end (* make_bound_op *);
$PAGE make_size_op
(*  MakeSizeOp creates a size operator for a reference.  *)

procedure make_size_op;

var basetype, subtype: typ;
    tl: tag_value_list;
    upb: expr;
    backup_tuple, tcursor: tuple;

begin
  backup_tuple := if_cursor;
  arg1 := ref_semantics (arg_list, ref_ctxt); (* See what we are to find the size of. *)

  if arg1^.desc.kind = unknown_type then begin
    bad_call;
    return;
  end;

  with arg1^ do
    if not ( ( (opcode = ident_ref) andif
               (id_sym^.kind in [types, vars, values, for_inds, consts]) ) orif
             (opcode in [field_ref, array_ref, ptr_ref, buffer_ref]) ) then begin
      err_node (err_size_arg,arg_list);
      bad_call;
      return;
    end;

  if pcomponent (arg1) then begin
    err_node (err_size_packable,arg_list);
    bad_call;
    return;
  end;

  basetype := arg1^.desc.base;
  while if_cursor^.prev <> backup_tuple do begin
    tcursor := if_cursor^.prev;
    dechain (tcursor);
  end;
  tag_scan (basetype, arg_list, false, subtype, tl, upb);
  std_fcall := rtsize (subtype, upb, false);
end (* make_size_op *);
$PAGE std_fcall - main routine
var
    args_ok: boolean;
    precision: prec_type;
    chk: tuple;
    backup_tuple, tcursor: tuple;

begin
  if func_sym^.kind = std_funcs then begin
    if prog_options.global_opt and in_body then
      writeln (glob_file, '+', func_sym^.name^.text);
    code := func_sym^.std_pf_code;
  end
  else (* Must be a type coercion. *)
    code := fnxfer;
  xrf_write (call_xrf);
  with arg_limits [code] do begin
    if not ck_arg_count (arg_list,min_parms,max_parms,name_node) then begin
      std_fcall := bad_expr ();
      return;
    end;
    if val_parms then begin
      arg1 := nil;
      arg2 := nil;
      arg3 := nil;
      args_ok := true;
      a1 := arg_list;
      if a1 <> nil then begin

        (*  We evaluate the first argument in the context of the function call.
            Note, however, that in many cases the argument is actually in a
            value context.  In these cases, we rely on the fact that the function
            in question may itself only be called in a value context.  *)

        arg1 := base_semantics (a1, context);
        if arg1^.desc.kind = unknown_type then
          args_ok := false;
        a2 := a1^.next;
        if a2 <> nil then begin
          arg2 := val_semantics (a2);
          if arg2^.desc.kind = unknown_type then
            args_ok := false;
          a3 := a2^.next;
          if a3 <> nil then begin
            arg3 := val_semantics (a3);
            if arg3^.desc.kind = unknown_type then
              args_ok := false;
          end;
        end;
      end;
      if not args_ok then begin
        std_fcall := bad_expr ();
        return;
      end;
    end (* val_parms *);
  end (* with arg_limits [code] *);
  case code of  fnabs:
      if chk_arg_type (arg1,a1,num_kinds,'numeric') then
        if arg1^.desc.kind = ints
          then make_unary_op (iabs_op,type_int)
          else make_unary_op (rabs_op,type_real)
      else
        bad_call;

    fnsqr:
      if chk_arg_type (arg1,a1,num_kinds,'numeric') then begin
        arg2 := arg1;
        arg1^.usage_count := arg1^.usage_count + 1;
        if arg1^.desc.kind = ints
          then make_binary_op (imul_op,type_int)
          else make_binary_op (rmul_op,type_real);
      end
      else
        bad_call;

    fnsqrt..fntan,
    fntanh..fncotan:
      if real_arg (arg1,a1)
        then make_unary_op (rops[code],type_real)
        else bad_call;

    fnarctan:
      if real_arg (arg1,a1) then
        if arg2 = nil then
          make_unary_op (arctan_op,type_real)
        else if real_arg (arg2,a2) then begin
          precision := max (arg1^.desc.precision, arg2^.desc.precision);
          arg1 := op_real (arg1, precision);
          arg2 := op_real (arg2, precision);
          make_binary_op (arctan_op,type_real);
        end
        else
          bad_call
      else
        bad_call;

    fnrandom:
      if arg1 = nil then begin
        new (std_fcall, random_op, random_op, 0);
        initexpr (std_fcall, type_real);
        emit (std_fcall);
      end
      else if real_arg (arg1,a1) then
        make_unary_op (random_op,type_real)
      else
        bad_call;

    fnmin:
      make_min_max_op (imin_op,rmin_op);

    fnmax:
      make_min_max_op (imax_op,rmax_op);

    fnodd:
      if chk_arg_type (arg1,a1,[ints],'an integer')
        then make_unary_op (odd_op,type_bool)
        else bad_call;

    fnround:
      if real_arg (arg1,a1) then
        if arg2 = nil then
          make_unary_op (round_op, type_int)
        else if chk_arg_type (arg2,a2,[ints],'an integer') then
          make_binary_op (round_op, type_real)
        else
          bad_call
      else
        bad_call;

    fntrunc:
      if chk_arg_type (arg1,a1,num_kinds,'numeric') then
        if arg1^.desc.kind = ints
          then std_fcall := arg1
          else make_unary_op (trunc_op,type_int)
      else
        bad_call;

    fnord:
      if arg1^.desc.kind in [pointers, files] then
        make_unary_op (sclcvt_op,type_fullword)
      else if chk_arg_type (arg1,a1,scl_kinds,'scalar') then
        if arg1^.desc.kind = ints
          then std_fcall := arg1
          else make_unary_op (sclcvt_op,type_fullword)
      else
        bad_call;

    fnchr:
      if chk_arg_type (arg1,a1,[ints],'an integer') then begin
	if chk_val_opt in cur_block^.semantic_options then
	  chk_scalar (type_char, arg1, a1);
        make_unary_op (sclcvt_op, type_char);
      end
      else
        bad_call;

    fnsucc:
      if chk_arg_type (arg1,a1,scl_kinds,'scalar') then begin
        arg2 := cst_expr (cst_scalar (1),arg1^.desc.base);
        make_binary_op (iadd_op,arg1^.desc.base);
      end
      else
        bad_call;

    fnpred:
      if chk_arg_type (arg1,a1,scl_kinds,'scalar') then begin
        arg2 := cst_expr (cst_scalar (1),arg1^.desc.base);
        make_binary_op (isub_op,arg1^.desc.base);
      end
      else
        bad_call;

    fnminimum:  begin
      backup_tuple := if_cursor;
      arg1 := ref_semantics (arg_list, ref_ctxt);
      if arg1^.desc.kind = reals then
        with arg1^.desc do
          std_fcall := cst_expr (mkreal(base^.rminval,precision), base)
      else if chk_arg_type (arg1,arg_list,scl_kinds,'scalar') then
        with arg1^.desc do
          std_fcall := cst_expr (cst_scalar(base^.minval), base)
      else
        bad_call;
      while if_cursor^.prev <> backup_tuple do begin
	tcursor := if_cursor^.prev;
	dechain (tcursor);
      end;
    end;

    fnmaximum:  begin
      backup_tuple := if_cursor;
      arg1 := ref_semantics (arg_list, ref_ctxt);
      if arg1^.desc.kind = reals then
        with arg1^.desc do
          std_fcall := cst_expr (mkreal(base^.rmaxval,precision), base)
      else if chk_arg_type (arg1,arg_list,scl_kinds,'scalar') then
        with arg1^.desc do
          std_fcall := cst_expr (cst_scalar(base^.maxval), base)
      else
        bad_call;
      while if_cursor^.prev <> backup_tuple do begin
	tcursor := if_cursor^.prev;
	dechain (tcursor);
      end;
    end;

    fneoln..fneof:  begin
      if arg1 = nil then begin
        arg1 := new_ident (file_input);
        xrf_use (file_input, name_node^.source, value_ctxt);
      end;
      if chk_arg_type (arg1, a1, [files], 'a file') then
        if (code = fneof) or (arg1^.desc.base^.file_kind = textfile) then begin
          if chk_fil_opt in cur_block^.semantic_options then
            chk := op1 (file_chk, nil, arg1);
          make_unary_op (io_ops [code], type_bool);
          xrf_write (fileblk_xrf);
          xrf_use (arg1^.desc.base^.file_class, a1^.source, value_ctxt);
        end
        else begin
          err_op (err_std_call_arg, 'a text file', a1);
          bad_call;
        end
      else
        bad_call;
    end;

    fncursor:
      if chk_arg_type (arg1, a1, [files], 'a file') then begin
        make_unary_op (cursor_op, type_int);
        xrf_write (fileblk_xrf);
        xrf_use (arg1^.desc.base^.file_class, a1^.source, value_ctxt);
      end
      else
        bad_call;

    fniostatus:
      if arg_list = nil then begin
        arg1 := nil;
        make_unary_op (iostatus_op, stat_io);
      end
      else begin
        arg1 := val_semantics (arg_list);
        if chk_arg_type (arg1, arg_list, [files], 'a file') then begin
          if chk_fil_opt in cur_block^.semantic_options then
            chk := op1 (file_chk, nil, arg1);
          make_unary_op (iostatus_op, stat_io);
          xrf_write (fileblk_xrf);
          xrf_use (arg1^.desc.base^.file_class, arg_list^.source, var_parm_ctxt);
        end
        else
          bad_call;
      end;

    fnextstatus:
      make_env_op (extstatus_op, type_int);

    fnmathstatus:
      make_env_op (mathstatus_op, stat_math);

    fnprogramstatus:
      make_env_op (prgmstatus_op, stat_program);

    fnspecialstatus:
      make_env_op (spclstatus_op, stat_special);

    fnexiostatus:
      make_env_op (exiostatus_op, stat_io);

    fndate:
      make_env_op (date_op, nil);

    fntime:
      make_env_op (time_op, type_int);

    fnruntime:
      make_env_op (runtime_op, type_int);

    fnmasked:
      begin
	arg1 := cond_semantics (arg_list, false, true);
	make_unary_op (masked_op, type_bool);
      end;

    fnpending:
      begin
	arg1 := cond_semantics (arg_list, false, true);
	make_unary_op (pending_op, type_bool);
      end;

    fnlength:
      if chk_arg_type (arg1,a1,str_kinds,'a string') then
        with arg1^.desc do
          if kind = chars then
            std_fcall := cst_expr (cst_scalar(1),type_int)
          else if (str_kind = varying) or str_flex then
            make_unary_op (length_op,type_int)
          else
            std_fcall := cst_expr (cst_scalar(str_length),type_int)
      else
        bad_call;

    fnlowercase:
      if chk_arg_type (arg1,a1,str_kinds,'a string') then begin
        make_unary_op (lwc_op, nil);
        std_fcall^.desc := arg1^.desc;
      end
      else
        bad_call;

    fnuppercase:
      if chk_arg_type (arg1,a1,str_kinds,'a string') then begin
        make_unary_op (upc_op, nil);
        std_fcall^.desc := arg1^.desc;
      end
      else
        bad_call;

    fnsubstr:
      if chk_arg_type (arg1,a1,str_kinds,'a string') and
        chk_arg_type (arg2,a2,[ints],'an integer') and
          ( (arg3 = nil) orif chk_arg_type (arg3,a3,[ints],'an integer') ) then
            std_fcall := new_substr ( arg1, arg2, arg3, true, a2 )
      else
        make_str_op (substr_ref, nil);

    fnindex:
      if chk_arg_type (arg1,a1,str_kinds,'a string') and
        chk_arg_type (arg2,a2,str_kinds,'a string') and
          ( (arg3 = nil) orif chk_arg_type (arg3,a3,[ints],'an integer') ) then
            make_str_op (index_op,type_int)
      else
        make_str_op (index_op,nil);

    fnverify:
      if chk_arg_type (arg1,a1,str_kinds,'a string') and
        check_chars (arg2,a2) and
          ( (arg3 = nil) orif chk_arg_type (arg3,a3,[ints],'an integer') ) then
            make_str_op (verify_op,type_int)
      else
        make_str_op (verify_op,nil);

    fnsearch:
      if chk_arg_type (arg1,a1,str_kinds,'a string') and
        check_chars (arg2,a2) and
          ( (arg3 = nil) orif chk_arg_type (arg3,a3,[ints],'an integer') ) then
            make_str_op (search_op,type_int)
      else
        make_str_op (search_op,nil);

    fnlowerbound:
      make_bound_op (lwb_op, 'LOWERBOUND');

    fnupperbound:
      make_bound_op (upb_op, 'UPPERBOUND');

    fndimension:
      make_bound_op (dim_op, 'DIMENSION');

    fnsize:
      make_size_op;

    fnextent:
      if chk_arg_type (arg1, a1, [files, pointers], 'a file or pointer') then
        if arg1^.desc.kind = files then
          if arg1^.desc.base^.file_kind <> textfile then begin
            if chk_fil_opt in cur_block^.semantic_options then
              chk := op1 (file_chk, nil, arg1);
            make_unary_op (filesize_op, type_int);
            xrf_write (fileblk_xrf);
            xrf_use (arg1^.desc.base^.file_class, a1^.source, value_ctxt);
          end
          else begin
            err_node (err_extent_textfile, a1);
            bad_call;
          end
        else
          make_unary_op (extent_op, type_int)
      else
        bad_call;

    fnaddress:  begin
      if not (sp_coe_opt in cur_block^.semantic_options) then begin
        err_node (err_addr_nospecial, name_node);
        cur_block^.semantic_options := cur_block^.semantic_options + [sp_coe_opt];
      end;
      arg1 := ref_semantics (arg_list, ref_ctxt);
      if arg1^.desc.kind = unknown_type then
        bad_call
      else if not (arg1^.opcode in [first_data_ref..last_data_ref]) then begin
        err_node (err_addr_ref, arg_list);
        bad_call;
      end
      else if pcomponent (arg1) then begin
        err_node (err_addr_packable,arg_list);
        bad_call;
      end
      else
        make_unary_op (addr_op,type_ptr);
    end;

    fnxfer:  begin
      arg1 := val_semantics (arg_list^.next);
      arg2 := ref_semantics (arg_list, ref_ctxt);
      if (arg2^.desc.kind in scl_kinds) andif
         chk_arg_type (arg1,arg_list^.next,[ints],'an integer') then begin
	if chk_val_opt in cur_block^.semantic_options then
	  chk_scalar (arg2^.desc.base, arg1, arg_list^.next);
        make_unary_op (sclcvt_op, arg2^.desc.base);
      end
      else if (arg2^.desc.kind = pointers) andif
              chk_arg_type (arg1,arg_list^.next,[ints,pointers],'an integer or pointer') then begin
        if not (sp_coe_opt in cur_block^.semantic_options) then begin
          err_node (err_ptr_cvt, name_node);
          cur_block^.semantic_options := cur_block^.semantic_options + [sp_coe_opt];
        end;
        make_unary_op (sclcvt_op,arg2^.desc.base);
      end
      else
        bad_call;
    end

  end (* case opcode *);
  xrf_write (end_xrf);
end (* std_fcall *).
$QM‰