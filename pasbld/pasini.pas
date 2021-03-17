$TITLE PASINI.PAS, last modified 5/11/84, zw
program pasini options special(word);
(*TYM-Pascal compiler symbol table and global data initialization*)
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM ptmcon.inc
$SYSTEM pasfil.inc
$SYSTEM pascfm.inc
$SYSTEM paserr.typ
$SYSTEM pasist.inc
$SYSTEM paslog.inc
$SYSTEM paspt.typ
$SYSTEM pasutl.inc
$SYSTEM pasval.inc
$SYSTEM passpf.nam
$SYSTEM pastal.inc
$SYSTEM pasenv.inc
$SYSTEM pasopd.inc
$SYSTEM ptmimd.typ
$SYSTEM ptmimd.inc
const
    no_val: val = ( no_value );
$PAGE init_log_record
procedure init_log_record;
begin
  (* preset those fields whose values never change *)
  with log_record do begin
    ki10 := false; (* ki_code_opt *)
    kl10 := true;
    opt_double := false;
    opt_progress := false;
    opt_virtual := false
  end
end;
$PAGE compiler_symbol
function compiler_symbol (
	            skind:   sym_kind;                  (* symbol kind *)
	            sname:   line_string;               (* external name or '' *)
	            tp_desc: typ;                       (* object data type or nil *)
	            sval:    val                        (* value record *)
		         ): sym;
begin
  compiler_symbol := new_sym(skind);
  with compiler_symbol^ do begin
    if sname <> '' then begin
      name := entername(sname);
      name^.scopechain := compiler_symbol
    end;
    block := root_block;	(* block the name appears in *)
    type_desc := tp_desc;
    alloc_type (tp_desc);               (* to insure all types in std table are mapped *)
    if skind in [consts,values,vars, conditions] then begin
      if skind in [vars, conditions] then
        dcl_class := external_sc
      else
	dcl_class := constant_sc;
      public_dcl := false;
      init_value := sval
    end
  end;
  if skind = types then
    chain_sym (root_block^.type_list, compiler_symbol)	(* put on block's chain of type ids *)
  else if skind in [consts, std_procs, std_funcs] then
    chain_sym (root_block^.id_list, compiler_symbol) (* put on block's chain of other names *)
end;
$PAGE prtypn
public function prtyp0 ( return_type: typ ): typ;
  begin
    if return_type = nil
      then prtyp0 := new_proc (procs, 0)
      else prtyp0 := new_proc (funcs, 0);
    prtyp0^.return_type := return_type;
    alloc_type (prtyp0)  (* redundant but safe *)
  end;
public function prtyp1 ( pkind: sym_kind; ptype: typ; return_type: typ ): typ;
  begin
    if return_type = nil
      then prtyp1 := new_proc (procs, 1)
      else prtyp1 := new_proc (funcs, 1);
    prtyp1^.return_type := return_type;
    with prtyp1^.params[1] do begin
      parm_kind := pkind;
      parm_type := ptype
    end;
    alloc_type (prtyp1)  (* redundant but safe *)
  end;
public function prtyp2 ( pkind1, pkind2: sym_kind;
                         ptype1, ptype2, return_type: typ ): typ;
  begin
    if return_type = nil
      then prtyp2 := new_proc (procs, 2)
      else prtyp2 := new_proc (funcs, 2);
    prtyp2^.return_type := return_type;
    with prtyp2^.params[1] do begin
      parm_kind := pkind1;
      parm_type := ptype1
    end;
    with prtyp2^.params[2] do begin
      parm_kind := pkind2;
      parm_type := ptype2
    end;
    alloc_type (prtyp2)  (* redundant but safe *)
  end;
$PAGE init_default_options
(*  INIT DEFAULT OPTIONS sets the DefaultOptions record to its initial default value.  *)
procedure init_default_options;
const initial_options: command_options =
     (  [ minimum(checklist)..maximum(checklist),
          trace_opt, qblocks_opt ], (* semantic options *)
        true, (* banner_opt *)
        true, (* code_opt *)
        false, (* debug_opt *)
        false, (* errors_opt *)
        true, (* finish_opt *)
        false, (* global_opt *)
        false, (* lsys_opt *)
	false, (* mainseg_opt *)
	true, (* masking_opt *)
        false, (* names_opt *)
        false, (* overlay_opt *)
        opt_is_auto, (* quick_opt *)
        opt_is_auto, (* source_opt *)
        false, (* standard_opt *)
        false, (* statistics_opt *)
        false, (* terse_opt *)
	true, (* underflow_opt *)
        45, (* page_length *)
        102, (* page_width *)
        0, (* alloc_mode *)
        nil, (* switches *)
        nil, (* dump_switches *)
	nil, (* search_list *)
        2048,    (* storage *)
	false ); (* ki_code_opt *)
begin
  default_options := initial_options
end;
$PAGE init_boolean
procedure init_boolean;
begin
  type_bool := new_type (bools);
  with type_bool^ do begin
    base_type := type_bool;
    minval := 0;
    maxval := 1;
    type_id := compiler_symbol (types, 'BOOLEAN', type_bool, no_val);
    chain_sym ( cst_list,
		compiler_symbol (consts, 'FALSE', type_bool, cst_scalar(0)) );
    chain_sym ( cst_list,
		compiler_symbol (consts, 'TRUE',  type_bool, cst_scalar(1)) )
  end
end;
$PAGE init_integer
procedure init_integer;
begin
  type_int := new_type(ints);
  with type_int^ do begin
    base_type := type_int;
    minval := -(2**31);
    maxval := 2**31 - 1
  end;
  type_int^.type_id := compiler_symbol (types, 'INTEGER', type_int, no_val);
  type_fullword := dcl_int (   type_int^.minval * (2 ** (mword_size - 32)),
			    - (type_int^.minval * (2 ** (mword_size - 32)) + 1));
  type_fullword^.type_id := compiler_symbol (types, 'MACHINE_WORD', type_fullword, no_val);
  type_non_neg := dcl_int (0, type_int^.maxval);
  alloc_type (type_non_neg)  (* compiler_symbol isn't used, so must do it here *)
end;
$PAGE init_real
procedure init_real;
begin
  type_real := dcl_real (-max_real, +max_real, srealprec);
  type_real^.type_id := compiler_symbol (types, 'REAL', type_real, no_val)
end;
$PAGE init_char
procedure init_char;
begin
  type_char := new_type(chars);
  with type_char^ do begin
    base_type := type_char;
    minval := 0;
    maxval := max_char
  end;
  type_char^.type_id := compiler_symbol (types, 'CHAR', type_char, no_val)
end;
$PAGE init_ptr
procedure init_ptr;
begin
  type_ptr := dcl_pointer (nil (* target type *));
  type_ptr^.type_id := compiler_symbol (types, 'PTR', type_ptr, no_val)
end;
$PAGE init_io_options
procedure init_io_options;
begin
  type_options := new_type (scalars);
  with type_options^ do begin
    base_type := type_options;
    minval := 0;
    maxval := 5;
    type_id := compiler_symbol (types, 'IO_OPTIONS', type_options, no_val);
    chain_sym (cst_list, compiler_symbol (consts, 'ASCII',    type_options, cst_scalar (0)));
    chain_sym (cst_list, compiler_symbol (consts, 'IMAGE',    type_options, cst_scalar (1)));
    chain_sym (cst_list, compiler_symbol (consts, 'CONTROL',  type_options, cst_scalar (2)));
    chain_sym (cst_list, compiler_symbol (consts, 'PRESERVE', type_options, cst_scalar (3)));
    chain_sym (cst_list, compiler_symbol (consts, 'SEEKOK',   type_options, cst_scalar (4)));
    chain_sym (cst_list, compiler_symbol (consts, 'RETRY',    type_options, cst_scalar (5)));
  end
end;
$PAGE init_status_types
procedure init_status_types;
begin
  stat_io := new_type (scalars);
  with stat_io^ do begin
    base_type := stat_io;
    minval := 0;
    maxval := 0;
    type_id := compiler_symbol (types, 'IO_STATUS', stat_io, no_val);
  end;
  stat_math := new_type (scalars);
  with stat_math^ do begin
    base_type := stat_math;
    minval := 0;
    maxval := 0;
    type_id := compiler_symbol (types, 'MATH_STATUS', stat_math, no_val);
  end;
  stat_program := new_type (scalars);
  with stat_program^ do begin
    base_type := stat_program;
    minval := 0;
    maxval := 0;
    type_id := compiler_symbol (types, 'PROGRAM_STATUS', stat_program, no_val);
  end;
  stat_special := new_type (scalars);
  with stat_special^ do begin
    base_type := stat_special;
    minval := 0;
    maxval := 0;
    type_id := compiler_symbol (types, 'SPECIAL_STATUS', stat_special, no_val);
  end;
  dcl_sconsts;
end;
$PAGE stt_constant
public procedure stt_constant (stt_name: line_string;
			       stt_type: typ;
			       stt_code: integer);
begin
  stt_type^.maxval := max (stt_type^.maxval, stt_code);
  chain_sym (stt_type^.cst_list,
	     compiler_symbol (consts, stt_name, stt_type, cst_scalar (stt_code)));
end;
$PAGE init_conditions
procedure init_conditions;
var new_sym: sym;
    cond: condtype;
const
    internal_names: condnames =
     (	'MATH_ERROR', 'IO_ERROR', 'PROGRAM_ERROR', 'ATTENTION',
	'STORAGE_OVERFLOW', 'STACK_OVERFLOW', 'SPECIAL_ERROR'  );
begin
  for cond := minimum (cond) to maximum (cond) do begin
    new_sym := compiler_symbol (conditions, internal_names[cond], nil,
				cst_string (condnamextern[cond]));
    new_sym^.standard := true;
    new_sym^.maskable := (cond = cnattention);
  end;
end (* init_conditions *);
$PAGE predef
public procedure predef ( int_name, ext_name: line_string; kind: sym_kind; sym_type: typ );
var new_sym: sym;
begin
  new_sym := compiler_symbol (consts, int_name, sym_type, cst_string (ext_name));
  new_sym^.kind := kind;
  new_sym^.dcl_class := external_sc
end;
$PAGE init_predef
procedure init_predef;
var type_file_name: typ;
    type_any_file: typ;
begin
  type_file_name := dcl_string (fnamsize, varying, false);
  type_file_name^.type_id := compiler_symbol (types, 'FILE_NAME', type_file_name, no_val);
  type_any_file := dcl_file (anyfile, nil);
  alloc_type (type_any_file);
  predef ('FILENAME', fnamextern, consts, prtyp1 (values, type_any_file, type_file_name));
  predef ('TRACE', trnamextern, consts, prtyp0 (nil));
  sys_ppf (* define system-dependent routines *)
end;
$PAGE init_misc
procedure init_misc;
var nil_sym: sym;
begin
  cdatesym := compiler_symbol (consts, 'COMPDATE', dcl_string (9, nonvarying, false),
			       cst_string ('DD-MMM-YY') );
  ctimesym := compiler_symbol (consts, 'COMPTIME', dcl_string (8, nonvarying, false),
			       cst_string ('HH:MM:SS') );
  nil_sym := compiler_symbol (consts, 'NIL', type_ptr, cst_nil);
  nil_sym := compiler_symbol (consts, 'NILF', dcl_file (anyfile(*file mode*), nil(*component type*)),
			      cst_nil)
end;
$PAGE init_std_pf
procedure init_std_pf;
var
  std_pf: std_pr_fun;
  nsym: sym;
begin
  for std_pf := minimum(std_proc) to maximum(std_proc) do
    if std_pf_id [std_pf] <> '' then begin
      nsym := compiler_symbol (std_procs, std_pf_id[std_pf], nil, no_val);
      nsym^.std_pf_code := std_pf
    end;
  for std_pf := minimum(std_func) to maximum(std_func) do
    if std_pf_id [std_pf] <> '' then begin
      nsym := compiler_symbol (std_funcs, std_pf_id[std_pf], nil, no_val);
      nsym^.std_pf_code := std_pf
    end
end;
$PAGE init_std_files
procedure init_std_files;
begin
  type_text := dcl_file (textfile (*file mode*), type_char (*component type*));
  with type_text^ do begin
    type_id := compiler_symbol (types, 'TEXT', type_text, no_val);
    packable := true
  end;
  file_tty := compiler_symbol (vars, 'TTY', type_text, no_val);
  chain_sym (root_block^.id_list, file_tty);
  filettyoutput := compiler_symbol (vars, 'TTYOUTPUT', type_text, no_val);
  chain_sym (root_block^.id_list, filettyoutput);
  file_input := compiler_symbol (vars, 'INPUT', type_text, no_val);
  chain_sym (root_block^.id_list, file_input);
  file_output := compiler_symbol (vars, 'OUTPUT', type_text, no_val);
  chain_sym (root_block^.id_list, file_output);
end;
$PAGE init_optimizer_symbols
procedure init_optimizer_symbols;
begin
  (*  Create the file class of type TEXT  *)
  type_text^.file_class := compiler_symbol (vars, '', type_text, no_val);
  with type_text^.file_class^ do begin
    dcl_class := fileblk_sc;
    public_dcl := true
  end;
  file_chain := type_text^.file_class;
  (*  Create the input/output optimization symbol  *)
  io_opsym := compiler_symbol (vars, '<I/O>', nil, no_val);
  io_opsym^.dcl_class := opt_sc
end;
$PAGE pasini
begin
  rewrite (tty);		(* in case of error message from WRPAS *)
  tm_constants;
  init_log_record;
  init_default_options;
  sym_vl_number := 0; (* Initialize the symbol numbering. *)
  sym_nvl_number := 0;
  vl_list := nil;
  root_name := nil;		(* establish the name table ... *)
  root_block := new_blk(root_blk,nil);		(* ... and the symbol table *)
  cur_block := root_block;
  tal_init; (* Initialize the type allocator. *)
  init_boolean;
  init_integer;
  init_real;
  init_char;
  init_ptr;
  init_io_options;
  init_status_types;
  init_conditions;
  init_predef;
  init_misc;
  init_std_pf;
  init_std_files;
  init_optimizer_symbols;
  env_name := nil;
  vl_base := sym_vl_number; (* Save the symbol numbering bases. *)
  nvl_base := sym_nvl_number;
  vll_base := vl_list;
  cmd_name_stack := nil;
  cmd_list := nil;
  last_cmd := nil;
  IF NOT wrpas(tmprefix || 'INI.ENV') THEN STOP
end.
    