$PAGE PASOPD.TYP, last modified 5/11/84, zw

TYPE
option_scalar = ((* CMD  BLK  NO  *)
opt_terse,	(*   X            *)
opt_verbose,	(*   X            *)
opt_length,	(*   X            *)
opt_width,	(*   X            *)
opt_enable,	(*   X            *)
opt_disable,	(*   X            *)
opt_quick,	(*   X         X  *)
opt_statistics,	(*   X         X  *)
opt_names,	(*   X         X  *)
opt_code,	(*   X         X  *)
opt_finish,	(*   X         X  *)
opt_source,	(*   X         X  *)
opt_search,	(*   X         X  *)
opt_lsystem,	(*   X         X  *)
opt_errors,	(*   X         X  *)
opt_banner,	(*   X         X  *)
opt_kicode,	(*   X         X  *)
opt_check,	(*   X    X    X  *)
opt_trace,	(*   X    X    X  *)
opt_qblocks,	(*   X    X    X  *)
opt_map,	(*   X    X    X  *)
opt_symbols,	(*   X    X    X  *)
opt_calls,	(*   X    X    X  *)
opt_assembly,	(*   X    X    X  *)
opt_xref,	(*   X    X    X  *)
opt_optimize,	(*   X    X    X  *)
opt_special,	(*   X    X    X  *)
opt_overlay,	(*   X    X    X  *)
opt_mainseg,	(*   X    X    X  *)
opt_debug,	(*   X    X    X  *)
opt_underflow,	(*   X    X    X  *)
opt_masking,	(*   X    X    X  *)
opt_global,	(*   X    X    X  *)
opt_standard,	(*   X    X    X  *)
opt_dump,	(*   X    X    X  *)
opt_alloc,	(*   X    X       *)
opt_storage,	(*   X    X       *)
opt_fortran,	(*        X       *)
opt_run,	(*                *)
opt_runoffset,	(*                *)
opt_help,	(*                *)
opt_exit );	(*                *)

TYPE
options_set = SET OF option_scalar;
(*  OPTIONLIST/OPTION_SCALAR conversion types.  *)
in_options_set = ARRAY [opt_trace..opt_optimize] OF optionlist;
to_options_set = ARRAY [optionlist] OF option_scalar;
(*  CHECK suboptions.  *)
chk_opts = ( opt_chk_ass, opt_chk_cas, opt_chk_com, opt_chk_fld, opt_chk_fil,
  opt_chk_inp, opt_chk_poi, opt_chk_str, opt_chk_sub, opt_chk_val, opt_chk_stk
  );
checklist = chk_ass_opt .. chk_stk_opt;
in_chk_opt_set = ARRAY [chk_opts] OF checklist;
to_chk_opt_set = ARRAY [checklist] OF chk_opts;
(*  SPECIAL suboptions.  *)
sp_opts = ( opt_sp_coe, opt_sp_ptr, opt_sp_wor );
speciallist = sp_coe_opt .. sp_wor_opt;
in_sp_opt_set = ARRAY [sp_opts] OF speciallist;
to_sp_opt_set = ARRAY [speciallist] OF sp_opts;
(*  Immediate commands.  *)
imd_commands = ( imd_environment, imd_target );
(*  Option name list types for LOOKUP calls.  *)
op_list = ARRAY [1..ORD(MAXIMUM(option_scalar))+1] OF cmd_lookup_record;
chk_op_list = ARRAY [1..ORD(MAXIMUM(chk_opts))+1] OF cmd_lookup_record;
sp_op_list = ARRAY [1..ORD(MAXIMUM(sp_opts))+1] OF cmd_lookup_record;
imd_list = ARRAY [1..ORD(MAXIMUM(imd_commands))+1] OF cmd_lookup_record;
