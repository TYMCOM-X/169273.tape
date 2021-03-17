$TITLE PASSMO - set semantic options
$LENGTH 42

module passmo;
$PAGE includes

$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE paserr.inc
$INCLUDE passw.inc
$INCLUDE pasutl.inc

$INCLUDE pasopd.inc
$PAGE sem_check_options

(*  SEM CHECK OPTIONS is called with the sub-options list for a CHECK option,
    and the sign of the option.  It updates the current semantic options.  *)

procedure sem_check_options ( nodes: parse_node; subr_block: blk; positive: boolean );

var
  sub_opt: parse_node;
  idx: integer;
  opt_code: integer;
  opt_set: set of checklist;

begin
  if nodes = nil then
    opt_set := [ minimum(checklist)..maximum(checklist) ]
  else begin
    opt_set := [];
    sub_opt := nodes;
    while sub_opt <> nil do begin
      idx := 1;
      if cmd_lookup (sub_opt^.name^.text, idx, ['A'..'Z'], opdcot_chk_opt_table, opt_code)
        then opt_set := opt_set + [opdmtc_map_to_chk_opt [chk_opts (opt_code)]]
        else err_node (err_chk_sub_option, sub_opt);
      sub_opt := sub_opt^.next;
    end;
  end;
  with subr_block^ do
    if positive then
      semantic_options := semantic_options -
                          [ minimum(checklist)..maximum(checklist) ] +
                          opt_set
    else
      semantic_options := semantic_options - opt_set;
 end;
$PAGE sem_special_options

(*  SEM SPECIAL OPTIONS is called with the sub-options list for a CHECK option,
    and the sign of the option.  It updates the current semantic options.  *)

procedure sem_special_options ( nodes: parse_node; subr_block: blk; positive: boolean );

var
  sub_opt: parse_node;
  idx: integer;
  opt_code: integer;
  opt_set: set of speciallist;

begin
  if nodes = nil then
    opt_set := [ minimum(speciallist)..maximum(speciallist) ]
  else begin
    opt_set := [];
    sub_opt := nodes;
    while sub_opt <> nil do begin
      idx := 1;
      if cmd_lookup (sub_opt^.name^.text, idx, ['A'..'Z'], opdsot_sp_opt_table, opt_code)
        then opt_set := opt_set + [opdmts_map_to_sp_opt [sp_opts (opt_code)]]
        else err_node (err_sp_sub_option, sub_opt);
      sub_opt := sub_opt^.next;
    end;
  end;
  with subr_block^ do begin
    if positive
      then semantic_options := semantic_options + opt_set
      else semantic_options := semantic_options - opt_set;
  end;
 end;
$PAGE set_sem_options

(* SET_SEM_OPTIONS sets a single semantic option for a subroutine as
   represented by its block and type nodes.  If the block node pointer
   is nil, the subroutine is assumed to be external, determining 
   whether or not certain options are applicable.       *)

public function set_sem_options
  ( opt_node: parse_node;   (* links to name of option word *)
    subr_type: typ; (* type node of subroutine *)
    subr_block: blk (* block node of same *)
    ): boolean;     (* if false, code is set *)

  var
    dump_id: parse_node; (* for chain of dump identifiers *)
    positive: boolean;
    opt_ix: integer;
    option_ix: option_scalar;   (* for lookup *)
    ix: integer;     (* dummy index for lookup_options *)
    sw: switch_ptr;

begin
  set_sem_options := false;     (* assume false *)
  with opt_node^.name^ do begin
    positive := not ( (length (text) >= 2) andif (substr (text, 1, 2) = 'NO') );
    if positive
      then ix := 1
      else ix := 3;
    if not cmd_lookup (text, ix, ['A'..'Z'], opdotb_option_table, opt_ix) then begin
      err_node (err_bad_option, opt_node);
      return;
    end;
  end;

  option_ix := option_scalar (opt_ix);

  if not positive and not (option_ix in opdnoo_no_options) then begin
    err_node (err_sem_opt_bad, opt_node);
    return;
  end;

  if not (option_ix in opdblo_block_options) then begin
    err_node (err_sem_opt_bad, opt_node);
    return;
  end;

  if option_ix = opt_fortran then begin
    if subr_block <> nil then begin
      err_node (err_int_opt_bad, opt_node);
      return;
    end
  end
  else begin
    if subr_block = nil then begin
      err_node (err_ext_opt_bad, opt_node);
      return;
    end;
  end;

  (* process option *)

  case option_ix of

    opt_fortran:
      subr_type^.fortran_call := true;

    opt_storage:
      begin
        if subr_block^.kind <> program_blk then begin
          err_node (err_prog_option, opt_node);
          return;
        end;
        if (opt_node^.defn = nil) orif (opt_node^.defn^.sym <> intconst) then begin
          err_node (err_number_expected, opt_node);
          return;
        end;
        prog_options.storage := opt_node^.defn^.value.ival
      end;

    opt_alloc:
      begin
        if subr_block^.kind <> program_blk then begin
          err_node (err_prog_option, opt_node);
          return;
        end;
        if (opt_node^.defn = nil) orif (opt_node^.defn^.sym <> intconst) then begin
          err_node (err_number_expected, opt_node);
          return;
        end;
        if opt_node^.defn^.value.ival > 99 then begin
          err_node (err_alloc_mode, opt_node);
          return;
        end;
        prog_options.alloc_mode := opt_node^.defn^.value.ival;
      end;

    opt_overlay:
      begin
        if subr_block^.kind <> module_blk then begin
          err_node (err_ovl_module, opt_node);
          return;
        end;
	if positive and prog_options.mainseg_opt then begin
	  err_node (err_ovl_mainseg, opt_node);
	  return;
	end;
        prog_options.overlay_opt := positive;
      end;

    opt_mainseg:
      begin
	if positive and prog_options.overlay_opt then begin
	  err_node (err_ovl_mainseg, opt_node);
	  prog_options.overlay_opt := false;
	end;
	prog_options.mainseg_opt := true;
      end;

    opt_debug:
      begin
        if not (subr_block^.kind in [program_blk, module_blk]) then begin
          err_node (err_deb_module, opt_node);
          return;
        end;
        prog_options.debug_opt := positive;
        if positive and (optimize_opt in subr_block^.semantic_options) then begin
          err_node (err_deb_optimize, opt_node);
          subr_block^.semantic_options := subr_block^.semantic_options - [optimize_opt];
          return;
        end;
      end;

    opt_masking:
      begin
	if subr_block^.kind <> program_blk then begin
	  err_node (err_prog_option, opt_node);
	  return;
	end;
	prog_options.masking_opt := positive;
      end;

    opt_underflow:
      begin
	if subr_block^.kind <> program_blk then begin
	  err_node (err_prog_option, opt_node);
	  return;
	end;
	prog_options.underflow_opt := positive;
      end;

    opt_global:
      begin
        if not (subr_block^.kind in [program_blk, module_blk, data_blk]) then begin
          err_node (err_prog_option, opt_node);
          return;
        end;
        prog_options.global_opt := positive;
      end;

    opt_standard:
      err_node (err_standard, opt_node);

    opt_check:
      sem_check_options (opt_node^.defn, subr_block, positive);

    opt_trace..opt_optimize:
      begin
        if (option_ix = opt_optimize) and positive and prog_options.debug_opt then begin
          err_node (err_deb_optimize, opt_node);
          return;
        end;
        with subr_block^ do
          if positive
            then semantic_options := semantic_options + [opdmto_map_to_optionlist [option_ix]]
            else semantic_options := semantic_options - [opdmto_map_to_optionlist [option_ix]];
      end;

    opt_special:
      sem_special_options (opt_node^.defn, subr_block, positive);

    opt_dump:
      begin
        dump_id := opt_node^.defn;      (* pointer to first dump parameter *)
        while dump_id <> nil do begin
          subr_block^.dump_switches :=
            enable_switch (subr_block^.dump_switches, dump_id^.name^.text, positive);
          dump_id := dump_id^.next;
        end;
      end
  end (* case *) ;
  set_sem_options := true;  (* no error *) 
end.
 