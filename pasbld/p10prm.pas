$TITLE P10PRM -- PDP-10 Parameter Tests

module p10prm;

$HEADER ptmprm.hdr
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$PAGE p_b_address
(*  P B ADDRESS determines if all parameters of a specified type must be passed
    by address instead of by value.  *)

public function p_b_address ( parm_type: typ ): boolean;
 begin
  p_b_address :=
    (parm_type^.kind in [arrays, records, strings]) or
    (parm_type^.size > 72) or
    (parm_type^.flexible);
 end;
$PAGE passed_by_address
(* PASSED BY ADDRESS determines if a parameter is one which is to be passed by
   address instead of by value. *)

public function passed_by_address ( parm: sym ): boolean;
 begin
  passed_by_address :=
    ((parm^.kind = vars) and (parm <> parm^.block^.return_sym)) or
    p_b_address (parm^.type_desc);
 end.
    