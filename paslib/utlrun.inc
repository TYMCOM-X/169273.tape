(*utlrun.inc, last modified 5/10/83, zw*)
$IFNOT utlruninc

$SYSTEM UTLWRT.TYP
$SYSTEM UTLIO.TYP
$INCLUDE UTLRUN.TYP

EXTERNAL FUNCTION run(nam : fil_nam; prmpt_flg : yes_or_no) : succeed_or_fail;
  (*run specified program, look for tmp file if not prompt*)

EXTERNAL PROCEDURE run_cln_up;
  (*clean up run utility*)

EXTERNAL PROCEDURE run_set_up;
  (*set up run utility*)

$ENABLE utlruninc
$ENDIF
(*end of utlrun.inc*)
    