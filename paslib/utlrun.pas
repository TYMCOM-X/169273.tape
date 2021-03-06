$WIDTH=100
$LENGTH=55
$TITLE utlrun.pas, last modified 4/7/83, zw
MODULE utlrun;
  (*run a program*)

$SYSTEM UTLWRT
$SYSTEM UTLIO

$PAGE utlrun declarations
$INCLUDE UTLRUN.TYP

VAR cnt_set_cln : int := zero;

$PAGE run a program
EXTERNAL PROCEDURE runprg
  (fil_nam : PACKED ARRAY [1 .. *] OF char; prmpt_flg : boolean);
  (*run specified program, look for tmp file if prompt flag is false*)

PUBLIC FUNCTION run(nam : fil_nam; prmpt_flg : yes_or_no) : succeed_or_fail;
  (*run specified program, look for tmp file if not prompt*)
  BEGIN
    runprg(nam, prmpt_flg); run := fail
    END;

$PAGE set up and clean up
PUBLIC PROCEDURE run_cln_up;
  (*clean up run utility*)
  BEGIN
    IF cnt_cln(cnt_set_cln) THEN RETURN;
    wrt_cln_up
    END;

PUBLIC PROCEDURE run_set_up;
  (*set up run utility*)
  BEGIN
    IF cnt_set(cnt_set_cln) THEN RETURN;
    wrt_set_up
    END.
  