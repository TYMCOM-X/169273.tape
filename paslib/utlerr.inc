(*utlerr.inc, last modified 5/5/83, zw*)
$IFNOT utlerrinc

$SYSTEM UTLWRT.TYP
$INCLUDE UTLERR.TYP

EXTERNAL EXCEPTION fatl_err;
EXTERNAL EXCEPTION err;

EXTERNAL VAR err_fil: txt_fil; (*error messages directed here*)

EXTERNAL VAR num_fatl_errs: pos_int; (*count fatl errs sgnld*)
EXTERNAL VAR num_errs: pos_int; (*count errs sgnld*)

EXTERNAL PROCEDURE sgnl_hlt;
  (*halt after current operation*)

EXTERNAL PROCEDURE sgnl_fatl_err(rtn: PROCEDURE);
  (*signal specified fatal error*)

EXTERNAL PROCEDURE sgnl_err(rtn: PROCEDURE);
  (*signal specified non-fatal error*)

EXTERNAL PROCEDURE err_trp(nam, ver: str; set_up, opr, cln_up: PROCEDURE);
  (*general purpose error trap*)

EXTERNAL PROCEDURE err_cln_up;
  (*clean up error utility*)

EXTERNAL PROCEDURE err_set_up;
  (*set up error utility*)

$ENABLE utlerrinc
$ENDIF
(*end of utlerr.inc*)
   