(*utlcmd.inc, last modified 5/5/83, zw*)
$IFNOT utlcmdinc

$SYSTEM UTLWRT.TYP
$SYSTEM UTLIO.TYP
$SYSTEM UTLSCN.TYP
$INCLUDE UTLCMD.TYP

EXTERNAL VAR lin: STRING [cmd_lin_len]; (*slc cmd lin*)
EXTERNAL VAR csr: 0 .. cmd_lin_len + 1; (*slc cmd lin csr*)
EXTERNAL VAR usr_cmd: cmd_rcd; (*slc cmd rcd*)

EXTERNAL FUNCTION cmd_scn(lin: str; VAR csr: str_csr; lst: cmd_lst;
  VAR cmd: cmd_rcd): succeed_or_fail;
  (*scan a command of the given list from the line*)

EXTERNAL PROCEDURE dpy_itm(VAR pos: ord_int; new_lin: yes_or_no; itm: str);
  (*display the itm at the current position if possible*)

EXTERNAL PROCEDURE dpy_cmd_lst(lst: cmd_lst);
  (*display command list*)

EXTERNAL PROCEDURE dpy_csr(lin: str; csr: str_csr);
  (*display line with pointer at cursor*)

EXTERNAL PROCEDURE cmd_err(VAR lin: str; VAR csr: str_csr; err_msg: str);
  (*display error message and cursor, clear line*)

EXTERNAL PROCEDURE ifl_cmd;
  (*process an indirect file from usr_cmd*)

EXTERNAL PROCEDURE get_lin(VAR lin: str; VAR csr: str_csr;
  prmpt, dflt: str);
  (*prompt user for input, return command line and cursor*)

EXTERNAL PROCEDURE get_cmd(VAR lin: str; VAR csr: str_csr;
  prmpt, dflt: str; lst: cmd_lst;
  VAR cmd_val: cmd_rcd);
  (*get a command of the given list from the line*)

EXTERNAL PROCEDURE slc_cmd(qst: str; lst: cmd_lst;
  VAR tkn_num: int);
  (*solicit a command*)

EXTERNAL FUNCTION slc_eol: succeed_or_fail;
  (*solicit end of line*)

EXTERNAL FUNCTION slc_lst: succeed_or_fail;
  (*solicit end or continuation of list*)

EXTERNAL FUNCTION slc_yon(qst: str; VAR yon_val: yes_or_no): succeed_or_fail;
  (*solicit yes or no*)

EXTERNAL FUNCTION slc_str(qst: str; VAR str_val: ^str): succeed_or_fail;
  (*solicit a string*)

EXTERNAL FUNCTION slc_wrd(qst: str; VAR wrd_val: ^str): succeed_or_fail;
  (*solicit a word*)

EXTERNAL FUNCTION slc_fil(qst: str; VAR fil_val: fil_nam): succeed_or_fail;
  (*solicit a file name*)

EXTERNAL FUNCTION slc_int(qst: str; VAR int_val: int): succeed_or_fail;
  (*solicit an integer*)

EXTERNAL FUNCTION slc_txt(qst: str; VAR txt_val: ^txt): succeed_or_fail;
  (*solicit lines of text*)

EXTERNAL PROCEDURE cmd_rst;
  (*reset command utility*)

EXTERNAL PROCEDURE cmd_cln_up;
  (*clean up command utility*)

EXTERNAL PROCEDURE cmd_set_up;
  (*set up command utility*)

$ENABLE utlcmdinc
$ENDIF
(*end of utlcmd.inc*)
    