(*utlscn.inc, last modified 5/5/83, zw*)
$IFNOT utlscninc

$SYSTEM UTLWRT.TYP
$SYSTEM UTLIO.TYP
$INCLUDE UTLSCN.TYP

EXTERNAL PROCEDURE scn_spcs(lin : str; VAR csr : str_csr);
  (*advance cursor to first non-space character in line, if any*)

EXTERNAL FUNCTION scn_rdy(lin : str; VAR csr : str_csr) : yes_or_no;
  (*determine if line is ready to be scanned, not end of line*)

EXTERNAL FUNCTION scn_len(lin : str; csr : str_csr; chrs : chr_set) : pos_int;
  (*return length of sub-string of lin which contains chrs from set*)

EXTERNAL FUNCTION scn_chr(lin : str; VAR csr : str_csr;
  ch : chr) : succeed_or_fail;
  (*try to scan a chr from the line*)

EXTERNAL FUNCTION scn_str(lin : str; VAR csr : str_csr;
  VAR str_val : ^str) : succeed_or_fail;
  (*try to scan a string from the given line*)

EXTERNAL FUNCTION scn_wrd(lin : str; VAR csr : str_csr;
  wrd_chrs : chr_set; VAR wrd_val : ^str) : succeed_or_fail;
  (*try to scan a word of the given chr set from the line*)

EXTERNAL FUNCTION scn_key(lin : str; VAR csr : str_csr;
  key_wrd : key; key_len : key_idx) : succeed_or_fail;
  (*try to scan a key word from line*)

EXTERNAL FUNCTION scn_cmd(lin : str; VAR csr : str_csr;
  cmd_wrd : key; cmd_abrv : key_idx) : succeed_or_fail;
  (*try to scan abreviated command word from line*)

EXTERNAL FUNCTION scn_nam(lin : str; VAR csr : str_csr;
  VAR nam_val : str) : succeed_or_fail;
  (*try to scan a name from the line, wildcards allowed*)

EXTERNAL FUNCTION scn_fil(lin : str; VAR csr : str_csr;
  VAR fil_val : fil_nam) : succeed_or_fail;
  (*try to scan a file name from the line, wildcards allowed*)

EXTERNAL FUNCTION scn_int(lin : str; VAR csr : str_csr;
  VAR int_val : int) : succeed_or_fail;
  (*try to scan an integer from the line*)

EXTERNAL FUNCTION scn_swt(lin : str; VAR csr : str_csr;
  swt_wrd : key; swt_abrv : key_idx) : succeed_or_fail;
  (*try to scan a switch from line*)

EXTERNAL FUNCTION scn_ifl(lin : str; VAR csr : str_csr;
  VAR ifl_val : fil_nam) : succeed_or_fail;
  (*try to scan an indirect file from line*)

EXTERNAL PROCEDURE scn_cln_up;
  (*clean up scanner utility*)

EXTERNAL PROCEDURE scn_set_up;
  (*set up scanner utility*)

$ENABLE utlscninc
$ENDIF
(*end of utlscn.inc*)

