(*utlwrt.inc, last modified 5/5/83, zw*)
$IFNOT utlwrtinc

$INCLUDE UTLWRT.TYP

EXTERNAL PROCEDURE inc(VAR i : int);
  (*increment an integer*)

EXTERNAL PROCEDURE dec(VAR i : int);
  (*decrement an integer*)

EXTERNAL FUNCTION cnt_cln(VAR cnt_set_cln : int) : bln;
  (*decrement for clean up, true if gt zero*)

EXTERNAL FUNCTION cnt_set(VAR cnt_set_cln : int) : bln;
  (*increment for set up, true if gt one*)

EXTERNAL PROCEDURE barf(msg : str);
  (*un-graceful program exit*)

EXTERNAL FUNCTION str_len(s : str) : pos_int;
  (*return practical length of string, ignor trailing spaces*)

EXTERNAL PROCEDURE jfy_str(VAR s : str);
  (*right justify a string*)

EXTERNAL FUNCTION cpy_str(s : str) : ^str;
  (*copy a string*)

EXTERNAL PROCEDURE del_str(VAR s : ^str);
  (*delete a string*)

EXTERNAL PROCEDURE set_lin(VAR lin : str; VAR csr : str_csr; s : str);
  (*set string to line, init cursor*)

EXTERNAL PROCEDURE clr_lin(VAR lin : str; VAR csr : str_csr);
  (*clear command line*)

EXTERNAL FUNCTION chk_eol(lin : str; csr : str_csr) : yes_or_no;
  (*check for end of line*)

EXTERNAL FUNCTION chk_chr(lin : str; csr : str_csr; chrs : chr_set) : yes_or_no;
  (*check if next char in line is in specified set*)

EXTERNAL PROCEDURE del_txt(VAR txt_ptr : ^txt);
  (*delete text, a list of lines*)

EXTERNAL FUNCTION str_int(s : str) : int;
  (*convert a string to an integer*)

EXTERNAL FUNCTION int_str(i : int) : STRING [15];
  (*convert an integer to a string*)

EXTERNAL PROCEDURE wrt_cln_up;
  (*clean up wart utility*)

EXTERNAL PROCEDURE wrt_set_up;
  (*set up wart utility*)

$ENABLE utlwrtinc
$ENDIF
(*end of utlwrt.inc*)
   