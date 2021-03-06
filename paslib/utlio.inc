(*utlio.inc, last modified 5/5/83, zw*)
$IFNOT utlioinc

$SYSTEM UTLWRT.TYP
$INCLUDE UTLIO.TYP

EXTERNAL FUNCTION opn_fil(VAR fil: txt_fil; nam: fil_nam;
   opn_mod: str): succeed_or_fail;
  (*try to open a file for specified mode*)

EXTERNAL PROCEDURE cls_fil(VAR fil: txt_fil);
  (*close a file, will not close TTY:*)

EXTERNAL PROCEDURE get_buf(VAR fil: txt_fil; VAR buf: str);
  (*get a line from a file, return nul if end of file*)

EXTERNAL FUNCTION psh_fil(VAR fil: txt_fil; nam: fil_nam;
  opn_mod: str): succeed_or_fail;
  (*push file onto stack, return opened file*)

EXTERNAL PROCEDURE pop_fil(VAR fil: txt_fil);
  (*close file, return file popped from stack*)

EXTERNAL PROCEDURE get_str(VAR fil: txt_fil; qst: str; VAR lin: str);
  (*input a string from a file*)

EXTERNAL PROCEDURE put_txt(VAR fil: txt_fil; txt_ptr: ^txt);
  (*write all lines of text to specified file*)

EXTERNAL PROCEDURE io_cln_up;
  (*clean up input/output utility*)

EXTERNAL PROCEDURE io_set_up;
  (*set up input/output utility*)

$ENABLE utlioinc
$ENDIF
(*end of utlio.inc*)
   