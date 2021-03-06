(*utlwrt.typ, last modified 5/5/83, zw*)
$IFNOT utlwrttyp

CONST
  max_int = 64000; max_byt = 255;
  yes = TRUE; on = TRUE; succeed = TRUE;
  no = FALSE; off = FALSE; fail = FALSE;
  alpha = ['A' .. 'Z', 'a' .. 'z', '_'];
  numeric = ['0' .. '9'];
  punct = ['.', ',', ';', ':', '?', '!'];
  nul = ''; spc = ' '; cma = ','; bel = CHR(7); cr = CHR(13); lf = CHR(10);
  crlf = cr || lf; nu = 'Nu?' || bel; ok = 'Ok.';
  zero = 0; one = 1; two = 2; three = 3;

TYPE
  bln = BOOLEAN; TRUE_or_FALSE = bln;
  yes_or_no = bln; on_or_off = bln; succeed_or_fail = bln;
  bit = zero .. one; byt = zero .. max_byt;
  int = INTEGER; pos_int = zero .. max_int; ord_int = one .. max_int;
  chr = CHAR; chr_set = SET OF chr;
  str_csr = pos_int; str = STRING[*];
  txt = RECORD str_ptr : ^str; nxt : ^txt END;

$ENABLE utlwrttyp
$ENDIF
(*end of utlwrt.typ*)
 