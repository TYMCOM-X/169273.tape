$WIDTH=100
$LENGTH=55
$TITLE utlscn.pas, last modified 5/5/83, zw
MODULE utlscn;
  (*scan various types of tokens from a line*)

$SYSTEM UTLWRT
$SYSTEM UTLIO

$PAGE utlscn declarations
$INCLUDE UTLSCN.TYP

VAR cnt_set_cln : int := zero;

$PAGE scan spaces, ready, len, chr
PUBLIC PROCEDURE scn_spcs(lin : str; VAR csr : str_csr);
  (*advance cursor to first non-space character in line, if any*)
  BEGIN
    WHILE chk_chr(lin, csr, [spc]) DO csr := csr + one
    END;

PUBLIC FUNCTION scn_rdy(lin : str; VAR csr : str_csr) : yes_or_no;
  (*determine if line is ready to be scanned, not end of line*)
  BEGIN
    scn_spcs(lin, csr); scn_rdy := NOT chk_eol(lin, csr)
    END;

PUBLIC FUNCTION scn_len(lin : str; csr : str_csr; chrs : chr_set) : pos_int;
  (*return length of sub-string of lin which contains chrs from set*)
  BEGIN
    scn_len := zero;
    WHILE chk_chr(lin, csr + scn_len, chrs) DO scn_len := scn_len + one
    END;

PUBLIC FUNCTION scn_chr(lin : str; VAR csr : str_csr;
  ch : chr) : succeed_or_fail;
  (*try to scan a chr from the line*)
  VAR sav_csr : str_csr;
  BEGIN
    sav_csr := csr; scn_chr := scn_rdy(lin, csr);
    scn_chr := scn_chr ANDIF (scn_len(lin, csr, [ch]) > zero);
    IF scn_chr THEN inc(csr) ELSE csr := sav_csr
    END;

$PAGE scan string, word and key
PUBLIC FUNCTION scn_str(lin : str; VAR csr : str_csr;
  VAR str_val : ^str) : succeed_or_fail;
  (*try to scan a string from the given line*)
  VAR sav_csr : str_csr; len : pos_int;
  BEGIN
    sav_csr := csr; scn_str := scn_rdy(lin, csr);
    IF scn_str THEN BEGIN
      len := str_len(lin) - csr + one; csr := csr + len;
      scn_str := (len > zero)
      END;
    IF scn_str THEN str_val := cpy_str(SUBSTR(lin, (csr - len), len))
    ELSE csr := sav_csr
    END;

PUBLIC FUNCTION scn_wrd(lin : str; VAR csr : str_csr;
  wrd_chrs : chr_set; VAR wrd_val : ^str) : succeed_or_fail;
  (*try to scan a word of the given chr set from the line*)
  VAR sav_csr : str_csr; len : pos_int;
  BEGIN
    sav_csr := csr; scn_wrd := scn_rdy(lin, csr);
    IF scn_wrd THEN BEGIN
      len := scn_len(lin, csr, wrd_chrs); csr := csr + len;
      scn_wrd := (len > zero)
      END;
    IF scn_wrd THEN wrd_val := cpy_str(SUBSTR(lin, (csr - len), len))
    ELSE csr := sav_csr
    END;

PUBLIC FUNCTION scn_key(lin : str; VAR csr : str_csr;
  key_wrd : key; key_len : key_idx) : succeed_or_fail;
  (*try to scan a key word from line*)
  VAR sav_csr : str_csr;
  BEGIN
    sav_csr := csr; scn_key := scn_rdy(lin, csr);
    IF scn_key THEN BEGIN
      scn_key := NOT chk_eol(lin, (csr + key_len - one))
        ANDIF (SUBSTR(key_wrd, one, key_len) = SUBSTR(lin, csr, key_len));
      csr := csr + key_len
      END;
    IF NOT scn_key THEN csr := sav_csr
    END;

$PAGE scan command and name
PUBLIC FUNCTION scn_cmd(lin : str; VAR csr : str_csr;
  cmd_wrd : key; cmd_abrv : key_idx) : succeed_or_fail;
  (*try to scan abreviated command word from line*)
  CONST cmd_chrs = alpha;
  VAR sav_csr : str_csr; len : pos_int; wrd : ^str;
  BEGIN
    sav_csr := csr; scn_cmd := scn_rdy(lin, csr);
    scn_cmd := scn_cmd ANDIF scn_wrd(lin, csr, cmd_chrs, wrd);
    IF scn_cmd THEN BEGIN
      len := str_len(wrd^);
      scn_cmd := (len >= cmd_abrv) ANDIF (len <= str_len(cmd_wrd))
        ANDIF (UPPERCASE(wrd^) = UPPERCASE(SUBSTR(cmd_wrd, one, len)));
      Del_str(wrd)
      END;
    IF NOT scn_cmd THEN csr := sav_csr
    END;

PUBLIC FUNCTION scn_nam(lin : str; VAR csr : str_csr;
  VAR nam_val : str) : succeed_or_fail;
  (*try to scan a name from the line, wildcards allowed*)
  VAR sav_csr : str_csr; nam_ptr, tmp_ptr : ^str; idx : pos_int;
  BEGIN
    sav_csr := csr; scn_nam := scn_rdy(lin, csr);
    scn_nam := scn_nam ANDIF scn_wrd(lin, csr,
      ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_', ':', '*', '?'], nam_ptr);
    IF scn_nam THEN BEGIN
      scn_nam := (LENGTH(nam_ptr^) <= LENGTH(nam_val));
      IF NOT scn_nam THEN del_str(nam_ptr)
      ELSE IF SEARCH(nam_ptr^, ['*']) > zero THEN BEGIN
        scn_nam := (str_len(nam_ptr^) = one); del_str(nam_ptr);
        IF scn_nam THEN BEGIN
          NEW(nam_ptr, LENGTH(nam_val)); nam_ptr^ := nam_val;
          FOR idx := one TO LENGTH(nam_ptr^) DO nam_ptr^[idx] := '?'
          END
        END
      ELSE BEGIN
        tmp_ptr := nam_ptr;
        NEW(nam_ptr, LENGTH(nam_val)); nam_ptr^ := nam_val;
        FOR idx := one TO LENGTH(nam_ptr^) DO nam_ptr^[idx] := spc;
        FOR idx := one TO LENGTH(tmp_ptr^) DO nam_ptr^[idx] := tmp_ptr^[idx];
        del_str(tmp_ptr)
        END
      END;
    IF scn_nam THEN nam_val := UPPERCASE(nam_ptr^) ELSE csr := sav_csr;
    IF scn_nam THEN del_str(nam_ptr)
    END;

$PAGE scan file name and integer
PUBLIC FUNCTION scn_fil(lin : str; VAR csr : str_csr;
  VAR fil_val : fil_nam) : succeed_or_fail;
  (*try to scan a file name from the line, wildcards allowed*)
  VAR sav_csr : str_csr; nam : STRING [6]; ext : STRING [3];
  BEGIN
    sav_csr := csr; scn_fil := scn_rdy(lin, csr);
    nam := '      '; ext := '   ';
    scn_fil := scn_fil ANDIF scn_nam(lin, csr, nam);
    IF scn_fil ANDIF NOT (scn_chr(lin, csr, '.')
      ANDIF (chk_eol(lin, csr) ORIF chk_chr(lin, csr, [spc])
        ORIF scn_nam(lin, csr, ext))) THEN ext := '   ';
    IF scn_fil THEN BEGIN
      jfy_str(nam);
      IF ext <> '   ' THEN fil_val := nam || '.' || ext ELSE fil_val := nam
      END
    ELSE csr := sav_csr 
    END;

PUBLIC FUNCTION scn_int(lin : str; VAR csr : str_csr;
  VAR int_val : int) : succeed_or_fail;
  (*try to scan an integer from the line*)
  VAR sav_csr : str_csr; len : pos_int; num : int;
  BEGIN
    sav_csr := csr; scn_int := scn_rdy(lin, csr);
    IF scn_chr(lin, csr, '-') ORIF scn_chr(lin, csr, '+') THEN
      scn_int := NOT scn_chr(lin, csr, spc)
    ELSE scn_int := yes;
    IF scn_int THEN BEGIN
      len := scn_len(lin, csr, numeric); csr := csr + len;
      IF len < one THEN scn_int := fail
      ELSE BEGIN
        GETSTRING(SUBSTR(lin, sav_csr, (csr - sav_csr)), num)
        EXCEPTION
          IO_ERROR : scn_int := fail;
          OTHERS : SIGNAL
        END
      END;
    IF scn_int THEN int_val := num ELSE csr := sav_csr
    END;

$PAGE scan switch and indirect file name
PUBLIC FUNCTION scn_swt(lin : str; VAR csr : str_csr;
  swt_wrd : key; swt_abrv : key_idx) : succeed_or_fail;
  (*try to scan a switch from line*)
  VAR sav_csr : str_csr;
  BEGIN
    sav_csr := csr; scn_swt := scn_rdy(lin, csr);
    scn_swt := scn_swt ANDIF scn_chr(lin, csr, '/');
    scn_swt := scn_swt ANDIF scn_cmd(lin, csr, swt_wrd, swt_abrv);
    IF NOT scn_swt THEN csr := sav_csr
    END;

PUBLIC FUNCTION scn_ifl(lin : str; VAR csr : str_csr;
  VAR ifl_val : fil_nam) : succeed_or_fail;
  (*try to scan an indirect file from line*)
  VAR sav_csr : str_csr;
  BEGIN
    sav_csr := csr; scn_ifl := scn_rdy(lin, csr);
    scn_ifl := scn_ifl ANDIF scn_chr(lin, csr, '@');
    scn_ifl := scn_ifl ANDIF NOT scn_chr(lin, csr, spc);
    scn_ifl := scn_ifl ANDIF scn_fil(lin, csr, ifl_val);
    IF NOT scn_ifl THEN csr := sav_csr
    END;

$PAGE set up and clean up
PUBLIC PROCEDURE scn_cln_up;
  (*clean up scanner utility*)
  BEGIN
    IF cnt_cln(cnt_set_cln) THEN RETURN;
    wrt_cln_up
    END;

PUBLIC PROCEDURE scn_set_up;
  (*set up scanner utility*)
  BEGIN
    IF cnt_set(cnt_set_cln) THEN RETURN;
    wrt_set_up
    END.
   