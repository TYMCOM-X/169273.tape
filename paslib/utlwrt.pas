$WIDTH=100
$LENGTH=55
$TITLE utlwrt.pas, last modified 5/5/83, zw
MODULE utlwrt;
  (*some PASCAL warts*)

$PAGE utlwrt types
$INCLUDE UTLWRT.TYP

VAR cnt_set_cln : int := zero;

$PAGE math functions
PUBLIC PROCEDURE inc(VAR i : int);
  (*increment an integer*)
  BEGIN
    i := i + one
    END;

PUBLIC PROCEDURE dec(VAR i : int);
  (*decrement an integer*)
  BEGIN
    i := i - one
    END;

$PAGE set up and clean up management, crash and burn
PUBLIC FUNCTION cnt_cln(VAR cnt_set_cln : int) : bln;
  (*decrement for clean up, true if gt zero*)
  BEGIN
    cnt_set_cln := cnt_set_cln - one;
    IF cnt_set_cln < zero THEN cnt_set_cln := zero;
    cnt_cln := cnt_set_cln > zero
    END;

PUBLIC FUNCTION cnt_set(VAR cnt_set_cln : int) : bln;
  (*increment for set up, true if gt one*)
  BEGIN
    cnt_set_cln := cnt_set_cln + one;
    cnt_set := cnt_set_cln > one
    END;

PUBLIC PROCEDURE barf(msg : str);
  (*un-graceful program exit*)
  BEGIN
    REWRITE(TTY); WRITELN(TTY); WRITELN(TTY, '? Barf.' || bel);
    WRITELN(TTY, msg);
    WRITELN(TTY, 'This should never happen!');
    STOP
    END;

$PAGE string manipulation: len, justify, copy, delete
PUBLIC FUNCTION str_len(s : str) : pos_int;
  (*return practical length of string, ignor trailing spaces*)
  BEGIN
    str_len := LENGTH(s);
    WHILE (str_len> zero) ANDIF (s[str_len] = spc) DO str_len := str_len - one
    END;

PUBLIC PROCEDURE jfy_str(VAR s : str);
  (*right justify a string*)
  VAR idx, off_set : pos_int;
  BEGIN
    off_set := LENGTH(s) - str_len(s);
    IF (off_set > zero) ANDIF (off_set < LENGTH(s)) THEN BEGIN
      FOR idx := str_len(s) DOWNTO one DO s[idx + off_set] := s[idx];
      FOR idx := one TO off_set DO s[idx] := spc
      END
    END;

PUBLIC FUNCTION cpy_str(s : str) : ^str;
  (*copy a string*)
  BEGIN
    NEW(cpy_str, str_len(s)); cpy_str^ := SUBSTR(s, one, str_len(s))
    END;

PUBLIC PROCEDURE del_str(VAR s : ^str);
  (*delete a string*)
  BEGIN
    IF s <> NIL THEN BEGIN DISPOSE(s); s := NIL END
    END;

$PAGE line manipulation: set, clr, chk_eol, chk_chr
PUBLIC PROCEDURE set_lin(VAR lin : str; VAR csr : str_csr; s : str);
  (*set string to line, init cursor*)
  BEGIN
    lin := s; IF lin = nul THEN csr := zero ELSE csr := one
    END;

PUBLIC PROCEDURE clr_lin(VAR lin : str; VAR csr : str_csr);
  (*clear command line*)
  BEGIN lin := nul; csr := zero END;

PUBLIC FUNCTION chk_eol(lin : str; csr : str_csr) : yes_or_no;
  (*check for end of line*)
  BEGIN
    chk_eol := (csr < one) OR (csr > str_len(lin))
    END;

PUBLIC FUNCTION chk_chr(lin : str; csr : str_csr; chrs : chr_set) : yes_or_no;
  (*check if next char in line is in specified set*)
  BEGIN
    chk_chr := (NOT chk_eol(lin, csr)) ANDIF (lin[csr] IN chrs)
    END;

$PAGE delete text, str_int, int_str
PUBLIC PROCEDURE del_txt(VAR txt_ptr : ^txt);
  (*delete text, a list of lines*)
  VAR tmp_ptr : ^txt;
  BEGIN
    WHILE txt_ptr <> NIL DO BEGIN
      tmp_ptr := txt_ptr; txt_ptr := tmp_ptr^.nxt; tmp_ptr^.nxt := NIL;
      del_str(tmp_ptr^.str_ptr); DISPOSE(tmp_ptr)
      END
    END;

PUBLIC FUNCTION str_int(s : str) : int;
  (*convert a string to an integer*)
  BEGIN
    GETSTRING(s, str_int)
    END;

PUBLIC FUNCTION int_str(i : int) : STRING [15];
  (*convert an integer to a string*)
  BEGIN
    PUTSTRING(int_str, i : 0)
    END;

$PAGE set up and clean up
PUBLIC PROCEDURE wrt_cln_up;
  (*clean up wart utility*)
  BEGIN
    IF cnt_cln(cnt_set_cln) THEN RETURN
    END;

PUBLIC PROCEDURE wrt_set_up;
  (*set up wart utility*)
  BEGIN
    IF cnt_set(cnt_set_cln) THEN RETURN
    END.
  