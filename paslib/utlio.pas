$WIDTH=100
$LENGTH=55
$TITLE utlio.pas, last modified 5/16/83, zw
MODULE utlio;
  (*input/output utility*)

$SYSTEM UTLWRT

$PAGE utlio declarations
$INCLUDE UTLIO.TYP

VAR cnt_set_cln: int := zero;

CONST fil_stk_siz = 10;
VAR fil_stk_ptr: zero .. fil_stk_siz := zero;
VAR fil_stk: ARRAY [one .. fil_stk_siz] OF txt_fil;

$PAGE open file, close file, get buffer
PUBLIC FUNCTION opn_fil(VAR fil: txt_fil; nam: fil_nam;
   opn_mod: str): succeed_or_fail;
  (*try to open a file for specified mode*)
  CONST get_mod = 'READ'; put_mod = 'WRITE'; app_mod = 'APPEND';
  BEGIN
    IF (nam = nul) ORIF (UPPERCASE(nam) = 'TTY:') THEN BEGIN
      IF opn_mod = get_mod THEN fil := TTY
      ELSE IF opn_mod = put_mod THEN fil := TTYOUTPUT
      ELSE IF opn_mod = app_mod THEN fil := TTYOUTPUT
      ELSE barf('Invalid opn_fil mode "' || opn_mod || '".')
      END
    ELSE BEGIN
      IF opn_mod = get_mod THEN RESET(fil, nam)
      ELSE IF opn_mod = put_mod THEN REWRITE(fil, nam)
      ELSE IF opn_mod = app_mod THEN REWRITE(fil, nam, [PRESERVE])
      ELSE barf('Invalid opn_fil mode "' || opn_mod || '".')
      END;
    opn_fil := succeed;
    EXCEPTION
      IO_ERROR: opn_fil := fail;
    END;

PUBLIC PROCEDURE cls_fil(VAR fil: txt_fil);
  (*close a file, will not close TTY:*)
  BEGIN
    IF (fil <> NILF) ANDIF
      (fil <> TTY) ANDIF (fil <> TTYOUTPUT) THEN CLOSE(fil);
    fil := NILF
    END;

PUBLIC PROCEDURE get_buf(VAR fil: txt_fil; VAR buf: str);
  (*get a line from a file, return nul if end of file*)
  BEGIN
    IF EOF(fil) THEN buf := nul
    ELSE BEGIN
      IF fil = TTY THEN BEGIN READLN(TTY); READ(TTY, buf) END
      ELSE BEGIN READ(fil, buf); READLN(fil) END
      END;
    EXCEPTION
      IO_ERROR: buf := nul
    END;

$PAGE text file push and pop (open and close)
PUBLIC FUNCTION psh_fil(VAR fil: txt_fil; nam: fil_nam;
  opn_mod: str): succeed_or_fail;
  (*push file onto stack, return opened file*)
  BEGIN
    psh_fil := fil_stk_ptr < fil_stk_siz;
    IF psh_fil THEN BEGIN
      inc(fil_stk_ptr); fil_stk[fil_stk_ptr] := fil;
      psh_fil := opn_fil(fil, nam, opn_mod);
      IF NOT psh_fil THEN BEGIN
        fil := fil_stk[fil_stk_ptr]; dec(fil_stk_ptr)
        END
      END
    END;

PUBLIC PROCEDURE pop_fil(VAR fil: txt_fil);
  (*close file, return file popped from stack*)
  BEGIN
    IF fil_stk_ptr > zero THEN BEGIN
      cls_fil(fil);
      fil := fil_stk[fil_stk_ptr]; dec(fil_stk_ptr)
      END
    END;

$PAGE get string, put text
PUBLIC PROCEDURE get_str(VAR fil: txt_fil; qst: str; VAR lin: str);
  (*input a string from a file*)
  BEGIN
    lin := nul;
    IF (fil <> NILF) ANDIF NOT EOF(fil) THEN BEGIN
      IF fil = TTY THEN BEGIN WRITE(TTYOUTPUT, qst); BREAK(TTYOUTPUT) END;
      get_buf(fil, lin)
      END
    END;

PUBLIC PROCEDURE put_txt(VAR fil: txt_fil; txt_ptr: ^txt);
  (*write all lines of text to specified file*)
  VAR tmp_ptr: ^txt;
  BEGIN
    tmp_ptr := txt_ptr;
    WHILE tmp_ptr <> NIL DO BEGIN
      IF tmp_ptr^.str_ptr <> NIL THEN WRITELN(fil, tmp_ptr^.str_ptr^);
      tmp_ptr := tmp_ptr^.nxt
      END
    END;

$PAGE set up and clean up
PUBLIC PROCEDURE io_cln_up;
  (*clean up input/output utility*)
  BEGIN
    IF cnt_cln(cnt_set_cln) THEN RETURN;
    CLOSE(); fil_stk_ptr := zero;
    wrt_cln_up
    END;

PUBLIC PROCEDURE io_set_up;
  (*set up input/output utility*)
  BEGIN
    IF cnt_set(cnt_set_cln) THEN RETURN;
    wrt_set_Up;
    OPEN(TTY); INPUT := TTY;
    REWRITE(TTYOUTPUT); OUTPUT := TTYOUTPUT;
    fil_stk_ptr := zero
    END.
   