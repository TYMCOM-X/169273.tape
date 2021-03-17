$WIDTH=100
$LENGTH=55
$TITLE UTLLST.PAS, last modified 3/9/84, zw
MODULE utllst;
(*TYM-Pascal listing utility*)

$SYSTEM UTLTYP.TYP
$SYSTEM UTLIO.INC

$INCLUDE UTLLST.TYP

PUBLIC PROCEDURE lstrdy(VAR fcb: lst_fcb);
(*ready the list file control block -- make sure the file is open*)
BEGIN
  WITH fcb DO IF fil = NILF
  THEN assume(opnfil(fil, nam, append_mode), '? Can not list to: ' || nam)
END;

PUBLIC PROCEDURE lstnop(VAR fcb: lst_fcb);
(*no action, used for default pag_hdr procedure*)
BEGIN
  lstrdy(fcb)
END;

PUBLIC PROCEDURE lstff(VAR fcb: lst_fcb);
(*emit form feed, default new_pag procedure*)
BEGIN
  lstrdy(fcb);
  WITH fcb DO BEGIN
    IF CURSOR(fil) > 1 THEN WRITELN(fil); (*make sure crlf preceeds ff*)
    PAGE(fil)
  END
END;

PUBLIC PROCEDURE lstatt(VAR fcb: lst_fcb; fil: txt_fil);
(*set up file control block for open file*)
BEGIN
  fcb.fil := fil; lstrdy(fcb);
  WITH fcb DO BEGIN
    nam := FILENAME(fil); pag_num := 1; lin_num := 1; col := 1; cont_col := 0;
    pag_wid := max_pag_wid; pag_len := 0;
    footing := lstnop; heading := lstnop; eject := lstff
  END
END;

PUBLIC FUNCTION lstopn(VAR fcb: lst_fcb; nam: fil_nam): yn;
(*try to set up list file control block for specified file*)
BEGIN
  lstopn := opnfil(fcb.fil, nam, write_mode);
  IF lstopn THEN lstatt(fcb, fcb.fil)
END;

PUBLIC PROCEDURE lstcls(VAR fcb: lst_fcb);
(*temporarily close list file*)
BEGIN
  lstrdy(fcb); clsfil(fcb.fil)
END;

PUBLIC PROCEDURE lstpag(VAR fcb: lst_fcb);
(*force new page with new_pag and pag_hdr*)
BEGIN
  WITH fcb DO BEGIN
    IF col <> 1 THEN WRITELN(fil); (*finish current line*)
    IF NOT ((lin_num = 1)  AND (col = 1)) THEN BEGIN (*if inside page*)
      footing(fcb); eject(fcb);
      pag_num := pag_num + 1; lin_num := 1; col := 1
    END;
    heading(fcb)
  END
END;

PUBLIC PROCEDURE lstskp(VAR fcb: lst_fcb);
(*move output to beginning of new line, maybe prepare for new page*)
BEGIN
  lstrdy(fcb);
  WITH fcb DO BEGIN
    IF (col = 1) AND (pag_len <> 0) AND (lin_num > pag_len) THEN lstpag(fcb)
    ELSE BEGIN
      lin_num := lin_num + 1; col := 1;
      IF NOT ((pag_len <> 0) AND (lin_num > pag_len)) THEN WRITELN(fil)
    END
  END
END;

PUBLIC PROCEDURE lstnskp(VAR fcb: lst_fcb; nskip, nleft: pag_len_range);
(*skip n times, new page in not speficied number of lines left*)
VAR i: pag_len_range;
BEGIN
  lstrdy(fcb)
  WITH fcb DO BEGIN
    IF (col = 1) AND (pag_len <> 0) AND (lin_num > pag_len) THEN lstpag(fcb)
    ELSE BEGIN
      col := 1;
      IF (pag_len = 0) IF (lin_num + nskip + nleft <= pag_len)
      THEN FOR i := 1 TO nskip DO lstskp(fcb)
      ELSE lin_num := pag_len + 1 (*prepare FOR new page*)
    END
  END
END;

PUBLIC PROCEDURE lsttab(VAR fcb: lst_fcb; colpos: pag_wid_range);
(*move to specified column position, maybe start new line*)
VAR i, pos: pag_wid_range;
CONST tab = CHR(#o011);
BEGIN
  lstrdy(fcb);
  WITH fcb DO BEGIN
    pos := MIN(colpos, pag_wid);
    IF col > pos THEN lstskp(fcb);
    IF col = pos THEN RETURN
    IF (col = 1) AND (pag_len <> 0) AND (lin_num > pag_len) THEN lstpag(fcb);
    FOR i := 1 TO (((pos - 1) DIV 8) - ((col - 1) DIV 8)) DO WRITE(fil, tab);
    WRITE(fil, ' ': MIN((pos - 1) MOD 8, pos - col)); col := pos
  END
END;

PUBLIC PROCEDURE lststr(VAR fcb: lst_fcb; str: gstr);
(*write string to list file within line and page bounds*)
VAR idx, len: pos_int;
BEGIN
  lstrdy(fcb);
  WITH fcb DO BEGIN
    IF (col = 1) AND (pag_len <> 0) AND (lin_num > pag_len) THEN lstpag(fcb);
    IF (LENGTH(str) > pag_wid - col + 1) AND
      (LENGTH(str) <= pag_wid - cont_col + 1) AND
      (cont_col <> 0)
    THEN lsttab(fcb, cont_col);
    idx := 1;
    LOOP
      len := MIN(LENGTH(str) - idx + 1, pag_wid - col + 1);
      WRITE(fil, SUBSTR(str, idx, len));
      idx := idx + len; col := col + len;
      EXIT IF (idx > LENGTH(str)) OR (cont_col = 0);
      lsttab(fcb, cont_col);
    END
  END
END;

PUBLIC PROCEDURE lstlin(VAR fcb: lst_fcb; str: gstr);
(*write string to list file and skip to new line*)
BEGIN
  lstrdy(fcb); lstwr(fcb, str); lstskp(fcb)
END;

PUBLIC PROCEDURE lstspc(VAR fcb: lst_fcb; spaces: pos_int);
(*write number of spaces to list file*)
BEGIN
  lsttab(fcb, fcb.col + spaces)
END.
