$WIDTH=100
$LENGTH=55
$TITLE utlcmd.pas, last modified 5/16/83, zw
MODULE utlcmd;
  (*interactive command utility*)

$SYSTEM UTLWRT
$SYSTEM UTLIO
$SYSTEM UTLSCN
$SYSTEM UTLRUN

$PAGE utlcmd declarations
$INCLUDE UTLCMD.TYP

CONST lin_len = 72; (*dpy lin len*)

VAR cnt_set_cln: int := zero;

PUBLIC VAR lin: STRING [cmd_lin_len]; (*slc cmd lin*)
PUBLIC VAR csr: 0 .. cmd_lin_len + 1; (*slc cmd lin csr*)

PUBLIC VAR usr_cmd: cmd_rcd; (*slc cmd rcd*)
VAR tmp_cmd: cmd_rcd; (*qry cmd rcd*)

VAR ifl_lev: pos_int := zero; (*level of indirect file nesting*)

$PAGE command scanner
PUBLIC FUNCTION cmd_scn(lin: str; VAR csr: str_csr; lst: cmd_lst;
  VAR cmd: cmd_rcd): succeed_or_fail;
  (*scan a command of the given list from the line*)
  VAR nul_ok: yes_or_no; nul_num: int; idx: ord_int;
  BEGIN
    cmd_scn := fail; nul_ok := no;
    FOR idx := one to UPPERBOUND(lst) DO BEGIN
      WITH cmd, lst[idx] DO CASE lst[idx].tkn_typ OF
        nul_tkn: BEGIN
          nul_ok := yes; nul_num := lst[idx].tkn_num; cmd_scn := fail
          END;
        eol_tkn: cmd_scn := chk_eol(lin, csr);
        cr_tkn: cmd_scn := chk_eol(lin, csr);
        cmd_tkn: cmd_scn := scn_cmd(lin, csr, cmd_wrd, cmd_abrv);
        key_tkn: cmd_scn := scn_key(lin, csr, key_wrd, key_len);
        swt_tkn: cmd_scn := scn_swt(lin, csr, swt_wrd, swt_abrv);
        wrd_tkn: cmd_scn := scn_wrd(lin, csr, wrd_chrs, wrd_val);
        int_tkn: cmd_scn := scn_int(lin, csr, int_val);
        fil_tkn: cmd_scn := scn_fil(lin, csr, fil_val);
        str_tkn: cmd_scn := scn_str(lin, csr, str_val);
        ifl_tkn: cmd_scn := scn_ifl(lin, csr, ifl_val);
        OTHERS: barf('Inconsistant token type in CMD_SCN.')
        END;
      EXIT IF cmd_scn DO cmd.tkn_num := lst[idx].tkn_num
      END;
    IF cmd_scn THEN cmd.tkn_typ := lst[idx].tkn_typ;
    IF NOT cmd_scn ANDIF nul_ok THEN WITH cmd DO BEGIN
      tkn_typ := nul_tkn; tkn_num := nul_num; cmd_scn := succeed
      END
    END;

$PAGE display item
PUBLIC PROCEDURE dpy_itm(VAR pos: ord_int; new_lin: yes_or_no; itm: str);
  (*display the itm at the current position if possible*)
  VAR new_pos: ord_int;
  BEGIN
    IF pos > one THEN new_pos := pos + two ELSE new_pos := pos;
    new_pos := new_pos + str_len(itm);
    IF (new_pos > lin_len) OR new_lin THEN BEGIN
      IF (NOT new_lin) AND (pos > one) THEN WRITE(',');
      WRITELN; pos := one; new_pos := pos + str_len(itm)
      END;
    IF pos <> one THEN WRITE(',' || spc);
    WRITE(SUBSTR(itm, one, str_len(itm))); pos := new_pos
    END;

$PAGE dpy nul_tkn, eol_tkn, int_tkn, key_tkn
PROCEDURE dpy_nul_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      EXIT IF tkn_typ = nul_tkn DO flg := on;
    IF flg THEN BEGIN
      dpy_itm(pos, yes, 'Anything else.'); dpy_itm(pos, yes, nul)
      END
    END;

PROCEDURE dpy_eol_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      EXIT IF tkn_typ = eol_tkn DO flg := on;
    IF flg THEN BEGIN
      dpy_itm(pos, yes, 'A carriage return.'); dpy_itm(pos, yes, nul)
      END
    END;

PROCEDURE dpy_cr_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      EXIT IF tkn_typ = cr_tkn DO flg := on;
    IF flg THEN BEGIN
      dpy_itm(pos, yes, 'A carriage return.'); dpy_itm(pos, yes, nul)
      END
    END;

PROCEDURE dpy_int_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      EXIT IF tkn_typ = int_tkn DO flg := on;
    IF flg THEN BEGIN
      dpy_itm(pos, yes, 'An integer.'); dpy_itm(pos, yes, nul)
      END
    END;

$PAGE dpy key_tkn, fil_tkn, cmd_tkn, str_tkn
PROCEDURE dpy_key_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      IF tkn_typ = key_tkn THEN BEGIN
        IF NOT flg THEN dpy_itm(pos, yes, 'A key, one of:');
        IF NOT flg THEN dpy_itm(pos, yes, nul);
        flg := on;
        dpy_itm(pos, no, '"' || SUBSTR(key_wrd, one, key_len) || '"')
        END;
    IF flg THEN dpy_itm(pos, yes, nul)
    END;

PROCEDURE dpy_fil_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      EXIT IF tkn_typ = fil_tkn DO flg := on;
    IF flg THEN BEGIN
      dpy_itm(pos, yes, 'A file name.'); dpy_itm(pos, yes, nul)
      END
    END;

PROCEDURE dpy_cmd_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      IF tkn_typ = cmd_tkn THEN BEGIN
        IF NOT flg THEN dpy_itm(pos, yes, 'A command, one of:');
        IF NOT flg THEN dpy_itm(pos, yes, nul);
        flg := on; dpy_itm(pos, no, SUBSTR(cmd_wrd, one, str_len(cmd_wrd)))
        END;
    IF flg THEN dpy_itm(pos, yes, nul)
    END;

PROCEDURE dpy_str_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      EXIT IF tkn_typ = str_tkn DO flg := on;
    IF flg THEN BEGIN
      dpy_itm(pos, yes, 'A string.'); dpy_itm(pos, yes, nul)
      END
    END;

$PAGE dpy wrd_tkn, swt_tkn, ifl_tkn, display command list
PROCEDURE dpy_wrd_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      EXIT IF tkn_typ = wrd_tkn DO flg := on;
    IF flg THEN BEGIN
      dpy_itm(pos, yes, 'A word.'); dpy_itm(pos, yes, nul)
      END
    END;

PROCEDURE dpy_swt_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      IF tkn_typ = swt_tkn THEN BEGIN
        IF NOT flg THEN dpy_itm(pos, yes, 'A switch, "/" followed by:');
        IF NOT flg THEN dpy_itm(pos, yes, nul);
        flg := on; dpy_itm(pos, no, SUBSTR(swt_wrd, one, str_len(swt_wrd)))
        END;
    IF flg THEN dpy_itm(pos, yes, nul)
    END;

PROCEDURE dpy_ifl_tkns(lst: cmd_lst; VAR pos: ord_int);
  VAR idx: ord_int; flg: on_or_off;
  BEGIN
    flg := off; FOR idx := one TO UPPERBOUND(lst) DO WITH lst[idx] DO
      EXIT IF tkn_typ = ifl_tkn DO flg := on;
    IF flg THEN BEGIN
      dpy_itm(pos, yes, '"@" a file name.'); dpy_itm(pos, yes, nul)
      END
    END;

PUBLIC PROCEDURE dpy_cmd_lst(lst: cmd_lst);
  (*display command list*)
  VAR pos: ord_int;
  BEGIN
    pos := one;
    dpy_itm(pos, yes, 'The command list is:');
    dpy_itm(pos, yes, nul);
    dpy_cmd_tkns(lst, pos);
    dpy_key_tkns(lst, pos);
    dpy_swt_tkns(lst, pos);
    dpy_fil_tkns(lst, pos);
    dpy_int_tkns(lst, pos);
    dpy_wrd_tkns(lst, pos);
    dpy_str_tkns(lst, pos);
    dpy_ifl_tkns(lst, pos);
    dpy_nul_tkns(lst, pos);
    dpy_eol_tkns(lst, pos);
    dpy_cr_tkns(lst, pos);
    dpy_itm(pos, yes, nul)
    END;

$PAGE cursor display and command error, indirect file command
PUBLIC PROCEDURE dpy_csr(lin: str; csr: str_csr);
  (*display line with pointer at cursor*)
  VAR idx, len: pos_int;
  BEGIN
    idx := one;
    WHILE NOT chk_eol(lin, idx) DO BEGIN
      len := str_len(lin) - idx + one; IF len > lin_len THEN len := lin_len;
      WRITELN(SUBSTR(lin, idx, len));
      IF (len < csr) OR (idx > csr) THEN idx := idx + len
      ELSE FOR idx := idx TO (idx + len) DO
        IF idx < csr THEN WRITE(spc)
        ELSE IF idx = csr THEN WRITELN('^')
      END
    END;

PUBLIC PROCEDURE cmd_err(VAR lin: str; VAR csr: str_csr; err_msg: str);
  (*display error message and cursor, clear line*)
  BEGIN
    WRITELN(bel);
    IF err_msg <> nul THEN WRITELN(err_msg)
    ELSE WRITELN('Command error.  Use "?" to display the command list.');
    WRITELN('The command line was:'); dpy_csr(lin, csr);
    clr_lin(lin, csr)
    END;

PUBLIC PROCEDURE ifl_cmd;
  (*process an indirect file from usr_cmd*)
  BEGIN
    IF INDEX(usr_cmd.ifl_val, '.') > 0 THEN BEGIN
      IF psh_fil(INPUT, usr_cmd.ifl_val, 'READ') THEN inc(ifl_lev)
      END
    ELSE BEGIN
      IF psh_fil(INPUT, usr_cmd.ifl_val || '.CMD', 'READ') THEN inc(ifl_lev)
      END
    END;

$PAGE get command, query command
PUBLIC PROCEDURE get_lin(VAR lin: str; VAR csr: str_csr;
  prmpt, dflt: str);
  (*prompt user for input, return command line and cursor*)
  BEGIN
    WHILE EOF AND (ifl_lev > zero) DO BEGIN
      pop_fil(INPUT); dec(ifl_lev)
      END;
    IF prmpt = nul THEN BEGIN
      IF dflt = nul THEN get_str(INPUT, '*', lin)
      ELSE get_str(INPUT, '<' || dflt || '> *', lin)
      END
    ELSE IF prmpt[str_len(prmpt)] IN ['.', '?', '!'] THEN BEGIN
      IF dflt <> nul THEN get_str(INPUT,
        crlf || prmpt || crlf || '<' || dflt || '>' || crlf || '|', lin)
      ELSE get_str(INPUT, crlf || prmpt || crlf || '|', lin)
      END
    ELSE BEGIN
      IF dflt = nul THEN get_str(INPUT, prmpt, lin)
      ELSE get_str(INPUT, prmpt || ' <' || dflt || '> ', lin)
      END;
    IF lin = nul THEN set_lin(lin, csr, dflt) ELSE set_lin(lin, csr, lin)
    END;

PUBLIC PROCEDURE get_cmd(VAR lin: str; VAR csr: str_csr;
  prmpt, dflt: str; lst: cmd_lst;
  VAR cmd_val: cmd_rcd);
  (*get a command of the given list from the line*)
  VAR qst_asked: yes_or_no;
  FUNCTION hlp(): succeed_or_fail;
    BEGIN
      hlp := (NOT chk_eol(lin, csr)) ANDIF chk_chr(lin, csr, ['?']);
      IF hlp ANDIF chk_eol(lin, csr + one) THEN BEGIN
        dpy_cmd_lst(lst); clr_lin(lin, csr);
        IF dflt <> nul THEN WRITELN('Enter <default> with carriage return.')
        END
      ELSE hlp := fail
      END;
  BEGIN
    qst_asked := no;
    LOOP
      EXIT IF (NOT hlp()) ANDIF cmd_scn(lin, csr, lst, cmd_val)
        ANDIF ((cmd_val.tkn_typ <> cr_tkn) ORIF qst_asked);
      IF NOT chk_eol(lin, csr) THEN cmd_err(lin, csr, nul);
      get_lin(lin, csr, prmpt, dflt);
      qst_asked := yes
      END
    END;

$PAGE solicit command, eol, list, yes/no and string
PUBLIC PROCEDURE slc_cmd(qst: str; lst: cmd_lst;
  VAR tkn_num: int);
  (*solicit a command*)
  BEGIN
    get_cmd(lin, csr, qst, nul, lst, usr_cmd);
    tkn_num := usr_cmd.tkn_num
    END;

PUBLIC FUNCTION slc_eol: succeed_or_fail;
  (*solicit end of line*)
  BEGIN
    slc_eol := chk_eol(lin, csr)
    END;

PUBLIC FUNCTION slc_lst: succeed_or_fail;
  (*solicit end or continuation of list*)
  BEGIN
    get_cmd(lin, csr, 'Enter comma or carriage return.', nul,
      ((1, eol_tkn),(2, key_tkn, ',', 1)), tmp_cmd);
    CASE tmp_cmd.tkn_num OF
      1: slc_lst := fail;
      2: slc_lst := succeed
      END
    END;

PUBLIC FUNCTION slc_yon(qst: str; VAR yon_val: yes_or_no): succeed_or_fail;
  (*solicit yes or no*)
  BEGIN
    get_cmd(lin, csr, qst, nul,
      ((1, cmd_tkn, 'YES', 1), (2, cmd_tkn, 'NO', 1),
      (3, swt_tkn, 'QUIT', 1)), tmp_cmd);
    CASE tmp_cmd.tkn_num OF
      1: yon_val := yes;
      2: yon_val := no;
      3: slc_yon := fail
      END
    END;

PUBLIC FUNCTION slc_str(qst: str; VAR str_val: ^str): succeed_or_fail;
  (*solicit a string*)
  BEGIN
    get_cmd(lin, csr, qst, nul,
      ((1, swt_tkn, 'QUIT', 1), (1, str_tkn)), tmp_cmd);
    CASE tmp_cmd.tkn_num OF
      1: str_val := tmp_cmd.str_val;
      2: slc_str := fail
      END
    END;

$PAGE solicit word, file name and integer
PUBLIC FUNCTION slc_wrd(qst: str; VAR wrd_val: ^str): succeed_or_fail;
  (*solicit a word*)
  CONST chrs: chr_set = alpha + numeric;
  BEGIN
    get_cmd(lin, csr, qst, nul,
      ((2, swt_tkn, 'QUIT', 1), (1, wrd_tkn, chrs)), tmp_cmd);
    CASE tmp_cmd.tkn_num OF
      1: wrd_val := tmp_cmd.wrd_val;
      2: slc_wrd := fail
      END
    END;

PUBLIC FUNCTION slc_fil(qst: str; VAR fil_val: fil_nam): succeed_or_fail;
  (*solicit a file name*)
  BEGIN
    get_cmd(lin, csr, qst, nul,
      ((1, fil_tkn), (2, swt_tkn, 'QUIT', 1)), tmp_cmd);
    CASE tmp_cmd.tkn_num OF
      1: fil_val := tmp_cmd.fil_val;
      2: slc_fil := fail
      END
    END;

PUBLIC FUNCTION slc_int(qst: str; VAR int_val: int): succeed_or_fail;
  (*solicit an integer*)
  BEGIN
    get_cmd(lin, csr, qst, nul,
      ((1, int_tkn), (2, cmd_tkn, 'QUIT', 1)), tmp_cmd);
    CASE tmp_cmd.tkn_num OF
      1: int_val := tmp_cmd.int_val;
      2: slc_int := fail
      END
    END;

$PAGE solicit text
PUBLIC FUNCTION slc_txt(qst: str; VAR txt_val: ^txt): succeed_or_fail;
  (*solicit lines of text*)
  VAR txt_ptr, tmp_ptr: ^txt; opr: int;
  PROCEDURE do_hlp;
    BEGIN
      IF slc_eol THEN BEGIN
        WRITELN;
        WRITELN('Enter one or more lines of text.');
        WRITELN('Use "/END" to exit or "/QUIT" to abort.');
        WRITELN('Use "@" file name to include text from a file.');
        WRITELN
        END
      ELSE cmd_err(lin, csr, 'HELP takes no argument here.')
      END;
  PROCEDURE slct(prmpt: str);
    BEGIN
      txt_ptr := NIL; tmp_ptr := NIL; slc_txt := fail;
      LOOP
        get_cmd(lin, csr, prmpt, nul,
          ((1, swt_tkn, 'HELP', 1), (2, swt_tkn, 'QUIT', 1),
          (3, swt_tkn, 'END', 1), (4, ifl_tkn), (5, str_tkn),
          (5, nul_tkn)), tmp_cmd);
        CASE tmp_cmd.tkn_num OF
          1: do_hlp;
          2: BEGIN
            del_txt(txt_ptr); WRITELN('Aborting text input.');
            slc_txt := fail
            END;
          3: BEGIN WRITELN('End of text input.'); slc_txt := succeed END;
          4: ifl_cmd;
          5: BEGIN
            IF tmp_ptr = NIL THEN BEGIN NEW(tmp_ptr); txt_ptr := tmp_ptr END
            ELSE BEGIN NEW(tmp_ptr^.nxt); tmp_ptr := tmp_ptr^.nxt END;
            tmp_ptr^.nxt := NIL; tmp_ptr^.str_ptr := tmp_cmd.str_val
            END
          END;
        EXIT IF tmp_cmd.tkn_num IN [2, 3]
        END
      END;
  BEGIN
    IF INPUT = TTY THEN WRITELN(TTY, crlf || qst);
    IF qst = nul THEN slct('*')
    ELSE IF qst[str_len(qst)] IN ['.', '?', '!'] THEN slct('|')
    ELSE slct(nul);
    IF slc_txt THEN txt_val := txt_ptr
    END;

$PAGE set up and clean up and reset
PUBLIC PROCEDURE cmd_rst;
  (*reset command utility*)
  BEGIN
    clr_lin(lin, csr);
    usr_cmd.tkn_num := zero; usr_cmd.tkn_typ := cmd_tkn
    END;

PUBLIC PROCEDURE cmd_cln_up;
  (*clean up command utility*)
  BEGIN
    IF cnt_cln(cnt_set_cln) THEN RETURN;
    run_cln_up;
    scn_cln_up;
    io_cln_up;
    wrt_cln_up
    END;

PUBLIC PROCEDURE cmd_set_up;
  (*set up command utility*)
  BEGIN
    IF cnt_set(cnt_set_cln) THEN RETURN;
    wrt_set_up;
    io_set_up;
    scn_set_up;
    run_set_up;
    cmd_rst
    END.
  