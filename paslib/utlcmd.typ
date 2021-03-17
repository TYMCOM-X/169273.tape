(*utlcmd.typ, last modified 5/5/83, zw*)
$IFNOT utlcmdtyp

CONST cmd_lin_len = 256; (*slc cmd lin*)

TYPE
  cmd_tkn_typ = (nul_tkn, eol_tkn, cr_tkn, cmd_tkn, key_tkn, swt_tkn, wrd_tkn,
    int_tkn, fil_tkn, str_tkn, ifl_tkn);
  cmd_tkn_rcd = RECORD (*element of cmd_lst*)
    tkn_num: int;
    CASE tkn_typ: cmd_tkn_typ OF
      nul_tkn: ();
      eol_tkn: ();
      cr_tkn: ();
      cmd_tkn: (cmd_wrd: key; cmd_abrv: key_idx);
      key_tkn: (key_wrd: key; key_len: key_idx);
      swt_tkn: (swt_wrd: key; swt_abrv: key_idx);
      wrd_tkn: (wrd_chrs: chr_set);
      int_tkn: ();
      fil_tkn: ();
      str_tkn: ();
      ifl_tkn: ()
    END;
  cmd_rcd = RECORD (*result of qry_cmd*)
    tkn_num: int;
    CASE tkn_typ: cmd_tkn_typ OF
      nul_tkn: ();
      eol_tkn: ();
      cr_tkn: ();
      cmd_tkn: ();
      key_tkn: ();
      swt_tkn: ();
      wrd_tkn: (wrd_val: ^str);
      fil_tkn: (fil_val: fil_nam);
      int_tkn: (int_val: int);
      str_tkn: (str_val: ^str);
      ifl_tkn: (ifl_val: fil_nam)
    END;
  cmd_lst = ARRAY [1 .. *] OF cmd_tkn_rcd;

$ENABLE utlcmdtyp
$ENDIF
(*end of utlcmd.typ*)
