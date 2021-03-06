$WIDTH=100
$LENGTH=55
$TITLE lib.pas, last modified 4/6/83, zw
PROGRAM lib;

CONST
  prog_nam = 'LIBRARY'; prog_ver = '1.0';
  prog_pur = 'This is a file library program.';

$SYSTEM WRTUTL
$SYSTEM SLCUTL
$SYSTEM HLPUTL
$SYSTEM ERRUTL
$SYSTEM LIBUTL

VAR cnt_set_cln : int := zero;

$PAGE set up and clean up
PUBLIC PROCEDURE cln_up;
  BEGIN
    IF cnt_cln(cnt_set_cln) THEN RETURN;
    lib_cln_up;
    err_cln_up;
    hlp_cln_up;
    slc_cln_up;
    wrt_cln_up
    END;

PUBLIC PROCEDURE set_up;
  BEGIN
    IF cnt_set(cnt_set_cln) THEN RETURN;
    wrt_set_up;
    slc_set_up;
    hlp_set_up;
    err_set_up;
    lib_set_up
    END;

$PAGE command procedures
PROCEDURE do_acc;
  VAR lib_fil : fil_nam;
  BEGIN
    IF slc_fil('Enter library file name.', lib_fil, 'LIB') THEN
      lib_opn(lib_fil)
    END;

PROCEDURE do_exp;
  BEGIN
    lib_pak;
    END;

PROCEDURE do_put;
  VAR fil : fil_nam;
  BEGIN
    REPEAT
      EXIT IF NOT
        slc_fil('Enter name of file to put into library.', fil, nul);
      lib_put(fil)
      UNTIL slc_end
    END;

PROCEDURE do_get;
  VAR fil : fil_nam;
  BEGIN
    REPEAT
      EXIT IF NOT
        slc_fil('Enter name of file to get from library.', fil, nul);
      lib_get(fil)
      UNTIL slc_end
    END;

PROCEDURE do_del;
  VAR fil : fil_nam;
  BEGIN
    REPEAT
      EXIT IF NOT
        slc_fil('Enter name of file to delete from library.', fil, nul);
      lib_del(fil)
      UNTIL slc_end
    END;

PROCEDURE do_dir;
  VAR fil : fil_nam;
  BEGIN
    IF slc_eol THEN lib_dir('??????.???')
    ELSE REPEAT
      EXIT IF NOT slc_fil('Enter name of file in library.', fil, nul);
      lib_dir(fil)
      UNTIL slc_end
    END;

$PAGE command interpreter
PUBLIC FUNCTION get_opr : int;
  BEGIN
    IF NOT slc_cmd('Enter ' || prog_nam || ' command.',
      ((1, cmd_tkn, 'HELP', 1), (2, cmd_tkn, 'QUIT', 1),
      (3, cmd_tkn, 'ACCESS', 1), (4, cmd_tkn, 'EXPUNGE', 1),
      (5, cmd_tkn, 'PUT', 2), (6, cmd_tkn, 'GET', 1),
      (7, cmd_tkn, 'DELETE', 2), (8, cmd_tkn, 'DIRECTORY', 2),
      (9, ifl_tkn)), get_opr) THEN get_opr := 2
    END;

PUBLIC FUNCTION do_opr(opr : int) : succeed_or_fail;
  BEGIN
    do_opr := succeed;
    CASE opr OF
      1 : hlp_cmd('LIB', 'LIB_CMD');
      2 : do_opr := fail;
      3 : do_acc;
      4 : do_exp;
      5 : do_put;
      6 : do_get;
      7 : do_del;
      8 : do_dir;
      9 : do_ifl
      END
    END;

BEGIN
  trp_errs(prog_nam, prog_ver, prog_pur)
  END.
    