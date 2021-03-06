$PAGE UTLIO.INC, last modified 1/6/84, zw
$IFNOT utlioinc

$SYSTEM UTLTYP.TYP
$SYSTEM UTLIO.TYP

EXTERNAL VAR
linnum: positive_integer;
byemsg: string_argument;

EXTERNAL PROCEDURE rdyio;
EXTERNAL PROCEDURE clstty;
EXTERNAL PROCEDURE opntty;
EXTERNAL PROCEDURE ttyoff;
EXTERNAL PROCEDURE ttyon;
EXTERNAL PROCEDURE ttyok(yes_no);
EXTERNAL PROCEDURE wrnum(number; integer; integer);
EXTERNAL PROCEDURE wrinteger(integer; integer);
EXTERNAL PROCEDURE wrspc(integer);
EXTERNAL PROCEDURE wrstr(generic_string);
EXTERNAL PROCEDURE wrlin(generic_string);
EXTERNAL FUNCTION rdlin(VAR generic_string): yes_no;
EXTERNAL FUNCTION skprd(integer): yes_no;
EXTERNAL PROCEDURE ask(generic_string; VAR generic_string);
EXTERNAL PROCEDURE asktty(generic_string; VAR generic_string);
EXTERNAL PROCEDURE ttymsg(generic_string);
EXTERNAL PROCEDURE byebye(generic_string);
EXTERNAL PROCEDURE assume(yes_no; generic_string);
EXTERNAL PROCEDURE clsfil(VAR text_file);
EXTERNAL FUNCTION opnfil(VAR text_file; file_name; open_mode): yes_no;
EXTERNAL PROCEDURE clsout;
EXTERNAL FUNCTION opnout(file_name; yes_no): yes_no;
EXTERNAL PROCEDURE clsin;
EXTERNAL FUNCTION opnin(file_name; yes_no): yes_no;
EXTERNAL FUNCTION popin: yes_no;
EXTERNAL FUNCTION pushin(file_name): yes_no;

$ENABLE utlioinc
$ENDIF
    