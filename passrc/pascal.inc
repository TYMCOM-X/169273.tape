$PAGE PASCAL.INC, last modified 4/3/84, zw
$IFNOT pascalinc

$SYSTEM UTLTYP.TYP
$SYSTEM UTLSRC.TYP
$SYSTEM UTLSW.TYP
$SYSTEM UTLLST.TYP
$SYSTEM UTLKEY.TYP
$SYSTEM UTLTIM.TYP
$SYSTEM PASLOG.TYP
$SYSTEM PASADR.TYP
$SYSTEM PASCAL.TYP
$SYSTEM PASERR.TYP

EXTERNAL VAR
version: string_argument;
cursrc: source_position;
srcfil: file_name;
lstfil: file_name;
relfil: file_name;
dmpfil: file_name;
dmpfcb: list_file;
lstfcb: list_file;
maxerr: error_severity;
errcnt: ARRAY[error_severity] OF positive_integer;
defopts: command_option_record;
glbopts: command_option_record;
semopts: SET OF semantic_option;
lstopts: yes_no;
lstexp: yes_no;
reqallc: yes_no;
dogen: yes_no;
qcgen: yes_no;
slncnt: positive_integer;
ilncnt: positive_integer;
target: target_code;
logrcd: log_record;
usecmd: yes_no;

EXTERNAL PROCEDURE unchain;
EXTERNAL PROCEDURE chain(file_name);
EXTERNAL PROCEDURE dpystat(file_name; INTEGER);

$ENABLE pascalinc
$ENDIF
