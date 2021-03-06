$PAGE DTUTL.INC, last modified 2/28/84, zw
$IFNOT dtutlinc

$SYSTEM DTUTL.TYP

EXTERNAL VAR dtstatus: dt_status;

EXTERNAL FUNCTION scnmonth(STRING[*]; VAR INTEGER; VAR months): BOOLEAN;
EXTERNAL FUNCTION scnday(STRING[*]; VAR INTEGER; VAR days): BOOLEAN;
EXTERNAL FUNCTION scndt
  (STRING[*]; VAR INTEGER; VAR dt_rcd; SET OF dt_fld): BOOLEAN;
EXTERNAL FUNCTION strdt(dt_str; SET OF dt_fld): dt_rcd;
EXTERNAL FUNCTION dtstr(dt_rcd; SET OF dt_fld): dt_str;
EXTERNAL FUNCTION dtadv(dt_rcd; INTEGER; dt_fld): dt_rcd;
EXTERNAL FUNCTION dtdif(dt_rcd; dt_rcd; dt_fld): INTEGER;
EXTERNAL FUNCTION dtcmp(dt_rcd; dt_rcd): BOOLEAN;
EXTERNAL FUNCTION dtpak(dt_rcd): INTEGER;
EXTERNAL FUNCTION pakdt(INTEGER): dt_rcd;
EXTERNAL FUNCTION dt: dt_rcd;

$ENABLE dtutlinc
$ENDIF
  