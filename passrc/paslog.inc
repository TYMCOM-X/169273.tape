(*PASLOG.INC, last modified 12/20/83, zw*)
$IFNOT pasloginc

$SYSTEM DTIME.TYP
$SYSTEM PASLOG.TYP

EXTERNAL VAR log_record: log_file_record;

EXTERNAL PROCEDURE newlog;
EXTERNAL PROCEDURE log_write;

$ENABLE pasloginc
$ENDIF
  