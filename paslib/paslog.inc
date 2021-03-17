(*paslog.inc, last modified 5/10/83, zw*)
$IFNOT pasloginc

$INCLUDE UTLTIM.TYP
$INCLUDE PASLOG.TYP

EXTERNAL VAR log_record: log_file_record; (*global log record*)

EXTERNAL PROCEDURE log_start;
  (*prepare the standard log record skeleton*)

EXTERNAL PROCEDURE log_write;
  (*append completed log record to log file*)

$ENABLE pasloginc
$ENDIF
(*end of paslog.inc*)
