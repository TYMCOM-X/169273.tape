$WIDTH=100
$LENGTH=55
$TITLE PASLOG.PAS, last modified 1/13/84, zw
MODULE paslog OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- activity log*)

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM DTIME.TYP
$SYSTEM UTLPDR.INC

$INCLUDE PASLOG.TYP

$PAGE definitions

EXTERNAL VAR log_record: log_file_record;

$PAGE newlog, log_write

PUBLIC PROCEDURE newlog;
(*prepare the standard log record skeleton*)
BEGIN
  log_record.users_ppn := user_ppn();
  log_record.opt_tmpcor := FALSE;
  log_record.opt_hash := FALSE;
  log_record.run_time := RUNTIME
END;

PUBLIC PROCEDURE log_write;
VAR fil: FILE OF log_file_record;
BEGIN
  log_record.run_time := RUNTIME - log_record.run_time;
  RESET(fil, pdr(log_fil));
  IF IOSTATUS = IO_OK THEN BEGIN
    CLOSE(fil); REWRITE(fil, pdr(log_fil), [PRESERVE]);
    IF IOSTATUS = IO_OK THEN WRITE(fil, log_record)
  END;
  CLOSE(fil);
END.
