MODULE DCEXT;

$HEADER DCEXT.HDR

$SYSTEM IDTIME.TYP
$SYSTEM DTIMEI.INC

EXTERNAL FUNCTION DC_DTIME(DTIME: DTIME_INT): DTIMEREC;


PUBLIC FUNCTION DC_EXT(DTIME: DTIME_INT): DTIME_EXT;

BEGIN
   WITH DC_DTIME(DTIME) DO
      DC_EXT := CHARS2(DAY) || '-' || DC_MONTH(MONTH) || '-' ||
		SUBSTR(CV_BIN_STR(YEAR),3,2) || ' ' || CHARS2(HOURS) ||
		':' || CHARS2(MINS) || ':' || CHARS2(SECS);
END.
 