MODULE DCDTIME;

$HEADER DCDTIM.HDR

$SYSTEM IDTIME.TYP

EXTERNAL FUNCTION DC_TIME(TIME_INT): TIMEREC;
EXTERNAL FUNCTION DC_DATE(DATE_INT): DATEREC;
EXTERNAL FUNCTION EXTR_TIME(DTIME_INT): TIME_INT;
EXTERNAL FUNCTION EXTR_DATE(DTIME_INT): DATE_INT;


PUBLIC FUNCTION DC_DTIME(DTIME: DTIME_INT): DTIMEREC;

VAR
   TIME_BIN: TIMEREC;
   DATE_BIN: DATEREC;

BEGIN

   TIME_BIN := DC_TIME(EXTR_TIME(DTIME));
   DATE_BIN := DC_DATE(EXTR_DATE(DTIME));

   WITH DC_DTIME DO
   BEGIN
      YEAR := DATE_BIN.YEAR;
      MONTH := DATE_BIN.MONTH;
      DAY := DATE_BIN.DAY;
      HOURS := TIME_BIN.HOURS;
      MINS := TIME_BIN.MINS;
      SECS := TIME_BIN.SECS
   END

END.
  