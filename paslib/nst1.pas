MODULE NST1;

$HEADER NST1.HDR

$SYSTEM IDTIME.TYP
$SYSTEM DTIMEI.INC

EXTERNAL FUNCTION DC_TIME(TIME_INT): TIMEREC;


PUBLIC FUNCTION NS_T1(TIME: TIME_INT): NS_TIME1;

VAR
    H: 0..23;
   AM: PACKED ARRAY [1..2] OF CHAR;

BEGIN
   WITH DC_TIME(TIME) DO

   BEGIN

      H := HOURS;
      if hours < 12 then begin
	 am := 'AM';
	 if H = 0 then H := 12;
      end
      else begin
	 am := 'PM';
	 if H > 12 then H := H - 12;
      end;
      NS_T1 := CHARS2(H) || ':'
         || CHARS2(MINS) || ':'
         || CHARS2(SECS) || ' '
         || AM

   END

END.
 