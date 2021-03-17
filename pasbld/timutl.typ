$PAGE TIMUTL.TYP, last modified 5/11/84, zw
$IFNOT timutltyp
(*TYM-Pascal day/time utility*)
(*HEADER TIMUTL.HDR*)

CONST
max_year = 2217;
day_zero = 'NOV 17, 1858';

TYPE
d_t_record = PACKED RECORD
  d, t: 0 .. 777777b
END;
dtime_int = d_t_record;
dtime_ext = PACKED ARRAY [1..18] OF CHAR; (* STANDARD EXTERNAL DAY/TIME *)
ns_ext = STRING[24]; (* FOR NON-STANDARD EXTERNAL DAY/TIMES *)
date_int = d_t_record; (* INTERNAL DATE *)
time_int = d_t_record; (* INTERNAL TIME *)
dec_date = 0..77777b; (* 15 BIT DEC DATE *)
tym_date = 0..77777b; (* 15 BIT TYMSHARE DATE *)
dec_time = 0..86400000; (* MILLISECONDS SINCE MIDNIGHT TIME *)
dtime_err = (dt_noerr,dt_err); (* ERROR CODES *)
days = INTEGER;
seconds = INTEGER;
week_day = (sunday,monday,tuesday,wednesday,thursday,friday,saturday);
ns_date1 = PACKED ARRAY [1..8] OF CHAR; (* 'MM/DD/YY' *)
ns_date2 = PACKED ARRAY [1..12] OF CHAR; (* 'MMM DD, YYYY' *)
ns_time1 = PACKED ARRAY [1..11] OF CHAR; (* 'HH:MM:SS PM' *)
dtimerec = RECORD (* BINARY DAY/TIME RECORD *)
  year: 1858..max_year;
  month: 1..12;
  day: 1..31;
  hours: 0..23;
  mins: 0..59;
  secs: 0..59
END;
daterec = RECORD (* BINARY DATE RECORD *)
  year: 1858..max_year;
  month: 1..12;
  day: 1..31
END;
timerec = RECORD (* BINARY TIME RECORD *)
  hours: 0..23;
  mins: 0..59;
  secs: 0..59
END;
$ENABLE timutltyp
$ENDIF
