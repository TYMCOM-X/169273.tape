(*utltim.typ, last modified 5/10/83, zw*)
$IFNOT utltimtyp

CONST
  max_year = 2217;
  day_zero = 'NOV 17, 1858';

TYPE
  dtime_int = PACKED RECORD d: 0..1 END; (*internal day/time*)
  dtime_ext = PACKED ARRAY [1..18] of char; (*standard external day/time*)
  ns_ext = STRING[24]; (*for non-standard external day/times*)
  date_int = PACKED RECORD d: 0..1 END; (*internal date*)
  time_int = PACKED RECORD d: 0..1 END; (*internal time*)
  dec_date = 0..77777B; (*15 bit dec date*)
  tym_date = 0..77777B; (*15 bit tymshare date*)
  dec_time = 0..86400000; (*milliseconds since midnight time*)
  dtime_err = (dt_noerr,dt_err); (*error codes*)
  days = INTEGER;
  seconds = INTEGER;
  week_day = (sunday,monday,tuesday,wednesday,thursday,friday,saturday);
  ns_date1 = PACKED ARRAY [1..8] of char; (*'mm/dd/yy'*) 
  ns_date2 = PACKED ARRAY [1..12] of char; (*'mmm dd, yyyy'*)
  ns_time1 = PACKED ARRAY [1..11] of char; (*'hh:mm:ss pm'*)
  dtimerec = RECORD (*binary day/time record*)
    year: 1858..max_year;
    month: 1..12;
    day: 1..31;
    hours: 0..23;
    mins: 0..59;
    secs: 0..59
    END;
  daterec = RECORD (*binary date record*)
    year: 1858..max_year;
    month: 1..12;
    day: 1..31
    END;
  timerec = RECORD (*binary time record*)
    hours: 0..23;
    mins: 0..59;
    secs: 0..59
    END;

$ENABLE utltimtyp
$ENDIF
(*end of utltim.typ*)
   