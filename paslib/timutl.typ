$PAGE TIMUTL.TYP, last modified 2/23/84, zw
$IFNOT timutltyp

CONST
max_year = 2217;
min_year = 1858;
day_time_zero = 'NOV 17, 1858';

TYPE
day_time_int = PACKED RECORD d, t: 0 .. #o777777 END; (*internal day/time*)
day_time_ext = PACKED ARRAY [1 .. 18] OF CHAR; (*standard external day/time*)
ns_ext = STRING[24];  (*for non-standard external day/times*)
date_int = PACKED RECORD d, t: 0 .. #o777777 END; (*internal date*)
time_int = PACKED RECORD d, t: 0 .. #o777777 END; (*internal time*)
dec_date = 0 .. #o77777; (*15 bit dec date*)
tym_date = 0 .. #o77777; (*15 bit tymshare date*)
dec_time = 0 .. 86400000; (*milliseconds since midnight time*)
timutl_err = (timutl_noerr,timutl_err); (*error codes*)
days = INTEGER;
seconds = INTEGER;
week_day = (sunday,monday,tuesday,wednesday,thursday,friday,saturday);
year_month = (january,february,march,april,may,june,july,august,
  september,october,november,december);
ns_date1 = PACKED ARRAY [1 .. 8] OF CHAR; (*'mm/dd/yy'*) 
ns_date2 = PACKED ARRAY [1 .. 12] OF CHAR; (*'mmm dd, yyyy'*)
ns_time1 = PACKED ARRAY [1 .. 11] OF CHAR; (*'hh:mm:ss pm'*)
day_time_rcd = RECORD  (*binary day/time RECORD*)
  year: 1858 .. max_year; month: 1 .. 12; day: 1 .. 31;
  hours: 0 .. 23; mins: 0 .. 59; secs: 0 .. 59
END;
date_rcd = RECORD  (*binary date RECORD*)
  year: 1858 .. max_year; month: 1 .. 12; day: 1 .. 31
END;
time_rcd = RECORD  (*binary time RECORD*)
  hours: 0 .. 23; mins: 0 .. 59;  secs: 0 .. 59
END;

CONST
internal_timutl_base: day_tim_int = ( 0, 0 );
day_of_week: ARRAY[week_day] OF STRING[9] =
  ('SUNDAY','MONDAY','TUESDAY','WEDNESDAY','THURSDAY','FRIDAY','SATURDAY');
abbrev_day_of_week: ARRAY[week_day] OF STRING[3] =
  ('SUN','MON','TUE','WED','THU','FRI','SAT');
month_name: ARRAY[year_month] OF PACKED ARRAY [1 .. 3] OF CHAR =
     (  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
	'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'  );
month_of_year: ARRAY[year_month] OF STRING[9] =
  ('JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY','AUGUST',
  'SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER');
abbrev_month_of_year: ARRAY[year_month] OF STRING[3] =
  ('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC');

$ENABLE timutltyp
$ENDIF
  