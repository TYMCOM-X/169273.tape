$PAGE DTUTL.TYP, last modified 2/28/84, zw
$IFNOT dtutltyp

(*day zero is Nov 17, 1858, last year is 2000*)

TYPE
dt_status = (dt_ok, dt_error);
dt_pak = RECORD
  CASE BOOLEAN OF
    TRUE: (number: INTEGER);
    FALSE: (days: 0 .. (2000 - 1858) * 365; seconds: 0 .. (60 * 60 * 24))
END;
dt_fld = (dt_year, dt_month, dt_day, dt_hour, dt_minute, dt_second);
dt_rcd = RECORD
  year: 1900 .. 2000; month: 1 .. 12; day: 1 .. 31;
  hour: 0 .. 23; minute: 0 .. 59; second: 0 .. 59
END;
dt_str = PACKED ARRAY [1 .. 17]; (*DD-MMM-YY HH:MM:SS*)
months = (january, february, march, april, may, june, july, august,
  september, october, november, december);
days = (sunday, monday, tuesday, wednesday, thursday, friday, saturday);

CONST
dt_zero: dt_rcd = (1858, ORD(november), 17, 0, 0, 0);
dt_time: SET OF dt_fld = [dt_hour, dt_minute, dt_second];
dt_date: SET OF dt_fld = [dt_year, dt_month, dt_day];
month_str: ARRAY [months] OF STRING[9] =
  ('JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY','AUGUST',
   'SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER');
day_str: ARRAY [DAYS] OF STRING[9] =
  ('SUNDAY','MONDAY','TUESDAY','WEDNESDAY','THURSDAY','FRIDAY','SATURDAY');

$ENABLE dtutltyp
$ENDIF
 