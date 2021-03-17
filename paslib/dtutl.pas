$TITLE DTUTL.PAS, last modified 2/28/84, zw
MODULE dtutl;
(*TYM-Pascal Day Time Utility*)

$HEADER DTUTL.HDR

$INCLUDE DTUTL.TYP

PUBLIC VAR dtstatus: dt_status := dt_ok;

PUBLIC FUNCTION scnmonth(STRING[*]; VAR INTEGER; VAR months): BOOLEAN;
(*try to scan month of year by name from string, update string cursor*)
BEGIN
END;

PUBLIC FUNCTION scnday(STRING[*]; VAR INTEGER; VAR days): BOOLEAN;
(*try to scan day of week by name from string, update string cursor*)
BEGIN
END;

PUBLIC FUNCTION scndt
  (STRING[*]; VAR INTEGER; VAR dt_rcd; SET OF dt_fld): BOOLEAN;
(*try to scan day time from string to record, update string cursor*)
BEGIN
END;

PUBLIC FUNCTION strdt(dt_str; SET OF dt_fld): dt_rcd;
(*convert day time string to specified field(s) in record format*)
BEGIN
END;

PUBLIC FUNCTION dtstr(dt_rcd; SET OF dt_fld): dt_str;
(*convert day time record field(s) to string format*)
BEGIN
END;

PUBLIC FUNCTION dtadv(dt: dt_rcd; val: INTEGER; fld: dt_fld): dt_rcd;
(*advance day time by value in specified field units*)
VAR year_adv, month_adv, day_adv, hour_adv, minute_adv, second_adv: INTEGER;
BEGIN
  dtadv := dt;
  WITH dtadv DO CASE fld OF
    dt_year
    dt_month
    dt_day
    dt_hour
    dt_minute
    dt_second
  END
END;

PUBLIC FUNCTION dtdif(dt1, dt2: dt_rcd; fld: dt_fld): INTEGER;
(*return difference between day times in specified field units*)
VAR year_dif, month_dif, day_dif, hour_dif, minute_dif, second_dif: INTEGER;
BEGIN
  year_dif := dt1.year - dt2.year;
  IF fld = dt_year THEN dtdif := year_dif
  ELSE BEGIN
    month_dif := dt1.month - dt2.month + 12 * year_dif;
    IF fld = dt_month THEN dtdif := month_dif
    ELSE BEGIN
    END
  END
END;

PUBLIC FUNCTION dtcmp(dt1, dt2: dt_rcd): BOOLEAN;
(*return TRUE if first day time before second*)
BEGIN
  dtcmp := TRUE;
  WITH dt1 DO
  IF year <> dt2.year THEN dtcmp := year < dt2.year
  ELSE IF month <> dt2.month THEN dtcmp := month < dt2.month
  ELSE IF day <> dt2.day THEN dtcmp := day < dt2.day
  ELSE IF hour <> dt2.hour THEN dtcmp := hour < dt2.hour
  ELSE IF minute <> dt2.minute THEN dtcmp := minute < dt2.minute
  ELSE dtcmp := second < dt2.second
END;

PUBLIC FUNCTION dtpak(dt: dt_rcd): INTEGER;
(*return packed day time from record format*)
VAR pak: dt_pak;
BEGIN
  pak.days := dtdif(dt_zero, dt, dt_day);
  pak.seconds := dt.second + (dt.minute + dt.hour * 60) * 60;
  dtpak := pak.number
END;

PUBLIC FUNCTION pakdt(dt: INTEGER): dt_rcd;
(*return record format from packed day time*)
VAR pak: dt_pak;
BEGIN
  pak.number := dt;
  pakdt := dtadv(dtadv(dt_zero, pak.seconds, dt_second), pak.days, dt_day)
END;

PUBLIC FUNCTION dt: dt_rcd;
(*return current day time record*)
BEGIN
  dt := strdt(DATE() || ' 00:00:00', []);
  dt := dtadv(dt, TIME() DIV 1000, dt_second)
END.
