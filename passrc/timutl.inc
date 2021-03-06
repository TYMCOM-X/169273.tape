$PAGE TIMUTL.INC, last modified 4/9/84, zw
$IFNOT timutlinc
(*TYM-Pascal day/time utility*)

(*HEADER TIMUTL.HDR*)

$SYSTEM TIMUTL.TYP
EXTERNAL FUNCTION dc_date(date_int): daterec;
EXTERNAL FUNCTION dc_time(time_int): timerec;
EXTERNAL FUNCTION dc_dtime(dtime_int): dtimerec;
EXTERNAL FUNCTION dc_ext(dtime_int): dtime_ext;
EXTERNAL PROCEDURE ec_time(VAR dtime_err; timerec; VAR time_int);
EXTERNAL PROCEDURE ec_date(VAR dtime_err; daterec; VAR date_int);
EXTERNAL PROCEDURE ec_dtime(VAR dtime_err; dtimerec; VAR dtime_int);
EXTERNAL PROCEDURE ec_ext(VAR dtime_err; ns_ext; VAR dtime_int);
EXTERNAL FUNCTION ns_d1(date_int): ns_date1;
EXTERNAL FUNCTION ns_d2(date_int): ns_date2;
EXTERNAL FUNCTION ns_t1(time_int): ns_time1;
EXTERNAL FUNCTION ec_dctime(dec_time): time_int;
EXTERNAL FUNCTION ec_dcdate(dec_date): date_int;
EXTERNAL FUNCTION ec_tsdate(tym_date): date_int;
EXTERNAL FUNCTION day_of_week(date_int): week_day;
EXTERNAL FUNCTION daytime: dtime_int;
EXTERNAL FUNCTION gmdtime: dtime_int;
EXTERNAL FUNCTION extr_date(dtime_int): date_int;
EXTERNAL FUNCTION extr_time(dtime_int): time_int;
EXTERNAL FUNCTION dt_combine(date_int; time_int): dtime_int;
EXTERNAL FUNCTION days_diff(dtime_int; dtime_int): days;
EXTERNAL FUNCTION secs_diff(dtime_int; dtime_int): seconds;
EXTERNAL FUNCTION add_days(dtime_int; days): dtime_int;
EXTERNAL FUNCTION add_secs(dtime_int; seconds): dtime_int;

$ENABLE timutlinc
$ENDIF
   