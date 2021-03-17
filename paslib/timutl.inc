$PAGE TIMUTL.INC, last modified 2/23/84, zw
$IFNOT timutlinc

$SYSTEM TIMUTL.TYP

EXTERNAL FUNCTION timrcd(time_int): time_rcd;
EXTERNAL FUNCTION dtrcd(day_time_int): day_time_rcd;
EXTERNAL FUNCTION timext(day_time_int): day_time_ext;
EXTERNAL FUNCTION daytime: day_time_int;
EXTERNAL PROCEDURE ec_ext(VAR timutl_err; ns_ext; VAR day_time_int);
EXTERNAL FUNCTION extr_date(day_time_int): date_int;
EXTERNAL FUNCTION extr_time(day_time_int): time_int;
EXTERNAL FUNCTION dt_combine(date_int; time_int): day_time_int;
EXTERNAL FUNCTION days_diff(day_time_int; day_time_int): days;
EXTERNAL FUNCTION secs_diff(day_time_int; day_time_int): seconds;
EXTERNAL FUNCTION add_days(day_time_int; days): day_time_int;
EXTERNAL FUNCTION add_secs(day_time_int; seconds): day_time_int;
EXTERNAL FUNCTION day_of_week(date_int): week_day;
EXTERNAL FUNCTION ns_d1(date_int): ns_date1;
EXTERNAL FUNCTION ns_d2(date_int): ns_date2;
EXTERNAL FUNCTION ns_t1(time_int): ns_time1;
EXTERNAL FUNCTION gmday_time: day_time_int;
EXTERNAL PROCEDURE ec_day_time(VAR timutl_err; day_time_rcd; VAR day_time_int);
EXTERNAL PROCEDURE ec_date(VAR timutl_err; date_rcd; VAR date_int);
EXTERNAL FUNCTION dc_date(date_int): daterec;
EXTERNAL PROCEDURE ec_time(VAR timutl_err; time_rcd; VAR time_int);
EXTERNAL FUNCTION ec_dcdate(dec_date): date_int;
EXTERNAL FUNCTION ec_tsdate(tym_date): date_int;
EXTERNAL FUNCTION ec_dctime(dec_time): time_int;

$ENABLE timutlinc
$ENDIF

