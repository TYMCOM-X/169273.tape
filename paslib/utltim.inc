(*utltim.inc, last modified 5/10/83, zw*)
$IFNOT utltiminc

$INCLUDE utltim.typ

EXTERNAL FUNCTION daytime: dtime_int;
EXTERNAL FUNCTION dc_ext(dtime: dtime_int): dtime_ext;
EXTERNAL PROCEDURE ec_ext(var err_code: dtime_err;
   dt_ext: ns_ext; var time_: dtime_int);
EXTERNAL FUNCTION extr_date(dtime: dtime_int): date_int;
EXTERNAL FUNCTION extr_time(dtime: dtime_int): time_int;
EXTERNAL FUNCTION dt_combine(date: date_int; time: time_int): dtime_int;
EXTERNAL FUNCTION days_diff(dtime1,dtime2: dtime_int): days;
EXTERNAL FUNCTION secs_diff(dtime1,dtime2: dtime_int): seconds;
EXTERNAL FUNCTION add_days(dtime: dtime_int;
   num_days: days): dtime_int;
EXTERNAL FUNCTION add_secs(dtime: dtime_int;
   num_secs: seconds): dtime_int;
EXTERNAL FUNCTION day_of_week(date: date_int): week_day;
EXTERNAL FUNCTION ns_d1(date: date_int): ns_date1;
EXTERNAL FUNCTION ns_d2(date: date_int): ns_date2;
EXTERNAL FUNCTION ns_t1(time: time_int): ns_time1;
EXTERNAL FUNCTION gmdtime: dtime_int;
EXTERNAL FUNCTION dc_dtime(dtime: dtime_int): dtimerec;
EXTERNAL PROCEDURE ec_dtime(var err_code: dtime_err; dtimebin: dtimerec;
   var dtime: dtime_int);
EXTERNAL PROCEDURE ec_date(var err_code: dtime_err;
   date_bin: daterec; var date: date_int);
EXTERNAL FUNCTION dc_date(date: date_int): daterec;
EXTERNAL PROCEDURE ec_time(var err_code: dtime_err;
   time_bin: timerec; var time: time_int);
EXTERNAL FUNCTION dc_time(time: time_int): timerec;
EXTERNAL FUNCTION ec_dcdate(d_date: dec_date): date_int;
EXTERNAL FUNCTION ec_tsdate(t_date: tym_date): date_int;
EXTERNAL FUNCTION ec_dctime(d_time: dec_time): time_int;

$ENABLE utltiminc
$ENDIF
(*end of utltim.inc*)
 