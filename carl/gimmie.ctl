:LOGFILE gimmie.log
run gimmie
copy (carl)check.pat,(carl)check.ctl
copy (carl)xexec.pat,(sys)xexec.ctl
r setlic
(sys)xexec.ctl,wc
dir (sys)xexec.*/ev
  