:LOGFILE exevue.log
:Parameters start-address=730000
:com vuelnk.ctl "/set:.high.:\start-address XVUE/ssave ="
:escape
r filex
vue.exe=xvue.shr
:escape
delete xvue.shr,xvue.low
