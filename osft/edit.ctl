:$SUBSTITUTION=1
:PARAMETERS MONTH-NUM=\$MM\,MONTH-DAY=\$DAY\,MONTH-DAYS=1,MONTH-NAME=\$MON\,YEAR=\$YY\
;
; Parameters = Month, Day, Days, Name_of_Month, Year
;               \Month-Num\     \Month-Day\    \Month-Days\       "\Month-Name\"           \Year\
;
do scream
file
scan.log


ct1,ct4,ded,dvue,edit10,editor,ftpeak,peak,sed,teco,tvedit,vue,xvue





run
\MONTH-NUM\,\MONTH-DAY\,\YEAR\
\MONTH-DAYS\
n
do redscr
scan.log
(VUE)yeux\YEAR\.\MONTH-NAME\
