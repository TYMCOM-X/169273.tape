SYS:RAID
PROD.=HEL./nolist/noload/nonstandard PTRAN
RESTAB.=PROD.QQQ/forward+FOO2./nolist/noload/nonstandard RTRAN
%S%B
/fail FTSAIL=FTSASW.FAI+HEAD.+SAIL.+PARSE.;
	+FOO2.+PROD./forward+DATA.+RESTAB./forward+ENDDAT.+SYM.;
	+GEN.+ARRAY.+EXPRS.+STATS.+LEAP.+TOTAL.+PROCSS.+COMSER
DSK:HLBSA9/library

   