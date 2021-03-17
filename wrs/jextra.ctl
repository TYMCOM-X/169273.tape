:logfile jextra.log
:time 600
r(xexec)minit
jextract joshua.dic
trim joshua.dic=joshua.dic
sort joshua.dic=joshua.dic/RECLEN:40
trim joshua.dic=joshua.dic
uniq joshua.dic=joshua.dic
