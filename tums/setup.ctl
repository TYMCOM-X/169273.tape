:logfile SETUP.LOG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Setup system for TUMS:
;    Enter MAILER in (SYS)XEXEC.CTL
;    Setup (MAIL) parameters correctly.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
R EDITOR
R (SYS)XEXEC.CTL
F"MAILER"D
NA
1,MAIL,(MAIL)MAILER
EX (SYS)XEXEC.CTL
Y
R SETLIC
(SYS)XEXEC.CTL,WC JL
R NONAME
CHANGE MAIL
6
Y
MAIL
SMTP
Y
Y
13
N
Y
Y
17
Y
Y
N
QUIT
  