:logfile maigen.log
del mailib.rel,mailog.rel,ontlib.rel,smtp.rel,smtlib.rel,mailer.rel
del arpa.rel,tymix.rel,tymx.rel,bubbx.rel,ontyme.rel,flib.rel
del ontmtp.rel,rdmail.rel,sendma.rel,tums.rel,eunix.rel
run makdcl
flib
mailib
mailog
ontlib
smtlib
:escape
com mailib(,rr),mailog,ontlib,smtlib,flib
load @SMTP
load @MAILER
load @ARPA
load @TYMIX
load @EUNIX
load @TYMX
load @BUBBX
load @ONTYME
load @ONTMTP
load @RDMAIL
load GOMAIL
load @TUMS
  