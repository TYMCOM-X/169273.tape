:LOGFILE gen.log
del *.dcl,*.rel
makdcl
auxlib
flib
flimit
licens
mailib
mailog
ontlib
:ESCAPE
com @LIBS
load @DOMAIN
load @EXEC
load @GOMAIL
load @MAILER
load @MAILST
load @MCAUTO
load @MEP
load @ONTMTP
load @ONTYME
load @RDMAIL
load @ROUTER
load @SENDMA
load @SIGNUP
load @SMTP
load @SMTPX
load @TMS
load @TUMS
DOMAIN
ROUTER
  