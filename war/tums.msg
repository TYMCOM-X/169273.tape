From: Joe Smith <JMS> 
Date: Sat, 3 Oct 87 10:23:29 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, danielsr@74, richardson 
Subject: (OSP)33035C.SAV rebuilt to handle 4 logical strings. 

I changed CONF33.MAC in M33 and OSP to have "XP DISKS,^D<4*8>" so it could
use all four logical strings (2 physical strings) for 3652 disks.  Note
that 2*8 defines BPA0-7 and BPC0-7 while 4*8 defines BPA0-17 and BPC0-17 when
DSKTYP='3652B'.

There are 4 generic 4-string monitors in SYS on all systems.  They are:
KL3652.SAV, KI3652.SAV, KL3675.SAV, KI3675.SAV.  Carl had Rick use KL3652
(which is stored in (M33:32)KLXC01.SAV) to do the add pack on F33.

/Joe

  