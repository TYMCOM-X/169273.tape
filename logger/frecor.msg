To:  fletcherc
CC:  osman, carl
Subject: FRECOR numbers

Craig,

  The results of our investigation so far is that we have some
conflicting yet interesting values for FRECOR for various memory sizes
for the /K monitor, and we show the reduction in actual available free
core for additional memory with the /L & /N monitors.

  The following table shows our sample of various CPU types with various
monitors.  FRECOR is the hard-coded value, BLKS is <FRECOR div ^D145>,
*145 is the number of blocks in FREPTR times ^D145, FREPTR is an AOBJN
word showing -<#blocks>,,<start addr> for monitor free core and MEMSIZ
is the physical memory size on-line for each machine.

    CPU SYS-MONITOR   FRECOR  BLKS * 145.     FREPTR     MEMSIZ

    KI C 34-P034/K-15  57722   251  60554  -254,,521215    2
    KI C 57-P034/K-13 125322   455 125416  -456,,606142    3

    KI D 23-P034/L25       0     0 266525 -1205,,463237    1

    KL F 28-P034/K-12 126762   463 127405  -465,,534231    1.5
    KL F 74-P034/K-12  77422   340 100223  -343,,521553    1.5
    KL D 56-P034/K-16 126762   463 127164  -464,,612516    3
    KL C 55-P034/K-13 103400   356 103537  -357,,644020    4

    KL D 37-P034/N         0     0 264536 -1176,,426203    1
    KL C 36-P034/N04       0     0 350626 -1466,,400000    1.75
    KL C 33-P034/L21-1     0     0 265200 -1200,,464407    2
    KL C 26-P034/N04       0     0 270735 -1215,,457623    2
    KL C 39-P034/L25-1     0     0 210161  -741,,541404    4

  We have not yet found the amount of FRECOR needed per file or
per frame (for MAGNUM), and thus do not yet have a suggested value
for LOGMAX for system 56.  I will let you know as soon as that
information is available.

/Carl
   