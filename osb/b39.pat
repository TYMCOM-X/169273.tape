 "\Patches for B39-P034/P-16.  Patch 39034P.SAV with PMON16.PAT first.\
 "\PATCH #15 is MAPSUP.PAT, PATCH #16 is DAYLIT.PAT\
 "\As of 15-May-85, only patches up thru #16 have been applied.\

 "\PATCH #17 INSSIM.PAT 24-Oct-86 JMS\
 "\Fix typo in INSSIM (T3 should be T4)\
UUOCON:
INSIM1+2/JRST @SIMTBL(T4)
 "\Make BADSAT typeout not go off edge of paper\
ERRCON:
BUGTY1+31/PUSHJ P,PRT22A
COMMON:
PATMAP[Q+000001000000

 "\PATCH #18 FILAMO.PAT 12-Dec-86 JMS\
 "\Move GETCBR to before GETATB to prevent FILAMO crashes\
FILFND:
DECRMV+3/PUSHJ P,GETCBR
DECRMV+4/PUSHJ P,GETATB
DECRMV+5/MOVE P2,T1
COMMON:
PATMAP[Q+400000

 "\PATCH #19 CTYLOG.PAT 12-Dec-86 JMS\
 "\Log CTY output in buffer.  KL ONLY!\
COMMOD:
CHNUNC-1000/CTYBUF:
CHNUNE+1/CTYPTR:440700,,0
CTYPTR+1/CTYLOG:
SCNSER:
CTYLOG+0/PUSH P,T1
CTYLOG+1/MOVE T1,CTYPTR
CTYLOG+2/IBP T1
CTYLOG+3/TRZ T1,777000
CTYLOG+4/MOVEM T1,CTYPTR
CTYLOG+5/ADDI T1,CTYBUF
CTYLOG+6/DPB T3,T1
CTYLOG+7/POP P,T1
CTYLOG+10/MOVEM T3,SPCTRY
CTYLOG+11/JRST CTYCHR+1
CTYCHR/JRST CTYLOG
COMMON:
CONFIG+107[CTYPTR,,CTYBUF
PATMAP[Q+200000

      