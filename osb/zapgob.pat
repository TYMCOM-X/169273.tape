 "\Patch #12 ZAPGOB.PAT 2-Jul-86 JMS \
 "\Cancel the "need to send gobbler" flag when sending ZAP to TYMNET\
 "\Ignore bad port number on message type 3, ignore message type 3\
SCNSER:
NOTLIN+10/SETZB T1,LDBOUT(U)
BADPRT+2/CAIN P1,3
BADPRT+3/JRST CNTPRT
TYPTRN+3/AOJA P4,SCNIN1
COMMON:
PATMAP[Q+000040000000
CONFIG+2T/
      