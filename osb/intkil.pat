 "\ Patch #3 for P035/B and P035/B04 - INTKIL.PAT 17-May-87 JMS \
 "\ Preserve J in the inactivity-timeout killer \
CLOCK1:
PAT/PUSH P,J
PAT+1/PUSHJ P,HNGMON
PAT+2/JFCL
PAT+3/POP P,J
PAT+4/JRST INACT1
INTKIL+2/JRST PAT
PAT+5/PAT:
PATSIZ/PAT
COMMON:
PATMAP[Q+040000000000
CONFIG+2T/  