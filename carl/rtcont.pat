"/ Patch to jump around misplaced code - RTCONT in SCNSER /

SCNSER:

PAT+0/CAIN T1,(F)
PAT+1/JRST RTATS1
PAT+2/JRST RTCONT+5

RTATS+4/JRST PAT

PAT+3/PAT:
PATSIZ/PAT
PATMAP/Q 100000000000
 