;Copy this file to your directory then "PCOM JOBS.CTL"
GFD
COPY (PJ)JOBS.DAT,JOBS.DAT
RUN (MPL)FILDDT
JOBS.DAT/D/P
"/     B/<BUBB:
"/     C/<CUPC:
"/     D/<DALLAS:
"/     F/<FREMONT:
"/     H/<HOUSTON:
"/     M/<MALINKRONDT:
"/     S/<CEGI:
"/     T/<TEST:
"/     W/<RANIER:
"/     X/<LAB:
"/     */<MAINT:
"/     -/<GONE:
15/  2000,,8.
16/     0+TEST   ,,12.
17/  1000+FREMONT,,22.
20/  1000+DALLAS ,,23.
21/     0+TEST   ,,24.
22/  1000+DALLAS ,,25.
23/  1000+CUPC   ,,26.
24/  1000+DALLAS ,,27.
25/  1000+FREMONT,,28.
26/  1000+CUPC   ,,29.
27/  1000+FREMONT,,30.
30/  1000+DALLAS ,,31.
31/     0+TEST   ,,32.
32/  1000+CUPC   ,,33.
33/  5000+DALLAS ,,34.
34/  1000+DALLAS ,,35.
35/     0+BUBB   ,,36.
36/  1000+DALLAS ,,37.
37/  1000+FREMONT,,38.
40/     0+BUBB   ,,39.
41/  1000+DALLAS ,,54.
42/  1000+CUPC   ,,55.
43/  1000+DALLAS ,,56.
44/  1000+DALLAS ,,57.
45/  1000+FREMONT,,58.
46/     0+CEGI   ,,59.
47/     0+CEGI   ,,60.
50/  1000+LAB    ,,62.
51/  1000+DALLAS ,,65.
52/  1000+CUPC   ,,70.
53/     0+GONE   ,,72.
54/ 11000+FREMONT,,74.
55/     0+GONE   ,,79.
56/     0+CEGI   ,,83.
57/     0+CEGI   ,,90.
60/     0+CEGI   ,,92.
61/  1000+LAB    ,,95.
62/     0+MAINT  ,,107.
63/     0+MAINT  ,,108.
64/     0+GONE   ,,118.
65/     0+MALLINKRONDT,,169.
66/     0+CEGI   ,,170.
67/     0+CEGI   ,,184.
70/     0+CEGI   ,,264.
71/     0+CEGI   ,,301.
72/     0+HOUSTON,,370.
73/     0+CEGI   ,,443.
74/     0+GONE   ,,633.
75/     0+GONE   ,,897.
76/  1000+LAB    ,,930.
77/     0+RANIER ,,1051.
100/400000,,1.
101/-1

;System 1 must come last, since MHX stops at first negative number
DECLAR ALL RD RD JOBS.DAT
DELETE (PJ)JOBS.BAK
RENAME (PJ)JOBS.BAK=(PJ)JOBS.DAT
RENAME (PJ)JOBS.DAT=JOBS.DAT
SET TTY WIDTH 80 CRLF
DIRECT (PJ)JOBS.DAT,JOBS.BAK/EVERYTHING
COPY JOBS.CTL,(PJ)SAME
DELETE JOBS.CTL
