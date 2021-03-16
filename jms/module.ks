!find rh11 lp10 dz11 kmc11 dr11 kl  dl11 drv11 (mondoc0 )*.*,(m33)*.*/o

(MONDOC)P034P.XMT        
{Page 1}
FOGCEN   DR11~C.MAC

{Page 2}
FILFND,CTYSIM,ONCE,DZKON,DR11~C,KMCSER,COMCON,F3TSER,MAGSER,REFSTR:

(MONDOC)P034P.MEM        
{Page 1}
FILFND,CTYSIM,ONCE,DZKON,DR11~C,KMCSER,COMCON,F3TSER,MAGSER,REFSTR:

(MONDOC)PDP700.TXT        
{Page 2}
 124=LPT  Lineprinter, LP10~

(MONDOC)HARDWR.NFO        
{Page 3}
The 2020 uses standard DEC hardware - a TU45 connected to an RH11~.

(MONDOC)DISKS.BAK        
{Page 1}
*(MONDOC)DISKS.NFO:DECDISK;Commands to RH11~ for RP06/RM03 on 2020.

{Page 5}
$text DECDISK;RP06/RM03/RH11~ on 2020.
The RH11~ disk controller in the 2020 can handle any combination RP04/RP06/RM03
automatically.  The PDP-11 console front-end on the KL also has an RH11~, for

(MONDOC)DISKS.NFO        
{Page 1}
*(MONDOC)DISKS.NFO:DECDISK;Commands to RH11~ for RP06/RM03 on 2020.

{Page 5}
$text DECDISK;RP06/RM03/RH11~ on 2020.
The RH11~ disk controller in the 2020 can handle any combination RP04/RP06/RM03
automatically.  The PDP-11 console front-end on the KL also has an RH11~, for

(MONDOC)ROOT.NLS        
{Page 141}
DZKON (support for DZ11~ driver, used when no network is connected
to the 20), DR11~C (network base ring simulator), CIOPR

{Page 144}
for RM units, copies the RH11~ registers into the proper unit data block
DR11~C:  Fixed bug which caused block mode output header message to be dropped

{Page 147}
PICON,COMMON,DR11~C,DZKON,CTYSIM: KS change. Now a second "SH"

{Page 165}
CIOPR:,DR11~C:,DZKON:,KSIORG:,MAGSER: These are KS specific modules which

(MONDOC)P034L.MEM        
{Page 1}
CTYSIM,CIOPR,KMCSER,DR11~C,DZKON,ONCE: Removed SEXTERNAL references to

(MONDOC)P034N.MEM        
{Page 1}
CTYSIM,CIOPR,KMCSER,DR11~C,DZKON,ONCE: Removed SEXTERNAL references to

(MONDOC)MODULE.DOC        
{Page 1}
        DR11~C          - DR11~-C  KS communications device

(M33)CIOPR.MAC        
{Page 1}
;DR11~C hardware parameters.
;Macro to make a DR11~c known to the system.
DEFINE DR11~C(BASEADDRESS,VECTORBASE),<
DR11~C(767770,300/4)
DR11~C(767760,310/4)            ; **jms4480**
DR11~C(767750,320/4)            ; **jms4480**
DR11~C(767740,330/4)            ; **jms4480**

{Page 2}
        PUSHJ   P,DLSEDR        ;1 PREPARE DR11~C FOR DATA.
        PUSHJ   P,DLWDR         ;2 WRITE WORD TO DR11~C.
        PUSHJ   P,DLRDR         ;3 READ WORD FROM DR11~C.
        PUSHJ   P,DLDDR         ;4 RELEASE DR11~C TO KMC & NEXILIS NODE.
; ARG 0: INTERFACE TYPE: 0=CTYSIM, 1=DZKON, 2=TYMBASE NODE, 3=DR11~C
        PUSHJ   P,DZREM##       ;TURN OFF DZ11~S.
;Install DR11~C interface to NEXILIS node
;DLWDR (FUNCTION 2) - WRITE WORD TO SELECTED DR11~C.
; ARG 0: DR11~C#,,16 BIT WORD TO SEND
DLWDR:  PUSHJ   P,DLSDR         ;GET DR11~C BASE ADDRESS.
;DLRDR (FUNCTION 3) - READ WORD FROM SELECTED DR11~C.
; ARG 0: DR11~C #,,ADR TO RECEIVE WORD READ.
DLRDR:  PUSHJ   P,DLSDR         ;GET DR11~C BASE ADDRESS.
;DLSEDR (FUNCTION 1) - SELECT DR11~C FOR USE BY THE 2020.
; ARG 0: DR11~C #,,0
DLSEDR: PUSHJ   P,DLSDR         ;GET HANDLES ON DR11~C
        HRRZ    T4,DRFLTB(T2)   ;SET DR11~C READY FOR OUR OUTPUT FLAG
;DLDDR (FUNCTION 4) - DESELECT DR11~C.
; ARG 0: DR11~C #,,0
DLDDR:  PUSHJ P,DLSDR           ;GET HANDLES ON DR11~C
         PUSHJ P,DLIDR          ;REINSTALL DR11~C DRIVER
;DLSDR.  LOAD P1 WITH BASE ADDRESS OF DR11~C SPECIFIED BY USER.
; IN ARG 0.  SKIP IF LEGAL DR11~C, SINGLE RETURN OTHERWISE.
; T3.RH HAS ARG0.RH.  T2 HAS DR11~C NUMBER.
        HLRZ T2,T3              ;DR11~C NUMBER
         JRST DLE0              ;ILLEGAL DR11~C NUMBER.
        MOVE P1,DRBATB(T2)      ;T2:=DR11~C BASE ADDRESS.

{Page 3}
        PUSHJ   P,DLSEDR        ;1 PREPARE DR11~C FOR DATA.
        PUSHJ   P,DLWDR         ;2 WRITE WORD TO DR11~C.
        PUSHJ   P,DLRDR         ;3 READ WORD FROM DR11~C.
        PUSHJ   P,DLDDR         ;4 RELEASE DR11~C TO KMC & NEXILIS NODE.

(M33)SCNSER.A02        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

(M33)P034P.XMT        
{Page 1}
FOGCEN   DR11~C.MAC

{Page 2}
FILFND,CTYSIM,ONCE,DZKON,DR11~C,KMCSER,COMCON,F3TSER,MAGSER,REFSTR:

(M33)MONS2.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)SCNSER.A03        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

(M33)SCNSER.A04        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

(M33)SCNSER.A14        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

(M33)SCNSER.B01        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

(M33)SCNSER.NEW        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

(M33)P034P.MEM        
{Page 1}
FILFND,CTYSIM,ONCE,DZKON,DR11~C,KMCSER,COMCON,F3TSER,MAGSER,REFSTR:

(M33)40867.TID        
{Page 1}
  227  2   DR11~CS.REL     M33           14:08 07-DEC-79  03:35 04-JAN-80    3 C
AKNAX

(M33)NEW.DIR        
{Page 1}
DR11~CS REL     3  ALL RD  NO    2-Jul-86

(M33)P034Q.DOC        
{Page 1}
    The 2020 still uses type 3 to determine how many DR11~s are on the KMC-11.

(M33)CHKPNT.MAC        
{Page 9}
  NRSW==11      ;NUMBER OF RH11~/RM03 STATUS WORDS SAVED.
  TYPRM==5      ;; 5    RM (RH11~C CONTROLLER FOR RM03 UNITS)

(M33)MONS3.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)MONRB.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)CPUS.ALL        
{Page 1}
MACRO: DR11~C

(M33)P034.MEM        
{Page 15}
DZKON (support for DZ11~ driver, used when no network is connected
to the 20), DR11~C (network base ring simulator), CIOPR
for RM units, copies the RH11~ registers into the proper unit data block
DR11~C:  Fixed bug which caused block mode output header message to be dropped

{Page 16}
PICON,COMMON,DR11~C,DZKON,CTYSIM: KS change. Now a second "SH"

{Page 19}
CIOPR:,DR11~C:,DZKON:,KSIORG:,MAGSER: These are KS specific modules which

{Page 24}
CTYSIM,CIOPR,KMCSER,DR11~C,DZKON,ONCE: Removed SEXTERNAL references to

{Page 27}
FILFND,CTYSIM,ONCE,DZKON,DR11~C,KMCSER,COMCON,F3TSER,MAGSER,REFSTR:

{Page 28}
        2020, since it uses message type 3 to determine how many DR11~s are

(M33)CHKPNT.OLD        
{Page 19}
  NRSW==11      ;NUMBER OF RH11~/RM03 STATUS WORDS SAVED.
  TYPRM==5      ;; 5    RM (RH11~C CONTROLLER FOR RM03 UNITS)

(M33)A14SCM.SCM        
{Page 1}
1)25    MAPALC(DRI)             ;DR11~C BLOCK INPUT PAGE
1)      MAPALC(DRO)             ;DR11~C BLOCK OUTPUT PAGE
2)25    ;*;MAPALC(DRI)          ;DR11~C BLOCK INPUT PAGE  (never referenced)
2)      ;*;MAPALC(DRO)          ;DR11~C BLOCK OUTPUT PAGE (never referenced)

{Page 3}
2)29    CNFCHN::EXP     CHNN##  ;(72) NUMBER OF DATA CHANNELS (DF10 or RH11~S or
 F3)

(M33)A14SCM.LOG        
{Page 1}
1)25    MAPALC(DRI)             ;DR11~C BLOCK INPUT PAGE
1)      MAPALC(DRO)             ;DR11~C BLOCK OUTPUT PAGE
2)25    ;*;MAPALC(DRI)          ;DR11~C BLOCK INPUT PAGE  (never referenced)
2)      ;*;MAPALC(DRO)          ;DR11~C BLOCK OUTPUT PAGE (never referenced)

{Page 8}
*=DR11~C.MAC[3,140674],DR11~C.MAC[3,42754]/C

(M33)A14SCM.CTL        
{Page 1}
=DR11~C.MAC[3,140674],DR11~C.MAC[3,42754]/C

(M33)DZKON.MAC        
{Page 1}
TITLE DZKON - Simulate a Tymnet Base and Remote using DZ11~s and the CTY
;DZ-11 register offsets, bit assignments.  See DZ11~ user's guide
; (EK-DZ11~0-UG-001) for semantics.

{Page 6}
TTACT:  0       ;-1 if CTY or DZ11~ output is in progress, 0 otherwise.
;Base addresses of the DZ11~s on the system.  The code makes a lot of

(M33)MON83.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)MON92.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)MONS4.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)MONMA.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)COMMON.A04        
{Page 29}
CNFCHN::EXP     CHNN##  ;(72) NUMBER OF DATA CHANNELS (DF10 or RH11~S or F3)

{Page 41}
RH1STS: RH11~CA                 ;RPCS1
RH1ST2: RH11~CA+10              ;RPCS2

(M33)COMMON.A14        
{Page 29}
CNFCHN::EXP     CHNN##  ;(72) NUMBER OF DATA CHANNELS (DF10 or RH11~S or F3)

{Page 41}
RH1STS: RH11~CA                 ;RPCS1
RH1ST2: RH11~CA+10              ;RPCS2

(M33)CIOPRS.LST        
{Page 3}
    59                          ;DR11~C hardware parameters.

{Page 4}
   143                          ;Macro to make a DR11~c known to the system.
   151                          DEFINE DR11~C(BASEADDRESS,VECTORBASE),<

{Page 5}
   181                          DR11~C(767770,300/4)^
   197                          DR11~C(767760,310/4)^

{Page 6}
   213                          DR11~C(767750,320/4)^
   229                          DR11~C(767740,330/4)^

{Page 9}
   371  000070' 260040  000264'         PUSHJ   P,DLSEDR        ;1 PREPARE DR11~
C FOR DATA.
   372  000071' 260040  000231'         PUSHJ   P,DLWDR         ;2 WRITE WORD TO
 DR11~C.
   373  000072' 260040  000246'         PUSHJ   P,DLRDR         ;3 READ WORD FRO
M DR11~C.
   374  000073' 260040  000312'         PUSHJ   P,DLDDR         ;4 RELEASE DR11~
C TO KMC & NEXILIS NODE.
   385                          ; ARG 0: INTERFACE TYPE: 0=CTYSIM, 1=DZKON, 2=TY
MBASE NODE, 3=DR11~C

{Page 10}
   453  000155' 260040  000000*         PUSHJ   P,DZREM##       ;TURN OFF DZ11~S
.

{Page 11}
   486                          ;Install DR11~C interface to NEXILIS node
   517                          ;DLWDR (FUNCTION 2) - WRITE WORD TO SELECTED DR1
1~C.
   518                          ; ARG 0: DR11~C#,,16 BIT WORD TO SEND
   519  000231' 260040  000335' DLWDR:  PUSHJ   P,DLSDR         ;GET DR11~C BASE
 ADDRESS.

{Page 12}
   533                          ;DLRDR (FUNCTION 3) - READ WORD FROM SELECTED DR
11~C.
   534                          ; ARG 0: DR11~C #,,ADR TO RECEIVE WORD READ.
   535  000246' 260040  000335' DLRDR:  PUSHJ   P,DLSDR         ;GET DR11~C BASE
 ADDRESS.
   550                          ;DLSEDR (FUNCTION 1) - SELECT DR11~C FOR USE BY 
THE 2020.
   551                          ; ARG 0: DR11~C #,,0
   552  000264' 260040  000335' DLSEDR: PUSHJ   P,DLSDR         ;GET HANDLES ON 
DR11~C
   571  000307' 550447  000057'         HRRZ    T4,DRFLTB(T2)   ;SET DR11~C READ
Y FOR OUR OUTPUT FLAG
   575                          ;DLDDR (FUNCTION 4) - DESELECT DR11~C.
   576                          ; ARG 0: DR11~C #,,0

{Page 13}
   577  000312' 260040  000335' DLDDR:  PUSHJ P,DLSDR           ;GET HANDLES ON 
DR11~C
   591  000330' 260040  000210'          PUSHJ P,DLIDR          ;REINSTALL DR11~
C DRIVER
   597                          ;DLSDR.  LOAD P1 WITH BASE ADDRESS OF DR11~C SPE
CIFIED BY USER.
   598                          ; IN ARG 0.  SKIP IF LEGAL DR11~C, SINGLE RETURN
 OTHERWISE.
   599                          ; T3.RH HAS ARG0.RH.  T2 HAS DR11~C NUMBER.
   601  000336' 554340  000010          HLRZ T2,T3              ;DR11~C NUMBER
   603  000340' 254000  000351'          JRST DLE0              ;ILLEGAL DR11~C 
NUMBER.
   604  000341' 200607  000063'         MOVE P1,DRBATB(T2)      ;T2:=DR11~C BASE
 ADDRESS.

{Page 16}
   694                                  PUSHJ   P,DLSEDR        ;1 PREPARE DR11~
C FOR DATA.
   695                                  PUSHJ   P,DLWDR         ;2 WRITE WORD TO
 DR11~C.
   696                                  PUSHJ   P,DLRDR         ;3 READ WORD FRO
M DR11~C.
   697                                  PUSHJ   P,DLDDR         ;4 RELEASE DR11~
C TO KMC & NEXILIS NODE.

{Page 23}
DR11~C     151#    181     197     213     229

(M33)COMMON.B01        
{Page 41}
RH1STS: RH11~CA                 ;RPCS1
RH1ST2: RH11~CA+10              ;RPCS2

(M33)FIND0N.PTR        
{Page 1}
DR11~C.MAC      

(M33)COMMON.BAK        
{Page 29}
CNFCHN::EXP     CHNN##  ;(72) NUMBER OF DATA CHANNELS (DF10 or RH11~S or F3)

{Page 41}
RH1STS: RH11~CA                 ;RPCS1
RH1ST2: RH11~CA+10              ;RPCS2

(M33)KSCOMM.MAC        
{Page 1}
drdo::0                 ;Don't use the DR11~C driver
drdo::1                 ;Access the DR11~C directly

(M33)MONS5.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)MONTW.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)BOOTS.173        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)BOOTS.175        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)BOOTS.176        
{Page 16}
IFN FT.RP,< CONSO 0,RMINI(R) >  ;RH11~ on 2020

(M33)BOOTS.17X        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)BOOTS.17Y        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)BOOTS.17Z        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)KSCOP.TXT        
{Page 1}
  03800   DR11~CS.REL

(M33)TRWCOP.TXT        
{Page 1}
  03800   DR11~CS.REL

(M33)TRWCOP.CMD        
{Page 1}
   03600   DR11~C.MAC

(M33)RMXKON.426        
{Page 1}
;IN ORDER TO CLEAR THE RH11~C CONTROLLER, A DRIVE WHICH DEFINATELY EXISTS
         CALL SAVSTS            ;READ RH11~ AND DRIVE STATUS INTO UDB.
CONERR: CALL SAVSTS             ;SAVE RH11~ AND DRIVE STATUS IN UDB.

(M33)MONEDP.CMD        
{Page 1}
DR11~C.MAC, (OSP)SAME 

(M33)MONEDP.OSP        
{Page 1}
DR11~C.MAC, (OSP)SAME 

(M33)BOOTS.168        
{Page 18}
  05600   UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED O
N KS.

(M33)COMC.UBR        
{Page 1}
DR11~C.MAC      

(M33)MCOPY.LOG        
{Page 1}
Map file: DR11~C.MAC,(OSP)
DR11~C.MAC ==> (OSP)DR11~C.MAC

(M33)TYPOUT.TXT        
{Page 1}
DR11~C.MAC

(M33)MONED.XMT        
{Page 2}
        2020, since it uses message type 3 to determine how many DR11~s are

(M33)MON60.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)MCOPY.CTL        
{Page 1}
DR11~C.MAC,(OSP)

(M33)FILIO.BAK        
{Page 12}
;efficient when using DF10 or RH11~).  To avoid end-of-track or command list

(M33)DR11C.DOC        
{Page 1}
DR11~C:  Fixed bug which caused block mode output header message to be dropped

(M33)44502.TID        
{Page 1}
   30  2   DR11~C .REL     M33           21:16 22-FEB-80  09:14 02-APR-80    3 C
AKNAX

(M33)MASTL.MFS        
{Page 1}
DR11~C,,I   5,0
DR11~C,53,J   5,1
DR11~C,106,K   5,1
DR11~C,125,L   5,1
DR11~C,178,M   5,2
DR11~C,231,N   5,2
DR11~C,284,B   6,2
DR11~C,286,C   6,2
DR11~C,339,D   6,3
DR11~C,368,E   6,3
DR11~C,421,F   6,4
DR11~C,447,G   6,4
DR11~C,468,H   6,5
DR11~C,499,I   6,6

(M33)KMCDDT.MAC        
{Page 1}
TITLE   KMCDDT  ROUTINES TO ACCESS KMC11~ FOR DEBUGGING
ADR=10          ;KMC11~ DEVICE ADDRESS
;DEFINE SOME KMC11~ MICRO INSTRUCTIONS
;SUBROUTINE TO EXECUTE KMC11~ INSTRUCTION

(M33)KMCSER.MAC        
{Page 3}
;  using DR11~C's.

(M33)MON59.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)MON95.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)S.A08        
{Page 25}
MAPALC(DRI)             ;DR11~C BLOCK INPUT PAGE
MAPALC(DRO)             ;DR11~C BLOCK OUTPUT PAGE

(M33)S.A05        
{Page 25}
MAPALC(DRI)             ;DR11~C BLOCK INPUT PAGE
MAPALC(DRO)             ;DR11~C BLOCK OUTPUT PAGE

(M33)S.XXX        
{Page 25}
;*;MAPALC(DRI)          ;DR11~C BLOCK INPUT PAGE
;*;MAPALC(DRO)          ;DR11~C BLOCK OUTPUT PAGE

(M33)S.B01        
{Page 25}
;*;MAPALC(DRI)          ;DR11~C BLOCK INPUT PAGE  (never referenced)
;*;MAPALC(DRO)          ;DR11~C BLOCK OUTPUT PAGE (never referenced)

(M33)B01SCM.XXX        
{Page 8}
2)      CNFCHN::EXP     CHNN##  ;(72) NUMBER OF DATA CHANNELS (DF10 or RH11~S or
 F3)

{Page 28}
2)      RH1STS: RH11~CA                 ;RPCS1
2)      RH1ST2: RH11~CA+10              ;RPCS2

{Page 60}
1)      RH1STS: RH11~CA                 ;RPCS1
1)      RH1ST2: RH11~CA+10              ;RPCS2

(M33)37841.TID        
{Page 1}
 1546  1   DR11~C .A27     M33           14:06 25-SEP-79  18:08 30-OCT-79    6 N
OBQOS
 1602  1   DR11~C .MAC     M33           18:44 18-OCT-79  18:08 30-OCT-79    8 X
IQNAV
 1603  1   DR11~C .DOC     M33           10:55 19-OCT-79  18:08 30-OCT-79    2 C
ULNUD
 2035  1   DR11~CS.REL     M33           18:47 18-OCT-79  18:08 30-OCT-79    3 R
EHGOQ
 2706  1   DR11~C .A27     M33           14:06 25-SEP-79  18:26 30-OCT-79    6 N
OBQOS
 2742  1   DR11~C .MAC     M33           18:44 18-OCT-79  18:26 30-OCT-79    8 X
IQNAV
 2743  1   DR11~C .DOC     M33           10:55 19-OCT-79  18:26 30-OCT-79    2 C
ULNUD
 3175  1   DR11~CS.REL     M33           18:47 18-OCT-79  18:26 30-OCT-79    3 R
EHGOQ
 4677  1   DR11~C .MAC     KS2020        14:19 10-JUL-79  18:17 13-NOV-79    6 N
OBQOS
 5415  1   DR11~C .MAC     KS2020        14:19 10-JUL-79  18:21 13-NOV-79    6 N
OBQOS

(M33)MAGSER.MAC        
{Page 3}
        MOVE P3,TKBCBA(T3)      ;Rh11~C base address
        MOVEI T1,CS2CLR         ;Clear Rh11~ in case nxu selected.

{Page 6}
        MOVE P3,TKBCBA(T2)      ;Rh11~C base address

{Page 9}
        MOVE T2,TKBCB      ;Base address of Rh11~C

{Page 10}
        CALL UNISE0             ;Select unit, set mode, put Rh11~C adr in P2.

{Page 14}
;U=UDB, F=DDB, J=KDB, P2=Rh11~C base address, S=DEVIOS.
        MOVE P2,TKBCBA(J)       ;Base address of the Rh11~C.
        RDIO T2,CS1OFS(P2)      ;RH11~C status.
FINOP:  MOVEI T1,F.DC+CS1TRE    ;Clear RH11~C, TU45 error state.

{Page 15}
        MOVE P2,TKBCBA(T2)      ;Rh11~C base address.

{Page 17}
        MOVE P2,TKBCBA(P2)      ;Rh11~C base address.

{Page 18}
;Returns with P2 set to the Rh11~C base address, and T2 to the IOWD after
        MOVE P2,TKBCBA(P2)      ;Get Rh11~C base address.

(M33)COMCON.A03        
{Page 14}
        CP LP10~1
        CP LP10~2

{Page 15}
        CP LP10~3
        CP LP10~4
        CP LP10~5
        CP LP10~6
        CP LP10~7
        CP LP10~8

{Page 16}
PDP10P: PTR 0,LP10~1-LP10~2,LP10~1
        PTR 1,LP10~3-LP10~4,LP10~3
        PTR 2,LP10~5-LP10~6,LP10~5
        PTR 4,LP10~7-LP10~8,LP10~7

(M33)COMMOD.P37        
{Page 42}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 43}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)COMMOD.A00        
{Page 42}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 43}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)MONS1.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)MONFZ.CMD        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)COMMOD.A01        
{Page 42}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 43}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)COMMOD.A02        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)COMMOD.A03        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)COMMOD.A04        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)COMCON.A04        
{Page 13}
        CP LP10~1
        CP LP10~2

{Page 14}
        CP LP10~3
        CP LP10~4
        CP LP10~5
        CP LP10~6
        CP LP10~7
        CP LP10~8

{Page 15}
PDP10P: PTR 0,LP10~1-LP10~2,LP10~1
        PTR 1,LP10~3-LP10~4,LP10~3
        PTR 2,LP10~5-LP10~6,LP10~5
        PTR 4,LP10~7-LP10~8,LP10~7

(M33)COMMOD.A06        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)COMMOD.A08        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)STOPCD.LOG        
{Page 1}
DR11~C.MAC      0
DR11~C.MAC      2

(M33)COMMOD.A13        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)COMMOD.B01        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)MONBTF.LST        
{Page 10}
   377                                          WRREG T3,CS2;           
CLEAR RH11~

(M33)CV3GP2.ME0        
{Page 8}
" DR11~CS=DR11~C",@cr,

(M33)P034H.MFS        
{Page 1}
DR11~C,    53,G 10,1
DR11~C,   106,H 10,1
DR11~C,   125,I 10,1
DR11~C,   178,J 10,2
DR11~C,   231,K 10,2
DR11~C,   284,L 10,2
DR11~C,   286,M 10,2
DR11~C,   339,N 10,3
DR11~C,   368,B 11,3
DR11~C,   421,C 11,4
DR11~C,   447,D 11,4
DR11~C,   468,E 11,5
DR11~C,5K,F 11,6

(M33)KSBLOD.MAC        
{Page 1}
 08100           MOVE RH,[1,,776700]     ;RH11~ BASE ADDRESS
  08600   UNILP:  MOVEI A,CS2CLR          ;CLEAR RH11~

(M33)DISK.MAC        
{Page 1}
    12900   ua1itb: 0                       ;RH11~ vectored interrupt tbl
                                      16300                           ;Needed to make RH11~C clear work.
16800                           ; for current I/O transfer, 0 if RH11~ idle.
  24000   dskint: 0               ;JSR here on RH11~ interrupts

{Page 2}
 02650           jrst dskclr             ;Clear Rh11~C and drive.

{Page 3}
  01340   dskini: movei a,ua1itb-53               ;RH11~ interrupt vector offset
                                    01430   dskin1: call c.rh                       ;clear RH11~

{Page 4}
00430   ; (2) The RH11~ sends a "transfer done" interrupt.
                                                         00470   ;Interrupts from the RH11~ are allowed only when the the null
00510   ; running.  The RH11~ interrupt routine decrements T if the null
04370           call dskclr             ;clear RH11~ and drive
  05950   ;Here to summarize the status of the Rh11~C and the drive D.
  08160   ;RH11~ interrupt service routine.
   08210           rdio a,@cs1adr          ;get RH11~ status
 08970   ?Can't clear RH11~C.

(M33)MONDOC.RCF        
{Page 1}
DR11~C.DOC

(M33)MONTW2.CMD        
{Page 1}
 DR11~CS=DR11~C

(M33)KMC.DOC        
{Page 1}
SCNSER, CTYSIM, DZKON, KMCSER, PICON, CIOPR, COMCON, DR11~C, ONCE, DDT,

(M33)SCNSER.B02        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

(M33)SCNSER.BAK        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

(M33)COMMON.A03        
{Page 41}
RH1STS: RH11~CA                 ;RPCS1
RH1ST2: RH11~CA+10              ;RPCS2

(M33)COMMON.OLD        
{Page 46}
RH1STS: RH11~CA                 ;RPCS1
RH1ST2: RH11~CA+10              ;RPCS2

{Page 57}
        WRREG T3,CS2;                   CLEAR RH11~

(M33)P035.LOG        
{Page 1}
MACRO: DR11~C

{Page 2}
ZOZLEF DR11~C.MAC

(M33)M33.FIL        
{Page 1}
DR11~C   MAC       8  29-DEC-81

(M33)39512.TID        
{Page 1}
  234  2   DR11~C .A27     M33           14:06 25-SEP-79  01:29 19-NOV-79    6 N
OBQOS
  267  2   DR11~C .MAC     M33           18:44 18-OCT-79  01:29 19-NOV-79    8 X
IQNAV
  270  2   DR11~C .DOC     M33           10:55 19-OCT-79  01:29 19-NOV-79    2 C
ULNUD
  566  2   DR11~CS.REL     M33           18:47 18-OCT-79  01:29 19-NOV-79    3 R
EHGOQ
 1621  1   DR11~C .A27     M33           14:06 25-SEP-79  01:35 19-NOV-79    6 N
OBQOS
 1654  1   DR11~C .MAC     M33           18:44 18-OCT-79  01:35 19-NOV-79    8 X
IQNAV
 1655  1   DR11~C .DOC     M33           10:55 19-OCT-79  01:35 19-NOV-79    2 C
ULNUD
 2153  1   DR11~CS.REL     M33           18:47 18-OCT-79  01:35 19-NOV-79    3 R
EHGOQ
 3413  1   DR11~C .B02     M33           18:44 18-OCT-79  03:06 04-JAN-80    8 X
IQNAV
 3414  1   DR11~C .DOC     M33           10:55 19-OCT-79  03:06 04-JAN-80    2 C
ULNUD
 3434  1   DR11~C .MAC     M33           12:46 05-DEC-79  03:06 04-JAN-80    9 H
ECDUD
 3710  1   DR11~CS.REL     M33           14:08 07-DEC-79  03:06 04-JAN-80    3 C
AKNAX
 5004  1   DR11~C .B02     M33           18:44 18-OCT-79  03:21 04-JAN-80    8 X
IQNAV
 5005  1   DR11~C .DOC     M33           10:55 19-OCT-79  03:21 04-JAN-80    2 C
ULNUD
 5025  1   DR11~C .MAC     M33           12:46 05-DEC-79  03:21 04-JAN-80    9 H
ECDUD

(M33)P034D.BTO        
{Page 1}
09:07:30.4  INITIA,ONCE,DZKON,DR11~C,MAGSER,CTYSIM,COMMON/ELB
09:07:34.4  DR11~C   LST

(M33)OSPCPY.CMD        
{Page 1}
(M33)DR11~C.MAC,(OSP)DR11~C.MAC

(M33)P034K.MFS        
{Page 1}
DR11~C,    53,E  8,1
DR11~C,   106,F  8,1
DR11~C,   125,G  8,1
DR11~C,   178,H  8,2
DR11~C,   231,I  8,2
DR11~C,   284,J  8,2
DR11~C,   286,K  8,2
DR11~C,   339,L  8,3
DR11~C,   368,M  8,3
DR11~C,   421,N  8,4
DR11~C,   447,B  9,4
DR11~C,   468,C  9,5
DR11~C,6K,D  9,6

(M33)MONBTI.LST        
{Page 10}
   377                                          WRREG T3,CS2;           
CLEAR RH11~

(M33)BOOTS.170        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)BOOTS.169        
{Page 18}
  05600   UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED O
N KS.

(M33)RMXKON.MAC        
{Page 1}
;IN ORDER TO CLEAR THE RH11~C CONTROLLER, A DRIVE WHICH DEFINATELY EXISTS

{Page 5}
        WRIO    T3,@DAAD1       ;Tell RH11~ track and sector
;by giving the RH11~ a single command.
        WRIO    T1,@T3          ;So RH11~ can address KS10 memory
         CALL SAVSTS            ;READ RH11~ AND DRIVE STATUS INTO UDB.
CONERR: CALL SAVSTS             ;SAVE RH11~ AND DRIVE STATUS IN UDB.

(M33)BOOTS.174        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)CIOPRF.LST        
{Page 2}
    52                          ;DR11~C hardware parameters.

{Page 3}
    70                          ;Macro to make a DR11~c known to the system.
    78                          DEFINE DR11~C(BASEADDRESS,VECTORBASE),<

{Page 4}
   108                          DR11~C(767770,300/4)
   109                          DR11~C(767760,310/4)            ; **jms4480**
   110                          DR11~C(767750,320/4)            ; **jms4480**
   111                          DR11~C(767740,330/4)            ; **jms4480**

{Page 6}
   167                                  PUSHJ   P,DLSEDR        ;1 PREPARE DR11~
C FOR DATA.
   168                                  PUSHJ   P,DLWDR         ;2 WRITE WORD TO
 DR11~C.
   169                                  PUSHJ   P,DLRDR         ;3 READ WORD FRO
M DR11~C.
   170                                  PUSHJ   P,DLDDR         ;4 RELEASE DR11~
C TO KMC & NEXILIS NODE.
   181                          ; ARG 0: INTERFACE TYPE: 0=CTYSIM, 1=DZKON, 2=TY
MBASE NODE, 3=DR11~C

{Page 7}
   247                                  PUSHJ   P,DZREM##       ;TURN OFF DZ11~S
.

{Page 8}
   280                          ;Install DR11~C interface to NEXILIS node
   311                          ;DLWDR (FUNCTION 2) - WRITE WORD TO SELECTED DR1
1~C.
   312                          ; ARG 0: DR11~C#,,16 BIT WORD TO SEND
   313                          DLWDR:  PUSHJ   P,DLSDR         ;GET DR11~C BASE
 ADDRESS.

{Page 9}
   327                          ;DLRDR (FUNCTION 3) - READ WORD FROM SELECTED DR
11~C.
   328                          ; ARG 0: DR11~C #,,ADR TO RECEIVE WORD READ.
   329                          DLRDR:  PUSHJ   P,DLSDR         ;GET DR11~C BASE
 ADDRESS.
   344                          ;DLSEDR (FUNCTION 1) - SELECT DR11~C FOR USE BY 
THE 2020.
   345                          ; ARG 0: DR11~C #,,0
   346                          DLSEDR: PUSHJ   P,DLSDR         ;GET HANDLES ON 
DR11~C
   365                                  HRRZ    T4,DRFLTB(T2)   ;SET DR11~C READ
Y FOR OUR OUTPUT FLAG
   369                          ;DLDDR (FUNCTION 4) - DESELECT DR11~C.
   370                          ; ARG 0: DR11~C #,,0
   371                          DLDDR:  PUSHJ P,DLSDR           ;GET HANDLES ON 
DR11~C

{Page 10}
   385                                   PUSHJ P,DLIDR          ;REINSTALL DR11~
C DRIVER
   391                          ;DLSDR.  LOAD P1 WITH BASE ADDRESS OF DR11~C SPE
CIFIED BY USER.
   392                          ; IN ARG 0.  SKIP IF LEGAL DR11~C, SINGLE RETURN
 OTHERWISE.
   393                          ; T3.RH HAS ARG0.RH.  T2 HAS DR11~C NUMBER.
   395                                  HLRZ T2,T3              ;DR11~C NUMBER
   397                                   JRST DLE0              ;ILLEGAL DR11~C 
NUMBER.
   398                                  MOVE P1,DRBATB(T2)      ;T2:=DR11~C BASE
 ADDRESS.

{Page 12}
   435  000014' 260040  000043'         PUSHJ   P,DLSEDR        ;1 PREPARE DR11~
C FOR DATA.
   436  000015' 260040  000061'         PUSHJ   P,DLWDR         ;2 WRITE WORD TO
 DR11~C.
   437  000016' 260040  000077'         PUSHJ   P,DLRDR         ;3 READ WORD FRO
M DR11~C.
   438  000017' 260040  000115'         PUSHJ   P,DLDDR         ;4 RELEASE DR11~
C TO KMC & NEXILIS NODE.

(M33)BOOTS.171        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)BOOTS.172        
{Page 18}
UNIINI: CONSO 0,RMINI(R)        ;ONE AND ONLY RH11~C, ONLY THING TRIED ON KS.

(M33)RMXKON.LST        
{Page 2}
    33                          ;IN ORDER TO CLEAR THE RH11~C CONTROLLER, A DRIV
E WHICH DEFINATELY EXISTS

{Page 9}
   307  000263' 713420  000063'         WRIO    T3,@DAAD1       ;Tell RH11~ trac
k and sector
   312                          ;by giving the RH11~ a single command.

{Page 10}
   347  000307' 713320  000010          WRIO    T1,@T3          ;So RH11~ can ad
dress KS10 memory

{Page 13}
   499  000466' 260040  000565'          CALL SAVSTS            ;READ RH11~ AND 
DRIVE STATUS INTO UDB.
   512  000476' 260040  000565' CONERR: CALL SAVSTS             ;SAVE RH11~ AND 
DRIVE STATUS IN UDB.

(M33)P034NP.LOG        
{Page 1}
DR11~C .LST       19 H[ 19]    2 

(M33)PAKCKS.MAC        
{Page 1}
    12900   ua1itb: 0                       ;RH11~ vectored interrupt tbl
                                      16300                           ;Needed to make RH11~C clear work.
16800                           ; for current I/O transfer, 0 if RH11~ idle.
  24000   dskint: 0               ;JSR here on RH11~ interrupts

{Page 2}
 02650           jrst dskclr             ;Clear Rh11~C and drive.

{Page 3}
  01340   dskini: movei a,ua1itb-53               ;RH11~ interrupt vector offset
                                    01430   dskin1: call c.rh                       ;clear RH11~

{Page 4}
00430   ; (2) The RH11~ sends a "transfer done" interrupt.
                                                         00470   ;Interrupts from the RH11~ are allowed only when the the null
00510   ; running.  The RH11~ interrupt routine decrements T if the null
04370           call dskclr             ;clear RH11~ and drive
  05950   ;Here to summarize the status of the Rh11~C and the drive D.
  08160   ;RH11~ interrupt service routine.
   08210           rdio a,@cs1adr          ;get RH11~ status
 08970   ?Can't clear RH11~C.

(M33)CHKSUM.CMD        
{Page 1}
DR11~C.MAC

(M33)FILES.CMD        
{Page 1}
DR11~C   MAC       8  17-JUN-80

(M33)KSXGLB.CTL        
{Page 1}
(M33)DR11~CS

(M33)MONGEN.FDM        
{Page 18}
" DR11~CS=DR11~C",@cr,

(M33)P034NP.CTL        
{Page 1}
DR11~C.LST,DZKON.LST,KMCSER.LST,KMCDDT.LST,MAGSER.LST,RMXKON.LST

(M33)DR11C.MAC        
{Page 1}
TITLE   DR11~C Interface to simulated Tymnet node
        STOPCD(,ENTRY,DR11~C)
;The DR11~C simulates a channel for use JG's LSI11
;DR11~C interrupt routine.
;DR11~C hardware parameters.
; by the DR11~C driver.
;HERE ON "INPUT DONE" INTERRUPT FROM DR11~C
        MOVEI   U,KRFMD         ;SET DR11~C BIT INDICATING WE HAVE THE WORD
IRDIS:  MOVEI   T1,KRFMD        ;SET DR11~ BIT INDICATING WE HAVE TAKEN INPUT
;HERE ON "OUTPUT DONE" INTERRUPT FROM DR11~C
        MOVEI   U,DRFN          ;CLEAR BIT IN DR11~C STATUS WORD
        WRIO    T4,@DROB        ;SEND DATA TO THE DR11~
        BSIO    U,@DRCSR        ;SET DR11~ STATUS BIT TO MAKE OUTPUT VISIBLE
        MOVEI   T1,KRFMD        ;SET DR11~ BIT TO ACCEPT LAST INPUT WORD

(M33)MONED.DIR        
{Page 1}
DR11~C.MAC

(M33)MONKS.        
{Page 1}
  DR11~CS=DR11~C.MAC

(M33)S.MAC        
{Page 25}
;*;MAPALC(DRI)          ;DR11~C BLOCK INPUT PAGE  (never referenced)
;*;MAPALC(DRO)          ;DR11~C BLOCK OUTPUT PAGE (never referenced)

(M33)FICHE.CMD        
{Page 1}
KMC.LST,KMCSER.LST,DZKON.LST,DR11~C.LST,CIOPR.LST,CTYSIM.LST

(M33)FICHE.TXT        
{Page 1}
#21: P034/K40           KMC,KMCSER,DZKON,DR11~C,CIOPR,CTYSIM

(M33)P034N.MFS        
{Page 1}
 DR11~C,,B  1,0
DR11~C,53,C   1,0
DR11~C,106,D   1,1
DR11~C,125,E   1,1
DR11~C,178,F   1,2
DR11~C,231,G   1,2
DR11~C,284,H   1,2
DR11~C,286,I   1,2
DR11~C,339,J   1,3
DR11~C,368,K   1,3
DR11~C,421,L   1,4
DR11~C,447,M   1,4
DR11~C,468,N   1,5
DR11~C,499,B   2,6

(M33)S.CAB        
{Page 27}
MAPALC(DRI)             ;DR11~C BLOCK INPUT PAGE
MAPALC(DRO)             ;DR11~C BLOCK OUTPUT PAGE

(M33)MONBTS.A04        
{Page 4}
        WRREG T3,CS2;                   CLEAR RH11~

(M33)MONBTS.A06        
{Page 4}
        WRREG T3,CS2;                   CLEAR RH11~

(M33)MONBTS.B01        
{Page 4}
        WRREG T3,CS2;                   CLEAR RH11~

(M33)COMCON.B01        
{Page 14}
        CP LP10~1
        CP LP10~2

{Page 15}
        CP LP10~3
        CP LP10~4
        CP LP10~5
        CP LP10~6
        CP LP10~7
        CP LP10~8

{Page 16}
PDP10P: PTR 0,LP10~1-LP10~2,LP10~1
        PTR 1,LP10~3-LP10~4,LP10~3
        PTR 2,LP10~5-LP10~6,LP10~5
        PTR 4,LP10~7-LP10~8,LP10~7

(M33)COMCON.MAC        
{Page 13}
        CP LP10~1
        CP LP10~2

{Page 14}
        CP LP10~3
        CP LP10~4
        CP LP10~5
        CP LP10~6
        CP LP10~7
        CP LP10~8

{Page 15}
PDP10P: PTR 0,LP10~1-LP10~2,LP10~1
        PTR 1,LP10~3-LP10~4,LP10~3
        PTR 2,LP10~5-LP10~6,LP10~5
        PTR 4,LP10~7-LP10~8,LP10~7

(M33)COMMOD.TAB        
{Page 38}
  TYPRM==5      ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)

{Page 39}
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH:!BLOCK SBSIZ     ;RH11~C/Rm03 registers of most recent hard error.
UNISB:! BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 40}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)STOPCD.ALL        
{Page 1}
--------========-------- DR11~C.MAC
DR11~C          STOPCD(,ENTRY,DR11~C)
DR11~C           STOPCD

(M33)COMMOD.XXX        
{Page 43}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 44}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)FIND0K.PTR        
{Page 1}
DR11~C.MAC      

(M33)STOPCD.NAM        
{Page 1}
--------========-------- DR11~C.MAC

(M33)STOPCD.L99        
{Page 1}
        PUSHJ P,DZREM           ;TURN OFF DZ11~S.
; (EK-DZ11~0-UG-001) for semantics.
DR11~C.MAC      
        ENTRY   DR11~C,DRINI
DR11~C::

(M33)MONTW1.CMD        
{Page 1}
 DR11~CW=DR11~C

(M33)COMCON.OLD        
{Page 14}
        CP LP10~1
        CP LP10~2

{Page 15}
        CP LP10~3
        CP LP10~4
        CP LP10~5
        CP LP10~6
        CP LP10~7
        CP LP10~8

{Page 16}
PDP10P: PTR 0,LP10~1-LP10~2,LP10~1
        PTR 1,LP10~3-LP10~4,LP10~3
        PTR 2,LP10~5-LP10~6,LP10~5
        PTR 4,LP10~7-LP10~8,LP10~7

(M33)COMCON.A14        
{Page 13}
        CP LP10~1
        CP LP10~2

{Page 14}
        CP LP10~3
        CP LP10~4
        CP LP10~5
        CP LP10~6
        CP LP10~7
        CP LP10~8

{Page 15}
PDP10P: PTR 0,LP10~1-LP10~2,LP10~1
        PTR 1,LP10~3-LP10~4,LP10~3
        PTR 2,LP10~5-LP10~6,LP10~5
        PTR 4,LP10~7-LP10~8,LP10~7

(M33)COMMOD.A12        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)COMMOD.MAC        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)FILCOM.LOG        
{Page 11}
1)15            CP LP10~4
2)              CP LP10~4

(M33)P034Q.MEM        
{Page 1}
        2020, since it uses message type 3 to determine how many DR11~s are

(M33)MON.CMD        
{Page 1}
DR11~C.MAC

(M33)CALST.MSG        
{Page 1}
   01800   DR11~C.MAC

(M33)REL.CMD        
{Page 1}
RENAME DR11~CS.REL,(oscurrent)
RENAME DR11~CW.REL,(oscurrent)
RENAME DR11~C.REL,(oscurrent)

(M33)MON.ALL        
{Page 1}
DR11~C.MAC

(M33)FSHFMT.CMD        
{Page 1}
KMC,KMCSER,DZKON,DR11~C,CIOPR,CTYSIM

(M33)CRASH.169        
{Page 1}
MAGSTS is tape drive status, CRS1 in the RH11~ connected to the TM02/TU45.
   200 = bit 07 = RDY  = RH11~ is Ready (should always be on in any dump)
"We keep seeing bit 13 get set in register CSR1 of the tape's RH11~".

(M33)COMMON.B02        
{Page 29}
CNFCHN::EXP     CHNN##  ;(72) NUMBER OF DATA CHANNELS (DF10 or RH11~S or F3)

{Page 41}
RH1STS: RH11~CA                 ;RPCS1
RH1ST2: RH11~CA+10              ;RPCS2

(M33)COMMON.SCM        
{Page 4}
2)29    CNFCHN::EXP     CHNN##  ;(72) NUMBER OF DATA CHANNELS (DF10 or RH11~S or
 F3)

(M33)COMMON.MAC        
{Page 29}
CNFCHN::EXP     CHNN##  ;(72) NUMBER OF DATA CHANNELS (DF10 or RH11~S or F3)

{Page 41}
RH1STS: RH11~CA                 ;RPCS1
RH1ST2: RH11~CA+10              ;RPCS2

(M33)FILIOL.LST        
{Page 21}
   836                          ;efficient when using DF10 or RH11~).  To avoid 
end-of-track or command list

(M33)COMMOD.BAK        
{Page 41}
    TYPRM==5    ;; 5    RM (RH11~C CONTROLLER FOR RM03, RP06 UNITS)
        XP SBSIZ,11     ;NUMBER OF WORDS NEEDED TO SAVE RH11~ REGISTERS.
UNISBH::!BLOCK SBSIZ    ;RH11~C/Rm03 registers of most recent hard error.
UNISB::!        BLOCK SBSIZ     ;RH11~C/RM03 registers saved here on error.

{Page 42}
TKBCBA::!UA3,,772440            ;(1)Base address of Rh11~C

(M33)FILIO.MAC        
{Page 12}
;efficient when using DF10 or RH11~).  To avoid end-of-track or command list

(M33)FILIO.SCM        
{Page 2}
2)      ;efficient when using DF10 or RH11~).  To avoid end-of-track or command 
list

(M33)MONBTL.LST        
{Page 10}
   377                                          WRREG T3,CS2;           
CLEAR RH11~

(M33)MONBTS.BAK        
{Page 4}
        WRREG T3,CS2;                   CLEAR RH11~

(M33)MONBTS.MAC        
{Page 4}
        WRREG T3,CS2;                   CLEAR RH11~

(M33)SCNSER.MAC        
{Page 13}
;03{}03 RESET THE INTERFACE, QUIETLY ZAP ALL CIRCUITS (DR11~ IS BAD ON KS)

{Page 192}
        MOVEI   T1,^D60         ;Here when KMC detects problem with a DR11~C
DRZAP:  PUSHJ   P,DETONE        ;Detach one port on this particular DR11~

!exit
    lyL
[