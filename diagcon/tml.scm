File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)1	;SEQ283
****
2)1	;SEQ480
2)	  
2)	  
2)	.SBTTL  SPECIAL MICRO-CODE ROUTINES     12-NOV-76
2)	 
2)	.REM    %
2)	RAMLOD - ROUTINE TO LOAD THE C-RAM WITH DATA POINTED TO BY THE 
2)	FIRST PARAMETER.  THE SECOND & THIRD PARAMETERS SPECIFY THE FIRST
2)	AND LAST C-RAM ADDRESSES TO LOAD.
2)	%
2)	RAMLOD: PUSH    <R1,R2,R3>
2)	        MRESET                  ;ENSURE C-RAM PARITY CHECK OFF
2)	        MOV     (R5)+,R1        ;GET BUFFER POINTER
2)	        MOV     (R5)+,R3        ;GET FIRST C-RAM ADDRESS
2)	        MOV     (R5)+,R2         ;GET LAST C-RAM ADDRESS
2)	1$:     MOV     R3,R0           ;CURRENT C-RAM ADDRESS
2)	        WCRAM                   ;WRITE A C-RAM LOCATION
2)	        ADD     #11.,R1         ;UPDATE BUFFER POINTER
2)	        INC     R3              ;NEXT C-RAM ADDRESS
2)	        CMP     R3,R2           ;DONE?
2)	        BLE     1$              ;NO, LOAD NEXT LOCATION
2)	        JSR     R5,RSTMBX       ;RESTORE RESET DEFAULT
2)	        POP     <R3,R2,R1>
2)	        RTS     R5              ;RETURN
2)	 
2)	 
2)	.REM    %
2)	USPEC - ROUTINE TO PERFORM MBOX DATA PATH LOOPBACK.  SPECIAL
2)	MICRO-CODE MUST BE LOADED, AN RSTMBX PERFORMED, & MEM TO C MIXER
2)	STEERED BEFORE CALL.  ARGUMENTS ARE ADDRESS & DATA POINTERS.
2)	THE LOOPBACK RESULT IS LOADED INTO THE AR, ARX & IR.
2)	%
2)	 
2)	USPEC:  MOV     R5,BRKPC        ;SAVE PC
2)	        MOV     (R5)+,1$        ;GET ADDRESS POINTER
2)	        MOV     (R5)+,2$        ;GET DATA POINTER
2)	        JSR     R5,USTRT1       ;START LOOPBACK U-CODE
2)	1$:     .WORD   0               ;ADDRESS POINTER GOES HERE
2)	2$:     .WORD   0               ;DATA POINTER GOES HERE
2)	        BCS     3$              ;ERROR
2)	        MOV     #14.,R0         ;BURT 14 CLOCKS
2)	        JSR     R5,BRST1
2)	3$:     RTS     R5              ;RETURN
2)	 
2)	 
2)	 
2)	;SEQ481
2)	 
2)	 
2)	.REM    %
2)	USTART - ROUTINE TO START MBOX DATA PATH LOOPBACK U-CODE.  SPECIAL
2)	MICRO-CODE MUST BE LOADED & AN RSTMBX PERFORMED WITH CRAFLG = THE
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

2)	OVERLAY ADDRESS.  ARGUMENTS ARE ADDRESS & DATA POINTERS.  KL WORD
2)	MEMAD 0-13 MUST BE SET FOR THE PROPER MEM/AD FUNCTION.
2)	 
2)	USTART: MOV     R5,BRKPC        ;SAVE PC
2)	USTRT1: PUSH    R1
2)	        DFXCTT                  ;DISABLE JRST DECODE
2)	                DISIOJ
2)	        MOV     #5,R0
2)	        JSR     R5,BRST1        ;BURST 5 CLOCKS
2)	        DFWRTT                  ;SET C-RAM ADDRESS = 2341
2)	                CADR41
2)	                LCRDAR
2)	        MOV     (R5)+,R0        ;GET ADDRESS POINTER
2)	        MOVB    (R0)+,MEMAD     ;BUILD 22-BIT ADDRESS
2)	        MOVB    (R0)+,MEMAD+1
2)	        MOVB    (R0)+,R1
2)	        BIC     #177700,R1
2)	        BIC     #77,MEMAD+2
2)	        BISB    R1,MEMAD+2
2)	        DFWRTT                  ;PUT ADDRESS & FUNCTION IN AR
2)	                MEMAD
2)	                LDAR
2)	        MOV     #4,R0           ;BURST 4 MBOX CLOCKS
2)	        JSR     R5,BRST1
2)	        MOV     (R5)+,R1        ;GET DATA POINTER
2)	        MOV     #LDAR,R0
2)	        DFWRT                   ;PUT DATA IN AR
2)	1$:     POP     R1
2)	        RTS     R5              ;RETURN
2)	 
2)	MEMAD:  WD36    0,0,0           ;MUST BE EVEN
2)	CADR41: WD36    4100,0000,0000
2)	 
2)	 
2)	 
2)	;SEQ283
**************
1)1	;SEQ507
1)	.SBTTL          MEM REF SUBROUTINE
1)	.REM    %
1)	        
1)	GETMEM - ROUTINE TO SAVE THE STATUS OF A MEMORY REFERENCE.  PUTS
1)	RQ0, RQ1, RQ2, RQ3, RD & WR RQS & THE SBUS ADR IN TABLE MEMTAB:
1)	%
1)	GETMEM: PUSH    R1
1)	        DFRDT                   ;READ REQUESTS
1)	                166             ;
1)	        MOV     @.DAT3,R1       ;
1)	        BIC     #177417,R1      ;CLEAR GARBAGE
1)	        TBIT    27              ;RD?
1)	        BEQ     1$              ;NO
1)	        BIS     #10,R1          ;YES
1)	1$:     TBIT    33              ;WR?
1)	        BNE     2$              ;NO
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	        BIS     #4,R1           ;YES
1)	2$:     MOVB    R1,MEMTAB               ;PUT RQS IN FIRST BYTE
1)	        DFRDT                   ;GET EBUS REG (PMA)
1)	                167             ;
1)	        MOV     .DAT3,R0        ;
1)	        MOV     #MEMTAB+1,R1    ;
1)	        MOVB    (R0)+,(R1)+     ;PUT IN NEXT 3 BYTES
1)	        MOVB    (R0)+,(R1)+     ;
1)	        MOVB    (R0),(R1)       ;
1)	        BICB    #300,(R1)       ;CLEAR GARBAGE
1)	        MOV     #MEMTAB+1,R1    ;
1)	        BICB    #3,(R1)         ;CLEAR OUT PMA 34 & 35
1)	        TENBIT  173,35          ;TEST SBUS ADR 34
1)	        BEQ     3$              ;CLEAR
1)	        BISB    #2,(R1)         ;SET
1)	3$:     TENBIT  174,35          ;TEST SBUS ADR 35
1)	        BEQ     4$              ;CLEAR
1)	        INCB    (R1)            ;SET
1)	4$:     MOV     #MEMTAB,R0              ;TABLE POINTER TO R0
1)	        MOV     R0,MEMRQ        ;SET FLAG
1)	        POP     R1
1)	        RTS     R5              ;RETURN
1)	MEMTAB: .WORD   0,0             ;MEM RQ STATUS
1)	MEMRQ:  .WORD   0               ;MEM STATUS FLAG
1)	;SEQ508
1)	.SBTTL          SBUS DIAG SUBROUTINES
****
2)1	.SBTTL          SBUS DIAG SUBROUTINES
**************
1)1	.SBTTL          MEMORY CONFIGURATION SAVE & RESTORE
1)	;FIND A MEMORY THAT RESPONDS AND SAVE ITS BASE ADDRESS.  IF 
1)	;NO MEMORY IS FOUND WITH FOUR CONSECUTIVE REFERENCES WITHIN THE
1)	;FIRST 256K THEN AN ERROR RETURN IS GIVEN.
1)	;SEQ297
1)	.REM    %
1)	        SAVMCN SAVES THE EXISTING MEMORY CONFIGURATION.  THE CONFIGURATION IS
1)	ASSUMED TO BE VALID.  FOR MF20S, ONLY A MAP OF CONFIGURED, USABLE CONTROLLERS
1)	IS KEPT.  THIS MEANS THAT IF POWER IS SHUT OFF THE CONFIGURATION IS DESTROYED.
1)	A C-BIT SET RETURN IS DONE ONLY IS 2 MF20S RESPOND TO ADDRESS 0.  
1)	A VARIABLE (TCWRTO) IS SET TO THE NUMBER OF THE CONTROLLER RESPONDING
1)	TO ADDRESS C (DMA20 IS ASSUMBED IF NO MA20/MB20/MF20 HAS THAT ADDRESS).
1)	        %
1)	NUMCON=5                ;CURRENT MAXIMUM NUMBER OF CORE MEMORY CONTROLLERS
1)	SAVMCN: PUSH    R1              ;SAV REG
1)	;-----HERE TO RECORD STATE OF MA20,MB20/DMA20 MEMORY.
1)	        LOAD    1,GTMCNF,13     ;LOAD AC PRGM (AC1-13)
1)	        MOV     #CONTAB,R1      ;PTR TO CONF TABLE SAVES TIME & SPACE
1)	        RUN     1
1)	        BCS     2$              ;AC ROUTINE HUNG
1)	1$:     READ17                  ;GET FUNCTION 0 RESULT
1)	        MOVB    3(R0),(R1)+     ;INTERLEAVE BITS TO TABLE
1)	        CONTIN                  ;DO FUNCTION 1
1)	        BCS     2$              ;AC ROUTINE HUNG
1)	        READ17                  ;GET FUNCTION 1 RESULT
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	        MOVB    (R0)+,(R1)+     ;REQUEST ENABLE BITS TO TABLE
1)	        BIC     #170317,-2(R1)  ;CLR JUNK FROM INT & R0 EN BITS
1)	        MOVB    (R0)+,(R1)+     ;MOV ADR BOUNDS STUFF
1)	        MOVB    (R0)+,(R1)+     ;DITTO
1)	         
1)	        BIC     #140003,-2(R1)  ;CLR JUNK BITS FROM ADR BOUNDS
1)	        CONTIN                  ;NEXT FUNCTION 0
1)	        BCC     3$
1)	2$:     ERRORA  ERRP            ;AC ROUTINE HUNG?
1)	3$:     CMP     R1,#CONTAB+<4*NUMCON>   ;DONE WITH ALL CONTRS YET?
1)	        BLO     1$              ;NO...KEEP GOING
1)	;SEQ298
1)	;-----THIS CODE GOES AND GETS MF20 INFO: USABLE MF20 & DESELECTED-AT-0 MAPS.
1)	        LOAD    0,GMFUMP,20     ;LD AC 0-17
1)	        RUN     0               ;RUN THE AC PRGM
1)	        BCS     2$              ;BR ON ERR
1)	        READ17                  ;GET RESULT DATA
1)	        MOV     (R0)+,R1        ;USABLE MF20 MAP TO R1 (BIT0=MF20 #10, ETC)
1)	        MOV     (R0),R0         ;GET DESELECTED-AT-0 MAP INTO R0        
1)	        MOV     R1,UMFMAP       ;SAV USABLE MF20 MAP
1)	;-----FIND OUT WHICH MF20,IF ANY, RESPONDS TO ADDRESS 0.
1)	        COM     R1              ;CALC UNUSABLE MF20 MAP
1)	        BIS     R1,R0           ;CALC UNUSABLE .OR. DESEL-AT-O MAP
1)	        COM     R0              ;CALC USABLE .AND. SEL-AT-0 MAP
1)	        BEQ     5$              ;NO MF20 RESPONDS AT 0.. GO TRY FOR MA20/MB20
1)	        MOV     #7,R1           ;INI MF20 # FOR SEARCH THRU MAP IN R0
1)	4$:     INC     R1              ;NXT MF20
1)	        CLC     
1)	        ROR     R0              ;IS THIS MF20 USABLE & SELECTED-AT-0?
1)	        BCC     4$              ;NO.. TRY NXT
1)	        BNE     10$             ;YES BUT THERE IS ANOTHER TOO.. ERROR! C SET RET
1)	        BR      9$              ;OK, JUST 1 MF RESPONDS TO 0.. GO SAV ITS #
1)	;----HERE TO FIND OUT WHICH MA20/MB20, IF ANY, RESPONDS TO ADDRESS 0.  IF NONE
1)	;DOES THEN WE ASSUME THAT THE DMA20 IS RESPONDING TO 0.
1)	5$:     CLR     R1              ;INI TABLE SRCH NDX
1)	6$:     BIT     #37700,CONTAB+2(R1)     ;IS CONTR LO ADR BOUND AT 0?
1)	        BNE     7$              ;NO.  TRY NEXT MA/MB
1)	        BIT     #4000,CONTAB(R1)        ;DOES CONTR RESP TO RQ 0?
1)	        BEQ     7$              ;NO. TRY NXT MA/MB
1)	        TSTB    CONTAB(R1)      ;IS CONTR TURNED ON (IE INTRL NOT 0)?
1)	        BNE     8$              ;YES.  THIS IS THE ONE, GO RECORD ITS #
1)	7$:     ADD     #4,R1           ;ELSE LOOK AT NXT MA20/MB20
1)	        CMP     R1,#12.         ;TRIED THEM ALL?
1)	        BLE     6$              ;NO, LOOP.  IF YES, JUST ASSUME DMA20.
1)	8$:     ASR     R1              ;DIVIDE NDX BY 4 TO GET CONTR #
1)	        ASR     R1              
1)	;-----HERE TO SET THE CONTROLLER NUMBER WHICH RESPONDS TO 0 AND EXIT.
1)	9$:     MOV     R1,TCWRT0       ;SAV # OF THE CONTROLLER WHICH RESPONDS TO 0
1)	        CLC                     ;IND NO ERROR
1)	10$:    POP     R1              ;GET BACK REG SAVED EARLIER
1)	        RTS     R5              ;RETURN TO CALLER
1)	;SEQ299
1)	;-----THE CORE MEMORY CONFIGURATION TABLE. 2 BYTES AND A WORD PER CONTROLLER.
1)	CONTAB: .REPT   NUMCON
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	        .BYTE   000             ;THE INTERLEAVE BITS (00II0000)
1)	        .BYTE   000             ;THE WORD REQUEST ENABLE BITS (0000RRRR)
1)	        .WORD   000000          ;THE ADR BOUNDS BITS (00SSSSLLLLUUUU00)
1)	        .ENDM
1)	TCWRT0: .WORD   0               ;NUMBER OF THE CONTR WHICH RESPONDS TO 0.  IF NO
1)	                                ;MA20/MB20/MF20 IS AT 0, THEN WE ASSUME DMA20.
1)	;SEQ300
1)	;-----AC PROGRAM TO READ SBUS DIAGS FROM DIFFERENT CORE MEMORY CONTROLLERS
1)	;SO THAT THE EXISTING CONFIGURATION CAN BE DETERMINED.
1)	GTMCNF: I10     SETZM,0,,16     ;1--
1)	        I10     IOR,16,,12      ;2--
1)	        IO10    SBDIAG,,,16     ;3--
1)	        I10     JRST,4,,5       ;4--
1)	   
1)	        I10     AOS,0,,16       ;5--
1)	        IO10    SBDIAG,,,16     ;6--
1)	        I10     JRST,4,,10      ;7--
1)	        I10     ADD,12,,13      ;10-
1)	        I10     JRST,0,,1       ;11-
1)	        WD36    0100,0000,0000  ;12-
1)	        WD36    0200,0000,0000  ;13-
1)	;-----THIS AC PROGRAM CREATES 2 MAPS:  ONE WHICH SAYS WHICH MF20S ARE USABLE,
1)	;AND ANOTHER SAYING WHICH MF20S ARE DESELECTED AT ADDRESS 0.  THE BITS IN THE
1)	;MAPS CORRESPOND POSITIONALLY.  IF THE MF20 IS NOT USABLE THEN THE DESELECTED
1)	;BIT IS MEANINGLESS.  THE PROGRAM IS HARDCODED TO LOOK AT CONTROLLERS 10-17.
1)	;THE TWO MAPS ARE BUILT IN AC17.  THE "USABLE" MAP IS IN BITS 28-35, BIT 35
1)	;IS FOR MF20 #10.  34 IS FOR #11, ETC.  THE OTHER MAP IS IN  BITS 12-19.  THESE
1)	;BIT POSITIONS ARE USED TO ERASE WORK DONE BY THE DPD-11.  A USABLE CONTROLLER
1)	;IS (1) AN MF20, (2) IN SOFTRWARE STATE 2 OR 3, AND (3) IS NOT DISABLED.
1)	GMFUMP: I10     LSH,17,,1               ;0-<START> SHIFT CTL/OUTPUT WD PRIOR TO NXT MF20
1)	        IO10    SBDIAG,,,15             ;1-DO FCN 1 TO GET MF20 STATUS
1)	        I10     LSH,16,,777767          ;2-GET STATUS INFO INTO RHE OF AC16
1)	;SEQ301
1)	        I10     CAIN,16,,500001 ;3-SKIP IF -USABLE (-MF20,-SS 2/3, OR DISABLED)
1)	        I10     IORI,17,,1      ;4-ELSE MARK CONTR AS USABLE
1)	        
1)	        I10     HRRI,15,,12     ;5-TURN FCN 1 INTO FCN 12 FOR ADRESP RAM RD
1)	        IO10    SBDIAG,,,15     ;6-RD ADRESP RAM WD 0
1)	        I10     TLNE,16,,10     ;7 - SKIP IF DESFL BIT IS 0
1)	        I10     IORI,17,,2000000        ;10-ELSE SET DESEL BIT IN MAP TOO
1)	        I10     SUB,15,,14      ;11-TURN FCN 12 INTO FCN 1, NXT CONTR #
1)	        I10     JUMPL,17,,0     ;12-LP TIL ALL MF20S DONE (AC17 BIT 00 = 0)
1)	        I10     JRST,4,,0       ;13-HALT WHEN DONE
1)	        WD36    0200,0000,0011  ;14-SUB FROM SBDIAG WD TO GET FCN 1, NXT MF
1)	        WD36    3600,0000,0001  ;15-FCN 1 SBDIAG FOR HIGHEST MF20.. MODIFIED
1)	        WD36    0000,0000,0000  ;16-FCN 1 & 12 RESULT GOES HERE
1)	        WD36    7760,0000,0000  ;17-LHF IS CONTROL # OF LPS.  MAPS BUILT HERE
1)	        .EVEN
1)	;SEQ303
1)	.REM    %
1)	        RSTMCN RESTORES THE MEMORY CONFIGURATION SVED BY SAVMCN.  ONLY DOES A
1)	C-BIT SET RETURN IF MF20 CONFIGURATION WAS BLOWN BY POWER OFF AND WE DO NOT HAVE
1)	A CORE MEMORY AT ADDRESS 0.
1)	        %
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	RSTMCN: PUSH    <R3,R4,R5>      ;SAVE REGS
1)	        
1)	        LOAD    1,7$,3          ;LOAD THE AC PROGRAM (AC1-3)
1)	        CLR     R5              ;INIT CONTROLLER NUMBER
1)	        MOV     #CONTAB,R3      ;CONF TABLE PTR INIT
1)	;-----SBUS DIAG FUNCTION 0 SETUP
1)	1$:     MOV     #CNFWD0+3,R4    ;DIAG WD PTR SAVES TIME & SPACE
1)	        MOVB    (R3)+,(R4)      ;INTERLEAVE BITS TO WORD
1)	        BISB    (R3)+,(R4)      ;WD RQ BITS TO WORD
1)	        PUSH    R5              ;SCRATCH CONTR # ON STACK
1)	        ROR     (SP)            ;SPLIT CONTR # 4-1
1)	        BCC     2$              ;BR IF EVEN CONTR
1)	        BISB    #200,(R4)       ;ELSE SET ODD CONTR BIT IN WD
1)	2$:     BISB    #100,(R4)+      ;SET CLR 0-5 BIT IN WD
1)	        MOVB    (SP)+,(R4)      ;HI ORD 4 BITS OF CONTR # TO WD
1)	;SEQ304
1)	;-----SBUS DIAG FUNCTION 1 WORD SETUP
1)	        TST     (R4)+           ;SKIP DIAG WD STUFF TO LEAVE ALONE
1)	        MOV     (R3)+,(R4)      ;MOV ADR BOUNDS BITS TO WD
1)	        BIS     #2,(R4)+        ;SET LOAD BOUNDS BIT IN WD
1)	        MOV     R5,(R4)         ;CONTR # TO WORD
1)	        SWAB    (R4)            ;POSIITON IT PROPERLY
1)	        ASR     (R4)
1)	;-----LOAD SBUS DIAG WORDS AND RUN PROGRAM
1)	        DPOST,4,CNFWD0          ;AC4
1)	        DPOST,6,CNFWD1          ;AC6
1)	        RUN     1
1)	        BCC     4$              
1)	3$:     ERRORA  ERRP            ;AC PROGRAM DIDN'T HALT
1)	;-----SELECT NEXT CORE MEMORY CONTROLLER OR GO ON TO MF20S.
1)	4$:     INC     R5              ;BUMP CONTR #
1)	        CMP     R5,#NUMCON      ;DONE YET?
1)	        BLT     1$              ;NO...CONTINUE
1)	;-----THIS SECTION REENABLES ANY MF20S WHICH EXISTED AND WERE USABLE WHEN
1)	;SAVMCN WAS CALLED.
1)	        LOAD    1,RSTMFP,16     ;LOAD THE AC PROGRAM
1)	        RUN     1               ;RUN IT
1)	        BCS     3$              ;BR ON ERROR
1)	        DFPC                    ;GET THE HALTED PC
1)	        TST     (R0)            ;PC 0 OR NOT 0?
1)	        BEQ     5$              ;0.. NORMAL COMPLETION
1)	        CMP     TCWRT0,#4       ;IS THE CONTR WHICH RESPONDS TO 0 A CORE MEM?
1)	        BLE     5$              ;YES..WE STILL HAVE MEM AT 0
1)	        SEC                     ;MF20S POWERED OFF.. CONFIG BLOWN
1)	        BR      6$              ;EXIT WITH C-BIT SET
1)	5$:     CLC
1)	6$:     POP     <R5,R4,R3>      ;RESTORE REGS
1)	        RTS     R5
1)	;SEQ305
1)	;-----AC PROGRAM TO DO SBDIAGS TO SET CORE MEM CONF.
1)	        .ODD            ;FORCE EVEN ALLIGNMENT FOR CNFWD0
1)	7$:     IO10    SBDIAG,,,4      ;1--DO SBUS DIAG FUNC 0
1)	        IO10    SBDIAG,,,6      ;2--DO SBUS DIAG FUNC 1
1)	        I10     JRST,4,,1       ;3--HALT
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	CNFWD0: WD36    0100,4000,0000  ;4--SBUS DIAG FUNCTION 0 WORD
1)	                                ;5--
1)	CNFWD1: WD36    0000,0000,1001  ;6--SBUS DIAG FUNCTION 1 WORD
1)	                                ;7--
1)	;-----THIS AC PROGRAM SCANS THRU THE MF20S AND REENABLES THOSE MF20S WHICH WERE
1)	;ON AT THE TIME SAVMCN WAS CALLED.  SINCE IT TAKES SO MUCH DATA TO BRING UP AN
1)	;MF20 AFTER POWER ON WE PUNT IF AN MF20 WHICH WAS ON IS NO LONGER ON, IE IT WAS
1)	;POWERED OFF FOR SOME REASON.  THE MAP UMFMAP IS CREATED AND SVAED HERE BY
1)	;SAVMCN.  AN MF20 WAS USABLE IF A MAP BIT IS SET.  BIT35=MF20#10.  34=#11, ETC.
1)	        .EVEN                   ;FORCE UMFMAP TO EVEN BOUNDARY
1)	                                ;0 - NOT USED
1)	RSTMFP: IO10    SBDIAG,,,16     ;1-<START>DO FCN 1 TO GET STATE OF MF20
1)	        I10     HRRI,16,,1201   ;2-CNG FCN 1 TO LD SS 2, NOT DISABLE
1)	        I10     TRNN,15,,1      ;3-WAS THIS MF20 USABLE WHEN SAVMCN WAS RUN?
1)	        I10     JRST,,,10       ;4--HERE IF NO.  DONT SET SS 2.
1)	        I10     TRNN,17,,1000    ;5-SKIP IF MF20 AT SS 2 (IE NOT POWERED OFF)
1)	        I10     JRST,4,,7       ;6-ELSE HALT.  MEM CONFIG BLOWN BY POWER OFF
1)	        IO10    SBDIAG,,,16     ;7-ALL OK.  SET SS2, NOT DISABLED
1)	        I10     ADD,16,,14      ;10-CNG SBDIAG WD BAK TO FCN 1 RD, NXT MF20
1)	        I10     LSH,15,,777777   ;11-LOOK AT NXT CTL BIT
1)	;SEQ306
1)	        I10     JUMPN,15,,1     ;12-LOOP IF MORE MF20S TO DO
1)	        I10     JRST,4,,0       ;13-ELSE HALT.. ALL DONE OK
1)	        WD36    0177,7777,6600  ;14-CHANGES FCN 1 SET -DIS/SS2 TO FCN 1 RD, NXT
1)	UMFMAP: WD36    0000,0000,0000  ;15-MAP OF USABLE MF20S (BIT35=10,BIT34=11,ETC)
1)	        WD36    2000,0000,0001  ;16-FCN 1 RD SBDIAG WD, MF20 #10 FIRST
1)	                                ;17-ECHO FROM FCN 1
1)	        .EVEN
1)	;SEQ518
1)	        .SBTTL CHANNEL UTILITY SUBROUTINES  11-DEC-75
1)	.REM    %
1)	        ROUTINE TO CONDITION THE EPT FOR NNEL.  CHANNEL IN
1)	R0; DATA POINTER IN R1.  CLEARS STATUS WORD 1.
1)	%
1)	SETEPT: MOV     R5,BRKPC                ;SAVE PC FOR FN. BREAK
1)	        SL      R0,2            ;CHANNEL * 4
1)	        MOVB    R0,EBRADR       ;TO ADDRESS LOWER
1)	        MOV     EBRFLG,R0               ;GET EPT BASE PAGE
1)	        ASL     R0              ;POSITION AT 1K BOUNDARY
1)	        MOV     R0,EBRADR+1     ;PUT IN ADDRESS
1)	        DFXCTT                  ;START THE CLOCK
1)	                STRCLK          ;
1)	        MOV     R1,R0           ;DATA POINTER
1)	        MOV     #EBRADR,R1      ;ADDRESS
1)	        DPOSVR                  ;DEPOSIT & VERIFY
1)	        BCS     1$              ;ERROR
1)	        INCB    EBRADR          ;EPT +1
1)	        MOV     #ZERO..,R0      ;
1)	        DPOS                    ;CLEAR IT
1)	        TSTB    CSHCLR          ;EBR SET?
1)	        BEQ     SETEBR          ;NO, SET IT
1)	1$:     RTS     R5              ;RETURN
1)	.REM    %
1)	        SUBROUTINE TO SET THE EBR TO THE VALUE IN EBRFLG.
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	ALSO SETS CACHE LOOK & LOAD IF CSHFLG SET.
1)	%
1)	SETEBR: MOV     EBRFLG,CSHCLR   ;NO, SET EBR DEFAULT
1)	        MOV     EBRFLG,CSHALW   ;
1)	        TST     CSHFLG          ;CACHE ENABLED?
1)	        BEQ     1$              ;NO, JUST SET ERR
1)	        XQT     CSHALW          ;YRS, ALSO SET LOOK & LOAD
1)	        BR      2$              ;
1)	1$:     XQT     CSHCLR          ;SET EBR TO DEFAULT
1)	2$:     RTS     R5              ;RETURN
1)	CSHCLR: IO10    CONO PAG,,0     ;MUST BE EVEN ADDRESS
1)	EBRADR: WD22    0               ;MUST BE ODD ADDRESS
1)	EBRFLG: .WORD   0               ;MUST BE EVEN ADDRESS
1)	;SEQ519
1)	.REM    %
1)	        ROUTINE TO READ THE CCW ADR.  RETURNS A POINTER TO THE
1)	22-BIT VALUE IN R0.
1)	%
1)	CCWRD:  PUSH    R1
1)	        CLR     ADRL            ;CLEAR ADDRESS BUFFER
1)	        CLR     ADRH            ;
1)	        MOV     #173,R1         ;FIRST DIAG FUNCTION
1)	        JSR     PC,CCWCHA       ;GET BITS 30-35
1)	        SWAB    R0              ;POSITION
1)	        ASR     R0              ;
1)	        BIC     #177700,R0      ;CLEAR GARBAGE
1)	        BIS     R0,ADRL         ;PUT IN ADDRESS BUFFER
1)	        JSR     PC,CCWCHA       ;GET BITS 24-39
1)	        SR      R0,3            ;POSITION
1)	        BIC     #170077,R0      ;CLEAR GARBAGE
1)	        BIS     R0,ADRL         ;PUT IN ADDRESS BUFFER
1)	        JSR     PC,CCWCHA               ;GET BITS 18-23
1)	        ASL     R0              ;POSITION
1)	        ROL     R0              ;UPPER 2 TO ADRH
1)	        ROL     ADRH            ;
1)	        ROL     R0              ;
1)	        ROL     ADRH            ;
1)	        BIC     #7777,R0        ;CLEAR GARBAGE
1)	        BIS     R0,ADRL         ;REST TO ADRL
1)	        JSR     PC,CCWCHA       ;GET BITS 14-17
1)	        SWAB    R0              ;POSITION
1)	        ASL     R0              ;
1)	        BIC     #177703,R0      ;CLEAR GARBAGE
1)	        BIS     R0,ADRH         ;PUT IN BUFFER
1)	        MOV     #ADRL,R0                ;POINT TO BUFFER
1)	        POP     R1
1)	        RTS     R5              ;RETURN
1)	CCWCHA: MOV     R1,R0           ;GET CURRENT DIAG FN.
1)	        DFRD                    ;DO DIAG READ
1)	        MOV     @.DAT2,R0               ;GET RESULT
1)	        INC     R1              ;NEXT FUNCTION
1)	        RTS     PC              ;5-19 IN R0
1)	;SEQ520
1)	.REM    %
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	        ROUTINE TO PERFORM A CBUS SELECT.  CHECKS FOR CBUS SEL
1)	XE & STEPS THE CLOCK UNTIL IT BECOMES TRUE.  R0 EQUALS THE
1)	CHANNEL NUMBER.  SETS THE C-BIT IF TIMEOUT.
1)	%
1)	CHSEL:  PUSH    <R1,R2>
1)	        BIC     #177770,R0      ;ONLY LEGAL CHANNELS
1)	        ADD     #170,R0         ;MAKE INTO DIAG READ FN.
1)	        MOV     R0,R1           ;SAVE FUNCTION
1)	        DFXCTT                  ;STOP THE CLOCK
1)	                STPCLK          ;
1)	        MOV     #48.,R2         ;MAX CLOCK COUNT
1)	1$:     DFRDT           ;READ FUNCTION 170
1)	                170             ;
1)	        TBIT    11              ;TEST CH T0
1)	        BEQ     2$              ;NOT SET
1)	        MOV     R1,R0           ;
1)	        DFRD                    ;READ PROPER FUNCTION
1)	        TBIT    12              ;TEST CBUS SEL XE
1)	        BNE     3$              ;MATCH?
1)	2$:     JSR     R5,MEMTST       ;NO, STEP CLOCK
1)	        DEC     R2              ;TIMEOUT?
1)	        BGT     1$              ;NO, TRY AGAIN
1)	        SEC                     ;SET C-BIT
1)	3$:     POP     <R2,R1>
1)	        RTS     R5              ;RETURN
1)	;SEQ521
1)	.REM    %
1)	        ROUTINE TO GET TO POINT IN CHANNEL TIMING WHERE C-BUS
1)	REQUESTS SHOULD APPEAR.  THIS IS 12 TICKS AFTER THE C-BUS SEL.
1)	(D TIME).  R0 EQUALSA THE CHANNEL NUMBER.  SETS THE C-BIT IF
1)	TIMEOUT.
1)	%
1)	CHSELD:         BIC     #177770,R0      ;ONLY LEGAL CHANNELS
1)	        ASL     R0              ;CHANNEL * 2
1)	        JMP     DSELCT(R0)      ;DISPATCH
1)	DSELCT: BR      CH0D
1)	        BR      CH1D
1)	        BR      CH2D
1)	        BR      CH3D
1)	        BR      CH4D
1)	        BR      CH5D
1)	        BR      CH6D
1)	        BR      CH7D
1)	CH0D:   MOV     #3,R0           ;CBUS SEL = 3
1)	        JMP     CHSEL           ;
1)	CH1D:   JSR     R5,TST8A                ;4,5 THIS SCAN?
1)	        BNE     1$              ;NO, 6,7
1)	        MOV     #4,R0           ;CBUS SEL = 4
1)	        JMP     CHSEL           ;
1)	1$:     MOV     #6,R0           ;CBUS SEL = 6
1)	        JMP     CHSEL           ;
1)	CH2D:   JSR     R5,TST8A        ;4,5 THIS SCAN?
1)	        BNE     1$              ;NO, 6,7
1)	        MOV     #5,R0           ;CBUS SEL = 5
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	        JMP     CHSEL           ;
1)	1$:     MOV     #7,R0           ;
1)	        JMP     CHSEL           ;
1)	CH3D:   CLR     R0              ;CBUS SEL = 0
1)	        JMP     CHSEL           ;
1)	CH4D:   MOV     #1,R0           ;CBUS SEL = 1
1)	        BR      CH4.5D  ;
1)	CH5D:   MOV     #2,R0           ;CBUS SEL = 2
1)	CH4.5D: JSR     R5,CHSEL                ;SELECT IT
1)	        BCS     1$              ;EXIT IF TIMEOUT
1)	        JSR     R5,TST8A        ;4,5 LAST SCAN?
1)	        BNE     1$              ;YES
1)	        JSR     R5,SCAN1        ;NO, DO 1 SCAN
1)	1$:     RTS     R5              ;RETURN
1)	CH6D:   MOV     #1,R0           ;CBUS SEL = 1
1)	        BR      CH6.7D          ;
1)	CH7D:   MOV     #2,R0           ;CBUS SEL = 2
1)	CH6.7D: JSR     R5,CHSEL                ;SELECT IT
1)	        BCS     1$              ;EXIT IF TIMEOUT
1)	        JSR     R5,TST8A                ;6,7 LAST SCAN?
1)	        BEQ     1$              ;YES
1)	        JSR     R5,SCAN1        ;NO, DO 1 SCAN
1)	1$:     RTS     R5              ;RETURN
1)	TST8A:  DFRDT           ;TEST CCW SEL 8A
1)	;SEQ522
1)	                175             ;
1)	        TBIT    13              ;
1)	        RTS     R5              ;RETURN WITH COND
1)	.REM    %
1)	        ROUTINE TO GENERATE A CBUS COMMAND.  ACCEPTS THE
1)	COMMAND WORD IN T1 & GENERATES IT FOR 4 CLOCKS.  FAST MODE
1)	MAY ALSO BE SET OR CLEARED.
1)	%
1)	CSIM:   PUSH    <R2,R1>
1)	        MOV     #LDCHAN,R0      ;DO FUNCTION
1)	        DFWRT                   ;
1)	        MOV     #4,R2           ;STEP CLOCK 4 TIMES
1)	        TST     TRCMEM          ;SELECT PRINT MEMORY?
1)	        BNE     1$              ;YES
1)	        MOV     R2,R0           ;NO, BURST CLOCK
1)	        JSR     R5,BRST1                ;NO, BURST CLOCK
1)	        BR      2$              ;
1)	1$:     JSR     R5,MEMTST       ;
1)	        DEC     R2              
1)	        BGT     1$
1)	2$:     MOV     #MODES+2,R1     ;FAST MODE BUFFER AREA
1)	        MOV     (SP),R0         ;GET FUNCTION DONE
1)	        MOVB    2(R0),(R1)      ;FAST TO BUFFER
1)	        BIC     #177677,(R1)    ;CLEAR EXTRA BITS
1)	        DFWRTT          ;CLEAR ALL EXCEPT FAST
1)	                MODES           ;
1)	                LDCHAN          ;
1)	        POP     <R1,R2>
1)	        RTS     R5              ;RETURN
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	MODES:  .WORD   0,0,0           ;BUFFER AREA
1)	;SEQ523
1)	.REM    %
1)	        GENERATES A SLOW RQ FOR THE CHANNEL IN R0.
1)	%
1)	SLOWRQ: JSR     R5,CHSEL                ;GET TO CBUS SELECT
1)	        BCS     3$              ;TIMEOUT
1)	        PUSH    <R1,R2>
1)	        MOV     #8.,R2          ;STEP CLOCK 8 TIMES
1)	        TST     TRCMEM          ;SELECT PRINT MEMORY?
1)	        BNE     1$              ;YES
1)	        MOV     R2,R0           ;NO, BURST CLOCK
1)	        BURST
1)	        BR      2$              ;
1)	1$:     JSR     R5,MEMTST               ;
1)	        DEC     R2              ;
1)	        BGT     1$              ;
1)	2$:     MOV     #SLOW,R1                ;SLOW BIT POINTER
1)	        JSR     R5,CSIM ;SIMULATE COMMAND
1)	        POP     <R2,R1>
1)	        CLC                     ;
1)	3$:     RTS     R5              ;RETURN
1)	SLOW:   WD36    0000,4000,0000
1)	.EVEN
1)	;SEQ524
1)	.REM   %
1)	        ROUTINE TO SIMULATE A REQUEST FOR CHANNEL R0.  THE EBUS
1)	DATA IS POINTED TO BY R1.  SETS THE C-BIT IF THE CORRECT CHANNEL
1)	CANNOT BE SELECTED.  RETURNS THE POINTER TO MEMORY REQUEST DATA
1)	IN R0 IF A MEMORY CYCLE WAS INITIATED.
1)	%
1)	SIMBUS: MOV     R5,BRKPC                ;SAVE PC
1)	        PUSH    R2
1)	        MOV    R0,R2           ;
1)	        DFXCTT                  ;STOP THE CLOCK
1)	                STPCLK          ;
1)	        MOV     R2,R0           ;
1)	        CLR     MEMRQ           ;CLEAR MEM RQ FLAG
1)	        CLR     R2              ;CLEAR SLOW RQ FLAG
1)	        BITB    #BIT7,2(R1)
1)	        BEQ     1$              ;NO
1)	        PUSH    R0
1)	        JSR     R5,SLOWRQ               ;GENERATE IT
1)	        POP     R0
1)	        BCS     3$              ;TIMEOUT
1)	        TSTB    3(R1)           ;ANY OTHER REQUESTS?
1)	        BEQ     2$              ;NO
1)	        INC     R2              ;YES, SET FLAG
1)	        BICB    #BIT7,2(R1)     ;CLEAR SLOW RQ BIT
1)	1$:     JSR     R5,CHSELD       ;GET TO D-TIME
1)	        BCS     2$              ;TIMEOUT
1)	        JSR     R5,CSIM         ;SIMULATE REQUEST
1)	        TST     R2              ;SLOW REQUEST?
1)	        BEQ     2$              ;NO
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

1)	        BISB    #BIT7,2(R1)     ;YES, RESTORE IT
1)	2$:     MOV     MEMRQ,R0                ;GET MEM STATUS
1)	3$:     POP     R2
1)	        RTS     R5              ;RETURN
1)	;SEQ525
1)	        ;STEPS THE CLOCK.  TESTS FOR LEADING EDGE OF RQ HOLD
1)	        ;IF TRCMEM SET.  GETS MEM STATUS IF EVENT DETECTED.
1)	MEMTST: TST     TRCMEM          ;SELECT PRINT MEMORY?
1)	        BNE     TSTMEM          ;YES, TRACE
1)	        JSR     R5,STEP1                ;STEP THE CLOCK
1)	        RTS     R5              ;RETURN
1)	TSTMEM: MOV     RQHOLD,R0       ;TEST FOR SBUS ADR HOLD
1)	        JSR     R5,EVENT1       ;
1)	        BCS     1$              ;NO MEM REF
1)	        JMP     GETMEM          ;GET MEM STATUS
1)	1$:     CLR     R0              ;
1)	        RTS     R5              ;RETURN
1)	        ;EITHER BURSTS THE CLOCK 24 TIMES OR SINGLE STEPS THE
1)	        ;CLOCK 24 TIMES WHILE TESTING FOR A MEMORY REFERENCE.
1)	SCAN1:  TST     TRCMEM          ;ASELECT PRINT MEMORY?
1)	        BEQ     2$              ;NO, DONT TRACE
1)	        PUSH    R2              
1)	        MOV     #24.,R2         ;CLOCK COUNT
1)	1$:     JSR     R5,TSTMEM               ;CHECK FOR MEM REF
1)	        DEC     R2              ;
1)	        BGT     1$              ;DONE?
1)	        POP     R2              ;YES
1)	        BR      3$              ;
1)	2$:     MOV     #24.,R0 ;BURST CLOCK 24
1)	        JSR     R5,BRST1        ;
1)	3$:     RTS     R5              ;RETURN
1)	TRCMEM: .WORD   0               ;MEMORY TRACE FLAG
1)	RQHOLD: S10.    162,31,1                ;SBUS ADR HOLD
1)	.SBTTL  *STOR11* DECSYSTEM10 PDP11 PROGRAM STORAGE SECTION
****
2)1	 
2)	 
2)	.REM    %
2)	CRRONE - ROUTINE TO LOAD A CACHE USE ALGORITHM TO ACCESS ONLY ONE
2)	CACHE.
2)	%
2)	CRRONE: MOV     R5,BRKPC                ;SAVE PC
2)	        JSR     R5,LDREF1               ;LOAD CODE
2)	        JSR     R5,RUNPR1               ;RUN IT
2)	        .WORD   2                       ;STARTING ADDRESS
2)	        RTS     PC
2)	 
2)	LDREF1: PUSH    R1
2)	        ROR     R0                      ;POSITION CACHE TO USE
2)	        ROR     R1
2)	        BIC     #177774,R0
2)	        MOV     R0,USEADR+2             ;PUT IN INDEX WORD
2)	        BIC     #77777,R1
2)	        MOV     R1,USEADR
File 1)	DSK:KBA.BU1	created: 1452 18-MAR-81
File 2)	DSK:KBA.ALL	created: 0915 19-MAR-81

2)	        POP     R1
2)	        JSR     R5,LOAD                 ;LOAD PROGRAM
2)	        .WORD   1
2)	        .WORD   -6
2)	        .WORD   USEADR
2)	        RTS     R5
2)	 
2)	USEADR: WD36    0,0,0
2)	        IO10    BLKO APR,,0,1           ;LOAD AN ADDRESS
2)	        I10     ADDI AC1,,4             ;NEXT ONE
2)	        I10     TRNE AC1,,774           ;DONE?
2)	        I10     JRST,,,2                ;NO, LOOP AGAIN
2)	        I10     JRST,4,,2               ;YES, HALT
2)	 
2)	 
2)	 
2)	.SBTTL  CACHE SWEEP ROUTINES
2)	.REM    %
2)	INVAL - INVALIDATES THE CACHE.
2)	%
2)	INVAL:  MOV     R5,BRKPC                ;SAVE PC
2)	        JSR     R5,LOAD                 ;LOAD PROGRAM
2)	        .WORD   1
2)	        .WORD   -4
2)	        .WORD   CSHSWP
2)	        JSR     R5,RUNPRT               ;RUN IT
2)	        .WORD   1                       ;STARTING ADDRESS
2)	        RTS     R5
2)	 
2)	        
2)	CSHSWP: IO10    DATAI CCA,,0
2)	        IO10    CONSZ APR,,200000
2)	        I10     JRST,,,2
2)	        I10     JSRT,4,,1
2)	.SBTTL  *STOR11* DECSYSTEM10 PDP11 PROGRAM STORAGE SECTION
**************
 l 
?