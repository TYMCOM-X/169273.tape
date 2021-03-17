;Name:          ALLOC.ASM
;Author:        Ward Christensen
;Written:       08/23/81
;Function:      Print allocation bit map of a disk
;Processor:     ASM or MAC
;Dependencies:  NONE
;Usage:

        ORG     100H
        JMP     BEGIN
PROMPT  CALL    EXIT
 DB CR,LF
 DB 'ALLOC   4/10/82     by Ward C.'
 DB CR,LF
 DB CR,LF
 DB '   ALLOC             show bit map.'
 DB CR,LF
 DB '   ALLOC B:          show bit map for drive B.'
 DB CR,LF
 DB '   ALLOC FOO.ASM     show bit map, with FOO.ASM as "*".'
 DB CR,LF
 DB '   ALLOC 22          zap (temporarily write-protect) 22K of map.'
 DB CR,LF
 DB '   ALLOC - 22        zap "all but" 22K.'
 DB CR,LF
 DB '   ALLOC - FOO.ASM   zap "all but" nnK, where nn = FOO.ASM size.'
 DB CR,LF
 DB CR,LF
 DB '   Notes:'
 DB CR,LF
 DB '   1)  Typing ^C restores the original map.'
 DB CR,LF
 DB '   2)  If interrupted, '
 DB              'ALLOC skips bit map and prints just totals.'
 DB CR,LF
 DB '   3)  Drive name prefix is ok:  "ALLOC - B:FOO.ASM"'
 DB CR,LF
 DB '   4)  "- " may precede or follow the operand:  "ALLOC 22 -"'
 DB CR,LF
 DB CR,LF,'$'

;History:
;               Based on my original alloc.asm idea,
;               but rewritten for CP/M 1.4-2.2 independence,
;               and to dynamically determine disk size.
;
;
;Revs:          (LAST FIRST)
;       04/10/82 Add negative allocations ("all but") and ability
;                to give implicit "nn" operand via filename.typ.
;                Also:  allow reexecution without reloading.
;                (Roy Lipscomb)
;       12/13/81 Check console status to interrupt display -    
;                particularly necess. on remote systems where
;                you may not want to see all the bit map.
;       12/08/81 re-compute max size - forgot to allow for dir.
;       10/03/81 Print "D"s for directory bits
;       09/13/81 Print total K on disk, not just "free"
;       08/25/81 add "nn" operand to "FORCE" allocation
;               of bits, i.e. so any file created on the disk
;               will be placed "further down".  A ^C restores the
;               original bit map.
;
;
;



;Init local stack
;
BEGIN   LXI     H,0             ;HL =
        DAD     SP              ;       CP/M STACK
        SHLD    STACK           ;SAVE FOR RETURN
        LXI     SP,STACK        ;SET NEW STACK
;
;DETERMINE IF INSTRUCTIONS, BITMAP ONLY, OR BITMAP PLUS ZAPPING WANTED.
        MVI     B,NO            ;SET FOR DEFAULT:  NOT "ALL BUT" OP

        LDA     FCB2+1
        CPI     '-'
        JZ      SETMIN1

        LDA     FCB+2
        CPI     ' '
        JNZ     SETMIN2

        LDA     FCB+1
        CPI     'H'
        JZ      PROMPT

        CPI     '-'
        JNZ     SETMIN2

        CALL    MOVFCB          ;OVERLAY '- ' WITH FILE NAME

SETMIN1 MVI     B,YES

SETMIN2 MOV     A,B
        STA     MINUS

;PRINT SIGNON PROMPT
;
        LXI     D,SIGNON        ;POINT TO SIGNON MSG
        MVI     C,PRINT         ;PRINT
        CALL    BDOS            ;       IT

;INITIALIZE WORK AREAS
        LXI     H,0
        SHLD    ZAP
        SHLD    ZAPHOLD

        LXI     H,2020H         ;INSERT SPACES INTO MESSAGES
        SHLD    ALOCDIR
        SHLD    ALOCUSD
        SHLD    ALOCFRE
        SHLD    ALOCTOT

        LXI     H,3020H         ;INSERT ' 0' INTO MESSAGES
        SHLD    ALOCDIR+2
        SHLD    ALOCUSD+2
        SHLD    ALOCFRE+2
        SHLD    ALOCTOT+2

        MVI     A,1
        STA     KPERBIT

        XRA     A
        STA     ROUND

        CALL    MAKMAP          ;CLEAR "MAP" AREA FOR GROUPS OF FILE.


;
;SEE IF SPECIFIC DISK REQUIRED
;
        LDA     FCB     ;GET DISK
        ORA     A       ;       DEFAULT?
        JZ      NODISK  ;       YES, NO SELECT

        DCR     A       ;ADJUST FOR LOGIN
        MOV     E,A     ;       PUT IN PARM REG
        MVI     C,LOGIN ;       SETUP REQUEST
        CALL    BDOS    ;       LOGIN REQUESTED DISK

;GET THE SIZE OF THE DISK
;
NODISK  MVI     C,GETVERS
        CALL    BDOS    ;GET VERSION
        MOV     A,L     ;L IS 0 IF 1.4
        ORA     A       ;ZERO?
        JNZ     ALOC22  ;       NO, IT'S 2.2
;
;THIS IS 1.4, SO GET PARM BLOCK FROM THE FRONT OF BDOS
;
        LHLD    BDOS+1  ;GET ADDR
        LXI     D,36H   ;OFFSET TO PARM
        DAD     D
        MOV     A,M     ;GET BLOCK SHIFT FACTOR
        INX     H
        INX     H
        MOV     E,M     ;GET DISK SIZE
        MVI     D,0
        INX     H       ;TO DIRECTORY BIT MAP SIZE
        MOV     B,M
        MVI     C,0
        JMP     ALOCCAL
;
; THIS IS 2.0 OR HIGHER
ALOC22  MVI     C,GETPARM
        CALL    BDOS    ;GET DISK PARM ADDR
        INX     H       ;SKIP
        INX     H       ;       TO
        MOV     A,M     ;       #K PER BIT
        INX     H       ;SKIP
        INX     H       ;       TO
        INX     H       ;       TOTAL
        MOV     E,M     ;       DISK
        INX     H       ;       SIZE
        MOV     D,M     ;MOVE IT TO DE
        INX     H
        INX     H
        INX     H
        MOV     B,M     ;DIR BIT MAP
        INX     H
        MOV     C,M     ;       TO BC

;SAVE DISK PARAMETERS, INITIALIZE VARIABLES
ALOCCAL STA     BSH             ;SAVE BLOCK SHIFT

        CALL    COMPDIR         ;COMPUTE DIR SIZE
        INX     H               ;FUDGE HL TO # OF BITS IN MAP
        SHLD    TOTBITS         ;SAVE FOR PRINT OF TOTAL


;CHECK COMMAND USAGE
;
        LDA     FCB+1
        CPI     ' '     ;OPERAND IS BLANK?
        JZ      PASSTWO ;       YES, NO OPTIONS

;INITIALIZE ZAPHOLD
        CALL    GETDEC  ;GET DECIMAL # TO ZAP OR (IF "ALL BUT") NOT ZAP
        CC      GETGRPS ;IF NON-DIGIT FOUND, TREAT AS FILENAME

;PASS ONE:  DON'T ZAP YET, JUST COMPUTE FREE GROUPS AND "ALL BUT" COUNT
;(SKIP IF NOT "ALL BUT" OPTION)
        LDA     MINUS
        CPI     YES
        JNZ     PASSTWO

        MVI     A,1
        STA     PASS

        CALL    BMAP    ;(RETURNS WITH FREE COUNT IN BC)

        PUSH    B
        POP     H
        CALL    GRPS2K  ;CONVERT HL (CONTAINS GROUP COUNT) INTO K

        CALL    INVERTZ ;SUBTRACT ZAPHOLD FROM HL, PUT IN ZAPHOLD

;PASS TWO:  ZAP BITS IF REQUESTED; PRINT BIT MAP
PASSTWO EQU     $
        MVI     A,2
        STA     PASS

        LHLD    ZAPHOLD
        SHLD    ZAP

        CALL    BMAP

        JMP     ALOCASC ;DONE, COMPUTE IN ASCII



;PROCESS BIT MAP

;INITIALIZE
BMAP    LXI     D,GROUPS-1 ;COMPUTE FIRST AVAILABLE GROUP AFTER
                           ;DIRECTORY (SUBTRACT 1 TO COUNTERACT INITIAL
                           ;"INX H" BELOW).
        LHLD    DIRSIZE
        MVI     H,0
        DAD     D
        SHLD    MAPINX

        LHLD    ZAP     ;(WILL BE ALL ZEROS ON PASS ONE)
        CALL    K2GRPS  ;CONVERT K TO GROUPS:  ZAP = ZAP/(#K/BIT)
        SHLD    ZAP

        CALL    CRLF0   ;RESET COLUMN COUNT

;DO ZAP
        LHLD    TOTBITS
        LDA     DIRSIZE ;BACK OUT
        CMA             ;       DIR BITS
        MOV     E,A     ;       FROM
        MVI     D,0FFH  ;       TOTAL
        INX     D       ;       DE = NEG DIR BITS
        XCHG
        DAD     D       ;       ADD TO HL

        PUSH    H       ;SAVE # BITS

        PUSH    D       ;SAVE WITHOUT DIR SUBTRACTED
        MVI     C,INQALC
        CALL    BDOS    ;GET ALLOCATION POINTER INTO HL
        POP     D       ;GET BIT MAP COUNT

        PUSH    H       ;SAVE POINTER TO ALLOCATION VECTOR
        CALL    ZAPEM   ;ZAP REQUESTED BITS
        CALL    PRINTD  ;PRINT "D"'S, SET UP MASK IN A
        POP     H

        POP     D       ;GET # WITH DIR SUBTRACTED

        LXI     B,0     ;INIT #K FREE

;PROCESS-BIT LOOP
ALOCBIT PUSH    PSW     ;SAVE MASK

        PUSH    D       ;SAVE COUNTDOWN OF BITS IN ALLOCATION VECTOR
        PUSH    H       ;SAVE POINTER TO ALLOCATION VECTOR
        INX     B       ;INCREMENT COUNT OF FREE

        ANA     M       ;FREE?
        MVI     A,'0'   ;       SETUP TO PRINT '0'

        LHLD    MAPINX  ;INDEX TO ENTRY IN MAP OF FILE'S GROUPS
        INX     H
        SHLD    MAPINX

        JZ      ALOCTYP ;       YES, PRINT '0'

;PROCESS "USED" BIT.
        DCX     B               ;ADJUST COUNT OF FREE

        MOV     A,M             ;GET FLAG FROM GROUP MAP
        ORA     A               ;IS IT GROUP FOR SPECIFIED FILE?
        JNZ     ALOCTYP         ;(YES IF A <> 0)

        MVI     A,'1'           ;  NO, SET TO "1"

;PRINT BIT
ALOCTYP PUSH    B
        CALL    STYPE   ;TYPE IT (Unless interrupted)

        LDA     COL     ;BUMP
        INR     A       ;       OUTPUT
        STA     COL     ;       COLUMN,

        CPI     50      ;       AND CRLF
        CZ      SCRLF   ;       AFTER 50

        POP     B
        POP     H
        POP     D

        DCX     D       ;COUNT DOWN # BITS
        MOV     A,D
        ORA     E
        JZ      BMAPX

        POP     PSW     ;GET MASK
        CALL    RIGHT
        JMP     ALOCBIT

;EXIT FROM BIT-MAP CREATION
BMAPX   POP     PSW
        RET

;
;MAKE (A) (MASK) AND (HL) (POINTER) POINT TO NEXT BIT
;
RIGHT   ORA     A       ;NO CARRY
        RAR             ;RIGHT 1 BIT,
        RNC             ;       RET IF NOT INTO CARRY
        RAR             ;       OTHERWISE MAKE IT 80H
        INX     H       ;TO NEXT BYTE
        RET


;CONVERT POSITIVE K INTO "ALL BUT" K, BY SUBTRACTING FROM NUMBER FREE.
;(ON ENTRY, HL = #K)
INVERTZ PUSH    H
        POP     B

        LHLD    ZAPHOLD ;GET AMOUNT TO PROTECT FROM ZAPPING,

        MOV     A,C     ;SUBTRACT FROM TOTAL FREE (IN BC)
        SUB     L
        MOV     L,A

        MOV     A,B
        SBB     H
        MOV     H,A

        JNC     INVERT2         ;IF # TO SAVE IS LARGER THAN # FREE,
        LXI     H,0             ;SAVE ALL FROM BEING ZAPPED

INVERT2 SHLD    ZAPHOLD ;NUMBER TO ZAP, TO LEAVE DESIRED NUMBER FREE
        RET



;
;DONE; BC = # BITS OFF, CONVERT TO K, THEN TO ASCII
;
;
;FIRST, MULTIPLY THE # OF BITS THAT WERE 0,
;       BY THE # OF K PER BIT
;
ALOCASC LDA     BSH     ;GET BLOCK SHIFT
        MOV     D,B
        MOV     E,C     ;MOVE COUNT TO DE
        XCHG            ;THEN TO HL
        SUI     3       ;IS IT 1K?
        JZ      ALOCASB ;YES

;DOUBLE # OF BITS FREE, TOTAL DISK SIZE, AND K/BIT.
ALOCINK DAD     H       ;DOUBLE # OF BITS FREE

        PUSH    H       ;DOUBLE DISK SIZE
        LHLD    TOTBITS
        DAD     H
        SHLD    TOTBITS
        POP     H

        PUSH    PSW             ;DOUBLE K/BIT
        LDA     KPERBIT
        ADD     A       ;DOUBLE
        DAA             ;CONVERT TO PACKED DECIMAL
        STA     KPERBIT
        POP     PSW
        DCR     A       ;COUNT DOWN
        JNZ     ALOCINK

ALOCASB SHLD    TOTFREE ;SAVE FOR FINAL CALC
        XCHG            ;BIT COUNT TO DE
        LXI     H,ALOCFRE+3 ;INIT FOR "ADD1"

ALOCASL MOV     A,D     ;DOWN TO
        ORA     E       ;       0 LEFT?
        JZ      ALOCPRT ;       YES, PRINT IT

        PUSH    H       ;SAVE POINTER
        CALL    ADD1    ;ADD 1 IN ASCII
        POP     H
        DCX     D       ;DCR COUNT
        JMP     ALOCASL
;
;READY TO PRINT, CONVERT PACKED DECIMAL K PER BIT TO ASCII
;
ALOCPRT LXI     H,KPERBIT
        MOV     A,M     ;GET VALUE
        MVI     M,' '   ;BLANK IT
        CPI     10
        JC      PRTLT10 ;IT IS LESS THAN 10
        PUSH    PSW     ;SAVE FOR LATER
        RAR ! RAR ! RAR ! RAR
        ANI     0FH     ;GET DIGIT
        ORI     '0'     ;MAKE ASCII
        MOV     M,A     ;STORE IT
        POP     PSW     ;GET LOW ORDER
        ANI     0FH     ;MAKE 0-9
PRTLT10 ORI     '0'     ;CONVERT TO ASCII
        INX     H
        MOV     M,A
;
;NOW, FORMAT #K USED, AND TOTAL DISK SIZE
;
        LHLD    TOTBITS
        PUSH    H               ;SAVE TOTAL
        XCHG
        LXI     H,ALOCTOT+3
        CALL    ASCICON ;CONV TO ASCII
        LHLD    TOTBITS ;GET TOT
        XCHG            ;TO DE
        LHLD    TOTFREE ;GET # FREE
        MOV     A,E
        SUB     L
        MOV     E,A
        MOV     A,D
        SBB     H
        MOV     D,A     ;DE = TOT USED
;
;SUBTRACT OUT DIRECTORY SIZE
;       
        LDA     DIRK
        CMA
        MOV     L,A
        MVI     H,0FFH
        INX     H               ;MAKE 2'S COMPLEMENT
        DAD     D
        PUSH    H               ;SAVE #K USED
        XCHG
        LXI     H,ALOCUSD+3
        CALL    ASCICON
;
;COMPUTE % FULL: (USED*100)/TOT
;
        POP     B               ;GET # USED
        MOV     H,B
        MOV     L,C             ;COPY TO HL
        MVI     E,0             ;INIT "OVERFLOW" BYTE
        CALL    X2              ;X2
        INX     H               ;ROUND UP .5K
        CALL    A1              ;+1=3
        CALL    X2              ;X6
        CALL    X2              ;X12
        CALL    X2              ;X24
        CALL    A1              ;X25
        CALL    X2              ;X50
        CALL    X2              ;X100
;
;E,H,L HOW CONTAIN 24 BIT "100 TIMES #K USED".
;
        POP     B               ;GET TOTAL K ON DISK
        MOV     A,C
        CMA
        MOV     C,A
        MOV     A,B
        CMA
        MOV     B,A
        INX     B               ;BC = NEGATIVE
        MVI     D,0FFH          ;INIT QUOTIENT TO -1
;
DIVIDE  INR     D               ;BUMP QUOTIENT
        DAD     B               ;"SUBTRACT"
        MOV     A,E
        ACI     0FFH            ;EXTEND "SUBTRACT" TO 24 BITS
        MOV     E,A
        JC      DIVIDE
        MOV     E,D
        MVI     D,0
        LXI     H,PERCENT+1
        MVI     M,' '
        INX     H
        MVI     M,' '
;
;NOW CONVERT TO DECIMAL, HL POINTS TO LOW ORDER ASCII
;
        CALL    ASCICON
;
;PRINT THE MESSAGE, SUPPRESSING REDUNDANT ' ' SO
;       IT "LOOKS NICER"
;
        LXI     H,STATS
        MVI     B,' '   ;INIT PREV CHAR
NBPR    MOV     A,M     ;GET CHAR
        INX     H
        ORA     A       ;DONE? (=00)
        JZ      DONE    ;       YES, EXIT
        CPI     ' '     ;SPACE?
        JNZ     NBNSP   ;       NO
        CMP     B       ;COMPARE TO PREV CHAR
        JZ      NBPR
NBNSP   MOV     B,A     ;SAVE FOR NEXT COMPARE
        CPI     '('     ;TREAT '(' AS SPACE SO
        JNZ     NBTYPE  ;       "(nK" is not "( nK"
        MVI     B,' '
NBTYPE  CALL    TYPE
        JMP     NBPR
;
;MULTIPLY E-H-L BY 2
;
X2      DAD     H               ;MULTIPLY HL BY 2, CARRY?
        MOV     A,E             ;GET 3
        RAL                     ;SHIFT, WITH CARRY IF ON
        MOV     E,A
        RET
;
;ADD BC TO E-H-L
;
A1      DAD     B
        RNC
        INR     E
        RET
;
STATS   DB      CR,LF
KPERBIT DB      1,' K/bit; '    ;1 IS CONVERTED TO ASCII
ALOCDIR DB      '   0K dir. + '
ALOCUSD DB      '   0K files +'
ALOCFRE DB      '   0K free = '
ALOCTOT DB      '   0K total.',CR,LF
PERCENT DB      ' ??% full',cr,lf,0
;
;COMPUTE THE DIRECTORY SIZE
;
COMPDIR PUSH    PSW
        MOV     H,B
        MOV     L,C     ;BIT MAP TO HL
        MVI     B,16    ;# OF SHIFTS
        MVI     C,0     ;# OF BITS IN DIR
CDSHFT  DAD     H       ;SHIFT OFF A BIT
        JNC     CDNOCY
        INR     C
CDNOCY  DCR     B
        JNZ     CDSHFT
        MOV     A,C     ;GET BITS/DIR
        STA     DIRSIZE
        XCHG
        POP     PSW     ;GET SHIFT FACTOR
        PUSH    PSW
        PUSH    H
        SUI     3       ;=0 IF 1K BLOCKS
        MOV     C,A     ;SHIFT COUNT TO C
        LDA     DIRSIZE
        MOV     B,A
        JZ      CDADD   ;GO ADD IT IN
CDSHK   ADD     A       ;MULT BY 2
        MOV     B,A     ;SAVE FOR ADD LOOP LATER
        DCR     C       ;MORE?
        JNZ     CDSHK   ;SHIFT TO COMPUTE #K
        MOV     A,B
        STA     DIRK
CDADD   LXI     H,ALOCDIR+3
        CALL    ADD1    ;ADD 1 IN ASCII
        DCR     B
        JNZ     CDADD
        POP     H
        POP     PSW
        RET
;
;PRINT A 'D' FOR EVERY DIR ENTRY BIT
;       AND ADJUST STARTING BIT MAP

PRINTD  LDA     DIRSIZE
        MOV     C,A
        MVI     B,80H   ;INIT MASK

CDPRTD  LDA     PASS
        CPI     2       ;PRINT IF PASS TWO
        MVI     A,'D'
        CZ      TYPE

        LDA     COL
        INR     A
        STA     COL

        MOV     A,B     ;GET MASK
        CALL    RIGHT   ;ROTATE RIGHT,
        MOV     B,A     ;       BUMPS H IF OVERFLOW

        DCR     C
        JNZ     CDPRTD

        MOV     A,B     ;GET INITIAL MASK
        RET
;
;EXIT PRINTING MSG FOLLOWING CALL EXIT
;
EXIT    POP     D
        MVI     C,PRINT
        CALL    BDOS
DONE    LHLD    STACK
        SPHL            ;RESTORE IT
        RET
;
;
;CONVERT GROUPS (IN HL) INTO K
GRPS2K  LDA     BSH     ;RESTORE BLOCK-SHIFT FACTOR

GRPLP   CPI     3       ;DOWN TO 1K?
        RZ
        DAD     H       ;MULTIPLY HL BY 2
        DCR     A
        JMP     GRPLP

;
;
;====>  K2GRPS  TAKES HL, WHICH IS # OF K TO
;               ALLOCATE, AND CONVERTS IT INTO
;               THE # OF BITS TO TURN OFF
;
K2GRPS  LDA     BSH     ;RESTORE BLOCK-SHIFT FACTOR

K2GP1   CPI     3       ;DOWN TO 1K?
        JZ      K2GP9
        CALL    HALFH   ;DIVIDE HL IN HALF
        DCR     A
        JMP     K2GP1
;
;DONE:  PUT OUT MESSAGE IF ROUNDED DOWN
K2GP9   LDA     ROUND
        ORA     A
        RZ                      ;RETURN IF NO ROUNDING

        LDA     PASS            ;DON'T PRINT MESSAGE DURING PASS 1
        CPI     1
        RZ

        LXI     D,ROUNDMS
        MVI     C,PRINT
        PUSH    H
        CALL    BDOS
        POP     H

        RET
;
ROUNDMS DB      '++ K to "zap" rounded down '
        DB      'to whole blocks ++',CR,LF,'$'
;
;DIVIDE HL BY 2
;
HALFH   PUSH    PSW     ;SAVE
        ORA     A       ;CLEAR CARRY
        MOV     A,H
        RAR
        MOV     H,A     ;H= .5 H
        MOV     A,L
        RAR
        MOV     L,A     ;L= .5 L
        LDA     ROUND   ;GET PREV ROUND (0 IF NONE)
   ACI     0       ;ADD 1 IF CARRY SET
        STA     ROUND   ;STORE BACK, 0 IF NO ROUND
        POP     PSW
        RET
;
;====>  ZAPEM   TURNS OFF THE REQUESTED # OF BITS IN MEMORY
;
ZAPEM   LDA     PASS    ;EXIT UNLESS PASS TWO
        CPI     2
        RNZ

        PUSH    D       ;SAVE # BITS IN MAP
        PUSH    H       ;SAVE POINTER TO BITMAP
        LHLD    ZAP     ;GET # TO ZAP OFF
        MOV     B,H     ;BC
        MOV     C,L     ;       = # TO ZAP
        POP     H       ;HL = POINTER
        PUSH    H
        MVI     A,80H   ;INITIAL BIT MASK
ZAPLP   PUSH    PSW     ;SAVE MASK
        ANA     M       ;CANDIDATE TO ZAP?
        JNZ     ZAPNO   ;       NO
;
;GOT ONE TO ZAP, ANY MORE REQUESTED?
;
        MOV     A,B     ;IS BC
        ORA     C       ;       =0?
        JZ      ZAPPED  ;       YA, WE ARE DONE
        DCX     B       ;       NO, SO COUNT IT
        POP     PSW     ;GET MASK
        PUSH    PSW     ;SAVE BACK
        ORA     M       ;TURN ON (TUNE IN, DROP OUT?)
        MOV     M,A     ;PUT BACK
;
;POINT TO NEXT BIT
;
ZAPNO   DCX     D       ;DECREMENT BIT MAP SIZE
        MOV     A,D     ;IS DE
        ORA     E       ;       =0?
        JZ      ZAPPED  ;       YES, WE ARE DONE
;
;NOT DONE, FIND NEXT BIT TO TEST
;
        POP     PSW     ;GET MASK
        ORA     A       ;CLEAR CARRY
        RRC             ;SHIFT AROUND, AND TO CARRY
        JNC     ZAPLP   ;LOOP IF STILL IN BYTE
        INX     H       ;TO NEXT BYTE
        JMP     ZAPLP
;
ZAPPED  POP     PSW     ;DELETE BIT MASK
        POP     H       ;PUT THINGS
        POP     D       ;       BACK
        RET

;MOVE FCB2'S DISK AND NAME TO FCB1
MOVFCB  MVI     B,12
        LXI     D,FCB
        LXI     H,FCB2
MOVFCB1 MOV     A,M
        STAX    D
        INX     D
        INX     H
        DCR     B
        JNZ     MOVFCB1
        RET
;


;CLEAR A LARGE AREA, TO USE AS MAP FOR SPECIFIED FILE (IF ANY)
MAKMAP  LXI     D,8*1024        ;8K (1 GROUP PER BYTE) SHOULD SUFFICE
        LXI     H,GROUPS
        MVI     B,0

MAKMAP1 MOV     M,B     ;CLEAR GROUP AREA FOR THIS FILE
        INX     H
        DCX     D
        MOV     A,D
        ORA     E
        JNZ     MAKMAP1

        RET


;FIGURE DIRECTORY BYTES PER GROUP-NUMBER AND GROUP-NUMBERS PER ENTRY.
GETGRPS LHLD    TOTBITS
        DCX     H

        MOV     A,H
        ORA     A               ;IS DISK CAPACITY <= 256 GROUPS?

        MVI     H,1
        MVI     L,16
        JZ      GRPS0           ;  YES, 1 BYTE/GROUP, 16 GROUPS/ENTRY

        MVI     H,2             ;  NO, 2 BYTES/GROUP, 8 GROUPS/ENTRY
        MVI     L,8

GRPS0   SHLD    BGE             ;SAVE BYTES/GROUP AND GROUPS/BYTE

;BUILD MAP FOR THIS FILE
        LXI     D,FCB
        MVI     A,'?'
        STA     FCBEXT          ;SET FOR NEXT DIR ENTRY OF THIS FILE

        MVI     A,SEARCH
        STA     SERCHCD         ;FIRST SEARCH ON THIS FILE
        JMP     GRPS5

;GROUP-SEARCHING LOOP
GRPS1
        LHLD    BGE             ;LOAD BYTES/GROUP AND GROUPS/ENT PARMS
        MOV     B,H             ;BYTES/GROUP TO B
        MOV     C,L             ;GROUPS/ENTRY TO C

        DCR     A
        MOV     L,A
        MVI     H,0

        DAD     H       ;X 32 TO GET OFFSET OF DIRECTORY ENTRY
        DAD     H
        DAD     H
        DAD     H
        DAD     H

        LXI     D,TBUFF         ;ADDRESS DESIRED DIRECTORY ENTRY
        DAD     D

        LXI     D,16
        DAD     D               ;ADDRESS DIR ENTRY'S GROUP NUMBERS

        MVI     D,0             ;INITIALIZE HIGH BYTE OF GROUP NUMBER

;COUNT THIS GROUP
GRPS3   MOV     E,M
        INX     H               ;GET GROUP NUMBER INTO DE

        MOV     A,B
        DCR     A
        JZ      GRPS4           ;IF 1 BYTE/GROUP-NUMBER, SKIP

        MOV     D,M
        INX     H

        MOV     A,D
GRPS4   ORA     E
        JZ      GRPS5           ;IF END OF GROUPS, QUIT THIS ENTRY

        PUSH    H

        LXI     H,GROUPS
        DAD     D
        MVI     M,FILMRK        ;SET ON "BIT" IN THIS FILE'S MAP

        LHLD    ZAPHOLD         ;INCREMENT COUNT OF FILE'S GROUPS
        INX     H
        SHLD    ZAPHOLD

        POP     H

        DCR     C               ;COUNTDOWN GROUPS IN THIS ENTRY
        JNZ     GRPS3

;SEARCH FOR NEXT ENTRY FOR FILENAME
GRPS5
        LDA     SERCHCD
        MOV     C,A
        MVI     A,SERCHNX
        STA     SERCHCD         ;NEXT SEARCH SHOULD BE "SEARCH NEXT"
        CALL    BDOS
        INR     A
        JNZ     GRPS1           ;MORE ENTRIES? CONTINUE

;ALL FILENAME'S ENTRIES ACCOUNTED FOR
        LDA     MINUS
        CPI     YES
        JZ      GRPS9

        LXI     H,0
        SHLD    ZAPHOLD         ;SET ZAP COUNT TO ZERO UNLESS "ALL BUT".
        RET

GRPS9   LHLD    ZAPHOLD
        CALL    GRPS2K          ;CONVERT GROUP COUNT INTO K COUNT
        SHLD    ZAPHOLD
        RET




;====>  GETDEC  GETS DECIMAL VALUE, STORES IN HL.
;               SETS CARRY IF ERROR.
;               BC POINTS TO INPUT.  SPACE TERMINATES
;
GETDEC  LXI     H,0     ;INIT RESULT
        LXI     B,FCB+1 ;INIT BC TO OPTION STRING
GETDLP  LDAX    B       ;TO NEXT
        INX     B

        CPI     ' '     ;END DELIM?
        JNZ     GETDLP1 ;  NO, CONTINUE
        SHLD    ZAPHOLD
        RET

GETDLP1 CPI     '0'     ;BAD?
        RC              ;       YES, RET W/CARRY
        CPI     '9'+1   ;SET CARRY IF 0-9
        CMC             ;FLIP CARRY
        RC              ;RET IF NOT 0-9

;GOT 0-9, MULT PREV BY 10
        MOV     D,H     ;SET UP
        MOV     E,L     ;       FOR MULT
        DAD     H       ;X2
        DAD     H       ;X4
        DAD     D       ;X5
        DAD     H       ;X10
;ADD IN THIS DIGIT
        SUI     '0'     ;DELETE ASCII BIAS
        ADD     L       ;ADD IT IN
        MOV     L,A     ;PUT BACK
        JNC     GETDLP  ;LOOP IF NO CARRY TO H
        INR     H       ;PROPAGATE CARRY
        JMP     GETDLP  ;       THEN LOOP

;
;TYPE CRLF IF HAVEN'T BEEN INTERRUPTED, AND IF NOT PASS 1
;
SCRLF   LDA     INTERR
        ORA     A
        JNZ     CRLF0   ;JUST RESET COLUMN

        LDA     PASS
        CPI     1
        JZ      CRLF0   ;SKIP IF PASS 1
;
;CR/LF, AND SET COLUMN TO 0
;
CRLF    MVI     A,CR
        CALL    TYPE
        MVI     A,LF
        CALL    TYPE
CRLF0   XRA     A
        STA     COL
        RET
;
;CONVERT NUMBER IN DE TO ASCII, STORE AT (HL)
;       (HL POINTS TO LOW-ORDER ASCII DIGIT)
;
ASCICON PUSH    H       ;SAVE LOW ORDER
ASCICLP POP     H
        MOV     A,D
        ORA     E
        RZ              ;RET IF DONE
        PUSH    H
        CALL    ADD1    ;BUMP IN ASCII
        DCX     D       ;DECREMENT DECIMAL COUNT
        JMP     ASCICLP
;
;ADD 1 IN ASCII
;
ADD1    MOV     A,M
        ORI     '0'
        INR     A
        MOV     M,A
        CPI     '9'+1
        RNZ
        MVI     M,'0'
        DCX     H
        JMP     ADD1
;
;TYPE CHAR UNLESS INTERRUPT FLAG SET, OR PASS 1
;
STYPE   PUSH    PSW     ;SAVE CHAR
        LDA     INTERR
        ORA     A       ;PREVIOUSLY INTERRUPTED?
        JNZ     SNOTYPE ;       YES, DON'T TYPE

        LDA     PASS
        CPI     1
        JZ      SNOTYPE ;SKIP IF PASS 1

        POP     PSW
        JMP     TYPE    ;TYPE THE CHARACTER
;
SNOTYPE POP     PSW
        RET
;
;TYPE CHAR IN A, SAVE ALL REGS;
;       TEST FOR BEING INTERRUPTED;
;       SAVE CHAR IN "INTERR";
;       ABORT IF ^C
;
TYPE    PUSH    B
        PUSH    D
        PUSH    H
        MVI     C,WRCON ;WRITE
        MOV     E,A     ;       THE
        CALL    BDOS    ;       CHAR
        MVI     C,CONST ;TEST CONSOLE
        CALL    BDOS    ;       STATUS
        ORA     A       
        JZ      NOCONST ;IF NONE, RETURN
        MVI     C,RDCON ;READ THE CHAR  
        CALL    BDOS
        STA     INTERR
        CPI     'C'-40H ;^C?
        JZ      DONE
NOCONST POP     H
        POP     D
        POP     B
FUNCT   RET
;
SIGNON  DB      'ALLOC as of 04/10/82. '
        DB      'Type ALLOC H for help',CR,LF,'$'
ZAP     DW      0       ;# BITS TO ZAP
ROUND   DB      0       ;ROUNDING LOSS?
INTERR  DB      0       ;NON-0 IF INTERRUPTED
COL     DB      0       ;CR/LF AFTER 50
ZAPHOLD DW      0

MINUS   DS      1       ;IS MINUS-OPTION BEING INVOKED? (YES OR NO)
PASS    DS      1       ;PASS 1 DOESN'T PRINT OR ZAP, JUST COMPUTES
BSH     DS      1       ;SAVE AREA FOR BLOCK SHIFT FACTOR
BGE     DS      2       ;# BYTES USED IN GIVING GROUP NUMBER (1 OR 2),
                        ; AND # GROUPS IN ONE DIR ENTRY (16 OR 8)
SERCHCD DS      1       ;"SEARCH FIRST" OR "SEARCH NEXT" FUNCTION CODE
DIRSIZE DS      1       ;BITS ON FOR DIRECTORY
DIRK    DS      1       ;#K IN DIRECTORY
TOTFREE DS      2
TOTBITS DS      2
MAPINX  DS      2
        DS      200
STACK   DS      2
;
;BDOS EQUATES
;
RDCON   EQU     1
PRINT   EQU     9
CONST   EQU     11      ;0BH
LOGIN   EQU     14      ;0EH    0=A
GETVERS EQU     12      ;0CH
SEARCH  EQU     17
SERCHNX EQU     18
GETPARM EQU     31      ;1FH
INQALC  EQU     27      ;1BH
WRCON   EQU     2

;
BDOS    EQU     5
FCB     EQU     5CH 
FCBEXT  EQU     FCB+12
FCB2    EQU     FCB+16
TBUFF   EQU     80H
CR      EQU     0DH
LF      EQU     0AH

FILMRK  EQU     '*'             ;CHAR TO MARK FILE'S GROUPS IN BIT MAP
NO      EQU     0
YES     EQU     1

;START OF AREA USED IN MAPPING GROUPS FOR OPERAND
GROUPS  EQU     $
   nnD^Ø