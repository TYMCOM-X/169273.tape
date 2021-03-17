
;                    DU.ASM V8.7 Revised 05/14/84
;         Disk Utility - Written by Ward Christensen 08/06/78
;
;       (See DU.DOC for description and detailed instructions.)
;
; This version of DU is compatible with CP/M 1.4, 2.x and 3.x and does
; not require alteration for various hardware configurations.  It ad-
; justs itself automatically to the correct number of sectors, tracks,
; directory size, etc.
;
; Because of the automatic adaption feature, no conditional assembly op-
; tions are included.  The only alteration that needs to be done is to
; use DDT to set the byte at 103H for your clock speed. Use 0 for 2MHz,
; 1 for 4MHz, 2 for 6MHz.  (This only affects the 'Z' SLEEP command.)
;
;----------------------------------------------------------------------
;
; This program has been heavily modified to allow it to work without
; modification on most versions of CP/M 1.4 and, hopefully, all versions
; of CP/M 2.x and 3.x.  If you have difficulty getting this program to
; run, AND if you are using CP/M 2.x or 3.x, AND if you know your BIOS
; to be bug-free, leave a message on Technical CBBS of Dearborn, Michi-
; gan (313)-846-6127 with a description of the problem and a summary of
; your hardware configuration.  One known possible problem involves the
; system tracks on some systems, and results from the system sectors
; being skewed. There is NO way for a program executing under CP/M to
; know about this.  This program assumes the standard convention of no
; skew being used on the system tracks. This usually isn't a problem be-
; cause the SYSGEN program can be used to get the system from the disk
; so that it can be modified.  This program should work under standard
; versions of CP/M 1.4.  The only requirement is that the BIOS "SETSEC"
; routine not modify the sector number passed to it in the 'B' register.
; Again, system tracks with skewed sectors will be a problem.
;
;               If you add any features or make any useful
;               changes to this program, please modem a copy
;               to the above CBBS, so the currency of the
;               program can be maintained.
;                                       - Ron G. Fowler
;
;-----------------------------------------------------------------------
; 05/14/84  Restored jump at the end of the VIEW routine from
;           "JMP  PROMPT" to "JMP  CLCGRP" (as in DU-V78) so that
;           allocation groups are correctly displayed.
;                                       - Bill Duerr
;
;
; 10/24/83  The :M change added to v8.3 was made after consultation with
;   v8.6    Ward Christensen (the original author), Dave Hardy, and some
;           others.  It was changed by the person updating v8.5, but is
;           now restored to the author's original intent.  Personal com-
;           ments deleted from v8.5 update since they served no useful
;           purpose.  Restored help format used in v8.3 and v8.4.  Other
;           minor changes.              - Irv Hoff
;
; 10/23/83  Radix escape ("#") extended to include both decimal and hex
;   v8.5    values.  Added "Mxx" command operand range check.  xx omitted
;           or less than the first data group now starts with the first
;           data group.   xx greater than the last group now gives "not
;           within tracks" error.  Previously "M" would loop.  "M" output
;           delimiters and CRLFs now properly handled.  Termination of
;           "M" command by ^C now handled properly; stack had HL pushed
;           twice.  TOS is supposed to be INnBUF pointer, but TOS was
;           garbage and INBUF pointer was at TOS+2.  Help updated to re
;           flect the correct default radix on prototype commands.   Out-
;           put of "#" command revised, now includes both decimal and
;           hex.  Fixed long standing bug in "M".  If disk was not posi-
;           tioned within a group (e.g. disk was positioned within a
;           system track), disk would be improperly positioned after "M"
;           was executed.  This occured because "M" was calling POSGP2,
;           but the disk wasn't within a group.  In this case, "M" now
;           uses track/sector positioning.  Restored the "(" toggle to
;           the way it was before it got changed in v8.3.
;                                       - Peter H. Haas
;
; 10/21/83  Added command 'K'.  This allows you to save a file from the
;   v8.4    'YANK' sequential memory without leaving DU.  Intended for
;           use under 3.0, which doesn't have a save command, but works
;           fine for 2.2.  I haven't tested it for 1.4.
;                                       - Jeffrey J. Nonken
;
; 10/18/83  Changed the map toggle to conform with original intent to
;   v8.3    show erased files.  (Includes an '*' if that block has been
;           written over by another file.)  Can be toggled off with '('
;           prior to asking for 'M'.  Also added a change suggested by
;           Sigi Kluger.                - Irv Hoff
;
; 10/15/83  Combined DU-V81 version of Peter Haas and DU-V81 version of
;   v8.2    Bob Clyne.  Some changes in the help guide.
;                                       - Irv Hoff
;
; 10/14/83  Added support for disks where the boot track (track 00)
;   v8.1    contains fewer sectors than other tracks (e.g. 8-inch disks
;           formatted in IBM System/34 format where track 00 is single-
;           density with 128 byte records, and other tracks are double-
;           density with 256 or 1024 byte records.  Defaults to SPT,
;           but may be changed to another value by the "B[nn]" command.
;           "B<cr>" or "B0" changes back to SPT.  Before this change,
;           DU would get Record Not Found (RNF) errors on some LSI-type
;           controllers (e.g. 179X, uPD765), or would read/write the wrong
;           sector on SASI-type controllers (DTC, SHUGART).  In the case
;           of SASI-type controllers, the sector read/written in error was
;           usually (S# MOD 26) on the next track (track 01).
;                                       - Peter H. Haas
;
; 10/12/83  Fixed minor bug which caused garbage to be sent to the
;   v8.1    printer if CBIOS console output routine did not return with
;           the character still in the 'C' register.
;                                       - Bob Clyne
;
; 10/07/83  Fixed minor bug in WFM's code which prevented DU from work-
;   v8.0    ing under CP/M 2.2.  Verified operation on Magnolia CP/M+
;           (banked).  Changed MHz patch location to allow 6MHz.  NOTE:
;           No changes made to CP/M+ operation.
;                                       - Sigi F. Kluger
;
; 08/28/83  Modified SFK's CP/M+ fixes so they would work:  Used BDOS
;   v7.9a   function 50 for 3.x BIOS disk calls.  Added deblocking code
;           for CP/M 3.x disk I/O.  Flagged all mods with initials (WFM)
;           as comment.  NOTE:  Using BDOS function 50 should work for
;           MP/M, if some MP/M user will check code to see if it is
;           compatible.                 - (WFM)
;
; 03/27/83  Modified for use with CP/M+ (version 3.0).  Re-enabled dis-
;   v7.9    play of erased file toggle (on/off) and suppressed CP/M+
;           date/time fields.           - Sigi F. Kluger
;
;
;               Many other changes between 1978 and 1983, by
;               Ron Fowler, Keith Petersen, Bruce Ratoff, Ward
;               Christensen, (HRF) and (DJH).
;
;
; 08/06/78  Originally written to reconstruct blown disks on CBBS via
;           remote access.              - Ward L. Christensen
;
;
;-----------------------------------------------------------------------
;
; Sorry for the lack of comments in the code portion of this program, it
; it was just hacked together to satisfy my needs, but lots of other
; people found it useful.  Its external documentation is good, but it's
; sadly lacking comments on the instructions.
;                                       - Ward L. Christensen
;
;-----------------------------------------------------------------------
;
; System equates
;
BASE    EQU     0               ;(set to 4200H for TRS-80)
;
FCB     EQU     BASE+5CH
BDOS    EQU     BASE+5
PRINT   EQU     9
GVERS   EQU     12
RESETDK EQU     13
SELDK   EQU     14
SRCHF   EQU     17              ;search first
SUSER   EQU     32
GETDSK  EQU     25
GETDPB  EQU     31
;
BHOME   EQU     8               ;BIOS function numbers
BSELDSK EQU     9
BSETTRK EQU     10
BSETSEC EQU     11
BSETDMA EQU     12
BREAD   EQU     13
BWRITE  EQU     14
BSECTRN EQU     16
;
TRNOFF  EQU     15              ;CP/M 1.4 offset from base
                                ;of BDOS to SECTRAN routine
SKWOFF  EQU     1AH             ;CP/M 1.4 offset to skew table
S2OFF   EQU     14              ;offset into FCB for S2 byte
DPBOFF  EQU     3AH             ;CP/M 1.4 offset to DPB within BDOS
S2MASK  EQU     0FH             ;mask for extended RC bits of S2
DPBLEN  EQU     17              ;size of CP/M 3.x disk parameter block
;
;
; Define ASCII characters
;
CR      EQU     0DH             ;carriage return
LF      EQU     0AH             ;line feed
TAB     EQU     09H             ;tab
BS      EQU     08H             ;backspace
;
;
ORG     BASE+100H
;
;
        JMP     PASTCK          ;jump over clock byte and i.d.
;
CLOCK   DB      1               ;0= 2mhz, 1=4mhz, 2=6mhz
        DB      'DU.COM v8.7 05/14/84',1AH  ;allow type du.com
;
PASTCK  LHLD    BDOS+1          ;get pointer to BDOS entry
        MVI     L,0             ;set HL=base of BDOS
        SPHL                    ;put stack there
        SHLD    SETSTK+1        ;save for later lxi sp instr.
        MVI     C,GVERS         ;get CP/M version nr
        CALL    BDOS
        SHLD    VERFLG          ;save version and MP/M flag
        MOV     A,L             ;...version numberr for a flag
        CPI     30H
        LXI     H,10            ;CP/M 2.x dpb offset
        JC      NOPLUS
        LXI     H,12            ;CP/M 3.x dpb offset
;
NOPLUS  SHLD    DPBOFS
        LXI     H,3000H         ;initialize "y" command memory pointer
        SHLD    YNKADR
;
;
; Set up local jumps to BIOS
;
        LHLD    BASE+1          ;warm boot pointer
        LXI     D,3             ;ready for add
        DAD     D
        SHLD    VCONST+1        ;const
        DAD     D
        SHLD    VCONIN+1        ;conin
        DAD     D
        SHLD    VCONOT+1        ;conout
        DAD     D
        SHLD    VLIST+1         ;list
        CPI     30H             ;CP/M 3.x?
        JNC     HELLO
        DAD     D               ;punch
        DAD     D               ;reader
        DAD     D
        SHLD    VHOME+1         ;home
        DAD     D
        SHLD    VSELDK+1        ;seldsk
        DAD     D
        SHLD    VSETRK+1        ;settrk
        DAD     D
        SHLD    VSTSEC+1        ;setsec
        DAD     D
        SHLD    VSTDMA+1        ;setdma
        DAD     D
        SHLD    VREAD+1         ;read
        DAD     D
        SHLD    VWRITE+1        ;write
        LDA     VERFLG
        ORA     A
        JZ      DOCPM1
        DAD     D               ;listst
        DAD     D
        SHLD    VSCTRN+1        ;sectran
        JMP     HELLO
;
DOCPM1  LHLD    BDOS+1
        MVI     L,0             ;BDOS on page boundary
        XCHG
        LXI     H,TRNOFF        ;CP/M 1.4 sectran routine offset
        DAD     D
        SHLD    VSCTRN+1
        LXI     H,SKWOFF        ;CP/M 1.4 skew table offset
        DAD     D
        SHLD    SECTBL          ;set up skew table pointer
;
HELLO   CALL    ILPRT
        DB      CR,LF,'DISK UTILITY v8.7',CR,LF
        DB      'Universal Version under ',0
        LDA     MPMFLG
        ORA     A
        JZ      HELCPM
        MVI     A,'M'
        JMP     HELMPM
;
HELCPM  MVI     A,'C'
;
HELMPM  CALL    TYPE
        CALL    ILPRT
        DB      'P/M ',0
        LDA     VERFLG
        ORA     A
        JNZ     NO14
        MVI     A,14H
;
NO14    PUSH    PSW
        RAR
        RAR
        RAR
        RAR
        ANI     0FH
        ORI     30H
        CPI     '9'+1
        JC      NOALPH
        ADI     'A'-'9'
;
NOALPH  CALL    TYPE
        MVI     A,'.'
        CALL    TYPE
        POP     PSW
        ANI     0FH
        ORI     30H
        CALL    TYPE
        CALL    ILPRT
        DB      CR,LF,LF
        DB      'Type ? for help',CR,LF
        DB      'Type X to exit'
        DB      CR,LF,0
        CALL    GETSTP          ;set up parameters
        LXI     H,BASE+80H      ;to input buff
        MOV     A,M
        ORA     A
        JZ      PRMPTR          ;no command
;
;
; Got initial command, set it up
;
        MOV     B,A             ;save length
        DCR     B
        JZ      PRMPTR
        LXI     D,INBUF
        INX     H               ;skip len
        INX     H               ;skip ' '
        CALL    MOVE
        MVI     A,CR
        STAX    D
        LXI     H,INBUF
        JMP     PRMPTI
;
PRMPTR  XRA     A
        STA     QFLAG
        CALL    RDBUF
;
PRMPTI  MVI     A,255
        STA     TOGO            ;loop count for "/"
        STA     TOGO+1
;
PROMPT  EQU     $
;
SETSTK  LXI     SP,$-$          ;modified at init
        XRA     A               ;zero 2-up print
        STA     TWOUP           ;..switch
        MVI     A,1
        STA     FTSW            ;tell search not to increment
        PUSH    H
        LXI     H,BASE+100H
        SHLD    BUFAD           ;for rdbyte
        POP     H
        CALL    CTLCS           ;abort?
        JZ      PRMPTR          ;..yes, read buffer
;
;
; Do we have to position in directory after find?
;
        LDA     FINDFL
        ORA     A
        JNZ     POSDIR          ;position in directory
        MOV     A,M
        CPI     CR
        JZ      PRMPTR
        CPI     ';'             ;logical cr?
        INX     H
        JZ      PROMPT
        CALL    UPCASE
        STA     DUMTYP          ;type of dump (a,d,h)
;
;
; Command dispatcher
;
        CPI     '+'
        JZ       PLUS
        CPI     '-'
        JZ      MINUS
        CPI     '='
        JZ      SEARCH
        CPI     '<'
        JZ      SAVE
        CPI     '>'
        JZ      RESTOR
        CPI     '#'
        JZ      STATS
        CPI     '?'
        JZ      HELP
        CPI     'A'
        JZ      DUMP
        CPI     'B'
        JZ      BOOT
        CPI     'C'
        JZ      CHG
        CPI     'D'
        JZ      DUMP
        CPI     'F'
        JZ      POSFIL
        CPI     'G'
        JZ      POS
        CPI     'H'
        JZ      DUMP
        CPI     'K'
        JZ      SVFILE          ; SAVE FILE FROM YANKED SECTORS
        CPI     'L'
        JZ      LOGIN
        CPI     'M'
        JZ      MAP
        CPI     'N'
        JZ      NEWDSK
        CPI     'P'
        JZ      PRNTFF
        CPI     'Q'
        JZ      QUIET
        CPI     'R'
        JZ      DOREAD
        CPI     'S'
        JZ      POS
        CPI     'T'
        JZ      POS
        CPI     'U'             ;CP/M 2.x, 3.x only
        JZ      USER
        CPI     'V'
        JZ      VIEW
        CPI     'W'
        JZ      DORITE
        CPI     'X'
        JZ      XIT
        CPI     'Y'
        JZ      YANK
        CPI     'Z'
        JZ      SLEEP
        CPI     '/'
        JZ      REPEAT
        CPI     '('
        JZ      TOGERA
;
WHAT    XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '?',0
        JMP     PRMPTR
;
;
; TOGERA - toggle display of erased files
;
TOGERA  LDA     TOGE
        CMA
        STA     TOGE
        JMP     PROMPT
;
;
; Exit - since CP/M+ doesn't do a warm boot at jump 0000, we have to
; reset the disk system!
;
XIT     MVI     C,RESETDK
        CALL    BDOS
        JMP     BASE
;
;
; Memory full error
;
MEMFUL  XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '+++ OUT OF MEMORY +++'
        DB      CR,LF,0
        JMP     PRMPTR
;
;
; Print disk statistics
;
STATS   PUSH    H
        CALL    ILPRT
        DB      'Statistics for drive ',0
        LDA     DRIVE
        ADI     'A'
        CALL    TYPE
        MVI     A,':'
        CALL    TYPE
        CALL    ILPRT
        DB      CR,LF,'Tracks:',9,9,0
        LHLD    MAXTRK
        INX     H
        PUSH    H
        CALL    DEC
        MVI     A,9
        CALL    TYPE
        POP     B
        CALL    HEXZ
        CALL    ILPRT
        DB      CR,LF,'Sys tracks:',9,0
        LHLD    SYSTRK
        PUSH    H
        CALL    DEC
        MVI     A,9
        CALL    TYPE
        POP     B
        CALL    HEXZ
        LHLD    SPT00
        XCHG
        LHLD    SPT
        XCHG
        CALL    SUBDE
        JC      STAT1
        CALL    ILPRT
        DB      CR,LF,'Sec/track:',9,0
        JMP     STAT2
;
STAT1   CALL    ILPRT
        DB      CR,LF,'Sec/trk 00:',9,0
        LHLD    SPT00
        PUSH    H
        CALL    DEC
        MVI     A,9
        CALL    TYPE
        POP     B
        CALL    HEXZ
        CALL    ILPRT
        DB      CR,LF,'Sec/non-trk 00:',9,0
;
STAT2   LHLD    SPT
        PUSH    H
        CALL    DEC
        MVI     A,9
        CALL    TYPE
        POP     B
        CALL    HEXZ
        CALL    ILPRT
        DB      CR,LF,'Groups:',9,9,0
        LHLD    DSM
        PUSH    H
        CALL    DEC
        MVI     A,9
        CALL    TYPE
        POP     B
        CALL    HEXB
        CALL    ILPRT
        DB      CR,LF,'Dir groups:',9,0
        LHLD    DIRGRP
        PUSH    H
        CALL    DEC
        MVI     A,9
        CALL    TYPE
        POP     B
        CALL    HEXZ
        CALL    ILPRT
        DB      CR,LF,'Sec/group:',9,0
        LDA     BLM
        INR     A
        MOV     L,A
        MVI     H,0
        PUSH    H
        CALL    DEC
        MVI     A,9
        CALL    TYPE
        POP     B
        CALL    HEXZ
        CALL    ILPRT
        DB      CRDir entries:',9,0
        LHLD    DRM
        INX     H
        PUSH    H
        CALL    DEC
        MVI     A,9
        CALL    TYPE
        POP     B
        CALL    HEXZ
        CALL    CRLF
        POP     H
        JMP     PROMPT
;
;
; The following command resets the disk system thru CP/M, and may be
; usable for changing the disk density or format.  This can only be done
; if your BIOS resets the auto-density select parameters at every track-
; zero access.
;
NEWDSK  PUSH    H
        MVI     C,RESETDK
        CALL    BDOS
        LDA     DRIVE
        MOV     C,A
        POP     H
        CALL    SELECT
        JMP     PROMPT
;
;
; Quiet mode
;
QUIET   STA     QFLAG           ;now quiet
        JMP     PROMPT
;
;
; Repeat buffer contents
;
REPEAT  CALL    DECIN        down
        DCX     D               ;make up for prev inx d
        XCHG
        SHLD    TOGO
        MOV     A,H             ;all done?
        ORA     L
        XCHG                    ;get back inbuf ptr
        JNZ     PROMPT          ;no, keep going
        JMP     PRMPTR          ;all done
;
;
; Set CP/M 2.x, 3.x user number
;
USER    LDA     VERFLG
        ORA     A
        JZ      WHAT
        CALL    DECIN           ;get requested user no.
        MOV     A,E
        CPI     32              ;valid?
        JNC     WHAT
        MOV     A,D
        ORA     A
        JNZ     WHAT
        MVI     C,SUSERdown
        DCX     D               ;make up for prev inx d
        XCHG
        SHLD    TOGO
        MOV     A,H             ;all done?
        ORA     L
        XCHG                    ;get back inbuf ptr
        JNZ     PROMPT          ;no, keep going
        JMP     PRMPTR          ;all done
;
;
; Set CP/M 2.x, 3.x user number
;
USER    LDA     VERFLG
        ORA     A
        JZ      WHAT
        CALL    DECIN           ;get requested user no.
        MOV     A,E
        CPI     32              ;valid?
        JNC     WHAT
        MOV     A,D
        ORA     A
        JNZ     WHAT
        MVI     C,SUSER
        PUSH    H               ;save char pointer
        CALL    BDOS            ;set user no.
        POP     H
        JMP     PROMPT
;
;
; Toggle print flag
;
PRNTFF  LDA     PFLAG
        XRI     1
        STA     PFLAG
        JMP     PROMPT
;
;
; Sleep routine, in tenths of a second
;
SLEEP   CALL    HEXIN           ;get count if any
        MOV     A,E             ;any?
        ORA     A
        JNZ     SLEPLP
        MVI     E,10
;
SLEPLP  LXI     B,8000          ;approximately .1 second at 2 MHz
        LDA     CLOCK
        ORA     A
        JZ      SLEEP2
        LXI     B,16000         ;approximately .1 second at 4 MHz
        CPI     1
        JZ      SLEEP2
        LXI     B,24000         ;approximately .1 second at 6 MHz
;
SLEEP2  DCX     B
        MOV     A,B
        ORA     C
        JNZ     SLEEP2
        PUSH    D
        CALL    CTLCS
        POP     D
        JZ      PRMPTR
        DCR     E
        JNZ     SLEPLP
        JMP     PROMPT
;
;
; Check for CTL-C or CTL-S
;
CTLCS   CALL    CONST
        ORA     A
        JNZ     GETC
        ORI     1               ;no char, retn nz
        RET
;
GETC    CALL    CONIN
        ANI     1FH             ;allow ASCII
        CPI     'S'-40H
        CZ      CONIN
        CPI     'C'-40H
        RET                     ;0 set if ctl-c
;
;
; Find our way at initialization
;
GETSTP  MVI     C,GETDSK
        CALL    BDOS            ;get curnt dsk
        MOV     C,A             ;..we have to select
        JMP     SELECT          ;..to get the dph
;
LOGIN   CALL    DOLOG
        JMP     PROMPT
;
DOLOG   MOV     A,M             ;disk req?
        LXI     D,0
        CPI     CR
        JZ      LGNODK
        CPI     ';'
        JZ      LGNODK
        CALL    UPCASE
        INX     H
        SUI     'A'
        MOV     C,A
;
SELECT  PUSH    H
        MOV     A,C
        STA     DRIVE           ;remember later where we are
        CALL    V3CHEK          ;check for version 3.x
        JC      VSELDK          ;jump if 1.x or 2.x
        MVI     E,0             ;set first call flag
        MVI     A,BSELDSK       ;select disk function
        CALL    BDOS50          ;call BIOS via BDOS
        JMP     VSELD3
;
VSELDK  CALL    $-$             ;addr filled in by 'INIT'
        LDA     VERFLG
        ORA     A               ;if not CP/M 2.x ...
        JZ      SELSKP          ;..then skip this junk
;
VSELD3  MOV     A,H
        ORA     L
        JZ      WHAT            ;select error
        MOV     E,M             ;get the sector table pointer
        INX     H
        MOV     D,M
        DCX     H               ;point to start of DPH
        XCHG
        SHLD    SECTBL
        LXI     H,10            ;offset to DPBPTR
;
DPBOFS  EQU     $-2             ;modified during init
        DAD     D
        MOV     A,M             ;pick up dpb pointer
        INX     H               ;..to use
        MOV     H,M             ;..as parameter
        MOV     L,A             ;..to logit
;
SELSKP  CALL    LOGIT
        LHLD    SYSTRK          ;reset track and sector
        XCHG                    ;..to directory
        CALL    SETTRK          ;..on every
        LXI     D,1             ;..login
        CALL    SETSEC          ;..change
        LHLD    PHYSEC          ;this logic will tell
        MOV     A,H             ;..if first sec
        ORA     L               ;..is physical 0
        STA     FIRST0
        CALL    CLCSUB
        POP     H
;
LGNODK  CALL    NORITE
        RET
;
;
; Read in the disk directory
;
REDDIR  PUSH    H
        CALL    NORITE          ;positioning lost
        LHLD    SYSTRK
        SHLD    CURTRK
        LXI     H,1
        SHLD    CURSEC
        LHLD    DRM             ;get dir size from dpb
        INX     H               ;make 1-relative
        CALL    ROTRHL
        CALL    ROTRHL          ;divide by 4 (4 names/sector)
        MOV     B,H
        MOV     C,L
        LXI     D,DIRECT        ;DMA address
;
RDIRLP  PUSH    B
        PUSH    D
        MOV     B,D
        MOV     C,E
        LDA     BDOS+2          ;check mem avail
        DCR     A
        CMP     D
        JC      MEMFUL
        CALL    SETDMA
        LHLD    CURTRK
        XCHG
        CALL    SETTRK
        LHLD    CURSEC
        XCHG
        CALL    SETSEC
        CALL    READ
        CALL    NXTSEC
        POP     D
        POP     B
        LXI     H,80H
        DAD     D
        XCHG
        DCX     B
        MOV     A,B
        ORA     C
        JNZ     RDIRLP
        LXI     B,BASE+80H
        CALL    SETDMA
        POP     H
        RET
;
;
; Map the directory
;
MAP     CALL    HEXIN
        PUSH    H               ;save inbuf ptr
        LHLD    CURTRK
        SHLD    SAVTRK          ;save track ...
        LHLD    CURSEC
        SHLD    SAVSEC          ;  ... and sector
        MOV     A,E             ;get start
        ORA     D               ;nothing?
        JZ      MAPRD           ;..yes, dflt
        LHLD    DIRGRP
        CALL    SUBDE
        CMC                     ;start >= default ?
        JC      MAPRD           ;..no, default
        LHLD    DSM             ;get max
        CALL    SUBDE           ;start exceed max ?
        JC      OUTLIM          ;..yes, abort
        XCHG
        JMP     MAPRD1
;
MAPRD   LHLD    DIRGRP
;
MAPRD1  CALL    REDDIR          ;read in directory
        MOV     B,H
        MOV     C,L
;
MAPDF   LDA     DLMREQ
        ORA     A               ;delimiter required ?
        CNZ     DELIM           ;  yes, doit
        CALL    HEXB
        MVI     A,'-'
        CALL    TYPE
        MVI     A,' '
        STA     DUPFLG
        CALL    GETGRP          ;get grp(c) to HL
;
MAPCNT  INX     B               ;next grp #
        PUSH    H
        LHLD    DSM             ;get highest grp #
        INX     H               ;plus 1 for comparison
        MOV     A,L             ;when BC reaches DSM+1..
        CMP     C               ;..then we have exceeded..
        JNZ     MAPC1           ;..the disk capacity..
        MOV     A,H
        CMP     B
;
MAPC1   POP     H
        JZ      MAPEND          ;..and we are done
        PUSH    H
        CALL    GETGRP          ;get another
        POP     D               ;see if same
        CALL    CTLCS
        JZ      MAPND2
        MOV     A,D
        CMP     H
        JNZ     MAPDIF
        MOV     A,E
        CMP     L
        JZ      MAPCNT          ;same, continue
;
;
; Different file encountered
;
MAPDIF  DCX     B
        CALL    HEXB
        INX     B
        XCHG
        CALL    MAPNAM
        JMP     MAPDF
;
;
; End of map
;
MAPEND  DCX     B               ;get last
        CALL    HEXB
        CALL    MAPNAM
;
;
; End of map - reposition to previous group
;
MAPND2  CALL    CRLF
        LHLD    SYSTRK
        XCHG
        LHLD    SAVTRK
        CALL    SUBDE           ;within a group ?
        JNC     MAPND3          ;  yes, POSGP2 can handle it
        LHLD    SAVSEC          ;  no, use track/sector positioning
        SHLD    CURTRK
        XCHG
        CALL    SETSEC
        LHLD    SAVTRK
        SHLD    CURTRK
        XCHG
        CALL    SETTRK
        CALL    READ
        XRA     A
        STA     NOTPOS
        POP     H
        JMP     INQ
MAPND3  LHLD    GROUP
        XCHG
        JMP     POSGP2
;
;
; Print file name pointed to by HL
;
MAPNAM  CALL    SPACE
        MOV     A,H
        ORA     L               ;none?
        JZ      NONAME
        MOV     A,M             ;see if alloc
        CPI     0E5H            ;free?
        MVI     A,' '
        JNZ     MPNSP1
        MVI     A,'('
;
MPNSP1  CALL    TYPE
        PUSH    H               ;save pointer
        MOV     A,M
        CALL    HEX             ;show user number
        CALL    SPACE
        INX     H               ;skip user byte
        PUSH    B
        MVI     B,8
        CALL    MAPN2
        MVI     A,'.'
        CALL    TYPE
        MVI     B,3
        CALL    MAPN2
        LDA     DUPFLG
        CALL    TYPE            ;space or star
        POP     B
        MOV     A,M             ;get extent
        CALL    HEX
        POP     H
        MOV     A,M
        CPI     0E5H
        MVI     A,' '
        JNZ     MPNSP2
        MVI     A,')'
;
MPNSP2  CALL    TYPE            ;")" if erased file
        JMP     FLIP
;
NONAME  CALL    ILPRT
        DB      '    ++FREE++        ',0
;
FLIP    LDA     TWOUP
        XRI     1
        STA     TWOUP
        MVI     A,0FFH
        STA     DLMREQ          ;delimiter now required
        RET
;
DELIM   LDA     TWOUP
        ORA     A
        JNZ     DELIM1
        CALL    CRLF
        JMP     DELIM2
;
DELIM1  MVI     A,':'
        CALL    TYPE
        CALL    SPACE
;
DELIM2  XRA     A
        STA     DLMREQ          ;delimiter was output
        RET
;
;
; Print name, length in B
;
MAPN2   MOV     A,M
        ANI     7FH             ;strip possible 2.x attribute bit
        INX     H
        CPI     ' '             ;printable?
        JC      MAPN2H          ;..no, in hex
        CPI     7EH             ;7E is leadin on some CRT's
        JC      MAPN2A
;
MAPN2H  CALL    BHEX
        JMP     MAPN2Z
;
MAPN2A  CALL    TYPE
;
MAPN2Z  DCR     B
        JNZ     MAPN2
        RET
;
;
; Find which file group (BC) belongs to
;
GETGRP  LHLD    DRM             ;max dir entry #
        INX     H               ;make 1-relative
        SHLD    FILECT
        LXI     H,0
        SHLD    MFPTR
        LXI     H,DIRECT
;
GETGLP  PUSH    H               ;save pointer to name
        PUSH    H
        LDA     TOGE
        ORA     A
        JNZ     SKERA
        INX     H
;
SKERA   MOV     A,M             ;pick up dn byte
        POP     H
        CPI     0E5H
        JZ      GETGNF
        MOV     A,M             ;this code for CP/M+ only...
        CPI     21H             ;suppress time/date field...
        JZ      GETGNF
        LXI     D,14            ;now get record count
        DAD     D               ;..S2 portion ..
        MOV     A,M             ;..is 0 in CP/M 1.4
        ANI     0FH
        MOV     E,A
        INX     H
        MOV     A,M
        ORA     E
        JZ      GETGNF
        MVI     E,16            ;first set for 8-bit grps
        LDA     DSM+1
        ORA     A
        JZ      SMALGP
        MVI     E,8             ;nope, big groups
;
SMALGP  MOV     D,A             ;save group size indicator
;
GETGL2  INX     H               ;pointing into dm field
        CALL    GRPCMP          ;compare BC group # against 1 dm field
        JNZ     NOTGOT          ;jump if found one
;
;
; Found the file
;
        PUSH    H               ;save group pointer
        LHLD    MFPTR
        MOV     A,H
        ORA     L
        POP     H
        XTHL                    ;get entry start and save pointer
        JZ      MPFRST
        MVI     A,'*'
        STA     DUPFLG
;
MPFRST  SHLD    MFPTR
        XTHL                    ;as they were
;
NOTGOT  DCR     E               ;else count down
        JNZ     GETGL2          ;go test some more
;
GETGNF  POP     H               ;not this one!
        LXI     D,32            ;so go to next
        DAD     D
        XCHG
        LHLD    FILECT          ;there is limit to everything
        DCX     H
        SHLD    FILECT
        MOV     A,H
        ORA     L
        XCHG                    ;re-align
        JNZ     GETGLP
;
;
; Get the allocation address, if any
;
        LHLD    MFPTR
        RET
;
;
; Yank the current sector into memory at location YNKADR
;
YANK    LDA     7               ;get top byte of CCP pointer
        MOV     B,A
        LDA     YNKADR+1        ;get top byte of memory pointer
        CMP     B
        JNC     YMFULL          ;if memory full, say so and abort yank
        LDA     WRFLG           ;check if a read has been done
        ORA     A
        JZ      BADW            ;if no read, then cannot yank, so abort
        PUSH    H
        LHLD    YNKADR          ;move sector into yank memory
        XCHG
        LXI     H,BASE+80H
        MVI     B,128
        CALL    MOVE
        CALL    ILPRT           ;tell where last byte is
        DB      'LAST ADDR=',0
        LHLD    YNKADR          ;calculate last byte
        LXI     B,80H
        DAD     B
        SHLD    YNKADR          ;save last byte+1 for next yank
        DCX     H
        MOV     A,H
        CALL    HEX
        MOV     A,L
        CALL    HEX
        CALL    ILPRT
        DB      CR,LF,0
        POP     H
        JMP     PROMPT
;
YMFULL  XRA     A               ;memory full, so say so
        STA     QFLAG           ;set to not quiet mode first
        CALL    ILPRT
        DB      '++ YANK MEMORY FULL ++'
        DB      CR,LF,0
        JMP     PROMPT
;
;
; Save the current sector
;
SAVE    LDA     WRFLG
        ORA     A
        JZ      BADW            ;none to save
        PUSH    H
        LXI     H,BASE+80H
        LXI     D,SAVBUF
        MVI     B,128
        CALL    MOVE
        MVI     A,1             ;..show
        STA     SAVEFL          ;..saved exists
        POP     H
        JMP     PROMPT
;
;
; Restore the current sector
;
RESTOR  LDA     SAVEFL
        ORA     A
        JZ      NOSAVE          ;none to save
        PUSH    H
        LXI     H,SAVBUF
        LXI     D,BASE+80H
        MVI     B,128
        CALL    MOVE
        POP     H
        JMP     PROMPT
;
NOSAVE  XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '++ NO "<" SAVE COMMAND ISSUED ++'
        DB      CR,LF,0
        JMP     PRMPTR
;
;
; Move (HL) to (DE) length in B
;
MOVE    MOV     A,M
        STAX    D
        INX     H
        INX     D
        DCR     B
        JNZ     MOVE
        RET
;
NORITE  XRA     A               ;get 0
        STA     WRFLG           ;cannot write now
        RET
;
;
; No match in search, try next character
;
SRNOMT  POP     H
        CALL    CTLCS           ;abort?
        JNZ     SEARCH          ;..yes
        LXI     H,INBUF
        MVI     M,CR
        JMP     CLCGRP          ;show where stopped
;
;
; Search for character string
;
SEARCH  PUSH    H               ;save string pointer
;
SRCHL   CALL    RDBYTE          ;get a byte
        MOV     B,A             ;save it
        MOV     A,M             ;check next match character.
        CPI     '<'             ;will it be hex?
        MOV     A,B             ;restore disk character
        JZ      SRCHL1
        ANI     7FH             ;next character is ASCII...strip bit 7
;
SRCHL1  PUSH    PSW
        CALL    GETVAL          ;get search value
        MOV     B,A
        POP     PSW
        CMP     B               ;match?
        JNZ     SRNOMT          ;no match
        INX     H
        MOV     A,M             ;done?
        CPI     CR
        JZ      SREQU
        CPI     ';'
        JNZ     SRCHL
;
;
; Got match
;
SREQU:  XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '= AT ',0
        LDA     BUFAD
        ANI     7FH
        CALL    HEX
        CALL    CRLF
        JMP     CLCGRP
;
;
; Get value from input buffer
;
GETVAL  MOV     A,M
        CPI     '<'             ;hex escape?
        RNZ                     ;no, return
;
;
; "<<" means one "<"
;
        INX     H
        MOV     A,M
        CPI     '<'
        RZ
;
;
; Got hex
;
        PUSH    D
        CALL    HEXIN           ;get value
        CPI     '>'             ;proper delimiter?
        MOV     A,E             ;get value
        POP     D
        JNZ     WHAT            ;error
        RET
;
;
; Read a byte at a time
;
RDBYTE  PUSH    H
        LDA     FTSW            ;first read?
        ORA     A
        JNZ     READ1
        LHLD    BUFAD
        MOV     A,L
        ORA     A               ;in buffer?
        JM      NORD            ;yes, skip read
;
;
; Have to read
;
        CALL    NXTSEC
;
READ1   XRA     A
        STA     FTSW            ;not first read
        LHLD    CURSEC
        XCHG
        CALL    SETSEC
        LHLD    CURTRK
        XCHG
        CALL    SETTRK
        CALL    READ
        CALL    CLCSUB
        LXI     H,BASE+80H
;
NORD    MOV     A,M
        INX     H
        SHLD    BUFAD
        POP     H
        RET
;
;
; View the file in ASCII starting at current sector, stepping through
; the disk
;
VIEW    LDA     WRFLG
        ORA     A
        JZ      BADDMP
        CALL    HEXIN           ;get displ if any
        PUSH    H
        MOV     A,E
        ORA     A
        JNZ     VIEWLP
        INR     E               ;dflt=1
;
VIEWLP  LXI     H,BASE+80H      ;to data
;
VEWCHR  CALL    CTLCS
        JZ      VEWEND
        MOV     A,M
        CPI     1AH
        JZ      VEWEOF
        ANI     7FH
        CPI     7EH
        JNC     VIEWHX          ;show rubout and tilde as hex
        CPI     ' '
        JNC     VIEWPR
        CPI     CR
        JZ      VIEWPR
        CPI     LF
        JZ      VIEWPR
        CPI     TAB
        JZ      VIEWPR
;
VIEWHX  MOV     A,M             ;not ascii...print as <nn>
        CALL    BHEX
        JMP     VIEWNP
;
VIEWPR  CALL    TYPE
;
VIEWNP  INR     L
        JNZ     VEWCHR
        DCR     E
        JZ      VEWEND
        PUSH    D               ;save count
        CALL    NXTSEC
        LHLD    CURSEC
        XCHG
        CALL    SETSEC
        LHLD    CURTRK
        XCHG
        CALL    SETTRK
        CALL    READ
        POP     D               ;restore count
        JMP     VIEWLP
;
VEWEOF  CALL    ILPRT
        DB      CR,LF,TAB,'++ EOF ++',CR,LF,0
;
VEWEND  POP     H
        CALL    CRLF
        JMP     CLCGRP
;
;
; Dump in hex or ascii
;
DUMP    LDA     WRFLG
        ORA     A
        JNZ     DUMPOK
;
BADDMP  XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '++ Can''t dump, no sector read ++',CR,LF,0
;
EXPL    XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      'Use G command following F,',CR,LF
        DB      'or R or S following T',CR,LF,0
        JMP     PRMPTR
;
DUMPOK  MOV     A,M
        CPI     ';'
        JZ      DUMPDF          ;default
        CPI     CR
        JNZ     DMPNDF
;
;
; Use default
;
DUMPDF  LXI     B,BASE+80H
        LXI     D,0FFH
        JMP     DUMP1
;
DMPNDF  CALL    DISP
        MOV     B,D
        MOV     C,E
        CPI     CR
        JZ      DUMP1
        CPI     ';'
        JZ      DUMP1
        INX     H               ;skip ','
        CALL    DISP
;
;
; BC = start, DE = end
;
DUMP1   PUSH    H               ;save command pointer
        MOV     H,B
        MOV     L,C
;
DUMPLP  MOV     A,L
        ANI     7FH
        CALL    HEX
        CALL    SPACE
        CALL    SPACE
        LDA     DUMTYP
        CPI     'A'
        JZ      DUMPAS
        PUSH    H               ;save start
;
DHEX    MOV     A,M
        CALL    HEX
        MOV     A,L
        ANI     3
        CPI     3
        CZ      SPACE
        MOV     A,L
        ANI     7
        CPI     7
        CZ      SPACE
        MOV     A,E
        CMP     L
        JZ      DPOP
        INX     H
        MOV     A,L
        ANI     0FH
        JNZ     DHEX
;
DPOP    CALL    CTLCS
        JZ      PRMPTR
        LDA     DUMTYP
        CPI     'H'
        JZ      DNOAS           ;hex only
        POP     H               ;get start address
;
DUMPAS  CALL    ASTER
;
DCHR    MOV     A,M
        ANI     7FH
        CPI     ' '
        JC      DPER
        CPI     7EH
        JC      DOK
;
DPER    MVI     A,'.'
;
DOK     CALL    TYPE
        MOV     A,E
        CMP     L
        JZ      DEND
        INX     H
        MOV     A,L
        ANI     0FH
        JNZ     DCHR
;
DEND    CALL    ASTER
        CALL    CRLF
        PUSH    D
        CALL    CTLCS
        POP     D
        JZ      PRMPTR
        MOV     A,E
        CMP     L
        JNZ     DUMPLP
        POP     H
        JMP     PROMPT
;
DNOAS   POP     B
        CALL    CRLF
        MOV     A,E
        CMP     L
        JNZ     DUMPLP
        POP     H
        JMP     PROMPT
;
;
; Position
;
POS     PUSH    PSW
        MOV     A,M
        CPI     ';'
        JZ      POSINQ
        CPI     CR
        JNZ     POSOK
;
POSINQ  POP     PSW
        JMP     INQ
;
POSOK   POP     PSW
        CPI     'T'
        JZ      POSTKD
        CPI     'S'
        JZ      POSSCD
        CPI     'G'
        JZ      POSGPH
        JMP     WHAT
;
POSTKD  CALL    DECIN
;
POSTRK  PUSH    H
        LHLD    MAXTRK
        CALL    SUBDE
        POP     H
        JC      OUTLIM
        CALL    SETTRK
        CALL    NORITE          ;track does not read
        MVI     A,1
        STA     NOTPOS          ;show not positioned
        JMP     CLCGRP
;
POSSCD  CALL    DECIN
        MOV     A,D
        ORA     E
        JZ      WHAT            ;do not allow sector 0
;
POSSEC  PUSH    H
        LHLD    SPT
        CALL    CHK00
        CALL    SUBDE
        POP     H
        JC      WHAT
        CALL    SETSEC
        CALL    READ
        XRA     A
        STA     NOTPOS          ;positioned ok
;
CLCGRP  CALL    CLCSUB
        JMP     INQ
;
;
; Calculate group from track and sector
;
CLCSUB  PUSH    H
        LHLD    SYSTRK
        XCHG
        LHLD    CURTRK
        CALL    SUBDE
        XCHG
        LHLD    SPT
        CALL    MULT
        XCHG
        LHLD    CURSEC
        DCX     H
        DAD     D
        LDA     BLM
        MOV     B,A
        MOV     A,L
        ANA     B
        STA     GRPDIS
        LDA     BSH
        MOV     B,A
;
CLCLOP  CALL    ROTRHL
        DCR     B
        JNZ     CLCLOP
        SHLD    GROUP
        POP     H
        RET
;
;
; Position in the directory after a find (does not work in CP/M-2.x)
;
POSDIR  PUSH    H               ;save INBUF
        LHLD    BSH
        XRA     A
        STA     FINDFL          ;cancel position request
        LDA     DIRPOS          ;get position
        RAR
        RAR
        PUSH    PSW
        ANA     H
        STA     GRPDIS
        POP     PSW
;
POSDLP  RAR
        DCR     L
        JNZ     POSDLP
        ANI     1               ;get group
        MOV     L,A             ;setup for POSGP2
        MVI     H,0
        SHLD    GROUP
        XCHG
        JMP     POSGP2          ;position to it
;
POSGPH  CALL    HEXIN
;
POSGRP  PUSH    H
        LHLD    DSM
        CALL    SUBDE
        POP     H
        JC      OUTLIM
        XCHG
        SHLD    GROUP
        XCHG
        XRA     A
        STA     GRPDIS
        PUSH    H
;
POSGP2  CALL    GTKSEC
        CALL    SETTRK
        XCHG
        CALL    SETSEC
        CALL    READ
        XRA     A
        STA     NOTPOS          ;now positioned
        POP     H
        JMP     INQ
;
GTKSEC  MOV     H,D
        MOV     L,E
        LDA     BSH
;
GLOOP   DAD     H
        DCR     A
        JNZ     GLOOP
        LDA     GRPDIS
        ADD     L               ;cannot carry
        MOV     L,A
;
;
; Divide by nr of sectors, quotient=track, remainder=sector
;
        XCHG
        LHLD    SPT
        CALL    NEG
        XCHG
        LXI     B,0
;
DIVLP   INX     B
        DAD     D
        JC      DIVLP
        DCX     B
        XCHG
        LHLD    SPT
        DAD     D
        PUSH    H
        LHLD    SYSTRK
        DAD     B
        XCHG
        POP     H
        INX     H
        RET
;
POSFIL  CALL    NORITE
        MVI     A,1
        STA     FINDFL          ;so we position later
        LXI     D,FCB
        XRA     A
        STAX    D
        INX     D
        MVI     B,8
        CALL    MVNAME
        MVI     B,3
        CALL    MVNAME
        LXI     D,FCB
        MVI     C,SRCHF
        PUSH    H
        CALL    BDOS
        INR     A
        JNZ     FLOK
        STA     DIRPOS          ;group 0 if not found
        CALL    ILPRT
        DB      '++ FILE NOT FOUND ++',CR,LF,0
        POP     H
        JMP     PROMPT
;
FLOK    DCR     A
        STA     DIRPOS          ;save position in directory
        ANI     3
        MOV     L,A
        MVI     H,0
        DAD     H               ;x32 bytes/entry
        DAD     H
        DAD     H
        DAD     H
        DAD     H
        LXI     D,BASE+80H
        DAD     D               ;HL points to entry
        LXI     D,32
        XCHG
        DAD     D
        XCHG
        MVI     A,'D'
        STA     DUMTYP
        JMP     DUMPLP          ;which pops H
;
MVNAME  MOV     A,M             ;get the character
        CPI     '.'             ;see if extension is next
        JZ      MVIPAD          ;if so, pad end of file name
        CPI     ':'             ;else, see if drive designation end
        JZ      MVIPAD          ;yes, advance pointer, &c.
        CPI     CR              ;carriage return?
        JZ      PAD             ;end of line means end of file name
        CPI     ';'             ;else, see if delimiter
        JZ      PAD             ;if so, same as CR
        CALL    UPCASE          ;otherwise convert to upper case
        STAX    D               ;save the character in FCB
        INX     H               ;point to next character
        INX     D               ;point to next FCB character
        DCR     B               ;count down max # of characters
        JNZ     MVNAME          ;loop if more to go
        MOV     A,M             ;advance pointer to next delimiter
        CPI     CR              ;CR is end of line delimiter
        RZ
        CPI     ';'             ;semi-colon is parameter delimiter
        RZ
        INX     H
        CPI     '.'             ;period is file name delimiter
        RZ
        CPI     ':'             ;colon is drive/user delimiter
        RZ
        JMP     WHAT
;
MVIPAD INX      H
;
PAD     MVI     A,' '
;
PAD1    STAX    D
        INX     D
        DCR     B
        JNZ     PAD1
        RET
;
;-----------------------------------------------------------------------
;                   Save sequential memory on disk
;
; As CP/M v 3.0 does not have a SAVE function, one has been added here.
;
; Syntax is:
;               Kdu:filename.ext
;               ^^^      ^     ^
;               |||      |     +-- file extension (0-3 characters)
;               |||      +-------- file name (1-8 characters)
;               ||+--------------- user # (or none)
;               |+---------------- drive designation (A-P or none)
;               +----------------- DU command
;
; Drive and user may be omitted.  If so, omit the colon as well.  Drive
; must be specified if the user is.  If the user # is omitted, the cur-
; rent user is used.  If the drive is omitted, the current CP/M default
; drive is used.
;
; This function saves the current contents of sequential memory into a
; disk file.  The contents of sequential memory are determined by the
; 'yank' function, and the pointer of that function is used here.  If
; nothing has been yanked, you get an error.  Once the file has been
; saved, the 'yank' pointer is re-initialized to its original value
; (3000H).  Control is  returned to DU.
;
;
; Turn free-form file name string into file control block
;
SVFILE  XRA     A
        STA     UZER            ;default to current user
        LXI     D,FCB           ;point to file control block
        STAX    D               ;default to current drive
        INX     D
        MVI     B,8             ;name is 8 chrs. (drive is 0-3 chrs.)
        CALL    MVNAME          ;get it, with padding
        DCX     H               ;take a look at the delimiter
        MOV     A,M
        INX     H
        CPI     ':'             ;was it drive/user?
        JNZ     NODRV           ;no, assume it was the name
        PUSH    H               ;save file name address
        LXI     H,FCB+1         ;ok then, start with the drive #
        MOV     A,M
        CALL    UPCASE          ;convert to upper case
        SUI     40H             ;make 'A'-'P' into 1-16
        STA     FCB             ;save as drive #
        INX     H               ;point to user number
        MOV     A,M             ;see if user #
        CPI     ' '
        JZ      UZE3            ;no, skip it
        MVI     B,2             ;no more than 2 digits
        MVI     C,0
;
UZE1    MOV     A,M             ;get a digit
        CPI     ' '             ;see if colon
        JZ      UZE2            ;if so, finished
        MOV     A,C             ;get current value
        ADD     A               ;*2
        ADD     A               ;*4
        ADD     C               ;*5
        ADD     A               ;*10
        MOV     C,A             ;save it
        MOV     A,M             ;get the digit
        SUI     30H             ;make into number
        ADD     C               ;add to current value
        MOV     C,A
        INX     H
        DCR     B
        JNZ     UZE1            ;do another
;
UZE2    MOV     A,C             ;get user number
        INR     A               ;add displacement (0 means no change)
        STA     UZER
;
UZE3    MVI     B,8             ;That was the drive/user.  Now get
                                ; the file name for real.
        POP     H               ;restore file name pointer
        LXI     D,FCB+1         ;point to file name again
        CALL    MVNAME
;
NODRV   MVI     B,3             ;get extension, if any
        CALL    MVNAME
        SHLD    ENDVAL          ;save location of end of command
        MVI     B,24            ;pad the rest of the dcb with 0
        XRA     A
        CALL    PAD1
;
;
; See if anything is there
;
        LHLD    YNKADR          ;get address of yank pointer
        MOV     A,H             ;compare with start of sequential memory
        CPI     30H             ;starts at 3000
        JNZ     SV1
        MOV     A,L
        ANA     A
        JNZ     SV1
        CALL    ILPRT           ;send error message
        DB      'Empty!',CR,LF,0
        JMP     ESVFIL
;
;
; Set the user number
;
SV1     MVI     E,0FFH          ;see what current user is
        MVI     C,32
        CALL    BDOS
        STA     CUZER           ;save it
        LDA     UZER            ;default user?
        ANA     A
        JZ      UZERD           ;yes, no change
        DCR     A               ;correct offset that was added
        MOV     E,A
        MVI     C,32
        CALL    BDOS            ;set new user
;
;
; Create the file
;
UZERD   MVI     C,15            ;open file
        LXI     D,FCB
        CALL    BDOS
        INR     A               ;see if 0ffh (open error)
        JZ      SV2             ;if error, file does not exist
        CALL    ILPRT
        DB      'File exists! Delete? ',0
        CALL    CONIN           ;see what the answer is
        CALL    UPCASE
        CPI     'Y'             ;see if yes
        JZ      DELF            ;yes, delete the file
        CALL    ILPRT
        DB      'N',CR,LF,0
        JMP     ENDSV           ;restore user # and return
;
DELF    CALL    ILPRT
        DB      'Y',CR,LF,0
        LXI     D,FCB
        MVI     C,19            ;delete file
        CALL    BDOS
;
SV2     MVI     C,22            ;make file
        LXI     D,FCB
        CALL    BDOS
        INR     A               ;see if 0ffh (open error)
        JNZ     SV2A
        CALL    ILPRT
        DB      'No dir space!',CR,LF,0
        JMP     ENDSV
;
SV2A    LXI     H,3000H         ;starting DMA address
        SHLD    YDN
        XRA     A
        STA     SCOUNT
;
SV3     LHLD    YDN             ;get starting location
        XCHG
        MVI     C,26            ;set dma address
        CALL    BDOS
        LXI     D,FCB           ;get file
        MVI     C,21            ;write sequential
        CALL    BDOS
        ANA     A
        JZ      SV4
        CALL    ILPRT
        DB      'Write error',CR,LF,0
        JMP     ENDSVC
;
SV4     LXI     H,SCOUNT        ;advance sector count
        INR     M
        LHLD    YDN
        LXI     D,80H
        DAD     D               ;point to next sector address
        SHLD    YDN
        XCHG                    ;save in DE
        LHLD    YNKADR          ;get next yank address
        MOV     A,D
        CMP     H               ;see if we got there yet
        JC      SV3             ;not yet
        MOV     A,E
        CMP     L
        JC      SV3             ;no, do another
        LHLD    SCOUNT
        MVI     H,0
        CALL    DEC             ;print # of sectors
        CALL    ILPRT
        DB      ' sectors written.',CR,LF,0
        LXI     H,3000H
        SHLD    YNKADR          ;reset yank address
;
ENDSVC  LXI     D,FCB
        MVI     C,16            ;close file
        CALL    BDOS
;
ENDSV   LDA     CUZER           ;get original user #
        MOV     E,A
        MVI     C,32            ;set it
        CALL    BDOS
;
ESVFIL  LHLD    ENDVAL          ;restore end-of-line location
        JMP     PROMPT          ;return
;
PLUS    LXI     D,1             ;default to 1 sector
        MOV     A,M             ;get next character
        CPI     CR              ;CR?
        JZ      PLUSGO          ;..yes, default to 1
        CPI     ';'
        JZ      PLUSGO
        CALL    HEXIN           ;get #
        MOV     A,D
        ORA     E
        JZ      WHAT
;
PLUSGO  CALL    NXTSEC
        DCX     D               ;more to go?
        MOV     A,D
        ORA     E
        JNZ     PLUSGO          ;..yes
;
;
; Ok, incremented to sector, setup and read
;
PLUSMI  PUSH    H
        LHLD    CURSEC
        XCHG
        CALL    SETSEC
        LHLD    CURTRK
        XCHG
        CALL    SETTRK
        POP     H
        CALL    READ
        JMP     CLCGRP
;
MINUS   LXI     D,1             ;set default
        MOV     A,M             ;get character
        CPI     CR              ;CR?
        JZ      MINGO           ;..yes, default=1
        CPI     ';'
        JZ      MINGO
        CALL    HEXIN           ;..no, get ##
        MOV     A,D
        ORA     E
        JZ      WHAT
;
MINGO   PUSH    H
        LHLD    CURSEC
        DCX     H
        MOV     A,H
        ORA     L
        JNZ     MINOK
        LHLD    CURTRK
        MOV     A,H
        ORA     L
        JNZ     SEASH
        LHLD    MAXTRK          ;wrap to end of disk
        SHLD    CURTRK
        LHLD    MAXSEC
        JMP     MINOK
;
SEASH   DCX     H
        SHLD    CURTRK
        LHLD    SPT
        CALL    CHK00           ;check for track 00
;
MINOK   SHLD    CURSEC
        POP     H
        DCX     D
        MOV     A,D
        ORA     E
        JNZ     MINGO
        JMP     PLUSMI
;
;
; Go to next sector
;
NXTSEC  PUSH    H
        PUSH    D
        LHLD    CURSEC
        INX     H
        XCHG
        LHLD    SPT
        CALL    CHK00           ;check for track 00
        CALL    SUBDE
        XCHG
        JNC     CHKLST
        LHLD    CURTRK
        INX     H
        XCHG
        LHLD    MAXTRK
        CALL    SUBDE
        JNC     TRASK
        LXI     D,0             ;wrap to start of disk
;
TRASK   XCHG
        SHLD    CURTRK
        LXI     H,1
;
NEXTOK  SHLD    CURSEC
        POP     D
        POP     H
        RET
;
CHKLST  PUSH    H               ;save new current sector
        LHLD    MAXTRK          ;are we on last track?
        XCHG
        LHLD    CURTRK
        CALL    SUBDE
        POP     H               ;get new current sector back
        JC      NEXTOK          ;if not on last track
        XCHG                    ;else, see if we are past
        LHLD    MAXSEC          ;..last CP/M usable sector
        CALL    SUBDE
        XCHG
        JNC     NEXTOK          ;if not past last usable sector
        LXI     D,0             ;else, wrap to start of disk
        JMP     TRASK
;
;
; Boot track processing
;
BOOT    CALL    DECIN
        MOV     A,D
        ORA     E               ;operand given ?
        JNZ     BOOT1           ;  yes, check it out
        XCHG
        LHLD    SPT             ;  no, give non-track 00 spt
        XCHG
        JMP     BOOT2
;
BOOT1   PUSH    H
        LHLD    SPT
        CALL    SUBDE           ;operand <= non-track 00 spt ?
        POP     H
        JC      WHAT            ;  no, give bad news
;
BOOT2   XCHG
        SHLD    SPT00           ;save track 00 spt
        XCHG
        JMP     PROMPT
;
;
; Check for Boot track
;
CHK00   PUSH    H               ;save spt on stack
        LHLD    CURTRK
        MOV     A,H
        ORA     L               ;track 00 ?
        JNZ     CHK001          ;  no, leave spt as-is
        LHLD    SPT00           ;  yes, get spt for track 00
        XTHL                    ;replace on stack
;
CHK001  POP     H               ;get spt (trk 00 or non-trk 00) back
        RET
;
;
; Tell what group, displacement, track, sector, physical sector
;
INQ     CALL    INQSUB
        JMP     PROMPT
;
;
; Position inquiry subroutine executed via: G S or T (with no operands)
;
INQSUB  PUSH    H
        LHLD    SYSTRK
        XCHG
        LHLD    CURTRK
        CALL    SUBDE
        JC      NOGRP
        CALL    ILPRT
        DB      'G=',0
        LHLD    GROUP
        MOV     B,H
        MOV     C,L
        CALL    HEXB
        MVI     A,':'
        CALL    TYPE
        LDA     GRPDIS
        CALL    HEX
        CALL    ILPRT
        DB      ', ',0
;
NOGRP   CALL    ILPRT
        DB      'T=',0
        LHLD    CURTRK
        CALL    DEC
        CALL    ILPRT
        DB      ', S=',0
        LHLD    CURSEC
        CALL    DEC
        CALL    ILPRT
        DB      ', PS=',0
        LHLD    PHYSEC
        CALL    DEC
        CALL    CRLF
        POP     H
        RET
;
CHG     MOV     A,M             ;get type (HEX, ASCII)
        CALL    UPCASE
        PUSH    PSW             ;save "H" or "A"
        INX     H
        CALL    DISP            ;get, validate display to DE
        INX     H
        LXI     B,0             ;show no 'THRU' address
        CPI     '-'             ;test delimiter from display
        JNZ     CHGNTH          ;no through
        PUSH    D               ;save from
        CALL    DISP            ;get through
        INX     H               ;skip end delimitera
        MOV     B,D
        MOV     C,E             ;BC = through
        POP     D               ;get from
        JMP     CHGAH
;
CHGNTH  CPI     ','
        JNZ     WHAT
;
CHGAH   POP     PSW
        CPI     'H'
        JZ      CHGHEX
        CPI     'A'
        JNZ     WHAT
;
;
; Change ASCII
;
CHGALP  MOV     A,M
        CPI     CR
        JZ      PROMPT
        CPI     ';'
        JZ      PROMPT
        LDAX    D
        CPI     ' '
        JC      CHGAHX
        CPI     7EH
        JNC     CHGAHX
        JMP     CHGA2
;
CHGAHX  CALL    BHEX
        JMP     CHGA3
;
CHGA2   CALL    TYPE
;
CHGA3   SHLD    BACK            ;in case "thru"
        CALL    GETVAL          ;ASCII or <HEX>
        STAX    D               ;update character
        INX     H               ;to next input character
;
;
; See if 'THRU' requested
;
        MOV     A,C
        ORA     A
        JZ      CHANTH
        CMP     E               ;done?..
        JZ      PROMPT          ;..yes
        LHLD    BACK
;
CHANTH  INR     E
        JNZ     CHGALP
        MOV     A,M
        CPI     CR
        JZ      PROMPT
        CPI     ';'
        JZ      PROMPT
        JMP     WHAT
;
;
; Change HEX
;
CHGHCM  INX     H
;
CHGHEX  MOV     A,M
        CPI     CR
        JZ      PROMPT
        CPI     ';'
        JZ      PROMPT
        CPI     ','             ;delimiter?
        JZ      CHGHCM
        PUSH    D
        SHLD    HEXAD           ;in case 'THRU'
        CALL    HEXIN           ;positions to delim
        MOV     A,E             ;get value
        POP     D               ;..address
        PUSH    PSW             ;save value
        LDAX    D               ;get old
        CALL    HEX             ;echo in HEX
        POP     PSW             ;get new
        STAX    D               ;save new
        MOV     A,C             ;see if 'THRU'
        ORA     A
        JZ      CHHNTH          ;..no.
        CMP     E               ;..yes, done?
        JZ      PROMPT
        LHLD    HEXAD           ;..no: more
;
CHHNTH  INR     E
        JNZ     CHGHEX
        MOV     A,M
        CPI     CR
        JZ      PROMPT
        CPI     ';'
        JZ      PROMPT
        JMP     WHAT
;
DOREAD  LDA     NOTPOS
        ORA     A
        JNZ     CANTRD
        CALL    READ
        JMP     PROMPT
;
CANTRD  XRA     A
        STA     QFLAG           ;not quiet
        CALL    ILPRT
        DB      '++ Can''t read - not positioned ++',CR,LF
        DB      'Position by:',CR,LF
        DB      9,'Track then Sector, or',CR,LF
        DB      9,'Group',CR,LF,0
        JMP     PROMPT
;
DORITE  CALL    WRITE
        JMP     PROMPT
;
BHEX    PUSH    PSW
        MVI     A,'<'
        CALL    TYPE
        POP     PSW
        CALL    HEX
        MVI     A,'>'
        CALL    TYPE
        RET
;
HEXB    LDA     DSM+1
        ORA     A
        JZ      HEXX
        MOV     A,B
        CALL    HEX
;
HEXX    MOV     A,C
        JMP     HEX
;
HEXZ    MOV     A,B
        ORA     A
        CNZ     HEX
        MOV     A,C
;
HEX     PUSH    PSW
        RAR
        RAR
        RAR
        RAR
        CALL    NIBBL
        POP     PSW
;
NIBBL   ANI     0FH
        CPI     10
        JC      HEXNU
        ADI     7
;
HEXNU   ADI     '0'
        JMP     TYPE
;
;
; Decimal output routine
;
DEC     PUSH    B
        PUSH    D
        PUSH    H
        LXI     B,-10
        LXI     D,-1
;
DECOU2  DAD     B
        INX     D
        JC      DECOU2
        LXI     B,10
        DAD     B
        XCHG
        MOV     A,H
        ORA     L
        CNZ     DEC
        MOV     A,E
        ADI     '0'
        CALL    TYPE
        POP     H
        POP     D
        POP     B
        RET
;
SPACE   MVI     A,' '
        JMP     TYPE
;
ASTER   MVI     A,'*'
        JMP     TYPE
;
;
; Inline print routine
;
ILPRT   XTHL
;
ILPLP   CALL    CTLCS           ;abort?
        JZ      PRMPTR
        MOV     A,M
        CPI     1               ;pause?
        JNZ     ILPOK
        CALL    CONIN
        CPI     3               ;abort?
        JZ      PRMPTR
        JMP     ILPNX
;
ILPOK   CALL    TYPE
;
ILPNX   INX     H
        MOV     A,M
        ORA     A
        JNZ     ILPLP
        INX     H
        XTHL
        RET
;
;
; Display  calls HEXIN, and validates a sector displacement, then con-
; verts it to an address
;
DISP    CALL    HEXIN
        PUSH    PSW             ;save delimiter
        MOV     A,D
        ORA     A
        JNZ     BADISP
        MOV     A,E
        ORA     A
        JM      BADISP
        ADI     80H             ;to point to buffer at BASE+80h
        MOV     E,A
        MVI     D,BASE/256
        POP     PSW             ;get delimiter
        RET
;
BADISP  XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '++ BAD DISPLACEMENT (NOT 0-7F) ++'
        DB      CR,LF,0
        JMP     PRMPTR
;
DHIN    INX     H               ;skip '#'
;
HEXIN   LXI     D,0
        MOV     A,M
        CPI     '#'             ;decimal?
        JZ      HDIN            ;make decimal
;
HINLP   MOV     A,M
        CALL    UPCASE
        CPI     CR
        RZ
        CPI     ';'
        RZ
        CPI     ','
        RZ
        CPI     '-'             ;'THRU'?
        RZ
        CPI     '>'
        RZ
        INX     H
        CPI     '0'
        JC      WHAT
        CPI     '9'+1
        JC      HINNUM
        CPI     'A'
        JC      WHAT
        CPI     'F'+1
        JNC     WHAT
        SUI     7
;
HINNUM  SUI     '0'
        XCHG
        DAD     H
        DAD     H
        DAD     H
        DAD     H
        ADD     L
        MOV     L,A
        XCHG
        JMP     HINLP
;
HDIN    INX     H               ;skip '#'
;
DECIN   LXI     D,0
        MOV     A,M
        CPI     '#'
        JZ      DHIN
;
DINLP   MOV     A,M
        CALL    UPCASE
        CPI     CR
        RZ
        CPI     ';'
        RZ
        CPI     ','
        RZ
        CPI     '-'             ;'THRU'?
        RZ
        CPI     '>'             ;escape ?
        RZ                      ;  yup
        INX     H
        CPI     '0'
        JC      WHAT
        CPI     '9'+1
        JNC     WHAT
        SUI     '0'
        PUSH    H
        MOV     H,D
        MOV     L,E
        DAD     H               ;x2
        DAD     H               ;x4
        DAD     D               ;x5
        DAD     H               ;x10
        ADD     L
        MOV     L,A
        MOV     A,H
        ACI     0
        MOV     H,A
        XCHG
        POP     H
        JMP     DINLP
;
;
; Read in a console buffer full
;
RDBUF   CALL    ILPRT
        DB      CR,LF,':',0
;
RDBF1   LXI     H,INBUF
        MVI     B,0
;
RDBLP   CALL    CONIN
        MOV     C,A             ;save for bs test
;
;
; Evaluate control characters
;
        CPI     'U'-40H
        JZ      RDCTLU
        CPI     CR
        JZ      RDCR
        CPI     'H'-40H
        JZ      RDBS
        CPI     7FH
        JZ      RDBS
        CPI     'R'-40H
        JZ      RDCTLR
        CPI     'X'-40H
        JZ      RDCTLX
        CPI     ' '
        JC      RDBLP
        MOV     M,A
        INX     H
        INR     B
        JM      FULL
        CALL    TYPE
        JMP     RDBLP
;
FULL    DCR     B
        DCX     H
        MVI     A,'*'           ;signal we are full
        CALL    TYPE
        JMP     RDBLP
;
;
; Got CR
;
RDCR    MOV     M,A             ;save it
        CALL    TYPE            ;echo it
        MVI     A,LF            ;echo..
        CALL    TYPE            ;..lf
        LXI     H,INBUF
        RET
;
;
; Got delete or bs, echo if bs
;
RDBS    XRA     A               ;at front..
        ORA     B               ;..of line?
        JZ      RDCTLU          ;..yes, echo ^u
        DCX     H
        DCR     B
        MOV     A,C
        CPI     'H'-40H         ;bs?
        JZ      BACKUP          ;echo the backspace character
        MOV     A,M             ;echo..
        CALL    TYPE            ;..deleted character
        JMP     RDBLP
;
BACKUP  CALL    WIPER
        JMP     RDBLP
;
RDCTLX  INR     B
;
RDCX1   DCR     B
        JZ      RDBF1
        CALL    WIPER
        JMP     RDCX1
;
WIPER   PUSH    B
        PUSH    D
        PUSH    H
        LXI     D,BSMSG         ;backspace, space, backspace
        MVI     C,PRINT
        CALL    BDOS
        POP     H
        POP     D
        POP     B
        RET
;
BSMSG   DB      BS,' ',BS,'$'
;
;
; Got CTL-R, retype
;
RDCTLR  MVI     M,CR
        CALL    CRLF
        LXI     H,INBUF
        MVI     B,0
;
RDCRL   MOV     A,M
        CPI     CR
        JZ      RDBLP
        CALL    TYPE
        INR     B
        INX     H
        JMP     RDCRL
;
;
; Got CTL-U or backup to beginning of line.
;
RDCTLU  MVI     A,'^'
        CALL    TYPE
        MVI     A,'U'
        CALL    TYPE
        JMP     RDBUF
;
CRLF    MVI     A,CR
        CALL    TYPE
        MVI     A,LF
        JMP     TYPE
;
UPCASE  CPI     60H
        RC
        ANI     5FH             ;make upper case
        RET
;
CONST   PUSH    B
        PUSH    D
        PUSH    H
VCONST  CALL    $-$             ;address filled in by 'INIT'
        POP     H
        POP     D
        POP     B
        RET
;
CONIN   PUSH    B
        PUSH    D
        PUSH    H
VCONIN  CALL    $-$             ;address filled in by 'INIT'
        POP     H
        POP     D
        POP     B
        RET
;
;
; Console out with tab expansion
;
TYPE    PUSH    B
        PUSH    D
        PUSH    H
        MOV     C,A             ;for output routine
        CPI     TAB
        JNZ     TYPE1
;
TYPTAB  MVI     A,' '
        CALL    TYPE
        LDA     TABCOL
        ANI     7
        JNZ     TYPTAB
        JMP     TYPRET
;
;
; Filter out control characters to prevent garbage during view of file
;
TYPE1   CPI     ' '
        JNC     TYPEQ
        CPI     CR
        JZ      TYPEQ
        CPI     LF
        JNZ     TYPNCR
;
TYPEQ   LDA     QFLAG
        ORA     A
        PUSH    B               ;save chr. (some CBIOS clobber the reg.)
VCONOT  CZ      $-$             ;address filled in by 'INIT'
        POP     B               ;retrieve the char.
;
;
; Update column used in tab expansion
;
        MOV     A,C             ;get character
        CPI     CR
        JNZ     TYPNCR
        MVI     A,0
        STA     TABCOL
        JMP     TYPLST
;
TYPNCR  CPI     ' '             ;CTL-character?
        JC      TYPLST          ;..no change in column
        LDA     TABCOL
        INR     A
        STA     TABCOL
;
TYPLST  LDA     PFLAG
        ANI     1
        CNZ     LIST            ;from 'C' reg.
;
TYPRET  POP     H
        POP     D
        POP     B
        RET
;
LIST    PUSH    B               ;saved regs
        PUSH    D
        PUSH    H
VLIST   CALL    $-$             ;address filled in by 'INIT'
        POP     H
        POP     D
        POP     B
        RET
;
HOME    PUSH    H
        CALL    V3CHEK          ;check for version 3.x
        JC      VHOME           ;for 1.x or 2.x
        MVI     A,BHOME         ;BIOS function number
        CALL    BDOS50          ;call BIOS via BDOS
        POP     H
        RET
;
VHOME   CALL    $-$             ;address filled in by 'INIT'
        POP     H
        RET
;
;
; Set track # in DE
;
SETTRK  PUSH    H
        LHLD    MAXTRK
        CALL    SUBDE
        POP     H
        JC      OUTLIM
        XCHG
        SHLD    CURTRK
        XCHG
        MOV     B,D
        MOV     C,E
        PUSH    H
        CALL    V3CHEK          ;check for version 3.x
        JC      VSETRK          ;for 1.x or 2.x
        MVI     A,BSETTRK       ;BIOS function number
        CALL    BDOS50          ;call BIOS via BDOS
        POP     H
        RET
;
VSETRK  CALL    $-$             ;addrress filled in by 'INIT'
        POP     H
        RET
;
SETSEC  PUSH    H
        PUSH    D
        LHLD    SYSTRK
        XCHG
        SHLD    CURSEC
        LHLD    CURTRK
        CALL    SUBDE
        POP     B
        MOV     H,B
        MOV     L,C
        PUSH    PSW             ;save flags
        CALL    V3CHEK          ;check for version 3.x
        JC      SETSE0          ;for 1.x or 2.x
        DCX     H               ;scale sector # to 0
        LDA     PHM             ;physical sector mask
        ANA     L               ;a=log. sector in buffer
        STA     LOGSEC          ;save for later
        PUSH    D
        LDA     PSH             ;physical sector shift count
        ORA     A               ;check for no shift
        JZ      SETSE1          ;jump if none
        MOV     D,A             ;loop count in 'D'
;
SETSE2  CALL    ROTRHL          ;shift sector number right
        DCR     D               ;decrement loop count
        JNZ     SETSE2          ;loop until done
;
SETSE1  POP     D
        INX     H               ;scale sector # to 1
        MOV     B,H             ;repeat number in BC
        MOV     C,L
;
;
; Moved next label up 1 line to remove 2.2 stack crash
;
SETSE0  POP     PSW             ;restore flags
        JNC     NOTSYS
        LDA     FIRST0          ;see if first sector is 0
        ORA     A
        JNZ     GSTSEC          ;no, jump away
        DCX     H               ;yes, so decrement
        JMP     GSTSEC          ;..requested, then go
;
NOTSYS  LHLD    SECTBL
        XCHG
        DCX     B
        CALL    V3CHEK          ;check for version 3.x
        JC      VSCTRN          ;for 1.x or 2.x
        MVI     A,BSECTRN       ;BIOS function number
        CALL    BDOS50          ;call BIOS via BDOS
        JMP     VSCTR3
;
VSCTRN  CALL    $-$             ;addr filled in by 'INIT'
;
VSCTR3  LDA     SPT+1           ;if spt<256 (hi-ord = 0)
        ORA     A               ; then force 8-bit translation
        JNZ     VSCTR1          ; else keep all 16 bits
        MOV     H,A
;
VSCTR1  LDA     VERFLG          ;see if version 2.x
        ORA     A               ;set flags
        JNZ     GSTSEC          ;jump if CP/M 2.x
        MVI     H,0             ;CP/M 1.4 good to only 8 bits
        MOV     L,C             ;most BIOS return the
                                ;  physical sec # in register 'C'
GSTSEC  SHLD    PHYSEC          ;this may be reduntant in most 1.4
        MOV     B,H             ;  versions, but should cause no
        MOV     C,L             ;  problems
        CALL    V3CHEK          ;check for version 3.x
        JC      VSTSEC          ;for 1.x or 2.x
        MVI     A,BSETSEC       ;BIOS function number
        CALL    BDOS50          ;call BIOS via BDOS
        POP     H
        RET
;
VSTSEC  CALL    $-$             ;address filled in by 'INIT'
        POP     H
        RET
;
OUTLIM  XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '++ Not within tracks 0-',0
        PUSH    H
        LHLD    MAXTRK
        CALL    DEC
        POP     H
        CALL    ILPRT
        DB      ' ++'
        DB      CR,LF,0
        CALL    NORITE
        JMP     PRMPTR
;
SETDMA  PUSH    H               ;save HL
        MOV     H,B             ;get DMA address in HL
        MOV     L,C
        SHLD    DMAADR          ;save for later
        POP     H               ;restore HL
        CALL    V3CHEK          ;check for version 3.x
        JC      VSTDMA          ;for 1.x or 2.x
        MVI     A,BSETDMA       ;BIOS function number
        JMP     BDOS50          ;call BIOS via BDOS
;
VSTDMA  JMP     $-$             ;address filled in by 'INIT'
;
READ    MVI     A,1
        STA     WRFLG
        PUSH    H
        CALL    V3CHEK          ;check for version 3.x
        JC      VREAD           ;for 1.x or 2.x
        CALL    READ3           ;read a physical sector
        JNZ     RDERR           ;if error on read
        XCHG                    ;sector address to DE
        LHLD    DMAADR          ;DMA address to HL
        XCHG                    ;reverse addresses for:
        CALL    MOVE            ;move
        POP     H               ;read finished
        RET
;
VREAD   CALL    $-$             ;address filled in by 'INIT'
        ORA     A
        JZ      READOK
;
RDERR   XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '++ READ failed, sector may be invalid ++'
        DB      CR,LF,0
;
READOK  POP     H
        RET
;
WRITE   LDA     WRFLG
        ORA     A
        JNZ     PWRITE
;
BADW    XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '++ CANNOT WRITE UNLESS READ ISSUED ++'
        DB      CR,LF,0
        JMP     EXPL
;
PWRITE  PUSH    H
        MVI     C,1             ;force write type 1 in case 2.x deblock
        CALL    V3CHEK          ;check for version 3.x
        JC      VWRITE          ;for 1.x or 2.x
        CALL    READ3           ;get physical sector
        JNZ     WRERR           ;if error
        XCHG                    ;sector address to DE
        LHLD    DMAADR          ;DMA address to HL
        CALL    MOVE            ;move data to buffer
        MVI     A,BWRITE        ;BIOS write function
        CALL    BDOS50          ;call BIOS via BDOS
        JMP     VWRIT3          ;write done
;
VWRITE  CALL    $-$             ;addr filled in by 'INIT'
;
VWRIT3  ORA     A
        JZ      WRITOK
;
WRERR   XRA     A
        STA     QFLAG
        CALL    ILPRT
        DB      '++ WRITE failed ++',CR,LF,0
;
WRITOK  POP     H
        RET
;
READ3   LXI     B,DBUFF         ;CP/M version 3.x read
        PUSH    B               ;save for later
        MVI     A,BSETDMA       ;set DMA address
        CALL    BDOS50
        MVI     A,BREAD         ;read a buffer
        CALL    BDOS50
        POP     H               ;buffer address to HL
        ORA     A               ;check error code
        RNZ                     ;return if error
        LXI     B,128           ;logical sector size
        LDA     LOGSEC          ;get logical sect number
        ORA     A               ;no offset if zero
        RZ                      ;HL=sect addr, BC=len
;
READ3L  DAD     B               ;offset to desired sector
        DCR     A               ;count down log sector number
        JNZ     READ3L          ;loop until done
        RET                     ;HL=sect addr, BC=len
;
V3CHEK  LDA     VERFLG          ;check for version 3.x
        CPI     30H             ;carry set if not 3.x
        RET
;
BDOS50  SHLD    HLREG           ;save regs in parameter block
        XCHG
        SHLD    DEREG
        MOV     H,B
        MOV     L,C
        SHLD    BCREG
        STA     FUNC
        LXI     D,FUNC          ;point to parameter block
        MVI     C,50            ;BDOS function number
        JMP     BDOS
;
;
;***********************************************************************
;
;                            Help Guide
;
;***********************************************************************
;
HELP    CALL    ILPRT
        DB      CR,LF,CR,LF,'Operands in brackets [...] are optional'
        DB      CR,LF,'Numeric values: ''n'' are decimal, ''x'' hex'
        DB      CR,LF,CR,LF
        DB      '+[n]     step in [n] sectors',CR,LF
        DB      '-[n]     step out [n] sectors',CR,LF
        DB      '#        print disk parameters for current drive',CR,LF
        DB      '=Abc     search for ASCII Abc from current sector',CR,LF
        DB      '             caution: upper/lower case matters',CR,LF
        DB      '             use <xx> for hex:',CR,LF
        DB      '                 to find "IN 0" use: =<db><0>     or'
        DB      CR,LF
        DB      '                 "(tab)H,0(CR)(LF)" use: =<9>H,0<D><A>'
        DB      CR,LF
        DB      '<        save current sector into memory buffer',CR,LF
        DB      '>        restore saved sector',CR,LF
        DB      '(        toggle map display of erased files',CR,LF
        DB      '?        help (displays this guide)',CR,LF
        DB      'A[ff,tt] ASCII dump',CR,LF
        DB      'B[nn]    Boot track number of sectors per track'
        DB      CR,LF,CR,LF,CR,LF,CR,LF,CR,LF,CR,LF,CR,LF
        DB      '[More]'
        DB      1,CR,'       ',CR,LF,CR,LF
        DB      'CHANGE:',CR,LF,CR,LF
        DB      'CHaddress,byte,byte... (hex)',CR,LF
        DB      'CAaddress,data...  (ASCIIx',CR,LF
        DB      '         <xx> Allowed for imbedded hex',CR,LF
        DB      'CHfrom-through,byte, e.g. CH0-7F,E5',CR,LF
        DB      'CAfrom-through,byte',CR,LF
        DB      'D[ff,tt] Dump (hex+ASCII)',CR,LF
        DB      'Fn.t     Find file',CR,LF
        DB      'Gnn      CP/M Allocation Group nn',CR,LF
        DB      'H[ff,tt] hex dump',CR,LF
        DB      'K[du:]filename[.ext]  Dump sequential memory to disk'
        DB      CR,LF,'L        Log in drive',CR,LF
        DB      'LX       Log in drive X',CR,LF
        DB      'M[nn]    Map [from group nn]'
        DB      CR,LF,CR,LF,CR,LF,CR,LF,CR,LF
        DB      CR,LF,CR,LF,CR,LF,CR,LF
        DB      '[More]'
        DB      1,CR,'       ',CR,LF,CR,LF
        DB      'N        New disk',CR,LF
        DB      'P        Printer toggle switch',CR,LF
        DB      'Q        Quiet mode (no msgs)',CR,LF
        DB      'R        Read current sector',CR,LF
        DB      'Snn      Sector nn',CR,LF
        DB      'Tnn      Track nn',CR,LF
        DB      'Unn      User nn for Find command (CP/M-2 only)',CR,LF
        DB      'V[nn]    View [nn] ASCII sectors',CR,LF
        DB      'W        Write current sector',CR,LF
        DB      'X        Exit program',CR,LF
        DB      'Y        Yank current sector into sequential memory'
        DB      CR,LF,'Z[nn]    Sleep [nn tenths]',CR,LF
        DB      '/[nn]    Repeat [nn (decimal) times]'
        DB      CR,LF,CR,LF,CR,LF,CR,LF,CR,LF,CR,LF
        DB      CR,LF,CR,LF,CR,LF,CR,LF,CR,LF
        DB      '[More]'
        DB      1,CR,'       ',CR,LF,CR,LF,CR,LF,CR,LF,CR,LF,CR,LF,CR,LF
        DB      'Cancel a function with C or Ctl-C',CR,LF
        DB      'Suspend output with S or Ctl-S',CR,LF
        DB      'Separate commands with ";"',CR,LF
        DB      '         Example: G0',CR,LF
        DB      '         +;D;Z#20;/',CR,LF
        DB      '             would step in, dump, sleep 2 seconds'
        DB      CR,LF
        DB      '             and repeat until  CTL-C is typed',CR,LF
        DB      'All "nn" usage except "/", "T", and "S" are hex',CR,LF
        DB      '             (use #nn for decimal)'
        DB      CR,LF,CR,LF,CR,LF,CR,LF,CR,LF
        DB      '(See DU.DOC for complete examples)'
        DB      CR,LF,CR,LF,CR,LF,CR,LF,0
        JMP     PROMPT
;
;
;***********************************************************************
;
;                       Utility Subroutines
;
;***********************************************************************
;
GRPCMP  MOV     A,C
        INR     D
        DCR     D
        JZ      CMP8
        CMP     M
        INX     H
        RNZ
        MOV     A,B
;
CMP8    CMP     M
        RET
;
;
; 2's complement HL ==> HL
;
NEG     MOV     A,L
        CMA
        MOV     L,A
        MOV     A,H
        CMA
        MOV     H,A
        INX     H
        RET
;
;
; HL/2 ==> HL
;
ROTRHL  ORA     A
        MOV     A,H
        RAR
        MOV     H,A
        MOV     A,L
        RAR
        MOV     L,A
        RET
;
;
; Collect the number of '1' bits in 'A' as a count in 'L'
;
COLECT  MVI     H,8
;
COLOP   RAL
        JNC     COSKIP
        INR     L
;
COSKIP  DCR     H
        JNZ     COLOP
        RET
;
;
; HL-DI ==> HL
;
SUBDE   MOV     A,L
        SUB     E
        MOV     L,A
        MOV     A,H
        SBB     D
        MOV     H,A
        RET
;
;
; Quick kludge multiply, HL*DE ==> HL
;
MULT    PUSH    B
        PUSH    D
        XCHG
        MOV     B,D
        MOV     C,E
        MOV     A,B
        ORA     C
        JNZ     MULCON
        LXI     H,0             ;filter special case
        JMP     MLDONE          ;..of multiply by 0
;
MULCON  DCX     B
        MOV     D,H
        MOV     E,L
;
MULTLP  MOV     A,B
        ORA     C
        JZ      MLDONE
        DAD     D
        DCX     B
        JMP     MULTLP
;
MLDONE  POP     D
        POP     B
        RET
;
;
; Routine to fill in disk parameters with every drive change
;
LOGIT   LDA     VERFLG
        ORA     A               ;if not CP/M 2.x then
        JZ      LOG14           ;  do it as 1.4
        LXI     D,DPB           ;  then move to local
        MVI     B,DPBLEN        ;  workspace
        CALL    MOVE
        LHLD    SPT
        SHLD    SPT00           ;assume track 00 same
        JMP     LOGCAL
;
LOG14   LHLD    BDOS+1          ;first find 1.4 BDOS
        MVI     L,0
        LXI     D,DPBOFF        ;then offset to the 1.4 DPB
        DAD     D
        MVI     D,0
        MOV     E,M             ;now move parameters
        INX     H
        XCHG
        SHLD    SPT
        SHLD    SPT00           ;assume track 00 same
        XCHG
        MOV     E,M
        INX     H
        XCHG
        SHLD    DRM
        XCHG
        MOV     A,M
        INX     H
        STA     BSH
        MOV     A,M
        INX     H
        STA     BLM
        MOV     E,M
        INX     H
        XCHG
        SHLD    DSM
        XCHG
        MOV     E,M
        INX     H
        XCHG
        SHLD    AL0
        XCHG
        MOV     E,M
        XCHG
        SHLD    SYSTRK
;
LOGCAL  LXI     H,GRPDIS
        MOV     A,M
        PUSH    PSW
        LDA     BLM
        MOV     M,A
        PUSH    H
        LHLD    DSM
        XCHG
        CALL    GTKSEC
        SHLD    MAXSEC
        XCHG
        SHLD    MAXTRK
        POP     H
        POP     PSW
        MOV     M,A
        MVI     L,0             ;initialize count
        LDA     AL0             ;start with al0
        CALL    COLECT          ;count bits
        LDA     AL1             ;do al1
        CALL    COLECT          ;count bits
        MVI     H,0             ;make into 16-bit integer
        SHLD    DIRGRP          ;save count
        RET
;
;
; Temporary storage area
;
BUFAD   DW      BASE+100H       ;forces initial read
HEXAD   DW      0               ;to re-fetch a value
TOGO    DW      0FFFFH          ;repeat count (FFFF=cont)
TWOUP   DB      0
TOGE    DB      0
PFLAG   DB      0               ;1=print
GROUP   DW      0
GRPDIS  DB      0
SAVEFL  DB      0
CURTRK  DW      0
CURSEC  DW      1
PHYSEC  DW      1
LOGSEC  DB      0               ;logical sector
TABCOL  DB      0
FILECT  DW      0
DIRPOS  DB      0
FINDFL  DB      0               ;1=must position after find
FTSW    DB      1               ;search w/o increment
NOTPOS  DB      1               ;initially not positioned
WRFLG   DB      0               ;may not write until '+', '-', or 'G'
QFLAG   DB      0               ;quiet? (0=no)
FIRST0  DB      0               ;sets to 0 if first sec # is 0
DRIVE   DB      0
MAXTRK  DW      0
MAXSEC  DW      0
SPT00   DW      0               ;track 00 sectors per track
DLMREQ  DB      0               ;delimiter required switch
DIRGRP  DW      0               ;count of groups allocated to directory
SAVTRK  DW      0               ;track save area
SAVSEC  DW      0               ;sector save area
VERFLG  DB      0               ;CP/M version number
MPMFLG  DB      0               ;non-zero => MP/M
SECTBL  DW      0               ;pointer to sector skew table
MFPTR   DW      0
DUPFLG  DB      0
YNKADR  DW      0               ;pointer to current yank address
DMAADR  DW      0080H           ;DMA address
;
FUNC    DB      0               ;BDOS parameter block
AREG    DB      0
BCREG   DW      0
DEREG   DW      0
HLREG   DW      0
;
BACK    DS      2               ;to back up in "0CA0-7F,X"
DUMTYP  DS      1
;
YDN     DS      2               ;"K" work area
UZER    DS      1
CUZER   DS      1
SCOUNT  DS      1
ENDVAL  DS      2
;
;-----------------------------------------------------------------------
;
; The disk parameter block is moved here from CP/M
;
DPB     EQU     $
;
SPT     DS      2
BSH     DS      1
BLM     DS      1
EXM     DS      1
DSM     DS      2
DRM     DS      2
AL0     DS      1
AL1     DS      1
CKS     DS      2
SYSTRK  DS      2
PSH     DS      1               ;physical shift count
PHM     DS      1               ;physical sector mask
;
;               End of disk parameter block
;-----------------------------------------------------------------------
;
SAVBUF  DS      128
INBUF   DS      128
DBUFF   DS      4096            ;physical sector buffer
;
;
; Directory read in here; also search work area
;
WORK    EQU     $
DIRECT  EQU     $
;
        END
    U".¢