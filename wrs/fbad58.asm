;                         FBAD.ASM ver. 5.8
;                         (revised 12/12/84)
;                         (from a 11/29/84 revision)
;                  NON-DESTRUCTIVE DISK TEST PROGRAM
;
;                    (formerly called FINDBAD.ASM)
;
; FBAD will find all bad blocks on a disk and build a file [UNUSED].BAD
;to  allocate  them, thus "locking out" the bad blocks so CP/M will not
;use them.  This allows continued use of the disk as though it  had  no
;bad areas.
;If an [UNUSED].BAD file is found on the disk before the test, you will
;be prompted to keep the existing file (and all currently flagged bad blocks)
;or erase it and only flag the bad blocks found on this test pass.
;
; Originally  written  by Gene Cotton, published in "Interface Age" for
;September 1980, page 80.
;
; See notes below concerning 'TEST' conditional assembly option, SYSTST
;and BADUSR directives.
;
; This  program  has  been written to allow it to work with (hopefully)
;all CP/M 2.x systems, and most 1.4 CP/M systems.  It has  been  tested
;on numerous different disk systems.
;
;=======================================================================
;
; SYSTST, BADUSR and ASTRS options:
;
; Many  double-density  disk systems have single-density system tracks.
;If this is true with your system, you can change the program  to  skip
;the system tracks, without re-assembling it.  To do this, set the byte
;at  103H  to a 0 if you don't want the system tracks tested, otherwise
;keep it 1.  The program tests if you have  a  "blocked"  disk  system,
;that  is,  when the same physical disk is separated into logical disks
;by use of the SYSTRK word in the disk parameter block.  If more than 5
;tracks are specified, the program skips the system tracks.
;
; If  you are using CP/M 2.x , you may assign the user number where the
;[UNUSED.BAD] file will be created, by changing the byte at 104H to the
;desired user number.

; FINDBAD  displays the TRACK-Nr it has checked on the screen-terminal.
;If you like to log the results on a printer (or you  have  a  hardcopy
;terminal,  you  may  want  to change LOC 105H to a non-zero value, and
;FINDBAD will display a * for each track checked.  The number  in  105H
;controls  the  number  of  *'s  per line.  (Note patch values are HEX:
;76=4CH.)  Use ^P to turn the printer on before running FBAD,  it  will
;be automatically turned off by the warm boot at the end.

; NOTE:  These changes can be done with DDT as follows:
;
;               A>DDT FBAD.COM
;               -S103
;               103 01 00       ;Don't test SYSTEM tracks
;               104 FF 0F       ;Put [UNUSED.BAD] in USER 15
;               105 00 4C       ;Issue CR/LF after 76 *'s
;               106 31 .        ;Finished with changes
;               -^C
;
;               A>SAVE xx FBAD.COM
;
;=======================================================================
;
;                          USING THE PROGRAM
;
; Before  using  this  program  to  "reclaim"  a diskette, the diskette
;should be reformatted.  If this  is  not  possible,  at  least  assure
;yourself  that  any  existing  files  on  the  diskette do not contain
;unreadable  sectors.   If  you  have  changed  disks  since  the  last
;warm-boot, you must warm boot again before running this program.
;
; To  use the program, insert both the disk containing FBAD.COM and the
;diskette to be checked into the disk drives.  The diskette  containing
;FBAD.COM  can  be  the  one  that  is  to be checked.  Assume that the
;program is on drive "A" and the suspected bad disk is  on  drive  "B".
;In  response  to  the CP/M prompt "A>", type in FBAD B: This will load
;the file FBAD.COM from drive "A" and test the diskette  on  drive  "B"
;for  unreadable  sectors.   The  only  allowable  parameter  after the
;program name is a drive specification (of the form " N:")  for  up  to
;four  (A  to  D) disk drives.  If no drive is specified, the currently
;logged-in drive is assumed to contain the diskette to check.
;
; The  program  first  checks the CP/M System tracks (up to 5), and any
;errors here prohibit the disk from being used on drive "A", since  all
;"warm boots" occur using the system tracks from the "A" drive.  Floppy
;disks  normally  use 2 tracks for the system; Winchester disks may use
;one or more.
;
; Version  5.5  and  later  automatically skip the system check if 5 or
;more tracks are reserved for the system.  This allows the  program  to
;be used on BOTH floppy and Winchester systems without patching.
;
; The  program  next  checks  the  first two data blocks containing the
;diskette directory.  If errors occur here, the program terminates with
;the control returning to CP/M.  No  other  data  blockes  are  checked
;since error in the directory render the disk useless.
;
; Finally,  all  the  remaining  data  blocks are checked.  Any sectors
;which are unreadable cause the data block which contains  them  to  be
;stored  temporarily  as  a "bad block".  At the end of this phase, the
;message "nn bad blocks found" is displayed (where nn  is  replaced  by
;the  number  of  bad blocks, or "No" if no read errors occur).  If bad
;blocks occur, the filname [UNUSED].BAD is created, the  list  of  "bad
;blocks"  is  placed  in  the allocation map of the directory entry for
;[UNUSED].BAD, and the file is closed.  When the number of "bad blocks"
;exceeds 16, the program will open additional extents  as  required  to
;hold  the  overflow.   If  the diskette has more than 32 "bad blocks",
;perhaps it should be sent to the "big disk drive in the sky"  for  the
;rest it deserves.
;
; If  any "bad blocks" do occur, they are allocated to [UNUSED].BAD and
;no longer will be available  to  CP/M  for  future  allocation.   This
;effectively  locks  out  bad  sectors  on  the  diskette, allowing its
;continued use.
;
;                 Using the TEST conditional assembly
;
; A  conditional  assembly has been added to allow testing this program
;to make sure  it  is  reading  all  sectors  on  your  disk  that  are
;accessible  to  CP/M.   The program reads the disk on a block-by-block
;basis, so it is necessary to first  determine  the  number  of  blocks
;present.   To  commence,  we  must know the number of sectors/block (8
;sectors/block for standard IBM single density format).  If this  value
;is not known, it can be easily found by saving one page in a test file
;and interrogating using the STAT command:
;
;       A>SAVE 1 TEST.SIZ
;       A>STAT TEST.SIZ
;
; For  standard single-density, STAT will report this file as being 1k.
;The file size reported (in bytes) is the size of a block.  This  value
;divided  by 128 bytes/sector (the standard CP/M sector size) will give
;sectors/block.  For our IBM single density example, we have:
;
;      (1024 bytes/block) / (128 bytes/sector) = 8 sectors/block.
;
; We  can  now  calculate  blocks/track (assuming we know the number of
;sectors per track.  In our example:
;
;      (26 sectors/track) / (8 sectors/block) = 3.25 blocks/track
;
; Now  armed with the total number of data tracks (75 in our IBM single
;density example), we get total blocks accessible:
;
;      75 (tracks/disk) x (3.25 blocks/track) = 243.75 blocks/disk
;
; CP/M  cannot  access  a  fractional  block,  so we round down (to 243
;blocks in our example).  Now multiplying total blocks by  sectors  pre
;block  results in total sectors as should be reported when TEST is set
;YES and a good disk is read.  For our  example,  this  value  is  1944
;sectors.
;
; Finally,  note  that if SYSTST is set YES, the sectors present on the
;system tracks will be displayed as well.   In  the  previous  example,
;this  results  in 1944 + 52 = 1996 sectors (reported separately by the
;TEST conditional).  Version 5.4 reported these as a  single  total  at
;the end.
;
; Run  the  program on a KNOWN-GOOD disk.  It should report that it has
;read the correct number of sectors.   The  test  conditional  assembly
;should then be set NO and the program re-assembled.  The test routines
;cannot be left in, since this program does not read all the sectors in
;a  block  that  is  found to be bad and thus will report an inaccurate
;number of sectors read.
;
;=======================================================================
;
; 12/12/84  Ron Schwabel - Tampa, FL (813) 962-4358 (voice)
;               Tampa Bay RCPM/RBBS (813) 831-7276 (Charlie Hoffman)
;
;               Added the ability to keep bad blocks that were flagged in
;               a previous [UNUSED].BAD file 
;               If a block was ever flagged as bad by this program, it is
;               probably weak.  If on a subsequent test, it makes it
;               through the BIOS retries and is read successfully,
;               I want the block to stay in the UNUSED.BAD file.
;               Removed the code in LTOP which cleared the high
;               byte of HL after a call to SECTRN.  My BIOS (Morrow DJDMA)
;               sets the high bit of HL to indicate SIDE 1 of a
;               double sided drive.
;
;
; 11/29/84 Integrated Mike Webbs idea to display Track-Nr. Changed DOC
;          up front accordingly. (BGE)
;
; 07/04/84 Added Ted Shapin's fixes from 1981 that were not included in
;          the 06/07/84 version.  Reformatted. (IMH)
;
; 06/07/84 Added code at CHKSYS: to skip system tracks if more than 5 are
;          present (most systems use 1 or at most 2 tracks for the sys-
;          tem). This makes the program practical for both floppy and
;          Winchester systems.  Cosmetic change for printer logging to
;          add CR/LF after 76 *'s.  Fixed problem in DECOUT to give cor-
;          rect total for max size Winchester disks. (DHR)
;
; 05/21/81 Corrected error in description of how to set SYSTST byte at
;          103H.  Added CRLF to block error message. (KBP)
;
; 05/19/81 Corrected omission in DOLOG routine so that BADUSR will work
;          correctly. Thanks to Art Larky. (CHS)
;
; 04/10/81 Changed extent DB from -1 to 0FFH so program can be assembled
;          by ASM.  Added BADUSR info to instructions for altering with
;          DDT. (KBP)
;
; 04/09/81 Changed sign-on message, added control-c abort test, added
;          '*' to console once each track. (RGF)
;
; 04/07/81 Re-wrote to add the following features:
;               1) "Universal" operation
;               2) DDT-changeable "SYSTRK" boolean (see above)
;               3) Report to console when bad blocks are detected (RGF)
;
;=======================================================================
;
YES:    EQU     0FFH
NO:     EQU     0
BEEP:   EQU     07H
;
;=======================================================================
;
; Conditional assembly switch for testing this program (for initial
; testing phase only - see remarks above.)
;
TEST:   EQU     NO              ;yes for testing only
;
;=======================================================================
;
; System equates
;
BASE:   EQU     0               ;normal CP/M base address
BDOS:   EQU     BASE+5          ;CP/M warm boot entry
FCB:    EQU     BASE+5CH        ;CP/M default FCB location
;
;
; Define ASCII characters used
;
CR:     EQU     0DH             ;carriage return character
LF:     EQU     0AH             ;line feed character
TAB:    EQU     09H             ;tab character
;
DPBOFF: EQU     3AH             ;CP/M 1.4 offset to DPB within BDOS
TRNOFF: EQU     15              ;CP/M 1.4 offset to sector xlate routine
;
;
        ORG     BASE+100H
;
;
        JMP     START           ;jmp around option bytes
;
;
; If you want the system tracks tested, then put a 1 here, otherwise 0.
;
SYSTST: DB      0       ;1
;
;
; If using CP/M 2.x change this byte to the user number you want
; [UNUSED].BAD to reside in.
;
BADUSR: DB      0               ;user # where [UNUSED.BAD] goes
;
;
; Set this byte to the number of *'s you want per display line
;
ASTRS:  DB      0,0             ;number of *'S PER LINE (0 == 'track xx')
;
;
;=======================================================================
;
;                         PROGRAM STARTS HERE
;
;=======================================================================
;
START:  LXI     SP,NEWSTK       ;make new stack
        CALL    START2          ;go print signon
        DB      CR,LF,'FBAD58 - Bad Sector Lockout '
        DB      'program',CR,LF,CR,LF
        DB      'Type CTRL-C to abort',CR,LF,'$'
;
START2: POP     D               ;get message address
        CALL    PSTRNG          ;
        CALL    SETUP           ;set BIOS entry, and check drive
        CALL    ZMEM            ;zero all available memory
        CALL    FINDB           ;establish all bad blocks
        JZ      NOBAD           ;say no bad blocks, if so
        CALL    SETDM           ;fix DM bytes in FCB
;
NOBAD:  CALL    CRLF
        MVI     A,TAB
        CALL    TYPE
        LXI     D,NOMSG         ;point first to 'NO'
        LHLD    BADBKS          ;pick up # bad blocks
        MOV     A,H             ;check for zero
        ORA     L
        JZ      PMSG1           ;jump if none
        LXI     D,BEEPS
        CALL    PSTRNG          ;
        LXI     H,BADBKS
        CALL    DECOUT          ;oops..had some bad ones, report
        LXI     D,BEEPS
        CALL    PSTRNG          ;
        JMP     PMSG2
;
PMSG1:  CALL    PSTRNG          ;
;
PMSG2:  LXI     D,ENDMSG        ;rest of exit message
;
PMSG:   CALL    PSTRNG          ;
;
         IF     TEST
        MVI     A,TAB           ;get a tab
        CALL    TYPE            ;print it
        LXI     H,SECCNT
        CALL    DECOUT          ;print it
        LXI     D,SECMSG        ;point to message
        CALL    PSTRNG          ;
         ENDIF                  ;TEST
;
        JMP     BASE+0          ;exit to CP/M warm boot
;
;
; Get actual address of BIOS routines
;
SETUP:  LHLD    BASE+1          ;get base address of BIOS vectors
;
;
; WARNING...Program modification takes place here...do not change.
;
        LXI     D,24            ;offset to "SETDSK"
        DAD     D
        SHLD    SETDSK+1        ;fix our call address
        LXI     D,3             ;offset to "SETTRK"
        DAD     D
        SHLD    SETTRK+1        ;fix our call address
        LXI     D,3             ;offset to "SETSEC"
        DAD     D
        SHLD    SETSEC+1        ;fix our call address
        LXI     D,6             ;offset to "DREAD"
        DAD     D
        SHLD    DREAD+1         ;fix our call address
        LXI     D,9             ;offset to CP/M 2.x SECTRAN
        DAD     D
        SHLD    SECTRN+1        ;fix our call address
        MVI     C,12            ;get version function
        CALL    BDOS
        MOV     A,H             ;save as flag
        ORA     L
        STA     VER2FL
        JNZ     GDRIV           ;skip 1.4 stuff if is 2.x
        LXI     D,TRNOFF        ;CP/M 1.4 offset to SECTRAN
        LHLD    BDOS+1          ;set up jump to 1.4 SECTRAN
        MVI     L,0
        DAD     D
        SHLD    SECTRN+1
;
;
; Check for drive specification
;
GDRIV:  LDA     FCB             ;get drive name
        MOV     C,A
        ORA     A               ;zero?
        JNZ     GD2             ;if not,then go specify drive
        MVI     C,25            ;get logged-in drive
        CALL    BDOS
        INR     A               ;make 1-relative
        MOV     C,A
;
GD2:    LDA     VER2FL          ;if CP/M version 2.x
        ORA     A
        JNZ     GD3             ;SELDSK will return select error
;
;
; Is CP/M 1.4, which doesn't return a select error, so we have to do it
; here
;
        MOV     A,C
        CPI     4+1             ;check for highest drive number
        JNC     SELERR          ;select error
;
GD3:    DCR     C               ;back off for CP/M
        PUSH    B               ;save disk selection
        MOV     E,C             ;align for BDOS
        MVI     C,14            ;select disk function
        CALL    BDOS
        POP     B               ;get back disk number
;
;
; EXPLANATION: WHY WE DO THE SAME THING TWICE
;
;       You might notice that we are doing the disk selection twice, once
;       by a BDOS call and once by direct BIOS call.  The BIOS call is
;       necessary in order to get the necessary pointer back from CP/M
;       (2.x) to find the sector translate table.  The BDOS call is ne-
;       cessary to keep CP/M in step with the  BIOS...Later the file
;       [UNUSED].BAD may need to be created and CP/M must know which
;       drive is being used.
;
SETDSK: CALL    $-$             ;direct BIOS vedtor filled in at init
        LDA     VER2FL
        ORA     A
        JZ      DOLOG           ;jump if CP/M 1.4
        MOV     A,H
        ORA     L               ;check for 2.x
        JZ      SELERR          ;jump if select error
        MOV     E,M             ;get sector table pntr
        INX     H
        MOV     D,M
        INX     H
        XCHG
        SHLD    SECTBL          ;store it away
        LXI     H,8             ;offset to dpb pointer
        DAD     D
        MOV     A,M             ;pick up DPB pointer
        INX     H               ;  to use
        MOV     H,M             ;  as parameter
        MOV     L,A             ;  to logit
;
DOLOG:  CALL    LOGIT           ;log in drive, get disk parms
        CALL    GETDIR          ;calculate directory information
;
;
; Now set the required user number
;
        LDA     VER2FL
        ORA     A
        RZ                      ;no users in CP/M 1.4
        LDA     BADUSR          ;get the user number
        MOV     E,A             ;bdos call needs user # in 'E'
        MVI     C,32            ;get/set user code
        CALL    BDOS
        RET
;.....
;
;
; Look for bad blocks
;
FINDB:  LDA     SYSTST
        ORA     A
        JZ      DODIR           ;jump if no system tracks to be tested
        CALL    CHKSYS          ;check for bad blocks on track 0 and 1
;
DODIR:  CALL    CHKDIR          ; for bad blocks in directory
        CALL    TELL1
        DB      CR,LF,'Testing data area...',CR,LF,'$'
;
TELL1:  POP     D
        CALL    PSTRNG          ;
        LDA     ASTRS           ;set column count
        STA     COLUMN
        CALL    ERAB            ;erase any [unused].bad file
        LHLD    DIRBKS          ;start at first data block
        MOV     B,H             ;put into 'BC'
        MOV     C,L
;
FINDBA: CALL    READB           ;read the block
        CNZ     SETBD           ;if bad, add block to list
        INX     B               ;bump to next block
        LHLD    DSM
        MOV     D,B             ;set up for (MAXGRP - CURGRP)
        MOV     E,C
        CALL    SUBDE           ;do subtract: (MAXGRP - CURGRP)
        JNC     FINDBA          ;until CURGRP>MAXGRP
        CALL    CRLF
        LHLD    DMCNT           ;get number of bad sectors
        MOV     A,H
        ORA     L               ;set zero flag, if no bad blocks
        RET                     ;return from "FINDB"
;.....
;
;
; Check system tracks, notify user if bad, but continue
;
CHKSYS: LHLD    SYSTRK          ;get # system tracks
        MOV     A,H             ;get high part
        ORA     A
        JNZ     SKPSYS          ;skip system track check if non-zero
        MOV     A,L             ;get low part
        CPI     5               ;>5 tracks?
        JNC     SKPSYS          ;skip check if so
        LDA     ASTRS           ;set column counter
        STA     COLUMN
        CALL    CHSY1           ;print message
        DB      CR,LF,'Testing system tracks...',CR,LF,'$'
;
CHSY1:  POP     D
        CALL    PSTRNG          ;
        LXI     H,0             ;set track 0,sector 1
        SHLD    TRACK
        INX     H
        SHLD    SECTOR
;
CHKSY1: CALL    READS           ;read a sector
        JNZ     SYSERR          ;notify, if bad blocks here
        LHLD    SYSTRK          ;set up 
        XCHG
        LHLD    TRACK
        CALL    SUBDE           ;do the subtract
        JC      CHKSY1          ;loop while track < SYSTRK
;
         IF     TEST
        CALL    CRLF
        MVI     A,TAB           ;get a tab
        CALL    TYPE            ;print it
        LXI     H,SECCNT        ;get # sectors so far
        CALL    DECOUT          ;print it
        LXI     D,SYSMSG        ;point to message
        CALL    PSTRNG          ;
         ENDIF                  ;TEST
;
        RET                     ;return from "CHKSYS"
;.....
;
;
SKPSYS: LXI     D,SKPMSG        ;say skipping system tracks
        MVI     C,9
        JMP     BDOS
;
SKPMSG: DB      CR,LF,'Skipping system tracks...',CR,LF,'$'
;
SYSERR: LXI     D,ERMSG5        ;say no go, and bail out
PSTRNG: MVI     C,9             ;BDOS PRINT string function
        CALL    BDOS
        RET                     ;return from "syserr" or subroutine
;.....
;
;
; Check for bad blocks in directory area
;
CHKDIR: CALL    CHKD1
        DB      CR,LF,'Testing directory area...',CR,LF,'$'
;
CHKD1:  POP     D
        CALL    PSTRNG
        LXI     B,0             ;start at block 0
;
CHKDI1: CALL    READB           ;read a block
        JNZ     ERROR6          ;if bad, indicate error in directory area       
        INX     B               ;bump for next block
        LHLD    DIRBKS          ;set up (CURGRP - DIRBKS)
        DCX     H               ;make 0-relative
        MOV     D,B
        MOV     E,C
        CALL    SUBDE           ;do the subtract
        JNC     CHKDI1          ;loop until CURGRP > DIRGRP
        RET                     ;return from "CHKDIR"
;.....
;
;
; Read all sectors in block, and return zero flag set if none bad
;
READB:  CALL    CNVRTB          ;convert to track/sector in 'HL' regs.
        LDA     BLM
        INR     A               ;number of sectors/block
        MOV     D,A             ;  in 'D' register
;
READBA: PUSH    D
        CALL    READS           ;read skewed sector
        POP     D
        RNZ                     ;error if not zero...
        DCR     D               ;debump sector/block
        JNZ     READBA          ;do next, if not finished
        RET                     ;return from "READBA"
;.....
;
;
; Convert block number to track and skewed sector number
;
CNVRTB: PUSH    B               ;save current group
        MOV     H,B             ;need it in 'HL'
        MOV     L,C             ; for easy shifting
        LDA     BSH             ;DPB value that tells how to
;       
SHIFT:  DAD     H               ;  shift group number to get
        DCR     A               ;  disk-data-area relative
        JNZ     SHIFT           ;  sector number
        XCHG                    ;rel sector # into 'DE'
        LHLD    SPT             ;sectors per track from DPB
        CALL    NEG             ;faster to dad than call SUBDE
        XCHG
        LXI     B,0             ;initialize quotient
;
;
; Divide by number of sectors
;       quotient = track
;            mod = sector
;
DIVLP:  INX     B               ;dirty division
        DAD     D
        JC      DIVLP
        DCX     B               ;fixup last
        XCHG
        LHLD    SPT
        DAD     D
        INX     H
        SHLD    SECTOR          ;now have logical sector
        LHLD    SYSTRK          ;but before we have track #,
        DAD     B               ;  we have to add system track offset
        SHLD    TRACK
        POP     B               ;this was our group number
        RET
;.....
;
;
; READS reads a logical sector (if it can) and returns zero flag set if
; no error.
;
READS:  PUSH    B               ;save the group number
        CALL    LTOP            ;convert logical to physical
        LDA     VER2FL          ;now check version
        ORA     A
        JZ      NOTCP2          ;skip this stuff if CP/M 1.4
        LHLD    PHYSEC          ;get physical sector
        MOV     B,H             ;into 'BC'
        MOV     C,L
;
SETSEC: CALL    $-$             ;address filled in at init
;
;
; QUICK NOTE OF EXPLANATION: This code appears as if we skipped the
; SETSEC routine for 1.4 CP/M users.  That is not true.  In CP/M 1.4,
; the call within the LTOP routine to SECTRAN actually does the set
; sector, so no need to do it twice.          (RGF)
;
NOTCP2: LHLD    TRACK           ;now set the track
        MOV     B,H             ;CP/M wants it in 'BC'
        MOV     C,L
;
SETTRK: CALL    $-$             ;address filled in at init
;
;
; Now do the sector read
;
DREAD:  CALL    $-$             ;addresss filled in at init
        ORA     A               ;set flags
        PUSH    PSW             ;save error flag
;
         IF     TEST
        CALL    INCSEC          ;increment sector count
;       LHLD    SECCNT          ;get sector count
;       INX     H               ;add one
;       SHLD    SECCNT          ;save new count
         ENDIF                  ;TEST
;
        LHLD    SECTOR          ;get logical sector #
        INX     H               ;we want to increment to next
        XCHG                    ;but first...check overflow
        LHLD    SPT             ;  by doing (SECPERTRK-SECTOR)
        CALL    SUBDE           ;do the subtraction
        XCHG
        JNC     NOOVF           ;jump if not SECTOR > SECPERTRK
;
;
; Sector overflow...bump track number, reset sector
;
        LHLD    TRACK
        INX     H
        SHLD    TRACK
        LDA     ASTRS           ;Check if column length set
        ORA     A
        JNZ     HDCOPY          ;non-zero - do *'s
        SHLD    DECWRK          ;DECOUT destroys
        LXI     D,TRKMSG        ;
        CALL    PSTRNG          ;
        LXI     H,DECWRK        ;
        CALL    DECOUT          ;
        JMP     NOCRLF          ;

HDCOPY: MVI     A,'*'           ;tell console another track done
        CALL    TYPE
        LDA     COLUMN          ;check column
        ORA     A               ;skip if zero
        JZ      NOCRLF
        DCR     A
        STA     COLUMN
        JNZ     NOCRLF          ;jump if less than 80
        LDA     ASTRS           ;reset column
        STA     COLUMN
        CALL    CRLF

NOCRLF: CALL    STOP            ;see if console wants to quit
        LXI     H,1             ;new sector number on next track
NOOVF:  SHLD    SECTOR          ;put sector away
        POP     PSW             ;get back error flags
        POP     B               ;restore group number
        RET
;.....
;
;
; Convert logical sector # to physical
;
LTOP:   LHLD    SECTBL          ;set up parameters
        XCHG                    ;  for call to SECTRAN
        LHLD    SECTOR
        MOV     B,H
        MOV     C,L
        DCX     B               ;always call SECTRAN  w/zero-rel sec #
;
;
SECT1:  CALL    SECTRN  ;DO THE SECTOR TRANSLATION
        LDA     SPT+1   ;CHECK IF BIG TRACKS
        ORA     A       ;SET FLAGS (TRACKS > 256 SECTORS)
        JNZ     LTOP1   ;NO SO SKIP
        lda     track   ;check for track 0
        mov     b,a
        lda     track+1 ;high order
        ora     b
        jnz     ltop1   ;not track 0
;       MOV     H,A     ;ZERO OUT UPPER 8 BITS
;
LTOP1:  SHLD    PHYSEC  ;PUT AWAY PHYSICAL SECTOR
        RET
;.....
;
;
; Sector translation vector
;
SECTRN: JMP     $-$             ;filled in at init
;
;
;Put bad block in bad block list
;
SETBD:  PUSH    B
        CALL    SETBD1
        DB      CR,LF,'Bad block: $'
;
SETBD1: POP     D       ;RETRIEVE ARG
        MVI     C,9     ;PRINT STRING
        CALL    BDOS
        POP     B       ;GET BACK BLOCK NUMBER
        MOV     A,B
        CALL    HEXO    ;PRINT IN HEX
        MOV     A,C
        CALL    HEXO
        CALL    CRLF

        lxi     h,dm    ;point to exitsing bad blocks
setbd2: mov     a,m     ;get first 8 bits of bad map entry
        inx     h
        cmp     c       ;is new entry already there ?
        jz      setbd4  ;maybe
        lda     dsm+1   ;check size of block entries
        ora     a
        jz      setbd3  ;small blocks
        inx     h       ;skip over high order half
setbd3: push    h
        xchg            ;save hl
        lhld    dmptr
        xchg
        call    subde   ;scan pointer-(dmptr)
        pop     h       ;restore scan pointer
        jc      setbd2  ;continue searching
        jmp     setbd9  ;put it away

setbd4: lda     dsm+1
        ora     a
        rz              ;small blocks = done
        mov     a,m     ;get high order half of existing block
        cmp     b       ;compare with block to be added
        rz              ;really already there
        inx     h       ;point to low order of next block
        jmp     setbd3  ;check if done

setbd9: LHLD    DMCNT   ;GET NUMBER OF SECTORS
        LDA     BLM     ;GET BLOCK SHIFT VALUE
        INR     A       ;MAKES SECTOR/GROUP VALUE
        MOV     E,A     ;WE WANT 16 BITS
        MVI     D,0
        DAD     D       ;BUMP BY NUMBER IN THIS BLOCK
        SHLD    DMCNT   ;UPDATE NUMBER OF SECTORS
        LHLD    BADBKS  ;INCREMENT NUMBER OF BAD BLOCKS
        INX     H
        SHLD    BADBKS
        LHLD    DMPTR   ;GET POINTER INTO DM
        MOV     M,C     ;...AND PUT BAD BLOCK NUMBER
        INX     H       ;BUMP TO NEXT AVAILABLE EXTENT
        LDA     DSM+1   ;CHECK IF 8 OR 16 BIT BLOCK SIZE
        ORA     A
        JZ      SMGRP   ;JUMP IF 8 BIT BLOCKS
        MOV     M,B     ;ELSE STORE HI BYTE OF BLOCK #
        INX     H       ;AND BUMP POINTER
;
SMGRP:  SHLD    DMPTR   ;SAVE DM POINTER, FOR NEXT TIME THROUGH HERE
        RET             ;RETURN FROM "SETBD"
;
;Eliminate any previous [UNUSED].BAD entries
;
ERAB:   lxi     d,bfcb  ;bad FCB
        xra     a
        sta     bfcb+12 ;clear extent
        mvi     c,15    ;open file
        call    bdos    ;try to open file
        cpi     0ffh    ;not found ?
        rz              ;yes, no need to delete it
        sta     dirofs  ;directory offset in DMA buffer
erab0:  lxi     d,erabms
        mvi     c,9
        call    bdos
        mvi     c,1     ;console input
        call    bdos
        cpi     'a'
        jc      erab1   ;already upper case
        ani     05fh    ;force upper case
erab1:  cpi     'Y'
        jz      erab4   ;do it  
        cpi     'N'
        jnz     erab0   ;invalid response
        call    erab2   ;load tables ...
;                       and then erase old file

erab4:  call    crlf
        LXI     D,BFCB  ;POINT TO BAD FCB
        MVI     C,19    ;BDOS DELETE FILE FUNCTION
        CALL    BDOS
        RET

erabms: db      'Erase Existing [UNUSED].BAD file ? (Y/N) $'
;                       flag old bad blocks for this run
;Move bad allocated blocks to DM area 
;
erab2:  LXI     H,DM    ;GET DM
        SHLD    DMPTR   ;SAVE AS NEW POINTER
        LDA     EXM     ;GET THE EXTENT SHIFT FACTOR
        MVI     C,0     ;INIT BIT COUNT
        CALL    COLECT  ;GET SHIFT VALUE
        LXI     H,128   ;STARTING EXTENT SIZE
        MOV     A,C     ;FIRST SEE IF ANY SHIFTS TO DO
        ORA     A
        JZ      erab2b  ;JUMP IF NONE
;
erab2a: DAD     H       ;SHIFT
        DCR     A       ;BUMP
        JNZ     erab2a  ;LOOP
;
erab2b: PUSH    H       ;SAVE THIS, IT IS RECORDS PER EXTENT
        LDA     BSH     ;GET BLOCK SHIFT
        MOV     B,A
;
erab2c: CALL    ROTRHL  ;SHIFT RIGHT
        DCR     B
        JNZ     erab2c  ;TO GET BLOCKS PER EXTENT
        MOV     A,L     ;IT'S IN L (CAN'T BE >16)
        STA     BLKEXT  ;SETDME WILL NEED THIS LATER
        POP     H       ;GET BACK REC/EXT
;
erab2d: XCHG            ;NOW HAVE REC/EXTENT IN DE
        LHLD    DMCNT   ;COUNT OF BAD SECTORS
;
erab2e: PUSH    H
        push    d
        lda     bfcb+15 ;record count
        mov     l,a
        mvi     h,0
        MOV     B,H     ;SAVE Record Count IN BC
        MOV     C,L
        pop     d
        CALL    SUBDE   ;HAVE TO SUBTRACT FIRST
        POP     H       ;THIS POP MAKES IT COMPARE ONLY
        push    psw
        dad     b       ;new count of bad records
        shld    dmcnt   ;save count of bad sectors
        pop     psw
        JC      erab2g  ;JUMP IF LESS THAN 1 EXTENT WORTH
        MOV     A,B
        ORA     C       ;TEST IF SUBTRACT WAS 0
        rz              ;EXTENT IS empty (SPL CASE)
        PUSH    H       ;SAVE TOTAL
        PUSH    D       ;AND SECTORS/EXTENT
        CALL    erab2g  ;get next EXTENT
        POP     D       ;GET BACK SECTORS/EXTENT
        POP     H       ;AND COUNT OF BAD SECTORS
        cpi     0ffh    ;check return from search next
        rz
        JMP     erab2e  ;AND LOOP
;
;Load an extent's worth of bad sectors/block numbers.
;       bc contains the number of records in this extent
;
erab2g: mov     a,b
        ora     c       ;check record count
        mvi     a,0ffh  ;assume zero
        rz              ;no more to process
        push    b       ;save records in this extent
        lda     blm     ;block mask
        inr     a       ;make sectors/block
        cma             ;make negative
        mov     e,a
        mvi     d,0ffh
        inx     d       ;make 2-s complement
        pop     h       ;hl = number of records
        lxi     b,0
;               divide recs in this extent by recs/block giving blocks
erab2i: dad     d
        inx     b       ;bump quotient
        jc      erab2i  ;not done yet
        dcx     b       ;account for overshoot
        lhld    badbks  ;bad block count
        dad     b
        shld    badbks
;
erab2k: LXI     h,bfcb+16       ;DISK ALLOC MAP IN FCB
        xchg
        lhld    dmptr   ;point to bad allocation map
;
erab2l: LDAX    D
        MOV     M,A
        INX     H
        INX     D
;
;Now see if 16 bit groups...if so,
;we have to move another byte
;
        LDA     DSM+1   ;THIS TELLS US
        ORA     A
        JZ      erab2m  ;IF ZERO, THEN NOT
        LDAX    D       ;IS 16 BITS, SO DO ANOTHER
        MOV     M,A
        INX     H
        INX     D
;
erab2m: DCR     c       ;COUNT DOWN
        JNZ     erab2l
        shld    dmptr
        lda     bfcb+12
        inr     a
        sta     bfcb+12 ;next extent number
        lxi     d,bfcb
        mvi     c,15    ;open next extent
        CALL    bdos    
        sta     dirofs
        ret
;
;Create [UNUSED].BAD file entry
;
;
OPENB:  LXI     D,BFCB          ;point to bad FCB
        MVI     C,22            ;BDOS make file function
        CALL    BDOS
        CPI     0FFH            ;check for open error
        RNZ                     ;return from "OPENB", if no error
        JMP     ERROR7          ;bail out...cannot create [UNUSED].BAD
;
CLOSEB: XRA     A
        LDA     BFCB+14         ;get CP/M 2.x 'S2' byte
        ANI     1FH             ;zero update flags
        STA     BFCB+14         ;restore it to our FCB (won't hurt 1.4)
        LXI     D,BFCB          ;FCB for [UNUSED].BAD
        MVI     C,16            ;BDOS close file function
        CALL    BDOS
        RET                     ;return from "CLOSEB"
;.....
;
;
; Move bad area DM to BFCB
;
SETDM:  LXI     H,DM            ;get DM
        SHLD    DMPTR           ;save as new pointer
        LDA     EXM             ;get the extent shift factor
        MVI     C,0             ;init bit count
        CALL    COLECT          ;get shift value
        LXI     H,128           ;starting extent size
        MOV     A,C             ;first see if any shifts to do
        ORA     A
        JZ      NOSHFT          ;jump if none
;
ESHFT:  DAD     H               ;shift
        DCR     A               ;bump
        JNZ     ESHFT           ;loop
;
NOSHFT: PUSH    H               ;save this, it is records per extent
        LDA     BSH             ;get block shift
        MOV     B,A
;
BSHFT:  CALL    ROTRHL          ;shift right
        DCR     B
        JNZ     BSHFT           ;to get blocks per extent
        MOV     A,L             ;it is in 'L' (cannot be 16)
        STA     BLKEXT          ;SETDME will need this later
        POP     H               ;get back rec/ext
;
SET1:   XCHG                    ;now have rec/extent in 'DE'
        LHLD    DMCNT           ;count of bad sectors
;
SETDMO: PUSH    H               ;set flags on (DMCNT-BADCNT)
        CALL    SUBDE           ;have to subtract first
        MOV     B,H             ;save result in 'BC'
        MOV     C,L
        POP     H               ;this POP makes it compare only
        JC      SETDME          ;jump if less than 1 extent worth
        MOV     A,B
        ORA     C               ;test if subtract was 0
        JZ      EVENEX          ;extent is exactly filled (special case)
        MOV     H,B             ;restore result to 'HL'
        MOV     L,C
        PUSH    H               ;save total
        PUSH    D               ;and sectors/extent
        XCHG
        CALL    SETDME          ;put away one extent
        XCHG
        SHLD    DMPTR           ;put back new DM pointer
        POP     D               ;get back sectors/extent
        POP     H               ;and count of bad sectors
        JMP     SETDMO          ;and loop
;
;
; Handle the special case of a file that ends on an extent boundary.
; CP/M requires that such a file have a succeeding empty extent in order
; for the BDOS to properly access the file.
;
EVENEX: XCHG                    ;first set extent w/bad blocks
        CALL    SETDME
        XCHG
        SHLD    DMPTR
        LXI     H,0             ;now set one with no data blocks
;
;
; Fill in an extent's worth of bad sectors/block numbers.  Also fill in
; the extent number in the FCB.
;
SETDME: PUSH    H               ;save record count
        LDA     EXTNUM          ;update extent byte
        INR     A
        STA     EXTNUM          ;save for later
        STA     BFCB+12         ; and put in FCB
        CALL    OPENB           ;open this extent
        POP     H               ;retrieve rec count
;
;
; Divide record count by 128 to get the number of logical extents to put
; in the EX field
;
        MVI     B,0             ;init quotient
        LXI     D,-128          ;-divisor
        MOV     A,H             ;test for special case
        ORA     L               ;  of no records
        JZ      SKIP
;
DIVLOP: DAD     D               ;subtract
        INR     B               ;bump quotient
        JC      DIVLOP
        LXI     D,128           ;fix up overshoot
        DAD     D
        DCR     B
        MOV     A,H             ;test for wraparound
        ORA     L
        JNZ     SKIP
        MVI     L,80H           ;record length
        DCR     B
;
SKIP:   LDA     EXTNUM          ;now fix up extent num
        ADD     B
        STA     EXTNUM
        STA     BFCB+12
        MOV     A,L             ;mod is record count
        STA     BFCB+15         ;that goes in receive byte
;
MOVDM:  LDA     BLKEXT          ;get blocks per extent
        MOV     B,A             ;into 'B'
;
SETD1:  LHLD    DMPTR           ;point to bad allocation map
        XCHG
        LXI     H,BFCB+16       ;disk alloc map in FCB
;
SETDML: LDAX    D
        MOV     M,A
        INX     H
        INX     D
;
;
; Now see if 16 bit groups...if so, we have to move another byte
;
        LDA     DSM+1           ;this tells us
        ORA     A
        JZ      BUMP1           ;if zero, then not
        LDAX    D               ;is 16 bits, so do another
        MOV     M,A
        INX     H
        INX     D
;
BUMP1:  DCR     B               ;count down
        JNZ     SETDML
        PUSH    D
        CALL    CLOSEB          ;close this extent
        POP     D
        RET
;.....
;
;
; Error messages
;
SELERR: LXI     D,SELEMS        ;say no go, and bail out
        JMP     PMSG
;
SELEMS: DB      CR,LF,'Drive specifier out of range$'
;
ERMSG5: DB      CR,LF,BEEP,BEEP,'+++ Warning...System tracks'
        DB      ' bad +++',CR,LF,CR,LF,'$'
;
ERROR6: LXI     D,ERMSG6        ;oops...clobbered directory
        JMP     PMSG
;
ERMSG6: DB      CR,LF,BEEP,BEEP,'Bad directory area, try reformatting$'
;
ERROR7: LXI     D,ERMSG7        ;say no go, and bail out
        JMP     PMSG
;
ERMSG7: DB      CR,LF,BEEP,BEEP,'Can''t create [UNUSED].BAD$'
;
BEEPS:  DB      BEEP,BEEP,BEEP,'$'
;
;=======================================================================
;
;                            SUBROUTINES
;
;=======================================================================
;
; Decimal output routine
;
; EXPLANATION:
;
;       If the disk size is at or near the maximum of 65,536 sectors,
;       the sector count addition overflows 16 bits.  The counter has
;       been increased to 24 bits. This routine destroys the counter-
;       value it is called with.
;
DECOUT: PUSH    B
        PUSH    D
        LXI     B,-10
        LXI     D,-1
;
DECOU2: CALL    ADD24
        INX     D
        JC      DECOU2
        LXI     B,10
        CALL    ADD24
        MOV     A,D
        ORA     E
        PUSH    H
        MOV     A,M
        MOV     M,E
        MOV     E,A
        INX     H
        MOV     A,M
        MOV     M,D
        MOV     D,A
        INX     H
        MVI     M,0
        POP     H
        CNZ     DECOUT          ;recursive call..
        MOV     A,E
        ADI     '0'
        CALL    TYPE
        POP     D
        POP     B
        RET
;..
;
;
ADD24:  PUSH    H
        ORA     A               ;clear carry
        MOV     A,M             ;do 24-bit add
        ADC     C
        MOV     M,A
        INX     H
        MOV     A,M
        ADC     B
        MOV     M,A
        INX     H
        MOV     A,M
        ADC     B
        MOV     M,A
        POP     H
        RET
;.....
;
;
; CR/LF to the console
;
CRLF:   MVI     A,CR
        CALL    TYPE
        MVI     A,LF            ;fall into 'TYPE'
;
TYPE:   PUSH    B
        PUSH    D
        PUSH    H
        MOV     E,A             ;character to 'E' for CP/M
        MVI     C,2             ;print console function
        CALL    BDOS            ;print character
        POP     H
        POP     D
        POP     B
        RET
;.....
;
;
; Subroutine to test console for CTL-C abort
;
STOP:   LHLD    BASE+1          ;find BIOS in memory
        MVI     L,6             ;offset to console status
        CALL    GOHL
        ORA     A               ;test flags on zero
        RZ                      ;return if no character
        LHLD    1               ;now find console input
        MVI     L,9             ;offset for CONIN
        CALL    GOHL
        CPI     'C'-40H         ;is it CTL-C?
        RNZ                     ;return if not
        LXI     D,ABORTM        ;exit with message
        CALL    PSTRNG
        JMP     BASE+0          ;then leave
;.....
;
;
ABORTM: DB      CR,LF,'Test aborted by control-C',CR,LF,'$'
;
;
; A thing to allow a call to @HL
;
GOHL:   PCHL
;
;
; Zero all of memory to hold DM values
;
ZMEM:   LHLD    BDOS+1          ;get top-of-memory pointer
        LXI     D,DM            ;starting point
        CALL    SUBDE           ;get number of bytes
        MOV     B,H
        MOV     C,L
        XCHG                    ;begin in 'HL', count in 'BC'
;
ZLOOP:  MVI     M,0             ;zero a byte
        INX     H               ;point past
        DCX     B               ;count down
        MOV     A,B
        ORA     C
        JNZ     ZLOOP
        RET
;.....
;
;
; Subtract DE from HL
;
SUBDE:  MOV     A,L
        SUB     E
        MOV     L,A
        MOV     A,H
        SBB     D
        MOV     H,A
        RET
;.....
;
;
; Negate HL
;
NEG:    MOV     A,L
        CMA
        MOV     L,A
        MOV     A,H
        CMA
        MOV     H,A
        INX     H
        RET
;.....
;
;
; Move from (HL) to (DE), with count in (BC)
;
MOVE:   MOV     A,M
        STAX    D
        INX     H
        INX     D
        DCR     B
        JNZ     MOVE
        RET
;.....
;
;
; Print byte in accumulator in hex
;
HEXO:   PUSH    PSW             ;save for second half
        RRC                     ;move into position
        RRC
        RRC
        RRC
        CALL    NYBBLE          ;print most significant nybble
        POP     PSW
;
NYBBLE: ANI     0FH             ;low nybble only
        ADI     90H
        DAA
        ACI     40H
        DAA
        JMP     TYPE            ;print in hex
;.....
;
;
; Subroutine to determine the number of groups reserved for the directory
;
GETDIR: MVI     C,0             ;init bit count
        LDA     AL0             ;read dir grp bits
        CALL    COLECT          ;collect count of dir grps..
        LDA     AL1             ;..in 'C'
        CALL    COLECT
        MOV     L,C
        MVI     H,0             ;'BC' now has a default start grp #
        SHLD    DIRBKS          ;save for later
        RET
;.....
;
;
; Collect the number of '1' bits in 'A' as a count in 'C'
;
COLECT: MVI     B,8
;
COLOP:  RAL
        JNC     COSKIP
        INR     C
;
COSKIP: DCR     B
        JNZ     COLOP
        RET
;.....
;
;
; Shift HL right one place
;
ROTRHL: ORA     A               ;clear carry
        MOV     A,H             ;get hihg byte
        RAR                     ;shift right
        MOV     H,A             ;put back
        MOV     A,L             ;get low byte
        RAR                     ;shift with carry
        MOV     L,A             ;put back
        RET
;.....
;
;
; Total sector count routine (24-bit)
;
INCSEC: LXI     H,SECCNT        ;point to sector count
        MVI     A,1             ;add 1
        ORA     A               ;clear carry
        ADC     M               ;add current total
        MOV     M,A             ;  and save 1st byte
        RNC                     ;return if no carry
        INX     H               ;incr pointer
        MVI     A,0             ;reset a
        ADC     M               ;add 2nd byte
        MOV     M,A             ; and save
        RNC                     ;return if no carry
        INX     H               ;increment pointer
        MVI     A,0             ;and again for 3rd byte
        ADC     M
        MOV     M,A
        RET
;.....
;
;
; Routine to fill in disk parameters
;
LOGIT:  LDA     VER2FL
        ORA     A               ;if not CP/M 2.x then
        JZ      LOG14           ;  do it as 1.4
        LXI     D,DPB           ;  then move to local
        MVI     B,DPBLEN        ;  workspace
        CALL    MOVE
        RET
;...
;
;
LOG14:  LHLD    BDOS+1          ;first find 1.4 BDOS
        MVI     L,0
        LXI     D,DPBOFF        ;then offset to 1.4's DPB
        DAD     D
        MVI     D,0             ;so 8 bit parms will be 16
        MOV     E,M             ;now move parms
        INX     H               ;  down from BDOS disk parm block
        XCHG                    ;  to ours
        SHLD    SPT
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
        RET
;.....
SYSMSG: DB      ' system sectors read',CR,LF,'$'
SECMSG: DB      ' directory/data sectors read',CR,LF,'$'
TRKMSG: DB      CR,'Track   $'
NOMSG:  DB      'No$'
ENDMSG: DB      ' bad blocks found',CR,LF,'$'
dirofs: db      0       ;offset of directory entry in DMA buffer
;
BFCB:   DB      0,'[UNUSED]BAD',0,0,0,0
FCBDM:  DS      17
;
; The disk parameter block is moved here from CP/M
;
DPB:    EQU     $               ;disk parameter block (copy)
SPT:    DS      2               ;sectors per track
BSH:    DS      1               ;block shift
BLM:    DS      1               ;block mask
EXM:    DS      1               ;extent mask
DSM:    DS      2               ;maximum block number
DRM:    DS      2               ;maximum directory block number
AL0:    DS      1               ;directory allocation vector
AL1:    DS      1               ;directory allocation vector
CKS:    DS      2               ;checked directory entries
SYSTRK: DS      2               ;system tracks
;
DPBLEN: EQU     $-DPB           ;length of disk parameter block
;
        DS      80              ;room for 40 level stack
NEWSTK  EQU     $               ;our stack
;
;
BLKEXT: DB      0               ;blocks per extent
DIRBKS: DW      0               ;calculated # of dir blocks
VER2FL: DB      0               ;version 2.x flag
;
BADBKS: DB      0,0,0           ;count of bad blocks
SECTOR: DW      0               ;current sector number
TRACK:  DB      0,0             ;current track number
PHYSEC: DW      0               ;current physical sector number
SECTBL: DW      0               ;sector skew table pointer
;
EXTNUM: DB      0FFH            ;used for updating extent number
DMCNT:  DW      0               ;number of bad sectors
DMPTR:  DW      DM              ;pointer to next block identification
;
SECCNT: DB      0,0,0           ;number of sectors read
COLUMN: DB      0               ;crt column counter
DECWRK: DB      0,0,0           ;TRACK-storage for DECOUT
;
DM      EQU     $               ;Bad block allocation map
;
        END
 	I.~/