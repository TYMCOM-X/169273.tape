;JOB%52(551) TO MAKE MACRO-10 VERSION 52
;SUBMIT WITH COMMAND	.SUBMIT MACRO/TIME::15:/OUTPUT:0
;
;THIS CONTROL FILE IS PROVIDED FOR INFORMATION PURPOSES ONLY.  THE
;PURPOSE OF THE FILE IS TO DOCUMENT THE PROCEDURES USED TO BUILD
;THE DISTRIBUTED SOFTWARE.  IT IS UNLIKELY TO BE ABLE TO BE EXECUTEED
;WITHOUT MODIFICATION ON OTHER SYSTEMS.  IN PARTICULAR, ATTENTION
;SHOULD BE GIVEN TO ERSATZ DEVICES AND STRUCTURE NAMES, PPN'S AND
;OTHER SUCH SYSTEM PARAMETERS.  SUBMIT TIMES MAY VARY DEPENDING ON
;CONFIGURATION AND LOAD.  THE AVAILABILITY OF SUFFICIENT DISK SPACE
;AND CORE IS MANDATORY.  THIS CONTROL FILE HAS NOT BEEN EXTENSIVELY
;TESTED ON ALTERNATE CONFIGURATIONS.  IT HAS BEEN USED SUCCESSFULLY
;FOR THE PURPOSE FOR WHICH IT IS INTENDED: TO BUILD THE DISTRIBUTED
;SOFTWARE.
;
;THIS VERSION OF THE CONTROL FILE EXPECTS:
;MACRO.MAC, MCR52.RND, MACRO.HLP ON SELF:
;ALL OTHER FILES ON [10,7]
;IT FIRST USES VERSION 50A (ON [10,7]) TO CREATE VERSION 52
;THEN THAT VERSION TO CREATE ITSELF AGAIN
;BOTH VERSIONS ARE THEN COMPARED.
;
;REQUIRED FILES:	(LATEST RELEASED VERSIONS)
;[10,7]	MACRO.EXE
;	HELPER.REL
;	LINK.EXE
;	LNK???.EXE
;	FILCOM.EXE
;	DIRECT.EXE
;	COMPIL.EXE
;	PIP.EXE
;	CREF.EXE
;	RUNOFF.EXE
;[SELF]	MACRO.MAC
;	MACRO.CTL
;	MCR50A.RND
;	MACRO.HLP
;
;OUTPUT FILES:
;	MACRO.EXE
;	MACRO.REL
;OUTPUT LISTINGS:
;	MACRO.MAP
;	MACRO.DOC
;	MACRO.LST
;	MACRO.LOG
.R SETSRC
*C DSKC:
;
;
;MAKE A RECORD OF WHAT IS BEING USED
.SET WATCH VERSION
.IF (ERROR)
.ASSIGN DEC: SYS:
;
.DIRECT/CHECKSUM MACRO.MAC,MCR52.RND,MACRO.HLP,DEC:HELPER.REL
.IF (ERROR) ;SOME FILES ARE OPTIONAL
;
;COMPILE, LOAD AND SAVE MACRO, USING A TEMPORARY NAME
.LOAD MACRO.MAC/COMPILE/MACRO
.SSAVE XMACRO.EXE
;
;NOW USE THE CREATED VERSION OF MACRO TO COMPILE ITSELF
;
;COMPILE MACRO-10 SOURCE PRODUCING BINARY AND CREF FILES
.RUN XMACRO
*MACRO.REL,MACRO.CRF/C=MACRO.MAC
.IF (NOERROR) .GOTO SAVE
;
;IN CASE CREF CAUSED QUOTA EXCEEDED
.DELETE MACRO.CRF
;COMPILE MACRO-10 SOURCE PRODUCING BINARY FILE
.RUN XMACRO
*MACRO.REL,=MACRO.MAC
;
SAVE::
.LOAD/MAP MACRO.REL/REL
.SSAVE MACRO.EXE
.GET MACRO
.VERSION
.IF (ERROR) .E 137
;
;NOW COMPARE THE TWO FILES FOR ANY DIFFERENCES
;
; CORRESPONDING SHR FILES ARE GENERATED FIRST
; SINCE THIS VERSION OF FILCOM DOES NOT ALLOW COMPARISON OF EXE
; FILES
;
.GET MACRO.EXE
.OSSAVE MACRO.SHR
.GET XMACRO.EXE
.OSSAVE XMACRO.SHR
.R FILCOM
*TTY:=MACRO.SHR,XMACRO.SHR/400020L/Q
.IF (ERROR) .PLEASE ASSEMBLY OF MACRO USING MACRO FAILED^[^[
;
;PRODUCE MACRO-10  .DOC AND .LST FILES
;
.R RUNOFF
*DSK:MCR52.DOC=MCR52.RND
.IF (ERROR) ;
;
.R CREF
*DSK:MACRO.LST=MACRO.CRF
.IF (ERROR) ;WILL FAIL IF CREFFING EXCEEDED QUOTA
;
.DELETE XMACRO.*
.IF (ERROR) ;
;
;FINAL CHECKSUM DIRECTORY OF PERMANENT OUTPUT
.DIRECT/CHECKSUM MACRO.EXE,MACRO.REL,MACRO.MAP,MACRO.LST
.IF (ERROR) ;
.DIRECT/CHECKSUM MACRO.HLP,MACRO.DOC
.IF (ERROR) ;CERTAIN FILES MAY NOT EXIST
;
.PLEASE MACRO ASSEMBLY AND PRODUCTION SUCCESSFUL^[^[
.DELETE MACRO.SHR,MACRO.REL
;[END OF MACRO.CTL]
 