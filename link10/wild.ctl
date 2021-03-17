;JOB%1(20) TO MAKE WLD7A.REL FROM WLD7A.MAC
;SUBMIT WITH COMMAND  .QUEUE I:=WLD7A/RESTART:1
;
;THIS CONTROL FILE IS PROVIDED FOR INFORMATION PURPOSES ONLY.  THE
;PURPOSE OF THE FILE IS TO DOCUMENT THE PROCEDURES USED TO BUILD
;THE DISTRIBUTED SOFTWARE.  IT IS UNLIKELY TO BE ABLE TO BE EXECUTED
;WITHOUT MODIFICATION ON OTHER SYSTEMS.  IN PARTICULAR, ATTENTION
;SHOULD BE GIVEN TO ERSATZ DEVICES AND STRUCTURE NAMES, PPN'S AND
;OTHER SUCH SYSTEM PARAMETERS.  SUBMIT TIMES MAY VARY DEPENDING ON
;CONFIGURATION AND LOAD.  THE AVAILABILITY OF SUFFICIENT DISK SPACE
;AND CORE IS MANDATORY.  THIS CONTROL FILE HAS NOT BEEN EXTENSIVELY
;TESTED ON ALTERNATE CONFIGURATIONS.  IT HAS BEEN USED SUCCESSFULLY
;FOR THE PURPOSE FOR WHICH IT IS INTENDED: TO BUILD THE DISTRIBUTED
;SOFTWARE.
;
;REQUIRED FILES:  (LATEST RELEASED VERSIONS)
;[10,7]	PIP.SHR
;	DIRECT.SHR
;	COMPIL.SHR
;	MACRO.SHR
;	CREF.SHR
;	C.MAC
;	SCNMAC.MAC
;
;[SELF]	WLD7A.CTL
;	WLD7A.MAC
;	WLD7A.RND
;
;OUTPUT FILES:
;	WLD7A.REL
;	WLD7A.LOG
;	WLD7A.RND
;OUTPUT LISTINGS:
;	WLD7A  CREF LISTING
;
;
;COPY FILES FROM [10,7] AND USE PRIVATE "SYS:"
.RUN DSK:PIP[10,7]
*/X_DSK:PIP.SHR[10,7],COMPIL.SHR[10,7],MACRO.SHR[10,7]
*/X_DSK:CREF.LOW[10,7],CREF.SHR[10,7],C.MAC[10,7],SCNMAC.MAC[10,7]
;
;MAKE A RECORD OF WHAT IS BEING USED
.SET WATCH VERSION
.IF (NOERROR) .GOTO A
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=*.SHR
.GOTO A
A:.RUN DSK: DIRECT[10,7]
*TTY:/CHECKSUM=WLD7A.MAC+C.MAC+SCNMAC.MAC+WLD???.RND+WLD7A.CTL
.ASSIGN DSK: SYS:
;
;MAKE U.MAC TEMP FILE TO FORCE UNIVERSALS OF C AND SCNMAC
.COPY U.MAC=TTY:
*%.C==-3

;COMPILE, PRODUCING CREF FILE
.COMPILE /CREF /COMPILE U.MAC+C.MAC,U.MAC+SCNMAC.MAC,WLD7A.MAC
.RUN DSK:DIRECT[10,7]
*TTY:/CHECKSUM=WLD7A.REL
;
;PRODUCE SOURCE LISTING AND TELL OPERATOR
.ASSIGN DSK LPT
.CREF
.RUN RUNOFF[10,7]
*WLD7A.DOC=WLD7A.RND
.DEAS
.PLEASE WLD7A SUCCESSFUL
;
;REMOVE ALL TEMPORARY FILES
%FIN: .DELETE MACRO.SHR,CREF.LOW,CREF.SHR,COMPIL.SHR,PIP.SHR,C.MAC,SCNMAC.MAC
.DELETE U.MAC,C.LST,C.UNV,SCNMAC.REL,SCNMAC.UNV,SCNMAC.LST
.PRI WLD7A.LOG,/DISP:RENAME WLD7A.LST,/COP:3 WLD7A.DOC
.K/F
    