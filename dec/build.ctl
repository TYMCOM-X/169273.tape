;*******************************************************************************
;BUILD.CTL - Master control file to build cusps
;
;VERSION 1(15)
;EDIT 1 - BEGUN MAY 1979 BY R/E.
;[2] CHANGE NETWOR TO LOAD SCAN AND HELPER
;[3] REMOVE /CREF FROM SYSERR SINCE WITH LISTING FILES IT WOULD REQUIRE TOO MUCH DISK SPACE
;[4] ADD KDPLDR SECTION
;[5] UPDATE BOOTS SECTION
;[6] CHANGE AVAIL TO USE /R FOR SELF CONTAINED PROGRAM
;[7] BRING INITIA AND OPSER SECTIONS UPTODATE
;[10] USE LINK AND /SSAVE TO GET REPRODUCIBLE CHECKSUMS
;[11] FIX UP THE BOOTM SECTION TO BUILD ALL NECESSARY VERSIONS
;[12] ADD SECTION TO BUILD BOOT
;[13] TAKE BOOT OUT; NOW ON MONITOR TAPE
;[14] TAKE BOOTS OUT; NOW ON UNSUPPORTED TAPE
;     ALSO FIX UP A FEW OTHER FILES
;[15] Add build procedure for DDT11.
;
;-----
;Running BUILD:
;
;Submit BUILD with the following command:
;	.SUBMIT BUILD[,]/TAG:cusp,cusp
;where "cusp" is replaced by the cusp name, such as SETSRC or BACKUP.
;
;-----
;Requirements:
;1)	The sources and associated files that make up a cusp are located
;	in an SFD dedicated to that cusp.
;2)	Programs required to build the cusp(s) reside in the UFD. These
;	are Macro, Link, Cref, etc.
;
;Note:	This control file will also work if SFDs are not used. However,
;	the directory may contain other files not associated with the
;	particular cusp being built.
;
;-----
;If this control file is not started at a specific tag, a checksummed directory
;of the files needed to build all cusps will be taken.
;*******************************************************************************
;
.SET WATCH VERSION
.NOERROR
.DIRECT/CHECKS MACRO.EXE,LINK.EXE,LNK???.EXE,CREF.EXE,COMPIL.EXE,DIRECT.EXE
;
.GOTO EXIT
ACTDAE::
;*******************************************************************************
;                               ACTDAE
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	UUOSYM.UNV
;	GLXMAC.UNV
;	ORNMAC.UNV
;	QSRMAC.UNV
;	ACTSYM.UNV
;	GLXINI.REL
;	ACTDAE.MAC
;	ACTRCD.MAC
;Output files:
;	ACTDAE.DOC
;	ACTDAE.EXE
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPILE /CREF /COMP ACTRCD,ACTDAE
.IF (ERROR) .GOTO DIR
;
.R LINK
*ACTDAE/SAVE=ACTDAE/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
ACTSYM::
;*******************************************************************************
;				ACTSYM
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	ACTSYM.MAC
;
;Output files:
;	ACTSYM.DOC
;	ACTSYM.UNV
;	ACTSYM.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMP /CREF /COMPIL ACTSYM.MAC
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
BACKUP::
;*******************************************************************************
;				BACKUP
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;	WILD.REL
;Required files:
;	BACKRS.MAC
;	BACKUP.MAC
;	ENDECR.MAC
;	USGSUB.MAC
;
;Output files:
;	BACKUP.DOC
;	BACKUP.EXE
;	BACKUP.HLP
;	BACKUP.LST
;	ENDECR.REL
;	USGSUB.REL
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP ENDECR.MAC
.IF (ERROR)  .GOTO DIR
.COMPIL /CREF /COMP USGSUB.MAC
.IF (ERROR)  .GOTO DIR
.COMPIL /CREF /COMP BACKUP.MAC,BACKRS.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*BACKUP/SSAVE=BACKUP,BACKRS/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
BOOT11::
;*******************************************************************************
;				BOOT11
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	BOOT11.MAC
;
;Output files:
;	BOOT11.DOC
;	BOOT11.EXE
;	BOOT11.HLP
;	BOOT11.MEM
;	BOOT11.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP BOOT11.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*BOOT11/SSAVE=BOOT11/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
BOOTDX::
;*******************************************************************************
;				BOOTDX
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	BOOTDX.HLP
;	BOOTDX.MAC
;	DXMCA.ADX
;	DXMPA.A8
;
;Output files:
;	BOOTDX.EXE
;	BOOTDX.DOC
;	BOOTDX.MEM
;	BOOTDX.LST
;	A8DDT.MEM
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP BOOTDX.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*BOOTDX/SAVE=BOOTDX/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
BOOTM::
;*******************************************************************************
;				BOOTM
;*******************************************************************************
;
; THIS SECTION CREATES A BOOTM FOR THE KI, KS, AND KL PROCESSORS
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	FILEX.EXE
;	RSXT10.EXE
;	CONVRT.EXE
;Required files:
;	BOOTM.MAC
;	DXLD.MAC
;	DXMCA.ADX
;	DXMPA.A8
;
;Output files:
;	BOOTMI.RDI	FOR TU70 READ-IN TAPE ON KI10
;	BOOTML.EXB	FOR RSX20F LOADING ON THE KL10
;	BOOTMS.EXE	FOR INPUT TO SMFILE TO PRODUCE KS10 VERSION
;	BOOTM.DOC
;	BOOTM.LST
;	DXD2.DOC
;	DXLD.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP DXLD
.COMPIL /CREF /COMP TTY:+DSK:BOOTM.MAC
*MAGRIM==0	;NOT IN MAGRIM FORMAT
=
=
;
;WE NOW HAVE BOOTM.REL FOR USE WITH DXLD AND FOR LOADING ALONE TO
;PRODUCE THE KS10 BOOTM.
;
.R LINK
*DXLD/SAVE=DXLD.REL,BOOTM.REL/NOSTART/GO
.IF (ERROR) .GOTO DIR
.GET DSK:DXLD
.VERSION
.IF (ERROR) .GOTO DIR
;
;NOW MAKE BOOTMI.RDI FOR USE WITH TU70 READ-IN
.ASSIGN DSK:OUT
.RUN DXLD
.SAVE BOOTM
.COPY BOOTMI.RDI=BOOTM.RDI
;
;NOW CONVERT THE .EXE FILE TO A .SAVE FILE TO FEED INTO RSXT10
;
.R FILEX
*BOOTM.SAV=BOOTM.EXE
.IF (ERROR) .GOTO DIR
;
;NOW CONVERT THE .SAV FILE TO AN .EXB FILE FOR LOADING BY RSX20F
;
.R RSXT10
*CONVERT BOOTM.SAV BOOTML.EXB
.IF (ERROR) .GOTO DIR
;
;NOW LOAD BOOTM ALONE TO PRODUCE INPUT TO SMFILE FOR THE KS10
;
.R LINK
*BOOTMS/SAVE=BOOTM/GO
.IF (ERROR) .GOTO DIR
;
;FINALLY, CREATE BOOTMS.RDI WITH SMFILE
;WE CAN'T DO THIS UNDER BATCH BECAUSE SMFILE DOESN'T GO INTO INPUT
;WAIT SO BATCON CAN'T TELL IT WANTS ANOTHER LINE.
;
;.RUN SMFILE
;*OUTPUT MTBOOT BOOTMS.EXE BOOTMS.RDI
;*EXIT
;.IF (ERROR) .GOTO DIR
;
;NOW MAKE THE PAPER TAPE OF BOOTM IF NEEDED
;
;.COMPIL /CREF /COMP TTY:+DSK:BOOTM.MAC
;*MAGRIM==0	;NOT IN MAGRIM FORMAT
;=
;=
;.R MACRO
;*PTP:BOOTM=FTBTM,BOOTM
;THIS OBJECT PROGRAM BEING AN EXEC MODE BOOTSTRAP, THERE IS
; NO WAY TO TEST IT UNDER BATCH, SO WE LET THAT PASS.
;
.GOTO COMMON
COMPIL::
;*******************************************************************************
;				COMPIL
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	COMPIL.MAC
;
;Output files:
;	COMPIL.DOC
;	COMPIL.EXE
;	COMPIL.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP COMPIL.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*COMPIL/SSAVE=COMPIL/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
CONFIG::
;*******************************************************************************
;				CONFIG
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	CONFIG.MAC
;
;Output files:
;	CONFIG.DOC
;	CONFIG.EXE
;	CONFIG.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP CONFIG.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*CONFIG/SSAVE=CONFIG/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
CONV10::
;*******************************************************************************
;				CONV10
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	ORNMAC.UNV
;	GLXMAC.MAC
;	UUOSYM.UNV
;	GLXINI.REL
;	OPRPAR.REL
;	CONV10.MAC
;
;Output files:
;	CONV10.DOC
;	CONV10.EXE
;	CONV10.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMP /CREF /COMP CONV10.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*CONV10/SAVE=CONV10/GO
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
CREF::
;*******************************************************************************
;				CREF
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	CREF.MAC
;
;Output files:
;	CREF.DOC
;	CREF.EXE
;	CREF.HLP
;	CREF.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP CREF.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*CREF/SSAVE=CREF,REL:HELPER/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
CRSCPY::
;*******************************************************************************
;				CRSCPY
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN
;Required files:
;	CRSCPY.MAC
;
;Output files:
;	CRSCPY.DOC
;	CRSCPY.EXE
;	CRSCPY.HLP
;	CRSCPY.MEM
;	CRSCPY.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP CRSCPY.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*CRSCPY/SSAVE=CRSCPY,REL:SCAN,REL:HELPER/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
DAEMON::
;*******************************************************************************
;				DAEMON
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	DAEMON.MAC
;
;Output files:
;	DAEMON.DOC
;	DAEMON.EXE
;	DAEMON.LST
;
;
.COMPIL /CREF /COMP DAEMON.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*DAEMON/SSAVE=DAEMON/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
DDT::
;*******************************************************************************
;				DDT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	JOBDAT.REL
;	MACTEN.REL
;	UUOSYM.REL
;	JOBDAT.UNV
;	MACTEN.UNV
;	UUOSYM.UNV
;Required files:
;	DDT.MAC
;	F1EDDT.MAC
;	F1FDDT.MAC
;	F1UDDT.MAC
;	F1VDDT.MAC
;
;Output files:
;	DDT41A.DOC
;	DDT41A.MEM
;	DDT.EXE		(EXECUTABLE DDT WITH SYMBOLS)
;	DDT.LST
;	DDT.REL		(USER MODE DDT)
;	EDDT.REL	(EXEC MODE DDT)
;	FILDDT.REL	(FILE DDT)
;	VMDDT.REL	(USER MODE VIRTUAL MEMORY DDT)
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
;Make DDT.REL
.COMPILE /CREF /COMP DDT.REL=F1UDDT.MAC+DDT.MAC
.IF (ERROR)  .GOTO DIR
;
;Make EDDT.REL
.COMPILE /CREF /COMP EDDT.REL=F1EDDT.MAC+DDT.MAC
.IF (ERROR)  .GOTO DIR
;
;Make FILDDT.REL
.COMPILE /CREF /COMP FILDDT.REL=F1FDDT.MAC+DDT.MAC
.IF (ERROR)  .GOTO DIR
;
;Make VMDDT.REL
.COMPILE /CREF /COMP VMDDT.REL=F1VDDT.MAC+DDT.MAC
.IF (ERROR)  .GOTO DIR
;
;HERE TO MAKE THE NEW .EXE FILES REPLETE WITH SYMBOLS
LOAD::
.CHKPNT LOAD
.ERROR ?
;
;Make DDT.EXE
.R LINK
*/NOINITIAL/LOCALS/SYMSEG:LOW/PATCHS:2K DDT.REL/NOLOCALS, -
*REL:JOBDAT.REL, REL:MACTEN.REL, REL:UUOSYM.REL/GO
.IF (ERROR)  .GOTO DIR
.DDT
=HRLZ 1,.JBSYM^[X
=HRR 1,.JBSYM^[X
=MOVE 2,.JBFF^[X
=SUB 2,.JBREL^[X
=ADDI 2,1777^[X
=SUBI 1,(2)^[X
=MOVE 2,.JBREL^[X
=SUBI 2,2000^[X
=HRRM 1,.JBUSY^[X
=HRRM 1,.JBSYM^[X
=BLT 1,(2)^[X
=MOVSI 3,1(2)^[X
=IORI 3,DDT^[X
=HRLM 2,.JBCOR^[X
=CORE 2,^[X
=MOVEM 3,.JBSA^[X
=HLRM 3,.JBFF^[X
=HRRM 3,.JBREN^[X
=.JBVER/
*%%DDT
=PAT../
*D:
=0<17>0^[Z
*^C
.SAVE DDT
;
;Make FILDDT.EXE
.R LINK
*FILDDT/SAVE=FILDDT.REL/GO
.IF (ERROR)  .GOTO DIR
;
;Make VMDDT.EXE
.R LINK
*VMDDT/SAVE=VMDDT.REL/GO
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
DDT11::
;*******************************************************************************
;				DDT11
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	DDTSYM.MAC
;	DDTGP.MAC
;	DDTFIL.MAC
;	DDTSIM.MAC
;	DDT11.MAC
;	MACSYM.MAC
;
;Output files:
;
;	DDT11.EXE
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL/COM MONSYM
.IF (ERROR) .GOTO DIR
.COMPIL/COMP DDTSYM
.IF (ERROR) .GOTO DIR
.COMPIL/COMP DDTGP
.IF (ERROR) .GOTO DIR
.COMPIL/COMP DDTFIL
.IF (ERROR) .GOTO DIR
.COMPIL/COMP DDTSIM
.IF (ERROR) .GOTO DIR
.COMP/COMP DDT11
.IF (ERROR) .GOTO DIR
.R LINK
*DDT11/SAVE
*DDTFIL,DDTGP,DDTSIM,DDT11
*/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
DECLAR::
;*******************************************************************************
;				DECLAR
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;	WILD.REL
;Required files:
;	DECLAR.MAC
;
;Output files:
;	DECLAR.DOC
;	DECLAR.EXE
;	DECLAR.HLP
;	DECLAR.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP DECLAR.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*DECLAR/SSAVE=DECLAR/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
DIRECT::
;*******************************************************************************
;				DIRECT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;	WILD.REL
;Required files:
;	DIRECT.MAC
;
;Output files:
;	DIRECT.DOC
;	DIRECT.EXE
;	DIRECT.HLP
;	DIRECT.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP DIRECT.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*DIRECT/SSAVE=DIRECT/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
DTELDR::
;*******************************************************************************
;				DTELDR
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	DTELDR.MAC
;
;Output files:
;	DTELDR.DOC
;	DTELDR.EXE
;	DTELDR.HLP
;	DTELDR.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP DTELDR.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*DTELDR/SSAVE=DTELDR/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
FILCOM::
;*******************************************************************************
;				FILCOM
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	FILCOM.MAC
;
;Output files:
;	FILCOM.DOC
;	FILCOM.EXE
;	FILCOM.HLP
;	FILCOM.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP FILCOM.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*FILCOM/SSAVE=FILCOM/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
HELP::
;*******************************************************************************
;				HELP
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	HELP.MAC
;
;Output files:
;	HELP.DOC
;	HELP.EXE
;	HELP.HLP
;	HELP.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP HELP.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*HELP/SSAVE=HELP/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
INITIA::
;*******************************************************************************
;				INITIA
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	INITIA.MAC
;
;Output files:
;	INITIA.DOC
;	INITIA.EXE
;	INITIA.HLP
;	INITIA.LST
;	INITIA.MEM
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP INITIA.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*INITIA/SSAVE=INITIA/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
JOBDAT::
;*******************************************************************************
;				JOBDAT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	JOBDAT.MAC
;
;Output files:
;	JOBDAT.DOC
;	JOBDAT.LST
;	JOBDAT.REL
;	JOBDAT.UNV
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPILE /CREF /COMP JOBDAT.MAC
.IF (ERROR)  .GOTO DIR
;
;MAKE TEMP FILE TO FORCE UNIVERSALS
.COPY UNV.MAC=TTY:
	%..UNV==0

;
;COMPILE AGAIN TO GET JOBDAT.UNV
.R MACRO
*JOBDAT=UNV.MAC,JOBDAT.MAC
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
KDPLDR::
;***********************************************************************
;		KDPLDR
;****************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO
;	HELPER.REL
;	SCAN.REL
;Required files:
;	KDPLDR.MAC
;
;Output files:
;	KDPLDR.DOC
;	KDPLDR.EXE
;	KDPLDR.HLP
;	KDPLDR.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPILE /CREF /COMP KDPLDR
.IF (ERROR) .GOTO DIR
.R LINK
*KDPLDR/SAVE=KDPLDR/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
LOGIN::
;*******************************************************************************
;				LOGIN
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;	WILD.REL
;Required files:
;	LOGIN.MAC
;	FACTOR.MAC
;	ACTSYM.UNV
;
;Output files:
;	FACTOR.DOC
;	LOGIN.DOC
;	LOGIN.EXE
;	LOGIN.HLP
;	LOGIN.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP LOGIN.MAC,FACTOR.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*LOGIN/SSAVE=LOGIN/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
LOGOUT::
;*******************************************************************************
;				LOGOUT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;	WILD.REL
;Required files:
;	LOGOUT.MAC
;
;Output files:
;	LOGOUT.DOC
;	LOGOUT.EXE
;	LOGOUT.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP LOGOUT.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*LOGOUT/SSAVE=LOGOUT/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
MACRO::
;*******************************************************************************
;				MACRO
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	MACRO.MAC
;
;Output files:
;	MACRO.DOC
;	MACRO.EXE
;	MACRO.HLP
;	MACRO.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP MACRO.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*MACRO/SSAVE=MACRO/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
MACSYM::
;*******************************************************************************
;				MACSYM
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	MACSYM.MAC
;
;Output files:
;	MACSYM.DOC
;	MACSYM.UNV
;	MACSYM.MEM
;	MACSYM.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.R MACRO
*MACSYM,MACSYM/C=MACSYM
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
MAKLIB::
;*******************************************************************************
;				MAKLIB
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;	WILD.REL
;Required files:
;	MAKLIB.MAC
;
;Output files:
;	MAKLIB.DOC
;	MAKLIB.EXE
;	MAKLIB.HLP
;	MAKLIB.LST
;	MAKLIB.MAN
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP MAKLIB.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*MAKLIB/SSAVE=MAKLIB/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
NETLDR::
;*******************************************************************************
;				NETLDR
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	NETLDR.MAC
;
;Output files:
;	NETLDR.DOC
;	NETLDR.EXE
;	NETLDR.HLP
;	NETLDR.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP NETLDR.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*NETLDR/SSAVE=NETLDR/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
NETWOR::
;*******************************************************************************
;				NETWOR
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;Required files:
;	NETWOR.MAC
;
;Output files:
;	NETWOR.DOC
;	NETWOR.EXE
;	NETWOR.HLP
;	NETWOR.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPILE /CREF/COMP NETWOR
.IF (ERROR)  .GOTO DIR
.R LINK
*NETWOR/SSAVE=NETWOR/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
NOPAG0::
;*******************************************************************************
;				NOPAG0
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	NOPAG0.MAC
;
;Output files:
;	NOPAG0.EXE
;	NOPAG0.LST
;	NPG1.DOC
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP NOPAG0.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*NOPAG0/SSAVE=NOPAG0/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
OMOUNT::
;*******************************************************************************
;				OMOUNT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	OMOUNT.MAC
;
;Output files:
;	OMOUNT.DOC
;	OMOUNT.EXE
;	OMOUNT.HLP
;	OMOUNT.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP OMOUNT.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*OMOUNT/SAVE=OMOUNT/GO
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
OPSER::
;*******************************************************************************
;				OPSER
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	OPSER.MAC
;
;Output files:
;	OPSER.DOC
;	OPSER.EXE
;	OPSER.HLP
;	OPSER.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP OPSER.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*OPSER/SSAVE=OPSER/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
PFH::
;*******************************************************************************
;				PFH
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	PFH.MAC
;
;Output files:
;	PFH.DOC
;	PFH.EXE
;	PFH.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP PFH
.IF (ERROR) .GOTO DIR
.R LINK
*PFH/SAVE=PFH/GO
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
PIP::
;*******************************************************************************
;				PIP
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	PIP.MAC
;
;Output files:
;	PIP.EXE
;	PIP.HLP
;	PIP.LST
;	PIP33C.DOC
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP PIP.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*PIP/SSAVE=PIP/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
PROJCT::
;*******************************************************************************
;				PROJCT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	ACTRCD.REL
;	MACTEN.REL
;	UUOSYM.REL
;Required files:
;	PROJCT.MAC
;
;Output files:
;	PROJCT.DOC
;	PROJCT.EXE
;	PROJCT.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP PROJCT.MAC
.IF (ERROR) .GOTO DIR
;
.LOAD PROJCT.REL
.IF (ERROR) .GOTO DIR
.SAVE PROJCT.EXE
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
QUOLST::
;*******************************************************************************
;				QUOLST
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	QUOLST.MAC
;
;Output files:
;	QLT5.DOC
;	QUOLST.EXE
;	QUOLST.HLP
;	QUOLST.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP QUOLST.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*QUOLST/SSAVE=QUOLST/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
REACT::
;*******************************************************************************
;				REACT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	REACT.MAC
;
;Output files:
;	REACT.DOC
;	REACT.EXE
;	REACT.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP REACT.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*REACT/SSAVE=REACT/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
RSXT10::
;*******************************************************************************
;				RSXT10
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	MACSYM.UNV
;	UUOSYM.UNV
;Required files:
;	RSXT10.MAC
;	RSXCMN.MAC
;	RSXTTL.MAC
;
;Output files:
;	RSXT10.DOC
;	RSXT10.EXE
;	RSXFMT.HLP
;	RSXT10.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPILE /CREF RSXTTL.MAC+RSXT10.MAC+RSXCMN.MAC
.IF (ERROR)  .GOTO DIR
;
.LOAD RSXCMN
.IF (ERROR)  .GOTO DIR
;
.SAVE RSXT10.EXE
.IF (ERROR)  .GOTO DIR
;
.R CREF
*DSK:RSXT10.LST=RSXCMN
.IF (ERROR)  .GOTO DIR
.GOTO DIR
SCDSET::
;*******************************************************************************
;				SCDSET
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	FORT??.REL
;Required files:
;	SCDSET.FOR
;	SCDEXE.MAC
;
;Output files:
;	SCDSET.DOC
;	SCDSET.EXE
;	SCDSET.LST
;	SCDSET.MEM
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP SCDSET.FOR,SCDEXE.MAC
.IF (ERROR)  .GOTO DIR
.LOAD SCDSET,SCDEXE
.IF (ERROR) .GOTO DIR
.SSAVE SCDSET
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
SETSRC::
;*******************************************************************************
;				SETSRC
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	SETSRC.MAC
;
;Output files:
;	SETSRC.DOC
;	SETSRC.EXE
;	SETSRC.HLP
;	SETSRC.LST
;	SETSRC.MEM
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP SETSRC.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*SETSRC/SSAVE=SETSRC/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
SYSINF::
;*******************************************************************************
;				SYSINF
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	C.UNV
;Required files:
;	INFLIB.MAC
;	INFSYM.MAC
;	SYSINF.MAC
;
;Output files:
;	SYSINF.DOC
;	SYSINF.EXE
;	SYSINF.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP INFSYM.MAC,INFLIB.MAC(P,,),SYSINF.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*SYSINF /MAP/SAVE=
/SEARCH SYSINF/REQUIRE:(VERSION,INIMOD,INFEXC,INFO,INFERR,INFDAT)
*/SEARCH INFLIB
*/SEARCH SYSINF /START:INFEXC/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
SYSTAT::
;*******************************************************************************
;				SYSTAT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	SYSTAT.MAC
;
;Output files:
;	SYSTAT.DOC
;	SYSTAT.EXE
;	SYSTAT.HLP
;	SYSTAT.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP SYSTAT
.IF (ERROR)  .GOTO DIR
.R LINK
*SYSTAT/SSAVE=SYSTAT/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
TECO::
;*******************************************************************************
;				TECO
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	TECO.MAC
;	TECO.ERR
;
;Output files:
;	TECO.DOC
;	TECO.EXE
;	TECO.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP TECO.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*TECO/SSAVE=TECO/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
UFDSET::
;******************************************************************************
;				UFDSET
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	JOBDAT.UNV
;Required files:
;	UFDSET.MAC
;
;Output files:
;	UFDSET.DOC
;	UFDSET.REL
;	UFDSET.LST
;	UFDPRM.UNV
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP UFDSET.MAC
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
UMOUNT::
;*******************************************************************************
;				UMOUNT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	UMOUNT.MAC
;
;Output files:
;	UMOUNT.DOC
;	UMOUNT.EXE
;	MOUNT.HLP
;	DISMOU.HLP
;	UMOUNT.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP UMOUNT.MAC
.IF (ERROR)  .GOTO DIR
.R LINK
*UMOUNT/SSAVE=UMOUNT,REL:HELPER/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
UUOSYM::
;*******************************************************************************
;				UUOSYM
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	UUOSYM.MAC
;
;Output files:
;	UUOSYM.DOC
;	UUOSYM.REL
;	UUOSYM.UNV
;	UUOSYM.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMP UUOSYM
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
;*******************************************************************************
;BUILD.CTL - Master control file to build 'Special Catagory A' cusps
;
;-----
;Running BUILD:
;
;Submit BUILD with the following command:
;	.SUBMIT BUILD[,]/TAG:cusp,cusp
;where "cusp" is replaced by the cusp name, such as SETSRC or BACKUP.
;
;-----
;Requirements:
;1)	The sources and associated files that make up a cusp are located
;	in an SFD dedicated to that cusp.
;2)	Programs required to build the cusp(s) reside in the UFD. These
;	are Macro, Link, Cref, etc.
;
;Note:	This control file will also work if SFDs are not used. However,
;	the directory may contain other files not associated with the
;	particular cusp being built.
;
;-----
;If this control file is not started at a specific tag, a checksummed directory
;of the files needed to build all cusps will be taken.
;*******************************************************************************
;
.SET WATCH VERSION
.NOERROR
.DIRECT/CHECKS MACRO.EXE,LINK.EXE,LNK???.EXE,CREF.EXE,COMPIL.EXE,DIRECT.EXE
;
.GOTO EXIT
AVAIL::
;*************************************************************************
;			AVAIL
;*************************************************************************
;
;Required cusps:
;	COMPIL.exe
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	COBOL?.EXE
;	LIBOL.EXE
;Required files:
;	AVAIL.CBL
;	AVLLOD.CCL
;	ERRUNV.MAC
;	REDERR.MAC
;	SYRUNV.MAC
;
;Output files:
;	AVAIL.DOC
;	AVAIL.EXE
;	AVAIL.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.R MACRO
*SYRUNV=SYRUNV
.IF (ERROR) .GOTO DIR
.R MACRO
*ERRUNV=ERRUNV
.IF (ERROR) .GOTO DIR
;
.COMPILE /COMP /CREF REDERR
.IF (ERROR) .GOTO DIR
;
;COMPILE WITH /R TO FORCE LIBOL INTO HIGH SEGMENT
;  YIELDING A SELF CONTAINED PROGRAM
.R COBOL
*AVAIL=AVAIL/O/P/R
.IF (ERROR) .GOTO DIR
.R LINK
*@AVLLOD
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
C::
;*******************************************************************************
;				C
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	MACRO.EXE
;	PIP.EXE
;Required files:
;	CHEAD.MAC
;	MACTEN.MAC
;	UUOSYM.MAC
;
;Output files:
;	C.DOC
;	C.MAC
;	C.UNV
;	C.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.R PIP
*C.MAC=CHEAD.MAC,MACTEN.MAC,UUOSYM.MAC
.IF (ERROR) .GOTO DIR
.COMPIL /CREF /COMPIL TTY:+DSK:C.MAC
*%.C==-3
=
=
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
CREDIR::
;*******************************************************************************
;				CREDIR
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;	WILD.REL
;	C.UNV
;Required files:
;	CREDIR.MAC
;
;Output files:
;	CREDIR.DOC
;	CREDIR.EXE
;	CREDIR.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL CREDIR.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*CREDIR/SSAVE=CREDIR/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
DELFIL::
;*******************************************************************************
;				DELFIL
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	DELFIL.MAC
;
;Output files:
;	DELFIL.DOC
;	DELFIL.EXE
;	DELFIL.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPILE /CREF /COMPIL DELFIL.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*DELFIL/SSAVE=DELFIL/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
DSKLST::
;*******************************************************************************
;				DSKLST
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	DSKLST.MAC
;
;Output files:
;	DSKLST.DOC
;	DSKLST.EXE
;	DSKLST.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.LOAD /CREF /COMPIL DSKLST.MAC
.IF (ERROR) .GOTO DIR
.SSAVE
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
DSKRAT::
;*******************************************************************************
;				DSKRAT
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	DSKRAT.MAC
;
;Output files:
;	DSKRAT.DOC
;	DSKRAT.EXE
;	DSKRAT.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL DSKRAT.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*DSKRAT/SSAVE=DSKRAT/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
FE::
;*******************************************************************************
;				FE
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	MACSYM.UNV
;Required files:
;	FE.MAC
;
;Output files:
;	FE.DOC
;	FE.EXE
;	FE.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMP /CREF /COMPIL FE.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*FE/SSAVE=FE/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
FEFILE::
;*******************************************************************************
;				FEFILE
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	FEFILE.MAC
;
;Output files:
;	FEFILE.DOC
;	FEFILE.EXE
;	FEFILE.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMP /CREF /COMPIL FEFILE.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*FEFILE/SSAVE=FEFILE/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
FILDAE::
;*******************************************************************************
;				FILDAE
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	SCAN.REL
;Required files:
;	FILDAE.MAC
;
;Output files:
;	FILDAE.DOC
;	FILDAE.EXE
;	FILDAE.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMP /CREF /COMPIL FILDAE.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*FILDAE/SAVE =/SEGMENT:LOW FILDAE,REL:SCAN/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
GLOB::
;*******************************************************************************
;				GLOB
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;Required files:
;	GLOB.MAC
;
;Output files:
;	GLOB.DOC
;	GLOB.EXE
;	GLOB.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL GLOB.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*GLOB/SSAVE=GLOB,REL:HELPER/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
HELPER::
;*******************************************************************************
;				HELPER
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	MACRO.EXE
;	MACTEN.UNV
;	UUOSYM.UNV
;Required files:
;	HELPER.MAC
;
;Output files:
;	HELPER.DOC
;	HELPER.REL
;	HELPER.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL HELPER.MAC
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
MACTEN::
;*******************************************************************************
;				MACTEN
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	MACTEN.MAC
;
;Output files:
;	MACTEN.DOC
;	MACTEN.UNV
;	MACTEN.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL MACTEN.MAC
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
MAKVFU::
;*******************************************************************************
;				MAKVFU
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	MACTEN.UNV
;Required files:
;	MACROS.MAC
;	MAKVFU.MAC
;
;Output files:
;	MACROS.UNV
;	MAKVFU.DOC
;	MAKVFU.EXE
;	MAKVFU.HLP
;	MAKVFU.LST
;	NORMAL.VFU
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL MACROS.MAC/NOBIN, MAKVFU.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*MAKVFU/SSAVE=MAKVFU/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
PAL10::
;*******************************************************************************
;				PAL10
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	PAL10.MAC
;
;Output files:
;	PAL10.DOC
;	PAL10.EXE
;	PAL10.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL PAL10.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*PAL10/SSAVE=PAL10/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
RUNOFF::
;*******************************************************************************
;				RUNOFF
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;	HELPER.REL
;	SCAN.REL
;	WILD.REL
;Required files:
;	RUNOFF.MAC
;
;Output files:
;	RUNOFF.DOC
;	RUNOFF.EXE
;	RUNINP.HLP
;	RUNOFF.HLP
;	RUNOFF.KDB
;	RUNOFF.MCR
;	RUNOFF.LST
;	RNFDOC.STD
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL RUNOFF.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*RUNOFF/SSAVE=RUNOFF/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
SCAN::
;*******************************************************************************
;				SCAN
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	MACRO.EXE
;	MACTEN.UNV
;	UUOSYM.UNV
;Required files:
;	SCAN.MAC
;	SCNMAC.MAC
;
;Output files:
;	SCAN.DOC
;	SCAN.REL
;	SCAN.LST
;	SCNMAC.LST
;	SCNMAC.UNV
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMP /CREF /COMP TTY:+DSK:SCNMAC.MAC(P,,),SCAN.MAC(P,,)
*%.C==-3
=
=
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
SOUP::
;*******************************************************************************
;				SOUP
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	CAMCTL.MAC
;	CAMIO.MAC
;	CAMLOW.MAC
;	COMERG.MAC
;	CAMPAR.MAC
;	CORFIH.MAC
;	FEDCTL.MAC
;	FEDIT.MAC
;	FITRAK.MAC
;	SERVIS.MAC
;	10K.MAC
;	10KIMP.MAC
;
;Output files:
;	CAM.EXE
;	FED.EXE
;	COMP10.EXE
;	CAM.MAP
;	FED.MAP
;	COMP10.MAP
;	SOUP.DOC
;	SOUP.HLP
;	CAMCTL.LST
;	CAMIO.LST
;	CAMLOW.LST
;	COMERG.LST
;	CAMPAR.LST
;	CORFIH.LST
;	FEDCTL.LST
;	FEDIT.LST
;	FITRAK.LST
;	SERVIS.LST
;	10K.LST
;	10KIMP.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL CAMCTL.MAC,CAMIO.MAC,CAMLOW.MAC,COMERG.MAC,COMPAR.MAC,CORFIH.MAC,FEDCTL.MAC,FEDIT.MAC,FITRAK.MAC,SERVIS.MAC,10KIMP.MAC,10K.MAC
.IF (ERROR) .GOTO DIR
.R LINK
*CAM/SSAVE=CAMLOW,CAMCTL,CAMIO,COMERG,COMPAR,CORFIH,FITRAK,SERVIS/GO
.IF (ERROR) .GOTO DIR
.COMPIL /COMPIL TTY:+DSK:CAMLOW.MAC
*CMPRSW=1
=
=
.IF (ERROR) .GOTO DIR
.R LINK
*FED/SSAVE=CAMLOW,FEDCTL,FEDIT,CAMIO,CORFIH,SERVIS/GO
.COMPIL /COMPIL TTY:+DSK:CAMIO.MAC
*BIGSW=1
=
=
.IF (ERROR) .GOTO DIR
.R LINK
*COMP10/SSAVE=10KIMP,10K,COMPAR,CAMIO/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
SYSDPY::
;*******************************************************************************
;				SYSDPY
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	LINK.EXE
;	LNK???.EXE
;	MACRO.EXE
;Required files:
;	SYSDPU.MAC
;	SYSDPY.MAC
;
;Output files:
;	SYSDPY.EXE
;	SYSDPA.EXE
;	SYSDPB.EXE
;	SYSVBX.EXE
;	SYSV50.EXE
;	SYSV52.EXE
;	SYSV61.EXE
;	SYSHZL.EXE
;	SYSDLT.EXE
;	SYSANS.EXE
;	SYSDPY.LST
;	SYSDPA.LST
;	SYSDPB.LST
;	SYSVBX.LST
;	SYSV50.LST
;	SYSV52.LST
;	SYSV61.LST
;	SYSHZL.LST
;	SYSDLT.LST
;	SYSANS.LST
;	SYSDPY.DOC
;	SYSDPY.MAN
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.R MACRO
*NUL:=F/U
*NUL:=S/U
*NUL:=NETPRM/U
*NUL:=MACSYM/U
*NUL:=D36PAR/U
*NUL:=SYSDPU/U
.IF (ERROR) .GOTO DIR
*SYSDPY=TTY:,DSK:SYSDPY
*V.DISP==0			;VT06 terminal
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSDPA=TTY:,DSK:SYSDPY
*V.DISP==1			;VT05A terminal
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSDPB=TTY:,DSK:SYSDPY
*V.DISP==2			;VT05B terminal
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSVBX=TTY:,DSK:SYSDPY
*V.DISP==3			;VB10C display
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSV50=TTY:,DSK:SYSDPY
*V.DISP==5			;VT50 DECscope
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSV52=TTY:,DSK:SYSDPY
*V.DISP==6			;VT52 DECscope
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSV61=TTY:,DSK:SYSDPY
*V.DISP==4			;VT61 DECscope
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSHZL=TTY:,DSK:SYSDPY
*V.DISP==20			;Hazeltine 2000 terminal
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSDLT=TTY:,DSK:SYSDPY
*V.DISP==21			;Delta Data Telterm terminal
=^Z
=^Z
.IF (ERROR) .GOTO DIR
*SYSANS=TTY:,DSK:SYSDPY
*V.DISP==7			;VT100 or other ANSI terminal
=^Z
=^Z
.IF (ERROR) .GOTO DIR
.R LINK
*SYSDPY/SSAVE=SYSDPY/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSDPA/SSAVE=SYSDPA/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSDPB/SSAVE=SYSDPB/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSVBX/SSAVE=SYSVBX/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSV50/SSAVE=SYSV50/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSV52/SSAVE=SYSV52/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSV61/SSAVE=SYSV61/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSHZL/SSAVE=SYSHZL/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSDLT/SSAVE=SYSDLT/GO
.IF (ERROR) .GOTO DIR
.R LINK
*SYSANS/SSAVE=SYSANS/GO
.IF (ERROR) .GOTO DIR
;
.GOTO COMMON
WILD::
;*******************************************************************************
;				WILD
;*******************************************************************************
;
;Required cusps:
;	COMPIL.EXE
;	CREF.EXE
;	DIRECT.EXE
;	MACRO.EXE
;	HELPER.REL
;	MACTEN.UNV
;	UUOSYM.UNV
;Required files:
;	WILD.MAC
;
;Output files:
;	WILD.DOC
;	WILD.REL
;	WILD.LST
;
.SET WATCH VERSION
.ASSIGN DEC SYS
.ASSIGN DEC UNV
.ASSIGN DEC REL
;
.COMPIL /CREF /COMPIL WILD.MAC
.IF (ERROR)  .GOTO DIR
;
.GOTO COMMON
COMMON::
;*******************************************************************************
;				Common Ending
;*******************************************************************************
;
;.ASSIGN DSK LPT
;.CREF
.IF (ERROR)  .GOTO DIR
;.DEASSI LPT
;
DIR::
.NOERROR
;.DIRECT /CHECKS DSK:
;
%ERR::
%CERR::
%TERR::
;
EXIT::
%FIN::
    G@
3£