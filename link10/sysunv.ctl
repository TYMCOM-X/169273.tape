! SYSUNV.CTL  %1(3)  02-Aug-82  /RWS
!
! Input files:
!
!		MACRO.EXE	MACRO modified EE's
!		SYSBLD.MAC	Used to make SYSUNV.UNV from scratch, may
!				NOT currently work
!		SYSUNV.MAC	Used to make SYSUNV.UNV
!		SYSUNV.RNM	Used to make the SYSUNV documentation, using
!				DSR (DEC Standard RUNOFF)
!		TYPER.MAC	Used to build the various version of TYPER
!		TYP7I.T10	Used to make PSECT'ed TOPS-10 TYPER without
!				SCAN support
!		TYP7I.T20	Used to make PSECT'ed TOPS-20 TYPER without
!				SCAN support
!		TYP7N.T10	Used to make two segment TOPS-10 TYPER without
!				SCAN support
!		TYP7N.T20	Used to make two segment TOPS-20 TYPER without
!				SCAN support
!		TYP7Q.T10	Used to make one segment TOPS-10 TYPER for use
!				with QUASAR/GLXLIB
!		TYP7Q.T20	Used to make one segment TOPS-20 TYPER for use
!				with QUASAR/GLXLIB
!		TYP7S.T10	Used to make two segment TOPS-10 TYPER with
!				SCAN support
!		TYP7S.T20	Used to make two segment TOPS-20 TYPER with
!				SCAN support
!
! Output files:
!
!		SYSUNV.MAN	The SYSUNV documentation, using DSR (DEC
!				Standard RUNOFF)
!		SYSUNV.UNV	The SYSUNV universal file
!		TYP7I.REL	PSECT'ed TYPER without SCAN support
!		TYP7N.REL	Two segment TYPER without SCAN support
!		TYP7Q.REL	One segment TYPER for use with QUASAR/GLXLIB
!		TYP7S.REL	Two segment TYPER with SCAN support

! Assemble SYSUNV (if needed)

.PATH /SYS			;In case DSK:MACRO.EXE does not exist
.ERROR %
.DIRECT SYSUNV.UNV
.IF (NOERROR) .GOTO MAKSYS
.ERROR ?
.RUN MACRO
*=SYSBLD.MAC,SYSUNV.MAC
.IF (ERROR) .GOTO ABORT

! Assemble SYSUNV

MAKSYS::
.ERROR ?
.RUN MACRO
*,SYSUNV/C=SYSUNV.MAC
.IF (ERROR) .GOTO ABORT
.
.R CREF
*DSK:SYSUNV.LST/O=SYSUNV.CRF
.
.R VERSIO
*SYSUNV.UNV,SYSUNV.LST=SYSUNV.MAC
.IF(ERROR).;Ignore
.

! The documentation file listing

.SET WATCH ALL
.R DSR			;DEC Standard RUNOFF (alias NRUNOF from TOOLS tape)
*SYSUNV.MAN=SYSUNV.RNM
.IF (ERROR) .GOTO ABORT
.SET WATCH NONE

! Assemble TYPER

TYPER::
.RUN MACRO
*TYP7I=TYP7I.T10,TYPER/U		; KL without SCAN (multiple sections)
.RUN MACRO				; To avoid "CORE ALLOCATION PROBLEM"
*TYP7N,TYPER/C=TYP7N.T10,TYPER/U	; KL without SCAN
.RUN MACRO
*TYP7Q=TYP7Q.T10,TYPER/U		; KL without SCAN for QUASAR, T1=3
.RUN MACRO
*TYP7S=TYP7S.T10,TYPER/U		; KL with SCAN
.
.R CREF
*DSK:TYPER.LST=TYPER
.
.R VERSION				; Set .RBVER and .RBSPL
*TYP7??,TYPER=TYPER.MAC
*TYP7I.REL=/NAME:"KL-PSC" 		; With PSECTs
*TYP7N.REL=/NAME:"KL-NO" 		; With no SCAN support
*TYP7Q.REL=/NAME:"KL-QSR" 		; For GALAXY-QUASAR
*TYP7S.REL=/NAME:"KL-SCN" 		; With SCAN interface
*/EXIT
.IF(ERROR).;Ignore


! Inform the world what happened

.PLEASE *** [SYSUNV.CTL] - No errors detected^[
.PRINT /HEADER SYSUNV.LST,TYPER.LST
.PRINT /HEADER SYSUNV.MAN/FORMS:NARROW
.GOTO FINISH

ABORT::
.PLEASE *** [SYSUNV.CTL] - An error was detected^[

FINISH::

.DIRECT				;End of SYSUNV.CTL

  