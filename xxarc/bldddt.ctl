; Batch control file to build DDT.VMX, FILDDT.EXE, FILDDT.SYM,
; DDT.EXE (user mode), and DDT.REL
;
;	Required files:
;			DDT.MAC		; DDT source
;			FTVDDT.MAC	; Feature tests for DDT.VMX
;			FTFDDT.MAC	; Feature tests for FILDDT
;			FTUDDT.MAC	; Feature tests for user-mode DDT
;			XDDT.MAC	; Invoke DDT on .R
;			MAC:CHEAD
;			MAC:MACTEN
;			MAC:UUOSYM
;
.SET WATCH VERSION
.R NEW
.IF (ERROR)			;; Don't let this stop us
;
;	Verify all source files present
;
.ERROR %
.DIRECT /ERNONE/F DDT.MAC,FTEDDT.MAC,FTUDDT.MAC,FTVDDT.MAC,FTFDDT.MAC
.DIRECT /ERNONE/F XDDT.MAC,MAC:.MAC(CHEAD,MACTEN,UUOSYM)
.ERROR
;
;	Build DDT.VMX
;
.R MACRO
*MAKDDT=FTVDDT.MAC,DSK:DDT

.EXECUTE /REL MAKDDT			;; Make DDT.VMX
;
;	Build FILDDT
;
.R MACRO
*FILDDT=FTFDDT.MAC,DSK:DDT

.R LINK
*FILDDT/GO				;; Make FILDDT.EXE
.NSSAVE FILDDT
;
.R LINK
*FILDDT/SYFILE=/LOCALS FILDDT/GO	;; Make FILDDT.SYM

; Now make DDT.EXE
;
.COPY C.TMP=TTY:
*%.C==-1		; No end statement (is in XDDT).
*^Z
;
.R MACRO				;; Build user mode DDT
*DDT=FTUDDT.MAC,DSK:DDT

.R MACRO
*XDDT=C.TMP,MAC:CHEAD,MAC:MACTEN,MAC:UUOSYM,DSK:XDDT

.R LINK
*NUL:/SYFILE=XDDT/LOCAL,DDT/PATCHSIZE:1100/G
.DDT
*XDDT^[:^Z
.NSSAVE DDT
;
; Now to clean up some temporary files
;
.DELETE *.TMP/NOASK
.IF(ERROR)	; No problem
%FIN::
