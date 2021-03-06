!SWIL.CTL %1 -- Control file to make the SWIL package
!
!Submit with command .SUBMIT SWIL/TIME:10:00/RESTART to rebuild
!SWIL from scratch; or with /TAG:CREF to build CREF listings too.
!
!
!   This control file is provided for information purposes only.
!   The  purpose  of the file is to document the procedures used
!   to build the distributed software.  It  is  unlikely  to  be
!   able  to  be executed without modification on other systems.
!   In particular, attention should be given to  ersatz  devices
!   and structure names, PPN's and other such system parameters.
!   Submit times may vary depending on configuration  and  load.
!   The  availability  of  sufficient  disk  space  and  core is
!   mandatory.  This  control  file  has  not  been  extensively
!   tested  on  alternate  configurations.   It  has  been  used
!   successfully for the purpose for which it is  intended:   to
!   build the distributed software.
!
!
!
!Required files:	(Latest released versions)
!SYS:	PIP.EXE
!	DIRECT.EXE
!	COMPIL.EXE
!	MACRO.EXE
!	LINK.EXE
!	LNKSCN.EXE
!	LNKXIT.EXE
!	LNKERR.EXE
!	LNKLOD.EXE
!	LNK999.EXE
!UNV:	JOBDAT.UNV
!	MACTEN.UNV
!	UUOSYM.UNV
!DSK:	SWIL.CTL		;This file
!	SWIL.CMD		;COMPIL assembly command file
!	SWIL.CCL		;PIP concatenation command file
!	SWIL.MAC		;SWIL universal definitions module
!	SWIFIL.MAC		;SWIL file-level operations module
!	SWIWLD.MAC		;SWIL local wildcard operations module
!	SWILIO.MAC		;SWIL I/O operations module
!	SWINET.MAC		;SWIL network operations module
!	SWISCN.MAC		;SWIL command scanning module
!	SWIHLP.MAC		;SWIL "HELP" processor module
!	SWIQUE.MAC		;SWIL GALAXY interface module
!	SWIERM.MAC		;SWIL error text/message module
!	SWITOU.MAC		;SWIL typeout (command output) module
!	SWIMEM.MAC		;SWIL memory manager module
!	SWIMSC.MAC		;SWIL miscellaneous routines module
!
!Output files:
!DSK:	SWIL.UNV		;Universal definitions for SWIL users
!	SWIL.REL		;Scan Wild I/o Library search file
!
!Output listings:
!DSK:	*.LST			;CREF listings, if SUBMIT/TAG:CREF
!	SWIL.LOG		;Batch log file
!
!Here for normal SWIL assembly - no CREF listings generated
!
.DELETE SWIL.CRF
.IF (ERROR)
.GOTO BUILD
!
!
!
!Here /TAG:CREF for SWIL assembly and CREF listings
!
CREF::
.COPY SWIL.CRF=NUL:
.GOTO BUILD
!
!
!
!Start the actual build process
!
BUILD::
.CHKPNT BUILD
!
!Catch all "%" errors (none are expected)
!
.ERROR %
!
!Set up to use all field image software from DEC:
!
;.ASSIGN DEC SYS
;.ASSIGN DEC REL
;.ASSIGN DEC UNV
!
!Make a record of all files used and appropriate versions
!
.SET WATCH VERSION
!
!Record checksums of all files used
!
.R DIRECT
*/NOOPTION/CHECKSUM -
*SYS:PIP.EXE,    SYS:DIRECT.EXE, SYS:COMPIL.EXE, SYS:MACRO.EXE, -
*SYS:LINK.EXE,   SYS:LNKSCN.EXE, SYS:LNKXIT.EXE, -
*SYS:LNKERR.EXE, SYS:LNKLOD.EXE, SYS:LNK999.EXE, -
*REL:JOBDAT.REL, REL:MACTEN.REL, REL:UUOSYM.REL, -
*UNV:JOBDAT.UNV, UNV:MACTEN.UNV, UNV:UUOSYM.UNV,-
*DSK:SWIL.MAC,   DSK:SWIFIL.MAC, DSK:SWIWLD.MAC, -
*DSK:SWILIO.MAC, DSK:SWINET.MAC, DSK:SWISCN.MAC, -
*DSK:SWIHLP.MAC, DSK:SWIQUE.MAC, DSK:SWIERM.MAC, -
*DSK:SWITOU.MAC, DSK:SWIMEM.MAC, DSK:SWIMSC.MAC
!
!First build the "library" package
!
.DIRECT/ERNONE SWIL.CRF
.IF (NOERROR).GOTO BUIL2
!
!			*** Bizarro Buggo ***
!
!	SWIL ***MUST*** be compiled separately from the SWIL.CMD file
!	used to compile all the rest of the modules - otherwise LINK
!	goes Ill Mem Ref . . .
!
BUIL1::
.CHKPNT BUIL1
.COMPILE/COMPILE SWIL.MAC
.COMPILE/COMPILE @SWIL.CMD
.GOTO PIP
!
BUIL2::
.CHKPNT BUIL2
.DELETE SWIL.CRF
.COMPILE/COMPILE/CREF SWIL.MAC
.CREF
.COMPILE/COMPILE/CREF @SWIL.CMD
.CREF
.GOTO PIP
!
!Combine into the library REL file
!
PIP::
.CHKPNT PIP
.R PIP 100P
SWIL.CCL@
!
!Checksum all output files
!
.R DIRECT
*/NOOPTION/CHECKSUM SWIL.UNV, SWIL.REL
    