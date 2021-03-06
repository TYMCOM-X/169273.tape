!DDT.CTL %40(220) -- Control file to make all TOPS-10 flavors of DDT
!Submit with command .SUBMIT DDT/TIME:10:00/RESTART
!
!
!This control file is provided for information purposes only.
!The  purpose  of the file is to document the procedures used
!to build the distributed software.  It  is  unlikely  to  be
!able  to  be executed without modification on other systems.
!In particular, attention should be given to  ersatz  devices
!and structure names, PPN's and other such system parameters.
!Submit times may vary depending on configuration  and  load.
!The  availability  of  sufficient  disk  space  and  core is
!mandatory.  This  control  file  has  not  been  extensively
!tested  on  alternate  configurations.   It  has  been  used
!successfully for the purpose for which it is  intended:   to
!build the distributed software.
!
!
!
!Required files:	(Latest released versions)
!DEC:	PIP.EXE
!	DIRECT.EXE
!	COMPIL.EXE
!	MACRO.EXE
!	LINK.EXE
!	LNKSCN.EXE
!	LNKXIT.EXE
!	LNKERR.EXE
!	LNKLOD.EXE
!	LNK999.EXE
!	CREF.EXE
!	CHEAD.MAC
!	MACTEN.MAC
!	UUOSYM.MAC
!DSK:	DDT.MAC
!	FTEDDT.MAC
!	FTUDDT.MAC
!	FTFDDT.MAC
!	FTVDDT.MAC
!	DDT40.RND
!
!Output files:
!	EDDT.REL	(Exec mode DDT)
!	DDT.REL		(User mode DDT)
!	DDT.EXE		(Executable DDT with symbols)
!	FILDDT.EXE	(File DDT)
!	DDT.VMX		(User mode virtual memory DDT)
!
!Output listings:
!	DDT.LOG
!	DDT40.DOC
!
!
!Catch all "%" errors (none are expected)
.ERROR %
!
!Set up to use all field image software from DEC:
.ASSIGN DEC SYS
.ASSIGN DEC REL
.ASSIGN DEC UNV
.ASSIGN DEC MAC
!
!Make a record of all files used and appropriate versions
.SET WATCH VERSION
!
!Record checksums of all files used
.R DIRECT
*/CHECKSUM SYS:PIP.EXE,SYS:DIRECT.EXE,SYS:COMPIL.EXE,SYS:MACRO.EXE-
*,SYS:LINK.EXE,SYS:LNKSCN.EXE,SYS:LNKXIT.EXE,SYS:LNKERR.EXE-
*,SYS:LNKLOD.EXE,SYS:LNK999.EXE,SYS:CREF.EXE-
*,MAC:CHEAD.MAC,MAC:MACTEN.MAC,MAC:UUOSYM.MAC-
*,DSK:DDT.MAC,DSK:FTEDDT.MAC,DSK:FTUDDT.MAC,DSK:FTFDDT.MAC-
*,DSK:FTVDDT.MAC,DSK:DDT40.RND
!
!Make parameter file to generate C.REL for DDT symbols
.COPY CPARAM.MAC=TTY:
*	%.C==-2		;ASSEMBLE 2 PASSES WITH END STATEMENT
*^Z
!
!Generate C.REL for DDT symbols
.COMPILE/COMPILE DSK:C.REL=DSK:CPARAM+MAC:CHEAD+MAC:MACTEN+MAC:UUOSYM
.IF (ERROR) !OK, UUOSYM bug gives 5 "A" errors -- REL file is still OK
!
!Make EDDT.REL
.COMPILE/COMPILE EDDT.REL=FTEDDT.MAC+DDT.MAC
!
!Make DDT.REL
.COMPILE/COMPILE DDT.REL=FTUDDT.MAC+DDT.MAC
!
!Make DDT.EXE
.R LINK
*/LOCALS/SYMSEG:LOW/PATCHS:2K DDT.REL/NOLOCALS,C.REL/GO
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
=PAT../
*D:
*^[^[Z
*^C
.NSAVE DDT
!Make FILDDT.EXE
.LOAD/COMPILE FILDDT.REL=FTFDDT.MAC+DDT.MAC
.NSAVE FILDDT
!
!Make DDT.VMX
.EXECUTE/COMPILE VMDDT.REL=FTVDDT.MAC+DDT.MAC
!
!Produce documentation
.R RUNOFF
*DDT40.DOC=DDT40.RND
!
!Checksum all output files
.R DIRECT
*/CHECKSUM EDDT.REL,DDT.REL,DDT.EXE,FILDDT.EXE,DDT.VMX,DDT40.DOC
!
!Tell results to operator
.PLEASE DDT Successful^[
.GOTO FIN
!
!
!Here on any errors -- tell the operator
%ERR::
%CERR::
%TERR::
.PLEASE DDT Unsuccessful^[
!
!
!Here at end to delete all temporary files
FIN::
%FIN::
.DELETE CPARAM.MAC,C.REL,FILDDT.REL,VMDDT.REL
    