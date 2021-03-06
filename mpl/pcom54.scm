File 1)	DSK:PCOM53.SAI	created: 1925 07-MAR-83
File 2)	DSK:PCOM54.SAI	created: 1926 07-MAR-83

1)1	require (Ifcr PRELIMINARY thenc '101 elsec '1 endc lsh 18) lor '53 version;
1)	require "
****
2)1	require (Ifcr PRELIMINARY thenc '101 elsec '1 endc lsh 18) lor '54 version;
2)	require "
**************
1)3	Ifcr PRELIMINARY thenc
1)	    SW!SN (LICENSE,0)
1)	endc
1)	    SW!SN (MAIL,-1)
****
2)3	    SW!SN (LICENSE,1)
2)	    SW!SN (MAIL,-1)
**************
1)6	     Calli( 0,If Detach! then calli!LOGOFF else calli!EXIT );
1)	end;
****
2)6	    Calli( 0,If Detach! then calli!LOGOFF else calli!EXIT );
2)	end;
************ **
1)7	procedure SYNC;
1)	begin					! synchronize with slave;
****
2)7	simple procedure SYNC;
2)	begin					! synchronize with slave;
**************
1)17	    /LOG[:arg]	- Specify the log file creation and deletion criteria
****
2)17	    /LICENSE	- Pass user's current license to the processing frame
2)	    /LOG[:arg]	- Specify the log file creation and deletion criteria
**************
1)17	    /LICENSE	- Pass user's current license to child frame
1)	    /PRINT:arg	- Print various debugging messages
****
2)17	    /PRINT:arg	- Print various debugging messages
**************
1)17	    APPEND, DETACH, HELP, LOG, MAIL,
1)	    NEWS, SUPERSEDE, TIME, TRULIMIT",
1)	Ifcr PRELIMINARY thenc ",
1)	experimental:
1)	    BAIL, CHARS, LICENSE, PRINT, SLEEP",
1)	endc  "
****
2)17	    APPEND, DETACH, HELP, LICENSE, LOG,
2)	    MAIL, NEWS, SUPERSEDE, TIME, TRULIMIT",
2)	Ifcr PRELIMINARY thenc ",
2)	experimental:
2)	    BAIL, CHARS, PRINT, SLEEP",
2)	endc  "
**************
1)17	PCOM ""news"" has been incorporated into the standard system
1)	documentation files.  Please refer to PCOM.DOC for further details.
1)	");
****
File 1)	DSK:PCOM53.SAI	created: 1925 07-MAR-83
File 2)	DSK:PCOM54.SAI	created: 1926 07-MAR-83

2)17	                -- New Features --
2)	 o Added /LICENSE switch which passes the current frame
2)	   license to the processing frame.  Use /NOLICENSE to
2)	   inhibit the passing of license to the child.
2)	Please refer to PCOM.DOC for further details.
2)	");
**************
1)21	    Ifcr PRELIMINARY thenc Integer array MyLic[0:1]; endc
1)	    loop!count _ 0;			! set clock running;
****
2)21	    loop!count _ 0;			! set clock running;
**************
1)22		"/",Print!switch("LOG"),			! Log file?;
****
2)22		"/",Print!switch("LICENSE"),			! pass user license;
2)		"/",Print!switch("LOG"),			! Log file?;
**************
1)22		    "/",Print!switch("LICENSE"),		! * pass user license;
1)	! *SW$OVB*  "/",Print!switch("PRINT"),			! * print debugging;
****
2)22	! *SW$OVB*  "/",Print!switch("PRINT"),			! * print debugging;
**************
1)25	    FD _ Create!child;		! create a child frame;
****
2)25	    if not swLICENSE then	! if user specified "/NOLICENSE";
2)	      frame!block[ FrmLIC ]_ 0;	!  then clear user license;
2)	    FD _ Create!child;		! create a child frame;
**************
1)27		    Print!switch("TIME")&" "&			! TimeLimit;
****
2)27		    (If swLICENSE < 0 then ""
2)		      else Print!switch("LICENSE")&" ")&	! License set?;
2)		    #cr);
2)		OutPtr(Port,"; Job limits: "&
2)		    Print!switch("TIME")&" "&			! TimeLimit;
**************
