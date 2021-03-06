File 1)	DSK:PCOM63.SAI	created: 1923 01-JUL-83
File 2)	DSK:PCOM67.SAI	created: 0304 12-AUG-83

1)1	Define PRELIMINARY = FALSE;		comment TRUE if not-released;
1)	require (Ifcr PRELIMINARY thenc '101 elsec '1 endc lsh 18) lor '63 version;
1)	require "
1)	PCOM - TYMCOM-X interim PERP exec language
1)	Assembly: Load @PCOM.CMD
1)	Sources:  PCOM.SAI, UUOSYM.DEF, SWDEF.DEF
1)	Library:  PCOINT.DCL, PCOINT.SAI [REL], PCODUL.SAI
1)	License:  ALL license bits!!! " message;
****
2)1	Define PRELIMINARY = FALSE;		comment TRUE if not-released  ;
2)	Define LICENSEHACK = TRUE;		comment TRUE if poking JBTLIC ;
2)						comment -- not im plemented -- ;
2)	Define ROYALTYHACK = FALSE;		comment TRUE if poking JBTLOG ;
2)	Define PCOM!VERSION = (Ifcr PRELIMINARY
2)				 thenc '101
2)				 elsec '1 endc lsh 18)
2)				lor '67;
2)	require PCOM!VERSION version;
2)	require "
2)	PCOM "&CVOS(PCOM!VERSION lsh -18)&"."&CVOS(PCOM!VERSION land '777777)&
2)	     " - TYMCOM-X interim PERP exec language
2)	Assembly: Load @PCOM.CMD
2)	Sources:  PCOM.SAI, UUOSYM.DEF, SWDEF.DEF
2)	Library:  PCOINT.DCL, PCOINT.SAI [REL]
2)	License:  ALL license bits!!! " message;
**************
1)1	require "PCODUL.DCL" source!file;
1)	require 25 polling!interval;
****
2)1	require 25 polling!interval;
**************
1)2	;
****
2)2	,	Error!FMT = Error!ISA+1	! Formatting error;
2)	,	Error!CMD = Error!FMT+1	! Command error;
2)	;
**************
1)3	    SW!SS (SLEEP,-1,SW$VRQ)
****
2)3	endc
2)	Ifcr ROYALTYHACK thenc
2)	    SW!SS (ROYALTY,1)
2)	endc
2)	Ifcr PRELIMINARY thenc
2)	    SW!SS (SLEEP,-1,SW$VRQ)
**************
1)6			[else] Cvs( ErrorCondition )  ))&
****
2)6			[Error!FMT] "Formatting error",
2)			[Error!CMD] "Command error",
2)			[else] Cvs( ErrorCondition )  ))&
**************
1)8	    IncLic;				! Increase license;
1)	    Name _ CvUser( AUN );		! Convert username;
File 1)	DSK:PCOM63.SAI	created: 1923 01-JUL-83
File 2)	DSK:PCOM67.SAI	created: 0304 12-AUG-83

1)	    DecLic;				! Reduce license;
1)	    Return( Name );			! Return;
1)	end;
****
2)8	    integer CvnChan,CvnEof;
2)	    integer array Look[ 0 : !RBUNM+1 ];
2)	    IncLic;				! Increase license;
2)	    Look[ !RBCNT ]_ !RBUNM+1;		! PPN->USER;
2)	    Look[ !RBPPN ]_ !Xwd( 1,1 );	! (UFD)  ;
2)	    Look[ !RBNAM ]_ AUN;		! [user] ;
2)	    Look[ !RBEXT ]_ CVSIX("UFD   ");	! .UFD   ;
2)	    Open(CvnChan_getchan,"DSK",'17,0,0, 0,0, CvnEof_-1);
2)	    If not ( CvnEof )
2)	     then begin
2)		Chnior(CvnChan,Look[!RBCNT],!CHLK);
2)		CvnEof_ not ( !SKIP! );
2)		Release(CvnChan);
2)	     end;
2)	    DecLic;				! Reduce license;
2)	    If ( CvnEof )
2)	     then Return( Cvos(!lh(Aun))&","&Cvos(!rh(Aun)) )
2)	     else Return( Cv6str(Look[!RBUNM])&Cv6str(Look[!RBUNM+1]) );
2)	end;
**************
1)8		[7] Cvs(101+R[1])[2 for 2]),
1)		[else] Null
****
2)8		[7] Cvs(101+R[1])[2 for 2],
2)		[else] Null
**************
1)9	    Assign( $Second      Second$);	! Current seconds of minute;
1)	    Assign( $SS,         SS$);		! Current seconds of minute SS;
****
2)9	    Assign( $Second,     Second$);	! Current seconds of minute;
2)	    Assign( $SS,         SS$);		! Current seconds of minute SS;
**************
1)22	    DecLic;			! reduce license;
****
2)22	    Ifcr LICENSEHACK thenc
2)		require Crlf&"Is it time to take out the LICENSE hack yet?"&Crlf
2)		    message;
2)		If swLICENSE > 0
2)		 then begin "LICSET"
2)		    Integer array PassLicense[0:1];
2)		    PassLicense[0]_ Calli( !Xwd(-1,!GTLIC), Calli!GETTAB );
2)		    PassLicense[1]_ ( ( Calli( !Xwd(!GTLIC,!GTSLF), Calli!GETTAB )
2)				      land '777777 ) + (frame!index land '777) ) 
2)				    lor !bit(0); ! Virtual address + frame #;
2)		    Calli( Location(PassLicense[0]), Calli!POKE );
2)		 end "LICSET";
2)	    endc
2)	    DecLic;			! reduce license;
**************
1)26	    start!code
File 1)	DSK:PCOM63.SAI	created: 1923 01-JUL-83
File 2)	DSK:PCOM67.SAI	created: 0304 12-AUG-83

****
2)26	    Ifcr ROYALTYHACK thenc
2)		require Crlf&"IS it time to take out the JBTLOG hack?"&Crlf
2)		    message;
2)	    endc
2)	    start!code
**************
1)27	    If File!Error then begin			! If error...;
1)		Fatal("COM Filename error """&Name&"""");
1)	      end
1)	      else begin
1)		Delim!list _ Del!Chr & Delim!list;	! add delimiter to list;
****
2)27	    If File!Error
2)	     then begin					! If error...;
2)		Fatal("COM Filename error """&Name&"""");
2)	     end
2)	     else begin
2)		Delim!list _ Del!Chr & Delim!list;	! add delimiter to list;
**************
1)27		open(IChan,IDevice,1,4,0,1024,BRK,EOF);	! open the file;
1)		lookup(IChan, Name, EOF_-1);		!   "   "   "  ;
1)		If EOF and not Ext!Found then		! if not there, then;
1)		  lookup(IChan, Name&Default!Ext, EOF);	!  try other extension;
1)		Check!Logfile( LogFileName );		! skip :LOGFILE if any;
****
2)27		open(IChan,IDevice,1,4,0,1024,BRK,EOF_-1);
2)		If ( EOF )				! abort if device is bad;
2)		 then Fatal("COM Device error """&IDevice&""" - Aborting.");
2)		lookup(IChan, Name, EOF_-1);		! open the file;
2)		If EOF and not Ext!Found then		! if not there, then;
2)		 lookup(IChan,Name&Default!Ext,EOF_-1);	!  try other extension;
2)		If ( EOF )				! if still not there;
2)		 then begin				!  print a message;
2)		    Fatal("COM Filename error ("& Cvos(!rh(EOF))&
2)			  ") for file """&Name&"""");
2)		    S!Chan_ Lop(IChan);			!  clear channel table;
2)		    Return;				!  and abort;
2)		 end;
2)		Check!Logfile( LogFileName );		! skip :LOGFILE if any;
**************
1)29	  end;
1)	  Forget!Substitutions;			! clear current substitutions;
****
2)29	  end "one file";
2)	  Forget!Substitutions;			! clear current substitutions;
**************
1)29	  If Delim!list neq Del!Chr then	! if delimiter mangled then;
1)	    Set!Delimiter( Delim!list );	!   use current top of stack;
1)	  EOF _ false;				! clear end of file;
1)	end;
1)	Status _ Status!DONE;			! let parent know we are done;
****
2)29	  If ( Delim!list neq Del!Chr )		! if delimiter mangled then;
File 1)	DSK:PCOM63.SAI	created: 1923 01-JUL-83
File 2)	DSK:PCOM67.SAI	created: 0304 12-AUG-83

2)	   then Set!Delimiter( Delim!list );	!   use current top of stack;
2)	  EOF _ ERR!;				! if no errors, clear end of file;
2)	end "main loop";
2)	Status _ Status!DONE;			! let parent know we are done;
**************
