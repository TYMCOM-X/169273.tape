

define
    F.CCL  = 1		comment start program at start+1 ;
,   F.NTTY = 2		comment do not pass TTY: to child ;
,   F.NWAI = 4		comment do not wait for child to terminate ;
,   F.NEOL = 8		comment do not add (#cr-#lf) at end of tmpcor data ;
,   F.NLIC = 16		comment do not pass user license to child frame ;
,   F.SAVE = 32		comment save terminated frame ;

,   E.NOER = 000	comment no errors detected (EXIT or F.NWAI+started) ;
,   E.HNHS = 001	comment child halted with no halt status ( ? ^C) ;
,   E.OPN  = 002	comment open failure for device DSK: ;
,   E.ENT  = 003	comment enter failure for temp file (on disk) ;
,   E.OUT  = 004	comment output error for temp file (on disk) ;
,   E.GTAB = 005	comment gettab failed (frame probably vanished) ;
,   E.HIBR = 006	comment HIBER failed, should never happen ;
,   E.IFS  = 007	comment Illegal program-name specification ;

,   E.HS   = 10000	comment Unexpected halt status ( code 1CCTT ) ;
,   E.FO   = 20000	comment FRMOP error ( code is 2XXXX ) ;
,   E.RT   = 30000	comment RETACH error ( code is 3XXXX ) ;
;
external integer procedure FRMRUN(
	    String  ProgramName;
	    Integer ProgramFlags;
	    Reference Integer ProgramCode;
	    String  TempFileName;
	    String  TempFileData;
	    String  SaveFileName(null) );
