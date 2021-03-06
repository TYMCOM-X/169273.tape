File 1)	DSK:PCOM57.SAI	created: 0514 30-MAR-83
File 2)	DSK:PCOM60.SAI	created: 0515 30-MAR-83

1)1	require (Ifcr PRELIMINARY thenc '101 elsec '1 endc lsh 18) lor '57 version;
1)	require "
****
2)1	require (Ifcr PRELIMINARY thenc '101 elsec '1 endc lsh 18) lor '60 version;
2)	require "
**************
1)1	Library:  PCOINT.DCL, PCOINT.SAI [REL]
1)	License:  WC RC SY" message;
1)	require "(SAILIB)SAIL.DEF" source!file;
****
2)1	Library:  PCOINT.DCL, PCOINT.SAI [REL], PCODUL.SAI
2)	License:  WC RC SY HF" message;
2)	require "(SAILIB)SAIL.DEF" source!file;
**************
1)1	require 25 polling!interval;
****
2)1	requir e "PCODUL.DCL" source!file;
2)	require 25 polling!interval;
**************
1)4	,	Parent, Child		! frame numbers of parent & child;
****
2)4	,	MyLicense		! initial license - to protect against HF;
2)	,	Parent, Child		! frame numbers of parent & child;
**************
1)6	simple procedure InitUser;
1)	begin
1)	    Redefine CF!LIC = !bit(16);		! until UUOSYM.DEF is right;
1)	    Status _ Status!BEGIN;		! tell parent we've started;
1)	    Parent _ calli(0,calli!PJOB);	! remember who we are;
1)	    frame!block[ FrmCNT ]_ (CF!LIC lor 5);
1)	    frame!block[ FrmPPN ]_ calli( !Xwd( -1,!GTPPN ), calli!GETTAB );
1)	    frame!block[ FrmPRV ]_ calli( !Xwd( -1,!GTPRV ), calli!GETTAB );
1)	    frame!block[ FrmAUN ]_ calli( !Xwd( -1,!GTAUN ), calli!GETTAB );
1)	    frame!block[ FrmUNM ]_ calli( !Xwd( -1,!GTUNM ), calli!GETTAB );
1)	    frame!block[ FrmUN1 ]_ calli( !Xwd( -1,!GTUN1 ), calli!GETTAB );
1)	    frame!block[ FrmLIC ]_ calli( !Xwd( -1,!GTLIC ), calli!GETTAB );
1)	    MyName _ cv6str(frame!block[ FrmUNM ]) & cv6str(frame!block[ FrmUN1 ]);
1)	end;
1)	require InitUser initialization;
1)	simple procedure SetDetach;
1)	begin
1)	    Detach! _ !lh(calli(!xwd(-1,!gtLIN),calli!GETTAB))=0;
1)		! set detach flag if we are detached;
1)	    if Detach! then 
1)		calli(JP!NAT lor calli(!xwd(-1,!GTPRV),calli!GETTAB),calli!SETPRV);
1)		! set NO ATTACH so we don't bother user when logging in;
1)	end;
1)	require SetDetach initialization;
1)	string procedure EXIT(Integer Msg(0) );	! string so it can be "printed" [ugh];
****
2)6	string procedure EXIT(Integer Msg(0) );	! string so it can be "printed" [ugh];
**************
1)6			[Error!LIC] "License WC RC SY required",
1)			[Error!CFM] "Create Frame Error # ("&Cvos(!rh(ErrorCondition))&")",
File 1)	DSK:PCOM57.SAI	created: 0514 30-MAR-83
File 2)	DSK:PCOM60.SAI	created: 0515 30-MAR-83

****
2)6			[Error!LIC] "License WC RC SY HF required",
2)			[Error!CFM] "Create Frame Error # ("&Cvos(!rh(ErrorCondition))&")",
**************
1)6	    then EXIT( Error!LIC lsh 18 );
****
2)6	    or (L land LC!HF = 0)
2)	    then EXIT( Error!LIC lsh 18 );
**************
1)6	procedure BrkIni;
****
2)6	simple procedure InitUser;
2)	begin
2)	    Redefine CF!LIC = !bit(16);		! until UUOSYM.DEF is right;
2)	    Status _ Status!BEGIN;		! tell parent we've started;
2)	    Parent _ calli(0,calli!PJOB);	! remember who we are;
2)	    frame!block[ FrmCNT ]_ (CF!LIC lor 5);
2)	    frame!block[ FrmPPN ]_ calli( !Xwd( -1,!GTPPN ), calli!GETTAB );
2)	    frame!block[ FrmPRV ]_ calli( !Xwd( -1,!GTPRV ), calli!GETTAB );
2)	    frame!block[ FrmAUN ]_ calli( !Xwd( -1,!GTAUN ), calli!GETTAB );
2)	    frame!block[ FrmUNM ]_ calli( !Xwd( -1,!GTUNM ), calli!GETTAB );
2)	    frame!block[ FrmUN1 ]_ calli( !Xwd( -1,!GTUN1 ), calli!GETTAB );
2)	    frame!block[ FrmLIC ]_ calli( !Xwd( -1,!GTLIC ), calli!GETTAB );
2)	    MyLicense _ frame!block[ FrmLIC ];	! copy license;
2)	    Calli( MyLicense land (lnot LC!HF), calli!SETLIC );	! reduce license;
2)	    MyName _ cv6str(frame!block[ FrmUNM ]) & cv6str(frame!block[ FrmUN1 ]);
2)	end;
2)	require InitUser initialization;
2)	simple procedure SetDetach;
2)	begin
2)	    Detach! _ !lh(calli(!xwd(-1,!gtLIN),calli!GETTAB))=0;
2)		! set detach flag if we are detached;
2)	    if Detach! then 
2)		calli(JP!NAT lor calli(!xwd(-1,!GTPRV),calli!GETTAB),calli!SETPRV);
2)		! set NO ATTACH so we don't bother user when logging in;
2)	end;
2)	require SetDetach initialization;
2)	procedure BrkIni;
**************
1)7	internal simple procedure ABORT(reference integer WHY);
****
2)7	string procedure CvNAME( integer AUN );
2)	begin
2)	    string Name;
2)	    Calli( MyLicense, calli!SETLIC );	! Increase license;
2)	    Name _ CvUser( AUN );		! Convert username;
2)	    Calli( MyLicense land (lnot LC!HF), calli!SETLIC );
2)	    Return( Name );			! Reduce license and return;
2)	end;
2)	internal simple procedure ABORT(reference integer WHY);
**************
1)24	    label INPUT.PROMPT;		! goto here for purpose of reprompting;
****
2)24	    integer Array F[1:6];	! used for file info;
File 1)	DSK:PCOM57.SAI	created: 0514 30-MAR-83
File 2)	DSK:PCOM60.SAI	created: 0515 30-MAR-83

2)	    label INPUT.PROMPT;		! goto here for purpose of reprompting;
**************
1)24		integer C,B,E; string F; integer array T[0:1];
1)		S _ TMPIN( "PCO", E );	comment SAIL function;
1)		if E then begin
1)		    F _ cvs( 1000+Parent )[2 to 4]&"PCO.TMP";
1)		    open(C_getchan,"DSK",1,4,0,256,B,E);
1)		    lookup(C,F,E);
1)		    if E then print("?cant read ",F," error code '",E,
1)					EXIT( !Xwd(Error!RCF,!rh(E)) )  );
****
2)24		integer C,B,E; string File; integer array T[0:1];
2)		S _ TMPIN( "PCO", E );	comment SAIL function;
2)		if E then begin
2)		    File _ cvs( 1000+Parent )[2 to 4]&"PCO.TMP";
2)		    open(C_getchan,"DSK",1,4,0,256,B,E);
2)		    lookup(C,File,E);
2)		    if E then print("?cant read ",File," error code '",E,
2)					EXIT( !Xwd(Error!RCF,!rh(E)) )  );
**************
1)24	    if EOF and not Ext!Found then
1)		lookup(IChan,CmdName_CmdFile&Default!Ext,EOF_-1); ! alternate?;
1)	    Ifcr PRELIMINARY thenc
1)		if swPRINT land P$$FIL then
1)		    Print("LOOKUP(",cvs(ichan),") ",CmdName," ",!rh(Eof),crlf);
1)	    endc
1)	    if EOF then begin
1)		lookup(Ichan,CmdName_CmdFile,EOF_-1);	! Get right error code, etc;
1)		print ("?input file """,CmdFile,""" ",case !rh(EOF) of (
1)		    [0] "not found",
****
2)24	    FileInfo( F );				! Find the right file;
2)	    if EOF and not Ext!Found
2)	     then lookup(IChan,CmdFile&Default!Ext,EOF_-1); ! alternate?;
2)	    if EOF
2)	     then begin
2)		print ("?input file """,CmdFile,""" ",case !rh( F[2] ) of (
2)		    [0] "not found",
**************
1)24		    [else] "LOOKUP error '"&cvos(!rh(EOF)) ),crlf);
1)		release(IChan);
1)		if IACSW then go to INPUT.PROMPT
1)			 else EXIT( !Xwd( Error!IFE,!rh(EOF) ) );
1)	    end;
1)	    CmdFile _ CmdName;			! Keep the name straight;
1)	    Status  _ Status!LOOK;		! Confirm file can be found;
****
2)24		    [else] "LOOKUP error '"&cvos(!rh( F[2] )) ),crlf);
2)		release(IChan);
2)		if IACSW then go to INPUT.PROMPT
2)			 else EXIT( !Xwd( Error!IFE,!rh( F[2] ) ) );
2)	     end
2)	     else FileInfo( F );
2)	    CmdFile _ "(" & CvName(F[4]) & ")" & Cv6str(F[1]) &
File 1)	DSK:PCOM57.SAI	created: 0514 30-MAR-83
File 2)	DSK:PCOM60.SAI	created: 0515 30-MAR-83

2)		      (if !lh(F[2]) neq 0
2)		        then "." & cv6str(F[2] land (-1 lsh 18))
2)			else "");		! Keep the name straight;
2)	    Status  _ Status!LOOK;		! Confirm file can be found;
**************
1)24		  Integer Array F[1:6];
1)		  Lookup(Ochan, LogFileName, Eof_-1);	! see if file exists;
****
2)24		  Lookup(Ochan, LogFileName, Eof_-1);	! see if file exists;
**************
1)24		Ifcr PRELIMINARY thenc
****
2)24		FileInfo( F );
2)		LogFileName _ "(" & CvName(F[4]) & ")" & Cv6str(F[1]) &
2)			      (if !lh(F[2]) neq 0
2)				then "." & cv6str(F[2] land (-1 lsh 18))
2)				else "");		! Keep the name straight;
2)		Ifcr PRELIMINARY thenc
**************
1)27		    " command processed: "&
1)		    (If MakeLog then LogFilename&" = " else "")&! Log file?;
****
2)27		    " - processed: "&
2)		    (If MakeLog then LogFilename&" = " else "")&! Log file?;
**************
1)27		OutPtr(Port,"; Job limits: "&
1)		    Print!switch("TIME")&" "&			! TimeLimit;
****
2)27		OutPtr(Port,"; Limits: "&
2)		    Print!switch("TIME")&" "&			! TimeLimit;
**************
1)27		OutPtr(Port,"; Job status: "&disposition&"."&#cr);
1)		If MakeLog then OutPtr(Port, "; Output recorded in file "&
****
2)27		OutPtr(Port,"; Status: "&disposition&"."&#cr);
2)		If MakeLog then OutPtr(Port, "; Output recorded in file "&
**************
  