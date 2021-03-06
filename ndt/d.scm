File 1)	DSK:PHOTO.SAI	created: 1556 22-FEB-83
File 2)	DSK:PHOTO.OLD	created: 1557 22-FEB-83

1)1	require 2 version;
1)	COMMENT version history: 
1)	2	add "HALT before PHOTO" option.
1)	1	(or none) first version
1)	;
1)		IFCR declaration(kequ)=0 THENC
1)			simple procedure setUpperCase; ttyup(true);
1)			require setUpperCase initialization;
1)			define kequ="equ";
1)		ENDC
1)	require "(SAILIB)SAIL.DEF" source!file;
1)	!get!module(mapping);
1)	define CALLI!VCLEAR =-'66, CALLI!SETOTF=-'126;
1)	! frame ops:;
1)	define	!FOCON='25, !FOHST='23, !FODFH='13, !FOCFH='12, 
1)		!FOREP=7, !FORPC=6, !FOHLT=5, !FORVA=2;
1)	! ac cess rights:;
1)	define	!arRDM='0,!arCHM='1,!arRAC='2,!arWAC='3,!arVAR='4,!arVAW='5,
1)		!arHLT='6,!arSTP='7,!arHNG='10,!arCLR='11,!arSVA='12,
1)		!arSAA='13,!arRVA='14,!arRAA='15,!arRUN='16,!arRDS='17,
1)		!arRAD='20,!arRFI='21,!arRPI='22,!arSMF='23,!arWAK='24,
1)		!arATT='25,!arDET='26,!arTKP='27,!arGVP='30,!arGVC='31;
1)	! frame descriptors:;
1)	define	FD!ABS=0, FD!FAM=1, FD!OTF=2, FD!CRH=3, FD!SLF=4, 
1)						CH!FRM=0, CH!PRO=1, CH!LIC=2, 
1)		CREATED!HANDLE	= FD!CRH*!bit(20),  
1)		OTHER!FRAME	= FD!OTF*!bit(20);
1)	! gettab tables:;
1)	define	!GTUNM=-'22, !GTUN1=-'21, !GTSTS='0, !GTNAM='3;
1)	! chanio opcodes:;
1)	define	!CHFTR='45;
1)	define routine = "simple procedure";
1)	! for debugging, use: let routine = procedure;
1)	define	JobMappingPage='377; 
****
2)1	require "(SAILIB)SAIL.DEF" source!file;
2)	!get!module(mapping);
2)	define other!frame= 2*!bit(20);
2)	define calli!vclear =-'66, calli!setotf=-'126;
2)	define	!FOHST='23, !FODFH='13, !FOCFH='12, 
2)		!FOREP=7, !FORPC=6, !FORVA=2,
2)		FD!ABS=0, FD!FAM=1, FD!OTF=2, FD!CRH=3, FD!SLF=4, 
2)						CH!FRM=0, CH!PRO=1, CH!LIC=2; 
2)	define	!GTUNM=-'22, !GTUN1=-'21, !GTNAM='3;
2)	define	!CHFTR='45;
2)	let routine = procedure;
2)	define	JobMappingPage='377; 
**************
1)3			!XWD(CH!LIC,'077777) + CREATED!HANDLE, 0;
1)			own safe integer array typeAndRights[0:1];
****
2)3			!XWD(2,'77777) + FD!CRH*!bit(20), 0;
2)			own safe integer array typeAndRights[0:1];
**************
File 1)	DSK:PHOTO.SAI	created: 1556 22-FEB-83
File 2)	DSK:PHOTO.OLD	created: 1557 22-FEB-83

1)3	boolean routine destroyHandle( reference integer handle );
1)	if FRMOPV( !FODFH, handle, 0, !skip! )
1)	 then begin handle := 0; return(true) end
1)	 else return(false);
1)	integer routine readPC( integer handle );
****
2)3	boolean routine destroyHandle( integer handle );
2)		return( FRMOPV( !FODFH, handle, 0, !skip! ) );
2)	integer routine readPC( integer handle );
**************
1)3		if frmopr( !FORVA, handle, ac, !SKIP! )
1)		 then begin !SKIP!:=0; return(handle) end else return(-1);
1)	boolean routine MapHis( integer myPage, hisPage, handle);
1)		begin
1)		hisPage := hisPage lor !bit(1);	! bit => page in other frame;
1)		myPage := myPage lor (1*!bit(7) + 1*!bit(17));
****
2)3		begin
2)		if frmopr( !FORVA, handle, ac, !SKIP! )
2)		 then return(handle) else return(-1);
2)		end;
2)	boolean routine MapHis( integer myPage, hisPage, handle);
2)		begin
2)		hisPage := hisPage lor !bit(1);
2)		myPage := myPage lor (1*!bit(7) + 1*!bit(17));
**************
1)4		[3] "Address check for device " & CV6STR(device) & ":",
1)		[4] "Illegal Memory Reference in UUO",
****
2)4		[3] "Address check for device " & CVXSTR(device) & ":",
2)		[4] "Illegal Memory Reference in UUO",
**************
1)4	       ) & " device " & CV6STR(device) & ":",
1)	   [3] case subCode of (
****
2)4	       ) & " device " & CVXSTR(device) & ":",
2)	   [3] case subCode of (
**************
1)4		[3] CV6STR(device) & ":" & CV6STR(dw1) & " setup",
1)		[4] CV6STR(device) & ":" & CV6STR(dw1) & " saved",
1)		[5] "Frame cleared",
****
2)4		[3] CVXSTR(device) & ":" & CVXSTR(dw1) & " setup",
2)		[4] CVXSTR(device) & ":" & CVXSTR(dw1) & " saved",
2)		[5] "Frame cleared",
**************
1)4		[2] "Device " & CV6STR(device) & ": not available ",
1)		[3] "file " & CV6STR(device) & ":" & CV6STR(dw1) 
1)			& "." & CV6STR(dw2) & " not found ",
1)		[4] "file " & CV6STR(device) & ":" & CV6STR(dw1) 
1)			& "." & CV6STR(dw2) & " not a Save File",
1)		[5] "file " & CV6STR(device) & ":" & CV6STR(dw1) 
1)			& "." & CV6STR(dw2) & " attempt to ENTER failed",
1)		[6] "Core Argument not specified for magtape " 
File 1)	DSK:PHOTO.SAI	created: 1556 22-FEB-83
File 2)	DSK:PHOTO.OLD	created: 1557 22-FEB-83

1)			& CV6STR(device) & ":",
1)		[7] "Magtape Hiseg format bad on " & CV6STR(device) & ":",
1)		[8] "REMAP UUO error",
1)		[9] "Magtape GET failure on " & CV6STR(device) & ":",
1)		[10] "No Start Address",
****
2)4		[2] "Device " & CVXSTR(device) & ": not available ",
2)		[3] "file " & CVXSTR(device) & ":" & CVXSTR(dw1) 
2)			& "." & CVXSTR(dw2) & " not found ",
2)		[4] "file " & CVXSTR(device) & ":" & CVXSTR(dw1) 
2)			& "." & CVXSTR(dw2) & " not a Save File",
2)		[5] "file " & CVXSTR(device) & ":" & CVXSTR(dw1) 
2)			& "." & CVXSTR(dw2) & " attempt to ENTER failed",
2)		[6] "Core Argument not specified for magtape " 
2)			& CVXSTR(device) & ":",
2)		[7] "Magtape Hiseg format bad on " & CVXSTR(device) & ":",
2)		[8] "REMAP UUO error",
2)		[9] "Magtape GET failure on " & CVXSTR(device) & ":",
2)		[10] "No Start Address",
**************
1)5	define !FE!NRT!='3,!FE!NLC!='5,!FE!ALR!='20,!FE!ALH!='21,!FE!CSJ!='22; 
1)	string procedure FRMOP!error( integer code );
1)	begin	define FrmopErrorMax='36;
1)		preset!with	"No handle in slot"
1)			,"Bad (nonmatching) universal ID number"
1)			,"No rights to do operation"
1)			,"Bad frame number"
1)			,"Not enough license"
1)			,"Undefined frame descriptor type"
1)			,"Not a child"
1)			,"Cannot lock context pages"
1)			,"Bad access rights code"
1)			,"No parent"
1)			,"Bad family dispatch type"
1)			,"Bad handle index number in FD"
1)			,"Bad FRMOP function code"
1)			,"Address bad: replicate failed (.FORVA or .FOWVA)"
1)			,"Page I/O error"
1)			,"Already running"
1)			,"Already halted"
1)			,"Cannot stop frame with JACCT set"
1)			,"Handle descriptor expected not given"
1)			,"Handle (index) already used"
1)			,"No frame handle indices left"
1)			,"Bad count"
1)			,"Bad table number"
1)			,"Bad handle type"
1)			,"Cannot create handle (access failure)"
1)			,"Bad start vector offset"
1)			,"Cannot make child its own inferior in frame tree"
1)			,"Cannot continue frame"
1)			,"Cannot do frame jump: target frame not in user mode"
1)			,"Rights in target handle exceed maximum specifications"
1)		;OWN SAFE STRING ARRAY FrmopErrorText[1:FrmopErrorMax];
File 1)	DSK:PHOTO.SAI	created: 1556 22-FEB-83
File 2)	DSK:PHOTO.OLD	created: 1557 22-FEB-83

1)	return( if 0<code<FrmopErrorMax
1)		 then FrmopErrorText[code]
1)		 else "unknown FRMOP error #'" & cvos(code)
1)	);
1)	end;
1)6	string routine showPC( integer pc );
****
2)5	string routine showPC( integer pc );
**************
1)6	 then "Last Halt (" & cvs(HSB[2]) & "):" 
1)		& HaltStatus(HSB[1], HSB[5],HSB[6],HSB[7])
1)		& " at " & showPC(HSB[3]) 
1)		& " last UUO at " & showPC(HSB[4]) 
1)	 else "Cannot read HSB"
****
2)5	 then "Halt #" & cvs(HSB[2]) & ":" 
2)		& HaltStatus(HSB[1], HSB[5],HSB[6],HSB[7])
2)		& " at PC=" & showPC(HSB[3]) 
2)		& " last UUO at PC=" & showPC(HSB[4]) 
2)	 else "Cannot read HSB"
**************
1)7		return( cv6str( gettab(frame,!gtunm) ) 
1)			& cv6str( gettab(frame,!gtun1) ) );
1)	string procedure readprogram( integer frame );
1)		return( cv6str( gettab(frame,!gtnam) ) );
1)8	boolean routine agrees( string question;
1)				integer defaults(0);
1)				string help(null) );
1)	! asks question and returns true if yes, false if no. (prints help if ?)
1)	! defaults:  -1 => <CR>=true, 0 => <CR>=false, 1 => must reply
1)	;
1)	while true do
1)	begin	string reply;
1)	print(question, ": ");
1)	if length(reply := inchwl)=0 
1)	 then if defaults=1
1)		 then print("you must reply Yes or No" & crlf)
1)		 else return(defaults NEQ 0)
1)	 else if kequ(reply, "YES"[1 for length(reply)]) then return(true)
1)	 else if kequ(reply, "NO"[1 for length(reply)]) then return(false)
1)	 else if length(help)=0 then print("Answer Yes or No",
1)					(if defaults=1 then null
1)					 else ", <CR> means " & 
1)					    (if defaults then "Yes" else "No")),
1)					"." & crlf )
1)	 else if length(help)<200
1)	       or reply="?" 
1)	       or kequ(reply,"HELP"[1 for length(reply)]) then print(help, crlf)
1)	 else print("Try ?",crlf);
1)	end;
1)9	string lastUser, lastProgram;
1)	routine showStatus;
1)	print( crlf & " user ", lastUser := readuser( other!frame ),
1)		" running ", lastProgram := readprogram( other!frame ), 
File 1)	DSK:PHOTO.SAI	created: 1556 22-FEB-83
File 2)	DSK:PHOTO.OLD	created: 1557 22-FEB-83

1)		", PC = ", showPC( readPC( other!frame ) ),
1)		crlf & "  ", readHSB( other!frame ), crlf&crlf );
1)10	integer jobHandle;
1)	integer routine makeOtherFrame( integer job );
****
2)6		return( cvxstr( gettab(frame,!gtunm) ) 
2)			& cvxstr( gettab(frame,!gtun1) ) );
2)	string procedure readprogram( integer frame );
2)		return( cvxstr( gettab(frame,!gtnam) ) );
2)7	integer routine makeOtherFrame( integer job );
**************
1)10		jobHandle := handle;
1)		calli( handle, calli!setotf );
1)		if !skip!
1)		 then begin
1)			showStatus;
1)			if -1=readAc(1,other!Frame) and !skip! neq 0
1)			 then print("Sorry, can't even read Ac 1" & crlf)
1)			 else if agrees( "Is " & lastUser & "'s job " & cvs(job)
1)					& " [" & lastProgram 
1)					& "] the frame you want to photo?", -1 )
1)			       then return( JobMappingPage lsh 9 );
1)					! location of base of page to map;
1)		      end
1)		 else print("Cannot set it up as my `other frame'." & crlf);
1)		destroyHandle(jobHandle);
1)		return(false);
1)		end;
1)	routine unmapJobPage; 
****
2)7		calli( handle, calli!setotf );
2)		if not !skip!
2)		 then begin destroyHandle(handle); return(false); end;
2)		
2)		print( " PC = ", showPC( readPC( other!frame ) ),
2)			crlf, " user ", readuser(job),
2)				" running ", readprogram(job),
2)			crlf, " Halt status = ", readHSB( other!frame ),
2)			crlf, "Is that the one? " );
2)		if 0 neq ((inchwl+0) land '137) neq "Y"
2)		 then begin destroyHandle(handle); return(false); end;
2)		return( JobMappingPage lsh 9 );
2)				! location of base of page to map;
2)		end;
2)	integer routine readJobsAcs( integer ac );
2)		return( readAc( ac, other!frame ) );
2)	routine unmapJobPage; 
**************
1)11	procedure saveOtherFrame( integer jobPage, chan; r!p(map1) pageHolder );
1)	begin	r!p(map1) OtherPageHolder;	! just for efficiency;
1)		integer ac, page;
1)		integer lastExtant, lastNonExtant; 
1)		boolean wasExtant,nextExtant, hadZero;
1)	OtherPageHolder := map1cr(0,511);
File 1)	DSK:PHOTO.SAI	created: 1556 22-FEB-83
File 2)	DSK:PHOTO.OLD	created: 1557 22-FEB-83

1)	for ac := 0 step 1 until '17
1)	   do map1:a[pageHolder][ac] := readAc( ac, other!frame );
1)	wasExtant := 0 neq mapJobPage(0);
****
2)8		integer job, chan, ac, page, JobPage;
2)		r!p(map1) PageHolder, OtherPageHolder; 
2)		integer lastExtant, lastNonExtant; 
2)		boolean wasExtant,nextExtant;
2)	do print(" Job number to get a photo of: ")
2)	 until JobPage := makeOtherFrame( job := cvd(inchwl) );
2)	do print(" File to store photo on: ")
2)	 until 0 LEQ chan := opfile( inchwl, "WVM", 16 );
2)	pageHolder := map1cr(0,511);
2)	OtherPageHolder := map1cr(0,511);
2)	getmap( pageHolder, "N", chan, 1 );
2)	for ac := 0 step 1 until '17
2)	   do map1:a[pageHolder][ac] := readJobsAcs(ac);
2)	wasExtant := 0 neq mapJobPage(0);
**************
1)11	if hadZero := wasExtant
1)	 then arrblt( map1:a[pageHolder]['20], memory[JobPage+'20], 512-'20 );
1)	print("[0");
****
2)8	if wasExtant
2)	 then arrblt( map1:a[pageHolder]['20], memory[JobPage+'20], 512-'20 )
2)	 else usererr(0,1,"No job page 0?");
2)	print("[0");
**************
1)11			 then begin
1)				chniov( chan, page lsh 9, !CHFTR ); 
1)				! try to extend the file (save creating pages);
1)				! if it works, dont bother w/ make/destroy;
1)				if not !skip! then ! somehow trick worked?;
1)				 for lastExtant:= lastExtant+1 step 1 until page
1)				 do begin ! WRS speedup: (save SAT search);
1)				    getmap(pageHolder, "N", chan, lastExtant ); 
1)				    getmap(null!r, "D", chan, lastExtant ); 
1)				    end;
1)				wasExtant := true;
1)			      end;
1)			getmap( pageHolder, "N", chan, page + 1 );
****
2)8			 then chniov( chan, page lsh 9, !CHFTR ); 
2)				! try to extend the file (save creating pages);
2)				! if it doesn't work, who cares;
2)			getmap( pageHolder, "N", chan, page + 1 );
**************
1)11			lastExtant := page;
****
2)8			if not wasExtant
2)			 then begin	integer fpage;
2)				for fpage := lastExtant+1 step 1 until page
2)				   do getmap( null!r, "D", chan, fpage ); 
2)				wasExtant := true;
File 1)	DSK:PHOTO.SAI	created: 1556 22-FEB-83
File 2)	DSK:PHOTO.OLD	created: 1557 22-FEB-83

2)			      end;
2)			lastExtant := page;
**************
1)11	unmapJobPage;
1)	if not hadZero
1)	 then if lastExtant < 1
1)	   then begin		! since only ACs, clip the file length back;
1)		chniov( chan, '20, !CHFTR );		
1)		if not !skip! then print(crlf & "Can't clip file?");
1)		print(crlf & "No memory pages, file just has Acs!" & crlf);
1)		end
1)	   else print(crlf & "
1)	Frame had some core pages, but no page 0?  
1)	The first file page was created anyway to show the ACs (it is 0-padded)
1)	" );
1)		
1)	getmap( OtherPageHolder, "U", 0,0 );
1)	getmap( pageHolder, "U", 0,0 );
1)	end;
1)12	integer procedure attemptHalt;
1)	! returns a halt status code if successful, message and 0 otherwise;
1)	begin	integer error;  
1)	if GETTAB(other!frame,!gtSTS) GEQ 0 or not !skip! ! frame not running;
1)	 then print(crlf, "Frame is already Halted")
1)	 else if FRMOPV( !FOHLT, other!frame, 0, error )
1)	 then begin ! successful;
1)			integer attempts;
1)			preload!with 2; own safe integer array HaltStatus[0:2];
1)		do for attempts := 1 step 1 until 10
1)		    do if GETTAB(other!frame,!gtSTS)<0
1)			 then return( if frmopr( !FOHST, other!frame, 
1)							HaltStatus[0], !SKIP! )
1)					 then 1 + HaltStatus[2]
1)					 else -1 )
1)			 else calli(1,"SLEEP")
1)		 until agrees("The HALT-frame was successful, " 
1)			& "but the frame is still running." & crlf 
1)			& "Forget about waiting for the HALT to take effect?",
1)			-1 );
1)		print(crlf& "Frame-HALT ignored (forgotten)");
1)	      end
1)	 else print(crlf & "Cannot halt frame because: ", FRMOP!error(error) );
1)	return(0);
1)	end;
1)	procedure letgo( integer haltSequence );
1)	if haltSequence=0 then usererr(0,1,"I didn't halt him!") else
1)	begin	integer error;
1)	if GETTAB(other!frame,!gtSTS) < 0 and !SKIP!
1)	 then begin
1)		print(crlf& "Frame is running again (by itself?).  "
1)			& "No Continue done.");
1)		return;
1)	      end;
1)	if haltSequence > 0
File 1)	DSK:PHOTO.SAI	created: 1556 22-FEB-83
File 2)	DSK:PHOTO.OLD	created: 1557 22-FEB-83

1)	 then begin	! make sure same halt status number.;
1)			preload!with 2; own safe integer array HaltStatus[0:2];
1)		if frmopr( !FOHST, other!frame, HaltStatus[0], !SKIP! )
1)		and haltSequence neq 1 + HaltStatus[2]
1)		 then begin
1)			print("
1)	The halt status number is not the same as when the frame was halted!
1)	The number after I halted it was: ", Haltsequence-1, 
1)				", while now it is ", HaltStatus[2], "." );
1)			If not agrees( "Shall I CONTINUE the frame anyway?", 1 )
1)			 then return;
1)		      end;
1)	      end;
1)	if not FRMOPV( !FOCON, other!frame, 0, error )
1)	 then print(crlf & "Cannot continue frame because: ", 
1)			FRMOP!error( error ) );
1)	end;
1)13		integer chan, whereMapped, isHalted;
1)		r!p(map1) forFirst; 
1)	do print(" Job number to get a photo of: ")
1)	 until whereMapped := makeOtherFrame( cvd(inchwl) );
1)	do print(" File to store photo on: ")
1)	 until 0 LEQ chan := opfile( inchwl, "WVM", 16 );
1)	forFirst := map1cr(0,511);
1)	getmap( forFirst, "N", chan, 1 );
1)	if isHalted := agrees("Shall I to try to HALT him during the photo?",0,
1)				"This makes a better picture, "
1)				& "but requires more license " & crlf
1)				& "(and is more disruptive). <CR> means No." )
1)	 then isHalted := attemptHalt;
1)	showStatus;
1)	saveOtherFrame( whereMapped, chan, forFirst );
1)	print("]
1)	Photo complete.");
1)	showStatus;
1)	if isHalted
1)	 then begin
1)		letgo(isHalted);
1)		showStatus;
1)	      end;
1)	destroyHandle(jobHandle);
1)	chniov( chan, 0,0 );	! is basically release( chan );
1)	end "jobget" .
****
2)8	print("]");
2)	unmapJobPage;
2)	getmap( pageHolder, "U", 0,0 );
2)	getmap( OtherPageHolder, "U", 0,0 );
2)	chniov( chan, 0,0 );	! is basically release( chan );
2)	end "jobget" .
**************
 