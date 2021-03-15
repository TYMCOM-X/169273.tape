entry
	Token,  SubSet, LukLud, SetOut, OpenFile, EnterFile, 
	OpnMAP, OpnMSC, OpnGDF, OpnFDF, ClsMAP, ClsMSC, ClsGDF, ClsFDF,
	GetLoc, AskLoc, PtrLoc, GetNod, AskNod, DateSt, Daytim,
	Ask,    Since,  IsFrom, LicOk,  DecLic, IncLic,
	GetGDF, GetFDF, GetMap, FndMap, UpdCnt, FDFlst, FDFdel,
	SetOpt, PrtOpt, ReqPrt, SupCpy,
	GetFil, GetReq, GetUsr, GetFrm, GetLst
;
begin "SPL subroutines"

require "(SAILIB)SAIL.DEF" source!file;
require "UUOSYM.DEF" source!file;
require "SPLDEF.DEF" source!file;
require "SPLFMT.DEF" source!file;
require "SPL.DCL"    source!file;
require "SPLARR.DCL" source!file;
require "SPLPAG.DCL" source!file;
require "SPLLIC.DCL" source!file;
require "SPLDAY.DCL" source!file;
require "SPLSTR.DCL" source!file;


Own String  Text, Txt;
Own Integer UseLoc, UseCount, PrintChan, Eof, Err, Flag;
Own Integer User1, User2, HashValue, LudIndex, LudBlock;
Own Integer GdfChn, FdfChn, MapChn, MscChn;
Own Integer GdPage, FdPage, MapPage, MscPage;
Own Integer GDMapped, FDMapped, MapMapped, MscMapped;
Own Integer GdfAccess, FdfAccess, MapAccess, MscAccess;
Own safe Integer array FileBlock[1:6];


internal simple string procedure Token (reference string S);
! ----------------------------------------------------------------------;
!									;
!	Token		Scan the string S and return the first word	;
!			or punctuation character to the caller.		;
!									;
! ----------------------------------------------------------------------;
begin
    string S1;
    Scan( S, BrkWht, Brk );	! clear whitespace;
    S1 _ scan(S, BrkBrk, Brk);	! get the next word;
    If length(S1) = 0		! null?;
	then if length(S) = 0	!   null source also?;
	    then return (null)	!    yes - return null;
	    else S1 _ lop(S);	!    no  - get a character;
!   Scan( S, BrkWht, Brk );	! comment >> clear whitespace <<;
    Return(S1);			! return the token;
end;


internal simple boolean procedure SubSet(String Str, Sub);
! ----------------------------------------------------------------------;
!									;
!	SubSet		Return TRUE if all the characters in string	;
!			"STR" are some subset of those in string "SUB".	;
!									;
! ----------------------------------------------------------------------;
begin "SubSet"
    Integer C,D,E;
    If Length( Str ) = 0 or Length( Sub ) = 0 then Return( False );
    While Length( Str )
     do begin
       C_ Lop(Str);  E_ False;
       For D_ 1 step 1 until Length( Sub )
	 do If C = Sub[D for 1] then E_ True;
       If not E then Return( False );
     end;
    Return( True );
end;

Simple Integer Procedure LudHash;
! ----------------------------------------------------------------------;
!									;
!	LudHash		Returns the "hashed" value of the username so	;
!			that the user may be located in the LUD more	;
!			quickly.					;
!									;
! ----------------------------------------------------------------------;
Start!code
    define T1=1, T2=2, T3=3, T4=4, A=5, B=6, C=7, D='10, J='13, K='14;
    label Hash, Hash1, Rnd1, Rnd2;
	MOVE	T1,USER1;
	MOVE	T2,USER2;
	SETZ	A,;
	MOVE	B,['555555555555];
	MOVE	C,['361275431652];
	MOVE	D,['612754316523];
	MOVEI	J,4;			! times to loop;
HASH1:	ADD	C,T1;
	ROTC	T1,-'22;
	MOVEI	K,5;
RND1:	MOVE	T3,C(A);
	MUL	T3,[5*5*5*5*5*5*5*5*5*5*5*5*5*5*5];
	ADDM	T4,D(A);
	AOJE	A,RND2;
	MOVNI	A,1;
	TRNE	C,1;
	SKIPL	D;
	MOVEI	A,0;
	EXCH	B,D;
RND2:	SOJG	K,RND1;
	SOJG	J,HASH1;
	XOR	D,C;
	MOVE	T1,D;
	TLZ	T1,'400000;
	IDIVI	T1,887;
	MOVEI	T2,1(T2);
	MOVEM	T2,LudBlock;
	MOVE	T1,B;
	XOR	T1,D;
end;


internal simple boolean procedure OpenFile(
			Reference Integer Chn, Flg; String Name );
! ----------------------------------------------------------------------;
!									;
!	OpenFile	Open the given file NAME on the chanel Chn	;
!			without allocating any input or output buffers.	;
!			This call is primarily for use in obtaining	;
!			a proper chanel for the designated file so	;
!			that the file may be MAPPED into memory via	;
!			other monitor calls.				;
!									;
! ----------------------------------------------------------------------;
begin
    Open( Chn_getchan,"DSK",'17,0,0,0,0,Eof_-1 );
    Lookup( Chn, Name ,Eof_-1);
	If Eof then begin "file lookup error"
	    If Flg
	     then begin
		Close( Chn );
		Enter( Chn, Name, Eof_-1);
		If Eof
		 then Print("File ",Name,
			    " enter error (",cvos(!rh(Eof)),")",crlf)
		 else begin Close( Chn ); Lookup( Chn, Name, Eof_-1) end
	     end
	     else Print("File ",Name," not found (",cvos(!rh(Eof)),")",crlf);
	    If Eof then Return( False );
	end "file lookup error";
    Return( True );
end;

internal simple integer procedure EnterFile( Integer Chn, Exc; procedure Recovery );
! ----------------------------------------------------------------------;
!									;
!	EnterFile	Perform the same operations as OpenFile, but	;
!			do an ENTER or M-ENTER on the file so that it	;
!			is writeable.  This routine is used whenever	;
!			we have to map a page in a file for read-write.	;
!									;
! ----------------------------------------------------------------------;
begin
    FileInfo( FileBlock );			! get info from last lookup;
    Chnior( Chn, FileBlock[1],If Exc		! determine if normal;
		 then !chENT else !chMEN);	!   or multiple enter;
    If !skip! then Return( True )		! return enter success;
     else begin Recovery; Return( False ); end	!  (if false - do closeup);
end;

internal simple procedure ClsMAP;
begin
    Calli( !Xwd('2001, (MAPadr lsh -9)), Calli!VCLEAR );
    MapAccess_ 0;	MapMapped_ -1;
    Close( MapChn );	Release( MapChn );
end;
internal simple boolean procedure OpnMAP( Integer Prot, Exclusive(False) );
begin
    If Prot < M!Read or Prot > M!Write		! ? protection in range;
     then Return( False );			!  no - give error;
    MapMapped_ -1;  MapAccess_ Prot;		! page #, protection;
    Flag_ OpenFile( MapChn, False, MapFile );	! ? open file;
    Return( If Flag and Prot > M!Read		! ? ok ? writing file;
	   then EnterFile( MapChn, Exclusive, ClsMAP )
	   else Flag );
end;


internal simple procedure ClsMSC;
begin
    Calli( !Xwd('2001, (Mscadr lsh -9)), Calli!VCLEAR );
    MscAccess_ 0;	MscMapped_ -1;
    Close( MscChn );	Release( MscChn );
end;

internal simple boolean procedure OpnMSC(
	String FilNam; Integer Prot, OkNonExist(False), Exc(False) );
begin
    If Prot < M!Read or Prot > M!Write
     then Return( False );
    MscAccess_ Prot;    MscMapped_ -1;
    Flag_ OpenFile( MscChn, OkNonExist, FilNam );
    Return( If Flag and Prot > M!Read
	     then EnterFile( MscChn, Exc, ClsMSC )
	     else Flag )
end;

internal simple procedure ClsGDF;
begin
    Calli( !Xwd('2001, (GDFadr lsh -9)), Calli!VCLEAR );
    GdfAccess_ 0;	GDMapped_ -1;
    Close( GdfChn );	Release( GdfChn );
end;

internal simple boolean procedure OpnGDF( Integer Prot, Exclusive(False) );
begin
    If Prot < M!Read or Prot > M!Write
     then Return( False );
    GdfAccess_ Prot;    GDMapped_ -1;
    Flag_ OpenFile( GdfChn, False, GDFile );
    Return( If Flag and Prot > M!Read
	     then EnterFile( GdfChn, Exclusive, ClsGDF )
	     else Flag )
end;


internal simple procedure ClsFDF;
begin
    Calli( !Xwd('2001, (FDFadr lsh -9)), Calli!VCLEAR );
    FdfAccess_ 0;	FDMapped_ -1;
    Close( FdfChn );	Release( FdfChn );
end;

internal simple boolean procedure OpnFDF( Integer Prot, Exclusive(False) );
begin
    If Prot < M!Read or Prot > M!Write
     then Return( False );
    FdfAccess_ Prot;    FDMapped_ -1;
    Flag_ OpenFile( FdfChn, False, FDFile );
    Return( If Flag and Prot > M!Read
	     then EnterFile( FdfChn, Exclusive, ClsFDF )
	     else Flag )
end;

internal integer procedure LukLud(String User; Reference Integer Dist, Priv );
! ----------------------------------------------------------------------;
!									;
!	LukLud		Read through the LUD looking for "user" and	;
!			return the district and privilege word for	;
!			that user.					;
!									;
! ----------------------------------------------------------------------;
begin "Read LUD"
    If not OpnMSC( LudFile, M!Read )		! Open Lud /Read;
      then Return(2);				! file not available;
    If Length( User )				! null user is requestor;
     then begin
	User1_ CvSIX( User[1 for 6] );
	User2_ CvSIX( User[7 for 6] );
     end
     else begin
	User1_ Calli( !Xwd(-1,!GTUNM),calli!gettab );
	User2_ Calli( !Xwd(-1,!GTUN1),calli!gettab );
     end;
    HashValue_ LudHash;				! hash the username;
						! this also sets LudBlock;
    While TRUE do begin
	If not PageMp(MscChn, (LudBlock+3) lsh -2, MscAdr lsh -9, M!Read)
	 then begin ClsMSC;  Return( 3 ); end;
	LudIndex_ ((LudBlock-1) mod 4) * '200;
	While MSCrec[ LudIndex ] > 0 do begin
	    If MSCrec[ LudIndex+L!HASH ] = HashValue then begin
		Dist _ MSCrec[ LudIndex+L!DIST ] land '377;
		Priv _ MSCrec[ LudIndex+L!PRV ];
		ClsMSC;  Return( -1 );
	     end
	     else LudIndex_ (MSCrec[ LudIndex+L!SIZE ] land '177) + LudIndex;
	 end;
	If MSCrec[ LudIndex ] < 0
	 then LudBlock_ !rh( MSCrec[ LudIndex ] )
	 else Done;
     end;
    Dist_ Priv_ 0;
    ClsMSC;  Return( 0 );
End "Read LUD";

internal simple procedure DecLic;
! ----------------------------------------------------------------------;
!									;
!	DecLic		Reduce process license to user Frame license.	;
!									;
! ----------------------------------------------------------------------;
Calli( !Xwd( !rh(FrmLic),!rh(FrmLic) ), calli!SETLIC );

internal simple procedure IncLic;
! ----------------------------------------------------------------------;
!									;
!	IncLic		Increase the process license to original state.	;
!									;
! ----------------------------------------------------------------------;
Calli( FrmLic, calli!SETLIC );


simple procedure MyPrint( Integer Chan; String Str );
! ----------------------------------------------------------------------;
!									;
!	MyPrint		Routine to "replace" print when we want to	;
!			do more than "SETPRINT" will allow.		;
!									;
! ----------------------------------------------------------------------;
Out( If Chan = -1 then PrintChan else Chan, Str );

Internal simple integer procedure SetOut(Integer Chan(-1) );
! ----------------------------------------------------------------------;
!									;
!	SetOut		Routine to actually modify GOGTAB to use my	;
!			own "print" routine whenever anything calls	;
!			the regular print routine.			;
!									;
! ----------------------------------------------------------------------;
begin
    require "(FTSYS)GOGTAB.DEF" source!file;
    External Integer array GOGTAB[0:$$PROU];
    Integer OldChan;
    Outstr( "PRT: "&cvos(Gogtab[$$PROU]) );
    OldChan_ If GOGTAB[$$PROU] neq 0
	      then PrintChan else -1;
    If Chan > 0
     then begin
	PrintChan_ Chan;
	GOGTAB[$$PROU]_ Location( MyPrint );
     end
     else GOGTAB[$$PROU]_ 0;
    Outstr( ":"&Cvos(gogtab[$$PROU])&" == "&Cvs(OldChan)&Crlf );
    Return( OldChan );
end;


internal simple integer procedure GetLoc(Integer Node);
! ----------------------------------------------------------------------;
!									;
!	GetLoc		Return the printer location associated with	;
!			a specific node.  If the node doesn't have a	;
!			printer location assigned, then return 0.	;
!									;
! ----------------------------------------------------------------------;
begin
    Integer Pri;
    If not OpnMSC( NodPRI, M!Read )
     then Return( 0 );
    Pri_ 0;
    If PageMp( MscChn, (Node lsh -9) + 1,
		     MscAdr lsh -9, M!Read )
     then begin
	Pri_ !lh( MSCrec[Node land '777] );
	If Pri = 0 and LudDist < 10 and Node geq '5000
	 then If PageMp( MscChn, (Node lsh -9)-3,
			   MscAdr lsh -9, M!Read )
	       then Pri_ !lh( MSCrec[Node land '777] );
     end;
    ClsMSC;
    Return( Pri );
end;

internal simple integer procedure AskLoc( String Prompt );
! ----------------------------------------------------------------------;
!									;
!	AskLoc		Prompt the user and ask for a printer location	;
!			number.  The answer should be a decimal number.	;
!									;
! ----------------------------------------------------------------------;
While TRUE
 do begin
    Print( Prompt );
    If Length( Text_ Inchwl ) and SubSet( Text, "0123456789" )
     then begin
	UseLoc_ Cvd( Text );
	If 1 leq UseLoc leq 256
	 then Return( UseLoc )
	 else Print( "Illegal location number." )
     end
     else Print( "Printer location expected." );
    Print( "  Please re-enter line.", Crlf)
 end;


internal simple string procedure PtrLoc(Integer Pri; Reference string Phone);
! ----------------------------------------------------------------------;
!									;
!	PtrLoc		Return a string the postal address associated	;
!			with the specified printer location.		;
!									;
! ----------------------------------------------------------------------;
begin
    Integer L, Idx, Bas;
    Text_ Txt_ Phone_ null;			! Clear results;
    If not OpnMSC( PriAdr, M!Read )		! Open PRIADR file;
     then Return( null );
    If PageMp( MscChn, (Pri lsh -3) + 1,	! Map the appropriate page;
		 MscAdr lsh -9, M!Read )
     then begin "got info page"			! Build Address & Phone;
	Bas_ MscAdr + '100 * (Pri land '7) + 1;	! Bas_ Addr + index + 1;
	Text_ If Pri = 5 then null		! don't do this for lockheed;
		else "TYMSHARE, INC." & Crlf;	! Initial address;
	For L _ 0 step 1 until 3		! get 4 address lines;
	 do begin "get address lines"
	    Txt_ CvmSix(!Xwd('440600,Bas+L*10),60);	! get 60 characters;
	    If not Kequ(Txt,Spaces[1 for 60])	! if we got anything;
	     then Text_ Text & Txt & Crlf;	!  then add to the message;
	 end "get address lines";
	Txt_ CvmSix(!Xwd('440600,Bas+40),60);	! get 60 characters;
	If Kequ(Txt,Spaces[1 for 60])		! if all spaces;
	 then Txt_ null;			!  then replace with null;
     end "got info page";			! Text & Txt now setup;
    Phone_ Txt;					! always reset phone #;
    ClsMSC;					! closeup files;
    Return( Text );				! return location name;
end;

internal simple integer procedure GetNod;	! get the tymnet node;
! ----------------------------------------------------------------------;
!									;
!	GetNod		Return the user's TYMNET node as reported by	;
!			the operating system.				;
!									;
! ----------------------------------------------------------------------;
begin
    Integer T;
    T _ calli( !Xwd( -1,!GTLOG ), calli!GETTAB );
    Return(  ((T lsh -8) land '77) lor ((T lsh -10) land '7700)  );
end;

internal simple integer procedure AskNod( String Prompt );
! ----------------------------------------------------------------------;
!									;
!	AskLoc		Prompt the user and ask for a node number.	;
!			The answer should be an octal number which	;
!			may have a "+" following to designate an	;
!			alternate node map.				;
!									;
! ----------------------------------------------------------------------;
While TRUE
 do begin
    Print( Prompt );
    UseLoc_ 0;
    If Length( Text_ Inchwl ) and SubSet( Text, "01234567 +" )
     then begin
	For UseCount_ 1 step 1 until Length( Text )
	 do begin
	    If "0" leq Text[UseCount for 1] leq "7"
	     then UseLoc_ (UseLoc * '10) + (Text[UseCount for 1] - "0");
	    If "+"  =  Text[UseCount for 1]
	     then UseLoc_ UseLoc + '4000;
	 end;
	If 1 leq UseLoc leq '7777
	 then Return( UseLoc )
	 else Print( "Illegal node number." )
     end
     else Print( "Printer location expected." );
    Print( "  Please re-enter line.", Crlf)
 end;


internal string procedure Daytim( Integer UseTime );
! ----------------------------------------------------------------------;
!									;
!	Daytim		Return the time of day in the form HH:MM:SS	;
!			using 24-hour time from 00:00:00 to 23:59:59.	;
!									;
! ----------------------------------------------------------------------;
begin
    Integer T,HH;
    T _ If UseTime > 0
	 then UseTime div 1000
	 else Calli(0,calli!timer) div 60;
    HH _ T Div 3600;	T  _ T Mod 3600;
    Return( CvS(100 + HH)[2 for 2] & ":" &
	    CvS(100 + (T Div 60))[2 for 2] & ":" &
	    Cvs(100 + (T Mod 60))[2 for 2]  );
end;

internal simple boolean procedure Since;
! ----------------------------------------------------------------------;
!									;
!	Since		Prompt theuser for a date and return true if	;
!			a proper date has been received.		;
!									;
! ----------------------------------------------------------------------;
begin
    Print( "Since? ");
    While "?" = Text_ Inchwl
     do Print( "Please enter a date in the form MM/DD/YY or DD-MMM-YY",
		Crlf, "Since? " );
    Return( 0 leq RsDate_ GetDay( Text, -1 ) );
end;

internal simple boolean procedure Ask(String Prompt);
! ----------------------------------------------------------------------;
!									;
!	Ask		Prompt the user and read a "YES" or "NO"	;
!			response from the terminal.  Then Return	;
!			TRUE if yes, and FALSE if no.			;
!									;
! ----------------------------------------------------------------------;
begin "Ask"
    Print( Prompt ); Text _ Inchwl;
    while Text[1 for 1] = '40 or Text[1 for 1] = '11 do lop(Text);
    while Text[inf for 1] = '40 or Text[inf for 1] = '11
	do Text _ Text[1 to inf-1];
    If (0 < Length(Text) < 4) and Kequ(Text,"YES"[1 for Length(Text)])
	then return(True) else return(False);
end "Ask";


internal simple boolean procedure IsFrom;
! ----------------------------------------------------------------------;
!									;
!	IsFrom		Return True if the user answers "YES" to this	;
!			date, taken from the current GDF record.	;
!									;
! ----------------------------------------------------------------------;
Return( Ask( "From "&Datest(!lh( !GDF( Date ) ))&"? " ) );

internal simple boolean procedure LicOk;
! ----------------------------------------------------------------------;
!									;
!	LicOk		Return True if the given OPER name and password	;
!			provide the user with non-zero license.		;
!									;
! ----------------------------------------------------------------------;
begin
    Print( "Oper name: ");
    While "?" = SvOper_ Inchw
	Print("
Enter your operator license name as it appears in the SETOP file.
Oper name: " );
    Return ( 0 < GetLic( SvOper ) );
end;

internal integer procedure GetGDF( Integer GDF );
! ----------------------------------------------------------------------;
!									;
!	GetGDF		Map the proper GDF page, if necessary, for the	;
!			specified GDF entry number and return a value	;
!			which corresponds to the proper offset within	;
!			the mapped page.				;
!									;
! ----------------------------------------------------------------------;
begin
    If GDMapped neq ((GDF + 3) lsh -2) then	! ? page already mapped;
      If not PageMp(	GdfChn,				! channel;
			GDMapped_ ((GDF+3) lsh -2),	! page;
			GDFadr lsh -9,			! memory addr;
			GdfAccess )			! protection;
	 then Return( -1 );			! ? error;
    Return( 1 + ((GDF-1) mod 4) );		! return block # in page;
end;

internal integer procedure GetFDF( Integer FDF );
! ----------------------------------------------------------------------;
!									;
!	GetFDF		Map the proper FDF page, if necessary, for	;
!			the specified FDF block number and return two	;
!			half-words which correspond to the proper block	;
!			offset within the mapped page and the start-	;
!			index within that block as [Block,,Index].	;
!									;
! ----------------------------------------------------------------------;
begin
    Integer Idx;
    If FDMapped neq ((FDF + 3) lsh -2) then		! ? same page;
      If not PageMp(	FdfChn,				! no, map on chan;
			FDMapped_ ((FDF+3) lsh -2),	!     this block;
			FDFadr lsh -9,			!     to memory;
			FdfAccess )			!     this access;
	 then Return( -1 );				! ? cant, error;
    FDF_ 1 + ((FDF-1) mod 4);				! make page rounded;
    For Idx_ 1 step 1 until 16				! start looking;
     do If FDFrec[FDF,Idx,FDF!Request] lsh 6 =		!   at each entry;
	     !GDF( Request ) lsh 6 and			!   in this block;
	   !rh( FDFrec[FDF,Idx,FDF!Date] ) =		!   for a matching;
	     !lh( !GDF( Date ) )			!   GDF-FDF word;
	  then Return( !Xwd(Fdf, Idx) );		! ? found, return it;
    Print( "Fatal FDF mismatch error -- Aborting.", Crlf );
    Return( -1 );					! else return error;
end;


internal Integer procedure GetMap;
! ----------------------------------------------------------------------;
!									;
!	GetMap		Map the file SPOOL.MAP and return the count	;
!			of the highest legal record in the GDF file.	;
!			Unmap the file when done.			;
!									;
! ----------------------------------------------------------------------;
begin
    Integer C;
    If not OpenFile( MapChn, False, MAPfile )		! ? can open file;
     then Return(0);					!  no -- give error;
    PageMp( MapChn, 1, MAPadr lsh -9, M!Read );		! page 1, read-only;
    C _ MAPrec[ Map!Count ];				! get count;
    Calli(!Xwd('2001, (MAPadr lsh -9)), Calli!VCLEAR);	! clear page;
    Close( MapChn );	Release( MapChn );		! clean up;
    Return( C );					! bye;
end;

internal simple integer procedure FndMap( Integer Top );
! ----------------------------------------------------------------------;
!									;
!	FndMap		Pages through the MAP looking for a request	;
!			that matches.  The returned index is then	;
!			able to be used by GETGDF for picking out the	;
!			appropriate record.				;
!									;
! ----------------------------------------------------------------------;
begin
    Integer NeedMap, IdxMap, ReqMap;
    NeedMap_ ((Top min 0) + '1020) lsh -9;	! determine page needed;
    If MapMapped neq NeedMap			! ? page already mapped;
     then If not PageMp( MapChn,		! channel;
			 MapMapped_ NeedMap,	! page;
			 MAPadr lsh -9,		! memory addr;
			 MapAccess )		! protection;
	    then Return( -1 );			! ? return error;
    If Top leq 0				! ? called with 0 or -1;
     then Return( MAPrec[ Map!Count ] );	!  then return max count;
    If RsNum = 0				! if no request;
     then Return( Top )				!  then return current entry;
     else For IdxMap_ Top+'20 step -1 until '21	! loop through map;
      do begin "request search"
	If MapMapped neq NeedMap_ (IdxMap + '1000) lsh -9
	 then If not PageMp(  MapChn,	 	! channel, page;
			      MapMapped_ NeedMap,
			      MAPadr lsh -9,	! memory addr;
			      MapAccess )	! protection;
		then Return( -1 );		! ? return error;
	ReqMap_ RsNum mod 1000;			! get batch number;
        If ReqMap = MAPrec[ IdxMap land '777 ] land '1777
         then Return( IdxMap - '20 )		! ? found one, so return;
      end "request search";
    Return( -1 );
end;


simple string procedure FileSpec( Reference string S, Dev;
				  Reference Boolean Dot, Err);
! ----------------------------------------------------------------------;
!									;
!	FileSpec	Read a filespec from the given string and	;
!			pass a "legal" name back to the caller.  Also	;
!			return information regarding errors, device	;
!			and "dot".					;
!									;
! ----------------------------------------------------------------------;
begin "read a file name"
    String Usr, Fil, Ext, Ppn;
    Dot _ Err _ False;					! Initially false;
    Dev _ Usr _ Fil _ Ext _ Ppn _ Null;			! Initially empty;
    ArrClr( RsBlk );					! Clear storage;
    Fil _ Scan( S, BrkNam, Brk );			! Read name?;
    If Brk = ":" then begin
	Lop( S );					! eat ":";
	Dev _ Fil;					! set device;
	Fil _ Scan( S, BrkNam, Brk );			! Re-read name?;
    end;
    If Brk = "(" then begin
	Lop( S );					! eat "(";
	Usr _ Scan( S , BrkUsr, Brk );			! get username;
	RsBlk[F!USR]_ CvSIX( Usr[1 for 6] );		! convert username;
	RsBlk[F!US2]_ CvSIX( Usr[7 for 6] );		!  in two parts;
	Usr _ "(" & Usr & ")";				! slap on the parens;
	If Brk neq ")" then err _ True;			! everything ok?;
	Fil _ Scan( S , BrkNam, Brk );			! then filename;
    end;
    If Brk = "." then begin
	Lop( S );					! eat ".";
	Dot _ True;					! declare extension;
	Ext _ "." & Scan( S , BrkNam, Brk );		! and read it;
	RsBlk[F!EXT]_ CvSIX( Ext[2 for 3] );		! convert to value;
    end;
! --------------------------------------------------------------------------;
!    If Brk = "[" then begin						    ;
!	Lop( S );					! eat "["	    ;
!	Ppn _ "[" & Scan( S , BrkPpn, Brk ) & "]";	! get PPN	    ;
!	Lop( S );					! eat "]"	    ;
!	If Brk neq "]" then err _ True;			! closed ok?	    ;
!	If Length(Usr) then err _ True;			! (user) & [ppn] ?  ;
!    end;								    ;
! --------------------------------------------------------------------------;
    Scan( S, BrkWht, Brk );				! clear whitespace;
    If Length(Dev) = 0 then Dev _ "DSK";		! Default to DSK;
    RsBlk[F!DEV]_ CvSIX( Dev );				!  make sure got copy;
    RsBlk[F!NAM]_ CvSIX( Fil );				!  for later;
    Return ( Usr & Fil & Ext & Ppn );			! All done;
end "read a file name";

internal simple procedure UpdCnt( Integer Dir, Loc(-1) );
! ----------------------------------------------------------------------;
!									;
!	UpdCnt		Read SPLCNT or REMCNT file and update the	;
!			count in the appropriate direction.  If the	;
!			count goes below zero, something must be wrong,	;
!			so set it to zero.				;
!			    Loc(-1) says use GDF[Index,4] as location.	;
!			    Loc( 0) says use center as the location.	;
!			    Loc(+n) says use printer location n.	;
!									;
! ----------------------------------------------------------------------;
begin
    UseLoc_ (If Loc < 0				! Set printer location;
	      then !GDF( PrinterLoc )		!  to default (if -1);
	      else If 1 = !GDF( OptionsRH ) land '7
		    then 0			!  to 0 if center;
		    else Loc )			!  or to specified loc;
	    land '377;				!  within range;
    OpnMSC( If UseLoc = 0			! Open file: writeable;
	     then CenFile			!  for center if loc = 0;
	     else RemFile			!  for remote otherwise;
	    , M!Write				! Open Writeable;
	    , True );				! Create if not found;
    If not PageMp(  MscChn, 1,
		    MscAdr lsh -9, M!Write )	! ? map page to memory;
     then begin
	Chnior( MscChn, 1, !chCFP );		! create page;
	PageMp( MscChn, 1,
		MscAdr lsh -9, M!Write );	! map it;
     end;
    UseCount_ !rh( MSCrec[ UseLoc ] );		! get count;
    UseCount_ If Dir < 0			! update it in;
		then UseCount + 1		!  the positive;
		else UseCount - 1;		!  or negative direction;
    If UseCount < 0 then UseCount_ 0;		! perform zero check;
    MSCrec[ UseLoc ]_ If UseLoc = 0		! reset counter in page;
		then UseCount			! with new count;
		else UseCount lor !bit(0);	! or count with changed bit;
    ClsMSC;					! close channel;
end;

internal simple procedure FDFlst( String HeadingLine; Boolean License );
! ----------------------------------------------------------------------;
!									;
!	FDFlst		Pages through the FDF file and lists info	;
!			about each file spooled on the terminal.	;
!			Different information is printed depending	;
!			upon the user's license and privilages.		;
!									;
! ----------------------------------------------------------------------;
begin
    Own Integer F, J, K, L, C;
    If 0 > F_ GetFDF( J_ !GDF( FDBlock) )
     then Return;
    K_ !lh( F ); L_ !rh( F );
    Print( HeadingLine, Crlf );
    For C _ 1 step 1 until !GDF( FileCount ) lsh -26
     do begin "printing FDF records"
	Print(	If FDFrec[K,L,FDF!FileDir1] neq 0
		 then "(" & Cv6str( FDFrec[K,L,FDF!FileDir1] ) &
			    Cv6str( FDFrec[K,L,FDF!FileDir2] ) & ")"
		 else null,
		CvXstr( FDFrec[K,L,FDF!FileName] ), ".",
		CvXstr( FDFrec[K,L,FDF!FileExt] lsh 18 ),
		"/", FDFrec[K,L,FDF!Request] lsh -30,
		If License
		 then 	"  (" &
			Cv6STR( !lh(FDFrec[K,L,FDF!FileExt]) lsh 18 ) &
			")" & Crlf
		 else Crlf);
	If L < 16
	 then L_ L + 1
	 else begin L_1; K_ K + 1; end;
	If K > 4 
	 then begin
	    If 0 > F_ GetFDF( J_J+1 ) then Return;
	    K_ !lh( F ); L_ !rh( F );
	 end;
     end;
    Print( Crlf );
end;


internal simple procedure FDFdel;
! ----------------------------------------------------------------------;
!									;
!	FDFdel		Pages through the FDF file and deletes each	;
!			file spooled.					;
!									;
! ----------------------------------------------------------------------;
begin
    Integer F, J, K, L, C, DelChn, Deof;	! declare variables;
    If 0 > F_ GetFDF( J_ !GDF( FDBlock) )	! attempt to get FDF;
     then Return;				!  oops! not there;
    Open( DelChn_ Getchan, "DSK", '17, 0,0, 0,0, Deof_-1);
    If Deof then Return;			! open a channel for deletes;
    For C _ 1 step 1 until !GDF( FileCount ) lsh -26
     do begin "deleting FDF records"
	Lookup( Delchn, SpoolDir&		! lookup each file;
			Cv6str( FDFrec[K,L,FDF!FileName] ) & "." &
			Cv6str( !lh(FDFrec[K,L,FDF!FileExt]) lsh 18 )
			, Deof_-1 );		! and delete it;
	If not Deof then Rename( Delchn, "", 0, Deof_-1);
	If L < 16				! check for next record;
	 then L_ L + 1				! remember 16 per block;
	 else begin L_1; K_ K + 1; end;		! so sometimes next block;
	If K > 4 				! remember 4 blocks per page;
	 then begin				! so sometimes next page;
	    If 0 > F_ GetFDF( J_J+1 ) then Return;
	    K_ !lh( F ); L_ !rh( F );
	 end;
     end "deleting FDF records";
    Release( DelChn );				! give the channel back;
end;


internal simple boolean procedure SixArrow;
! ----------------------------------------------------------------------;
!									;
!	SixArrow	Always Returns TRUE and converts the SIXBIT	;
!			character pointed to by CvmByt to ASCII with	;
!			the exception of "^" or "_" which are both	;
!			converted to the 2-character sequence CR-LF.	;
!			It keeps track of whether it needs to finish	;
!			a CRLF sequence via the flag SIXCRLF which	;
!			should be initially zeroed to insure good data.	;
!			The resultant character is left in CvmChr.	;
!									;
! ----------------------------------------------------------------------;
begin
    If SixCrlf					! if LF needed;
     then begin SixCrlf_ False; CvmChr_ #LF end	!  then set LF;
     else begin					!  else ;
	CvmChr_ Ildb( CvmByt ) + '40;		!   read a byte;
	If CvmChr = "^" or			!   and check for special;
	   CvmChr = "_"				!   characters;
	 then begin				!  if *special*;
	    SixCrlf_ True;			!  set LF flag;
	    CvmIdx_ CvmIdx - 1;			!  decrement total count;
	    CvmChr_ #CR				!  substitute a CR;
	 end
     end;
    Return( True );				! return;
end;

internal simple procedure SetOptLine;
! ----------------------------------------------------------------------;
!									;
!	SetOptLine	Copy all the GDF Opts into local variables.	;
!									;
! ----------------------------------------------------------------------;
begin
    Integer Opt;
    OptIndent_ !lh(!GDF( Indentation )) land '377;
    Opt_ !GDF( OptionsRH ); 
    OptFullCase_ (Opt lsh -16) land '3;
    OptHeading_  (Opt lsh -15) land '1;
    OptCopies_   (Opt lsh -9) land '77;
    OptTransfer_ (Opt lsh -8) land  '1;
    OptQuest_    (Opt lsh -7) land  '1;
    OptFortran_  (Opt lsh -6) land  '1;
    OptDouble_   (Opt lsh -5) land  '1;
    OptSite_     (Opt lsh -3) land  '3;
    OptPtrSite_  (Opt land '7);
    Opt_ !GDF( OptionsLH );
    OptPaper_    (Opt rot  3) land  '7;
    OptParts_    (Opt rot  6) land  '7;
    OptDecolate_ (Opt rot  9) land  '7;
    OptDeliver_  (Opt rot 12) land  '7;
    OptCustomer_ (Opt rot 13) land  '1;
    OptChgOther_ (Opt rot 15) land  '3;
    OptKatakana_ (Opt rot 16) land  '1;
    OptLPPage_ !rh( Opt );
end;

internal simple string procedure PrtOptLine;
! ----------------------------------------------------------------------;
!									;
!	PrtOptLine	Print the Opt line, for hardcopy, etc.	;
!									;
! ----------------------------------------------------------------------;
Return( "Opts: " &
    (If OptHeading  then "Heading," else "") &
    (If OptQuest    then "Quest,"   else "") &
    (If OptFortran  then "Fortran," else "") &
    (If OptDouble   then "Double,"  else "") &
    ("Lines= " & CvS( OptLPPage  ) & ",") &
    (If OptIndent   then "Indent= " & Cvs( OptIndent ) & "," else "") &
    (If OptFullCase then "Fullch,"  else "") &
    ("Copies= " & Cvs( OptCopies ) & ",") &
    (If OptPtrSite = 1 then "Center" else "Remote") &
    Crlf );

internal simple procedure ReqPrt( Boolean Duplicate; Integer License );
! ----------------------------------------------------------------------;
!									;
!	ReqPrt		Print the hard-copy report for the operator	;
!			or put the hard-copy into the SLVSUP file.	;
!									;
! ----------------------------------------------------------------------;
begin
  SetOptLine;			! copy Opts;
  Print(
    If Duplicate then DuplicateBanner else "",
    "Request number     ",CvXstr(!GDF( Request ) lsh 6),
    Spaces[1 for 14], "Control # ------ QA by:-------", Crlf,
    "Date & Time        ",DateSt(!lh(!GDF( Date ))),
    "   ", Daytim( 60000 * !rh(!GDF( Time )) ), Crlf,
    "Requested by       ",CvmSix( location(!GDF( RequestName )), 24 ), Crlf,
    "Entered by         ",CvmSix( location(!GDF( RequestUser )), 12 ), Crlf,
    "Entered for        ",CvmSix( location(!GDF( ChargeUser  )), 12 ), Crlf,
    "Project code       ",CvmSix( location(!GDF( Project     )), 12 ), Crlf,
    "Telephone Number   ",CvmSix( location(!GDF( Telephone   )), 24 ), Crlf,
    "District           ",!lh(!GDF( District )) land '377, Crlf,
    If OptTransfer then ( Crlf &
	"Spooled to location " & CvS( !GDF( OriginLoc )) & Crlf &
	"For transfer to " & CvmAsc( location(!GDF( TransferLoc )), 25 ) &
	Crlf  ) else "",
    Crlf, PrtOptLine, Crlf,
    "Description of work:", Crlf, "Print on ",
    Case OptPaper of (
      [1] "SMALL ",	[2] "LARGE ",	[3] "TTY   ",
      [4] "SPECIAL ",[5] "SMALL WHITE ", [6] "LARGE WHITE "),
    If OptPaper < 4 then CvS( OptParts ) & " part " else "",
    "paper    ", !GDF( Pages ) lsh -18, " Pages", Crlf,
    If OptPaper = 4 then ( Crlf &
	CvmAsc( location(!GDF( SpecialInstructions )), 135 ) & Crlf )
	else "",
    Crlf );
  FDFlst( "The following files:", License );
  Print(
   "Delivery instructions:", Crlf, "Deliver by: ",
    Case OptDeliver of (
      [0] "U.S. Mail, 1st Class",	[1] "Hold for Pickup",
      [2] "Courier Service",		[3] "Special Delivery",
      [4] "Air Mail",			[5] "Air Express",
      [6] "Inter-Office Mail" ), Crlf, Crlf,
    "Mail to:", Crlf,
    CvmArr(!Xwd( '440600, location(!GDF(DeliveryAddress))), 240, SixArrow),
    Crlf, "Additional delivery inst.: ",
    CvmAsc( location(!GDF( Instructions )), 145 ), Crlf );
    If not Duplicate
     then Print( Crlf );
    Print( If Duplicate then DuplicateBanner else "", Crlf);
end;

internal simple procedure SupCpy( Integer License );
! ----------------------------------------------------------------------;
!									;
!	SupCpy		Make a "hardcopy" of this request in the	;
!			operator notification file (*1BATCH)SSPOOL.DAT	;
!			for SLVSUP or whatever to read.			;
!									;
! ----------------------------------------------------------------------;
begin
    Own integer supchan, supeof, filesize, filedate, savechan;
    Open(supchan_getchan, "DSK",0, 0,1, 256,0, supeof_-1);
    if not supeof
     then begin "we have an open channel"
	Lookup(supchan, SSpool, supeof_-1);	! see if file exists;
	If supeof				! no, FNF - create file;
	 then begin "create file"
	  FileInfo( FileBlock );		! read file data;
	  Chnior(supchan,FileBlock[1],!chENT);	! enter (create or supersede);
	  supeof_ If !SKIP!			! if skip, then all is good;
		 then 0 else !rh(FileBlock[2]);	!  else set flag;
	  If not supeof then begin "no errors"	! Otherwise -- we must stop;
	      Close(supchan);			! create file;
	      Lookup(supchan,SSpool,supeof_-1);	! then lookup as if was there;
	  end "no errors"
	  else begin "lookup error"
	      Release( supchan );		! give channel back;
	      Print( "Spool error 1",
		     " - cannot find operator notification file.",
		     Crlf);
	      Return
	  end "lookup error";
	end "create file";
	FileInfo( FileBlock );			! read file data;
	FileSize _ FileBlock[6];		! remember the wordsize;
	FileDate _ FileBlock[2];		! remember the ext,,date;
	Chnior( supchan, FileBlock[1], !chENT);	! enter;
	supeof_ If !SKIP!			! if skip, then all is good;
		 then 0 else !rh(FileBlock[2]);	!  else set flag;
	while supeof = 3
	 do begin "get rights to file"
	    Calli( !Xwd(1,10), calli!HIBER );	! sleep 10 seconds;
	    FileBlock[2]_ FileDate;		! restore ext,,date;
	    Chnior(supchan,FileBlock[1],!chENT);! enter file again;
	    supeof_If !SKIP!			! track enter errors;
		    then 0
		    else !rh(FileBlock[2]);
	 end "get rights to file";
	If supeof				! must be non-(3) enter error;
	 then begin "enter error"
	    Release( supchan );			! give channel back;
	    Print( "Spool error 2",		! print error message;
		   " - cannot write operator notification file.",
		   Crlf );
	    Return				! return to caller;
	 end "enter error";
	SaveChan_ SetOut( supchan );		! use file channel;
	Useto(supchan, (Filesize lsh -2) + 1);	! append to file;
	ReqPrt( False, License );		! do hardcopy into file;
	SetOut( SaveChan );			! restore printing terminal;
	Close( supchan );			! close file channel;
    end "we have an open channel";
    Release( supchan );				! give channel back;
end;


internal simple boolean procedure GetFil;
! ----------------------------------------------------------------------;
!									;
!	GetFil		Read a filename from "LINE" or prompt the user	;
!			at the terminal for same.			;
!									;
! ----------------------------------------------------------------------;
begin
    While TRUE
     do begin
	If Line neq "?" and Length( Text_ FileSpec(Line, RsDev, RsDot, Err) )
	 then begin
	    RsFile_ Text;
	    If not Err then Return( True );
	    Print( "Illegal file name.", Crlf );
	    Line_ "?";
	 end
	 else begin
	    If Length( Text )
	     then Print( "Enter the name of a file in the form ""(USERNAME)FILE.EXT"""
			, Crlf );
	    RsFile_ null;
	    Print( "File Name: " );
	    If Length( Line_ Inchwl ) = 0 then return( False );
	 end;
     end;
end;

internal simple integer procedure GetReq;
! ----------------------------------------------------------------------;
!									;
!	GetReq		Read a request number for "LINE" or prompt the	;
!			user for same.					;
!									;
! ----------------------------------------------------------------------;
begin
    While TRUE
     do begin
	Text_ Token( Line );
	If SubSet( Text, "0123456789" )
	 then begin
	    RsNum_ Cvd( Text );
	    If 1000 leq RsNum leq 99999
	     then Return( RsNum )
	     else begin
		Print( "Illegal request number.", Crlf );
		Line_ null;
	     end;
	 end
	 else begin
	    If Length( Text )
	     then If "?" = Text
		then Print( "Enter the request number.", Crlf )
		else Print( "Five digit request number expected.", Crlf);
	    RsNum_ 0;
	    Print( "Request: " );
	    If Length( Line_ Inchwl ) = 0 then return( False );
	 end;
     end;
end;

internal simple boolean procedure GetUsr;
! ----------------------------------------------------------------------;
!									;
!	GetUsr		Read a username from "LINE" or prompt the user	;
!			at the terminal for same.			;
!									;
! ----------------------------------------------------------------------;
begin
    While TRUE
     do begin
	While "," = Text_ Token( Line ) do;
	If Length( Text ) and Text neq "?"
	 then begin
	    RsUser_ Text;
	    Return( True );
	 end
	 else begin
	    If Length( Text ) then Print( "Enter a username.", Crlf );
	    RsUser_ null;
	    Print( "User Name: " );
	    If Length( Line_ Inchwl ) = 0 then return( False );
	 end;
     end;
end;

internal simple boolean procedure GetFrm;
! ----------------------------------------------------------------------;
!									;
!	GetFrm		Read a printer forms type from "LINE" or from	;
!			the user's terminal if line is NULL.		;
!									;
! ----------------------------------------------------------------------;
begin
    While TRUE
     do begin
	While "," = Text_ Token( Line ) do;
	If Length( Text ) and Text neq "?"
	 then begin
	    RsForm_ Text;
	    Return( True );
	 end
	 else begin
	    If Length( Text ) then Print( "Enter a printer forms type.", Crlf );
	    RsForm_ null;
	    Print( "Forms type: " );
	    If Length( Line_ Inchwl ) = 0 then return( False );
	 end;
     end;
end;

internal simple boolean procedure GetLst( String Array SubCommand );
! ----------------------------------------------------------------------;
!									;
!	GetLst		Get one of the elements from the specified	;
!			list for the user.  Read from "LINE" if it is	;
!			possible, or prompt the user at the terminal.	;
!									;
! ----------------------------------------------------------------------;
begin
    Integer Idx,Cmd;
    Text_ If Length( Line )			! Assign the command;
     then Token( Line )				! then get next token;
     else "ALL";				! default to "XXX ALL";
    Cmd_ 0;					! initialize counter;
    For Idx_ 1 step ArrInfo( SubCommand, 1 )	! All, Center, Remote, User;
	       until ArrInfo( SubCommand, 2 )	!   assume we're intertwined;
     do If Equ( SubCommand[Idx][1 for length( Text )], Text)
	 then Cmd_ Idx;				! If a match, remember it;
    Case Cmd of begin
	[0] If SubSet( Text, "0123456789" ) then begin	! ? assume req #;
		Line_ Text;
		Return( GetReq neq 0 );
	    end;
	[1] begin RsSite_ 3; Return(True); end;	! "ALL" from both lists;
	[2] begin RsSite_ 1; Return(True); end;	! "CENTER"    "     "  ;
	[3] begin RsSite_ 2; Return(True); end;	! "REMOTE"    "     "  ;
	[4] Return( GetUsr );			! "USER"      "     "  ;
	[5] Return( GetFrm );			! "FORMS"     "     "  ;
	[6] begin RsSite_-1; Return(True); end;	! "TAPE" from print list;
	[else] Return( False )
    end;
end;


end "SPL subroutines";
  #@Z