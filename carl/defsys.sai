begin "DEFER/PERP SYSTEMS"

require ('1 lsh 18) lor '0 version;
require "{}[]" delimiters;
define	#ctrl(c) = {(((c) land '37)&null)}
,	#ht = {('11&null)}  ,	#lf = {('12&null)}  ,	#vt = {('13&null)}
,	#ff = {('14&null)}  ,	#cr = {('15&null)}  ,	crlf = {('15&'12)}
,	! = {comment} , !xwd(l,r) = {(((L) lsh 18) lor ((R) land '777777))}
,	!lh(i) = {((I) lsh -18)}  ,	!rh(i) = {((I) land '777777)}
;

Define System.File = {"PERP.SYS"};
Define System.Id   = {CvSix("#PERP#")};
Define System.Max  = 255;
Define Calli!Exit  = '12;

Define Lsh.Def = 0, Lsh.Tym = 9, Lsh.Fre = 18;
Define S.(x) = {(( S.Word lsh -( Lsh.}&{x}&{ )) land '777 )};
Define W.(x) = {( S.}&{x}&{ lsh Lsh.}&{x}&{ )};

Define All.Commands = {

!C!( CHANGE, " n or n-m  Change a range of system entries." )
!C!( LIST, "   n or n-m  List a range of system entries." )
!C!( READ, "   <file>    Read (SYS)PERP.SYS or <file> into memory." )
!C!( WRITE, "  <file>    Write DSK:PERP.SYS or <file> to disk." )

!C!( HELP, " or ?        Reprint this message." )
!C!( EXIT, "             Exit program.", C.Quit )
!C!( QUIT, "             Same as EXIT." )

    };

Redefine Len.Commands = 0;
Redefine !C!(Nam,Hlp,AltC) = {
    Ifcr Len.Commands > 0 thenc , endc CvPS(Nam)
    Redefine Len.Commands = Len.Commands + 1;};
Preset!with All.Commands;
Own safe string array CmdName[1:Len.Commands];


Redefine Len.Commands = 0;
Redefine !C!(Nam,Hlp,AltC) = {
    Ifcr Len.Commands > 0 thenc , endc Cvps(Nam) & Hlp
    Redefine Len.Commands = Len.Commands + 1;};
Preset!with All.Commands;
Own safe string array CmdText[1:Len.Commands];


Redefine !C!(Nam,Hlp,AltC) = {
    Ifcr Length(CvPS(AltC))
     thenc AltC;
     elsec C.}&{Nam}&{;
    endc};

Own Safe Integer Array System[ 0 : System.Max ];

Own Integer Brk, BrkRan, BrkLin, BrkWht, BrkBrk, BrkCmd;
Own Integer BrkNam, BrkVar, BrkUsr, BrkPpn;
Own Integer Chan, Achan, Eof, EndOfFile, Count, Index, C;
Own Integer R.Count, R.Begin, R.End;
Own Integer S.Word, S.Def, S.Tym, S.Fre, S.Mod;

Own String  AtChan, AtLine, Filename, Command, Line, L;


simple procedure BrkIni;
! ----------------------------------------------------------------------;
!									;
!	BrkIni		Define and initialize the breakset tables	;
!			to be used by various INPUT and SCAN calls	;
!			throughout the program.				;
!									;
! ----------------------------------------------------------------------;
begin
    Define ##Cmd = {";=/ "&#ht}
    ,      ##Wht = {" "&#ht&#cr}
    ,      ##Brk = {" !@#$%^&*()-_+=~`[]|\:;'<>,.?/" & '42 & '173 & '175}
    ,      ##Chr = {"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"}
    ,      ##Num = {"0123456789"}
    ,      ##Nam = {##Chr & ##Num}
    ,      ##Ppn = {"01234567,"}
    ,      ##End = {#lf&#vt&#ff&#cr&#esc};

    setbreak(BrkLin_Getbreak,#lf&#ff,     #cr,  "SINF");  ! line;
    setbreak(BrkRan_Getbreak,":-, ",      crlf, "SINK");  ! range;
    setbreak(BrkNam_Getbreak,##Nam,       crlf, "RXNF");  ! name or token;
    setbreak(BrkUsr_Getbreak, ")",        null, "SINK");  ! end of username;
    setbreak(BrkPpn_Getbreak,##Ppn, ##Wht&crlf, "RXNK");  ! octal ppn;
    setbreak(BrkWht_Getbreak,#lf & ##Wht, crlf, "RXNK");  ! white space;
    setbreak(BrkBrk_Getbreak,#lf & ##Brk, #cr,  "RINK");  ! all break chars;
    setbreak(BrkCmd_Getbreak,#lf & ##Cmd, #cr,  "SINK");  ! command line;
end;
require BrkIni initialization;


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
    Fil _ Scan( S, BrkNam, Brk );			! Read name?;
    If Brk = ":" then begin
	Lop( S );					! eat ":";
	Dev _ Fil;					! set device;
	Fil _ Scan( S, BrkNam, Brk );			! Re-read name?;
    end;
    If Brk = "(" then begin
	Lop( S );					! eat "(";
	Usr _ "(" & Scan( S , BrkUsr, Brk ) & ")";	! get username;
	If Brk neq ")" then err _ True;			! everything ok?;
	Fil _ Scan( S , BrkNam, Brk );			! then filename;
    end;
    If Brk = "." then begin
	Lop( S );					! eat ".";
	Dot _ True;					! declare extension;
	Ext _ "." & Scan( S , BrkNam, Brk );		! and read it;
    end;
    If Brk = "[" then begin
	Lop( S );					! eat "[";
	Ppn _ "[" & Scan( S, BrkPpn, Brk ) & "]";	! read nnn,nnn;
	Lop( S );					! eat brk;
	If Brk neq "]" or Length( Usr )			! complain if bad;
	 then err _ True;				! not a "]" or (USER);
    end;
    Scan( S, BrkWht, Brk );				! clear whitespace;
    If Length(Dev) = 0 then Dev _ "DSK";		! Default to DSK;
    Return ( Usr & Fil & Ext & Ppn );			! All done;
end "read a file name";

simple string procedure Token (reference string S);
! ----------------------------------------------------------------------;
!									;
!	Token		Scan the string S and return the first word	;
!			or punctuation character to the caller.		;
!									;
! ----------------------------------------------------------------------;
begin
    Scan( S, BrkWht, Brk );	! clear whitespace;
    L _ scan(S, BrkBrk, Brk);	! get the next word;
    If length(L) = 0		! null?;
	then if length(S) = 0	!   null source also?;
	    then return (null)	!    yes - return null;
	    else L _ lop(S);	!    no  - get a character;
    Return(L);			! return the token;
end;

boolean procedure Range;
! ----------------------------------------------------------------------;
!									;
!	Range		Set up variables R.Begin, R.End and R.Count	;
!			and return true/false if values are in range.	;
!									;
! ----------------------------------------------------------------------;
begin
    R.Begin_ 1;    R.End_ System.Max;
    R.Count_ R.Count + 1;
    Scan( Line, BrkWht, Brk );
    If Length( Line ) = 0
     then Return( R.Count = 1 );
    L_ Scan( Line, BrkRan, Brk );
    If Brk = "," or Brk = 0
     then Return( 0 < (R.Begin_ R.End_ Cvd( L )) leq System.Max );
    If Brk = ":" or Brk = "-"
     then R.Begin_ Cvd(L);
    If R.Begin leq 0
     then Return( False );
    L_ Scan( Line, BrkRan, Brk );
    Return( (Brk = "," or Brk = 0) and
	     0 < R.Begin leq (R.End_ Cvd( L )) leq System.Max )
end;


simple procedure Fatal( String Reason );
! ----------------------------------------------------------------------;
!									;
!	Fatal		Routine for ATFILE for printing error messages.	;
!									;
! ----------------------------------------------------------------------;
Print(	Crlf, "Unable to open file: ", AtLine,
	"(", Reason, ").", Crlf, "Please RE-ENTER line: " );

recursive string procedure AtFile( Boolean Typeit (False) );
! ----------------------------------------------------------------------;
!									;
!	ATFILE		Routine to allow input from either the TTY or	;
!			any level of indirect files until SAIL runs	;
!			out of channels.				;
!									;
! ----------------------------------------------------------------------;
begin "ATFILE"
    If Length( AtChan ) = 0 then AtLine_ Inchwl		! default to inchwl;
     else begin "read from file"
	AtLine_ Input( AtChan, BrkLin );		! read initial line;
	While Brkvar = 0 and not EndOfFile		! ? eof, ? crlf;
	 do AtLine_ AtLine & Input( AtChan, BrkLin );	!  then read more;
	If Typeit and not EndOfFile
	 then Print( AtLine, Crlf );			! ? wants it printed;
	If EndOfFile					! if this was a read;
	 then begin "end of reading"
	    Release( Lop(AtChan) );			! and found eof;
	    Return( AtFile( Typeit ) );			! return self;
	 end "end of reading"
     end "read from file";
    If AtLine = "@"
     then begin "nest command files"			! ? command file;
	Lop( AtLine );					! remove "@" char;
	If 0 geq AChan_ GetChan
	 then begin "no channels available"
	    Fatal( "no channels" );
	    Return( AtFile( Typeit ) );
	 end;	    
	Open( AtChan_ AChan & AtChan,
	      "DSK", 1, 1,0, 512,Brkvar, EndOfFile_-1);	! get channel;
	If Not EndOfFile
	 then Lookup( AtChan, AtLine, EndOfFile_ -1);	! ? file found;
	If EndOfFile
	 then begin "cant find file"
	    Release( Lop(AtChan) );			! chop channel list;
	    Fatal( "open error" );			! complain about file;
	 end "cant find file"
	 else If Typeit
	       then Print( "(Reading from file """,AtLine,""")  " );
	Return( AtFile( Typeit ) );			! try file-line;
     end "nest command files";
    Return( AtLine );					! done;
end "ATFILE";


string procedure Prompt( String P );
! ----------------------------------------------------------------------;
!									;
!	Prompt		Print a line and read an answer using ATFILE.	;
!									;
! ----------------------------------------------------------------------;
begin
    Print( P );
    Return( AtFile( True ) );
end;


integer procedure CvPrt( String P; Integer D );
! ----------------------------------------------------------------------;
!									;
!	CvPrt		Convert the response from Prompt to decimal	;
!			and return that value if geq 0, else return	;
!			the specified default.				;
!									;
! ----------------------------------------------------------------------;
Return( If Length( L_ Prompt(P) ) and 0 leq C_ Cvd(L)
	 then C
	 else D  );


Simple procedure NotImp;
Print( "Not Implemented", Crlf );


Simple Procedure C.Read;
! ----------------------------------------------------------------------;
!									;
!	"READ"		Read a new copy of the current system list	;
!			of PERP master systems.				;
!									;
! ----------------------------------------------------------------------;
begin
    If not Length( Filename_ FileSpec(Line,L,Eof,Eof) )
     then Filename_ "(SYS)"&System.File;
    Print( "Reading file ", Filename, Crlf );
    Arrclr( System );
    Count_0;
    System[0]_ System.Id;
    Open( Chan_Getchan, "DSK", '17, 1,0, 1024, Brk, Eof_-1);
    If eof
     then begin
	Print( "DSK not available? - Serious problems exist.",Crlf );
	Return
     end;
    Lookup( Chan, Filename, Eof_-1);
    If eof
     then begin
	Print( Filename," not found.  Empty list created.",Crlf );
	Return
     end;
    Arryin( Chan, System[0], System.Max+1 );
    If eof
     then Print( "Short file -- ",!rh(eof)," words actually read.", Crlf );
    Close( Chan );
    release( Chan );
    If System[0] = System.Id
     then For Index_ 1 step 1 until System.Max
	   do If System[Index] neq 0
	       then Count_ Count + 1
	       else
     else begin
	if System[0] neq 0
	 then begin
	    Print( "Bad file format found in word 0  [",
		    CvXstr(System[0]),"]  '",Cvos(System[0]), Crlf );
	    ArrClr( System )
	 end
     end;
    Print( (If Count = 0
	     then "Empty list created."
	     else Cvs( Count )&" entries found."), Crlf );
    System[0]_ System.Id;
end;


Simple Procedure C.Write;
! ----------------------------------------------------------------------;
!									;
!	"WRITE"		Write out a copy of the in-core system list	;
!			on the current user's directory.		;
!									;
! ----------------------------------------------------------------------;
begin
    If not Length( Filename_ FileSpec(Line,L,Eof,Eof) )
     then Filename_ System.File;
    If S.Mod
     then Print( "Writing ",Filename,Crlf )
     else Print( "No changes made, writing ",Filename," anyway.", Crlf );
    Open( Chan_Getchan, "DSK", '17, 0,1, 1024, Brk, Eof_-1);
    If eof
     then begin
	Print(  "DSK not available? - Serious problems exist.", Crlf,
		Filename, " not written.", Crlf );
	Return
     end;
    Enter( Chan, Filename, Eof_-1);
    If eof
     then begin
	Print(  "Enter failure for ", Filename, " (",!Rh(Eof),")", Crlf,
		Filename, " not written.", Crlf );
	Return
     end;
    ArryOut( Chan, System[0], System.Max+1 );
    Close( Chan );
    Release( Chan );
    S.Mod_ False;
end;


Simple Procedure ChangeList( Boolean Change );
! ----------------------------------------------------------------------;
!									;
!	ChangeList	Perform common functions for the "CHANGE"	;
!			and "LIST" commands.				;
!									;
! ----------------------------------------------------------------------;
begin "Change-List"
    Integer W,D;
    GetFormat(W,D);
    SetFormat(4,0);
    While ( Range )
     do begin "each range"
	For Index_ R.Begin step 1 until R.End
	 do If (0 neq S.Word_ System[Index]) or Change
	     then Print("  System:",  Index,
			"  Default:", S.(Def),
			"  Tym:",     S.(Tym),
			"  French:",  S.(Fre),
			Crlf );
	If Change
	 then begin "change range"
	    S.Word_ System[R.Begin];
	    S.Def_  CvPrt("Default:  ", S.(Def) );
	    S.Tym_  CvPrt("Tymshare: ", S.(Tym) );
	    S.Fre_  CvPrt("French:   ", S.(Fre) );
	    S.Word_ W.(Def) lor W.(Tym) lor W.(Fre);
	    Print( Crlf );

	    For Index_ R.Begin step 1 until R.End
	     do begin "make changes"
		If S.Word neq System[Index]
		 then S.Mod_ True;
		System[Index]_ S.Word
	     end "make changes"
	 end "change range"
    end "each range";
    SetFormat(W,D);
end "Change-List";

Simple Procedure C.Change;
ChangeList( True );

Simple Procedure C.List;
If Count > 0 or S.Mod
 then ChangeList( False )
 else Print( "Empty list -- Nothing to do.", Crlf );


Simple Procedure C.Help;
begin
    Print(  Crlf, "Master PERP-System manager %",
	    Cvos(!lh(memory['137])), ".",
	    Cvos(!rh(memory['137])), Crlf,
	    "Commands:", Crlf );
    For Index_ 1 step 1 until Len.Commands
     do Print( "  ", CmdText[ Index ], Crlf );
    Print( Crlf );
end;

Simple Procedure C.Quit;
Calli( 0, Calli!EXIT );


Simple string procedure Perform!Command;
! ----------------------------------------------------------------------;
!									;
!    Perform!Command	This is the CRUX of the main program.  This	;
!			routine reads the user's command line an then	;
!			dispatches to the proper routine to perform	;
!			the specified command.  Initial data requests	;
!			for each command are also read and verified	;
!			by this routine before further processing is	;
!			allowed.					;
!									;
! ----------------------------------------------------------------------;
begin "perform command"
    Integer Cmd, Cdx, C, P;

    Line_ AtFile( True );			! Get a command;
    If Length( Line ) = 0			! Ignore blank lines;
     then Return( Null );
    Command_ Token( Line );			! Get the first word;
    If Equ(Command, "?") then Command_ "HELP";	! "?" means "HELP";
    Cdx _ 0;
    For Cmd _ 1 step 1 until Len.Commands
     do If Equ( Command, CmdName[ Cmd ][1 for length(Command)] )
	 then If Cdx neq 0
		then Return( "Ambigious command." )
		else Cdx _ Cmd;
    If 0 < Cdx leq Len.Commands
     then begin "setup command defaults"
	Scan( Line, BrkWht, Brk );		! remove any whitespace;
	If Line = "?"				! if first character is "?";
	 then Return( CmdText[ Cdx ] );		! give an help message;
	Command_ CmdName[ Cdx ];		! copy name for later;
	R.Count_ 0;				! initialize range indicator;
	Case Cdx-1 of begin All.Commands end;
     end "setup command defaults"
     else Return( "Invalid command" );
    Return( Null );
end;


TTYUP( True );
Print(  Crlf, "Master PERP-system manager %", Cvos(!lh(Memory['137])),
	".",Cvos(!rh(Memory['137])), "]", Crlf );

C.Read;

While TRUE
 do begin
    String More;
    Print( "=> " );
    Print( More_Perform!Command );
    If Length(More) then Print( Crlf );
 end;



end "DEFER/PERP SYSTEMS";
   