begin "SETOP SYSTEMS"

require "(CARL)DEFOPR.INF"   source!file;

require "(SAILIB)SAIL.DEF"   source!file;
require "(SAILIB)UUOSYM.DEF" source!file;
require "(CARL)BRKINI.REQ"   source!file;
require "(CARL)DAYTIM.REQ"   source!file;
require "(CARL)KLLT.REQ"     source!file;


Define System.File = {"(SYS)DRWSPR.SYS"};
Define System.Id   = {CvSix("SETOPR")};

Define L!MG = '1000000;			! manager license ;

Define All.Commands = {

!C!( CHANGE, " opername  Change an opername entry." )
!C!( LIST, "   opername  List one or more opername entries." )
!C!( READ, "   <file>    Read system file or <file> into memory." )
!C!( TOGGLE, "           Toggle the various debugging flags." )
!C!( TRACE, "  opername  Print trace information for an entry." )

!C!( HELP, " or ?        Reprint this message." )
!C!( EXIT, "             Exit program writing out any changes." )
!C!( QUIT, "             Exit program without writing out any changes." )

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

define spaces = 
    {"                                                                "};
define lpad(x,y) = {(spaces[1 for y]&(x))[inf-(y-1) to inf]};
define lpaz(x,y,z) = {(z[1 for y]&(x))[inf-(y-1) to inf]};


record!class C(	r!p (C) Nxt;		! Next comment record ;
		String	Line		! Actual comment line ;
	    );

record!class G(	r!p (G)	Nxt;		! Next grant record ;
		Integer	Lic;		! License being granted ;
		String	Who;		! Who is granting, granted ;
		Integer	Date;		! Date of grant ;
		Integer	Start;		! Start date (date of grant) ;
		Integer	Stop		! Stop date (infinate) ;
	    );

record!class O(	r!p (O)	Mgr;		! Manager record pointer ;
		r!p (O)	Sub;		! First subordinate opername ;
		r!p (O)	Nxt;		! Next opername at this level ;
		r!p (O)	Bak;		! Previous opername at this level ;
		r!p (O) Lnk;		! In-core link for forgetting ;
		Integer	E!Date;		! Entry date-time ;
		String	E!Who;		! Who entered this record ;
		String	Oper;		! Opername ;
		String	Name;		! Employee name ;
		Integer	Employee;	! Employee number ;
		Integer	District;	! District number ;
		String	Phone;		! Employee phone and mail stop ;
		String	Manager;	! Manager opername ;
		String	User;		! List of usernames ;
		String	Mail;		! Security mail address ;
		Integer	C!Date;		! Change date-time ;
		String	C!Who;		! Who changed this record ;
		Integer	Password;	! Password hash ;
		Integer	P!Date;		! Password change date-time ;
		String	P!Who;		! Who changed this password ;
		Integer	License;	! Accumulated license ;
		r!p (G)	Gift;		! Received license gifts ;
		r!p (G)	Grant;		! Granted licenses ;
		r!p (C) Remark		! Comments about the record ;
    );

record!class DM( r!p (DM) Nxt;		! Next dummy manager ;
		r!p (O) Mgr		! This dummy record ;
	    );


r!p (G) GDormant, GHead;
r!p (O) ODormant, OHead, OList, OLink;
r!p (DM) DHead, DList, Dis, Dat;

r!p (C) C!b, C!t, C!n;
r!p (G) G!b, G!t, G!n;
r!p (O) O!b, O!t, O!n;
r!p (G) R!b, R!t, R!n;

r!p (O) MySelf, MyRoot;


Integer Chan, Eof, LineLength, EndOfFile, Count, Index, I;
Integer R.Count, R.Begin, R.End;
Integer Level;

Boolean FileModified;

String  Filename, Command, Line, Str, L;
String  MyOper, MyName;


simple r!p (O) procedure Traverse( r!p (O) r );
begin "traverse tree"
    own r!p (O) t;

    if not( r )				! is this a cruel joke ;
     then return( null!record );	! return empty handed ;

    if ( t_ O:Sub[ r ] )		! if a subordinate ;
     then begin "down a level"
	Level_ Level + 1;		! count levels downward ;
	return( t );			!  that is who is next ;
     end "down a level";

    if ( t_ O:Nxt[ r ] )		! if a sibling ;
     then return( t );			!  that is who is next ;

    while ( r_ O:Mgr[ r ] )		! go up one level at a time ;
     do begin "up a level"
	Level_ Level - 1;		! no more downward travel ;
	if ( t_ O:Nxt[ r ] )		!  checking for siblings ;
	 then return( t );		!  return if found, else loop ;
     end "up a level";

    return( null!record );		! nothing left at this level ;

end "traverse tree";



procedure LinkList;
begin "link list"
    own r!p (DM) d;
    own r!p (O) r, l;

    l_ OHead;				! we have an initial head ;
    if ( d_ DHead )			! start at the head ;
     then while ( d_ DM:Nxt[ d ] )	! for each tree ;
     do begin "link tree"
	O:Lnk[ l ]_ r_ DM:Mgr[ d ];	! start with the manager ;
	while ( r_Traverse( l_ r ) )	! have each record ;
	 do O:Lnk[ l ]_ r;		! point to next ;
     end "link tree";

    O:Lnk[ l ]_ null!record;		! end of list ;

end "link list";


define TopManager = '2000;
safe r!p (O) array Mgrs[ 0:TopManager-1 ];
safe string  array Managers[ 0:TopManager-1 ];
integer UsedMgrs;


simple procedure ClearManagers;
begin "management purge"
    own r!p (DM) r, l;

    arrClr( Mgrs );			! clear pointer array ;
    arrClr( Managers );			! clear text array ;
    UsedMgrs_ -1;			! reset manager count ;

    if ( r_ DHead )			! get manager list ;
     then begin "reset pointers"

	DHead_ null!record;		! reset link pointers ;

	while ( r_ DM:Nxt[ l_ r ] )	! while any in list ;
         do begin "clear manager links"
	    DM:Mgr[ r ]_ null!record;	! forget manager ;
	    DM:Nxt[ l ]_ null!record;	! forget previous link ;
	 end "clear manager links";

	DM:Nxt[ l ]_ null!record;	! forget last link ;

	l_ r_ null!record;		! forget pointers ;

     end "reset pointers";

end "management purge";
require ClearManagers initialization;


Procedure NoG( reference r!p (G) S );
begin "keep dormant"

    G:Nxt[ S ]_ GDormant;		!   place at front ;
    GDormant_ S;			!   of list ;
    S_ null!record;			!   clear pointer ;

 end "keep dormant";

Procedure NoO( reference r!p (O) S );
begin "keep dormant"

    O:Nxt[ S ]_ ODormant;		!   place at front ;
    ODormant_ S;			!   of list ;
    S_ null!record;			!   clear pointer ;

 end "keep dormant";



r!p (G) Procedure NewG;
begin "new record"
    r!p (G) Rec;

    if ( GDormant )			! if any on dormant list ;
     then begin "dormant records"

	Rec_ GDormant;			! copy head of the list ;
	GDormant_ G:Nxt[ Rec ];		! replace head with next ;
	G:Nxt[ Rec ]_ null!record;	! clear "next" in new ;

	G:Who[ Rec ]_ null;		! give a fresh record ;
	G:Start[ Rec ]_ G:Stop[ Rec ]_
	G:Lic[ Rec ]_ G:Date[ Rec ]_ 0;

     end "dormant records"
     else Rec_ new!record( G );		! create new record ;

    return( Rec );

end "new record";


r!p (O) Procedure NewO;
begin "new record"
    r!p (O) Rec;

    if ( ODormant )			! if any on dormant list ;
     then begin "dormant records"

	Rec_ ODormant;			! copy head of the list ;
	ODormant_ O:Nxt[ Rec ];		! replace head with next ;
	O:Remark[ Rec ]_ null!record;
	O:Gift[ Rec ]_ null!record;
	O:Grant[ Rec ]_ null!record;
	O:Mgr[ Rec ]_ null!record;
	O:Sub[ Rec ]_ null!record;
	O:Nxt[ Rec ]_ null!record;	! clear "next" in new ;

	O:Oper[ Rec ]_			! no left-over strings ;
	O:Manager[ Rec ]_
	O:Name[ Rec ]_
	O:Phone[ Rec ]_
	O:User[ Rec ]_
	O:Mail[ Rec ]_
	O:E!Who[ Rec ]_
	O:C!Who[ Rec ]_
	O:P!Who[ Rec ]_ null;

	O:Employee[ Rec ]_		! give a "fresh" record ;
	O:District[ Rec ]_
	O:E!Date[ Rec ]_
	O:C!Date[ Rec ]_
	O:P!Date[ Rec ]_
	O:License[ Rec ]_
	O:Password[ Rec ]_ 0;

     end "dormant records"
     else Rec_ new!record( O );		! create new record ;

    return( Rec );

end "new record";


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

string procedure Prompt( String P );
! ----------------------------------------------------------------------;
!									;
!	Prompt		Print a line and read an answer.		;
!									;
! ----------------------------------------------------------------------;
begin
    Print( P );
    Return( inchwl );
end;


integer procedure CvPrt( String P; Integer D );
! ----------------------------------------------------------------------;
!									;
!	CvPrt		Convert the response from Prompt to decimal	;
!			and return that value if geq 0, else return	;
!			the specified default.				;
!									;
! ----------------------------------------------------------------------;
Return( If Length( L_ Prompt(P) ) and 0 leq I_ cvd(L)
	 then I
	 else D  );


Simple procedure NotImp;
Print( "Not Implemented", Crlf );



procedure Subordinate( r!p (O) r, m );
begin "make subordinate"
    r!p (O) t, v;

    if ( t_ O:Sub[ M ] )		! any subordinates ;
     then begin "sort subordinates"

	while ( t )			! yes, check order ;
	 do if ( kllt( O:Oper[ t ], O:Oper[ r ] ) )
	     then t_ O:Nxt[ v_ t ]	! try next subordinate ;
	     else done;			! we are in front ;

	if ( t )			! found one ;
	 then begin "insert new subordinate"
	    O:Bak[ r ]_ O:Bak[ t ];	! copy back pointer ;
	    O:Bak[ t ]_ r;		! set next to point to me ;
	    O:Nxt[ r ]_ t;		! set my next pointer ;
	    if ( v_ O:Bak[ r ] )	! if any back pointer ;
	     then O:Nxt[ v ]_ r;	!  change pointer to me ;
	    if ( t = O:Sub[ M ] )	! if first subordinate ;
	     then O:Sub[ M ]_ r;	!  yes, now it's me ;
	 end "insert new subordinate"
	 else begin "tack on the end"
	    O:Nxt[ v ]_ r;		! I must be next for previous ;
	    O:Bak[ r ]_ v;		! and previous is my previous ;
	 end "tack on the end";

     end "sort subordinates"
     else O:Sub[ M ]_ r;		! set as only subordinate ;

end "make subordinate";


procedure DoGrant( r!p (G) r );
begin "process grants"
    own string l;

    G:Lic[ r ]_ !xwd( cvo(Line[3 for 6]), cvo(Line[10 for 6]) );
    G:Date[ r ]_ CvDay( Line[17 for 18] );
    G:Who[ r ]_ Line[36 for 12];
    G:Start[ r ]_ if ( length( l_ Line[49 for 18] ) )
		   then CvDay( l )
		   else G:Date[ r ];
    G:Stop[ r ]_ if ( length( l_ Line[67 for 18] ) )
		  then CvDay( l )
		  else 0;

ifcr false thenc
    G:Date[ r ]_ cvo( Line[17 for 12] );
    G:Who[ r ]_ Line[30 for 12];
    G:Start[ r ]_ if ( length( l_ Line[43 for 18] ) )
		   then Cvo( l )
		   else G:Date[ r ];
    G:Stop[ r ]_ if ( length( l_ Line[61 for 18] ) )
		  then Cvo( l )
		  else 0;
endc

end "process grants";


r!p (O) Procedure FindRoot( r!p (O) Self );
begin "find root"
    r!p (O) r;

    r_ Self;				! base record ;
    while ( O:Mgr[ r ] )		! as long as anyone there ;
     do r_ O:Mgr[ r ];			! find out that manager ;

    return( r );

end "find root";



simple integer procedure HashManager( String Name );
begin "hash manager"

    return(
	( ( (   cvSix( Name )
	      + cvSix( Name[7 for 6] ) )
	    land '377777777777 )
	  mod TopManager )
	);

end "hash manager";


procedure AddManager( string Name; r!p (O) M );
begin "add a manager"
    own integer Mgr;

    print( Name," ",HashManager( Name ),crlf );
    for Mgr_ HashManager( Name ) upto TopManager-1
     do if not( Mgrs[ Mgr ] )
	 then done;

    if not( Mgrs[ Mgr ] )
     then begin "set manager"

	Managers[ Mgr ]_ Name;		! remember manager name for later ;
	Mgrs[ Mgr ]_ M;			! remember pointer to manager ;
	UsedMgrs_ UsedMgrs + 1;		! count managers added ;

     end "set manager"
     else usererr( 0,UsedMgrs, "Cannot fit manager: ","X" );

end "add a manager";


r!p (O) Procedure FindManager( String Name );
begin "find manager"
    integer Mgr;
    r!p (O) M;
    r!p (DM) d;

    if ( kequ( Name, Spaces[1 for 12] ) )
     then return( OHead );		! no manager, use figurehead ;

    for Mgr_ HashManager( Name ) upto TopManager-1
     do if not( Mgrs[ Mgr ] )
	 then done
	 else if ( kequ( Name, Managers[ Mgr ]  ) )
	       then return( Mgrs[ Mgr ] );

    if ( d_ DHead )			! start at top ;
     then while ( d_ DM:Nxt[ d ] )	! for each tree in list ;
     do begin "each tree"

	M_ DM:Mgr[ d ];			! start with manager ;
	if ( kequ( Name, O:Oper[ M ] ) )
	 then done "each tree";

	while ( M_ Traverse( M ) )	! loop through tree ;
	 do if ( Kequ( Name, O:Oper[ M ] ) )
	     then done "each tree";	! got one, skip out ;

     end "each tree";

    if not( M )				! if blank, we went through them ;
     then return( null!record );	!  all and none was found ;

    AddManager( Name, M );		! Add manager to list ;
!    Print( "*", Name, crlf );
    return( M );			! remember and return pointer ;

end "find manager";


Procedure ValidateManager;
begin "validations"
    r!p (DM) d;
    r!p (O) r, v;

    MySelf_ null!record;		! insure security, reset each time ;

    while ( true )
     do begin "validate name"

	if not( length( MyOper_ Prompt( "Opername: " ) ) )
	    or ( MyOper = "?" )
	 then begin "no name specified"
	    print( crlf &"Please enter your Opername"& crlf );
	    continue;
	 end "no name specified";

	MyOper_ ( MyOper & spaces )[1 for 12];

	if ( d_ DHead )			! tree trunk ;
	 then while ( d_ DM:Nxt[ d ] )	!  for each branch ;
	 do begin "each branch"
	    v_ DM:Mgr[ d ];		! there is a manager tree ;
	    if ( kequ( MyOper, O:Oper[ v ] ) )
	     then done "each branch";	! I am the manager ;
	    while ( v_ Traverse( v ) )	! as long as anyone there ;
	     do if ( kequ( MyOper, O:Oper[ v ] ) )
		 then done "each branch";	! we found me ;
	 end "each branch";

	if ( v neq null!record )	! did we fall out or find out ;
	 then begin "setup self"
	    OList_ NewO;		! setup dummy pointer record ;
	    O:Sub[ OList ]_ MySelf_ v;	! found me ;
	    MyRoot_ FindRoot( MySelf );	! now set my root ;
	    done;			! and finish validation ;
	 end "setup self";

	print( "No opername found matching ", MyOper, crlf );

     end "validate name";

    print( (if O:Mgr[ MySelf ]
	     then "Manager: "& O:Oper[ O:Mgr[ MySelf ] ] &" "
	     else null),
	   "Root: ", O:Oper[ MyRoot ],

	   crlf );

end "validations";


procedure Forget;
begin
    own r!p (O) r, t;
    own r!p (G) h, l;
    own r!p (C) j, k;

    ClearManagers;			! clear our management list ;

    r_ OHead;				! start at beginning ;
    OHead_ null!record;

    if not( r )				! if just starting, ok ;
     then return;

    while ( r_ O:Lnk[ t_ r ] )		! for each current record ;
     do begin "forget record"

	O:Lnk[ t ]_ null!record;	! forget it's pointer ;
	NoO( t );

	O:Nxt[ r ]_ O:Bak[ r ]_		! forget it's information ;
	O:Mgr[ r ]_ O:Sub[ r ]_ null!record;

	if ( h_ O:Gift[ r ] )
	 then begin "forget gifts"
	    while ( h_ G:Nxt[ l_ h ] )
	     do NoG( l );
	    NoG( l );
	 end "forget gifts";

	if ( h_ O:Grant[ r ] )
	 then begin "forget grants"
	    while ( h_ G:Nxt[ l_ h ] )
	     do NoG( l );
	    NoG( l );
	 end "forget grants";

	if ( k_ O:Remark[ r ] )
	 then begin "forget comments"
	    while ( k_ C:Nxt[ j_ k ] )
	     do C:Nxt[ j ]_ null!record;
	    C:Nxt[ j ]_ null!record;
	 end "forget comments";

     end "forget record";

    O:Lnk[ t ]_ null!record;		! forget it's pointer ;
    NoO( t );

end;


r!p (O) procedure CheckMgr( String Name );
begin "check manager"
    r!p (DM) r, l;

    if ( r_ DHead )			! start at head of list ;
     then while ( r_ DM:Nxt[ l_ r ] )	! while any on list ;
     do if ( kequ( Name, O:Oper[ DM:Mgr[ r ] ] ) )
	 then begin "found manager"
	    DM:Nxt[ l ]_ DM:Nxt[ r ];	! remove entry from list ;
!	    print( "-",O:Oper[ DM:Mgr[ r ] ], crlf );
	    return( DM:Mgr[ r ] );	! return the "found" record ;
	 end "found manager";

    return( NewO );			! new record ;

end "check manager";


Procedure C.Read;
! ----------------------------------------------------------------------;
!									;
!	"READ"		Read a new copy of the master opername list.	;
!									;
! ----------------------------------------------------------------------;
begin
  r!p (DM) d;
  r!p (O) M, r;
  string Name;

  print( "Reading system file."& Crlf );
  if not( length( Filename_ Line ) )
   then Filename_ System.File;

  Forget;				! empty list ;
  Count_0;

  OHead_ r_ NewO;			! base record ;
  DHead_ new!record( DM );		! top of tree ;

!  if ( 0 > Chan_ VMFile( Filename, VM$Read ) ) ;
  open( Chan_ getChan,"DSK",'0,4,0,LineLength_512,Brk,Eof_-1 );
  if ( Eof )
   then usererr( 0,Eof,"No disk available","X" );
  Lookup( Chan, Filename, Eof_ -1 );
  if ( Eof )
   then begin
	print( Filename," not found.  Empty list created.",Crlf );
	return
   end;

  while true
   do begin "read loop"

    while ( length(Line_ input(Chan,BrkLin)) )	! ignore up to a blank line ;
     do begin "miscellaneous"

	if ( Line = "L" )
	 then begin "licenses"
	    O:License[ r ]_ !xwd( cvo(Line[3 for 6]), cvo(Line[10 for 6]) );
	 end "licenses"
	 else if ( Line = "G" )
	 then begin "grants"
	    if not( G!t )
	     then G!t_ O:Grant[ r ]_ NewG
	     else G!t_ G:Nxt[ G!t ]_ NewG;
	    DoGrant( G!t );
	 end "grants"
	 else if ( Line = "R" )
	 then begin "receipts"
	    if not( R!t )
	     then R!t_ O:Gift[ r ]_ NewG
	     else R!t_ G:Nxt[ R!t ]_ NewG;
	    DoGrant( R!t );
	 end "receipts"
	 else begin "comments"
	    if not( O:Remark[ r ] )		! if none, create initial ;
	     then O:Remark[ r ]_ C!t_ new!record( C );
	    C!t_ C:Nxt[ C!t ]_ new!r( C );	! create comment line ;
	    C:Line[ C!t ]_ Line;
	 end "comments";

     end "miscellaneous";

    if not( Brk )				! have we seen eof? ;
     then begin "finished"
	Print( (if Count = 0
		 then "Empty list created."
		 else Cvs( Count )&" entries found."), Crlf );
	done "read loop";
     end "finished";

    Count_ Count + 1;				! Count records ;
    G!t_ R!t_ null!record;			! Clear pointers ;
    print( ";" );

    Line_ Input( Chan, BrkLin );		! OperName:date time:who ;
    Name_ Line[1 for 12];			! name for manager check ;
    r_ CheckMgr( Name );			! Get a new opername record ;

    O:Oper[ r ]_ Name;				! setup identification ;
    O:E!Date[ r ]_ cvDay( Line[14 for 18] );
    O:E!Who[ r ]_ Line[33 for 12];
!    O:E!Date[ r ]_ cvo( Line[14 for 12] );
!    O:E!Who[ r ]_ Line[27 for 12];

    Line_ input( Chan, BrkLin );		! Emp#:district:Manager ;
    O:Employee[ r ]_ cvd( Line[ 1 for  6] );
    O:District[ r ]_ cvd( Line[ 8 for  4] );
    O:Manager[ r ]_ Line[13 for 12];

    O:Name[ r ]_ input( Chan, BrkLin );		! Employee name ;
    O:Phone[ r ]_ input( Chan, BrkLin );	! Employee phone ;
    O:User[ r ]_ input( Chan, BrkLin );		! Legal usernames ;
    O:Mail[ r ]_ input( Chan, BrkLin );		! Security mail address ;

    Line_ input( Chan, BrkLin );		! change date time:who ;
    O:C!Date[ r ]_ cvDay( Line[ 1 for 18] );
    O:C!Who[ r ]_ Line[20 for 12];
!    O:C!Date[ r ]_ cvo( Line[14 for 12] );
!    O:C!Who[ r ]_ Line[27 for 12];

    Line_ input( Chan, BrkLin );		! hash:date time:who ;
    O:Password[ r ]_ CVO( Line[ 1 for 12] );
    O:P!Date[ r ]_ cvDay( Line[14 for 18] );
    O:P!Who[ r ]_ Line[33 for 12];
!    O:P!Date[ r ]_ cvo( Line[14 for 12] );
!    O:P!Who[ r ]_ Line[27 for 12];

    if not( M_ O:Mgr[ r ]_ FindManager( O:Manager[ r ] ) )
     then begin "fake manager"

	DM:Nxt[ DList_ new!record( DM ) ]_ DM:Nxt[ DHead ];
	DM:Nxt[ DHead ]_ DList;		! make this the head ;

	DM:Mgr[ DList ]_ O:Mgr[ r ]_ M_ NewO;
	O:Oper[ M ]_ O:Manager[ r ];	! create a record for manager ;
	AddManager( O:Oper[ M ], M );	! add it to the list ;
!	print( "+",O:Oper[ M ], crlf );

     end "fake manager";

    Subordinate( r, M );		! make r subordinate to M ;

   end "read loop";

  LinkList;				! make the linear links ;

  if ( d_ DHead )			! start at the head ;
   then while ( d_ DM:Nxt[ d ] )	! for each tree ;
	 do print( "Tree: ", O:Oper[ DM:Mgr[ d ] ], Crlf );

  ValidateManager;			! figure out who we are ;

end;


r!p (G) procedure WriteGrant( r!p (G) r; string Type("R") );
begin "write grants"

    CPrint( Chan, Type,
		  ";" & lpad( cvOS( !lh( G:Lic[ r ] ) ), 6 ),
			lpad( cvOS( !rh( G:Lic[ r ] ) ), 7 ),
		  ";" & lpad( cvOS( G:Date[ r ] ), 12),
		  ";" & G:Who[ r ],
		  (if G:Start[ r ] neq G:Date[ r ]
		      or G:Stop[ r ] neq 0
		    then ";" & lpad( cvOS( G:Start[ r ] ), 12 )
		    else null),
		  (if G:Stop[ r ] neq 0
		    then ";" & lpad( cvOS( G:Stop[ r ] ), 12 )
		    else null),
		  Crlf );

    return( G:Nxt[ r ] );		! next pointer ;

end "write grants";


Procedure WriteFile( Boolean Reality );
! ----------------------------------------------------------------------;
!									;
!	"WRITE"		Write out a copy of the in-core system list	;
!			on the current user's directory.		;
!									;
! ----------------------------------------------------------------------;
begin
    r!p (O) r;

    if not( FileModified )
     then Print( "No changes made."& Crlf );
    Print( "Writing..." );

!    if ( 0 > Chan_ VMFile( Filename, VM$Update ) ) ;
    open( Chan_ getChan,"DSK",'0,0,4,LineLength_ 512,Brk,Eof_ -1 );
    if ( Eof )
     then usererr( 0,Eof,"Cannot open disk for writing" )
     else Lookup( Chan, Filename, Eof_ -1 );
    enter( Chan, Filename, Eof_ -1 );
    if ( Eof )
     then begin
	Print(  "Enter failure for file (",!Rh(Eof),")"&
		Crlf &"File not written."& Crlf );
	Return
     end;

    r_ OHead;				! base record ;
    C!t_ O:Remark[ r ];			! any initial comments? ;

    while ( C!t )
     do begin "initial comments"
	CPrint( Chan, C:Line[ C!t ] & Crlf );
	C!t_ C:Nxt[ C!t ];
     end "initial comments";

    while ( r_ O:Lnk[ r ] )		! as long as there is life ;
     do begin "write loop"

    !    Count_ Count + 1;		! Count records ;
    print( "." );

	CPrint( Chan, Crlf,		! all entries START, blank line ;
		      O:Oper[ r ],
		      ";", lpad( cvOS( O:E!Date[ r ] ), 12 ),
		      ";", O:E!Who[ r ], Crlf );

	CPrint( Chan, lpaz( cvS( O:Employee[ r ] ), 6, "000000" ),
		      ";", lpad( cvS( O:District[ r ] ), 4 ),
		      ";", O:Manager[ r ], Crlf );

	CPrint( Chan, O:Name[ r ],  crlf );
	CPrint( Chan, O:Phone[ r ], crlf );
	CPrint( Chan, O:User[ r ],  crlf );
	CPrint( Chan, O:Mail[ r ],  crlf );

	CPrint( Chan, lpad( cvOS( O:C!Date[ r ] ), 12 ),
		      ";", O:C!Who[ r ], Crlf );

	CPrint( Chan, lpad( cvOS( O:Password[ r ] ), 12 ),
		      ";", lpad( cvOS( O:P!Date[ r ] ), 12 ),
		      ";", O:P!Who[ r ], Crlf );

	if ( O:License[ r ] )
	 then CPrint( Chan, "L;",
			    lpad( cvOS( !lh( O:License[r] ) ), 6 ),
			    lpad( cvOS( !rh( O:License[r] ) ), 7 ),
			    Crlf );

	if ( R!t_ O:Gift[ r ] )
	 then while ( R!t )
	       do R!t_ WriteGrant( R!t, "R" );

	if ( G!t_ O:Grant[ r ] )
	 then while ( G!t )
	       do G!t_ WriteGrant( G!t, "G" );

	if ( C!t_ O:Remark[ r ] )
	 then while ( C!t_ C:Nxt[ C!t ] )
	       do CPrint( Chan, C:Line[ C!t ], Crlf );

     end "write loop";

    Close( Chan );
    FileModified_ False;
    Print( "Done."& Crlf );

end;



r!p (G) procedure printGrant( r!p (G) r; string Type("R") );
begin "print grants"

    print( Type,
	  ";",  lpad( cvOS( !lh( G:Lic[ r ] ) ), 6 ),
		lpad( cvOS( !rh( G:Lic[ r ] ) ), 7 ),
	  ";",  TymDay( G:Date[ r ] ),
	  ";",  G:Who[ r ],
	  (if G:Start[ r ] neq G:Date[ r ]
	      or G:Stop[ r ] neq 0
	    then ";" & TymDay( G:Start[ r ] )
	    else null),
	  (if G:Stop[ r ] neq 0
	    then ";" & TymDay( G:Stop[ r ] )
	    else null),
	  Crlf );

    return( G:Nxt[ r ] );		! next pointer ;

end "print grants";


procedure printEntry( r!p (O) r );
begin "print entry"

	print(  Crlf,		! all entries START, blank line ;
		O:Oper[ r ],
		";", TymDay( O:E!Date[ r ] ),
		";", O:E!Who[ r ], Crlf );

	print(  lpaz( cvS( O:Employee[ r ] ), 6, "000000" ),
		";", lpad( cvS( O:District[ r ] ), 4 ),
		";", O:Manager[ r ], Crlf );

	print(  O:Name[ r ] , crlf );
	print(  O:Phone[ r ], crlf );
	print(  O:User[ r ] , crlf );
	print(  O:Mail[ r ] , crlf );

	print(  TymDay( O:C!Date[ r ] ),
		";", O:C!Who[ r ], Crlf );

	print(  lpad( cvOS( O:Password[ r ] ), 12 ),
		";", TymDay( O:P!Date[ r ] ),
		";", O:P!Who[ r ], Crlf );

	if ( O:License[ r ] )
	 then print( "L;",
			lpad( cvOS( !lh( O:License[r] ) ), 6 ),
			lpad( cvOS( !rh( O:License[r] ) ), 7 ),
			Crlf );

	if ( R!t_ O:Gift[ r ] )
	 then while ( R!t )
	       do R!t_ PrintGrant( R!t, "R" );

	if ( G!t_ O:Grant[ r ] )
	 then while ( G!t )
	       do G!t_ PrintGrant( G!t, "G" );

	if ( C!t_ O:Remark[ r ] )
	 then while ( C!t_ C:Nxt[ C!t ] )
	       do print( C:Line[ C!t ], Crlf );

end "print entry";


procedure printTrace( r!p (O) r );
begin "print trace"
    r!p (O) t;

	print(	O:Oper[ r ],
		" ^:", O:Oper[ O:Mgr[ r ] ],
		" v:",
		( if ( t_ O:Sub[ r ] ) then O:Oper[ t ] else "-none-      " ),
		" <:",
		( if ( t_ O:Bak[ r ] ) then O:Oper[ t ] else "-none-      " ),
		" >:",
		( if ( t_ O:Nxt[ r ] ) then O:Oper[ t ] else "-none-      " ),
		Crlf );

end "print trace";

Simple Procedure ChangeList( Boolean Change );
! ----------------------------------------------------------------------;
!									;
!	ChangeList	Perform common functions for the "CHANGE"	;
!			and "LIST" commands.				;
!									;
! ----------------------------------------------------------------------;
begin "Change-List"

    FileModified_ true;

end "Change-List";

Simple Procedure C.Change;
return;


Procedure C.List;
begin "list it"
    r!p (O) r;

    Level_ 0;				! assume level 0 ;
    r_ O:Sub[ OList ];			! starting with pointer to self ;

    if length( Str_ Token( Line ) )
     then begin "find name"

	Str_ (Str & spaces)[1 for 12];
	while ( r )
	 do begin "list one"
	    if ( kequ( Str, O:Oper[ r ] ) )
	     then done
	     else r_ traverse( r );
	    if ( Level leq 0 )
	     then done;
	 end "list one";

	if ( r  and  ( Level > 0 ) )
	 then printEntry( r )
	 else print( "?OPRNNF Opername ",Str," not found"& crlf );

     end "find name"
     else begin "all names"

	while ( r )
	 do begin "list all"
	    printEntry( r );
	    r_ traverse( r );
	    if ( Level leq 0 )
	     then done;
	 end "list all";

     end "all names";

    print( Crlf );

end "list it";


Procedure C.Trace;
begin "list it"
    r!p (O) r;

    Level_ 0;				! assume level 0 ;
    r_ O:Sub[ OList ];			! starting with pointer to self ;

    if length( Str_ Token( Line ) )
     then begin "find name"

	Str_ (Str & spaces)[1 for 12];
	while ( r )
	 do begin "list one"
	    if ( kequ( Str, O:Oper[ r ] ) )
	     then done
	     else r_ traverse( r );
	    if ( Level leq 0 )
	     then done;
	 end "list one";

	if ( r  and  ( Level > 0 ) )
	 then printTrace( r )
	 else print( "?OPRNNF Opername ",Str," not found"& crlf );

     end "find name"
     else begin "all names"

	while ( r )
	 do begin "list all"
	    printTrace( r );
	    r_ traverse( r );
	    if ( Level leq 0 )
	     then done;
	 end "list all";

     end "all names";

    print( Crlf );

end "list it";


Simple Procedure C.Write;
WriteFile( true );


Simple Procedure C.Quit;
Calli( 0, Calli!EXIT );


Simple Procedure C.Help;
begin
    Print(  Crlf,  "License manager %",
	    cvOS(!lh(memory['137])), ".",
	    cvOS(!rh(memory['137])), Crlf,
	    "Commands:", Crlf );
    For Index_ 1 upto Len.Commands
     do Print( "  ", CmdText[ Index ], Crlf );
    Print( Crlf );
end;


Simple Procedure C.Toggle;
FileModified_ not FileModified;


Simple Procedure C.Exit;
begin
    Line_ System.File;

    if ( FileModified )
     then WriteFile( false );

    C.Quit;
end;

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

    Line_ inchwl;				! Get a command;
    If Length( Line ) = 0			! Ignore blank lines;
     then Return( Null );
    Command_ Token( Line );			! Get the first word;
    If kequ(Command, "?") then Command_ "HELP";	! "?" means "HELP";
    Cdx _ 0;
    For Cmd _ 1 step 1 until Len.Commands
     do If kequ( Command, CmdName[ Cmd ][1 for length(Command)] )
	 then If Cdx neq 0
		then Return( "Ambigious command." )
		else Cdx _ Cmd;
    If 0 < Cdx leq Len.Commands
     then begin "setup command defaults"
	Scan( Line, BrkWht, Brk );		! remove any whitespace;
	If Line = "?"				! if first character is "?";
	 then Return( CmdText[ Cdx ] );		! give an help message;
	Command_ CmdName[ Cdx ];		! copy name for later;
	Case Cdx-1 of begin All.Commands end;
     end "setup command defaults"
     else Return( "Invalid command" );
    Return( Null );
end;


TTYUP( True );
Print(  Crlf, "License manager %", Cvos(!lh(Memory['137])),
	".",Cvos(!rh(Memory['137])), Crlf );

! VMValF_ false;				! try to speed up i/o ;
C.Read;					! read in the data ;
While TRUE
 do begin
    String More;
    print( "SETOP> " );
    print( More_ Perform!Command );
    if length( More ) then print( Crlf );
 end;



end "SETOP SYSTEMS";
    @Q