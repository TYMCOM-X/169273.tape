entry
	AtLine,		comment Line by Line read from file;

;

require "MYSAIL.DEF" source!file;
require "BRKINI.REQ" source!file;

Own Integer AChan, EndOfFile, AtBrk;	! input file vars ;
Own String  AtChan, AtText;		! stacked channels;
simple procedure Fatal( String Reason );
! ----------------------------------------------------------------------;
!									;
!	Fatal		Routine for AtLine for printing error messages.	;
!									;
! ----------------------------------------------------------------------;
Print(	Crlf, "Unable to open file: ", AtText,
	"(", Reason, ").", Crlf, "Please RE-ENTER line: " );

internal recursive string procedure AtLine( Boolean Typeit (False) );
! ----------------------------------------------------------------------;
!									;
!	AtLine		Routine to allow input from either the TTY or	;
!			any level of indirect files until SAIL runs	;
!			out of channels one line at a time.		;
!									;
! ----------------------------------------------------------------------;
begin "AtLine"

    If Length( AtChan ) = 0 then AtText_ Inchwl		! default to inchwl;
     else begin "read from file"

	AtText_ Input( AtChan, BrkLin );		! read initial line;
	While ( AtBrk = 0 and not EndOfFile )		! ? eof, ? crlf;
	 do AtText_ AtText & Input( AtChan, BrkLin );	!  then read more;

	If ( Typeit and not EndOfFile )			! ? wants it printed;
	 then Print( AtText, Crlf );

	If ( EndOfFile )				! if this was a read;
	 then begin "end of reading"			!   past end of file;
	    Release( Lop(AtChan) );			! forget chan;
	    Return( AtLine( Typeit ) );			! and return self;
	 end "end of reading"
     end "read from file";

    If ( AtText = "@" )					! If first char = "@";
     then begin "nest command files"			! ? command file;

	Lop( AtText );					! remove "@" char;

	If ( 0 geq AChan_ GetChan )			! If any channels ;
	 then begin "no channels available"
	    Fatal( "no channels" );			! No, none left ;
	    Return( AtLine( Typeit ) );			! Try again ;
	 end;	    

	Open( AtChan_ AChan & AtChan,
	      "DSK", 1, 1,0, 512,AtBrk, EndOfFile_-1);	! get channel;

	If Not ( EndOfFile )
	 then Lookup( AtChan, AtText, EndOfFile_ -1);	! ? file found;

	If ( EndOfFile )
	 then begin "cant find file"
	    Release( Lop(AtChan) );			! chop channel list;
	    Fatal( "open error" );			! complain about file;
	 end "cant find file"
	 else If ( Typeit )
	       then Print( "(Reading from file """,AtText,""")  " );

	Return( AtLine( Typeit ) );			! try file-line;
     end "nest command files";

    Return( AtText );					! Return text read ;
end "AtLine";

 