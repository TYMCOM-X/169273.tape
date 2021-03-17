Internal Simple Boolean Procedure SubSet( String Str, Sub );
! ----------------------------------------------------------------------;
!									;
!	SubSet		Return TRUE if all the characters in string	;
!			STR are some subset of those in string SUB.	;
!									;
! ----------------------------------------------------------------------;
begin "SUBSET"
    own integer array Chars[0:'177];

    If not( length( Sub ) )
     then return( false );

    arrclr( Chars );
    while ( length( Sub ) )
     do Chars[ Lop( Sub ) ]_ true;

    while ( length( Str ) )
     do If not( Chars[ Lop( Str ) ] )
	 then return( false );

    return( true );

end "SUBSET";

