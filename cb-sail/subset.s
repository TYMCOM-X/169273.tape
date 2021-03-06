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

 