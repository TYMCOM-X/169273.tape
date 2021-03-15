! requires (SAILIB)SAIL.DEF & BRKINI.S ;

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
    Return(S1);			! return the token;
end;

