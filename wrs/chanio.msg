From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 29 Apr 86 0:22:51 PDT 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: SAIL TSR 

The SAIL CHANIO runtime library procedures do not return the value
from the AC as CALLI and AUXIO do.  The result is no way to check
the error code.  The fix is trivial, just put a
	MOVE 0,3
before the return.  Below is a SAIL version of the routine that does
not bother with polling points, but then I don't think any of the
CHANIO's are interruptable, anyway.  Beware, if you load this as a
library, it won't work with (SYS)SAILOW if its scanned first!  -Bill

	(WRS:39)CHANIO.SAI
------------------------------------------------------------------------
entry	CHNIOR
,	CHNIOV
;
begin	"CHNIO"

internal simple integer procedure CHNIOR(
    integer C; reference integer A; integer X );
comment
	Special cnhior because real one doesn't return error codes.
;
start!code
    external integer !skip!;
    define R0 = 0;
    protect!acs R0;
	hrl	 R0,access(X);
	hrr	 R0,access(C);
	setom	 !skip!;
	chanio	 R0,access(A);
	 setzm	 !skip!;
end;

internal simple integer procedure CHNIOV(
    integer C, A, X );
comment
	Special cnhiov because real one doesn't return error codes.
;
start!code
    external integer !skip!;
    define R0 = 0;
    protect!acs R0;
	hrl	 R0,access(X);
	hrr	 R0,access(C);
	setom	 !skip!;
	chanio	 R0,access(A);
	 setzm	 !skip!;
end;

end $
   