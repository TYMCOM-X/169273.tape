.xchargDef 6,!XCTCC,|Write TRU Component Checkpoint|;
.b!ex
	
	MOVE	AC,[-count,,[!XCTCC]]
	XCHARG	AC,
	  error return	;AC contains <error code>
	normal return	;AC unchanged

Where the <error code> is from {tabRef XCSET}.
.e!ex

The value of <count> is ignored by the system; (this
calling sequence has been chosen to be consistent with
that of other XCHARG functions).

If the caller lacks {XC} license, or if no stream
accounting is active, take the error return.

Generate a TRU component checkpoint record in the
accounting stream, which records the value of
each component at the time of the call.  Take the
normal return.

.endSec !XCTCC:
  