.setuuoDef 11,!STBST,|Set/Read BOOTS command string|;
.b!ex
	MOVE	ac,[!STBST,,<addr>]
	SETUUO	ac,
	 error return		;no license or string too long
	normal return		;AC/max length in characters

ADDR:	<flag>B0,,<stringaddress>

<flag>=1 to write the BOOTS command string
<flag>=0 to read the string

<stringaddress>= address of ASCIZ string to write to monitor
			or to copy into from monitor
<stringaddress>=0 to clear the BOOTS command string to nulls

.e!ex

If the caller has neither {RC} nor {OP} license, take
the error return.

If <addr>=0, set AC to the maximum BOOTS command string
length in characters and take the normal return.

If <flag>=0 copy characters from the BOOTS command
string in the monitor to <stringaddress>, terminating
on the null character; set AC to the maximum BOOTS command
string length in characters and take the normal return.

If <flag>=1,
.b!nest
If the caller is missing {OP} license, take the
error return.

If <stringaddress>=0,
.b!nest
Set the BOOTS
command string to null, set AC to the maximum BOOTS
command string length in characters, and take the
normal return.

.e!nest
If <stringaddress> is non-zero,
.b!nest
Copy characters from
<stringaddress> to the BOOTS command string.  If
the caller's ASCIZ string is longer than the maximum
BOOTS command string length, copy as much as possible
and terminate with a null.  Set AC to the maximum
BOOTS command string length in characters, and take
the error return if the string was too long, or
the normal return if the string was within the
maximum.
.e!nest
.e!nest
.endSec !STBST:
  