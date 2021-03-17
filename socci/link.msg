From: SDANIELS
Date: APRIL 7,1980 18:03
	RE: LINK10 vs the MONITOR
I have "successfully" loaded a monitor with LINK10 (some checkout by
Todd demonstrated it actually did things under the simulator).  The
version of LINK used is now (MPL:37)LINK10.SHR, the command file used
is (SDANIELS:

From: SDANIELS
Date: APRIL 7,1980 18:03
37)MON01.CCL, and the various product files are (SDANIELS:37)
MON01.MAP,.SAV,.LOG  
	Problems/kluges:	1) PPNs used in .CCL rater than USERNAMES
				2) SEXTERNs are treated as EXTERNs, 
					/DEFINED away the undefined ones
	[note: by dropping the

From: SDANIELS
Date: APRIL 7,1980 18:03
 /define, and changing /go to /undef,
		You could get a list of which ones need to be fixed,
		and then manuaally enter to link the /define at end of load]
Command given:
	.RUN (MPL)LINK10
	*@mon01
	... much muncheng, some LNKSEX messages, final

From: SDANIELS
Date: APRIL 7,1980 18:03
ly done
Suggested changes to .CCL:
	1) the MAP file is extremely large, probably want to choose
	   better contents than :ALL.
	2) (later, "develop" project) How about putting symbols to a
		symbol file rather than into the core image.
But, IT W

From: SDANIELS
Date: APRIL 7,1980 18:03
ORKS!!!
	-Scott

