init
sendma dencoff
Run the SPPOPER job, CLEAN, this morning:
	mhx
	@splist
	@clean
	y
	clean.rpt
	n
Watch it, in case it hangs up.

Get the list of systems needing cleanup:
	.record
	. . . :  monday.log
	.spmhx
	. . . :  clean.rpt
	.type pend.rpt
	.exi

    