init
sendma dencoff
Run the SPPOPER job, CLEAN2, this morning:
	mhx
	@splist
	@clean2
	y
	clean2.rpt
	n
Watch it, in case it hangs up.

Get the current list of systems needing cleanup:
	.record
	. . . :  tues.log
	.spmhx
	. . . :  clean2.rpt
	.type pend.rpt
	.exi

Compare Monday's & Tuesday's logs.  Clean up systems with SPOOL jobs on both
lists.
1.)  stuck in PEND state & more than 1 week old:
	.spool
	*cancel <req. #>
2.)  stuck in PROC state:
	.spfix
	. . . :  charge <req #>
3.)  stuck in TAPE state
	<log in to host where printed>
	.copy (pj)spool.tap, (sppoper)spool.tap
	.debtap
	LOGIN STRING:  sspambin: <orig host>

 