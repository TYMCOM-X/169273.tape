************************************************************************
* NOTE: This document is "Tymshare Proprietary", for disclosure only   *
* to Tymshare employees with a bonafide need for the information.      *
************************************************************************

The following is the current TRU formula (as of 25-Apr-79):

TRU = 	   KCT * .0000003371
	 + MCY * .000009004
	 + PWL * .000000154
	 + SOK * .001
	 + ELP * .00016
	 + BCS * .00535
	 + CHR * .000856
	 + BET * .004
	 + BIO * .0004
	 + MPC * .000168
	 + PWS * .02696
	 + ((4 * ERS + LKS) * 3 + DRS + DWS) * .00077
	 + ((4 * ERN + LKP) * 3 + DRD + DWR) * .00674
where

KCT is working set size in K times microcycles
MCY is microcycles (EBOX/2^18 + MBOX/2^15)
EBOX is execution time in clock cycles/2 (excluding memory access time)
MBOX is number of memory references (excluding fast memory references)
PWL is (WSK * PADD * (CHGF + 1) * 10000)/(PADD + CHGF) where  
	WSK is 2 * size of working set in pages after PADD
		pages have been added to working set.
	PADD is number of pages just added to working set
		via WSCTL or page fault.
	CHGF is charge factor, currently = 100.
		Only WSCTL and page fault increment the PWL counts
		(no charge for CORE UUO, RUN UUO, etc. adding pages)
SOK is number of units added to premium cell by TYMCHG UUOs * 1000.
ELP is elapsed time  *  working set size
BCS is break characters  *  working set size
CHR is non-block I/O terminal characters in  +  out
BET is elapsed seconds that ports for a job were in block I/O mode
BIO is block I/O characters input and output.
	(There is a minimum charge of 80. characters for block output)
MPC is VCR + 4 * MAP + HS where
	VCR is number of VCREATs done to user address space, including
		CORE UUO
	MAP is number of MAP operations done by user
	HS is number of pages mapped by monitor while creating high
		segments EXCEPT those mapped while job had XC license.
PWS is pages added to UWS by user via WSCTL or page fault
	(doesn't include those added by CORE UUO, RUN, etc.)
	(Also doesn't include virgin private pages.)
ERS is ENTERs and RENAMES  *  working set size in K
LKS is LOOKUPs times working set size in K
DRS is disk blocks (128 words) read times working set size in K
	The UFD reading CHANIO charges 1 block per UFD page read.
DWS is disk blocks (128 words) written times working set size in K
	VALPAG, FCREAT charge 4 blocks for each page.
ERN is ENTERs and RENAMEs done
LKP is LOOKUPs done
DRD is disk blocks (128 words) read
	The UFD reading CHANIO charges 1 block per UFD page read.
DWR is disk blocks (128 words) written
	VALPAG, FCREAT charge 4 blocks for each page.

1K = 1024 words = 2 pages.
   