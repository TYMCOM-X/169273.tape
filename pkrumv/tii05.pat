	if	isis
	if	silins

	patch(861204,1630,pkrumv,siout,,0a2)
	lr	r0,ln		:test if port available
	srls	r0,1
	tbt	r0,siopta
	jn	sioidl		:no, on to next line
	lhl	r1,sioco,ln	:get current CCW
siout1	lb	r0,0,r1		:and its status
	sis	r0,1
	je	siout3		:done, we can do something
	jl	sioidl		:output still active, wait
	lr	r3,r1		:need to check if chain succeeded
	lhl	r1,siono,ln	:get other CCW (from last output)
	lb	r2,0,r1		:and check its status
	jnfs	siout2		:it finished, should be OK
	jal	r10,sioqur	:chain to previous failed, requeue it
	jfs	siout3		:and see if more to do
siout2	lr	r1,r3		:restore CCW pointer
siout3	lhl	r9,siotst,ln	:get co-routine offset
	j	seg1,r9		:and go do something

	kill	siott,siot2,siot2a,siot3,siot4

siott	lh	r4,kdln,ln	:get link descriptor
	jl	sioidl		:inactive, on to next line
	ts	ks.sem,r4	:semaphore for SYLVEX (?)
	lhl	r5,nrxm,r4	:next record
	lh	r6,recn,r5	:get record number
	sh	r6,lar,r4	:has it been acknowledged?
	lhr	r6,r6		:for modular arithmetic
	jg	siot2		:no, maybe we can send it
	ts	idle,r4		:flag idle link
	lhl	r3,siono,ln	:get CCW for previous operation
	lb	r0,0,r3		:and check its status
	je	sioids		:output still active
	rbt	ln,sdumm	:line is idle, should we send dummy?
	je	sioids		:no
	rbt	ln,ssent	:yes, mark it sent
	lhi	r0,siodum/10	:and build CCW
	sth	r0,2,r1
	jfs	siot4
siot2	lb	r0,llxm,r5	:is this a retransmission?
	jefs	siot2a		:no
	lis	r0,1		:yes, count it as such
	ahm	r0,trrxmt,ln,
	jfs	siot3
siot2a	lh	r6,rlnk,r5	:advance things for next pas
	sth	r6,nrxm,r4
siot3	jal	r10,siosnd	:finish building packet and CCW
siot4	jal	r10,sioque	:try to chain it or start output
	j	siout1		:back for more

	conpatch(atln9a+0e,,4)
	lhi	r0,siott-seg1	:reflect changed location of SIOTT

	conpatch(sioc,,44)
sioque	lhl	r3,siono,ln	:swap CCW pointers
	sth	r1,siono,ln
	sth	r3,sioco,ln
sioqur	lis	r2,0		:set current to not-chained
	sth	r2,4,r1
	lis	r0,1		:insert write command
	sth	r0,0,r1
	lis	r0,2		:set previous to chained
	sth	r0,4,r3
	lb	r0,0,r3		:check status of previous
	sis	r0,1
	jlefs	sioqu1		:either saw chain or output still active
	stb	r0,0,r3		:missed chain, so fake it
	sth	r2,4,r3		:put stop command in other CCW
	lr	r0,ln		:test if port active (may be redundant)
	srls	r0,1
	tbt	r0,siopta
	jnfs	sioqu1		:not available, skip output request
	svc	3,0b0+r0	:start output
	jal	r12,svce3b	:ERROR
sioqu1	lr	r1,r3		:point to other CCW
	jr	r10		:return

	conpatch(siort,,4)
	lhl	r1,siono,ln	:note that we swapped CCW pointers

	conpatch(sior1,,2e)
	lhl	r2,siono,ln	:check status of previous CCW
	lb	r0,0,r2
	je	sioidl		:output still active, wait
	sth	r9,siotst,ln	:set co-routine
	lhl	r2,siorsp,ln	:fetch reset packet
	srls	r2,4		:for SIO
	sth	r2,2,r1		:store it in CCW
	jal	r10,sioque	:queue it
	lh	r0,fastc+2,,	:set time
	sth	r0,lnrtim,ln,
	j	siout1		:back for more

	endpatch(Try and chain SIO output records)

	ei
	ei
  