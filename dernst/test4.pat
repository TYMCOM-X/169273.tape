::	The following patch fixes a problem with ASCII dialect hosts.  The
::	symptoms of the problem are 06 crashes due to buffer problems when
::	receiving long files from ASCII idalect hosts such as inter-Ontyme
::	versions of Ontyme.
PATCH(850409,1800,DRE,SNDDR,,6)
	J	PA1PTR,,
CONPATCH(PA1PTR,,12)
	LHL	1,DPORT
	RBT	R1,EXTWDR,,
	LIS	R0,3
	J	SNDDR+6,,
ENDPATCH(Turn off EXTWDR in ASCII dialect so that backpreasure works)

