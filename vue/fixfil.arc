Subttl	VUEMAC	Fixfil

fixfil:	move	a,filblk	; Get device ("DSK")
	movem	a,zapdev+1	; Set up for OPEN
	open	3,zapdev	; Open
	  jrst	E$$COD		;   Can't Open Device
	move	a,filblk+1 	; Get the file name.
	movem	a,newblk	; Store for lookup
	move	a,bakx		; We want filename.BAK
	movem	a,newblk+1 	;   for the lookup
	move	a,ppn 		; Get the ppn
	movem	a,newblk+3 	;   again for the lookup.
	setzm	newblk+2	; Make sure everything is kosher
	lookup	3,newblk	; Lookup [filename.bak]
	  jrst	fixfi0		;   Not-there -- ok, just do rename
	setzm	newblk		; Delete this file.
	setzm	newblk+1	; ...
	rename	3,newblk	; Delete.
	  jfcl
fixfi0:	move	a,filblk+1 	; Get the file name
	movem	a,newblk	;   store it.
	move	a,filblk+2 	; Get the extension
	hllzm	a,newblk+1 	;   and store it.
	setzm	newblk+2 	; Initially "unknown"
	move	a,ppn 		; Get the ppn again
	movem	a,newblk+3	;   and store it.
	lookup	3,newblk 	; Lookup for the rename
	  jrst	fixfi1 		;  **Ignore Errors**
	move	a,bakx 		; Set the extension to
	hllm	a,newblk+1 	;   .BAK
	move	a,newblk+2	; Copy the old protection, etc.
	movem	a,filblk+3	;   into the current filblk
	move	a,ppn 		; Get the ppn
	movem	a,newblk+3 	;   because lookup broke it.
	rename	3,newblk	; Do the rename.
	  jfcl			; **Ignore Errors**
fixfi1:	close	3,		; Close up and go home
	release	3,		; ...
	popj	s, 		; Return

realup:	move	a,s
	move	a,-1(a)	;get the first argument
	movei	v,0	;default failure
	lookup	2,0(a)	;lookup on the block.
	  popj	s,	;failed
	movei	v,1	;otherwise return true.
	popj	s,

