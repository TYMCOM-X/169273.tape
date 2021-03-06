bind keyequal= ((plit asciz'=') )<36,7>;
bind commentstring= ((plit asciz'!') )<36,7>;
bind keythen= ((plit asciz'then') )<36,7>;
bind keyend= ((plit asciz'end') )<36,7>;
bind keybegin= ((plit asciz 'begin'))<36,7>;
bind keydo=((plit asciz 'do'))<36,7>;
bind keyof=((plit asciz 'of'))<36,7>;
bind keyset=((plit asciz'set'))<36,7>;
bind keyelse=((plit asciz'else'))<36,7>;

bind MetaValue=#200,  CtrlxValue=#400;

own xiudent;

external insertchar,toprompt,writestring,writeloss;
external lowend,getchxr,abortC,repcount,rubout;
external munchline,munchnnl,whitespace,escvalue;
external iudent,spacect,tabwidth,indend,commentpos;
external distbl,hend,hbot,lowend,lowptr,hptr;
external eolbreak,wbreak,untabline,tabline,getindent,setindent;
external ctrln2,ctrlp2,moveforward,delbackward,delforward;
external tabsize,metab,BegLin,updatecursor,moveback,EndLin,bfblk1;
external filescan,initdispatch;

Global Routine stringp (astring,wordp) = begin local tc,tp,cpos;

! This routine is used to decide if a keyword is on a line anywhere
! before the comment field.

    if .wordp and
       .lowend neq 1 and
       not wbreak(scann(lowptr))
     then return 0;			! if it is a trailer of a word.

    cpos_.lowend;			!first char
    tp_.astring;

    while 1 do begin
	tc_scani(tp);
	if .tc eql 0 then exitloop;
	if getchxr(.cpos) neq .tc then return 0;
	cpos_.cpos+1
     end;

    if (.wordp and not wbreak(getchxr(.cpos))) and
	.cpos-.lowend neq .hbot-.hend
     then return 0;

    return 1
end;

Global Routine linep (keyword)= begin local tp;

! This routine is used to test if the LAST keyword on a line
! is the magic keyword being searched for.

    tp_.keyword;
    BegLin();	!get to the left margin
    if .hend eql .hbot or eolBreak(scann(hptr))
     then return 0;
    while 1 do begin			!do the check for the whole line
	if stringp(.tp,1) then return 1;
	if stringp(commentstring,0) then return 0;
	if .hend eql .hbot then return 0;
	if eolBreak(scann(hptr)) then return 0;
	moveforward(1)
    end
end;


Global Routine elinep (keyword) = begin local nblank,tp;
    tp_.keyword;
    nblank_0;
    BegLin();
    if .hend eql .hbot or eolBreak(scann(hptr))
     then return 0;
    while 1 do begin
	if stringp(commentstring,0) then exitloop;
	if .hend eql .hbot then exitloop;
	if eolBreak(scann(hptr)) then exitloop;
	if not wbreak(scann(hptr)) then nblank_1;
	moveforward(1)
     end;
    if .nblank then metab();
    if stringp(.tp,1) then return 1;
end;

Global Routine mylf  = begin local endflag,tc;
    endflag_0;
    xiudent_0;
    BegLin();
    while not eolbreak(scann(hptr))
     do begin
	if .hend eql .hbot then exitloop;
	if not whitespace(scann(hptr)) then exitloop;
	moveforward(1)
    end;
    if .hend eql .hbot or
       eolBreak(scann(hptr))
     then begin
	BegLin();
	insertchar("?M");
	EndLin();
	return
    end;
    if linep(keyend) then begin
	xiudent_if .indend then -1 else 0;
	endflag_1
    end
    else if elinep(keythen) or elinep(keyequal) or
	    elinep(keydo) or elinep(keyof) or
	    elinep(keyset) or elinep(keyelse)
	  then xiudent_1;
    BegLin();
    if .lowend neq 1 then begin
	ctrlp2();			!if there is a line, go back to it
	if elinep(keythen) or elinep(keyelse) or
	   elinep(keyequal) or elinep(keydo)
	 then xiudent_.xiudent-1;
	ctrln2();
     end;

    if linep(keybegin) then xiudent_1;

    if .endflag and not .indend then untabline();

    spacect_ GetIndent(0) + (.tabwidth * .xiudent);
    if .spacect lss 0 then spacect_0;

    EndLin();
    insertchar("?M");
    SetIndent( .spacect );		!do the indentation.
end;

    Global Routine MetaBS = begin
    local temp;

    ctrle();
    temp_getpos();		       !the position of the cr.
    while .temp lss .repcount do begin
	insertchar(" ");
	temp_.temp+1;
    end;
    repcount_1;			       !we are done now.
end;

 global routine forceinput (tp)= begin
	csnum_.csnum+1;
	rstk[.rptr]_.callchar;
	rstk[.rptr+1]_.repcount;
	rptr_.rptr+2;
	cstk[.csnum]_.tp; !start at the right q-register.
	end;

global routine metae = begin
    csnum_.csnum+1;
    rstk[.rptr+1]_1;		       !repcount value.
    rptr_.rptr+2;		       !push on stack.
    cstk[.csnum]_0;		       !means from the tty.
end;

Global Routine myaltm  = begin local tp;
	initdispatch();
	scrmod_1;
	muline_1;
	bfblk1_.defdsk;
	(bfblk1+1)_.dirtemp;
	(bfblk1+2)_sixbit 'vue';
	(bfblk1+3)_0;
	(bfblk1+4)_gppn();
	combuf(0);
	(distbl+MetaValue+"m")_metam;
	tp_metam;
	repcount_2;
	comchar_"m";
	base_#200;
	end;

   	%	Comment out this code!!!
Global Routine Dired  = begin
	dirtemp_.(bfblk2+1);		!save the old buffer name.
	bfblk1_.defdsk;
	muline_1;
	scrmod_1;
	(bfblk1+1)_sixbit 'dired';
	(bfblk1+2)_sixbit 'vue';
	(bfblk1+3)_0;
	(bfblk1+4)_gppn();
	combuf(0);
	(distbl+"?R"+CtrlxValue)_dirlist;
	(distbl+"?D"+CtrlxValue)_dirlist;
	(distbl+MetaValue+"m")_myaltm;
	end;
	%	! Finish comment out
	% comment out this ... it belongs to dired mode %
	%
Global Routine dirlist  = begin local tppn,glbuffer[100];
	local fblk [6],tp,tc;
	local dbuf [128];

	hitsts_1;
	ToPrompt(plit asciz 'PPN in brackets: ');
	flush_0;
	tp_(glbuffer-1)<1,7>;
	getstr(.tp);
	if not filescan (tp,fblk) then return ctrlg();
	if .flush then return ctrlg();
	tppn_.(fblk+4);
	lowend_1;
	hbot_.hend;
	fblk_-1;	!start it at the 0'th file structure
	while 1 do begin
		jbstr(fblk);	!calculate the next disk structure
		if .fblk eql 0 or .fblk eql -1 then begin
			curline_1;
			return metalt() !done
			end;
		tp_(fblk-1)<0,6>;
		incr i from 1 to 6 do begin
			tc_scani(tp);
			tc_.tc+#040;
			insertchar(.tc)
			end;
		insertchar(":");insertchar("?M");
		(fblk+1)_.tppn;
		(fblk+2)_sixbit 'ufd';
		(fblk+3)_0;
		(fblk+4)_#1000001;	!1,1 is the mfd
		open (2,#16,.fblk,0<0,0>);	!open the disk
		if not realup(fblk+1) then exitcompound;
		while 1 do begin
			if not dmpin(2,dbuf,#200) then exitloop;
			incr i from 0 to 126 by 2 do begin
				if .dbuf[.i] eql 0 then exitcompound;
				tp_(dbuf[.i]-1)<0,6>;
				insertchar("?I");
				incr i from 1 to 6 do begin
					tc_scani(tp);
					tc_.tc+#040;
					insertchar(.tc)
					end;
				insertchar(".");
				incr i from 1 to 3 do begin
					tc_scani(tp);
					tc_.tc+#040;
					insertchar(.tc)
					end;
				insertchar("?M");
				incr i from 1 to 3 do incp(tp);
				end;
			end;
		end;
	end;

			%	! end comment out

Subttl	OLD MACRO FACILITY !!!

	cain	v,35	; from Readch!
	jrst	rmacro

rmacro:	;come here to attach a reader macro to the stuff.
	pushj	s,readch	;recurse to get what number.
	cain	v,":"		; hack - ":" says quote it
	  jrst	[movei	v,35
		 popj	s,]
	cain	v,7		; bell - abort
	  jrst	[pushj	s,ctrlg
		 movei	v,7
		 popj	s,]
	cain	v,"r"		; r - the emergency pop
	  jrst	[pushj	s,popio	;undo the attach
		 jrst	readch]	;and really get a char.

	subi	v,"0"		;to binary form.
	move	a,csnum
	imuli	a,MaxPPerFrame	;calc. offset.
	add	v,a		;this is the offset.
	addi	v,parmarray	;this is the base.
	move	v,0(v)		;this is the actual data.
	aos	csnum		;begin of io attach push.
	move	a,csnum
	movem	v,cstk(a)
	move	a,rptr
	movei	v,1
	movem	v,rstk+1(a)
	movei	v,2
	addm	v,rptr
	jrst	readch

    Subttl	VUEMAC	Moveforward - Move pointer forward n bytes

movefo:	setzm	ctrlct		; clear "passed" a page mark flag
	move	a,curlin	; save current line to see
	movem	a,ocl		;   if we pass a crlf.
	move	a,s		; get the number of bytes
	move	a,-1(a)		;   we are to move.
	move	c,lowend	;get .lowend of buffer.
	idivi	c,5		;make an address
	add	c,table(c+1)	;convert to an ildb pointer
	add	c,p		;offset from buffer address
	move	d,hbot		;get .hbot of buffer.
	idivi	d,5		;make an address
	add	d,table(d+1)	;convert to an ildb pointer
	add	d,p		;offset from buffer address
	addm	a,lowend
	addm	a,hbot
lp1:	sosge	a		;do loop ctr.
	  jrst	car		;go home after checking ocl with curlin.
	ildb	e,d		;get byte
	idpb	e,c		;dump it
	caie	e,15		;return?
	cain	e,14		;form-feed?
	  caia			;yes, skip
	 jrst	lp1		;no, loop
	caie	e,15		;skip if a c-ret
	  jrst	.+4		;no, do ff
	aos	curlin		;count lines
	aos	bfrlin		;count lines in buffer
	jrst	lp1
	aos	ctrlct		;count of these.
	aos	bfrpag		;count pages in buffer
	setzm	bfrlin		;clear line-count (0-n)
	jrst	lp1

Subttl	VUEMAC	Moveback - Move the pointer backward n bytes

moveba:	setzm	ctrlct
	move	a,curlin	;save curlin
	movem	a,ocl		;in ocl.
	move	a,s		;can not indirect off of sp with this bliss.
	move	a,-1(a)		;get the number of bytes we are to move.
	move	c,lowend	;get lowend of buffer
	idivi	c,5		;make an address
	add	c,table+1(c+1)	;convert to a ldb pointer
	add	c,p		;offset from buffer address
	move	d,hbot		;get the hbot value
	idivi	d,5		;make an address
	add	d,table+1(d+1)	;convert to a ldb pointer
	add	d,p		;offset from buffer address
	movn	b,a		;negate to subtract
	addm	b,lowend	;decrement value
	addm	b,hbot		;of lowend and hbot
lp2:	sosge	a		;do loop ctr.
	  jrst	car		;go home and set muline perhaps
	add	c,[7b5]		;decrement number
	caig	c,0
	  sub	c,[430000,,1]
	ldb	e,c		;get byte
	add	d,[7b5]		;decrement the thing.
	caig	d,0
	  sub	d,[430000,,1]
	dpb	e,d		;dump it
	caie	e,15		;return?
	cain	e,14		;form-feed?
	  caia			;yes, skip
	 jrst	lp2		;no, loop
	caie	e,15		;skip if a c-ret
	  jrst	.+4		;no, do ff
	sos	curlin		;count lines
	sos	bfrlin		;count lines in buffer
	jrst	lp2
	sos	ctrlct		;count of these.
	sos	bfrpag		;count pages in buffer
	setzm	bfrlin		;clear line-count (0-n)
	jrst	lp2


car:	skipe	ctrlct
	  jrst	mulpag
	move	a,ocl
	camn	a,curlin
	  popj	s,
	movei	a,1
	movem	a,muline
	popj	s,

mulpag:	movei	a,1
	movem	a,muline
	movem	a,scrmod
	popj	s,

  fef:	seto	b,		; Get the tty characteristics
	getlch	b		; Of your own port
	movem	b,tymtty	; Remember them
	tlo	b,220		; Magic bits NFC!NCM
	setlch	a		; Now setup special ones
	hrroi	b,.Axrvx	; ^S/^Q
	setzm	inirvx		; turn it off and save it
	Auxcal	b,inirvx
	Hrroi	b,.Axcfs	; Now setup port status
	Auxcal	b,io.nee!io.nec!io.fcs!.iobin
	Popj	p,		; Then Return

fen:	move	b,tymtty	; Remember these characteristics?
	setlch	b		; Now set them back
	hrroi	b,.Axrvx	; ^S/^Q
	push	p,inirvx	; Make sure this doesn't get clobberred
	Auxcal	b,inirvx	; restore it
	pop	p,inirvx	;  ...
	Popj	p,		; Then return
   