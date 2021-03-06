Module Extend = begin

bind zone1 = #2000;	       ! C-X ( commands in dispatch.

global iudent=72,spacect=0,tabwidth=8,indend=0,commentpos=41;
global hatuvl=4,sefmod=1,sosmod=1,caseok=0;
global indent=0,bsback=0,crwnin=0,spaces=0;
global jstins=0,filins=0,abbrex=0;

external Pdate,Ptime,mcrbydef;
external baudrate,autoinc,statusline;
external symbol,getnum,getroutine,Toprompt,wrtnum,Goxy;
external escvalue,delvalue,metaprefix,xprefix;
external MaxPatches,initdispatch,modblk,errstr,flush;
external discat,distbl,patsp,readch,writeloss,writestring;
external parmarray,freezone,MaxPPerFrame;
external rstk,cstk,csnum,rptr;
external ctrla,ctrlb,ctrlc,ctrld,ctrle,ctrlf,ctrlg,ctrll,ctrlk,ctrlm,ctrln;
external ctrlo,ctrlp,ctrlq,ctrlr,ctrls,ctrlt,ctrlv,ctrlw,ctrly,ctrlz;
external metaa,metab,metac,metad,metae,metaf,metak,metal,metam,quiet;
external metap,metaq,metar,metav,metaw,metaz,metadel,metcw,metabang;
external metalt,metagt,metacp,metacn,metalc,metarc,metalb,metarb,rubout;
external Ictrls,Ictrlr,nextpage,lastpage,killmultiline;
external ctrlxv,ctrlxr,ctrlxw,ctrlxs,ctrlxf,ctrlxe,ctrlxi,ctrlxt;
external uctrlc,ctrlx2,uctrld,uctrln,uctrlp,uctrlz,ctrlxz;
external SelBuf,LstBuf,uctrlo,ctrlar,metaar,metcar,ctrlat,ctrlxx;
external ctrlxx,ctrlxm,metac,metaquest,metaj;
external tabline,untabline,toleft,toright,metarp,metalp;
external clrkbf,appkbf,mycj,selfi,setrgt,setlft;

forward bindkey,metas,extend;
global routine approc(place,proc) = begin local oldvalue;
    ! proc  is the address of the procedure
    ! place is the index where it should go.

    if (MaxPatches<0,0>)*2+1 leq .patsp then begin
	errstr(plit asciz'Rebind space overflow, reseting with INIT mode.');
	initdispatch();
	(modblk+1)_sixbit'INIT';
	return
    end;

    oldvalue_.(@distbl)[.place];		!the value there now.
    if (.oldvalue leq -zone1) or
       (.oldvalue geq 0) then begin
	!cdr code fault handler.
	discat[.patsp]_.oldvalue;
	discat[.patsp+1]_0;			!nil at the end of the list.
	(@distbl)[.place]_-.patsp;		!point to us.
	patsp_.patsp+2
    end;
    discat[.patsp]_.proc;			!the procedure.
    discat[.patsp+1]_.(@distbl)[.place];	!the next thing to do.
    (@distbl)[.place]_-.patsp;			!point to us.
    patsp_.patsp+2
end;
global routine setproc(place,proc) = (@distbl)[.place]_.proc;

global routine getpair = begin    ! standard command rebinder.
    bind CtrlxValue=#400;
    bind MetaValue=#200;
    local tc,toff;

    toff_0;
    tc_readch();	       !get the thing.
    if .tc eql "?G" then return (errstr(plit asciz'Cannot Do Rebind');-1);
    writeloss(.tc);
    if .tc eql .MetaPrefix then toff_MetaValue
    else if .tc eql .XPrefix then toff_CtrlxValue;
    if .toff neq 0 then begin
	tc_readch();
	if .tc eql "?G" then return (errstr(plit asciz'Cannot Do Rebind');-1);
	writeloss(.tc)
    end;
    return .toff+.tc
end;
global routine uctrll = begin
    ! the rebinder.
    local old,new;
    ToPrompt(plit asciz'Copy key:');
    old_getpair();
    if .old lss 0 then return;
    WriteString(plit asciz' to new key:');
    new_getpair();
    if .new lss 0 then return;
    (@distbl)[.new]_.(@distbl)[.old]
end;
global routine ctrlxl = begin local mname,new;
    !macro rebinder.

    ToPrompt(plit asciz'Macro Slot: ');
    mname_readch();
    if .mname geq "A" and .mname leq "Z" then mname_.mname+("a"-"A");
    if .mname lss "a" or .mname gtr "z" then begin
	errstr(plit asciz'Illegal Slot Name.');
	return
    end;
    writeloss(.mname);
    WriteString(plit asciz' bind to key: ');
    new_getpair();
    if .new lss 0 then return;
    (@distbl)[.new]_-(zone1+.mname-"a")
end;
global routine setparm = begin	       !^Xright square bracket
    local pn,oldcallchar,oldrepcount,oldtp,tbp,tc;

    pn_readch();	       !read which parameter we are.
    pn_.pn-"0";			       !adjust it.
    ! First we handle the prompt stuff.
    ToPrompt(plit asciz'');
    while 1 do begin		       !get chars until altmode.
	tc_readch();
	if .tc eql .EscValue then exitloop;
	writeloss(.tc)
    end;
    rptr_.rptr-2;
    oldcallchar_.rstk[.rptr];
    oldrepcount_.rstk[.rptr+1];
    oldtp_.cstk[.csnum];
    csnum_.csnum-1;
    tbp_.freezone;		       !the first free byte in parmland.
    while 1 do begin		       !read chars from caller to EscValue
	tc_readch();
	writeloss(.tc);		       !and echo it.
	if .tc eql .EscValue then exitloop; !we are done.
	replacei(freezone,.tc)	       !plug it in.
    end;
    replacei(freezone,"?]");	       !part of the return sequence.
    replacei(freezone,"r");	       !the rest thereof.
    csnum_.csnum+1;		       !now restore I/O attachments.
    cstk[.csnum]_.oldtp;	       !in the samemanner as above.
    rstk[.rptr+1]_.oldrepcount;
    rstk[.rptr]_.oldcallchar;
    rptr_.rptr+2;

    parmarray[.csnum*MaxPPerFrame<0,0>+.pn]_.tbp !remember the parm.
end;
global routine popio = begin
    if .csnum eql 0 then return;   !already popped to top.
    csnum_.csnum-1; !return from qregister.
    rptr_.rptr-2;
    repcount_.rstk[.rptr+1];
    callchar_.rstk[.rptr];
    comchar_"E"+#400;		!  ^X-E
    newcount_0
end;

routine DayTime = begin local val;  external flubfr;
    Writeloss(" ");
    Flubfr();
    PDate(-1,.Val_-1);
    Writeloss(" ");
    Flubfr();
    PTime(-1,.Val_-1)
end;

routine debcor = begin local val;
    external wrtnum, wrtoct, wrtsix;

    val _ GetNum(Writestring(plit asciz' loc: '));
    Writeloss(" "); WrtOct(.val); Writestring(plit asciz '/  ');
    WrtOct(.(.val)<18,18>); Writestring(plit asciz',,');
    WrtOct(.(.val)< 0,18>);
    Writeloss(" "); Wrtnum(.(.val)<0,36>); Writestring(plit asciz'. ');
    Writestring(plit asciz' |'); WrtSix((.val)<0,36>); Writeloss("|")
end;

Macro CommandTable(Item) =
    Item ('??',				0)
    Item ('Miscellaneous:',		0)
    Item ('Help Me',			metaquest)
    Item ('Abort Operation',		ctrlg)
    Item ('Extended Command',		extend)
    Item ('Exit to Superior',		ctrlc)
    Item ('Bind Function',		bindkey)
    Item ('Bind Macro Slot',		ctrlxl)
    Item ('Copy Key',			uctrll)
    Item ('Major Mode',			Metam)
    Item ('Set Mode Parameter',		Metas)
    Item ('Terminal Type',		Asktty)
    Item ('Daytime',			Daytime)
    Item ('Define Macro',		mcrbydef)
    Item ('Begin Keyboard Macro',	ctrlxm)
 !  Item ('End Keyboard Macro',		ctrlg)
    Item ('Execute Keyboard Macro',	metac)
    Item ('Keyboard Macro Query',	metaa)
    Item ('Load Macro Slot',		metal)
    Item ('Insert Self',		selfi)
    Item ('Indent Next',		mycj)

    Item ('Cursor Manipulation:',	0)
    Item ('Backward Character',		ctrlb)
    Item ('Forward Character',		ctrlf)
    Item ('Up Text Line',		ctrlp)
    Item ('Down Text Line',		ctrln)
    Item ('Beginning of Line',		ctrla)
    Item ('End of Line',		ctrle)
    Item ('Forward Word',		metaf)
    Item ('Backward Word',		metab)
    Item ('Up Paragraph',		metalb)
    Item ('Down Paragraph',		metarb)
    Item ('Set-Erase Mark',		ctrlat)
    Item ('Exchange Point and Mark',	ctrlxx)

    Item ('Screen Control:',		0)
    Item ('Redisplay Screen',		ctrll)
    Item ('Toggle Display Output',	quiet)
    Item ('Scroll Line to Top',		metabang)
    Item ('Beginning of Screen',	metacp)
    Item ('End of Screen',		metacn)
    Item ('Next Screen',		ctrlv)
    Item ('Previous Screen',		metav)
    Item ('Beginning of Text',		metalt)
    Item ('End of Text',		metagt)
    Item ('Beginning of Page',		metalc)
    Item ('End of Page',		metarc)
    Item ('Next Page',			nextpage)
    Item ('Previous Page',		lastpage)
    Item ('Scroll Screen',		ctrlz)
    Item ('Reverse Scroll Screen',	metaz)

    Item ('Killing and Deleting:',	0)
    Item ('Delete Character',		ctrld)
    Item ('Rubout Character',		rubout)
    Item ('Delete Word',		metad)
    Item ('Rubout Word',		metadel)
    Item ('Kill Line',			ctrlk)
    Item ('Kill Multiline',		killmultiline)
    Item ('Kill Region Mark',		ctrlw)
    Item ('Kill Region String',		metaw)
    Item ('Copy Region',		metcw)
    Item ('Delete Horizontal Space',	metak)
    Item ('Restore Killed Text',	ctrly)
!   Item ('Restore Previous Killed Text', metay)
    Item ('Open Blank Lines',		ctrlo)
!   Item ('Delete Blank Lines',		ctrlxo)	! Note this is already in use
    Item ('Clear Kill Buffer',		clrkbf)
    Item ('Append Kill Buffer',		appkbf)


    Item ('Search and Replace:',	0)
    Item ('Incremental Search',		Ictrls)
    Item ('Reverse Search',		Ictrlr)
    Item ('String Search',		ctrls)
    Item ('Reverse String Search',	ctrlr)
    Item ('Replace String',		metar)
    Item ('Query Replace String',	metaq)
    Item ('Match Left Parenthesis',	metarp)
    Item ('Match Right Parenthesis',	metalp)

    Item ('File Operations:',		0)
    Item ('Visit File',			ctrlxv)
!   Item ('View File',			uctrlv)
    Item ('Read File',			ctrlxr)
!   Item ('Find File',			ctrlxf) ! Note this is already in use
    Item ('Write File',			ctrlxw)
    Item ('Save File',			ctrlxs)
    Item ('Finish File',		ctrlxf)
    Item ('Save and Run Superior',	ctrlxe)
    Item ('Insert File',		ctrlxi)
    Item ('Execute File',		ctrlxt)

    Item ('Window Operations:',	0)
    Item ('Create Window',		uctrlc)
    Item ('Split Current Window',	ctrlx2)
    Item ('Delete Window',		uctrld)
    Item ('Next Window',		uctrln)
    Item ('Previous Window',		uctrlp)
    Item ('Enlarge Window',		uctrlz)
    Item ('Shrink Window',		ctrlxz)

    Item ('Buffer Operations:',	0)
    Item ('Select Buffer',		SelBuf)
    Item ('List Buffers',		LstBuf)
    Item ('Read Killed Buffer',		uctrlo)
    Item ('Kill Buffer',		ctrlg)	! Not implemented yet
    Item ('Unmodify Buffer',		ctrlg)	! Not implemented yet

    Item ('English Language:',	0)
    Item ('Invert Case Character',	ctrlar)
    Item ('Invert Case Word',		metaar)
    Item ('Invert Case Region',		metcar)
    Item ('Indent Line',		Tabline)
    Item ('Unindent Line',		Untabline)
    Item ('Indent Region',		Toright)
    Item ('Unindent Region',		Toleft)
!   Item ('Beginning of Sentence',	metaa)
!   Item ('End of Sentence',		metae)
!   Item ('Kill Sentence',		metak)
    Item ('Transpose Characters',	ctrlt)
!   Item ('Transpose Words',		metat)
!   Item ('Transpose Lines',		ctrlxt)
!   Item ('Transpose Regions',		uctrlt)
    Item ('Justify Paragraph',		metaj)
    Item ('Right Margin',		setrgt)
    Item ('Left Margin',		setlft)
    Item ('Memory',			debcor)

	0 $;

Macro ParameterTable (Item) =
    Item ('??Help',			0)
    Item ('Baud Rate',			baudrate)
    Item ('Argument Multiplier',	hatuvl)
    Item ('Center Screens',		sefmod)
    Item ('Indentation Width',		tabwidth)
    Item ('Right Margin',		iudent)
    Item ('Left Margin',		indent)
    Item ('Indent Crown Line',		crwnin)
    Item ('Backspace Justify',		bsback)
    Item ('Page Mode',			sosmod)
    Item ('Case Mode',			caseok)
    Item ('Space Mode',			spaces)
    Item ('Expand Abbreviations',	abbrex)
    Item ('Justify Inserts',		jstins)
    Item ('Fill Inserts',		filins)
    Item ('Autosave Frequency',		autoinc)
    Item ('Delete Value',		DelValue)
    Item ('Escape Value (not meta)',	EscValue)
    Item ('Meta Prefix Value',		metaprefix)
    Item ('Control-X Prefix Value',	xprefix)
    Item ('Status Line',		statusline)
    Item ('Comment Position',		commentpos)
	0 $;


    macro PName( Text, Addr ) = (plit asciz Text)<36,7>,$;
    b();
    Writestring(plit asciz'Key: ');
    key _ getpair();
    if .key lss 0 then return ctrlg();
    (@distbl)[.key]_.CFuncts[.name];
    repcount_1
end;

Global routine extend = begin
local i;
external repcount;
    Do begin
	ToPrompt(plit asciz 'Meta-X ');	! Print M-X
	I _ Symbol(CNames);
	If .I eql 0 then Writestring(plit asciz' ** Help not available **')
    end until .I neq 0 or .flush;
    if .flush then return eGlobal routine MakBox( flag, str, num ) = begin
bind strtcol = 12, textcol = strtcol+4
! if flag is  0 then pr
    if .InTheBox
     then begin
	BoxIndex_ .BoxIndex + 1;
	Goxy(strtcol,.BoxIndex+strtrow); Writeloss("|");
	Goxy(textcol,.BoxIndex+strtrow);
	Writestring(.str);
	If .flag
	 then begin
	    Goxy(restcol,.BoxIndex+strtrow);
	    WrtNum(.num)
	 end;
	Goxy(stopcol,.BoxIndex+strtrow); Writeloss("|");
	If .BoxIndex eql BoxSize
	 then begin
	    BoxIndex_ 1;
	    Goxy(strtcol,BoxSixe+1);
	    Writestring( BoxTop );
	    Goxy(strtcol,BoxSize+3);
	    Writestring( "-- Type a space for more --" );
	    Return ( readch() eql " " )
	 end
	 else return 1
     end;
	    


Global routine metas = begin local I,Temp,ChangedScreen;
    routine ListParameters = begin local Idx;
    	external GOxy,Clearscreen,ClearLine;

	     str2col = 32, txt2col = str2col+4, rst2col = str2col+24,
	     stopcol = 64, strtrow = 1;
	Idx _ 1;				! Skip the first one [0]
	Clearscreen();
	Goxy(strtcol,strtrow); Writestring(plit
    asciz'+--------------------------------------------------------------+');
	While .Pnames[.Idx] neq 0 do begin
	    Idx _ .Idx + 1;
	    If
		Goxy(textcol,.Idx+strtrow);
		Writestring(.PNames[.Idx]);
		Goxy(restcol,.Idx+strtrow);
		WrtNum(..PLocs[.Idx]);

	    Idx _ .Idx + 1
	end;
!	    Goxy(strtcol,.Idx+strtrow+1); Writeloss("|");
!	    Goxy(stopcol,.Idx+strtrow+1); Writeloss("|");
	    Goxy(strtcol,.Idx+strtrow); Writestring(plit
		    asciz'+----------------------------------------+')
    end;

    ChangedScreen _ 0;
    Do begin
	ToPrompt(plit asciz 'Parameter: ');
	I _ Symbol(PNames);
	If .I eql 0 then begin ListParameters(); ChangedScreen _ -1 end;
    end until .I neq 0 or .flush;
    If .flush then ToPrompt(plit asciz'Aborted')
    else begin
	If .I gtr 0 then begin
	    Writestring(plit asciz' Old value was ');
	    WrtNum(..PLocs[.I]);
	    Temp _ GetNum(Writestring(plit asciz', New value: '));
	    If .flush then ToPrompt(plit asciz'Aborted')
	    else .PLocs[.I] _ .Temp
	end
    end;
    If .ChangedScreen neq 0 then Ctrll()
end;

global routine bindkey = begin
    external repcount;
    local key,name;
    ToPrompt(plit asciz'Function Name: ');
    name _ Symbol(CNames);
    if .flush then return errstr(plit asciz'Aborted');
    if .name leq 0 then return ctrlg();
    Writestring(plit asciz'Key: ');
    key _ getpair();
    if .key lss 0 then return ctrlg();
    (@distbl)[.key]_.CFuncts[.name];
    repcount_1
end;

Global routine extend = begin
local i;
external repcount;
    Do begin
	ToPrompt(plit asciz 'Meta-X ');	! Print M-X
	I _ Symbol(CNames);
	If .I eql 0 then Writestring(plit asciz' ** Help not available **')
    end until .I neq 0 or .flush;
    if .flush then return errstr(plit asciz'*Aborted*');

    if .i gtr 0 then while .repcount gtr 0 do begin
	if ..CFuncts[.i] neq 0 then (.Cfuncts[.i])();	! Attempt to use this.
	repcount _ .repcount -1
    end;
    repcount_1				! reset so subtract = 0
end;


end eludom
    