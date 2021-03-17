6-AUG-74  0825		network site CMU
 ***** FTP mail from [A700SA00] (SAIL)
 CMU bug report, with fix: C9	[IOSER/14] LE03	(5-Aug-74)
 	INPUT was not noticing new pages for the SETPL thing.  Typo:
 	had a CAIE C,14 instead of CAIE D,14 just below IN2:.
 

8-AUG-74  0926		network site CMU
 ***** FTP mail from [A700SA00] (SAIL)
 Oops, sorry, ignore the last msg -- meant to send the following:
 CMU bug report =D1=  LDE 8-Aug-74
 	ASSIGNC does random evil when given a <constant-expression>.
 	The following causes an ill mem, for example:
 BEGIN
 REQUIRE "[]()" DELIMITERS;
 STRING ST;
 INTEGER I;
 DEFINE INT(A)=[ASSIGNC A=0; ST_CVPS(A);];
 INT();
 WHILE LENGTH(ST) DO OUTSTR(CVOS(LOP(ST))&".");
 END
 

12-JUN-74  1634		network site CMU
 ***** FTP mail from [A700SA00] (SAIL)
 CMU bug report.  Bug =C1=	LDE 12-Jun-74
 	Cannot have ASSIGNC as the first thing following a BEGIN --
 	get a "CANNOT BEGIN A STATEMENT LIKE THIS, PROBABLY A DECLARATION
 	AFTER A STATEMENT" message.  Can be "fixed" by putting either a
 	decl or statement before the ASSIGNC.  See ASSIGN.SAI[CMU,AIL]
 	or [A700SA00].

15-MAY-74  0608		1,RS
 I have found a bug in EXPR!TYPE.  the code (page 23 of GEN)
 is not recursive -- i.e., things that need to be saved are not
 stacked by only stored in locations.  consequently, when one has
 embedded called to EXPR!TYPE (as in macros that call it),
 bad things happen to the code.
 
 my fix, which seems to work, is to use a stack in the obvious
 way.  shall i put it in?  do you want to see it?  (probably a good
 idea.)
 			bob smith.
 

 
03-MAY-74  1720		XGP,TVR
 Didn't quite work, did it?
 				Tovar
 ---------------------------------------------------------------------
 .COM
 
 
 COULD NOT DO REMAP TO GET A SAIL SEGMENT!
 SETPR2 DONE INSTEAD.  YOUR JOB SHOULD BE HAPPY SO LONG AS 
 IT DOES NOT DO A RESET OR OTHER BADNESS. GOOD LUCK.
 ALSO, IF YOU WANT TO RUN THIS WAY, BEWARE OF RESTARTING.
 SAIL: PLTX  ?
 ERROR IN JOB 43
 ILLEGAL UUO AT USER 401732
 ^C
 .R PPSAV~
 ---------------------------------------------------------------------
 

23-MAR-74  0831		network site CMU
 ***** FTP mail from [A700SA00] (SAIL)
 Dear folks,
 	We notice there has been some activity putting in tenex stuff.
 Is there some place we could look (or someone who will tell us) what
 the state of the world is.  We have been beating on Sail very heavily,
 and are attempting very hard to stay up-to-date with bug fixes (since
 we seem to bump into them frequently).
 	I guess, we mostly want to know how stable the S,AIL world is
 and what, if any, changes will effect us non-tenex folks.
 		thanks,
 			Lee Erman (LDE@SAIL or ERMAN@CMU-10B)
  
11-MAR-74  1936		CMU,AIL
 People,
 	We have a new IOSER that mostly reflects some CMU features
 that are within CMUSW -- it also has a couple of minor error msg
 improvements.  We would very appreciate if someone could bless this
 copy and make it be IOSER on S,AIL, so that we won't have to
 edit these changes in in the future.  The new version is
 IOSER[CMU,AIL].  We have labeled our changes with =b4=, all within
 the CMUs.  
 	Please see if this can happen before anymore changes are made
 to IOSER at your end.
 	Would appreciate a confirmation when all is well.
 		thanx,
 			LDE


29-APR-74  1157		network site CMU
 ***** FTP mail from [A700SA00] (SAIL)
 CMU bug report (and fix).  CMU bug =B7=  [IOSER/8] LDE (29-Apr-74
 	SOSNUM and friends not getting initialized to 0 in OPEN.
 Haven't the slightest idea how you folks keep from getting garbage
 or Ill-Mem-refs.  If this is a non-bug, I would like to know what
 we're doing wrong.
 		thanx,  Lee
 (P.s.  this is in a complete new SAIl we brought over from
 X,AIL on 28-Apr-74).
  

11-APR-74  1448		network site MAXC
 Date: 11 APR 1974 1452-PDT
 From: SPROULL at PARC-MAXC
 Subject: A BUG
 cc:   RS at SU-AI
 
 - - - -
 The following bug, ompiled at the AI project with the /H
 siwtch on , or compiled here at Xerox just normally (remember,
 /H is standard on TENEX SAIL), gives the "Virual origin ---
 horseshit message.  However, thee is no trouble with the
 virtual origin -- the code comiled is fine, etc.
 
 (I remember finding a bug relating to this section of things
 when I took SAIL to NIH -- this error message was printed
 out when it should not have been.  Is is possible that the
 edit was lost?? I peered vbriefly at the ARRAY code on S,AIL
 and couldn't remember enuf context to decide whether the fix
 was there, or lost....)
 
 Here is the code:
 
 00100	begin "classify"
 00200	  integer uttn,i,j,k,l,m,n,iword,iutt,fnum,flen,lnum,llen,ds,
 00300	          depth,obr,descthresh,pdtol,dist,iw,ld,br,lt,utts,i1,
 00350	          iclass,ndesc,fb,nt;
 00400	  integer array big[1:20860,1:4],cs[1:13820],classes[1:55],
 00500	          where[1:40],ns[1:100],ibest[1:10];
 00550	  real res1,res2;
 00600	  real array result[1:55],iresult[1:55],best[1:10];
 00700	  string word;
 00800	  string array thewords[0:100];
 00850	end;
 
 
 So, that's it.  Have fun.
 			Bob Sproull
 -------
 

31-MAR-74  2332		THE,JRL
 no error message with COP(listitemvar)
 

06-APR-74  1251		1,RS
 	(1)  I DECIDED TO COME UP HERE SO THAT FILES WOULD
 BE IN TV FORMAT WHEN YOU GET THEM, THUS CREATING LESS CONFUSION.
 	(2)  I TRANSFERRED TOTAL OVER TO [S,AIL].  PERHAPS
 I SHOULD HAVE WAITED BUT IT WAS SO LITTLE DIFFERENT.
 	(3)  SAIL AND GOGOL HAVE LIKEWISE SOME LITTLE PATCHES
 IN THEM.  THE PATCHES ARE IN THE FILES ON [10X,AIL], I GUESS
 I'LL LEAVE THEM FOR YOU TO SRCCOM AND TRANSFER.  GOGOL HAS
 1 LINE CHANGED [EXCEPT FOR DIRECTORY-VERSION PAGES], AND
 SAIL HAS ABOUT 25 LINES CHANGED.  
 	ALL OF THE ABOVE ARE UNDER TENX CONDITIONAL
 ASSEMBLY SWITCHES, SO THERE SHOULD BE NO CHANCE FOR IMPACT.
 	ALSO, EVERYTHING IS IN TV FORMAT.
 
 			BOB SMITH
 E
 
05-APR-74  0025		ACT,REG
 R SAIL
 _foo.			____this shouldn't be an error!

 SAIL COMMAND ERROR ....
 String space exhausted unexpectedly.    ____ this probably should be fixed


27-MAR-74  1943		network site MAXC
 Date: 27 MAR 1974 1945-PDT
 From: SPROULL at PARC-MAXC
 Subject: Sail bugs
 
 - - - -
 Here are some more SAIL things:
 
 1. The export world complains about the description of FILEINFO,
 on page 43 of the manual.  It says it returns a 6-word block,
 and the list of things gives 7 items.  Furthermore, these are not
 correct in the export world (10/50 or TENEX).  Suggest a comment
 about what happens in wexport world.  For example:  "FILEINFO
 fills the array with the results of the last LOOKUP or ENTER;
 consult the 10/50 manual for the format of these entries."
 
 2. The following program (which comes from an example on page
 30 of the manual) does not work, i.e. gets a syntax error inside
 the second remember....
 
 00100	begin
 00200	integer number; context context1,context2;
 00300	remember number in context1;
 00400	remember context1 in context2;
 00500	end;
 00600	
 
 
 That"s it.
 			Bob
 -------

20-MAR-74  0848		1,RS
 	WELL IT SEEMS TO WORK.  THE FOLLOWING FILES EXIST:
 		(1) SAILRS.REL
 		(2) SAILRS.DMP
 THE .DMP WAS LOADED /Y/VSAILRS, WHICH I THINK GETS RAID, BUT
 IT MAY NOT BE THE CURRENT WAY YOU LOAD THESE THINGS.
 
 	IT SEEMS TO WORK ON SEVERAL PROGRAMS.  I COMPILED AND LOADED AND
 TESTED PUB WITH IT, AND THE RESULTING .DOC FILE SRCCOMED
 TO ONE DONE WITH SYSTEM PUB.  SO IT LOOKS OK.
 
 			SEE YOU LATER,
 			BOB
 
19-MAR-74  1413		ACT,REG
 If you're planning on putting up a new SAIL soon, why not make the
 change to REQUIRE LIBRARY and REQUIRE LOAD MODULE that we talked about?

19-MAR-74  1315		network site MAXC
 Date: 19 MAR 1974 1314-PDT
 From: SPROULL at PARC-MAXC
 Subject: MAGTAPE WRITER
 
 - - - -
 THROUGH THE MIRACLES OF MODERN COMMUNICATION, HERE I AM AT UTAH,
 LOGGED INTO THEIR SYSTEM, SENDING YOU A MESSAGE.!!!!
 
 I ABANDONED MY SPECIAL MAGTAPE WRITER.  INSTEAD, I WRITE TAPES
 IN STANDARD TENEX DUMPER FORMAT (THIS IS A FORMAT THAT EVERY
 TENEX SITE CAN READ).  THEN, I WROTE A SAIL PROGRAM THAT CAN
 READ SUCH TAPES ON ANY SYSTEM (INCLUDING, INCDENTALLY, A TENEX
 ONE).  HOWEVER, THIS IS NOT QUITE WHAT IS NEEDED FOR BOOTSTRAPPING,
 BECAUSE THE SITE WILL NOT HAVE SAIL WITH WHICH TO RUN THE
 READER POGRAM.  IF, HOWEVER, THE ONLY THING NEEDED IS TOGET A
 NEW VERSION OF SAIL UP, THEN THIS WORKS FINE.
 
 SO LET ME KNOW....  I WILL BE BACK IN PA BY MONDAY.
 		BOB
 -------


16-MAR-74  1337		network site MAXC
 Date: 16 MAR 1974 1337-PDT
 From: SPROULL at PARC-MAXC
 Subject: -1's
 
 - - - -
 Yes, Dan and I are the source of the -1's. At the time (in the distant
 past) it sounded as if that was the right thing to put there to plese
 DEC.  Unfortunately, DEChas changed its mind several times since then.
 		Bob
 -------

12-MAR-74  1626		network site MAXC
 Date: 12 MAR 1974 1622-PDT
 From: SPROULL at PARC-MAXC
 Subject: Sail bugs
 
 - - - -
 Here are two SAIL bugs for you:
 1.  The compiler does not handle -5 mod 1  correctly (yes, Virginia, both
 	of those are constants).  It does a 'movsi 230007' as the
 	answer!! (Where it gets that number from, I don't care to
 	even guess).
 2.  CVO will not read the most negative number (12 7's) because the
 	IMUL gets overflow when the sign flips.  This
 	causes an overflow trap (when trigs running), and an error
 	message.  The cure (I think -- I fixed this a NIH years ago,
 	and I don't quite remember, so you probably want to think about
 	it a bit) is to put JFCL .+1 under the IMUL, so that the
 	overflow code leves well enough alone.
 
 Bob Smith's new SAIL is really great here.  Boy is it slick.
 So nice.  Productivity up 3% overnight.  Hope the merge goes smoothly
 . Let me know if I can help.
 
 Happy Spring!
 			Bob
 -------
 
12-MAR-74  0310		network site MAXC
 Date: 12 MAR 1974  311-PDT
 From: SWINEHART at PARC-MAXC
 Subject: SAIL BUGS
 
 - - - -
 Have trivial fix for non-constant null-string bug, very non-trivial
 fix for the suspected (now confirmed) flaw in your copy-to-top
 fix (which is in general very nice, not really a kludge, and helps
 a lot in my ultimate fix -- the macro body formal search problem again,
 of course).  This time I really have the fix.  THe problem has always 
 been my saving multiple string pointers on the stack so GC
 would adjust all together.  Now I will just save counters,
 and let substring operator regenerate BPs at right time --
 will save but one string, and your PNAME fix will do the rest -- 
 I'll try to get that into a copy of SYM manana, perhaps try it out somehow.
 Saved one instruction.
 
 anyhow, talk to you tomorrow.
 
 DCS
 -------
 
11-MAR-74  1937		CMU,AIL
 People,
 	We have a new IOSER that mostly reflects some CMU features
 that are within CMUSW -- it also has a couple of minor error msg
 improvements.  We would very appreciate if someone could bless this
 copy and make it be IOSER on S,AIL, so that we won't have to
 edit these changes in in the future.  The new version is
 IOSER[CMU,AIL].  We have labeled our changes with =b4=, all within
 the CMUs.  
 	Please see if this can happen before anymore changes are made
 to IOSER at your end.
 	Would appreciate a confirmation when all is well.
 		thanx,
 			LDE

12-MAR-74  2110		network site CMU
 ***** FTP mail from [A610HS11] (HEARSAY-2)
 JIM,
 	DCS CLAIMS HE HAS A FIX TO BOTH EXTANT
 STRING GARBAGE COLLECTOR PROBLEMS AND THAT HE WILL GET
 THEM IN PRETTY SOON.  (HE JUST TOLD ME ABOUT THIS TODAY.)
 IT WOULD BE NICE IF WE COULD GET THOSE FIXES %IN BEFORE THE
 TENEX MERGER HAPPENS.
 	THANX,
 		LEE
 
6-MAR-74  1453		network site CMU
 ***** FTP mail from [A700SA00] (SAIL)
 CMU bug report, with (hopefully) a fix:	=B3= [LEPRUN /36] LDE (6-Mar-74)
 	FP1 (single word free list) getting fouled up.  Problem seems
 	to be a MOVEM FP,FP1(TABL) near ARYL+5 that looks like it
 	wants to be a HRRM.
 I am somewhat leary of all this stuff, so would appreciate a
 conformation of this ASAP.
 	thanx,
 		Lee
  

23-FEB-74  1258		network site CMU
 CMU bug report.  =B1=  LDE  23-Feb-74.
 	Seems that itemvar arrays get allocated looking
 	like string arrays if they are declared as STRING ITEMVAR ARRAY
 	or STRING ARRAY ITEMVAR ARRAY.  See ITBUG.SAI[A700SA00] or [CMU,AIL].
 	Besides being wasteful of space (twice as long), the accesses to it
 are wrong for non-built in ones, etc.
 
