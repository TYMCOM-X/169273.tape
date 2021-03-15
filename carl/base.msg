From: Joe Smith <JMS@D35> 
Date: Tue, 1 Sep 87 18:21:41 CDT 
To: fletcherc 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: A Varian PDP-10 Base in the Augment area. 

There is an orange PDP-10 dual base complete with "72" CPU in the Augment
junk area at Liberty Street.  It looks to be the base for V70 and V72 from
Valley Forge.  I don't think it belongs in the Augment area.  Shouldn't it
be moved to FSC or IPC property?  /Joe
From: Joe Smith <JMS> 
Date: Tue, 6 Oct 87 20:16:28 CDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: F26 survived a base reload! 

F26's base was having problems (many jobs running LOGINN in TI state) and
it was reloaded.  Host 26 was not available thru net because the EBUS base
was waiting for the PDP10 to tell it what host number to use.  From KLDCP
I was able to deposit a host-to-base message in the ORING specifying the
host number and 127 ports.  When ORPPDP was poked, host 26 became visible.
OPER could log in, but others couldn't because the base thought the PDP10
was SHUT but F26 thought it had answered.  Setting SCHED 400000 and SCHED 0
cleared up that misconception.  I will add the code to send the right
messages to SCNSER; it may be worth adding a DDT patch to all P035/C when
it's ready.  /Joe
From: Osman Guven <OSMAN@X35> 
Date: Mon, 2 Nov 87 17:06:04 PST 
To: Monitor Group <OSMAN>, Joe Smith <JMS@X35>, Carl Baltrunas <Carl@X35> 
Cc: Craig Fletcher <FletcherC@X35>, Dennis Coffey <DenCoff@X35> 
Subject: EBUS E.C.O and /C03 .. 

FOR YOUR INFO ..
I got this mail from Rick and he got it from Tom M.  There seems to be
some confusion: (a)What EBUS E.C.O did to resyncing BASE with the PDP10
(b)What systems running with /C03 monitor, resync patch and (c)What are
the resync condition that /C03 will handle.  I have showed to Rick how
/C03 monitor resyncs with the BASE.  -Osman-
===============================================================================

Osman;

Received the below from T. Marconi - is there anything I have to do,
other than figure it out and inform Operators?

Thanks Rick

--------------------------------------------------------------------

 
                  I  P  S     M  E  M  O  R  A  N  D  U  M         
 
 
   Date: FRIDAY , 10/30/87 12:05:33
 
   To: Rick Daniels
       Jerry Meyer
 
   Cc: Ed Barens
       Bill Fischer
       Ed Roop
       Leland Yarbrough
       Dallas Shift supervisors
 
   From: Tom Marconi
 
   Subj: DECsystem10 P035/C monitor with E-BUS uprev.
   ----------------------------------------------------------------------
     As of today  10-30-1987 all of the Dallas  DECsystem10's are running
   with the  P035/C monitor and the  uprev E-BUS Tymnet board.  With this
   combination we have  seen a drop in NOBASE type  stopcode crashes that
   took the DEC system and the engine  down. With this added value to our
   system, we should see a much needed improvment of CPU reliability.
 
     The CTTY will report the following messages:
 
      %%EVENT stopcode NOBASE at dd-mmm-yyyy hh:mm:ss
      No response from base. Please reload it. PDP-10 is still up.
 
     After the  base is reloaded  or reinit the  DEC system and  the base
   will resync and you will receive the following message twice:
 
      %% EVENT stopcode BASEOK at dd-mmm-yyyy hh:mm:ss
      Code in base is version 2.00 - debug flag is OFF
 
     I wish we had this 5+ years ago.  In any case, this is much like how
   the TYMCOM-370 base/operating system works. If the DEC user has LOGOUT
   on DISCONN, no problem, the user is  logged off the system, but if the
   user has DETACH  on DISCONN this will  put him in a  detach state.  If
   and only IF  the user is smart  they will reattach to  the detach job.
   If the user does not, we will start getting a bunch of control C jobs.
   We can do several things to correct these massive control C's,
 
      1) Change all users to DETACH ON DISCCON with a 15 MINUTE TIMEOUT
      2) Re-educate the user on how and why to reattach.
      3) Keep everything the same and run the FREE program.
 
     I have talked with OSMAN and as of Monday we are going to submit the
   FREE to run  all the time in detach  and kill all the  control C jobs.
   This is just for short term until whom  ever can come up with a better
   solution.
 
     As we all know, this should not be  coded as a crash nor recorded in
   the MUR  database, but FSC  should be  notified for the  simple reason
   that  the base  may have  a  problem and  may  need to  correct it  on
   schedule time.
 
    Any futher questions or comments, please either call me or send me an
   ONTYME.
 
                                 Thank you,
                                Tom Marconi

===============================================================================










Received: from X32.Tymnet by D35.Tymnet; Fri, 4 Mar 88 12:11:30 PST
Return-path: <OSMAN@D35> 
From: Osman Guven <OSMAN@D35> 
Date: Fri, 4 Mar 88 12:10:30 PST 
To: Monitor Group <OSMAN>, Joe Smith <JMS@D35>, Carl Baltrunas <Carl@D35> 
Cc: Craig Fletcher <Fletcherc@D35> 
Subject: B39's Base .. 

o  As of 3/4 10:00 System X32 running with NW2100.BND base code 
   which is same as B39's base.
     ISIS Version:     7.03
     NODECODE Version: 5.21
     EBUS Version:     2.00

o  If problem with B39's base this weekend, I will load
   (BUBBNET:33)NW2674.BND which is the same version of
   ISIS, NODECODE and EBUS as the rest of the base codes.
     ISIS Version:     7.01
     NODECODE Version: 5.10
     EBUS Version:     2.00

This way we will know more about B39 base problem.

-Osman-
Received: from F29.Tymnet by D35.Tymnet; Fri, 1 Apr 88 16:25:58 PST
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 1 Apr 88 16:12:01 PST 
To: kalenda@39 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC 
Subject: B39 base crashes. 

I got your note and agree that you people should be responsible for INIting
B39.  But how long should operations leave B39 down waiting for someone to
look at 2674 via DDT?  I mentioned using INI only if it is necessary to
get B39 back up quickly.

Is it possible to do an up-line dump of node 2674 to ELF or 33 via
gateway?  Right now, I don't think anyone has logged in to 2674 to determine
if slot 1 was stopped due to an ISIS error or a Dispatcher error.

By the way: Although we did generate new code for NW2674, it has never been
loaded.  We had scheduled using the new code, but due to a communications
mixup it was not loaded.  As far as I know, 2674 is running the same code
it always has.

		/Joe
Received: from F29.Tymnet by D35.Tymnet; Wed, 13 Apr 88 16:23:19 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Wed, 13 Apr 88 16:18:58 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: New base code on F38. 

Paul Krumviede (who has a cubicle next to mine in building F) was talking
about upgrading F38's base to the latest version of ISIS and node code.
Something about the old node code being a bottleneck - the maximum amount
of data going thru the base was limited by inefficiencies in node code, not
by the number of sync (or SIO) lines.
			/Joe
Received: from F29.Tymnet by D35.Tymnet; Tue, 3 May 88 11:41:55 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 3 May 88 11:37:49 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: EBUS and time 

(begin forwarded message)

Received: from F33.Tymnet by F29.Tymnet; Mon, 2 May 88 18:48:45 PDT
Return-path: <PKrumv@F33.Tymnet> 
From: Paul Krumviede <PKrumv@F33.Tymnet> 
Date: Mon, 2 May 88 18:09:38 PDT 
To: Joe Smith <JMS@F29> 
Subject: EBUS and time 

Joe,
  Ron and I were a bit curious about the simultaneous crashes of the PDP-10s
in Dallas, and noticed that the base on all the machines that were crashed
by the EBUS had been up a bit over 41 days, which is about the resolution of
31 bits of FASTC.  It looks like the half-second logic in the EBUS saw that
FASTC wrapped, and we then got bit by the nature of signed arithmetic on a
68010.  The EBUS decided that a half-second had passed, looked for a key,
etc.  I guess I should change the branch to something other than BLT...
  I hope this resolves the problem.  Ron commented that this was real
familiar.  So we can get about 41 days 10 hours 12 minutes 20 seconds on
a base.

paul

(end forwarded message)

I had wondered about FASTC overflowing, but did not pursue it.
Friday must have been 41 days after the last power failure.
		/Joe
Received: from F29.Tymnet by D35.Tymnet; Tue, 14 Jun 88 21:05:46 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 14 Jun 88 20:59:41 PDT 
To: danielsr@74 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC 
Subject: New base code for F29 and F58. 

Paul noticed that the TYM files on F33 for nodes 6720 and 6415 were over a
year out-of-date.  In fact, the BND file for 6415 was also wrong.

When I added the changes to the TYM files, I assumed that TYMNET was keeping
the backup directory of (TYM5:33) up to date with the primary directory on
(TYM5:54).  (EBUS)NW6720.BND and (EBUS)NW6415.BND have been updated on
all three systems.

			/Joe
From: Osman Guven <OSMAN@D35> 
Date: Thu, 7 Jul 88 12:18:18 PDT 
To: Monitor Group <OSMAN>, Joe Smith <JMS@D35>, Carl Baltrunas <Carl@D35> 
Cc: Craig Fletcher <Fletcherc@D35> 
Subject: EBUS Base Code .. 

(begin forwarded message)
----------------------------------------------------------------
From: Osman Guven <OSMAN@D35> 
Date: Thu, 7 Jul 88 12:10:09 PDT 
To: qsats.d/ellenberg@ontyme 
Subject: EBUS Base Code .. 

Doug ..

Per our conversation ..

	The "NEW EBUS BASE CODE" which has had a couple of fixes, has been
running in Fremont D. C. successfully.  Dallas D. C. will start running it
this week.  They are loading the bases from user name (EBUS) on systems
25, 54 and 33.  The base codes are named NWxxxx.BND.

	Process of releasing the Base Code officially is in progress with
the NTD people.

-Osman-
----------------------------------------------------------------
(end forwarded message)

Received: from F29.Tymnet by D35.Tymnet; Tue, 19 Jul 88 16:34:28 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 19 Jul 88 16:57:33 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: New code for PDP-10 bases (EBUS). 

(begin forwarded message)

Received: from B39.Tymnet by F29.Tymnet; Tue, 19 Jul 88 16:53:00 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 19 Jul 88 16:26:46 PDT 
To: NTD.BASELINE@ONTYME 
Subject: New code for PDP-10 bases (EBUS). 


        **********************************************
        *      ONTYME REQUEST TO BASELINE MGR.       *
        *          FOR CODE TRANSMITTAL TO           *
        *        BETATEST or SOURCE/BASELINE         *
        *                                            *
        *         form: (baseline)ontyme.doc         *
        **********************************************

To:            Baseline Manager
From:          Joe Smith
Mgr:           Craig Fletcher
Date:          07/19/88
Subject:       Source Patch


Product ID:    EBUS
Product Name:  PDP-10 Base Code
Product Usage: Restricted, Public Net
Test Site:     X32, all PDP-10 systems in Fremont Data Center

Files to be moved:             Cksum:
------------------------       ------
(EBUS:39) EBUS02.O00           JOXRUM
(EBUS:39) EBUS02.W00           WEBSIR


Reason for transmittal of code:
-------------------------------

This release (19-Jul-88) consists of patches to the 68K object code
only.  All changes are stored in EBUS02.O00 ('oh', 'zero', 'zero').

Fix problem with restarting after a Dispatcher crash.
The symptom was that the IPI would immediately crash when the
slot was restarted, causing the slot to crash again.  After
several repetitions, the 68010's stack would overflow and the
entire Engine would have to be reset.

Fix problem with the base zeroing out the PDP-10's ring pointers
when the base was restarted.

Fix problem with the base crashing the PDP-10 host for no
apparent reason 41 days and 10 hours after the node was loaded.

Thank you, Joe Smith (408)922-6220 TXS.J/SMITH
File access this memo: (EBUS:39)EBUS02.002


(end forwarded message)

For Your Info: The form used for sending this message is (BASELINE)ONTYME.DOC
on all code-generation systems.  The previous 68K object file has been
renamed to (EBUS)EBUS02.OL0 ('oh', 'el', 'zero').      /Joe
Received: from F33.Tymnet by D35.Tymnet; Wed, 10 Aug 88 15:06:06 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Wed, 10 Aug 88 14:34:43 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, Paul Krumviede <PKRUMV@33> 
Subject: Average and max chars per second thru the base? 

A question came up while talking with Krumviede: what is the limiting factor
in getting characters into and out of the PDP-10s?  Is it:
 a) the number of characters SCNSER can handle,
 b) the speed in which the 68000 shuffles data from PDP-10 to Engine memory,
 c) the Dispatcher moving data to/from slot 1 and node code,
 d) the thruput of node code talking to the switchers.

The IRING size of 128 words and ORING of 256 means a maximum of 500 in
and 1000 out per tick, or 30K cps in and 60K cps out.  This does not
count block-I/O, but I'm sure that UUOCON can't provide that much
thruput on non-block-I/O lines.  Node code on D25's and D54's bases
reported an average of 700 in/out (at the time we were watching) and a
high-water mark of around 6800 characters per second in and out.

Carl: Did you determine that JBTCNI/JBTCNO was valid or bogus?  I looked at
a couple of systems (including F33-P035/D-7) and noticed that JBTCIN+0 and
JBTCOT+0 were between 1.0 and 2.0 times UPTIME.  That is, an average of 1
character per tick = 60 per second over a 131 hour period.

The question that started this discussion was "would enabling the trace buffers
in the 68000 code slow down thruput?"

The question itself is not as important as the fact that the metrics are not
available.
			/Joe
From: Carl A Baltrunas <Carl@F35.Tymnet> 
Date: Wed, 10 Aug 88 22:47:32 PDT 
To: Joe Smith <JMS>, Osman Guven <OSMAN>, Craig Fletcher <FletcherC> 
Subject: Average/max chars per sec base<==>PDP-10 

At 30 chars/msg = 8 words/msg gives 16 msgs in and 32 msgs out for a more
precise character count of 480 in and 960 out per tick (60th second).  If
the base keeps up with 1/60th second response, the total would be 28,800 in
and 57,600 out cps.  However, I do not know how fast the base polls the ring.
(This is important, since it checks the key each 1/2 second...)

On the KL, we can easily change the size of the ring on a heavily loaded
system to determine if SCNSER is the bottleneck.  The rings can be any size
we want as long as it's a power of 2.  (I'd like to see 1-page or 1K RINGS
for such a test.)  I had done this once, at the time we found the bug in
the KMC code for the 2020.

JBTCNI and JBTCNO are valid and presumed accurate not counting block-io which
is found in JBTBIO  (see (CARL:33)SYSDAT.SAI for actual table if JBTBIO is not
correct).  We do have sampling of the data you request every fifteen minutes
for all systems that ran SYSDAT since the beginnig of June-88.

I think you are right... the real bottleneck may be UUOCON and not SCNSER for
normal I/O.  If you'd like, I can finish whipping up my SHOVEL program to do
just that... shovel data through the interface, using regular I/O, TTCALLS, and
BLOCK-IO or even all three to determine the maximum rate.  (TTCALLS would use
things like OUTSTR/OUTPTR or .AXPTR/.AXSTR).

What did you guys decide?

/Carl
Received: from F33.Tymnet by D35.Tymnet; Thu, 25 Aug 88 15:51:49 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 25 Aug 88 15:44:27 PDT 
To: Ed Barens <IPC.E/Barens@Ontyme>, Jim English <IPC.J/English@Ontyme>, Leland
	Yarbrough <IPC.L/Yarbrough@Ontyme>, Tom Marconi <IPC.T/Marconi@Ontyme>,
	Bill Fischer <IPC.B/Fischer@Ontyme>, Cheryl Eldred
	<IPC.C/Eldred@Ontyme>, Rick Daniels <IPC.R/Daniels@Ontyme>, Ed Roop
	<IPC.E/Roop@Ontyme> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, Paul Krumviede <PKRUMV@33> 
Subject: TYM files for all EBUS bases updated. 

The TYM files have been updated to include features that will help the
REBUILD option.  The new base code is now in (TYMNET:25)ND####.BND and
(TYM5:54)ND####.BND.  Please use them instead of the NW####.BND files the
next time the base is reloaded.

In each TYM file, the symbol REBTST is defined so that OPTION(REBLD) works
right and S0CORE has been bumped up from 240 to 350 to make more room for
the rebuild buffers.

The old BND files have been renamed to BK####.BND on (TYMNET:25) and on
(TYM5:54).

			Joe Smith
			TYMCOM-X Support Group
Received: from F33.Tymnet by D35.Tymnet; Mon, 12 Dec 88 0:02:28 PST
Return-path: <JMS@F74.TYMNET> 
From: Joe Smith <JMS@F74.TYMNET> 
Date: Sun, 11 Dec 88 23:58:18 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: Available memory for slots in X32's base. 

Memory requirements of the PDP-10 bases.

The current set of BND files use 809k of core.  Only 215k bytes are unused.

Node code requires a minimum of 235k, anything above that amount is used for
buffers (needed by the REBUILD option).  All the TYM files have S0CORE
to 350k.  (B39 had problems with rebuilding circuits with less than 300set

The EBUS code in slot 1 needs only 10k, but an additional 256k is allocated
if the option EBDUMP is selected.  This memory is for dumping the 68K's
memory into the Engine to analyze crash dumps.  Nobody uses this option.

Slot 2 had 100k allocated, but needs only 10k for storing the 68K code.

I juggled the memory configurations around, and came up with the following:

Slot  Low  High Core  Use
FF  00000 177FF  93k  Kernel (DDT host 1294)
 0  17800 55FFF 250k  Node Code (XRAY h294)
 1  56000 587FF  10k  EBUS, host 32
 2  58800 5C7FF  16k  68000 code for EBUS
 3  5C800 617FF  20k  Slot for Joe, host 24
 4  61800 667FF  20k  Slot for Carl, host 28
 5  66800 6B7FF  20k  Slot for Osman, host 57
 6  6B800 F07FF 532k  Big slot, host 930 (512k of buffer)
    F0800 FFFFF       unused

See (EBUS:33)TS2100.CTL and .LOG for details.
From: Carl A Baltrunas <Carl@F35.Tymnet> 
Date: Tue, 13 Dec 88 22:32:53 PST 
To: Monitor Group <Carl>, Osman Guven <Osman>, Joe Smith <JMS>, Craig Fletcher
	<fletcherc> 
Subject: ref: EBUS 

I got a call on Friday from someone in TYMNET (I forget her name at the
moment, anyway) trying to determine exactly WHO is responsible for the
maintenance of EBUS and/or IPI, because there is some need to have the
source code moved/archived/translated to SUN.

I also recall Joe mentioning that porting EBUS to SUN s a waste, since
it is only necessary as long as the PDP-10's are around anyway...

Anyway, I wanted to mention this to everyone since, this is YET ANOTHER
area that we can make a precedence for field service company.

/Carl

PS. If I didn't make it clear, I was told that she was told that no one in
    NTD is responsible for EBUS and they may already think that we are the
    people that maintain it.  (However, THAT may not be official, YET!)

Joe:  Comment?  Did she also call you?  I gave her your number in bldg. F.
  7@]%o