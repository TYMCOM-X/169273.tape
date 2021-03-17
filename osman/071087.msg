From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 5 Jan 87 11:13:13 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: F3TSER bug. 

It usually takes 11 tapes to backup 930 and 7 to backup 62.  Did you know it
is possible to save all of 930's files on 2 tapes?  It is very simple.  All
you have to do is mount the second tape without a write-ring.  When you tell
TITO to continue, it appears to save the rest of DSKB but the tape does not
move.  That is, whenever you do a write operation to a write-locked tape on
the F3, F3TSER does not catch the error.  The tape controller refuses to write
to a write-locked tape, and tells F3TSER about the illegal command, but F3TSER
ignores the error, lies to the operating system by saying that the write
request succeeded, and all that wonderful data drops into the bit bucket.

In other news, I was unable to do an all-files on 930 on Friday because the
tape keeps losing vacuum.  Capik has been notified.    /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 6 Jan 87 17:50:14 PST 
To: OSMAN 
Subject: Numbers for RECTBL for 3650-blocks 

To make 3650 blocks work, replace RECTBL in CPXKON with the following:
CPXKON$:
RECTBL+00[   002000,,000000
RECTBL+01[   014000,,000000
RECTBL+02[   026000,,000000
RECTBL+03[   040000,,000000
RECTBL+04[   052000,,000000
RECTBL+05[   064000,,000000
RECTBL+06[   076000,,000000
RECTBL+07[   110000,,000000
RECTBL+10[   122000,,000000
RECTBL+11[   134000,,000000
RECTBL+12[   146000,,000000
RECTBL+13[   160000,,000000
RECTBL+14[   172000,,000000
RECTBL+15[   204000,,000000
RECTBL+16[   216000,,000000
RECTBL+17[   230000,,000000
RECTBL+20[   242000,,000000
RECTBL+21[   254000,,000000
RECTBL+22[   266000,,000000	This address is RECTB1
RECTBL+23[   300000,,000000
RECTBL+24[   312000,,000000
RECTBL+25[   324000,,000000
RECTBL+26[   336000,,000000	This address is RECTB2
RECTBL+27[   350000,,000000
RECTBL+30[   362000,,000000

Note: Since RECTB1 and RECTB2 get overwritten by this changed, the resultant
core image will read 3650-blocks only.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 6 Jan 87 20:37:36 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: 35BP5A ready for testing. 

(OSP)35BP5A.SAV is the same as 35CPXA/35BPXA with the following exceptions:
It uses BP5KON.MAC instead of BPXKON or CPXKON
It uses CO5MOD.MAC instead of COMMOD
CONFIG has been patched to X24-P035/A

It will only talk to 3650 in blocks (25 blocks per track instead of 18) and
has 2 strings defined on BPA.  32BP5A.SAV is similar, it has 1 string on BPA
and 1 string on BPC.  See (OSP)COMMOD.SCM and (OSP)BPXKON.SCM for details.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 6 Jan 87 20:48:56 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Listings of BPXKON 

I have created (M33)BPXKON.LST and (M33)CPXKON.LST for you to look at.  /Joe
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 7 Jan 87 9:45:46 PST 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Keywords: 5 
Subject: I AM IN FREMONT.. 


If anybody needs me I am here testing 35BP5A..
-Osman-
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 7 Jan 87 10:23:44 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: KTEH'S documentary on Silicon Valley. 

Silicon Valley, part 3 (Boomtown) will be shown for the first time on
Wednesday, January 14 at 9pm.  Parts 1 and 2 (Company Town and Hometown)
will be rerun on January 21 and January 28 at 9pm.      /Joe
Received: from X62.Tymnet by X930.Tymnet; Thu, 8 Jan 87 1:34:46 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 8 Jan 87 1:32:09 PST 
To: osman@x930 
Subject: "Osman, System 28 was down when"... 

Osman,
  System 28 was down when I was ready to send my files.  There are 3
copies of ASP.   (CARL:28) is my first version which may or may not
work with SETRES...

(CARL:74,58)ASP11.SAV is the newest latest greatest.
(CARL:74,58)ASP11D.SAV is asp11 with ddt.
(CARL:28)   ASP.SAV   is my first version (see above)

/Carl;
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 8 Jan 87 11:32:20 PST 
To: FletcherC, Osman 
Subject: Testing 3652 

To test 3652 we need to use 35BP5A or 32035A monitor, connect up a string of
3652 disks, and load the 3674 with the microcode that puts the strings
consecutively.  Then define a two-pack system on the first spindle (BPA0 and
BPA10).  If the controller works as advertised, we won't see the contention
problem between the two logical volumes.                /Joe
From: FLETCHERC@X930.Tymnet 
Date: Thu, 8 Jan 87 14:53:17 PST 
To: jms, carl, osman, dencoff 
Subject: "John Healy would like the option"... 


John Healy would like the option of using our 2020 during the afternoons
of the weeks of February 9th and February 23rd for 2020 maintenance courses
he has scheduled.  Do you see any conflicts with his request? Please
let me know by January 12th if there are any problems.

	Craig
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 8 Jan 87 15:27:19 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: use of 2020 

Healy talked to me, I have no problems with him using the 2020.  Steve Capik
will be up tomorrow (Friday) to work on the 3rd RM03.  I plan to start 930's
all-files while he is here so we can watch the tape lose vacuum.  /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 8 Jan 87 16:33:30 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: List of entry points in CPXKON. 

The file (M33)DISKIO.MEM documents the entry points between FILIO and
BPXKON/CPXKON/RMXKON/APXKON.  /joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 8 Jan 87 18:37:59 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, wrs 
Subject: Wonderful F3TSER (revisited) 

Carl and I discovered that if you read a record that is more than 15 pages
long on an F3, then some poor user's UPT gets wiped out.  The unfortunate
job either can't run anything at all, or every command typed will do a
CONTINUE on the current core image.  Anyway, he tried to read 24 pages with
ASP, and that mangled my context pages to the point that everything I typed
threw me back into SED.

%UPS=771000, %ONC=752000, %UPS-%ONC=^D15 pages.   Wonderful.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 10 Jan 87 4:23:53 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: ASP changes and testing 

I believe the changes we discussed for ASP are complete.  I've done
preliminary testing on 930 and the tests were successful.  SAVE and
COMPARE were tested.

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 12 Jan 87 0:24:27 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Oncall 

I am on call as of 25 minutes ago.  I let Osman answer the beeps for 58 and 32
since he was working with them eariler this evening.  I am very disappointed.
FILAUS did not go away after the change to blocks.  Looks like the problem is
not specific to CPXKON since it has been seen with BP5KON and APXKON.  AND
finding two holes in (SYS:32)LUD.SYS is very disturbing.  /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 12 Jan 87 0:30:20 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: FILAUS alias DECUMC+10 

I want to bring up the fact that FILAUS is 'not' a new problem... we may
very well have introduced something else to cause FILAUS or maybe fixed
something else which used to die sooner (never reaching the FILAUS check).
We don't really know.  Our suspicion is that we've introduced something
because we have been seeing more and more of them.

Osman has been backing up crashes for a few years now... it might be well
worth our while to look at some of the old crashes to see if they were
caused by holes in the files.  Does anyone think this tact is worthwhile?

Can we look at the ATB/SPT that is being decremented and figure out which
page in the file we are looking at when the crash occurrs and verify that
there is a hole in the file?

Can we verify that the block we are decrementing is current and not just
old garbage because a pointer didn't get updated?

Can we verify that we don't have multiple pointers for different users or
files pointing to the same blocks, thus one (current) gets updated but the
other (pointing at what it thinks is something else) doesn't?

This doesn't seem to be the same problem we've seen where we get a mixedup
SAT page? or can it be?  ... do the SAT bits get cleared before or after a
DECUMC call?  If indeed we have 1 user and we think we have 2, we should in
some other case have two users and think we have 1, thus the SAT bit gets
deallocated and possibly reused before the 2nd user is done...  that is a
frightening thought!

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 12 Jan 87 0:44:48 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: meetings and my schedule 

I need to have my car worked on (smog test) for my registration renewal
probably by wednesday.  Sine I may be at the mercy of the service station
for scheduling, I may miss one of our 1pm or 10am meetings.  I will try
to get the work done without conflicts, but am mentioning it for your
information.

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 12 Jan 87 19:43:30 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Why it's called the EBUS-BASE. 

The following was found in (940MAN)620IL.NEW

  THERE ARE FIVE BUSSES IN THE VARIAN 620I/L.  THE MOST IMPORTANT ONE IS THE
"E" BUSS WHICH ACTS AS A INTERFACE BETWEEN THE 620I/L AND THE MEMORY INTERFACE
CONNECTION (MIC), WHICH IS CONNECTED TO THE PDP10. THE HOST/BASE OPERATIONS
FLOW THROUGH THE "E" BUSS.
  THE "L" BUSS PROVIDES A 12-BIT PARALLEL COMMUNICATION PATH FROM
THE L REGISTER TO THE ADDRESS DECODERS IN THE MEMORY MODULES.
  THE "W" BUSS PROVIDES A PARALLEL DATA COMMUNICATION PATH (16 BIT)
FROM THE W REGISTER TO THE MEMORY MODULES (UP TO 8). THE OTHER
TWO BUSSES ARE THE "C" AND "S" BUSSES.

Received: from EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 13 Jan 87 12:40:24 PST
From: IPC.L/MORRIS@Ontyme.Tymnet 
Date: Tue, 13 Jan 87 12:40:24 PST 
To: FSC.O/GUVEN@Ontyme.Tymnet 
Message-id: A00858@Ontyme.Tymnet 
Subject: CUPERTINO SHUTDOWN SCHEDULE 

                              M E M O R A N D U M 
     
     
     TO:    DISTRIBUTION 
     
     FROM:  LANDA MORRIS 
            RESOURCE PLANNING & MANAGEMENT   
     
     SUBJ:  CUPERTINO SHUTDOWN SCHEDULE 
     
-------------------------------------------------------------------------  
     
     Below is an update for the Cupertino shutdown plan in relation to
the Fremont Data Center. 
     
     Some entries for the Dallas Center are listed when a Cupertino   
system's hardware and users are being split between both Dallas and   
Fremont.  
     
     Should any notable changes occur, an updated schedule will be sent.   
     
Thank you,
Landa/RPM 
     
                    CUPERTINO -> FREMONT SHUTDOWN SCHEDULE  
     
     
                 KI'S                                    KL'S    
                 ----                                    ----    
     
JANUARY   
-------   
     
   1/2-22                                1/23-25  
        Install 22's <PA> in Fremont            Move F-28 USERS -> F-38    
        for 22's USERS                   1/30-2/1 
   1/16-18                                      Move C-33 USERS -> Fre-    
        Move C-57 USERS -> Dallas' <SS>         mont's <CCC>
   1/16-18
        Move C-22 USERS from <AA> ->    
        Fremont's <PA>   
   1/19-2/5    
        Deinstall C-22's <AA> 
        (Excess H/W)
   1/19-30
        Deinstall C-57's <EW> (Excess   
        H/W - Base -> Fremont for <PB>  
     
FEBRUARY  
--------  
     
   2/2-19                                2/2-13   
        Install <PB> in Fremont for             Deinstall C-33's <VV> 
        70's USERS                       2/6-8    
   2/20-22                                      Move C-26 USERS -> Fre-    
        Move C-70 USERS from <PP> ->            mont's <FFF>
        Fremont's <PB>                   2/9-23   
                                                Deinstall C-26's <ZZ> 
                                         2/14-20  
                                                Ship 33's <VV> -> Fremont  
                                         2/21-3/19
                                                Install <VV> in Fremont    
                                                for 29's USERS   
                                         2/23-27  
                                                Ship 26's <ZZ> -> Fremont  
                                         2/23-3/6 
                                                Deinstall <PP>   
                                         2/28-3/5 
                                                Install <ZZ> in Fremont    
                                                for 39's USERS   
     
MARCH
-----
     
                                         3/6-8    
                                                Move C-39 USERS -> Fre-    
                                                mont's <ZZ> 
                                         3/7-13   
                                                Ship <PP> -> Fremont  
                                         3/9-20   
                                                Deinstall C-39's <RR> 
                                         3/20-22  
                                                Move C-29 USERS -> Fre-    
                                                mont's <VV> 
                                         3/23-4/3 
                                                Deinstall 29's <XX>   
     
     
     
     
  DISTRIBUTION 
  ------------ 
     
  ED BARENS    
  KEN BEDELL   
  FRED CANNING 
  KEN CARTA    
  SHIRLEY CHIANG    
  KENT CLAMPITT
  LLOYD CLAYTOR
  RICK DANIELS 
  CHERYL ELDRED
  JENNIFER ENGLISH  
  BILL FISCHER 
  TERRY FUJIMOTO    
  DAVE GEISLER 
  GREG GRAY    
  OSMAN GUVEN  
  KEN HOLCOMB  
  BRUCE KEIM   
  DICK KOVACH  
  JAN KRIVANEC 
  TOM MARCONI  
  GREG MARUS   
  STEVE METCALF
  GERRY MEYER  
  JIM NORTON   
  BERT NOVAK   
  RAYLENE PAK  
  SUE PAL 
  GARY PEREZ   
  DAN PETROSKY 
  BYRON RAMSEY 
  BILL RICHARDSON   
  LENNY RODRIGUEZ   
  ED ROOP 
  SRAMAIL 
  VIDA STAFFORD
  JULIE WAGNER 
  GARY WALKER  
  BOB WARE
  DAN WEDGE    
  LELAND YARBROUGH
Received: from EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 15 Jan 87 16:41:42 PST
From: FSC.J/ZUMMO@EMSFSC.Ontyme.Tymnet 
Date: 15 JAN 87 15:53:22 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P89544@Ontyme.Tymnet 
Subject: ORDERING PARTS 
NETWORKSPARESORDERING: 

     
                               Memorandum    
     
    Date:  January 15,  1987  
     
      To:  ALL FIELD SERVICE LOCATIONS  
     
     
     
     
    From:  MATERIAL SUPPORT   
     
 Subject:  NETWORK SPARES ORDERING 
     
     
              ************************************************************ 
              *                                                          * 
              *                    McDonnell Douglas                     * 
              *                  Field Service Company                   * 
              *                                                          * 
              *                  SPARES ORDERS PROGRAM                   * 
              *                                                          * 
              ************************************************************ 
     
                                  Documentation   
                                  **************  
     
THE SPARES ORDERS PROGRAM IS AN ON LINE ORDER ENTRY SYSTEM THAT IS USED FOR
ORDERING INITIAL SPARES, INCREASE TO SPARES OR REPLACEMENT OF DEFECTIVE PARTS.  
THIS PROGRAM IS NOT USED FOR PRIORITY ONE ORDERING. ALL ORDERS PLACED ARE  
C1 PRIORITY (REPLENISHMENT). FURTHER DOCUMENTATION AND INSTRUCTIONS CAN BE 
FOUND BY ACCESSING THE PROGRAM.    
     
     
     
     
ACCESSING THE PROGRAM:   
     
1. WHEN THE SYSTEM DISPLAYS THE MESSAGE "Please Type Your Terminal Identifier"  
   OR A STRING OF NONSENSE, ENTER AN " A "   
     
2. THE SYSTEM DISPLAYS THE MESSAGE "Please Log in:"    
     
3. ENTER: FSCORD:55 (Return)  
     
4. THE SYSTEM WILL DISPLAY THE MESSAGE "Password: 
     
5. ENTER: ON-LINE (Return)    
     
6. THE FOLLOWING MENU WILL BE DISPLAYED:
     
     
     
     
     
NOTE: OPTIONS FIVE (5) AND SIX (6) ARE NOT ACTIVE AT THIS TIME.  
     
     
     
     
     
     
     
     
     
                 MCDONNELL DOUGLAS FIELD SERVICE COMPANY    
                             ORDERS PROGRAM  
     
     
     
     
     
       **** SELECTIONS ****   
     
1  READ, WRITE OR DELETE MESSAGES          (MAIL BOX)  
2  SEARCH FOR PART NUMBER                  (NETWORK PART LIST)   
3  SEARCH FOR PART NUMBER                  (MANUFACTURING MASTER PART LIST)
4  PLACE AN ORDER,                         (VALLEY FORGE REPAIR CENTER)    
5  PLACE AN ORDER,  ***NOT ACTIVE***       (IRVINE REPAIR CENTER)
6  PLACE AN ORDER,  ***NOT ACTIVE***       (ATLANTA REPAIR CENTER)    
7  FIELD SITE ADDRESSES  
8  ENGINE TRACKING REPORT
9  LIST PARTS ON BACK ORDER   
9a LIST ALL PARTS SHIPPED                   (FROM REPAIR CENTERS)
10 PRINT DOCUMENTATION/INSTRUCTIONS
11 PULL HARD COPY OF OPEN ORDER    
12 LIST ION CODES (INVENTORY) 
13 LOGISTICS OPTIONS
14 SEARCH FOR AN ONTYME NAME  
     
     
ENTER SELECTION  OR  (Return) TO END:   
     
     
     
     
     
     
CONTROLS: 
     
CONTROL A     BACKSPACE CURSOR            (SAME AS BACKSPACE)    
CONTROL S     TEMPORARILY STOP DATA FLOW. (SAME AS PAUSE)   
CONTROL Q     RESUME DATA FLOW
ESCAPE        LOGOUT OR RETURN TO MAIN MENU  
     
     
     
     
IF YOU HAVE ANY QUESTIONS OR PROBLEMS PLEASE CONTACT MATERIAL SUPPORT 
AT (215)666-9826.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 15 Jan 87 19:45:43 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: (JMS)DSKCLN.TXT 

I put some observations about DSKCLN in (JMS)DSKCLN.TXT
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 18 Jan 87 1:12:54 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: RECOV.SYS lists only files that are completely wiped out. 

Currently, the only files mentioned in RECOV.SYS are those that have been
eliminated because their RIB could not be found or the RIB was a multiply used
page.  For every file listed in RECOV.SYS, there is at least one other that
has been damaged.  Files that have been wounded but not killed are not being
reported.  See (JMS)RECOV.TXT for more info.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 18 Jan 87 1:31:24 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, WRS 
Subject: Soley's modified RPG has been installed on all systems. 

Our users no longer need to make the distinction between "mail waiting" and
"[mail waiting]".  The MAIL command can be used to read either.  RPG now
reads TUMS mail if there is no RPG mail.  Thanks to Bill for this interrim
solution - Carl and I hope to eliminate RPG mail someday.   /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 18 Jan 87 1:35:22 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: DSKCLN actions, BDA 

Re: BDA bit
    My viewpoint on BDA is that we should make it stand for what we have
thought it meant.  This file has been found bad by the damage assement
program, DSKCLN.  At present it means the entire file has been eliminated.
I believe that it is reasonable to use BDA to mean DSKCLN has found anything
wrong with the file.
    This includes bad rib pointers, hard read of a rib page, multiply used
pages, and even bad allocation (although it is possible to have RIBALP set
to a different value than RIBSIZ+<ribs> specifies.  RIBALP specifies the
allocated file size including holes.)
    Things such as wrong UFD time and ufd quota fixups can continue to be
'invisible' to users, since these are expected to be fixed the next time
the user logs in or dskcln runs.

Re: DSKCLN for release...
    To attain our goal of 3650 block mode within a secure environment, we
should modify DSKCLN to put all files which it finds bad, (that is any that
we feel BDA should be set), into RECOV.SYS and print them out on the CTY
as well.  Exactly what happens to multiply used ribs or ribs with hard read
errors is open to discussion.  The main problem with ribs that have hard
read errors is that DSKCLN will mark as FREE any pages in the unreadable rib
and when the rib becomes readable, those pages will be multiply used.
    For multiply used pages, we could give the page to the most recently
created file (if that information were available... it isn't since a newer
creation date is only a data field with a higher number stored in it, not
a real internal creation date).  Otherwise our only solution is to mark it
bad and not to let anyone access that page until all pointers to that page
have been deleted.

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 19 Jan 87 1:00:57 PST 
To: osman 
Subject: P035/A11 in NEWMON. 

The monitor with the REFSTR bug fixed has been put on NEWMON on systems
62, 95, 930, 58, 36, 70, 57, 32.  It has not been telecopied to 22 or 39 yet
(they were down).  It is in (M33) on 26.  32 is already running P035/A11.
Carl will be ready to run the latest DSKCLN in fix mode tomorrow.  /Joe
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 19 Jan 87 12:28:38 PST 
To: JMS 
Subject: MONITORS... 

FINISHED COPYING ..A11 MONITOR TO F22 AND B39.
-OSMAN-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 20 Jan 87 7:23:48 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline calls 


System 22 at 8:45 pm, Monday.  Keeps crashing....

System 58 at 6:00 am, Tuesday.  Hole messages on CTY.

I ran newest DSKCLN in checking mode to re-generate a list of files
for system 58 to restore....

System 22 just keeps crashing... something is dreadfully wrong.
To eliminate the DSKCLN as a cause, I put the OLD version which is
36 plain... back in sys.  It is also there as DSKCLN.S36.  The newest
one is in (OSU:32).

Oops... I meant (OSU:22)   ... I'm fading fast... need to get to sleep.

I spent most of the night trying to track down the 22 stuff using DSKCLN
to trap the bad files so they could be foun as bad.  Apparently the system 
still crashes every so many hours... I suggest trying an older monitor now
that the older DSKCLN is back on SYS:22.  Anything... the current behavior
is unacceptable.

/Carl

PS:  I'm going to bed... please call me around 11 to wake me so we can all
discuss 22 more.... we need to do something... and fast.  /Carl

From: CARL@X930.Tymnet 
Date: Tue, 20 Jan 87 7:30:04 PST 
To: osman, jms 
Subject: "Joe, Feel free to trace the DSKCLN"... 

Joe,
  Feel free to trace the DSKCLN through FILDEL to check it.  It looks more
or less reasonable, but every time I start to spend some time looking at the
data blocks at the time we get to FILDEL, system 22 crashes.  Please try to
look at 22 to see what we can do... 

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 21 Jan 87 21:27:44 PST 
To: Craig Fletcher <FletcherC@X930.Tymnet>, IPC.E/ROOP@ONTYME,
	IPC.S/METCALF@ONTYME, IPC.L/YARBROUGH@ONTYME 
Cc: DenCoff, Osman, Carl 
Subject: The new PDP-10 bases need to be validated. 

Whenever new hardware is assigned to be used as a PDP-10 system, we in TXS
need to be notified as early as possible to modify the configuration files,
and Validations need to be notified so that the new base can be added to the
class of PDP-10 bases.  Failure to do this can cause problems for any of our
customers that need to build aux circuits.

I noticed this problem when I tried to build a circuit to CUD10:930 from the
new KL in Solar Way.  Its base, node 10350, was not in the group and class
list that CUD10 is valid for.

There was not much of a problem when C70 moved from Cup-A to Cup-C since it is
the backup for the private nets and is not used much.  But I see the potential
for severe problems when C22 is moved to the hardware with a new base node.

If what I fear is true, then all of the private net users can log in to F22,
but none of them can build circuits out.  They will not be able to get LOADII
or NAD to build a circuit to their private nodes.  And since C70 is running on
a relatively new base, it is possible they won't be able to C70 either.

			Joe Smith

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 22 Jan 87 5:09:16 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: C22 F22 Information 

Joe, Osman,...
   To add to the morass of confusion...  for what appeared to be no reason
at all, system 22's accounting device was released at 3:20am and the system
went to BOOTS.  Fremont OPS forced a FULL DSKCLN at 3:35 and attempted to
bring the system back up.

   Apparently, CUPC OPS must have take the system down, because around that
time they were starting to do the ASP rebuild of system 22 in CUPC and the
operator forgot about F22 (and since the packs say the same thing, C22 at
the moment there must have been some confusion).   Anyway, CUPC OPS forgot
to do a CHANGE SYS and came up as C22 (after taking F22 down!) and began the
ASP restore.  C22 is on hardware AA base node 2100!

    F-22 finished full DSKCLN sometime between 4:15ish and 4:45ish when OPS
called the hotline because they couldn't login to F22 even though the CTY
said that DSKCLN was completed.  They complained that it told them it was
shut.  (I believe that this information came from probe).

    I tried to login to 22 and got what appeared to be a super-shut system.
OPS reassured me that DSKCLN sait it was done and initialization completed.
I asked OPS to tell me more of what happened.  I went in with probe to check
on 22 and it said SHUT and up on node 2100.  I asked OPS what node F22 was
on and they said 3374.  Hmmmm!  I asked them to check and the base itself
was flashing 3374 they told me.

  I logged in to 930 and printed the conf file and it said 3374, but very
nicely (thank you joe or osman) commented out was hardware 'AA' node 2100!
I then explainded that I thought 22 must have been brought up in CUPC.  I
called BLDG-C OPS and Tom told me they were doing an ASP restore and they
were on the third tape.  I had him check, and apparently the operator had
forgotten to do a CHANGE SYS.  >>ARRRRGH<<<  Tom said he'd have them stop
the ASP and start over after bringing the system back up with a CHANGE SYS.

  I called fremont OPS back and told Lynn that my suspicions were correct.
OPS in CUPC had come up on the net with C22 and that was why F22 went away.
I suspect that CUPC logged in to F22, saw that it said C22 and took it
down... that explains why F22 went down without Fremont OPS knowing about
it, and C22 up on the net SHUT explains why F22 would not come into the
network answered.

  F22 came back up at 5:12 and ran a FAST DSKCLN and was up answered by
5:20 am.  C22 is up as "12 SHUT 0 IIX-N EBUS SLOT# 1" and all is well in
the world.

  (except that I've lost another hour of sleep and both my wife and I
   were awakened unnecessarily.. sigh)

/Carl
See the OPER MAIL on F22 for fremont OPS side of the story!
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 22 Jan 87 10:41:52 PST
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 22 JAN 87 10:01:33 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TYMOPS.V/STAFFORD@Ontyme.Tymnet, TYMIS.J/WAGNER@Ontyme.Tymnet,
	TXS.J/SMITH@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	FIN.R/KOVACH@Ontyme.Tymnet, IPC.C/ELDRED@Ontyme.Tymnet,
	IPC.S/MARCOTTE@Ontyme.Tymnet, OPERA.SUP@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet, IPC.L/MORRIS@Ontyme.Tymnet,
	IPC.S/PAL@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.C/FLETCHER@Ontyme.Tymnet 
Message-id: A06037@Ontyme.Tymnet 
Subject: "M E M O R A N D U M DATE> 22 JAN"... 

                         M E M O R A N D U M





DATE>      22 JAN 87  09:54

TO>        Vida Stafford
           Landa Morris
           Julie Wagner
           Ed Roop

COPIES>    Dick Kovach
           Joe Smith
           Osman Guven
           Cheryl Eldred

FROM>      Rick Daniels


SUBJECT>   New Nodes coming up in Fremont


-----------------------------------------------------------------------



The following three Ontymes from RPM indicates the NODES that are to
be transferred/brought up in Fremont along with the Host Numbers.

To prevent having the same fiasco that happened with system 22 when it came
up in Fremont, it is requested that the appropriate departments take whatever
action is neccessay to insure these nodes are set up correctly, i e. in
the correct GROUP TABLE and MAGNUM Data bases.

For your information System 33 will be coming up on NODE 2333 the weekend
of 30 January 1987.  It is presently on NODE 6415.

Users on these systems must be able to build aux circuits to and from these
Systems.

The two new NODES in Fremont ND 3374 and ND 10350 are here.  If Vialidations
                                                                Page  2

is responsible in setting these NODES up, it is requested I be Ontyme when
everything is completed.  (OPERA.SUP).

Please contact me at (415) 498-2595 if you have questions.

Thanks Rick Daniels


                           M E M O R A N D U M

     ATTN:  JULIE WAGNER

   COPIES:  RICK DANIELS
            SUE PAL

     FROM:  LANDA MORRIS

     SUBJ:  NODE #4525/C-57

-------------------------------------------------------------------------

    THIS IS TO REQUEST DEINSTALLATION OF NODE #4525 CURRENTLY BEING USED
IN CupC WITH HOST 57.  THE DEINSTALLATION CAN BEGIN ON MONDAY, JANUARY 19
1987.

     NODE #4525 WILL BE SHIPPED TO FREMONT BY IPC AND WILL BE USED WITH
HOST 70 IN THE FUTURE.  THE CODE NEEDS TO BE READY FOR THIS BY MONDAY,
FEBRUARY 2, 1987.

     THE NEW COST CODE WILL BE FREMONT'S #2018 AND CID #378.

     SITE CONTACTS WILL BE SUE PAL IN CUPERTINO AND RICK DANIELS AT THE
FREMONT DATA CENTER.

     I CAN BE REACHED AT (408)446-6870 OR ONTYME IPC.L/MORRIS.


THANK YOU,
LANDA/RPM


                           M E M O R A N D U M


     ATTN:  JULIE WAGNER

   COPIES:  RICK DANIELS
            SUE PAL

     FROM:  LANDA MORRIS

     SUBJ:  NODE #6415/C-33

--------------------------------------------------------------------------

     This is to request deinstallation of node #6415 currently on-line
with system 33 in CupC.  The deinstallation can begin on Monday, 2-2-87.

                                                                Page  3

     Node #6415 will be shipped to the Fremont Data Center by IPC and will
be used with system 29 in the future.  The code needs to be ready for this
by Monday, 2-23-87.

     The new cost code will be Fremont's #2018, Cid #378.

     The site contacts are Sue Pal at the Cupertino Center, and Rick
Daniels in Fremont.

     I can be reached at (408)446-6870.


Thank you,
Landa/RPM



                           M E M O R A N D U M


     ATTN:  JULIE WAGNER

   COPIES:  RICK DANIELS
            SUE PAL

     FROM:  LANDA MORRIS

     SUBJ:  NODE #4200/C-26

-------------------------------------------------------------------------

     AS PART OF THE CUPERTINO SHUTDOWN PLAN, NODE #4200 NEEDS TO BE
DISCONNECTED FROM CUPC.  IT IS CURRENTLY BEING USED WITH HOST 26, A PDP
KL SYSTEM.  THE HARDWARE WILL BE MOVED TO FREMONT BY IPC THE LAST WEEK
OF FEBRUARY.

     THE NODE SHOULD BE DISCONNECTED EFFECTIVE MONDAY, 2-9-87 FROM CUPC
AND CODE NEEDS TO BE AVAILABLE BY FRIDAY, 2-27-87.  IT WILL BE USED WITH
SYSTEM 39 IN FREMONT.  IT WILL BE DEDICATED TO THE NTD GROUP AND REQUIRES
BUBBNET SERIVCE.

     PLEASE TRANSFER COST CODE CHARGES TO #2018, CID #378.

     THE SITE CONTACT FOR CUPC IS SUE PAL, AND FREMONT'S IS RICK DANIELS.

     IF YOU NEED MORE INFORMATION, MY NUMBER IS (408)446-6870.


THANK YOU,
LANDA/RPM
From: Joe Smith <JMS@X930> 
Date: Fri, 23 Jan 87 0:44:23 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: DSKCLN version 36J(102) works. 

The results are in (SYS)RECOV.SYS and readable by CTYLOG.
The only difference between 36J(101) and 36J(102) is that the latter outputs
"Multiply used pages - starting pass 2" and "SATs written to disk", even when
in fix mode.  (I wanted it to be obvious why DSKCLN was taking twice as long as
usual.)

Because SPAGES was bad on C26, DSKCLN always found multiply used pages, and
always did pass 2.  That's why DSKCLN took twice as long on C26 than the
other systems.

It will look very impressive when we tell them that the new version of DSKCLN
will eliminate the FILAUS problem and run twice as fast as the old one.
It will be impressive after the existing bad files are eliminated.
			/Joe
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 23 Jan 87 8:39:42 PST
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 23 JAN 87 08:01:20 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.O/GUVEN@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A06645@Ontyme.Tymnet 
Subject: "Osman; The below is forwarded"... 

Osman;

The below is forwarded for your information.

Thanks Rick

-----

                            M E M O R A N D U M


                                                    [] TYMNET - INTERNAL
DATE>      22 JAN 87  17:50                            SERVICES

TO>        RICK DANIELS

COPIES>    KATHLEEN KELLY
           LANDA MORRIS
           PATTY QUEVEDO
           ED ROOP
           VIDA STAFFORD

FROM>      JULIE WAGNER
           INTERNAL SERVICES


SUBJECT>   GROUP TABLE CHANGES FOR IPC ORDERS


-----------------------------------------------------------------------


THANK YOU FOR YOUR RECENT MEMO REGARDING THE PROBLEMS WITH BRINGING UP
HOST 22 ON NODE 2333 IN FREMONT.  AFTER REVIEWING THE ORDER WITH 
TYMNET VALIDATIONS, WE FOUND WHERE OUR ORDER PROCESS BROKE DOWN.

TO PREVENT THIS FROM HAPPENING AGAIN COMMUNICATION TO TYMNET VALIDATIONS
WILL BE MADE BY TYMNET INTERNAL SERVICES AT THE TIME OF NOTIFICATION BY
IPC OF CHANGES AND NEW ADDITIONS TO YOUR BASE HOSTS.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 23 Jan 87 14:39:48 PST
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 23 JAN 87 13:15:43 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.O/GUVEN@Ontyme.Tymnet, OPERA.SUP@Ontyme.Tymnet 
Message-id: A06945@Ontyme.Tymnet 
Subject: "Okay folks!! Everyone been asking"... 

Okay folks!! Everyone 's been asking what's happening, well here it is -
for this weekend at least.

F28

Complete ALL FILES, RAWACT, BACKUP.

1700 - SHUT System and start storing users files to tape.  Restore to 38
as they complete.  (If Jan Krivanec calls and needs to get on 28, UNSHT system
so she can get on.  Once she is on, SHUT the system).

The above refers to PPR 28309.  A command files has been set up to do this
job.  See Brad for details.  When entering these tapes in the Library don't
put system 28 anywhere is the label.  

F58

Start ALL FILES  after BACKUP tonight.  You cannot do the RESTORE until everything
is completed on System 28.  If they get the tape controller and tape drive
installed and checked out on the KL (58 NEW), you can start the restore.


If, by the time all users are stored to tape on 28,and the tape controller and
tape drive is not working on the new KL, reconfigure the tape drives on 28 to
restore the ALL FILES to 58.

Osman will more than likely be on site as he has to do some testing on 58.
After he is through, the new KL and 58 that is on hardware EEE is taken down.
Page on-call fsc.  They will recable the DISK DRIVES that are running on the new KL
to hardware EEE.
f74

I will be in Sunday night/monday morning to oversee this project.

NOTE;  I'll will be unavailable from 1700 - 2200 tohight.  I will call in
as soon as I return home.

Thanks Rick
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 23 Jan 87 14:39:58 PST
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 23 JAN 87 13:21:31 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (26 names) 
Message-id: A06950@Ontyme.Tymnet 
Subject: "SUBJ: REFERENCE MY ONTYME 06942."... 

SUBJ: 


REFERENCE MY ONTYME 06942.

The RESTSORE to 58 cannot be completed until after OSMAN has done his
testing.

Thanks Rick.
From: Joe Smith <JMS@X930> 
Date: Fri, 23 Jan 87 22:05:19 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, wrs 
Subject: Bug in TELECO/AUXMOX - dies after TALK ALL. 

At 20 minutes before 10pm, AUXMOX stopped working on system 55.  It died
again, 10 minutes before 10pm.  I was watching with SYSTAT CONT:30.  It
was receiving 5 pages per 30 seconds up until the message "System going
down in 10 minutes" showed up on my screen.  Then it hung for about 5
minutes, not receiving any data.  About that time TELECO on 930 gave up
waiting and aborted.  AUXMOX should have requested that TELECO re-transmit
the last packet.  Bug.
From: Joe Smith <JMS@X930> 
Date: Sat, 24 Jan 87 1:46:31 PST 
To: OSMAN 
Subject: CTYLOG mods in (OSP). 

32035A, 22035A, 57035A, 55035A, and 58035A in (OSU) all have the CTYLOG
feature included.  These 4 and 1/2 systems are the ones that have moved
recently.   /Joe
From: Joe Smith <JMS@X930> 
Date: Sat, 24 Jan 87 19:19:45 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Manual fix of SPAGES on 26, 70 not complete. 

All 3 of the trouble files in (OSNF:26) have "1" punched in the prime RIB
and a FMUP in the spare RIB that still points to SPAGES.  See DSKCLN.LOG
in (JMS:26),(JMS:70),(JMS:58) for details.    /Joe
From: Osman Guven <osman@X930.Tymnet> 
Date: Sun, 25 Jan 87 5:35:46 PST 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: F58 Move to 3650 BLOCK disk.. 

1. F58 MOVE TO 3650 BLOCK MODE DISK IS COMPLETED.
2. AS OF 1/25/87 04:00 SYSTEM IS UP FOR T/S.
3. SYSTEM IS DEFINED FOR 12 PACK SYSTEM AND HAVE 4
   GOOD SPARE DISK.
4. ASP SAVE/RESTOR WORKS, PLEASE RESUME ASP SAVE. 
5. IF ANY PROBLEM WITH SYSTEM F58 OR ANY QUESTION
   PLEASE NOTIFY TYMCOM-X SOFTWARE GROUP.
6. NEW KL-10 IS NOW CONNECTED TO THE 3652. I BUILD
   ONE PACK BLOCK SYSTEM AND IT IS UP.
-Osman-
From: CARL@X930.Tymnet 
Date: Mon, 26 Jan 87 0:54:13 PST 
To: jms, osman 
Subject: "Re: Busy weekend... etc. Because"... 

Re: Busy weekend... etc.
Because of the schedule of things here at home, I haven't had any time to
make the modifications I'd hoped to have ready today.  I expect to be in
by noon and have something ready in the afternoon.  See you then.

/Carl
Received: from X62.Tymnet by X930.Tymnet; Mon, 26 Jan 87 8:41:21 PST
Return-path: <DenCoff@X62> 
From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X62> 
Date: Mon, 26 Jan 87 8:37:48 PST 
To: (Monitor Group:), Carl, Osman, Joe <JMS> 
Cc: Craig <FletcherC> 
Subject: Absence from office. 

I'll not be in today.  I have a bad cold, with laryngitis.  The cold is pretty
severe, but I am now on the mend.  I hope to be in Tuesday.
-D.-

From: FLETCHERC@X930.Tymnet 
Date: Mon, 26 Jan 87 14:41:18 PST 
To: osman 
Subject: "Related to our move to ALbrae."... 

Related to our move to ALbrae.
Can you find out if the engine there has or can hold all the
required hardware to connect 930,62,95 and Eric Dortman's system(s)
to the network. We may have to order some equipment from TYMNET.
Thanks.

	Craig
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 27 Jan 87 4:30:17 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: AHS results 

Joe,
  I have some interesting results to bring up.  I've only
seen the results on one system (26) so far but,
I am confident that a repeat of the
run will produce similar results.  If
this is rtrue, then we have a fix on waht
is happenning with ASP, and why it doesn't work.

Interesting.
From: CARL@X930.Tymnet 
Date: Tue, 27 Jan 87 9:49:55 PST 
To: osman, fletcherc 
Subject: "I may be a few minutes late for"... 

I may be a few minutes late for our 10am meeting.  Am on my way.
/Carl
From: Joe Smith <JMS@X930> 
Date: Tue, 27 Jan 87 16:34:06 PST 
To: osman 
Subject: New monitor and BOOTS 

(M33:38)32BA12.SAV is built with BP5KON for testing 3652 in blocks.
(M33:38)32PA12.SAV is built with CPXKON for testing 3652 in pages.
(M33:38)BOOT5B.KL is for testing fast BOOTS on 3652 in blocks.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 27 Jan 87 18:56:55 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, William R Soley <wrs@C39.Tymnet> 
Subject: MAPSUP.PAT fixing supermap error 

Systems F22, D23, C26, C29, F30, C33, B36, F38, B39, F58, F70 and F74
have all been patched in teh running monitor, SYSTEM.SAV and NEWMON.SAV
to reflect the change to MAPUS4+22/ CAMGE M,UNIPPU(U) instead of CAMG...

This fixes an OLD off by one bug when doing super-map i/o to any flavor
disks when a single map request split across two units.  This fix should
make ASP more reliable and we should no longer see any bad files after
an ASP restore (unless the files were already damaged before the SAVE).
/Carl
From: Joe Smith <JMS@X930> 
Date: Wed, 28 Jan 87 0:59:27 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: 58's disks looking good. 

The following glitches can be explained.  System OK otherwise.

From: OPER
Date: JANUARY 27,1987 9:31
F58/EEE HAS 4 HDEV ERRORS ON BPA13 (580009) AT 0850 PST 1/27/87.
BPA12 (580008) HAS 2 THUNGS.
BPA15 (580011) HAS 1 THUNG.

Date: JANUARY 27,1987 18:38
P035/A07 patched to become P035/A-1 in SYS:SYSTEM.SAV only.
Fix super map errors across unit boundaries (which caused THUNGs).  /CARL

From: JMS
Date: JANUARY 28,1987 0:49
Whenever you do a PSP on system 58, you will find that SYSTAT does not know
how to decode SLBN and HLBN on 3650 disks.  In particular, the hard read error
on DSKB11(BPA13) is at 200022,,000511 octal.  For a 3330, that page number is
CYL:3 HEAD:16 RECORD:10, but since 58 is running on 3650 disks, the bad spot is
really on CYL:1 HEAD:22 RECORD:18.  That corresponds to the HRE found in
(SYS)SPAGES.SYS, which can safely be ignored.   /Joe
From: Joe Smith <JMS@X930> 
Date: Wed, 28 Jan 87 15:14:13 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, wrs 
Subject: Problem with ASP located and fixed. 

Every ASP tape created before 28-Jan-87 is missing data.  For every three
disks saved to tape, two pages were missing.  They are page 5 on disk 3n+1
and page 15 of disk 3n+2.  These were not picked up due to a long standing
bug in MAPIO that handles the CHANIO function to super-map disk pages.

The missing pages on disk 3n+1 caused no trouble, since it is the third
page of BOOTS, which was never used (BOOTS is actually 1.75 pages long), and
was re-written by running BOOTS.KI or BOOTS.KL.

The octal addresses of the other missing pages were page 17 on unit 2, page 17
on unit 5, page 17 on unit 10, page 17 on unit 13, page 17 on unit 16.  For
systems with 3 strings, page 17 on unit 21 and page 17 on unit 24 would also
be missing.  This means that after doing an ASP restore on a 14-pack system,
4 files would lose one page of data each.  If the page wasn't a RIB, there
would be no indication.  Instead of a page of data, the file would have a
page full of zeros that the formatter (PAKCOP) had written and ASP hadn't.

How the problem occurred:

When ASP read from the disk, it would do so by super-mapping a range of 24
pages.  When it got to the end of DSKB0, the request of 24 pages would
encompass the last 16 pages of DSKB0 and the first 6 pages of DSKB1.  Due to
the bug in MAPIO, the first 16 pages in the range would be mapped correctly,
but the 17th would get mapped to the first page on cylinder 808 (which is not
allowed) and the 18th thru 24th would get mapped to pages 0 thru 4 on DSKB1
(instead of 0 thru 5).  The next group of 24 pages would get mapped to pages
6 thru 31 on DSKB1, which means that page 5 of DSKB1 never got read from the
disk.  On restore, the same bug meant that page 5 of DSKB1 never got written
back to the disk.

As mentioned earlier, missing page 5 is no big deal.  However, page 15 (octal
17) was most likely part of someone's file.  No pages were missing from DSKB0,
DSKB3, DSKB6, etc since 3*68680 is evenly divisible by 24.

For 3650 disks formatted in pages (C26, C70, and F58's old disks), the pattern
repeated evry 4 disks.  Disk 4n+0 lost nothing, 4n+1 lost page 17 (octal 21),
4n+2 lost page 11 (octal 13), and 4n+3 lost page 5 (BOOTS again).  Page 13
on DSKB2 was part of [1,4].UFD, losing meant losing 102 files from SYS.

This bug has existed ever since P033 (when TYMCOM-X converted to pages) and
is now fixed.  The DSKCLN bug has also existed for the same period of time.
Since P033 came out in the summer of 1977, these are "ten-year bugs".

[End of (M33)SUPMAP.TXT]
Received: from EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 29 Jan 87 11:26:52 PST
From: IPC.L/THOMAS@EMSFSC.Ontyme.Tymnet 
Date: 29 JAN 87 09:05:09 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: A10055@Ontyme.Tymnet 
Subject: F22 Review of Problems 

Date:       January 28, 1987
 
To:         Gary Walker
 
Copies:     Rick Daniels, Steve Metcalf, Sue Pal, Bill Fischer
            Osman Guven, Cheryl Eldred
 
From:       Ed Roop
 
Subject:    F22 Review of Problems
 
 
Gary,
 
Here is a written follow-up to the several conversations we had    
on F22's reliability and disk storage integrity.
 
The  users  of System 22 experienced two problems when  migrating 
from  the  Cupertino center to Fremont data  center.   The  first 
problem was the lack of AUX circuit capability.  This was brought 
on by using a new node for F22 in Fremont.  The node table in the 
Supervisor never was updated to reflect the new node number.  The 
paperwork breakdown has been analyzed and changes put in place to 
eliminate this problem.   The second problem, the more serious of 
the  two,  was the lack of integrity of the disk file  structure.  
Initially  the disk file errors appeared to be a problem  of  the 
move.   Fremont  operations rebuilt the system on January 20th in 
hopes of correcting the problem.  Problem calls continued through 
the 21th causing operations to move C22 disk packs to Fremont  to 
run  on F22.   This resulted in a loss of three days' updates  to 
F22 users.   The daily backup tapes were not used to minimize the 
lost  days  because of concern on the validity of files on  those 
tapes.   On  further  review of the disk problems, we now believe 
most  of the disk file damage occurred prior to the move of  this 
system.   Below is a brief list of System outages caused by  disk 
problems.
 
       Nov. 25          Dec. 8         Jan. 2
                        Dec. 14        Jan. 4
                        Dec. 16        Jan. 15
                        Dec. 17
                        Dec. 22
 
The   following  are  known  usernames  affected  that had  files 
restored:
 
         DOLCODE             VKL             DUCODE
         ENSS                ANZNET          PHILIPNET
         BOFACODE            HSBCCODES       NSCNGT
         NSSU                CIDNCODE        MWALTERS
         DBRUNET             MPL             INTLTECH
 
 
 
 

Page 2
 
 
The  system  software and hardware on F22 is now  stabilized  and 
running well.   We know of no remaining disk file damage.  But if 
you  or  the  field would want  to  verify  files,  the  critical 
timeframe would be from November 25 to January 22.
 
If  I can be of further help on this matter,  please feel free to 
contact me.
 
Ed
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 30 Jan 87 0:04:24 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: ASP saves and rebuilds 

We have a "small" problem with ASP rebuilds.  I called Fremont ops to
determine if operations records what monitor was used to take the ASP
dumps.  The answer is, they don't... however, at the beginning of each
shift, they log what monitor is up on each system, so by extrapolation
it can be determined what monitor was "known" to be running.

Re:  What happens if an ASP dump is taken with ???-P034/P37 and they
     attempt to do the restore using ???-P034/P37-1 where -1 is the new
     patch MAPSUP.PAT fixing the off by 1 supermap.

What happens is that on selected units, the first n pages (where n is
determined using the formula that Joe mentioned in his last message on
this topic) are shifted on the disk by 1 page.

This means that the system won't come up, because most likely one or both
of the HOM pages are in the wrong places and the disk looks scrambled!

(This also means that Osman should NOT use the new monitor to restore any
 old ASP tapes on the test systems... otherwise we will think something
 new is broken, until we realize what happenned!)

I called Fremont and CUPC ops and told them that they need to contact
software support BEFORE doing any ASP rebuilds for the next few weeks
so that we can determine which monitor they should use to do the rebuild.

Tomorrow, we can talk to Rick Daniels and get it squared away for all of
the data centers.  I also think that we need to have operations log what
monitor is used in all future TITO and ASP saves and restores so that any
problems caused by monitor or program changes can be detected.
(That also means logging TITO and ASP version numbers)

When I patched CUPC and FREMONT hosts earlier this week, I did not patch
the monitor version number, so if a rebuild is necessary we need to get
the time and date to match against when the systems were patched. PATLOG
exists on most of the systems.

Osman, have all of the Dallas machines been patched?  We ought to collect
a list of times for each system... I will start such a list in the file
(m33:930)SYSPAT.INF for just such a purpose.

/Carl
Received: from EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 30 Jan 87 12:40:29 PST
From: IPC.L/MORRIS@EMSFSC.Ontyme.Tymnet 
Date: 30 JAN 87 10:35:00 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: A10903@Ontyme.Tymnet 
Subject: CUPERTINO SHUTDOWN SCHEDULE 

                              M E M O R A N D U M 
     
     
     TO:    DISTRIBUTION 
     
     FROM:  LANDA MORRIS 
            RESOURCE PLANNING & MANAGEMENT   
     
     SUBJ:  CUPERTINO SHUTDOWN SCHEDULE 
     
-------------------------------------------------------------------------  
     
     Below is an update for the Cupertino shutdown plan in relation to
the Fremont Data Center. 
     
     Some entries for the Dallas Center are listed when a Cupertino   
system's hardware and users are being split between both Dallas and   
Fremont.  
     
     Should any notable changes occur, an updated schedule will be sent.   
     
Thank you,
Landa/RPM 
     
                    CUPERTINO -> FREMONT SHUTDOWN SCHEDULE  
     
     
                 KI'S                                    KL'S    
                 ----                                    ----    
     
JANUARY   
-------   
     
 * 1/2-22                              * 1/23-25  
        Install 22's <PA> in Fremont            Move F-28 USERS -> F-38    
        for 22's USERS                   1/30-2/1 
 * 1/16-18                                      Move C-33 USERS -> Fre-    
        Move C-57 USERS -> Dallas' <SS>         mont's <CCC>
 * 1/16-18
        Move C-22 USERS from <AA> ->    
        Fremont's <PA>   
   1/19-2/5    
        Deinstall C-22's <AA> 
        (Parts -> Fremont)    
 * 1/19-30
        Deinstall C-57's <EW> (Excess   
        H/W - Base -> Fremont for <PB>  
     
FEBRUARY  
--------  
     
   2/7-19                                2/2-20   
        Install <PB> in Fremont for             Deinstall C-33's <VV> 
        70's USERS                       2/9-13   
   2/20-22                                      Ship 55's <FF> -> Fremont  
        Move C-70 USERS from <PP> ->     2/13-15  
        Fremont's <PB>                          Move 26's USERS -> Fre-    
   2/23-3/6                                     mont's <FFF>
        Deinstall <PP>                   2/16-27  
                                                Deinstall 26's <ZZ>   
                                         2/16-3/5 
                                                Install <FF> in Fremont    
                                                for 39's USERS   
                                         2/23-27  
                                                Ship 33's <VV> -> Fremont  
     
MARCH
-----
     
   3/7-13                                3/2-19   
        Ship <PP> -> Fremont                    Install <VV> in Fremont    
                                                for 29's USERS   
                                         3/6-8    
                                                Move C-39 USERS -> Fre-    
                                                mont's <FF> 
                                         3/9-20   
                                                Deinstall 39's <RR>   
                                                (H/W -> ?)  
                                         3/20-22  
                                                Move 29's USERS -> Fre-    
                                                mont's <VV> 
                                         3/23-4/3 
                                                Deinstall 29's <XX>   
                                                (H/W -> Dallas)  
     
APRIL
-----
     
                         4/6-5/15  
                              Clean-up & move from CupC
MAY  
---  
     
                                         5/1-31   
                                                Remove C-36 from service   
                                                (H/W -> ?)  
     
  *  COMPLETED 
     
     
     
  DISTRIBUTION 
  ------------ 
     
  ED BARENS    
  KEN BEDELL   
  FRED CANNING 
  KEN CARTA    
  SHIRLEY CHIANG    
  KENT CLAMPITT
  LLOYD CLAYTOR
  LEE CORBIN   
  RICK DANIELS 
  CHERYL ELDRED
  JENNIFER ENGLISH  
  BILL FISCHER 
  TERRY FUJIMOTO    
  DAVE GEISLER 
  GREG GRAY    
  OSMAN GUVEN  
  KEN HOLCOMB  
  BRUCE KEIM   
  DICK KOVACH  
  JAN KRIVANEC 
  TOM MARCONI  
  GREG MARUS   
  STEVE METCALF
  GERRY MEYER  
  JIM NORTON   
  BERT NOVAK   
  RAYLENE PAK  
  SUE PAL 
  GARY PEREZ   
  DAN PETROSKY 
  BYRON RAMSEY 
  BILL RICHARDSON   
  LENNY RODRIGUEZ   
  ED ROOP 
  SRAMAIL 
  VIDA STAFFORD
  JULIE WAGNER 
  GARY WALKER  
  BOB WARE
  DAN WEDGE    
  LELAND YARBROUGH
From: Joe Smith <JMS@X930> 
Date: Fri, 30 Jan 87 18:39:47 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: C70 is still up. 

(M33:930)NOTICE.70 has not been updated since 7-Jan-87.  Last I checked, its
uptime was 552.5 hours.  That's 23 days of continuous uptime!
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 30 Jan 87 18:46:27 PST 
To: Joe Smith <JMS@X930> 
Cc: Monitor Group <JMS>, Carl, FletcherC, DenCoff 
Subject: Re: C70 is still up. 
In-reply-to: your message of Fri, 30 Jan 87 18:39:47 PST

Record for a PDP-10 for Tymshare was 1600 hrs with
1100 hrs and system was taken down and another 500 hrs
before it actualy crashed with P/E. It was system C36 when
it was a KI-10.
-Osman-
From: Joe Smith <JMS@X930> 
Date: Sun, 1 Feb 87 16:04:30 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Non-optimal page allocation still in P035/B. 

1-Feb-87 at 14:56 - changed GETAP0+2 in SWAMP from MOVEI T3,1 to MOVEI T3,0.
This change to the running monitor was to make C26's files more equally
distributed on X32's disks in Fremont.

Before the patch is the column labeled "one".  At the instant I ran SYSTAT,
SWAMP was picking on BPA12 only.  After the patch, column "three" shows pages
being allocated from BPA13+BPA14+BPA15.  Once the free space on those three
packs matched that on BPA12, then it was also used (column "four").  As soon as
BPA11 thru BPA15 got down to 76648 pages each, then new pages were allocated
equally on all 12 packs.  The column marked "all" was at 15:30.

           one    three     four      all
DSKB:   953143   945496   937572   907247
BPA2     76648    76648    76648    75604
BPA3     76648    76648    76648    75605
BPA4     76648    76648    76648    75603
BPA5     76648    76648    76648    75605
BPA6     76648    76648    76648    75602
BPA7     76648    76648    76648    75605
BPA10    76648    76648    76648    75605
BPA11    76717    76717    76717    75602
BPA12    81893    81144    81016    75602
BPA13    85863    83580    81016    75602
BPA14    85863    83580    81017    75602
BPA15    85863    83580    81017    75602


Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 2 Feb 87 8:40:17 PST
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 02 FEB 87 06:56:35 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (26 names) 
Message-id: A11689@Ontyme.Tymnet 
Subject: "SUBJ: All Operators; Keep SSW"... 

SUBJ: 

All Operators;

Keep SSW 4 set on all PDP-10's.  If system should crash

or be recycled, bring it up on (SYS)NEWMON.SAV until further notice.

Thanks Rick
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 2 Feb 87 10:39:55 PST
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 02 FEB 87 10:05:28 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (27 names) 
Message-id: A11909@Ontyme.Tymnet 
Subject: ASP Dump Rebuilds 

                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      02 FEB 87  10:04

TO>        Shift Supervisors
           All Operators

COPIES>    Cheryl Eldred
           Steve Metcalf

FROM>      Rick Daniels


SUBJECT>   ASP Dump Rebuilds


-----------------------------------------------------------------------


I have just been informed that after all TYMCOM-10's come up on
(SYS)NEWMON.SAV, we cannot rebuild the system with  the ASP Dumps
that have already been taken.

Software Support is to be notified if this  should occur.

Once we have taken an ASP dump with monitor P035/B, the rebuild
procedures will be the same.

From what I understand, we have to be running on the present (SYS)SYSTEM.
SAV monitor to use previously taken ASP Dumps.
From: Joe Smith <JMS@X930> 
Date: Mon, 2 Feb 87 12:43:22 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: DSKCLN version 36J(103) created. 

Known deficiency in DSKCLN:  After doing a restore from TITO all-files, every
	single file on the disk has RIPALC set.  When DSKCLN is creating the
	linked list of files that need fixing, it runs out of core.  The
	program CLRALC turns off this bit on all files so DSKCLN can run.

Bug in DSKCLN:	If there is garbage in the UFD, then CLRALC causes the monitor
	to crash when it attempts to LOOKUP that garbage.  CLRALC can't run
	until DSKCLN fixes the disk, DSKCLN can't run until CLRALC runs.

Cure:	Just before adding to the fix-up chain, check the amount of core
	currently in use.  If too much, ignore RIPALC.  RIPALC will be zeroed
	on the next fast DSKCLN, or the next TITO all-files save, or the next
	time anyone does "DIR/WORDS", or any other operation that looks up
	all files.  This is edit 103 to DSKCLN 36J.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 7 Feb 87 18:45:15 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline call 

System D31, 5:00pm-6:20pm, Dallas OPS, Lois @ 214-637-7435

Symptom: Files and UFDs disappeared or became unaccessable without any
         apparent reason.  Lois was running a MAGNUM procedure under
         (BILLING10) and got a fatal magnum error and couldn't proceed.

Problem: One of the disks had a SAT Failure just prior to the problems
         which occurred.

Solution: As per procedure setup by Tom Marconi, I told them to take the
          system down and run a full DSKCLN and call me back when it was
          complete.  DSKCLN went as expected, no failures were found and
          the system was back up an answered prior to 6:20pm when Lois
          called back.

/Carl
From: Joe Smith <JMS@X930> 
Date: Sat, 7 Feb 87 22:03:33 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: THUNGs 

Writing files to the disk does not cause THUNG on 28.  Neither does running
TAKEIT by itself.  However, the HANG command to TAKEIT consistantly causes
BPC0 hung, BPC1 hung, etc.  Osman found it was always BPC, never BPA, even
though DSKB1,3,5,7, etc are on BPC.  Sounds like COMMON/MONBTS does not
do the right things when writing the modified HOME pages out.  This may be
related to the bogus "serious restart error" message.   /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 7 Feb 87 23:26:15 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: SUBMIT/XEXEC 

I'm putting the most current versions of XEXEC, EXECX and SUBMIT on SYS 
on all three of our machines (930, 62 and 95).  I believe that some of the
problems occurring with XEXEC et al are things which were "fixed" in one
of the latest versions.

Re: SUBMIT %1(205) supersedes SUBMIT %1(204)
Changes are to default the username field when a non-privileged user asks
for all jobs for his username or all jobs of name 'x' for his username,
and fix the bug where SUBMIT looped forever on deleted records until they
went away (and they didn't if XEXEC wasn't running).

Re: EXECX %1(222) supersedes EXECX %1(220)
Changes were to use the right default username when none was specified on
the :COM filename  command line.  Previously it was using the logged in job's
username (and the logged in job was EXECX running under OPER) and thus the
:COM command appeared not to work (unless you were OPER).

Dennis,
  Please review the "bugs" you asked me about concerning these areas and
check that the documentation conforms to the current behavior.  Let me know
if we need to make any additional changes to the wildcard/verify code.

Also not mentioned properly above... in EXECX, :TIME is now short for the
new :TIMELIMIT command and :TRULIMIT has been added.  In SUBMIT, "Y" now
works for "YES" in response to the verify prompt.  It was checking for the
entire word.

/Carl
From: Joe Smith <JMS@X930> 
Date: Sun, 8 Feb 87 3:15:40 PST 
To: osman 
Subject: Test to pinpoint THUNG 

I'd like you to do the following on X28:
In COMMON, set breakpoints at HNGALL, CRASHZ, CRSHOM, NEWMON.
Bring up the system, run TAKEIT, proceed from each breakpoint.
I need to know the order in which the breakpoints are hit, and when the
HUNG device problem occurs.

I'm going to do some more looking at listings of MAPIO, FILIO, and PCBIO
tomorrow.  /Joe
From: Joe Smith <JMS@X930> 
Date: Sun, 8 Feb 87 11:30:31 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hotline call at home (beeper not called) to B39. 

8-Feb-87 08:11/Joe
Contact: System 39, Cupertino, Abdul
         Abdul did not call the beeper number, did not let Carl's phone ring
         long enough to be answered, then called Joe at home.
Symptom: System dies with FILAUU while doing TITO all-files with P035/B.

Problem: DECUMC called with P2 pointing to a job's dummy ATB instead of a real
         ATB.  FILAUU complains when it decrements some random JBT table to -1.

Solution: Ran DSKCLN 36J(103) in check mode - no problems reported.
         Gave them B39-P035/B01 since it has the FILAMO bug fix and it types
         out the ATB on FILAUU/FILAMO/FILAUS and related stopcodes.


From: Joe Smith <JMS@X930> 
Date: Sun, 8 Feb 87 16:47:30 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: TDERR.PAT for P035/B 

TDERR.PAT has been applied to NEWMON on all systems.  It fixes the problem of
"serious restart error while writing to HOM pages".  The bug was: If the disk
is formatted in pages, MONBTS would set up to read in pages, and if the disk
was formatted in blocks, MONBTS would set up to read in pages.

I asked Osman to put in breakpoints - we found that the THUNGs occur when
TAKEIT kills job 2.  What sort of weirdness is CHKPNT up to?
From: Joe Smith <JMS@X930> 
Date: Sun, 8 Feb 87 19:10:37 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: ATTACH CHKPNT = THUNG. 

I was able to get hung disk messages by "SET OPR", "ATTACH 2".  CHKPNT
shutting down caused a THUNG on BPA4 and HUNG on BPC0,1,2,3,4,5,6.  It looks
like I/O was in progress on BPA4, that's why it got THUNG instead of HUNG
(the T means "while transfer was in progress").  Maybe releasing the ACT:
device causes PI channel 3 to get zapped.  It shouldn't though.  /JMS
From: Joe Smith <JMS@X930> 
Date: Mon, 9 Feb 87 17:30:18 PST 
To: CARL, OSMAN 
Subject: Test monitors 

KIXB01,KLXB01,24XB01,32XB01 all have 8 strings defined.
KIXB01 and KLXB01 have the command list size set for 25 blocks (3650)
24XB01 and 32XB01 have the command list size set for 18 blocks (3330)
24 is a KI, 32 is a KL.
From: Joe Smith <JMS@X930> 
Date: Mon, 9 Feb 87 23:25:20 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: 22 up on F22-P035/B 

Works fine using BPXKON instead of the old CPXKON.
See (JMS)DSKCLN.F22 for all the gory details.
From: Joe Smith <JMS@X930> 
Date: Tue, 10 Feb 87 17:38:37 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: THUNGs located. 

CHKPNT does a "read and reset buffered log" STRUUO on every unit in order
to log all accumulated disk errors.  It does all 14 units one right after
another - the BPC units return a busy flag, but for some reason do not
return the un-busy flag.  (Probably because BPA0 and BPC0 are the same
physical unit.)  There is no other-volume logic in FILIO for this UUO yet.
Until it is, I have modified CHKPNT to HIBER for 1/8 second between requests.
This should give the 3674 plenty of time to return the buffered log.  /JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 11 Feb 87 7:03:18 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: CUPC testing 

I rebuilt a 2-pack test system from OSMAN's all-files tape in his office
and more current files from C70's recent all-files.

I made a 1.005 tape copy (1 plus a little) with TITO of that system.

I ran my new ASP to make a save set, did a compare with some interessting
results, (works 99.9% but a small but variable number of pages appears to
be different without apparent discomfort to the file system.

Attempted a second save set but had hardware problems reading from disk...
used the original ASP save to restore to one of the disks in the 2-pack
system and a second one to replace the "bad" one.  Did a compare on the
restored data and it matches, (all except 2 pages... no explanation, one
was obviously a .REL file page, and the other was miscellaneous bits).

Ran a FULL DSKCLN with 36J(103) after the all-files rebuild, after the
ASP asp restore and prior to coming up as X12.  NNo complaints from any
of the DSKCLN runs.

COnclusion:
  CPXKON appears to work in blocks on 3330's.  No load testing was done
          other than ASP.  It would be a good test to run a few ASH jobs
          at once to see what it does...
  ASP 13(101) appears to do everythng it is supposed to do. One check is
          still needed for restart verification of the disk page and for
          starting too near the end of the structure, but that can be
          done later.
  X12 is up, open for users/testing. 32xb01.sav

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 11 Feb 87 7:21:50 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Meeting today... 

Assuming a meeting around 1pm... could someone please call me at home
about an hour before the meeting.  I've been here since noon yesterday
and am going home to get some sleep in about 10 minutes.

# is (408) 945-4314... thanks.

/Carl
From: Joe Smith <JMS@X930> 
Date: Wed, 11 Feb 87 21:06:50 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: DSKCLN detected holes on C26. 

The following files on System 26 have the wrong checksum: (11-Feb-87)
BOLRES (DATAPAK)NW2051.D01      file not found on D31
RARFUP (DATAPAK)TSTA02.BND      file not found on D31
MINPAP (DATAPAK)H1506.NIB       file not found on D31
JUKBUK (DATAPAK)DKKA07.N06      file not found on D31
FUNTIB (DATAPAK)DKHA05.NIB      file not found on D31
CUSVOP (DATAPAK)DKUA06.N06      file not found on D31
VUGQAB (DATAPAK)TSTA01.BND      file not found on D31
QISDEB (BPTYME)BP2017.BND       wrong checksum on D31 also
DAWYEB (BPTYME)BP2242.N00       wrong checksum on D31 also
HOTYAP (USGSCODE)GS2077.N03     wrong checksum on D31 also
ZAPMOS (TECHSERV)AK2051.DMP     file not found on D31
TARFOW (VACODE)VA2317.N01       wrong checksum on D31 also
YITTEV (VACODE)VA2364.N04       wrong checksum on D31 also
DOJZOV (TEANET)BK2625.N02       file not found on D31
ZUCTOW (SPS)NTR.ERS             file not found on D31
SIMHAB (PHILIPTAS)SO2011.N05    wrong checksum on D31 also
DOFKIL (UKOPS)NW2017.BND        file not found on D31, wrong on D37 also
LAXROD (SWTAS)ALLAN.NI2         wrong checksum on D31 also
HIXVUM (UKNET)UK2101.BND        wrong checksum on D31 also
YOLCED (TECHTRAIN)SB2014.NIB    file not found on D31
JUSHIK (TSNNET)DC2507.BND       wrong checksum on D31 also
KUXVIY (TSNNET)LS2111.N00       file not found on D31
BUVDOG (SWBNET)LP2755.N00       wrong checksum on D31 also
VAWZUK (SWBNET)ST2326.N00       file not found on D31
MUQDOR (SWBNET)SP2267.N00       wrong checksum on D31 also
QELDIX (SWBNET)SP2551.NIB       file not found on D31
BEJLEC (EPACODE)N10334.N02      wrong checksum on D31 also
TIKDOF (EPACODE)N10335.NIB      wrong checksum on D31 also
XAGMOH (SIRTITAS)B12012.N01     file not found on D31
FISWIC (SIRTITAS)R12026.N01     file not found on D31

This list should be given to Ed Roop, and the individual users notified.
/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 12 Feb 87 14:55:10 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Power outage 

Fremont Liberty took a short power hit that knocked out our machines.
Everything has been reloaded thanks to the swift efforts of Dennis
Coffey.  Everything that is, except X95.  It does not respond to the
boot switch or BT typed to the KS10> prompt.  I didn't see a 2020 boot
tape for TYMCOM-X in the machine room... So, we have decided to power
the system down and leave it down until Monday.

/Carl
From: Joe Smith <JMS@X930> 
Date: Tue, 17 Feb 87 23:27:49 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: C70-P035/B01 

The B01 monitor is running on system 70 - no problems.  There are only 5
holes remaining - when they are filled, C70 can run DSKCLN 36J(103).

As soon as F58 comes up on P035/B01, we will have the same monitor running
on all the 3650/3652 systems.  /Joe
From: Joe Smith <JMS@X930> 
Date: Tue, 17 Feb 87 23:34:23 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: List of holes. 

There are only 4 systems that have files with holes.  The files are listed
in (JMS)FEB17.HOL
From: Joe Smith <JMS@X930> 
Date: Wed, 18 Feb 87 5:39:35 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: non-HOTLINE call from D37 

18-Feb-87 05:20/Joe
Contact: System 37, Dallas, (214)637-7435, Allen
Problem: SPLFIX "repair" could not find FDF records, files not printed.
Fix:     Told them to use "R SENDMAIL" to "DENCOFF" for these problems.
         Problem not critical enough to fix in the middle of the night.
!RUN (UTIL)SPLFIX
SPOOL Data Base Operations Maintenance Tool, v. 3.0
System 37 (Dallas)    18-FEB-87 07:30

Option (? for help) *REPAIR
Checking the status of SPOOL . . . . . . No one running SPOOL
Disabling SPOOL . . . SPOOL disabled.

Beginning SPOOL data base repair at 18-FEB-87 07:30.
Beginning pass 1:
  Scanning the GDF file...
  Scanning the FDF file...
  Scanning the internal records...
    DAMAGE:  request #194, dated 6-JAN-87 is missing 1 FDF records.
    DAMAGE:  request #254, dated 22-JAN-87 is missing 1 FDF records.
    DAMAGE:  request #393, dated 15-FEB-87 is missing 1 FDF records.
Fatal error.
Enabling SPOOL . . . Enabled.
NO GOOD RECORDS FOUND!!
Option (? for help) *QUIT
From: FLETCHERC@X930.Tymnet 
Date: Wed, 18 Feb 87 14:44:52 PST 
To: osman 
Subject: "Please put together the list of"... 

Please put together the list of excess equipment that we would
like from Ed Roop separated into two sections- one for
Product Support and one for Training.  Here is a first cut:

Training
Memorex type xx controller
4 type xx disk drive
one e-bus base

Product Support
KI10
256K core memory (or whtever)
Memorex type xx controller
4 type xx disk drive

	Craig
From: Joe Smith <JMS@X930> 
Date: Thu, 19 Feb 87 18:06:47 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Date expressions 

If you want to see some really hairy date expressions, check the file
(PERPOPER:62)PHONIX.MSG and *.MSA.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 20 Feb 87 6:39:37 PST
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 20 FEB 87 04:41:21 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (25 names) 
Message-id: A22147@Ontyme.Tymnet 
Subject: "SUBJ: M E M O R A N D U M MC DONNELL-DOUGLAS"... 

SUBJ: 





                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      20 FEB 87  04:40

TO>        All Operators
           Shift Supervisors

COPIES>    Cheryl Eldred
           Osman Guven

FROM>      Rick Daniels


SUBJECT>   (UTIL)PSPATO


-----------------------------------------------------------------------


Until I can find out why we can't run (UTIL)PSPATO from OPER, log

into (UTIL) and run it from there.  This seems to work for now.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 20 Feb 87 8:39:38 PST
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 20 FEB 87 07:22:50 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (26 names) 
Message-id: A22223@Ontyme.Tymnet 
Subject: "SUBJ: M E M O R A N D U M MC DONNELL-DOUGLAS"... 

SUBJ: 





                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      20 FEB 87  04:40

TO>        All Operators
           Shift Supervisors

COPIES>    Cheryl Eldred
           Osman Guven

FROM>      Rick Daniels


SUBJECT>   (UTIL)PSPATO


-----------------------------------------------------------------------


Until I can find out why we can't run (UTIL)PSPATO from OPER, log

into (UTIL) and run it from there.  This seems to work for now.


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


You can now run (UTIL)PSPATO from OPER.  I found what the problem was.

Thanks Rick
From: Joe Smith <JMS@X930> 
Date: Sun, 22 Feb 87 19:50:02 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Holtline calls 

22-Feb-87 19:37/Joe
Contact: System TRW's 2020, Anaheim, 800-345-6090, Chuck from NSSC
Problem: While running TITO, the command "COF DAY22.DIR" failed.
Fix:     Disk was full - files deleted.

22-Feb-87 17:36/Joe
Contact: System 35, Dallas, (214)637-7435, Tyrone
Problem: Could not GFD, could not LOGIN, then crashed with and APRFAT stopcode,
         "CDP" Cache Directory Parity Error.
Fix:     Told him to call maintenance to run diagnostics.  I don't expect any
         thing to be found - CDP is usually cleared by reloading the system.

21-Feb-87 15:25/Joe
Contact: System 37, Dallas, (214)637-7435, Paul
Problem: PAKCPX saw only units 300-307 on the string of 3652 disks.
Fix:     Told them to try PAKCP5.  If that does not work, have the 3674
         wired for bits 2,3,4 all don't care to make all 8 strings show up.

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 23 Feb 87 1:54:56 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline call 

23-Feb-87 00:50/Carl
Contact:  System 35, Dallas, (214)637-7435, John Roddam
Symptom:  Initialization failure, NO USER for JOB DSKCLN.  Apparently
          the previous problem reported to Joe, cannot GFD etc., may have
          been caused by a bad LUD.SYS in (SYS).  No speculation on the
          co-incidence of the cache-directory-parity error.

Problem:  As in symptom above.  (SYS)LUD.SYS appears to have been modified
          by some unknown agent:  hardware? validations?  who knows...

          After attempting "PPN username" for OPER, CARL, SPL and others it
          seemed like a hardware glitch or some other one-time fluke that
          had wiped out part of the disk/ufd's.  I started a full DSKCLN and
          after Steve Atwell mentioned that he had done the same with no
          real errors, I aborted that and tried "DIR [1,21]" which printed
          a nice neat directory.  I then tried my own ppn and SPL.  Hmmm.

          I renamed LUD.SYS to LUD.BAD, copied it back to LUD.SYS for TITO
          and restored the file from the Saturday all-files tape.  All the
          "missing" UFD's were now restored and I had operations halt the
          machine and do a full DSKCLN.

Received: from EMSFSC.Ontyme.Tymnet by X930.Tymnet; Mon, 23 Feb 87 14:40:33 PST
From: FSC.M/MILLER@EMSFSC.Ontyme.Tymnet 
Date: 23 FEB 87 12:45:36 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P95210@Ontyme.Tymnet 
Subject: COMMERCE OFFICE MOVE 
OFFICEMOVE: 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y



DATE>      23 FEB 87  11:42

TO>        ALL CONCERNED

COPIES>    ALL

FROM>      MARC MILLER - LOS ANGELES


SUBJECT>   OFFICE MOVE


-----------------------------------------------------------------------





PLEASE NOTE THE FOLLOWING .

THE LOS ANGELES - (COMMERCE) FIELD SERVICE OFFICE HAS MOVED

THE NEW ADDRESS IS

5900 S. EASTERN AVENUE
COMMERCE   CALIF.  90040

SUITE 186


PLEASE NOTE THE ONLY CHANGE IS THE SUITE NUMBER.

THE PHONE NUMBER REMAINS THE SAME - (213)721-3322.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 23 Feb 87 23:03:33 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: CHKPNT 

Well,
  apparently CHKPNT %31(102) to whit we have in CHKPNT.CAB is in some
unfinished state (as of 24-Jun-85).  It is NOT worth the effort at this
time to figure out what needs to be finished.  There is code commented
out (as if waiting for more to be written) which looks suspiciously like
the code to prevent CHKPNT from dying when it runs out of space.  I put
in the code that Joe installed in his version 30.4 (I had a 30.4 at one
point but removed the edit because it didn't work as expected) as version
%31(103) now in CHKPNT.CAB in (OSU:930).

  The old %31(102) is now in (osu:930)CHKPNT.312.  Neither this file nor
CHKPNT.CAB will assemble in their current states and I expect to spend no
furhter time working on them until the other 365x work is completed.

  Tomorrow I will look at ASP in more detail and see about scheduling time
on the test machine(s) in Fremont (or Cupertino).

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 24 Feb 87 5:48:17 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline Call 

24-Feb-87 05:30 /Carl
Contact:  Dallas OPS, (214)637-7435, System 35.
Symptom:  Customer having problems using EDITOR.  User from VALUE-LINE
          with username P4NEW receives message cannot enter .TMP file and
          and then EDITOR dies with an address check or some similar ilk.

Problem:  User P4NEW [40315,57542] has 583 files with 7005 pages and a disk
          quota of 2500 pages.  Apparently, his quota was lowered by someone
          since there are no huge files in his directory.  Most of the files
          are 2-5 page files with a bunch of 40+ page files.  (I didn't look
          at .TMP files).

Solution: Have customer contact his representative and have his quota raised
          to it's proper value (or delete the majority of his files?).  This
          may have been a result of the LUD.SYS file damage.  I will send a
          message to resources to notify them that there was a problem and
          that they should check their changes to system 35 since Friday.
/Carl
From: Joe Smith <JMS@X930> 
Date: Tue, 24 Feb 87 12:47:26 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Versions of CHKPNT on SYS: 

(SYS)CHKPNT.SAV version 30.3 checksum TUDPUP is on all systems except 26 and 37.
Systems 26 and 37 are running version 30.4 checksum BIGSIS for 3652 disks.
The new version is (OSU)CHKPNT.SAV everywhere; source is in (OSU:930)CHKPNT.304.
930 and 62 are running version 31(100) checksum MEFFAX, source unknown.
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 25 Feb 87 10:56:43 PST 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Comp Day.. 

Will be takeing 2/25 & 2/26 as comp-day.
-Osman-
From: FLETCHERC@X930.Tymnet 
Date: Wed, 25 Feb 87 16:16:37 PST 
To: osman 
Subject: "subj: 11/730 diag's Had another"... 

subj: 11/730 diag's

Had another request for 11/730 diags on tu58
This one for Scotty Paul
Cranford Office
11 Commerce Drive
Cranford, N>J> 07016


Can you get a copy for Lois to send?

	Craig
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 26 Feb 87 5:47:46 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: schedule 

I'm going home from CUPC soon... and don't expect to come to the office
today.  I will try to go through the output I've generated on the 3330
disks running ASP and summarize it for our meeting.

The results (in my opinion) are inconclusive.  I only used the NEW ASP
and I only used the 32xb01.sav monitor Joe built for 3330 blocks length
command list.

I did NOT have a single run where everything matched.  Some pages were
different each time, and on multiple compares, the different pages
changed.  I do NOT like this at all.  I do NOT suspect ASP as the culprit
but I have no better explanations.

CPXKON is in 32xb01.sav that I used.  It is worth testing again later
with BPXKON to see if there are any differences (or patterns).

/Carl

Good night all... it's been a long day(?) !!!
From: Joe Smith <JMS@X930> 
Date: Thu, 26 Feb 87 16:22:08 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: D31 coming up on 3652 disks this weekend. 

Tom Marconi asked for a 3652 monitor for system D31.
   From: JMS, FEB 26, 1987 18:16
   To: CRALST   Subject: Software for 3652 disks
   (SYS)NEWMON.SAV has D31-P035/B01 monitor
   (M33)PAKCPX.SAV formats 3650/3652 disks in blocks
   (M33)BOTLOD.SAV punches paper-tape boots for the KI
   (M33)BOOTYI.SAV writes BOOTS for 3652, blocks, KI
                           Joe Smith

From: Joe Smith <JMS@X930> 
Date: Thu, 26 Feb 87 18:30:37 PST 
To: Peter Stratman <CTMKTG7@X930> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Running SUBMIT on the 2020s. 

P034/P-10 is the latest monitor supported on the KS machines.  SUBMIT will
not run on the P034/P monitor.  SUBMIT requires P034/P37 or P035/B.
P035/B cannot be supported on the 2020 until KSFEFS is modified to put
BOOTS in the right place on disk.

There are no software conflicts between P034/N and P034/P, which means that
S59, S60, S92, S264, S301, and Mallenkrodt's M169 can be upgraded with no
problems.  S443 is running P034/K which may be a hassle.  Upgrading W1051
will take as much effort as upgrading TRW from P034/J to P034/P.

I would like to see all of the 2020 systems upgraded to P034/P or better
by the end of June 1987.

				/Joe
Received: from X62.Tymnet by X930.Tymnet; Thu, 26 Feb 87 22:22:35 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 26 Feb 87 22:15:34 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: Hotline Calls 

Tuesday,
  4 or 5 calls - all related to system 35's problems.
  problem eventually solved as per my previous messages.

Thursday,
  2 calls (so far) - system F74, Bonnie

  Problem.  System had one or more SAT failure messages on OPR or CTY
after which some files became inaccessible (SYSTAT for one) and they
reloaded.  Accounting under username APACCT complained of RIB errors
both before and after the reload.

  One file in their directory had a bad rib.  2 in TAPELIB.  I ran FNDBAD
and it generated a list of files with HRE and the three files mentioned
above without saying what was wrong with the three files.  It turned out
these are bad ribs.  (Joe, FNDBAD ought to say what is wrong!)

  2nd call was from Tim (Jim?) in Freont because he couldn't restoer the
files which had RIB errors.  Even with license he couldn't delete them.
I told him about the DELETE FILE.EXT/BAD command/switch and the files
were deleted ok.

/Carl
From: CARL@X930.Tymnet 
Date: Sat, 28 Feb 87 13:49:13 PST 
To: jms, osman 
Subject: "Beeper call My beeper went off"... 

Beeper call
 My beeper went off sometime while I was asleep last night or this morning.
The number was for Dallas, but when I woke up and called Dallas, no one knew
when or why I might have been beeped.

  Any Ideas what time it was?

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 1 Mar 87 8:31:16 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline call 

Sunday, 7:50 am /Carl
Symptom: DSKCLN gave error "Bad rib detected in critical file"
         It listed (SYS)1022F.SHR and (SYS)DIRIT.SHR wtih WHC which means
         the entries had the wrong hash code.
I deleted both files and told operations to run a FULL DSKCLN and then
to restore the files from the all-files.

I ran DSKCLN in checking mode twice. Once before and once after
the files were deleted.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 1 Mar 87 9:48:26 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Update on hotline call 

Rerun of DSKCLN in checking  mode found additional files also bad...
so I looked at the UFD and found page 4 had what looked like blocks
2,3 and 4 copied to blocks 1,2 and 3.  All the rest of the pages looked
OK.  I BLTed the data to the right phase and erased the duplicate data.
Ran full dskcln in checking mode and intend to turn the system over
to operations to come up with REFLAG set and restore all of SYS from the
previous all-files/backup tapes using MODE SYS NEWEST after first doing
a full dskcln.


DSKCLN is running clean at the moment.

/Carl
From: Joe Smith <JMS@X930> 
Date: Thu, 5 Mar 87 1:13:12 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: File with bad RIB on D65. 

System 65 is running P035/B-1 on 3330s with BPXKON.
!SYSNO
TYMSHARE D65-P035/B-1 1-30-87
!TYPE FNDBAD.RPT
(SDAVIS)AR8612.MSG       DRB

Legend:
  DRB: Dead RIB (use DELETE/BAD to remove file)

!GFD SDAVIS
DSKB:AR8612.MSG rib error

!ASSIGN DSK STR
DSK assigned

!R TSTSUP
$1B>>DATBRK   PAGE/   1   LOOK$X
File: AR8612.MSG

<FOUND>
$1B>>DATBRK   PAGE/   200004,,170602   D<D+777>0$N
D/   200012,,32747
D+1/   200004,,57371
D+2/   200006,,24023
D+3/   200002,,172500
D+4/   200002,,173031
D+5/   200002,,173205
D+6/   200004,,161506
D+7/   200006,,100662
D+10/   200004,,161665
D+11/   200002,,113345
D+12/   200012,,202421
D+13/   200012,,176233
D+14/   200002,,23117
D+15/   200014,,57217
D+16/   200002,,23272
D+17/   200014,,57355
D+20/   200002,,23472
D+21/   200014,,57550
D+22/   200002,,23644
D+23/   200014,,57721
D+24/   200002,,24020
D+25/   200014,,60100
D+26/   200014,,31167
D+27/   200010,,36742
D+30/   200004,,206067
D+31/   200004,,177234
D+32/   200004,,177366
D+33/   200004,,177543
D+34/   200004,,177724
D+35/   200004,,200076
D+36/   200006,,107672
D+576/   666666
D+577/   200004,,170602
D+776/   666666
D+777/   200004,,170602

D/   200012,,32747   PAGE!   $Q
$P
$1B>>DATBRK   PAGE/   200012,,32747   D/   341606,,506424   $T;885
   
D+1/
    
D+2/    I no   
D+3/   ticed   
D+4/    that   
The data in the RIB is shifted 100 words - 1/2 of a block, not a full
block.  Very strange.		/Joe
From: Joe Smith <JMS@X930> 
Date: Thu, 5 Mar 87 17:06:29 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Files with holes 

Files with holes in them, as of 5-Mar-87

None detected: 22,23,26,29,30,31,33,34,37,38,54,55,56,57,58,65,70,74,95
35: (SYS)LUD.XXX[1,4] 4 holes (known bad copy of LUD.SYS)
62: (SYS)SPAGES.SYS[1,4] (can be fixed by refreshing DSKB)
930: (SYS)SPAGES.SYS[1,4] (can be fixed by refreshing DSKB)
930: (NEWSPL)TEST.ME[3,700202] (deliberate hole)

25: (TYMNET)ND3414.BND[21477,175251]
25: (TYMNET)ND4057.N03[21477,175251]
25: (TYMNET)ND2206.N02[21477,175251]
25: (TYMNET)ND2700.N03[21477,175251]
25: (TYMNET)ND2335.N01[21477,175251]
25: (TYMNET)ND2335.N01[21477,175251]
25: (TYMNET)ND4663.N02[21477,175251]
25: (ACCG1)D29603.S03[21477,215544]
25: (ACCG1)D29603.S04[21477,215544]
25: (ACCG1)D29603.S05[21477,215544]
25: (ACCG1)D29603.S10[21477,215544]
25: (MSTSCHED)MASTER.DMS[21477,307121]
25: (CNFE)EJ2517.BND[11020,136151]
25: (CATECH)ND2377.N03[44054,251263]
25: (CHALIT)BH1210.NW1[30455,107741]
25: (ISISPCM)ND2057.N00[37071,27056]
25: (ISISPCM)ND2004.BND[37071,27056]
25: (CANDERSON)CA3452.D12[44062,51243]
25: (JJOELS)MAC3IS.RPT[11334,474103]
25: (OUTDIAL)BK2431.BND[24261,311005]
25: (HQLAB)JL2253.N00[21476,34750]
25: (BCHEN)OPT02.P01[21476,121017]
25: (BTHOMSON)STUFF.DOC[21476,314715]
25: (WARNERD)DW4255.N02[44054,267432]
25: (SLICM)R80D.LST[26007,34174]
25: (MDCIRVINE)DSYN2B.DOC[45261,400005]

27: (ISISDOC)ISIS2.DOC[11334,247524]
27: (ISISDOC)NAD.DOC[11334,247524]
27: (M8045)BANQUE.FRM[15767,216377]
27: (M8045)TABL.AFF[15767,216377]
27: (INSDGSS1)TBASRT.SHR[11011,371163]
27: (SYS)CRA001.SAV[1,4]
27: (HWPROD)EQUIP.TST[14717,404113]
27: (HWPROD)I0MOMI.MR1[14717,404113]
27: (HWPROD)INVOIC.TST[14717,404113]
27: (HWPROD)BZ8GJH.MX1[14717,404113]
27: (ISGFM)NOV.01[47167,474035]

Received: from EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 6 Mar 87 8:40:00 PST
From: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Date: 06 MAR 87 08:23:14 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P97223@Ontyme.Tymnet 
Subject: Bad files.. 

Jan..

Here is a list of all the files which are still bad
on system D25 and D27. -Osman-

25: (TYMNET)ND3414.BND[21477,175251]
25: (TYMNET)ND4057.N03[21477,175251]
25: (TYMNET)ND2206.N02[21477,175251]
25: (TYMNET)ND2700.N03[21477,175251]
25: (TYMNET)ND2335.N01[21477,175251]
25: (TYMNET)ND2335.N01[21477,175251]
25: (TYMNET)ND4663.N02[21477,175251]
25: (ACCG1)D29603.S03[21477,215544]
25: (ACCG1)D29603.S04[21477,215544]
25: (ACCG1)D29603.S05[21477,215544]
25: (ACCG1)D29603.S10[21477,215544]
25: (MSTSCHED)MASTER.DMS[21477,307121]
25: (CNFE)EJ2517.BND[11020,136151]
25: (CATECH)ND2377.N03[44054,251263]
25: (CHALIT)BH1210.NW1[30455,107741]
25: (ISISPCM)ND2057.N00[37071,27056]
25: (ISISPCM)ND2004.BND[37071,27056]
25: (CANDERSON)CA3452.D12[44062,51243]
25: (JJOELS)MAC3IS.RPT[11334,474103]
25: (OUTDIAL)BK2431.BND[24261,311005]
25: (HQLAB)JL2253.N00[21476,34750]
25: (BCHEN)OPT02.P01[21476,121017]
25: (BTHOMSON)STUFF.DOC[21476,314715]
25: (WARNERD)DW4255.N02[44054,267432]
25: (SLICM)R80D.LST[26007,34174]
25: (MDCIRVINE)DSYN2B.DOC[45261,400005]

27: (ISISDOC)ISIS2.DOC[11334,247524]
27: (ISISDOC)NAD.DOC[11334,247524]
27: (M8045)BANQUE.FRM[15767,216377]
27: (M8045)TABL.AFF[15767,216377]
27: (INSDGSS1)TBASRT.SHR[11011,371163]
27: (SYS)CRA001.SAV[1,4]
27: (HWPROD)EQUIP.TST[14717,404113]
27: (HWPROD)I0MOMI.MR1[14717,404113]
27: (HWPROD)INVOIC.TST[14717,404113]
27: (HWPROD)BZ8GJH.MX1[14717,404113]
27: (ISGFM)NOV.01[47167,474035]
From: Joe Smith <JMS@X930> 
Date: Mon, 9 Mar 87 17:04:58 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Mallenkrodt's crashes 

Crashes on M169 - Mallenkrodt's 2020

CRA020 18-Feb-87 05:04  MAGSTS/ 164200 = Tape hardware error
CRA021 21-Feb-87 04:54  MAGSTS/ 164200 = Tape hardware error
CRA022 21-Feb-87 05:09  MAGSTS/   4200 = OK
CRA023 26-Feb-87 02:34  crash code not executed, MAGSTS not recorded in dump
CRA024 26-Feb-87 09:50  MAGSTS/ 160200 = Tape hardware error
CRA025 26-Feb-87 13:07  MAGSTS/ 164200 = Tape hardware error
CRA026 27-Feb-87 14:13  MAGSTS/ 160200 = Tape hardware error
CRA027                  dump taken after memory was zeroed - no information
CRA028  6-Mar-87 04:48  MAGSTS/   4200 = OK
  Above crashes with P034/N, CRA029 from P035/B
CRA029  6-Mar-87 21:03  MAGSTS/ 160200 = Tape hardware error

MAGSTS is tape drive status, CRS1 in the RH11 connected to the TM02/TU45.
100000 = bit 15 = SC   = Special condition (set if TRE or MCPE get set)
 40000 = bit 14 = TRE  = Transfer Error
 20000 = bit 13 = MCPE = Massbus Control Parity Error (TM02 sent bad parity)
  4000 = bit 11 = DVA  = Device Available (tape is on-line)
   200 = bit 07 = RDY  = RH11 is Ready (should always be on in any dump)

Crashes 020, 021, 024, 025, 026, and 029 all indicate that there is something
wrong with the tape drive.  The problem appears to only occur after EOT has
been reached.  Field Service needs to run diags on the drive - tell them
"We keep seeing bit 13 get set in register CSR1 of the tape's RH11".

On another subject, crashes 004, 006, 007, 011, 012, 013, 014, 016, 017, 018,
and 019 all have STOPPC/SCNSER:GOBIO2+12, which was fixed 8-Apr-86 in P034/P22.
The patch is in (OSARC)BIOZAP.PAT and is in P035/B but is not in P034/P-14.
From: Joe Smith <JMS@X930> 
Date: Mon, 9 Mar 87 17:10:45 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Correction to previous message. 

BIOZAP.PAT is patch #9 in PMON.PAT and IS in P034/P-14.  Therefore Mallenkrodt
should be running (M33)MA034P.SAV which has M169-P034/P-14.   /Joe
Received: from X62.Tymnet by X930.Tymnet; Tue, 10 Mar 87 9:13:55 PST
Return-path: <DenCoff@X62> 
From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X62> 
Date: Tue, 10 Mar 87 9:08:52 PST 
To: (Monitor Group:), Carl, Osman, Joe <JMS> 
Cc: Craig <FletcherC>, TXSSup 
Subject: Mallenkrodt's (M33)MA034P.SAV monitor. 

This monitor, in (M33:169), is ... /P-10, not ... /P-14.  If ... /P-14 is the
version needed, we need to get it from X930, or patch it on M169.
-D.-
From: Joe Smith <JMS@X930> 
Date: Tue, 10 Mar 87 14:12:15 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Differences between /P-10 and /P-14. 

Differences between P034/P-10 and P034/P-14 monitors:
  Patch #11 RESET.PAT did not solve anything (SIMUUF still occurs)
  Patch #12 cancels the gobbler after zap (crashed EBUS bases, not LSI-11)
  Patch #13 changes the scheduler fairness (affects busy systems like 39)
  Patch #14 made INFO stopcodes work on KI (does not affect KL or KS)

Therefore /P-10 is suitable for use on the 2020 systems, it is not worth
patching /P-10 to /P-14 level.    /Joe
From: Joe Smith <JMS@X930> 
Date: Tue, 10 Mar 87 23:11:06 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, wrs 
Subject: Gateways to BUBBNET in Fremont. 

When TELECOpying to B39, use gateways 1552 or 2866.  Coming from B39, use
hosts 12 or 11.  These two gateways are located in Solar Way and do not require
a trip to Cupertino and back (which happens when using gateway 5946/2).
From: Joe Smith <JMS@X930> 
Date: Wed, 11 Mar 87 12:56:01 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: New DSKCLN distributed. 

DSKCLN version 36J(103) has been placed in SYS on all systems in Dallas.
This version is now running on all systems.
From: Joe Smith <JMS@X930> 
Date: Wed, 11 Mar 87 14:16:38 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: correction to previous message 

New DSKCLN not on D25 or D27 yet because of files with holes.
I am running new DSKCLN in check mode to verify how many holes are
still left.  /Joe
From: Joe Smith <JMS@X930> 
Date: Wed, 11 Mar 87 21:17:57 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Only 35 holes remain. 

The following files on systems D25 and D27 still have holes.
25:
(TYMNET:25)ND3414.BND[21477,175251]
(TYMNET:25)ND4057.N03[21477,175251]
(TYMNET:25)ND2206.N02[21477,175251]
(TYMNET:25)ND2700.N03[21477,175251]
(TYMNET:25)ND2335.N01[21477,175251]
(TYMNET:25)ND4663.N02[21477,175251]
(ACCG1:25)D29603.S03[21477,215544]
(ACCG1:25)D29603.S04[21477,215544]
(ACCG1:25)D29603.S05[21477,215544]
(ACCG1:25)D29603.S10[21477,215544]
(MSTSCHED:25)MASTER.DMS[21477,307121]
(CNFE:25)EJ2517.BND[11020,136151]
(CATECH:25)ND2377.N03[44054,251263]
(CHALIT:25)BH1210.NW1[30455,107741]
(ISISPCM:25)ND2057.N00[37071,27056]
(ISISPCM:25)ND2004.BND[37071,27056]
(CANDERSON:25)CA3452.D12[44062,51243]
(JJOELS:25)MAC3IS.RPT[11334,474103]
(OUTDIAL:25)BK2431.BND[24261,311005]
(HQLAB:25)JL2253.N00[21476,34750]
(BCHEN:25)OPT02.P01[21476,121017]
(BTHOMSON:25)STUFF.DOC[21476,314715]
(WARNERD:25)DW4255.N02[44054,267432]
(SLICM:25)R80D.LST[26007,34174]
(MDCIRVINE:25)DSYN2B.DOC[45261,400005]
27:
(ISISDOC:27)ISIS2.DOC[11334,247524]
(ISISDOC:27)NAD.DOC[11334,247524]
(M8045:27)BANQUE.FRM[15767,216377]
(M8045:27)TABL.AFF[15767,216377]
(INSDGSS1:27)TBASRT.SHR[11011,371163]
(HWPROD:27)EQUIP.TST[14717,404113]
(HWPROD:27)I0MOMI.MR1[14717,404113]
(HWPROD:27)INVOIC.TST[14717,404113]
(HWPROD:27)BZ8GJH.MX1[14717,404113]
(ISGFM:27)NOV.01[47167,474035]

/Joe
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 12 Mar 87 9:51:16 PST 
To: jms 
Subject: D54's 3652 monitor is gen & copied. 


Received: from X62.Tymnet by X930.Tymnet; Thu, 12 Mar 87 22:44:53 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 12 Mar 87 22:42:52 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: Fremont power failure 

The fremont data center took a power failure tonight... so if
you're wondering why some of the machines were down that is why.

No hotline call... I was curious why I couldn't build circuits to
any fremont hosts, but Dallas was up... sigh.

/Carl
From: FLETCHERC@X930.Tymnet 
Date: Mon, 16 Mar 87 14:10:04 PST 
To: osman 
Subject: "If you have some extra TU58 casssetes"... 

If you have some extra TU58 casssetes we would like to send them
to Russ Wheeler at Parse for shipping new versions of the diagnostics
to us for text.

test

	Craig
From: Joe Smith <JMS@X930> 
Date: Mon, 16 Mar 87 14:55:54 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: D54 up on 3652 disks. 

They came up at 05:53 Sunday, March 15th.  That was quick - up a whole day
before Monday morning.  Looks like the rebuild procedure was clear this time.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 18 Mar 87 13:05:39 PST 
To: Joe Smith <JMS@X930.Tymnet>, Osman Guven <Osman@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet> 
Subject: Schedule 

I will be working from home for the rest of the day... if you need anything
feel frree to give me a call.  408-945-43
14   /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 19 Mar 87 18:26:17 PST 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: Product distribution proposal 

In the past, there have been various schemes to push files around to
one or more systems in order to keep software versions up to date.  Some
of these schemes involved people time to verify that the system was up
and running, that the proper software was being distributed, that the
proper license and protection were propagated and... and... and...

Most of these systems were too time intensive to work without someone
keeping track of what was happenning.  All of these systems relied on the
distribution of products from the source system to each of the destination
systems.  Most of these systems have fallen into disuse and much of what is
now running on our systems is not properly supportable.

I would like each of you to think about the ramifications of a replacement
distribution system that relies on pulling the software from the source
system automatically whenever it detects a discrepancy between the released
version of the product and the available version.

    ----------------------------------------------------------------

In thinking over the new SETOP database which will be centralized on one
host and how the individual changes would be distributed, some rather new
and obvious solutions came to mind.  If it was the responsibility of each
host to update it's OPER database, all changes could be marked on the
central system whose only responsibility would be to notify the outlying
systems that they needed to pull an update.

This same idea could be utilized in a software product distribution scheme
where product information is kept in one or more centralized places with
proper authorization for license and access to specific directories.  Each
individual system would have a similar "stripped-down" database which has
only the information needed by that system.  This individual database would
be updated by authorized users (or programs) whenever a new release is made.
The master database would have copies of the information for verification
and tracking purposes (including an audit trail).

Periodically, an XEXEC subjob would check to see that the software on the
system matched the database.  Any discrepancies would generate a separate
subjob to re-install the proper software with the correct license, protection
and checksums.  Mail would be sent to the appropriate authorized users for
any action taken (eg. Changing license or protection, restoring the file
from the source, requesting the file from the latest backup, etc).

The hierarchy of authorized users could be product managers, operations,
security people and the like.  I've begun to put together a list of needed
information about products, people and systems and I would appreciate
greatly any comments on the idea.

Since the security aspect is linked into the new SETOP, I will be making
my initial concentration in that area (two stones with one bird).

/Carl
From: Joe Smith <JMS@X930> 
Date: Mon, 23 Mar 87 8:09:04 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: I am on call this week. 


From: Joe Smith <JMS@X930> 
Date: Mon, 23 Mar 87 13:30:51 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: FIND dies on Hard Read Error. 

I found a problem running FIND under MHX.  If any file FIND is looking at has
a hard read error, FIND will die with "?Illegal Memory Reference at PC 031267"
which clears the terminal's input buffer.  MHX will sit there forever.
Received: from EMSFSC.Ontyme.Tymnet by X930.Tymnet; Mon, 23 Mar 87 16:40:34 PST
From: FSC.SUP@EMSFSC.Ontyme.Tymnet 
Date: 23 MAR 87 16:16:05 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P99954@Ontyme.Tymnet 
Subject: NEW ONTYME VERSION/COMMANDS 

TO:  ALL ACCOUNT USERS
FR:  FSC.SUP

ONTYME VERSION 23 HAS BEEN INSTALLED.  PLEASE READ *** ONTYME23.NEWS FOR
DETAILS ON THE NEW COMMANDS.  (:READ *** ONTYME23.NEWS)

IF YOU HAVE ANY QUESTIONS ON THE NEW COMMANDS, JUST CONTACT FSC.SUP
From: Joe Smith <JMS@X930> 
Date: Wed, 25 Mar 87 15:41:37 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Why PJ wasn't running on any system. 

There some hidden restrictions on what can be in JOBS.DAT due to the
incompetent programming in PAM.
1) (PJ)JOBS.DAT can not be more than 128 words long.  One block and only
   one block is read in when PAM starts up.
2) Host numbers greater than 1519 cause illegal memory references.
   This is due to a stupid algorithm to flag which host numbers need
   to be looked at.  It defines an 999 word array to hold the information
   copied from JOBS.DAT.  If host N is mentioned in JOBS.DAT, then ARRAY[N]
   is set nonzero.  There is no bounds checking, which means any host number
   greater than 999 (such as 1051) will exceed the bounds of the array.
   Host numbers greater than 1519 will go to memory locations greater than
   3777 octal, which causes the ILL MEM REF.

This bug did not cause any problems until after the BUBBNET gateway host
numbers were added (host 1552 in particular).  I have redistributed a new
(PJ)HOST.CTL with the offending numbers deleted.        /Joe
Received: from EMSTXS.Ontyme.Tymnet by X62.Tymnet; Tue, 21 Apr 87 16:44:08 PDT
From: IPC.L/THOMAS@EMSTXS.Ontyme.Tymnet 
Date: 21 APR 87 23:26:05 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A50924@Ontyme.Tymnet 
Subject: Cupertino Data Center Shutdown 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Wed, 22 Apr 87 15:55:49 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, DenCoff 

Date:        April 22, 1987
 
To:          The World
 
From:        Bill Fischer
 
Subject:     Cupertino Data Center Shutdown
 
*****************************************************************
 
Effective   May  13-15,  1987, the following  employees  will  be 
relocated to the Fremont Data Center, 4340 Solar Way, Fremont, CA 
94537.   While  in  the  process of  moving,  employees  will  be 
available for EMERGENCY calls only.
 
                          Old Number            New Number
 
Bill Fischer             408-446-7762            415-498-2500
 
Ed Roop                  408-446-6656            415-498-2500
 
Debbie Burns             408-446-6629            415-498-2500
 
Linda Freitas            408-446-6630            415-498-2500
 
Vivian Hiatt-Bock        408-446-7150            415-498-2500
 
Bob Jacobs               408-446-7759            415-498-2500
 
Jan Krivanec             408-446-7081            415-498-2500
 
Ingrid Lopes             408-446-8844            415-498-2500
 
Chia-Lin (Julian) Min    408-446-8039            415-498-2500
 
Landa Morris             408-446-6870            415-498-2500
 
Khanh-Quoc Nguyen        408-446-7670            415-498-2500
 
Resources/Validations    408-446-6315 or 6629    415-498-2500
 
Software Distribution    (See individual numbers above)
 
Sue Pal                  408-446-7705            415-498-2500
 
 
 
 
 
 
 
 
 
 
 

The  following employees will be relocated to Fremont Data Center 
as of May 1, 1987.
 
 
Bill Richardson          408-446-7705            415-498-2500
 
David Debroveck          408-446-7610            415-498-2500
 
 
 
THE CUPERTINO DATA CENTER WILL BE OFFICIALLY CLOSED ON 
 
MAY 31, 1987.
Received: from D27.Tymnet by X930.Tymnet; Thu, 23 Apr 87 12:34:12 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Thu, 23 Apr 87 14:27:55 CDT 
To: Monitor Group <JMS>, Carl@930, Osman@930 
Cc: FletcherC@930, DenCoff@930 
Subject: Usage of D27 

It is 2:30pm Dallas time.  If you exclude the OPER job, CHKPNT, XEXEC, PAM,
and SRACRT running CRTSLV, there are only 2 jobs.  SRA detached running FTICP
and VALCMC running 1022.  This particular KI has 1 meg of memory, 7 disks
on-line, and enough empty space to shrink down to only 4 disks.  No wonder
they want to change it.		/Joe
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Sat, 25 Apr 87 22:18:42 PDT
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 25 APR 87 07:19:12 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: OPERA.C/ELDRED@Ontyme.Tymnet, IPC.S/METCALF@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	OPERA.SUP@Ontyme.Tymnet 
Message-id: A52309@Ontyme.Tymnet 
Subject: New ASP Software, Version 7 

                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      25 APR 87  07:17

TO>        Cheryl Eldred

COPIES>    Steve Metcalf
           Osman Guven
           Carl Baltrunas

FROM>      Rick Daniels


SUBJECT>   New ASP Software, Version 7


-----------------------------------------------------------------------


On 23/24 April, this software was used to take an ASP dump of F30.
The SAVE portion worked flawlessly.  When VERIFYING the tapes, some changes
had to be made to the software to make it work.  After changes were made,
the VERIFY worked.

After completing the SAVE and VERIFY, another change was made to make
RESTARTING a tape easier.  The SAVE portion of this software is ready to
be used.

Before putting it into production on all Hosts, one other phase must
be done - REBUILDING from the ASP Save, Version 7.

What is required is that two Hosts here at Fremont be scheduled to have 
this done.  Recommended hosts are:  F70 and F33.  F26 should be scheduled
sometime later on.  This schedule allows testing on all types of configeration.

Software Support will be contacted to determine the shortest procedures
to accomplish this so downtime will be kept to a minimum.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 23 Apr 87 14:44:11 PDT
From: IPC.SRAMAIL@EMSTXS.Ontyme.Tymnet 
Date: 23 APR 87 21:36:18 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A51726@Ontyme.Tymnet 
Subject: SYSTEM D57 SHUTDOWN 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Sat, 25 Apr 87 22:24:22 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, DenCoff 
SYSTEMD57SHUTDOWN: 

                         M E M O R A N D U M





DATE>      23 APR 87  13:35

TO>        ALL SYSTEM D57 USERS

COPIES>    

FROM>      DEBBIE BURNS
           RESOURCE PLANNING & MANAGEMENT


SUBJECT>   SYSTEM D57 SHUTDOWN


-----------------------------------------------------------------------




     RPM, IN A COOPERATIVE EFFORT WITH DALLAS OPERATIONS
WILL BE PERMANENTLY CLOSING SYSTEM D57 ON MAY 31, 1987.

     ALL INTERNAL USERS WHO ARE VALID ON SYSTEM D57
MUST TRANSFER ALL OF THEIR FILES AND TAPES TO ANOTHER
PDP10 SYSTEM BY MAY 18, 1987.

     ALL USERS WILL BE REMOVED FROM SYSTEM D57 ON MAY 18,
1987.

     IF YOU HAVE ANY QUESTIONS, CONCERNS OR REQUIRE
ALTERNATE RESOURCES, PLEASE CALL ME AT 408-446-6629 OR
SEND AN ONTYME TO IPC.SRAMAIL.

     YOUR COOPERATION IN THIS MATTER IS GREATLY APPRECIATED.
From: Joe Smith <JMS@X930> 
Date: Mon, 27 Apr 87 14:59:09 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: St. Louis location 

I am staying at the Park Terrace Hilton (314)426-5500 room 1514.
Roe West's telephone (by the KL's) is   (314)234-5588.
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 27 Apr 87 15:41:20 PDT 
To: Joe Smith <JMS@X930> 
Subject: Re: St. Louis location 
In-reply-to: your message of Mon, 27 Apr 87 14:59:09 PDT

Hello there Joe..
Having fun??
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 28 Apr 87 14:42:45 PDT
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 28 APR 87 12:54:33 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.O/GUVEN@Ontyme.Tymnet, OPERA.SUP@Ontyme.Tymnet 
Message-id: A53250@Ontyme.Tymnet 
Subject: "Osman; Under my ID on 74 the is"... 

Osman;

Under my ID on 74 (DANIELSR), the is a document I would like for you to
look at and if there is something I missed or is to be added, please get
back to me on it before Friday.

The file is called ASPDMP.MEM

Thanks Rick
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 28 Apr 87 17:11:34 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: /B03 Monitor & GTSAP1 crash.. 


Hello Joe..

Remember the crashes on D55 last weekend right after rebuild on
3652 disk. Running with /B03 monitor they were keep crashing with
GTSAP1 STCODE. System would not run more than hour or so.

When operation try to bring F26 up with /B03 this mornning, system
crashed with GTSAP1 and crash is saved. System is now up with
SYSTEM which is /B01.

Only crash saved is on F26. The ones on D55 could not be saved because
of CRASH.SAV being 0 word long and BOOTS not quite working.

-Osman-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 29 Apr 87 12:23:49 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Micronode 2346 

Yesterday, I took an LSI-11/03-SE box and a BA11-ME box from the 2020 rack
and substituted the DUV-11 and DRV-11 cards from 2346 into it.  It is now
in the network as node 2346 and has been up for 20+ hours so far.  The old
LSI-11/03-LJ box is sitting on the table in the lab.

The new 2346 has the processor and memory boards that were in the box in
the 2020 rack... so, we have verified that it is not any of the communications
boards rthat were causing the problems.  We have not eliminated either the
11/03-LJ box itself (power supply etc) or the cpu and memory boards that
were in the old box.  We can worry about that later.... for now we have a
working network.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 30 Apr 87 18:09:14 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: 2020 Progress (Cough) 

Some progress has been made in getting the 2020 up and running...
however, not enough...

1) By swapping the RH11's we can boot from magtape on UBA1/RH=776700
2) We have a KS1LOD.DMP file which uses UBA1 and the first RH.
3) We can load in the monitor.

after swapping the RH11s back to their original positions, we can start
to bring the system up... however, we get a PAGEME (using KSXN13) or an IME
stopcode.  IME in FILIO @ ERRRCV+12/LDB T2,PGYPNO  just after LDB PG,UNYPAC.
THis is the normal case.  If I load DDT and try to single step, or put in a
breakpoint at ERRRCV+anything it prints $n>> and then falls into the infinate
loop printing the "Manual restart from 147" message without printing any of
the stopcode info.

  All of this happens just when I try to enter FILES... I haven't gotten far
enough to even try to refresh the disks.

  The above error got me thinking maybe one of the drives/packs has a problem
so I moved the plugs around and tried a single pack (the one that had gotten
the home page inconsistancy errors) and the stopcode PCBIO(FINPCB+13) started
occurring.  At this point, I'm stopping for the evening.

Joe: If you have any ideas... let me know via MAIL, or call in the afternoon.

  And another thing... we only have 448K of memory (7 of 8 boards) in the
machine... are we missing one? is it out for repair? does anyone know?

Craig: Any help we can get on getting the other RH11 fixed, or the memory
       checked out... I doubt these stopcodes are all software problems...
       also that first drive, write-locked lamp...

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 30 Apr 87 19:26:45 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: ABLE dual I/O model 10039 

1) I have the documentation from Dennis Ernst in my office.  He believes
   one of the DR's on the card still works, but doesn't know which one.

2) Bill Wu mentioned that ABLE is still doing business and we could most
   probably have the card repaired by them, thus giving us our full DR-11
   link capability for our network.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 3 May 87 2:14:56 PDT 
To: Craig Fletcher <fletcherc@X930.Tymnet>, Monitor Group <CARL@X930>, Osman
	Guven <Osman@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Cc: Dennis Coffey <dencoff@X930.Tymnet> 
Subject: Incidental location costs 

Craig,
  Dennis Ernst tells me we can not expect to run our DUV-11 or DUP-11 line
at speeds greater than 9600.  The devices will do 19.2, but the software and
the LSI-11 cannot handle the interrupts at those speeds... in fact they are
often having data over-runs at 9600 due to the single interrupt capability
in the 11/03.

  If we assume we are to house our machines 1.5 miles away from our normal
workplace with no actual operational support, is anyone taking into account
the reduced productivity caused by having to drive that 3 miles, parking 2
times (assuming that the light rail does not extend that far?), or the cost
of having to drive or take public transportation just to reload our hosts
or the micro-bases?

  Or... the fact that we would more or less be unable to car pool on any
given day, (since we might need our own auto at any time to drive over to
our lab machines?)

  I realize that these qustions might seem picky, not to mention the extra
expense of communications, maybe forcing us to use more TYMNET resources on
the public net...  we might not actually save anything in the long run by
moving our machines to another site...  >>KICK<< >>TUMBLE<<  [soapbox just
forcibly removed from beneath my feet].

  Anyway, I think I've voiced what concerns that have come to haunt me.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 3 May 87 2:29:37 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Micronet and Host numbers 

I've talked to validations and tehy see no reason why we cannot use some of
the two-digit host numbers for our hosts.  I still need to check with Ed Roop
about using some of the old 940 host numbers (although Jan said we could use
36 after this weekend, and 57 at the end of May) such as 10 thru 21. If so,
we can then re-use 118, 930 and 934 for DZ-11 and DZV-11 devices on our net
so that we can build circuits to them from TYMNET.

More later... probably by meeting time on Wednesday.

/Carl
From: Joe Smith <JMS@X930> 
Date: Mon, 4 May 87 19:00:17 PDT 
To: danielsr@74 
Cc: carl, osman 
Subject: F33 monitor ready for 3652. 

F33-P035/B03-1 has been copied from (M33)33XB03 to (SYS)NEWMON.SAV.  This
monitor is defined for one physical string of 3652 disks with the GTSAP1
bug fixed.   /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 5 May 87 2:54:09 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Takeit %50 

I modified TAKEIT to continue even though it cannot access DSK or create
a LOG file.  (IF it can, it will.  If not, C'est la vie).

I also added code to set and clear KSYS.

Except for one small detail of the HOLD command (TAKEIT keeps an internal
counter and holds it steady, while the monitor KSYS counter doesn't have
the capability to "hold" at a specific value) which needs to be discussed
and implemented based on that discussion, TAKEIT %50 is ready for testing.

One possibility is for TAKEIT to reset the KSYS word in the monitor each
minute.  I put in the hooks and code for that, but it isn't quite turned
on (Need to move it to another location).

Another is that we set the sign bit, (only allowing 17 bits for the value)
and the sign-extended 18th bit says HOLD the current value.  (Since we do
not allow the monitor to go negative... true? [I didn't check]).

We can also change the SETUUO to use ac/ fn,addr   addr/ bits,,time
   or even better                    ac/ bits+fn,,time
   or add another setuuo or two... HOLD KSYS, CONTINUE KSYS

Comments...

/Carl
From: Joe Smith <JMS@X930> 
Date: Wed, 6 May 87 7:12:02 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hotline call F26 

 6-May-87 06:22/Joe
Contact:  System F26, Fremont (415)498-2588, Tim Witherspoon.

Problem:  OPER says "NO PASSWORD FILE".  WC was missing.

Action:   Set WC on password file.  Creation date was 5-May at 13:21.
          Unknown as to why WC was missing.

From: Joe Smith <JMS@X930> 
Date: Wed, 6 May 87 9:39:31 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Cause of GTSAP1 stopcode in /A02, /A05, and /B03. 

There were two changes to SWAMP involved.  1) A DSKOFF instruction was moved to
avoid possible race conditions, and 2) better load balancing by putting new 
files on the unit with most space. 

The real problem was when the DSKOFF instruction was moved and the nearby
"JRST .-3" was not noticed.  The fix was to put a label at the right place and
jump to it.  The bug did not cause problems on a lightly loaded system, the
GTSAP1 crash occurred only when SAT I/O was in progress at the instant that
a new job tried to log in.

The second change caused SAT I/O to occur when new files were being created.
It forced the RIB of the new file to be created on the unit with the most
space instead of the SAT that happened to be in core.

Now that both changes are in and working properly, the monitor allocates disk
space better.  This should cause a noticable performance improvement when
DSKB is rebuilt from TITO all-files.   /Joe

From: Joe Smith <JMS@X930> 
Date: Wed, 6 May 87 9:57:28 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Monitor distribution. 

Systems running P035/B01 (3650 or 3652 disks)
  D25(52B), F26(52B), D31(52B), D37(52B), D54(52B), D55(52B), F58(50B), F70(50P)
Systems running P035/B03-1 (changes for AR/ARX, GTSAP1, CTYLOG)
  F29(75B), X95, X930			(F29 is a 3330-II blocks system)
Systems with /B03-1 in (SYS)NEWMON.SAV but are not running it
  F26(52B), F33(52B), D55(52B)		(F33 converting to 52B on 9-May?)
Systems with /B03-1 in (M33)##XB03.SAV (needs to be copied to NEWMON)
  D25, D37, D54, F70

 7:27:39 on Wed May 6,1987
TYMSHARE F22-P035/B-1	DSKB:  73802 Pages free, 21 users
TYMSHARE D23-P035/B-1	DSKB:  26600 Pages free, 64 users
TYMSHARE D25-P035/B01	DSKB: 197277 Pages free, 43 users
TYMSHARE F26-P035/B01	DSKB:  65593 Pages free, 70 users
TYMSHARE D27-P035/B-1	DSKB: 195828 Pages free,  8 users
TYMSHARE F29-P035/B03-1	DSKB:  32030 Pages free, 14 users
TYMSHARE F30-P035/B-1	DSKB: 214208 Pages free, 14 users
TYMSHARE D31-P035/B01	DSKB: 118354 Pages free, 26 users
TYMSHARE F33-P035/B-1	DSKB:  21767 Pages free, 18 users
TYMSHARE D34-P035/B-1	DSKB: 108916 Pages free, 13 users
TYMSHARE D35-P035/B-1	DSKB:  67249 Pages free,  8 users
TYMSHARE D37-P035/B01	DSKB: 340244 Pages free, 83 users
TYMSHARE F38-P035/B-1	DSKB: 104473 Pages free, 33 users
TYMSHARE D54-P035/B01	DSKB: 315950 Pages free, 64 users
TYMSHARE D55-P035/B01	DSKB: 712371 Pages free, 31 users
TYMSHARE D56-P035/B-1	DSKB: 113919 Pages free,114 users
TYMSHARE D57-P035/B-1	DSKB: 244123 Pages free, 13 users
TYMSHARE F58-P035/B01	DSKB:  72461 Pages free, 11 users
TYMSHARE D65-P035/B-1	DSKB:  26709 Pages free, 53 users
TYMSHARE F70-P035/B01	DSKB: 125625 Pages free, 24 users
TYMSHARE F74-P035/B-1	DSKB: 197314 Pages free, 31 users
TYMSHARE X95-P035/B03-1	DSKB:   7121 Pages free,  5 users
TYMSHARE X930-X62/B03-1	DSKB:   1495 Pages free,  8 users
From: Joe Smith <JMS@X930> 
Date: Thu, 7 May 87 19:14:51 PDT 
To: DANIELSR@74 
Cc: OSMAN, CARL 
Subject: New B39-P034/P-14 has been built 

39034P.SAV was created on (OSP:29) and telecopied to (JMS:39) and should
be in (M33:39)39034P.SAV.  This monitor is set up for 3 strings of disks,
BPA0-BPA7, BPA10-BPA17, and BPB0-BPB7.    /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 9 May 87 4:43:46 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: (OSU:930)TAKEIT 

TAKIET.SAI is nearly ready... but not quite... I've been pretty busy and
did not finish what I started.  %47 is back up on SYS.  Apparently TAKEIT
is doing something I don't see readily to keep the count internally even
with the changes I made for reading the cound from the monitor...  That is
all that needs to be fixed... (other than the documentation comments Joe
has already made, I didn't touch that yet).

See you all next month.

/Carl
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 12 May 87 12:42:33 PDT
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 12 MAY 87 11:39:43 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: IPC.T/MARCONI@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	OPERA.SUP@Ontyme.Tymnet 
Message-id: A58504@Ontyme.Tymnet 
Subject: "Tom; Under FREMONTDOC:74 there"... 

Tom;

Under FREMONTDOC:74 there is a document on how to take ASP dumps uding
the CONSOLE coommand.  The filename is ASPDMP.MEM.

You are welcome to have your Operators use it, if you desire.

Let me know if you have any comments.

Thanks Rick
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 12 May 87 12:42:41 PDT
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 12 MAY 87 11:51:46 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: IPC.T/MARCONI@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	OPERA.SUP@Ontyme.Tymnet 
Message-id: A58522@Ontyme.Tymnet 
Subject: "Tom; Also under FREMONTDOC:74, "... 

Tom;

Also under FREMONTDOC:74, there is a document I will be using when
we convert F33  from 3675's to 3652's.  The filename is F33CON.MEM.

You might want to look it over to see if there is anything in it you can
use in the future.

Let me know your comments.

Thanks Rick
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 8 May 87 10:44:51 PDT
From: IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet 
Date: 08 MAY 87 18:15:25 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A57351@Ontyme.Tymnet 
Subject: Cupertino Data Center Shutdown 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Tue, 12 May 87 15:37:17 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, DenCoff 

Date:        May 8, 1987
 
To:          The World
 
From:        Bill Fischer
 
Subject:     Cupertino Data Center Shutdown
 
*****************************************************************
 
Effective   May  18,  1987,  the  following  employees  will   be 
relocated at the Fremont Data Center, 4340 Solar Way, Fremont, CA 
94537.                                                            
 
 
                          Old Number              New Number
 
Bill Fischer             408-446-7762            415-498-2521   
                                                 415-498-2522
 
Ed Roop                  408-446-6656            415-498-2562
 
Debbie Burns             408-446-6629            415-498-2515
 
Linda Freitas            408-446-6630            415-498-2516
 
Vivian Hiatt-Bock        408-446-7150            415-498-2563
 
Bob Jacobs               408-446-7759            415-498-2558
 
Jan Krivanec             408-446-7081            415-498-2556
 
Ingrid Lopes             408-446-8844            415-498-2564
 
Chia-Lin (Julian) Min    408-446-8039            415-498-2560
 
Landa Morris             408-446-6870            415-498-2557
 
Khanh-Quoc Nguyen        408-446-7670            415-498-2559
 
Resources/Validations    408-446-6315 or 6629    415-498-2514
 
Software Distribution    (See individual numbers above)
 
Sue Pal                  408-446-7705            415-498-2500
 
 
 
 
 
 
 
 
 
 


The  following employees will be relocated at Fremont Data Center 
as of May 4, 1987.
 
 
Bill Richardson          408-446-7705            415-498-2500
 
David Debroveck          408-446-7610            415-498-2500
 
 
 
 
****************************************************************
*                                                              *
*    THE CUPERTINO DATA CENTER WILL BE OFFICIALLY CLOSED ON    *
*    MAY 31, 1987.                                             *
*                                                              *
****************************************************************
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 11 May 87 16:44:14 PDT
From: TYMHQ.SUP@EMSTXS.Ontyme.Tymnet 
Date: 12 MAY 87 00:17:22 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: I32837@Ontyme.Tymnet 
Subject: Bert Novak's New Assignment 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Tue, 12 May 87 15:37:43 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, DenCoff 
BertNovak'sNewAssignment: 

                                     MCDONNELL DOUGLAS
                                 INFORMATION SYSTEMS GROUP
                                          TYMNET

DATE>      11 MAY 87  15:19

TO>        Tymnet

COPIES>    

FROM>      Clint DeGabrielle


SUBJECT>   Bert Novak's New Assignment


-----------------------------------------------------------------------


Effective April 27, 1987 Bert Novak has been appointed President
of McDonnell Douglas Field Service Company.  We are pleased with 
Bert's appointment and look forward to closely working with him.  
At the present time, we are in the process of selecting a successor
for Bert here at Tymnet.  It is our intention to keep Bert's 
group intact and to replace Bert as soon as possible.  

Until such time as a replacement is named, Bert's organization 
will report to me.



Clint DeGabrielle
Vice-President Marketing and Field Operations
Tymnet McDonnell Douglas

/to
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 13 May 87 10:42:58 PDT
From: IPC.L/MORRIS@EMSTXS.Ontyme.Tymnet 
Date: 13 MAY 87 09:31:25 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A58933@Ontyme.Tymnet 
Subject: Host # Order 
HOSTNUMBERORDER: 

     ATTN:  CARL BALTRUNAS

   COPIES:  OSMAN GUVEN

     FROM:  LANDA MORRIS

     SUBJ:  HOST NUMBER ORDER


     HI!  I HAVE PLACED AN ORDER WITH TYMNET TO RESERVE FOUR (4) PDP-10
HOST NUMBERS FOR YOUR USE.

     THIS IS PER CARL'S ONTYME #A54482 SENT 4-30-87.  ON THE ORDER I
LISTED CARL AS THE PRIMARY CONTACT PERSON AND OSMAN AS THE SECONDARY
CONTACT.

     I WILL KEEP YOU POSTED ON THE STATUS OF THIS ORDER.

     IF YOU NEED MORE INFORMATION I CAN BE REACHED AT EX. 6870.  AFTER
5-18 I WILL BE AT (415)498-2557.  (WE'RE MOVING!)


THANK YOU,
LANDA/RPM
From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Date: Wed, 13 May 87 16:05:12 PDT 
To: nets.p/hidy@ontyme, netops.v/stafford@ontyme 
Subject: Network validation: Group 44 access needed. 
Resent-from: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Resent-date: Wed, 13 May 87 16:19:12 PDT 
Resent-to: (Monitor Group:), Carl, Osman, Joe <JMS> 
Resent-cc: Craig <FletcherC>, TXSSup 



                                  MEMORANDUM


                                                              TYMCOM-X SYSTEMS

DATE:  13-May-87    16:06:18

  TO:  Vida Stafford

  CC:  Paul Hidy

FROM:  Dennis Coffey  (Tymcom-X Systems)

SUBJ:  Network validation:  Group 44 access needed. 

------------------------------------------------------------------------------

Vida,

    The four of us who support the Tymcom-X monitor and utilities need access
to outdial network hosts, in order to support Tymcom-X systems which are not
on the public network.  The need is not frequent, but when the need arises the
need is urgent.

    I have spoken with Paul Hidy about our need.  He indicated that he would
be informing you of his approval of our request.

    The usernames needing vlaidation in Group 44 are:
        DENCOFF
        CARL
        JMS
        OSMAN

    If there are any questions or problems with this request, please call me
at X2588 (415/794-2588), or Ontyme TXS.D/COFFEY.


                                                Thanks,
                                                Dennis Coffey
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 15 May 87 8:41:10 PDT
From: TYMOPS.V/STAFFORD@EMSTXS.Ontyme.Tymnet 
Date: 15 MAY 87 08:27:31 
To: TXS.D/COFFEY@EMSTXS.Ontyme.Tymnet 
Message-id: M92587@Ontyme.Tymnet 
Subject: access to group 44 
Resent-from: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Resent-date: Fri, 15 May 87 11:10:33 PDT 
Resent-to: (Monitor Group:), Carl, Osman, Joe <JMS> 
Resent-cc: Craig <FletcherC>, TXSSup 
Networkvalidation: Group 44 access needed.

Dennis

Your request per the following ontyme has been completed.

Vida

Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Wed, 13 May 87 16:36:41 PDT
From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Date: Wed, 13 May 87 16:05:12 PDT 
To: netops.v/stafford@ontyme, nets.p/hidy@ontyme 
Subject: Network validation: Group 44 access needed. 
Resent-from: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Resent-date: Wed, 13 May 87 16:34:45 PDT 
Resent-to: TYMOPS.V/STAFFORD@ONTYME 



                                  MEMORANDUM


                                                              TYMCOM-X SYSTEMS

DATE:  13-May-87    16:06:18

  TO:  Vida Stafford

  CC:  Paul Hidy

FROM:  Dennis Coffey  (Tymcom-X Systems)

SUBJ:  Network validation:  Group 44 access needed. 

------------------------------------------------------------------------------

Vida,

    The four of us who support the Tymcom-X monitor and utilities need access
to outdial network hosts, in order to support Tymcom-X systems which are not
on the public network.  The need is not frequent, but when the need arises the
need is urgent.

    I have spoken with Paul Hidy about our need.  He indicated that he would
be informing you of his approval of our request.

    The usernames needing vlaidation in Group 44 are:
        DENCOFF
        CARL
        JMS
        OSMAN

    If there are any questions or problems with this request, please call me
at X2588 (415/794-2588), or Ontyme TXS.D/COFFEY.


                                                Thanks,
                                                Dennis Coffey
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 15 May 87 14:42:54 PDT
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 15 MAY 87 13:01:52 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (23 names) 
Message-id: A60211@Ontyme.Tymnet 
Subject: CRASH MAIL ON TYMCOM-10's 

                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      15 MAY 87  13:00

TO>        ALL OPERATORS
           SHIFT SUPERVISORS

COPIES>    CHERYL ELDRED
           OSMAN GUVEN

FROM>      RICK DANIELS


SUBJECT>   CRASH MAIL ON TYMCOM-10's


-----------------------------------------------------------------------


       The intent of this memo is to remind all operators of what to
       include in crash mail and to make the crash mail uniform and
       standard.  This memo was previously submitted on 22 February
       1985 and 27 November 1986.

       Comments as HELLO USER, SORRY FOR THE INCONVIENCE, ETC., should not
       be included in the mail.  CRASH MAIL should be sent at all times the
       system crashes, no matter if it is PRIMETIME or NON-PRIMETIME.

       SYSTEM DOWN       ______ ____

       Brief description why the system is  going  down  or  taken  down.   For
       example:

             (1) System down with BOOTS LOADED crash. Please include
                   information about the STOPCD name.

             (2) System down with BASE INITIATED crash.

                                                                Page  2

             (3) System down with NO CTY output.

             (4) System down with message on Cty:

                 CRAM parity error or

                 FM parity error or

                 Fatal processor error or

                 KL halted  PC/VMA or

                 EXEC parity error, if available indicate

                     SBUS FN=xxxxxxx,,xxxxxx

                     ERA=xxxxxx,,xxxxxx

                     CONI=xxxxxx,,xxxxxx

             (5) System taken down by Deposit 1 in 30.
                 (This should not occur unless all other procedures have been
                  tried)

                 If so indicate if it came to boots.

             (6) System scheduled down for ASP Dump.
                 (Include any problems during the Dump)

             (7) System scheduled down to ADD DISK PACK.

             (8) System scheduled down for:

                 Disk P/M

                 Memory P/M

                 CPU P/M

                 ETC

       If anything is done to the hardware, please indicate.  For example:

             (1) Reconfigured memory.

             (2) Deselected memory.

             (3) Reloaded base.

             (4) Re-IMPL ed disk controller.

             (5) Moved a disk pack from drive A1 to D2.
                 (Include BPA#, BPB#, BPC# and PACK ID number)

             (6) T/M Replaced module in cpu, memory, disk controller, base etc.
                 T/M Replaced power supply in CPU, MEMORY, DISK CONTROLLER,
                 BASE, ETC.

                                                                Page  3

       DATE/TIME        Date and time of crash or down time.       _________

       MONITOR VERSION  Version of monitor system was running and coming up on.       _______ _______
                        Indicate if monitor is loaded from disk or tape.

       CRASH FILE NAME  Name of saved CRASH, CRA001.SAV etc.       _____ ____ ____

       PHYSICAL MEMORY  Amount of total physical memory online. Indicate       ________ ______
                        if there is a "HOLE" in memory.

       DSKCLN           Type of DSKCLN ran. Indicate if system chosen or       ______
                        forced (Operator chosen).

       UPTIME           Time system started to run DSKCLN.       ______

       ANSWERED         Time system available for T/S (Time Sharing)       ________

       NAME             Full name of the operator sending the crash mail.       ____
                        (NO INITIALS or FICTICIOUS NAMES)
z
From: Joe Smith <JMS@X930> 
Date: Fri, 15 May 87 15:00:57 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: REFSTR not working for 2020 systems. 

REFSTR assigns 64 pages for the KS front-end file (SYS)FEFILE.SYS, and
allocates page 14 for its RIB, but does not write the RIB to disk.
The first time a full DSKCLN is run after refreshing the disk, the pages
assigned to the front-end file system get marked as available for use.
The changes to fix this will only affect 2020 monitors.  /Joe
From: Joe Smith <JMS@X930> 
Date: Fri, 15 May 87 15:33:28 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Running ERRCHK on 2020's. 

How do you run ERRCHK?  The built-in help message is pretty worthless in that
it does not say what order the commands are to be given, how to start it
running.  I know it understands BP devices, but what about RM (KS) or AP (F3)
disks?  Do we need to modify it to understand 3650 disks?    /Joe
From: Joe Smith <JMS@X930> 
Date: Sun, 17 May 87 18:08:57 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, WRS 
Subject: Inactivity timeout bug fixed. 

There is a bug in the job inactivity timeout killer that has been fixed.
The bug has existed for some time, and caused F29 to crash 3 times this
weekend.  The noname stopcode at TTYFND+2(SCNSER+13721) was triggered when
the inactivity timeout monitor noticed a job logged in as MAIL running TMS
that had been idle for more than 15 minutes.  In the process of killing it,
accumulator J got set to 777454, which is not in the JBT tables.  The was
not noticed on F29 before because (MAIL)TMS was not running on that system
until this weekend.  (Bill said he was moving the major MAIL functions from
system 39 to system 29.)  This may also explain some of B39's crashes.

The fix is in (M33)INTKIL.PAT on all systems, and has been applied to
(SYS:29)SYSTEM.SAV=F29-P035/B03-3 and (SYS:29)NEWMON.SAV=F29-P035/B04-3.

			/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 19 May 87 7:44:11 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: To reach me 

If you need to reach me... MAIL is fine for messages.  I will log in and
check a few times a day (when able).  I am staying at the Red Bull Motor Inn
(a best western) on rt. 22 in Somerville, NJ.   The phone is 201-722-4000
room 271.

During AT&T office hours, I should be reachable at 201-580-5520.  Angela is
at 201-580-5520 (AT&T) or 201-822-3100 (MDC office).

/Carl
From: Joe Smith <JMS@X930> 
Date: Tue, 19 May 87 13:52:10 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Preventing XEXEC from running after DSKCLN during system rebuild. 

As an interim solution, I have modified INITIA to not start the XEXEC job
if the file (XEXEC)INITIA.SYS does not exist.  Instead, it will output
"?INITIA cannot run XEXEC until (XEXEC)INITIA.SYS is restored from tape."
every 5 minutes until the file is created or until the OPER kills the
job.  Until Carl finishes SUBOPR, this will ensure that XEXEC does not
start after the full DSKCLN that is necessary between restoring SYS and
the rest of the structure.

If (XEXEC)INITIA.SYS is accidently deleted, just create a new one.  It
doesn't matter what's in the file as long as it exists.
/Joe
From: Joe Smith <JMS@X930> 
Date: Wed, 20 May 87 0:45:27 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hotline calls 

20-May-87 00:27/Joe
Contact:  Fremont Operations, Bonnie, (415)498-2558, System F33.
Problem:  Front-end 11 died, tried to run TAKEIT to bring system down, but
          CHKPNT was stuck with JACCT on (it was waiting for the SETUUO to
          finish typing on the CTY which was not responding).
Solution: Logged in, tried to set license to use MDDT, but got stuck in ML
          because CHKPNT was stuck.  Told them to get FSC to try to clear the
          PDP11 that KLDCP could deposit nonzero in 30.  Else power off KL.


18-May-87 23:38/Joe
Contact:  Fremont Operations, Bonnie, (415)498-2558, System F33.
Problem:  As per procedure, they needed one of us to give license to XEXEC.SAV


From: Joe Smith <JMS@X930> 
Date: Wed, 20 May 87 1:13:36 PDT 
To: CARL, OSMAN, DENCOFF 
Subject: Cute interaction between FIND and PCOM/EXECX 

Guess what happened when I submitted the following CTL file:
	FIND M7860 M8204 M7819 M7269 M7294 (M33)*.*/O
	FIND M7860 M8204 M7819 M7269 M7294 (KS2020)*.*/O
	FIND M7860 M8204 M7819 M7269 M7294 (JMS)*.*/O
From: JMS@X930.Tymnet 
Date: Thu, 21 May 87 23:30:42 PDT 
To: osman 
Subject: "KSXB05 works. I used it to create"... 

KSXB05 works.  I used it to create the one-pack system 95 is running on now.
A bootable tape is in the rack on top of the 2020.  /Joe
From: Joe Smith <JMS@X930> 
Date: Tue, 26 May 87 1:06:17 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hotline calls during Memorial Day Weekend. 

25-May-87 14:30/Joe
Contact:  Fremont Operations, Debbie, (415)498-2558, System F33.
Problem:  Several BADSAT, RIBERR stopcodes, one REDSAT, scattered across units.
Reply:    Try one more full DSKCLN, then call in maintenance to look at disk
          system, I suspect the disk controller.


23-May-87 09:00/Joe
Contact:  Dallas Operations, (214)637-7436, System D55.
Problem:  While restoring files for the rebuild, TITO said some files were not
          getting XC license.
Solution: Poked the job running TITO, told them to continue and re-restore all
          the problem files (including, of course, XEXEC.SAV).



Received: from F29.Tymnet by X930.Tymnet; Tue, 26 May 87 11:32:22 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Tue, 26 May 87 11:19:01 PDT
Return-path: <IPC.L/MORRIS@EMSTXS.Ontyme.Tymnet> 
From: IPC.L/MORRIS@EMSTXS.Ontyme.Tymnet 
Date: 26 MAY 87 11:08:20 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A63557@Ontyme.Tymnet 
Subject: Host # Order 
HOST#ORDER: 

     ATTN:  CARL BALTRUNAS
            OSMAN GUVEN

     FROM:  LANDA MORRIS

     SUBJ:  HOST # ORDER


     FOUR HOST NUMBERS HAVE BEEN RESERVED FOR YOUR USE FOR TESTING PER
CARL'S ONTYME #A54482.  THEY ARE:

     3984
     4117
     4284
     4525

     IF YOU HAVE ANY QUESTIONS, I CAN BE REACHED AT (415)498-2557.


THANK YOU,
LANDA/RPM
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 26 May 87 12:01:25 PDT 
To: jms 
Subject: "Message 151 chars): New Read Received:"... 

Message 151 (748 chars): New Read
Received: from F29.Tymnet by X930.Tymnet; Tue, 26 May 87 11:32:22 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Tue, 26 May 87 11:19:01 PDT
Return-path: <IPC.L/MORRIS@EMSTXS.Ontyme.Tymnet> 
From: IPC.L/MORRIS@EMSTXS.Ontyme.Tymnet 
Date: 26 MAY 87 11:08:20 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A63557@Ontyme.Tymnet 
Subject: Host # Order 
HOST#ORDER: 

     ATTN:  CARL BALTRUNAS
            OSMAN GUVEN

     FROM:  LANDA MORRIS

     SUBJ:  HOST # ORDER


     FOUR HOST NUMBERS HAVE BEEN RESERVED FOR YOUR USE FOR TESTING PER
CARL'S ONTYME #A54482.  THEY ARE:

     3984
     4117
     4284
     4525

     IF YOU HAVE ANY QUESTIONS, I CAN BE REACHED AT (415)498-2557.


THANK YOU,
LANDA/RPM
From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Date: Thu, 28 May 87 10:17:18 PDT 
To: (Monitor Group:), Carl, Osman, Joe <JMS> 
Cc: Craig <FletcherC>, TXSSup 
Subject: New phone numbers for Bldg. C former occupants. 

The folks from Bldg.  C, including the Resource Planning & Management unit and
the Software Distribution & Installation unit, have moved to Solar Way, so
they have new phone numbers.

On top of this, the tie line between here and Solar Way has been taken out.
Because of this, you have to get out of the old Tymshare telephone network
into the public phone network, then dial the exchange and the whole 4-digit
extension number to reach anyone over there.  This is also true of the PDP-10
operators, and Lenny.


the numbers are:

Ed Roop				415/498-2562
Julian Minh			415/498-2560
Vivian Hyatt			415/498-2563

    RPM:
Resources			415/498-2514
Jan Krivanic			415/498-2556
Landa Morris			415/498-2557

    SDI:
Bob Jacobs			415/498-2558
Khanh Nguyn			415/498-2559
Ingrid (Dolovitchenyj) Lopes	415/498-2564

I'm sure there are others, but these are the only ones I can think of whom we
would need to know.


-D.-
From: Joe Smith <JMS@X930> 
Date: Fri, 29 May 87 1:50:11 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: (MPL)TYPBAT types the contents of the BAT pages. 

It uses the same routine that is now part of "TYPE BAT" in the FILES dialog.
If you run (MPL)TYPBAT on 930, the F3 is having problems with record 0 on
head 0 of many cylinders.  If you run it on any of the other systems, you
will see that the 3652 disks have not bad spots, the 3675 disks have a log
of bat spots, and they are all on record 14 of an odd head.  The pattern is
so strong, it looks like BPXKON is having problems.      /Joe
From: Joe Smith <JMS@X930> 
Date: Fri, 29 May 87 1:55:04 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, IPC.T/MARCONI@ONTYME, IPC.R/DANIELS@ONTYME 
Subject: /B04 is expected to have several "%% EVENT stopcode BADPAG" messages. 

Running /B02 or later once will activate the badblock recording routines.
/B04 will output a message on the CTY when a badblock is detected, all
previous monitors noticed the badblock but never said anything.

This means that system that starts running /B04 on the old disks is expected
to get sporadic BATPAG messages until the first time an ASP is done.  During
the ASP, flurry of up to 20 BADPAG messages would not be unusual.  After the
ASP completes, the number of new BADPAG messages should slow to less than
one a week.  Operations can run (MPL)TYPBAT to see how bad the disks are.

The point I'm trying to make is that the sudden flare-up of BADPAG messages
is not the fault of the new monitor.  The disks are getting read errors and
a deficiency in the previous monitors caused problems to not be reported.

		/Joe
From: Joe Smith <JMS@X930> 
Date: Fri, 29 May 87 16:32:36 PDT 
To: Dennis Ernst <DErnst@B39> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Can't load micronet from D57 anymore. 

Help!  System 57 went away.  Right now we have absolutely no way to reload
6657 if it goes down.  We can't use NODLOD on 930 because it's 930's base.
We can't use NODLOD from 95 because routing from 6760 to 2346 is not working.
We can't use LOADII from 57 now that it's gone.  We can't use LOADII from 39
because of passwords thru the gateway.  We would like to use LOADII from 95
but it is not authorized to do so.

Could you go through channels and get LOADII working for us on system 95?
And as a backup, get NEW11 validated on F26 and D31 (since our micronet is
sort of a private net)?

			/Joe
From: Joe Smith <JMS@X930> 
Date: Fri, 29 May 87 18:40:26 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: /B04 should not be run on 3675 disks. 

There appears to be a bug in BPXKON that is aggravated by /B04 doing
contiguous allocation.  I will send another mail message with details,
the following is a list of how many bad spots TYPBAT reported on each unit.
                                Summary from TYPBAT as of 29-May-87
  New 3650/3652 disks
25:3652B 0 0 0 0 0 0 0 0 0 0 0 0
26:3652B 0 0 0 0 0 0 0 0 0 0 0 0 0 0
31:3652B 0 0 0 0 0 0 0 0 0 0 0 0
33:3652B 0 0 0 0 0 0 0 0 0 0 0 0 0 0
34:3652B 0 0 0 0 0 0 0 0 0 0 0 0
54:3652B 0 0 0 0 0 0 0 0 0 0 0 0
55:3652B 0 0 0 0 0 0 0 0 0 0 0 0
58:3652B 0 1 0 0 0 0 0 0 0 0 0 0        1 spot at DSKB1 page 34664
70:3650P 0 0 0 0 0 0 0 0 0 0

  Old 3675 disks with old monitor
23:3675B BAT pages not set up
27:3675B BAT pages not set up
35:3675B BAT pages not set up
37:3675B 0 0 0 0 0 0 0 0 0 0 0 0 0 0
56:3675B BAT pages not set up
65:3675B BAT pages not set up

  Old 3675 disks with P034/B04.  All but 2 spots are record 18 + search error
22:3675B 1 0 2 0 1 1 1 0 0 0 0 0 0 0
29:3675B 2 0 0 3 13 0 0 2 0 1 2 3 0 0
30:3675B 0 1 0 0 0 0 0 3 0 1 0 1 0 0 0 1 1 1 1 0
38:3675B 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
39:3675B 25 31 28 31 31 27 26 0 29 0 31 27 29 31 1
74:3675B 3 0 2 1 0 0 1 2 1 1 2 0 1 0 0 0 0 1 2 0 0 1 0 0 0 0 2 0 2

  Other
95:RM03B 0 0
930:CDCP 32 7 8                 These are all real read errors on the F3's disk

Note that system 39 shows an average of 26 "bad" spots per disk!
From: Joe Smith <JMS@X930> 
Date: Fri, 29 May 87 18:46:38 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Possible explanation for the multitude of BADPAG occurances. 

We have observed two kinds of bad spots on the disk.  A real one is consistantly
bad - you couldn't read it last week and you can't read it this week.  All the
bad spots on 930 (62's disk) are real.  A phantom bad spot causes a read error
once - copying the file to itself clears up the read error.  Almost all of the
read errors on the IBM disks are phantom.

It looks now as if there is (and always has been) a bug in BPXKON.  Whenever the
monitor decides to read in a bunch of pages on the same cylinder, it calls
BPXKON repeatedly until BPXADD says it can't handle any more.  If the pages are
contiguous on disk and contiguous in core, BPXADD is given a request that could
very easily exceed the size of the command list.  When this occurs, BPXADD is
committed to do the transfer, so it has to split it up and do the second part
after getting an interrupt from the first part done.  When a request spans the
track boundry, BPXKON has to create a new select-head command before it
continues building read commands.  I do not like the code that handles end-of-
track and changed that section in CPXKON.

My reason for suspecting BPXKON is that for all but 2 of the read errors, it
was for records 15-18 of an even track.  If BPXADD did not recognize the end
of the track, it would attempt to read records 19-22 of the current track
instead of 1-3 of the next track.  Of all the ERRCHK reports I looked at, they
all said "Search error, Count error, Record 18".

If BPXKON did have this failure in the past, a read error would occur when the
read request filled up the command list and overflowed onto the next track at
the same time.  This scenario is a virtual certainty when running ASP.  ASP
would get 30 errors per disk, not report them (because it turned off error
checking on read), and CORE1 would attempt to log them in the BAT pages when
the pages were released.  Since the BAT self-pointers were not initialized,
nothing would get into the BAT pages.  The only trace left that a read error
had occured is that the "M" bit would be set so the page would not be 
recycled.  This would last only until the next DSKCLN, at which there would
be no log of the phantom read error.

P035/B04 had two changes.  One is that the read errors get logged to the CTY
as BADPAG stopcodes and logged in the BAT pages.  The second is that MAPIO
now makes new files be allocated to contiguous pages on disk when it is
convenient to do so.  This second change makes disk I/O more efficient.  It
also increases the probability that programs other than ASP will cause the
channel command list to get filled up and trigger the BPXKON bug.  CPXKON does
not exhibit this bug.

Therefore I suggest we go forward with /B04 on all 3650/3652 systems.
Until BPXKON is fixed (or CPXKON verified as reliable with 3675 blocks),
the 3675 disk systems should NOT run /B04 (or /B05).

To verify some of the things I've said, we need to run ASH on all systems.
If it is BPXKON, then unreported read errors would cause ASP to write bad
data to tape.  If system 39 is any indication, then the bug means that 30
out of 68680 pages per unit were written bad.

		/Joe
From: JMS@X930.Tymnet 
Date: Sat, 30 May 87 0:12:07 PDT 
To: osman 
Subject: "has sources to P035/B. I built"... 

(OSP:95) has sources to P035/B.  I built 22XB01.SAV from *.B01 in (M33:930).
It has been copied to (M33:22), (M33:70), and (SYS:22)NEWMON.  It is
"F22-P035/B01-1 3652".    The command file is (M33)MON33B.CMD if it needs
to be reloaded.  /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 31 May 87 23:28:46 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: I'm back! 

I got in an hour ago... began unpacking etc... plugged in my beeper
'cause unless I'm mistaken, I'm on call this week (6/1 - 6/7)...

Will see you all later (Monday).
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 31 May 87 23:31:47 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: BPXKON/ASP ? 

Is it possible that the "read errors" we see with BPXKON while running an
ASP are indeed caused by the same BPXKON problem causing the BADPAG stopcd's?

(or am I barking up the wrong tree)?
/Carl
From: Joe Smith <JMS@X930> 
Date: Mon, 1 Jun 87 11:37:48 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: HELP version 6(75)-2 has been distributed to all systems. 

Sometime back we decided to have the HELP command run (SYS)HELP instead of
RPG.  I put a hack into DEC's HELP program so that it can find files on (SYS).
It now responds to "HELP" to give general info, "HELP *" to give a list of
available topics.  Once we have a suitable base of *.HLP files distributed,
we should mention it in (SYS)ACCESS.MSG.   /Joe
Received: from F22.Tymnet by X930.Tymnet; Mon, 1 Jun 87 13:24:18 PDT
Return-path: <DANIELSR@F22.Tymnet> 
From: DANIELSR@F22.Tymnet 
Date: Mon, 1 Jun 87 13:23:16 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman; Just cleared my mail on"... 


Osman;

Just cleared my mail on 22 again and the mail was sent to me by Joe
about the monitor friday night at 2300.  My apologies.

Thanks Rick
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 1 Jun 87 13:22:56 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Telephones at MDC center 

Craig,
  as per the 5.18.87 minutes for the relocation users group concerning
telephones at MDC center...

"Wiring orders have been placed for the telephone system.  The Scheme
calls for two terminals and one voice line per office.  Work areas and
file rooms requiring special requirements need to be identified with
your telephone coordinator.  Departments requiring other than twisted
pair wiring for data need to contact Judy Carlson.  Recommended tele-
phone equipment standards will remain as issued."

Our telephone needs (as we presented them for the current AT&T system
at Liberty) with multi-line capability for the hotline, support-line
and our own individual lines with call-hold (via call-waiting or multiple
lines for our individual stations) do not fall into the "two terminals
and one voice line per office" guidelines.

Are we going to be able to have the multi-line phones in the new offices
with these capabilities?

Will we need more than twisted pair to communicate with our micronodes?
Will our access to our micronodes be via the phone, or via hard lines?
If by phone, what data speeds and might we not want a couple of direct
dial modems installed on dataset lines on the micronodes in the new lab
building?  (I guess we still need to talk to Judy Carlson about this?)

/Carl
Received: from F74.Tymnet by X930.Tymnet; Mon, 1 Jun 87 14:38:30 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Mon, 1 Jun 87 14:36:24 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman; It appears that will be"... 

 Osman;

It appears that will be be doing F58 conversion this weekend.  Will be
converting from 3650's to 3675's.

If a monitor is needed, guess you may as well start on it.  I would like
everything settled by Thursday so that the document I put out has all
informnation.

Thanks Rick
From: JMS@X930.Tymnet 
Date: Wed, 3 Jun 87 23:47:17 PDT 
To: OSMAN, CARL 
Subject: "P035/B06 has been telecopied to"... 

P035/B06 has been telecopied to NEWMON on 32 and 62.  Both need to be reloaded
in the morning.  I tried reloading 32 using TAKEIT, but it's still down.  /JOE
Received: from F29.Tymnet by X930.Tymnet; Thu, 4 Jun 87 16:51:22 PDT
Return-path: <DANIELSR@F29.Tymnet> 
From: DANIELSR@F29.Tymnet 
Date: Thu, 4 Jun 87 16:50:08 PDT 
To: OSMAN, DANIELSR 
Subject: "There is 7 hdat errors on this"... 


There is 7 hdat errors on this system, but I can't find any files
that are bad when running FNDBAD.  Any thoughts on this.

Thanks Rick
From: JMS@X930.Tymnet 
Date: Fri, 5 Jun 87 0:17:07 PDT 
To: osman 
Subject: "Another test for 32 32XB06.SAV"... 

Another test for 32 - 32XB06.SAV is set for 3675B instead of 3652B.  If
that doesn't work, try 29XB03.* to see if TELECOPY still has problems.
From: JMS@X930.Tymnet 
Date: Sat, 6 Jun 87 21:23:19 PDT 
To: carl, osman 
Subject: "I verified that system 32 could"... 

I verified that system 32 could reload from SYSTEM by itself if SW4 is off.
Unfortunately, I set sense switch 4 in order to do a FULL DSKCLN, and forgot
to turn it off.  X32 is probably at ONCE> right now, waiting for a GO.
Whoever gets there first should turn off SW4 so it can continue.  /Joe
From: Joe Smith <JMS@X930> 
Date: Sat, 6 Jun 87 21:26:16 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Status of 32XB03 and 32XB06. 

I spent this afternoon at Building-C trying to find out why 29XB03 worked but
32XB06 didn't.  TELECOPY would not work with 32XB03 either, until I made
32's CONFIG file look like 29's.  The only significant difference was
"XP JOBS,^D64" versus "XP JOBS,^D80".  I tried adjusting JOBS.  TELECO does
not work with JOBS set to 64, 72, 76, 77.  It does work with 78, 80, 108, 127.
I don't know why.  32XB06 works now that I set JOBS to 127 and added the
following to COMMON:
  IFCPU(<KI,KL>),<IFL JOBN-^D78,<
  PRINTX ? TELECOPY won't run with JOBS less than 78>>
From: Joe Smith <JMS@X930> 
Date: Sun, 7 Jun 87 18:28:51 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Fremont node changed. 

They took away my favorite TYMSAT!  The dial-up number in Fremont 490-7366
now is answered by node 7117 instead of 3220.  If you have 3220 in your
SWITCH.INI for LOGON, replace it with 7117.  /Joe
Received: from D34.Tymnet by X930.Tymnet; Tue, 9 Jun 87 13:08:37 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Tue, 9 Jun 87 15:06:19 CDT 
To: OSMAN 
Subject: Test monitor for Dallas on 27 and 34 

The test monitors are on Dallas's KI (D27).
57XB06.SAV is a 5x monitor - KI with BPA+BPC for 3650/3652 disks in blocks.
72XB06.SAV is a 7x monitor - KI with BPA+BPB for 3 strings of 3675 disks.
From: JMS@X930.Tymnet 
Date: Tue, 9 Jun 87 14:45:07 PDT 
To: CARL, OSMAN 
Subject: "Somebody moved the line from the"... 

Somebody moved the line from the LN03 to a different socket on 2770 and
did not put it back.  Two of my listings went into the bit bucket.
From: Joe Smith <JMS@X930> 
Date: Tue, 9 Jun 87 18:19:18 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: AGENDA.TXT has been created in (MONDOC). 

The file (MONDOC)AGENDA.TXT on X930 needs to be updated.  This is what it has
so far:

(MONDOC:930)AGENDA.TXT - Agendas for the Tymcom-X Support Group meetings.

10-Jun-87
1) Old business
   a) Report on changes to systems 31 and 58
   b) Report on status of /B06
   c) Report on KI/KL for our use

2) more to be added here

 3-Jun-87
1) Decide which monitor version to run in Fremont.
   Decided to create P035/B06 from /B03-3 (nothing from /B04 or /B05 except
   to include the DDT patches.

2) Move to South Bay Center
   a) Determine phone requirements - 3 lines with outdial modems.
   b) Determine async requirements - Carl will draw a new diagram.

3) Computing resources
   a) Put 2020's on dispatch.
      Codes are XMR4257 and XMR4267 for new KS and X95 respectively.
   b) Acquire KI10 system in Bldg. C for swing machine.
      Dallas said they would provide a test KI.

4) AT&T Conversion
   a) Report on recomendation
   b) Will IPC fund TOPS10 accounting development?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 10 Jun 87 0:33:19 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: (MONDOC)AGENDA.TXT 

MONDOC is very cluttered... since the agendas are more than monitor meetings
I suggest we keep the file in directory (INFO).

/Carl
Received: from F29.Tymnet by X930.Tymnet; Mon, 8 Jun 87 8:55:15 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Mon, 8 Jun 87 8:48:47 PDT
From: IPC.R/SWANSON@EMSTXS.Ontyme.Tymnet 
Date: 08 JUN 87 16:26:07 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A68354@Ontyme.Tymnet 
Subject: ISG CONVERSION SERVICE 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Wed, 10 Jun 87 18:11:59 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, DenCoff 

                        ISG CONVERSION SERVICE

   THE ISG CONVERSION SERVICE IS SET UP TO SERVE THE NEEDS OF THE ISG 
USER COMMUNITY.  ALONG WITH THE CURRENT CONVERSIONS THAT WE HAVE THE 
CAPABILITY TO DO THERE ARE OVER 490 OTHER CONVERSION PACKAGES 
AVAILABLE TO US.  IF YOU HAVE A NEED TO CONVERT WORD PROCESSING FILES 
WHETHER IT IS WITHIN YOUR OWN DEPARTMENT OR WITH A CUSTOMER,  CALL US 
AND LET US KNOW IF WE CAN HELP YOU IN ANYWAY! 


                        ISG CONVERSION SERVICE
                        (314) 233-8250
                        307-2E-E15
Received: from F29.Tymnet by X930.Tymnet; Wed, 10 Jun 87 16:01:54 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Wed, 10 Jun 87 15:53:57 PDT
From: IPC.I/LOPES@EMSTXS.Ontyme.Tymnet 
Date: 10 JUN 87 23:38:01 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A69761@Ontyme.Tymnet 
Subject: 1022 Replacement Project 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Wed, 10 Jun 87 18:12:40 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, DenCoff 

 
DATE>      10 June 1987   3:33 PDT
 
TO>        Edsel Budar                       Bill Euske
           Fred Canning                      Bob Fultz
           Regina Chen                       Kenneth Holcomb
           Paul Corcoran                     Rai-ki Huang
           Chung Lam                         Stephen Poppe
           A.S. Rajaraman                    Gary Walker
 
FROM>      Ingrid Lopes
 
SUBJECT>   1022 Replacement Project
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
     This is to remind you that we will be meeting with represent-
     atives from Oracle to discuss the benchmark we would like to run
     on that product.  We will host the meeting on Friday, June 12th,
     at 10 am, not 9 am as previously announced.  We  will meet in the
     Resource Support conference room at Solar.  We welcome your
     attendance and contributions to this benchmark process.  If you
     can attend, please call me at 415/498-2564.
 
     Best regards.......Ingrid Lopes
From: Joe Smith <JMS@X930> 
Date: Wed, 10 Jun 87 22:35:16 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hack to BPXKON installed on 58. 

The unit data block has enough room in the command list to handle 18 blocks
(1 track).  If a request for several contiguous blocks starts in the middle
of the track, then it will hit the end of the track before it hits the end
of the command list.  A portion of the spaghetti code near TRNCNT is supposed
to handle this, switch to the next head, and continue adding until the
command list is full.  That is one of the places I was uncomfortable with.
By putting a JFCL at TRNCNT+3, BPXADD won't do any fancy footwork to fake out
FILIO.  It will simply take the non-skip return, which tells FILIO to not add
anything more until the transfer-done interrupt comes in for the current
request.  Then FILIO will resume with the next block (which need not be on
a page boundry).

I logged in to 58 to test this, and found it SHUT due to all the "hard" errors.
Without the patch, ASH got several errors within the first 7000 pages.  With
the patch installed, it got no errors.   I zeroed out the BAT blocks so we
and the CTY can see new entries.  /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 11 Jun 87 1:05:41 PDT 
To: Joe Smith <JMS@X930> 
Cc: Monitor Group <JMS>, Osman, FletcherC, DenCoff 
Subject: Re: Hack to BPXKON installed on 58. 
In-reply-to: your message of Wed, 10 Jun 87 22:35:16 PDT

...atta boy...
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 11 Jun 87 11:11:35 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Setop meeting 

I have a meeting this afternoon with Wayne Bartlett and Bill Soley
about SETOP, PEAK and a few other things... I may or may not return
to the office this afternoon.

If not, I should be available at home sometime after 3pm.  /Carl
My number there is 408-945-4314.

Received: from F29.Tymnet by X930.Tymnet; Thu, 11 Jun 87 11:31:53 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Thu, 11 Jun 87 11:23:58 PDT
Return-path: <OPERA.SUP@EMSTXS.Ontyme.Tymnet> 
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 11 JUN 87 10:55:13 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (25 names) 
Message-id: A70046@Ontyme.Tymnet 
Subject: "SUBJ: M E M O R A N D U M MC DONNELL-DOUGLAS"... 

SUBJ: 





                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      11 JUN 87  10:51

TO>        All Operators
           Shift Supervisors

COPIES>    Cheryl Eldred
           Bill Fischer

FROM>      Rick Daniels


SUBJECT>   Sending Mail regarding STOPCODES


-----------------------------------------------------------------------


Your attention is invited to STOPCODE messages that print out on the
OPR terminal and/or CTY terminal.

Mail should be sent to CRALST on all STOPCODES, whether they be of
the INFO type or not.  Software Support has to be aware of these
messages.

When they print out, please read what they say and follow their
instructions.

Thanks for your attention and concern.

Rick
From: Joe Smith <JMS@X930> 
Date: Thu, 11 Jun 87 12:18:15 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Found BPXKON bug for /B06. 

If the command list is big enough to hold 25 blocks, then there were cases
where BPXKON would transfer the last 2 blocks on one track, and then try to
transfer 23 blocks on the next track.  That does not work - 3675's have only
18 blocks.  BPXKON assumed that the command list was a certain size, and
never checked it.  The following is now in /B06 for systems with 3675 disks.

File 1)	DSK:BPXKON.B03	created: 1431 03-FEB-87
File 2)	DSK:BPXKON.MAC	created: 1149 11-JUN-87

1)14		MOVEI P2,^D18
****
2)14	IF2,<PRINTX [BPCMSZ in COMMOD must big enough for at least 18 blocks]>
2)	;*; XP BPCMSZ,^D<18*7+1+3+2+1>	;LOADER will complain if mismatch
2)	;*; ;Above line commented out - can be more than 18 as of P035/B06.
2)		MOVEI P2,^D18
**************
1)19	TRNCNT:	JUMPE P4,T2POP0	;REALLY OUT OF TRANSFER TO DO
1)		HRRZ T3,P3
****
2)19	;Here from BPXADD if P2 has gone to zero, indicating end of track.
2)	;If P4 is also 0, we have hit the end of the cylinder and should stop.
2)	;If there are more words to transfer, we have to switch heads and
2)	;continue on the next track.
2)	TRNCNT:	JUMPE P4,T2POP0	;STOP AT END OF CYLINDER
2)		HRRZ T3,P3
**************
1)19		JRST T2POP0	;NO, MUST BE OUT OF LIST NOT TRACK
1)		SUBI T3,2(P1)	;SEE HOW MUCH MORE WILL FIT
1)		IDIVI T3,7
1)		LSH T3,B2WLSH
****
2)19		 JRST T2POP0	;NO, COMMAND LIST MUST BE FULL
2)		SUBI T3,2(P1)	;SEE HOW MUCH MORE WILL FIT
2)		IDIVI T3,7	;(7 command words to xfer 1 block)
2)	;Make sure we don't attempt to go past end of track if command list is
2)	; set for ^D25 blocks.  Fixed in P035/B06
2)		CAILE	T3,^D18	;More than one track's worth left?
2)		 MOVEI	T3,^D18	;Yes, limit ourselves to notice end of track
2)		LSH T3,B2WLSH	;# of words that can be handled (full blocks)
**************
From: Joe Smith <JMS@X930> 
Date: Mon, 15 Jun 87 0:12:17 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hotline call 


From: Joe Smith <JMS@X930> 
Date: Mon, 15 Jun 87 0:13:14 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hotline call 

14-Jun-87 23:56/Joe
Contact:  Fremont Operations, Rick, (415)498-2558, System F33.
Problem:  One of the new 3650 disks has hardware problems - kept going offline,
          now gets unit check one a particular cyl/surf/head.  PAKCOPY gets
          stuck there, even with sense switch two set.  Rick wanted to know
          if he should go ahead with ASSIGN and DEASSIGN to reformat that one
          track.  25 blocks will be lost, but the rest of the pack can then be
          copied.

From: Joe Smith <JMS@X930> 
Date: Mon, 15 Jun 87 12:22:35 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: F38 recovering from bad 3650 disk. 

System 38 had the HDA on BPA3 go bad.  PAKCPX had problems copying out the
pack due to "equipment check" on head 21 record 20 on 344 of the 555 cylinders.
I hacked at PAKCPX to get it to continue after such errors.  Rick Daniels read
the numbers to me over the phone, I wrote a quick program to create a DDT file,
and used TSTSUP on system 38 to create (JMS)BADPAG.MUP which had pointers to
all those pages in its RIB.  This caused DSKCLN to report the names of all the
files that were using those pages.  (JMS)FIX38.* shows what happened.  The end
result is FIX38.FIL (which was copied to (OPER:38)FIX38.FIL) which lists 274
files and 3 UFDs being damaged.  Restoring these files will be a lot quicker
than redoing the rebuild.  Time-in=02:18, time-out=12:31.   /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 15 Jun 87 13:03:00 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: WHEN program change 

TO make WHEN work correctly with ARGPAR, only switches may exist in the
SWITCH.INI file.  To make the OLD usage of WHEN work do the following:

Old:  WHEN  /MULTI CARL, OSMAN, JMS
New:  WHEN  /MULTI /USERS:CARL, OSMAN, JMS

You may also specify /USERS:xxx on the command line, but that defeats the
purpose of the command line and overrides it. SO, proper usage is:

WHEN user,user/switch
WHEN ?
WHEN

This works but the results are funny:

WHEN user1,user2/USERS:user3,user4

...this reports user3 and user4, completely ignoring user1 and user2.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 17 Jun 87 2:05:16 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: (INFO)AGENDA.TXT 

I made up an agenda... of course, it was written in a vacuum, so there
may be things I left out, and things I put in that should be thrown out.
Please feel free to look at the agenda and make changes.

I moved it to (INFO)AGENDA.TXT out of (MONDOC) as mentioned last week.
I also typed in the significant things I had noted from the last meeting
as the minutes.  The minutes currently precede the agenda, since they
inevitably replace the agenda.. if we follow it !!!

See you all at the meeting.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 17 Jun 87 2:10:33 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: AGENDA - MINUTES 

Please read the first couple of pages in (INFO)AGENDA.TXT before the
meeting.  The minutes of the previous meeting(s) can be found herein
and should be read prior to the meeting so we do not waste valuable
meeting time rehashing last week's meeting.

/Carl
From: Joe Smith <JMS@X930> 
Date: Wed, 17 Jun 87 19:14:23 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: System 38 will bring up P035/B07 tonight. 

I looked at the tracks left in the circular buffer and found a cause of
THUNGs.  There were several outstanding read requests, including one for unit
324.  When a request for 314 was added, for some reason it got a busy signal.
This is normal, the unit will be restarted when the not-busy signal comes in.
When the transfer on unit 324 finished, the not-busy signal came in as if it
were for units 320-327 instead of for units 300-317 as was expected.  Therefore
CPXKON tried to restart waiting units on BPB (unit 324 is BPB4), but not the
units on BPA (unit 314 is BPA14).  The THUNG was because unit 314 was not
restarted within 3 seconds.  Until we teach COMMOD and CPXKON that a single
controller (BPA) can have more than 16 disks, I put in a one-line hack that
restarts all waiting units when the not-busy signal comes in.  It will now find
any waiting unit, be it on BPA, BPB, BPC, or BPD.

File 1)	DSK:CPXKON.B06	created: 1100 12-JUN-87
File 2)	DSK:CPXKON.MAC	created: 1817 17-JUN-87
1)7	KONEN1:	HLLZ	T1,BPXCUB(J)	;Get mask of 16 unit busy flags
****
2)7		   ;This *HACK* is to allow 3 strings on one 3674 controller
2)		MOVEI	J,BPACB##	;*HACK* Start at 1st string *HACK*
2)		   ;The *HACK* is supposed to fix THUNG problems on F38
2)	KONEN1:	HLLZ	T1,BPXCUB(J)	;Get mask of 16 unit busy flags
**************
1)7	CRST:	JFFO	T1,.+2		;Find first unit that was busy
****
2)7	IF2,<PRINTX [Hack at KONEN1 needs to be eliminated]>
2)	CRST:	JFFO	T1,.+2		;Find first unit that was busy
**************
				/Joe
From: Joe Smith <JMS@X930> 
Date: Wed, 17 Jun 87 21:35:45 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Why /B07. 

Craig and Dennis:
  The previous message did not mention that the decision to create 38XB07
was made by Osman, Carl and myself.  The change is one line to CPXKON, but
is necessary to solve the THUNG problem on 38 and 3652 systems.  We also
decided that all systems currently running /B06 should come up on /B07 as
soon as possible.  The systems we scheduled to run /B06 next week will be
running /B07 at that time.

The correct fix (which will be after P035/C) will be to teach COMMOD and
CPXKON that there can be more than 2 strings on a controller.  The places
that currently store 16 flag bits in a halfword need to be modified to
handle 64 bits in two words.  Then update the CONFIG files so that systems
with many strings on a single physical controller show up as a single
logical controller.  In particular, system 38 will have BPA0-BPA27 instead
of BPA0-BPA17 + BPB0-BPB7, and systems with 3652s will have BPA0-BPA7 +
BPA40-BPA47 instead of BPA0-BPA7 + BPC0-BPC7.

The above change will make CPXKON aware of reality.  When it determines that
controller "BPA" is busy, it will correctly understand that the units on all
4 strings on that controller are busy.  Prior to /B07, CPXKON figured that
because the second 2 strings were assigned to "BPB", that it was a seperate
piece of hardware, and was not affected by "BPA" being busy.

I expect 38XB07 will not get any THUNGS.    /Joe
Received: from F29.Tymnet by X930.Tymnet; Tue, 16 Jun 87 10:19:03 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Tue, 16 Jun 87 9:29:46 PDT
From: NETS.TTE@EMSTXS.Ontyme.Tymnet 
Date: 16 JUN 87 16:49:30 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: I47474@Ontyme.Tymnet 
Subject: TTE Schedule Update 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Wed, 17 Jun 87 21:56:57 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, DenCoff 
TTEScheduleUpdate: 

                            M E M O R A N D U M

                          NETWORK SYSTEMS COMPANY
                         NETWORK SYSTEMS MARKETING


DATE>      16 JUN 87  08:47

TO>        All Employees

COPIES>    

FROM>      Tymnet Training and Education


SUBJECT>   TTE Schedule Update


-----------------------------------------------------------------------


TTE has add the following classes to the schedule-

     TTE 100 Basic Data Communication            June 17-19
     TTE 502 TYMNET Concepts                     July 8-17
     TTE 430 Assembly Language Programming       July 20-24
     TTE 440 TYMNET Engine Programming           July 27-31
     TTE 442 ISIS Slot Programming and Debugg.   September 14-25
     TTE 210 IBM Data Comm and Intro to SNA      October 12-16
     TTE 230 TYMNET 3270/CMT                     October 19-23
     TTE 240 TYMNET SNA                          October 26-30

To enroll for any TTE course through OnTyme access               
  :EXEC *** TTE.ENROLL
From: FLETCHERC@X930.Tymnet 
Date: Thu, 18 Jun 87 8:50:10 PDT 
To: osman 
Subject: "Would you come up with a paper"... 

Would you come up with a paper format similar to the chart presented in
the Wednesday meeting that will show status and expected status of
monitor propagation at Fremont and eventualy include Dallas.  I read mail
about /B07 and concur that /B07 should replace /B06 in propagation as
soon as possible.

	Craig
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 18 Jun 87 8:51:55 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: THUNG Fix, /B07 monitor .. 

I copied P035/B07 from (M33) to (SYS)NEWMON.SAV on systems
F26, F29, F39, F58, F70. Sent mail to oper to bring the
systems up with NEWMON next time they come up. System F38
is running with /B07 as of today.
-Osman-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 18 Jun 87 18:19:31 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Microcycle clock 

The contents of JBTCMP,JBTCP2 are the low&high 2-word result of the
KL EBOX time (RDEACT instruction) since system startup.  JBTCMP(J) and
JBTCP2(J) contain the KL EBOX time per job, since login.

We currently throw away the MBOX time, by overwriting it with the EBOX
time.  ICP reports the total of the EBOX times for all jobs for the
interval taken, divided by the fraction of a second to give MCY/SEC.

/Carl
Received: from F29.Tymnet by X930.Tymnet; Fri, 19 Jun 87 12:17:57 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Fri, 19 Jun 87 12:17:42 PDT
Return-path: <OPERA.SUP@EMSTXS.Ontyme.Tymnet> 
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 19 JUN 87 11:59:03 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (27 names) 
Message-id: A72945@Ontyme.Tymnet 
Subject: NEW MONITORS for TYMCOM-10'S 

                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      19 JUN 87  11:58

TO>        SEE CC LIST

COPIES>    SEE CC LIST

FROM>      Rick Daniels


SUBJECT>   NEW MONITORS for TYMCOM-10'S


-----------------------------------------------------------------------


TYMCOM-10 Software Support will at times send mail to Operations
with instructions to bring a system up on a different monitor
and when it should be done.

Normally it is done during regular scheduled downtime.  The exceptions
to this rule are:

     a.  If a system is continuously going down; or

     b.  If USERS' are having problems with their software; or,

     c.  Problems of an EMERGENCY nature.

In these cases, they may request the system be recycled immediately
after they have solved the problem.  The crash code to use during
this recycle is the same crash code that was used when the problem
first developed.

Whatever shift this occurs on, the Operator should contact their Shift
Supervisor, or Senior Operator.  Please contact me if you cannot contact
the Shift Supervisor, or Senior Operator.
                                                                Page  2


The Shift Supervisor, or Senior Operator, should insure that the system
logbook has complete information entered into it.  They should also 
insure that:

     a.  SSW #4 is set

     b.  A note put on the CTY as to what monitor to bring system
         up on when recycled again or if the system should go down.

     c.  A monitor tape made for the host to use for COLDSTARTS.

     d.  Run DSKMAP to see if it has been updated.

     e.  Run R CRSHID;/SYS on the host to check monitor versions
         installed.

     f.  If there is a difference, then run RU (QASYS)MAPUPD.
         This program will update the DSKMAP.

     g.  When completed, run DSKMAP again and post it on the
         mainframe.

Please contact me if you have questions.

Thanks;

Rick Daniels
Received: from F29.Tymnet by X930.Tymnet; Fri, 19 Jun 87 15:17:05 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Fri, 19 Jun 87 15:14:18 PDT
Return-path: <OPERA.SUP@EMSTXS.Ontyme.Tymnet> 
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 19 JUN 87 15:07:09 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: OPERA.C/ELDRED@Ontyme.Tymnet, OPERA.J/WALLACE@Ontyme.Tymnet,
	OPERA.M/BELLOPATRICK@Ontyme.Tymnet, OPERA.S/MARCOTTE@Ontyme.Tymnet,
	OPERA.SUP@Ontyme.Tymnet, IPC.B/FISCHER@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, TXS.J/SMITH@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.C/FLETCHER@Ontyme.Tymnet 
Message-id: A73074@Ontyme.Tymnet 
Subject: "M E M O R A N D U M MC DONNELL-DOUGLAS"... 

                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      19 JUN 87  15:04

TO>        Shift Supervisors
           Cheryl Eldred
           Bill Fischer

COPIES>    Osman Guven
           Carl Baltrunas
           Joe Smith
           Craig Fletcher

FROM>      Rick Daniels


SUBJECT>   Testing ASP (Version 7)


-----------------------------------------------------------------------


The weekend of 18 July this software will be tested on system 58.
Between now and then, 14 disk packs need to be set up to be used.

For the next few weekends, I will schedule this system down at 2400
on Friday nights until 0800, Saturday mornings.  The regular ASP dump
will be rescheduled for this timeframe.  The remaining time after the
dump is to be used to REFRESH, FORMAT, and INITIALIZE the packs.

On the weekend of 27 June, 7 packs will be done, or as many as possible,
and 7 packs the weekend of 4 July.  If the weekend of 11 July is needed to
finish up, it will be scheduled.  These packs will be identified and set
aside for use on 18 July.

On 18 July, the system will be scheduled down from 2400, Friday night
with a scheduled uptime of 2400, 19 July.

                                                                Page  2

During this time an ASP dump will be taken on F58 using (OSU)ASP.SAV
(Version 7) software.  After tapes have been verified, the system packs
will be taken off-line and put aside.

The packs that were set up previously will be put on-line.

After the file structure has been built, the ASP tapes will be restored.
After the restore, the system will be UNSHUT for users.

Any problems, a backup will be taken (if possible), and the packs will
be switched.

Please see me if questions.
Received: from F29.Tymnet by X930.Tymnet; Sun, 21 Jun 87 9:01:57 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Sun, 21 Jun 87 8:49:02 PDT
Return-path: <OPERA.SUP@EMSTXS.Ontyme.Tymnet> 
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 21 JUN 87 08:40:48 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: OPERA.C/ELDRED@Ontyme.Tymnet, OPERA.J/WALLACE@Ontyme.Tymnet,
	OPERA.M/BELLOPATRICK@Ontyme.Tymnet, OPERA.S/MARCOTTE@Ontyme.Tymnet,
	OPERA.SUP@Ontyme.Tymnet, IPC.B/FISCHER@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, TXS.J/SMITH@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A73198@Ontyme.Tymnet 
Subject: "M E M O R A N D U M MC DONNELL-DOUGLAS"... 

                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS TYMSHARE




DATE>      21 JUN 87  08:38

TO>        Shift Supervisors
           Cheryl Eldred
           Bill Fischer

COPIES>    O. Guven
           J. Smith
           C. Baltrunas

FROM>      Rick Daniels


SUBJECT>   Addendum to A73074


-----------------------------------------------------------------------



This is a follow-up on my ONTYME A73074.

Sequence of events leading up to the ASP test on 18 July and responsi-
bilities:

22 JUN     DAYS

Obtain 14 good disk packs and make up slap-tags.  Slap-tags will be
identified as follows:

     a.  Pack ID

     b.  Vender and serial number of pack.

     c.  NEW (ASP VER 7)

                                                                Page  2

When completed, packs will be stacked in the vicinity of F58 disks
drive and identified why they are there.  A list of pack serial
numbers will be provided the COORDINATOR.

26 JUN     SWINGS

Locate the 14 disk packs above and put in front of disk drives as
identified on slap-tags.

27 JUN     MIDS

Start ASP using (OSU)ASP, Version 7 on current running disk packs.

I will be in before ASP dump is completed to assist in pack movement.

Please contact me if any questions.
From: FLETCHERC@X930.Tymnet 
Date: Mon, 22 Jun 87 11:37:46 PDT 
To: carl, jms, osman, dencoff 
Subject: "Please tell me what Tymnet related"... 

Please tell me what Tymnet related courses you have taken to date
so I can determine what courses to enroll you in for the rest
of the year.  Tell = send mail.

	CRaig
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 22 Jun 87 12:10:00 PDT 
To: fletcherc 
Subject: Tymnet Training .. 

I have taken:

o  TTE 102  --  Tymnet Network Operation
o  TTE 330  --  Tymnet Engine Maintenance

-Osman-
Received: from F29.Tymnet by X930.Tymnet; Mon, 22 Jun 87 12:16:25 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Mon, 22 Jun 87 12:03:47 PDT
Return-path: <OPERA.SUP@EMSTXS.Ontyme.Tymnet> 
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 22 JUN 87 11:35:10 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (26 names) 
Message-id: A73484@Ontyme.Tymnet 
Subject: "There is an updated ASPDMP.MEM"... 

There is an updated ASPDMP.MEM under the username FREMONTDOC:74 with 
latest procedures.

It is recommended you obtain a copy and if there are questions or 
changes, please let me know.

Thanks Rick
From: FLETCHERC@X930.Tymnet 
Date: Mon, 22 Jun 87 13:23:41 PDT 
To: osman 
Subject: "Would you resend today's mail?"... 

Would you resend today's mail?  It went by before I could read it.

	Craig
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 22 Jun 87 15:10:28 PDT 
To: fletcherc 
Subject: Tymnet Training .. 

I have taken:

o  TTE 102  --  Tymnet Network Operation
o  TTE 330  --  Tymnet Engine Maintenance

-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 22 Jun 87 15:38:33 PDT 
To: opera.sup@emstxs.ontyme.tymnet 
Subject: Base crashes & FTACFC STOPCD .. 

                             M E M O R A N D U M


                                                      TYMCOM-X SUPPORT



   DATE:  22 June, 1987

     TO:  Rick Daniels

   FROM:  Osman Guven

SUBJECT:  Base crashes and FTACFC STOPCD crashes on /B07

--------------------------------------------------------------------

o  Base crashes occurs at both Data Centers PDP10's.  Frequency of the
   crashes is about the same.  One of the enhancement we have in the
   que about TYMCOM-X and EBUS slot code is to eliminate some of these
   type of crashes: TTYBMT, TTYBPN, CNTPRT, ZAPBUG, BASE etc.

o  Yes, Joe is looking into FTACFC STOPCD crashes.

-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 22 Jun 87 15:40:00 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Base crashes & FTACFC STOPCD .. 

                             M E M O R A N D U M


                                                      TYMCOM-X SUPPORT



   DATE:  22 June, 1987

     TO:  Rick Daniels

   FROM:  Osman Guven

SUBJECT:  Base crashes and FTACFC STOPCD crashes on /B07

--------------------------------------------------------------------

o  Base crashes occurs at both Data Centers PDP10's.  Frequency of the
   crashes is about the same.  One of the enhancement we have in the
   que about TYMCOM-X and EBUS slot code is to eliminate some of these
   type of crashes: TTYBMT, TTYBPN, CNTPRT, ZAPBUG, BASE etc.

o  Yes, Joe is looking into FTACFC STOPCD crashes.

-Osman-
From: Joe Smith <JMS@X930> 
Date: Mon, 22 Jun 87 15:41:29 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Created /B08 instead of /B07-1. 

As per discussion (with JMS, CARL, OSMAN, DENCOFF present), the fix system
38's FTACFC crash will not be distributed as a DDT patch.  Instead, /B08
will replace all copies of nnXB07.SAV.

The crash on F38 showed FTA3 was given a command, FTA1 interrupted while
waiting for FTA3 to finish so FTA1 was given a new command, then the unit
check on FTA3 was noticed.  Due to what appears to be a minor glitch in the
SA10 microcode, the pointer stored at memory location 300+2 implied FTA1
when the unit-check was reported for FTA3 at location 300+1.  This
inconsistancy caused the FTACFC stopcode, which is the same as KAF with
CH1's PC indicating a loop at PI channel 3 at SGPREV in FTASER.  The patch
is to set T4 to a reasonable value and return instead of getting stuck in
a very slow loop searching 256K for the bogus pointer.

File 1)	DSK:FTASER.B07	created: 1859 02-JAN-87
File 2)	DSK:FTASER.MAC	created: 1516 22-JUN-87
1)46		MOVEI	T2,UNICMD+1(T3)	;ADDR OF 1ST READ CMD.
1)		CAIGE	T1,(T2)		;T1 should point past this addr
1)		 STOPCD	(.,JOB,FTACFC,,<FTA error - Cannot Find Command>)
1)	SGPRV2:	SKIPN	(T2)		;SKIP 0
****
2)46					;T1 has pointer from RH of location 302
2)		MOVEI	T2,UNICMD+1(T3)	;Skip set-density, get addr of read cmd
2)		MOVE	T4,T2		;Use top of command list if FTACFC
2)		CAIL	T1,UNICMD(T3)	;Does it point before the command list
2)		CAILE	T1,UNIDON(T3)	; or after the end of the command list?
2)		 STOPCD	(CPOPJ,INFO,FTACFC,,<FTA error - Cannot Find Command>)
2)	SGPRV2:	SKIPN	(T2)		;SKIP 0
**************
1)52	SNEW:	HRRZ	T1,1(U)		;SET UP T4 FOR THE CMD
1)		PUSHJ	P,SGPREV	;GENERATORS TO TIC TO.
****
2)52	IF2,<PRINTX [Should use BMX pointer if async error status]>
2)	SNEW:	HRRZ	T1,1(U)		;SET UP T4 FOR THE CMD
2)	;22-Jun-87 crash dump on F38 shows that SA10 may leave garbage at 1(U)
2)		PUSHJ	P,SGPREV	;GENERATORS TO TIC TO.
**************			/JMS
Received: from F58.Tymnet by X930.Tymnet; Mon, 22 Jun 87 22:09:57 PDT
Return-path: <JMS@F58.Tymnet> 
From: JMS@F58.Tymnet 
Date: Mon, 22 Jun 87 22:07:15 PDT 
To: osman 
Subject: "FTACFC is in It did not show up"... 

FTACFC is in (SYS:58)CRA001.SAV.  It did not show up before because CRSHID
couldn't find the symbol table.  (No one ran MDDT while that particular monitor
was on SYS:, and M02172 got overwritten by a later load.)  This is what CRSHID
reports now:

16 JUN 0703 F58-P035/B06. 3675B$? #1332-KL CRA001, StopPC/FTACFC  M02172
            Key: 75660 CH3/usr+1

21 JUN 2301 F58-P035/B06. 3675B$? #1332-KL CRA002, StopPC/DM30.1  M02172
            Key:  Zero CH1/usr+1 Dep 1 in 30
            Base interface not initialized by host yet.
Received: from F74.Tymnet by X930.Tymnet; Tue, 23 Jun 87 7:51:33 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 23 Jun 87 7:49:23 PDT 
To: OSMAN, DANIELSR 
Subject: "Received mail, or rather OPER"... 

Received mail, or rather OPER did, from user CTOPSMGR (JM LEGOUPIL)
in Paris saying that 1022 will expire very soon.  He
is referring exactly to FTX22, which is on the UPL account.  Says it will
expire on 24 June.

Can you give them some help?  Please let me know?

Thanks Rick
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 23 Jun 87 8:30:02 PDT 
To: DANIELSR@F74.Tymnet 
Cc: DANIELSR 
Subject: Re: "Received mail, or rather OPER"... 
In-reply-to: your message of Tue, 23 Jun 87 7:49:23 PDT

1022 is beeing handled by SRA, Ed Roop's people. I am sure they
will give you an answer. We don,t have anything to do with it.

-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 23 Jun 87 9:56:41 PDT 
To: Fletcherc 
Subject: VAX 8000 .. 

Craig ..

Yes, we do have some information available on VAX 8000 in the
"Microfiche Tank". Here is a list:

o  All of Hardware Manuals

o   Diagnostics ..

    EBDNA -- Ver  1.0 -- KA820 Serial Line Unit Function Test
    EBKAX -- Ver  1.5 -- CPU Cluster Exerciser
    EBKMP -- Ver  1.0 -- Multi-Processor Test
    EBSAA -- Ver 10.4 -- Diagnostic Supervisor
    EBUCA -- Ver  2.2 -- EEPROM Config Util
    ED0AA -- Ver  9.8 -- Consule
    EDCLA -- Ver  1.0 -- SBI Exerciser
    EDK5A -- Ver  8.0 -- Array Microdiagnostic
    EDKAA -- Ver  9.5 -- CPU Kernel Exerciser
    EDKCA -- Ver  5.0 -- MBOX Test #1
    EDKHA -- Ver  7.0 -- MBOX Diag #6
    EDKIA -- Ver  1.1 -- MBOX Diag #7
    EDSAA -- Ver 10.4 -- Diagnostic Supervisor

-Osman-
From: FLETCHERC@X930.Tymnet 
Date: Tue, 23 Jun 87 10:39:47 PDT 
To: osman 
Subject: "What happened to list of Microvax"... 

What happened to list of Microvax II diagnostics supported hardware?
Also could youy tell me what version of diagnostics is the latest?

	Craig

From: JMS@X930.Tymnet 
Date: Tue, 23 Jun 87 11:07:54 PDT 
To: osman 
Subject: "has checksums of the files on"... 

(M33)CKSUM.B08 has checksums of the files on 930.
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 23 Jun 87 11:08:20 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Information .. 

o  /B08 Didn't get to F29 & F33, I finished the copy.

o  Last night, F29 had some KAF crashes while restoring accouting 
   tapes from Ontyme machines just like F28 was a few months ago.  
   Asked Rick to come up with /B08 and use the same Ontyme accounting
   tape which was crashing the system to see if the fix in /B08 will
   take care  of the problem.  He will schedule the system to try it
   today or tomorow.

-Osman-
From: Joe Smith <JMS@X930> 
Date: Tue, 23 Jun 87 11:15:06 PDT 
To: Rick Daniels <OPERA.SUP@EMSTXS.Ontyme> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Summary of recent monitor changes. 

P035/B04 includes the ability to run with AR/ARX parity trap enabled.
         It runs on systems with 365x disks.

P035/B06 fixes the problem of bogus "search errors" on systems with 3675 disks.

P035/B07 would normally be called P035/B06-1.  It fixes the problem of THUNGs
	 on systems with 3 strings on a single 3674 and on 3652 disks.

P035/B08 would normally be called P035/B06-2.  It allows the system to continue
	 after an FTACFC stopcode, which occurs only when unrecoverable read or
	 write errors occur on one FTA while a second FTA is doing I/O.

With the possible exception of system 39, we want to be running /B08 on all
systems in the Fremont Data Center.

			/Joe Smith
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 23 Jun 87 12:40:52 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Group Meeting .. 

Craig and I will be going to a meeting at Fremont D. C. 
tomorrow (6/24 at 10:30).  Our group meeting is rescheduled 
from 10:00 am to 1:00 pm.

-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 23 Jun 87 14:21:27 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: This afternoon .. 

I am going to BLDG C to test the KL Diagnostic Pakc.

-Osman-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 23 Jun 87 20:43:14 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Jon Mosser <MosserJ@X930.Tymnet>,
	Dennis Coffey <dencoff@X930.Tymnet>, William R Soley <WRS@B39.Tymnet>,
	Wayne D Bartlett <BARTLETW@B39.Tymnet>, Baigent, Landa Morris
	<IPC.L/Morris@Ontyme.Tymnet>, Ed Roop <IPC.E/Roop@Ontyme.Tymnet>,
	Ingrid Lopes <IPC.I/Lopes@ontyme.tymnet>, Jan Krivanec
	<IPC.J/Krivanec@Ontyme.Tymnet>, Rick Daniels
	<IPC.R/Daniels@ontyme.tymnet> 
Subject: New SETOP requirements 

In preparation for building a test database on system X930 for the new
SETOP program I am asking each of you to send me mail containing all of
the following information:

   1. Your Name
   2. Your ISG identification number
      (This is NOT necessarily your employee number if you are part of
       MDFSCO or MDCSC.  It is the 6-digit number on your ISG badge,
       such as 212344, most probably not 008834 with leading zeros.)
   3. Your current OPER name
   4. Your current telephone number and mail stop
   5. Your TUMS or ONTYME name (or both) for security mail
   6. Your Username (or list of usernames, if you use more than one)
   7. Your opername manager (who gave you license)
   8. Your immediate supervisor's name, phone number and department
   9. Your current license

  10. Why do you need whatever licenses you have.  Please be specific on
      each and every license listed in item 9.  If you do not know why you
      have a particular license, please say so.  If you have a particular
      need for more license than you have, please say so, and why.

      If you are a manager and require license only because one of your
      subordinates requires the license to do their job, please say so
      and specify the above information for each of your subordinates so
      they may be entered into the database.

Thank you all.
From: Joe Smith <JMS@X930> 
Date: Wed, 24 Jun 87 0:48:28 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Maximum jobs versus disks. 

Our current monitor configuration can handle only 32 jobs when 64 disks 
(8 strings) are defined.  Lowering the number of disks to 48 allows 80 jobs,
and with 32 disks 128 jobs can be handled.

These limits are based on the restriction currently in ONCE that the entire
monitor (including the symbol table) fit in 112K of unpaged core.

/Joe
Received: from F74.Tymnet by X930.Tymnet; Wed, 24 Jun 87 4:54:42 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Wed, 24 Jun 87 4:48:36 PDT 
To: osman, danielsr 
Subject: "Brought F29 up on NEWMON manually"... 

Brought F29 up on NEWMON (P035/B08) manually to rerun HS2287 ONTYME
accounting tape.  This was done to see if the system would cacrash with the same STOPCODE that occurred on 22 Jun.

After bringing the system up, I mounted this tape of FTA2
and started the restore.  I also mounted a tape on FTA1 and started
storing the directory of UTIL to it.

The accounting tape stopped after "Creating User Information" typed
out (same place as before).  The TITO tape stopped shortly after this.
Approximately one minute later, F29 went down with same "KAK" stopcode.

(That's KAF, not KAK)

I then dismounted the tapes and brought the  system up.  I have the
hardcopy if additional information is needed.

Rick
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 24 Jun 87 8:30:11 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: /B08 Testing of KAF .. 

---------------------------------------------------------------------

Message 218 (964 chars): New Read
Received: from F74.Tymnet by X930.Tymnet; Wed, 24 Jun 87 4:54:42 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Wed, 24 Jun 87 4:48:36 PDT 
To: osman, danielsr 
Subject: "Brought F29 up on NEWMON manually"... 

Brought F29 up on NEWMON (P035/B08) manually to rerun HS2287 ONTYME
accounting tape.  This was done to see if the system would cacrash with the same STOPCODE that occurred on 22 Jun.

After bringing the system up, I mounted this tape of FTA2
and started the restore.  I also mounted a tape on FTA1 and started
storing the directory of UTIL to it.

The accounting tape stopped after "Creating User Information" typed
out (same place as before).  The TITO tape stopped shortly after this.
Approximately one minute later, F29 went down with same "KAK" stopcode.

(That's KAF, not KAK)

I then dismounted the tapes and brought the  system up.  I have the
hardcopy if additional information is needed.

Rick


---------------------------------------------------------------------

Thanks Rick.  This is very valuable information.  Please save the tape
that causes the crash.  We will be talking about this in our group meeting
this afternoon and come up with another patch/fix to the monitor to
fix the problem.

-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 24 Jun 87 8:53:26 PDT 
To: Rick Daniels <opera.sup@emstxs.ontyme.tymnet> 
Subject: /B08 testing on F29 .. 


Thanks Rick.  This is very valuable information.  Please save the tape
that causes the crash.  We will be talking about this in our group meeting
this afternoon and come up with another patch/fix to the monitor to
fix the problem.

-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 24 Jun 87 16:49:31 PDT 
To: Rick Daniels <opera.sup@emstxs.ontyme.tymnet>, Monitor Group
	<Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: /B08 Monitor .. 

Rick ..

o  (SYS)NEWMON.SAV on all of the PDP10's except B39 is a P035/B08 monitor.
   Please bring all the systems up with it next time they come up.  Some
   of them already running with /B08 monitor, this is to make all of them
   run with the same monitor.

o  B39, please continue to run it with P034/P-16 monitor untill we find
   out why it wouldn't run the /B06 monitor.

o  FTACFC STOPCD, we are looking into it, will let you know when we come
   up with a solution.

-Osman-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 24 Jun 87 17:23:41 PDT 
To: Joe Smith <JMS@X930.Tymnet> 
Cc: Osman Guven <Osman@X930.Tymnet> 
Subject: License... 

As per our "wall" discussion, I'd like you to think about this:

What if,  license on a file worked like a "protection".
          If a file has more license than the process attempting to
          update or supersede (ENTER UUO) then the "ENTER" fails.
          Rename to reduce the license (or increase the license) would
          always succeed if the process has the final license.

So, just as you cannot delete a file marked NO NO NO, you can rename it
    to a lower protection, and delete it.

So, a file with license can only be copied by a process with that or greater
    license.  It can be renamed, reducing the license, then copied, renamed
    or deleted.

    This also prevents accidently renaming a file, losing it's license, and
    never knowing it....  a current problem, not so much a security risk.

--------------------
So, what about TELECOPY, of files with license?  Trying to copy files to
files on other systems without license would fail (as it should).  TELECOPY
should be fixed as part of implementing such a scheme (as it would need to
be fixed, in order to work correctly).
--------------------

What do we really lose?  The convenience of being able to create a file and
then have to manually put license on it.  If a file has a license and it is
to be superseded, the license goes away (unless it is specified in the ENTER)
(which many programs such as LINK do not know enough to do).

I'm not advocating this change.... I just want us to think about it, and
how something similar could help our "license" problems with new, restored,
updated, copied, stolen programs and/or data.
/Carl
Received: from F33.Tymnet by X930.Tymnet; Wed, 24 Jun 87 21:39:44 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Wed, 24 Jun 87 21:34:57 PDT 
To: osman, opera.sup@ontyme 
Subject: /B08 for system 39. 

I have created B39-P035/B08 after modifying its config file to match that of
system 30.  It has the same number of ports (128 instead of 145), same number
of jobs (108 instead of 127) and the same number of disks (3 strings).
Please try this monitor at your convenience, it is in NEWMON.    /Joe
From: Joe Smith <JMS@X930> 
Date: Wed, 24 Jun 87 21:48:24 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Osman Guven <Osman@X930.Tymnet> 
Subject: Re: License... 
In-reply-to: your message of Wed, 24 Jun 87 17:23:41 PDT

One feature we would like to keep is the current one where if the job has
enough license, and the existing destination file has any license, then a
superseding ENTER preserves license.  That is "!GET (OSU)LOGINN" followed
by "!SSAVE (SYS)LOGINN" preserves the old protection and license.  Is this
what you meant by "license should be like protection"?  I agree that a frame
that has WF but not JL should get an error on ENTER if it were to try to
supersede (SYS)LOGINN.SHR.

/Joe
From: Joe Smith <JMS@X930> 
Date: Wed, 24 Jun 87 22:12:51 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: License disappears (sometimes) on RENAME. 

After reading Carl's message I did some testing.  I GFDed to SPL, and then,
without setting license, I tried ".RENAME COPY.S=COPY.SAV".  The file COPY.SAV
had JL and RF license before and after.  (And still after renaming back to
COPY.SAV.)  Then I tried ".RENAME (JMS)COPY.SAV=(SPL)COPY.SAV".  The license
disappeared when the file changed directories!  After reinstating JL and RF,
I tried renaming it the other direction, from my logged in directory to the
GFDed directory.  License went away.  One last test was to reset everything
and set WF only on my frame and then try renaming across directories.  This
time RF survived (since it is implied by WF) but JL did not.

Until today I did not know that license got left behind on a RENAME.  It is
not what I expected.  However the preservation of license when doing
 "RENAME (SYS)PROGRAM.OLD=(SYS)PROGRAM.SAV" is a feature worth preserving.
/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 25 Jun 87 0:01:03 PDT 
To: Joe Smith <JMS@X930.Tymnet>, Osman Guven <Osman@X930.Tymnet> 
Subject: License again 

Yes, want to preserve:  load/get program; SSAVE program     keeps license
      if you have the same license... although, if you don't have ALL the
      license, I think it ought to fail... you should NOT be able to do
      anything by way of ENTER if you do not have same or greater license.

Rename across directories, if you don't have license, it goes away... as
      expected.  Are you saying that if you HAVE the license, it also goes
      away just because it is across directories?  (It should always go
      away if you don't have it [now] whether or not it is across dir's
      but you should be able to rename license across dir's if you have it!)

My suggestion is to make the enter fail, if you do NOT h
ave enough license
which would cause a "protection"/"license" failure and abort...  so people
can't "accidently" rename or copy over a file that has license.

/Carl
Received: from F33.Tymnet by X930.Tymnet; Thu, 25 Jun 87 4:14:52 PDT
Return-path: <DANIELSR@F33.Tymnet> 
From: DANIELSR@F33.Tymnet 
Date: Thu, 25 Jun 87 4:13:02 PDT 
To: OSMAN, OPER, DANIELSR, MARCOTTE, MIKEBELL, JWALLACE 
Subject: "Osman; I backed off of the b08"... 


Osman;

I backed off of the b08 monitor on system 33.  We were getting more
crashes with it thant with b04-3.

Let me know what you think.

Thanks Rick
Received: from F33.Tymnet by X930.Tymnet; Fri, 26 Jun 87 0:50:27 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Fri, 26 Jun 87 0:44:08 PDT 
To: OSMAN, DANIELSR 
Subject: CRA002.SAV indicates that the SA10 is failing intermitantly. 

The PC in CH1 shows that an SA10 interrupt was detected using by the
CONSO 274,400000 at SA0INT.  However the CONI 274,SA0STS at CRSDEV+3 is 0!
If the SA10 is indeed flaking out, that would explain the SAXINX problem.
From: Joe Smith <JMS@X930> 
Date: Fri, 26 Jun 87 1:14:04 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: 8 of 10 crashes on 33 are SAXINX 

Most of F33's crashes are KAF stopcodes, but the crash dumps show that a
SAXINX info stopcode had just occurred and the info hadn't gotten a chance to
type out on the CTY.  One crash implied that the SA10 stopped working the
time it was determined that the SA10 caused the interrupt and the time the
status was read to determine which channel caused the interrupt.  The
changes to COMMOD in /B09 will get more information and try to continue.
Note: This does not affect F29's problem with reading ONTYME accounting.
That problem is still under investigation.     /Joe
From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Date: Fri, 26 Jun 87 9:20:39 PDT 
To: osman 
Subject: Master dictionary file for SPELL. 

I ran a MHX job, running FINDIT, to look for *.DIC.  I found no (SYS)WORDS.DIC
file on any system.  (I believe that is the master dictionary.)

However, I found a file that looks like a personal copy of the master
dictionary file.  It is (JENGLISH:56) WORDS.DIC, which is 178 pages.  Another
file of possible interest is (CARL:930) HYPHEN.DIC (34 pages), which looks
like a file of possible hyphenization of often used big words.

The list of *.DIC files is (DENCOFF:930) SPLDIC.WHR.  I have a hard-copy, if
you are interested.


-D.-
Received: from F33.Tymnet by X930.Tymnet; Fri, 26 Jun 87 10:12:51 PDT
Return-path: <DANIELSR@F33.Tymnet> 
From: DANIELSR@F33.Tymnet 
Date: Fri, 26 Jun 87 10:12:05 PDT 
To: JMS, OSMAN, DANIELSR 
Subject: "Do you want F33 to come up on"... 


Do you want F33 to come up on NEWMON the next time is crashes
or is recycled?

Thanks Rick
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 26 Jun 87 10:27:23 PDT 
To: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Subject: Re: Master dictionary file for SPELL. 
In-reply-to: your message of Fri, 26 Jun 87 9:20:39 PDT

Thank you Mr. D.
From: Joe Smith <JMS@X930> 
Date: Fri, 26 Jun 87 11:48:44 PDT 
To: DANIELSR@F33.Tymnet 
Cc: OSMAN, DANIELSR 
Subject: Re: "Do you want F33 to come up on"... 
In-reply-to: your message of Fri, 26 Jun 87 10:12:05 PDT

Yes, do bring up 33 on NEWMON.  If the INFO stopcode SAXINX comes out like I
expect then the line with "Contents of T4 ..." will be very important.
/Joe
From: Joe Smith <JMS@X930> 
Date: Fri, 26 Jun 87 11:57:03 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: No meeting. 

The 1pm Friday meeting has been canceled due to insufficient data.
I believe /B09 will help F33's KAF stopcodes due to heavy FTA usage.
However F29's KAF stopcodes from the ONTYME accounting is a different problem,
and needs further study.  I will send another message later today.  /Joe
From: Joe Smith <JMS@X930> 
Date: Fri, 26 Jun 87 17:13:33 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Found loop in FTASER. 

FTASER has several phases of "DC-R" recovery (DataCheck-Read).  Phase 1 is to
re-read the record forwards.  Phase 2 is to read the record backwards.  Phase 3
is to continue reading backwards if phase 2 worked but did not read the entire
record.  The calculation to determine where to resume reading misinterpreted
the command list and got into an infinite loop (causing KAF).  The check for
getting out of the loop failed if the current command list did not start at
the beginning of the area reserved for command lists.  The fix was to stop at
a zero word as well.  
File 1)	DSK:FTASER.B09	created: 1516 22-JUN-87
File 2)	DSK:FTASER.MAC	created: 1524 26-JUN-87

1)47		SOJA	T1,SDMPC2	;SEE IF CONTINUE.
****
2)47		SKIPE	-1(T1)		;Done if no previous command
2)		SOJA	T1,SDMPC2	;SEE IF CONTINUE.
**************

The system does not hang anymore.  However, the OSIRUS RESTORE program tries
to re-read the same record over and over again.  But that's its problem (or
possibly a misfeature in FOROTS).  P035/B10 will be put on F29 tonight, and
eventually all other systems in Fremont.    /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 26 Jun 87 18:40:45 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Rick Daniels <IPC.R/Daniels@Ontyme.Tymnet> 
Subject: Hotline (type) call 

System F30 was experiencing a rise in hard device errors on BPA6.  They
took the system down to do a PAKCOP of the pack (3675's) and saw the same
hardware check problem we had with system 38 two weeks ago.

Rick called and wanted to know if it was OK to use Joe's modified PAKCOP.
I said, go... and we realized that that version was for 3650's in blocks.
We decided to run ASP version 7 to copy out the pack to tape and then do
a restore to a new pack.

We did a compare of the save prior to the restore and found 78(*) pages that
did not copy exactly.  Nothing was done to see how "off" the pages were.
We restored the tapes to a new drive/pack and I created a file with 78 pages
in it for DSKCLN to find multiply used pages and mark the files bad.

System came up with a FULL DSKCLN and reported errors as expected in RECOV.SYS
and on the console.  One UFD was affected.  Operations has the list of files
from FNDBAD to restore and the UFD name to work through the all-files and
backups.

I told them to call us if they had any more problems.

We received a verbal "thanks" from both Bob Fischer and Cheryl Eldred for
saving them a rebuild.

/Carl
Received: from B39.Tymnet by X930.Tymnet; Sun, 28 Jun 87 1:56:16 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 28 Jun 87 1:41:24 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: RDIST program (finally) 

I've finally written RDIST as promised.  It updates files on multiple systems.
I've attached the manual page for it.  Locations are:
  source:  (XEXEC:29)RDIST.SAI
  document:(XEXEC:39)RDIST.PUB
I'll be putting it on XEXEC everywhere soon.  Advise of suggestions.  Please
consider fixing TELECOPY to allow the TALK command in a command file and to
not always update existing file (i.e. supercede).  -Bill
   (XEXEC)RDIST                    Tymcom-X                     (XEXEC)RDIST


   SYNOPSIS

           Remote file distribution.

   SYNTAX

           RDIST outfile=f1,f2,...,fn/USER:u/HOST:h/CHECK/LIST/TRACE/VERBOSE

   DESCRIPTION

           RDIST maintains  duplicate copies of  selected files  on multiple
           systems.   This is  accomplished by  building a  circuit  to each
           system  and running  the (SPL)FINDIT  program to  obtain creation
           date, checksum, protection and license data on the  files.  RDIST
           then  builds a  TELECOPY command  file and  executes  TELECOPY to
           update those files which have incorrect checksums.

           The parameters are as follows:

             outfile=      output file for status log; default is no log

             f1,f2,...,fn  files to be updated, each may contain user name
                           and wild cards; if no user name is given the
                           one given by /USER:u is assumed; default is *.*

             /USER:u       login name to build circuit under (requires AC
                           license); default is current user

             /HOST:h       list of host numbers to update; default is all

             /CHECK        do everything but run TELECOPY

             /LIST         generate list only; does not compare files or
                           run TELECOPY; replaces running FINDIT via MHX

             /TRACE        show circuit traffic on terminal for debugging

             /VERBOSE      show status of each file, even if its current;

   CAVEATS

           The program has no way to correct bad protection or license.












                                 June 28, 1987                             1

From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Date: Sun, 28 Jun 87 18:18:58 PDT 
To: (Monitor Group:), Carl, Osman, Joe <JMS> 
Cc: Craig <FletcherC>, TXSSup 
Subject: Jury duty. 

The jury summons I received 3 weeks ago told me to call this weekend, to learn
if/when I have to report for actual jury duty.  My number, 51, is like such a
number was for the draft lottery:  an early choice.

I have to report to the San Jose Facility, Juror Assembly Annex, at 9:30 am,
Monday, June 26.  I'll let you know as soon as I know more about my schedule.

I hope I don't get a murder trial, like Bruce Ring did!

-D.-
From: JMS@X930.Tymnet 
Date: Mon, 29 Jun 87 9:38:32 PDT 
To: osman 
Subject: "No hotline calls this week for"... 

No hotline calls this week (except for 30 which Carl answered during normal
business hours).  /Joe
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 29 Jun 87 9:40:16 PDT 
To: JMS@X930.Tymnet 
Subject: Re: "No hotline calls this week for"... 
In-reply-to: your message of Mon, 29 Jun 87 9:38:32 PDT

I turned in a timecard for last week w/o any call-ins,
as if I was on call.  Thanks, we are even now.
-Osman-
Received: from F58.Tymnet by X930.Tymnet; Mon, 29 Jun 87 11:50:59 PDT
Return-path: <DANIELSR@F58.Tymnet> 
From: DANIELSR@F58.Tymnet 
Date: Mon, 29 Jun 87 11:47:33 PDT 
To: JMS, OSMAN 
Subject: "Wek keep getting check for device"... 


Wek keep getting 'Address check for device DSK for location 467054; UUO

at exec PC 000150' on system 58.  What does this mean?

Thanks Rick
Received: from F29.Tymnet by X930.Tymnet; Mon, 29 Jun 87 12:01:42 PDT
Received: from EMSTXS.Ontyme.Tymnet by F29.Tymnet; Mon, 29 Jun 87 10:45:56 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 29 JUN 87 10:34:16 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (28 names) 
Message-id: A75677@Ontyme.Tymnet 
Subject: Out Of Office 

                         M E M O R A N D U M





DATE>      29 JUN 87  10:33

TO>        All Concerned

COPIES>    

FROM>      Rick Daniels


SUBJECT>   Out Of Office


-----------------------------------------------------------------------


This is to inform you that I will be unavailable from 3 July
through 10 July. 

Thanks 

Rick Daniels
From: Joe Smith <JMS@X930> 
Date: Mon, 29 Jun 87 12:15:45 PDT 
To: DANIELSR@F58.Tymnet 
Cc: OSMAN 
Subject: Re: "Wek keep getting check for device"... 
In-reply-to: your message of Mon, 29 Jun 87 11:47:33 PDT

ADDRESS CHECK usually means that the SAV or SHR file is corrupted.  If you know
the name of the program, you can see if it has the same checksum on another
system.  If it happens on your terminal, you can type "VERSION" to get the
name of the program.  /Joe
Received: from F74.Tymnet by X930.Tymnet; Mon, 29 Jun 87 15:00:51 PDT
Return-path: <osman@X930.Tymnet> 
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 29 Jun 87 14:59:39 PDT 
To: Rick Daniels <opera.sup@emstxs.ontyme.tymnet>, Monitor Group
	<Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: /B10 Monitor .. 

Rick..

(SYS)NEWMON.SAV on all Fremont D. C. systems are P035/B10 monitor.
Please bring the systems up with it next time they come up.  It has
a fix so that the system won't crash (KAF) when OSIRUS RESTORE is
reading ONTYME accounting tapes.  Thank You.

-Osman-
From: Joe Smith <JMS@X930> 
Date: Mon, 29 Jun 87 21:34:19 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: TUMS distribution not complete. 

System 58 users were complaining that the MAIL command produced "Address Check
for device DSK, UUO at user PC 150".  (MAIL:58)RDMAIL.SAV was corrupted - the
GET command bombed out.  After copying over a new version, the following
discrepencies were noticed:

(MAIL:35)MAILER.SAV wrong protection.
(MAIL:58)MAILER.SAV wrong checksum
(MAIL:58)SENDMA.SAV wrong checksum
(MAIL:58)RDMAIL.SAV was causing "ADDRESS CHECK", I telecopied from 29
(MAIL:62)MAILER.SAV wrong protection, no license
(MAIL:62)RDMAIL.SAV no license

On some systems (XEXEC)*.DOC is protected ALL RD NO instead of ALL RD RD.
From: Joe Smith <JMS@X930> 
Date: Tue, 30 Jun 87 1:17:40 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Ignore message about TUMS distribution. 

Sorry, wrong address.
From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Date: Tue, 30 Jun 87 8:33:17 PDT 
To: Carl, Osman, Joe <JMS> 
Cc: Craig <FletcherC>, TXSSup 
Subject: My jury duty. 

Today will be jury selection by judge and lawyers.  I should know today if I'm
on a jury.  Hope to see you this afternoon.

-D.-
From: Joe Smith <JMS@X930> 
Date: Tue, 30 Jun 87 14:13:52 PDT 
To: CARL, WRS 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: SETOP should clear input buffer if no DRWSPR.EPO file. 

There is a very small window between the time that PJ copies the new oper
password file to SYS and the time that it sets WC license on it.  If an MHX
job just happens to try to use "O$" during that time, SETOP complains that
there is "NO PASSWORD FILE" and exits without clearing the input buffer.
When this happenned to me today, my oper password showed up on the command
line where anyone looking over my shoulder could see.  Because of MEXEC, it
tried to run a program matching the first word of my oper password.  This
word was then made available to SYSTAT for the world to see.

This is not good.
Received: from B39.Tymnet by X930.Tymnet; Tue, 30 Jun 87 17:07:30 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 30 Jun 87 16:58:18 PDT 
To: Joe Smith <JMS@X930> 
Cc: CARL, Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Re: SETOP should clear input buffer if no DRWSPR.EPO file. 
In-reply-to: your message of Tue, 30 Jun 87 14:13:52 PDT

Of course PJ should be fixed to specify the license in the enter rather
than doing a rename after the fact.  Aside from that, OPER should also
always take a password, regardless of weather there is an oper password
file or if its bad or if the name specified does not exist.  This is the
same strategy we'll be releasing in the network as soon as we get a round
tuit.  [it also makes guessing harder]  -Bill
From: Joe Smith <JMS@X930> 
Date: Wed, 1 Jul 87 18:51:32 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Memo to Ed Roop about 32 disks. 

The following is in (M33)32DISK.MEM.  Whoever is going to the meeting in 
Solar Way at 13:00 on 7/02/87 can give copies to Roop, Fischer, etc.
			/Joe

--------------------------------------------------------------------------

From the TYMCOM-X Support Group.
In reference to memo from Ed Roop dated June 4, 1987.

We are currently running a monitor with 32 disk addresses defined.  F74
has 29 active spindles disks, F38 had 28 active disks before it was
converted.  Running with 32 units of model 3675 disks is not a problem.

We have run a test monitor configured for a 3652 system with 32 addresses
and a test monitor configured for a 3652 system with 64 addresses.
Although only 1 string of 3652 disks was connected at the time, we see no
problems with using a second string of 3652 disks due to the uniformity
of disk addressing.  All 32 addresses go through the same 3674 controller.

Due to the current limitation on the size of the resident monitor, a
system can be configured for more than 32 addresses only at the expense
of the number of jobs the system can handle.

        A monitor with 32 disks can be defined with 128 jobs.
        A monitor with 48 disks can be defined with  80 jobs.
        A monitor with 64 disks can be defined with  32 jobs.

Note that each additional string of 3652 disks (with 16 addresses)
reduces the maximum number of jobs by 48.  There is an absolute maximum
of 64 units per structure.  Defining the DSKB structure with more than
32 units is not recommended.  Recovering from an HDA failure on a 48 unit
DSKB would take more than 48 hours of down-time.

Unless the sytems were to use multiple structures, we cannot recommend
attaching more than 32 disk units per CPU.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 1 Jul 87 23:59:07 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Host numbers!!! 

We (finally) received the host numbers we ordered back in May.  We ordered
four 2-digit host numbers for PDP-10 development use.  We received...

    14, 14, 16 and 17 for our use

We also received, in late May, four 4-digit host numbers for our use.
(I don't have these handy, but will save them away for later).

With these 8 numbers, and the ones we already have, we should be able
to configure our micronet with printer devices accessible from the real
network.

/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 2 Jul 87 16:59:21 PDT 
To: Joe Smith <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 
Subject: X930 Hang, reloaded monitor .. 


From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 3 Jul 87 20:24:26 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: X930 All-Files Complete. 

Used set 'E', took 11 tapes.
From: FLETCHERC@X930.Tymnet 
Date: Tue, 7 Jul 87 11:59:02 PDT 
To: osman 
Subject: "Ray Donahue called me today with"... 

Ray Donahue called me today with an advance order for a TDS pack.

Mark Milano should soon be ordering a TDSKLD6-01 for 2060 AT
Columbia University.  How are you fixed for preparing another
KL diagnostic pack?

	Craig
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 7 Jul 87 13:00:24 PDT 
To: FLETCHERC@X930.Tymnet 
Subject: Re: "Ray Donahue called me today with"... 
In-reply-to: your message of Tue, 7 Jul 87 11:59:02 PDT

No problem. If it is really urgent I could make it this Friday(7/10).
Next week would be better due to Friday's schedule.
-Osman-
From: FLETCHERC@X930.Tymnet 
Date: Tue, 7 Jul 87 13:47:11 PDT 
To: osman 
Subject: "Forget to mention in first mail"... 

Forget to mention in first mail that 2060 contract begins August 15th.
So it is not urgent, but they might like it ahead of time.

	Craig
Received: from F38.Tymnet by X930.Tymnet; Thu, 9 Jul 87 19:10:49 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Thu, 9 Jul 87 19:09:44 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: EQUIP CHECK on F38's 3652. 



From: JMS
Date: JULY 9,1987 18:57
The THUNGs on BPA12 and BPB6 are a side affect of the HDEV:4 on BPA13.
I tried to read CYL:1 HEAD:22 RECORD:23 4 times to see if it would cause
an EVENT stopcode BADPAG on unit 12 page 511.  It did not.  However, it
took more than 3 seconds for the monitor to do all the retries on that page,
which caused the THUNGs.  BPA12 and BPB6 were not hung - the monitor was
ignoring them while doing the retries.  The best way to clear this disk
error is to ASSIGN and DEASSIGN an alternate track to CYL:1 HEAD:22.
Due to the way SPAGES is used, no data will be lost.   /Joe
The THUNGs on BPA12 and BPB6 are a side affect of the HDEV:4 on BPA13.
I tried to read CYL:1 HEAD:22 RECORD:23 4 times to see if it would cause
an EVENT stopcode BADPAG on unit 12 page 511.  It did not.  However, it
took more than 3 seconds for the monitor to do all the retries on that page,
which caused the THUNGs.  BPA12 and BPB6 were not hung - the monitor was
ignoring them while doing the retries.  The best way to clear this disk
error is to ASSIGN and DEASSIGN an alternate track to CYL:1 HEAD:22.
Due to the way SPAGES is used, no data will be lost.   /Joe
    @Fi