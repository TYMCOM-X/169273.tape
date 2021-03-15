From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 21 Aug 85 5:19:27 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Still here... etc! 

Since I'm still here & it's 5:15 already... I will probably be in
quite late tomorrow.  Feel free to call me (after NOON) if you need
anything, [Before NOON if it's an emergency, but I won't be very awake].

See STOPCD.MEM for my "minor" changes.  I haven't decided whether to
remove the module name or not (yet).  The asterisks under the STOPCD
names mean that we've seen this one.  The number of *'s is indicitive
of how often: * for once, *** for a few, ****** for lots...

these are the ones that the descriptions must be the best we can do
for the moment.  LATER, we can put in all the rest....
/Carl

FCR.E+4 alias SIMIOX is the frontrunning one to fix... and oddly
enough, we've made it a JOB stopcd which means only the offending
job will die... the system will continue running.  [What a way to
fix a bug from killing the system?]
From: FLETCHERC@X930.Tymnet 
Date: Wed, 21 Aug 85 11:46:36 PDT 
To: carl 
Subject: future dates 

I agree with you that UUOs should not be able to set a file date
in the future and that the monitor should be changed.

	Craig
Received: from C39.Tymnet by X930.Tymnet; Wed, 21 Aug 85 12:36:34 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 21 Aug 85 12:22:57 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Future dates! 
In-reply-to: your message of Tue, 20 Aug 85 19:55:00 PDT

I agree.  They should either all check, or none check.  -Bill
Received: from C39.Tymnet by X930.Tymnet; Sun, 25 Aug 85 13:48:29 PDT
Return-path: <LAGOLD@C39.Tymnet> 
From: LAGOLD@C39.Tymnet 
Date: Sun, 25 Aug 85 13:47:30 PDT 
To: Sally, JMS, Carl 
Subject: Great America today 

Hi.  It's almost 2pm; where are y'all?

--Lynn and Mark
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 29 Aug 85 1:03:33 UT
From: NTD.D/CHANG@Ontyme.Tymnet 
Date: 28 AUG 85 17:47:06 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: NSMS.D/SHINDELL@Ontyme.Tymnet, MIS.F/CANNING@Ontyme.Tymnet,
	TYMOPS.S/POPPE@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	CPC.N/MARSH@Ontyme.Tymnet, TYMOPS.R/TOMASI@Ontyme.Tymnet,
	NSS.M/CADY@Ontyme.Tymnet, NSS.G/WALKER@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.RAJA@Ontyme.Tymnet, NTD.K/HOLCOMB@Ontyme.Tymnet,
	NTD.J/HAINAUT@Ontyme.Tymnet, NTD.D/ALCORN@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.D/JONAS@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.J/KOPF@Ontyme.Tymnet,
	NTD.B/SOLEY@Ontyme.Tymnet, NTD.D/ERNST@Ontyme.Tymnet,
	NTD.N/BURRIESCI@Ontyme.Tymnet, NTD.J/NEIDRAUER@Ontyme.Tymnet,
	NTD.R/RAFFO@Ontyme.Tymnet, NTD.G/MATOKA@Ontyme.Tymnet,
	NTD.W/WANG@Ontyme.Tymnet, NTD.O/SERANG@Ontyme.Tymnet,
	NTD.R/TENG@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet, TXS.@Ontyme.Tymnet
	
Message-id: I37722@Ontyme.Tymnet 
Subject: 14-bit Node Numbers and Deletion of TYMNET-I in PUBNET 

TO:  Dave Shindell 
FR:  David Chang
RE:  14-bit Node Numbers and Deletion of TYMNET-I in PUBNET
DA:  August 28, 1985

=====================================================================

This memo is to summarize all the action items concerning the deployment
of the 14-bit node numbers and deletion of TYMNET-I nodes in the public
network.  In conjunction with these is the creation of a 940 network
(NET940).  Two PIRs are associated with these projects:  #1987 (14-bit
node number) and #2006 (NET940).  It is planned that testing will
commence in September, betatesting in October, and deployment in
November.  Coordination of the following action items is absolutely
essential in the successful completion of these projects.

1. NTD (Supervisor Unit) will make modifications in the Supervisor code
in order to accomodate the 14-bit node numbers and the deletion of
TYMNET-I nodes.  

2. NTD (Greg Matoka and John Kopf) will make modifications in the
Supervisor code and Gateway slot in order to accomodate the creation of
the 940 network.  Changes are being made to pass the originating node
number from PUBNET to NET940 for billing purposes.  This means that the
creation of NET940 will be transparent to the 940 billing.
Unfortuately, this billing is unable to handle 14-bit node numbers, so
the Supervisor of NET940 will insure that all logins must be from a
12-bit node number.  (For details, see NET940.DO and NET940.GED)

3. Steve Poppe will reserve about 50 (or whatever is appropriate) 12-bit
node numbers at the upper end of the range for special purposes, such
as a new node for a 940 user.

4. NTD (Romolo Raffo and Weyyi Wang) will make modifications to
accomodate 14-bit node numbers in the communications between the
Supervisor and (RAM and TMCS).  These affect the TYPE 2 message from the
Accounting slave, two commands from TMCS, and the response to the STAT
command in the Error Log.  The details have been worked out.

5. Carl Baltrunas will provide a mechanism for automatic self-initiation of 
PDP-10s in order to eliminate the need of phantom jobs originating from the
Supervisor.  The eradication of the phantom jobs will be part of the
deletion of TYMNET-I code in the Supervisor (this deletion is
accomplished by an assembly switch, thus will not affect existing and
new TYMNET-I private networks, eg. NET940).

6. Jill Neidrauer has assured me the validation programs can handle the
14-bit node numbers, especially in the data structures like the MUD and
Class/Group Tables.

7. Fred Canning is extremely concerned about the handling of the billing
with 14-bit node numbers.  He has indicated most, if not all, customers
expect 4-digit node numbers in the accounting tapes.  For the TYMSHARE
users, he will make modifications to handle 4-digit decimal node numbers
(currently, it is 4-digit octal) because it is much more difficult to go
to the 5-digit number.  Hopefully, this will suffice until the
ACCOUNTING-II project is developed.

8. Dave Shindell will coordinate with various users of the
implementation of the 14-bit node numbers.  It is imperative that
customers can accomodate the larger node numbers.  Furthermore, it is
important that a meeting is held to determine the display of node
numbers in PROBE, TMCS, X-RAY, etc.  The various considerations are
"octal, decimal, hexadecimal" and "new only or both old and new" node
numbers.  Such a determination should be conveyed to NTD as soon as
possible.

9.  Nikki Marsh has assured me that the 370s can handle the 14-bit node
numbers.

I can handle the coordination of the technical modifications that must
be done in order to effect the larger node numbers, but it would be most
helpful if you could coordinate the necessary "thing to do" among the
external customers, as well as PUBNET operations.  Please utilize the
above PIR numbers, unless you wish me to write up another PIR. 

If anyone in the copy list knows of anything that should be done and
that is not on the above list, please let me know.  Thank you.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 29 Aug 85 20:27:26 PDT 
To: Dennis Coffey <dencoff@C26.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Jon Mosser <mosserj@X930.Tymnet>,
	Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: 14-bit node numbers 

Yes, I said 14-bit node numbers.  TYMNET NTD is looking at expanding the
current 12-bit numbers since they are currently at '7200+ and will soon
overflow the 12-bit field.

Gazel Tan has asked me to help identify potential software problems on
the PDP-10s if they were to go to 14-bit node numbers.  In the monitor,
the problem is minimal, we have a few extra bits and could easily make
room for up to 16-bit fields.

Various utility programs would falter badly, not to mention SPOOL, which
has relied on 11-bit node numbers and is already needing a fix.

I'd like you to examine the PDP-10 utilities and possibly applications
which do anything with node numbers and make a list: programs which key
off the node (eg. SPOOL,MAGNUM) and programs which simply report what is
read (eg. SYSTAT).

Once we have the list, we can determine what impact changing the node
numbers to 14-bits will have on our environment.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 29 Aug 85 20:42:34 PDT 
To: ntd.d/chang@ontyme, ntd.g/tan@ontyme 
Subject: 14-bit problems... 

re: Royalty accounting products...
   just a side comment :: MAGNUM on the PDP-10s uses this mechanism and
                          and there are only 5 (five) people left in the
                          entire MAGNUM group.  Contact Frank Siegel to
                          let him know of this impending change!
  /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 1 Sep 85 1:22:52 PDT 
To: jms, Dennis Coffey <dencoff@C26.Tymnet> 
Cc: Osman Guven <osman@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: 12-bit node numbers 

Sigh... SYSTAT only reads up to node 3000!   That's why we see *6657* in
the systat listings.  FYI there are two places in SYSTAT that do a
  MOVEI M,3000   instruction then later CAMGE M,xxxxx to check the node
number.  I binary-patched (SYS:930)SYSTAT.SHR to  MOVEI M,7777  to test
it and "le voila" it now prints node names higher than 3000!

Oh, and since TYMNET may or may not have updated things... I wrote a
little program, DEFNOD which is in (TYMNET) that reads NODES.MAP and
does nice things to it.

It's basically DEFSYS/DEFPRI/DEFNOD hacked to work on the right file.
DEFSYS looks at (SYS)PERP.SYS,  DEFPRI looks at (SPOOL)NODPRI and now
DEFNOD looks at (TYMNET)NODES.MAP....

sigh... gee.. it's easy to write/modify programs which read binary files
and print out the entries, and modifies them... real easy... sigh...

Anyway... nodes should type out on 930 a little more sensibly now...
However, don't bother trying to transmit SYSTAT or anything like that
until we get the new 14-bit node standard implemented....

I've talked to Dave Smith, and since he was asked to co-ordinate with
me how to do the new standard, and the base gets it as a 16-bit word,
chances are that we will go to 2 8-bit bytes as I proposed the other
day.   (however, NOTHING will be done until the new bases are completely
deployed in the network and the varians are all gone!!!)

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 1 Sep 85 16:20:26 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Dennis Coffey <dencoff@C26.Tymnet>, Osman Guven <osman@X930.Tymnet>, Craig
	Fletcher <fletcherc@X930.Tymnet> 
Subject: Re: 12-bit node numbers 
In-reply-to: your message of Sun, 1 Sep 85 1:22:52 PDT

When the monitor changes to 16-bit node numbers, there should be a new bit in
the feature-test GETTAB table.  If the bit is off (or the GETTAB fails), then
LDBLOG has 12 bit numbers.  If the bit is on, then 16-bit numbers are in use.
I mention this now so that the programs can be modified before the change.

/JOE
Received: from C26.Tymnet by X930.Tymnet; Tue, 3 Sep 85 9:54:20 PDT
Return-path: <DENCOFF@26.Tymnet> 
From: Dennis Coffey <DENCOFF@26.Tymnet> 
Date: Tue, 3 Sep 85 9:45:35 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: 12-bit node numbers 
In-reply-to: your message of Sun, 1 Sep 85 1:22:52 PDT

I assume your message was informational only, and not a request for
action on my part.  I appreciate the info about DEFNOD -- I don't
remember your having mentioned that one previously.

I thank you for DEFPRI and DEFNOD -- they should greatly help us keep
the SPOOL node map current, with ease.

/D.
From: KEN@X930.Tymnet 
Date: Tue, 3 Sep 85 11:33:57 PDT 
To: carl 
Subject: dragon book 

    Carl,

    I have my copy here at work.  I looked, and it seems that the length of
    the stuff being reduced on the stack is known because the production's
    identity is somehow encoded in the state identity.  I think that in the
    case of states with more than one reduction (involving different
    productions), some arbitrary rule is used to determine which gets
    selected.

    What I can't seem to determine without gobs of reading is what the GOTO
    of a reduce state is.  It needs further study, but if you're in the
    neighborhood, you can take a look.

    /ken
From: FLETCHERC@X930.Tymnet 
Date: Wed, 11 Sep 85 15:43:38 PDT 
To: carl, jms, osman, baigent, helgem, dencoff 
Subject: 930 backups 


Since we are now without a COOP student to do the backups on system
930 we must have an alternative mechanism. After discussing with each
of you some suggested ways of handling the backups I am proposing
the following procedure in the interests of simplicity and reliability.
Each person in the list will have one day of the week assigned to
do incremental backups- this assignment will last "forever" and should
be easy to rememeber.  The monthly ALL FILES backup will rotate
across all memeb
across all members.

The winning assignments are:

Monday - Osman
Tuesday - Dan
Wednesday - Carl
Thursday - Joe
Friday - Bill

Monthly ALL FILES:

September - Dennis
October - Bill
November - Joe
December - Dan
January - Carl
February - Osman

See Osman if you have any questions about the procedure to follow.
Please do the incrementals at a regular time each day, say between
4 p.m. and 6 p.m.

If there are any problems with the assignments see me and we will
consider adjustments

		Craig
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 14 Sep 85 19:59:56 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: OFFICE-1 

OFFICE1 is no longer a valid name on the network.  Are you still valid on
OFFICE1 as CARL:1088?  What is the password?
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 13 Sep 85 5:04:06 UT
From: HRD.TOSH@Ontyme.Tymnet 
Date: 13 SEP 85 03:43:13 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A14599@Ontyme.Tymnet 
Subject: VACATION SCHEDULE 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Mon, 16 Sep 85 14:38:48 PDT
Resent-To: Carl A Baltrunas <carl@X930.Tymnet>

                   HUMAN RESOURCES DIVISION - MEMORANDUM
                   *************************************


<M D C - I S G>

DATE>      12 SEP 85  20:42

TO>        ALL MANAGMENT

COPIES>    

FROM>      DAN TOSH


SUBJECT>   VACATION SCHEDULE


-----------------------------------------------------------------------


              VACATION SCHEDULING AND PAY ADVANCES
 
 
 
Effective September 30, l985, with the implementation of the ISG 
benefit package, earned vacation records for all ISG employees 
will be maintained in the MDC Payroll Personnel System (PPS II).
Once this is accomplished, advance pay for pre-scheduled vacations 
will be issued out of PPS II.
 
Requests for advance vacation pay will be as follows:
 
 1)  Vacation schedules are to be submitted via OnTyme message to 
     MDCHQ.PERSONNEL at least 2 weeks prior to the start of the
     vacation.
 
 2)  Requests should be made in the following format:
 
     Employee number, employee name (last, int.), first day of 
     vacation, number of vacation days
  
     Example:
 
     E.N.          NAME                   SCHEDULE 
 
     123456        Jones, J.          11/28/85 for 10 days
                                                                Page  2

 
 3)  For questions regarding vacation schedules and advances, call
     (314) 233-2974.
 
 4)  As a backup to the above OnTyme procedure, vacation schedules 
     can be called in to ISG Human Resources at (314) 232-7490.
 
 5)  It will be the responsibility of the office managers to 
     satisfy internal approvals before the schedule is transmitted 
     to PPS II for input.  This can be accomplished through the 
     use of MDC form #4006-2 which, when approved, would be 
     retained by department supervision.
 
 6)  Cancellations will be processed in the same manner.  Include 
     the same information along with the message "cancel 
     vacation".
 
 7)  Vacation advances will be made in 5 day increments only.
     NOTE: No vacation advance will be issued for schedules
           which include unearned and therefore borrowed days
           under the "Accelerated Vacation Policy". Vacations
           consisting partly or totaly of borrowed days will 
           not be pre-scheduled as outlined above rather they
           are to be reported throught the payroll attendance 
           system at the time the vacation is taken.
 
 8)  Vacation schedules for less than 5 days will not be advanced.  
     No schedule is to be reported to PPS II.  These days will be 
     reported through the weekly attendance collection system.
 
 9)  Vacation advance checks will be issued the Friday prior to 
     the first day of vacation.
 
10)  This procedure will become effective with vacations beginning
     on or after 10-14-85.  Approved vacation schedules for that 
     date or later should be submitted at your earliest       
     convenience to insure advance pay.
 
11)  Direct your questions to the writer via OnTyme HRD.TOSH.
Received: from C39.Tymnet by X930.Tymnet; Tue, 24 Sep 85 19:00:59 PDT
Return-path: <CARL@C39.Tymnet> 
From: CARL@C39.Tymnet 
Date: Tue, 24 Sep 85 18:58:35 PDT 
To: gazel 
Subject: 940-net, 14bit node #s 

Hi,
  Just thought I'd drop you a line... didn't you say something about
sending me copies of whatever specs/memos there were about these two
things... especially 940-net, so that I ould go over them to see if
there was anything that might cause problems on the PDP-10s (in addition
to the known PERP problems).

? /Carl
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 25 Sep 85 17:12:56 UT
From: IPC.JENNY@Ontyme.Tymnet 
Date: 25 SEP 85 08:26:33 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: IPC.L/CORBIN@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A23876@Ontyme.Tymnet 
Subject: D54 Software/Montior Group User 

                         M E M O R A N D U M





DATE>      25 SEP 85  10:24

TO>        Carl Baltrunas
           Lee Corbin

COPIES>    

FROM>      Jenny Guerrero
           Resource Planning and Management


SUBJECT>   D54 Software/Montior Group Usernames


-----------------------------------------------------------------------



System D54 will no longer be a Tymshare system come mid-October at this    
time, the system will become a Tymnet Public Net system.  Could you look   
over the following users at determine which names can be removed from 
system D54.  I will be removing usernames on October 17, 1985.   
     
The usernames are:  
CID 1 - GAN 11447   
GBLOOD    
SSPAMBIN *
     
CID 2 - GAN 2  
ACT10      ACTRTY * 
DMOULTON   UAS *    
SPOOLRM * 
     
CID 4 - GAN 3  
*2TRR
     
CID 16 - GAN 3 
                                                                Page  2

FTSYS *    CARL *      CHERIE      FLETCHERC   IMPORT  
HELGEM     JMS         KEN         M33         MAIL *  
MATOKA     MCH         MPL *       NDT         OSMAN * 
OSNG       OSU         PUB *       SAILIB *    SAILTEST *   
SALLY      SPL         SPPARCH *   SPPOPER *   SYSMAINT *   
TBAUSPS    TSUCOOP     TYMGRIPE    XEXEC *     ZONNIE  
SSPERP     SSPRINT  
     
CID 94 - GAN 3 
SALTYRON  
SSDEV
SSUTIL    
     
CID 122 - GAN 3
SYSNEW *   *6NEWS *    ARCHIVES    FTTBA       QALIBSYM
QASYS *    SSARCHB     SSARCHD     SSBACKUP *  SSINSTALL *  
TBATLIB * 
     
CID 377 - GAN 3
FTSYSDOC  
SYSDOC    
     
CID 14144 - GAN 146 
SSVALSUP  
XFINDEV   
XFINTEST  
     
   * - These names, I believe, need to remain on the system 
     
If there are any questions, please call me at (214)637-7324 or ontyme 
IPC.JENNY 
     
Thank you 
Jenny Guerrero 
Resource Control
Received: from C39.Tymnet by X930.Tymnet; Wed, 25 Sep 85 14:45:16 PDT
Return-path: <GAZEL@C39.Tymnet> 
From: GAZEL@C39.Tymnet 
Date: Wed, 25 Sep 85 14:43:08 PDT 
To: carl 
Subject: 940-net, etc... 

Carl,

The DO and GED for 940-net are in (dndspec)net940.do and net940.ged.
(on system 39). Feel free to pull copies.  The memo on 14-bit node #s
is not on-line - where should I send a copy?

Sorry about the delay,
Gazel
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 25 Sep 85 14:59:33 PDT 
To: gazel 
Subject: Mail - Interoffice 

Gazel,
  I'd guess that regular interoffice mail sent to:

	Carl A. Baltrunas
	39100 Liberty Street
	Fremont, CA

should be fine... for any paper documents...
Talk to you later...

/Carl
From: FLETCHERC@X930.Tymnet 
Date: Thu, 26 Sep 85 11:48:43 PDT 
To: carl 
Subject: 940net meeting 

Will you attend a meeting on Wednesday, October 2, at 1:00 p.m. in the
conference room at 10231 Bubb Rd. (bldg. C) concerning 940net.

Craig

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 27 Sep 85 1:40:24 PDT 
To: IPC.JENNY@Ontyme.Tymnet 
Cc: IPC.L/CORBIN@Ontyme.Tymnet, TXS.SUP@Ontyme.Tymnet, dencoff 
Subject: Re: D54 Software/Montior Group Usernames 
In-reply-to: A23876@Ontyme.Tymnet of 25 SEP 85 08:26:33

Jenny,
  As to my understanding for how the TYMCOM-X and Software-Distribution
groups provide support to any PDP-10 running TYMCOM-X, I find it difficult
to see any difference between a "Tymshare" and a "Tymnet Public Net" host.
In that regard, I would ask that any GAN 3 accounts be left "as is" on the
new system 54 username list.

  In particular, there are 2 or three lists of GAN 3 accounts which are
critical for system operation, crucial for maintaining tools, crucial for
maintaining TYMCOM-X operations and crucial for software distribution.
It also sems that some of the usernames on your list were really in GAN
11447 instead of GAN 3...

  Anyway I have no complaints if you remove the following accounts:

    GBLOOD, DMOULTON, *2TRR, XFINDEV, XFINTEST

  If Lee Corbin has no complaints, then you may also remove:

    SSARCHB, SSARCHD

  All the rest (in this list) do have some function, albeit the usernames
are really only needed for support and some should contain no files.

- needed for system operation-
    SYS, FTSYS, IMPORT, MAIL, XEXEC, TYMGRIPE, M33, CRASH
- needed for operating system tool support libraries -
    M33, OSU, OSNF, MPL, SPL, XEXEC
- needed for system language support subsystems -
    PUB, SAILIB, SAILTEST, NDT, TBAUSPS, ZONNIE, TBATLIB, FTTBA,
    SYSNEWS, *6NEWS, SYSDOC, FTSYSDOC, QASYS
- needed for spool/batch/perp support subsystems -
    SPPOPER, SPPARCH, SYSMAINT, SSPRINT, SSPERP, SPOOLRM, SSPAMBIN
- needed for system support & software distribution -
    CARL, OSMAN, JMS, SYSMAINT, MPL, SPL, XEXEC, 
    SSINSTALL, SSBACKUP, QASYS, QALIBSYM
- needed for system tools support & backup support -
    FLETCHERC, MATOKA, KEN, MCH, ZONNIE, CHERIE, SALLY, HELGEM,
    DENCOFF, TSUCOOP
- needed for royalty accounting -
    ACTRTY, UAS, ACT10, BILLING10, UN1
- needed for validations programs -
    SSVALSUP, CUD10


Note to Lee:  We need to get together sometime to make up a definitive
	      list of usernames and who is responsible for them and what
	      they are for, as far as a list of system-critical users.
	      This list is needed so that we can create a LUD.SYS with
	      all the critical users, and whenever Jenny (or anyone else
	      in resources) brings up a "clean" system, we can put it
	      out there without all this "fuss" about which usernames to
	      "keep" on the new system.

Well, Jenny... the above list is by no means COMPLETE.. I merely took the
list you sent me and re-arranged all the ones I knew something about and
added a few to make sure they did NOT get removed (if they are even still
there on system 54).  If you have any more questions about any of the
names on this list Please ONTYME me @ TXS.SUP and TXS.C/Baltrunas

Also, please ONTYME me "BEFORE" you delete "ANY" GAN 3 or cost code 4011
usernames.  We have some critical sources out on some systems even though
the user who maintained those sources is no longer with the company.  As
we consolidate the sources, we have been removing the usernames... we do
not however, want them to just "disappear" in any of the global sweeps that
have been taking place within the last year.  We've already "lost" one or
two sets of compiler & runtime sources.  Thanks an awful lot.

/Carl
From: FLETCHERC@X930.Tymnet 
Date: Mon, 30 Sep 85 8:52:02 PDT 
To: osman, carl, baigent, dencoff, helgem 
Subject: vacation requests 

If you havent't already done so, please fill out an official paper request
for the vacation scheduled between now and the end of the year.

	Craig
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 1 Oct 85 23:00:53 PDT 
To: ken 
Subject: Tomorrow... 

I'll be over that way... I have a 1:00 mtg in Bldg C about 940NET...
no idea how long it will last...  may be at kirks, 11:5ish if I can
wake up early enough (depends on how late I stay up working now)...
Anyway, if I can, I'll try to come by (assuming I don't make it to
Kirks, and/or you don't have time for lunch...)  If I awake in time
for Kirks, I'll send mail or leave a msg letting you know for sure.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 2 Oct 85 11:37:51 PDT 
To: ken 
Subject: Today... 

Well,
  I slept through the alarm (radio wasn't loud enough) and didn't make
it in time for lunch before my 1:00 meeting.  However, I will still stop
by after the meeting... see you then.

Ciao!!  /Carl
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 4 Oct 85 1:05:46 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 30 SEP 85 08:55:00 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J43553@Ontyme.Tymnet 
Subject: REDUCTION IN FORCE 

SEPTEMBER 20, 1985  
     
     
TO:      ALL FIELD OPERATIONS LOCATIONS 
     
FROM:    CURT MILLER, VP-FIELD OPERATIONS    
     
SUBJECT: REDUCTION IN FORCE   
     
PLEASE SEE THAT THE FOLLOWING LETTER FROM VERN HART, PRES-FIELD  
OPERATIONS, IS POSTED AND/OR DISTRIBUTED:    
     
     
DEAR FIELD SERVICE COMPANY EMPLOYEE:    
     
WHEN YOU WORK FOR A COMPANY, PROBABLY ONE OF THE MOST DISTURBING 
THINGS THAT CAN HAPPEN IS A REDUCTION IN FORCE.  THE LOSS OF A JOB, OR
THE THREAT OF IT, IS A VERY SERIOUS MATTER TO EVERYONE.
     
MANY OF YOU HAVE MADE USE OF "OPEN LINE" TO EXPRESS YOUR CONCERNS AND 
TO ASK QUESTIONS.  I WISH TO THANK ALL THOSE WHO HAVE DONE SO AND TO  
EXPRESS MY APPRECIATION FOR SHARING YOUR FEELINGS.  EACH QUESTION OR  
CONCERN WILL BE ANSWERED INDIVIDUALLY AND, IN ADDITION, THE RESPONSES 
WILL BE SUMMARIZED IN COMPANY PUBLICATIONS FOR THE INFORMATION OF ALL.
     
SINCE THIS INFORMATION WILL BE PUBLISHED IN THE NEAR FUTURE, I WILL   
NOT, IN THIS LETTER, ADDRESS ALL OF THE QUESTIONS I HAVE RECEIVED
EITHER THROUGH "OPEN LINE" OR IN PERSONAL CONVERSATIONS.  MY PURPOSE  
HERE IS TO SKETCH OUT FOR YOU THE CIRCUMSTANCES WHICH RESULTED IN THE 
NEED FOR A REDUCTION IN FORCE AND THEN TO TOUCH BRIEFLY ON WHERE WE ARE    
GOING AND SOME OF THE THINGS WE ARE DOING TO GET THERE.
     
THE PRESENT BUSINESS SLUMP IN THE COMPUTER INDUSTRY IS CERTAINLY NOT  
NEWS TO ANYONE.  IT BEGAN TO DEVLOP LATE LAST YEAR AND HAD GROWN TO   
SERIOUS PROPORTIONS BY THE SECOND QUARTER.  ALL COMPUTER MANUFACTURERS
HAVE EXPERIENCED ITS EFFECTS, INCLUDING IBM, DEC, ATT-IS, HEWLETT
PACKARD, DATA GENERAL, AND WANG.  ALL HAVE HAD TO ADJUST THEIR 1985 PLANS  
IN ONE WAY OR ANOTHER.  MANY HAVE TAKEN COST REDUCTION ACTIONS RANGING
FROM REDUCED WORK WEEKS AND PERIODIC SHUT DOWNS TO REDUCED WAGES,
REDUCTIONS IN FORCE, AND PLANT CLOSINGS.
     
A KEY PART OF THE MCDONNELL DOUGLAS ISG STRATEGY HAS BEEN TO DIVERSIFY
ITS OPERATIONS BY ESTABLISHING ELEVEN BUSINESS UNITS WHICH, BY WORKING
TOGETHER, WILL ADDESS A MULTIPLICITY OF MARKETS.  IN A NORMAL YEAR, THE    
EXPECTATION IS THAT A SMALL NUMBER OF UNITS MIGHT HAVE A RELATIVELY   
WEAK PERFORMANCE BUT THAT THIS WOULD BE OFFSET BY STRONG PERFORMANCES OF   
THE OTHERS.  HOWEVER, WHEN THE OVERALL RESULTS FOR ISG IN THE SECOND  
QUARTER OF THIS YEAR WERE EXAMINED IN JULY, THE REVERSE SITUATION WAS 
FOUND.  WHEREAS A FEW UNITS WERE DOING WELL, THE MAJORITY WERE FALLING
SHORT OF PLAN AND THE OUTLOOK WAS NOT GOOD.  
     
AS A RESULT, EACH BUSINESS UNIT, INCLUDING THE FIELD SERVICE COMPANY, 
WAS GIVEN A PERFORMANCE IMPROVEMENT TARGET AND ASKED TO DEVELOP PLANS 
TO MAKE IT HAPPEN.  
     
EVEN THOUGH THE FIELD SERVICE COMPANY WAS ON PLAN THROUGH THE END OF  
JUNE, IT WAS APPARENT THAT OUR PLANNED REVENUE GOALS FOR THE LAST HALF
OF THE YEAR, WOULD FALL WELL SHORT OF PLAN.  WE CLEARLY COULD NOT AFFORD   
TO SPEND ON THE CURRENT RATE, AND THEREFORE, THE ELIMINATION OR  
SIGNIFICANT CURTAILMENT OF ACTIVITIES AND FUNCTIONS WHICH WERE NOT    
ABSOLUTELY ESSENTIAL TO OUR BUSINESS, BECOME NECESSARY.  WE ALSO FOUND
THAT NORMAL BELT TIGHTENING WOULD NOT PRODUCE THE REQUIRED RESULTS TO 
MEET OUR NEW GOALS AND THEREFORE, A REDUCTION IN FORCE BECAME NECESSARY.   
     
IT IS MY VIEW THAT THE ACTIONS TAKEN, THOUGH PAINFUL, WERE A NECESSARY
CORRECTION UNDER THE CIRCUMSTANCES AND WILL KEEP US ON COURSE TOWARDS 
OUR LONG-TERM OBJECTIVE OF BECOMING THE SUPPLIER OF CHOICE TO THE OTHER    
ISG BUSINESS UNITS. 
     
MANY PEOPLE WHO WERE NOT AFFECTED BY THE REDUCTION IN FORCE, HAVE, QUITE   
NATURALLY, EXPRESSED CONCERN FOR THOSE WHO WERE AND FOR THEIR OWN FUTURE   
WITH THE COMPANY.  LET ME SAY THAT I KNOW OF NO ONE IN MANAGEMENT WHO 
WILL CONTEMPLATE A REDUCTION IN FORCE WITH ANYTHING BUT THE GREATEST  
RELUCTANCE.  I KNOW I SPEAK FOR THE ENTIRE SENIOR STAFF AS WELL AS MYSELF  
WHEN I SAY THAT.  AS FAR AS WE CAN TELL TODAY, THE FORCE REDUCTION WE 
HAVE JUST MADE SHOULD BE ADEQUATE TO THE PRESENT NEED.  THAT IS NOT TO
SAY THAT CONTINUED ACTIONS WILL NOT BE REQUIRED TO INCREASE SALES AND 
CONTROL COSTS.  QUITE THE CONTRARY IS TRUE.  TO ACHIEVE OUR TARGETS, THE   
COMING MONTHS WILL REQUIRE EXTRA EFFORTS AND ELIMINATION OF ALL  
UNNECESSARY COSTS.  I AM CONFIDENT THAT WE WILL MAKE THOSE EFFORTS AND
THAT OUR COST CONTROLS WILL BE EFFECTIVE.    
     
WE ARE ALSO ENGAGED AT THIS TIME IN DETAILED PLANNING FOR 1986.  MOST 
INDUSTRY OPINION SUPPORTS A VIEW OF A CONTINUATION OF SLOW COMPUTER   
SALES THROUGH AT LEAST THE FIRST HALF OF 1986.   THEREFORE, WE ARE    
DEVELOPING PLANS WHICH TAKE THIS INTO ACCOUNT AND WILL CAREFULLY CONTROL   
OUR GROWTH TO MITIGATE AGAINST ANY FURTHER NEED FOR A FORCE REDUCTION.
     
FOR THE LONGER TERM, I BELIEVE WE ARE WELL-POSITIONED TO TAKE ADVANTAGE    
OF THE UPTURN WHEN IT HAPPENS.  BY THE END OF THE DECADE, OUR TARGET IS    
TO AT LEAST DOUBLE OUR REVENUES THROUGH EXPANDED SALES TO OTHER ISG   
BUSINESS UNITS AND TO ADDITIONAL OEM CUSTOMERS.   
     
IN EARLY 1986, ALMOST ALL OF MANAGEMENT WILL BE ENGAGED-ONE WAY OR ANOTHER-
WITH HELPING TO DEVELOP A LONG-RANGE BUSINESS STRATEGY TO ACHIEVE OUR 
GOAL OF PROFITABLE GROWTH.  ESSENTIAL TO THAT STRATEGY IS THE INVOLVEMENT  
OF EVERYONE IN THE QUEST FOR PRODUCTIVITY GAINS THROUGH QUALITY IMPROVEMENT.    
IN THE FINAL ANALYSIS, THE ONLY COMPANIES WHO CAN PROVIDE LONG-TERM   
EMPLOYMENT ARE THOSE WHO CAN BE CONTINUOUSLY PROFITABLE.  IN OUR HIGHLY    
COMPETITIVE INDUSTRY, THE SUCCESSFUL COMPANIES WILL BE THOSE WHO FOCUS ON  
QUALITY, WHO DELIVER PRODUCTS WHICH CONFORM EXACTLY TO CUSTOMER REQUIREMENTS,   
ARE COMPETITIVELY PRICED, ARE DELIVERED AS COMMITTED AND ARE PROFESSIONALLY
SERVICED. 
     
WITH YOUR HELP, I BELIEVE WE WILL MAKE OUR COMPANY A GREAT SUCCESS.   
     
     
     
VERN HART 
PRESIDENT
From: Osman Guven <osman@C930.Tymnet> 
Date: Sun, 6 Oct 85 11:58:49 PDT 
To: Carl A Baltrunas <carl@C930.Tymnet> 
Subject: PAY STANDARTS.. 


Subject: JOB CLASSIFICATION STANDARDIZATION 

                   MCDONNELL DOUGLAS FIELD SERVICE COMPANY  
     
                             INTEROFFICE MEMO
     
     
     
     
TO:       FIELD OPERATIONS ENGINEERS    
     
FROM:     VERN HART 
     
SUBJECT:  JOB CLASSIFICATION STANDARDIZATION 
     
DATE:     SEPTEMBER 25, 1985  
     
COPIES:   CURT MILLER, WYLIE TAYLOR
     
--------------------------------------------------------------------- 
     
DURING THE PAST THREE (3) MONTHS, THE PERSONNEL DEPARTMENTS FROM THE  
FORMER BUSINESS ENTITIES OF TYMSHARE, TYMNET, AND MICRODATA, AS WELL  
AS MEMBERS OF THE FIELD OPERATIONS STAFF, HAVE BEEN REVIEWING THE
INDUSTRY PRACTICES IN AN EFFORT TO STANDARDIZE JOB CLASSIFICATIONS    
AND THE ASSOCIATED OVERTIME/STANDBY/SHIFT DIFFERENTIAL POLICIES. 
     
EFFECTIVE SEPTEMBER 30, 1985 THE FOLLOWING PRACTICES WILL BE
IMPLEMENTED:   
     
     1.  EMPLOYEES IN POSITIONS OF FIELD SERVICE ENGINEER I,
         FIELD SERVICE ENGINEER II, AND SENIOR FIELD SERVICE
         ENGINEER WILL BE RE-CLASSIFIED TO A NON-EXEMPT, HOURLY  
         STATUS.    
     
     2.  OVERTIME COMPENSATION FOR ALL NON-EXEMPT (HOURLY)  
         FIELD OPERATIONS PERSONNEL WILL BE PAID AS FOLLOWS:
     
           * WORK HOURS EXCEEDING EIGHT (8) HOURS/DAY, UP TO
             TWELVE (12) HOURS/DAY, WILL BE PAID AT TIME-AND-    
             ONE-HALF (1.5) BASE SALARY.
     
           * WORK HOURS EXCEEDING TWELVE (12) HOURS/DAY WILL
             BE PAID AT DOUBLE-TIME (2.0) BASE SALARY. 
     
           * HOURS EXCEEDING FORTY (40) HOURS/WORK WEEK WILL
             WILL BE PAID AT TIME-AND-ONE-HALF (1.5) BASE SALARY.
     
     
     3.  STANDBY COMPENSATION FOR ALL NON-EXEMPT (HOURLY) FIELD  
         OPERATIONS PERSONNEL WILL BE PAID AS FOLLOWS: 
     
           * FIFTEEN PERCENT (15%) OF BASE PAY -- MONDAY    
             THROUGH SATURDAY, CALCULATED AND PAID IN FOUR (4)   
             HOUR INCREMENTS. 
     
           * TWENTY PERCENT (20%) OF BASE PAY -- SUNDAYS AND
             HOLIDAYS, CALCULATED AND PAID IN FOUR (4) HOUR 
             INCREMENTS. 
     
           * SERVICE PERFORMED DURING STANDBY HOURS WILL BE 
             COMPENSATED, IN ADDITION TO STANDBY HOURS, AS  
             FOLLOWS:    
     
                  MONDAY - SATURDAY:  PAID AT TIME-AND-ONE- 
                  HALF (1.5) OF HOURLY BASE PAY.  
     
                  SUNDAYS AND HOLIDAYS:  PAID AT DOUBLE-TIME
                  OF (2.0) OF HOURLY BASE PAY.    
     
     
4.  SHIFT DIFFERENTIAL FOR NON-EXEMPT (HOURLY) FIELD OPERATIONS  
    PERSONNEL WILL BE PAID AS FOLLOWS:  
     
           * SECOND SHIFT - 8% OF BASE PAY   
     
           * THIRD SHIFT - 12% OF BASE PAY   
     
     
DISTRICT SUPERVISOR AND GROUP LEADER CLASSIFICATIONS WILL NOT CHANGE  
FROM EXEMPT STATUS AT THIS TIME PENDING FURTHER REVIEW.
     
IF YOU REQUIRE FURTHER INFORMATION/DEFINITION, PLEASE CONTACT YOUR    
IMMEDIATE MANAGER.  
     
     
VERN HART, PRESIDENT
FIELD SERVICE COMPANY
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 10 Oct 85 1:10:26 UT
From: NTD.W/WANG@Ontyme.Tymnet 
Date: 09 OCT 85 15:04:10 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: I58023@Ontyme.Tymnet 
Subject: PIR# 1903 - CCT expansion 

     TO:  Carl Baltrunas
   FROM:  Weyyi Wang (X6190)
     CC:  David Chang			  Gazel Tan
 
   Subj:  PIR# 1903 - CCT expansion

	The implementation of PIR# 1903 (expand CCT) will allow the
   CCT(Circuit Characteristic Type) of circuit requests to have 6 bits.
   The current SUP permits 5-bit CCT. The extra bit, which is half-
   duplex flag, is represented as control-H in the login string. The new
   SUP (since V41.4) will support half-duplex circuit thru control-H as
   well as 6-bit CCT.

	Please let me know if there is any impact on the PDP-10 operating
   systems. Thanks.
From: FLETCHERC@X930.Tymnet 
Date: Mon, 14 Oct 85 8:46:02 PDT 
To: osman, dencoff, carl, helgem, bring, jms 
Subject: call in when absent 

If you are sick or absent for any unexpected reason please
notify Lois Frost at x2651 and she will let me know.
If you want to let me know via mail as well, that is fine
but consider Lois,
 the primary point of contaact.

	Craig
From: FLETCHERC@X930.Tymnet 
Date: Mon, 14 Oct 85 9:11:31 PDT 
To: carl, jms, osman 
Subject: possible meeting with Bill Kersten. 


Our new VP, Bill Kerse
Kersten, will be in Fremont tomorrow and may be able to
have a group meeting with us for a short while sometime
between 10:00 a.m. and 12:00 noon.  Us means all of Fremont
Technical Operations.  Please be here in case he is
available.


Daryll from Pagenet will have the new Motorola pagers
at 2:00 p.m. Thursday October 17th, so be sure to be
here to get the new ones and turn in the old ones.
	CRaig
From: MATOKA@X930.Tymnet 
Date: Tue, 15 Oct 85 16:20:36 PDT 
To: Carl 
Subject: Loadii on sys 39 

Carl, there seems to be a problem on sys 39 that a few of us over here
at NTD are having when we try to use loadii. After coming back with the
response "BUILDING CIRCUIT", Loadii will just sit there for several minutes 
without doing anything. It dosen't seem to matter what's getting loaded,
the result (or lack of them) is always the same. In addition, when attempting
to log in to sys 39, I sometimes just get hung up after typing the carriage return. I can build circuits to other hosts on Tymnet with no problem. Do you
have any idea what the problem might be?
-Greg
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 15 Oct 85 17:01:45 PDT 
To: MATOKA@X930.Tymnet 
Subject: Re: Loadii on sys 39 
In-reply-to: your message of Tue, 15 Oct 85 16:20:36 PDT

Greg,
  My guess is that th eproblems that you are seeing with LOADII have something
to do with the "new" base... Especially since nothing else has changed.  Can
you localize the time frame when this has been happenning?
/Carl
From: MATOKA@X930.Tymnet 
Date: Tue, 15 Oct 85 21:04:58 PDT 
To: Carl 
Subject: Loadii 

Carl:
	I first noticed the problem around 3:00 P.M. this afternoon. It
was working fine this morning as late as 11:00 A.M. Doyou know if there's
anyone I could contact that was responsible for loading the base?
-Greg
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 16 Oct 85 10:50:55 PDT 
To: MATOKA@X930.Tymnet 
Subject: Re: Loadii 
In-reply-to: your message of Tue, 15 Oct 85 21:04:58 PDT

My understanding is that the new base was up & down for a few hours the
other day, probably not yesterday, but i don't know for sure.  If the
base is cut assuming 128 ports, and 39 actually has 145 in the monitor,
there could be a problem.  The eventual solution there is to expand to
256 ports, but that hasn't been done yet for the monitor.

I don't have any answers on why LOADII is or isn't working right.  That
is assuming it was working at 11 and not at 3?  Sigh.  Talk to Philip A.
or Dave Smith (or Bob Sorenson) about the base.  Someone there should know.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 16 Oct 85 12:08:53 PDT 
To: NTD.W/WANG@Ontyme.Tymnet 
Cc: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Circuit Characteristic Type on PDP-10. 

In reference to your message to Carl Baltrunas: Does this mean that PIR# 1903
will use 7 bits of information?  That is, one bit for half-duplex and six for
the CCT?  If so, the code in the base and in the PDP-10 will have to be
changed.

Currently, we get four 8-bit bytes from the base when a circuit is built.
The first has the terminal identifier, the next two have the originating node
number, the last has the port number on the originating node.  The format of
the first byte is:

Bit 7 = ignored, always set to zero by the base.
Bit 6 = ignored, always set to zero by the base.
Bit 5 = set by the base if the circuit is half duplex.
Bits 4-0 = 5-bit code for terminal identifier.  Of the 32 possible values for
	this field, 24 are documented in the PDP-10 monitor.

It will be a simple change in the PDP-10 monitor to handle a 6-bit code, if
the bit signifying half-duplex is moved.   I recommend that the new base code
pass that information in the high-order bit.  New format:

Bit 7 = set by the base if the circuit is half duplex.
Bit 6 = ignored, available for future expansion.
Bits 5-0 = 6-bit code for terminal identifier (CCT).

Joe Smith
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 17 Oct 85 1:09:22 UT
From: NTD.W/WANG@Ontyme.Tymnet 
Date: 16 OCT 85 14:42:44 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: I61457@Ontyme.Tymnet 
Subject: PIR# 1903 - Your message A40652 & A40655 

     TO:  Joe Smith
   FROM:  Weyyi Wang (X6190)
     CC:  Carl Baltrunas
	  David Chang			  Gazel Tan
 
   Subj:  PIR# 1903 - Your message A40652 & A40655

	The purpose of PIR 1903 is to expand CCT from currently 5-bit
   CCT to 6-bit CCT. With compatibility in mind, the new SUP will support
   one of the following login strings when the circuit is a half-duplex:
	1). 5-bit CCT plus control-H
	2). 6-bit CCT with bit 5 as half-duplex flag. (I adopt your naming
	convention here, where bit 7 and 6 are ignored, bit 4-0 are TID.)

	The second option is new. From the PDP-10 point of view there should
   not be any changes of CCT. The only difference is that the CONSAT has 
   options to specify the half-duplex circuit in the login string, as 
   described above.

	I will call you when we test this feature in Bubbnet, so you can
   help us confirm the compatibility.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 22 Oct 85 17:26:35 UT
From: NTD.W/WANG@Ontyme.Tymnet 
Date: 22 OCT 85 08:56:22 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: I64239@Ontyme.Tymnet 
Subject: CCT Expansion (PIR# 1903) 

     TO:  Joe Smith
   FROM:  Weyyi Wang (446-6190)
     CC:  Carl Baltrunas
	  David Chang			  Gazel Tan
 
   Subj:  CCT Expansion (PIR# 1903)

	The CCT expansion has been tested at SUP 7 of Bubbnet. Please
   verify that there are no impacts on PDP-10 O/S. Thanks.

	(Note. Make sure that SUP 7 is active when you do the test. I
   will wake up SUP 7 for you if necessary. Just give me a call.)
From: FLETCHERC@X930.Tymnet 
Date: Wed, 23 Oct 85 12:32:18 PDT 
To: carl, osman, jms, dencoff, helgem, bring, baigent 
Subject: Bill Kersten meeting 


This Thursday Bill Kersten will be in town and we are scheduling a meeting
at 2pm in the multi-purpose room where everyone can meet him.  See you there.

	Craig
From: Ken Dawson <Ken@X930.Tymnet> 
Date: Sun, 27 Oct 85 23:55:25 PST 
To: carl 
Subject: class 

    It seems that I'll be attending class on N First St this whole bloody
    coming up week.  All about ISIS programming, or some such.  So it seems
    I won't be fit to talk to 'til next week.

    I am awfully glad to see lonely CRs recognized.  The only files I can
    think of which might contain ^D, and which might be printed are
    PEAK.INI files.

    /ken
Received: from C39.Tymnet by X930.Tymnet; Mon, 28 Oct 85 10:34:31 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 28 Oct 85 10:24:35 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Cc: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: "OLD USER NAMES VIDA VALIDATIONS"... 

(begin forwarded message)
----------------------------------------------------------------
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 28 Oct 85 9:58:18 PST 
To: WRS@C39.Tymnet 
Cc: INTL.VIDA@ONTYME.TYMNET 
Subject: "OLD USER NAMES VIDA VALIDATIONS)"... 

OLD USER NAMES - VIDA (TYMNET VALIDATIONS) IS TRYING TO "CLEAN UP OLD NAMES".
NOW UP TO GUY BLOOD - HAS GOT SEVERAL PEOPLE TO SAY O.K., DO IT. THEY ARE
(AT LEAST) THE FOLLOWING: JILL N. & NANCY,DENNIS JONAS, & TYMSHARE VAL'D'T'S.
I TOLD HER TO WAIT!!!! UNTIL  I CONTACTED YOU - CNA WE - IF NOT, WHY??
NEXT MEETING - WILL YOU BE HERE @ GOURDIAN SYS. MEETING FRI??, IF SO HOW
ABOUT AN EARLY LUNCH HERE?
----------------------------------------------------------------
(end forwarded message)

I don't know of any reason to hold off canceling GBLOOD.  If you (Carl)
don't either, please reply to Wayne and myself so he can tel Vida to go
ahead.  Thanks -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 28 Oct 85 10:46:54 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: William R Soley <wrs@C39.Tymnet> 
Subject: Old Usernames - GBLOOD 

I know of no reason why username GBLOOD needs to be around for the
smooth operation of the PDP-10 software.  If the 940s can survive,
then I say cancel it.

As far as cancelling other OLD usernames, I would like to be contacted
first, PRIOR TO cancelling any accounts in GAN 3 or dept 4011. Thanks.
/Carl
Received: from C39.Tymnet by X930.Tymnet; Mon, 28 Oct 85 11:53:11 PST
Return-path: <BARTLETW@C39.Tymnet> 
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 28 Oct 85 11:27:26 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: William R Soley <wrs@C39.Tymnet> 
Subject: Re: Old Usernames - GBLOOD 
In-reply-to: your message of Mon, 28 Oct 85 10:46:54 PST

THANKS FOR THE PROMPT ANS. WILL INFORM VIDA OF YOUR REQUEST, AND 
HAVE HER PROCEED WITH THE CANCELLATION.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 28 Oct 85 15:20:49 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: My location 

I am staying at the Stouffer Concourse Hotel, (314)429-1100 room 707.
The phone number in MDC building 706 is (314)323-7151.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 29 Oct 85 9:07:17 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: correction to previous message 

The building and phone number were wrong.  ISG is in building 307,
the phone number is (314)232-7151.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 30 Oct 85 17:25:33 PST 
To: Osman Guven <osman@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet> 
Subject: Re: Message from Donahue 

Osman,
  Craig was by and we talked about the msg from Donahue... it did bring
up an interesting co-incidence.  I looked at the crashes and BOTH of the
crashes had APRSTS[ NoMem MemBusParity, ERASTS[ 602000,,APRINT+2  which
is really APRINT+2 plus 3 from bits 0-1 == APRFAT+2 which is by a weird
co-incidence RDERA ERASTS!  Both crashes had TTYZNE as the stopcd, and
were caused by a message type 15 (leave deferred echo mode) while there
were characters left to be echoed  LDBECC(U) ... Hmmmm.  Both crashes are
at DIEREH!  Both crashes have a completely different idea of how they got
to DIEREH... one was at TTYBMT (bad message type? how did it get here?)
if it was processing a stopcd?  and the other was in the null job!

Both had a CH1 in progress from the same location DIE1A+13... Hmmmm.

Joe?  any ideas?
Osman, please talkto craig BEFORE you answer Donahue.

/Carl

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 2 Nov 85 22:46:43 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: OSTC.DEC2 in St Louis is host 4483 and 4484. 

TYMNET host 4483 connects to 24 async 1200 baud lines on the DN87S known as
DEC2A(22), and host 4484 connects to 32 lines on the other DN87S, DEC2B(32).

A list of disk structures and PPNs on them is in (CB-DECDIR)DEC2.DIR.
The file (CB-DECDIR)MONI.DIR is a list of all the files on the disk that
holds their monitor sources.  The rest of their software, including CUSPs,
language compilers, and GALAXY is on SOFT:[10,7,*].  That disk's directory
is on 930 in (CB-DECDIR)SOFT.DIR.

/JMS
From: Osman Guven <osman@C930.Tymnet> 
Date: Wed, 6 Nov 85 1:29:50 PST 
To: Fletcherc, Carl A Baltrunas <carl@C930.Tymnet> 
Subject: Schedule of 11/6.. 


I worked all night last night, soo I would not be in the
office in the morning.
Osman..
From: Osman Guven <osman@C930.Tymnet> 
Date: Wed, 6 Nov 85 12:23:24 PST 
To: Carl A Baltrunas <carl@C930.Tymnet> 
Subject: will be working from home till 3pm. 


From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 6 Nov 85 12:49:52 PST 
To: Ingrid Dolovitchenyj <IPC.I/Dolovitchenyj@Ontyme.Tymnet> 
Subject: ARCHIVE 

I took a "quick" look at the documentation you sent over and it looks
like it may be possible to simply change the ARCHIVE program to accomodate
a larger tape number.  I will get back to you on this as soon as I get
finished with the current project I'm working on... XEXEC, a replacement
for PERP to run on the PDP-10s.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 7 Nov 85 16:32:36 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: St Louis TOPS-10 operators 

The operators of the TOPS-10 KL-SMP system in St. Louis are:

	Roe West - lead operator
	Joe Hulett - 2nd day operator
	Mary Meyers - night operator

The phone numbers are (314)234-5588 and (314)234-5589.  Both phones are
near the KL CPUs, and if no answer, they ring over to (314)234-7175.  The
latter number is by the COEES VAX and is manned 24 hours a day.

Jim Crews is the supervisor of Production Services, his number is (314)233-4847.
Received: from C39.Tymnet by X930.Tymnet; Fri, 8 Nov 85 12:20:19 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 8 Nov 85 11:57:50 PST 
To: carl@930 
Subject: REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70 

(begin forwarded message)
----------------------------------------------------------------
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Fri, 8 Nov 85 12:04:57 UT
From: NSS.E/RIORDAN@Ontyme.Tymnet 
Date: 07 NOV 85 14:39:54 
To: NTD.WRS@Ontyme.Tymnet 
Message-id: M36869@Ontyme.Tymnet 
Subject: REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70 

 
                            MEMORANDUM
 
 
  DATE:     NOVEMBER 7, 1985
 
  TO:       TYMNET, FIELD SERVICES, PAYMENT SERVICES, TRAVEL
            MANAGEMENT SERVICES, NETWORK TECHNOLOGIES PERSONNEL
            ET AL 
 
  FROM:     EILEEN RIORDAN
 
  SUBJECT:  REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70
 
  ==============================================================
 
  Many  of you are already aware of the December 1 implementation 
  of  a  four-host system for the generation and  maintenance  of 
  public  network  code.   These four hosts will be  the  current 
  three:  25, 33 and 70, and the new system, 54.
 
  In  the  course  of  implementing  this  project,  we  will  be 
  validating to system 54 ONLY those USERNAMES DIRECTLY  INVOLVED 
  WITH  the  GENERATION and MAINTENANCE OF PUBLIC  NETWORK  CODE.  
  Those usernames, with appropriate justification, should already 
  have been submitted to me.   Any username which is currently on 
  25,  33  or 70,  which was not requested to be validated to 54, 
  will  be removed from 25,  33 and/or 70 effective  December  1.  
  Usernames currently on 54,  for which we have no request,  will 
  also be removed December 1.
 
  The purpose of this memo is to provide you with enough time to
  relocate  your files to a more appropriate host.   If you are a 
  member of the Tymnet organization, and need assistance in this, 
  contact myself or Karen Lynas (fin.k/lynas).   If you are not a 
  member of Tymnet,  contact Systems Resources (ipc.sramail)  for 
  assistance.  
 
  All  of the following usernames are candidates for deletion  on 
  December  1.   PLEASE REVIEW THIS LIST VERY CAREFULLY.   Due to 
  the large number of usernames involved,  we will not do a  tape 
  backup  prior  to the username removal (although data  will  be 
  available   on   the  last  weekly  tape  done   routinely   by 
  operations).
 
  We appreciate your cooperation.

*1BATCH                            KADNEY    
*1KEN                              KALENDA   
*2TRR                              KDCDIAG   
*2TTY                              KDDIAG    
*6NEWS                             KEN       
00083                              KHOLLAND  
1213MGR                            KILLIONJ  
1BATCH                             KKELLER   
31013399                           KOPFJ     
32BIT                              KOVACH    
6NEWS                              KSALEHI   
ABELE                              LAGOLD    
ACCG1                              LAMBP     
ACHAO                              LAMPMAN   
ACHERIN                            LANEK     
ACT10                              LASALES   
ACTRTY                             LDDC      
ADAZZI                             LEAHYP    
ADPCODE                            LELANDY   
AKRONE                             LHEINZEL  
ALBERTAC                           LIBRARY1  
ALFENN                             LIENEMANC 
ALLFILES                           LINK10    
AMUNDSON                           LMAURER   
ANALYSIS                           LMORRIS   
ANVERM                             LNIRENBERG
APLAB                              LOVELL    
APLATT                             LSHERMAN  
ARCHIVES                           LUEDERK   
ARELLA                             LUSKERJ   
ATLANTICTECH                       LYNASK    
ATT                                M33       
ATTAM                              MACEMON   
ATTC                               MADDERRA  
ATTE                               MAGNUM    
ATTS                               MAIL      
ATTW                               MAINTDIAG 
AVINASH                            MALAVI    
AYARS                              MALUBINA  
BACKUP                             MARACT    
BAHCODES                           MARCOMM   
BAIGENT                            MARCONIT  
BASESUP                            MARKMCW   
BATES                              MART1     
BBENG                              MART2     
BDACODES                           MART3     
 
BECHLYT                            MART4     
BERT                               MART5     
BETATESTINT                        MARTYB    
BILLF                              MARYC     
BLAIR                              MATOKA    
BNOLLEY                            MATS      
BOBH                               MATS.01   
BOBJ                               MATTHEWS  
BOFADMIN                           MATUCHAD  
BONDAD                             MBERGER   
BONSTOTT                           MCCUSKER  
BOURDEAU                           MCDAVID   
BRETTF                             MCH       
BREZELL                            MCKIERNAN 
BRING                              MCNEY     
BRUCES                             MCONWAY   
BSQUIST                            MDCIRVINE 
BSTALING                           MDOWNEY   
BSTRONG                            METCALFS  
BSTURM                             MEYERG    
BTHOMSON                           MFGRECEP  
BUBBNET                       MIKELU    
CADPLOT                            MIKEMO    
CARL                               MIKERUDE  
CARLSTEDTH                         MILLERK   
CARRELLA                           MIMI      
CATECH                             MISTAPE   
CATTANEO                           MITCHELJ  
CBROWN                             MLEWIS    
CC                                 MONDOC    
CCHANG                             MOORING   
CCHEN                              MORIARTY  
CENTECH                            MOSSERJ   
CENTRALTECH                        MOXON     
CESDOC                             MPL       
CGIBSON                            MREBMANN  
CHANG                              MSEDSEL   
CHEDIN                             MSHEN     
CHENC                              MSLOUISE  
CHERIE                             MSMA      
CHRISM                             MSPOPPE   
CHUNGLAM                           MSTSCHED  
CIDNCODE                           MVSMURS   
CLARKET                            MWALTERS  
CLEMENS                            NAD       
 
CLOVER                             NADM      
CMTI                               NADMRES   
CMSSUP                             NASON     
CMTI                               NDT       
CNFE                               NEATECH   
CNFEMGR                            NEDIST    
CNFESEC                            NEPNET    
CNFETECH                           NET1STAGE 
CNSALES                            NETANAL   
COBOLQA                            NETCONAD  
COLLECTR                           NETDATA   
CONSULTING                         NETDB     
CORBELL                            NETDETAIL 
CORPTEL                            NETDEV2   
CORSO                              NETDVDOC  
CORTEZ                             NETEK     
CPAPRINT                           NETENG    
CRASH                              NETEOM    
CRAY                               NETMAPNS  
CRFSM                              NETMID    
CTOPSMGR                           NETOPS    
CUD10                              NETPERM   
CUPCOPS                            NETSALES  
CUPCWRN                            NETSTAT   
CUS.SAT                            NETSUPPT  
CUSNETS                            NEWS      
CUSTORDS                           NFERECEP  
CWCODES                            NFESEC    
CWDIAG                             NGREGORY  
CWILSON                            NLYONS    
CWU                                NPRODUCT  
DAILYPROGS                         NSCNET    
DAILYREPS                          NSDADM    
DALMON                             NSDBINS   
DALSWRN                            NSDBKUP   
DANIELSR                           NSDLIB    
DAUGHTJ                            NSDSINS   
DAULTON                            NSJENKS   
DAVARJ                             NSTECH    
DAVISA                             NTDLIBRARY
DB130                              NTS       
DB131                              NTSDEMO   
DB132                              NWILLIAMS 
DB134                              NYPRINT   
DB220                              OAHUCODE  
DB225                              OASYS     
DB228                              OHARAB    
DBA                                OCONNORK  
DBACCNTRL                          OHARROW   
DBACOVE                            OLDPLOT   
DBADOUG                            OLSONT    
DBAHST                             OSERANG   
DBBTS                              OSIRIS    
 
DBCENTSALE                         OSIRIS1   
DBCEO              
DBCES                              OSNF      
DBCIS                              OSU       
DBCNFE                             PACAULT   
DBCNFED                            PACTECH   
DBCNFEH                            PARPRINT  
DBCTS                              PARSTW    
DBDDD                              PATCH     
DBDML                              PATELN    
DBDND                              PATROL    
DBEASTSALE                         PATTIMCD  
DBENFE                             PCM       
DBENFEB                            PCORCORAN 
DBENFEN                            PCROUTS   
DBENFEW                            PDUNLEVY  
DBENG                              PEARLS    
DBENGA                             PEREZG    
DBEPN                              PETER     
DBETS                              PGJCOG    
DBFEDMKTG                          PHIL.CRAIG
DBFIELDOPS                         PHILJ     
DBFIN                              PILCHER   
DBHANNAH                           PISTAT    
DBHQTECH                           PJ        
DBLISS                             PLOT      
DBMAN                              PLOTTING  
DBMKTG                             PMANLEY   
DBNESALE                           PMEASUREME
DBNSTECH                           PMELLET   
DBOPER                             PMORSE    
DBOPSPLNG                          PNAGEL    
DBPNETS                            PNELSON   
DBPRES                             PNETPG    
DBPROJ                             PNETS     
DBRES                              PNETSUP   
DBSNI                              POINTERC  
DBSWTS                             POPPE     
DBTAR                              POSTAL    
DBTIO                              PQUE      
DBTLC                              PRAMS     
DBTMP                              PRIMP     
DBTS                               PRINCE    
DBTSHACCT                          PRODEV01  
DBVPSALES                          PRODEV02  
DBWESTSALE                         PRODNET   
DBWNFE                             PROJMGT   
DBWNFES                            PSBROWN   
DBWTS                              PUB       
DCHANG                             PUBNET    
DCOPRINT                           PWARN     
DCORNELY                           QABUILDOSC
DENCOFF                            PWSLIB    
 
DEPOT                              QALIBSYM  
DEPTECH                            QASYS     
DGAETANI                           QSMITH    
DGAUVIN                            RADMON    
DGOAD                              RAFALA    
DHAWES                             RAFFO     
DHSMITH                            RAFFOR    
DIAG                               RBROWN    
DIAG10C                            RCACODES  
DIAG10D                            RCHUN     
DIAG10V                            RESMGT    
DIAGQA                             RESRFP    
DIAMONDP                           RESZ      
DIST216                            RFMC      
DJEZEK                             RGFOX     
DKNOTT                             RHOWLAND  
DLANE                              RICHARDS  
DLASATER                           RICHARDSON
DLEE                               RISDIAG   
DMACOMB                            RJK       
DMILLER                            RKLUTE    
DMIREQ                             RMILTON   
DMOULTON                           RNIELSEN  
DNDNAC                             RODDAMJ   
DNOLLEY                            RODRIGUESP
DOCAIL                             ROLAND    
DONAHUE                            ROLANDK   
DONNELLY                           ROOPED    
DOOLITTLER                         ROY       
DPELSUE                            RSB       
DRAY                               RSCSSTAT  
DREID                              RSPRENKEL 
DRISCOLL                           RTREHIN   
DUNLEVYM                           RUDMAN    
DUPJOB10                           RVIVIER   
DWEST                              RWS       
DYOUNG                             SAIL      
DZISA                              SAILIB    
EADAMS                             SAILTEST  
EASTECH                            SALLY     
EASTLINE                           SANDYMF   
EASTPNET                           SBS       
EASTTECH                           SCAPIK    
EBS                                SCHARF    
ECMOPS                             SCHED     
EDIDADM                            SCHENDEL  
EDWALL                             SCHOU     
ENFESEC                            SCHRAMM   
ENGHNTR                            SCRIBNER  
ENGR                               SDERRY    
ENSS                               SEATECH   
EQUIPDIV                           SECONDHIF 
EPASCAL                            SEMAR     
 
EPASCAL1                           SETECH    
ERICV                              SHARMA    
EST                                SHEPPARDA 
ETAM                               SHSU      
ETULK                              SHWONG    
EUROTECH                           SIRTI     
EWELSH                             SLATERK   
EWINDLE                            SLIISIS   
F40                                SLLDCAIN  
FACILITIES                         SLOVAKM   
FACSEC                             SMITHDE   
FADMIN                             SMYTHEC   
FBARTON                            SNAVELYJ  
FBATCH                             SOCKWELL  
FCANNING                           SOLOMON   
FCM                                SOMERVIL  
FEDACCG                            SORRELL   
FERRISK                            SOURCEINT 
FFISHMAN                           SOUTHSALES
FICHEGEN                           SPEX      
FIELDNUM                           SPIELLER  
FJAVADI                            SPINALE   
FLETCHERC                          SPL       
FRANK                              SPOOL     
FRANKWU                            SPOOLRM   
FREITAS2                           SPOOLING  
FRENCH                             SPPARCH   
FSIEGEL                            SPPOPER   
FSUPPORT                           SPUNKTEST 
FTAPSELL                           SRA1      
FTSYS                              SRACRT    
FTSYSDOC                           SRAFRCST  
FTTBA                              SRAMOV    
FUTUREPLOT                         SRASO     
GAUCI                              SROOS     
GBEIDLER                           SSARCHD   
GBLOOD                             SSBACKUP  
GCOLES                             SSDEV     
GCONE                              SSEREP    
GGARDINA                           SSINSTALL 
GKASHER                            SSJREP    
GKING                              SSPAMBIN  
GMASTERS                           SSPERP    
GOLOBIC                            SSPAMSYM  
GORDON                             SSPERP    
GRANVOLD                           SSPRINT   
GREATECH                           SSTERLING 
GSWINTON                           SSTRANS   
GWALPOLE                           SSUTIL    
HAGERL                             SSVALSUP  
HAWB                               STAFFORD  
HAYESDRL                           STALLING  
HELGEM                             STARKSG   
 
HOLD                               STEINJ    
HQLAB                              STHOMPSON 
HQTECH.TIP                         STIER     
HQTECH1                            STIMACM   
HTSENG                             STOY      
HUEYMA                             STSUI     
HULTQUIS                           STUTZMANC 
HVBURTON                           SUDHIR    
INDIAL2                            SVOTH     
INFO                               SWARTZM   
INSCODE                            SWATS     
INSDCOM                            SWHITE    
INSDGSS1                           SWONG     
INSNET                             SYNCMAG   
INTACT                             SYS       
INTLACCTG                          SYSADM20  
INTLPROGS                          SYSADM21  
INTYM                              SYSBACKUP 
ISINSTALL                          SYSDOC    
ISIS                               SYSMAINT  
ISISDOC                            SYSNEWS   
ISISFILES                          TAPELIB   
ISISTECH                           TAPELIBE  
JAKOBITZ                           TBAFTDEB  
JAIL                               TBAFTTRANS
JALCAN                             TBATLIB   
JANAN                              TCASS     
JBRENNAN                           TCHECKTID 
JBUXTON                            TCHOW     
JCARTER                            TCMS      
JCLMIN                             TCUPAL    
JCM                                TECHDOC   
JEFSEY                             TECHLAB   
JENGLISH                           TECHLIB   
JENNYG                             TECHTRAIN 
JHSIEH                             TECHW     
JHSIEH1                            TELCODBS  
JJOELS                             TESTASB   
JLEE                               TESTMNCH  
JLIOU                              TGBILL    
JLIOU1                             TGCODES   
JMARCIN                            TGJUDY    
JMS                                TGRUSS    
JMARTELL                           TIIDEV    
JMCOLINROB                         TKELLEY   
JMESSINA                           TMYLES    
JMORSE                             TNETOPS   
JMS                                TNSCDOC   
JNEAL                              TNSCMKTG  
JOHNSOND                           TNSETS    
JONAS                              TNSUKM    
JORDANR                            TNXAIL    
JSOUNG                             TOMASI    
 
JWAGNER                            TRAINLAB  
JWANG                              TRAINSEC  
JWILLOTT                           TRUCONTROL
                                   TSAMPSON  
                                   TSUCOOP   
                                   TSUIS     
                                   TSWALLACE 
                                   TTMSBUBB  
                                   TTMSISIS2 
                                   TTMSTS    
                                   TTS317    
                                   TWANG     
                                   TYMDATA   
                                   TYMGEN    
                                   TYMGRIPE  
                                   TYMISIS   
                                   TYMRES    
                                   TYMTEST   
                                   UAS       
                                   UFD       
                                   UIS       
                                   UKDOC     
                                   UN1       
                                   UPTGROVE  
                                   USEVAL    
                                   USIAKR    
                                   UTIL      
                                   VALDEV    
                                   VALIDATE  
                                   VARDIAG   
                                   VATECH    
                                   VIVIAN    
                                   VKRLEE    
                                   VMCMST1   
                                   VMLIBR    
                                   VUE       
                                   WAGNERD   
                                   WALKERG   
                                   WESTSALE  
                                   WASOWSKI  
                                   WASUPT    
                                   WCMFIXIT  
                                   WEISINGER 
                                   WESTECH   
                                   WEUSKE    
                                   WHITLOCK  
                                   WHONG     
                                   WIEBET    
                                   WNFE      
                                   WNFEOPS   
                                   WNFESEC   
                                   WNFETECH  
                                   WRFS      
 
                                   WRFSC     
                                   WRHOST    
                                   WRONSTADT 
                                   WRPNET    
                                   WRS       
                                   WSHAW     
                                   X22113    
                                   XAIL      
                                   XCONSULT  
                                   XX13420001
                                   YAMM      
                                   YEUX      
                                   YORKS     
                                   YOUNGERC  
                                   YOUNGS    
                                   YOUNGV    
                                   YOUNGV    
                                   ZAITCHICKH
----------------------------------------------------------------
(end forwarded message)

Have you seen this?  She must be out of her mind!  I don't expect Eileen
to really do something like this.  Maybe she got the lists mixed up and
sent us the one of names to keep.  I'm sending her a message to that effect
and will forward any reply.  Please let me know if you have anything vanish
(not that I can do anything, just that I'm curious).  -Bill
Received: from C39.Tymnet by X930.Tymnet; Fri, 8 Nov 85 13:01:14 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 8 Nov 85 12:02:36 PST 
To: NSS.E/RIORDAN@Ontyme.Tymnet 
Cc: Al Fenn <NTD.A/Fenn@Ontyme.Tymnet>, Bill Euske <NTD.W/Euske@Ontyme.Tymnet> 
Subject: Re: REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70 
In-reply-to: M36869@Ontyme.Tymnet of 07 NOV 85 14:39:54

I received your list of usernames you intend to cancel and am a little
shocked.  You should never delete a username in GAN 1 (operations)
or GAN 3 (systems).  That will take care of a large part of the list.
They can be easily identified using the accounting data base on 74.  If
you would like help in this matter, let me know and I'll send you an
actual list of GAN 1 and GAN 3 names that appear on your list.

I also believe there might be some other mechanisms that you are perhaps
unaware of that would serve to control the usage you are concerned about
without wholesale annihilation.  Please give me a call so we can talk
about alternatives.

	"...  Due to the large number of usernames involved, we will
	not do a tape backup prior to the username removal (although
	data will be available on the last weekly tape done routinely
	by operations)."

I suggest a special all-files dump be made prior to the massacre which
should be kept in a well documented place with a directory on microfiche
as per standard operations procedure.  This tape set should be retained
for at least three years.  We have lost much valuable source code to
actions such as this -- no matter how careful you are there will always
be something deleted that shouldn't have been.  It is the files that are
infrequently accessed (such as source files which are only accessed when
there is a bug to fix) which people are least aware of and usually don't
miss until it would otherwise be too late.

	"... PLEASE REVIEW THIS LIST VERY CAREFULLY. ..."

Although I do hope those who receive this message do take care, you have
placed the responsibility completely upon yourself by soliciting a list
of exceptions to your rule, as opposed to soliciting a list of inclusions.
Certainly a less dangerous approach to the matter would be to ask for a
response of yes or no on each name.  If you get no response, assume you
haven't contacted the right person and keep the name valid until you are
able to identify its owner.

-Bill
Received: from C39.Tymnet by X930.Tymnet; Fri, 8 Nov 85 13:01:15 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 8 Nov 85 12:02:36 PST 
To: NSS.E/RIORDAN@Ontyme.Tymnet 
Cc: Al Fenn <NTD.A/Fenn@Ontyme.Tymnet>, Bill Euske <NTD.W/Euske@Ontyme.Tymnet> 
Subject: Re: REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70 
In-reply-to: M36869@Ontyme.Tymnet of 07 NOV 85 14:39:54

I received your list of usernames you intend to cancel and am a little
shocked.  You should never delete a username in GAN 1 (operations)
or GAN 3 (systems).  That will take care of a large part of the list.
They can be easily identified using the accounting data base on 74.  If
you would like help in this matter, let me know and I'll send you an
actual list of GAN 1 and GAN 3 names that appear on your list.

I also believe there might be some other mechanisms that you are perhaps
unaware of that would serve to control the usage you are concerned about
without wholesale annihilation.  Please give me a call so we can talk
about alternatives.

	"...  Due to the large number of usernames involved, we will
	not do a tape backup prior to the username removal (although
	data will be available on the last weekly tape done routinely
	by operations)."

I suggest a special all-files dump be made prior to the massacre which
should be kept in a well documented place with a directory on microfiche
as per standard operations procedure.  This tape set should be retained
for at least three years.  We have lost much valuable source code to
actions such as this -- no matter how careful you are there will always
be something deleted that shouldn't have been.  It is the files that are
infrequently accessed (such as source files which are only accessed when
there is a bug to fix) which people are least aware of and usually don't
miss until it would otherwise be too late.

	"... PLEASE REVIEW THIS LIST VERY CAREFULLY. ..."

Although I do hope those who receive this message do take care, you have
placed the responsibility completely upon yourself by soliciting a list
of exceptions to your rule, as opposed to soliciting a list of inclusions.
Certainly a less dangerous approach to the matter would be to ask for a
response of yes or no on each name.  If you get no response, assume you
haven't contacted the right person and keep the name valid until you are
able to identify its owner.

-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 8 Nov 85 15:38:01 PST 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70 
In-reply-to: your message of Fri, 8 Nov 85 11:57:50 PST

Gulp!  No, I hadn't seen that message.

I took a quick look at the usernames and TYMNET was not one of them, so
it's pretty conclusive that this is the list she intends to DELETE.
Her list includes: (UFD) (SYS) (FTSYS) (UN1) (*1BATCH) (SPOOL) etc ...

I will send her a note so that she knows that I'm concerned about the
deletion of system usernames, so that I will be copied on any further
memos.   Sigh!

Let me know if you hear anything more.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 8 Nov 85 15:47:51 PST 
To: NSS.E/Riordan@Ontyme.Tymnet 
Cc: William R Soley <wrs@C39.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Osman Guven <osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet>, Dennis
	Coffey <dencoff@C26.Tymnet> 
Subject: Removal of usernames from systems 25, 33 and 70 

I just received a forwarded copy of your memo regarding the removal of
usernames from these 3 systems and I am *VERY* concerned that you would
even think of this kind of wholesale username annihilation.

I realize it is your responsibility to manage the system resources for
TYMNET, but this kind of action shows a marked lack of understanding of
the systems involved.  TYMCOM-X systems require a fairly substantial
list of GAN 1 and GAN 3 usernames for the smooth operation of both the
operating system itself and related utilities.  System accounting and
billing require some usernames in GANs 4 and 6.  Software distribution
requires usernames in GANs 1 and 11447.

I believe that there must be a better mechanism for consolidating the
system resources of TYMNET and removing unnecessary users from your new
or dedicated TYMCOM-X systems.

Please give me a call (afternoons) at 415-794-2515  or ext 116-2515 so
that we may talk about this matter.

/Carl

Received: from C39.Tymnet by X930.Tymnet; Sun, 10 Nov 85 16:28:40 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sun, 10 Nov 85 16:26:17 PST 
To: carl 
Subject: "BILL I WOULD INDEED APPRECIATE"... 

(begin forwarded message)
----------------------------------------------------------------
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Sat, 9 Nov 85 1:26:27 UT
From: NSS.E/RIORDAN@Ontyme.Tymnet 
Date: 08 NOV 85 16:05:20 
To: NTD.WRS@Ontyme.Tymnet 
Message-id: M37205@Ontyme.Tymnet 
Subject: "BILL I WOULD INDEED APPRECIATE"... 

BILL
I WOULD INDEED APPRECIATE A LIST OF ALL USERNAMES IN GAN1 AND GAN3 ...
THO AM FEARFUL AT YOUR SUGGESTIONS THAT THIS WILL ENCOMPASS MOST
OF MY LIST.  WHO ARE ALL THSES WONDERFUL PEOPLE WHO
MUST BE ON THESE HOSTS.  I CERTAINLY HAVE NO OBJECTION TO PEOPLE
BEING THERE WHO ARE IN AN OPERATIONS/SUPPORT FUNCTION, WHO DO NOT
GEN/MAINTAIN CODE - BUT I AM AFRAID THERE ARE MASSES OF OTHER
USERNAMES, LEFT OVER FROM PREVIOUS USES OF 25/33/70/54, AS WELL
AS USERNAMES FROM OTHER DIVISIONS AND PEOPLE WHO'VE LEFT THE
COMPANY.   I DO NOT UNDERSTAND, WITH ALL THE PUSH FOR SECURITY WHY,
FOR EXAMPLE, GBLOOD IS STILL VALID EVERYWHERE I LOOK.  THE
ONLY EXCUSE I KEEP GETTING FROM MY ATTEMPTS TO DELETE IT ARE
'THERE MIGHT BE FILES THERE ......` -  SOOOOOO HAVE SOMEONE COPY 
THOSE FILES TO AN ACTIVE DIRECTORY.  

I WILL FOLLOW YOUR RECOMMENDATION ABOUT THE ALLFILES DUMP AND MICROFICHE.

I FIRST ASKED FOR THE INCLUSIONS ABOUT 6 WEEKS AGO.  I DO NOT HAVE
THE TIME OR THE RESOURCES AT MY DISPOSAL TO DO A ONE-BY -ONE
QUERY OF THESE USERNAMES EITHER.  I AM MORE THAN WILLING TO BE SURE
INFORMATION IS AT LEAST STORED SOMEWHERE BEFORE I PURGE THESE
HOSTS, BUT THEN PURGE I WILL.  MY PAST EXPERIENCES HAVE PROVEN
THAT A FAIRLY SMALL MINORITY WILL NEED TO BE RESTORED, WHICH, GIVEN
PROPER JUSTIFICATION, I WILL WILLINGLY DO.

AS ALWAYS I APPRECIATE AND VALUE YOUR INPUT - IT IS ALWAYS A LEARNING
EXPERIENCE FOR ME.

EILEEN
----------------------------------------------------------------
(end forwarded message)

Just got this reply to my reply which I copied you on.  I'll copy you on
the reply to the reply to the reply, too, when I send it.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 11 Nov 85 12:34:09 PST 
To: William R Soley <wrs@C39.Tymnet> 
Subject: Re: Eileen's reply 

I will refrain from making any comments at the moment.  There really are
directories outside of GANs 1 and 3 that are important to keep around, as
you may have noticed in my message to Eileen.

No doubt there are "some" GAN 1 and 3 usernames which don't absolutely
need to be on every system, but I would prefer that we be left to police
these directories ourselves.  Do you want me to have someone here sort
out the names and make a list of usernames to KEEP?  Then pass it to you
for inclusions you may have... before it gets sent to Eileen.  Sigh!

So little to do and so much time...

/Carl
Received: from C39.Tymnet by X930.Tymnet; Mon, 11 Nov 85 16:03:02 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 11 Nov 85 15:20:56 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Eileen's reply 
In-reply-to: your message of Mon, 11 Nov 85 12:34:09 PST

If you know of anything outside GANs 1, 3 or 4 that you want kept (interpret
"want" however you wish) you should send her something (or me).  Otherwise,
I agree, I think she'll simply keep all GAN 1 and 3 names although I haven't
heard back from her yet.  I wouldn't bother making a list.  -Bill

Re: "So little to do and so much time..." - I think its just the knee of
the performance curve - like Ethernet where the throughput approaches zero
as the load approaches infinity - or the more you have to do, the less you 
get done.  Recent studies have shown that this phenomenon can be triggered
by a vist from your in-laws.  *SIGH*  Moral #1: NEVER have a family reunion
unless the family members can stand being in the same room!

-Bill
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 12 Nov 85 3:48:24 UT
From: NSS.E/RIORDAN@Ontyme.Tymnet 
Date: 11 NOV 85 08:51:46 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: M37419@Ontyme.Tymnet 
Subject: "Thank you Carl. I have already"... 

Thank you Carl.  I have already been duly reprimanded about Gan1 and
Gan3 usernames and am having alist of those usernames provided to me.
I tried for nearly 6 weeks to get people to volunteer to me why they
should remain o those systems.  That list you saw is what was left.
Received: from C39.Tymnet by X930.Tymnet; Tue, 12 Nov 85 13:33:57 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 12 Nov 85 13:27:21 PST 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: CHKPNT and disk space 

Something's wrong.  I was logged in to 25 when it ran out of disk space.
As usual, CHKPNT died.  I called operations and told them to restart it
(as there was now some disk space) but they said they couldn't do any-
thing either.  The AUN that has ACT: assigned is supposed to be exempt
from AC wait.  I logged in to OPER and attached to CHKPNT, ^C, FIN, CORE 0
and even logging out wouldn't unwedge the system.  Something strange was
happening.  Its possible they had more than one CHKPNT running?  Oh well,
they got impatient and 1 in 30 with fast DSKCLN.  Thought you should know.
This happens so often something should be done.  I would think CHKPNT
could recover gracefully as soon as more disk became available.  It
would also be nice if RPG and DIRIT were special cases that never went in
to AC wait so people could delete files when this happens.  That's kind
of gross, but maybe a bit in the RIB that says this program is exempt
from accounting.  -Bill
From: Ken Dawson <Ken@X930.Tymnet> 
Date: Wed, 13 Nov 85 8:17:29 PST 
To: carl 
Subject: REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70 

(begin forwarded message)
Message-id: I75246@Ontyme.Tymnet 
Subject: REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70 

  DATE:     NOVEMBER 7, 1985
  TO:       TYMNET, FIELD SERVICES, PAYMENT SERVICES, TRAVEL
            MANAGEMENT SERVICES, NETWORK TECHNOLOGIES PERSONNEL
            ET AL 
 
  FROM:     EILEEN RIORDAN
  SUBJECT:  REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70
  ==============================================================
     ...
  Text of Eileen's message deleted!
(end forwarded message)

Did you know about this? Are you being violated?
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 15 Nov 85 1:02:39 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Network problems. 

I had difficulty getting to system 74.  The Fremont TYMSAT was not talking to
the supervisor, in that when I typed "JMS:930" or "JMS:74" it just sat there,
and did not ask for password.  I had to go thru Oakland to get the manual
DSKCLN started.  Once it was going, I typed two Control-C's, CCONTINUE, and
DETACH so I could hang up and try the Fremont node 3220 again.  It still
would not let me in as JMS from that node.  But username BUBB worked.
I typed C to get back to TYMNET, and was able to login JMS:74 and attach
to the DSKCLN job OK.

DSKCLN pointed out problems in [1,30773], user TAPELIB.  I GFD'd there,
went to do a DIR/EVERYTHING, and got no response on my terminal.  Tried hanging
up and redialing, had to go thru BUBB again, but got nothing after the
<beep>shut<beep> message.  At this point I called the Data Center and told
them to recycle system 74 with a full DSKCLN.

I had to login thru Oakland again, the Fremont node 3220 is not even talking
to BUBB now.

/JMS


From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 15 Nov 85 2:09:23 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: F3XP06 

Recompiled all REL files, same checksum as the other two tries.  Still
does not pass license when MDDT creates new filddt.  Hmmm.  Will have to
check further.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 15 Nov 85 23:20:30 UT
From: IPC.V/HIATTBOCK@Ontyme.Tymnet 
Date: 13 NOV 85 10:12:14 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A60777@Ontyme.Tymnet 
Subject: Accounting databases Admit list 

                             M E M O R A N D U M                                
              SOFTWARE DISTRIBUTION AND VALIDATIONS SUPPORT                     
                                                                                
                                                                                
DATE> November 13, 1985                                                         
                                                                                
TO> Carl Baltrunas                                                              
    Bill Soley                                                                  
                                                                                
FROM> Vivian HiattBock                                                          
                                                                                
SUBJECT> Accounting databases Admit list                                        
                                                                                
--------------------------------------------------------------------            
                                                                                
     We are cleaning up the ADMIT lists for the accounting databases            
in CUD10 on system 74.  All personal name id's are being removed to             
make GFD`ing to a known ADMITted user more difficult.  My                       
suggestion is that you (Carl and Bill) choose a generic userid name             
that will be kept secure because it will have UPDATE privelege.                 
This will be the only userid with UPDATE outside of the Validations group.      
                                                                                
     I will be talking with you later about finding a safe id for the           
owner of the databases.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 19 Nov 85 19:19:51 PST 
To: Ken Dawson <ken@X930.Tymnet> 
Subject: System 24 (noon to midnight) 

System 24 is supposed to be available from noon to midnight!
Don't put your only copy of anythig there, but feel free to hammer
on it a little.  We may wipe things out once or twice in the near future
so feel free to work then telecopy it off when done... sigh!  Only worry
if we tell you something of a disaster... sigh!

I validated KEN, KDAWSON, PEAK, KLIB, KDEV1 for your use.  If you need more
check with me first so you will NOT get zapped.  PS: Don't use TOO MUCH
space, but feel free to use some.  /Carl
Received: from C39.Tymnet by X930.Tymnet; Tue, 19 Nov 85 19:57:19 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 19 Nov 85 19:54:41 PST 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: 24? 

Do I take it you'd like TUMS up on 24?  ...
44  DETACHED      TELECO 46       TI       321.45  0:00:00 MAIL +$
044TEL  COM       2  19-NOV-85 (CARL)
-Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 19 Nov 85 20:42:37 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dencoff, dernst 
Subject: RPORTN on system 95, micronode 6760. 

The off-by-one discrepency between the PDP-10 and its base also affects the
2020.  I noticed that the micronode was assigning port numbers starting at
17 when logging into 95, so I build a monitor with RPORTN=^D16.  That is,
one that would only accept port numbers 0-17 (octal, or 0-15 decimal) from
6760.  The monitor got two INFO stopcodes TTYBZP when it came up.  In each,
the message type was 43 (date/time from supervisor) and the port number was
20 (octal).  The system came up fine, and allowed me to login on TTY17 (octal).

The monitor 95XP07.SAV is currently built with RPORTN=^D17 so that it can
handle 16 real ports (00 to 17 octal) and one fake port TTY20 for date/time.

The micronodes on the Paris machines appear to assing port numbers starting
with 37 octal and working down, and never use ports 40 to 77.  Reducing RPORTN
for those 2020s would add to available free core.

/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 20 Nov 85 0:17:05 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: P07 works. 

In P06 I commented out most of the references to UNYPPL, the number of blocks
that have not been transferred when error recovery was initiated.  In P07 I
deleted all the code that referenced UNYPPL or was called when UNYPPL was
nonzero.  Around midnight I made a P06+ monitor, using P06 sources except for
the P07 version of FILIO.  This monitor does run MDDT correctly.

Appearantly there is at least one path from APXKON to FILINT that does not
have a LSH T1,2 and left garbage in the low-order two bits.  I cannot explain
why FILDDT under FRMRUN could not read (SYS)SYSTEM.SAV but could when run
manually.

/JSM
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 20 Nov 85 21:26:02 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: DISKETTES WITH GAMES ON IT 

...ARE IN DAN'S OFFICE ON HIS DESK.  I SUSPECT
HE CAME AND GOT THEM, BUT YOU'LL HAVE TO ASK
HIM THAT.  LET ME KNOW WHEN YOU GET ANY OR ALL
OF THE 2 DISKS COPIED INTO RAINBOW FORMAT.  WE
CAN ALWAYS LOOK AT THE SOURCES... I'D LIKE TO
SEE THEM ANYWAY... MAYBE CONVERT TO SAIL ON THE
PDP-10... SIGH!
/CARL
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 22 Nov 85 0:20:18 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dencoff 
Subject: PAKCOPY 

I plan to test PAKCOPY on system 24 on Friday.  There are three versions.
See (M33)PAKCOP.LOG for details.

/JMS
From: Ken Dawson <Ken@X930.Tymnet> 
Date: Mon, 25 Nov 85 9:05:49 PST 
To: carl 
Subject: c24 

    When the time comes, and :24 can stay up for more than a few seconds at
    a time, would you please insure that it has an up-to-date version of
    the oper password file?

    Thanks,

    /ken
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 25 Nov 85 23:40:12 PST 
To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: 24's OPER password file 

I have *NOT* moved it over from 930 as of this moment.  I was in the
process of moving the file just now and the host became un-available
through the net.  (Probably, base crashed?)  If either of you wish to
move the file, feel free... just send mail.  The file is:
  (SYS)DRWSPR.EPO

Ciao.  /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 26 Nov 85 0:31:53 PST 
To: Dennis Coffey <dencoff@C26.Tymnet>, Carl A Baltrunas <Carl@X930.Tymnet>,
	Osman Guven <osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Subject: 24's OPER password file 

System 24 now has the OPER password file from system 930.  Anyone who
is valid on system 930 is now valid on 24.  Anyone who knows of any OPER
names that need to be present on OUR development system should let me
 BEFORE they add the names.

Also, OSMAN & JOE, please put any names we "allow" on our system other
than those for BLDG-C Operations under TXS.WATCH    -Thanks
/Carl
Received: from C26.Tymnet by X930.Tymnet; Tue, 26 Nov 85 4:02:32 PST
Return-path: <CARL@C26.Tymnet> 
From: CARL@C26.Tymnet 
Date: Tue, 26 Nov 85 2:40:49 PST 
To: dencoff 
Subject: PAM 

Dennis,
  I think the problem with PAM is not that it is looping because it
cannot get a segment or any such problem like that.  It is possible
that such a loop exists and if so found, we should fix it... but I am
pretty sure that many of the high-segments, at least ONE anyway, do
the following:

    X: HRLZ    AC,PORT            ; get port,,input image char & skip
       HRRI    AC,.AXI8S
    Y: AUXCAL  AC,CHAR            ; read character if one is ready
        JRST   Y                  ; no character, try again
       ....                       ; got character, continue

I found it in a running PAM job on system 34 which was killing the
system by running "wild".  The PC was at 400247 which is at program
break 237' or 400237'  since 400010' is  the starting point for either
0' or 400000' as found in most program listings.

/Carl

Received: from C36.Tymnet by X930.Tymnet; Tue, 26 Nov 85 17:03:38 PST
Return-path: <DennisCoffey@C36.Tymnet> 
From: Dennis Coffey; 116, X2588; 39100 Liberty St., Fremont <DENCOFF@C36> 
Date: Tue, 26 Nov 85 16:40:58 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Your recent LINK message. 

Hi,

    I saw your LINK message, belatedly.  I was logged-in, through the MUX, to
both 26 and 36.  I'm moving my 'home' to 36--NTD does not load a host down
nearly so badly as Tymshare Op's.  You probably tried to send the mail while I
was in the middle of the move, which might explain why it was not read.

    Re.  PJ:  I noticed the TIGHT loop in the code I had looked at, and
wondered.  I thought that code might be some MACRO magic I didn't know about.
I'm pleased to know that I was right about the loop (pleased because my
instincts about such a poorly 'tested' loop were right), but I'm also saddened
to see such code in software so important to the 'Tymshare' X's.

    Thanks for looking at PJ!  I thought it might not be TOO hard to replace.
I'm glad you see it that way.  I will have it documented, but not immediately.

    A pleasant Thanksgiving to you and Cherie (sp?)!

Dennis
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 27 Nov 85 14:18:54 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dencoff 
Subject: (M33)NOINTE.PAT 

Patch #4 is for NO INTERRUPT ENABLE during JOB, INFO, and DEBUG stopcodes.
It disables the CONO PI instruction that re-enabled channel 1 interrupts for
these stopcodes.

/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 27 Nov 85 20:37:17 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dencoff 
Subject: (TYMNET)BASES. 

The file (TYMNET)BASES has a list of PDP-10 base node numbers as of today.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 27 Nov 85 23:05:08 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dencoff 
Subject: 24XP09 

I copied 24XP09 to NEWMON.  This monitor is designed to handle 3330's in
pages.  I will go to CUPC Friday to test it.

/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 29 Nov 85 18:41:51 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: X930 back up. 

I left the disks powerred off while I went to dinner, turned them back on when
I got back, and the system came up OK.

/JMS
P.S. You can delete the messages for you on systems 24 and 35.
Received: from C24.Tymnet by C930.Tymnet; Fri, 29 Nov 85 19:01:02 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 29 Nov 85 12:24:28 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dencoff 
Subject: X930 down with disk problem. 

When I tried to LOGIN to 930 today, the monitor said "?LOGINN NOT FOUND".
Found all units with ready light on, but APA2 had "hardware indications on
drive".  System 930 is down, needs disk maintenance.
Received: from C24.Tymnet by C930.Tymnet; Fri, 29 Nov 85 19:43:57 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 29 Nov 85 19:42:43 PST 
To: Osman Guven <osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Base and/or system 24 losing characters. 

(begin forwarded message)

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 29 Nov 85 19:38:47 PST 
To: ntd.ds@ontyme 
Subject: Base and/or system 24 losing characters. 

System 24 (the eh0erimental 3350 machine) is sending garbled characters.
Sometimes ">" comes out as ".", "v" as "f!", and sometimes the columns output
by DIR don't line up because spaces are getting lost.  At this point, I can't
tell if C24 is flakey, or if it's base is acting up.

Just thought you'd want to know.
/JMS

(end forwarded message)

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 29 Nov 85 20:26:19 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Where did the old files in M33 come from? 

There are 4000 pages in files in (M33) that were created before 1-Jan-85.
Where did they come from?  They are using up all the free space on 930.
I thought we renamed those files to (OSARC).

/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 1 Dec 85 16:28:38 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: P09 works in pages on C24. 

The good news is that I formatted, refreshed, and restored (SYS) to a one-pack
system on C24, using 3675 formatted in pages.  The bad news is that BOOTS does
not know how to read it.  I had to have the "blocks" pack on-line to get BOOTS
and the monitor (SYS)3675.SAV from it.  This test monitor then decides that the
"blocks" pack is improperly formatted, ignores it as being off-line, and only
looks at the "pages" pack.  Right now, the "pages" pack on BPA0 has only SYS:
from C34 and a few .SAV files in M33.

The next step is to jumper the 3650 for native mode, run (SYS)PAKCP5 from the
"blocks" pack to format the 3650, and use (SYS)3650.SAV to refresh it.
/JMS
From: Osman Guven <osman@C930.Tymnet> 
Date: Mon, 2 Dec 85 8:50:25 PST 
To: jms, Carl A Baltrunas <carl@C930.Tymnet>, fletcherc, dencoff 
Subject: my schedule today.. 

I have a very bad cold, will be working from home.
If any body needs me here are the numbers:
Home(408)224-5457....Pager(408)977-7961 & (415)498-3841
P.S. I have been on the phone with Fremont D.C. since 0600 am.
     this mornning...F58(new base) and F74(disk).
Osman..
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 4 Dec 85 23:17:32 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, WRS 
Subject: TOPS-10 KL paging. 

If you're curious as to how TOPS-10 7.02 sets up the EPT to use KL-paging,
the information is in the INFO tree under KL10EPT.		/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 5 Dec 85 16:06:44 PST 
To: Osman Guven <osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 
Subject: N10 patch 

I believe the source-code change was to add SETOM MULCNT+0 just after
SETOUT+1 in SCNSER.  P034.MEM says the change was added 30-Jul-85.
From: CTMKTG7@X930.Tymnet 
Date: Fri, 6 Dec 85 11:02:15 PST 
To: FLETCHERC, CARL, JMS 
Subject: URGENT MSG ON ONTYME 

COULD YOU PLEASE CLEAR YOUR MAIL ON YOUR TXS ONTYME
ACCOUNTS ? THX, PETER STRATMAN..
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Sat, 7 Dec 85 7:24:05 UT
From: CT.DPM@Ontyme.Tymnet 
Date: 06 DEC 85 10:19:09 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.C/FLETCHER@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.J/SMITH@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet,
	TXS.SUP@Ontyme.Tymnet 
Message-id: A77745@Ontyme.Tymnet 
Subject: "E M O R A N D U M------------------------------"... 

 
 
 
 
 
  -----------------------M E M O R A N D U M------------------------------
  MDISF                             telephone  ST-CLOUD [33](1)46.02.70.12
 
  ------------------------------------------------------------------------
 
  Date:    December 6th, 1985 - 7:16 p.m. CST
 
  To       Craig Fletcher (txs.c/fletcher)
 
  Copies   Carl Baltrunas (txs.c/baltrunas)
           Joe Smith (txs.j/smith)
           Jack Snavely (netops.j/snavely)
           Jean-Francois Guillou (ct.dpm)
           Francis Bonnin (ct.dpm)
           txs.sup
 
  From     Peter Stratman (ct.dpm)
 
  Subject  Tymcom-X monitor urgently required for S90 (P034P).
 
  ------------------------------------------------------------------------
 
 
  We are currently involved in the process  of installing a Tymnet II base
  on  S90 (the  PDP10  installed here  in St.  Cloud).   Jack Snavely  has
  informed  us of  the  fact  that one  of  the  problems that  have  been
  detected, an  intermittent loss  of characters,  has been  solved on  US
  PDP10's by installing a recent release of the operating system.
 
  As this is one of the reasons why we have reverted to the Tymnet I base,
  and because of the  fact that we would like to test  the new software as
  much as possible, we would like to have the  new monitor release for S90
  very soon.
 
  It may, however, also be necessary  to update other system software. S90
  is currently still  running on a /K  monitor, and a lot  of software may
  have changed since. I  am not aware of what changes  may be necessary to
  move to a  /P monitor, (CUSPS, operator  procedures, user documentation,
  APL-related, etc).
 
  Could you give me the information I  will need to plan this upgrade cor-
  rectly ?  Tentatively, given  the current Tymnet  I elimination  date of
  Dec. 15th,  Tuesday night, Dec. 12th.   our time, seems  appropriate for
  the monitor  upgrade. (this  means the  day of  Dec. 12th.,  your time).
  Could you please confirm this schedule as soon as possible ?
 
  I can be contacted via ontyme on  ct.dpm, or mail to CTMKTG7:930, if you
  need more information or assistance.
 
  Best regards, Peter Stratman.
                                    -1-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 8 Dec 85 21:22:56 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: 3650 progress 

To Craig and Dennis: Recap of Friday's testing.

  We tried using PAKCP5 (a modified version of PAKCOP) to format the 3650s
in native mode in pages.  This failed when it got to cylinder 542.  It did
however format the 5 alternate cylinders (559-555) and some of the regular
cylinders (554-543) before dying with "NO RECORD FOUND" for record 0 on
cylinder 542.  The REFRESH command, which usually clears problems like this
by resetting the assignment of alternate cylinders, could not reset RECORD 0
HEAD 0 CYLINDER 0.

  Osman brought up the SA10 diagnostics, formatted the 3650 in native mode
blocks.  I disabled the test in ONCE that checks for the special TYMSHARE
format on the disk, and we are now up running on the 3650 in native blocks.

To Osman and Carl: Why PAKCOP failed.

  When the 3650's were formatted in compatibility moded, the 815 logical
cylinders used physical cylinders 0-542 and half of 543.  The RECORD 0 (R)
and HOME ADDRESS (HA) information on these cylinders was correct for 3675
compatibility mode, but wrong for 3650 native mode.  PAKCP5 had no problem
formatting cylinders 559-544 since they still had the correct HA/R0 info
from the factory.  The question we had about the SET FILE MASK byte was a
red herring, octal 300 gives the most privileges, setting other bits disables
certain SEEK commands.

  The problem is that the commands PAKCOP uses to refresh HA and R0 on the
3675 and 3650 in compatibility mode get COMMAND REJECT when attempted on a
pack in native mode.  The 3674 manual states that the WRITE HOME ADDRESS
has an additional requirement for 3650 native mode - it must be proceeded by
a successful SEARCH HOME ADDRESS command.  That is:

	SEARCH HOME ADDRESS
	TIC back if search failed
	WRITE HOME ADDRESS
	WRITE RECORD 0

  The SA10 diagnostics did this when it formatted the 3650 in native blocks.
Now that valid HA and R0 info is on the disk, all we have to do is run the
existing PAKCP5 to format it in native pages.

/JMS
From: Osman Guven <osman@C930.Tymnet> 
Date: Mon, 9 Dec 85 8:52:14 PST 
To: Joe Smith <jms@C930.Tymnet>, Carl A Baltrunas <carl@C930.Tymnet>, dencoff,
	fletcherc 
Subject: 3650 Project.. 

Joe, I am in BLDG C today and I read you mail about formatting.
To see if everything works the way we are predicting, i will
go ahead and see if i can build a PAGE MONITOR/NATIVE MODE test system.
Osman..
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 10 Dec 85 19:26:14 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: BOTLOD & BOOTS are ready to list 

See (M33)*.LST/AFTER 1-DEC-85
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 10 Dec 85 19:34:28 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: BOTLOD 

Created BOTLOD.INF and BOTLOD.CMD, added comments and PRINTX to BOTLOD.MAC
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 6 Dec 85 7:49:05 UT
From: TYMOPS.J/SNAVELY@Ontyme.Tymnet 
Date: 06 DEC 85 00:06:55 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M42915@Ontyme.Tymnet 
Subject: TYMNET I PROJECT SCHEDULE 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:26:34 PST
Resent-To: Carl A Baltrunas <carl@X930.Tymnet>

TO:      DISTRIBUTION

FROM:    JACK SNAVELY


SUBJECT:  TYMNET I PROJECT SCHEDULE


THE SOFTWARE PROBLEMS WHICH HAVE DELAYED THE CONVERSION ARE BEING RE-
SOLVED AT A RAPID PACE.  WE EXPECT RESOLUTION OF THE REMAINING PROBLEMS
BY THIS WEEKEND.  IN ORDER TO MAKE THE CONVERSION AS ORDERLY AND 
QUICK AS POSSIBLE, WE ARE SCHEDULING IMPLEMENTATION OF THE NEW SUPER-
VISOR FOR 3:00 A.M. SUNDAY MORNING, 12/15.  THIS WILL ALLOW ONE WEEK
FOR CONVERSION, WHICH SHOULD BE SUFFICIENT FOR THE FEW REMAINING NODES.

CURRENT STATUS OF SOFTWARE PROBLEMS ARE:

940 NET - THE AUX CIRCUIT PROBLEM WHICH IMPACTED VALIDATIONS ON MONDAY
          HAS BEEN RESOLVED AND TESTING IS CONTINUING.

E-BUSS - NEW CODE IS BEING LOADED NOW WHICH FIXES THE FOLLOWING PROBLEMS:

         1.  INPUT PROCESSING BEING STOPPED

         2.  LOGINS FROM X.25

         3.  LOGIN FROM HSA

         THE PROBLEM OF LOST OUTPUT DATA IS A BUG IN AN OLD VERSION OF 
         THE MONITOR ON SOME OF THE ADP 10'S.  THE FIX IS TO LOAD THE 
         LATEST VERSION OF THE MONITOR.

HSA KATAKANA - NEW VERSION WHICH FIXES ALL KNOWN PROBLEMS.

GATEWAY - NEW VERSION RELEASED WHICH FIXES:

          1. LOADii PROBLEM

          2. CRASH FROM 6C63

I HOPE I HAVE ADDRESSED ALL OF THE KNOWN PROBLEMS.  IF THERE ARE ANY NEW
CONCERNS, PLEASE RESPOND IMMEDIATELY.  ITIME IS SHORT, AND CONVERSION 
BECOMES MORE CRITICAL WITH EACH PASSING DAY.


THANK YOU FOR YOUR SUPPORT AND ASSISTANCE IN THIS PROJECT,

JACK SNAVELY
From: TYMOPS.J/SNAVELY@Ontyme.Tymnet 
Date: 27 NOV 85 22:22:27 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M41605@Ontyme.Tymnet 
Subject: TYMNET-1 ELIMINATION 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:27:50 PST
Resent-To: Carl A Baltrunas <carl@X930.Tymnet>

                     T Y M N E T    O P E R A T I O N S

DATE>      27 NOV 85  13:51

TO>        ALL TYMNET PERSONNEL

COPIES>    INTERNATIONAL AND FIELD SERVICES PERSONNEL

FROM>      JACK SNAVELY


SUBJECT>   TYMNET-1 ELIMINATION


-----------------------------------------------------------------------


THE TYMNET-1 ELIMINATION SCHEDULED FOR SUNDAY DECEMBER 1 HAS BEEN DELAYED.

A SCHEDULE FOR COMPLETION WILL BE PUBLISHED ON MONDAY DECEMBER 2


THIS DELAY IS DUE TO UNRESOLVED SOFTWARE PROBLEMS ENCOUNTERED IN THE
FINAL STAGES OF CONVERSION.  SINCE EVERYONE IS PREPARED FOR THIS CONVERSION,
ADVANCE NOTICE COMPLETION WILL BE SHORT.  I EXPECT ABOUT TWO DAYS NOTICE
WILL BE SUFFICIENT.

PLEASE BE READY.
From: IOD.BEDELL@Ontyme.Tymnet 
Date: 02 DEC 85 16:47:36 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Cc: FSC.D/COFFEY@Ontyme.Tymnet, IPC.B/JACOBS@Ontyme.Tymnet,
	FSC.C/BALTRUNAS@Ontyme.Tymnet, CT.TECHSERV@Ontyme.Tymnet,
	IPC.J/KRIVANEC@Ontyme.Tymnet, IOD.CUPERTINO@Ontyme.Tymnet,
	FSC.@Ontyme.Tymnet 
Message-id: A74286@Ontyme.Tymnet 
Subject: Request for assistance 
Resent-From: Carl A Baltrunas <Carl@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:40:45 PST
Resent-To: Carl A Baltrunas <Carl@X930.Tymnet>

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1985-12-02  16:29 PST                  Telex 4993666 MDISI CPTO
 
TO>        Dennis Coffey (FSC.D/COFFEY)
           Bob Jacobs (IPC.B/JACOBS)
 
COPIES>    Carl Baltrunas (FSC.C/BALTRUNAS)      Eric Jay (CT.TECHSERV)
           Jan Krivanec (IPC.J/KRIVANEC)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Request for assistance
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     I am requesting your assistance in order to supply some answers to
     Cegi Tymshare (France) as it relates to an accounting project they
     have underway.
 
     CT plans to do their own accounting for hosts which belong to them
     and are located in St-Cloud, France [hosts S90 (KL10), S51 & S53 (IBM
     4341)].  This will allow our MIS Dept. to discontinue month-end
     accounting for these hosts.  The statements and questions below
     have been taken directly from a memo sent to me by Eric Jay, and
     related to Tymcom X, Validations and the Tape library.
 
     I have spoken with Jan about the validation issues; Jan, CTOPSMGR
     is already valid on host 74.
 
     Regarding item b. The 2 user names are valid on 930 and OPER names
     exist also.  I am interested in your comments about Perp, Telecopy
     and PCOM.
 
     Bob, I require your comments about item d.
 
     The actual message follows:
 
     b.  TYMCOM-X:
 
      o   Raw accounting files generation and  format as documented in
          the "Info"  database on system 930.  For this we  need user-
          names:  CTMKTG1  and CTMKTG7 to  remain validated  on system
          930 and oper names STRATMAN and  JAY valid with a READ-FILES
          license

 
Request for assistance                                              Page 2
 
 
 
      o   PERP, TELECOPY, PCOM
 
          Our new  accounting system needs  these utilities  to remain
          supported  (by  MDC)  or replacements  made  available  with
          advanced notice or early warning of their discontinuation
 
     c.  For general validation information we need
 
      o   Continued  access  to  X22 databases  on  system  74  called
          (CUD10)USERDB.DMS and (CUD10)ADDRDB.DMS
 
      o   Username CTOPSMGR validated on system 74
 
      o   Read-files license on system 74
 
      o   advanced notification  of any changes or  discontinuation of
          service that might happen on the above X22 databases
 
     d.  Tape library database
 
      To perform Tapes accounting, we need  access to the central Tape
      library on host 74 (username TAPELIBE, X22 files) and notification
      of changes or discontinuation of service.
 
     ** end of partial message from CT **
 
     I look forward to your comments on the above, either via Ontyme or a
     phone call.  Best regards. Ken
From: Osman Guven <osman@C930.Tymnet> 
Date: Wed, 11 Dec 85 11:16:44 PST 
To: fletcherc, dencoff, Carl A Baltrunas <carl@C930.Tymnet>, jms 
Subject: Vacation schedule.. 

I will take 12/12 & 12/16 & 12/18 as vacation days and couple of
half day copm time next friday and tuesday. I will be in library
studying for the final exams. if you need anything leave mail on
system or phone message at home.
Osman..

Received: from C39.Tymnet by X930.Tymnet; Wed, 11 Dec 85 11:35:30 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 11 Dec 85 11:30:22 PST 
To: carl@930, jms@930 
Subject: TYMNET I PROJECT SCHEDULE 

(begin forwarded message)
----------------------------------------------------------------
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Wed, 11 Dec 85 17:06:00 UT
From: NTD.SUP@Ontyme.Tymnet 
Date: 11 DEC 85 09:00:29 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I88863@Ontyme.Tymnet 
Subject: TYMNET I PROJECT SCHEDULE 

TO:      DISTRIBUTION

FROM:    JACK SNAVELY


SUBJECT:  TYMNET I PROJECT SCHEDULE


THE SOFTWARE PROBLEMS WHICH HAVE DELAYED THE CONVERSION ARE BEING RE-
SOLVED AT A RAPID PACE.  WE EXPECT RESOLUTION OF THE REMAINING PROBLEMS
BY THIS WEEKEND.  IN ORDER TO MAKE THE CONVERSION AS ORDERLY AND 
QUICK AS POSSIBLE, WE ARE SCHEDULING IMPLEMENTATION OF THE NEW SUPER-
VISOR FOR 3:00 A.M. SUNDAY MORNING, 12/15.  THIS WILL ALLOW ONE WEEK
FOR CONVERSION, WHICH SHOULD BE SUFFICIENT FOR THE FEW REMAINING NODES.

CURRENT STATUS OF SOFTWARE PROBLEMS ARE:

940 NET - THE AUX CIRCUIT PROBLEM WHICH IMPACTED VALIDATIONS ON MONDAY
          HAS BEEN RESOLVED AND TESTING IS CONTINUING.

E-BUSS - NEW CODE IS BEING LOADED NOW WHICH FIXES THE FOLLOWING PROBLEMS:

         1.  INPUT PROCESSING BEING STOPPED

         2.  LOGINS FROM X.25

         3.  LOGIN FROM HSA

         THE PROBLEM OF LOST OUTPUT DATA IS A BUG IN AN OLD VERSION OF 
         THE MONITOR ON SOME OF THE ADP 10'S.  THE FIX IS TO LOAD THE 
         LATEST VERSION OF THE MONITOR.

HSA KATAKANA - NEW VERSION WHICH FIXES ALL KNOWN PROBLEMS.

GATEWAY - NEW VERSION RELEASED WHICH FIXES:

          1. LOADii PROBLEM

          2. CRASH FROM 6C63

I HOPE I HAVE ADDRESSED ALL OF THE KNOWN PROBLEMS.  IF THERE ARE ANY NEW
CONCERNS, PLEASE RESPOND IMMEDIATELY.  ITIME IS SHORT, AND CONVERSION 
BECOMES MORE CRITICAL WITH EACH PASSING DAY.


THANK YOU FOR YOUR SUPPORT AND ASSISTANCE IN THIS PROJECT,

JACK SNAVELY
----------------------------------------------------------------
(end forwarded message)

Re: bug in old monitor for character loss on output
Is this true?  It still fails on 39 as of yesterday afternoon.
-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 11 Dec 85 12:38:10 PST 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: TYMNET I PROJECT SCHEDULE 
In-reply-to: your message of Wed, 11 Dec 85 11:30:22 PST

Re: BUG IN OLD MONITOR?
This was the problem that you found with MULCNT being off and we've
patched that on those systems that aren't running the new monitor.

If it is still occurring on 39, I suspect there may be another problem
either with SCNSER or the new base... can you VERIFY that it is dropping
2 characters as in the problem you discovered?

/carl
From: CTMKTG7@X930.Tymnet 
Date: Thu, 12 Dec 85 13:34:51 PST 
To: CARL 
Subject: PATCH TO S90 MONITOR. 

CARL,

SORRY FOR GETTING BACK SO LATE. YOUR PATCH WORKS OK ON S90.
WE HAVE OTHER PBS. THOUGH. DO YOU KNOW IF OTHER MONITOR
RELATED PROBLEMS HAVE BEEN DETECTED AND CORRECTED IN POST-K
VERSIONS ?
THANK YOU, PETER.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 12 Dec 85 23:52:41 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: P034/P10 

The monitor 24XP10 should be able to determine what kind of disk it is
running on, and reacts nicer to DEPOSIT 1 IN 30.  It hasn't been tested yet.
/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 13 Dec 85 0:24:16 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: 24XP10 

Does it handle more than one type of disk?
Do we have to ONLY have one type spinning at a time?
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 13 Dec 85 0:28:11 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Craig
	Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet> 
Subject: Re: 24XP10 
In-reply-to: your message of Fri, 13 Dec 85 0:24:16 PST

All the information about a disk is stored in the Unit Data Block.  Each
disk has its own UDB.  The information is set up for each disk, one unit at
a time at ONCE-only time.  Therefore you can have an RP06 and an RM03 on line
at the same time on a 2020, and can build a two-pack DSKB from them (but it
is not a good idea to have different types of disks in one structure).

BPXKON has always allowed a mixture of 100 Mb and 200 Mb disks on the same
string.  The capacity test would determine whether the disk had 404 or 808
cylinders, and the Monitor would happily use whatever was out there.

I had to remove the test for 3330-I (100 Mb) to fit 3650 definitions into a
two-bit field.  24XP10 is designed to recognize the following types of disk:
0 = 3350 in blocks (we won't be using this format other than for testing)
1 = 3330 in blocks (this is what all our systems currently use)
2 = 3350 in pages  (this is the approved format for 3650 disks)
3 = 3330 in pages  (to get 11% more our of existing disks)

This means, however, if both 3650 spindles are on-line, and they both say
they are DSKB0, then the Monitor will complain because it will see both,
even if one is formatted in native pages and the other is not.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 13 Dec 85 1:47:57 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: 24xp10 

thanks for the clarification....
  so all that is needed in the monitor NOW is the REFSTR stuff
to assign physical tracks to the monitor.  Right?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 16 Dec 85 23:54:34 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Meeting today with Ed Roop 

Since I've not heard anything, I am going to assume that we ARE NOT
having a meeting with Ed.  If this changes, please call me at home and
leave a message for me on the recorder... also send mail...  I have to
drop Ed off at the airport at 12 for a 1:00 flight.  So let me know if
the meeting is on or off.  Thanks.
/Carl
From: Osman Guven <osman@C930.Tymnet> 
Date: Tue, 17 Dec 85 11:43:52 PST 
To: Carl A Baltrunas <carl@C930.Tymnet> 
Subject: Give me call.. 

Please give me a call whe you get in (415)794-2513
I am getting calls from customers compleaning about BIONIC..
I know you were working on some solution to the problem
BIONIC/NEW BASE having. I am not sure how long I am goning
to be here but here is the gals number (612)339-4401 Dana Cuarmey
who needs help..I will not be in tomorrow vacation day..so see
you thursady..
Osman..
Received: from C39.Tymnet by X930.Tymnet; Thu, 19 Dec 85 14:51:04 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 19 Dec 85 14:48:31 PST 
To: carl@930 
Subject: SYSTAT ILL MEM REF 

!sysn
TYMSHARE D23-P034/N10-2 5-10-85
!daytime
19-DEC-1985 14:48:05 PST

!sys (oper)
 1  *2157* #174   SYSNO  24       ^C      1255.46 13:48:34 OPER +
 2  DETACHED      CHKPNT 6        TI       264.01  0:00:02 OPER *+$

 2  121985 SAT 393   DUMP UN1         OPER
 2  121985 DAT 19    DUMP SYS         OPER

 2  ACT
ILL MEM REF for location 365441 at user 410265

!
----------------------------------------------------------------
This appears to be repeatable, although it may be
dependent on the state of the monitor data structures.
-Bill
Received: from C24.Tymnet by X930.Tymnet; Thu, 19 Dec 85 17:11:42 PST
Return-path: <osman@C930.Tymnet> 
From: Osman Guven <osman@C930.Tymnet> 
Date: Thu, 19 Dec 85 16:59:24 PST 
To: carl@930 
Subject: 24XP10 Status.. 

Joe..

24XP10 Doesn't run with neither 3650/NATIVE/PAGE or 3650/SIMULATION/BLOCK.
With 3650/NATIVE/PAGE can't pass the reading the home page.
With 3650/SIMULATION/BLOCK just before start running DSKCLN (after
printing disk drive status) just dies w/o any out put.
Osman..
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 28 Dec 85 0:42:59 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: ? Vacation ? 

Yes, I know I'm (we're) supposed to be on vacation.  Somebody forgot to
tell that to the calendar.  January 1, is fast approaching.  I plan to keep
track of my time and we can work it out later.

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 29 Dec 85 17:49:17 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: Me 

I'm back and my beeper is on.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 30 Dec 85 14:58:13 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: system 39 username purge 

Someone in NTD sent validations a request to delete a list of users not
in NTD.  The list was in progress when Bill Soley noticed that (SPL) had
disappeared and he went to validations and stopped the purge...  I went
to check on 39 and found username (CARL) gone, and called validations to
see what happenned.

In all, 12 GAN 3 usernames were purged.  Bill and I made a list and gave
it to Jan Krivanek to have the accounts restored.  So, if you notice that
things on 39 are missing, ** Don't Panic ***,  they are being restored.

Of concern were: (BRING), (CARL), (DENCOFF), (FLETCHERC), (HELGEM), (JMS),
(SPL) and (TSUCOOP).  There were others, but of no consequence.

Jan assured me that she would put in the batch rquests to have operations
restore the file that were deleted.
/Carl
Received: from C36.Tymnet by X930.Tymnet; Tue, 31 Dec 85 14:31:59 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Tue, 31 Dec 85 14:07:06 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Support Request 

Carl,
I've received a call from Tymnet's International Customer Support office. in
Vienna, VA, re. a problem the customer is seeing on host 65.  I don't klnow
where the problem is originating  It could be in a gateway's code, the new
bases' code, or in the Monitor.

The problem's circumstances:  a user, logging in to host 65 can't complete the
login procedure and gain access to the host.  The users reporting the problem
are in either Austria or Singapore.  I believe, since the one I know has the
same UUN as Otis Elevator, the host is dedicated to Otis and the two users
have such very similar usernames, that the users are Otis branch offices.  The
user makes it through the gateway, successfully.  The user successfully gets
past the 'please log in: ' and the 'password: ' parts of the logon.  When they
get to the prompt for 'project code: ' they immediately get logged off the
host, bofore they can input anything.  This occurs in a manner which causes
the gateway to report to the terminal that the breaking of the circuit came
from the host.

This may be coming from any of a number of causes.  I don't know where to
begin.  If you could give me some hints, or some directions to begin my search
for causes, I'd surely appreciate it.

                                                                    Thankjs!!!
                                                                    Dennis
Received: from C36.Tymnet by X930.Tymnet; Thu, 2 Jan 86 15:05:13 PST
Return-path: <CARL@C36.Tymnet> 
From: CARL@C36.Tymnet 
Date: Thu, 2 Jan 86 15:03:39 PST 
To: dencoff 
Subject: Otis problem 

It is probable that there is a project-code validations program setup
for the users in question and that somehow the program is not working.
If you'll give me the usernames I can look into it further to see if
this is indeed the problem.   /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 2 Jan 86 15:15:27 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: 24 still "Host not available through net" 

Send me mail when it's up...  /Carl
Received: from C36.Tymnet by X930.Tymnet; Thu, 2 Jan 86 15:31:53 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Thu, 2 Jan 86 15:19:23 PST 
To: CARL@C36.Tymnet 
Subject: Re: Otis problem 
In-reply-to: your message of Thu, 2 Jan 86 15:03:39 PST

Damn, I thought you were on vacation!  Are you ever going to get any real
vacation?

Re. Otis username:  I assume you know that there is no problem when the
username is logged into from within Tymnet, but the problem only occurs when
the user comes through a gateway.  (The preceding was just a way of telling
you those facts in case you hadn't heard them---in case the assumption was
wrong.  Your reply seems to show that my telling you was unnecessary.)

The only username I know to be involved in the problem is 'LOC358' on host 65.
Received: from C36.Tymnet by X930.Tymnet; Thu, 2 Jan 86 16:59:20 PST
Return-path: <CARL@C36.Tymnet> 
From: CARL@C36.Tymnet 
Date: Thu, 2 Jan 86 16:55:33 PST 
To: dencoff 
Subject: re: Otis LOC358 

I can't find anything wrong at the host end.  LOGINN should not
be zapping them.  They have no project-code validation in effect
for this username, and the INIT program simply types out a msg
on their terminal.  We need a password to try further attempts
in reproducing the problem.  I suspect they should check with
tymnet about where their circuits are being zapped, but first
get more info... eg. what gateway, can we gateway into their
private net then out again as them?  ... what usernames,passwords
etc.   /Carl
Received: from C36.Tymnet by X930.Tymnet; Fri, 3 Jan 86 9:22:51 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 3 Jan 86 9:09:01 PST 
To: CARL@C36.Tymnet 
Subject: re: Otis LOC358 
In-reply-to: your message of Thu, 2 Jan 86 16:55:33 PST

Re. passwords--into a private net to enter the public network via an X.75
gateway, then into the username with the problem:  I have been given some
usernames and passwords, not exactly what you wanted, but similar to what you
suggest.  I'll not write them down--please call me if you want to try
duplicating the problem, and I'll give them to you.

/Dennis
Received: from C36.Tymnet by X930.Tymnet; Fri, 3 Jan 86 16:29:47 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 3 Jan 86 14:44:03 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Gateway-Host 65 Problem. 

I've finally gotten a reply to my call to Dave Smith.  He tells me that 65's
base had an older version of the base code:  a version of code that was buggy.
He is contacting Tymnet to get them to install the proper code into the base.

I'll notify Tymnet International that I got Tymnet's N.T.D.  to get on Tymnet
Operations to get them to fix their bug.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 3 Jan 86 16:52:00 PST 
To: Dennis Coffey <dencoff@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Re: Gateway-Host 65 Problem. 

Congratulations on a sleuthing job well done in discovering that the
circuit zaps were occurring at the base and not the host.  Please try
to follow up and see if the problem persists after the new version of
base code gets loaded.

Keep up the good work!

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 5 Jan 86 23:57:13 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: See you between 8:00 and 8:30 on Monday! 

Thank you for the ride to and from work...  I will expect you
sometime between 8;00 and 8:30.  See you then.  /Carl

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 6 Jan 86 23:54:29 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: Car Fixed fine.... Thank you! 

Osman,
  ...so sorry that I'm sending this so late... the can magically came
back at 8:00 PM and it's all better.  Thank you for the ride, and for
being available for tomorrow.  I'll be in by 10:00 for th meeting.

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 7 Jan 86 18:41:35 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: C24 crash during TITO ALL-FILES. 

The EUE stopcode was caused by FILIO trying to start I/O to unit 340.
Since only 20 disk unit blocks are defined, it picked up garbage as a
dispatch address of PUSHJed to it.  I haven't yet determined why the
PGY table had that unit number in it.     /JMS
Received: from C36.Tymnet by X930.Tymnet; Thu, 9 Jan 86 11:06:46 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Thu, 9 Jan 86 10:55:50 PST 
To: Carl Baltrunas <CARL@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Subject: MAKINI not working. 

Carl or Joe:
I've been trying to customize RPG for the SPOOL Operator username (SPPOPER) on
host 22 (among others).  I can successfully do what I need for the current
frame, but when I logoff and log back in the special processors and the do
list I established are no longer in my list, in spite of my having given the
command CTEST MAKINI after all my other CTEST commands.

The do list I get when I run TC after logging in contains MCH, SPL & MPL.  I
don't know how Mike's directory gets in there, and the other directories I
specified in my CTEST in my prior frame get deleted.  I use exactly the same
RPGINI.CTL with PCOM that I successfully used on the other hosts.  It appears
there's something unusual about host 22.  I don't understand the difference.
Received: from C39.Tymnet by X930.Tymnet; Thu, 9 Jan 86 11:20:31 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 9 Jan 86 11:16:06 PST 
To: FIN.VIDA@Ontyme.Tymnet 
Cc: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: EMSTXS class 4 

User name: emstxs
Options: 160,ignore-colon,mystery-bit,no-password
1: (class) 4. (home) 832.

Something needs to be done to eliminate the restriction that no
circuits can originate from system 39.  Probably adding 39 to
class 4.  TUMS is blowing up on 39 trying to read somebody's mail
for them.  I guess they don't miss it because I haven't gotten
any complaints, but it should be fixed anyway.  Thanks -Bill
From: FLETCHERC@X930.Tymnet 
Date: Thu, 9 Jan 86 11:35:36 PST 
To: carl 
Subject: tymnet training 


I have registered you for TYMNET training. Here are the courses
and dates:

102 TYMNET Network Operations		4/2-4/11

400 Intro to ISIS software		4/14-4/19

440 TYMNET engine programming		4/21-4/25


Classes start at 8:30 a.m. at 2665 N. First St. in San Jose.

	Craig
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 9 Jan 86 15:17:09 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Serial numbers in (M33)TERMS.TXT 

If you update (M33)TERMS.TXT (found in INFO tree under PEOPLE/TERMINV),
it won't be much of a bother when Inventory asks for serial numbers again.

/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 10 Jan 86 23:57:17 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: P034/P13 

SCNSER: Added space in the LDB to remember the login string sent to the
	supervisor when building a circuit.  This allows us to track an
	outgoing port and where it is going, username:host in most cases.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 12 Jan 86 7:24:17 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline call 

*** My beeper was "ON" and in it's charger - it did NOT go off ***
#'s are 408-977-7960 and 415-498-3841.

12-Jan-86 07:00/Carl
Contact:  Cupertino BLDG C, OPS, VIJAY, (408)446-7705, System C34

Symptom:  PJ not running, in ^C state.

Problem:  PJ running LOGINN in ^C state.  Apparently it never ran the
          init program and went to an EXIT someplace?  When I logged-in
          I experienced tha same problem.  I typed ^C^C and then took
          a look at PJ.

Solution: I built a circuit to PJ:34 and was asked to attach, which I
          did.  I then logged out PJ and built another circuit.  The
          second time it ran PAM and detached like it was supposed to
          do.  Problem solved.  Cause of problem still unknowm.  I did
          check for HRE HWE BDA set on LOGINN.SHR but everything seems
          to be ok.

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 13 Jan 86 4:07:06 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline Call 

*** My beeper was "ON" and in it's charger - it did NOT go off ***
#'s are 408-977-7960 and 415-498-3841.

13-Jan-86 03:35/Carl
Contact:  Cupertino BLDG C, OPS, L.T. Smith, (408)446-7705, System 55

Symptom:  Page i/o error on OPER.SAV and SYSTAT.SHR

Problem:  System experienced some disk errors on BPA6 earlier and the
          files (SYS)OPER.SAV, (SYS)SYSTAT.LOW, (MAGNUM)MAGNRN.SHR and
          (RCONX)I6SAFV.MR0 were marked with HRE and (SYS)011286.DAT
          was marked with BDA.

Solution: I set my SETE password and copied OPER.SAV on top of itself
          which cleared HRE (file wasn't really bad).  Same worked for
          (MAGNUM) and (RCONX) files.  I copied (SYS)SYSTAT.LOW from
          system 35 to 55.  Everything seems to be ok now.

Note:  (RCONX) is customer for Donna Kurimay, who also called OPS to
       complain.  Hopefully fixing MAGNRN and their .MR0 file also
       solved their problem. /Carl

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 13 Jan 86 4:16:18 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline addendum 

Maintenance replaced a couple of cards in BPA6, so a bad drive was the
original cause of the HRE and BDA errors seen this morning. Since it
was known "bad" the first try was to see if the data was actually good,
which apparently worked in 3/4 cases.  /Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 13 Jan 86 10:53:15 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: Beepers 

My beeper was also on and in it charger all weekend.  Mine did not go off
either.		/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 13 Jan 86 11:43:52 PST 
To: Dennis Ernst <DErnst@C39.Tymnet> 
Subject: Node 6657 

Dennis,
  I thought that you fixed the problem with the node going zonkers when
trying to build a circuit out results in a supervisor message regarding
the host (built to) unavailable (super-shut?).  I did this to host 26
on friday and the node came up with a stopcode then I typed P and it
came up with a different one.  Another P and the same stopcode but a
different set of data in the registers... all succeeding Ps were then
identical.

  I have the numbers if you'd like them.  Let me know.   /Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 13 Jan 86 12:21:50 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, JMS, FLETCHERC 
Subject: BEEPER.. 

MY BEEPER WAS ON BUT DIDN'T GO OFF EITHER..
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 14 Jan 86 2:09:29 UT
From: ISIPR.G/MARUS@Ontyme.Tymnet 
Date: 13 JAN 86 16:00:36 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: KTL.TOKYO@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	ACS.G/MASTERS@Ontyme.Tymnet, ISIPR.G/MARUS@Ontyme.Tymnet,
	TXS.@Ontyme.Tymnet 
Message-id: J95507@Ontyme.Tymnet 
Subject: More on Toshiba, ref J93432 

           McDonnell Douglas                      ISIPR.IRVINE   T-208
           Information Systems International
           Pacific Region                         Tel. [-1] (714) 250-1000

DATE>      13 JAN 86  16:00                       Telex 9105951546 MICD NPBH

TO>        M.Ogura(KTL.TOKYO)

COPIES>    C.Baltrunas(TXS.C/BALTRUNAS)
           G.Masters(ACS.G/MASTERS)

FROM>      G.Marus(ISIPR.G/MARUS)


SUBJECT>   More on Toshiba, ref J93432


-----------------------------------------------------------------------


Ogura-san,

1. Today I got some hard copy of the sessions from Toshiba. As I mentioned
in J93432, Jim Buck of Toshiba said that the system worked normally down
to the point where the system calls (J1STAR)CHK and (J1STAR)TRDS. Let
me reproduce the hard copy from that point on:


RUN (J1STAR)CHK
RUN (J1STAR)TRDS

                        *** TYMSHARE ONLINE ***

1 : LOGON               4 : RECEIVE DATA                7 : LOGOFF
2 : SEND COMMAND        5 : FILE STATUS                 8 : END
3 : SEND DATA           6 : COMMAND

                                OPERATION NO ? _

---------------------------------------------------------------------------


*SEND DS01.LOGFILE

* SNDJ1

                                                                Page  2

*SEND DS02.SDATA

ERROR                   C527            IDLE

2. Ogura-san, is any of the above helpful to you? Can you let us know
what might be going on here? Is there any documentation available on the
Toshiba system? Can you tell us what is being generated by the system,
and what is being typed in by the user?

I tried to call Jim Buck today, but he was busy in meetings. Ogura-san, I
will make copied of their session, and airmail them to you tomorrow.

3. Ref item 3 of J93432: the error message that appears on the screen but
not the printer is:

NORMAL DISC                     05DD                    SESSION COMP.

Ogura-san, Carl, Gary--do any of you recognize this error message as
belonging to the application, the PDP-X, or to HSA? I don't recognize it
at all, and I think it may be a message from the TI 990 intelligent
terminal. Look forward to any comments you may have.

Thanks and regards

Greg
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 14 Jan 86 10:04:45 UT
From: KTL.HASHIMOTO@Ontyme.Tymnet 
Date: 14 JAN 86 01:47:47 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: NTD.P/SPIES@Ontyme.Tymnet, NTD.L/MAURER@Ontyme.Tymnet,
	NTD.A/FENN@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	IOD.BAMBERG@Ontyme.Tymnet, CT.TECHSERV@Ontyme.Tymnet,
	TUK.WOKING@Ontyme.Tymnet, TNS.H/ALBERT@Ontyme.Tymnet,
	ISIPR.CUPERTINO@Ontyme.Tymnet, ACS.G/MASTERS@Ontyme.Tymnet,
	KTL.SAEKI@Ontyme.Tymnet, TYMOPS.J/SNAVELY@Ontyme.Tymnet,
	ACS.T/SILVA@Ontyme.Tymnet, NSS.R/SPRENKEL@Ontyme.Tymnet,
	NSMS.G/SMITH@Ontyme.Tymnet, NSS.J/GOMES@Ontyme.Tymnet,
	TNS.DOCQUIER@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NSS.N/SHUKLA@Ontyme.Tymnet, NSS.G/WALKER@Ontyme.Tymnet,
	EDI.L/BAIRD@Ontyme.Tymnet, ISIPR.G/MARUS@Ontyme.Tymnet,
	CT.AMSTERDAM@Ontyme.Tymnet, TUK.WOKING@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, TYMOPS.B/TOMASI@Ontyme.Tymnet,
	ISIPR.LITTLE@Ontyme.Tymnet, CORP.MAIL@Ontyme.Tymnet,
	NTD.DS@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, KTL.OSAKA@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A99292@Ontyme.Tymnet 
Subject: "Kokusai Tymshare OnTyme KTL.TOKYO"... 

     
     
             Kokusai Tymshare                OnTyme  KTL.TOKYO   
                                             Phone   [-81] (3) 262-8711    
                                             Fax     [-81] (3) 262-8757    
Date      : 86/01/14  18:38 (JST)  
     
Attn      : Phil Spies      (NTD.P/SPIES)    
            Linda Maurer    (NTD.L/MAURER)   
            Al Fenn         (NTD.A/FENN)
            Patrick Driscoll(NTD.P/DRISCOLL) 
            Ron Bamberg     (IOD.BAMBERG)    
            Francis Bonnin  (CT.TECHSERV)    
            Peter Boraston  (TUK.WOKING)
            Albert Handelmann       (TNS.H/ALBERT)
     
Copies    : Peter Haas      (ISIPR.CUPERTINO)
            Gary Masters    (ACS.G/MASTERS)  
            Aki Saeki       (KTL.SAEKI) 
            Jack Snavely    (TYMOPS.J/SNAVELY)    
            Terry Silva     (ACS.T/SILVA)    
            Rick Sprenkel   (NSS.R/SPRENKEL) 
            Gordon Smith    (NSMS.G/SMITH)   
            John Gomes      (NSS.J/GOMES)    
            Thierry Docquier(TNS.DOCQUIER)   
            Mike Rude       (NTD.M/RUDE)
            Nermala Shukla  (NSS.N/SHUKLA)   
            Gary Walker     (NSS.G/WALKER)   
            Larry Baird     (EDI.L/BAIRD)    
            Greg Marus      (ISIPR.G/MARUS)  
            Tony Bakker     (CT.AMSTERDAM)   
            Keith Roberts   (TUK.WOKING)
            Kuni Kamoi      (KTL.TOKYO) 
            Dennis Coffey   (TXS.D/COFFEY)   
            Carl Baltrunas  (TXS.C/BALTRUNAS)
            Bob Tomasi      (TYMOPS.B/TOMASI)
            Pete Little     (ISIPR.LITTLE)   
            Warren Prince   (CORP.MAIL) 
            Dave Smith      (NTD.DS)    
            Tom Asama       (KTL.TOKYO) 
            Makoto Ogura    (KTL.TOKYO) 
            Tak Yasuma      (KTL.TOKYO) 
            Shuichi Inoue   (KTL.OSAKA) 
     
From      : Nob Hashimoto   (KTL.TOKYO) 
     
Subject   : URGENT PROBLEM    
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
AFTER NEW HSA WAS INSTALLED IN LAST WEEK SATURDAY(JAPANESE NODE),
THE URGENT PROBLEM HAS BEEN OCCURING IN JAPAN AND EUROPE (WE
DID NOT HEAR FROM THE U.S. CUSTOMERS YET BUT I THINK THE SAME    
PROBLEM HAS BEEN OCCURING IN THE U.S.) THIS WEEK AS FOLLOWS;
     
IN THE CASE OF UPLOADING, THE SERIES OF THE SAME CHARACTER(THAT IS, AAAA   
, 0000, BBBB, ZZZZ) BEFORE SPACE WERE CHANGED INTO SPACES.  
WE HAVE CONFIRMED THAT A FEW CUSTOMERS IN JAPAN AND GERMANY HAS OCCURED    
YESTERDAY AND IN THE RESAULT OF OUR TEST USING OUR MINI COMPUTER, THE 
SAME PROBLEM OCCURED.    
WE DECIDED TO STOP CUSTOMER USE HSA TODAY.   
COULD YOU FOLLOW UP HSA CUSTOMERS YOUR SIDE ?
BY THE END OF TOMMORROW(14 JAN IN YOUR TIME) PLEASE FIX THE PROBLEM.  
THANKS AND BEST REGARDS. 
P.S. THE OTHER PROBLEMS OCCURED BEFORE HAVE BEEN CONTINUING NOW AND   
WE INFORM YOU OF THE SITUATION IN DETAIL LATER.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 15 Jan 86 13:56:27 PST 
To: Landa Morris <IPC.L/Morris@Ontyme.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet>, Carl A Baltrunas <Carl@X930.Tymnet>, Bruce
	Ring <bring@C26.Tymnet> 
Subject: username changes 

Landa,
  Please rename the following two usernames in the MUD, the accounting
and in the LUD/DUL on all pdp-10s.

  (old)    (ppn)      (new)    (files)
   KEN    3, 35556    PEAK     move all files from (KEN) into (PEAK)
   MCH    3,231227    RING     files in (MCH) may be deleted

Please let me know when the name change is completed.  Also, in the
MUD, please leave the class and groups the same as before and do not
change (KEN)'s password.  I will setup a new password for (RING) so
it doesn't matter if you change that one.  Thanks a lot.

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 15 Jan 86 17:41:11 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Kernel host numbers 

When you find the list of kernel host numbers, please update (MONDOC)BASES.DOC
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 16 Jan 86 10:06:00 UT
From: KTL.HASHIMOTO@Ontyme.Tymnet 
Date: 15 JAN 86 23:45:37 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: NTD.P/SPIES@Ontyme.Tymnet, NTD.L/MAURER@Ontyme.Tymnet,
	NTD.A/FENN@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	IOD.BAMBERG@Ontyme.Tymnet, TUK.WOKING@Ontyme.Tymnet,
	TNS.H/ALBERT@Ontyme.Tymnet, ISIPR.CUPERTINO@Ontyme.Tymnet,
	ACS.G/MASTERS@Ontyme.Tymnet, KTL.SAEKI@Ontyme.Tymnet,
	TYMOPS.J/SNAVELY@Ontyme.Tymnet, ACS.T/SILVA@Ontyme.Tymnet,
	NSS.R/SPRENKEL@Ontyme.Tymnet, NSMS.G/SMITH@Ontyme.Tymnet,
	NSS.J/GOMES@Ontyme.Tymnet, TNS.DOCQUIER@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NSS.N/SHUKLA@Ontyme.Tymnet,
	NSS.G/WALKER@Ontyme.Tymnet, EDI.L/BAIRD@Ontyme.Tymnet,
	ISIPR.G/MARUS@Ontyme.Tymnet, CT.AMSTERDAM@Ontyme.Tymnet,
	TUK.WOKING@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TYMOPS.B/TOMASI@Ontyme.Tymnet, ISIPR.LITTLE@Ontyme.Tymnet,
	CORP.MAIL@Ontyme.Tymnet, NTD.DS@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.OSAKA@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A01044@Ontyme.Tymnet 
Subject: "Kokusai Tymshare OnTyme KTL.TOKYO"... 

     
     
             Kokusai Tymshare                OnTyme  KTL.TOKYO   
                                             Phone   [-81] (3) 262-8711    
                                             Fax     [-81] (3) 262-8757    
Date      : 86/01/16  16:43 (JST)  
     
Attn      : Phil Spies      (NTD.P/SPIES)    
            Linda Maurer    (NTD.L/MAURER)   
            Al Fenn         (NTD.A/FENN)
            Patrick Driscoll(NTD.P/DRISCOLL) 
            Ron Bamberg     (IOD.BAMBERG)    
            Peter Boraston  (TUK.WOKING)
            Albert Handelmann       (TNS.H/ALBERT)
     
Copies    : Peter Haas      (ISIPR.CUPERTINO)
            Gary Masters    (ACS.G/MASTERS)  
            Aki Saeki       (KTL.SAEKI) 
            Jack Snavely    (TYMOPS.J/SNAVELY)    
            Terry Silva     (ACS.T/SILVA)    
            Rick Sprenkel   (NSS.R/SPRENKEL) 
            Gordon Smith    (NSMS.G/SMITH)   
            John Gomes      (NSS.J/GOMES)    
            Thierry Docquier(TNS.DOCQUIER)   
            Mike Rude       (NTD.M/RUDE)
            Nermala Shukla  (NSS.N/SHUKLA)   
            Gary Walker     (NSS.G/WALKER)   
            Larry Baird     (EDI.L/BAIRD)    
            Greg Marus      (ISIPR.G/MARUS)  
            Tony Bakker     (CT.AMSTERDAM)   
            Keith Roberts   (TUK.WOKING)
            Kuni Kamoi      (KTL.TOKYO) 
            Dennis Coffey   (TXS.D/COFFEY)   
            Carl Baltrunas  (TXS.C/BALTRUNAS)
            Bob Tomasi      (TYMOPS.B/TOMASI)
            Pete Little     (ISIPR.LITTLE)   
            Warren Prince   (CORP.MAIL) 
            Dave Smith      (NTD.DS)    
            Tom Asama       (KTL.TOKYO) 
            Makoto Ogura    (KTL.TOKYO) 
            Tak Yasuma      (KTL.TOKYO) 
            Shuichi Inoue   (KTL.OSAKA) 
     
From      : Nob Hashimoto   (KTL.TOKYO) 
     
Subject   : NEW HSA PROBLEM   
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
THE PROBLEM WHICH THE MULTIPLE CHARCTERS FOLLOWED BY MULTIPLE BLANKS ARE   
CONVERTED TO BLANKS WAS FIXED BY NEW PATCH FILE OF HSA.
THE MOST OF OUR CUSTOMERS ARE USING OUR SERVICE IN MULTI-COUNTRIES AND
WE HAVE TO FOLLOW UP THEM.    
COULD YOU LET ME KNOW WHEN THE PROBLEM WAS FIXED ON YOUR SIDE?   
THANKS AND REGARDS. 
     
P.S. 
 ONE OF OUR CUSTOMERS IN LOS ANGELS HAD THE SAME PROBLEM AT 9:20 ON JAN.16 
 (JST). COULD YOU CHECK WHEN THE PROBLEM OCCURED BEFORE OR AFTER IT WAS    
  FIXED ?
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 16 Jan 86 10:06:26 UT
From: KTL.HASHIMOTO@Ontyme.Tymnet 
Date: 16 JAN 86 00:23:23 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: NTD.P/SPIES@Ontyme.Tymnet, NTD.L/MAURER@Ontyme.Tymnet,
	NTD.A/FENN@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	IOD.BAMBERG@Ontyme.Tymnet, TUK.WOKING@Ontyme.Tymnet,
	TNS.H/ALBERT@Ontyme.Tymnet, ISIPR.CUPERTINO@Ontyme.Tymnet,
	ACS.G/MASTERS@Ontyme.Tymnet, KTL.SAEKI@Ontyme.Tymnet,
	TYMOPS.J/SNAVELY@Ontyme.Tymnet, ACS.T/SILVA@Ontyme.Tymnet,
	NSS.R/SPRENKEL@Ontyme.Tymnet, NSMS.G/SMITH@Ontyme.Tymnet,
	NSS.J/GOMES@Ontyme.Tymnet, TNS.DOCQUIER@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NSS.N/SHUKLA@Ontyme.Tymnet,
	NSS.G/WALKER@Ontyme.Tymnet, EDI.L/BAIRD@Ontyme.Tymnet,
	ISIPR.G/MARUS@Ontyme.Tymnet, CT.AMSTERDAM@Ontyme.Tymnet,
	TUK.WOKING@Ontyme.Tymnet, KTL.KAMOI@Ontyme.Tymnet,
	TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TYMOPS.B/TOMASI@Ontyme.Tymnet, ISIPR.LITTLE@Ontyme.Tymnet,
	CORP.MAIL@Ontyme.Tymnet, NTD.DS@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.OSAKA@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A01052@Ontyme.Tymnet 
Subject: "Kokusai Tymshare OnTyme KTL.TOKYO"... 

     
     
             Kokusai Tymshare                OnTyme  KTL.TOKYO   
                                             Phone   [-81] (3) 262-8711    
                                             Fax     [-81] (3) 262-8757    
Date      : 86/01/16  17:21 (JST)  
     
Attn      : Phil Spies      (NTD.P/SPIES)    
            Linda Maurer    (NTD.L/MAURER)   
            Al Fenn         (NTD.A/FENN)
            Patrick Driscoll(NTD.P/DRISCOLL) 
            Ron Bamberg     (IOD.BAMBERG)    
            Peter Boraston  (TUK.WOKING)
            Albert Handelmann       (TNS.H/ALBERT)
     
Copies    : Peter Haas      (ISIPR.CUPERTINO)
            Gary Masters    (ACS.G/MASTERS)  
            Aki Saeki       (KTL.SAEKI) 
            Jack Snavely    (TYMOPS.J/SNAVELY)    
            Terry Silva     (ACS.T/SILVA)    
            Rick Sprenkel   (NSS.R/SPRENKEL) 
            Gordon Smith    (NSMS.G/SMITH)   
            John Gomes      (NSS.J/GOMES)    
            Thierry Docquier(TNS.DOCQUIER)   
            Mike Rude       (NTD.M/RUDE)
            Nermala Shukla  (NSS.N/SHUKLA)   
            Gary Walker     (NSS.G/WALKER)   
            Larry Baird     (EDI.L/BAIRD)    
            Greg Marus      (ISIPR.G/MARUS)  
            Tony Bakker     (CT.AMSTERDAM)   
            Keith Roberts   (TUK.WOKING)
            Kuni Kamoi      (KTL.KAMOI) 
            Dennis Coffey   (TXS.D/COFFEY)   
            Carl Baltrunas  (TXS.C/BALTRUNAS)
            Bob Tomasi      (TYMOPS.B/TOMASI)
            Pete Little     (ISIPR.LITTLE)   
            Warren Prince   (CORP.MAIL) 
            Dave Smith      (NTD.DS)    
            Tom Asama       (KTL.TOKYO) 
            Makoto Ogura    (KTL.TOKYO) 
            Tak Yasuma      (KTL.TOKYO) 
            Shuichi Inoue   (KTL.OSAKA) 
     
From      : Nob Hashimoto   (KTL.TOKYO) 
     
Subject   : LOST DATA    
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
OUR STAFF HAVE ALREADY INFORMED SOME OF YOU THAT DATA WAS LOST IN THE CASE OF DOWN-LOAD BEFORE.
WE COULD CONFIRM THIS PROBLEM HAVE BEEN CONTINING ON OUR SIDE.   
     
HERE ARE THE LATEST INFORMATION REGARDING DATA LOST PROBLEM WHICH WE COULD 
CONFIRM.  
     
1. ABOUT 11:30 ON JAN 14(JST), THE PROBL;EM WAS IN MOVETERM ON HOST#48 AND 
   MULTI-RECORDS WERE LOST.   
     
2. ABOUT 9:50 ON JAN 16(JST), THE PROBLEM WAS IN SPOOL ON HOST#34 AND MULTI-    
   RECORDS WERE LOST.    
     
3. ABOUT 10:00 ON JAN. 16, THE PROBLEM WAS IN KPUNCH ON HOST#57 AND MULTI- 
   RECORDS WERE LOST.    
     
COULD YOU CHECK THIS PROBLEM AND SOLVE IT WITH THE OTHER PROBLEMS AS SOON AS    
POSSIBLE ?
THANKS AND BEST REGARDS.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 16 Jan 86 11:37:51 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: RPG problem for JCLMIN:55 

If you say "COMPILE A.MAC+B.MAC" and B.MAC does not exist but B.REL does, then
RPG says COMMAND ERROR instead of NO SUCH FILE - B.MAC.  The problem does not
occur if all source files exist, or if B.REL is deleted.  I noticed that
(JCLMIN:55) did not have TEST.MAC but did have TEST.REL.     /JMS
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 17 Jan 86 22:06:22 UT
From: NTD.G/CONE@Ontyme.Tymnet 
Date: 17 JAN 86 11:38:09 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: I04446@Ontyme.Tymnet 
Subject: NEW HSA PROBLEM 

To:  DISTRIBUTION
 
FR:  AL FENN
 
DT:  1/17/86
 
RE:  NEW HSA PROBLEM
 
 
*****************************************************************
 
The multiple character to multiple blanks problem was resolved by 

two  patches issued on January 14,  1986.   We received a  report 
from KTL today, the 16th, that this problem was resolved.
 
The  problem  of  lost  data  continues.    Specific  times  were 

reported by KTL.  Bob Tomasi is looking into this by checking the 
log reports for that time.   We believe that buffer zaps could be 

the  cause  of this problem.   One of the HSA interfaces  counted 
receiving over 300 black balls in the last day.   It is  possible 

that  this lost data problem may be caused by the EBUSS code  and 
maybe resolved by the EBUS fixes.
 
The  EBUS  code  fixes  were completed on the  15th  and  testing 

started on the 16th.
 
In  investigating the Japanese network configuration,  Mike  Rude 

discovered  that  the  machines in which the HSA  runs  are  very 
overloaded.   The machines report both foreground and  background 

overloads  during  peak usage.   The machine at KTL,  Node  3575, 
seems to be over configured with IHSA, BSC, and Tymsat.
 
 
AF/gc
 
 
DISTRIBUTION:
 
TOM ASAMA
LARRY BAIRD
TONY BAKKER
CARL BALTRUNAS
RON BAMBERG
PETER BORASTON
DENNIS COFFEY
THIERRY DOCQUIER
JOHN GOMES
PETER HAAS
ALBERT HANDELMANN
SHUICHI INOUE
KUNI KAMOI
PETE LITTLE
GREG MARUS
GARY MASTERS
BURT NOVAK

MAKOTO OGURA
WARREN PRINCE
KEITH ROBERTS
AKI SAEKI
NERMALA SHUKLA
TERRY SILVA
GORDON SMITH
JACK SNAVELY
RICK SPRENKEL
BOB TOMASI
GARY WALKER
TAK YASUMA
 
NTD:
 
PAT DRISCOLL
BILL EUSKE
LINDA MAURER
MIKE RUDE
DAVE SMITH
PHIL SPIE
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 18 Jan 86 0:13:35 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: TAUX JMS:TYMNET/95 

TAUX won't allow me to login using JMS:TYMNET/95, but AUXIO and
TELECOPY do.  Why?
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 20 Jan 86 14:04:55 UT
From: KTL.OGURA@Ontyme.Tymnet 
Date: 20 JAN 86 03:50:38 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: NTD.P/SPIES@Ontyme.Tymnet, NTD.L/MAURER@Ontyme.Tymnet,
	NTD.A/FENN@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	IOD.BAMBERG@Ontyme.Tymnet, TUK.WOKING@Ontyme.Tymnet,
	TNS.H/ALBERT@Ontyme.Tymnet, ISIPR.CUPERTINO@Ontyme.Tymnet,
	ACS.G/MASTERS@Ontyme.Tymnet, KTL.SAEKI@Ontyme.Tymnet,
	TYMOPS.J/SNAVELY@Ontyme.Tymnet, ACS.T/SILVA@Ontyme.Tymnet,
	NSS.R/SPRENKEL@Ontyme.Tymnet, NSMS.G/SMITH@Ontyme.Tymnet,
	NSS.J/GOMES@Ontyme.Tymnet, TNS.DOCQUIER@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NSS.N/SHUKLA@Ontyme.Tymnet,
	NSS.G/WALKER@Ontyme.Tymnet, EDI.L/BAIRD@Ontyme.Tymnet,
	ISIPR.G/MARUS@Ontyme.Tymnet, CT.AMSTERDAM@Ontyme.Tymnet,
	TUK.WOKING@Ontyme.Tymnet, KTL.KAMOI@Ontyme.Tymnet,
	TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TYMOPS.B/TOMASI@Ontyme.Tymnet, ISIPR.LITTLE@Ontyme.Tymnet,
	CORP.MAIL@Ontyme.Tymnet, NTD.DS@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.OSAKA@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A03156@Ontyme.Tymnet 
Subject: "Kokusai Tymshare OnTyme KTL.TOKYO"... 

     
     
             Kokusai Tymshare                OnTyme  KTL.TOKYO   
                                             Phone   [-81] (3) 262-8711    
                                             Fax     [-81] (3) 262-8757    
Date      : 86/01/20  20:48 (JST)  
     
Attn      : Phil Spies      (NTD.P/SPIES)    
            Linda Maurer    (NTD.L/MAURER)   
            Al Fenn         (NTD.A/FENN)
            Patrick Driscoll(NTD.P/DRISCOLL) 
            Ron Bamberg     (IOD.BAMBERG)    
            Peter Boraston  (TUK.WOKING)
            Albert Handelmann       (TNS.H/ALBERT)
     
Copies    : Peter Haas      (ISIPR.CUPERTINO)
            Gary Masters    (ACS.G/MASTERS)  
            Aki Saeki       (KTL.SAEKI) 
            Jack Snavely    (TYMOPS.J/SNAVELY)    
            Terry Silva     (ACS.T/SILVA)    
            Rick Sprenkel   (NSS.R/SPRENKEL) 
            Gordon Smith    (NSMS.G/SMITH)   
            John Gomes      (NSS.J/GOMES)    
            Thierry Docquier(TNS.DOCQUIER)   
            Mike Rude       (NTD.M/RUDE)
            Nermala Shukla  (NSS.N/SHUKLA)   
            Gary Walker     (NSS.G/WALKER)   
            Larry Baird     (EDI.L/BAIRD)    
            Greg Marus      (ISIPR.G/MARUS)  
            Tony Bakker     (CT.AMSTERDAM)   
            Keith Roberts   (TUK.WOKING)
            Kuni Kamoi      (KTL.KAMOI) 
            Dennis Coffey   (TXS.D/COFFEY)   
            Carl Baltrunas  (TXS.C/BALTRUNAS)
            Bob Tomasi      (TYMOPS.B/TOMASI)
            Pete Little     (ISIPR.LITTLE)   
            Warren Prince   (CORP.MAIL) 
            Dave Smith      (NTD.DS)    
            Tom Asama       (KTL.TOKYO) 
            Makoto Ogura    (KTL.TOKYO) 
            Tak Yasuma      (KTL.TOKYO) 
            Shuichi Inoue   (KTL.OSAKA) 
     
From      : Nob Hashimoto   (KTL.TOKYO) 
     
Subject   : CONTINUING HSA PROBLEMS
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
I WOULD LIKE TO REPORT THE SITUATION OF HSA PROBLEMS TODAY(JST;JAN 20).    
     
1. MULTIPLE CHARACTERS BEFORE MULTIPLE BLANK BEING CONVERTED TO BLANKS.    
    THIS PROBLEM DID NOT OCCURS.   
     
2. "EOT" SEQUENCE TROUBLE DID NOT OCCUR.
     
3. DATA LOST PROBLEMS DID OCCUR (NOT SOLVED) 
     
4. RESPOND TIME IS DELAY(TIME-OUT PROBLEM, IS NOT SOLVED.)  
     
5. FILE COULD NOT BE CLOSED(TYMCOM-X).  
     
6. HSA IS HOLDING LINE AFTER JOB ON TYMCOM WAS KILLED. 
     
WE WOULD LIKE TO HEAR AS SOON AS POSSIBLE THAT ALL OF PROBLEMS WERE SOLVED 
COMPRETELY. OFF COURSE, OUR CUSTOMERS TOO.   
THANKS AND REGARDS.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 20 Jan 86 18:07:40 UT
From: KTL.OGURA@Ontyme.Tymnet 
Date: 20 JAN 86 06:17:56 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: NTD.DS@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	ACS.G/MASTERS@Ontyme.Tymnet, ISIPR.G/MARUS@Ontyme.Tymnet,
	ACS.T/SILVA@Ontyme.Tymnet, NTD.P/SPIES@Ontyme.Tymnet,
	TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/FLETCHER@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, DSS.D/KURIMAY@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, KTL.KAMOI@Ontyme.Tymnet,
	KTL.SAEKI@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.OSAKA@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A03213@Ontyme.Tymnet 
Subject: "Kokusai Tymshare OnTyme KTL.TOKYO"... 

     
     
             Kokusai Tymshare                OnTyme  KTL.TOKYO   
                                             Phone   [-81] (3) 262-8711    
                                             Fax     [-81] (3) 262-8757    
Date      : 86/01/20  23:16 (JST)  
     
Attn      : Dave Smith      (NTD.DS)    
            Carl Baltrunas  (TXS.C/BALTRUNAS)
            Gary Masters    (ACS.G/MASTERS)  
     
Copies    : Greg Marus      (ISIPR.G/MARUS)  
            Terry Silva     (ACS.T/SILVA)    
            Phil Spies      (NTD.P/SPIES)    
            Dennis Coffey   (TXS.D/COFFEY)   
            Craig Fletcher  (TXS.C/FLETCHER) 
            Osman Guven     (TXS.O/GUVEN)    
            Donna Kurimay   (DSS.D/KURIMAY)  
            Tom Asama       (KTL.TOKYO) 
            Kuni Kamoi      (KTL.KAMOI) 
            Aki Saeki       (KTL.SAEKI) 
            Tak Yasuma      (KTL.TOKYO) 
            Shuichi Inoue   (KTL.OSAKA) 
     
From      : Makoto Ogura    (KTL.TOKYO) 
     
Subject   : Trials for BIONIC through revised base program on C34
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
     
At the noon of last saturday (18-Jan) in JST, I was informed by the president   
Asama, he is staying in Cupertino, that some base program of C34 was revised.   
     
Then I tried BIONIC as follows.    
     
Trial-1   
-------   
     
        18-Jan 13:30  --  14:40   (JST) 
     
        1.1  10 trials for (SYS:34)BIONIC          .... OK  
        1.2  10 trials for (FTSYS:34)BIONIC        .... OK  
        1.3  10 trials for (UPLKT:34)BIONIC        .... OK  
        1.4  10 trials for (SYS:57)BIONIC          .... OK  
     
        I thought a base program of C57 was not revised yet. So I checked  
        the difference between revised-base and not-revised-base.
        However both had the same conclusion.
        And I didn't know (FTSYS:34)BIONIC was the same one as (UPLKT:34)BIONIC,
        which is %1(6). So I checked the UPLKT's. 
     
trial-2   
--------  
     
        18-Jan  17:05  --  17:20    (JST)    
     
        2.1  10 trials for (SYS:34)BIONIC        ..... ok   
        2.2  10 trials for (SYS:57)BIONIC        ..... ok   
     
     
Trial-3   
--------  
     
        20-Jan  9:20  --  10:30      (JST)   
     
        3.1  5 trials for (SYS:34)BIONIC
                3 trials ( from the 1st to the 3rd ) was not OK. 
                In every trial, BIONIC closed normally but some data was lost.  
                2 trials were OK.  
        3.2  5 trials for (FTSYS:34)BIONIC         ..... OK 
        3.3  5 trials for (SYS:57)BIONIC           ..... OK 
     
Trial-4   
-------   
     
        20-Jan  18:30  --  22:00  (JST) 
     
        4.1  10 trials for (SYS:34)BIONIC    
                1 trial (the 3rd trial) was not OK. BIONIC closed but some 
                data was lost.
                9 trials were OK.  
        4.2  10 trials for (FTSYS:34)BIONIC          .....OK
        4.3  10 trials for (SYS:57)BIONIC    
                1 trial (the 3rd trial) was not OK. BIONIC closed, but some data
                was lost.
                9 trials were OK.  
     
     
From the observation of the trials, every BONIC(SYS:34,FTSYS:34,UPLKT:34, and   
SYS:57) seems to be closed normally and I think the current situation is a bit  
better than the last week's, however it is not enough to say the problem   
is fixed. 
     
Best regards,  
Makoto
From: The FLU bug <Carl@X930.Tymnet> 
Date: Mon, 20 Jan 86 14:28:37 PST 
To: Osman Guven <osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>, Lois Frost
	<FSC.L/FROST@Ontyme.Tymnet> 
Subject: Rest Period, 1986 

It appears that I've been given a forced rest period with the FLU coming
down to visit me this past week-end.  I've spent most of the past 2 days
in bed.  I will at least try to login and check my mail just in case any
emergencies crop up, but I don't expect to be doing much work for the
next couple of days.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 20 Jan 86 14:48:32 PST 
To: Ken Dawson <Peak@X930.Tymnet> 
Subject: Illness in the family 

Due to a bout with the changing weather, I'm feeling a bit under
and will be reachable at home sporadically and by mail.  I'll let
you know when I make it back to the office.

PS:  You may have a couple of NTD people calling you about a monitor
     error message generated by PEAK when it's executing a CHANIO of
     some sort.  I took a quick look at it, but not enough to check
     which CHANIO to determine why it's getting the message.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 20 Jan 86 14:55:47 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: TAUX /subhost 

TAUX does it's own checking for username:host and most probably
doesn't allow username:host/subhost as a valid format.  AUXIO
passes whatever you type, substituting username for (^P), i think,
but if you just say :host it loses, while ^P:host should work.

TAUX ought to be taught to to more checking, or less checking.
Sigh!
/Carl
Received: from C36.Tymnet by X930.Tymnet; Mon, 20 Jan 86 16:02:27 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Mon, 20 Jan 86 15:57:13 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: BIONIC.SAV 

Carl,
    Here's Ingrid's confirmation of distribution/installation of BIONIC.  She
was unable to install in FTSYS on CEGI hosts, because these hosts do not have
FTSYS.  What shall I advise her to do with the copies she has on the CEGI
hosts?
                                                                       Thanks,
                                                                       Dennis

(begin forwarded message)
----------------------------------------------------------------
From: IPC.I/DOLOVITCHENYJ@Ontyme.Tymnet 
Date: 20 JAN 86 11:27:19 
To: TXS.D/COFFEY@Ontyme.Tymnet 
Message-id: A03666@Ontyme.Tymnet 
Subject: BIONIC.SAV 

                         M E M O R A N D U M





DATE>      20 JAN 86  11:26

TO>        DENNIS COFFEY

COPIES>    

FROM>      INGRID DOLOVITCHENYJ


SUBJECT>   BIONIC.SAV


-----------------------------------------------------------------------


     Hello, Dennis,
    
     As of Friday noon the file BIONIC.SAV was placed on all FTSYS
     directories on all domestic PDP-10's and F-3's.  I found that
     the CEGI hosts do not have a directory named FTSYS, so the
     file is in SSINSTALL on those hosts, with the appropriate
     protection and license.   Let me know if you would
     like for me to place the file on another directory on the CEGI
     hosts.
            Ingrid
----------------------------------------------------------------
(end forwarded message)

Received: from C36.Tymnet by X930.Tymnet; Mon, 20 Jan 86 16:09:22 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Mon, 20 Jan 86 16:02:09 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: BIONIC/EBUSS Base/HSA 

    It looks like our friends at Tymnet are beginning to get some managerial
(as well as techie) focus on the KTL problems, as some progress is being made
toward resolution of these problems.  Congratulations on your contribution
toward this resolution---as well as toward this managerial awareness.

/D.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 20 Jan 86 16:21:10 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, BRING, DENCOFF 
Subject: CSORT.HLP 

I located an older manual which described CSORT (the COBOL SORT program).
I enterred the information into (*6NEWS)CSORT.HLP and TELECOPYd it to all
PDP-10 systems.    /JMS
From: FLETCHERC@X930.Tymnet 
Date: Mon, 20 Jan 86 16:53:09 PST 
To: carl, osman, jms 
Subject: timecards 

Would you enter standby time with sub-account 6024 in the future.
I think the other directions are in error.

	Craig
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 28 Jan 86 6:06:35 UT
From: KTL.YASUMA@Ontyme.Tymnet 
Date: 27 JAN 86 22:03:10 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: NTD.P/SPIES@Ontyme.Tymnet, NTD.L/MAURER@Ontyme.Tymnet,
	NTD.A/FENN@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	IOD.BAMBERG@Ontyme.Tymnet, TUK.WOKING@Ontyme.Tymnet,
	TT.EM@Ontyme.Tymnet, IOD.CUPERTINO@Ontyme.Tymnet,
	ACS.G/MASTERS@Ontyme.Tymnet, TYMOPS.J/SNAVELY@Ontyme.Tymnet,
	ACS.T/SILVA@Ontyme.Tymnet, NSS.R/SPRENKEL@Ontyme.Tymnet,
	NSMS.G/SMITH@Ontyme.Tymnet, NSS.J/GOMES@Ontyme.Tymnet,
	TNS.DOCQUIER@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NSS.N/SHUKLA@Ontyme.Tymnet, NSS.G/WALKER@Ontyme.Tymnet,
	EDI.L/BAIRD@Ontyme.Tymnet, ISIPR.G/MARUS@Ontyme.Tymnet,
	CT.AMSTERDAM@Ontyme.Tymnet, TUK.WOKING@Ontyme.Tymnet,
	TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TYMOPS.B/TOMASI@Ontyme.Tymnet, ISIPR.LITTLE@Ontyme.Tymnet,
	CORP.MAIL@Ontyme.Tymnet, NTD.DS@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.OSAKA@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet,
	CT.HQ@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A08905@Ontyme.Tymnet 
Subject: "It looks that our clients in Europe"... 

It looks that our clients in Europe returned back on air for the
present. 
Thank you very much for your support on this matter.

I hope that some troubles to be remained will fix soon.

Best Regards,
Tak

DISTRIBUTION
============
  Peter Boraston  (TUK.WOKING)
  Albert Handelmann       (TT.EM)
  Peter Haas      (IOD.CUPERTINO)
  Gary Masters    (ACS.G/MASTERS)  
  Jack Snavely    (TYMOPS.J/SNAVELY)    
  Terry Silva     (ACS.T/SILVA)    
  Rick Sprenkel   (NSS.R/SPRENKEL) 
  Gordon Smith    (NSMS.G/SMITH)   
  John Gomes      (NSS.J/GOMES)    
  Thierry Docquier(TNS.DOCQUIER)   
  Mike Rude       (NTD.M/RUDE)
  Nermala Shukla  (NSS.N/SHUKLA)   
  Gary Walker     (NSS.G/WALKER)   
  Larry Baird     (EDI.L/BAIRD)    
  Greg Marus      (ISIPR.G/MARUS)  
  Tony Bakker     (CT.AMSTERDAM)   
  Keith Roberts   (TUK.WOKING)
  Kuni Kamoi      (KTL.TOKYO) 
  Dennis Coffey   (TXS.D/COFFEY)   
  Carl Baltrunas  (TXS.C/BALTRUNAS)
  Bob Tomasi      (TYMOPS.B/TOMASI)
  Pete Little     (ISIPR.LITTLE)   
  Katherine Kalocsai (CT.HQ)
  Francis Bonnin  (CT.DPM)
  J-F Guillou     (CT.DPM)
  Dave Smith      (NTD.DS)    
  Makoto Ogura    (KTL.TOKYO) 
  Shuichi Inoue   (KTL.OSAKA) 
  Nob Hashimoto   (KTL.TOKYO)
  Linda Maurer (NTD.L/MAURER)
  Al Fenn (NTD.A/FENN)
  Phil Spies (NTD.P/SPIES)
  Patrick Driscoll (NTD.P/DRISCOLL)
  Ron Bamberg (IOD.BAMBERG)
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 30 Jan 86 0:09:22 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: 24XP14 works. 

Tracy's diagnostics wrote over part of SYS, I had to refresh the pack
(but only restores SYS, no others UFDs).  The /P14 monitor correctly
identifies what type of disk it is talking to, I currently have it
set to ignore the blocks pack.  I used the old BOOTS to load the new BOOTS,
the new one comes up, but can't read any disk.  

Either there is something with the paper-tape punch on 57, or PTPSER is
sending extra nulls.  I could not get the old BOTLOD program to punch
out an exact duplicate of the current papertape bootstrap.
Therefore I cannot create a new BOTLOD tape for testing.
I will try the PTP on 22 and 70 next.

I found another reason why DSKCLN needs to be run by the monitor 
instead of by INITIA - if (OPER)[1,21].UFD is bad, INITIA dies
and does not run DSKCLN, therefore the UFD does not get fixed.
Its even wors when the STRUUO causes a stopcode - you can't even GFD
to OPER.  I had to delete  the UFD by hand.


DSKCLN then ran, and said that (SYS)CRA003.SAV had lost data.
The monitor does not handle lost data well, when COPYCRASH tried to
LOOKUP CRA003.SAV, FILIO hung.  All PI's disabled, PI 3
currently in progress, IO bus request on PI 1, program request on PI 7.

I will probably go straight to CUP-C tomorrow (Thursday).
/JMS
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 30 Jan 86 14:04:00 UT
From: TUK.BRENTFORD@Ontyme.Tymnet 
Date: 30 JAN 86 04:57:52 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A10960@Ontyme.Tymnet 
Subject: Tape creation from host F58 

                                                     [-44]
        McDonnell Douglas               TUK.LONDON     (01) 379-7822
        Information Systems Limited         BRENTFORD  (01) 568-4277
        (United Kingdom)                    WOKING     (04862) 71311

DATE>      30 JAN 86  12:55

TO>        Neil Sullivan(ENS.NS)

COPIES>    Gary Masters(ACS.G/MASTERS)
           Ron Bates(ACS.R/BATES)
           Carl Baltrunas(TXS.C/BALTRUNAS)
           Dave Smith(NTD.DS)
           Peter Haas(IOD.HQ)

FROM>      Roger McGinley(TUK.BRENTFORD)


SUBJECT>   Tape creation from host F58


-----------------------------------------------------------------------


Neil,
    I am having a problem writing some files to tape which were
originally created by International Support for;

Mercury Communications Ltd.
Ninety Long Acre,
London WC2E 9NP.

The filenames were 'MCL105.ALL' and 'MCL115.ALL', and were created
for username 'MCLACCTG' on host F58. In creating the tape, I am using
a record length of 255, block length 2550, and EBCDIC format. The
files are being written to our remote DCS system located in;

Kew Bridge House,
Kew Bridge Road,
Brentford, Middx,
England TW8 0EJ.

The problem is that for each record of 255 chars being written to tape,
2 blank records are also being added, hence giving 3 times the number
of records there should be. Therefore our client i.e. Mercury Comms
                                                                Page  2

is unable to process the tape. At the host end I am using the command
"R PUNCH\'FILE' FIXED 'RECL'" which works perfectly on host C55. I
have been informed by Gary Masters that both hosts are using the same
base code.
   Is it possible that the files were created incorrectly, or maybe they
have carriage returns at the end of each record. I would appreciate any
help you could give me with this problem as I don't think that it is
a problem with our remote DCS system.

Regards,
      Roger Mc Ginley.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 31 Jan 86 19:06:51 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: 3650 status 

I have a hacked version of BOOTS (named BOOTX) that reads from pages
packs only.  It works, but currently it is read from the blocks pack,
then it reads the monitor from the pages pack.  

The paper-tape punch on C70 works, I have some more debugginf go do
on BOTLOD.

Talked to Fred Wright at Systems Concepts, he says that the 3350's
cannot be completely reformatted by outputing a predetermined string
of bytes like the 3330's.  To rewrite the HA (home Address), you have to
read the existing HA, read the sense bytes from that read, merge the
sense byte data with the new HA data, then rewrite the HA.

He also mentioned that the location CRECL has the numbers of words per
record.  Setting it to 1000 will format the pack in paged.  
Unfortunately that causes a length error unless a cople of locations are
kludged up.  I haven't tried the kludge yet.

I formatted all but 3 disks, and will try creating a 10-pack system.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 31 Jan 86 23:21:02 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: C24 has 1 million pages free 

DSKB consists of ten 3650's, it's up, and running 3 jobs running
DIRECT and CKSUM to exercise the disks.

/JMS
From: Osman Guven <osman@X930.Tymnet> 
Date: Sat, 1 Feb 86 18:56:03 PST 
To: jms, Carl A Baltrunas <carl@X930.Tymnet>, dencoff, fletcherc 
Subject: ..Information.. 


1. I will be on standby as the schedule indicates (2/3 to 2/9)
2. It was my turn to do the month end ALL-FILES on X930 and
   came in saturday and done it.
3. While doing ALL-FILES node 6657 crashed had the recycle system.
4. NEW F3: I am not sure what stage is the micronode-tymnet 
   line status is (Dennis Coffey was looking into it) but, system
   is usable from consule to build one pack system etc..

Osman..
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Feb 86 2:30:58 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Dallas /P monitors 

Due to concern over the E-BUS crashes Dallas (Marconi) called and wants
us to quickly look at the dumps and then have the files removed to get
some disk space back.

   *** PLEASE, DO NOT DELETE THEM WITHOUT TALKING TO ME /CARL ***

Also, Marconi asked me to cut /P monitors for all the Dallas machines
due to pressure from Bill Fisher (spelling?).  I told him that we would
build them, put in the appropriate patches and dispatch them to the
systems sometime beginning Monday (today).  I vote that all STOPCDs on
the dallas machines cause a reload after the message is typed.  Once we
find a solution to the DIEREH/CRASH/147 crashes we can release a new
version of the monitor.

I expect to be in by noon... if anyone needs me earlier, feel free to
call me and wake me.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Feb 86 2:38:24 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: BIONIC Release to (SYS). 

Dennis,
  I've told everyone that the planned release of BIONIC %1(7) to the
SYS directory would commence by informing Ingrid to move the (FTSYS)
version to (SYS) today, February 3.

  If you can contact Ingrid and have her begin the move, starting with
system 55, 34 and 57, please...  Thanks!

  Also, we need to ONTYME to Terry Silva ?<EDI.T/SILVA@ONTYME>? and
Greg Marus ?<?>?  (See previous BIONIC messages for proper addresses)
so that they may inform the people and customers that they support.
/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 3 Feb 86 10:02:39 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, JMS, DENCOFF, FLETCHERC 
Subject: CRASH FILES.. 

SORRY, BUT I HAVE ALLREADY DELETED THE CRASH FILES LAST WEEKEND
BECAUSE OF "DISK STORAGE SORTAGE". IF WE NEED THEM WILL NEED 
TO GET THEM FROM ALL-FILES OR BACKUP TAPES.

OSMAN..
Received: from C36.Tymnet by X930.Tymnet; Mon, 3 Feb 86 11:05:46 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Mon, 3 Feb 86 9:40:58 PST 
To: Ingrid Dolovitchenyj <IPC.I/DOLOVITCHENYJ@Ontyme.Tymnet> 
Cc: isipr.g/marus@ontyme, acs.t/silva@ontyme, carl 
Subject: BIONIC.SAV 



                               MEMORANDUM


                                                       TYMCOM-X SUPPORT

DATE:  3-Feb-86

  TO:  Ingrid Dolovitchenyj (Software Distribution and Installation)

COPY:  Terry Silva (ACS), Greg Marus (International, Pacific Rim)

FROM:  Dennis Coffey  (Tymcom-X Support)

SUBJ:  Transmittal (Move) of New BIONIC to SYS Directory.

-----------------------------------------------------------------------

Ingrid:

The field test of the new version of the BIONIC program has been completed
with full success.  This version incorporates the functions of the old ASCII
BIONIC program and the KATAKANA BIONIK program.

Please complete the transmittal of the new BIONIC preogram:  move the file
BIONIC.SAV from the FTSYS directory on all TYMCOM-X hosts with the new version
to the SYS directory on all these hosts, deleting the older version in SYS in
the process.  The first hosts to complete this on need to be hosts 55, 57 and
34.

Greg & Terry:

I thought you might like to know about this change, so you can provide
proper notification to customers.  Although this version provides greater
functionality, reliability and speed than earlier versions, the change from
previous versions is not completely transparent to users.  If you've any
questions about that you may contact me.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Feb 86 16:41:41 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: Re: Crashes deleted 

No, we don't really have the time to look at any of these thoroughly,
I was just hoping to see if we can pinpoint a pattern and look at the
crashes in detail.

In particular, block-io request negotiations and block output requests
to the base seem to be crashing the base....
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Feb 86 19:58:39 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Node 2770 up! 

I finally got node 2770 up and running.  I swapped hardware around with
Dennis' blessing and he re-cut code for it with the swapped hardware. I
put the DUV-11 board from node 555 in place of the MDB DUV-11 look-alike
and replaced the M9400 (REV11-A) bootstrap with the M8012 BDV11 board
from the 2-cabinet micronode.  I found another DUV cable and used that
while trying to find out why it wasn't loading.

Turns out it's supposed to be plugged into slot 3 (0,1,2,3 : 4th) from the
left (and not from the right) on node 3143.

We need to do the following:

    1)  Make a new monitor for system X118 with ^D33 ports.
    2)  Make a TITO save tape.
    3)  Make sure the disks are formatted (I think they are).
    4)  Define a 2 or 3 pack structure.  I vote for 3.
    5)  Refresh & rebuild it from 930's all-files tape, selected directories.

=====================================================================
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Feb 86 20:35:28 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: New system code "FX" for 118 


From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Feb 86 23:08:12 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: FX monitor built (m33)FXXP15.SAV 

Joe,
  Any reason P15 shouldn't work?  If not, it's ready to go.
If so, then we should simply build a /P from sources in (OSP).
Either a monitor tape, or just copy it across with TITO, then
we can bring the other system up on the network.

When loading, there were lots and lots of "lost" packets according
to the LOADII program.  I donno if that means we have a flakey line
or not.  We'll find out later.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Feb 86 23:12:14 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: uCode only handles 17 ports? 

You put a note (I presume you, correct me if I'm wrong) in the CONFF3.MAC
file saying that the microcode only works for 17 ports?  Uhhh... system
897 has been configured for 32 ports (in the base) and 40 in it's monitor.
[I did the same for 118's monitor since it also has 32 ports in the base
code.]  Where did you come up with your count?

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 4 Feb 86 0:22:51 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: uCode only handles 17 ports? 
In-reply-to: your message of Mon, 3 Feb 86 23:12:14 PST

I think the comment is a misunderstanding.  CONFCA has 40 ports on 897.
I don't remember adding that PRINTX.  May have, fuzzy thinking, see next msg.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 4 Feb 86 0:24:28 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: May not be in Tuesday. 

I feel like I may be coming down with something.  If it's not better, I won't
be in to work tomorrow.  I can be reached at my home phone (415)790-0608.
/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 4 Feb 86 1:56:23 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: System Pack-Set & FEFILE 

Please see my comments on (M33)FEFILE.NWS and PSETID.NWS about the
proposed changes to the HOM page and unit data blocks for the changes
needed to standardize BOOTS and FEFILE, as well as provide a beginning
solution to the problems we are going to see with non-removable packs
and larger disk systems.

COMMOD.MAC has already been changed to reflect these new entries.
     I re-used some of the SWAP pointers, HOMK4S > HOMSPS, HOMSWP > HOMFEF
     and UNISWP > UNIFEP.

REFSTR, UUOCON and KSFEFS still need to be modified.
KSBLOD has already been modified to use BOOT pages 3,4,5 as all other
processors already do.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 4 Feb 86 2:04:58 PST 
To: Osman Guven <osman@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet> 
Subject: Monitor meeting... 

Craig is in class... we ought to get together to bring each other up-to-date
on all the things going on... but I donno if it'll be as early as 10:00am
as usually scheduled.

See you all when I get in.

/carl
From: FLETCHERC@X930.Tymnet 
Date: Tue, 4 Feb 86 11:01:52 PST 
To: jms, carl, osman 
Subject: 3650 schedule 


Talked with Ed Roop today and he wants to wait until we can fully support
native mode before bringing up 3650's on system 26.  Therefore he wants
an estimate of how long he will have to wait, which implies a schedule for
the remaining tasks.  Carl and Joe, on Wednesday afternoon if you are both
in I would like to go over the remaining tasks and produce a schedule.

	Criag
From: FLETCHERC@X930.Tymnet 
Date: Tue, 4 Feb 86 11:05:36 PST 
To: carl 
Subject: PERP 

Ed Roop advises that 940's may go away before the end of 1986 so we need
to begin testing perpl (xexec) on -10's as soon as possible, say March.


	Craig
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 4 Feb 86 16:05:36 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: Schedule? 

Are you still in class?  I thought that you class was over this week...
Drop me a line...  /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 5 Feb 86 0:27:13 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: System 118 

System 118 is being rebuilt from 930's last all-files tapes made by OSMAN
this past week-end.  Whomever come in earliest, please put up the next
tape(s) and continue the restore.

The plan is to make a duplicate 930 file structure, then to delete all
the monitor sources from 118 and delete all the applications sources that
are on 930 (leaving them on 118).  There of course, will be some small
duplication, but that chan be pared down as time goes by.

/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 5 Feb 86 8:19:04 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Schedule? 
In-reply-to: your message of Tue, 4 Feb 86 16:05:36 PST

YES, I AM STILL IN CLASS. TTE 330 WAS A TWO WEEK CLASS AND
THIS WEEK IS THE  LAST. I PROBABLY SEE YOU FRIDAY.

OSMAN..
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 5 Feb 86 22:05:38 UT
From: NSS.J/SMITH@Ontyme.Tymnet 
Date: 05 FEB 86 12:12:38 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: M57245@Ontyme.Tymnet 
Subject: "Carl, It wasn't ONTEX that needed"... 

Carl,

It wasn't ONTEX that needed changing, it was AUP.  After all the effort
to find it, I just noticed a note from Lynne that the dimension needed
increasing.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 6 Feb 86 1:20:08 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>, William R Soley
	<wrs@C39.Tymnet>, Peak Maintainer <Peak@X930.Tymnet> 
Subject: System 118 up 50000+p free 

System 118 is up!!!
I've deleted major portions of (M33),(OSU),(KSBACK) and a few more,
completely deleted (OSP),(OSB) and a few more, removed CTMKTG1 and
CTMKTG7 from the system, and trimmed as much as I could find out of
"Monitor-only" directories.

The result: 50,000 +++ free pages on system 118!

!da
6-Feb-1986 1:19:15 PST

!sysno
TYMSHARE X118-P034/P15 2-3-86
! core 0
! core
0:0+0/512 pages
Vir. core left=51004


Sigh!

/Carl
Received: from C36.Tymnet by X930.Tymnet; Thu, 6 Feb 86 9:00:33 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Thu, 6 Feb 86 8:19:58 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: 118 

I tried to log in to 118, but found it shut, to me.  I guess I've got to be
validated on its one-node micro-network, yes?  Will you get that done?

50K pages!!  You do good work!  Congratulations!  I believe I should make it
my home system, yes?  I think it would be good to also validate Bruce Ring
(BRING), yes?

Shall I (or will you) have Bill Soley include 118 in the TUMS world?
Received: from C36.Tymnet by X930.Tymnet; Thu, 6 Feb 86 12:03:34 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Thu, 6 Feb 86 11:56:02 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: User's LOGINN problem. 

  I got a call from Fred Canning.  His username on host 74 is somehow tied up
in a way that won't let him log in.  He tells me that he has 2 detached frames
running LOGOUT, both in SLeep state.  He cannot attach to them, and he cannot
log in to a new frame.  He had operations try to KJ the sleeping detached
frames, but they can't.

  I don't know how to help him.  Can you give me some help on this?

  Thx! /D
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 6 Feb 86 15:14:47 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: Bug in PAKCOP's routine to validate HOM blocks. 

With this bug, I don't see how PAKCOP ever worked, unless the version on
tape was patched but the source wasn't.

File 1)	DSK:PAKCOP.RSV	created: 1855 06-Dec-85
File 2)	DSK:PAKCOP.MAC	created: 1503 06-Feb-86

1)59		HLRS DAT
1)		MOVSI T1,(BYTE (8) 200)
**** ;At RDVNXB+24L
2)59	;*;	HLRS DAT	;*WRONG* LH has CYL,HEAD,REC bytes
2)		HRLS DAT	;RH points to the page of data just read in
2)		MOVSI T1,(BYTE (8) 200)
**************
1)64		 JRST FMTER2	;COULD NOT SET ALTERNATE TRACK FORMATS
1)		MOVEI	CYL,<CYLS/2>-1	;3330-1 (100 megabyte = RP04)
**** ;At FMTDO+3L
2)64	;*;	 JRST FMTER2	;COULD NOT SET ALTERNATE TRACK FORMATS
2)		  OASC [ASCIZ/ ATTEMPTING TO FORMAT REST OF PACK ANYWAY
2)	/]
2)		MOVEI	CYL,<CYLS/2>-1	;3330-1 (100 megabyte = RP04)
**************
1)66		 POPJ P,	;DO NOT MARK UNFORMATTED IF BAD
1)		HLRS DAT
**** ;At VERIFY+3L
2)66	;*;	 POPJ P,	;DO NOT MARK UNFORMATTED IF BAD
2)		OASC	[ASCIZ /ATTEMPTING TO VERIFY REST OF PACK ANYWAY
2)	/]
2)		HLRS DAT
**************
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 8 Feb 86 21:33:20 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, dencoff, Osman Guven
	<osman@X930.Tymnet> 
Subject: Silicon Valley Digital PC Users Group 

Meets at 7:30pm at DEC, 2525 Augustine Drive in Santa Clara.

Digital will host the meeting this time.  There will be hands on demonstrations
of various pieces of software on several PC's.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Feb 86 22:53:15 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Changes ... 

COMMOD: Decided 
Not to put FEFILE.SYS rib into HOM page 'cause there is
	only space for one file and I'm putting SPAGES.SYS in place of
	SWAP.SYS, so... I removed HOMFEF==:21 and put in HOMSPG==:21 and
	HOMFEP==:14 which contains #pages,,1st-page pointer which is
	later copied into UNIFEP with all the other unit stuff.  For now,
	I've decided to add the FE pages to HOME.SYS until we come up
	with a better idea.
Not yet documented in MONDOC/P034.MEM ... will do that when it's all
ready for testing/use.

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 11 Feb 86 23:55:04 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: TXS.SUP 

Does MDC offer special discounts on PC's?  What is the policy on leave of
absence?
Received: from X95.Tymnet by X930.Tymnet; Wed, 12 Feb 86 0:01:38 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 11 Feb 86 23:59:28 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: 95XP15 does not work 

It gets an EUE immediately after "RMA0 FIRST RIB PAGE CONSISTANCY ERROR".
What is it looking at?  The RIB to SAT.SYS?  Is it looking at page 10?
If so, why isn't following the pointer in the HOM page?
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 12 Feb 86 0:22:34 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: ASP works, PAKCOP does 

Patched BPXKON and 24XP16 to get ASP to work (details later).
The old copy routine in PAKCP5 works, "OLDC"+"DOIT".
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 12 Feb 86 0:24:16 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: CAN NOT ADD UNIT TO STRUCTURE. 

Tried to add BPA15 to the 12-pack system, FILES reported "CAN NOT ADD UNIT
TO STR".  What can cause that message?  I did an ADD with 24XP14 with no
problems.  Anyway, once the system was up, I could access the unit even tho
it was not part of DSKB:

	!RUN (MPL)FILDDT
	File: BPA15/U
	2001$6T/BPA15

Received: from C39.Tymnet by X930.Tymnet; Wed, 12 Feb 86 0:31:44 PST
Return-path: <CARL@C39.Tymnet> 
From: CARL@C39.Tymnet 
Date: Wed, 12 Feb 86 0:06:09 PST 
To: carl, jms@c930, osman@c930, fletcherc@c930, smithde@c39 
Subject: PDP-10 SCNSER BUG 

It's "official", that one of the major causes of the base crashing at
the moment is caused by a timing problem in SCNSER.  What happens is
this:

  o  The user sends a BLOCK-IO OUTPUT request at PDP-10 UUO level.
  o  The circuit gets zapped out in the network.
  o  The base tells us via the input ring that the port is zapped.
  o  The monitor clears out the LDB for the port and flags that a
     zapper needs to be echoed.
  o  The monitor now goes to process the BLOCK-IO OUTPUT at SCNSER
     interrupt level, picks up the BLOCK-IO output buffer address
     (which incidently has been zeroed by the circuit zap code)
     and proceeds to build an output message using 16 bits of the
     port LDB address as the character count and 6 as the buffer
     address.
  o  The base tries to process this message and gets itself crashed
     by ISIS after messing up the rings with an incredibly large
     message.
  o  In the process of going down, the base "sometimes" takes the 10
     down; other times the 10 is crashed by DEP 1 in 30.

This is NOT a new bug.  It has been happenning for quite a while.  The
Varian base used to attempt to process this and generally went on it's
merry way after outputting only a single byte (most probably a null).
This bug has caused crashes in every version of our monitor running with
the new bases.

The fix is for the PDP-10 to NOT put the bad message type into the ORING
by making additional checks at interrupt level to determine whether the
port has been zapped since the BLOCK-IO OUTPUT request was issued, but
prior to the actual data transfer.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 12 Feb 86 3:48:50 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: BIO-ZAP patch 

I've made patch #9, BIOZAP.PAT and installed it on systems C26 & D25
both in SYSTEM.SAV and the running monitor.  The patch addresses two
areas:
  1) STOPCD if T3/ LDBBIO(U) = 0 and a zapper was not seen
                                 if seen, then act if no room for data
     simulates STOPCD(DEBUG,BIOZAP)

  2) GETBIO routine ought to check if the port it's allocating a buffer
     on is really there (not zapped).

If C26 & D25 seem to run "clean" then I will propagate the patch to all
the other systems and fit the fix into the sources.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 12 Feb 86 3:54:58 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: BIO-ZAP addendum 

PS. System 930, has this patch installed in the running monitor xxxx-1.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 12 Feb 86 10:52:23 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: BPXKON is stupid. 

It's so dumb that it treats "RECORD NO FOUND" as if it were a bad spot on the
disk, and blithely assumes that the record will reappear if it waits long
enough.

The problem that kept ASP restore from working is the same one that caused
"I/O error to page 124" when I tried to "SAVE (SYS)NEWMON" on C24.

If FILIO tries to output more than 21 contiguous core pages to contiguous
disk pages, BPXKON has to split the request up into 3 or more requests of
7 pages each (7 pages per track).  The routine that does this made a bad
assumption.  Since the size of the command list area is big enough to handle
one cylinder on a 3330 in blocks (1 cyl = 380 blocks), then it thought it
could use the amount of space remaining in the command list to calculate the
end-of-cylinder boundry.  But 380 pages is 1.8 cylinders on a 3650 (and 4.0
cylinders on a 3330 in pages).  It correctly built the command list to transfer
pages 1-7 of track 0, 1-7 of track 1, 1-7 of track 2, but then added 8-10 of
track 2.  The disk complained that record 8 does not exist, but stupid BPXKON
treated it like a bad spot on the disk.

A three-line patch to BPXKON solved this problem.

An ASP dump stores the first 317 cylinders on one tape and 238 on the 2nd (total
of 555 cylinders).  An ASP restore takes 26 minutes, including the time it takes
to rewind the tape and mount the second one.

/JMS

P.S. The old copy routine in PAKCP5 ("OLDC" and "DOIT") works, takes somewhere
between 25 and 30 minutes to copy one 3650 pack (I wasn't watching when it
finished).
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 12 Feb 86 14:10:08 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: ADD PACK in FILES 

I verified today that 24XP14 can be used to ADD a pack, but 24XP16
can't.  System 24 now has a 13-pack DSKB:, 1.4 million pages.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Feb 86 3:45:59 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: ADDPAK vs REFRESH 

Joe,
  The code in ADD UNIT tests a lot of things and all go to CANTAD which
prints out the message CAN NOT ADD UNIT TO STRUCTURE.  However, I suspect
that it took the error return from PAKREF.  This is because there is an
AOBJN pointer that says to read the RIB for each of the per-pack files
which were (2) SAT.SYS, HOME.SYS and now are (3) SAT.SYS, HOME.SYS and
SPAGES.SYS.  In PAKREF, at PREF2 + it says to error return if the rib is
blank... and in the OLD structures SWAP.SYS has been obliterated.  You
need to REFRESH first to setup the rib for SPAGES.SYS and then an ADDPAK
should work fine.

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 14 Feb 86 21:10:30 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: HOTLINE call for C36 

14-Feb-85 18:30/JMS
Contact:  Cupertino BLDG C, OPS, Debbie Hitower, (408)446-7705, System C36

Symptom:  DSKCLN stopcode FILUUO(042522) at FUNM+6 reading LUD.SYS[1,4]
          Ran FULL DSKCLN, it reported "FILE 1,1.UFD[1,1] BAD DATA IN RIB"

Action:   Brought up system MANUAL, ran DSKCLN in check mode, no errors.
          Brought up system to run FULL DSKCLN, got BAD DATA again.
          Brought up system READONLY, ran TSTSUP, RIB looks fine.
          Set 1B17 in STATES, tried running DSKCLN, system answered.

          May be an intermittant disk read problem, system is now up.

/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 15 Feb 86 20:06:33 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	OSMAN 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Rainbow meeting Thursday, Feb 20th. 

The San Francisco Bay Area DEC PC User Group meets 6:30pm to 8:30pm 20-Feb-86.
At DEC, 31st floor, 101 California Street, S.F.
(BART costs $2.80, leaves at 5:00 and 5:20, goes to 1/2 block from meeting.)

RAINBOW: A presentation on Local Area Networks and external disk storage for
the Rainbow is tentatively scheduled to be presented by Network Sciences
& Corvus.

PRO-350: Discussion of P/OS 3.0 and conversion of DECUS Public Domain
software for the PRO.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 17 Feb 86 0:43:27 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: 24XP16 works, loads BOOTS 

C24 is up and runnin from NEWMON.  It is running Osman's (OPER)SPACY1
and SPACY2.  I will merge MONBTS into BOTLOD tomorrow and have a new
paper tape punched.

/JMS
Received: from C36.Tymnet by X930.Tymnet; Mon, 17 Feb 86 11:00:43 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Mon, 17 Feb 86 10:22:16 PST 
To: Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven <OSMAN@X930.Tymnet>, Joe
	Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet> 
Subject: 118's New Configuration, per Tymnet. 

  We have our answer from Tymnet, re. the F-3's network configuration:

      The F-3's new host number:		   62
      The LSI-11 micro-Tym-Base's node number:	10115
      The LSI-11's Kernel-host number:		 7964
      The neighbor node:			 3143

  Note:  the physical port on 3143, to which node 10115 (the micro-Tym-Base)
      is attached doesn't matter:  in Tymnet II the engine's software
      dynamically associates the physical port number on which it finds the
      neighbor with the logical port number to which the nieghbor is assigned
      in its .TYM file.  The only time it matters is placing the clock jumpers
      on the proper sync board pins.

  We can install everything, even before Tymnet Operations cuts the changed
      code for 3143, as long as we maintain 2770 until we have all the code
      for the LSI-11 and the F-3 with the new numbers.

                                                                   /Dennis
Received: from C39.Tymnet by X930.Tymnet; Mon, 17 Feb 86 14:43:22 PST
Return-path: <SMITHDE@C39.Tymnet> 
From: SMITHDE@C39.Tymnet 
Date: Mon, 17 Feb 86 14:37:44 PST 
To: carl@x930 
Subject: SCNSER bug patch 

Has the patch for fixing the block output problem been deployed yet?
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 17 Feb 86 18:59:48 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: List of PDP-10 systems in (PJ)JOBS.DAT 

Hosts 62 and 118 have been added to JOBS.DAT, host 897 has been disabled.
Received: from C36.Tymnet by X930.Tymnet; Wed, 19 Feb 86 11:51:13 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Wed, 19 Feb 86 11:35:40 PST 
To: Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven <OSMAN@X930.Tymnet>, Joe
	Smith <JMS@X930.Tymnet>, Bill Mortenson <HELGEM@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet>, Tymcom-X Acct. Sup.
	<TXSSUP@X930.Tymnet> 
Subject: Cegi Tymshare accounting project 

(begin forwarded message)
----------------------------------------------------------------
Received: from C26.Tymnet by C36.Tymnet; Tue, 18 Feb 86 4:01:48 PST
Received: from X930.Tymnet by C26.Tymnet; Tue, 18 Feb 86 2:05:56 PST
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 18 Feb 86 10:01:53 UT
Return-path: <CT.E/JAY@Ontyme.Tymnet> 
From: CT.E/JAY@Ontyme.Tymnet 
Date: 18 FEB 86 00:30:30 
To: TXS.D/COFFEY@Ontyme.Tymnet 
Cc: TXS.D/COFFEY@Ontyme.Tymnet 
Message-id: A22609@Ontyme.Tymnet 
Subject: Cegi Tymshare accounting project 

 
The following was recieved from Ken BeDell.  Paragraph 1 refers to items that
we (Carl & I) have been supporting Ken and Eric on.  I have given an interim
answer to Ken, but decision is needed re. their access to 930.

/Dennis
----------------------------------------------------------------------------
 
 
  -----------------------M E M O R A N D U M------------------------------
  McDonnell Douglas Information Systems                 Business Management
  telephone  ST-CLOUD [33](1)46.02.70.12                Ontyme CT.TECHSERV
  ------------------------------------------------------------------------
 
  Date:      February 18th, 1986 - 9:27 a.m. CST
 
  To:        Ken BeDell (IOD.CUPERTINO)
 
  Copies:    Dennis Coffey (TXS.D/COFFEY)  Bill Fischer  (IPC.B/FISCHER)
             Bob Jacobs    (IPC.B/JACOBS)  Jan Krivanec  (IPC.J/KRIVANEC)
             Sateesh Lele  (MIS.MAIL)      Pat Lue       (MIS.P/LUE)
             Nikki Marsh   (IPC.N/MARSH)   Etienne Bouillot (CT.ACT)
             J.F Guillou   (CT.DPM)        Yves Menard      (CT.TECHSERV)
             Peter Stratman (CT.DPM)       Patrice de Beranger (CT.ACT)
 
  From:      Eric Jay (CT.TECHSERV)
 
  Subject:   Cegi Tymshare accounting project
             answers to December memo
 
  Reference: A79562 dated 10-DEC-85 from IOD.CUPERTINO
             A16297 dated 07-FEB-86 from IOD.CUPERTINO
  ------------------------------------------------------------------------
 
 
  Ken, receive my apologies for answering so late to your December memo.
 
  1.  Oper names  for host 930 to  access INFO database: copying  930 INFO
      database to system 90  will not work. System 90 is  due to be defin-
      itively taken out of the network way before the end of 1986
 
      System 74 (accounting host) might be a better choice
 
      We need essentially EVERYTHING that exists in the INFO database
 
      Removing RF from 930  does not seem practical: as far  as I can tell
      this is used both to access INFO database and to access monitor code
      when weing for a replacement
 
  2.  Accessing on host 74 the Validation database: we will be using user-
      name CTOPSMGR and oper names JAY or STRATMAN (the latter to be vali-
      dated)
 
      Files accessed are: USERDB.DMS, CUSTDB.DMS, ADDRDB.DMS
 
  3.  Current status  of the  project: we  are running  January accounting
      cycle with your and our data in  parallel. We will let you know when
      we do not need your data any more
 
      We can  give you a royalty  report containing connect,  tru (without
      pru), pru (as a separate amount) for prime, nonprime and batch time.
 
      Please provide  a list  of royalty codes  as viewed  by MIS  and the
      relationship between each royalty code and the corresponding code in
      the TYMCOM/370  raw accounting. After reviewing the TYMCOM-370 moni-
      tor notes,  it seems that the  corresponding TYMCOM-370 code  can be
      one of the following raw accounting fields:
 
          Accounting record type 5, field  "PREMPGNM" for the royalty code
          and field "PREMPRU" for the pru amount
 
          Accounting record type 9, field "TRNSPGNM" and field "TRNSCHGS"
 
      Please indicate what kind of record layout you need for your royalty
      data
 
  I hope this answers your questions.
 
    Regards,
    Eric
 
  PS: special note  for Patrice de Beranger: the royalty  report will have
  to be  taken from your  data and sent  to Ken  BeDell. Since we  (YM and
  myself) provide you with detailed data you  are the only one in position
  to provide this report, since you do the totaling

----------------------------------------------------------------
(end forwarded message)

Received: from C36.Tymnet by X930.Tymnet; Wed, 19 Feb 86 11:51:54 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Wed, 19 Feb 86 11:48:07 PST 
To: Ken BeDell <IOD.BEDELL@Ontyme.Tymnet>, Carl Baltrunas <CARL@X930.Tymnet>,
	Osman Guven <OSMAN@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet>, Bill
	Mortenson <HELGEM@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet>, Tymcom-X Acct. Sup.
	<TXSSUP@X930.Tymnet> 
Subject: Eric Jay's Ontyme of 18 Feb (A22609). 



                                  MEMORANDUM


                                                              TYMCOM-X SYSTEMS

DATE:  19-Feb-86  10:26:07

  TO:  Ken BeDell

FROM:  Dennis Coffey  (Tymcom-X Systems)

SUBJ:  Eric Jay's Ontyme of 18 Feb (A22609). 

------------------------------------------------------------------------------

Ken,

      Eric referred to two items to which I can (should?) provide answers:

1.)  PERP replacement:  the project is our number two priority, and should be
completed within about one month (my rough estimate--I'm not directly
involved in it at this time).  We are confident that this is poenty of time
prior to the date expiration of the 940s.

2.)  OPER license access on 930:  Eric's point about the plans to take system
90 out of the network well before the end of the year is news to me, and is a
very valid point.  Their need to access monitor code on 930 will be taken
under advisement.  No answer to this problem immediately, but soon.


      Regards,
      Dennis
~?
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 19 Feb 86 22:18:57 UT
From: NSS.E/RIORDAN@Ontyme.Tymnet 
Date: 19 FEB 86 11:23:01 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: NTS.RVIVIER@Ontyme.Tymnet, NTS.R/VIVIER@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: M61747@Ontyme.Tymnet 
Subject: CORRUPTED .NIB FILE PROBLEM 

TO>  EILEEN RIORDAN
FROM>  BERYL AGUA

SUBJECT> CORRUPTED .NIB FILE PROBLEM

HI EILEEN, JUST WANTED TO KEEP YOU UPDATED ON THE CORRUPTED .NIB FILE PROBLEM
WE ARE STILL EXPERIENCING.  WE HAVE BEEN MAINTAINING A LOG IN PROJECT
IMPLEMENTATION AS OF 1-21-86, WITH AN OUTSTANDING TOTAL OF (41) DOCU-
MENTED OCCURENCES ON BOTH SYSTEM 25 AND 54.  THE LAST OCCURENCE WAS ON
2-13-86.  LAST I CHECKED WITH YOU, THERE WAS NO STATUS ON PROGRESS FROM 
THE MONITOR GROUP INVESTIGATING THE PROBLEM.  ANY RECENT UPDATES?  AS YOU
MIGHT ASSUME, THIS PROBLEM IS SLOWING DOWN THE WORK FLOW IN PROJECT
IMPLEMENTATION WHEN WE HAVE TO STOP AND REGEN A NODE AGAIN, NOT TO MENTION
THE OFF-HOURS CALLS THE CODE GENERATORS ON THE BEEPER ARE RECEIVING.

ALSO, LET'S PLAN TO GET TOGETHER THIS WEEK TO DISCUSS ENLARGING THE
TYMNET DIRECTORY.  THANKS FOR ALL YOUR HELP.

BERYL
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Feb 86 15:26:58 PST 
To: Eileen Riordan <NSS.E/RIORDAN@Ontyme.Tymnet>, Ron Vivier
	<NTS.R/VIVIER@Ontyme.Tymnet>, Ron Vivier <NTS.RVIVIER@Ontyme.Tymnet> 
Subject: Corrupted .NIB file problm 

Eileen, Ron, ...
  Ron and I have still not connected on this problem.  I suspect that maybe
a detailed ONTYME message to me "Carl Baltrunas <TXS.C/BALTRUNAS@ONTYME.Tymnet>"
may be the only way that we may connect.
  From the brief description you gave me (Eileen), I cannot believe that this
is a PDP-10 monitor induced problem.  It was assumed that the monitor "change"
which occurred in December when the new bases were installed was, in part, the
major culprit.  Systems 25 and 54 where the problem is occurring are running
a P034/N10 monitor.  The dash number (-7 or -8) is a binary patch to fix a bug
and none of these in any way affects the file system.

  I mention the monitor version P034/N10 simply because that monitor has been
running since MAY of 1985 everywhere without any of the problems you have
reported.

  Our latest monitor, P034/P which will be released to all DALLAS machines
this week has been running in Fremont and Cupertino for months.  Again, there
is plain evidence of the reliability of the monitor itself.  I cannot believe
that any patches which (since December) which have ONLY been introduced to
help prevent the new bases from crashing or losing data could be garbling
your .NIB files.  Someone must have modified one of the utilities utilized
by the code maintainers, if anything at all in the software is suspect...

Ron and I need to connect and discuss this in detail, since I still do NOT
know what exactly is happenning to the files.  If it is a monitor problem,
then it should be easily reproducible by some process which can be shown to
work correctly on another system running another monitor.  Sigh!

  Ron,
    Continue to try to contact me...  Fremont-Liberty (116 tieline)
extension 2515 in the afternoons.  (PS. I'm not in the office today!).

/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 20 Feb 86 0:15:29 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: P16.. 

AS FAR AS MAKING ONE PACK SYSTEM P16 DID WORK OK.
AS A METTER OF FACT SYSTEM UP WITH THE ONE PACK I MADE
THIS AFTERNOON..
Received: from C36.Tymnet by X930.Tymnet; Thu, 20 Feb 86 11:52:13 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Thu, 20 Feb 86 11:30:29 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: CEGI Access to X930 INFO & MONITOR. 

Carl,

      Really, a couple of items:

1.)  I just now got a phone call from Ken BeDell, re.  Cegi's request for
continued RF license on 930.  It appears they may very well need continued RF
license (the only license they have on 930, Ken tells me) per Eric Jay's
Ontyme.  I promised Ken I'd get back to him.  It's not very urgent.

2.)  I'd like to suggest that you change my 'ALIAS' in your TUMS.INI, to
'DENCOFF@C36' from 'DENCOFF@X930'.  I believe I could get your messages
faster:  X930 forwards to C26, which forwards to C36; despite my MAIL.FWD,
which reads "!DENCOFF@C36.Tymnet".

3.)  I hope you're doing better!  Will we see you soon?  Take care of
yourself.

      /Dennis
Received: from C36.Tymnet by X930.Tymnet; Fri, 21 Feb 86 17:12:18 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 21 Feb 86 17:02:41 PST 
To: Craig Fletcher <FLETCHERC@X930.Tymnet>, Carl Baltrunas <CARL@X930.Tymnet>,
	Osman Guven <OSMAN@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet>, Bill
	Mortenson <HELGEM@X930.Tymnet> 
Cc: Tymcom-X Acct. Sup. <TXSSUP@X930.Tymnet> 
Subject: absence from office. 

My family will be arriving from St.  Louis (for the wedding) at 10:58,
Tuesday, Feb 25, at the San Jose airport.  I will be meeting their plane, so I
will be out of the office during the middle of Tuesday, departing here about
10 am (if their plane is not greatly delayed).

	/Dennis
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 21 Feb 86 22:33:57 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: D31 & D37??? 

what was wrong with them??
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 22 Feb 86 1:37:19 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Cc: Osman Guven <osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>,
	Dennis Coffey <dencoff@X930.Tymnet 
Subject: Interesting host numbers. 

The OAG (Official Airline Guide) is available thru host 1158.  1160 is
also interesting.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 22 Feb 86 3:34:57 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: BAD SYSUFD PATCH 

Correction:  The patch to turn off the weird SYS:.UFD = (UFD).UFD does
what we expected... however, we didn't expect that some old accounting
programs actually used that "feature"/"bug" and thus, we cannot patch
it at the moment.

   I went around to most of the Dallas machines with the new /P-8 monitors
and in an effort to just "get it done before I forgot again" I put the
instructiuon  "CAME T1,MFDPPN" back where it was patched out of both the
running /P-8 monitors and (SYS)NEWMON on
  27, 31, 37, 56 and 65.   Just fixed (SYS)NEWMON on 25, 72 and 79.

DID NOT FIX systems 23 and 54 since they were unavailable for some reason
at this time.  Someone (OSMAN,JOE) please check them and un-do the SYSUFD
patch:

  FILUUO$:   CHKPP1+2/ JFCL 0   <== replace with CAME T1,MFDPPN

I did NOT update P-8 to P-8 because I was in a hurry and don't feel all
that well at the moment... I was just trying to do it while it was on my
mind.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 22 Feb 86 3:44:41 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: D23 fixed. It just came up. /Carl 


From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 22 Feb 86 21:31:15 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: PERTWEE.TAPE 

3rd Doctor - Jon Pertwee
3A(4) Spearhead from Space	01,02,03,04            1
3B(7) The Silurians             05,06,07,08,09,10,11   1
3C(7) The Ambassadors of Death  12,13,14,15,01,02,03   2
3D(7) Inferno                   04,05,06,07,08,09,10   2
3E(4) Terror of the Autons      11,12,13,14            2
3F(6) The Mind of Evil          15,01,02,03,04,05      3
3G(4) The Claws of Axos         06,07,08,09            3
3H(6) Colony of Space           10,11,12,13,14,15      3
3J(5) The Daemons               01,02,03,04,05         4
3K(4) The Day of the Daleks     06,07,08,09            4
3M(4) The Curse of Peladon      10,11,12,13            4
3L(6) The Sea Devils            14,15,01,02,03,04      5
3N(6) The Mutants               05,06,07,08,09,10      5
3O(6) The Time Monster          11,12,13,14,15,01      6
3R(4) The Three Doctors         02,03,04,05            6
3P(4) Carnival of Monsters      06,07,08,09            6
3Q(6) Frontier in Space         10,11,12,13,14,15      6
3S(6) Planet of the Daleks      01,02,03,04,05,06      7
3T(6) The Green Death           07,08,09,10,11,12      7
3U(4) The Time Warrior          13,14,15,01            8
3W(5) Invasion of the Dinosaurs 02,03,04,05,06         8
3X(4) Death to the Daleks       07,08,09,10            8
3Y(6) The Monster of Peladon    11,12,13,14,15,01      9
3Z(6) Planet of the Spiders     02,03,04,05,06,07      9

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 24 Feb 86 8:37:43 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Cc: Osman Guven <osman@X930.Tymnet> 
Subject: What was the hotline calls? 

My beeper went off 3 times.  What happenned to D25?  I notice that C26 has
only 4000 pages free.
From: IPC.L/THOMAS@Ontyme.Tymnet 
Date: 21 FEB 86 14:44:33 
To: FSC.O/GUVEN@Ontyme.Tymnet 
Message-id: A25933@Ontyme.Tymnet 
Subject: "DISTRIBUTION Eileen Riordan Gary"... 
Resent-From: Osman Guven <osman@X930.Tymnet>
Resent-Date: Mon, 24 Feb 86 9:16:10 PST
Resent-To: jms, Carl A Baltrunas <carl@X930.Tymnet>

DISTRIBUTION
 
Eileen Riordan
Gary Walker
 
Sue Pal
Gary Perez
 
Osman Guven
Craig Fletcher
 
Bill Richardson
 
Jan Krivanec
 
Bill Fischer
 
 
 
MEMO
 
Date:         February 21, 1986
 
To:           Distribution
 
From:         Ed Roop
 
Subject:      SA10/3350 Upgrade
 
 
Everything  appears  ready  to do the 3350 disk upgrade  to  C26.  
Final testing will be completed on February 26th.   At that time, 
Sue  Pal  will put out an Ontyme with specific times and  also  a 
system message.   The migration will take place over the  weekend 
of March 1, 2.  Please notify your field personnel of this event.  
If  conflicts in schedule arise,  please contact Sue Pal  through 
February 26, as I will be out of town until then.
From: FLETCHERC@X930.Tymnet 
Date: Mon, 24 Feb 86 9:50:32 PST 
To: carl, osman, dencoff, bring 
Subject: TYMCOM-X meeting 


Meeting this week will be moved to Wednesday morning at 10 a.m.

	Craig
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 24 Feb 86 14:49:54 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Condition... 

This past week just hasn't been my week.... On Saturday afternoon, I pulled
(or ripped or strained) a muscle in my back and right side.  Since then, I
can hardly move...  we've tried most of the normal treatments for aches and
pains and nothing seems to help.   I have a 3:13 appt at Kaiser in Milptas
to have it looked at.  I don't know if I will be in tomorrow or not.. I will
let you know.

I called in my stand-by hours to Lois, via Mary G & Jon M as 12.8 as Osman
responded to the 2-3 calls while I was incapacitated...  I hope to be back
in the office as soon as possible.

Osman tells me that my latest REFSTR did not work... so I will try to look
over the new code and find out why it doesn't work right.  I may have to go
to Cupertino as wel to test it...  I'll let you all know as soon as I know
anything.

/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 24 Feb 86 16:06:34 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: REFSTR.. 

HALT STOPCD...AT PGEUOR
UUO= 000000,,000000
PG= 000000,,274145
UNIT #(U)= 000000,,000072
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 24 Feb 86 21:27:22 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: TYM444... 

Yes, I have a 444 in the office in BLDG C and you are welcome to 
use it. I will be in BLDG C tomorrow again and see you there when 
ever you get in.

-Osman-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 24 Feb 86 23:49:04 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: Bldg C 

I was planning on getting in around 9am... but one of our cars looks it
may have a blown head-gasket (i hope that's all) so I have to car-pool
with Cherie... Tomorrow she has to go to SF before going into work, so
when she gets back from SF she'll pick me up and I'll probably get there
sometime after 1pm (not really sure... she has to be in SF at 11;30).

I'm expecting to try going through the REFSTR code with DDT to see where
it's dying, then I'll decide if it's easy enough to try to fix and re-test
it again...and again...

  See you then.
/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 25 Feb 86 11:40:52 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: C24 Schedule.. 

Dave S. and John K. wants to use the system around 3 pm
today for base code debuging. Please let me know your
schedule before 2 pm today, call me at (408)446-7705

-Osman-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 25 Feb 86 16:41:29 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: 999-999 -99 

Beeper went off at 4:39pm for "999-9999 -99".  Assumed to be a test.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 25 Feb 86 19:12:50 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	CC:Ring 
Subject: Testing TOPS-10 programs on the 2020. 

Craig,
  To use TOPS-10, just BOOT system 95 from the DSKA and DSKB packs (DSKA
has to be in the drive with the 0 plug).  To log in, type "I" on the TYM444,
INITIA will log in a [1,2] job there.


		/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 25 Feb 86 19:45:20 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: REFSTR 

I believe it is finished.  The problem was that after running off the end
of the unit list, I didn't reset U to point at anything (let alone the first
unit) and PCBIO got upset.  I put in code to reset it, and it worked fine.
I built a 4-pack system, then disolved it, then built a 2 pack system.  I am
restoring the all-files tape to it to prove to myself that it will do all
that it's supposed to... then I will do an ADD-PACK to verify that that will
also work correctly.  I don't expect any problems.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 25 Feb 86 22:20:36 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: 24 re:: REFSTR ? 

The 2-pack system died in the middle of the all-files rebuild.  I'm not
syre why, it got an IME stopcd and I lost the crash by trying to dump it
to the "good" packs instead of the "2-pack" packs.

Anyway, It seems one of the UFD's is bad but that's all I know at the
moment. I just crashed it... will tell all more tomorrow.  /Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 25 Feb 86 23:24:17 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: (M33)SYSTEM.NWS 

What happened to (M33)SYSTEM.NWS?  I was moving TUMS messages to it.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 26 Feb 86 8:53:57 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: (m33)SYSRTEM.NWS? 

???
  I may have acidentally zapped it, thinking it was a product of having been
GFD'd to M33 while reading TXSSUP mail or some such, but I don't remember
deleteing it...

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Feb 86 3:32:37 PST 
To: Osman Guven <osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet>, Craig
	Fletcher <fletcherc@X930.Tymnet> 
Subject: Hours... 

I will probably NOT be in early today....  I've finished a REFSTR that
works as best as the packs will let it.  (It created SPAGES.SYS just fine,
but I think DSKCLN didn't like 2 pages in the file and marked them as
holes in the rib, [I donno what it did to the pages in the SATs]).  I built
a 3-pack system, dumped a bunchof files to it from 930, and thinned the
files down a little.

There's a "working" skeleton system on 24 now... not everything works...
for example, TELECOPY works pulling, but not pushing?  May be a base bug,
but don't depend on it!  SPOOL is not expected to work, etc...

I didn't write a boots, and am not worried about it at present.... we need
to do that Osman,...  

Most of the MONITOR utility programs and such are out there...

  (SYS) has some compilers, etc, PEAK, VUE, TECO, ...RPG, PIP, DIRIT, etc.
 (FTSYS) has TECO 124a, and a few updated things, eg MACRO %53B
  (OSU) has some tools, some things that were in (WRS)
  (MPL) has more tools
 (CARL) and (JMS) have various copies of other things & DEC programs.
 (PJ) has a jOBS.DAT FILE but there is no (TYMNET) files.

Good night....

OSMAN will make an all-files of this system then overwrite it by making
the biggest 3650 system he can without wiping out our 2 1-pack systems.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Feb 86 3:52:21 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: 24XP18 wirks! 

Sew, eye will meke uh 26XP18 for the Kay-El wen eye wake...

-Chow!   /Carl
Received: from C36.Tymnet by X930.Tymnet; Thu, 27 Feb 86 12:54:30 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Thu, 27 Feb 86 12:50:56 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: REFSTR 
In-reply-to: your message of Tue, 25 Feb 86 19:45:20 PST

Carl,
     Congratulations!  You obviously know your stuff, and do good work.
   /Dennis

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 27 Feb 86 17:23:40 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Cc: DENCOFF 
Subject: DENCOFF is not in group 0. 

Dennis is not able to look at interesting hosts (such as 1158 and 1160)
because he is limited to host 1274, host 930, and all the hosts in group 1.
Could we add group 0 to his access list in the MUD?
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 27 Feb 86 17:27:27 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	CC:Ring 
Subject: (TYMNET)NODES.MAP updated for 14-bit node numbers. 

NODES.MAP is now 96 pages long - it has a 3-word entry for all 16834 possible
14-bit node numbers.  All nodes that do not have an explicit name now have
a 9-character implicit name.  The implicit name consists of a space, a hyphen,
the 5-digit node number (from 00000 to 37777), another hyphen, and another
space.  The program (TYMNET)DEFNOD does not list implicit names so that
it is easier to see the defined node names.

Now that the fully expanded NODES.MAP has been telecopied to all systems,
SYSTAT won't get into a loop anymore.  SYSTAT currently does not even attempt
to look for the name of any mode above 7777, which is OK since the base passes
only 12 bits to SCNSER (for LDBLOG).

Try running SYSTAT (with license set) on a "real" system to see the new names.

/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 27 Feb 86 18:53:43 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: I got paid for stand-by. 

I am now all paid up, with the exception of 4 hours call-in on 22-Jan.
This is reflected in (M33)HOTLIN.PAY.	/JMS
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 28 Feb 86 8:50:07 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Monitors 

We need to get the KI & KL monitors with or w/o REFSTR
tu cut today so that we can continue with this weekend
schedule of testing 3650 monitor with KL system. Please
let me know as soon as we have monitor to test.
-Osman-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 28 Feb 86 16:58:59 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Please DOCUMENT your changes!!! 

Please!!!
  Especially any changes which affect how things work.  There isn't any
thing mentioned about making it IMPOSSIBLE to GFD to [1,1] except in the
sources.  I didn't have TSTSUP and I needed to look at some stuff in
[1,1] on 24 and couldn't... 'cause I didn't have a lot of stuff and did
NOT know that you had put in that check.

  I also think that if you have JL you should be able to GFD to [1,1]!!
  ..or WA  ...what do you think? ...  if I can scribble anywhere, I ought
  to be able to get around the software locks so I can see what I want to
  scribble on withot lots of difficulty!

/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 28 Feb 86 18:56:11 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: C24/24xp19.. 


I started to build 12 pack system using 24xp19
I will be checking disk errors with 12 pack system.
Will see what happens.
-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 28 Feb 86 21:31:29 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: c24.. 

I build a 12 pack system with only minimal files.
i will continue to biuld onto same packs tomorrow.
i did not see anything suspecious neither with 24xp19
or disks, see what happens tomorrow.
-Osman-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 1 Mar 86 12:01:43 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Please DOCUMENT your changes!!! 
In-reply-to: your message of Fri, 28 Feb 86 16:58:59 PST

Sorry about that.  The change that made SYS: be [1,4] always was a result of
trying to avoid the problem that output messages to the CTY constantly when
at detached job, GFD'd to [1,1] could not run SYS:LOGOUT.  When the patch was
removed, I put in the CHGPPN to [1,1] check.

1) No one is allowed to LOGIN to [1,1].
2) No one is allowed to GFD to [1,1].  If you need to look at files in [1,1],
   put the PPN in the LOOKUP block.  Relying on the default is wrong.
3) No one is allowed to create files in [1,1] without the extension of UFD.

The latter point may be debated, but if you have WF and JL you don't need to
use GFD, just reference the PPN [1,1] explicitly.

What was it that you needed to do what required GFDing to UFD?
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Sun, 2 Mar 86 14:59:02 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Thanks! 

      Thanks, to you and Cherie for being with us.  Your being among our
official Quaker witnesses have made you two part of our marriage, and your
celebration with us and our families and friends added to our joy and
celebration.  See you in a week!


      Your friends,
      Dennis and Nancy


P.S.  Nancy says "Thank you" cards will follow the honeymoon.
From: FLETCHERC@X930.Tymnet 
Date: Mon, 3 Mar 86 11:29:50 PST 
To: carl 
Subject: standby 

Lois still needs a hardcopy of standby time for week of 17-23 fEB.

	cRAIG
From: FLETCHERC@X930.Tymnet 
Date: Mon, 3 Mar 86 11:38:26 PST 
To: carl, osman 
Subject: TYMCOM-X meeting 

I think we'll skip the meeting this week sinch it will be only the
three of us.  Same time, same station on March 11th.

	Craig
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Mar 86 11:52:30 PST 
To: jms 
Subject: GFD to [1, 1] 

Simple...
  I didn't have TSTSUP or (MPL) or a few other directories and I was
trying to both look at and delete files/UFDs from [1,1].  Since I dodn't
have the tools available, IF I could have GFD'd there I could have done
a DELETE *.*/WAIT and said Y or N to each deletion.  Instead, I had to
rely on a copy of DMS which didn't do much better... sigh.

  Anyway, I agree... GFD should prevent anyone from going to [1,1]!!!
But I think that R DDT; 0[1,,1  chgppn$x  should still work to get me
there if I hav sufficient license.

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Mar 86 11:56:50 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Meeting 

Ok, no meeting tomorrow...
  In lieu of meeting, either Osman or I shall try to give you an update
on our progress to date.

  I believe the test worked.  Osman built a 12-pack system with 24XP19
and successfully ran with the KL.  One glitch was that the KL monitor did
not have Joe's "ignore-blocks-packs" code and it at first tried to come
up using the 1-pack blocks pack until that was powered down.  REFSTR
appears to work as expected.  One drive powered itself off and would not
spin up and that was where we were last night.  Osman will have to fill
in the rest of the info.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Mar 86 13:10:46 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: X930 outage 

System 930 will be unavailable tomorrow, 4-Mar-86 so that it can be moved
to its new location in the old KL-26 lab.  Steve Capik will be coming in
shortly after 8:00 am to begin the move.

/Carl
Received: From EMSTXS.Ontyme.Tymnet by C930.Tymnet; Tue, 4 Mar 86 23:04:31 UT
From: IOD.CUPERTINO@Ontyme.Tymnet 
Date: 04 MAR 86 13:45:19 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	SLI.SUP@Ontyme.Tymnet, IOD.CUPERTINO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A32736@Ontyme.Tymnet 
Subject: Batch requests for Sligos 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-03-04  13:27 PST                  Telex 4993666 MDISI CPTO
 
TO>        Dennis Coffey (TXS.D/COFFEY)
 
COPIES>    Carl Baltrunas (TXS.C/BALTRUNAS)
           Henri Tocheport (SLI.SUP)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Batch requests for Sligos
 
REFERENCE> Attached Ontyme #A31491
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Dennis, I phoned you yesterday but have not received a call back,
     therefore thought you might not be in office this week; hence I'll
     cover via Ontyme.
 
     If the Tymcom X Batch program is not in your area of responsibility,
     will you please forward this message to the appropriate person.
 
     Note message which follows on next page from Sligos in France.  Bas-
     ically, they are asking if we can remove the present arrangement
     whereby all Batch Requests from District 38 are sent to user name
     SLIBATCH, for translation, rather than the standard Batch environ-
     ment.  I have no idea how easy (or difficult) this would be to fix.
 
     I look forward to your comments.  Regards. Ken
 
     P.S. Carl and Dennis, as I have mentioned before, I use a Tymcom 370
          which links to the Ontyme system.  This allows a standard list of
          recipients for distribution of ontyme messages based on the ***
          DIRECT files located on Ontyme hosts.  When I use this automated
          system the default account for both of you is FSC rather than TXS;
          This is apparantly because the TXS account does not have a ***
          DIRECT file, but FSC does.  I'd suggest you look into this unless
          you consider the FSC account as your default and you regularly log
          into these names (which I do not believe to be the case).
 
                                        v
                                        |
                                        |
                                        |
                                        |
                                        V

MSG#:A31491
 IN#:  6242
  TO: CUPERTINO
FROM: SLI      SUP
SENT: 03 MAR 86 09:44:16
READ: 03 MAR 86 09:56:58
  CC: CUPERTINO  SLI.SUP
 
 
 
           SLIGOS                       PUTEAUX  (FRANCE)
 
DATE>      03 MAR 86  18:42             Tel. [-33] (1) 47 76 42 42
 
TO>        Ken BeDell (IOD.CUPERTINO)
 
COPIES>
 
FROM>      Henri Tocheport (SLI.SUP)
 
 
SUBJECT>   PPR request and SLIBATCH
 
 
-----------------------------------------------------------------------
 
 
 
Ken,
 
We have a problem of batch request on system 25. As you know any
PPR issued by a district 38 username must be cleared and re issued
by username SLIBATCH. Now SLIBATCH is not valid on :25. If this
name is added on :25 it must be logged on by time to time and
clear PPR's. Sligos' people using private (or public) network systems
in the USA have few batch requests to submit, they can write in
english. So I wonder if the best way is not to give district 38
usernames a direct access to BATCH program and suppress SLIBATCH
clearing. Do you think this is possible ?
 
Thank you for your help
Best regards, Henri
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 6 Mar 86 4:39:39 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, jms, dencoff, fletcherc 
Subject: 3650 Project & This weekend.. 

1. Well, today(3/6) I finally go to "Jury Duty"

2. 3650 Project for C26 is "GO" for this weekend.
   I build a 12-pack system over the weekend but,
   2 hrs after finished rebuilding the system lost
   one of the disk therefore lost the 12-pack system.

3. I am on the process of building an other 10-pack
   system, to check out disk, monitor and all the other
   utilities, pakcop, asp, tito etc..

4. I have not acounter any problems so far with disk,
   like disk errors or any utilities not doing what 
   they suppose too.

5. I will continue to rebuild and checking. I will
   do asp dump after restoring the all-files.

6  For this weekend I will assist ops for the move
   and be available for help.

-Osman-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 6 Mar 86 8:38:06 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	CC:Ring, WRS 
Subject: California license plate 

Seen on a Datsun B-210, a normal everyday California license plate: 930LUD.
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 6 Mar 86 9:05:53 PST 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Cc: Carl A Baltrunas <carl@X930.Tymnet>, Joe Smith <jms@X930.Tymnet>, Craig
	Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	CC:Ring, WRS 
Subject: Re: California license plate 
In-reply-to: your message of Thu, 6 Mar 86 8:38:06 PST

He must have been Ernie Soccie's car..
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 6 Mar 86 9:07:05 PST 
To: jms, Carl A Baltrunas <carl@X930.Tymnet>, fletcherc, dencoff 
Subject: FULL DSKCLN on C24??? 

Full DSKCLN is taking 1 hr and 20 mins to run.
F38 which is 26 pack system takes only 55 mins. Hmmm????
Any ideas why??
-Osman-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 6 Mar 86 9:19:49 PST 
To: Osman Guven <osman@X930.Tymnet> 
Cc: Carl A Baltrunas <carl@X930.Tymnet>, fletcherc, dencoff 
Subject: Re: FULL DSKCLN on C24??? 
In-reply-to: your message of Thu, 6 Mar 86 9:07:05 PST

I think it is because F38 has two 3672 control units and four 3673 string
controllers.  This means that 4 disks can be seeking to a new cylinder at
the same time, and 2 disks can be searching for ID= at the same time.
C24 has only 1 control unit and 2 string controllers.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 6 Mar 86 18:14:35 UT
From: IOD.BEDELL@Ontyme.Tymnet 
Date: 05 MAR 86 15:41:40 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: CT.DPM@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.D/COFFEY@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet,
	CT.TECHSERV@Ontyme.Tymnet, ISIPR.CUPERTINO@Ontyme.Tymnet,
	IOD.CUPERTINO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A33955@Ontyme.Tymnet 
Subject: Tymcom X Batch program and re-routed PPR's for 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-03-05  15:24 PST                  Telex 4993666 MDISI CPTO
 
TO>        Phil Costes (CT.DPM)
 
COPIES>    Carl Baltrunas (TXS.C/BALTRUNAS)
           Dennis Coffey (TXS.D/COFFEY)
           Jean-Fran,cois Guillou (CT.DPM)
           Eric Jay (CT.TECHSERV)          Peter Haas (ISIPR.CUPERTINO)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Tymcom X Batch program and re-routed PPR's for
           translation purposes
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Phil, Henri Tocheport of Sligos has recently requested that we elimi-
     nate the old Batch arrangement whereby Batch requests submitted by
     users in branch 38 are dumped into the SLIBATCH directory.  It seems
     they no longer have a need for the "translation" of such requests and
     prefer to have them placed in the standard BATCH (OPS) directory.
 
     I have spoken with Carl Baltrunas about this issue.  What we would
     like to do is eliminate both the Sligos and Cegi Tymshare special
     arrangement whereby Batch requests are dumped to either the SLIBATCH
     or FBATCH directory, depending on the branch number of the requestor.
 
     So, the question is: Would you foresee any problem in eliminating
     this special lookup arrangement within the Tymcom X Batch system?  I
     suspect most, if not all, of your billable clients are on host S90,
     therefore translation is likely not required; basically, no need for
     two requests when one will do, directly to BATCH directory.
 
     Please let us know if you have any objection to eliminating the code
     within the Batch program which performs the above process.  It would
     likely make the Batch mechanism more efficient by eliminating the
     branch lookup code and could allow for easier enhancement to the code,
     if required in the future.
 
     We look forward to your earliest response.  Regards. Ken
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 7 Mar 86 2:39:39 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>, Lois Frost
	<FSC.L/Frost@Ontyme.tymnet> 
Subject: schedule 

I will be in a little later than usual today...  we are going to
try to have cable installed again sometime this morning.

If you need me, call me at home... 408-945-4314.  Thanks all.

See you...
/Carl
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 7 Mar 86 14:04:25 UT
From: CT.DPM@Ontyme.Tymnet 
Date: 07 MAR 86 02:32:50 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A35181@Ontyme.Tymnet 
Subject: "M E M O R A N D U M MCDONNELL"... 

                          M E M O R A N D U M





                   MCDONNELL DOUGLAS INFORMATION SYSTEMS
Saint-Cloud - France                               Amsterdam - The Netherlands
Tel. [33] (1) 46.02.70.12                          Tel. [31] (20) 24.93.31



DATE>      07 MAR 86  11:29

TO>        KEN BEDELL (IOD.CUPERTINO)

COPIES>    C. BALTRUNAS (TXS.C/BALTRUNAS)
           D. COFFEY (TXS.D/COFFEY)
           J.F. GUIILOU (CT.DPM)
           E. JAY (CT.TECHSERV)
           P. HAAS (ISIPR.CUPERTINO)

FROM>      PHIL COSTES (CT.DPM)


SUBJECT>   TYMCOM X BATCH PROGRAM S90


-----------------------------------------------------------------------


KEN,

I JUST RECEIVED YOUR MEMO ABOUT TYMCOM X BATCH PROGRAM.
I THINK YOU CAN ELIMINATE THE CODE WITHIN THE BATCH PROGRAM WHICH PERFORMS
THE "SPECIAL ROUTE" BETWEEN SLIBATCH AND FBATCH DIRECTORY.
ACTUALLY IT WILL BE BETTER ALL BATCHES ARE STORED  AND MANAGED FROM
FBATCH DIRECTORY. IN FACT SINCE MAY SLIGOS SUBMITTED A FEW BATCHES WENT
DIRECTLY TO SLIOPER DIRECTORY. IL WILL BE EASIER FOR THE OPERATORS
TO HANDLE TYMCOM X S90 FROM ONLY ONE DIRECTORY.

THANK YOU AND BEST REGARDS.  PHIL
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 7 Mar 86 17:59:07 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Metamorphosis: 118 => 62 

System 118 is dormant again!  System 62 is up on the network after a few
attempts to reload node 10115.  I've put it on physical port 4 on node
3143 and used the jumpers from port 3 (node 2770).  To see the new net
configuration, print the file (CARL)TXSNET.PIC.

/Carl
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Sat, 8 Mar 86 2:05:47 UT
From: IOD.BEDELL@Ontyme.Tymnet 
Date: 07 MAR 86 15:45:44 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: CT.DPM@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	CT.TECHSERV@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	CT.DPM@Ontyme.Tymnet, ISIPR.CUPERTINO@Ontyme.Tymnet,
	IOD.CUPERTINO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A35853@Ontyme.Tymnet 
Subject: Tymcom X Batch program 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-03-07  15:40 PST                  Telex 4993666 MDISI CPTO
 
TO>        Phil Costes (CT.DPM)
 
COPIES>    Carl Baltrunas (TXS.C/BALTRUNAS)  Eric Ja"y (CT.TECHSERV)
           Dennis Coffey (TXS.D/COFFEY)
           Jean-Fran,cois Guillou (CT.DPM)
           Peter Haas (ISIPR.CUPERTINO)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Tymcom X Batch program
 
REFERENCE> Your ontyme #A35181
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Phil, thanks for the memo regarding Batch program on the X's.
 
     Your memo stated:  "Actually it will be better all batches are stored
     and managed from FBATCH directory."
 
     I'm a little confused by this because the 'standard' procedure does
     not use the FBATCH directory to my knowledge; in fact, I'm not cer-
     tain of the directory used via the standard Batch program.
 
     In any event, I am requesting Dennis Coffey and Carl Baltrunas, via
     copy of this memo, to proceed with the change which will eliminate
     the transfer of Batch requests to either the SLIBATCH or FBATCH
     directory (as based on district number of the user).
 
     If this is contrary to your understanding, please let me know imme-
     diately.
 
     Best regards. Ken
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 7 Mar 86 20:36:44 PST 
To: Joe Smith <jms@X930.Tymnet>, William R Soley <wrs@C39.Tymnet>, Ken Dawson
	<Peak@X930.Tymnet> 
Subject: Would you believe... 

TELECOpy lets you specify weird names like FOO.@$# if you're pulling or
pushing files, but AUXMOX seems to have a more limited view of the world
and will not let you do anything of the sort.  Sigh!
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 7 Mar 86 21:28:44 PST 
To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, wrs, Ken Dawson <Peak@X930.Tymnet> 
Subject: TELECOPY thinks 62 is a 370 

I've "patched" the table in TELECO.SAV which knows what type of hosts
each system is.  (Dennis, I want to change this, so come and see me
when you come back to work).  This patch is on SYS on systems 930, 62,
36, 35 and 39.  With the patch, TELECO should now work OK since it
now thinks it's a TYMCOM-X system.  I may patch it on all systems if
I get the chance... will let you all know.  If anyone needs it before
then... send me mail & I'll patch it there.  I believe that we're about
to release an updated TELECO.SAV to add 62...  sigh.
/Carl

PS: IF it's version 22.3, absolute locations in the .SAV file mentioned
    here, 3115/ 3,,27005 (system 60) 3116/ 4,,27014 (system 61) and
    3117/ 4,,27014 and can be changed to 3,,27005 (system 62)... sigh!
From: Osman Guven <osman@X930.Tymnet> 
Date: Sat, 8 Mar 86 17:27:46 PST 
To: dencoff, jms, Carl A Baltrunas <carl@X930.Tymnet>, fletcherc 
Subject: C26 up with 3650's... 

Well, C26 is up and running with the "3650 DISKS"
as 10-Pack system and still have 158000 pages free.
So far, I have not discovered anything unusual except:
1) DSKCLN taking longer, 1 hr and 14 mins on KL.
2) After ASP dump, picked up 2 or 3 HDEV errors on
   most of the disks. Disks were running error  free
   during ALL-FILES restore.
3) SA10NW diagnostic tape which works when loaded on KI
   doesn't work on KL. Something we need to look into
   soon so that MAINT people have tools to work with.
   I have an idea what the problem might be, will look
   into it later.

-Osman-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 9 Mar 86 1:17:06 PST 
To: Osman Guven <osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 
Subject: C26 went down at 1:18 

I was in the process of sending RPG mail to OSMAN saying how I was exercizing
the disks with 3 PCOM files when it went down with a FILAUS stopcode.  LT Smith
says that one of the  disks was making a loud humming noise just before it
went down (one near the middle of the string).
Received: from C26.Tymnet by X930.Tymnet; Sun, 9 Mar 86 8:03:54 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 9 Mar 86 8:03:01 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: C26 has no DSKB1 ! 

I can use (MPL)FILDDT to look at DSKB0: and DSKB2:-DSKB11:, but not DSKB1:!
If I use ASS DSKB1 STR, then TSTSUP reads DSKB11 instead of DSKB1.
SY also implies that BPA2=DSKB1 and BPA2=DSKB11.  What is going on?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Mar 86 12:23:10 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Schedule 

I will be working at home today... it appears that my wife has gone to
work with my keys and my extra car key in her purse... leaving me stranded.
If you need me for anything... cal me at home, 408-945-4314. Thanks all.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Mar 86 12:27:51 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: STRUUO fn .FSETS/.FSRRL 

Joe,
  In addition to modifying SYSTAT's table, there are two functions in the
STRUUO .FSETS to simulate hard errors and .FSRRL to read/reset the error
log in the 3330's.  Should these be made to work (are the features available)
in the 3650's?
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 10 Mar 86 12:32:30 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: STRUUO fn .FSETS/.FSRRL 
In-reply-to: your message of Mon, 10 Mar 86 12:27:51 PST

Command code A4 is READ AND RESET BUFFERED LOG.  When the disk controller
sees this command, it returns the 24 sense bytes corresponding to whatever
disk unit got an error.  This function works for any disk controller that
talks to the SA-10 (ie, it is not specific to the 3650).  I'm not sure what
.FSETS is supposed to do, but .FSRRL works as is.
From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Date: Mon, 10 Mar 86 12:47:16 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: HOTLINE Scheduel 

Craig,
  I am on-call beginning today, 10-Mar-85 until sunday 16-Mar-85.  I will
be switching with someone for part of the weekend, since I will be out of
town in Arizona on Saturday and Sunday.  More later...
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Mar 86 13:41:06 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: 26: BPA1/BPA10 == DSKB5 

Joe,
  The reason that this happens is that the BPA1 is a left subset of
BPA10 and since there is no BPA1 at the moment (powered off) it picks
the first unit that matches... i.e. BPA10 and returns DSKB5.  As for
DSKB1 (BPA15) and DSKB11 (BPA2) both returning BPA2, again this is
doing the same thing... it's searching the physical tables for a left
subset and stops when it finds a match.  *** THIS IS A BUG ***  It
ought to check for exact matches first, then if it cannot find one
return the left subset.

  I manually checked each unit, and got the correct results in BPA
order.  SY checks using logical device names.  If Osman had not reversed
the order of the logical units and the physical units, this would never
have shown up!  Osman did a good thing!!!  It means we need to fix it
at some point.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Mar 86 13:51:25 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: what happened to 930? 45 mins ago? 


From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 10 Mar 86 13:58:34 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Subject: C26 BPAx...DSKBx 

I don't understand then, why SY on F38 reports it incorrectlly also??
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 10 Mar 86 14:04:00 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, jms 
Subject: Rest of the day.. 

I am going to AUBRAE and then home, see you later.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 10 Mar 86 14:34:38 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: what happened to 930? 45 mins ago? 
In-reply-to: your message of Mon, 10 Mar 86 13:51:25 PST

2770 lost contact with the network, then 6657 got a NXM.  Reloaded 930 
and ran NODLOD on 2770.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Mar 86 14:35:16 PST 
To: Osman Guven <osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Subject: System 38 DSKB1 mismatch 

Osman,
  Just as I said... the code looks for a left subset.. thus, in looking
for DSKB1 in the list: BPA0=DSKB0, BPA1=DSKB14, BPA2=DSKB2, ..

  DSKB1 matches the first 5 characters of DSKB14, so it returns BPA1.
This is done by the DSKCHR UUO in trying to match BP, BPA, ..etc.  The
ONLY solution is to check for exact matches first, then look for any
partial matches.   *Sigh*

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 10 Mar 86 14:44:30 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Osman Guven <osman@X930.Tymnet> 
Subject: Re: System 38 DSKB1 mismatch 
In-reply-to: your message of Mon, 10 Mar 86 14:35:16 PST

Gee, TOPS-10 never had that problem.  But then TOPS-10 does not allow a 9-pack
DSKB, it has DSKB0 to DSKB7 only.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Mar 86 15:28:30 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: (M33)MONBUG.NFO 

As an aid to remembering little things that need work, but otherwise work
I've created an INFO node MONBUG with subnodes for each bug that needs
something done for it.  DSKCHR is the only entry at the moment... but it's
there... please add things to it for things we need to do but haven't time
to do now... so we won't forget them!
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Mar 86 15:42:38 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: BOOTS 

What's happenning... Re: 930/62 BOOTF3 ?
Something to be put on tape instead of a monitor... I tried BOOT?.F3
from SYS: and also your BOOTF3 from (m33) and neither worked to get
me where I wanted to be.  I ended up loading /N10 from tape then run
TAKEIT to reload the other day.
/Carl
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 11 Mar 86 2:03:51 UT
From: IOD.BEDELL@Ontyme.Tymnet 
Date: 10 MAR 86 16:58:19 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: CT.DPM@Ontyme.Tymnet, SLI.SUP@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	CT.TECHSERV@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	CT.DPM@Ontyme.Tymnet, ISIPR.CUPERTINO@Ontyme.Tymnet,
	IOD.CUPERTINO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A37125@Ontyme.Tymnet 
Subject: Change to Batch program 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-03-10  16:56 PST                  Telex 4993666 MDISI CPTO
 
TO>        Phil Costes (CT.DPM)
           Henri Tocheport (SLI.SUP)
 
COPIES>    Carl Baltrunas (TXS.C/BALTRUNAS)  Eric Ja"y (CT.TECHSERV)
           Dennis Coffey (TXS.D/COFFEY)
           Jean-Fran,cois Guillou (CT.DPM)
           Peter Haas (ISIPR.CUPERTINO)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Change to Batch program
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Henri and Phil, reference recent dialogue related to the Tymcom X
     Batch program, initiated by Henri via his message #A31491, dated
     March 3rd.
 
     Action will be taken by the Tymxom X support group to alter the Batch
     code so that transfers to FBATCH and SLIBATCH will no longer take
     place.
 
     Dennis Coffey will be in touch with us soon, indicating a time table
     for this change.  Best regards. Ken
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 10 Mar 86 19:47:27 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet> 
Subject: Re: System 38 DSKB1 mismatch 
In-reply-to: your message of Mon, 10 Mar 86 14:35:16 PST

Thank you both Carl & Joe, I understand it now.

From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 11 Mar 86 8:03:55 PST 
To: jms, Carl A Baltrunas <carl@X930.Tymnet>, fletcherc, dencoff 
Subject: Today.. 

I am taking a "comp" day. will be in tomorrorw.
-Osman-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 11 Mar 86 9:29:45 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, DENCOFF 
Subject: Today.. 

I won't be in until after lunch.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 12 Mar 86 11:09:30 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: CRASH.SAV on C26 was 0 pages long. 

I thought you fixed the table that ONCE uses to determine the size of CRASH.SAV.
It created CRASH.SAV that was 0 words long.  Osman and I went thru your CRAFIX
proceedure to make it 262144 words long.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 12 Mar 86 12:51:08 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, dencoff 
Subject: TELECOPY: 'UNRECOGNIZED ERROR FROM SUPERVISOR - NUMBER 7'. 

TELECOPY needs to be modified so that it understands SUP error 7.  It should
say "ACCESS BARRED - GROUP FOR USERNAME DOES NOT INCLUDE DESTINATION HOST".

The actual cause of the problem is that host 62 is not in GROUP 1.
Received: from C39.Tymnet by X930.Tymnet; Wed, 12 Mar 86 22:02:54 PST
Received: from X62.Tymnet by C39.Tymnet; Wed, 12 Mar 86 22:02:37 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Wed, 12 Mar 86 21:58:08 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: TELECO & HOST 62. 

      I have discovered one portion of the TELECO interface to the rest of the
world that works O.K.:  I have sent a file, pushing from 62 to 36.  that's all
I know works.  I have not been able to send the other direction, either
pushing or pulling.

	    Dennis
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 13 Mar 86 10:16:50 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, jms 
Subject: DSKCLN on X930.. 

Looks like we have not run FULL/FAST DSKCLN on X930
after P15 monitor. This morning when micro node 6657
went down I tried to recycle the system and it wanted
to run FAST but failed(see out put on consule). I tried
running FULL same failure. Wanted to see what was causing
it soo brought the system up with N08 and DSKCLN start
to run OK. Took system down and brought it up with
P20 without DSKCLN. Hmmmm???
-Osman-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 13 Mar 86 18:14:41 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	CC:Ring 
Subject: 10115 (X62-base) reloaded. 

After the last time X62 came up, it was beeping waiting for the time.  The
ADM3 connected to node 10115 got no response.  So I logged into system 39,
created (MPL)RELOAD.115, and ran it.

How are we going to reload the PDP-11 microTYMbases once C39 is on BUBBNET?
Is it simply a matter of putting a Gateway command and password in RELOAD.* ?
What password will LOADII need to build a circuit to 3163 for reloading?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Mar 86 18:59:24 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Reloading from 39? 

Unknown as to how that will work.... I've obsconded with a copy of SMITII
and it is in (SMITHDE) which seems to be the only place that it will run
from without logging you out.  So, if 930 & 6657 is up, we can load from
there, and after copying it to 62, we should be able to go the other way
with some ease.  TELECO works fine through the gateway, and LOADII will
go through a gateway, but I do not know the procedure.

System 57 is a backup of some fo the code on 39, but (CURR11) and (NEW11)
are not backed up anywhere as far as I know.  maybe we should also find
the reason whay we cannot load 6657 from 930... I believe it has somethng
to do with CIOPR... sigh.  History bites us again!

/Carl
Received: from C36.Tymnet by X930.Tymnet; Fri, 14 Mar 86 16:58:51 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 14 Mar 86 16:51:11 PST 
To: Carl Baltrunas <CARL@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Subject: Network Access: This Building. 

Below are the messages I sent to craig and Pat Diamond about the need for
change to our network access.
----------------------------------------------------------------
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 14 Mar 86 12:59:50 PST 
To: Craig Fletcher <FLETCHERC@X930.Tymnet> 
Cc: FSC.P/DIAMOND@Ontyme 
Subject: Network Access: This Building. 



                                  MEMORANDUM


                                                              TYMCOM-X SYSTEMS

DATE:  14-Mar-86  15:33:16

  TO:  Craig Fletcher

  CC:  Pat Diamond

FROM:  Dennis Coffey  (Tymcom-X Systems)

SUBJ:  Field Service Company's Network Access: This Building. 

------------------------------------------------------------------------------

      I've received a phone call from Tymnet:  FSC's access to Tymnet in this
building, both for terminals and for our hosts, is not 'legal'.  I.e., Payment
Systems Co.  pays for the engines in this building and for the synchronous
network line to connect to the public network.  Pmnt. Sys. Co.  is billed by
Tymnet for this service.  We do not pay for it at all.  I am told, by Tymnet,
that our traffic "degrades Pmnt. Sys. Co.'s service", and that our use of
service purchased by another company is not allowed.

      I have discussed this with Pat Diamond, and he has agreed to handle the
solution of this problem.  He and I discussed some alternatives, and the
suggestion which he said he will persue most strongly includes:  the
installation on site here (as a 'lab' machine--self installed) of a Tymnet
Engine that FSC owns; and upgrading the line(s) (from here to Solar Way) which
Pmnt. Sys. Co.  'owns', from 9.6 Kbps to 14.4 Kbps.

      Our lab machine would be the pass-through connection for our micro-
Tym-Bases and would provide any terminal connections we might need to take off
Pmnt. Sys. Co.'s engines.  The only traffic that Pmnt. Sys. Co.'s engine
will have may be pass-through traffic.  We could share costs for the lines
with Pmnt. Sys. Co., connecting one line to their machine and the other to
our machine.  Pat tells me that this would probably provide service MUCH more
quickly than ordering new phone service from Pac. Tel., and--if Sal Spinelli
of Pmnt. Sys. Co. agrees--be less expensive.

      Pat tells me that he will contact Sal next week:  Sal is in Irvine this
week, but next week will be available at Tymnet, Orchard Parkway.  He will be
in meetings there next week about their network access problems, but be able
to be reached by the Tymnet operator.

      Dennis
----------------------------------------------------------------
----------------------------------------------------------------
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 14 Mar 86 15:57:13 PST 
To: Craig Fletcher <FLETCHERC@X930.Tymnet> 
Cc: FSC.P/DIAMOND@Ontyme 
Subject: Network access change: Liberty. 

      Discussion of this pending change with Joe Smith has shown this to be an
opportunity:  if we get the lab node, we could have an ISIS machine which
would allow an Async Host Interface TYMCOM slot, as well as a Async Terminal
Interface CONSAT slot.  The Host Interface slot could allow us to bring the
the 2020 (host 95) into the network quickly, plus we could put the consoles
for the hosts on them -- allowing us to use the consoles remotely (through
network sub-addressing).

      Another alternative could be connecting to the Albrae St.  location.
This could provide redundancy, if this were in addition to the lines discussed
previously.  If we order any lines, I believe ordering directly from Pac.
Tel. will will be significantly less expensive than ordering from Tymnet.
Tymnet's monthly charge for service between here and Solar Way:  Tymnet's
monthly charge would be over $700 -- the advantage they offer is Tymnet
Network Control's reporting of and follow up on the line outage.

      Pat suggested another cost-saving alternative for redundancy from our
engine to the network:  a dial-up Network sync line, rather than a 4-wire
dedicated line.
----------------------------------------------------------------
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 14 Mar 86 21:00:35 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Bug in P20 was in MAPIO at SPRBNS 

The flow gets to SPRBNS via a JUMPE U, so I put a MOVEI U,0 there.  P21 works.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 17 Mar 86 0:34:33 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: /P21 Good detective work. 


Like proof-reading, checking and re-checking code that I added, I know
what it's supposed to do and miss the obvious.  I'd looked at that, knowing
that the test for U=0 was used to get to the right place, but hadn't gone
far enough to trace everyone that calls it.. what do they expect back!

Anyway, that means we can go on to build on what has been done to date
rather than butting heads against the wall to figure out why it isn't
working.   Good show!  and thanks!

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 17 Mar 86 0:38:46 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: I'mback! 


From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 17 Mar 86 0:39:29 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: By the way 

It took 7 monitors (trial and error, adding 2 modules at a time) to zero in
on MAPIO.  I take it that was what you were going to do anyway.

Did you know that the circuit gets zapped if you try to run MUDDY?

I dod not record Amazing Stories tonight - it was one that you already have -
"Fine Tuning".  No Blakes-Seven due to pledges.  No ice scating this weekend.
I did put a Hitchcock on the Stories tape (The schedule advertized 2 Hitchcocks,
but NBC ran Amazing Stories at 10:30 instead).  I put little bits and pieces
of Tom Baker in "The Curse of King Tut's Tomb" on your Who tape.

I'm leaving for home now, ask Sally about the expensive flyers for WOTS.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 17 Mar 86 0:50:14 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Module by module 

Yes, I was going to do same... thanks again.  I'm a bit tires, so I'm
going to hit the sack soon.  Will let Cherie see expensive fliers when
we come over... Cherie's a little out of it, probably just tired too,
so I'll have to let you know if we're going to make it over Monday night.

Gnight!
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 17 Mar 86 14:06:00 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	CC:Ring 
Subject: Using the GATEWAY command in TELECOPY. 

To TELECOPY files to or from system 39, you have to start on a PDP-10 on the
public net, build a gateway to BUBBNET, then copy the files.  The host numbers
of the gateways are: 5946, 1461, 1579, 5516, 5981, 1552, 2866, 3171, and 7255.
For example:

	.R TELECOPY
	: GATEWAY 7255;JMS:39
	Password <password for JMS on BUBBNET>
	: (*6NEWS:39)TELECO.DOC,(JMS:930)		;Pull a file from 39
	: (JMS:930)SWITCH.INI,:39			;Push a file to 39
	: QUIT

The gateway host numbers are listed in (*6NEWS)BUBNET.DOC on most PDP-10 systems.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 17 Mar 86 23:40:22 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	CC:Ring 
Subject: 3650 disk project. 

Remaining items in the 3650 project:                    17-Mar-86

1) Verify that APT10 works.  It is designed to transfer files between the
   TYMCOM-X system and KLDCP DECtapes via the CTY.  Use it to put SA10NW on
   a DECtape.   This requires a standalone KL, brought up in CONSOLE mode.

2) Determine why the paper-tape version of BOTLOD does not work like the version
   in MONBTS.  This requires a standalone KI with 3650 disks for complete veri-
   fication, although most of the work could be done on C24 with Ampex disks.

3) After the KI version of BOTLOD works, put the KL version on the DECtapes
   using APT10.  This can be done during a scheduled downtime.

4) Determine where PAKCP5 fails when writing the secondary HOM and BAT pages.
   This is the only thing keeping the COPY command from working.  This requires
   standalone time on the CPU with 3650 disks, not more than one weekend.

5) Make a single version of PAKCOPY that understands 3675 in blocks and pages.
   This means that the size of the areas used for channel command lists need to
   be determined at run time, not as an assembly option.  This can be done on
   C24 with Ampex disks.

6) Locate the routines in the monitor that assume that all units of a structure
   are the same size.  Determine whether it is feasible to have a DSKB composed
   of a mixture of packs formatted in blocks and in pages.

7) Modify BOOTS to understand all 4 disk types.  Right now, "BOOTS" understands
   3675 blocks only, and "BOOTX" understands 3650 pages only.  This requires
   standalone time on C24 with Ampex disks, and can be verified on C26
   during a scheduled reload of the system.

8) Add a command to BOOTS telling it which set of packs to look at.  This is so
   that a one-pack test system can be kept spinning and not confuse the Monitor
   by having two packs claiming to be "DSKB0".  This will have a side effect of
   making BOOTS much faster in loading the monitor, and can be done on 930.

9) Install 2nd upgraded SA10 for continued testing on C24.

10) Add code to PAKCP5 that can rewrite HA/R0 on 3650.  I have disassembled the
   relevant portions of SA10NW.SAV, and will put them in PAKCOP.MAC.  Testing
   requires days of standalone time on C24 with new SA10 and 3650 disks.

11) Merge PAKCP5 with the pages version of 3675 PAKCOP.

12) Determine whether bad blocks really work.  Verify that PAKCOPY assigns
    alternate tracks correctly.  Make it look at the BAT block that SA10NW
    creates.  Make the monitor add new entries when it discovers bad pages.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 18 Mar 86 0:13:14 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Re: PAKCOP 3675 blocks/pages 

It might be a nice idea to "read-only" blocks and "write-only" pages with
a version of PAKCOP... Yes, I realize we will have extra pages after just
such a transfer, and that SAT.SYS would need to be fixed up, but it might
well be worth doing the formatting to pages and using only the same size
in blocks until the next REFRESH. That way, we save having to do a reformat
and a full-rebuild at the same time.
/Carl
From: FLETCHERC@X930.Tymnet 
Date: Tue, 18 Mar 86 16:40:15 PST 
To: jms, osman, carl, dencoff 
Subject: remaining task for 3650 project 


At our TYMCOM-X meeting of 18Mar86 we discussed all the tasks listed
in Joe's mail and reordered them in a slightly different priority.

Priority 1 is to complete all items required by page mode first.
a) Fix Botlod: tasks 1,2,3 from Joe's list
b) Determine why PAKCP5 fails (4)
c) PUt SA10NW on DECtape for KL's
d) Add code to PAKCP5 to rewrite HA/R0 (10)

Priority 2 is to extend software to handle all pages or all blocks by
runtime decision.

a) Make version of PAKCOP that understands blocks and pages(5)
b) Modify BOOTS to understand all 4 disk types (7)
c) Merge PAKCP5 with pages version of 3675 PAKCOP(11)

Priority 3 - items we would like
a) Boots command to choose pack set (8)
b) See if feasible to have structure of mixed pages,blocks (6)

Add to group 1
a)use sector 0 for track replacement in monitor (12)

From: FLETCHERC@X930.Tymnet 
Date: Thu, 20 Mar 86 9:39:52 PST 
To: jms, carl, osman, bring, helgem, dencoff 
Subject: TYMCOM-X meeting 


TYMCOM-X meeting will be slipped from Tuesday, March 25 to
Wednesday, March 26 at the same time: 10a.m.

	Craig
From: FLETCHERC@X930.Tymnet 
Date: Thu, 20 Mar 86 10:38:24 PST 
To: carl 
Subject: task list 


Please bring a list of tasks to the TYMcom-x meeting (similar to the
3650 list) that covers in priority order what needs to be done to
replace PERP on the -10's.

	Craig
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 22 Mar 86 13:14:52 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	CC:Ring, wrs, helgem, scapik, peak 
Subject: It's a fuzzy spot of light. 

                        .


                                              .
              .                    .
     .



                 Sagittarius
 .



       .                                .




  /
 /
*

South on 101 for 60 miles to San Juan Bautista, then 11 miles (another hour) to
Fremont Peak State Park.  We arrived at 4:07am; the moon was still up.  But you
could see Halley's with the naked eye.  At 4:30 the moon went down and the view
got better.  The tail of the comet took up half the field of view in 7x35
binoculars ("It looks like all the pictures of comets!").  It started to fade
at 5:00am when the sky got light.  ("We saw Halley's comet!")

Through a telescope, the field of view is too small to see the entire tail all
at the same time.  One guy had an 8-inch by 7 foot reflector - it showed the
nucleus as a distinct disk in the middle of a fuzzy spot.  And Saturn's rings
are quit spectacular even in the smaller telescopes.

The moon sets 1/29th of a day later each time, so it will still be in the sky
at 4:45 Sunday morning; that's the last time Halley's will be visible for
about 2 weeks.

4:07am                  .


                                              .
              .                    .
     .



                 Sagittarius
 .



       .                                .




  /
 /
*

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 25 Mar 86 3:15:46 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet,
	Bruce 
Subject: Hotline Call. 

24-Mar-85 23:41/JMS
Contact:  Cupertino BLDG C, OPS, Karen, (408)446-7705, Systems 22,23,29,35,36,70

Symptom:  DSKCLN interlock failed when system reloaded.

Diagnosis: INITIA version 22 TELECOPYd to SYS, did not have enough license.

Cure:     Brought up C35 MANUAL, put OP,SY,ST,JL on INITIA.SAV, told Karen and
          Osman to do the same to the others when they are ready.

Note:     The placement of version 22 of INITIA.SAV on SYS was delayed until
          I was on-call, so that I would be notified if any problems occured.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 25 Mar 86 3:54:49 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Craig
	Fletcher <fletcherc@X930.Tymnet> 
Subject: I'll be in late Tuesday. 

As you can see by the time of this message, I won't be in to work until
the afternoon.			/JMS
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 25 Mar 86 5:42:55 PST 
To: dencoff, jms, Carl A Baltrunas <carl@X930.Tymnet>, fletcherc, helgem 
Subject: Tonight at BLDG C.. 

1. P034/P19 When trying to initilize a pack formated
   with PAKCP3 (does it in pages) would not recognize
   it as having formated correctly.

2. System C24 up with one pack ampex disk

3. Couldn't get APT10 to work. Will figure it out
   how it works, not enough time tonight.

4. I will be in tomorrow.

-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 27 Mar 86 10:57:25 PST 
To: jms, Carl A Baltrunas <carl@X930.Tymnet>, fletcherc 
Subject: 3330 Test packs.. 

1. Made one pack (PAGE) and one pack (BLOCK) 3330 Ampex
   test system pack. Used the all files tape which I made
   before disolving the 3650 page disk. I used P19 making
   both of the test packs, so what predicted works and
   verified.

2. Tonight I will schedule a KL system to work on getting
   APT10 to work.

   -Osman-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Mar 86 11:05:52 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>, helgem 
Subject: Schedule...Cupertino 

I willbe in Cupertino again this afternoon to resume my meeting with Dennis
Ernst about reconfiguring our micronet.  I shall be working at home until
12.  Please call me if you need anything.  408-945-4314.

/Carl
Received: from C57.Tymnet by X930.Tymnet; Thu, 27 Mar 86 12:12:13 PST
Received: from X62.Tymnet by C57.Tymnet; Thu, 27 Mar 86 12:11:56 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Mar 86 12:10:16 PST 
To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Significant event! 

My car is at 077762 and will pass the 077777 mark on my way to cupertino
this afternoon!  /Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 28 Mar 86 9:06:19 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Today 

I won't be in today, I have a very bad cold.  If you need to reach me, my
phone is (415)790-0608.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 30 Mar 86 11:25:18 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Hotline call 10:15am Sunday. 

30-Mar-85 10:18/JMS
Contact:  Fremont Ops, Elisha Padilla, (415)498-2588, System 28.

Symptom:  SPOOL not running, SPLFIX gets a fatal error on file SPOOL.GBK
        Renaming files, wait...
        LOOKUP failed for (SPOOL)SPOOL.GBK.
        Fatal error.
        Enabling SPOOL . . . Enabled.
         Can not continue !
        Please call the TYMCOM-X Support Line : (408) 446-6709

Diagnosis: SPLFIX had created SPOOL.GNW, SPOOL.FNW, and SPOOL.MNW.  The files
        SPOOL.MBK and SPOOL.FBK were dated 28-Feb-86 but SPOOL.GBK did not
        exist (maybe DSKCLN deleted it).  SPLFIX thought it needed to find
        this BAK file just so it could delete it.

Cure:   "COPY SPOOL.GDF,SPOOL.GBK", "RUN (UTIL)SPLFIX".  Done.

Note:   Carl and I both agree that this is not really a hotline problem.
        Maybe we need another policy for this kind of support on weekends.
        Anyway, SPLFIX needs to be changed so that it does not use the old phone
        number.

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 31 Mar 86 4:35:55 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet>, wrs 
Subject: Earthquake, 5.6 at 3:56. 

Woke up at 3:56 Monday morning with the bed shaking.  KGO says it was 5.6 on
the Richter scale, epicenter at Del Val Lake, between Fremont and Livermore.
No damage, some knick-knacks fell off the shelf, and the cuckoo clock started.
All the PDP-10s are up (except 74); I expected to find some down due to disks
powering down.
From: FLETCHERC@X930.Tymnet 
Date: Mon, 31 Mar 86 14:02:01 PST 
To: carl, osman, dencoff, bring, helgem, jms 
Subject: TYMCOM-X meeting 


TYMCom-x MEEting will be delayed until 11:00 a.m. on Tuesday
April 1.  See you there. This is not an April fool's joke.

	Craig

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 31 Mar 86 15:01:24 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: CONSAT/TYMCOM class. 

I am in the TTE 235 class until Friday.  I cam across some interesting info.

SUB HOST ADDRESSING - Allows a user to connect to a particular line or set of
lines on a TYMCOM (async host interface).  Selection is based on two 
characters in the username.  For instance, "TRAINING78", "TRAINING75", and
"TRAININGKL" would allow connection to the 780 host, the 750 host, or the
KL host.

PORT ADDRESSING - Allows a host (or user) to connect to a particular line
on a CONSAT (async terminal interface).  Selection is based on digits after
a period in the host number.  For instance, "JMS:931.19" and "JMS:123.1".
Each addressable line can have many HOST.NUMBER combinations assigned to it.
Any host can seize an addressable port as long as no one else is using it.
These lines can also be used to log in from.

Addressing features must be enabled on a line by line basis in the TYM file.
Received: from C36.Tymnet by X930.Tymnet; Mon, 31 Mar 86 16:01:57 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Mon, 31 Mar 86 14:38:14 PST 
To: tymops.loadii@ontyme 
Cc: Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven <OSMAN@X930.Tymnet>, Joe
	Smith <JMS@X930.Tymnet>, Bill Mortenson <HELGEM@X930.Tymnet>, Craig
	Fletcher <FLETCHERC@X930.Tymnet>, Tymcom-X Acct. Sup.
	<TXSSUP@X930.Tymnet> 
Subject: Users to validate for LOADII. 



                                  MEMORANDUM


                                                              TYMCOM-X SYSTEMS

DATE:  31-Mar-86  14:39:16

  TO:  TYMOPS.LOADII

  CC:  Craig Fletcher

FROM:  Dennis Coffey  (Tymcom-X Systems)

SUBJ:  Users to validate for LOADII. 

------------------------------------------------------------------------------

      The following users need to be validated for use of LOADII.  All users
are members of the Tymcom-X Systems unit -- the organization which writes and
maintains the Operating System and utilities for the Tymcom-X hosts.  

      The reason for their use of LOADII is to re-load the node code for the
micro-Tym-Bases in the "micro-network" (currently 5 nodes) which supports the
unit's lab hosts.  These hosts (930, 62 and 95) are used for development and
testing of the Operating System and utilites for Tymcom-X hosts.  The hosts
from which we will need to run LOADII are those on which the micro-Tym-base
code is developed and stored:  39 and 57.  The users are:

        USER                USERNAME    GAN UUN       COST CODE
        --------            --------    --- ------    ---------
        Carl Baltrunas        CARL        3 352477    4011
        Joseph M. Smith       JMS         3 431116    4011
        Dennis Coffey         DENCOFF     3 410765    4011
        Osman Guven           OSMAN       3 156346    4011
        Bruce Ring            RING        3 231227    4011
        (TXS Unit gen. use)   SYSMAINT    3  50527    4011
        (TXS Acct. Sup.)      TXSSUP      3 160031    4011

      If more information is needed, please contact me, at Ontyme
TXS.D/COFFEY, or phone 415/794-2588 (or tie-line 116, X2588).  The unit
manager is Craig Fletcher:  Ontyme TXS.C/FLETCHER, phone 415/794-2403
(or tie-line 116, X2403).

      Thank you,
      Dennis Coffey
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 2 Apr 86 2:05:12 UT
From: FSC.C/FLETCHER@Ontyme.Tymnet 
Date: 01 APR 86 16:46:19 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P42141@Ontyme.Tymnet 
Subject: "DISTRIBUTION Terry Dager Ken Bedell"... 

DISTRIBUTION
 
 
Terry Dager
Ken Bedell
Phil Marson
Terry Griffin
Scott Holiman
 
Gary Walker
Mark Lauver
Bert Novak
 
Bob Hall
Dick Kovack
Pat Lue
 
Bill Fischer
Gary Perez
Kent Clampitt
Bruce Keim
Tom Mueller
 
Craig Fletcher
Lloyd Claytor
 
Dick Gunther
Walt Eisman
 
.pa

M E M O
 
Date:         March 25, 1986
 
To:           Distribution
 
From:         Ed Roop
 
Subject:      Overview of Bldg. 'A' System Shutdown
 
*****************************************************************
 
Current System:
 
   940 systems are C1, C2, C6, C8, C9, C11, C14, C16, and C20
 
   PDP10 systems are C22, and C70
 
 
Tentative Schedule
 
 5/86  -  Systems C1 and C9 taken out of service.
       *  Removal is dependent on the Netval engine going into
          production and replacing the CUD function on C1.
          Target date is approximately 4/15.
 
       -  C22 swapped with C24 in Bldg. 'C'.
       *  Dependent on SA10 13350 upgrade project in Bldg. 'C'.
 
 6/86  -  Two (2) Augment F3 systems removed from service
 
 7/86  -  Systems C8 and C16 taken out of service.
       *  Requires new Tymvalidate program to be running on a
          PDP10.  Dynatax users will also be required to use
          another electronic mail system.
 
       -  System C11 will be taken out of service per MDSI
          contracts.
 
 8/86  -  Systems C2 and C6 taken out of service.
       *  Requires the running of PERP on a PDP10.
          Target date for this is now set at 7/1/86.
 
 9/86  -  One (1) Augment F3 system removed from service
 
10/86  -  System C14 taken out of service.
 
       -  System C70 to be moved to an available KL in Bldg. 'C'
          or Dallas.
 
11/86   - Last  Augment  F3 system in  Bldg.  'A'  removed  from 
          service
 
12/86  -  System C20 taken out of service per MDSI contract.
 

Page 2
 
 
       -  C24 to be moved to an available PDP10 in Northern
          California or taken out of service.
 
 
The  Tymnet  and Ontyme groups will need to determine what to  do 
with network and outdial equipment located in  Bldg.  'A'.   Also 
maintenance will be canceled on the 940 hardware on their removal 
date from service;  but all equipment should be left in place and 
declared excess on 12/86.
 
If  you  have need of more information or have  concerns,  please 
feel free to contact Gary Perez (6681) or myself,  Ed Roop (6566) 
at your convenience.
Received: from B39.Tymnet by X930.Tymnet; Fri, 4 Apr 86 14:38:05 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 4 Apr 86 14:38:02 PST 
To: carl@930, jms@930 
Subject: patch B39-P034/P-7 

P034/P-6 patched to become P034/P-7
in SYS:SYSTEM.SAV only.
installed SCHED1 performance patch /WRS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 6 Apr 86 17:59:42 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Hotline Call 

06-Apr-86 15:50 /Carl
Dallas Operations, Tyrone Hill, (214) 637-7435, System 27.

Problem: PFLLDC crashes, system stays up between 4 min and 2 hrs.
	 Also, DIEREH intersperced over the past 3 days.

Analysis: Nothing positive, system was crashing at PFLLDC in CORE1.
	  Both J and Job reported 0.  Stack showed EXCMPE probably
	  fell into USRFLT then xct PFLLDC at USRFLT+3.  I patched
	  the running monitor & system.sav to determine if the STOPCD
	  was caused by null-job or non-zero LDC.. Added PFLNUL in
	  patch area & system crashed at PFLNUL a while later...

Solution: Still unknown, Steve Atwell in Dallas maintenance tried
	  swapping various boards, the latest were E/UBR and address
	  decoders and after the PFLNUL crash the diagnostics finally
	showed a positive fault in the PI clock interrupt and they
	took over to run diagnostics about 18:00.  Will call me at
	home if it starts crashing again after fixing the problem.
	They feel it is definately HW but wanted help in pin-pointing
	the problem.  (they had replaced all of the pager boards and
	the problem still wouldn't go away).

/Carl   2hr, 20 min.
From: FLETCHERC@X930.Tymnet 
Date: Mon, 7 Apr 86 12:53:07 PST 
To: carl 
Subject: standby time 

No word from you by 12 noon so I have put in your time as 12.8 + 3.0 

Remember that I asked for timecard by noon on Monday.

	Craig
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 8 Apr 86 22:05:11 UT
From: IOD.CUPERTINO@Ontyme.Tymnet 
Date: 08 APR 86 11:08:30 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	CT.DPM@Ontyme.Tymnet, IOD.CUPERTINO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A54789@Ontyme.Tymnet 
Subject: Problems with aux-circuits (TELECOPY) 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-04-08  11:02 PST                  Telex 4993666 MDISI CPTO
 
TO>        Dennis Coffey (TXS.D/COFFEY)
 
COPIES>    Carl Baltrunas (TXS.C/BALTRUNAS)
           Peter Stratman (CT.DPM)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Problems with aux-circuits (TELECOPY)
 
REFERENCE> Attached Ontyme #A54672
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Dennis, please note attached memo from CT France.
 
     Please look it over and let me know if you can be of assistance with
     regard to the problem they are experiencing, or suggest someone who
     might be able to look into the issue.
 
     Thanks and best regards. Ken
 
                                        v
                                        |
                                        |
                                        |
                                        |
                                        V

MSG#:A54672
 IN#:  6588
  TO: CUPERTINO       
FROM: CT       DPM             
SENT: 08 APR 86 08:42:20  
READ: 08 APR 86 08:56:18  
  CC: CUPERTINO  TYMOPS.P/QUEVEDO  TYMOPS.V/STAFFORD  CT.DPM 
      CT.DPM 
 
 
 
 
 
 
  -----------------------M E M O R A N D U M------------------------------
  MDISF                             telephone  ST-CLOUD [33](1)46.02.70.12
  D.P.M
  ------------------------------------------------------------------------
 
  Date:    April 8th, 1986 - 6:36 p.m. CST
 
  To:        Ken BeDell (IOD.CUPERTINO)
 
  Copies:    Patti Quevedo (TYMOPS.P/QUEVEDO)
             Vida Stafford (TYMOPS.V/STAFFORD)
             Francis Bonnin (CT.DPM)
             Jean-Francois Guillou (CT.DPM)
 
  From:      Peter Stratman (CT.DPM)
 
  Subject:   Problems creating aux-circuits to XDS-NET hosts.
 
  ------------------------------------------------------------------------
 
 
  Since the beginning of  this month, we have an important  problem on our
  2020 hosts which prohibits all use of TELECOPY, PERP, and other programs
  that depend on aux-circuits.
 
  The problem occurs as  soon as an attempt is made  to create an aux-cir-
  cuit to an XDS host from one of  our 2020's. This attempt will fail, and
  ALL attempts  after it.  No more circuits  can be  created to  ANY other
  host, XDS, PDP, or 370.
 
  The message that is returned by TELECOPY, for example, is:
 
    UNRECOGNISED ERROR FROM SUPERVISOR - NUMBER 6
 
  We have noticed that a supervisor  takeover will re-enable the possibil-
  ity to create circuits to non-XDS hosts.
 
  We feel that this problem may be due to validation changes, for our sys-
  tems/bases, that  affect the  supervisor. Could  you please  investigate
  this ?  In any  case, this is  creating a lot  of inconvenience  for our
  users, so any help from you to explain and solve the problem very rapid-
  ly would be much appreciated.
 
  Best regards, Peter.
 
 
 
 
 
 
                                    -1-
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 8 Apr 86 22:27:24 PST 
To: TYM.D/ERNST@Ontyme, Dernst@c39 
Subject: Problems with aux-circuits (TELECOPY) 

(begin forwarded message)

Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 8 Apr 86 22:05:11 UT
From: IOD.CUPERTINO@Ontyme.Tymnet 
Date: 08 APR 86 11:08:30 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	CT.DPM@Ontyme.Tymnet, IOD.CUPERTINO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A54789@Ontyme.Tymnet 
Subject: Problems with aux-circuits (TELECOPY) 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-04-08  11:02 PST                  Telex 4993666 MDISI CPTO
 
TO>        Dennis Coffey (TXS.D/COFFEY)
 
COPIES>    Carl Baltrunas (TXS.C/BALTRUNAS)
           Peter Stratman (CT.DPM)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Problems with aux-circuits (TELECOPY)
 
REFERENCE> Attached Ontyme #A54672
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Dennis, please note attached memo from CT France.
 
     Please look it over and let me know if you can be of assistance with
     regard to the problem they are experiencing, or suggest someone who
     might be able to look into the issue.
 
     Thanks and best regards. Ken
 
                                        v
                                        |
                                        |
                                        |
                                        |
                                        V

MSG#:A54672
 IN#:  6588
  TO: CUPERTINO       
FROM: CT       DPM             
SENT: 08 APR 86 08:42:20  
READ: 08 APR 86 08:56:18  
  CC: CUPERTINO  TYMOPS.P/QUEVEDO  TYMOPS.V/STAFFORD  CT.DPM 
      CT.DPM 
 
 
 
 
 
 
  -----------------------M E M O R A N D U M------------------------------
  MDISF                             telephone  ST-CLOUD [33](1)46.02.70.12
  D.P.M
  ------------------------------------------------------------------------
 
  Date:    April 8th, 1986 - 6:36 p.m. CST
 
  To:        Ken BeDell (IOD.CUPERTINO)
 
  Copies:    Patti Quevedo (TYMOPS.P/QUEVEDO)
             Vida Stafford (TYMOPS.V/STAFFORD)
             Francis Bonnin (CT.DPM)
             Jean-Francois Guillou (CT.DPM)
 
  From:      Peter Stratman (CT.DPM)
 
  Subject:   Problems creating aux-circuits to XDS-NET hosts.
 
  ------------------------------------------------------------------------
 
 
  Since the beginning of  this month, we have an important  problem on our
  2020 hosts which prohibits all use of TELECOPY, PERP, and other programs
  that depend on aux-circuits.
 
  The problem occurs as  soon as an attempt is made  to create an aux-cir-
  cuit to an XDS host from one of  our 2020's. This attempt will fail, and
  ALL attempts  after it.  No more circuits  can be  created to  ANY other
  host, XDS, PDP, or 370.
 
  The message that is returned by TELECOPY, for example, is:
 
    UNRECOGNISED ERROR FROM SUPERVISOR - NUMBER 6
 
  We have noticed that a supervisor  takeover will re-enable the possibil-
  ity to create circuits to non-XDS hosts.
 
  We feel that this problem may be due to validation changes, for our sys-
  tems/bases, that  affect the  supervisor. Could  you please  investigate
  this ?  In any  case, this is  creating a lot  of inconvenience  for our
  users, so any help from you to explain and solve the problem very rapid-
  ly would be much appreciated.
 
  Best regards, Peter.
 
 
 
 
 
 
                                    -1-

(end forwarded message)

Dennis,
  Could this be a micronode problem...?  I thought that we were experiencing
a similar problem when we tried to build a circuit to a host that was down
and the supervisor message zapped the micro-node somehow...  Anyway, if you
get this message let me know what the status is.

/Carl
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 10 Apr 86 22:05:52 UT
From: CT.DPM@Ontyme.Tymnet 
Date: 10 APR 86 10:40:57 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: IOD.CUPERTINO@Ontyme.Tymnet, TYMOPS.P/QUEVEDO@Ontyme.Tymnet,
	TYMOPS.V/STAFFORD@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet,
	CT.DPM@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A56272@Ontyme.Tymnet 
Subject: Aux-circuit problems on 2020's 

 
 
 
 
 
  -----------------------M E M O R A N D U M------------------------------
  MDISF                             telephone  ST-CLOUD [33](1)46.02.70.12
  D.P.M
  ------------------------------------------------------------------------
 
  Date:    April 10th, 1986 - 8:34 p.m. CST
 
  To:      Ken BeDell (IOD.CUPERTINO)
 
  Copies:  Patti Quevedo (TYMOPS.P/QUEVEDO)
           Vida Stafford (TYMOPS.V/STAFFORD)
           Dennis Coffey (TXS.D/COFFEY)
           Carl Baltrunas (TXS.C/BALTRUNAS)
           Francis Bonnin      Jean-Francois Guillou (CT.DPM)
 
  From:    Peter Stratman (CT.DPM)
 
  Subject: Aux-circuit problems on 2020's
 
  Reference:    A54672 04/08/86
 
  ------------------------------------------------------------------------
 
 
  Ken,
 
  As of yesterday, the problem we had  has been solved for 3 systems.  The
  systems are  : 83, 170, and  264. This leaves  59, 60, 92, 301,  and 443
  which still don't have TELECOPY or PERP.
 
  I realise that you may not have had up-to-date information on our exist-
  ing 2020's. Please note that host 184  is temporarily off the network as
  it is being replaced by a VAX.
 
  I do not know why the three systems are now able to create circuits nor-
  mally to IX's, so  I have copied in all persons  contacted previously. I
  hope someone will be able to explain this problem soon !
 
  Best regards, Peter.
 
 
 
 
 
 
 
 
 
 
 
                                    -1-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 11 Apr 86 11:12:52 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: ONCDSK does not use HOM page to fine SAT.SYS 

The reason why ONCDSK dies on the 2020 is because it does not use %HOM+HOMSAT
to locate SAT.SYS - it uses the "constant" LPNSAT instead.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 14 Apr 86 0:11:14 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: DStandby duty 

I am on call this week.		Joe (415)790-0608
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 14 Apr 86 23:36:07 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>,
	scapik, dernst 
Subject: Problems reloading node 2346. 

Reloading node 2346 (the box with the expansion chassis) presents a problem.
The only way to get it to go to its bootstrap is to turn DC power off and back
on again.  Doing this crashes the other two PDP-11s that connect to it via
DR11s.  This causes node 6657 do go to ODT (it can be restarted via 1000G but
930 has to be recycled) and node 10115 do go to ODT (it can not be restarted and
62 has to be recycled).

We need a different PDP-11 front panel for our connector node.  Perhaps falling
back to the old set up where the 11/03-SE has only one DRV11 so that it does
not crash any other nodes.  

			Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 16 Apr 86 0:35:28 PST 
To: Dennis Ernst <DErnst@C39.Tymnet>, Joe Smith <jms@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet> 
Subject: pitiful micronet 

Well, I finally got everything loaded with the code from (new11:57) or
(dernst:57) whichever was newest...

    throughput is atrocious at the moment, from home, long turnaround
    time... makes me think I'm using GTE's TELENET?

Here's the scoop:

    6657 loads fine with LOADII, only one problem (see below).

    6760 had some cards jumpered wrong, DLV11-J was either bad or had
	 the jumpers address and interrupt vectors setup NOT for the
	 console; DUV11 were switched wrong for TYMNET and for uNet.
	 After fixes, it loaded fine with LOADII.  (did not check to
	 see if co configuration links were correct)  However, it seemed
	 to think that host 95 wasn't there.... (see below).

    2346 would not load through 6657 or 10115 using NODLOD at first.
	 jumpers were wrong for DLV11-J, reset them and console worked.
	 as a last resort I pulled the DRV11 cables out for 6657 and
	 flipped them over and swapped J1 & J2.  It loaded fine from
	 6657 after that.  I attempted the same swap with the DRV11 to
	 10115 and met with similar results (loading went ok).  But...
	 although it loaded from 10115, the link between 10115 and 2346
	 never stayed up (if it ever came up), same if loaded from 6657.
	 If I attempted to login to :930, I got the message "host not
	 available through net", yet... it let me login to XRAY:st@6657
	 and XRAY:st@2770 without any trouble.

   10115 loaded fine with LOADII, and seemed to be configured ok. Seemed? 
	 becuse attempts to login to 930 or 95 were met with the weird
	 message "NET940/111 circuit cleared" which we have seen before.
	 Also, it never connected with 2346 even though it could load it!
	 this "NET940/111" message also comes from 6760... did we lose our
	 group and class status for these nodes again?

The load device... Ok Dennis, fine when I ^C out of NODLOD that I have to
    reload or restart 6657 to get rid of the "*" busy status... but when I
    finish loading 2346 and it says "F" status, then wait awhile and it
    turns into "*" without me doing anything and the "*" won't go away for
    a long time (if ever), and I want to load 10115 from 930 through 6657
    and 2346, now what do I do????  The same thing happens if I load 2346
    from 10115.  The load device stays busy for a long time and I can't even
    reload the same node if I crash it or swap boards or some such...


I think I covered everything...  I cannot spend any more time on this problem
at the moment!  I also cannot put up with any more downtime for system 62.
Joe & Dennis, can you work on getting these things to talk to each other?  I'd
appreciate it if you could keep 10115 up (but if you can work on that part of
the problem first, today...wednesday is a good time to work on 10115 and 2346
while I'm taking my long lunch with the old TYMCOM-X group.

The whole focus of this network is to allow access to our machines from each
other without going through TYMNET nodes... it would be nice if we could do
at least that much...

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 16 Apr 86 1:56:30 PST 
To: Joe Smith <jms@X930.Tymnet>, Dennis Ernst <DErnst@C39.Tymnet> 
Subject: One more thing... 

The DUV11 links need some attention to make sure that they have
the proper cros-ovrs and such... it seems that 2346 thinks the link to
6760 is NX ?   10115 should think it's link to 6760 is NX but not the
one from 2346!  Sigh!
/Carl
Received: from B39.Tymnet by X930.Tymnet; Wed, 16 Apr 86 10:25:38 PST
Return-path: <DERNST@B39.Tymnet> 
From: DERNST@B39.Tymnet 
Date: Wed, 16 Apr 86 9:39:41 PST 
To: carl@x930.tymnet, jms@x930.tymnet 
Subject: pitiful micronet 

Carl,

   Here is what I think.  

H6657   There used to be an OLD load problem like that.  Maybe
	930 has on old version of nodlod.

H6760	Perhaps a device has the same address as the DRV for 95.
	That would give some strange results.  The most likely thing
	is a loose cable to the DRV.  Less likely, but possible is
	a broken DRV.

H2346	The DUV to 6760 is not addressed correctly.

H10115	The link to 2346 may be down because you did the load over it
	and it may have a clean-up problem.

Dennis

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 16 Apr 86 14:38:19 PST 
To: Dennis Ernst <DErnst@C39.Tymnet> 
Subject: Where is the newest NODLOD 

Where and what version should I expect to find for the newest version of the
NODLOD program.  If that is a problem, it should be easiest to fix by using
the newest code...right...  I'll check the vectors and things later... at the
moment, my concern is that the links show an incredible amount of error counts
on some of the lkinks and i think that that may impede throughput... can you
check on that or tell me how...
ps. where is the document (online) that says what the status device info means?
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 16 Apr 86 14:42:56 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: schedule... 

lunch was fine,... interesting...  anyway, I'll be working at home
today, to cut the temptation to fixup the txs-net stuff that isn't
quite working.  As long as '62 stays up... call me at home if you
want anything... /carl
Received: from B39.Tymnet by X930.Tymnet; Wed, 16 Apr 86 15:01:29 PST
Return-path: <DERNST@B39.Tymnet> 
From: DERNST@B39.Tymnet 
Date: Wed, 16 Apr 86 14:59:52 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Where is the newest NODLOD 
In-reply-to: your message of Wed, 16 Apr 86 14:38:19 PST

CARL,

LOOK IN MY DIRECTORY ON 930 FOR NODLOD.  WHICH LINKS HAVE ERROR
COUNTS?  NEVER WAS AN ONLINE VERSION OF STATUS DEVICE INFO.  JUST
CALL WHEN THE COUNTS ARE HIGH FOR LINES THAT ARE UP.

DENNIS
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 18 Apr 86 22:06:50 UT
From: IOD.BEDELL@Ontyme.Tymnet 
Date: 18 APR 86 09:38:05 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: CT.DPM@Ontyme.Tymnet, NTD.D/ERNST@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet,
	CT.DPM@Ontyme.Tymnet, INTL.PATTI@Ontyme.Tymnet,
	TXS.D/COFFEY@Ontyme.Tymnet, INTL.VIDA@Ontyme.Tymnet,
	IOD.CUPERTINO@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A61596@Ontyme.Tymnet 
Subject: Problems with aux-circuits to 940net 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-04-18  09:27 PST                  Telex 4993666 MDISI CPTO
 
TO>        Peter Stratman (CT.DPM)
 
COPIES>    Dennis Ernst (NTD.D/ERNST)
           Carl Baltrunas (TXS.C/BALTRUNAS)  Jean-Fran,cois Guillou (CT.DPM)
           Francis Bonnin (CT.DPM)           Patti Quevedo (INTL.PATTI)
           Dennis Coffey (TXS.D/COFFEY)      Vida Stafford (INTL.VIDA)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Problems with aux-circuits to 940net
 
REFERENCE> Your Ontymes: A54672 and A56272
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Peter, I spoke with Dennis Ernst today regarding the aux-circuit
     problem reported by you on April 8th and 10th.
 
     Dennis believes this to be a known bug, which has been fixed in the
     next version of code; this newest version will be tested very soon.
 
     Dennis will send you an Ontyme once more verification and testing has
     been completed.  Best regards. Ken
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 18 Apr 86 22:07:13 UT
From: IOD.BEDELL@Ontyme.Tymnet 
Date: 18 APR 86 11:27:44 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: CT.TECHSERV@Ontyme.Tymnet, IOD.CUPERTINO@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	IPC.J/KRIVANEC@Ontyme.Tymnet, FIN.P/LUE@Ontyme.Tymnet,
	CT.ACT@Ontyme.Tymnet, CT.ACT@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet,
	CT.TECHSERV@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A61662@Ontyme.Tymnet 
Subject: CT Accounting Project 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-04-18  11:13 PST                  Telex 4993666 MDISI CPTO
 
TO>        Eric Ja"y (CT.TECHSERV)
 
COPIES>    [see attached Copy Distribution List]
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   CT Accounting Project
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Eric, I believe the last correspondence on this topic was your memo
     dated February 17th (A22104).
 
     Due to other commitments and priorities during past two months, I
     have spent little time on this subject.  Also, many other items have
     an indirect affect on some of the issues, therefore I have not con-
     sidered any of the items critical from a viewpoint of timing.
 
     In reponse to your memo, please note the following which refer to
     your item numbers:
 
      1.  According to Dennis and Carl Baltrunas, the INFO data base will
          eventually reside on all Tymcom X hosts.  Eventually, the OPER
          names may be removed from host 930, but no action is being taken
          at this time.
 
          More information will be supplied on these items in the future.
 
          The replacement for PERP is forthcoming, as has been indicated
          by TXS in the past; should be a matter of a few weeks at most.
 
      2.  Jan understands your needs.  Oper names Jay and Stratman are
          already valid.
 
      3.  As you are likely aware, the Commercial Billing system will
          migrate to St. Louis by September.  I do not think we should
          eliminate the regular accounting process (invoices and detail
          supplied by MIS and/or St. Louis), even though you have your own
          accounting in place.  I say this because it may cause unforseen

 
CT Accounting Project                                               Page 2
 
 
          problems (e.g. Tymshare UK e-o-m accounting as related to usage
          of hosts S51/S53).
 
          At this point in time I am not concerned about Royalty account-
          ing being supplied by your system because 1) the e-o-m process
          will continue as referenced above 2) vendor royalty payments are
          now the responsibility of ACS rather than IOD (MDISI) and 3) CT
          CT has their own agreements for Express and Focus.
 
     That's the extent of my comments at this time.  Best regards. Ken
 
 
 
 
COPY DISTRIBUTION>
 
           Carl Baltrunas (TXS.C/BALTRUNAS)
           Dennis Coffey (TXS.D/COFFEY)
           Jan Krivanec (IPC.J/KRIVANEC)
           Pat Lue (FIN.P/LUE)
 
           Etienne Bouillot (CT.ACT)
           Patrice de Berranger (CT.ACT)
           Jean-Fran,cois Guillou (CT.DPM)
           Yves Menard (CT.TECHSERV)
           Peter Stratman (CT.DPM)
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 21 Apr 86 11:01:41 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: I am on call this week. 

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 22 Apr 86 18:06:19 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: 24XP24 

BOTLOD works, now DIRIT doesn't.  (CARL)DIRECT and CKSUM still work,
but DIRIT says "TOTAL OF 0 PAGES IN 0 FILES".  Grrrrrrr.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 23 Apr 86 23:05:59 PST 
To: Dennis Coffey <dencoff@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Mux 

CARL is now valid on 2096, I didn't try OSMAN or JMS.
I am still not valid on 1274... T'would be nice for both,
but I won't make waves as long as one is around.
/Carl
Received: from C36.Tymnet by X930.Tymnet; Thu, 24 Apr 86 11:00:51 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Thu, 24 Apr 86 10:45:05 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Re: Mux 
In-reply-to: your message of Wed, 23 Apr 86 23:05:59 PST

Carl, et.  al.:

      I finally (today) get in touch with Mike Macek (mgr.  of NetCon & NetQC)
about getting us on MUX 1274.  There is no problem with our being on 1274:
there has just been a month-long game of phone tag between him and me,
exacerbated by my being away for my classes.  Mike promised to get us on 1274
by tomorrow.  If you three are not on 1274 by Monday please let me know:  I
now have a mechanism for getting in touch with Mike when he's away from his
office.

      Re.  LOADII:  Mike told me who to contact.  I tried the recommended
Ontyme to TYMOPS.LOADII, without any response, so I'll now try a phone call.
Please bring me up to date:  do we want the same usernames as for the MUX (our
personal usernames)?

Thx!
D.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 24 Apr 86 22:17:38 UT
From: IPC.R/DANIELS@Ontyme.Tymnet 
Date: 24 APR 86 10:57:57 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.C/BALTRUNAS@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet,
	TXS.@Ontyme.Tymnet 
Message-id: A65110@Ontyme.Tymnet 
Subject: "Carl; Reference your Ontyme A64609."... 

Carl;

Reference your Ontyme A64609.

Set the time and place.

Rick
From: FLETCHERC@X930.Tymnet 
Date: Thu, 24 Apr 86 16:50:41 PST 
To: carl 
Subject: rick's memo on patching systems 

Can i see a copy of the memo?

	craig
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 25 Apr 86 10:44:59 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet>, wrs 
Subject: "Hackers - Wizards of the Electronic Age" on Channel 9. 

KQED is showing a documentary with Greenblatt, Stallman, Wozniac, Budge,
Atkinson, the PDP-1, MIT, etc.  On channel 9, 11:00pm Friday and 4:30pm Sunday.
From: FLETCHERC@X930.Tymnet 
Date: Fri, 25 Apr 86 16:55:03 PST 
To: carl 
Subject: missed you 

Didn't see you around today. Please explain.

	Craig
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 25 Apr 86 22:00:05 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Mail/Messages 

1) Monday at 9, meeting with Wayne, see you then.
2)) Not around, was in Cupertino... mtg with Acct 10-12
                                  talked to John Kopf 3-5
3) Had lunch and checked with a few people in between meetings.

Spent remainder of time in transit... am working at home tonight,
haven't logged into 930, since I was on 62 for most of my work...

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 25 Apr 86 23:43:34 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: C24 keeps crashing in a two-instruction loop, interrupts off. 

Symptom: RUN is on, PI ENABLED all off, PI IN PROGRESS on channel 3, PI
REQUEST on channel 7, pressing SINGLE INST and hitting CONT shows that the PC
is in a 1, 2, or 3 instruction loop, one of the instructions has zero in the
left half.  Can't do a 147 or a 146 restart - parts of COMMON have been wiped
out.

The loop for (SYS)CRA001 was around USRXIT+17.  The code there hsav been wiped
out by what looks like a stack.

This problem also occurred this afternoon in P19.  The operators were unable
to read-in BOTLOD until after core was cleared.  I have experienced this 
sort of hang with other monitors, but only on C24's hardware.  Could be
flakey pager or UUO trapping.  C24 wouldn't stay up long enough for me to
check (SYS)CRA002.

		/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 26 Apr 86 19:01:05 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Hotline Call 

26-Apr-86 11:46 and 12:15/Carl
Contact:  CUPC Operations, Steve Carrothers, (408)446-7705, System C26

Symptom:  System crashes attempting all-files with TITO.  Last user done
	  was (BETATESTINT).  Assumption was that a file in BETATESTINT
	  was bad.  Checked with Osman since similar problem occurred on
	  13-Apr-86, system 26.

Problem:  File 041486.SAT in (BILLING10) was bad.  File could be looked
	  at, but not read completely.  CKSUM and copy failed & crashed
	  the system.

	  Examined file with TSTSUP.  File has a hole in it an otherwise
	  appears to be normal.  These files should not have holes in them!
	  Apparently something is deleting a page without updating all the
	  proper file-system tables and thus a hole in this file seems to
	  be causing a crash.

Solution: The file was marked bad by hand using TSTSUP and this prevents
	  TITO from attempting to put it on tape.  The file was renamed to
	  (CRASH:26) for further examination.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 26 Apr 86 19:05:34 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>, William R Soley
	<wrs@C39.Tymnet>, Dennis Ernst <DErnst@C39.Tymnet> 
Subject: Happy 1st Wedding Anniversary 

Carl & Cherie tied the knot 27-Apr-85 2:00:00 PM EST
It is nearly one year later and they are still talking!
They will be celebrating this weekend... join with me
to wish them a very happy first anniversary!!!!!!!!!!!

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 26 Apr 86 19:35:04 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: INTADR stuff. 

The idea of expanding the 70-word INTADR block can be done, I have located all
the places that need to be changed to allow this.  Unfortunately, the following
interrupt causes can not take direct advantage of the extra room in the block:
	(4) Timer trap
	(6) WAKE uuo
	(13) Notice to quit
	(14) Exit in child frame
	(16) TRU limit exceeded
	(0-2,4-12)  All port initiated interrupts except for 3=no room for UUO
These interrupts are detected while not in the job's context and cannot access
user address space.

This is an argument for having a nonswappable process data block, which is
allocated when the frame is assigned, resides in exec virtual address space,
and is pointed to by a new JBT table.

See (M33)INTADR.MEM for details.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 27 Apr 86 18:08:06 PDT 
To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: INTADR thoughts 

Joe,
  is it possible to return in our 35 word block, the "cause" of the interrupt
on a given channel, and not overwrite it when a second interrupt occurs on
the same channel, i.e. force the event to "block"?, so that if we have 2 or 3
causes assigned to the same channnel, we can distinguish the cause, and the
next cause will interrupt us again, etc...

  or does this just get us back to the "we only have one bit" scenerio... i
think it does, so this is again, all for naught... sigh.

/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Sun, 27 Apr 86 22:50:26 PDT 
To: Monitor Group <osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Bill Mortanson <helgem@930.Tymnet>,
	Dennis Coffey <dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tyment> 
Subject: I am on call 4/28 to 5/4 


Received: from B39.Tymnet by X930.Tymnet; Mon, 28 Apr 86 11:18:11 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 28 Apr 86 11:09:53 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Happy 1st Wedding Anniversary 
In-reply-to: your message of Sat, 26 Apr 86 19:05:34 PST

Well, Happy Anniversary, of course!  May you have many more.  -Bill
(Please forward to Cherie - don't know her address)
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 28 Apr 86 19:52:15 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: HBO interference 

If you want to see what the message looked like when the hacker messed with
HBO's signal, you can catch the replay of Entertainment Tonight.  Set your VCR
for Channel 4 at 6:00am, the item should show up at 6:17am.

Sally often sets our VCR for that time if something interesting comes up on
the 7:30pm showing and she was not ready to tape it then.


Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 28 Apr 86 21:18:34 UT
From: NSS.F/CHEN@Ontyme.Tymnet 
Date: 28 APR 86 22:08:46 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M85169@Ontyme.Tymnet 
Subject: Change of System 37 E-Bus Base Installation Schedule 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Tue, 29 Apr 86 2:36:41 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 

                            M E M O R A N D U M

                                                        [] TYMNET - NETWORK
                                                           SYSTEMS SUPPORT
DATE>      28 APR 86  13:55

TO>        THE WORLD

COPIES>    Shirley Chiang
           Gary Walker

FROM>      Fun-Lin Chen


SUBJECT>   Change of System 37 E-Bus Base Installation Schedule


-----------------------------------------------------------------------


Due to request from the field, we are changing system 37 E-Bus installation
time from 5/24/86 22:00 to 5/3/86 (SAT) 22:00 (Central Time).

Systems running with the new version of E-Bus codes are:

    25, 26, 31, 36, 54, 55, 58.


Current installation plan for the rest of the PDP-10 systems is:

      WEEK               SYSTEM            DATE            TIME
----------------------------------------------------------------------------
II.  5/2 - 5/8            C33            5/2  (FRI)    2400(Pacific)
                          C35            5/2  (FRI)    2400(Pacific)
                         *D37            5/3  (SAT)    2200(Central)
                          F28            5/7  (WED)    2300(Pacific)
                          F30            5/8  (THU)    2400(Pacific)
----------------------------------------------------------------------------
III. 5/9 - 5/15           C34            5/10  (SAT)   0600(Pacific)
                          C57            5/10  (SAT)   0600(Pacific)
                          F74            5/12  (MON)   0030(Pacific)
                          F38            5/13  (TUE)   2100(Pacific)
----------------------------------------------------------------------------
IV.  5/16 - 5/22          C22            5/16  (FRI)   2400(Pacific)
                          C29            5/16  (FRI)   2400(Pacific)
                          D65            5/16  (FRI)   2200(Central)
                          D56            5/18  (SUN)   0200(Central)
                                                                Page  2

----------------------------------------------------------------------------
V.   5/23 - 5/29          C70            5/23  (FRI)   1900(Pacific)
                          D27            5/23  (FRI)   2200(Central)
                          D23            5/24  (SAT)   0300(Central)
----------------------------------------------------------------------------
VI.  5/30 - 6/5           D79            5/31  (SAT)   2200(Central)
                          D72            6/1   (SUN)   0230(Central)


If you have questions about this revised schedule, please call me at
(408)-942-5190 or send an Ontyme message to nss.f/chen.

Regards,

Fun-Lin Chen
HQTECH
Applications Support
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 29 Apr 86 16:13:30 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Found why INFO stopcodes caused recursion is DIE handler. 

When setting up accumulator U to point to the right LDB, the wrong value
was used.  Attempting to output characters to a detached line caused the
recursive stopcode.  (M33)DIETYO.PAT has been installed on 26, it will be
copied to all 10s.			JMS
Received: from C36.Tymnet by X930.Tymnet; Wed, 30 Apr 86 11:27:12 PDT
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Wed, 30 Apr 86 11:05:28 PDT 
To: (Monitor Group:), Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven
	<OSMAN@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet>, Bruce Ring <BRING@C36.Tymnet>,
	Tymcom-X Acct. Sup. <TXSSUP@X930.Tymnet> 
Subject: 2 questions. 

      1.)  Terminal lines to our micro-network:  Yasu has moved the 4 lines
from the cluster of cubicles which includes Bruce's terminal, mine, and the
terminal ( ADM-31 & slaved TI-820 RO) used for 930 & 62 backups.  They are now
in the vicinity of the patch-panel on the back of the rack with our host
cluster.  They are the four lines ending on the floor, to the left of the
other lines from the ceiling.  They are identified by the colored tape:
	yellow:  Bruce's terminal
	blue:  my terminal
	red:  'backup' terminal
	orange:  unused, currently ending in unused cubicle, but long enough
	    to reach any of the other cubicles, like Bruce's Diablo.
	
      Where do they plug in at the host site?  Also, Yasu needs to know where
Craig's line will plug in when he moves it off the Engine, so Craig will be
out of the Network for the least time possible.

      2.)  Have received a call from Mallenkrodt:  their host is suddenly
flakey, with an average up-time of 2 hours.  Sometimes it auto-boots, and
sometimes it must be manually booted.  I have some information about the last
2 crashes, but will need some help interpreting.

Thanks,
Dennis
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 30 Apr 86 16:40:03 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, dernst 
Subject: Micronode can't talk to TYMNET. 

Our PDP-11 nodes are currently running with code from CURR11.  Does that code
correctly handle "host down"?  (At one time attempting to login to a down or
shut host caused the PDP-11 to get a fatal node error.)

From 930, I tried to build an aux circuit to host 90 in Paris.  TAUX build the
circuit, which was immediately zapped.  From that point on, I could not build
any circuit from 6657 to anywhere in TYMNET.  But new circuits could still
be build from TYMNET nodes to 930.  The outgoing connection to TYMNET remained
in this state until I disconnected the sync line between 6657 and 3146 for
60 seconds.  I tried "TAUX :90" three more thimes, with the same results.

Similar problems occured when I tried to "TAUX :90" from host 95.  It did
not get the circuit established, instead TAUX reported "monitor too slow,
timeout".  Again, the microbase, node 6760 was messed up and stayed that way
until I disconneced its sync line to 3146, waited 60 seconds, and reconnected
the line.  Then I could build a circuit to any host other than 90, 92, and 930.

While all this was going on, 10115 got a fatal node error BPA.  I don't know
if this had anything to do with the "host 9x" problem, but I had to reload
host 62.
			/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 30 Apr 86 18:58:17 PDT 
To: Dennis Coffey <dencoff@X930.Tymnet> 
Subject: things... 

Craig's line... connect to to 2770, there are plenty of free lines on
the back of the dz11 panel.  Ask Joe if you need assistance.

TYMBASIC... please contact Ingrid so I can look over the code so I can
write some routines... since "we" support it, sorta, we ought to know where
the sources are anyway, not just for the project I'm helping Jan with.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 30 Apr 86 19:01:31 PDT 
To: jms 
Subject: new11 vs curr11 

Curr11 is all wrong... configuration of devices wise...
anyway, yes! the bug you describe is exactly one that the NEWW11 code is
supposed to fix.
/Carl
Received: from C36.Tymnet by X930.Tymnet; Thu, 1 May 86 8:56:10 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 1 May 86 8:55:55 PDT
Return-path: <helgem@X930.Tymnet> 
From: Helge Mortensen <helgem@X930.Tymnet> 
Date: Thu, 1 May 86 8:52:39 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: 62 Allfiles 

Carl/
     You may be interested to know that system 62 disks were stored success-
fully to 10 mag tapes on 4/30/86.
				Regards,
				Bill

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 1 May 86 8:57:03 PDT 
To: helgem 
Subject: re: System 62 backup 

Hooray!!!!!!
Received: from C36.Tymnet by X930.Tymnet; Thu, 1 May 86 12:47:28 PDT
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Thu, 1 May 86 12:40:59 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: things... 
In-reply-to: your message of Wed, 30 Apr 86 18:58:17 PDT

Re. TBA:  I've submitted a BATCH request to the CUP-C operators to restore the
latest archived version of TBA (file TBAF.981) to (TSUCOOP:38).  No mail from
OPER to DENCOFF:38, yet, confirming completion of PPR.

/D.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 1 May 86 13:53:37 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: ONTYME Layoff 

Apparently, there was a meeting this morning and ONTYME's development
group of 10 lost 7 people.  Three managers were also cut.  Adele and Lois
and Stuart are still among the survivors.  Ken Dawson was one of the 7
developers given 2 weeks notice.  (The standard line of finding transfer
positions inside the company.)

To Craig:
   i do not know what our RFP situation is, but it would be a very valuable
asset to our support capability, especially our applications group to talk
to Ken Dawson about joining our group.  i think he is interested, but i believe
we don't have any positions.

  i think it would be a waste to see that talent disappear, and have to train
someone new, since Ken already has much of the training we are developing.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 1 May 86 19:55:50 PDT 
To: Peak Maintainer <Peak@X930.Tymnet> 
Subject: Resume, etc. 

Ken,
  I sent a msg to those in our group about the rift in ONTYME and added a
comment to Craig, about your abilities and how you'd fit into our support
picture, but as yet, no response.

What did you do for ONTYME, as in write slot code, assembler/pascal, etc...
I'm trying to talk a group at Orchard Parkway into writing something into a
slot instead of running on a set of host (PDP-10) computers.  Yes, you read
that right... to run something NOT on a 10.  What they're doing is really a
network product and should be built into the net instead of chewing up a lot
of resources, as well as generating lots of network traffic to talk to the
net.

Anyway, if they are interested, might you be interested?  I don't know what
you do in ONTYME, so I don't know your capabilities as far as engine/slot
code writing.

/Carl
Received: from C36.Tymnet by X930.Tymnet; Fri, 2 May 86 14:00:47 PDT
Received: from X62.Tymnet by C36.Tymnet; Fri, 2 May 86 13:59:41 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Fri, 2 May 86 13:29:31 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: TBA sources. 

I could find nothing among the files, except for a list of directories that
the TBA group used to use.  I found losts of ITSRs, but only 2 transmittals.
I found 3 (2 VERY old) transmittals in TRABAS.  The one archive I recovered
(TBAF.981) was ONLY a .INF file!  It was done by Sam Feldman in 1980, from
host 23:  there is NOTHING in any TBA-related directories on 23!

I then did an MHX job, looking at directory listings of the 10 directories
that were originally TBA development directories, on all 'Tymshare' PDP-10s.
The results of that (heavily edited, to remove all non-relavent directories
from the file) are in file:  (DENCOFF:62) TBASRC.LOC.  (This file is 45
pages!)  

All the source stuff and documentation available is in 4 directories on 5
hosts.  2 of these hosts almost exactly duplicate 2 other hosts' contents for
the TBA directories they contain.  Access date shows that these are all
archival directories---consuming 13,000 pages on the 5 hosts!

I have a hard-copy listing of (DENCOFF:62)TBASRC.LOC.  I'll leave it on your
desk.

/D.
Received: from C36.Tymnet by X930.Tymnet; Fri, 2 May 86 16:02:23 PDT
Return-path: <LINDLEY@C36.Tymnet> 
From: LINDLEY@C36.Tymnet 
Date: Fri, 2 May 86 15:36:48 PDT 
To: carl@930 
Subject: Phone message 

Carl:
I received a phone message yesterday intimating that you were unsure whether
I was in the group that was laid off.  The message was probably confused but
as of now I am still here as a member of ACS.  Adele, also.  Situation very
non-positive at the moment as far as morale of group, etc.

Ah, well.  Life is certainly interesting.
-Lois
Received: from B39.Tymnet by X930.Tymnet; Fri, 2 May 86 18:54:09 PDT
Return-path: <DERNST@B39.Tymnet> 
From: DERNST@B39.Tymnet 
Date: Fri, 2 May 86 18:51:58 PDT 
To: CARL@X930 
Subject: NODE CODE 

Carl,
  Everything has been updated in (DERNST) and (NEW111)on 57.
Dennis
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 4 May 86 2:07:44 PDT 
To: WRS 
Cc: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Security problem. 

I stumbled across a way to log in as any user on any PDP-10 without AC
license.  It involves NET940.  Are you aware of it?  /JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 5 May 86 0:47:19 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: I am on call this week 

Home phone (415)790-0608.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 5 May 86 17:03:53 UT
From: NTD.J/KOPF@Ontyme.Tymnet 
Date: 05 MAY 86 08:54:10 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: I61536@Ontyme.Tymnet 
Subject: "Carl: I talked to Bill Euske about"... 

Carl:

  I talked to Bill Euske about your desire to take over EBUS.  He says
that you should submit a formal proposal to him concerning this, and
he can proceed from there.

John Kopf
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 5 May 86 21:17:07 PDT 
To: jms 
Subject: Code in nodes... 


I'd like to know what code is running in our micronodes... where did you
reload them from?  (NEW11:57) or (DERNST:57) ?

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 5 May 86 22:31:09 PDT 
To: Joe Smith <jms@X930.Tymnet>, Dennis Ernst <DErnst@C39.Tymnet> 
Subject: MicroNode weirdness 

Dennis,
  As per our previous conversations... we still haven't had time to look
at the problem between 2346 and 10115 with the DRV11's, but after loading
the new code from system 57, we see pretty much the same as before.

However, in addition to receiveing weirdness about NET940 when trying to
build a circuit to 95 or 930 from 62... if we try to build a circuit to
system 62 from 930, I now get a circuit to host 6!

This is beginning to suspiciously look like a truncation problem of some
sort.  If the host is "known" but inaccessible due to a down link, it tries
to build a circuit to the host on tymnet using the first digit of the host
number given.  Seems pretty ominous!
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 5 May 86 23:41:09 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Node code 

Sorry, I meant to leave you a message Sunday.  6657 and 10115 loaded from
NEW11:57.  2770 loaded from SYS:930.  Tried loading 6760 from NEW11, but it
would not talk to 95, said "host down", it is currently running CURR11:57.
I did not try anything from DERNST.
/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 6 May 86 8:37:34 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Node code 

(DERNST:57) <==> (NEW11:57)  so says cksum		FYI
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 6 May 86 8:39:24 PDT 
To: Joe Smith <jms@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet> 
Subject: Personnel Favor {sic} 

Joe, Dennis...
   Could I get one of you to walk over to Kaiser (Fremont Clinic) to
the personnel department (it is in the Rancho Bldg) and pick up form
#06232 - Dispute Resolution Procedure Form.  I ask, because we are
trying to get a copy of the form without alerting Kaiser Santa Clara
who the form is for...

   This is part of the continuing saga with my wife and her boss.  If
they ask who the form is for, it would be appreciated if you could
misdirect them by saying that it is for a friend, relative, etc...
without using cherie's or my name.

   Also, it would be nice if you could get 2 of them.
I will try to call you to give you more detail sometime around noon
when we get our "long" break.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 6 May 86 11:16:52 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Send Mail... 

...when you get back from Kaiser, so I can phone Cherie,
   or better yet, call cherie to let her know whether you were able
   to get the form...  71-9-985-4249  let me know what happened...

Thanks a whole lot... we owe you a dinner!!!
/Carl
Received: from C36.Tymnet by X930.Tymnet; Tue, 6 May 86 13:01:32 PDT
Received: from X62.Tymnet by C36.Tymnet; Tue, 6 May 86 13:01:11 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Tue, 6 May 86 12:42:02 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Cc: Joe Smith <JMS@X930.Tymnet> 
Subject: re. Personnel Favor {sic} 

I got your message from Joe (my TUMS.TIX file was damaged--hardware read
error--so I couldn't get it on the system), and went to Kaiser today.  Their
personnel dept staff (person) was off today.  Her office was locked, so I
couldn't get the form.  I'll call tomorrow (795-3197), and if anyone is in
personnel I'll pick up the form--I hope.

D.
Received: from C36.Tymnet by X930.Tymnet; Tue, 6 May 86 13:11:26 PDT
Received: from X62.Tymnet by C36.Tymnet; Tue, 6 May 86 13:11:08 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Tue, 6 May 86 13:04:17 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: addendum to last msg. 

Re. the delivery of the Kaiser form to you:  How about my delivering it to you
at your house, with your paycheck Thursday. after work?  I believe your house
is more accessable to my commute route than is Tymnet training's facility.

D.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 6 May 86 17:33:29 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: OPER passwords on 1051. 

Does anyone have a valid OPER password on Rainer Bank's system?  I don't.
Since they are running P034/J I can't use SETE.
Received: from B39.Tymnet by X930.Tymnet; Tue, 6 May 86 22:11:13 PDT
Return-path: <DERNST@B39.Tymnet> 
From: DERNST@B39.Tymnet 
Date: Tue, 6 May 86 22:08:33 PDT 
To: CARL@X930 
Subject: MICRONODE 

Carl,
  I think your theory is right.  There is new code to handle that and
I will look into it.  Might have something by late tomorrow.
Will let you know.

Dennis
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 7 May 86 8:44:19 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: MICRONODE 

(begin forwarded message)

Received: from B39.Tymnet by X930.Tymnet; Tue, 6 May 86 22:11:13 PDT
Return-path: <DERNST@B39.Tymnet> 
From: DERNST@B39.Tymnet 
Date: Tue, 6 May 86 22:08:33 PDT 
To: CARL@X930 
Subject: MICRONODE 

Carl,
  I think your theory is right.  There is new code to handle that and
I will look into it.  Might have something by late tomorrow.
Will let you know.

Dennis

(end forwarded message)

Ah ha... maybe we will see some light at the end of this tunnel yet.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 7 May 86 13:47:32 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet>, WRS 
Subject: LOGOUT loops forever if the UFD interlock is busy. 

LOGOUT gets the UFD interlock before cleaning up the directory (deleting TMP
files and such).  If the DSKB interlock is busy for 6 minutes, it jumps to
the label NXTSTR to do the next structure.  Unfortunately, NXTSTR has "DSKB"
hardcoded in, therefore it loops back trying DSKB again and again.

Dennis reported that there were 6 jobs logged in as WRPNET and all 6 were
in SL wait state, had been for some time, and had JACCT making them immune
to TAKEIT.  I located the call to LOKSTR in the listing and put a JFCL in
the error return in (SYS:28)LOGOUT.SHR.  5 minutes later one of the jobs
timed out, hit the JFCL, did its work, released the UFD interlock, and went
away.  Within a few seconds all the other jobs did likewise.

We should either patch LOGOUT on all systems or distribute a new release as
soon as possible.
				/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 7 May 86 15:30:53 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Cc: Monitor Group <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Craig
	Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>,
	Bruce Ring <ring@X930.Tymnet>, WRS 
Subject: Re: LOGOUT loops forever if the UFD interlock is busy. 
In-reply-to: your message of Wed, 7 May 86 13:47:32 PDT

Joe,
  this is a known bug...

On some systems WRS has patched the interlock check to a JFCL... it happens
a lot on very heavily loaded systems that somehow the interlock gets stuck
and the all jobs get stuck trying to loginn or logout. 

I haven't "patched" it because i think the timeout wait is needed, but... it
ought to fall out of the timeout loop after so many tries.   if you want to
change the source to loop for a few tries, then fall out, fine by me...

This has been a known bug for quite a long time.. in fact I thought you knew
about it... only single bit correction on your memory?  try bi-polar...

/carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 7 May 86 16:28:59 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, WRS 
Subject: Changes to LOGOUT. 

The old code did correctly time out after 6 minutes, but instead of proceeding
on to the next structure, it started over with DSKB again.  The following
FILCOM shows how I left the label NXTSTR intact in case we ever do handle
multiple structures.  It now tries for the DSKB interlock only once, and
outputs a message if it times out and the TTY is still attached.

File 1)	DSK:LOGOUT.471	created: 1350 01-Dec-82
File 2)	DSK:LOGOUT.MAC	created: 1558 07-May-86

1)1	tymfix==1	; tymshare bugfix
1)	if2 <printx	lic needed: wc op sy st hf jl>
1)	;ver 47.1: 12/1/82 - Don't die if running a pre-inactivity logout
**** ;At top of file +2L
2)1	tymfix==2	; tymshare bugfix
2)	if2 <printx	lic needed: wc op sy st hf jl>
2)	;Ver 47.2: 5/7/86 - Don't loop forever if UFD interlock is busy
2)	;		Output correct program name on "KJOB n,n".
2)	;ver 47.1: 12/1/82 - Don't die if running a pre-inactivity logout
**************
1)12	nxtstr:	move a,[sixbit /dskb/]
1)		movem a,strbuf
1)		pushj p,lokstr	;get the interlock
1)		 jrst nostr1	;can not set it
1)		movei a,17
**** ;At NXTSTR
2)12	NXTSTR:	move a,[sixbit /DSKB/]
2)		EXCH a,strbuf		;[47.2] STRBUF is -1 first time thru
2)		CAMN A,[SIXBIT /DSKB/]	;[47.2] Tried DSKB already?
2)		 JRST NOUFD2		;[47.2] Yes, don't loop forever
2)		pushj p,lokstr		;get the interlock
2)		 jrst NOSTR1		;can not set it, flag error, get next STR
2)		movei a,17
**************
1)13		jrst truchk		;go do accounting after checkin tru and gfd
1)	nostr:	pushj p,nlkstr		;so info not used on logout
1)	nostr1:	tlo f,filf		;our count is not really right, -1 in acounting
1)		jrst nxtstr		;but try next anyway
**** ;At NOUFD1+2L
2)12	NOUFD2:				;[47.2] Done all STRs that LOGOUT knows about
2)		jrst truchk		;go do accounting after checkin tru and gfd
2)	nostr:	pushj p,nlkstr		;so info not used on logout
2)	NOSTR1:	tlo f,filf		;our count is not really right, -1 in acounting
2)		jrst nxtstr		;but try next anyway
**************
1)15	wtlok:	sojle s,cpopj	;tried enough?
1)		movei a,1
1)		sleep a,
1)		skipa	;and try again
1)	lokstr:	movei s,ufdgil
1)		movei b,ulkstr	;code to lock it
1)		move c,strbuf
**** ;At WTLOK
2)14	;Routines to get and release the UFD interlock
2)	;[47.2] Rewrote LOKSTR routine to give message on timeout
2)	LOKSTR:	movei s,ufdgil	;Try 360 times (6 minutes)
2)	LOKST1:	movei b,ulkstr	;code to lock it
2)		move c,strbuf
**************
1)15		struuo a,
1)		 jrst wtlok
1)		jrst cpopj1	;set now
1)	nlkstr:	movei b,uclstr	;clear code
**** ;At LOKSTR+5L
2)14		struuo a,	;Set interlock
2)		 SKIPA
2)		jrst cpopj1	;set now
2)		movei a,1
2)		sleep a,	;Sleep for 1 second
2)		SOJG S,LOKST1	;Try again
2)		MOVE N,STRBUF
2)		PUSHJ P,SIXPRT	;Name of structure = "DSKB"
2)		MOVEI M,[ASCIZ /: UFD interlock busy too long, proceeding anyway
2)	/]
2)		PUSHJ P,MSG
2)		POPJ P,		;Error return from LOKSTR
2)	nlkstr:	movei b,uclstr	;clear code
**************

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 7 May 86 17:02:59 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, WRS 
Subject: Hopefully the last word. 

From: William R. Soley <WRS@C39.Tymnet>
  I think there is also a problem in the whole concenpt of the UFD interlock.
  If a process obtains the lock and dies without releasing it, it will be
  forever busy (as I recall).  This has happened to me a few times.  The
  symptom is the same, except that they never come out of sleep.  Someone
  should probably check this, hopefully its just my imagination.  -Bill

From: Carl A Baltrunas <Carl@X930.Tymnet>
    On some systems WRS has patched the interlock check to a JFCL... it happens
  a lot on very heavily loaded systems that somehow the interlock gets stuck
  and the all jobs get stuck trying to loginn or logout.
    I haven't "patched" it because i think the timeout wait is needed, but... it
  ought to fall out of the timeout loop after so many tries.   if you want to
  change the source to loop for a few tries, then fall out, fine by me...

From: Joe Smith
The above two messages appear to have a misconception.  The STRUUO to get the
UFD interlock never waits.  It returns success or failure immediately.  The
surrounding code has a counter that starts at ^D360 and has a SLEEP for 1
second.  This means that whenever the PUSHJ to LOKSTR gives the error return,
then the job has been waiting for 6 minutes already.  The real culprit is
whoever removed the code that checks the job's search list and put the
constant SIXBIT/DSKB/ in at NXTSTR.  That hack caused the infinite loop.

/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 8 May 86 2:48:39 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: System 95 hangs within a few minutes if RPORTN is too big. 

Symptom: 2020 hung, responds to nonzero in 30, was looping at channel 7.
        channel 7 interrupt.  (Does not happen with 95's regular monitor.)

Details: This occurs while running MA034P (the patched P monitor for
        Malenkrodt) which has RPORTN=^D109.  RPORTN>64 means to use 2nd DR11,
        but X95 does not have 2nd DR11 connected.

To KS10 I typed "HA" to halt the KS, "SI" to do a single instruction, and "RP20"
to repeat the SI 20 times.  The PCs reported showed a 5 instruction loop at
STLST in SCNSER.

The list of TTY chuncks it was trying to free had looped back upon itself.
STLST could not find the end, and had AOSed TTFREN several million times.
After the first time thru the looped list, all the back pointers had been
zeroed out.  The following should catch this next time:

File 1)	DSK:SCNSER.P25	created: 0056 08-May-86
File 2)	DSK:SCNSER.MAC	created: 0217 08-May-86

1)109	STLST:	MOVE	T1,TTFREE
1)		EXCH	T1,(T2)		;CHANGE EL POINTER
1)		HRRZM	T2,TTFREE	;AND FREE POINTER
1)		AOS	TTFREN		;MARK ONE MORE FREE
1)		HRRZ	T2,T1		;ANY MORE?
1)		JUMPN	T2,STLST	;YES
1)		POPJ	P,		;ALL DONE
1)	XMTWAK:	HRRZ	F,LDBDDB(U)
**** ;At STLST
2)109	;JMS 7-May-86 - X95(KS) has a nasty habit of looping here.  Added TTYFLM.
2)	STLST:	MOVE	T1,TTFREE	;Pointer to current free list
2)		EXCH	T1,(T2)		;Make current chunk point to the old free list
2)		HRRZB	T2,TTFREE	;Put current chunk at start of free list
2)		AOS	TTFREN		;Mark one more free
2)		TRNE	T1,777777	;If there is a next pointer,
2)		 HLRZ	T2,(T1)		; get its back pointer
2)		CAME	T2,TTFREE	;Does next point back to current?
2)		 STOPCD (CPOPJ,DEBUG,TTYFLM,,<TTY Free List Messed Up>) ;;STLST+7
2)		HRRZ	T2,T1		;Did current chunk have a next one?
2)		JUMPN	T2,STLST	;Yes, continue until all are freed up
2)		POPJ	P,		;All done
2)	XMTWAK:	HRRZ	F,LDBDDB(U)
**************
1)170	AUXSLP:	XCTFU	<ILDB	T2,T1>	;GET CHR
**** ;At AUXSLP
2)170		SETZM	AUXSTR		;Clear out accumulated string
2)		MOVE	T2,[AUXSTR,,AUXSTR+1]
2)		BLT	T2,AUXSTR+AUXSTL-1
2)	AUXSLP:	XCTFU	<ILDB	T2,T1>	;GET CHR
**************

Received: from B39.Tymnet by X930.Tymnet; Thu, 8 May 86 11:22:53 PDT
Return-path: <BARTLETW@C39.Tymnet> 
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Thu, 8 May 86 11:07:31 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: WRS@C39.Tymnet 
Subject: Re: Memo from Jeff... 
In-reply-to: your message of Wed, 23 Apr 86 21:48:09 PST

DID YOU GET CLASS&GROUP DOC'S??? THERE IS  AN OLD DESCRIPTIVE WRITE UP
BY JEFF O. - SEE ATTACHED. IT MAY BE GONE, IT IS OUTDATED SOME.
ANOTHER TOPIC - WILL BE @ LIBERTY FRI. PM. WITH BILL S. WILL YOU BE
IN ~2 TO 3 PM? IF SO WILL STOP IN  - SUBJECT SETOP & WHAT YOU FOUND
RE THE GERMAN ISSUE.
----------------------------------------------------------------
Received: From EMSTYM.Ontyme.Tymnet by C39.Tymnet; Sat, 18 May 85 19:15:18 UT
From: TYM.J/OLIVETO@Ontyme.Tymnet
Date: 18 MAY 85 11:46:01
To: TYM.W/BARTLETT@Ontyme.Tymnet
Cc: TYM.D/TIVOL@Ontyme.Tymnet, IOD.HQ@Ontyme.Tymnet, IOD.HAAS@Ontyme.Tymnet, TNS.RETHO@Ontyme.Tymnet, TNS.MELLET@Ontyme.Tymnet, TNS.DOCQUIER@Ontyme.Tymnet, TNS.NCS@Ontyme.Tymnet, TYM.NS@Ontyme.Tymnet, TYM.R/MCCORMICK@Ontyme.Tymnet, TYM.K/CURTIS@Ontyme.Tymnet, TYM.JEFSEY@Ontyme.Tymnet, TYM.W/BARTLETT@Ontyme.Tymnet, TYM.G/WALKER@Ontyme.Tymnet, TYM.S/WONG@Ontyme.Tymnet, TYM.D/SHINDELL@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet, TYM.D/LANE@Ontyme.Tymnet, TYM.J/TSAI@Ontyme.Tymnet, TYM.A/SHUR@Ontyme.Tymnet, TYM.B/BENNECOFF@Ontyme.Tymnet, TYM.S/WHITE@Ontyme.Tymnet, TYM.J/OLIVETO@Ontyme.Tymnet, FIN.V/STAFFORD@Ontyme.Tymnet, FIN.P/QUEVEDO@Ontyme.Tymnet, FIN.E/LONERO@Ontyme.Tymnet, FIN.S/AIKEN@Ontyme.Tymnet, TYM.@Ontyme.Tymnet
Message-id: I87730@Ontyme.Tymnet
Subject: Documentation on Class & Group Tables

=========================  M E M O R A N D U M  =======================

            TYMNET - McDonnell Douglas Network Systems Company
                External Network Svcs  -  Technical Support
                  Phone: 703/827-9110   Telex: (23) 82693

DATE>      18 MAY 85  14:18

TO>        See Distribution

COPIES>    

FROM>      Jeff Oliveto


SUBJECT>   Documentation on Class & Group Tables


-----------------------------------------------------------------------


On (INTLTECH)CLASS.DOC systems 25, 33 & 70 you will find a document
that describes the class and group tables as they should exist after
Vida Stafford gets the go ahead from Dennis Jonas that the Sup's 
Class & Group space has been upgraded.  This is in about 2 weeks.
Most classes are up-todate.  Classes 24, 35 & 62 are in the most
need of updating.  I have assigned group responsibility were
I best felt it belonged.  By no means should the group responsiblity
assignments be considered official.  I wish all those addressed would
pull a copy and give me any input you can.  After all parties
have been heard from I will give this list to Vida Stafford as
a guide for her to prepare an official "TYMNET Class & Group Reference
Guide".

   Now is your chance to be heard.  Friday June 7th I will give
Vida an updated version of (INTLTECH)CLASS.DOC.  I will assume
all input is complete at that time.

Thanks - Jeff -
----------------------------------------------------------------
Received: from B39.Tymnet by X930.Tymnet; Thu, 8 May 86 12:29:27 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 8 May 86 12:27:34 PDT 
To: carl@930 
Subject: MAP bug 

Ooops!  Looks like a boundary condition error...
Do you have a newer version that fixes this?  Or maybe the source?
-Bill

------------------------------------------------------------------------
!dir (kopfj)ebus.bnd/word
EBUS    BND      63   4-APR-86   31744

!r(spl)map
Map file: (kopfj)ebus.bnd
 (to file) ebus.bnd
(KOPFJ)EBUS.BND ==> EBUS.BND
Map file: ^C

!dir ebus.bnd/word
EBUS    BND      62   4-APR-86   31232

!r ddt
31744.=76000
31232.=75000	^C

!
------------------------------------------------------------------------
Received: from B39.Tymnet by X930.Tymnet; Thu, 8 May 86 18:05:55 PDT
Return-path: <DERNST@B39.Tymnet> 
From: DERNST@B39.Tymnet 
Date: Thu, 8 May 86 17:59:33 PDT 
To: CARL@X930 
Subject: ROUTING PROBLEM 

Carl,
  I found the routing problem you had with just the first digit getting
to the sup.  There are new files in (DERNST:57). 

Dennis
Received: from B39.Tymnet by X930.Tymnet; Thu, 8 May 86 18:06:00 PDT
Return-path: <DERNST@B39.Tymnet> 
From: DERNST@B39.Tymnet 
Date: Thu, 8 May 86 18:03:42 PDT 
To: Carl@X930 
Subject: Routing bug 

Carl,
   I have found and fixed the bug which caused only the first
digit of a host number to be sent to the sup.  There are new files in
(DERNST:57).

Dennis
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 8 May 86 18:47:41 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: DSKCLN does not like P034/P18 or later. 

It is complaining that the RIB to HOM.SYS does not match what DSKCHR says.
If we want to run the old DSKCLN with the new monitor, we have to make ONCDSK
read the RIB to HOM.SYS, and make DSKCHR match what is actually out there,
instead of what REFSTR would like to put there.    /JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 8 May 86 21:14:57 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, dernst 
Subject: New code in 10115. 

I reloaded X62-base from (DERNST:57)ND0115.BND, then deposited +3 in location
210 on the F3 so that 62 would resync with the base.  From the terminal
connected to 10115 I can login to 930 and 95 with no problem.  But from 62,
If I TAUX to 930 or 95 it loses a lot of characters.  In just doing a
DIR, one line in every 10 gets messed up.  Did something happen to backpressure
on AUX circuits thru 10115?  /JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 8 May 86 21:48:53 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, dernst 
Subject: ND6657 ok. 

Reloaded X930-base from DERNST:57.  Works fine.  No problem with TAUX to 62
or 95.  10115 loses characters on outgoing aux circuits to any host.
Received: from C36.Tymnet by X930.Tymnet; Thu, 8 May 86 22:14:49 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 8 May 86 22:14:33 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 8 May 86 22:13:05 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: 10115 et al 

We should try reloading the code, it may just be a flakey something, but
i doubt it.  We should do it just ot be sure.

I'll mail to DERNST and let him know.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 8 May 86 22:20:10 PDT 
To: Dennis Ernst <DErnst@C39.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Subject: Micronode madness 

Dennis,
  Joe reloaded the nodes 5557 and 10115 with the new code and other than
a change in response (XRAY of node 3143/3146 reports a 25% error rate on
the lines at 4.8) it seems to work.  We are able to build circuits now.

  There still seems to be a problem... we will try reloading the nodes
to see if it makes a difference and will let you know... the problem is
that backpressure doesn't seem to work and that there is a major mess
caused by garbling and lossage of characters from any circuits built OUT
of host 62 (on 10115).  Terminals on 10115 do NOT experience the problem.

  It can be demonstrated by TAUXing out of 62 to ?any? host.  Did you
fix one problem only to plague us with another?  This seems similar to the
ancient character lossage we had on the micronodes for AUX circuits...
Was any of that code modified?

/Carl

PS  6657 seems ok... the problem has to date only been seen on 10115:62.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 8 May 86 23:12:01 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: StopCD UUOJB0 C70 

Two crashes on C70 are UUOJB0... one reported a move instruction to
the console, and the other (ops didn't specify)...  worth looking into...
maybe HW problems or something else.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 9 May 86 10:58:56 PDT 
To: Jolee Martinez <PMTS.J/Martinez@Ontyme.Tymnet> 
Subject: Time is 11:00 

Hi there Jolee!
  This is a test, just to make sure I have the correct address.

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 9 May 86 15:44:08 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, SCAPIK 
Subject: Problem with middle RM03 on X95. 

The middle of the three disks on the 2020 went off-line by itself.  The single
circuit breaker in back had tripped.  When I tried to power it back up, the
FAULT light came on.  I moved the disk and ID plug over to the RM03 closest
to the CPU, which works.  Do we have do go thru red tape to get the disk
worked on?			/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 9 May 86 19:35:45 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Patches to the P monitor. 

(M33)PMON.PAT has been updated to include BIOZAP.PAT and DIETYO.PAT.

Differences between P034/P-8 and P034/P-10.

Patch #9, BIOZAP.PAT - Do not crash the base when a zap is received on a port
that is doing block-I/O and the PDP-10 hasn't had a chance to do any clean-up
yet.  This occurs sometimes when a circuit is zapped while running TELECOPY.

Patch #8, DIETYO.PAT - Fix INFO stopcodes so that TTYZNE, WRTSAT, and BADSAT
do not crash the system.  Instead, they output an appropriate message to the
CTY and the system continues.  Fix all other types of stopcodes to update the
key so that the base won't get unhappy and deposit nonzero in 30 until after
the crash dump is started.

P034/P-10 has been installed on the following systems:
9-May-86 in NEWMON on all Dallas machines at the request of Tom Marconi.
D23, D25, D27, D31, D37, D54, D56, D65, D72, D79.
Received: from X62.Tymnet by X930.Tymnet; Mon, 12 May 86 11:00:58 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Mon, 12 May 86 10:40:19 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven <OSMAN@X930.Tymnet> 
Subject: Need your help. 

Need help from one of you two:  host 169's Oper password file doesn't have the
required license, so se (Joe & I) need soemone who knows the SETE passowrd for
the N monitor to set the license on DRWSPR.EPO.  Plese get in touch with Joe
or me ASAP.  

Thx.
D.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 12 May 86 12:58:19 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: ON Call 

I am on call this week.  5/12 to 5/18
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 13 May 86 1:04:02 UT
From: PMTS.J/MARTINEZ@Ontyme.Tymnet 
Date: 12 MAY 86 14:32:50 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J40021@Ontyme.Tymnet 
Subject: "TO: CARL A BALTRUNAS FROM: JOLEE"... 

TO: CARL A BALTRUNAS
FROM: JOLEE E MARTINEZ

HI YA CARL,
     I GOT YOUR MESSAGE.  TODAY HAS BEEN CRAZY BEING THE FIRST DAY
BACK TO WORK AND ALL. I AM SWAMPED.  HOPE YOU HAD A NICE WEEKEND.
TAKE CARE AND I'LL BEING TALKING TO YOU LATER.

CIAO,  JOLEE
From: Steve Capik <FSC.S/Capik@Ontyme.Tymnet> 
Date: Tue, 13 May 86 8:37:55 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Steve Capik <FSC.S/CAPIK@Ontyme.tymnet> 
Subject: ken dawson 

Carl
     I don't know if you've heard that ken got laid off or not.  I know you 
mentioned something about trying to get ken into the tymcom X group.  How
is everything working out on the mini 11 net.   
					enjoy
						steve
Received: from B39.Tymnet by X930.Tymnet; Tue, 13 May 86 17:41:43 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 13 May 86 17:32:13 PDT 
To: Carl A. Baltrunas <Carl@X930.Tymnet13-may- 
Subject: "forwarded message) From: Wayne"... 
o21986: 
PFORBECANCELLED""...: 

(begin forwarded message)
----------------------------------------------------------------
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 13 May 86 11:09:32 PDT 
To: WRS@C39.Tymnet 
Subject: "FILE BACKUP FOR BE CANCELLED""... 

FILE BACKUP FOR "TO BE CANCELLED" USER NAMES:
IF RESEARCH BY VAL'D'T'S UNCOVERS INFO THAT THEY USE TO DECIDE IF 
SPECIAL BACKUP IS NEEDED, THEY BACKUP USERS'S FILES. OTHERWISE T13-may-o21986

220 X930.Tym". (CARL PROBABLY KNOWS BETTER
THAN VIDA - BUT SHE UNDERSTANDS IT IS DAILY FOR THE LAST WEEK, 
WEEKLY FOR THE LAST MONTH, AND MONTHLY FOR THE LAST 6 MONTHS).
VAL.'S CRITERIA - NOT PRECISE - JUST IF THEY RECOGNIZE AS PROGRAMMER,
OR A WELL USED NAME LIKE ? (CUD1), OR ??
----------------------------------------------------------------
(end forwarded message)

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 13 May 86 17:41:00 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Two meetings this week for owners of Rainbow-100 computers. 

The SILICON VALLEY DIGITAL PC USER'S GROUP meets every 2nd Wednesday.
The SAN FRANCISCO BAY AREA DEC PC USER'S GROUP meets every 3rd Thursday.
Since this month started on a Thursday, the next meetings are consecutive.

Wednesday, May 14th, 7:00pm - At DEC's offices at 2525 Augusting Drive in 
Santa Clara.  Agenda: Select a new steering committee, develop a list of future
topics.  The usual question and answer session will follow, representatives
from DEC are expected to be there.

Thursday, May 15th, 6:30pm - At DEC's offices at 101 California Street, 31st
Floor in San Francisco (1/2 block from BART's Embarcadero Station).
Topics: Programming Languages (comparison of the languages available for the
Rainbow-100, DECmate II, and PRO-350).  Public Domain software will be
available for a slight fee to core the duplication costs.

I am planning to go to both, does anyone else plan to BART up to SF?
		/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 13 May 86 19:19:05 PDT 
To: wrs 
Subject: re: forwarded message. 

Please re-forward message... due to micronode outage -- lossage of
character data -- mail is getting garbled -- all mail === aux-circuits
are losing data ------- /Crl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 13 May 86 19:20:43 PDT 
To: dernst 
Subject: character lossage 

Due to character lossage .... 
   we are losing characters in out TUMS mail messages... please!!!!! can
you look into the problem.... /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 13 May 86 22:07:12 PDT 
To: dernst 
Cc: Joe Smith <jms@X930.Tymnet> 
Subject: tests 

I reloaded 10115, and 62 and I haven't been able to get it to fail.
I also reloaded 930, but not 6657 and haven't been able to get it to fail
that I can see.  I tried old monitors first, then the same ones that were
running... same...

I donno if the /P25 monitors have been patched on dsk, NEWMON.SAV to reset
the tty width paramerter to 80, but in any case... that;'s what I've found.
If it persists, we will contact you again...

Let me know if you find anything that should cause the problem, and/or if
wecan come to some agreement as how to resynch without screwing up our tables
and your internal tables...  what we have already should work... but maybe
it doesn't...

It is possible that all the problems started when 10115 got the BPA's but
I thought it was before that.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 14 May 86 11:08:36 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, DERNST 
Subject: X95-base still running (CURR11)ND6760.BND 

Had to reload 6760 from CURR11 because the code in NEW11 refuses to talk to
the DR that goes to the 2020.

LOADII finally recognizes us, the only license required is RF to read from
CURR11.				/Joe
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 14 May 86 11:11:35 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Bug in F3 microcode. 

I was checking the instructions that DEC defined after the KI was built to see
if the F3 executed these new opcodes the same as the KL and KS.  I haven't gone
thru the entire list, because I was rudely halted in the double-integer codes.

DADD = opcode 114 = add pair of ACs to pair of memory locations and store the
		    71-bit integer result in the two ACs. 
This works on the F3.

DSUB = opcode 115 = subtract pair of memory locations from pair of ACs and
		    store the 71-bit integer result in the two ACs.
This works on F3.

DMUL = opcode 116 = multiply pair of memory locations by pair of ACs and store
		    the quad-word result in four ACs.
Unimplemented on F3 - traps to INSSIM which goes to ILLINS.

DDIV = opcode 117 = divide quad-word number in four ACs by pair of memory
		    locations.  Put quotient in two ACs, reminder in two ACs.
This halts the F3 microcode.

The F3 is halted very dead by opcode 117.  The MI STOP light was on, as were
MI PAR ERROR and MEM PAR ERROR.  (I have hardcopy showing which lights were on
and what instruction I used.)

Since I was not sure that the microcode could be successfully restarted, so
I reloaded the microcode and reloaded 930.  I am sure the same would happen
on 62, where an unprivileged user running DDT can halt the system.

/Joe
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 14 May 86 22:46:29 PDT 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: new uuos 

Look good.  My only comment is really a non-comment.  I don't
know why LOGOUT on STOP would be set if you didn't really want
the child to logout.  I guess there is no harm as long is there
is no way it can ever get to command level.  If it were defined
to be "LOGOUT on COMMAND LEVEL" then I would feel better.  The
parent could still look at it since on a frame stop with an
enabled parent, the frame is left in user mode, not command mode.
The only difference with what you said and this is that if at
any time for any reason, the port went into command mode, it
would log out.  That's basically the objective behind it.  -Bill
Received: from B39.Tymnet by X930.Tymnet; Wed, 14 May 86 22:54:03 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 14 May 86 22:52:22 PDT 
To: Carl A. Baltrunas <Carl@X930.Tymnet>, jms 
Subject: PCOM slowness 

Do you know what causes PCOM to wait long periods after running
some programs.  It seems to have to do with the orange ball.  Do
you know what about a program makes it fast or slow?  It is very
frustrating (VERY).  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 14 May 86 22:51:54 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: Logout on stop 

My understanding is that if the frame stops...
  EXIT,   EXIT 1,  HALT,  ^C,  FLEXIT,  FLSTOP,  .HALT
  address check, ill mem ref, page i-o error, parity error, etc...

if logout on stop is set, it logs out.

In a BATCH/XEXEC environment, if I have a job running a program
and I don't want to baby it, setting logout on stop will make it
go away, (I don't need to determine why it stopped), I'll just
need to know it went away.

Hmmm... you're probably right... shouldn't need to set logout on stop
I need to think about it again... the idea to change PVLOS came about
because if a child stopped, I'd detect it with .IAFEX and then go and
look at all my children to see who stopped.. and of course, the job
that caused the interrupt would be gone (i think, or at least running
LOGOUT!).

If we add .IALCF as proscribed, then setting logout on stop would NOT
be necessary.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 14 May 86 23:35:42 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: PCOM ball logic 

Yes,
  I think it has something to do with the yellow ball logic,
but do not know excatly what causes it.  It seems to have something
to do with how the program exits, releases i/o channels, especially
if it inits a channel for TTY.  Then it has to timeout instead of
just reflecting the yellow ball.

I started to look into it, but never finished... remember the X, XX,
etc programs I showed you once, different exits, releases causing
differnet timing on yellow balls...  

SYSTAT and DIRIT are big offenders of the wrong way to exit! sigh.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 14 May 86 23:40:56 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: HALT AT USER PC 407745 IN LOGON. 

I HAD A LINE IN SWITCH.INI THAT ENDED WITH "/SEL:OTHER;ALWAYS".  THE SEMI
COLON SHOULD HAVE BEEN A COLON.  IS THAT HALT YOURS, OR IS IT IN SCAN
CAUSED BY CALLING .TIAUC AFTER READING AN EOL CHAR WITHOUT STARTING SCAN
ON A NEW LINE?

62 RESYNCED JUST FINE.  1B0+3 IN 210, THEN 3 IN 210.  DID THIS WHILE
10115 WAS BEING LOADED, NO PROBLEMS (MESSAGE "<BEEP>ATTEMPTING TO RESYNC
WITH NETWORK BASE" EVERY 30 SECONDS OR SO ON THE CTY).
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 14 May 86 23:49:13 PDT 
To: jms 
Subject: EVENT stopcode SYNBAS 

I think even the initial synch with the network should appear on the CTY
so that we know it's talking... via an event stopcode... and likewise a
similar event if we lose contact with the base...

then maybe we can forgo the "attempting to resync with network base"
because we will be told of the "positive" evet of synchronization!

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 14 May 86 23:53:05 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: addendum... 

if type 3 is seen, and we know the version, we can report that as part
of the event information!
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 17 May 86 14:16:22 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet>, scapik 
Subject: System 62 died - hardware problems. 

I came in Saturday and found the second F3 halted.  MEM PAR ERROR was on,
MI STOP was on, but MI PAR ERROR was off (this is different from the DDIV
halt).  I could not even start to reload the microcode in 62 because its
tape drive has vacuum problems - it won't load the tape.  62 is sitting in
the same state I found it, the lights still show the microcode status at the
time of the halt
				/Joe
Received: from X95.Tymnet by X930.Tymnet; Sat, 17 May 86 18:00:14 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sat, 17 May 86 17:31:24 PDT 
To: DERNST 
Cc: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: 10115 got a BPA 

I halted 6657 and then typed 1000G to see if 930 could resync with its base.
After 3 minutes it noticed that the base was not talking, and performed the
resync code.  I had no problems logging in to 930 from 6657.  I tried 
logging in from 2346 to JMS (without using :930) to see if it had any problems.
It did.  2346 would not let me talk to TYMNET.  Then I noticed the BPA error
on 10115.  It wasn't there before I halted 6657.  I don't know if going into
ODT on 6657 caused it, or if trying to login from 2346 caused it.

Fatal node error BPA
Registers:
  155100  141500  000001  141500  127062  133700  136076  016014  000344
Stack:
136076: 127410  000000  001230  120210  000004  000000  121032  000004
136116: 117534  000303  000001  076366  000000  127410  127610  076042
136136: 013040
Debug:
  000000  000000  000000  000000  000000  000000  000000  000000
Received: from B39.Tymnet by X930.Tymnet; Sun, 18 May 86 4:00:54 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 8 May 86 12:27:34 PDT 
To: carl@930 
Subject: MAP bug 
Resent-from: William R. Soley <WRS@C39.Tymnet> 
Resent-date: Sun, 18 May 86 3:46:27 PDT 
Resent-to: Carl A. Baltrunas <Carl@X930.Tymnet> 

Ooops!  Looks like a boundary condition error...
Do you have a newer version that fixes this?  Or maybe the source?
-Bill

------------------------------------------------------------------------
!dir (kopfj)ebus.bnd/word
EBUS    BND      63   4-APR-86   31744

!r(spl)map
Map file: (kopfj)ebus.bnd
 (to file) ebus.bnd
(KOPFJ)EBUS.BND ==> EBUS.BND
Map file: ^C

!dir ebus.bnd/word
EBUS    BND      62   4-APR-86   31232

!r ddt
31744.=76000
31232.=75000	^C

!
------------------------------------------------------------------------
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 19 May 86 9:21:38 PDT 
To: Monitor Group <osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Bill Mortanson <helgem@930.Tymnet>,
	Dennis Coffey <dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Schedule.. 

1. I am on call this week 5/19 to 5/25..Pager (415)498-3841
2. I will be spending most of this week at Aubrea/Trainning
   (408)942-5253.
-Osman-
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 19 May 86 17:05:00 UT
From: NTD.J/KOPF@Ontyme.Tymnet 
Date: 19 MAY 86 09:03:35 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: I69265@Ontyme.Tymnet 
Subject: "Carl: 1. There are rumors that"... 

Carl:

1.  There are rumors that all is not yet well with the EBUS code;  crashes
are still happening.  Do you have any specifics I can look at?

2.  Any progress in your group taking over EBUS?

3.  You forwarded the OMTYME about a proposal to support EBUS to another
user (BRING@62);  It could not be delivered, and TUMS returned it to me
rather than to you.

John Kopf
Received: from B39.Tymnet by X930.Tymnet; Mon, 19 May 86 12:06:03 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 19 May 86 12:03:42 PDT 
To: jms@930, carl@930, osman@930 
Subject: LOGINN installation 

I found LOGINN improperly installed on system 65.  It requires RF for
the new version, which it didn't have.  I fixed it, but I don't know
how many other systems are like that.  Maybe the transmittal was wrong?
-Bill
Received: from B39.Tymnet by X930.Tymnet; Mon, 19 May 86 12:13:06 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 19 May 86 12:12:20 PDT 
To: jms@930, carl@930, osman@930 
Subject: LOGINN.SHR 

Found it bad on 58 as well.  All fixed now.  According to
MHX that covers all the systems.  -Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 19 May 86 13:59:41 PDT 
To: dernst 
Cc: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: More info on 6760. 

You were right about the LTC.  When it's on, the PDP-11 does not successfully
load, ODT shows the PC as 001270.  When it's off, both NEW11 and CURR11 load.
But I still have problems (NEW11)ND6760.BND - it won't talk to 95 and it won't
talk to TYMNET even though it says the link is up.

(CURR11)ND6760.BND			(NEW11)ND6760.BND
#C					#C
V=254,0					V=255,1
ST=135264				ST=144756
D0=S,2					D0=S,2
D1=T,24					D1=T,4
D2=L,20,F,S,H,95			D2=W,1
D3=L,30,I,S,G,TYMNET			D3=L,20,F,S,H,95
					D4=L,10,B,P,L,2346
					D5=L,10,I,S,G,TYMNET
P=203,213,221				P=105,102,112
L=5556					L=5657
CIN=0 COUT=0 CPS=0			CIN=2 COUT=1 CPS=0
#L2					#L3
STATUS=UP,117532			STATUS=DN,134660
EC=0,0,0,0,0,0,0,0,0,0,0,0,0		EC=0,0,0,0,0,0,0,0,0,0,0,0,0
#L3					#L4
STATUS=UP,120234			STATUS=DN,135362
EC=0,0,0,0,0,0,0,0,0,0,0,0,0		EC=0,0,0,0,0,0,0,0,0,0,0,0,0
#K					   0,0,0,0,0,400,0,0,0,0,0,0,0
					   0,0,0,0,0,0
					#L5
					STATUS=UP,136102
					EC=0,0,0,0,0,0,0,0,0,0,0,0,0
/Joe					#K
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 19 May 86 16:16:23 PDT 
To: IPC.R/DANIELS@ONTYME, IPC.G/PEREZ@ONTYME, IPC.T/MARCONI@ONTYME 
Cc: jms@X930, carl@x930, osman@x930, fletcherc@X930, dencoff@X930, ring@X930 
Subject: P034/P-10 has been installed in (SYS)NEWMON.SAV on all systems. 

All 10 patches in PMON.PAT have been installed in the P034/P monitor, and
the resulting SAV file has been copied to (SYS)NEWMON on all IPC PDP-10s,
except for system 26 (which needs C26-P034/P19 for the 3650 disks).
Patch #10 is the one which allows WRTSAT, BADSAT, and TTYZNE stopcodes to
be processed without crashing the system.

Please try loading from NEWMON at the next scheduled reload.

/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 20 May 86 23:12:31 PDT 
To: helgem@x930 
Subject: File restore 

Bill,
  I took the liberty, since I was doing it for my directory, to restore
any files that were on the :62 incremental tape to your directory on 930.







It turns out that, it's not good enough.  Tomorrow, you need to take the
all-files tape and get back the sources for NEXTIM that were on 62...
That may be the only thing we need, but I cannot be sure. Set the mode
to MODE OLD NEWEST and that should leave any files on 930 newer than the
tape alone, and only restore files created on 62 after you stopped using
930.  It's really the only way to get all the files back....

/Carl

PS>  I cannot rebuild XEXEC from your sources on 930.  /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 22 May 86 20:17:41 PDT 
To: lindley 
Subject: Who is RMURPHY? 


Received: from C36.Tymnet by X930.Tymnet; Fri, 23 May 86 13:52:19 PDT
Received: from X62.Tymnet by C36.Tymnet; Fri, 23 May 86 13:51:49 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 14 May 86 23:29:44 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: 62 is up 


From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 26 May 86 3:54:11 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: I am on call this week. 

I will be at a science fiction convention Sunday and Monday with my beeper.

26-May-86 03:33/Joe
Contact:  Cupertion Ops, L.T. Smith, (408)446-7705, System 36

Symptom:  PJ not running.
          Tried "TAUX PJ:36", but job logged out.

Solution: Ran PAM under (JMS).  Could not MASQUE as PJ because both (XEXEC)
          and (MPL) were wiped out.

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 26 May 86 9:06:56 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Update to HOTLINE call for system 36. 

26-May-86 08:45/Joe
Symptom:  Operations restored directories MPL and XEXEC and reloaded C36.
          PJ not running again.

Diagnosis: PAM.LOW and PAM.SHR had no license.

Cure:     Set WC,OP,SY,ST,JL,AC,WF.

Received: from B39.Tymnet by X930.Tymnet; Wed, 28 May 86 20:00:44 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 28 May 86 16:46:51 PDT 
To: carl@930, jms@930, osman@930 
Cc: NSS.G/WALKER@Ontyme.Tymnet 
Subject: Monitor on 25 

They (Gary Walker) have asked me to install FREE on 25 and 54 because they
are getting host out of ports.  I did.  But, I notice that on 25, the
monitor is configured for JOBN=127. and RPORTN=100.  This isn't very good.
Since FREE only guarentees 10 free jobs, unless alot of them are detached,
it isn't going to help them any on 25 until they get a new monitor (which
they probably should, anyway).  

[Gary: JOBN is the number of jobs allowed, RPORTN is the number of
real (network) ports allowed.]

-Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 29 May 86 18:29:23 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: Hunter Liggett. 

Jerry McCoy and Ray Donahue found and fixed two hardware problems with the 
Army's KI - one module in the pager and one in the disk controller.  Bart
Wheeler, Ed Kochanowski, and I added patches to PULSAR up thru autopatch tape
7.  This is expected to lessen their problems with the weekly backup hanging
between reel changes.  The Army and PRC seemed pleased with the results.
/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 30 May 86 17:04:36 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, jms,
	helgem, dencoff, bring 
Subject: Monitor/TYMCOM-X meeting 


I'll be in TYMNET TTE.110 class next week but
you are welcome to have a TYMCOM-X meeting anyway.  All the
classrooms are booked for Tuesday so I reserved classroom
2 for 10:00 to 12:00 on Wednesday June 4.

	Craig
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 30 May 86 19:08:00 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Ring 
Subject: Backup of 930 files. 

I did an incremental of 930 today.  It appears that all the correct files
have been restored after the system rebuild, therefor I will do an ALL FILES
Monday, June 2nd.  You might want to TELECOPY some of your files from 62 to
930 so they will get on the ALL FILES save.   /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 1 Jun 86 14:22:19 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Ring, Dennis Ernst <DErnst@C39.Tymnet> 
Subject: Micronet reloaded 

Reloaded 2346 from (DERNST)SV2346.IMG - OK (load device left busy).
Attempted to reload 930's base while in CONSOLE mode using NODLOD V1.6 - failed
  with "Resetting 6657", "Unknown node response code: 0", "[node nak on reset
  pattern]".
Reloaded 6657 from (DERNST)ND6657.BND - OK.
Reloaded 2770 from (DERNST)SV2770.IMG - OK.
Could not attempt to reload 10115 using NODLOD because APA0 HOM page wiped out.
Reloaded 10115 from (DERNST)ND0115.BND - OK.
Reloaded 6760 from (DERNST)ND6760.BND - code loaded OK, would not talk to 95,
  would not talk to TYMNET (link to 2346 still not working yet).
Reloaded 95's base from (CURR11)ND6760.BND - OK.

Miniengine 3146 is having problems, it crashed twice last night and four times
since I got here - NETCON has been notified.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 1 Jun 86 16:11:16 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Ring 
Subject: X62 is down with disk problems. 

Did any of you take my suggestion and copy files from 62 to 930?
It died 31-May at 16:20 with a noname stopcode in PCBIO.  Both HOM pages
on APA0 are unreadable.   /JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 1 Jun 86 16:16:17 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Files from 62 to 930? 

I don't recall making a backup copy to 930, but my main sources were
copied about 6:30pm on friday to 22,54 and one of the systems in fremont.
So, if I list anything, it was only a couple of lines that I "may have" changed.
/carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 1 Jun 86 20:54:44 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Ring 
Subject: C26 crashes. 

Operations tell me that the first crash occurred when they took the ALL FILES
tapes that were written on C70 and mounted them on C26 in order to get a listing
(C70 is bogged down compared to C26).  The other two crashes (at FILAUS, alias
DECUMC+10) occurred when TITO was running "STORE (BILLING10)*.SAT".
It takes 1 hour 15 minutes to do a full DSKCLN on C26. 

I plan to use (CARL)DIRECT with /W/F to get the file names, then run TSTSUP
on each, looking for holes.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 2 Jun 86 0:50:31 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Ring 
Subject: Hotline calls for 1-Jun-86 

1-Jun-86 18:00/Joe
Contact:  Cupertino Ops, David Jorgensen, (408)446-7705, System 26.
3 crashes in the last shift.  1 with no indication, 2 with stopcode FILAUS.
(FILAUS is in FILFND at DECUMC+10)  Checked JOB and $Q+JBTPRG in all crashes.
If the job was running TITO, then the file name and extension can be found in
.CPCAC+15 and .CPCAC+16 (contents of ACs 15 and 16 in AC block 0 on the KL).
Found references to the following files in (BILLING10): 052786.SAT, 052086.SAT,
050286.SAT.  All three files now have the NFS bit set so TITO will not touch
them.
**** not resolved yet ***


1-Jun-86 16:20/Joe
Contact:  Cupertino Ops, David Jorgensen, (408)446-7705, System 36.
Symptom:  Many stopcodes, DSKCLN says problesm in [1,4].UFD.
Resolution: Ran DSKCLN in check mode, 1/4 of all directories hosed.  Called
        in Hardware Maintenance.  The found a bad memory box.  Replaced with
        memory from C24.  DSKCLN ran normal.
Note:     Somebody deleted (MPL) and (XEXEC) again!


1-Jun-86 13:14/Joe  (came in to check on 62)
Symptom:  NONAME stopcode PCBIO+xxxx(xxxx).
        Reloaded from tape - APA0 HOM pages messed up.  X62 still down.

From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 2 Jun 86 0:51:40 PDT 
To: carl, osman, Craig Fletcher <FletcherC@X930.Tymnet> 
Subject: Hotline duty 

Carl is on call now.  I had 13 hours of call-in this week (12.8+13.0=25.8).
This does not include the time Osman had Wednesday night when I was in King
City.			/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 2 Jun 86 18:30:01 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: System 62 status 

System 62 was out of commission due to some scribbling on the disk.

I rewrote APA0's 1st & 2nd HOM pages, copying them from APA1 and
making the appropriate changes.  Then, I re-created a rib page for
(SYS)SAT.SYS and brought the system up to TITO directories (HELGEM)
and (CARL).  The system went through a FULL DKSCLN without any
apparent problems...  If anyone lost files (other than Bill or myself)
I can only offer my sympathy.

Files I created late friday night appeared to be intact, so I doubt
anyone else lost anything.

Dennis Ernst is looking into the micronode problems... please record
any fatal node error messages, and if "1000G" works, or if the node
doesn't crash, please login as "XRAY:ST@" from the node's CTY and do
a "C" command.  Record the number "V=255,2" (for example) with the
fatal error.  At the moment, version 255,2 is the proper new version.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 3 Jun 86 1:09:16 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: System 26 hotline call 

2-Jun-86 23:55 /Carl
Contact:  Cupertino Ops, Tom, (408)446-7705, System 26

Problem:  Two recent crashes @19:11 CRA016 and @21:44 CRA017 showing a
          STOPCD APRFAT, NoMemory MemBusParity... but in CRA017 registers
          at .CPCAC+15,16 contained ONTR03.R00, from directory BETATEST.
          A CHKSUM of the file resulted in a crash.  Told OPS to send mail
          to the user and to have maintenance check out the APRFAT/CPU
          angle with a quick once-over.  File renamed to directory CRASH
          setting NFD.

Solution: Not really solved.. this problem has been cropping up more and
          more lately.


/Carl
Received: from B39.Tymnet by X930.Tymnet; Tue, 3 Jun 86 2:18:49 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 3 Jun 86 2:16:42 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: Access Date bug 

It appears impossible to create, supercede, update or rename a file such
that it has any access date other than the one it had, or todays.  This
is a bug.  The YAMM says you can always set it to a date in the past.  I
want to create files with ancient creation dates so I can tell when they've
been accessed.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 3 Jun 86 11:19:35 PDT 
To: wrs 
Cc: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Access date vs Creation date 

It appears you've got your wires crossed, (or your eyes).  Creation dates
CAN be set to the past without any problems.  Access dates say create and
supersede must be 0 or in the future; update and rename are not clear as
to how they work with non-zero dates... and I thought that that worked if
rename is used.  If it doesn't then, I don't know that it should allow a
person to set access dates to the past.. since the rename is an access and
should update the access date to the present!

/Carl

Ancient creation dates work fine... I guess update & rename only allow the
original or the present access date, so ancient access dates are illegal.
I understand what you want to do, so I sympathize.

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 3 Jun 86 11:51:08 PDT 
To: wrs 
Cc: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>, Dennis
	Coffey <dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: RMURPHY and GAN 3 

My understanding is that RMURPHY is working on the TUMS project... but
I don't know who he is or why he should have a GAN 3 username.  Another
GAN should be setup for this project or he should have a GAN from NTD,
not TYMCOM-X.  I realise that someone just did a GAN change on that user
but GAN 3 should NOT have been used.  Please rectify this, or have Ken
Jones (or whomever is running the show contact me).

/Carl
Received: from B39.Tymnet by X930.Tymnet; Tue, 3 Jun 86 15:31:00 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 3 Jun 86 15:14:15 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Re: Access date vs Creation date 
In-reply-to: your message of Tue, 3 Jun 86 11:19:35 PDT

Ooops, I did say "creation" when I meant "access" in my last sentence.
At any rate, the YAMM says that Create/Supercede can set only current or
future (but why?) and update/Rename can set only current or past.  One
would have to close with option '10 to preserve a date in the past.  At
any rate, I can't set anything but what it was or current using any
combination of lookup/enter/rename.  TITO would want to set it to the
past, as well as other archive like programs.  I am sure it used to
work, but it probably got broken when Ernie added the fast disk clean
stuff.  -Bill
Received: from B39.Tymnet by X930.Tymnet; Tue, 3 Jun 86 15:59:20 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 3 Jun 86 15:25:46 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>, Dennis
	Coffey <dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Re: RMURPHY and GAN 3 
In-reply-to: your message of Tue, 3 Jun 86 11:51:08 PDT

Well, I don't know what's going on with RMURPHY.  I certainly didn't suggest
he be in GAN 3 or even know he was changing GAN for that matter.  In fact I
suggested he not do anything regarding validations and I was just going to
MOVUSR him to the systems he needed (with fixup).  At any rate, they don't
seem to be able to decide what to do:
  11/11/83 originally validate in GAN 11334
  4/11/85 canceled for a day - no other change
  5/3/86 revalidated with GAN 50527
  5/31/86 revalidated with GAN 3
-Bill
Received: from C36.Tymnet by X930.Tymnet; Tue, 3 Jun 86 16:00:47 PDT
Received: from X62.Tymnet by C36.Tymnet; Tue, 3 Jun 86 16:00:33 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 3 Jun 86 12:08:29 PDT 
To: bring 
Subject: Username RING 

Will you please start using RING when you are working on TYMCOM-X
work.  Thank you.  I am going to forward your mail to RING from
now on, so you'll have to login as RING to get it.
/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 4 Jun 86 13:25:20 PDT 
To: Dennis Ernst <DErnst@C39.Tymnet>, Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Ring 
Subject: Resync-with-base code working on F3 

12:59	930 got an info stopcode TTYZNE
13:00	6657 got a Fatal Node Error BFC and restarted
13:02	930 got event stopcode NOBASE (no response from base, PDP-10 OK)
13:03	930 got event stopcode BASEOK (resync with base successful)
13:04	Logged in to 930, able to build AUX circuits without losing characters.

The above events show that something went wrong in 930's base, and sent a
login request on a port that had just been zapped.  930 complained that it did
not have a chance to respond to the zap when the login request came in.  It
output the TTYZNE (Zapper Not Echoed) message and continued.  Then 6657 noticed
the problem, output its BFC message, reset itself, and waited for 930 to resync.
After 90 seconds of no activity in the IRING, 930 output the NOBASE message to
the CTY, which says "No response from base.  Please reload it.  PDP-10 is OK."
930 then reset IRPPDP, ORPPDP, IRP620, ORP620, and went thru its resync code.
This succeeded, producting the message BASEOK.

And they all lived happily ever after.  Yea!
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 4 Jun 86 13:29:43 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: System 62 - Events 

During an attempt to do the all-files on 62, we kept losing vacuum on
the tape drive.  Steve Capik was called but has not yet arrived to date.

Last night, we decided to try the Kennedy 9400 drive that Dan & Eric
had sitting on the cart to see if it really was compatible and whether
the F3 could handle it.  Apparently, under timesharing, the 9400 works
just fine at 1600 or 6250 bpi.

Due to lack of an available working 9100 drive, we decided to see how much
tape an all-files took at 6250 and started the all-files.  It completed
this afternoon, and with the exception of one tape (accidently) written at
1600 bpi, the save took 6.5 tapes.  (5.5 @ 6250, 1 @ 1600).

I believe that the all-files takes less time as well, but we didn't really
perform any accurate timing tests.

/Carl

PS... it would be nice to be able to use a 9400 on one of our systems for
      all of the backups.....
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 4 Jun 86 14:02:26 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Ring 
Subject: Node at Albrae 

The node at Albrae for FSC Training is 6630, Kernel host 3217, TOM host 8119.
The TYM files are in (TYM5:54)ND6630.TYM and (TYM5:33)ND6630.TYM.   /JMS
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 4 Jun 86 16:40:23 PDT 
To: Dennis Ernst <DErnst@C39.Tymnet>, carl 
Subject: 6657 glitched again 

Fatal node error NXM
Registers:
172714  144200  151273  160001  127522  136060  140444  023454  000000
Stack:
140444: 017672 127522 076634 130064 151703 071264 156000 064210
140464: 151700 065764 127522 013104
Debug: 0 0 0 0 0 0 0 0

6657 restarted, 930 sync'ed ok.			/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 5 Jun 86 1:30:05 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Cc: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>, Dennis
	Coffey <dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Weirdies on system 62 

Joe,
    System 62 has some really weird problems with job #3, #5 and #7.  It
is possible that I can replicate the problem even further, but it tears
the system apart with "?error in detached job..." messages to the CTY
and OPR terminals, eating up to 60% of the CPU.  The IRONY is that SYSTAT
doesn't show anything.

  I was about to logoff and ran SY to see things at a glance, and WHOA..
there are 3 jobs, detached, running logout in ^C state.  So I try killing
them first with LOGOUT, then DDT and the HANG uuo, then TAKEIT... no dice
so I go to reload the system...  OOPS!

  Takeit sets the current terminal to OPR, and >Zap< >Zing< >Zowie< there
comes a flood of "?error in detached job..." messages all over the place.
Tons and tons of them... I finally got out of TAKEIT and zapped the OPR
port.  I then attached to the 3 jobs one at a time, couldn't run anything
at all (seems like no context pages? or something?) ...anyway, CORE 0 did
work, and by attaching, the "?error..." messages stopped going to the CTY
and OPR terminals.

  Jobs are still there, detached... I wanted to make sure we could get a
good crash somehow before we reload it.  Even if it means patching a good
copy of BOOTS into the monitor!!!

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 5 Jun 86 19:05:42 PDT 
To: CARL, LINDLEY 
Subject: PPN for LINDLEY 

I ran (CUD10)USERS and it reported a UFD entry without LUD or DUL entry for
[11334,56073].  The directory had files in it created 19-Mar-96, and the
RIB of the UFD said it was LINDLEY.  The LUD says LINDLEY is [50527,2041]
and that UFD had only one file - MAIL.  I renamed the files from
[11334,56073] into (LINDLEY) and deleted [11334,56073].UFD.   /Joe
Received: from C36.Tymnet by X930.Tymnet; Thu, 5 Jun 86 23:00:45 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 5 Jun 86 23:00:20 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Thu, 5 Jun 86 17:38:49 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: re: troubles on X62. 

After asking you about the problems on X62 I read the mail from you about it.
It looks like a possibly nasty problem -- more lthan just a puzzlement, but a
real bug if it finds its way to a production host.

Re. the detached jobs:  I have experienced a few jobs 'disappearing' recently.
They looked like classic Tymnet circuit outages, so I didn't pay MUCH
attention -- I was going through the MUX, and suspected it was a MUX outage.
This suspicion was supported by the fact that I also lost my circuit to the
MUX from my terminal.  Of course by BOSS circuit and all my slave circuits
were also going through 10115.  ONe strangeness I noticed:  in spite of my
priveledge word settings, the jobs I lost were never detached -- as far as I
could tell.

I have been using usernames DENCOFF, SPPOPER and SYSMAINT during the last
couple of days.  If any of the detached frames in never-never land belonged to
any of those usernames, that might explain why the jobs disappeared.
Received: from C36.Tymnet by X930.Tymnet; Fri, 6 Jun 86 12:20:41 PDT
Received: from X62.Tymnet by C36.Tymnet; Fri, 6 Jun 86 12:20:20 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 6 Jun 86 12:19:52 PDT 
To: lindley@x930 
Subject: Thank you very much. 

/Carl
Received: from C36.Tymnet by X930.Tymnet; Fri, 6 Jun 86 13:48:41 PDT
Received: from X62.Tymnet by C36.Tymnet; Fri, 6 Jun 86 13:48:09 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 6 Jun 86 13:42:59 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, LINDLEY, ANEWMAN 
Subject: LINDLEY and ANEWMAN on 62 

I moved Lois' and Adele's files from 930 to 62 after correcting their PPNs.
(LINDLEY)=[50527,402041] and (ANEWMAN)=[50527,402042].
By the way, who is (NEWMAN)=[11447,15776]?  There is a copy of BIONIC there
on 62.   /Joe
Received: from B39.Tymnet by X930.Tymnet; Mon, 9 Jun 86 13:25:33 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 9 Jun 86 12:47:20 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: Re: "SEE ATTACHED BELIEVE MPL IS OPER"... 
In-reply-to: your message of Fri, 6 Jun 86 14:26:26 PDT

Wayne, as you requested, I've deleted oper name MPL for Paula Leahy.
She was in a tree that was chock full of terminated employees so I
deleted them all.  I don't know what the relationship was, am a bit
puzzled.  MPL is a directory belonging to Tymcom-X development, so
its possible that's not her real oper name, or it could be coincidence.
Anyway, deleted are:
	MPL
	DHSMITH
	HINCKLEY
	MCH
These are only deleted from 34 and 39 and associated slaves.  The
foonlys and 2020s are left to those who might care.

-Bill
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 9 Jun 86 16:56:10 PDT 
To: Monitor Group <osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Bill Mortanson <helgem@930.Tymnet>,
	Dennis Coffey <dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: I am on call 5/9 to 5/15..Osman.. 


From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 10 Jun 86 9:22:52 PDT 
To: carl 
Subject: LOAD/MAP TEST.MAC does not work with LINKER v6 

When using LOADER, the above command creates MAP.MAP, when using LINKER,
LINK complains that MAP.REL not found.		/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 10 Jun 86 13:09:18 PDT 
To: TXS.C/BALTRUNAS@ONTYME 
Subject: GETTING A FILE 

:GET * A88843
:TYPE
:SEND TXS.SUP
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 10 Jun 86 13:18:50 PDT
From: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Date: 10 JUN 86 13:13:18 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A88577@Ontyme.Tymnet 
Subject: GETTING A FILE 

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 10 Jun 86 13:09:18 PDT 
To: TXS.C/BALTRUNAS@ONTYME 
Subject: GETTING A FILE 

:GET * A88843
:TYPE
:SEND TXS.SUP
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 11 Jun 86 2:04:01 PDT 
To: Peak Maintainer <Peak@X930.Tymnet> 
Subject: Subject: Mail on 62 

There is an experimental XEXEC running on system 62.

From now until Friday, the normal MAILER/ONTMTP functions will NOT
work.  Thus, mail will not get forwarded from 62 or read by ONTYME
on any regular basis.

I'm off for the weekend, (Wednesday-Thursday)  See you Friday.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 12 Jun 86 21:45:39 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Mondo messages... 

Hmmmm... interesting.  I changed the code to put the byte count in the
left half of the job number, as prescribed by the doctor (you)... I will
check, but please test the changes you made to the .STTLK function to
see if you are indeed handling that, or if still only searching for a 0
byte.  I started to make it put in a zero byte, and decided to put in the
size field instead... less string-space munching... sigh.

see you toimorrow.
/carl
Received: from B39.Tymnet by X930.Tymnet; Thu, 12 Jun 86 22:56:11 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 12 Jun 86 22:52:50 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: SCREAM/STREAM 

I've updated (MPL)STREAM.SAI to work with larger node files.  I reloaded
SCREAM to make use of this and then telecopied it to all systems.  I hope
there is a more formal procedure, but I figured nobody could care since
the old version is useless (unless you delete (TYMNET)NODES.MAP).  -Bill
Received: from B39.Tymnet by X930.Tymnet; Thu, 12 Jun 86 22:56:19 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 12 Jun 86 22:54:30 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: SCREAM 

By the way, thats in (MPL:62) that I made the changes.  -Bill
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 13 Jun 86 11:46:49 PDT 
To: Craig Fletcher <FletcherC@X930.Tymnet> 
Cc: carl, osman 
Subject: I won't be in today. 

I still have this nasty cough and sore throat.  Kaiser recommended Drixorol,
which will keep me from doing much work.  I'll be at home (790-0608) if you
need me.
				/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 15 Jun 86 14:51:44 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet> 
Subject: New CALLI -151 : FINASS 

We've implemented a new uuo, Frame INteruppt ASSignment to allow one to
assign an interrupt cause to another frame.  Primary usage will be to
trap EXIT/LOGOUT in child frame on a frame by frame basis (instead of
.IAFEX, exit in any child).  We will later have .FOCSI : cause software
interrupt in frame with causes .IASIP (software interrupt from parent),
.IASIC (child), .IASIS (sibling), .IASIA (any frame)...

Anyway, I'd like to use this stuff in SAIL and at the moment I don't want
to have to release a new version of SAIL... (at least not yet)...

Do you know if there are *ANY* sail programs using INTDEV(DEV,CAUSE,CHAN)
which executes the INTRMT uuo to assign the interrupt channel.

Documentation says that this is only good for the ACT: device and I suspect
that only CHKPNT.MAC uses this uuo.  Does that sound realistic?  ...or are
there old programs lying around (in sail, or other languages) to attempt to
use the INTRMT uuo?

Do you know of any SAIL programs using INTRMT directly?

--------------

On system 62 in the meantime, I've "patched" SAIL.LOW and SAISGA.SHR to
use "FINASS 1," (calli -151) in the one place that had "INTRMT 1," (calli -52)
in the code.

I will propagate this change throughout the world if you can't come up
with any objections...

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 16 Jun 86 11:50:38 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Ring 
Subject: Standby 

I am on call this week.  (415)790-0608.  /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 16 Jun 86 12:22:23 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>, Lois Frost
	<FSC.L/Frost@Ontyme.Tymnet> 
Subject: Schedule 

I have an eye-doctor appointment at 1:30 and do not expect to be in the
office before 2:30.  See you then.

/Carl
Received: from B39.Tymnet by X930.Tymnet; Mon, 16 Jun 86 20:11:58 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 16 Jun 86 20:09:18 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet> 
Subject: Re: New CALLI -151 : FINASS 
In-reply-to: your message of Sun, 15 Jun 86 14:51:44 PDT

The only use of INTRMT anywhere that I know of is CHKPNT.  I don't understand
why you need to patch SAIL, why not just add a new runtime for the new type
in a library that gets required?  Doesn't it just return a channel number?
-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 17 Jun 86 0:04:33 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: INTRMT etc... 

No, if it were that simple, I'd do it.  It isn't clear where anything is
for SAIL as far as rebuilding it and it would involve rebuilding the runtime
libraries as well as the same modules inside the compiler (although that
probably wouldn't matter).

The way it's setup, INTMAP knows how to put things together for the sail
interrupt handlers.  The call is INTMAP( x,PROC,AOBJN_POINTER ) and the "x"
is put togethere by a macro in (SAILIB)TYMINT.DEF which generates one of
   BYTE (9) cause (9) chan (3) '7 (15) port
   BYTE (9) cause (9) chan (3) '5 (15) device
   BYTE (18) chan          (3) '1 (15) cause

INTMAP looks at bits 18-20 to determine whether to do a TINASS, INTRMT or
INTASS or to even try to do it completely different using special UUOs for
the APRENB system.

INTMAP is the routine which just needs to be modified... to handle another
type of value and fo a FINASS uuo instead of the others.   A quick and
very painless way to implement the code was to patch the existing runtimes
to execute a FINASS instead of an INTRMT and pass it the right BYTE combination
of cause, chan, '5?, frame-descriptor.

It works fine on system 62.  Until a new release of SAIL can be managed with
our lack of personnel power (not to mention that the current project is at the
state it was expected to be in on April 30th, today, a month and 1/2 later)
it seemed like a feasible solution...

As it stands we have to release a new monitor to support it... and that hasn't
been done yet either....

Anyway, thanks... CHKPNT is probably the only program using INTRMT and so
noone will miss the old behavior.  So sorry if I sound grumpy... I've had a
frustrating day trying to get the behavior I want from a few measely UUOs
and I'm very tired and this is all off the top[ of my head.  The format shown
may be incorrect, but you get the general idea.... sigh.

/Carl
Received: from B39.Tymnet by X930.Tymnet; Tue, 17 Jun 86 0:36:00 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 17 Jun 86 0:27:36 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: INTRMT etc... 
In-reply-to: your message of Tue, 17 Jun 86 0:04:33 PDT

Yech.  Oh well.  If you could find a working copy of INTMAP, you could put
a new one in (SAILIB) and as long as it was loaded before the library, all
would be okay.  Wouldn't have to release a new SAIL until you were ready.
I suppose you could also release a routine in that manner that would patch
the INTMAP at runtime.  I just hate to see binaries out there for which there
are no sources.  But then, its a tradeoff no matter how it goes.  

I understand.  This week is getting off to a bad start for me, too.  I just
am feeling overwhelmed by everything.  To really halp Ken Jones out with
TUMS requires much more time than I have right now (just bad timing) and
it all seems like "striving after wind".  Oh well.  

-Bill

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 17 Jun 86 2:20:51 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: .iaFEX .iaECF et al 

Joe,
  It appears that I am able to detect .iaECF at least once by a single
frame, but have not had any luck in getting it to work repetedly.  I
made a small test program which run (JMS)SLEEP.SAV over and over in the
child frame waiting for any of: run bit off in JBTSTS, FEX detected,
ECF detected and LCF detected.  At no time was LCF ever seen although
I did kill the job each time in the test.

  The file (CARL:930)MSG.JMS run under the MUX on X62-P034/P28 shows the
results of the test.  FEX is detected reliably at the points that I want
to be able to detect ECF... ECF is detected once in the appropriate cases
but ONLY once!

  I did a find on JBYEXC and it is tied up with the HIBER UUO in both
PICON and SCNSER.  It is reasonable to assume that maybe ECF should be
enabled in the same way? (using channel 36, ugh) or another bit on the
hiber to detect it, not counting a hiber bit to detect FOCSI et al.

  We need to look at it more closely... because it is NOT reliable.

  Also, *BUG* in code at  INTAD1+some, SETZM JBTFNT##(J) should be a
MOVE T2,[777700000000] ANDM T2,JBTFNT##(J)  or something which doesn't
clear the 2 fields which are set by the parent, not the child...
  I'm sure this is not causing the problem in the test case, but it is
in need of repair.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 17 Jun 86 2:36:41 PDT 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Cheeze - sniff sniff 

Did you happen to remember to get any land-o-lakes cheese while
in Boston?

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 17 Jun 86 23:08:48 PDT 
To: carl, wrs 
Subject: How to indicate "wink wink" in mail messages. 

Return-Path: <@SU-AI.ARPA:TVR@CCRMA>
Received: from SU-AI.ARPA by SUMEX-AIM.ARPA with TCP; Mon 16 Jun 86 03:40:31-PDT
Received: from CCRMA-F4 by SU-AI with PUP; 14-Jun-86 14:03 PDT
Date: 14 Jun 86  1401 PDT
From: Tovar <TVR%CCRMA@SU-AI.ARPA>
To:   SU-BBoards@SU-AI.ARPA
Subject: Those silly ASCII icons on BBoard...
(Courtesy of Symbolics, Palo Alto.   -- TVR)

13-Jun-86  1744	@QUABBIN.SCRC.Symbolics.COM:MLB@RUSSIAN.SPA.Symbolics.COM [straz@MEDIA-LAB.MIT.EDU: (really from Weaver) smiley]
Received: from SU-AI.ARPA by CCRMA with PUP; 13-Jun-86 17:44 PDT
Received: from SCRC-QUABBIN.ARPA by SU-AI.ARPA with TCP; 13 Jun 86  16:31:36 PDT
Received: from RUSSIAN.SPA.Symbolics.COM by QUABBIN.SCRC.Symbolics.COM via CHAOS with CHAOS-MAIL id 8623; Fri 13-Jun-86 19:31:38 EDT
Received: from LOS-TRANCOS.SPA.Symbolics.COM by RUSSIAN.SPA.Symbolics.COM via CHAOS with CHAOS-MAIL id 190804; Fri 13-Jun-86 16:20:42-PDT
Date: Fri, 13 Jun 86 16:23 PDT
From: Marc Le Brun <MLB@RUSSIAN.SPA.Symbolics.COM>
Subject: [straz@MEDIA-LAB.MIT.EDU: (really from Weaver) smiley]
To: joker%ccrma@SU-AI.ARPA
Message-ID: <860613162322.9.MLB@LOS-TRANCOS.SPA.Symbolics.COM>

Received: from ELEPHANT-BUTTE.SCRC.Symbolics.COM by RUSSIAN.SPA.Symbolics.COM via CHAOS with CHAOS-MAIL id 190803; Fri 13-Jun-86 16:13:27-PDT
Received: from STONY-BROOK.SCRC.Symbolics.COM by ELEPHANT-BUTTE.SCRC.Symbolics.COM via CHAOS with CHAOS-MAIL id 21685; Fri 13-Jun-86 18:28:17 EDT
Received: from MEDIA-LAB.MIT.EDU (MEDIA-LAB.ARPA) by STONY-BROOK.SCRC.Symbolics.COM via INTERNET with SMTP id 30878; 13 Jun 86 18:26:47 EDT
Received: by MEDIA-LAB.MIT.EDU (5.31/4.8)  id AA29169; Fri, 13 Jun 86 18:28:30 EDT
Date: Fri, 13 Jun 86 18:28:30 EDT
From: Steve Strassmann <straz@MEDIA-LAB.MIT.EDU>
Message-Id: <8606132228.AA29169@MEDIA-LAB.MIT.EDU>
To: fun@SCRC-STONY-BROOK.ARPA, marg@MEDIA-LAB.MIT.EDU
Subject: (really from Weaver) smiley


Date: Wednesday, 31 August 1983  01:41-EDT
From: Mark Plotnick <MP at MIT-XX>
To:   info-cobol at MIT-MC
Re:   the last whole smiley face catalog :-)

Awhile back, you may remember some discussion about "smiley face
codes".  Well, here are some new ones, culled from netnews
(done@teklabs, rew@hao, ksf@security, msg@houxl, and futrelle@uiucdcs).

:-)       smiley face
;-)       winking smiley face
[:|]      submitter is a robot (or other appropriate AI project)
:>)       submitter has a big nose
:<|       submitter attends an Ivy League school
:%)%      submitter has acne
=:-)      submitter is a hosehead
:-(*)     submitter is getting sick of most recent netnews articles and
		is about to vomit
:-)8      submitter is well dressed
8:-)      submitter is a little girl
:-)-{8    submitter is a big girl
%-)       submitter is cross-eyed
#-)       submitter partied all night
:-*       submitter just ate a sour pickle
-:-)      submitter sports a mohawk and admires Mr. T
:-'|      submitter has a cold
:-)'      submitter tends to drool
':-)      submitter accidentally shaved off one of his eyebrows this morning
8:]       submitter is a gorilla
0-)       submitter wearing scuba mask
P-)       person submitting is getting fresh
|-)       submitter is falling asleep
.-)       submitter has one eye
:=)       submitter has two noses
:-D       submitter talks too much
:-o       submitter is shocked
  ___
 /   \
| RIP |
|_____|   submitter has recently died
From: CM Fletcher <fletcherc@x930.Tymnet> 
Date: Wed, 18 Jun 86 8:43:49 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Cheeze - sniff sniff 
In-reply-to: your message of Tue, 17 Jun 86 2:36:41 PDT

Sorry, but I never had time to look for lAND O LAKES cheese.
By the time I met my minimum family requirements, it was time to head
for the airport.

	Craig
Received: from B39.Tymnet by X930.Tymnet; Wed, 18 Jun 86 14:04:33 PDT
Return-path: <DERNST@B39.Tymnet> 
From: DERNST@B39.Tymnet 
Date: Wed, 18 Jun 86 14:01:22 PDT 
To: jms@x930 
Cc: carl@x930 
Subject: node 6657 

Joe,
  This is probably an old bug.  I will look at the problem when the newest
code is loaded and the problem reoccurs.

Dennis
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 18 Jun 86 17:46:42 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, HelgeM 
Subject: Hotline call - F58. 

18-Jun-86 05:23 /Joe
Contact:  Fremont, Operations, Jim Wallace, (415)498-2588, F58

Problem:  File damage after power outage.  [1,1].UFD read error for RIB.
          Got STOPCD SCHED1(132217) attempting to LOGIN while READONLY.

Solution: Told them to let maintenence check the disk drives and try to come
          up again.  If DSKCLN still has problems, then they will have to
          rebuild from the ASP tapes.

From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 19 Jun 86 5:07:51 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, HelgeM 
Subject: Hotline call for F38 

19-Jun-86 04:42 /Joe
Contact:  Fremont, Operations, Jim Wallace, (415)498-2588, F38

Problem:  Incrementing hard device errors on one pack.  Could not copy pack
          with PAKCOP - too many data check errors on many different spots.
          (Problem appears to have been caused by yesterday's power failure.)
          Copied pack on an IBM 370, the copy causes "device check for job 1"
          when system is brought up.  System runs with old pack, although
          users complain about damaged files.

Solution: Agreed to Jim's suggestion that he come up with old pack until they
          could schedule a system rebuild.

Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 5:28:33 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 19 Jun 86 5:21:46 PDT 
To: CARL, Peak Maintainer <Peak@X930.Tymnet> 
Subject: Validations at work on system 36 

UFD		NAME on UFD	Current PPN for name
11334,1222	RWS		11334,401222
11334,22613	BASELINE	21476,404146
11334,26715	SOURCE		21476,404152
11334,133144	BETATEST	21476,404153
50527,402622	RMURPHY		11334,431676

The program (CUD10)USERS shows that the above UFDs do not have matching LUD
and DUL entries.	/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 19 Jun 86 6:05:31 PDT 
To: carl 
Subject: 62 to 930 mail kludge. 

I re-enabled the kludge that sends mail to 930 via 36 to clear out
(MAIL:62)930.Q.  The correct program will be in place by the time your
read this.  /Joe
Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 6:12:04 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 19 Jun 86 6:10:33 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Mon, 9 Jun 86 12:52:08 PDT 
To: tymops.m/sadler@ontyme 
Cc: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Request for TMCS User Validation 




                                  MEMORANDUM


                                                              TYMCOM-X SYSTEMS

DATE:  9-Jun-86  12:54:00

  TO:  Mike Macek, c/o Mike Sadler

FROM:  Dennis Coffey  (Tymcom-X Systems)

SUBJ:  Request for TMCS User Validation 

------------------------------------------------------------------------------

We, in the Tymcom-X Systems unit, request validation of two usernames for
access to TMCS.  The purpose for this use of TMCS is to monitor the bases of
the hosts for which we develop and maintain the monitors.  The monitors
communicate directly with the bases, so information about these ISIS nodes is
essential.

usernames:	TXSSUP SYSMAINT CARL JMS OSMAN DENCOFF  (The first two are
		'group' usernames, and essential; the others are personal
		usernames for programmers in the unit.)

terminal type:	HP2621 (TYM 444)

license requested:  P20

time zone:	-480 (Pacific)

color:		white



						Thanks,
						Dennis Coffey
Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 6:12:13 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 19 Jun 86 6:10:39 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Tue, 10 Jun 86 12:48:16 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Re: Need Source Code. 

Carl, here's a copy of Bill's msg about TUMS format and migrating users. /D.

(begin forwarded message)
----------------------------------------------------------------
Received: from B39.Tymnet by X62.Tymnet; Mon, 9 Jun 86 17:44:26 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 9 Jun 86 17:25:57 PDT 
To: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Subject: Re: Need Source Code. 
In-reply-to: your message of Mon, 9 Jun 86 15:59:10 PDT

Okay, the format is described in great verbosity in RFC-822, "Standard for
the Format of ARPA Internet Text Messages".  This document is online in
(WRS:39)RFC822.TXT, however, I can say much more briefly, that a message
is simply a sequence of header records in any order followed by a blank
line followed by the body of the message text.  A header record is a field
name followed by a colon and the body of the field.  If its too long to
asthetically fit on one line, it may be continued by indenting the cont-
inuation lines with one or more tabs or spaces.  The headers you are
interested in are:
	FROM: address
	TO: address, address, address
	DATE: day-of-week, dd mmm yy hh:mm:ss zzz
	SUBJECT: whatever
An address should be of the form
	real name <username@host.TYMNET>
or	username@host.TYMNET

To send a message or group of messages, create a file (MAIL)jjjMAI.TMP
where jjj is your job number in decimal with leading zeros.  Put any
number of messages in it seperated by a ^D.  Don't put a CRLF after the
control D or that will qualify as the blank line that seperates the
headers from the body.  After the file is created, run (MAIL)MAILER.

Suggest you try it a few times with the editor and watch the results.

On the other hand, I wonder if it might just be simpler to let people
read all their RPG mail and then switch to TUMS.  For example:

	Change RPG where it outputs "No mail." to instead do a RUN UUO
	to (MAIL)RDMAIL.

This way if there is any unread RPG mail laying around, it will get
read first.  This requires very little coding.  After this has been
out a while, just have the monitor run the mail program directly.

-Bill
----------------------------------------------------------------
(end forwarded message)

Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 6:12:30 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 19 Jun 86 6:10:48 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 11 Jun 86 2:01:38 PDT 
To: Peak Maintainer <Peak@X930.Tymnet> 
Subject: Mail on 62 

There is an experimental XEXEC running on system 62.

From now until Friday, the normal MAILER/ONTMTP functions will NOT
work.  Thus, mail will not get forwarded from 62 or read by ONTYME
on any regular basis.

I'm off for the weekend, (Wednesday-Thursday)  See you Friday.
/Carl
Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 6:12:37 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 19 Jun 86 6:10:54 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Wed, 11 Jun 86 15:26:30 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Tymcom-X Job Processor System test 

Carl, I poked at it (SUBMIT) today.  Found a couple of little bugs, & informed
Bill (Helge) of them.  He was glad to have help in finding bugs.  

I found one bug that is a puzzlement:  only once in several tries could I get
a job into the queue.  The other jobs I tried to enter to the system did not
show up when I gave SUBMIT the "STATUS" command, either with ^D set or without
^D set.

I am not finger-pointing at either of you, I'm just letting you know that
there appears to be some problem.  I showed it to Bill during a short break
from his class, and didn't have time to look at it in any depth, so he didn't
(at the time) know where (which side of the interface between SUBMIT and
XEXEC, or in the interface) the problem was.

I hope it's an easy-to-fix bug.

/D.
Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 6:12:45 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 19 Jun 86 6:11:02 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 12 Jun 86 1:09:56 PDT 
To: CARL 
Subject: Files in (TIIDEV:62) 

FYI: FIND VUE (TIIDEV)GRAB.SAI/V:3
Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 6:13:12 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 19 Jun 86 6:11:23 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 18 Jun 86 11:38:28 PDT 
To: carl, Dennis Ernst <DErnst@C39.Tymnet> 
Subject: 6657 died and went to ODT 

Fatal Node Error TMP
Registers:
  000000  146400  145703  000006  127522  135660  140450  017604  000013
Stack:
140450:  076634  130064  145703  071264  150700  064210  145700  065764
140470:  127522  013104
Debug: 0 0 0 0 0 0 0 0
5
 126034
@

It was sitting there, at the at-sign.  I typed 1000G and it restarted.
/Joe
Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 6:13:20 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 19 Jun 86 6:11:29 PDT
Return-path: <Peak@X930.Tymnet> 
From: Ken Dawson <Peak@X930.Tymnet> 
Date: Wed, 18 Jun 86 14:38:55 PDT 
To: Carl A Baltrunas<Carl@X930.Tymnet> 
Subject: :36's lud? 

    Carl,

    I was running 'findit' on :36 to find files of the description
    'ontm##.*', and the list which issued forth was long and featured
    (different) entries from (BETATEST) at the beginning of the  list and
    towards the end of the list.  Using DIRIT on (BETATEST) causes the
    latter to be displayed.

    /ken
Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 6:13:46 PDT
Received: from X62.Tymnet by C36.Tymnet; Thu, 19 Jun 86 6:11:46 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 19 Jun 86 6:10:11 PDT 
To: carl 
Subject: 62 to 930 kludge 

this is a test
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 19 Jun 86 17:07:12 UT
From: IOD.CUPERTINO@Ontyme.Tymnet 
Date: 19 JUN 86 09:44:42 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.D/COFFEY@Ontyme.Tymnet, FIN.MAIL@Ontyme.Tymnet,
	IPC.B/KEIM@Ontyme.Tymnet, IPC.J/KRIVANEC@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, MDISF.TS@Ontyme.Tymnet,
	MDISF.AF@Ontyme.Tymnet, ISIGB.HQ@Ontyme.Tymnet, MDISF.TS@Ontyme.Tymnet,
	ISIPR.IRVINE@Ontyme.Tymnet, IOD.CUPERTINO@Ontyme.Tymnet,
	TXS.@Ontyme.Tymnet 
Message-id: A93579@Ontyme.Tymnet 
Subject: Shutdown of DEC PDP10 host S90 

           McDonnell Douglas                      IOD.CUPERTINO   VG1-A01
           Information Systems International
                                                  Tel. [-1] (408) 446-6473
 
DATE>      1986-06-19  09:27 PDT                  Telex 4993666 MDISI CPTO
 
TO>        Dennis Coffey (TXS.D/COFFEY)     Dick Kovach (FIN.MAIL)
           Bruce Keim (IPC.B/KEIM)          Jan Krivanec (IPC.J/KRIVANEC)
 
COPIES>    Carl Baltrunas (TXS.C/BALTRUNAS)  J.F. Guillou (MDISF.TS)
           Etienne Bouillot (MDISF.AF)       Ray Southam (ISIGB.HQ)
           Phil Costes (MDISF.TS)
           Don Davidson (ISIPR.IRVINE)
 
FROM>      Ken BeDell (IOD.CUPERTINO)
 
SUBJECT>   Shutdown of DEC PDP10 host S90
 
REFERENCE> Attached Ontyme A93404
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
     Please note the memo from MDISF (former Cegi Tymshare), which follows
     on next page.
 
     This simply means the PDP/KL host S90, located in St.-Cloud, France,
     will be permanently shut-down at the end of this month.
 
     Bruce; I assume you'll remove the PDP-10 software support charges
     from CPIPS, effective July 1st '86 (amount being approximately
     $2,098.00/month); the Fiscal Center is SM, Dept. No. X014.
 
     Please let me know if you have any questions or if others should be
     notified of this change.  With Best Regards, Ken
 
                                        v
                                        |
                                        |
                                        |
                                        |
                                        V

MSG#:A93404
 IN#:  7027
  TO: HQ              
 FOR: CUPERTINO       
FROM: MDISF    TS              
SENT: 19 JUN 86 06:55:12  
READ: 19 JUN 86 07:56:17  
 
 
 
                          M E M O R A N D U M
 
 
                   MCDONNELL DOUGLAS INFORMATION SYSTEMS
 
Saint-Cloud - France                               
Tel. [33] (1) 46.02.70.12                          
 
 
 
DATE>      19 JUN 86  15:48
 
TO>        WHOM IT MAY CONCERN
 
COPIES>    TECHNICAL SERVICES
 
FROM>      PHIL COSTES (MDISF.TS)
 
 
SUBJECT>   SYSTEM S90 SHUTDOWN PERMANENTLY
 
 
-----------------------------------------------------------------------
 
 
SYSTEM S90 WILL BE PERMANENTLY SHUTDOWN ON JUNE 27,1986.
 
PLEASE REVIEW AND TRANSFER ANY NEEDED FILES OR TAPES TO
ANOTHER HOST.
 
IF YOU HAVE ANY QUESTIONS, PLEASE SEND THEM TO ONTYME-ID
MDISF.TS (ATT. PH COSTES)
 
 
THANK YOU.    PHILIPPE COSTES
From: Ken Dawson <Peak@X930.Tymnet> 
Date: Thu, 19 Jun 86 11:15:55 PDT 
To: Carl A Baltrunas<Carl@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Subject: Validations on :36 

    My guess is that "RWS" stands for Ron W.  Saltgaver, and that this
    whole set of problems is due to an unartful backdoor revalidation.  The
    last name listed (RMURPHY), remember, was discovered to have been moved
    into gan 3 sometime earlier, probably by RWS at the behest of Ken
    Jones, and this looks like a followup effort.

    /ken
Received: from C36.Tymnet by X930.Tymnet; Thu, 19 Jun 86 11:27:03 PDT
Return-path: <KDawson@C36.Tymnet> 
From: Ken Dawson <KDawson@C36.Tymnet> 
Date: Thu, 19 Jun 86 11:24:53 PDT 
To: Carl A Baltrunas<Carl@X930.Tymnet> 
Subject: yet another system move 

    for the purposes of your TUMS.INI file, lois and i are being moved to
    system :35 effective saturday.  oh well.

    /ken
Received: from B39.Tymnet by X930.Tymnet; Thu, 19 Jun 86 14:06:03 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 19 Jun 86 13:25:55 PDT 
To: Joe Smith <JMS@X930.Tymnet> 
Cc: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: Re: (MAIL:62)930.SAV has problems. 
In-reply-to: your message of Thu, 19 Jun 86 6:20:17 PDT

The circuit is getting zapped immediately by system 930.  I can reproduce
it with TAUX as follows (the key is that the mail program sends a ^D in
front of the login string in order to get a higher throughput class):

    !taux
    Login: MAIL:930;
    [network passthrough created]
    ...
    220 X930.Tymnet SMTP server v10 ready; Thu, 19 Jun 86 13:32:33 PDT
    
But, with a control-D...
    
    !taux
    Login: ^dMAIL:930;
    [network passthrough created]
    [network passthrough deleted]
    EXIT

I suspect (just guessing) that the uNet is passing the ^d along in the
needle to the host and 930's LOGINN program can't deal with it.  When
a circuit really goes through Tymnet, I think the ^d is stripped.  I'm
not sure of that, so don't do anything on my account.  At any rate, I
guess you could change LOGINN to ignore any ^A, ^B, ^C or ^D it sees
when its collecting the username.  You could also have Dennis check to
see what the uNode is doing and probably fix it.  I could also give
you a version of TYMX that doesn't send a ^d.  Your pick.  -Bill
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 19 Jun 86 15:03:15 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Cc: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: Re: (MAIL:62)930.SAV has problems. 
In-reply-to: your message of Thu, 19 Jun 86 13:25:55 PDT

LOGINN has been modified to ignore control characters (other than CR) when
reading in the username.  This allows ^A,^B,^C,^D,^I,^V gouging characters.
(^I tells supervisor to selected interactive path - land lines instead of
satellite links, and ^V is for high-volume paths - satellite links OK.)

Version 67.4 has been installed on 930, 62 and 95.   /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 19 Jun 86 16:48:30 PDT 
To: carl 
Cc: Osman 
Subject: Re: Suggestion. 

(begin forwarded message)

Received: from B39.Tymnet by X930.Tymnet; Thu, 19 Jun 86 16:42:52 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 19 Jun 86 16:38:27 PDT 
To: Joe Smith <JMS@X930.Tymnet> 
Subject: Re: Suggestion. 
In-reply-to: your message of Thu, 19 Jun 86 15:49:59 PDT

If you'll fix LOGOUT to do POP right (I don't think that was ever done) then
I'll put PUSH in TUMS.  What LOGOUT should do if the command is POP is to
look to see if it has a detached parent.  If so, before doing its normal
thing, it should do a RETACH to put the TTY back on the parent frame.  I
suppose it might actually want to do this in all cases, even if run as LOGOUT.
In fact, I think that's probably wise.  Your opinion?  -Bill

(end forwarded message)

I just checked.  COMCON treats POP exactly like BYE/LOGOUT/KJOB.  LOGOUT.MAC
does not check for POP.  Should we put the check for detached parent in
COMCON, LOGOUT, or both?

As for adding a PUSH command to COMCON, one possibility is to have it run
INITIA in the child frame to initialize it.  As long as LOGINN does not read
SWITCH.INI using INITIA may be preferrable to running LOGINN in the child
frame.  Needs more thought.
					/Joe
Received: from C35.Tymnet by X930.Tymnet; Sat, 21 Jun 86 14:20:19 PDT
Return-path: <Ken@X930.Tymnet> 
From: Ken Dawson <Ken@X930.Tymnet> 
Date: Mon, 26 May 86 23:48:45 PDT 
To: carl 
Subject: meltdown on 36 

both (peak) and (kdawson) have no files on :36. others appear untouched.
do you know anything about this?
/ken
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 21 Jun 86 17:16:20 PDT 
To: Peak Maintainer <Peak@X930.Tymnet> 
Cc: William R Soley <wrs@C39.Tymnet> 
Subject: 36 meltdown? 

No idea... but it is rumored that this week or next week (or so) that
36 is going to move into BUBBnet and it is possible that someone is
purging files again.... sigh.


-------------------------------------
Subject: meltdown on 36 <<- forwarded message

both (peak) and (kdawson) have no files on :36. others appear untouched.
do you know anything about this?
/ken
-------------------------------------

Sigh!.... MDC and it's tiny little company strategy
   and validation services  --- > STRIKE AGAIN < ---

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 21 Jun 86 23:06:32 PDT 
To: dernst, Dennis Ernst <DErnst@C39.Tymnet> 
Subject: Network problems? 

Dennis,
  We've been running for awhile with your new code in node 10115 and
2346, but I don't believe that I've reloaded 6657 or 2770 yet...

Anyway, today while I was printing something on the terminal connected
to 2346 --thru--10115--to--X62-- and was logged in on 10115 to X62, I
observed a strange behavior...  On my terminal on 10115, a lot of the
characters I way typing would automatically <beep> instead of actually
passing the character typed.  It was as if the input character chunks
were full and it couldn't buffer my characters.

This was with only 2 circuits... one typing out to a port on 2346 which
was fine; the other my port on 10115.  I logged out and back in, just as
the listing was finishing, and did not see the problem again.  I did NOT
try printing another file, but I will later and report if it happens
again.

  Until recently, LOGINN did not like ^A-^B-^C-^D-^R characters which I
beleive you just pass on, but Joe fixed LOGINN to ignore them, and I did
login with ^D^Rcarl:62 on the 2346 circuit.  Should this be a problem?

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 22 Jun 86 0:13:01 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, William R Soley <wrs@C39.Tymnet> 
Subject: Frame access rights, WAKE UUO 

I just don't believe this.....

To use the WAKE UUO, one needs to have .ARWAK rights over the other frame.
To have .ARWAK rights, one needs to BE the other frame or have WC license.

..just about every .ARxxx for doing anything to another frame needs to
have WC license.  this makes this UUO just about useless except for having
a process which sleeps waiting for various interrupts and then in the
proper interrupt receipt code, issues a WAKE UUO on itself so the main-line
code can process the interrupt at it's leisure.

I just don't believe this.....

Note to WRS... it's agreed that the FRAMING project was never completely
     implemented.  But, is there anything left of the design document (if
     there ever was a full design) that makes sense out of the various
     access rights as far as actual usage?  Does anything really use the
     created handles, setting various rights that doesn't already have
     lots of license (i.e. WC or RC).

Back to the WAKE.... since this is what started this...  TOPS-10 allows
a HIBER to enable for WAKE by self,same project or any job... I'd think
we should somehow allow that... although I realise we don't have any real
bits left to designate this... sigh!

Note to Joe... Does FRMOP function .FOCSI actually work yet?  I forget
     what you said the last time we talked about this.  It isn't crucial
     at the moment, waiting for the job to wake up (10 secs) won't hurt
     but it would be nice to do it in a more elegant way.  Giving WC to
     the SUBMIT program is the other alternative.... sigh.  Also, what
     access rights did you use for allowing .FOCSI?  Do we need access
     rights that take into account that the calling frame is one of:
	- a parent, - a child, - a sibling,
	- same ppn, - same gan, - same gan + account-supervisor
	- any other (non-family)
	- any other gan,uun
     that is, rights which because of these things, would permit other
     access rights to be enabled automatically.  For example, the parent
     should have certain privileges over any child, subject to variation
     based on JP.EXO (of course).

-- Please comment -- Bill?, Joe?

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 22 Jun 86 3:51:58 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, HelgeM 
Subject: Hotline call for system 36. 

22-Jun-86 03:36 /Joe
Contact:  Cupertino Operations, Vijay, (408)446-7705, C36

Problem:  Head crash.  They were unable to copy-out the old pack and did not
          have enough disks to build a new DSKB on a new set of packs.  They
          wanted permission from Software Support to copy over the existing
          set.  This will destroy any further chances of salvaging data if
          the ASP rebuild fails, but they just do not have any more 3330
          disks.

Solution: Permission granted.  It's nice to see that they asked.

From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 22 Jun 86 6:25:05 PDT 
To: carl, osman 
Subject: SWITCH.INI entries for LOGON on system 39. 

	/sel:BUBB39:Node:2601,2616,2034,2036,2240,2043,2042,2037,2255 -
	/sel:BUBBNET:Node:6024,7344,7345,7346,7347,7350,7351,7353,7354 -

The first line is the node numbers of the Bubbnet side of the gateways; they
are what LOGON will use if you login from TYMNET to host B39.

The second line is the node numbers of the Tymnet side of the gateways; they
are what LOGON will use if you login to a Tymnet host from a terminal on
Bubbnet.                /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 22 Jun 86 6:26:47 PDT 
To: carl 
Subject: The last remaining bit in the HIBER call. 

My copy of the YAMM implies that '4 is not used.  We could use it to mean
"cancel any remaining sleep time when an interrupt occurs".  This would allow
many INTADR interrupt routines to be reduced to two instructions: SETOM and
DISMIS.  The combination of the DISMIS and this new HIBER bit would tell
the monitor to return to HIBER+2 instead of going back to sleep.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 22 Jun 86 7:49:54 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Craig Fletcher
	<fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>, William R
	Soley <wrs@C39.Tymnet> 
Subject: Re: Frame access rights, WAKE UUO 
In-reply-to: your message of Sun, 22 Jun 86 0:13:01 PDT

FRMOP function .FOCSI=44 to Cause Software Interrupt works in P034/P29.
I modified (CARL:62)FRMTST.SAI to match the description in (M33:930)NEWUUO.NFO
and tested .IASIC (Software Interrupt from Child) and .IASIA (Software Interrupt
from Any).  I did not test .IASIP (from Parent) or .IASIS (from Sibling) but
they should work.  

This particular FRMOP function does not require any access rights whatsoever.
All that is required is for the target job to assign the appropriate interrupt
cause using FINASS.  Simpling setting .IASIA tells the Monitor that you are
willing to receive interrupts from anybody (any GAN, any UUN, any or no 
license).  It will even accept Software Interrupt from job 0!

Here is a case where it would be desirable to return from the HIBER uuo
(instead of resuming hibernation) after an INTADR interrupt had been processed.
I'm not sure whether the Monitor cancels the remaining sleep time or not.
I will check into this further.

			/Joe

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 22 Jun 86 18:37:10 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Wake UUO 

According to my empirical testing (putting one job to sleep on the mux and
issuing a wake uuo on a second circuit) SLEEP always go to completion, ...
although I did not check to see what happens to a sleep-time during the
INTADR interrupts.  HIBER will NOT wake up unless you specify that a WAKE
will cause it to awaken, and you do need WC to use the WAKE uuo.

In effect, the frmop .FOCSI is a WAKE interrupt which must be enabled for
(as does HIBER) and requires no license to use.  If the WAKE uuo worked in
this fashion, we wouldn't really need the .FOCSI function.

Along these lines,  we'd like to eventually see a FINASS to allow software
interrupts for SELF, and same gan (with and/or without account supervisor).
Of course, same gan could be seen as sibling, but... not really.
These would be for completeness, but I don't see the need... no more than
for adding .IASIS (sibling).
/Carl
Received: from B39.Tymnet by X930.Tymnet; Mon, 23 Jun 86 13:04:20 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 23 Jun 86 12:45:56 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>, Dennis
	Coffey <dencoff@X930.Tymnet> 
Subject: Re: Frame access rights, WAKE UUO 
In-reply-to: your message of Sun, 22 Jun 86 0:13:01 PDT

Ernie was very conservative with access rights with the intent to relax
most of them when the project was done.  The idea was to keep people
outside the group from writting code taking advantage of things that were
subject to change or were going away.  I don't think there were any
motivational documents ever created.

The WAKE UUO used to work on any frame without any license.  The worst
damage one could do with it is waste resources by looping sending wakes
to everybody.  That's no worse than looping except that other people will
be billed for the TRUs.  However, I think that one needs a mechanism
that allows any unpriv'd program to send a wake signal.  I propose that
WAKE be it.  There also exists CLUB wake which provides excellent
capabilities for cooperating programs.  Remember that one of the most
common uses for WAKE is to signal that you've left something for the
other program in some file or sharred memory.  CLUBS are perfect for that
since you can join the CLUB based on a sharred file/memory page and
this solves the otherwise tedious and nondeterministic problem of
trying to get the frame numbers of the other jobs.  And then there is
the new stuff you're adding.  I think all the bases are pretty well
covered.  Don't overlook CLUBS it is THE way to manage sharred access
to a file or queue!

-Bill
Received: from B39.Tymnet by X930.Tymnet; Mon, 23 Jun 86 13:50:16 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 23 Jun 86 13:23:39 PDT 
To: Joe Smith <JMS@X930.Tymnet> 
Cc: Carl A Baltrunas <Carl@X930.Tymnet>, Monitor Group <CARL@X930>, Osman Guven
	<osman@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>, Dennis
	Coffey <dencoff@X930.Tymnet> 
Subject: Re: Frame access rights, WAKE UUO 
In-reply-to: your message of Sun, 22 Jun 86 7:49:54 PDT

I'm confused about your statement 
>> Here is a case where it would be desirable to return from the HIBER uuo
>> (instead of resuming hibernation) after an INTADR interrupt had been
>> processed.
I think it does, but not by canceling the sleep time.  At any rate, I think
it should be consistent in that HIBER should behave the same for all INTADR
style interrupts.  Please let me know if I'm wrong about current behavoir.
-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 24 Jun 86 12:43:55 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: INTADR & HIBER 

...not sure... but we believe that if you are at a hiber and receive an
INTADR interrupt other than on channel 36, you either continue the HIBER
or restart the HIBER after you issue the DISMIS.  After we check on this
we will let you know for sure.  We're also thinking on using the last bit
'4 to mean do NOT resume or restart HIBER after any INTADR interrupt...
...will let you know what we do.
Thanks for the info about access rights.
/Carl
Received: from B39.Tymnet by X930.Tymnet; Tue, 24 Jun 86 14:19:16 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 24 Jun 86 14:03:17 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: INTADR & HIBER 
In-reply-to: your message of Tue, 24 Jun 86 12:43:55 PDT

Hmmm, that might explain the spastication in PCOM sometimes.  I think
it assumes the HIBER will always exit after any interrupt.  If it only
does for some (i.e. character and not yellow ball) then a race would
account for the odd behavoir (sort of).  -Bill  (anxious to hear what
you find)
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 24 Jun 86 19:16:36 PDT 
To: carl 
Subject: MACRO/LINK 

MACRO version 53B(1244) and LINK version 6(2376) have been restore to
(MACRO53:62).  The three little files in that directory were renamed to
(MACRO53TST).			/Joe\
Received: from X62.Tymnet by X930.Tymnet; Wed, 25 Jun 86 20:28:14 PDT
Return-path: <Peak@X930.Tymnet> 
From: Ken Dawson <Peak@X930.Tymnet> 
Date: Wed, 25 Jun 86 20:27:09 PDT 
To: Carl A Baltrunas<Carl@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Subject: C24 

Where did system 24 come from? How come you can't set license?
/ken
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 26 Jun 86 13:41:22 PDT 
To: Ken Dawson <Peak@X930.Tymnet> 
Cc: Carl A Baltrunas<Carl@X930.Tymnet>, osman 
Subject: Re: C24 
In-reply-to: your message of Wed, 25 Jun 86 20:27:09 PDT

C24 is a KI used for development and testing of the TYMCOM-X monitor and new
base code.  I telecopied the OPER password file from 930 to 24 today.
		/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 26 Jun 86 14:24:39 PDT 
To: dencoff 
Subject: re: $Variables 

1) the $ sys it is a system parameter.  It is illegal to use a $ in any
   other variables in the first position.  I'll have to check to see if
   it may be used as foo$bar, for example.  If the PCOM doc gives a set
   of characters and $ is one of them, then foo$bar is legal.

2) for the :DEFINE x=y  or  :x=y  the delimiters are not needed, but if
   substitution is desired, the delimiters are necessary for any or all
   parameters.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 26 Jun 86 14:29:55 PDT 
To: helgem 
Subject: ILL MEM REFs 

Grrrr.  The problem is that you do not OPEN the queue explicitly, so if
the queue isn't open, you cannot get the defaults from the queue.  Since
I want you to use the defaults from the queue, I guess you need to open
the queue when you startup, and close it when you exit...  Or you can be
a little more gracious and only open it when you need to... for example,
if this is an INSERT, (since on a modify the queue must be open already)
then do the following...

	IF ( InQue )
	 then if not( OpnQue )
	       then begin "cannot open queue"
		print( error-message );
		return;
	       end "cannot open queue";

If the queue gets open, everything is fine... if not, there is a problem
and the error message should indicate this.

/Carl
Received: from B39.Tymnet by X930.Tymnet; Thu, 26 Jun 86 14:44:13 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 26 Jun 86 14:40:55 PDT 
To: carl@930 
Subject: RMurphy validation 

Ron's going to be distributing the latest TUMS version to all systems
this week (better hurry up) and really needs access to 62, 95, 930.  If
I don't hear from you, I'll assume you don't mind and I'll go ahead and
validate RMURPHY on those hosts along with an OPER name.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 26 Jun 86 14:57:01 PDT 
To: wrs 
Cc: jms 
Subject: RMURPHY valid on 930, 62, 95 

HAven't had time to respond...
  no problem, feel free to validate him with movusr, etc...
  as long as he isn't in GAN 3.

I believe that he is still in GAN 3 on a couple of systems
and he should check that himself or with Jan or Landa in
validations so that it gets corrected and he doesn't lose
anything.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 26 Jun 86 14:59:48 PDT 
To: jms 
Subject: beepre false alarm? 

Just got one... was wondering igf it is just mine or all 3?
/Carl
PS I'm ignoring it.
Received: from B39.Tymnet by X930.Tymnet; Thu, 26 Jun 86 15:47:04 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 26 Jun 86 15:41:38 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: jms 
Subject: Re: RMURPHY valid on 930, 62, 95 
In-reply-to: your message of Thu, 26 Jun 86 14:57:01 PDT

RMURPHY now valid on 62, 95, 930.  Oper name is the same.  He is in
GAN 11334.  Thanks -Bill
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 26 Jun 86 20:05:11 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: osman 
Subject: Re: beepre false alarm? 
In-reply-to: your message of Thu, 26 Jun 86 14:59:48 PDT

My beeper went off twice this afternoon, both times it had "---- 1 ----".
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 27 Jun 86 3:33:29 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Doctor's appt 

I have an eye appointment at 11:30 which should take about an hour, so
I do not expect to be in before 1:00 pm.  See you all when I get in.

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 27 Jun 86 21:32:08 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, HelgeM 
Subject: Changed SIMUUF stopcode on F38 from JOB to INFO. 

System 38 experienced many SIMUUF stopcodes today when MIS people GFD'd 
around.  It appears to be whatever program they ran before GFD (or LOGOUT) is
the culprit.  (MAGNUM?)  They have a July 1st deadline have have to have the
system up.  To allow them to stay up, I patched SIMUUF to be an INFO stopcode
that halts the job and lets the system stay up.

	!GET (SYS)SYSTEM
	!DDT
	ERRCON$:   SIMUUF/   XCT USETO0+232
	USETO0+232/   PUSHJ P,DIE
	USETO0+233/   TSZA PG,656546(P2)
	USETO0+234/   SETZ 0(J)   400000+S$INFO,,BUGSTP
	./   SETZ BUGSTP(U)
	^C
	!SAVE (SYS)SYSTEM

I may make this official as patch #11 on all systems after our 1-Jul deadline.
/Joe
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 27 Jun 86 21:58:12 PDT 
To: Joe Smith <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 
Subject: C26 Is up for testing.. 

Starting right now till 18:00 6/28 we have system C26's
3650 hardware for testing. System up as 26 and shut( sc 400000).
I will be in CUPC in the morning..See you there.

Regarding SIMUUF crashes on F38, I thought they were happinning
when system ran out of disk storage. If it is true making it INFO
STOPCD doesn't realy help/solve the problem, unless opeartion gets
some storage back by deleting files(some how)..
-Osman-
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 28 Jun 86 4:09:55 PDT 
To: CARL 
Subject: MONED1 is not working. 

.RUN MONED1
System id [F3]: 62
[default version from P034.MEM]
Version [P034/P30]:

?GETMAP error for file (M33)SYSID.DAT: -3
Called from 20560  Last SAIL call at 2142
EXIT

At this point, CNFN62.MAC has been truncated.
Received: from X62.Tymnet by X930.Tymnet; Tue, 1 Jul 86 10:04:21 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 1 Jul 86 10:01:18 PDT 
To: Jan Krivanec <IPC.J/Krivanec@Ontyme.Tymnet>, Landa Morris
	<IPC.L/Morris@Ontyme.Tymnet> 
Subject: Name change for username RING 

Jan or Landa,

  I would like username RING, ppn 3,231227 changed to TXSDOC in the
mud and on all PDP-10s.  Please let me know if this can be done.
There is no hurry, just let me know when you get to it.  Thanks.

-Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 1 Jul 86 12:56:37 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, HelgeM, WRS 
Subject: TAUX should not use / or # as a switch character. 

I found some interesting facts when looking to see what characters can be
used to specify optional switches to programs that parse usernames.  Page 6
of the NETVAL ERS says usernames can be up to 20 characters, they can contain
the letters A-Z, digits 0-9, " ", "#", "&", "*", "$", ",", ".", "/".  Note
that slash and poundsign are part of the username, a comma is legal but not
recommended (in case the user wants to build a circuit from X.25 to TYMNET).

It also says that the dash "-" is ignored on aux circuits.  If you try to
build an aux circuit to "LINK-10", the sup ignores the hyphen, looks up the
name "LINK10", and completes the circuit.  If the username has a hyphen in
it, (such as "NTD-SECURITY"), then NOBODY can build an aux circuit to that
name (the sup says that "NTDSECURITY" is not in the MUD).

This is an interesting way to build usernames that not even AC license can
crack.					/JMS
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 2 Jul 86 8:50:15 PDT 
To: jms, dencoff, Carl A Baltrunas <carl@X930.Tymnet>, Bill Mortanson
	<helgem@930.Tymnet> 
Subject: I am in BLDG C today.. 

I will be in BLDG C today with Rhett to show him BASE diagnostic,
and Testing the monitor.
-Osman-
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 2 Jul 86 16:51:31 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, HelgeM 
Subject: Two new patches for the P034/P monitor. 

 "\Patch #11 RESET.PAT 1-Jul-86 JMS \
 "\Make RESET uuo release all SIMIO per-process pages\
 "\This is expected to solve the SIMUUF(FCR.E+3) crashes\
IOCSS:
RELEA5+5/JRST .+2
IOALL+5/JFCL
IOALL+4/MOVEI W,MAXCHN
COMMON:
PATMAP[Q+000100000000

 "\Patch #12 ZAPGOB.PAT 2-Jul-86 JMS \
 "\EBUS ONLY - Do NOT apply this patch to any 2020 monitors\
 "\Cancel the 'need to send gobbler' flag when sending ZAP to TYMNET\
 "\This should stop the KEY=354 base crashes on D65\
SCNSER:
NOTLIN+10/SETZB T1,LDBOUT(U)
 "\Ignore bad port number on message type 3, ignore message type 3\
 "\This will allow John Kopf's new base code to work with the P monitor\
BADPRT+2/CAIN P1,3
BADPRT+3/JRST CNTPRT
TYPTRN+3/AOJA P4,SCNIN1
COMMON:
PATMAP[Q+000040000000
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 3 Jul 86 17:03:50 UT
From: IPC.L/MORRIS@Ontyme.Tymnet 
Date: 03 JUL 86 09:38:22 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A01185@Ontyme.Tymnet 
Subject: USERNAME CHANGE 

     ATTN:  CARL

     FROM:  LANDA

     SUBJ:  USERNAME CHANGE


     HI!  USERNAME RING HAS BEEN CHANGED TO TXSDOC ON ALL SYSTEMS AS
REQUESTED IN YOUR ONTYME #A99564.


THANK YOU,
LANDA/RPM
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 19 Jun 86 17:08:59 UT
From: MDISF.TS@Ontyme.Tymnet 
Date: 19 JUN 86 14:55:12 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A93404@Ontyme.Tymnet 
Subject: SYSTEM S90 SHUTDOWN PERMANENTLY 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Thu, 3 Jul 86 12:23:36 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 

                          M E M O R A N D U M


                   MCDONNELL DOUGLAS INFORMATION SYSTEMS

Saint-Cloud - France                               
Tel. [33] (1) 46.02.70.12                          



DATE>      19 JUN 86  15:48

TO>        WHOM IT MAY CONCERN

COPIES>    TECHNICAL SERVICES

FROM>      PHIL COSTES (MDISF.TS)


SUBJECT>   SYSTEM S90 SHUTDOWN PERMANENTLY


-----------------------------------------------------------------------


SYSTEM S90 WILL BE PERMANENTLY SHUTDOWN ON JUNE 27,1986.

PLEASE REVIEW AND TRANSFER ANY NEEDED FILES OR TAPES TO
ANOTHER HOST.

IF YOU HAVE ANY QUESTIONS, PLEASE SEND THEM TO ONTYME-ID
MDISF.TS (ATT. PH COSTES)


THANK YOU.    PHILIPPE COSTES
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 30 Jun 86 21:10:04 UT
From: ISGHQ.K/PETERS@Ontyme.Tymnet 
Date: 30 JUN 86 17:31:04 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J58206@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS CORPORATION"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Thu, 3 Jul 86 13:30:17 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 


                   MCDONNELL DOUGLAS CORPORATION

                      ADMINISTRATIVE BULLETIN


No. MDC-86-47                                      30 June 1986


To:           ISG A-E, CO A-D, MDC A-C

Subject:      Promotion of Jerry Causley to Information Systems
              Group Executive Officer

                                                                   


1.  As of today Jerry Causley is promoted to the position of 
    Information Systems Group Executive Officer, reporting to me.  
    The ISG Organization structure will otherwise remain as it is, 
    except that Jerry Causley will also retain his position as 
    President, McDonnell Douglas Information Systems 
    International.

2.  Bob Fischer will become the Chairman of the Information 
    Systems Group, reporting to me.  In that role Bob will 
    continue to develop ISG's strategies and will be the focal 
    point for corporate issues affecting ISG, including continuing 
    as a member of the HQ Council.  Also, he will provide 
    leadership in top level sales and marketing activities, and he 
    will continue on the boards of directors of Sanus Health 
    Corporation and the National Electronic Information 
    Corporation.

3.  Jerry Causley has been the head of MDISI since January 1985.  
    He joined CMC in England in January 1970 as a district sales 
    manager.  CMC was acquired by Microdata in 1977.  After a 
    series of pomotions Jerry was named Managing Director of 
    Microdata Information Systems Limited in 1980, responsible for 
    all Microdata sales and service activities in the U.K., Middle 
    East, and Africa.  Early in 1984 he became Senior Vice 
    President of Microdata Corporation with responsibility for all 
    of Europe.  In his positions of increasing responsibility, 
    Jerry has built strong teams and achieved outstanding results.

4.  I believe Jerry will be able to provide strong leadership to 
    turn ISG's strategic vision into reality.  Please give him 
    your full cooperation.



John F. McDonnell
President
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 1 Jul 86 21:07:38 UT
From: ISGHQ.K/PETERS@Ontyme.Tymnet 
Date: 01 JUL 86 16:59:27 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J58647@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS INFORMATION"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Thu, 3 Jul 86 13:30:24 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 


            MCDONNELL DOUGLAS INFORMATION SYSTEMS GROUP

                      ADMINISTRATIVE BULLETIN


No. ISG-86-66                                      30 June 1986


To:           ISG A-E, MDC A-C

Subject:      EXECUTIVE VICE PRESIDENT OF MDISI

                                                                   


I am very pleased to announce the appointment of John Thornber as 
Executive Vice President, McDonnell Douglas Information Systems 
International.  John will be responsbile for the day-to-day 
operational control of MDISI.  He will report to me as I will 
continue to act as President of MDISI, as well as Group Executive 
Officer of ISG.

During the first six months of 1986, MDISI has continued to grow 
in revenue and profits but still has a major task during the 
second half of this year to achieve plan.

The Sales and Marketing direction of MDISI is well under control, 
thanks to the strengths of the Regional Managers and the Business 
Planning Groups.  I therefore have every confidence that the 
strong management team reporting to John will ensure that MDISI 
has yet another year of outstanding performance.

I know you will all join me in supporting John and wishing him 
every success in his new role.



Jerry Causley
Group Executive Officer
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 3 Jul 86 22:29:50 PDT 
To: OSMAN 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff, HelgeM 
Subject: P034/P31 

The /P31 monitor has been telecopied to 26(KL), 34(KI), 95(KS), and 62(F3).
It is running in all of our machines (as NEWMON) and should be tested on
26 and 24.

Have a happy 4th  -  /Joe
From: Osman Guven <osman@X930.Tymnet> 
Date: Sat, 5 Jul 86 1:25:11 PDT 
To: Joe Smith <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 
Subject: 26XP31 and P034/P-12.. 

o  All the systems patched (SYS)NEWMON to become P034/P-12.

o  System C26 is up with 26XP31 w/o any problem so far.

o  I was going to test the new monitor on C24 also but,
   Ampex disk/controller was disconnected to check the
   new SA10/3674.

-Osman-
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 9 Jul 86 15:35:47 PDT 
To: carl 
Subject: Total of 8941 pages in accounting files 

(SYS)####86.DAT - 3097 pages in 165 files.
(UN1)####86.SAT - 5844 pages in 135 files (missing some from 04 and 05).

It's been 6 months since the last quarterly report and purge.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 9 Jul 86 21:59:02 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Update to RESET.PAT and /P-12 

We may be on the right track, but Roy Roskilly's problem showed that the
SIMUUF crashes haven't been completely fixed.  The JRST .+3 was not in error.

The code at RELEA5 uses the UXCTBU macro, which I confused with the XCTBU
macro.  The latter generates one word of in-line code, but UXCTBU produces
	PUSHJ P,DOXCT
	XCT N,[INSTRUCTION]

DOXCT sets the "previous context user" flag (PC.UIO) if necessary so that
the INSTRUCTION refers to the user's address space, even for nested MUUOs.

I have changed the instruction at RELEA5+5 from JRST RELEA5+7 back to its
original JRST RELEA5+10, but left the other two word in RESET.PAT.
NEWMON and all running monitors have been repatched, Roskilly's test on
D56 works now.

Back to the drawing board.		/Joe
Received: from B39.Tymnet by X930.Tymnet; Thu, 10 Jul 86 3:08:35 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 10 Jul 86 2:38:31 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: UUOCON$: CONGMS: date/time stuff 

Updating the clock to track the supervisor is a good idea, particularly
since the SUPs will be running with WWV receivers in the near future.
However, I suggest changing the algorithm to something like this:

int ticsError, THSDAT, TIME;

comgms( newDate, newTime )
int newDate, newTime;
{
	ticsError = subtractDates( newDate, newTime, THSDAT, TIME );
}

and then in CLOCK1 where real time is kept at interrupt level...

ch1tic()
{
	own int oddTic;
	oddTic = ! oddTic;
	if( oddTic && ticsError < 0 ) {
		/* skip a tic */
		++ticsError;
	}
	else if( ticsError > 0 ) {
		/* double tic */
		--ticsError;
		doTic();
		doTic();
	}
	else {
		/* normal tic */
		doTic();
	}
}

This will be much nicer to programs that expect realtime to an increasing
function, like CHKPNT and RPG.  CHKPNT might recover okay, but MIS may be
confused by the lack of serialization in the log.  RPG won't exactly break,
but it may neglect to recompile source code for which the object is really
obsolete causing programmers enourmous confusion, etc.  Anyway, the above
algorithm is basically what we're using in the SUP to update the internal
clock when we get updates from WWV.  It simply goes half as fast or twice
as fast until it gets caught up.

There are other programs that count on monotonic increasing real time, but
all the ones I can think of are only interested in relative time measurement
and use UPTIME so they don't have to deal with wrap.  Sadly, reading UPTIME
requires SY license.  That's probably overly conservative.

-Bill
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 10 Jul 86 15:47:36 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Cc: (Monitor People) Carl@930, Osman@930 
Subject: Re: UUOCON$: CONGMS: date/time stuff 
In-reply-to: your message of Thu, 10 Jul 86 2:38:31 PDT

I like the way you presented your suggestion.  It has been implemented in
the /P32 sources.  Event TIMEOK will output to the CTY when the time is
changed, and another message will be output when the adjustment is complete.
/Joe
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 10 Jul 86 16:17:13 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: I will be in BLDG C tomorrow.. 


From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 10 Jul 86 18:01:14 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Can't run TAKEIT if the disk is full. 

Bonnie at Fremont Ops had a problem trying to run TAKEIT on F30.  It complained
about being unable to create 1TAKEIT.TMP (she was job 1).  Dennis ran TAKEIT
for her.

I GFD'd to OPER and ran (SYS)QUOLST, which reported that the OPER directory
had exceeded its logged-in quota by 125 pages.  The biggest file there was
FTA1 and it had 24936 pages.  I deleted this file at Bonnie's request.

In this case, TAKEIT should have output the message "Cannot create temporary
file because directory (OPER) is full".  Or suggest that the user GFD to a
different directory and try again.
						/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 11 Jul 86 10:29:23 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Today 

I will be in late today - I will be in Santa Clara picking up information
about Intel's 80386 microprocessor.	/Joe
From: CARL@X930.Tymnet 
Date: Fri, 11 Jul 86 13:11:55 PDT 
To: Hans J. Kintsch <FSC.H/Kintsch@Ontyme.Tymnet> 
Subject: "Re: Ontyme usernames Hans, I have"... 

Re: Ontyme usernames

Hans,
  I have been assigned the username FSC.C/Baltrunas, and my only usage is
for communication with support people in groups outside of FSC that do not
know of my other Ontyme account, and for "group" messages within FSC.

  My other account is TXS.C/Baltrunas from which I do all of my normal
correspondence with people outside of FSC.  I also am the account supervisor
for the entire TXS account.  This account is a small account which is used
solely for communication regarding support of the TYMCOM-X Operating systems
and their utilities, and as such is NOT appropriate to be merged into the
FSC account.  We deal with software issues and not hardware.

  My title is Sr. Systems Programmer (or was), my department is 90842 if
you want the MicroData Payroll department (4011 if you want the Tymshare
- Tymnet cost-code number).

Justification for use:  FSC.C/Baltrunas  - FSC communication,
  TXS.SUP & TXS.C/Baltrunas  - intercompany communication and software
                               support of the PDP-10 (TYMCOM-X) systems.


PS:  Sorry for the delay in this response, obviously these accounts have
     not already been removed... we get pretty busy.

/Carl Baltrunas
Tymcom-X Operating Systems
McDonnell Douglas
Field Service Company
From: CARL@X930.Tymnet 
Date: Fri, 11 Jul 86 17:16:26 PDT 
To: wrs@B39 
Subject: "Re: Grumble Blind I realize that"... 

Re: Grumble *^%!@*$ Blind &%^&*(

I realize that you went and upgraded XEXEC on a few systems.... apparently
so that SENDMA would work everywhere (?)...  anyway, in the process you had
to add "1,MAIL,(MAIL)MAILER" to (SYS)XEXEC.CTL...  sigh.

On system 28, the old version WAS running, and there WAS a set of 2 programs
setup to run continuously (or each hour) using the old format.  When you put
up the new version and added your 1 line to the control file, it might have
beeen nice if you had changed the 2-4 lines already existing...

"0,TCSUP,RUN ALTDET"  to   "0,TCSUP,(TCSUP)ALTDET"   like a good little hacker!

I only noticed, 'cause I got a call that ALTDET wasn't running and that they
had a job called "RUN" in  ^C  state instead.  sigh!

/slightly disgruntled
Received: from B39.Tymnet by X930.Tymnet; Fri, 11 Jul 86 18:09:45 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 11 Jul 86 17:21:07 PDT 
To: CARL@X930.Tymnet 
Subject: Re: "Re: Grumble Blind I realize that"... 
In-reply-to: your message of Fri, 11 Jul 86 17:16:26 PDT

Sorry, that wasn't me, but its my fault because I didn't warn him.  Ron
did it and logged in as me without my permission because he had TRU control
problems.  Anyway.  If you have any other XEXEC stuff running, you should
check it.  Or let me know what systems and I'll check it.  I thought I was
the only one who used it - I don't remember giving it to anyone else to put
anywhere, 28 must have been runing a bootleg copy.  -Bill
Received: from X62.Tymnet by X930.Tymnet; Fri, 11 Jul 86 22:29:56 PDT
Return-path: <JMS@X62.Tymnet> 
From: JMS@X62.Tymnet 
Date: Tue, 8 Jul 86 1:40:41 PDT 
To: carl, osman, fletcherc@x930 
Subject: This week 

I am on call this week.
Received: from X62.Tymnet by X930.Tymnet; Fri, 11 Jul 86 22:30:05 PDT
Return-path: <Peak@X930.Tymnet> 
From: Ken Dawson <Peak@X930.Tymnet> 
Date: Fri, 11 Jul 86 22:24:00 PDT 
To: Carl A Baltrunas<Carl@X930.Tymnet> 
Subject: wipe/yank problem 

    It seemed to be a tiny problem in setting my "left in core" count which
    was hangining me up.  Instead of being set to "max chars" when a chunk
    is read into memory, "left in core" was being set to the entire kill
    buffer size.  This had disasterous consequences.
    The fix was small, and seemingly quite effective.

     More later.

     ./ken
Received: from X62.Tymnet by X930.Tymnet; Sat, 12 Jul 86 20:28:25 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sat, 12 Jul 86 20:16:20 PDT 
To: wrs@b39 
Subject: "Bill, I've recently run into an"... 

Bill,
  I've recently run into an interesting problem using interrupts in SAIL
and I'd like to know if you've run into anything similar...

  I'm using immediate interrupts, all of which simply set a flag and then
return, except for 1 routine in the processing half of the 2 programs. In
the scheduling half it doesn't exist, and that half is where I'm seeing the
problem.

  1)  SAIL does a lot of  SUB P,[const,,const]  JRST @const(P)  where const
      is usually 2 or 3.  What I'm doing at the time of the problem is a
      call to CHNIOR/CHNIOV that under normal conditions works just fine.

  2)  Under interrupt conditions, this is speculation, if the code is caught
      between the SUB and JRST instruction and an interrupt occurs and it
      puts anything on the stack (at all) it may overwrite what is already
      there and the JRST @const(P) could very well go into never-never land.

  3)  I think that this is happenning!  Anyway, I'm trying to narrow it down
      and will let you know more later...

PS: an aside, are we going to see "~?" command come back to this SENDMA
    program, so the vanilla user can enjoy the comforts of being able to
    see what he/she has and possibly correct it  (please copy to Ron)
/Carl
Received: from X62.Tymnet by X930.Tymnet; Sat, 12 Jul 86 21:05:16 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sat, 12 Jul 86 21:01:56 PDT 
To: wrs@b39, jms@x930 
Subject: "Bill, Hmmm. Sail does take great"... 

Bill,
  Hmmm.  Sail does take great care not to clobber my stack P, so i do not
think that the SUB/JRST combination I mentioned earlier is the problem...
howerver, it pretty reliably dies in the same place, ill mem ref -- P is
clobbrred pretty bad, last remnants of the ACS appear to be returning from
CHNIOR/V but nothing clear (since SAIL cycles it's ACs as it does things).
More later...

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 13 Jul 86 1:01:07 PDT 
To: carl 
Cc: osman 
Subject: (M33)SYSID.TXT 

I combined the info from CNFN##.MAC into SYSID.TXT.  It would be nice if
MONED1 would append to that file.		/Joe
From: CARL@X930.Tymnet 
Date: Sun, 13 Jul 86 1:23:52 PDT 
To: jms 
Subject: "sysid.txt seems like a reasonable"... 

sysid.txt seems like a reasonable thing... I'd expect that if you look
at the configuration files in OSP and OSB you will pick up large chunks
of missing numbers, and the stray one or 2 that you don't see.

/Carl
From: CARL@X930.Tymnet 
Date: Sun, 13 Jul 86 17:22:08 PDT 
To: osman, jms 
Subject: "Re: SYSID.TXT I needed a break, "... 

Re: SYSID.TXT

I needed a break, so I merged (OSB)CNFN##.MAC into (M33)SYSID.TXT today.
It's possible that there are some other files lying around which have
the "missing" system id's in them, (e.g. cnfn??.old, etc.) but I didn't
go looking too hard.

I also left the "      " blank Id's in the file, feel free to delete them
if we're planning on starting with M00001.  I thought the history might
be reasonable.

/Carl
From: CARL@X930.Tymnet 
Date: Sun, 13 Jul 86 20:08:41 PDT 
To: osman, jms 
Subject: "Re: SYSID.TXT I Also added those"... 

Re: SYSID.TXT
I Also added those items from (OSARC)CNFN##.* to SYSID.TXT.
/Carl
Received: from B36.Tymnet by X930.Tymnet; Mon, 14 Jul 86 11:00:49 PDT
Return-path: <KJJ@B36.Tymnet> 
From: KJJ@B36.Tymnet 
Date: Mon, 14 Jul 86 10:08:15 PDT 
To: carl@x930 
Subject: French Hosts and Your Hosts 

Carl, I believe that you have talked to Bill Soley so you know that Ron
Murphy and myself are conducting a pilot project to determine the
feasability of turning TUMS into a product offerring.  During this 
period we have assumed the responsibility ofr distributing  new 
versions of TUMS.
 
1. Can you tell me if new versions are to be distributed to the French
   hosts and if so, then how is that distribution to be coordinated?
 
2. Are the new versions to be distributed to the hosts that you are 
   responsible for and are there any special coordination requirements?
 
I have found that you are difficult to reach on the telephone.  You can 
call me at X6324.
 
Thanks, cheers, Ken Jones.



Received: from X62.Tymnet by X930.Tymnet; Mon, 14 Jul 86 14:36:16 PDT
Received: from B39.Tymnet by X62.Tymnet; Mon, 14 Jul 86 14:35:02 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 14 Jul 86 14:00:49 PDT 
To: CARL@X62.Tymnet 
Subject: Re: "Bill, I've recently run into an"... 
In-reply-to: your message of Sat, 12 Jul 86 20:16:20 PDT

SAIL sets up a special stack for immediate interrupts.  It has one stack
for all interrupts and supposedly ensures they are mutually exclusive by
disabling the system while any are pending, thus making priority flat.
It doesn't sound like the access to deallocated stack is the problem.
If all you do is set a flag (a global boolean not in a record) then that
can't be it.  What do you do in the one non-trivial routine you mentioned?

-Bill
Received: from B39.Tymnet by X930.Tymnet; Mon, 14 Jul 86 15:02:11 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 14 Jul 86 14:41:34 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: SCHED1.PAT 

What procedure do you want me to follow for putting SCHED1.PAT into 39.
It keeps disappearing (why?).  Anyway, I think you ought to make the
change in the source code.  It makes a very dramatic difference in
performance of interactive jobs.  I can usually tell when it has gone
away just by response time.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 15 Jul 86 0:30:55 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: READ FRMO JOE 

If run by another program, say one called "READ", I'd say...
	READ FROM JOE
this runs program READ which is designed to run (MAIL)TUMS
TUMS sees the line "READ FROM JOE".
The first token IS a legal command, so it dispatches to READroutine
which then processes "FROM JOE".

This works the same as if I had said "TUMS<CR>" followed by "READ FROM JOE".
The only exception is that it was really run from EXEC level.
----------------
Hmmmm... Yes, READ only returning one message is a problem.  Actually I
find it annoying that it only returns one message.  I seem to anticipate
a whole slew of messages and only get one, to which I then continue to
read by typing ^J or ^K  (next and next-delete).  It wouldn't be so bad
if READ did read all of them... RDMAIL reads all of them!  The current
functionality of READ is to PERUSE the mail anyway, not to just READ it.
There is no way to currently issue one command (say from a command file)
which will READ ALL UNREAD and actually get all of the mail.

Maybe a new modifier ONCE or ONE could be added and if set, will only
print the first message {yuk} "READ ONCE ALL UNREAD"  or better yet,
"READ ALL UNREAD PAUSE" will pause after each one (back to command mode),
allowing commands like NEXT/PREVIOUS/etc to work on CSET like READ works
now.
-----------------------
PS. Better to make sweeping changes like this now, before it goes to the
whole world.  sigh.   /Carl
Received: from B39.Tymnet by X930.Tymnet; Tue, 15 Jul 86 1:47:33 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Jul 86 1:38:29 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: READ FRMO JOE 
In-reply-to: your message of Tue, 15 Jul 86 0:30:55 PDT

TYPE will read all the messages at once, like RDMAIL does.  Its arguments
are identical to READ.  If you prefer it, you can do a
	SET MACRO READ "TYPE NEW UNREAD"
or whatever your fancy.  I would like to add a message at the prompt that
says how many unread messages from the read-set have yet to be displayed.
So even if you are reading already read messages, like READ FROM CARL, it
would remind you how many you have to go in some fashion like:
	[3 more msgs]
	>next
	...
	[2 more msgs]
	>
etc.

I like your suggestion and will plan on implementing it in the near
future.  Thanks

-Bill
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 15 Jul 86 22:29:51 PDT 
To: wrs 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Scheduler patch to make interactive jobs run better. 

I renamed (M33:39)SCHED1.PAT to SCHED1.WRS and telecopied SCHED1.PAT and
SCHED1.RUN to all systems.  The RUN file changes QQYTTY from 6 ticks to 30
ticks (1/10 second to 1/2 second) on the fly, the PAT file changes where
QQYTTY is initialized by ONCE code.

Now that this is an official patch (#13, and in the sources), it should
not get lost anymore.  It does make a noticable difference on 930 when
trying to run an editor while MACRO is building a monitor.
/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 16 Jul 86 6:12:21 PDT 
To: scapik 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: System 62 down - serious memory parity error. 

X62 halted with MEM PAR ERR on.  Could not load monitor from tape.
Ran MEMTST - it calls out address 33164 - losing bits.
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 16 Jul 86 8:37:27 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: I am working in BLDG C today.. 


From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 18 Jul 86 11:21:24 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: I have a doctor's appointment at 3:15 today. 

I have an eye appointment at 3:15 today.  So, at least until after the
appointment I shall be working from home.  If there is anything you need
from me, feel free to call me.  (Since Joe is out, and Osman is in CUPC,
unless either of you need me for something in particular, I shall work
the entire day from home.)

/Carl
From: CARL@X930.Tymnet 
Date: Fri, 18 Jul 86 19:33:33 PDT 
To: kdawson@c35 
Subject: "The gizmo that forwards mail is"... 

The gizmo that forwards mail is sort of under development by R Murphy,
but that shouldn't be your problem...

XEXEC (Bill Soley's XEXEC) runs the mailer periodically and that is
more or less disabled, functionally or actually depending on the way
my version of XEXEC is running that day.

/Carl
From: CARL@X930.Tymnet 
Date: Sat, 19 Jul 86 14:21:07 PDT 
To: ken Dawson <KDawson@C35.Tymnet> 
Subject: "Ken, The Gizmo on 930 for collecting"... 

Ken,
  The Gizmo on 930 for collecting ONTYMEs was not running because RMURPHY
went around and installed WRS's newest XEXEC everywhere and didn't fixup the
control files to do the right things for the new format.  I fixed the CTL
file on 930... so let me know if the outage continues.


/Carl


Actually, let RMURPHY and KJJ know!
Received: from X62.Tymnet by X930.Tymnet; Sat, 19 Jul 86 16:56:42 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 19 Jul 86 16:53:59 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: ILL MEM REF/ILL INST found! 

I've found the problem.  It was a typo in a subroutine that did an OPEN
with the open block [ 0, 'DSK', '17 ]  instead of  [ '17, 'DSK', 0 ]!!!

More news at 11.
/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 21 Jul 86 23:11:34 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Craig
	Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: Re: ILL MEM REF/ILL INST found! 
In-reply-to: your message of Sat, 19 Jul 86 16:53:59 PDT

The Monitor should never allow a nonzero address for the 3-word buffer header
that is less than 140.  Wiping out the AC and proceeding is a bug.  /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 22 Jul 86 0:42:27 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Unit types in BPXKON and CPXKON. 

Each disk driver can return 2 bits of unit type to the monitor, allowing
a single driver to handle up to 4 different types if disks.  The 2 bits are
set by the capacity test, which is called from ONCDSK via PUSHJ P,@KONCPY(J).
The capacity test is a routine named BPXCPY in both BPXKON and CPXKON.

BPXKON does one test - it attempts to seek to cylinder 500.  If it gets an
error, it know it is talking to a 3670 (alias 3330 mod I) which as 440 cylinders
and holds 100 megabytes.  (We no longer have any disks of this type, although
the code is still there, and in PAKCOP.)  If the seek to cylinder 500 succeeds,
BPXKON assumes that it is talking the a 3675 (alias 3330 mod II, or Ampex 830).
This 200 megabyte disk has 880 cylinders, and is on all our systems except for
C26.  BPXKON knows that these devices have 19 heads, with 18 blocks per track.

When I created CPXKON from BPXKON, I modified the first test and added a second.
It now attempts to seek to cylinder 600.  If this succeeds, CPXKON assumes it
is talking to a 3675 (3330-II) 200 megabyte disk.  If it fails, CPXKON assumes
it is talking to a 3650, 300 megabyte disk.  Note that the 100 megabyte disk
is not supported.  The second test is to read the COUNT field of record 1 on
cylinder 0 track 0.  If it says the record consists of 576 bytes, it is 
formatted in blocks of 128 words each.  If it says the record consists of 2304
bytes, then it is formatted in pages of 512 words each.  Any other value 
indicates an improperly formatted disk.

As currenlty implemented, the 4 types of disks that CPXKON recognizes are
3675 in blocks, 3675 in pages, 3650 in blocks, and 3650 in pages.  (3650 in
blocks is a combination we do not want to support.)  By adding another test
to CPXKON (a seek to cylinder 900), then it could recognize a 3652 which 1110
cylinders (but is otherwise identical to the 3650).  That would make the 4
types of disks supported be 3675 in blocks, 3675 in pages, 3650 in pages, and
3652 in pages.

At this point, there is some question as to whether the 3652 has a native mode.
The Memorex manual "3674 Storage Control Unit Theory of Operations Manual"
3674.21-02 (PN 804332) says that the 3652 can emulate two 3650 volumes, or two
3675 volumes.  This may mean that the 3652 does not have a native mode, but
the manual does not explicitly state so.  All it has is a note, "For an
explanation of 3652 or 3644 emulation modes consult the appropriate Theory of
Operations Manual (3644.21 or 3652.21)."  We need to obtain this manual or
ask a Memorex representative for clarification.

/Joe
Received: from X62.Tymnet by X930.Tymnet; Tue, 22 Jul 86 5:00:11 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Mon, 21 Jul 86 8:52:30 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: ILL MEM REF/ILL INST found! 
In-reply-to: your message of Sat, 19 Jul 86 16:53:59 PDT

Hallelujah!!  Hallelujah!!  Hallelujah!!  

That bug was a termite -- buried deep, far from the symptom -- but you found
and exterminated it.

Congratulations!

/D.
Received: from X62.Tymnet by X930.Tymnet; Tue, 22 Jul 86 5:00:15 PDT
Return-path: <Peak@X930.Tymnet> 
From: Ken Dawson <Peak@X930.Tymnet> 
Date: Mon, 21 Jul 86 14:13:04 PDT 
To: Carl A Baltrunas<Carl@X930.Tymnet> 
Subject: ontyme message pickup and delivery 

    Carl,

    Has anyone been hosing with node code surrounding :930?  The last
    couple of ontyme messages which the appropriate TUMS object has
    forwarded to me have experienced MASSIVE data loss in the process.
    It's curious:  after I sent you mail about my loss of forwarding, and
    after said was restorted, I received 14 messages, most of which were
    intact.  The later ones (2, I think) in the 14-message burst of
    transmission lost data, however.  I just received 1 message in a single
    burst which was so badly mangled that I had to actually log into OnTyme
    to read it again.

    Anyway, you get the picture.  I suspect the base code because this has
    happened before and been the fault of some form of engine-resident
    code, rather than application code (to wit:  vis-a-vis:  see also:  the
    proper TUMS object).  Tell me otherwise, if such is the case.

    /Ken
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 22 Jul 86 9:11:10 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: I am on call this week.. 


From: CARL@X930.Tymnet 
Date: Tue, 22 Jul 86 10:16:24 PDT 
To: peak 
Subject: "Ken, I just got your mail along"... 

Ken,
 I just got your mail along with a ton of other mail... on 930.  Again,
I have not seen any data lossage recently on 930.  So, I suspect the
prolem is not in the micro-node code... more later.
I am sick today, not at the office... flu symptoms... sigh.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 22 Jul 86 17:58:39 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Suggestions... 

If we can keep a counter of how many DDBs we have created, added to the DDB
chains, then if we search more than that many times, we've looped somewhere!

Why not do once a minute code while catching up on time?  Unless it only
checks against uptime counters instead of actual clock time...  it could
confuse people who do DAYTIME commands...

(in particular, inactivity timeouts)


What happens to characters other than ^Z typed on the CTY in NETWORK mode?


What is th actual format of the SYSUDT variable, NSWTBL+5... is it the UDT
the system came up or the TDT?   KSYS is still ticks to system reload, right?

Can I assume that /P32 works?

I have a message from Peter (Stratman?) from CEGI asking about upgrading
their systems to /P (actually whatever is needed for XEXEC) and about some
testing of XEXEC on their systems.  I think that we need to clarify our
support picture with CEGI (or whatever they're now called) before we take
any action.

/Carl

PS:  I'm still not feeling too well, but will try to do some work while I'm
     away.  /Carl
Received: from X62.Tymnet by X930.Tymnet; Tue, 22 Jul 86 22:26:17 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Tue, 22 Jul 86 8:53:01 PDT 
To: (Monitor Group:), Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven
	<OSMAN@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Subject: Incremental Backups. 

Help!  What tape contains the last incremental backup for X62?  The last
incremental I could find logged was dated 6-6-86.  (This was in the 3-ring
binder on the tape rack in the big lab room.)

I need to restore a recent copy of my TUMS.MSG file, which I've accidently
deleted from my directory.


Thanks!
D.
Received: from X62.Tymnet by X930.Tymnet; Tue, 22 Jul 86 22:31:31 PDT
Return-path: <JMS@X62.Tymnet> 
From: JMS@X62.Tymnet 
Date: Tue, 22 Jul 86 22:29:17 PDT 
To: carl 
Subject: Embedded control characters in CTL files. 

PCOM handles (JMS)MID95.CTL just fine, including the imbedded ^D character.
EXECX does not appear to do this right.  I tried to test :SEND CHARACTER in
MID930.CTL, but the job is stuck.		/Joe
Received: from B39.Tymnet by X930.Tymnet; Wed, 23 Jul 86 12:18:40 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Jul 86 12:00:35 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: Deployment of FREE Program on PDP10s 

(begin forwarded message)
----------------------------------------------------------------
Received: from C26.Tymnet by B39.Tymnet; Mon, 21 Jul 86 19:00:42 PDT
Received: from EMSNTD.Ontyme.Tymnet by C26.Tymnet; Mon, 21 Jul 86 23:06:50 UT
Return-path: <TYMOPS.V/STAFFORD@Ontyme.Tymnet> 
From: TYMOPS.V/STAFFORD@Ontyme.Tymnet 
Date: 21 JUL 86 15:09:58 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: M09402@Ontyme.Tymnet 
Subject: Deployment of FREE Program on PDP10s 

                 MCDONNELL DOUGLAS NETWORK SYSTEMS COMPANY

                     T Y M N E T    O P E R A T I O N S

                            M E M O R A N D U M

DATE>      21 JUL 86  15:08

TO>        William Soley

COPIES>    Gary Perez
           Steve Metcalf
           Ed Barens 
           Wayne Bartlett

FROM>      Vida Stafford


SUBJECT>   Deployment of FREE Program on PDP10s


-----------------------------------------------------------------------



It seems that some users log into systems and do not log off after completing
their work. This results in a heavily loaded system that needs to be 'policed'
on a regular basis.

There is a software program called 'free' that checks usernames on a regular
basis and logs off any users that are inactive and/or are in a Control C
state.

I am requesting that this program be loaded on the following Tymnet Dedicated
Systems:     Private Net Code Genning   Public Net Code Genning
               22,37,26,31                  25,70,33,54
The above systems are in a critical need of this program.


I appreciate any help you can give on this.
----------------------------------------------------------------
(end forwarded message)

Is your group interested in taking over the FREE program?  It seems to be
gaining popularity.  Its already running on a couple of these systems, but
will have to be setup on the others.  I could set it up, then turn it over
or turn it over and let you set it up, or keep it -- your preference.  -Bill
From: FLETCHERC@X930.Tymnet 
Date: Wed, 23 Jul 86 13:33:41 PDT 
To: carl 
Subject: "I need your pass at performance"... 


I need your pass at performance review by Friday so we can meet on Tuesday
July 29 and "do" the review.  OK?

	Craig
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 23 Jul 86 21:41:01 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Micronet back up. 

930, 62, and 95 all running P034/P32.  6657 and 10115 reloaded from DERNST:57.
Incremental save of 930, 62, and 95 completed on tape INC-23.

Monitor sources have been telecopied to M33:26 (total of 2689 pages in 338
files) so that we won't be wiped out if the Liberty Street office is blown
up.		/JMS
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 24 Jul 86 21:08:39 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: /P33 is up 

P034/P33 is running on 930, 62, 95.   It has been telecopied to 24 and 26.
KIXP33.SAV is ready to be tested with the 3650/52 disks in Dallas.  /Joe
From: CARL@X930.Tymnet 
Date: Fri, 25 Jul 86 13:17:47 PDT 
To: fletcherc 
Subject: "Re: My pass on annual review..."... 

Re:  My pass on annual review...

Craig,
  I am still not feeling well, and will NOT be in today (Friday), so it
will be impossible to give you anything by today.  I will try to come in
(at least on Sunday, when I do the incremental) and work on the review
so you will have the first pass to go over on Monday morning.

  I've pretty much been resting, sleeping or running a small fever since
Monday evening and have not been getting any work done.  I will give you
a recount of sick vs. work hours for the payroll records on monday (since
I did take my weekend on Wednesday & Thursday even thought I was sick).

  Leave me mail if you have any questions... I am going back to sleep for
now.  /Carl
From: CARL@X930.Tymnet 
Date: Fri, 25 Jul 86 13:25:39 PDT 
To: dencoff 
Subject: "Dennis, check with Osman or Joe, "... 

Dennis,
  check with Osman or Joe,  but we have been backing up systems 930 & 62
on the same tapes.... the tape log is in the old co-op room.   /Carl
Received: from X62.Tymnet by X930.Tymnet; Fri, 25 Jul 86 13:59:08 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Fri, 25 Jul 86 13:53:52 PDT 
To: CARL@X930.Tymnet 
Subject: Re: "Dennis, check with Osman or Joe, "... 
In-reply-to: your message of Fri, 25 Jul 86 13:25:39 PDT

----------------------------------------------------------------
Received: from X930.Tymnet by X62.Tymnet; Fri, 25 Jul 86 13:27:07 PDT
Return-path: <CARL@X930.Tymnet> 
From: CARL@X930.Tymnet 
Date: F,1486 13:25:39 PDT 
To: dencoff 
Subject: "Dennis, check with Osman or Joe, "... 

Dennis,
  check with Osman or Joe,  but we have been backing up systems 930 & 62
on the same tapes.... the tape log is in the old co-op room.   /Carl
----------------------------------------------------------------
thanks!  /D.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 25 Jul 86 14:33:46 PDT 
To: Dennis Ernst <DErnst@C39.Tymnet> 
Cc: carl 
Subject: Losing characters on aux circuits. 

Any clue as to what would cause 6657 garble characters on aux circuits?
It usually goes away if we reload the host after any sort of "Fatal Node Error",
but not always.  That is, it is usually visible only after the F3 has gone
thru its resync code without being completely reloaded, but sometimes the
problem is stubborn and stays even though 930 was reloaded.

I don't understand why the problem affects outgoing aux circuits, and not
incoming aux circuits or terminal initiated logins.

/Joe
From: Ken Dawson <Peak@X930.Tymnet> 
Date: Fri, 25 Jul 86 22:58:38 PDT 
To: Carl A Baltrunas<Carl@X930.Tymnet> 
Subject: mystery file 

    On (PEAK:62), there is a file Y which I seem to have authored, but do
    not remember creating.  It is a cornucopia of stuff, some text (mail in
    fact), a sail program, some binary stuff (a SAV or REL file perhaps?),
    much of it Lois's and all of it mixed unmercifully.  It looks like a
    disk page salad, and that's why I thought you might be interested in
    it. Let me know when I can delete.

    /ken

    ps: hope you are feeling better.
Received: from X62.Tymnet by X930.Tymnet; Sat, 26 Jul 86 18:43:42 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Fri, 25 Jul 86 15:07:10 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Job Processor Sys's Documentation. 

Carl,

In my role as the virtual Bruce Ring, I need to know how the Job processor
handles certain situations:

1.)  User has 
SMTP server v
21 reater to the the Job Processor System (the
action to be taken with the log file text) to APPEND, and has set the DISPOSE
parameter to the the Job Processor System (the statement of specifications
determining the disposition of the log file) to DELETE.  What happens to the
pre-existing log file info when the job completes normally?  Does the system
delete the whole log file, including the pre-existing info, or does it delete
only the new i
SMTP serve current execution of the job?

2.)  Does LOGLIMIT apply to APPENDed log files in toto, or only to the
appended portion, or does it apply at all?

3.)  Does RESTART work?  How is a job restarted?  When is a job restarted?

4.)  Is any MAILER implemented?  Joe has found that a job he entered got hung
starting up the mailer, with no mailer started and therefore no execution of
the job.

5.)  Should unimplemented commands and parameters, like SPOOL
SMTP server v
21 rea on-line documentation, or should the on-line documentation be made
unavailable for now?  The following are what I currently believe to be
unimplimented:
	Parameters:
		SPOOL
		UNIQUE
	
	Commands:
		CODE


Most of the rest of my questions are more appropriately directed at Helge,
since they pertain to his program.  I would like to meet with you to discuss
the meaning and naming of some of the commands, parameters, etc.

thx!
D.
Received: from X62.Tymnet by X930.Tymnet; Sat, 26 Jul 86 18:43:50 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sat, 26 Jul 86 18:40:37 PDT 
To: dernst@b39, jms@x930 
Subject: "Re: We are experiencing a lot"... 

Re: We are experiencing a lot of character lossage on the micro-net

In many cases where AUX circuits initiated from hosts on the micronet
are attempting to transfer data to other hosts, we are seeing a lot of
character lossage.  MAIL messages in particular 
S getting garbled all
over the place.  This message DOES NOT contain any typos!

Could this be a result of the new micronode code?

/Carl
Received: from X62.Tymnet by X930.Tymnet; Sun, 27 Jul 86 5:00:40 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sun, 27 Jul 86 5:00:43 PDT 
To: dencoff@x62 
Subject: "Dennis, your mail message was"... 
DaSun, 27Jul864: 55:53 PDT

Dennis,
  your mail message was too garbled to make any sense of...  I will be
glad to meet with you on Monday, assuming that I feel well enough to
stay the entire day. I am still not feeling well.  sigh...

  much of the intended set of features is somewhat implemented, but
everything is not...  MAIL is not implemented at all.  I actually
removed the mailer code... more on that later.   I think there are a
few misconceptions, and what might be best is for me to present an
overview of how everything works and fits together, including log
files and mail systems in two parts:  1) the intended version, 2) the
current implementation.

  When would be a good time on Monday?

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 27 Jul 86 18:43:57 PDT 
To: dernst@b39 
Subject: node 6760 BRZ error 

reg: 0 0 143552 153213 137126 143552 145606 0 5
stk: 145606:  -nothing-
deb: all zeroes

From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 27 Jul 86 21:57:47 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: SZRPT.MAC is a mess. 

The author of SZRPT really screwed up.  Look at the following:
	T1=6
	T2=7
	LS=12
	T3=13
	54=54
	UFDBF=12
	MFDBF=13
...
	ARRAY UFDBF[3],MFDBF[3]
...
	CALLI
	INIT UF,17
	SIXBIT /DSK/
	UFDBF
	  JRST ...
...
GTUFD1:	MOVE T1,UFDBLK(UFDBF)
...
The coding is atrocious.  It's a wonder it works at all.  (In fact, it doesn't
even wait for an answer when it asks for "sort by").  It looks very much as if
the program was written before the CHANIO function that reads the UFD.  Then
someone added the .CHUFD call, which made the 3-word buffer header for reading
the UFD in buffered mode obsolete, so that person decided to reuse the name
UFDBF, but did not purge all the previous references to the symbol.

As currently coded, SZRPT attempts to use two overlaping buffer headers in the
ACs, which causes it to die with "?Address check for device at user PC 174"
when running under the /P33 monitor.  Since this program is so blatently wrong,
I will put a new version of SZRPT on SYS, and not retract the edit to the
Monitor that caught this problem.   /Joe
Received: from X62.Tymnet by X930.Tymnet; Mon, 28 Jul 86 8:58:02 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Mon, 28 Jul 86 8:49:13 PDT 
To: CAR 
Subject: "would be a good time?" As soon"... 
>.<CRLF>: 
ageSubject: Re: "Dennis, your mail message was"...
In-reply-to: your message of Sun, 27 Jul 86 4:55:53 PDT

"What would be a good time?"  As soon as you're ready to meet.  I'm at the
office and ready.  /D.
Received: from B39.Tymnet by X930.Tymnet; Mon, 28 Jul 86 16:43:29 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 28 Jul 86 16:34:22 PDT 
To: Wayne D. Bartlett <BARTLETW@B39.Tymnet>, JMARCIN@B36, (Monitor People)
	Carl@930, JMS@930, Osman@930 
Subject: CUD fn 31 and PASSWOrd v42 

I have tested the new CUD function 31 which verifies a user's current
password.  It seems to work correctly.  I have changed the new
PASSWORD program creating version 42 in (WRS:62)PASSWO.*.  The new
version begins by asking the user for his current password.  This
is to prevent an unauthorized user from walking up to a terminal and
changing passwords.  This new version should be transmitted as soon
as it is QA'd.

I have also noticed thatmost PDP-10 systems are still running version
20 of the PASSWORD program.  Why is this so?

-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 28 Jul 86 23:50:18 PDT 
To: William R Soley <wrs@C39.Tymnet>, Monitor Group <CARL@X930>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bartletw@B39 
Subject: Password version 20 

It was transmitted... it think there was a breakdown in the transmittal
procedure and never made it to the rest of the world.

/Carl
Received: from X62.Tymnet by X930.Tymnet; Tue, 29 Jul 86 9:31:41 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Tue, 29 Jul 86 9:29:04 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Password version 20 
In-reply-to: your message of Mon, 28 Jul 86 23:50:18 PDT

Carl,
Is this something I need to do something about, or to know about?
/D.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 29 Jul 86 15:37:31 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: P034/P33-1 has hang fixed. 

My fault.  I started to re-arrange the dispatch table for the base-to-host
messages, got part way thru, then changed my mind.  I undid the changes except
for the case when we get a message type 5 = New Supervisor has taken over the
node.  The <AOJA P4,SCNIN1> was missing, causing SCNSER to POPJ without
advancing the IRING pointer, therefore it saw the same message over and over
and did not do anything about it.  24XP33 and 26XP33 have been patched on
M33 on their respective systems.  The /P33-1 monitor will not have this
particular hang problem anymore.		/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 30 Jul 86 5:21:11 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: I'm on call this week. C26 hotline call. 

30-Jul-86 05:14/Joe
Contact:  Cupertino Operation, (408)446-7705, Tom, System C26.

Reason:   System 26 was left running the P034/P33 monitor with instructions to
          call the hotline if the machine crashed (to get a dump if the "hang"
          problem occurred.  The hang has been fixed, but SIMUUF hasn't.  Told
          them to reload from SYSTEM.SAV, no further notification necessary.

From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 30 Jul 86 5:33:53 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Correction to previous message 

30-Jul-86 05:14/Joe
Contact:  Cupertino Operation, (408)446-7705, Tom, System C26.

Reason:   The plans were to leave 26 running P034/P33 monitor with instructions
          to call the hotline if the machine crashed (to get a dump if the
          "hang" problem occurred).  The hang has been fixed, but the notice to
          call the hotline was not cancelled.  Anyway, they got a SIMUUF
          stopcode while running the /P19-4 monitor, which means that RESET.PAT
          has not solved the problem.  Told them to reload from SYSTEM.SAV,
          no further notification necessary.  26XP33 still not tested?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 30 Jul 86 13:49:08 PDT 
To: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Still not feeling too well 

Craig, (Dennis)
  I'm still not feeling too well, so I expect to only be in for the
afternoon.  See you in a while.  /Carl
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 9 Jul 86 1:07:06 UT
From: OTSUPA.ONTYME@Ontyme.Tymnet 
Date: 08 JUL 86 22:21:05 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A02933@Ontyme.Tymnet 
Subject: OnTyme Version 22 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 1 Aug 86 15:47:20 PDT 
Resent-to: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@C26.Tymnet>, Joe Smith <jms@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 

                      APPLIED COMMUNICATIONS SYSTEMS
 
                           M E M O R A N D U M
 
 
                                                      []  ISG     
 
DATE>      08 JUL 86  13:47

TO>        OnTyme Account Supervisors

COPIES>    

FROM>      Bill Bartlett
           OnTyme Marketing Support Manager


SUBJECT>   OnTyme Version 22


-----------------------------------------------------------------------


    Applied Communications Systems (ACS) is pleased to announce that
    version 22 of OnTyme will be deployed for customers beginning in
    August!  The following features and enhancements have been added.
 
    SUBJECT LINE.  On a :SEND or :REPLY the system will prompt for
    subject.  The subject text supplied by the sender will appear in
    the message header and all the message lists (:IN, :OUT, old,
    long, short, etc.).
 
    X.ONTYME.  This feature is an interface with which applications,
    PC "front ends", or intelligent terminal procedures may interact
    with OnTyme in a consistent, reliable, binary, machine-oriented
    format.
    
    PATHWAY.  This feature allows the user to log into any network
    host during an OnTyme session, capture data from the target host
    into the OnTyme workspace, transfer data between OnTyme and the
    network host, and execute commands on the host. The target host
    may reside in either the same network as OnTyme or in another
    network accessible through a gateway.
 
    XMODEM.  This feature is used for binary file transfer using the
    XMODEM protocol.
 
    SYSTEM GENERATED MESSAGES.  Version 22 of OnTyme will put a
    message in the :IN list of the sender regarding invalid recipients
    so they will no longer have to look through their :OUT (OLD) lists.
 
    RESERVED CHARACTERS. To facilitate future enhancements, six
    characters (!,@,#,$,%,&) will no longer be allowed in file names.
    These will be used later as 'wildcard' characters for displaying
    information with the :LIST command.
 
    :REPLY (cc)(msg#).  This command will send the contents of the
    workspace to the original recipients of msg#.
 
                                                                Page  2

    COMMAND SCANNER. The OnTyme code has been improved and made more
    efficient.  Many system messages have been grammatically improved
    and made more consistent.
 
    Version 22 is available on the Marketing test host so that you can
    become familiar with the new product features. To request a
    temporary id on the test host, send a request to ACS.ONTYME.
 
    More complete documentation is being written and included in user
    manuals, technical manuals, and *** files in OnTyme as
    appropriate.  Definitions of the enhancements in version 22 of
    OnTyme may be found in the public file *** ONTYME.NEWS. Please
    distribute this announcement within your organization.  
 
 
    Version 22 WARNING!
    -------------------
    All 'intelligent front end' processes should be adequately tested
    for compatibility with version 22 of OnTyme before August 1.  
 
    ACS will provide technical assistance to complete this test.  To
    provide the longest possible lead time for any necessary code
    conversions, please schedule these tests as soon as possible.  Ids
    on the Marketing test host are now available upon request from
    ACS.ONTYME. Please keep ACS.ONTYME informed of the results.
 
    There are two enhancements in version 22 of OnTyme that have the
    POTENTIAL of disrupting 'intelligent front end' processes such as
    OnTyme :EXEC procedures, PC based packages, and intelligent
    terminal procedures that match on the text of OnTyme messages or
    the format of the :SEND, :REPLY, :IN and :OUT commands.  
    
    First, the text of many of the more obscure (not frequently
    encountered) system and error messages has been revised for
    grammatical improvement and consistencey. The most common messages
    such as ACCEPTED and ALL MESSAGES READ have not been changed.  You
    may obtain a list of the text changes by sending a request to
    ACS.ONTYME.  In the event that an incompatibility is found between
    a front-end process and version 22, ACS will urge that the
    X.ONTYME protocol be adopted in the front-end to preclude any
    future problems.  Use of X.ONTYME will fix the problem once and
    for all.  Please :READ *** ONTYME22.XONTYME for further
    information about this protocol.
    
    Second, each OnTyme session will default to having subject line
    enabled.  Each :SEND or :REPLY command will be followed by a
    prompt for subject.  The format of the :IN and :OUT lists will
    change to show a subject line for each message.  The subject line
    feature may be disabled at the session level providing a very
    simple solution to any compatibility problems caused by subject
    line.  Please :READ *** ONTYME22.SUBJLINE for further information
    about the subject line feature.
 
    Please direct any questions about this announcement to ACS.ONTYME.
 
    Thank you.
    ACS.ONTYME
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 15 Jul 86 21:14:40 UT
From: IPC.SRAMAIL@Ontyme.Tymnet 
Date: 15 JUL 86 18:45:33 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A06589@Ontyme.Tymnet 
Subject: 940'S SHUTDOWN 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 1 Aug 86 15:47:26 PDT 
Resent-to: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@C26.Tymnet>, Joe Smith <jms@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 

                         M E M O R A N D U M





DATE>      15 JUL 86  10:42

TO>        THE WORLD

COPIES>    

FROM>      RESOURCE PLANNING AND MANAGEMENT


SUBJECT>   940'S SHUTDOWN


-----------------------------------------------------------------------



     
FOR YOUR INFORMATION ALL XDS940 SYSTEMS WILL BE TAKEN OUT OF SERVICE BY    
DECEMBER 31, 1986.  
     
AT PRESENT, WE HAVE SCHEDULED TO SHUTDOWN C-14 ON JULY 31, 1986 AT 2400 PST.    
     
ALL CUSTOMERS VALID ON C-14 WILL BE MOVED TO C-16. WE WILL COORDINATE THE  
DETAILS OF THE MOVE WITH THE BRANCH OFFICES. 
     
ALL INTERNAL USERS VALID ON C-14 MUST BEGIN TO SAVE ANY FILES OR TAPES
CREATED ON C-14 TO ANOTHER HOST PRIOR TO THE SHUTDOWN ON 07/31/86.    
     
IF YOU HAVE ANY QUESTIONS REGARDING THE XDS940 SYSTEMS SHUTDOWN, I CAN BE  
REACHED AT (408) 446-6630 OR ONTYME IPC.SRAMAIL.  
     
     
THANK YOU,
LINDA FREITAS, RPM
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Sun, 20 Jul 86 1:20:50 UT
From: ISGHQ.K/PETERS@Ontyme.Tymnet 
Date: 16 JUL 86 16:12:02 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J63991@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS INFORMATION"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 1 Aug 86 15:47:29 PDT 
Resent-to: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@C26.Tymnet>, Joe Smith <jms@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 


            MCDONNELL DOUGLAS INFORMATION SYSTEMS GROUP

                      ADMINISTRATIVE BULLETIN


No. ISG-86-71                                      16 July 1986


To:           All ISG Employees

Subject:      SMOKING POLICY

                                                                   


Many of our employees and visitors prefer to conduct their 
business in a smoke-free atmosphere.  While we respect each 
individual's right to smoke or not, we are also responsible for 
providing a comfortable, safe and healthy workplace.

The following guidelines are an attempt to appropriately balance 
the preferences of both smokers and nonsmokers.

    o    Employees who smoke should do so primarily in their 
         immediate work areas, should respect the wishes of their 
         fellow employees, and should observe all designated 
         no-smoking areas.

    o    Employees may designate their office/cubicle or work area 
         as a smoking or no-smoking area.  "No Smoking" signs are 
         available through stationery.

    o    Smoking is not permitted in elevators, hallways, 
         libraries and restrooms.

    o    Smoking will not be permitted in meetings or classes, 
         unless the meeting leader determines that it is agreeable 
         to all participants.

    o    Smoking will be permitted in designated areas within 
         breakrooms, dining areas (other than the food service 
         areas), lobbies and reception areas, provided that these 
         areas are properly ventilated.  Such areas must be 
         clearly designated and have adequate separation from 
         no-smoking areas.

    o    Smoking is not permitted in areas where it would endanger 
         lives, damage equipment and/or otherwise pose a safety 
         hazard.  These areas will be designated accordingly.

    o    Management has the responsiblity to resolve disputes 
         which may arise and to make reasonable accommodations for 
         all employees.



Page 2


In addition to these guidelines, all ISG organizations must ensure 
that smoking policies for their work locations are in accordance 
with any local government ordinances.

The successful implementation of this policy will require your 
cooperation, courtesy and accommodation of individual 
preferences.  Questions regarding this policy should be directed 
to your local Human Resources organization.




R. A. Fischer
Chairman
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 23 Jul 86 21:18:42 UT
From: ISGHQ.K/PETERS@Ontyme.Tymnet 
Date: 23 JUL 86 20:22:20 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J66860@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS CORPORATION"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 1 Aug 86 15:47:34 PDT 
Resent-to: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@C26.Tymnet>, Joe Smith <jms@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 


                   MCDONNELL DOUGLAS CORPORATION

                      ADMINISTRATIVE BULLETIN


No. MDC-86-57                                      23 July 1986


To:           ISG Lists A - H

Subject:      DEPARTURE OF R. A. FISCHER

                                                                   


Effective immediately, ISG Chairman Bob Fischer is leaving MDC to 
accept a position with Prime Computer Inc., of Natick, 
Massachusetts.


Bob has played a crucial role is shaping MDC's information systems 
strategy and in building an organization to pursue that strategy.  
I hope you will join me in wishing him all the best.




J. F. McDonnell
President
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 25 Jul 86 1:21:29 UT
From: TYMOPS.V/STAFFORD@Ontyme.Tymnet 
Date: 10:12 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M10546@Ontyme.Tymnet 
Subject: Validations Move to Orchard Parkway 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 1 Aug 86 15:47:38 PDT 
Resent-to: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@C26.Tymnet>, Joe Smith <jms@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 

                 MCDONNELL DOUGLAS NETWORK SYSTEMS COMPANY

                     T Y M N E T    O P E R A T I O N S

                            M E M O R A N D U M

DATE>      24 JUL 86  18:09

TO>        The World

COPIES>    

FROM>      Tymnet Validations Department


SUBJECT>   Validations Move to Orchard Parkway


---------------------------------------------------
     
[7mI[0m	SKIP T1,[SI[

IXBIT /DSKions Department will be moving to the Orchard Parkway
facility on Friday July 25. We can be reached all day on Friday at our
present numbers. But beginning on Monday July 28, our phone numbers will
be changed to the following:


    EMPLOYEE NAME                          EMPLOYEE PHONE NUMBER
    --------------                         ----------------------

Stephanie Aiken                              942-5361
Do
     
[7mI[0m	SKIP                    942-5386
Emy Lonero                                   942-5355
Karen Lynas                                  942-5339
Patti Quevedo                                942-5009
Vida Stafford                                942-5352

Our ontyme names WILL NOT CHANGE. If you have problems reaching us during
the first few days after our move, just send an ontyme.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 31 Jul 86 21:05:27 UT
From: ACS.L/METOUR@Ontyme.Tymnet 
Date: 31 JUL 86 18:22:16 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A15277@Ontyme.Tymnet 
Subject: Significant Business Issue 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 1 Aug 86 15:47:44 PDT 
Resent-to: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@C26.Tymnet>, Joe Smith <jms@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 

 
July 29, 1986
 
To :  All ISG employees
Re :  Significant Business Issue
 
=================================================================
 
In the mail you will be receiving an invitation to participate in 
selecting ISG's Significant Business Issue (SBI) from Jeremy J. 
Causley.  In the attached Participant's Guide, you are provided 
SBI Idea Sheets to submit your ideas.  In addition to these 
hardcopy Idea Sheets, you have the option of submitting your 
ideas via OnTyme.
 
To submit your ideas via OnTyme, type the following :
 
                    :EXEC *** FORMS.SBI/IDEA
 
This OnTyme Idea Sheet will forward automatically to St Louis.  
Each Idea Sheet must be submitted as soon as possible.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 1 Aug 86 17:05:07 UT
From: ISGHQ.K/PETERS@Ontyme.Tymnet 
Date: 01 AUG 86 17:31:52 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J70565@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS COMPUTER SYSTEMS"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 1 Aug 86 15:47:48 PDT 
Resent-to: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@C26.Tymnet>, Joe Smith <jms@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet> 



            MCDONNELL DOUGLAS COMPUTER SYSTEMS COMPANY

                      ADMINISTRATIVE BULLETIN


No. CSC-86-26                                      28 July 1986



Effective immediately, Vern Z. Smith, Executive Vice President, 
McDonnell Douglas Computer Systems Company, is being re-assigned 
to a position of Special Assistant to the President of McDonnell 
Douglas Computer Systems Company.  As a result of this change, 
Vern's direct reports will now report directly to me.


Your cooperation during this period of transition will be 
appreciated.



Otis H. Brinkley
President
McDonnell Douglas
Computer Systems Company
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 1 Aug 86 16:21:03 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: ADD-PACK on C26.. 

BE AWARE!!!!

Operation will be doing a "ADD-PACK" to C26 tonight.
I gave them the correct pakcopy "PAKCP5" and remind
them about INIT commend, doesn't initilize second HOME
and BAT pages but system take care of it during ADD-PACK

-Osman-
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 2 Aug 86 8:41:58 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hotline call for F30. 

2-Aug-86 05:15/Joe
Contact:  Fremont Operation, (415)498-2588, Merina, System F30.

Problem:  UIL stopcode with memory errors.  Deselected sector of memory, then
          got "Inconsistant Hash Constant" and "Too many last logical units"
          and "Logical unit 89 missing from DSKB".

Solution: Went on-site set breakpoints and check the home pages.  DSKB11
          messed up.  Of the 24 nonzero words in the HOM page, all but 5 were
          zero.  The check for missing logical units gets into an infinite loop
          if HOMLUN does not match HOMLOG.  Patched the HOM page on BPA13.
          Came up in CONSOL mode, ran DSKCLN in CHECK mode.  It complained that
          unit 9 was missing RIB pointers.  Put them in using (SPL)FILDDT.
          DSKCLN ran on in check mode, recycled and ran FULL DSkCLN at 8:30am.

From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 2 Aug 86 20:39:16 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: F58 still down. 

Progress report in first page of (M33)HOTLIN.LOG.From: FLETCHERC@X930.Tymnet 
Date: Mon, 4 Aug 86 8:44:26 PDT 
To: CARL 
Subject: "SUBJECT: REVIEW HILE LOOKING OVER"... 

SUBJECT: REVIEW
 HILE LOOKING OVER THINGS FOR YOUR REVIEW  FIND THAT  WILL NEED SOME MORE
PAPERWORK FROM YOU.   @LEASE FILL OUT "JPERFORMANCE EVIEW UPPLEMENT"
PART 2 GOALS AND OBJECTIVES.  ANDHUMAN RESOURCE MANAGEMENT FORMS.  I
THINK YOU RECEIVED COPIES OF EACH INITIALLY. IF NOT, \OIS CAN SUPPLY.
FOR SOME REASON CAPS DONT SEEM TO BE WORKING.  SEE YOU IN MID AUGUST.

    CRAIGReceived: from X62.Tymnet by X930.Tymnet; Mon, 4 Aug 86 12:20:06 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Mon, 4 Aug 86 12:15:37 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Hdwr read error, your file. 

I got the following error msg when I GFDed to MPL:62:
	DSKB:DPY.SAI hardware data read error detected.
I don't know if it's significant, but I thought you might like/need to know,
in case the file is damaged.

/D.From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 5 Aug 86 1:58:19 PDT 
To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet> 
Subject: New LS %3 

I updated (XEXEC:39)LS.SAI .SAV and copied it to 930 & 62.  It
now looks at your width parameter and will pring as many columns
as will fit reasonably.                   ^PRINT^
/Carl

PS: I left mail for WRS to move it around... I didn't want to
    bother with gateways.Received: from X62.Tymnet by X930.Tymnet; Tue, 5 Aug 86 10:15:44 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Tue, 5 Aug 86 10:12:01 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: New LS %3 
In-reply-to: your message of Tue, 5 Aug 86 1:58:19 PDT

a nice fix to LS!  thx!  /D.From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 5 Aug 86 10:41:21 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: I AM IN BLDG C TODAY.. 

I FORGOT THE MEETING AND CAME HERE STRAIGHT FROM HOME.
OSMANReceived: from X62.Tymnet by X930.Tymnet; Tue, 5 Aug 86 11:09:13 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Tue, 5 Aug 86 11:06:35 PDT 
To: Osman Guven <osman@X930.Tymnet> 
Cc: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Re: I AM IN BLDG C TODAY.. 
In-reply-to: your message of Tue, 5 Aug 86 10:41:21 PDT

O.,

Craig is in St. Louis this week.  (He'll be in Hawaii next week.)  I'm the
only one here.  You haven't missed anything.  No problem.  /D.  From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 6 Aug 86 18:11:44 PDT 
To: carl 
Subject: Which FDM 

How many different versions of FTM do you know of?  Which ones work?From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 7 Aug 86 10:39:44 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Escalation #M03757 

(begin forwarded message)
----------------------------------------------------------------
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 7 Aug 86 17:03:41 UT
From: UKNS.LEEWAKE@Ontyme.Tymnet 
Date: Thu, 7 Aug 86 17:03:41 UT 
To: TXS.O/GUVEN@Ontyme.Tymnet 
Cc: TXS.O/GUVEN@Ontyme.Tymnet, QSATS.S/TSUI@Ontyme.Tymnet,
	UKNS.WALLACE@Ontyme.Tymnet, UKNS.LEEWAKE@Ontyme.Tymnet 
Message-id: J72826@Ontyme.Tymnet 
Subject: Escalation #M03757 

=======================  M E M O R A N D U M  =========================

                                           MCDONNELL DOUGLAS INFORMATION
DATE>      07 AUG 86  16:06                          SYSTEMS CONSULTANT

TO>        Osman Guven

COPIES>    S.Tsui B.Wallace

FROM>      Lee Wakefield


SUBJECT>   Escalation #M03757


-----------------------------------------------------------------------


 Osman,

 I sent you a message on system 26, but incase you did not get it I will
repeat my findings here.

 I logged into system 26 for about an hour (40 times or so) and the
system did not crash. I noted one small point that maybe worth a mention,
a few times when the server set read file, not all of the "did not set"
message was printed IE.

 TYMSHARE C26-P034/P19-5 2-28-86
-
-
Did not set WC RC OP GD TD ST JL AC XC WA
-


 I would get :-


 TYMSHARE C26-P034/P19-5 2-28-86
-
-
AC XC WA
-

 I just got your message on sys26 stating the above is a side effect
of the patch untill a better solution is found. Suits me!

   Thanks

    Lee
----------------------------------------------------------------
(end forwarded message)
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 7 Aug 86 23:28:59 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Solved some of the APRFAT and UIL stopcodes. 

Osman and I tracked down the cause some of the APRFAT and UIL stopcodes.
It starts when a user in Great Britain uses a menu server to login and set
status.  Apparently a leave-deferred-echo message catches up to input characters
with all the satellite delays.  In any case, SCNSER sees message type 15 when
it is not ready for it.  On the KLs running P034/P-12, this causes a TTYZNE
info stopcode, with contents of P1 = 15 (the system stays running).  On the
KIs, any INFO stopcode turns off paging and causes a UIL halt stopcode.  On
C26-P034/P19-4, the INFO stopcode runs to completion, but accidently sets the
NXM flag causing an APRFAT stopcode at the next clock tick.

For C26, putting a JFCL at LDEC+2 in SCNSER will avoid the INFO stopcode.
For the KI systems, (M33)INFOKI.PAT will make INFO stopcodes work.

It is possible that the menu server is not strictly following TYMNET-II
protocol in respect to green balls and deferred echoing.  Since it tells the
host it has echoed the characters it is sending (but really doesn't), I
suspect the menu server.  We need to come up with a better way for SCNSER
to handle this problem.

				/JoeReceived: from B39.Tymnet by X930.Tymnet; Fri, 8 Aug 86 5:53:30 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 8 Aug 86 5:53:04 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: Deployment of FREE Program on PDP10s 

(begin forwarded message)
----------------------------------------------------------------
Received: from C26.Tymnet by B39.Tymnet; Mon, 21 Jul 86 19:00:42 PDT
Received: from EMSNTD.Ontyme.Tymnet by C26.Tymnet; Mon, 21 Jul 86 23:06:50 UT
Return-path: <TYMOPS.V/STAFFORD@Ontyme.Tymnet> 
From: TYMOPS.V/STAFFORD@Ontyme.Tymnet 
Date: 21 JUL 86 15:09:58 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: M09402@Ontyme.Tymnet 
Subject: Deployment of FREE Program on PDP10s 

                 MCDONNELL DOUGLAS NETWORK SYSTEMS COMPANY

                     T Y M N E T    O P E R A T I O N S

                            M E M O R A N D U M

DATE>      21 JUL 86  15:08

TO>        William Soley

COPIES>    Gary Perez
           Steve Metcalf
           Ed Barens 
           Wayne Bartlett

FROM>      Vida Stafford


SUBJECT>   Deployment of FREE Program on PDP10s


-----------------------------------------------------------------------



It seems that some users log into systems and do not log off after completing
their work. This results in a heavily loaded system that needs to be 'policed'
on a regular basis.

There is a software program called 'free' that checks usernames on a regular
basis and logs off any users that are inactive and/or are in a Control C
state.

I am requesting that this program be loaded on the following Tymnet Dedicated
Systems:     Private Net Code Genning   Public Net Code Genning
               22,37,26,31                  25,70,33,54
The above systems are in a critical need of this program.


I appreciate any help you can give on this.
----------------------------------------------------------------
(end forwarded message)

So, how do you want to handle this?
-BillFrom: JMS@X930.Tymnet 
Date: Sat, 9 Aug 86 21:38:38 PDT 
To: carl 
Subject: "6657 now on 3143. I borrowed the"... 

6657 now on 3143.
I borrowed the gender changed from the ADM that we to TITO backups on in
order to connect 6657 to 3143's distribution panel.  /JoeFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 11 Aug 86 20:29:58 PDT 
To: WRS, oliveb!oliven!gnome@tymix.tymnet, Mark Crispin
	<MRC%PANDA@SUMEX-AIM.ARPA> 
Cc: carl 
Subject: Get-together at the Smith's? 

After spending so much time getting the apartment in order, it's a shame to
waste it.  Do you have any plans for the coming weekend?  If there is enough
response, we can have a party on the 16th and/or 17th.  Would you and your
wife be able to come?  Please RSVP as soon as possible so I can notify the
others.

			Joe and Sally Smith
			Home phone (415)790-0608From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 11 Aug 86 21:16:32 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: P034/P33 status. 

The file (M33)P33TMP.PAT shows what has been patched to solve the problems
found in the /P33 monitor.  I don't plan to make a /P34 until I find out why
INFO stopcodes set the NXM flag on the KL.  INFO stopcodes work find on the F3,
the KS, and now that SVSETS has been patched, the KI.  It may be in the code
that reads S-bus diags from all possible memory controllers (code borrowed from
TOPS-10 which may not be worth keeping on TYMCOM-X).

			/JOEFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 12 Aug 86 0:10:54 PDT 
To: carl 
Cc: jms 
Subject: Channel 54 WHO schedule. 

This message is so that I can have the schedule on-line.
Jun 23 Timeflight, Time-2, Time-3, Time-4, Arc of Infinity,               Robot
Jun 30 Arc-2, Arc-3, Arc-4, Snakedance, Snake-2,                   Ark in Apace
Jul 07 Snake-3, Snake-4, Mawdryn Undead, Maw-2, Maw-3,      Sontaran Experiment
Jul 14 Maw-4, Terminus, Terminus-2, Termin-3, Termin-4,   Genesis of the Daleks
Jul 21 Enlightenment, light-2, light-3, light-4, Kings,    Revenge of Cybermen
Jul 28 Kings-2, Warriors of Deep, Deep-2, Deep-3, Deep-4,  Terror of the Zygons
Aug 04 Awakening, Awake-2, Frontios, Frontios-2, Frontios-3,     Planet of Evil
Aug 11 Front-4, Resurrection of Daleks, Res-2, Res-3, Res-4,   Pyramids of Mars
    Thursday, Aug-14, Interview with Colin Baker and "Twin Dilemma"
Aug 18 Planet o Fire, Fire-2, Fire-3, Fire-4, Caves Androzani, Android Invasion
Aug 25 Caves-2, Caves-3, Caves-4, Attack Cybermen, Cyber-2,    Brain of Morbius
    Thursday, Aug-28, "Attack of the Cybermen" is Colin Baker, 22nd Season.
Sep 01 Cyber-3, Cyber-4, Vengence on Varos, Varos-2, Varos-3,  Seeds of Doom
Sep 08 Varos-4, Mark of the Rani, Rani-2, Rani-3, Rani-4,  Masque of Mandragora
Sep 15 Two Doctors, Two-2, Two-3, Two-4, Two-5,                The Hand of Fear
Sep 22 Two-6, Timelash, Time-2, Time-3, Time-4,             The Deadly Assassin
Sep 29 Revelation of the Daleks, Rev-2, Rev-3, Rev-4, ?????,   The Face of Evil
Oct 06 ?????, ?????, ?????, ?????, ?????,                       Robots of Death

        Tentative Schedule (needs to be verified)
Colin Baker, repeat 24-Sep-86 to 4-Nov-86                      more Tom Baker
William Hartnell, 5-Nov-86 to 18-Feb-87
Patrick Troughton, 19-Feb-87 to 1-Apr-87       Davison 11-Apr-87 to 11-Jul-87
Jon Pertwee 2-Apr-87 to 15-Jun-87 (53 of 128, not the 73 that have repeated)
Tom Baker, 16-Jun-87 to 8-Feb-88               Colin Baker 18-Jul to 29-Aug-87
more Tom Baker                                 Hartnell 5-Sep-87 to 26-Dec-87
more Tom Baker                                 Troughton 2-Jan-88 to 30-Jan-88
Davison, 9-Feb-88 to 15-May-88                 Pertwee 6-Feb-88 to 9-Apr-88
Colin Baker, 17-May-88 to 27-Jun-88            Hartnell 16-Apr-88 to 6-Aug-88
                                               Troughton 13-Aug-88 to 10-Sep-88
Up thru March 1985, Colin Baker has 7 stories, 30 parts.
Pertwee has 24 stories with 128 parts, 10 stories with 53 parts new in 1985.
(the other 14 stories with 73 parts have been repeated already.)
Troughton had 21 stories with 119 parts, only 5 stories with 30 parts left.
Hartnell had 29 stories with 134 parts, only 17 stories with 76 parts left.
Received: from X62.Tymnet by X930.Tymnet; Tue, 12 Aug 86 12:07:31 PDT
Return-path: <JMS@X62.Tymnet> 
From: JMS@X62.Tymnet 
Date: Tue, 12 Aug 86 12:05:00 PDT 
To: carl 
Subject: "in job 7 context page I/O error."... 

?error in job 7 - context page I/O error.

I attached to job 7 running MAILER, did a GET (MAIL)MAILER, CSTART and
DETACH.  For some reason I did not get the ! prompt, and had to type
Control-C after the GET.  Did EXECX get upset?From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 12 Aug 86 12:30:30 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: I am on call 8/11 to 817.. 

o I am on call this week.

o I will be spending some of this week at Fremont D. C.
  debuging the BASE diagnostic. If you need me call
  Fremont or my pager.

-Osman-From: JMS@X930.Tymnet 
Date: Tue, 12 Aug 86 19:33:23 PDT 
To: CARL 
Subject: "LOGINN version 70.0 is on 930, "... 

LOGINN version 70.0 is on 930,62,95

It is on SYS if you want to play with running LOGINN in the child frame.Received: from B39.Tymnet by X930.Tymnet; Wed, 13 Aug 86 10:47:06 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 13 Aug 86 10:46:05 PDT 
To: carl@930 
Subject: FREE 

What should I do about FREE?
-BillFrom: Steve Capik <FSC.S/Capik@Ontyme.Tymnet> 
Date: Thu, 14 Aug 86 14:44:59 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Steve Capik <FSC.S/CAPIK@Ontyme.tymnet> 
Subject: user status on 22 

Carl 
       I have been trying to read some .doc files on system 22 and every
time I try to copy them over to my directory I get a read violation (2).
The directories have the tymnet documentation.  the two directories are (nts)
and (engdiag).  Could you change my status enough to be able to read the .doc 
files. 
					STEVEFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 14 Aug 86 14:48:55 PDT 
To: WRS@B39, CARL@X930, oliveb!oliven!gnome@tymix.tymnet, Mark Crispin
	<MRC%PANDA@SUMEX-AIM.ARPA> 
Subject: Weekend plans cancelled. 

Something came up, so Sally and I will not be having a party this weekend.
It looks like we will busy between now and Worldcon.

			/JoeFrom: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 17 Aug 86 14:01:04 PDT 
To: dennis ernst <Dernst@B39.Tymnet> 
Subject: Micronode outage... 

Dennis,
  I sent you a copy of the CTY output and left word for you to call and
haven't heard back yet, so I'm sending you detailed mail instead.

  Node 2346 keeps crashing and just goes out to lunch... "P" seems to
contnue it and then immediately another stopcode.  "1000G" appears to do
the same.

  Node 10115 & 6657 both seem to also get BFC stopcodes every so often.
I don't know the timing of the CSE's on 2346 vs outages on 10115 or 6657
but I know that 2346 only seems to stay up a short while every time it
is reloaded (short = 1 hour or so).

  Also, after a BFC, we've seen buffers get transposed for different lines
when one or both lines are AUX circuits from the host.  Dennis Coffey has
gotten my text & I his on our respective screens.

  I would really like something done so that 2346 will at least stay up a
much longer time between stopcodes.  We've reloaded it and reloaded it
figuring that you haven't had time to look at it, and I'm not even sure
there is a way for you to look at a "dead" node.

/CarlFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 18 Aug 86 1:16:23 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: I am on call this week. 

The schedule worked out fortuitously.  I am on call 18-Aug to 24-Aug and 8-Sep
to 14-Sep, which means my requested vacation time won't disrupt the rotation.
	/JoeFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 18 Aug 86 1:21:52 PDT 
To: carl 
Cc: Osman, DenCoff 
Subject: Considerations for LOGINN. 

Right now, if you run LOGINN in the child frame giving it a positive AUN, it
GETTABs the username and uses that, which must be in the LUD.  (The positive
AUN could be anything, including [0,1] since it is overwritten by the PPN
specified in the LUD.)  In order to avoid the question "do you want to attach
to this job", LOGINN version 70.0 completely bypasses the loop that checks
for other jobs same PPN.  Question: How should XEXEC/EXECX/LOGINN handle
batch requests for people with the bit set that says "do not allow other jobs
to login under this name".  Should such PPNs be prevented from runnning SUBMIT
in the first place?  Do we still have such restricted users?

Another thought came to me while thinking about a modem connected to our
micronet.  We could have usernames that were accessable only if you dialed this
particular modem.  Why not allow them to come in from TYMNET via double login?
That is, put TXS930, TXS62, and TXS95 in the MUD with no password, and have
LOGINN ask for the local name and password.  Then Osman could allow his kids
to access our systems without using his password.  And TXS95 would be valid,
whether we were running TYMCOM-X or TOPS-10.

		/JoeFrom: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 18 Aug 86 8:59:26 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: I am in Fremont D.C. today.. 

From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 18 Aug 86 12:11:49 PDT 
To: carl, osman 
Subject: Passwords in SETEKS. 

The file SETEKS.MAC holds SETE passwords for the external TYMCOM-X systems
(KS-2020's).  Dennis and I have put our alternate passwords in it already.
Please add entries for yourself if you want to have a different SETE password
on these external systems.		/JoeFrom: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 18 Aug 86 14:17:10 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: GROUP FSC ONTYME NAME 

Craig,
  As per our actual usage of ONTYME and the cost-cutting efforts within
FSC as a whole, in your absense we have talked about having our individual
FSC.x/yyyyyy accounts removed and having a group ONTYME name FSC.TYMCOMX or
FSC.TYMCOM-X.  We can discuss this at our next group meeting.

/CarlFrom: CARL@X930.Tymnet 
Date: Tue, 19 Aug 86 12:32:23 PDT 
To: dernst@b39 
Subject: "Node 10115 Fatal node error ILV"... 

Node 10115 - Fatal node error ILVReceived: from B39.Tymnet by X930.Tymnet; Tue, 19 Aug 86 17:02:44 PDT
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 19 Aug 86 15:18:01 PDT 
To: Joe Smith <JMS@X930.Tymnet> 
Subject: Re: FREE program installed. 
Resent-from: Joe Smith <JMS@X930.Tymnet> 
Resent-date: Tue, 19 Aug 86 17:15:12 PDT 
Resent-to: Monitor Group <JMS>, Carl, Osman 
Resent-cc: FletcherC, DenCoff 
In-reply-to: your message of Tue, 19 Aug 86 14:48:24 PDT

Putting it in the monitor might be nice.  The only disadvantage would be the
loss of the log.  I supose you could put stuff in the stream.  On the other
hand, I'm not sure anybody ever looks at the log.  I'd suggest the following
parameters:

	Number of free jobs desired.
	Minimum idle time required to kill a job.
	Maximum idle time allowed.
	Some method of handling exemptness (JBTPRV?)

Minimum idle would normally be something like 5 minutes.
Maximum idle would normally be 2^35-1.
Number of free jobs desired would be JOBN/10.
Exemptness is the only tricky part.

You wouldn't need a polling interval since you can check whenever somebody
logs in and less than the desired free jobs left.  And also whenever a job
becomes idle for the minimum time and less than the desired free jobs left.

This would be better than FREE does now.

Maybe also consider ports free?

-BillFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 20 Aug 86 11:20:41 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: HOTLINE not working after hours. 

I called 794-2567 last night.  It rang and rang at Mary's desk, and was not
forwarded to South Bay Answering Service.  This was supposed to have been
fixed Aug 12th.

			/JoeReceived: from B39.Tymnet by X930.Tymnet; Wed, 20 Aug 86 12:58:23 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 20 Aug 86 12:53:33 PDT 
To: (Monitor People) Carl@930, JMS@930, Osman@930 
Subject: crash on 36 

I was doing some framing stuff last night on 36 when it crashed.  I have no
idea if it was related.  It does not seem to be reproducible, if so.  I was
not doing anything fancy, just !foCFM, !foRUN, !foHST, HANG.  Let me know if
there is anything I should temporarily avoid.  -BillFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 20 Aug 86 13:23:28 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Cc: (Monitor People) Carl@930, Osman@930, FletcherC 
Subject: Re: crash on 36 
In-reply-to: your message of Wed, 20 Aug 86 12:53:33 PDT

It does not look like this particular crash was saved.  If it happens again,
be sure to call Operations and get them to deposit 1 in 30.  We have seen
a couple of similar hangs, here circuits into the system are till up, but
no response.  The base is still happy since channel 1 is being serviced, but
channel 7 is stuck in an infinite loop, trying to find the end of the DDB
chain.  In two of the crashes, there is a loop in the linked list of TTY
DDBs.  It looks as if the routine that unlinks a TTY DDB when the job is
killed or zapped was storing the wrong new forward pointer.  I have added
a consistancy check to SCNSER, it is is the P034/P33 monitor.




			/JoeFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 20 Aug 86 23:12:58 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: BOTLOD works on 3650 

The version of BOTLOD on (M33:70) punches a paper tape that is capable
of reading 3330 formatted in blocks, 3330 formattedn in pages, and 3650
formatted in pages.   It also creates BOTLOD.KL, which is a .SAV file
that needs to be converted to a .A10 file and copied to the KL's DECtape.

		/JoeFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 21 Aug 86 16:08:42 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Tape backup schedule 

Since we have 7 days a week coverage, we should be doing TITO incremental
saves every day.  I propose the following schedule:

	Saturday, Sunday	Carl
	Monday, Tuesday		Osman
	Wednesday, Thursday	Joe
	Friday			Dennis

We need to decide on a new schedule for doing the ALL FILES.From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 21 Aug 86 16:41:02 PDT 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Schedule.. 


I will be taking this Friday 8/22 as a comp time-off.
I have two days of comp time from last Holiday.
-Osman-From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 21 Aug 86 18:56:04 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: MONED1 working again. 

There is a bug in the GETMAP routine, which maps file pages into memory.
Aparently it will not allow more than 12 pages to be mapped.

When MONED1 is reading P034.MEM backwards from the end, it looks for the
latest number like "/P29".  Then it looks for the major version number, such
as "P034/Q".  If the major version number is 12 pages from the end of the
file, then MONED1 dies trying to map SYSID.DAT.  If the major number is more
than 12 pages from the end, then MONED1 dies trying to find it.

My work-around was to duplicate the text "P034/Q" closer to the end of the
file.  To prevent this from happening again, we need to either 1) Fix MONED1
to not use GETMAP, or 2) Release monitors more often.

			/JoeFrom: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 22 Aug 86 8:21:39 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Misc BACKUP & MONED1 

Backup schedules...
  incrementals every day sounds nice, but it does take time and
requires physical access to the machine...

  as we are being paid a bit more than operators, we should try to
minimize our "operations" tasks as much as possible.  ALL-FILES are
a fact of life... and once a month is the minimum we can get by on.
incrementals, on the other hand vary depending on how much work we
do on our machines.  Since there are only 4 of us, I feel that four
times a week (unless we have a special case) is enough giving each
of us one turn at it each week.

  we should look at the amount of work done on a daily basis on each
machine (in general) and decide when the 4 optimal times to backup
the system are taking into consideration what time of day each of us
would be dong the backup.
  (clearly, my backing up the system last thing on Sunday, and someone
else backing up the system first thing on Monday isn't very good...)

As long as I work Saturday & Sunday, I feel a backup of my work as I
finish up on Sunday lets us start the week fresh.  Tuesday afternnon
(after our weekly meeting) sounds like another good time, since we all
try to get things finished up for the tuesday meeting "show & tell".
I'm not sure when the other two "best" times ought to be...  anyone
have any better suggestions...?


Re: MONED1 -- Good work... I'm suprised we didn't run into this on the
    /K monitor release... must have barely made it with /J64 under the
    wire.  We've also been a bit more verbose than on previous releases
    of the monitor and that probably accounts for /P32 too far from
    the end.

    Now that you mention it, GETMAP does have that restriction on the
    number of pages... I wonder if it is limited to 12, or if it can
    just be changed to a larger number...  anyway, if I plug in the
    VMFILE routines, it won't matter since it only keeps one page mapped
    at a time (just like SIMIO).

/CarlReceived: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 20 Aug 86 15:45:13 PDT
From: HRD.TRNG@Ontyme.Tymnet 
Date: sbimemo 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A26044@Ontyme.Tymnet 
Subject: SIGNIFICANT BUSINESS ISSUE (SBI) IDEAS 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 22 Aug 86 8:40:44 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, mosserj 

*************************  MEMORANDUM  **************************
 
   DATE:  AUGUST 20, 1986
 
     TO:  ALL EMPLOYEES (Please Post)
 
   FROM:  STEPHEN REYNOLDS
          MANAGEMENT & PROFESSIONAL DEVELOPMENT (CUPERTINO)
 
SUBJECT:  SIGNIFICANT BUSINESS ISSUE (SBI) IDEAS
 
*****************************************************************
 
The deadline for submitting ideas for the ISG SBI has been 
extended to August 28.  The deadline is being extended because 
some employees indicated that they did not receive Jerry 
Causley's letter of July 24, 1986, and still wanted to contribute 
SBI ideas.  (To get a copy of Jerry's letter, contact Lorna Loram 
at 408/446-8825.)
 
All employees are welcome to submit their ideas for an SBI for 
the ISG.  All submissions will be combined with the ideas 
generated at the brainstorming sessions held last week at various 
ISG facilities, and will be reviewed by the ISG Management 
Committee during the week of September 8.  Ideas can be submitted 
prior to August 28 via two methods:
 
        . Mail your idea to:
               
                    SBI Ideas
                    Bldg. 306-2E, Room 278
                    P.O. Box 516
                    St. Louis, MO 63166
 
        . Use Ontyme to submit your idea; type
 
                    :EXEC *** FORMS.SBI/IDEA
 
Questions about the SBI process can be directed to me (408/446-
7811) or the User Support Center in St. Louis (314/232-6943).
 
 
SLR/lhFrom: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 22 Aug 86 21:47:32 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Cc: Wayne D. Bartlett <BARTLETW@B39.Tymnet>, nancyb@tymix, carl, Fletcherc 
Subject: Re: NONAME 
In-reply-to: your message of Fri, 22 Aug 86 18:25:11 PDT

NONAME.SIM is in (BURRIESCIN:38) and in an FDM file (SSSPOOL:74)NONAMF.041
I believe that we, the TYMCOM-X Support Group are responsible for it.  /JoeReceived: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Sat, 23 Aug 86 17:44:05 PDT
Received: from B39.Tymnet by -EMSTUMS.Ontyme.Tymnet; Wed, 20 Aug 86 10:02:01
	PDT
Received: From SLVMCL1.McAuto.Tymnet by B39.Tymnet; Wed, 20 Aug 86 16:40:14 UT
From: GREG GRAY <M189321@SLVMCL1.McAuto.Tymnet> 
Date: 08/20/86 11:37:00 
To: IPC.J/Krivanec@Ontyme.Tymnet 
Subject: Notice to PDP Customers 

*** Forwarding note from M189321 --SLVMCL1  08/19/86 16:06 ***
To: TUMSGATE--SLVM307  IPC.S/Metcalf      TUMSGATE--SLVM307  IPC.L/Yarbrough
    TUMSGATE--SLVM307  IPC.S/Perez        TUMSGATE--SLVM307  IPC.J/Krivanec
FROM: GREG GRAY ;  L205, LOC, 32007
SUBJECT: Notice to PDP Customers
With the Service Billing migration to St. Louis nearing completion, modif-
ications are becoming final. One of the mods is the change of length to the
user name field. Our thoughts in order to reach all PDP users is to send a
message upon 'sign on' or other applicable way that you may recommend. The
following message is requested to be sent:

"As part of the consolidation process wherein McDonnell Douglas Information
Services Group is integrating the former Tymshare data processing with those
services previously provided by McAuto and, at the same time, introducing a
degree of commonality between processing on IBM equipment and Digital Equip-
ment PDP 10s, it has become necessary to standardize the user name field size
at eight characters. A few customers currently have user names which exceed
this parameter. Please review your files and, if any user names in excess of
eight bytes are in existence, modify them to the extent necessary to reduce
them to eight or less. Effective 1 November 1986, both invoicing reports and
machine readable detail accounting information will reflect only eight
positions in this field. MDISG appreciates your cooperation in this endeavor
and regrets any inconvenience it may cause."

The above message has been agreed to by Joe Godwin, director of Customer
Billing and Forest Moore, director of Remote Computing Services. If you have
any questions or concerns please contact me as soon as possible. We would like
to have this to the customers by the end of August at he latest.
Greg Gray

cc: M143950 --SLVMCL1  WILSON    R. K.    M170734 --SLVMCL1  JOE GODWIN
    TUMSGATE--SLVMCL1  ipc.b/keim         TUMSGATE--SLVM307  IPC.S/Metcalf
    TUMSGATE--SLVM307  IPC.L/Yarbrough    TUMSGATE--SLVM307  IPC.S/Perez
    TUMSGATE--SLVM307  IPC.J/Krivanec     TUMSGATE--SLVMCL1  ipc.e/roopFrom: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 24 Aug 86 15:25:11 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: Notice to PDP Customers 

(begin forwarded message)
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Sat, 23 Aug 86 17:44:05 PDT
Received: from B39.Tymnet by -EMSTUMS.Ontyme.Tymnet; Wed, 20 Aug 86 10:02:01
	PDT
Received: From SLVMCL1.McAuto.Tymnet by B39.Tymnet; Wed, 20 Aug 86 16:40:14 UT
From: GREG GRAY <M189321@SLVMCL1.McAuto.Tymnet> 
Date: 08/20/86 11:37:00 
To: IPC.J/Krivanec@Ontyme.Tymnet 
Subject: Notice to PDP Customers 

*** Forwarding note from M189321 --SLVMCL1  08/19/86 16:06 ***
To: TUMSGATE--SLVM307  IPC.S/Metcalf      TUMSGATE--SLVM307  IPC.L/Yarbrough
    TUMSGATE--SLVM307  IPC.S/Perez        TUMSGATE--SLVM307  IPC.J/Krivanec
FROM: GREG GRAY ;  L205, LOC, 32007
SUBJECT: Notice to PDP Customers
With the Service Billing migration to St. Louis nearing completion, modif-
ications are becoming final. One of the mods is the change of length to the
user name field. Our thoughts in order to reach all PDP users is to send a
message upon 'sign on' or other applicable way that you may recommend. The
following message is requested to be sent:

"As part of the consolidation process wherein McDonnell Douglas Information
Services Group is integrating the former Tymshare data processing with those
services previously provided by McAuto and, at the same time, introducing a
degree of commonality between processing on IBM equipment and Digital Equip-
ment PDP 10s, it has become necessary to standardize the user name field size
at eight characters. A few customers currently have user names which exceed
this parameter. Please review your files and, if any user names in excess of
eight bytes are in existence, modify them to the extent necessary to reduce
them to eight or less. Effective 1 November 1986, both invoicing reports and
machine readable detail accounting information will reflect only eight
positions in this field. MDISG appreciates your cooperation in this endeavor
and regrets any inconvenience it may cause."

The above message has been agreed to by Joe Godwin, director of Customer
Billing and Forest Moore, director of Remote Computing Services. If you have
any questions or concerns please contact me as soon as possible. We would like
to have this to the customers by the end of August at he latest.
Greg Gray

cc: M143950 --SLVMCL1  WILSON    R. K.    M170734 --SLVMCL1  JOE GODWIN
    TUMSGATE--SLVMCL1  ipc.b/keim         TUMSGATE--SLVM307  IPC.S/Metcalf
    TUMSGATE--SLVM307  IPC.L/Yarbrough    TUMSGATE--SLVM307  IPC.S/Perez
    TUMSGATE--SLVM307  IPC.J/Krivanec     TUMSGATE--SLVMCL1  ipc.e/roop
(end forwarded message)

Bill, have you seen this message?

  Seems that someone needs to tell "them" that it isn't just the PDP-10
  customers that will be affected.  This affects all of TYMNET usernames
  in the MUD.  (Of course we "can" go through and weed out usernames that
  are non-unique to 8 letters, but that's a ridiculous waste of time; for
  a group of companies (ISG) not making a lot of money, this kind of
  "busy-work" is very wasteful of resources.)  Comments?

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 24 Aug 86 15:32:37 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Today 

I've been at home working today... (Sunday) ...My wife  has a bad cold
that may require me taking her to the hospital because of complications
with her asthma.  If you need me for anything, try my home number first
(408) 945-4314, then the office x2515, then my beeper (408) 977-7960.

I will be in the office later to do the incrementals on 930 and 62.  I'm
just waiting till the last minute because of my wife.  See you all
tomorrow.

/Carl


Re: ON CALL

    I will be on call from monday Aug 25 to sunday Aug 31.

/CarlReceived: from X62.Tymnet by X930.Tymnet; Mon, 25 Aug 86 9:41:20 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Mon, 25 Aug 86 9:34:55 PDT 
To: (Monitor Group:), Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven
	<OSMAN@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet>, Tymcom-X Acct. Sup.
	<TXSSUP@X930.Tymnet> 
Subject: My TTE Class this week. 

I am in Tymnet Training's 445 class all week.  I will be checking my TUMS
mail regularly.  If you need to get a phone message to me, the number here
is 408/435-0239.  See you in a week, or sooner.
/D.From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 26 Aug 86 19:42:32 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: The HOTLINE works (after 8 rings). 

I noticed that Dallas called our beeper, so I tried dialing 794-2567 myself.
It rang and rang, and it took 30 seconds for the phone system to decide that
it should forward the call to South Bay Answering Service.  We may need to
warn the Data Centers to let the phone ring 20 times before giving up.  /JoeReceived: from B39.Tymnet by X930.Tymnet; Thu, 28 Aug 86 0:46:45 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 28 Aug 86 0:41:12 PDT 
To: DenCoff@62, Carl@930, JMS@930, Wayne D. Bartlett <BARTLETW@B39.Tymnet>,
	JMARCIN@36 
Subject: ""... 
Subj: 


Subject: "I've made changes to PASSWORD"... 
PDTre43ofPASSWORD: 

I've made changes to PASSWORD as noted below.  The sources and supporting
files are in (WRS:62)PASSWO.* and are ready for QA and transmittal.  It
is currently running on systems 36 and 39 in Bubbnet.  This version will
not work in the public net until Jill releases the latest version of NETVAL
in the next couple weeks.  Since there is a significant security improvement
from this version, as well as improved user friendliness, it is strongly
su
Subject: "Differences between password version"... 
PDTreitsreleasebeasexpedientaspossible.Pleaseletme: 
knowifthereisanywayIcanhelp.Thanks-Bill: 

Differences between password version 40 and password version 43:

1. The user is now prompted for his network password.  This is done regardless
of which user's password is being changed.  The verifies that the person
running the password program is authorized to make the change and prevents
someone from walking up to a logged in terminal and changi
Subject: "2. Changes have been made to allow"... 
PDTre: 
ord.: 

2. Changes have been made to allow the program to work in BUBBNET.

3. Two new error messages have been added:
	PWBAD# = 18	?password not correct for user <>
	PWNTV# = 19	?command not implemented by NETVAL: <>

4. Security has been improved by providing an audit trail in the NETVAL
operator log.  The audit trail includes the host, node, port, and job
numbers and the user name making the change.  Also date and time in UTC.
From: CARL@X930.Tymnet 
Date: Thu, 28 Aug 86 15:15:14 PDT 
To: osman 
Subject: "Subject: Terminal in your office"... 

Subject: Terminal in your office

  Sorry if my terminal in your office seems to be an inconvenience... I've
needed three terminals to do any work on XEXEC for the past week or so.  I
didn't expect you in, so I didn't move it back to my office.  I've been in
using your ofice in your absense (and after you've gone for the day).

  I just didn't want you to get the wrong impression.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 28 Aug 86 15:25:37 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: New Monitor on 33, 34, 36 & 57 

Osman,
  Sue called... Systems 33 and 36 will be scheduled to come up with the
new monitor tonight as you requested.

  Systems 34 and 57 will be scheduled for sometime over the weekend since
tonight would not be a good time for the Japanese customers on those two
systems.
/Carl
From: TXS-TEXT@X930.Tymnet 
Date: Thu, 28 Aug 86 15:28:54 PDT 
To: carl 
Subject: "RE: your correspondence od 1:30"... 


RE: your correspondence od 1:30 PM yesterday

Did I make the correct move? (I think I did!) Anyway, onward. I just got back
from Radio Shack. I bought two of them. Is that Okay? I just went in and when
the salesman said "Can I help you?" I just held the cable up and said "I need
two of these." and he got them for me. Easy, huh? Please call me about an 
hour BEFORE you plan to come home, so I can try to get something on the table for you, okay? ILYVMADYFI
/Wad
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 29 Aug 86 9:55:07 PDT 
To: CARL@X930.Tymnet 
Subject: Re: "Subject: Terminal in your office"... 
In-reply-to: your message of Thu, 28 Aug 86 15:15:14 PDT

No Carl, I didn't get any wrong impression. I just can't work
in a crowded office.
-Osman-
From: Osman Guven <osman@X930.Tymnet> 
Date: Fri, 29 Aug 86 9:58:56 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: New Monitor on 33, 34, 36 & 57 
In-reply-to: your message of Thu, 28 Aug 86 15:25:37 PDT

Thanks for the info. Over the weekend 33,36,22,70,34,57,54 they
should all be running the P034/P34 monitor.
-Osman-
Received: from B39.Tymnet by X930.Tymnet; Sat, 30 Aug 86 0:49:23 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 30 Aug 86 0:44:20 PDT 
To: carl@930 
Subject: names not in mud 

The following GAN 3 names are valid onart ma36 or 39 or both, but are
not in the MUD.  This doesn't matter to us, but if you'd like them put in
the MUD, let me know.  -Bill   (BUBBNET mud that is)
Received: from B39.Tymnet by X930.Tymnet; Sat, 30 Aug 86 0:49:29 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 30 Aug 86 0:45:31 PDT 
To: carl@930 
Subject: names not in mud 

CHERIE		3,300745	not in mud   
KL10		3,222635	not in mud
TUMS		3,431046	not in mud
TXS-TEST	3,346701	not in mud  art ma	3,231227	not in mud   
YEUX		3,377412	not in mud
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 31 Aug 86 6:02:34 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline Call 31-Aug-86 5:20am 


31-Aug-86 5:20 am /Carl
Caller:   Building C OPS, 408-446-7705, System 26

Problem:  MAPAL1 stopcode (2 - 1st @ 1:00am, 2nd @ 5:xx am) while doing
          the all-files store to tape.

          Both crashes listed problems with memory box 113 error in
          sec 3, port 7 -- (memory reset) BEFORE taking a crash dump.

Solution: Ran TITO/TTR on 2nd tape of the allfiles and did a directory to
          see the last file stored to tape.
          Ran DIRIT with the command "DIR (BILLING10)/UNSORTED" to find
          the file(s) in the same vicinity of the last file stored to the
          tape.  (TTR reported an I/O error before reaching the checksum
          on the last file, indicating that it started the file but DID NOT
          finish the store).
          Ran CKSUM on the "next" presumably good file without error.
          Renamed the "bad" file to (CRASH) and ran LKBDDT to set NFS and NDL
          on the file so that TITO would not try to back it up, and noone
          would accidently try to delete it and have page-deallocation crash
          the system.

Background: Last all-files was done on 8/16/86.  We were running /P19-5 on
            the 15th, /P33-5 on the 16th, and /P34 on the 24th.

            The BAD NEWS is that the bad file was (BILLING10)082686.SAT
            which must have been created after we started running /P34.

Received: from X62.Tymnet by X930.Tymnet; Tue, 2 Sep 86 2:27:49 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 27 AuXS32:17 PDT 
To: Landa Morris <IPC.L/Morris@Ontyme.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Dennis Coffey
	<dencoff@X62.Tymnet>, txssup@x930 
Subject: Username changes 

Landa,
  as per our phone message, I'd like the following usernames changed...

    TXS-TEXT  gan 3  uun 300745   to  TXSTEXT      remove the dash
    TXS-TEST  gan 3  uun 346701   to  TXSTEST      remove the dash

    HELGEM    gan 3  uun  XSto  SUBMIT       new name

  the first 2 are valid on all PDP-10s and the third may be on all, but
  I am not sure.  I looked up SUBMIT in the MUD already, it doesn't yet
  exist.

  HELGEM used to belong to  HELGE MORTENSEN and now contains files for the
  user interface called SUBMIT for the perp-replacement program.  That is
  the reason for the name change.  If you need a "responsible for" name on
  SUBMIT, please use my name.  Everything else on XSrrect.

Thanks a bunch.

/Carl


Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 2 Sep 86 13:54:20 PDT
From: IPC.L/MORRIS@Ontyme.Tymnet 
Date: 02 SEP 86 13 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.J/SMITH@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	IPC.L/MORRIS@Ontyme.Tymnet 
Message-id: A32259@Ontyme.Tymnet 
Subject: USERNAME CHANGES 
ad'm: 

SUBJ: 


     ATTN:  CARL

     FROM:  LANDA

     SUBJ:  USERNAME CHANGES


     HI!  THE THREE USERNAMES YOU LISTED IN ONTYME A31879 HAVE BEEN
CHANGED TO THE SPECIFIED NEW ID'S.  PJ (AS YOU KNOW) WILL NEED SOME
TIME TO UPDATE ALL THE LUDS.  NAME CHANGES ARE AS FOLLOWS:

     OLD UNAME     NEW UNAME
ad'm ------     ---------

     TXS-TEXT   =  TXSTEXT
     TXS-TEST   =  TXSTEST
     HELGEM     =  SUBMIT


THANK YOU,
LANDA/RP
From: CARL@X930.Tymnet 
Date: Tue, 2 Sep 86 17:04:54 PDT 
To: dernst@b39 
Subject: "Re: Friggin' Character Lossage"... 

Re: (&*^*^&* Friggin' Character Lossage (&*^&*%

I reloaded everything on Friday... nothing died, nothing got any stopcodes
and yet, we're experiencing lots and lots of character lossage.  The same
monitor was running for a few days without any problems... then <WHAM>...

i haven't received a single mail message from ONTYME or TUMS from another
host (building circuits into 930!!!!) that was NOT garbled.  Please....

take a peek at the network...  /Car
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 2 Sep 86 18:42:35 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: I'm back 

WORLDCON was fine.  New Orleans won the honor of hosting the 1988 Worldcon,
Boston in 1989 (England in 1987, but we will be going to the alternate in
New Mexico instead).

I noticed several garbled mail messages, so I hung 930, stopped its microcode
long enough for the base to notice, and then reloaded the F3.  The problem is
something in P034/P34 that affects all processors - COMMON does not clean up
after itself when taking the system down like is used to.  In the process of
fixing that, I will look into passing more information across reloads.  Such
as time of day, and reason for the reload.			/Jo
From: Ken Dawson <Peak@X930.Tymnet> 
Date: Wed, 3 Sep 86 1:15:30 PDT 
To: Carl A Baltrunas<Carl@X930.Tymnet> 
Subject: lest you miss it 

    62's dow
Received: from X62.Tymnet by X930.Tymnet; Wed, 3 Sep 86 14:17:25 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 3 Sep 86 13:55:35 PDT 
To: Osman Guven <osman@X930.Tymnet> 
Subject: this is a test 


this i a test message to try to see if the micronodes are screwing up.
this i a test message to try to see if the micronodes are screwing up.
this i a test message to try to see if the micronodes are screwing up.
this i a test message to try to see if the micronodes are screwing up.
this i a test message to try to see if the micronodes are screwing up.
this i a test message to try to see if the micronodes are screwing up.
this i a test message to try to see if the micronodes are screwing up.
this a a test message to try to see if the micronodes are screwing up.
this i a test message to try to see if the micronodes are screwing up.
this i a test message to try to see if the micronodes are screwing up.
-
/car
Received: from X62.Tymnet by X930.Tymnet; Thu, 4 Sep 86 13:18:17 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 4 Sep 86 13:17:09 PDT 
To: osman 
Subject: "Subject: OOPS! I presume you turned"... 

Subject: OOPS!

I presume you turned off your ADM36 terminal...  when you did, it zapped
the circuit and XEXEC logged off.. sigh.

/Car
Received: from X62.Tymnet by X930.Tymnet; Thu, 4 Sep 86 14:15:11 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 4 Sep 86 13:57:06 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: Crash mail 

Osman,
   I'm not sure if CUPC is providing this information... but I know that
DALLAS is NOT...  in the crash mail, I'd like them to list what monitor
was running as well as what monitor was loaded after the crash.  Dallas
is telling us the STOPCD and the monitor that is being loaded, but not the
monitor that was running at the time of the crash.

   Does everyone agree that this information is a good idea?

/Car
Received: from X62.Tymnet by X930.Tymnet; Thu, 4 Sep 86 16:15:20 PDT
Return-path: <DENCOFF@C62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C62> 
Date: Thu, 4 Sep 86 16:03:07 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Crash mail 
In-reply-to: your message of Thu, 4 Sep 86 13:57:06 PDT


Carl,
  for what my opoinion is worth, re. the monitor that crashed:  good idea!
/D
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 6 Sep 86 2:30:22 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: HOM/RIB/UFD documentation. 

After spending all of Friday with C57, I added to the disk documentation and
put it all in a single menu in the INFO tree.  To get to it, type "DISK" when
you get into the INFO program, or just "TYPE (MONDOC)RIB.NFO".  /Jo
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 6 Sep 86 15:19:56 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: (MONDOC)RIB.DOC 

I updated the HOM page section to accurrately reflect the monitor.
also rib pointers unit/page division is "noted" to cross the half-word
boundary bits 17-35 correctly.
/Car
From: Osman Guven <osman@X930.Tymnet> 
Date: Sat, 6 Sep 86 19:59:25 PDT 
To: peak, Joe Smith <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>,
	dencoff 
Subject: PEAK on X930.. 


PEAK seems to have some problem..
Is it me or it real have problem.???

Things go mishap when it is wrtting "Checkpoint".

-Osman
Received: from B39.Tymnet by X930.Tymnet; Mon, 8 Sep 86 13:00:25 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 8 Sep 86 12:45:52 PDT 
To: JMS@930, CARL@930, DENCOFF@62 
Cc: ksinclair@36 
Subject: "BILL, WHO IS THE RUNOFF EXPERT?"... 

(begin forwarded message)
----------------------------------------------------------------
Received: from B36.Tymnet by B39.Tymnet; Sun, 7 Sep 86 14:58:41 PDT
Return-path: <KSINCLAIR@B36.Tymnet> 
From: KSINCLAIR@B36.Tymnet 
Date: Sun, 7 Sep 86 14:57:41 PDT 
To: WRS 
Subject: "BILL, WHO IS THE RUNOFF EXPERT?"... 



BILL,

WHO IS THE RUNOFF EXPERT?  

HOW DO YOU MAKE PICTURES IN RUNOFF?  IN PUB YOU CAN USE
.BEGIN VERBATIM

PICTURE HERE

.END


HOW DO YOU DO THE EQUIVALENT IN RUNOFF?


THANKS,

KEVIN
----------------------------------------------------------------
(end forwarded message)

Any RUNOFF experts?  Please reply to Kevin.  Thanks -Bil
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 8 Sep 86 14:27:12 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: (MONDOC)TRWJ2P.DOC 

The file (MONDOC:930)TRWJ2P.DOC is a technical document listing all the
pertinent changes that affect 2020s in particular (some KI/KL/F3 items
were removed) from the /J monitor thru /P

I did NOT get to writing somethng in english... Dennis, please look it
over and ask Joe if you have any questions...  feel free to write an english
translation or overview, otherwise I will attend to it when i return next
wednesday.  Joe, feel free to assist Dennis with explanations of TRWJ2P
and let him know what need to be mentioned for 2020s... since much of the
technical stuff will be meaningless to TRW.

See you later.

/Car
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 8 Sep 86 14:32:31 PDT 
To: Craig Fletcher <fletcherc@X930.Tymnet>, Lois Frost
	<FSC.L/Frost@Ontyme.Tymnet> 
Subject: TOPS-10 Monitor Internals for 7.03 

Craig,

  EY-5268E-LO  TOPS-10 Monitor Internals for 7.03
  is 10 days, scheduled for 20-Oct-86

  There is space remaining for 10 people at the moment.

  To register, the following is required:
  1) Student Name     ;   Carl A Baltrunas
  2) Company Name     ;   McDonnell Douglas Field Service Company
  3) Mailing Address  ;   39100 Liberty Street
     for confirmation ;   Fremont, CA  94538

  4) if paying with purchase order, the PO number and a copy of the PO
     to be sent prior to the class start date, the company will be billed
     after the class is complete.
     if paying with a company check, the check should be received prior
     to the class start date.

  I presume we will be paying with a PO, so I could not register myself
  today.  Please initiate whatever needs to be done to get a PO number
  and we can call them and register me then.

  I did not verify the price, $2600, and I suggest we do that since the
  schedule was apparently changed since the last catalogue printing, the
  price may also have changed.

---------------------------
PS.
  I will try to give Craig a call on Tuesday.

/Car
Received: from X62.Tymnet by X930.Tymnet; Mon, 8 Sep 86 16:15:10 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Mon, 8 Sep 86 14:41:41 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: (MONDOC)TRWJ2P.DOC 
In-reply-to: your message of Mon, 8 Sep 86 14:27:12 PDT

THANKS!!   /D
From: CARL@X930.Tymnet 
Date: Fri, 12 Sep 86 3:48:36 PDT 
To: osman 
Subject: "Re: System 62 OOO OO OO OO OO"... 

Re: System 62

  OOO
OO   OO
OO   OO
OO   OO
  OOO   ,    "BIG O" ...

Hi...  I was on 62 when it "went away".  According to the network, both
2770 and 10115 went out of the network, and thus access to 62 went away.
micronode 2346 is also down, needs to be re-booted?  so cannot get to 62
from 930.

  anyway, please send me a mail message and let me know what happenned on
the console around 3:30-4:00am on Friday morning.

  if I was working, I'd have come into the office to find out myself...

PS.  could you also pick up my paycheck and bring it to the airport with the
     tickets, so I can give it to Cherie to cash while I'm in Irvine.

Thanks much...  see you Sunday night...

/Car
From: JMS@X930.Tymnet 
Date: Fri, 12 Sep 86 23:21:48 PDT 
To: carl 
Subject: "CARDS.SAI and MAIN01.VUE Could"... 

CARDS.SAI and MAIN01.VUE

Could not resync with tymbase.  Did not even after reload.  Saved
CARDS.SAI and MAIN01.VUE to 930.  /Jo
From: CARL@X930.Tymnet 
Date: Sat, 13 Sep 86 0:38:16 PDT 
To: jms 
Subject: "Re: CARDS.SAI Thanks... see you"... 

Re: CARDS.SAI

Thanks... see you wednesday.

PS> Osman is going to ask Craig about having our meeting on Thursday
    to give us a day back to see what's happenning... we'll see what
    happens.

Thanks again.
/Car
From: CARL@X930.Tymnet 
Date: Sun, 14 Sep 86 3:18:26 PDT 
To: jms, dencoff 
Subject: "Re: Machine room.... In the hopes"... 

Re: Machine room....

In the hopes of working on my little toy program before going off to
Irvine, I came in to the office to find out what was wrong with 930
and 62.   As I found them, both 6657 and 10115 were up and running
fine.  62 was running fine, but not talking to 10115.  I tried reloading
both 62 and 10115, and still they would not talk.

I got 2346 to reload on the first try, no problems... and reloaded
10115 thru it from 930.

Node 3146 looks like they put the wrong code in it again... the
clock is 3x normal speed so it isn't talking to 6657.  I moved the
cable using the pass/cross cable from the terminal in the CO-OPs
room to 3143 and it seems to be talking to 3143 just fine.  10115
is also just fine talking to 3143.

At the moment, 3143 isn't talking to the rest of TYMNET... no idea
why.  Will call network control before I go home.

Anyway, please put the cable back on 3146 when the node is fixed...
(or just leave it on 3143...)

I powered :62 off since it would not synch and it kept getting UIL
stopcodes... the temperature in the machine room was pretty warm, 
although I didn't look at the thermometr...  I suspect memory is
either still bad or bad again... feel free to run the memory diags
on the foonly memtst tape.

See you all on wednesday.
/Car
From: CARL@X930.Tymnet 
Date: Sun, 14 Sep 86 3:32:04 PDT 
To: jms, dencoff 
Subject: "re: 3143 not talking... it is"... 

re: 3143 not talking...

it is dead-ended off nodes in solar way, and solar way is scheduled
with a power recabling etc, till 6am...

i told them about 3146 3x, and they said they'd pass it along to
dave malmberg... they = network control.

Ciao.
/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 14 Sep 86 23:20:44 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: RX02 floppies on the 2020. 

I found an unsupported program that allows a KS-2020 running TOPS-10 7.03
to read and write 8-inch floppies.  It might be worth it to get an RX02
floppy disk drive and connected it to system 95.  The we could take the KL
BOTLOD program, transfer it to the 2020 via magtape, write it on the floppy,
take the floppy to the data center, and copy it to KLDCP DECtape.

The files are currently in (DEC:930)RX20.INS,RX20.DOC,RTFLX.HLP,RX2SER.MAC
/Jo
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 16 Sep 86 14:48:21 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: My beeper doesn't beep! 

My unit was paged 10 times last night.  It recorded the numbers, but made no
noise.  It is silent when I turn it on.  That's why I did not respond to the
Hotline call from Cup-C at 10:30pm 15-Sep.

It was about C26 getting MAPAL1 stopcodes everytime they try to do an all-files.
I have solved the BADSAT/MAPAL1 problem - see next message.  /Jo
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 16 Sep 86 14:53:02 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: BADSAT/MAPAL1 problem located. 

When a bad SAT is detected, the Monitor sets some flags so that it will not
use any of the 9216 pages the SAT refers to.  The system continues, and will
request a full DSKCLN next time to rebuild the SAT from scratch.
There are two ways a SAT can be marked bad.  If there is a read error, then
the count of bad SAT reads in SATBRD is incremented.  If the count from SATCNT
is not correct, then T4 does not match STTFPC.  In all the crashes I looked at,
SATBRD was zero, as was STRDDB+1, which should have pointed to BPA0CB.

Before P034/P21         After P034/P21       ;3/6/86
ADDM T1,UNITAL(U)       ADDM T1,UNITAL(U)    ;Reduce available pages on unit
                        HRRZ T2,UNISTR(U)    ;Point to right STRDDB
ADDM T1,STRTAL+STRDDB   ADDM T1,STRTAL(T3)   ;Reduce avail pages on structure
SETZM STTFPC(T2)        SETZM STTFPC(T2)     ;Zero free page count for SAT

That should have been "HRRZ T3,UNISTR(U)".  The typo T2 to be changed, STRDDB+1
to be zeroed, and MAPAL1 stopcode when the next user got a page fault.
I have patched SYSTEM and NEWMON to be C26-P034/P33-6 and C26-P034/P34-1.

                        /Jo
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 16 Sep 86 21:06:06 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Bravo! MAPAL1 

Joe,
 it looks good to me... but whay didn't it happen a lot more? and not on
930 or 62?  any ideas?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 16 Sep 86 21:09:47 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: On call... 

I am on call as of midnight... (or now, since joe's Beeper isn't making any
noise at the moment.)  I am home and have my beeper ON.

/Car
From: Osman Guven <osman@X930.Tymnet> 
Date: Tue, 16 Sep 86 21:59:42 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Monitor Group <CARL@X930>, Joe Smith <jms@X930.Tymnet>, Craig Fletcher
	<fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet> 
Subject: Re: Bravo! MAPAL1 
In-reply-to: your message of Tue, 16 Sep 86 21:06:06 PDT

ATTA BOY....Joe PH. Smith..
-Osman
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 18 Sep 86 7:33:34 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: CUPC called me at home. 

C70 had MISSING OR INCONSISTANT PACK-SET ID.  L.T. Smith tried calling
(415)495-3891 without success (the number is 3841).  He tried the TYMCOM-X
Hotline at (415)794-2567 but got no answer after 6 rings.  He did have my
home phone number, left over from Tuesday night.  I fixed the PACK-SET ID
problem over the phone, then told the right beeper number and warned them
about the amount of time it takes our phone system to forward to South Bay.

Craig, we need to send out an updated memo so that all three datacenters will
use the same procedure.  Also, who do we call about getting my beeper fixed
before Sep 29th?		/Jo
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 18 Sep 86 9:57:13 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: My beeper is getting false alarms 

If the answering service is calling me, they also need to be re-informed
about how to ring our beepers.

/car
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 18 Sep 86 10:38:15 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: I sent false alarm. 

At 7:05am today I was testing to make sure the number is 3841 not 3891.
I was not fully awake, and the pager timed out before I could put in a dummy
number.  That's why the beepers said "-----1-----" this morning.  /Jo
Received: from X62.Tymnet by X930.Tymnet; Thu, 18 Sep 86 13:39:31 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 18 Sep 86 13:34:12 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: 2020 reminder 

This is a reminder that the monitor work to standardize the home, bad and
boot pages across all hardware types should be completed.  Joe has worked
on a newer boots for each type, so that should be done.

For the KS, the monitor tools, KSFEFS and friends, are still setup to use
the old page asignments and need to be checked, changed and/or tested to
insure that we do not have any conflicts.

The changes were made to the monitor at the same time SPAGES.SYS was put
into the sources.  I know what I've done, so whomever gets the task of
figuring out where things are, which are done and which are not should
talk to me when they get around to working on this.

BEWARE: ** Until this new set of utilities are working, the new monitor
           cannot be released for 2020 processors. **
/Carl
Received: from X62.Tymnet by X930.Tymnet; Thu, 18 Sep 86 19:08:13 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 18 Sep 86 19:01:10 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: "Bill, I am about ready to release"... 

Bill,
  I am about ready to release a "beta-test" version of my new XEXEC job
controller to system B36 beginning on Monday 22-Sep-86.  I don't have a
lot of documentation ready at the moment, and the user interface still
needs a little work before it will satisfy me (it is workable, but can
be used without any problems, except for some fancy option combinations
which were implemented incorrectly).

  It will completely supersede your XEXEC, and I need a guarantee that
RMURPHY will not be superseding it with ROLAID in the future.  It is
dependent on /P34 and later monitors...

  Give me a call (afternoon) if oyu can to talk about putting it up on
system 36.  When system 39 gets the new monitor, we can put XEXEC up on
that system too.

/Car
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 19 Sep 86 11:45:23 PDT
From: OPERA.B/DONG@Ontyme.Tymnet 
Date: F30 TIME RESET 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: OPERA.BATCH@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet,
	IPC.S/MARCOTTE@Ontyme.Tymnet, IPC.M/BELLOPATRICK@Ontyme.Tymnet,
	IPC.J/WALLACE@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.J/SMITH@Ontyme.Tymnet 
Message-id: A41514@Ontyme.Tymnet 
Subject: "TO: TYMCOM-10 SUPPORT FROM: BRAD"... 

TO:  TYMCOM-10 SUPPORT
FROM:  BRAD DONG/FRMT OPS

SYSTEM:  F30/BBB
APR:  1388

THE SYSTEM TIME HAS HAD TO RESET 4 TIMES IN THE LAST TWO DAYS.
THE FIRST 3 TIMES , SYSTEM TIME WAS AHEAD APPROX 30-40 MIN.
THIS MORNINGS TIME RESET SYSTEM WAS BEHIND 1HR 56MIN.  
NOTE:  F30 ANSWERED THIS MORNING FROM ASP/PM 52 MIN BEHIND
CURRENT TIME.

IS THERE ANYTHING YOU CAN DO
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 21 Sep 86 2:23:50 PDT 
To: CARL 
Subject: KERMIT in ALGOL. 

The file (JMS)B78KER.ALG has the version of KERMIT in ALGOL for the
Burroughs 7800.  Do you suppose it would be worth it to convert this to SAIL?
/Jo
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Sun, 21 Sep 86 23:48:58 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 22:15:34 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 21:15:31 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 20:15:29 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 19:15:31 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 18:15:32 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 17:15:32 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 16:15:29 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 15:15:29 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 14:15:38 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 13:15:30 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 12:15:26 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 11:15:28 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 10:15:26 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 9:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 8:15:30 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 7:15:26 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 6:15:30 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 5:15:30 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 4:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 3:15:30 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 2:15:31 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 1:15:29 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sun, 21 Sep 86 0:15:47 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 23:15:38 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 22:15:28 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 21:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 20:15:25 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 19:15:25 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 18:15:28 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 17:15:29 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 16:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 15:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 14:15:25 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 13:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 12:15:24 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 11:15:25 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 10:15:24 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 9:15:24 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 8:15:29 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 7:15:24 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 6:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 5:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 4:15:25 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 3:15:26 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 2:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 1:15:23 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Sat, 20 Sep 86 0:15:38 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Fri, 19 Sep 86 23:15:26 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Fri, 19 Sep 86 22:15:24 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Fri, 19 Sep 86 21:45:29 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Fri, 19 Sep 86 21:15:23 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Fri, 19 Sep 86 20:15:27 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Fri, 19 Sep 86 19:45:19 PDT
Received: from X930.Tymnet by EMSTXS.Ontyme.Tymnet; Fri, 19 Sep 86 19:22:56 PDT
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 19 Sep 86 19:16:38 PDT 
To: OPERA.B/DONG@Ontyme.Tymnet 
Cc: TXS.J/SMITH@Ontyme.Tymnet, OPERA.BATCH@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet, IPC.S/MARCOTTE@Ontyme.Tymnet,
	IPC.M/BELLOPATRICK@Ontyme.Tymnet, IPC.J/WALLACE@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.J/SMITH@Ontyme.Tymnet 
Subject: Re: "TO: TYMCOM-10 SUPPORT FROM: BRAD"... 
In-reply-to: A41514@Ontyme.Tymnet of F30 TIME RESET

I have built a new monitor for F30 and copied it to NEWMON.  This monitor will
adjust the time-of-day whenever it gets the date/time from the Supervisor.
The adjustment is gradual so that accounting files will not be messed up
by a sudden change.  There will be an EVENT stopcode "TIMEOK" whenever the
time is adjusted by more than 60 seconds.  At the end of the gradual adjustment,
another message will come out on the CTY stating what the date/time has been
set to.

The time will be adjusted automatically at least once a day, when the new
Supervisor takes over the network.  It will also be changed whenever the
system changes from being SHUT to UNSHUT.  Therefore you can cause an
immediate adjustment by simply typing "SCHED 400000" then "SCHED 0".

                        /JO
Received: from X62.Tymnet by X930.Tymnet; Mon, 22 Sep 86 9:25:08 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Mon, 22 Sep 86 9:07:51 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Your help/editing with TRWJ2P.DOC. 

Carl,

   Thanks!!  I've included all your recommended changes.

   Q.:  re. your added info that LOGOUT ON STOP may be set at Login by
turning on the appropriate bit in the LUD:  how is this bit turned on?  Since
the audience for this is not DEC systems people, quick reference to the
program or mechanism which gets the bit turned on would be very helpful.

                                                                Thanks,
                                                                D
From: FLETCHERC@X930.Tymnet 
Date: Mon, 22 Sep 86 13:59:43 PDT 
To: jms, carl, osman, dencoff 
Subject: "Subj: 1987 goals Because of a"... 

Subj: 1987 goals

Because of a conflict I would like to reschedule the goals meeting from
1;00 p.m. Friday September 26, to 10:00 a.m. Friday September 26
in classroom 2.

	CRai
From: CARL@X930.Tymnet 
Date: Mon, 22 Sep 86 17:24:07 PDT 
To: wrs 
Subject: "FILDDT"... 

? FILDDT 
Received: from X62.Tymnet by X930.Tymnet; Tue, 23 Sep 86 16:09:14 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Tue, 23 Sep 86 15:56:46 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Job Processor Documentation. 

Received: from X62.Tymnet by X930.Tymnet; Tue, 23 Sep 86 16:09:17 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Tue, 23 Sep 86 16:06:34 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Job Processor Documentation. 

Carl,
  Please look at the following;  it's my typed copy of what you had written on
my board.  Any updates or needed changes would be appreciated.
						Thanks,  /D.

-------------------------------------------------------------------------------

			     Job Processor System

I.)  What it does for the user.

II.)  How to use it:

	A.)  Commands to get info about a job (a request):
		1.)  STATUS	[list of what it gives]
		2.)  LIST	[list of what it gives]
		3.)  SHOW	[list of what it gives]

	B.)  Commands to enter a job (a request):
		1.)  Specifying when  (see D.)
		2.)  INSERT.  INSERT Parameters:
			a.)  Logging action	[values]
			b.)  Dispose of log file	[values]
			c.)  Mail	[values]
			d.)  Restart	[values]
			e.)  Unique	[values]
			f.)  Priority	[values]
			g.)  CCL	[values]
			h.)  Halt	[values]
			i.)  Run	[values]
			j.)  Detach	[values]
			k.)  Time limit	[values]
			l.)  TRU limit	[values]
			m.)  Start label	[values]
			n.)  Logfile name	[values]

	C.)  Housekeeping commands:
		1.)  MODIFY		
		2.)  DELETE (CANCEL)	
		3.)  UNDELETE		
		4.)  KILL		
		     PAUSE		
		     STOP		
		     CONTINUE		
		5.)  REENTER		
		6.)  REQUEUE		

	D.)  Specifying when:
		1.)  Date & time:
			a.)  Specific date & time
			b.)  Day of week
			c.)  day of month  (n months at Day)
		2.)  Repetition interval; interval of:
			a.)  months
			b.)  days
			c.)  hours
			d.)  minutes
		3.)  Combinations of these two
		4.)  Modifiers to above:
			a.)  AFTER
			b.)  BEFORE
			c.)  AND
			d.)  EXCEPT
			e.)  EVERY
		4.)  Special:
			a.)  STARTUP
			b.)  EVERY STARTUP
			c.)  SYSTEM STARTU
Received: from X62.Tymnet by X930.Tymnet; Wed, 24 Sep 86 13:29:00 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 24 Sep 86 13:26:46 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: schedule 

I worked till 3am this morning, and since the guy working on my car
yesterday didn't finish (he supposed to finish today), I shall be
working at home this afternoon.

Iff you need to talk to me, call me.  408-945-4314.  See you all
tomorrow.

Dennis, your outline looks fine.

/Car
Received: from X62.Tymnet by X930.Tymnet; Thu, 25 Sep 86 3:49:51 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 25 Sep 86 3:48:58 PDT 
To: jms, osman, dencoff 
Subject: "re: I killed all jobs in the QUEUE"... 

re: I killed all jobs in the QUEUE to put up a new version.

Please re-enter your jobs on system 62.

/Car
From: FLETCHERC@X930.Tymnet 
Date: Thu, 25 Sep 86 11:40:47 PDT 
To: carl 
Subject: "Osman and I talked with Ed Roop"... 

Osman and I talked with Ed Roop and Bill Fischer yesterday.  Ed wondered
when the PERP replacement would be under test as he would like to try
removing one PERp controller by mid October and have the new version on
the affected -10's by then.

Will you be sure to contact the right people on 36 or 39 to see that we
can begin testing by 27 september.

	Crai
Received: from B39.Tymnet by X930.Tymnet; Thu, 25 Sep 86 12:13:01 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 25 Sep 86 12:05:03 PDT 
To: CARL@X930.Tymnet 
Subject: Re: "FILDDT"... 
In-reply-to: your message of Mon, 22 Sep 86 17:24:07 PDT

----------------------------------------------------------------
Received: from X930.Tymnet by B39.Tymnet; Mon, 22 Sep 86 17:24:08 PDT
Return-path: <CARL@X930.Tymnet> 
From: CARL@X930.Tymnet 
Date: Mon, 22 Sep 86 17:24:07 PDT 
To: wrs 
Subject: "FILDDT"... 

? FILDDT ?
----------------------------------------------------------------
...reference too obscure to decipher.
(sorry, behine a week in mail
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 25 Sep 86 13:18:03 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: I will be out sick today. 

I am not feeling well, and will not be in today.  I will make it to the 10am
meeting tomorrow.  /Jo
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 18 Sep 86 13:45:22 PDT
From: IPC.L/MORRIS@Ontyme.Tymnet 
Date: CUPERTINO SHUTDOWN 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A40921@Ontyme.Tymnet 
Subject: CUPERTINO DATA CENTER SHUTDOWN 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 26 Sep 86 21:35:25 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 

     
     
     
                           M E M O R A N D U M    
     
     
     ATTN:  THE WORLD    
     
     FROM:  LANDA MORRIS 
            RESOURCE PLANNING & MANAGEMENT   
     
     SUBJ:  CUPERTINO DATA CENTER SHUTDOWN   
     
-------------------------------------------------------------------------  
     
     IPC IS PLANNING TO SHUTDOWN THE CUPERTINO DATA CENTER BETWEEN NOW
AND JUNE 1987.  ALL SYSTEMS AT THE CENTER WILL BE DISTRIBUTED EITHER TO    
THE FREMONT OR DALLAS CENTER. 
     
     EACH SYSTEM'S USER LOAD WILL BE TRANSFERRED AS A SINGLE UNIT, RATHER  
THAN BY INDIVIDUAL CUSTOMER MOVES.  THIS WILL KEEP THE SYSTEM MOVE TRANS-  
PARENT TO THE USER, WITH LITTLE EFFECT.  MOST OF THE MOVES WILL OCCUR 
DURING WEEKENDS.    
     
     THE CHART BELOW SHOWS THE CUPERTINO HOST, IT'S USER LOAD DESTINATION  
AND THE TENTATIVE MOVE DATE:  
     
     
     HOST           DESTINATION                 DATE   
     ----           -----------                 ----   
     
     C-22             FREMONT                   11-21-86    
     
     C-26             FREMONT                   10-30-86    
     
     C-29             FREMONT                   2-13-87
     
     C-33             FREMONT                   12-26-86    
     
     C-34             DALLAS                    10-3-86
     
     C-35             DALLAS                    11-28-86    
     
     C-36             DALLAS                    4-17-87
     
     C-39             DALLAS                    3-13-87
     
     C-57             DALLAS                    12-5-86
     
     C-70             FREMONT                   1-16-87
     
     
     SOME DELAYS MAY OCCUR IN THIS TIMETABLE AND IF SO, AN UPDATED    
CHART WILL BE ISSUED.    
     
     RPM WILL NOTIFY EVERYONE VIA ONTYME AND SYSTEMS MESSAGES PRIOR TO
EACH SYSTEM MOVE.   
     
     IF YOU HAVE ANY QUESTIONS PLEASE CONTACT RESOURCES AT (408)446-6315   
OR ONTYME IPC.SRAMAIL.   
     
     
THANK YOU,
LANDA/RP
From: TXSSUP@X930.Tymnet 
Date: Fri, 26 Sep 86 21:35:39 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: ""... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 26 Sep 86 21:35:33 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 

Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 25 Sep 86 15:44:38 PDT
From: IPC.L/MORRIS@Ontyme.Tymnet 
Date: CUPERTINO SHUTDOWN 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A44561@Ontyme.Tymnet 
Subject: CUPERTINO DATA CENTER SHUTDOWN 

     
     
     
                           M E M O R A N D U M    
     
     
     ATTN:  THE WORLD    
     
     FROM:  LANDA MORRIS 
            RESOURCE PLANNING & MANAGEMENT   
     
     SUBJ:  CUPERTINO DATA CENTER SHUTDOWN   
     
-------------------------------------------------------------------------  
     
     BELOW IS AN UPDATED COPY OF THE CUPERTINO SHUTDOWN SCHEDULE.  IT IS   
IN REFERENCE TO THE ONTYME SENT 9/19/86.
     
     AS MENTIONED BEFORE, THE SYSTEMS WILL HAVE THEIR USERS MOVED AS A
SINGLE UNIT, RATHER THAN BY INDIVIDUAL CUSTOMER MOVES AND MOST WILL OCCUR  
DURING WEEKENDS.    
     
     THE CHART BELOW SHOWS THE CUPERTINO HOST, IT'S USER LOAD DESTINATION  
AND THE TENTATIVE MOVE DATE:  
     
     
     HOST           DESTINATION                 DATE   
     ----           -----------                 ----   
     
     C-22             FREMONT                   11-21-86    
     
     C-26             FREMONT                   10-30-86    
     
     C-29             FREMONT                   2-13-87
     
     C-33             FREMONT                   12-26-86    
     
     C-34             DALLAS                    10-3-86
     
     C-35             DALLAS                    11-28-86    
     
     C-36             DALLAS                    4-17-87
     
     C-39             DALLAS                    3-13-87
     
     C-55             DALLAS                    1-9-87 
     
     C-57             DALLAS                    12-5-86
     
     C-70             FREMONT                   1-16-87
     
     
     SOME DELAYS MAY OCCUR IN THIS TIMETABLE AND IF SO, AN UPDATED    
CHART WILL BE ISSUED.    
     
     RPM WILL NOTIFY EVERYONE VIA ONTYME AND SYSTEMS MESSAGES PRIOR TO
EACH SYSTEM MOVE.   
     
     IF YOU HAVE ANY QUESTIONS PLEASE CONTACT RESOURCES AT (408)446-6315   
OR ONTYME IPC.SRAMAIL.   
     
     
THANK YOU,
LANDA/R
From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Date: Fri, 26 Sep 86 21:37:51 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: "MDFSC, MDCSC, MDISI ISG LISTS"... 

(begin forwarded message)
----------------------------------------------------------------
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 23 Sep 86 11:45:54 PDT
From: ISGHQ.C/BANDLE@Ontyme.Tymnet 
Date: Tue, 23 Sep 86 11:45:54 PDT 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J89749@Ontyme.Tymnet 
Subject: "MDFSC, MDCSC, MDISI ISG LISTS"... 

MDFSC, MDCSC, MDISI & ISG LISTS A-E

PROMOTION   OF   CHRIS   SPAMPINATO  AS  MANAGER,  TECHNICAL  SUPPORT
ORGANIZATION

It  is  my  pleasure to announce the promotion of Chris Spampinato to
the  position  of Manager, Technical Support Organization-Irvine, for
the  Field  Service  Company,  reporting  directly  to  me  effective
immediately.

Due  to  the  recent  structural  change  of  the  Technical  Support
Organization,  Chris  assumes  the  responsibility  for  the  product
support departments within the Irvine location.

Chris  will focus on providing product and technical support for both
hardware  and  software.  The  major thrust will be in support of CSC
products,  but  will  also  include  third  party product evaluation,
excluding DEC products.

Chris's  previous  management  responsibilities  included  Department
Manager,  Technical  Support,  and  Manager, Marketing Field Support.
Chris joined the Company in 1979 as a Technical Support Analyst.

Please join me in wishing Chris success in her new assignment.

William H. Kersten
Vice President
Technical Operations
McDonnell Douglas Field Service Company

CONCURRENCE:

Vern H. Hart
President
McDonnell Douglas Field Service Company
----------------------------------------------------------------
(end forwarded message)

Craig,
   What does this mean?  Do you still report to Kersten?  ...or are
you the "unofficial" T.S.O.-Fremont manager?  Or is our new group name
mentioned in today's meeting mean we are now a different group...

I'm confused.

/CarReceived: from X62.Tymnet by X930.Tymnet; Sun, 28 Sep 86 4:27:28 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sun, 28 Sep 86 4:26:35 PDT 
To: dernst@b39 
Subject: "we are experiencing data lossage"... 

we are experiencing data lossage again.  Probably after some stopcode
but I thought that you were trying to fix that problem?

/Carl
Received: from X62.Tymnet by X930.Tymnet; Sun, 28 Sep 86 5:15:18 PDT
Return-path: <MAIL@X62.Tymnet> 
From: Carl Baltrunas <Stupid-Test@Imagination.Tymnet> 
Sender: MAIL@X62.Tymnet 
Date: Fri, 01 jan 95 14:92:15 zzz 
To: Carl@X62, JMS@X62 
Subject: whatever 

This si a foolish messge built by hand to test what is necessary
to send mail via the XEXEC/EXECX stuff I'm working on.

/Carl
Received: from B39.Tymnet by X930.Tymnet; Tue, 30 Sep 86 6:24:09 PDT
Return-path: <CARL@B39.Tymnet> 
From: CARL@B39.Tymnet 
Date: Tue, 30 Sep 86 6:16:31 PDT 
To: Bill Soley <WRS@B39> 
Subject: "Bill, FREE is NOT running on system"... 

Bill,
  *** FREE is NOT running on system 36 ***

You have a *BUG* in either ARGPAR or ARGS!  When reading commands from
a command file (at CCL entry) if there is a CRLF at the end of the line
your code dies;  possibly because it includes some part of the crlf in
the string name it is trying to match against a PNAME?

If I construct a ???FRE.TMP file by hand without the crlf, FREE works
fine at CCL entry.  Apparently, you are unaware that the CRLF is tobe
expected...

Also, I looked at (WRS:39)ARGPAR.SAI  and noticed that it does NOT have
the changes I made to it in my directory on 930 (or 62?) about various
item array types.  I may even have fixed the CRLF problem, I do not
really remember anymore what else I fixed ( I know I mentioned it to you
at the time ).

Anyway, until ARGPAR/ARGS is fixed, FREE will NOT run under the new XEXEC
since parameters passed to a program in a TMP file always have a CRLF on
the end of the line.
/Carl


*** please respond ***
From: CARL@X930.Tymnet 
Date: Tue, 30 Sep 86 6:37:31 PDT 
To: jms 
Subject: "Subject: CR or no CR On TMPCOR"... 

Subject: CR or no CR

On "standard" TMPCOR command files, does a  CR LF  pair get tacked on
the end of the line?

SCAN seems to accept either format... Bill Soley's ARGS/ARGPAR routines
do not accept the CRLF (in fact it barfs pretty badly).

SUBMIT puts a CRLF into the TMP file it creates, and at the moment does
not have an option to [omit] it.  Helge & I talked about this and I decided
that the CRLF was expected, either by testing some TYMCOM-X programs or
some DEC programs... I forget which...

Anyway, do you know if there is a standard policy on this?
/Carl
From: CARL@X930.Tymnet 
Date: Tue, 30 Sep 86 6:45:06 PDT 
To: wrs@b39 
Subject: "Re: CRLF on TMPCOR files The in"... 

Re: CRLF on TMPCOR files

The *BUG* in free is in ARGS!  When it reads in ???xxx.TMP, it reads the
line as a bunch of tokens, but doesn't strip off any CRLF that might be
there even though it eliminates whitespace and stuff like that...

I may put an option in SUBMIT to NOT put a CRLF in the TMP file, but this
won't help B36 to run FREE today...  (re: TELECOPY via operator batch
requests seem to take awhile, and that will be after I make the changes
and test them; sigh),

It'll be easier to fix ARGS.SAI!
/Carl
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 29 Sep 86 12:41:45 PDT
From: IPC.SRAMAIL@Ontyme.Tymnet 
Date: SYSTEM C6 SHUTDOWN 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A46054@Ontyme.Tymnet 
Subject: SYSTEM C6 CLOSING PERMANENTLY 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Tue, 30 Sep 86 12:19:09 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 

                         M E M O R A N D U M





DATE>      29 SEP 86  12:23

TO>        ALL CONCERNED

COPIES>    

FROM>      DEBBIE BURNS
           RESOURCE PLANNING & MANAGEMENT


SUBJECT>   SYSTEM C6 CLOSING PERMANENTLY


-----------------------------------------------------------------------


     PLEASE BE AWARE THAT AS OF OCTOBER 3, 1986 AT 2400 (PST)
SYSTEM C6 WILL NO LONGER BE AVAILABLE FOR TIMESHARING.

     PLEASE TRANSFER ALL FILES AND TAPES YOU CURRENTLY HAVE
ON SYSTEM C6 TO ANOTHER HOST BEFORE 10/03/86.

     ALL INTERNAL USERS WILL BE REMOVED FROM C6 ON 10/03/86
AT 4:00 PM (PST).

     IF YOU HAVE ANY QUESTIONS OR REQUIRE ALTERNATE RESOURCES
PLEASE CALL ME AT (408)446-6629 OR SEND AN ONTYME TO IPC.SRAMAIL.

THANK YOU.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 30 Sep 86 8:42:51 PDT
From: IPC.SUP@Ontyme.Tymnet 
Date: Tue, 30 Sep 86 8:42:52 PDT 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A46491@Ontyme.Tymnet 
Subject: CENTER POWER DOWN 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Tue, 30 Sep 86 12:19:13 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 

DATE: 26 SEPT 1986 12:30                                                        
                                                                                
TO: THE WORLD                                                                   
                                                                                
FROM: DALLAS COMPUTER CENTER (IPC)                                              
                                                                                
SUBJECT: CENTER POWER DOWN                                                      
-----------------------------------------------------------------------         
                                                                                
   The DALLAS DATA CENTER will experience a complete BUILDING POWER SHUT        
   DOWN on  October 11,  1986 begining at 2300 (cdt) until approximately        
   0700 (cdt) October 12,  1986.  At this time, we will be improving our        
   electrical equipment to the building and computer floor. The following       
   systems affected in the powerdown are:                                       
                                                                                
    41, 43, 47, 49, 82; 23, 25, 27, 31, 34, 37, 54,                             
    56, 65, 72, 79; 1596, 2588; and all TTMS systems.                           
                                                                                
                                                                                
    Please adjust your processing schedule as necessary.                        
                                                                                
                                                                                
    Thank you                                                                   
    Dallas Computer Operations; <MDCISG IPC>                                    
    (214)637-7435 or Ontyme IPC.DCO
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 30 Sep 86 8:43:16 PDT
From: IPC.SUP@Ontyme.Tymnet 
Date: Cupertino Shutdown 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A46498@Ontyme.Tymnet 
Subject: CUPERTINO DATA CENTER SHUTDOWN 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Tue, 30 Sep 86 12:19:17 PDT 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet> 

     
     
     
                           M E M O R A N D U M    
     
     
     ATTN:  THE WORLD    
     
     FROM:  LANDA MORRIS 
            RESOURCE PLANNING & MANAGEMENT   
     
     SUBJ:  CUPERTINO DATA CENTER SHUTDOWN   
     
-------------------------------------------------------------------------  
     
     BELOW IS AN UPDATED COPY OF THE CUPERTINO SHUTDOWN SCHEDULE.  IT IS   
IN REFERENCE TO THE ONTYME SENT 9/19/86.
     
     AS MENTIONED BEFORE, THE SYSTEMS WILL HAVE THEIR USERS MOVED AS A
SINGLE UNIT, RATHER THAN BY INDIVIDUAL CUSTOMER MOVES AND MOST WILL OCCUR  
DURING WEEKENDS.    
     
     THE CHART BELOW SHOWS THE CUPERTINO HOST, IT'S USER LOAD DESTINATION  
AND THE TENTATIVE MOVE DATE:  
     
     
     HOST           DESTINATION                 DATE   
     ----           -----------                 ----   
     
     C-22             FREMONT                   11-21-86    
     
     C-26             FREMONT                   10-30-86    
     
     C-29             FREMONT                   2-13-87
     
     C-33             FREMONT                   12-26-86    
     
     C-34             DALLAS                    10-3-86
     
     C-35             DALLAS                    11-28-86    
     
     C-36             DALLAS                    4-17-87
     
     C-39             DALLAS                    3-13-87
     
     C-55             DALLAS                    1-9-87 
     
     C-57             DALLAS                    12-5-86
     
     C-70             FREMONT                   1-16-87
     
     
     SOME DELAYS MAY OCCUR IN THIS TIMETABLE AND IF SO, AN UPDATED    
CHART WILL BE ISSUED.    
     
     RPM WILL NOTIFY EVERYONE VIA ONTYME AND SYSTEMS MESSAGES PRIOR TO
EACH SYSTEM MOVE.   
     
     IF YOU HAVE ANY QUESTIONS PLEASE CONTACT RESOURCES AT (408)446-6315   
OR ONTYME IPC.SRAMAIL.   
     
     
THANK YOU,
LANDA/RPM
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 30 Sep 86 15:44:19 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Can't login over shut to Fayez. 

After asking Fayez-Sarofim to bring up the system using KMCMAN, I was unable
to login.  I received the following:
        please log in: jms:370;
        shut ^G
        host shut
        please log in:
They must be running a version of LOGINN that does not allow login over shut.

This is the procedure they use to bring up the system normally:
        ^\                      Control-backslash
        KS10>ZM                 Zero memory
        KS10>BT                 Get BOOTS
        BOOTS:/S4               Set sense-switch 4
        BOOTS:CONSOL            Load SYS:CONSOL.SAV
        >CONS                   Go into CONSOLE mode
        please log in: ^Z L OPER
        .O GOD                  Get all license
        !R NODLOD               Reload the KMC
        !R RELOAD               This zeros sense-switch 4 and loads SYSTEM.SAV
        BOOTS:(SYS)SYSTEM.SAV   This is automatic as a result of RELOAD.SAV
        DSKCLN runs

At one time, the only way you could run NODLOD on a 2020 was by running a
special monitor (SYS)CONSOL.SAV that had the CIOPR uuo implemented.  I don't
know if KMCLOD.SAV will run on H370-P034/J-5.  The version of INITIA they have
does not attempt to run KMCLOD.  Note that if the operator is unable to set
license, then he can't run NODLOD or RELOAD, therefor DSKCLN does not get run.

The problem started when the disk got clobbered, and the operator restored all
files from a backup tape after using "O GOD" instead of REFLAG.  They are
restoring all files now.                /Joe
Received: from B39.Tymnet by X930.Tymnet; Tue, 30 Sep 86 15:56:03 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 30 Sep 86 15:17:59 PDT 
To: CARL@B39.Tymnet 
Subject: Re: "Bill, FREE is NOT running on system"... 
In-reply-to: your message of Tue, 30 Sep 86 6:16:31 PDT

Oh well.  That's okay about FREE - I'll get to it some day.  Does your
ARG### have all my stuff in it or have we diverged?  -Bill
From: CARL@X930.Tymnet 
Date: Tue, 30 Sep 86 16:28:42 PDT 
To: wrs@b39 
Subject: "My ARGS.SAI only defines the size"... 

My ARGS.SAI only defines the size of the array to the same as in ARGPAR.
Otherwise it is the same, and has the same bug.  I will fix the bug and let
you know...

My ARGPAR.SAI does all that yours does, and allows a different breakset for
arguments since I found typing FOO A B C D  is the same as FOO ABCD with
your version.  I added array itemvars of type real integer and string.  It
should otherwise match your version.

/Carl
From: CARL@X930.Tymnet 
Date: Tue, 30 Sep 86 17:12:41 PDT 
To: dencoff@x62 
Subject: "Bravo! Bravo! Bravo!"... 

Bravo!
  Bravo!
    Bravo!
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 30 Sep 86 20:55:31 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Bug in (SPL)MAP causes BOOTS to fail! 

I got "READ ERR OR EOF" consistantly from BOOTS after I had used (SPL)MAP
to copy F3XP37.SAV to (SYS)NEWMON.SAV on 930 (and 62XP37.SAV on 62).
If you do "DIRECT (SYS)NEWMON.*/WORDS" you will see that the BAD file is
exactly 512 words too short, and the size of the good file is a multiple
of 512 words long.  I had to use either COPY or GET/SAVE to duplicate this
monitor.

Carl,
  When is MAP going to be fixed to rescan the command line?  The DUPLICATE
command is already in the monitor waiting for a new DUPLIC.SAV to be put
on SYS:.       /Joe
Received: from X62.Tymnet by X930.Tymnet; Wed, 1 Oct 86 11:42:22 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Wed, 1 Oct 86 11:21:41 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Nice work! 

I've just looked at the latest output from SUBMIT's SHOW command:  looks good!
It's clean, straight-forward, logical and attractive.
/D.

z
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 1 Oct 86 18:32:01 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: P034/P37 monitors created 

Monitors have been built for the following:
C22, C24, C26, C29, C33, C34, C36, C39, C55, C57, C70 (all systems in CUP-C&A)
D31, D32	31 rebuilt for 3 strings 3675, 32 with 2 "strings" 3652
D23, F28, D56   so Roy Roskilly can test PERP replacement

I am in the process of TELECOPYing them to the appropriate systems.   /Joe
Received: from X62.Tymnet by X930.Tymnet; Wed, 1 Oct 86 19:25:10 PDT
Return-path: <JMS@X62.Tymnet> 
From: JMS@X62.Tymnet 
Date: Wed, 1 Oct 86 19:22:29 PDT 
To: carl 
Subject: "I can't believe you're using PEAK"... 

I can't believe you're using PEAK to create a log file.  It has so much
unnecessary overhead as compared to AUXIO when it comes to creating a
simple log file.  Couldn't you have just MASQUEd as TXSSUP instead of
going to a MUX and back?  To each his own, I guess...   /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 2 Oct 86 5:37:03 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Problems restoring C70's all-files to C24. 

 2-Oct-86 04:06/Joe
Contact:  Cupertino Operations, (408)446-7705, L.T. Smith, System C24
Problem:  He refreshed the disks on 24, and started restoring all files from
          C70's backup tapes.  TITO restored all UFDs, all of SYS, then died
          with stopcode IME in the middle of directory BAILEY on the first tape.
Diagnosis: This happens on all systems - after REFRESH, there is a limited
          number of SATs that can be kept in core and none are written out.
          When the last in-core SAT is filled up, the system dies with IME,
          and you have to run a FULL DSKCLN.  Then the operator has to get
          ALL license (without using REFLAG!) and resume restoring.

Fix:      I ran TSTSUP (it and DECDDT are on SYS on all systems as of 25-Sep)
          and found that page 0 of [1,1].UFD had been overwritten with garbage.
          Probably when the monitor stepped all over itself when the SATs
          filled up.  Told LT to do the following:
          1. Get a directory of the all-files tape to find where SYS ends
          2. Refresh DSKB again
          3. Restore from tape positions 1-<end of SYS>
          4. Run BOOTS to write BOOT.KI to home pages
          5. Take the system down
          6. Bring up the system and run a FULL DSKCLN
          7. Find someone who has an OPER name with all license
          8. Finish restoring rest of all-files

-----------------------------------------------------------------
With the upcoming moves, it is critical that we get a new version of TITO
out to SYS, one with a single change: TITO must type the starting and ending
positions for each directory when doing STORE ALL or RESTORE ALL.

And we have got to come up with a better way of giving the operator all
license when they have to RESTORE ALL.

				/Joe
Received: from X62.Tymnet by X930.Tymnet; Thu, 2 Oct 86 12:15:16 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Thu, 2 Oct 86 11:58:30 PDT 
To: Joe Smith <JMS@X930.Tymnet> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC 
Subject: Re: P034/P37 monitors created 
In-reply-to: your message of Wed, 1 Oct 86 18:32:01 PDT

Joe,
  Thanks for giving Roskilly the new Monitor on all 3 hosts!  /D.
Received: from X62.Tymnet by X930.Tymnet; Thu, 2 Oct 86 16:22:13 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 2 Oct 86 16:19:25 PDT 
To: wrs@b39 
Subject: "BIll, To send to an ARCHIVE file, "... 

BIll,
  To send to an ARCHIVE file, what gets put into MAIL.FWD on the
users directory?

  I tried !SUBMIT.WSH,   !SUBMIT@SUBMIT.WSH,  !SUBMIT@(SUBMIT)SUBMIT.WSH
and they all send things to (MAIL)WSH.Q  where WSH is the mail domain...

I put #TUMS ARCHIVE  into (SUBMIT)SUBMIT.WSH  and so all is setup ready
for the right MAIL.FWD file.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 2 Oct 86 17:29:32 PDT 
To: JMS@X62.Tymnet 
Subject: Re: "I can't believe you're using PEAK"... 
In-reply-to: your message of Wed, 1 Oct 86 19:22:29 PDT

Cute subject...
  Actually, AUXIO would have been better.  Agreed!

{new subject}
  The reason for TAUX to the MUX and then to :62 was simply so that I
could attach to the job from home when I logged into the MUX.  That way
I would be assured of capturing the data.

  PEAK also keeps a "checkpoint" file and if the AUXIO job was aborted,
I suspect that the data would have been trashed... so, for the extra
overhead involved, the extra security may have been worth it.  {sigh}.

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 2 Oct 86 19:28:43 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: "I can't believe you're using PEAK"... 
In-reply-to: your message of Thu, 2 Oct 86 17:29:32 PDT

What we really need is a version of AUXIO that uses mapping so that the
partial log file is kept and is visible.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 2 Oct 86 19:44:33 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: (M33)20MON.VER has a list of the external 2020 monitors. 

It lists NEWMON,OLDMON,SYSTEM,BACKUP,CONSOL for all the 2020's except TRW.
From: CARL@X930.Tymnet 
Date: Fri, 3 Oct 86 1:34:46 PDT 
To: jms 
Subject: "Subject: SETLIC, ENABLE and DISABLE"... 

Subject: SETLIC, ENABLE and DISABLE

Assume that ENABLE and DISABLE will take license names...
(see (m33:930)LICENA.MAC for tables and code).

The SETLIC UUO is now described to be able to "raise" the frame license
if the process license contains WC.  Assume that any raise to the process
or frame license is reflected in a similar raise to the "saved" license.
(I believe this is true already for process license, although JBTSLC(LH)
may be cleared each time at process initialization?)

If then, a SETLIC UUO which reduces frame license to 0 is executed, only
the enabled bits will be cleared, but the JBTSLC(RH) shall remain, just
as if the user had done a DISABLE command.

Of course, another UUO could be invented, although it shouldn't be needed
since SETLIC is supposed to perform that very function.  Some programs may
"break" because they "accidently" clear RH, but I believe that even now
that actually reduces frame license.

Once SETLIC with WC has been executed to raise the saved license, the
saved frame license remains at that level and SETLIC can raise and lower
the enabled frame license at will.  Thus,  the LOGINN program can take
any passed license and disable it automatically thus removing the need
to create a frame with "disabled" license.  The only catch I see is if
license is once saved how will we really reduce it?   We said SETE...
and that may be the only way... what do you think?

/Carl
From: CARL@X930.Tymnet 
Date: Fri, 3 Oct 86 3:22:49 PDT 
To: jms 
Subject: "Joe, I will do more converting"... 

Joe,
  I will do more converting to english and in a more suitable order of
precidence, but (M33:930)P034Q.DOC contains my first pass at making english
and combining things in groups.

  I "pre"documented ENABLE/DISABLE and new SETLIC with license bit tables
which is all in one place so it can be removed if we do not put the code
into the monitor.   Even if SETLIC doesn't work, can be added later to
LOGINN... it would be nice to have license names.  (I know I sound like
I'm pushing this by mentioning it a lot, but I don't mean to push it, just
giving my opinion.)

Please take a look at P034Q.DOC and make any comments you think appropriate.
Did we leave anythng out? not in P034.MEM but in the monitor?

I hacked out rehashing comments about code added and removed, fixes to
things broken by new code, etc... a bit sleepy so, I'm rambling.. sorry.
See you later.  G'night!

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 3 Oct 86 12:04:16 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: C24 crashed last night also. It's up now. 

 3-Oct-86 02:06/Joe
Contact:  Cupertino Operations, (408)446-7705, L.T. Smith, System C24
Problem:  BADSAT, [1,1].UFD messed up, etc. when doing RESTORE of C70's files.
          One of the disks had gone off-line, reports of BADSAT on other disks.
Diagnosis: Ran TSTSUP, word 25 of page 1 pointed to [1,1].UFD RIB at page 20712.
          Word 51 of that page pointed to UFD data page for hash 0, it was
          200006,,000012 - the first non HOM page on DSKB3.  That particular
          page was full of ASCII text, TYMSAT engine code in particular.
          Something crumped on the SAT for DSKB3 again allowing the Monitor
          to write over (UFD) again.
Fix:      Turned off RBREAL in RIB pointer (making it 6,,12).  Told operations
          to run another FULL DSKCLN, restore the UFD section of the tape again,
          and resume restoring from where they left off.
Followup: As of noon Friday, TITO was in the middle of restoring (MOORING).
          System uptime 9:35.  PSP reports 952145 pages free, no errors.

From: CARL@X930.Tymnet 
Date: Fri, 3 Oct 86 13:00:16 PDT 
To: dencoff 
Subject: "Dennis, Look at and see if you"... 

Dennis,
  Look at (m33::930)P034Q.MEM  and see if you understand it all.  Let me
know of anything that you don't understand, so on my second pass today
I can make it understandable.

  Thanks.
/Carl
Received: from B36.Tymnet by X930.Tymnet; Fri, 3 Oct 86 14:03:31 PDT
Return-path: <RMURPHY@B36.Tymnet> 
From: Ron Murphy <RMURPHY@B36.Tymnet> 
Date: Fri, 3 Oct 86 13:53:20 PDT 
To: carl@x930.tymnet 
Subject: SUBMIT/TUMS TCMSG 

Carl,
   It appears that the reason I'm not seeing the results I expect under
the new SUBMIT facility is that the facility only runs parameterless
programs rather than accepting a command line like XEXEC.SAV did.  
Previously the child frame was supposed to do the equivalent of
"PCOM TCMSG."  It looks like SUBMIT is set up to run TCMSG.SAV instead.
I guess I'm going to write a SAIL program to do the equivalent of PCOM
TCMSG.  Questions:  (1) is there any easier way to do this; (2) is
there any way to set stuff up without being logged into the associated
directory?  It's pretty hard to log into MAIL because the SMTP receiver
program is set up as an init file and isn't designed to be escaped out
of.  I understand that for security/integrity only the logged-in
directory should normally be able to change its queueing but it
would be useful to have a kludge around this...
Ron
Received: from B36.Tymnet by X930.Tymnet; Fri, 3 Oct 86 17:17:54 PDT
Return-path: <CARL@B36.Tymnet> 
From: CARL@B36.Tymnet 
Date: Fri, 3 Oct 86 17:12:01 PDT 
To: rmurphy@b36 
Subject: "Ron, I looked at your TCMSG.CL"... 

Ron,
  I looked at your TCMSG.CL file and found out why your job is not running.
In fact it IS running fine.  However, there is a bug with ^D in all CTL
files in XEXEC which is NOT in PCOM and therefore your ^Ds are ignored at
the moment.

  If you change all your ^Ds to ":SEND CHARACTER 4<crlf>" it will work fine.
Yes, put a crlf on the line and begin your next message on the next line!
When the job is processed, the output will put a ^D on the line and the
next line from the command file will continue on the same line.

This should also work in PCOM so that you can run PCOM by hand to test
your job.

/Carl


I will let you know when the bug is fixed... sorry about the bug!  /Carl
Received: from B36.Tymnet by X930.Tymnet; Fri, 3 Oct 86 17:30:01 PDT
Return-path: <RMURPHY@B36.Tymnet> 
From: Ron Murphy <RMURPHY@B36.Tymnet> 
Date: Fri, 3 Oct 86 17:27:51 PDT 
To: carl@x930.tymnet 
Subject: TUMS/XEXEC 

Carl,
   Thanks very much for checking on this TCMSG.CTL thing.  I'll add
the send character statement and that should be fine.  
Ron
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 3 Oct 86 19:10:40 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: (M33)STOPCD.MEM is done. 

The first draft of the STOPCD.MEM is done.  I am soliciting comments as to
what else should be added to the beginning of the file.   /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 4 Oct 86 23:18:57 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: PAKCP5 works! 

I located the reason why PAKCP5 would not correctly write HA and R0 (Home
Address and Record 0) on 3650's.  There were two off-by-one bugs in the code
I added (unfortunately they did not cancel each other out).

I located the reason why PAKCP5 and PAKCP3 could not write the secondary
HOM and BAT pages.  Code written a long time ago to handle RECSIZ=^D152 used
the wrong half of the accumulator when RECSIZ=PAGSIZ.

I fixed both, refreshed the alternate tracks on unit 315, and successfully
copied 314 to 315.  Now we have two copies of system 24's DSKB14 on those
two units.  Neither one is usable as is, but the copy worked!

The next step is to verify 3675 packs formatted with PAKCP3 in pages work
with CPXKON.  After that, make a single PAKCOP that can handle all three
formats.
				/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 5 Oct 86 0:32:08 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: HOTLINE call from D34 

 5-Oct-86 00:06/Joe
Contact:  Dallas Operations, (214)637-7435, Greg Moran, D34
Problem:  PAM not running, (UTIL)GOPJ would not start it.
Diagnosis: Tried "TAUX PJ:34" from system 34, got "?access barred".
          This means that D34's base (node 6703) is not in the list of
          TYMSHARE nodes.  The supervisor says 6703 is not in the class list
          valid for PJ.
Fix:      Logged into D23, set AC license, "R (SPL)TAUX" and "PJ:34".
          This is a temporary hack until Validations adds node 6703 to list.

From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 5 Oct 86 12:22:59 PDT 
To: carl 
Subject: 6703 is not in class 16 (TSB Tymshare Base Nodes). 

We need to get a hole of someone in Validations ASAP and get node 6703
(D34-base) inserted into class 16 so that PJ can work.
I don't have a phone number for anyone in Validations.  Would it be possible
to contact Sandy McFarland today (Sunday)?  /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 5 Oct 86 12:56:45 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: PJ on D34 

Because GOPJ does not work on D34, I modified (SYS)XEXEC.CTL to start (PJ)PAM
at system startup.   Maybe we should do this on all systems for as long as
(PJ)PAM needs to run.  /Joe
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Mon, 6 Oct 86 11:57:39 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven <OSMAN@X930.Tymnet>, Joe
	Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet> 
Subject: "the Hap's", this am. 

X62 crashed this morn, while I was working on it.  I can't bring it back up by
myself, and Steve Capik is on vacation all week.  Any messages I have for you
all I can send from X930.  I will remove my MAIL.FWD from X930 until X62 gets
fixed.  Please send all TUMS mail to me at DENCOFF:930 unti further notified.

Until X62 is back up, I can do no work on the Job Processor Documentation, or
on SWITCH.INI documentation!  HELP!!

Joe, re.  C70:  I have replaced a damaged copy of (SYS)EDITOR.SHR with a good
copy from another host.  Barbara, in bldg C, told me about the file in (SYS)
on C70 (file RECOV.SYS) which lists some files reported as having 'data
missing'.  The file appears to be output of DSKCLN (or something similar), and
lists 6 files from (SYS), and one file from (M33).  Although the file is on
C70, it says it is data from host C24.  I have looked at the referenced files
with DIRIT and CKSUM, on C70 and other hosts.  The files look O.K., and I've
received no user complaints.  I'm assuming the files are now O.K.  The files
are:

(SYS)COPLUD.LOW, EXIT.SAV, PIP.LOW, SSPICE.SAV, UUOSYM.UNV, FTUPDL.SAV
(M33)CRSHID.LOG (this created 5-oct-86)

Joe, I was unable to get in touch with Sally, to get your beeper.  She must
have left home by 8:40.

/D.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 6 Oct 86 13:33:14 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Terminal access at DECUS. 

We do have access to terminals, I am dialing in from the San Francisco node,
at (415)974-1300.  /Joe
Received: from B39.Tymnet by X930.Tymnet; Tue, 7 Oct 86 2:35:51 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 7 Oct 86 2:05:23 PDT 
To: CARL@X930.Tymnet 
Subject: Re: "Re: CRLF on TMPCOR files The in"... 
In-reply-to: your message of Tue, 30 Sep 86 6:45:06 PDT

Don't worry - any program that can't handle a CRLF deserves what it gets.
This of course, means alot of programs will have to be recompiled, but then
what are computers for?  -Bill
Received: from X62.Tymnet by X930.Tymnet; Tue, 7 Oct 86 2:35:58 PDT
Received: from B39.Tymnet by X62.Tymnet; Tue, 7 Oct 86 2:35:35 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 7 Oct 86 2:16:47 PDT 
To: CARL@X62.Tymnet 
Subject: Re: "BIll, To send to an ARCHIVE file, "... 
In-reply-to: your message of Thu, 2 Oct 86 16:19:25 PDT

In file (SUBMIT)MAIL.FWD:
	SUBMIT/SUBMIT.WSH@X930.Tymnet

In file (SUBMIT)SUBMIT.WSH:
	#TUMS ARCHIVE

If the extension of the archive file is .TUM, it may be left off.  To specify
a null extension, use:
	SUBMIT/SUBMIT.@X930.Tymnet

This is the same address format used to specify a distribution list file name.
-Bill
Received: from X62.Tymnet by X930.Tymnet; Tue, 7 Oct 86 22:37:52 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Tue, 7 Oct 86 21:47:32 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Update: spt. call answers. 

Carl,

   I've had a call or two.  I'm going to need you to answer them for me.
Here's the list:


1.)  MDC MIS in St. Louis, re. billing for the 10's, has a MAGCON problem,
since some ASCII characters have not EBCDIC equivalent.  The tapes being
created with MAGCON are getting some kind of error in conversion, resulting in
multiple characters in the ouput tape for one ASCII input character  (I
believe).  I have told the man in St. Louis that St. Louis MIS/Accounting must
decide what they want for EBCDIC representation for non-EBCDIC ASCII
characters.  We're going to have to modify MAGCON's conversion table for their
use (again!).  (Small world:  it's the same input data that I had trouble with
2 years ago!  This time the end user can get what he wants in conversion.)

Dale Reitzler, MIS Project Leader, is the one who wanted to know if MAGCON
pads out the last block.  Please give him the answer you gave me -- I ran a
test and confirmed what you had said.  It sure'nuff does.  He's O.K.  His
phone number is 314/233-5165.  He has Mark Nichols, from Cupertino, on site.


2.)  Frank Rodriguez, "Tymshare" MIS, Cupertino (X 6333), is having a problem
with an accounting program on C70 -- since the move from bldg.  A to C24's
hardware.  His problem is with (BILLING10)GETRTY.  [SHR & LOW]).  He's getting
a COBOL run-time error message ("MEMORY PROTECTION VIOLATION @ USER LOCATION
<nnnn> LIBOL ...").  I understand that the drives are Winchesters that were
just moved down the street.

Despite the fact that some other files on C70 got corrupted in the move, I
find no evidence that his object code file is corrupted (see (DENCOFF:62)
GETRTY.LOC -- number of words and cksum same as all other systems).  I'm
stumped, unless (sudden flash!)  file protection or license got changed in the
move.


3.)  JULIE MORK, Ranier Bank (S1051), is having a MAGNUM problem.  she's
getting the MAGNUM error message:  "DISK FULL OR QUOTA EXCEEDED FOR A
TEMPORARY INTERNAL FILE."  Too late to call her back, I realized that she's
probably doing a sort on a large data set.  Her problem occurs when she's
working with a MAGNUM relation called "SERVICE_HISTORY".  Here are the stats:
"Relation" contains 95962 records, occupying 3518 pages.
Username RBFMDA (sp?) is valid for 300,000 pages.
PSP command shows 29683 pages available.

She runs out of core when running a MAGNUM procedure "ZERO_YEAR_TO_DATE_
HISTORY", which does something with SERVICE_HISTORY.  That relation is the one
common factor to the failure.  She's probably doing some sorting on it.  The
one way to determine with certainty (that I can think of) to check on the
"running out of core" hypothesis is to be logged in, running SYSTAT with the
CONT switch while she's running her trouble causer.


That's all I can think of, right now.
/D.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 8 Oct 86 14:41:02 PDT
From: NTD.A/NEWMAN@Ontyme.Tymnet 
Date: TELECO problem 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.SUP@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, NTD.L/MAURER@Ontyme.Tymnet,
	NTD.P/DRISCOLL@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet 
Message-id: I37516@Ontyme.Tymnet 
Subject: TELECO problem - follow up of call to Dennis Coffey on 10/8 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      08 OCT 86  13:10                  Network Technology Development

TO>        TXS.SUP
            Dennis Coffey
            Carl Baltrunas

COPIES>    <see CC list>

FROM>      Adele Newman


SUBJECT>   TELECO problem - follow up of call to Dennis Coffey on 10/8


-----------------------------------------------------------------------


The Special Interfaces Unit is experiencing a problem with TELECO
which is causing delays on a high-priority release of test software
promised this week to a customer.  When copying the source file
(BSCDEV:39)bbxsvv.rrr with TELECO, the resulting target file has a
different checksum.  The file being copied is an ASCII source file
which was built by concatenating a number of smaller files.  We have
done the following tests:
 
1.  Using a "gateway" statement to cause the file to be copied into
    another directory on 39, the source file is TELECOpied.  The
    checksum of the target file does not match, but a DIF (defaulted
    and with /BIN) show no differences encountered.  TELECO parameters
    were defaulted.
 
2.  The same test was done using /BINARY option with bytesize=7, left
                                                                Page  2

    fill, 5 bytes to a word.  We get the same results as in the first
    test (including the same checksums).
 
3.  The same test was done using /IMAGE option.  Again, the results
    are similar to "1" and "2", except that the target file checksum
    is different from the one that results from the first two tests.
 
NOTE:  Attempting to TELECO the result file from test 1 causes TELECO
to print the following message:
  "There are "holey" pages in your file.  Do you wish to proceed?"
We answer YES, and then TELECO seems to be in a loop, printing:
  "Holey page table not properly sent to the Donor - will try again."
 
The two files (from test 1) appear to have an equivalent assembly
output file (NIB); this is still being checked.  Even if this
workaround can be used, if it checks out, this is not acceptable for
software distribution, since it represents a lengthy procedure which
must be followed to verify that each target file is OK.
 
Please advise as soon as possible regarding solutions or other
alternatives.  Let me know if anything else is needed in addition to
this memo.  Linda Maurer of Special Interfaces will be following up on
this problem.
 
 
Regards,
  Adele Newman (X.25/X.75 Unit)
  (408)446-7410
Received: from C35.Tymnet by X930.Tymnet; Wed, 8 Oct 86 16:29:59 PDT
Return-path: <LINDLEY@C35.Tymnet> 
From: LINDLEY@C35.Tymnet 
Date: Wed, 8 Oct 86 16:14:07 PDT 
To: carl 
Subject: ken's farewell 

Carl:
As you doubtless know, Friday will be Ken Dawson's last day (he is going to
Grid).  The farewell gathering will be after work upstairs at Eli McFly's, in
case you would like to come.

Hope everything is going well for you.  Some day we should get together for
lunch.
-Lois
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 8 Oct 86 18:40:35 PDT
From: NTD.A/NEWMAN@Ontyme.Tymnet 
Date: TELECO problem - follow up to msg I37516 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	NTD.L/MAURER@Ontyme.Tymnet, NTD.E/BUELL@Ontyme.Tymnet,
	NTD.P/DRISCOLL@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet 
Message-id: I37693@Ontyme.Tymnet 
Subject: "Carl, I've done the other test"... 

Carl,
 
    I've done the other test you requested; this was a "vanilla"
TELECO between Bubbnet hosts 39 and 36 using defaults.  The results
are the same as what we saw with gateway copies (rocky road!).  Here's
a summary:
 
When copying original file from 39 to 39 using gateway statement:
    original file:  (BSCDEV:39)BBXSVV.RRR     checksum is JAQPEJ
    target file:    (EBUELL:39)BBXSVV.RRR     checksum is WUKBIJ
 
When copying original file from 39 to 36 - no gateway:
    original file:  (BSCDEV:39)BBXSVV.RRR     checksum is JAQPEJ
    target file:    (ANEWMAN:36)BBXSVV.RRR    checksum is WUKBIJ
In this case, this was the result of the second try.  The first
attempt aborted with the message:
    TRANSFER ABORTED DUE TO UNEXPECTED CONTROLLER CODE
 
    I appreciate your help on this.
 
Regards and thanks,
   Adele
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Wed, 8 Oct 86 20:33:45 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven <OSMAN@X930.Tymnet>, Joe
	Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet> 
Subject: DECUS, Thurs., ... 

Just to get any needed coordination done, here are my Thursday plans:  I only
wish to attend 2 Thursday sessions, starting at 12:30.  I'll be in the office
to give any needed coverage, and get some moderately urgent work done.  I'll
leave Fremont about 11 am.

I just noticed X62 has crashed.  It crashed twice Monday:  it just got hung
with parity error lights on for both MI & memory.  I ran MEMTST, and got
plenty of completed, error-free iterations of the test.  Joe tells me that
Steve Capik has found that the power supply has been irregular:  varying in
output over time, reguiring fairly frequent readjustments.  I think Steve
should be called as soon as he gets back from vacation, Monday.

/D.
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Wed, 8 Oct 86 20:46:29 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Items on msg sent you Tues. 

Carl,

  What's the status of the items on the TUMS mail msg I sent you Tues night?
Did you get them taken care of -- at least the more urgent items?

Thanks,
D.
Received: from X62.Tymnet by X930.Tymnet; Thu, 9 Oct 86 15:22:50 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 9 Oct 86 15:14:56 PDT 
To: dencoff 
Subject: "Re: Msgs your link. I was hee"... 

Re: Msgs & your link.

I was hee all the time you linked... i think.. did you type ^G or anything?
that's the way I always try to get someone's attention...

Anyway,
  I also called Julie, she had gone right after you talked to her, so I
assumed that you took care of everything....
  I left the call to Cheryl London to you, since she also called Craig and
he told me why she was trying to get ahold of you. to arrange a call...

  The problem with :70 was a bad copy of LIBOL7.SHR on SYS.  Frank's problem
was also NOT described properly by Frank.  By doing a SYSTAT on his job or
by his typing VERSION (with the new monitor) we found it wasn't even running
GETRTY at the time the error occurred.  I had him have OPS do a restore from
another system (C35?) to C70 of that file, and he says everything is fine now.

  I talked to Adele yesterday... still haven't looked at their problem per se
but did give her somehting to try.  TELECO seems to be munching their data
somehow... possibly dropping some bit 35's.. I'll check that this afternoon.

  Nothing else...really... oh yes, Karen in validations, a customer had a
problem running PASSWORD form a 5-digit node... It was the old version, %20
even before the changes Bill and I added, so I'll call Ingrid to see about
moving the file around... getting WRS's newer version out everywhere....
/Carl
Received: from C57.Tymnet by X930.Tymnet; Thu, 9 Oct 86 16:05:27 PDT
Received: from tymix.Tymnet by C57.Tymnet; Thu, 9 Oct 86 16:03:21 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA09662;
	Thu, 9 Oct 86 16:02:38 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA05285; Thu, 9 Oct 86 16:01:22 pdt
Return-path: <wrs@eunix.Tymnet> 
From: wrs@eunix (Bill Soley) 
Date: Thu, 9 Oct 86 16:01:22 pdt 
To: carl@x930 
Message-id: <8610092301.AA05285@eunix.Tymnet> 
Subject: "dimensions m kg sec coul candela"... 

/ dimensions
m			*a*
kg			*b*
sec			*c*
coul			*d*
candela			*e*
dollar			*f*
radian			*g*
bit			*h*
erlang			*i*
degC			*j*

/ constants

fuzz			1
pi			3.14159265358979323846
c			2.997925+8 m/sec fuzz
g			9.80665 m/sec2
au			1.49597871+11 m fuzz
mole			6.022169+23 fuzz
e			1.6021917-19 coul fuzz
energy			c2
force			g
mercury			1.33322+5 kg/m2-sec2
hg			mercury

/ dimensionless

degree			1|180 pi-radian
circle			2 pi-radian
turn			2 pi-radian
grade			.9 degree
arcdeg			1 degree
arcmin			1|60 arcdeg
ccs			1|36 erlang
arcsec			1|60 arcmin

steradian		radian2
sphere			4 pi-steradian
sr			steradian

/ Time

second			sec
s			sec
minute			60 sec
min			minute
hour			60 min
hr			hour
day			24 hr
da			day
week			7 day
year			365.24219879 day fuzz
yr			year
month			1|12 year
ms			millisec
us			microsec

/ Mass

gram			millikg
gm			gram
mg			milligram
metricton		kilokg

/ Avoirdupois

lb			.45359237 kg
lbf			lb g
ounce			1|16 lb
oz			ounce
dram			1|16 oz
dr			dram
grain			1|7000 lb
gr			grain
shortton		2000 lb
ton			shortton
longton			2240 lb

/ Apothecary

scruple			20 grain
apdram			60 grain
apounce			480 grain
appound			5760 grain

/ Length

meter			m
cm			centimeter
mm			millimeter
km			kilometer
nm			nanometer
micron			micrometer
angstrom		decinanometer

inch			2.54 cm
in			inch
foot			12 in
feet			foot
ft			foot
yard			3 ft
yd			yard
rod			5.5 yd
rd			rod
mile			5280 ft
mi			mile

british			1200|3937 m/ft
nmile			1852m

acre			4840 yd2

cc			cm3
liter			kilocc
ml			milliliter

/ US Liquid

gallon			231 in3
imperial		1.20095
gal			gallon
quart			1|4 gal
qt			quart
pint			1|2 qt
pt			pint

floz			1|16 pt
fldr			1|8 floz

/ US Dry

dry			268.8025 in3/gallon fuzz
peck			8 dry-quart
pk			peck
bushel			4 peck
bu			bushel

/ British

brgallon		277.420 in3 fuzz
brquart			1|4 brgallon
brpint			1|2 brquart
brfloz			1|20 brpint
brpeck			554.84 in3 fuzz
brbushel		4 brpeck

/ Energy Work

newton			kg-m/sec2
nt			newton
joule			nt-m
cal			4.1868 joule

/ Electrical

coulomb			coul
ampere			coul/sec
amp			ampere
watt			joule/sec
volt			watt/amp
ohm			volt/amp
mho			/ohm
farad			coul/volt
henry			sec2/farad
weber			volt-sec

/ Light

cd			candela
lumen			cd sr
lux			cd sr/m2

/ Money
/ epoch June 16, 1980 Linns Stamp News
/ (Wall Stree Journal; Manfra, Tordella and Brookes, Inc.)

$			dollar
argentinapeso		.0056 $
australiadollar		1.1460 $
austriaschilling	.0793 $
bahamsdollar		1.0300 $
belgiumfranc		.0352 $
belizedollar		.5500 $
bermudadollar		1.0300 $
brazilcruzeiro		.0201 $
canadadollar		.8628 $
chilepeso		.0303 $
chinayuan		.6780 $
chinadollar		.0278 $
colombiapeso		.0218 $
cypruspound		2.8500 $
denmarkkrone		.1814 $
dominicanrepublicpeso	1.0000 $
eastcarribeandollar	.3900 $
equadorsucre		.0375 $
egyptpound		2.1910 $
fijidollar		1.3000 $
finlandmarkka		.2747 $
francefranc		.2422 $
germanymark		.5647 $
germanmark		germanymark
greatbritainpound	2.2970 $
britainpound		greatbritainpound
greecedrachma		.0233 $
honduraslempira		.5200 $
hongkongdollar		.2040 $
icelandpound		.0034 $
indiarupee		.1285 $
indonesiarupiah		.0016 $
/ iranrial		??????
iraqdinar		3.40000 $
irelandpound		2.0860 $
israelshekel		.0219 $
italylira		.0012 $
jamaicadollar		.5950 $
japanyen		.0045 $
koreawon		.0017 $
lebanonpound		.2939 $
malaysiadollar		.4635 $
mexicopeso		.0439 $
netherlandsguilder	.5140 $
newzealanddollar	.9870 $
nigerianaira		1.6000 $
norwaykrone		.2057 $
pakistanrupee		.1015 $
paraguayguarani		.0081 $
perusol			.0037 $
phillippinespeso	.1360 $
polandzloty		.0300 $
portugalescudo		.0204 $
saudiarabiariyal	.3003 $
singaporedollar		.4684 $
southafricarand		1.2825 $
southkoreawon		koreawon
spainpeseta		.0143 $
sudanpound		2.1000 $
swedenkrona		.2391 $
switzerlandfranc	.6064 $
tanzaniashilling	.1400 $
thailandbhat		.0500 $
trinidaddollar		.4300 $
turkeylira		.0204 $
uruguaynewpeso		.1158 $
ussrruble		1.5600 $
russiaruble		ussrruble
venezuelabolivar	.2329 $
yugoslaviadinar		.0600 $
zairezaire		1.2300 $
zambiakwacha		1.3600 $

mark			germanymark
bolivar			venezuelabolivar
peseta			spainpeseta
rand			southafricarand
escudo			portugalescudo
sol			perusol
guilder			netherlandsguilder
peso			mexicopeso
yen			japanyen
lira			italylira
dinar			iraqdinar
rupee			indiarupee
drachma			greecedrachma
franc			francefranc
markka			finlandmarkka
sucre			equadorsucre
pound			britainpound
poundsterling		britainpound
cruzeiro		brazilcruzeiro
ruble			ussrruble
/ rial			iranrial

/ PDP-11

baud			bit/sec
byte			8 bit
word			2 byte
block			512 byte
K			1024 word
tc			578 block
rktrack			12 block
rkcylinder		2 rktrack
rk			203 rkcylinder
rptrack			10 block
rpcylinder		20 rptracks
rp			406 rpcylinder
rftrack			8 block
rfshoe			8 rftrack
rfdisk			16 rfshoe
rf			2 rfdisk

/ Trivia

%			1|100
admiraltyknot		6080 ft/hr
apostilb		cd/pi-m2
are			1+2 m2
arpentcan		27.52 mi
arpentlin		191.835 ft
astronomicalunit	au
atmosphere		1.01325+5 nt/m2
atm			atmosphere
atomicmassunit		1.66044-27 kg fuzz
amu			atomicmassunit
bag			94 lb
bakersdozen		13
bar			1+5 nt/m2
barie			1-1 nt/m2
barleycorn		1|3 in
barn			1-28 m2
barrel			42 gal
barye			1-1 nt/m2
bev			1+9 e-volt
biot			10 amp
blondel			cd/pi-m2
boardfoot		144 in3
bolt			40 yd
bottommeasure		1|40 in
britishthermalunit	1.05506+3 joule fuzz
btu			britishthermalunit
refrigeration		12000 btu/ton-hour
buck			dollar
cable			720 ft
caliber			1-2 in
calorie			cal
carat			205 mg
cent			centidollar
cental			100 lb
centesimalminute	1-2 grade
centesimalsecond	1-4 grade
century			100 year
cfs			ft3/sec
chain			66 ft
circularinch		1|4 pi-in2
circularmil		1-6|4 pi-in2
clusec			1-8 mm-hg m3/s
coomb			4 bu
cord			128 ft3
cordfoot		cord
crith			9.06-2 gm
cubit			18 in
cup			1|2 pt
curie			3.7+10 /sec
dalton			amu
decade			10 yr
dipotre			/m
displacementton		35 ft3
doppelzentner		100 kg
dozen			12
drop			.03 cm3
dyne			cm-gm/sec2
electronvolt		e-volt
ell			45 in
engineerschain		100 ft
engineerslink		100|100 ft
equivalentfootcandle	lumen/pi-ft2
equivalentlux	lumen/pi-m2
equivalentphot		cd/pi-cm2
erg			cm2-gm/sec2
ev			e-volt
faraday			9.652+4 coul
fathom			6 ft
fermi			1-15 m
fifth			4|5 qt
fin			5 dollar
finger			7|8 in
firkin			9 gal
footcandle		lumen/ft2
footlambert		cd/pi-ft2
fortnight		14 da
franklin		3.33564-10 coul
frigorie		kilocal
furlong			220 yd
galileo			1-2 m/sec2
gamma			1-9 weber/m2
gauss			1-4 weber/m2
geodeticfoot		british-ft
geographicalmile	1852 m
gilbert			7.95775-1 amp
gill			1|4 pt
gross			144
gunterschain		22 yd
hand			4 in
hectare			1+4 m2
hefnercandle		.92 cd
hertz			/sec
hogshead		2 barrel
hd			hogshead
homestead		1|4 mi2
horsepower		550 ft-lb-g/sec
hp			horsepower
hyl			gm force sec2/m
hz			/sec
imaginarycubicfoot	1.4 ft3
jeroboam		4|5 gal
karat			1|24
kcal			kilocal
kcalorie		kilocal
kev			1+3 e-volt
key			kg
khz			1+3 /sec
kilderkin		18 gal
knot			nmile/hr
lambert			cd/pi-cm2
langley			cal/cm2
last			80 bu
league			3 mi
lightyear		c-yr
line			1|12 in
link			66|100 ft
longhundredweight	112 lb
longquarter		28 lb
lusec			1-6 mm-hg m3/s
mach			331.46 m/sec
magnum			2 qt
marineleague		3 nmile
maxwell			1-8 weber
metriccarat		200 mg
mev			1+6 e-volt
mgd			megagal/day
mh			millihenry
mhz			1+6 /sec
mil			1-2 in
millenium		1000 year
minersinch		1.5 ft3/min
minim			1|60 fldr
mo			month
mpg			mile/gal
mph			mile/hr
nail			1|16 yd
nauticalmile		nmile
nit			cd/m2
noggin			1|8 qt
nox			1-3 lux
ns			nanosec
oersted			2.5+2 pi-amp/m
oe			oersted
pace			36 in
palm			3 in
parasang		3.5 mi
parsec			au-radian/arcsec
pascal			nt/m2
pc			parsec
pennyweight		1|20 oz
percent			%
perch			rd
pf			picofarad
phot			lumen/cm2
pica			1|6 in
pieze			1+3 nt/m2
pipe			4 barrel
point			1|72 in
poise			gm/cm-sec
pole			rd
poundal			ft-lb/sec2
pdl			poundal
proof			1|200
psi			lb-g/in2
quarter			9 in
quartersection		1|4 mi2
quintal			100 kg
quire			25
rad			100 erg/gm
ream			500
registerton		100 ft3
rehoboam		156 floz
rhe			10 m2/nt-sec
rontgen			2.58-4 curie/kg
rood			1.21+3 yd
rope			20 ft
rutherford		1+6 /sec
rydberg			1.36054+1 ev
sabin			1 ft2
sack			3 bu
seam			8 bu
section			mi2
shippington		40 ft3
shorthundredweight	100 lb
shortquarter		25 lb
siemens			/ohm
sigma			microsec
skein			120 yd
skot			1-3 apostilb
slug			lb-g-sec2/ft
span			9 in
spat			4 pi sr
spindle			14400 yd
square			100 ft2
stere			m3
sthene			1+3 nt
stilb			cd/cm2
stoke			1-4 m2/sec
stone			14 lb
strike			2 bu
surveyfoot		british-ft
surveyorschain		66 ft
surveyorslink		66|100 ft
tablespoon		4 fldr
teaspoon		4|3 fldr
tesla			weber/m2
therm			1+5 btu
thermie			1+6 cal
timberfoot		ft3
tnt			4.6+6 m2/sec2
tonne			1+6 gm
torr			mm hg
township		36 mi2
tun			8 barrel
water			.22491|2.54 kg/m2-sec2
wey			40 bu
weymass			252 lb
Xunit			1.00202-13m
k			1.38047-16 erg/degC
Received: from X62.Tymnet by X930.Tymnet; Fri, 10 Oct 86 10:46:00 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Fri, 10 Oct 86 10:41:21 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Bad BUG in SUBMIT. 

Pls. see (SUBMIT)SUBMIT.WSH.  It's easy to get SUBMIT into an infinite loop,
and the program must trap user interrupts:  I tried to "^C" out of the loop
and got no response, so I logged in to another frame and "KJ"ed the frame
running SUBMIT.
/D.
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 10 Oct 86 18:40:28 PDT
From: NTD.L/MAURER@Ontyme.Tymnet 
Date: :OUT 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.D/COFFEY@Ontyme.Tymnet,
	NTD.L/MAURER@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	NTD.B/KRUEGER@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet 
Message-id: I39103@Ontyme.Tymnet 
Subject: "Carl, We found that if we the"... 

Carl,

We found that if we "pulled" the files on System 39 to System 26, 
from the System 26 side, instead of "pushing" them from System 39,
that we could telecopy successfully.  This gives us a solution for
now, and, hopefully, gives you an additional clue to resolve the
problem.

Thanks for your help.

Linda
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 10 Oct 86 20:58:31 PDT 
To: carl 
Cc: osman 
Subject: Going north for the weekend. 

We're going to Octacon - an SF convention in Santa Rosa - for the weekend.
I'm taking my beeper to test if our current pager reaches that far, but I
don't expect it to work.  /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 13 Oct 86 19:55:21 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Everybody is running /P37 (except B39) 

The following table was created Monday afternoon, 13-Oct-86.

BASE    SYSNO           CPU   SYSID        MEMORY       JOBS     TYMNET+PTY
ND2100  C22-P034/P37 Apr# 641 M01775 KI-10 2047 pages 108 frames 196 ports
ND6716  D23-P034/P37 Apr#1275 M01791 KL-10 4095 pages 110 frames 196 ports
ND7165  D25-P034/P37 Apr#1460 M01794 KL-10 4095 pages 127 frames 204 ports
ND4200  C26-P034/P37 Apr#1286 M01778 KL-10 2047 pages 100 frames 192 ports
ND6715  D27-P034/P37 Apr# 666 M01795 KI-10 1536 pages 108 frames 196 ports
ND2333  F28-P034/P37 Apr#1427 M01793 KL-10 4095 pages 110 frames 196 ports
ND6712  C29-P034/P37 Apr#1307 M01779 KL-10 4095 pages  80 frames 182 ports
ND6721  F30-P034/P37 Apr#1388 M01801 KL-10 4095 pages 108 frames 196 ports
ND7004  D31-P034/P37 Apr# 761 M01790 KI-10 4095 pages  60 frames 172 ports
ND6415  C33-P034/P37 Apr#1081 M01780 KL-10 6143 pages 108 frames 196 ports
ND6703  D34-P034/P37 Apr# 574 M01781 KI-10 2047 pages  60 frames 172 ports
ND2334  C35-P034/P37 Apr#1386 M01782 KL-10 6143 pages 127 frames 204 ports
ND2762  B36-P034/P37 Apr#1415 M01783 KL-10 4095 pages  80 frames 182 ports
ND3115  D37-P034/P37 Apr#1461 M01796 KL-10 8191 pages 110 frames 196 ports
ND7021  F38-P034/P37 Apr#1405 M01802 KL-10 3072 pages  80 frames 182 ports
ND2674  B39-P034/P-13 pr#1354 M01532 KL-10            127 frames 207 ports
ND4274  D54-P034/P37 Apr#1376 M01797 KL-10 4095 pages 127 frames 204 ports
ND4177  C55-P034/P37 Apr#1336 M01787 KL-10 8191 pages 127 frames 204 ports
ND4725  D56-P034/P37 Apr#1383 M01792 KL-10 8191 pages 127 frames 204 ports
ND4525  C57-P034/P37 Apr# 542 M01788 KI-10 6143 pages 110 frames 196 ports
ND6720  F58-P034/P37 Apr#1332 M01803 KL-10 4095 pages  80 frames 182 ports
N10115  X62-P034/P37 Apr#   5 M01773 FOONL 1024 pages  32 frames  64 ports
ND2332  D65-P034/P37 Apr#1380 M01798 KL-10 3584 pages 108 frames 196 ports
ND5473  C70-P034/P37 Apr# 532 M01805 KI-10 2047 pages 108 frames 196 ports
ND6717  D72-P034/P37 Apr# 657 M01799 KI-10 2047 pages  60 frames 172 ports
ND6722  F74-P034/P37 Apr#1421 M01804 KL-10 4095 pages  80 frames 182 ports
ND5604  D79-P034/P37 Apr# 565 M01800 KI-10 1536 pages  60 frames 172 ports
ND6760  X95-P034/P37 Apr#4097 M01774 KS-10 1024 pages  32 frames  62 ports
ND6657  X930-P034/P37 Apr#  6 M01771 FOONL 1024 pages  32 frames  64 ports
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 14 Oct 86 0:01:30 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Pleas read (m33:930)P035.DOC 

It is my final pass, subject to comment on the changes we are documenting
for the users.  It seems under performance we do not list much, but I do
think that we did more, as far as performance due to reliability, so any
comments will be appreciated.
/care
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 14 Oct 86 0:03:47 PDT 
To: carl 
Subject: Balloons 

Do you have any of your white balloons left?  I brought my string of black
ones in to the office, just to see what sort of comments they would cause.
From: CARL@X930.Tymnet 
Date: Tue, 14 Oct 86 1:29:20 PDT 
To: jms 
Subject: "Balloons, etc. We did until tonight..."... 

Balloons, etc.

We did until tonight... Cherie went on a "breaking" spree and I think
they all got wiped out.  I just looked and didn't see any.

Please check over P034.MEM... I just revised it a tiny bit.. but there are
couple of questions... eg. when the date & time are set more than 60 minutes
from the current time, beeping the CTY... is this continuous until a SET
HAPPY or something?  What's the deal?

Just check the accuracy of my statements... I may have misinterpreted some
of your edits.... sigh.
/Carl
From: CARL@X930.Tymnet 
Date: Tue, 14 Oct 86 15:42:15 PDT 
To: txstext 
Subject: "919.51 to account... balance is"... 

...deposited 919.51 to account... balance is now  2521.06...
luv ya...
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 14 Oct 86 18:00:18 PDT 
To: NTD.L/MAURER@ONTYME, NTD.P/DRISCOLL@ONTYME, NTD.B/KRUEGER@ONTYME,
	NTD.A/NEWMAN@ONTYME 
Cc: CARL, Dennis Coffey <DenCoff@X930.Tymnet>, Craig Fletcher
	<FletcherC@X930.Tymnet>, OSMAN 
Subject: Found the source of the TELECOPY problem on B39. 

I tried many various commands, then discovered that there were two versions of
TELECOPY on the system.  Bad checksums occurred only when I typed "TELECOPY" or
"DO TELECOPY" instead of "R TELECOPY".  (FTSYS)TELECO.SAV dated 16-Apr-81,
version 21, is the culprit.  The correct version is in (SYS)TELECO.SAV, version
22.3, last modified in 1986.

I have deleted the obsolete version of TELECOPY from FTSYS.  Let me know if
this solves the problem.

		Joe Smith
		TXS.J/SMITH@ONTYME
Received: from X62.Tymnet by X930.Tymnet; Thu, 16 Oct 86 4:15:14 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 16 Oct 86 3:41:26 PDT 
To: lindley@c35 
Subject: "Lois, Did I hear you right about"... 

Lois,
  Did I hear you right about Ken Dawson leaving the company as of last
Friday?  I was unable to come for any going away last week due to DECUS
in San Francisco all week.  WHat's the story?

  -Carl
Received: from X62.Tymnet by X930.Tymnet; Thu, 16 Oct 86 9:09:33 PDT
Received: from C35.Tymnet by X62.Tymnet; Thu, 16 Oct 86 9:09:12 PDT
Return-path: <LINDLEY@C35.Tymnet> 
From: LINDLEY@C35.Tymnet 
Date: Thu, 16 Oct 86 9:05:56 PDT 
To: carl@62 
Subject: Ken's departure 

Carl:

Yes, Ken has gone to Grid in Mountain View.  His last day was last Friday.
I was at DECUS on Thursday - somehow didn't see you!

Ken left the PEAK notebooks for me (or whoever).  The latest PEAK is on
system 35, with the documentation updated (more or less) in SYSNEWS.
Probably it should be propogated either legally (transmittal) or quietly.

-Lois
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 16 Oct 86 12:18:58 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: STOPCD.MEM is ready 

Please give me your comments on pages 2 thru 4.  Let me know if there is 
anything you feel should be added there.   /Joe
Received: from X62.Tymnet by X930.Tymnet; Thu, 16 Oct 86 17:03:24 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 16 Oct 86 17:03:00 PDT 
To: submit 
Subject: "this is a test!"... 

this is a test!
Received: from X62.Tymnet by X930.Tymnet; Thu, 16 Oct 86 17:13:33 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 16 Oct 86 17:09:30 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet>,
	SUBMIT Maintainer <submit@X62.Tymnet> 
Subject: Sending mail to SUBMIT@X62 

Sending mail to "SUBMIT Maintainer <SUBMIT@X62>" will result in appending
to the bug/wish file (SUBMIT:62)SUBMIT.WSH without human intervention.

In a future release of SUBMIT, after I get XEXMAI written, I will add the
BUG command which will automatically send mail to SUBMIT.

/Carl
Received: from X62.Tymnet by X930.Tymnet; Thu, 16 Oct 86 17:20:53 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 16 Oct 86 17:20:28 PDT 
To: Submit Maintainer <Submit@X62> 
Subject: "testing 1, 2, 3, 4."... 

testing 1,2,3,4.
Received: from X62.Tymnet by X930.Tymnet; Thu, 16 Oct 86 17:20:55 PDT
Return-path: <Postmaster@X62.Tymnet> 
From: Postmaster 
Date: Thu, 16 Oct 86 17:20:38 PDT 
To: CARL@X62.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Invalid address(s):
cc


----------
RCPT To: cc 
From: CARL@X62.Tymnet 
Date: Thu, 16 Oct 86 17:20:28 PDT 
To: Submit Maintainer <Submit@X62> 

testing 1,2,3,4.
Received: from X62.Tymnet by X930.Tymnet; Thu, 16 Oct 86 17:23:06 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 16 Oct 86 17:22:48 PDT 
To: submit 
Subject: "testing 1, 4, 9, 16..."... 

testing 1,4,9,16...
Received: from X62.Tymnet by X930.Tymnet; Thu, 16 Oct 86 18:03:26 PDT
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 16 Oct 86 18:02:52 PDT 
To: submit 
Subject: "test!"... 

test!
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 17 Oct 86 0:39:37 PDT 
To: WRS 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: LOGINN version 70.1 installed. 

The latest version of LOGINN has been put on SYS on all systems.  Version 70.1
plugs a security hole from creating a child frame and running LOGINN it in.
The old version of LOGINN did not detect this situation, and unconditionally
read the username from the command port.  Any unpriviliged user could attach
a PTY to the child frame, and send the characters "OPER<CR>" to it.  LOGINN
assumed that the characters came from TYMNET and that the Supervisor had 
verified the password already.  The new version uses the username that the
CREFRM uuo stored in GETTAB tables.

A description of the changes to LOGINN and LOGOUT are found in (OSU)LOGINN.INF
and (OSU)LOGOUT.INF on all systems.    /Joe

From: CARL@X930.Tymnet 
Date: Fri, 17 Oct 86 1:30:55 PDT 
To: jms, dencoff 
Subject: "Joe, Dennis, Please officially"... 

Joe, Dennis,...
  Please officially transmit LOGINN !!!

Needed for transmittal are:
  1  instructions, or a pcom/xexec command file, or both
  2  list of all sources, checksums
  3  list of all output - .SAV,SHR,.LOW and checksums
  4  license and protection and directoy to put files

Ingrid will take care of the archiving and re-distribution if you
haven't already distributed it.

/Carl
From: CARL@X930.Tymnet 
Date: Fri, 17 Oct 86 2:47:20 PDT 
To: jms, osman, dencoff 
Subject: "Nw santa clara number 408-986-0646"... 

Nw santa clara number 408-986-0646  node 4165:03-xxx-

Hmmmmmm.
Received: from X62.Tymnet by X930.Tymnet; Fri, 17 Oct 86 11:53:41 PDT
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Fri, 17 Oct 86 11:42:29 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Subject: My PPN. 

Joe & Carl,

I've noticed something strange on the hosts which I've looked at:  my UUN has
a new bit set.  To my knowledge, my PPN has been:  [3,10765].  The response to
PJOB command on hosts other than our F3s gives:  [3,410765].  Do I ignore
this difference?  do you know why I may have 2 PPNs?

Thanks, /D.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 17 Oct 86 12:11:56 PDT 
To: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Cc: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Re: My PPN. 
In-reply-to: your message of Fri, 17 Oct 86 11:42:29 PDT

Until recently, MOVUSR did not like PPNs with a programmer part greater than
377777.  For instance, my PPN on 930 was [3,31116] instead of [3,431116].
TYMNET changed from 17-bit programmer codes to 18 bits, but MOVUSR was wrong.
Your correct PPN is [3,410765].  To correct it, we copy all your files to
a different directory (or to tape), delete the bad UFD, and MOVUSR to build
the right one.   /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 17 Oct 86 12:27:22 PDT 
To: carl 
Subject: Won't run MIDNIT.CTL - says "Abort-License". 

I set RF on my frame, re-inserted MIDNIT to run now with option LICENSE, it
comes back and says job is Abort-License.  What gives?  How am I supposed to
pass license to a PERP job these days?   /Joe
From: CARL@X930.Tymnet 
Date: Sun, 19 Oct 86 5:23:37 PDT 
To: dencoff, jms 
Subject: "Re: Pendant Moi? About the 18-bit"... 

Re: Pendant Moi?

About the 18-bit PPNs.  I beleive that MOVUSR was never the problem.  The
validations group didn't properly distribute the new version of UPDLUD to
all systems.  There was a misunderstanding about what name and what dir to
put the files on.  UPDLUD or FTUPDLud, (CUD10) (SYS) or (FTSYS).  Anyway,
their slave ran (SYS)FTUPTL for awhile and nooe ever changed UPDLUD on the
(SYS) directory and MOVUSR called that.  Sigh!

Another case of 1/2 the implementation followed by attrition... sigh!

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 19 Oct 86 5:47:37 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: VCR Watch and miscellaneous check-up for burglers etc 

Joe,

  The plan is that Cherie & I are both going to Colorado Springs.

Our neighbors Karen & jack will take care of CZAZU, feeding and walking
and such.  If you need to talk to them, their phone number is 263-3905.


Our Skating coach Wayne will stop by to water the plants and collect our
mail every couple of days.  If something happens and you cannot get ahold
of me, Karen or Jack,  Wayne or his roommate David can be reached at
263-8345.


I've asked David to check the VCR's but i don't know how reliable he will
be about it.  So, if you can make it, please try to stop by between the
end of the monday-night DR.WHO episode and the beginning of the tuesday-
night episode to switch tapes.

The layout, the tapes & vcrs and the plan:

    We have 2 VCRs, with name-tags:  FRED and ETHEL

    Fred is taping Blake's 7:
	Sunday night - (tonight) the last episode of the series
        Next Sunday  - the first episode of the series

    Ethel is taping DR. WHO:
	Monday night - the last episode of Vengeance on Varos
	Tuesday, etc - additional episodes of other Colin Baker stories

    All the programming is done.  All we need done is to have the
	tapes changed.  One time only, between monday and tuesday night
	sometime after the last episode of Vengeance on Varos, but
	before the next series.  If both VCR's are given a fresh tape
	then everything will be fine!

Thanks a lot!


For your efforts, feel free to take/eat any of the perishable food in
the refrigerator, to use the pool or jaccuzi, etc.  DO NOT EAT THE
LAND-O-LAKES CHEESE.. Cherie will kill to get that cheese since it is
only sold in the new-england area of the country...

Thank you again for checking on things...

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 19 Oct 86 18:23:23 PDT 
To: scapik, Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: X62 died before 8pm Saturday. 

Does the F3 microcode RAM go to all ones when power glitches?  I came in
and found all the lights on the top row of 62's display on.  All the lights
if MI ADR were on, the MEM ADDRS was 0,,577603.  The microcode reloaded with
no problems, I cut a new tape for 62XP37 and loaded it.  /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 19 Oct 86 18:30:00 PDT 
To: wrs, Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: New version of TAUX on SPL. 

Version 21 outputs the name of the circuit when it is build and when it
is deleted.  It also restores the previous behavior where Control-G comes
out as BEEP instead of "^G" (so the bell from wrong OPER password comes
thru).  The switch character has been changed to ! since / and # are legal
in TYMNET names and/or sub-hosts.  See (SPL)TAUX.INF for more info.




			/Joe
Bill: We don't really need two versions of TAUX (on XEXEC and SPL) - one
should be deleted.  And since TAUX does not respond to CCL entry, the
comment referring to CTEST should be removed from TAUX.PUB.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 19 Oct 86 18:36:04 PDT 
To: wrs 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Searching the PGY table does not flush cache. 

When the monitor wants to find whether a particular disk page is in core,
it does not make a linear search thru the PGY tables.  On a 4-meg system,
the last 12 bits of the disk address are used to index into an array
of heads of linked lists.  One of the PGY entries is the pointer to the
next core page that has the same disk hash.  The average linked list is
a chain of 2 or 3 entries.  After 4 or so compares, the monitor can decide
that the page is not in core, without flushing the cache.    /Joe
Received: from B39.Tymnet by X930.Tymnet; Mon, 20 Oct 86 13:17:30 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 20 Oct 86 13:09:25 PDT 
To: Gazel Tan <GAZEL@C39.Tymnet> 
Cc: carl@930 
Subject: Re: B39 
In-reply-to: your message of Mon, 20 Oct 86 11:32:13 PDT

I was not directly involved with testing the new monitor, but I was
aware of it and had more or less given an okay.  It has supposedly been
running on other machines for some time without trouble.  It has new
features that would be beneficial for us since they allow us to run
a PERPlike program and resume our AFD.  If there has been unreasonable
down time then I'll have them back off for a while.  If there have been
"funninesses" that might help by being explained, please let Carl or
myself know.  Thanks -Bill
From: Joe Smith <JMS@X930.Tymnet> 
Date: Tue, 21 Oct 86 17:50:29 PDT 
To: carl, Dennis Ernst <DErnst@B39.Tymnet> 
Subject: Can't get to host 575 from node 6657. 

From 930's base, if I try to login as "JMS:575;", I get dropped back to
"please log in" as soon as I type the semicolon.  However, if I type
"JMS:X575", the circuit is built to Office-11.  Same thing happens when
trying to TAUX out of 930.  Why does ND6657 treat host 575 special?
Received: from B39.Tymnet by X930.Tymnet; Wed, 22 Oct 86 15:56:20 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 22 Oct 86 15:54:01 PDT 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: setop 

I've been getting complaints about SETOP not being on 39.  I noticed
you were the last to write on it.  The program that's there seems to
just exit 1, without doing anything.  39 should be our master password
system in BUBBNET, and 36 should be a slave.  Any comments?  I have a
copy of the old SETOP I can put back in SYS if that's all that needs
to be done.  Thanks -Bill
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 22 Oct 86 18:15:35 PDT 
To: wrs, Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: B39 crashes. 

System 39 has 2*MAPAL1, 3*EUE, 2*CHTFNF, 2*FILANF, FILAMO+FILAUU dumps.
I have not finished looking at all of them, but here's what I found so far.
MAPAL1 - DDB+DEVRIB has a bogus retrieval pointer, RBREAL (1B1) not on.
EUE - Part of the SETPRV uuo overwritten by an ATB.
EUE - PC matches P1 which is on top of stack, wild POPJ during DEVHNG.
EUE - Part of the CORE uuo overwritten by an FNB.
FILAMO+FILANF+FILAUU can all be caused by an ATB pointer not really pointing
to an ATB.
CHTFNF - Mapped page being unmapped, can't find core page number in CHT.

CRA008(EUE) and CRA009(MAPAL1) occured while running B39-P034/P-13.
Those two dumps look almost exactly like the EUE and MAPAL1 dumps from
running B39-P034/P37.

Maybe we should try expanding the amount of memory that can be used for
creating DDB/ATB/NMB/DRB blocks.  Running C55's monitor on B39 (which is
cut for 80 jobs instead of 127) will have a side effect of making more
core available for these blocks.

			/Joe
Received: from B39.Tymnet by X930.Tymnet; Thu, 23 Oct 86 11:59:44 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 22 Oct 86 15:54:01 PDT 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: setop 

I've been getting complaints about SETOP not being on 39.  I noticed
you were the last to write on it.  The program that's there seems to
just exit 1, without doing anything.  39 should be our master password
system in BUBBNET, and 36 should be a slave.  Any comments?  I have a
copy of the old SETOP I can put back in SYS if that's all that needs
to be done.  Thanks -Bill
Received: from B39.Tymnet by X930.Tymnet; Thu, 23 Oct 86 11:59:49 PDT
Received: from X930.Tymnet by B39.Tymnet; Wed, 22 Oct 86 18:29:02 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 22 Oct 86 18:15:35 PDT 
To: wrs, Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: B39 crashes. 

System 39 has 2*MAPAL1, 3*EUE, 2*CHTFNF, 2*FILANF, FILAMO+FILAUU dumps.
I have not finished looking at all of them, but here's what I found so far.
MAPAL1 - DDB+DEVRIB has a bogus retrieval pointer, RBREAL (1B1) not on.
EUE - Part of the SETPRV uuo overwritten by an ATB.
EUE - PC matches P1 which is on top of stack, wild POPJ during DEVHNG.
EUE - Part of the CORE uuo overwritten by an FNB.
FILAMO+FILANF+FILAUU can all be caused by an ATB pointer not really pointing
to an ATB.
CHTFNF - Mapped page being unmapped, can't find core page number in CHT.

CRA008(EUE) and CRA009(MAPAL1) occured while running B39-P034/P-13.
Those two dumps look almost exactly like the EUE and MAPAL1 dumps from
running B39-P034/P37.

Maybe we should try expanding the amount of memory that can be used for
creating DDB/ATB/NMB/DRB blocks.  Running C55's monitor on B39 (which is
cut for 80 jobs instead of 127) will have a side effect of making more
core available for these blocks.

			/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 23 Oct 86 23:17:53 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Hotline call for F28 

23-Oct-86 22:30/Joe
Contact:  Fremont Operations, (415)498-2589, Mike, System F28
Problem:  Incrementing hard read errors, on one particular cylinder.  FSC
          checked the drive, OK, problem moved when pack was moved.
          They already copied out this pack once today, this new duplicate
          has its own set of bad spots.  Note: They were unable to copy the
          pack on F28, and did it on F40 (an IBM system) instead.
          FNDBAD reported 3 files with HRE.
Solution: Told them to rename the files to *.BAD and restore good copies from
          tape.  The files are now (NETSTAT)61014A.BAD, (SRA)PSPDAT.BAD, and
          (MSNTRACK)DUNS.BAD.  The file in NETSTAT is 10978 pages long.
Remark:   I will write a program tomorrow to free up all except the bad pages.



Need to find out why they could not copy the pack with PAKCOP.  They should
not have used an IBM to copy it - the problems they are having now are a
direct result of not having alternate tracks assigned on the destination pack.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 24 Oct 86 11:35:27 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, William R Soley <WRS@B39.Tymnet> 
Subject: MDC has 70 PDP-10 CPUs. 

Yes, seventy of them.  For a list of their serial numbers, go into the INFO
tree, menu SYSTEM, item HARDWARE.   /Joe
Received: from X95.Tymnet by X930.Tymnet; Fri, 24 Oct 86 18:14:25 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 24 Oct 86 17:51:03 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet>, CARL@X930 
Subject: Origination node in version 257 

I see that 95 is in 6657's routing table and it told 2770.  Now when I log in
from 2770 to "JMS:95", the needle reports 6657 as node of origination, since
that is where connection goes into TYMNET.  This is probably a good thing, but
was unexpected.  Now when (SYS)LOGON tries to set terminal type based on node
number, it thinks its the ADM3A on 6657 instead of the VT102 on 2770.
(LOGON has been confused ever since the Sup quit reporting the physical port
of origin.  It was nice while it lasted).

What command is used to get XRAY to display the routine table?  I'm curious
to see what other hosts are "known" to exist in the micronet.  Is there a
file with a list of all the commands to XRAY?

		/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 27 Oct 86 15:08:12 PST 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Cc: Carl 
Subject: Status of (DERNST:930)SV2346.IMG 

It works, in that I can now log in to hosts 148 and 575 from 2770 and 6657.
But I cannot log in to 95 from 2346 because of "HPATH 95,6760", which should
be "HPATH 95,6760,TYMNET".

Suggested changes to RULAB.MAC, from Joe Smith 27-Oct-86

Comment out all references to host 118X in the Fremont micronet, but don't
delete the lines.  118X is a nonexistant host on node 2346.

For node 2346
  remove NPATH entries for 3203,6533,6535
  remove HPATH entries for 148 and 575
  for HPATH 95, add TYMNET (this oversight prevents me from logging in to 95)
  for HPATH 62, add TYMNET
  change to HPATH 118,6657,TYMNET
  change to HPATH 930,6657,TYMNET

For node 2770
  remove NPATH entries for 6533 and 6535
  change to HPATH 118,*,TYMNET

For node 3203
  remove semicolon to add back NPATH 6533, 6533
  remove HPATH entries for 930 and 934

For node 6533
  remove NPATH entries for 6657, 2770, 2346
  remove HPATH entry for 930, 118, 118X

For node 6535
  remove NPATH entries for 6657, 2770, 2346
  remove HPATH entry for 930, 118

For node 6657
  remove NPATH entries for 3203, 6533, 6535
  remove HPATH entries for 148, 575
  change to HPATH 118,2770,TYMNET

For node 6760
  remove NPATH entries for 3203, 6533, 6535
  remove HPATH entries for 148, 575
  for HPATH 118, add TYMNET

For node 10115
  remove NPATH entries for 3203, 6533, 6535
  remove HPATH entries for 148, 575
  for HPATH 118, add TYMNET

I have done all this in (JMS)RULAB.MAC on 39 and 930.   /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 29 Oct 86 0:05:43 PST 
To: CARL 
Subject: 10476 pages in 247 files on (UN1). 

10 months!  Use it or lose it.  We need space to build monitors.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 29 Oct 86 0:22:45 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Why PAKCP5 got hung while accessing unit 340. 

When there is something to do in the block multiplexor list, the SA10 sends
commands to the devices, one right after another.  The 3674 controller will
respond to the first one, and will reject all succeeding commands, saying
"I'm busy now".  PAKCOP sees the busy flag, and sets a bit for the unit saying
that it needs to be restarted when the control unit is available.  When the
3674 finishes that first command, it will cause an interrupt with the CU-END
bit set for for the first unit on the controller (disk unit 300).
PAKCOP checks to see if any other units on this controller got the BUSY status.
If so, those commands are put back into the block mux list and the SA10 is
restarted.  This works just fine for the first 32 disk units on the controller.

The problem is that the bits for units 340-377 are stored in a different word
than the bits for units 300-337.  When unit 340 is busy, it reports 340 busy,
but when the 3674 is available, it reports unit 300.  So PAKCOP got the CU-END
interrupt for 300, saw that 300-337 were not busy, and kept waiting for the
interrupt that said to restart I/O on units 340-337.

I patched the KONEND routine to check for waiting units on all controllers
when any CU-END was detected.  Voila - PAKCP5 saw all the units.

I'm still working on a solution to the contention problem.  I could format
units 300 and 301 together, 340+341, 300+341, and 301+340.  But attempting
to format units 300+340 or 301+341 caused "UNRECOVERABLE ERROR - LENGTH ERROR".

/JOE

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 30 Oct 86 15:59:36 PST 
To: Joe Smith <jms@X930.Tymnet>, dencoff 
Subject: (UN1) Files 

I will gather some data on sunday... but if you want space back before then
please just backup the directory and delete it...

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 31 Oct 86 13:37:12 PST 
To: CARL, DENCOFF 
Subject: PPN of SUBMIT changed. 

I changed the PPN of SUBMIT to [3,416154] on 930,62,95 so that it matches the
rest of the systems.  I defined HELGEM to be [3,16154] on our 3 systems.
The LPASSWORD needs to be reset for SUBMIT.   /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 3 Nov 86 18:31:14 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Host 4323 is SAN FRAN. OUTDIAL 

To use the San Francisco OUTDIAL facility, log in to host 4323.  For the OUTDIAL
hosts that use Tymshare model 923 modems, you dial by typing in the letter K
(without a RETURN), wait for the modem to prompt "DIAL:", then enter the seven
digits to be dialed in the 415 area code, end with RETURN.  For example:

please log in: JMS:4323;

PORT NUMBER   1
HOST NUMBER 4323
SAN FRAN. OUTDIAL

TYMSHARE MDL 923 1.37B
1200 BAUD
$K
DIAL: 9741300

DIALING ..9741300                       ONLINE!
x||x`|xxx<`xx`xx<`xx<~xx@x<`x|x@xxxx<~xxterminal identifier
-3706-024-
please log in: jms:930;

[LONMSG San Francisco]
[LONSTD  3-Nov-86 18:28:31 Frame: 5 on TTY13 Network: 3706-107]

.sys .
 5* SanFrancis #13 SYSTAT 32      RN         4.37  0:00:19 JMS +$

.bye
6.52 tru
terminal time: 0:00:27

please log in: ;
user name: ;
user name: ;
user name: ;
user name: ;
user name: ;
user name: ;
user name: ;
please see your representative
if you are having trouble logging in
CIRCUIT TO TERMINAL DISCONNECTED

please log in:

Note that typing semicolon 8 times in a row will convince the TYMSAT to hang
up on you.					/Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 4 Nov 86 16:31:50 PST 
To: txstext 
Subject: "Nw santa clara number 408-986-0646"... 

(begin forwarded message)

From: CARL@X930.Tymnet 
Date: Fri, 17 Oct 86 2:47:20 PDT 
To: jms, osman, dencoff 
Subject: "Nw santa clara number 408-986-0646"... 

Nw santa clara number 408-986-0646  node 4165:03-xxx-

Hmmmmmm.

(end forwarded message)

Cherie,
  This is a new number I found for the Santa-Clara area.
It should be less noisy... please post it in the office.

  408 - 986 - 0646

/Quack
From: TXSTEXT@X930.Tymnet 
Date: Tue, 4 Nov 86 16:38:52 PST 
To: CARL 
Subject: "Waddle, Regarding you last mail"... 


Waddle,

Regarding you last mail message:

ppppphhhhhttttt!!!!!

Yours,

Quack
From: CARL@X930.Tymnet 
Date: Tue, 4 Nov 86 16:43:24 PST 
To: cherie 
Subject: "Huh? I thought that you'd like"... 

Huh?
  I thought that you'd like to know another number that works...
especially since x6001 and x7111 no longer work... sigh.
From: TXSTEXT@X930.Tymnet 
Date: Wed, 5 Nov 86 14:05:50 PST 
To: carl 
Subject: "Re: You last mail msg. of 4 NOV"... 


Re: You last mail msg. of 4 NOV 86 - Thank you.

Cherie
From: TXSTEXT@X930.Tymnet 
Date: Wed, 5 Nov 86 15:06:41 PST 
To: carl 
Subject: "Carl: The Rainbow is out of paper."... 


Carl:

The Rainbow is out of paper. 

Cherie
Received: from X62.Tymnet by X930.Tymnet; Thu, 6 Nov 86 1:39:38 PST
Return-path: <TXSTEXT@X62.Tymnet> 
From: TXSTEXT@X62.Tymnet 
Date: Wed, 5 Nov 86 13:22:28 PST 
To: carl 
Subject: "Carl, You can come home this evening, "... 


Carl,

You can come home this evening, if you want. I don't want you to stay at work j     just to pout. 

Sincerely,
Cherie
Received: from X62.Tymnet by X930.Tymnet; Thu, 6 Nov 86 1:39:42 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 6 Nov 86 1:35:16 PST 
To: dencoff 
Subject: "Re: JOBPRC.DOC JOBPRC.RND output"... 

Re: (CARL:62) JOBPRC.DOC <== JOBPRC.RND  output from (SPL)DOCGEN

Please type it out on hardcopy, width 80 paper and review it for
typos, glitches and whatnot.  It still needs some small fixups just
to get a "printable" copy, but my printer ran out of paper and I
could not easily look at the entire document to fix everything...

After I see it on paper, I can make the fixups...

/Carl

PS>  Some sections still need to be written.... I put in the headings
but didn't fill in the vbody of text.  either wait for me, or make the
corrections on paper and I'll show you what needs to be said to DOCGEN
to get the desired results.

/Carl
Received: from X62.Tymnet by X930.Tymnet; Thu, 6 Nov 86 8:51:13 PST
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Thu, 6 Nov 86 8:46:05 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: (CARL:62) JOBPRC.RND. 

Carl,

  We need to get together, re. desired final output.  I have, as you
requested, printed a copy of the .DOC file.  I will have marked it up for
modification, after discussion.  Thanks for all the time and knowledge you
have put into this doc.
/D.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 6 Nov 86 14:53:03 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Names of the monitor, "/A" or not "/A". 

When I tried to create P035, I had a lot of hassles from MONED1.  It refused
to let me specify a monitor name without a slash.  Therefore I skipped ahead
to P035/A.  But then MONED got obstinate.  After much gnashing of teeth, new
SYSIDs were assigned and CNFNxx.MAC updated for all systems.  At that point,
I decided it wasn't worth playing games by MONED1's rules, and decided that
we really want P035 without "/A".

In OSP, the current CNFNxx.MAC files have been amended to say P035.  When
the people in distribution use "LOAD/SAVE:xx035 @MONxx", it will produce
P035 - this is the released monitor version.

However, if the CONFxx.MAC files need to be updated (increase jobs, more
disks, etc), then MONED1 must be run, and this will create P035/A.
Therefore, after the monitors have been distributed, (OSP) will produce
only P035/A.

The next monitor to be released after this one will be P035/B.

			/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Fri, 7 Nov 86 23:39:28 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Did not test 3652 on D25 Friday night. 

System 25 was having problems with 3675 and/or 3672.  They had been down
since 3:00pm our time, and did not get back up until 10:00pm our time.
Since I at least one hour from being ready, I told them bring the system
up for users, we will reschedule another night.   /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 8 Nov 86 13:46:09 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: hotline calls 


08-Nov-86 02:10/Carl
Contact:  Dallas Operations, (214)637-7445, Greg, System 25.

Problem:  System 25 had been down the previous evening with disk or
          controller problems.  Joe was expecting to use the system for
          testing but was unable to have access until after 10 PM.  They
          has some minor problems which Joe talked them through when the
          system was brought up.  This call was to find out how to delete
          some files DSKCLN had marked as bad.  The file names were all
          jumbled punctuation characters with a few letters thrown in.

Solution: I checked the ribs and the UFD data pages for errors and found
          everything intact.  Then I renamed all the files except the bad
          ones into OSNF and deleted *.* to remove the weird file names.
          I restored the files to BASELINE and told operations to restore
          from the all-files and incrementals anything that was lost.

07-Nov-86 18:00/Carl
Contact:  Fremont Operations, (415)498-2588, System 74.

Problem:  System crashed with no CTY output during the ASP restore.

Solution: Initially told operations that there should be no reason that
          they could NOT continue the restore at the beginning of the
          current tape.  Talked to Osman and found that the reason for
          the rebuild was that they were having problems since the head
          crash earlier this week.  I called operations back and told
          them that it would be better to restart the restore from tape
          one, to insure that everything was in order.


/Carl
Received: from B39.Tymnet by X930.Tymnet; Sat, 8 Nov 86 21:39:01 PST
Return-path: <CARL@B39.Tymnet> 
From: CARL@B39.Tymnet 
Date: Sat, 8 Nov 86 21:32:15 PST 
To: wrs 
Subject: "Bill, I have added to both the"... 

Bill,
  I have added  "&#cr&#lf" to both the two "T" mode break tables in ARGS
and to the one "T" mode break table in ARGPAR.  This change apparently
does the trick of removing the trailing CRLF from the .TMP file read at
CCL entry.

  ARG.SAI and ARGPAR.SAI are in your directory on 39.   (my new code)
  ARGS ^^
  ARGS.REl and ARGPAR.REL are in your directory on 36 & 39. (new code)
  ARGS.WRS and ARGPAR.WRS are your original module on any /all systems.

  (WRS:36)FREE.SAV is relinked with the new code and appears to work
with the new XEXEC.  I am moving a copy of this to the Tymnet code
generation systems.  I would like you to rebuild FREE or provide the
sources in a transmittal package so that we may have it archived and
transmitted...  what this means is removal of "(WRS)" from the .REQ
files... sigh!

  Let Joe or me know when you have it ready.   /Carl

Received: from X62.Tymnet by X930.Tymnet; Sat, 8 Nov 86 23:14:49 PST
Return-path: <TXSTEXT@X62.Tymnet> 
From: TXSTEXT@X62.Tymnet 
Date: Fri, 7 Nov 86 10:41:30 PST 
To: carl 
Subject: "Hi! It's me! Remember yesterday, "... 


Hi!

It's me! Remember yesterday, when I told you that every time I log in, my right margin switch is always set to 72, and you told me it shoulld be set to 80? Well, today I tried to input the Christmas lists, it's still set at 72, I tried to c 
change it ti 80, and now it won't even let me change it!!! GRRR!

Please help! ILYVMADFYFI! If you remember, please call before you leave to
come home, so I can have supper started, okay? Bye, love.
/Cherie
Received: from X62.Tymnet by X930.Tymnet; Sat, 8 Nov 86 23:14:54 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sat, 8 Nov 86 3:44:49 UT 
To: dernst@b39 
Subject: "Dennis, micro WHY DOES THE MICRONODE"... 

Dennis,
  &*^%)*)(&^&*^*)_*)* micro *(&*&(*&(&*(&^

  WHY DOES THE MICRONODE TAKE MY <FF> (octal 14) CHARACTERS AND CONVERT
  THEM TO 8 LINE-FEED CHARACTERS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  I WANT TO BE ABLE TO SEND whatever CHARACTERS i WANT out through AN
  AUX-circuit on THE micronet BASES!!!

  ******************

  I've revived my project to print files on a local printer, eg. TI800
  (TYM 757) or some other printing device connected to a port on the
  micronode and it seems that when I send a form feed, it gets changed
  into 8 line-feeds.   This is MOST unsatisfactory!!!

  ******************

  Could you please generate some code for 2770 that allocates 2 or 3 of
  the high-end (or low end, just let me know so I know where to put the
  cables) ports to allow me to use them as remote printers...

  I know that you mentioned that if we had host numbers, we could just
  build a circuit to that host (or subhost) and it could be some sort of
  dedicated line, for us to use like this...

  We also have access to a DEC LN03, which I want to connect to a line and
  send data through it... 

  If possible, I'd like these lines to not have to time-out or print PLEASE
  LOG IN:  or  PLEASE SEE YOUR REP  messages.  Either set at 9600 (one for
  the LN03) or 2400/1200 (for the TI-800 printer/terminals)  or selectable
  through some mechanism.

  *******************

  Please give me a call on Monday afternoon....

/Carl
From: CARL@X930.Tymnet 
Date: Sun, 9 Nov 86 14:19:38 PST 
To: jms 
Subject: "What time did the beeper go off?"... 

What time did the beeper go off?
   I was at the rink (beeper in car) from 12 to 1:30 and got
home a little after 2 and called them, you had already called...

My beeper only went off once...  did yours go off more than once?
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 9 Nov 86 18:43:45 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: system 26 problems 

System 26 has had 2-3 bad sat stopcodes and 1 USRUS1 stopcode.  It was
related to me after not finding anything that they were doing an all-files
when each of these occurred.  ... They were on OPER at the time... all the
file sin OPER seem OK... the next directory is OSERANG with one file and
that is OK... the next directory is OSMAN and that seemed ok... I say
seemed, because after I did a directory of OSMAN and ran CKSUM on the first
and last few files  DIR (OSMAN)/UNSORTED  then we got the rib error and
then any lookup for files with the UFD in that directory failed...

The BAD SAT is on BPA3, page 10 ... with a likelyhood that one of the
system ufd pages reside there....

anyway, I told them to PAkCOPy the pack to another drive and see if
the problem goes away with a full dskcln, since the problem seems to
be intermittant... but mostly that drive.   I haven't entertained the
prospect of memory or controller beig bad, since everything else seems
to work fine...

/Carl
Received: from X62.Tymnet by X930.Tymnet; Sun, 9 Nov 86 23:51:28 PST
Return-path: <LWALTON`N`X62.Tymnet@X62.Tymnet> 
From: Larry & Linda <LWALTON`N`X62.Tymnet> 
Date: Sun, 9 Nov 86 23:50:24 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: mail test 

Tahnk you for setting up our phone modem, Carl!
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 10 Nov 86 0:59:43 PST 
To: carl 
Subject: PEECOM version 1(3) 

I modified PEECOM to not hang on a null and rescan problerly if no semicolon.
Now when I try ".PCOM CPUS/WATCH", the message
	VERSION NUMBER MISMATCH 16777220
come out when PEECOM starts.  If I hit CR, it runs ok.  Where does message
come from?  /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 10 Nov 86 2:20:23 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Status of C26. 

I had them force a fast DSKCLN, then I logged in thru the MUX, SET OPR on one
job, and ran CKSUM on (OPER), (OSERANG), (OSMAN).  They ran TITO starting with
some directory in the N or O range.  The following came out on my OPR terminal:

?INFO stopcode "BADSAT" at 09-Nov-1986 20:11:12
C26, hardware ZZ, KL-1286, base 4200, C26-P034/P37, 9-30-86, M01778
   SAT IO: 000000   STT: 200153   STTFPC: 001133   SATCNT: 001132   SATBRD: 0
Requesting FULL DSKCLN
 ?
? BAD SAT ON BPA4 AT PAGE 10

!psp
DSKB:   103253
BPA4(DSKB7:260007):  SATFAIL:1

SATIO=0 means no read errors detected.  STT=200153 is the address of BPA4's
first SAT page.  STTFPC=1133 is the monitor's last known free page count for
this SAT.  SATCNT=1132 is current count of free pages.  SATBRD=0 means this is
the first reported bad read for any SAT.

SWAMP is complaining that SATCNT does not equal STTFPC.  This means that one
of the F or M bits got set to unavailable (zero) behind SWAMP's back.  Either
some routine marked one of the SAT bits and did not decrement STTFPC, or what
was read from the disk does not match what was in core when SWAMP/PCBIO
decided to swap the SAT page out.

This mangling of a bit in the SAT stored on page 10 has occurred on three
different units on C26 today, it occurred twice on C70 when it moved to 3650
disks, and it occurred twice on D34 when it moved to 3675 disks in Dallas.
That is, it does not appear to be a disk hardware problem, is not limited to
CPXKON, and could be a software or SA10 problem.  Adding the 1/2 meg of memory
to C26 may have aggravated the problem, I don't know.

I went into SYSTEM.SAV (26XP37) and boosted NMSTPC (Number of monitor SAT
PCB's) from 30 (2*15.) to 48 (DISKS*3).  This should cut down on the amount of
swapping of SAT pages.   /Joe

See also (M33)HOTLIN.LOG
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Nov 86 2:18:39 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Version number mismatch... 

Chances are it come from SAILs internal checking routines.  One or
more of th elibrary routines were compiled with FTSYS SAIL instead of
SYS SAIL most probably when we were doing all that work long ago.

If I get  a chance, I'll look into it.  PEECOM needs another switch
which can be passed to it with the request # for XEXEC so it can watch
the queue, and check on the log file.. thus giving the PCOM-like effect
of DCOM/WATCH although a ^C won't stop the job.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Nov 86 2:21:56 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: New version of FREE 

I've put a new version of FREE on 36, and in CARL on 54.  I need to copy
it around... but it should do the trick and we should be able to run the
new XEXEC system on the Tymnet code systems very soon.

/Carl    (progress)
Received: from X62.Tymnet by X930.Tymnet; Mon, 10 Nov 86 6:15:18 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Nov 86 3:23:22 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: *** Hooray! *** 

I found the problem with entering a specific date.  Apparently, a routine
to return the current date in the form MM DD YYYY was returning the year
as 1987 since sometime in october.  I changed the routine and the check for
past dates now works correctly again.

This bug was fixed in %1(157) of SUBMIT, now on SYS on 62.  TELECOpy
started to all systems immediately...

/Carl
Received: from X62.Tymnet by X930.Tymnet; Mon, 10 Nov 86 11:15:31 PST
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Mon, 10 Nov 86 9:25:22 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: New version of FREE 
In-reply-to: your message of Mon, 10 Nov 86 2:21:56 PST

Carl,

  So you're busy removing all known blocks to our deployment of XEXEC!  Con-
gratulations!  You did it so quickly, you made it look easy.  /D.
Received: from X62.Tymnet by X930.Tymnet; Mon, 10 Nov 86 11:15:36 PST
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Mon, 10 Nov 86 9:28:23 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: *** Hooray! *** 
In-reply-to: your message of Mon, 10 Nov 86 3:23:22 PST

. . . and your good work just continues.  . . .             /D.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 10 Nov 86 21:39:04 PST 
To: OSMAN 
Cc: carl 
Subject: System crashes if NMSTPC=143. 

We cannot use DISKS*13.  There is a maximum of 63 PCBs allowed.  Therefore
NMSTPC+NMRBPC cannot exceed 77 octal.  Until PCBIO is changed, we can NOT
keep all SATs in core.     /Joe
From: CARL@X930.Tymnet 
Date: Mon, 10 Nov 86 22:38:38 PST 
To: dernst@b39 
Subject: "Dennis, Hmmmm... I thought that"... 

Dennis,
   Hmmmm...  I thought that you said that SV2770.IMG would be out on 57
later this afternoon... It apparently never made it...  I got impatient
and copied (CURR11:39)SV2770.IMG to (CARL:930) and reloaded it.

It seems to have set the highest line to 1200 baud instead of 9600 baud.
i didn't have time to try out the other 3 lines, but assume that something
may not be right...

Did you finish?  (not on 57? so maybe you have to make some more changes?)
What I copied from 39 works fine, FF and all that... just at 1200 baud.

  I'll give you a call this next afternoon after our monitor group meeting.

/Carl
Received: from B39.Tymnet by X930.Tymnet; Mon, 10 Nov 86 22:48:07 PST
Return-path: <CARL@B39.Tymnet> 
From: CARL@B39.Tymnet 
Date: Mon, 10 Nov 86 22:45:48 PST 
To: dernst 
Subject: "I see... You switched them 1200"... 

...Oh I see...
  You switched them 1200 on the last 2, and 9600 on the next to last 2.

that's OK. As long as I know that that is what it is configured for. They
seem to work fine at 1200.  I'l try the 9600 line tomorrow to see what it
looks like.

/Carl
Received: from X62.Tymnet by X930.Tymnet; Tue, 11 Nov 86 19:45:34 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 11 Nov 86 19:43:40 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X62.Tymnet> 
Subject: BBOARD 

BBOARD is the username to send mail to to make bulletin board messages
available to all of us.

To read the messages you haven't seen... run MARKER.

.MARKER                      ; for thos of us with special command mode set
What file? (BBOARD)BBOARD.ARC
 
... and away we go ...

From: CARL@X930.Tymnet 
Date: Tue, 11 Nov 86 20:28:47 PST 
To: bboard 
Subject: "testing 1 2 3 4"... 

testing 1 2 3 4
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 12 Nov 86 11:58:53 PST 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Status of PAKCOP.. M E M O R A N D U M 

                                                      TYMCOM-X SUPPORT

   DATE:  11-November-1986
     TO:  ALL DATA CENTERS
   FROM:  Osman Guven

--------------------------------------------------------------------

     As of this memo there are two pack copy programs. PAKCOP.SAV and
     PAKCP5.SAV.

     PAKCOP.SAV is the same old pack copy which we have been using on
     the Memorex 3675's and Ampex DM-331's. You must still use this 
     program when you work with 3675's and DM-331's, but there have been
     some enhancement added to it.

     PAKCP5.SAV is the new version which only works with the Memorex 
     3650 and 3652 disks which we have started to put on the PDP10's.

     Operationally, procedures of both program are the same as before.
     Only one command REFRESH, has been changed to ZFORMAT.

     If a 3650/3652 pack can not be ZFORMATed on the PDP10 because of
     a defective track, which must be done at least once when they are
     put on the PDP10 to work with TYMCOM-X operating system, defective
     track could be rewritten (fixed) under IBM/VM system using FIDS
     SA (Surface Analysis). This has been allready tested and proven to work.

     If there is any question feel free to give me a call at (415)794-2513
     or send mail to user name OSMAN on PDP10' or FSC.O/GUVEN on ONTYM.
     Thank You..
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 12 Nov 86 12:46:35 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: BBOARD messages. 

Due to a limitiation in the TUMS mailing system, we cannot use the username
BBOARD.  TUMS assumes that we are trying to send mail to the UNIX BBOARD
system and snarfs up the mail...  sigh!

I've setup a mailbox for username INFO instead.  Untested as of this moment,
and this will only change if TUMS eats that mail also.  GRRRRRRR.

/Carl

Example:

   .SENDMA INFO
   Type letter:
   New LN03 printer is up on the micro-net.  To use the automatic spooling
   capability, simply copy the file you wish to have printed into directory
   (LPQ) on system 930 or 62.  At the moment no checking for binary files
   is performed, so please be careful what you copy into LPQ.

      Three formats are available for printing:

      Normal - Elite font, letter format, 60-61 lines by 88 characters
               all files not listed below are considered Normal

      Wide   - Compressed font, landscape format, 60-61 lines by 136 chars
               files with the extension .LSW are printed in this format

      8 LPI  - Compressed font, letter format, 80-82 lines by 100 chars
               files with the extension .LS8 are printed in this format

   If you wish normal printing of files with .LS8 and .LSW extensions, you
   must change the extension to something else (eg. .LST) when you copy it
   to (LPQ).   Please check to see if you are overwriting a file of the
   same name prior to copying your files to LPQ, otherwise someone else may
   take your printout.

   /Carl

From: CARL@X930.Tymnet 
Date: Wed, 12 Nov 86 13:07:56 PST 
To: info 
Subject: "New LN03 printer is up on the"... 

   New LN03 printer is up on the micro-net.  To use the automatic spooling
   capability, simply copy the file you wish to have printed into directory
   (LPQ) on system 930 or 62.  At the moment no checking for binary files
   is performed, so please be careful what you copy into LPQ.

      Three formats are available for printing:

      Normal - Elite font, letter format, 60-61 lines by 88 characters
               all files not listed below are considered Normal

      Wide   - Compressed font, landscape format, 60-61 lines by 136 chars
               files with the extension .LSW are printed in this format

      8 LPI  - Compressed font, letter format, 80-82 lines by 100 chars
               files with the extension .LS8 are printed in this format

   If you wish normal printing of files with .LS8 and .LSW extensions, you
   must change the extension to something else (eg. .LST) when you copy it
   to (LPQ).   Please check to see if you are overwriting a file of the
   same name prior to copying your files to LPQ, otherwise someone else may
   take your printout.

   /Carl
Received: from B39.Tymnet by X930.Tymnet; Thu, 13 Nov 86 1:32:32 PST
Return-path: <CARL@B39.Tymnet> 
From: CARL@B39.Tymnet 
Date: Thu, 13 Nov 86 1:30:24 PST 
To: wrs 
Subject: "Bug in LS program.... If one has"... 

Bug in LS program....

  If one has UNDELETE enabled, one gets funny named .TMP files in one's
directory... when one then runs LS to get a directory, LS seems to stop
when it hits one of thes bad files... in any case, it doesn't list all
of the user's files.  Even if it doesn't print the bad names, it ought to
continue with the rest of the real files...

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Nov 86 2:39:05 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: XEXEC 

XEXEC is now running on C29, C35, B36, C55, C57, D23, D27, D34, D56, D65
and D79.   Due to FREE, it is not running on C26, C33, C70, D25, D31, D37
and D54.   C22 and D72 are inaccessible at the moment, so no verification
can be made.

  As it is late, I'll check the Fremont data centeres tomorrow.

/Carl;
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 13 Nov 86 7:18:01 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: EXEXEC running in Fremont Datacenter. 

It is up on 28,30,38,58,74.   /JMS
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 13 Nov 86 13:24:51 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Node 3143 will be down sometime Friday night. 

NETCON called, node 3143 is scheduled to be reloaded with a new version of
the TYMSAT code after 10pm Friday night.  During that time, systems 62 and
node 2770 will not be connected to TYMNET.

This notification is a result of a request I made to have them ONTYME TXS.SUP
or call me at 2512 whenever 3143 or 3146 is scheduled to be down.   /Joe
Received: from X62.Tymnet by X930.Tymnet; Sat, 15 Nov 86 0:15:23 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Fri, 14 Nov 86 23:41:03 PST 
To: dencoff 
Subject: "Re: Modifications to the system"... 

Re: Modifications to the system

I made some minor changes to your PRPCHG.DOC file along with one
major revision.

*** MAJOR CHANGE ***
  :RESTART n                Not implemented.  Ignored.

This is now correct!  For the sake of conversion, it is ignored for
the moment.  Later, I may add it in when the Hold$Restart stuff is
working the way I'd like it to work.

XEXEC.RND/SUBMIT.DOC

The CCL option replaces the TMPName option completely.  The old CCL option
is redundant and setting the CCL name (or parameters) will flag the proper
start condition.  If the RUN option is set and parameters are given, the
CCL name will be setup automatically from the first 3 characters of the
command file name if a CCL option HAS NOT been issued previously.

The LOGOUT option has been added to allow a running job to log itself out
without first halting and waiting for the job processor to log it out.
(Waiting to be logged out hasn't really been tested fully, while programs
such as MAILER and ONTMTP log themselves out.  This bit must be set on
these programs, otherwise they will be marked Abort-Logout).

Abort-CCL has been removed.  Apparently Abort-CCL replaced Abort-Run in
an earlier version and Abort-Run was never re-implemented.  So, I have
renamed Abort-CCL to Abort-Run since that is the intent, and I have added
a new condition Abort-Logout which means the job went away without the
job processor's knowledge.  This is now an error.!

I will change the documentation to reflect these changes.

Effective in SUBMIT %1(162), XEXEC %1(173), EXECX %1(174).
Received: from X62.Tymnet by X930.Tymnet; Sat, 15 Nov 86 7:15:11 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sat, 15 Nov 86 4:13:03 PST 
To: submit 
Subject: "Version 1(165) and some earlier"... 

Version 1(165) and some earlier
Problem is that if you ABORT out of an insert, and possibly a modify
then proceed to insert a new record... the old record info is still
there... or at least some of it, eg. LOGFILE in my experience.

/Carl
Received: from X62.Tymnet by X930.Tymnet; Sat, 15 Nov 86 8:22:52 PST
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Sat, 15 Nov 86 7:50:37 PST 
To: CARL@X62.Tymnet 
Subject: Re: "Re: Modifications to the system"... 
In-reply-to: your message of Fri, 14 Nov 86 23:41:03 PST

I like your changes!  and you're gonna change the doc., ... thanks!  /D.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 15 Nov 86 13:43:47 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: 3650 problems. 

I've fixed the APRFAT crashes, BADSAT crashes, MAPAL1 crashes, and made
GIVPG1 be continuable.  Now we have SEAR14 crashes.  A summary of these
problems can be found in (M33)3650.BUG.  Sigh.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 15 Nov 86 13:46:29 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: C22 rebuild. 

15-Nov-86 11:35/Joe
Contact:  Cupertino Operations, (408)446-7705, Fred, System 22.

Problem:  Initialization failure - bad UFD.  DSKCLN not running.

          Logged in over shut, found OPER.UFD unreadable, as well as M33.UFD
          and others.  Ran TSTSUP, it reported DEVICE error while trying to
          read page 4 of [1,1].UFD data.  PSP reported:

BPA12(DSKB:220006):  SDATI(CTRL:2 DRV:H Data field ECC uncorrectable) SCONI(
 Unrecoverable data check Count error) SLBN(CYL:102 HEAD:17 RECORD:8) SDAT:5
 HDATAI(CTRL:2 DRV:H Data field ECC uncorrectable) HCONI( Unrecoverable data
 check Count error) HLBN(CYL:102 HEAD:17 RECORD:8) HDEV:44

Fix:      Told them to reformat BPA12 and rebuild system from ASP tapes.

From: CARL@X930.Tymnet 
Date: Sat, 15 Nov 86 20:09:34 PST 
To: jms 
Subject: "re: Weird startup problem on system"... 

re: Weird startup problem on system 72.

I tried to startup my XEXEC on system 72 last night and it just kept
sitting there without doing anything...

I looked at the job with FRMDDT and it was looping in the code I wrote
to sleep, loop back and try again inside the call to type a message to
the CTY using the talk job SETUUO function 10.  It appears that the
SETUUO was either failing or the CTY was being flooded with output and
it was returning a pointer to use to continue the uuo.  I donno... I
put a JFCL at the error address and it took off flying.

I've not heard anything about 72 going down... so this behavior was most
bewildering.  I thought you'd like to know, because I think that we need
to address the problem of the CTY being flooded with output, such as when
a detached job has LOGOUT on STOP set and caannot logout...  "?<cr>" is
typed over and over again...

/Carl
Received: from D54.Tymnet by X930.Tymnet; Sat, 15 Nov 86 21:08:21 PST
Return-path: <CARL@D54.Tymnet> 
From: CARL@D54.Tymnet 
Date: Sat, 15 Nov 86 23:02:00 CST 
To: jms@x930 
Subject: "New bit for bit x this data center"... 

New bit for (PJ)JOBS.DAT...
  bit x = this data center

maybe better - bit field  (2-3 bits) with the data center number.
thus, all programs like SUPCHK and (SPL)COPY and the like can look
at word 1 for the host number and find that host's entry to see
what data center this machine is in... then a sub-list can be made
of those systems in this particular center.

Then, (SPL)COPY or SUPCHK can look at only those hosts...

...since you'r updating PAM...

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 16 Nov 86 4:57:52 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Question about REBUILD proceedure. 

16-Nov-86 04:45/Joe
Contact:  Cupertino Operations, (408)446-7705, L.T. Smith, System 22.
Problem:  When rebuilding the system, do all the packs have to be the same?
Answer:   ASP dumps DSKB0, then DSKB1, etc.  It does not matter if DSKB0 has
          moved from BPA1 to BPA7.  If the system had 14 packs when ASP was
          run, then it must have 14 packs when restoring.  The physical
          location of the packs is irrelevant.

From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 12 Nov 86 11:58:53 PST 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Status of PAKCOP.. M E M O R A N D U M 

                                                      TYMCOM-X SUPPORT

   DATE:  11-November-1986
     TO:  ALL DATA CENTERS
   FROM:  Osman Guven

--------------------------------------------------------------------

     As of this memo there are two pack copy programs. PAKCOP.SAV and
     PAKCP5.SAV.

     PAKCOP.SAV is the same old pack copy which we have been using on
     the Memorex 3675's and Ampex DM-331's. You must still use this 
     program when you work with 3675's and DM-331's, but there have been
     some enhancement added to it.

     PAKCP5.SAV is the new version which only works with the Memorex 
     3650 and 3652 disks which we have started to put on the PDP10's.

     Operationally, procedures of both program are the same as before.
     Only one command REFRESH, has been changed to ZFORMAT.

     If a 3650/3652 pack can not be ZFORMATed on the PDP10 because of
     a defective track, which must be done at least once when they are
     put on the PDP10 to work with TYMCOM-X operating system, defective
     track could be rewritten (fixed) under IBM/VM system using FIDS
     SA (Surface Analysis). This has been allready tested and proven to work.

     If there is any question feel free to give me a call at (415)794-2513
     or send mail to user name OSMAN on PDP10' or FSC.O/GUVEN on ONTYM.
     Thank You..
From: Osman Guven <osman@X930.Tymnet> 
Date: Wed, 12 Nov 86 11:58:53 PST 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject:  Status of PAKCOP..
                             M E M O R A N D U M

                                                      TYMCOM-X SUPPORT

   DATE:  11-November-1986
     TO:  ALL DATA CENTERS
   FROM:  Osman Guven

--------------------------------------------------------------------

     As of this memo there are two pack copy programs. PAKCOP.SAV and
     PAKCP5.SAV.

     PAKCOP.SAV is the same old pack copy which we have been using on
     the Memorex 3675's and Ampex DM-331's. You must still use this 
     program when you work with 3675's and DM-331's, but there have been
     some enhancement added to it.

     PAKCP5.SAV is the new version which only works with the Memorex 
     3650 and 3652 disks which we have started to put on the PDP10's.

     Operationally, procedures of both program are the same as before.
     Only one command REFRESH, has been changed to ZFORMAT.

     If a 3650/3652 pack can not be ZFORMATed on the PDP10 because of
     a defective track, which must be done at least once when they are
     put on the PDP10 to work with TYMCOM-X operating system, defective
     track could be rewritten (fixed) under IBM/VM system using FIDS
     SA (Surface Analysis). This has been allready tested and proven to work.

     If there is any question feel free to give me a call at (415)794-2513
     or send mail to user name OSMAN on PDP10' or FSC.O/GUVEN on ONTYM.
     Thank You..
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 12 Nov 86 12:46:35 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: BBOARD messages. 

Due to a limitiation in the TUMS mailing system, we cannot use the username
BBOARD.  TUMS assumes that we are trying to send mail to the UNIX BBOARD
system and snarfs up the mail...  sigh!

I've setup a mailbox for username INFO instead.  Untested as of this moment,
and this will only change if TUMS eats that mail also.  GRRRRRRR.

/Carl

Example:

   .SENDMA INFO
   Type letter:
   New LN03 printer is up on the micro-net.  To use the automatic spooling
   capability, simply copy the file you wish to have printed into directory
   (LPQ) on system 930 or 62.  At the moment no checking for binary files
   is performed, so please be careful what you copy into LPQ.

      Three formats are available for printing:

      Normal - Elite font, letter format, 60-61 lines by 88 characters
               all files not listed below are considered Normal

      Wide   - Compressed font, landscape format, 60-61 lines by 136 chars
               files with the extension .LSW are printed in this format

      8 LPI  - Compressed font, letter format, 80-82 lines by 100 chars
               files with the extension .LS8 are printed in this format

   If you wish normal printing of files with .LS8 and .LSW extensions, you
   must change the extension to something else (eg. .LST) when you copy it
   to (LPQ).   Please check to see if you are overwriting a file of the
   same name prior to copying your files to LPQ, otherwise someone else may
   take your printout.

   /Carl

From: CARL@X930.Tymnet 
Date: Wed, 12 Nov 86 13:07:56 PST 
To: info 

   New LN03 printer is up on the micro-net.  To use the automatic spooling
   capability, simply copy the file you wish to have printed into directory
   (LPQ) on system 930 or 62.  At the moment no checking for binary files
   is performed, so please be careful what you copy into LPQ.

      Three formats are available for printing:

      Normal - Elite font, letter format, 60-61 lines by 88 characters
               all files not listed below are considered Normal

      Wide   - Compressed font, landscape format, 60-61 lines by 136 chars
               files with the extension .LSW are printed in this format

      8 LPI  - Compressed font, letter format, 80-82 lines by 100 chars
               files with the extension .LS8 are printed in this format

   If you wish normal printing of files with .LS8 and .LSW extensions, you
   must change the extension to something else (eg. .LST) when you copy it
   to (LPQ).   Please check to see if you are overwriting a file of the
   same name prior to copying your files to LPQ, otherwise someone else may
   take your printout.

   /Carl
Received: from B39.Tymnet by X930.Tymnet; Thu, 13 Nov 86 1:32:32 PST
Return-path: <CARL@B39.Tymnet> 
From: CARL@B39.Tymnet 
Date: Thu, 13 Nov 86 1:30:24 PST 
To: wrs 

Bug in LS program....

  If one has UNDELETE enabled, one gets funny named .TMP files in one's
directory... when one then runs LS to get a directory, LS seems to stop
when it hits one of thes bad files... in any case, it doesn't list all
of the user's files.  Even if it doesn't print the bad names, it ought to
continue with the rest of the real files...

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Nov 86 2:39:05 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: XEXEC 

XEXEC is now running on C29, C35, B36, C55, C57, D23, D27, D34, D56, D65
and D79.   Due to FREE, it is not running on C26, C33, C70, D25, D31, D37
and D54.   C22 and D72 are inaccessible at the moment, so no verification
can be made.

  As it is late, I'll check the Fremont data centeres tomorrow.

/Carl;
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 13 Nov 86 7:18:01 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: EXEXEC running in Fremont Datacenter. 

It is up on 28,30,38,58,74.   /JMS
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 13 Nov 86 13:24:51 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Node 3143 will be down sometime Friday night. 

NETCON called, node 3143 is scheduled to be reloaded with a new version of
the TYMSAT code after 10pm Friday night.  During that time, systems 62 and
node 2770 will not be connected to TYMNET.

This notification is a result of a request I made to have them ONTYME TXS.SUP
or call me at 2512 whenever 3143 or 3146 is scheduled to be down.   /Joe
Received: from X62.Tymnet by X930.Tymnet; Sat, 15 Nov 86 0:15:23 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Fri, 14 Nov 86 23:41:03 PST 
To: dencoff 

Re: Modifications to the system

I made some minor changes to your PRPCHG.DOC file along with one
major revision.

*** MAJOR CHANGE ***
  :RESTART n                Not implemented.  Ignored.

This is now correct!  For the sake of conversion, it is ignored for
the moment.  Later, I may add it in when the Hold$Restart stuff is
working the way I'd like it to work.

XEXEC.RND/SUBMIT.DOC

The CCL option replaces the TMPName option completely.  The old CCL option
is redundant and setting the CCL name (or parameters) will flag the proper
start condition.  If the RUN option is set and parameters are given, the
CCL name will be setup automatically from the first 3 characters of the
command file name if a CCL option HAS NOT been issued previously.

The LOGOUT option has been added to allow a running job to log itself out
without first halting and waiting for the job processor to log it out.
(Waiting to be logged out hasn't really been tested fully, while programs
such as MAILER and ONTMTP log themselves out.  This bit must be set on
these programs, otherwise they will be marked Abort-Logout).

Abort-CCL has been removed.  Apparently Abort-CCL replaced Abort-Run in
an earlier version and Abort-Run was never re-implemented.  So, I have
renamed Abort-CCL to Abort-Run since that is the intent, and I have added
a new condition Abort-Logout which means the job went away without the
job processor's knowledge.  This is now an error.!

I will change the documentation to reflect these changes.

Effective in SUBMIT %1(162), XEXEC %1(173), EXECX %1(174).
Received: from X62.Tymnet by X930.Tymnet; Sat, 15 Nov 86 7:15:11 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sat, 15 Nov 86 4:13:03 PST 
To: submit 

Version 1(165) and some earlier
Problem is that if you ABORT out of an insert, and possibly a modify
then proceed to insert a new record... the old record info is still
there... or at least some of it, eg. LOGFILE in my experience.

/Carl
Received: from X62.Tymnet by X930.Tymnet; Sat, 15 Nov 86 8:22:52 PST
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Sat, 15 Nov 86 7:50:37 PST 
To: CARL@X62.Tymnet 
Subject: Re: "Re: Modifications to the system"... 
In-reply-to: your message of Fri, 14 Nov 86 23:41:03 PST

I like your changes!  and you're gonna change the doc., ... thanks!  /D.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 15 Nov 86 13:43:47 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: 3650 problems. 

I've fixed the APRFAT crashes, BADSAT crashes, MAPAL1 crashes, and made
GIVPG1 be continuable.  Now we have SEAR14 crashes.  A summary of these
problems can be found in (M33)3650.BUG.  Sigh.
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 15 Nov 86 13:46:29 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: C22 rebuild. 

15-Nov-86 11:35/Joe
Contact:  Cupertino Operations, (408)446-7705, Fred, System 22.

Problem:  Initialization failure - bad UFD.  DSKCLN not running.

          Logged in over shut, found OPER.UFD unreadable, as well as M33.UFD
          and others.  Ran TSTSUP, it reported DEVICE error while trying to
          read page 4 of [1,1].UFD data.  PSP reported:

BPA12(DSKB:220006):  SDATI(CTRL:2 DRV:H Data field ECC uncorrectable) SCONI(
 Unrecoverable data check Count error) SLBN(CYL:102 HEAD:17 RECORD:8) SDAT:5
 HDATAI(CTRL:2 DRV:H Data field ECC uncorrectable) HCONI( Unrecoverable data
 check Count error) HLBN(CYL:102 HEAD:17 RECORD:8) HDEV:44

Fix:      Told them to reformat BPA12 and rebuild system from ASP tapes.

From: CARL@X930.Tymnet 
Date: Sat, 15 Nov 86 20:09:34 PST 
To: jms 

re: Weird startup problem on system 72.

I tried to startup my XEXEC on system 72 last night and it just kept
sitting there without doing anything...

I looked at the job with FRMDDT and it was looping in the code I wrote
to sleep, loop back and try again inside the call to type a message to
the CTY using the talk job SETUUO function 10.  It appears that the
SETUUO was either failing or the CTY was being flooded with output and
it was returning a pointer to use to continue the uuo.  I donno... I
put a JFCL at the error address and it took off flying.

I've not heard anything about 72 going down... so this behavior was most
bewildering.  I thought you'd like to know, because I think that we need
to address the problem of the CTY being flooded with output, such as when
a detached job has LOGOUT on STOP set and caannot logout...  "?<cr>" is
typed over and over again...

/Carl
Received: from D54.Tymnet by X930.Tymnet; Sat, 15 Nov 86 21:08:21 PST
Return-path: <CARL@D54.Tymnet> 
From: CARL@D54.Tymnet 
Date: Sat, 15 Nov 86 23:02:00 CST 
To: jms@x930 

New bit for (PJ)JOBS.DAT...
  bit x = this data center

maybe better - bit field  (2-3 bits) with the data center number.
thus, all programs like SUPCHK and (SPL)COPY and the like can look
at word 1 for the host number and find that host's entry to see
what data center this machine is in... then a sub-list can be made
of those systems in this particular center.

Then, (SPL)COPY or SUPCHK can look at only those hosts...

...since you'r updating PAM...

/Carl
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 16 Nov 86 4:57:52 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: Question about REBUILD proceedure. 

16-Nov-86 04:45/Joe
Contact:  Cupertino Operations, (408)446-7705, L.T. Smith, System 22.
Problem:  When rebuilding the system, do all the packs have to be the same?
Answer:   ASP dumps DSKB0, then DSKB1, etc.  It does not matter if DSKB0 has
          moved from BPA1 to BPA7.  If the system had 14 packs when ASP was
          run, then it must have 14 packs when restoring.  The physical
          location of the packs is irrelevant.

From: Joe Smith <JMS@X930.Tymnet> 
Date: Sun, 16 Nov 86 18:45:14 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, William R Soley <WRS@B39.Tymnet> 
Subject: Oper password file loses license when restoring from backup tape. 

16-Nov-86 18:15/Joe
Contact:  Cupertino Operations, (408)446-7705, Vijay, System 22.
Problem:  Could not set license after restoring from 14th's incremental.
Diagnosis: TITO complained about two files:
          (SYS)DRWSPR.EPO not given license WC
          (BOFANET)ONTYME.SAV not given license AC (SY,HF,RF,WF)
Fix:      Ran SETE then SETLIC on those two files.  System now up.
Note:     Carl and I have been discussing ways of giving license to TITO
          instead of to the operators.  We need to do something about it.

In this particular case, ASP restored all files with correct license.  Now if
TITO had all the license in the world, restoring files to SYS would not lose
license.  Giving license to TITO would be OK if
  1) TITO threw away license if not run by OPER.
  2) TITO threw away license if the TRANSLATE command was given.
  3) TITO kept license only for "RESTORE ALL" and the system number in the
     header matched the current system number.  (Operators would still need
     do set RF on their frame to do a STORE operation.)

This approach would not work if (SYS)TITO.SAV got wiped out and they had to
run TITO from tape.  Right now, only REFLAG gives the operator license when
running from tape.  We could make TITO use "SETE 777777,-1" to get all license
without having to take the system down, but it may be difficult to ensure that
only TITO gets to use it, and not some malicious program.

Or we could make it a restriction that if TITO gets wiped out, they have to
take the system down to get license from ONCE.  Then we could add a "TITO"
command to ONCE that takes device name as an argument (such as SYS: or FTA0:,
etc).  This command would tell ONCE to set CONSOLE-MANUAL mode, log in OPER
(without requiring ^Z L OPER), give the process all the license in the world,
then run TITO from the specified device.  This would avoid DDT and REFLAG and
give them a one-shot license for restoring files to SYS.  Once SYS was
restored and DSKCLN run, they could use the licensed copy of TITO on disk to
restore the rest of the files.

Any comments?
                                        /JOE
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 16 Nov 86 19:35:12 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Restore answer... 

Yes, ASP restore is a structure restore... there not only must be the
same number of packs, they must all have an intact structure (built with
DEFINE and then REFRESH, or left from a previous set of system packs).

What is on those packs doesn't matter.  For example, a system 25 pack-set
could be used for a system 27 rebuild, as long as the system is brought
up as system 27 (change system number) in case ASP checks things like that
in the tape headers.

/carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 16 Nov 86 19:43:29 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: TITO license 

I'm still in favor of the  SETE 777777,-1  idea, although make it only work
via UUO from usrname OPER.  We put that into TITO so that it can setup its
license correctly, but only have it set the license if the given command is
STORE ALL and no translates have been enabled.

I also like the ONCE command TITO or REBUILD to come up in console mode.
This would set both REFLAG and the DOCTY flag to be used whenever a rebuild
operation is needed, either ASP or TITO...

I think we need to write up a document with the changes in it, so we can
hammer at the pros & cons of each situation.  We've discussed various parts
of various proposals on this topic many times, and each time, we overlook
a situation that we don't feel is important, which after some thought is
the main reason none of theses schemes has ever been implemented.

I'm creating (M33)OPRLIC.INF for this purpose.
/Carl
From: CARL@X930.Tymnet 
Date: Sun, 16 Nov 86 20:37:45 PST 
To: jms 

(m33)OPRLIC.INF now has the text from your message and my message with a
fair amount of editing done to it to make things more concise.  After we
agree on what is to be done and what it will affect, we should implement
it, and soon, so thatit can be documented and given to operations.

XEXEC/EXECX/SUBMIT are all going to need us to fixup, since no one else
has QM.  I'll probably have to give it to SSINSTALL also.

/Carl
From: Osman Guven <osman@X930.Tymnet> 
Date: Sun, 16 Nov 86 22:07:19 PST 
To: Monitor Group <Osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: SHE is here.. 

Well.. SHE is here..Yeahhhhhhh.....
She is born on Nov 14, 7:21 am and named her LAYLA MARIE....
I will stay home monday and use that as my last Labor Day
holiday, see you guys later..
-Osman-
Received: from F28.Tymnet by X930.Tymnet; Sun, 16 Nov 86 23:34:13 PST
Return-path: <CARL@F28.Tymnet> 
From: CARL@F28.Tymnet 
Date: Sun, 16 Nov 86 23:33:22 PST 
To: submit@x62 

F28 perp users JKEARNEY, PCM, ENSS, PGRADE
Received: from C22.Tymnet by X930.Tymnet; Sun, 16 Nov 86 23:44:42 PST
Return-path: <CARL@C22.Tymnet> 
From: CARL@C22.Tymnet 
Date: Mon, 17 Nov 86 2:44:02 EST 
To: submit@x62 

system 22 perp users TYMSTAR, PROJMGT, WRFS, WRFSC
From: Joe Smith <JMS@X930.Tymnet> 
Date: Mon, 17 Nov 86 11:45:13 PST 
To: carl 
Subject: XEXEC got stuck on D34. 

It had been in TI wait for over 48 hours.  I attached to it and got:
RECUUO CALLED IN NEW RECORD SYSTEM.  RECOMPILE.
Called from 123772  Last SAIL call at 22562
^^C
.VER
XEXEC 1(167)
From: JMS@X930.Tymnet 
Date: Mon, 17 Nov 86 16:47:33 PST 
To: carl, dencoff 

LN03 summary is in (INFO)MISC.NFO, in the INFO tree as
GENERAL/MISC/LN03.

From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Mon, 17 Nov 86 20:59:57 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: XEXEC Deployment. 

I've looked at the versions of SUBMIT, XEXEC & EXECX on SYS and on SUBMIT on
the various hosts.  (I didn't look at our lab hosts.)

The hosts with the latest edits (respectively (167), (172) & (174)) in SYS
are:  22,26,54.

The hosts with these latest edits in SUBMIT are:  35,27,72,28,58,74.

The hosts completely lacking this edition are:  29,34,55,57,23,56,65,79,30,38.
I'm 'DO COPY'ing the latest edits onto SUBMIT on these hosts.  We should be
able to get the latest everywhere tomorrow.   /D.
Received: from B39.Tymnet by X930.Tymnet; Tue, 18 Nov 86 13:16:12 PST
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 18 Nov 86 13:08:07 PST 
To: CARL@B39.Tymnet 
Subject: Re: "Bug in LS program.... If one has"... 
In-reply-to: your message of Thu, 13 Nov 86 1:30:24 PST

I agree it ought not stop (of course).  I would guess its a problem in
the Dave Smith REDUFD routines.  -Bill
(I'll put it on the list)
From: TXSTEXT@X930.Tymnet 
Date: Tue, 18 Nov 86 13:48:38 PST 
To: carl 

Hi!

I got a call from the chiropractor's billing office yesterday;  they asked me
to send them one of McDonnell-Douglas' insurance claim forms so they can bill
for our insurance coverage.  Please bring one home tonoc, if possible.
Thanks!                          /Quack (Waddle)
Received: from B39.Tymnet by X930.Tymnet; Tue, 18 Nov 86 14:02:03 PST
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 18 Nov 86 13:43:32 PST 
To: Joe Smith <JMS@X930.Tymnet> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Re: Oper password file loses license when restoring from backup tape. 
In-reply-to: your message of Sun, 16 Nov 86 18:45:14 PST

The problem of license being lost in a restore situation is common.  What you
really want to insure is that the tape hasn't been tampered with.  If there
was a way to know when a tape has left the computer center, such as a mark
that would be made on the label, or a sticker that would be removed, then we
can safely restore all license to any file.  The problem is not limited to
(SYS).  In fact, it is worse for non-(SYS) files since they are usually more
obscure and fewer people are familiar with what license they should have.
Adele and I talked and talked about this problem, and I believe the solution
that was to be used in SNARL was to give the program all license.  It would
use its license to restore files only if the tape was written as either
  DUMP ALL   or   DUMP ALL CHANGED
The problem is, this information is not available in TITO.  I think the
next best thing is to either (1) change TITO to make a note on such tapes,
(2) require the operator to specify what kind of tape it is.  The first is
obviously preferable.  In the case where TITO does not have enough license,
it will complain when it first starts up, instructing the operator to either
use some ONCE command to get license, or to call the hotline or whatever.
Comments?  -Bill
Received: from C35.Tymnet by X930.Tymnet; Tue, 18 Nov 86 17:52:25 PST
Return-path: <CARL@C35.Tymnet> 
From: CARL@C35.Tymnet 
Date: Tue, 18 Nov 86 17:48:13 PST 
To: wrs@b39 

Re:  RECUUO .... IN NEW RECORD SYSTEM, PLEASE RECOMPILE

This message comes out of SAIL and aborts my XEXEC jo on some systems.
Not all of them, just some of them.  I've never seen this on 930 or 62.
Do you have any ideas on why this is happenning?

My only guess is that a record garbage collect isn't happenning.. but
there is no indication that this is the problem.  I do have a couple
of record structures which are close to or > 20 entries... which might
not be being deallocated...  I plan to put in a call to $RECGC to try
to force a garbage collect.... but, I'd like your opinion... first ...

Please try to respond ASAP, even if to say, "I donno- never seen it".
/Carl
Received: from F28.Tymnet by X930.Tymnet; Wed, 19 Nov 86 1:17:38 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 19 Nov 86 1:15:22 PST 
To: CARL 
Subject: XEXEC not running on F28, in ^C state. 

I restarted it.  You might want to ask operations what came out on the CTY
around 8pm (I'm not sure what day).  /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 19 Nov 86 1:23:22 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, WRS 
Subject: (SPL)DELPAG deletes usable pages from a bad file. 

I freed up about 1000 pages by creating DELPAG and running it on the files
in (BADFILES).  All the good pages were returned to the system, all the bad
pages are locked up in files that have the do-not-delete bit set.
See (SPL)DELPAG.DOC for more info.    /Joe
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Nov 86 3:19:58 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: System 72 

Joe,
  Remember I mentioned that the SETUUO to talk to another job didn't
work on system 72.  Well it still doesn't work.  It seems to work fine
on just about all other systems.  72 is running /P37!

It is very reliably broken on that system.  I wonder if there is something
wrong with the monitor image???

In any case, I'd like it checked... it seems strange that it runs well on
all the other systems.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Nov 86 3:22:34 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: XEXEC in TI and ^C state 

XEXEC seems to be goin gto ^C state, or TI state on some systems.
The job mix doesn't seem to have much to do with it.  I haven't
correllated whether the version number of XEXEC/EXECX has something
to do with it or not.  If you find it stopped, please make a note
of the version and report it to me.

Everyone,  if you find XEXEC stopped on any system , either restart it
or repor tit to me immediately so that I can cehck it.  Apparently
there is something stopping it.

/Carl
Received: from B39.Tymnet by X930.Tymnet; Wed, 19 Nov 86 3:34:05 PST
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 19 Nov 86 3:32:52 PST 
To: Joe Smith <JMS@X930.Tymnet> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Re: (SPL)DELPAG deletes usable pages from a bad file. 
In-reply-to: your message of Wed, 19 Nov 86 1:23:22 PST

How do things get into (BADFILES) in the first place?
-Bill
Received: from C35.Tymnet by X930.Tymnet; Wed, 19 Nov 86 4:00:36 PST
Received: from B39.Tymnet by C35.Tymnet; Wed, 19 Nov 86 4:00:23 PST
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 19 Nov 86 3:22:33 PST 
To: CARL@C35.Tymnet 
Subject: Re: "Re: RECUUO IN NEW RECORD SYSTEM, "... 
In-reply-to: your message of Tue, 18 Nov 86 17:48:13 PST

It must be an incompatibility in the object code if it expects that
recompilation will fix it.  Are you sure all your modules are compiled
with the same version of the compiler?  Alternatively, RECUUO leads
me to believe that the "old" record system used LUUO's for some record
operations.  This message is probably emited when the obsolete record
LUUO gets called in which case, it may be that you've jumped into some
non-code.  I've never seen it, but I do suspect record garbage collect
bugs in TUMS.  -Bill  (keep me posted, very curious)
Received: from C35.Tymnet by X930.Tymnet; Wed, 19 Nov 86 8:13:03 PST
Return-path: <DENCOFF@C35.Tymnet> 
From: DENCOFF@C35.Tymnet 
Date: Wed, 19 Nov 86 8:08:45 PST 
To: carl@x930 

at 8 am I checked SYSTAT for XEXEC, EXECX, MAILER, PAM & FREE.  on the following
6 hosts EXECX was not running:  22,29,33,35,72,79.  XEXEc was not running on 54.
~v
they are all running SUBMIT 1(165), XEXEC 1(172) & EXECX 1(174).  Idon't know
any more than that.  -Dennis-
Received: from X62.Tymnet by X930.Tymnet; Wed, 19 Nov 86 8:15:11 PST
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Wed, 19 Nov 86 7:44:52 PST 
To: (Monitor Group:), Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven
	<OSMAN@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet>, Tymcom-X Acct. Sup.
	<TXSSUP@X930.Tymnet> 
Subject: PERP REPLACEMENT INSTALLED 

The 'going away' version of the PERP and DEFER programs (which gives a
warning message before running the program, and which will run SUBMIT
after 20-NOV)is everywhere.  the system banner is up on all hosts 
except the four which currently have 'going down for p.m.' messages.
operators have been notified to put the banner up when theirs is taken
down.

Carl & I have been getting the most reliable version on all hosts, with
proper license and protection on ALL files.  I believe it's installed!
-Dennis-
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 19 Nov 86 10:02:05 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: RCHARG not working on 54. 

The hotline call for system D54 was a complaint that (SYS)RCHARG does not work.
Control-T shows that it is continually in the RN state, SYSTAT shows it has
no files open.  It has Control-C trapped, therefore the only way out of the
program is to turn off the terminal (zap the circuit).

Has this symptom been seen before?   /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 19 Nov 86 10:52:55 PST 
To: CARL 
Subject: XEXEC on 54 and 72. 

Host 54 ran out of ports.  XEXEC version 174 was SL, SUBMIT said it was running
the FREE job, but no job was logged in running FREE and MAILER was overdue.
I restarted XEXEC and did a RERUN on FREE.  FREE then killed 12 jobs in ^C state
but left the 10 jobs running 1022F4 that had been in TI for over 16 hours.
FREE needs two limits; number of minutes for jobs in ^C state and number of minutes
for jobs in TI state.

There is a bug in the counted string output routine in UUOCON.  If the error
returned in "no room", the byte pointer is correctly updated but the byte count
is returned in the LH of the AC instead of the LH of the word with the job 
number.  You can kludge XEXEC to get around this bug: If the RH of the value
returned in the AC is no-room and the LH of the AC is nonzero, then store
the LH of AC in the LH of first word of argument block.  I will create a
patch file.					/Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Wed, 19 Nov 86 12:14:04 PST 
To: CARL, Lois Lindley <Lindley@C35.Tymnet> 
Subject: RCHARG sources 

Where are the sources to RCHARG?  I could not find them anywhere looking
for RCHAR#.*.  Does it do something stupid like go into an infinite loop
when it can't build a circuit to a 940?  I could not find any documentation
other than the little bit in (SPPOPER:62).      /Joe
Received: from B39.Tymnet by X930.Tymnet; Wed, 19 Nov 86 14:02:13 PST
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 19 Nov 86 13:13:07 PST 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: YAMM 

Whatever happened to the new YAMM?  Where are the most current sources?
-Bill

PS - I can't find anything wrong with LS or REDUFD, except that I remember
that the .CHUFD Chanio will occasionally return nothing if it finds no
hits for a file in a hash chain.  This would cause REDUFD to stop since
it can't tell the difference between no data in this block and no data.
Any comments?
Received: from B39.Tymnet by X930.Tymnet; Wed, 19 Nov 86 14:02:20 PST
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 19 Nov 86 13:21:31 PST 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: example of ls breaking 

Do you have a reproducable example of ls not listing all the files?
That would make it easier, I think.  -Bill
Received: from X62.Tymnet by X930.Tymnet; Thu, 20 Nov 86 0:15:11 PST
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Wed, 19 Nov 86 23:42:50 PST 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: (SYS)JQUEUE.SYS needs QM lic. 

Carl,

  If the queue file needs QM license, we're going to need to kill xexec on
some hosts, temporarily.  Since it and/or EXECX has the file open in update
mode, I can't give the file license while the system is running (as far as I
currently know).  All other files are now accurately installed, I believe.

I think every host except 23, 35, 62, 79, 95 & 930 is running the latest
edition.  All files are installed with (believed to be) proper protection and
license, except for (SYS) JQUEUE.SYS.  I was unable to install QM on this file
in the following hosts without (I believe) taking the Job Processor system
down, because XEXEC and/or EXECX had the queue file opened in update mode.
It's in your hands.  the hosts are:  28, 29, 38, 54, 58, 70, 72, 74 & 79.

I looked in some of the queues.  the only glaring discrepancy I found was the
lack of (PJ)PAM on host 35's queue.

I'll finish updating the .DOC files tomorrow.

G'nite. ...
-D-
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 20 Nov 86 1:57:32 PST 
To: IPC.L/YARBOROUGH@ONTYME 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Status of 3652 testing as of Wednesday night. 

I finally got the system to recognize both halves the 3652 string.  It now
recognizes all 16 units (BPA0-BPA7 & BPC0-BPC7).  I was able to define a
two pack structure using both logical volumes of a single spindle.  The
system came up fine, refreshed DSKB, but hung while TITO was restoring
UFDs to the disk.  I may have lost a disk interrupt or had both units
waiting on each other.  Progress has been made, but I'm stopping for the
night.  I'll let you know if I find anything further.

			/Joe
Received: from X62.Tymnet by X930.Tymnet; Thu, 20 Nov 86 13:15:15 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Thu, 20 Nov 86 2:31:39 PST 
To: jms 

Re: .gtNM1 = JBTNM1 = '31, .gtNM2 = JBTNM2 = '32

I thought the CHGPPN uuo was supposed to set the name fields when
one either ran GFD or executed the uuo somewhere else... I GFD'd
to SUBMIT and the fields went to 0 instead of sixbit/SUBMIT/.

Is this broken also?  I have code in XEXEC and SUBMIT that depends on
to work...

/Carl
From: TXSTEXT@X930.Tymnet 
Date: Thu, 20 Nov 86 13:57:35 PST 
To: carl 

Dear Tony;

(Sounds strange, no?) Do we have any manila envelopes at home? If not, do you
have any at work? Heeeeeeeeeeelllllllllppppppppp!!!!!!!!!

Love, Wad

P.S. C.B., Call home!
Received: from X62.Tymnet by X930.Tymnet; Thu, 20 Nov 86 15:20:39 PST
Received: from X930.Tymnet by X62.Tymnet; Thu, 20 Nov 86 15:20:35 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 20 Nov 86 14:40:24 PST 
To: CARL@X62.Tymnet 
Subject: Re: "Re: JBTNM1 JBTNM2 I thought the"... 
In-reply-to: your message of Thu, 20 Nov 86 2:31:39 PST

If a PPB exists for that PPN when the CHGPPN uuo is executed, then JBTNM1
and JBTNM2 get set from that PPB.  If no PPB exists and no other frame is
GFD'd or logged in under that PPN, then JBTNM1 and JBTNM2 are zeroed.
The GFD program activates a PPB indirectly by looking up the UFD checking
for files with error, reading ACCESS.LIS, or by reading ACCESS.MSG.
(Unfortunately there is no channel open at the time of the CHGPPN uuo if
GFD has to create the UFD from scratch.)  Therefore if you use the GFD
program to go to an existing directory, JBTNM1 and JBTNM2 are set up.

If you put the CHGPPN uuo into the SUBMIT program, you should have the
UFD open as the time of the CHGPPN.                   /Joe
Received: from C35.Tymnet by X930.Tymnet; Fri, 21 Nov 86 8:31:24 PST
Return-path: <DIST124A@C35.Tymnet> 
From: Jim O'Connell <DIST124A@C35.Tymnet> 
Date: Fri, 21 Nov 86 11:28:27 EST 
To: carl@x930 
Subject: VUE documentation. 

     
     Carl, Bill Soley told me that you might be able to provide for me
     documentation for VUE.  If so, please let me know.  If not, could
     you help me locate someone who could get me a copy?  Thanks, Jim

From: TXSTEXT@X930.Tymnet 
Date: Fri, 21 Nov 86 8:55:34 PST 
To: carl 

Dear Tony,

Please don't forget to bring home a copy of the completed insurance form for the chiropoactor. I'd like to mail it and get them off my back tonight. Okay?
Love, Waddle
Received: from C57.Tymnet by X930.Tymnet; Fri, 21 Nov 86 12:05:17 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 21 Nov 86 11:18:46 PST 
To: carl@930 
Subject: C57, (OPER). 

I can't get there from here.  When I try to taux to OPERR:57 I get "access
barred".  I can't get in to re-start XEXEC.  -D.-
Received: from EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 21 Nov 86 16:41:55 PST
From: ISIPR.G/MARUS@Ontyme.Tymnet 
Date: C57 problem, ref A78374 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	ISIPR.G/MARUS@Ontyme.Tymnet 
Message-id: J17175@Ontyme.Tymnet 
Subject: C57 PROBLEM, REF A78374 

           McDonnell Douglas                      ISIPR.IRVINE   T-208
           Information Systems International
           Pacific Region                         Tel. [-1] (714) 250-1000

DATE>      21 NOV 86  16:21                       Telex 9105951546 MICD NPBH

TO>        D.Coffey(TXS.D/COFFEY)
           C.Baltrunas(TXS.C/BALTRUNAS)

COPIES>    K.Kamoi(KTL.TOKYO)
           M.Ogura(KTL.TOKYO)
           H.Itoh(KTL.TOKYO)
           S.Deguchi(KTL.TOKYO)

FROM>      G.Marus(ISIPR.G/MARUS)

SUBJECT>   C57 PROBLEM, REF A78374

-----------------------------------------------------------------------

Carl, Dennis,
Below I'm reproducing memo A78374 from Makoto Ogura of KTL. As he
mentions, KTL was not able to reproduce the problem reported by
their customer, but they did find something strange on C57. If you
need more information on this from KTL, could you contact Ogura-san
directly at KTL.TOKYO?
 
Ogura-san, when you have more information on this problem next week,
will you let Carl and Dennis of X support know about it.
 
I'll ask both sides to keep me in the Ontyme loop; let me know if there's
anything else I can do to help with this.
 
Thanks and regards, Greg
|
|
|
V


     
     
        Network Information Service          OnTyme  KTL.TOKYO   
                                             Phone   [-81] (3) 262-8711    
                                             Fax     [-81] (3) 262-8757    
Date      : 86/11/21  21:19 (JST)  
     
Attn      : Greg Marus      (ISIPR.G/MARUS)  
     
Copies    : Hajime Itho     (KTL.TOKYO) 
            Satoru Deguchi  (KTL.TOKYO) 
            Kuni Kamoi      (KTL.TOKYO) 
     
From      : Makoto Ogura    (KTL.TOKYO) 
     
Subject   : C57 Problem (?)   
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
Greg san, 
     
Now, I have strange problem on C57.
This afternoon, A-AIS users on C57 called us and informed that Katakana tools   
did not work well. (UPLKT)KPUNCH said "CAN NOT OPEN FILE:xxxxxxxx" and the 
specified file has no problem. We could not reproduce it at our side, however   
the users still reported the problem to us. Also I found some unmatch between   
WHO and SYSTAT. All which I saw is related to DIRECTORY of the system.
     
Is there anychanges on C57 or C57's directory control ?
     
As we could not reproduce it at our side, we will check more next week.    
So if possible, please check on C57 and let us know it before our checking 
at user terminal.   
     
     
Best regards,  
Makoto
Received: from X62.Tymnet by X930.Tymnet; Fri, 21 Nov 86 19:15:15 PST
Return-path: <DENCOFF@X62.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@X62> 
Date: Fri, 21 Nov 86 17:13:43 PST 
To: ISIPR.G/MARUS@Ontyme.Tymnet 
Cc: KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet, KTL.TOKYO@Ontyme.Tymnet,
	KTL.TOKYO@Ontyme.Tymnet, ISIPR.G/MARUS@Ontyme.Tymnet, Carl 
Subject: Re: C57 PROBLEM, REF A78374 
In-reply-to: J17175@Ontyme.Tymnet of C57 problem, ref A78374

                                  MEMORANDUM

                                                              TYMCOM-X SYSTEMS

DATE:  21-Nov-86  17:14:51

  TO:  Makoto Ogura, CC:  Greg Marus

FROM:  Dennis Coffey  (Tymcom-X Systems)

SUBJ:  Re: C57 PROBLEM, REF A78374 
In-reply-to: J17175@Ontyme.Tymnet of C57 problem, ref A78374

------------------------------------------------------------------------------

Ogura san,

I believe the problem on host C57 has been fixed.  The problem was in the GFD
program.  Under one set of conditions the problem would occur.  When the user
used GFD to get access to another directory, then used the GFD command
(without a username) to return to his logged-in directory, he would not get to
his own directory but would get "lost".  

This problem began occuring recently.  I believe it was fixed about 24 hours
ago.  If the users continue to have trouble, please let us know.

Regards,
Dennis Coffey
 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 22 Nov 86 17:26:54 PST 
To: Craig Fletcher <FletcherC@X930.Tymnet> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: 3652 status. 

Found an interesting bug; Missing index register in "MOVEM T1,UNISTS(U)" caused
location 36 to be wiped out, which caused DDT to get an ill mem ref when it
went to type out the location at the next breakpoint.  Fixed that, then got a
stopcode we have not seen before - SWPNIO = swap done to a page that is not
marked for I/O in progress.  I've been reading over FILIO, adding more comments,
and will have something to try Sunday or Monday.  /Joe
From: Joe Smith <JMS@X930.Tymnet> 
Date: Sat, 22 Nov 86 17:59:36 PST 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: BADSAT on system 26 every Saturday. 

22-Nov-86 17:44/Joe (Carl did not answer 3 beeps)
Contact:  Cupertino Operation (408)446-7705, David Jorgenson, System 26.
Problem:  Same as 9-Nov - System gets BADSAT when TITO all-files gets to
          directories starting with "O".  Sometime later it gets error reading
          RPG.SHR, then dies with something like USRUS1 stopcode.
Workaround: Whenever system gets BADSAT while TITO is doing the all files,
          let system stay up until either TITO gets error reading files or
          users get error reading files from SYS, then force a fast DSKCLN
          and tell TITO to resume starting after last complete directory saved.

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 22 Nov 86 18:42:27 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: BADSAT on system 26. 

Correction: DIdn't answer 1 beep. at 17:44

The previous 2 beebs, were at 6:10 am for system 26, re: BADSAT
  and 6:20 am for system 65, re: bad pack or bad drive causing read errors.

Fred called me back at 10ish this morning, re: more BADSAT problems and
stopcode EUE after the BADSAT occurred.

For system 26, I told Fred to take the system down and run a FAST DSKCLN
and to call me if the problem was not fixed.  Apparently the problem keeps
coming back while trying to do an all-files with TITO.  It had problems
with directories CARL and CNFE this morning.

After the DSKCLN, I told them to continue with the all-files where it left.
Apparently, Joe told them the same for the third call today.  No other
indications were found.

For system 65, I restored SYSTAT and DSKCLN from another system and PSP
indicated a bad drive/pack, so I told them to call TM to look at the drive,
if TM says ok, to try it again, or if it fails, PAKCOP the bad pack and
go from there...  after TM is finished.

PS:  THanks Joe, for catching the 17:44 call.  I was out shopping and
     didn't hear the beep.  Noticed it when I got home... system 26 was
     up running FULL DSKCLN (Dave didn't force FAST) and should be up
     for timesharing by 7:00 pm.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 22 Nov 86 18:55:07 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: I didn; t want you to think I was slouching. 

My beeper only went off 1 time between 3pm and 6:30pm... did you hear 3
beeps, or did your beeper just say 3, including this mornings other calls?

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 23 Nov 86 0:42:19 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline calls - busy weekend 

(...and it's only saturday night!)

22-Nov-86 19:20 /Carl
Contact:  Fremont Operations via answering service, Bonnie, System F58.
Problem:  System crashed 5 times during previous 8 hours plus a few in
          the morning: 7:48 BADSAT, 11:21 THUNG on all drives + BADSAT
          on unit 2, 15:03 FILAUS stopcode, 17:37 SEAR14 stopcode, 17:44
          FILAUS stopcode.   ... (additional crashes, but times not
          recorded by me, operations has the times)...  more SEAR14 and
          FILAUS stopcodes.

          All files save in progress, incremental backup save in progress
          and the last crash, both incremental and all-files in progress.

          Last files to tape foreach were: (NETSTAT)61119B.DMI, 61105A.DMS
          and 61114C.DMS.  These files were ok.  Files recorded in the
          NETSTAT directory just before and after each of these appeared
          to be ok except 61113B.DMS which caused the system to crash with
          FILAUS during my examination.  I renamed it to (CRASH) and set
          NDL and NFS bits.  TThe NETSTAT directory has 470,000+ pages in
          500+ files, many of the DMS and DMI files are 5000 to 10000 pages
          each.  (CKSUM takes a long time!!!)   I mention this, because we
          are badly in need of a utility program which reads the ribs of
          all files, or all files in a directory, looking for bad pointers.
          DSKCLN is supposed to do this, and fix them (or mark them bad)
          and apparently it doesn't!

Plan:     I've told operations to restore the file and continue with both
          the incremental backup and the all files, and to call me if they
          crash with FILAUS.

          I told Bonnie to have operations call us every time they get the
          FILAUS stopcode, so that we can locate the bad file immediately.
          If they get a BADSAT error, or BADSAT crash, that they should
          (on an error, not a crash) continue running until TITO complains
          about reading files (or until they cannot access system files)
          and then to recycle and force a fast DSKCLN.  I told her that we
          need NOT be called for each BADSAT... Sending mail is enough until
          further notice.  We are aware of the problem and are working on a
          fix.

(it's been a long weekend) sigh.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 23 Nov 86 23:20:38 PST 
To: Monitor Group <CARL@X930>, Osman Guven <osman@X930.Tymnet>, Joe Smith
	<jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: Hotline calls 

** Turnover to Osman - in case you get calls **

Mostly system 26 and 58.  Same problems BADSAT (26 & 58) and FILAUS (58).
For the bad sat, operations has pretty much been told to proceed until they
have problems accessing system files such as RPG, LOGINN or LOGOUT.  For
FILAUS, they have been told to call us immediately, so that we may look at
the crash and the corresponding backup or all-files tape (TITO) to see what
file caused the crash.

26 had a few problems with BADSAT during the all-files, and they reloaded to
run a fast DSKCLN (forced over FULL) to clear the problem.

System 58 had me on the phone off and on all day with FILAUS and BADSAT trying
to do the daily backup and the all-files.  Username NETSTAT which has over
450,000 pages in 500+ files, many of which are 5k-10k pages in size, had many
bad files which caused FILAUS.  Some were fixed by the DSKCLN which ran, others
have been renamed to (CRASH) with NDL and NFS set.

As of this message, the all-files finished and they were working on the daily
backup, in directory NETSTAT.  I hope yopu don't have any calls...  /Carl

Goodnight.
b =Ó