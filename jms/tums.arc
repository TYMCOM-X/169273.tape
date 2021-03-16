From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 10 Dec 85 12:18:12 PST 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: TUMS installation 
In-reply-to: your message of Tue, 10 Dec 85 4:38:17 PST

System 95 is the 2020 on Liberty Street, it is located within 10 feet of
930.  System 24 is the KI for the 3650 disk project.  Everyone who is valid
on 930 is valid on 95 and 24, but only I use 95 and only Osman, Carl, and I
use 24.  System 35 is what the three of us use to leave messages if 930 is
down.

/JMS
From: Osman Guven <osman@C930.Tymnet> 
Date: Tue, 5 Nov 85 14:13:49 PST 
To: Carl A Baltrunas <carl@C930.Tymnet>, jms 
Subject: Requesting CH7 Interrupt while proccessing stopcd. 

Joe, Carl..
Put this patch on all of the /P monitor system.

"/PATCH #3: Skip CONO PI,REQCLK at KEYSET+6 while in STOPCD processor/
PICON:
PAT/SETOM TIMEF
PAT+1/SETOM CLKFLG
PAT+2/JRST KEYSET+5
KEYSET+4/JRST PAT
KEYSET+5/SKIPN .CPCPC
PAT+3/PAT:
PATSIZ/PAT
COMMON:
PATMAP[Q+040000000000
CONFIG+1T/
CONFIG+2T/

Osman..
P.S.
Carl..
Your 444 is still broke, FSC guy will be in late in the afternoon
to fix it.















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
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 25 Nov 85 23:40:12 PST 
To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: 24's OPER password file 

I have *NOT* moved it over from 930 as of this moment.  I was in the
process of moving the file just now and the host became un-available
through the net.  (Probably, base crashed?)  If either of you wish to
move the file, feel free... just send mail.  The file is:
  (SYS)DRWSPR.EPO

System 24 now has the OPER password file from system 930.  Anyone who
is valid on system 930 is now valid on 24.  Anyone who knows of any OPER
names that need to be present on OUR development system should let me
 BEFORE they add the names.

Also, OSMAN & JOE, please put any names we "allow" on our system other
than those for BLDG-C Operations under TXS.WATCH    -Thanks
/Carl
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
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 29 Nov 85 20:26:19 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Where did the old files in M33 come from? 

There are 4000 pages in files in (M33) that were created before 1-Jan-85.
Where did they come from?  They are using up all the free space on 930.
I thought we renamed those files to (OSARC).

/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 4 Dec 85 23:17:32 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, WRS 
Subject: TOPS-10 KL paging. 

If you're curious as to how TOPS-10 7.02 sets up the EPT to use KL-paging,
the information is in the INFO tree under KL10EPT.		/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 5 Dec 85 9:01:12 PST 
To: osman@c24 
Subject: 24XP09 copied to 26 and 34. 

I sent it to 34 because that way you don't have to even dismount the tape.
From: CTMKTG7@X930.Tymnet 
Date: Fri, 6 Dec 85 11:02:15 PST 
To: FLETCHERC, CARL, JMS 
Subject: URGENT MSG ON ONTYME 

COULD YOU PLEASE CLEAR YOUR MAIL ON YOUR TXS ONTYME
ACCOUNTS ? THX, PETER STRATMAN..
From: Osman Guven <osman@C930.Tymnet> 
Date: Mon, 9 Dec 85 8:52:14 PST 
To: Joe Smith <jms@C930.Tymnet>, Carl A Baltrunas <carl@C930.Tymnet>, dencoff,
	fletcherc 
Subject: 3650 Project.. 

Joe, I am in BLDG C today and I read you mail about formatting.
To see if everything works the way we are predicting, i will
go ahead and see if i can build a PAGE MONITOR/NATIVE MODE test system.
Osman..
Received: from C39.Tymnet by X930.Tymnet; Tue, 10 Dec 85 4:44:37 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 10 Dec 85 4:38:17 PST 
To: jms@930 
Subject: TUMS installation 

35, 24, 95 ???
If you want to put TUMS up on some particular system, I generally don't mind,
but please talk to me first.  There are still a few kludges that are very
fragile and could easily bring the whole thing to its knees.  What are 24
and 95, anyway, I never see them up to log in and find out.  Oh well.  -Bill
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
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 6 Dec 85 7:48:48 UT
From: ISGHQ.POLICY@Ontyme.Tymnet 
Date: 05 DEC 85 21:08:56 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J84424@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:25:39 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-184
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 05 Dec 85



To:       ISG Lists A - E

Subject:  INFORMATION SYSTEMS GROUP PROCUREMENT OFFICE

Ref:      (a) D.  J.  Garrity  Administrative Bulletin ISG-85-180
              dated 26 November 1985



The  above referenced Administrative Bulletin established the ISG
Procurement   Office   with   responsibilities   for   ISG   wide
procurement  policy  and  standards  and for directing group wide
purchasing,  inventory  issues, equipment leasing and procurement
planning.      This   Administrative   Bulletin   delineates  the
assignments and responsibilities within ISG Procurement Office.

W.  E.  (Bill)  Herbst  is  appointed  Manager-Group Negotiations
reporting  to  the  writer.    Bill is responsible for Group wide
Purchasing  policy  and  standard  statements  and  for obtaining
economies  of scale through Group wide consolidation, negotiation
and  administration  of  agreements.    Additionally,  Bill  will
provide  negotiation  assistance  to any requesting Business Unit
or  function.  C. E. (Clyde) Miller, Senior Buyer, will report to
Bill as Principal Negotiator.

T.  E.  (Tom) Steed is appointed Senior Section Manager Equipment
Monitoring  and  Planning  reporting  to  the  writer.    Tom  is
responsible   for   Group  wide  Inventory  policy  and  standard
statements  and  for establishing common equipment communications
and   transfers   between   businesses  and  functions.    K.  H.
(Kathleen)  Fischer,  Analyst, will report to and assist Tom with
the above responsibilities.

T.   M.   (Tom)   Anderson   is  appointed  Principal  Specialist
Administration  reporting  to the writer.  Tom is responsible for
Group  wide  third  party  equipment  leasing policy and standard
statements    and    for   obtaining   third   party   financing,
administration   of  financial  leases  and  originating  product
resale financial offerings.



-2-
ISG-85-184
05 Dec 85



These  changes  are  effective immediately, please give Bill, Tom
and Tom your full support in their new responsibilities.


(Original signed by D. J. Boland)

D. J. Boland
Manager Procurement-ISG


CONCURRENCE:


(Original signed by D. J. Garrity)

D. J. Garrity
Staff Vice President Administration-ISG
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 6 Dec 85 7:49:05 UT
From: TYMOPS.J/SNAVELY@Ontyme.Tymnet 
Date: 06 DEC 85 00:06:55 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M42915@Ontyme.Tymnet 
Subject: TYMNET I PROJECT SCHEDULE 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:25:44 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

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
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Sat, 7 Dec 85 11:44:57 UT
From: CT.DPM@Ontyme.Tymnet 
Date: 06 DEC 85 18:19:09 
To: TXS.SUP@Ontyme.Tymnet 
Cc: TXS.C/FLETCHER@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.J/SMITH@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet, CT.DPM@Ontyme.Tymnet,
	TXS.SUP@Ontyme.Tymnet 
Message-id: A77745@Ontyme.Tymnet 
Subject: "E M O R A N D U M------------------------------"... 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:25:49 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

 
 
 
 
 
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
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 9 Dec 85 19:36:11 UT
From: ISGHQ.POLICY@Ontyme.Tymnet 
Date: 09 DEC 85 14:28:16 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J85272@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:25:55 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-187
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 06 Dec 85


To:       ISG Management Levels A - E

Subject:  ISG INFORMATION SERVICES ORGANIZATION


The  formation  of  the ISG Information Services Organization was
announced   in  ISG  Administrative  Bulletin  ISG-85-181,  dated
26   November   1985.      Group   Information   Services  office
responsibilities  were  identified  in  that  AB.   The following
assignments   are   announced   in   order  to  carry  out  those
responsibilities.

Policy  Management  -  The  writer  will  act  as Manager, Policy
Management  until further notice.  Responsibilities of this Group
Office    function   includes   Policy   Statements,   Management
Directives,  Organization Charts, Administrative Bulletins, Group
Level   Procedures,   Authority  Delegation,  Policy  Audit,  MDC
Corporate  interface  and related studies.  Members of the Policy
Management staff will report directly to me.

Information  Standards  -  H.  L.  (Harvey)  Leemon  is appointed
Manager,   Information  Standards  reporting  to  me.    Harvey's
responsibilities  consist  of Internal ISG Management Information
Systems  (MIS)  and  Office  Systems  Standards, APS, IRM and USC
interface dination.    Assessment of O/A and MIS technology,
Executive Systems support and related studies are also included.

Information  Architecture and Planning - D. C. (Don) Mengersen is
appointed   Manager,   Information   Architecture   and  Planning
reporting   to  me.    Don's  responsibilities  involve  Internal
Information  Systems  planning,  Information Architecture design,
evaluation  of  Productivity Tools, Information Quality Analyses,
Vendor/Product reviews and recommendations, and related studies.

These   assignments  are  effective  immediately.    Please  give
Harvey,  Don,  and I your full support in accomplishing these new
responsibilities.

(Original signed by R. D. Greco)

R. D. Greco, Director
Information Services

Concurrence:

(Original signed by D. J. Garrity)

D. J. Garrity, Staff Vice President
Administration-ISG
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 10 Dec 85 23:48:56 UT
From: DSD.SUP@Ontyme.Tymnet 
Date: 10 DEC 85 18:52:34 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A80065@Ontyme.Tymnet 
Subject: "TO: ALL TYMNET, TYMSHARE DIVERSIFIED"... 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:26:01 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

 
TO:        ALL  TYMNET,  TYMSHARE DIVERSIFIED BUSINESS UNIT SALES 
           REPRESENTATIVES AND ADMINISTRATIVE PERSONNEL
 
 
FROM:      VIRGINIA NYE/OPERATIONS MANAGER-DISTRIBUTED SYSTEMS CO
 
 
=================================================================
 
 
EFFECTIVE  JANUARY  1,  1986,  DISTRIBUTED SYSTEMS  COMPANY  WILL 
OFFICIALLY BE OUT OF THE EQUIPMENT LEASING BUSINESS.  IN THE PAST 
OUR DIVISION HAS HANDLED THE LEASING OF TERMINALS,  PRINTERS  AND 
MODEMS    TO   "TYMSHARE"   CUSTOMERS.     THIS   INCLUDED;   THE 
ADMINISTRATION, BILLING OF NEW LEASES, RENEWALS AND CANCELLATIONS 
AND PURCHASE CONVERSIONS.  
 
 
AS OF JANUARY 1, ALL OF THE ASSETS WILL TRANSFER OUT OF DSC FIXED 
ASSETS INTO THE COST CENTER OF THE BUSINESS  UNIT   RECEIVING THE 
MONTHLY  REVENUE  FROM THE CUSTOMER LEASING THE  EQUIPMENT.   THE 
BUSINESS UNIT NOW WILL BECOME RESPONSIBLE FOR THE  ASSETS.   THIS 
RESPONSIBILITY  INVOLVES  TAKING BACK THE EQUIPMENT,  SHOULD  THE 
CUSTOMER CANCEL.  DSC WILL NO LONGER:
 
 
                 o  ARRANGE FOR THE PICK UP
 
                 o  ARRANGE FOR THE BILLING
 
                 o  ARRANGE FOR THE BILLING TO BE DISCONTINUED
 
                 o  RECEIVE THE EQUIPMENT INTO OUR WAREHOUSE
 
 
ANY EQUIPMENT THAT CANCELLED PRIOR TO DECEMBER 1, WILL BE  PICKED 
UP AND RETURNED  TO  OUR  WAREHOUSE.   ANY  CANCELLATION  LETTERS 
RECEIVED AFTER DECEMBER 2, WILL BE SENT TO THE CUSTOMERS'BUSINESS 
UNIT.
 
 
THERE IS HELP AVAILABLE FOR THE BUSINESS UNITS THAT LACK  STORAGE 
AREA.   IF  YOU  DO  NOT WANT OR NEED  YOUR  CUSTOMER'S  RETURNED 
EQUIPMENT YOU MAY CONTACT BOBBIE HINES AT  714/952-6555.   BOBBIE 
IS MANAGER OF  THE EQUIPMENT DISPOSITION AND SALES DEPARTMENT FOR 
ISG. SHE CAN HELP YOU DISPOSE OF YOUR ASSET AT FAIR MARKET VALUE.
 
 
NORTHERN  AIR  FREIGHT  HAS  HANDLED THE  PICK  UP  OF  CANCELLED 
EQUIPMENT FOR  SEVERAL YEARS.  THEY WILL GO TO THE CUSTOMER SITE; 
PACK  AND BOX THE EQUIPMENT AND SHIP TO YOUR DESIGNATED LOCATION.
THEIR CORPORATE OFFICE IN SEATTLE, WASHINGTON (800/426-5962) WILL 
HANDLE  ALL THE ARRANGEMENTS.

 
CURRENTLY,  MOST OF THE EQUIPMENT OUT ON LEASE HAS BEEN THERE FOR 
MORE  THAN  2 YEARS AND IS FULLY DEPRECIATED OR  HAS A  VERY  LOW 
BOOK  VALUE.   IT IS OUR SUGGESTION THAT YOU ATTEMPT TO GET  YOUR 
CUSTOMER TO PURCHASE THIS LEASED EQUIPMENT AT A LOW  COST.   THIS 
PURCHASE  WOULD  RESULT  IN  REVENUE FOR  YOUR  OFFICE  AND  YOUR 
CUSTOMER  COULD  SIGN  UP FOR A MAINTENANCE  AGREEMENT  WITH  THE 
MCDONNELL DOUGLAS FIELD SERVICE COMPANY.  THIS WOULD PREVENT YOUR 
OFFICE FROM EVER HAVING TO DEAL WITH THE RETURN.
 
 
IF YOU HAVE ANY QUESTIONS REGARDING THIS NEW POLICY YOU CAN  CALL 
YOUR COMPANY CONTROLLER OR ONTYME DAN SAENZ DSD.D/SAENZ.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 10 Dec 85 23:49:39 UT
From: NETS.TERRIO@Ontyme.Tymnet 
Date: 10 DEC 85 19:20:10 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: I88229@Ontyme.Tymnet 
Subject: Move to Another Building 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:26:06 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

                              M E M O R A N D U M

                                                     [] Network Systems
                                                        Marketing
DATE>      10 DEC 85  11:14

TO>        To the World

COPIES>    Jim Mullen

FROM>      Terrie Osteen


SUBJECT>   Move to Another Building


-----------------------------------------------------------------------


On December 11, 1985, the Network Systems Marketing Group
will have moved to their new facilities at:

                         2450 N. First St.
                         San Jose, CA   95131
  Operator/Receptionist  (408) 435-0200


Names and phone numbers are as follows:
--------------------------------------


Jim Mullen         (408) 435-7777
Terrie Osteen               -7739
Dennis Purpura              -7742
Liz Foss                    -7740
Rob Albertson               -7785
Bob Barbour                 -7741
Rise Ciufia                 -7745
Sue Carroll                 -7743
Tom Chard                   -7744

Betty Jo Chang              -7778
Mark Farlin                 -7722
Rourke McCusker             -7756
Jerry Messina               -7757
Gary Stoy                   -7726

                                                                Page  2

Helen Wenzel                -7747
Lois Mowrey                 -7748
Francine Blas               -7751
Michelle Wooden             -7752
Stephanie Carroll           -7749
Kim Clemens                 -7750
Virginia (temporary)        -7721
Secretary (temporary)       -7784

Rick Forberg                -7779
Gerry Louvierre             -7753
Hal Kroeger          
Nathan Gregory
Anver Meghji         

Lloyd Nirenberg's group will remain at the Orchard Parkway facility.

To reach any of the above individuals from Orchard Parkway, dial
84+ext.

From Valley Green, dial 117+84+ext.
Received: from C39.Tymnet by X930.Tymnet; Wed, 11 Dec 85 11:35:16 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 11 Dec 85 11:19:08 PST 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: TUMS installation 
In-reply-to: your message of Tue, 10 Dec 85 12:18:12 PST

Okay, sounds like you guys are busy.  Is everything working okay on those
new TUMS systems?  -Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 11 Dec 85 13:16:05 PST 
To: IPC.SRAMAIL@ONTYME 
Subject: Utilization of Data Center systems. 

I have been trying to keep a list of active PDP-10, 940, and 370 systems.
The information I have is out of date.  Could you provide me with a new list?

TXS.J/SMITH

============================================================================

SYS10;TYMCOM-X systems.         As of 10-Dec-85

Sysno  CPU  #    OS  Site ID   Utilization
-----  -------  ---  -------   -----------
C22    KI  742  TYM  CUPC      PRIVATE NETWORK (DOMESTIC & INTERNATIONAL)
D23    KL 1275  TYM  DALLAS    U.S. POSTAL SERVICE / JAPAN
C24    KI  532  TYM  CUPC      3350 development and BASE development
D25    KL 1460  TYM  DALLAS    TYMNET / CODE GENERATION BACKUP
C26    KL 1307  TYM  CUPC      COEES (DEDICATED)
D27    KI  761  TYM  DALLAS    INSD CUSTOMERS/U.S.POSTAL SERVICE/EUROPE
F28    KL 1388  TYM  FREMONT   TYMNET / ONTYME ACCOUNTING
C29    KL 1286  TYM  CUPC      COEES
F30    KL 1427  TYM  FREMONT   TYMNET ACCOUNTING
D31    KI  666  TYM  DALLAS    INSD CUSTOMERS / U.S. POSTAL SERVICE
D32    KL       TYM  HOUSTON       not in service  (had a plotter)
C33    KL 1081  TYM  CUPC      TYMSHARE - INTERNAL
C34    KI  641  TYM  CUPC      TYMSHARE - INTERNAL (MASTER OPER PASSWORD)
C35    KL 1386  TYM  CUPC      U.S. POSTAL SERVICE / TYMQUOTE
C36    KL 1415  TYM  CUPC      COEES
D37    KL 1461  TYM  DALLAS    U.S. POSTAL SERVICE / PRIVATE NET (BACKUP)
F38    KL 1405  TYM  FREMONT   TYMSHARE ACCOUNTING
C39    KL 1354  TYM  CUPC      NETWORK DEVELOPMENT / PRIVATE NET
D54    KL 1376  TYM  DALLAS    INSD/U.S. P.S./TTMS CUSTOMERS/ORDER BACKUP
C55    KL 1336  TYM  CUPC      GENERAL TIMESHARING / RAILTRACK
D56    KL 1383  TYM  DALLAS    TYMNET ORDER ENTRY / INTERNATIONAL ACCTG
C57    KI  542  TYM  CUPC      NETWORK DEVELOPMENT / TYMNET
F58    KL 1332  TYM  FREMONT   TYMSHARE ACCOUNTING
S59    KS 4097  TYM  ST-CLOUD  CEGI
S60    KS 4097  TYM  ST-CLOUD  CEGI
D65    KL 1380  TYM  DALLAS    OTIS ELEVATOR (DEDICATED)
C70    KI  733  TYM  CUPC      TYMNET CODE GENERATION
D72    KI  657  TYM  CUPC      TYMSHARE ACCOUNTING
F74    KL 1421  TYM  FREMONT   TYMSHARE ACCOUNTING/VALIDATION DB/TYMUSE
D79    KI  565  TYM  DALLAS    TYMSHARE ACCOUNTING
S83    KS 4097  TYM  ST-CLOUD  CEGI
L88    KI       TYM  LOCKHEED      not in service       (had a card reader)
P90    KL 1305  TYM  ST-CLOUD  CEGI, (FORMERLY SLIGOS IN PUTEAUX)
S92    KS 4097  TYM  ST-CLOUD  CEGI
X95    KS 4267  TYM  LIBERTY   TYMCOM-XX DEVELOPMENT
C107   KL       TYM  CUP-C         not in service       (test monitor)
C108   KL       TYM  CUP-C         not in service       (other test monitor)
C118   KS       T20  VG-2          not in service (alias C67-B on BUBBNET)
F145   KS 4097  T20  FREMONT       not in service
M169   KS 4097  TYM  ST. LOUIS MALLINCKRODT CHEMICAL CO
S170   KS 4097  TYM  ST-CLOUD  CEGI
S184   KS 4097  TYM  ST-CLOUD  CEGI
S264   KS 4097  TYM  ST-CLOUD  CEGI
S301   KS 4097  TYM  ST-CLOUD  CEGI
H370   KS 4097  TYM  HOUSTON   FAYEZ SAROFIM
S443   KS 4097  TYM  ST-CLOUD  CEGI
X675   KS                          not in service
C897   F3    5  TYM  CUP-A     NTD DEVELOPMENT
X930   F3    6  TYM  LIBERTY   TYMCOM-X DEVELOPMENT
W1051  KS 4097  TYM  SEATTLE   RAINIER NATIONAL BANK
P1292  KS            PARIS         not in service

  BUBBNET (use gateway BUBB to get from TYMNET to BUBBNET)
Sysno  CPU  #    OS  Site ID   Utilization
-----  -------  ---  -------   -----------
VG54-B KS       TYM  BUBBNET
C67-B  KS       T20  BUBBNET       not in service (alias C118 on TYMNET)
C84-B  VAX      VMX  BUBBNET
F94    KL 1332  T20  BUBBNET       hardware now part of F58

  TRWNET (use gateway 633 to get from TYMNET to TRW-NET)
Sysno  CPU  #    OS  Site ID   Utilization
-----  -------  ---  -------   -----------
TRW33  KS 4097  TYM  ANAHEIM   TRW  (GATEWAY 633)

======================================================================

SYS20;TYMCOM-20 systems (AUGMENT).  (as of early 1985 - needs to be updated)

Host#  Name     Operating System
----  -------   -----------------------
1088  OFFICE1   TENEX <CARL> valid here
1643  OFFICE2   TENEX
      OFFICE3   redirected to OFFICE8
1087  OFFICE4   TYMCOM-20 2(4), EXEC 5(713)-2, 26KL
 185  OFFICE5   TENEX
 896  OFFICE6   TENEX
      OFFICE7   redirected to OFFICE4
 232  OFFICE8   TENEX
 818 (office9)  not available thru network
      OFFICE10  redirected to OFFICE5
 575 (office11) shut
 212 (office12) not available thru network
2300 (office13) host down
1311 (office14) TENEX
 895  OFFICE15  TENEX (does not have same network password)
     (office16) ?
     (office17) ?
 118 (office18) not available through net (was a 2020)
     (office19) ?
 148 (office20) shut

======================================================================

TYMCOM-IX systems.  Memo from RESOURCE PLANNING & MANAGEMENT as of 5-JULY-1983

C1    unrestricted (internal)
C2    unrestricted (internal)
C3    TYMSHARE ACCOUNTING SYSTEM
C4    dedicated to M.D.S.I.
C5    dedicated to TRANSAMERICA
H6    restricted 1000-1600 PST
H8    unrestricted (internal - IMS system)
C9    unrestricted (internal)
H11   dedicated to M.D.S.I.
H12   restricted 0800-1500 PST
C15   dedicated to M.D.S.I.
C16   restricted 0700-1600 PST
H17   restricted 0500-1400 PST
C18   restricted 0600-1400 PST
C20   dedicated to M.D.S.I.

======================================================================

TYMCOM-370 systems.  Memo from RESOURCE PLANNING & MANAGEMENT as of 5-JULY-1983

F40   3033   TSD DEVELOPMENT, OS DEVEL, MIS MVS DEVEL, GENERAL TYMSHARING
D41   4341   GENERAL TIMESHARING, EXXON, SOUTHWEST BELL, EDI
V42    158   GENERAL TIMESHARING, TTMS INTERNAL
V43    158   GENERAL TIMESHARING, U.S. POSTAL SERVICES
V44   4341   GENERAL TIMESHARING, WESTERN ELECTRIC, MDS DEVELOPMENT
D45    158   TYMSHARE - ACCOUNTS RECEIVABLE
D46   4341   DYNATAX DEVELOPMENT
D47   3033   ARMY CORP OF ENGINEERS, DEDICATED GOVERNMENT USE
V48   3033   GENERAL TIMESHARING, ELMER FOX, MDS CUSTOMERS, WESTERN ELECTRIC
D49   3033   GENERAL TIMESHARING, BEATRICE FOODS, SOUTHWEST BELL
D52          TAX CENTERS
D82          TAX CENTERS
D85    158   GENERAL TIMESHARING, EUROPE
F142  4341   ?

======================================================================


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
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 13 Dec 85 1:47:57 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: 24xp10 

thanks for the clarification....
  so all that is needed in the monitor NOW is the REFSTR stuff
to assign physical tracks to the monitor.  Right?
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 13 Dec 85 14:00:50 UT
From: FSC.JM/SMITH@Ontyme.Tymnet 
Date: 12 DEC 85 23:46:33 
To: TXS.J/SMITH@Ontyme.Tymnet 
Message-id: P09907@Ontyme.Tymnet 
Subject: TEST 

From: Sally Smith <SALLY@X930> (38725 Lexington St #247, Fremont CA 94536) 
Date: Thu, 12 Dec 85 23:44:16 PST 
To: TXS.J/SMITH@ONTYME 
Subject: TEST 

FROM SALLY TO TXS.J/SMITH@ONTYME
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
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 18 Dec 85 22:16:35 UT
From: TYMOPS.J/SNAVELY@Ontyme.Tymnet 
Date: 18 DEC 85 21:02:17 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M46065@Ontyme.Tymnet 
Subject: TYMNET I ELIMINATION 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Thu, 19 Dec 85 2:59:51 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

ATTN:   ACCOUNT SUPERVISORS

        PLEASE FORWARD THE FOLLOWING MESSAGE TO ALL ACCTUSERS.
        THNAK YOU FOR YOUR COOPERATION.

TO:     DISTRIBUTION

SUBJ:   TYMNET I ELIMINATION

ELIMINATION IS TAKING PLACE TONIGHT.  PLEASE ENSURE ALL HSA SLOTS ARE
RUNNING VERSION 1.05.  THIS IS THE LATEST RELEASE AND HAS FIXES FOR ALL
KNOWN PROBLEMS.

THIS CONVERSION CANNOT BE POSTPONED; SO BE READY.

JACK
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
From: Osman Guven <osman@C930.Tymnet> 
Date: Mon, 30 Dec 85 9:48:10 PST 
To: jms 
Subject: Back.. 

How was the Planet Mars and the Marchian Vacation..??
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 24 Dec 85 2:07:36 UT
From: NSS.J/GERLACH@Ontyme.Tymnet 
Date: 23 DEC 85 23:08:36 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M47100@Ontyme.Tymnet 
Subject: TRAINING BROCHURE 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 31 Dec 85 0:05:23 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

                            M E M O R A N D U M

                                                        [] TYMNET - NETWORK
                                                           SYSTEMS SUPPORT
DATE>      23 DEC 85  15:04

TO>        ALL SUPERVISORS

COPIES>

FROM>      BOB GRANDEY


SUBJECT>   TRAINING BROCHURE


-----------------------------------------------------------------------



THE 1986 TRAINING BROCHURE IS NOW AVAILABLE FROM TYMNET PUBLICATIONS
DOCUMENT E-007.  PLEASE ORDER A SUPPLY FOR YOUR CUSTOMERS.

NOTE THE NEW AND REVISED COURSES AND THE CHANGES IN THE REGISTRATION,
CANCELLATION PROCEDURES.

TTE WILL DO EVERYTHING POSSIBLE TO PROVIDE YOUR CUSTOMERS WITH THE
QUALITY TRAINING THEY NEED, INCLUDING CUSTOMIZED COURSES.

PLEASE CONTACT ME WITH QUESTIONS.

BOB GRANDEY,ONTYME NSS.R/GRANDEY, PHONE-435-0239 EXT.503


HAPPY HOLIDAYS,

BOB GRANDEY
Received: from C39.Tymnet by X930.Tymnet; Thu, 2 Jan 86 23:09:09 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 2 Jan 86 23:07:50 PST 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: MAIL on 930 
In-reply-to: your message of Thu, 2 Jan 86 18:24:19 PST

Thanks for letting me know.  File distribution is a real pain in the neck.
I've started a fresh copy over.  I'll try to check on it later but if you
notice anything further funny let me know.  Take care -Bill
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
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 13 Jan 86 12:21:50 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, JMS, FLETCHERC 
Subject: BEEPER.. 

MY BEEPER WAS ON BUT DIDN'T GO OFF EITHER..
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 13 Jan 86 15:35:12 PST 
To: Brett Fairchild <IPC.B/FAIRCHILD@ONTYME.tymnet> 
Subject: Formatting more than one 3330 disk at a time. 

When you run the SA10 program to format a disk, setting UWP to -1 enables
formatting the drive on unit 0 only.  If you want to format 3 disks at the
same time, using units 3,4, and 7, then you can enter:

	UWP+3/  0   -1
	UWP+4/  0   -1
	UWP+7/  0   -1

Up to 16 disks can be formatted at the same time using this method.

/JMS
From: FLETCHERC@X930.Tymnet 
Date: Wed, 15 Jan 86 9:58:52 PST 
To: carl, jms, osman, bring 
Subject: F3 

Talked to Carson about one of the returning F3's and he said we could
have the use of one.  It will still be his in reserve in case of dire
necessity so we might have to give it up until such time as he has
no more need.  Odds are, however, that we would have the use of it
and Carson would never take it back.  Taylor will wheel it into
our "new" computer room.

	Craig
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 15 Jan 86 10:41:48 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, bring 
Subject: F3 system from scratch! 

It might be a good idea to keep a 1-pack system on this 2nd F3 to use
as our "test-bed" system from scratch.  Later, after we've accomplished
this feat, the single drive can be "added" using ADDPACK in ONCDSK to
bring the system upto it's full complement of disks.

/Carl
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
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 21 Jan 86 14:01:49 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: BIONIC on SYS 

Terry Silva (ext 6368) wants to know when the new BIONIC will be put on
the SYS area, (SYS:55) in particular.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 21 Jan 86 13:37:42 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Cc: Osman Guven <osman@X930.Tymnet> 
Subject: KLDCP talks to TOPS-10/TYMCOM-X via APT10. 

Carl, could you verify that the commands in (KL10:930)APT10.HLP are listed in
the small blue KL notebook?  APT10.SAI was written by Bill Soley 8 years ago
when he first came to TYMSHARE.  It is supposed to allow us to copy a new
BOTLOD onto the KLDCP DECtapes.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 21 Jan 86 14:58:29 PST 
To: dencoff 
Subject: Node numbers of old PDP-10 bases. 

If the following nodes should NOT be still missing:
  Node:    23  D65-base old             As of 31-Dec-85
  Node:    24  D27-base old
  Node:    41  D72-base old
  Node:    32  C57-base old
  Node:   117  D79-base old
  Node:   155  C22-base old
  Node:   563  F30-base old
  Node:   761  F28-base old
  Node:  2100  C34-base
  Node:  2107  C70-base
  Node:  2234  S83-base
  Node:  2235  S184-base
  Node:  2332  D79-base
  Node:  2333  F28-base
  Node:  2334  C35-base
  Node:  2374  C22-base
  Node:  2712  S301-base
  Node:  2770  TXS-2770
  Node:  3000  S90-base
  Node:  3024  S59-base
  Node:  3025  S92-base
  Node:  3115  D37-base
  Node:  3321  S443-base
  Node:  3427  S60-base
  Node:  3475  S264-base
  Node:  3761  S170-base
  Node:  4177  C55-base
  Node:  4200  C29-base
  Node:  4272  H370-base
  Node:  4274  D54-base
  Node:  4525  C57-base
  Node:  4725  D56-base
  Node:  5473  C24-base
  Node:  5541  C897-base
  Node:  5650  D65-base
  Node:  6415  C33-base
  Node:  6657  X930-Base
  Node:  6712  C26-base
  Node:  6714  C36-base
  Node:  6715  D27-base
  Node:  6716  D23-base
  Node:  6717  D72-base
  Node:  6720  F58-base
  Node:  6721  F30-base
  Node:  6722  F74-base
  Node:  6760  X95-Base
  Node:  7004  D31-base
  Node:  7021  F38-base
  Node:  7162  C39-base
  Node:  7165  D25-base

  Node:  2366  MUX-2096
  Node:  2367  MUX-1274
  Node:  2457  CUPC-ops
  Node:  2470  SantaClara
  Node:  2512  CUP-C
  Node:  3143  Liberty-3
  Node:  3146  Liberty-6
  Node:  3220  Fremont
  Node:  3335  CUP-3335
  Node:  4764  St. Louis
  Node:  7344  VG-7344
  Node:  7345  VG-7345
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 22 Jan 86 12:58:06 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: FLEXIT on P034/P. 

Do you have any programs that predictably use FLEXIT?  Soley thinks that is
what caused C39 to crash every time TUMS or MAILER got some sort of an error.
I had to renamed *.SAV to *.SV just so C39 would stay up, Bill has changed the
log code to use HALT in place of FLEXIT for now.	/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 22 Jan 86 15:17:35 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Back on my feet today 

Well,
  I seem to be getting back on my feet.  I feel better today than I have
all week... more or less down to just the runny nose and lack of energy
stage.  I will see how things go and will try to be in tomorrow or Friday.
If I stay home tomorrow, I will probably be working.  If so, i will send
mail so that it won't need to be listed as sick-leave.

Hope to see you all then.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 22 Jan 86 16:48:12 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: (OSU)ONCALL 

I added my name to ONCALL.RES and ONCALL.SCH, now how do we convince it that
the seven consecutive days always start on Monday?  /JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 23 Jan 86 1:43:48 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: ONCALL seems to have a bug 

I've looked at the code, found something fishy, but since it's some
of my more cryptic (un-commented) code, I'm not sure if it'd break
worse than it is now.  I have a couple of ideas, but I'm too woosy
to pursue them at the moment.  I'll send mail when it's fixed.  the
problem is in the part that changes the schedule using the data in
ONCALL.SCH, (sigh)!
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 23 Jan 86 14:06:53 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Your phone 

Your phone has been set to forward to 2416.
Steve Schramm (ext 7012) asked if you know of a "pretty printer" for PASCAL.
Lynn Laurence (ext 5024) has a question about a TYMCOM.INIT file.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 23 Jan 86 13:56:40 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Working at home today. 

I'm feeling better, but want to take one more day without going in & out
of the weather (although it looks nice outside) to insure that I'm really
over this FLU bug.  So, I shall be available at home.  408-945-4314 and
via mail.

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 23 Jan 86 18:33:42 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Obscure INFO command 
In-reply-to: your message of Thu, 23 Jan 86 17:13:23 PST

Please fix the CONTENTS command so that it does not output two consecutive
formfeeds (which cause the LA50 to munch the paper).  Better yet, don't output
any formfeeds at all - INFO wastes far too much paper.

Teach INFO to not use image-mode output when not using escape sequences.
Right now, the monitor settings of TTY NO FORM and TTY NO TAB don't work when
INFO goes to TTY or the default.

Teach INFO to go into terminal input wait instead of sleeping so that the
following can be put in a PCOM CTL file:
	TTY CLASS TTY
	R INFO
	BUILD
	QUIT

/JMS
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
Received: from C36.Tymnet by X930.Tymnet; Fri, 14 Feb 86 12:30:07 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 14 Feb 86 12:15:53 PST 
To: Carl Baltrunas <CARL@X930.Tymnet>, Osman Guven <OSMAN@X930.Tymnet>, Joe
	Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet> 
Subject: Absence from office. 

      I've called Lois so she can inform Craig.  Everyone else in the office
has probably noticed:  I'm here (home) not there.  I seem to have caught
Nancy's flu--no, I'm not cleaning up the yard.

      I'll see you Monday.  I hope to have kicked this by then.
Received: from C36.Tymnet by X930.Tymnet; Mon, 17 Feb 86 9:42:01 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Mon, 17 Feb 86 9:30:54 PST 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: Rainbow meeting Thursday, Feb 20th. 
In-reply-to: your message of Sat, 15 Feb 86 20:06:33 PST

Joe,  thanks for the info re. the DEC PC User Group meeting.  Sounds very 
interesting, but I'm too busy with wedding preparations to consider going.

/Dennis
From: FLETCHERC@X930.Tymnet 
Date: Mon, 17 Feb 86 11:25:13 PST 
To: jms 
Subject: various 


You are confirmed for TYMNET class starting 19February.

Please get your Standby time in by noon on Monday's in the future.

Please see me about some TOPS10 related work which we can do for
Planning Research Corp at Fort Hunter LIggett near King City , ca

	CRaig
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 18 Feb 86 17:07:09 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Software Notebooks. 

I left a message for Caron Geddes saying that we are missing updates 89,
90, 93, 94.  We have up thru 88, 91, 92, and 95.   /JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 18 Feb 86 17:49:12 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: Head/Ear Ache... 

This headache/earache combination seems to be getting worse.  I have an
appointment at Kaiser tonight at 7:00 to see what is causing the problem.
If it doesn't get any better, there is a good chance that I will be at
home tomorrow.  (Possibly working?)

I will keep you all posted on developments.  I hope that all will be
cleared up and I'll be in as usual tomorrow.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Feb 86 9:53:18 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>,
	<FSC.L/FROST@Ontyme.tymnet> 
Subject: Update on my condition 

The pain in the side of my head isn't getting much worse, but it also
isn't getting any better.  Lying down seems to decrease the frequency
and the intensity somewhat, but I don't know how much.  The doctor did
NOT find anything conclusive and told me to wait a day or two and come
back if the pain didn't go away.
  I have a dental appt this afternoon to determine if the pain is from
a cavity in one of my lower teeth... I don't think it will be, but we
will see.
  I will be working from home in the meantime... send mail or if it is
urgent feel free to call me.

/Carl
Received: from C39.Tymnet by X930.Tymnet; Tue, 27 Aug 85 17:01:59 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 27 Aug 85 16:24:49 PDT 
To: NSS.E/RIORDAN@Ontyme.Tymnet 
Cc: jms@930 
Subject: Re: "Bill The resource procurement"... 
In-reply-to: M10291@Ontyme.Tymnet of 27 AUG 85 11:40:47

----------------------------------------------------------------
    From: NSS.E/RIORDAN@Ontyme.Tymnet 
    Date: 27 AUG 85 11:40:47 
    To: NTD.WRS@Ontyme.Tymnet 
    Message-id: M10291@Ontyme.Tymnet 
    
    Bill,
    
    The resource procurement people in St. Louis are looking for someone
    technical to help them determine of 'Readstat" and "ICP' will function
    in the TOPS-10 environment.  (They are gearing up to create a pdp-10
    productin environment in the STL data center).

    Do you know who might be of assistance to them - someone savvy in
    the Dec environemtn?   I figured if it isn' t you - you might at
    least know someone inhouse who is that technically astute.

    Many thanks,
    Eileen
----------------------------------------------------------------

No.  Neither program will function outside of Tymcom-X.  They both take
advantage of an intimate knowledge of internal monitor data structures
which are quite different from current TOPS-10.  This is particularly
true of ICP.  Most of the information displayed by ICP is unique to
Tymcom-X as is the method ICP uses to obtain it.  I am sure there are
utilities similar to READST provided by DEC for use on TOPS-10, and I
believe TOPS-10 SYSTAT and SYSDPY provide features similar to ICP.

I suggest you contact Joe Smith in Fremont since he is familiar with
these utilities and recently came from a TOPS-10 environment.  I sent
him a copy of this reply.

-Bill
Received: from C39.Tymnet by X930.Tymnet; Wed, 28 Aug 85 22:35:55 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 28 Aug 85 22:34:35 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: C-10 ? 
In-reply-to: your message of Wed, 28 Aug 85 20:19:51 PDT

  Do you know of a C compiler that runs on the PDP-10?
  Just curious.  /JMS

There was once one at MIT.  Written in MIDAS (assembler), I think.
On the AI lab system.  Don't know if its still around.  You can try
sending a builten message.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 1 Sep 85 21:27:46 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: re: feature-test gettab 

Yes!!!!  (I had almost forgotten about that)!

In fact, we should put in the fact that we have STOPCDs and a few
other things...  hmmm... more cleanup work for /P01.../Q

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 27 Sep 85 6:09:08 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Hotline call system 57, disk problems 

I just finished poing around trying to fix "serious errors" as reported
by DSKCLN on system 57.  It was dying with PCBIO stopcds, @ CTLFN1+...
when it searched throught the entire PCB list without finding the page
(rib, sat, or upt?) page it was looking for... sounds ominous like either
the disk or the memory was garbaged...  I couldn't find anything that I
could fix.  A few counts were off and a few bad ribs.  I left it with
Tom running a full dskcln to fixup the bad files... I did fix the refresh/
missing/inconsistant packset id problem and fiddled a few files to get
DSKCLN to decide that the errors were "serious to users" but not "serious
to the system"...

I've scheduled lunch with Bill Soley @ 1:30 pm in campbell, so after my
sleepless morning... I may or may not make it to the office this afternoon.

Call me if you need me., or beep me if I'mnot home...  /Carl
From: FLETCHERC@X930.Tymnet 
Date: Mon, 14 Oct 85 9:15:23 PDT 
To: carl, jms 
Subject: work hours 


I would like you both to start coming in by 10:00 a.m. to allow
better overlap with others in the department and so you are
here to answer questions that come up during regular working
hours.

	Craig
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
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 22 Oct 85 17:01:11 UT
From: NTD.W/WANG@Ontyme.Tymnet 
Date: 22 OCT 85 08:56:22 
To: TXS.J/SMITH@Ontyme.Tymnet 
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
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 25 Oct 85 12:36:24 PDT 
To: Mail Wizard <MAIL> 
Subject: TUMS-10 bug report (v36 of 20-AUG-85) 

More on previous message:
	>read 1:99
	?EvalElement: illegal range: 1:99
	?error in message set expression:
		"1:99"
	No messages read.
I goofed, there were only 76 messages in TUMS.MSG, but the wording of the
error message implies that I used illegal syntax.  Please accept these two
suggestions:

1) If the end of the range exceeds the maximum, use the maximum and continue.
2) Change the word "illegal" in error messages to something less severe
   whenever possible.  Consider the poor secretary who bursts into tears
   (thinking she would be arrested) because the computer said she did something
   illegal.

/JMS
Received: from C39.Tymnet by X930.Tymnet; Fri, 25 Oct 85 13:48:29 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 25 Oct 85 13:48:26 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: TUMS-10 bug report (v36 of 20-AUG-85) 
In-reply-to: your message of Fri, 25 Oct 85 12:36:24 PDT

Suggestion well taken as I've always had a warm spot in my heart for poor
secretaries who are prone to being intimidated by great big computers.
Thanks -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 26 Oct 85 21:51:44 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Beeper battery! 

My "extra" beeper battery doesn't seem to want to charge at all.  I've
had it charging in the spare slot on my charger and when I went to use
it on Friday, it was dead as a doornail!  I've tried charging it a little
inside the beeper while turned on, and again it doesn't seem to work.  I
haven't tried it with the beeper OFF, because I wanted to make sure Osman
or Joe was available while I had the beeper OFF.

I may want to try another charger or another battery... anyway, if I don't
answer the page this weekend, it's because my batteries are on the blink,
so feel free to call me if you need help.. /Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 15 Nov 85 14:54:59 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Brett Fairchild. 

I answerred the message he left for us Friday morning.  He had questions about
running TWICE to define a new pack.  I left him my home phone number in case
he has problems running TWICE this weekend.  I haven't told him my beeper 
number, since I expect you might want to work out some sort of an arrangement
for that service.

By the way, his ONTYME name is IPC.B/FAIRCHILD, I told him to use TXS.J/SMITH
as my ONTYME name.

/JMS
From: FLETCHERC@X930.Tymnet 
Date: Tue, 19 Nov 85 13:32:46 PST 
To: jms 
Subject: various 

Joe:
I like the format of your "reports" on calls for TYmcom-X assistsance.
Can they go in a file anywhere or jst down the mail hole?\

PlEASE keep track of occurences and durations of phone call requests
from Brett Fairchild and company.  That goes for $85/hour since they
did not choose to buy any regular software maintenance.

Don't give jhim your beepeer number either.  We need to figure out a
way to givvvve him analog of TYMCOM-x emergency service at a price.

	Craig
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 29 Nov 85 19:38:47 PST 
To: ntd.ds@ontyme 
Subject: Base and/or system 24 losing characters. 

System 24 (the experimental 3350 machine) is sending garbled characters.
Sometimes ">" comes out as ".", "v" as "f!", and sometimes the columns output
by DIR don't line up because spaces are getting lost.  At this point, I can't
tell if C24 is flakey, or if it's base is acting up.

Just thought you'd want to know.
/JMS
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 6 Dec 85 7:48:29 UT
From: IPC.SRAMAIL@Ontyme.Tymnet 
Date: 05 DEC 85 17:00:44 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A76710@Ontyme.Tymnet 
Subject: D-47 - A DEDICATED GOVERNMENT SYSTEM 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Tue, 10 Dec 85 23:25:35 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

     
     DATE:  DECEMBER 4, 1985  
     
       TO:  ALL INTERNAL NON-GOVERNMENT USERS VALID ON HOST D-47 
     
     COPY:  ED ROOP 
     
     
     FROM:  DEBBIE BURNS 
            JAN KRIVANEC 
            RESOURCE PLANNING & MANAGEMENT   
     
     SUBJ:  D-47 - A DEDICATED GOVERNMENT SYSTEM  
     
     
     
     ON JANUARY 1, 1986, SYSTEM D-47 WILL BECOME A DEDICATED GOVERNMENT    
     SYSTEM.  PLEASE BEGIN TO MOVE YOUR USER FILE DIRECTORY AND ANY   
     INTERNAL TAPES TO ANOTHER 370 SYSTEM.   
     
     IF YOU ARE ONLY VALID ON SYSTEM D-47, PLEASE CALL RPM TO BE VALIDATED 
     ON ANOTHER 370 SYSTEM.   
     
     ON DECEMBER 31, 1985, WE WILL REMOVE ALL NON-GOVERNMENT INTERNAL 
     USERS FROM D-47.    
     
     IF YOU HAVE ANY QUESTIONS, PLEASE CALL DEBBIE BURNS AT 408 446-6629,  
     OR MYSELF AT 408 446-7081. DEBBIE AND I CAN ALSO BE REACHED BY   
     ONTYME AT IPC.SRAMAIL.   
     
     THANK YOU.
From: Osman Guven <osman@C930.Tymnet> 
Date: Wed, 11 Dec 85 11:16:44 PST 
To: fletcherc, dencoff, Carl A Baltrunas <carl@C930.Tymnet>, jms 
Subject: Vacation schedule.. 

I will take 12/12 & 12/16 & 12/18 as vacation days and couple of
half day copm time next friday and tuesday. I will be in library
studying for the final exams. if you need anything leave mail on
system or phone message at home.
Osman..

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
From: FLETCHERC@X930.Tymnet 
Date: Wed, 22 Jan 86 16:42:20 PST 
To: carl, osman, jms 
Subject: standby 

Would the person on call send me mail Monday so I know who to contact
if the answering service calls me.  Of course, mention that you are on
call in the mail message.

	Craig
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
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 3 Feb 86 10:02:39 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, JMS, DENCOFF, FLETCHERC 
Subject: CRASH FILES.. 

SORRY, BUT I HAVE ALLREADY DELETED THE CRASH FILES LAST WEEKEND
BECAUSE OF "DISK STORAGE SORTAGE". IF WE NEED THEM WILL NEED 
TO GET THEM FROM ALL-FILES OR BACKUP TAPES.

OSMAN..
From: FLETCHERC@X930.Tymnet 
Date: Mon, 3 Feb 86 11:59:29 PST 
To: jms 
Subject: TOPS10 updates 


Joe,
I'd like to have a look at the TOPS10 updates that come in just to
see what we're getting from DEC.
Also, what happened to your RAINBOW?  Noticed an empty space
in your cube.

		Craig
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
Date: Sat, 8 Feb 86 21:33:20 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, dencoff, Osman Guven
	<osman@X930.Tymnet> 
Subject: Silicon Valley Digital PC Users Group 

Meets at 7:30pm at DEC, 2525 Augustine Drive in Santa Clara.

Digital will host the meeting this time.  There will be hands on demonstrations
of various pieces of software on several PC's.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 19 Feb 86 12:51:54 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: TTYTAB 

Do you plan to make TTYTAB output the login string that was used to build
an AUX circuit?  I'd like to be able to monitor the progress of MHX and
DO COPY jobs.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Feb 86 13:09:49 PST 
To: Joe Smith <jms@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet> 
Subject: Dentist... 

I'm off to the dentist for a couple of hours... so I will notbe available
at home.  In emergency, call my beeper.  sigh!

/Carl
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 10 Feb 86 22:09:35 UT
From: ISGFM.W/LANDGRAF@Ontyme.Tymnet 
Date: 10 FEB 86 20:07:42 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J05198@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Wed, 19 Feb 86 22:56:08 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>




    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO: ISG-86-019
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: FEB. 10, 1986




TO:  ISG EMPLOYEES


SUBJECT:  REISSUING OF CORPORATE AMERICAN EXPRESS CARDS




l.  Incidents  involving terrorist actions have increased in number
    and   severity   in  the  past  months.    In  order  to  avoid
    unnecessary  danger  to  employees,  Corporate American Express
    Cards are being reissued with the company's name omitted.

2.  The  new  Corporate  American  Express  Card will have the same
    account  number  as  the  card the employee is currently using.
    Therefore,  when  the new card is received, the card containing
    the company's name should be destroyed.

3.  The  new  Corporate American Express Card is still considered a
    corporate  card  and  all  rules and regulations governing such
    card is still in effect.

4.  The  reissuing  of all cards should be completed by 1 April 86.
    If  you  have  not  received your new card by that date, please
    contact Glenda Goodman in St. Louis at (314) 234-6898.



    R. A. Mundloch
    Controller - ISG
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 17 Feb 86 18:06:13 UT
From: CBU.R/KAMINSKI@Ontyme.Tymnet 
Date: 17 FEB 86 16:55:09 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J07681@Ontyme.Tymnet 
Subject: PROMOTION OF TIMOTHY A. HOLEMAN 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Wed, 19 Feb 86 22:56:13 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

     MCDONNELL DOUGLAS          ADMINISTRATIVE          NO:  DIS-86-007  
DIVERSIFIED INFORMATION SYSTEMS     BULLETIN            DATE:  17 Feb 86
         COMPANY



To:          ISG Lists A through E

Subject:     PROMOTION OF TIMOTHY A. HOLEMAN





I am pleased to announce the promotion of Tim Holeman to Manager - Business 
Applications effective 06 January 1986.  In this position, Tim will continue to 
report to Lawrence Hadding and will be responsible for all technical offices 
within Professional Services which support state and local government 
services.  These offices are located in Missouri, Oklahoma, Illinois and 
Pennsylvania and primarily support state government custom development.

Tim has over ten years of experience with McDonnell Douglas and has held 
various technical, supervisory and management positions during this time.

Please join me in congratulating Tim on this well deserved promotion.



(Original signed Charles K. Martin)



Charles K. Martin, Director
Professional Services Company
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 17 Feb 86 18:06:29 UT
From: CBU.R/KAMINSKI@Ontyme.Tymnet 
Date: 17 FEB 86 17:04:03 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J07685@Ontyme.Tymnet 
Subject: ISG MANAGEMENT INFORMATION SYSTEMS -- ST. LOUIS AREA SERVICE 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Wed, 19 Feb 86 22:56:17 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

     MCDONNELL DOUGLAS          ADMINISTRATIVE          NO:  DIS-86-008  
DIVERSIFIED INFORMATION SYSTEMS     BULLETIN            DATE:  14 Feb 86
         COMPANY



To:          ISG Lists A through E, MDAIS Lists A through E

Subject:     ISG MANAGEMENT INFORMATION SYSTEMS -- ST. LOUIS AREA SERVICE
             CENTER





The formation of the ISG Management Information Systems Service Centers was announced 
in ISG Administrative Bulletin ISG-85-181 dated 26 November 1985.  To carry out the 
responsibilities of the St. Louis Area Service Center, I am pleased to announce the 
following assignments reporting to me:

    Ed Langford is appointed Manager, Systems Planning and Architecture.  In this 
    capacity he is responsible for the planning, architecture and data management of 
    internal information systems, evaluation and implementation of productivity tools 
    and development methodologies, internal forms management and the office 
    automation user support center.

    Tom Farrington is appointed Manager, Systems Analysis and Programming.  In this 
    capacity he is responsible for the analysis and programming support of all 
    internal systems.

The systems supported by the St. Louis Area Service Center include all internal 
systems for the Diversified Information Systems Company and for the Information 
Processing Center, the St. Louis Regional Common Systems and direct support of other 
internal systems requested by other ISG units.

The above assignments are effective immediately.  Your continued support of Ed and 
Tom in accomplishing their new responsibilities will be appreciated.



(Original signed by Karen Gedera)


Karen S. Gedera, Director
Management Information Systems
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Tue, 18 Feb 86 18:06:19 UT
From: ISGFM.W/LANDGRAF@Ontyme.Tymnet 
Date: 18 FEB 86 14:44:36 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J08015@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Wed, 19 Feb 86 22:56:22 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

     MCDONNELL DOUGLAS       ADMINISTRATIVE     NO: ISG-86-021
INFORMATION SYSTEMS GROUP      BULLETIN         DATE: Feb 13 '86


TO:       ISG MANAGEMENT LEVEL A-E

SUBJECT:  ISG GROUP HUMAN RESOURCES FUNCTION



I am pleased to announce the formation of the ISG Human Resources 
Group Staff organization.

This organization, reporting to me, will serve as a focal point 
for Human Resource activities throughout ISG providing overall 
policy guidelines and direction.  In this capacity, it will serve 
as a resource group for the Regional and Business Unit Human 
Resource staffs.

COMPENSATION

Gordon W. McCandless, Manager - Compensation, will manage the 
resources required to develop, coordinate, implement and monitor, 
organization, compensation, pay practice and travel/relocation 
policies and programs for the ISG.  He will also provide 
functional advice and counsel to Senior Management and consulting 
services to the Business Unit and Regional Human Resource 
organizations.

Judy H. Lowe, Specialist - Human Resources, will assist Gordon in 
this function.


BENEFITS

Joe H. Hoffman, who is promoted to Manager - Benefits, will 
provide counsel and leadership in the benefit area including the 
administration of benefit policies and programs for all of ISG, 
both domestic and foreign locations.  Joe will also be responsible 
for the design of benefit programs and practices suitable for ISG 
employees.


EQUAL OPPORTUNITY PROGRAMS

Lois J. Ford, who is promoted to Manager - EOP, will act as a 
consultant to the Human Resources Regional and Business Unit 
staffs in all matters related to EEO/AAP and discipline concerns 
and serve as the interface on these matters between Corporate and 
ISG.


HUMAN RESOURCES SERVICES AND ADMINISTRATION

Corinne S. Karp, Specialist - Human Resources Administration, will 
provide policy direction on employee relations activities, such as 
Employee Recognition, Safety and Security; monitor miscellaneous 
ISG Human Resource programs, have responsibility for the Employee 
Handbook and Human Resource policy interface on the Management 
Handbook, and function in an administrative capacity.


HUMAN RESOURCE SYSTEMS AND RECORDS

Jan L. Belfield, Manager - Human Resource Systems and Records will 
develop and communicate Human Resource Systems and Record 
Standards and Policies to ISG Group Management, Regional and 
Business Unit Human Resource Systems Organizations.  Jan will also 
be responsible for Human Resource Systems and Records Planning and 
Architecture, and determining requirements for the initial 
development and on-going enhancements of ISG consolidated Human 
Resource Systems.


MANAGEMENT AND PROFESSIONAL DEVELOPMENT

Kenneth G. Best, Director Management and Professional Development 
is responsible for the ISG Management and Professional Development 
function.  In addition to the St. Louis based staff, Ken is 
responsible for the Regional Management and Professional 
Development Centers in Irvine and Cupertino.  The Irvine Center is 
managed by Penny Scholl.  The Cupertino Center is managed by 
Stephen Reynolds.  The Management and Professional Development 
organization provides ISG's core management development programs 
as well as specific Human Resource development activities that are 
unique to particular Business Units.



In addition, Jori G. Alwell, Group Staff Assistant, will have 
responsibility both in Human Resources and in the Executive 
Office.  Reporting to me, Jori will continue to be responsible for 
the ISG Annual Bonus Plan and Executive Compensation matters.  In 
the Executive Office, she will assist and support Rich Schmitt, 
Executive Assistant to the Group Executive Officer in certain 
projects and tasks.

I am confident that this organization will help provide the level 
of Human Resources support that our Business Units expect and 
deserve.




M. R. Becker
Staff Vice President
Human Resources
From: Osman Guven <osman@X930.Tymnet> 
Date: Thu, 20 Feb 86 0:13:24 PST 
To: jms 
Subject: BOTLOD 

I could not get BOTLOD to puch paper tape on C70.
GF M33
RUN BOTLOD
I THINK IT WAS GETTING ILL MEM REF..
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 20 Feb 86 11:25:49 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>, Lois Frost
	<FSC.L/Frost@ONTYME.Tymnet> 
Subject: Working at home this morning 

I shall be working at home again this morning.  The sharp pains have
receeded, but I have another dental appt this afternoon and will come
in to the office after the appt.  (proabably around 4pm).  See you
then.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 20 Feb 86 12:33:29 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: SETOP? 

By the way, why are you scrounging around for SETOP stuff?

/Just curious
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 20 Feb 86 15:28:57 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: SETOP? 
In-reply-to: your message of Thu, 20 Feb 86 12:33:29 PST

SETOP was due to a conceptual error.  I was using the wrong program to give
myself WA license on all systems.  Now that it hsa been set on 34 that should
do it.  By the way, (MPL:930)SETOP.LOG has a list of SETOP files.  SETOP.SAV
exists on many systems in MPL.  Do you want to delete them?
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 20 Feb 86 15:43:47 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: PCOM version 1.63 

I have PCOM.SHR and PCOM.LOW in my directory on all 10s with appropriate license.
Do we want to replace SYS:PCOM with verion 1.63 on all systems?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 22 Feb 86 3:44:41 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: D23 fixed. It just came up. /Carl 


From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 24 Feb 86 8:37:43 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Cc: Osman Guven <osman@X930.Tymnet> 
Subject: What was the hotline calls? 

My beeper went off 3 times.  What happenned to D25?  I notice that C26 has
only 4000 pages free.
From: Osman Guven <osman@X930.Tymnet> 
Date: Mon, 24 Feb 86 21:31:32 PST 
To: jms 
Subject: Just to say "Hello".. 


From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 24 Feb 86 22:28:07 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: Re: Just to say "Hello".. 
In-reply-to: your message of Mon, 24 Feb 86 21:31:32 PST

Ho goes 26?  Did BOTLOD work?
I ran 1DOWN so that C24, X62, X95, X118, and X930 have messages if
"host not available".
Received: from C39.Tymnet by X930.Tymnet; Thu, 23 Jan 86 16:08:45 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 23 Jan 86 16:03:11 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet> 
Subject: Re: FLEXIT reliability 
In-reply-to: your message of Wed, 22 Jan 86 15:20:52 PST

I use a special version of FRMRUN with most stuff stripped out since
there is no terminal stuff.  I use (SYS:39)SAIL.  If you can tell
me what was on the CTY for the dates/times that correspond to the
crashes you're interested in, I can probably tell you pretty much what
was happening at the time.  The call to FLEXIT is just calli( xx, calli!FLEXIT );

I can give you the code when I know what program(s) are involved,
there are at least three places that do CREFRM's.  Often followed by
a disown.  If it is MAILER, it could be run by XEXEC.  Do you know
who the parent is from the crash?

-Bill
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
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 14 Feb 86 1:07:41 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Standby and call-in pay 

The file (M33)HOTLIN.PAY shows that the three of us are all missing at least
one week's standby pay.  I did not get paid for the two call-ins I had on
January 20th.
			Joe
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 19 Feb 86 0:23:25 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: SETOP 

SETOP does NOT exist on most systems.  It is supposed to exist on 34 and
on most of "our" systems [930,118,24] and some other F3s and 2020s.  If
it exists elsewhere, it ought to be deleted!!!

I noticed that your job was hanging up & I killed it, saving the LOG file.
Please, don't copy SETOP.SAV to all systems.  It's bad enough that 34 is
a "master"... we don't need to have multiple masters, sicne they will get
overwritten anyway with whatever is on system 34 by PJ.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Feb 86 13:03:32 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: TTYTAB was a hack 

TTYTAB was an interim hack to determine what would and would not work to
get a list of tty table info.  I do not intend to expand upon it.  The
useful code from TTYTAB has already been incorporated into SY using th
"P" option.  It is not released because it does not have a viable way to
determine if the string is available in all monitors.  Re: WE NEED TO
MAKE AN FETTBL THAT MATCHES THE FEATURES IN OUR MONITOR!!!!!  Then all
these nifty programs can be propagated, and will run on various flavors
of monitors.
  I'm thinking of trying to implement negative indicies to FETTBL so we
will NOT conflict with DEC, but I haven't even looked at what we have at
the moment.  I do believe that it is NOT in /N at all.

On 930, use SY to see what progress your MHX jobs are making.  Actually,
MHX should be trashed and another replacement used...

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 20 Feb 86 10:04:58 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: BOTLOD 

Sorry about that.  I made BOTLOD test the PC flags, and BLT itself into hi core
if running in EXEC mode, or punch tape in USER mode.  A typo testes the wrong
flag.  The new version has been telecopied to all the CUP KIs.  /JMS
From: FLETCHERC@X930.Tymnet 
Date: Tue, 25 Feb 86 15:59:50 PST 
To: jms 
Subject: 7.02 

Can we run 7.02 on the 2020 to test some of the KI10 applications
programs running under 7.01a at Hunter Liggett?

	CRaig
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 25 Feb 86 16:41:29 PST 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet 
Subject: 999-999 -99 

Beeper went off at 4:39pm for "999-9999 -99".  Assumed to be a test.
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
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Feb 86 3:52:21 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: 24XP18 wirks! 

Sew, eye will meke uh 26XP18 for the Kay-El wen eye wake...

-Chow!   /Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 27 Feb 86 17:23:40 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Cc: DENCOFF 
Subject: DENCOFF is not in group 0. 

Dennis is not able to look at interesting hosts (such as 1158 and 1160)
because he is limited to host 1274, host 930, and all the hosts in group 1.
Could we add group 0 to his access list in the MUD?
Received: from C36.Tymnet by X930.Tymnet; Fri, 28 Feb 86 9:38:25 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 28 Feb 86 9:33:09 PST 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: DENCOFF is not in group 0. 
In-reply-to: your message of Thu, 27 Feb 86 17:23:40 PST

Joe,
   Thanks for suggesting group 0.  I see that I'm am getting to have use for it.
/Dennis
Received: from C36.Tymnet by X930.Tymnet; Fri, 28 Feb 86 10:42:03 PST
Return-path: <DENCOFF@C36.Tymnet> 
From: Dennis Coffey Tymcom-X <DENCOFF@C36> 
Date: Fri, 28 Feb 86 10:28:11 PST 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: (TYMNET)NODES.MAP updated for 14-bit node numbers. 
In-reply-to: your message of Thu, 27 Feb 86 17:27:27 PST

Joe,

      I've looked at SYSTAT on host 36.  The update looks good!  

      I noticed that some of the elements in that field are only 8 characters
long ("DETACHED" and some of the already existing explicit node names),
causing non-alignment of the following 3 fields (port #, program name and
program size).  I know this is probably something that you are already taking
care of--it looks like the NODES.MAP update data input is not complete.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 3 Mar 86 8:51:18 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Beeper person. 

I am on standby this week.   I'll be checking my messages on 930 every break.
Home phone = (415)790-0608.		/JMS
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
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 3 Mar 86 15:53:05 PST 
To: wrs 
Subject: Limit of 8192 in LS 

I noticed that LS can't be used on TYMNET:25 (13627 files) and TYM5:54
(8803 files).       Is 8192 a hard limit?  (wouldn't really bother me if it was)
Received: from C39.Tymnet by X930.Tymnet; Mon, 3 Mar 86 20:54:50 PST
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 3 Mar 86 20:48:20 PST 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: Limit of 8192 in LS 
In-reply-to: your message of Mon, 3 Mar 86 15:53:05 PST

Yes, it is.  Sadly, there is no way to change the size of an array in SAIL,
and there is no way to predict the number of files in a directory, although
I could deduce an upper limit by miltiplying 102 by the number of pages in
the UFD.  Actually, I could expand the array, perhaps - I'd really be alloc'ing
a new one and releasing the old one so I'd have to do a BLT, but its probably
better than crashing the program.  If anyone cares, I'll try this.  -Bill
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
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 9 Mar 86 7:18:37 PST 
To: OSMAN@C26.TYMNET 
Subject: DELETE (JMS)*.TMP 

(begin forwarded message)

Received: from C26.Tymnet by X930.Tymnet; Sun, 9 Mar 86 6:51:34 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 9 Mar 86 6:49:55 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: DELETE (JMS)*.TMP 

If the disk fills up, kill the job(s) logged in to JMS.  This should delete
the *.TMP files in my directory.  See (JMS)FILLUP.LOG for details.

(end forwarded message)

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
Received: from C26.Tymnet by X930.Tymnet; Sun, 9 Mar 86 6:51:34 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 9 Mar 86 6:49:55 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: DELETE (JMS)*.TMP 

If the disk fills up, kill the job(s) logged in to JMS.  This should delete
the *.TMP files in my directory.  See (JMS)FILLUP.LOG for details.
Received: from C26.Tymnet by X930.Tymnet; Sun, 9 Mar 86 7:20:57 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Sun, 9 Mar 86 7:21:43 PST 
To: OSMAN@C26 
Subject: C26 

I have nothing more to test on C26.  I'm going to bed.
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
From: FIN.SECURITY@Ontyme.Tymnet 
Date: 05 MAR 86 17:55:50 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: A33584@Ontyme.Tymnet 
Subject: Parking Lots at Valley Green 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Mon, 10 Mar 86 12:58:32 PST
Resent-To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet>

DATE:      5 March 1986

TO:        All Employees

SUBJECT:   Parking Lots at Valley Green

From:      Ron Birdsong - Manager, Security Control


On Saturday and Sunday, 8 and 9 March 1986 the parking lots
at 20605 and 20705 Valley Green (VG1, VG2 and VG3) will be
resurfaced and striped.  Those employees needing access to
these facilities should park on the south side of VG2 and
the west side of VG1 and VG2 adjacent to the lobby entrance.
All employees entering VG1 and VG2 can do so through the
front lobby entrance.  This entrance will be open 24 hours
a day during this weekend as well as the lobby to VG3.
All employees should have their vehicles moved to the
appropriate spaces no later than 5:00 AM on 8 March 1986.
Your assistance in this matter is greatly appreciated.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Mar 86 13:51:25 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: what happened to 930? 45 mins ago? 


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
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 10 Mar 86 14:44:30 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Osman Guven <osman@X930.Tymnet> 
Subject: Re: System 38 DSKB1 mismatch 
In-reply-to: your message of Mon, 10 Mar 86 14:35:16 PST

Gee, TOPS-10 never had that problem.  But then TOPS-10 does not allow a 9-pack
DSKB, it has DSKB0 to DSKB7 only.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 14 Mar 86 22:06:43 UT
From: ISGFM.W/LANDGRAF@Ontyme.Tymnet 
Date: 14 MAR 86 18:25:34 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J17385@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Mon, 17 Mar 86 0:46:00 PST 
Resent-to: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven
	<osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Resent-cc: Craig Fletcher <fletcherc@X930.Tymnet>, Bruce Ring
	<bring@C26.Tymnet>, helgem 





    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO: ISG-86-032
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: Mar 14, '86


         ISG Lists A - H

         DEFERRAL OF ISG MANAGEMENT DEVELOPMENT

CC:      Business Unit Coordinators



One  of  the  principles  given  to us this year is to manage ISG's
operating  results month by month.  In so doing, we have determined
a  need  to  minimize  ISG costs in the first half of the year.  To
respond  to that need we have moved the Management and Professional
Development  Phases  I-VII  provided  to  MDAIS  into the first two
quarters of 1986.

To  accomplish  this adjustment, MDAIS Phases have been substituted
for  ISG  Phases.    Subsequently, those ISG Phases scheduled after
March 17 will be deferred into the last half of the year.
 
In  addition  to  having  a  significant  favorable impact on ISG's
operating  results  for the first half of this year, the completion
dates  for  Phases  III  and V are moved forward.  As a result, the
full  line of Phases will be in production for the ISG beginning in
June.

The  revised  schedule  and allocations will be available from your
business  unit  coordinator  on  April 8.  It is important that you
continue   to   plan   for   your,   and/or   your   subordinates',
participation  in  the  proper  Phase.  Advance planning allows for
timely  notification  of  participants and for time to complete the
required prework for Phase attendance.




K. G. Best, Director 
Management & Professional Development 
McDonnell Douglas Information Systems Group
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
Date: Mon, 17 Mar 86 22:34:34 PST 
To: CHERIE 
Subject: He's on his way home. 


From: Cherie Marinelli <Cherie@X930.Tymnet> 
Date: Mon, 17 Mar 86 22:46:32 PST 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Thanks! 


From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 18 Mar 86 0:17:32 PST 
To: William R Soley <wrs@C39.Tymnet>, Carl A Baltrunas <Carl@X930.Tymnet>,
	Osman Guven <osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet> 
Subject: (TYMNET)NODES.MAP 

Bill,
  This was updated to handle node numbers > 7777 for SYSTAT and it's
obvious that some programs out there need to be fixed... i.e. SCREAM
and ACSCAN... but the problem probabluy is not in the fact that the
arrays for node numbers is smaller than NODES.MAP, but that there are
people logging in from nodes larger than 7777!
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 19 Mar 86 12:24:02 PST 
To: TYM.DJM@ONTYME 
Subject: What happened to JMS on 1274 and 2096? 

I have been valid on the mux at 1274 and 2096 for almost a year.  Now all of
a sudden it says "Please Ontyme TYM.DJM FOR VALIDATION ON THE MUX".  Please
reinstate JMS with char 1C, CARL with char 60, and OSMAN with char 60.
JMS, CARL, and OSMAN are the names of the three people in TXS (Tymcom-X
Support).  We need the MUX to keep the PDP-10s running; without it NTD will
have problems running NAD and LOADII.

	Joe Smith	(415)794-2512.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 24 Mar 86 11:52:09 PST 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Standby 

I am on call this week.  790-0608
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
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 26 Mar 86 1:46:09 PST 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <bring@C26.Tymnet>, helgem 
Subject: XEXEC.PRJ :930 

The list of milestones and objectives, albeit simplified to a few points
without gorey detail, can be found in ((CARL:930)XEXEC.PRJ).  There were
many many more milestones, but in terms of what is left to do, these were
all that came to mine mind.
/Carl
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
Date: Mon, 31 Mar 86 4:44:00 PST 
To: g.joesmith@score.arpa 
Subject: Earthquake, 5.6 at 3:56. 

(begin forwarded message)

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
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

(end forwarded message)

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

Received: from C57.Tymnet by X930.Tymnet; Thu, 27 Mar 86 12:12:13 PST
Received: from X62.Tymnet by C57.Tymnet; Thu, 27 Mar 86 12:11:56 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Mar 86 12:10:16 PST 
To: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Significant event! 

My car is at 077762 and will pass the 077777 mark on my way to cupertino
this afternoon!  /Carl
From: FLETCHERC@X930.Tymnet 
Date: Mon, 31 Mar 86 14:02:01 PST 
To: carl, osman, dencoff, bring, helgem, jms 
Subject: TYMCOM-X meeting 


TYMCom-x MEEting will be delayed until 11:00 a.m. on Tuesday
April 1.  See you there. This is not an April fool's joke.

	Craig

Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 2 Apr 86 22:13:43 UT
From: ISGFM.K/PETERS@Ontyme.Tymnet 
Date: 02 APR 86 18:48:42 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J23907@Ontyme.Tymnet 
Subject: "McDonnell Douglas Information"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 4 Apr 86 22:53:57 PST 
Resent-to: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet> 


            McDonnell Douglas Information Systems Group

                      ADMINISTRATIVE BULLETIN

No. ISG-86-040                                   April 2, 1986


To:      ISG List A - E

Subject: ASSIGNMENT OF RICHARD G. HATZ AND JAMES G. MCDANIEL



I am pleased to announce the assignment of Richard (Dick) Hatz to
the position of Manager Contracts-ISG, reporting to me.

Dick's responsibilities in this new position will encompass both
commercial and government contract policy statements, provide
contract and negotiation assistance to the various business units
on an as requested basis, and coordinate MDC corporate and ISG
required contract related reports and special projects.

Reporting to Dick will be James (Jim) McDaniel who will be
primarily responsible for performing government contracting
activities relating to the above functions.  In addition, Jim is
currently acting as the ISG focal point for the ISG Government
Contract Survey.

Dick has twenty years of experience with MDC.  He started his
career in MCAIR Procurement and for the past fifteen years with us
has performed in various management positions in all areas of
commerical and government contracts and pricing, both domestic and
international.

Jim brings considerable expertise and experience in the government
contracting sector and has been performing in various capacities
in this regard since he joined MDC in May of 1979.

Please join me in wishing success to Dick and Jim in their new
assignments.



R. A. Mundloch
Staff Vice President and Controller-ISG
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Thu, 3 Apr 86 22:06:36 UT
From: CBU.R/KAMINSKI@Ontyme.Tymnet 
Date: 03 APR 86 19:27:34 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J24440@Ontyme.Tymnet 
Subject: REORGANIZATION OF DIVERSIFIED INFORMATION SYSTEMS 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Fri, 4 Apr 86 22:54:01 PST 
Resent-to: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet> 

     MCDONNELL DOUGLAS          ADMINISTRATIVE          NO:  DIS-86-010
DIVERSIFIED INFORMATION SYSTEMS     BULLETIN            DATE:  07 Apr 86
         COMPANY



To:          ISG Lists A through E

Subject:     REORGANIZATION OF DIVERSIFIED INFORMATION SYSTEMS
             COMPANY COMMERCIAL CONTRACT ADMINISTRATION





As the final step in the division of responsibilities and personnel
between the Diversified Controller's and ISG Group Controller's
organizations, I am pleased to announce the appointment of Jerry L.
Welsh as head of Commercial Contracts Administration for the
Diversified Controller.

With MDC since 1972, Jerry originally was involved with governmental
contracting for McDonnell Douglas Astronautics Company and in 1976
joined the McAuto Commercial Contracts group.  Subsequently he has
held a series of positions of increasing responsibility within that
organization and is well prepared to now assume a leadership role in
the Diversified Controller's organization coordinating contractual
activities in support of several Business Units - predominately
Diversified Information Systems Company.

Jerry replaces Richard G. Hatz who, along with James G. McDaniel,
will form the Contracts policy and coordination section of the ISG
Group Controller's organization.

In addition to his existing contract development and negotiating
team, Jerry will also have the Diversified contracts administrative
groups in St. Louis and Vienna, Virginia reporting to him.

Please join me in wishing Jerry success in his new assignment.





Joseph M. Godwin, Director
Contracts and Customer Services
Diversified Information Systems Company
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 14 Apr 86 0:11:14 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: DStandby duty 

I am on call this week.		Joe (415)790-0608
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 22 Apr 86 18:06:19 PST 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: 24XP24 

BOTLOD works, now DIRIT doesn't.  (CARL)DIRECT and CKSUM still work,
but DIRIT says "TOTAL OF 0 PAGES IN 0 FILES".  Grrrrrrr.
Received: from C36.Tymnet by X930.Tymnet; Wed, 23 Apr 86 12:52:13 PST
Received: from X62.Tymnet by C36.Tymnet; Wed, 23 Apr 86 12:51:21 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 23 Apr 86 12:43:32 PST 
To: jms@930 
Subject: testing 

test
Received: from C36.Tymnet by X930.Tymnet; Wed, 23 Apr 86 12:52:18 PST
Received: from X62.Tymnet by C36.Tymnet; Wed, 23 Apr 86 12:51:26 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 23 Apr 86 12:51:09 PST 
To: jms@930 
Subject: another 

test
Received: from X95.Tymnet by X930.Tymnet; Wed, 23 Apr 86 15:06:58 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 23 Apr 86 15:03:53 PST 
To: dencoff, dernst 
Subject: Node 6760 and host 95 are back up. 

I reseated all the connectors, put "IN" and "OUT" markings on the ribbon
cables, and reloaded from (CURR11:57)ND6760.BND - it now works.

			/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 23 Apr 86 23:05:59 PST 
To: Dennis Coffey <dencoff@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Mux 

CARL is now valid on 2096, I didn't try OSMAN or JMS.
I am still not valid on 1274... T'would be nice for both,
but I won't make waves as long as one is around.
/Carl
From: JMS@X930.Tymnet 
Date: Thu, 24 Apr 86 23:25:35 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: "From: Joe Smith Street in Fremont"... 
~v: 

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Thu, 24 Apr 86 23:20:26 PST 
To: Osman Guven <osman@X930.Tymnet> 
Subject: P24 is on NEWMON 

P034/P24 is in (SYS)NEWMON on systems 930, 95, 62, and 24.  C24 needs to be
reloaded from NEWMON.

Are we still planning on having a meeting Friday?  Give me a call at home if
it is going to be tomorrow.

		/JMS
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 25 Apr 86 10:44:59 PST 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet>, wrs 
Subject: "Hackers - Wizards of the Electronic Age" on Channel 9. 

KQED is showing a documentary with Greenblatt, Stallman, Wozniac, Budge,
Atkinson, the PDP-1, MIT, etc.  On channel 9, 11:00pm Friday and 4:30pm Sunday.
From: Osman Guven <osman@X930.Tymnet> 
Date: Sun, 27 Apr 86 22:50:26 PDT 
To: Monitor Group <osman@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Bill Mortanson <helgem@930.Tymnet>,
	Dennis Coffey <dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tyment> 
Subject: I am on call 4/28 to 5/4 


From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 28 Apr 86 19:52:15 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: HBO interference 

If you want to see what the message looked like when the hacker messed with
HBO's signal, you can catch the replay of Entertainment Tonight.  Set your VCR
for Channel 4 at 6:00am, the item should show up at 6:17am.

Sally often sets our VCR for that time if something interesting comes up on
the 7:30pm showing and she was not ready to tape it then.


From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 30 Apr 86 10:09:48 PDT 
To: jms 
Subject: trainnet host/gateway #s 

I thought that you had a list of the numbers somewhere... couldn't
find them easily in info, so I'm asking... where is it?
/carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 30 Apr 86 13:05:20 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: trainnet host/gateway #s 
In-reply-to: your message of Wed, 30 Apr 86 10:09:48 PDT

Tymnet hosts 1820 and 1822 are the gateways to Train Net.  They are now in the
INFO tree under the topic GATEWAYS.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 5 May 86 0:47:19 PDT 
To: Monitor Group <jms@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>, Osman
	Guven <osman@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Bruce Ring <ring@X930.Tymnet> 
Subject: I am on call this week 

Home phone (415)790-0608.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 2 May 86 21:06:34 UT
From: ISGFM.K/PETERS@Ontyme.Tymnet 
Date: 02 MAY 86 19:25:06 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J36344@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS INFORMATION"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Mon, 5 May 86 1:19:35 PDT 
Resent-to: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet> 



            MCDONNELL DOUGLAS INFORMATION SYSTEMS GROUP

                      ADMINISTRATIVE BULLETIN


No. ISG-86-50                                        May 2, 1986


To:      ISG Lists A-H

CC:      E. H. Ridenhour

Subject: PERSONAL COMPUTER STANDARD SOFTWARE REVISION


Accelerator:PC  has  been  removed  from  the  ISG  Standard for PC
Software  and  is replaced by QUICKEYS.  QUICKEYS is available from
the  User  Support  Centers  and  is provided to ISG PC users at no
charge.

QUICKEYS  allows your PC to process input faster.  Cursor movements
in  any  PC software can be accelerated to higher speeds.  QUICKEYS
is  a  memory  resident  program and may be incompatible with other
memory resident programs.

To  obtain  a copy of the QUICKEYS software and user guide, contact
the User Support Centers at:


    Cupertino, CA          (408) 446-7031

    Cypress, CA            (714) 952-6152

    St. Louis, MO          (314) 233-5116






R. D. Greco, Director
Information Services - ISG
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 2 May 86 21:06:54 UT
From: ISGFM.K/PETERS@Ontyme.Tymnet 
Date: 02 MAY 86 19:25:58 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J36345@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS INFORMATION"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Mon, 5 May 86 1:19:40 PDT 
Resent-to: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet> 



            MCDONNELL DOUGLAS INFORMATION SYSTEMS GROUP

                      ADMINISTRATIVE BULLETIN


No. ISG-86-51                                        May 2, 1986


To:      ISG Lists A-H

CC:      E. H. Ridenhour

Subject: PERSONAL COMPUTER STANDARD SOFTWARE REVISION


Tym/COMM,  a  product  of  McDonnell Douglas Applied Communications
Systems,  has  been  added  to  the  ISG Personal Computer Software
Standard.    Tym/COMM  is  a general purpose communications package
which  provides  asynchronous  terminal  emulation  (DEC  VT52, DEC
VT100,   and   HP2621),   file   transfer   capabilities,   and   a
sophisticated  procedure  language.    Tym/COMM supports XMODEM and
COPYPC  file  transfer  protocols.    X.PC will be available in the
near future.

The  User  Support  Centers  will  provide  referral  support only.
Applied  Communications Systems will provide product support.  This
software  may  be  obtained  by submitting a procurement request to
your appropriate procurement organization.

Users  who  need  IBM  PC/XT  or  AT&T  PC  6300  telecommunication
software  to  interface  the standard word processor, Multimate, to
OnTyme  electronic  mail  may obtain ISG Mail at no charge from the
User Support Centers.





R. D. Greco, Director
Information Services - ISG
Received: from C36.Tymnet by X930.Tymnet; Fri, 2 May 86 15:02:10 PDT
Received: from X62.Tymnet by C36.Tymnet; Fri, 2 May 86 15:00:37 PDT
Received: From EMSTXS.Ontyme.Tymnet by X62.Tymnet; Fri, 2 May 86 21:06:21 UT
From: ISGFM.K/PETERS@Ontyme.Tymnet 
Date: 02 MAY 86 19:24:12 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J36342@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS INFORMATION"... 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Mon, 5 May 86 1:19:51 PDT 
Resent-to: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet> 



            MCDONNELL DOUGLAS INFORMATION SYSTEMS GROUP

                      ADMINISTRATIVE BULLETIN


No. ISG-86-49                                       May 2, 1986


To:      ISG Lists A-H

CC:      E. H. Ridenhour

Subject: PERSONAL COMPUTER MENU AND MAIL SOFTWARE


Effective  May  1,  1986,  ISG  implemented a new Personal Computer
Installation  and Service program.  This service, provided by Field
Service  Company,  includes  burning in the hardware and delivering
the   hardware/software   to   the   user  as  in  the  past,  plus
installation  of  a  menu  system,  ISG Office Manager, software to
access  OnTyme,  ISG  Mail,  and  all  standard software ordered by
new users.

ISG  Office  Manager  and  ISG  Mail  are now available for current
users  and may be obtained from the User Support Centers.  There is
no charge for this software.

Procurement  requests  should be processed as they have been in the
past.    Requests  for  hardware  service should be directed to the
User Support Centers.

User Support Centers can be contacted at:

    Cupertino, CA          (408) 446-7031

    Cypress, CA            (714) 952-6152

    St. Louis, MO          (314) 233-5116





R. D. Greco, Director
Information Services - ISG
Received: from C36.Tymnet by X930.Tymnet; Fri, 2 May 86 15:32:03 PDT
Received: from X62.Tymnet by C36.Tymnet; Fri, 2 May 86 15:01:08 PDT
Received: From EMSTXS.Ontyme.Tymnet by X62.Tymnet; Fri, 2 May 86 21:06:58 UT
From: NSS.TTE@Ontyme.Tymnet 
Date: 02 MAY 86 21:37:14 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M86970@Ontyme.Tymnet 
Subject: SNA Class - Jim Cavin/Pin Chu 
Resent-from: Tymcom-X Supervisor <TXSSUP@X930.Tymnet> 
Resent-date: Mon, 5 May 86 1:19:56 PDT 
Resent-to: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Jon
	Mosser <mosserj@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>,
	Dennis Coffey <dencoff@C26.Tymnet>, Dan Baigent <baigent@X930.Tymnet>,
	Bruce Ring <bring@C26.Tymnet>, Helge Mortensen <helgem@X930.Tymnet> 

                            M E M O R A N D U M

                                                        [] TYMNET - NETWORK
                                                           SYSTEMS SUPPORT
DATE>      02 MAY 86  13:36

TO>        Distribution

COPIES>    

FROM>      Training and Education


SUBJECT>   SNA Class - Jim Cavin/Pin Chu


-----------------------------------------------------------------------


TTE is pleased to announce that we have secured Jim Cavin of IBM as a great
instructor for a SNA class July 14-18.  This class is an additional to the
recently released 3Q schedule.

Mr. Cavin is an instructor with IBM and has conducted SNA training for
Tymnet in the past.  Those who attended his classes have rated him
outstanding.  Jim will conduct the first 3.5 days of the course covering the
following aspects of SNA-early assumptions, a layered approach, the
elements, configurations and direction.  Pin Chu will once again team with
Jim and will cover Tymnets' implementations of SNA during the last 1.5 days
of the course.

The class will be held in the TTE facility in San Jose.  The cost per
student will be $300.00 for MDC employees.

Field Service Company employees should enroll via Jim Lau - OnTyme FSC.J/Lau

All others should enroll by accessing Executive file - :EXEC *** TTE.ENROLL

We suggest that you enroll early as we expect this class to full quickly.
When you enroll refer to class TTE 240  July 14-18.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Mon, 5 May 86 15:23:44 PDT 
To: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Meeting? 

Are we having a meeting Tuesday morning?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 5 May 86 21:17:07 PDT 
To: jms 
Subject: Code in nodes... 


I'd like to know what code is running in our micronodes... where did you
reload them from?  (NEW11:57) or (DERNST:57) ?

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
Received: from B39.Tymnet by X930.Tymnet; Tue, 6 May 86 16:56:26 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 6 May 86 16:38:52 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Subject: Re: TUMS messages between hosts 930 and 62. 
In-reply-to: your message of Tue, 6 May 86 13:37:05 PDT

Please try to let me know before you do things like that.  I do appreciate
your involvement and don't mind things like this, but its all pretty
delicate right now and I'm trying to hold it together with chewing gum
until I can get the new versions out.  Thanks -Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 6 May 86 17:33:29 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: OPER passwords on 1051. 

Does anyone have a valid OPER password on Rainer Bank's system?  I don't.
Since they are running P034/J I can't use SETE.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Wed, 7 May 86 16:47:49 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: TUMS messages between hosts 930 and 62. 
In-reply-to: your message of Tue, 6 May 86 16:38:52 PDT

I hoped the changes I did to 930 and 62 were localized, I don't want to do
anything that would break your program.  By the way, when do you expect to
have a new version out?  /JMS
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
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 9 May 86 19:50:10 PDT 
To: jms@x62 
Subject: test 

Sending to 62.
Received: from C36.Tymnet by X930.Tymnet; Fri, 9 May 86 19:51:16 PDT
Received: from X62.Tymnet by C36.Tymnet; Fri, 9 May 86 19:50:59 PDT
Received: from X930.Tymnet by X62.Tymnet; Fri, 9 May 86 19:50:46 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 9 May 86 19:50:10 PDT 
To: jms@x62 
Subject: test 

Sending to 62.
  =^3S<