From: <BTHOMSON@39.Tymnet>
Date: Tue, 2 Oct 84 10:32:46 EDT
To: wrs@39
Subject: Status

From: <BTHOMSON@39.Tymnet>
Date: Tue, 6 Nov 84 1:40:43 EST
To: WRS@39
Subject: VERY INTERESTING/ CHASE

Received: From EMSNTD.Ontyme.Tymnet by 39.Tymnet; Wed, 7 Nov 84 20:19:25 UT
From: TYM.BBT@Ontyme.Tymnet
Date: 07 NOV 84 11:20:50  
To: TYM.WRS@Ontyme.Tymnet
Cc: TYM.BBT@Ontyme.Tymnet, TYM.LDUNN@Ontyme.Tymnet, TYM.M/GREEN@Ontyme.Tymnet, TYM.JLEE@Ontyme.Tymnet, TYM.GLW@Ontyme.Tymnet, TYM.G/WALKER@Ontyme.Tymnet, ACS.W/BARTLETT@Ontyme.Tymnet, TYM.WRS@Ontyme.Tymnet, NTD.@Ontyme.Tymnet
Message-id: I69241@EMSNTD.Ontyme.Tymnet
Subject: (CIDNNET) security problems

Received: From EMSNTD.Ontyme.Tymnet by 39.Tymnet; Tue, 13 Nov 84 22:39:19 UT
From: ACS.W/BARTLETT@Ontyme.Tymnet
Date: 13 NOV 84 14:14:45  
To: TYM.WRS@Ontyme.Tymnet
Message-id: A78159@EMSNTD.Ontyme.Tymnet
Subject: USGS

Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Tue, 11 Jun 85 19:10:48 UT
From: TYM.SHINDELL@Ontyme.Tymnet 
Date: 11 JUN 85 11:42:10 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I98736@Ontyme.Tymnet 
Keywords: SECURITY, INFORMATION 
Subject: SECURITY ON INFORMATION 

                         M E M O R A N D U M

DATE>      11 JUN 85  11:34                                 [] T Y M N E T

TO>        BILL SOLEY

COPIES>    LYNN LAWRENCE

FROM>      DAVID M. SHINDELL


SUBJECT>   SECURITY ON INFORMATION


-----------------------------------------------------------------------


BILL,


I KNOW YOU ARE  QUITE BUSY, BUT I HAVE JUST BEEN TOLD BY SHARON YOUNG IN

DATA BASE ADMIN. THAT SHE NOTICED ACCIDENTALLY SOMEONE USING THE INFORMATION

USERNAME ON SYS 28 TO GFD TO CNFE.  AGAIN, THIS IS *TO* USERNAME CNFE FROM

THE INFORMATION USERNAME ON 28!!!  THIS SHOULD NOT BE POSSIBLE UNLESS

SOMEONE HAS GOTTEN PAST YOUR FINE SECURITY, CORRECT??


WE ALSO HAD A SITUATION WHERE THE INFORMATION PROGRAM BOMBED AND WE FOUND

OURSELVES BACK AT THE REGULAR MONITOR WITH THE ABILITY TO DO A DIR COMMAND

AT THE - PROMPT.  THIS WAS LAST WEEK AND THE REASON FOR THE CRASH HAS BEEN

ELIMINATED, BUT IT CERTAINLY SEEMED SUSPECT.



CAN YOU SHED ANY LIGHT ON THIS SITUATION FOR US OR LOOK TO SEE IF THERE IS

A SPECIFIC PECULIAR PROBLEM DURING THIS PHASE OF THE MOON.  WE WOULD GREATLY

APPRECIATE ANYTHING YOU DO.


THANKS.
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 12 Jun 85 1:29:02 PDT 
To: TYM.SHINDELL@Ontyme.Tymnet 
Cc: TYM.W/Bartlett@Ontyme 
Keywords: SECURITY, INFORMATION 
Subject: Re: SECURITY ON INFORMATION 
In-reply-to: I98736@Ontyme.Tymnet of 11 JUN 85 11:42:10

Thank you for your report on the INFORMAION:28 problem.  I haven't been
able to check it out since 28 is down until tomorrow.  I suspect that
the "logout-on-stop" privelege bit has been somehow cleared.  This could
have happened accidentally by validations or someone running NONAME or
rebuilding the LUD or whatever.  Since nobody really supports NONAME,
it is taking a long time to get this bit added to its tables.  Until
then it is invisible, and probably gets cleared when any other bit in
the privelege word is changed.  *SIGH*  Anyway, please refresh my
memory on which systems, other than 28, I should check (as well as
any usernames, other than INFORMATION, if any).  Also, assuming they did
get out of the program, wouldn't they need license to GFD to CNFE?
This is unless CNFE put INFORMATION on its ACCESS.LIS or they are in
the same GAN.  INFORMATION should be in a GAN all by itself (or
perhaps with other public-access user names, like ACCESS# or whatever).

Also, I've forwarded a copy of this and your mail to Wayne Bartlett.
Please copy him on all potential security violations.  Thanks -Bill
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 12 Jun 85 2:01:48 PDT 
To: TYM.SHINDELL@Ontyme.Tymnet 
Cc: TYM.W/Bartlett@Ontyme 
Keywords: SECURITY, INFORMATION 
Subject: Re: SECURITY ON INFORMATION 
In-reply-to: I98736@Ontyme.Tymnet of 11 JUN 85 11:42:10

The CUD system just came up and I noticed that INFORMATION is multitargeted
to 28 and 74.  Since 28 is still down, I decided to check 74 to see if it
had the problem I suspected.  I looked at the LUD with FILDDT and it did
not have PV.LOS set in the privelege word for INFORMATION.  I tried making
a change to the privs using NONAME to see if it would clear the bit, but
it did not.  The culprit must lie elsewhere.  At any rate, the bit is now
set (again) on 74, and assuming that I find the same problem on 28, I will
set it again there tomorrow.  

On the issue of GFD'ing to CNFE, I see no way this could have happened on
74 without license.  CNFE and INFORMATION are in different GANS, and CNFE
has no ACCESS.LIS.  (note there are other names in the GAN with INFORMATION,
but these may not be a threat:
  CANACTG, MKTGINFO, INFORMATION, TYMRES, X121, INTLINFO, NEWS, WIST, INTLTEST2
From the sound of some of these, they should have PV.LOS set as well - let
me know if this guess is true.

I will look in the stream on 28 to see what I can piece together about
the GFD.  I would appreciate the date and approximate time of day (and
time zone) that it happened.  If the person has a hard copy, the job number
of the GFD'd job would be even better.  

-Bill
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 12 Jun 85 2:09:57 PDT 
To: TYM.SHINDELL@Ontyme.Tymnet 
Cc: TYM.W/BARTLETT@Ontyme.Tymnet 
Keywords: SECURITY, INFORMATION 
Subject: Re: SECURITY ON INFORMATION 
In-reply-to: I98736@Ontyme.Tymnet of 11 JUN 85 11:42:10

My last message implied that the GFD problem happened on 74.  I realize
it happened on 28, but I was just checking the conditions on 74 since
it was up at the time.
-Bill
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 12 Jun 85 2:33:00 PDT 
To: Tym.shindell@ontyme 
Cc: TYM.W/Bartlett@Ontyme 
Keywords: SECURITY, INFORMATION 
Subject: SECURITY ON INFORMATION 

I've checked 74 thouroughly.  The stream indicates that from June 1 to date,
the only programs ever run under user (INFORMATION) were
  (ACCSTOR)RCMINI
  (ACCSTOR)MENU
  (MKTGINFO)INFO
  (SYS)LOGOUT
and usually in that order.  This indicates there has been no violation on
74 in June.  I will check 28 tomorrow.

Meanwhile, I did some experimentation.  Even with the PV.LOS bit clear,
the very nature of the way (ACCSTOR)RCMINI is run, as the project code
validator program, forces the PV.RCM bit set.  I verified that this was
happening, and staying set.  Even though this is not as secure as PV.LOS,
it does provide adaquate security, and it does work on older monitors.
It behaves like this (if PV.LOS is clear, and PV.RCM is set):
  If the job is in command mode, and any command other than 
    START
    CONTINUE
    DDT
    REENTER
    DETACH
    LOGOUT
    KJOB
  is typed, the restricted command program will be run, in this case,
  its (ACCSTOR)RCMINI or (SYS)LOGOUT, I forget which.  At any rate, the
  command is not processed.
If it is a new monitor, and PV.LOS is set, regardless of PV.RCM:
  If the job is about to return to command mode, run LOGOUT.

In sumary, even with PV.LOS being accidentally cleared, and even if they
were running the old monitor, and even if they bombed the program, they
should not have been able to do anything but those commands mentioned
above, which are harmless unless the program has been loaded with DDT.

-Bill
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 13 Jun 85 0:57:13 PDT 
To: TYM.SHINDELL@Ontyme.Tymnet 
Cc: TYM.W/Bartlett@Ontyme 
Keywords: SECURITY, INFORMATION 
Subject: Re: SECURITY ON INFORMATION 
In-reply-to: I98736@Ontyme.Tymnet of 11 JUN 85 11:42:10

I've checked 28 thouroughly.  The stream indicates that from June 1 to date,
the only programs ever run under user (INFORMATION) were:
  (ACCSTOR)RCMINI
  (ACCSTOR)MENU
  (MKTGINFO)INFO
  (SYS)LOGOUT

Since the GFD command runs (SYS)GFD, it would have appeared on this list
had it have been run.  This indicates there has been no violation from
that username since June 1.

It is possible, since the SYSTAT program does not do any concurency
control (sigh), that what was seen was the result of a sequence such as:

	job 1 logs in as (INFORMATION)
	job 2 does SYSTAT
	job 2 SYSTAT GETTAB's job 1's user name: (INFORMATION)
	job 2 is dismissed for quantum expired
	job 1 logs out
	job 1 logs in as (CNFE)
	job 2 SYSTAT GETTAB's job 1's PPN: (CNFE)
	job 2 compares user name to PPN, finding different, shows
		job 1 as being logged in to (INFORMATION) and GFD'd
		to (CNFE), when in reality this state never existed.

The probability of this happening on a heavily loaded system, such as 28,
is really very high.  I would venture that at least 1 out of every 100
systats shows some phantom data.

Meanwhile, I have set the PV.LOS bit in the LUD on both 28 and 74.  With
PV.RCM set, there was never much of a threat, except that PV.RCM can be
cleared if the user ever got into DDT or such.  PV.LOS can not be cleared
once it is set.

I will consider this investigation closed unless I receive more evidence.
Thank you for bringing it to my attention, we're much better safe than
sorry.  Keep in touch.

-Bill
From: Postmaster 
Date: Wed, 30 Apr 86 14:43:58 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by EMSTYM.Ontyme.Tymnet
The response was:
:send NSMS.B/FULZ
RECIPIENT IS NOT RECOGNIZED
NSMS    B/FULZ          
NO VALID RECIPIENT


----------
RCPT To: NSMS.B/FULZ@ONTYME.TYMNET 
Received: from B39.Tymnet by EMSTYM.Ontyme.Tymnet; Wed, 30 Apr 86 14:37:25 PDT
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Wed, 30 Apr 86 14:31:14 PDT 
To: NSMS.B/FULZ@ONTYME.TYMNET 
Subject: X121 ADDRESS - REVENUE OPPORTUNITY. 

PER OUR CONVERSATION, THE FOLLOWING MEMO'S FROM JEFF O. EXPLAIN
IN MORE DETAIL;
----------------------------------------------------------------
Received: from EMSTYM.Ontyme.Tymnet by C39.Tymnet; Tue, 25 Feb 86 18:02:14 UT
From: ENS.J/OLIVETO@Ontyme.Tymnet 
Date: 25 FEB 86 09:13:12 
To: TYM.W/BARTLETT@Ontyme.Tymnet 
Cc: ENS.NS@Ontyme.Tymnet, ENS.A/SHUR@Ontyme.Tymnet,
	ENS.B/BENNECOFF@Ontyme.Tymnet, ENS.D/LANE@Ontyme.Tymnet,
	ENS.J/OLIVETO@Ontyme.Tymnet, ENS.J/TSAI@Ontyme.Tymnet,
	ENS.S/WHITE@Ontyme.Tymnet, ENS.R/MCCORMICK@Ontyme.Tymnet,
	ENS.K/CURTIS@Ontyme.Tymnet, NSMS.C/ROLLIS@Ontyme.Tymnet,
	NSMS.N/JENKS@Ontyme.Tymnet, NSMS.D/SHINDELL@Ontyme.Tymnet,
	TYM.W/BARTLETT@Ontyme.Tymnet, TYMOPS.D/MURTAUGH@Ontyme.Tymnet,
	ENS.N/WILLIAMS@Ontyme.Tymnet 
Message-id: I24754@Ontyme.Tymnet 
Keywords: X121, ADDRESSING 
Subject: Secure X.121 Addressing Procedures 

========================  M E M O R A N D U M  ========================
 
          TYMNET - McDonnell Douglas Network Systems Company
        External Network Services [ExNet]  -  Technical Support
     Phone: 703/356-6995    Telex: (23) 82693    Drop: 310600024915
 
DATE>      25 Feb 86  10:30
 
TO>        Neil Sullivan
           Nancy Jenks
 
COPIES>    See Distribution
 
FROM>      Jeff Oliveto
 
SUBJECT>   Secure X.121 Addressing Procedures
 
-----------------------------------------------------------------------
 
     The  document  below,  written   by  Stephen  White,  describes this
groups  outline for  secure  gateway  access. I  have forwarded it on for
inclusion in the March 01 86 Tech Tips.
 
      What  I don't  understand    is  why,   especially  considering our
current  drive to  increase   revenues,  we  GIVE AWAY  such a marketable
commodity.  NO  other network   can  offer  as  secure  gateway access as
Tymnet. I  feel  that  open   gateway   access  via   group  19 should be
considered  a standard  service.  But our  secure NUN access; that offers
password  protection,   multi &  priority  targeting and, if accessed via
FTCC (DNIC  3124)  gateway   specific  access,  should not be FREE.  This
service  should  at least  have  a  $50  monthly   maintance fee plus $15
for each additional NUN.
 
     Something  to  consider,   gateways via  the  BOC's are coming. This
opens  Tymnet and  its customers to a  different breed of hackers. I feel
action  is necessary  before  Tymnet gets  dragged through the newspapers
as Telenet  has. I am  open for  discussions  with anyone from marketing.
 

 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   This document is for Tymnet internal use only.             February 86
   Tymnet External Network Services - OnTyme ENS.ITS or call 703-356-6995
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here follows  a  discussion  of   some   alternative  procedures by which
Tymnet  may be accessed   from  'foreign'  networks. Emphasis is given to
the security  offered   by Tymnet's    advanced   implementation of X.121
addressing.
 
For the  purposes of  this discussion  a 'foreign' network is any network
other  than Tymnet  and  includes not  only other  country's networks but
also other  networks  in the  United States  of America. Network security
from gateways  is  increasingly     important  as  Tymnet  brings up X.75
service  with the Bell   Operating Companies:  over 160 such gateways are
already planned.   Tracing   'hacker'   calls  back   through gateways is
time-consuming  and often  fruitless. Tymnet has no jurisdiction over the
operation  of foreign  networks,  requests we  make to them are completed
at their discretion.
 
A host  may elect to be  open to calls worldwide or to restrict access by
requiring  a username  and/or  password to be  given to Tymnet before the
call is  routed  to their   host.  Tymnet  addressing  also allows for an
address  to be priority  and/or   multi-targeted to a group of hosts. The
wide range  of options    available  enable a  host to  tailor its access
procedures  to satisfy  its requirements for security and ease of access.
 
Tymnet's  security  is at its weakest  at the x.75 gateways where we have
to conform  to  the CCITT   standards  used  by  networks  worldwide. The
spirit  of the  CCITT standards    x.121  and x.75  is  that, as with the
telephone,  if you  know a  number you  may call  it. For hosts that wish
this  we provide  group 19 (see  below). Tymnet is unique in the level of
security  it can provide   through enhanced  features like those outlined
below.  In these matters   prevention is  definitely better than cure, we
therefore  urge  careful  reading    of  this   document  so  that a full
appreciation  of the  implications   of each   alternative may be gained.
 
 
 
 
OPEN GATEWAY ACCESS  - Group 19
-------------------------------
Hosts  which wish to be  addressable from any foreign network with Tymnet
connections  and  with  minimal   network   security  applied may request
(preferably as a written request) to be in group 19.
 
A host  in group 19 is  presumed to have security checks performed by the
host operating  system.  A list  of group 19  host addresses is available
to foreign   administrations. A host that does not wish its address to be
so widely  available  should   request to be   red-listed (un-published).
 
 

 
TYMNET - ExNet Tech Spt                                Page - 02
 
A group 19 host is addressable as....(note the zero after the 3106)..
 
3106+0+<host>+<spid>   where:  3106    = Tymnet's Data Network Identification
                                         code (DNIC) allocated to Tymnet by
                                         the U.S. State Department.
                                 0     = 'identifier' digit.
                               <host>  = a 5 digit host number.
                               <spid>  = up to a 4 digit sub-addr (optional).
 
Example: 3106000127  would address host 127, with no sub-host.
 
Advantages                              Related disadvantages
----------                              ---------------------
Easy Access - no password to enter.     Address may become known to
                                        non-legitimate users, 'hackers'.
 
Wide range of sub-hosts addresses.
 
Worldwide access.                       Access cannot be limited from 
                                        particular countries.
 
                                        no priority/multi targetting
                                        capability.
 
 
 
ALTERNATIVES TO GROUP 19
------------------------
To take  advantage of  Tymnet's  security  checking capabilities on calls
from other  networks  a  host needs  one of the  alternatives to group 19
for its  access  procedures.  Using  one (or  more) of the options below,
with password  protection,  a  very effective  initial screening of calls
is carried  out  by the  network      supervisor.  Note  that  for a host
migrating  from  group 19 it is not  necessary to change the host number,
moreover  the X.121  address that is  sent to the host may not need to be
changed.
 
 
Suppose  host 0127  wishes to  be  removed  from  group 19, or set up for
restricted gateway access: the options are...
 
OPTION (A) - Using a Network User Number (NUN) with password protection.
------------------------------------------------------------------------
Send  a request to  ENS.DTE for a  numeric username as an NUN to form the
x.121  address. We   recommend that a  password  and red-listing (address
un-published)  be requested   though these  items are optional. Addresses
in this format are of the form...(note the 9 after the DNIC)...
 
3106+9+<host>+<spid>   where:  3106   = Tymnet's DNIC
                                 9    = 'identifier' digit
                              <host>  = a 5 digit hostnumber
                              <spid>  = up to 4 digits of sub-addr (optional).
 
   Example:  For host 127, sub-host 00...
             310690012700    (NUN=90012700)

 
TYMNET - ExNet Tech Spt                                Page - 03
 
 
Note:  If a password  is required  then the Tymnet supervisor will prompt
   for the  password  before  the  circuit  is  built to  the hosth the
   address  being a username,  a  password may  be added/changed by the host
   at any time,  even  if no password   was  originally  requested. Password
   changes  take effect  in about  20  minutes  -  considerably quicker than
   removing the host from group 19 and validating an NUN!
 
Advantages                              Related disadvantages
----------                              ---------------------
Easy for Tymnet to identify host.       Host identifiable to an outsider.
 
Password protection,  managed by        Caller may have to enter a password.
host on Tymnet.
 
Password may be in Call                 Password in CUD is passed to host.
User Data (CUD).                        CUD limited to 12 characters.
 
Easy for hosts to originate calls.      Easy to guess/search for address -
                                        hence a password is recommended.
 
Multi/Priority targetting.              Sub-host addressing is awkward with
                                        current implementation, a separate
                                        NUN is required for each sub-host.
                                        A version of XCOM code will soon be
                                        available to overcome this limitation.
 
By addressing 31249.... instead of      DNIC 3124 can only be used from those
31069.... access may be limited to      countries served by FTCC.   In the
particular international links.         event of an outage at FTCC, backup 
                                        facilities may be limited and access
                                        temporarily lost from affected links.
                                        Links may serve more than one network.
 
Note:  For X.25  hosts,  be sure   to  coordinate  the  change of called
       address with the  host. If it is difficult for the host to change
       its software to accept a new called address it may be possible to
       perform address modification at the host interface to overcome this.
 
Example:   For an  X.25 host  a  patch,   (bchen:33)caddr.301 for version
3.01, may  be included  which  will ALWAYS  substitute the called address
in an  IIX call for the  address specified in the CLDADR statement. Other
patches,  capable  of more   elaborate   manipulation,  may be available,
contact HQTECH XCOM group or ITS for information.
 
OPTION (B) - Using a Random Network User Number with password protection.
-------------------------------------------------------------------------
Request  a random NUN  (from  ENS.DTE) for  the x.121 address, ask for it
to be red-listed (un-published) and have a password.
 

 
TYMNET - ExNet Tech Spt                                Page - 04
 
Addresses of this format are of form...(note 99 after the DNIC)..
 
3106+99+<####>     where:  3106  = Tymnet's DNIC
                            99   = 'identifier' digits
                          <####> = four to eight (4-8) 'random' digits.
 
Example: For host 127,  31069985326   (NUN=9985326)
 
Note:   If sub-host addressing   is required  then specify in the request
to ENS.DTE exactly  what  the  last   1-4   digits   should  be for every
sub-host.
 
Advantages and disadvantages of option (b)
------------------------------------------
Same as  for option  (a)  except that  with the  host number no longer in
the address,  problem   diagnosis   may be   slowed: the  price for being
cryptic.
 
 
OPTION (C) - The 'user friendly' approach.
------------------------------------------
From a foreign  network,  users   are able  to enter  a called address of
"31069"  to direct their  call  to Tymnet.  The Tymnet supervisor prompts
them to  "please log  in: " just  as on  Tymnet. If a class 1, 2, 4 or 24
username is entered,   the   supervisor    will   allow   the  call to be
completed.  By default  ALL    'domestic'  usernames  have class 4 access
and may be used to log in from other networks.
 
If you wish to  control  which     usernames  may  be  used  from foreign
networks,  give  'domestic' usernames  only class 35 access and 'foreign'
usernames  only class  1 access.  A class 1  username MUST originate from
an X.75  gateway  host  whereas  a  class  35  username  is EXCLUDED from
gateway access.   This  provides    good    security,   standard 'log in'
procedures and user friendly network responses.
 
Advantages                              Related disadvantages
----------                              ---------------------
Easy for async/3270 hosts that          More difficult for host 
need to 'key-off' usernames for         to originate calls.
sub-host/application routing.
 
User friendly network responses.
 
Username/password combination is
difficult to 'crack'.  Tymnet host
is able to change password for user.
 
Log in procedures are very similar
for both domestic and foreign users.
 

 
TYMNET - ExNet Tech Spt                                Page - 05
 
 
CONCLUSION
----------
A requirement  for 'external   network'  access is by no means synonymous
with  a host needing  to be in group  19, with all the attendant security
implications.  Choosing  one of the alternative options, or a combination
of (c) with  (a)  or (b),  can   offer   effective   protection and other
benefits.
 
If you  have questions   regarding gateway  access or the content of this
document, please  contact   External      Network    Services  by calling
(703)356-6995 or OnTyme ENS.ITS.
 
-
----------------------------------------------------------------
----------------------------------------------------------------
Received: from EMSTYM.Ontyme.Tymnet by B39.Tymnet; Mon, 7 Apr 86 18:02:19 UT
From: ENS.J/OLIVETO@Ontyme.Tymnet 
Date: 07 APR 86 06:59:19 
To: TYM.W/BARTLETT@Ontyme.Tymnet 
Cc: ENS.NS@Ontyme.Tymnet, TYM.W/BARTLETT@Ontyme.Tymnet,
	ENS.J/OLIVETO@Ontyme.Tymnet 
Message-id: I45496@Ontyme.Tymnet 
Subject: "Wayne; Below is the Ontyme I sent"... 

Wayne;

     Below is the Ontyme I sent 15 minutes after our phone conversation
on 28 Mar 86.  I sent you a copy then.  If you feel additional information
is required let me know.

- Jeff -

========================  M E M O R A N D U M  ========================
 
          TYMNET - McDonnell Douglas Network Systems Company
        External Network Services [ExNet]  -  Technical Support
     Phone: 703/356-6995    Telex: (23) 82693    Drop: 310600024915
 
DATE>      28 Mar 86  17:15
 
TO>        AllSales/AllTech
 
COPIES>    Wayne Bartlett
           Gordon Smith
 
FROM>      Jeff Oliveto
 
SUBJECT>   Async Outdial Access from X.75 Locations
 
-----------------------------------------------------------------------
 
      This is to clarify  Ontyme I41508 dtd 28 March by Wayne Bartlett.
All Async  Outdial  hosts have  been  removed from Group 19 as of 3/28.
This  is after several  incidents of misuse by hackers. As my Ontyme of
13 March stated, group 19 offers no security from hackers.
 
     For any  customer  that  wishes  secure  gateway access to private
async outdial ports there are two recommended meathods:
 
1)  Send  a called  address of  31069. At  the gateway "please log in:"
prompt enter a domestic username with a password.
 
2)  For  host originated  calls, a prefered  meathod might be obtaining
an NUN,  (an X.121  address that begins with a 9 such as 3106901234). A
password  is STRONGLY   recommended. For  host originated calls that do
not want  to be prompted  for  a password,  the password can be entered
into  the Call User  Data  field of the  Call Request packet if it does
not exceed  12  characters. If the  customer insists on no password, at
least  we can quickly add  one once they get their thousand dollar plus
phone bill for traffic that they did not generate.
 

 
TYMNET - ExNet Tech Spt                                Page - 02
 
     Please refer to (TECHREF:56)SECURE.DOC for additional  information
on secure  gateway  access.   For   those  that  have  been effected by
their removal  from  group 19,  contact ExNet Tech Spt at 703/356-6995.
We will see  that you  get  reinstated  with a 9  type X.121 address as
soon as  possible.  Again,  this  only  effects  async outdial hosts in
Group 19.
 
     Tymnet is  unique  in the   level  of   security it  can offer its
customers. Let's use it as a selling tool.
 
- Jeff -
----------------------------------------------------------------
 Y ô