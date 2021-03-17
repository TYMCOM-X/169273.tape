From: Carl A Baltrunas <CARL@930.Tymnet> 
Date: Tue, 16 Apr 85 3:47:12 PST 
To: wrs 
Subject: TUMS 0n system 33 

TUMS or some parts of it seem to be sick on system 33.
I keep seeing  (MAIL) running MAILER ^C when I login.  /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 13 May 85 23:45:26 PDT 
To: wrs, Carl A Baltrunas <carl@C930.Tymnet, Osman Guven <osman@C930.Tymnet,
	Joe Smith <jms@C930.Tymnet 
Cc: Craig Fletcher <fletcherc@C930.Tymnet 
Subject: testing *BUG* testing 

TUMS has recursion? and angle-bracket problems!
Bill, Look at the names in the "To:" field!

I have myself defined as "Carl A Baltrunas  <Carl@X930.Tymnet>"
and each of Joe, Osman, and Craig similarly setup.  I have
a group {Monitor "Carl,Osman,Joe,CC:Craig"}.  I sent this message
to  WRS,MONITOR  and the final-closing ">" angle-bracket is missing
for each name in the "To:" and "CC:" fields.   Also, If this is
sent to  MONITOR,WRS  then instead of WRS being first, he is added
to the end of the "CC:" list as if I had said:
     To: Carl,Osman,Joe,CC:Craig,CC:Wrs
Apparently "," doesn't reset out of the "CC:" field.  {Sigh}!
Received: from C39.Tymnet by C930.Tymnet; Tue, 14 May 85 0:57:08 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 14 May 85 0:50:04 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet 
Subject: Re: testing *BUG* testing 
In-reply-to: your message of Mon, 13 May 85 23:45:26 PDT

The CC: takes effect until a matching ";" as per RFC-821 (obscure).  As
for the missing ">" in the list, I am puzzled and will look into it.  I
take it you haven't seen the ">" missing in any other cases?  -Bill
Received: from C39.Tymnet by C930.Tymnet; Tue, 14 May 85 1:13:37 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 14 May 85 1:06:42 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet 
Cc: Osman Guven <osman@C930.Tymnet, Joe Smith <jms@C930.Tymnet, Craig Fletcher
	<fletcherc@C930.Tymnet 
Subject: Re: testing *BUG* testing 
In-reply-to: your message of Mon, 13 May 85 23:45:26 PDT

I've changed a comparison in mailib from "<" to "leq" which should correct
the missing ">" problem.  Regarding the "CC:" - it should be paired with a
";" to close the group as per RFC-821.  It doesn't matter much, except in
aliases.  -Bill

PS - please test it again after the telecopies settle down (ie all *.SAV
created after 5/14)
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 16 May 85 1:05:56 PDT 
To: wrs 
Subject: Shame on you!!!! 

Shame on you... for reading the system number out of CNFTBL or it's GETTAB
equivalent!!!!!  Isn't there a SYSID or SYSNO/SYSNUM gettab that returns
a number?  I only noticed this 'cause I hacked the CONFIG msg on 930 for
about 20 minutes one afternoon to say X20 so that I could run MAKJOB on 930!
Well, Due to some mailer error, I just got a message that was recieved way
back on the 22nd of April, saying it was received on system 20... maybe it
created 930.Q to pass it along???? HMMMM....
/Carl
Received: from C39.Tymnet by C930.Tymnet; Sat, 18 May 85 13:31:35 PDT
Return-path: <WRS@C39.Tymnet 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 16 May 85 12:34:07 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet 
Subject: Re: Shame on you!!!! 
In-reply-to: your message of Thu, 16 May 85 1:05:56 PDT

CONFIG is the only place with the "site code" prefix.  I suppose I could
get the letters from CONFIG and the number from SYSNUM or whatever its
called, but that would be wierd.  Anyway, this leaves the options open,
you coud have a host name there before the "=" in CONFIG and MAIL would
work fine.
-Bill
From: Cherie Marinelli <VUE@X930.Tymnet> 
Date: Fri, 17 May 85 17:22:58 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Washer & Dryer 


Love,

Why shouldn't I use them yet; is something wrong?

-Mee
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 17 May 85 17:30:37 PDT 
To: Cherie Marinelli <VUE@X930.Tymnet> 
Subject: Re: Washer & Dryer 
In-reply-to: your message of Fri, 17 May 85 17:22:58 PDT

It's not that's something's wrong... it's more that after all
the work put into making sure that they're setup, I'd like to
be there when they FIRST get used and to make certain that it
is all setup right...

PS:  I'll try to be home early today... I'm starving... didn't
     eat anything yet today...  sigh!  Got to bank, took out
     $100... have to go to Los Gatos to sign the signature cards...
/Carl
From: Joe Smith <JMS@C930.Tymnet> (Liberty Street in Fremont) 
Date: Tue, 21 May 85 11:46:53 PDT 
To: carl 
Subject: (CARL)LOGON does not like 159:04:21 

Osman asked me "How long has system 55 been up?"  Suspecting a trick question,
I checked it with both LOGON and SYSTAT.  Sure enough, LOGON=36:50:15 and
SYSTAT=195:54:36, a difference of 159:04:21.  This number, when expressed as
ticks, multiplied by 1000 and then divided by 60 to get milliseconds is equal
to 2^35=Integer Overflow.  FOROTS has the same problem.  You need to divide
by 60 first, look at the magnitude of the uptime, and don't bother with
milliseconds if it's more than 1 hour.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 21 May 85 12:38:22 PDT 
To: Osman Guven <osman@C930.Tymnet>, jms 
Subject: Uptime... 

Well,
  I changed it from IMULI ac,^D1000 IDIVI ac,^D60  to IDIVI ac,^D60 then
IMULI ac,^D1000 and it seems to make absolutely no difference in the result.
I didn't check to see if it was overflowing... it may just be the routine
in SCAN that I'm calling!!!
/Carl;
Received: from C39.Tymnet by C930.Tymnet; Wed, 22 May 85 14:19:43 PDT
Return-path: <WRS@C39.Tymnet 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 22 May 85 14:18:02 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: (IMPORT) username 
In-reply-to: your message of Sun, 12 May 85 21:32:07 PDT

Sorry for the delay.  I had forgotten about that stuff.  I don't
remember why the ignore colon, better change the password just in case.
I've copied those files someplace more useful and will delete them as
soon as the copy completes.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 22 May 85 15:05:40 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: (IMPORT) username 
In-reply-to: your message of Wed, 22 May 85 14:18:02 PDT

(IMPORT) changed to "New Industry" via MUDDY.
PS. Going through normal channels, SRA hath said only one username per
    person, so I've changed TJB ==> SALLY for Joe's wife, and WILKES ==>
    CHERIE for my new wife (3 weeks and a couple of days now).  I wanted
    you to know, in case Becky wanted to send either of them mail.......
/Carl
From: Joe Smith <JMS@C930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 24 May 85 0:43:17 PDT 
To: CARL 
Subject: Position sensitive switches in LOGON 

Why does /STATUS have to come after /NOTIFY ?  I tried putting the switch
on the first LOGON line of (SALLY)SWITCH.INI, and it did not work.

Which switches to LOGON are position sensitive?
From: Joe Smith <JMS@C930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 24 May 85 0:43:17 PDT 
To: CARL 
Subject: Position sensitive switches in LOGON 

Why does /STATUS have to come after /NOTIFY ?  I tried putting the switch
on the first LOGON line of (SALLY)SWITCH.INI, and it did not work.

Which switches to LOGON are position sensitive?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 24 May 85 14:27:57 PDT 
To: Craig Fletcher <fletcherc@C930.Tymnet> 
Subject: Perp Replacement Status 

I've been writing up "informal" notes on the perp replacement project
and keeping them in (CARL:930).  The following files are of interest:

PRPRPL.STS  - Status, milestone schedule
PRPRPL.QUE  - Queue format, necessary info
PRPRPL.*    - Miscellaneous notes...

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 24 May 85 17:20:24 PDT 
To: Bill Soley <wrs@C39.Tymnet> 
Subject: Try this.... 


    .R DDT			; no license necessary, doesn't matter
    0[  1,,0			; put 1,,0 into ac0
    FRMOP$X			; xct FRMOP 0,0  ac=1,,0 block=CF.LOG,,0
it should SKIP
    0[  1000nnn			; where "nnn" it the new job number
    ^C				; out of DDT
    .SYSTAT nnn			; systat shows running LOGINN TI
    .AT nnn			; attach... and try to do anything

Let me know what you find out.........
/Carl
Received: from C39.Tymnet by C930.Tymnet; Sat, 25 May 85 20:55:58 PDT
Return-path: <WRS@C39.Tymnet 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sat, 25 May 85 20:50:29 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: performance and 4meg 
In-reply-to: your message of Mon, 20 May 85 13:57:25 PDT

Thanks - The trailing ">" bug has been fixed.  Let me know if you 
continue to see it.  -Bill
From: VUE@C930.Tymnet 
Date: Wed, 29 May 85 16:55:20 PDT 
To: carl 
Subject: Yawnnnnnnn...........zzzzzzzzzzzzzzzzz 


Hi. I'm home. I'm tired. I'm gonna take a nap now so I'll have some engerny
for our lesson tonight. Okay? I'm not going to stay logged in, either. I
just wanted to let you know. I love you.....-Mee
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 31 May 85 17:16:38 PDT 
To: ct.techserv@ontyme.tymnet 
Cc: txs.sup@ontyme.tymnet, txs.d/coffey@ontyme.tymnet, iod.hq@ontyme.tymnet 
Subject: System 59, Accounting data fixup 

Peter,

  Verison %30(4) of CHKPNT should never have been distributed to any
system, let alone any of your 2020s.  It is in an interim state and
someone probably copied it from system 930 accidently.

  In any case, I have a program which condenses the huge files created
by version %30(4) to their proper size.  I took the liberty of running
my program on system 59 prior to sending this message, so that you could
run your accounting for MAY without any further holdup.

  I also concatenated the "good" .DAT and .SAT files with the condensed
files and replaced your .DAT and .SAT files with the rebuilt files.  The
files concerned were:

    (SYS)052885.DAT condensed                 replaced (SYS)052885.DAT
    (SYS)052985.BAD condensed (SYS)052985.DAT replaced (SYS)052985.DAT
    (UN1)052885.BAD condensed                 replaced (UN1)052885.SAT
    (UN1)052985.BAD condensed (UN1)052985.SAT replaced (UN1)052985.SAT

  With these "rebuilt" files, you should NOT lose ANY accounting
information for the 28 May or the 29 May.  The .BAD files contained
all the accounting information as usual.  CHKPNT %30(4) mistakenly
wrote 256 empty block between each good data block.

  Sorry for the mixup.  We will continue to investigate how the wrong
CHKPNT was installed.

                                        Sincerely...  /Carl
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Sun, 2 Jun 85 10:56:12 PDT 
To: carl 
Subject: Host 59 CHKPNT cleanup 

You do good work!  Thanks for cleaning up the mess, for letting Peter know, and
for copying me on your message to Peter.

See you Monday,
?d.
/D.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 3 Jun 85 1:56:06 PDT 
To: Carl A Baltrunas <carl@C930.Tymnet>, Osman Guven <osman@C930.Tymnet>, Joe
	Smith <jms@C930.Tymnet> 
Cc: Craig Fletcher <fletcherc@C930.Tymnet> 
Subject: (CRASH)CRSHID.YAM, CRSTAB.MAY, CRSTAB.ALL 

Crash table data has been run using the corrected data files CRSHID.YAM
and CRSHID.ALL which includes the MAY totals.  UUOCON:INSSIM+2 changed
to ERRCON:ILLINS+2 and enpty module names filled in.

Note:  Osman, do we care for our records whether we call it INSSIM+2 or
       ILLINS+2 ???  since INSSIM+2 is really only valid on an F3?  I've
       tried to keep it ILLINS+2...

Anyway, I guess we can bring the results to the tuesday meeting.  In case
anyone has noticed, 6 ILLINS+2 crashes last month, NONE are the zero-UPT
type crashes as best I can tell from the PC contents.  Can we determine
from looking at what is coded in MUR whether there were considered software?
or hardware?
/Carl
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Mon, 3 Jun 85 14:50:03 PDT 
To: carl 
Subject: TRW's host 33 on TRWNET. 

	They have a public network username, TRWNET.  It had been on
host 32.  It was moved to host 54, about one month ago.  They use the
network host for storing backup copies of files from their host 33.
Last week, when trying to TELECOPY a file from their host 33 to username
TRWNET on public network host 54, they received an unprecidented error
message that concerned them:  "MUST COPY FROM OR TO SYSTEM 123".

	TELECOPY on both public network hosts appears the same (same
checksum).  Do you know of some file or process which might have existed
on host 32 or somewhere esle in the public network which translated
reference to their host 33 to our host 123?  It appears that this had
previously been done, somehow.

	An alternative would be to convince them that Tymnet host 123
is, in fact, their TRWNET host 33 or the access to it.  They are
difficult to mollify, I understand from the A.C., so restoration of the
original automatic translation might be the preferable solution to their
problem.  Your recommendations would be welcome.

Thanks!
Dennis
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 4 Jun 85 12:08:30 PDT 
To: dencoff 
Subject: TRWNET 

I doubt very much if there is a problem with TELECOPY.  I'd more than
expect that something else is wrong (unless their copy went bad?).
Host 123 must be something on their network, since 123 is (was) a
supervisor on our net...

The procedure they should use is to use the GATEWAY command to get to
our network and then to one of our hosts.  The host number used in the
GATEWAY command is the host that they should be using to send/receive
data files, and 33 is the host they are sending/receiving from on their
network.

If this continues to be a problem, then I suggest that you get a username
and a password from the TRW A.C. so that we may login to their system and
try to copy stuff.  Please find out what monitor TRW is running, so we
can determine whether we need OPER privilages or not.  If they are at-least
running /J we are OK. (I think).  Anyway, as part of your packaging, are
we going to be sending TRW updates?
  I'd like them to have a /N10 monitor, or at least a /N with the right
fixes patched.  This requires a new LOGOUT as of /K.... etc.
/Carl
Received: from C897.Tymnet by C930.Tymnet; Tue, 4 Jun 85 13:29:02 PDT
Return-path: <JMS@C930.Tymnet> 
From: Joe Smith <JMS@C930.Tymnet> (Liberty Street in Fremont) 
Date: Tue, 4 Jun 85 13:26:27 PDT 
To: CARL@C930 
Subject: LOGON.SHR copied to 897 

I copied (CARL)LOGON to 897.  It looks like there are important files in
(WILKES:897), such as DSKCLK.MAC.

--JOE

Received: from C39.Tymnet by C930.Tymnet; Tue, 4 Jun 85 19:21:32 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 4 Jun 85 19:19:06 PDT 
To: carl 
Subject: lunch 

I think we have a lunch meeting sched for tomorrow.  I will be up late
working on the new base, and I have a meeting at 14:00.  I suggest that
we begin promptly at 11:30 so we will be assured of having at least 2
hours to talk.  If this seems to restrictive, we can meet another day.
My schedule will be somewhat freer in 2-3 weeks.  Probably don't want
to wait that long.  -Bill
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Wed, 5 Jun 85 16:03:42 PDT 
To: carl 
Subject: Host 930 

In trying to get copies of the  old TXSSUP files, to review them for
missing information, I have not yet been able to rpint one whole file
on the 350 terminal without getting interrupted by "OUTPUT DEVICE ERROR".
This is becoming a pain!  I only see this error message on 930; in fact,
I've never seen it from another host in the 4 1/2+ years I've been using
the Tymcom-X hosts.

Am I doing something wrong, or is there something I can do to avoid this
error?

Thanks!
/D.
Received: From EMSTXS.Ontyme.Tymnet by C930.Tymnet; Tue, 4 Jun 85 22:43:19 UT
From: ISGHQ.POLICY@Ontyme.Tymnet 
Date: 04 JUN 85 21:44:58 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: J99604@Ontyme.Tymnet 
Subject: ORGANIZATION OF MCDONNELL DOUGLAS FIELD SERVICE 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Mon, 10 Jun 85 14:31:59 PDT
Resent-To: Dennis Coffey <dencoff@X930.Tyment>, Bruce Ring <bring@X930.Tyment>,
	Dan Baigent <baigent@X930.Tymnet>, Helge Mortensen
	<helgem@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>

           MCDONNELL DOUGLAS        ADMINISTRATIVE    NO:   ISG-85-78
     INFORMATION SYSTEMS GROUP        BULLETIN        DATE: 30 MAY 85



     To:           ISG LIST A - E (Including MDAIS)

     Subject:      ORGANIZATION OF MCDONNELL DOUGLAS FIELD SERVICE 
                   COMPANY

     Recently, the ISG Executive Board approved integration of three 
     (3) large field service organizations.  They were:

         .    The former Microdata North American Field Service 
              Division;

         .    The former Tymshare Computer Systems & Support Division;

         .    The former Tymnet Network Field Engineering Division.

     This new resource group, McDonnell Douglas Field Service Company, 
     has a synergistic purpose.  It will:

         .    Enable improved ISG customer and business unit 
              satisfaction from a more effective post-sale support 
              function.

         .    Better use its considerable aggregate of geographical 
              distribution, service capability and managerial talent.

         .    Contribute to a stronger partnership with ISG 
              Engineering, Manufacturing and Sales to better ensure 
              shared success.

     Following a very carefully developed set of operating principles, 
     an architectural structure and management staff have been selected 
     for the Field Service Company.

     By utilizing participative management methods, there is an 
     exciting potential for growth and success for the Field Service 
     organization.

     The following appointments have been made and are effective 
     immediately:

     WARREN CAGGIANO - Director of Sales
                       _________________

         Warren was the r Manager of Sales & Marketing of Computer 
         Support Services Division.  He has been with Tymshare MDC for 
         eight (8) years, during which time he has played an 
         instrumental role in the growth of Tymshare's third party 
         maintenance organization (CSS).

     VERN HART - Vice President of Technical Operations (Acting)
                 _______________________________________________

         Interviews are scheduled; we expect to fill this position in 
         the very near future.


     HANS KINTSCH - Controller and Director of Financial Administration
                    ___________________________________________________

         Prior to joining MDFSC, Hans was the Vice President of Finance 
         for the Braegen Corporation.  Previously Hans held a number of 
         posts with Cal Comp.  He received both his B.S. degree and his 
         Masters in Business Administration from the University of 
         California, Irvine.

     JOHN LORTS - Director of Material and Logistics
                  __________________________________

         John came to McDonnell Douglas in September, 1984, from Texas 
         Instruments.  At Texas Instruments, he spend thirteen (13) 
         years in various Field Service Management positions.  These 
         positions included teaching, managing Technical Support, 
         Regional Field Management, Marketing/Merchandising and Program 
         Management.

     BOB MARTINEZ - Director of Planning and Marketing
                    __________________________________

         Bob was the former Director of Staff Resources and Security 
         for Tymshare.  He also held positions of Facility Manager, 
         Marketing & Customer Support on behalf of Stanford Research 
         Institute and was National Director for Computer Management 
         Services.  At Computer Management Services, Bob was 
         responsible for Video, Computer Based, Human Resource & 
         Management Development Training.

     CURT MILLER - Vice President of Field Operations
                   __________________________________

         Curt was the former Vice President of Network Field 
         Engineering.  He has been in the Field Service business for 
         twenty-five (25 years).  Before working with Tymshare, he 
         worked for IBM, Burroughs, G. E. and NCR.

     JOHN SWARBRICK - Vice President - Quality
                      ________________________

         John joined Tymshare in January, 1973, as Manager of Computer 
         Maintenance.  He has held positions as Sales Manager, Depot 
         Manager and General Manager of Computer Systems & Support.  
         His most recent position was that of Vice President of 
         Computer Support Services (CSS).

     Within the next few weeks, there will be further realigning of the 
     respective organizations to ensure that the McDonnell Douglas 
     Field Service Company can perform its mission of:

              Providing quality, end-to-end, cost effective support for 
              MDC customers and business units with growth and profit 
              ability consistent with user satisfaction.



     (Original signed by Vern H. Hart)

     Vern H. Hart
     President
     MCDONNELL DOUGLAS FIELD SERVICE COMPANY
Received: From EMSTXS.Ontyme.Tymnet by C930.Tymnet; Mon, 10 Jun 85 19:18:08 UT
From: TYMNFE.SUP@Ontyme.Tymnet 
Date: 10 JUN 85 19:10:56 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M32155@Ontyme.Tymnet 
Subject: ORGANIZATION OF MDFSC--FIELD OPERATIONS DIVISION 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Mon, 10 Jun 85 14:34:34 PDT
Resent-To: Carl A Baltrunas <carl@X930.Tymnet>

               MCDONNELL DOUGLAS FIELD SERVICE COMPANY 
     
                     FIELD OPERATIONS DIVISION    
     
     
     
     
DATE:    JUNE 8, 1985    
     
     
     
TO:    ISG MANAGEMENT LIST A-E (INCLUDING MDAIS)  
       FIELD OPERATIONS EMPLOYEES  
     
FROM:  CURT MILLER, VICE PRESIDENT 
       FIELD OPERATIONS, MDFSC
     
SUBJ:  ORGANIZATION OF MDFSC--FIELD OPERATIONS DIVISION
     
-------------------------------------------------------------------------- 
     
     
FOLLOWING A SERIES OF MEETINGS WITH THE NATIONAL/REGIONAL/OPERATIONS/ 
ADMINISTRATIVE MANAGERS OF THE THREE (3) FIELD SERVICE ORGANIZATIONS  
OF MICRODATA, COMPUTER SYSTEMS AND SUPPORT, AND NETWORK FIELD ENGINEERING, 
AN ORGANIZATIONAL STRUCTURE WAS FORMULATED WHICH BEST UTILIZES THE    
TALENTS,  ENERGIES AND NEEDS OF THE FIELD OPERATIONS DIVISION.   
     
I AM CONFIDENT IN THE ABILITIES OF ALL OUR FIELD OPERATIONS EMPLOYEES 
TO MAKE THIS DIVISION ONE OF THE STRONGEST ASSETS OF THE FIELD SERVICE
COMPANY.  THERE ARE CONSIDERABLE CHALLENGES AHEAD, BUT WITH EACH 
INDIVIDUAL CONTRIBUTING HIS/HER BEST, WE WILL NOT ONLY MEET THE OVERALL    
COMPANY GOALS, BUT EACH INDIVIDUAL WILL BE PROVIDED WITH AN OPPORTUNITY    
TO GROW WITH THE COMPANY.
     
THE NEW ORGANIZATION IS EFFECTIVE MONDAY, JUNE 10, 1985, AND IS AS    
FOLLOWS:  
     
     
EASTERN FIELD OPERATIONS------Thomas (Tom) J. Brozena, Director  
     
   Tom has been the Eastern Regional Manager for Tymshare   
   Computer Systems and Support (CSS) since 1984.  Prior    
   to that, he was a Federal Marketing Representative for   
   Tymshare and TRW after a 22 year career in the data 
   processing area of the United States Marine Corps.  
   Tom will continue to be based out of the Washington, D.C. area.    
   The new location of the Eastern Field Operations Headquarters is:  
     
                 EASTERN FIELD OPERATIONS    
                 10001 GEORGE PALMER HIGHWAY 
                 SUITE 131    
                 LANHAM, MD 20706  
                 (301) 459-8363    
                 ONTYME: TCMS.SUP (request ontyme 
                 forwarding to Tom on the THS. system) 
     
   As Director of Eastern Field Operations, Tom will oversee
   the Field Service activities in the following states:    
     
        Maine                       Connecticut   
        New Hampshire               Rhode Island  
        Vermont                     Delaware 
        New York                    Maryland 
        Pennsylvania                West Virginia 
        Massachusettes              Virginia 
        North Carolina              South Carolina
        New Jersey  
     
     
     
CENTRAL FIELD OPERATIONS------Ronald (Ron) P. Dvorsky, Director  
     
   Ron has been in the Field Service business for 20 years, most 
   recently as the Eastern Region Field Operations Director for  
   the former Microdata Corporation.  Ron will be based out of   
   Altanta, Georgia, the new location of the Central Field Operations 
   Headquarters:    
     
                 CENTRAL FIELD OPERATIONS    
                 6195 BARFIELD ROAD, N.E.    
                 SUITE 170    
                 ATLANTA, GA 30328 
                 (404) 252-8065    
                 ONTYME: MICROD.R/DVORSKY    
     
   The Central Field Operations area of responsibility will be:  
     
        North Dakota                Missouri 
        South Dakota                Arkansas 
        Nebraska                    Louisiana
        Oklahoma                    Wisconsin
        Kansas                      Illinois 
        Texas                       Michigan 
        Minnesota                   Indiana  
        Iowa                        Ohio
        Kentucky                    Tennesse 
        Mississippi                 Alabama  
        Georgia                     Florida  
        Puerto Rico 
     
     
     
WESTERN FIELD OPERATIONS------Gene R. Huffman, Director
     
   Gene was the former Microdata Central Region Field Operations 
   Director and has had 26 years experience in the Field Service 
   business.  Gene is currently residing in Oakbrook, Illinois,  
   but will be relocating shortly to California.  Until such time
   as the new Western Field Operations Headquarters location is  
   finalized, Gene will direct the Western Area from his current 
   location:   
     
                 WESTERN FIELD OPERATIONS    
                 (temporary location)   
                 2809 BUTTERFIELD ROAD  
                 SUITE 175    
                 OAK BROOK, IL 60521    
                 (312) 920-8050    
                 ONTYME: MICROD.G/HUFFMAN    
     
Gene will be responsible for the Field Service activities in the 
following states:   
     
        Alaska                      Idaho    
        Hawaii                      Nevada   
        Washington                  Arizona  
        Oregon                      Utah
        California                  New Mexico    
        Montana                     Wyoming  
        Colorado    
     
     
     
FIELD SUPPORT-----------------Jerry H. McCoy, Director 
     
   Jerry has been with the former Tymshare organization for 6    
   years, most recently as National Manager, Computer Systems    
   and Support.  He has a solid background in Field Service 
   and Management which will be utilized in a liaison role  
   to both internal and external customers.  Jerry will also
   be responsible for the MDC internal account, new product 
   introduction, data center management, and overall field  
   support.    
     
   Jerry will continue to be located in the Bay Area at:    
     
                 FIELD SUPPORT
                 39100 LIBERTY STREET   
                 FREMONT, CA 94538 
                 (415) 794-2525    
                 ONTYME: TCMS.J/MCCOY   
     
     
     
FIELD TELECOMMUNICATION SUPPORT--Patrick (Pat) A. Diamond, Manager    
     
   Pat has been with the former Tymnet organization for
   approximately 5 years, most recently as the National
   Field Manager.  He has considerable telecommunication    
   experience and has been on the forefront of several 
   advanced communication projects.  Pat is responsible
   for integrating the telecommunication/network technology 
   into the Field Operations Division.  
     
   Pat will be located in the Bay Area, and can be reached  
   currently at:    
     
                 FIELD TELECOMMUNICATION SUPPORT  
                 10161 BUBB ROAD   
                 CUPERTINO, CA 95014    
                 (408) 446-7676    
                 ONTYME: TYMNFE.DIAMOND 
     
     
     
OPERATIONS SUPPORT-------------Marlene M. Ford, Manager
     
   Marlene has been with the former Microdata Field Service 
   Group for 4 years, most recently as the Customer Service 
   Operations Manager.  She has significantly contributed   
   to the outstanding success of the Microdata Central 
   Dispatch function and overall high operational quality   
   of the Field Service group.  In addition to her current  
   position, Marlene is also the Vice President of the 
   North American Field Service Managers, Orange County
   Chapter.    
     
   The Operations Group will be headquartered in Irvine,    
   California, and have responsibility for central dispatch,
   call screening, and remote operations centers.  Marlene  
   can be reached as follows: 
     
                 OPERATIONS SUPPORT
                 2361 MCGAW AVENUE 
                 P.O. BOX 19501    
                 IRVINE, CA 92713  
                 (714) 250-1000, EXT. 7145   
                 ONTYME: MICROD.M/FORD  
     
     
ADMINISTRATION----------------Patti J. McDonald, Manager    
     
   Patti has been with the former Tymnet Field Engineering  
   organization for the last 3 years.  She has been in 
   administrative management positions for 8 years and 
   brings a high degree or organizational ability to the    
   Field Operations Division.  Patti will be responsible    
   for coordinating all administration, training, and  
   personnel functions, as well as providing Field inter-   
   face to finance and computer system resources. 
     
   Patti will be located in the Bay Area, and currently
   can be reached at:    
     
                 ADMINISTRATION    
                 10161 BUBB ROAD   
                 CUPERTINO, CA 95014    
                 (408) 446-6763    
                 ONTYME: TYMNFE.P/MCDONALD
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 14 Jun 85 21:48:13 PDT 
To: carl 
Subject: Dinner at 6:15 

The memo I got says it starts at 6:15pm.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 14 Jun 85 21:55:12 PDT 
To: carl 
Subject: PROBE:300 no longer valid on TSN. 

Oh, no!  Now I can't get to MARKET (and this little piggy went ...).
Whatever shall I do?
From: Cherie Marinelli <Cheryl@X930.Tymnet> 
Date: Thu, 13 Jun 85 20:37:08 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Company 

I think er're gonna be having some on Saturday.....do you think you can see
your way clear to come home soon and (...sigh...) clean up the office?
I really want the house to look nice for them (it's Debbie and her soon-
to-be-fiance) since she's my best friend and I want her to be impressed...
please? I'll even take care of the laundry you haven't gotten to, if you
will please straighten up the office...please...? I know I'm probably
nagging you, but thois is really important to me...please????
ILYVM,EIYAS,ADYFI!!! - Mee
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 8 May 85 17:12:47 PDT 
To: Osman Guven <osman@C930.Tymnet> 
Subject: system 22 base 

Osman,
  I talked with a few people... it seems that the base for 22 is really
ordered for 67 (although it doesn't matter what host) and is setup for
2 201 cards, [Ed Roop thinks each card handles 32 lines], which means it
has 64 ports.  He is going to check tomorrow to see if they can upgrade
it to handle more ports, then will get back to us to cut a monitor for
it with the proper number of ports.

  It is node # 155.  The base for system 34 actually has more ports than
64, but we've only cut 34 for that many.  We will have to do some little
experimentation to find out what RPORTN to cut for a few of these hosts,
but for now.... we wait.
/Carl

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 12 May 85 21:32:07 PDT 
To: wrs 
Subject: (IMPORT) username 

Bill,
  Can you tell me what's happenning with username IMPORT, in as much as
I've been using it on 930 and elsewhere as storage for imported products
for TYMCOM-X and it seems you have a program someone setup on system 29
to do something special with a TBA program of some sort...

  Enlighten me?

  PS. I'm turning off the ignore-colon option in the mud so that I can
      store things on 26 under this username. ?Objections?  Let me know.

/Carl
From: Osman Guven <osman@C930.Tymnet> 
Date: Fri, 17 May 85 10:28:31 PDT 
To: Carl A Baltrunas <carl@C930.Tymnet>, jms 
Subject: C22../N10 

System C22 now has a working /N10 monitor and up with it.

Osman..
Received: from C39.Tymnet by C930.Tymnet; Sat, 25 May 85 20:59:50 PDT
Return-path: <WRS@C39.Tymnet 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sat, 25 May 85 20:58:32 PDT 
To: carl 
Subject: 930's copy of (MAIL)39.SAV 

(MAIL:930)39.SAV somehow got sick.  Its checksum was bad, but its
creation date and error bits were okay.  I installed a new copy and
all is okay now.  Your mail to 39 has been dalayed as a result.  The
bad copy would "ADDRESS CHECK..." when it was run.  -Bill
Received: from C39.Tymnet by C930.Tymnet; Sat, 25 May 85 21:05:11 PDT
Received: from C930.Tymnet by C39.Tymnet; Sat, 25 May 85 21:03:01 PDT
Return-path: <Carl@X930.Tymnet 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 20 Apr 85 0:59:17 PST 
To: Carl A Baltrunas <carl@C930.Tymnet>, Osman Guven <osman@C930.Tymnet>, Joe
	Smith <jms@C930.Tymnet> 
Cc: Craig Fletcher <fletcherc@C930.Tymnet> 
Subject: New directory (NOBACKUP) 

As a space-saving-reserving measure, I've created directory (NOBACKUP)
which will hold files which don't need to be backed up, most of which
are being kept around to reserve disk space for when we need to do a
full monitor transmittal (I see one coming soon!)...

Please, under pain of listening to a long tirade of verbal abuse, do NOT
delete any of these files.

I've put last year's accounting files, the transmitted /N *.SAV files,
last year's chkpnt data files, the (OSP)*.SAV files for updated/rebuilt
/N monitors that we have distributed to new and reconfigured systems.

Don't put anything  there without telling me what it is, and don't
delete any of it.  These files will NOT appear on the all-files tapes
and WILL make the bi-weekly backup go faster.
/Carl
From: SYSMAINT@C930.Tymnet 
Date: Fri, 31 May 85 10:14:49 PDT 
To: carl 
Subject: packages: BOOTS, UNDELE 

     
CARL,  
     I cannot find the source of BOOTS.SAV.  It exists in 2 versions...
ver. 1 is on all the 10s and 930.  V. 17 is on 897.  How do you want
them distributed?
     
     Does BOOTS.DMP have a source(s)?  I'm not sure of its origins.

     Lastly, should UNDELE be changed in its source code to reflect a
change in its version, that is, from v.  1 to v.  2?  That would include
loading the new binary version on the systems.  (Both old and new
versions say version 1 now).  If the answer is yes, do you want a new
logfile etc.  in the archives, or just the updated source and binary?
     
     Thanks....(sysmaint:930) Michael 
From: Joe Smith <JMS@C930.Tymnet> (Liberty Street in Fremont) 
Date: Sat, 1 Jun 85 13:31:02 PDT 
To: carl 
Subject: I'm back (I'm dead). 

Details about DECUS when I become not-dead.  Story about CREJB. uuo was not
correct - 7.03 has CRCTX. to create context (ala PUSH command), but the only
way a new job is created is via sending characters to LOGIN - BATCON sends
a single long command to LOGIN via the PTY as before.

/JMS
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Thu, 6 Jun 85 12:31:27 PDT 
To: carl 
Subject: Accessing programs in packages 

We have determined that each program included in a package can--and
will--be accessable in the TRABAS data base.  When the information about
each package is inserted into the TRABAS data base, all information
about each componenet program will also be inserted into the data base.

The information about each program will be:  (1.)  system number (the
host on which the package FDM file is being assembled); (2.) software
name (the original name of the program, as named in previous
transmittals); (3.) version number (the ver. number of the current
version of the program); (4.) date (the date of the entry of the package
and programs into the TRABAS data base); (5.) programmer name (the name
of the programmer who wrote the current version of the program; and (6.)
FDM name (the file name of the FDM file containing the package).

Doing a LOOKUP in (QASYS:38)TRABAS will provide the FDM file-id, which
can be used with a LOOKUP in (QASYS:38)ARCHIV to have the needed file
restored from Archives.  the Package FDM file will contain a
documentation file which will list a description of the contents of the
FDM file, plus an FDM file for each program contained in the package.
To extract a particular file from the archived package FDM, you must
open the package FDM file, WRITE to disk the contained program FDM file,
then open the program's FDM file and write the individual file(s) to
disk.
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Thu, 6 Jun 85 13:38:36 PDT 
To: acs.h/spencer@ontyme, txssup 
Subject: Software Upgrades for Mallinkrodt. 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Mon, 10 Jun 85 14:34:25 PDT
Resent-To: Carl A Baltrunas <carl@X930.Tymnet>



			       MEMORANDUM

                                                       TYMCOM-X SUPPORT

DATE:	June 6, 1985

  TO:	Hank Spencer

FROM:	Michael Chinn

SUBJ:	Packages

-----------------------------------------------------------------------

                     SOFTWARE PACKAGES OVERVIEW

     The following represents a listing of packages and the programs
that are included in the packages.  This listing represents the initial
phase of the updates, which tries to include all the monitor utility
programs.  Later phases will update other groups of programs, such as
the language compilers.  This initial phase includes about one-third of
the programs which will be checked and updated.  We plan to finish in 2
or 3 more months with this first phase.  As we finish a particular
package, we will contact you to inform you of our intentions and the
effects of the upgrade, and to allow you to inform us of any objections
by Mallinkrodt to the upgrades, before we contact the Software
Transmittal Group to install a particular package on their system.  (I
realize this may mean a steady trickle of changes, but the alternative
might be a flood.  I believe and hope that the improvements in security,
efficiency, and supportability make this a worthwhile effort.)

     Disclaimer:  This list is not exhaustive, and there will probably
be many changes as we work our way through the list.  But we will
let you know of any changes.
     
     If there are any questions or information we can supply, please
call us or send an OnTyme.  Thanks for your help....Michael Chinn

Packaging Group:
Dennis Coffey (415) 794-2588 OnTyme: TXS.D/COFFEY
Dan Baigent (415) 794-2564   OnTyme: TXS.D/BAIGENT
Michael Chinn (415) 794-2583 OnTyme: (via):Dennis Coffey

------------------------------------------------------------------------

Package name:  INCLUDED PROGRAMS
Purpose of programs in package.
--------------------------------

Operating System: MONITOR
the heart of the operating system.

Monitor called procedures: KMCLOD, BOOTS
Installs a copy of the monitor, 'boots' the system and saves some crash
information.

Load Complex: MAGWRT, LP20FS, KSFEFS
Load monitor from tape, control interface to network.

Startup:  INITIA, COPYCR
Startup program, program to save crash information.

Session:  LOGINN, GFD, ACCESS
Logging in, changing directories, controlling access from other
directories.

Accounting:  CHKPNT
Gathers and writes accounting information.

Access:  NONAME, PROJECT, LPASSW, LVALID
Setting parameters on use of directory, changing user password,
validating users.

License:  OPER, SETLIC, SETE
Setting and changing Operator-type license.

Master license:  SETOP, OPPS
Providing user with access to license, allowing user to change license
password.

Disk/tape:  TITO
Tape input-output program.

Operator tools:  TAKEIT, HANGUP, NODLOD, RELOAD
Used by operators to take down systems, reload nodes..

Information:  SYSTAT, ICP, DSKMAP, ERRCHK
Provide systems performance/load information to users.

Commands:  LISTER, LOADER, RPG
Handling of commands like LOAD,PRINT,COMPILE, RUN, MAIL

File operations:  DIRIT, FILCOM, UNDELE, PIP
Handling of commands like DIRECTORY, DIFFER, COPY
 
***end memo***
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Thu, 6 Jun 85 14:08:34 PDT 
To: acs.h/spencer@ontyme, txssup 
Subject: Software upgrades for Mallinkrodt 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Mon, 10 Jun 85 14:34:27 PDT
Resent-To: Carl A Baltrunas <carl@X930.Tymnet>



			       MEMORANDUM

                                                       TYMCOM-X SUPPORT

DATE:	June 6, 1985

  TO:	Hank Spencer

FROM:	Michael Chinn

SUBJ:	Packages

-----------------------------------------------------------------------

	In order to provide better support of the operating system,
languages and utilities for systems running Tymcom-X, we are currently
organizing our software into groups of related programs called "packages".
These packages are designed to integrate the most current versions of 
each of their component programs, so that the installation of the package
will insure each host is kept up to date with new software releases.

	On 14-June-85 the current release of the FILOPS package
(version 001) will be distributed to all Tymcom-X systems.  This package
includes the following programs:
  
--------------------------------------------------------------------------
Program Name: DIRIT  
Current Version:  33
New Version:  33.1

Comments:  Minor code change to delete a bug.  No visible change to any
users.  Has been on at least one other system for 3 years without
problems.
------------------------------------------------------------------------
Program Name:  UNDELE

Comments:  We would like to install this program and provide the
documentation which is currently available to other users.  This program
has been used in its two versions for more than 3 years (second version
released Mar 1984).

     I am enclosing a copy of the file UNDELE.DOC which is the file
which explains the use of this command, and is installed in the system
to provide user documentation.  The user may also type the /HELP switch while
using the command UNDELETE to receive information on possible options.
-------
                           "UNDELETING" FILES

     The UNDELETE command will restore a file that has been deleted
during the current login or gfd session, if the UNDELETE ability has
been enabled. 

     The general form of the UNDELETE command is

   -UNDELETE [file identifier1,file identifier2...] [switches]

file identifier    Specify the file(s) to be restored.  The usual wild
                   cards can be used (#, *, "ALL").

     If no file identifier is specified, all files deleted during the
present login or gfd session are restored.  

     When UNDELETE is enabled, all files that are deleted can be
recovered until the user either logs out or gfd's to another directory.
UNDELETE is enabled by giving the command

   -UNDELETE/ENABLE

and disabled by the command

   -UNDELETE/DISABLE

Both enabling and disabling remain in effect until they are changed,
and do not need to be typed at the beginning of each session.

     The UNDELETE command switches are:

  Switch                        Function

/LIST               List all files that have been deleted.

/RECOVER            Recover specified files, or all.  (default)

/PURGE              Expunge specified files; they cannot then be restored.

/SELECT             Select action to take for a list of files,
                    or rename a single specified file.

/ENABLE             Enable UNDELETE capability.

/DISABLE            Disable UNDELETE capability.

/BEGIN              Enter UNDELETE program.

/FAST               List only filename, extension and success message.

/?                  Type the on-line help message.


     If a user attempts to recover a file when another file by the
same name exists, UNDELETE will give the error message 

    "**ALREADY EXISTING FILE!**"

If this occurs the user can then use the /SELECT switch to rename the
file.  UNDELETE will prompt for the new name for the file.

     /SELECT can be used with a list of files to select the action to
take for each file.  UNDELETE will prompt for the action to take.
Responses, and their results are:

  Response               Action

  Control-R          Recover this file with the old name.
   <CR>              Skip this file, do not recover.
  Control-D          Expunge this file; it cannot then be recovered.
  name Control-R     Recover this file and rename it to "name".
  name <CR>          Recover this file and rename it to "name".
***end of undele.doc***
-----------------------------------------------------------------------

Program Name:  CHKPNT
Current Version: 30.2
New Version:  30.3
Comments:  A fairly minor change which insures that accounting data
will be written even when the "debug" switch is set.  This could prevent 
the loss of revenue information in the event of a crash.  There
would be no apparent changes to users.

Changes between versions:  The following is the programmer's infor-
mation log which records the changes between versions.

30.3    /TODD  10-21-82: Previous versions of CHKPNT did not write 
        records because debug switch was on.  Fixed this so that we 
        can charge for interrupted sessions.
------------------------------------------------------------------------

	Please note that earlier versions of the above programs will no
longer be supported.  If you have any questions or problems regarding
the distribution or installation of this package, please contact the
indicated member of the Packaging group listed below.

Thank you.

		
Contacts:
           Packaging Group ( + contact for this package)
           Dennis Coffey (415) 794-2588, OnTyme: TXS.D/COFFEY
           Dan Baigent   (415) 794-2564, OnTyme: TXS.D/BAIGENT 
        +  Michael Chinn (415) 794-2583, OnTyme (via) : TXS.D/COFFEY

From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Thu, 6 Jun 85 8:48:59 PDT 
To: sysmaint, baigent 
Subject: Packaging change (simplification) 
Resent-To: Carl A Baltrunas <carl@X930.Tymnet>

	First item:
Craig agrees that we do the following to save unnecessary redundancy,
especially when transmitting software to the domestic, customer 2020's.

We check the current copy of the executable binary file of the program
on the "production location hosts" (as they are called on the
transmittal form) to determine if they are identical with the executable
binary file created by our reconstruction of the file from the archived
source files.  This checking is done in this way:
	1.)  Determine the version number of the file built from the
sources and of the files on the production hosts.
	2.)  Determine the checksum of the file built from the sources
and of the files on the production hosts.
	3.)  If the production host's file has the same version number
and checksum as the file built from sources, telecopy this executable
binary file from the production host to another file name on the host
with the executable binary file rebuilt from sources.  Check for any
differences between the two files with the 'differences' command (which
runs the FILCOM program).

If the executable binary file from the production host is identical to
the executable binary file built from sources, transmittal of the
executable binary file to the production host is unnecessary.  This
transmittal should definitely NOT be done if the host is a customer host
(such as a domestic 2020).

	Second item:
The names of the packages that Craig suggested for the transmittal data
base, TRABAS, (and thus for everyone's access to the packages in
archives) is a 6 character mnemonic name, such as the filename part of
the file-id in the .DOC file in the archive FDM file, with ".PKG" as the
last four of the 10 characters allowed in the TRABAS and ARCHIV data
bases.  E.g.: the COMMAND package will be called "COMMAN.PKG" on the
"Software name" line of the transmittal form.

Since this name will be the name by which the package will be known
henceforth, we should be careful about what we choose for the first six
characters.  Coordination of this naming is recommended if you have any
questions.

	Third item:

Re. retransmittals of previously released packages:  If the package has
been transmitted, the "production" hosts' software has been installed,
and no changes to these installed files are to be made, then the new
transmittal should be marked "Archive only".

Every package transmitted should be re-transmitted with the following
changes, and these changes should be implemented for every new package
transmitted.:
	1.)  Package name should follow the new convention of
"<6-character name>.PKG".
	2.)  Version number(s) for packages should reflect the new
convention, including the manner in which the FDM file's name is
affected by the version numbering scheme.  Package file name format:

	"<4-char pkg name abbrev.>n[A|T].mmm"
(where:  n = digit 0 thru 9, reflecting the host class;
	 "A" or "T" = code reflecting Archive or Transmittal file; and
	 mmm = package version number, starting with 001.)

	3.)  The transmittals should include the following additional
information about EACH included program and instructions to Software
Distribution to enter this information into the TRABAS data base:  
	    a.)  System number:  packaging host number;
	    b.)  Program name:  name of individual program, as in last
transmittal of individual program;
	    c.)  Transmittal number:  (numbering method to be
determined); 
	    d.)  Version number:  version number of the individual
program;
	    e.)  Programmer name:  name of programmer who wrote the
current version of the individual program;
	    f.)  File name:  name of the package's current archive FDM
file; 
	    g.)  File extension:  extension of the package's current
archive FDM file.

If you have any questions, please ask,
Dennis
Received: From EMSTXS.Ontyme.Tymnet by C930.Tymnet; Mon, 17 Jun 85 22:08:32 UT
From: FIN.S/AIKEN@Ontyme.Tymnet 
Date: 17 JUN 85 14:16:48 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.C/BALTRUNAS@Ontyme.Tymnet, FIN.V/STAFFORD@Ontyme.Tymnet,
	FIN.S/AIKEN@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: A53678@Ontyme.Tymnet 
Subject: HOST 95 

TO:    CARL BALTRUNAS
FROM:  STEPHANIE AIKEN

CC:    VIDA STAFFORD

RE:    HOST 95


CARL,

VIDA HAS ADDED HOST 95 TO GROUP 1 PER YOUR REQUEST.

THANK YOU
STEPHANIE
Received: from C39.Tymnet by C930.Tymnet; Mon, 17 Jun 85 22:47:29 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 17 Jun 85 22:24:45 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Deletion of (SYS)mmddyy.DAT Jan-Mar 
In-reply-to: your message of Mon, 17 Jun 85 12:48:25 PDT

I realize how frustrating vanishing files can be.  Frustration that
often leads people to say things that are unwarrented, out of character,
and usually regreted later (I'll stop at that or I'll regret it later).
Anyway, I've not deleted any files not in my directory on 930 in the
recent past.  Certainly if I were to delete files in another directory,
it would only be ones I was somehow responsible for.  If nobody has
admitted it, its probably because however did it was (1) unaware they
did it, or is (2) unaware you want to know who did it.

-Bill

PS - I can't say for sure, but I don't even remember deleting any files
from my directory on 930 last week, but that's not the kind of thing
that makes a lasting impression in my mind.  I also don't have time to
check it now.

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Jun 85 0:35:53 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: Deletion of (SYS)mmddyy.DAT Jan-Mar 
In-reply-to: your message of Mon, 17 Jun 85 22:24:45 PDT

I didn't want to sound like I was blaming you... I wanted merely to
elicit a response.  Thanks for your co-operation.

Subject:  Monitor changes.
As per agreement from our entire group, we would like to ONLY make those
changes which will enable us to provide STOPCD messages and better CTY
messages (understandable by operations) for approx. 3 weeks.  Thus, I'd
like you to comment on changes you'd like to see, but restrain yourself
from making any for thr duration.
  We'd like to release /P with the above-mentioned changes around the
beginning of the month, assuming all goes well.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Jun 85 1:32:41 PDT 
To: Mail Wizard <MAIL> 
Subject: TUMS-10 bug report (v34 of 04-JUN-85) 

I said "LO #30" to see my last 30 messages... fingers missed...
should have been "LI #30"... Anyway, the "LOGOUT" command should
complain that there's extra junk on the line... since I didn't
want to LOGOUT anyway... when this happens, the user probably
made a mistake!!!!
    (as I did)
/Carl
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 19 Jun 85 1:27:20 PDT 
To: carl 
Subject: Re: "... or even ''suspected'' of deleting ..." 

You forgot the Swastica!  Really, Carl, that's not the way we do
things in this country, or this company.

Anyway, I have a fog in the back of my head about somebody, I think
Greg or Lee, writting something to cleanup (SYS).DAT and (UN1).SAT
and some other things automatically.  It was intended for KS's, but
maybe its being run, manually or otherwise, on 930.  I don't have any
idea its name or really if it exists or was just a wish.

-Bill
Received: from C39.Tymnet by C930.Tymnet; Wed, 19 Jun 85 1:48:27 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 19 Jun 85 1:43:22 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: TUMS-10 bug report (v34 of 04-JUN-85) 
In-reply-to: your message of Wed, 19 Jun 85 1:32:41 PDT

Well, actually LOGOUT does complain, but after it logs you out.
Probably not too swift.  I'll make that a special case.  Thanks
for the suggestion.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Jun 85 1:51:11 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: "... or even ''suspected'' of deleting ..." 
In-reply-to: your message of Wed, 19 Jun 85 1:27:20 PDT

Grrrrrrr.  You realize that I was "just a little" ticked...  anyway...

                        ========*       #
                                #       #
                                #       #
                        *=======@=======*
                        #       #
                        #       #
                        #       *========

I think this is the right direction....   Sorry to have pushed your
buttons.  I doubt that "any" automatic things is working, 'cause it
never deleted them before.... anyway, incident forgotten 'till next
time.     /Carl

PS. Thanks for the cheerful humor!
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 21 Jun 85 17:39:33 PDT 
To: LINDLEY@C36.Tymnet 
Bcc: CARL@X930.Tymnet 
Subject: Re: NODPRI 
In-reply-to: your message of Fri, 21 Jun 85 17:23:35 PDT

Hmmmm... Well, I gave it to Dennis Coffey... I donno why it's not done.
I'll forward your message to him....  /Carl
Received: from C39.Tymnet by X930.Tymnet; Mon, 24 Jun 85 12:59:42 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 24 Jun 85 12:59:15 PDT 
To: carl 
Subject: (XEXEC)BS 

New hack in (XEXEC) will do a binary search of text files.  Look at
(XEXEC)BS.DOC for more information.  Good for phone book, etc.  -Bill

(pass this on to the group, please)
From: Cherie Marinelli <Cherie@X930.Tymnet> 
Date: Mon, 1 Jul 85 18:44:42 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: food 


Hi, babe!

Did you, by any chance, eat the last of the hamburger(s)? Just womndering.
I went to the fridge to get one, and they were ALL GONE!
Again, I'm sorry aobut your record...when I got homw from work, I gave
myself twenty lashes with the hose, Okay?
I'd kinda like to go skating tonight...(yes, I realise how hot it is
outside, but I really would like to go, and we need to pr
actice. 
I soaked the gardens (flower and veget
bvle) and the house, and the dog, and
me, just to cool us all off...Come home soon, okay? ILYVMADYFI! -W.W.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Mon, 1 Jul 85 22:12:51 UT
From: MFG.A/FARLEY@Ontyme.Tymnet 
Date: 01 JUL 85 22:54:01 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M40697@Ontyme.Tymnet 
Subject: CENTREX & TIELINE ACCESS 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Wed, 3 Jul 85 12:37:56 PDT
Resent-To: Carl A Baltrunas <carl@X930.Tymnet>

 
 
DATE:  JULY 1, 1985
 
  TO:  THE WORLD
 
FROM:  ANITA FARLEY
 
SUBJ:  CENTREX & TIELINE ACCESS
_______________________________________________________________________
 
It has come to my attention that a number of you did not receive the
memo which preceded the 2450 North First Street phone listing which
explained how to dial into and out of the phone system.  Following is
a copy of the information:
 
 
************************************************************************
*                                                                      *
*                     TELEPHONE SYSTEM ACCESS                          *
*                                                                      *
*                     2450 NORTH FIRST STREET                          *
*                                                                      *
************************************************************************
 
 
Dialling From               Dialling To               Process
-------------               -----------               -------
 
Centrex                     Fremont                   1) Dial 116
                                                        (dial tone)
                                                      2) Dial Station #
 
Centrex                     Orchard Parkway           1) Dial 117
                                                        (dial tone)
                                                      2) Dial Station #
 
Centrex                     2450 N. 1st St            1) Call Orch Pkwy
                                                        (117 tieline)
                                                      2) Dial 84
                                                        (dial tone)
                                                      3) Dial Station #
 
Centrex                     TYMNET Training           1) Call Orch Pkwy
                            2665 N. 1st St              (117 tieline)
                                                      2) Dial 82
                                                        (dial tone)
                                                      3) Dial Station #
 
Orchard Parkway             Centrex                   1) Dial 80
                                                        (dial tone)
                                                      2) Dial Station #
 
Orchard Parkway             Fremont                   1) Call Centrex
                                                        (80 tieline)
                                                      2) Call Fremont
                                                        (116 tieline)
                                                      3) Dial Station #
 
North 1st St                Orchard Parkway           1) Dial 101
2450 & 2665                                             (dial tone)
                                                      2) Dial Station #
 
North 1st St                Centrex                   1) Call Orch Pkwy
2450 & 2665                                             (101 tieline)
                                                      2) Call Centrex
                                                        (80 tieline)
                                                      2) Dial Station #
 
North 1st St                Fremont                   1) Call Orch Pkwy
2450 & 2665                                             (101 tieline)
                                                      2) Call Centrex
                                                        (80 tieline)
                                                      3) Call Fremont
                                                        (116 tieline)
                                                      4) Dial Station #
 
 
***********************************************************************
 
Centrex - all phones on Bubb Road and Valley Green Drive
Fremont - self-explanatory
Orchard Parkway - TYMNET 2710 Orchard Parkway
North 1st St - TYMNET Training & Education 2665 N. 1st St
               AND - MFG, DSC 2450 N. 1st St
 
* N O T E *
 
2450 N. 1st Street does not have direct dial capability.  If you do not
use a tieline, you must dial the main number 408/435-0200 and ask for
the extension.  Dialling the number directly will only get you a busy
signal.
 
Orchard Parkway tieline to Centrex is no longer 180.  It is now 80.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 20 Jun 85 14:33:00 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: addendum to deleted (SYS) files 

I took WF away from a lot of OPER names... canceled a few that don't
need to be on 930.  Most of the NTD names are still intact.  Feel free
to check.

This action was done because (SYS)*.REL were among the deleted files.
We can live with missing .DAT files but not .REL and others.  I doubt
that it was intentional... someone slipped?  Well, it's harder to slip.
/Carl
Received: from C39.Tymnet by X930.Tymnet; Thu, 20 Jun 85 15:08:36 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 20 Jun 85 14:58:34 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: addendum to deleted (SYS) files 
In-reply-to: your message of Thu, 20 Jun 85 14:33:00 PDT

If .REL files vanished from (SYS), that implies "stupid".  This would
tend to indicate somebody that doesn't know better.  I don't see any
reason why any NTD people that you don't know need names at all on 930.
I don't see any reason why anyone outside Tymcom-X Systems family and
friends need WF on 930.  That is to say, feel free to cancel all but
SY RF from anyone in NTD -- and even that if you think its warrented.
I don't even see why anyone but
  Dennis, Bill Euske, Gazel, Mike Rude, Greg, myself, Adele, Lois,
  Jill, and Nancy
even need user names on 930.  I might be able to help identify user
names if you have any you're not sure of.
-Bill
(if nothing else, you get UFD space back, te he he)
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 20 Jun 85 16:53:15 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: addendum to deleted (SYS) files 
In-reply-to: your message of Thu, 20 Jun 85 14:58:34 PDT

Yes.... what I did was for exactly what you just said.  I was merely
letting you know that I did it.  I think I cancelled OPER names for
Euske & RUDE...
   People having names are Nancy,Lois,Jill,Lynn,Greg,You,Dennis,Ishan
from NTD.  All others are gone.  If you think Euske or Rude really
need usernames or OPER status, feel free to put them back.
/Carl
From: Cherie Marinelli <Cherie@X930.Tymnet> 
Date: Tue, 9 Jul 85 16:53:25 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: sk8ing 


Hi, Dollface!

How would you feel about possibly not going skating tonoc? I have alot of
homework to do, and my ribs really don't feel like having much fun...does
that make any sense to you? Perhaps  you coulld explain it to me...
Call me when you finish reading this, okay? ILY....YFI! -Mee
From: Cherie Marinelli <Cherie@X930.Tymnet> 
Date: Thu, 11 Jul 85 16:47:23 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Hi! 


I'm home. ...have 
 slight headache....going to lie down fore awhile.....let
me know if there is anything in particular you want me to make you for supper,
okay? 
           Hogs and quiches.....Mee
From: Cherie Marinelli <Cherie@X930.Tymnet> 
Date: Thu, 18 Jul 85 0:02:53 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: "ilyvmadyfi.....i sent this in"... 

ilyvmadyfi.....i sent this in lower case because i wanted to tell you, but i k
you have a headache, and i didn't want to make it worse; i just wanted you to know. please come to bed a.s.a.p., because i sleep better when i know you're there,  and i can snuggle up to you. okay? -Mee.
Received: from C26.Tymnet by X930.Tymnet; Wed, 24 Jul 85 14:52:35 PDT
Return-path: <DENCOFF@26.Tymnet> 
From: Dennis Coffey <DENCOFF@26.Tymnet> 
Date: Wed, 24 Jul 85 14:39:38 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Cc: Jon Mosser <MOSSERJ@X930.Tymnet> 
Subject: PEAK for users 
Resent-From: Carl A Baltrunas <Carl@X930.Tymnet>
Resent-Date: Fri, 26 Jul 85 2:42:27 PDT
Resent-To: ken

I have had a request from Peter Stratman, CEGI-Tymshare, for PEAK to be
made available for customer use.  I have checked with Jon, who tells me
to do it, with the explicit provision that we will provide no support.

The only work I see necessary to carry this out is:  1.)  remove the
test for the Tymshare bit in the program, 2.)  the creation of a pricing
schedule, and 3.)  transmittal of the files to the CEGI hosts.  the only
files that I know to be needed by the program and users are:
(SYSNEWS) PEAK.CHT, PEAK.KEY, PEAK.NEW, PEAK.SUM, PEAK.TXT, PEAK.TUT and
(SYS) PEAK.SAV, PEAK.HLP

Please let me know of anything I've missed or any problems we might
encounter.  Please copy Jon on your answer.

Thanks!
/D.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 24 Jul 85 14:54:00 PDT 
To: Dennis Coffey <DENCOFF@26.Tymnet> 
Cc: Jon Mosser <MOSSERJ@X930.Tymnet> 
Subject: Re: PEAK for users 
In-reply-to: your message of Wed, 24 Jul 85 14:39:38 PDT
Resent-From: Carl A Baltrunas <Carl@X930.Tymnet>
Resent-Date: Fri, 26 Jul 85 2:42:29 PDT
Resent-To: ken

The provision of "no support" sounds nice on paper, but it doesn't hold
water for a product like PEAK.

I'd like to know the following:
   1)  Does CEGI use the Tymshare bit for anything?
       i.e. do any of their programs check for it?
   2)  Do they restrict their customers in any way?
   3)  Do their customers have regular TYMNET access, i.e. TYMNET usernames
       or are they all "only" on the 2020's ((and their 1 KL)).

Depending upon the answers to these questions, HOW to release PEAK to their
users will change.

An aside:  Since Ken Dawson is working for TYMNET again, (as of last wednesday)
           there may be some development work on PEAK in the forseeable future
           and quite possibly, he could make the necessary changes for CEGI for
           us.

One more question for CEGI: How many customer groups do they have?  GAN's...
   PEAK has an internal list of customers who may use it, and depending upon
   their needs, that list could be augmented, or made external, so that CEGI
   could restrict or allow usage of PEAK on a customer by customer basis.

Let me know what you find out.
/Carl

PS:  Until it is decided that PEAK is error-free enough for general customer
     use, I'm against removing the TYMSHARE-BIT restriction.  Especially if
     we're not supporting it!!!!!
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 29 Jul 85 22:59:27 PDT 
To: Cherie Marinelli <Cherie@X930.Tymnet>, Carl A Baltrunas <Carl@X930.Tymnet>,
	Osman Guven <osman@X930.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: IT'S UP /N22 

I DONNO WHY, BUT THE PROBLEM SEEMS TO HAVE BEEN IN CTYSIM.
CAVEAT: ALL THE POPJ'S TO JRST'S ARE IN [NOW] AND THE JSR => JRST
        IS IN.  THE ONLY CHANGES TAKEN OUT WERE THE EXTRAA FEW
        CHARACTERS ADDED TO THE ^Z? MESSAGE AND THE CHECK FOR
        LOWER-CASE H TO RUN HAANGUP.  NOW IT RUNS!!!!!!! AND
        IT RUNS APPARENTLY FINE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

BEWARE OF INOCUOUS LITTLE CHANGES!!!   /CARL
From: Cherie Marinelli <Cherie@X930.Tymnet> 
Date: Tue, 30 Jul 85 18:51:19 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: <<sigh>> 


Ne soyez pas agite avec moi, s'il vous plait. Je n'etais pas essayer a vous
renderer agite, et que vous etes maintenant, je le regrette. Mais je ren-
contes tres clairement aux fois ancienes, que vous m'avez informe que vous
n'aime pas le patinage aussi bien que moi....et, si c'est vrai, qu'est-ce que vvous voudriez que je ferai? Mais, c'est encore chaqu'un a son gout, n'est-ce
pas, et je ne voudrais pas que vous restez irrite avec moi. JTTBENVLO, EB?
     -Moii
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 13 Aug 85 20:34:10 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, wrs, ken 
Subject: (M33:930) => (M33) (OSU) 

In an attempt to clean up and organize things... M33 is being *split* into
two separate directories.  The monitor and things like MONED will remain
in directory (m33).. {includes MDDT,SYMAUX,???DDT,etc.}
  Things which are monitor utilities such as LOGINN,LOGOUT,TAKEIT,etc. are
all moved to directory (OSU).  This is an evolutionary project and will
take a little time for the dust to settle... so, please be patient...

  **** IF YOU HAVE (M33) IN YOU "DO-LIST", PLEASE BEWARE ****
	*** you might just add OSU to your list? ***
	*** This affects things with special-command-mode!!! ***

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 13 Aug 85 22:00:20 PDT 
To: Cherie Marinelli <Cherie@X930.Tymnet> 
Subject: I'm coming home... 

...I've done my time...
......so, tie a yellow ribbon  'round that old oak tree ...

sigh!
From: KEN@X930.Tymnet 
Date: Wed, 14 Aug 85 17:26:48 PDT 
To: carl 
Subject: peak v2.0(320) 

I tried it on a rainbow in 132 mode
it works
/ken
/--/?aa
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 14 Aug 85 22:43:01 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, William R Soley <wrs@C39.Tymnet> 
Subject: KL-10 Fixed for JSP 

After some searching... I found the problems with KL halted, JSP!
Joe modified the sources to cover this and rebuilt the monitors
with CPUS.CTL.  I ran MONED1 to update things to /N24 ...

I updated both the running monitors and (SYS)NEWMON on 35, 36, 39 & 55
to reflect the changes [/N23-1] of PUSHJ P,SAVE4 ==> JSP T4,SAVE4 at
REDSBC+1 and a POPJ P, at CVPPC both in ERRCON.

The test, using S$INFO,,CPOPJ1 did not seem to work completely since it
still caused the system to go to boots.  CUPC OPS is inter-office mailing
the CTY output to me.

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 18 Aug 85 19:06:08 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: INFO stopcodes 

Hmmmm.  Seems we should put in the code that we discussed:
  If it is NOT a stopcode which is going to take the system down... this
includes JOB,INFO and EVENT stopcodes ...then we setup code which duplicates
the ITMINT code and we make sure that the key continues to be refreshed.
This means "possibly" saving the contents of the key...and restoring it,
but I think we don't need to do that.
/Carl
From: Carl A Baltrunas <CARL@930.Tymnet> 
Date: Tue, 9 Apr 85 23:07:35 PST 
To: mosserj 
Cc: fletcherc, jms, dencoff 
Subject: Ref: F40 LINK 

...
  From: MOSSERJ, APR 9, 1985   14:31
  Carl,
  Did Craig give you that software change request from Dan Wedge
  pertaining to f40 link ?? If so, Any status?
  jon
...
Jon,
  I don't recall receiving any requests from Craig.  Dennis mentioned
something to me about Fortran-10 & Link (I think), but I'm pretty sure
that this is the first I heard about F40 & Link.  My understanding is
that the 3rd phase of the F40 compilation is completed by special codes
in LOADER, and I do not know if LINK-10 handles that properly.  That is
the main reason why LINK-10 has not ever been considered as an official
replacement for LOADER.
  This entire issue needs further examination, and I will need some
clarification of what Dan Wedge is requesting and what he really needs.
/Carl
From: Joe Smith <JMS@C930.Tymnet> 
Date: Fri, 12 Apr 85 16:21:51 PST 
To: OSMAN, CARL 
Subject: Determining CPU and Monitor type. 

The subroutines are in (SPL)CPUTYP.MAC and (SPL)CPUTYP.SAV
/JOE
Received: From EMSTXS.Ontyme.Tymnet by C930.Tymnet; Wed, 24 Apr 85 23:06:02 UT
From: IOD.HQ@Ontyme.Tymnet 
Date: 24 APR 85 14:47:28  
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.D/BAIGENT@Ontyme.Tymnet, CPC.G/PEREZ@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, CT.TECHSERV@Ontyme.Tymnet,
	CT.TECHSERV@Ontyme.Tymnet, CT.TECHSERV@Ontyme.Tymnet,
	CT.TECHSERV@Ontyme.Tymnet, IOD.HQ@Ontyme.Tymnet, IOD.HQ@Ontyme.Tymnet,
	TXS.@Ontyme.Tymnet 
Message-id: A05205@Ontyme.Tymnet 
Subject: Host P90 accounting related to PERP 

           M E M O R A N D U M
 
DATE>      1985-04-24  22:30 GMT
 
TO>        Dan Baigent (TXS.D/BAIGENT)
           Gary Perez (CPC.G/PEREZ)
 
COPIES>    Carl Baltrunas (TXS.C/BALTRUNAS)  Eric Ja"y (CT.TECHSERV)
           Richard Dumas (CT.TECHSERV)       Peter Stratman (CT.TECHSERV)
           J.F. Guillou (CT.TECHSERV)        Peter Haas (IOD.HQ)
 
FROM>      Ken BeDell (IOD.HQ)
 
SUBJECT>   Host P90 accounting related to PERP
 
REFERENCE> attached Ontyme # A04612
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
     Dan, please note memo from Peter Stratman (Cegi Tymshare France),
     which follows on next page.
 
     I believe his analysis of the problem is quite accurate.  Originally,
     the base for host C14 was node 145 and this node was assigned a time
     zone equal to Central European time.  Apparantly, sometime in the
     recent past, the base node for C14 was changed to node 232, which has
     a U.S. (pacific) time zone.
 
     If I recall the workings of Perp this would cause the problem as
     indicated by Peter; do you concur?
 
     Please get back to me with your thoughts on this issue, it's obvious-
     ly very important due to the accounting ramifications.
 
     Note to Gary Perez: Do you have any idea when the base node changed
     for host C14?  According to my present sources node 145 is now the
     dual base for hosts 11 and 16; node 232 is the dual base for hosts 8
     and 14.  Do you know if this is correct?
 
     If this is true and Peter's analysis is correct, we must change the
     time zone of node 232 to Central European time.  Will this cause
     problems in other areas, such as for host 8?
 
     I look forward to a prompt response on this item.  Thanks and best
     regards.  Ken
 
                                        v
                                        |
                                        |
                                        |
                                        |
                                        V

MSG#:A04612
 IN#:  34162
  TO: HQ
FROM: CT       TECHSERV
SENT: 24 APR 85 07:35:34
READ: 24 APR 85 08:06:22
 
 
  -----------------------M E M O R A N D U M------------------------------
  CEGI - Tymshare                              ST-CLOUD   [33](1) 602.7012
  Technical division                           CT.TECHSERV
  ------------------------------------------------------------------------
 
  Date:      April 24th, 1985 - 5:33 p.m. CET
 
  To:        Ken BeDell (iod.hq)
 
  Copies:    Peter Haas (iod.hq)
             Richard Dumas (ct.techserv)
             Jean-Francois Guillou (ct.techserv)
             Eric Jay (ct.techserv)
 
  From:      P. Stratman
 
  Subject:   Errors in March accounting for host P90.
  ------------------------------------------------------------------------
 
  Ken,
 
  We have  found an important problem  concerning the accounting  for host
  P90. This problem has been detected in the accounting files for March on
  host 90, but may be present in the files for earlier months, and on some
  of the U.S. PDP X's.
 
  The problem is that part, or all,  of our customers' PERP and DEFER jobs
  have been accounted for using prime-time  product codes for TRU and con-
  nect  time.    This  has  resulted   in  enormous  differences   in  the
  prime/non-prime ratio for  customers that rely on heavy use  of PERP and
  DEFER.
 
  I have briefly  looked into this problem,  and it seems to  me that this
  may be  related to a change  in the base  node number for host  C14, the
  european customer PERP driver. If this new  number (232) is defined as a
  U.S. node number, this may explain our problem.
 
  I would like to have the following information :
 
  1.  Can you confirm  that this node number  change is the cause  of this
      problem ? If so, since when has the node number changed ?
 
  2.  Has this problem  occurred on U.S. hosts on which  our customers are
      valid ?
 
  3.  Will the accounting  for April be correct, and when  can the problem
      be corrected permanently ?
 
  As the end of the month approaches,  I would very much appreciate having
  a response as soon as possible.
  Thank you, Peter.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 7 May 85 22:42:14 PDT 
To: wrs 
Subject: PCOM suggestions 

Bill,
  I'm going to be making some major modifications to PCOM in the near
future, some of which may require monitor changes to the LOGIN/CREFRM
uuos, in order to make a PCOM which can control multiple streams at the
same time.  Granted, there are limitations... "X" number of interrupt
channels per stream (ORG,CHR,???), Only 48 total channels, with 2 per
stream minimum that's still only 24 streams...

  If you have any suggestions on how to "better" implement these things,
please let me know pretty soon.

  One possibility is to implement a SCNSER-like ring which is filled by
each frame in-process and the data extracted and written to various .LOG
files at normal non-interrupt level.  This would resolve worrying about
backpressure on PTYs (I hope).
/Carl
Received: from C39.Tymnet by C930.Tymnet; Wed, 8 May 85 2:31:42 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 8 May 85 1:52:14 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: PCOM suggestions 
In-reply-to: your message of Tue, 7 May 85 22:42:14 PDT

Put all interrupts of a like-kind at the same priority (same channel)
and use POLPRT (sigh).  You might want to consider using SAIL processes.
Actually deffered interrupts create a backpressure problem.  The immediate
interrupts keep happening, even if SAIL isn't processing the deffered ones
very fast (at all).  You could have a ring for each PTY that's filled at
immediate int level, and when its full, disable the interrupt.  If you woke
the background process whenever (1) yellow ball came in, (2) line feed came
in, (3) ring full; then the background process could handle characters in
reasonable amounts rather than every character.  If the background took a
character from a full ring, it would reenable the interrupt.  Since more
than one interrupt per channel, you can't mask the channel, have to reassign
the cause to channel 0.  

I assume this is PERP oriented.  May I suggest a Unix like scheme:  have
a directory protected to allow anyone to create a file in it.  Lets call it
(PERP).  To queue a job for later execution, you just put the command file
in (PERP).  The RIBAUT is the username used to run it under.  One of the first
lines might contain a command like
    :AT yy-mm-dd hh:mm:ss
which would cause the file to be ignored until then.  (PERP would remember
the time so it didn't have to look each time).  This command could have some
kind of date expression whenever that becomse feasable (I have a nice
recursive descent interpreter in TUMS you might be able to bastardize).

The file would need JL license to validate it since RIBAUT only needs
WF to be changed (or require JL to change RIBAUT like I always thought
it should be - this won't break anything except TITO which might already
have JL).  As I have learned with TUMS, avoiding a master control file
which must be multiply updated is a real good way to keep the system
simple, easy to maintain (no special tools needed), and very reliable
(robust) since the worst thing that could go wrong (within reason) can
only screw up one job.

-Bill
Received: from C39.Tymnet by C930.Tymnet; Fri, 10 May 85 16:31:55 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 10 May 85 16:00:54 PDT 
To: carl 
Subject: BAT blocks broken 

I'm getting complaints about bad spots that keep coming up.  897 (F3) is
being plagued right now by a couple that keep getting reallocated.  It
seems that the BAT block mechanism doesn't work.  There is code in the
monitor that is supposed to put bad blocks that it comes across into BAT.SYS
so they won't be reused.  Either it doesn't know how or doesn't care.  I
suggest this might be easy to fix if it were looked in to.  Its priority
is questionable since on KI,KL they can just copy the bad pack out.  On
systems like 897, 930 with fixed media, it is more serious.  An alternative
would be to take a program which scans the whole disk looking for bad blocks
and building BAT.SYS - I started to write such a tool and its beginnings are
in (WRS)RERR.
In fact there are several programs in various stages of completeness:
   (WRS:930)
	FNDBAD		scans all UFD's for bad files (ones marked bad)
	RERR		scans structure page by page looking for hard(soft) err
	DP		prints disp pages for all files in a directory
			and some nice statistics about storrage (try it!)
	DSTAT		similar to DP but will scan whole system - some of
			the statistics seem to be broken
	BAT		lists the contents of (SYS)BAT.SYS in meaningful form
These programs were never put in (MPL) since they were never finished and
tested.

-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 13 May 85 23:26:48 PDT 
To: Carl A Baltrunas <carl@C930.Tymnet>, Osman Guven <osman@C930.Tymnet>, Joe
	Smith <jms@C930.Tymnet 
Cc: Craig Fletcher <fletcherc@C930.Tymnet>, William R. Soley <wrs@C39.Tymnet 
Subject: PDP-10 Ports & Bases 

FYI: Dave Smith of NTD is interested in whether we can dispense with
     the host/base message telling the base how many ports we have on
     our particular host.  What problem is there in assuming that we
     have the maximum?  (This is apparently 256 due to 8-bit fields.)

  I've looked through sections of the monitor and cannot find any real
reason why we cannot assume 256 real ports.  The two places that I've
seen that restrict the monitor's port capacity are the sixbit device
name in the Device Data Block (DDB) which allows 3 (octal) digits and
the 9-bit port field in the HIBER uuo (Calli 72).  In either case, we
have from 0 to 777 (512 decimal) available, plenty for 256 physical
ports and 256 additional PTY ports (128 PTY pairs).

  If any of you think of any reason WHY we cannot assume 256 physical
ports for the new base interface, please let me know as soo as possible.

Thanks!
/Carl
From: Joe Smith <JMS@C930.Tymnet> 
Date: Mon, 13 May 85 23:50:32 PDT 
To: CARL 
Subject: User mode windowing. 

A good paging editor will not assign a fixed page number as the page
corresponding to the beginning of the current file.  That would cause
unnecessary page swapping when switching to the alternate file
(thrashing).  I want to be able to edit 2 files on asplit screen and be
able to bounce from one to the other quickly.  This is a bit more
complicated that what we discussing (your diagrams seemed to imply that
the part of the file that was in memory was in contiguous virtual
pages).  To obtain the most use out of a limited number if window pages
requires that the buffers may be using nonconsecutive virtual pages and
a least-recently-used algorthim for assigning them.  I.E., a user-mode
software page table.

I checked TOPS-10 FILDDT to see how it handles a single external address
space.  When it looks a disk file (EXE or not), it uses a pool of 20
pages for windowing, plus a 200 entry symbol-table cache.

SED can quickly switch to an alternate file using a single control
character command.  It recognizes when the alternate is the same as the
primary (but at at different line) and simply switches pointers to the
file in memory.  When switching to a different file, it writes out what
is in memory to the destination file (making a backup file), then reads
in the alternate file.  Therefore only one file is in memory at any
given time, and the entire file is in contiguous memory.  PEAK appears
to do something similar, but is a lot slower.

That is all.
/JOE
From: LOIS@C930.Tymnet 
Date: Wed, 15 May 85 10:18:29 PDT 
To: carl 
Subject: return of Lois 

Carl

Well, I am back at Tymshare.  Or, to be more accurate, Tymnet.  How amazing to
find that I still have a valid network name, and I remembered the password!
How nice to find that I have a peak.ini that I can copy when needed.

Hope everything went great in the east.  My best to you and Sherry.  Next
Tuesday I will be moving to VG2--right now I am in building H.

-Lois
P.S.  Remembering hfow to run this mail system also took a bit of thought.
Funny how the old patterns return.
From: Joe Smith <JMS@C930.Tymnet> 
Date: Wed, 15 May 85 20:23:24 PDT 
To: CARL 
Subject: MACRO53 bug 

WAYSYS is defind in UUOCON as 400, J as 2.
SWPMXL and SWPTBL are external, defined in COMMON, with SWPMXL=7000.
At NUMTAB+13 in UUOCON is the line
	XWD SWPMXL+WAYSYS,SWPTBL(J)

Old MACRO generates 000402* 000000* so that LINK will add 7000 to 402 and
sets the LH to 007402.

New MACRO generates 000002# 000000* and the POLISH tells LINK to add 7000
to 400, store 007400 in the LH of NUMTAB+13 and in the LH of location 2.

DEC fixed this problem by defining the entries in NUMTAB as:
	XWD SWPMXL+WAYSYS+J,SWPTBL

I found the problem by single stepping the store at SY.CHL, and saw that
it was changing 2,,5260 to 7400,,5260 but MDDT says it should be 7402,,5260.

/JOE

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 16 May 85 0:13:35 PDT 
To: LOIS@C930.Tymnet 
Subject: Re: return of Lois 
In-reply-to: your message of Wed, 15 May 85 10:18:29 PDT

Well well well...

  I guess I don't need to call you at Everest Solutions for lunch this
week after all.  Hmmm.  I hope that congratulations are in order for your
new position....  Give me a call some afternoon... soon?  Take care,
'cause you're my adopted west-coast mom, an' everyone knows moms have
to take care!!!
			/Carl

PS:  I'll be home most of the day on Thursday 'cause we're getting our
    new Maytag washer and dryer delivered sometime during the day.  The
    number there is 408-945-4321.     -Ciao!
Received: from C39.Tymnet by C930.Tymnet; Thu, 16 May 85 1:04:03 PDT
Received: from C39.Tymnet by 20.Tymnet; Mon, 22 Apr 85 9:43:59 PST
Return-path: <DERNST@C39.Tymnet 
From: DERNST@C39.Tymnet 
Date: Mon, 22 Apr 85 9:39:42 PST 
To: carl@930 
Subject: micro-node printers 

Carl,
The real printer stuff was working off 554 until Stan appropriated the
printer for the VVax.  I can set up a fixed baud port~ for you at any speed.  I am not sure how everything like back-preasure
works.  I know there is a problem with form-feeds getting
changed to line-feeds, and I haven't had time to look into it.
Dennis
From: BAIGENT@C930.Tymnet 
Date: Fri, 17 May 85 9:34:45 PDT 
To: CARL 
Subject: JOB QUEUE SPECIFICATIONS. 

Carl,
The file (BAIGENT:930)PRPFIL.FRM contains the specifications of the current
PERP job list as well as a list of those items that will be needed for the
PERP replacement program's job queue.  Please take some time to review this
file, add any fields you feel should be included and make comments on the
size (format) of each of these fields (i.e. how many words for date and time
etc...).
Thanks...Dan
From: LOIS@C930.Tymnet 
Date: Mon, 20 May 85 9:26:02 PDT 
To: carl 
Subject: lunch & misc 

Lunch:  We are moving to VG2 on Tuesday, do that would not be a good day for
us either.  I'll check with Adele and let you know later.

Access message:  Someone might want to wander around 930 checking for .vue
and .bak files that could be deleted.  Adele did delete over 1300 pages from
her directory.
Received: from C39.Tymnet by C930.Tymnet; Mon, 20 May 85 12:43:55 PDT
Return-path: <WRS@C39.Tymnet 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 20 May 85 12:37:50 PDT 
To: carl 
Subject: performance and 4meg 

Just thinking...I wonder how many times somebody does a linear search of
the PGY table.  I can't help but think that performance on 39 isn't what it
should be -- things get slow and I do a HOG and there isn't really anything
running that I would think should make it that slow.  A linear search could
take as long as 75 mS!  If we do one every time a page is mapped, that's
about twice as long as it takes to actually read the page off disk!!!  I
thought you might want to check it out some time.  -Bill

PS - 75 mS ~= 4096 x 2 x 9.16uS based on 12-13 memory references per loop
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 21 May 85 14:49:22 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Cc: Osman Guven <osman@C930.Tymnet>, Joe Smith <jms@C930.Tymnet> 
Subject: Re: performance and 4meg 
In-reply-to: your message of Mon, 20 May 85 12:37:50 PDT

Just thinking.... I think that the PGY table is "cached"... does that
mean that every time it does a PGY table sweep, it clears all of cache
by putting the PGY table into the cache!

Also, what if we uncache the PGY table... does that mean that since the
KL reads 4 words at a time, linear searches would be slower if it's not
cached... 4 times slower, having to read 4 words again and again?

Suggestions?  Comments?
/Carl
From: Osman Guven <osman@C930.Tymnet> 
Date: Tue, 21 May 85 16:56:50 PDT 
To: Carl A Baltrunas <carl@C930.Tymnet> 
Subject: SSW Setting.. 

Setting sense switches on a KL-10 system is the function of KLDCP.
Sense switches are 1 2 3 4 5 6
                   - - - - - -
                   0 0 0 0 0 1 = 01    SSW6
                   0 0 0 0 1 0 = 02    SSW5
                   0 0 0 1 0 0 = 04    SSW4
                   0 0 1 0 0 0 = 10    SSW3
                   0 1 0 0 0 0 = 20    SSW2
                   1 0 0 0 0 0 = 40    SSW1

To set SSW2 just simply type the KLDCP command " SW 20 ".
If I could be help any further please let me know.

Osman..
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 23 May 85 17:17:42 PDT 
To: Bill Soley <wrs@C39.Tymnet> 
Cc: Osman Guven <osman@C930.Tymnet>, Joe Smith <jms@C930.Tymnet>, Craig
	Fletcher <fletcherc@C930.Tymnet> 
Subject: .FOCFM - Create frame, NEW STYLE 

Remember the bits added to this, 1B17 to run LOGINN, 1B16 to pass license?
I'm looking at what needs to be passed along to a child whenever a job
is started up by a PERP/PCOM/EXEC process.  I've come up with a slightly
longer list than what originally was installed:

	Original		My List
	========		=======
	PPN			PPN
	PRIVS			PRIVS
	AUN			AUN
	Username		Username
				Network Data
				Project Code
	License (if 1B16)	License
				Tru Limit ???
				Core Limit ???

I'm wondering what would break if I added any of these to the standard
list of things to do.  Granted, TRU limit and CORE limit should probably
be set by LOGINN as it is now [LUD DATA], but there has to be a way to
set UPTLOG/LDBLOG and the project-code for royalty & normal accounting.

It may even be reasonable to set these on the LOGIN UUO... I haven't
really looked yet....

Anyway, what have you written which passes license using 1B16?  And how
easy would it be to change it.  I don't know of many programs which even
KNOW about the new format, let alone use it.  It doesn't exist in /K!

Also, new thought:  I'm planning on changing something in this area, and
probably making a new LOGINN which checks to see if things like license
and project code are set already, so as not to haggle with any project-
code validations program if you are running LOGINN in a frame setup by
the .FOCFM FRMOP call.

Please comment now, before I do anything....
/Carl

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 24 May 85 17:20:24 PDT 
To: Bill Soley <wrs@C39.Tymnet> 
Subject: Try this.... 


    .R DDT			; no license necessary, doesn't matter
    0[  1,,0			; put 1,,0 into ac0
    FRMOP$X			; xct FRMOP 0,0  ac=1,,0 block=CF.LOG,,0
it should SKIP
    0[  1000nnn			; where "nnn" it the new job number
    ^C				; out of DDT
    .SYSTAT nnn			; systat shows running LOGINN TI
    .AT nnn			; attach... and try to do anything

Let me know what you find out.........
/Carl
Received: from C39.Tymnet by C930.Tymnet; Sat, 25 May 85 20:55:58 PDT
Return-path: <WRS@C39.Tymnet 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sat, 25 May 85 20:50:29 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: performance and 4meg 
In-reply-to: your message of Mon, 20 May 85 13:57:25 PDT

Thanks - The trailing ">" bug has been fixed.  Let me know if you 
continue to see it.  -Bill
Received: from C39.Tymnet by C930.Tymnet; Sat, 25 May 85 21:09:25 PDT
Received: from C930.Tymnet by C39.Tymnet; Sat, 25 May 85 21:05:24 PDT
Received: from C39.Tymnet by 20.Tymnet; Mon, 22 Apr 85 9:43:59 PST
Return-path: <DERNST@C39.Tymnet 
From: DERNST@C39.Tymnet 
Date: Mon, 22 Apr 85 9:39:42 PST 
To: carl@930 
Subject: micro-node printers 

Carl,
The real printer stuff was working off 554 until Stan appropriated the
printer for the VVax.  I can set up a fixed baud port~ for you at any speed.  I am not sure how everything like back-preasure
works.  I know there is a problem with form-feeds getting
changed to line-feeds, and I haven't had time to look into it.
Dennis
Received: from C39.Tymnet by C930.Tymnet; Sat, 25 May 85 21:38:38 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sat, 25 May 85 21:03:14 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Osman Guven <osman@C930.Tymnet>, Joe Smith <jms@C930.Tymnet>, Craig
	Fletcher <fletcherc@C930.Tymnet> 
Subject: Re: .FOCFM - Create frame, NEW STYLE 
In-reply-to: your message of Thu, 23 May 85 17:17:42 PDT

----------------------------------------------------------------
From: Carl A Baltrunas  <Carl@X930.Tymnet>
Date: Thu, 23 May 85 17:17:42 PDT

Remember the bits added to this, 1B17 to run LOGINN, 1B16 to pass license?
I'm looking at what needs to be passed along to a child whenever a job
is started up by a PERP/PCOM/EXEC process.  I've come up with a slightly
longer list than what originally was installed:

	Original		My List
	========		=======
	PPN			PPN
	PRIVS			PRIVS
	AUN			AUN
	Username		Username
				Network Data
				Project Code
	License (if 1B16)	License
				Tru Limit ???
				Core Limit ???

[ If the child is being "cloned" from the parent, that is, if it is
being created with the same PPN, etc. then it should get all these things
from the parent.  In fact, I believe that since Darren's work, it will
take all these things from the parent, except TRU LIMIT.  Remember that
Core limit is in JBTPRV so it always has been set.  On the other hand, if
the parent is logging it in with a new PPN, etc. and running LOGINN, then
these things are all specified in the LUD (and should be).  I don't think
Darren finished the changes to LOGINN to go with RUN LOGINN IN CHILD.
I'm not sure, this was in progress when I left.  At any rate, it was
intended (and maybe implemented) so that the user's INIT program and
project code verification program would not be run.  
]

I'm wondering what would break if I added any of these to the standard
list of things to do.  Granted, TRU limit and CORE limit should probably
be set by LOGINN as it is now [LUD DATA], but there has to be a way to
set UPTLOG/LDBLOG and the project-code for royalty & normal accounting.

[ I agree.  There should be a write frame attribute and maybe an
argument to CREFRM to specify a special value for UPTLOG.  This will
allow the submited job to be run "as if from the terminal it was
submited from".  The PERP control file would simply retain the UPTLOG of
the frame that submited the job.  I think that LDBLOG should never be set,
rather, change the GETTAB that everybody uses now to use UPTLOG if it
finds LDBLOG 0 or no terminal.  This is the only total solution since
you can't set LDBLOG if there is no terminal.
]

It may even be reasonable to set these on the LOGIN UUO... I haven't
really looked yet....

Anyway, what have you written which passes license using 1B16?  And how
easy would it be to change it.  I don't know of many programs which even
KNOW about the new format, let alone use it.  It doesn't exist in /K!

[ Yes, I have a few, I think all the MAIL stuff does, not sure.  XEXEC
certainly does.  Why do you want to change it? ]

Also, new thought:  I'm planning on changing something in this area, and
probably making a new LOGINN which checks to see if things like license
and project code are set already, so as not to haggle with any project-
code validations program if you are running LOGINN in a frame setup by
the .FOCFM FRMOP call.

[ Yes, I believe that Darren decided to use JLOG=1 and JBTAUN=0 as a
signal to LOGINN to do special stuff.  After all, when you're running
LOGINN, it will set JBTAUN from the LUD.  Also, it should leave JBTPPN
alone if its non-0, but set it to the AUN if its 0.  I don;t think you
want to run INIT program either.   Again, look in Darren's directory to
see if he started any work on LOGINN, it may just need testing. ]

Please comment now, before I do anything....
/Carl

----------------------------------------------------------------

Hope comments are useful.  Sorry for delay.  -Bill
Received: from C39.Tymnet by C930.Tymnet; Sat, 25 May 85 21:39:03 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sat, 25 May 85 21:31:59 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Try this.... 
In-reply-to: your message of Fri, 24 May 85 17:20:24 PDT

LOGINN is trying to read the username from the base.  LOGINN needs to
be changed to finish the CF.LOG project.  This might be partly done,
look in Darren's directory.  LOGINN

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 26 May 85 2:45:45 PDT 
To: wrs 
Subject: Ref: Try this! 

Well, we looked at LOGINN.  It is indeed trying to read from the
connected port (i.e. the base).  I hacked a PCOM to run LOGINN in the
frame and, (Voila!) it takes the first line of input from the .CTL
file as the login line from the network.  Magic AC license, login as
anyone by knowing what to say to LOGINN... {sigh}

I'll probably be hacking/modifying/rewriting LOGINN this next week
or so.  Other than doing a few special UUOs and the same structure
stuff that GFD does, it needn't do much else.  If you'd like to get
together to chat about this, let me know, otherwise I will probably
feel free to arbitrarily change things.

Darren left *NOTHING* of any work he started but never completed.  It
is possible that Lois stored things to tape from his directory, but
I really doubt it.

I'm looking at INITIA, LOGINN, GFD and LOGOUT all at once to see what
is common between them.  Maybe I'll get a new LOGINN & INITIA out of
this mess.

PS: You realize that I'm mostly just keeping you posted on things that
    are and aren't going on with the monitor.  As soon as any/all that
    needs to be done for the perp replacement (which I may call XEXEC
    for obvious reasons, [so it can be run at startup]) is completed
    we will be releasing a new monitor... P034/P?  {sigh}.
/Carl
Received: from C39.Tymnet by C930.Tymnet; Tue, 28 May 85 11:26:44 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 28 May 85 11:18:47 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Ref: Try this! 
In-reply-to: your message of Sun, 26 May 85 2:45:45 PDT

It would probably be nice to have a LOGINN.SAI ==> .SHR -- probably not
wise to use the high segment, or maybe it is, there's always REFLAG.
Oh well.  The stuff that INITIA does along the lines of LOGINN should
be eliminated.  Just let LOGINN do it.  The monitor can start exactly one
INITIA using CREFRM and it can spawn whatever it likes - basically like
XEXEC does now.  You might keep the monitor involved in the DSKCLN stuff,
that's too much for INITIA to know.  That's my babble.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 28 May 85 12:14:19 PDT 
To: wrs 
Subject: LOGINN, INITIA, DSKCLN, et al 

Points well taken.  I'd already thought about LOGINN.SAI, and maybe a
LOGINN that optionally reads SWITCH.INI and thus eliminates the need for
most INIT programs... or possibly an INITIA that works as on TOPS-10 on
each line before LOGINN runs... which reads SWITCH.INI and (SYS)TTY.INI
for configurations... especially knowing about LOCAL and REMOTE lines...

DSKCLN may be too much for INITIA to know about, but it may also be much
too much for the monitor to keep track of... sigh!
/Carl
Received: from C39.Tymnet by C930.Tymnet; Tue, 28 May 85 12:57:26 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 28 May 85 12:56:58 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: LOGINN, INITIA, DSKCLN, et al 
In-reply-to: your message of Tue, 28 May 85 12:14:19 PDT

I like LOGINN reading SWITCH.INI - INITIA would be okay if we had hardwire
lines.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 29 May 85 18:59:45 PDT 
To: Lois Lindley <Lindley@C36.Tymnet> 
Subject: Hi 

Hi,
  I see that you now have a new username on system 36.
Thought that I'd send you mail to make you feel a little
more welcome.
               Ciao.   /Carl
Received: from C39.Tymnet by C930.Tymnet; Thu, 30 May 85 10:17:54 PDT
Return-path: <LINDLEY@C39.Tymnet> 
From: LINDLEY@C39.Tymnet 
Date: Thu, 30 May 85 10:15:19 PDT 
To: carl@930 
Subject: new? me 

As you can probably tell, my new username is LINDLEY.  Would you give
me an account on 930, if that is all right.  Useful for finding out
information from info, etc.

Did you tell Cheri that I have the saleslip from the gift if she would
like something else?  Just let me know and I can send it interoffice.
I want you to have something you want, so don't mind in the least if you
exchange it.

LINDLEY is valid on 36 and 39, for sending mail or whatever.
-Lois
Received: from C39.Tymnet by C930.Tymnet; Thu, 30 May 85 16:56:25 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 30 May 85 16:55:29 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@C930.Tymnet> 
Subject: Re: INITIA/LOGINN meeting 
In-reply-to: your message of Wed, 29 May 85 13:36:37 PDT

Starting LOGINN with it GFD'd won't currently cause the target directory
to be created, however, that is certainly the correct behavoir for the
new SAIL version.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 31 May 85 3:43:22 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Cc: Joe Smith <jms@C930.Tymnet> 
Subject: Re: INITIA/LOGINN meeting 
In-reply-to: your message of Thu, 30 May 85 16:55:29 PDT

SAIL version would be nice... still thinking on that.  However, if I
want it to have all the bells and whistles using SWITCH.INI it'd be
so much easier to keep LOGINN in MACRO.  I'm planning on hacking DECs
latest LOGIN and putting the pieces directly into our LOGINN to do
all the SCAN/WILD stuff.  Today I've spent losts (losts of lost) time
changing the case and re-formatting LOGINN.  Getting done...
  Is Wednesday (next) good for a lunch/meeting?  Joe still hasn't been
to Kirks as far as I know.  -Carl
Received: from C39.Tymnet by C930.Tymnet; Fri, 31 May 85 10:58:44 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 31 May 85 10:56:50 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@C930.Tymnet> 
Subject: Re: INITIA/LOGINN meeting 
In-reply-to: your message of Fri, 31 May 85 3:43:22 PDT

I won't know my Wednesday schedule until Monday.  Its probably okay,
but I just got put on this task force with omni-premptive priority.
I have you down for 12:00 Wednesday - meet at my office.  -Bill
From: Joe Smith <JMS@C930.Tymnet> (Liberty Street in Fremont) 
Date: Mon, 3 Jun 85 21:14:38 PDT 
To: carl 
Subject: DR WHO records 

Sally bought me the DR WHO picture disk, one with the backwards picture.
Apparently there are 2 backwards disks - one with sound effects on both sides,
the other is a combination of BBC-22316 and BBC-22462.  Your picture disk has
music on one side, mine has BBC-22316A and BBC-22316B stamped in, scratched out,
and BBC-22002A and B scratched in.

BBC-22316A	Sound Effects #19, side 1
1. The Central Control Room in Exillon City -- Dr. Who and the Exillons.
2. The Dalek Control Room -- Death to the Daleks.
3. Metebelis III Atmosphere -- Planet of the Spiders.
4. Styre's Scouting Machine -- The Destructors.
5. Dalek Hatching Tanks on Skaros -- Genesis of the Daleks.
6. Zygon Spaceship Control Centre -- Dr. Who and the Zygons.
7. Sutekh Time Tunnel -- Pyramid of Mars.
8. The Interior of Xoanon -- Face of Evil.

BBC-22316B	Sound Effects #19, Side 2
1. The Shrine of the Sisterhood of Karn -- Dr. Who and the Brain of Morbius.
2. Kraal Disorentation Chamber -- Android Invasion.
3. The Mandragora Helix -- The Curse of Mandragora.
4. Atomic Reactor Runs Wild -- The Hand of Death.
5. Wind-Mine Machine -- Robots of Death.
6. Distillation Chamber -- The Talons of Weng Chi'ang
7. Cloning and Miniaturisation Process -- The Enemy Within.
8. Inside Dr. Who's Mind -- The Enemy Within.
9. Tardis Interior (in flight)
10. Tardis Interior (stationary)
11. Tardis Observation Screen Operates
12. Tardis Door Opens
13. Sonic Screwdriver
14. Fission Gun (2 blasts) -- Ark in Space.
15. Tech Gun -- Face of Evil.
16. Gallifreyan Staser Gun (3 blasts) -- The Deadly Assassin.
17. Vardan Gun -- The Invasion of Time.
18. Sontaran Gun (3 blasts) -- The Invasion of Time.
19. Gallifreyan Staser (3 blasts) -- The Invasion of Time.
20. Dematerializer Gun (switch on and fire) -- The Invasion of Time.
21. Dalek Gun (3 blasts) -- Genesis of the Daleks.
22. Dragon Ray-Gun -- The Talons of Weng Chi'ang.

BBC-22462A	DR. WHO The Music, Side 1
1. Tardis -- Doctor Who (1963 to 1980)
2. The Sea Devils
3. Meglos
4. Nyssa's Theme -- The Keeper of Traken
5. Kassia's Wedding Music -- The Keeper of Traken
6. The Threat of Melkur -- The Keeper of Traken
7. Exploring the Lab -- Four to Doomsday
8. Nyssa is Hypnotized -- Four to Doomsday
9. The Leisure Hive

BBC-22462B	DR. WHO The Music, Side 2
1. Omega Field Force -- Arc of Infinity
2. Ergon Thread -- Arc of Infinity
3. Terminaton of the Doctor -- Arc of Infinity
4. Banqueting Music -- Warrior's Gate
5. TSS Machine Attacked -- Kinda
6. Janissary Band -- Snakedance
7. Subterranean Caves -- Earthshock
8. Requiem -- Earthshock
9. March of the Cybermen -- Earthshock
10. Doctor Who (August 1980 to present)

Which part of the Sound Effects do you have?
--JOE
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 4 Jun 85 11:49:32 PDT 
To: Joe Smith <JMS@C930.Tymnet> (Liberty Street in Fremont) 
Subject: Re: DR WHO records 
In-reply-to: your message of Mon, 3 Jun 85 21:14:38 PDT

From your descriptions, I'd say "Dr. Who, The Music", since I do have
music on one side and more-or-less sound effects on the other.

Mine has 220^%^& (becomes unreadable) scratched out on side 1, and
BBC 22316B scratched out on side 2 with BBC 22002B 1VDTL 11/11/83'
scratched in next to it (also on side 2).

The front has PRINTED on it, BBC records and tapes, BBC-22002.
There are approx. 6 separate tracts (items) on side 1, and 22 of
them on the back.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Tue, 4 Jun 85 23:02:54 PDT 
To: carl, LAGOLD@C39 
Subject: Contents of DR WHO picture disc. 

There are 2 Doctor Who picture disks.  The both have a reversed picture of the
five doctors, and have "BBC records & tapes BBC-22002" printed on the front.
The one in the clear plastic pouch has BBC 22316A scratched on the front and
has sound effects on both sides (8 tracks on front side, 22 on back).
The one in the black cardboard frame has "2200" barely visible beneath the
scratching on the front which has 6 tracks of music.

Music + Sound Effects (black cardboard): Side 1 below, side 2 same as #19.
Sound Effects only (clear plastic pouch): Same as Sound Effects #19, both sides.

        ----------------------------------------------------------

The "Sound Effect" record has the diamond logo and TARDIS on the front, has the
circular vortex on back (in gray and white), and is labelled "No. 19" on back.
The picture disk in the clear plastic pouch is the same as this record.

BBC-22316A	Sound Effects #19, side 1	clear pouch picture disc side 1
1. The Central Control Room in Exillon City -- Dr. Who and the Exillons.
2. The Dalek Control Room -- Death to the Daleks.
3. Metebelis III Atmosphere -- Planet of the Spiders.
4. Styre's Scouting Machine -- The Destructors.
5. Dalek Hatching Tanks on Skaros -- Genesis of the Daleks.
6. Zygon Spaceship Control Centre -- Dr. Who and the Zygons.
7. Sutekh Time Tunnel -- Pyramid of Mars.
8. The Interior of Xoanon -- Face of Evil.

BBC-22316B	Sound Effects #19 side 2	both picture discs side 2
1. The Shrine of the Sisterhood of Karn -- Dr. Who and the Brain of Morbius.
2. Kraal Disorentation Chamber -- Android Invasion.
3. The Mandragora Helix -- The Curse of Mandragora.
4. Atomic Reactor Runs Wild -- The Hand of Death.
5. Wind-Mine Machine -- Robots of Death.
6. Distillation Chamber -- The Talons of Weng Chi'ang
7. Cloning and Miniaturisation Process -- The Enemy Within.
8. Inside Dr. Who's Mind -- The Enemy Within.
9. Tardis Interior (in flight)
10. Tardis Interior (stationary)
11. Tardis Observation Screen Operates
12. Tardis Door Opens
13. Sonic Screwdriver
14. Fission Gun (2 blasts) -- Ark in Space.
15. Tech Gun -- Face of Evil.
16. Gallifreyan Staser Gun (3 blasts) -- The Deadly Assassin.
17. Vardan Gun -- The Invasion of Time.
18. Sontaran Gun (3 blasts) -- The Invasion of Time.
19. Gallifreyan Staser (3 blasts) -- The Invasion of Time.
20. Dematerializer Gun (switch on and fire) -- The Invasion of Time.
21. Dalek Gun (3 blasts) -- Genesis of the Daleks.
22. Dragon Ray-Gun -- The Talons of Weng Chi'ang.

        ----------------------------------------------------------

Music + Sound Effects picture disk comes in a black cardboard frame.

BBC-22002 side 1 - Selections from both sides of "Dr Who The Music"
1. March of the Cybermen -- Earthshock
2. Terminaton of the Doctor -- Arc of Infinity
3. Banqueting Music -- Warrior's Gate
4. Doctor Who signature tune, August 1980 to present
5. The Threat of Melkur -- The Keeper of Traken
6. The Leisure Hive (with TARDIS sounds)

BBC-22002 side 2 - Same as BBC-22316B above (22 tracks of sound effects)

        ----------------------------------------------------------

"Dr Who The Music" has the picture of the 5 Doctors, not reversed on the jacket.
Both logos appear on the back of the jacket, both theme songs are included.

BBC-22462A	DR. WHO The Music, Side 1
1. Tardis -- Doctor Who (1963 to 1980)
2. The Sea Devils
3. Meglos
4. Nyssa's Theme -- The Keeper of Traken
5. Kassia's Wedding Music -- The Keeper of Traken
6. The Threat of Melkur -- The Keeper of Traken         (also on picture disc)
7. Exploring the Lab -- Four to Doomsday
8. Nyssa is Hypnotized -- Four to Doomsday
9. The Leisure Hive (with TARDIS noises)                (also on picture disc)

BBC-22462B	DR. WHO The Music, Side 2
1. Omega Field Force -- Arc of Infinity
2. Ergon Threat -- Arc of Infinity
3. Terminaton of the Doctor -- Arc of Infinity          (also on picture disc)
4. Banqueting Music -- Warrior's Gate                   (also on picture disc)
5. TSS Machine Attacked -- Kinda
6. Janissary Band -- Snakedance
7. Subterranean Caves -- Earthshock
8. Requiem -- Earthshock
9. March of the Cybermen -- Earthshock                  (also on picture disc)
10. Doctor Who (August 1980 to present)                 (also on picture disc)

        ----------------------------------------------------------

There is also a 45-rpm record with just the two "signature tunes".

--JOE
Received: from C36.Tymnet by C930.Tymnet; Thu, 6 Jun 85 15:22:13 PDT
Return-path: <LINDLEY@C36.Tymnet> 
From: LINDLEY@C36.Tymnet 
Date: Thu, 6 Jun 85 15:21:14 PDT 
To: CARL@930 
Subject: Happy birthday!! 

Have a very happy birthday!!!!!!!  (29 is a good age!?!?)
-Lois

P.S.  It was nice seeing you yesterday, and meeting Joe.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Thu, 6 Jun 85 21:22:59 PDT 
To: CARL 
Subject: HiFi VCRs 

  The following is in response to question about rumors of VHS-HiFi machines
  that cannot read normal VHS audio tracks.
 
Date: 19 Mar 1985 0739-EST
From: Dawn Banks <BANKS at LATOUR>
To: LSM.SMITH at MARKET
Subject: RE: Video madness
Mailed to: MARKET::LSM.SMITH

   Can't say as I've heard that.  May sound like someone got confused over
VHS HiFi (using the helical scan with FM modulated audio, which is not
compatible with Dobly Stereo, which uses linear tracks?).

   I have a VHS HiFi portable deck (at Magnavox clone of the Panasonic PV-9600).
When I play a Dolby Stereo tape in it, I get mono sound, since I don't have the
linear stereo heads.  On the other hand, when I play VHS HiFi tapes, I get real
good sound.  I did a side-by-side comparison of my Beta and VHS HiFi decks
a coupla weeks back, by recording a black screen with some music from a half
speed master analog disk.  The Beta machine sounded a bit better, in that it
appeared to have more dynamic range and/or presence.  I think this is due to
the VHS system companding the sound more than Beta (both use single band
companders, VHS is just more radical).  One other interesting note is that
on playback, while the Beta had better sound (only very slightly), the VHS
had fewer dropouts on the black screen (i.e. the VHS's screen was blacker).
I guess that doesn't surprise me any, since it's always looked like Beta tapes
have a higher dropout rate than VHS.  Early on, I noticed that to get the same
dropout rate that I'm used to from standard TDK on a VHS deck, I have to buy
a higher grade Beta tape.  On the other hand, my picture quality on the VHS HiFi
machine is not quite what I'm used to from non-HiFi machines, so I think they
picked up a bit of extra noise with the depth multiplexing.  Note that both
machines have a semi-noticable (depends on what you're playing) hum/buzz that
appears to be 60Hz and/or 1800Hz related.

   Then again, have you heard of the new super Beta decks that should be out
soon?  About 20% more picture resolution (300 vs 240), and a switch that
makes the video frequency response flat (most decks equalize the video/chroma
in a very non-linear fashion) so that the colors don't go wierd when you copy
tapes.

   Later.  (Say hi to Crispin for me).
    --------
[End of forwarded message]
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 7 Jun 85 12:57:05 PDT 
To: carl 
Subject: Additions to LONCNF message. 

In addition to the number of frames and ports could you output the number of
disks and tapes?  It might all fit if the message were something like
[LONCNF F30-P034/N10 KL-1427 3072 pages 108 frames 150 ports 24 disks 8 tapes]
Received: From EMSTXS.Ontyme.Tymnet by C930.Tymnet; Mon, 10 Jun 85 19:18:08 UT
From: TYMNFE.SUP@Ontyme.Tymnet 
Date: 10 JUN 85 19:10:56 
To: TXS.SUP@Ontyme.Tymnet 
Message-id: M32155@Ontyme.Tymnet 
Subject: ORGANIZATION OF MDFSC--FIELD OPERATIONS DIVISION 
Resent-From: Tymcom-X Supervisor <TXSSUP@X930.Tymnet>
Resent-Date: Mon, 10 Jun 85 14:32:22 PDT
Resent-To: Dennis Coffey <dencoff@X930.Tyment>, Bruce Ring <bring@X930.Tyment>,
	Dan Baigent <baigent@X930.Tymnet>, Helge Mortensen
	<helgem@X930.Tymnet>, Carl A Baltrunas <carl@X930.Tymnet>

               MCDONNELL DOUGLAS FIELD SERVICE COMPANY 
     
                     FIELD OPERATIONS DIVISION    
     
     
     
     
DATE:    JUNE 8, 1985    
     
     
     
TO:    ISG MANAGEMENT LIST A-E (INCLUDING MDAIS)  
       FIELD OPERATIONS EMPLOYEES  
     
FROM:  CURT MILLER, VICE PRESIDENT 
       FIELD OPERATIONS, MDFSC
     
SUBJ:  ORGANIZATION OF MDFSC--FIELD OPERATIONS DIVISION
     
-------------------------------------------------------------------------- 
     
     
FOLLOWING A SERIES OF MEETINGS WITH THE NATIONAL/REGIONAL/OPERATIONS/ 
ADMINISTRATIVE MANAGERS OF THE THREE (3) FIELD SERVICE ORGANIZATIONS  
OF MICRODATA, COMPUTER SYSTEMS AND SUPPORT, AND NETWORK FIELD ENGINEERING, 
AN ORGANIZATIONAL STRUCTURE WAS FORMULATED WHICH BEST UTILIZES THE    
TALENTS,  ENERGIES AND NEEDS OF THE FIELD OPERATIONS DIVISION.   
     
I AM CONFIDENT IN THE ABILITIES OF ALL OUR FIELD OPERATIONS EMPLOYEES 
TO MAKE THIS DIVISION ONE OF THE STRONGEST ASSETS OF THE FIELD SERVICE
COMPANY.  THERE ARE CONSIDERABLE CHALLENGES AHEAD, BUT WITH EACH 
INDIVIDUAL CONTRIBUTING HIS/HER BEST, WE WILL NOT ONLY MEET THE OVERALL    
COMPANY GOALS, BUT EACH INDIVIDUAL WILL BE PROVIDED WITH AN OPPORTUNITY    
TO GROW WITH THE COMPANY.
     
THE NEW ORGANIZATION IS EFFECTIVE MONDAY, JUNE 10, 1985, AND IS AS    
FOLLOWS:  
     
     
EASTERN FIELD OPERATIONS------Thomas (Tom) J. Brozena, Director  
     
   Tom has been the Eastern Regional Manager for Tymshare   
   Computer Systems and Support (CSS) since 1984.  Prior    
   to that, he was a Federal Marketing Representative for   
   Tymshare and TRW after a 22 year career in the data 
   processing area of the United States Marine Corps.  
   Tom will continue to be based out of the Washington, D.C. area.    
   The new location of the Eastern Field Operations Headquarters is:  
     
                 EASTERN FIELD OPERATIONS    
                 10001 GEORGE PALMER HIGHWAY 
                 SUITE 131    
                 LANHAM, MD 20706  
                 (301) 459-8363    
                 ONTYME: TCMS.SUP (request ontyme 
                 forwarding to Tom on the THS. system) 
     
   As Director of Eastern Field Operations, Tom will oversee
   the Field Service activities in the following states:    
     
        Maine                       Connecticut   
        New Hampshire               Rhode Island  
        Vermont                     Delaware 
        New York                    Maryland 
        Pennsylvania                West Virginia 
        Massachusettes              Virginia 
        North Carolina              South Carolina
        New Jersey  
     
     
     
CENTRAL FIELD OPERATIONS------Ronald (Ron) P. Dvorsky, Director  
     
   Ron has been in the Field Service business for 20 years, most 
   recently as the Eastern Region Field Operations Director for  
   the former Microdata Corporation.  Ron will be based out of   
   Altanta, Georgia, the new location of the Central Field Operations 
   Headquarters:    
     
                 CENTRAL FIELD OPERATIONS    
                 6195 BARFIELD ROAD, N.E.    
                 SUITE 170    
                 ATLANTA, GA 30328 
                 (404) 252-8065    
                 ONTYME: MICROD.R/DVORSKY    
     
   The Central Field Operations area of responsibility will be:  
     
        North Dakota                Missouri 
        South Dakota                Arkansas 
        Nebraska                    Louisiana
        Oklahoma                    Wisconsin
        Kansas                      Illinois 
        Texas                       Michigan 
        Minnesota                   Indiana  
        Iowa                        Ohio
        Kentucky                    Tennesse 
        Mississippi                 Alabama  
        Georgia                     Florida  
        Puerto Rico 
     
     
     
WESTERN FIELD OPERATIONS------Gene R. Huffman, Director
     
   Gene was the former Microdata Central Region Field Operations 
   Director and has had 26 years experience in the Field Service 
   business.  Gene is currently residing in Oakbrook, Illinois,  
   but will be relocating shortly to California.  Until such time
   as the new Western Field Operations Headquarters location is  
   finalized, Gene will direct the Western Area from his current 
   location:   
     
                 WESTERN FIELD OPERATIONS    
                 (temporary location)   
                 2809 BUTTERFIELD ROAD  
                 SUITE 175    
                 OAK BROOK, IL 60521    
                 (312) 920-8050    
                 ONTYME: MICROD.G/HUFFMAN    
     
Gene will be responsible for the Field Service activities in the 
following states:   
     
        Alaska                      Idaho    
        Hawaii                      Nevada   
        Washington                  Arizona  
        Oregon                      Utah
        California                  New Mexico    
        Montana                     Wyoming  
        Colorado    
     
     
     
FIELD SUPPORT-----------------Jerry H. McCoy, Director 
     
   Jerry has been with the former Tymshare organization for 6    
   years, most recently as National Manager, Computer Systems    
   and Support.  He has a solid background in Field Service 
   and Management which will be utilized in a liaison role  
   to both internal and external customers.  Jerry will also
   be responsible for the MDC internal account, new product 
   introduction, data center management, and overall field  
   support.    
     
   Jerry will continue to be located in the Bay Area at:    
     
                 FIELD SUPPORT
                 39100 LIBERTY STREET   
                 FREMONT, CA 94538 
                 (415) 794-2525    
                 ONTYME: TCMS.J/MCCOY   
     
     
     
FIELD TELECOMMUNICATION SUPPORT--Patrick (Pat) A. Diamond, Manager    
     
   Pat has been with the former Tymnet organization for
   approximately 5 years, most recently as the National
   Field Manager.  He has considerable telecommunication    
   experience and has been on the forefront of several 
   advanced communication projects.  Pat is responsible
   for integrating the telecommunication/network technology 
   into the Field Operations Division.  
     
   Pat will be located in the Bay Area, and can be reached  
   currently at:    
     
                 FIELD TELECOMMUNICATION SUPPORT  
                 10161 BUBB ROAD   
                 CUPERTINO, CA 95014    
                 (408) 446-7676    
                 ONTYME: TYMNFE.DIAMOND 
     
     
     
OPERATIONS SUPPORT-------------Marlene M. Ford, Manager
     
   Marlene has been with the former Microdata Field Service 
   Group for 4 years, most recently as the Customer Service 
   Operations Manager.  She has significantly contributed   
   to the outstanding success of the Microdata Central 
   Dispatch function and overall high operational quality   
   of the Field Service group.  In addition to her current  
   position, Marlene is also the Vice President of the 
   North American Field Service Managers, Orange County
   Chapter.    
     
   The Operations Group will be headquartered in Irvine,    
   California, and have responsibility for central dispatch,
   call screening, and remote operations centers.  Marlene  
   can be reached as follows: 
     
                 OPERATIONS SUPPORT
                 2361 MCGAW AVENUE 
                 P.O. BOX 19501    
                 IRVINE, CA 92713  
                 (714) 250-1000, EXT. 7145   
                 ONTYME: MICROD.M/FORD  
     
     
ADMINISTRATION----------------Patti J. McDonald, Manager    
     
   Patti has been with the former Tymnet Field Engineering  
   organization for the last 3 years.  She has been in 
   administrative management positions for 8 years and 
   brings a high degree or organizational ability to the    
   Field Operations Division.  Patti will be responsible    
   for coordinating all administration, training, and  
   personnel functions, as well as providing Field inter-   
   face to finance and computer system resources. 
     
   Patti will be located in the Bay Area, and currently
   can be reached at:    
     
                 ADMINISTRATION    
                 10161 BUBB ROAD   
                 CUPERTINO, CA 95014    
                 (408) 446-6763    
                 ONTYME: TYMNFE.P/MCDONALD
Received: from C39.Tymnet by C930.Tymnet; Thu, 13 Jun 85 1:32:52 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 13 Jun 85 1:20:33 PDT 
To: carl@930 
Subject: SYSTAT and the phantom jobs (A Hardy Boy's Mystery) 

Somebody at Tymnet just saw a SYSTAT showing some job logged in to user
(INFORMATION) and GFD'd to (CNFE) - in annother GAN!  INFORMATION is the
no password account used to get network access and marketing data.  It
uses PV.RCM and PV.LOS (logout on stop).  Anyway, a scan of the stream
indicated GFD was never run from that user name.  So much for that.  But
what about the SYSTAT?

It is possible, since the SYSTAT program does not do any concurency
control (sigh), that what was seen was the result of a sequence such as:

	job 1 logs in as (INFORMATION)
	job 2 does SYSTAT
	job 2 SYSTAT GETTAB's job 1's AUN: (INFORMATION)
	job 2 is dismissed for quantum expired
	job 1 logs out
	job 1 logs in as (CNFE)
	job 2 SYSTAT GETTAB's job 1's PPN: (CNFE)
	job 2 SYSTAT compares AUN to PPN, finding different, shows
		job 1 as being logged in to (INFORMATION) and GFD'd
		to (CNFE), when in reality this state never existed.

This is really a SYSTAT TSR suggesting that SYSTAT always gettab the
universal process ID before and after gettabing all other information
about each job.  If the first process id isn't the same as the last,
it should do a SLEEP 0 and loop.  The SLEEP 0 enhances its chance of
getting all the way through the next time without being dismissed.

Unfortunately, its not that simple when talking about files, since
the file data is obtained in a different order than the job information.
A more complex scheme is required, but beyond the scope of a TSR.

Please forward this to the appropriate person(s).

-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Jun 85 2:09:11 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: SYSTAT and the phantom jobs (A Hardy Boy's Mystery) 
In-reply-to: your message of Thu, 13 Jun 85 1:20:33 PDT

I suspect that the appropriate person(s) in this case is me.  I also
suspect that this will just have to be a restriction in SYSTAT since
mucking around reading JBT tables via GETTAB works regardless of the
fact that a job is even logged in...  checking universal program idx
sounds ok, but may vary too much even for correct information about
a job ('cept for things like core and jbtnam... sigh)
  Maybe the best solution is a JOBINF UUO that returns gobs of data
about a job, potentially locking down the job's UPT to get that info
too... [futureistically speaking of course].  Anything else could put
SYSTAT into an infinate loop if a "careless" process decided to run
many many many separate programs, 1page or so, .SHR files to speed
page activation, etc...  SYSTAT might never be able to GETTAB everything
fast enough?

SIgh!
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Jun 85 2:17:18 PDT 
To: wrs 
Subject: TUMS feature? 

It would be nice to send mail using a /AFTER:daytime setup... so that
say /AFTER:14-Jun-85:15:35:00 would be sent, but not received until
after said time.

This makes reminders sent to oneself, actually useful!!!!


Subject: TUMS timing on 930.
I changed XEXEC.CTL to only run the mailer every 2 hrs and ONTYME
every three.  Mostly to recduce "hourly" core-usage which "steals"
disk space whilst we are still crunched.  (Artificially, anyway...
I'm purposly keeping a slew of accounting files around to hog space
for an impending monitor transmittal.)

Subject: /P transmittal.
May be soon.  It will be /N10 plus a new STOPCD macro with more
informative messages, instead of just BOOTS LOADED!
The /Q release (expected end of summer) will have the necessary code
for the new perp-replacement.
/Carl
Received: from C36.Tymnet by C930.Tymnet; Thu, 13 Jun 85 8:01:13 PDT
Return-path: <LINDLEY@C36.Tymnet> 
From: LINDLEY@C36.Tymnet 
Date: Thu, 13 Jun 85 7:57:05 PDT 
To: carl@930 
Subject: SPOOL nodpri and priadr 

Carl,

Ken Bedell called to ask me some questions about updating the SPOOL
nodpri and priadr files.  I did a little investigation, and it seems
that the files have not been updated since last June.  In Ken's
conversations concerning the problem with Nancy B, she had said a
necessary file for PJ was missing from all the 10s, which seems
curious, to say the least.  (MPL:930)spool.mem contains instructions
for hand copying over a nodpri file, which I tried.  There are
a few strange entries in the file, so I did nothing about installing
it.

It really does seem like a PJ problem, but it would be nice for
internatinal if the node map could be updated.  Ken has left 
and said he was turning over the problem to Peter Haas.  I
suggested they contact you.4

The files I did are on system 36.  Any questions, send mail
or give me a call.
-Lois

P.S. my extension is x6197.
Received: from C39.Tymnet by C930.Tymnet; Thu, 13 Jun 85 11:23:42 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 13 Jun 85 11:18:55 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: SYSTAT and the phantom jobs (A Hardy Boy's Mystery) 
In-reply-to: your message of Thu, 13 Jun 85 2:09:11 PDT

True, JOBINF would be a much better solution.  It could return
open files as well.  -Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 19 Jun 85 14:02:46 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Carl A Baltrunas <carl@C930.Tymnet>, Osman Guven <osman@C930.Tymnet>, Craig
	Fletcher <fletcherc@C930.Tymnet> 
Subject: Re: /N11 
In-reply-to: your message of Wed, 19 Jun 85 5:12:50 PDT

Changed "INADEF:" to "INADEF::" in COMMON.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 19 Jun 85 14:04:33 PDT 
To: CARL 
Subject: What I did when LOADER complains about JOBDAT. 

	.CTEST SETPROC LOADER=SYS:TC
	.LOAD @MONF3
	.TECO 001LOA.TMP
	*FS/G$/U$EX$$
	.CTEST RUN LOADER
	UNDEFINED SYSBOL INADEF, REFERENCED IN COMCON
	.FIND
	Target 1: INAD
	Search in COM###.MAC
	.TECO COMMON.MAC
	*FNINADEF:$INADEF::
0TT$EX$$
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Jun 85 16:29:30 PDT 
To: Craig Fletcher <fletcherc@C930.Tymnet> 
Subject: Microfiche 

What do we need in the way of Purchase-Order or Requisition to go
ahead and print some microfiche?

Need to know since we have an impending monitor release, and I think
there are some revisions to some standard software, including listings
we ought to be making on microfiche as we generate each package for
the packaging project.

As soon as I have a PO/REQ or whatever we need, then I can get with
Dennis Coffey to show them what they need to do to generate fiche.
I don't know how "open" we can make the requisition?  I'd hate to
have to get a new one every time we want to print a set of listings
to microfiche....
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 19 Jun 85 16:38:15 PDT 
To: jms 
Subject: Microfiche... 

Am waiting for a response from Craig about what we need for making
microfiche, but see no reason why we shouldn't be able to make fiche
of the DEC 7.03 documentation....
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 16 Jun 85 18:28:47 PDT 
To: Carl A Baltrunas <carl@C930.Tymnet>, Osman Guven <osman@C930.Tymnet>, Joe
	Smith <jms@C930.Tymnet> 
Cc: Craig Fletcher <fletcherc@C930.Tymnet> 
Subject: /N11 


Ok,  New features/tables added seem to work fine now:

  1) New GETTAB table pointers to existing data.
  2) New FETTBL feature test table.
  3) New SET DEFAULT INACTIVITY command, SETUUO function, (OP license)
  4) SET AUTOLOGOUT will now default to the inactivitiy timeout if no
     value is given... zero may still leave flags set... will fix!
  5) Part of #1, STOPCD table pointer in place.  Need to build actual
     data area and setup pointers in COMMON & UUOCON.
  6) Part of #2, we will probably use the left-half bits for TYMCOM-X
     features and the right-half bits for equivalent features that DEC
     has in TOPS-10.
  7) C930 is now X930.

/Carl
Received: from C39.Tymnet by X930.Tymnet; Fri, 21 Jun 85 13:56:14 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 21 Jun 85 13:44:33 PDT 
To: carl 
Bcc: CARL@X930.Tymnet 
Subject: subject? 

When makeing changes to TUMS to make it "look like" SEND, I was thinking of
letting it behave exactly like
    TUMS <addresses>
except that if the command is SEND, not TUMS, it will prompt for addresses
if none were provided.  The major difference is the SUBJECT: prompt.  Do
you think that will bother anyone?  I also have to change it to accept
RPG style mailing lists (@xyzzy.cmd) but was planning to do that anyway.

The big problem is reading mail.  I can fix RDMAIL to behave exactly like
the MAIL command does, but it won't create a MAIL.BOX file as per RPG.
In theory, I could read the RPG temp core file and see if it has an entry
and "simulate" the RPG format (ugh).  The question I think, here, is if
you think any customers use CTEST SETMAIL <filename>.

The final problem is one of "LETTER"s.  I'm thinking of just leaving the
existing sicko kludge for now and changing the MAIL/TUMS commands to
check the LUD bit and run RPG if its set (gag).

What do you think about all this?
Received: from C36.Tymnet by X930.Tymnet; Fri, 21 Jun 85 17:23:38 PDT
Return-path: <LINDLEY@C36.Tymnet> 
From: LINDLEY@C36.Tymnet 
Date: Fri, 21 Jun 85 17:23:35 PDT 
To: Carl@930 
Bcc: CARL@X930.Tymnet 
Subject: NODPRI 

Carl,

I notice the OLD nodpri is still on 36.  Whatever happened to putting
up a new one?  Just curious.
-Lois
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 21 Jun 85 17:41:43 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Bcc: CARL@X930.Tymnet 
Subject: Re: subject? 
In-reply-to: your message of Fri, 21 Jun 85 13:44:33 PDT

I don't see any real problems with SUBJECTs.  As far as customers go, we
really don't have all that many customers (period) let alone that many
that should complain if we change anything.  I'd guess that we'd want to
make some kind of "PRODUCT ANNOUNCEMENT" and do away with RPG-MAIL.

As far as MAIL.BOX..., CTEST SETMAIL can disappear and the MAIL program
can simply run TUMS... since it does eveything that MAIL does now.

TUMS needs a "purge" command which deletes messages without putting them
into the archive file... (maybe that exists already?)  I'd even favor a
"dispose" command which prints the headers (optionally via TUMS.INI any
or all parts of a message) and allows the user to keep,delete,archive,reply,
forward,resend each message.  This way TUMS.MSG files won't become huge
and cause problems.  (I do this by hand at the moment, but like the added
convenience).

I'd say, simulate the action of the RPG mailbox/(notice) but under NO
circumstances, simulate it's format.  Anyone wanting to convert mailboxes
can run "NEWMAIL" (the conversion program) and be done with it.

Letters... well... that brings up the BB aspect!!!  I'd like to know how
difficult it would be to have a BB option (maybe need license? or some
validation) where a SPECIAL mailbox is setup that everyone can see the
LETTER messages and possibly others. ... see next messge....

Then the LETTER program would run a "subset" TUMS to insert into the
specialty mailbox "SYSTEM".
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 21 Jun 85 17:56:40 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Bcc: CARL@X930.Tymnet 
Subject: Specialty Mailboxes 

As I mentioned earlier,  specialty mailboxes  would be for bulletin-board
messages.  TUMS would have a master list (like mailing-lists) kept in
(MAIL)BBOARD.DAT  which specifies all active lists.  Some lists would be
"public" that anyone can write to... others would not (eg. "SYSTEM") and
would require some type of license.  Sending to a list would put it up
on the board, which then ANYONE can read (but not delete).  Messages would
then be deleted when they EXPIRE!!!!!!  (have to implement expirations).
TUMS.IDX could have space to keep pointers into "N" BBoard lists so that
you would only see newer messages by default, but could see older ones if
they still exist.

LOGINN would need to be able to read the special SYSTEM BBoard file and
maybe update TUMS.IDX ??? (This is the major problem?)  Or maybe the
entire "unexpired-list" in the SYSTEM BBoard would be seen.  [This aspect
requires more thought]!   BBoards on the whole would have their own .IDX
file residing on (MAIL) [and could even share IDXs -- eg. SF-LOVERS and
DR-WHO BBoards could share a .MSG and .IDX file and would indeed be the
same mailing list, just having different names?]

Anyway, BBoards require being able to have a "USE BBoard bb-name" command
which would then allow reading of the BBoard like one's own MSG file with
some restrictions, of course.  The main reason behind BBoards is to stop
duplication of long messages to members of a group from taking up so much
valuable disk space, one copy to a customer.

Maybe we should have lunch next week... and talk about them?
PS: I've made a similar suggestion about BBoards on ONTYME to rid the
    world of corporate messges sent to *.SUP then being resent from each
    xxx.SUP to xxx.* filling up their disks as well.... sigh!
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 21 Jun 85 18:16:01 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Bcc: CARL@X930.Tymnet 
Subject: LETTER 

Just remember, LOGINN (written in MACRO) needs to be able to manipulate
and read the message file setup for SYSTEM messges.  It's possible that
the message info would be kept "MUCH" simpler... ?
   ...just a thought...     sigh!
Received: from C39.Tymnet by X930.Tymnet; Fri, 21 Jun 85 13:56:14 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 21 Jun 85 13:44:33 PDT 
To: carl 
Subject: subject? 
Resent-From: Carl A Baltrunas <Carl@X930.Tymnet>
Resent-Date: Fri, 21 Jun 85 18:20:23 PDT
Resent-To: Joe Smith <jms@C930.Tymnet>

When makeing changes to TUMS to make it "look like" SEND, I was thinking of
letting it behave exactly like
    TUMS <addresses>
except that if the command is SEND, not TUMS, it will prompt for addresses
if none were provided.  The major difference is the SUBJECT: prompt.  Do
you think that will bother anyone?  I also have to change it to accept
RPG style mailing lists (@xyzzy.cmd) but was planning to do that anyway.

The big problem is reading mail.  I can fix RDMAIL to behave exactly like
the MAIL command does, but it won't create a MAIL.BOX file as per RPG.
In theory, I could read the RPG temp core file and see if it has an entry
and "simulate" the RPG format (ugh).  The question I think, here, is if
you think any customers use CTEST SETMAIL <filename>.

The final problem is one of "LETTER"s.  I'm thinking of just leaving the
existing sicko kludge for now and changing the MAIL/TUMS commands to
check the LUD bit and run RPG if its set (gag).

What do you think about all this?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 21 Jun 85 17:41:43 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: subject? 
In-reply-to: your message of Fri, 21 Jun 85 13:44:33 PDT
Resent-From: Carl A Baltrunas <Carl@X930.Tymnet>
Resent-Date: Fri, 21 Jun 85 18:20:25 PDT
Resent-To: Joe Smith <jms@C930.Tymnet>

I don't see any real problems with SUBJECTs.  As far as customers go, we
really don't have all that many customers (period) let alone that many
that should complain if we change anything.  I'd guess that we'd want to
make some kind of "PRODUCT ANNOUNCEMENT" and do away with RPG-MAIL.

As far as MAIL.BOX..., CTEST SETMAIL can disappear and the MAIL program
can simply run TUMS... since it does eveything that MAIL does now.

TUMS needs a "purge" command which deletes messages without putting them
into the archive file... (maybe that exists already?)  I'd even favor a
"dispose" command which prints the headers (optionally via TUMS.INI any
or all parts of a message) and allows the user to keep,delete,archive,reply,
forward,resend each message.  This way TUMS.MSG files won't become huge
and cause problems.  (I do this by hand at the moment, but like the added
convenience).

I'd say, simulate the action of the RPG mailbox/(notice) but under NO
circumstances, simulate it's format.  Anyone wanting to convert mailboxes
can run "NEWMAIL" (the conversion program) and be done with it.

Letters... well... that brings up the BB aspect!!!  I'd like to know how
difficult it would be to have a BB option (maybe need license? or some
validation) where a SPECIAL mailbox is setup that everyone can see the
LETTER messages and possibly others. ... see next messge....

Then the LETTER program would run a "subset" TUMS to insert into the
specialty mailbox "SYSTEM".
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 21 Jun 85 17:56:40 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: Specialty Mailboxes 
Resent-From: Carl A Baltrunas <Carl@X930.Tymnet>
Resent-Date: Fri, 21 Jun 85 18:20:26 PDT
Resent-To: Joe Smith <jms@C930.Tymnet>

As I mentioned earlier,  specialty mailboxes  would be for bulletin-board
messages.  TUMS would have a master list (like mailing-lists) kept in
(MAIL)BBOARD.DAT  which specifies all active lists.  Some lists would be
"public" that anyone can write to... others would not (eg. "SYSTEM") and
would require some type of license.  Sending to a list would put it up
on the board, which then ANYONE can read (but not delete).  Messages would
then be deleted when they EXPIRE!!!!!!  (have to implement expirations).
TUMS.IDX could have space to keep pointers into "N" BBoard lists so that
you would only see newer messages by default, but could see older ones if
they still exist.

LOGINN would need to be able to read the special SYSTEM BBoard file and
maybe update TUMS.IDX ??? (This is the major problem?)  Or maybe the
entire "unexpired-list" in the SYSTEM BBoard would be seen.  [This aspect
requires more thought]!   BBoards on the whole would have their own .IDX
file residing on (MAIL) [and could even share IDXs -- eg. SF-LOVERS and
DR-WHO BBoards could share a .MSG and .IDX file and would indeed be the
same mailing list, just having different names?]

Anyway, BBoards require being able to have a "USE BBoard bb-name" command
which would then allow reading of the BBoard like one's own MSG file with
some restrictions, of course.  The main reason behind BBoards is to stop
duplication of long messages to members of a group from taking up so much
valuable disk space, one copy to a customer.

Maybe we should have lunch next week... and talk about them?
PS: I've made a similar suggestion about BBoards on ONTYME to rid the
    world of corporate messges sent to *.SUP then being resent from each
    xxx.SUP to xxx.* filling up their disks as well.... sigh!
Received: from C39.Tymnet by X930.Tymnet; Fri, 21 Jun 85 23:42:46 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 21 Jun 85 23:39:18 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: LETTER 
In-reply-to: your message of Fri, 21 Jun 85 18:16:01 PDT

Thanks for your responses.  I haven't read them in detail yet, but I like
what I did read, especially a general buliten board system of which system
messages are an instance of.  Lunch to talk about this is also good.  It'll
probably have to be in a couple weeks, because I'm S*W*A*M*P*E*D!  We also
need to talk about a new SETOP and other political issues dealing with a
certain gross disregard for the importance of security by certain company
presidents (and vice presidents).  Sigh.  -Bill
Received: from C39.Tymnet by X930.Tymnet; Mon, 24 Jun 85 12:59:42 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 24 Jun 85 12:59:15 PDT 
To: carl 
Bcc: CARL@X930.Tymnet 
Subject: (XEXEC)BS 

New hack in (XEXEC) will do a binary search of text files.  Look at
(XEXEC)BS.DOC for more information.  Good for phone book, etc.  -Bill

(pass this on to the group, please)
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Mon, 24 Jun 85 22:40:10 PDT 
To: CARL, OSMAN 
Subject: (M33)CPUS.CTL 

I created CPUS.CTL to verify that the changes I made will assemble on all
4 types of machine.  It does not create any SAV files, just verifies that
the monitor will assemble and load.  We should consider making this mandatory
after any monitor change.

/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 24 Jun 85 23:04:56 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Cc: OSMAN 
Subject: Re: (M33)CPUS.CTL 
In-reply-to: your message of Mon, 24 Jun 85 22:40:10 PDT

It's a good idea to make sure it all assembles on each processor after
any major set of changes.  BUT it is a bit of over-kill for every change.
Make sure it works on 930 and all the code works... then try it out on
each CPU.  If you change any MACROs in S, then try it on all, but just
making a code change in non-processor-dependant areas will work on all if
it works on one...  DEFINATELY make sure it works on any that we give to
ANY customer machine!!!!
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Tue, 25 Jun 85 16:03:10 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Cc: carl 
Subject: Re: subject? 
In-reply-to: your message of Fri, 21 Jun 85 13:44:33 PDT

When you replace RPG in SEND, please make the following work:
	.SEND CARL We have a meeting at 2:00pm
	.SEND JMS PCOM job "BUILD" finished with no errors

I ask that single-line messages be send to all jobs logged in under that
user name (or alias) and that the single-line message not be saved on disk
if there was at least one job that received the message.  If there is no one
logged in under that user name to receive the message in real-time, save it
in (MAIL), but do not copy it to TUMS.MSG when it is read.

/JMS
Received: from C39.Tymnet by X930.Tymnet; Tue, 25 Jun 85 18:43:52 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 25 Jun 85 18:36:29 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Cc: carl 
Subject: Re: subject? 
In-reply-to: your message of Tue, 25 Jun 85 16:03:10 PDT

I don't have any objections, in theory, but I suggest it has to be a
seperate command, since blanks are legal (although ignored) in addresses,
there is no way to parse
    .SEND JMS hi guy - how are you?
I suggest WRITE as a command to behave as you stated.  I'm not sure about
the auto-delete-on-read option.  I was thinking of adding a local-defined
field "options:" which could contain things like
    Options: auto-delete-on-read
    Options: return-receipt-requested
    Options: message-gobbler
but I hate to introduce incompatable fields.  -Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 26 Jun 85 3:46:23 PDT 
To: Carl A Baltrunas <carl@C930.Tymnet>, Osman Guven <osman@C930.Tymnet>, Joe
	Smith <jms@C930.Tymnet> 
Cc: Craig Fletcher <fletcherc@C930.Tymnet> 
Subject: FCR.E+4 crashes 

I've taken a preliminary look at all of them.  There is no set pattern
except for the function which they were attempting to execute.

FCR.E+4 is reached when there is either an FCREATE (VCREATE UUO) or
MAPPING (CHANIO FUNCTION .CHMFP) error.  In the crash case, it is clear
that the code first does a CREATE-FILE-PAGE then attempts to map that
same file page into memory.  That MAP is what appears to be failing.

I've patched all the systems which have this crash occurring to capture
the UUO AC before it clears the page number part.  Without the page #
in monitor memory it is difficult to verify that the page already exists
in memory...  I await the next crash!   {Sigh}.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 26 Jun 85 13:45:07 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Subject: DEFINE X( ARG<DEFAULT> ) is new in MACRO %47, (SYS)MACRO is %46 

Sorry, but in my manual, there are change bars indicating that it is
probably not supported in version %46.  Apparently you must change the
STOPCD macro again..... SIGH!!!!
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 26 Jun 85 15:36:17 PDT 
To: carl 
Subject: BUG in MACRO 43 

I remember when the SPR came out to fix this bug.

File 1)	DSK:S.BAD	created: 1307 26-Jun-85
File 2)	DSK:S.MAC	created: 1531 26-Jun-85

1)11	IFIDN <CONT>,<.>,  <S$TEMP==0>;;	;Same as <.+1>
****
2)11	IFIDN <CONT>,<.>,  <S$TEMP==0>;;	;Same as ".+1"

The first line does not assmble with MACRO 46, the 2nd one does.
MACRO 46 spaced out the 4th "<", and thought the 4th ">" was end of macro
definition, and started assembling the remaining lines.

Restriction: With MACRO 46 it is OK to put ">" after ";", but not "<".
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 26 Jun 85 18:25:15 PDT 
To: carl 
Subject: MACRO 46 has problems. 

It dies on the first file after COMMON, and it does not seem to matter
which file, as long as it is assembled after COMMON.  MACRO 53 works fine.
/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 26 Jun 85 18:48:38 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Subject: Re: MACRO 46 has problems. 
In-reply-to: your message of Wed, 26 Jun 85 18:25:15 PDT

I suspect that we may need to RESET macro %46 after doing COMMON and COMMOD?
It's possible that it's filled it's table space and isn't clearing out...
I remember DEC command files which ran MACRO by hand which every 5-6 files
in a group got out and did an R MACRO.... sigh... so much for .UNVs ?
I'll keep looking.... maybe COMMON does something strange to confuse it,
'cause I saw the error messages inside COMMON which said something about
UNIVERSALS?
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Jun 85 2:16:06 PDT 
To: Joe Smith <jms@C930.Tymnet>, Osman Guven <osman@C930.Tymnet> 
Subject: F3.SAV & F3M46.SAV 

Joe, Your F3.SAV and my F3M46.SAV were built from the same sources
using MACRO %53 and MACRO %46 (TYMSHARE 12.5) and then loaded using
LOADER.  The are not the same size.  Delta = 187 words for the savefiles.
I'd say try running F3M46 and if it runs, then try F3 and see if it runs.
If they both run... we need more testing...
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Jun 85 2:28:39 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Subject: LINK ? What's wrong with it? 

No matter how I try to run it, no matter which one I try, I get
ILL INST at user 000010
????????????????????????????  any ideas?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Jun 85 2:30:02 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Subject: MACRO %46 problem 

See MONF3P.CMD for details.... however on a full re-compile this too
has problems... you can get through it by restarting macro... sigh!

The error message  ?UNIVERSAL PROGRAM(S) MUST HAVE SAME OUTPUT
                     SPECIFICATION AS OTHER FILES
was caused by COMMOD!

COMMOD wanted (needed) /P and as long as the universals didn't ALSO say
/P the error message occurred.  I added /P to everything and it more-or-
less works.  It gets down to ACTSER and wants more /Ps.  It may be a good
idea to rebuild MACRO %46 12.5 with a default larger by 2-3x PDL and then
dispense with all the /Ps ...  in any case, the .REL files on disk are
made by MACRO %46, not %53.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Thu, 27 Jun 85 13:31:38 PDT 
To: carl, osman 
Subject: None of the monitors built last night work. 

(M33)F3.SAV - Runs null job, does not run INITIA, and no DSKCLN
(M33)F3CAB.SAV - Acts just like F3.SAV
(M33)F3M46.SAV - INITIA runs and dies "?Initialization failure GETTAB"
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Jun 85 19:49:01 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Subject: /N11 

I made one backing up all to .N11 sources.  However, .N11 isn't enough.
COMMON at least is newer than that.  I didn't have time.  I'm supposed
to be home by 8, so My monitor isn't tested.  Try reloading and running
with (M33)F3XNCB.SAV and see if it runs.  If it does, then we have a
place to start
./Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 27 Jun 85 20:00:23 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Subject: F3XNCB IS UP AND RUNNING.... IT ISN@T REALLY /N11 BUT 

IT SEEMS TO WORK.... I WILL DO MORE LAATER.... SEND ME MAIL.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 28 Jun 85 10:44:32 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Cc: Osman Guven <osman@C930.Tymnet> 
Subject: Current state of M33 

S.MAC = your newest S non-universalized
??SYM.MAC = old .N11 non-universalized
*.MAC = old sources to .N11 except for COMMON which .N11 is NOT a true
        copy of what we had at /N11, rather it is .N11 + your .CPxxx stuff.

??SYM.CAB + S.CAB = universalized parameter definition files
*.CAB = the old *.MAC files which I backed up to *.N11  which need to be
        examined to determine what isn't working, if anything...

I am going to copy everything to another directory and try building things
using the non-universalized parameter files and the current *.CAB sources.
Then, after testing, if it works, we can add *.JMS on top of that.

Sigh....
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Sun, 30 Jun 85 23:09:55 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Craig
	Fletcher <fletcherc@X930.Tymnet> 
Subject: F3XN12 has a working stopcode processor 

I put in a STOPCD with the name of SIMIOX at FCR.E+3 and deliberately invoked
it.  The output from the CTY is taped to my whiteboard.  It says:

  ?Stopcode SIMIOX, type=JOB, on XX(F3#6) at 30-JUN-1985 23:00:29 PDT
  Job 5 on TTY11 running FILDDT user [3,42754]
  UUO is 043300,,000014 at user PC 000331
  File DSK:NEWMON.SAV[1,4]
  UUO.ID,,UUO.ERROR.CODE = 000001,,000006
  USER.WORD.COUNT,,USER.ADDRESS = 002000,,011647

  Reload monitor

  -------------------------------------------------------

  BASE INITIATED CRASH 1-JUL-1985 6:00:29 GMT
  BOOTS LOADED

/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 30 Jun 85 23:12:39 PDT 
To: jms 
Subject: /N13 

In any case, working or not working, all those changes should be /N12 with
new changes /N13 unless, of course it drastically dies!

Also, your new routine to print DEV:FILE.EXT[P,PN] should be modified before
it is released as /P to type DEV:(USER)FILE.EXT  spaces [P,PN].
We really need both, and since the DRB/PPB info is available, you should use
it.  (Assuming it is non-blank in the username field.)
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 3 Jul 85 20:50:36 PDT 
To: Osman Guven <osman@C930.Tymnet>, Joe Smith <jms@C930.Tymnet> 
Subject: Current monitor... 

Seems to go out to lunch ... or some job? is taking over the CPU for
brief (1-5 seconds) periods of time.  May just be a fluke or may be some
monitor,scheduling problem.  We need to check it out before putting this
on a customer system.

/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Mon, 8 Jul 85 15:14:31 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Osman Guven <osman@C930.Tymnet> 
Subject: Re: Current monitor... 
In-reply-to: your message of Wed, 3 Jul 85 20:50:36 PDT

I have noticed the problem also.  One time, I noticed the TI printer had 
stopped, and characters I typed in were being echoed one character per second.
This happenned twice in an hour, but the system continued normally.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 10 Jul 85 11:53:24 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: Names of stopcodes. 

It doesn't really matter what names are chosen, but I have been using a
convention where the first 3 letters indicate the module and the last 3 are
from the description.  For instance, all STOPCDs in F3TSER, MAGSER, and FTASER
start with "MTA".  All in LPTSER start with "LPT".  Some in PICON start with
"APR" for conditions detected via APR interrupt, others have "PAG" if it is
a page fault or inconsistancy in the UPT.  Any that I put in SCNSER will use
the prefix "TTY".

For the one that are referenced via STOPCD(,XCT,name), I have been putting
"name::" in front of the full definition just to make it obvious to the code
reader that a global lable is being defined here.
/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 10 Jul 85 12:17:29 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Subject: Fixed COMCON 

You zapped a "$" that was a comment terminator when you changed the
code  JRST USCHED  to add the "##".  Sigh. Apparently some of that
stuff is commented out -- I changed it to repeat 0,<> to make it obvious.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 10 Jul 85 17:29:09 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Subject: UUOCON: Don't forget to add STOPCD(?, ?, UUOWXL, , <Old USRDIE>) 


From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 12 Jul 85 14:26:56 PDT 
To: Dennis Coffey <dencoff@X930.Tymnet>, txssup 
Subject: System directories, FAYEZ deletions 


The following is a list of areas and the associated directories needed to
make the entry work, and to support it.  Some of the listed directories
probably do not exist on some system, thus hampering our support in these
areas.  Where multiple lines exist, the first line is required, the second
contains backup and storage (not always necessary or used) and the third
line provides library or "backup" support.

System:
    SYS, FTSYS, OPER, M33, UN1, XEXEC
    support: CARL, JMS, OSMAN, MPL, SPL, OSNF, UTIL, SYSMAINT

Utilities:
    SYS, FTSYS, OPER, SPOOL, PJ, TYMNET, *1BATCH, *6NEWS, TYMGRIPE
    support: CARL, JMS, OSMAN, BURRIESCIN, MARCINJ, SYSMAINT, SPPOPER,
	     SSPAMBIN, SSPERP, SPPARCH, SSPRINT, SSSPOOL
	     MPL, SPL, UTIL, WRS, DONAHUE (backup HW support)

MAGNUM:
    SYS, FTSYS, MAGNUM, FTMAGNUM, UPL, ACTRTY, UAS

Fortran10:
    SYS, FTSYS, UPL

TymBasic
    SYS, FTSYS, FTTBA, TBATLIB (? ACTRTY, UAS)

Distribution:
    SSINSTALL, SSBACKUP, QASYS, CALSTATE, SYSADM21, SYSINST, SYSBACKUP

Library areas:
    PUB, SAILIB, UPL, SPL, MPL

Validations:
    VALIDATE, CUD10, SRAVAL

MDFSC Hardware support:
    DONAHUE, DIAG10

Received: from C39.Tymnet by X930.Tymnet; Tue, 16 Jul 85 11:23:48 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 16 Jul 85 11:17:40 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Cc: CARL 
Subject: Re: [mail waiting] does not always come out. 
In-reply-to: your message of Wed, 26 Jun 85 18:27:04 PDT

----------------------------------------------------------------
Received: from X930.Tymnet by C39.Tymnet; Wed, 26 Jun 85 18:30:49 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 26 Jun 85 18:27:04 PDT 
To: CARL, WRS 
Subject: [mail waiting] does not always come out. 

Osman and I have both noticed today that if you are in an editor when someone
sends mail via TUMS, the little message does not come out on your terminal
when you exit from the editor.  Carl sent me 5 messages today, and I was
not notified until I manually ran TUMS.

/JMS
----------------------------------------------------------------

--Does this still happen?  I have been unable to reproduce it.  If it
is still happening, do you have other jobs logged in at the time?  -Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Tue, 16 Jul 85 14:32:07 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Cc: CARL 
Subject: Re: [mail waiting] does not always come out. 
In-reply-to: your message of Tue, 16 Jul 85 11:17:40 PDT

Today I logged in at 11:05am.  I went into a full-screen editor, and remained
there until 1:05pm.  When I exitted, I got a directory listing, then logged off.
I immediately logged back in because I had forgotten to type out a particular
file.  When I logged in at 1:06pm, I received the message that I had mail
waiting.  It was from the message you sent at 11:23am.  I had received the
period prompt at least 3 times since the message had been sent, but in all
three cases the monitor did not tell me that mail was waiting.  I had only
one job logged in at any time.

In this case, I did not expect to see the message "You have mail from ..."
because I was not at monitor level.  The problem is that the "[mail waiting]"
message typed by COMCON just before it prints the period (or exclamation point)
is not coming out when I return to monitor command level.

/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 16 Jul 85 15:08:28 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Cc: Joe Smith <jms@C930.Tymnet>, txssup 
Subject: [mail waiting] ? algorithm 

Instead of deciding who to send/set the mail-waiting bit for, what if...

When a message is sent, set the MAIL-WAITING bit for all jobs logged in
under that username, thus programs like PEAK,VUE,TUMS,etc. can look for
the bit and let the user know s/he has mail.

When the user runs TUMS,RDMAIL or some mail reading process, then that
process should look to see if any other frames are logged-in under the
particular username still awaiting the MAIL-WAITING typout by COMCON and
clear it for all such frames.

Thus, if an individual program sees the flag and wants to clear it, it
may without consequence, and it may also detect that some other process
cleared it, and notify the user that the previous "mail" had been read.

A new UUO or even a new privilage would not be too bad an idea... privs
needed to clear it on any frame but your own, (maybe JL or WF is all?),
and anyone may set or clear it on their own frame.

What do you think?
/Carl

Received: from C39.Tymnet by X930.Tymnet; Tue, 16 Jul 85 16:34:40 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 16 Jul 85 16:10:52 PDT 
To: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Cc: CARL 
Subject: Re: [mail waiting] does not always come out. 
In-reply-to: your message of Tue, 16 Jul 85 14:32:07 PDT

Did you get the "mail" prompt in the status line (if PEAK, I don't know
if VUE has this)?  I believe the problem is most likely that the message
was discarded due to lac of buffer/ring space.  I have seen this happen
before with other monitor messages (ILL MEM REF, PDL OV, etc.) -- there
is no mechanism to "backpressure" these messages since they occur at
interrupt level.  Sigh.  Let me know if you think this might be the
problem.  The solution is to (1) fix the monitor somehow (no suggestions),
or (2) change the editor to send a yellow ball and wait for orange ball
before doing the exit.  This is advisable anyway, since it is messing
with terminal characteristics.  I think PEAK already does this.  -Bill
Received: from C39.Tymnet by X930.Tymnet; Tue, 16 Jul 85 16:35:12 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 16 Jul 85 16:15:52 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@C930.Tymnet> 
Subject: Re: [mail waiting] ? algorithm 
In-reply-to: your message of Tue, 16 Jul 85 15:08:28 PDT

That's mostly how it happens now:

    procedure SetMailWaitingForUser( userName )
    begin
	forEach job suchThat JBTUNM( job ) = userName
	begin
	    if JBTSTS( job ) = ^C
		then sendTalkMsg( job, "[mail from..." )
		else setMailWaiting( job )
	end
    end

Now, PEAK looks at mail waiting in JBTPRV and sets a "MAIL" prompt in
the status line.  It currently does not clear the bit, although the
new monitor does allow it to be cleared by SETPRV UUO.  The monitor
clears the bit after outputting the message (or trying).  I am in
favor of the suggestion to not have the monitor or LOGINN clear the
bit, and have RDMAIL/TUMS go around an clear it.  I'll change TUMS and
RDMAIL to do this, if you change the monitor and LOGINN.  I propose
making the existing SET-NEW-MAIL-WAITING UUO do this by taking the
sign bit of the AC field to indicate to set or clear...

	MOVE	ac,JOB
	
	TLO	ac,400000	; clear mail waiting
	SETNMW	ac,

-Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 17 Jul 85 13:05:46 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: 99 stopcodes of CORE1 on the wall 

99 stopcodes of CORE1, take one down, pass it around, 98 stopcodes of CORE
Honest.  The STOPCD macro occurs 99 times in CORE1.MAC!
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 17 Jul 85 13:16:56 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: I hate RPG 

It's *&@#$ incompatible.  It won't allow the following:
	TYPE MON.TEC		;This is a comment
	DELETE MON.BAK
I want my comments!  I want ";" and "!" for in-line comments! Grr.
From: KEN@X930.Tymnet 
Date: Wed, 17 Jul 85 14:52:07 PDT 
To: carl 
Subject: hello there 

Well, it couldn't last forever.  But who would have thought the prodigal
son would wind up on the 2nd floor amongst all this furniture, working for
his old boss, AND being paid?

Most certainly not I.

But I've learned to adapt.

Hello, carl, how are you doing?  Shall we chat?

Hee hee hee

---ken
Received: from C39.Tymnet by X930.Tymnet; Wed, 17 Jul 85 17:47:52 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 17 Jul 85 17:43:46 PDT 
To: carl, osman 
Subject: LOGINN license and TUMS 

LOGINN version 67-3 (current) which checks for (MAIL)ppn.MAI and
prints [MAIL WAITING] needs RF which the old version didn't.  It
has been unable to check for mail for non-gan-3 users (MAIL is in
GAN 3) for some time since I guess software distribution screwed
up.  LOGINN states correctly the license it needs when it is
assembled.  I talked to Dennis and he will cal the software dis-
tribution people to straighten this out.  In the mean time I have
changed the license on the systems with TUMS.  There is no security
problem since LOGINN reduces its license before running INIT files.

This is for your information, only, no action required.
-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 18 Jul 85 0:24:21 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: [MAIL WAITING] LOGINN? etc... 

I'll have to re read all this, but I'm confused.... why should either LOGINN
or the monitor not clear the mail-waiting bit as now?  That way the message
is seen only once...

  If not... won't I see [mail waiting] everytime i hit a CRLF once someone
has sent me mail until I finally have time to read it...  that won't do...

If I'm in a hurry, giving a demo, etc, don't have time to read mail 'cause
of how much I get....  why should I expect to see the mail-waiting prompt
more than once, and mess up my demo or clutter my screen.  This just will
not do.
/Carl
Received: from C39.Tymnet by X930.Tymnet; Thu, 18 Jul 85 1:31:08 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 18 Jul 85 1:24:09 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: [MAIL WAITING] LOGINN? etc... 
In-reply-to: your message of Thu, 18 Jul 85 0:24:21 PDT

Yes, that's right.  You'd get [mail waiting] forever until read.  If that's
not what you meant, then I misunderstood you.  I agree that would be a bit
much.  As it is, MAILER will remind you every time it runs if you have unread
mail.  So I guess there aren't any changes necessary, then, except to fix the
li
Received: from C39.Tymnet by X930.Tymnet; Thu, 18 Jul 85 1:31:13 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 18 Jul 85 1:26:19 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: [MAIL WAITING] LOGINN? etc... 
In-reply-to: your message of Thu, 18 Jul 85 0:24:21 PDT

Oh well, I forgot I wasn't in PEAK and typed ^U^H^D to delete that lower case
"e" in PeEAK and guess what the ^D did.  That was all I had to say, anyway,
so... By the way - vacation 7/27 through 8/19...  This means...

					      1
	COEFFICIENT of INSANITY   =   ----------------
				      TIME to VACATION

That is, as vacation draws near, insanity approaches infinity.
-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 18 Jul 85 14:03:36 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: mail-waiting, etc. 

I will try to recap what I think I meant in a later message.  Basically,
the mail-waiting bit should get set always, regardless of whether the user
is in ^C state or not.  The [mail from xxxxxx] message is gravy.

    I know about vacations... I'm finishing up a "forced" vacation.  On
Monday I was at Kaiser in Santa Clara waiting paitently with Cherie who
was having some exploratory surgery (testing) done that day.  In the
middle of a yawn, the muscle under my chin wouldn't relax and caused
enough pain that I passed out.  I spent the next few hours inside the
emergency room as a patient with a severe concusion.  It was more than
an hour before I even "knew" where I was, although they told me that I
kept asking questions like, "Where's my wife" and "What's today's date".

    So, I've been at home recovering, all week.  They want me to see a
neurologist before they let me drive again, just to make sure there's no
permanent damage that might cause problems or re-occur while driving.
Cherie said that when I fell my head hit the floor and bounced (it hit
with that much force).

I hope that this is not an omen of what to expect as "forced" retirement.

Sigh!	/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 18 Jul 85 15:05:36 PDT 
To: KEN@X930.Tymnet 
Subject: Re: hello there 
In-reply-to: your message of Wed, 17 Jul 85 14:52:07 PDT

Welllll.... so what's your extension....  /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 18 Jul 85 17:22:31 PDT 
To: Joe Smith <jms@C930.Tymnet> 
Cc: Osman Guven <osman@C930.Tymnet>, Craig Fletcher <fletcherc@C930.Tymnet> 
Subject: Monitor changes 

Joe,
  I'd like you to put together a PCOM command file, whether it be done with
some sort of TECO macro or what... but something that will produce a set of
output files *.SCM or *.DIF (or something unique?) which can then be filed
with the transmittal or the MEM file to show all the changes made to all the
source files.
  I'd like to see an INCREMENTAL differences... such that

    COMMON.MAC <= COMMON.N13 <= COMMON.N12 <= COMMON.N11 <= COMMON.N06
    CORE1.MAC  <= CORE1.N12  <= CORE1.N10
    ONCE.MAC   <= ONCE.N12   <= ONCE.N02

We see for each module, differencs from the .N00 released monitor to present:

    COMMON.DIF <= {{.N00,.N06 + .N06,.N11 + .N11,.N12 + .N12,.N13 + .N13,.MAC}}
    CORE1.DIF  <= {{.N00,.N10 + .N10,.N12 + .N12,.MAC}}
    ONCE.DIF   <= {{.N00,.N02 + .N02,.N12 + .N12,.MAC}}

Maybe produce a single file... (N to P)
    P034NP.DIF <= {{ COMMON.DIF + CORE1.DIF + ONCE.DIF + ... }}

At least think about it.  Because we need some sort of control over the
changes after they've been done, as a review to insure thaat we have not
added any problems by mis-typing anything.

/Carl
Received: from C39.Tymnet by X930.Tymnet; Fri, 19 Jul 85 14:00:46 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 19 Jul 85 5:15:29 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: mail-waiting, etc. 
In-reply-to: your message of Thu, 18 Jul 85 14:03:36 PDT

Ouch, that even hurts to read.  I hope
it doesn't create cronic headaches.  I
have an infinitesimal tollerance for
headaches.  Well, yes, get well.

MAIL WAITING: why set the bit if the
mail waiting message has already been
output using TALK?  Then you'd get two.

Sorry for the 40 char lines.  I'm using
my new TRS-80 #100 lap computer to send
this.  For $299 I decided I could put
up with a 8x40 screen (LCD).  I plan to
make it into a packet radio node.  Then
I could use Tymnet from my car, plane,
walking in the park, etc.  Anyway, its
a 3 day sale ending Sat if you care.

Bye for now.  Take care of yourselves.
-Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 19 Jul 85 19:59:05 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: MACRO generates bad code if ; ; in comment after STOPCD at end of
	literal. 

In SEGCON, it originally said
STOPCD()]	;OTHER ERRORS IMPLY BUG
I changed it to
	STOPCD ]	;OTHER ERRORS IMPLY BUG ;;ADD1PG+5
which caused MACRO to generate bad code.  Had to move "]" to next line.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 19 Jul 85 20:28:24 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: F3XN14 is in (SYS)NEWMON.SAV 

Fixed all occurrances of where MACRO was generating bad code, it is now
running on the F3.  The monitors KIXN14, KLXN14, and KSXN14 need to be tested.
Received: from C26.Tymnet by X930.Tymnet; Mon, 22 Jul 85 21:00:22 PDT
Return-path: <DENCOFF@26.Tymnet> 
From: Dennis Coffey <DENCOFF@26.Tymnet> 
Date: Mon, 22 Jul 85 14:13:15 PDT 
To: Jon Mosser <MOSSERJ@X930.Tymnet>, Carl Baltrunas <CARL@X930.Tymnet> 
Subject: suggested utility (support for SPOOL) 

SPOOL has a current maximum for the network node numbers which can be
automatically associated with spool 'remote' sights.  The largest node
number that can be accepted is 3776.  Tymnet currently has node numbers
ranging as high as 6000+.  The user can bypass this limitation by
specifying a node number which he knows is in the valid Tymsat list.

Allowing users access to the SPNODE program (currently in SPPOPER) would
provide a service to users of SPOOL.  If their attempt to use SPOOL from
a Tymsat not in the list fails, and they do not already know the number
of a node which is assigned to the printer location they desire, they
have a potential problem.  Being able to run SPNODE from the SYS
directory would allow them to easily learn a node number to give the
SPOOL program to get their desired results.

I recommend that we place SPNODE in the SYS directory, with protection
to allow all users to run it, and license to allow the program to read
the NODPRI and PRIDAR files in the SPOOL directory.

Please let me know if I should do this.

thanks!
/D.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 23 Jul 85 2:06:32 PDT 
To: Dennis Coffey <dencoff@X930.Tymnet>, Jon Mosser <mosserj@C930.Tymnet> 
Subject: in reply to: suggested utility (support for SPOOL) 

PLEASE.... NO!!!!!

SPOOL IS ALREADY A PATCHWORK MONSTROSITY!!!!!  ADDING YET ANOTHER
PROGRAM ("SPNODE") ONLY COMPLICATES MATTERS YET ANOTHER LEVEL.

PLEASE DO THIS THE RIGHT WAY, AND FIX SPOOL TO UNDERSTAND HIGHER NODE
NUMBERS.  DENNIS & I TALKED ABOUT THIS AND THE TRAINING AND HANDHOLDING
NECESSARY TO ADD ANOTHER PIECEMEAL "HANGER-ON" PROGRAM FOR THE SPOOL
USER IS NOT, I REPEAT, "N" "O" "T" WORTH THE TIME IT WOULD TAKE TO FIX
SPOOL.

--- Already, SPOOL consists of:
    SPOOL, BCHARG, RCHARG, RBCHG, SHC, TAPCHG,
    SLVSUP, SUPSLV, SPFIX, SPLFIX, 1PNUPD  and a couple of others...


Not to mention, SPNODE and a few other "internally" used programs.
I haven't touched the code in SPOOL for close to 3 years, but I still
recall enough to know that the changes necessary to SPOOL to make it
read node<==>printer correlations for nodes higher than node 3777 is
only a couple of lines of code.  In fact, it could make the code smaller
if we removed the "+" feature.  However, that not withstanding, I'd bet
that I could patch the current "running" spool by changing references to
a couple of constants... that's how trivial the change would be.


The "work" involved would be to generate a version of 1PNUPD that ran on
the PDP-10s instead of on the 940s, and I understand that that is a real
necessity before the end of the year anyway!   (And I have a solution for
that as well.  -Sigh).

/Carl
Received: from C26.Tymnet by X930.Tymnet; Tue, 23 Jul 85 9:39:49 PDT
Return-path: <DENCOFF@26.Tymnet> 
From: Dennis Coffey <DENCOFF@26.Tymnet> 
Date: Tue, 23 Jul 85 9:13:29 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Jon Mosser <mosserj@C930.Tymnet> 
Subject: Re: in reply to: suggested utility (support for SPOOL) 
In-reply-to: your message of Tue, 23 Jul 85 2:06:32 PDT

Carl, doesn't the 1PNUPD program need PJ, so that migrating 1PNUPD to a
Tymcom-X host would also need migrating PJ to a Tymcom-X?

I know that PJ doesn't currently do the updating of the node number to
printer location data base files on the Tymcom-X's, so the current lack
of PJ on a Tymcom-X is academic (for this job).  We're currently doing
the updating of these data base files by hand, and having a version of
1PNUPD on a Tymcom-X would make that process easier, as in interim
solution to the current, hopefully temporary lack of a Tymcom-X version
of PJ.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 23 Jul 85 12:56:24 PDT 
To: Dennis Coffey <DENCOFF@26.Tymnet> 
Cc: Jon Mosser <mosserj@C930.Tymnet> 
Subject: Re: in reply to: suggested utility (support for SPOOL) 
In-reply-to: your message of Tue, 23 Jul 85 9:13:29 PDT

1PNUPD merely updates the NODPRI/PRIADR databases.
As far as updating the data from "a" master system to all the rest of
the systems, that requires a periodic transfer program, such as what the
(PJ)PAM program was doing.

When PERP is completed, PAM may simply vanish, since it will already have
been replaced by PERP....  sigh.  So, don't worry about PJ dependancies
since PJ & PAM won't exist for much longer.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Tue, 23 Jul 85 23:20:08 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: N15 

The reason why the STOPCODE processor died was due to a missing PUSHJ P,INLMES
in front of an in-line ASCIZ message.  It still needs to minor reformatting,
so that the output from the stopcode processor does not go past 72 columns.
/JMS
Received: from C39.Tymnet by X930.Tymnet; Wed, 24 Jul 85 2:38:06 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Wed, 24 Jul 85 2:30:05 PDT 
To: CARL 
Subject: THE BUG 

Well, that part of SCNSER is a rat nest, after I decided I needed
to draw a picture, it became pretty clear what was happening.  I
still haven't decided why it ever works.  I do think it is the speed,
although, I think the varian is faster, never allowing the ring to
become full.  At any rate, the fix is...

	SCNSER$:
	SETOUT+0.5!  SETZM MULCNT+0

That is to say, insert that instruction after the first one in
routine SETOUT.  This just emphasizes the fact that there is no
space.  Some places check MULCNT, some check MXMCNT.  

Actually, I think it does happen with the varians, I've had an
occasional report of two character being lost, but when I checked
the black ball counts, I passed them off as some other network or
terminal problem.  I recommend this change be made to the next release
and patched on any system running Engine base.  It is very, very
safe (i.e. I don't expect it to cause any problems)  

-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 24 Jul 85 12:58:19 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: THE BUG 
In-reply-to: your message of Wed, 24 Jul 85 2:30:05 PDT

Bill,
  I will look at the code and see if that is the only thing necessary
to catch/fix the problem.  I agree, that part of code is a rat's nest
and that generally defies making a simple fix. Sigh.  Let you know..
Probably put it up on system C39 asap if it runs on 930.

  Last 2 changes we're trying to put in are to make sure the STOPCD
printer stuff works right in all cases, formatted nicely, and I've been
trying to get the inactivity timeout code to be setup in all cases that
a user sets PVDINA, either by SETPRV or LOGIN or CREFRM.  The code is
in, but fails testing...  SETPRV works fine, but LOGIN doesn't.  Soon
as that's fixed, we'll schedule it up on 39.
/Carl
From: Osman Guven <osman@C930.Tymnet> 
Date: Wed, 24 Jul 85 13:14:18 PDT 
To: Carl A Baltrunas <carl@C930.Tymnet>, jms 
Subject: NOTES.. 

Notes:

1. Micronode 552 is now (7/24  12:45) running TYMNET II code as node 6657.
   Any problem please let Dennis E. know, old code still in (CURR11).
2. I would like to take off during the day tomorrow (7/25).
3. Worked in BLDG C last night helping T/M.

Osman..
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 24 Jul 85 14:54:00 PDT 
To: Dennis Coffey <DENCOFF@26.Tymnet> 
Cc: Jon Mosser <MOSSERJ@X930.Tymnet> 
Bcc: CARL@X930.Tymnet 
Subject: Re: PEAK for users 
In-reply-to: your message of Wed, 24 Jul 85 14:39:38 PDT

The provision of "no support" sounds nice on paper, but it doesn't hold
water for a product like PEAK.

I'd like to know the following:
   1)  Does CEGI use the Tymshare bit for anything?
       i.e. do any of their programs check for it?
   2)  Do they restrict their customers in any way?
   3)  Do their customers have regular TYMNET access, i.e. TYMNET usernames
       or are they all "only" on the 2020's ((and their 1 KL)).

Depending upon the answers to these questions, HOW to release PEAK to their
users will change.

An aside:  Since Ken Dawson is working for TYMNET again, (as of last wednesday)
           there may be some development work on PEAK in the forseeable future
           and quite possibly, he could make the necessary changes for CEGI for
           us.

One more question for CEGI: How many customer groups do they have?  GAN's...
   PEAK has an internal list of customers who may use it, and depending upon
   their needs, that list could be augmented, or made external, so that CEGI
   could restrict or allow usage of PEAK on a customer by customer basis.

Let me know what you find out.
/Carl

PS:  Until it is decided that PEAK is error-free enough for general customer
     use, I'm against removing the TYMSHARE-BIT restriction.  Especially if
     we're not supporting it!!!!!
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 26 Jul 85 0:07:24 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Comments!!! 

Joe,
  I realize that I'm "HARPING", but I do think that we can have a little
more description with our code.  I looked at the output from CPUS and then
the code in ERRCON where you go for %UPT+UPTMPC inside the KI conditional.

Since I don't have a HW REF manual at home, (silly me), I'm not sure if
what you're trying to do was a typo or what...  Does the KI store the
EXEC PC somewhere other than UPTMUP ?

    KISYM.MAC       
    {Page 6}...
    XP UPTMUU,424          ;MUUO STORED HERE
    XP UPTMUP,425          ;MUUO PC STORED HERE

The point being, if I knew where you were getting the EXEC PC from, I
might have been able to fix it.  Sans comments, this isn't as easy...

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 26 Jul 85 2:44:02 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, William R Soley <wrs@C39.Tymnet> 
Subject: (MPL:930)WHOMAI 

Lists by username, ppn, filesize(words), sixbitppn, days since read
the information that can be gleamed from DIR (MAIL)*.MAI so that you
can see who does and doesn't have TUMS mail waiting.  This is mainly
a debugging tool, so I put it on MPL not SPL or XEXEC.  Comments?
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 26 Jul 85 2:54:33 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Subject: Disk Space 

Joe,
  Please see what DEC monitor sources that we have on-line that we
can now discard.  I.E. Stuff you put up for looking at for the KA in
Chicago (if at all).  We're down below 8K and we pretty much need
a good 10K + some buffer to generate a full set of MMonitor listings
for microfiche.  I did pretty much clear out the (NOBACKUP) directory
of deletable stuff....  & I know that (M33) has gotten pretty big..
but that all needs to be archived with the transmittal.... sigh! so
it needs to be around.   /Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 26 Jul 85 12:14:13 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Comments!!! 
In-reply-to: your message of Fri, 26 Jul 85 0:07:24 PDT

TYPIME:	PUSHJ	P,INLMES##
	 ASCIZ /Instruction is /
	MOVE	T1,@%UPT+UPTMUP ;Kernal Trap MUUO saved PC of failed instruction
	PUSHJ	P,HWDPNT
	MOVE	T1,%UPT+UPTMU
	PUSHJ	P,PCP		;Print " at Exec PC xxxxxx"

With the typo removed, it is obvious that it references %UPT+UPTMUP twice.
The first time indirect to pick up the instruction, the second time directly
to puck up the PC that pointed to the instruction.  The KI responds to a
page failure by exectuting an MUUO in Kernal mode as a result of a Trap,
therefore the new PC is from %UPT+UPTKTR but the old PC is at 425 like all
other UUOs.

There was a comment that was eliminated when I changed HRRZ T1,%UPT+UPTMUP and
MOVE T1,(T1) to MOVE T1,@%UPTMUP.  The key thing is the "@".  Anyway, the
comments in that section of code have been updated to be redundantly redundant.
Received: from C39.Tymnet by X930.Tymnet; Fri, 26 Jul 85 12:46:35 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 26 Jul 85 12:44:15 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: (MPL:930)WHOMAI 
In-reply-to: your message of Fri, 26 Jul 85 2:44:02 PDT

Sounds alot like 
  RDMAIL -A
its a debugging tool so it isn't documented.
-Bill
Received: from C39.Tymnet by X930.Tymnet; Fri, 26 Jul 85 12:50:05 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 26 Jul 85 12:45:19 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Nice try.... sigh! 
In-reply-to: your message of Fri, 26 Jul 85 2:49:02 PDT

The MAILER checks all the .MAI files by creation date (date of first message)
and returns to sender anything older than 60 days.  This keeps mail from
piling up too much except:
  1. current APPEND library function goes out of its way to update the
      creation date on an append (WRONG!) so if user keeps getting mail,
	this feature will never happen - this will be fixed.
  2. plan to add check to see if user is still valid so mail will get
    returned to sender immediately upon cancelation of a username, rather
	than waiting for 60 day timeout.
  3. need to write an <any-imaginable-date-string> to <universal-date>
	translator to make this all work right.
-Bill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 26 Jul 85 21:17:53 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Don't know why it did not /D on the UIL. 

Changed ERRCON to make sure WHATUU always gets called.  Could not find why
the boot text got changed.  Did not stay long enough to load N20.
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Mon, 29 Jul 85 0:01:00 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Haynok, horned god of the rubber tent 

         "Chow Mein and Bowling"

         Chow mein and bowling, twinkies and champagne
         Moon over McDonalds, frozen pizza in the rain (in the rain)
         Dancing in the dump truck, dining with the dog
         Laughing in the bathtub, with a ducky and a frog
         Stake me to an anthill, shoot me to the moon
         With out you I am no one, like a burger with no bun (bun bun bun)
         Like a burger with no bun, I can't go on

         On the spot we first met, flowers grow today
         Where's that little toxic waste-dump, where two children used to play
         Used to (cough cough) play
         Meat and diet cola, duck a la butane
         Refried beans and rare wines, like they give you on a plane
         Oh, take away my game shows, make me watch the news
         With out you I am no know one, like a gumball with no gum
         Like a slumlord with no slum
         I can't go on (he can't go on)
         I can't go on (he can't go on)
         I can't go on (he can't go on)
         I can't go on (he can't go on)
         I really can't go on

From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Mon, 29 Jul 85 12:40:39 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: DATE/TIME FROM SUPERVISOR 

552 BURPED.  OSMAN RESTARTED IT, HAD TO RELOAD 930.  THIS TIME 930 DID NOT
GET THE DATE/TIME FROM THE NETWORK.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 31 Jul 85 17:02:41 UT
From: NTD.J/NEIDRAUER@Ontyme.Tymnet 
Date: 31 JUL 85 09:17:29 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: I24087@Ontyme.Tymnet 
Subject: Netval (new CUD update Slave) Test 

From: JMARCIN@C36.Tymnet
Date: Wed, 31 Jul 85 9:06:11 PDT
To: txs.c/baltrunas@ontyme.tymnet
Subject: Netval (new CUD update Slave) Test


						July 31, 1985

Carl:

Netval version 3.00 is operational in VALNET; in anticipation of it being
installed in the Public network soon (there seems to now be a holdup in
procuring the hardware), I would like you to test the communication of
any Tymcom-X programs that you know about that access NCUD1 to update the
CUD (password change program is all I can think of).  It can be accessed
for testing with the username VALNETNCUD1, defined as follows:

- VALNETNCUD1 has been set up in the public network with the following
attributes:

		ignore host
		transparent gateway
		Origin:
		    Class 0
		Destinations:
		    2 home host gateways to VALNET: 5762, 5763

- VALNETNCUD1 has been set up in VALNET with the following attributes:

		ignore host
		no password
		Origin:
		    Class 0
		Destination:
		    home host 83 (Netval host)


Please give it a try, and if you have any questions or problems, let me know.


Thanks.

Jill
Received: from X95.Tymnet by X930.Tymnet; Wed, 31 Jul 85 12:27:01 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Mon, 29 Jul 85 22:04:20 PDT 
To: CARL 
Subject: Weirdness 

So what do you think of "Chowmein and Bowling"?
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 31 Jul 85 13:41:00 PDT 
To: NTD.J/NEIDRAUER@Ontyme.Tymnet 
Subject: Re: Netval (new CUD update Slave) Test 
In-reply-to: I24087@Ontyme.Tymnet of 31 JUL 85 09:17:29

Jill,
  To test... I must change the programs which now build circuits to NCUD1
to build circuits to VALNETNCUD1 (spelling?)... right?  I will assume that
I'm right unless you send me mail saying otherwise.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Wed, 31 Jul 85 14:38:19 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Weirdness 
In-reply-to: your message of Wed, 31 Jul 85 13:28:53 PDT

You mean you did not watch "Television Parts"?  Shame on you!  It was
broadcast in place of "Saturday Night Live" on the 27th.


From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 31 Jul 85 18:08:09 PDT 
To: lagold 
Subject: Nosey me... 

Lynn,
  Dare I ask if you are using, or intending to use any of the files in
your directory on this system (930)?  I ask, because the files were from
a disused mail system, and no longer bear any (I repeat, ANY) relationship
to any currently existing real-world TYMCOM-X mail system.
  May I be nosey and ask what you're up to?
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 31 Jul 85 18:50:32 PDT 
To: jmarcin@c39 
Subject: valnet-ncud1 

Jill,
  I tried a couple of things out using VALNETNCUD1 instead of NCUD1
and met with pretty consistant (successful) results.  I didn't try
changing passwords, but I did try changing class, adding/deleting
class and requesting MUD info for username data verification.  I'd
say that most things work OK.  I didn't do a thorough test, nor did
I try to validate/cancel a user.  I will try those later and let you
know if there's any problems.
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 1 Aug 85 2:13:52 PDT 
To: jms 
Cc: William R Soley <wrs@C39.Tymnet> 
Subject: XEXEC on 95 

I believe bill got his wires (licenses) crossed!

I saw your msg on top of system 95, by the console & noticed bill said
give XEXEC.CTL JL license.  He probably meant WC license!!!!!
Sigh...
/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 1 Aug 85 2:21:55 PDT 
To: William R Soley <wrs@C39.Tymnet>, Joe Smith <jms@X930.Tymnet> 
Subject: WC vs JL 

Bill!!!!!!!!!
    WHat's up with switching licenses?  On 930, XEXEC Has everything
and the .CTL has WC.  That's the way it is with most of the OLDer
versions of XEXEC.  Why does :39 now use JL?  Are we deciding that
more people are allowed to phutz with the .CTL file now?  Sigh!!!
/Carl

What's up?
Received: from C39.Tymnet by X930.Tymnet; Thu, 1 Aug 85 7:07:31 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 1 Aug 85 6:58:29 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet> 
Subject: Re: WC vs JL 
In-reply-to: your message of Thu, 1 Aug 85 2:21:55 PDT

In the old version, XEXEC would bestow
license on a frame if the control file
asked for it.  Since it would bestow
anything up to and including WC, it
was appropriate for the .CTL file to
require WC (like DRWSPR.EPO).  But, now
XEXEC no longer does that (it was a
security violation, anyway because it
would automatically give license to a
possibly corrupted program) so it no
longer needs WC authentication.  Since
it does do JACCT-LOGIN type stuff it
should require that license.  The 
intent is not to relax it, just to make
the license fit the task.  (by the way
the LUD should have JL on it as a
tamper protection, as well.)  If you
think this gives too many people access
to the file, then look closely at those
that have JL but not WC (hopefully not
many) and fix that, since with JL they
could write their own XEXEC, anyway.
-Bill
Received: from C36.Tymnet by X930.Tymnet; Thu, 1 Aug 85 8:48:37 PDT
Return-path: <JMARCIN@C36.Tymnet> 
From: JMARCIN@C36.Tymnet 
Date: Thu, 1 Aug 85 8:39:05 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet 
Subject: Re: valnet-ncud1 
In-reply-to: your message of Wed, 31 Jul 85 18:50:32 PDT


Just wanted to be sure that you understand that the CUD and
MUDs that you are updating with VALNETNCUD1 are in a different
network, VALNET; you can access the network by logging
in to the username VALNET, and that will get you from either
Bubbnet or Tymnet into the valnet network. Once there isn't
much you can do...you can try setting a home host to 83,
the Netval host...other than that there are only 2 other
nodes, both supervisor machines.

Thanks for the testing.

I WOULD appreciate it if you tried to change a password:

Set a user to home host 83 and login to Netval to be sure the
name can access it. Then with your program, change the password and
try to login again...it usually updates the MUDs pretty quickly.

Let me know if you have any problmes...since we're in a test mode,
both Supervisors may not be updating...
Jill
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Thu, 1 Aug 85 18:59:36 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, LAGOLD@C39 
Subject: KTEH showing only 5/6 of two stories. 

PLANET OF THE DALEKS and INVASION OF THE DINOSAURS are 6 parts each, but
PREVIEW-54 shows PLANET as 5 parts ending August 7th and DINOSAURS at 5 parts
ending August 14th.  KTEH is currently checking with Lionheart to look into
the discrepancy.
/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 2 Aug 85 2:11:59 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: WC/JL et al, XEXEC-PCOM 

Bill,
  You brought up an interesting point about XEXEC putting whatever license
on a particular frame.  PCOM must have the same problem.  We've basically
been saying:  If I have license, I can write a program requiring it and set
a PCOM/XEXEC command file (.CTL) with that license, and then anyone using
my PCOM file would more-or-less have the license provided.   Well, as long
as I have the license, I can do that.  If someone tampers with the file, the
license goes away.   >> FINE <<

  But, what if someone "reads" the file, sees what programs it runs and
substitutes one of their own which does whatever THEY want it to do.  All
they need is WF.  PCOM v1.63 (not on all systems) and old versions of XEXEC
(again, not on all systems) supply the frame license.  The programs do all
the rest.

  We need a mechanism by which the PCOM/PERP/XEXEC program can verify the
programs that are to be run.  That's a tall order.  On the one hand, running
the current instantiation of a program is fine, checksums, author, etc...
So, I run a system program, it gets updated, should my PCOM job die?  Should
it lose it's license? (possibly causing it to die).  Should it be removed
from the queue?  [What programs does it verify? {{All,Some,None,System?}}]

  Then, the **smart** user can change the path (GFD, or GFD from a program)
and the program which gets run isn't even the one which was tested... we
lose again.

  This may be a hole we have to plug!
/Carl
Received: from C39.Tymnet by X930.Tymnet; Fri, 2 Aug 85 7:33:17 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Fri, 2 Aug 85 7:28:30 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: WC/JL et al, XEXEC-PCOM 
In-reply-to: your message of Fri, 2 Aug 85 2:11:59 PDT

Yes, its a real can of worms.  We have
to settle for some compromises, or we'll
end up with Gnosis.  Its basically the
Trojan Horse problem.  PCOM doesn't
really make it much worse.  After all,
how often do you check DIRIT or RPG to
see if its been tampered with before
you run it with gobs of license set?
Even if you did, all it takes is one
person to not check.  SIGH - thats too
heavy to think about on vacation, it
gives me a headache.  -Bill
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 2 Aug 85 17:08:03 UT
From: PMTS.C/STUTZMAN@Ontyme.Tymnet 
Date: 02 AUG 85 06:39:43 
To: TXS.PEAK@Ontyme.Tymnet 
Message-id: J20369@Ontyme.Tymnet 
Subject: An idea for an enhancement to PEAK. 

To:    Whoever reads TXS.PEAK

From:  Craig Stutzman

Re:    An idea for an enhancement to PEAK.
------------------------------------------------------------------------------

As a former VUE user, I was trying to do the "Invert Case in Region" function
in PEAK and discovered that it did not exist.  "No problem," I thought.  "I'll
just create my own macro and bind it to some key sequence."  Then I discovered
that it was impossible to even do this (unless I have overlooked something or
you have an unpublished PEAK function).

In my labors to create such a macro, what I needed was a new function similar
to this:  "Repeat the next function until mark (or end-of-line, paragraph,
window, page, etc.)"  Then, I could have defined a region in a macro and
repeated the "Toggle Case of Character" to the end of the region.  I'm sure
that this "Repeat" function would be very useful to create other nice macros.

I don't know if anyone is chartered to add enhancements to PEAK, but I would
vote to add this sort of "Repeat" function.

Thanks for listening.

Craig Stutzman.
Received: from C36.Tymnet by X930.Tymnet; Fri, 2 Aug 85 13:02:55 PDT
Return-path: <KEN@C36.Tymnet> 
From: KEN@C36.Tymnet 
Date: Fri, 2 Aug 85 12:40:00 PDT 
To: pmts.c/stutzman@ontyme 
Cc: carl@x930 
Subject: PEAK - Toggle case in region, etc 

    Craig,

    It seems clear that the easiest solution to the problem at hand, the
    inability to modify case within a region, is to provide a set of
    functions parallel to those availible for tokens and characters.  Since
    the logic is already designed, it seems little effort to provide these.

    However, I'm intrigued by your other suggestion, that of "repeat the
    follwing command until some condition takes place".  It would indeed
    expand the power of macros considerably, but needs some thought.  I'll
    keep in touch about it.

    Meanwhile, which system(s) do you work on?

    /ken dawson
Received: from C36.Tymnet by X930.Tymnet; Fri, 2 Aug 85 15:53:51 PDT
Return-path: <JMARCIN@C36.Tymnet> 
From: JMARCIN@C36.Tymnet 
Date: Fri, 2 Aug 85 15:49:07 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet 
Subject: Re: Password changes 
In-reply-to: your message of Fri, 2 Aug 85 2:44:17 PDT

Thanks for your help...I saw that you logged into Netval...hopefully
it should replace the 940 CUD updater over Labor day weekend ,
or later this year.

There should be no changes necessary when it comes up, since we
will just change the home system for NCUD1...I am assuming that
the program(s) that access the cud login to NCUD1 without a host.

If you can think of any other programs to test or have any questions,
let me know....thanks again...
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 2 Aug 85 16:04:31 PDT 
To: JMARCIN@C36.Tymnet 
Subject: Re: Password changes 
In-reply-to: your message of Fri, 2 Aug 85 15:49:07 PDT

I don't know if it's a good assumption to make that program(s) that
access the cud login to NCUD1 without a host.  Most [PASSWORD,etc.]
still have "NCUD1:1;" hardcoded into them.  It might be best to turn
off the colon option for username NCUD1 when you make the switch.

I can fix PASSWORD with a new release, but that has NOT been done as
yet.  Let me know... otherwise, I'll wait until there's something else
to change.   /Carl
From: LAGOLD@X930.Tymnet 
Date: Wed, 7 Aug 85 23:03:09 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Nosey me... 
In-reply-to: your message of Wed, 31 Jul 85 18:08:09 PDT

Carl -

Feel free to be nosy.

For a variety of reasons I'd rather not go into, my boss's boss is
requiring that I send my boss a message every day when I get in
and keep a log of my arrival times (said Section Mgr. is one of
these fascist types -- ex-NAVY -- who likes "punctuality").  I wrote
a quickie that does the logfile and use SEND to do the former.
What I was hoping to do was write a program which would automatically
send the message (cc'ing me) and make the appropriate entry in the
timestamp file.  I sort of realized that the files weren't going to
be much use; this is why I have abandoned the project for the time
being.  Besides, he may outgrow this phase of his before I could
find the right library files, etc., to use.

I had copied those files so I could telecopy them to 39.

Please forgive me if I was out of line.


Thanks,

--Lynn
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 8 Aug 85 2:07:35 PDT 
To: lagold 
Subject: Out of line? 

No No No.... nothing of the sort...  (actually, I wanted to make sure
that you didn't waste your time on something that didn't work anymore.)

WRS's RDMAIL & SENDMA & TUMS programs completely replaced those NBSMAIL
files...

However, on to your daily log of mail messages.  If you have a program
to run, or (if you know about PCOM) you can setup a command file which
can be periodically run.  PCOM has some of the TOPS-10 "MIC" features
in as much as there are substitution variables which you can set, and
there are system variables which contain things like, date/time... &
other info.

I've got an allergy appt tomorrow (thursday), so may be in to the office
late afternoon or working at home... (after 2pm).  If you'd like to give
me a call and discuss the details of what you're trying to do, I think
It may be possible without writing much of anything but a contrrol file.
/Carl

PS:  My office # is (116 tieline to Fremont) x 2515, and at home it's
     (415) 945-4314 ... feel free to call.
From: LAGOLD@X930.Tymnet 
Date: Thu, 8 Aug 85 9:25:21 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Out of line? 
In-reply-to: your message of Thu, 8 Aug 85 2:07:35 PDT

Thanks.  I know about PCOM and use it on a daily basis here for
things like compiling the interface I work on so I can do other
things at my terminal and also be left with a nice log of the
compilation.  If you have any documentation on PCOM (I've blown
it up a few times), I'd really appreciate pointers.

What I wanted to do was write a program which would
1) mail a message to my boss (for amusement, picking the text of the
   message out via a case statement and random number)
2) into a file on my directory called ARRIVE.LOG, append the date
   and time.

I wrote something using LOGLIB's routines which does the latter, but
didn't know where to look for the former.  I didn't want to use PCOM because
the amount of time it would take for the PCOM job to start up would cut
too much time off the time my boss would receive her msg.  Basically, the
situation is this: she'd rather not have to give a damn as to when I get in.
HER boss, however, wants her to keep strict track of it -- in other words,
just after the factory workers get to stop punching a timeclock, I "get" to
start.  Although there are zillions of holes in this (hacking LOGLIB to
subtract a few milliseconds, having the timestamp program ask me for my
"real" arrival time if it was after 9:30 (when I must be in by), etc.),
I promised Lin (my boss) that I'd be straight with her and let HER do the
covering up.  (Of course there's the easy man's way out -- logging in from
home as soon as I wake up and running the routine...)  It's not a pretty
situation, but if her boss wants me to do this shit, I plan on taking
as much time as I want in doing this kind of stuff.

Thanks,

--Lynn
Received: from C36.Tymnet by X930.Tymnet; Thu, 8 Aug 85 16:45:28 PDT
Return-path: <LINDLEY@C36.Tymnet> 
From: LINDLEY@C36.Tymnet 
Date: Thu, 8 Aug 85 16:40:14 PDT 
To: carl@930 
Subject: How are you? 

Carl:
Just wondered how you are feeling and doing.  Are you back driving?
Did you get an official OK from the doctor?  How is Cherie?  etc.

-Lois
From: Cherie Marinelli <Cherie@X930.Tymnet> 
Date: Thu, 8 Aug 85 21:05:32 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: plans... 


hi, sweetheart!

why don't we go and get the tent tonight? (if the Price Club is open, that
is.....) we should sit down and plan exactly what we're going to take, too.
by the way, "tonight" is Friday...... could you perhaps look at trying to
clean off the couch cushions this weekend? I really don't have any good ideas
as to how best to go about it...i love you very much and don't you forget it!

                                     - Mee
Received: from C26.Tymnet by X930.Tymnet; Fri, 9 Aug 85 10:31:40 PDT
Return-path: <DENCOFF@26.Tymnet> 
From: Dennis Coffey <DENCOFF@26.Tymnet> 
Date: Fri, 9 Aug 85 9:40:13 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: Qs. re. a couple of op. sys. utils. 

1.)  SYSTAT:  version on hosts is 61.6 (we think--result of VER on
program is "470061 021006"); and object code's files license varies:
most have WC HF, but a couple vary.  Do you know anything about 61.6
(not in archives, last archived is 61.5), and do you know what license
should be set on the file on all hosts?

2.)  (MPL)ICP:  What is proper protection:  AL RD RD, ALL RD NO or ALL
RUN RUN?  (This differs from host to host.)  What is proper license to
be set:  RC HF RF, RC HF R or WC RF HF?  (also differs host to host.)
Program not archived, transmittal not found.

3.)  (SRAUTIL)FTICP:  do we do anything with it?  Who would we contact
about this program (not in transmittal data base nor in our file
cabinet)?

4.)  DSKMAP:  no transmittals found; ver.  in transmittal data base is
1.1, version report by VER command on hosts is 2.56 ("032002 000056").
Do you know anything about sources, etc. for this one?

5.)  (SPL)SY:  apparently not transmitted:  nothing found in transmittal
data base or file cabinet.  Various checksums for .SAV file, but ver.
is consistent at 2.72.  Protection varies between ALL RD NO & ALL RUN
RUN.  License varies between:  none, RC SY and SY RF.  What do you know,
or where can I learn about this one?


Whether these operating system-related informational utilities are for
"general" use or for our section, Craig wants them archived and
transmitted to the hosts so they will be consistent for us and all
users.  Any of the above requested information will help us in doing
this.

	Thanks!
	D.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 9 Aug 85 17:34:58 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: XEXEC bug? huh??? 

I was looking at the .CTL file & wondering WHEN it scheduled stuff..
so, I put in:
    1,carl,pcom hourly
    24,carl,pcom daily
    24+1,carl,pcom daione
    24+2,carl,pcom daitwo
         ....
    24+23,carl,pcom dait23

well, OPER's time-zone is Pacific, so is CARL's...

the log shows:
    00:00
	PCOM DAILY on job 7
    23:00
	PCOM DAIONE on job 7
	PCOM DAITEN on job 11
	PCOM DAIELV on job 12
	    ....
	PCOM DAIT19 pn job 22
    22:00
	PCOM DAITWO on job 7
	PCOM DAIT20 on job 1
	    ....
	PCOM DAIT23 on job 10
    21:00
	PCOM DAITHR on job 7
    20:00
	PCOM DAIFOR on job 6
    19:00
	PCOM DAIFIV on job 5
    18:00
	PCOM DAISIX on job 7
    17:00 ....

Huh?  Is this right?  It's actually backwards.... like I had said
24-n,carl,pcom FOOOhh

/Carl
Received: from C36.Tymnet by X930.Tymnet; Mon, 12 Aug 85 13:48:45 PDT
Return-path: <KDAWSON@C36.Tymnet> 
From: KDAWSON@C36.Tymnet 
Date: Mon, 12 Aug 85 13:35:11 PDT 
To: carl@930 
Subject: 132-column redisplay 

    When I added "Tym431 (aka ADM-11)" to Peak's redisplay, I did a differ
    with your SYDISP.SAI, and found the latter to contain code to support
    the Vt102's 132-column screen.  My question is whether this was ever
    proven to work.  If so, I should pull it over to Peak's redisplay.

    Meanwhile, the Tym431 (NTD's "new"???  terminal of choice) is
    essentially an ADM3A with a Clear-to-End-of-Line sequence equivalent to
    the 420 and 425.  I'd like to update SY at least on :36 and :930 with
    this type some time soon.

    /ken
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 12 Aug 85 17:01:17 PDT 
To: Dennis Coffey <dencoff@C26.Tymnet> 
Subject: SPOOL TAPE PROLEM 

DENNIS,
  I TRIED IT & IT WORKS FINE ON 930...

SOMETHING ELSE IS WRONG...

PLEASE HAVE OPERATIONS CUT A SPOOL TAAPE WITH A FEW REQUESTS ON IT
WRITTEN AT 1600 BPI AND WE WILL TRY TO DUPLICATE THE PROBLEM HERE.
OTHERWISE, MAYBE I CAN GO THERE... WE WILL SEE.

/CARL
Received: from C36.Tymnet by X930.Tymnet; Mon, 12 Aug 85 17:26:23 PDT
Return-path: <KEN@C36.Tymnet> 
From: KEN@C36.Tymnet 
Date: Mon, 12 Aug 85 17:19:34 PDT 
To: carl@930 
Subject: fdm & rpg & mexec ??? 

    I put FDM up on (SPL).  What license should it have?  (SPL) is on my do
    list at at some point, and I'm set up for MEXEC.  When I say
    "fdm<crlf>", I get "Invalid Command."  from v0.7 of FDM.  But when I
    say "do fdm<crlf>" I get into the program and get a proper prompt.  Any
    idea why this is?

    /ken
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 13 Aug 85 1:46:49 PDT 
To: KEN@C36.Tymnet 
Subject: Re: fdm & rpg & mexec ??? 
In-reply-to: your message of Mon, 12 Aug 85 17:19:34 PDT

Well,
  that's one of the things that doesn't work.... it uses a "new"
command scanner that tries to read the monitor command.  I wanted
FDM<cr> to give a prompt, but FDM command<cr> to work... sigh at
the moment, neither do...  So, DO FDM<cr> or R (direcotry)FDM<cr>
or FDM<cr>  [wait for error msg] START<cr>  all work fine.
/Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont) 
Date: Fri, 16 Aug 85 17:41:11 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: STOPCDs don't continue if the base gets impatient. 

We got the CTY output from C36 from when Carl was testing INFO stopcodes
on the KL.  With the latest patch, the stopcode is output correctly, and
the system continues, but at the next clock tick it notices that nonzero
has been deposited in 30, and reloads the monitor.  In the first test,
30 contained 000404,,003337 and in the second test it contains 001070,,002177.

It appears that the base got upset that KEY620 was not being updated while
the stopcode information was being output.

The long-term solution is to tell the new base code that the PDP-10 is
experiencing a stopcode, and that it should not expect KEY620 to be updated
for 90 seconds.  A short-term hack would be to modify the routine that
outputs to the CTY and have it update KEY620 while waiting to send the
next character.

/JMS
Received: from C39.Tymnet by X930.Tymnet; Tue, 20 Aug 85 16:47:20 PDT
Return-path: <WRS@C39.Tymnet> 
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 20 Aug 85 15:24:03 PDT 
To: carl@930, osman@930, Wayne D. Bartlett <BARTLETW@C39.Tymnet>,
	nss.e/riordan@ontyme, matoka@897, ntd.a/newman@ontyme, jmarcin@36,
	ntd.s/schramm@ontyme 
Subject: I'm back (sigh) 

Well, here I am.  Ready for another vacation.  I know I've promised various
of you that I'd do various things when I get back, but I've forgotten most
of them, I'm sure.  So, please, let me know if you are expecting anything
and I'll probably get to it next week after attacking my "IN" basket (with
a front-end-loader).  Thanks for your patience, its good to be back (really).
-Bill
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 20 Aug 85 19:55:00 PDT 
To: Monitor
Subject: Future dates! 

Osman got a complaint from MARCONI about a file in his directory which
had a date sometime in the future.  It was one of the MAGNRN.SHR files
probably copied or restored from tape into (MARCONIT).  Osman wanted
to know if we could explain how the date got set to the future...

Well,  the ENTER & CHANIO .CHENT calls both check the date value and
set it to today's date [THSDAT] if it is in the future.

But,  both RENAME (probably .CHREN too) and UPDATE-ENTER don't seem
to care at all.  We were able to set the date to any date we please
past or present WITHOUT ANY LICENSE.

This isn't really a problem, but unless someone knows of a good reason
why any file UUO can set the date/time field of a file to the future,
I vote that we should put the appropriate checks in the appropriate
places and stop this fiction before anyone else complains.
/Carl
Received: from C39.Tymnet by C930.Tymnet; Thu, 30 May 85 10:17:54 PDT
Return-path: <LINDLEY@C39.Tymnet> 
From: LINDLEY@C39.Tymnet 
Date: Thu, 30 May 85 10:15:19 PDT 
To: carl@930 
Subject: new? me 

As you can probably tell, my new username is LINDLEY.  Would you give
me an account on 930, if that is all right.  Useful for finding out
information from info, etc.

Did you tell Cheri that I have the saleslip from the gift if she would
like something else?  Just let me know and I can send it interoffice.
I want you to have something you want, so don't mind in the least if you
exchange it.

LINDLEY is valid on 36 and 39, for sending mail or whatever.
-Lois
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 21 Aug 85 5:07:15 UT
From: NTD.D/CHANG@Ontyme.Tymnet 
Date: 20 AUG 85 18:11:52 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.C/BALTRUNAS@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet, TXS.@Ontyme.Tymnet 
Message-id: I33687@Ontyme.Tymnet 
Subject: Phantom Jobs to PDP-10s 

TO:  Carl Baltrunas
FR:  David Chang
RE:  Phantom Jobs to PDP-10s
DA:  August 20, 1985
CC:  Bill Soley, Gazel Tan

====================================================================

I am in the process of preparing the Supervisor for an all T-II
network for PUBNET.  I understand that currently the Supervisor builds
phantom jobs to PDP-10s to get them started, but that there exists a
mechanism (eg. adding PJ PAM to XEXEC or something) for the PDP-10s to
come up by themselves.  Please be prepared for this eventuality in the
near future, say late September or early October, because modifications
are being made to eliminate the phantom jobs.
 
If there are any problems, please let me know.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 1 Oct 85 11:55:29 PDT 
To: Osman Guven <osman@X930.Tymnet> 
Subject: I'm on my way...to the office. 


From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 11 Oct 85 11:55:47 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: Tonight 

Are you coming over to see new WHO tonight?  Sally needs advance warning.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 23 Oct 85 18:17:39 PDT 
To: Cherie Marinelli <Cherie@X930.Tymnet> 
Subject: ILY! 

...and you seem to have forgotten?
     so, this is a reminder!
/Quack
From: Cherie Marinelli <Cherie@X930.Tymnet> 
Date: Thu, 24 Oct 85 8:17:01 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet> 
Subject: re:"ily!" YLM? 


I haven't forgotten; i simply find it difficult to believe, when you tell me
to "fuck off" in public. "Fuck off" means to leave the person alone. So I
have. This rejects me as much as my pushing you away rejects you. Think
about it. -Cherie
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 24 Oct 85 11:44:21 PDT 
To: Cherie Marinelli <Cherie@X930.Tymnet> 
Subject: re:"ily!" YLM? 
In-reply-to: your message of Thu, 24 Oct 85 8:17:01 PDT

Yes! ILY.
  You seem to have this propensity for thinking only of yourself.  If you
think about it... you started all of this.  YOU got hysterical about the car
and yelled at me while I was trying to ask you which "GEORGE" you were talking
about... I tried to help you to calm down, that didn't work, and you just
yelled again.  So, damned if I do and damned if I don't, I yelled back.  The
way you were acting, "fuck off" was pretty calm but appropriate...

  If you think it's a rejection, then you better think about when and why
I said it, 'cause you were acting like a jerk at the time.  (and by the way
a deserted gas station IS NOT nearly "in-public", you can really lay it on
thick, knowing fully well the problems I have about feeling guilty, even
when I'm not).

  Now that you're "calm" you feel that my comments in the heat of an
argument are "inappropriate".. well, that just TOO BAD!

  As far as I'm concerned, it should have been forgotten.  But, NO.. you
won't drop it.  You have to push it and blame me for it all, and make sure
that the "rejection" message gets through to me.  Well, it worked... on
Sunday night...  but I chose to try to forget it...

  But you better straighten yourself out dearie, 'cause "IF" we don't start
having some kind of cooperation and consideration being shown, you can
forget any relationship.  You've been in a piss-ass mood since Sunday, and
as far as I'm concerned, it's your own damn fault.  So, ship up or shape out!

  And I'm tired of the misery, yours and mine.  If I didn't love you, then
I wouldn't care, but I do, and I do.  So there!

/Quack
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Mon, 7 Oct 85 17:07:33 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 07 OCT 85 09:11:31 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J47739@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-149
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 02 Oct 85



To:       ISG Lists A - D

Subject:  ADMINISTRATIVE BULLETINS (AB's)



1.  The following guidelines are established regarding the use 
    of AB's within ISG:

          Administrative Bulletins (AB's) will be used 
          throughout ISG as the means of announcing organization 
          changes, promotions, new or revised policies requiring 
          immediate implementation and other administrative 
          matters of substantial importance.  Two types of AB's 
          will be used:

          -   ISG AB's - Issued by ISG HQ/Group Staff

          -   ISG Company AB's -  Issued by ISG Business 
                                  Units/Organizations reporting 
                                  directly to the Group 
                                  Executive Officer or the Group 
                                  Operating Officer.

          All AB's should be drafted on the appropriate AB form, 
          signed by a Director or higher level and ready for 
          issuance (excluding AB number and date) and forwarded 
          to the issuing organization.  All ISG AB's involving 
          policy will require ISG-HQ signature.  All company 
          AB's involving policy will require signature of 
          Company Head.  AB's which identify newly assigned or 
          revised reporting relationships should include the 
          signature of the principal to whom these personnel 
          will report.

          The issuing organization will assure that all position 
          titles used in the AB are correct and will verify with 
          Human Resources that all announcements of promotions 
          or other title changes have been approved prior to 
          release of the AB.

          AB distribution should be customized whenever feasible 
          to include the specific Management Levels and 
          combination of ISG and other MDC organizations that 
          are impacted by the information being communicated.  
          Examples:  (1) For organization change in a Company 
          Fiscal area, Dist.--Company Levels A-E; (2) For change 
          in Cafeteria hours ISG Campus, Dist.--ISG and 
          MDAIS-St. Louis, Levels A-H; (3) For new Director 
          appointment ISG Group, Dist.--All ISG levels A-D.



          An ISG AB is required:

          -   when an ISG company announcement includes 
              concurrence signature(s) of ISG HQ or ISG Group 
              Staff principals,

          -   for all announcements of a policy nature which 
              materially impact any ISG company other than the 
              issuing organization,

          -   for all announcements of Vice President or higher 
              level promotion (distribution to include MDC 
              levels A through D).


          ISG AB's

          Responsibility for issuance is assigned to ISG Policy 
          Management.

          These AB's will be issued on ISG AB stationery. The AB 
          number will consist of a prefix - "ISG" followed by 
          the year (2 digits) and a single consecutive number, 
          i.e. - ISG-85-XXX.  (All numbers to be assigned by ISG 
          Policy Management.)


          ISG Company AB's

          Responsibility for issuance is assigned to each 
          applicable ISG Company head, reporting directly to the 
          Group Executive Officer or the Group Operating 
          Officer, or their appointed representative.  Each 
          company shall inform ISG Policy Management of the 
          individual assigned this responsibility.  It is 
          recommended that this be the same individual 
          responsible for handling policy coordination for each 
          ISG Company.

          These AB's will be issued on ISG Company AB 
          stationery.  The AB number will consist of the 
          specific company appreviation as a prefix followed by 
          the year (2 digits) and a single consecutive number, 
          i.e. - For "Health Systems", HS-85-XX.  Each ISG 
          Company shall include ISG Policy Management in the 
          distribution of their AB's.



2.  Please assure that communication of AB type information 
    within your area of concern is accomplished by an AB and is 
    prepared in accordance with the requirements as stated 
    above.  All ISG AB's should be forwarded to ISG Policy 
    Management-St. Louis, Dept. L089, Bldg. 301 for issuance and 
    a copy of each ISG company AB should be sent to them for 
    information.  Please call Ext. 36113 if you have any 
    questions on this subject.



(Original signed by R. D. Greco)

R. D. Greco, Director
Information Systems/Policy Management


Concurrence:



(Original signed by R. A. Fischer)

                            
R. A. Fischer
Group Executive Officer
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 8 Oct 85 1:06:07 UT
From: FSC.L/ROSENTHAL@Ontyme.Tymnet 
Date: 07 OCT 85 14:19:22 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J48169@Ontyme.Tymnet 
Subject: MICOMS Procedure File Update 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      07 OCT 85  14:17

TO>        MICOMS Users

COPIES>    

FROM>      Linda Rosenthal


SUBJECT>   MICOMS Procedure File Update


-----------------------------------------------------------------------


An  updated version of the procedure file used to logon to MICOMS with 
an IBM PC has been placed in an OnTyme shared file named  ** PROCFILE.
To  receive  and  edit a copy of the procedure file to  be  used  with 
MICOMS, take the following steps:

1.  Log onto the OnTyme system using Tymcomm in your usual manner.

2.  To get the procedure file into your OnTyme workspace type
    :GET ** PROCFILE

3.  To view the procedure file enter
    :TYPE

4.  Edit the procedure file using the OnTyme editing features:
    --if your computer is hardwired, delete the first two lines of the 
      procedure file
    --if  using a dial-up modem,  change the phone number on the first 
      line of the procedure file
    --on  the line that reads "SEND signon mi123456 mi123456" you will 
      need to replace 123456 with your employee number

5.  To  save this file to your diskette/hard disk enter the  following 
    commands.   An explanation of the following steps can be found  on 
    pages  A-17  to A-19 of the TymComm booklet "Tym/COMM For The  IBM 
    Personal Computer," Version 2.
    
    COMMAND TO TYPE:                 SYSTEM RESPONSE:

    :LOAD ON <ENTER>                 Accepted
                                     --Commands   entered  from   this 
                                       point  on  will not display  on 
                                       the screen

    :TYPE <ALT> <R>                  Enter PC Fileid For Recording:
    
    PROC1.TYM/B <ENTER><ENTER>       The  procedure file will  display 
                                     on  the screen and will  also  be 
                                     saved  to  your disk.   When  the 
                                     procedure   file  is   completely 
                                     displayed continue with the  next 
                                     step:

    <ALT> <C>                        Recording File Closed

    :LOAD OFF <ENTER>                Accepted

    :ERASE <ENTER>

6.  Logoff OnTyme in your usual manner. 

7.  The  procedure  file will be on your diskette/hard disk under  the 
    filename Proc1.Tym.

If you have any questions,  contact Linda Rosenthal, MICOMS instructor 
at 415-794-2557.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 8 Oct 85 21:06:17 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 08 OCT 85 10:04:44 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J48805@Ontyme.Tymnet 
Subject: "M C D O N N E L L D O U G L A"... 

                    M C D O N N E L L   D O U G L A S 
 
          D I S T R I B U T E D    S Y S T E M S   C O M P A N Y 
 
 
 
TO:       ALL ISG BUSINESS UNITS AND SALES OFFICES 
 
 
FROM:     Jan Giordano 
 
 
***************************************************************************** 
 
                           A N N O U N C I N G 
 
                       N E W   P R I C I N G   O N 
 
                 I B M    P C   P R O D U C T    L I N E  
 
**************************************************************************** 
 
EFFECTIVE 0CTOBER 15, 1985... 
 
     New pricing is available from Distributed Systems for  
     IBM PC, PC/XT and PC/AT delivered in approved VAD applications. 
 
     FEATURING... 
 
        ******************************************** 
 
          MINIMUM 33% OFF LIST FOR PC'S AND XT'S 
          ON QUANTITY ONE! 
 
 
          OVER 40% DISCOUNT OFF LIST AVAILABLE ON 
          VOLUME PURCHASES!
 
        ********************************************** 
 
V A D ???? 
---------- 
 
VAD is the VALUE ADDED DEALER arrangement between McDonnell Douglas and
IBM which allows any ISG Business Unit to remarket the IBM PC Product
line to their customers as long as it includes an approved VAD
offering. 
 
There are several approved VAD offerings within McDonnell Douglas.  The
most general being that McDonnell Douglas can remarket an IBM PC as long
as the application includes connection through a McDonnell Douglas
network (i.e., TYMNET) to a host computer. 
 
 

INTERESTED IN RECEIVING MORE INFORMATION??? 
------------------------------------------- 
 
If you have previously requested to be on DSC's mailing list to receive
pricing information, you can expect to receive the new Product &
Pricebook during the week of October 21, 1985. 
 
If you have not made a request and would like to receive the Product &
Pricebook, send an Ontyme to  Marian Lawrence at DSD.M/LAWRENCE. 
 
************************************************************************* 
 
                    M C D O N N E L L   D O U G L A S 
 
           D I S T R I B U T E D   S Y S T E M S   C O M P A N Y 
 
                              A Service of 
 
               MCDONNELL DOUGLAS COMPUTER SYSTEMS COMPANY 
 
****************************************************************************
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 10 Oct 85 1:11:13 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 09 OCT 85 14:13:17 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J50113@Ontyme.Tymnet 
Subject: An International Opportunity 

                         M E M O R A N D U M

DATE>       08 OCT30 12:24                             []  MDCISI Pacific Rim

TO>        All

COPIES>    Tommy Arnett Mike Rebmann

FROM>      Harry Matthews


SUBJECT>   An International Opportunity

---------------------------------------------------------------------       

McDonnell Douglas Information Systems International currently has an
exciting  career opportunity available for a Project  Manager.  MDCISI 
Pacific  (formerly Tymnet International) has an opening for  qualified
individual  who  has  the drive and ambition to succeed and  wants  to 
become part of the finest implementation team Tymnet ever assembled!

 Qualifications required: 

                - previous project management expirience
                - prior line management expirience
                - effective verbal, written, presentation and
                  listening skills
                - previous system delivery and project team
                  membership advantageous
                - broad knowledge of entire Tymnet product line
                - well rounded data communications background
                - desire for extended overseas assignments
                - previous position with extensive customer 
                  interface


   Duties and Responsibilities:

                - planning   and   managing  the   onsite   activities                                            
                  associated  with international network implemntation                                               
                - writing detailed project plans
                - reporting progress against the plan
                - planning network expansion and subsequent 
                  deployment
                - analyzing problems and recommending timely
                  solutions
                - performing budget analysis and tracking project
                  costs
                - ability to coordinate efforts of several
                  departments and individuals within Tymnet, the
                  customer and local agents



   Qualified individuals should first contact your manager and clearly 
state intensions to announce your candidacy.  Secondly,  send a ontyme 
to me   followed very shortly by a resume' to my office.

                    Harry Matthews
                    Manager, International
                    Project Implementation
                    Tymnet, Inc.
                    2710 Orchard Parkway
                    San Jose, Ca. 95134
                    (408) 942 5279


   I look forward to hearing from you.



   Sincerly;


   Harry Matthews
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 10 Oct 85 1:11:31 UT
From: FSC.TRAINING@Ontyme.Tymnet 
Date: 09 OCT 85 15:52:12 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J50238@Ontyme.Tymnet 
Subject: "THE COURSE SCHEDULE FOR TECHNICAL"... 

THE COURSE SCHEDULE FOR TECHNICAL TRAINING NORTH (FREMONT)
FOR THE PERIOD JANUARY 1986 TO JUNE 1986 HAS BEEN FILED
AS A PUBLIC FILE.  TO ACCESS THIS INFORMATION TYPE
  :READ ** TRNGSCHED.NTH/86
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 10 Oct 85 17:06:32 UT
From: FSC.A/BERRY@Ontyme.Tymnet 
Date: 10 OCT 85 09:33:15 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J50723@Ontyme.Tymnet 
Subject: "NEW ENGLAND THIRD PARTY HAS MOVED."... 

NEW ENGLAND THIRD PARTY HAS MOVED.
THE NEW ADDRESS IS----
MC DONNELL DOUGLAS FIELD SERVICE COMPANY
12 MANOR PARKQDWAY, SUITE B
SALEM,
NEW HAMPSHIRE   03079

PHONE
603-893-3400
RGDS
AL BERRY
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 11 Oct 85 17:09:29 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 11 OCT 85 09:08:50 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J51676@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-150
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 10 Oct 85



To:       ISG Lists A - E

Subject:  FORMATION OF MCDONNELL DOUGLAS ELECTRONIC MESSAGING 
          SYSTEMS COMPANY



I am pleased to announce the formation of the McDonnell Douglas 
Electronic Messaging Systems Company.  MDEMS combines two 
existing businesses, FTCC and OnTyme, to aggressively pursue a 
larger share of the Electronic Messaging Market.  Combined 
revenue of FTCC and OnTyme in 1985 is $30 Million and the 
planned 1986 revenue goal for MDEMS is $40 Million.  This new 
unit will be a product organization and will rely on other ISG 
business units for the distribution of its products.

One key element of the 1986 MDEMS sales plan is to encourage all 
ISG business units to sell these messaging products.  At the 
present time, the bulk of the messaging revenue originates from 
the Diversified Systems Company and FTC Communications.  These 
organizations will continue to aggressively sell these products, 
but MDEMS will begin working with the other business units to 
adapt the products to their markets.

During the remainder of 1985, the MDEMS organization structure, 
business plan and 1986 financial operating plan will be 
developed to enable us to implement our messaging business 
strategy.

MDEMS will be managed by Terry Donaher, Vice President of 
Network Applications for the business and Network Systems 
Company and will report to Warren Prince, President of McDonnell 
Douglas Business and Network Systems Company.



(Original signed by G. E. Liebl)

Gary E. Liebl
Group Operating Officer
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 11 Oct 85 17:10:52 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 11 OCT 85 09:21:54 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J51692@Ontyme.Tymnet 
Subject: NEW CHANGES FOR AP 

                     M E M O R A N D U M
                                                     FINANCE DIVISION



DATE>      10 OCT 85  10:15

TO>        THE WORLD

COPIES>    

FROM>      MARSHA OLSON-FORD
           AP SUPERVISOR


SUBJECT>   NEW CHANGES FOR AP


-----------------------------------------------------------------------


THE FOLLOWING IS A CURRENT EMPLOYEE LISTING OF RESPOSIBILITIES AND PHONE
NUMBERS FOR ACCOUNTS PAYABLE.




DICK SARGENTI           X7260
                        DISBURSEMENTS MANAGER

MARSHA OLSON-FORD       X6421
                        ACCOUNTS PAYABLE SUPERVISOR

CLARK MOSES             X6053
                        SPECIAL ASSIGNMENTS

LAURA MESSINEO          X6427
                        ACCOUNTS PAYABLE LEAD FOR ALL PROCESSORS
                        REAL ESTATE
                        FAST PAYS
                        BENEFITS
                        MONTHLY REPORTS

                        JUDY VANT                X7723
                                                 PROCESSES A-B

                        DEBBIE BOITANO           X6429
                                                                Page  2

                                                 PROCESSES C-E

                        NANCY NILES              X7943
                                                 PROCESSES F-L
                                                 STANDARD PAYS, TAXES

                        ROSS REYES               X7871
                                                 PROCESSES M-Q

                        NANETTE DELUMPA          X7953
                                                 PROCESSES R-T

                        JOHN CIERNICK            X7169
                                                 PROCESSES U-Z

                        CLARIECE COOPER          X6707
                                                 PROCESSES MICROBAND
                                                 BILLING SETTLEMENTS (TYMNET)

DEBBIE TAFF             X7952
                        ACCOUNTS PAYABLE COORDINATOR:
                        RESPOSIBLE FOR FREIGHT,UTILITIES,VENDOR BOOK
                        AND CHECK DESK AREA
                        PROCESSES PHONES

                        JUANA TORRES             X6653
                                                 PROCESSES FREIGHT
                                                 PROCESSES UTILITIES

                        SOCORRO DE LA SERNA      X6668
                                                 VENDOR BOOK
                                                 MAIL

                        JIM BIAGI                X6606
                        ROGER SLOWEY             CHECK  DESK AREA

TRISH PRYOR             X6052
                        RESPONSIBLE FOR RECORDS ROOM

                        LORETTA ROMERO           X6052
                                                 RECORDS ROOM
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 11 Oct 85 21:07:30 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 11 OCT 85 12:15:56 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J51901@Ontyme.Tymnet 
Subject: PSM BACKPLANE 

     
     
  TO:  NFEALL  
FROM:  NTS
SUBJ: PSM BACKPLANE 
     
   THIS MEMO IS TO LET THE FIELD KNOW OF SOME PROBLEMS WE HAVE   
BEEN HEARING ABOUT ON THE PSM BACKPLANE. THE BACKPLANE PINS ARE  
VERY EASILY BENT IF THE CARDS ARE NOT PUT IN PROPERLY. YOU HAVE  
TO INSERT THE CARD SLOWLY UNTIL YOU FEEL THE PINS SEAT IN THE    
CONNECTOR, THEN, PUSH IT IN THE REST OF THE WAY UNTIL SEATED
COMPLETELY. IF YOU DO NOT DO IT THIS WAY YOU ARE LIKELY TO BEND  
THE PINS AND THE CARD WILL NOT FUNCTION PROPERLY. IF YOU DO BEND 
SOME PINS, TURN THE POWER OFF TO THE PSM CARD CAGE AND TRY TO    
STRAIGHTEN OUT THE PINS. IF YOU ARE UNABLE TO DO THIS, OR, THERE 
ARE TOO MANY PINS BENT, YOU CAN NOW ORDER JUST THE BACKPLANE INSTEAD  
OF THE WHOLE CARD CAGE. THE PART NUMBER IS 430190-001. IF YOU HAVE    
ANY QUESTIONS ON THIS PLEASE FEEL FREE TO CALL NTS.    
     
THNKS
DON
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Sat, 12 Oct 85 5:05:39 UT
From: FSC.K/BARNES@Ontyme.Tymnet 
Date: 11 OCT 85 14:48:56 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J52085@Ontyme.Tymnet 
Subject: NEW EMPLOYEES - FSC SPARES ADMINISTRATION 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      10 OCT 85  10:44

TO>        DISTRIBUTION

COPIES>    

FROM>      KEN BARNES


SUBJECT>   NEW EMPLOYEES - FSC SPARES ADMINISTRATION


-----------------------------------------------------------------------



I WOULD LIKE TO TAKE THIS OPPORTUNITY TO WELCOME ABOARD OUR NEWEST ADDITIONS
TO THE FSC SPARES ADMINISTRATION ORGANIZATION.  THEY HAVE JOINED US THROUGH
TRANSFERRING FROM THE FIELD OPERATIONS.  UP TO NOW THEY HAVE BEEN INVOLVED
WITH MATERIAL SUPPORT OF THE ISO 3RD PARTY MAINTENANCE SUPPORT FOR
MCDONNELL DOUGLAS FIELD SERVICE COMPANY.

PLEASE JOIN ME IN WELCOMING TO OUR STAFF -

                  JAYLEE KENNEDY, NORRISTOWN, PA
                  ANITA MEYER, DALLAS, TX
                  KAREN MULACEK, LISLE, IL
                  LES ROGERS, FREMONT, CA
                  STEVE ZORNES, LANHAM, MD

SPARES ADMINISTRATION IS A KEY GROUP WITHIN THE MATERIAL SUPPORT ORGANI-
ZATION.  OUR MAIN RESPONSIBILITY IS TO PROVIDE SUPPORT AND MANAGEMENT OF
SPARES INVENTORY THROUGHOUT FIELD SERVICE.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Sat, 12 Oct 85 5:06:08 UT
From: FSC.D/BILAR@Ontyme.Tymnet 
Date: 11 OCT 85 19:30:40 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J52289@Ontyme.Tymnet 
Subject: "memomemomemomemomemomemomemomemomemomemomemomemomemomemomemomemo"... 

memomemomemomemomemomemomemomemomemomemomemomemomemomemomemomemo
e                                                              m
m            m       m  eeeeeee  m       m  oooooooo           e
o            mm     mm  e        mm     mm  o      o           m 
m            m m   m m  e        m m   m m  o      o           o
e            m  m m  m  eeeeee   m  m m  m  o      o           m
m            m   m   m  e        m   m   m  o      o           e
o            m       m  e        m       m  o      o           m
m            m       m  eeeeeee  m       m  oooooooo           o 
e                                                              m   
memomemomemomemomemomemomemomemomemomemomemomemomemomemomemomemo   
================================================================
                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
================================================================

                     OCT.-11-1985 0331 G.M.T.

================================================================

             TO:   ** Acctusers  (fsc)

             FROM:  David J. Bilar, Bubbnet (Tymnet-WNFE)

             SUBJECT:  Physical Inventory of Tymnet Engines
                       and Equipment

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

     As most all of you should know by now, we are being put
   through another inventory. This may or may not be a problem
   to you unless you run out of forms. Should anyone have not
   pulled unformatted copies from sys.54 and reformatted them
   to some readable state, or if you've had just about enough
   of your Great Expectations being ruined by your employment
   at the Counting House, even if you feel that life is pre-  
   sently sending you through Fields of positively charged 
   Copper, and you're a proton, I have a series of files designed  
   to take care of such an emergency.

     All it takes is a phone call to (408) 446-6149 8:30-5:30 <---P.S.T.
   Monday-Friday, or an ontyme to fsc.d/bilar. If given 0.5 day
   warning, I will have your copies sent out via ontyme or
   interoffice (with proper address) by the next morning. If
   anyone hasn't started yet, this stuff might be of some help, no?

     The files include instruction summary, all 3 forms, and all 
   3 sets of instructions. They are set up on 1 page each,
   where possible.

     Let me know If i can be of some help.



                                      Reguards,

                                          David J. Bilar
                                          Field Engineer-Trainee
                                          WNFE-BUBB
                                          Bubb rd., bldg."D"
                                          Cupertino, Ca.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Wed, 16 Oct 85 1:06:50 UT
From: FSC.J/LORTS@Ontyme.Tymnet 
Date: 15 OCT 85 16:17:10 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J54426@Ontyme.Tymnet 
Subject: "TO: ALL FIELD PERSONEL FR: N.D.C."... 

TO:  ALL FIELD PERSONEL
FR:  N.D.C.
SUB: PARTS BOXES

    THE BOXES USED FOR TERMINAL PARTS ARE
RE-USABLE.  PLEASE USE THESE BOXES FOR RETURNING
DEFECTIVE PARTS FOR REPAIR.  THEY ARE SPECIFICALLY
DESIGNED TO PROTECT THE PARTS DURING TRANSPORTATION.
YOUR CO-OPERATION WILL BE GREATLY APPRECIATED.


                          THANK YOU,
                             N.D.C.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Mon, 21 Oct 85 17:11:24 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 21 OCT 85 09:28:45 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J57919@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-155
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 11 Oct 85



To:       ISG A to E

CC:       L. Ault (MDAC-HB), J. N. Jeng (DAC), F. A. Westhoff 
          (MCAIR)

Subject:  San Jose/Santa Clara, California Hotel Accommodations



1.  Information Systems Group has obtained special accommodation 
    rates with the Marriott Hotel, Santa Clara and the Embassy 
    Suites, Santa Clara for MDC employees.

2.  The Marriott Hotel is located at Highway 101 and Great 
    American Parkway (telephone (408) 988-1500); special MDC 
    rate for a hotel sleeping room is seventy-nine dollars 
    ($79.00) per night, single or double, Monday-Sunday.  The 
    rate is net/non-commissionable and applicable tax is not 
    included.  Reservations should be made by the MDC Authorized 
    Agency or individual directly with the Hotel Reservation 
    Department.  Reference to the special MDC rate should be 
    made when making reservations.  Billing procedures (paid for 
    individually upon check-out or billed to MDC) will be 
    established at the time the reservation is being made.  This 
    special rate is not a guaranteed space agreement and 
    therefore accommodations are subject to availability.

3.  The Embassy Suites location is 2885 Lakeside Drive (Highway 
    101 and Bowers) (telephone (408) 496-6400) special MDC rate 
    is eighty dollars ($80.00) single, ninety dollars ($90.00) 
    double, Monday thru Thursday per night, and sixty-nine 
    dollars ($69.00) up to four (4) people Friday thru Sunday 
    per night.  The rate is net/non-commissionable and 
    applicable tax is not included.  These rates include the 
    standard amenities of a two (2) room suite, complimentary 
    made-to-order breakfast every morning, complimentary 
    refreshments each evening, complimentary limousine service 
    to and from San Jose, California airport and a 'no tipping' 
    policy.  Reservations should be made by the MDC Authorized 
    Agency or individual directly with the Embassy Suites 
    Reservation Department.  Reference to the MDC special rate 
    should be made when making reservations.  Billing procedures 
    will be established at the time the reservation is being 
    made.  These special rates are not guaranteed space 
    agreements and therefore accommodations are based on 
    availability.



4.  These special rates are effective through 31 December 1985 
    at which time volume will be assessed and we may be in a 
    position to make the commitment necessary to get lower rates 
    in 1986.  These agreements provide excellent accommodations 
    in two (2) of the finer hotels in the San Jose airport 
    vicinity.  However, if you find any kind of problem with the 
    service you receive while staying at the hotels, a call to 
    Mike Tipton, McDonnell Douglas Computer Systems Company, 
    (408) 435-7606, Purchasing Department, will be all that is 
    required for us to follow-up with hotel administration.

5.  All MDC personnel are encouraged to take full advantage of 
    these offers while in the San Jose/Santa Clara area.



(Original signed by D. J. Garrity)

D. J. Garrity
Staff Vice President
Administration - ISG
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Mon, 21 Oct 85 21:16:51 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 21 OCT 85 11:20:37 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J58066@Ontyme.Tymnet 
Subject: ACS NEWSFLASH 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      21 OCT 85  11:19

TO>        ALL FSC USERS

COPIES>    

FROM>      FSC.SUP


SUBJECT>   ACS NEWSFLASH


-----------------------------------------------------------------------


The monthly ACS Newsflash has been made a shared file.  If you are
interested in reading it, type :READ ** ACS.NEWSFLASH
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 24 Oct 85 1:09:37 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 23 OCT 85 17:06:14 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J60505@Ontyme.Tymnet 
Subject: ONTYME USAGE 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      23 OCT 85  17:05

TO>        ALL FSC ACCOUNT USERS

COPIES>    

FROM>      CARMEN REYES (FSC.SUP)


SUBJECT>   ONTYME USAGE


-----------------------------------------------------------------------


AS SOME OF YOU MAY HAVE NOTICED, RESPONSE TIME ON ONTYME HAS GOTTEN SLOWER
AND SOMETIMES YOU RECEIVE A MESSAGE SAYING - SUBHOST OUT OF PORTS -

BOTH OF THESE HAVE OCCURRED BECAUSE OF THE LARGE VOLUME OF USERS ON OUR
ACCOUNT.  

DO NOT STAY LOGGED IN FOR ANY LONGER THAN NECESSARY.  WHEN YOU FINISH,
USE ONE OF THE LOGOFF COMMANDS (:Q, :EX, :LOG).  IF YOU JUST TURN OFF
YOUR TERMINAL, YOU'RE STILL CONNECTED TO THE SYSTEM UNTIL YOU ARE TIMED
OUT.  THAT KEEPS SOMEONE ELSE FROM LOGGING ON.

THE USERS ON THE WEST COAST CAN DO EVERYONE A SPECIAL FAVOR.  STAGGER YOUR
LOG-IN TIMES.  IF YOU ARE ONE OF THE MANY WHO LIKE TO PICKUP YOUR MESSAGES
AND GO FOR A CUP OF COFFEE FIRST THING IN THE MORNING,  TRY PICKING UP
YOUR MESSAGES LATER ON IN THE DAY.  TRY AND AVOID PEAK TIMES.

IF YOU DO RECEIVE A SUBHOST OUT OF PORTS MESSAGE, WAIT AT LEAST 15 MINUTES
BEFORE TRYING AGAIN.

ANOTHER TRICK MIGHT BE TO USE THE :SIG COMMAND WHEN YOU ARE DONE.  IF THERE
ARE TWO OR THREE OF YOU WHO ALL WANT TO CHECK YOUR MESSAGES, HAVE ONE PERSON
LOG IN.  WHEN THAT PERSON IS DONE, :SIG WILL SIGN THEM OUT BUT LEAVE
THE LINE CONNECTED INTO THE ACCOUNT.  THEN THE NEXT PERSON LOGS IN WITHOUT
HAVING TO REMAKE A CONNECTION THROUGH THE NETWORK.

IF ANYONE OUT THERE HAS ANY SUGGESTIONS FOR DEALING WITH OUR ONTYME PROBLEMS,
                                                                Page  2

SEND THEM TO ME (FSC.SUP) AND IF THEY'RE GOOD I'LL PASS THEM ALONG.


LET'S WORK TOGETHER TO KEEP OUR LINES OF COMMUNICATION OPEN AND CLEAR.

CARMEN
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 25 Oct 85 5:04:19 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 24 OCT 85 15:53:00 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J61469@Ontyme.Tymnet 
Subject: NEW ONTYME ACCOUNT FOR NATIONAL TECH SUPPORT 

-----------------------NATIONAL TECHNICAL SUPPORT------------------------

TO:      THE WORLD
FR:      KIMBERLEY HOLLAND
RE:      NEW ONTYME ACCOUNT FOR NATIONAL TECH SUPPORT

------------------------------------------------------------------------


On Monday, October 28th 1985, National Technical Support    
will be moving to its own ontyme account EMSNTS.  
Please take note on your any copy lists or when you send    
a ontyme to anybody in NTS.  The following are those who    
will be involved int the moved.    
     
        Don Johnson       - NTS.DEJ  
        Doug Wagner       - NTS.D/WAGNER  
        Ed Blair          - NTS.E/BLAIR   
        Don Macomb        - NTS.D/MACOMB  
        Ed Mooring        - NTS.ETM  
        Tim Kochmann      - NTS.T/KOCHMANN
        Jim Stein         - NTS.J/STEIN   
        August Cattaneo   - NTS.A/CATTANEO
        Steve Resnick     - NTS.S/RESNICK 
        Bert Novak        - NTS.B/NOVAK   
        Tim DeGoosh       - NTS.T/DEGOOSH 
        Bruce Shepherd    - NTS.B/SHEPHERD
        Ron Vivier        - NTS.RVIVIER   
        Mike Hawkins      - NTS.M/HAWKINS 
        Kimberley Holland - NTS.HOLLAND   
        NTSOPS            - NTS.NTSOPS    
        TYMDATA           - NTS.TYMDATA   
     
If you have any questions or problems, please call me at    
408/446-8091.  
     
Thank you,
Kimberley Holland
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 3 Sep 85 10:29:26 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Today. 

I have come down with a summer cold and will not be in today.  If you need me
I can be reached at 790-0608.

/N28 is now running on all systems that were running /N23 or later, with the
exception of C35, which was loaded from tape after a disk error.

/JOE
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Tue, 3 Sep 85 10:29:26 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: Today. 

I have come down with a summer cold and will not be in today.  If you need me
I can be reached at 790-0608.

/N28 is now running on all systems that were running /N23 or later, with the
exception of C35, which was loaded from tape after a disk error.

/JOE
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 5 Sep 85 12:57:26 PDT 
To: jms 
Subject: Ghosts? 

Hmmmm.  Maybe we do... I seriously doubt that the drive turned itself off
or pushed write-protect.  So, someone keeps playing with 95's hardware, I'd
guess.     sigh!      /Carl
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 13 Sep 85 20:16:48 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: EDDT vs MCA-25 

I fixed the version of EDDT in M33 to work with all KL-10s.  The monitor
built from OSP will work with the MCA-25, as long as you don't try to use EDDT.

To repeat: It is OK to use the CPU serial number to distinguish the KS from the
KL provided that you get the CPU serial number via GETTAB.  It is not OK to
use APRID to distinguish the two processors because APRID returns a bunch of
bits in addition to the CPU serial number.  For programs that run in EXEC mode
on all CPUs (such as EDDT), the only distinguishing feature is that the KL
returns different data on EXTEND[CVTBDO].

/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 14 Sep 85 15:29:45 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: EDDT & P034/P 

The "jist" of the changes to EDDT are that P034/P, the monitor will run
on all processors, including processors with an MCA-25.  However, to use
EDDT on those systems with an MCA-25 you need to run the next version
of the monitor.
-Right?
/Carl
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
Received: from C26.Tymnet by X930.Tymnet; Tue, 17 Sep 85 16:20:58 PDT
Return-path: <DENCOFF@26.Tymnet> 
From: Dennis Coffey <DENCOFF@26.Tymnet> 
Date: Tue, 17 Sep 85 12:02:21 PDT 
To: Jon Mosser <MOSSERJ@X930.Tymnet>, Bill Mortenson <HELGEM@X930.Tymnet>, Dan
	Baigent <BAIGENT@X930.Tymnet>, Carl Baltrunas <CARL@X930.Tymnet>, Osman
	Guven <OSMAN@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <FLETCHERC@X930.Tymnet> 
Subject: Password changes 



                               MEMORANDUM


                                                       TYMCOM-X SUPPORT

DATE:  17-Sep-85

  TO:  Monitor and Utilities People

FROM:  Dennis Coffey  (Tymcom-X Support)

SUBJ:  Password changes for selected usernames.

-----------------------------------------------------------------------

Due to Colman Jung's departure, LOGINN and OPER passwords have been
changed for usernames that he used.  Those affected are:

LOGINN only:	TUCOOP & SPPOPER

LOGINN & OPER:	SYSMAINT

If you wish to know the new passwords, please ask me.  (I'm just
reluctant to put them in writing, in this memo.)

If any of you knows of an OPER password for usernames TSUCOOP or
SPPOPER, please let me know -- I believe they may have have existed, but
I don't know them.  Let me know them:  I'll make them consistent with
the LOGINN passwords.  This would be particularly valuable for username
SPPOPER, which often uses license.
Received: from C26.Tymnet by X930.Tymnet; Wed, 18 Sep 85 10:41:40 PDT
Return-path: <DENCOFF@26.Tymnet> 
From: Dennis Coffey <DENCOFF@26.Tymnet> 
Date: Wed, 18 Sep 85 10:34:17 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: PASSWORDS, ESP. FOR SYSMAINT 

Carl,

    I can't set any license on host 930 using SYSMAINT's OPER password
-- either the old OPER password or the new one we spoke of yesterday.
Therefore, I can't change the password to one I do know.  I don't know
any usernames with adequate license/status to set a password.  Will you
help?

Thanks!
D.
Received: from C26.Tymnet by X930.Tymnet; Thu, 19 Sep 85 2:14:11 PDT
Return-path: <CARL@C26.Tymnet> 
From: CARL@C26.Tymnet 
Date: Thu, 19 Sep 85 2:11:35 PDT 
To: helgem 
Subject: U.SAI (UQUE.SAI) 

Bill,
  I changed the macros in U.SAI and the places in UQUE.SAI that you had
byte pointers to the descriptor words to use the new macros that I defined
in JQUEUE.DEF.  While I was there, I moved the EXPRESS; to outside the
loop and removed the extra CRLF.

I ran it & it works fine....  I did notice one thing...
   DO YOU CALL THE CLSQUE ROUTINE BEFORE YOU EXIT?
  If not, you should... it's now keeping track of how many people
have the queue open, and if you don't close it, it thinks you still
have it open even after you EXIT.

/Carl
Received: from C26.Tymnet by X930.Tymnet; Thu, 19 Sep 85 17:08:21 PDT
Return-path: <DENCOFF@26.Tymnet> 
From: Dennis Coffey <DENCOFF@26.Tymnet> 
Date: Thu, 19 Sep 85 17:02:37 PDT 
To: Carl Baltrunas <CARL@X930.Tymnet> 
Subject: SPPOPER's OPER lic. 

In the 'loop' of hosts that SPPOPER checks and maintains are 3 CEGI
hosts:  59, 90 & 92.  One of the tasks performed for SPOOL maintenance
is the renaming of SPOOL.SAV & FAKSPO.SAV, between SPOOL & SPPOPER --
across GANs.  SPPOPER does not have any license on the 3 CEGI hosts.
Should it?  If so, would you provide it?  . . . or must I contact Peter
Stratman about this?

Thanks!
D.
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Fri, 20 Sep 85 21:02:40 UT
From: CT.TECHSERV@Ontyme.Tymnet 
Date: 20 SEP 85 11:46:10 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Cc: TXS.D/COFFEY@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.SUP@Ontyme.Tymnet 
Message-id: A20838@Ontyme.Tymnet 
Subject: INITIA ON HOST S443 

 
 
 
 
 
  -----------------------M E M O R A N D U M------------------------------
  CEGI - Tymshare                   telephone  ST-CLOUD   [33](1) 602.7012
                                    from OCT, 25 1985  [33](1) 46.02.70.12
  Technical division                           CT.TECHSERV
  ------------------------------------------------------------------------
 
  Date:      September 20th, 1985 - 8:42 p.m. CET
 
  To:        Dennis Coffey (TXS.D/COFFEY)
             Carl Baltrunas (TXS.C/BALTRUNAS)
 
  Copies:    TXS.SUP
 
  From:      Peter Stratman
 
  Subject:   INITIA ON HOST S443
 
  ------------------------------------------------------------------------
 
 
  Dennis and Carl,
 
  I have now tried out the INITIA you have provided me with on host 443.
 
  I am afraid that the result is not positive. The system gets to the fol-
  lowing point :
 
    512K OF MEMORY ONLINE.
 
    System in auto-restart, running INITIA
 
  .. and then seems to hang forever.
 
  Could you please investigate further into this problem ?
 
  Thank you, and have a good weekend, Peter.
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                                    -1-
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
Date: Fri, 20 Sep 85 21:02:56 PDT 
To: Carl A Baltrunas <carl@X930.Tymnet>, Osman Guven <osman@X930.Tymnet>, Joe
	Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet> 
Subject: The interface between FILIO and xxxKON is in blocks. 

FILIO does the following:
	LDB T1,PGYPNO
	LSH T1,BLKSPP
	MOVEM T1,UNIBLK(U)
	PUSHJ P,@KONxxx(J)

APXKON uses:
	MOVE T1,UNIBLK(U)
	LSH T1,-BLKSPP
	IDIVI T1,PPC	;Pages per cylinder
	IDIVI T2,SPT	;Sectors per track

RMXKON and BPXKON use block numbers when talking to the disks.


I have started writing (M33)SAXPRM.MAC - I need help in finding the definitions
of ALL the bits and bytes that the SA-10 understands.

/JMS
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 20 Sep 85 23:33:57 PDT 
To: Joe Smith <jms@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: SAXPRM 

Lots of the SA-10 stuff (at least what we use) can be found in COMMON and
COMMOD, including the interrupt code.  BPXKON may have some more, but I'd
expect it NOT to have much.   /Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 21 Sep 85 22:36:11 PDT 
To: Dan Baigent <baigent@X930.Tymnet> 
Subject: OPER password on TYMCOM-X 

Your OPER password was found in a text file on your directory that
anyone could read (i.e. it wasn't encrypted).  It is NOT a good
practice to leave that information around where others can get it.

Also, it is not a good idea to put it into command files such as
(BAIGENT:33)INIT.CMD which any "snoopy" person can find it.

I changed it to something less creative.  Please do not have your
INIT program set license in the future.  The only programs that should
set license should be OPER and SETE!  Thank you....  see me to get
your new password... or have Jon login to system 34 and setup another
one (if you need it sooner).

/Carl

PS. the only reason I found all this was that you wanted your home
system changed and I tried it out.. and got 33 the first time.
From: BAIGENT@X930.Tymnet 
Date: Mon, 23 Sep 85 11:10:05 PDT 
To: CARL 
Subject: HELP! 

Carl,
I seem to be unable to log into any system except 2096, 33 and 930
(I get "access not permitted" for all others).  You did'nt happen to
make a minor error when you changed my home system, did you?  I have
taken out my password from the INIT.CMD file on 930 and would like to 
copy it to all my other directories, if I can ever get back into
them.  Please take care of this ASAP!
thank you...Dan.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 24 Sep 85 15:59:50 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: XEXEC 

I intend to delete (SYS)XEXEC.* soon.  Please comment...

/Carl
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 24 Sep 85 16:01:18 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Cc: txssup, Joe Smith <jms@X930.Tymnet>, Osman Guven <osman@X930.Tymnet> 
Subject: ? Deletion ? 

Oh well...   I thought that that might get your attention... give me a
call (work or home) to talk about this in more detail.

We've been working on this PERP-replacement project for a little while
and it seems that we have some duplication of effort...

  PERP  (runs on a 940) controls up to 10 jobs at a time and puts the
        logfile back out on the user's directory on the system it was
        run on.  Sends mail from PERPOPER about completion.  Will put
        a job in it's queue and reschedule it for another run date if
        the job is "periodic", otherwise it is deleted when completed

  PAM   (runs under (PJ) on most systems) controls up to 10 jobs and
        runs them as high-segments from a single job with periods that
        range from 15 minutes to 150 minutes.

  XEXEC (runs on some PDP-10s) schedules single line commands to run a
        particular program or a PCOM command file at periodic intervals
        of one or more hours.  The period may be offset by "n" hours as
        well, so that jobs do not all run at midnight.

  INITIA (runs on PDP-10s at startup) schedules a series of jobs which
        must be run in sequence (DSKCLN first) in order to bring the
        system up for general timesharing service.  Other jobs include
        CHKPNT, COPYCR, XEXEC (mentioned above), and KMCLOD.

  PCOM  (runs on all our PDP-10s) reads a command file and interprets
        the commands a la PERP by creating another frame and typing the
        commands to it.  If run in DETACH mode, it creates an extra
        detached frame to control the processing frame.

Well....
  As you can see there are a lot of similar programs lying in wait to do
the same kind of things.  PERP works in days, PAM in minutes, XEXEC in
hours (and at startup), INITIA at startup and PCOM once per invocation.

  I doubt that all things can be simplified into 1 (ONE) program, but
there seems to be a lot that can be eliminated with a perp-replacement
that understands date-time expressions down to the minute level.  Please
excuse the length of this message, but I will continue with a description
of what we intend to do in the near future:

1)  System startup.
    Force a .DSKCLN command on TTY0 to run DSKCLN
    Force a .INITIA command on TTY? (0 or 1) to run INITIA

2)  INITIA starts what it must ( CHKPNT, XEXEC ) or maybe just XEXEC
    and disappears altogether.  If run LOGGED-IN it runs your init file.

3)  XEXEC is the perp-replacement program/scheduler that I'm nearing the
    preliminarly completion on... description later...
    XEXEC starts up the jobs scheduled to run at system-startup:
          CHKPNT, COPYCR, KMCLOD, PAM (?), FREE, ....
    XEXEC hangs around and runs jobs which need to be run periodically:
          (PJ)TRUCTL, other PJ jobs, (MAIL)MAILER, other MAIL functions,
          anything else that any user may have put into the queue.

4)  CHKPNT is made responsible for un-shutting the system and saying:
    "initialization completed" since INITIA only chews up resources looking
    to see if CHKPNT got the date and time yet.  Since CHKPNT is already
    responsible for shutting the system if accounting dies, why not?

The system is now up, open, and operational... PERP is >dead< / >replaced<
(ding dong the [sic] PERP is dead, the PERP is dead, the PERP is dead, ...)


My new XEXEC, name chosen since no changes need be made to INITIA to make
it work, would replace your XEXEC program and provide the same services
via a more accessible queue.  It is basically a PCOM derivative that does
what PCOM used to do for up to "n" jobs at a time, instead of just one!

I've got it working now such that it reads a specially formatted queue-file
which is manipulated by a co-operating program called SUBMIT which replaces
the PDP-10 PERP/DEFER programs.  My current version makes an ordered list
in memory and then runs the jobs one at a time.  By the end of this week, I
hope to have all the bugs shaken out and have it run the jobs one by one at
their scheduled time (or as close as possible thereafter).  By the end of
next week, I hope to have a preliminary version which runs "n" jobs at a
time (as necessary) presuming I don't run out of channels for reading the
input files.

We should discuss the security aspect, license passing, etc pretty soon
before I cast it in concrete...  how about lunch sometime this week???

Call me at home (408) 945-4321, or after 1;pm at the office 116-2515 or
(415)794-2515 direct.   Talk to you then.  This message is already MUCH
MUCH too long.

/Carl

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
Received: from C39.Tymnet by X930.Tymnet; Thu, 26 Sep 85 0:32:28 PDT
Return-path: <CARL@C39.Tymnet> 
From: CARL@C39.Tymnet 
Date: Thu, 26 Sep 85 0:32:09 PDT 
To: fletcherc 
Subject: test 


Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 2 Oct 85 13:03:19 PDT
From: TXS.J/SMITH@Ontyme.Tymnet 
Date: 02 OCT 85 12:56:52 
To: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A29714@Ontyme.Tymnet 
Subject: New accounts in FSC 

TO: Everyone in TXS
Subject: New accounts in FSC

The ONTYME message of 20-Sep stated that TCMS and THS accounts were being
moved to FSC.  It did not mention that users in TXS have been copied to 
FSC.  As of 2-Oct, the TXS users have not been deleted, maybe they don't
know about us.)  Anyway, the file (MAIL:930)ONTYME.DAT needs to be updated
with your new key.

	TXS.J/SMITH = FSC.J/SMITH = JMS@930
Received: From EMSTXS.Ontyme.Tymnet by X930.Tymnet; Wed, 2 Oct 85 13:05:17 PDT
From: TXS.J/SMITH@Ontyme.Tymnet 
Date: 02 OCT 85 12:56:52 
To: TXS.PEAK@Ontyme.Tymnet 
Message-id: A29714@Ontyme.Tymnet 
Subject: New accounts in FSC 

TO: Everyone in TXS
Subject: New accounts in FSC

The ONTYME message of 20-Sep stated that TCMS and THS accounts were being
moved to FSC.  It did not mention that users in TXS have been copied to 
FSC.  As of 2-Oct, the TXS users have not been deleted, maybe they don't
know about us.)  Anyway, the file (MAIL:930)ONTYME.DAT needs to be updated
with your new key.

	TXS.J/SMITH = FSC.J/SMITH = JMS@930
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 4 Oct 85 1:06:01 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 30 SEP 85 11:43:12 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J43751@Ontyme.Tymnet 
Subject: MERGER OF TYMNFE INTO FSC 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      30 SEP 85  10:17

TO>        THE WORLD

COPIES>    

FROM>      CARMEN REYES (FSC.SUP)


SUBJECT>   MERGER OF TYMNFE INTO FSC


-----------------------------------------------------------------------


Effective September 30th, the TYMNFE Ontyme account has been
merged into the FSC (Field Service Compnay) Ontyme account.
 
If you correspond regularly with users in TYMNFE, please change
your mailing lists to reflect the new usernames.
 
Effective October 7th, we will begin deleting usernames from
TYMNFE and cancel the account.  Do not send any messages to
this account after Friday, October 4th.   
 
An updated public directory should be available shortly.  If you
need a copy in order to change any mailing lists, send an OnTyme
to FSC.C/REYES and request one.
 
Thank You.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 28 Oct 85 18:10:44 PST 
To: Cherie Marinelli <Cherie@X930.Tymnet> 
Subject: Where are your sizes? 

Woof Woof! Grrrrr!  Woof!!!  heh huh heh huh heh huh heh huh heh pant pant!!
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 8 Oct 85 17:06:02 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 08 OCT 85 09:42:35 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J48788@Ontyme.Tymnet 
Subject: "You can now transfer files from"... 

               *********FLASH*********

You can now transfer files from a PDP-X to the FSC OnTyme account.
For this to work, modify or create the file ACCESS.ONT under your 
PDP-X username as follows:

	USERNAME;EMSFSC
	ACCOUNT.FSC
	ONTYME NAME X/XXXXX  (where X=your username)

That should do it.  If you have any problems with this, call me.

Also, a big THANK YOU to the folks who brought this problem
to my attention.

Carmen (FSC.SUP)
(408) 446-6940
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 8 Oct 85 21:06:45 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 08 OCT 85 10:14:58 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J48817@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-152
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 08 Oct 85



To:       ISG Lists A - E (Excluding Irvine/Newport Beach 
          Locations)

Subject:  MAIL FOR ISG IRVINE/NEWPORT BEACH AREA LOCATIONS



In the past several months there has been a significant increase 
in the amount of mail sent to ISG Irvine/Newport Beach locations 
which has been incorrectly addressed.  In some cases, this mail 
is rerouted to the proper location.  Often, however, this mail 
is returned to the sender.  Incorrectly addressed mail usually 
results in delayed communications and unnecessary costs 
including additional mail handling plus, in some cases, 
duplicate postage expense.

To facilitate the delivery of mail for ISG employees in the 
IRVINE/NEWPORT BEACH area, it is requested that all such mail be 
addressed as follows, regardless of organization:

        McDonnell Douglas Information Systems Group
        Attention:  (Insert Employee Name)
        P.O. Box 19501
        Irvine, CA  92713

Note:   Mail addressed in the above manner will be delivered to 
        the appropriate ISG IRVINE/NEWPORT BEACH location twice 
        daily.

The following identifies the four primary ISG locations in the 
Irvine/Newport Beach area  including address information for 
Federal Express shipments, major ISG organizations represented 
at each location along with the FAX, TWX (where applicable) and 
Phone Number.  These addresses are not to be used for mail.

    1.  McDonnell Douglas Information Systems Group
        4000 MacArthur Blvd., Newport Beach, CA  92660

        Major Org. - ISG HQ, ISG Group Staff, MDBNSC, MDCSC and
                     MDDSC

        FAX   (714) 752-0984
        TWX   (910) 595-1546
        TWX   (910) 595-1764 (Back Up)
        Phone (714) 250-1000



    2.  McDonnell Douglas Information Systems Group
        2361 McGaw Avenue, Irvine, CA  92714

        Major Org. - MDFSC

        FAX   (714) 250-7248
        Phone (714) 250-1000

    3.  McDonnell Douglas Information Systems Group
        17481 Red Hill Avenue, Irvine, CA  92714

        Major Org. - MDCSC

        FAX   (714) 250-1780
        Phone (714) 250-1000

    4.  McDonnell Douglas Information Systems Group
        1562 Reynolds Avenue, Irvine, CA  92714

        Major Org. - MDCSC

        FAX   (714) 662-0360
        Phone (714) 250-1000

Please assure that mail for ISG Irvine/Newport Beach locations 
is addressed in accordance with the above.




D. J. Garrity
Staff Vice President
Administration - ISG
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 22 Oct 85 1:08:22 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 21 OCT 85 15:48:23 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J58414@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-159
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 18 Oct 85



To:       MDISG A through E, MDC - A through C, Bulletin Board 
          Posting

Subject:  RESIGNATION OF GARY E. LIEBL



I sincerely regret to announce the resignation of Gary E. Liebl, 
currently the Group Operating Officer of the Information Systems 
Group, effective Thursday, October 17, 1985.

Gary has accepted an appointment as President, Chief Operating 
Officer and member of the Board of Cipher Data Products, Inc., 
of San Diego, California, a leading supplier of magnetic tape 
peripherals and controllers.

During the time we worked together as ISG executive board 
members and, since June, as the team leading the ISG, I have 
developed great respect for his capabilities and for him as a 
person.  He will be greatly missed, and I am sure we all wish 
him much success in the future.

Prior to his present ISG position, Gary was President of 
McDonnell Douglas Computer Systems Company and President of 
Microdata Corporation.  Gary was also appointed a Corporate Vice 
President of MDC in July 1983.

Gary's departure makes necessary some alterations in the senior 
management structure of ISG; those changes will be announced in 
a few days.



(Original signed by R. A. Fischer)

Robert A. Fischer
Group Executive Officer
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Wed, 23 Oct 85 1:07:45 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 22 OCT 85 17:33:09 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J59515@Ontyme.Tymnet 
Subject: HEALTH PLAN INFORMATION 

                   HUMAN RESOURCES DIVISION - MEMORANDUM
                   *************************************


<M D C - I S G>

SUBJECT>   HEALTH PLAN INFORMATION


-----------------------------------------------------------------------


Over the past few weeks, many of you have had questions about 
your new health benefits.  The most frequently asked questions 
are answered here to help everyone understand these new plans.  
Other questions may be referred to your Human Resources 
Department:
 
     Southern California             (714) 250-1000
                                      Ext. 2184 or 2239
     Northern California             (408) 446-7792
     St. Louis                       (314) 232-9181
 
 
GENERAL AMERICAN PLAN
 
 
WHEN ARE MY ISG BENEFITS EFFECTIVE?
 
If you were an ISG employee prior to 9/30/85, your benefits 
become effective 9/30/85.  If you were disabled or on a leave of 
absence on that date, your coverage will be delayed until you 
return to work.
 
 
I HAVE A DEPENDENT WHO WAS HOSPITALIZED ON 9/30/85.  IS MY 
DEPENDENT COVERED UNDER THE OLD PLAN OR THE NEW PLAN?
 
                                                                Page  2

If a dependent was confined in a hospital or convalescent nursing 
home prior to 9/30/85, he/she will be covered under the old plan 
until discharged.  The day after discharge, the dependent will be 
covered under the new plan.
 
 
DO I HAVE TO SATISFY AN ADDITIONAL 1985 ANNUAL DEDUCTIBLE WITH 
GENERAL AMERICAN BEFORE PAYMENTS WILL BE MADE?
 
Deductible expenses incurred between 1/1/85 and 9/29/85 under 
your previous health plan apply toward General American's 1985 
deductible.  If the deductible on your previous plan was less 
than General American's, you will need to satisfy the difference.  
This information was sent by the previous carrier to General 
American on 9/30/85.
 
NOTE:  Under the General American Plan, deductible expenses 
incurred during October, November or December apply toward the 
following year's deductible.
 
 
WHAT DO I DO WITH BILLS INCURRED PRIOR TO 9/30/85 THAT HAVE NOT 
BEEN SUBMITTED?
 
All claims must be submitted to your previous carrier as soon as 
possible.  Send claims to:
 
     Former Tymshare/Tymnet employees:
 
          New Systems Inc.
          P.O. Box 11290
          San Francisco, CA.  94101
 
     Former Microdata employees:
 
          Northwestern National Life Insurance Company
          Regional Claims Center
          730 Fairmont Avenue - Suite 202
          Glendale, CA  91203
 
 
HOW WILL GENERAL AMERICAN KNOW ABOUT DEDUCTIBLE EXPENSES I 
SUBMITTED AFTER 9/30/85 FOR EXPENSES INCURRED PRIOR TO 9/30/85?
 
Keep your copy of the Explanation of Benefits (EOB) form which 
accompanies all claim payments.  Attach this form to claims you 
submit to General American so that you are credited with the 
proper deductible amount.
 
 
WHEN WILL I RECEIVE MY PCS CARD?
 
Your PCS card(s) will be mailed by the end of October.  Until you 
receive your card, you must pay for the drug.  Keep the receipt 
which itemizes patient's name, doctor's name, prescription 
number, date, quantity dispensed and amount charged.  You will be 
reimbursed the charge for the drug, less the appropriate 
deductible ($1.50 or $3.00) by attaching the receipt to the PCS 
                                                                Page  3

claim form.  Claim forms are available from the Human Resources 
office.  This form requires information found only on your PCS 
card, so wait until you receive your card before submitting the 
form for reimbursement.  This procedure applies for drugs 
purchased for dependent children over age 19 or from non-
participating pharmacies.
 
 
HOW DO I FILL A PRESCRIPTION THROUGH THE MAIL ORDER OPTION?
 
You will receive a mail order form for the America's Pharmacy 
with your PCS card.  You may order by mail the amount prescribed 
by your doctor, up to a 6 month supply, by paying only one 
deductible.
 
 
WHO DOES MY DOCTOR/HOSPITAL CALL FOR VERIFICATION OF MY 
INSURANCE?
 
Call the MDC Group Insurance Office in St. Louis at
(314) 232-9181.
 
 
WHAT IF MY DOCTOR RECOMMENDS HOSPITALIZATION?
 
Tell your doctor that you must have Pre-Admission Certification 
or General American will not pay the hospital bill.  Ask the 
doctor to call Monday through Friday:
 
     St. Louis Area          1-314-367-8401
     Minnesota               1-800-642-4456
     Other Areas             1-800-328-9903
 
 
WHAT ABOUT EMERGENCY SITUATIONS?
 
Your doctor must call for certification within one working day 
after admission to the hospital.
 
 
WHEN MUST I OBTAIN A SECOND SURGICAL OPINION?
 
Unless you obtain a second opinion for some non-emergency 
surgical procedures, General American will pay half the amount it 
would normally pay for the surgeon's fees.  These procedures are 
listed on page 13 of Your Group Insurance summary plan 
description.
 
 
WHAT IF A NEED EMERGENCY SURGERY?
 
Second opinions are not required in emergency situations.
 
 
ARE THERE ANY SPECIAL CLAIM PROCEDURES?
 
Yes.  When you get a second opinion, get a special sticker for 
your claim form from the Group Insurance Office or your Human 
                                                                Page  4

Resources Office.  The doctor giving the second opinion must 
complete and sign the sticker for the second opinion to be paid 
at 100%.  Separate bills for tests related to the second opinion, 
require a similar sticker for reimbursement.
 
 
WILL I RECEIVE ADDITIONAL INFORMATION ON THE PRE-ADMISSION 
CERTIFICATION AND THE SECOND SURGICAL OPINION FEATURES?
 
A mailing was sent to all ISG employees which included brochures 
on Second Surgical Opinions, Pre-Admission Certification and 
General American Plan information card(s).  If you did not 
receive this information, please contact Human Resources.
 
 
WHAT IF I STILL HAVE QUESTIONS OR NEED ADDITIONAL WALLET CARDS?
 
Call the MDC Group Insurance Office at (314) 232-9181.
 
 
WHAT IS THE GENERAL AMERICAN POLICY NUMBER?
 
General American policy number is MCP-8100.
 
 
WHERE MAY I GET ISG CLAIM FORMS?
 
ISG claim forms have been distributed to all field offices or you 
may contact your Human Resources Office. Claim forms for General 
American are color coded:  Medical - White; Dental - Grey; Vision 
- Pink; PCS - Blue.
 
 
WHO DO I CALL TO FIND OUT IF THE CLAIM FORM I SUBMITTED HAS BEEN 
PROCESSED?
 
Claims are processed through St. Louis rather than locally.  
Please allow more time for your claim to be processed.  For 
inquiries, call the St. Louis Group Insurance Office, (314)
232-9181.
 
 
 
HEALTH MAINTENANCE ORGANIZATIONS
 
 
WHEN WILL I RECEIVE MY HMO MEMBERSHIP CARDS?
 
It depends on your location and the HMO you selected.  If you 
have not already received your cards, you will receive them by 
year end.
 
 
WHAT DO I DO IN THE MEANTIME?
 
Identify yourself as a McDonnell Douglas Information Systems 
Group employee.  Supply your name and social security number to 
the HMO office.  The HMO office may request that you sign a 
                                                                Page  5

waiver for charges rendered until they can verify your 
membership.
 
 
HOW DO I FIND OUT WHAT MY GROUP NUMBER IS FOR THE HMO I SELECTED?
 
Call your local Human Resources Office or St. Louis Group 
Insurance Office.  The number to call in St. Louis for HMO or 
information on your Prepaid Dental Plan is (314) 233-0618.
:
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Sat, 26 Oct 85 14:33:20 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 25 OCT 85 17:41:13 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J62692@Ontyme.Tymnet 
Subject: 'TYMNET' NETWORK MACHINE INVENTORY 

                              M E M O R A N D U M 

                     MCDONNELL DOUGLAS FIELD SERVICE COMPANY

DATE>      25 OCT 85  15:44                   [] LOGISTICS OPERATIONS

TO>        ALL FIELD SERVICE PERSONNEL

COPIES>    E.MARTINO
           C.MILLER
           G.BOWMAN
           D.DURAND

FROM>      GEORGE WASOWSKI


SUBJECT>   'TYMNET' NETWORK MACHINE INVENTORY


-----------------------------------------------------------------------


GENTLEMEN FIRST OF ALL LET ME THANK YOU FOR THE OUTSTANDING EFFORT PUT
FORTH BY ALL PERSONNEL ON THIS MACHINE INVENTORY. I KNOW WHAT A EFFORT
IT WAS TO COUNT THE 'NETWORK' IN SUCH A SHORT TIME.

BUT WE HAVE A SMALL PROBLEM, WE HAVE NOT QUITE BUT THIS MONSTER TO BED
YET. CORPORATE FINANCE IS STILL COMING UP SHORT ON WHAT WE COUNTED VS
WHAT IS ON THE BOOKS. THEREFORE THEY HAVE ASKED US TO DO AND 'GIVE'
A LITTLE MORE.

PLEASE COUNT AND RECORD THE SERIAL NUMBERS OF ALL ENGINES THAT FALL
INTO THESE CLASSES; SPARES (WE ONLY HAVE MICROS AS SPARES)
                    PENDING INSTALLS
                    DE-INSTALLS (IN ANY CONDITION, STRIPPED OR OTHERWISE)
                    ALL LAB MACHINES

DO 'NOT' COUNT INSTALLED NETWORK MACHINES OR PRIVATE NETS.
DO 'NOT' WORRY ABOUT APPLYING ASSET TAGS.

JUST SUPPLY THE ENGINE TYPE, THE LOCATION (ADDRESS), AND THE MOST
IMPORTANT ITEM THE SERIAL #.

THIS COUNT 'MUST BE ACCOMPLISHED BY WEDNESDAY OCT.30,1985. ALL THE MACHINES
SHOULD BE EASILY ACCESSABLE AS THEY SHOULD BE WITHIN EASY REACH.

PLEASE ONTYME YOUR COUNTS TO: ELIEEN MARTINO..FIN.E/MARTINO
                              GEORGE WASOWSKI..FSC.G/WASOWSKI

THANKS FOR YOUR HELP (AGAIN)

GEORGE.....................................................
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Wed, 30 Oct 85 15:14:27 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 29 OCT 85 15:20:47 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J64982@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-162
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 28 Oct 85



To:       ISG Lists A-E; MDC Plant Lists A-C

Subject:  CHANGES IN ISG ORGANIZATIONAL STRUCTURE



When ISG's top management structure was formed in June of this 
year, it was built around the complementary capabilities 
Gary Liebl and I brought to our respective positions.  Now, 
Gary's departure has compelled us to re-evaluate that structure 
and decide whether to continue with out Group Executive/Group 
Operating Officer arrangement or switch to a simpler arrangement 
that consolidates all reporting under one Group executive.

As is clear from the attached preliminary organizational chart*, 
we have chosen the latter.  The purpose of this A.B. is to 
announce a number of organizational changes related to this 
revised organizational structure, all of which are effective 
immediately.

First, in order to have a manageable number of people reporting 
directly to me, I have made the following changes in reporting 
relationships:

    .  John Clancy now reports directly to me.  In addition to 
       his ongoing duties as President, McDonnell Douglas 
       Manufacturing and Engineering Systems Company, John will 
       oversee the activities of McDonnell Douglas Distribution 
       Systems Company, under Otis Brinkley, President, and 
       McDonnell Douglas Communications Industry Systems 
       Company, under Buck Yoder, Executive Vice President and 
       General Manager.

    .  Vern Smith now reports directly to me.  In addition to 
       his ongoing duties as President, McDonnell Douglas 
       Computer Systems Company, Vern will oversee the 
       activities of McDonnell Douglas Field Service Company, 
       under Vern Hart, President.  Since one of the Field 
       Service Company's primary tasks is to maintain Computer 
       Systems hardware in the marketplace, this grouping will 
       facilitate an even closer working relationship in this 
       activity.



*Chart not transmitted via OnTyme



ISG-85-162
Page 2


    .  Rich Calvert now reports directly to me.  In addition to 
       his ongoing duties as President, McDonnell Douglas 
       Diversified Information Systems Company, Rich will 
       oversee the activities of McDonnell Douglas Information 
       Processing Company, under Ted Bellan, President.  While 
       the Information Processing Company serves many ISG 
       business units, Diversified is its largest customer.  As 
       with Computer Systems and Field Service, this grouping 
       will be beneficial to both parties.

    .  McDonnell Douglas Health Systems Company, under 
       Leo Mirowitz, President, reported to me before Gary 
       became Group Operating Officer.  Now this company will 
       again report directly to me.  I am also pleased to 
       announce that McDonnell Douglas Information Systems 
       International Company, under Jerry Causley, President, 
       will now for the first time report directly to me.

It is important to note that, while some business units are 
being grouped for reporting purposes under a few ISG senior 
executives, there is no intent to change individual business 
unit missions or consolidate operations and financial 
reporting.  The companies being grouped together are still key 
elements of the ISG strategy and will continue to function 
independently.  I will continue to take a personal interest in 
these business units, but I must rely on John, Vern and Rich to 
oversee the direct management of these organizations.

Along with the above business unit realignments, I am making the 
following changes in the structure of ISG's Group-level staff 
organizations:

    .  Charlie Dignan, Staff Vice President-Corporate Accounts, 
       now reports to Jim Donovan, Vice President of Marketing 
       and Planning-ISG.

    .  Stan Bjurstrom, Chief Counsel-ISG (East), and 
       Hank Kohlmann, Chief Counsel-ISG (West), now report to 
       Mark Kuhlmann, Staff Vice President and Associate General 
       Counsel-ISG.

    .  Bernie Hathaway, Staff Vice President-Productivity, now 
       reports directly to me.

    .  Dennis Carvalho, Director of Human Resources-ISG 
       (St. Louis), now reports to Mike Becker, Staff Vice 
       President of Human Resources-ISG.


ISG-85-162
Page 3


Some further organizational changes are being made in response 
to our need to limit the number of strategic thrusts we can 
realistically pursue at one time.  These changes are summarized 
below:

    .  For the time being, we have decided to eliminate 
       McDonnell Douglas Financial Transaction Systems Company's 
       growth strategy and to focus on the activities of 
       McDonnell Douglas Payment Systems Company.  As a result, 
       McDonnell Douglas Financial Transaction Systems Company, 
       as it was organized earlier this year, has been 
       dissolved.  Alden Heintz, President, McDonnell Douglas 
       Payment Systems Company, now reports directly to me in my 
       capacity as Group Executive Officer-ISG.

    .  In a related move, the Check Card and Credit Reporting 
       Development organizations, headed by John Mazzola and 
       David Jung and formerly part of McDonnell Douglas 
       Financial Transaction Systems, now report to 
       Otis Brinkley.  We believe in the need to continue work 
       on our Distribution Systems strategy, even though we have 
       limited 1986 funds available for this activity.  Because 
       we believe that both of these former Financial 
       Transaction Systems thrusts have synergy with our plans 
       for the wholesale/retail distribution industry, we want 
       them to be evaluated for integration into our 
       Distribution Systems strategy.

    .  The Computer Systems Marketing and Sales organization, 
       under Mike Coleman, has by all measurements had an 
       extremely difficult year in 1985.  In an effort to 
       strengthen this organization, it has been move out of 
       McDonnell Douglas Business and Network Systems Company, 
       and will now report to Otis Brinkley.  Considering that 
       about a third of our Computer Systems sales have 
       historically been into the retail distribution 
       marketplace and that Computer Systems products are 
       planned as the foundation of our Distribution Systems 
       strategy, this new alignment should allow ISG to take 
       better advantage of our domestic Computer Systems sales 
       force.  The fact that Distribution Systems, Computer 
       Systems Marketing and Sales and the Computer Systems 
       Company all are located in Irvine, California, will 
       facilitate a close working relationship among the three.

    .  As a result of the preceding change, the name of 
       McDonnell Douglas Business and Network Systems Company is 
       now McDonnell Douglas Network Systems Company.  Warren 
       Prince will continue as President of this company, and 
       will report directly to me.



ISG-85-162
Page 4


In the coming weeks, more details will be provided by the 
business units and staff organizations affected by these 
changes.  In the meantime, the attached preliminary 
organizational chart should help clarify our new top management 
alignment.



(Original signed by R. A. Fischer)

Robert A. Fischer
Group Executive Officer
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 1 Nov 85 3:21:28 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 30 OCT 85 10:49:26 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J65573@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS ADMINISTRATIVE"... 

    MCDONNELL DOUGLAS       ADMINISTRATIVE     NO:  ISG-85-163
INFORMATION SYSTEMS GROUP      BULLETIN        DATE: 29 Oct 85



To:       ISG Employees Eligible for Benefits

Subject:  ISG OPTIONAL SPECIAL ACCIDENT INSURANCE PLAN



During the month of November 1985 eligible employees who are not 
now members may elect to enroll in the Optional Special Accident 
Insurance Program.  Current members may elect to make other 
changes, such as redesignation in the amount of insurance or 
inclusion of their family.

If an ISG employee is married to another ISG employee, and both 
are eligible for the Optional Special Accident Insurance, they 
may each elect coverage as employees but they may not insure 
each other as spouses.  If these employees have eligible 
children, either employee may elect family coverage in order to 
cover the children but only one parent may cover the children.

If your spouse is a salaried employee of MDC but is not an ISG 
employee, and you are both eligible for an MDC-sponsored Special 
Accident Insurance plan, the rule again applies that you may 
each enroll as employees but you may not insure each other as 
spouses.

The schedule of benefits and weekly cost is as follows:

    Level of                Weekly Cost of Coverage For:
    Coverage                You Only        Your Family

    $ 25,000                 $ .23             $ .34
    $ 50,000                 $ .45             $ .68
    $ 75,000                 $ .68             $1.02
    $100,000                 $ .90             $1.36
    $125,000                 $1.13             $1.70
    $150,000                 $1.35             $2.04
    $175,000                 $1.58             $2.38
    $200,000                 $1.80             $2.72
    $225,000                 $2.03             $3.06
    $250,000                 $2.25             $3.40



ISG-85-163
Page 2


If you elect family coverage, consult your Group Insurance 
Summary Plan Description for the definition of an eligible 
child.  The benefit payable depends on your family makeup:

     SPOUSE ONLY       SPOUSE & CHILDREN       CHILDREN ONLY

         60%             Spouse - 50%               20%
                          Child - 10%

Enrollment cards can be obtained from your local Human Resources 
Office serving your area.  Completed cards must be dated during 
the month of November 1985, and forwarded to your Human Resource 
Office no later than November 29, 1985.



(Original signed by M. R. Becker)

M. R. Becker
Staff Vice President
Human Resources - ISG
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 1 Nov 85 3:21:38 UT
From: TXS.C/BALTRUNAS@Ontyme.Tymnet 
Date: 31 OCT 85 12:18:30 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: A51806@Ontyme.Tymnet 
Subject: testing junk mail! 

From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 31 Oct 85 12:14:47 PST 
To: fsc.c/baltrunas@ontyme 
Subject: testing junk mail! 

Hi!
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Sat, 2 Nov 85 3:12:19 UT
From: FSC.B/SANDQUIST@Ontyme.Tymnet 
Date: 01 NOV 85 11:33:41 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J67620@Ontyme.Tymnet 
Subject: GENERAL MANAGERS MEETING 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      01 NOV 85  10:18

TO>        ALL SUPERVISORS AND ABOVE

COPIES>    

FROM>      VERN HART


SUBJECT>   GENERAL MANAGERS MEETING


-----------------------------------------------------------------------


A GENERAL MANAGERS MEETING FOR THE FIELD SERVICE COMPANY IS SCHEDULED
FOR NOVEMBER 5TH AND 6TH.  ON NOVEMBER 5TH, THE MEETING WILL BE HELD
AT THE MARRIOTT IN IRVINE.  THE MEETING WILL BEGIN PROMPTLY AT
3:00 P.M. AND END AT 5:00 P.M.

ON NOVEMBER 6TH, THE MEETING WILL BE HELD IN THE CONFERENCE ROOM AT
THE LIBERTY ADDRESS IN FREMONT.  THE MEETING TIME WILL BE THE SAME
AS THE NOVEMBER 5TH MEETING.

THESE MEETINGS WILL BE LIMITED TO SUPERVISORS AND ABOVE.  BECAUSE OF
THE COST, WE REQUEST THAT ONLY THE FIELD SUPERVISORS AND MANAGERS IN
THE IMMEDIATE VICINITIES ATTEND.  PERSONNEL FROM THE FIELD THAT HAPPEN
TO BE IN THE AREA AT THE TIME OF THE MEETINGS ARE WELCOME TO JOIN.

YOUR PARTICIPATION IN THIS MEETING WILL BE APPRECIATED.
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 6 Nov 85 14:32:25 PST 
To: junkmail 
Subject: mistakes 


Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 12 Nov 85 3:53:11 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 11 NOV 85 13:46:22 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J73516@Ontyme.Tymnet 
Subject: REMOVAL OF USERNAMES FROM SYSTEMS 25, 33 AND 70 

CARL,

WHO ELSE SHOULD I SEND THIS TO?

CARMEN





 
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
 
  We appreciate your cooperation.:load off


 
 
 
 
 
 
 
 
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
BUBBNET                            MFGSEC    
BURNS1                             MFORDE    
BURRIESCIN                         MICRO     
BUSPLN                             MICROCYCLE
BWALLACE                           MIDAS     
BWILLIAMS                          MIDBAK    
C/LANGDON                          MIDCENTRAL
CAAPROJ                            MIKELU    
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
DBCEO                              OSMAN     
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
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 12 Nov 85 3:53:47 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 11 NOV 85 14:55:21 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J73622@Ontyme.Tymnet 
Subject: ONTYME 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      11 NOV 85  14:55

TO>        ALL FIELD SERVICE PERSONNEL

COPIES>    

FROM>      CARMEN REYES (FSC.SUP)


SUBJECT>   ONTYME


-----------------------------------------------------------------------


In order to alleviate the load on our account and attempt to cut
back on 'junk mail', some new administrative procedures are going
into effect.
 
As of Monday, November 11th, any requests for additions and
deletions to the FSC account will be handled by the people listed
below:
 
     Sales                              FSC.L/EVERINGHAM
     Marketing and Planning             FSC.SUP
     Technical Operations               FSC.L/FROST
     Finance and Administration         FSC.SUP
     Field Operations - East            FSC.S/HARRIS
     Field Operations - Central         FSC.D/BRANNON
     Field Operations - West            FSC.N/RICHARDSON
     Field Operations - Headquarters    FSC.B/SANDQUIST
     Material and Logistics             FSC.SUP
 
These people will also be responsible for message distribution in
their area.  We are in the process of standardizing the file
names used for mailing lists, to make them easier to use.  Any
OnTyme problems should be referred to the appropriate person, who
will then, if necessary, escalate it.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Wed, 13 Nov 85 3:48:59 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 12 NOV 85 15:44:18 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J74683@Ontyme.Tymnet 
Subject: "THE FORMER TYMSHARE AND TYMNET"... 

     THE FORMER TYMSHARE AND TYMNET LOGISTICS FUNCTIONS (NDC, FREMONT
AND TYMNET WORLD WIDE PARTS, CUPERTINO) HAVE MERGED INTO (1) OPERATION
"M.D.F.S.C. MATERIAL SUPPORT."

     EFFECTIVE DEC. 2, 1985 BOTH FUNCTIONS WILL BE LOCATED IN A NEW 
FACILITY. THE ADDRESS IS :


          M.D.F.S.C.
          40547 ALBRAE ST.
          FREMONT, CA  94538
          ATTN. : MATERIAL SUPPORT
          PHONE (415) 659-8600  (SWITCH BOARD)


     PLEASE HOLD ALL RETURN SHIPMENTS TO BOTH FUNCTIONS FROM NOV. 18
THRU NOV. 29. YOU MAY RESUME NORMAL SHIPMENTS TO THE NEW ADDRESS ON
NOV. 30.

     WE WILL BE SHUT DOWN FOR ALL SHIPPING, EXCEPT P-1 EMERGENCIES
BETWEEN NOV. 27 AND DEC. 2. WE WILL RESUME NORMAL SHIPPING ACTIVITIES
ON DEC. 3.

                             THANK YOU,

                                           MATERIAL SUPPORT
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Wed, 13 Nov 85 3:49:28 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 12 NOV 85 16:04:37 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J74704@Ontyme.Tymnet 
Subject: NEW ONTYME HOST 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      12 NOV 85  16:05

TO>        ALL FSC ACCOUNT USERS

COPIES>    LES METOUR

FROM>      CARMEN REYES (FSC.SUP)


SUBJECT>   NEW ONTYME HOST


-----------------------------------------------------------------------


Good news!  The FSC account is being moved to a new OnTyme host.
This should give us better response time and easier accessibility.
 
The move will take place the weekend of November 15th.  There are
a few things that each of you need to know:
 
     1.   Only the usernames and the files will be moved.
          The IN, IN OLD, OUT, and OUT OLD messages will
          be left behind.
 
     2.   If you have any messages you want to save, put
          them in a file.*  (see * below)
 
     3.   ALL MESSAGES MUST BE SENT AND/OR READ by 12:00
          noon, PST on Friday November 15th.
 
     4.   You have until 4:00 pm, PST on November 15th 
          to save any messages.
 
     5.   The FSC account will be UNAVAILABLE from 5:00 pm,
          PST, November 15th until 3:00 pm, PST, November
          16th.  DO NOT ATTEMPT TO ACCESS THE ACCOUNT DURING
          THIS TIME.
 
     6.   A new letter will appear in front of messages on
          the new host.  Instead of a J, you will see a P.
                                                                Page  2

 
All internal accounts have been notified of this move.  They have
been asked to hold all their messages for FSC until the move is
completed.  Thanks for your help and understanding.
 
 
* - Here are two procedures for saving your messages as files:
 
     1.  There is an EXEC available between the hours of
         3:00 pm and 6:00 am PST, called ** FSC.FILEMSGS.
         Decide which messages you want to save and write
         down the complete message numbers.  Type in 
         :EXEC ** FSC.FILEMSGS and answer the prompts.
 
     2.  Determine which messages you want to save and
         write down the full message numbers.  For each
         message to be saved, issue the following commands:
          :ERASE
          :GET message#
          :FILE * filename
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 15 Nov 85 23:21:18 UT
From: FSC.J/ZUMMO@Ontyme.Tymnet 
Date: 15 NOV 85 07:40:13 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J76858@Ontyme.Tymnet 
Subject: PROCEDURES 

TO: ALL FIELD PERSONNELL 
FR: JOE ZUMMO (MDFSC)    
RE: PROCEDURES 
     
     
     
WHEN TRANSFERING DEFECTIVE PARTS TO THE KING OF PRUSSIA REPAIR CENTER 
PLEASE INCLUDE THE FOLLOWING INFORMATION.    
     
ENTER PART#     :ENTER THE PART NUMBER HERE. 
QUANTITY        :ENTER THE NUMBER OF PIECES BEING SHIPPED FOR THIS PART#.  
     
TRANSFER TO     :IF THE DESTINATION IS KOP THEN THIS WILL ALWAYS BE 1102.  
     
IS PART DEFECTIVE       :ENTER Y   
MODE OF TRANSFER    
CARRIER                 :ENTER UPS OR FED-X OR WHOEVER THE CARRIER IS.
     
AWB.            :ENTER THE AIR WAYBILL NUMBER (UPS STAMP NO.,FED-X BILL OF 
                 LADING NO. ETC.). 
     
DATE SHIPPED    :ENTER THE DATE THE PACKAGE SHIPPED OR PHYSICALLY LEFT.    
     
ETA             :ENTER APROX. DATE OF ARRIVAL AT KOP.  
     
COMMENTS:       ENTER THE SERIAL NUMBERS OF THE PARTS BEING SHIPPED.  
                IT IS VERY IMPORTANT THAT YOU ENTER THE SERIAL NUMBERS
                FOR POSITIVE IDENTIFICATION. YOU SHOULD BE ABLE TO GET
                ABOUT FIVE 8-DIGIT NUMBERS IN THIS FIELD.   
     
     
A PROPERLY COMPLETED TRANSFER SHOULD LOOK LIKE THIS:   
     
TRANSFER FROM: 1201 
PART # 390010-001   
QUANTITY:   1 OLD LEVEL:     11 NEW LEVEL:      10
TRANSFER TO:  1102  
PART STATUS:  DEFECTIVE  
CARRIER: UPS   
DATE SHIPPED: NOV,15,1985    ETA: NOV,20,1985
COMMENTS: S/N=1234-1234  
     
     
IS ALL INFORMATION CORRECT? Y 
TRANSFER COMPLETE!  TRANSFER # =     47146   
ARE YOU FINISHED WITH THIS OPTION? 
     
WRITE THE TRANSFER NUMBER ON THE DEFECTIVE PART TAG WHICH SHOULD ALREADY   
BE ATTACHED TO THE DEFECTIVE PART. 
     
WRITE YOUR INVENTORY LOCATION NUMBER ON THE DEFECTIVE TAG (IE.1201)ETC.    
     
THAT SHOULD DO IT. IF WE ALL FOLLOW THE SAME PROCEEDURES THERE WILL BE
LESS CONFUSION AND THE PHYSICAL COUNTS EACH YEAR SHOULD BE MORE ACCURATE.  
     
IF YOU HAVE ANY PROBLEMS TRANSFERING PARTS PLEASE CONTACT ME AND WE WILL   
STREIGHTEN IT OUT.  
     
                 THANX FOR YOUR HELP    
                 JOE ZUMMO (215)265-8172
                 FSC.J/ZUMMO
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 15 Nov 85 23:21:45 UT
From: FSC.J/ZUMMO@Ontyme.Tymnet 
Date: 15 NOV 85 09:27:40 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: J77023@Ontyme.Tymnet 
Subject: EMERGENCY SHIPMENT PROCEDURES 

     
     
     
     
     
                               Memorandum    
     
    Date:  November 15,  1985 
     
      To:   ALL FIELD PERSONELL            Copies:  AL CARACIOLO 
                                                   GEORGE WASOWSKI    
     
     
     
    From:   JOE ZUMMO (WWP)   
     
 Subject:  EMERGENCY SHIPMENT PROCEDURES
     
     
     
     
FOR THE INTERIM THE FOLLOWING ORDER PRIORITY CODES WILL APPLY:   
     
PRIORITY             RESPONSE TIME               MODE OF TRANSPORTATION    
********             ******** ****               **** ** **************    
A1: SYSTEM          IMMEDIATE STOCK CHECK        NEXT FLIGHT OUT 
    DOWN            DISTRICT/REGION              TICKET COUNTER  
     
     
DEFINITION:  A SYSTEM DOWN CAN BE DEFINED AS A NETWORK ORIENTED FAILURE    
             (NODE OR OTHER PIECE OF EQUIPMENT) THAT PROHIBITS THE TRAN-   
             SMISSION OF DATA. (THIS DOES NOT INCLUDE LAB MACHINES).  
     
ACTION:      UNDER THE A1 PRIORITY, THE PART(S) REQUIRED IS TO BE INVES-   
             TIGATED WITHIN THE INVENTORY SYSTEM AND OBTAINED FROM WITHIN  
             THE DISTRICT/REGION INVENTORY. MATERIAL NOT AVAILABLE FROM    
             THE REQUIRING REGION WILL PLACE A CODE A1 ORDER WITH REGIONAL 
             MANAGER'S APPROVAL, OR HIS DESIGNEE, WITH WWP REPAIR CENTER.  
     
             FIELD LAB MACHINES ARE TO BE STRIPPED TO REDUCE NETWORK DOWN  
             TIME.  
     
     
     
A2: UNIT DOWN        24 TO 48 HOURS              OVERNIGHT DELIVERY   
     
DEFINATION:  A NODE/PERIPHERAL UNIT DOWN WHICH DOES NOT TAKE THE NETWORK   
             DOWN, HOWEVER, MAY RESTRICT OPERATIONS.   
     
ACTION:      UNDER THE A2 PRIORITY, THE PART(S) REQUIRED ARE TO BE INVEST- 
             IGATED WITHIN THE INVENTORY SYSTEM AND OBTAINED FROM THE DIST-
             RICT/REGION INVENTORY. MATERIAL NOT AVAILABLE FROM THE REQUIRING   
             REGION WILL PLACE A CODE A2 ORDER WITH THE DISTRICT MANAGER'S 
             APPROVAL WITH WWP REPAIR CENTER.
     
     
C1: REPLENISHMENT    30 TO 45 DAYS                UPS OR SURFACE 
     
DEFINITION:  STOCK REPLENISHMENT ORDERS REQUIRED FOR THE REPLENISHMENT OF  
             SPARE PARTS USED DURING MAINTENANCE ACTIVITIES.
     
     
ACTION:      ORDER SPARE PART(S) FROM WWP REPAIR CENTER USING THE OPTION   
             "O" FEATURE WITHIN THE PARTS PROGRAM.
     
     
Subject: EMERGENCY SHIPMENT PROCEDURES                           Page  2   
     
     
     
NOTE: WWP REPAIR CENTER WILL PROVIDE 24 HOUR STANDBY SERVICE FOR PRIORITY  
      A1 SHIPMENTS. 
     
WWP CONTACT: JOE ZUMMO (215)265-8172    
WWP MANAGER: AL CARACIOLO
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 21 Nov 85 3:35:17 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 19 NOV 85 17:04:10 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P01915@Ontyme.Tymnet 
Subject: 'LOST' MESSAGES 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      19 NOV 85  16:54

TO>        ALL FSC ACCOUNT USERS

COPIES>    

FROM>      CARMEN REYES (FSC.SUP)


SUBJECT>   'LOST' MESSAGES


-----------------------------------------------------------------------


It has been brought to my attention that some of you may not be aware
of the fact that our OnTyme account was switched to a new host last
weekend.  (Have you noticed the improved response time?)

Because of the move, any incoming messages that you did not read by
4:00 pm, November 15th have been left behind.  If you are concerned
about not receiving an important message, there is a solution.

I can go in and get a hardcopy of your messages and mail them to you.
Just send me your name and a mailing address and I'll send them out.
If you need them any faster, please call me at (408) 446-6940.

The last day that I can do this is November 27th, so get your request
in by then.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 22 Nov 85 19:19:41 UT
From: FSC.B/SANDQUIST@Ontyme.Tymnet 
Date: 21 NOV 85 13:38:52 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P02775@Ontyme.Tymnet 
Subject: UPDATING OF MICOMS 

TO:       ALL FIELD SERVICE USERS

FROM:     JON MOSSER

RE:       UPDATING OF MICOMS


IT HAS BEEN BROUGHT TO OUR ATTENTION THAT 
SOME OF THE FIELD HAS BEEN TOLD TO STOP USING
MICOMS.

MICOMS HAS NOT BEEN 'SHUT OFF" AND
WILL CONTINUE TO BE USED UNTIL OFFICIAL
NOTICE IS GIVEN TO THE FIELD. (SOME TIME IN
DECEMBER).

PLEASE CONTINUE TO USE IT UNTIL THAT TIME.

iF YOU ARE USING FSR'S INSTEAD OF MAR'S 
THE INFORMATION MUST BE ENTERED IN MICOMS
ALSO.

IF THERE ARE ANY QUESTIONS, PLEASE CONTACT
ME VIA ONTYME.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 28 Nov 85 3:53:23 UT
From: FSC.G/WASOWSKI@Ontyme.Tymnet 
Date: 26 NOV 85 10:18:58 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P04164@Ontyme.Tymnet 
Subject: NETWORK MACHINE INVNETORY 

TO: ALL FIELD SERVICE                           CC: C.MILLER   
                                                    J.BOWMAN  
                                                    D.DURAND  
FR: GEORGE WASOWSKI                                 E.MARTINO    
     
SUBJ: NETWORK MACHINE INVNETORY    
     
APPARENTLY ALL THE MACHINES WERE NOT COUNTED DURING THE INVENTORY IN  
OCTOBER, THE COUNT IS APPROXIMATELY 1100 MACHINES SHORT.    
     
SO THE FOLLOWING EXERCISE WILL TAKE PLACE IMMEDIATELY. ATTACHED TO    
THIS ONTYME IS A LIST OF ALL THE NODES #'S THAT WERE 'NOT' COUNTED    
DURING THE INVENTORY. PER THIS LIST THE FOLLOWING INFORMATION WILL    
BE SUPPLIED;   
                NODE#           SERIAL#         MACHINE TYPE
                ---------------------------------------------   
     
THIS INFORMATION WILL BE SUPPLIED FOR MANNED SITES BY WEDNESDAY NOV.  
27TH, 'ALL' OTHER SITES ARE DUE BY DEC. 3RD. THESE DATES ARE 'FIRM'   
NO EXTENSIONS OR EXCEPTIONS WILL BE MADE. THE INFORMATION WILL BE
GIVEN DIRECTLY TO CORP. FINANCE AND THERE ARE TWO (2) METHODS TO 
GET THE INFO TO THEM.    
     
1.  ONTYME PEGGY BOLLINGER (FIN.P/BOLLINGER) 
2. CALL DIRECTLY WITH INFO TO,  PEGGY BOLLINGER (408)446-7760    
                        OR      BARBARA MCINTYRE (408)446-6777    
     
THERE IS A SECOND METHOD OF LOCATING THE MISSING EQUIPMENT (SIC).
THERE IS A REPORT ON SYSTEM 54 UNDER MY DIRECTORY (WASOWSKI)ENG.RPT,  
THIS REPORT IS A SORT BY MACHINE SERIAL # OF ALL THE MISSING NODES.   
ON THIS REPORT THERE IS A SITE NUMBER COLUM, THIS IS THE SAME SITE    
USED ON THE OES SALES ORDERS FOR NEW INSTALLS. ALSO YOU WILL NOTICE   
A DATE SHIPPED COLUM, THIS SHOWS THE DATE THE EQUIPMENT WAS SHIPPED   
FROM MANUFACTURING. THERE ARE SOME REMARKABLE MACHINES LISTED THERE   
223 OF THEM WERE SHIPPED IN 1985 AND WE CAN'T FIND THEM, THEN THERE   
192 THAT WERE SHIPPED IN 1984 THAT WE CAN'T FIND. IT'S IMPOSSIBLE TO  
BELIEVE THAT MACHINES THAT WERE SHIPPED FROM 1 TO 22 MONTHS CAN'T
BE ACCOUNTED FOR.   
     
ANY AND ALL MACHINES ARE TO LOOKED AT TO FIND THESE SERIAL #'S. ALL   
NETWORK MACHINES, LAB MACHINES (YES, EVEN THOSE ONES THAT YOU HAVE    
MADE YOUR SELF OR ARE NOT SUPPOSE TO HAVE), PENDING INSTALLS, DE-
INSTALLS, AND ALL SPARE MICROS. ALL MICROS ARE TO BE COUNTED GOOD,    
BAD, SPARES, WHATEVER....
     
THE IMPORTANCE OF FINDING THESE MACHINES CANNOT BE RELAYED IN THE SPACE    
AVAILABLE. THIS EQUIPMENT IS WORTH SEVERAL MILLONS OF DOLLARS AND
MUST BE ACCOUNTED FOR !!!!!!  
     
CURT MILLER IS FULLY AWARE OF THIS SITUATION AND IS  IN TOTAL AGREEMENT    
WITH FINANCE AND NYSELF THAT THIS EQUIPMENT MUST BE FOUND AND THEIR   
SERIALS RECORDED. CURT HAS ASSURED FINANCE THE THE FIELD WILL GIVE IT'S    
FULL COOPERATION IN THIS PROJECT.  
     
REGARDS..............................................  
     
     
     
******** BELOW IS A LIST OF ALL THE NODES (BY NODE#) NOT COUNTED ******    
     
     
12      14      15      17      21      27      32      46      47      61 
77      106     116     121     145     150     154     155     163     201  
212     232     234     264     266     267     300     361     411     441 
470     563     564     633     634     732     761     1000    1003    1010   
1012    1013    1017    1037    1112    1153    1170    1204    1241    1264    
2007    2047    2064    2071    2073    2100    2101    2105    2107    2112 
2120    2121    2122    2130    2137    2150    2151    2154    2233    2247 
2255    2263    2332    2333    2334    2346    2347    2351    2360    2374 
2376    2402    2410    2411    2413    2420    2421    2452    2455    2461 
2464    2467    2475    2477    2501    2504    2506    2507    2516    2520 
2534    2561    2573    2574    2575    2614    2630    2637    2640    2655 
2660    2662    2664    2676    2677    2702    2713    2714    2720    2723 
2736    2745    2746    2747    2756    2761    2770    2773    3002    3005 
3016    3020    3021    3027    3035    3036    3037    3040    3041    3042 
3043    3045    3054    3055    3062    3064    3065    3067    3072    3073 
3100    3103    3104    3115    3120    3121    3136    3150    3153    3154 
3163    6164    3165    3174    3175    3176    3202    3203    3210    3212 
3213    3233    3236    3237    3243    3245    3255    3270    3273    3302 
3306    3307    3317    3322    3325    3345    3346    3352    3355    3357 
3361    3363    3364    3370    3373    3376    3406    3407    3410    3414 
3431    3432    3436    3450    3452    3456    3500    3501    3510    3514 
3515    3533    3535    3537    3541    3542    3546    3547    3550    3552 
3554    3564    3572    3573    3576    3577    3600    3614    3623    3625 
3631    3636    3637    3640    3642    3643    3665    3676    3677    3700 
3701    3704    3706    3707    3724    3725    3731    3733    3737    3751 
3752    3762    3765    4000    4002    4003    4010    4011    4022    4027 
4036    4041    4047    4051    4064    4077    4100    4101    4106    4107 
4130    4132    4135    4136    4141    4142    4143    4153    4154    4156 
4160    4161    4173    4200    4207    4210    4211    4217    4222    4224 
4226    4240    4242    4245    4246    4257    4262    4263    4272    4274 
4276    4323    4324    4342    4343    4344    4351    4360    4361    4362 
4406    4410    4427    4431    4435    4437    4440    4474    4525    4553 
4600    4606    4610    4612    4616    4625    4630    4631    4632    4633 
4636    4650    4652    4661    4664    4672    4704    4706    4715    4717 
4722    4725    4735    4736    4737    4755    4767    5002    5003    5004 
5005    5006    5007    5011    5012    5014    5043    5056    5062    5070 
5071    5075    5101    5102    5103    5105    5106    5107    5111    5116 
5117    5120    5122    5123    5127    5142    5143    5145    5156    5161 
5162    5163    5164    5165    5172    5173    5174    5175    5176    5177 
5200    5201    5223    5227    5231    5232    5233    5236    5241    5242 
5243    5244    5245    5256    5260    5263    5271    5274    5275    5276 
5301    5302    5304    5306    5307    5310    5311    5312    5313    5314 
5316    5321    5325    5332    5334    5335    5341    5347    5352    5354 
5361    5363    5364    5365    5370    5372    5416    5417    5420    5421 
5422    5427    5430    5431    5432    5433    5434    5435    5436    5437 
5456    5472    5473    5474    5475    5512    5514    5515    5516    5517 
5522    5526    5530    5531    5555    5556    5557    5561    5575    5576 
5577    5636    5642    5645    5646    5647    5650    5651    5652    5654 
5655    5656    5657    5661    5662    5663    5664    5671    5675    5732 
5733    5734    5736    5737    5752    5754    5760    5776    6010    6024 
6025    6026    6034    6104    6106    6127    6131    6133    6147    6150 
6151    6160    6161    6162    6164    6165    6207    6210    6211    6217 
6224    6225    6226    6227    6232    6233    6234    6254    6255    6256 
6257    6260    6261    6262    6263    6264    6265    6275    6303    6304 
6305    6306    6307    6326    6327    6330    6331    6332    6333    6334 
6335    6336    6340    6342    6344    6345    6346    6347    6350    6356 
6360    6361    6362    6363    6373    6401    6404    6405    6410    6413 
6415    6420    6424    6426    6432    6433    6440    6450    6451    6452 
6453    6454    6455    6456    6457    6460    6461    6462    6463    6464 
6465    6466    6467    6470    6471    6474    6475    6476    6477    6511 
6517    6524    6525    6526    6527    6530    6532    6533    6535    6541 
6542    6543    6550    6552    6554    6555    6556    6557    6560    6561 
6562    6563    6565    6571    6572    6573    6575    6607    6610    6611 
6612    6613    6615    6616    6617    6620    6621    6622    6624    6630 
6631    6634    6635    6636    6640    6641    6642    6643    6644    6645 
6651    6652    6653    6654    6655    6656    6657    6660    6662    6663 
6664    6666    6667    6670    6672    6673    6675    6676    6700    6704 
6705    6712    6714    6715    6716    6717    6720    6721    6722    6726 
6727    6727    6730    6731    6732    6733    6734    6735    6736    6737 
6740    6741    6742    6743    6744    6745    6746    6747    6750    6751 
6752    6753    6760    6761    6762    6764    6765    6771    6772    6773 
6776    7001    7004    7005    7006    7007    7010    7011    7012    7013 
7014    7015    7016    7021    7022    7023    7032    7034    7035    7043 
7051    7052    7053    7054    7055    7056    7057    7060    7061    7062 
7064    7066    7073    7074    7075    7076    7102    7104    7105    7106 
7113    7114    7115    7116    7122    7123    7127    7130    7131    7132 
7133    7134    7135    7136    7137    7140    7141    7142    7143    7144 
7145    7146    7147    7150    7151    7152    7160    7161    7162    7163 
7164    7165    7166    7167    7170    7171    7172    7173    7175    7176 
7201    7202    7203    7204    7205    7206    7207    7213    7214    7215 
7216    7217    7220    7221    7223    7224    7226    7227    7232    7235 
7236    7237    7240    7241    7245    7247    7250    7251    7252    7253 
7254    7255    7256    7257    7260    7261    7262    7263    7265    7266 
7267    7270    7271    7273    7274    7275    7276    7277    7300    7302 
7303    7304    7306    7307    7310    7311    7312    7313    7314    7315 
7316    7317    7320    7321    7322    7323    7324    7325    7326    7327 
7330    7331    7332    7333    7334    7335    7336    7337    7340    7341 
7342    7343    7344    7345    7346    7347    7350    7351    7352    7353 
7354    7355    7356    7357    7361    7362    7363    7364    7365    7366 
7367    7370    7371    7372    7373    7374    7375    7376    7377    7400 
7402    7403    7404    7411    7412    7413    7414    7415    7416    7417 
7420    7421    7422    7423    7424    7425    7426    7430    7431    7432 
7433    7434    7435    7437    7440    7441    7442    7443    7444    7445 
7446    7447    7450    7451    7452    7453    7454    7455    7456    7457 
7460    7461    7462    7463    7464    7465    7466    7467    7470    7471 
7472    7473
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Thu, 28 Nov 85 3:53:41 UT
From: FSC.G/WASOWSKI@Ontyme.Tymnet 
Date: 27 NOV 85 09:42:04 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P04541@Ontyme.Tymnet 
Subject: "TO: ALL FIELD SERVICE FR: GEORGE"... 

TO: ALL FIELD SERVICE

FR: GEORGE WASOWSKI


JUST A REMINDER WE ARE MOVING TO A NEW BLDG. IN FREMONT

AS OF DEC.2 1985 (WORLD WIDE PARTS) WILL BE LOCATED AT

MDFSC
40547 ALBRAE ST.
FREMONT, CALIF.   94538

PHONE# 415-659-8600   (MAIN SWITCH BOARD)
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 3 Dec 85 19:34:07 UT
From: IOD.BEDELL@Ontyme.Tymnet 
Date: 02 DEC 85 16:47:36 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Cc: FSC.D/COFFEY@Ontyme.Tymnet, IPC.B/JACOBS@Ontyme.Tymnet,
	FSC.C/BALTRUNAS@Ontyme.Tymnet, CT.TECHSERV@Ontyme.Tymnet,
	IPC.J/KRIVANEC@Ontyme.Tymnet, IOD.CUPERTINO@Ontyme.Tymnet,
	FSC.@Ontyme.Tymnet 
Message-id: A74286@Ontyme.Tymnet 
Subject: Request for assistance 

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
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Wed, 4 Dec 85 23:15:35 UT
From: FSC.L/FROST@Ontyme.Tymnet 
Date: 03 DEC 85 14:44:33 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P05974@Ontyme.Tymnet 
Subject: TYMNET-1 ELIMINATION 

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
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Wed, 4 Dec 85 23:17:34 UT
From: FSC.J/ZUMMO@Ontyme.Tymnet 
Date: 04 DEC 85 12:53:25 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P06336@Ontyme.Tymnet 
Subject: SITE ADDRESSES 

TO: ALL FSC PERSONELL    
FR: JOE ZUMMO  
RE: SITE ADDRESSES  
     
THE FOLLOWING IS THE LATEST ADDRESS LIST FOR FIELD INVENTORY LOCATIONS.    
IF ANY ADJUSTMENTS ARE NEEDED PLEASE CONTACT ME (215)265-8172 OR ONTYME    
FSC.J/ZUMMO    
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         1000 
         MDFSC 
         40547 ALBRAE ST.
         FREMONT    
         CALIF. 94538    
         INVENTORY ADJUSTMENT 
     
     
     
         SITE NUMBER:         1101 
         TYMNET/WWP REPAIR CENTER  
         1003 B WEST 9TH AVE. 
         KING OF PRUSSIA 
         PA. 19406  
         215-265-8172    
     
     
     
         SITE NUMBER:         1102 
         MDFSC 
         1003A W. 9TH AVE.    
         KING OF PRUSSIA 
         PA. 19406  
         ATTN: JOE ZUMMO 
     
     
         SITE NUMBER:         1201 
         MDFSC 
         40547 ALBRAE ST.
         FREMONT    
         CALIF. 94538    
         415/659-8600    
     
     
     
         SITE NUMBER:         1212 
         MDFSC/WWPHQ
         40547 ALBRAE ST.
         FREMONT    
         CALIF. 94538    
         INV.ADJ.ACCT.   
     
     
     
         SITE NUMBER:         2000 
         TYMNET/CNFE(RFSC)    
         6333 HARRY HINES BLVD.    
         DALLAS
         TEXAS  75235    
         INVENTORY ADJUSTMENT 
     
     
     
         SITE NUMBER:         2101 
         TYMNET/CNFE
         6333 HARRY HINES BLVD.    
         DALLAS
         TEXAS  75235    
         214-637-7440    
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         2102 
         TYMNET, INC./SOUTHGATE BLDG.   
         2101 S. HWY. #35, RM. #219
         AUSTIN
         TEXAS 78741
         512/447-7091    
     
     
     
         SITE NUMBER:         2103 
         TYMNET/CNFE
         6950 SQUIBB RD.,BLDG. #2,STE. #110  
         SHAWNEE MISSION 
         KANSAS  66202   
         913/677-0561    
     
     
     
         SITE NUMBER:         2104 
         TYMNET/CNFE
         6950 SQUIBB RD.,BLDG. #2,STE. #120  
         SHAWNEE MISSION 
         KANSAS  66202   
         913/677-1593    
     
     
     
         SITE NUMBER:         2105 
         TYMNET-CNFEOKC  
         6125 W. RENO, STE. #1000A 
         OKLAHOMA CITY   
         OK  73127  
         (405)495-6220   
     
     
     
         SITE NUMBER:         2111 
         TYMNET/CNFE
         6333 HARRY HINES BLVD.    
         DALLAS
         TEXAS  75235    
         214/637-7440    
     
     
     
         SITE NUMBER:         2201 
         TYMNET/CNFE
         11999 KATY FREEWAY SUITE 140   
         HOUSTON    
         TEXAS  77079    
         713-870-0913    
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         2202 
         TYMNET INC./TECHNOLOGY PARK    
         520 GUTHERIDGE COURT 
         NORCROSS   
         GA.  30092 
         404-446-2327    
     
     
     
         SITE NUMBER:         2203 
         TYMNET/CNFE
         135 WALL ST. #203    
         ORLANDO    
         FLA.  32804
         305-422-0049    
     
     
     
         SITE NUMBER:         2204 
         TYMNET/CNFEM    
         4784 N.W. 167TH ST.  
         MIAMI 
         FLA.  33014
         305/620-0332    
     
     
     
         SITE NUMBER:         2205 
         TYMNET-CNFENO   
         921 CANAL ST., STE. #629, 6TH FLR.  
         NEW ORLEANS
         LA.  70112 
         (504) 525-1839  
     
     
     
         SITE NUMBER:         2207 
         TYMNET/CNFE
         2551 BLAIRSTONE PINES DR. 
         TALLAHASSEE
         FLA.  32301
         904 / 878 - 3573
     
     
     
         SITE NUMBER:         2208 
         MDFSC 
         1835 UNION AVE. #162 
         MEMPHIS    
         TN.   38104
         901-274-4715    
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         2209 
         MDFSC 
         921 CANAL ST. # 629  
         NEW ORLEANS
         LA.  70112 
         M. ROGACZEWSKI  
     
     
     
         SITE NUMBER:         2301 
         TYMNET/CNFE
         799 ROOSEVELT RD. BLDG.1  
         GLEN ELLYN 
         ILL.  60137
         312-469-2600    
     
     
     
         SITE NUMBER:         2302 
         TYMNET/CNFE
         4700 ASHWOOD DR. RM 406   
         CINNCINATI 
         OHIO  45241
         513-489-4777    
     
     
     
         SITE NUMBER:         2303 
         TYMSHARE INC.   
         15 SOUTH 5TH ST.-STE.50   
         MINNEAPOLIS
         MN. 55402  
         (612)333-4354   
     
     
     
         SITE NUMBER:         2304 
         TYMNET, INC.    
         409 PLYMOUTH RD.
         PLYMOUTH   
         MICHIGAN  48170 
         313/453-7640    
     
     
     
         SITE NUMBER:         2305 
         MDFSC 
         325 McDONNELL BLVD. TYMNET BLDG.307 
         LEV 1, WEST END 
         HAZELWOOD , MO. 63042
         (314)233-7667   
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         2306 
         MDFSC/TYMNET-CNFEDM  
         820 1ST ST., STE. #2000   
         WEST DES MOINES 
         IOWA  50265
         (515) 255 - 1069
     
     
     
         SITE NUMBER:         2307 
         TYMNET INC 
         547 W. JACKSON BLVD. 
         CHICAGO    
         IL. 60606  
         312-427-5257    
     
     
     
         SITE NUMBER:         2308 
         TYMNET-CNFEB    
         250 N. SUNNYSLOPE RD.
         BROOKFIELD 
         WI.  53005 
         (414) 782-7833  
     
     
     
         SITE NUMBER:         3000 
         TYMNET, INC.    
         55 MIDDLETOWN AVE.   
         NORTH HAVEN
         CT.  06473 
         INVENTORY ADJUSTMENT 
     
     
     
         SITE NUMBER:         3101 
         TYMNET/ENFE
         12 MANOR PKWY.  
         SALEM,
         N. H.  03079    
         603/893-3400    
     
     
     
         SITE NUMBER:         3102 
         MDFSC 
         642 KREAG RD. 3RD FL.
         PITTSFORD  
         NEW YORK 14534  
         (716)385-9300   
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         3103 
         DIGITAL EQUIPMENT CORPORATION  
         129 PARKER ST., PKO1-1/F3 
         MAYNARD    
         MA.  01754 
         ATTN:  STEVEN GOODAL 
     
     
     
         SITE NUMBER:         3104 
         TYMSHARE, INC.  
         800 OLD COTTAGE GROVE RD. 
         BLOOMFIELD,
         CT.  06002 
         203 / 243 - 2007
     
     
     
         SITE NUMBER:         3105 
         MDFSC 
         12 ALFRED ST. STE.100
         WOBURN
         MASS. 01801
         (617)938-0353   
     
     
     
         SITE NUMBER:         3106 
         MDFSC 
         10 LAFAYETTE SQ. 14TH FL. 
         BUFFALO    
         NEW YORK 14203  
         (716)854-4770   
     
     
     
         SITE NUMBER:         3107 
         TYMNET/ENFECAN  
         34 ADELAIDE ST., W., 6TH FLR.  
         TORONTO    
         CANADA M5H1L6   
         416 / 863 - 6919
     
     
     
         SITE NUMBER:         3201 
         MDCFSC/TYMNET-ENFE   
         10 CORP. PL. S., STE. #4  
         PISCATAWAY 
         N. J.  08854    
         201/562-1452    
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         3202 
         TYMNET INC. A'PORT INT'L PLZ.  
         RTE'S #1 & #9   
         NEWARK
         N.J. 07114 
         201-824-1676    
     
     
     
         SITE NUMBER:         3203 
         TYMNET INC.
         30 WALL ST.
         NEW YORK   
         N.Y. 10005 
         212-269-0010    
     
     
     
         SITE NUMBER:         3204 
         TYMNET INC. N.F.E.   
         55 MIDDLETOWN AVE.   
         NORTH HAVEN
         CT. 06473  
         203-562-1885    
     
     
     
         SITE NUMBER:         3301 
         TYMNET/ENFE
         1003-A W. 9TH AVE.   
         KING OF PRUSSIA,
         PA.  19406 
         215/265-8280    
     
     
     
         SITE NUMBER:         3302 
         TYMNET/ENFE
         309 SMITHFIELD ST.STE.5500
         PITTSBURGH 
         PA.15219   
         (412)642-6852   
     
     
     
         SITE NUMBER:         3303 
         TYMNET-ENFEM    
         GREENTREE EXECUTIVE CAMPUS, BLDG. #5002C 
         MARLTON    
         N.J. 08053 
         (609) 665-7985  
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         3401 
         TYMNET/ENFE
         11900 PARKLAWN DR., STE.#200   
         ROCKVILLE  
         MD.  20852 
         (301) 881-8683  
     
     
     
         SITE NUMBER:         3402 
         TYMNET/ENFE
         10340 OLD DEMOCRACY LN.   
         FAIRFAX,   
         VA. 22030  
     
     
     
     
         SITE NUMBER:         3403 
         TYMNET/ENFE
         10 PARK PLAZA, STE.#6
         RESEARCH TRIANGLE PARK,   
         N. C.  27709    
         919/549-9324    
     
     
     
         SITE NUMBER:         3404 
         MDFSC 
         4904 MILLRIDGE PKWY. 
         MIDLOTHIAN 
         VA. 23113  
         (703)691-8385   
     
     
     
         SITE NUMBER:         3405 
         TYMNET/ENFE
         451 E. 28TH ST. 
         CHARLOTTE  
         N. C.  28205    
         704/ 332-9010   
     
     
     
         SITE NUMBER:         3406 
         TYMNET/ENFEPENNSAUKEN
         2500 MCCLELLAN BLVD. 
         PENNSAUKEN 
         N. J.  08110    
         609/665-7985    
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         3501 
         TYMNET INC. N.F.E    
         55 MIDDLETOWN AVE.   
         NORTH HAVEN
         CT.  06473 
         203-562-1885    
     
     
     
         SITE NUMBER:         4000 
         TYMNET, INC.    
         3255 SCOTT BLVD., STE. #4-B    
         SANTA CLARA
         CA.  95050 
         INVENTORY ADJUSTMENT 
     
     
     
         SITE NUMBER:         4101 
         TYMNET/WNFE #4101    
         497 SEAPORT CT., STE. #101
         REDWOOD CITY    
         CA.  94063 
         415 / 367 - 1335
     
     
     
         SITE NUMBER:         4102 
         TYMNET INC # 4102    
         497 SEAPORT CT.,STE. 101  
         RREDWOOD CITY   
         CA. 94063  
         415-367-1335    
     
     
     
         SITE NUMBER:         4103 
         TYMNET INC # 4103    
         497 SEAPORT CT.,STE. 101  
         REDWOOD CITY    
         CA. 94063  
         415-367-1335    
     
     
     
         SITE NUMBER:         4104 
         TYMNET INC # 4104    
         497 SEAPORT CT.,STE. 101  
         REDWOOD CITY    
         CA. 94063  
         415-367-1335    
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         4105 
         TYMNET INC 
         33 EAST MAGNOLIA, SUITE 13
         STOCKTON   
         CA 95202   
         ATTN FIELD ENGINEER  
     
     
     
         SITE NUMBER:         4106 
         TYMNET/WNFE
         497 SEAPORT CT.,STE.#101  
         REDWOOD CITY    
         CA.  94063 
         415/367-1335    
     
     
     
         CANCELLED ADDRESS    
         CANX  
         CANX  
         CANX  
         TKELLEY    
     
     
     
         SITE NUMBER:         4201 
         MDFSC/TYMNET-WNFES   
         17962 COWAN AVE.
         IRVINE
         CA.  92714 
         714 / 863 - 9350
     
     
     
         SITE NUMBER:         4202 
         TYMNET/WNFE
         20 SOUTH PALM   
         ALHAMBRA   
         CA.  91803 
         818/576-8923    
     
     
     
         SITE NUMBER:         4203 
         MDFSC/TYMNET-WNFES   
         17962 COWAN AVE.
         IRVINE
         CA.  92714 
         714 / 863 - 9350
     
     
     
         SITE NUMBER:         4204 
         TYMNET/NOVA COMPUTERS
         1001 BISHOP ST. STE.800   
         HONOLULU   
         HAWAII  96813   
         808/521-8011    
     
     
     
         SITE NUMBER:         4205 
         TYMNET/WNFE
         111 WEST MONROE STE.400   
         PHOENIX    
         AZ.  85003 
         602-253-6757    
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         4206 
         TYMNET INC 
         5353 MISSION CENTER RD RM 122  
         SAN DIEGO  
         CA. 92108  
         ATTN FIELD ENGINEER  
     
     
     
         SITE NUMBER:         4501 
         TYMNET/WNFE
         3255 SCOTT BLVD. 4B  
         SANTA CLARA
         CA. 95050  
         408/446-7770    
     
     
     
         SITE NUMBER:         4601 
         TYMNET/WNFE
         777 GRANT ST    
         SUITE B #100    
         DENVER CO. 80203
         303-861-7081    
     
     
     
         SITE NUMBER:         4602 
         TYMNET/WNFE
         180 NICKERSON ST., STE.108
         SEATTLE,   
         WA. 98109  
         (206)284-8249   
     
     
     
         SITE NUMBER:         4603 
         TYMNET/WNFE
         180 NICCERSON ST., STE.108
         SEATTLE,   
         WA. 98109  
         (206)284-8249   
     
     
     
         SITE NUMBER:         4604 
         TYMNET % ALASCOM INC.
         360 EGAN DRIVE  
         JUNEAU
         AL.  99802 
         DAVE MILLETTE   
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         4605 
         MDFSC 
         7227 POTOMAC    
         BOISE 
         ID, 83706  
         DAN HOWARD 
     
     
     
         SITE NUMBER:         4606 
         MDFSC 
         6900 S.W. HAINES RD. 
         TIGARD
         OR.   97226
         STEVE VEST 
     
     
     
         SITE NUMBER:         4607 
         MDFSC 
         36 E. STRATFORD 
         SALT LAKE CITY  
         UT.   84115
         DAN SISLER 
     
     
     
         SITE NUMBER:         5000 
         TYMNET, INC.    
         10161 BUBB RD., BLDG. "G" 
         CUPERTINO  
         CA.  95014 
         INVENTORY ADJUSTMENT 
     
     
     
         SITE NUMBER:         5101 
         TYMNET/TRAINING 
         2665 N. FIRST ST.    
         SAN JOSE   
         95134 
         (408) 435-0239  
     
     
     
         SITE NUMBER:         5201 
         TYMNET/NTS 
         10161 BUBB RD.  
         CUPERTINO  
         CAL  95014 
         408-446-7998    
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         5555 
         TYMNFE.NTS 
         10161 BUBB RD., BLDG. "G" 
         CUPERTINO  
         CA.  95014 
         INV. ADJ. ACCT. 
     
     
     
         SITE NUMBER:         6000 
         TYMNET-INT'L., INC.  
         10161 BUBB RD., BLDG. "G" 
         CUPERTINO  
         CA.  95014 
         INVENTORY ADJUSTMENT 
     
     
     
         SITE NUMBER:         6101 
         TYMNET %CABLE & WIRELESS  
         NEW MERCURY HOUSE    
         22 FINWICK STREE
         HONG KONG  
         011852-52831336 
     
     
     
         SITE NUMBER:         6102 
         CHASE MANHATTAN BANK NA   
         50 RAFFLES PLACE
         TELECOMMUNICATIONS DEPT.  
         SINGAPORE  
         N/A   
     
     
     
         SITE NUMBER:         6103 
         ROBIN INFORMATION SYSTEMS (HK)LTD.  
         RM3727-40 SUN HUNG KAI CENTRE  
         GLOUCESTER ROAD 
         WANCHAI, HONG KONG   
         N/A   
     
     
     
         SITE NUMBER:         6104 
         CHASE MANHATTAN 
         72 NANKING EAST RD. SEC.2 
         TAIPEI
         REP. OF CHINA   
         N/A   
     
     
     
     
     
         DECEMBER  3, 1985   ADDRESS LIST    
     
     
         SITE NUMBER:         6201 
         CHASE MANHATTAN BK,N.A.% TEL.C 
         5ND 6TH FL.,SALAMANLYA    
         MANAMA
         BAHRAIN    
         N/A   
     
     
     
         SITE NUMBER:         6301 
         CHASE MANHATTAN BANK, N. A.    
         EDIFICIO PROSPERIDAD, CUARTO DE COMPUTER 
         126 VIA ESPANA, PANAMA CITY    
         PANAMA
         N/A   
     
     
     
         SITE NUMBER:         6302 
         CHASE MANHATTAN 
         254 MUNOZ RIVERA AVE.
         PUERTO RICO
         SAN JUAN  00936 
         N/A   
     
     
     
         SITE NUMBER:         6401 
         TYMNET % CHASE MANHATTAN BANK  
         SALAN MEDAN MEDDEKA BARAT-6    
         JAKARTA    
         INDONESIA  
         N/A   
     
     
     
         SITE NUMBER:         6901 
         KOKUSAI TYMSHARE,LTD.
         1-13-5 KUDAN KITA    
         CHIYODA-KU 
         TOKYO,JAPAN 102 
         (3)262-8711
     
     
     
         SITE NUMBER:         7101 
         MDFSC/CSS  
         4340 SOLAR WAY  
         FREMONT,CA.94538
         408-498-2500    
         ATTN: L. RODRIGUEZ
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Fri, 6 Dec 85 3:29:55 UT
From: FSC.L/FROST@Ontyme.Tymnet 
Date: 05 DEC 85 08:06:51 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P06644@Ontyme.Tymnet 
Subject: PUBLIC NETWORK CODE SYSTEMS 

                             MEMORANDUM
 
  DATE:      DECEMBER 4, 1985
 
  TO:        ALLTECH & FIELD SERVICES
 
  COPY:      GARY WALKER
             BERYL AGUA
 
  FROM:      EILEEN RIORDAN
 
  SUBJECT:   PUBLIC NETWORK CODE SYSTEMS
 
  ===============================================================
 
  We  are  now  targeting implementation of the  four-host  public 
  network  code system for the weekend of  December  13-15.   This 
  would  mean that effective Monday,  December 16,  the  following 
  systems will be used as outlined:
 
             D25        TO BE USED BY HQ AND FIELD AS THE
                        PRIMARY SYSTEM FOR GENERATION OF
                        ALL CODE FOR NODES WITH NUMBERS
                        BELOW 4000.  ALSO TO BE USED BY
                        NETCON AND FIELD AS THE PRIMARY
                        SYSTEM FOR LOADING CODE FOR NODES
                        BELOW # 4000.
 
             C70        TO BE USED BY THE ABOVE FOR THE 
                        ABOVE REASONS WHEN D25 IS UNAVAILABLE.
 
                        TO BE USED BY TYMNET OPERATIONS
                        AS A BACKUP SYSTEM FOR NETSTATS.
 
             D54        TO BE USED BY HQ AND FIELD AS THE
                        PRIMARY SYSTEM FOR GENERATION OF
                        ALL CODE FOR NODES WITH NUMBERS 4000
                        AND ABOVE.  ALSO TO BE USED BY NETCON
                        AND FIELD AS THE PRIMARY SYSTEM FOR
                        LOADING CODE FOR NODES 4000 AND ABOVE.
 
             C33        TO BE USED BY THE ABOVE FOR THE
                        ABOVE REASONS WHEN D54 IS UNAVAILABLE.
 
                        TO BE USED BY PMTS AS THEIR PRIMARY
                        SYSTEM FOR DEVELOPMENT ACTIVITIES
                                                                Page  2

 
 
  Validations  has validated those usernames submitted to me  over 
  the  past  two months on D54 and other systems  as  appropriate.  
  Please verify that you are valid where you need to be.
 
 
  
  It will be your responsibility to transfer your own files to the 
  appropriate  system and delete files no longer necessary  (i.e., 
  no longer need to keep files relating to nodes below 4000 on C33 
  after December 15).
 
  We  will work with operations to copy the TYM4000 directory from 
  25 to 54 and 33 (then deleting it from 25 & 70); and copying the 
  TYMNET directory from 25 to 70.
 
  Beryl Agua will shortly publish the new backup procedures  which 
  reflect the above configuration.   These procedures require that 
  each  Region (or Area) have a central directory available on all 
  four  systems  which will be used to store  all  field-generated 
  code for backup purposes.   These directories will also serve to 
  eliminate  confusion  in  locating  code  for  'on-call'   field 
  personnel.    Please notify her by Friday, December 6 as to what 
  those central directories are.
 
  We  will be closely monitoring the use of these four systems and 
  request your cooperation in minimizing the use of aux ciruits to 
  go from system to system.  This, while convenient, unnecessarily 
  ties up job slots/ports on multiple machines, blocking access to 
  others who may need it, and slowing down responsiveness.
 
  We  sincerely  hope  this new arrangement  will  improve  system 
  availability and response time.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Sat, 7 Dec 85 7:26:37 UT
From: FSC.DBA@Ontyme.Tymnet 
Date: 06 DEC 85 09:23:20 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P07264@Ontyme.Tymnet 
Subject: Change to the Node Naming Convention 

                      T Y M N E T    O P E R A T I O N S 


                              M E M O R A N D U M 



   Date: December 5th, 1985

     To: Distribution

   From: Denise Cornely

Subject: Change to the Node Naming Convention

----------------------------------------------------------------------

The Node naming convention is being changed to include a number for the ap-
propriate region. The break down is as follows:

                WNFE = 1
                CNFE = 2
                ENFE = 3

The district is also being changed in some cases to fit more closely with the 
city. The breakdown is as follows:

** WNFE **

Old District       New District          
   Letter             Letter             District (City)

     A                  C                China Basin
     S                  S                Santa Clara - South Bay
     S                  E                Santa Clara - East Bay
     L                  L                Los Angeles
     T                  T                Seattle
     V                  D                Denver
     X                  P                Phoenix


** CNFE **

     C                  C                Chicago
     M                  S                St. Louis
     D                  D                Dallas
     H                  H                Houston
                        U                Austin
     H                  A                Atlantic





Old District       New District
   Letter             Letter             District (City)


** ENFE **

     B                  B                Boston
     N                  N                New York
     P                  P                Philidelphia
     W                  W                Washington










The break down will be as follows, the first six (6) characters will still be 
for the Company name or city if Tymnet, the underscore will change to the number 
that corresponds with the appropriate region, the second to the last character 
will be the district letter from above, and the last letter will correspond with 
the software type of the engine. The breakdown of software type is as follows:

          A = Async Tymsat or Consat
          B = Base
          I = Isis (regardless of engine i.e mini-engine, micro-                 
              engine, etc.)
          S = Switcher
          U = ATC


The node name should look like the following:

         DALLAS2DI   this would be a Tymnet node in Dallas (DALLAS),
                     which is CNFE (2), the district is Dallas (D),                                            
                     and the engine is running ISIS software (I).
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Sat, 7 Dec 85 7:27:02 UT
From: FSC.G/WASOWSKI@Ontyme.Tymnet 
Date: 06 DEC 85 16:41:19 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P07562@Ontyme.Tymnet 
Subject: MACHINE INVENTORY 

TO: ALL FIELD SERVICE                           CC: CURT MILLER
                                                    DERIC DURAND   
FR: GEORGE WASOWSKI                                 JERRY BOWMAN 
     
SUBJ: MACHINE INVENTORY  
     
WE ARE AGAIN STLL WORKING ON GETTING ALL THE SERIAL NUMBERS OF THE    
MISSING ENGINES. THE WORK THAT HAS BEEN DONE USING THE NODE LIST 
HAS PRODUCED ALMOST 200 MACHINES THAT WERE NOT PREVIOUSLY COUNTED,    
GREAT JOB !!!!!!!!!!
     
BUT WE STILL NEED MORE. PLEASE PULL THE REPORT THAT I MENTIONED IN    
MY OTHER ONTYME, IT IS ON SYSTEM 54 UNDER MY DIRECTORY (WASOWSKI)ENG.RPT.  
THIS REPORT CONTAINS THE SERIAL NUMBERS OF ALL THE MACHINES THAT 
HAVE NOT BEEN ACCOUNTED FOR. THIS REPORT IS SORTED BY SERIAL NUMBER,  
GIVES MACHINE TYPE, AND TELLS VIA A SITE ID# WERE THE LAST KNOWN 
LOCATION OF THIS MACHINE WAS. THE SITE ID # USED ARE THE SAME ONES    
THAT YOU SEE ON THE PROJECTS FOR NEW INSTALLS.    
     
PULL AND REVIEW THIS REPORT AS IT MAY TRIGGER SOMETHING SOME WHERE OR 
MAYBE SOME OF YOU HAVE A LIST OF NODES WITH SERIAL #'S THAT YOU MAINTAN.   
     
THANKS FOR YOUR COOPERATION.................................................... 
     
REGARDS   
GEORGE B. WASOWSKI  
     
                                ***********
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 10 Dec 85 19:20:28 UT
From: FSC.L/FROST@Ontyme.Tymnet 
Date: 09 DEC 85 09:41:23 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P07847@Ontyme.Tymnet 
Subject: TYMNET I PROJECT SCHEDULE 

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
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 10 Dec 85 19:21:03 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 09 DEC 85 10:59:52 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P07904@Ontyme.Tymnet 
Subject: CHANGE OF LOCATION 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      09 DEC 85  10:58

TO>        ALL FSC ONTYME USERS

COPIES>    

FROM>      CARMEN REYES  (FSC.SUP)


SUBJECT>   CHANGE OF LOCATION


-----------------------------------------------------------------------


Just thought I'd let you know that I'm changing my office location
this week.  The new address and phone number are listed below.

                    McDonnell Douglas Field Service Company
                    39100 Liberty Street
                    Fremont,  CA  94538
                    (415) 794 - 2546

This change is effective Friday December 13th.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 10 Dec 85 19:21:36 UT
From: FSC.SUP@Ontyme.Tymnet 
Date: 09 DEC 85 14:22:19 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P08122@Ontyme.Tymnet 
Subject: *** REMINDER *** ISG EDUCATIONAL REIMBURSEMENT PLAN *** 

                   HUMAN RESOURCES DIVISION - MEMORANDUM
                   *************************************


<M D C - I S G>

DATE>      09 DEC 85  10:11

TO>        ALL EMPLOYEES (PLEASE POST)

COPIES>    

FROM>      STEPHEN REYNOLDS
           MANAGEMENT & PROFESSIONAL DEVELOPMENT


SUBJECT>   *** REMINDER *** ISG EDUCATIONAL REIMBURSEMENT PLAN ***


-----------------------------------------------------------------------


*** REMINDER *** REMINDER *** REMINDER *** REMINDER *** REMINDER


ISG employees taking courses during the winter or spring of 1986, who
wish to receive reimbursement under the ISG Educational Reimbursement
(ER) plan, are reminded that ER applications must be RECEIVED by
Human Resources AT LEAST TWO WEEKS PRIOR TO THE FIRST DAY OF CLASS.

Completed ER applications should be sent to Management & Professional
Development, VG2-A04, Cupertino.  Requests for blank applications and
questions about the plan can be directed to Renee McNeil at HRD.TRNG
or 408/446-7812.
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 10 Dec 85 19:22:08 UT
From: FSC.TRAINING@Ontyme.Tymnet 
Date: 09 DEC 85 16:58:24 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P08231@Ontyme.Tymnet 
Subject: CORRECTION TO 1986 SCHEDULE 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      09 DEC 85  16:57

TO>        ALL CONCERNED

COPIES>    

FROM>      TECHNICAL TRAINING DEPARTMENT


SUBJECT>   CORRECTION TO 1986 SCHEDULE


-----------------------------------------------------------------------


PLEASE MADE THE FOLLOWING CORRECTION TO THE FIELD SERVICE TECHNICAL

TRAINING SCHEDULE OF COURSES HANDBOOK.

PAGE 19:  TU78/TM78 MAGNETIC TAPE RA81 DISK SYSTEMS COURSE
          COURSE LENGTH IS LISTED AS 2 DAYS

          ACTUAL COURSE LENGTH IS 1 WEEK (5 DAYS)

THANK YOU
TECHNICAL TRAINING
FREMONT
Received: From EMSFSC.Ontyme.Tymnet by X930.Tymnet; Tue, 10 Dec 85 19:22:37 UT
From: FSC.J/LORTS@Ontyme.Tymnet 
Date: 10 DEC 85 10:49:14 
To: FSC.C/BALTRUNAS@Ontyme.Tymnet 
Message-id: P08487@Ontyme.Tymnet 
Subject: DELEGATION OF AUTHORITY 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y
 
 

DATE>      10 DEC 85  10:48

TO>        ALL FSC

COPIES>    

FROM>      JOHN LORTS


SUBJECT>   DELEGATION OF AUTHORITY


-----------------------------------------------------------------------


I PLAN VACATION DURING THE PERIOD OF DECEMBER 13TH THRU DECEMBER 22ND.
IN MY ABSENCE JERRY BOWMAN WILL BE ACTING FOR ME.

JERRY CAN BE CONTACTED AT (714) 250-1000 OR VIA ONTYME AT FSC.J/BOWMAN.

PLEASE FEEL FREE TO CONTACT JERRY RELATIVE TO ALL MATTERS OF LOGISTICS
AND MATERIAL SUPPORT.  JERRY WILL BE MORE THAN HAPPY TO ASSIST YOU.
   a@8b