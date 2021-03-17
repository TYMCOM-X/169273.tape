Received: from F29.Tymnet by D35.Tymnet; Tue, 3 May 88 11:41:55 PDT
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
Received: from D55.Tymnet by D35.Tymnet; Wed, 4 May 88 15:24:30 PDT
Return-path: <OSMAN@D35> 
From: Osman Guven <OSMAN@D35> 
Date: Wed, 4 May 88 15:23:25 PDT 
To: Jan Krivanec <IPC.J/Krivanec@Ontyme> 
Subject: Delete user name .. 

Jan..

Please delete the following user names from the PDP10's
Names are valid only on systems F29 and D55.

 PASCAL10A
 PASCAL10B
 PASCAL10C
 PASCAL10D
 PASCAL10E
 PASCAL10F
 PASCAL10G
 PASCAL10H
 PASCAL10I
 PASCOOR
 PASDEV15
 PASDEV16
 PASDEV17
 PASDEV18
 PASDEV19
 PASDEV40
 PASGDOC
 PASGQA
 PASGSRC
 PASHOW
 PASIDOC
 PASIQA
 PASISRC
 PASPROC
 PASRELCR
 PASULIB
 PASXMIT

Thank you.
-Osman-
Received: from F29.Tymnet by D35.Tymnet; Thu, 5 May 88 14:05:57 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 5 May 88 14:05:09 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 5 May 88 14:00:46 PDT
Return-path: <IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet> 
From: IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet 
Date: 05 MAY 88 08:04:36 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A93602@Ontyme.Tymnet 
Subject: "Osman, The following users are"... 

Osman,    
     
The following users are valid on F26, which remains active. I only    
deleted users valid on F29 and D55.
     
Here are the users valid on F26:  PASGDOC    
                                  PASGQA
                                  PASGSRC    
                                  PASHOW
                                  PASIDOC    
                                  PASIQA
                                  PASISRC    
                                  PASPROC    
                                  PASRELCR   
                                  PASULIB    
                                  PASXMIT    
     
If these names need to be deleted, please let me know. 
     
Thank you,
Jan
From: Osman Guven <OSMAN@D35> 
Date: Thu, 5 May 88 15:56:24 PDT 
To: Jan Krivanec <IPC.J/Krivanec@Ontyme> 
Subject: More user name to delete .. 

Jan ..

Please delete the following user names from F26 also.
     
PASGDOC    
PASGQA
PASGSRC    
PASHOW
PASIDOC    
PASIQA
PASISRC    
PASPROC    
PASRELCR   
PASULIB    
PASXMIT    
     
Thank you.
-Osman-
Received: from F26.Tymnet by D35.Tymnet; Thu, 5 May 88 16:33:06 PDT
Return-path: <OSMAN@F26.Tymnet> 
From: OSMAN@F26.Tymnet 
Date: Thu, 5 May 88 16:32:00 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "To find out the reason of File""... 

To find out the reason of "Disk File" related crashes on systems
running with 3650/3652 disks in BLOCKS, F26 is running with a
diagnostic patch which makes "any correctable disk error" to a HDEV
error.

When a HDEV error accours on a file, it is marked HRE by the monitor.
The file is really "not bad" and copying the file to itself will clear
the HRE bit and the file is OK.

Don't be alarmed by the hard errors which the system picks up
any time it gets a read error.  If the system crashes just do
the normal things to bring the system up.

The diagnostic patch will be removed as soon as we have determined the
best way to handle read errors on 3650/3652 blocks systems.

Please note: The problem with handling read errors on 3650/3652 disks does
not occur if the disks are formated in pages.  It may be worth considering
the idea of converting the blocks systems to pages.  F74 has not been
having problems.

-Osman-
Received: from F26.Tymnet by D35.Tymnet; Thu, 5 May 88 16:50:26 PDT
Return-path: <OSMAN@D35> 
From: Osman Guven <OSMAN@D35> 
Date: Thu, 5 May 88 16:46:30 PDT 
To: Jerry Meyer <FSC.G/Meyer@Ontyme>, Tom Marconi <IPC.T/Marconi@Ontyme> 
Subject: "ECC ERROR" patch on F26 .. 

Just for your info ..
==============================================================================

To find out the reason of "Disk File" related crashes on systems
running with 3650/3652 disks in BLOCKS, F26 is running with a
diagnostic patch which makes "any correctable disk error" to a HDEV
error.

When a HDEV error accours on a file, it is marked HRE by the monitor.
The file is really "not bad" and copying the file to itself will clear
the HRE bit and the file is OK.

Don't be alarmed by the hard errors which the system picks up
any time it gets a read error.  If the system crashes just do
the normal things to bring the system up.

The diagnostic patch will be removed as soon as we have determined the
best way to handle read errors on 3650/3652 blocks systems.

Please note: The problem with handling read errors on 3650/3652 disks does
not occur if the disks are formated in pages.  It may be worth considering
the idea of converting the blocks systems to pages.  F74 has not been
having problems.

-Osman-

==============================================================================
Received: from X32.Tymnet by D35.Tymnet; Thu, 5 May 88 19:44:02 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 5 May 88 19:41:52 PDT 
To: Ed Barens <IPC.E/Barens@Ontyme>, Jim English <IPC.J/English@Ontyme>, Leland
	Yarbrough <IPC.L/Yarbrough@Ontyme>, Tom Marconi <IPC.T/Marconi@Ontyme>,
	Bill Fischer <IPC.B/Fischer@Ontyme>, Cheryl Eldred
	<IPC.C/Eldred@Ontyme>, Rick Daniels <IPC.R/Daniels@Ontyme>, Ed Roop
	<IPC.E/Roop@Ontyme>, Bill Richardson <FSC.B/Richardson@Ontyme>, Jerry
	Meyer <FSC.G/Meyer@Ontyme>, Ray Donahue <FSC.R/Donahue@Ontyme>, Steve
	Atwell <FSC.S/Atwell@Ontyme> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC 
Subject: Why all Dallas systems crashed last Friday. 

In the following table, the column labeled "CDT" lists when the Dallas
systems received "INFO stopcode BASE" or when they crashed, on 29-Apr-88.
"Reloaded" is when the node first got date/time after reload on 19-Mar-88.
"Host crashed" is when the node marked the host down (time is in GMT).

SYS    Node      Reloaded     Host crashed   CDT   Elapsed time
---   ------   ------------   ------------  -----  ------------
D23   ND6716   079:15:25:35   121:01:37:24  20:37  041:10:11:51
D25   ND7165   079:13:05:06   120:23:16:50  18:18  041:10:11:54
D31   ND7004   079:07:16:24   120:17:28:05  12:28  041:10:11:41
D34   ND4200   079:12:57:04   120:23:08:48  18:11  041:10:11:46
D35   ND2107   079:12:49:15   120:23:00:51  18:03  041:10:11:36
D37   ND3115   105:19:21:28    (25-May-88)
D54   ND4274   125:11:01:31   (not in log)  18:22  (41:10:11:xx)
D55   ND5577   079:14:53:14   121:01:07:57  20:07  041:10:14:43
D56   ND4725   079:15:39:35   121:01:51:21  20:51  041:10:11:54
D65   ND2332   079:12:57:55   120:23:09:37  18:30  041:10:11:42

Paul Krumviede came up with this diagnosis: The "FASTC" clock on the Engine
overflows every 41 days 10 hours 12 minutes 20 seconds.  When this occurs,
the base mistakenly thinks the PDP-10 is not responding and crashes the host.

My diagnosis is that this causes "INFO stopcode BASE" with KEY=0.  In the
process of outputing this message, the PDP-10 dies with an IME stopcode
(or possibly an APRFAT with NXM).

Until new EBUS base code is generated, I suggest reloading the base
at least once a month, while the PDP-10 is down for PM or ASP.

                        /Joe Smith
Received: from F29.Tymnet by D35.Tymnet; Sat, 30 Apr 88 14:05:51 PDT
Received: from D35.Tymnet by F29.Tymnet; Sat, 30 Apr 88 14:05:02 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Sat, 30 Apr 88 14:00:25 PDT
From: IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet 
Date: 29 APR 88 22:55:38 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A91364@Ontyme.Tymnet 
Subject: "Date: April 29, 1988 To: All System"... 
Resent-from: Tymcom-X Supervisor <TXSSUP> 
Resent-date: Thu, 5 May 88 19:47:44 PDT 
Resent-to: JMS, OSMAN, CARL, FLETCHERC 

Date:  April 29, 1988    
     
  To:  All System F-40 Users  
     
Copy:
     
From:  Resource Planning & Management   
     
------------------------------------------------------------------------   
     
     
System 40 at our Fremont Data Center is leaving tymsharing effective  
June 30, 1988. 
     
All Users will be moved to our Dallas Data Center - Host 43 or 47. A  
memo will be sent to your managers with details of the move.
     
If you have any questions, please escalate them to your manager, as we
will be in touch with them during the closure of F-40. 
     
Thank you,
     
Jan Krivanec   
RPM
Received: from F29.Tymnet by D35.Tymnet; Fri, 6 May 88 14:08:35 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 6 May 88 14:07:11 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 6 May 88 14:00:44 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 06 MAY 88 08:16:51 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.J/SMITH@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A94195@Ontyme.Tymnet 
Subject: "Joe; What is the earliest can"... 

Joe;

What is the earliest can we expect the EBUS base code to be re-generated?

Reference your Ontyme A94078.

Thanks 

Rick
Received: from F29.Tymnet by D35.Tymnet; Fri, 6 May 88 14:08:40 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 6 May 88 14:07:15 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 6 May 88 14:00:58 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 06 MAY 88 13:21:40 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (32 names) 
Message-id: A94418@Ontyme.Tymnet 
Subject: "M E M O R A N D U M DATE> 06 MAY"... 

                         M E M O R A N D U M





DATE>      06 MAY 88  13:20

TO>        All TYMCOM-10 Operators
           Shift Supervisors

COPIES>    Joe Smith
           Osman Guven
           Sue Pal
           Bill Richardson

FROM>      Rick Daniels


SUBJECT>   EBUSRELOAD


-----------------------------------------------------------------------


Reference TXS.J/SMITH Ontyme A94078 concerning EBUS base code reloads.

Starting the 15th of May, all TYMCOM-10'S scheduled down for ASP dumps
will have the EBUS reloaded during the dump.

If you do not know the string to load, call NETWORK Control and find out
what it is.  After doing the reload, SAVE the HARDCOPY for Bill RICHARDSON.

Until the EBUS base code is re-genned and loaded, the EBUS will be reloaded
once a month.

Any questions, please contact me.

Thanks

Rick
Received: from F33.Tymnet by D35.Tymnet; Fri, 6 May 88 20:11:04 PDT
Return-path: <Carl@F33.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Fri, 6 May 88 19:55:30 PDT 
To: Landa Morris <IPC.L/Morris@Ontyme.Tymnet>, Jan Krivanec
	<IPC.J/Krivanec@Ontyme.Tymnet> 
Cc: Monitor Group <CARL>, Osman Guven <Osman>, Joe Smith <JMS>, Craig Fletcher
	<fletcherc>, Ed Roop <IPC.E/Roop@Ontyme.Tymnet> 
Subject: PDP-10 Monitoring Tool: SYSDAT 

As of monday morning, May 9, 1988 there will be a job logged into (SRA) on
all PDP-10s running the program (SRA)SYSDAT.SAV.  THis job will write a log
file with the name <YY><MM><DD>.MON in directory (SRA) on each system.

This data includes all the system-wide information available at this time
for performance monitoring.  The system is identified in each record which
also contains CPU microcycles, frame, port and memory usage, disk capacity
and free space, user, monitor and swapping/paging i/o rates, base characters
read and written and block-i/o usage.

Please DO NOT log this job out.  It should be running whenever the system is
up for normal timesharing in order to monitor system performance.  At present
a snapshot is taken every 15 minutes and recorded.  (If the sample rate needs
to be adjusted, please let me know and I will change the program.)

/Carl

From: Osman Guven <OSMAN@D35> 
Date: Sat, 7 May 88 12:19:45 PDT 
To: Craig Fletcher <Fletcherc@D35> 
Subject: F26 ECC Memo .. 

To find out the reason of "Disk File" related crashes on systems
running with 3650/3652 disks in BLOCKS, F26 is running with a
diagnostic patch which makes "any correctable disk error" to a HDEV
error.

When a HDEV error accours on a file, it is marked HRE by the monitor.
The file is really "not bad" and copying the file to itself will clear
the HRE bit and the file is OK.

Don't be alarmed by the hard errors which the system picks up
any time it gets a read error.  If the system crashes just do
the normal things to bring the system up.

The diagnostic patch will be removed as soon as we have determined the
best way to handle read errors on 3650/3652 blocks systems.

Please note: The problem with handling read errors on 3650/3652 disks does
not occur if the disks are formated in pages.  It may be worth considering
the idea of converting the blocks systems to pages.  F74 has not been
having problems.

-Osman-
Received: from F29.Tymnet by D35.Tymnet; Mon, 9 May 88 14:05:41 PDT
Received: from D35.Tymnet by F29.Tymnet; Mon, 9 May 88 14:04:56 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Mon, 9 May 88 14:00:51 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 09 MAY 88 10:56:53 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A94984@Ontyme.Tymnet 
Subject: "M E M O R A N D U M DATE> 09 MAY"... 

                         M E M O R A N D U M





DATE>      09 MAY 88  10:47

TO>        All TYMCOM-10 Operators
           Shift Supervisors

COPIES>    Bill Fischer
           Ed Roop
           Jan Krivanec
            Tom Marconi
           Jim English
           Sue Pal
           Osman Guven
           Carl Baltrunas
           Joe Smith

FROM>      Rick Daniels


SUBJECT>   Converting F26 from BLOCK monitor to PAGE monitor


-----------------------------------------------------------------------


System F26 will be converted from a BLOCK monitor to a PAGE monitor
the weekend of 28-29 May.  The system will be scheduled down from
0000 (PDT), 28 May until approximately 2400 (PDT), 29 May.

At 0000 (PDT), 23 May, the following ACCESS.MSG will be placed on the
System:

    ATTENTION ALL USERS:

     The weekend of 28/29 May, this system will be converted from
     a BLOCK monitor to a PAGE monitor.  All Users are requested to
     back up their files on System 31.  It is anticipated F26 will
     unavailable until approximately 2400 (PDT), 29 May.
                                                                Page  2


NO ALL-FILES will be taken on 17 May.  An ALL-FILES, using SET #6,
will be started at 0000 (PDT), 24 May and will continue during ALL
SHIFTS until completed before 1600 (PDT) Friday the 27th.  If the
ALL-FILES completes before this time, a BACKUP will be taken.  A
FINAL backup will be taken after the System is SHUT.

Make a HARDCOPY directory of Tape #1 and keep with the ALL-FILES.

Final procedures for the conversion will be given to ALL SHIFTS the
week of 23 May.

Any questions, please contact me.

Thanks



Rick
Received: from F29.Tymnet by D35.Tymnet; Mon, 9 May 88 15:51:25 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Mon, 9 May 88 15:36:15 PDT 
To: <wrs@buffalo> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC 
Subject: Re: MCAUTO.Q was 1511 pages long. 
In-reply-to: <8805092051.AA10859@buffalo.Tymnet.com> of Mon, 9 May 88 13:51:31
	PDT

MCAUTO.Q has been taken care of.  I created a TECO-macro called SPLIT.TEC.
Using "MUNG SPLIT,MCAUTO.Q" produced MAILQ.100 thru MAILQ.348 - where each
message was stored in a seperate file.  I then used FINDIT to get a list
of the file names with their checksums.  Using the list sorted by checksums
I deleted all names from the list that had a duplicate checksum.  (For the
most part, only the messages from Office-8 showed up.)  The resultant CMD
file was given to "COPY @MAILQ.CMD" to append all the good messages together.
(Did you know the limit to "COPY 1+2+3+...+63,SUM" has a limit of 64 files?)
The end result was a new MCAUTO.Q that was 109 pages long, which was
processed with no further problems.  MAILQ.### and the old 1511 page MCAUTO.Q
have been deleted.

We have had similar problems twice before.  SMTP needs a command that will
refuse all mail from a particular host, giving it an error code that the
SMTP interface is temporarily out of service.

The following is the message I sent to Bill Daul:

From: Joe Smith <JMS@F29.TYMNET> 
Date: Wed, 4 May 88 21:24:19 PDT 
To: <CARTER@OFFICE-1.ARPA>, <DAUL@OFFICE-1.ARPA> 
Subject: Re: Use smaller messages for testing the MCAUTO gateway. 
In-reply-to: <MDC-TDC-DM964@OFFICE-1> of 4 May 88 22:02 CDT

This messsge is being sent to Dave and Bill, please pass it on to Young
if necessary.   /Joe

-----------------------------------------------------
Received: from OFFICE-1.ARPA by F29.Tymnet; Tue,y 88 14:16:16 PDT
From: William Daul / McAir / McDonnell-Douglas Corp <WBD.MDC@OFFICE-1.ARPA> 
Date: 3 May 88 14:10 PDT 
To: JMS%F29.Tymnet@OFFICE-1.ARPA 
Subject: TUMS and AUGMENT/OFFICE-8 (problem solved) 

I have renamed 3 suspect message.  Things should flow normally until we do it 
again (by accident) or find the problem.  --Bi((
----
From: Dave Carter McDonnell Douglas <TDC.MDC@OFFICE-1.ARPA> 
Date: 4 May 88 22:02 CDT 
To: Joe Smith <JMS%F29.TYMNET@OFFICE-1.ARPA> 

Joe - sorry the Tumsgate seems stressed.  My view is that I am not getting 
useable feedback from something.  Either Augment, or Tumsgate. I certainly 
never get any feedback from Profs.  The messages are representative of the 
reports we are distributing electronically and it is valuable to see if they 
make it completely in a useable form.

Is this the new Sun based Tumsgate?  If so, does that mean you are checking 
things out also?  We would treat it more delicately if we thought you were also
testing new stuff.  Too many variables at one time.

Thanks for your response to us.      Dave Carter
------------------------------------------------------------------

The TUMS gateway is system F29, a PDP-10 approximately 100 feet south of
Office-1 in Solar way.  Since Bill Soley has gone to the Turbo-Engine project,
the TUMS mail system is under the TYMCOM-X Support Group.  The TUMS gateway
has not changed for over 2 years.

I got involved when I notice that username MAIL was chewing up an inordinate
amount of CPU time.  There appeared to be two problems.
  1) The message was truncated, in that TUMS did not get the End-Of-Message
     indicator.  The log shows "time out waiting for command".  This is
     most likely a bug in the SMTP program on Office-8 because logged in
     another circuit and resent the entire message immediately.
  2) The message had no blank lines, and the body got smushed up into
     the header.
Problem #1 created a message queue that was 1511 disk pages long.  It
contained about 100 copies of 3 slightly different messages.  The only
difference was the subject: "Testing TUMS format with quick body" or something
to that effect.  When I sent the "please use smaller messages for testing"
message, I was under the assumption that TDC and/or YOUNG was repeatedly
sending messages manualy.  If Office-8 hadn't timed out or did not try to
resend the messages, there wouldn't have been a problem.

As a side note: I did see two copies of the message that did go out OK.
I believe it was 9 pages long and had lots of blank lines.  Sorry, I did not
take notice of the sender or recipient.

			/Joe



Received: from F33.Tymnet by D35.Tymnet; Tue, 10 May 88 9:41:06 PDT
Return-path: <DANIELSR@F33.Tymnet> 
From: DANIELSR@F33.Tymnet 
Date: Tue, 10 May 88 9:39:11 PDT 
To: OPER, DANIELSR, OSMAN 
Subject: "KEEP RUNNING ON HOST 33. THIS"... 


KEEP 'SUPCHK' RUNNING ON HOST 33.  THIS IS A NEW VERSION AND WE WANT
TO SEE IF IT IS SATISFACTORY.  IF ANYTHING UNUSUAL COMES OUT, SAVE
AND SEND TO ME.  THANKS RICK

Received: from F29.Tymnet by D35.Tymnet; Wed, 11 May 88 14:06:35 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 11 May 88 14:06:22 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Wed, 11 May 88 14:04:46 PDT
Return-path: <IPC.D/MILLER@EMSFSC.Ontyme.Tymnet> 
From: IPC.D/MILLER@EMSFSC.Ontyme.Tymnet 
Date: 11 MAY 88 12:09:14 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: A96108@Ontyme.Tymnet 
Subject: Customer survey results 

DATE> 11 MAY 88
 
FROM> Fremont SBI Group #2
 
 
SUBJ> Customer survey results
 
                              M E M O R A N D U M
 
-----------------------------------------------------------------------------
 
     Fremont data center thanks you for your participation in the customer
 
satisfaction survey. Based upon results of the survey, we have taken steps
 
to make improvements in the problems areas.
 
 
     As of this writing training classes have been implemented in all areas
 
of operations and distribution to enhance customer satisfaction to it's
 
highest possible level.
 
 
     We expect to be distributing a follow up survey sometime in June 88.
 
Continued participation would be greatly appreciated.
Received: from F74.Tymnet by D35.Tymnet; Wed, 11 May 88 14:30:56 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Wed, 11 May 88 14:30:10 PDT 
To: OPER, DANIELSR, OSMAN 
Subject: "YOU CAN NOW RUN THE NEW VERSION"... 


YOU CAN NOW RUN THE NEW VERSION OF "SUPCHK" ON ANY TYMCOM-10 HERE AT
FREMONT.

RICK
Received: from F29.Tymnet by D35.Tymnet; Wed, 11 May 88 17:52:36 PDT
Received: from B39.Tymnet by F29.Tymnet; Wed, 11 May 88 17:52:24 PDT
Received: from tymix.Tymnet by B39.Tymnet; Wed, 11 May 88 17:48:41 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02224; Wed, 11 May 88 17:50:54 PDT
Received: by antares.Tymnet.com (3.2/SMI-3.2) id AA01126; Wed, 11 May 88
	17:49:53 PDT
Return-path: <antares!jms@tymix.Tymnet> 
From: antares!jms (joe smith) 
Date: Wed, 11 May 88 17:49:53 PDT 
To: F29!CARL, F29!OSMAN 
Message-id: <8805120049.AA01126@antares.Tymnet.com> 
Subject: You can now send to JMS@Anteres 

Soley fixed tymix so that mail sent to username@Anteres now works.
From: Osman Guven <OSMAN@D35> 
Date: Wed, 11 May 88 23:30:25 PDT 
To: <antares!jms@tymix.Tymnet> 
Subject: Re: You can now send to JMS@Anteres 
In-reply-to: <8805120049.AA01126@antares.Tymnet.com> of Wed, 11 May 88 17:49:53
	PDT

Heeaayy its OK.  See if this gets to you .
-Osman-
Received: from F29.Tymnet by D35.Tymnet; Thu, 12 May 88 14:07:38 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 12 May 88 14:06:25 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 12 May 88 14:00:59 PDT
Return-path: <IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet> 
From: IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet 
Date: 12 MAY 88 07:53:47 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A96370@Ontyme.Tymnet 
Subject: help 

Osman,

I am sending you John Screech's memo. He is having difficulty with
files on F26. I've considered giving the users RF license. But some
of the users already have RF licence. So here I am, at a s.

Can you help him? Your a genius, thats why I'm asking you for help.

Call me if you have any questions.

Thanks so much,
Jan






                              MCDONNELL DOUGLAS
                             INFORMATION SYSTEMS

-------------------------------------------------------------------------
                           M E M O R A N D U M
-------------------------------------------------------------------------

                                                     MDISINET
                                                  NETWORK OPERATIONS
DATE>      12 MAY 88  12:00                          GROUP

TO>        JAN KRIVANEC

COPIES>    M.WALTERS UKOPS

FROM>      J.SCREECH


SUBJECT>   SYS 26


-----------------------------------------------------------------------


Jan,

    We noticed this problem Tuesday of last week,I cannot give you the names
of all the operators I personnaly spoke to,as I just asked for the sys 26
operator.I beleive Mike Walters spoke to a Mr lew.

   Our main problem is that due to the way Elf builds circuits to the DEC 10
systems,it is now impossible to access (SOURCE)ELFSLV.* because RF licence
is required.
   This problem is also seen when running (SOURCE)NAD from a command file
that does not have RF licence.

The user names we know have this problems are as follows.

BILLTEMP
MWALTERS
JSCREECH
TCLERK
BPTYME
                                                                Page  2


These are the names we know of,but their may be others who have not reported
the problem to us.


Best regards
John
Received: from F29.Tymnet by D35.Tymnet; Fri, 13 May 88 14:07:29 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 13 May 88 14:06:13 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 13 May 88 14:00:41 PDT
Return-path: <IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet> 
From: IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet 
Date: 13 MAY 88 08:38:50 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: UKNS.JSCREECH@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet 
Message-id: A96971@Ontyme.Tymnet 
Subject: System 26 

Date:  May 13, 1988 
     
  To:  John Screech 
     
Copy:  Osman Guven  
     
From:  Jan Krivanec 
     
Subj:  System 26    
     
-------------------------------------------------------------------------  
     
     
     
John,
     
Osman Guven, Software Analyist, has told me that your SOURCE file has 
a protected shield. It was once removed, then recently it was added   
again. I presume it was added last week.
     
Osman said the only way to get to this file is to use RF license to   
clear security to access this file. Please let me know which OPER names    
need RF license. I will check to see if they are valid with RF license.    
If not I will add the userids with license. I will also need OPER
passwords for the new users. Please let me know what they should be.  
     
For security reasons, I can see why the person of (SOURCE)ELFSLV.* has
put a shield up. Since we don't know who owns this file, we can not call   
to announce your troubles.    
     
I sorry that this news is unpleasant. We hope to get you back in working   
order as soon as possible.    
     
Regards,  
Jan
Received: from F29.Tymnet by D35.Tymnet; Fri, 13 May 88 19:37:24 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 13 May 88 19:19:28 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, WRS 
Subject: UPDUFD.PAT created to try to solve UFDER1 crashes on F29. 

It looks like the MAIL programs were trying to set the "dumped" bit on a
file that was marked for deletion.  The code in FILUUO:CLOSIN at CLOSRN
says to jump to CLSIN2 only if no one else is reading the file and the
file is NOT marked for deletion.  The crash shows that the ATB got marked
for deletion between the time it took to get from UPDUFD+0 to UPDUFD+14.
Since that section takes two page faults, there was time for this to occur.

Scenario:
1) Job 1 runs ONTYME or SMTPX or other MAIL program that creates *.FEM file.
2) Job 1 does a CLOSE trying to set the dumped bit (so that TITO won't store
   the FEM file to tape).  At the time, no other job is looking at the file.
3) While doing the CLOSE, job 1 gets a page fault while reading the RIB of
   the UFD and gets a page fault while reading the UFD data page.
4) While job 1 is waiting for disk I/O, jobs 2 deletes the file.
5) Job 2 lucks out, finds the UFD data before job 1 gets a chance to run, and
   the monitor removes the FEM file from the UFD data page.
6) Job 2 finally gets a chance to read the UFD data page, can't find the
   file (because it was deleted).   Crash with UFDER1.

Fix: Before calling DIRSRC to search the directory, make sure ATPDEL hasn't
     gotten set.
Received: from X32.Tymnet by D35.Tymnet; Sat, 14 May 88 11:35:22 PDT
Return-path: <OSMAN@D35> 
From: Osman Guven <OSMAN@D35> 
Date: Sat, 14 May 88 11:33:35 PDT 
To: Monitor Group <OSMAN>, Joe Smith <JMS@D35>, Carl Baltrunas <Carl@D35> 
Cc: Craig Fletcher <Fletcherc@D35> 
Subject: X32 rebuild on 3652P .. 

X32 is rebuild from ALL-FILES and BACKUP tapes.  All the files are up
to date and nothing is lost.  It is build on 4 pack, 3652 in pages.
After the rebuild and ASP dump was taken and all went OK.  Changed DSKTYP
to 3652 in CONF32.MAC and rebuild a X32-P035/D05 monitor.  System is up
and running with it.  I think we can consider this to be alfa testing for
F26's rebuild in pages on 5/28/88.

-Osman-
From: Postmaster@D35.Tymnet 
Date: Sat, 14 May 88 17:10:02 PDT 
To: Osman Guven <OSMAN@D35> 
Subject: Returned MAIL 

The following message was not delivered because:
User has not read mail in 30 days: RICHARDSON
Last logout: 29 Apr 88


----------
RCPT To: <RICHARDSON> 
From: Osman Guven <OSMAN@D35> 
Date: Fri, 15 Apr 88 14:09:50 PDT 
To: FLETCHERC 
Cc: Bill Fischer <IPC.B/Fischer@Ontyme>, DANIELSR, RICHARDSON, SALTYRON@39,
	BBUNYAN@39, JSTIER@39, KALENDA@39, JMS, CARL 
Bcc: OSMAN@D35.Tymnet 
Subject: Suggestive steps for B39 .... 

To isolate and/or fix the problem B39 is having, these are
suggestive steps to take.

o  Most of the system crashes in last 2 weeks always started
   with TTYZNE event stopcode, follewed by some number of BASE
   event stopcodes and than crashed with some kind of STOP 
   stopcode.  To find out more information about the crashes, 
   made the TTYZNE a STOP stopcode and monitor should goto
   BOOTS when it detects it.  A crash dump should be taken to 
   be  analized.

o  B39's Base code should be tested running with the same
   base code as the rest of the systems, which is:
   Node code version 5.10
   ISIS code version 7.01
   to see if B39 will run as well as the other PDP10s.

o  Put diagnostic patch in monitor to save I/O ring messages 
   between the Base and the PDP10 to find out the message sequences 
   just prior to crash.

o  Run one of the Fremont D. C. hosts with the same base code
   version as B39:
   Node code version: 5.22
   ISIS code version: 7.03
   to see if it will experience the same problem as B39 is having.

-Osman-
Received: from F74.Tymnet by D35.Tymnet; Mon, 16 May 88 8:57:32 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Mon, 16 May 88 8:53:22 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman; How id the PAGE monitor"... 


Osman;

How id the PAGE monitor go with your test on X32 Dual Density drives?

Did the present (SYS)SYSTEM.SAV monitor wrork?  Can a BLOCK monitor
be used for convertng to a PAGE system?

Will F26's present (SYS)SYSTEM.SV monitor be okay for the conversion
to PAGES?

Will be back in the office on 23 May.

Thanks Rick
From: Osman Guven <OSMAN@D35> 
Date: Mon, 16 May 88 13:05:31 PDT 
To: <DANIELSR@F74.Tymnet> 
Subject: Re: "Osman; How id the PAGE monitor"... 
In-reply-to: your message of Mon, 16 May 88 8:53:22 PDT

Test of PAGE rebuild on X32 using 3652 went OK.  First I used F26's
monitor, after the rebuild I build X32's own PAGE monitor and the
system is up and running with it now.

Present monitor on F26 will work when doing the PAGE coversion.


BLOCK monitor will not work on PAGE system.

-Osman-


Received: from F29.Tymnet by D35.Tymnet; Tue, 17 May 88 14:11:36 PDT
Received: from D35.Tymnet by F29.Tymnet; Tue, 17 May 88 14:09:47 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Tue, 17 May 88 14:00:59 PDT
Return-path: <IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet> 
From: IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet 
Date: 17 MAY 88 07:49:32 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A98196@Ontyme.Tymnet 
Subject: "Osman, I have more information"... 

Osman,

I have more information on John Screech's dilemna. You can
ontyme them at 

 UKNS.MWALTERS
 UKNS.JSCREECH
 TYMOPS.NSSC

This new ontyme has me somewhat baffled.

Jan



=======================  M E M O R A N D U M  =========================

                                          Principal Network Consultant
                                                McDonnell Douglas
                                               Information Systems
DATE>      16 MAY 88  11:34                      (UKNS.MWALTERS)

TO>        Jan Krivanec

COPIES>    Osman Guven
           NSSC
           John Screech

FROM>      Mike Walters


SUBJECT>   Protection Changes on system 26


-----------------------------------------------------------------------


Jan,

John is out of the office today so I will respond on his behalf.

The problem we are seeing has nothing to do with our personal RF licenses.

The problem can be defined as follows:-

When one is using ELF to load into a private network one must log into
the Dec-X system to "pull" down code for future loading. This is done by
instructing ELF to login and ELF automatically runs something that calls
the (SOURCE)ELFSLV program.
     This 'something' is now unable to start ELFSLV. If this were to happen
     on system 31 as well, ALL private networks supported from these
     2 hosts would be unable to function any further!

When we in the UK are changing code on our network we do final assemblies
via the PCOM/DCOM program in order to leave a trace file of the assembly
output.
     PCOM and DCOM are unable to read (SOURCE)NAD program anymore.

I believe that the problem is that either
     a) the RF licenses have been taken away from these programs
or   b) The Source directory has been changed.
                                                                Page  2


WHen referred to Jeff Liu at NTD on the ELF problem, he did manage to correct
the situation for a short time, but it came back.

I trust this will help in finding out what is going wrong.

Regards
Received: from F33.Tymnet by D35.Tymnet; Tue, 17 May 88 15:43:09 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 17 May 88 15:38:06 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: Long way to Fremont. 

From South Bay Center, there is one 56Kb line to Nicholson Lane.
When we login to the Fremont hosts, the circuit goes to Nicholson then to
Fremont.  Execpt when that one 56Kb line is down.  Today, logins are going
from South Bay Center to Dallas, from Dallas to Nicholson Lane, from
Nicholson Lane to Fremont.   Wonderful, yes?
Received: from D37.Tymnet by D35.Tymnet; Tue, 17 May 88 16:20:15 PDT
Return-path: <JMS@D37.Tymnet> 
From: JMS@D37.Tymnet 
Date: Tue, 17 May 88 18:15:24 CDT 
To: OPER, MARCONIT, SRA, MEYERG, JENGLISH, BILLF, LELANDY, OSMAN, CARL, JMS,
	ZONE, SCRIBNER, RODDAMJ, TYMRES5, PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "At approximately 6:10pm CDT, two"... 

At approximately 6:10pm CDT, two messages appeared on all terminals.
Something saying "TO ALL USERS, THE SWING SHIFT IS NOW ON DUTY -- MDC".
Messages like this cause all sorts of confusion to our users, desMessages like this cause all sorts of confusion to our users, destroying
screen displays and putting unwanted output on important hardcopy listings.
Please make sure this does not happen again.

			/Joe Smith
			Tymcom-X Support Group
Received: from D37.Tymnet by D35.Tymnet; Tue, 17 May 88 20:21:38 PDT
Return-path: <JENGLISH@D37.Tymnet> 
From: JENGLISH@D37.Tymnet 
Date: Tue, 17 May 88 22:18:38 CDT 
To: OPER, MARCONIT, SRA, MEYERG, JENGLISH, BILLF, LELANDY, OSMAN, CARL, JMS,
	ZONE, SCRIBNER, RODDAMJ, TYMRES5, PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "I have talked to the Operator"... 

I have talked to the Operator responsible for sending that
message and want you to know that I have been assured it
will not happen again. Please accept our apologies and hope
it did not upset too many users.
 
       Jim English
       Second Shift Supervisor
       Dallas Operations
Received: from F33.Tymnet by D35.Tymnet; Tue, 17 May 88 20:28:02 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 17 May 88 20:13:02 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: What's in OSP. 

Directory OSP has the production monitor only.  (No patch files, no patched
monitors.)  On each system, OSP has P035D.MEM, P035D.DOC, STOPCD.MEM, and the
appropriate ##035D.SAV file.  A backup copy of ##035D.SAV is also on (OSP:33)
and 33's duplicate is on (OSP:34).

DMON.PAT is in M33 on all systems and has all patches up thru #7 for UFDER1.

The ##035D.SAV file in M33 on all systems was created from (OSP)##035D.SAV
using DMON.PAT.  They have the "###-P035/D-7" monitor.

Example of use:

System 37 added a second string of 3652 disks in March.  
1) Edit (M33:33)CONF37.MAC to add 2nd string, then "CPY CONF37.MAC/HOST:32".
2) GFD to OSP and run MONED1 there to create a new 37035D.SAV
3) TELECOPY from (OSP:33)37035D.SAV to (OSP:37)37035D.SAV
4) Login to 37 and apply DMON patches:
   GET (OSP)37035D
   DDT
   ^Y (M33)DMON.PAT
   SAVE (M33)37035D

To repeat: OSP has the virgin, unpatched monitor.  M33 has patches.

		/Joe
Received: from F33.Tymnet by D35.Tymnet; Tue, 17 May 88 20:36:51 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 17 May 88 20:30:36 PDT 
To: wrs 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC 
Subject: (MAIL:33)______.MAI 

It looks like someone with WC tried to sabotage the "rdmail -a" command.
The file _______.MAI has author PPN of [777777,777777] and has the message

  RCPT To: <requestop>
  From: REQUESTOP@F33.Tymnet 
  Date: Tue, 10 May 88 20:06:56 PDT 
  To: requestop 
  
  foo on you
  ^D

This causes RDMAIL to attempt to read block -1 of the DUL.

Received: from F29.Tymnet by D35.Tymnet; Wed, 18 May 88 14:04:15 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 18 May 88 14:04:03 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 18 May 88 14:00:48 PDT
Return-path: <IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet> 
From: IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet 
Date: 18 MAY 88 11:43:20 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: UKNS.MWALTERS@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	UKNS.JSCREECH@Ontyme.Tymnet, TYMOPS.NSSC@Ontyme.Tymnet 
Message-id: A99022@Ontyme.Tymnet 
Subject: SOURCE 

Date:  May 18, 1988 
     
  To:  Mike Walters   (UKNS.MWALTERS)   
     
Copy:  Osman Guven    (TXS.O/GUVEN)
       John Screech   (UKNS.JSCREECH)   
       NSSC           (TYMOPS.NSSC)
     
From:  Jan Krivanec   (IPC.J/KRIVANEC)  
     
Subj:  SOURCE  
     
 Ref:  Ontyme S46841
     
------------------------------------------------------------------------   
     
     
Mike,
     
Osman called me this morning and told me that SOURCE which was protected,  
in now unprotected. He could not find the owner of this id to explain 
the problems you were having. Osman said that you should not have any 
problems now. If you do, please let us know immediately.    
     
Thank you for your patience in dealing with this dilemna.   
     
Best regards,  
Jan
Received: from F29.Tymnet by D35.Tymnet; Sat, 21 May 88 23:22:20 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Sat, 21 May 88 23:20:01 PDT 
To: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
Cc: osman 
Subject: Re: "Joe; What is the earliest can"... 
In-reply-to: A94195@Ontyme.Tymnet of 06 MAY 88 08:16:51

The new base code is stored in (EBUS:33)NW####.BND.
Duplicate copies are on (EBUS:25) and (EBUS:54) as appropriate.

Osman plans to test this new code before putting it into production.

			/Joe
Received: from F74.Tymnet by D35.Tymnet; Mon, 23 May 88 6:10:34 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Mon, 23 May 88 6:09:24 PDT 
To: CARL, OSMAN, DANIELSR 
Subject: "Carl; You can install the UPDATED"... 


Carl;

You can install the UPDATED ASP software on all our System anytime you
want.  Please let me know when it has been done.

Thanks 
rick

Received: from F74.Tymnet by D35.Tymnet; Wed, 25 May 88 8:05:57 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Wed, 25 May 88 8:04:44 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman; Have one item to talk about"... 


Osman;

Have one item to talk about on F26's conversion to PAGE monitor.

Forgot to mention it to you yesterday.

Thanks Rick
From: Tymcom-X Supervisor <TXSSUP> 
Date: Wed, 25 May 88 21:58:45 CDT 
To: osman, carl 
Subject: FSC Internal User move F-40 to D-43 

(begin forwarded message)
----------------------------------------------------------------
Received: from F29.Tymnet by D35.Tymnet; Wed, 25 May 88 14:05:50 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 25 May 88 14:04:59 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 25 May 88 14:00:31 PDT
Return-path: <IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet> 
From: IPC.J/KRIVANEC@EMSTXS.Ontyme.Tymnet 
Date: 24 MAY 88 23:39:49 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A01722@Ontyme.Tymnet 
Subject: FSC Internal User move F-40 to D-43 

Date:  May 24, 1988 
     
  To:  FSC Internal Users valid on host F-40 
       TXS Internal Users valid on host F-40 
     
Copy:
     
From:  Jan Krivanec 
     
Subj:  FSC Internal User move F-40 to D-43   
     
------------------------------------------------------------------------   
     
     
     
In two weeks, I will be moving your accounts from F40 to D43. The move
will begin at 5pm pdt on June 3rd. 
     
A previous memo was sent to you on May 4th, listing all users found   
on F40. Several ids were found valid on a Dallas IBM host. These users
are responsible for moving their own files to Dallas. A memo regarding
information to transfer files to Dallas was sent on May 10th.    
     
Again, here is the list of userids that I will be moving to D-47:
     
MAINTFSC  
ABELE
AGTDMS    
CSS1 
CSS10
CSS11
CSS5 
CSS6 
CSS7 
CSS8 
CSS9 
DEPUY
FINNEGAN  
JACKT
LARRYB    
MIKEA
MORTON    
TDFOCUS   
TDMS 
TDMSDB    
BAIGENT   
BSTRONG   
CUSTIN    
DOC2 
NDT  
OSU  
BRUCES    
FSCSITE   
JCARTER   
WFACDB    
WNFE 
CADPLOT   
CADPRO3   
CADPRO4   
CADPRO5   
CADTEST   
DWALPOLE  
HVBURTON  
JCCHEN    
JWILLOTT  
MIKEMO    
MSTRBRD   
NORTHRIP  
RBROWN    
SOLOMON   
SULRICH   
JCM  
     
And again, this is the list of userids that are responsible for moving
their files to Dallas by the closing of June 30th.
     
1213MGR   
OSMAN
TXSDOC    
ANVERM    
MOORING   
PKRUMV    
SALTYRON  
BUDGET    
     
And finally, this list contains users only valid on F-40 with one file:    
MAIL ADDRESS. These users will be removed from the move and will be   
removed from F40 on May 31, 1988:  
     
ARELLA    
FACILITY  
FSCLI
LAUVER    
WAGNERD   
WWPC 
WWPM 
BILLROD   
BROWNR    
CHINJEFF  
DBLISS    
GOLOBIC   
GUINASSO  
JGIUSTI   
KENNEDY   
KOLICZEW  
PATELN    
     
To ensure a smooth center-to-center customer move, the users that I am
moving must be FREE of bad files. If BAD FILES  are found on the day of    
the move, I will delete them from their directory.
     
Any users who have BATCH jobs, must be canceled on June 3rd and re-   
submitted on D-47.  
     
The users involved in this move, must be logged off at the time of the
move. This includes disconnect users. Any users found logged on when  
the move begins, will be messaged to log off within two minutes. After
a two minute lapse, the users will be forced off. 
     
If you have any questions, I can be reached at (415)498-2556.    
     
Thank you,
Jan
----------------------------------------------------------------
(end forwarded message)

Received: from F74.Tymnet by D35.Tymnet; Thu, 26 May 88 11:26:24 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Thu, 26 May 88 11:24:30 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman; The username 10UPDATE is"... 


Osman;

The username 10UPDATE is now on all TYMCOM-10's, except D23.  As soon
as it is up, the username will be validated on.

The password is CHANGE IT.  You can change it to whater you want>

Thansk Rick

From: Osman Guven <OSMAN@D35> 
Date: Thu, 26 May 88 15:12:04 PDT 
To: Jan Krivanec <IPC.J/Krivanec@Ontyme> 
Subject: F40 to D43 move .. 

Jan ..

Please delete the following user names from F40, these user names
don't need to be moved to D43.
==========
BAIGENT
BSTRONG
TDMS
TDMSDB


Please move the followig user name from F40 to D43:
==========
OSU

Thank you.
-Osman-
From: Osman Guven <OSMAN@D35> 
Date: Thu, 26 May 88 15:51:12 PDT 
To: Rick Daniels <IPC.R/Daniels@Ontyme> 
Subject: ASP 7(110) version .. 

Rick ..

(FTSYS)ASP.SAV on all sytem is the new version "ASP 7(110).
Please have the OPER use it from (FTSYS) for the next 3-4 weeks.
When you satisfied with it, then we move it to (SYS).  Thank you.

-Osman-
Received: from F33.Tymnet by D35.Tymnet; Thu, 26 May 88 17:45:16 PDT
Return-path: <Carl@F33.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Thu, 26 May 88 17:43:01 PDT 
To: Osman Guven <Osman>, Joe Smith <JMS> 
Subject: 2400 Modem 

Just wanted to let you know that the modem works fine.

432-8618 appears to be the only 2400 number around SJ.
If I get a chance I will look to see what else is around in the PORTS
database (although I guess INFORMATION is just as good, albeit a little
out of date).

/Carl
Received: from F33.Tymnet by D35.Tymnet; Fri, 27 May 88 8:23:07 PDT
Return-path: <Carl@F33.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Fri, 27 May 88 2:34:50 PDT 
To: Joe Smith <JMS>, Osman Guven <Osman> 
Subject: New SYSDAT %2(15) on (CARL:33) 

There is a newer version of SYSDAT on (CARL:33) and (SRA:33).

I have also started a telecopy to all systems, but 14,17,25,30,32,35 are
down at the moment (or were) please check ???TEL.OUT w/ TELCHK for more
details.  TELECOPY to (CARL:*).  Sometime over the weekend I will put the
new copy from CARL into SRA so the next recycle will run th enew version.

The new version prints the datestamp for each line on the current line
based on the time of the first character of the line instead of the old
LF character.

Received: from F74.Tymnet by D35.Tymnet; Sat, 28 May 88 10:01:37 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Sat, 28 May 88 9:58:54 PDT 
To: OSMAN, OPER, DANIELSR 
Subject: "OSMAN; WHEN USING OPERQM, THE"... 


OSMAN;

WHEN USING OPERQM, THE PASSWORD WOULD NOT WORK.  THE PASSWORD WAS THE
SAME AS WE SET UP.  TRIED THREE TIMES, BUT NO AVAIL.  WORKED OKAY ON
SYSTEM 74.

ASK THE OPERATORS TO CALL SOFTWARE SUPPORT AFTER ALL THE ALL-FILE AND
BACKUP TAPES ARE RESTORED SO SOMEONE CAN GIVE (SUBMIT)SUBMIT.SAV ISLICENSE
BEFORE RUNNING THE FINAL DISK CLEARN.

THANKS RICK
Received: from F29.Tymnet by D35.Tymnet; Tue, 31 May 88 14:04:28 PDT
Received: from D35.Tymnet by F29.Tymnet; Tue, 31 May 88 14:04:17 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Tue, 31 May 88 14:00:43 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 31 MAY 88 09:24:29 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A03898@Ontyme.Tymnet 
Subject: "M E M O R A N D U M DATE> 31 MAY"... 

                         M E M O R A N D U M





DATE>      31 MAY 88  09:23

TO>        Dick Kovach
           Vida Stafford
           Barbara Victorino

COPIES>    Bill Fischer
           Ed Roop
           Osman Guven
           Cheryl Eldred

FROM>      Rick Daniels


SUBJECT>   Converting System 33 to PAGE monitor


-----------------------------------------------------------------------


Plans are being formulated to convert System 33 from a BLOCK
monitor to a PAGE monitor starting on 2 July.  This conversion needs
to be done by ALL-FILES and will take approximately FIVE (05) DAYS
to complete once started.  Converting to a PAGE monitor will increase
the file storage by eleven percent (11%) and will handle the READ ERRORS
more effectively.

The system will be unavailable from 0000, 2 July (PDT) until approximately
2400, 6 July (PDT).

If this timeframe is unacceptable, please give us an alternate
timeframe.

Please contact McDonnel Douglas, Fremont Operations at (415) 498-2595
or Ontyme IPC.R/DANIELS with your proposals by 10 June.

Thanks
                                                                Page  2


Rick Daniels
Received: from F74.Tymnet by D35.Tymnet; Thu, 2 Jun 88 8:37:11 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Thu, 2 Jun 88 8:36:01 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman; Please type out file 33PAGE.MEM"... 


Osman;

Please type out file 33PAGE.MEM under my username DANIELSR:74.

Let me know if you have comments?

Thansks Rick
Received: from D54.Tymnet by D35.Tymnet; Thu, 2 Jun 88 17:56:19 PDT
Return-path: <OSMAN@D35> 
From: Osman Guven <OSMAN@D35> 
Date: Thu, 2 Jun 88 17:42:35 PDT 
To: Rick Daniels <IPC.R/Daniels@Ontyme> 
Subject: New base codes .. 

Rick ..

Sorry I took so long to get the BASE CODES in right system
and right directories and still having problem with it.
Problem is: files are being deleted by some automated process
from (TYMNET) and (TYM5) directories.  Untill I find out what
is going on and why, you could use (EBUS) user name instead of
(TYMNET) and (TYM5) on D25, D54, F33.  (EBUS) userame is valid
on 25, 54, 33 and have the BASE CODES for all the systems.
Name of the BASE CODES are NWxxxxx.BND.

I will not be in tomorrow but if you have any question give me
a call at home or send mail. (408)224-5457.  Joe will be in
tomorrow if you want and need to you can call him at work also.

-Osman-

From: Osman Guven <OSMAN@D35> 
Date: Thu, 2 Jun 88 17:57:30 PDT 
To: jms 
Subject: New base codes .. 

(begin forwarded message)
----------------------------------------------------------------
Received: from D54.Tymnet by D35.Tymnet; Thu, 2 Jun 88 17:56:19 PDT
Return-path: <OSMAN@D35> 
From: Osman Guven <OSMAN@D35> 
Date: Thu, 2 Jun 88 17:42:35 PDT 
To: Rick Daniels <IPC.R/Daniels@Ontyme> 
Subject: New base codes .. 

Rick ..

Sorry I took so long to get the BASE CODES in right system
and right directories and still having problem with it.
Problem is: files are being deleted by some automated process
from (TYMNET) and (TYM5) directories.  Untill I find out what
is going on and why, you could use (EBUS) user name instead of
(TYMNET) and (TYM5) on D25, D54, F33.  (EBUS) userame is valid
on 25, 54, 33 and have the BASE CODES for all the systems.
Name of the BASE CODES are NWxxxxx.BND.

I will not be in tomorrow but if you have any question give me
a call at home or send mail. (408)224-5457.  Joe will be in
tomorrow if you want and need to you can call him at work also.

-Osman-

----------------------------------------------------------------
(end forwarded message)

From: OSMAN@D35.Tymnet 
Date: Fri, 3 Jun 88 21:44:09 PDT 
To: osman 
Subject: "Received: from F29.Tymnet by D35.Tymnet; "... 

Received: from F29.Tymnet by D35.Tymnet; Fri, 3 Jun 88 14:06:37 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 3 Jun 88 14:06:19 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 3 Jun 88 14:00:42 PDT
Return-path: <OPERA.SUP@EMSTXS.Ontyme.Tymnet> 
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 03 JUN 88 07:42:41 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (30 names) 
Message-id: A05478@Ontyme.Tymnet 

SUBJ: 





                        M E M O R A N D U M

                                                 MC DONNELL-DOUGLAS IPS




DATE>      03 JUN 88  07:41

TO>        All TYMCOM-10 Operators

COPIES>    Shift Supervisors

FROM>      Rick Daniels


SUBJECT>   NEW EBUS BASE CODE


-----------------------------------------------------------------------


Starting this weekend subject code will be loaded until all our PDP-10's
have the new code.

Code will be loaded from the username (EBUS) on systems 25, 33, or 54.
Name of EBUS BASE CODE is:  NWxxxx.BND.  NOTE THE NEW FILENAME.

Load from this username, and filename, until further notice.

B39 and X32 will be loaded in their usual way.  No change to the loading
procedures on these two systems.

Thanks

Rick Daniels
Received: from F29.Tymnet by D35.Tymnet; Fri, 3 Jun 88 14:07:24 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 3 Jun 88 14:07:08 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Fri, 3 Jun 88 14:04:58 PDT
Return-path: <FSC.C/FLETCHER@EMSFSC.Ontyme.Tymnet> 
From: FSC.C/FLETCHER@EMSFSC.Ontyme.Tymnet 
Date: 03 JUN 88 09:19:41 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P69420@Ontyme.Tymnet 
Subject: STANDBY FOR JUNE 1988 









                         MCDONNELL DOUGLAS FIELD SERVICE COMPANY
                         MCDONNELL DOUGLAS FIELD SERVICE COMPANY

                                    Inter-Office Memo

=================================================================

TO:         Distribution
FROM:       Craig Fletcher
DATE:       31 March 1988
SUBJECT:    Standby For April 1988


Here is the Software Standby Schedule for May:


   Week beginning:       5/02/88           C. Baltrunas
                         5/09/88           J. Smith
                         5/16/88           O. Guven
                         5/23/88           C. Baltrunas
                         5/30/88           J. Smith


Regards, Craig

cc: C. Baltrunas, O. Guven, J. Smith, J. Logsdon, R. Rawlins
Received: from F29.Tymnet by D35.Tymnet; Fri, 3 Jun 88 14:07:27 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 3 Jun 88 14:07:11 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Fri, 3 Jun 88 14:05:07 PDT
Return-path: <FSC.C/FLETCHER@EMSFSC.Ontyme.Tymnet> 
From: FSC.C/FLETCHER@EMSFSC.Ontyme.Tymnet 
Date: 03 JUN 88 09:43:05 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P69430@Ontyme.Tymnet 
Subject: STANDBY FOR JUNE 1988 









               MCDONNELL DOUGLAS FIELD SERVICE COMPANY

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>MEMO<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


TO:         Distribution (PLEASE disregard P69420 Ontyme) 
FROM:       Craig Fletcher
DATE:       3 June 1988
SUBJECT:    Standby For June 1988


Here is the Software Standby Schedule for June:

   Week beginning:       6/06/88           O. Guven
                         6/13/88           C. Baltrunas
                         6/20/88           J. Smith
                         6/27/88           C. Baltrunas



Regards, Craig

cc: C. Baltrunas, O. Guven, J. Smith, J. Logsdon, R. Rawlins
Received: from F29.Tymnet by D35.Tymnet; Fri, 3 Jun 88 17:28:29 PDT
Return-path: <JMS@F29.Tymnet> 
From: JMS@F29.Tymnet 
Date: Fri, 3 Jun 88 17:26:06 PDT 
To: osman 
Subject: "X17 is down it printed out 60"... 

X17 is down - it printed out 60 pages of "Unit RMA0 Hung" before the CTY ran
out of paper.  I tried to restart it, but the system halts with
%HLTD 000102,,7xxxxxx.  I'm not sure what 102 means, but the PC is not that
of a HALT instruction.     /Joe
tums
Received: from F33.Tymnet by D35.Tymnet; Sat, 4 Jun 88 15:03:08 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Sat, 4 Jun 88 15:01:48 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: 74XD06 and 33XD06 are ready. 

Monitors for the two production-test machines are in M33 on F74 and F33.
Received: from F33.Tymnet by D35.Tymnet; Sat, 4 Jun 88 15:12:51 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Sat, 4 Jun 88 15:03:07 PDT 
To: osman 
Subject: Flight schedules. 

Sally plans to be at the airport by 10:00 to get on United flight 234 which
leaves at 10:52am Thursday June 9th.  My mother and grandmother arrive at
10:35am on Continental flight 839.  I will tell them to meet you at the
baggage claim, with you in a TYMSHARE hat.

I have booked them at the Best Western San Jose Lodge at 1440 N 1st St.
That way they can ride the trolley to downtown San Jose.

I will let you know if there are any further details.
			/Jo
Received: from F33.Tymnet by D35.Tymnet; Mon, 6 Jun 88 7:16:14 PDT
Return-path: <DANIELSR@F33.Tymnet> 
From: DANIELSR@F33.Tymnet 
Date: Mon, 6 Jun 88 7:14:37 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman; Please re-check the NW6720.BND"... 


Osman;

Please re-check the NW6720.BND file for System 58.  See crash mail sent by
Operators.  If it is okay, I will have them retry tonight.

Thanks Rick
Received: from F74.Tymnet by D35.Tymnet; Mon, 6 Jun 88 8:34:11 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Mon, 6 Jun 88 8:32:27 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman; Can you move to Feel comfortable"... 


Osman;

Can you move (FTSYS)ASP.SAV to (SYS)?  Feel
comfortable with this version.  Send mail when completed.

Thanks Rick

Received: from F29.Tymnet by D35.Tymnet; Mon, 6 Jun 88 14:05:16 PDT
Received: from D35.Tymnet by F29.Tymnet; Mon, 6 Jun 88 14:05:00 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Mon, 6 Jun 88 14:00:43 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 06 JUN 88 10:07:12 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: FSC.B/KERSTEN@Ontyme.Tymnet, TXS.C/FLETCHER@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	TXS.J/SMITH@Ontyme.Tymnet, IPC.B/FISCHER@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A06251@Ontyme.Tymnet 
Subject: "M E M O R A N D U M DATE> 06 JUN"... 

                         M E M O R A N D U M





DATE>      06 JUN 88  10:02

TO>        Bill H. KERSTEN

COPIES>    Craig FLETCHER
           Carl Baltrunas
           Osman Guven
           Joe Smith
           Bill Fischer

FROM>      Rick Daniels


SUBJECT>   Letter Of Appreciation


-----------------------------------------------------------------------


During the past 18 months, the TYMCOM-10 Software Support Group, has
been a tremendous help in solving some long overdue problems on
these systems.

They have put in long hours to provide updated software for the
conversion of these systems from MEMOREX disk drives 3675's to 3650's
and 3652's; from BLOCK monitor to PAGE monitor; updated disk recovery
software; and numerous other software changes to improve the 
availability of these system for our customers.

The Fremont Data Center wishes to express our appreciation to:

     Craig FLETCHER

     Carl BALTRUNAS

     Osman GUVEN

                                                                Page  2

     Joe SMITH

for their untiring dedication, long hours and support for a smooth
transition during this period.
Received: from F74.Tymnet by D35.Tymnet; Tue, 7 Jun 88 7:41:10 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 7 Jun 88 7:39:30 PDT 
To: OSMAN, DANIELSR 
Subject: "Osman The reload on F29 using"... 


Osman

The reload on F29 using (EBUS:54)NW6415.BND three times did not work.

Brought up, reloaded EBUS with (TYM5)ND6415.BND.

Thanks Rick
From: Carl A Baltrunas <Carl> 
Date: Tue, 7 Jun 88 10:24:04 PDT 
To: oper@f30, billf@f30, Monitor Group <Carl>, Osman Guven <Osman>, Joe Smith
	<JMS>, Craig Fletcher <fletcherc>, danielsr@f30, michaelb@f30, sra@f30,
	richardson@f30, pkrumv@f33, fsc.r/donahue@ontyme 
Subject: F30 problems & recovery 

Due to the apparent hardware problems with drive A1, it was not possible to
bring the system up for timesharing. The system was able to be brought up in
manual mode for disk inspection, however, none of the system files in (SYS)
were available.

I logged in "REFRESH MODE" and was able to run the disk reading programs and
disk repair programs (ASH and TSTSUP) from tape to determine the extent of the
damage to (SYS).  The retrieval block for [1,4].UFD contained garbage in
locations 131-777 which appeared to be UFD data.  Location 131 is the correct
word address for a UFD entry to start, so this confirms that somehow UFD data
was inadvertantly written into the RIB of the UFD instead of one of the UFD
data pages.  I cleared out the garbage data and fixed the pointers.

We also had read errors for the first BAT page and the third BOOT page.  I
copied the bad data for the BAT page to memory and corrected it.  After
rebooting the system, we ran ASP from tape to make an image copy of the bad
pack.  Then we restored the tape copy to a newly formatted pack.  All was
successful and the system came up with a FULL DSKCLN showing only 3 bad files.
Two of these were open when the system crashed and the third was caused by
a RIB error shortly before the first crash.

The message "BPB6 SECOND BAT BLOCK CONSISTANCY ERROR" was caused by the second
BAT page havinfg correct data which did not match the bad data from the first
BAT page.  This morning, I zeroed out the BAT page data to reflect running on
a newly formatted pack and this message should not occur anymore.

It is not completely certain that this was the only damage to the original
pack.  However, it was the only apparent damage to the file structure.  the
actions taken by the operations staff in taking the system down and calling
both hardware and software support was a significant aid in making the
successful recovery possible.

/Carl Baltrunas

Received: from F29.Tymnet by D35.Tymnet; Tue, 7 Jun 88 14:04:47 PDT
Received: from D35.Tymnet by F29.Tymnet; Tue, 7 Jun 88 14:04:20 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Tue, 7 Jun 88 14:00:24 PDT
Return-path: <NTD.W/EUSKE@EMSTXS.Ontyme.Tymnet> 
From: NTD.W/EUSKE@EMSTXS.Ontyme.Tymnet 
Date: 07 JUN 88 17:42:38 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: I01843@Ontyme.Tymnet 
Subject: OUT-OF-THE-OFFICE 

ATTENTION ALL ONTYME SUPS.....


PLEASE SEND MESSAGE # I01819 TO EVERYONE IN YOUR ONTYME GROUP.

THANKS,
LINDA JOHNSON
NTD.SUP
From: Osman Guven <OSMAN@D35> 
Date: Wed, 8 Jun 88 12:00:32 PDT 
To: Monitor Group <OSMAN>, Joe Smith <JMS@D35>, Carl Baltrunas <Carl@D35> 
Cc: Craig Fletcher <Fletcherc@D35> 
Subject: FYI .. 

o  $55/sq-ft is what MSO is charging at Fremont D.C.

o  There is room to put some development systems(1 or 2 SUN's)
    free of charge for the rest of the year.

-Osman-
Received: from F29.Tymnet by D35.Tymnet; Wed, 8 Jun 88 14:05:10 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 8 Jun 88 14:04:19 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 8 Jun 88 14:00:26 PDT
Return-path: <NTD.W/EUSKE@EMSTXS.Ontyme.Tymnet> 
From: NTD.W/EUSKE@EMSTXS.Ontyme.Tymnet 
Date: 07 JUN 88 21:55:11 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: I02057@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS INFORMATION"... 

 
 
 
 
 
                         MCDONNELL DOUGLAS
                     INFORMATION SYSTEMS GROUP
                      INTEROFFICE MEMORANDUM
 
 Network Systems Company            Network Technology Development
 
 
DATE>      07 JUN 88  09:12

TO>        NTD PERSONNEL

COPIES>    MDC PERSONNEL

FROM>      BILL EUSKE


SUBJECT>   ABSENCE FROM OFFICE 6/7 THROUGH 6/15/88


-----------------------------------------------------------------------


I will be out-of-the-office June 7 through June 15, 1988.
Dave Alcorn will be acting in my stead during this time.
Please contact Dave regarding NTD issues that may need
immediate attention.  You may contact Linda Johnson for
NTD administrative needs.

BE/lj
Received: from F30.Tymnet by D35.Tymnet; Thu, 9 Jun 88 12:00:14 PDT
Return-path: <DANIELSR@F30.Tymnet> 
From: DANIELSR@F30.Tymnet 
Date: Thu, 9 Jun 88 11:58:58 PDT 
To: OSMAN, JMS, CARL, DANIELSR, OPER 
Subject: "OSMAN; WHEN USING LETTER" ON F30, "... 


OSMAN;

WHEN USING "R LETTER" ON F30, WE GET ERROR MESSAGE "BLOCK DOESNT EXIT".

DO YOU HAVE AN EXPLANATION?

THANKS RICK
From: Osman Guven <OSMAN@D35> 
Date: Fri, 10 Jun 88 8:40:19 PDT 
To: Rick Daniels <IPC.R/Daniels@Ontyme> 
Subject: FYI .. 

Hi Rick ..

Here is the summary of what we talked on the phone this mornning.

o  System 33 & 74 have new monitor P035/D06 and send mail to CRALST.

o  All Fremont system now have the ASP 7(110) in (SYS). 

o  Joe knows about the EBUS code problem with F29 and F58

o  "R LETTER" problem on F30 appears to be working now.

o I will be on vacation till June 20.

-Osman-
From: Osman Guven <OSMAN@D35> 
Date: Fri, 10 Jun 88 9:04:27 PDT 
To: Monitor Group <OSMAN>, Joe Smith <JMS@D35>, Carl Baltrunas <Carl@D35> 
Cc: Craig Fletcher <Fletcherc@D35> 
Subject: FYI .. 

(begin forwarded message)
----------------------------------------------------------------
From: Osman Guven <OSMAN@D35> 
Date: Fri, 10 Jun 88 8:40:19 PDT 
To: Rick Daniels <IPC.R/Daniels@Ontyme> 
Subject: FYI .. 

Hi Rick ..

Here is the summary of what we talked on the phone this mornning.

o  System 33 & 74 have new monitor P035/D06 and send mail to CRALST.

o  All Fremont system now have the ASP 7(110) in (SYS). 

o  Joe knows about the EBUS code problem with F29 and F58

o  "R LETTER" problem on F30 appears to be working now.

o I will be on vacation till June 20.

-Osman-
----------------------------------------------------------------
(end forwarded message)

I sent the above mail to Rick, so that things will get done.
I did ASP and NEWMON installation myself because I felt responsible
to complete the work since I will not be here next week to follow up.
-Osman-
Received: from F74.Tymnet by D35.Tymnet; Mon, 13 Jun 88 6:58:22 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Mon, 13 Jun 88 6:56:04 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "SOFTWARE SUPPORT: MAIL FROM 11"... 


SOFTWARE SUPPORT:

MAIL FROM 11 JUN 21:31:34 TO 13 JUN 4:14:28 DID NOT GO TO MY

TUMS.MSG FILE NOR DOES IT APPEAR WHEN I DO A "LIST 1:%".

PLEASE PROVIDE INFORMATION AS TO WHY?

THANKS RICK
Received: from F74.Tymnet by D35.Tymnet; Mon, 13 Jun 88 9:44:20 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Mon, 13 Jun 88 9:41:39 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "Since F74 has been up on NEWMON, "... 


Since F74 has been up on NEWMON, there has been THREE crashes (3, 4, and 5).

Are these related to the NEWMON or the updated EBUS code that wasloaded
at the same time?

ThansIf system goes down one more time, we will back off of NEWMON and
bring up on SYSTEM monitor.

Thanks Rick Daniels
Received: from F74.Tymnet by D35.Tymnet; Mon, 13 Jun 88 13:34:35 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Mon, 13 Jun 88 13:32:14 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "System 74 has been brought with"... 


System 74 has been brought with SYSTEM.SAV.  Plans are that if
anymore "CIRCUITS NOT RESPONDING" message print out on SUPCHK, the
EBUS will be loaded with OLD code and system brought up on NEWMON.SAV.
Tyring to determine what is causing crashes.

Thanks Rick Daniels
Received: from F74.Tymnet by D35.Tymnet; Mon, 13 Jun 88 15:46:32 PDT
Return-path: <OPER@F74.Tymnet> 
From: OPER@F74.Tymnet 
Date: Mon, 13 Jun 88 15:46:03 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "F74/DDD APR: 1354 STOPCODE CRASH"... 

F74/DDD APR:  1354 -- STOPCODE - CRASH
DATE:  06/13/88   DOWN:  13:12   ANS:  13:27
STOPCODE TYPE:  HALT   NAME:  CRASH   M NUMBER:  M02525
LOADED P035/D06 FROM DISK.  SYSTEM CHOSE FAST DSKCLN AT 13:17.
MEMORY ONLINE: 4096K   BUSS MODE:  4   INTERLEAVE
CRASH FILE:  CRA006

   FRAME 23 TTY162 DANIELSR[1,142734] AT F74
   RUNNING (SYS)LOGON[1,4]
   CONTENTS OF 30: 000001,,220000 = SIXBIT : !2: KEY: 0
   THIS TIME SYSTEM IS UP ON P035/D-4 MONITOR.

FREMONT OPERATIONS...AMB
Received: from F74.Tymnet by D35.Tymnet; Tue, 14 Jun 88 8:35:06 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 14 Jun 88 8:33:59 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "All EBUS's IN THE FREMONT DATA"... 


All EBUS's IN THE FREMONT DATA CENTER 
Received: from F74.Tymnet by D35.Tymnet; Tue, 14 Jun 88 8:37:17 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 14 Jun 88 8:35:15 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "All EBUS's in the Fremont Data"... 


All EBUS's in the Fremont Data Center have been reloaded with (EBUS;25,33,54)
NWXXXX.BND with the exception of Hosts 29 and 58.  Cannot load these
system from either 25, 33, or 54.

Thansk Rick
Received: from F29.Tymnet by D35.Tymnet; Tue, 14 Jun 88 11:41:14 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 14 Jun 88 11:39:38 PDT 
To: <DANIELSR@F74.Tymnet> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC 
Subject: Re: "System 74 has been brought with"... 
In-reply-to: your message of Mon, 13 Jun 88 13:32:14 PDT

I agree.  Hold off on NEWMON until we have had a chance to analyze every
one of the crashes from /D06.  
				/Joe
Received: from F29.Tymnet by D35.Tymnet; Tue, 14 Jun 88 12:19:01 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 14 Jun 88 12:17:48 PDT 
To: <DANIELSR@F74.Tymnet> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, wrs 
Subject: Re: "SOFTWARE SUPPORT: MAIL FROM 11"... 
In-reply-to: your message of Mon, 13 Jun 88 6:56:04 PDT

The following messages were found in "JZ(!.FEM" dated 28-Apr and "CA(!.FEM"
dated 13-Jun.  If TUMS dies for any reason (such as Illegal Memory Reference
or SAIL String Garbage Collect Error), it often leaves behind a *.FEM temp
file.  The way to recover is to recover is:
        DIR *.FEM
        RENAME *.FEM,MAIL.
        TUMS
        EXIT
If there are more than 2 *.FEM files, repeat the above procedure until they
are all taken care of.

Carl and Soley tell me the problem is in the way SAIL does its garbage
collection, which is part of the SAIL runtime routines.  It doesn't look like
this bug will ever be given the priority needed to fix it.
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
Received: from F74.Tymnet by D35.Tymnet; Thu, 16 Jun 88 9:44:50 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Thu, 16 Jun 88 9:42:23 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "Carl; Per our conversation yesterday"... 

Carl;

Per our conversation yesterday the Inter-Office Memo sent by Craig of
12 February 1988 does not list any PAGER number.

It only lists the Secretary number and the 800 number.

Thanks Rick
Received: from F74.Tymnet by D35.Tymnet; Fri, 17 Jun 88 9:32:45 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Fri, 17 Jun 88 9:29:25 PDT 
To: CARL, JMS, OSMAN, DANIELSR 
Subject: "Need an answer on the following"... 


Need an answer on the following as soon as possible.

When Operators are using TAKEIT to take a system down for ASP dumps,
they are using deposit 1 in 30 in most cases when they get the message
"?can't halt frame with JACCT: job 2, user OPER <CHKPNT> RN".
Usually, they art at a -7 or -10 before entering the 1 in 30.
Received: from F74.Tymnet by D35.Tymnet; Fri, 17 Jun 88 9:34:38 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Fri, 17 Jun 88 9:32:47 PDT 
To: carl, jms, osman, danielsr 
Subject: "What I want to know is: Is it"... 


What I want to know is:

Is it a good practice to deposit 1 in 30?

How long should thy wait for this job to be HUNG?

Why doesn't it HANG JACCT within a reasonable amount of time?

Thanks Rick
Received: from F29.Tymnet by D35.Tymnet; Fri, 17 Jun 88 14:09:06 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 17 Jun 88 14:07:02 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 17 Jun 88 14:00:47 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 17 JUN 88 10:40:59 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.J/SMITH@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A10887@Ontyme.Tymnet 
Subject: HDEV, HDAT, SATFAILS, ETC 

                         M E M O R A N D U M





DATE>      17 JUN 88  10:40

TO>        Software Support

COPIES>    

FROM>      Rick Daniels


SUBJECT>   HDEV, HDAT, SATFAILS, ETC


-----------------------------------------------------------------------


Need clarification about HDEV, HDAT, SATFAILS, RIBS,
and THUNGS reported when running PSP on 3330's and 3350's.

ON 3330'S
---------

Are all of a serious nature that a FULL  disk clean should be
run before and after an ASP Dump?

At other times, if it can wait, should the system be scheduled
down for a FULL disk clean?

ON 3350'S
---------

Do we need to be concerned about HDEV errors and THUNGS?

If SATFAILS, HDAT, and RIBS show on the PSP, should a FULL
disk clean be run before and after ASP dump?

There seems to be a misunderstanding here on the above and I
want to get the correct information out about it.
                                                                Page  2


Thanks

Rick
From: Carl A Baltrunas <Carl> 
Date: Mon, 20 Jun 88 19:12:05 PDT 
To: Monitor Group <Carl>, Osman Guven <Osman>, Joe Smith <JMS>, Craig Fletcher
	<fletcherc> 
Subject: Inventory 

Please check the serial numbers and ISG-TAG numbers on any equipment you
have at home and either update the file (CARL:33)TERMS.TXT or send the the
appropriate information to me (mail or paper).

/Carl
Received: from F74.Tymnet by D35.Tymnet; Tue, 21 Jun 88 11:31:28 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 21 Jun 88 11:27:47 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "All Fremont Systems 26, 29, 30, "... 


All Fremont Systems (22, 26, 29, 30, 33, 38, 58, 74) have been reloaded
with (EBUS;25,33,54).

22(6-6-88), 26(6-11-88, 29(6-21-88), 30(6-6-88), 33(6-4-88), 38(6-8-88),
58(6-20-88), AND 74(6-11-88).

Thanks Rick Daniels
Received: from F29.Tymnet by D35.Tymnet; Tue, 21 Jun 88 14:06:42 PDT
Received: from D35.Tymnet by F29.Tymnet; Tue, 21 Jun 88 14:06:30 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Tue, 21 Jun 88 14:04:53 PDT
Return-path: <IPC.S/PAL@EMSFSC.Ontyme.Tymnet> 
From: IPC.S/PAL@EMSFSC.Ontyme.Tymnet 
Date: 21 JUN 88 11:34:34 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: A11994@Ontyme.Tymnet 
Subject: Change Control Meeting 

D....MEMO-SENT             ATE> 21 JUN 88                     ATE> 21 JUN 88
 
FROM> Sue Pal
 
 
SUBJ> Change Control Meeting
 
                              M E M O R A N D U M
 
-----------------------------------------------------------------------------
 
This is to remind you that Change Control Meeting will be held tomorrow
(6/22) at 10:30.
See you there ! (Free Donuts !)
 
 Upcoming Events.........
 
Guest Speaker.....Danis Marchal
Subject...........Turbo Engine
Date..............07/06/88 (after Change Control)
 
Please let me know if you would like to know about any other Products.
 
Thank you,
Sue Pal
Fremont Data Center
Received: from F74.Tymnet by D35.Tymnet; Tue, 21 Jun 88 14:40:35 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 21 Jun 88 14:39:41 PDT 
To: OSMAN 
Subject: "Osman; From what I can find out, "... 


Osman;

From what I can find out, F29 was reloaded today with the correct
EBUS code.

thanks rick
Received: from X32.Tymnet by D35.Tymnet; Wed, 22 Jun 88 19:01:11 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Wed, 22 Jun 88 18:59:11 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, Bill Richardson <FSC.B/Richardson@Ontyme> 
Subject: Read errors on X32's 3652 disks. 

The following files have read errors:
(M33         :  32) PATLOG.SAV    18 02-Feb-79 22:53 -- bad~ checksum
(M33         :  32) OPERN.DOC      5 18-Dec-81 20:21 -- bad~ checksum
(M33         :  32) MONWSH.NFO     3 10-Mar-86 17:17 -- bad~ checksum
(M33         :  32) SCNSER.YY0   129 19-Feb-88 18:35 -- bad~ checksum ******
(M33         :  32) NOTICE.38      2 17-Jun-88 01:01 -- bad~ checksum ******
(M33         :  32) PAKCOP.ZZZ     2 06-Jan-87 20:47 -- bad~ checksum ******
(M33         :  32) NOTICE.34      0 20-Feb-77 02:16 -- bad~ license     53
(M33         :  32) CHKPNT.300     5 22-Sep-80 16:01 -- bad~ checksum
(M33         :  32) NOTICE.74      2 14-Jun-88 18:45 -- bad~ checksum CUPCIT
(M33         :  32) CRSH34.73      7 19-Dec-74 16:28 -- bad~ checksum

I started RONLY and DSKCLN jobs to check for more.  /Joe
Received: from F29.Tymnet by D35.Tymnet; Wed, 22 Jun 88 19:04:25 PDT
Received: from B39.Tymnet by F29.Tymnet; Wed, 22 Jun 88 17:52:10 PDT
Received: from tymix.Tymnet by B39.Tymnet; Wed, 22 Jun 88 17:51:28 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA10471; Wed, 22 Jun 88 17:50:27 PDT
Received: by antares.Tymnet.com (3.2/SMI-3.2) id AA18043; Wed, 22 Jun 88
	17:46:37 PDT
Return-path: <JMS@F29.TYMNET> 
From: antares!jms (joe smith) 
Date: Wed, 22 Jun 88 17:46:37 PDT 
To: carl, osman, fletcherc 
Message-id: <8806230046.AA18043@antares.Tymnet.com> 
Subject: PDP-10 architecture 
Resent-from: Joe Smith <JMS@F29.TYMNET> 
Resent-date: Wed, 22 Jun 88 19:03:37 PDT 
Resent-to: Monitor Group <JMS>, Carl, Osman 
Resent-cc: FletcherC 

From tymix!oliveb!ames!hc!lanl!beta!hwe Wed Jun 22 17:45:36 PDT 1988
Article 518 of comp.arch:
Path: antares!tymix!oliveb!ames!hc!lanl!beta!hwe
>From: hwe@beta.lanl.gov (Skip Egdorf)
Newsgroups: comp.arch
Subject: Re: Compiler complexity (was: VAX Always Uses Fewer Instructions)
Summary: More Ancient History, PDP-10
Keywords: RISC CISC
Message-ID: <20345@beta.lanl.gov>
Date: 18 Jun 88 03:02:51 GMT
References: <6921@cit-vax.Caltech.Edu> <28200161@urbsdc> <10595@sol.ARPA> <20338@beta.lanl.gov>
Organization: Los Alamos National Laboratory
Lines: 62

In article <20338@beta.lanl.gov>, jlg@beta.lanl.gov (Jim Giles) writes:
> 
> This discussion brings up a question about the actual use of CISC
> instructions.  It seems that RISC vs. CISC is probably about a draw
> for raw compute speed of special coded sequences.  The question is:
> how many programs actually get full advantage from the large instruction
> sets offered by CISC?
> ...
> In more recent years, has the state of the compiler art improved so that
> good code generators for CISC machines can be built easily?  Or is it
> still a hit or miss operation?  Do modern compilers for CISC make good
> use of the variety in the instruction set?
> 
> J. Giles
> Los Alamos

As I seem to like to dredge up ancient history in this forum, these
thoughts sent me to my dusty closet of 'olde stuff' to fetch volume
1 (I also have V2) of a collection of papers produced by DEC called
"PDP-10 Applications in Science".
I think that I got this sometime in 1970, though most of the papers
seem to be from around 1967-1968.
The third paper in the book is

"Selecting and Evaluating a Medium Scale Computer System"
"A Study Undertaken for Louisiana State University at New Orleans"
by Jon A. Stewart, Director, LSUNO Computer Research Center

The paper is a summary of an in-depth comparison of three major
systems of the time; the SDS Sigma-5, the IBM 360/44, and the
DEC PDP-10. The paper is very fine at bringing back the memories
of the good old days, but of most interest to this discussion is
an appendix containing compiler-generated code for a simple
fortran benchmark.

The upshot was that the PDP-10 generated much shorter code,
that also ran faster. (I can publish a sample if there is
enough interest.)

While this paper drew no architectural conclusions (such as
are discussed in this forum) the main reason (if I
remember my DEC Propaganda of the day) was the very orthogonal
instruction set as opposed to a smaller number of instructions.

This clean and complete instruction set allowed the compiler
to use a large percentage of the instruction set. While I believe
that the clean instruction set came first, and the compiler
that used the large percentage of that instruction set came later,
rather than the current risc idea of profiling code to determine what
will be in the instruction set, it was a haunting hint of where
we would be today.

I recall seeing published statements about how many of the KA-10's
366 instructions were actually used by the FORTRAN compiler.
Does anyone still have these numbers??

I also have some DEC Propaganda from about 1973 bragging about
the optimization done by DEC FORTRAN. This includes examples
of generated code. Again, if anyone is interested...

					Skip Egdorf
					hwe@lanl.gov


Received: from X32.Tymnet by D35.Tymnet; Wed, 22 Jun 88 19:27:26 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Wed, 22 Jun 88 19:22:31 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, Bill Richardson <FSC.B/Richardson@Ontyme> 
Subject: re: Previous message about X32 read errors. 

The list of files with read errors came from a log file on F33 created 22-Jun
at 5:00am.  The files themselves look good; they have the right checksums now.
I believe the files got marked bad when the disk hardware acted flakey (the
system couldn't read RIB or data pages) but that's been fixed.  Any more
news as to what when wrong with the hardware?  /Joe
From: Carl A Baltrunas <Carl> 
Date: Thu, 23 Jun 88 0:55:13 PDT 
To: Monitor Group <Carl>, Osman Guven <Osman>, Joe Smith <JMS>, Craig Fletcher
	<fletcherc> 
Subject: Strnage behavior? 

I looked at the ?ill mem ref for SYSDAT and it's kinda strange.  The code is
somehow mangling the byte pointer which initially points into page 777 and
in adjusting it to point to the right offset (using a subtract) the byte ptr
becomes ['10677,,-1].  The illmem ref appears when LDB 11,PTR is executed
and PTR contains the value in brackets above.

Don't ask me... but I think an illmem ref for 310000 or 50533 is out-to-lunch.
Someone has gotten the wrong info from somewhare.

Joe: any ideas?  Osman:  Isn't this strange behavior for hte hardware?
Any: Am I missing something rather obvious?

/Carl
Received: from F74.Tymnet by D35.Tymnet; Thu, 23 Jun 88 7:12:09 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Fri, 17 Jun 88 9:29:25 PDT 
To: CARL, JMS, OSMAN, DANIELSR 
Subject: "Need an answer on the following"... 
Resent-from: DANIELSR@F74.Tymnet 
Resent-date: Thu, 23 Jun 88 7:11:53 PDT 
Resent-to: JMS, CARL, OSMAN, DANIELSR 


Need an answer on the following as soon as possible.

When Operators are using TAKEIT to take a system down for ASP dumps,
they are using deposit 1 in 30 in most cases when they get the message
"?can't halt frame with JACCT: job 2, user OPER <CHKPNT> RN".
Usually, they art at a -7 or -10 before entering the 1 in 30.
Received: from X32.Tymnet by D35.Tymnet; Thu, 23 Jun 88 12:42:37 PDT
Return-path: <DANIELSR@X32.Tymnet> 
From: DANIELSR@X32.Tymnet 
Date: Thu, 23 Jun 88 12:41:34 PDT 
To: CARL, OSMAN, JMS, DANIELSR, FSC.R/DONAHUE@ONTYME 
Subject: "Has anyone looked at the CTYLOG"... 


Has anyone looked at the CTYLOG on this system?  May have a problem.

Thanks Rick
Received: from F74.Tymnet by D35.Tymnet; Thu, 23 Jun 88 12:53:26 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Fri, 17 Jun 88 9:32:47 PDT 
To: carl, jms, osman, danielsr 
Subject: "What I want to know is: Is it"... 
Resent-from: DANIELSR@F74.Tymnet 
Resent-date: Thu, 23 Jun 88 7:12:16 PDT 
Resent-to: JMS, OSMAN, CARL, DANIELSR 


What I want to know is:

Is it a good practice to deposit 1 in 30?

How long should thy wait for this job to be HUNG?

Why doesn't it HANG JACCT within a reasonable amount of time?

Thanks Rick
Received: from F29.Tymnet by D35.Tymnet; Thu, 23 Jun 88 14:09:40 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 23 Jun 88 14:07:54 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 23 Jun 88 14:00:37 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 23 JUN 88 07:05:31 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A12813@Ontyme.Tymnet 
Subject: "Do you have anything for me on"... 

Do you have anything for me on this yet?

Thanks Rick

-----------------------------------------------------------------------




                         M E M O R A N D U M





DATE>      17 JUN 88  10:40

TO>        Software Support

COPIES>    

FROM>      Rick Daniels


SUBJECT>   HDEV, HDAT, SATFAILS, ETC


-----------------------------------------------------------------------


Need clarification about HDEV, HDAT, SATFAILS, RIBS,
and THUNGS reported when running PSP on 3330's and 3350's.

ON 3330'S
---------

Are all of a serious nature that a FULL  disk clean should be
run before and after an ASP Dump?

At other times, if it can wait, should the system be scheduled
down for a FULL disk clean?

ON 3350'S
---------

Do we need to be concerned about HDEV errors and THUNGS?

If SATFAILS, HDAT, and RIBS show on the PSP, should a FULL
disk clean be run before and after ASP dump?

There seems to be a misunderstanding here on the above and I
want to get the correct information out about it.
                                                                Page  2


Thanks

Rick
Received: from F29.Tymnet by D35.Tymnet; Thu, 23 Jun 88 14:09:49 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 23 Jun 88 14:09:18 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Thu, 23 Jun 88 14:07:05 PDT
Return-path: <IPC.E/BARENS@EMSFSC.Ontyme.Tymnet> 
From: IPC.E/BARENS@EMSFSC.Ontyme.Tymnet 
Date: 22 JUN 88 14:14:45 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: A12589@Ontyme.Tymnet 
Subject: D65 
SYSTEM65SHUTDOWN: 

                           M E M O R A N D U M    
     
     
     ATTN:  DALLAS OPERATIONS 
            FREMONT OPERATIONS
     
   COPIES:  ED BARENS - DCO                       DICK KOVACH - MIS   
            RICK DANIELS - FCO                    SHARON MARCOTTE - FCO    
            CHERLY ELDRED - FCO                   CINDY SMITH - PMTS  
            JENNIFER ENGLISH - RPM                SAL SPINALE - PMTS  
            CATHY FLYNN - PMTS                    GARY WALKER - QSATS 
     
     FROM:  LANDA MORRIS - RPM
     
     SUBJ:  SYSTEM 65 SHUTDOWN
     
-------------------------------------------------------------------------  
     
     SYSTEM 65, LOCATED IN DALLAS, WILL BE SHUTDOWN ON FRIDAY, JULY 1,
1988.  BELOW IS A LIST OF EVENTS THAT WILL OCCUR: 
     
     
     *  RPM HAS VALIDATED THE FOLLOWING PMTS USERS ON SYSTEM 33 IN    
        FREMONT.  ALL OTHER USERS VALID ON D-65 WILL GO AWAY WITH THE 
        SYSTEM SHUTDOWN. 
     
           1.  CROWELLV                 6.  JTORPHY    
           2.  CSTICKET                 7.  NARAYANANS 
           3.  GCHENG                   8.  OPSCODE    
           4.  HOANGO                   9.  QCONTROL   
           5.  HTRINH                  10.  SSLADE
     
     *  PMTS WILL TRANSFER FILES FROM D-65 TO F-33 FOR THE ABOVE USERS
        BETWEEN NOW AND 7-1-88.    
     
     *  ON 7-1 D-65 WILL BE SHUT TO PMTS USERS AT 1800 PDT.  RPM WILL 
        THEN CHANGE THE HOME SYSTEM FOR THE ABOVE USERS FROM D-65 TO  
        F-33.  
     
     *  THE DCO TAPE LIBRARIAN WILL TRANSFER ALL 12 CUSTOMER TAPES    
        LISTED IN FILE (SRAMOV:65)65.TAP TO HOST 33 IN THE TAPELIB    
        DATABASE.   
     
     *  DALLAS OPERATIONS WILL ARRANGE DELIVERY OF THE CUSTOMER TAPES 
        TO FREMONT ON A DIRECT FLIGHT, COUNTER TO COUNTER SERVICE.    
     
     *  DCO WILL PERFORM A FINAL ALLFILES ON D-65.
     
     *  ALL USERS VALID ON D-65 WILL BE REMOVED BY RPM.
     
     *  D-65 WILL BE PERMANENTLY SHUTDOWN.   
     
     
     
     IF YOU HAVE ANY QUESTIONS I CAN BE REACHED AT (415)498-2557 OR VIA    
ONTYME AT IPC.L/MORRIS   
     
     
THANK YOU,
LANDA/RPM
Received: from F29.Tymnet by D35.Tymnet; Thu, 23 Jun 88 16:44:27 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 23 Jun 88 16:43:10 PDT 
To: OSMAN 
Subject: Names of Ada compilers 

(begin forwarded message)

Received: from B39.Tymnet by F29.Tymnet; Wed, 22 Jun 88 20:33:10 PDT
Received: from tymix.Tymnet by B39.Tymnet; Wed, 22 Jun 88 20:34:47 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11679; Wed, 22 Jun 88 20:07:19 PDT
Received: by antares.Tymnet.com (3.2/SMI-3.2) id AA18226; Wed, 22 Jun 88
	19:49:01 PDT
Return-path: <antares!jms@tymix.Tymnet> 
From: antares!jms (joe smith) 
Date: Wed, 22 Jun 88 19:49:01 PDT 
To: f29!jms 
Message-id: <8806230249.AA18226@antares.Tymnet.com> 
Subject: Names of Ada compilers 

From tymix!oliveb!pyramid!ames!pasteur!ucbvax!TOR.NTA.NO!haug%vax.runit.unit.uninett Wed Jun 22 19:48:08 PDT 1988
Article 112 of comp.lang.ada:
Path: antares!tymix!oliveb!pyramid!ames!pasteur!ucbvax!TOR.NTA.NO!haug%vax.runit.unit.uninett
>From: haug%vax.runit.unit.uninett@TOR.NTA.NO (Steinar Haug)
Newsgroups: comp.lang.ada
Subject: Conditional compilation
Message-ID: <909*haug@vax.runit.unit.uninett>
Date: 13 Jun 88 00:24:00 GMT
Sender: daemon@ucbvax.BERKELEY.EDU
Organization: The Internet
Lines: 19

Hi,

I'm developing programs for both DEC VAX (VAX/VMS Ada compiler) and
SUN-3 (Alsys Ada compiler). I need to use some form of conditional
compilation. The Alsys compiler has the BEGIN_COMPILE/END_COMPILE
pragmas to control the compilation but I have not found any similar
mechanism for the VAX/VMS compiler. Can anybody out in Adaland tell
me how to do conditional compilation with the VAX/VMS compiler?

The LRM contains a reference [10.6] to conditional compilation but
it seems rather cryptic and it is not obvious to me how I could use
it in practice.

Steinar Haug             ! ARPA:        haug%vax.runit.unit.uninett@tor.nta.no 
Database Research Group  !   or:        steinar@tor.nta.no                     
Computing Research Center! EAN(X.400):  haug@vax.runit.unit.uninett            
 University of Trondheim ! BITNET/EARN: haug@norunit                           
7034 Trondheim, NORWAY   ! VMS Mail:    psi%02422530001003::12423              
                         !       or:    psi%0242211000114::z_haug_s            



(end forwarded message)

From this message it is obvious that DEC makes an Ada compiler for VMS, and
Alsys makes an Ada compiler for Unix, Sun in particular.
From: Osman Guven <OSMAN@D35> 
Date: Thu, 23 Jun 88 18:42:58 PDT 
To: <DANIELSR@X32.Tymnet> 
Cc: CARL, JMS, DANIELSR, FSC.R/DONAHUE@ONTYME 
Subject: Re: "Has anyone looked at the CTYLOG"... 
In-reply-to: your message of Thu, 23 Jun 88 12:41:34 PDT

Yes.. Looks like BPA1 is formatted but not initilized.
-Osman-
From: Osman Guven <OSMAN@D35> 
Date: Thu, 23 Jun 88 18:44:37 PDT 
To: <DANIELSR@F74.Tymnet> 
Cc: carl, jms, danielsr 
Subject: Re: "What I want to know is: Is it"... 
In-reply-to: your message of Fri, 17 Jun 88 9:32:47 PDT

Untill we come up with a fix in TAKEIT it is OK to deposit 1 in 30.
Should waite 5 mins before doing deposit 1 in 30.
-Osman-
From: Osman Guven <OSMAN@D35> 
Date: Thu, 23 Jun 88 19:28:43 PDT 
To: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
Cc: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Subject: Re: "Do you have anything for me on"... 
In-reply-to: A12813@Ontyme.Tymnet of 23 JUN 88 07:05:31

Rick ..
I will bring the issue to Carl and Joe and come with an answer.
-Osman-
Received: from F29.Tymnet by D35.Tymnet; Fri, 24 Jun 88 14:08:41 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 24 Jun 88 14:06:55 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 24 Jun 88 14:00:39 PDT
Received: from F29.Tymnet by EMSTXS.Ontyme.Tymnet; Thu, 23 Jun 88 19:43:17 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 23 Jun 88 19:31:06 PDT
Return-path: <OSMAN@D35> 
From: Osman Guven <OSMAN@D35> 
Date: Thu, 23 Jun 88 19:28:43 PDT 
To: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
Cc: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Subject: Re: "Do you have anything for me on"... 
In-reply-to: A12813@Ontyme.Tymnet of 23 JUN 88 07:05:31

Rick ..
I will bring the issue to Carl and Joe and come with an answer.
-Osman-
Received: from F29.Tymnet by D35.Tymnet; Fri, 24 Jun 88 14:08:48 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 24 Jun 88 14:07:03 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 24 Jun 88 14:00:52 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 24 JUN 88 07:45:23 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: IPC.J/ENGLISH@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.J/SMITH@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A13372@Ontyme.Tymnet 
Subject: "SUBJ: M E M O R A N D U M DATE>"... 

SUBJ: 





                         M E M O R A N D U M





DATE>      24 JUN 88  07:43

TO>        Jim English

COPIES>    Software Support

FROM>      Rick Daniels


SUBJECT>   Checking NODE CODE Loads


-----------------------------------------------------------------------


On the TYMCOM-10's there is a utility  that identifies date and time,
slot, user and directory that a specified  file(s) was accessed on the
TYMCOM-10 currently logged into.

For example, I want to know when anyone loaded node or slot code for 
node 2430.  The output of this request and the full session is listed below.
The program is FIND.SAV.  To execute it, you must log into the system
you suspect the code was loaded from.  The code and documentation is located
in the SPL directory.

The following is the node 2430 session example:

R (SPL)FIND

Target 1:  2430.      For all occurrances of "2430." in my specified file.
                      Be sure to put a 'period' after the number.

Target 2: <cr>        At this point, more search strings could be entered
                      but this is all I'm interested in.  Also, rather than
                      2430 as a string, I entered 2430. to capture all
                      occurrances of ##2430.* and also avoid capturing
                      other loads that happened to be done where the time
                                                                Page  2

                      stamp had the string 2430 in it.

search in: .R         This is where all commands are entered.  ".R" to
                      file all lines containing "2430." in them.

Recording file: RICK.TMP      This is my file name.

search in: (SYS)LOADII.ACC     Search for "2430." in file (SYS)LOADII.ACC.

(SYS)LOADII.ACC: 1 (14)       Target 1 found 14 times.

search in: .Q         At this point, more command could be entered like ".T"
                      to enter new targets, etc.  ".Q" entered to close
                      file RICK.TMP and end the session.

The following is the file RICK.TMP and the format is the following:

File name that was searched:

DATE ! START TIME ! STOP-TIME ! ? ! SLOT ! LOADII-CMD ! SUCCESS !
USERNAME ! FILE-NAME ! NEIGHBOR ! LINE-# ! NETWORD ! ? ! ? !TARGET #) !
"STRING" ! # OF MATCHES

***********************************************************************

(SYS)LOADII.ACC

(PAGE 1)...
110485 155656 16 1 1        LD OK OPER  (TYMNET)ND2430.`BND 2355 1

************************************************************************

To obtain the above file, enter TYP RICK.TMP on terminal.

This could be useful for resolving erroneous code load problems, etc,
and can be tailored by inputing the appropriate search string.

This only lets you know if the code was loaded from the right directory
with the correct filename.  It does not tell you what version was loaded.

The output on the CTTY when the code is loaded should report the current
version loaded.

Thanks Rick Daniels
Received: from D65.Tymnet by D35.Tymnet; Fri, 24 Jun 88 14:53:05 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 24 Jun 88 16:51:06 CDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: GAN 3 files on D65. 

The following came from (JMS:65)SZRPT.LST:

24-JUN-1988	14:20
 %   pages files quota username
 19   1955    9  25000 OSNF
 34   1476  143  10000 MPL
 46   1198  112   5000 SPL
 56   1012   44  26000 MAIL
 63    741   51  25000 CARL
 70    646   78   5000 XEXEC
 76    619   69  40000 JMS
 81    462   37   2500 OSMAN
 85    431   42  25000 WRS
 89    342   57   1250 SAILIB
 92    338   33  10000 SUBMIT
 95    306   21   1250 PEAK
 98    300    9  25000 M33
 99    147   12   1250 SPPOPER
100      6    3   1250 FLETCHERC

As you can see, there are not very many files that need to be moved when
D65 goes away July 1st.    /Joe
Received: from X32.Tymnet by D35.Tymnet; Fri, 24 Jun 88 21:36:31 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 24 Jun 88 21:30:16 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: Found cause of F74 crashes with /D06. 

The dispatch table for the AUXCAL uuo was getting trashed.  This table was
followed by a table with counts of TTCALL uuos, called TTCCNT which was
set up for 20 entries (0 thru 17 octal).  The problem is that UUOCON sometimes
calls the TTYUUO routine with a negative number in accumulator W, such as
for "leave defered echo".  The instruction AOS TTCCNT(W) when W was negative
caused the damage.  This damage would not be noticed until the next poor
sucker tried to read the terminal CLASS setting.

Fix: Add room for -5 thru -1 to TTCCNT.    /Joe
Received: from F33.Tymnet by D35.Tymnet; Sat, 25 Jun 88 10:39:38 PDT
Return-path: <DANIELSR@F33.Tymnet> 
From: DANIELSR@F33.Tymnet 
Date: Sat, 25 Jun 88 10:34:49 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "Joe; Would like to comply with"... 


Joe;

Would like to comply with your request to bring all Fremont Systems
up on P035/D-7, but it is not installed on Systems 22, 38, 58.

The last time we brought F33 and F74 up on this monitor we had to
back off.  Have not heard if these were fixed.

My understanding is we do not bring anything up on NEWMON or any other
software unless I receive mail from Ingrid LOPES in Ed Roops department.

Thanks Rick
Received: from F29.Tymnet by D35.Tymnet; Mon, 27 Jun 88 14:04:11 PDT
Received: from D35.Tymnet by F29.Tymnet; Mon, 27 Jun 88 14:03:43 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Mon, 27 Jun 88 14:00:26 PDT
Return-path: <NTD.L/JOHNSON@EMSTXS.Ontyme.Tymnet> 
From: NTD.L/JOHNSON@EMSTXS.Ontyme.Tymnet 
Date: 27 JUN 88 16:54:23 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: I10087@Ontyme.Tymnet 
Subject: ABSENCE FROM OFFICE 

                                   M E M O R A N D U M

Network Technology Development                        Network Systems Company





TO:  NSC PERSONNEL
FROM: BILL EUSKE
SUBJ: ABSENCE FROM OFFICE



I WILL BE AWAY FROM THE OFFICE 6/27/88 THROUGH 6/29/88 AND LANA VAYSBURD
WILL BE ACTING IN MY STEAD.  YOU MAY CONTACT LANA AT EXT. 6538 REGARDING
ISSUES INVOLVING NTD.


BE/lj
From: Carl A Baltrunas <Carl> 
Date: Wed, 29 Jun 88 4:12:07 PDT 
To: Monitor Group <Carl>, Osman Guven <Osman>, Joe Smith <JMS>, Craig Fletcher
	<fletcherc>, Rick Daniels <OPERA.SUP@EMSTXS.Ontyme> 
Subject: SYSDAT & CTYLOG 

The bug in the SYSDATa program has been fixed!!! So, the CTYLOG output is
again being collected on all systems [except 32 which has been down].

Joe: By re-arranging the order of a couple of operations, everything now
     works as intended.  The problem was caused whenever data was added~
     to the log and the previous data ended exactly on a page boundary.

The new version is %2(17) and is installed on (SRA) on all systems except
the test system :32.  WHen 32 comes up, I will copy it over.

/Carl
Received: from X32.Tymnet by D35.Tymnet; Wed, 29 Jun 88 17:15:08 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Wed, 29 Jun 88 17:12:26 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: FASTC test went OK. 

Paul poked FASTC to be 7FFF 0000 on X32's base.  We watched it increment to
8000 0000 with no problem.  (The date as reported by XRAY is incorrect, but
that was to be expected.)   /Joe
Received: from F29.Tymnet by D35.Tymnet; Thu, 30 Jun 88 14:07:49 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 30 Jun 88 14:06:23 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 30 Jun 88 14:00:38 PDT
Return-path: <ACS.L/EDWARDS@EMSTXS.Ontyme.Tymnet> 
From: ACS.L/EDWARDS@EMSTXS.Ontyme.Tymnet 
Date: 29 JUN 88 23:14:59 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A15321@Ontyme.Tymnet 
Subject: *** File for ordering ACS PC Software and Publications 

                      APPLIED COMMUNICATIONS SYSTEMS

 
                           M E M O R A N D U M
 
 
                                                      []  ISG     
 
DATE>      29 JUN 88  14:54

TO>        All ACS
           ACS sales 
           *** Supervisors

COPIES>    

FROM>      Lynne Edwards


SUBJECT>   *** File for ordering ACS PC Software and Publications


-----------------------------------------------------------------------


There is now available the file *** SHIP.ROUTINE for ordering all ACS
PC Software - Personal OnTyme ($64/$95) and Tym/COMM ($114/$170), and 
Publications -
OnTyme Quick Reference Guides ($2.35/$3.50)
OnTyme Reference Manual ($23.60/$35.00)
OnTyme Primer ($18.20/$27.00)
OnTyme Student Guide ($10.00/$15.00)
OnTyme Instructor's Guide ($23.60/$35.00)
OnTyme Class Slide show ($50.60/$75.00)
and marketing literature for OnTyme Plus (Success stories, brochures and data 
sheets.)

By typing :EXEC *** SHIP.ROUTINE you will get a menu that will ask if you
would like to order software or publications. Once in the exec you will need
to know the quantity, address and shipping method. If it is an order for
a customer, you will also need to know the Service Agreement number, Customer
ID and whether or not they are tax exempt. For internal orders, you will need
a cost code.

Once you have answered all of the questions, you may make changes before
it is automatically sent to ACS.ORDERS for fulfillment.

If you have any questions or comments, please feel free to call or OnTyme
me. (408)922-7667.


* ($X/$X) = internal and commercial prices
Received: from F29.Tymnet by D35.Tymnet; Sat, 2 Jul 88 14:05:35 PDT
Received: from D35.Tymnet by F29.Tymnet; Sat, 2 Jul 88 14:04:47 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Sat, 2 Jul 88 14:00:40 PDT
Return-path: <OPERA.SUP@EMSTXS.Ontyme.Tymnet> 
From: OPERA.SUP@EMSTXS.Ontyme.Tymnet 
Date: 01 JUL 88 14:14:33 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (32 names) 
Message-id: A16333@Ontyme.Tymnet 
Subject: "Please DO NOT refer to HARDWARE"... 

Please DO NOT refer to HARDWARE XXX, SYSTEM X32 as a spare KL as it
is not a SPARE KL

Fremont Data Center, Operations Department, is the customer on this
system.  Field Service Company (which includes TYMCOM-10 Software
Support,) maintains it for the center.

Whenever a host is SWAPPED with this hardware, indicate in the 
mail that System 29, Hardware JJJ was swapped with System 32,
Hardware XXX.


Thanks

Rick Daniels
Received: from F33.Tymnet by D35.Tymnet; Tue, 5 Jul 88 19:35:16 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 5 Jul 88 19:25:59 PDT 
To: Rick Daniels <OPERA.SUP@EMSTXS.Ontyme>, Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: Free space on system 33. 

Just before the conversion, F33 was almost 100% full on 24 units.
Formatting in pages allows for 7 pages per track instead of 25 blocks per
track.  (7 pages = 28 blocks.)  The ratio of 28 to 25 is 1.12 = +12 %.
This makes 24 logical units in pages the equivalent of 26.88 block units.
The added 2.88 units at 103500 pages per makes for about 298000 pages free.

F33 now has 288270 pages free.  It's a lot better than the low of 14697 pages
it got down to on 30-Jun.				/Joe
From: Osman Guven <OSMAN@D35> 
Date: Tue, 5 Jul 88 22:25:08 PDT 
To: <OPERA.SUP@EMSTXS.Ontyme.Tymnet> 
Subject: Re: "Please DO NOT refer to HARDWARE"... 
In-reply-to: A16333@Ontyme.Tymnet of 01 JUL 88 14:14:33

Yes, it is a very good idea as to not to refer to X32 as "SPARE"
system.  SPARE has the connotation of being dispensable which
I don't like our system being refered to.

By the way, Joe tested the 41 day crash on the base successfully.
	
-Osman-
From: Osman Guven <OSMAN@D35> 
Date: Tue, 5 Jul 88 22:48:38 PDT 
To: Ed Barens <IPC.E/Barens@Ontyme>, Jim English <IPC.J/English@Ontyme>, Jim
	Zone <IPC.J/Zone@Ontyme>, John Roddam <IPC.J/Roddam@Ontyme>, Leland
	Yarbrough <IPC.L/Yarbrough@Ontyme>, Jerry Meyer <FSC.G/Meyer@Ontyme> 
Subject: FYI .. 


Ed ..

Per our phone conversation ..

o  Please go ahead and load Dallas's Bases whenever the systems come
   down next time.  Base codes are located on systems 25, 54 and 33.
   User name is (EBUS), file name NWxxxx.BND.

   Fremont D.C. has been running with the "NEW BASE CODE" successfully and
   without any problems.  Again, one of the important fixes in the new base
   code is: Prevention of host/base crashes when system has been up more
   than 41 days.

   If you have any problems with the new base code, please send mail indicating
   on what system it was not successful.


o  I am planning to be in Dallas Aug 2-4 to teach a seminar to "Operations"
   and "MDFSCO" and whoever else is interested in attending.

   The basic thrust of the seminar will be to go over the new and updated
   monitor functionality and some system utilities.

See you all soon.

-Osman-
Received: from F29.Tymnet by D35.Tymnet; Wed, 6 Jul 88 14:05:55 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 6 Jul 88 14:05:40 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Wed, 6 Jul 88 14:03:54 PDT
Return-path: <FSC.M/MILLER@EMSFSC.Ontyme.Tymnet> 
From: FSC.M/MILLER@EMSFSC.Ontyme.Tymnet 
Date: 06 JUL 88 09:02:44 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P75293@Ontyme.Tymnet 
Subject: OFFICE RELOCATION 
OFFICECHANGE: 

                M C D O N N E L L   D O U G L A S
 
            F I E L D   S E R V I C E   C O M P A N Y



DATE>      05 JUL 88  08:36

TO>        ALL FSC

COPIES>    

FROM>      MARC MILLER


SUBJECT>   OFFICE CHANGE


-----------------------------------------------------------------------


TECHNICAL SUPPORT FOR NETWORK SYSTEMS

HAS MOVED IN THE LOS ANGELES AREA.

OLD ADDRESS WAS 5900 S. EASTERN AVE. COMMERCE 90040 (213)721-3322


NEW ADDRESS AND TELEPHONE NUMBER:

MCDONNELL DOUGLAS
2111 ANDERSON ST. UNIT B
VERNON, CALIFORNIA  90058

(213)589-1122


PLEASE UPDATE YOUR TELEPHONE DIRECTORIES
From: Osman Guven <OSMAN@D35> 
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
Received: from F29.Tymnet by D35.Tymnet; Fri, 8 Jul 88 14:07:35 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 8 Jul 88 14:07:16 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 8 Jul 88 14:00:40 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 08 JUL 88 11:12:41 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: OPERA.C/ELDRED@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, TXS.C/FLETCHER@Ontyme.Tymnet 
Message-id: A18386@Ontyme.Tymnet 
Subject: PDP10 PROBLEMS 

Osman;

Would appreciate someone from Software Support plan on attending this
meeting on 13 July at 1500 to clear up some questions.

Thanks Rick

DATE> 07 JUL 88
 
FROM> Cheryl Eldred
 
TO>   Mike Bellopatrick
      Mike Cromwell
      Sharon Marcotte
 
CC>   Rick Daniels
      Bill Fischer
 
SUBJ> PDP10 PROBLEMS
 
                              M E M O R A N D U M
 
-----------------------------------------------------------------------------
We need to reconstruct the events that occurred, from 7/1 to 7/6, in relation
to F74.
 
Please review the logs, for your shift hours, and confer with those members of
your staff who were involved in the interaction with F74 during the weekend
indicated above.
 
Please compile a in depth report that includes all pertinent data in relation
to F74.  I would like a copy of this report by the morning of Tuesday, 7/12.
Additionally, I am calling a staff meeting for Wednesday, 7/13, at 1500 to dis-
cuss F74 as well as F33's conversion.  Please bring the report you compiled
with you to this meeting.
 
Thank you,
 
Cheryl
Received: from F29.Tymnet by D35.Tymnet; Fri, 8 Jul 88 14:07:45 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 8 Jul 88 14:07:23 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 8 Jul 88 14:00:48 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 08 JUL 88 11:27:37 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: IPC.J/ENGLISH@Ontyme.Tymnet, IPC.E/BARENS@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A18393@Ontyme.Tymnet 
Subject: "SUBJ: To: Jim English Ed Barens"... 

SUBJ: 

To:  Jim English
     Ed Barens

From: Rick Daniels

Under (FREMONTDOC:74) there are two updated TYMCOM-10 Mini-manuals.

Filenames are :  PDPTAP.M1A and PDPREC.M5A.  In M1A  taking an ASP
Save has been updated regarding HARD errors before the SAVE   and
after.

M5A - See Summary of Changes.

Thanks Rick Daniels
Received: from F74.Tymnet by D35.Tymnet; Mon, 11 Jul 88 7:15:02 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Mon, 11 Jul 88 7:13:44 PDT 
To: OSMAN, CARL, JMS, DANIELSR 
Subject: "When I go into TUMS and do a READ, "... 


When I go into TUMS and do a READ, I receive the following message:

% phase error; EOT found in message.

What does this mean?

Thanks Rick
Received: from F29.Tymnet by D35.Tymnet; Mon, 11 Jul 88 14:05:07 PDT
Received: from D35.Tymnet by F29.Tymnet; Mon, 11 Jul 88 14:26:38 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Mon, 11 Jul 88 14:00:44 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 11 JUL 88 09:03:57 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A19182@Ontyme.Tymnet 
Subject: FYI .. 
FYI..: 

Osman;

In reference to the below message.  If FSC wants to pay for my
expenses, Bill Fischer says I can go.  IPC can't afford it.

Thanks Rick

----------------------------------------------------------------
Received: from F29.Tymnet by EMSTXS.Ontyme.Tymnet; Thu, 7 Jul 88 12:18:37 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 7 Jul 88 12:01:40 PDT
Return-path: <OSMAN@D35> 
From: Osman Guven <OSMAN@D35> 
Date: Thu, 7 Jul 88 12:01:02 PDT 
To: Rick Daniels <IPC.R/Daniels@Ontyme> 
Subject: FYI .. 

(begin forwarded message)
----------------------------------------------------------------
From: Osman Guven <OSMAN@D35> 
Date: Tue, 5 Jul 88 22:48:38 PDT 
To: Ed Barens <IPC.E/Barens@Ontyme>, Jim English <IPC.J/English@Ontyme>, Jim
        Zone <IPC.J/Zone@Ontyme>, John Roddam <IPC.J/Roddam@Ontyme>, Leland
        Yarbrough <IPC.L/Yarbrough@Ontyme>, Jerry Meyer <FSC.G/Meyer@Ontyme> 
Subject: FYI .. 


Ed ..

Per our phone conversation ..

o  Please go ahead and load Dallas's Bases whenever the systems come
   down next time.  Base codes are located on systems 25, 54 and 33.
   User name is (EBUS), file name NWxxxx.BND.

   Fremont D.C. has been running with the "NEW BASE CODE" successfully and
   without any problems.  Again, one of the important fixes in the new base
   code is: Prevention of host/base crashes when system has been up more
   than 41 days.

   If you have any problems with the new base code, please send mail indicating
   on what system it was not successful.


o  I am planning to be in Dallas Aug 2-4 to teach a seminar to "Operations"
   and "MDFSCO" and whoever else is interested in attending.

   The basic thrust of the seminar will be to go over the new and updated
   monitor functionality and some system utilities.

See you all soon.

-Osman-
----------------------------------------------------------------
(end forwarded message)
Received: from B39.Tymnet by D35.Tymnet; Mon, 11 Jul 88 14:40:54 PDT
Return-path: <DANIELSR@B39.Tymnet> 
From: DANIELSR@B39.Tymnet 
Date: Mon, 11 Jul 88 14:40:45 PDT 
To: OPER, BBUNYAN, JSTIER, SRA, RICHARDSON, DANIELSR, OSMAN, CARL, JMS, TYMRES,
	SALTYRON, KRUMVIEDE, FSC.R/DONAHUE@ONTYME 
Subject: "Ron; It appears ELFOPER is messed"... 


Ron;

It appears ELFOPER is messed up on system 38.  Or is it that this
system is always down.  System 83 - no problem.


Please let me know.  Thanks rick
Received: from B39.Tymnet by D35.Tymnet; Mon, 11 Jul 88 15:15:12 PDT
Return-path: <DANIELSR@B39.Tymnet> 
From: DANIELSR@B39.Tymnet 
Date: Mon, 11 Jul 88 15:14:22 PDT 
To: OPER, BBUNYAN, JSTIER, SRA, RICHARDSON, DANIELSR, OSMAN, CARL, JMS, TYMRES,
	SALTYRON, KRUMVIEDE, FSC.R/DONAHUE@ONTYME 
Subject: "Ron; Can someone send me the procedures"... 


Ron;

Can someone send me the procedures I need to change the passwords

for ELFOPER, OPER, UTIL, and HOLD using NETVAL on BUBBNET.

Thanks Rick
Received: from B39.Tymnet by D35.Tymnet; Mon, 11 Jul 88 15:31:35 PDT
Return-path: <DANIELSR@B39.Tymnet> 
From: DANIELSR@B39.Tymnet 
Date: Mon, 11 Jul 88 15:31:44 PDT 
To: OSMAN 
Subject: "OSMAN; Thanks for the monitor"... 


OSMAN;


Thanks for the monitor - I'll get it on tape.

Rick
Received: from F33.Tymnet by D35.Tymnet; Mon, 11 Jul 88 20:27:52 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Mon, 11 Jul 88 20:20:38 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: F30's problem was an off-by-one bug in ONCE. 

EVASIZ has the highest unused address.  When EVACLR rounded it up to a page
boundary, it did so using a method that failed if EVASIZ just happened to
be on a page boundary.  In particular, the monitor asked for memory up to
and including address 744777, which made EVASIZ be 745000 (the first unused
location).  The old code using IORI T4,777 and ADDI T4,1 would round that
up to address 746000, instead of leaving it at 745000.  (A kludge to get
around the problem was to bump PATSIZ up by a couple of pages - incrementing
it by any number that was not a multiple of 145 did the trick.)  The file
EVACLR.PAT has been created, and added to DMON.PAT as patch #8.   /Joe

File 1)	DSK:ONCE.D07	created: 2007 29-APR-88
File 2)	DSK:ONCE.MAC	created: 1955 11-JUL-88

1)25	        IORI	T4,777		;MAKE SURE LAST LOC LOOKS LIKE IS
1)					; LAST LOC OF A PAGE
1)		ADDI	T4,1		;T4 GETS FIRST UNUSED LOCATION
1)		CAMLE 	T4,EVAMAX	;MAKE SURE ONCE NOT OVERWRITTEN.
****
2)25	 ;P035/D08 - fix so it doesn't crash when EVASIZ is on a page boundary
2)	        ADDI	T4,777		;MAKE SURE LAST LOC LOOKS LIKE IS
2)					; LAST LOC OF A PAGE
2)		TRZ	T4,777		;T4 GETS FIRST UNUSED LOCATION
2)		CAMLE 	T4,EVAMAX	;MAKE SURE ONCE NOT OVERWRITTEN.
**************
From: Carl A Baltrunas <Carl> 
Date: Tue, 12 Jul 88 0:34:26 PDT 
To: Monitor Group <Carl>, Osman Guven <Osman>, Joe Smith <JMS>, Craig Fletcher
	<fletcherc> 
Subject: Delay in mail F30/B39 

I just got around to sending mail.  Looks like mail form operations and
Joe finding the solution to F30 are all that is necessary.  Bravo!

PS. F30 is up with PATSIZ set to PAT+3000.  I didn't check to see if Joe
    changed it back when he fiddled with NEWMON on F30...?

/Carl
Received: from F29.Tymnet by D35.Tymnet; Tue, 12 Jul 88 12:42:33 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 12 Jul 88 12:59:09 PDT 
To: <DANIELSR@F74.Tymnet> 
Cc: OSMAN, CARL, DANIELSR 
Subject: Re: "When I go into TUMS and do a READ, "... 
In-reply-to: your message of Mon, 11 Jul 88 7:13:44 PDT

Soley says it means the TIX file is confused.  This error can be ignored.
The next version of TUMS will fix the error when it happens.  /Joe
Received: from F29.Tymnet by D35.Tymnet; Tue, 12 Jul 88 14:07:45 PDT
Received: from D35.Tymnet by F29.Tymnet; Tue, 12 Jul 88 14:28:17 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Tue, 12 Jul 88 14:05:32 PDT
Return-path: <IPC.S/PAL@EMSFSC.Ontyme.Tymnet> 
From: IPC.S/PAL@EMSFSC.Ontyme.Tymnet 
Date: 12 JUL 88 12:04:58 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: A19917@Ontyme.Tymnet 
Subject: "Reminder...Change Control Meeting"... 

Reminder...Change Control Meeting will be held Wednesday 7/13 at 10:30.
There will be a presentation on Turbo Engine.  See you there !
 
Thank you,
Sue Pal
Received: from F29.Tymnet by D35.Tymnet; Thu, 14 Jul 88 11:19:57 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 14 Jul 88 11:24:12 PDT 
To: DANIELSR@F29.Tymnet 
Cc: OPER, BILLF, CARL, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON, PKRUMV@F33,
	FSC.R/DONAHUE@ONTYME 
Subject: Re: "Appears that the CTYLOG file is"... 
In-reply-to: your message of Thu, 14 Jul 88 7:53:46 PDT

(MPL)CTYLOG reads and displays all output to the CTY that is still in the
buffer.  Once it's caught up, CTYLOG sleeps waiting for new messages to
appear in the buffer.  When they do, these messages are reported to your
terminal in real-time.  If you are running CTYLOG at 4:00, 8:00, or 12:00,
you will see the BIGBEN messages on your terminal at the same time they
show up on the CTY.

CTYLOG will keep watching and sleeping until you type a RETURN.
			/Joe
Received: from F74.Tymnet by D35.Tymnet; Thu, 14 Jul 88 11:29:27 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Thu, 14 Jul 88 11:27:41 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME 
Subject: "OPERATIONS; PLEASE PRINT AND POST"... 


OPERATIONS;

PLEASE PRINT AND POST NEW DSKMAPs ON ALL PDP-10's(EXCEPT X32).

DSKMAPs NOW INDICATE FROM WHERE TO LOAD THE EBUS AND WHAT PACKCOPY
VERSION TO USE ON THE SYSTEMS.

THANKS RICK
Received: from F38.Tymnet by D35.Tymnet; Thu, 14 Jul 88 12:30:03 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 14 Jul 88 12:21:31 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, Rick Daniels <OPERA.SUP@EMSTXS.Ontyme> 
Subject: "?Address check for device TTY" means "no such user (GAMES)". 

Rourke McCusker (the head of QSATS) had a problem that when he typed
"DO CIAO", it aborted with "?Address check for device TTY..." today, but it
was working yesterday.  That obscure error message really means that one
of the directories in his DO list does not exist.  (The DO list had been
set up for him by somebody else in 1983.)  The directory that was deleted
is GAMES.  I fixed his problem by:
	-EDIT INIT.SAI
	<remove "GAMES">
	-LOAD INIT.SAI
	-SAVE INIT
	-INIT

File 1)	DSK:INIT.BAK[21477,243135]	created: 0954 13-JUL-83
File 2)	DSK:INIT.SAI[21477,243135]	created: 1209 14-JUL-88

1)1	setdo("GAMES","PNETS","UPL","MPL","TYMNET","SPL");	! do list;
1)	RPGini;					! set this all up;
****
2)1	setdo("PNETS","UPL","MPL","TYMNET","SPL");	! do list;
2)	RPGini;					! set this all up;
**************

I'm sending this message so that we can all recognize this problem when other
people report it.

				/Joe
Received: from D55.Tymnet by D35.Tymnet; Thu, 14 Jul 88 12:49:31 PDT
Return-path: <JMS@D55.Tymnet> 
From: JMS@D55.Tymnet 
Date: Thu, 14 Jul 88 14:43:55 CDT 
To: OPER, SRA, MEYERG, JENGLISH, BILLF, LELANDY, OSMAN, CARL, JMS, ZONE,
	SCRIBNER, RODDAMJ, PKRUMV@F33, FSC.R/DONAHUE@ONTYME, DIAG10D 
Subject: "NW5577.BND was created 18-May"... 

NW5577.BND was created 18-May using a TYM file which specified 2 sync lines.
ND5577.BND was created 19-May when D55's base was upgraded to use SIO.
I rebuilt NW5577.BND today with the right configuration, and copied it to
the standard places - (EBUS) on F33, D25, D54, and (TYM5) on D54
Received: from F29.Tymnet by D35.Tymnet; Thu, 14 Jul 88 14:05:35 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 14 Jul 88 14:27:04 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 14 Jul 88 14:00:27 PDT
Return-path: <ACS.E/GAGLIARDI@EMSTXS.Ontyme.Tymnet> 
From: ACS.E/GAGLIARDI@EMSTXS.Ontyme.Tymnet 
Date: 14 JUL 88 18:53:50 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A21087@Ontyme.Tymnet 
Subject: BEGINNERS ONTYME CLASS 
BEGINNERSONTYMECLASS: 

                      APPLIED COMMUNICATIONS SYSTEMS

 
                           M E M O R A N D U M
 
 
                                                      []  ISG     
 
DATE>      14 JUL 88  10:49

TO>        *** SUPERVISORS

COPIES>    

FROM>      ACE GAGLIARDI


SUBJECT>   BEGINNERS ONTYME CLASS


-----------------------------------------------------------------------


PLEASE POST THE FOLLOWING FLIER FOR THE UPCOMING BEGINNERS ONTYME CLASS.

THANKS,
ACE

******************************************************************************
 
                             LEARN THE ESSENTIALS
 
                                      of
 
                                    ONTYME
 
 
                                    at the
 
 
                               ONTYME BEGINNERS
 
                                  "BOOT CAMP"
 
 
                 Just be ready to roll up your sleeves and get
                   your hands dirty, 'cause emphasis will be
 
                              HANDS-ON TRAINING.
 
 
                         WHEN:  JULY 22, 1988 (FRIDAY)
 
                8:30 - 10:30   OnTyme Training      Ace Gagliardi
               10:35 - 12:30   ISG Mail Training    Tricia Callahan
 
                           **  PLEASE BE PROMPT ! **
 
 
                        WHERE:  MCDONNELL DOUGLESS TRAINING CENTER
                                2665 NORTH FIRST ST.
                                SUITE 250 - ROOM 7
                                SAN JOSE, CA
 
 
                  TO REGISTER:  Call Ace Gagliardi at
                                 (408)922-7052
 
 
 
 
               CLASS SIZE IS LIMITED SO PLEASE REGISTER EARLY !!
 
******************************************************************************
Received: from X32.Tymnet by D35.Tymnet; Thu, 14 Jul 88 22:50:41 PDT
Return-path: <JMS@X32.Tymnet> 
From: JMS@X32.Tymnet 
Date: Thu, 14 Jul 88 22:43:40 PDT 
To: CARL, OSMAN, JMS, DANIELSR, FSC.R/DONAHUE@ONTYME 
Subject: "X32 down due to parity error."... 

X32 down due to parity error.

19:07 - Osman disables sector 3, hole from 6000000 to 777777, 2560K online
20:10 - Parity error sector 0, modules 0 and 1.  Sector 0 disabled, sector 2
 	switched to be ad address 0.  System dies in ONCE while sizing memory,
	parity error in sector 1, modules 0 and 1.  Sector 1 disabled, leaving
	only sector 2 in the LX box, and 1Meg in the LS box.
22:37 - System up with hole from 2,,000000 to 7,,777777, highest address is
	13,,777777 wit 1536K on line.

		/Joe
Received: from F29.Tymnet by D35.Tymnet; Sun, 17 Jul 88 14:03:46 PDT
Received: from D35.Tymnet by F29.Tymnet; Sun, 17 Jul 88 14:25:23 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Sun, 17 Jul 88 14:00:42 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 17 JUL 88 13:11:13 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: NTD.R/SALTGAVER@Ontyme.Tymnet, NTD.S/BUNYAN@Ontyme.Tymnet,
	NTD.J/STIER@Ontyme.Tymnet, TXS.C/FLETCHER@Ontyme.Tymnet,
	TXS.J/SMITH@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, IPC.E/ROOP@Ontyme.Tymnet,
	IPC.B/FISCHER@Ontyme.Tymnet, IPC.S/MARCOTTE@Ontyme.Tymnet,
	IPC.M/BELLOPATRICK@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A22084@Ontyme.Tymnet 
Subject: Converting System 39 To A PAGE Monitor 

                         M E M O R A N D U M





DATE>      17 JUL 88  13:10

TO>        Ron Saltgaver

COPIES>    See CC List

FROM>      Rick Daniels


SUBJECT>   Converting System 39 To A PAGE Monitor


-----------------------------------------------------------------------


To give system 39 more disk space, Fremont Data Center is planning
on converting it to a PAGE monitor the weekend of 6 - 7 August 1988.
Converting to a PAGE monitor from a BLOCK monitor picks up about 11%
of free pages on each unit.

While doing this conversion, system 39 will be unavailable for
approximately 48 hours.

Please discuss this with your Group/Users and let Fremont Data Center
know if this timeframe is acceptable.  It is is not, let the Center
know when it can be done.

Thanks



Rick Daniels
(IPC.R/DANIELS)
Telephone:  (415)498-2595
Received: from F29.Tymnet by D35.Tymnet; Mon, 18 Jul 88 12:57:50 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Mon, 18 Jul 88 12:58:44 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: Dispatch wants us to close incidents. 

A typical call to Central Dispatch goes like this:
  o  The Data Center has a problem that may be software, they call Dispatch.
  o  Dispatch opens an incident number, pages us.
  o  We call Dispatch, get system number, this marks incident as answered.
  o  We look at problem, fix it, or tell them it's a hardware problem.
  o  Third Party Maintenance fixes the hardware, opens a new incident number,
     gives Dispatch a code number for what was replaced, closes the incident.
  o  Third Party happens to notice an open incidence number pertaining to
     the system they just fixed, and closes it for us.

We have been unaware of that last step.  If it is not done, a timer
automatically escalates it.  If the original incident was logged on Friday
evening, it may escalate to the Regional Manager by Monday morning.

To avoid this in the future, we need to be sure to get the incident number
when answering the page from Dispatch, and to call Dispatch back when we
are done working on the call.  The incident number needs to included in the
mail sent to MONITOR and included in (M33:33)HOTLIN.LOG.

		/Joe
Received: from F29.Tymnet by D35.Tymnet; Tue, 19 Jul 88 14:06:24 PDT
Received: from D35.Tymnet by F29.Tymnet; Tue, 19 Jul 88 14:26:58 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Tue, 19 Jul 88 14:04:34 PDT
Return-path: <FSC.L/RODRIGUEZ@EMSFSC.Ontyme.Tymnet> 
From: FSC.L/RODRIGUEZ@EMSFSC.Ontyme.Tymnet 
Date: 19 JUL 88 10:47:58 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P77432@Ontyme.Tymnet 
Subject: ECLTIP 

               -5.2 VOLT HEAT SINK TECH TIP

KL-10 USING H761 FOR CPU ECL POWER.

NOTES

    IN THE KL HANDBOOK DIAGRAMS/MULS P.7,PB2 ON HSA5 SHOULD BE 
    LABELED PB3,IN ORDER FOR HSA5 TO FOLLOW THE SAME CONVENTION 
   AS HSA3 & 4.

    IN THE KL10D PRINTS H761(CONTROL)WIRING DIAGRAM SHEET 2,HSA5
    DOES NOT SHOW THE CORRECT PIN USAGE FOR J11 & 12.PB1 USES
    PINS OF J11.PB3 USES PINS 1,2,3 OF J12.

I HAD A POWER PROBLEM THAT I ISOLATED TO A BAD HSA3 TRIPPING CB3.
I LOOKED IN THE KL-10 HANDBOOK UNDER DIAGRAMS/MULS P.6B,7AND
SHEET2 OF THE KL-10D PRINTS H761 WIRING DIAGRAM TO PIN-
POINT THE DEFECTIVE PB. WHICH TURNED OUT TO BE PB3.I KNEW THAT 
HSA5 HAS 2 UNUSED SECTIONS PB2 & PB4,STATUS UNKNOWN.

I MOVED THE 2 WIRES CONNECTED TO CB3(H761 WIRING DIAG.SHEET1/6C)
DOWN TO THE TOP REAR UNUSED PB2 CONNECTION OF HSA5.I REMOVED THE 3
WIRES IN P8(MALE PINS 1,2,3)WHICH ARE ASSOCIATED WITH PB3(ECL P7)
TO P11 PINS 4,5,6 OF HSA5 (MATCH PINS 1&4,2&5,3&6)I DOUBLE
CHECKED MY MOVES AND POWERED UP THE KL-10 AFTER LEAVING A NOTE ON 
THE WIRES AND INSTALLING THE COVER.POWER WAS RESTORED AND I RAN 
DIAGS.

I HOPE THAT THIS EXAMPLE WILL BE CLEAR ENOUGH SO THAT THE F.E. CAN
USE PB4 OF HSA5 FOR A BAD PB ON HSA4 IN A PINCH.

I RECOMMEND STOCKING THE FOLLOWING PARTS FOR LOCAL REPAIR OF
HEATSINKS.&a010c023R

        1.DOW CORNING 340 SILICONE HEATSINK COMPOUND OR EQUIV.
 
        2.TRANSISTOR 2N5301 ,2N3715

        3.RECTIFIER 1111481

        4.TRANSISTOR SOCKET 1210130


GOOD LUCK!!!

LENNY RODRIGUEZ.
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
Received: from X32.Tymnet by D35.Tymnet; Wed, 20 Jul 88 1:28:41 PDT
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Wed, 20 Jul 88 1:01:03 PDT 
To: Craig Fletcher <fletcherc>, Joe Smith <JMS>, Osman Guven <Osman> 
Subject: AI project report form 

Craig,
  As per the FAX that I received from Jerry Swanson requesting info on the
expert system that I will be working on.  [I did NOT bring a copy of the
newly written objective we discussed this week home.]  Could you please
check the enclosed info and send me a response via PDP-10 mail rather than
ONTYME as to content and the correct titles of the experts.

  Our titles probably don't matter, but since this is to be published for
all of ISC, I would appreciate that we all agree on titles so that Bill K
and each of us have no objections.  I don't care whether I am a Sr Systems
Programmer, or Sr. Systems Software Engineer, Maintenance Software E...  I
feel that we shoul dbe consistent among ourselves.

Joe,  any comments? please CC: Craig

Craig,  please indicate Osman's title,  I do not know if he is Sr or not as
        far as official titles are concerned, since he is definately Sr in
        his hardware knowledge!

----- From the FAX -----
Enclosure 1:  Categories
    Interpretation    Inferring meaning from data
    Prediction        Inferring likely consequences of given situation
    Diagnosis         Inferring system malfunctions from observables
    Design            Configuring objects under constraints
    Planning          Designing Actions
    Monitoring        Comparing observations to expected outcomes
    Debugging         Prescribing remedies for malfunctions
    Repair            Executing plans to administer remedies
    Instruction       Diagnosing, debugging and correcting students
    Control           Governing overall system behavior
    Classification    Placing an item in one of many possible groups
    Learning          Self modification of knowledge base
    Other             A category other than those listed above

Enclosure 2:  Standard AI Project Report Format
    Project Title     Title of the project
    Objective         Main objective of the application
    Category          Application category from enclosure
    PI                Principle Investigator, component
    Expert            Experts name, component, title
    COmpletion        Expected project completion dates
    Funding           FUnding source

----- my response (draft) -----



Project Title:  TYMCOM-X Stopcode Diagnosis (and Action)

Objective:      The main objective of this project is to determine the cause
                of the TYMCOM-X operating system shutdown from the stopcode
                messages printed on the system console.  A secondary objective
                is to prescribe the appropriate action to take to bring the
                system back to full optimum performance.

Categories:     Diagnosis, Planning, Repair

PI:             Carl A Baltrunas, MDFSCO

Experts:        Carl A Baltrunas, MDFSCO, Sr. Systems Software Engineer
                Joseph Smith, MDFSCO, Sr. Systems Software Engineer
                Osman Guven, MDFSCO, Systems Software Engineer

Completion:     4th quarter, 1988.

Funding:        MDFSCO department 90842


----- message continued -----

Craig,  we didn't give the project a title, and I recal talking about a
        disk-repair expert system, so I wanted to clarify what our objective
        was to match the project to your job objective and mine.

Am I presuming too much in saying we will pool the knowledge from all three
experts?  Should I include anyone else?  (For diagnosis, i don't know if
anyone other than Craig could be added; Craig is good at pulling information
out of all of us in a directive manner;  For planned response/repair there
may be help from Rick or BIll R?  comments?)

PS.  I realize I should have asked this stuff while I was at the office, but
     my brain was on vacation even though I was at the office.

/Carl
Received: from F29.Tymnet by D35.Tymnet; Thu, 21 Jul 88 2:52:30 PDT
Received: from B39.Tymnet by F29.Tymnet; Wed, 20 Jul 88 19:54:06 PDT
Received: from tymix.Tymnet by B39.Tymnet; Wed, 20 Jul 88 19:23:10 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA08866; Wed, 20 Jul 88 19:18:54 PDT
Received: by antares.Tymnet.com (3.2/SMI-3.2) id AA11407; Wed, 20 Jul 88
	19:14:30 PDT
Return-path: <JMS@F29.TYMNET> 
From: antares!jms (joe smith) 
Date: Wed, 20 Jul 88 19:14:30 PDT 
To: carl, osman, Craig Fletcher <FletcherC> 
Message-id: <8807210214.AA11407@antares.Tymnet.com> 
Subject: Ada quote 
Resent-from: Joe Smith <JMS@F29.TYMNET> 
Resent-date: Thu, 21 Jul 88 3:39:21 PDT 
Resent-to: Monitor Group <JMS>, Carl, Osman 
Resent-cc: FletcherC 

From tymix!oliveb!ames!mailrus!cornell!rochester!pt.cs.cmu.edu!sei!ajpo!eberard Wed Jul 20 19:13:41 PDT 1988
Article 207 of comp.lang.ada:
Path: antares!tymix!oliveb!ames!mailrus!cornell!rochester!pt.cs.cmu.edu!sei!ajpo!eberard
>From: eberard@ajpo.sei.cmu.edu (Edward Berard)
Newsgroups: comp.lang.ada
Subject: "Ada Bashing" article in Federal Computer Week
Keywords: Ada, C, Federal Computer Week
Message-ID: <356@ajpo.sei.cmu.edu>
Date: 20 Jul 88 13:45:36 GMT
Lines: 44

The July 11, 1988 issue of Federal Computer Week has an editorial
written by Fred Reed ("Ada: A View From the Field", page 19) which
supposedly presents both the pros and cons of Ada. The following
quotes are representative:

"What follows is one man's opinion, but it is an informed opinion --
he programs in Ada as well as C and other -- so I thought it was worth
passing along. We'll call him Bob because, Ada being a fairly sacred
cow, he didn't want to be named."

"... Ada has been called everybody's favorite 17 programming
languages." 

"Fewer programming errors. Among other features, Bob explained, Ada
has what are called strong typing and range checking. Strong typing
means that the programmer is forced to tell the computer the nature of
each piece of information he uses. This allows the machine to be sure
incompatible kinds of information are not mixed."

"Ada, said Bob, is well suited to being understood and maintained by
many programmers who will work on a large program over its lifetime.
This is why it saves money, he said."

"However, he noted, these strength also result in weaknesses. For
example, the programmer has to write extra code to do the strong
typing and range checking, and this, among other things, makes Ada
code slow to write."

"Further, because the program constantly has to check ranges and data
types as it runs, Ada produces much slower programs than some other
languages.

"An Ada program typically runs a tenth as fast as the same program in
C," said Bob. "I think we are seeing the bureaucratic attitude at its
ultimate. Ada is a prudent approa

ch to software. It is defintely
suboptimal in performance -- slow, bulky, verbose and slow to write.
But it is reliable and easy to work with after it is written. It is
safe. These are qualities prized by bureaucrats. DoD chose reliability
over performance."

				-- Ed Berard
				   (301) 695-6960


Received: from B39.Tymnet by D35.Tymnet; Thu, 21 Jul 88 9:42:03 PDT
Return-path: <JMS@B39.Tymnet> 
From: JMS@B39.Tymnet 
Date: Thu, 21 Jul 88 9:36:48 PDT 
To: OPER, BBUNYAN, JSTIER, SRA, RICHARDSON, DANIELSR, OSMAN, CARL, JMS, TYMRES,
	SALTYRON, KRUMVIEDE, FSC.R/DONAHUE@ONTYME 
Subject: "I was unable to login over shut"... 

I was unable to login over shut last night to investigate B39's crash dumps.
It appears that username JMS no longer has shut-override status.  The grave
shift operators were having problems as well.  Please investigate as to why
I could not login over shut to JMS:39 from a gateway.  Also verify usernames
CARL and OSMAN.           /Joe
Received: from F29.Tymnet by D35.Tymnet; Fri, 22 Jul 88 14:08:01 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 22 Jul 88 14:29:50 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 22 Jul 88 14:00:44 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 21 JUL 88 19:27:04 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (30 names) 
Message-id: A23830@Ontyme.Tymnet 
Subject: Out Of Office 

                         M E M O R A N D U M





DATE>      21 JUL 88  19:26

TO>        Supervisors

COPIES>    See CC list

FROM>      Rick Daniels


SUBJECT>   Out Of Office


-----------------------------------------------------------------------


I will not be in the office on 29 July and will be out of the area
the weekend.

Rick Daniels
Received: from F29.Tymnet by D35.Tymnet; Fri, 22 Jul 88 14:08:13 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 22 Jul 88 14:30:02 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 22 Jul 88 14:01:02 PDT
Return-path: <ENS.K/BEDELL@EMSTXS.Ontyme.Tymnet> 
From: ENS.K/BEDELL@EMSTXS.Ontyme.Tymnet 
Date: 22 JUL 88 09:58:37 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: FSC.O/GUVEN@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	FSC.C/BALTRUNAS@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	OPERA.D/HIGHTOWER@Ontyme.Tymnet, ENS.K/BEDELL@Ontyme.Tymnet 
Message-id: I07176@Ontyme.Tymnet 
Subject: MAGNUM problem on host F74 

           McDonnell Douglas Network Systems Company              M/S: F24
               TYMNET External Network Services
                                                  Tel. [-1] (408) 922-6473
 
DATE>      1988-07-22  09:48 PDT                  Fax. [-1] (408) 922-7030
 
TO>        Osman Guven (FSC.O/GUVEN)
                   and (TXS.O/GUVEN)
 
COPIES>    Carl Baltrunas (FSC.C/BALTRUNAS)
                      and (TXS.C/BALTRUNAS)
           Debbie Hightower (OPERA.D/HIGHTOWER)
 
FROM>      Ken BeDell (ENS.K/BEDELL)
 
SUBJECT>   MAGNUM problem on host F74
 
REFERENCE> M77363 attached
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Osman, please note the attached Ontyme message from NIS (Japan).
 
     They're having problems with Magnum on host F74, and it's not appar-
     ant to me what must be done to fix the problem.
 
     I note no one from the Tymcom X support group was copied and Rick
     Daniels is presently out of his office, although I have phone call
     into him.
 
     Hopefully, we can identify and fix the problem prior to Japan's Mon-
     day (Sunday afternoon).
 
     Let me know if I can be of assistance.  Thanks and regards,
     Ken (x 6473).
 
                                        v
                                        |
                                        |
                                        |
                                        |
                                        V

MSG#:M77363
 IN#:  2019
  TO: K/BEDELL        
FROM: NIS      OGURA           
SENT: 21 JUL 88 23:09:28  
READ: 22 JUL 88 08:42:07  
  CC: IPC.R/DANIELS  IPC.J/KRIVANEC  K/BEDELL  NIS.KAMOI  NIS.OSAKA 
      NIS.ITOH  NIS.OGURA 
 
 
     
     
        Network Information Service          OnTyme  NIS.TC 
                                             Phone   [-81] (3) 551 6115    
                                             Fax     [-81] (3) 551 6355    
Date      : 88/07/22  15:06 (JST)  
     
Attn      : Rick Daniels    (IPC.R/DANIELS)  
     
Copies    : Jan Krivanec    (IPC.J/KRIVANEC) 
            Ken BeDell      (ENS.K/BEDELL)   
            Kuni Kamoi      (NIS.KAMOI) 
            Shuichi Inoue   (NIS.OSAKA) 
            Hajime Itoh     (NIS.ITOH)  
     
From      : Makoto Ogura    (NIS.OGURA) 
     
Subject   : F74 (MAGNUM) Problem   
            URGENT !!!   
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
Rick,
     
This morning on Japan, we have a problem about MAGNUM access on F74, maybe 
you know. I checked F-center on it and was respounded that it would be
followed at noon.   
Then, this afternoon, i tried and found some MAGNUM access was OK, however 
some MAGNUM access still have the problem. We, NIS,may be requested by
the Japanese users some conpensation about the problem, because adding
the current problem they have the big problem at the begin of this month.  
     
        1. NIS must explain about the problem to the customers.  
                What was happened on F74.    
                What was the cause.
           Please let me know it in detail.  
     
        2. Some user still has the problem. Some user can not use MAGNUM yet.   
           So please advise us how we can repaire it or what you can do for it. 
     
           Major usernames which is indicated messages from the system saying   
           the file was damaged.   
     
                MMETAL   
                MVX 
                MPPC
                NETBILL  
     
           Especially, about MVX, it seems that the files on MAGNUM which  
           was not mentioned in the message was garbled. Simple command on 
           MAGNUM is NOT available. Please advise ASAP.
     
           I attached sample log of MVX access for your reference.    
     
NIS, and the customers want to repaire it by the next Monday morning. 
So please reply ASAP.    
     
Best regards,  
Makoto    
     
=============== access log for MVX  ================== 
-day 
22-JUL-1988 14:36:03 JST 
     
-WHO 
 1  SRA   
 2  OPER  
 3  PJ    
 4  PERPOPER   
 5  PERPOPER   
 6* OGURA +    
 7  OPER  
 8  NISADM
 9  OPER  
10  DANIELSR   
11  NETENTRY   
12  NETENTRY   
14  J1STAR
15  MKTGINFO   
16  RICHARDSON 
18  OGURA +    
     
-GFD MVX  
DSKB:X8N5FR.TTV found bad by damage assessment program 
DSKB:XD0DFR.MR0 found bad by damage assessment program 
DSKB:N761S2.MP0 found bad by damage assessment program 
DSKB:TOK.TRN found bad by damage assessment program    
     
-MAG 
22-JUL-1988  14:36:18    
     
=:SCHEMA AIS   
     
User: OGURA        in MVX
     
     
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
Illegal Memory Reference at 312000554413
     
?HALT at user PC 555000  
     
-MAG 
22-JUL-1988  14:36:30    
     
=:SCHEMA AIS   
     
User: OGURA        in MVX
     
     
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
     
?FATAL ERROR NUMBER = 0004    
     
?HALT at user PC 510110  
     
-MAG 
22-JUL-1988  14:36:48    
     
=:SCHEMA AIS   
     
User: OGURA        in MVX
     
     
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
     
?FATAL ERROR NUMBER = 0004    
     
?HALT at user PC 510110  
     
-    
-LOG 
96.35 tru 
terminal time: 0:01:06   
=================  end of log  =======================
Received: from F29.Tymnet by D35.Tymnet; Fri, 22 Jul 88 14:09:52 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 22 Jul 88 14:31:36 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Fri, 22 Jul 88 14:06:27 PDT
Return-path: <ENS.K/BEDELL@EMSFSC.Ontyme.Tymnet> 
From: ENS.K/BEDELL@EMSFSC.Ontyme.Tymnet 
Date: 22 JUL 88 09:58:37 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Cc: FSC.O/GUVEN@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	FSC.C/BALTRUNAS@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	OPERA.D/HIGHTOWER@Ontyme.Tymnet, ENS.K/BEDELL@Ontyme.Tymnet 
Message-id: I07176@Ontyme.Tymnet 
Subject: MAGNUM problem on host F74 

           McDonnell Douglas Network Systems Company              M/S: F24
               TYMNET External Network Services
                                                  Tel. [-1] (408) 922-6473
 
DATE>      1988-07-22  09:48 PDT                  Fax. [-1] (408) 922-7030
 
TO>        Osman Guven (FSC.O/GUVEN)
                   and (TXS.O/GUVEN)
 
COPIES>    Carl Baltrunas (FSC.C/BALTRUNAS)
                      and (TXS.C/BALTRUNAS)
           Debbie Hightower (OPERA.D/HIGHTOWER)
 
FROM>      Ken BeDell (ENS.K/BEDELL)
 
SUBJECT>   MAGNUM problem on host F74
 
REFERENCE> M77363 attached
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Osman, please note the attached Ontyme message from NIS (Japan).
 
     They're having problems with Magnum on host F74, and it's not appar-
     ant to me what must be done to fix the problem.
 
     I note no one from the Tymcom X support group was copied and Rick
     Daniels is presently out of his office, although I have phone call
     into him.
 
     Hopefully, we can identify and fix the problem prior to Japan's Mon-
     day (Sunday afternoon).
 
     Let me know if I can be of assistance.  Thanks and regards,
     Ken (x 6473).
 
                                        v
                                        |
                                        |
                                        |
                                        |
                                        V

MSG#:M77363
 IN#:  2019
  TO: K/BEDELL        
FROM: NIS      OGURA           
SENT: 21 JUL 88 23:09:28  
READ: 22 JUL 88 08:42:07  
  CC: IPC.R/DANIELS  IPC.J/KRIVANEC  K/BEDELL  NIS.KAMOI  NIS.OSAKA 
      NIS.ITOH  NIS.OGURA 
 
 
     
     
        Network Information Service          OnTyme  NIS.TC 
                                             Phone   [-81] (3) 551 6115    
                                             Fax     [-81] (3) 551 6355    
Date      : 88/07/22  15:06 (JST)  
     
Attn      : Rick Daniels    (IPC.R/DANIELS)  
     
Copies    : Jan Krivanec    (IPC.J/KRIVANEC) 
            Ken BeDell      (ENS.K/BEDELL)   
            Kuni Kamoi      (NIS.KAMOI) 
            Shuichi Inoue   (NIS.OSAKA) 
            Hajime Itoh     (NIS.ITOH)  
     
From      : Makoto Ogura    (NIS.OGURA) 
     
Subject   : F74 (MAGNUM) Problem   
            URGENT !!!   
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
Rick,
     
This morning on Japan, we have a problem about MAGNUM access on F74, maybe 
you know. I checked F-center on it and was respounded that it would be
followed at noon.   
Then, this afternoon, i tried and found some MAGNUM access was OK, however 
some MAGNUM access still have the problem. We, NIS,may be requested by
the Japanese users some conpensation about the problem, because adding
the current problem they have the big problem at the begin of this month.  
     
        1. NIS must explain about the problem to the customers.  
                What was happened on F74.    
                What was the cause.
           Please let me know it in detail.  
     
        2. Some user still has the problem. Some user can not use MAGNUM yet.   
           So please advise us how we can repaire it or what you can do for it. 
     
           Major usernames which is indicated messages from the system saying   
           the file was damaged.   
     
                MMETAL   
                MVX 
                MPPC
                NETBILL  
     
           Especially, about MVX, it seems that the files on MAGNUM which  
           was not mentioned in the message was garbled. Simple command on 
           MAGNUM is NOT available. Please advise ASAP.
     
           I attached sample log of MVX access for your reference.    
     
NIS, and the customers want to repaire it by the next Monday morning. 
So please reply ASAP.    
     
Best regards,  
Makoto    
     
=============== access log for MVX  ================== 
-day 
22-JUL-1988 14:36:03 JST 
     
-WHO 
 1  SRA   
 2  OPER  
 3  PJ    
 4  PERPOPER   
 5  PERPOPER   
 6* OGURA +    
 7  OPER  
 8  NISADM
 9  OPER  
10  DANIELSR   
11  NETENTRY   
12  NETENTRY   
14  J1STAR
15  MKTGINFO   
16  RICHARDSON 
18  OGURA +    
     
-GFD MVX  
DSKB:X8N5FR.TTV found bad by damage assessment program 
DSKB:XD0DFR.MR0 found bad by damage assessment program 
DSKB:N761S2.MP0 found bad by damage assessment program 
DSKB:TOK.TRN found bad by damage assessment program    
     
-MAG 
22-JUL-1988  14:36:18    
     
=:SCHEMA AIS   
     
User: OGURA        in MVX
     
     
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
Illegal Memory Reference at 312000554413
     
?HALT at user PC 555000  
     
-MAG 
22-JUL-1988  14:36:30    
     
=:SCHEMA AIS   
     
User: OGURA        in MVX
     
     
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
     
?FATAL ERROR NUMBER = 0004    
     
?HALT at user PC 510110  
     
-MAG 
22-JUL-1988  14:36:48    
     
=:SCHEMA AIS   
     
User: OGURA        in MVX
     
     
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
     
?FATAL ERROR NUMBER = 0004    
     
?HALT at user PC 510110  
     
-    
-LOG 
96.35 tru 
terminal time: 0:01:06   
=================  end of log  =======================
Received: from F29.Tymnet by D35.Tymnet; Fri, 22 Jul 88 15:12:01 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 22 Jul 88 15:34:17 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: AT&T crash dump analysis. 

Ray Donahue asked me to look at another crash dump.  I didn't find anything.


Received: from D35.Tymnet by F29.Tymnet; Thu, 21 Jul 88 14:15:18 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 21 Jul 88 14:01:11 PDT
Return-path: <FSC.R/DONAHUE@EMSTXS.Ontyme.Tymnet> 
From: FSC.R/DONAHUE@EMSTXS.Ontyme.Tymnet 
Date: 21 JUL 88 11:43:03 
To: TXS.J/SMITH@EMSTXS.Ontyme.Tymnet 
Message-id: P77995@Ontyme.Tymnet 
Subject: CRASH ANALYSIS 

JOE,COULD YOU PLEASE LOOK AT CRASH DOM600.EXE ON SPIDER.THE ONLY THING
THAT LOOKS DIFFERENT FROM THE LAST CRASH IS THAT FILDAE IS RUNNING ON
THE CPU WHICH TOOK THE SYSTEM DOWN.CPU #1 SER 1078 GOT THE DOM....

THANKS
RAY DONAHUE


From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 22 Jul 88 15:26:38 PDT 
To: <FSC.R/DONAHUE@EMSTXS.Ontyme.Tymnet> 
Subject: Re: CRASH ANALYSIS 
In-reply-to: P77995@Ontyme.Tymnet of 21 JUL 88 11:43:03

Tracking down MM interlock problems on an SMP system is very nasty.
I looked at it, and found nothing obvious.  My guess is that it will take
over a week of solid looking to find the problem, which will undoubtedly
require a change to the software to fix.  As I understand it, this would
exceed the amount of software support we have promised (if any) to this
customer.  I'll leave it at that.     /Joe

Received: from F33.Tymnet by D35.Tymnet; Fri, 22 Jul 88 17:51:04 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 22 Jul 88 17:48:23 PDT 
To: Rick Daniels <OPERA.SUP@EMSTXS.Ontyme>, osman 
Subject: PAKCOP version 30. 

I put the BOTLOD routine into PAKCOP.  It is in (M33) on 33 and 26 only and
needs to be tested to verify that adding the new routine to load BOOTS did
not impact anything else.   /Joe
Received: from F33.Tymnet by D35.Tymnet; Sun, 24 Jul 88 21:16:41 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Sun, 24 Jul 88 21:15:37 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: Hotline calls this weekend. 

24-Jul-88 13:00/Joe                     (Sunday)
Contact:  Fremont Operations, L T Smith, (415)498-2588, System 32
Dispatch: Called me at home, said they paged me twice with no response,
          however L T's page came thru (I was outdoors in Fremont at the time).
Problem:  X32 had  3 crashes after Lenny replaced some boards, an IME, a UIL,
          and PAGP3R.  System ran DSKCLN with no problems, died only while
          running diags (such as MEMEXR or XCORE).  Lenny wanted to know
          if this IME is the same as the previous ones.
Diagnosis:  IME caused when PC was set to the middle of CTYLOG.  There were
          no tracks as to how it got there.  The IME is not related to the
          SKIPLE J,.CPJOB problem this CPU was having earlier.

23-Jul-88 09:00/Joe                     (Saturday)
Contact:  Fremont Operations, Debbie Hightower, (415)498-2588, System 32
Problem:  Lenny was running diagnostics, trying to reproduce the problem on
          CPU 1336 (B39).  The two-pack test system would fail, but diags
          wouldn't.  Between the time I was paged and when I answered,
          DDQCB failed on bit 27.  All he needed at this point is verification
          that it was the same bit that was set in the IME dumps.


Received: from F29.Tymnet by D35.Tymnet; Mon, 25 Jul 88 14:07:20 PDT
Received: from D35.Tymnet by F29.Tymnet; Mon, 25 Jul 88 14:16:53 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Mon, 25 Jul 88 14:00:47 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 25 JUL 88 08:31:59 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A24646@Ontyme.Tymnet 
Subject: MAGNUM Problem on F74 

     
     
        Network Information Service          OnTyme  NIS.TC 
                                             Phone   [-81] (3) 551 6115    
                                             Fax     [-81] (3) 551 6355    
Date      : 88/07/25  19:48 (JST)  
     
Attn      : Ken BeDell      (ENS.K/BEDELL)   
     
Copies    : Rick Daniels    (IPC.R/DANIELS)  
            Jan Krivanec    (IPC.J/KRIVANEC) 
            Joe M Smith     (FSC.JM/SMITH)   
            Kuni Kamoi      (NIS.KAMOI) 
            Hajime Itoh     (NIS.ITOH)  
     
From      : Makoto Ogura    (NIS.OGURA) 
     
Subject   : MAGNUM Problem on F74  
            Ref I07635   
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
Ken san,  
     
Thanks for your support on that matter. 
     
However, now, we still have the problem on username MVX:74. 
We still have the Fatal MAGNUM Error as I am indication with attachement   
of this letter.
     
Please try and check it, and please advise on that matter.  
     
Best regards,  
Makoto    
===================  Log for username MVX:74 access ==================
-day 
25-JUL-1988 19:41:05 JST 
     
-who 
 1  SRA   
 2  OPER  
 3  OPER  
 4  PERPOPER   
 5  PERPOPER   
 6  PJ    
 7  OPER  
 8  ITOH  
 9  CUD10 
10  TOKPRINT   
11  OGURA +    
12  OHTA  
13  NISADM
14  OPER  
15  MFUDO 
16* OGURA +    
     
-gfd mvx  
     
-MAG 
25-JUL-1988  19:41:18    
     
=:SCHEMA AIS   
     
User: OGURA        in MVX
     
     
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
Illegal Memory Reference at 312000554413
     
?HALT at user PC 555000  
     
-MAG 
25-JUL-1988  19:41:32    
     
=:SCHEMA AIS   
     
User: OGURA        in MVX
     
     
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
     
?FATAL ERROR NUMBER = 0004    
     
?HALT at user PC 510110  
     
-DAY 
25-JUL-1988 19:41:45 JST 
     
-LOG 
65.55 tru 
terminal time: 0:00:51   
================== End of the Log ====================================
Received: from D34.Tymnet by D35.Tymnet; Tue, 26 Jul 88 15:33:43 PDT
Return-path: <JMS@D34.Tymnet> 
From: JMS@D34.Tymnet 
Date: Tue, 26 Jul 88 17:28:31 CDT 
To: OPER, SRA, MEYERG, JENGLISH, BILLF, LELANDY, OSMAN, CARL, JMS, ZONE,
	SCRIBNER, RODDAMJ, TYMRES5, PKRUMV@F33, FSC.R/DONAHUE@ONTYME, DIAG10D 
Subject: "The TYM file for node 4200 has"... 

The TYM file for node 4200 has some discrepencies which may have caused
NW4200.BND to fail.  It has ZITEL=0, NDP=128, and CONSAT=0 whereas all the
other EBUS bases have ZITEL=1, NDP=256, and CONSAT=1.  ZITEL=0 is valid
only for MAC I with core memory, not MAC II with semiconductor memory.

The new TYM file has been telecopied to (EBUS) and the new BND file has
been stored in (TYMNET:25) and (TYMNET:33)NW4200.BND.

Please try this file on the next scheduled reload of D34.    /Joe
Received: from F29.Tymnet by D35.Tymnet; Thu, 28 Jul 88 11:00:19 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 28 Jul 88 11:13:38 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: :(BETATEST)MEMO.962 

(begin forwarded message)

Received: from D35.Tymnet by F29.Tymnet; Thu, 21 Jul 88 14:15:15 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 21 Jul 88 14:01:04 PDT
Return-path: <NTD.BASELINE@EMSTXS.Ontyme.Tymnet> 
From: NTD.BASELINE@EMSTXS.Ontyme.Tymnet 
Date: 20 JUL 88 16:42:44 
To: TXS.J/SMITH@EMSTXS.Ontyme.Tymnet 
Message-id: I06360@Ontyme.Tymnet 
Subject: :(BETATEST)MEMO.962 
PatchFileReleaseforEBUS: 

     
     
     
        ================================================================   
         ******    SOURCE    ***     SOURCE    ***    SOURCE     ******    
        ================================================================   
           ***** CODE  PATCH  *** CODE  PATCH  *** CODE  PATCH  ***** 
        ================================================================   
     
     
        Date:20-Jul-88      16:35  
     
        To:   NTD DEVELOPMENT STAFF, TYMNET  
     
        From: Sheldon Cohen,  Baseline Software Distribution
     
        Subj: Patch File Release for  EBUS   
     
        File access this Memo: (BETATEST)MEMO.962 
     
        Product Name:    E-BUS BASE FOR PDP-10    
        Code Version:   02.00        DATE:07/20/88
        Release Document:     (BASELINE)EBUS02.X00
        Release Description:  (BASELINE) NONE
        Modified Files:       (SOURCE)  
                                        EBUS02.O00   JOXRUW 
                                        EBUS02.W00   WEBSIR 
     
        Test Site Location(s): X32, all PDP-10 systems in Fremont Data Center   
     
        ================================================================   
        Summary of Changes:   
     
        Engine code now drives front panel.  
        Engine code now contains Operation Monitor.    
        68K code has been completely rewritten!   
        There now exists an ERS and an IMS for this product.
.    
        This release (19-Jul-88) consists of patches to the 68K object code
        only.  All changes are stored in EBUS02.O00 ('oh', 'zero', 'zero').
        The new checksum for EBUS02.O00 is JOXRUW.
.    
        Fix problem with restarting after a Dispatcher crash.    
        The symptom was that the IPI would immediately crash when the 
        slot was restarted, causing the slot to crash again.  After   
        several repetitions, the 68010's stack would overflow and the 
        entire Engine would have to be reset.
.    
        Fix problem with the base zeroing out the PDP-10's ring pointers   
        when the base was restarted.    
.    
        Fix problem with the base crashing the PDP-10 host for no
        apparent reason 41 days and 10 hours after the node was loaded.    
.    
        ================================================================   
        This  Code is intended for general deployment. 
     
        This  code is now  available on  NTD's  system 39.   For Tymnet    
        release  date and system information,  contact  Tymnet  Quality    
        Assurance.  
     
        Testing  should  be coordinated with JOE SMITH 
           Phone:    408-922-6220  
           ONTYME:   TXS.J/SMITH   
        Please use ONTYME whenever possible. 
        ================================================================   
         ******    SOURCE    ***     SOURCE    ***    SOURCE     ******    
        ================================================================   
           ***** CODE  PATCH  *** CODE  PATCH  *** CODE  PATCH  ***** 
        ================================================================

(end forwarded message)

As of 27-Jul-88, the new (SOURCE)EBUS02.O00 is on D25, D54, and F33.
This means that if PI (Project Implementation) makes a new BND file for
any base, they will automatically get Krumviede's patches.
Received: from F29.Tymnet by D35.Tymnet; Wed, 3 Aug 88 14:11:05 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 3 Aug 88 14:08:42 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 3 Aug 88 14:00:41 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 03 AUG 88 08:09:51 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.J/SMITH@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, ENS.K/BEDELL@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A28570@Ontyme.Tymnet 
Subject: Continuing F74 problems 

SUBJ: 

           McDonnell Douglas Network Systems Company              M/S: F24
               TYMNET External Network Services
                                                  Tel. [-1] (408) 922-6473
 
DATE>      1988-08-02  15:54 PDT                  Fax. [-1] (408) 922-7030
 
TO>        Makoto Ogura (NIS.OGURA)
 
COPIES>    [see attached Copy Distribution List]
 
FROM>      Ken BeDell (ENS.K/BEDELL)
 
SUBJECT>   Continuing F74 problems
 
REFERENCE> M79463
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
     Ogura-san, received your Ontyme of today regarding F74 file problems.
     Rick Daniels is not in office today, so I cannot obtain additional
     information at this time.
 
     I am unclear as to the reliability of F74 Disk drives this week.
     Hopefully Rick and/or Mike Bellopatrick can supply you with present
     status, if this problem has continued during recent days.
 
     For your information, I've not seen message A25087 from Mike.
 
     As I'm sure you realize, we do not have personnel here to analyze and
     solve Magnum problems.  If the issues relate to hardware, IPC Ops.
     can respond accordingly; if they involve Tymcom X software other
     than Magnum, the Tymcom X Support group can analyze and solve.
 
     Accordingly, I'm requesting that Rick supply you with an update on
     the present status of F74 hardware problems, as they may be responsi-
     ble for the problems you're experiencing.
 
     Regards. Ken
 
COPY DISTRIBUTION>
 
           Mike Bellopatrick (IPC.M/BELLOPATRICK)
           Rick Daniels (IPC.R/DANIELS)
           Hajime Itoh (NIS.ITOH)
           Kuniyoshi Kamoi (NIS.KAMOI)
           Jan Krivanec (IPC.J/KRIVANEC)
           Joe M Smith (FSC.JM/SMITH)
           Kazuo Watanabe (NIS.K/WATANABE)
Received: from F29.Tymnet by D35.Tymnet; Wed, 3 Aug 88 14:11:20 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 3 Aug 88 14:08:56 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 3 Aug 88 14:01:06 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 03 AUG 88 08:11:43 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.J/SMITH@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, ENS.K/BEDELL@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A28572@Ontyme.Tymnet 
Subject: "SUBJ: SOFTWARE SUPPORT: Does anyone"... 

SUBJ: 

SOFTWARE SUPPORT:

Does anyone have suggestions.  Thanks Rick

------------------------------------------


        Network Information Service          OnTyme  NIS.TC
                                             Phone   [-81] (3) 551 6115
                                             Fax     [-81] (3) 551 6355
Date      : 88/08/02  20:50 (JST)
 
Attn      : Ken BeDell      (ENS.K/BEDELL)
 
Copies    : Rick Daniels    (IPC.R/DANIELS)
            Mike Bellopatrick       (IPC.M/BELLOPATRICK)
            Jan Krivanec    (IPC.J/KRIVANEC)
            Joe M Smith     (IPC.JM/SMITH)
            Kuni Kamoi      (NIS.KAMOI)
            Hajime Itoh     (NIS.ITOH)
            Kazuo Watanabe  (NIS.K/WATANABE)
 
From      : Makoto Ogura    (NIS.OGURA)
 
Subject   : F74 Problems (STILL WE HAVE)
            Ref A25087
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
Ken san,
 
Thank you for your support on our service on F74.
 
Right now, NIS found some file problems on F74.
Please pass this OnTyme to appropriate person.
 
Flat file problem:
 
        From Jul's 25th evening to 26th evening in JST, files  (MPPC:74)
        SHOHI.A29 and/or SHOHI.X29 were garbled. They are the element of ISAM
        and I don't know which one (or both) is garbled. 
        Yesterday, the user who is the owner of the username MPPC pointed out
        that illegal figures appeared in their reports. So, NIS checked, found
        the problem, and could recovered.
        Now, NIS is requested to explain the cause of the problem by the user,
        because he know troubles on Jul, (04th, 06th, and 21st), and is afraid
        of reliability of our host.
        
        Please check and let me know what was happened.
 
MAGNUM problem:
 
        Kazuo Watanabe got the all files of MVX:74 at 18th JUl from the backup
        files of the Fremont center, and we found the MAGNUM error at the 
        execution of programs on the (restored) MAGNUM. From 18th to 20th Jul,
        we could execute the programs without problem every day.
        I attached the execution log with this OnTyme, so please check it.
        You can find the fatal MAGNUM error at the middle of the log.
 
        So the MAGNUM is seeemed to still have a problem. Please check and 
        advise me.
 
        Also, Mike Bellopatrick mentioned in his OnTyme #A25087 about the
        missing MAGNUM file on MVX:74, so please let me know what kind of file
        is missed, and it's id if possioble.  ######.M??    .... ?
 
Again MAGNUM problem:
 
        Today, we found a magnum text file P.REP01(exec level id is MG5WI9.ME2)
        of TYMPAS (the name of the schema) on NETBILL:74 was garbled without
        explicit file modification in past 2 weeks. About the file, no warning
        is indicated. Regarding the file, it is recoverble, however we are
        afraid that there are this kind of silent garbled files which are not
        found by our check, yet.
 
All those problems have no explicit relation with the trouble on 21st Jul,
we can only supporse they were caused by it, however it is the fact that
after the trouble, we find this kind of file (especially MAGNUM file)
problems frequently. We feel something was wrong on F74. So please check it 
again and let me know the status.
 
At this moment, we find the problem on MAGNUM in MVX:74 and NETBILL:74.
And I hope no more MAGNUM id which has the problem appeared.
 
Best regards,
Makoto
===================  LOG OF THE TRIAL =====================
-OPE ALL
KAMOI
 
-
Did not set WC RC OP GD TD ST JL AC XC WA
-ED
*R (MPPC)TOK.TRN
 workspace is kata kana
129092 Chars
*W TRAN.TOK
 Existing file -OK? Y
129092 Chars
*Q
 
-ED
*R (MPPC)TOK.MST
 workspace is kata kana
44 Chars
*W TMST.TOK
 New file -OK? Y
44 Chars
*Q
 
-ED
*R (MPPC)TOK.PRO
2300 Chars
*W PROC.TOK
 New file - Ok? Y
2300 Chars
*Q
 
-ED
*R (MPPC)OSA.TRN
 workspace is kata kana
148248 Chars
*W TRAN.OSA
 New file -OK? Y
148248 Chars
*Q
 
-ED
*R (MPPC)OSA.MST
 workspace is kata kana
44 Chars
*W TMST.OSA
 New file -OK? Y
44 Chars
*Q
 
-ED
*R (MPPC)OSA.PRO
1620 Chars
*W PROC.OSA
 New file - Ok? Y
1625 Chars
*Q
 
-ED
*R (MPPC)NAG.TRN
 workspace is kata kana
23420 Chars
*W TRAN.NAG
 New file -OK? Y
23420 Chars
*Q
 
-ED
*R (MPPC)FUK.TRN
 workspace is kata kana
12596 Chars
*W TRAN.FUK
 New file -OK? Y
12596 Chars
*Q
 
-WATCH RUN
 
-DAY
1-AUG-1988 19:02:04 JST
 
-RENAME PERP.CCC,PERP.XXX
 
NO FILES SATISFYING REQUEST
[119.76   ]
 
-DELETE TRAN.ALL,TOK.*,OSA.*,NAG.*,FUK.*,BIL.TRN,TRAN.MOT,KIN.TRN
 
FILES DELETED:
TRAN    ALL
BIL     TRN
FUK     TRN
NAG     TRN
OSA     MST
OSA     PRO
OSA     TRN
TOK     PRO
TOK     MST
TOK     TRN
[3.85   ]
 
-REN SAKI.DAT,TRAN.ALL
 
[1.48   ]
 
-DELETE REP###.*
 
FILES DELETED:
REP33   T0
REP9    T0
REP23   TA
REP23   TB
REP17   TA
REP17   TB
REP51   TA
REP51   TB
[3.02   ]
 
-DELETE *.DA,*.DB
 
FILES DELETED:
TRAN    DB
TRAN    DA
TMST    DA
TMST    DB
[2.17   ]
 
-MAGNUM AIS
1-AUG-1988  19:04:48
 
User: MVX         
 
:RUN P.DELETE
LOADING P.DELETE
EXECUTION
 
 
EXECUTION COMPLETED
 
:DO P.BACKUP
Backup of PROC.REL Created.
Backup of UKETE.FIL Created.
Backup of UKETE.SHORI.FIL Created.
Backup of SHITE.FIL Created.
Backup of TEIKI.KARI.ZAN.FIL Created.
Backup of YOKINZAN.FIL Created.
Backup of GOZAN.FIL Created.
Backup of BUSHU.FIL Created.
Backup of SHOAC.FIL Created.
Backup of SHOHIN.FIL Created.
Backup of SHIYO.FIL Created.
Backup of PLAC.FIL Created.
Backup of DATANUM.FIL Created.
 
:TIME
17.95/785.84(1)
 
:RUN AISD01
LOADING AISD01
EXECUTION
 
 
** AISD01  START **
** AISD01  END   **
 
EXECUTION COMPLETED
 
:TIME
53.61/848.48(5)
 
:RUN AISD02
LOADING AISD02
EXECUTION
 
 
** AISD02  START **
 
*UKETE      TRAN.TOK        0               0 
            TRAN.OSA        0               0 
           -----------------------------------
             TOTAL          0               0 
 
*SHITE      TRAN.TOK      201   1,228,095,188 
            TRAN.OSA        0               0 
           -----------------------------------
             TOTAL        201   1,228,095,188 
 
** P.FD03.1 START ** 
 
 
?FATAL MAGNUM ERROR ** CONTACT TYMSHARE REPRESENTATIVE.
Illegal Memory Reference at 712000631045
 
?HALT at user PC 631325
[5545.64   ]
 
-TIME
0.15 TRU
5676.09 TRU
terminal time: 11:11.00
-QUIT
?QUIT?
 
-RENAME TRAN.KIN,KIN.TRN
 
NO FILES SATISFYING REQUEST
[2.41   ]
 
-RENAME TRAN.TOK,TOK.TRN
 
[1.52   ]
 
-RENAME TMST.TOK,TOK.MST
 
[1.47   ]
 
-RENAME PROC.TOK,TOK.PRO
 
[1.44   ]
 
-RENAME TRAN.OSA,OSA.TRN
 
[1.49   ]
 
-RENAME TMST.OSA,OSA.MST
 
[1.45   ]
 
-RENAME PROC.OSA,OSA.PRO
 
[1.49   ]
 
-RENAME TRAN.NAG,NAG.TRN
 
[1.44   ]
 
-RENAME TRAN.FUK,FUK.TRN
 
[1.49   ]
 
-RENAME TRAN.BIL,BIL.TRN
 
[1.45   ]
 
-MAG AIS
1-AUG-1988  19:12:34
 
User: MVX         
 
:RUN AISD04
LOADING AISD04
EXECUTION
 
 
** AISD04  START **
The last change to relation SHOHIN.FIL terminated abnormally.
Some of the last changes have possibly been lost.
The structure of the RELATION has possibly also been damaged.
Do you want MAGNUM to attempt RELATION structure recovery? TIME
Execution halted in procedure "P.SHOHIN.F"
Execution halted in procedure "AISD04"
 
:RUN P.DELETE
LOADING P.DELETE
EXECUTION
 
 
EXECUTION COMPLETED
 
:QUIT
1-AUG-1988  19:12:42
 
EXIT
[106.99   ]
 
-DIR /TOT
 
TOTAL PAGES 14231 FILES 540
[1.40   ]
 
-DAY
1-AUG-1988 19:13:11 JST
 
-LOGOUT
5800.41 tru
terminal time: 0:12:30
======================  END OF THE LOG  ======================
Received: from F29.Tymnet by D35.Tymnet; Wed, 3 Aug 88 14:11:27 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 3 Aug 88 14:09:04 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 3 Aug 88 14:01:21 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 03 AUG 88 11:28:37 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: IPC.J/KRIVANEC@Ontyme.Tymnet, ENS.K/BEDELL@Ontyme.Tymnet,
	NIS.ITOH@Ontyme.Tymnet, NIS.KAMOI@Ontyme.Tymnet,
	NIS.K/WATANABE@Ontyme.Tymnet, NIS.OGURA@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet, IPC.M/BELLOPATRICK@Ontyme.Tymnet,
	TXS.J/SMITH@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet 
Message-id: A28772@Ontyme.Tymnet 
Subject: MAGNUM F74 

                         M E M O R A N D U M





DATE>      03 AUG 88  11:27

TO>        Makoto Ogura

COPIES>    See 'CC' List

FROM>      Rick DAniels


SUBJECT>   MAGNUM F74


-----------------------------------------------------------------------


Makoto;

Read through your Ontyme  M79463 today.  I do not know that much about
MAGNUM, only enough to clear up errors that we may have on our own, which
I learned from the MAGNUM Group that use to be here.

What I did this morning is the following:

GFD MVX
[07:47:39]

MAG AIS
[07:47:48]
3-AUG-1988 7:47:49

User:  DANIELSR   in MVX

":"RUN AISD04
LOADING AISDO4
EXECUTION

** AISD04  START **
                                                                Page  2

The last change to relation SHOHIN.FIL terminated abnormally.
Some of the last changes have possible been lost.
The structure of the RELATION has possible also been damaged.
Do you want MAGNUM to attempt RELATION structure recovery?  I answered Y here.
Structure recovery is complete.
The RELATION structure is internally consistent, however,
Data loss may have occurred.  Execution proceeds.

The last change to relation GOZAN.FIL terminated abnormally.

(The same recovery procedures reported here and I did the same as above)

The last change to relation BUSHU.FIL terminated abnormally.

(The same recovery procedures reported here and I did the same as above)

** AISD04  END **

EXECUTION COMPLETED

":"Q
3-AUG-1988  7:49:55

EXIT

MAG AIS
[07:50:42]
3-AUG-1988   7:50:43

User:  DANIELSR     in MVX

":"RUN AISD04
LOADING AISD04
EXECUTION

** AISDO4  START **
** AISD04  END   **

EXECUTION COMPLETED

":"Q
3-AUG-1988   7:53:35

EXIT


As menioned before, there may still be some files garbled on System 74,
but we have no way of knowing which ones until the USERS lets  us know.

The files can be restored if the Users know which ones they are from the
lastest all-files and backups.  Even after doing this, the User may have to
recreate some files.

Fremont Data Center needs to schedule this system down for the next three
Fridays in order to check-out/replace some HDA's  It is imperative that
system 74 be scheduled down this coming Friday to start this work.

Thanks Rick Daniels
                                                                Page  3


P> S> Please change the ID of FSC.JM/SMITH to TXS.J/SMITH
Received: from F33.Tymnet by D35.Tymnet; Wed, 3 Aug 88 20:08:46 PDT
Return-path: <Carl@F33.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Wed, 3 Aug 88 19:56:43 PDT 
To: Monitor Group <CARL>, Osman Guven <Osman>, Joe Smith <JMS>, Craig Fletcher
	<fletcherc> 
Subject: F74 SAT.SYS rebuild 

This afternoon, I had to rebuild the RIB to SYS:SAT.SYS while the system
was still up.  They had a SATRED error for a sat-page on logical unit 4.
The page was legitimately bad, could not read/write it.  I suggested they
PAKCPY it out as soon as possible, since DSKCLN would get upset trying to
re-write the SATs the next DSKCLN.

While checking, I looked at the rib, and gee-golly-oh-my it was garbage!
I was going to clear HRE on SAT.SYS initially, but hmmm...  So, I told
Rick what was going on and began the repair.  I rebuilt it using the RIB
from system F33 (also 365x's formatted in pages) as the template.
  Details:  Used PEAK to create FOO.DDT showing all page pointers then
            ^Y (read command file in DDT) in TSTSUP to do most of the
            typing in... 18 units x 13 page pointers ...

Anyway, all said and done, the system crashed for SAT2ND on a different
page on the same logical unit 4, before I could clear HRE from the status
bits in [1,4]UFD entry for SAT.SYS.  DSKCLN found my reconstructed RIB
to specification and the system came up.  I then fixed the UFD entry.

Again, if this tool we talked about last week existed, the reconstruction
would not have been necessary, a simple page copy would have been enough!

/Carl
Received: from F29.Tymnet by D35.Tymnet; Thu, 4 Aug 88 14:10:17 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 4 Aug 88 14:09:11 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 4 Aug 88 14:00:51 PDT
Return-path: <NIS.OGURA@EMSTXS.Ontyme.Tymnet> 
From: NIS.OGURA@EMSTXS.Ontyme.Tymnet 
Date: 04 AUG 88 06:13:38 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: IPC.R/DANIELS@Ontyme.Tymnet, ENS.K/BEDELL@Ontyme.Tymnet,
	IPC.M/BELLOPATRICK@Ontyme.Tymnet, IPC.J/KRIVANEC@Ontyme.Tymnet,
	ENS.L/HEINZEL@Ontyme.Tymnet, TXS.J/SMITH@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	NIS.HQ@Ontyme.Tymnet, NIS.KYODA@Ontyme.Tymnet, NIS.KAMOI@Ontyme.Tymnet,
	NIS.T/TAKAHASHI@Ontyme.Tymnet, NIS.ITOH@Ontyme.Tymnet,
	NIS.K/WATANABE@Ontyme.Tymnet, NIS.OGURA@Ontyme.Tymnet 
Message-id: M80017@Ontyme.Tymnet 
Subject: F74 Problems MORE SERIOUSLY 
F74ProblemsMORESERIOUSLY: 

     
     
        Network Information Service          OnTyme  NIS.TC 
                                             Phone   [-81] (3) 551 6115    
                                             Fax     [-81] (3) 551 6355    
Date      : 88/08/04  22:04 (JST)  
     
Attn      : Rick Daniels    (IPC.R/DANIELS)  
            Ken BeDell      (ENS.K/BEDELL)   
     
Copies    : See  Distribution 
     
From      : Makoto Ogura    (NIS.OGURA) 
     
Subject   : F74 Problems MORE SERIOUSLY 
            Ref A25087 , I12225 , A28772 , I12281 
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
Rick and Ken san,   
     
Thank you for your investigation about the problem.    
However, the problem is growing more and more seriously for NIS. 
     
Today we found other problems on F74 regarding the MAGNUM files. 
     
1. MFUDO  
     
        Another garbled MAGNUM relation file of a customer was found this  
        morning. The relation is PLAC.FIL of MFSAIS(schema) on MFUDO:74.   
        The MFSAIS sesmed to be OK past one week, however last night monthly    
        reporting process on MFSAIS was executed and the garbling was found.    
        The file was heavy damaged and was very hard to recover. 
     
        Also we found some strange matter that the relation had 2 instances
        for one key value!!  For long time, we believe the key is an unique
        value insuch kind of file, but today we knew it was not fit the    
        actual filing system. 
        I've copied the all in the schema into username NISADM:74.    
        So please check it by yourself as follows.
     
                -GFD NISADM   
                -MAGNUM MFSAIS
                :DO FILE OGURA.CHK 
     
        Then you can get 2 instances which have the same key value.   
     
        Anyway Tadashi Takahashi is working to recoverthe MFUDO's system,  
        but he expects it is hard to recover whole data on the D/B and we  
        may be requested some compensation from the customer for the lost  
        of the data.
     
2. MPPC   
     
        A procedure of schema AIS on MPPC:74 had been broken sometime and  
        the last night's overnight execution was aborted by it.  
        So this morning, we, the customer and NIS, had to check the system 
        and made recovery shoot.   
        About the matter, we were complained by the customer because the delay  
        of reporting with checking and recovery was very critical for their
        monthly reporting processing which was done just this period of the
        month. 
        And, for the past few month, this user faced host problem at their 
        monthly processing every month. 
     
3. MAGNUM Command problem
     
        Some MAGNUM commands like  
                CRE .....
                BACKUP ....   
        are not enable, and (after we made arrangement to get the logging for   
        it) after re-login we find the commands work without problem. 
     
        About the matter, we have no evidence, however we experienced the  
        same problem few days ago. 
     
4. MVX    
     
        Schema AIS of MVX, as I mentioned many times in my OnTyme, was prepared 
        for some important project of NIS. But, as I mentioned too, it was 
        heavily garbled. So Kazuo Watanabe let it to go back at 18th Jul and    
        tried to execute 18th, 19th, and 20th operation on the schema.
        However it was failed by MAGNUM FATAL ERROR.   
        Andtoday, he tried to recompile schema and found also the MAGNUM FATAL  
        ERROR. 
     
5. Internal Billing 
     
        Our internal billing system is also damaged. (Maybe recoverble)    
     
     
From the observation of F74and MAGNUM past 2 weeks, we feel they are very  
un-stable. And the check for the machine, as Rick mentioned, is needed for 
the current and future service.    
However, please advise us what you can do for the those garbled files and  
systems.  
     
We feel and afraid that we find more non-awared garbling for the coming two
weeks, because the almost of monthly processing of the users are going to be    
executed for that period.
     
Best regards,  
Makoto    
     
     
     
     
     
Distribution   
============   
     
     
     
Mike Bellopatrick       (IPC.M/BELLOPATRICK) 
Jan Krivanec    (IPC.J/KRIVANEC)   
Lisa Heinzel    (ENS.L/HEINZEL)    
Joe Smith       (TXS.J/SMITH) 
Carl Baltrunas  (TXS.C/BALTRUNAS)  
Osman Guven     (TXS.O/GUVEN) 
Kei Suzuki      (NIS.HQ) 
Ivan Kyoda      (NIS.KYODA)   
Kuni Kamoi      (NIS.KAMOI)   
Tadashi Takahashi       (NIS.T/TAKAHASHI)    
Hajime Itoh     (NIS.ITOH)    
Kazuo Watanabe  (NIS.K/WATANABE)
Received: from F29.Tymnet by D35.Tymnet; Fri, 5 Aug 88 14:07:34 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 5 Aug 88 14:06:25 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 5 Aug 88 14:00:45 PDT
Return-path: <ENS.K/BEDELL@EMSTXS.Ontyme.Tymnet> 
From: ENS.K/BEDELL@EMSTXS.Ontyme.Tymnet 
Date: 04 AUG 88 17:31:57 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: NIS.OGURA@Ontyme.Tymnet, ENS.K/BEDELL@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, IPC.M/BELLOPATRICK@Ontyme.Tymnet,
	IPC.R/DANIELS@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	ENS.L/HEINZEL@Ontyme.Tymnet, NIS.ITOH@Ontyme.Tymnet,
	NIS.KAMOI@Ontyme.Tymnet, NIS.KYODA@Ontyme.Tymnet,
	IPC.J/KRIVANEC@Ontyme.Tymnet, TXS.J/SMITH@Ontyme.Tymnet,
	NIS.SUZUKI@Ontyme.Tymnet, NIS.T/TAKAHASHI@Ontyme.Tymnet,
	NIS.K/WATANABE@Ontyme.Tymnet 
Message-id: I12713@Ontyme.Tymnet 
Subject: Serious F74 problems 

           McDonnell Douglas Network Systems Company              M/S: F24
               TYMNET External Network Services
                                                  Tel. [-1] (408) 922-6473
 
DATE>      1988-08-04  17:21 PDT                  Fax. [-1] (408) 922-7030
 
TO>        Makoto Ogura (NIS.OGURA)
 
COPIES>    [see attached Copy Distribution List]
 
FROM>      Ken BeDell (ENS.K/BEDELL)
 
SUBJECT>   Serious F74 problems
 
REFERENCE> M80017
 
 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
 
     Ogura-san, received your latest memo regarding F74 Magnum problems.
 
     Although we all appreciate and sympathize with the problems you are
     experiencing with Magnum internal file corruption, we can do little
     or nothing to solve the problems associated with Magnum code or
     files.
 
     As I have mentioned in past Ontymes, we have no remaining expertise
     with Magnum here at our facility, or anywhere in the U.S. for that
     matter.
 
     It is my feeling that NIS has the Magnum knowledge necessary to ana-
     lyze and correct the problems better than anyone else (i.e. re. MFUDO
     relation which reflects 2 instances for one key value: I assume it
     will be necessary to dump the relation to a 'flat' file and make nec-
     essary adjustments).
 
     However, we also realize the problems you are reporting are due to
     hardware disk failures on host F74.  Operations and the maintenance
     staff are doing everything possible to remedy these problems.
 
     I strongly suggest you keep close records of all problems, for future
     credit request requirements, and keep me posted on these problems if
     you wish.
 
 
 

 
Serious F74 problems                                                Page 2
 
 
     I'm not trying to discourage your communication to us on such issues,
     but wish to emphasize that we can do very little to assist you in
     resolving Magnum file corruption problems.
 
     We're all hopeful the hardware problems will be resolved soon.
 
     Best regards. Ken
 
 
 
 
COPY DISTRIBUTION>
 
           Carl Baltrunas (TXS.C/BALTRUNAS)
           Mike Bellopatrick (IPC.M/BELLOPATRICK)
           Rick Daniels (IPC.R/DANIELS)
           Osman Guven (TXS.O/GUVEN)
           Lisa Heinzel (ENS.L/HEINZEL)
           Hajime Itoh (NIS.ITOH)
           Kuniyoshi Kamoi (NIS.KAMOI)
           Ichiro Kyoda (NIS.KYODA)
           Jan Krivanec (IPC.J/KRIVANEC)
           Joe Smith (TXS.J/SMITH)
           Keisuke Suzuki (NIS.SUZUKI)
           Tadashi Takahashi (NIS.T/TAKAHASHI)
           Kazuo Watanabe (NIS.K/WATANABE)
Received: from F29.Tymnet by D35.Tymnet; Fri, 5 Aug 88 14:07:42 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 5 Aug 88 14:06:31 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 5 Aug 88 14:01:03 PDT
Return-path: <NIS.OGURA@EMSTXS.Ontyme.Tymnet> 
From: NIS.OGURA@EMSTXS.Ontyme.Tymnet 
Date: 05 AUG 88 05:34:28 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (18 names) 
Message-id: M80263@Ontyme.Tymnet 
Subject: F74 Problems MORE SERIOUSLY 
F74ProblemsMORESERIOUSLY: 

SUBJ: F74 Problems MORE SERIOUSLY   

     
     
        Network Information Service          OnTyme  NIS.TC 
                                             Phone   [-81] (3) 551 6115    
                                             Fax     [-81] (3) 551 6355    
Date      : 88/08/05  21:32 (JST)  
     
Attn      : Rick Daniels    (IPC.R/DANIELS)  
            Ken BeDell      (ENS.K/BEDELL)   
     
Copies    : See  Distribution 
     
From      : Makoto Ogura    (NIS.OGURA) 
     
Subject   : F74 Problems MORE SERIOUSLY 
            Ref A25087 , I12225 , A28772 , I12281 , I12713  
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
Ken san,  
     
Thank you for your OnTyme.    
     
Today we could get more information about the current MAGNUM atatus.  
     
MAGNUM commands as  
     
        COPY TEXT .... TO FILE ....       and
        BACKUP ......  TO ......   
     
are not available on some schemas with error message "User Directory Access
Error", and available on other schemas. It depends on the directory which the   
schema is located. And if the destinaton file is not-existing (i.e. new) file,  
the problem is not appeared.  
The  problem is reproducable on the specific schema as 
     
                ONTYME.DB on DISTRICT239:74, 
                MMCAIS    on MMETAL:74, and etc.  
     
We guess the matter points is a part of MAGNUM copy mechanism which copies 
a MAGNUM file to a exec file as new file. And also, we found , on the same 
schema, a mechanism reading record from MAGNUM file and writing it onto exec    
(new) file works without problem, because    
     
        LIST FROM ..... TO ..... and    
        MAGNUM report writing 
     
has no problem.
     
I tried the COPY and BACKUP commands after the schema recompiling, however 
the problem still be appeared.
     
     
As you understand, this problem is very serious for NIS and reliability of 
MDC's RCS service in Japan.   
     
     
After host moving to F74 last autumn, we experienced heavy host problems every  
half a year, the cash memory trouble for over two months at the begin of this   
year and this disk un-reliability problem from Jun.    
Then our customers are going to feel something critical to operate their   
system, especially the financial system, on our service.    
     
I wonder that someone in IPC mentioned his MAGNUM on F74 had no problem, and    
hope he has no hidden problem as we are having.   
     
We know, you have no team for MAGNUM at this moment, however Mr. Matsumori,
the president of NIS, may not agree to pay the MAGNUM fee for such unstable
MAGNUM service and support, but may indicate some compensation from MDC for NIS.
     
At this moment, we can't evaluate the current MAGNUM on F74 run normally or not,
and only thing we need it to offer stable MAGNUM service to the customers. 
The simple example of the stable service is that the MAGNUM command, which 
I mentioned above,should work normally in anytime in any schema. 
     
Can we continue the MAGNUM service on F74 or should we change the service host  
to another host(X) ?
     
Please advise us.   
     
Best regards,  
Makoto    
     
     
     
     
     
Distribution   
============   
     
     
     
Ron Bamberg     (TYMPLN.HQ)   
Mike Bellopatrick       (IPC.M/BELLOPATRICK) 
Jan Krivanec    (IPC.J/KRIVANEC)   
Lisa Heinzel    (ENS.L/HEINZEL)    
Joe Smith       (TXS.J/SMITH) 
Carl Baltrunas  (TXS.C/BALTRUNAS)  
Osman Guven     (TXS.O/GUVEN) 
Mike Tanabe     (NIS.HQ) 
Kei Suzuki      (NIS.HQ) 
Ivan Kyoda      (NIS.KYODA)   
Kuni Kamoi      (NIS.KAMOI)   
Shuichi Inoue   (NIS.OSAKA)   
Tadashi Takahashi       (NIS.T/TAKAHASHI)    
Hajime Itoh     (NIS.ITOH)    
Kazuo Watanabe  (NIS.K/WATANABE)
Received: from F29.Tymnet by D35.Tymnet; Mon, 8 Aug 88 14:06:24 PDT
Received: from D35.Tymnet by F29.Tymnet; Mon, 8 Aug 88 14:05:57 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Mon, 8 Aug 88 14:01:03 PDT
Return-path: <NIS.OGURA@EMSTXS.Ontyme.Tymnet> 
From: NIS.OGURA@EMSTXS.Ontyme.Tymnet 
Date: 08 AUG 88 03:50:28 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (18 names) 
Message-id: M80513@Ontyme.Tymnet 
Subject: F74 Problems MORE SERIOUSLY 
F74ProblemsMORESERIOUSLY: 

SUBJ: F74 Problems MORE SERIOUSLY   

     
     
        Network Information Service          OnTyme  NIS.TC 
                                             Phone   [-81] (3) 551 6115    
                                             Fax     [-81] (3) 551 6355    
Date      : 88/08/08  19:47 (JST)  
     
Attn      : Ken BeDell      (ENS.K/BEDELL)   
     
Copies    : See  Distribution 
     
From      : Makoto Ogura    (NIS.OGURA) 
     
Subject   : F74 Problems MORE SERIOUSLY 
            Ref A25087 , I12225 , A28772 , I12281 , I12713  
     
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
     
Ken san,  
     
Over Saturday and Sunday, we checked major relations and procedures of
customers on F74 by recompiling the procedures and accessing all instances 
for the relations. And we found several problem, however they were recovered.   
     
MFUDO's Problem
     
  However, about MFUDO's schema, by this moment, NIS can't complete to recover  
  it's destroyed data even though Tadashi Takahashi tried to do for last three  
  days.   
     
Problem   
     
  Right now, we still have the system reliability problem of MAGNUM.  
     
        1. Kazuo Watanabe recreated (NOT only recompile schema) the schema on   
           MVX:74, and found some program on the schema run normally sometime,  
           and run abnormally sometime without explicit changing for the program
           and execution environment.   
     
        2. And, I'm not sure this is related with the current problem, one of   
           person reported that temp files which was created by MAGNUM are still
           remained when he logged off all usernames related the directory (at  
           Saturday ecening) then logged in again (at Monday morning).
     
  I'm sorry I can not indicate the evidence for both case, but I feel something 
  unstable about F74 and MAGNUM.   
     
The Current Status  
     
  Again at this moment we can't evaluate what is incorrect. 
     
        - The current MAGNUM processer ?
        - Individual files on the MAGNUM schema ? 
        - Something others ?  
     
  Please set up the MAGNUM processer in normal status, or please point out 
  what is wrong if the cause is being outside of the MAGNUM.
     
     
Counter Plan   
     
  To avoid future problem, I'd like to discuss following matters.
     
        1. Moving host from F74 to other host.    
           Because almost of the current problems are believed to come from
           the disk problem. If we can get another host, the MAGNUM on that
           host works without problem. (Of cource, the support for MAGNUM has   
           still problem.)    
     
        2. Special BACKUPs for NIS's customer
           We supporse the backup operation at the center is taken when our
           our customers are operating their files by the time difference  
           between Japan and USA. So the restore operation from the backups
           is very complicated. Please arrange Following special backup for
           NIS.
     
                - NIS indicates file-list file containing username and file
                  ids.   
                - NIS indicates username-list file containing usernames.   
                - Center execute backup for all files indicated in the file-    
                  list file at 06:00 JST (Start 06:00 JST, and end the
                  operation within two hours, if possible.) every day through   
                  Monday to Friday/Saturday in JST.    
                - Center execute backup for all files under the usernames  
                  indicating in the username-list file at 06:00 of Sunday JST.  
                - All data should be remained for one month. (i.e. it's need    
                  31 Mag Tapes for this operation.)    
                - Backup information (backup starting time, backup ending time, 
                  file-ids, file creation date & time, size, etc.) for every    
                  operation is informed to NIS as on-line file just after the   
                  operation.  
           By above mechanism, we can easly follow the problem speedy.
           If this is feasible, we will discuss on details and charges.    
     
  How do you think about the matters ?  
     
Charges   
     
  Regarding the problem, we user a huge amount of TRUs by checking and
  recovering as compiling, programing, test run, and etc. And NIS will intend   
  to negotiate about the charges and costs after everything is over.  
     
     
Best regards,  
Makoto    
     
     
     
     
     
Distribution   
============   
     
     
     
Ron Bamberg     (TYMPLN.HQ)   
Rick Daniels    (IPC.R/DANIELS)    
Mike Bellopatrick       (IPC.M/BELLOPATRICK) 
Jan Krivanec    (IPC.J/KRIVANEC)   
Lisa Heinzel    (ENS.L/HEINZEL)    
Joe Smith       (TXS.J/SMITH) 
Carl Baltrunas  (TXS.C/BALTRUNAS)  
Osman Guven     (TXS.O/GUVEN) 
Mike Tanabe     (NIS.HQ) 
Kei Suzuki      (NIS.HQ) 
Ivan Kyoda      (NIS.KYODA)   
Kuni Kamoi      (NIS.KAMOI)   
Shuichi Inoue   (NIS.OSAKA)   
Tadashi Takahashi       (NIS.T/TAKAHASHI)    
Hajime Itoh     (NIS.ITOH)    
Kazuo Watanabe  (NIS.K/WATANABE)
Received: from F29.Tymnet by D35.Tymnet; Mon, 8 Aug 88 14:07:19 PDT
Received: from D35.Tymnet by F29.Tymnet; Mon, 8 Aug 88 14:06:55 PDT
Received: from EMSFSC.Ontyme.Tymnet by D35.Tymnet; Mon, 8 Aug 88 14:04:50 PDT
Return-path: <FSC.K/OKEY@EMSFSC.Ontyme.Tymnet> 
From: FSC.K/OKEY@EMSFSC.Ontyme.Tymnet 
Date: 08 AUG 88 09:21:55 
To: FSC.O/GUVEN@EMSFSC.Ontyme.Tymnet 
Message-id: P81564@Ontyme.Tymnet 
Subject: STANDBY 
StandbyforAugust1988: 

              MCDONNELL DOUGLAS FIELD SERVICE COMPANY
 
 
                            MEMORANDUM
 
 
 
TO:           Distribution

FROM:         Craig Fletcher

DATE:         29 July 1988

SUBJECT:      Standby for August 1988

_________________________________________________________________

Here is the Software Standby Schedule for August:

   Week Beginning:   8/01/88            C. Baltrunas
                     8/08/88            J. Smith
                     8/15/88            O. Guven
                     8/22/88            J. Smith
                     8/29/88            C. Baltrunas

Regards, Craig

cc: C. Baltrunas, O. Guven, J. Smith, J. Logsdon, R. Rawlins,
Received: from F29.Tymnet by D35.Tymnet; Wed, 10 Aug 88 14:07:20 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 10 Aug 88 14:06:12 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 10 Aug 88 14:00:31 PDT
Return-path: <ACS.L/METOUR@EMSTXS.Ontyme.Tymnet> 
From: ACS.L/METOUR@EMSTXS.Ontyme.Tymnet 
Date: 10 AUG 88 19:37:37 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A31720@Ontyme.Tymnet 
Subject: ONTYME BEGINNING TRAINING - - - > BLITZ #1 

                        MCDONNELL DOUGLAS
                   INFORMATION SYSTEMS COMPANY
                   ***************************
 
                 Applied Communications Systems 
 
                       M E M O R A N D U M
 
                                                 []  MDISC
DATE>      09 AUG 88  12:08

TO>        Beginning OnTyme Users

COPIES>    Nancy Browning
           Robert Grosso
           Steven Eng
           Ace Gagliardi

FROM>      Les Metour
           MDISC Implementation


SUBJECT>   Beginning OnTyme Training & ISGMail Overview


-----------------------------------------------------------------------


As part of the project to bring OnTyme to MDISC, training is
being delivered across the country to key offices.  If you
are in a smaller office, you probably did not see a flyer
announcing the class.  (Or perhaps you're so busy you haven't
read the bulletin board!)  Either way, below you'll find our
first training schedule.

If you are interested in participating, please register by 
phoning the appropriate contact as soon as possible - as class
size is limited.  Each session lasts 3 1/2 hours, scheduled either
in the morning or afternoon.  Most are morning sessions from 8:30 to
12:00.  When you call to register, you will be informed of the
class time.

I look forward to meeting those who plan on attending.




Best,
Les





                             BECOME AN ONTYME
                                COMMAND(er)

                               at one of the

                      OnTyme Beginner's "Boot Camps"!

                    * * * * * * * * * * * * * * * * * *
                    *                                 *          
                    *          Sign up NOW,           *
                    *       As we show you HOW,       *
                    *                                 *
                    *     To sharpen your SKILLS,     *
                    *   Thru OnTyme visual DRILLS.    *
                    *                                 *
                    * * * * * * * * * * * * * * * * * *


                        PRESENTING ONTYME & ISGMAIL



    WHEN            WHERE                TO REGISTER - CONTACT   


    Thurs 8/11      Dallas, Texas        Deana Tabor          214 620-5185

    Wed   8/17      Vienna, Virginia     Tim Carter           703 356-6962

    Thur  8/18      Florham Park, NJ     Gloria McCartney     201 822-3100

    Fri   8/19      New York, NY         Crystal Simmons      212 351-5870

    Tues  8/23      St Louis, MO         User Support Center  314 233-5116
                    [Hazelwood]

    Fri   8/26      Chicago, IL          Debbie Williams      312 318-1923
                    [Rosemont]


Here are the exact addresses of each office.

Dallas :        1501 LBJ Freeway - 6th floor
Vienna :        2070 Chainbridge Rod, Suite 200  -  Training Room A
Florham Park :  25 Hanover Road  
New York :      335 Madison Avenue, 11th floor -  Training Room #2
St Louis :      325 McDonnell Blvd -  Executive Briefing Center, Room 215 
Chicago  :      6250 North River Road, Suite 7000 - 6th Floor
Received: from F29.Tymnet by D35.Tymnet; Wed, 10 Aug 88 14:07:25 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 10 Aug 88 14:06:17 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 10 Aug 88 14:00:40 PDT
Return-path: <ACS.L/METOUR@EMSTXS.Ontyme.Tymnet> 
From: ACS.L/METOUR@EMSTXS.Ontyme.Tymnet 
Date: 10 AUG 88 19:40:41 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: A31723@Ontyme.Tymnet 
Subject: ...about MSG# a31720 

 
                        MCDONNELL DOUGLAS
                   INFORMATION SYSTEMS COMPANY
                   ***************************
 
                 Applied Communications Systems 
 
                       M E M O R A N D U M
 
                                                 []  MDISC
DATE>      10 AUG 88  11:38

TO>        MDISC ONTYME ACCOUNT SUPERVISORS

COPIES>    

FROM>      LES METOUR


SUBJECT>   MSG # A31720


-----------------------------------------------------------------------


AT YOUR EARLIEST CONVENIENCE, PLEASE DISTRIBUTE MSG# A31720 TO
YOUR ** ACCTUSERS FILE, OR A SPECIAL DISTRIBUTION LIST YOU'VE
CREATED WHICH WILL ENSURE DELIVERY TO USERS WHO WOULD
BE INTERESTED IN BEGINNING ONTYME & ISGMAIL TRAINING.

MANY THANKS.

-LES
Received: from F29.Tymnet by D35.Tymnet; Wed, 10 Aug 88 14:07:29 PDT
Received: from D35.Tymnet by F29.Tymnet; Wed, 10 Aug 88 14:06:21 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Wed, 10 Aug 88 14:01:08 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 09 AUG 88 14:06:20 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (35 names) 
Message-id: A31328@Ontyme.Tymnet 
Subject: Copying Out UNITS Using ASP Dump 

                         M E M O R A N D U M





DATE>      09 AUG 88  14:04

TO>        Shift Supervisors

COPIES>    Bill Fischer
           Bill Richardson
           Software Support

FROM>      Rick Daniels


SUBJECT>   Copying Out UNITS Using ASP Dump


-----------------------------------------------------------------------


As everyone is aware of, or has heard, ASP can be used to copy
out a unit, or units, on the TYMCOM-10's.

ASP is only used if PACKCOPY cannot be used, or if it fails to
copy out a unit - it is not a general procedure to use instead
of PACKCOPY.

If not done correctly, a system can be wiped out.

It is requested that Shift Supervisors get together and let
me know the best time and day of the week I can instruct Operators
of these procedures.  Please submit a joint schedule to me by
19 August.

All other interested personnel are invited.

Thanks Rick Daniels
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
Received: from F29.Tymnet by D35.Tymnet; Thu, 11 Aug 88 14:06:12 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 11 Aug 88 14:05:10 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 11 Aug 88 14:00:23 PDT
Return-path: <NETS.SUP@EMSTXS.Ontyme.Tymnet> 
From: NETS.SUP@EMSTXS.Ontyme.Tymnet 
Date: 11 AUG 88 00:20:27 
To: TXS.SUP@EMSTXS.Ontyme.Tymnet 
Message-id: I14893@Ontyme.Tymnet 
Subject: Distribution to account users 
TechPubOrders: 

                  M C D O N N E L L    D O U G L A S
                     INFORMATION SYSTEMS COMPANY
                       NETWORK SYSTEMS COMPANY
DATE>      10 AUG 88  16:15

TO>        All Employees

COPIES>    

FROM>      Susan Koehler/Documentation/


SUBJECT>   Tech Pub Orders


-----------------------------------------------------------------------


As of August 1, 1988, any inquiries regarding Technical Publications
Orders should be sent on OnTyme to NETS.TECHPUBSERVE or telephone
toll free (800) 541-8716.

Thank You

Dennis A. Hawes
Received: from F33.Tymnet by D35.Tymnet; Thu, 11 Aug 88 17:58:05 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 11 Aug 88 17:56:30 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: TYMCOM-X system configurations. 

The following can be found in the INFO tree on F33 under "DATACENTER".

$text DATACENTER;TYMCOM-X systems sorted by Data Center.
Be sure to update (INFO)SYSNO.NFO whenever (M33)CONF##.MAC changes.

  San Jose, Nicholson Lane (PST)
  SYSNO       serial# HDW  base  core disk units (used) job TTY+PTY
------------  ------- --- ------ ---- ----------------- --- -------
X14-P035/D08  KS-4257 KSa ND6760 512K  4*RM03  3/blocks  32  32+32
X17-P035/D08  KS-4267 KSb ND6657 512K  4*RP06  3/blocks  32  32+32
X930-P035     F3----5 F3a ND2770 512K  4*CDC   3/pages   32  32+32
X934-P035     F3----6 F3b ND2770 512K  4*CDC   3/pages   32  32+32

  Fremont Data Center, Solar Way (PST)
  SYSNO       serial# HDW  base  core disk units (used) job TTY+PTY
------------  ------- --- ------ ---- ----------------- --- -------
F22-P035/D-7  KL-1421 GGG ND3374 1.5M 16*3652 14/blocks 128 128+78
F26-P035/D-7  KL-1324 FFF N10350 4.0M 20*3652 16/pages  120 128+74
F29-P035/D-7  KL-1081 JJJ ND6415 2.0M 16*3675 14/blocks 128 128+78
F30-P035/D-8  KL-1388 CCC ND6721 2.0M 24*3675 20/blocks 128 128+78
X32-P035/D08  KL-1307 XXX ND2100 2.0M 48*3652  4/pages  128 128+78
F33-P035/D-7  KL-1427 BBB ND2333 2.0M 32*3652 24/pages  120 128+74
F38-P035/D-7  KL-1405 AAA ND7021 4.0M 24*3650 21/blocks 128 128+78
B39-P035/D-8  KL-1336 III BB2674 2.0M 20*3675 17/pages  128 145+78
F58-P035/D-8  KL-1332 EEE ND6720 2.0M 16*3675 14/blocks 128 128+78
F74-P035/D-8  KL-1354 DDD ND6722 4.0M 24*3650 18/pages  128 128+78

  Dallas, Texas (CST)
  SYSNO       serial# HDW  base  core disk units (used) job TTY+PTY
------------  ------- --- ------ ---- ----------------- --- -------
D23-P035/D    KL-1275  OO ND6716 2.0M 16*3675 12/blocks 128 128+78
D25-P035/D    KL-1460  GG ND7165 2.0M 16*3652 12/blocks 128 128+78
D31-P035/D    KL-1415  YY ND7004 2.0M 16*3652 14/pages  128 128+78
D34-P035/D    KL-1286  ZZ ND4200 2.0M 16*3652 12/blocks 128 128+78
D35-P035/D    KL-1096  QQ ND2107 2.0M 16*3675 10/blocks 128 128+78
D37-P035/D    KL-1461  HH ND3115 4.0M 20*3652 16/blocks 120 128+74
D54-P035/D    KL-1376  II ND4274 4.0M 16*3652 12/blocks 128 128+78
D55-P035/D    KL-1386  CC ND5577 4.0M 16*3652 12/blocks 128 128+78
D56-P035/D    KL-1383  JJ ND4725 2.0M 20*3675 16/blocks 128 128+78
 65           KL-1380  LL ND2332 2.0M (spare KL - not in use)

  Saint Cloud, France (Western Europe Time)     10 systems (9*KS,0*KL)
  SYSNO       serial# HDW  base  core disk units (used) job TTY+PTY
------------  ------- --- ------ ---- ----------------- --- -------
S59-P034/N    KS-4097 -59 ND3024 512K  4*RP06  1/blocks  30  64+14  2-14-85
 60-P034/N    KS-4097 -60   3427              no longer timesharing
S83-P034/N    KS-4097 -83 ND2234 512K  4*RP06  1/blocks  30  64+14  2-14-85
 90-P034/K-12 KL-1305 -90   3000              no longer timesharing
 92-P034/N-4  KS-4097 -92   3025              no longer timesharing
 170-P034/N-4 KS-4097 -S1   3761              no longer timesharing
 184-P034/K-7 KS-4097 -S2   2235              host number has been re-assigned
S264-P034/N   KS-4097 -S3 ND3475 512K  4*RP06  1/blocks  30  64+14  05-4-85
 301-P034/N   KS-4097 -S4   2712              no longer timesharing
S443-P034/K-8 KS-4097 -S5 ND3321 512K  4*RP06  1/blocks  30  64+14  7-13-83


Other customers                               3 systems (4*KS)
  SYSNO       serial# HDW  base  core disk units (used) job TTY+PTY
------------  ------- --- ------ ---- ----------------- --- -------

   TRW, Anaheim CA (access to TRWNET via gateway 633)
TW33-P034/P-10 KS-4097 TW ND0514 512K  4*RP06  3/blocks  30  64+14  12-2-81

   Fayez-Sarofim & Co., Houston TX
H370-P034/J-5 KS-4097  FZ ND4272 512K  4*RP06  2/blocks  30 100+4  10-29-81

   Ranier Bank, Seattle WA
W1051-P034J-6 KS-4097  RB ND5274 512K  4*RP06  3/blocks  30  64+4   3-23-81

   Mallenkrodt Chemical Co., St. Louis MO
 169-P034/N   KS-      MA                     no longer timeshareing
Received: from F29.Tymnet by D35.Tymnet; Fri, 12 Aug 88 14:09:47 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 12 Aug 88 14:09:15 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 12 Aug 88 14:00:49 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 12 AUG 88 08:43:41 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.J/SMITH@Ontyme.Tymnet, TXS.O/GUVEN@Ontyme.Tymnet,
	TXS.C/BALTRUNAS@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A32793@Ontyme.Tymnet 
Subject: "Software Support: Forwarded for"... 

Software Support:

Forwarded for your information.

By the way, F30 will be converted to a PAGE monitor the
weekend of 19 August.

Thanks Rick Daniels

=========================================================

     
     
TO: GARY WALKER
     
CC: DOUG ELLENBERG  
    ED ROOP    
    ED BARENS  
    RICK DANIELS    
    LELAND YARBROUGH
    JAMES ZONE 
    JIM ENGLISH
    JOHN RODDAM
    BILL FISCHER    
    SHARON MARCOTTE 
    JENNIFER ENGLISH
     
FR: LINDA FREITAS   
    RESOURCE PLANNING AND MANAGEMENT    
     
SUBJECT:  TYMNET CODE MACHINE CONVERSION SCHEDULE 
     
-------------------------------------------------------------------------- 
     
     A proposed schedule has been derived to facilitate the plans to  
     convert the Tymnet code machines to a page monitor format.  
     The systems have been scheduled in order of need as this upgrade 
     will considerably increase the storage availability.   
     
     Please review and distribute this schedule to allow projects
     and workloads to be planned accordingly. All of the conversions  
     are scheduled during weekends, and coordination with other data  
     center will take place to insure availability of its designated  
     back-up host. A system message will be posted prior to its  
     conversion date.    
     
     If any of the systems scheduled poses a problem, please call me  
     at (415)498-2514 as soon as possible and we can make arrangements
     to reschedule. 
     
     
         SYSTEM:        CONVERSION DATE:
     
         D54            weekend of August 26 thru 28   
     
         D25            weekend of September 9 thru 11 
     
         D37            weekend of October 14 thru 16  
     
         F22            weekend of October 21 thru 23  
     
         D= Dallas Data Center
         F= Fremont Data Center
Received: from F29.Tymnet by D35.Tymnet; Fri, 12 Aug 88 14:09:52 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 12 Aug 88 14:09:23 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 12 Aug 88 14:01:01 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 12 AUG 88 10:15:51 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (18 names) 
Message-id: A32862@Ontyme.Tymnet 
Subject: PAGE monitor Conversion Schedule - Fremont Data Center 

                         M E M O R A N D U M





DATE>      12 AUG 88  10:15

TO>        See CC List

COPIES>    

FROM>      Rick Daniels


SUBJECT>   PAGE monitor Conversion Schedule - Fremont Data Center


-----------------------------------------------------------------------


The following proposal is submitted of the intended dates when the
remainder of the TYMCOM-10's are scheduled for conversion at the
Fremont Data Center.

     F58 - Weekend of 9 through 11 September

     F29 - Weekend of 11 through 13 November

     F38 - Weekend of 25 through 27 November

At the completion of this schedule all Fremont TYMCOM-10's
will be running on a PAGE vice BLOCK monitor  This will prevent
some confusion as to what type of monitor the systems are up on.
It will also cut down of remembering what PACKCOPY version to
use.  Instead of remembering FOUR version;s; only have to
remember TWO.

Please review and distribute this schedule to allow projects and
workloads to be planned accordingly.

If any of the systems poses a problem, please call me at (415)498-2595
as soon as possible and we can make arrangements to reschedule.
                                                                Page  2


Thanks


Rick Daniels
Received: from F74.Tymnet by D35.Tymnet; Tue, 16 Aug 88 11:44:43 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 16 Aug 88 11:43:36 PDT 
To: JMS, CARL, OSMAN 
Subject: "I will need a PAGE monitor fo"... 


I will need a PAGE monitor fo system 58 by 29 August.

Thanks

Rick
Received: from F29.Tymnet by D35.Tymnet; Tue, 16 Aug 88 12:14:09 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Tue, 16 Aug 88 12:10:26 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: Turbo milestones 

11-Aug-88 4:00pm  Sync line with multiple circuits brought up in Bubbnet.
12-Aug-88 4:29pm  Turbo Engine node 2155 came up using PROM based Rodney
                  and the local disk (i.e. not downloaded into RAM).

The node mentioned above was a 5-slot Turbo, in a box about the size of
a Micro Engine.  It is sitting right next to a full sized Turbo in the
Bubbnet Lab on B5.    /Joe
Received: from F29.Tymnet by D35.Tymnet; Tue, 16 Aug 88 14:04:26 PDT
Received: from D35.Tymnet by F29.Tymnet; Tue, 16 Aug 88 14:04:09 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Tue, 16 Aug 88 14:00:40 PDT
Return-path: <IPC.J/ENGLISH@EMSTXS.Ontyme.Tymnet> 
From: IPC.J/ENGLISH@EMSTXS.Ontyme.Tymnet 
Date: 15 AUG 88 17:19:15 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Message-id: A33860@Ontyme.Tymnet 
Subject: Your visit to Dallas 

DATE> 15 AUG 88
 
FROM> Jim English
 
TO>   Osman Guven
 
CC>   Ed Barens
      Leland Yarbrough
 
SUBJ> Your visit to Dallas
 
                              M E M O R A N D U M
 
-----------------------------------------------------------------------------
 
 
Osman;
    I just wanted to say Thank you for coming by here last week
and sharing your information and time with us. I know that what
you had passed on to us is greatly appreciated by the Operators
and by us Supervisors. It has opened new doors of thinking for
us all. Thanks again. It was really nice to finally get to meet
you in person after all these years of telephone contact. I have
to tell you that you didn't look anything like I imagined you, I'm
sure the same goes for you! Ha ha ...
 
Take care for now Osman, and Thanks again...
 
Jim English
Received: from F74.Tymnet by D35.Tymnet; Thu, 18 Aug 88 10:31:45 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Thu, 18 Aug 88 10:30:19 PDT 
To: JMS, CARL, OSMAN, DANIELSR 
Subject: "How come there is no on X32? With"... 


How come there is no (SYS)BACKUP.SAV on X32?

With this file not being there I cannot run (QASYS)MAPUPD
to update the DSKMAP.

Thansk Rick
Received: from F29.Tymnet by D35.Tymnet; Thu, 18 Aug 88 12:48:38 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 18 Aug 88 12:44:12 PDT 
To: <DANIELSR@F74.Tymnet> 
Cc: CARL, OSMAN, DANIELSR 
Subject: Re: "How come there is no on X32? With"... 
In-reply-to: your message of Thu, 18 Aug 88 10:30:19 PDT

At one time Carl and I were toying with the idea of eliminating BACKUP.SAV
so as not confuse things with BACKUP.SHR - but nothing has come of that.
I copied SYSTEM.SAV to BACKUP.SAV on X32's 3675 packs - I think it is already
there on the 3652 packs.

What is the status of the disks connected to X32?  Are the 3675s there for
long term or short term?

				/Joe
Received: from F74.Tymnet by D35.Tymnet; Thu, 18 Aug 88 13:10:33 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Thu, 18 Aug 88 13:09:20 PDT 
To: JMS, CARL, OSMAN, DANIELSR 
Subject: "Joe; I'll check with Bill Richardson"... 


Joe;

I'll check with Bill Richardson Tuesday (He is off today and tomorrow -
I'm off Monday) and will let you about x32 on the 3675's.

Thanks Rick
Received: from F29.Tymnet by D35.Tymnet; Thu, 18 Aug 88 14:11:00 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 18 Aug 88 14:09:09 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 18 Aug 88 14:00:44 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 18 AUG 88 13:44:22 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (32 names) 
Message-id: A35295@Ontyme.Tymnet 
Subject: "When X32 is running on the 3652's"... 

When X32 is running on the 3652's an ASP Dump will be done.
It will be up to the Operator to determine what X32 is up on.


ALL-FILES will be taken whether it is up on 3652's or 3675's.
Received: from F29.Tymnet by D35.Tymnet; Thu, 18 Aug 88 15:41:49 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 18 Aug 88 15:39:51 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: How to load the Turbo Engine file system. 

(begin forwarded message)

Received: from B39.Tymnet by F29.Tymnet; Thu, 18 Aug 88 15:38:01 PDT
Received: from tymix.Tymnet by B39.Tymnet; Thu, 18 Aug 88 15:33:56 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19443; Thu, 18 Aug 88 15:36:13 PDT
Received: by antares.Tymnet.com (3.2/SMI-3.2) id AA02733; Thu, 18 Aug 88
	14:00:21 PDT
Return-path: <antares!jms@tymix.Tymnet> 
From: antares!jms (joe smith) 
Date: Thu, 18 Aug 88 14:00:21 PDT 
To: f29!jms 
Message-id: <8808182100.AA02733@antares.Tymnet.com> 
Subject: How to load the Turbo Engine file system. 

~jms/loadsfs.doc			17-Aug-88

To set up a Turbo Engine from scratch:
  I.   Set up the GP cards and disk hardware (done in Manufacturing).
  II.  Run MAKESFS to set up the sfs root directory.
  III. Run SFS and create top-level directories.
  IV.  Copy the six mandatory files to disk.
  V.   Bring up Turbo Engine in the network, then copy remaining files.

-----------------------------------------------------------------------------

How to do it when BITS is working:

Part I will not be necessary if manufacturer sets up the hardware correctly.

Part II.  Load makesfs via BITS and run it.
  1.  Invoke BITS and enter "pl -p u makesfs.e GP.133".
  2.  Go to the TTYA port on GP.133 and enter 51849.

Part III.  Load sfs via BITS and create top-level directories
  1.  Invoke BITS and enter "pl -p f sfs.e GP.133".
  2.  Enter "fo mkdir /isis".
  3.  Enter "fo mkdir /GP.133".
  4.  Enter "fo mkdir /GP.135".

Part IV.  Load the six mandatory files using BITS.
  1.  "fs -p f sfs.e        /GP.133/load".
  2.  "fs -p i kernel.e     /GP.135/load".
  3.  "fs -t s isom.e       /isis/debugger".
  4.  "fs -t s dispatcher.e /isis/dispatcher".
  5.  "fs -t c syscon.e     /isis/syscon".
  6.  "fs -t s nodecode.e   /isis/slot00".

Part V.  Bring up Turbo Engine.
  1.  Enter "bs GP.133" and "bs GP.135" to BITS.
  2.  Copy any other files as needed after Turbo is up in the network.

-----------------------------------------------------------------------------

How to do it without BITS:

All board addresses and Unix directory names are what where used in the
demonstration given the week of August 15.  The actual names and addresses
will be different on your system.

You must be at the keyboard of a Sun workstation to do this.
  1.  Make sure GP board in slot 0 is strapped for VME arbitration master.
  2.  Verify File Server is addressed as GP.133 and ISIS is GP.135.
  3.  Both boards must have Probug ROMs, not Rodney ROMs.
  4.  Login to the workstation and start suntools.
  5.  Open 2 shell windows, and enter "skytool tb6" in one of them.
  6.  Use the other window to locate the following files:
      Sun programs: skytool, skyload, tw, skycheck, skyclock
      Probug bootables: dl.x, makesfs.x, sfs.x, filebug.x, rodney.x
      X.409 binaries: dispatcher.d, isom.d, nodecode.d, kernel.d, sfs.d
      Node specific: syscon.d, nodecode.d

Part I.  Set up the GP cards and disk hardware.
  1.  Connect a line from TTYA on the workstation to TTYA on GP.133.
  2.  (here we do somthing to run skyclock and skycheck on each GP card).
  3.  Enter "skyload -b dl.x tb6" to the shell.
  4.  Enter "j800a000" to skytool which starts dl.
  5.  Enter "in" to initialize the disk.
  6.  Enter "bl" to change block length.
  7.  Enter 3 blanks and then "1024"
  8.  Enter "y" to "make permanent" and return to "abort".
  9.  Enter "ex" to get back to Probug.

Part II.  Run makesfs to set up the sfs root directory.
  1.  Enter "skyload -b /usr/calvin3/rodney/sfs/4.4e/makesfs.x tb6" to shell.
  2.  Enter "j" to skytool.
  3.  Makesfs will type recommended disk size.  Enter this number to skytool.
  4.  It will take about 45 minutes for this to complete.

Part III. Run sfs and create top-level directories.
  1.  Enter "skyload -b /usr/calvin3/rodney/sfs/4.4e/sfs.x tb6" to shell.
  2.  Enter "skyload -b /usr/opus/diag/bergeron/rodney.x tb6" to shell.
  3.  Enter "m 8016970" to skytool (to patch Rodney to not look at disk).
  4.  Enter "1200" right after where "1A00" is displayed.
  5.  Enter "j" to skytool.  Rodney will output messages and start sfs.
  6.  Enter "m /isis" to the "cmd:" prompt (to make /isis direcory).
  7.  Enter "m /GP.133" and "m /GP.135".
  8.  Enter "f /" to list files.  The 3 directories should be there.
  9.  Enter "c /isis/syscon" to create an empty file (due to bug in "tw").
 10.  Enter "c /isis/debugger", "c /isis/dispatcher", "c /isis/slot00".
 11.  Enter "c /GP.133/load" and "c /GP.135/load".

Part IV.  Copy the six mandatory files to disk.
  1.  Disconnect cable from GP.133 and plug into TTYA on GP.135.
  2.  Enter "skyload -b /usr/calvin3/rodney/sfs/4.4e/filebug.x tb6" to shell.
  3.  Enter "skyload -b /usr/opus/diag/bergeron/rodney.x tb6" to shell.
  4.  Enter "m 8016970" to skytool (to patch Rodney to not look at disk).
  5.  Enter "1200" right after where "1A00" is displayed.
  6.  Enter "j" to skytool.  Rodney will output messages and start filebug.
  7.  Enter "tw /isis/syscon <syscon.d" to shell.  Note: tw is very slow.
  8.  Enter "tw /isis/slot00 <nodecode.d". 
  9.  Enter "tw /isis/dispatcher <isis68k/dispatcher/1.0/bin/dispatcher.d".
 10.  Enter "tw /isis/debugger <isis68k/isom/1.0/bin/isom.d".
 11.  Enter "tw /GP.135/load <isis68k/kernel/1.01/bin/kernel.d".
 12.  Enter "tw /GP.133/load <rodney/sfs/4.4e/sfs.d".

Part V.   Bring up Turbo Engine in the network, then copy remaining files.
  1.  Power card cage off, wait 30 seconds, then back on again.
  2.  Once the node has come up in the network, BITS can be used for all
      further transfers.


(end forwarded message)

I creaThe above is a summary I created after the demonstration given by
Ron Saltgaver, Pat Driscoll, and Pat Bergeron.    /Joe
Received: from F33.Tymnet by D35.Tymnet; Thu, 18 Aug 88 20:04:10 PDT
Return-path: <Carl@F33.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Thu, 18 Aug 88 19:59:18 PDT 
To: Joe Smith <JMS>, Osman Guven <Osman> 
Subject: Tymnet access points 

The following has been added to (M33:33)PHONES


Tymnet (Anaheim)                        714/370-1200    Dial-up
Tymnet (Fremont, CA)                    415/490-7366    Dial-up

Tymnet (San Jose, CA) 1200              408/432-3430    Vadic
Tymnet (San Jose, CA) 1200 unpublished  408/943-8623    Vadic, node 4067
Tymnet (San Jose, CA) 1200 unpublished  408/943-8626-8  Vadic, node 4067
Tymnet (San Jose, CA) 1200 unpublished  408/943-8642-5  Vadic, node 4067
Tymnet (San Jose, CA) 1200              800/235-1719    Vadic, node 7205
Tymnet (San Jose, CA) 2400              408/432-8618    Concord
Tymnet (San Jose, CA) 2400              800/289-6638    Concord, nd 3455
Tymnet (San Jose, CA) 2400 unpublished  408/434-9021    Concord, nd 10561
       others, 9402, 90xx = 30 34 42-4 52 63-4 71 79 86 88-9 98

Tymnet (San Francisco, CA) inside CA    800/222-0555    Vadic dail-up
       nodes 3035, 3707, 7512, 7513
Tymnet (San Francisco, CA) outside CA   800/268-9323    Vadic, node 6625
Tymnet (Columbus, OH) 1200              800/826-7784    Vadic, node 7421

I will fill in the node numbers for 432-3430 and 432-8618 after I check them
out from home.

Note:  ALL the San Jose nodes are at Nicholson Lane!  So, in the event of
       a power outage such as last night, all of SJ is out of luck!

       Of course, there's always the 800 numbers for SF and Columbus!

/Carl
Received: from F29.Tymnet by D35.Tymnet; Fri, 19 Aug 88 23:39:28 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 19 Aug 88 23:35:05 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: John Adam's FSC picnic 27-Aug. 

All the people at Nicholson Lane are having a company picnic a week from 
tomorrow, on August 27th.  We're supposed to tell Karin how many wives, kids,
etc so she can tell them.  It will be at the "Odd Fellow's" Lodge, I think
in Saratoga.  /Joe
Received: from F74.Tymnet by D35.Tymnet; Tue, 23 Aug 88 12:52:04 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 23 Aug 88 12:48:56 PDT 
To: JMS, OSMAN, CARL, DANIELSR 
Subject: "When taking an ASP dump of units"... 


When taking an ASP dump of units running on 3652"s (if PACKCOPY
does not work) does each units need to be dump separately  to a set
of tapes, or after dumping one unitcan you continue on the same
tape with the next?

Thanks Rick
Received: from F33.Tymnet by D35.Tymnet; Tue, 23 Aug 88 14:29:48 PDT
Return-path: <DANIELSR@F33.Tymnet> 
From: DANIELSR@F33.Tymnet 
Date: Tue, 23 Aug 88 14:28:02 PDT 
To: OSMAN, JMS, CARL, DANIELSR 
Subject: "Osman; Have you had a chance to"... 


Osman;

Have you had a chance to delete the following from
the BDA list on system 33:

[21476,420212].ufd(jburrough)

Thanks Rick
From: OPER@D35.Tymnet 
Date: Tue, 23 Aug 88 20:56:32 CDT 
To: OSMAN 
Subject: "ATTN OSMAN: BATCH REQUEST NUMBER"... 

ATTN OSMAN:
BATCH REQUEST NUMBER 34978 IS COMPLETED
WITH 176 FILES TRANFERED TO 35 ON USERNAME REQTYM
THANK YOU FROM DSMITHD/DALLAS OPERATIONS
Received: from F33.Tymnet by D35.Tymnet; Wed, 24 Aug 88 15:20:31 PDT
Return-path: <FLETCHERC@F33.Tymnet> 
From: FLETCHERC@F33.Tymnet 
Date: Wed, 24 Aug 88 15:18:50 PDT 
To: OSMAN 
Subject: "OSMAN: CAN YOU GET ME A LIST WITH"... 

OSMAN:
CAN YOU GET ME A LIST WITH THE CORRESPONDENCE
BETWEEN >PDP-10 SOFTWARE ID'S, E.G. XX0033, OR WHATEVER
AND THE SYSTEM SERIAL NUMBER FOR ME TO FEED TO DISPATCH?

	CRIAG
Received: from F29.Tymnet by D35.Tymnet; Thu, 25 Aug 88 10:37:56 PDT
Return-path: <JMS@F29.Tymnet> 
From: JMS@F29.Tymnet 
Date: Thu, 25 Aug 88 10:31:03 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME, LELANDY@F29 
Subject: "F29's AR.ARX crash on 23-Aug was"... 

F29's AR.ARX crash on 23-Aug was caused by an ARX parity error in user mode,
probably bit 1.  I can't say for sure because the word of user core was not
saved in the crash dump.  The contents of (7:1) is reported as being
200000,,000000.  Assuming that the word was supposed to be 0, the bad
parity would come from bit 1 while doing an <IDPB 5,2> where AC 5 has
40 (an ASCII blank) and AC 2 has 350700,,561343.  Note that the latter
matches the page fail word (500) of 777000,,561343 as reported in the
crash mail.					/Joe
Received: from F33.Tymnet by D35.Tymnet; Thu, 25 Aug 88 12:57:49 PDT
Return-path: <LIVERSON@F33.Tymnet> 
From: LIVERSON@F33.Tymnet 
Date: Thu, 25 Aug 88 12:57:06 PDT 
To: OSMAN 
Subject: "I WOULD APPRECIATE YOU INCREASING"... 

I WOULD APPRECIATE YOU INCREASING MY INACTIVITY TIMER TO 4 HOURS.  I JUMP
FROM SYSTEM TO SYSTEM AS A PART OF MY JOB AND END UP LOGGING ONTO THE
VARIOUS SYSTEMS A COUPLE DOZEN TIMES/DAY.  THIS IS NOT ONLY INCONVENIENT
BUT VERY TIME CONSUMING AND MAKES MY JOB ALL THAT MUCH HARDER.  OTHERS
IN MY GROUP HAVE THE SAME PROBLEM.  THEY INCLUDE GGILLIAM, MZORTMAN,
CKOCIOLEK, RFISHER, PDIXON, AND MKLEIN.  I AM SURE THEY WOULD ALSO APPRECIATE
AN INCREASE IN THEIR INACTIVITY TIMERS AS WELL.
 
THANX
LARRY R. IVERSON
LIVERSON
Received: from F29.Tymnet by D35.Tymnet; Thu, 25 Aug 88 14:10:34 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 25 Aug 88 14:05:46 PDT 
To: <LIVERSON@F33.Tymnet> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC 
Subject: Re: "I WOULD APPRECIATE YOU INCREASING"... 
In-reply-to: your message of Thu, 25 Aug 88 12:56:11 PDT

The inactivity timer is currently a system-wide parameter.  It is set
by the operator, and affects all users that have inactivity timeout
enabled.  Your request for an individual timeout will be brought up
at the next Monitor Development meeting.   /Joe
Received: from D25.Tymnet by D35.Tymnet; Thu, 25 Aug 88 15:27:12 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 25 Aug 88 17:19:15 CDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, Paul Krumviede <PKRUMV@33> 
Subject: AUXMOX overloading of F33 to end in a couple of days. 

I talked to Phillipe Michel in Project Implementation about the many jobs
running AUXMOX.  The "522 Deployment Project" is modifying the TYM and
CMD files of all nodes in the Public Net to use Node Code version 5.22.
The BND files are being telecopied from D25/D54 to F33 as they are
created, because many nodes are reloaded the same night as when their
code is generated.  The people in PI did not know that you could enter
more than one file specification at a time when running (SPL)COPY.
Therefore each node was transfered as a seperate TELECO job.

Phillipe said they should be done in a few days.     /Joe
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
Received: from F29.Tymnet by D35.Tymnet; Fri, 26 Aug 88 14:08:55 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 26 Aug 88 14:07:33 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 26 Aug 88 14:00:37 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 26 AUG 88 08:37:29 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: TXS.J/SMITH@Ontyme.Tymnet, TXS.C/BALTRUNAS@Ontyme.Tymnet,
	TXS.O/GUVEN@Ontyme.Tymnet, IPC.R/DANIELS@Ontyme.Tymnet 
Message-id: A38531@Ontyme.Tymnet 
Subject: "Joe; In reference to your Ontyme"... 

Joe;

In reference to your Ontyme A38154; TYM files for all EBUS bases updated.

The following BND files are not on systems as indicated:

NOT ON D54 (This System will be down this weekend for PAGE conversion)

(TYMNET)ND3374.BND  and (TYMNET)ND2333.BND


NOT ON D25


(TYM5)ND6415.BND
(TYM5)ND6720.BND
(TYM5)ND6721.BND
(TYM5)ND7021.BND
(TYM5)N10350.BND
(TYM5)ND6722.BND

Thanks Rick
Received: from F29.Tymnet by D35.Tymnet; Fri, 26 Aug 88 14:08:58 PDT
Received: from D35.Tymnet by F29.Tymnet; Fri, 26 Aug 88 14:07:37 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Fri, 26 Aug 88 14:00:50 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 26 AUG 88 11:05:05 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (35 names) 
Message-id: A38650@Ontyme.Tymnet 
Subject: "SUBJ: M E M O R A N D U M DATE>"... 

SUBJ: 





                         M E M O R A N D U M





DATE>      26 AUG 88  11:03

TO>        All PDP-10 Operators
           Shift Supervisors

COPIES>    Joe Smith
           Carl Baltrunas
           Osman Guven
           Sue Pal
           Bill Fischer

FROM>      Rick Daniels


SUBJECT>   Updated EBUS Node Code


-----------------------------------------------------------------------


Starting this weekend, all PDP-10's will be scheduled to have their
EBUS reloaded.

Those scheduled will be on the SPECIAL EVENTS form.  Please check
it to be sure of what HOST to load the code from.  The code is not
on all the systems that are usually used.
Received: from F29.Tymnet by D35.Tymnet; Fri, 26 Aug 88 16:37:15 PDT
Received: from B39.Tymnet by F29.Tymnet; Fri, 26 Aug 88 16:37:05 PDT
Received: from tymix.Tymnet by B39.Tymnet; Fri, 26 Aug 88 16:33:16 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03561; Fri, 26 Aug 88 16:36:13 PDT
Received: by antares.Tymnet.com (3.2/SMI-3.2) id AA01669; Fri, 26 Aug 88
	16:27:55 PDT
Return-path: <antares!osman@tymix.Tymnet> 
From: antares!osman (osman guven) 
Date: Fri, 26 Aug 88 16:27:55 PDT 
To: osman@f29 
Message-id: <8808262327.AA01669@antares.Tymnet.com> 
Subject: This is send to "osman@f29". 

Joe says this will be sent to tymix, then B39, then F29, then D35.
Received: from F33.Tymnet by D35.Tymnet; Fri, 26 Aug 88 22:22:41 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 26 Aug 88 22:21:19 PDT 
To: Rick Daniels <OPERA.SUP@EMSTXS.Ontyme>, Ed Barens <IPC.E/Barens@Ontyme>,
	Jim English <IPC.J/English@Ontyme>, Leland Yarbrough
	<IPC.L/Yarbrough@Ontyme>, Jim Zone <IPC.J/Zone@Ontyme>, John Roddam
	<IPC.J/Roddam@Ontyme> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, Paul Krumviede <PKRUMV@33> 
Subject: List of PDP-10 EBUS-base BND files 

SYS    Primary load file   checksum   Secondary   Tertiary      as of 26-Aug-88
---  ---------------------  ------  ------------  --------
D23    (TYM5:54)ND6716.BND  DIQJAS  (TYMNET:33)   (EBUS:25)
D25    (TYM5:54)ND7165.BND  NATGUC  (TYMNET:33)   (EBUS:54)
D31    (TYM5:54)ND7004.BND  YIBQEL  (TYMNET:33)   (EBUS:25)
D34  (TYMNET:25)ND4200.BND  JAJVIT  (TYMNET:33)   (EBUS:54)
D35  (TYMNET:25)ND2107.BND  FEYPOX  (TYMNET:33)   (EBUS:54)
D37  (TYMNET:25)ND3115.BND  RONYAF  (TYMNET:33)   (EBUS:54)
D54  (TYMNET:25)ND4274.BND  SAZKAH  (TYMNET:33)   (EBUS:25)
D55    (TYM5:54)ND5577.BND  XEDWEV  (TYMNET:33)   (EBUS:25)
D56  (TYMNET:25)ND4725.BND  ZOMQUP  (TYMNET:33)   (EBUS:54)
D63- (TYMNET:25)ND4613.BND  ROFSER  base not in use, node number still assigned
D65- (TYMNET:25)ND2332.BND  YEBZIZ  base not in use, node number still assigned
F22  (TYMNET:25)ND3374.BND  YIZPUV  (TYMNET:33)   (EBUS:54)
F26    (TYM5:54)N10350.BND  SOXRAS  (TYMNET:33)   (EBUS:25)
F29    (TYM5:54)ND6415.BND  QUNMED  (TYMNET:33)   (EBUS:25)
F30    (TYM5:54)ND6721.BND  XEQMIX  (TYMNET:33)   (EBUS:25)
F32  (TYMNET:25)ND2100.BND  ZAVXUB  (TYMNET:33)   (EBUS:54)
F33  (TYMNET:25)ND2333.BND  XARBOW  (TYMNET:54)*  (EBUS:54) other secondary
F36-   (TYM5:54)ND6714.BND  HAVHEB  base not in use, node number still assigned
F38    (TYM5:54)ND7021.BND  RIKSUK  (TYMNET:33)   (EBUS:25)
F39    <<ELF>>  BB2674.BND  HIGKUQ  (BUBBNET:33)  (EBUS:33) can't load from 39
F58    (TYM5:54)ND6720.BND  NEWBAK  (TYMNET:33)   (EBUS:25)
F74    (TYM5:54)ND6722.BND  RUPMEB  (TYMNET:33)   (EBUS:25)

The primary directory for nodes 2000 to 4777 is (TYMNET:25).
The primary directory for nodes 5000 and above is (TYM5:54).
The secondary directories are (TYMNET:33) and (TYM5:33).

The only exception for EBUS node code is ND2333.BND which is duplicated in
(TYMNET:54) so that F33's base can be reloaded even if D25 is down.
The tertiary load files are to be used only if the primary and secondary
systems are not available.  Note that B39 is loaded from an ELF system.

[End of (EBUS:33)EBUS.SUM]                              <Joe Smith>
Received: from F58.Tymnet by D35.Tymnet; Tue, 30 Aug 88 11:56:49 PDT
Return-path: <OPER@F58.Tymnet> 
From: OPER@F58.Tymnet 
Date: Tue, 30 Aug 88 11:56:07 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME, LELANDY@F29 
Subject: "F58/EEE APR: 1332 OTHER CRASH"... 

F58/EEE APR:  1332 -- OTHER - CRASH
DATE:  08/29/88   DOWN:  23:59   ANS:  09:38
LOADED P035/D-7 FROM DISK.  OPERATOR FORCED FULL DSKCLN AT 09:28.
MEMORY ONLINE: 2048K   BUSS MODE:  4   INTERLEAVE
CRASH FILE:  NONE

   UNIT BPA14(580010) HUNG, WOULD'T COMEUP ON LINE.
   FSC CHECKED BOTH PACK AND DR AND FOUND HEAD CRASH.
   SYSTEM REBUILD WITH ASP TAPES WAS COMPLETED 
   AT 07:40. BUT UNABLE TO BRING SYSTEM. WHILE TRYING
   TO DO FULL DSKCLN RECEIVED AN ERROR MSG
   BPA0:58000=B0 MISSING OR INCONSISTENT PACK SET ID
   FOR UNIT BPA1, ETC...ETC. FOR ALL DRIVES. CONTACTED
   SOFTWARE SUPPORT, AND SUPPORT WALKED OPERATIONS
   THROUGH RESETTING (MISSING OR INCONSISTENT PACK SET ID)
   ERROS. FULL DSKCLN STARTED AT 09:28 AND ANSERED AT 09:38.
   $

FREMONT OPERATIONS...AMB
Received: from F74.Tymnet by D35.Tymnet; Tue, 30 Aug 88 13:38:01 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Tue, 30 Aug 88 13:34:47 PDT 
To: OSMAN, DANIELSR, SRA 
Subject: "SRA has all their mail forwarded"... 


SRA has all their mail forwarded to SRA:29, but when they log into
SRA:29 they do not received the statement [You have mail waiting.
To read it, type "MAIL".]

When they type in "R RDMAIL", it reports 'no mail'.  They have to 
READ all there mail by running "R TUMS" and read each one separately.

Any suggestions?  Thanks Rick
From: OSMAN@D35.Tymnet 
Date: Tue, 30 Aug 88 13:47:03 PDT 
To: osman 
Subject: "mail 1"... 

mail 1
From: OSMAN@D35.Tymnet 
Date: Tue, 30 Aug 88 13:47:14 PDT 
To: osman 
Subject: "mail 2"... 

mail 2
From: OSMAN@D35.Tymnet 
Date: Tue, 30 Aug 88 13:49:49 PDT 
To: osman 
Subject: "mail 3"... 

mail 3
From: OSMAN@D35.Tymnet 
Date: Tue, 30 Aug 88 13:50:01 PDT 
To: osman 
Subject: "mail 4"... 

mail 4
From: Osman Guven <OSMAN@D35> 
Date: Tue, 30 Aug 88 13:56:38 PDT 
To: osman@antares 
Subject: testing 

this is being sent to osman@antares
From: Osman Guven <OSMAN@D35> 
Date: Tue, 30 Aug 88 13:57:14 PDT 
To: antares!osman@tymix 
Subject: test2 

this is being sent to antares!osman@tymix
Received: from F29.Tymnet by D35.Tymnet; Tue, 30 Aug 88 14:06:14 PDT
Received: from D35.Tymnet by F29.Tymnet; Tue, 30 Aug 88 14:05:20 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Tue, 30 Aug 88 14:00:48 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 30 AUG 88 11:49:45 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (34 names) 
Message-id: A39821@Ontyme.Tymnet 
Subject: "SUBJ: M E M O R A N D U M DATE>"... 

SUBJ: 





                         M E M O R A N D U M





DATE>      30 AUG 88  11:47

TO>        All TYMCOM-10 Operators
           Shift Supervisors

COPIES>    Bill Fischer
           Bill Richardson
           Joe Smith
           Carl Baltrunas
           Osman Guven

FROM>      Rick Daniels


SUBJECT>   Hardware Read Errors/Missing or Inconsistent Pack ID's


-----------------------------------------------------------------------


If one, or both of the following ERROR prints out when bringing
up a TYMCOM 10 - STOP!!!!!!!!!!!!!!!!!!!!!!!!!!!


     DO NOT ATTEMPT TO DO ANYTHING ELSE
     ----------------------------------


     "BPA, BPB, BPC ## FIRST HOM PAGE HARDWARE READ ERROR"
     "BPA, BPB, BPC ## FIRST BAT PAGE HARDWARE READ ERROR"

                            OR

     "MISSING OR INCONSISTENT PACK SET IF FOR UNIT BPA/BPB/BPC ##"

At this point, notify your SHIFT SUPERVISOR, who will notify the
TYMCOM-10 Coordinator.
                                                                Page  2


An updated memorandum will be forthcoming on this subject as
soon as I can get with Software Support.

Thanks 

Rick
Received: from F38.Tymnet by D35.Tymnet; Wed, 31 Aug 88 12:51:59 PDT
Return-path: <JMS@F38.Tymnet> 
From: JMS@F38.Tymnet 
Date: Wed, 31 Aug 88 12:47:08 PDT 
To: OPER, BILLF, CARL, JMS, DANIELSR, OSMAN, MICHAELB, SRA, RICHARDSON,
	PKRUMV@F33, FSC.R/DONAHUE@ONTYME, LELANDY@F29 
Subject: "I am currently running ASH on"... 

I am currently running ASH on F30 and F58 to locate potential bad spots.
The select light on the disk will be on continuously - this is normal.
From: Carl A Baltrunas <Carl@F35.Tymnet> 
Date: Thu, 1 Sep 88 2:41:57 PDT 
To: Joe Smith <JMS>, Osman Guven <Osman> 
Subject: MOD comments? 

My rough draft of the MOD for performance analysis/monitoring is
in (CARL:35,33,29)PERF.MOD.  I would greatly appreciate any cand
all comments.  I need a fresh look before I'm done, but I think
I haven't left anything important to us out.

I'll also run it by Lloyd before I finish it...

/Carl
Received: from F74.Tymnet by D35.Tymnet; Thu, 1 Sep 88 8:43:06 PDT
Return-path: <DANIELSR@F74.Tymnet> 
From: DANIELSR@F74.Tymnet 
Date: Thu, 1 Sep 88 8:41:40 PDT 
To: OSMAN 
Subject: "Have a User who wants 4 million"... 


Have a User who wants 4 million bytes of storage on 74.  How is this
converted to pages?  Is there a formula I can use?

Thanks Rick
Received: from D56.Tymnet by D35.Tymnet; Thu, 1 Sep 88 10:54:08 PDT
Return-path: <OPER@D56.Tymnet> 
From: OPER@D56.Tymnet 
Date: Thu, 1 Sep 88 12:46:10 CDT 
To: OPER, SRA, MEYERG, JENGLISH, LELANDY, OSMAN, CARL, JMS, SCRIBNER, RODDAMJ,
	TYMRES5, PKRUMV@F33, FSC.R/DONAHUE@ONTYME, DIAG10D, ZONE, DANIELSR 
Subject: "D56-JJ/NODE-4725/APR-1383/DOWN"... 


D56-JJ/NODE-4725/APR-1383/DOWN AT 12:24 (CDT) ON09/01/88
WITH ERROR CODE-STOPCODE PAGWK2/MAINTENANCE NOTIFIED.
G.MEYERS CHECKED SYSTEM/BROUGHT UP ON P035/D-7 MON WITH
2048K MEM ON LINE/FST DSKCLN AT 12:35 (CDT)
CRASH SAVED TO CRA001/SYSTEM WAS BACK IN THE NETWORK AT
12:39 (CDT)               THANK YOU ....DCO...AA
Received: from F29.Tymnet by D35.Tymnet; Thu, 1 Sep 88 14:05:02 PDT
Received: from D35.Tymnet by F29.Tymnet; Thu, 1 Sep 88 14:04:25 PDT
Received: from EMSTXS.Ontyme.Tymnet by D35.Tymnet; Thu, 1 Sep 88 14:00:48 PDT
Return-path: <IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTXS.Ontyme.Tymnet 
Date: 01 SEP 88 13:06:10 
To: TXS.O/GUVEN@EMSTXS.Ontyme.Tymnet 
Cc: (33 names) 
Message-id: A41036@Ontyme.Tymnet 
Subject: "To test out a theory that I discussed"... 

To test out a theory that I discussed with Software Support, NO DSKCLN
will be run before an ASP dump on any TYMCOM-10's.

A FAST DSKCLN will be run at all times after the ASP dump.

Thanks for your cooperation with this.

Rick Daniels
Received: from F26.Tymnet by D35.Tymnet; Thu, 1 Sep 88 15:02:44 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 1 Sep 88 15:09:37 PDT 
To: Bill Richardson <FSC.B/Richardson@Ontyme> 
Cc: osman 
Subject: Interval timer on 1307 

Looking at the CTYLOG on F26 shows that CPU 1307's interval timer is gaining
47 minutes a day; 2 minutes an hour.  Whenever that system is unshut, or a
new SUP takes over, the number of minutes of adjustment = number of hours sinc
last adjustment times 2.  I assume that adjusting the CPU's interval timer
is relatively easy and can be done when F26 goes back to hardware FFF.
Received: from F33.Tymnet by D35.Tymnet; Thu, 1 Sep 88 15:31:57 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 1 Sep 88 15:09:41 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC 
Subject: Why character I/O is slow on F26 today. 

F26 is running on XXX KL#1307 base 2100 instead of FFF KL#1324 base 10350.
The two 9600 baud lines on node 2100 are the bottleneck for loading private
net nodes (node 10350 has 2 memory-shuffler lines).    /Joe
Received: from F29.Tymnet by D35.Tymnet; Thu, 1 Sep 88 20:32:41 PDT
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 1 Sep 88 20:31:18 PDT 
To: osman 
Subject: On call Saturday. 

Sally and I are going to Ren-Faire on Sunday.  I can be on call Saturday.
I may take that time to clean up my desk at work.  /Joe
y@rš