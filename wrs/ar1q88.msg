Received: from tymix.Tymnet by B39.Tymnet; Fri, 1 Jan 88 20:52:22 PST
Received: by tymix.Tymnet (5.51/4.7) id AA08149; Fri, 1 Jan 88 21:31:43 PST
Received: by birdix.Tymnet.com (3.2/SMI-3.2) id AA04279; Fri, 1 Jan 88 20:43:08
	PST
Return-path: <birdix!wrs@tymix.Tymnet> 
From: birdix!wrs (Bill Soley) 
Date: Fri, 1 Jan 88 20:43:08 PST 
To: Ontyme!NTD.NOVAGRATSKY, B39!WRS 
Message-id: <8801020443.AA04279@birdix.Tymnet.com> 
Subject: ISIS 8.0 

I tried 8.0 again and this time the node code would not even come up.  I think
it might be some interaction between the node code and ISIS.  The node is
2521 and all the files are in (BUBBNET)BB2521.* on system 39.  Please feel
free to take a look at the configuration to see if I am doing anything wrong.
I can make arrangements for a dump if you would like, although I've never
done a whole-machine dump before so a brief tutorial would be helpful.  I would
guess that I just hit boot and use the LOADII dump function?  -Bill

It is a micro-Engine with no front panel.
Received: from F29.Tymnet by B39.Tymnet; Sun, 3 Jan 88 1:02:39 PST
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Sun, 3 Jan 88 9:01:11 UT 
To: MAIL 
Subject: Req #21186 DSK:(MAIL)RDIST.CTL Abort-Time (1) 

; Processed:  Req #21186 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  03-Jan-88 00:00:00
; Actual run: 03-Jan-88 00:00:02
;
; Options:  Log:Supersede Mail:Error Unique:Job
; Limits:  Time 60  TRUs 0
; Flags: 
; Status:  Abort-Time (1)  at  03-Jan-88 01:01:10
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from D65.Tymnet by B39.Tymnet; Sun, 3 Jan 88 18:55:34 PST
Return-path: <wlaforge@d65.tymnet> 
From: William LaForge <wlaforge@d65.tymnet> 
Date: Sun, 3 Jan 88 21:53:44 EST 
To: Mail Wizard <MAIL> 
Subject: TUMS-10 bug report (v45000000 of 23-MAY-87) 

I have received the same 11 messages about 4 times now.
Bill
Received: from D31.Tymnet by B39.Tymnet; Mon, 4 Jan 88 2:08:47 PST
Return-path: <jlindback@d31> 
From: jlindback@d31 
Date: Mon, 4 Jan 88 10:49:55 
To: William R. Soley <WRS@B39> 
Subject: Problems with TUMS on system 26. 

I have problems with TUMS on system 26. It's impossilble to get in to
it, the only response I get from it is "VERSION MISMATCH".
I have renamed all the files to see if there was anything in the INI or
TIX files that it didn't like, but with no result.

Could you pls take a look at this.

Rgds

Joacim
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 4 Jan 88 3:56:22 PST 
To: <wlaforge@d65.tymnet> 
Subject: Re: TUMS-10 bug report (v45000000 of 23-MAY-87) 
In-reply-to: your message of Sun, 3 Jan 88 21:53:44 EST

I'm not sure what the problem was.  It had to do with one of the messages
causing an ILL MEM REF in TUMS when it was being read.  When TUMS dies like
that it is not certain weather the messages were successfully transfered to
your private mail file, so just to be safe, they are kept around and resent
to you next time the mailer runs.  It seems to be okay now.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 4 Jan 88 3:58:32 PST 
To: <jlindback@d31> 
Subject: Re: Problems with TUMS on system 26. 
In-reply-to: your message of Mon, 4 Jan 88 10:49:55

The copy of TUMS seems to be bad on that system.  I'll send over another copy.
Thanks for letting me know.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Mon, 4 Jan 88 6:57:51 PST
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Mon, 4 Jan 88 14:56:26 UT 
To: MAIL 
Subject: Req #21186 DSK:(MAIL)RDIST.CTL Abort-Time (1) 

; Processed:  Req #21186 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  04-Jan-88 05:55:13
; Actual run: 04-Jan-88 05:55:16
;
; Options:  Log:Supersede Mail:Error Unique:Job
; Limits:  Time 60  TRUs 0
; Flags: 
; Status:  Abort-Time (1)  at  04-Jan-88 06:56:25
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from tymix.Tymnet by B39.Tymnet; Mon, 4 Jan 88 8:10:45 PST
Received: by tymix.Tymnet (5.51/4.7) id AA15629; Mon, 4 Jan 88 08:46:32 PST
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA23705; Mon, 4 Jan 88 08:46:33
	PST
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Mon, 4 Jan 88 08:46:33 PST 
To: WRS@B39.Tymnet.com 
Message-id: <8801041646.AA23705@calvin.Tymnet.com> 
Subject: Re: Bus Error Code 

I've no problem with handling latecomers in a second stage.  It is more
important to finish off the MMU code so that Thomas can complete
his testing and we can release a version with full MMU functionality.

Pat
From: LDIAS@B39.Tymnet 
Date: Mon, 4 Jan 88 10:13:39 PST 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: TUMS-10 bug report (v45000003 of 07-DEC-87) 
In-reply-to: your message of Thu, 31 Dec 87 17:00:41 PST

THANK YOU
LARRY
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 4 Jan 88 13:28:10 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: <QSATS.D/ELLENBERG@EMSTYM.Ontyme.Tymnet>, TYM.SECURITY@ONTYME.TYMNET 
Subject: Re: CMF Support Task Force - First Meeting 
In-reply-to: your message of Fri, 11 Dec 87 8:08:41 PST

A late response, but I missed this point the first time I read the message.
Re: your concern about making te password hash algorithm available to private
network customers via NETVAL and SUP source code.  Since the algorithm is
a one-way-cipher, there is very low risk of the customer (or anyone) being
able to reverse it.  The exposure there is very minor.  On the other hand,
the exposure of having the MUD data base is very significant.  As an example
of this, UNIX systems make known both the HASH algorithm and the password file
which contains the hashed passwords.  The UNIX algorithm is slightly more
secure than ours for reasons I'll explain upon request, but the difference is
not enough to cause concern over our agorithm being "public knowledge".  

The biggest risk, in my opinion, is the proliferation of "simple" passwords.
Despite a resolution by the security action committee several years ago,
endorsed by Warren Prince, we still have over 30 thousand "simple" passwords
in the MUD.  Elimination of these would eliminate the risk of "dictionary
atacks" which can be very fruitful particularly with the present policy of
giving an error message for a bad user name before asking for a password.

-Bill
Received: from F29.Tymnet by B39.Tymnet; Tue, 5 Jan 88 9:40:18 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 5 Jan 88 9:34:14 PST
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 05 JAN 88 09:13:53 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I31252@Ontyme.Tymnet 
Subject: "T Y M N E T MCDONNELL DOUGLAS"... 


            _________________________________________
             
                           T Y M N E T 
 
            MCDONNELL DOUGLAS NETWORK SYSTEMS COMPANY
 
                  M  E  M  O  R  A  N  D  U  M
            _________________________________________
 
 
 
 
DATE:          January 4, 1987
 
TO:            ALL EMPLOYEES
 
FROM:          Warren Prince
 
 
================================================================
 
 
I am pleased to announce the appointment of James W.  Patty,  Jr. 
as  Vice  President  of  Marketing and Field Operations reporting 
to me.
 
Clint  DeGabrielle  will  report  to Jim  as  Vice  President  of 
Marketing.
 
Jim  brings  to  Tymnet  a wealth of  experience  in  sales  and
marketing  of  computer  systems  and  telecommunications.    He 
previously  was  Vice President of Sales and Marketing for  MCI's 
Pacific Division.
 
Jim  graduated from the University of Virginia with a  degree  in 
Electrical  Engineering.   He  began  his  career  with IBM  as a
systems engineer in Washington D.C.  and progressively moved into 
sales  and marketing management positions.   He was  promoted  to 
Manager of Customer Executive Programs in Poughkeepsie, New York.
 
When  IBM formed Satellite Business Systems in 1977,  Jim  became 
Director  of  Marketing  and subsequently was  promoted  to  Vice 
President Marketing and Service Delivery.  
 
With the merger of SBS into MCI in 1986, Jim assumed the position 
of Vice President of Marketing Services and later was promoted to 
Vice President of Sales and Marketing for MCI's Pacific Division.
 
We welcome Jim to Tymnet.
Received: from F29.Tymnet by B39.Tymnet; Tue, 5 Jan 88 9:40:23 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 5 Jan 88 9:34:43 PST
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 05 JAN 88 09:23:39 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I31263@Ontyme.Tymnet 
Subject: "T Y M N E T MCDONNELL DOUGLAS"... 

 
 
 
                           T Y M N E T
 
            MCDONNELL DOUGLAS NETWORK SYSTEMS COMPANY
 
                  M  E  M  O  R  A  N  D  U  M 
 
 
 
 
 
DATE:          January 4, 1988
 
TO:            NETWORK SYSTEMS COMPANY EMPLOYEES
 
FROM:          Warren Prince
 
SUBJECT:       NSC'S SBI EXECUTIVE
 
=================================================================
 
Bob  Tomasi,  Vice  President  of Operations,  has  accepted  the 
assignment  as the Network Systems Company executive responsible 
for  our SBI efforts.
 
Although,  we have already had some successes in the SBI Program, 
there is a great deal more to do.  The MDNSC Executive Management 
team  has received the final team report which was  chartered  to 
develop  a  program to effectively involve every employee in  the 
SBI  process.   The  program  draws  on  elements  of  successful 
programs  from  companies  such  as  IBM,  Hewlett  Packard,  and 
Northern  Telecom  and adapts them to our  needs.   You  will  be 
hearing more about our SBI Program over the next few months.
 
Please  give  Bob  your  support and feel free to  send  him  any 
suggestions or ideas regarding the SBI process at mail stop C52.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 5 Jan 88 14:39:00 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: CMF Support Task Force - First Meeting 
In-reply-to: your message of Tue, 5 Jan 88 8:13:42 PST

Sorry for delay - I'll go decipher that file right now.  Let me know what
the USPS username being used was and I'll tell you if its on the list of
"trivial" passwords.  More in a bit.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Tue, 5 Jan 88 15:20:33 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 5 Jan 88 15:17:27 PST
Return-path: <NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 05 JAN 88 15:09:03 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I31614@Ontyme.Tymnet 
Subject: Skywalker Weekly Mtg. 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                                  MCDONNELL DOUGLAS
                              INFORMATION SYSTEMS GROUP
                               INTEROFFICE MEMORANDUM
 
          Network Systems Company            Network Technology Development
          Tymnet                                           No:CSS880105.001
                                                Date:January 5, 1988  15:06
 
          SUBJECT>       Weekly Skywalker Meeting
 
          TO>            Dick Rawson              BJC
                         Dan Lasater              Bill Euske
                         John Kopf                Bill Soley
                         Pat Driscoll             Michael Stimac
                         Dave Alcorn              Dave Bliss
                         John Coons               Lana Vaysburd
 
          COPIES>        Al Fenn
 
          FROM>          Ken Holcomb
          -----------------------------------------------------------------
 
          Due to meeting conflicts with Air France, our regularly scheduled
          meeting for this week is cancelled. Our next meeting will  be 13-
          Jan-88.
 
          Thanks......Ken
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                                          1
Received: from F29.Tymnet by B39.Tymnet; Tue, 5 Jan 88 16:00:47 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 5 Jan 88 15:54:57 PST
Return-path: <NTD.G/KENNEDY@EMSNTD.Ontyme.Tymnet> 
From: NTD.G/KENNEDY@EMSNTD.Ontyme.Tymnet 
Date: 05 JAN 88 15:16:57 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I31634@Ontyme.Tymnet 
Subject: nano engine firmware release 
NanoEngineFirmwareRelease: 

 
 
                         M E M O R A N D U M
 
                                     [] T Y M N E T / MCDONNELL DOUGLAS
                                             Network Technology Development
 
 
DATE>           01/05/88
 
TO>             NTD 
 
FROM>           Greg Kennedy - Product Support
 
 
SUBJECT>        Nano Engine Firmware Release
 
 
-----------------------------------------------------------------------
 
The firmware for the Nano Engine is in the process of ECO release and can be 
found on the eunix system in the ~psntd\nano\jedec directory.  There are three 
files.
 
        nepv100.eve - the jedec file for the even PROM
 
        nepv100.odd - the jedec file for the odd PROM
 
        nanoengn.rom - the jedec file that can be downloaded to the Data I/O 
                       programmer and split into the even and odd halves.
 
 
This release is version 1.00.  The floppies and PROM set document part numbers 
are as follows:
 
        380074-001      PROM Set, Nano Engine
 
        780260-001      CRM: Nano Engine Even
 
        780261-001      CRM: Nano Engine Odd
Received: from F29.Tymnet by B39.Tymnet; Tue, 5 Jan 88 16:00:59 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 5 Jan 88 15:55:07 PST
Return-path: <NTD.BASELINE@EMSNTD.Ontyme.Tymnet> 
From: NTD.BASELINE@EMSNTD.Ontyme.Tymnet 
Date: 05 JAN 88 15:25:29 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I31649@Ontyme.Tymnet 
Subject: :SUPERVISOR NUMBERS FOR PRODUCTS ALF & DISP 
SupervisorHexCodeNumbersofDISPandALF.: 

     
     
     
     
     
     
     
                     TYMNET/NETWORK TECHNOLOGY DEVELOPMENT  
                         SOFTWARE DISTRIBUTION SUPPORT 
     
                              M E M O R A N D U M 
     
     
     
   date:   January 5, 1988    
     to:   David Chang   
     cc:   Gary Stoy
   from:   Sheldon Cohen 
subject:   Supervisor Hex Code Numbers of DISP and ALF.
     
     
     
The following two new product IDs have been included   
into the appropriate NTD databases;
     
Prod ID   Description            Supervisor Hex Code   
-------   -------------------    -------------------   
 DISP     ISIS 68K Dispatcher          00A2  
 ALF      ISIS 68K Alfred              00A3  
     
Developer:  David Chang  
 Mktg Mgr:  Gary Stoy    
     
     
     
     
Sheldon Cohen  (408/922-6144) 
Baseline Manager  NTD.S/COHEN  (B5-1)   
     
<
Received: from tymix.Tymnet by B39.Tymnet; Wed, 6 Jan 88 7:26:59 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24622; Wed, 6 Jan 88 07:30:31 PST
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA02768;
	Wed, 6 Jan 88 07:30:28 PST
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA05103; Wed, 6 Jan 88 07:30:24
	PST
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 6 Jan 88 07:30:24 PST 
To: hsing@calvin, wrs@calvin 
Message-id: <8801061530.AA05103@calvin.Tymnet.com> 
Subject: Meeting with IOCP group 

If you haven't heard yet from Roger Schroeder, a meeting has been
scheduled for Wednesday, 3pm, in the 4th floor conference room,
to discuss IOCP/Rodney integration issues.  Please plan on
attending.  I would be there as well except I'm spending the
week hosting AF.

Pat
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Wed, 6 Jan 88 8:42:26 PST 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: CMF Support Task Force - First Meeting 
In-reply-to: your message of Tue, 5 Jan 88 14:39:00 PST

Did you check to see if "trivial password"????????
Received: from tymix.Tymnet by B39.Tymnet; Wed, 6 Jan 88 14:49:26 PST
Received: by tymix.Tymnet (5.51/4.7) id AA25924; Wed, 6 Jan 88 14:52:55 PST
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04594; Wed,
	6 Jan 88 14:52:50 PST
Received: by tymix.Tymnet (5.51/4.7) id AA25920; Wed, 6 Jan 88 14:52:46 PST
Received: by birdix.Tymnet.com (3.2/SMI-3.2) id AA11586; Wed, 6 Jan 88 14:50:53
	PST
Return-path: <birdix!uucp@tymix.Tymnet> 
From: birdix!uucp 
Date: Wed, 6 Jan 88 14:50:53 PST 
To: wrs@tymix 
Message-id: <8801062250.AA11586@birdix.Tymnet.com> 
Subject: "uuxqt cmd wrs) status"... 

uuxqt cmd (rmail wrs) status (DENIED)
Received: from tymix.Tymnet by B39.Tymnet; Wed, 6 Jan 88 18:07:38 PST
Received: by tymix.Tymnet (5.51/4.7) id AA26429; Wed, 6 Jan 88 17:49:44 PST
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA13247; Wed, 6 Jan 88 17:49:29
	PST
Return-path: <dfields@opus> 
From: dfields@opus (David Fields) 
Date: Wed, 6 Jan 88 17:49:29 PST 
To: NTD.A/CHIPPA@EMSNTD.Ontyme.Tymnet.com,
	NTD.A/NEWMAN@EMSNTD.Ontyme.Tymnet.com,
	NTD.D/CHANG@EMSNTD.Ontyme.Tymnet.com,
	NTD.D/RAWSON@EMSNTD.Ontyme.Tymnet.com,
	NTD.D/SMITH@EMSNTD.Ontyme.Tymnet.com,
	NTD.D/TAI@EMSNTD.Ontyme.Tymnet.com, NTD.G/YU@EMSNTD.Ontyme.Tymnet.com,
	NTD.H/RIVERS@EMSNTD.Ontyme.Tymnet.com,
	NTD.N/BURRIESCI@EMSNTD.Ontyme.Tymnet.com,
	NTD.P/CHU@EMSNTD.Ontyme.Tymnet.com,
	NTD.R/RAFFO@EMSNTD.Ontyme.Tymnet.com,
	NTD.R/TENG@EMSNTD.Ontyme.Tymnet.com,
	NTD.S/WEDEL@EMSNTD.Ontyme.Tymnet.com, WRS@B39.Tymnet.com,
	kopfj@B39.Tymnet.com 
Message-id: <8801070149.AA13247@opus.Tymnet.com> 
Subject: LOGII GED Update 

The following changes have been made to the LOGII GED to reflect 
implementation changes made since the last update of this document.

   1. Expanded password MO description to include a method for
      sending null passwords.

   2. Included a description of a logon session wherein a port
 may be assigned before any information is sent to the 
      Supervisor.

   3. Changed Response Code 6 from "Unused, Invalid" to "Invalid
      Throughput Class in ICRM/NCRM".

   4. Removed restrictions on mixing of parsed and unparsed logon
      MO's, as long as information is not duplicated.

   5. Changed Reject Code 0A from "Unused, Invalid" to "Duplicate
      Logon Field".

   6. Reversed the ordering of RCRM RMO's.

   7. Added "PID of Originating Host" to list of excluded MO's in
      needles for Non-LOGII destinations.

   8. Removed one instance of the Terminating host number from 
      CCN and NCN, leaving only the instance within the ISIS Dispatcher
      sequence.

   9. Changed the "Needle Data" MO to the same code as the ISN to 
      to simplify the construction of the ISN for Node Code.

  10. Changed the "SUP.CP" name in the NCN discussion to "SUP.SP".

In addition to the above, it should be noted that the designation
"Required Message Object" is something of a misnomer. RMO's are not
present in LOGII messages if they have a default value and the 
value to be represented is the default. In addition, certain RMO's
are excluded from needles when the destination is not LOGII capable.

The formatted document is available as (DNDSPEC:39)LG2GED.DOC. If you
have any questions, suggestions or changes, please contact me at
x6753 or via ONTYME at NTD.D/FIELDS.

						...Dave Fields

From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 6 Jan 88 18:44:00 PST 
To: <driscoll@calvin> 
Cc: hsing@calvin 
Subject: Re: Meeting with IOCP group 
In-reply-to: <8801061530.AA05103@calvin.Tymnet.com> of Wed, 6 Jan 88 07:30:24
	PST

I'm trying to keep from dying of this cold (yes it is my third since November).
Therefore I won't be at that meeting you had earlier today.  Sorry I didn't let
you know in advance but I haven't been reading my mail very often.  They said
on the news there are 6 different colds going around (some are flu) -- I hope
this doesn't mean I have 3 more to go!  I say 50% chance I'll be in on Friday.
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 7 Jan 88 9:26:53 PST
Received: by tymix.Tymnet (5.51/4.7) id AA29749; Thu, 7 Jan 88 09:30:14 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00330; Thu, 7 Jan 88 09:30:11
	PST
Return-path: <julies@hobbes> 
From: julies@hobbes (Julie Stier) 
Date: Thu, 7 Jan 88 09:30:11 PST 
To: wrs@hobbes 
Message-id: <8801071730.AA00330@hobbes.Tymnet.com> 
Subject: netval 


Bill,

I didn't get your aspen (check my own) until this morning, and it seemed a 
bit too early to call you then.  We are aware of a major problem with
netval ( like not updating the sups ) but haven't found out why or
what yet!!!  It is suspected that it may be a bad board, and we will run
diags this morning.  If you have any clue as to what is going on - please
feel free to share.

julie


Received: from tymix.Tymnet by B39.Tymnet; Thu, 7 Jan 88 10:40:13 PST
Received: by tymix.Tymnet (5.51/4.7) id AA00182; Thu, 7 Jan 88 10:43:39 PST
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01089;
	Thu, 7 Jan 88 10:43:36 PST
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11532; Thu, 7 Jan 88 10:43:35
	PST
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Thu, 7 Jan 88 10:43:35 PST 
To: wrs@calvin 
Message-id: <8801071843.AA11532@calvin.Tymnet.com> 
Subject: Next Rodney Release 

A new version of Rodney is scheduled to be released tomorrow, however
this must be delayed since the bus error code is not completed.  There
is a piece you are responsible for which must be completed before
integration testing of the bus error handling can be done.  When will
this code be available?  I need to know as soon as possible so that
a decision can be made whether to release Rodney without full
bus error handling or to wait and incorporate your additional code.

Hope you're feeling better,

Pat
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 7 Jan 88 11:19:15 PST 
To: <julies@hobbes> 
Subject: Re: netval 
In-reply-to: <8801071730.AA00330@hobbes.Tymnet.com> of Thu, 7 Jan 88 09:30:11
	PST

I'd call Omar and ask him if he knows of anything.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 7 Jan 88 11:41:07 PST 
To: <pchu@opus> 
Cc: pchu@opus, tsay@opus, feldman@comet 
Subject: Re: rsi.h, new events 
In-reply-to: <8801071701.AA14759@opus.Tymnet.com> of Thu, 7 Jan 88 09:01:07 PST

In rsi.h it was a typeo RBRK should have been DBRK which stands for DELTA
BREAK.  I changed it to just BREAK to be more consistent with the 2681 data
sheet.  Sorry for the change.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 7 Jan 88 12:59:15 PST
Received: by tymix.Tymnet (5.51/4.7) id AA00574; Thu, 7 Jan 88 13:02:41 PST
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA03048;
	Thu, 7 Jan 88 13:02:38 PST
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA17585; Thu, 7 Jan 88 13:02:33
	PST
Return-path: <pchu@opus> 
From: pchu@opus (Paul Chu) 
Date: Thu, 7 Jan 88 13:02:33 PST 
To: pchu@opus, tsay@opus, wrs@opus 
Message-id: <8801072102.AA17585@opus.Tymnet.com> 
Subject: new rsi.h 

Bill,

Thanks for the new update, I have updated mine.

I've a question about the RSI_IPC if you don't mind.
TTY driver needs to look at the IPC interrupt, and I believe the clock process
does so too.  So the question is : I need to do rsiSet of the event RSI_IPC
would this be a problem of one overwriting another?

Thanks,

Paul Chu
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 7 Jan 88 17:15:56 PST 
To: <pchu@opus> 
Cc: pchu@opus, tsay@opus 
Subject: Re: new rsi.h 
In-reply-to: <8801072102.AA17585@opus.Tymnet.com> of Thu, 7 Jan 88 13:02:33 PST

I can't think of a reason Rodney's clock stuff would care about RSI_IPC.  If
you have one in mind, let me know.  It only needs RSI_COUNTER.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 7 Jan 88 21:26:40 PST
Received: by tymix.Tymnet (5.51/4.7) id AA02952; Thu, 7 Jan 88 21:30:07 PST
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA22419; Thu, 7 Jan 88 21:30:03
	PST
Return-path: <pchu@opus> 
From: pchu@opus (Paul Chu) 
Date: Thu, 7 Jan 88 21:30:03 PST 
To: WRS@B39.Tymnet.com 
Message-id: <8801080530.AA22419@opus.Tymnet.com> 
Subject: Re: new rsi.h 

Bill,
I wasn't quite sure who else would be interested in the RSI_IPC besides
TTY driver, clock came to mind only because the input port change can
be programmed for the clock or timer to generate an interrupt.

I guess I can safely assume that TTY driver will be the only one to set
the RSI with RSI_IPC.

Thank you for the clarification.
Paul Chu
Received: from tymix.Tymnet by B39.Tymnet; Fri, 8 Jan 88 4:37:21 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04040; Fri, 8 Jan 88 04:23:41 PST
Received: by niagara.Tymnet.com (3.2/SMI-3.2) id AA00432; Wed, 6 Jan 88
	18:15:13 PST
Return-path: <niagara!wrs@tymix.Tymnet> 
From: niagara!wrs (Bill Soley) 
Date: Wed, 6 Jan 88 18:15:13 PST 
To: b39!wrs 
Message-id: <8801070215.AA00432@niagara.Tymnet.com> 
Subject: test to wrs@b39.tymnet 

This should go via tymix (implicitly).
Received: from tymix.Tymnet by B39.Tymnet; Fri, 8 Jan 88 4:37:47 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04042; Fri, 8 Jan 88 04:23:41 PST
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07361; Fri,
	8 Jan 88 04:23:38 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04030; Fri, 8 Jan 88 04:23:32 PST
Received: by niagara.Tymnet.com (3.2/SMI-3.2) id AA00422; Wed, 6 Jan 88
	18:14:31 PST
Return-path: <niagara!wrs@tymix.Tymnet> 
From: niagara!wrs (Bill Soley) 
Date: Wed, 6 Jan 88 18:14:31 PST 
To: wrs@tymix 
Message-id: <8801070214.AA00422@niagara.Tymnet.com> 
Subject: test to tymix!wrs 

Mailalias should resend this to 39.
Received: from F29.Tymnet by B39.Tymnet; Fri, 8 Jan 88 8:44:02 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 8 Jan 88 8:47:40 PST
Return-path: <NTD.J/SCHENK@EMSNTD.Ontyme.Tymnet> 
From: NTD.J/SCHENK@EMSNTD.Ontyme.Tymnet 
Date: 08 JAN 88 08:45:28 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I33098@Ontyme.Tymnet 
Subject: ANNUAL BLOOD DRIVE 
MDCANNUALBLOODDRIVE: 

 
 
 
 
 
                         MCDONNELL DOUGLAS
                     INFORMATION SYSTEMS GROUP
                      INTEROFFICE MEMORANDUM
 
 Network Systems Company            Network Technology Development
 
 
 DATE>       January 8, 1988
 
 SUBJECT>     MDC ANNUAL BLOOD DRIVE
 
 TO>          All NTD Personnel
 
 FROM>        Joy Schenk x 6152
 -----------------------------------------------------------------
 On Tuesday, January 26, nurses from the American Red Cross will be
 here to conduct the annual blood drive.  During the next few days,
 posters and other information will be posted on bulletin boards
 throughout the complex.
 
 I will be taking sign-ups until Friday, January 22, for our divi-
 sion.  If you are interested in being a donor, please contact me.
 Should you have any questions or concerns, you may call one of the
 following numbers:
 
             Dave Naumann  x  7409
             Renee McNeil  x  8007
             Red Cross     =  408/292-6242
 
 There is additional information at my desk, and an informative
 film will be shown in the waiting area prior to giving blood.
 The Red Cross does ask that you try to provide a specific time
 so they are able to schedule the appropriate number of nurses.
 
 Thanks, and we're looking forward to a very successful drive!
Received: from tymix.Tymnet by B39.Tymnet; Fri, 8 Jan 88 8:52:22 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04510; Fri, 8 Jan 88 08:55:50 PST
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA02668; Fri, 8 Jan 88 08:55:47
	PST
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Fri, 8 Jan 88 08:55:47 PST 
To: WRS@B39.Tymnet.com 
Message-id: <8801081655.AA02668@calvin.Tymnet.com> 
Subject: Re: Next Rodney Release 

Whatever piece needs to be glued to Thomas Fok's code to complete
testing of his code is what is needed.  If this is the inventory code,
fine, if code for berrBegin/berrEnd is also past of this code then
please complete it immediately.  I'm also in and out of the office
on AF business, so if you could point Yu-Liang at the code when
it's ready I would appreciate it.  Thanks,

Pat
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 8 Jan 88 10:57:31 PST 
To: <driscoll@calvin> 
Subject: Re: Next Rodney Release 
In-reply-to: <8801081655.AA02668@calvin.Tymnet.com> of Fri, 8 Jan 88 08:55:47
	PST

I'll try to have berrBegin/End by Monday.  I've taken a bad turn so I don't
know what will come.  I really shouldn't even try, but heck, I'm a hopeless
workaholic as my wife frequently reminds me (but she's in Tahoe with the
baby where I am supposed to be, too).  Oh no - I'm babling - Bye!  -Bill
Received: from X32.Tymnet by B39.Tymnet; Fri, 8 Jan 88 11:13:09 PST
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Wed, 6 Jan 88 23:05:42 PST 
To: William R Soley <WRS@B39.Tymnet> 
Subject: re: NONAME 

Thanks for the feedback... I will look into the ?i/o to unnassigned chan
problem, since it seems to occur more than I had noticed it...

Mind you, this is still the old NONAME source, written in SIMPL, and that
is why the input is still so cretinous...  will try to fix the bugs before
it is released to the public.

/Carl
Received: from D65.Tymnet by B39.Tymnet; Sat, 9 Jan 88 12:46:27 PST
Return-path: <wlaforge@d65.tymnet> 
From: William LaForge <wlaforge@d65.tymnet> 
Date: Sat, 9 Jan 88 15:44:50 EST 
To: Mail Wizard <MAIL> 
Subject: TUMS-10 bug report (v45000003 of 07-DEC-87) 

this is not a bug
this is not a bug

Have you considered the use of the command bye as an alternate to exit?

Since making heavy use of a DG computer, which uses BYE for everything,
I have finally begun using BYE to log off of 65.  I now use 65 only to
use tums (a million times better than ONTYME).  And everyday now, tums
tells me that it doesn't know what BYE means.

I know, this is very petty.  But having seen how very responsive you are
to so many other requests, I figure "it would not hurt to ask."
Received: from tymix.Tymnet by B39.Tymnet; Sat, 9 Jan 88 17:19:06 PST
Received: by tymix.Tymnet (5.51/4.7) id AA11806; Sat, 9 Jan 88 17:17:11 PST
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA14758;
	Sat, 9 Jan 88 17:17:08 PST
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA02010; Sat, 9 Jan 88 17:17:06
	PST
Return-path: <jwillott@opus> 
From: jwillott@opus (Jim Willott) 
Date: Sat, 9 Jan 88 17:17:06 PST 
To: wrs@opus 
Message-id: <8801100117.AA02010@opus.Tymnet.com> 
Subject: How do I get dp or dp2 to print in landscape from psroff? 


Received: from F29.Tymnet by B39.Tymnet; Mon, 11 Jan 88 14:15:06 PST
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Mon, 11 Jan 88 14:12:39 PST
Return-path: <IPC.R/SWANSON@EMSTUMS.Ontyme.Tymnet> 
From: IPC.R/SWANSON@EMSTUMS.Ontyme.Tymnet 
Date: 11 JAN 88 21:49:32 
To: TUMS.SUP@EMSTUMS.Ontyme.Tymnet 
Message-id: A49530@Ontyme.Tymnet 
Subject: ISG CONVERSION SERVICE 



TO:       ALL ISG BUSINESS UNITS

SUBJECT:  ISG WORD PROCESSING CONVERSION SERVICE


    ISG is now offering Word Processing Document conversions.  This
service is being performed in the St. Louis Computer Center.  Current
software conversions include the following:

                    WANG 8" to MULTIMATE
                    WANG 8" to IBM DISPLAYWRITER
                    WANG 8" to DISPLAYWRITE III
                    WANG 5 1/4" to MULTIMATE
                    MULTIMATE to WANG 8"
                    MULTIMATE to IBM DISPLAYWRITER
                    MULTIMATE to XEROX 860
                    IBM DISPLAYWRITER to WANG 8"
                    IBM DISPLAYWRITER to MULTIMATE


   Software pairings (single-direction-links between incompatibles)
are readily available for virtually all major Word Processing
packages and dedicated systems on the market today.  Additional
pairings will be ordered as needed.  A total list of the available
software pairings for conversions can be aquired from the location
below.


    For further information reguarding the conversion service,  you
may contact:

                         ISG CONVERSION SERVICE
                         MAIL STOP: 307/2E/L237/D15
                         Phone:  314-233-8250
Received: from F29.Tymnet by B39.Tymnet; Mon, 11 Jan 88 15:25:37 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 11 Jan 88 15:20:00 PST
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 11 JAN 88 14:59:21 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I34124@Ontyme.Tymnet 
Subject: "TO: ALL ISG BUSINESS UNITS SUBJECT:"... 



TO:       ALL ISG BUSINESS UNITS

SUBJECT:  ISG WORD PROCESSING CONVERSION SERVICE


    ISG is now offering Word Processing Document conversions.  This
service is being performed in the St. Louis Computer Center.  Current
software conversions include the following:

                    WANG 8" to MULTIMATE
                    WANG 8" to IBM DISPLAYWRITER
                    WANG 8" to DISPLAYWRITE III
                    WANG 5 1/4" to MULTIMATE
                    MULTIMATE to WANG 8"
                    MULTIMATE to IBM DISPLAYWRITER
                    MULTIMATE to XEROX 860
                    IBM DISPLAYWRITER to WANG 8"
                    IBM DISPLAYWRITER to MULTIMATE


   Software pairings (single-direction-links between incompatibles)
are readily available for virtually all major Word Processing
packages and dedicated systems on the market today.  Additional
pairings will be ordered as needed.  A total list of the available
software pairings for conversions can be aquired from the location
below.


    For further information reguarding the conversion service,  you
may contact:

                         ISG CONVERSION SERVICE
                         MAIL STOP: 307/2E/L237/D15
                         Phone:  314-233-8250
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 12 Jan 88 11:28:58 PST 
To: CARL@D35 
Cc: WRS@C39.Tymnet 
Subject: "SETOP SINCE WE ARE NOW WELL PAST"... 

SETOP - SINCE WE ARE NOW WELL PAST 1987 - IS IT READY???????
CAN WE MEET NEXT WEEK THURS OR FRI (1/21-22) TO REVIEW?????
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 12 Jan 88 11:40:25 PST 
To: <wlaforge@d65.tymnet> 
Subject: Re: TUMS-10 bug report (v45000003 of 07-DEC-87) 
In-reply-to: your message of Sat, 9 Jan 88 15:44:50 EST

You can define your own commands using the "SET MACRO" command.  For example,
>set macro bye exit
>profile
>
will do what you asked for.  The PROFILE command makes the change permanent
by writting it out to the file TUMS.INI in your home directory.  I've also
seen people do things like
>set macro trash "delete noarchive"
and the like.  If the body is more than one word it must be in quotes.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 12 Jan 88 11:43:00 PST 
To: <jwillott@opus> 
Subject: Re: How do I get dp or dp2 to print in landscape from psroff? 
In-reply-to: <8801100117.AA02010@opus.Tymnet.com> of Sat, 9 Jan 88 17:17:06 PST

I don't know of a way.  You could probably trick it by sneaking in the proper
translate command into the postscript stream but that would be alot of black
magic and it might still refuse to output more than 8.5" across.  -Bill
Received: from D65.Tymnet by B39.Tymnet; Tue, 12 Jan 88 17:03:28 PST
Return-path: <wlaforge@d65.tymnet> 
From: William LaForge <wlaforge@d65.tymnet> 
Date: Tue, 12 Jan 88 20:01:14 EST 
To: <WRS@B39.Tymnet> 
Subject: Re: TUMS-10 bug report (v45000003 of 07-DEC-87) 
In-reply-to: your message of Tue, 12 Jan 88 11:40:25 PST

Gee, why didn't I think of that?

Thanks,
Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 13 Jan 88 6:44:11 PST
Received: by tymix.Tymnet (5.51/4.7) id AA25164; Wed, 13 Jan 88 06:42:07 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09144; Wed, 13 Jan 88
	06:42:05 PST
Return-path: <dlasater@hobbes> 
From: dlasater@hobbes (Dan Lasater) 
Date: Wed, 13 Jan 88 06:42:05 PST 
To: WRS@B39.Tymnet.com 
Message-id: <8801131442.AA09144@hobbes.Tymnet.com> 
Subject: Re: polymorphous illness 


Received: from tymix.Tymnet by B39.Tymnet; Wed, 13 Jan 88 6:48:34 PST
Received: by tymix.Tymnet (5.51/4.7) id AA25171; Wed, 13 Jan 88 06:46:28 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09150; Wed, 13 Jan 88
	06:46:26 PST
Return-path: <dlasater@hobbes> 
From: dlasater@hobbes (Dan Lasater) 
Date: Wed, 13 Jan 88 06:46:26 PST 
To: WRS@B39.Tymnet.com 
Message-id: <8801131446.AA09150@hobbes.Tymnet.com> 
Subject: Re: polymorphous illness 

I hope you get well soon and do make every effort to get over it now.
Doctors need language to confound the innocent just like programmers.
Imagine what a sane non-programmer would think a deadly embrace while
trying to access a shared data structure between two concurrent procedures.
Dan
Received: from tymix.Tymnet by B39.Tymnet; Wed, 13 Jan 88 7:00:00 PST
Received: by tymix.Tymnet (5.51/4.7) id AA25205; Wed, 13 Jan 88 06:57:56 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09163; Wed, 13 Jan 88
	06:57:53 PST
Return-path: <dlasater@hobbes> 
From: dlasater@hobbes (Dan Lasater) 
Date: Wed, 13 Jan 88 06:57:53 PST 
To: wrs@hobbes 
Message-id: <8801131457.AA09163@hobbes.Tymnet.com> 
Subject: Craig Fletcher 


I've been approached by Craig Fletcher for a management position here.
Can you tell me anything about him?  
Dan
Received: from tymix.Tymnet by B39.Tymnet; Wed, 13 Jan 88 12:24:29 PST
Received: by tymix.Tymnet (5.51/4.7) id AA25798; Wed, 13 Jan 88 12:22:19 PST
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10947;
	Wed, 13 Jan 88 12:22:15 PST
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA19843; Wed, 13 Jan 88
	12:22:13 PST
Return-path: <baba@calvin> 
From: baba@calvin (Duane Hentrich) 
Date: Wed, 13 Jan 88 12:22:13 PST 
To: wrs@calvin 
Message-id: <8801132022.AA19843@calvin.Tymnet.com> 
Subject: longjmp, a SEGMENT VIOLATION in 


Greetings!

	I'm trying to debug the session under dbx & rksim
and I would appreciate some assistance since it is dying
with the error noted inthe subject line.  

	I've tried to trace to the error but it seems that
when trace is on all I get to execute is clksig's.

	Looking forward to talking with you.

Your Servant

d'baba Duane Hentrich
6096

From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 14 Jan 88 0:58:00 PST 
To: <dlasater@hobbes> 
Subject: Re: Craig Fletcher 
In-reply-to: <8801131457.AA09163@hobbes.Tymnet.com> of Wed, 13 Jan 88 06:57:53
	PST

I only ever met Craig once and really don't remember him.  I have not heard
anything bad from the people that work for him.  You might ask Dennis Coffee
who just started for Ron Saltgaver - I believe Dennis reported to Craig.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Thu, 14 Jan 88 8:34:20 PST
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 8:30:36 PST 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Error response(s) from SLVMCL1.McAuto.Tymnet ...
SNDMSG000R Transmit message, end with '.':
SNDMSG001R Transmit recipients, end with '.':
DMKCSP053E 092      NOT IN CP DIRECTORY
SNDMSG003W Send to 092 at SLVMCL1 failed due to error 53
SNDMSG004W Send failed to 1 recipients.
SNDMSG005E Note sent to 0 recipients.
R; T=0.08/0.26 10:30:14



----------
RCPT To: <092@SLVMCL1.McAuto.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 8:30:03 PST 
To: KEN THOELE <092@SLVMCL1.McAuto.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected
in extended name field or 1st line of body.

----------
Received: from SLVMCL1.McAuto.Tymnet by F29.Tymnet; Thu, 14 Jan 88 8:29:58 PST
From: KEN THOELE <092@SLVMCL1.McAuto.Tymnet> 
Date: 14JAN88 1026AM 
To: M067885@SLVN307.MCAUTO.TYMNET <M067885@SLVN307.McAuto.Tymnet> 
RECEIVED: from F29.Tymnet by SLVMCL1.McAuto.Tymnet; Thu, 14 Jan 88 8:27:41 PST
RECEIVED: from TMS.Tymnet by F29.Tymnet; Thu, 14 Jan 88 8:27:03 PST
MESSAGE-ID: 048099@TMS.Tymnet
SUBJ: TEST NOTE

THIS IS A TEST MESSAGE SENT FROM TMS THROUGH TUMSGATE ON THE
WEST COAST TO PROFS.
 
THST'S ALL.
 
.........KEN THOELE
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Thu, 14 Jan 88 9:13:53 PST 
To: <Carl@F33.Tymnet> 
Cc: WRS@C39.Tymnet 
Subject: Re: Meeting... next week 
In-reply-to: your message of Wed, 13 Jan 88 18:38:16 PST

GLAD TO HEAR PROGRESS - Suggest 11:30 thurs, 1/21 lunch and meeting..
By way of copy, Bill... can you join us, for lunch at least?????
Received: from F29.Tymnet by B39.Tymnet; Thu, 14 Jan 88 11:45:53 PST
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 11:26:40 PST 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by EMSTUMS.Ontyme.Tymnet
The response was:
:send SYSTEM.ONTYME
SUBJ: 
NONEXISTENT RECIPIENT
SYSTEM.ONTYME
MESSAGE NOT SENT


----------
RCPT To: <SYSTEM.ONTYME.@EMSTUMS.Ontyme.Tymnet> 
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 14 Jan 88 11:23:28 PST
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 10:49:48 PST 
To: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    RCPT TO: <CHAPLEAU@OFFICE-1.ARPA>

  The error code and message was:
    550 No such mailbox here

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <CHAPLEAU@OFFICE-1.ARPA>

----------
RCPT To: <CHAPLEAU@OFFICE-1.ARPA> 
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Thu, 14 Jan 88 10:48:26 PST
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 14 JAN 88 18:32:30 
To: <CHAPLEAU@OFFICE-1.ARPA> 
Message-id: A50733@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - INVALID RECIPIENTS 

Message #A50726 was not delivered to the following recipient(s):

        CSC.J/MURRAY.

These recipients do not exist.
Please check the spelling of the recipient(s) listed above.

(undelivered message follows)
------------------------------------------------------------------------
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 14 Jan 88 10:23:16 PST
Received: from OFFICE-1.ARPA by F29.Tymnet; Thu, 14 Jan 88 10:10:40 PST
Return-path: <CHAPLEAU@OFFICE-1.ARPA> 
From: John Chapleau, Augmentation Systems Division / MDC
        <CARE.MDC@OFFICE-1.ARPA> 
Date: 14 Jan 88 09:56 PST 
To: DEV.MDC@OFFICE-1.ARPA, SKH.MDC@OFFICE-1.ARPA, LMP.MDC@OFFICE-1.ARPA,
        csc.j/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        csc.d/weaver%Ontyme.Tymnet@OFFICE-1.ARPA,
        csc.m/sorscher%Ontyme.Tymnet@OFFICE-1.ARPA 
Cc: ipc.n/marsh%Ontyme.Tymnet@OFFICE-1.ARPA, mosher@SUN.COM,
        VLC.MDC@OFFICE-1.ARPA, fsc.r/mcneil%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.ja/smith%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.s/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        txs.o/guven%Ontyme.Tymnet@OFFICE-1.ARPA,
        txs.c/baltrunas%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.v/brown%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.r/ferguson%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.r/williamson%Ontyme.Tymnet@OFFICE-1.ARPA,
        tym.d/burke%Ontyme.Tymnet@OFFICE-1.ARPA 
Message-id: <MDC-CARE-CY1NT@OFFICE-1> 
Subject: Chinese lunch on 1st Street. Fri 1/22. 

Next week Mary Coppernoll's annual birthday reckoning is due, and her immediate
(D23) work group will be treating her to lunch at the House of Cathay, 1339 N 
1st.  Anyone else on D2 is invited to just come along and buy a single share of
this Chinese lunch.

We'd like to start lunch sometime between than 11:30 and noon, because some of 
us have an interest in attending a 1:30 presentation that afternoon at the 
Apple 4-Phase auditorium on "Hypertext Publishing and the Evolution of 
Knowledge". 

--john chapleau   rsvp 408-922-6586
Received: from F29.Tymnet by B39.Tymnet; Thu, 14 Jan 88 11:46:03 PST
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 11:29:42 PST 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by EMSTUMS.Ontyme.Tymnet
The response was:
:send SYSTEM.ONTYME
SUBJ: 
NONEXISTENT RECIPIENT
SYSTEM.ONTYME
MESSAGE NOT SENT


----------
RCPT To: <SYSTEM.ONTYME.@EMSTUMS.Ontyme.Tymnet> 
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 14 Jan 88 11:26:41 PST
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 10:49:50 PST 
To: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    RCPT TO: <CHAPLEAU@OFFICE-1.ARPA>

  The error code and message was:
    550 No such mailbox here

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <CHAPLEAU@OFFICE-1.ARPA>

----------
RCPT To: <CHAPLEAU@OFFICE-1.ARPA> 
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Thu, 14 Jan 88 10:48:56 PST
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 14 JAN 88 18:32:37 
To: <CHAPLEAU@OFFICE-1.ARPA> 
Message-id: A50734@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - INVALID RECIPIENTS 

Message #A50730 was not delivered to the following recipient(s):

        CSC.D/WEAVER.

These recipients do not exist.
Please check the spelling of the recipient(s) listed above.

(undelivered message follows)
------------------------------------------------------------------------
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 14 Jan 88 10:25:59 PST
Received: from OFFICE-1.ARPA by F29.Tymnet; Thu, 14 Jan 88 10:10:48 PST
Return-path: <CHAPLEAU@OFFICE-1.ARPA> 
From: John Chapleau, Augmentation Systems Division / MDC
        <CARE.MDC@OFFICE-1.ARPA> 
Date: 14 Jan 88 09:56 PST 
To: DEV.MDC@OFFICE-1.ARPA, SKH.MDC@OFFICE-1.ARPA, LMP.MDC@OFFICE-1.ARPA,
        csc.j/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        csc.d/weaver%Ontyme.Tymnet@OFFICE-1.ARPA,
        csc.m/sorscher%Ontyme.Tymnet@OFFICE-1.ARPA 
Cc: ipc.n/marsh%Ontyme.Tymnet@OFFICE-1.ARPA, mosher@SUN.COM,
        VLC.MDC@OFFICE-1.ARPA, fsc.r/mcneil%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.ja/smith%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.s/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        txs.o/guven%Ontyme.Tymnet@OFFICE-1.ARPA,
        txs.c/baltrunas%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.v/brown%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.r/ferguson%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.r/williamson%Ontyme.Tymnet@OFFICE-1.ARPA,
        tym.d/burke%Ontyme.Tymnet@OFFICE-1.ARPA 
Message-id: <MDC-CARE-CY1NT@OFFICE-1> 
Subject: Chinese lunch on 1st Street. Fri 1/22. 

Next week Mary Coppernoll's annual birthday reckoning is due, and her immediate
(D23) work group will be treating her to lunch at the House of Cathay, 1339 N 
1st.  Anyone else on D2 is invited to just come along and buy a single share of
this Chinese lunch.

We'd like to start lunch sometime between than 11:30 and noon, because some of 
us have an interest in attending a 1:30 presentation that afternoon at the 
Apple 4-Phase auditorium on "Hypertext Publishing and the Evolution of 
Knowledge". 

--john chapleau   rsvp 408-922-6586
Received: from F29.Tymnet by B39.Tymnet; Thu, 14 Jan 88 11:46:10 PST
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 11:32:44 PST 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by EMSTUMS.Ontyme.Tymnet
The response was:
:send SYSTEM.ONTYME
SUBJ: 
NONEXISTENT RECIPIENT
SYSTEM.ONTYME
MESSAGE NOT SENT


----------
RCPT To: <SYSTEM.ONTYME.@EMSTUMS.Ontyme.Tymnet> 
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 14 Jan 88 11:29:43 PST
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 10:50:01 PST 
To: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    RCPT TO: <CHAPLEAU@OFFICE-1.ARPA>

  The error code and message was:
    550 No such mailbox here

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <CHAPLEAU@OFFICE-1.ARPA>

----------
RCPT To: <CHAPLEAU@OFFICE-1.ARPA> 
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Thu, 14 Jan 88 10:49:22 PST
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 14 JAN 88 18:32:44 
To: <CHAPLEAU@OFFICE-1.ARPA> 
Message-id: A50736@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - INVALID RECIPIENTS 

Message #A50732 was not delivered to the following recipient(s):

        CSC.M/SORSCHER.

These recipients do not exist.
Please check the spelling of the recipient(s) listed above.

(undelivered message follows)
------------------------------------------------------------------------
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 14 Jan 88 10:28:38 PST
Received: from OFFICE-1.ARPA by F29.Tymnet; Thu, 14 Jan 88 10:10:55 PST
Return-path: <CHAPLEAU@OFFICE-1.ARPA> 
From: John Chapleau, Augmentation Systems Division / MDC
        <CARE.MDC@OFFICE-1.ARPA> 
Date: 14 Jan 88 09:56 PST 
To: DEV.MDC@OFFICE-1.ARPA, SKH.MDC@OFFICE-1.ARPA, LMP.MDC@OFFICE-1.ARPA,
        csc.j/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        csc.d/weaver%Ontyme.Tymnet@OFFICE-1.ARPA,
        csc.m/sorscher%Ontyme.Tymnet@OFFICE-1.ARPA 
Cc: ipc.n/marsh%Ontyme.Tymnet@OFFICE-1.ARPA, mosher@SUN.COM,
        VLC.MDC@OFFICE-1.ARPA, fsc.r/mcneil%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.ja/smith%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.s/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        txs.o/guven%Ontyme.Tymnet@OFFICE-1.ARPA,
        txs.c/baltrunas%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.v/brown%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.r/ferguson%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.r/williamson%Ontyme.Tymnet@OFFICE-1.ARPA,
        tym.d/burke%Ontyme.Tymnet@OFFICE-1.ARPA 
Message-id: <MDC-CARE-CY1NT@OFFICE-1> 
Subject: Chinese lunch on 1st Street. Fri 1/22. 

Next week Mary Coppernoll's annual birthday reckoning is due, and her immediate
(D23) work group will be treating her to lunch at the House of Cathay, 1339 N 
1st.  Anyone else on D2 is invited to just come along and buy a single share of
this Chinese lunch.

We'd like to start lunch sometime between than 11:30 and noon, because some of 
us have an interest in attending a 1:30 presentation that afternoon at the 
Apple 4-Phase auditorium on "Hypertext Publishing and the Evolution of 
Knowledge". 

--john chapleau   rsvp 408-922-6586
Received: from F29.Tymnet by B39.Tymnet; Thu, 14 Jan 88 12:17:09 PST
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 12:08:18 PST 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by EMSTUMS.Ontyme.Tymnet
The response was:
:send SYSTEM.ONTYME
SUBJ: 
NONEXISTENT RECIPIENT
SYSTEM.ONTYME
MESSAGE NOT SENT


----------
RCPT To: <SYSTEM.ONTYME.@EMSTUMS.Ontyme.Tymnet> 
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 14 Jan 88 12:04:46 PST
From: Postmaster@F29.Tymnet 
Date: Thu, 14 Jan 88 11:18:49 PST 
To: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    RCPT TO: <CHAPLEAU@OFFICE-1.ARPA>

  The error code and message was:
    550 No such mailbox here

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <CHAPLEAU@OFFICE-1.ARPA>

----------
RCPT To: <CHAPLEAU@OFFICE-1.ARPA> 
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Thu, 14 Jan 88 11:17:58 PST
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 14 JAN 88 19:05:54 
To: <CHAPLEAU@OFFICE-1.ARPA> 
Message-id: A50759@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - INVALID RECIPIENTS 

Message #A50757 was not delivered to the following recipient(s):

        TYM.D/BURKE.

These recipients do not exist.
Please check the spelling of the recipient(s) listed above.

(undelivered message follows)
------------------------------------------------------------------------
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 14 Jan 88 10:57:25 PST
Received: from OFFICE-1.ARPA by F29.Tymnet; Thu, 14 Jan 88 10:11:56 PST
Return-path: <CHAPLEAU@OFFICE-1.ARPA> 
From: John Chapleau, Augmentation Systems Division / MDC
        <CARE.MDC@OFFICE-1.ARPA> 
Date: 14 Jan 88 09:56 PST 
To: DEV.MDC@OFFICE-1.ARPA, SKH.MDC@OFFICE-1.ARPA, LMP.MDC@OFFICE-1.ARPA,
        csc.j/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        csc.d/weaver%Ontyme.Tymnet@OFFICE-1.ARPA,
        csc.m/sorscher%Ontyme.Tymnet@OFFICE-1.ARPA 
Cc: ipc.n/marsh%Ontyme.Tymnet@OFFICE-1.ARPA, mosher@SUN.COM,
        VLC.MDC@OFFICE-1.ARPA, fsc.r/mcneil%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.ja/smith%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.s/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        txs.o/guven%Ontyme.Tymnet@OFFICE-1.ARPA,
        txs.c/baltrunas%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.v/brown%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.r/ferguson%Ontyme.Tymnet@OFFICE-1.ARPA,
        fsc.r/williamson%Ontyme.Tymnet@OFFICE-1.ARPA,
        tym.d/burke%Ontyme.Tymnet@OFFICE-1.ARPA 
Message-id: <MDC-CARE-CY1NT@OFFICE-1> 
Subject: Chinese lunch on 1st Street. Fri 1/22. 

Next week Mary Coppernoll's annual birthday reckoning is due, and her immediate
(D23) work group will be treating her to lunch at the House of Cathay, 1339 N 
1st.  Anyone else on D2 is invited to just come along and buy a single share of
this Chinese lunch.

We'd like to start lunch sometime between than 11:30 and noon, because some of 
us have an interest in attending a 1:30 presentation that afternoon at the 
Apple 4-Phase auditorium on "Hypertext Publishing and the Evolution of 
Knowledge". 

--john chapleau   rsvp 408-922-6586
Received: from tymix.Tymnet by B39.Tymnet; Thu, 14 Jan 88 13:26:22 PST
Received: by tymix.Tymnet (5.51/4.7) id AA00543; Thu, 14 Jan 88 13:24:14 PST
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18824;
	Thu, 14 Jan 88 13:24:12 PST
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA07031; Thu, 14 Jan 88 13:24:07
	PST
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Thu, 14 Jan 88 13:24:07 PST 
To: rodney@comet 
Message-id: <8801142124.AA07031@comet.Tymnet.com> 
Subject: protocol id assignments 

I have created a file /usr/calvin3/rodney/v1/include/protocol.h to
hold the protocol ID assignments.  I've started it off with SFS
and session protocol numbers.  Any other protocols in use by us or
the patrons will have to be added.
	Steve
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 14 Jan 88 15:19:34 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: <Carl@F33.Tymnet> 
Subject: Re: Meeting... next week 
In-reply-to: your message of Thu, 14 Jan 88 9:13:53 PST

I'm still ill so ask me about lunch next week.  -Bill
From: DLASATER@B39.Tymnet 
Date: Fri, 15 Jan 88 8:27:09 PST 
To: Mail Wizard <MAIL> 
Subject: TUMS-10 bug report (v45000003 of 07-DEC-87) 

I seem to have no end of problems reading my mail.  I consistantly
get an error while starting TUMS when it tells me I have mail
waiting.  On re-entry to Tums I find I have the same mail as last time.
Dan Lasater
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 15 Jan 88 10:34:10 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: INTL.VIDA@ONTYME.TYMNET 
Subject: Re: "Hope you are feeling better...."... 
In-reply-to: your message of Fri, 15 Jan 88 10:01:30 PST

Probably, I don't know exactly what's going on yet with the reorganization.
So far you are the only one to tell me anything about it despite my having
asked several times.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 15 Jan 88 10:54:38 PST 
To: DLASATER@B39.Tymnet 
Subject: Re: TUMS-10 bug report (v45000003 of 07-DEC-87) 
In-reply-to: your message of Fri, 15 Jan 88 8:27:09 PST

Well, I managed to get it to read your new mail by splitting it in half and
letting it read half at a time.  I'm not really sure that's what did it since
it behaves differently each time I run it.  Oh well.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 15 Jan 88 17:02:51 PST
Received: by tymix.Tymnet (5.51/4.7) id AA05684; Fri, 15 Jan 88 17:00:31 PST
Received: by niagara.Tymnet.com (3.2/SMI-3.2) id AA00872; Fri, 15 Jan 88
	16:41:40 PST
Return-path: <niagara!wrs@tymix.Tymnet> 
From: niagara!wrs (Bill Soley) 
Date: Fri, 15 Jan 88 16:41:40 PST 
To: ntd.s/black@ontyme, ntd.w/euske@ontyme, wrs@39 
Message-id: <8801160041.AA00872@niagara.Tymnet.com> 
Subject: still leave 

Starr, this is to follow up the phone messages I left in case they got lost.
The 4th through the 15th should be accounted as sick time.  I expect to be
back on Monday, but I'm having a little relapse and the Dr. wants to try
putting me back on a modified dosage of the drug I had the awful reaction to.
Being out more than 5 contiguous days, I understand I need to file for short
term disability.  Is that correct?  Is there anything I need to do now or can
all that wait until I come back?  Thanks -Bill
Received: from F29.Tymnet by B39.Tymnet; Fri, 15 Jan 88 17:15:40 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 15 Jan 88 17:12:59 PST
Return-path: <NTD.BASELINE@EMSNTD.Ontyme.Tymnet> 
From: NTD.BASELINE@EMSNTD.Ontyme.Tymnet 
Date: 15 JAN 88 16:57:56 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I36772@Ontyme.Tymnet 
Subject: :RELEASE OF SOFTWARE BASELINE 'Z' 
RELEASEOFSOFTWAREBASELINE"Z": 

     
     
       =================================================================   
        *** NETWORK SOFTWARE BROADCAST *** NETWORK SOFTWARE BROADCAST ***  
        =================================================================  
        **** UPDATE **** UPDATE **** UPDATE **** UPDATE **** UPDATE *****  
        =================================================================  
                                                              PAGE 1 of 2  
     
        DATE:       January 15, 1988    
        TO:         DISTRIBUTION   
        FROM:       Sheldon Cohen,  Software Distribution Support
        SUBJ:       RELEASE OF SOFTWARE BASELINE "Z"   
        REFERENCE:  Baseline Document  (BASELINE)LETTER.Z03 
     
        Baseline "Z" is released as of January 15, 1988 and includes  
        the following changes:
     
        1. NEW PRODUCTS ADDED TO THE BASELINE            Old     New  
        --------------------------------------------    -----   ----- 
           none
     
        2. UPDATED BASELINE PRODUCTS                     Old     New  
        --------------------------------------------    -----   ----- 
           DISK   Engine 10MB Disk Diagnostic           03.01   03.02 
           ELF    Eclectic Load Facility                04.00   04.02 
           I2IS   ISIS2 Kernel                          07.00   07.03 
           RAM    Raw Accounting Merger                 10.07   12.01 
     
     
        3. INTERMEDIATE RELEASES TO BASELINE             Old     New  
        --------------------------------------------    -----   ----- 
           none
     
     
        The complete Baseline "Z" document set may be printed by 
        specifying (BASELINE)PRINT.Z on system 39.  For information   
        concerning (BASELINE)PRINT.Z on other systems, contact   
        Marketing/QA.    
     
        A final print file for the previous Baseline "Y" document
        set updated October 15, 1987 may be obtained by specifying    
        (BASELINE)PRINT.Y on system 39. 
     
     
     
        =================================================================  
        *** NETWORK SOFTWARE BROADCAST *** NETWORK SOFTWARE BROADCAST ***  
        =================================================================  
        **** UPDATE **** UPDATE **** UPDATE **** UPDATE **** UPDATE *****  
        =================================================================  
     
     
     
     
        A Baseline Release and Deletion Schedule is included below:   
     
     
                      BASELINE RELEASE AND DELETION SCHEDULE
                             THROUGH July  15, 1989    
     
     
     
        BASELINE:           Q            R            S            T  
        Release Date:   Oct 15,1985  Jan 15,1986  Apr 15,1986  Jul 15,1986 
        Delete  Date:       **           **           **           ** 
     
     
        BASELINE:           U*           V*           W*           X* 
        Release Date:   Oct 15,1986  Jan 15,1987  Apr 15,1987  Jul 15,1987 
        Delete  Date:   Apr 15,1988  Jul 15,1988  Oct 15,1988  Jan 15,1989 
     
     
        BASELINE:           Y*           Z*           A            B  
        Release Date:   Oct 15,1987  Jan 15,1988  Apr 15,1988  Jul 15,1988 
        Delete  Date:   Apr 15,1989  Jul 15,1989  Oct 15,1989  Jan 15,1990 
     
     
     
        BASELINE:           C            D            E            F  
        Release Date:   Oct 15,1988  Jan 15,1989  Apr 15,1989  Jul 15,1989 
        Delete  Date:   Apr 15,1990  Jul 15,1990  Oct 15,1990  Jan 15,1991 
     
     
     
        *   Baselines supported as of January 15, 1988 (U through Z). 
        **  Baselines previously deleted and stored on history tape.
Received: from tymix.Tymnet by B39.Tymnet; Mon, 18 Jan 88 19:02:12 PST
Received: by tymix.Tymnet (5.51/4.7) id AA17751; Mon, 18 Jan 88 19:05:37 PST
Received: by niagara.Tymnet.com (3.2/SMI-3.2) id AA02827; Mon, 18 Jan 88
	18:40:23 PST
Return-path: <niagara!wrs@tymix.Tymnet> 
From: niagara!wrs (Bill Soley) 
Date: Mon, 18 Jan 88 18:40:23 PST 
To: wrs@b39 
Message-id: <8801190240.AA02827@niagara.Tymnet.com> 
Subject: test to 39 

Hello
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 19 Jan 88 10:34:14 PST 
To: WRS@C39.Tymnet 
Subject: "Hope you are well enough to get"... 

Hope you are well enough to get to your messages.... I'm waiting 
answers to last one.... WILL I SEE YOU TODAY @ 2;00?????B3 CONF RM...
Received: from tymix.Tymnet by B39.Tymnet; Tue, 19 Jan 88 12:59:20 PST
Received: by tymix.Tymnet (5.51/4.7) id AA21607; Tue, 19 Jan 88 13:02:50 PST
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15237; Tue,
	19 Jan 88 13:02:48 PST
Received: by f104.sun.com (3.2/SMI-3.2) id AA07843; Tue, 19 Jan 88 13:02:45 PST
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 19 Jan 88 13:02:45 PST 
To: wrs@f104 
Message-id: <8801192102.AA07843@f104.sun.com> 
Subject: k_clear() 

Thanks.  Also, Mike Jastrzebski has references to k_clear() in his boot
code.  My impression was that this was to cancel any pending k_causes
that were done spuriously (I think you said that k_waiting processes
have to be able to wake up for no reason sometimes).  I implemented this
in my version of exec.c so that it simply cleared the wakeup flag, but
I'm still having some timing problems and I wondered if k_clear() is
supposed to do something else as well?


Received: from tymix.Tymnet by B39.Tymnet; Tue, 19 Jan 88 13:44:07 PST
Received: by tymix.Tymnet (5.51/4.7) id AA21684; Tue, 19 Jan 88 13:47:40 PST
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15536;
	Tue, 19 Jan 88 13:47:36 PST
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA00598; Tue, 19 Jan 88 13:47:29
	PST
Return-path: <rongchu@opus> 
From: rongchu@opus (Rong Chu) 
Date: Tue, 19 Jan 88 13:47:29 PST 
To: comstock@opus, doyle@opus, ernst@opus, hsing@opus, rogers@opus,
	rongchu@opus, wrs@opus 
Message-id: <8801192147.AA00598@opus.Tymnet.com> 
Subject: Rodney - IOCP Scheduler Meeting 

The Rodney-IOCP Background Scheduler interface meeting will be held
on Wednesday, Jan. 20, 1988 at 2:00 PM in Conference Room 1 on the
3rd floor.  Please schedule to attend the meeting.

Received: from F29.Tymnet by B39.Tymnet; Mon, 4 Jan 88 9:45:34 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 4 Jan 88 9:39:39 PST
Return-path: <NTD.K/JONES@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/JONES@EMSNTD.Ontyme.Tymnet 
Date: 04 JAN 88 09:29:19 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I30554@Ontyme.Tymnet 
Subject: New PROFS Extended Mail 

Subject:  New PROFS Extended Mail

     To:  Murphy, Ron
          Soley, Bill

 Attach:

I am sorry that I don't have particulars, but EMMS (December 15, 1987) had the
following information.

There is a new PROFS Extended Mail which provides an X.400 and it sounds like
TCP/IP interface (but not full TCP/IP mailer (RFC822)).  I.e., gateway support
for addresses on TCP/IP networks such as ArpaNet.

Cheers, Ken Jones

[PC ID 20:se301a022:05381]
Received: from tymix.Tymnet by B39.Tymnet; Thu, 7 Jan 88 8:57:47 PST
Received: by tymix.Tymnet (5.51/4.7) id AA29668; Thu, 7 Jan 88 09:01:13 PST
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00269;
	Thu, 7 Jan 88 09:01:10 PST
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA14759; Thu, 7 Jan 88 09:01:07
	PST
Return-path: <pchu@opus> 
From: pchu@opus (Paul Chu) 
Date: Thu, 7 Jan 88 09:01:07 PST 
To: pchu@opus, tsay@opus, wrs@opus 
Message-id: <8801071701.AA14759@opus.Tymnet.com> 
Subject: rsi.h, new events 

Bill,

As i was browsing through the header files, I came accross the rsi.h with
some new additions.

One of the additions is channel B received break, and the event is defined
differently from the "channel A received break".  Since TTY does not
use channel B, so I won't use this event.

However, to satisfy my curiosity, is there a typo in the name of the event
"RSI_DBRK_B" as oppose to "RSI_RBRK_A" ?

Paul
Received: from tymix.Tymnet by B39.Tymnet; Mon, 11 Jan 88 15:15:56 PST
Received: by tymix.Tymnet (5.51/4.7) id AA18004; Mon, 11 Jan 88 15:14:01 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00207; Mon, 11 Jan 88
	15:13:58 PST
Return-path: <dlasater@hobbes> 
From: dlasater@hobbes (Dan Lasater) 
Date: Mon, 11 Jan 88 15:13:58 PST 
To: wrs@hobbes 
Message-id: <8801112313.AA00207@hobbes.Tymnet.com> 
Subject: need for a meeting 

Bill,
Roger Schroeder tells mee there is a need for you and Terry
Doyle to discuss how Rodney will link and at what address and
how different modules will be called.  We also need yuliang and
Rong Chu in the meeting.  Please let me know when such a meeting
can be held.

Thanks
Dan
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 15 Jan 88 16:00:39 PST 
To: WRS@C39.Tymnet 
Subject: "Let me fill in a little... have"... 

Let me fill in a little... have been told whole story, but much meant
little... 1)Mike has left NTD to work for ISG Corp. in business 
planning??!!.. will not relocate, its the job Dick Gunther had...
So Al reorganized.. all software product dev. now under Bill E., Ken
and Alcorn almost unaffected, but new "research" group established
under person I do not Know (Mike Stimak there)... Bill E. told me 
people like you have choice(???!!!) of where you report...(but I
gather only at reasonable time intervals.....)BUT do get info
from the "horse's mouth".........
OH>>> hope you can attend tues @ 2:00.. next "MUD REFORMAT" meeting.. 
Received: from tymix.Tymnet by B39.Tymnet; Tue, 19 Jan 88 11:36:59 PST
Received: by tymix.Tymnet (5.51/4.7) id AA21461; Tue, 19 Jan 88 11:40:33 PST
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA14518;
	Tue, 19 Jan 88 11:40:30 PST
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA29239; Tue, 19 Jan 88 11:40:28
	PST
Return-path: <rogers@opus> 
From: rogers@opus (Roger Schroeder) 
Date: Tue, 19 Jan 88 11:40:28 PST 
To: wrs@opus 
Message-id: <8801191940.AA29239@opus.Tymnet.com> 
Subject: IOCP MTG 

A meeting will be held WED at 2pm in the 3rd floor conf. rm to discuss the
Rodney/scheduler IOCP issues.
Thank You 
ROGER S
Received: from F29.Tymnet by B39.Tymnet; Wed, 20 Jan 88 17:39:55 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 20 Jan 88 17:34:25 PST
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 20 JAN 88 16:56:16 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I38808@Ontyme.Tymnet 
Subject: Career Change 

                  TYMNET -  MDC NETWORK SYSTEMS COMPANY
                         WESTERN FIELD OPERATIONS
                         ________________________


                             
DATE:       January 19, 1987	           

TO:         Tymnet Personnel         		                 

COPIES:     Jim Patty
            Clint DeGabrielle	         

FROM:       Rory Enright            

SUBJECT:    Career Change    	        

REF:        		        


________________________________________________________________________


It is with mixed emotions that I must tell you that I have decided
to make a career change.  I will be leaving Tymnet at the
end of January to join N.E.T. as the Director of Sales Support. This
opportunity to impact a young rapidly expanding company is very
exciting, but it is also  very hard to leave Tymnet. The  last 6 years 
have been the most enjoyable in my career.  In that time, Tymnet has
seen many changes and it's been fun being part of the evolution.

Thanks to the hard work of a lot of people, Tymnet is positioned to
have another good year in 1988. I'll miss the opportunity to be part of
that success, but look forward to hearing about the continued victories.


Regards,


       Rory
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Thu, 21 Jan 88 9:33:42 PST 
To: WRS@C39.Tymnet 
Subject: "Hope to see you at lunch today..."... 

Hope to see you at lunch today... But in case I forget then, what about the
plan to have lunch with Bill Putney of NET???????????
Received: from tymix.Tymnet by B39.Tymnet; Thu, 21 Jan 88 12:00:57 PST
Received: by tymix.Tymnet (5.51/4.7) id AA01501; Thu, 21 Jan 88 12:04:39 PST
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01501; Thu,
	21 Jan 88 12:04:36 PST
Received: by f104.sun.com (3.2/SMI-3.2) id AA09020; Thu, 21 Jan 88 12:04:32 PST
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 21 Jan 88 12:04:32 PST 
To: wrs@f104 
Message-id: <8801212004.AA09020@f104.sun.com> 
Subject: k_clear() 

I solved my timing problem.  I'm adding k_clear() to exec.c.  The
trapsim code doesn't seem to screw current code up but I didn't test multiple
queuing.  I'll add it to the current release.
Ron


Received: from X32.Tymnet by B39.Tymnet; Thu, 21 Jan 88 12:18:09 PST
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Wed, 20 Jan 88 18:38:45 PST 
To: WRS 
Subject: I got an Amiga! 

I don't know if you have heard, but I bought an Amiga 2000.  Nifty colors,
does multitasking, and its video output is compatible with VCRs.

The way tasks talk to each other in AmigaDOS is very similar to another
M68000 system.  See if this sounds familiar:
0) Sender builds a message in its memory.
1) Sender calls routine giving it a pointer to the message.
2) The pointer is put on the linked list on recipient's message port.
3) Recipient gets a signal and/or interrupt that a message is available.
4) Recipient gets message, copies data to its memory, returns message.
5) Pointer to message gets put on original sender's linked list.
6) Original sender is notified.
7) Sender now allowed to free memory allocated to message.

/Joe

Received: from X32.Tymnet by B39.Tymnet; Thu, 21 Jan 88 14:58:24 PST
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Thu, 21 Jan 88 13:02:21 PST 
To: WRS, CARL 
Subject: ARGPAR has problems with TMPCOR 

From (CARL)DIRECT I gave the following command:
*PJ.RCF/INDIRECT=(PJ)*.DAT/TMP:ROL:"ROLAID PJ/S"/RUN:DSK:(XEXEC)ROLAID/RUNOFF:1
and ROLAID said "%no option matching /S".  However, it did correctly pick up
the file name.  When I copied the TMPCOR file to disk, it worked.
  */TMP:FOO:"ROLAID PJ/S"/RUN:DSK:(MPL)PIP ;This line given to (CARL)DIRECT
  *009ROL.TMP=TMP:FOO
  *FOO.FOO=009ROL.TMP^Z
  .CTEST RUN (XEXEC)ROLAID

Examining FOO.FOO with FILDDT showed it to be 4 words long.
  "R","O","L","A","I"
  "D"," ","P","J","/"
  "S",nul,nul,nul,nul
  nul,nul,nul,nul,nul

I guess some sort of end-of-line confusion at end-of-file from TMPCOR.
			/Joe
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 21 Jan 88 15:05:35 PST 
To: <JMS@F29.TYMNET> 
Subject: Re: TUMS-10 bug report (v45000003 of 07-DEC-87) 
In-reply-to: your message of Wed, 20 Jan 88 18:19:56 PST

Thanks - I fixed it in version 45.4 which may be telecopied from (TUMS:39) if
you'd like or it will work its way around via RDIST in a few days.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 21 Jan 88 15:08:34 PST 
To: <JMS@F29.TYMNET> 
Subject: Re: I got an Amiga! 
In-reply-to: your message of Wed, 20 Jan 88 18:38:45 PST

Hmmm sounds very familiar.  Thats nice.  They should have used rings (not
really).  -Bill   Do you have a C compiler?
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 21 Jan 88 15:14:25 PST 
To: ipc.c/achord@ontyme 
Subject: New PROFS Extended Mail 

(begin forwarded message)
----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Mon, 4 Jan 88 9:45:34 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 4 Jan 88 9:39:39 PST
Return-path: <NTD.K/JONES@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/JONES@EMSNTD.Ontyme.Tymnet 
Date: 04 JAN 88 09:29:19 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I30554@Ontyme.Tymnet 
Subject: New PROFS Extended Mail 

Subject:  New PROFS Extended Mail

     To:  Murphy, Ron
          Soley, Bill

 Attach:

I am sorry that I don't have particulars, but EMMS (December 15, 1987) had the
following information.

There is a new PROFS Extended Mail which provides an X.400 and it sounds like
TCP/IP interface (but not full TCP/IP mailer (RFC822)).  I.e., gateway support
for addresses on TCP/IP networks such as ArpaNet.

Cheers, Ken Jones

[PC ID 20:se301a022:05381]
----------------------------------------------------------------
(end forwarded message)

Might want to check this out before reinventing the wheel.  If you could get
source code, you could unbundle the SMTP from the TCP/IP.  TCP/IP is the
ArpaNet transport protocol (Transport Control Protocol) and the link protocol
(Internet Protocol).  Then again, sometimes its easier to write your own code
than to kludge somebody elses into doing something for which it was not
designed.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Thu, 21 Jan 88 15:19:46 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 21 Jan 88 15:18:53 PST
Return-path: <NTD.W/EUSKE@EMSNTD.Ontyme.Tymnet> 
From: NTD.W/EUSKE@EMSNTD.Ontyme.Tymnet 
Date: 21 JAN 88 15:16:36 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I39336@Ontyme.Tymnet 
Subject: ABSENCE FROM OFFICE 
ABSENCEFROMOFFICE: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      21 JAN 88  15:15                  Network Technology Development

TO>        NTD PERSONNEL

COPIES>    AL FENN

FROM>      BILL EUSKE


SUBJECT>   ABSENCE FROM OFFICE


-----------------------------------------------------------------------


I will be away from the office, in France then St. Louis, from
Saturday, January 23, 1988 to Monday, February 8, 1988, returning
to work on Tuesday, February 9, 1988.

In my absence Dan Lasater will be acting in my stead.  Please direct all
questions or problems to him.

Thank you for your cooperation.

Bill Euske
Received: from F29.Tymnet by B39.Tymnet; Thu, 21 Jan 88 17:01:00 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 21 Jan 88 17:05:09 PST
Return-path: <NTD.BASELINE@EMSNTD.Ontyme.Tymnet> 
From: NTD.BASELINE@EMSNTD.Ontyme.Tymnet 
Date: 21 JAN 88 16:32:12 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I39390@Ontyme.Tymnet 
Subject: :NEW PRODUCT ID; DTWO 

     
     
     
     
     
     
     
                     TYMNET/NETWORK TECHNOLOGY DEVELOPMENT  
                         SOFTWARE DISTRIBUTION SUPPORT 
     
                              M E M O R A N D U M 
     
     
     
   date:   January 21, 1988   
     to:   Pat Manley    
     cc:   Jerry Messina 
           Mitchell Ai-Chang  
   from:   Sheldon Cohen 
subject:   New Product ID - DTWO   
     
     
     
The following new product ID has been included    
into the appropriate NTD databases;
     
Prod ID   Description                             Supervisor Hex Code 
-------   -------------------                     ------------------- 
 DTWO     Turbo-Engine: GP/FS/IOCP Post Level 2      none required    
     
     
    Developer:  Pat Manley    
Marketing Mgr:  Jerry Messina 
     
     
     
     
Sheldon Cohen  (408/922-6144) 
Baseline Manager  NTD.S/COHEN  (B5-1)   
     
<
Received: from tymix.Tymnet by B39.Tymnet; Fri, 22 Jan 88 11:44:29 PST
Received: by tymix.Tymnet (5.51/4.7) id AA03601; Fri, 22 Jan 88 11:48:52 PST
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09281;
	Fri, 22 Jan 88 11:48:47 PST
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA18271; Fri, 22 Jan 88
	11:48:43 PST
Return-path: <rmurphy@calvin> 
From: rmurphy@calvin (Ron Murphy) 
Date: Fri, 22 Jan 88 11:48:43 PST 
To: wrs@calvin 
Message-id: <8801221948.AA18271@calvin.Tymnet.com> 
Subject: glob.ramLo and glob.ramHi 

How are we going to set these fields?
Received: from tymix.Tymnet by B39.Tymnet; Fri, 22 Jan 88 14:20:42 PST
Received: by tymix.Tymnet (5.51/4.7) id AA03949; Fri, 22 Jan 88 14:25:00 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10866; Fri, 22 Jan 88
	14:24:51 PST
Return-path: <changc@hobbes> 
From: changc@hobbes (Ching Chang) 
Date: Fri, 22 Jan 88 14:24:51 PST 
To: changc@hobbes, wrs@hobbes 
Message-id: <8801222224.AA10866@hobbes.Tymnet.com> 
Subject: Reschedule Meeting 

Bill :

Tom Clark called in, he hopes to reschedule the Monday (1/25) 3 pm
meeting. Could we meet at 1 pm Monday (1/25) ?

My phone number is 922-6045. 


Ching Chang

Received: from tymix.Tymnet by B39.Tymnet; Fri, 22 Jan 88 14:33:22 PST
Received: by tymix.Tymnet (5.51/4.7) id AA03978; Fri, 22 Jan 88 14:37:46 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10984; Fri, 22 Jan 88
	14:37:42 PST
Return-path: <changc@hobbes> 
From: changc@hobbes (Ching Chang) 
Date: Fri, 22 Jan 88 14:37:42 PST 
To: wrs@hobbes 
Message-id: <8801222237.AA10984@hobbes.Tymnet.com> 
Subject: Re-schedual Meeting 


Bill :

Tom Clark called in, he hopes to reschedual Monday (1/25) 3pm meeting.
Could we meet at 1pm Monday (1/25) ?

My phone number is 922-6045.


Ching Chang
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 22 Jan 88 15:08:26 PST 
To: <changc@hobbes> 
Subject: Re: Reschedule Meeting 
In-reply-to: <8801222224.AA10866@hobbes.Tymnet.com> of Fri, 22 Jan 88 14:24:51
	PST

Actually, I'd like to make it Tuesday if possible.  There is a group going to
Sun on Monday to look at some new software and I'l like to go if possible.  It
is not critical, though, so if it is too difficult to reschedule, don't bother.
Any time in the afternoon is okay with me on Tuesday, and well, Monday, too.
Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 22 Jan 88 15:23:06 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04073; Fri, 22 Jan 88 15:27:33 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA11392; Fri, 22 Jan 88
	15:27:30 PST
Return-path: <changc@hobbes> 
From: changc@hobbes (Ching Chang) 
Date: Fri, 22 Jan 88 15:27:30 PST 
To: wrs@hobbes 
Message-id: <8801222327.AA11392@hobbes.Tymnet.com> 
Subject: Mex 


Received: from F29.Tymnet by B39.Tymnet; Fri, 22 Jan 88 15:30:34 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 22 Jan 88 15:31:59 PST
Return-path: <NTD.L/JOHNSON@EMSNTD.Ontyme.Tymnet> 
From: NTD.L/JOHNSON@EMSNTD.Ontyme.Tymnet 
Date: 22 JAN 88 15:13:24 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I39924@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS INFORMATION"... 

 
 
 
 
 
                         MCDONNELL DOUGLAS
                     INFORMATION SYSTEMS GROUP
                      INTEROFFICE MEMORANDUM
 
 Network Systems Company            Network Technology Development
 
 
DATE>      22 JAN 88  15:08

TO>        NTD
            MARKETING
           QSATS

COPIES>    

FROM>      LANA VAYSBURD


SUBJECT>   OUT-OF-THE-OFFICE


-----------------------------------------------------------------------


I WILL BE OUT OF THE OFFICE FROM 1/23/88 THROUGH 2/8/88.
PLEASE CONTACT THE APPROPRIATE SECTION MANAGER WITHIN MY DEPARTMENT,
NETWORK COMMUNICATIONS INTERFACES, WHILE I AM AWAY.

AVI MARGALIT WILL BE THE ACTING MANAGER OF THE ASYNC INTERFACE
SECTION.

REGARDS,

LANA VAYSBURD
Received: from tymix.Tymnet by B39.Tymnet; Sun, 24 Jan 88 20:30:33 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04895; Sun, 24 Jan 88 20:34:51 PST
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01709; Sun,
	24 Jan 88 20:34:46 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04886; Sun, 24 Jan 88 20:34:34 PST
Received: by niagara.Tymnet.com (3.2/SMI-3.2) id AA11567; Sun, 24 Jan 88
	20:23:52 PST
Return-path: <niagara!wrs@tymix.Tymnet> 
From: niagara!wrs (Bill Soley) 
Date: Sun, 24 Jan 88 20:23:52 PST 
To: feldman@tymix, seltzer@tymix, wrs@tymix 
Message-id: <8801250423.AA11567@niagara.Tymnet.com> 
Subject: Sun trip 

What time?  If I need to be at Tymnet before 11:30 AM, please call me at
home and let me know with as much notice as possible.  Thanks -Bill

Received: from F29.Tymnet by B39.Tymnet; Mon, 25 Jan 88 14:11:02 PST
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Mon, 25 Jan 88 14:09:39 PST 
To: <WRS@B39.Tymnet> 
Subject: Re: I got an Amiga! 
In-reply-to: your message of Thu, 21 Jan 88 15:08:34 PST

The original development for the Amiga was done on a Sun system using the
Greenhill compiler and a thing called BillBoard to downline load programs.
I don't have a C compiler yet.  All the include files are for Lattice-C,
but some people like Manx (Aztec) C better.  I haven't decided which one to
buy.  I hear there is a board and software for the Amiga to allow it to
run NFS.  I bought the assembler, which appears to be a superset of ASM68K.
		/Joe
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 26 Jan 88 3:08:14 PST 
To: <JMS@F29.TYMNET> 
Subject: Re: I got an Amiga! 
In-reply-to: your message of Mon, 25 Jan 88 14:09:39 PST

I have a lot of respect for the Amiga, I'm sure you'll have fun with it.
I used Aztec C on a Z-80 system and was pretty impressed.  I've also used
Lattice on a MS-DOS and was not terribly impressed.  But then that's MS-DOS
and both of these were old versions (i.e. about 4 years ago).  -Bill
Received: from X32.Tymnet by B39.Tymnet; Tue, 26 Jan 88 7:57:52 PST
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Tue, 26 Jan 88 0:59:38 PST 
To: jms, William R Soley <WRS@B39.Tymnet> 
Subject: ARGPAR 

I will not swear, but I think RDIST may be using (WRS)ARGPAR and there
were fixes to some TMPCOR prolems in the (SAILIB)ARGPAR version.  I know
I fixed some problems with ARGPAR, but I don't know which version Bill
is now using.

/Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 26 Jan 88 11:28:59 PST 
To: <NTD.S/BLACK@EMSNTD.Ontyme.Tymnet> 
Subject: Re: "Bill, I will take care of the"... 
In-reply-to: I41082@Ontyme.Tymnet of 26 JAN 88 09:58:44

Thank you, time accounting shall follow shortly...-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 26 Jan 88 11:32:48 PST 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 3 Jan 88

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

20	3

31	8

46	20	1915	2302

12	8

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 26 Jan 88 11:32:50 PST 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 10 Jan 88

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

13	40

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 26 Jan 88 11:32:51 PST 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 17 Jan 88

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

13	40

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 26 Jan 88 11:32:51 PST 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 24 Jan 88

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

31	8

32	4

20	3

46	29	1915	2302

------------------------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Tue, 26 Jan 88 15:52:15 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 26 Jan 88 15:50:40 PST
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 26 JAN 88 15:31:36 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I41358@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS INFORMATION"... 

 
 
 
 
 
                         MCDONNELL DOUGLAS
                     INFORMATION SYSTEMS GROUP
                      INTEROFFICE MEMORANDUM
 
 Network Systems Company            Network Technology Development
 
 
DATE>      22 JAN 88  15:08

TO>        NTD
            MARKETING
           QSATS

COPIES>    

FROM>      LANA VAYSBURD


SUBJECT>   OUT-OF-THE-OFFICE


-----------------------------------------------------------------------


I WILL BE OUT OF THE OFFICE FROM 1/23/88 THROUGH 2/8/88.
PLEASE CONTACT THE APPROPRIATE SECTION MANAGER WITHIN MY DEPARTMENT,
NETWORK COMMUNICATIONS INTERFACES, WHILE I AM AWAY.

AVI MARGALIT WILL BE THE ACTING MANAGER OF THE ASYNC INTERFACE
SECTION.

REGARDS,

LANA VAYSBURD
Received: from F29.Tymnet by B39.Tymnet; Wed, 27 Jan 88 7:23:49 PST
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Wed, 27 Jan 88 7:23:06 PST
Return-path: <IPC.R/SWANSON@EMSTUMS.Ontyme.Tymnet> 
From: IPC.R/SWANSON@EMSTUMS.Ontyme.Tymnet 
Date: 27 JAN 88 14:58:29 
To: TUMS.SUP@EMSTUMS.Ontyme.Tymnet 
Message-id: A55204@Ontyme.Tymnet 
Subject: "TO: ALL ISG BUSINESS UNITS SUBJECT:"... 



TO:       ALL ISG BUSINESS UNITS

SUBJECT:  ISG WORD PROCESSING CONVERSION SERVICE


    ISG is now offering Word Processing Document conversions.  This
service is being performed in the St. Louis Computer Center.  Current
software conversions include the following:

                    WANG 8" to MULTIMATE
                    WANG 8" to IBM DISPLAYWRITER
                    WANG 8" to DISPLAYWRITE III
                    WANG 5 1/4" to MULTIMATE
                    MULTIMATE to WANG 8"
                    MULTIMATE to IBM DISPLAYWRITER
                    MULTIMATE to XEROX 860
                    IBM DISPLAYWRITER to WANG 8"
                    IBM DISPLAYWRITER to MULTIMATE


   Software pairings (single-direction-links between incompatibles)
are readily available for virtually all major Word Processing
packages and dedicated systems on the market today.  Additional
pairings will be ordered as needed.  A total list of the available
software pairings for conversions can be aquired from the location
below.


    For further information reguarding the conversion service,  you
may contact:

                         ISG CONVERSION SERVICE
                         MAIL STOP: 307/2E/L237/D15
                         Phone:  314-233-8250
Received: from tymix.Tymnet by B39.Tymnet; Wed, 27 Jan 88 9:12:11 PST
Received: by tymix.Tymnet (5.51/4.7) id AA17102; Wed, 27 Jan 88 09:11:45 PST
Received: from mccloud.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id
	AA17625; Wed, 27 Jan 88 09:11:40 PST
Received: by mccloud.Tymnet.com (3.2/SMI-3.2) id AA01001; Wed, 27 Jan 88
	09:11:37 PST
Return-path: <driscoll@mccloud> 
From: driscoll@mccloud (Pat Driscoll) 
Date: Wed, 27 Jan 88 09:11:37 PST 
To: wrs@mccloud 
Message-id: <8801271711.AA01001@mccloud.Tymnet.com> 
Subject: TUMS 

How do I send mail to ONTYME from UNIX?

Pat

From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Wed, 27 Jan 88 9:54:12 PST 
To: WRS@C39.Tymnet 
Subject: "Bill Putney... NET.. Lunch set"... 

Bill Putney... NET.. Lunch set for fri.,1/29 @ 11:45 @ Scott's in
Palo Alto... need to leave here ~ 11:15... suggest you meet me here
at my office And I'll drive up.. Please confirm.......
Received: from tymix.Tymnet by B39.Tymnet; Wed, 27 Jan 88 11:10:57 PST
Received: by tymix.Tymnet (5.51/4.7) id AA17702; Wed, 27 Jan 88 11:10:26 PST
Received: from mccloud.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id
	AA18424; Wed, 27 Jan 88 11:10:22 PST
Received: by mccloud.Tymnet.com (3.2/SMI-3.2) id AA01118; Wed, 27 Jan 88
	11:10:18 PST
Return-path: <driscoll@mccloud> 
From: driscoll@mccloud (Pat Driscoll) 
Date: Wed, 27 Jan 88 11:10:18 PST 
To: rodney@mccloud 
Message-id: <8801271910.AA01118@mccloud.Tymnet.com> 
Subject: X409 Load/Dump Image change 

The RED chapter on Binary Load/Dump File X.409 Format Specification has changed as follows:

1> patronType is renamed imageType

2> a BOOLEAN called patronFlag has been added to indicate whether the image is of a
   Patron.  ISIS, for example, is a patron, while an ISIS slot is NOT a patron.

3> PatronClass is renamed ImageClass

The Rodney bootstrap needs to make the following checks before loading a file:

  - that the patronFlag is true (i.e. nonzero)
  - that the loadFlag is true (i.e. nonzero)

If either flag is false then the load operation is aborted and a log message must be sent.
For dumps, the Rodney bootstrap sets the fields as follows:

   - loadFlag false (i.e. zero)
   - patronFlag true
   - imageType 0 (unless the patron type can otherwise be determined)

Comments?

Pat
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 27 Jan 88 11:21:46 PST 
To: DLASATER@B39.Tymnet 
Subject: Re: TUMS-10 bug report (v45000004 of 21-JAN-88) 
In-reply-to: your message of Wed, 27 Jan 88 7:36:03 PST

I can send it to your account on the Sun, then you'd never have any string
garbage collect problems again.  Let me know -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 27 Jan 88 11:22:36 PST 
To: <driscoll@mccloud> 
Subject: Re: TUMS 
In-reply-to: <8801271711.AA01001@mccloud.Tymnet.com> of Wed, 27 Jan 88 09:11:37
	PST

Just use an address like
	NTD.B/SOLEY@Ontyme
where you would normally use the user name.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 27 Jan 88 11:23:45 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: "Bill Putney... NET.. Lunch set"... 
In-reply-to: your message of Wed, 27 Jan 88 9:54:12 PST

Confirmed
CU 11:15 in your office on Friday.  -Bill
From: Postmaster@B39.Tymnet 
Date: Wed, 27 Jan 88 11:35:22 PST 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No router table entry for "MCCLOU.Q".



----------
RCPT To: <driscoll@mccloud> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 27 Jan 88 11:22:36 PST 
To: <driscoll@mccloud> 
Bcc: WRS@B39.Tymnet 
Subject: Re: TUMS 
In-reply-to: <8801271711.AA01001@mccloud.Tymnet.com> of Wed, 27 Jan 88 09:11:37
	PST

Just use an address like
	NTD.B/SOLEY@Ontyme
where you would normally use the user name.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Wed, 27 Jan 88 14:43:45 PST
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 27 Jan 88 14:27:36 PST
Return-path: <TYMOPS.V/STAFFORD@EMSNTD.Ontyme.Tymnet> 
From: TYMOPS.V/STAFFORD@EMSNTD.Ontyme.Tymnet 
Date: 27 JAN 88 14:23:48 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: M43757@Ontyme.Tymnet 
Subject: La Roy Tymes 

Hello Bill,

How's the family doing? I guess by now you get a full nights sleep?

I need some info if you have it. Do you happen to know of any usernames that
should have their passwords changed or at least be changed themslelves that
would have belonged to La Roy Tymes. I understand he is terminating
as of the 29th (Friday). I will not cancel anything without thoroughly
researching....

If you know of any, I would appreciate your input.

Thanks,

Vida
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 28 Jan 88 0:00:25 PST 
To: <TYMOPS.V/STAFFORD@EMSNTD.Ontyme.Tymnet> 
Subject: Re: La Roy Tymes 
In-reply-to: M43757@Ontyme.Tymnet of 27 JAN 88 14:23:48

Only his personal account that I know of.  Its all academic with his
knowledge of the network he hardly needs a password.  Network control
needs to be notified to take away his probe name if any and TMCS.  I
can't think of anything else.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 28 Jan 88 0:02:53 PST 
To: <feldman@comet> 
Subject: Re: DH gateways 
In-reply-to: <8801272014.AA19503@comet.Tymnet.com> of Wed, 27 Jan 88 12:14:01
	PST

I'm not sure I understand what you mean about using the gateway without the
Sun.  Part of my confusion, no doubt, comes from a lack of understanding of
exactly what 1822 is.  Is it the data link protocol (i.e. as opposed to HDLC)
or is it the physical interface (i.e. as opposed to RS-422) or some combination?

Its interesting because the Sun price list says that DDN Sun-Link software will support 1822 with or without an MCP implying that it will do so directly
without any box, yet Bill Barns says Sun said it wouldn't work.  ??????

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 9:31:33 PST
Received: by tymix.Tymnet (5.51/4.7) id AA23429; Thu, 28 Jan 88 09:31:10 PST
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA24029; Thu,
	28 Jan 88 09:30:56 PST
Received: by tymix.Tymnet (5.51/4.7) id AA23421; Thu, 28 Jan 88 09:30:54 PST
Received: by niagara.Tymnet.com (3.2/SMI-3.2) id AA15716; Thu, 28 Jan 88
	09:25:59 PST
Return-path: <niagara!wrs@tymix.Tymnet> 
From: niagara!wrs (Bill Soley) 
Date: Thu, 28 Jan 88 09:25:59 PST 
To: rodney@hobbes 
Message-id: <8801281725.AA15716@niagara.Tymnet.com> 
Subject: k_berrBegin() meeting 


The meeting will be in the 5th floor conference room at 2:00 today.

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 10:14:28 PST
Received: by tymix.Tymnet (5.51/4.7) id AA23525; Thu, 28 Jan 88 10:13:56 PST
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA20258; Thu, 28 Jan 88 10:13:52
	PST
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Thu, 28 Jan 88 10:13:52 PST 
To: WRS@B39.Tymnet.com 
Message-id: <8801281813.AA20258@comet.Tymnet.com> 
Subject: Re: DH gateways 

I believe that 1822 specifies both the physical and the data link protocol,
though I could be mistaken.  In any event, there seem to be two options
to 1822, called "LH" (Local Host) and "DH" (Distant Host) which determine
the physical line characteristics.  I would guess that neither LH or DH
are compatible with RS232 or RS423.

There seems to be a variant of 1822 called 1822-J, which specifies the
HDH options.  This seems to run over normal serial lines such as RS232.
As a result, HDH interfaces seem to be less expensive.

I am really guessing all of this based on some catalogs i've seen,
but it seems to make sense.
	steve
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:07:11 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24565; Thu, 28 Jan 88 16:10:46 PST
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00615; Thu, 28 Jan 88
	16:10:40 PST
Return-path: <pchu@hobbes> 
From: pchu@hobbes (Paul Chu) 
Date: Thu, 28 Jan 88 16:10:40 PST 
To: pchu@hobbes, wrs@hobbes 
Message-id: <8801290010.AA00615@hobbes.Tymnet.com> 
Subject: k_sprout, parameter type for mainArg 

Bill,

The type defined in the exec.c for this parameter is "long",
however, it is described in the Rodney GED as u_long.
TYhought you want to know.  paul
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:23:51 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24380; Thu, 28 Jan 88 15:07:07 PST
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA03718; Thu, 28 Jan 88 15:07:04
	PST
Return-path: <dencoff@opus> 
From: dencoff@opus (Dennis Coffey) 
Date: Thu, 28 Jan 88 15:07:04 PST 
To: Postmaster@B39.Tymnet.com 
Message-id: <8801282307.AA03718@opus.Tymnet.com> 
Subject: Re: Returned MAIL 

?
From: Postmaster@B39.Tymnet 
Date: Thu, 28 Jan 88 16:34:49 PST 
To: <WRS@B39.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    .

  The error code and message was:
    554 sendall: too many hops (17 max)

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <wrs@buffalo>

----------
RCPT To: <wrs@buffalo> 
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:33:58 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24714; Thu, 28 Jan 88 16:37:16 PST
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:32:38 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24703; Thu, 28 Jan 88 16:35:57 PST
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:31:14 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24694; Thu, 28 Jan 88 16:34:40 PST
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:30:05 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24688; Thu, 28 Jan 88 16:33:39 PST
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:29:03 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24681; Thu, 28 Jan 88 16:32:36 PST
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:28:01 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24667; Thu, 28 Jan 88 16:31:35 PST
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:27:00 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24651; Thu, 28 Jan 88 16:30:37 PST
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:26:04 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24644; Thu, 28 Jan 88 16:29:45 PST
Received: from tymix.Tymnet by B39.Tymnet; Thu, 28 Jan 88 16:25:07 PST
Received: by tymix.Tymnet (5.51/4.7) id AA24639; Thu, 28 Jan 88 16:28:50 PST
Return-path: <WRS@B39.Tymnet> 
From: WRS@B39.Tymnet 
Date: Thu, 28 Jan 88 16:24:13 PST 
To: wrs@buffalo 
Message-id: <8801290028.AA24639@tymix.Tymnet> 

hello test
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 28 Jan 88 22:56:33 PST 
To: <IPC.PROFS@EMSTUMS.Ontyme.Tymnet> 
Subject: Re: Tele-Fax Number 
In-reply-to: A56233@Ontyme.Tymnet of 29 JAN 88 05:30:28

Okay - thanks for fax number - the dioagaram I had was rendered void by some new
information so I will send it along later after I get it uypdated.  I can type,
really.  -Bill
 c@R