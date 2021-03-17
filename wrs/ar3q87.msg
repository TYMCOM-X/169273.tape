Received: from F29.Tymnet by B39.Tymnet; Wed, 1 Jul 87 2:20:18 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Wed, 1 Jul 87 9:22:10 UT 
To: MAIL 
Subject: Req #8050 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #8050 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  01-Jul-87 00:00:00
; Actual run: 01-Jul-87 00:00:06
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from tymix.Tymnet by B39.Tymnet; Wed, 1 Jul 87 4:50:31 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05300; Wed, 1 Jul 87 04:52:22 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04195;
	Wed, 1 Jul 87 04:52:15 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA03781; Wed, 1 Jul 87 04:52:17
	PDT
Return-path: <wrs@calvin> 
From: wrs@calvin (Bill Soley) 
Date: Wed, 1 Jul 87 04:52:17 PDT 
To: rodney@calvin 
Message-id: <8707011152.AA03781@calvin.Tymnet.com> 
Subject: RED and change bars 

Change bars are now automatic in RED, there is no need for the .Cb and
.Ce macros.  The SCCS data is used to produce them.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 1 Jul 87 14:55:00 PDT 
To: feldman@comet 
Subject: module symbols 

(begin forwarded message)
----------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 1 Jul 87 14:32:54 PDT 
To: driscoll@calvin 
Subject: module symbols 

/*
 * Pat- Please review before I distribute. -Bill
 */

Naming symbols inside Rodney should follow some agreed upon convention to
make things more consistent and less confusing.  The following is favored
by Pat and myself:

System calls begin with some functional group identifier as a prefix, but
do not have an underscore, for example:
	slgReport, ipcfEnter, symEnter, etc.

Symbols which are internal to a module (and declared static) can be named
as appropriate.  Symbols which are global to Rodney, but not available to
the patron, should be named with a short identifer of the module, followed
by an underscore, for example:
	k_reset, l_crash, s_zap, etc.
The modules would be:
	a_	agent
	b_	boot
	i_	ipcf
	k_	kernel
	l_	logger
	s_	session
	ss_	skysap
One character should be used unless it conflicts or is confusing.  The
first character following the _ is capitalized as if the prefix was not
present, for example:
	int k_scaler;
	struct k_Structure
	#define k_CONSTANT

Things which transcend modules or are frequently used and well known need
not have a module prefix, for example:
	glob, RERR_FOO, etc.
----------------------------------------------------------------
(end forwarded message)

Steve, can I get your comments on this, too, even though it doesn't
apply to patrons.  -Bill
From: Postmaster@B39.Tymnet 
Date: Thu, 2 Jul 87 0:07:47 UT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
User has not read mail in 30 days: CHANG
Last logout: 23 Mar 87


----------
RCPT To: <chang> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 2 Jun 87 1:09:17 PDT 
To: chang 
Bcc: WRS@B39.Tymnet 
Subject: comments on AUS566.RPT 

Finally...

>	(DCHANG:39)AUS566.RPT	- 5/28/87

>	140,000 entries.  Currently, the MUD in the public net
>	contains about 10,000 entries, which represents 23%.

Actually, as of 19-Dec-86, the public net MUD had 98,572 entries.  I also
don't understand why two entries are required per port.

----

>	* AUSTPAC may need to administer their address with one
>	facility.  Currently, NSHS is used for the existing network,
>	and NETVAL will be used in the new network.  It seems that
>	in order to provide an automatic update feature, a permanent
>	circuit is required between NSHS and NETVAL with an established
>	protocol.

They could use the NCUD1 protocol, perhaps with minor changes.

----

>	* Does AUSTPAC use 3-digit DNICs (or pseudo-DNICs) for access to
>	other networks (eg. 961, 933, 944; see EPSN 98, p3)?

If so, no problem, just validate 10 names each.

----

>	* There are many address-handling options, including those
>	listed in section 5.6 p10 and in EPSN p2 ('mandatory',
					     ^
					     Which ESPN?

----

>	* EPSN 84, section 6.2, p7 briefly describes charging behavior
>	and implies that NUA may be used as a called address.  Is it
>	not the function of the NUA to represent a NUI in a X.121
>	address form in the call request?

I think all this means is that we have to present the actual port address
in accounting records regardless of what method was used to address it.
This would be available with the "symbolic address in access profile" project.

----

>	5.6.6.1.2	WC
>	The AUSTPAC numbering plan, as it applies to the numbering of ports and
>	terminals in the network, Network User Identifiers (NUI), and Closed
>	Users Group (CUG), will mainly be supported by the Supervisor via the
>	Master User Directory (MUD).  The Protocol Identifiers (PI) will be

CUG in MUD?

----

>	5.6.6.1.7	FC
>	In Tymnet, the address table is centrally located at the Supervisor as
>	the MUD, which is a hash table in a disk.  No pre-allocation of table
>	space is required to accomodate unused address ranges.

There is also an address table necessary in each interface to know which port
to route to on various addresses.  This is probably statically allocated in
the TYM file, but not sure.

----

>	5.6.6.1.17	FC/WC/MC

>	FC/WC (vi) Facilities to specify on a per port basis transit barring is
>	provided at the X.25/X.75 interface.  With the implementation of the
>	Symbolic Addressing project, transit barring (barring calls based on
>	the originating network and terminating network) will be supportable
>	at the Supervisor level via the MUD.  It is assumed that transit
>	barring refers to immediate neighbor networks.

Its not clear how this would work since we have not defined the behavoir of
multiple transits, yet.  I think it would only be on the basis of the origin
and destination rather than the neighbor networks.

----

>	(2) the originating interface indicates "NUI override", and

What does "NUI override" mean in Tymnet?

----

>	The analysis also includes certain address-handling options that are
>	handled by the Supervisor:
>	(1) CUG:  The originating interface will send the requested CUG

Sup handle CUG?

----

>	(iv) Details of the symbolic address handling provided to satisfy the
>	requirements stated in paragraph 5.6.6.1.15.

We need "symbolic address in access profile" for this, I think.

----

I'm confused about NUA/NUI processing, as well.

----

>	According to EPSN 98, "an NUI may be defined on a per port basis to
>	be either 'mandatory', 'optional', or 'forbidden'".  This will require
>	special customization.  One method of implementation is to have the
>	defintion of the port be sent to the Supervisor for analysis.  

I think this might be able to be done without any customization by careful
planning of the access profiles.  I need to understand NUI/NUA stuff better
to be sure, though.

----

In general: very nicely done - stated plainly and accurately.  I should have
comments on Omar's response soon.  Probably Wednesday evening.

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 2 Jul 87 8:37:12 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01353; Thu, 2 Jul 87 08:41:04 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA07636; Thu, 2 Jul 87 08:41:04
	PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Thu, 2 Jul 87 08:41:04 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707021541.AA07636@calvin.Tymnet.com> 
Subject: Re: module symbols 

Looks okay.  This should be included with the other naming
conventions as well before distribution.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Thu, 2 Jul 87 9:38:54 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00191; Thu, 2 Jul 87 09:42:46 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA17155; Thu, 2 Jul 87 09:23:27
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Thu, 2 Jul 87 09:23:27 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707021623.AA17155@comet.Tymnet.com> 
Subject: Re: module symbols 

it looks reasonable.
	steve
Received: from F29.Tymnet by B39.Tymnet; Thu, 2 Jul 87 15:23:24 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 2 Jul 87 15:27:18 PDT
Return-path: <NTD.D/BLISS@EMSNTD.Ontyme.Tymnet> 
From: NTD.D/BLISS@EMSNTD.Ontyme.Tymnet 
Date: 02 JUL 87 15:17:20 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I54993@Ontyme.Tymnet 
Subject: h/w Engineering Meeting 

        Bill:

        Received your message regarding the conflict with to  July  7  meeting.
        This  meeting is now rescheduled to July 9, 9:00 AM to 12:00 noon, here
        in Bldg.  H

        Dave
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 2 Jul 87 17:48:20 PDT 
To: davarj@calvin (Jonathan Davar) 
Subject: Re: k_terminate 
In-reply-to: <8707020127.AA06928@calvin.Tymnet.com> of Wed, 1 Jul 87 18:27:32
	PDT

>
>    What happens when a process calls k_terminate with its own pid?
>
The process is terminated as if it had returned from its main function, or
some other process did a k_terminate().  In fact, if the PID is NULL, the
PID of the current process is assumed so you don't have to look it up to
kill yourself.

-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 2 Jul 87 17:50:55 PDT 
To: NTD.D/BLISS@EMSNTD.Ontyme.Tymnet 
Subject: Re: h/w Engineering Meeting 
In-reply-to: I54993@Ontyme.Tymnet of 02 JUL 87 15:17:20

Okay - I've confirmed the 9th at 9:00.  Thanks for being flexible.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 3 Jul 87 14:52:44 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07455; Fri, 3 Jul 87 14:56:35 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA17747;
	Fri, 3 Jul 87 14:56:19 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11646; Fri, 3 Jul 87 14:56:29
	PDT
Return-path: <ebuell@calvin> 
From: ebuell@calvin (Eric Buell) 
Date: Fri, 3 Jul 87 14:56:29 PDT 
To: ebuell@calvin, wrs@calvin 
Message-id: <8707032156.AA11646@calvin.Tymnet.com> 
Subject: "Bill, My concern about process"... 

Bill,

My concern about process trees is that if the Session Services have a session
active for SKYSAP and the process within SKYSAP that initiated the session
goes away, I would like to have the processes within the Session Services
go away also.  Actually, a much safer solution would be for the Session
Services to be informed of loss of process and be able to release the other
end of the connection before terminating.  The ability to be imformed if
a board (TSAP) goes away (Connection Oriented mode) already exists, it only
seems fair that the same feature be available for internal processes.
ISIS handles this situation for slots (a zapper is sent to the remote.

If you feel that this is not necessary or is too difficult a feature to
provide that's OK with me, I'm only trying to provide as much protection
and robustness for the Session Services (and Rodney) as possible.

Eric
Received: from F29.Tymnet by B39.Tymnet; Sat, 4 Jul 87 0:39:44 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Sat, 4 Jul 87 7:43:51 UT 
To: MAIL 
Subject: Req #8173 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #8173 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  04-Jul-87 00:00:00
; Actual run: 04-Jul-87 00:00:02
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 4 Jul 87 23:56:09 PDT 
To: coons@opus (John Coons) 
Subject: Re: UNIX documentation 
In-reply-to: <8707032110.AA22545@opus.Tymnet.com> of Fri, 3 Jul 87 14:10:04 PDT

Xerox printing is pretty expensive.  I doubt that would be cost effective.
Also, the tutorials and full reference manuals for such things as SCCS,
lint, make, MM, etc. are not part of the online documentation, that I know
of.  I think $200 a copy is less than we can make it for if you consider
the fully burdened cost and the paper and all.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 5 Jul 87 0:01:36 PDT 
To: ebuell@calvin (Eric Buell) 
Subject: Re: "Bill, My concern about process"... 
In-reply-to: <8707032156.AA11646@calvin.Tymnet.com> of Fri, 3 Jul 87 14:56:29
	PDT

A SKYSAP process should never "disappear" without doing some call to the
session layer to tell it to clean up.  Just killing the process is not
enough, even if you had that feature, because you'll still need to free
malloc'd memory and probably update your data structures.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 6 Jul 87 18:04:16 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05749; Mon, 6 Jul 87 18:08:15 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA21921;
	Mon, 6 Jul 87 18:07:53 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14327; Mon, 6 Jul 87 18:08:16
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 6 Jul 87 18:08:16 PDT 
To: wrs@calvin 
Message-id: <8707070108.AA14327@calvin.Tymnet.com> 
Subject: testing 

Regarding testing - I am writting a test plan for Boot. I talked to
Ron and Yu-Liang and I am getting an impression no special code will
be needed on my part to test the boot (for example). Do you share this
opinion? Yu-L. suggests that all those famous waits,sleeps,alarms...
will be transparent to me under Unix.?.
I realize I may simplify and modify the simulated file loading/dumping
in agreement with file server, but is it all I have to think about?
Am I right?
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 6 Jul 87 18:09:02 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: testing 
In-reply-to: <8707070108.AA14327@calvin.Tymnet.com> of Mon, 6 Jul 87 18:08:16
	PDT

I don't feel comfortable commenting on test plans since I really don't
think I have a good feel for what is needed.  Somebody needs to do something
to simulate a file server for you, either the real one or a fake one.  Then
you should write some scripts to test the basic operations of the boot
under the simulator.  Its difficult for me to say what needs to be written
without being inside the code, and without knowing really what is expected.
Personally I feel that detailed testing is best left until later when more
things are working and we can test on a real machine, but this view is not
sharred by management.  I suggest talking with Pat.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 7 Jul 87 7:24:19 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07865; Tue, 7 Jul 87 07:03:14 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA22490;
	Tue, 7 Jul 87 07:02:50 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA27404; Tue, 7 Jul 87 07:03:16
	PDT
Return-path: <jwillott@opus> 
From: jwillott@opus (Jim Willott) 
Date: Tue, 7 Jul 87 07:03:16 PDT 
To: wrs@hobbes 
Message-id: <8707071403.AA27404@opus.Tymnet.com> 
Subject: Re: disk space? 

Thanks for the info.  The DXF format has changed between 1.8 and 2.5, and
between 2.5 and 2.6.

Jim
Received: from F29.Tymnet by B39.Tymnet; Tue, 7 Jul 87 8:16:06 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 7 Jul 87 8:15:09 PDT
Return-path: <IPC.C/BLOCK@EMSNTD.Ontyme.Tymnet> 
From: IPC.C/BLOCK@EMSNTD.Ontyme.Tymnet 
Date: 07 JUL 87 07:58:27 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: A78005@Ontyme.Tymnet 
Subject: Can't reach you using... 

Bill I can no longer send you notes via your id 
WRS@C39.TYMNET
Has it been taken from the system or is there a problem with 

Ontyme or the network?  Maybe I don't know how to specify the name when
I use Ontyme directly, but it gave me an invalid recipient
please advise.  thanks
Received: from F29.Tymnet by B39.Tymnet; Tue, 7 Jul 87 13:59:51 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 7 Jul 87 13:46:35 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 07 JUL 87 13:14:43 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I55496@Ontyme.Tymnet 
Subject: Absence from Office 

                 MCDONNELL DOUGLAS NETWORK SYSTEMS COMPANY

                     T Y M N E T    O P E R A T I O N S

                            M E M O R A N D U M



DATE>      02 JUL 87  10:43

TO>        All Bay Area Employees

COPIES>    

FROM>      Chuck Herman


SUBJECT>   Absence from Office


-----------------------------------------------------------------------


On July 7th thru July 10th I will be out of the office. If you have
any questions please call Dave Naumann (8825) for the Mail Services, 
Judy Carlson (5315) for Telephones, or George Wasowski (5017) for the 
Warehouse.
Received: from F38.Tymnet by B39.Tymnet; Tue, 7 Jul 87 14:05:37 PDT
Return-path: <MKTGINFO@F38.Tymnet> 
From: MKTGINFO@F38.Tymnet 
Date: Tue, 7 Jul 87 13:59:48 PDT 
To: WRS 
Subject: ACCESS# PROGRAM 

OK BILL IAM
JUST ABOUT READY TO RELEASE THE ACEE
JUST ABOUT READY TO RELEASE THE ACCESS# PROGRAM.  THIS PROGRAM WILL BE 
DIFFERENT FROM THE S
IM GOING TO STATR AGAIN
Received: from tymix.Tymnet by B39.Tymnet; Tue, 7 Jul 87 14:16:19 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09763; Tue, 7 Jul 87 14:20:28 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA24379;
	Tue, 7 Jul 87 14:20:04 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA16867; Tue, 7 Jul 87 14:20:31
	PDT
Return-path: <davarj@calvin> 
From: davarj@calvin (Jonathan Davar) 
Date: Tue, 7 Jul 87 14:20:31 PDT 
To: wrs@calvin 
Message-id: <8707072120.AA16867@calvin.Tymnet.com> 
Subject: K_termina

Bill,

How does a process terminate, without returning, in the event that
it might want to terminate two different processes, without knowing
which one was itself?

Jonathan.

Something elegant. Not puting a killer process on the background

Received: from tymix.Tymnet by B39.Tymnet; Tue, 7 Jul 87 17:23:39 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11053; Tue, 7 Jul 87 17:27:49 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA25114;
	Tue, 7 Jul 87 17:27:25 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA17893; Tue, 7 Jul 87 17:27:51
	PDT
Return-path: <davarj@calvin> 
From: davarj@calvin (Jonathan Davar) 
Date: Tue, 7 Jul 87 17:27:51 PDT 
To: wrs@calvin 
Message-id: <8707080027.AA17893@calvin.Tymnet.com> 
Subject: Macros for doc. formatting 


Bill,
I tried using .S5 in a document. Are you responsible for the macro package,
and if so, could you modify it? Or who is responsible if you are not?

Jonathan

Received: from F29.Tymnet by B39.Tymnet; Wed, 8 Jul 87 22:59:47 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Wed, 8 Jul 87 22:47:25 PDT 
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
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Wed, 8 Jul 87 22:44:18 PDT
From: Postmaster@F29.Tymnet 
Date: Wed, 8 Jul 87 22:27:28 PDT 
To: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    .

  The error code and message was:
    452 Requested action not taken: insufficient system storage

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <KLENSIN@INFOODS.MIT.EDU>

----------
RCPT To: <KLENSIN@INFOODS.MIT.EDU> 
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Wed, 8 Jul 87 22:26:21 PDT
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 09 JUL 87 03:04:54 
To: <@OFFICE-1.ARPA:KLENSIN@INFOODS.MIT.EDU> 
Message-id: A79003@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - INVALID RECIPIENTS 

Message #A79002 was not delivered to the following recipient(s):

        APPCOMEB.OEU.

These recipients do not exist.
Please check the spelling of the recipient(s) listed above.

(undelivered message follows)
------------------------------------------------------------------------
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Wed, 8 Jul 87 19:49:59 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Wed, 8 Jul 87 19:49:40 PDT
Received: from INFOODS.MIT.EDU by OFFICE-1.ARPA; 8 Jul 87 19:44:42 PDT
Received: by INFOODS id <00000516064@INFOODS.MIT.EDU> ; Wed, 8 Jul 87 22:38:45
        EDT
Return-path: <@OFFICE-1.ARPA:KLENSIN@INFOODS.MIT.EDU> 
From: John C Klensin <KLENSIN@INFOODS.MIT.EDU> 
Date: Wed, 8 Jul 87 22:36:11 EDT 
To: APPCOMEB.OEU%ONTYME.TYMNET@OFFICE-1.ARPA 
Message-id: <870708223611.00000516064@INFOODS.MIT.EDU> 
Subject: FAX and ECONET 
X-VMS-Mail-To: COLLINGSWORTH

Sue Hacking

Dear Sue,
   I tried sending the following through last Monday, only to have 
Tymshare reply that your ECONET address had been deactivated.  I assume 
that it was a temporary accounting problem and that things are now going 
through.  If that turns out to not be true, we'll adopt another plan, 
like the slow, but reliable, post.
    john
---------------------------------------
   We just tried a FAX transmission again on the number you gave, and it
apparently went through, so you should have it in the morning.  Our
local FAX operator says, this time, that it would "take as little as its
being out of paper for it to not respond as a fax machine", so we assume
that was the problem last time. 
  With luck, this will end the saga, and maybe the next time we will be
able to use the ECONET<->ARPANET gateway. 

  FYI, in the event that the fax once again does not go through, the
instructions for responding to this message from ECONET are as follows: 

  1) In preparing your message, make the first line ween "ARPA" 
and "(TUMS)" and nowhere else.
  2) Make the second line blank.
  3) Put in the message as usual
  4) When you are finished and prepare to send the message, use
      TUMS.GATEWAY
at the "ID" prompt rather than a ECONET user name.
  Usually this works and goes through smoothly.  Sorry for the fussy 
process - it isn't our fault and is probably the best that can be 
managed.

Best regards,
  John Klensin
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 4:07:05 PDT 
To: davarj@calvin (Jonathan Davar) 
Subject: Re: Macros for doc. formatting 
In-reply-to: <8707080027.AA17893@calvin.Tymnet.com> of Tue, 7 Jul 87 17:27:51
	PDT

Use 
  .H 5 "blah blah"
rather than
  .S5 "blah blah"
STDDFS is a gross and disgusting kludge written by somebody who didn't
know anything about TROFF and MM and I hope will go away soon.  Actually
it should just be turned into a preference file to setup a "standard"
style as opposed to trying to reinvent things like the heading macro.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 4:09:38 PDT 
To: rmurphy@f104 (Ron Murphy) 
Subject: Re: ipcf.h 
In-reply-to: <8707080403.AA00865@f104.sun.com> of Tue, 7 Jul 87 21:03:11 PDT

If you edit the read only copy, your changes will be lost!!!
To make it writable, do
	sccs edit file.mm
(or whatever)

PROFS: its getting CMS errors and crashing the virtual machine.  I've
turned it back to SL to try and figure out.  Somebody edited the PROFILE
EXEC on 7/3, was that you?  I blamed SL since it was done at 8:08 AM
and I doubt you or I would have been up then (actually 5:08 here).
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 9 Jul 87 11:46:18 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19252; Thu, 9 Jul 87 11:50:37 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01319; Thu, 9 Jul 87 11:50:07
	PDT
Return-path: <gauci@hobbes> 
From: gauci@hobbes (Ed Gauci) 
Date: Thu, 9 Jul 87 11:50:07 PDT 
To: wrs@hobbes 
Message-id: <8707091850.AA01319@hobbes.Tymnet.com> 
Subject: ipcf messages 

Bill,

  I thought I heard you say that the IPCF receiver functions will be
  passed a message handle by reference instead of value.  This is no
  problem, but I'd like to know asap since I have to reflect it in
  ISIS documentation.

Thx, Ed.
Received: from tymix.Tymnet by B39.Tymnet; Thu, 9 Jul 87 13:52:10 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA20325; Thu, 9 Jul 87 13:56:29 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA02749; Thu, 9 Jul 87 13:56:33 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 9 Jul 87 13:56:33 PDT 
To: WRS@B39.Tymnet 
Message-id: <8707092056.AA02749@f104.sun.com> 
Subject: Re: ipcf.h 

I think I have figured out how to use SCCS.  I didn't edit PROFILE,
that is bizarre.  I've only gotten up at 5 about 10 times in the
last five years and that wasn't one of them!
Ron
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Thu, 9 Jul 87 13:59:51 PDT 
To: WRS@C39.Tymnet 
Subject: "LUNCH TOMMORROW BRENT NOT AVAILABLE, "... 

LUNCH TOMMORROW - BRENT NOT AVAILABLE, BUT SUGGEST WE MEET ANYWAY,
BUT AT CHILI'S @11:30 - NOT HERE. PLEASE CONFIRM TONIGHT!!?!
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 14:01:10 PDT 
To: Joe Smith <JMS@X930> 
Subject: Re: Could you add indirect files to RDIST? 
In-reply-to: your message of Thu, 9 Jul 87 8:59:38 PDT

Yea, I'll work on that.  Actually, I think I'll add it to args and then
it will magically appear in several programs.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 14:03:56 PDT 
To: Joe Smith <JMS@X930> 
Subject: Re: Choice of editor on ~V command. 
In-reply-to: your message of Thu, 9 Jul 87 9:03:17 PDT

A conspicously missing feature.  I'll add that, too.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 14:04:42 PDT 
To: gauci@hobbes (Ed Gauci) 
Subject: Re: ipcf messages 
In-reply-to: <8707091850.AA01319@hobbes.Tymnet.com> of Thu, 9 Jul 87 11:50:07
	PDT

Bill,

  I thought I heard you say that the IPCF receiver functions will be
  passed a message handle by reference instead of value.  This is no
  problem, but I'd like to know asap since I have to reflect it in
  ISIS documentation.

Thx, Ed.
----------------------------------------------------------------
This is true.  Also, the timeout in ipcfSend() has gone away.  There will
be a draft of the new IPCF section within 24 hours.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 14:07:26 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: "LUNCH TOMMORROW BRENT NOT AVAILABLE, "... 
In-reply-to: your message of Thu, 9 Jul 87 13:59:51 PDT

Ok - see you at XChilis, sans Brent, 11:30.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 9 Jul 87 14:23:29 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA20623; Thu, 9 Jul 87 14:27:48 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA02069;
	Thu, 9 Jul 87 14:27:16 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA26186; Thu, 9 Jul 87 14:27:51
	PDT
Return-path: <davarj@calvin> 
From: davarj@calvin (Jonathan Davar) 
Date: Thu, 9 Jul 87 14:27:51 PDT 
To: wrs@calvin 
Message-id: <8707092127.AA26186@calvin.Tymnet.com> 
Subject: .H5 


H5 improves the state of things: Sequence numbering is not screwed up as 
it was with S5. However, H5 sections disappear completely. HELP!!

Jonathan

Received: from tymix.Tymnet by B39.Tymnet; Thu, 9 Jul 87 16:10:21 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA21768; Thu, 9 Jul 87 16:14:40 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04225;
	Thu, 9 Jul 87 16:14:06 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA03145; Thu, 9 Jul 87 16:14:36
	PDT
Return-path: <kopfj@opus> 
From: kopfj@opus (John Kopf) 
Date: Thu, 9 Jul 87 16:14:36 PDT 
To: wrs@opus 
Message-id: <8707092314.AA03145@opus.Tymnet.com> 
Subject: Bill: will you be in the office soon for consultation? 


John Kopf
From: WRS@B39.Tymnet 
Date: Thu, 9 Jul 87 18:27:46 PDT 
To: wrs/baby 
Subject: "hi"... 

hi
From: Postmaster@B39.Tymnet 
Date: Thu, 9 Jul 87 18:29:04 PDT 
To: WRS@B39.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Invalid address(s):
<OSNF@B39.Tymnet
SPL@B39.Tymnet>


----------
RCPT To: <OSNF@B39.Tymnet
SPL@B39.Tymnet> 
From: WRS@B39.Tymnet 
Date: Thu, 9 Jul 87 18:29:01 PDT 
To: wrs/baby 

hello
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 19:02:20 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: test 

test
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 19:02:48 PDT 
To: wrs@office-1.arpa 
Subject: "test"... 

test
From: Postmaster@B39.Tymnet 
Date: Thu, 9 Jul 87 19:04:47 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    RCPT TO: <wrs@office-1.arpa>

  The error code and message was:
    550 No such mailbox here

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <wrs@office-1.arpa>

----------
RCPT To: <wrs@office-1.arpa> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 19:02:48 PDT 
To: wrs@office-1.arpa 
Bcc: WRS@B39.Tymnet 

test
Received: from F29.Tymnet by B39.Tymnet; Sat, 11 Jul 87 5:30:35 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Sat, 11 Jul 87 5:25:05 PDT 
To: <WRS@B39.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by EMSNTD.Ontyme.Tymnet
The response was:
:send ** acctusers cns.ermail nsms.B/Corenson tymops.V/Stafford qsats.G/Walker
SUBJ: 
IN FILE ** `O5qR,, LINE 000222, 
NONEXISTENT RECIPIENT
NTD.SH/WONG
MSG # I57378 SENT AT 11 JUL 87 05:23:04  


----------
RCPT To: <NTD.SH/WONG.@EMSNTD.Ontyme.Tymnet> 
Received: from F29.Tymnet by EMSNTD.Ontyme.Tymnet; Sat, 11 Jul 87 5:20:23 PDT
Received: from B39.Tymnet by F29.Tymnet; Sat, 11 Jul 87 5:20:03 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 11 Jul 87 5:11:54 PDT 
To: wrs/baby 
Subject: William Christian Soley 

The latest addition to the Soley family: William Christian Soley
Born at home, 11-Jul-87 02:10 PDT, 9.25 lbs.
Both Becky and the baby are in excellent health.

-Bill
Received: from F29.Tymnet by B39.Tymnet; Sat, 11 Jul 87 5:30:38 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Sat, 11 Jul 87 5:35:17 PDT
Return-path: <@OFFICE-1.ARPA> 
From: Postmaster@OFFICE-1.ARPA 
Date: 11 Jul 87 05:32:05 PDT 
To: WRS@B39.Tymnet 
Subject: Mail Item Disposition 

Regarding your message sent Sat, 11 Jul 87 5:11:54 PDT 
   Subject: William Christian Soley 

SUMEX-AIM.ARPA Unknown host/domain: delivery canceled for 
LYNN%PANDA@SUMEX-AIM.ARPA
Received: from F29.Tymnet by B39.Tymnet; Sat, 11 Jul 87 5:59:26 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Sat, 11 Jul 87 5:44:42 PDT
Received: from F29.Tymnet by EMSNTD.Ontyme.Tymnet; Sat, 11 Jul 87 5:20:23 PDT
Received: from B39.Tymnet by F29.Tymnet; Sat, 11 Jul 87 5:20:03 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 11 Jul 87 5:11:54 PDT 
To: wrs/baby 
Subject: William Christian Soley 

The latest addition to the Soley family: William Christian Soley
Born at home, 11-Jul-87 02:10 PDT, 9.25 lbs.
Both Becky and the baby are in excellent health.

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Sat, 11 Jul 87 9:07:52 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03714; Sat, 11 Jul 87 09:12:22 PDT
Return-path: <whitehur@tymix.Tymnet> 
From: whitehur@tymix.Tymnet (Pamela K. Whitehurst) 
Date: Sat, 11 Jul 87 09:12:22 PDT 
To: wrs@tymix.Tymnet 
Message-id: <8707111612.AA03714@tymix.Tymnet> 
Subject: congratulations 


Sounds like you involved in a real long term project... enjoy

PKW
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Sat, 11 Jul 87 11:25:06 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: William Christian Soley 

Bill and Becky,


MY SINCEREST CONGRATULATIONS ON THE NEW MEMBER OF YOUR FAMILY.

BILL, IF YOU NEED ANYTHING JUST HOLLER, ANYTHING AT ALL.

Take Care All of You....

Omar

Received: from tymix.Tymnet by B39.Tymnet; Sat, 11 Jul 87 12:24:46 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03911; Sat, 11 Jul 87 12:29:15 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA11466;
	Sat, 11 Jul 87 12:28:38 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04050; Sat, 11 Jul 87
	12:29:21 PDT
Return-path: <davarj@calvin> 
From: davarj@calvin (Jonathan Davar) 
Date: Sat, 11 Jul 87 12:29:21 PDT 
To: wrs@calvin 
Message-id: <8707111929.AA04050@calvin.Tymnet.com> 
Subject: Congratulations !! 


Is this your first child?

Jonathan.

Received: from X930.Tymnet by B39.Tymnet; Sat, 11 Jul 87 14:35:20 PDT
Return-path: <osman@X930.Tymnet> 
From: Osman Guven <osman@X930.Tymnet> 
Date: Sat, 11 Jul 87 13:36:11 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: William Christian Soley 
In-reply-to: your message of Sat, 11 Jul 87 5:11:54 PDT

Well, you sure kept it quiet.  I didn't even know Becky was expecting.

You are now part of a very elite group.  Your initiation will include
changing dirty diapers, being awakened nightly at 01:00, 03:00, 05:00 ...
dirty fingerprints on your floppy disks, crayon drawings on your once
clean walls, assembling various toys which require an engineering degree,
giving driving lessons and paying college tuition, just to name a few.

By the time you have six you will be an expert at answering questions
like "Daddy, what's that?" and "Why???" But by then you will have learned
what the joys and rewards of fatherhood are.

Congratulations to both of you .. !!!

-Osman-
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 11 Jul 87 22:32:35 PDT 
To: davarj@calvin (Jonathan Davar) 
Subject: Re: Congratulations !! 
In-reply-to: <8707111929.AA04050@calvin.Tymnet.com> of Sat, 11 Jul 87 12:29:21
	PDT

Yes, he's the first.  Thanks - Bill
From: Dan Lasater <DLASATER@C39.Tymnet> 
Date: Mon, 13 Jul 87 7:39:25 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: William Christian Soley 
In-reply-to: your message of Sat, 11 Jul 87 5:11:54 PDT

Congratulations...
Dan L
From: JKATO@B39.Tymnet 
Date: Mon, 13 Jul 87 7:41:20 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: William Christian Soley 
In-reply-to: your message of Sat, 11 Jul 87 5:11:54 PDT

Congratulations!!!!  All of us July birthday'ers think
July babies are the BEST!!
From: Gazel Tan <GAZEL@C39.Tymnet> 
Date: Mon, 13 Jul 87 9:08:11 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: William Christian Soley 
In-reply-to: your message of Sat, 11 Jul 87 5:11:54 PDT

Becky & Bill,

 C O N G R A T U L A T I O N S  ! ! ! ! ! ! ! !

gazel
Received: from X930.Tymnet by B39.Tymnet; Mon, 13 Jul 87 9:20:35 PDT
Return-path: <DenCoff@X930> 
From: Dennis Coffey (Tymcom-X Systems) <DenCoff@X930> 
Date: Mon, 13 Jul 87 9:20:00 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: William Christian Soley 
In-reply-to: your message of Sat, 11 Jul 87 5:11:54 PDT

Bill,
  Congratulations, Dad!
-Dennis-
From: Sandy McFarland <SandyMF@B39.Tymnet> 
Date: Mon, 13 Jul 87 9:52:49 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: William Christian Soley 
In-reply-to: your message of Sat, 11 Jul 87 5:11:54 PDT



CONGRATULATIONS and GOOD LUCK!!!!!!!!!!!!!!!!!!!!!!!!

slm
Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 10:41:54 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 13 Jul 87 10:44:08 PDT
Return-path: <NTD.D/GRAHAM@EMSNTD.Ontyme.Tymnet> 
From: NTD.D/GRAHAM@EMSNTD.Ontyme.Tymnet 
Date: 13 JUL 87 10:13:20 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I57578@Ontyme.Tymnet 
Subject: WILLIAM CRISTIAN SOLEY 

GOOD MORNING BILL,

CONGRATULATIONS TO YOU AND YOUR WIFE!  WHAT A NICE WAY TO START
MONDAY, HEARING SUCH GOOD NEWS.

GOD BLESS YOU AND YOUR FAMILY.


DEB
Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 10:53:16 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 13 Jul 87 10:44:25 PDT
Return-path: <NTD.J/SHEN@EMSNTD.Ontyme.Tymnet> 
From: NTD.J/SHEN@EMSNTD.Ontyme.Tymnet 
Date: 13 JUL 87 10:15:03 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I57580@Ontyme.Tymnet 
Subject: William Christian Soley 

Congratulations!!!!!
Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 10:53:39 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Mon, 13 Jul 87 10:55:32 PDT
Received: from Xerox.COM by OFFICE-1.ARPA; 13 Jul 87 10:52:39 PDT
Received: from Burger.ms by ArpaGateway.ms ; 13 JUL 87 10:06:49 PDT
Return-path: <@OFFICE-1.ARPA:Joseph_Kaminski.PARC@Xerox.COM> 
From: KAMINSKI.PARC@Xerox.COM 
Sender: "Joseph_Kaminski.PARC"@Xerox.COM 
Date: 13 Jul 87 10:05:46 PDT (Monday) 
To: WRS%B39.Tymnet@OFFICE-1.Arpa 
Cc: KAMINSKI.PARC@Xerox.COM 
Message-id: <870713-100649-1749@Xerox> 
Subject: William Christian Soley 


Hi BECKY, BILL AND BILL

CONGRETULATION....

JOE.
From: Kal Kalenda <KALENDA@B39.Tymnet> 
Date: Mon, 13 Jul 87 11:11:00 PDT 
To: WRS 
Subject: William Christian 


Congratulations, Best wishes.    Kal
From: David Chang <DChang@B39.Tymnet> 
Date: Mon, 13 Jul 87 11:36:51 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: Congradulations! 

Bill,

Congradulations!!!  I am very glad to hear that both mother and baby are
fine.  Hope that everything is fine with you also.  We'll talk more about
fatherhood when you come in.  Have fun!

David
Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 12:01:47 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 13 Jul 87 12:04:07 PDT
Return-path: <TYMOPS.V/STAFFORD@EMSNTD.Ontyme.Tymnet> 
From: TYMOPS.V/STAFFORD@EMSNTD.Ontyme.Tymnet 
Date: 13 JUL 87 11:37:53 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: M04976@Ontyme.Tymnet 
Subject: Congratulations! 

Bill and family...

Congratulations!  
At over 9 lbs. sounds like he probably looks 3 months olad already.

Glad that Mother and Son are doing fine. Am looking foirward to seeing
some pictures of the baby on your next visit to Orchard Parkway.

Again, congratulations to Mom and Pop.
Vida
Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 12:56:41 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 13 Jul 87 12:50:28 PDT
Return-path: <NTD.L/JOHNSON@EMSNTD.Ontyme.Tymnet> 
From: NTD.L/JOHNSON@EMSNTD.Ontyme.Tymnet 
Date: 13 JUL 87 12:19:45 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I57706@Ontyme.Tymnet 
Subject: "congrats!!!!!"... 

congrats!!!!!
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 13 Jul 87 13:55:49 PDT 
To: feldman@comet 
Subject: Lynn Gold 

Do you know her current address (i.e. network mail)?  Thanks -Bill
Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 14:21:34 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 13 Jul 87 14:09:52 PDT
Return-path: <NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet> 
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 13 JUL 87 13:51:22 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.P/CHU@Ontyme.Tymnet,
	NTD.D/CHANG@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.L/VAYSBURD@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet,
	NTD.R/RAFFO@Ontyme.Tymnet, NTD.O/SERANG@Ontyme.Tymnet,
	NTD.H/RIVERS@Ontyme.Tymnet, NTD.N/BURRIESCI@Ontyme.Tymnet 
Message-id: I57807@Ontyme.Tymnet 
Subject: SWBT Schedule Update 

 
                      Tymnet/Network Technology Development
                          PROGRAM MANAGEMENT DEPARTMENT

 
                               M E M O R A N D U M 

 
                                                                []  ISG
 
DATE>      13 JUL 87  13:31

TO>        Gazel Tan
           Paul Chu
           Adele Newman
           Romolo Raffo
           Dave Chang

COPIES>    Ken Holcomb
           Distribution List (** NUMPLAN)

FROM>      Allyn Morriss


SUBJECT>   SWBT Schedule Update


-----------------------------------------------------------------------


With last Friday's meeting and luncheon behind us, I would like to
ask for your schedule updates. Mark up the schedule I handed out
at the meeting and get it to me no later than this Thursday.
Dave Chang has already given me his for the SUPervisor Section.
 
I will update the integrated schedule and distribute it to you when
all revisions are received. If there is something you would like to 
discuss about your schedule, stop by or call.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 13 Jul 87 14:59:31 PDT 
To: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>, Ken Holcomb
	<NTD.K/HOLCOMB@Ontyme.Tymnet>, Mike Rude <NTD.M/Rude@Ontyme.Tymnet>,
	Betty Jo (BJ) Chang <NTD.B/CHANG@Ontyme.Tymnet>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme.Tymnet>, Dennis Ernst
	<NTD.D/ERNST@Ontyme.Tymnet>, John Kopf <KopfJ@B39.Tymnet>, Dan Lasater
	<DLasater@B39.Tymnet>, Jeff Oliveto <ENS.J/OLIVETO@Ontyme.Tymnet>,
	Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>, Romolo Raffo
	<NTD.R/RAFFO@Ontyme.Tymnet>, Dick Rawson <NTD.D/RAWSON@Ontyme.Tymnet>,
	Adele Newman <NTD.A/NEWMAN@Ontyme.Tymnet>, Dave Smith
	<NTD.D/SMITH@Ontyme.Tymnet>, DChang, Gazel Tan <GAZEL@B39>, Paul Chu
	<NTD.P/CHU@Ontyme.Tymnet>, Diana Tai <NTD.D/TAI@Ontyme.Tymnet>, Bill
	Soley <WRS@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>, Ashok Chippa
	<ACHIPPA@B39>, Henry Rivers <NTD.H/RIVERS@Ontyme.Tymnet>, Nancy Holt
	<NTD.N/BURRIESCI@Ontyme.Tymnet>, Dennis Jonas <JONAS@B39>, Scott Wedel
	<NTD.S/WEDEL@Ontyme.Tymnet>, Wayne Bartlett
	<TYM.W/BARTLETT@Ontyme.Tymnet>, Rosie Hull <NTD.R/HULL@Ontyme.Tymnet> 
Subject: LOGII GED UPDATE 

Hello Folks,

The LOGII GED has been updated to as follows:

- Throughput value 1 (cntrl-B) is now indicated as being 160cps rather
than 128cps.  The CIRCUI.DOC indicates 128cps, and Scott Wedel says
the Node Code actually uses 160cps.  Perhaps some documentation updating
is in order.

- The padding of command channel messages to an even number of bytes
has been deemed uneccessary, and has been removed.

- Section 4.1 examples now include the LGSC message.

- Some guidelines for X.409 usage within LOGII are established in section
4.2 LOGII Message philosophy.  Also RMOs are now defined to be Sequences.
If no SMOs are present, then the SMO Set is not included and the message
is only an RMO Sequence.
It is also stated that the interfaces must await a response from the
Supervisor, before sending any ICRS messages on a logon port.

- The ICRM message, section 4.3 contains a bit in the message header
which the dispatcher sets to indicate that a requested is licensed to
be an Auxiliary circuit.

- The ICRS message, section 4.5 contains only a SMO set and no RMO sequence.

- The rules for NCRM integrity are included section 4.6.

- The interface rules for turning echo on and clearing the logon port
input and output buffers, are stated for each normal logon response code
in the RCRM/RCRI messages (section 4.7).  Two uneccessary reject codes have
been made unused and invalid.

- In section 4.10.1 NCN Overview and Compatibility issues, the handling
of the takeover message LOGII/LOGI indication from the Supervisor
has been slightly modified.  Also indicated is the patching of the
current LOGI Supervisor 42.01 to handle and ignore a LOGII NCRM message.

- In sections 4.9, 4.10 and 4.12 the CCN, NCN and ISN have been reorganized
to follow a consistent structure throughout the three messages.
A needleNodeCount MO has been added (MO-28), indicating the number
of node number entries in the needleNodeList MO.  The NCN tail is now
simply a control character pair, with no other associated data.

- In the X.409 tutorial, section 7, the description of long form length
has been corrected and the example now uses a current MO, rather
than the eliminated Dispatcher Key MO.

- The example in section 8 now correctly shows the message size to include the
4 bytes of header data, as well as the MOs.


The Document has been updated on DNDSPEC.

Thanks....Omar

Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 16:19:40 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05540; Mon, 13 Jul 87 16:01:06 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA17078;
	Mon, 13 Jul 87 15:59:55 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA10007; Mon, 13 Jul 87
	16:00:42 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Mon, 13 Jul 87 16:00:42 PDT 
To: wrs@calvin 
Message-id: <8707132300.AA10007@calvin.Tymnet.com> 
Subject: File Design review 

Bill,

 Will you be able to attend a File design review tomorrow
in the morning around 10am? The document is in /RID/SFS/FILESYS
(or something like that).

Pat
Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 16:23:02 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03697; Mon, 13 Jul 87 09:50:46 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13479;
	Mon, 13 Jul 87 09:49:37 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA06547; Mon, 13 Jul 87
	09:50:21 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Mon, 13 Jul 87 09:50:21 PDT 
To: rodney@calvin 
Message-id: <8707131650.AA06547@calvin.Tymnet.com> 
Subject: Brown Bag Seminar on Unit Testing 

A brown bag seminar will be held today at 11:30 over at the
VG2 training room.  Charlotte Healender will present a
structured approach to internal design and test plans.
Please plan on attending.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 16:23:29 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03608; Mon, 13 Jul 87 09:35:45 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA06438; Mon, 13 Jul 87 09:35:23 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Mon, 13 Jul 87 09:35:23 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707131635.AA06438@f104.sun.com> 
Subject: Re: William Christian Soley 

Hi Bill,
Congratulations and... happy child care activities!
Ron
Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 16:41:43 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 13 Jul 87 16:35:35 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 13 JUL 87 16:27:53 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I57952@Ontyme.Tymnet 
Subject: FOCUS VERSION CHANGE 

                      R E M O T E   C O M P U T I N G
                              S E R V I C E S


                                                                  [MDC-ISG]

DATE>      07 JUL 87  10:25

TO>        SYSTEM 43 FOCUS USERS

COPIES>    

FROM>      CHERYL SCHROEDER
           RCS.C/SCHROEDER


SUBJECT>   FOCUS VERSION CHANGE


-----------------------------------------------------------------------


  A T T E N T I O N   S Y S T E M    4 3   U S E R S !!!

THIS IS A REMINDER THAT WE WILL BE ROLLING FOCUS VERSION 5.0.3 INTO
PRODUCTION AND VERSION 5.0.4 INTO FIELD TEST ON JULY 15, 1987.

PLEASE TEST YOUR APPLICATION USING THE CURRENT FIELD TEST VERSION 5.0.3
TO MAKE SURE YOUR APPLICATION RUNS AND NO MODIFICAITONS ARE NECESSARY.

WE ARE GOING TO BE ROLLING NEW VERSIONS OF FOCUS ON A MORE REGULAR BASIS.
FOCUS VERSION 5.0.5 WILL AVAILABLE SOME TIME IN SEPTEMBER AND WE
WILL BE ROLLING 5.0.4 INTO PRODUCTION AND 5.0.5 INTO FIELD TEST AT THAT
TIME.  WE WILL NOTIFY YOU IN ADVANCE OF THE VERSION CHANGES SO YOU
CAN COMPLETE THE NECESSARY TESTING.

PLEASE CALL CAROL BURKETT AT (314) 234-8786 OR CHERYL SCHROEDER AT
(314) 234-6776 IF YOU HAVE ANY QUESTIONS.


ORIGINAL MESSAGE NUMBER :  J92023
Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 17:13:13 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06028; Mon, 13 Jul 87 17:15:37 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA17684;
	Mon, 13 Jul 87 17:14:27 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA10871; Mon, 13 Jul 87
	17:15:14 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 13 Jul 87 17:15:14 PDT 
To: wrs@calvin 
Message-id: <8707140015.AA10871@calvin.Tymnet.com> 
Subject: "Bill, 1. the in the glob is it"... 

Bill,

1. the 'loadFile" in the glob - is it the default file to load by boot?
2. your statement in Rodney document:
     struct BootBits bootDefault = {....}
  is it a valid C declaration? In other words is it an equivalent
of a #define statement that defines a constatnt structure?

Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 13 Jul 87 18:49:21 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "CONGRATULATIONS AND WELCOME TO"... 
In-reply-to: <8707130557.AA05498@calvin.Tymnet.com> of Sun, 12 Jul 87 22:57:42
	PDT

Thank you, I suggest you get a copy of the troff manual and the mm manual.
Remember, that when using mm, you should be very careful about using troff
commands - use only those that are suggested in the mm manual or you will
risk side effects producing strange and unpredictable effects.  More to the
point,
	.nf	no fill
	.fi	fill
	.b	I don't know - not a troff command, maybe a macro
	.B	bold face
	.bp	break page
However, you should probably use displays (.DS .DE) in stead of .nf .fi
and also .SK in place of .bp.  In genera, only use direct troff commands
if you can not find a mm macro to do what you want.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 13 Jul 87 19:02:27 PDT 
To: driscoll@calvin (Pat Driscoll) 
Subject: Re: File Design review 
In-reply-to: <8707132300.AA10007@calvin.Tymnet.com> of Mon, 13 Jul 87 16:00:42
	PDT

I think I'll pass on that, I feel very comfortable with what Steve has and
I should stay with Becky while she is confined to bed.  Thanks for letting
me know about it, though.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 13 Jul 87 19:20:04 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, 1. the in the glob is it"... 
In-reply-to: <8707140015.AA10871@calvin.Tymnet.com> of Mon, 13 Jul 87 17:15:14
	PDT

The load file is an informational entry set by the boot to the name of
the file the system was most recently loaded from.  -Bill
Oh yes, struct BootBits bootDefault = {...}; is standard C and it
is not equivalent to #define in that it actually allocates storage in
our case in ROM and initializes it to the specified values at link/load
time.  See K&R p. 124.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Wed, 15 Jul 87 11:59:41 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 15 Jul 87 12:01:00 PDT
Return-path: <NTD.BASELINE@EMSNTD.Ontyme.Tymnet> 
From: NTD.BASELINE@EMSNTD.Ontyme.Tymnet 
Date: 15 JUL 87 11:59:04 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I58793@Ontyme.Tymnet 
Subject: :RELEASE OF SOFTWARE BASELINE "X" 
RELEASEOFSOFTWAREBASELINE"X": 

     
     
       =================================================================   
        *** NETWORK SOFTWARE BROADCAST *** NETWORK SOFTWARE BROADCAST ***  
        =================================================================  
        **** UPDATE **** UPDATE **** UPDATE **** UPDATE **** UPDATE *****  
        =================================================================  
                                                              PAGE 1 of 2  
     
        DATE:       July 15, 1987  
        TO:         DISTRIBUTION   
        FROM:       Sheldon Cohen,  Software Distribution Support
        SUBJ:       RELEASE OF SOFTWARE BASELINE "X"   
        REFERENCE:  Baseline Document  (BASELINE)LETTER.X03 
     
        Baseline "X" is released as of July 15, 1987 and includes
        the following changes:
     
        1. NEW PRODUCTS ADDED TO THE BASELINE            Old     New  
        --------------------------------------------    -----   ----- 
        MCP     Mutual Conversion & Pass-Through         n/a    03.01 
     
        2. UPDATED BASELINE PRODUCTS                     Old     New  
        --------------------------------------------    -----   ----- 
        DCPU    Engine CPU Diagnostic                   03.01   04.00 
        DISK    Engine 10MB Disk Diagnostic             03.00   03.01 
        DMAC    Universal Engine MAC Diagnostic         06.00   07.00 
        DMEM    Engine Memory Diagnostic                03.00   04.00 
        DREL    Reloceted Instruction Diagnostic        01.00   02.00 
        ISHA    ISIS HSA Server                         01.06   01.07 
     
        3. INTERMEDIATE RELEASES TO BASELINE             Old     New  
        --------------------------------------------    -----   ----- 
        none   
     
        The complete Baseline "X" document set may be printed by 
        specifying (BASELINE)PRINT.X on systems 22,23,25,26,31,33,    
        37,39,54,57,65, or 70.
     
        A final print file for the previous Baseline "W" document set 
        updated April 15, 1987 may be obtained by specifying (BASELINE)    
        PRINT.W on systems 22,23,25,26,31,33,37,39,54,57,65 or 70.    
     
     
     
     
     
        =================================================================  
        *** NETWORK SOFTWARE BROADCAST *** NETWORK SOFTWARE BROADCAST ***  
        =================================================================  
        **** UPDATE **** UPDATE **** UPDATE **** UPDATE **** UPDATE *****  
        =================================================================  
     
     
     
     
        A Baseline Release and Deletion Schedule is included below:   
     
     
                      BASELINE RELEASE AND DELETION SCHEDULE
                             THROUGH July  15, 1988    
     
     
     
        BASELINE:           M            N            O            P  
        Release Date:   Oct 15,1984  Jan 15,1985  Apr 15,1985  Jul 15,1985 
        Delete  Date:       **           **           **           ** 
     
     
        BASELINE:           Q            R            S*           T* 
        Release Date:   Oct 15,1985  Jan 15,1986  Apr 15,1986  Jul 15,1986 
        Delete  Date:       **           **       Oct 15,1987  Jan 15,1988 
     
     
        BASELINE:           U*           V*           W*           X* 
        Release Date:   Oct 15,1986  Jan 15,1987  Apr 15,1987  Jul 15,1987 
        Delete  Date:   Apr 15,1988  Jul 15,1988  Oct 15,1988  Jan 15,1989 
     
     
        BASELINE:           Y            Z            A            B  
        Release Date:   Oct 15,1987  Jan 15,1988  Apr 15,1988  Jul 15,1988 
        Delete  Date:   Apr 15,1989  Jul 15,1989  Oct 15,1989  Jan 15,1990 
     
     
     
        *   Baselines supported as of July 15, 1987 (S through X).    
        **  Baselines previously deleted and stored on history tape.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 15 Jul 87 14:25:58 PDT 
To: rmurphy@f104 (Ron Murphy) 
Subject: Re: skylog red halves 
In-reply-to: <8707152118.AA00869@f104.sun.com> of Wed, 15 Jul 87 14:18:59 PDT

I'll be in a little this afternoon to talk with Pat and we can get together
and figure it out.  See you sometime after 3:00.  Thanks -Bill
Received: from X930.Tymnet by B39.Tymnet; Wed, 15 Jul 87 14:27:25 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 15 Jul 87 14:31:38 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: Thanks 


From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 15 Jul 87 14:31:32 PDT 
To: driscoll@calvin (Pat Driscoll) 
Subject: Re: Edit of rid/ipcf.mm 
In-reply-to: <8707151633.AA17778@calvin.Tymnet.com> of Wed, 15 Jul 87 09:33:34
	PDT

I've made delta's of both red/kernel and rid/ipcf so we can see where it
stands.  I've forgotten exactly how far I was through things when the labor
started.  I think its mostly done, though, making a listing now.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 15 Jul 87 14:50:37 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14361; Wed, 15 Jul 87 14:55:05 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA20344; Wed, 15 Jul 87
	14:54:39 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 15 Jul 87 14:54:39 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707152154.AA20344@calvin.Tymnet.com> 
Subject: Re: Edit of rid/ipcf.mm 

Bill,

Labor does tend to interrupt things a bit.  Yu-liang will be sending
you mail regarding the kernel scheduler.  It needs to be integrated
with the Rodney kernel simulator so that we can proceed with
unit testing.

Pat
Received: from F29.Tymnet by B39.Tymnet; Wed, 15 Jul 87 15:59:48 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 15 Jul 87 15:56:21 PDT
Return-path: <NTD.BASELINE@EMSNTD.Ontyme.Tymnet> 
From: NTD.BASELINE@EMSNTD.Ontyme.Tymnet 
Date: 15 JUL 87 15:43:35 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I58996@Ontyme.Tymnet 
Subject: :DELETION OF SOFTWARE BASELINE "S" 
DELETIONOFSOFTWAREBASELINE"S": 

     
     
       =================================================================   
        *** NETWORK SOFTWARE BROADCAST *** NETWORK SOFTWARE BROADCAST ***  
        =================================================================  
        **** UPDATE **** UPDATE **** UPDATE **** UPDATE **** UPDATE *****  
        =================================================================  
                                                            PAGE 1 of 2    
     
        DATE:  July 15, 1987  
        TO:    DISTRIBUTION   
        FROM:  Sheldon Cohen, Development Systems Support   
     
        SUBJ:  DELETION OF SOFTWARE BASELINE "S"  
     
        REFERENCE:  Baseline Document   (BASELINE)LETTER.Y01
     
        A Baseline Release and Deletion Schedule is included below:   
     
     
                      BASELINE RELEASE AND DELETION SCHEDULE
                             THROUGH July  15, 1988    
     
     
     
        BASELINE:           M            N            O            P  
        Release Date:   Oct 15,1984  Jan 15,1985  Apr 15,1985  Jul 15,1985 
        Delete  Date:       **           **           **           ** 
     
     
        BASELINE:           Q            R            S*           T* 
        Release Date:   Oct 15,1985  Jan 15,1986  Apr 15,1986  Jul 15,1986 
        Delete  Date:       **           **       Oct 15,1987  Jan 15,1988 
     
     
        BASELINE:           U*           V*           W*           X* 
        Release Date:   Oct 15,1986  Jan 15,1987  Apr 15,1987  Jul 15,1987 
        Delete  Date:   Apr 15,1988  Jul 15,1988  Oct 15,1988  Jan 15,1989 
     
     
        BASELINE:           Y            Z            A            B  
        Release Date:   Oct 15,1987  Jan 15,1988  Apr 15,1988  Jul 15,1988 
        Delete  Date:   Apr 15,1989  Jul 15,1989  Oct 15,1989  Jan 15,1990 
     
     
     
        *   Baselines supported as of July 15, 1987 ("S" through "X").
        **  Baselines previously deleted and stored on history tape.  
     
            The next Baseline release will be Baseline "Y", on Oct. 15, 1987.   
            At that time, only Baselines "T" through "Y" will be supported.
     
     
     
     
     
     
     
        =================================================================  
        *** NETWORK SOFTWARE BROADCAST *** NETWORK SOFTWARE BROADCAST ***  
        =================================================================  
        **** UPDATE **** UPDATE **** UPDATE **** UPDATE **** UPDATE *****  
        =================================================================  
                                                            PAGE 2 of 2    
     
     
The following products are marked for deletion on October 15, 1987.   
These programs have been superceded by a more current version and are 
now 18 months old.  If you have a need to retain one of these releases,    
notify NTD.BASELINE prior to 10/14/87.  We will confirm your request  
and respond to you via Ontyme.
     
     
===============================================   
 ACCT     (0066)    
 ACCOUNTING SLAVE (SUPERVISOR)
         BASELINE S - VERSION 05.01     03/15/86  
===============================================   
 DCPU
 ENGINE CPU DIAGNOSTIC   
         BASELINE S - VERSION 03.00     03/15/86  
===============================================   
 DMAC
 UNIVERSAL ENGINE MAC DIAGNOSTIC   
         BASELINE S - VERSION 05.00     03/15/86  
===============================================   
 DMEM     (008C)    
 ENGINE MEMORY DIAGNOSTIC
         BASELINE S - VERSION 02.00     03/15/86  
===============================================   
 ERLG     (0068)    
 ERROR LOG SLAVE (SUPERVISOR) 
         BASELINE S - VERSION 07.01     03/15/86  
===============================================   
 PRB      (0041)    
 NETWORK ACCESS AND CONTROL PGM (PROBE) 
         BASELINE S - VERSION 35.01     03/15/86  
===============================================   
 SUP      (0043)    
 TYMNET SUPERVISOR  
         BASELINE S - VERSION 41.01     03/15/86  
===============================================   
 SYSM     (006B)    
 SYSTEM MESSAGE SLAVE (SUPERVISOR) 
         BASELINE S - VERSION 05.01     03/15/86  
===============================================   
 UN2      (006C)    
 MASTER USER DIRECTORY SLAVE (SUPERVISOR)    
         BASELINE S - VERSION 05.01     03/15/86  
===============================================   
 UTIL     (0059)    
 UTILITY SLAVE (SUPERVISOR)   
         BASELINE S - VERSION 01.02     03/15/86  
===============================================
Received: from tymix.Tymnet by B39.Tymnet; Wed, 15 Jul 87 16:35:12 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14917; Wed, 15 Jul 87 16:39:42 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA02605; Wed,
	15 Jul 87 16:38:22 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00972; Wed, 15 Jul 87 16:39:14 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 15 Jul 87 16:39:14 PDT 
To: wrs@f104 
Message-id: <8707152339.AA00972@f104.sun.com> 
Subject: .vX 

I see exactly how this works now.  I hadn't understood the alternating font
idea, so didn't realize that the leading "", or null, in a comparable 
expression, was significant.  It works fine.
Received: from F29.Tymnet by B39.Tymnet; Thu, 16 Jul 87 1:05:28 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Thu, 16 Jul 87 8:09:54 UT 
To: MAIL 
Subject: Req #9052 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #9052 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  16-Jul-87 00:00:00
; Actual run: 16-Jul-87 00:00:03
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Thu, 16 Jul 87 9:50:09 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 16 Jul 87 9:49:42 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 16 JUL 87 09:14:18 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I59242@Ontyme.Tymnet 
Subject: FOCUS VERSION ROLL ON SYSTEM 47 

                      R E M O T E   C O M P U T I N G
                              S E R V I C E S


                                                                  [MDC-ISG]

DATE>      13 JUL 87  10:21

TO>        THE WORLD

COPIES>    

FROM>      CAROL BURKETT (RCS.C/BURKETT)
           CHERYL SCHROEDER (RCS.C/SCHROEDER)


SUBJECT>   FOCUS VERSION ROLL ON SYSTEM 47


-----------------------------------------------------------------------


THE BENCHMARKING REQUIREMENTS FOR FOCUS ON SYSTEM 47 HAVE UNDERGONE
ANOTHER REVISION, SO THE VERSION ROLL DESCRIBED IN MY JUNE 24TH MEMO
HAS CHANGED.

ON SATURDAY, JULY 25TH, THE FOCUS VERSIONS ON SYSTEM 47 WILL BE
AS FOLLOWS:

OLD FOCUS        - VERSION 4.5.4

PRODUCTION FOCUS - VERSION 5.0.3

FIELDTEST FOCUS  - VERSION 5.0.3

THIS SCENARIO DIFFERS FROM OUR ORIGINAL PLAN BECAUSE, INSTEAD OF
LOADING VERSION 5.0.4 INTO FIELDTEST, WE ARE LOADING 5.0.3.
IT IS UNUSUAL TO HAVE THE SAME VERSION IN PRODUCTION AND FIELDTEST,
BUT, BECAUSE OF OPERATING SYSTEM REQUIREMENTS, IT IS NECESSARY THAT
WE DO THIS FOR THE BENCHMARK.

AFTER THE COMPLETION OF THE BENCHMARK, WE WILL BE INSTALLING VERSION
5.0.4 INTO FIELDTEST WHICH WILL BRING US BACK TO OUR NORMAL VERSION
ALIGNMENT.

THIS LATEST CHANGE SHOULD HAVE NO IMPACT ON OTHER COMMERCIAL CLIENTS
SINCE ONLY INTERNAL USERS ACCESS THE FIELDTEST VERSION OF FOCUS.

IF YOU HAVE ANY QUESTIONS REGARDING THIS PLAN, PLEASE CALL
CAROL BURKETT (314-234-8786) OR CHERYL SCHROEDER AT
(314-234-6776).
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 16 Jul 87 14:56:05 PDT 
To: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Subject: Re: "MCDONNELL DOUGLAS NETWORK SYSTEMS"... 
In-reply-to: I59232@Ontyme.Tymnet of 16 JUL 87 08:52:35

I thought we were going to keep our old furnature.  Which is it?
Please let me know. Thanks -Bill
----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Thu, 16 Jul 87 9:50:03 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 16 Jul 87 9:49:23 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 16 JUL 87 08:52:35 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I59232@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS NETWORK SYSTEMS"... 



                 MCDONNELL DOUGLAS NETWORK SYSTEMS COMPANY

                     T Y M N E T    O P E R A T I O N S

                            M E M O R A N D U M



DATE>      14 JUL 87  08:58

TO>        ALL BAY AREA EMPLOYEES

COPIES>	    MARK LAUVER    

FROM>      JOHN BAUMGARTEN


SUBJECT>   NEW OPEN OFFICE FURNITURE


-----------------------------------------------------------------------


YOU ARE WELCOME TO COME SEE THE NEW STEELCASE OPEN OFFICE FURNITURE
"MOCK-UPS" LOCATED ON THE FIRST FLOOR NEAR MANAGEMENT & PROFESSIONAL 
DEVELOPMENT DEPT. BEHIND THE TRAINING ROOM IN VG2 (20705 VALLEY GREEN).

THESE MOCK-UPS WILL GIVE YOU A GENERAL IDEA OF THE TYPE OF CUBICLE WHICH 
WILL BE IN THE NEW MCDONNELL DOUGLAS CENTER.
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Thu, 16 Jul 87 15:04:11 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18489; Thu, 16 Jul 87 15:08:38 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA08818;
	Thu, 16 Jul 87 15:07:16 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA25045; Thu, 16 Jul 87
	15:08:07 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 16 Jul 87 15:08:07 PDT 
To: wrs@calvin 
Message-id: <8707162208.AA25045@calvin.Tymnet.com> 
Subject: "Bill, In my opinion there should"... 

Bill,
In my opinion there should be a third argument to IpcfDone() -
- caller's TSAP. Currently I see no protection for "done-ing"
someone else's message. I know the chance for that is infinitesimly
small but not zero. If you had a TSAP at least you could verify that
message belongs to this TSAP.
The problem arose during my detailed design.
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Thu, 16 Jul 87 22:32:47 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19701; Thu, 16 Jul 87 22:37:08 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10747;
	Thu, 16 Jul 87 22:35:46 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA00205; Thu, 16 Jul 87
	22:36:38 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 16 Jul 87 22:36:38 PDT 
To: wrs@calvin 
Message-id: <8707170536.AA00205@calvin.Tymnet.com> 
Subject: "This is what I had in mind 2^32)."... 

This is what I had in mind (1: 2^32). Is it acceptable?
Is there any chance to see handle = 00000000 ?
I understand this is pure speculation on my part (you don't have
to respond)
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Tue, 7 Jul 87 10:13:28 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA08662; Tue, 7 Jul 87 10:17:38 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA22979; Tue,
	7 Jul 87 10:17:14 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00141; Tue, 7 Jul 87 10:17:41 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 7 Jul 87 10:17:41 PDT 
To: wrs@f104 
Message-id: <8707071717.AA00141@f104.sun.com> 
Subject: TUMS PROFS 

A TMS user said her mail didn't get through on Thursday.  I looked at the
log in MAIL on 29 and saw lots of errors like "FILE system error detected.
virtual addr 191, reason code 05." Any idea why this is happening?  The
errors go as far back as the beginning of the log on Sunday.  
Since neither of us always has time to respond quickly to these things, maybe
a larger log file size would help tracking specific problems people report.
E.g. 300 blocks?
Ron
Received: from X930.Tymnet by B39.Tymnet; Fri, 17 Jul 87 5:07:13 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 17 Jul 87 5:00:14 PDT 
To: William R Soley <WRS@B39.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Subject: FINDIT and *.TMP 

I think FINDIT does ignore *.TMP unless you explicitly specify .TMP...
Next time I get a chance I'll verify that... but that's what it used to
do when it was first written.

/Carl
Received: from F29.Tymnet by B39.Tymnet; Fri, 17 Jul 87 14:53:44 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Fri, 17 Jul 87 14:58:11 PDT
Return-path: <DAUL@OFFICE-1.ARPA> 
From: William Daul / McDonnell-Douglas / APD-ASD <WBD.MDC@OFFICE-1.ARPA> 
Date: 17 Jul 87 14:50 PDT 
To: WRS.MDC@OFFICE-1.ARPA 
Message-id: <MDC-WBD-BT9ST@OFFICE-1> 
Subject: I forgot to... 

...congradulate you, your wife and your new rug-rat!  Good to hear everyone is 
doing well.  I suppose your sleep pattern has changed.  Next time you are 
nearby...bring in a picture.  Again, congradulations!  --Bi//

Received: from F29.Tymnet by B39.Tymnet; Mon, 20 Jul 87 11:19:57 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 20 Jul 87 11:10:50 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 20 JUL 87 10:42:11 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I60663@Ontyme.Tymnet 
Subject: "The OnTyme I.D. for the ISG Library"... 

The OnTyme I.D. for the ISG Library is changing from IPC.LIBRARY to
ISGFM.LIBRARY as of 22 July 1987 (Wednesday).  Please send any
requests to this new I.D.
Thanks.
Received: from F29.Tymnet by B39.Tymnet; Mon, 20 Jul 87 11:20:03 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 20 Jul 87 11:11:00 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 20 JUL 87 10:54:58 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I60671@Ontyme.Tymnet 
Subject: TTE 450 Network Troubleshooting 

                            M E M O R A N D U M

                          NETWORK SYSTEMS COMPANY
                         NETWORK SYSTEMS MARKETING


DATE>      20 JUL 87  09:08

TO>        All Employees

COPIES>    

FROM>      Tymnet Training and Education


SUBJECT>   TTE 450 Network Troubleshooting


-----------------------------------------------------------------------





********************* TTE 450 has space available ****************************

There are still space available in TTE 450 Network Troubleshooting.  This
course is tentatively not scheduled to be conducted again until next year.

The dates for TTE 450 is Aug. 10 - Aug 21.  So enroll now while there is still
time and space.

To enroll, using Ontyme:  EXEC *** TTE.ENROLL, or contact Cheryl McGowan at
(408)432-5062.


************************** ENROLL NOW ****************************************
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 20 Jul 87 12:04:07 PDT 
To: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>, Ken Holcomb
	<NTD.K/HOLCOMB@Ontyme.Tymnet>, Mike Rude <NTD.M/Rude@Ontyme.Tymnet>,
	Betty Jo (BJ) Chang <NTD.B/CHANG@Ontyme.Tymnet>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme.Tymnet>, Dennis Ernst
	<NTD.D/ERNST@Ontyme.Tymnet>, John Kopf <KopfJ@B39.Tymnet>, Dan Lasater
	<DLasater@B39.Tymnet>, Jeff Oliveto <ENS.J/OLIVETO@Ontyme.Tymnet>,
	Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>, Romolo Raffo
	<NTD.R/RAFFO@Ontyme.Tymnet>, Dick Rawson <NTD.D/RAWSON@Ontyme.Tymnet>,
	Adele Newman <NTD.A/NEWMAN@Ontyme.Tymnet>, Dave Smith
	<NTD.D/SMITH@Ontyme.Tymnet>, DChang, Gazel Tan <GAZEL@B39>, Paul Chu
	<NTD.P/CHU@Ontyme.Tymnet>, Diana Tai <NTD.D/TAI@Ontyme.Tymnet>, Bill
	Soley <WRS@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>, Ashok Chippa
	<ACHIPPA@B39>, Henry Rivers <NTD.H/RIVERS@Ontyme.Tymnet>, Nancy Holt
	<NTD.N/BURRIESCI@Ontyme.Tymnet>, Dennis Jonas <JONAS@B39>, Scott Wedel
	<NTD.S/WEDEL@Ontyme.Tymnet>, Wayne Bartlett
	<TYM.W/BARTLETT@Ontyme.Tymnet>, Rosie Hull <NTD.R/HULL@Ontyme.Tymnet> 
Subject: LOGII GED update 

Hello Folks,

The LOGII GED has been updated to as follows:

Section 4.2 indicates that NCN messages contain no RMO sequence.  All
ISIS messages now use bit 1 of the header flags to indicate (if set) that
a message is not the first message of a continuation sequence.

In section 4.4.1, a new ECRI code is added, indicating that a circuit
request has been rejected due to an invalid CCT.  The CCT MO now states
that a CCT MO must be present in the ICRM, or the Node Code will reject
the circuit (with a bad ICRM format ECRI indication).  If the Node Code
detects a non-zero CCT with an auxiliary circuit request, or a zero
CCT with a normal circuit request, the circuit is rejected (with
an invalid CCT ECRI).

In section 4.10.2, the NCN Head has been changed to contain no RMO Sequence
but to simply be a single NCN Head Implicit Sequence.

In section 4.10.3, the NCN Body has been changed to contain no RMO Sequence
but to simply be a single NCN Body Implicit Sequence.  The nCNFlags element
has been removed from the NCN Body.

In section 4.11.1, the LGSC message has an intra-node circuit indication
bit added to the message header flags.

In section 4.12.1, the ISN now has a reserved 16 bit field following
the message size, for use by the Dispatcher in constructing intra-node
circuits.  Also the intra-node circuit bit indication has been added
to the message header flags.  It is now stated that the Node Code
will not divide up ISNs into partial message portions, regardless
of message size and continuation message rules, if the ISN has the
intra-node circuit indication.

The iSNDispSeq element has been restructured, and the origIIX and origPort
elements have been added to the ISIS Dispatcher Sequence.  Also the Aux
circuit indication has been added to the ISN flags.

The Document has been updated on SUPDEV:39.

Thanks....Omar
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 20 Jul 87 14:59:44 PDT 
To: gauci@hobbes (Ed Gauci) 
Subject: Re: ipcf 
In-reply-to: <8707201726.AA06362@hobbes.Tymnet.com> of Mon, 20 Jul 87 10:26:01
	PDT

>  Bill,
>     does ipcfDeassign clean up IPCF directory entries that belong to the
>  deassigned TSAP?
>  Ed.

I hadn't planned on it, but that's an excellent idea.  I shall put it in
the GED and do it.  Thanks for the suggestion.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 20 Jul 87 15:03:54 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, o.k. I concur that k_reset()"... 
In-reply-to: <8707201841.AA09682@calvin.Tymnet.com> of Mon, 20 Jul 87 11:41:06
	PDT

Well, I guess its okay to make it asynchronous - that is for rkBootStrap
to return to the caller after doing a sprout.  The caller may not expect
it, so we have to make sure its well documented or he might do something
rude like crash.  Now that you mention it I remember that we talked about
doing a k_reset from rkBootStrap and ruled it out because there would be
no way to do, for example, a "running dump".

I'm sorry I was confused about all this.  The sprout idea is good.  It
would help me if I had a copy of the matrix we did on the board that one
day.  Somebody was going to put that in the RED, probably Pat, I'll check
with him.

-Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Wed, 22 Jul 87 12:07:50 PDT 
To: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>, Ken Holcomb
	<NTD.K/HOLCOMB@Ontyme.Tymnet>, Mike Rude <NTD.M/Rude@Ontyme.Tymnet>,
	Betty Jo (BJ) Chang <NTD.B/CHANG@Ontyme.Tymnet>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme.Tymnet>, Dennis Ernst
	<NTD.D/ERNST@Ontyme.Tymnet>, John Kopf <KopfJ@B39.Tymnet>, Dan Lasater
	<DLasater@B39.Tymnet>, Jeff Oliveto <ENS.J/OLIVETO@Ontyme.Tymnet>,
	Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>, Romolo Raffo
	<NTD.R/RAFFO@Ontyme.Tymnet>, Dick Rawson <NTD.D/RAWSON@Ontyme.Tymnet>,
	Adele Newman <NTD.A/NEWMAN@Ontyme.Tymnet>, Dave Smith
	<NTD.D/SMITH@Ontyme.Tymnet>, DChang, Gazel Tan <GAZEL@B39>, Paul Chu
	<NTD.P/CHU@Ontyme.Tymnet>, Diana Tai <NTD.D/TAI@Ontyme.Tymnet>, Bill
	Soley <WRS@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>, Ashok Chippa
	<ACHIPPA@B39>, Henry Rivers <NTD.H/RIVERS@Ontyme.Tymnet>, Nancy Holt
	<NTD.N/BURRIESCI@Ontyme.Tymnet>, Dennis Jonas <JONAS@B39>, Scott Wedel
	<NTD.S/WEDEL@Ontyme.Tymnet>, Wayne Bartlett
	<TYM.W/BARTLETT@Ontyme.Tymnet>, Rosie Hull <NTD.R/HULL@Ontyme.Tymnet> 
Subject: LOGII GED UPdate 

Hello Folks,

The LOGII GED has been updated to as follows:

The logon success indication in the CCN and the LGSC message now contains
the Shut Override indication.

The Document has been updated on SUPDEV:39 and OSERANG:39.

Thanks....Omar

Received: from tymix.Tymnet by B39.Tymnet; Wed, 22 Jul 87 12:15:21 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00406; Wed, 22 Jul 87 12:17:19 PDT
Received: by eunix.Tymnet (5.51/4.7) id AA00610; Wed, 22 Jul 87 12:04:26 PDT
Return-path: <kopfj@eunix> 
From: kopfj@eunix (John Kopf) 
Date: Wed, 22 Jul 87 12:04:26 PDT 
To: skywalker@eunix 
Message-id: <8707221904.AA00610@eunix.Tymnet> 
Subject: New IIX Global Message type 

	file:	sky_doc/iix

In response to an ancient request, it would be possible for the
SKYWALKER Dispatcher to clear IIX circuits with IIX messages
rather than the current "out of channels" etc. text-strings.

The proposed solution is to send (for IIX circuits) the sequence:
		SIIX; 80 + 88 + term-reason; detach;
(where 1-byte term-reason corresponds to those defined in the
accounting document), (rather than the text-string followed by the detach).

Please verify that global message 0008 (8088) is not currently in use--
If available, I will document this usage in CIRCUIT.DOC, and implement
the necessary code.

If I receive no objections by August 1, 1987, I will assume approval
and proceed.

John Kopf
x6305

Received: from tymix.Tymnet by B39.Tymnet; Wed, 22 Jul 87 13:11:42 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00547; Wed, 22 Jul 87 13:09:33 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA20912; Wed,
	22 Jul 87 13:08:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00543; Wed, 22 Jul 87 13:09:07 PDT
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA20534; Wed,
	22 Jul 87 12:51:39 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA27194; Wed,
	22 Jul 87 12:51:29 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Wed, 22 Jul 87 12:51:29 PDT 
To: oliveb!tymix!wrs 
Message-id: <8707221951.AA27194@oliven.ATC.OLIVETTI.COM> 
Subject: That topic again 


This just came across the NET and you might find some of
it interesting...

--------------------------
Path: oliveb!pyramid!amdahl!ames!rutgers!princeton!udel!rochester!pt!vlsi.cs.cmu.edu!tsa
From: tsa@vlsi.cs.cmu.edu (Thomas Anantharaman)
Newsgroups: sci.med,soc.men
Subject: Re : Circumcision
Message-ID: <1069@vlsi.cs.cmu.edu>
Date: 22 Jul 87 04:53:02 GMT
Distribution: na
Organization: Carnegie-Mellon University, CS/RI
Lines: 281
Xref: oliveb sci.med:2648 soc.men:1295
Posted: Wed Jul 22 00:53:02 1987

Craig Werner Writes :

>	What you must remember is that circumcision started out as a
> religious ritual, and remains one today, perhaps moreso, if you look
> at the tone of arguments given by people who are against the operation.

While true if refering to Jewish or Moslem circumcision, the origins of male
cirumcision in USA are not religious and some history is instructive.
Believe it or not, circumcision was introduced in USA in the 1870's by the
medical profession to curb the practice of masturbation. At that time the US
medical profession considered masturbation to be one of the leading causes
of insanity. The practice of routine circumcision of males to prevent
insanity caught on gradually in USA, Great Britain, Canada and Australia,
but almost nowhere else.  

Not only was the medical profession mistaken in believing that masturbation
causes insanity, they were also mistaken in believing that circumcision
makes masturbation impossible.  The belief was that the exposed glans would
be so painfully sensitive to the direct touch that masturbation would be
impossible. They ignored the possiblity of the glans getting desensitized on
exposure, or of the use of artificial lubrication to reduce the friction. It
should be pointed out that circumcision was also advocated for girls, but
only if caught masturbating. In the few cases where circumcision was found
not to be effective in curbing masturbation (ie parents caught Junior at it
again), doctors did not hesitate to take more drastic steps : In the case of
girls this ussually involved cliteridectomy, while in the case of boys
blistering of parts of the penis, surgical clamps and sutures on the glans
which made any erection very painful, and in some cases amputation of the
penis was tried. (As an aside I sometimes wonder what kind of family life
Americans had 100 years ago with parents spying on their children in order
to find evidence indicating the need for mutilating surgery !).

By the turn of the century European psychiatrists of Freud's school managed
to convince the English speaking medical world that insanity was not an
incurable disease, so the more barbaric forms of multilation mentioned above
disapeared. However US doctors continued to advocate the more harmless form
of male circumcision since they still felt it was worthwhile in preventing
insanity, even if it was curable. The prevalance of male circumcision
continued to grow in USA as the following statistics indicate :

Year		Estimated Circumcision Frequency

1870		5
1880		10
1890		15
1900		25
1910		35
1920		50
1930		55
1940		60
1950		70
1960		75
1970		80
1979		85

It was not until the 1930's that the medical profession managed to convince
itself that masturbation was entirely harmless. By then the practice of
circumcision had developed a momentum of its own and the medical profession
hated to admit that they had been performing an entirely unnecessary
operation on millions of people, so they suddenly "discovered" a whole host
of medical reasons to justify this operation.  Most of the medical research
purporting to show the medical benefits of circumcision date from the 1930's
and 40's. This research was accepted with relief and rather uncritically and
so the US medical profession continued advocating routine male circumcision.

Medicine was socialized in Great Britain around 1945 and the Government
ruled that male circumcision was not covered, and would have to be paid for
by the patient. The practice of medical circumcision disapeared almost
overnight from Britain, but continued to grow in USA, Canada and Australia.

In the 1960's and 70's more careful research failed to validate earlier
results of the medical benefits of circumcision, but most of the medical
profession in USA wasn't paying any attention. The exception were
Pediatricians who had to handle the complications resulting from this
operation that is usually done by the Gynocologist delivering the baby.
Their national association passed a resolution in 1975 calling for an end to
routine male circumcision. But they failed to change the stand of the
American Medical Association (AMA), largely because of opposition from the
Gynocologists. 

However the AMA finally changed its position in 1985, so that the official
position of the AMA today is that there is no medical reason for routine
circumcision.  This decision received no publicity, and parents considering
circumcision were rarely informed by their doctor of the change in the AMA's
position.

The AMA stand finally received a lot of publicity in 1986 when Blue
Cross/Blue Shield decided that they would no longer pay for circumcision,
refering complaining customers to the AMA opinion. The issue was even
debated on TV in the Donaghue(?) show sometime last month.  I didn't see it,
but apparantly Donaghue was trying to play it up as the medical scandal of
the decade, with the medical profession performing an unnecessary (albeit
harmless) operation on close to 100 million people.

One hopes that circumcision will disappear from USA the way it did from
Great Britain in the 1940's, but this is not certain.  Medical insurance
stopped paying for circumcisions in Canada sometime in the 1960's (anyone
know exactly ?) but the circumcision rate has only been dropping slowly at a
rate of about 5% a decade. Unlike in the case of Britain in the 1940's, the
majority of fathers in Canada and USA are already circumcised, and may be
unable to psychologically accept that their circumcision was unnecessary, or
feel embarassed at showing themselves in front of their uncircumcised sons
in the nude.

> 	In countries without running water, where circumcision is
> unclearly divided on religious grounds (Jewish versus Moslem versus
> virutually everyone else), the differential rate of penile cancer
> is enormous.  In countries that have running water, this difference
> disapears - uncircumcised men have penile cancer no more frequently
> than circumcised men (by the way, in rural China, the rate of penile
> cancer is 8% over a lifetime, except among China's moslems, where it
> is much lower.)

The latest evidence does not support the claim that circumcision prevents
penile cancer. To start with the incidence of penile cancer in countries
that do not practice circumcision is generally no higher than in USA where
most males are circumcised. The following are the number of new cases per
100 000 male population per year:

USA :		0.8
Sweden :	1.1
Norway :	1.1
Finland:	0.5
Denmark:	0.9
(National Cancer registries;for USA American Cancer Society, May 26,1972).

Most of the evidence purporting to show that circumcision protects against
penile cancer is based on poor studies. The following is one such example :
(Quoted from reference [1] below):  "In 1961, Dr Samuel Licklider reported
on the 131 cases of penile cancer treated at the Memorial Hospital for
Cancer and Allied Diseases in New York City in the 40-year period from 1919
to 1959. The distribution of these 131 cases by religion revealed that 4
were Jews. According to Dr. Licklider, this table demonstrated the  low
incidence of penile cancer among Jews. The reason given was circumcision.
[However the] circumcision status was not determined for any of the 131
cases." The frequency of circumcision amongst non-jews in USA in 1960 was
around 75%, but this fact was totally ignored in the study !

Note that the total deaths from penile cancer in USA in 1977 was 225,
compared to 540 deaths due to cancer of the clitoris & labia. (American
Cancer Society, July 27, 1977). The ratio between male and female genital
cancer is similar in countries that do not practice male circumcision. In
Sweden the number of deaths in 1968 were 43 due to penile cancer and 121 due
to cancer of the clitoris & labia (The Cancer Registry, Sweden, 1971).
Anyone claiming that circumcision prevents penile cancer is going to have to
explain why the same does not apply to cancer of the clitoris & labia.

Craigs reference to the rate of penile cancer China is illustrative of the
way annecdotal evidence gets promoted to the status of accepted fact.  This
statistic is referenced in numererous medical papers, but the source given
is usually another medical papar which in turn gives yet another medical
paper as its source. 

Actually anyone quoting this statistic could have used common sense to rule
out its validity. The Peoples Republic of China does not collect any
statistics on the incidence of penile cancer and until recently foreigners
were excluded from collecting first hand evidence. Moreover the 8% figure is
off by several orders of magniture from any reasonable value. As given above
the anual rate of penile cancer (new cases not deaths) in the USA and Europe
is about 1 per 100000 males.  Given a life expectance of 70 years this works
out to 0.07% of the male population over their lifetime (the cancer is fatal
in only about 1/3 of such cases). This makes the 8% figure for China over
100 times that of USA and Europe !

The reference below did manage to track down the original source of the 8%
statistic to a doctor who got the statistic at an international conference
during the course of a conversation with a doctor from China. The latter
told him that some 8% of the patients in his clinic had penile cancer, and
that none of them were moslem.  If I remember correctly the number of
patients was 28 (certainly less than 100), so not only is the 8% figure
statistically insignificant, but it is terribly skewed because the sample
included only patients of this particular clinic who were there because they
had some chronic disease.


> 	It's clear, however, that in the United States, circumcision
> is not a medical neccesity (running water appears to be nearly
> universal), so circumcision should be decided for or against for
> other reasons.

There seems to be an implication that in the absence of running water (or
personal hygeene) circumcision is medically beneficial, which in the absence
of any scientific evidence is highly questionable. One reason for doubt is
that in Europe all men are tought to regularly wash the foreskin, while
women rarely bother to wash the foreskin of the clitoris.  Gynocologists
report that they routinely find significant amounts of Smegma on the
foreskin of women, and the glans of the clitoris is often irritated and
inflamed on acount of this. Yet there has never been any suggestion that
European women are in any additional danger of cancer of the clitoris on
account of this neglect.

> 	To call it mutilation is a bit excessive, however.  For one,
> in many cases, the foreskin retracts on its own during normal growth.
> Two, a sizeable number (10-20%) of uncircumsized men actually run
> into problems when the foreskin does not retract normally (adherence).
> This makes erection painful by forming a constrictive ring, which
> actually can cause tissue damage in some cases by blocking off blood
> flow. The foreskin clearly is an expendable piece of tissue, sometimes
> even better off by its absence.

In the first place the foreskin is almost never retracted in a normal
uncircumcised male though it will often retract when the penis is erect.
(maybe that's what Craig meant by "growth" :-))

Second Craig is confusing the problem of adherance, in which the foreskin
partly adheres to the glans, with the unrelated problem of phimosis, in
which the foreskin is so tight as to make errection painful. 

Almost all males are born with the foreskin adhering to the skin of the
glans, a condition that normally disappears only with puberty. Contrary
to widespread belief in USA it is a mistake to try to retract the foreskin
of an uncircumcised male before puberty, since this will almost always
result in part of the glans being "skinned", producing a very painful wound.
In fact in male infants that are circumcised, the "skinned" glans is
perhaps more painful than the actual circumcision wound, (particularly since
the wound often gets soaked in urine).

Likewise the foreskin of all males tends to be tight for some time during
puberty when the penis increases in size rather suddenly. Normally frequent
involuntary erections during this period insure that the foreskin stretches
to accomodate the growth of the penis. Even when the foreskin does not
stretch sufficiently it can usually be treated by regulary stretching the
foresking by hand, and only in extreme cases is circumcision required.


> 	But as I said, rational argument one way or the other, will
> generally not dissuade anybody.  I just think calling it mutilation is
> a little strong a term.

Circumcision can be viewed as harmless since none of the NATURAL functions
of the foreskin (protecting the glans, and as a source of pheromes) are
important in today's world.

On the other hand the foreskin clearly enhances the pleasure of
masturbation, since no messy lubricant is ever required to reduce the
intensity of direct contact with the glans. For a circumcised male
masturbation is only a poor substitute for sex, while for an uncircumcised
male, masturbation is a reasonable alternative to sex. I hope I haven't
encouraged some fundamentalist parents to have their son(s) circumcised :-)

What is really barbaric is that this operation is invariably performed on
infants without the benefit of any anaesthesia in USA. The prime motivation
for this practice is to avoid the mortality risk associated with the use of
any anaesthesia. From the doctors point of view it is a simple tradeoff
between the risk of being sued by the parents if the patient dies, and the
non-existent risk of being sued by the patient for pain and suffering he
cannot remember. Question : are other surgical operations in USA likewise
performed on infants without anaesthesia ?

Most doctors in USA seem to believe that the pain of infant circumcision
does no permanant damage. However there is evidence to the contrary. To
check if male and female behaviour is genetically differentiated, the
behaviour of male and female infants under the age of one was tested (in
USA). The surprising result of the research was that even at that age male
and female infants responded in dramatically different ways to external
stimulus.  Female response was characterized by passive curiousity. Male
infants tended to react much more agressively, taking evasive action if
anything got too close. However when European researchers attempted to
repeat the experiments, they found they could not duplicate the results. In
fact they found that both male and female infants behave rather like the
female infants in USA. The conclusion they came to was that the very painful
experience most males in USA are subjected to within days of being born
makes them permanantly more distrustful of the outside world ! The US
medical establishment continues to deny this interpretation, perhaps out of
some kind of guilt complex, similar to the one which prompted it to
uncritically accept research justifying circumcision in the 1940's.

Reference  :

Circumcision, by Edward Wallerstein, Springer Series : Focus on Men
(1980).

>-- 
> 	        Craig Werner   (future MD/PhD, 3 years down, 4 to go)
> 	     werner@aecom.YU.EDU -- Albert Einstein College of Medicine
>              (1935-14E Eastchester Rd., Bronx NY 10461, 212-931-2517)
>      "... Has Determined That Cigarette Smoking Is Dangerous To Your Health"

Thomas Anantharaman

tsa@vlsi.cs.cmu.edu.arpa
Received: from tymix.Tymnet by B39.Tymnet; Thu, 23 Jul 87 11:03:09 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04533; Thu, 23 Jul 87 11:04:57 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00154; Thu,
	23 Jul 87 11:03:25 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA01699; Thu, 23 Jul 87 11:04:37 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 23 Jul 87 11:04:37 PDT 
To: wrs@hobbes 
Message-id: <8707231804.AA01699@f104.sun.com> 
Subject: Re: *.ps 

Yes, the way I use them is as an intermediate form for sunroff.  These
are just postscript files (output from psroff given a .r).
Received: from F29.Tymnet by B39.Tymnet; Thu, 23 Jul 87 12:01:50 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 23 Jul 87 11:50:04 PDT
Return-path: <NSMS.B/CORENSON@EMSNTD.Ontyme.Tymnet> 
From: NSMS.B/CORENSON@EMSNTD.Ontyme.Tymnet 
Date: 23 JUL 87 11:26:03 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I62527@Ontyme.Tymnet 
Subject: New Addition 

 
 
                             M E M O R A N D U M
   
   
   
                                        T Y M N E T
                                        Network Services & Marketing Support
   DATE>    July 23, 1987
   
   TO>      Bill Soley
   
   FROM>    Brant
   
   --------------------------------------------------------------------------
   
   Congratulations!  Glad to hear that mother, baby, and ... father are doing
   fine.  See you next Thursday with Wayne for lunch.
   
   Brant
Received: from F29.Tymnet by B39.Tymnet; Thu, 23 Jul 87 13:04:34 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 23 Jul 87 13:04:14 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 1303PM 23JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 23 Jul 87 13:04:11 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by F29.Tymnet; Thu, 23 Jul 87 13:04:10 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

Received: from tymix.Tymnet by B39.Tymnet; Thu, 23 Jul 87 13:19:41 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04898; Thu, 23 Jul 87 13:22:01 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01773; Thu,
	23 Jul 87 13:20:29 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA01887; Thu, 23 Jul 87 13:21:43 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 23 Jul 87 13:21:43 PDT 
To: wrs@f104 
Message-id: <8707232021.AA01887@f104.sun.com> 
Subject: chbar.awk 

Okay, you can change it since it's read only (thus only a copy?!).
.PS = 1, .PE = 0.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 23 Jul 87 16:06:42 PDT 
To: rmurphy@f104 
Subject: Returned MAIL 

(begin forwarded message)
----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Thu, 23 Jul 87 13:04:34 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 23 Jul 87 13:04:14 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 1303PM 23JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 23 Jul 87 13:04:11 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by F29.Tymnet; Thu, 23 Jul 87 13:04:10 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

----------------------------------------------------------------
(end forwarded message)

Hmmm?
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 23 Jul 87 16:08:27 PDT 
To: rmurphy@f104 (Ron Murphy) 
Subject: Re: chbar.awk 
In-reply-to: <8707232021.AA01887@f104.sun.com> of Thu, 23 Jul 87 13:21:43 PDT

Done, its the real thing.  Its only writable by the author.  The big
red flag is when its writable by nobody.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 23 Jul 87 16:38:31 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05805; Thu, 23 Jul 87 16:40:40 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA02051; Thu, 23 Jul 87 16:40:24 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 23 Jul 87 16:40:24 PDT 
To: WRS@B39.Tymnet 
Message-id: <8707232340.AA02051@f104.sun.com> 
Subject: Re: Returned MAIL 

Looks like I forgot the conditional, "if mai:from[M] then maiRTS..."
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 23 Jul 87 16:38:40 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: "HAVE BIG PROBLEM RE ARCHIVING"... 
In-reply-to: your message of Thu, 23 Jul 87 11:50:32 PDT

Well, somehow your archive file got set to "AR852" and so it did what
you said and put all your old messages in the file AR852.  I changed
your archive file back to "(BARTLETW)AR<q>Q<y>.MSG" and re-deleted all
the messages in AR852 so they should all be where you expect them to
be, now.  You also have alot of unread mail in your main file, do you
know about that?  If its just because you deleted your .TIX file, be
advised that that bug is fixed and you should never have to do that
again.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 23 Jul 87 18:02:15 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05961; Thu, 23 Jul 87 18:04:29 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05382; Thu,
	23 Jul 87 18:02:57 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA02110; Thu, 23 Jul 87 18:04:12 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 23 Jul 87 18:04:12 PDT 
To: wrs@f104 
Message-id: <8707240104.AA02110@f104.sun.com> 
Subject: TMS 

TMS is being flaky, I put in a program change which may or may not fix it,
so let me know if you get more of those RTS to nobody messages.
Received: from tymix.Tymnet by B39.Tymnet; Thu, 23 Jul 87 18:39:07 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06010; Thu, 23 Jul 87 18:41:26 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05716;
	Thu, 23 Jul 87 18:39:54 PDT
Received: from mac.tymnet.com by calvin.Tymnet.com (3.2/SMI-3.2) id AA09452;
	Thu, 23 Jul 87 18:40:57 PDT
Received: by mac.tymnet.com (3.2/SMI-3.2) id AA01247; Thu, 23 Jul 87 18:36:51
	PDT
Return-path: <seltzer@mac> 
From: seltzer@mac (Les Seltzer) 
Date: Thu, 23 Jul 87 18:36:51 PDT 
To: wrs@calvin 
Message-id: <8707240136.AA01247@mac.tymnet.com> 
Subject: UNITAP question 


Bill,

Where is the new version of UNITAP on system 39.
The code in my directory is years old.

Les
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 23 Jul 87 21:23:21 PDT 
To: rmurphy@f104 (Ron Murphy) 
Subject: Re: TMS 
In-reply-to: <8707240104.AA02110@f104.sun.com> of Thu, 23 Jul 87 18:04:12 PDT

ok thanks -bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 23 Jul 87 21:30:33 PDT 
To: seltzer@mac (Les Seltzer) 
Subject: Re: UNITAP 
In-reply-to: <8707240134.AA01243@mac.tymnet.com> of Thu, 23 Jul 87 18:34:43 PDT

These are the current locations of UNITAP.  All seem out of date except the
one in (CMF:39).  I'd suggest deleting all the old ones, and puting the
correct one someplace convenient such as XEXEC.  Actually, I'll do that,
then people with the proper init program can just say "UNITAP" and don't
have to worry where it is.  I'll leave deleting old versions to you if
that is what you want to do - after all we don't want people flapping
tapes.  -Bill

(NDT         :  39) UNITAP.CMD     2 10-Feb-84 12:05 PIBYIH  ALL  RD  NO
(NDT         :  39) UNITAP.V8B     9 10-Feb-84 16:44 BICHIW  ALL  RD  NO
(NDT         :  39) UNITAP.SAI     9 14-Mar-84 17:21 RANWAM  ALL  RD  NO
(NDT         :  39) UNITAP.SAV    10 14-Mar-84 17:22 KODYAB  ALL  RD  NO
(CMF         :  39) UNITAP.14      9 14-Mar-84 17:21 RANWAM  ALL  RD  NO
(CMF         :  39) UNITAP.SAV    25 13-Mar-87 14:11 PEXHOR  ALL  RD  NO
(CMF         :  39) UNITAP.SAI     9 13-Mar-87 14:10 CAKBEH  ALL  RD  NO
(SELTZER     :  39) UNITAP.SAV    10 16-Apr-84 17:11 KODYAB  ALL  RD  NO
(SELTZER     :  39) UNITAP.SAI     9 16-Apr-84 17:12 RANWAM  ALL  RD  NO
(SELTZER     :  39) UNITAP.V8B     9 16-Apr-84 17:12 BICHIW  ALL  RD  NO
(SELTZER     :  39) UNITAP.CMD     2 16-Apr-84 17:13 PIBYIH  ALL  RD  NO
(JSTIER      :  39) UNITAP.BAK     2 13-Jul-87 16:42 PIZKEP  ALL  RD  NO
(JSTIER      :  39) UNITAP.INS     2 13-Jul-87 16:48 PEWSUR  ALL  RD  NO
Received: from F29.Tymnet by B39.Tymnet; Fri, 24 Jul 87 4:02:16 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Fri, 24 Jul 87 4:04:14 PDT
Return-path: <DAUL@OFFICE-1.ARPA> 
From: William Daul / McDonnell-Douglas / APD-ASD <WBD.MDC@OFFICE-1.ARPA> 
Date: 24 Jul 87 02:15 PDT 
To: SGK.MDC@OFFICE-1.ARPA, RBC.COR@OFFICE-1.ARPA, WRS.MDC@OFFICE-1.ARPA 
Message-id: <MDC-WBD-BV46V@OFFICE-1> 
Subject: Frwd: SS# & Utilities -- a story 
Comment: I thought you might like to read this little episode!

From: {hao!gatech!spaf@ames.arpa  (Gene Spafford)}DDN
Subject: SS# & Utilities -- a story
To: security@RUTGERS.EDU
Identifier: EXT-gatech-spaf-BV447
Length: 1 page(s)[estimate]
Posted: 21-Jul-87 09:42-PDT
Received: 24-Jul-87 01:58-PDT
Routing-info: 

As a matter of principle, I'm one of those people who won't give out my social 
security number when applying for utilities or credit cards. The reasons why 
have been discussed numerous times in various security-related groups.   It is 
my understanding that it is against the law to force someone to give his/her 
social security number unless it is a government agency; although I've often 
run into occasional resistance, a few moments of explanation has usually 
resulted in things working out okay.

Then there's today.  I'm moving to W. Lafayette Indiana in two weeks and I 
called to establish my phone service there.  Indiana is served by GTE for phone
service.  I did not anticipate any problems since I have an excellent credit 
history, as could be verified by a quick check with the local Southern Bell 
folks.  After the rep at GTE took all my information down, she asked for my 
SS#.  I explained that I don't give that out.  She informed me that I would be 
required to pay a $75 deposit if I refused to give my SS#.  So, I asked to talk
to her supervisor.

Her supervisor repeated that I would have to give my SS# to waive the deposit. 
I asked if they could simply call Southern Bell or take a credit card #, or 
they could call Purdue and verify my employment.  He said that wasn't enough --
I had to supply my SS#, no other option.  I enquired as to why they needed it 
-- he said it was for a credit check and to verify future disconnect requests. 
I explained that they could do that self-same credit check without the SS# 
*and* I don't give out my SS# precisely because I don't want it used as a 
verification number on my account.   He insisted I either supply the number or 
pay the deposit.  He also asked why I was being so stubborn -- it was even on 
my driver's license, wasn't it?  (It isn't -- and hasn't been.  In Georgia, you
have always had the option of having a different ID, and now the licenses are 
being issue with those as default.  The guy at GTE claims that the Indiana 
licenses are *required* to have the SS# on them -- anyone know if this is true?
 It shouldn't be...) I explained that having done some work in computer 
security, and personal experience, I know how that number can be abused.  He 
said I was the only person he'd ever run into to refuse to give the SS# (!).

I then asked him if the requirement for a SS# was written policy -- I wanted a 
copy to examine.  He informed me that such information was private to the 
company and I couldn't have a copy -- didn't I trust him?  I then asked if that
policy was on file with the state Public Service Commission.  At that he 
(rather loudly) asked if I wanted service with GTE or not?  I asked him very 
calmly if he was threatening to deny me service -- he quieted down.  I next 
explained that I wanted to see a copy of the written policy because it would be
interesting to include in an article I might write on improper use of SS#s.  He
became very quiet.  I offered to find the name and number of someone at 
Southern Bell who could verify my 9 years of service here.  He said to call 
back with that information (thankful to get rid of me, I guess).

The lady I talked to at Southern Bell was very helpful.  She informed me that 
all the Southern Bell operators are told not to force a SS# because it is 
against both policy and law, but if someone won't provide it they are to get a 
bank account # or credit card number (both of which I am willing to give in 
circumstances such as this).  She was more than willing to talk to the 
supervisor at GTE and give him a credit reference, if only he'd call.  She said
she'd also fill him in on policy.  *AND*, most interestingly, Southern Bell had
somehow obtained my SS# through other means and it was on file, but she marked 
it so that it was not to be given out to anyone, specifically not anyone with 
GTE Indiana. :-)

Back to GTE.  I called the supervisor (collect, of course) and gave him the 
name and number of the lady at Southern Bell.  He was very curt and said he'd 
probably still require a deposit.  He hung up on me.

20 minutes later the original GTE operator called me back and cheerily informed
me that my service would be turned on August 4 with *no* deposit required!

Questions

---------

1) Do many of you (net-readers) withhold your SS# in similar circumstances?

Do you have these kinds of confrontations too?

2) Anyone know if other people at GTE Indiana are such jerks, or is

this an isolated instance?

3) Anyone know if Indiana does, in fact, *require* that the SS# be

on the driver's license?

4) Should I bother to follow-up on this further?  That is, should I bother 
contacting the Public Service commission in Indiana about the treatment I 
received? (I'm currently not sure it is worth the effort).

Too bad we don't have a choice of phone companies as well as long

distance carriers -- I'd keep Southern Bell.

--

Gene Spafford

Software Engineering Research Center (SERC), Georgia Tech, Atlanta GA 30332

   Internet:    spaf@gatech.gatech.edu

   uucp:        ...!{decvax,hplabs,ihnp4,linus,rutgers,seismo}!gatech!spaf

Received: from F29.Tymnet by B39.Tymnet; Fri, 24 Jul 87 8:21:54 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Fri, 24 Jul 87 8:09:01 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 0808AM 24JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Fri, 24 Jul 87 8:08:44 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by F29.Tymnet; Fri, 24 Jul 87 8:08:44 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

Received: from F29.Tymnet by B39.Tymnet; Fri, 24 Jul 87 10:22:05 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 24 Jul 87 10:20:28 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 24 JUL 87 10:16:01 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I63034@Ontyme.Tymnet 
Subject: "DATE: July 22, 1987 TO: Bubb Rd."... 


DATE:  July 22, 1987

TO:    Bubb Rd. and Orchard Pkwy. Employees

From:  Greg Nauman

CC:    John Baumgarten, Gina Zwaagstra


In preparation for the upcoming move to the new South Bay Center,
furniture that is to be used in the new complex is being tagged by our
architects.  This process has began on Bubb Rd. and will continue on
Thursday (7-23-87) on Bubb Rd. and Orchard Pkwy.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 24 Jul 87 17:02:58 PDT 
To: rmurphy@f104 
Subject: Returned MAIL 

(begin forwarded message)
----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Fri, 24 Jul 87 8:21:54 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Fri, 24 Jul 87 8:09:01 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 0808AM 24JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Fri, 24 Jul 87 8:08:44 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by F29.Tymnet; Fri, 24 Jul 87 8:08:44 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

----------------------------------------------------------------
(end forwarded message)

You said if it happens again...
-Bill
Received: from X930.Tymnet by B39.Tymnet; Sun, 26 Jul 87 2:40:53 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 26 Jul 87 2:41:14 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: PDP-10 coding trick 

  In putting together my own octal print routine for my SETOP endeavor
I chose to combine the octal and decimal print routines into one.  This
had the side effect that octal numbers with the sign bit set became
a weird set of punctuation marks (since I had an unsigned print routine).

		  OOPS!

  If you start with a standard routine such as this...

  num = 1
  rem = num+1

OCTOUT:	SKIPA	rad,[^D8]		; octal
DECOUT: MOVEI	rad,^D10		; decimal
RDXOUT: IDIV	num,rad			; divide
	HRLM	rem,(P)			; save remainder on stack
	SKIPE	num			; any more digits?
	  PUSHJ	P,RDXOUT		; yes, loop
	HLRZ	num,(P)			; get digit from stack
	MOVEI	num,"0"(num)		; convert to digit
	OUTCHR	num			; print it (with routine of choice)
	POPJ	P,			; return when done

  I came up with this interesting bit of code as a work around that I had
never encountered in any bag of PDP-10 coding tricks...

OCTOUT:	SKIPA	rad,[^D8]		; octal
DECOUT: MOVEI	rad,^D10		; decimal
; ** begin trick **
	EXCH	num,rem			; setup number in remainder word
	SETZ	num,			; for 2-word divide instead
	LSHC	num,1			; shift sign (if any) to high word
	LSH	rem,-1			; shift low word back into position
	DIV	num,rad			; first divide, results same as IDIV
	CAIA				; or JRST .+2 to skip first divide
; ** end trick **
RDXOUT: IDIV	num,rad			; divide
	HRLM	rem,(P)			; save remainder on stack
	SKIPE	num			; any more digits?
	  PUSHJ	P,RDXOUT		; yes, loop
	HLRZ	num,(P)			; get digit from stack
	MOVEI	num,"0"(num)		; convert to digit
	OUTCHR	num			; print it (with routine of choice)
	POPJ	P,			; return when done

; Have you ever seen this used anywhere before?

PS.  TYMCOM-X only...
    replace
	MOVEI	num,"0"(num)
	OUTCHR	num
    with
	OUTCHI	"0"(num)		; outchr immediate

/Carl
Received: from tymix.Tymnet by B39.Tymnet; Sun, 26 Jul 87 17:17:39 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13460; Sun, 26 Jul 87 17:20:09 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA03123; Sun,
	26 Jul 87 17:18:25 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA01312; Sun, 26 Jul 87 17:19:49 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Sun, 26 Jul 87 17:19:49 PDT 
To: wrs@f104 
Message-id: <8707270019.AA01312@f104.sun.com> 
Subject: TMS RTS bug 

Okay, I think I fixed this bug where TUMS tries to RTS to nobody.  I was 
doing the usual check whether mai:From[M] was null except in this case
there was a way for it to be set up as "@TMS.Tymnet".  I prevented this
case.  If there are STILL problems after the date of this mail, let me
know...
Ron
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 26 Jul 87 17:20:36 PDT 
To: rmurphy@f104 
Subject: Returned MAIL 

(begin forwarded messages)
----------------------------------------------------------------
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 15:18:06 PDT 
To: Postmaster@B39.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 1519PM 26JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 15:18:02 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by B39.Tymnet; Sun, 26 Jul 87 15:18:02 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

----------------------------------------------------------------
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 15:25:16 PDT 
To: Postmaster@B39.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 1527PM 26JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 15:25:11 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by B39.Tymnet; Sun, 26 Jul 87 15:25:11 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

----------------------------------------------------------------
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 15:37:45 PDT 
To: Postmaster@B39.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE60  at 1539PM 26JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 15:37:40 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by B39.Tymnet; Sun, 26 Jul 87 15:37:40 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

----------------------------------------------------------------
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 15:43:14 PDT 
To: Postmaster@B39.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 1545PM 26JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 15:43:09 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by B39.Tymnet; Sun, 26 Jul 87 15:43:09 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

----------------------------------------------------------------
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 16:00:15 PDT 
To: Postmaster@B39.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 1602PM 26JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@B39.Tymnet 
Date: Sun, 26 Jul 87 16:00:01 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by B39.Tymnet; Sun, 26 Jul 87 16:00:01 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Sun, 26 Jul 87 16:01:18 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Sun, 26 Jul 87 15:44:17 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
The request was rejected by TMS.Tymnet
The response was:
:SEND 
Message rejected - invalid send field of   from LINE27  at 1543PM 26JUL87 .


----------
RCPT To: <@TMS.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Sun, 26 Jul 87 15:24:42 PDT 
To: @TMS.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected as
'TO: [TUMS-address]' in first line of message body.

----------
Received: from TMS.Tymnet by F29.Tymnet; Sun, 26 Jul 87 15:24:42 PDT
From: @TMS.Tymnet 
Message-id: @TMS.Tymnet 

----------------------------------------------------------------
(end forwarded messages)

...
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Sun, 26 Jul 87 18:05:31 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13512; Sun, 26 Jul 87 18:07:48 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA01466; Sun, 26 Jul 87 18:07:30 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Sun, 26 Jul 87 18:07:30 PDT 
To: WRS@B39.Tymnet 
Message-id: <8707270107.AA01466@f104.sun.com> 
Subject: Re: Returned MAIL 

Thanks... you probably realize those are old (prior to my last message).
Received: from tymix.Tymnet by B39.Tymnet; Sun, 26 Jul 87 19:58:42 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13658; Sun, 26 Jul 87 20:01:03 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA03398; Sun, 26 Jul 87
	19:59:13 PDT
Return-path: <feldman@hobbes> 
From: feldman@hobbes (Steve Feldman) 
Date: Sun, 26 Jul 87 19:59:13 PDT 
To: alexting@hobbes, baba@hobbes, davarj@hobbes, jennys@hobbes, wrs@hobbes 
Cc: foil@hobbes, julies@hobbes 
Message-id: <8707270259.AA03398@hobbes.Tymnet.com> 
Subject: Dual home directory changes 

You currently have a special home directory on opus in addition to your
normal one on calvin or hobbes, or a special home on calvin and your
normal home on opus.  When the Sun servers are transferred to N. First
St., all three servers will be sharing files via NFS, so there will be
no need to keep these special homes, and they will be deleted.

In preparation for the move, which is currently planned for Tuesday
July 28, please copy any files you wish to keep back to your primary
server.  If you have too much to copy easily, please let me know as
soon as possible so we can make special arrangements.  In any case,
please acknowledge this message so we won't worry that you don't know
about this.

	Steve
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 26 Jul 87 20:38:55 PDT 
To: rmurphy@f104 (Ron Murphy) 
Subject: Re: Returned MAIL 
In-reply-to: <8707270107.AA01466@f104.sun.com> of Sun, 26 Jul 87 18:07:30 PDT

Somehow I read the messages out of order.  Sorry.  Be seeing you.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 26 Jul 87 20:40:44 PDT 
To: feldman@hobbes (Steve Feldman) 
Subject: Re: Dual home directory changes 
In-reply-to: <8707270259.AA03398@hobbes.Tymnet.com> of Sun, 26 Jul 87 19:59:13
	PDT

Ack.  The files I have on opus are only copies of selected files from my
home system and may be deleted at your convenience.  Thanks for checking.
-Bill
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 10 Jul 87 11:01:30 PDT 
To: WRS@C39.Tymnet 
Subject: "QUESTION IN YOUR SPARE TIME(???)-"... 

QUESTION IN YOUR SPARE TIME(???)- I HAVE A 6/6/87 MEMO FROM CARL RE
SETOP PASSWORDS - WAS REVIEWING FOR OUR LUNCH - LOOKS LIKE HE IS
RESPONDING TO A MEMO (AT LEAST SOMETHING) FROM YOU. WAS THERE A MEMO??
IF SO COULD YOU COPY ME??????
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 10 Jul 87 11:24:50 PDT 
To: Wayne D. Bartlett <BARTLETW@B39.Tymnet> 
Subject: SETOP mail (old) 

Here is all the interesting back-mail on SETOP with Carl.  I hope this
matches your questions, but I don't know the subject so I can't tell.
-Bill

----------------------------------------------------------------
Received: from X930.Tymnet by B39.Tymnet; Sat, 6 Jun 87 16:20:41 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sat, 6 Jun 87 16:18:17 PDT 
To: William R Soley <WRS@B39.Tymnet>, Wayne D Bartlett <BARTLETW@B39.Tymnet> 
Subject: Re: SETOP passwords 

Of course!  A manager need not know the old password to change someone under
him.  How else could he set the password or change it, when an employee came
and had forgotten his password!!!

YES!!! I agree it should be changed... it should work like PASSWORD does now!
When a manager runs setop... before he can do anything, he must give his
own password... (and if you don't have WC set, I believe it does ASK for OPER
name and password).  Bill, you have enough license that you were not validated
when you changed the password.

Anyway, it makes sense that anyone doing anything should validate himself and
super licenses should still require the validation.  At present, if WRS or CARL
validated themselves, then they could only change people under them... that's
the way SETOP works.

We will have "super-user" status still.. but validatin is required to produce
a proper audit trail. So we should include the validation... and maybe.. just
maybe provide a way in to re-create the world if something happens to th
password file and it needs reconstruction.

/Carl
----------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 7 Jun 87 18:09:22 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Wayne D Bartlett <BARTLETW@B39.Tymnet> 
Subject: Re: SETOP passwords 
In-reply-to: your message of Sat, 6 Jun 87 16:18:17 PDT

I think you misunderstood.  I was saying that SETOP currently behaves properly.
It DOES require your password before you change anyone else's.  It either
requires it when you set WC with the OPER program, or when you enter SETOP.
Both would give you an audit trail if SETOP kept one.  I see no need for any
change to the procedure.  -Bill
----------------------------------------------------------------
Received: from X930.Tymnet by B39.Tymnet; Tue, 30 Jun 87 14:22:48 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
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
----------------------------------------------------------------
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 13 Jul 87 15:32:37 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: William Christian Soley 
In-reply-to: your message of Sat, 11 Jul 87 5:11:54 PDT

GREAT!!! CONGRATS - AND LIFE WILL BE DIFFERENT NOW!!!!!!!!
CAN WE RESCHEDULE LUNCH FOR MON,7/20????????
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 20 Jul 87 9:34:24 PDT 
To: WRS@C39.Tymnet 
Subject: "GOT NO FEEDBACK ON 7/20 LUNCH"... 

GOT NO FEEDBACK ON 7/20 LUNCH - ASSUME OFF - CAN YOU SUGGEST DATE - CAN PROBABLY GET BRENT - THIS WEEK OUT - BUT MON, THURS, OR FRI OK NEXT WEEK
DO YOU RECALL THE NAME AND PHONE (OR SOMETHING)ABOUT THE PERSON WHO KNOWS PEAK?
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 21 Jul 87 10:05:39 PDT 
To: WRS@C39.Tymnet 
Subject: "ARE YOU BACK ON THE WAVES"? IF"... 

ARE YOU BACK ON THE "AIR WAVES"? IF SO, HOW ABOUT RESPONDING TO MON'S MEMO??
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 21 Jul 87 12:41:18 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: "GOT NO FEEDBACK ON 7/20 LUNCH"... 
In-reply-to: your message of Mon, 20 Jul 87 9:34:24 PDT

Right.  Thursday of next week would be best for me.  Mon/Fri are almost
always difficult.  Would like to see Brant, too.  

The PEAK person is Ken Dawson.  Let me see if I have his phone number...
Nope.  I have his last two business numbers, but not his current
business number or home number.  Try Carl.

-Bill

PS - sorry I'm not all here - between Skywalker and William C. I think
I'll never get any sleep!  And then there is the repeater project...
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Wed, 22 Jul 87 8:44:03 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: "GOT NO FEEDBACK ON 7/20 LUNCH"... 
In-reply-to: your message of Tue, 21 Jul 87 12:41:18 PDT

GREAT - SEE YOU THURS 7/30 - LETS DECIDE WHERE DEPENDING ON BRANT -
e.g. IF HE CAN JOIN US. THANKS FOR THE NAME AT LEAST - MORE URGENT TO 
SOLVE THE PROBLEM THAN EVER!!!
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Thu, 23 Jul 87 11:50:32 PDT 
To: WRS@C39.Tymnet 
Subject: "HAVE BIG PROBLEM RE ARCHIVING"... 

HAVE BIG PROBLEM RE ARCHIVING FILES!!?? NOTHING IN 2Q87 AFTER 5/15, AND
NO 3Q87 FILE AT ALL!!!! (RESPONSE TO 3Q OPEN WAS 
%fError: file "DSK:AR3Q87.MSG", mode "ro1E:.MSG"
File not found.)
and I will need several of those memo's later!!!!!!!!what do I do??????
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 24 Jul 87 14:02:44 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: "HAVE BIG PROBLEM RE ARCHIVING"... 
In-reply-to: your message of Thu, 23 Jul 87 16:38:40 PDT

FIRST - THURS LUNCH - BRANT WILL JOIN US - 11:30 @ CHILI'S - AND IF MORE
TO THE DISCUSSION - ON TO YOUR OFFICE - BUT WE NEED TO LEAVE @2PM - O.K.??
THANKS FOR THE HELP -QUESTION - WHAT DO YOU CALL MY "MAIN FILE"? (I HAVE
DELETED MY TIX FILE MANY TIMES - YOU MAY RECALL MY 'MANY TIMES' OPENING
MESSAGE "ERROR AT ((SOME LOCATION)), SO I DO DEL TUMS.TIX AND REINITIALIZE
DOES THAT EXPLAIN???
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 24 Jul 87 17:33:40 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: "HAVE BIG PROBLEM RE ARCHIVING"... 
In-reply-to: your message of Fri, 24 Jul 87 14:02:44 PDT

Ok for Jul 30 at 11:30, but I probably won't have an office then.  If
I do it will be at N.1st.

Sent you mail about ACCESS# user name.  We can talk about it then if
you wish, or give me a call if you have a burning desire.

Re: .TIX file, TUMS.MSG is your main file, and TUMS.TIX is your main
index.  It should not be necessary to delete the .TIX file anymore as
the bug that made that necessary has been fixed.  You may still get the
ILL INST AT USER LOCATION 123456
 or
PC OUT OF BOUNDS AT USER LOCATION 123456
message but just reentering TUMS should recover.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 27 Jul 87 4:37:56 PDT 
To: ENS.J/OLIVETO@EMSNTD.Ontyme.Tymnet 
Subject: Re: TYMNET Numbering Plan 
In-reply-to: I63490@Ontyme.Tymnet of 26 JUL 87 04:51:53

Got your document on numbering plan.  Looks very good.  I'm interested
in attending the meeting, need to know time and place.  Thanks -Bill
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 27 Jul 87 9:16:29 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: ACCESS# 
In-reply-to: your message of Fri, 24 Jul 87 17:05:13 PDT

AM UNFAMALIAR WITH THIS SITUATION AND DDD - BUT PERHAPS BEST TO WAIT
AND DISCUSS @ LUNCH THURS.
NEW TOPIC - HAD TO DEL TIX AGAIN TODAY - USUAL ERROR MESSAGE (AS I RECALL)
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 27 Jul 87 9:21:49 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: "HAVE BIG PROBLEM RE ARCHIVING"... 
In-reply-to: your message of Fri, 24 Jul 87 17:33:40 PDT

GREAT - JUST GOT THIS MESSAGE - AFTER SENDING LAST - SO PLEASE IGNORE
PART ABOUT DEL TIX - WILL USE NEW "SOLUTION" FROM NOW ON
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 27 Jul 87 9:34:39 PDT 
To: WRS@C39.Tymnet 
Cc: CARL@X930 
Subject: "ANOTHER TOPIC RELATES TO PEAK"... 

ANOTHER TOPIC - RELATES TO PEAK (AND LOCATING KEN DAWSON) - WOULD LIKE YOUR
THOUGHTS AND COMMENTS ABOUT FOLLOWING RE SOURCE CODE "IMPROVED SECURITY".
AM ALSO COPYING CARL - ALTHOUGH DOUG AMY HAVE COPIED HIM DIRECTLY - 
----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Fri, 24 Jul 87 18:02:06 PDT
Received: from EMSTYM.Ontyme.Tymnet by F29.Tymnet; Fri, 24 Jul 87 18:03:50 PDT
Return-path: <QSATS.D/ELLENBERG@EMSTYM.Ontyme.Tymnet> 
From: QSATS.D/ELLENBERG@EMSTYM.Ontyme.Tymnet 
Date: 24 JUL 87 17:54:17 
To: TYM.W/BARTLETT@EMSTYM.Ontyme.Tymnet 
Message-id: M07903@Ontyme.Tymnet 
Subject: PROPOSAL FOR PROVIDING ENHANCED SECURITY IN THE 

            PROPOSAL FOR PROVIDING ENHANCED SECURITY IN THE
            -----------------------------------------------

     STRUCTURE AND ACCESS CAPABILITIES OF DIRECTORIES ON THE PDP 10
     --------------------------------------------------------------

                          CODE GENNING SYSTEMS
                          --------------------


                            Preliminary Copy

                  Additional Research Being Performed



















                             July 23, 1987

                                   by

                          Douglas G. Ellenberg




Contributions:

Individuals who contributed to the document and their organizations.


Quality Assurance    Hardware Technical Support    Network Technology Division

 Diana Houge             Ed Mooring                     Romolo Raffo
                         Paul Krumviede
                         Ron Vivier


Security             Field Service Company

Wayne Bartlett           Dennis Coffey
                         Carl Baltrunas








Overview:

Security is a major concern in all areas of the Network.  From
restricting access to the Network to restricting the range of
capabilities a user has once a successful connection is made to a
host, security is a concern.  This proposal seeks to address one part
of the security picture.

Today, all users on the PDP Code Genning Systems have the capability to
access the NAD program in the (SYS) directory.  This program gives them
Write File (WF) license.  With this capability, any user has the ability
to over-write any file on the system they are logged into.  With NAD they
are also able to gen code and create listings of the Tymnet source code.

What this proposal seeks to control is really two fold.  First, it will
restrict which users are allowed access to NAD in the (SYS) directory.
This will remove their Write File capabilities and not allow them to
create code listings.  Secondly, it will also restrict the ability to gen
code to only those users who have been identified as requiring this
capability.


Goals and Compromises:

The ultimate goal for the implementation of restricted access was to
disallow access to the (SOURCE) and (BETATEST) directories from all
users except those that require access.  Those requiring access include
all field and Headquarters support people; Project Implementation; the
Field Service Organization; Customers who, by contract, are allowed
access; the Headquarters Quality Assurance group; and the development
organization.  Groups that required access only to a limited set of
files in the (SOURCE) and (BETATEST) directories would then be
restricted on a file by file basis.  For example, Customer A has in
their contract a license to gen X.25, Node Code and ISIS.  They would be
allowed access to only the source for this code and be barred access to
the remaining source code files.

Unfortunately, barring access down to a per file per user basis is not
possible under current TYMCOM-X limitations.  This capability would
require modifications to the Operating System.  What is possible is to
restrict access to the (SOURCE) and (BETATEST) directories as a whole
and restrict access to some of the capabilities of the NAD program. 
Thus, restricting access on a more limited basis is possible.

After the restructuring of the PDP systems, as will be documented,
restriction to the (SOURCE) and (BETATEST) directories will be on
a per user basis.  Certain users will be barred from being able to
create listings of the source code and will be able to only write files
into their own directories.  They will not be able to over-write files
at will as they can today.




Restructuring with Restrictions:

There are copies of the code genning program, NAD, on all PDP Code Genning
Systems.  One NAD is the aforementioned copy in the (SYS) directory.
The second NAD is the copy in the (SOURCE) directory.  The only
difference between these two copies of the same program is the license
assigned to the programs.  The NAD program in the (SYS) directory has
Write File license (WF).  Use of this copy of NAD allows the user to
over-write files anywhere on the PDP system they are accessing.  The
copy of the NAD program in the (SOURCE) directory has only Home File
(HF) license.  This will still allow the user to over-write files in 
directories under the same GAN.  A change to NAD would be required
to restrict this further.[*]

There is also an important feature of NAD that is part of this
restructuring.  If a user lacks TYM license, NAD will not allow that
user to create listings of the source code.  Currently, most users that
gen code have TYM license.  They are able to both gen code and create
listings of the source code.

The changes proposed to create tighter controls on the capabilities of
users on the PDP Code Genning Systems are:

1.  Remove Write File (WF) license from NAD in the (SYS) directory.

2.  Place protections on NAD in the (SYS) directory so it could only be
    called and used by users that have the license required to do so.
    Even users in the same GAN will be restricted unless they have the
    license necessary to access the code.

3.  Structure the customer directories as follows.

    A customer will have a minimum of two directories.  One will be
    under a separate GAN and will be used for CODE STORAGE.  Code that
    is operating in their network will be stored in this directory.  The
    second directory will be under the same GAN as the (SOURCE) and
    (BETATEST) directories.  This directory will be used only for CODE
    GENERATION.  The second, or CODE GENNING directory, will have an
    ACCESS list allowing the customer to GFD into this directory from
    their CODE STORAGE directory.

4.  Remove TYM license from users who should not be able to create
    listings of the source code.

5.  The (SOURCE) and (BETATEST) directories should have directory access
    protection of  SHARABLE and LISTABLE only.  They currently have
    SHARABLE, LISTABLE and NEW FILES.




By making these changes, users not in the (SOURCE) and (BETATEST) GANs,
and without proper license, will not have access to NAD.  They will be
forced to only use the NAD program in the (SOURCE) directory.  Without
the TYM license, they would be unable to create listings of the source
code.  Without the additional licenses on the NAD program they would be
unable to over-write files not in their own directory.  With directory 
access set, users will be prevented from writing into the (SOURCE) and
(BETATEST) directories.

With the current protections placed on all (SOURCE) and (BETATEST)
source code, users without the proper license will not have access to
the files in these directories.  They would be restricted from even
viewing the source files.


Concerns and Questions:

Items marked with a [*] throughout this document require further
research.

PEAK and VUE, the text editors on the PDPs, require modification if they
are to be used by users that do not have TYM license.

Will changing the customer's GAN for the code genning directory cause
any problems?  Do any other programs key off the GAN, etc.

Will the customer be allowed to log into their code genning directory
directly or only be able to access it via a GFD from their storage
directory?

Who will be tracking the change of GANs for the customers?

Who will identify the customers requiring these changes?

Who will be removing the Write File (WF) license from (SYS)NAD and the
Home File (HF) license from (SOURCE)NAD?

An ACCESS.LST file will have to be maintained in the code genning
directories to allow the GFD access from the code storage directories.
Recovery procedures will have to be established for the recovering of
these files after accidental deletion.
----------------------------------------------------------------
CONSIDER THIS AS ANOTHER TOPIC NEXT THURS - 
Received: from F29.Tymnet by B39.Tymnet; Mon, 27 Jul 87 9:58:27 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Mon, 27 Jul 87 9:59:19 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Error response(s) from SLVMCL1.McAuto.Tymnet ...
SNDMSG000R Transmit message, end with '.':
SNDMSG001R Transmit recipients, end with '.':
DMKCSP053E 2330894  NOT IN CP DIRECTORY
SNDMSG003W Send to 2330894 at SLVMCL1 failed due to error 53
SNDMSG004W Send failed to 1 recipients.
SNDMSG005E Note sent to 0 recipients.
R; T=0.07/0.27 11:58:58



----------
RCPT To: <2330894@SLVMCL1.McAuto.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Mon, 27 Jul 87 9:58:48 PDT 
To: LINDA HOLLAND <2330894@SLVMCL1.McAuto.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected
in extended name field or 1st line of body.

----------
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Mon, 27 Jul 87 9:58:44 PDT
From: LINDA HOLLAND <2330894@SLVMCL1.McAuto.Tymnet> 
Date: 27JUL87 1154AM 
To: 132056 @IMSM .MCAUTO.TYMNET <132056@IMSM.McAuto.Tymnet> 
RECEIVED: from TMS.Tymnet by F29.Tymnet; Mon, 27 Jul 87 9:55:46 PDT
MESSAGE-ID: 060295@TMS.Tymnet
SUBJ: TEST

THIS IS A TEST.  LET ME KNOW IF THIS WORKS.
 
SMILE, IT ISN'T THAT BAD.
 
 
LINDA
Received: from F29.Tymnet by B39.Tymnet; Mon, 27 Jul 87 10:10:20 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Mon, 27 Jul 87 10:10:41 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Error response(s) from SLVMCL1.McAuto.Tymnet ...
SNDMSG000R Transmit message, end with '.':
SNDMSG001R Transmit recipients, end with '.':
DMKCSP053E 311/2330 NOT IN CP DIRECTORY
SNDMSG003W Send to 311/2330894 at SLVMCL1 failed due to error 53
SNDMSG004W Send failed to 1 recipients.
SNDMSG005E Note sent to 0 recipients.
R; T=0.07/0.23 12:10:20



----------
RCPT To: <311/2330894@SLVMCL1.McAuto.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Mon, 27 Jul 87 10:10:13 PDT 
To: LINDA HOLLAND <311/2330894@SLVMCL1.McAuto.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected
in extended name field or 1st line of body.

----------
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Mon, 27 Jul 87 10:10:08 PDT
From: LINDA HOLLAND <311/2330894@SLVMCL1.McAuto.Tymnet> 
Date: 27JUL87 1157AM 
To: 2332124@VMA .MCAUTO.TYMNET <2332124@VMA.McAuto.Tymnet> 
RECEIVED: from TMS.Tymnet by F29.Tymnet; Mon, 27 Jul 87 10:06:01 PDT
MESSAGE-ID: 060302@TMS.Tymnet
SUBJ: TEST

THIS IS A TEST
 
SMILE, IT ISN'T THAT BAD.
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 27 Jul 87 14:46:44 PDT 
To: WRS@C39.Tymnet 
Subject: "INVIEW OF OFFICE SITUATION HOW"... 

INVIEW OF OFFICE SITUATION - HOW ABOUT NEAR HERE LUNCH THURS??
YOUR CHOICE - COLILL OR ROGERS OR ????
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 27 Jul 87 15:57:04 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: ACCESS# 
In-reply-to: your message of Mon, 27 Jul 87 9:16:29 PDT

I don't believe deleting the .TIX file is necessary anymore.  Even though
you get that error, it should recover the next time you enter TUMS.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 27 Jul 87 16:02:27 PDT 
To: dave@hobbes (Dave Smith) 
Subject: Re: the Rodney utcGet() function 
In-reply-to: <8707271649.AA04506@hobbes.Tymnet.com> of Mon, 27 Jul 87 09:49:50
	PDT

It will be done in some manner to allow use of MOVEM.L for atomic reads
of utc and fastc.  fastc will "skip" along with utc durring adjustment.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 27 Jul 87 16:05:02 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: SETOP IN ASSEMBLER 
In-reply-to: your message of Mon, 27 Jul 87 13:43:06 PDT

Yes, you can turn of garbage collect and force it to always get more memory.
Set some flag in GOGTAB, not sure right now, its in the manual.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 27 Jul 87 16:05:55 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: "INVIEW OF OFFICE SITUATION HOW"... 
In-reply-to: your message of Mon, 27 Jul 87 14:46:44 PDT

Never mind, it looks like we'll still be here.  Put off til Friday (the
move that is).  Chillis still my choice.  -Bill
Received: from X930.Tymnet by B39.Tymnet; Tue, 28 Jul 87 9:11:27 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 28 Jul 87 9:10:15 PDT 
To: William R Soley <WRS@B39.Tymnet>, Wayne D Bartlett <BARTLETW@B39.Tymnet> 
Subject: Re: (SYS)NAD with WF 

Does anyone know if NAD properly protects the use of WF, or who maintains
NAD so I may talk to them about this.

Otherwise, my vote is that WF be removed immediately from (SYS)NAD, which
I will undertake... not only can anyone read any file, they can also over-
write any file (if only they knew...)

The file is protected ALL RUN RUN.

/Carl
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 28 Jul 87 9:46:17 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: William R Soley <WRS@B39.Tymnet> 
Subject: Re: (SYS)NAD with WF 
In-reply-to: your message of Tue, 28 Jul 87 9:10:15 PDT

PEOPLE WHO KNOW MUCH (MAYBE ALL) ARE ROMOLORAFFO, DON MURTAUGH, AND 
DOUG E. YOU HAVE GOOD ISSUE, ONLY URGE YOU TALK TO THEM BEFORE ANY
ACTION!!!!!!
Received: from tymix.Tymnet by B39.Tymnet; Tue, 28 Jul 87 13:01:20 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19261; Tue, 28 Jul 87 13:05:30 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06221; Tue, 28 Jul 87
	13:03:55 PDT
Return-path: <dave@hobbes> 
From: dave@hobbes (Dave Smith) 
Date: Tue, 28 Jul 87 13:03:55 PDT 
To: wrs@hobbes 
Message-id: <8707282003.AA06221@hobbes.Tymnet.com> 
Subject: Indian Hill style guide 

Lasater says we were going to update the Indian Hill style guide with
our changes.  Did we decide who?  If you, are they done?  If me, do you
know where it is kept online?  I have no problem doing it if you are too
busy.

Dave.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 28 Jul 87 16:05:47 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Wayne D Bartlett <BARTLETW@B39.Tymnet> 
Subject: Re: (SYS)NAD with WF 
In-reply-to: your message of Tue, 28 Jul 87 9:10:15 PDT

NAD is allegedly protected against abuse of the WF license.  This is,
however, hearsay.  I heard a rumor that Dennis Ernst was the last to
touch the fateful program, thus, by rule of sticky fingers... -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 28 Jul 87 16:07:46 PDT 
To: hsing@calvin (Yu-liang Hsing) 
Subject: Re: kernel scheduler 
In-reply-to: <8707281825.AA09443@calvin.Tymnet.com> of Tue, 28 Jul 87 11:25:09
	PDT

1. I know of no non-obvious interaction between setjmp/longjmp and
dbx.  It probably gets confused if you "trace" through one since it
switches stacks out from under it.  

2. By the time k_sleep returns, its not a sleeping process.  If you
want it to be cancelable you can use k_alarm, but canceling an alarm
on a sleeping process would leave it asleep forever.  I'm not sure what
is intended here.

-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 28 Jul 87 16:10:25 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, Your example that you provided"... 
In-reply-to: <8707281921.AA10045@calvin.Tymnet.com> of Tue, 28 Jul 87 12:21:09
	PDT

The document which shows the strings inside the struct is bogus.
My appologies for that.  It won't pass lint and wont produce correct code.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 28 Jul 87 16:11:24 PDT 
To: dave@hobbes (Dave Smith) 
Subject: Re: Indian Hill style guide 
In-reply-to: <8707282003.AA06221@hobbes.Tymnet.com> of Tue, 28 Jul 87 13:03:55
	PDT

I think the hard part is finding time to meet to decide on changes, 
although I think we agree and I think the changes are already there.
Oh well.  What's your schedule like?  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 28 Jul 87 16:33:42 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA20441; Tue, 28 Jul 87 16:37:52 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07156; Tue, 28 Jul 87
	16:36:15 PDT
Return-path: <gauci@hobbes> 
From: gauci@hobbes (Ed Gauci) 
Date: Tue, 28 Jul 87 16:36:15 PDT 
To: wrs@hobbes 
Message-id: <8707282336.AA07156@hobbes.Tymnet.com> 
Subject: "Bill, Yet another question regarding"... 

Bill,

  Yet another question regarding IPCF message handles:

  When I receive an IPCF should i copy the handle, or the handle 
  pointer?  What should I pass to ipcfDone()?

  Is there an updated Rodney spec available?

Ed.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 28 Jul 87 16:35:24 PDT 
To: gauci@hobbes (Ed Gauci) 
Subject: Re: "Bill, Yet another question regarding"... 
In-reply-to: <8707282336.AA07156@hobbes.Tymnet.com> of Tue, 28 Jul 87 16:36:15
	PDT

The RSI gives you a pointer to the handle.  If you use it before returning
from the RSI, you don't need to copy it, however, if you want to use it
later (i.e. from background) then you best copy it.  It was determined that
structure copy is OK to use since all the compilers we have do it very well
and all new compilers are likely to since its probably in the ANSI standard.
So...
  struct IpcfHandle myHandle;
  myHandle = *handleArgument;
Sorry if you already knew this, just anticipating a question.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 28 Jul 87 16:38:57 PDT 
To: gauci@hobbes (Ed Gauci) 
Subject: Re: "Bill, Yet another question regarding"... 
In-reply-to: <8707282336.AA07156@hobbes.Tymnet.com> of Tue, 28 Jul 87 16:36:15
	PDT

There is none printed, but if you want to do
  cd ~wrs/red
  make wrs.p
you'll get a copy of ipcf, and kernel with a bunch of changes made and
a few more to come.  -Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Wed, 29 Jul 87 15:19:31 PDT 
To: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>, Ken Holcomb
	<NTD.K/HOLCOMB@Ontyme.Tymnet>, Mike Rude <NTD.M/Rude@Ontyme.Tymnet>,
	Betty Jo (BJ) Chang <NTD.B/CHANG@Ontyme.Tymnet>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme.Tymnet>, Dennis Ernst
	<NTD.D/ERNST@Ontyme.Tymnet>, John Kopf <KopfJ@B39.Tymnet>, Dan Lasater
	<DLasater@B39.Tymnet>, Jeff Oliveto <ENS.J/OLIVETO@Ontyme.Tymnet>,
	Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>, Romolo Raffo
	<NTD.R/RAFFO@Ontyme.Tymnet>, Dick Rawson <NTD.D/RAWSON@Ontyme.Tymnet>,
	Adele Newman <NTD.A/NEWMAN@Ontyme.Tymnet>, Dave Smith
	<NTD.D/SMITH@Ontyme.Tymnet>, DChang, Gazel Tan <GAZEL@B39>, Paul Chu
	<NTD.P/CHU@Ontyme.Tymnet>, Diana Tai <NTD.D/TAI@Ontyme.Tymnet>, Bill
	Soley <WRS@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>, Ashok Chippa
	<ACHIPPA@B39>, Henry Rivers <NTD.H/RIVERS@Ontyme.Tymnet>, Nancy Holt
	<NTD.N/BURRIESCI@Ontyme.Tymnet>, Dennis Jonas <JONAS@B39>, Scott Wedel
	<NTD.S/WEDEL@Ontyme.Tymnet>, Wayne Bartlett
	<TYM.W/BARTLETT@Ontyme.Tymnet>, Rosie Hull <NTD.R/HULL@Ontyme.Tymnet> 
Subject: LOGII GED Update 

Hello Folks,

The LOGII GED has been updated to as follows:

The CCN Flags have a new bit 4, indicating that the needle node list
contains a passthrough node that is not LOGII capable.

The NCN Overview and Compatibility Issues has been expanded to properly
address the host LOGII status reporting within the takeover response
and ISIS and command channel host status update messages.  Also the
creation of LOGI, LOGI translated from LOGII, and LOGII needles is described,
indicating the dependence of the needle form
on the LOGII status of the termination Node/Interface or passthrough
nodes.

The Document has been updated on SUPDEV:39 and OSERANG:39.

Thanks....Omar
Received: from X930.Tymnet by B39.Tymnet; Thu, 30 Jul 87 11:15:08 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Thu, 30 Jul 87 11:10:34 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, WRS 
Subject: System 70 users are being moved to F33. 

I got call from Barbara V. in Validations on the 27th.  She wanted to know if
I really wanted to keep all of the 1000 page files in (JMS:70) when we get
moved to system 33.  These happened to be test files, which I deleted.
I ran SZRPT with option NUMACCOUNT to get a list of GAN 3 users on F70,
then had RDIST compare corresponding directories on F70 and F33.

The results are in (JMS:70)CHECK3.LOG.  If you want to check your own
directory, use "RDIST (yourname)*.*/HOST:33/MASTER:70/CHECK".     /JMS
Received: from F29.Tymnet by B39.Tymnet; Thu, 30 Jul 87 16:19:56 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 30 Jul 87 16:04:44 PDT
Return-path: <NTD.G/CONE@EMSNTD.Ontyme.Tymnet> 
From: NTD.G/CONE@EMSNTD.Ontyme.Tymnet 
Date: 30 JUL 87 15:47:44 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I65760@Ontyme.Tymnet 
Subject: MOVE REMINDERS 
MOVETONEWFACILITY: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      30 JUL 87  15:47                  Network Technology Development

TO>        ALL NTD PERSONNEL

COPIES>    AL FENN

FROM>      GAIL CONE


SUBJECT>   MOVE TO NEW FACILITY


-----------------------------------------------------------------------


JUST A COUPLE OF REMINDERS REGARDING THE MOVE....

- PLEASE DO NOT GO TO THE NEW BUILDING UNTIL MONDAY MORNING

- DO NOT TAG (LABEL) OFFICE FURNITURE (EXCEPT TRASH CANS)

- PLEASE LEAVE PHONES AT YOUR OLD WORK LOCATIONS

- TAKE ALL VALUABLES AND PLANTS HOME BEFORE THE WEEKEND

- PLEASE TRY TO FINISH PACKING BEFORE THE PICNIC.  ALSO, NO ONE
  IS TO BE IN THE BUILDING OTHER THEN THE MOVERS AFTER 4:00PM


IF YOU HAVE ANY QUESTIONS, PLEASE CALL ME AT X.7441.


HAPPY MOVING!!!!!
GC
Received: from tymix.Tymnet by B39.Tymnet; Mon, 6 Jul 87 18:05:51 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05754; Mon, 6 Jul 87 18:09:52 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA21924;
	Mon, 6 Jul 87 18:09:30 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14332; Mon, 6 Jul 87 18:09:54
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 6 Jul 87 18:09:54 PDT 
To: wrs@calvin 
Message-id: <8707070109.AA14332@calvin.Tymnet.com> 
Subject: "where is your baby Mike J."... 

where is your baby ?????
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 6 Jul 87 18:12:09 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "where is your baby Mike J."... 
In-reply-to: <8707070109.AA14332@calvin.Tymnet.com> of Mon, 6 Jul 87 18:09:54
	PDT

Not here yet.  We are anxious, but patient.  She was 2cm on Friday, and
is getting irregular pain contractions at a mean interval of 90 minutes.
This could go on for days, I understand.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 7 Jul 87 12:12:24 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09252; Tue, 7 Jul 87 12:16:40 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23857; Tue,
	7 Jul 87 12:16:16 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00244; Tue, 7 Jul 87 12:16:43 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 7 Jul 87 12:16:43 PDT 
To: wrs@f104 
Message-id: <8707071916.AA00244@f104.sun.com> 
Subject: Simple IPCF send 

I mean Simple IPCF rcv, actually.  Do I still want to issue done when I
use this?
Received: from tymix.Tymnet by B39.Tymnet; Tue, 7 Jul 87 20:58:56 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11437; Tue, 7 Jul 87 21:03:08 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA25570; Tue,
	7 Jul 87 21:02:43 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00865; Tue, 7 Jul 87 21:03:11 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 7 Jul 87 21:03:11 PDT 
To: wrs@f104 
Message-id: <8707080403.AA00865@f104.sun.com> 
Subject: ipcf.h 

Hi Bill,
     the ipcf.h file in lib (which may be old, I don't know) has a couple
of errors-- the bit fields use misspelled define constants.  They end with
SIZE instead of SZ.  Also, struct VmeId is not declared.  I made changes
to this with a copy that I created in the directory rodney/v1/skylog.
I don't know how to officially store this anywhere... what is the command?
Also, the copy which is "magically" placed in your directory when you 
do sccs edit is read-only.  Does this need more work? or do you just 
chmod the file?
     I guess a few of us are going to be learning SCCS real soon...
Thanks, Ron.
By the way, I need some kind of answer on the TUMS question I sent (why
PROFS doesn't work) so I can tell Jerry Oberfranc at TMS.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 8 Jul 87 14:01:22 PDT 
To: rmurphy@f104 (Ron Murphy) 
Subject: Re: Simple IPCF send 
In-reply-to: <8707071916.AA00244@f104.sun.com> of Tue, 7 Jul 87 12:16:43 PDT

Hmmm, yes, somehow.  I guess there will have to be a simple IpcfDone() as
well.  I'll add that.  -Bill
Received: from X930.Tymnet by B39.Tymnet; Thu, 9 Jul 87 8:58:02 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Thu, 9 Jul 87 8:59:38 PDT 
To: wrs 
Subject: Could you add indirect files to RDIST? 

The following command did not do what I hoped it would do:
	RDIST @(M33)MON.CMD/HOST:32,33,34
It said there were 0 files on the master system.    /Joe
Received: from X930.Tymnet by B39.Tymnet; Thu, 9 Jul 87 9:00:51 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Thu, 9 Jul 87 9:03:17 PDT 
To: wrs 
Subject: Choice of editor on ~V command. 

What is the name of the variable I need to set in TUMS.INI to make is use a
different editor on the ~V command?  As long as it looks at nnnEDS.TMP, I
should be able to use any editor, right?       /Joe
Received: from tymix.Tymnet by B39.Tymnet; Fri, 10 Jul 87 17:06:22 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00200; Fri, 10 Jul 87 17:10:41 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10499;
	Fri, 10 Jul 87 17:10:06 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA03117; Fri, 10 Jul 87
	17:10:45 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 10 Jul 87 17:10:45 PDT 
To: wrs@calvin 
Message-id: <8707110010.AA03117@calvin.Tymnet.com> 
Subject: "Bill, BABY In the definitions"... 

Bill,
BABY ????......
In the definitions of structures like RodneyBootRsp or ..Req you left
undefined the lenghts of file names (like loadName[?]). Am I correct
in assuming that will have to be fixed in a 'define'? Will it be valid
for the whole interface or every module will have its own?
The reason I am asking is that if we agree on a common number you could
put it in Rodney and I won't have to redefine those structeres in my
boot just because we picked different values. This way I can refer to
the names of your structures if I want to. Am I clear to you?
My concern can be coming with some gaps in my knowledge of C.
Mike J.
Received: from X930.Tymnet by B39.Tymnet; Fri, 10 Jul 87 17:28:22 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Fri, 10 Jul 87 17:24:09 PDT 
To: WRS 
Subject: Changes to RDIST. 

Bug #1: Time-out on 930 for when LOGINN was waiting for project code.
	LOGINN demands a CR not just an LF at the end of the project code.
Fix #1: Send two CR's - one for project code, one for "attach to job?".

Bug #2: Gives up too soon when a host with 1B0 set appears in JOBS.DAT.
Fix #2: Stop at -1 instead of negative.  MHX also has this bug, CPY does not.

New:  I added the /MASTER switch so that I could run RDIST on the slave system.
New:  I added version number in DEC standard form.

File 1)	DSK:RDIST.WRS	created: 0152 28-JUN-87
File 2)	DSK:RDIST.SAI	created: 1643 10-JUL-87

1)1	require "(SAILIB)SAIL.DEF" source!file;
****
2)1	require '1 lsh 24 lor '002 version;	comment version 1(2);
2)	require "(SAILIB)SAIL.DEF" source!file;
**************
1)1	require "ARGS.REQ" source!file;
1)	require "AUXLIB.REQ" source!file;
1)	require "(MPL)UUOSYM.SAI" source!file;
****
2)1	require "(WRS)ARGS.REQ" source!file;
2)	require "(WRS)AUXLIB.REQ" source!file;
2)	require "(MPL)UUOSYM.SAI" source!file;
**************
1)	item LISTONLY;
1)	string item LOGFILE;
1)	string item NAMES;
1)	item CHECK;
1)	string item HOSTS;
1)	integer item RETRIES;
1)	integer item TIMEOUT;
1)	item TRACE;
1)	string item USER;
1)	item VERBOSE;
1)	set AllFiles;
1)	string MASTER;
****
2)	string item LOGFILE;
2)	string item NAMES;
2)	        item CHECK;
2)	 string item HOSTS;
2)	        item LISTONLY;
2)	 string item MASTER;
2)	integer item RETRIES;
2)	integer item TIMEOUT;
2)	        item TRACE;
2)	 string item USER;
2)	        item VERBOSE;
2)	set AllFiles;
**************
1)3		while 0 < i_wordin(chan) do
1)		    if i land '1000000000 then
****
2)3		while -1 neq i_wordin(chan) do		! Stop at -1, not 1B0;
2)		    if i land '1000000000 then
**************
1)3		    	HostTab[h_h+1] _ i land '777777;
****
2)3				! The master system should be excluded here;
2)		    	HostTab[h_h+1] _ i land '777777;
**************
1)5	    auxOut( "SET LOGOUT"& #lf& "TTY NO CRLF"& #lf );
1)	    return( true );
****
2)5	    ! Send one CR for "project code" and another for "attach to job?";
2)	    auxOut( #cr& #cr& "SET LOGOUT"& #cr& "TTY NO CRLF"& #cr );
2)	    return( true );
**************
1)6	    auxOut( "R(SPL)FINDIT"&#lf );
1)	    auxSync( #lf& "Find file: " );
****
2)6	    auxOut( "R(SPL)FINDIT"&#cr );
2)	    auxSync( #lf& "Find file: " );
**************
1)6	    auxOut( NAMES&#lf );
1)	    auxSync( #lf );
****
2)6	    auxOut( NAMES&#cr );
2)	    auxSync( #lf );
**************
1)7	    S := S[1 to 14]& "*"& S[19 to 30]& ","& S[1 to 30];
1)	    cprint( TEL.COM, scan( S, noBlank, !SKIP! ), crlf );
****
2)7	    S := S[1 to 14]& datum(MASTER)& S[19 to 30]& ","& S[1 to 30];
2)	    cprint( TEL.COM, scan( S, noBlank, !SKIP! ), crlf );
**************
1)8	    Login( USER, cvd(MASTER) );
1)	    Findit( NAMES, Build );
****
2)8	    Login( USER, cvd(datum(MASTER)) );
2)	    Findit( NAMES, Build );
**************
1)10	    MASTER := cvs(calli(!xwd(!CNSYS,!GTCNF),calli!GETTAB));
1)	    datum(RETRIES) := 5;
****
2)10	    datum(MASTER) := cvs(calli(!xwd(!CNSYS,!GTCNF),calli!GETTAB));
2)	    datum(RETRIES) := 5;
**************
1)10		{CHECK,HOSTS,LISTONLY,RETRIES,TIMEOUT,TRACE,USER,VERBOSE},
1)		ARGS );
****
2)10		{CHECK,HOSTS,LISTONLY,MASTER,RETRIES,TIMEOUT,TRACE,USER,VERBOSE},
2)		ARGS );
**************
1)10		print( "Master host: ", MASTER, crlf );
1)		BuildDataBase( datum(USER), datum(NAMES) );
****
2)10		print( "Master host: ", datum(MASTER), crlf );
2)		BuildDataBase( datum(USER), datum(NAMES) );
**************
Received: from X930.Tymnet by B39.Tymnet; Mon, 13 Jul 87 12:10:38 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Mon, 13 Jul 87 12:05:51 PDT 
To: WRS 
Subject: RDIST suggestions. 

I noticed that /USER applies to the aux circuit that runs FINDIT and FINDIT
itself, but not to TELECOPY.  That is, when logged in as JMS, TELECOPY can read
or create files in M33, but cannot supercede files in M33 because the aux
circuit built by TELECO is for JMS not M33.  That's the main reason why I added
the /MASTER switch, so that RDIST could be run on the destination system GFD'd
to the right directory as a SUBMIT job.  (My changes are in (JMS:39)RDIST.SAI).

Another thing to consider in the next version is a /GATEWAY command so that
B39 can be kept current with the rest of the world.  /Joe
Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 16:14:35 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02598; Sun, 12 Jul 87 22:58:04 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12779;
	Sun, 12 Jul 87 22:56:56 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA05498; Sun, 12 Jul 87
	22:57:42 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Sun, 12 Jul 87 22:57:42 PDT 
To: wrs@calvin 
Message-id: <8707130557.AA05498@calvin.Tymnet.com> 
Subject: "CONGRATULATIONS AND WELCOME TO"... 

CONGRATULATIONS AND WELCOME TO THE KLAN OF FATHERS !!!

...and now a humble question:

I am putting my GID document in standard form using ?roff and make.
I am using other source-documents as examples. However since I can't
find examples of usage of some commands that I need I would like to
verify with you that:
.nf
.fi
.b
.bp

are - not file, file, break (force next line), and force next page.
Am I correct?
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 20:34:05 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06334; Mon, 13 Jul 87 20:36:14 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18737; Mon,
	13 Jul 87 20:35:03 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00397; Mon, 13 Jul 87 20:35:50 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Mon, 13 Jul 87 20:35:50 PDT 
To: wrs@f104 
Message-id: <8707140335.AA00397@f104.sun.com> 
Subject: include 

What are we doing re rodney.h, glob.h, and ipcf.h in terms of SCCS?
I realize that these are now referenced in the RED Makefile.  I had to change
rodney.h and saw no SCCS for it, so I overwrote the file with a new read-only
copy...
Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 20:36:38 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06339; Mon, 13 Jul 87 20:39:06 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18740; Mon,
	13 Jul 87 20:37:56 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00402; Mon, 13 Jul 87 20:38:42 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Mon, 13 Jul 87 20:38:42 PDT 
To: wrs@f104 
Message-id: <8707140338.AA00402@f104.sun.com> 
Subject: ... 

Also, aren't we going to have a Rodney error for bus error? I added one to the
logger codes, which seems like the wrong place.  And are we going to have 
similar error handling to the VME "jump trap" for case of local memory access
error?  Pat mentioned this the other day.
Ron
Received: from tymix.Tymnet by B39.Tymnet; Wed, 15 Jul 87 14:15:05 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14211; Wed, 15 Jul 87 14:19:26 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00374; Wed,
	15 Jul 87 14:18:08 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00869; Wed, 15 Jul 87 14:18:59 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 15 Jul 87 14:18:59 PDT 
To: wrs@f104 
Message-id: <8707152118.AA00869@f104.sun.com> 
Subject: skylog red halves 

I have a way to combine the halves of the skylog document by using awk and
two passes of vgr.  This will not affect the other modules and can be used
to apply arbitrarily many fonts to a module in the future (at a slight 
expense in elegance and a very small cpu cost).  The .vS and .vE macros
are "hidden" in turn, currently as .v4, .v5, .v0, and .v1, when going 
through vfontedpr, and then all converted back for roff processing.  I 
intend to change the Makefile line for .mmx modules.  Let me know what
you think...
Ron
Received: from tymix.Tymnet by B39.Tymnet; Wed, 15 Jul 87 14:18:10 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14224; Wed, 15 Jul 87 14:22:36 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00399; Wed,
	15 Jul 87 14:21:18 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00873; Wed, 15 Jul 87 14:22:10 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 15 Jul 87 14:22:10 PDT 
To: wrs@f104 
Message-id: <8707152122.AA00873@f104.sun.com> 
Subject: cont'd... 

Also, there may be a problem between the .c and .vX macros used in the 
same paragraph (or else I made some other mistake).  This showed up 
processing the skylog1.mmx half in the regular way.  When xroffed, 
the first paragraph in 1.2.1 "Message Framing" puts the last "APPLICATION 0"
in normal font even though .vX was invoked.  Any ideas?
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 16 Jul 87 18:58:47 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, In my opinion there should"... 
In-reply-to: <8707162208.AA25045@calvin.Tymnet.com> of Thu, 16 Jul 87 15:08:07
	PDT

Only the actual recipient of the message has a valid handle so there
is only one chance in 2^32 of happening on a valid handle.  -Bill
Received: from F33.Tymnet by B39.Tymnet; Thu, 16 Jul 87 22:47:46 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Thu, 16 Jul 87 22:48:53 PDT 
To: WRS 
Cc: CARL 
Subject: PNAMES vs strange .TMP names 

  !RDIST (M33)*.*/HOST:930,34/CHECK
  Master host: 33
  ERROR - (M33         ) &.TMP     
   USED AS PNAME FOR TWO DIFFERENT ITEMS
  Called from 24227  Last SAIL call at 1604
  ^DNo DDT
  A^
  EXIT

CPY ignores *.TMP, RDIST should also.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 17 Jul 87 4:26:24 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "This is what I had in mind 2^32)."... 
In-reply-to: <8707170536.AA00205@calvin.Tymnet.com> of Thu, 16 Jul 87 22:36:38
	PDT

If we used the tsap as opposed to the handle we'd only get a protection
of 1 in 2^16 so the handle is much better.  -Bil
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 17 Jul 87 4:27:50 PDT 
To: Joe Smith <JMS@X930> 
Cc: CARL 
Subject: Re: PNAMES vs strange .TMP names 
In-reply-to: your message of Thu, 16 Jul 87 22:48:53 PDT

It just runs FINDIT so...
I suppose I could check for .TMP and try to handle it gracefully, but the
problem here is not .TMP, its binary file names.  The program RDIST blew
up here because it got two files with the same (reported) name.  I guess
that they each contained an embedded blank and differed following that so
the cvxstr routine in sail (which does the wrong thing) stops at the first
blank.  I'm not sure what the best fix would be.  Maybe FINDIT could output
otherwise illegal file names in some special way like '123456654321.XYZ.
Suggestions?  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 17 Jul 87 4:32:18 PDT 
To: Joe Smith <JMS@X930> 
Cc: CARL 
Subject: Re: PNAMES vs strange .TMP names 
In-reply-to: your message of Thu, 16 Jul 87 22:48:53 PDT

Actually, I would think FINDIT would want to ignore .TMP, wouldn't it?
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 17 Jul 87 16:25:18 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22687; Fri, 17 Jul 87 16:29:54 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16691;
	Fri, 17 Jul 87 16:28:28 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA05067; Fri, 17 Jul 87
	16:29:21 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 17 Jul 87 16:29:21 PDT 
To: wrs@calvin 
Message-id: <8707172329.AA05067@calvin.Tymnet.com> 
Subject: "In your manual you say that rkBootstrap()"... 

In your manual you say that rkBootstrap() normally does not return.
Well, we better talk about it because in my current design I ALWAYS
return to you.
Also I am not sure how to tell if start address exist in the file.
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 17 Jul 87 17:30:50 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "In your manual you say that rkBootstrap()"... 
In-reply-to: <8707172329.AA05067@calvin.Tymnet.com> of Fri, 17 Jul 87 16:29:21
	PDT

I'll have to ook at the notes from our meeting where we made the table on
the white-board to be sure, but I recall that rkBootStrap() should do
k_reset() and so will never return to the caller.  As for the start address,
you'll have to check with whoever specified the file format (Selter?).
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 20 Jul 87 11:37:30 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA29894; Mon, 20 Jul 87 11:41:48 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06985;
	Mon, 20 Jul 87 11:40:09 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA09682; Mon, 20 Jul 87
	11:41:06 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 20 Jul 87 11:41:06 PDT 
To: wrs@calvin 
Message-id: <8707201841.AA09682@calvin.Tymnet.com> 
Subject: "Bill, o.k. I concur that k_reset()"... 

Bill,
o.k. - I concur that k_reset() never returns to the caller, but it
will eventually come back through BootInit() which at some point
will return to Rodney (according to my understanding). Also 
rkBootStrap() will return to the caller - remember it will only
sprout BootMain() - k_reset is never called out of rkBootStrap().
These are important points - please agree or disagree.
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Mon, 20 Jul 87 18:17:22 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01554; Mon, 20 Jul 87 18:21:39 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10147;
	Mon, 20 Jul 87 18:20:02 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA12726; Mon, 20 Jul 87
	18:21:00 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 20 Jul 87 18:21:00 PDT 
To: wrs@calvin 
Message-id: <8707210121.AA12726@calvin.Tymnet.com> 
Subject: "Thanks for the response I am glad"... 

Thanks for the response - I am glad we clarified it. I do have the matrix
we composed on the board - but it is hand written and not very neat.
I guess Pat probably recorded it in a document.
When I asked Pat about the validity of the Start Address he referred me
to you (?), the matter is not of terrible importance to me right now,
but will be soon when I start coding.
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 22 Jul 87 10:46:35 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07979; Wed, 22 Jul 87 10:48:08 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18921; Wed,
	22 Jul 87 10:47:17 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00558; Wed, 22 Jul 87 10:48:26 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 22 Jul 87 10:48:26 PDT 
To: wrs@f104 
Cc: rmurphy@f104 
Message-id: <8707221748.AA00558@f104.sun.com> 
Subject: Logger loose ends 

Pat one day mentioned implementing error recovery for local memory access
similar to the code you invented for VME error recovery.  Are we going to
do this?  Also, did we decide on a simpleDone function for IPCF done while
interrupts are disabled?
Also, the following changes got lost from v1/include some time after I 
added them on July 13:
RERR_LOG_NOLOG		/* no log file is available */
RERR_LOG_NOROOM		/* no buffer space is available */
RERR_LOG_TOOMANY	/* message quota for time period exceeded */
RERR_LOG_BUSERROR	/* bus error during VME read */
RERR_LOG_TRUNCATED	/* log message too long, truncated */
Can we add these back?
Thanks,
Ron
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 22 Jul 87 13:32:49 PDT 
To: rmurphy@f104 (Ron Murphy) 
Subject: Re: Logger loose ends 
In-reply-to: <8707221748.AA00558@f104.sun.com> of Wed, 22 Jul 87 10:48:26 PDT

1. No local memory recovery that I know of.
2. SimpleDone - yes.
3. Don't change include/* its just a copy.
   a. what calls are going to return these errors?  I thought the
      logger would never return an error.
   b. if you still want them in, I'll add them to the master copy.
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 22 Jul 87 13:58:14 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00699; Wed, 22 Jul 87 14:00:23 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00887; Wed, 22 Jul 87 14:00:07 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 22 Jul 87 14:00:07 PDT 
To: WRS@B39.Tymnet 
Message-id: <8707222100.AA00887@f104.sun.com> 
Subject: Re: Logger loose ends 

Pat came back from the last RED review and said slgReport should return
error codes after all.  The new errors (except changing RERR_NOLOG to
RERR_LOG_NOLOG for consistency) all come from slgReport.  According to
Pat, the ISIS group wanted to know if the Logger was working because
there is an alternate place to log messages (Alfred???).  This is 
probably good since the Logger often has no choice but to lose its
buffer when the card gets in trouble.  Of course, ISIS's alternate
mechanism may be no better off; I haven't really questioned their
wisdom in this.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 22 Jul 87 14:01:27 PDT 
To: rmurphy@f104 (Ron Murphy) 
Subject: Re: Logger loose ends 
In-reply-to: <8707222100.AA00887@f104.sun.com> of Wed, 22 Jul 87 14:00:07 PDT

Good enough, I won't question it either.  Its certainly safer to return
an error code which can be ignored than to return void.  I'll add the
codes.  Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 24 Jul 87 17:03:24 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA10149; Fri, 24 Jul 87 17:05:38 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12359;
	Fri, 24 Jul 87 17:04:02 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA13880; Fri, 24 Jul 87
	17:05:06 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 24 Jul 87 17:05:06 PDT 
To: wrs@calvin 
Message-id: <8707250005.AA13880@calvin.Tymnet.com> 
Subject: "I am slowly getting ready to begin"... 

I am slowly getting ready to begin coding so you will hear from me
more often to clarify certain points/details.
I have come across something I don't fully understand.
In your doc. you say that handle.msgPtr->datalength will get me this
particular field. well, msgPtr points to struct IpcfMsg which is nowhere
defined (?), second what if I want to address a field outside the header
like 'functionCode'. I doubt handle.msgPtr->functionCode would work.
That is why it is a mystery for me why  handle.msgPtr->datalength would
work sine nowhere there is an information about datalength belonging to
IpcfHdr struct.
My basic problem is what is the trick that would allow me accessing
individual fields of structures like:

struct {
struct	IpcfHder A;
struct	RodneyBootReq B;
}

given handle.msgPter, without allocating memory for them.
I have a few ideas but they are clearly incompatible with your example
with 'datalen..' so they are probably wrong.
If you feel like calling instead of writting - it's fine.
thanks,
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 24 Jul 87 17:45:54 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "I am slowly getting ready to begin"... 
In-reply-to: <8707250005.AA13880@calvin.Tymnet.com> of Fri, 24 Jul 87 17:05:06
	PDT

I will write down an example, then you can call me if you want more
explanation.  Its okay all evening...

Some confusion is coming from renaming ipcfMsg to ipcfHdr.  It is possible
I did not find all occurances of ipcfMsg in the document (or you have an
older one).  Anyway, ipcfMsg no longer exists so always consider it to
mean ipcfHdr if you see it somewhere.

Now then, this is one of several hundred ways you might do it:

struct BootReqMsg {
	struct IpcfHdr h;
	struct RodneyBootReq b;
} *brm;

Then you can fill in ipcf header fields like...
	brm->h.srcTsap = whateverMyTsapIsCalled;
		etc...
And you can fill in body fields by...
	brm->b.functionCode = RODNEY_BOOT_REQ;
	brm->b.tsap = whateverMyTsapIsCalledAgain;
		etc...
Now the variable length fields are trickey since they are really not there
in the struct definition.  This calls for a bit of "C" wizardry:

/*
 * fillin()
 * Example routine to fill in the string fields of a BootRequestMsg.
 * Assumes brm points to a block of memory that is big enough.
 * Computes sizes for strings and the whole message.
 */
void
fillin( brm, dumpName, loadName )
struct BootRequestMsg *brm;		/* place to put message */
char dumpName[];			/* 1st string */
char loadName[];			/* 2nd string */
{
	char *s;
	s = (char*)(&brm->b+1);		/* compute address of 1st string */
	strcpy( s, dumpName );		/* copy 1st string to message */
	s += brm->b.dumpLen = strlen( dumpName ) + 1;
					/* compute length of 1st string and
					 * address of 2nd string */
	strcpy( s, loadName );		/* copy 2nd string to message */
	s += brm->b.loadName = strlen( loadName ) + 1;
					/* compute length of 2nd string and
					 * address of end of message */
	brm->h.length = s - (char*)(&brm->b);
					/* compute length of message body */
}

Please don't hesitate to call if you have any questions 866-0443.
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 28 Jul 87 12:19:17 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19173; Tue, 28 Jul 87 12:21:50 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06167;
	Tue, 28 Jul 87 12:20:15 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA10045; Tue, 28 Jul 87
	12:21:09 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 28 Jul 87 12:21:09 PDT 
To: wrs@calvin 
Message-id: <8707281921.AA10045@calvin.Tymnet.com> 
Subject: "Bill, Your example that you provided"... 

Bill,
Your example that you provided for my benefit with filling in the
string fields in RodneyBootReq suggests that the string lie outside
the definition of the RodneyBootReq structure (that is why you
can say  &brm->b+1  where +1 puts you outside the size of the
structure). well, the definition of the RodneyBootReq structure
has those strings within it so I am not sure how to reconcile
both. am I clear to you?
thanks,
Mike J.
Received: from X930.Tymnet by B39.Tymnet; Wed, 29 Jul 87 19:28:18 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Wed, 29 Jul 87 19:27:01 PDT 
To: Wayne Bartlett <BARTLETW@B39.Tymnet>, wrs, weuske@B39, Dennis Ernst
	<DErnst@B39> 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Removing license from NAD. 

(SYS)NAD.SAV has Write Files license.  This is a very serious security breach.
I ran a test and verified that this allows unprivileged users destroy accountingdata, the oper password file, and anything else they want to.  If no-one comes
up with a valid reason not to within 24 hours, I will remove WF from all copies
of NAD.

			Joe Smith
			Tymcom-X Support Group
Received: from X930.Tymnet by B39.Tymnet; Thu, 30 Jul 87 9:22:16 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 30 Jul 87 9:16:43 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Cc: Joe Smith <JMS@X930.Tymnet>, Craig Fletcher <Fletcherc@X930.Tymnet>, Dennis
	Ernst <Dernst@B39.Tymnet>, Osman Guven <Osman@X930.Tymnet> 
Subject: Ref: MSG from Joe Smith, re: NAD and WF 

Wayne,
  Since Joe (also in class) was working with NAD, I had him test the ability
of a non-privileged, non-licensed user to write a file to a directory that
he otherwise would not have access to.  He chose (UN1) for the destination
directory.
  I heard from Dennis Ernst, that HF or WF should probably NOT be set on NAD.
As such, we will be removing WF from (SYS)NAD sometime today...


  If this causes someone some inconvenience, because they were relying on
this "capability", please have them contact one of us (Joe or Carl).

Please forward a copy of this message to Doug E.  Thanks.

/Carl
Received: from F70.Tymnet by B39.Tymnet; Thu, 30 Jul 87 9:33:46 PDT
Return-path: <JMS@F70.Tymnet> 
From: JMS@F70.Tymnet 
Date: Thu, 30 Jul 87 9:37:27 PDT 
To: wrs 
Subject: "Files on F70 that differ from"... 

Files on F70 that differ from files on F33:
.RDIST (WRS)*.*/CHECK/HOST:33
Master host: 70
50 files on master system.
Slave hosts: 33
Checking host 33
(WRS         :  33) ASCII.SAV     12 22-Oct-77 09:05 -- bad checksum
(WRS         :  33) TUMS.INI       3 21-Jun-86 20:27 -- bad checksum
(WRS         :  33) INIT.SAV      10 10-May-85 15:15 -- bad checksum
(WRS         :  33) I00395.BAD   150 22-Apr-82 09:36 -- bad checksum
(WRS         :  33) CIPHER.SAV    11 14-Feb-82 02:36 -- bad checksum
(WRS         :  33) HOG.SAV       12 21-Nov-78 13:34 -- bad checksum
(WRS         :  33) LUDLOC.SAV     2 10-Feb-79 23:18 -- bad checksum
(WRS         :  33) RTYFIX.SAV     2 22-Nov-83 12:48 -- bad checksum
(WRS         :  33) FREE.LOG       4 23-Jun-87 13:46 -- bad checksum
(WRS         :  33) CRASUM.SAV    14 22-Apr-81 19:47 -- bad checksum
(WRS         :  33) L.SAV          3 14-May-79 20:00 -- bad checksum

End of SAIL execution
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Thu, 30 Jul 87 10:07:21 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: William R Soley <WRS@B39.Tymnet>, Joe Smith <JMS@X930.Tymnet>, Craig
	Fletcher <Fletcherc@X930.Tymnet>, Dennis Ernst <Dernst@B39.Tymnet>,
	Osman Guven <Osman@X930.Tymnet> 
Subject: Re: Ref: MSG from Joe Smith, re: NAD and WF 
In-reply-to: your message of Thu, 30 Jul 87 9:16:43 PDT

SOUNDS GOOD!!  HAVE NOTIFIED (VIA ONTYME) BOTH DON MURTAUGH AND DOUG -
SO PLEASE PROCEED AS PLANNED - 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 30 Jul 87 10:37:49 PDT 
To: Joe Smith <JMS@X930> 
Cc: Wayne Bartlett <BARTLETW@B39.Tymnet>, weuske@B39, Dennis Ernst
	<DErnst@B39>, Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: Re: Removing license from NAD. 
In-reply-to: your message of Wed, 29 Jul 87 19:27:01 PDT

Thank you for checking with us.  It is bound to screw up
somebody, but nobody at NTD.  I think (SYS)NAD is used
mostly by Tymnet operations and private network customers.
It seems to me they need an alternative means to access
their code, but I don't understand what the procedures
are so I can't suggest such an alternative.  *SIGH*  I
think this is gross negligence on someone's part, but it
is unfortunate that our customers will be the ones to
suffer for it.  I knew about the WF on NAD, but was told
that it enforced its own security, which it at least used
to because I frequently have gotten protection failures
trying to read source code I'm assembling (being in a
different GAN than most of NTD) when I forget to set RF.

I propose more investigation before any action, even
as gross as this appearently is, it can probably be 
left alone a couple more days.  I would normally
volunteer to help investigate, but this is a very bad
time.

Thanks
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 30 Jul 87 10:51:54 PDT 
To: JMS@F70.Tymnet 
Subject: Re: "Files on F70 that differ from"... 
In-reply-to: your message of Thu, 30 Jul 87 9:37:27 PDT

That is interesting.  They should all be the same.  But I'm very
curious what prompted you to check.  Is there something I should
know about 70 or 33?  (is my head in the sand, again?)  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 30 Jul 87 10:58:22 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: Carl A Baltrunas <Carl@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet>, Craig
	Fletcher <Fletcherc@X930.Tymnet>, Dennis Ernst <Dernst@B39.Tymnet>,
	Osman Guven <Osman@X930.Tymnet> 
Subject: Re: Ref: MSG from Joe Smith, re: NAD and WF 
In-reply-to: your message of Thu, 30 Jul 87 10:07:21 PDT

It sounds like you have done more investigation.  I did a tiny bit, too, and
I see (SOURCE)NAD with HF on 70.  Presumably that is the only way that
a customer can access the files in (SOURCE) since they are mostly ALL RD NO.
However, if NAD does not enforce security itself, this is still a problem
because we do not want customers to be able to read/write even the source
directory except to assemble (but not make listing of) the files therein.
Although this is a smaller box than (SYS)NAD with WF, it still needs to be
addressed one way or another.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 9:04:35 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14016; Fri, 31 Jul 87 00:22:42 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05126;
	Fri, 31 Jul 87 00:23:24 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA03378; Fri, 31 Jul 87
	00:21:34 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 31 Jul 87 00:21:34 PDT 
To: wrs@calvin 
Message-id: <8707310721.AA03378@calvin.Tymnet.com> 
Subject: "Bill, I definitely need additional"... 

Bill,
I definitely need additional Rodney error RERR_??? in case rkBootStrap()
can't sprout BootMain and must return a failure code.
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 9:05:07 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12968; Thu, 30 Jul 87 16:23:37 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA03983;
	Thu, 30 Jul 87 16:23:53 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA02479; Thu, 30 Jul 87
	16:22:04 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 30 Jul 87 16:22:04 PDT 
To: wrs@calvin 
Message-id: <8707302322.AA02479@calvin.Tymnet.com> 
Subject: "Bill, what is the exact format"... 

Bill,
what is the exact format of k_sleep function? Got no document
on this subject.
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 9:06:07 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12813; Thu, 30 Jul 87 15:59:58 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA03797;
	Thu, 30 Jul 87 15:59:25 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA02329; Thu, 30 Jul 87
	15:57:36 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 30 Jul 87 15:57:36 PDT 
To: wrs@calvin 
Message-id: <8707302257.AA02329@calvin.Tymnet.com> 
Subject: "If there is no crashName[], should"... 

If there is no crashName[], should I put a NULL in its place in structure
and set crashLen = 1,
or no NULL and crashLen = NULL.
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 9:06:41 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12659; Thu, 30 Jul 87 15:15:39 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA03513;
	Thu, 30 Jul 87 15:16:04 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA02134; Thu, 30 Jul 87
	15:14:15 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 30 Jul 87 15:14:15 PDT 
To: wrs@calvin 
Message-id: <8707302214.AA02134@calvin.Tymnet.com> 
Subject: "Bill, what rkGetBoot() should"... 

Bill,
what rkGetBoot() should do if there is no valid set strategy?
it is unclear to me what code should be returned.

also a C question:

If A and B are strucures of the same kind A = B would copy B into A.
Now ltes say Ap is a pointer to structure A. Can copying be done
now with *Ap = B   ...or *Ap = *Bp  (Bp points to B) ???

Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 9:07:13 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12494; Thu, 30 Jul 87 14:11:51 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA03082; Thu, 30 Jul 87
	14:12:33 PDT
Return-path: <dave@hobbes> 
From: dave@hobbes (Dave Smith) 
Date: Thu, 30 Jul 87 14:12:33 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707302112.AA03082@hobbes.Tymnet.com> 
Subject: Re: Indian Hill style guide 

I'm easy.  We can meet any time.  Or if you prefer, I can make
a first cut at the changes and let you modify them.  -Dave
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 17:52:40 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA16750; Fri, 31 Jul 87 17:56:57 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05966;
	Fri, 31 Jul 87 17:57:27 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04665; Fri, 31 Jul 87
	17:55:36 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 31 Jul 87 17:55:36 PDT 
To: wrs@calvin 
Message-id: <8708010055.AA04665@calvin.Tymnet.com> 
Subject: "Bill, Your definition of RkGetBoot()"... 

Bill,
Your definition of RkGetBoot() defines two arguments:
char *dumpFile[];  (and simlilar for load)

in my opinion it should be without * since the name of the string
is also a pointer to this string and you clearly passing to me pointers
to where you want me put the names.
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 31 Jul 87 18:44:31 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, what is the exact format"... 
In-reply-to: <8707302322.AA02479@calvin.Tymnet.com> of Thu, 30 Jul 87 16:22:04
	PDT

void k_sleep( time )
u_long time;	/* time to sleep in uS */

Re: error codes, let me know what you need and I'll put it in the .h file.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 31 Jul 87 18:48:10 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "If there is no crashName[], should"... 
In-reply-to: <8707302257.AA02329@calvin.Tymnet.com> of Thu, 30 Jul 87 15:57:36
	PDT

----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 9:06:07 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12813; Thu, 30 Jul 87 15:59:58 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA03797;
	Thu, 30 Jul 87 15:59:25 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA02329; Thu, 30 Jul 87
	15:57:36 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 30 Jul 87 15:57:36 PDT 
To: wrs@calvin 
Message-id: <8707302257.AA02329@calvin.Tymnet.com> 
Subject: "If there is no crashName[], should"... 

If there is no crashName[], should I put a NULL in its place in structure
and set crashLen = 1,
or no NULL and crashLen = NULL.
Mike J.
----------------------------------------------------------------

NULL and crashLen = 1
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 31 Jul 87 18:49:18 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, what rkGetBoot() should"... 
In-reply-to: <8707302214.AA02134@calvin.Tymnet.com> of Thu, 30 Jul 87 15:14:15
	PDT

>  what rkGetBoot() should do if there is no valid set strategy?
>  it is unclear to me what code should be returned.

Return the default strategy.

>  If A and B are strucures of the same kind A = B would copy B into A.
>  Now ltes say Ap is a pointer to structure A. Can copying be done
>  now with *Ap = B   ...or *Ap = *Bp  (Bp points to B) ???

Yes, those all work.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 31 Jul 87 18:56:10 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, Your definition of RkGetBoot()"... 
In-reply-to: <8708010055.AA04665@calvin.Tymnet.com> of Fri, 31 Jul 87 17:55:36
	PDT

You are right.  I've modified the document to fix this error.  I think
it was an oversight.  Thanks -Bill
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 17:52:40 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA16750; Fri, 31 Jul 87 17:56:57 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05966;
	Fri, 31 Jul 87 17:57:27 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04665; Fri, 31 Jul 87
	17:55:36 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 31 Jul 87 17:55:36 PDT 
To: wrs@calvin 
Message-id: <8708010055.AA04665@calvin.Tymnet.com> 
Subject: "Bill, Your definition of RkGetBoot()"... 

Bill,
Your definition of RkGetBoot() defines two arguments:
char *dumpFile[];  (and simlilar for load)

in my opinion it should be without * since the name of the string
is also a pointer to this string and you clearly passing to me pointers
to where you want me put the names.
Mike J.
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Wed, 1 Jul 87 23:14:49 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09390; Wed, 1 Jul 87 18:27:36 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09153;
	Wed, 1 Jul 87 18:27:27 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA06928; Wed, 1 Jul 87 18:27:32
	PDT
Return-path: <davarj@calvin> 
From: davarj@calvin (Jonathan Davar) 
Date: Wed, 1 Jul 87 18:27:32 PDT 
To: wrs@calvin 
Message-id: <8707020127.AA06928@calvin.Tymnet.com> 
Subject: k_terminate 

What happens if a process calls k_terminate, but never returns to the
scheduler. Wait, scratch that! What happens when a process calls k_terminate
with its own pid?

Jonathan

Received: from F29.Tymnet by B39.Tymnet; Tue, 7 Jul 87 8:16:02 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 7 Jul 87 8:14:45 PDT
Return-path: <IPC.C/BLOCK@EMSNTD.Ontyme.Tymnet> 
From: IPC.C/BLOCK@EMSNTD.Ontyme.Tymnet 
Date: 07 JUL 87 07:46:01 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: A77999@Ontyme.Tymnet 
Subject: Loop In Tumsgate service machine 

Bill TUMSGATE is having a problem with a DISCONNECT/RECONNECT loop.
Friday reader files were queuing in TUMSGATE reader.  This weekend
operations called to say the service machine was reconnecting/disconnecting
on a regular basis.  Can you look into it?  It is still happening today.
Received: from F29.Tymnet by B39.Tymnet; Tue, 7 Jul 87 11:26:51 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 7 Jul 87 11:28:46 PDT
Return-path: <NTD.S/KING@EMSNTD.Ontyme.Tymnet> 
From: NTD.S/KING@EMSNTD.Ontyme.Tymnet 
Date: 07 JUL 87 11:09:10 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I55370@Ontyme.Tymnet 
Subject: RESCHEDULING OF SKYWALKER H/W PLANNING MEETING 
RESCHEDULINGOFSKYWALKERH/WPLANNINGMEETING: 

 
 
                            M E M O R A N D U M
 
                                             [] T Y M N E T, I N C
                                             Network Technology Development
 
DATE>    July 7, 1987
 
TO>      Bill Soley
 
COPIES>  Dave Bliss, Ron Holst, Jim Willott, Bob Chen, Paul Brown, Mike Mo
 
FROM>    Stan King
 
SUBJECT> RESCHEDULING OF SKYWALKER H/W PLANNING MEETING
 
---------------------------------------------------------------------------
The Skywalker Hardware Planning meeting originally scheduled for Thursday
July 9 has been cancelled.  The meeting will be rescheduled for Thursday
July 23 in the Building H large conference room from 9:00 a.m. until noon.
If there are any problems with this time let me know.  An alternate time is
Wednesday July 22 from 10:00 a.m. until 1:00 p.m.
 
Stan
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 8 Jul 87 14:00:05 PDT 
To: NTD.S/KING@EMSNTD.Ontyme.Tymnet 
Subject: Re: RESCHEDULING OF SKYWALKER H/W PLANNING MEETING 
In-reply-to: I55370@Ontyme.Tymnet of 07 JUL 87 11:09:10

Both are okay with me.  I'll put the 23'rd in my calender unless I hear
otherwise.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 4:05:48 PDT 
To: davarj@calvin (Jonathan Davar) 
Subject: Re: K_terminate 
In-reply-to: <8707072120.AA16867@calvin.Tymnet.com> of Tue, 7 Jul 87 14:20:31
	PDT

I would hope it would know which was itself, but if not, I guess it can
call k_pid() and find out.  Its not in the document yet, but it just
returns the pid of the current process and takes no args.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 31 Jul 87 18:51:43 PDT 
To: dave@hobbes (Dave Smith) 
Subject: Re: Indian Hill style guide 
In-reply-to: <8707302112.AA03082@hobbes.Tymnet.com> of Thu, 30 Jul 87 14:12:33
	PDT

If you have the time, that would be wonderful.  I'm slipping behind
due to various distractions and lack of sleep.  I think, however, that
all the points we were going to add (as memory serves) are covered in
section 8.2 on page 7 "Naming Conventions".  Was there anything else?
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 31 Jul 87 20:03:39 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA17053; Fri, 31 Jul 87 20:07:57 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06084;
	Fri, 31 Jul 87 20:08:35 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04858; Fri, 31 Jul 87
	20:06:45 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 31 Jul 87 20:06:45 PDT 
To: wrs@calvin 
Message-id: <8708010306.AA04858@calvin.Tymnet.com> 
Subject: "okay Bill...more q. 1. boot ack"... 

okay Bill...more q.

1. boot ack sends the _RSP message. the contents of this message should
relate to the current boot process or to the set strategy?

2. what are the RERR_??? codes pertaining to ipcfAssign() that tell
me (boot) in fact "no room for you right now". I was betting on
_NO_SSAPS <= code <= _NO_TSAPS but they sound like SKYSAP related
and I am not sure if I ever going to see them. this is important for me
so I can distinguish between hopless errors and temporary bottleneck.

deep reagards,
mike j.
Received: from tymix.Tymnet by B39.Tymnet; Sat, 1 Aug 87 1:26:31 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA17692; Sat, 1 Aug 87 01:30:45 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06231;
	Sat, 1 Aug 87 01:31:19 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA05171; Sat, 1 Aug 87 01:29:29
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Sat, 1 Aug 87 01:29:29 PDT 
To: wrs@calvin 
Message-id: <8708010829.AA05171@calvin.Tymnet.com> 
Subject: "Bill, along the same lines as"... 

Bill, along the same lines as last question - how serious is
inability to sprout a process. is it likely to go away after a bit
of pause?
...in case your answer is "it is fatal - crash" do you want me to crash
or would you rather prefer to get some sort of returned error code
(another RodneyError) so you could do with it whatever you want.
this would apply to places like rkBootstrap(), BootInit().
Mike J.
Received: from F29.Tymnet by B39.Tymnet; Tue, 4 Aug 87 15:17:50 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 4 Aug 87 15:10:24 PDT
Return-path: <NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 04 AUG 87 12:22:51 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I67420@Ontyme.Tymnet 
Subject: PHONE CHANGE 
PHONECHANGE: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      04 AUG 87  12:22                  Network Technology Development

TO>        ALL NTD

COPIES>    

FROM>      KEN HOLCOMB


SUBJECT>   PHONE CHANGE


-----------------------------------------------------------------------


FOR THOSE OF YOU WHO HAVE BEEN TRYING TO REACH ME BY PHONE
WITHOUT SUCCESS, MY NEW EXTENSION IS 6614.
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 4 Aug 87 15:37:06 PDT 
To: WRS@C39.Tymnet 
Subject: "ARE YOU MOVED??? WHAT IS THE NEW"... 

ARE YOU MOVED??? WHAT IS THE NEW PHONE NO?????
TODAY, MUCH PROBLEM WITH S39 AND TUMS??HARDWARE?? DROPPED SEVERAL TIMES
RIGHT WITH LOTS OF MSG'S TO BE EXPUNGED, PLUS SAME MSG'S APPEARING
4-5 TIMES OVER!!! OUCH!!!!!!!!(HAD ABOUT 30 REDUNDANT, AND TOOK 3-4 TRIES
TO GET RID OF THEM)
Received: from D37.Tymnet by B39.Tymnet; Wed, 5 Aug 87 9:54:09 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Wed, 5 Aug 87 16:54:14 UT 
To: WRS 
Subject: Req #11928 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #11928  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  05-Aug-87 09:29:00
; Actual run: 05-Aug-87 08:30:37
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 60  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Wed, 5 Aug 87 10:54:44 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Wed, 5 Aug 87 17:54:49 UT 
To: WRS 
Subject: Req #11928 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #11928  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  05-Aug-87 09:29:00
; Actual run: 05-Aug-87 09:54:14
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 60  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 5 Aug 87 11:01:08 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: "ARE YOU MOVED??? WHAT IS THE NEW"... 
In-reply-to: your message of Tue, 4 Aug 87 15:37:06 PDT

Still 6210, except 922-6210.  
Have to drop by and see me sometime, I'm on the 4th floor.  Out of the
elevator and turn right, past the hall and right at the isle.  2nd on the
right.  B4-535.  Re: TUMS, sorry about that.  Let me know if you can make
it happen at will.  Meanwhile, you're the only one complaining so I think
it may be something you're doing different from others.  I really have
no clues.  Oh well.  -Bill
Received: from D37.Tymnet by B39.Tymnet; Wed, 5 Aug 87 11:55:36 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Wed, 5 Aug 87 18:55:44 UT 
To: WRS 
Subject: Req #11928 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #11928  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  05-Aug-87 09:29:00
; Actual run: 05-Aug-87 10:54:50
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 60  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Wed, 5 Aug 87 14:10:35 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Cc: CARL@X930 
Subject: Re: "ARE YOU MOVED??? WHAT IS THE NEW"... 
In-reply-to: your message of Wed, 5 Aug 87 11:01:08 PDT

THANKS - WILL VIST - BUT AFTER LEG HEALS - INFACT, WOULD LIKE TO 
REVIEW SETOP WITH YOU AND CARL - HOW IS THURS OR FRI, 8/13 - 8/14??
Received: from F29.Tymnet by B39.Tymnet; Thu, 6 Aug 87 5:49:35 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Thu, 6 Aug 87 5:49:52 PDT
Return-path: <DAUL@OFFICE-1.ARPA> 
From: William Daul / McDonnell-Douglas / APD-ASD <WBD.MDC@OFFICE-1.ARPA> 
Date: 5 Aug 87 00:41 PDT 
To: William R. Soley <WRS%B39.Tymnet@OFFICE-1.ARPA> 
Message-id: <MDC-WBD-BY0PN@OFFICE-1> 
Subject: Re: William Christian Soley 
In-reply-to: EXT-WRS-BS5LS

Bill,

How is the little guy doing?  Is he playing with your PC yet?  --Bi//

From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 6 Aug 87 12:54:47 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: CARL@X930 
Subject: Re: Reviewing SETOP with Carl 
In-reply-to: your message of Wed, 5 Aug 87 14:10:35 PDT

Yes, either Thursday or Friday of next week are okay.  Lets meet at my office
about 1:30 instead of lunch.  I'd invite you all for lunch, but they're not
going to open the cafeteria until September.  Oh well.  Let me know which
day anyone prefers.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 6 Aug 87 12:59:25 PDT 
To: William Daul / McDonnell-Douglas / APD-ASD <WBD.MDC@OFFICE-1.ARPA> 
Subject: Re: William Christian Soley 
In-reply-to: <MDC-WBD-BY0PN@OFFICE-1> of 5 Aug 87 00:41 PDT

No, but I caught him reading Kernigham and Ritchie the other day and took
a picture so I expect him to start eyeing the keyboard soon.  -Bill

(actually he's doing very well, better than I am)
He's starting to smile.  
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 19:17:16 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00819; Thu, 6 Aug 87 19:16:36 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA14460;
	Thu, 6 Aug 87 15:35:21 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA12061; Thu, 6 Aug 87 15:33:23
	PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Thu, 6 Aug 87 15:33:23 PDT 
To: wrs@calvin 
Message-id: <8708062233.AA12061@calvin.Tymnet.com> 
Subject: test message 

This is a test message.  Please acknowledge.
Pat
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 19:32:12 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00824; Thu, 6 Aug 87 19:16:41 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13770;
	Thu, 6 Aug 87 14:40:35 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11237; Thu, 6 Aug 87 14:38:38
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 6 Aug 87 14:38:38 PDT 
To: wrs@calvin 
Message-id: <8708062138.AA11237@calvin.Tymnet.com> 
Subject: "hghhgh test"... 

hghhgh test
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 19:32:39 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00829; Thu, 6 Aug 87 19:16:45 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13805;
	Thu, 6 Aug 87 14:43:36 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11312; Thu, 6 Aug 87 14:41:39
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 6 Aug 87 14:41:39 PDT 
To: wrs@calvin 
Message-id: <8708062141.AA11312@calvin.Tymnet.com> 
Subject: jhj 

jhjh
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 19:33:41 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00861; Thu, 6 Aug 87 19:17:22 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04146;
	Tue, 4 Aug 87 11:08:04 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04098; Tue, 4 Aug 87 11:06:09
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 4 Aug 87 11:06:09 PDT 
To: wrs@calvin 
Message-id: <8708041806.AA04098@calvin.Tymnet.com> 
Subject: "Bill, I am confused a little."... 

Bill, I am confused a little. Not only you but other people are not
responding to my mail at all. I am suspecting some spooky is going on
with calvin. Could you just ack this mail ?
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 19:34:12 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00845; Thu, 6 Aug 87 19:17:04 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA11102;
	Wed, 5 Aug 87 20:24:00 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA10269; Wed, 5 Aug 87 20:21:58
	PDT
Return-path: <coons@opus> 
From: coons@opus (John Coons) 
Date: Wed, 5 Aug 87 20:21:58 PDT 
To: julies@opus, wrs@opus 
Message-id: <8708060321.AA10269@opus.Tymnet.com> 
Subject: TUMS 


It appears that TUMS has read my Ontymes since Monday, 8/3 and cleared
them from the Ontyme read list but not delivered them to opus as
it used to do. 

If this is not a known problem, you are now aware !

Please let me know if when and if this is remedied ...

Thanks,

John Coons
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 20:01:44 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00835; Thu, 6 Aug 87 19:16:52 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13810;
	Thu, 6 Aug 87 14:43:53 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11325; Thu, 6 Aug 87 14:41:56
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 6 Aug 87 14:41:56 PDT 
To: wrs@calvin 
Message-id: <8708062141.AA11325@calvin.Tymnet.com> 
Subject: test 

test
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 20:02:12 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00840; Thu, 6 Aug 87 19:16:58 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13417;
	Thu, 6 Aug 87 14:15:22 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA10307; Thu, 6 Aug 87 14:13:25
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 6 Aug 87 14:13:25 PDT 
To: wrs@calvin 
Message-id: <8708062113.AA10307@calvin.Tymnet.com> 
Subject: "Bill, it is quite unbelievable"... 

Bill,
it is quite unbelievable but a week(!) later I got all the messages that
I sent to you and you never got- back with 'unknown user'. I verified I did
not make an error in the username. Anyway could you please ack the
reception of this message to verify that everything is fine now?
thanks,
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 7 Aug 87 5:32:31 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04664; Fri, 7 Aug 87 05:32:12 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14476; Fri, 7 Aug 87 05:32:09
	PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Fri, 7 Aug 87 05:32:09 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8708071232.AA14476@calvin.Tymnet.com> 
Subject: Re: test message 

I got my old Ontymes, thanks.

Pat
Received: from F29.Tymnet by B39.Tymnet; Fri, 7 Aug 87 6:47:55 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Fri, 7 Aug 87 6:47:41 PDT
Return-path: <STONE@OFFICE-1.ARPA> 
From: Duane Stone / McDonnell Douglas <DLS.MDC@OFFICE-1.ARPA> 
Date: 7 Aug 87 09:35 EDT 
To: WBD.MDC@OFFICE-1.ARPA 
Cc: ASD.MDC@OFFICE-1.ARPA, ALG.MDC@OFFICE-1.ARPA, AWB.MDC@OFFICE-1.ARPA,
	M166087@SLVM307.McAuto.Tymnet, M189125@SLVM307.McAuto.Tymnet,
	ipc.n/marsh%Ontyme.Tymnet@OFFICE-1.ARPA, WRS.MDC@OFFICE-1.ARPA,
	RBC.COR@OFFICE-1.ARPA, KB003568@CSWDCMS1.McAuto.Tymnet,
	CSUK.D/Vachell%Ontyme.Tymnet@OFFICE-1.ARPA,
	hrd.trng%Ontyme.Tymnet@OFFICE-1.ARPA,
	ntd.k/jones%Ontyme.Tymnet@OFFICE-1.ARPA 
Message-id: <MDC-DLS-BY5PB@OFFICE-1> 
Subject: Re: Unbelievable MDC Pro-Space Publicity Story!!! 
In-reply-to: MDC-WBD-BY52E

The ad runs on channel 4 (NBC) in the Washington DC area, sometimes in the 
evening between local and national news and often during 11:00 news -- quite 
impressive.  Perhaps the MDC promoters of the Space Station feel they'll get 
more 'bang for the lobbying buck' in this area.  This same ad was used to open 
a space station conference last week in Tyson Corners VA (Vienna/Mclean) -- 
where several hundred 'beltway bandits' gathered to promote the space-station. 

My town, Reston VA, is seeing a lot of space-station related activity.  NASA 
space-station project management is moving into a new building out here (20 
miles west of Wash DC).  A large contract (billion dollars) was recently 
awarded to Grumman to support this NASA function, so they're locating here also
to be near NASA.  A number of smaller companies hoping for mere million dollar 
crumbs are starting to gather around the 'big boys'.

It's interesting to watch the whole process.  The local paper recently devoted 
a issue to the subject -- SPACE, Reston's Next Frontier.  with quotes like 'The
space station could make Reston a coroprate household name, much like Houston's
space flight center was' & 'You're going to see an influx of companies moving 
to the area to start up or set up new divisions'  Based on the combined 1100 
NASA & Grumman workforce, estimates are that 10,000 new space-related jobs will
move into the area in the next 3-5 years.

So.. there is a lot of activity re the space station hppening around here.  
Some of it's joint space-industrial complex hype, but once BIG contracts start 
coming, things start to move -- albeit several years late and at a very slow 
pace, it looks like there's enough momentum now to make the space station real.
 I have no idea where MDC fits into all this.  Except for the ad, I don't see 
much MDC space-related activity around Wash DC.

From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 7 Aug 87 8:50:53 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Cc: CARL@X930 
Subject: Re: Reviewing SETOP with Carl 
In-reply-to: your message of Thu, 6 Aug 87 12:54:47 PDT

PREFER THURS, 8/13, BUT FRI. OK
Received: from tymix.Tymnet by B39.Tymnet; Fri, 7 Aug 87 9:20:01 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05916; Fri, 7 Aug 87 09:19:33 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16505;
	Fri, 7 Aug 87 09:19:30 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA15644; Fri, 7 Aug 87 09:19:28
	PDT
Return-path: <coons@opus> 
From: coons@opus (John Coons) 
Date: Fri, 7 Aug 87 09:19:28 PDT 
To: wrs@opus 
Message-id: <8708071619.AA15644@opus.Tymnet.com> 
Subject: TUMS 


FYI, I still have received no Ontymes on opus (even those that should
have been queued).

Regards,

John
Received: from X930.Tymnet by B39.Tymnet; Fri, 7 Aug 87 10:28:35 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 7 Aug 87 10:26:25 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Cc: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: Reviewing SETOP with Carl 
In-reply-to: your message of Thu, 6 Aug 87 12:54:47 PDT

Thursday OK, but prefer Friday...
  let us know which you prefer.
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 7 Aug 87 10:31:37 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: Reviewing SETOP with Carl 
In-reply-to: your message of Fri, 7 Aug 87 10:26:25 PDT

CAN WE SETTLE ON THURS, THEN(MY PREFERENCE - WILL THEN BE ABLE TO
ATTEND KENNA CLUB LUNCH FRI) PLEASE CONFIRM THURS.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 7 Aug 87 10:40:54 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06593; Fri, 7 Aug 87 10:40:39 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00560; Fri, 7 Aug 87 10:40:36 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Fri, 7 Aug 87 10:40:36 PDT 
To: WRS@B39.Tymnet 
Message-id: <8708071740.AA00560@f104.sun.com> 
Subject: Re: license in files 

I changed my oper password...

Much chagrin,
Ron

Received: from F29.Tymnet by B39.Tymnet; Fri, 7 Aug 87 13:04:12 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 7 Aug 87 12:58:11 PDT
Return-path: <NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 07 AUG 87 11:59:28 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I69113@Ontyme.Tymnet 
Subject: TECHNICAL LIBRARY 
NTDTECHNICALLIBRARY: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      07 AUG 87  11:59                  Network Technology Development

TO>        NTD PERSONNEL

COPIES>    

FROM>      KEN HOLCOMB


SUBJECT>   NTD TECHNICAL LIBRARY


-----------------------------------------------------------------------


There is a temporary mini-library on the fifth floor, building B.
Available are Skywalker material, Sun Unix documentation, Unix
and C books, and about 20 periodicals.  (There is also a cubicle
"reading room" - #325).  Dorothy's office is #355, and Mary's is
#345.  Send your requests via interoffice mail to Technical Library,
B-5, B-345, or by Ontyme (ntd.d/sands or ntd.m/proffitt).  Temporary
telephone number 922-6150.
Received: from F29.Tymnet by B39.Tymnet; Fri, 7 Aug 87 15:44:16 PDT
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Fri, 7 Aug 87 15:38:25 PDT
Return-path: <SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet> 
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 07 AUG 87 18:55:49 
To: Postmaster@F29.Tymnet 
Message-id: A85957@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - DUPLICATE MESSAGE 

Message #A86155 was not delivered to the following recipient(s):

        ISGFM.LIBRARY.

The message is a duplicate. Please contact
your customer support representative.

(undelivered message follows)
------------------------------------------------------------------------
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Tue, 28 Jul 87 10:32:18 PDT
From: Postmaster@F29.Tymnet 
Date: Tue, 28 Jul 87 10:31:24 PDT 
To: ISGFM.LIBRARY@EMSTUMS.Ontyme.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No supplementary address found.

TUMS.GATEWAY is a gateway into the Tymnet Universal Mail System (TUMS).
Mail sent to TUMS.GATEWAY must have one or more supplementary addresses
provided in the first non-blank lines of the message.  The addresses
should be contained in a line or lines of the form:
    TO: user@domain (TUMS)
Please refer to the file "*** TUMS.GATEWAY" for more details.



----------
RCPT To: ?-No-Address-Specified-? 
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Tue, 28 Jul 87 10:31:24 PDT
From: ISGFM.LIBRARY@EMSTUMS.Ontyme.Tymnet 
Date: 28 JUL 87 17:11:32  
Message-id: J99056@Ontyme.Tymnet 


TO:      WILLIAM DAUL


FROM:    ISG LIBRARY, L206/300/2/220


SUBJECT: RESPONSE TO YOUR REQUEST FOR LIBRARY MATERIAL


         WE HAVE RECEIVED YOUR REQUEST. WE ARE FORWARDING TO YOU OUR AUDIO
         AND VIDEO CASSETTE LISTS.  HOPEFULLY THIS RESPONSE WILL GET
         THROUGH.


         THANK YOU
         ISG LIBRARY (ONTYME ID = ISGFM.LIBRARY)
Received: from F29.Tymnet by B39.Tymnet; Fri, 7 Aug 87 15:44:21 PDT
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Fri, 7 Aug 87 15:38:46 PDT
Return-path: <SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet> 
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 07 AUG 87 20:41:22 
To: Postmaster@F29.Tymnet 
Message-id: A86728@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - DUPLICATE MESSAGE 

Message #A89367 was not delivered to the following recipient(s):

        MDCSS.B/RUSSELL.

The message is a duplicate. Please contact
your customer support representative.

(undelivered message follows)
------------------------------------------------------------------------
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Wed, 5 Aug 87 14:04:24 PDT
From: Postmaster@F29.Tymnet 
Date: Wed, 5 Aug 87 14:01:32 PDT 
To: MDCSS.B/RUSSELL@EMSTUMS.Ontyme.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No supplementary address found.

TUMS.GATEWAY is a gateway into the Tymnet Universal Mail System (TUMS).
Mail sent to TUMS.GATEWAY must have one or more supplementary addresses
provided in the first non-blank lines of the message.  The addresses
should be contained in a line or lines of the form:
    TO: user@domain (TUMS)
Please refer to the file "*** TUMS.GATEWAY" for more details.



----------
RCPT To: ?-No-Address-Specified-? 
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Wed, 5 Aug 87 14:01:32 PDT
From: MDCSS.B/RUSSELL@EMSTUMS.Ontyme.Tymnet 
Date: 05 AUG 87 17:34:33  
Message-id: J02249@Ontyme.Tymnet 
Subject: Hi Steve 

<M175594@SLVMCL1.MCAUTO.TYMNET> (TUMS)
Hi Steve:
 
 I just read your Profs message.  I've been on vacation so as you no
doubt found out, I did not attend the RAAM meeting.
 
 How are things going??  Any thing interesting from the RAAM meeting??
When is the next RAAM meeting??
 
Ben Russell
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 7 Aug 87 16:25:59 PDT 
To: davarj@calvin (Jonathan Davar) 
Subject: Re: k_terminate 
In-reply-to: <8707020127.AA06928@calvin.Tymnet.com> of Wed, 1 Jul 87 18:27:32
	PDT

I previously said that one should not k_terminate ones own process.
Well, its coded now, and its okay, if you really want to, it will work
much the same as exit() does in Unix.  In fact if you call it with a
pid of 0 (NULL) it will terminate itself.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 7 Aug 87 17:27:25 PDT 
To: coons@opus (John Coons) 
Subject: Re: TUMS 
In-reply-to: <8708071619.AA15644@opus.Tymnet.com> of Fri, 7 Aug 87 09:19:28 PDT

Try eunix.  The data base says you're supposed to get stuff there, and
the log says the following has been forwarded to you there without any
reported error.  Maybe you need to create a ".forward" file.  I will
also change the data base to send to you on opus.  -Bill

220 eunix.Tymnet Sendmail 5.51/4.7 ready at Thu, 6 Aug 87 15:44:11 PDT
I67050@Ontyme.Tymnet From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet To: <coons@eunix>
I67420@Ontyme.Tymnet From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet To: <coons@eunix>
I67420@Ontyme.Tymnet From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet To: <baba@eunix>
I67645@Ontyme.Tymnet From: NTD.BJC@EMSNTD.Ontyme.Tymnet To: <coons@eunix>
I67771@Ontyme.Tymnet From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet To: <coons@eunix>
I68127@Ontyme.Tymnet From: NTD.ARLENE@EMSNTD.Ontyme.Tymnet To: <ebs@eunix>
220 eunix.Tymnet Sendmail 5.51/4.7 ready at Fri, 7 Aug 87 13:33:34 PDT
I69113@Ontyme.Tymnet From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet To: <coons@eunix>
Received: from X930.Tymnet by B39.Tymnet; Sun, 9 Aug 87 0:45:39 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Sun, 9 Aug 87 0:43:09 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: SETOP Review... 

Thursday 8.13, 1:30pm... I may have to request that we not start until 2:00pm.
I have another meeting on Thursday (actually South Bay Center orientation)
and I'm not sure whether I'll have enough time to get there earlier.  I'll
let you know when i get to the Office on Monday... and re-read the memo with
the meeting time (there are 2 times).

/Carl
Received: from F29.Tymnet by B39.Tymnet; Mon, 10 Aug 87 10:22:30 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Mon, 10 Aug 87 10:22:25 PDT
Received: from Xerox.COM by OFFICE-1.ARPA; 10 Aug 87 10:21:11 PDT
Received: from Burger.ms by ArpaGateway.ms ; 10 AUG 87 10:20:30 PDT
Return-path: <@OFFICE-1.ARPA:Joseph_Kaminski.PARC@Xerox.COM> 
From: KAMINSKI.PARC@Xerox.COM 
Sender: "Joseph_Kaminski.PARC"@Xerox.COM 
Date: 10 Aug 87 10:18:40 PDT (Monday) 
To: WRS%B39.Tymnet@OFFICE-1.Arpa 
Cc: KAMINSKI.PARC@Xerox.COM 
Message-id: <870810-102030-3616@Xerox> 
Subject: PARC-FORUM 


		PARC FORUM 
		Thursday, August 13, 1987
		4:00 P.M., PARC Auditorium

		
		JOHN MEYER
		President
		VENTURA SOFTWARE

		will speak on

 	Office Publishing with Ventura Publisher

A PC Week reviewer commented that "software as complete and capable as
Ventura Publisher is hard to improve," but release 1.1 "has made a great
program truly superb."  Favorably reviewed in every quarter, Ventura
Publisher has mastered the job of producing phototypeset-quality
documents in the office.  On the way to creating a program with the
power to process documents with fast turnaround time, the Ventura team
developed unique features that ideally suit Ventura Publisher to network
desktop publishing applications.

This Forum is OPEN.

Host: Kathy Jarvis  (PARC Technical Information Center x4042)

Refreshments will be served at 3:45 P.M.  Refreshments are provided only
for people attending the Forum.

Requests by Xerox employees for videotaping should be sent to Susie
Mulhern <Mulhern:PA:Xerox  or  Mulhern.PA> before Tuesday noon.

Directions to reach PARC:  The Auditorium is located at 3333 Coyote Hill
Road in Palo Alto, between Page Mill Road (west of Foothill Expressway)
and Hillview Avenue, in the Stanford Research Park.  To get here, take
Page Mill Road to Coyote Hill Road.   PARC is the only building on the
left, just over the crest of Coyote Hill.  Park in the large lower lot
if visitor parking is full and enter the auditorium at the upper level
of the building.  (The auditorium is located down the stairs and to the
left of the main doors.)


Received: from X930.Tymnet by B39.Tymnet; Mon, 10 Aug 87 12:51:55 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Aug 87 12:50:03 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: Thursday meeting time ok! 

It appears that the other meeting I had on Thursday will not be taking
place as scheduled.  See you there.
/Carl
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 10 Aug 87 12:54:49 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: William R Soley <WRS@B39.Tymnet> 
Subject: Re: SETOP Review... 
In-reply-to: your message of Sun, 9 Aug 87 0:43:09 PDT

LAST MEMO STATED THURS (1:30)AND ASKED FOR CONFIRMATION. BUT 2 PM IS OK.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 10 Aug 87 13:12:56 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Wayne D Bartlett <BARTLETW@B39.Tymnet> 
Subject: Re: Thursday meeting time ok! 
In-reply-to: your message of Mon, 10 Aug 87 12:50:03 PDT

Ok - I'm confused.  1:30 or 2:00?
  [ ] Aug 13 1:30 PM
  [ ] Aug 13 2:00 PM
  [ ] Aug 13 2:30 AM (AM? AM!)
-Bill
Received: from D65.Tymnet by B39.Tymnet; Mon, 10 Aug 87 17:02:02 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Mon, 10 Aug 87 18:59:29 CDT 
To: wrs 
Subject: 7000 pages in (MAIL:65). 

Of all GAN 3 users on system 65, directory MAIL is using the most space.
6912 pages in ST.* and STX.*.  

I used to have the most space on 65, but deleted 18000 pages from JMS.
Received: from tymix.Tymnet by B39.Tymnet; Mon, 10 Aug 87 18:17:40 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18858; Mon, 10 Aug 87 18:17:29 PDT
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04073;
	Mon, 10 Aug 87 18:17:22 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA02375; Mon, 10 Aug 87 18:17:27
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Mon, 10 Aug 87 18:17:27 PDT 
To: wrs@comet 
Message-id: <8708110117.AA02375@comet.Tymnet.com> 
Subject: tymix 

i made a new home directory for you on tymix and copied the contents
of /a/sdd/wrs into it.
	steve
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 10 Aug 87 18:31:29 PDT 
To: Joe Smith <JMS@X930> 
Subject: Re: 7000 pages in (MAIL:65). 
In-reply-to: your message of Mon, 10 Aug 87 18:59:29 CDT

6912 pages freed from MAIL:65.
Thanks for pointing it out.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 10 Aug 87 18:32:22 PDT 
To: feldman@comet (Steve Feldman) 
Subject: Re: tymix 
In-reply-to: <8708110117.AA02375@comet.Tymnet.com> of Mon, 10 Aug 87 18:17:27
	PDT

Thanks -Bill
Received: from X930.Tymnet by B39.Tymnet; Tue, 14 Jul 87 5:10:56 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 14 Jul 87 0:39:27 PDT 
To: wrs 
Subject: William Christian Soley 


CONGRATULATIONS!!!


Congratulations to both of you.  Apparently, both the baby and Becky are
doing well (or you would have said so)...  I just got your mail --- been
pretty busy myself lately ---  Cherie is iPasadena... (on the phone)
and she says Congratulations too!  (She just took 3rd place in the Regional
Junior Olympic Skating competition in Downey)...


CONGRATULATIONS!!!

/Carl & Cherie
Received: from X930.Tymnet by B39.Tymnet; Wed, 15 Jul 87 2:25:13 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 15 Jul 87 2:19:54 PDT 
To: wrs 
Subject: Username (ACCESS#) 

THey actually validated that on a PDP-10?????

They can't even look at it woth most software... I wonder if they will be
able to delete it?  NONAME won't let you look at it.

(This is a rhetorical comment... Lynn Lawrence tole me that she talked to
 you and you told her "#" was illegal in a TYMCOM-X username.)

Did you know that LOGINN runs init programs at start-address + 1 ???

(at least by demonstration... it appears that way)
(F40 may not like that... meaning, anyone using an F40 INIT program will
 experience problems depending upon what they do in their programs... the
 program in question does an LUUO '015 at the start address, and a JRST 140
 at start address+1, and it gets a HALT whenever started at start+1)

Is the behavior of LOGINN proper in your opinion?  Should it ever run the
init program at start+1... (I haven't looked, but is this documented?)

Please comment, but no urgency is required... I understand that your
domestic life has changed dramatically over the weekend and you will
definately need time to adjust.  (Are you taking any vacation time?)

Please say hello to Becky and the baby... I'll probably give you a call
but, I'm being "considerate" and waiting until the initial hub-bub has
a chance to die down...

/Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 15 Jul 87 2:26:52 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: Username (ACCESS#) 
In-reply-to: your message of Wed, 15 Jul 87 2:19:54 PDT

I think it's probably the project code validation program that's running
at start address plus one.  I don't remember that being true for INIT.
What does she want to do about it?  I can still hack things if she wants
to keep the username, but I'll just have to use FET in stead of NONAME.
Oh well.  Life here is going well (and at 150%) but I've given up entirely
on being able to plan anything farther than 30 seconds into the future at
least until 11-Jul-2008.  Oh my, I just realized he'll be graduating from
the class of 2005!  That year does not seem real.  Not even 1990 seems
real.  Maybe they're not.  End babble.  End message.  -Bill
Received: from X930.Tymnet by B39.Tymnet; Wed, 15 Jul 87 13:49:40 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 15 Jul 87 13:49:07 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: ACCESS# 

Thanks... you were right!  They had the same program setup as both the
INIT program and the project-code validation program, so I didn't bother
to check it...

I think F40 doesn't like to be started at start+1, so I'm going to
suggest that Lynn has a second program for PJCVFY that sets restricted
restricted mode and the INIT program to run and runs it at the regular
start address...

I think it might be nice (now that INITIA runs the init program for the
user) to allow the INIT command in restricted mode.  Maybe change INITIA
to run the RCF program if there is no init program so it works as expected.

/Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 15 Jul 87 14:01:10 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: ACCESS# 
In-reply-to: your message of Wed, 15 Jul 87 13:49:07 PDT

I have a special program for them to run that runs in front of the Fortran
one that sets up stuff.  Do a NONAME on user INFORMATION - I think its
called "SECURE" but I forget what directory its on.  Suggest you use that
if you're going to set it up for her (which would be nice since I'm kind
of tied up at the moment).  Thanks -Bill
Received: from X930.Tymnet by B39.Tymnet; Fri, 17 Jul 87 18:25:28 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 17 Jul 87 18:21:41 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: BubbNet hosts 

Bill,
  I will contact Sandy or Kal, but I thought that I'd run it past you first.
We were planning on configuring our micronet and hosts so that they could be
accessed from another network (other than TYMNET) such as as TrainNet or
BubbNet.  The connection into BubbNet was mostly for Dennis Ernst to be able
to access that micronet, but I see an advantage for System 39, if we were to
put both of our 2020s on dual bases, one host on TYMNET and one on BUBBNET
for each host.

  Since our micronet is somewhat the proving ground for what Dennis does, it
wouldn't be in conflict with having only NTD hosts on Bubbnet...  It would
give us direct connections into B39 and eliminate the gateway problems we have
in trying to support system 39 from Tymnet.

  Do you think that we would have any problems getting the host and node
connections to Bubbnet?

/Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 19 Jul 87 9:33:46 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: BubbNet hosts 
In-reply-to: your message of Fri, 17 Jul 87 18:21:41 PDT

Sounds good to me.  I think there would be no problem so long as there
is already a bubbnet line at your location.  It is in our best interest
to have you support 39, after all.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 26 Jul 87 17:21:04 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: PDP-10 coding trick 
In-reply-to: your message of Sun, 26 Jul 87 2:41:14 PDT

So is SETOP in assembler?  -Bill
Received: from X930.Tymnet by B39.Tymnet; Mon, 27 Jul 87 13:58:40 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 27 Jul 87 13:43:06 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: SETOP IN ASSEMBLER 

Heavens no!!!  At least not yet...

I have run into string garbage collect problems, so I reduced string usage
as much as possible on output by encoding my own fill routines for numeric
and string output to overwrite the same large constant string...  My string
space still gets larger than 64K and string GC occurs when it needs to expand
and the GC takes 30+ seconds...

Know any way to reduce, turn off string GC?

I'm now re-thinking the data structure into multiple files to increase the
access speed.. expect that to be done the end of this week... I'm in class
over at TYMNET TRAINING (Intro to ISIS) all this week.

Let Wayne know... I may have a preliminary "GED" for him by next week sometime.
...assuming some progress is made this week.

/Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 7 Aug 87 16:13:28 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: CARL@X930 
Subject: Re: Reviewing SETOP with Carl 
In-reply-to: your message of Fri, 7 Aug 87 8:50:53 PDT

OK I have it down for 1:30PM 8/13 at my office (B4-535 at N.1st St.)
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 11 Aug 87 16:27:36 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23683; Tue, 11 Aug 87 16:27:15 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA11541;
	Tue, 11 Aug 87 16:27:04 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA24838; Tue, 11 Aug 87 16:27:07
	PDT
Return-path: <coons@opus> 
From: coons@opus (John Coons) 
Date: Tue, 11 Aug 87 16:27:07 PDT 
To: julies@opus, wrs@opus 
Message-id: <8708112327.AA24838@opus.Tymnet.com> 
Subject: TUMS 


Sorry to be such a pain, but TUMS is still eating my ontymes and not
delivering them to opus (or eunix either).

Today, Ontymes #I70056 and J04105 were not on my in list, not on opus
or eunix, but there they were on my in old list (they were both sent
this morning).

Thanks for your continued support (Ed and I thank you ...)

John
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 11 Aug 87 18:46:11 PDT 
To: coons@opus (John Coons) 
Cc: feldman@comet 
Subject: Re: TUMS 
In-reply-to: <8708112327.AA24838@opus.Tymnet.com> of Tue, 11 Aug 87 16:27:07
	PDT

Hmessage-id: I69850@Ontyme.Tymnet
Hmessage-id: I70056@Ontyme.Tymnet
Hmessage-id: J04105@Ontyme.Tymnet
and a test message from me are in "eunix" outgoing mail queue trying to
get delivered to opus but failing miserably.  They have about 40 other
messages for company.  I changed your mail to be forwarded to opus directly
from system 29 (the Ontyme agent).  This should work around the problem.
Let me know if you continue to have problems.  -Bill
(perhaps there would be more there, but I think it gets blown away if
it can't be delivered for 3 days)
Received: from tymix.Tymnet by B39.Tymnet; Wed, 12 Aug 87 14:55:45 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA29766; Wed, 12 Aug 87 14:55:21 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA17024;
	Wed, 12 Aug 87 14:55:10 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA15567; Wed, 12 Aug 87
	14:55:16 PDT
Return-path: <hsing@calvin> 
From: hsing@calvin (Yu-liang Hsing) 
Date: Wed, 12 Aug 87 14:55:16 PDT 
To: wrs@calvin 
Message-id: <8708122155.AA15567@calvin.Tymnet.com> 
Subject: ipcf gid mm file 

Could you tell me where the ipcf gid mm file(s) are kept? The ipcf.mm
file was at /usr/calvin3/rodney/doc/rid.
Thanks.
Yu-Liang
Received: from F29.Tymnet by B39.Tymnet; Wed, 12 Aug 87 15:01:44 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 12 Aug 87 14:55:27 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 12 AUG 87 11:40:24 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I70805@Ontyme.Tymnet 
Subject: Consolidation of System F70 To F33 

                         M E M O R A N D U M





DATE>      05 AUG 87  09:53

TO>        ALL CONCERN

COPIES>    Gary Walker
           Linda Freitas

FROM>      Jenny English
           Resource Planning and Management


SUBJECT>   Consolidation of System F70 To F33


-----------------------------------------------------------------------



System F70 (Femont) is in the process of being consolidated onto system    
F30 (Fremont), in preparation of being permanently shutdown.  The follow-  
ing groups of users will be the first to be moved from system F70 to F33   
on August 14, 1987 beginning at 1800 pacific time.  The transfer of the    
files will be handled by Fremont Operations and RPM.   
     
If an user is already valid on system F33 with files, that users' files    
will not be overwritten with system F70 files unless otherwise specified.  
     
The users who are moving are: 
CID    DIST USERNAME              COMPANY NAME    
46     148  CSCHROED  Applied Communications 
138    141  NASON     Applied Communications 
208    230  NSCNET    Network Information Service 
            NISCNET 
345    201  OUTDIAL   Tymnet  
18011  144  ACSHSA    Applied Communications 
            BARLETW 
            BUSPLN  
            CSC.SUPT
            GMASTERS
            INSDGSS1
            ISINSTALL    
            MICRO   
            X22113  
28060  229  EADAMS  
34853  129  C4ASEES   Communication Branch - New Jersey
52389  172  SYNCMAG   Mats/Dag Dev.
52391  172  DTROL     Marsplus Staff    
53353  249  FSUPPORT  Electronic Data Interchange 














60097  229  OSIRIS    MDC-Electronic Messageing System 
            SCHRAMM 
            SLLDCAIN
20389  200  45 users  MDC-NSC/NTD  
and 58 users under GAN 11020 --- Field Service Engineers    
     
     
When the move has been completed, all the above users will be removed from 
system F70 on Tuesday, August 18, 1987. 
     
Please note, system F70 will no longer be timesharing after September 4, 1987.  

If there are any questions, I may be contacted at (214)637-7324 or ontyme
IPC.JENNY
     
     
Thank you 
Jenny English
Received: from F29.Tymnet by B39.Tymnet; Wed, 12 Aug 87 15:01:49 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 12 Aug 87 14:55:37 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 12 AUG 87 13:43:23 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I70916@Ontyme.Tymnet 
Subject: NEW MAIL-BOX 

                          M E M O R A N D U M


                   MCDONNELL DOUGLAS INFORMATION SYSTEMS

Saint-Cloud - France                               
Tel. [33] (1) 49.11.22.33                          
DATE>      10 AUG 87  10:38

TO>        ALL

COPIES>    

FROM>      FIUELD SERVICE


SUBJECT>   NEW MAIL-BOX


-----------------------------------------------------------------------




PLEASE NOTE THAT A NEW MAIL-BOX HAS BEEN CREATED FOR THE
FRENCH FIELD SERVICE, GROUP MANAGED BY JEAN-FRANCOIS GUILLOU :

                
                  MDISF.FS


SO AS ODF TODAY, SEND YOUR MAILS DIRECTLY TO THIS MAIL-BOX.
THE SECRETARY WILL THEN DISPATCH THE MEMO'S TO THE APPROPRIATE
PEOPELLE.


THANK YOU FOR YOUR ATTENTION.

BEST REGARDS,
FIELD SERVICE
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 12 Aug 87 15:30:31 PDT 
To: hsing@calvin (Yu-liang Hsing) 
Subject: Re: ipcf gid mm file 
In-reply-to: <8708122155.AA15567@calvin.Tymnet.com> of Wed, 12 Aug 87 14:55:16
	PDT

That is the gid.  /usr/calvin3/rodney/doc/rid/ipcf.mm  -  if its not there
at the moment, you just need to do
    sccs get ipcf.mm
or if you want to edit...
    sccs edit ipcf.mm
and it will appear.  Make knows how to do this automatically if you want
a copy to print, say
    make ipcf
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 12 Aug 87 19:29:09 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00291; Wed, 12 Aug 87 19:28:56 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00310;
	Wed, 12 Aug 87 19:28:46 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA17670; Wed, 12 Aug 87
	19:28:52 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Wed, 12 Aug 87 19:28:52 PDT 
To: wrs@calvin 
Message-id: <8708130228.AA17670@calvin.Tymnet.com> 
Subject: "thanks for your prompt response."... 

thanks for your prompt response. mike j.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 12 Aug 87 21:50:48 PDT 
To: carl@930, jms@930 
Subject: change these passwords 

The following passwords should probably be changed since they leave
something to be desired in terms of security.  They were "guessed"
by a program which tries words from a dictionary against all users
in the mud.  Thanks -Bill

public net:
FRENCH              FRENCH         [3,232631]      30Jul80 
GO AWAY             OSNF           [3,140673]      28Jan85 
NEW USER            OSB            [3,145107]      24Aug84 
NEW USER            OSP            [3,140674]      24Aug84 
NEW USER            T10AIL         [3,326471]      24Aug84 
NEW USER            TYMAIL         [3,326742]      24Aug84 
PASCALDOC           PASCALDOC      [3,315030]      17Apr80 

bubb net:
GO AWAY             JMS            [3,431116]      14Mar86 so
Received: from X930.Tymnet by B39.Tymnet; Wed, 12 Aug 87 22:05:06 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 12 Aug 87 22:03:16 PDT 
To: William R Soley <WRS@B39.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Subject: password changes 

Public net passwords have been changed for
  FRENCH & PASCALDOC  (same password)
  OSNF, OSB & OSP     (same different password)
  T10AIL & TYMAIL     (same different password)

If you need to know these for any reason, ask me soon... least I forget
what I changed them to... I NEVER write passwords down...
/Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 12 Aug 87 22:07:30 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: password changes 
In-reply-to: your message of Wed, 12 Aug 87 22:03:16 PDT

Thanks - see you tomorrow 1:30 at my office.  Need directions?
You'll get to see the expression on Wayne's face when I show him
a two inch thick list of passwords I guessed last night.  (20,000)
He's never seen it before since I haven't run one since before he
took this job.  He doesn't know its coming so it will be a surprise.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 31 Jul 87 21:59:08 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "okay Bill...more q. 1. boot ack"... 
In-reply-to: <8708010306.AA04858@calvin.Tymnet.com> of Fri, 31 Jul 87 20:06:45
	PDT

1. boot ack should describe the current process with the bits which
have been processed cleared from bootBits

2. If you get an RERR from the ipcf, you can consider it hopeless.  It
is not likely to clear up.  The only errors I would think you can recover
from are the failed done states of the ipcf message that are returned in
ipcf progress field.  Things like out of memory (which is the only reason
ipcfAssign and such would fail should be treated as fatal since the only
way to get more memory if the patron refuses to give it to us is to crash
the patron and take it anyway.  These errors indicate a serious problem
and are not something that is expected to happen on a correct processor.

-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 1 Aug 87 1:44:55 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, along the same lines as"... 
In-reply-to: <8708010829.AA05171@calvin.Tymnet.com> of Sat, 1 Aug 87 01:29:29
	PDT

The only reason for sprout to fail is if Rodney is out of memory.  In
the case of the boot, there is no appearent graceful recovery that I can
imagine, so I think crash.  Hopefully, when Rodney calls you back at bootInit
after the crash, you can continue where you left off.  This time there will
be memory (unless things are completely pathalogic).  It is likewise nice to
be able to continue if the ipcfAssign() fails due to lack of Rodney memory.
The ipcf send failure that's reported in the progress field of IPCF_NOROOM
is presumably temporary - retry with some time out.  The IPCF_TOOBIG is also
an indication that there is not enough memory at the receiving end, but this
is almost certain to be permanent (i.e. configuration error vs. run error).

Forgive me if I bable -- I hope I'm making some sense.
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 19:31:45 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00855; Thu, 6 Aug 87 19:17:17 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05455; Tue, 4 Aug 87 15:42:14
	PDT
Return-path: <wrs@hobbes> 
From: wrs@hobbes (Bill Soley) 
Date: Tue, 4 Aug 87 15:42:14 PDT 
To: wrs@hobbes 
Message-id: <8708042242.AA05455@hobbes.Tymnet.com> 
Subject: Rodney 1915, 2302 time accounting 


From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 6 Aug 87 21:58:23 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "hghhgh test"... 
In-reply-to: <8708062138.AA11237@calvin.Tymnet.com> of Thu, 6 Aug 87 14:38:38
	PDT

----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 19:32:12 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00824; Thu, 6 Aug 87 19:16:41 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13770;
	Thu, 6 Aug 87 14:40:35 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11237; Thu, 6 Aug 87 14:38:38
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 6 Aug 87 14:38:38 PDT 
To: wrs@calvin 
Message-id: <8708062138.AA11237@calvin.Tymnet.com> 
Subject: "hghhgh test"... 

hghhgh test
----------------------------------------------------------------
It appears that Tymix is back up...
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 6 Aug 87 22:06:10 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, it is quite unbelievable"... 
In-reply-to: <8708062113.AA10307@calvin.Tymnet.com> of Thu, 6 Aug 87 14:13:25
	PDT

Mike, I have received the following messages just now:
Tue, 4 Aug 87 11:06:09 PDT     "Bill, I am confused a little."...
Thu, 6 Aug 87 14:13:25 PDT     "Bill, it is quite unbelievable"...
Thu, 6 Aug 87 14:38:38 PDT     "hghhgh test"...
Thu, 6 Aug 87 14:41:39 PDT     jhj
Thu, 6 Aug 87 14:41:56 PDT     test
I suspect these are all the ones you've sent in the last 3 days.  I suspect
you have received the others back.  Tymix is obviously up now and everything
should be fine as long as it comes up at least a couple hours every other
day.  That is to say I think things are back to their old reliable self as
far as mail is concerned.  Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 7 Aug 87 0:47:55 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02996; Fri, 7 Aug 87 00:47:39 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15859;
	Fri, 7 Aug 87 00:47:36 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14134; Fri, 7 Aug 87 00:47:35
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 7 Aug 87 00:47:35 PDT 
To: wrs@calvin 
Message-id: <8708070747.AA14134@calvin.Tymnet.com> 
Subject: "Bill, about IPCF_GONE. if you"... 

Bill,
about IPCF_GONE. if you talk to 10 different tsaps at the same time
and you always set connectionMode bit, how do you know to which tsap
IPCF_GONE relates to? it is bit confusing to me.
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 7 Aug 87 0:51:05 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, about IPCF_GONE. if you"... 
In-reply-to: <8708070747.AA14134@calvin.Tymnet.com> of Fri, 7 Aug 87 00:47:35
	PDT

The first message sent with connectionMode set establishes a transport
connection.  After that, you may only send connectionMode messages to
that same tsap.  You may still send non-connectionMode messages anywhere
you want, but they are not subject to IPCF_GONE.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 7 Aug 87 0:56:29 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03063; Fri, 7 Aug 87 00:56:09 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15862;
	Fri, 7 Aug 87 00:56:07 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14151; Fri, 7 Aug 87 00:56:05
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 7 Aug 87 00:56:05 PDT 
To: wrs@calvin 
Message-id: <8708070756.AA14151@calvin.Tymnet.com> 
Subject: "Bill, thanks for such a speedy!"... 

Bill,
thanks for such a speedy! answer.
okay- it means if I never set connection bit I will never get _GONE
interrupt?
Mike
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 7 Aug 87 0:57:01 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, thanks for such a speedy!"... 
In-reply-to: <8708070756.AA14151@calvin.Tymnet.com> of Fri, 7 Aug 87 00:56:05
	PDT

Correct, the GONE interrupt is only for connection mode.  You could get
GONE progress code, though, if you attempt to send to a board that is out
to lunch (or dinner).

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 7 Aug 87 1:28:58 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03397; Fri, 7 Aug 87 01:28:38 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15895;
	Fri, 7 Aug 87 01:28:33 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14202; Fri, 7 Aug 87 01:28:31
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 7 Aug 87 01:28:31 PDT 
To: wrs@calvin 
Message-id: <8708070828.AA14202@calvin.Tymnet.com> 
Subject: "well, but progress code is an"... 

well, but progress code is an argument to interrupt routine, so
how can you pass it without the actual interrupt?
Mike
Received: from tymix.Tymnet by B39.Tymnet; Fri, 7 Aug 87 1:33:57 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03410; Fri, 7 Aug 87 01:33:36 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15911;
	Fri, 7 Aug 87 01:33:31 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14212; Fri, 7 Aug 87 01:33:30
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 7 Aug 87 01:33:30 PDT 
To: wrs@calvin 
Message-id: <8708070833.AA14212@calvin.Tymnet.com> 
Subject: "if you send to an out-to-lunch"... 

if you send to an out-to-lunch board - okay if ipcfSend() accepts
it what happens next? assume all interrupts are enabled except
the connection. do I get IPCF_FAILED, but then not IPCF_GONE!
mike
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 7 Aug 87 2:00:38 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "well, but progress code is an"... 
In-reply-to: <8708070828.AA14202@calvin.Tymnet.com> of Fri, 7 Aug 87 01:28:31
	PDT

GONE is a terminal state and you will therefore get an FAILED interrupt.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 7 Aug 87 2:01:56 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "if you send to an out-to-lunch"... 
In-reply-to: <8708070833.AA14212@calvin.Tymnet.com> of Fri, 7 Aug 87 01:33:30
	PDT

When sending a message to an out-to-lunch board, 
you may either get an RERR_TSAPI error return from ipcfSend(),
or an RERR_OKAY return followed some time later by an IPCF_FAILED
interrupt with the progress code set to IPCF_PROGRESS_NODEST.
I hadn't looked in the manual when I sent the last message, so
I got some of the names of the things wrong, but the concepts are
roughly the same, that is: IPCF_GONE, IPCF_PROGRESS_NODEST, and
RERR_TSAPI all mean that the recipient does not exist.  (none of
them imply one way or the other wether the recipient ever did exist)
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 11 Aug 87 22:34:24 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA24578; Tue, 11 Aug 87 22:34:07 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12355;
	Tue, 11 Aug 87 22:33:59 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11731; Tue, 11 Aug 87
	22:34:04 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 11 Aug 87 22:34:04 PDT 
To: wrs@calvin 
Message-id: <8708120534.AA11731@calvin.Tymnet.com> 
Subject: "Bill, q1. what is in your opinion"... 

Bill,
q1. what is in your opinion a reasonable thing to do in case a fatal
format error in load-file - crash or try the same file on different server?
q2. I want to verify with you I can use bcopy() and bzero() functions.
q3. I need another 'C' trick:
    lets say I have declared an integer variable A. I also have a pointer
    (char type) that points somwhere in the memory (pointer p).
    1. I want to copy byte *p into A, so it will appear in its proper
        least significant position in A.
    2. how about copying 2,3 or 4 consecutive bytes pointed by p into
       A to keep A happy!

?
thanks,
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 12 Aug 87 4:39:13 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, q1. what is in your opinion"... 
In-reply-to: <8708120534.AA11731@calvin.Tymnet.com> of Tue, 11 Aug 87 22:34:04
	PDT

> q1. what is in your opinion a reasonable thing to do in case a fatal
> format error in load-file - crash or try the same file on different server?

That's a tough one.  In reality, there is almost never going to be another
server.  Perhaps we should have a scheme for a backup file.  There could be
two copies of the same file on the same server and if the primary fails,
you try the backup.  What do you think?  I'm just thinking out loud.

> q2. I want to verify with you I can use bcopy() and bzero() functions.

Yes, in fact I have a couple macros that are nice for using with bzero().
They are in ~wrs/v1/include/zero.h

> q3. I need another 'C' trick:
>     lets say I have declared an integer variable A. I also have a pointer
>     (char type) that points somwhere in the memory (pointer p).
>     1. I want to copy byte *p into A, so it will appear in its proper
>         least significant position in A.
>     2. how about copying 2,3 or 4 consecutive bytes pointed by p into
>        A to keep A happy!

char *p;
int a;
p = "x";
a = *p;

results in a having a value of 0x78 which I think is what you wanted.
The compiler notices that they are objects of different sizes and does
the right thing.  Objects of type char, short, int, long are all compatible
in this way although storing a big value (i.e. int) into a small variable
(i.e. char) is questionable, it will work if you don't mind it discarding
the high order bits.  Also beware that most compilers will sign extend
the above example, so consider...
p = "\377";
a = *p;
Now a has the value of -1 because on the 68000 char's are signed.  You
can get around this by declaring p as
unsigned char *p;
(or more popular... u_char *p; )
so
u_char *p;
int a;
p = "\377";
a = *p;
Yields a with a value of 0377.

-Bill

Please also see Kernigham and Ritchie [1978] section 2.7 Type Conversions
pp. 39-42 and also section A-6 Conversions pp. 183,184.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 12 Aug 87 12:31:22 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA26811; Wed, 12 Aug 87 12:30:56 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15610;
	Wed, 12 Aug 87 12:30:46 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14539; Wed, 12 Aug 87
	12:30:51 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Wed, 12 Aug 87 12:30:51 PDT 
To: wrs@calvin 
Message-id: <8708121930.AA14539@calvin.Tymnet.com> 
Subject: "Bill, thanks for your answers."... 

Bill,thanks for your answers. having a back-up file in the same server
sounds ok because there is a good chance if one file is screwed up
the other is not. if we decide on it I can easily add this to my logic.
I was told however that there may be more than one server - SFS0 to SFS9
and currently this is my primary way to recover. if you say it will be
seldom that more than one is available - then there is a problem.

your answer to char/integer problem does not satisfy me 100% - it is
my fault I did not present the case clearly enough.
p is actually a pointer defined like p = (char*)malloc(size).
so 
a = *p
probably would transfer just one byte, anyway this statement lacks
the information how many bytes I want to transfer.
to be more specific I am going to use it when reading Start Address from
the file. suppose I find out that its length is 3 bytes (length of StartA..)
and I want to transfer those 3 bytes into 'a' which probably must
be declared as long int.
if i did
a = *p
how the system knows how many bytes I want to transfer?
thanks
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 12 Aug 87 15:20:43 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, thanks for your answers."... 
In-reply-to: <8708121930.AA14539@calvin.Tymnet.com> of Wed, 12 Aug 87 12:30:51
	PDT

Well, there are many difficulties in writting code like that which interfaces
with outside data structures.  I'm almost sure the start address would be 4
bytes and so you would just do a cast:

char *p;
u_long startAdr;
p = malloc(whatever);
a = *(u_long*)p;

This makes it think, temporarily, that p is a pointer to a long and so
it does a 4 byte move, but it isn't clear the bytes are in the right
order.  You may end up picking them out as bytes and doing << and | to
assemble them into the proper word - to be avoided if possible, but it
sometimes comes to that.  Now if you really wanted 3 bytes, there are a
few things you could do, none of them elegant.  The best is probably to
do the byte by byte thing and use << and |.  That would be most portable.
You could also do 
  a = *(u_long*)p & 0xFFFFFF;
which gets 4 bytes and then discards one.  Again, this suffers from
being byte order dependent.  I don't mean to say that taking advantage
of byte order is bad, but it should only be used when the tradeoff
between portability and effiiency is justified.  It should always by
marked as "byte order dependent" or "machine dependent" by a comment.

-Bill

PS - most of Ron's X.409 code uses byte operations with << and |.  It
is most general and really not the inefficient.  I think I lean that
way personally.



From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 12 Aug 87 15:29:10 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, thanks for your answers."... 
In-reply-to: <8708121930.AA14539@calvin.Tymnet.com> of Wed, 12 Aug 87 12:30:51
	PDT

Oh yes,  X.409 integers are very strange.  You never know ahead of time how
big they will be.  You should try to share Ron's routines so we won't have
to have space in ROM for two sets of almost identical routines.  -Bill
Received: from X930.Tymnet by B39.Tymnet; Wed, 12 Aug 87 22:18:15 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 12 Aug 87 22:13:35 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: Directions... 

I know where the building is..., as long as I keep the message with your
floor and cubicle number i shouldn't have too much trouble... although
I could use directions from the elevator on your floor, to your office.
I know what the layout of our floor in BLDG D looks like, so I presume the
standard elevator/facilities are similar.

I'd like to see the password list myself... you should have been able to
guess "MY" BUBBNET password.. it's an awkward (but correct) spelling of a
person's name... no spaces,numbers or special characters.

What is the top password?  "WELCOME"?

/Carl
Received: from X930.Tymnet by B39.Tymnet; Wed, 12 Aug 87 22:24:06 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 12 Aug 87 22:20:17 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: password guessing 

Have you put anything into your program to check for those people who
have taken things like "GO AWAY" or "ALICE" and changed them to "G0 AWAY"
and "AL1CE" or "A1ICE"  just to conform... still just as guessable if you
know the person/product/service only it conforms or is that c0nf0rms?

I know.... since my password is the name of a place (not in your dictionary)
that is mythical, with just such a misspelling to allow it to match the new
security spec of an imbedded non-alphabetic.

/Carl

Received: from X930.Tymnet by B39.Tymnet; Thu, 13 Aug 87 2:53:05 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Aug 87 2:39:08 PDT 
To: William R Soley <WRS@B39.Tymnet>, Wayne D Bartlett <BARTLETW@B39.Tymnet> 
Subject: Meeting Info 

Please type (read) the file (CARL:930)SETOP.PRP prior to our meeting.  I
have updated the document to reflect the changes I've made to the design
(some of which are implemented in my test version).

Some questions I have -- open for discussion are:

1)  Who should be allowed to see what information in the oper database.
    At present, any manager may see any subordinate and any individual
    may see their own information.
    How do we allow anyone to grant license to someone NOT subordinate
    one or more levels below them? (If they cannot see them!)

2)  How do we want to make various users able to see and change everything?
    (I'm not asking for implementation details... I'm asking from a policy
     point of view... from a legality point of view [freedom of information
     and all that]).
    Obviously, WRS & CARL & a few others can change anything... but how do
    we want to officiate that capability...  especially in the case of a
    system disaster.

On a different topic... we should discuss any fallout from the NAD episode
of a week or two ago and how that changes would affect various support
and development groups.  (I haven't gotten back to Doug or Dennis on this).


...I'm fading... more questions, but I'm becomng incoherent... see you
both at 1:30pm at Bill's office.

/Carl
Received: from F29.Tymnet by B39.Tymnet; Thu, 13 Aug 87 15:59:36 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 13 Aug 87 15:57:30 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Error response(s) from SLVMCL1.McAuto.Tymnet ...
SNDMSG000R Transmit message, end with '.':
SNDMSG001R Transmit recipients, end with '.':
DMKCSP053E SERVICES NOT IN CP DIRECTORY
SNDMSG003W Send to SERVICES at SLVMCL1 failed due to error 53
SNDMSG004W Send failed to 1 recipients.
SNDMSG005E Note sent to 0 recipients.
R; T=0.19/0.63 17:57:52



----------
RCPT To: <SERVICES@SLVMCL1.McAuto.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 13 Aug 87 15:56:29 PDT 
To: MDAIS NETWORK <SERVICES@SLVMCL1.McAuto.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected
in extended name field or 1st line of body.

----------
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Thu, 13 Aug 87 15:56:20 PDT
From: MDAIS NETWORK <SERVICES@SLVMCL1.McAuto.Tymnet>, TOM
	<CARR@SLVMCL1.McAuto.Tymnet> 
Date: 13AUG87 1746PM 
To: CA317439@DCMS5.MCAUTO.TYMNET <CA317439@DCMS5.McAuto.Tymnet> 
Subject: PROFS ACCESS VIA NET B OVER TYMNET 
RECEIVED: from TMS.Tymnet by F29.Tymnet; Thu, 13 Aug 87 15:53:23 PDT
MESSAGE-ID: 015164@TMS.Tymnet

I WANTED TO PASS ALONG SOME MORE INFO ON HOW TO ACCESS PROFS FROM A
LOCATION SUCH AS SEOUL, KOREA....THERE ARE A NUMBER OF PROCEDURES
REQUIRED TO CONNECT WITH THE PARTICULAR PROFS HOST WHICH
MAINTAINS YOUR PERSONAL PROFS ID...SUCH AS DCMS1 OR THE LIKE.......
THIS MESSAGE HAS BEEN COPIED FROM A PROCEDURE PUT TOGETHER BY LEWIS
HILL OF MDAIS...
 
         IBM 3708 PROTOCOL CONVERSION SYSTEM
 
INTRODUCTION:
 
A NEW IBM 3708 PROTOCOL CONVERTER HAS BEEN ADDED TO REPLACE THE PRESENT
PERLE PROTOCOL CONVERTER.  THERE ARE EIGHT IMPUT LINES FROM TYMNET WITH
AN ADDRESS OF 3106971557 AND PASSWORD OF MCAUTO.  EACH LINE IS AUTOBAUD,
OPERATING AT 300 OR 1200 BPS WITH TERMINALS CONFIGURED FOR FULL DUPLEX
(ECHOPLEX) AND NO PARITY.  EACH LINE WILL SUPPORT A CRT AND AN OPTIONAL
ATTACHED PRINTER SHARING A COMMON DIAL LINE.
 
TERMINAL CONFIGURATION:
 
THE IBM 3708 PROTOCOL CONVERTER SUPPORTS A NUMBER OF TERMINAL FORMATS
A FEW ARE LISTED AS FOLLOWS:
 
              1 -IBM 3101
              1A-IBM PC/FT TERM COLOR
              1B-IBM PC/FT TERM MONO
 
 
OPERATIONAL MATTERS:
 
1. DIAL UP TYMNET IN THE USUAL MANNER.
2. TYMNET REPLYS WITH: PLEASE LOG ON
   ENTER: 3106971557
3. TYMNET REPLIES WITH: ENTER PASSWORD
   ENTER: MCAUTO
4. TYMNET REPLIES WITH: HOST IS ON LINE
5. THE PROTOCOL CONVERTER LISTS A MENU OF TERMINALS
   ENTER: 5  OR 20 FOLLOWED BY A CARRIAGE RETURN
6. ENTER: 1 FOLLOWED BY A CARRIAGE RETURN ( THIS IS FOR THE OPERATIONAL
            LINE)
YOU WILL SEE ECHOS OF WHAT YOU ENTER FOR STEPS 5 AND 6....
7. THE PROTOCOL CONVERTER REQUESTS YOUR   SIGNON ID....
   ENTER: DCMS2. OR THE PROFS HOST ID WHICH YOU WANT
   (NOTE) THE PERIOD IS USED FOR ENTER....
8. NOW YOU LOG ON TO PROFS..
 
BE SURE TO USE VMEXIT WHEN YOU ARE DONE WITH PROFS.....
 
THE CHARACTERS
 
 
 
. WILL ALSO DISCONNECT YOU FROM THE PROTOCOL
CONVERTER....
WHEN YOU DISCONNECT FROM THE PROTOCOL CONVERTER, YOU ARE TAKEN BACK TO
THE TYMNET LOGON...
 
TAKE A LOOK AT THESE INSTRUCTIONS AND SEE IF THEY MAKE SENSE....
I'LL BE IN TOUCH WITH MORE INFORMATION...
 
REGARDS
TOM CARR
 
 
CARR 001 1748PM 13AUG87
SLFOX 130 1748PM 13AUG87
Received: from X930.Tymnet by B39.Tymnet; Thu, 13 Aug 87 17:15:09 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Aug 87 17:12:17 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: LPR from 930 

Apparently it doesn't work very well... can't even get the circuit...

   !lpr -t setop.prp
   BUBBN
   login: lost circuit
   Called from 402164  Last SAIL call at 2030
   EXIT
   !

Lookes like it starts to do gateway stuff then immediately dies...
sigh.

/carl
  From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 14 Aug 87 0:13:48 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: LPR from 930 
In-reply-to: your message of Thu, 13 Aug 87 17:12:17 PDT

I ran it this morning from 930 to print your document and it worked fine.
I assume you're running the one in XEXEC.  Maybe there is a problem with
the gateway it was trying to use.  -Bill
Received: from X930.Tymnet by B39.Tymnet; Fri, 14 Aug 87 11:11:55 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 14 Aug 87 11:02:41 PDT 
To: William R Soley <WRS@B39.Tymnet> 
Subject: LPR 

I presume I was running (XEXEC)LPR ... anyway, since it's going to be a few
weeks before I can realistically pick-up listing... I'll wait until we move.

Our plan is to de-install 930, etc on the 24th...  however, we expect to have
a KI/KL in the fremont data center to use for 3 weeks during the move host #
32 (KL) or 24 (KI, if they swap processors on us).

...more later...
/Carl
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 17 Aug 87 8:37:07 PDT 
To: WRS@C39.Tymnet 
Cc: CARL@X930 
Subject: "Confirming thurs, 9/3 meeting"... 

Confirming thurs, 9/3 meeting with you,Carl and Doug E. However, can we
make it @ 1:30????(have Dr.'s App'mt
NOTE - just for you - no caps!!!!!!!!
???was it to be inyour bldg, in a conference room????
Received: from D65.Tymnet by B39.Tymnet; Mon, 17 Aug 87 10:31:18 PDT
Return-path: <STHOMPSON@d65.Tymnet> 
From: SUSAN THOMPSON <STHOMPSON@d65.Tymnet> 
Date: Mon, 17 Aug 87 10:31:18 PDT 
To: Mail Wizard <MAIL> 
Cc: brahmb 
Subject: TUMS-10 bug report (v45000000 of 23-MAY-87) 

Somehow, my ontymes are getting read (pmts.s/thompson), but aren't
showing up in my TUMS list--Sue


Received: from F29.Tymnet by B39.Tymnet; Mon, 17 Aug 87 12:47:54 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 17 Aug 87 12:45:16 PDT
Return-path: <NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet> 
From: NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet 
Date: 17 AUG 87 08:04:28 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I72329@Ontyme.Tymnet 
Subject: ETHEL JAMES 

 
                      Tymnet/Network Technology Development
                          PROGRAM MANAGEMENT DEPARTMENT

 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      17 AUG 87  09:32

TO>        NTD Personnel

COPIES>    Ken Holcomb
           Ethel James
           Program Management Staff

FROM>      Allyn Morriss


SUBJECT>   Welcome Ethel James


-----------------------------------------------------------------------
Please join me in welcoming Ethel James as the newest member of my staff.
Ethel was hired on August 10th to perform project management duties and
will have several NTD programs to manage after completion of her startup 
training.
 
Ethel comes to us from Quadrex Corporation after serving in many capacities
for over twelve years. Her positions have been varied in such roles as
Technical Editor, Project Administrator, Manager of Administrative and Control
Systems and, most recently, as the Manager of Administration. Her managerial
and administrative skills makes her quite suitable to act as a program
coordinator in the number of areas that require a central contact to
interface with multiple organizations, in and out of NTD.
 
She will be kept busy in the initial weeks becoming familiar with other
job functions performed by Program Management while working to understand
Tymnet technologies. A key part of her training will be to meet the people
from NTD and those of other Network Systems organizations. Her first
assignment(s) will follow.
 
Again, join me in welcoming Ethel aboard and wishing her well in her
new role as a Senior Technical Support Analyst with prime responsibilities
for the project management of one or more NTD programs.
 
Regards,
Allyn Morriss (408/922-6151)
Manager, NTD/Program Management
NTD.A/MORRISS
Received: from F29.Tymnet by B39.Tymnet; Mon, 17 Aug 87 12:47:59 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 17 Aug 87 12:45:25 PDT
Return-path: <NTD.R/HARTSOE@EMSNTD.Ontyme.Tymnet> 
From: NTD.R/HARTSOE@EMSNTD.Ontyme.Tymnet 
Date: 17 AUG 87 10:56:36 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I72473@Ontyme.Tymnet 
Subject: WE'RE HERE! 
WE'REHERE!: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      17 AUG 87  10:55                  Network Technology Development

TO>        NTD EMPLOYEES

COPIES>    

FROM>      RILLA HARTSOE


SUBJECT>   WE'RE HERE!


-----------------------------------------------------------------------


FOR YOUR INFORMATION:

ENGINEERING IS LOCATED ON THE FIRST AND SECOND FLOORS.  THE FIRST FLOOR
IS PILOT OPERATIONS AND PRODUCT SUPPORT (KAREN GOSNEY AND DAVID STEPHENS).
EVERYONE ELSE IS ON THE SECOND FLOOR.

OUR MESSAGE CENTER NUMBER IS 6692 (ELLAN STOKES).  DAVE ALCORN'S NUMBER
IS 6066, MINE IS 6034.  TO LOCATE OUR MESSAGE CENTER/DAVE'S OFFICE/MY
OFFICE TAKE TWO RIGHT TURNS ONCE YOU STEP OFF THE ELEVATOR...

MORE LATER!

RILLA
Received: from F29.Tymnet by B39.Tymnet; Mon, 17 Aug 87 12:48:05 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 17 Aug 87 12:45:35 PDT
Received: from F29.Tymnet by EMSNTD.Ontyme.Tymnet; Mon, 17 Aug 87 11:13:51 PDT
Received: from B39.Tymnet by F29.Tymnet; Mon, 17 Aug 87 11:13:30 PDT
Received: from tymix.Tymnet by B39.Tymnet; Mon, 17 Aug 87 11:12:15 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14591; Sun, 16 Aug 87 11:28:59 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA02878; Sun, 16 Aug 87 11:28:52
	PDT
Return-path: <coons@opus> 
From: coons@opus (John Coons) 
Date: Sun, 16 Aug 87 11:28:52 PDT 
To: ntd.w/soley@ontyme 
Message-id: <8708161828.AA02878@opus.Tymnet.com> 
Subject: TUMS retry 


Bill,

My "thank you" for fixing TUMS for me was returned from B39.
Here it is again via Ontyme:

From MAILER-DAEMON@tymix Sun Aug 16 11:14:41 1987
Return-Path: <MAILER-DAEMON@tymix>
Received: from tymix.Tymnet by opus.Tymnet.com (3.2/SMI-3.2)
        id AA02856; Sun, 16 Aug 87 11:14:39 PDT
Received: by tymix.Tymnet (5.51/4.7)
        id AA14557; Sun, 16 Aug 87 11:14:33 PDT
Date: Sun, 16 Aug 87 11:14:33 PDT
From: MAILER-DAEMON@tymix (Mail Delivery Subsystem)
Subject: Returned mail: Cannot send message for 3 days
Message-Id: <8708161814.AA14557@tymix.Tymnet>
To: <coons@opus>
Status: R

   ----- Transcript of session follows -----
451 <WRS@39.Tymnet.com>... reply: read error
<WRS@39.Tymnet.com>... reply: read error

   ----- Unsent message follows -----
Received: by tymix.Tymnet (5.51/4.7)
        id AA01954; Thu, 13 Aug 87 10:45:59 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2)
        id AA02283; Thu, 13 Aug 87 10:45:48 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2)
        id AA00509; Thu, 13 Aug 87 10:45:54 PDT
Date: Thu, 13 Aug 87 10:45:54 PDT
From: coons@opus (John Coons)
Message-Id: <8708131745.AA00509@opus.Tymnet.com>
To: wrs@opus
Subject: TUMS
Cc: julies@opus


Bill,

Thanks for fixing TUMS for me ... I'm now getting my Ontymes on opus.
Did you fix this only for me, or will others be encountering the same
problem?

Thanks again,

John
Received: from tymix.Tymnet by B39.Tymnet; Mon, 17 Aug 87 12:52:24 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA17663; Mon, 17 Aug 87 12:52:17 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA14492;
	Mon, 17 Aug 87 12:51:58 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA06000; Mon, 17 Aug 87
	12:52:12 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 17 Aug 87 12:52:12 PDT 
To: wrs@calvin 
Message-id: <8708171952.AA06000@calvin.Tymnet.com> 
Subject: "Bill, I will ask you to add a"... 

Bill,
I will ask you to add a few errors to enum Rodney Error that will
be used in errorCode of boot rsp msg. dose it have to be something
general like - some failure, I am retrying - or something specific
like - msg deliver failure, or file server does not respond, or
file continuity failure, etc. ? I can do it either way.
also in case rkBootstrap() can not sprout Bootmain, should I crash
or - maybe better - return another Rodney Error code ?
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 17 Aug 87 13:17:30 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, I will ask you to add a"... 
In-reply-to: <8708171952.AA06000@calvin.Tymnet.com> of Mon, 17 Aug 87 12:52:12
	PDT

If sprout fails, I think just crash.  It is not something that will fix
itself so waiting and trying again won't help.  As for an error code, I'm
not sure that the caller can do anything constructive.  Its like "what do
you when you're trying to commit suicide and the gun doesn't work?".
You probably don't want to take it back to the store, you probably want
to just jump out a window.  -Bill
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 17 Aug 87 14:17:42 PDT 
To: WRS@C39.Tymnet, CARL@X930 
Subject: "Thought you would be interested"... 

Thought you would be interested in the following - also Brandt c. plans to 
join us on 9/3.- 
----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Mon, 17 Aug 87 12:40:00 PDT
Received: from EMSTYM.Ontyme.Tymnet by F29.Tymnet; Mon, 17 Aug 87 12:28:15 PDT
Return-path: <QSATS.D/ELLENBERG@EMSTYM.Ontyme.Tymnet> 
From: QSATS.D/ELLENBERG@EMSTYM.Ontyme.Tymnet 
Date: 17 AUG 87 09:24:37 
To: TYM.W/BARTLETT@EMSTYM.Ontyme.Tymnet 
Message-id: M12608@Ontyme.Tymnet 
Subject: NAD TECHALERT Update 
NADTECHALERTUPDATE-LicenseChange: 

                            M E M O R A N D U M

                                                        [] TYMNET - NETWORK
                                                           SYSTEMS SUPPORT
DATE>      17 AUG 87  09:22

TO>        ALLTECH
           ALLSALES

COPIES>    

FROM>      Douglas Ellenberg


SUBJECT>   NAD TECHALERT UPDATE - License Change


-----------------------------------------------------------------------


                     ******************************
                     ******************************
                     **   NAD TECHALERT UPDATE   **
                     ******************************
                     ******************************


                            PREVIOUS NOTICE
                            ---------------
      For system security reasons, the license assigned to the NAD
      program have been changed.  These changes were performed by the
      TYMCOM operations group in conjunction with Network Security.


                                 UPDATE
                                 ------
To date there have been no reported problems with NAD and the present
license assigned to it.  The measures undertaken on July 30th were
successful in allowing full access for NAD to the source files.

NAD is operating with 100% functionality and there was no interruption
to service.
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Mon, 17 Aug 87 15:37:41 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18347; Mon, 17 Aug 87 15:37:35 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15518;
	Mon, 17 Aug 87 15:37:16 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA07533; Mon, 17 Aug 87
	15:37:29 PDT
Return-path: <hsing@calvin> 
From: hsing@calvin (Yu-liang Hsing) 
Date: Mon, 17 Aug 87 15:37:29 PDT 
To: wrs@calvin 
Message-id: <8708172237.AA07533@calvin.Tymnet.com> 
Subject: invalid op-code 

When I tried to compile glob-Tadpoll.s with the C source files, I got an
(as: error (glob-Tadpoll.s:#): Invalid op-code) error message.  Should I
first assemble .s file then link it with C files or ...?
Yu-Liang
Received: from tymix.Tymnet by B39.Tymnet; Mon, 17 Aug 87 16:35:18 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18550; Mon, 17 Aug 87 16:35:10 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15843;
	Mon, 17 Aug 87 16:34:50 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA09150; Mon, 17 Aug 87
	16:35:02 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 17 Aug 87 16:35:02 PDT 
To: wrs@calvin 
Message-id: <8708172335.AA09150@calvin.Tymnet.com> 
Subject: "Bill, if I read your answer correctly"... 

Bill, if I read your answer correctly I don't have to worry about
the precise definition of errorCode in BOOT_RSP. But pobably I have
to be more precise when logging an error message.
Mike J.
Received: from D65.Tymnet by B39.Tymnet; Mon, 17 Aug 87 18:29:23 PDT
Return-path: <STHOMPSON@d65.Tymnet> 
From: SUSAN THOMPSON <STHOMPSON@d65.Tymnet> 
Date: Mon, 17 Aug 87 18:29:29 PDT 
To: Mail Wizard <MAIL> 
Cc: brahmb 
Subject: Thanks 

I'm getting my ontyme messages OK now--thanks for taking care of it.--Sue


Received: from X930.Tymnet by B39.Tymnet; Mon, 17 Aug 87 19:41:27 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Mon, 17 Aug 87 19:37:37 PDT 
To: wrs 
Cc: Monitor Group <JMS>, Carl, Osman, FletcherC, DenCoff 
Subject: FINDIT modified to not list TMP files. 

So as to not confuse RDIST with garbage names from deleted files, FINDIT
no longer reports files with an extension of ".TMP" or files with bit 8 set in
.RBLIC (the TMP file bit).  Column 60 of the output is blank for normal files,
is a dollar sign for JIS-8 Katakana files.  (SPL)FINDIT.DOC has been updated.
		/Joe
Received: from X930.Tymnet by B39.Tymnet; Mon, 17 Aug 87 19:44:51 PDT
Return-path: <JMS@X930.Tymnet> 
From: JMS@X930.Tymnet 
Date: Mon, 17 Aug 87 19:41:41 PDT 
To: wrs 
Subject: "RDIST sometimes skips files. On"... 

RDIST sometimes skips files.  On directories with many files (such as M33 which
has much less than 8000 files), RDIST sometimes reports "missing on slave" and
"missing on master" for files that exist on both systems.  Sometimes running 
RDIST a 2nd time works fine - I haven't noticed any particular pattern yet.
Just thought you'd want to know.   ?joe
Received: from F29.Tymnet by B39.Tymnet; Mon, 17 Aug 87 21:07:37 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Mon, 17 Aug 87 21:07:04 PDT 
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
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Mon, 17 Aug 87 21:04:22 PDT
From: Postmaster@F29.Tymnet 
Date: Mon, 17 Aug 87 20:59:51 PDT 
To: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No supplementary address found.

 is a gateway into the Tymnet Universal Mail System (TUMS).
Mail sent to  must have one or more supplementary addresses
provided in the first non-blank lines of the message.  The addresses
should be contained in a line or lines of the form:
    TO: user@domain (TUMS)
Please refer to the file "*** TUMS.GATEWAY" for more details.



----------
RCPT To: ?-No-Address-Specified-? 
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Mon, 17 Aug 87 20:49:46 PDT
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 18 AUG 87 00:32:38  
Message-id: A02657@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - INVALID RECIPIENTS 

Message #A02655 was not delivered to the following recipient(s):

        PMTS.B/MCKERRIE.

These recipients do not exist.
Please check the spelling of the recipient(s) listed above.

(undelivered message follows)
------------------------------------------------------------------------

Received: from F29.Tymnet by B39.Tymnet; Mon, 17 Aug 87 21:09:28 PDT
Return-path: <WRS@B39.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 17 Aug 87 21:09:25 PDT 
To: sandymf@b39 
Subject: emsbub 

Will it ever return?  If not, I'd love to delete all the kludges
that treat it as a special case.  thanks -Bill
Received: from D37.Tymnet by B39.Tymnet; Mon, 17 Aug 87 22:44:48 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 5:45:12 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 17-Aug-87 20:30:01
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Mon, 17 Aug 87 22:55:38 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 5:56:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 17-Aug-87 22:45:14
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:25:37 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 6:07:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 17-Aug-87 22:56:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:25:40 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 6:18:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 17-Aug-87 23:07:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:25:44 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 6:29:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 17-Aug-87 23:18:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:25:48 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 6:40:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 17-Aug-87 23:29:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:25:51 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 6:51:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 17-Aug-87 23:40:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:25:55 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 7:02:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 17-Aug-87 23:51:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:25:58 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 7:13:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 00:02:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:03 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 7:24:05 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 00:13:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:06 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 7:35:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 00:24:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:10 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 7:46:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 00:35:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:14 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 7:57:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 00:46:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:17 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 8:08:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 00:57:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:21 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 8:19:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 01:08:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:24 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 8:30:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 01:19:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:28 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 8:41:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 01:30:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:32 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 8:52:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 01:41:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:36 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 9:03:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 01:52:06
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:39 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 9:14:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 02:03:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:43 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 9:25:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 02:14:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:47 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 9:36:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 02:25:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:51 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 9:47:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 02:36:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:54 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 9:58:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 02:47:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:26:58 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 10:09:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 02:58:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:27:02 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 10:20:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 03:09:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 3:27:06 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 10:31:06 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 03:20:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:04 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 10:42:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 03:31:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:08 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 10:53:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 03:42:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:12 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 11:04:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 03:53:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:17 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 11:15:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 04:04:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:21 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 11:26:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 04:15:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:26 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 11:37:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 04:26:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:31 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 11:48:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 04:37:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:38 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 11:59:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 04:48:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:45 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 12:10:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 04:59:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:51 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 12:21:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 05:10:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:58:59 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 12:32:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 05:21:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:06 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 12:43:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 05:32:10
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from F29.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:11 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 18 Aug 87 11:48:57 PDT
Return-path: <NTD.R/HARTSOE@EMSNTD.Ontyme.Tymnet> 
From: NTD.R/HARTSOE@EMSNTD.Ontyme.Tymnet 
Date: 18 AUG 87 07:30:54 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I72902@Ontyme.Tymnet 
Subject: ENGINEERING 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      18 AUG 87  07:29                  Network Technology Development

TO>        NTD EMPLOYEES

COPIES>    

FROM>      RILLA HARTSOE


SUBJECT>   ENGINEERING 


-----------------------------------------------------------------------


AN ADDITIONAL NOTE TO MY ONTYME OF YESTERDAY...RON HOLST'S MECHANICAL
DESIGN GROUP IS LOCATED ON THE FIRST FLOOR.
Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:14 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 12:54:07 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 05:43:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from F29.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:18 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 18 Aug 87 11:49:10 PDT
Return-path: <NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet> 
From: NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet 
Date: 18 AUG 87 07:54:29 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I72916@Ontyme.Tymnet 
Subject: CONFERENCE ROOMS 
CONFERENCEROOMS: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      18 AUG 87  07:54                  Network Technology Development

TO>        NTD PERSONNEL

COPIES>    

FROM>      JOY SARGENT X 6152


SUBJECT>   CONFERENCE ROOMS


-----------------------------------------------------------------------


I SPOKE WITH LINDA JOHNSON YESTERDAY AND WE AGREED THE CLERICAL ASSISTANTS
ON EACH FLOOR COULD BE RESPONSIBLE FOR PROVIDING THE COFFEE IN THEIR
RESPECTIVE CONFERENCE ROOMS DURING A MEETING.  HOWEVER, BE SURE WHEN YOU
RESERVE THE ROOM THAT YOU ADVISE HER IF REFRESHMENTS WILL BE NEEDED.

THE PHONE NUMBERS ARE:

5TH FLOOR - 6649
4TH FLOOR - 6498
3RD FLOOR - 6069
2ND FLOOR - 6692
Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:19 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 13:05:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 05:54:07
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from F29.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:23 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 18 Aug 87 11:49:22 PDT
Return-path: <NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet> 
From: NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet 
Date: 18 AUG 87 10:25:14 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I73014@Ontyme.Tymnet 
Subject: SECURITY REQUESTS 
SECURITYREQUESTS: 

 
 
 
 
 
                        MCDONNELL DOUGLAS
                    INFORMATION SYSTEMS GROUP
                     INTEROFFICE MEMORANDUM
 
Network Systems Company            Network Technology Development
Tymnet                                 Date:  18 August 1987
 
 
SUBJECT>    SECURITY REQUESTS
 
TO>         NTD Personnel
 
FROM>       Joy Sargent
-----------------------------------------------------------------
 
Employees requesting a security escort to their cars should first
call x 6191 or x 6192, which is the security control center.  This
will enable the officer on duty to dispatch a guard to our lobby
for escort purposes.  The control center is manned on a 24-hour
basis.
 
If any employee is refused escort service, he/she should obtain the
name of the guard (if possible), note the date and time, and report
the incident to John A. Miller, Administrative Manager of Guard Ser-
vices, at x 6192.
Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:25 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 13:16:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 06:05:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:32 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 13:27:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 06:16:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:38 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 13:38:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 06:27:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:45 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 13:49:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 06:38:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 11:59:51 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 14:00:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 06:49:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:00 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 14:11:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 07:00:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:07 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 14:22:10 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 07:11:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:11 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 14:33:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 07:22:10
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:15 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 14:44:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 07:33:08
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:20 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 14:55:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 07:44:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:24 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 15:06:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 07:55:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:29 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 15:17:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 08:06:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:33 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 15:28:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 08:17:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:38 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 15:39:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 08:28:10
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:42 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 15:50:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 08:39:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:47 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 16:01:10 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 08:50:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:52 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 16:12:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 09:01:10
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:00:58 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 16:23:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 09:12:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:02 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 16:34:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 09:23:11
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:07 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 16:45:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 09:34:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:12 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 16:56:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 09:45:10
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:18 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 17:07:10 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 09:56:10
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:23 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 17:18:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 10:07:10
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:28 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 17:29:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 10:18:12
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:33 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 17:40:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 10:29:10
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:38 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 17:51:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 10:40:11
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:45 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 18:02:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 10:51:11
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:50 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 18:13:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 11:02:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:54 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 18:24:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 11:13:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:01:59 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 18:35:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 11:24:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:02:04 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 18:46:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 11:35:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:02:08 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 18:57:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 11:46:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:02:36 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 19:08:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 11:57:11
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from tymix.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:07:38 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA21049; Tue, 18 Aug 87 11:06:52 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18959; Tue, 18 Aug 87
	11:06:32 PDT
Return-path: <davarj@hobbes> 
From: davarj@hobbes (Jonathan Davar) 
Date: Tue, 18 Aug 87 11:06:32 PDT 
To: wrs@hobbes 
Message-id: <8708181806.AA18959@hobbes.Tymnet.com> 
Subject: Talk to me ! ! 


Bill,

please contact me sometime today at 922-6092 or with talk.

Thanks,
Jonathan.

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:13:37 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 19:19:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 12:08:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:24:40 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 19:30:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 12:19:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:35:42 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 19:41:10 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 12:30:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:46:46 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 19:52:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 12:41:11
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from F29.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:51:46 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Tue, 18 Aug 87 19:57:19 UT 
To: MAIL 
Subject: Req #9493 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #9493 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  18-Aug-87 10:18:43
; Actual run: 18-Aug-87 10:18:43
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 12:57:41 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 20:03:09 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 12:52:11
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 13:08:38 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 20:14:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 13:03:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

Received: from D37.Tymnet by B39.Tymnet; Tue, 18 Aug 87 13:08:39 PDT
Return-path: <PERPOPER@D37.Tymnet> 
Reply-to: SUBMIT 
From: D37 Job Processor 
Sender: PERPOPER@D37.Tymnet 
Date: Tue, 18 Aug 87 20:14:08 UT 
To: MAIL 
Subject: Req #13178 DSK:(MAIL)MAILER.SAV Abort-Time (0) -Normal- 

; Processed:  Req #13178  Running DSK:(MAIL)MAILER.SAV
; Scheduled:  17-Aug-87 20:32:00
; Actual run: 18-Aug-87 13:03:09
;
; Options:  Mail:Error Restart:Start Unique:Job
; Limits:  Time 10  TRUs 0
; Flags:   Run  Detach
; Status:  Abort-Time (0)   -Normal-

From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 18 Aug 87 15:35:49 PDT 
To: WRS@C39.Tymnet 
Subject: "I'm off til next tues 8/25 just"... 

I'm off til next tues 8/25 (I just knew you had to know!!)
Received: from tymix.Tymnet by B39.Tymnet; Tue, 18 Aug 87 17:25:18 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22581; Tue, 18 Aug 87 17:30:10 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA21686;
	Tue, 18 Aug 87 17:29:48 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA16605; Tue, 18 Aug 87
	17:30:04 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 18 Aug 87 17:30:04 PDT 
To: wrs@calvin 
Message-id: <8708190030.AA16605@calvin.Tymnet.com> 
Subject: "Bill, those infinite questions"... 

Bill,
those infinite questions about error conditions haunt me...

1. if load bits indicate that I should wait for a file and
   in the meantime F server goes bananas - should I keep on waiting?
2. if load bits indicate crash if file does not exist - and file
   does exist but F server becomes sick - what do next?
3. use residual memory image - like before file exists but server
   starts acting up...?
4. can you think of any F server error conditions that would
   suggest trying another server except perhaps _SHUT?

Mike
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 18 Aug 87 18:46:31 PDT 
To: JMS@X930.Tymnet 
Subject: Re: "What sort of a system is HOBBES?"... 
In-reply-to: your message of Mon, 17 Aug 87 19:55:52 PDT

hobbes is a Sun server running 4.3 Unix slightly modified by Sun.
25 MHz 68020.  So are calvin, opus, rocky, borris and bullwinkle.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 18 Aug 87 18:51:14 PDT 
To: mjastrz@calvin (Mike Jastrzebski) 
Subject: Re: "Bill, those infinite questions"... 
In-reply-to: <8708190030.AA16605@calvin.Tymnet.com> of Tue, 18 Aug 87 17:30:04
	PDT

>  1. if load bits indicate that I should wait for a file and
>     in the meantime F server goes bananas - should I keep on waiting?

Yes.

>  2. if load bits indicate crash if file does not exist - and file
>     does exist but F server becomes sick - what do next?

In general, if the file server is sick, I think you can treat it the
same as if the file was not found, except maybe log it.

>  3. use residual memory image - like before file exists but server
>     starts acting up...?

Same answer as #2.

>  4. can you think of any F server error conditions that would
>     suggest trying another server except perhaps _SHUT?

I don't know, what are the F server error conditions you aren't
sure how to handle?  I don't have a copy of the file system doc here.

-Bill
Received: from X930.Tymnet by B39.Tymnet; Tue, 18 Aug 87 19:24:14 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Tue, 18 Aug 87 19:24:23 PDT 
To: WRS@HOBBES 
Cc: WRS@B39 
Subject: The ".Tymnet" domain. 

One of the messages from your had the following:
     Received: from B39.Tymnet by X930.Tymnet
     Received: from tymix.Tymnet by B39.Tymnet
     Received: by tymix.Tymnet (5.51/4.7) id AA18817
     Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16948

Is "B39.Tymnet" part of the same domain as "xxxxx.Tymnet.com"?
Does Hobbes have its own connection to .COM or does it go thru
something else (like OFFICE-1.ARPA)?

I'm sending this message to see if TUMS understands "WRS@HOBBES"
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 19 Aug 87 2:19:25 PDT 
To: Joe Smith <JMS@X930> 
Subject: Re: The ".Tymnet" domain. 
In-reply-to: your message of Tue, 18 Aug 87 19:24:23 PDT

We're not really part of .com officially, and hobbes shouldn't really be
using that name.  It isn't clear what our official address will be if we
ever get around to getting one.  Probably (for example) jms@x930.mdc.com
or possibly jms@x930.tymnet.com, but not likely since that's not our name.
Currently, there is no name server in TUMS or on the Unix systems, so I
(tums) assume that anything ending in .com .gov .arpa .edu, etc. is on
the DARPA internet and route it to office-1.  So, wrs@hobbes.Tymnet.com
is not likely to work, although I may make a special case check for it
and change it to just wrs@hobbes.Tymnet (kludge).  At any rate, I've lost
track of your question.  I think you mea to understand how we can be
addressed from other nets and the answer is: from usenet, only by explicit
path ...  ...oliveb!tymix!jms%x930, and from anywhere on the DARPA internet:
jms%x930.Tymnet@office-1.arpa (also <@office-1.arpa:jms@x930.Tymnet> but
that doesn't work in some mailers so some sites may not be able to send
that kind of address).  Did I answer your question?  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 19 Aug 87 11:10:20 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00388; Wed, 19 Aug 87 11:15:22 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23855; Wed,
	19 Aug 87 11:14:59 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00383; Wed, 19 Aug 87 11:15:09 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Tue, 18 Aug 87 19:24:23 PDT 
To: WRS@HOBBES 
Cc: WRS@B39 
Message-id: <8708191815.AA00383@tymix.Tymnet> 
Subject: The ".Tymnet" domain. 

One of the messages from your had the following:
     Received: from B39.Tymnet by X930.Tymnet
     Received: from tymix.Tymnet by B39.Tymnet
     Received: by tymix.Tymnet (5.51/4.7) id AA18817
     Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16948

Is "B39.Tymnet" part of the same domain as "xxxxx.Tymnet.com"?
Does Hobbes have its own connection to .COM or does it go thru
something else (like OFFICE-1.ARPA)?

I'm sending this message to see if TUMS understands "WRS@HOBBES"
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 20 Aug 87 14:03:58 PDT 
To: BRUCES@D55.Tymnet 
Subject: Re: TUMS-10 bug report (v45000000 of 23-MAY-87) 
In-reply-to: your message of Wed, 19 Aug 87 12:41:52 PDT

* Wed, 19 Aug 87 8:20:04 PDT; MAILER 1.10;
ONYTME.Q: [no router table entry]
RTS: BRUCES@D55.Tymnet
?maiRTS: BRUCES@D55.Tymnet; Failed-RCPT: <mgt.k/hollnad@onytme>

So what's an ONYTME?
-Bill

(don't worry it happens to all of us)

Received: from D55.Tymnet by B39.Tymnet; Thu, 20 Aug 87 14:25:24 PDT
Return-path: <BRUCES@D55.Tymnet> 
From: BRUCES@D55.Tymnet 
Date: Thu, 20 Aug 87 14:28:53 PDT 
To: wrs@39 
Subject: Re: TUMS-10 bug report (v45000000 of 23-MAY-87) 

Sorry, I can't believe I looked at that 100 times and never noticed
the typo.

Bruce

Received: from F29.Tymnet by B39.Tymnet; Fri, 21 Aug 87 0:06:39 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Fri, 21 Aug 87 7:12:04 UT 
To: MAIL 
Subject: Req #11843 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #11843 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  21-Aug-87 00:00:00
; Actual run: 21-Aug-87 00:00:03
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Fri, 21 Aug 87 10:58:20 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 21 Aug 87 11:01:37 PDT
Return-path: <NTD.L/JOHNSON@EMSNTD.Ontyme.Tymnet> 
From: NTD.L/JOHNSON@EMSNTD.Ontyme.Tymnet 
Date: 21 AUG 87 10:15:27 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I74800@Ontyme.Tymnet 
Subject: DRUE BONDAD 
NEWRESPONSIBILITYFORDRUEBONDAD: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      21 AUG 87  10:13                  Network Technology Development

TO>        NTD MANAGERS

COPIES>    

FROM>      MICHAEL F. RUDE


SUBJECT>   NEW RESPONSIBILITY FOR DRUE BONDAD


-----------------------------------------------------------------------


I AM PLEASED TO ANNOUNCE THAT DRUE BONDAD HAS ACCEPTED THE RESPONSIBILITIES
OF ACTING MANAGER OF THE SNA/SDLC SECTION.

DRUE HAS BEEN A MEMBER OF THE SNA/SDLC TEAM SINCE 1983 WITH PRIME RESPONSIBILITY
FOR THE SDLC PRODUCT.  RECENTLY SHE ALSO ASSUMED RESPONSIBILITY FOR THE SNA
PRODUCT.

I FEEL THAT DRUE'S UNDERSTANDING OF THESE PRODUCTS AND EXPERIENCE WITHIN NTD
SHOULD ENABLE HER TO BE HIGHLY EFFECTIVE IN HANDLING HER NEW RESPONSIBILITIES.
I ASK YOU ALL TO HELP SUPPORT DRUE IN HER NEW ROLE.

MFR/lj
Received: from tymix.Tymnet by B39.Tymnet; Fri, 21 Aug 87 12:04:29 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09631; Fri, 21 Aug 87 12:09:42 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09594; Fri,
	21 Aug 87 12:09:36 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09622; Fri, 21 Aug 87 12:09:31 PDT
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA28925; Fri,
	21 Aug 87 09:50:41 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA21275; Fri,
	21 Aug 87 09:50:33 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Fri, 21 Aug 87 09:50:33 PDT 
To: oliveb!tymix!wrs 
Message-id: <8708211650.AA21275@oliven.ATC.OLIVETTI.COM> 
Subject: reply 


>Right.  I'll probably talk to you before you read this, but I'll reply
>anyway.  I won't forget about Friday, but since Ola's been sick, we are
>reluctant to bring the baby to the house.  I may come alone.  I know that
>Becky would like to see the stuff, but another day, perhaps.

Oh well, when she finds out, she'll be disappointed.

> We are considering alternatives.  One of them is to get
> rid of San Benito (sell it to someone else).

Well, I don't know what that really means.

>  One of them is to just
> sell everything and say "fuck it".

I get dibs on a couple of the GE's!

> One of them is to tripple the dues (which wouldn't work).

No kidding.

> One of them is to incorporate as a non-
> profit organization and then they can write it off (and so can we).

Sounds good.  That would also work well with the controller development
situation, until you actually start selling them.  What does it entail
and would the Corbin2 assholes go for it??

> One of them is to keep everything and look for another site.
-Bill

I doubt that you would have the time for that.
We'll talk tonight.

Gary

From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 21 Aug 87 13:14:14 PDT 
To: <feldman@comet> 
Subject: Re: phones.tym 
In-reply-to: <8708210021.AA00293@comet.Tymnet.com> of Thu, 20 Aug 87 17:21:14
	PDT

Re: ^L - they're gone now.  Wonderful dBase-III does \f\r\n at the end of
a page.  I suppose that's more general, but anoying.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 21 Aug 87 13:17:06 PDT 
To: <M173036@SLVMCL1.McAuto.Tymnet> 
Subject: Re: ARPA net and others 
In-reply-to: your message of 08/20/87 16:26:27

An arpanet user should be able to send you mail using the following address:
	M173036%SLVMCL1.McAuto.Tymnet@Office-1.ARPA
this should also work, but some sites don't handle it right:
	<@Office-1.ARPA:M173036@SLVMCL1.McAuto.Tymnet>

Likewise, you can send to ARPANET by simply using their address in the first
line of your PROFS message.  The line must be of the form:
	TO: address1, address2, ... , addressN (TUMS)
If there are too many addresses to fit on a line, you may have any number of
such lines as long as there are no blank lines or other lines in between.
An ARPANET address looks just like a TUMS address except it ends with one
of the following domains: .ARPA, .COM, .EDU, .GOV (and some others I don't
remember).  For example, to send me mail on my ARPANET address, begin your
PROFS note with
	To: Bill Soley <WRS@Office-1.ARPA> (TUMS)
or just
	To: WRS@Office-1.ARPA (TUMS)
[everything outside <> is ignored by the mail system]
Then send the PROFs note to TUMSGATE at SLVMCL1.  There is a directory of ARPA
users maintained by SRI-NIC which can be accessed online or in printed form.
Very few users are actually registered, however, so I've always found it
useless.  For more information on that, send mail to Bill Daul:
	Bill Daul <WBD.MDC@Office-1.ARPA>

TUMS can communicate through ARPANET to bitnet.  I don't know how.  I think
Bill Daul can tell you this, also.  In general, if you know how, you can
talk to just about anything through TUMS.  The ones I have used are:
	ISG operated Tymnet hosts
	Usenet (Unix)
	ARPAnet (DARPA Internet)
	Telex
	TWX
	WU - mailgram
	WU - telegram
	PROFS
	TMS
	Ontyme

I hope this helps -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 21 Aug 87 15:08:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09930; Fri, 21 Aug 87 15:13:20 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA11032; Fri,
	21 Aug 87 15:13:15 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09921; Fri, 21 Aug 87 15:13:10 PDT
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA01620; Fri,
	21 Aug 87 12:11:29 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA22402; Fri,
	21 Aug 87 12:11:24 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Fri, 21 Aug 87 12:11:24 PDT 
To: oliveb!tymix!wrs 
Message-id: <8708211911.AA22402@oliven.ATC.OLIVETTI.COM> 
Subject: so 


So, what's your schedule tonight??

Gary
Received: from tymix.Tymnet by B39.Tymnet; Thu, 20 Aug 87 15:13:33 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06415; Thu, 20 Aug 87 15:18:38 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04770; Thu,
	20 Aug 87 15:18:14 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA09442; Thu, 20 Aug 87 15:18:33 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 20 Aug 87 15:18:33 PDT 
To: wrs@f104 
Message-id: <8708202218.AA09442@f104.sun.com> 
Subject: Debugging the scheduler 

From rmurphy Wed Aug 19 16:25:31 1987
Return-Path: <rmurphy>
Received: by f104.sun.com (3.2/SMI-3.2)
	id AA08768; Wed, 19 Aug 87 16:25:22 PDT
Date: Wed, 19 Aug 87 16:25:22 PDT
From: rmurphy (Ron Murphy)
Message-Id: <8708192325.AA08768@f104.sun.com>
To: bill
Subject: background scheduling

You said today that if a process is a higher priority than another process,
the second one will not be run until the first process is done in some way
that I didn't get.  Is this: until the first process dismisses? (k_sleep or
k_wait)
Ron
 


From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 20 Aug 87 15:42:10 PDT 
To: oserang 
Subject: Phillip Glass 

Do you know anything about Phillip Glass?  I was thinking of getting one
of his albums, one of:
1.	Mishima (sound track, digital)
2.	The Photographer
3.	Glassworks
4.	Kronos Quartet Sculthorpe/Sallinen/Glass/Nancarrow/Hendrix
		Company, Purple Haze, et. al. (digital)
Any comments?  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 20 Aug 87 16:03:50 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06598; Thu, 20 Aug 87 16:08:40 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05506; Thu,
	20 Aug 87 16:08:13 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06591; Thu, 20 Aug 87 16:08:29 PDT
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA27445; Thu,
	20 Aug 87 15:32:10 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA15066; Thu,
	20 Aug 87 15:32:04 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Thu, 20 Aug 87 15:32:04 PDT 
To: oliveb!tymix!wrs 
Message-id: <8708202232.AA15066@oliven.ATC.OLIVETTI.COM> 
Subject: Don't forget 

This Friday you were supposed to have some time open in the
evening!

There will be interesting things for both B&B (and maybe even B).

By the way, since you two are going to SF (and we are not, I guess),
would it be possible to stop at Headlines while you are up there?
There are a couple of sites, one at Polk/Sutter and one on
Market St. (cross?).   I know that parking is really the pits,
but I thought I'd ask.

The item is a blue/green Lava Lite!  It costs $49 at headlines
and ~69 at the Exploratorium (that's an unreasonable price).
The blue lights are very hard to find whereas the red/yellow
ones you can get at Best for $39 -- in stock.  But I really
like the blue/green ones the best.  I think I'll go for a Prisoner
decor!  (Now, where did I leave that weather balloon?)

Gary
Received: from tymix.Tymnet by B39.Tymnet; Thu, 20 Aug 87 17:16:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06725; Thu, 20 Aug 87 17:21:13 PDT
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06172;
	Thu, 20 Aug 87 17:20:47 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA00293; Thu, 20 Aug 87 17:21:14
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Thu, 20 Aug 87 17:21:14 PDT 
To: wrs@comet 
Message-id: <8708210021.AA00293@comet.Tymnet.com> 
Subject: phones.tym 

the phones.tym you created has some control-L's in it.
	steve
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Thu, 20 Aug 87 17:15:08 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: Phillip Glass 

Bill,

I was thinking of getting some Phillip Glass myself but have no idea what
the stuff is like.  I'm worried about it being boring, but really don't
know for sure.  Let me know if you risk it.  There's lots of stuff in
the new age section that I have no idea what or who it is, and no
way to really find out.  Some of the CDs that I got that were really bad
I returned as defective (trading for something else), or just sold
to the stores that buy used CDs.

Thanks...Omar

From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 20 Aug 87 17:17:27 PDT 
To: <rmurphy@f104> 
Subject: Re: Debugging the scheduler 
In-reply-to: <8708202218.AA09442@f104.sun.com> of Thu, 20 Aug 87 15:18:33 PDT

A lower priority process will not run if a higher priority process is
on the run q (i.e. ready).  So: until the first process blocks. (k_sleep
or k_wait cause blocking, but k_dismiss does not).  Try getting a newer
copy of the exec gid:
	cd ~wrs/rid; make exec
-Bill
(it has a helpful state table)
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 20 Aug 87 17:25:54 PDT 
To: <oliveb!gnome@tymix.Tymnet> 
Subject: Re: Don't forget 
In-reply-to: <8708202232.AA15066@oliven.ATC.OLIVETTI.COM> of Thu, 20 Aug 87
	15:32:04 PDT

Right.  I'll probably talk to you before you read this, but I'll reply
anyway.  I won't forget about Friday, but since Ola's been sick, we are
reluctant to bring the baby to the house.  I may come alone.  I know that
Becky would like to see the stuff, but another day, perhaps.

Meanwhile, thanks for baby stuff.

Corbin & Corbin called Putney and said we need to pay them $175/month
for Monte Bello or get out.  This is most likely prohibitively
expensive.  We are considering alternatives.  One of them is to get
rid of San Benito (sell it to someone else).  One of them is to just
sell everything and say "fuck it".  One of them is to tripple the
dues (which wouldn't work).  One of them is to incorporate as a non-
profit organization and then they can write it off (and so can we).
One of them is to keep everything and look for another site.  One of
them is to have a nervous breakdown.  Oh well.

-Bill
Received: from F29.Tymnet by B39.Tymnet; Thu, 20 Aug 87 19:38:45 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 20 Aug 87 19:34:54 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 20 AUG 87 15:05:38 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I74463@Ontyme.Tymnet 
Subject: NTD ADDRESS/PHONE CHANGES 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      20 AUG 87  12:19                  Network Technology Development

TO>        ALL TYMNET PERSONNEL

COPIES>    AL FENN

FROM>      GAIL CONE


SUBJECT>   NTD ADDRESS/PHONE CHANGES


-----------------------------------------------------------------------



          PLEASE NOTE THAT NTD'S NEW LOCATION IS 2580 N. 1ST. ST. - BLDG B.
          OUR  NEW  FAX PHONE NUMBER IS 922-6125.   ALSO NOTE THAT FLOORS 1
          THROUGH 5 HAVE MESSAGE CENTERS.   FOR GENERAL  ASSISTANCE  PLEASE
          CONTACT:
 
          ELLAN STOKES  - X.7792 - 2ND FLOOR - ENGINEERING DEPT
          SILVIA PULIDO - X.6069 - 3RD FLOOR - NETWORK INTERFACES & APPS
          DIANA VORNE   - X.6498 - 4TH FLOOR - NTD HQ & NETWORK SYS. & ARCH
          ANNE HARRIMAN - X.6649 - 5TH FLOOR - CONSULTING & SUPPORT SERVCS
 
 
          THE FOLLOWING LIST INDICATES DEPT HEAD, DEPT, SECRETARY, ETC.
 
 
          AL FENN,V.P. - NTD HQ - X.6347 - 4TH FLOOR
               GAIL CONE,SECRETARY - X.6090 - 4TH FLOOR
 
          DAVE ALCORN,DIRECTOR - ENGINEERING DEPT.- X.6066 - 2ND FLOOR
               RILLA HARTSOE,SECRETARY - X.6034 - 2ND FLOOR
 
          BILL EUSKE,DIRECTOR - NETWORK SYS & ARCH - X.6093 - 4TH FLOOR
               STARR BLACK,SECRETARY - X.6722 - 4TH FLOOR
 
          MIKE RUDE,DIRECTOR - NETWORK INTERF & APPS - X.6160 - 3RD FLOOR
               LINDA JOHNSON,SECRETARY - X.6057 - 3RD FLOOR
          KEN HOLCOMB,DIRECTOR - CONS & SUPPORT SERVCS - X.6614 5TH FLOOR
               JOY SARGENT,SECRETARY - X.6152 - 5TH FLOOR
 
 
NTD PERSONNEL...

PLEASE MAKE SURE YOUR VISITORS ARE AWARE OF THE FACT THAT OUR
ACTUAL LOCATION IS 2580 NOT 2560.  THE LATTER IS FOR MAILING ONLY.

GC
Received: from F29.Tymnet by B39.Tymnet; Thu, 20 Aug 87 19:38:50 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 20 Aug 87 19:35:03 PDT
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Thu, 20 Aug 87 16:27:05 PDT
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Thu, 20 Aug 87 14:27:22 PDT
Return-path: <M173036@SLVMCL1.McAuto.Tymnet> 
From: Jim Weinhold <M173036@SLVMCL1.McAuto.Tymnet> 
Date: 08/20/87 16:26:27 
To: NTD.B/SOLEY@Ontyme.Tymnet 
Subject: ARPA net and others 

Cliff Achord directed me your way for the following questions, ifyou are not
the proper person please direct me elsewhere.

1. It looks as if we can send PROFS notes to ARPA net user's thru TUMSGATE
   on CP20.

2. How do we come up with the mailing address info for ARPA net users? I
   think that I have part of a address for a user. In that from some samples
   that I have, ARPA net users appear to have mail distributors.

3. Is BITNET accessable to TUMSGATE/CP20 Profs? What other nets can be
   accessed thru these means?

4. Do we have some kind of ARPA net directory?

Thanks,
Jim Weinhold
Phone 314-233-2903
Received: from F29.Tymnet by B39.Tymnet; Mon, 24 Aug 87 0:09:06 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Mon, 24 Aug 87 7:14:52 UT 
To: MAIL 
Subject: Req #11928 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #11928 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  24-Aug-87 00:00:00
; Actual run: 24-Aug-87 00:00:03
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Mon, 24 Aug 87 9:08:55 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 24 Aug 87 9:14:12 PDT
Return-path: <NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 24 AUG 87 09:10:06 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I75467@Ontyme.Tymnet 
Subject: Weekly Project Review 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                                  MCDONNELL DOUGLAS
                              INFORMATION SYSTEMS GROUP
                               INTEROFFICE MEMORANDUM
 
          Network Systems Company            Network Technology Development
          Tymnet                                           No:CSS870824.001
                                                 Date:August 24, 1987  6:44
 
          SUBJECT>       Next Skywalker Weekly Review
 
          TO>            Dick Rawson              BJC
                         Dan Lasater              Bill Euske
                         Gazel Tan                John Kopf
                         Bill Soley               Mike Rude
                         Pat Driscoll             Michael Stimac
                         Dave Alcorn              Dave Bliss
                         John Coons               Allyn Morriss
 
          COPIES>        Al Fenn
 
          FROM>          Ken Holcomb
          -----------------------------------------------------------------
 
          Due to  Mike Rebmann's  inability to change the Skywalker Project
          Meeting from Wednesday to Thursday, it is necessary to return our
          weekly meeting  to Thursday.   I  am sorry  for any inconvenience
          this may cause you.  Our next meeting is scheduled  for Thursday,
          August 27 at 1:30 in conference room B3-505.
 
          Ken........
 
 
 
 
 
 
 
 
 
 
 
 
 
                                          1
Received: from tymix.Tymnet by B39.Tymnet; Mon, 24 Aug 87 17:06:22 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02613; Mon, 24 Aug 87 17:11:43 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09038;
	Mon, 24 Aug 87 17:11:34 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA28105; Mon, 24 Aug 87
	17:11:38 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 24 Aug 87 17:11:38 PDT 
To: wrs@calvin 
Message-id: <8708250011.AA28105@calvin.Tymnet.com> 
Subject: "Bill, In case of DONE interrupt"... 

Bill,
In case of DONE interrupt I hope handle.msgPtr will point to the
outbound message that was declared DONE. is it the case? if not, and
handle is valid only for inbound msg I wonder how big a deal it was
to expand its meaning?
Mike J.
Received: from F29.Tymnet by B39.Tymnet; Mon, 24 Aug 87 18:01:53 PDT
Return-path: <JMS@D35> 
From: Joe Smith <JMS@D35> 
Date: Mon, 24 Aug 87 18:03:18 PDT 
To: WRS@B39 
Subject: MAIL.FWD goes into the bit bucket. 

When my MAIL.WFD said JMS@D35, all my mail went into the bit bucket.
After I changed it to JMS@D35.TYMNET it worked.  I would not expect that the
mail system be so persnickety about ".TYMNET" inside of MAIL.FWD.  /Jms
Received: from D35.Tymnet by B39.Tymnet; Mon, 24 Aug 87 18:02:02 PDT
Received: from F29.Tymnet by D35.Tymnet; Mon, 24 Aug 87 18:07:52 PDT
Return-path: <JMS@D35> 
From: Joe Smith <JMS@D35> 
Date: Mon, 24 Aug 87 18:03:18 PDT 
To: WRS@B39 
Subject: MAIL.FWD goes into the bit bucket. 

When my MAIL.WFD said JMS@D35, all my mail went into the bit bucket.
After I changed it to JMS@D35.TYMNET it worked.  I would not expect that the
mail system be so persnickety about ".TYMNET" inside of MAIL.FWD.  /Jms
Received: from F29.Tymnet by B39.Tymnet; Mon, 24 Aug 87 20:39:06 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 24 Aug 87 20:41:11 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 24 AUG 87 16:06:15 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I75871@Ontyme.Tymnet 
Subject: SOUTH BAY CENTER OCCUPANCY 
SOUTHBAYCENTEROCCUPANCY: 

 
 
 
 
 
                        MCDONNELL DOUGLAS
                    INFORMATION SYSTEMS GROUP
                     INTEROFFICE MEMORANDUM
 
Network Systems Company            Network Technology Development
Tymnet                                 Date:  24 August 1987
 
 
SUBJECT>    SOUTH BAY CENTER OCCUPANCY
 
TO>         NSC Personnel
 
FROM>       Secretaries/Network Technology Development Division
-----------------------------------------------------------------
Since we are already located at the new campus, we would really
appreciate an Ontyme (if possible) or memo from other divisions
as they move in.  One of the difficulties we've experienced is
sending mail out and not knowing if the recipient is already in
the complex, still at Valley Green, etc.
 
Please address the Ontyme to NTD.SUP and it will be forwarded to
all of us.
 
Thanks, and we look forward to having everyone here!!!!
Received: from F29.Tymnet by B39.Tymnet; Mon, 24 Aug 87 21:00:03 PDT
Return-path: <JMS@F29.Tymnet> 
From: JMS@F29.Tymnet 
Date: Mon, 24 Aug 87 21:01:28 PDT 
To: wrs@b39 
Subject: "I updated to not foward to X930."... 

I updated (MAIL:29)ONTYME.DAT to not foward to X930.
What is (MAIL:29)ONTYPE.DAT for?  I updated it also.  /Jms
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 25 Aug 87 3:00:41 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, In case of DONE interrupt"... 
In-reply-to: <8708250011.AA28105@calvin.Tymnet.com> of Mon, 24 Aug 87 17:11:38
	PDT

Yes. it will point to the message that is "done".
-Bill
From: Sandy McFarland <SandyMF@B39.Tymnet> 
Date: Tue, 25 Aug 87 9:30:29 PDT 
To: <WRS@B39.Tymnet> 
Subject: Re: emsbub 
In-reply-to: your message of Mon, 17 Aug 87 21:09:25 PDT

yes, it will
sl
Received: from tymix.Tymnet by B39.Tymnet; Tue, 25 Aug 87 13:50:26 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05570; Tue, 25 Aug 87 13:55:50 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA12637; Tue, 25 Aug 87 13:55:46 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 25 Aug 87 13:55:46 PDT 
To: WRS@B39.Tymnet 
Message-id: <8708252055.AA12637@f104.sun.com> 
Subject: Re: Logger background 

I guess so, that just seemed a little backwards to me.  Didn't know
how you were handling that.

Okee Dokee.
Ron
Received: from tymix.Tymnet by B39.Tymnet; Tue, 25 Aug 87 13:54:18 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05610; Tue, 25 Aug 87 13:59:45 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA12641; Tue, 25 Aug 87 13:59:39 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 25 Aug 87 13:59:39 PDT 
To: WRS@B39.Tymnet 
Message-id: <8708252059.AA12641@f104.sun.com> 
Subject: Re: Logger background 

Does the function to be sprouted always have to have the u_long mainArg
declared?
Received: from F29.Tymnet by B39.Tymnet; Tue, 25 Aug 87 16:18:34 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 25 Aug 87 16:19:22 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 25 AUG 87 15:55:56 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I76435@Ontyme.Tymnet 
Subject: LIBRARY PHONES 
LIBRARYPHONES: 

MSG#:I75797
 IN#:  2239
  TO: SUP
FROM: NTD      D/SANDS
SENT: 24 AUG 87 14:11:44
READ: 25 AUG 87 15:40:49
SUBJ: LIBRARY PHONES
 
THE LIBRARY FINALLY HAS A PHONE!!  DOROTHY'S NUMBER IS 922-6720,
MARY'S NUMBER IS 922-6229.  THANKS FOR YOUR PATIENCE.  DOROTHY
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 25 Aug 87 17:25:33 PDT 
To: <rmurphy@f104> 
Subject: Re: Logger background 
In-reply-to: <8708252059.AA12641@f104.sun.com> of Tue, 25 Aug 87 13:59:39 PDT

Not declaring the argument is a judgement call.  I think its okay.  After
all, there is precedent for it in main() which may be declared without.
You might ask someone else, like Steve, if you're still uncomfortable.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 25 Aug 87 17:27:32 PDT 
To: richteng 
Subject: data structure book 

I think I lent you an old textbook of mine on data structures back when you
were doing the garbage collect stuff on the sup.  Do you still have it?
Don't go crazy if you can't find it, I really don't remember what came of
it.  I believe it was Addison-Wessley Computer Science Series.  I don't
remember the author.  The title was simply Data Structures, I think.  -Bill
From: CARL@B39.Tymnet 
Date: Fri, 21 Aug 87 13:46:53 PDT 
To: wrs 
Subject: "Bill, Do you know why I get bared""... 

Bill,
  Do you know why I get "Access bared" when I try to build a circuit
from BUBBNET back into TYMNET?  It makes it a little difficult to do
anything on system 39.

  If I masque as WRS or something, it works fine...  if you could fix
the problem for me, I'd really appreciate it.  Thanks /Carl

PS.  We've played muciscal hosts in preparation for the move.  X930 is
still the same host, but right now it keeps crashing.  It would be best
to try sending mail to X14 or X17 if 930 is down, or maybe best to D35
during the move.   I will update my local MAIL.FWD to go to 35.

/carl
Received: from tymix.Tymnet by B39.Tymnet; Fri, 21 Aug 87 15:38:20 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09967; Fri, 21 Aug 87 15:43:48 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA11360; Fri, 21 Aug 87
	15:43:45 PDT
Return-path: <wrs@hobbes> 
From: wrs@hobbes (Bill Soley) 
Date: Fri, 21 Aug 87 15:43:45 PDT 
To: wrs@hobbes 
Message-id: <8708212243.AA11360@hobbes.Tymnet.com> 
Subject: bring TMS manual 

for rmurphy
Received: from tymix.Tymnet by B39.Tymnet; Tue, 25 Aug 87 18:41:19 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06549; Tue, 25 Aug 87 18:46:44 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15643;
	Tue, 25 Aug 87 18:46:35 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA07283; Tue, 25 Aug 87
	18:46:39 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 25 Aug 87 18:46:39 PDT 
To: wrs@calvin 
Message-id: <8708260146.AA07283@calvin.Tymnet.com> 
Subject: "Bill, a C question: struct A struct"... 

Bill,
a C question:

struct A {
	struct B b;
	struct C c;
	char d[50];
};

are the expressions below correct?

sizeof( A.b ) and
sizeof( A.d )

I suspect the first to be incorrect (the correct is sizeof( struct B ),
but how can I come up with a correct sizeof declaration for the
last element of structure A (regardless of the fact that I know
it is 50) ?

Mike
Received: from F29.Tymnet by B39.Tymnet; Wed, 26 Aug 87 10:58:03 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 26 Aug 87 10:49:37 PDT
Return-path: <NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet> 
From: NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet 
Date: 26 AUG 87 10:31:09 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I76773@Ontyme.Tymnet 
Subject: PAYCHECKS! 
PAYCHECKS!: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      26 AUG 87  10:30                  Network Technology Development

TO>        NTD PERSONNEL

COPIES>    

FROM>      JOY SARGENT


SUBJECT>   PAYCHECKS!


-----------------------------------------------------------------------


FOR YOUR INFORMATION, ON FRIDAY, 28 AUGUST, THE PAYCHECKS WILL BE
DELIVERED TO OUR BUILDING ABOUT 10:30AM.  PLEASE DO NOT PLAN ON RE-
CEIVING THEM ANY EARLIER THAN THIS.

THANKS!

JOY
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 26 Aug 87 12:58:31 PDT 
To: Richard Teng <richteng@B39.Tymnet> 
Subject: Re: data structures book 
In-reply-to: your message of Wed, 26 Aug 87 10:20:11 PDT

Oh well, thanks for checking, but I found another source (Knuth)
already.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 26 Aug 87 16:33:15 PDT 
To: rmurphy@f104 
Subject: TMS manual 

Its on my desk.  If I'm not around, help yourself.
Sorry it took so long.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Wed, 26 Aug 87 16:58:06 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 26 Aug 87 16:52:48 PDT
Return-path: <NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet> 
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 26 AUG 87 16:38:34 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.P/CHU@Ontyme.Tymnet,
	NTD.D/CHANG@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.L/VAYSBURD@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet,
	NTD.R/RAFFO@Ontyme.Tymnet, NTD.O/SERANG@Ontyme.Tymnet,
	NTD.H/RIVERS@Ontyme.Tymnet, NTD.N/BURRIESCI@Ontyme.Tymnet 
Message-id: I77095@Ontyme.Tymnet 
Subject: SWBT Project Status Meeting 

 
                      Tymnet/Network Technology Development
                          PROGRAM MANAGEMENT DEPARTMENT

 
                               M E M O R A N D U M 

 
                                                                []  ISG
 
DATE>      26 AUG 87  16:32

TO>        Gazel Tan
           Paul Chu
           David Chang
           Adele Newman
           Henry Rivers
           Romolo Raffo
           Nancy Holt

COPIES>    Ken Holcomb
           Distribution (NUMPLAN)

FROM>      Allyn Morriss


SUBJECT>   SWBT Project Status Meeting


-----------------------------------------------------------------------


There will be a SWBT project status meeting this Friday, 8/28 at 3:30PM
in Al Fenn's B4 Conference Room. Please be prepared to give schedule updates 
and to discuss plans, progress and problems. The meeting is to last one hour.
 
Please confirm your attendance.
 
Regards,
Allyn Morriss (408/922-6151)
Manager, NTD/Program Management
NTD.A/MORRISS
Received: from F29.Tymnet by B39.Tymnet; Wed, 26 Aug 87 16:58:06 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 26 Aug 87 16:52:48 PDT
Return-path: <NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet> 
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 26 AUG 87 16:38:34 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.P/CHU@Ontyme.Tymnet,
	NTD.D/CHANG@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.L/VAYSBURD@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet,
	NTD.R/RAFFO@Ontyme.Tymnet, NTD.O/SERANG@Ontyme.Tymnet,
	NTD.H/RIVERS@Ontyme.Tymnet, NTD.N/BURRIESCI@Ontyme.Tymnet 
Message-id: I77095@Ontyme.Tymnet 
Subject: SWBT Project Status Meeting 

 
                      Tymnet/Network Technology Development
                          PROGRAM MANAGEMENT DEPARTMENT

 
                               M E M O R A N D U M 

 
                                                                []  ISG
 
DATE>      26 AUG 87  16:32

TO>        Gazel Tan
           Paul Chu
           David Chang
           Adele Newman
           Henry Rivers
           Romolo Raffo
           Nancy Holt

COPIES>    Ken Holcomb
           Distribution (NUMPLAN)

FROM>      Allyn Morriss


SUBJECT>   SWBT Project Status Meeting


-----------------------------------------------------------------------


There will be a SWBT project status meeting this Friday, 8/28 at 3:30PM
in Al Fenn's B4 Conference Room. Please be prepared to give schedule updates 
and to discuss plans, progress and problems. The meeting is to last one hour.
 
Please confirm your attendance.
 
Regards,
Allyn Morriss (408/922-6151)
Manager, NTD/Program Management
NTD.A/MORRISS
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 27 Aug 87 4:11:20 PDT 
To: CARL@B39.Tymnet 
Subject: Re: "Bill, Do you know why I get bared""... 
In-reply-to: your message of Fri, 21 Aug 87 13:46:53 PDT

I fixed this.  Sorry for the delay, but NETVAL has been down every time
I checked lately.  -Bill

----------------------------------------------------------------
From: CARL@B39.Tymnet 
Date: Fri, 21 Aug 87 13:46:53 PDT 
To: wrs 
Subject: "Bill, Do you know why I get bared""... 

Bill,
  Do you know why I get "Access bared" when I try to build a circuit
from BUBBNET back into TYMNET?  It makes it a little difficult to do
anything on system 39.

  If I masque as WRS or something, it works fine...  if you could fix
the problem for me, I'd really appreciate it.  Thanks /Carl

PS.  We've played muciscal hosts in preparation for the move.  X930 is
still the same host, but right now it keeps crashing.  It would be best
to try sending mail to X14 or X17 if 930 is down, or maybe best to D35
during the move.   I will update my local MAIL.FWD to go to 35.

/carl
----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Wed, 26 Aug 87 16:58:06 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 26 Aug 87 16:52:48 PDT
Return-path: <NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet> 
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 26 AUG 87 16:38:34 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.P/CHU@Ontyme.Tymnet,
	NTD.D/CHANG@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.L/VAYSBURD@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet,
	NTD.R/RAFFO@Ontyme.Tymnet, NTD.O/SERANG@Ontyme.Tymnet,
	NTD.H/RIVERS@Ontyme.Tymnet, NTD.N/BURRIESCI@Ontyme.Tymnet 
Message-id: I77095@Ontyme.Tymnet 
Subject: SWBT Project Status Meeting 

 
                      Tymnet/Network Technology Development
                          PROGRAM MANAGEMENT DEPARTMENT

 
                               M E M O R A N D U M 

 
                                                                []  ISG
 
DATE>      26 AUG 87  16:32

TO>        Gazel Tan
           Paul Chu
           David Chang
           Adele Newman
           Henry Rivers
           Romolo Raffo
           Nancy Holt

COPIES>    Ken Holcomb
           Distribution (NUMPLAN)

FROM>      Allyn Morriss


SUBJECT>   SWBT Project Status Meeting


-----------------------------------------------------------------------


There will be a SWBT project status meeting this Friday, 8/28 at 3:30PM
in Al Fenn's B4 Conference Room. Please be prepared to give schedule updates 
and to discuss plans, progress and problems. The meeting is to last one hour.
 
Please confirm your attendance.
 
Regards,
Allyn Morriss (408/922-6151)
Manager, NTD/Program Management
NTD.A/MORRISS
Received: from F29.Tymnet by B39.Tymnet; Thu, 27 Aug 87 0:07:55 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Thu, 27 Aug 87 7:14:02 UT 
To: MAIL 
Subject: Req #12151 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #12151 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  27-Aug-87 00:00:00
; Actual run: 27-Aug-87 00:00:05
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from tymix.Tymnet by B39.Tymnet; Thu, 27 Aug 87 0:42:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11946; Thu, 27 Aug 87 00:47:24 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23408; Thu,
	27 Aug 87 00:47:13 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11939; Thu, 27 Aug 87 00:47:17 PDT
Received: by billp.unet.uucp (3.2/SMI-3.0DEV3SLAVE1) id AA10747; Wed, 26 Aug 87
	20:22:05 PDT
Return-path: <unet!billp@tymix.Tymnet> 
From: unet!billp (Bill W. Putney) 
Date: Wed, 26 Aug 87 20:22:05 PDT 
To: :wrs@tymix 
Message-id: <8708270322.AA10747@billp.unet.uucp> 
Subject: "tone generator driver XTAL MATRIX"... 

/*
 * tone generator driver
 */

#define XTAL	(14.31818e6)
#define MATRIX	(XTAL/7.0)
#define SAMPLE	(MATRIX/256.0)
#define	MAXBUF	((long) (SAMPLE))

char	sin[] = {
	0	/* sine table */
};

char	buf[MAXBUF];

int				/* size of composite waveform */
tg_compile(buf, f1, f2)
char buf[];
int f1, f2;
{
	register unsigned short i1, i2, t1, t2;
	register char *b;
	i1 = f1 ? (f1 << 8) / MAXBUF : 0;
	t1 = 0;
	i2 = f2 ? (f2 << 8) / MAXBUF : 0;
	t2 = 0;
	for (b=buf; b<buf+MAXBUF; t1+=i1, t2+=i2)
		*b++ = sin[t1>>8] + sin[t2>>8];
	return MAXBUF;
}

main()
{
	int i;
	printf("\007start\n");
	for (i=1000; i; --i)
		tg_compile(buf, 641, 1633);
	printf("\007stop\n");
}
Received: from tymix.Tymnet by B39.Tymnet; Thu, 27 Aug 87 0:53:37 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11963; Thu, 27 Aug 87 00:47:38 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23414; Thu,
	27 Aug 87 00:47:27 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11951; Thu, 27 Aug 87 00:47:30 PDT
Received: by billp.unet.uucp (3.2/SMI-3.0DEV3SLAVE1) id AA10909; Wed, 26 Aug 87
	23:45:56 PDT
Return-path: <unet!billp@tymix.Tymnet> 
From: unet!billp (Bill W. Putney) 
Date: Wed, 26 Aug 87 23:45:56 PDT 
To: :wrs@tymix 
Message-id: <8708270645.AA10909@billp.unet.uucp> 
Subject: test too... 

this is also a test.

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 27 Aug 87 0:54:02 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11956; Thu, 27 Aug 87 00:47:31 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23411; Thu,
	27 Aug 87 00:47:18 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11944; Thu, 27 Aug 87 00:47:23 PDT
Received: by billp.unet.uucp (3.2/SMI-3.0DEV3SLAVE1) id AA10892; Wed, 26 Aug 87
	23:28:22 PDT
Return-path: <unet!billp@tymix.Tymnet> 
From: unet!billp (Bill W. Putney) 
Date: Wed, 26 Aug 87 23:28:22 PDT 
To: :wrs@tymix 
Message-id: <8708270628.AA10892@billp.unet.uucp> 
Subject: test message 

this is a test message to see if we can send stuff through the x.25 conn.

-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 27 Aug 87 9:22:00 PDT 
To: <unet!billp@tymix.Tymnet> 
Subject: Re: test message 
In-reply-to: <8708270628.AA10892@billp.unet.uucp> of Wed, 26 Aug 87 23:28:22
	PDT

We got em!  But I notice strangness:
  1. they seem out of order
  2. look at the To: field, it has an extra ":"
These could be our problems, but I haven't seen either one before.

-Bill

----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Thu, 27 Aug 87 0:53:37 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11963; Thu, 27 Aug 87 00:47:38 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23414; Thu,
	27 Aug 87 00:47:27 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11951; Thu, 27 Aug 87 00:47:30 PDT
Received: by billp.unet.uucp (3.2/SMI-3.0DEV3SLAVE1) id AA10909; Wed, 26 Aug 87
	23:45:56 PDT
Return-path: <unet!billp@tymix.Tymnet> 
From: unet!billp (Bill W. Putney) 
Date: Wed, 26 Aug 87 23:45:56 PDT 
To: :wrs@tymix 
Message-id: <8708270645.AA10909@billp.unet.uucp> 
Subject: test too... 

this is also a test.

-Bill
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Thu, 27 Aug 87 0:54:02 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11956; Thu, 27 Aug 87 00:47:31 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23411; Thu,
	27 Aug 87 00:47:18 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11944; Thu, 27 Aug 87 00:47:23 PDT
Received: by billp.unet.uucp (3.2/SMI-3.0DEV3SLAVE1) id AA10892; Wed, 26 Aug 87
	23:28:22 PDT
Return-path: <unet!billp@tymix.Tymnet> 
From: unet!billp (Bill W. Putney) 
Date: Wed, 26 Aug 87 23:28:22 PDT 
To: :wrs@tymix 
Message-id: <8708270628.AA10892@billp.unet.uucp> 
Subject: test message 

this is a test message to see if we can send stuff through the x.25 conn.

-Bill
----------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 17 Aug 87 11:14:51 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: CARL@X930 
Subject: Re: "Confirming thurs, 9/3 meeting"... 
In-reply-to: your message of Mon, 17 Aug 87 8:37:07 PDT

OK for 1:30 Thursday.  I'll get a conference room.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 28 Aug 87 13:37:27 PDT 
To: <M108013@SLVMCL1.McAuto.Tymnet> 
Subject: Re: Test of multiple PROFS notes to Ontyme Users 
In-reply-to: your message of 08/28/87 15:05:02

Well so far I've only received one copy.
I believe the line you are refering to is filtered out (the on that
contains, among other things, the subject field, right?) so it is not
part of the checksum.  Below is a copy of the field that was checksumed
in the note you just sent me, for example:

--begin checksum--
From: Carol Block (M108013) L243 301-2W Ext. 33729
	<M108013@SLVMCL1.McAuto.Tymnet> 
Bill, I have a hunch that you will receive three notes from this test. The
check sum depending on how you do it may find a difference in the last line of
the note PROFS creates. It has a hex field that can be used for number of
copies of the note. It is not always consistant, so you cant use it to
eliminate duplicate notes, but it may be enough to fake out your newest mod to
identify copies. I thought of this after your last phone conversation and
thought I had better do a test and not leave it to the user that reported the
problem.
Have a good day,
Carol
--end checksum--
Received: from tymix.Tymnet by B39.Tymnet; Fri, 28 Aug 87 16:13:36 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04981; Fri, 28 Aug 87 16:19:06 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04806;
	Fri, 28 Aug 87 16:18:54 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA00383; Fri, 28 Aug 87
	16:19:03 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 28 Aug 87 16:19:03 PDT 
To: wrs@calvin 
Message-id: <8708282319.AA00383@calvin.Tymnet.com> 
Subject: "Bill, I only need your verification"... 

Bill,
I only need your verification that ipcfLookup( ABC, &hisTsap )
will put ABC's tsap into hisTsap ??
...
also in ipcfAssign( key, function,...) where function is to be a pointer
to the service routine - pointer in this instance is simple function's
name, right ?

Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 28 Aug 87 16:16:54 PDT 
To: <M108013@SLVMCL1.McAuto.Tymnet> 
Subject: Re: Success then 
In-reply-to: your message of 08/28/87 16:49:41

My messages are actually originating from B39.Tymnet and not Ontyme.  Ontyme
is only an intermediary (in some cases) so the data is just passed through
transparently.  Ontyme users can't do that except to put a seperate TUMS
header in the body of the message which is probably a great pain and not
worth it.  This message and the last one, although replies to your message
which was sent to my Ontyme name, were sent directly from system 39 through
TUMS without going through Ontyme.  I'm not being very clear, I hope this
makes sense.  Anyway glad to see it seems to be fixed.  Sorry if I dropped
the ball before, I had thought the problem had cleared up.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 28 Aug 87 16:50:49 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, I only need your verification"... 
In-reply-to: <8708282319.AA00383@calvin.Tymnet.com> of Fri, 28 Aug 87 16:19:03
	PDT

>  I only need your verification that ipcfLookup( ABC, &hisTsap )
>  will put ABC's tsap into hisTsap ??
	Either ABC must be a char array holding the name, or you
	meant to say
		ipcfLookup("ABC", &hisTsap)
	which will work.

>  also in ipcfAssign( key, function,...) where function is to be a pointer
>  to the service routine - pointer in this instance is simple function's
>  name, right ?
	You can say
		ipcfAssign(key, &function, ...)
	which might be more readible, but both are correct.  The "address of"
	operation is implied by simply referencing the function name so the
	"&" is optional.

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 10:15:36 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03888; Tue, 1 Sep 87 10:18:26 PDT
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18807;
	Tue, 1 Sep 87 10:18:22 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA06549; Tue, 1 Sep 87 10:18:20
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Tue, 1 Sep 87 10:18:20 PDT 
To: julies@hobbes, wrs@hobbes 
Message-id: <8709011718.AA06549@comet.Tymnet.com> 
Subject: Re: su, .login 

I have changed /etc/group on calvin, opus, and the workstations so
they get everything from the yellow pages (except some cmf stuff on calvin),
so the wheel group and the others only have to be updated on hobbes.

	steve
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 11:25:02 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04048; Tue, 1 Sep 87 11:27:49 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA19631;
	Tue, 1 Sep 87 11:27:46 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04672; Tue, 1 Sep 87 11:27:45
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 1 Sep 87 11:27:45 PDT 
To: wrs@calvin 
Message-id: <8709011827.AA04672@calvin.Tymnet.com> 
Subject: "Bill, while coding the boot I"... 

Bill,
while coding the boot I assumed in case of dumping the core that 
somehow I will know where the core starts and ends. have you got
any idea how those two values will be given to me?
also...in the attributes for dumpFile I set it to be read-only (??),
and start address to zero (what else?), type normal. how does it
sound?
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 11:29:54 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04076; Tue, 1 Sep 87 11:32:42 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA19753;
	Tue, 1 Sep 87 11:32:38 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04810; Tue, 1 Sep 87 11:32:36
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 1 Sep 87 11:32:36 PDT 
To: wrs@calvin 
Message-id: <8709011832.AA04810@calvin.Tymnet.com> 
Subject: "Bill, what could be a good value"... 

Bill, 
what could be a good value for timeout in ipcfSend() ?
how about 20000000 microsecs ?
Mike J.
Received: from F29.Tymnet by B39.Tymnet; Tue, 1 Sep 87 12:22:06 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Tue, 1 Sep 87 12:23:04 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Error response(s) from SLVMCL1.McAuto.Tymnet ...
SNDMSG000R Transmit message, end with '.':
SNDMSG001R Transmit recipients, end with '.':
DMKCSP053E 2336696  NOT IN CP DIRECTORY
SNDMSG003W Send to 2336696 at SLVMCL1 failed due to error 53
SNDMSG004W Send failed to 1 recipients.
SNDMSG005E Note sent to 0 recipients.
R; T=0.07/0.24 14:23:18



----------
RCPT To: <2336696@SLVMCL1.McAuto.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Tue, 1 Sep 87 12:22:36 PDT 
To: melany <2336696@SLVMCL1.McAuto.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No domain-style [RFC-819] address found;
unable to forward message.  Address expected
in extended name field or 1st line of body.

----------
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Tue, 1 Sep 87 12:22:32 PDT
From: melany <2336696@SLVMCL1.McAuto.Tymnet> 
Date: 01SEP87 1411PM 
To: M100826@SLCM307.MCAUTO.TYMNET <M100826@SLCM307.McAuto.Tymnet> 
RECEIVED: from TMS.Tymnet by F29.Tymnet; Tue, 1 Sep 87 12:18:18 PDT
MESSAGE-ID: 035168@TMS.Tymnet
SUBJ: just a small thank you.

i enjoyed the poem thank you very much.
i'm so glad this profs worked but obviously you smile more than
i do.
 
melany
have a nice day
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 13:51:35 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04262; Tue, 1 Sep 87 13:09:57 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA20428;
	Tue, 1 Sep 87 13:09:54 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA05975; Tue, 1 Sep 87 13:09:51
	PDT
Return-path: <bkrueger@calvin> 
From: bkrueger@calvin (Bodo Krueger) 
Date: Tue, 1 Sep 87 13:09:51 PDT 
To: rodney@calvin 
Message-id: <8709012009.AA05975@calvin.Tymnet.com> 
Subject: "Rodney lunch Lunch will be provided"... 

Rodney lunch

Lunch will be provided tomorrow in the large conference room on
the third floor (conference room "A") for an informal technical
review of Rodney.  Bill Euske will also be attending.  I will
be introducing some new team members and announcing some changes
to the team assignments.  We will NOT be discussing shedules.
Please plan on attending.

Thanks,

Pat
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 13:52:01 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04270; Tue, 1 Sep 87 13:16:55 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA20479;
	Tue, 1 Sep 87 13:16:50 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA06059; Tue, 1 Sep 87 13:16:44
	PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Tue, 1 Sep 87 13:16:44 PDT 
To: rodney@calvin 
Message-id: <8709012016.AA06059@calvin.Tymnet.com> 
Subject: who is Bodo Krueger 

I was logged in under Bodo's name when I sent the message
regarding the Rodney lunch.

Pat
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 1 Sep 87 15:25:57 PDT 
To: <alexting@calvin> 
Subject: Re: UNIX doc tools meeting 
In-reply-to: <8708312349.AA09012@calvin.Tymnet.com> of Mon, 31 Aug 87 16:49:16
	PDT

I plan to attend.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 1 Sep 87 15:39:35 PDT 
To: <feldman@comet> 
Subject: Re: su, .login 
In-reply-to: <8709011718.AA06549@comet.Tymnet.com> of Tue, 1 Sep 87 10:18:20
	PDT

Does that require the network to be functional?  (YP that is)  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 1 Sep 87 15:40:48 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, while coding the boot I"... 
In-reply-to: <8709011827.AA04672@calvin.Tymnet.com> of Tue, 1 Sep 87 11:27:45
	PDT

I'd dump all of "core".  I don't think dumps need to be read-only.
And they should have no start address as opposed to a zero start
address.  If the file format doesn't allow that, some special value
that is clearly illegal should be used, such as -1.  I guess that you
still need somebody to tell you where "all of core" is so Rodney can
give you a couple of variables somewhere, don't know what they'll
be called, but they would be either start and end address or start
address and size, or maybe all three.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 15:47:53 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04779; Tue, 1 Sep 87 15:52:27 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA21974;
	Tue, 1 Sep 87 15:52:23 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA10542; Tue, 1 Sep 87 15:52:22
	PDT
Return-path: <tsay@opus> 
From: tsay@opus (Hai-Fang Tsay) 
Date: Tue, 1 Sep 87 15:52:22 PDT 
To: wrs@opus 
Message-id: <8709012252.AA10542@opus.Tymnet.com> 
Subject: "The coding of malloc.c is ready"... 

The coding of malloc.c is ready for walk-through. 
Would you let me know when you will be available ?
Thanks.

Hai-Fang

From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 1 Sep 87 15:45:02 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, what could be a good value"... 
In-reply-to: <8709011832.AA04810@calvin.Tymnet.com> of Tue, 1 Sep 87 11:32:36
	PDT

By time out send, if you mean DONE, that should be quick, even 5 secs
would be long enough.  If you mean the reply from the file server,
20 seconds is probably good.  It should be easily changable and in
fact during integration, maybe we'll change all the modules to use
the same symbol for timeout.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 16:05:47 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04886; Tue, 1 Sep 87 16:10:23 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA07112; Tue, 1 Sep 87 16:10:19
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Tue, 1 Sep 87 16:10:19 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8709012310.AA07112@comet.Tymnet.com> 
Subject: Re: su, .login 

yes, yp requires the network to work.
	steve
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 1 Sep 87 16:09:40 PDT 
To: <tsay@opus> 
Subject: Re: "The coding of malloc.c is ready"... 
In-reply-to: <8709012252.AA10542@opus.Tymnet.com> of Tue, 1 Sep 87 15:52:22 PDT

How about tomorrow, Wed at about 1:30 in my office?  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 16:31:48 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05030; Tue, 1 Sep 87 16:36:19 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA07141; Tue, 1 Sep 87 16:36:16
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Tue, 1 Sep 87 16:36:16 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8709012336.AA07141@comet.Tymnet.com> 
Subject: Re: su, .login 

right.  hobbes is the master server, calvin and opus are slave servers.
when a client comes up it broadcasts asking for a yp server.  the first
one to answer wins (on the grounds that it is the least heavily loaded).
that server is used until there's no answer from it, in which case the
process starts over.  and you can be a server for yourself.  so unless
the network is totally dead or all the servers are down, yp will work.
	steve
Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 16:57:30 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05199; Tue, 1 Sep 87 17:02:07 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA11435; Tue, 1 Sep 87 17:02:04
	PDT
Return-path: <tsay@opus> 
From: tsay@opus (Hai-Fang Tsay) 
Date: Tue, 1 Sep 87 17:02:04 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8709020002.AA11435@opus.Tymnet.com> 
Subject: Re: "The coding of malloc.c is ready"... 

1:30 Wed. sounds good. See you then.    Hai_Fang
From: CARL@B39.Tymnet 
Date: Tue, 1 Sep 87 18:15:48 PDT 
To: wrs 
Subject: "?Access barred" 

From B39, I'm still getting barred.  Does it have something to do with the
wrong supervisor being up... each having a different database?

/Carl
Received: from F29.Tymnet by B39.Tymnet; Wed, 2 Sep 87 0:19:15 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Wed, 2 Sep 87 7:22:56 UT 
To: MAIL 
Subject: Req #12581 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #12581 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  02-Sep-87 00:00:00
; Actual run: 02-Sep-87 00:00:03
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from tymix.Tymnet by B39.Tymnet; Wed, 2 Sep 87 10:43:49 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07841; Wed, 2 Sep 87 10:48:22 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27502;
	Wed, 2 Sep 87 10:48:19 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA12876; Wed, 2 Sep 87 10:48:17
	PDT
Return-path: <dfields@opus> 
From: dfields@opus (David Fields) 
Date: Wed, 2 Sep 87 10:48:17 PDT 
To: wrs@opus 
Cc: dfields@opus 
Message-id: <8709021748.AA12876@opus.Tymnet.com> 
Subject: Connecting Ontyme to UNIX mail 

Julie Stier suggested you might know how to route Ontyme mail to the
UNIX mail system. Can you please tell me how to set that up?  If not,
do you know someone else who can?

Many thanks...
Dave Fields
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 2 Sep 87 10:46:10 PDT 
To: <dfields@opus> 
Subject: Re: Connecting Ontyme to UNIX mail 
In-reply-to: <8709021748.AA12876@opus.Tymnet.com> of Wed, 2 Sep 87 10:48:17 PDT

1. make sure your Ontyme password is different from any of your other
passwords (change it if necessary)
2. give me your Ontyme name, password, and the system you want it sent
to and the login name you want it sent to
3. sit back and enjoy TUMS
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 2 Sep 87 10:54:15 PDT 
To: CARL@B39.Tymnet 
Subject: Re: "?Access barred" 
In-reply-to: your message of Tue, 1 Sep 87 18:15:48 PDT

Well, I logged in to NETVAL and lo and behold, somebody had restored the
NETVAL disk to before the change.  They've been having alot of disk
trouble since the move.  Anyway, I made the change again so...here is
your current entry and all SUPs have been updated so it should work.
-Bill

NETVAL - Version 3.10 at 09/02/87 17:54:53

* disp user carl
GAN: 3
UUN: 352477
shut override
district: 3
entry changed: 09/02/87 17:51:06
password changed: 03/14/86
(1) Origin     : class: 0
    Destination:
         groups: 0
          hosts: 39 (home)

* disp update
Current time is 09/02/87 17:55:05 GMT
Blocks in CUD: 2039     MUD update EOF: 0 (0% full)
Class/group table last created: 08/31/87 18:36:28
Scheduled consistency period: 04:00 to 08:00

Sup                    MUD          CLASS/GROUP
          Ptr        Updated          Updated
121           0     09/02 17:53      08/31 18:37
123           0     09/02 17:53      08/31 21:36
124           0     09/02 17:53      08/31 18:37
127           0     09/02 17:53      08/31 19:08

Sup                   CONSISTENCY
     Last Start     Last Stop   End Block         Current Start   Block
121  09/02 04:00   09/02 04:12  complete            
123  09/02 04:00   09/02 04:11  complete            
124  09/02 04:00   09/02 04:08  complete            
127  09/02 04:00   09/02 04:11  complete            

* 
Received: from F29.Tymnet by B39.Tymnet; Wed, 8 Jul 87 14:56:14 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Wed, 8 Jul 87 22:00:25 UT 
To: MAIL 
Subject: Req #8390 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #8390 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  08-Jul-87 14:22:19
; Actual run: 08-Jul-87 14:22:21
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Fri, 10 Jul 87 0:32:52 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Fri, 10 Jul 87 7:37:35 UT 
To: MAIL 
Subject: Req #8720 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #8720 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  10-Jul-87 00:00:00
; Actual run: 10-Jul-87 00:00:04
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 0:10:29 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Mon, 13 Jul 87 7:12:30 UT 
To: MAIL 
Subject: Req #8824 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #8824 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  13-Jul-87 00:00:00
; Actual run: 13-Jul-87 00:00:05
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Sun, 19 Jul 87 0:07:12 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Sun, 19 Jul 87 7:11:48 UT 
To: MAIL 
Subject: Req #9269 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #9269 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  19-Jul-87 00:00:00
; Actual run: 19-Jul-87 00:00:02
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Wed, 19 Aug 87 20:53:12 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Thu, 20 Aug 87 3:58:59 UT 
To: MAIL 
Subject: Req #11744 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #11744 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  19-Aug-87 20:44:30
; Actual run: 19-Aug-87 20:44:34
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from tymix.Tymnet by B39.Tymnet; Mon, 31 Aug 87 15:40:16 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01116; Sun, 30 Aug 87 15:39:46 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13030; Mon, 31 Aug 87
	15:39:28 PDT
Return-path: <wrs@hobbes> 
From: wrs@hobbes (Bill Soley) 
Date: Mon, 31 Aug 87 15:39:28 PDT 
To: wrs@hobbes 
Message-id: <8708312239.AA13030@hobbes.Tymnet.com> 
Subject: prog util man 

Regina Chen
Received: from F29.Tymnet by B39.Tymnet; Wed, 2 Sep 87 12:17:50 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 2 Sep 87 12:07:01 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 02 SEP 87 11:35:03 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I80175@Ontyme.Tymnet 
Subject: D-27 User Move 

   Date:  August 31, 1987
     
     To:  All INTERNAL D-27 User's 
     
   From:  Resource Planning & Management
          Jan Krivanec   
     
Subject:  D-27 User Move 
     
 Reason:  System D-27 Shutdown
     
-------------------------------------------------------------------------  
     
     
System D-27 will be permanently shutdown on September 31, 1987. RPM   
has chosen to route all D-27 User's to System D-34 by September 26, 1987.  
     
If you are valid on D-27 and D-34 you are requested to move your own  
files and tapes to D-34. You are to be completed by September 26, 1987.    
     
If you are valid only on D-27, RPM and Operations will move your files
and tapes to D-34. This request will begin September 25, 1987 beginning    
at 1800 pst.   
     
If you have any questions, please call me at (415) 498-2556 or ontyme 
IPC.J/KRIVANEC.


Thank you,
Jan Krivanec
Received: from tymix.Tymnet by B39.Tymnet; Wed, 2 Sep 87 15:43:01 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA08914; Wed, 2 Sep 87 15:47:35 PDT
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00212;
	Wed, 2 Sep 87 15:47:31 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA08257; Wed, 2 Sep 87 15:47:28
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Wed, 2 Sep 87 15:47:28 PDT 
To: foil@comet, julies@comet 
Cc: alexting@comet, wrs@comet 
Message-id: <8709022247.AA08257@comet.Tymnet.com> 
Subject: syslog now running 

I have set up syslog so that all log messages go to hobbes.  This means
that some error messages which formerly used to appear on various
system consoles now all go to the console on hobbes, and are recorded
(along with lots more stuff) in /usr/spool/log/syslog on hobbes.

(All I had to do to set this up was create an alias for hobbes called
"loghost" in /etc/hosts.)
	steve
Received: from tymix.Tymnet by B39.Tymnet; Wed, 2 Sep 87 16:52:08 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09135; Wed, 2 Sep 87 16:56:46 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00734; Wed,
	2 Sep 87 16:56:38 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA01032; Wed, 2 Sep 87 16:55:35 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 2 Sep 87 16:55:35 PDT 
To: wrs@f104 
Message-id: <8709022355.AA01032@f104.sun.com> 
Subject: Makefile 

are you still editing the rid Makefile? I'd like to make a change therein.

Received: from tymix.Tymnet by B39.Tymnet; Thu, 3 Sep 87 14:47:48 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12351; Thu, 3 Sep 87 14:52:30 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05423; Thu,
	3 Sep 87 14:52:24 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA01855; Thu, 3 Sep 87 14:51:19 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 3 Sep 87 14:51:19 PDT 
To: wrs@f104 
Message-id: <8709032151.AA01855@f104.sun.com> 
Subject: k_simpleDone 

Documentation for k_simpleDone never found its way into v1/kernel/mem/crash.
I have assumed that this procedure has the same arguments as ipcfDone, i.e.
a handle and progress code.
Ron


Received: from tymix.Tymnet by B39.Tymnet; Thu, 3 Sep 87 16:34:48 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12625; Thu, 3 Sep 87 16:39:32 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06410; Thu,
	3 Sep 87 16:39:29 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA02219; Thu, 3 Sep 87 16:38:28 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Thu, 3 Sep 87 16:38:28 PDT 
To: wrs@hobbes 
Message-id: <8709032338.AA02219@f104.sun.com> 
Subject: Re: Makefile 

Thanks, I already pulled the rug out from under you and added the x409
rule into it.  If you saw that in the sccs diffs Makefile output, that's
why.
Ron
Received: from tymix.Tymnet by B39.Tymnet; Thu, 3 Sep 87 18:21:06 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12753; Thu, 3 Sep 87 18:25:49 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07360;
	Thu, 3 Sep 87 18:25:47 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA28606; Thu, 3 Sep 87 18:25:45
	PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Thu, 3 Sep 87 18:25:45 PDT 
To: wrs@calvin 
Message-id: <8709040125.AA28606@calvin.Tymnet.com> 
Subject: Man page for dfx2ps 

Bill,

  Man page for dfx2ps seems to have disappeared.

Pat
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 4 Sep 87 8:51:24 PDT 
To: qsats.d/ellenberg@ontyme.tymnet 
Cc: WRS@C39.Tymnet, carl@b39 
Subject: "note following message from Carl"... 

note following message from Carl - looks o.k. to me - any comments,
concerns????
----------------------------------------------------------------
From: CARL@B39.Tymnet 
Date: Thu, 3 Sep 87 23:07:50 PDT 
To: wrs, bartletw 
Subject: NAD %1.07 

I've looked at the code for NAD and it appears that listings are currently
available to anyone running (SOURCE)NAD or anyone having TYMSHARE license.

Actually if the FPN of NAD matches the PPN of the input file, (or TP license)
then a listing can be generated.  What this means is that the current scheme
will allow anyone running NAD to get listings of any file in SOURCE since the
only version of NAD that is really NAD will be in SOURCE.  Files in BETATEST
are not listable unless you have TP license.

I know where to make the changes... as soon as Dennis Ernst confirms which
source file is the correct file then I will make the changes we discussed.

I feel the following "other" changes should be made...
  1)  Listings can only be generated if you have TP license (or)
      if the input file is in your own directory (or)
      if the input file is in your own GAN [since these are unreadable if
                                            the file is protected, so all ok]
  2)  As per (1) don't care what FPN NAD is run from -- remove check.

...otherwise having everyone run (SOURCE)NAD will allow everyone to get
listings whether or not they have TP license.

To Dennis:  Any reason why your current changes cannot released to the
            entire world?  Or should I change (NAD)NAD01.R05 as well as
            your version [NAD01.R06 or NAD01.R07?]

To Wayne:  Please pass this on to Doug so he knows about the listing 'twist'.

/Carl
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Fri, 4 Sep 87 15:08:08 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA16049; Fri, 4 Sep 87 15:09:22 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA11023;
	Fri, 4 Sep 87 15:09:20 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04937; Fri, 4 Sep 87 15:09:17
	PDT
Return-path: <stimac@calvin> 
From: stimac@calvin (Michael Stimac) 
Date: Fri, 4 Sep 87 15:09:17 PDT 
To: wrs@calvin 
Message-id: <8709042209.AA04937@calvin.Tymnet.com> 
Subject: One of the most frequently 

misspelled words is "misspell".

:-)

michael
Received: from tymix.Tymnet by B39.Tymnet; Fri, 4 Sep 87 17:08:32 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA16220; Fri, 4 Sep 87 17:09:49 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12366; Fri,
	4 Sep 87 17:09:46 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA16216; Fri, 4 Sep 87 17:09:43 PDT
Received: by billp.unet.uucp (3.2/SMI-3.0DEV3SLAVE1) id AA11728; Fri, 4 Sep 87
	17:07:37 PDT
Return-path: <unet!billp@tymix.Tymnet> 
From: unet!billp (Bill W. Putney) 
Date: Fri, 4 Sep 87 17:07:37 PDT 
To: :wrs@tymix 
Message-id: <8709050007.AA11728@billp.unet.uucp> 
Subject: Well? ... 

Tonight?  I guess you talked to Becky about comming over tonight.

Well? ...

-Bill
Received: from F29.Tymnet by B39.Tymnet; Sat, 5 Sep 87 1:02:49 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Sat, 5 Sep 87 8:03:27 UT 
To: MAIL 
Subject: Req #12788 DSK:(MAIL)RDIST.CTL Done-End -Normal- 

; Processed:  Req #12788 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  05-Sep-87 00:00:00
; Actual run: 05-Sep-87 00:00:03
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Done-End   -Normal-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Tue, 8 Sep 87 2:51:34 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Tue, 8 Sep 87 9:55:57 UT 
To: MAIL 
Subject: Req #13040 DSK:(MAIL)RDIST.CTL Abort-Notice (0) -System Reset- 

; Processed:  Req #13040 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  01-Jan-64 00:00:00
; Actual run: 08-Sep-87 00:00:04
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Abort-Notice (0)   -System Reset-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Tue, 8 Sep 87 2:51:35 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Tue, 8 Sep 87 9:55:57 UT 
To: MAIL 
Subject: Req #13040 DSK:(MAIL)RDIST.CTL Abort-Notice (0) -System Reset- 

; Processed:  Req #13040 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  01-Jan-64 00:00:00
; Actual run: 08-Sep-87 00:00:04
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Abort-Notice (0)   -System Reset-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Tue, 8 Sep 87 3:40:11 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Tue, 8 Sep 87 10:44:25 UT 
To: MAIL 
Subject: Req #13040 DSK:(MAIL)RDIST.CTL Abort-Notice (0) -System Reset- 

; Processed:  Req #13040 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  01-Jan-64 00:00:00
; Actual run: 08-Sep-87 00:00:04
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Abort-Notice (0)   -System Reset-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Tue, 8 Sep 87 3:40:12 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Tue, 8 Sep 87 10:44:25 UT 
To: MAIL 
Subject: Req #13040 DSK:(MAIL)RDIST.CTL Abort-Notice (0) -System Reset- 

; Processed:  Req #13040 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  01-Jan-64 00:00:00
; Actual run: 08-Sep-87 00:00:04
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Abort-Notice (0)   -System Reset-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Tue, 8 Sep 87 4:56:40 PDT
Return-path: <PERPOPER@F29.Tymnet> 
Reply-to: SUBMIT 
From: F29 Job Processor 
Sender: PERPOPER@F29.Tymnet 
Date: Tue, 8 Sep 87 12:00:55 UT 
To: MAIL 
Subject: Req #13040 DSK:(MAIL)RDIST.CTL Abort-Notice (0) -System Reset- 

; Processed:  Req #13040 DSK:(MAIL)RDIST.OUT=DSK:(MAIL)RDIST.CTL
; Scheduled:  01-Jan-64 00:00:00
; Actual run: 08-Sep-87 00:00:04
;
; Options:  Log:Supersede Mail:Always Restart:Start Unique:Job
; Limits:  Time 0  TRUs 0
; Flags: 
; Status:  Abort-Notice (0)   -System Reset-
;
; Output recorded in file DSK:(MAIL)RDIST.OUT.

Received: from F29.Tymnet by B39.Tymnet; Tue, 8 Sep 87 12:19:25 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 8 Sep 87 12:20:05 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 08 SEP 87 12:14:36 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I82381@Ontyme.Tymnet 
Subject: TELLAGRAF & DISSPLA USAGE 

                      R E M O T E   C O M P U T I N G
                              S E R V I C E S


                                                                  [MDC-ISG]

DATE>      04 SEP 87  10:18

TO>        TELLAGRAF AND DISSPLA USERS

COPIES>    

FROM>      CHERYL SCHREODER
           PROCESSING SERVICES


SUBJECT>   TELLAGRAF & DISSPLA USAGE


-----------------------------------------------------------------------


BECAUSE OF THE RESPONSE WE RECEIVED FROM THE MEMO REGARDING
CANCELLATION OF TELLAGRAF AND DISSPLA, WE HAVE DECIDED NOT TO 
CANCEL THE PRODUCTS .

WE WILL CONTINUE TO MAINTAIN THE TELLAGRAF AND DISSPLA
PRODUCTS TILL USAGE IS SUCH THAT WE CAN NO LONGER JUSTIFY 
KEEPING THE PRODUCTS.


ANY QUESTIONS CAN BE DIRECTED TO ME AT RCS.C/SCHROEDER.
Received: from tymix.Tymnet by B39.Tymnet; Tue, 8 Sep 87 14:00:40 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01105; Tue, 8 Sep 87 14:05:23 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18478;
	Tue, 8 Sep 87 14:05:17 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA16640; Tue, 8 Sep 87 14:05:15
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 8 Sep 87 14:05:15 PDT 
To: wrs@calvin 
Message-id: <8709082105.AA16640@calvin.Tymnet.com> 
Subject: "Bill, a question: afunction(); "... 

Bill,
a "C" question:

afunction();
{
  int(??) abc;

........
  bfuncion();
  return;
}

how can I define int abc to be a local function inside afunction()
so bfunction() could also access it? is there a way at all? the point
is I don't want abc to be an external variable.
thanks,
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 8 Sep 87 15:54:51 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, a question: afunction(); "... 
In-reply-to: <8709082105.AA16640@calvin.Tymnet.com> of Tue, 8 Sep 87 14:05:15
	PDT

The function has to be declared in the same lexical scope as the usage or
in a surrounding lexical scope.  The dynamic (run time) scope is not
important to declarations.  You didn't give me enough information to answer
the question, but I'll take a guess

void
a()
{
	int c();
	... c() ... /* function c referenced here */
}

void
b()
{
	... a() ... /* function a referenced here */
}

In this example, c() does not need declaration inside b() since it is only
called inside a().  It does not matter in tis respect that b() calls a().
You may also say
	extern int c();
if the function is declared outside the module, but it is unnecessary and
a matter of style.  I usually do. 

By the way, this is the same way you should deal with forward references
that are not of type int.

-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 9 Sep 87 11:20:36 PDT 
To: <julies@hobbes> 
Cc: microtec@hobbes 
Subject: Re: Clock Wrapping and MCC68k 
In-reply-to: <8709091808.AA23665@hobbes.Tymnet.com> of Wed, 9 Sep 87 11:08:55
	PDT

The problem mentioned in Dave's memo is the same one I recently reported
as a Microtec bug.  The problem also occurs on the SUN C compiler.  Since
it is clearly a compiler bug and not a shortcomming of either the 68000
or of C, we should expect a fix [Julie: have you gotten any feedback on
it yet?].  I suggest that it be "worked around" in such a way as to be
easily understood and possibly later removed when the bug is fixed.  I've
found the following to work on both the SUN and MICROTEC compilers without
the need for any assembler code:
	int	CompareKludge;
	if ((CompareKludge = a - b) < 0) ...;

	#define SignBit 0x8000
	if ((a - b) & SignBit) ...;

The later is probably less obscure.  Its the one I use.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 11:44:13 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02838; Wed, 9 Sep 87 11:45:08 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23902;
	Wed, 9 Sep 87 11:45:05 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA24663; Wed, 9 Sep 87 11:45:04
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Wed, 9 Sep 87 11:45:04 PDT 
To: wrs@calvin 
Message-id: <8709091845.AA24663@calvin.Tymnet.com> 
Subject: "Bill, thanks for last answer and"... 

Bill,
thanks for last answer and I have another C question:

Can something like this be made a macro?:

if(test > 0) return(quit);
else goto LABEL;

and even if possible that LABEL be an argument to the macro.
all books on C describe only one kind of macros, I have not seen
even an example with a macro spanning over more than one line.

regards,
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 11:46:53 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02853; Wed, 9 Sep 87 11:47:51 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23927;
	Wed, 9 Sep 87 11:47:47 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA24671; Wed, 9 Sep 87 11:47:45
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Wed, 9 Sep 87 11:47:45 PDT 
To: wrs@calvin 
Message-id: <8709091847.AA24671@calvin.Tymnet.com> 
Subject: "a small comment to my last question:"... 

a small comment to my last question:
I have those lines of code repeating like 20 times throughout
the code and I can not make it a function for obvious reasons.
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 11:59:46 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02890; Wed, 9 Sep 87 12:00:49 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA24025; Wed,
	9 Sep 87 12:00:45 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA05833; Wed, 9 Sep 87 11:59:51 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 9 Sep 87 11:59:51 PDT 
To: wrs@f104 
Message-id: <8709091859.AA05833@f104.sun.com> 
Subject: background problem? 

You had suggested managing my background on/off facility with a special
event that could be k_caused from dbx.  When I call k_cause on this
event (or any other) when it does NOT have a pending k_wait, I get a
segmentation fault in k_cause at the line "while (pid = deq(&eventp->q)))".
Apparently eventp->q is not at a valid address but is at something like
0x7b.  When the event is k_waiting, it works ok.
Well?
Ron


Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 12:03:14 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02903; Wed, 9 Sep 87 12:04:20 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA24079; Wed,
	9 Sep 87 12:03:57 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02898; Wed, 9 Sep 87 12:03:48 PDT
Return-path: <WRS@B39.Tymnet.com> 
From: William R. Soley <WRS@B39.Tymnet.com> 
Date: Wed, 9 Sep 87 11:20:36 PDT 
To: <julies@hobbes> 
Cc: microtec@hobbes 
Message-id: <8709091903.AA02898@tymix.Tymnet> 
Subject: Re: Clock Wrapping and MCC68k 
In-Reply-To: <8709091808.AA23665@hobbes.Tymnet.com> of Wed, 9 Sep 87 11:08:55
	PDT

The problem mentioned in Dave's memo is the same one I recently reported
as a Microtec bug.  The problem also occurs on the SUN C compiler.  Since
it is clearly a compiler bug and not a shortcomming of either the 68000
or of C, we should expect a fix [Julie: have you gotten any feedback on
it yet?].  I suggest that it be "worked around" in such a way as to be
easily understood and possibly later removed when the bug is fixed.  I've
found the following to work on both the SUN and MICROTEC compilers without
the need for any assembler code:
	int	CompareKludge;
	if ((CompareKludge = a - b) < 0) ...;

	#define SignBit 0x8000
	if ((a - b) & SignBit) ...;

The later is probably less obscure.  Its the one I use.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 9 Sep 87 14:00:25 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, thanks for last answer and"... 
In-reply-to: <8709091845.AA24663@calvin.Tymnet.com> of Wed, 9 Sep 87 11:45:04
	PDT

Define it on one line...
#define THEMACRO(LABEL) if(test>0)return(quit);else goto LABEL;

There is a way to put new lines in macros (end the line with a
backslash), but there is no need to do it here and it makes it
harder to read.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 9 Sep 87 14:03:55 PDT 
To: <rmurphy@f104> 
Subject: Re: background problem? 
In-reply-to: <8709091859.AA05833@f104.sun.com> of Wed, 9 Sep 87 11:59:51 PDT

Do you initialize the event data structure to zero?  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 14:20:54 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03440; Wed, 9 Sep 87 14:22:07 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA25575;
	Wed, 9 Sep 87 14:22:03 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA26485; Wed, 9 Sep 87 14:22:00
	PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Wed, 9 Sep 87 14:22:00 PDT 
To: wrs@calvin 
Message-id: <8709092122.AA26485@calvin.Tymnet.com> 
Subject: "Bill, thanks for the answer, no...if"... 

Bill,
thanks for the answer, no...if use this macro....
THEMACRO(XX);  - is semicolon good or bad ?
your definition of THEMACRO has already the ; at the end.
mike
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 14:27:50 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03456; Wed, 9 Sep 87 14:29:01 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA05896; Wed, 9 Sep 87 14:28:07 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 9 Sep 87 14:28:07 PDT 
To: WRS@B39.Tymnet 
Message-id: <8709092128.AA05896@f104.sun.com> 
Subject: Re: background problem? 

Before the first k_cause, yes.  (Not between k_causes.)

Apparently the eventp itself is a bad data address.  Either eventp was
garbaged (probably unlikely) or the argument allocated by the k_call
from dbx was not in an area accessible to the program, maybe?
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 9 Sep 87 14:28:41 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, thanks for the answer, no...if"... 
In-reply-to: <8709092122.AA26485@calvin.Tymnet.com> of Wed, 9 Sep 87 14:22:00
	PDT

You're right its better to leave the semicolon off the end of the definition
and include when you call the macro.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 14:46:41 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03495; Wed, 9 Sep 87 14:47:54 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA26297; Wed, 9 Sep 87 14:47:52
	PDT
Return-path: <tfok@hobbes> 
From: tfok@hobbes (Thomas Fok) 
Date: Wed, 9 Sep 87 14:47:52 PDT 
To: wrs@hobbes 
Message-id: <8709092147.AA26297@hobbes.Tymnet.com> 
Subject: MMU for RODNEY & ISIS 

Mark Akselrod wants to know when will there be a meeting about the change 
of the memory/io addressing scheme.
Please send Mark a mail. He suggestes that Friday morning is a good time for
him. Thanks......
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 16:35:15 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03765; Wed, 9 Sep 87 16:36:25 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27817; Wed,
	9 Sep 87 16:36:22 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA05952; Wed, 9 Sep 87 16:35:30 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 9 Sep 87 16:35:30 PDT 
To: wrs@f104 
Message-id: <8709092335.AA05952@f104.sun.com> 
Subject: Wonders of dbx 

There seems to be a bug in dbx in which, if it catches an interrupt
signal for the program, it forgets how to pass parameters in the 
'call' command.  I used three different functions (k_cause, printf,
and one I wrote called dump) and in each case they work if the dbx
program has not yet seen an interrupt, but the parameter is garbaged
after the call if it has seen an interrupt!

Ron
 

From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 9 Sep 87 16:59:30 PDT 
To: <tfok@hobbes> 
Cc: driscoll@calvin 
Subject: Re: MMU for RODNEY & ISIS 
In-reply-to: <8709092147.AA26297@hobbes.Tymnet.com> of Wed, 9 Sep 87 14:47:52
	PDT

Pat was going to set this up.  Pat...?  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 9 Sep 87 17:00:34 PDT 
To: driscoll@calvin 
Subject: MMU for RODNEY & ISIS 

(begin forwarded message)
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 14:46:41 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03495; Wed, 9 Sep 87 14:47:54 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA26297; Wed, 9 Sep 87 14:47:52
	PDT
Return-path: <tfok@hobbes> 
From: tfok@hobbes (Thomas Fok) 
Date: Wed, 9 Sep 87 14:47:52 PDT 
To: wrs@hobbes 
Message-id: <8709092147.AA26297@hobbes.Tymnet.com> 
Subject: MMU for RODNEY & ISIS 

Mark Akselrod wants to know when will there be a meeting about the change 
of the memory/io addressing scheme.
Please send Mark a mail. He suggestes that Friday morning is a good time for
him. Thanks......
----------------------------------------------------------------
(end forwarded message)

Oops I forgot to send this with the last message.  You probably
wondered what it was all about.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 10 Sep 87 14:51:54 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07561; Thu, 10 Sep 87 14:53:02 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01810; Thu, 10 Sep 87
	14:52:57 PDT
Return-path: <davarj@hobbes> 
From: davarj@hobbes (Jonathan Davar) 
Date: Thu, 10 Sep 87 14:52:57 PDT 
To: wrs@hobbes 
Message-id: <8709102152.AA01810@hobbes.Tymnet.com> 
Subject: k_alarm 


Bill,

What process does k_alarm interrupt?

Jonathan

From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 10 Sep 87 14:53:47 PDT 
To: <davarj@hobbes> 
Subject: Re: k_alarm 
In-reply-to: <8709102152.AA01810@hobbes.Tymnet.com> of Thu, 10 Sep 87 14:52:57
	PDT

Whatever processes are waiting for the event passed to k_alarm will be
woken up.  If no process is blocked for the event when the timer expires,
a flag is set and the next (one) process to do k_wait(whateverEvent) will
wake up immediately.  k_alarm actually does a k_cause.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Thu, 10 Sep 87 18:23:11 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Thu, 10 Sep 87 18:24:00 PDT
Return-path: <DAUL@OFFICE-1.ARPA> 
From: William Daul / McDonnell-Douglas / APD-ASD <WBD.MDC@OFFICE-1.ARPA> 
Date: 10 Sep 87 18:00 PDT 
To: DCE.MDC@OFFICE-1.ARPA, DLS.MDC@OFFICE-1.ARPA, ALG.MDC@OFFICE-1.ARPA,
	DAP.MDC@OFFICE-1.ARPA, CARE.MDC@OFFICE-1.ARPA, SGK.MDC@OFFICE-1.ARPA,
	AICRA.MDC@OFFICE-1.ARPA, TDC.MDC@OFFICE-1.ARPA,
	M189125@SLVM307.McAuto.Tymnet 
Cc: WRS.MDC@OFFICE-1.ARPA 
Message-id: < AUGMENT, 129151, @OFFICE-1> 
Keywords: computer security, computer integrity, computer based information
	systems, data networks, trusted systems call for papers, meeting,
	conference 
Subject: FIRST ANNOUNCEMENT AND CALL FOR PAPERS- -IFIP / Sec'88 - COMPUTER
	SECURITY IN THE INFORMATION AGE 
Author: willis at RAND-UNIX
Access: unrestricted
Location: <AUGMENT, 129151, >
Originally-Sent-To: Security-Forum at R20.UTEXAS.EDU
Taken-From: COMPUTER SECURITY FORUM 14 August 1987 Volume 6: Issue 4

This item is recorded in an AUGMENT Journal.

To obtain a copy, contact the sender.

From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 14 Sep 87 9:39:59 PDT 
To: <akselrod@calvin> 
Subject: Re: Public addresses 
In-reply-to: <8709141623.AA03015@calvin.Tymnet.com> of Mon, 14 Sep 87 09:23:21
	PDT

Sorry, Mark, I got your message, but need to think about things some more
before I reply.  I don't anticipate any objection, but I don't want to
comit myself yet.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 14 Sep 87 9:41:06 PDT 
To: <driscoll@calvin> 
Subject: Re: Boot review 
In-reply-to: <8709141619.AA02997@calvin.Tymnet.com> of Mon, 14 Sep 87 09:19:45
	PDT

rid/boot?  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 14 Sep 87 10:03:35 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19060; Mon, 14 Sep 87 10:04:51 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA03441; Mon, 14 Sep 87
	10:04:46 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Mon, 14 Sep 87 10:04:46 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8709141704.AA03441@calvin.Tymnet.com> 
Subject: Re: Boot review 

That's the one.
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 27 Jul 87 16:55:06 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: "INVIEW OF OFFICE SITUATION HOW"... 
In-reply-to: your message of Mon, 27 Jul 87 16:05:55 PDT

O.K. CHILI'S IT IS
Received: from X930.Tymnet by B39.Tymnet; Mon, 10 Aug 87 14:07:32 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 10 Aug 87 13:25:28 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: Confirmation 

I figured it out!!!!!
  [X] Aug 13 1:30 PM
  [ ] Aug 13 2:00 PM
  [ ] Aug 13 2:30 AM (AM? AM!)

(actually 2:30 am is preferred, but I don't think either of
you would go for it...)
/Carl
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 17 Aug 87 11:09:05 PDT 
To: CARL@X930 
Cc: WRS@C39.Tymnet 
Subject: "My edit comments on your setop"... 

My edit comments on your setop memo follow(I'll use you no. scheme for ref):
Employee record -  6th line - replace"may also be found" by "can be listed
(and thus may be found)"
Security mail address - still think it should say "Mailing address  (tums
preferred, or Ontyme, or Other)
Oper name record - as definition of "license" - "current active licenses
for this name"
Still an open action item for me - to check to see if all"isg no.s" are
infact "employee no.s" - If so, you will change appropriate text accordingly.
Another action item for me - To establish validity critiera for input items.
   Some answers to your list of questions:
1)any manager see below him(her) and any person can see one person's 
records (but by opername signon)
2)similar ans, except the "change anything part" will need much more
thought - in general NO - I envision some kind of "change privelege
table, with each opername being assiged a 'level' of privelege"
Only a very SMALL no. of people to have "all priveleges".
     ______________________________________________________________
We will still need to modify PEAK, even with the latest thoughts on
NAD,amI right??? If so, please contact KEN DAWSON.
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 28 Aug 87 9:01:05 PDT 
To: CARL@X930, WRS@C39.Tymnet 
Subject: "reminder meeting next thurs. 9/3"... 

reminder - meeting - next thurs. 9/3 @ 1:30 - 4th F. B - NAD revisited.
From: CARL@B39.Tymnet 
Date: Thu, 3 Sep 87 23:07:50 PDT 
To: wrs, bartletw 
Subject: NAD %1.07 

I've looked at the code for NAD and it appears that listings are currently
available to anyone running (SOURCE)NAD or anyone having TYMSHARE license.

Actually if the FPN of NAD matches the PPN of the input file, (or TP license)
then a listing can be generated.  What this means is that the current scheme
will allow anyone running NAD to get listings of any file in SOURCE since the
only version of NAD that is really NAD will be in SOURCE.  Files in BETATEST
are not listable unless you have TP license.

I know where to make the changes... as soon as Dennis Ernst confirms which
source file is the correct file then I will make the changes we discussed.

I feel the following "other" changes should be made...
  1)  Listings can only be generated if you have TP license (or)
      if the input file is in your own directory (or)
      if the input file is in your own GAN [since these are unreadable if
                                            the file is protected, so all ok]
  2)  As per (1) don't care what FPN NAD is run from -- remove check.

...otherwise having everyone run (SOURCE)NAD will allow everyone to get
listings whether or not they have TP license.

To Dennis:  Any reason why your current changes cannot released to the
            entire world?  Or should I change (NAD)NAD01.R05 as well as
            your version [NAD01.R06 or NAD01.R07?]

To Wayne:  Please pass this on to Doug so he knows about the listing 'twist'.

/Carl
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 15 Sep 87 8:54:11 PDT 
To: WRS@C39.Tymnet 
Subject: "REMINDER OF THURS'S MEETING 2:00"... 

REMINDER OF THURS'S MEETING @ 2:00 - YOUR OFFICE - MAIN SUBJECT
IS THE APPROACH TO THE SECURITY DESIGN FOR THE NEW MUD REFORMAT.
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 15 Sep 87 8:56:41 PDT 
To: CARL@X930 
Cc: WRS@C39.Tymnet 
Subject: "REVIEW MEETING NEW SETOP HOW IS"... 

REVIEW MEETING - NEW SETOP - HOW IS WED 9/23 @ 10:30 @ BILL'S 
OFFICE(NEED YOUR CONFIRMATION - BILL) - THEN LUNCH - FOLLOWON
WITH NAD, ETC. IF NOT THEN, SUGGEST TIME ON 9/23 24 25 - THANKS.
Received: from tymix.Tymnet by B39.Tymnet; Tue, 15 Sep 87 11:05:37 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23939; Tue, 15 Sep 87 11:06:46 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA26109; Tue, 15 Sep 87
	11:06:36 PDT
Return-path: <gauci@hobbes> 
From: gauci@hobbes (Ed Gauci) 
Date: Tue, 15 Sep 87 11:06:36 PDT 
To: wrs@hobbes 
Message-id: <8709151806.AA26109@hobbes.Tymnet.com> 
Subject: testing simulations 

Bill,

  I understand that there are IPCF testing facilities that run on unix.
Is my understanding correct?  Is any documentation available?

Thx, Ed.
Received: from tymix.Tymnet by B39.Tymnet; Tue, 15 Sep 87 14:44:40 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA24515; Tue, 15 Sep 87 14:45:47 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA29443; Tue, 15 Sep 87
	14:45:38 PDT
Return-path: <bechlyt@hobbes> 
From: bechlyt@hobbes (Tom Bechly) 
Date: Tue, 15 Sep 87 14:45:38 PDT 
To: wrs@hobbes 
Message-id: <8709152145.AA29443@hobbes.Tymnet.com> 
Subject: file server 


Is there going to be a field in the Glob indicating the name of the file
server that was used to load the patron (ISIS).  If not, do you know how
this information can be made available.

thanks,

tom
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Tue, 15 Sep 87 15:16:43 PDT 
To: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>, Ken Holcomb
	<NTD.K/HOLCOMB@Ontyme.Tymnet>, Mike Rude <NTD.M/Rude@Ontyme.Tymnet>,
	Betty Jo (BJ) Chang <NTD.B/CHANG@Ontyme.Tymnet>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme.Tymnet>, Dennis Ernst
	<NTD.D/ERNST@Ontyme.Tymnet>, John Kopf <KopfJ@B39.Tymnet>, Dan Lasater
	<DLasater@B39.Tymnet>, Jeff Oliveto <ENS.J/OLIVETO@Ontyme.Tymnet>,
	Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>, Romolo Raffo
	<NTD.R/RAFFO@Ontyme.Tymnet>, Dick Rawson <NTD.D/RAWSON@Ontyme.Tymnet>,
	Adele Newman <NTD.A/NEWMAN@Ontyme.Tymnet>, Dave Smith
	<NTD.D/SMITH@Ontyme.Tymnet>, DChang, Gazel Tan <GAZEL@B39>, Paul Chu
	<NTD.P/CHU@Ontyme.Tymnet>, Diana Tai <NTD.D/TAI@Ontyme.Tymnet>, Bill
	Soley <WRS@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>, Ashok Chippa
	<ACHIPPA@B39>, Henry Rivers <NTD.H/RIVERS@Ontyme.Tymnet>, Nancy Holt
	<NTD.N/BURRIESCI@Ontyme.Tymnet>, Dennis Jonas <JONAS@B39>, Scott Wedel
	<NTD.S/WEDEL@Ontyme.Tymnet>, Wayne Bartlett
	<TYM.W/BARTLETT@Ontyme.Tymnet>, Rosie Hull <NTD.R/HULL@Ontyme.Tymnet> 
Subject: LOGII GED update 

Hello Folks,

The LOGII GED has been updated to as follows:

- The RCRM flags element has two new bits, indicating a logon zapper
ack is present, and whether the RCRM is a reject (MO-27 present).

- The RCRI MO-24, circuit rejects from the Node Code has code zero
reworded to read "ICRS received for port allready in logon mode".
Two new codes were added.  Code 4 indicates that an ICRS was received
on a port before that port was sent an RCRI.  Code 5 indicates that a port
timed out before sending a subsequent portion of a partial message.

- The unused byte present in all LOGII command channel messages
(NCRM, RCRM, and CCN) now contains 80 to avoid escaping.

- The CCN username for destinations that are not LOGII capable (reduced needle)
contains the non-LOGII username prefix and suffix.  Correspondingly, the
transparent logon indication and sub-host elements are not included in CCNs
bound for non-LOGII destinations, since this information is part of the
reduced needle username.

- All references to "host unknown" have been removed since there is no
difference between a "down" or "unknown" host.  This makes the RCRM/RCRI
MO-27 codes 6 and 16 unused and now invalid, and makes the RCRM/RCRI MO-04
code 8 unused and invalid.

- The Specified Destination List Supervisor translation does not convert
the SDL characters to upper case.

- Reverse charge host status reporting is documented in the section
on Reverse charge.

- The Supervisor takeover message for a LOGII Supervisor has bit 1
of the Supervisor number byte set to indicate a LOGII Supervisor, rather
than resetting bit 0.

- The logon reset indication causes the Supervisor to discard any
information for the logon session, except the CCT, OADR, IIX or UUN
requested indications.

- Accounting type 4.7 unfilled records are padded with "00" rather than
"FF".

- The 032A needle body control character pair is followed by a halfword
if physical record length for the needle body.

- Four new MOs 48 - 4B have been added to the CCN and NCN messages, for
Skywalker access licensing.


The Document has been updated on DNDSPEC:39, SUPDEV:39 and OSERANG:39.

Thanks....Omar


Received: from F29.Tymnet by B39.Tymnet; Tue, 15 Sep 87 15:28:33 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Tue, 15 Sep 87 15:25:47 PDT 
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
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Tue, 15 Sep 87 15:23:01 PDT
From: Postmaster@F29.Tymnet 
Date: Tue, 15 Sep 87 15:05:55 PDT 
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
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Tue, 15 Sep 87 15:05:23 PDT
From: SYSTEM.ONTYME@EMSTUMS.Ontyme.Tymnet 
Date: 15 SEP 87 22:04:43 
To: <CHAPLEAU@OFFICE-1.ARPA> 
Message-id: A12309@Ontyme.Tymnet 
Subject: UNDELIVERABLE MESSAGE - INVALID RECIPIENTS 

Message #A12306 was not delivered to the following recipient(s):

        APD.D/WEAVER.

These recipients do not exist.
Please check the spelling of the recipient(s) listed above.

(undelivered message follows)
------------------------------------------------------------------------
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Tue, 15 Sep 87 14:58:45 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Tue, 15 Sep 87 14:45:22 PDT
Return-path: <CHAPLEAU@OFFICE-1.ARPA> 
From: John Chapleau, Augmentation Systems Division / MDC
        <CARE.TYM@OFFICE-1.ARPA> 
Date: 15 Sep 87 14:41 PDT 
To: RDP.TYM@OFFICE-1.ARPA, SGK.TYM@OFFICE-1.ARPA, WBD.TYM@OFFICE-1.ARPA,
        DCE.TYM@OFFICE-1.ARPA, SKH.TYM@OFFICE-1.ARPA, JDH.TYM@OFFICE-1.ARPA,
        LMP.TYM@OFFICE-1.ARPA, ipc.n/marsh%Ontyme.Tymnet@OFFICE-1.ARPA 
Cc: MAC.TYM@OFFICE-1.ARPA, CARE.TYM@OFFICE-1.ARPA, BJ.TYM@OFFICE-1.ARPA,
        apd.j/murray%Ontyme.Tymnet@OFFICE-1.ARPA,
        apd.d/weaver%Ontyme.Tymnet@OFFICE-1.ARPA,
        apd.m/sorscher%Ontyme.Tymnet@OFFICE-1.ARPA 
Message-id: <TYM-CARE-C734A@OFFICE-1> 
Subject: Lunch for Betty Jolley 

Mary and I are going to take Betty to lunch tomorrow, Wed 9/16.  Let me know if
care to join us.

We are planning to leave VG a little after 12, and go over to the center to see
the D2 layout, and then about quarter to 1 over to the Coleman side of the 
airport for lunch at the 94th Aero-squadron.

--john x8035
Received: from tymix.Tymnet by B39.Tymnet; Tue, 15 Sep 87 17:50:50 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA24885; Tue, 15 Sep 87 17:51:56 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01720;
	Tue, 15 Sep 87 17:51:46 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA16400; Tue, 15 Sep 87
	17:51:45 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 15 Sep 87 17:51:45 PDT 
To: wrs@calvin 
Message-id: <8709160051.AA16400@calvin.Tymnet.com> 
Subject: "Bill, a few things: since I created"... 

Bill,
a few things:
since I created boot.h I think you should remove from rodney.h
all references to bootstrap just for consistency, otherwise we will
overlap.
in boot.h I defined all boot function codes (like RODNEY_BOOT_REQ),
is it ok. ?
the description of k_startPatron() is a bit confusing because it
suggests that it causes the Patron to be started, and just below
it says that "control" is passed tp Patron by (*......
I think it is only an unfortunate selection of words - k_start
only passes the start address.
question -
if a function expects 3 arguments, and I want to pass only the second
argument, others being null is it correct to say acall(, arg,)?
I am getting syntax errors in this area and have problems with
interpretting it.

Mike J.
From: CARL@B39.Tymnet 
Date: Tue, 15 Sep 87 21:04:18 PDT 
To: wrs@b39 
Subject: (MAIL)930.Q 

At least temporarily... you need to have the router forward mail for
system 930 to system 35.  most of the messages in this file on 39 were
probably sent from mailing lists..?..  anyway,   XXX@X930 won't get
delivered until 1) 930 comes back up, and 2) we get a tymnet line to
our micronet so that we can get to 930 from Tymnet.

can you do anything about it... (other than edit the .Q file changing all
the X930's to F35 and renaming it 35.Q ???)

Is this an existing capability of ROUTER.DAT?

/Carl
From: CARL@B39.Tymnet 
Date: Tue, 15 Sep 87 21:08:41 PDT 
To: wrs 
Subject: Bimbo @F70 

Can you tell me the name of the bimbo on F70 that was running a perp job
every 4 seconds and never reading his mail?

Have you implemented the "do not retrun this message" feature when users
do not read their mail?  ...or was it "discard after 30 days" ..or "n days"?
(the second is excellent for messages which are "timely").

/Carl
Received: from D35.Tymnet by B39.Tymnet; Tue, 15 Sep 87 22:09:41 PDT
Return-path: <Carl@D35.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Tue, 15 Sep 87 21:15:55 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: (SOURCE)NAD 

I made the changes to NAD, but I don't know what Dennis Ernst needs to do
to release his version (prior to my changes) as far as testing.  My new
version is on (CARL:39)NAD.SAV and is in need of testing.  It currently
has RF license for reading files in SOURCE and BETATEST and is protected
ALL RUN RUN.

/Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Sep 87 22:57:00 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, a few things: since I created"... 
In-reply-to: <8709160051.AA16400@calvin.Tymnet.com> of Tue, 15 Sep 87 17:51:45
	PDT

When the gid says that "passes control to the Patron by (*startAddress)()",
it means that the startPatron call does that.  It does it however as the
foreground, and you are running in the background so you will still be
returned to.  

You must always pass all arguments.  Depending on the argument type,
there could be a value that the function ignores or treats like no value
such as NULL or 0 or -1 or whatever.  

-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Sep 87 22:59:45 PDT 
To: CARL@B39.Tymnet 
Subject: Re: (MAIL)930.Q 
In-reply-to: your message of Tue, 15 Sep 87 21:04:18 PDT

If I change ROUTER.DAT, the mail will be sent to 35, but 35 needs to have
something to do with it.  It'll spend alot of ime sitting there trying to
deliver it to the real 930 or worse yet, to itself.  I'll work on it.  For
now I'll just send it to 35.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Sep 87 23:05:29 PDT 
To: CARL@B39.Tymnet 
Subject: Re: Bimbo @F70 
In-reply-to: your message of Tue, 15 Sep 87 21:08:41 PDT

I'm sorry, I have no idea how frequently he was running the PERP job.
I've since forgotten the name.  The discard feature is probably not in
the immediate future.  *SIGH*  -Bill
Received: from F29.Tymnet by B39.Tymnet; Wed, 16 Sep 87 11:02:55 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 16 Sep 87 11:03:13 PDT
Return-path: <NTD.BASELINE@EMSNTD.Ontyme.Tymnet> 
From: NTD.BASELINE@EMSNTD.Ontyme.Tymnet 
Date: 16 SEP 87 10:57:57 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I86242@Ontyme.Tymnet 
Subject: :PRODUCT UPDATES TO BASELINE 'Y' 
UPDATESTOSOFTWAREBASELINE"Y": 

     
     
       =================================================================   
        *** NETWORK SOFTWARE BROADCAST *** NETWORK SOFTWARE BROADCAST ***  
        =================================================================  
        **** UPDATE **** UPDATE **** UPDATE **** UPDATE **** UPDATE *****  
        =================================================================  
                                                            PAGE 1 of 1    
     
        DATE>      September 16, 1987   
        TO>        DISTRIBUTION    
        FROM>      Sheldon Cohen, Software Distribution Support  
        REFERENCE> Baseline Document   (baseline)letter.y02 
     
        SUBJ>      UPDATES TO SOFTWARE BASELINE "Y"    
     
     
     
        PLEASE BE ADVISED THAT ALL PRODUCTS YOU WISH TO HAVE ADDED TO THE  
        BASELINE "Y" RELEASE MUST BE SUBMITTED NO LATER THAN 10/14/87.
     
        contact - NTD.BASELINE through ontyme.    
     
        =================================================================  
        *** NETWORK SOFTWARE BROADCAST *** NETWORK SOFTWARE BROADCAST ***  
        =================================================================  
        **** UPDATE **** UPDATE **** UPDATE **** UPDATE **** UPDATE *****  
        =================================================================
Received: from X32.Tymnet by B39.Tymnet; Wed, 16 Sep 87 14:11:48 PDT
Return-path: <JMS@X32.Tymnet> 
From: JMS@X32.Tymnet 
Date: Wed, 16 Sep 87 14:07:57 PDT 
To: wrs 
Subject: "Two people have asked do you get"... 

Two people have asked "how do you get ONTYME to send to TUMS?"
The file "*** TUMS.HLP" does not exist on any ONTYME system (appearantly
ONTYME outputs a message that refers to that file).  One of the askers
was just looking for documentation, the other is Tom Marconi who wants to
have IBM370 CRASH-MAIL sent to a PDP-10 system.  Is there a way to go from
TYMCOM-370 to TYMCOM-X other than via ONTYME?   /Joe
Received: from D35.Tymnet by B39.Tymnet; Wed, 16 Sep 87 16:29:34 PDT
Return-path: <JMS@X32> 
From: Joe Smith <JMS@X32> 
Date: Wed, 16 Sep 87 18:25:47 CDT 
To: wrs 
Subject: Host 14 is still on 940NET. 

(begin forwarded message)

Received: from D35.Tymnet by D35.Tymnet; Sat, 29 Aug 87 19:10:43 PDT
Return-path: <JMS@D35> 
From: Joe Smith <JMS@D35> 
Date: Sat, 29 Aug 87 20:52:47 CDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Landa Morris <IPC.L/Morris@Ontyme>, Jan Krivanec
	<IPC.J/Krivanec@Ontyme>, Vida Stafford <TYMOPS.V/Stafford@Ontyme> 
Subject: Host 14 is still on 940NET. 

When I tried to log into JMS:14, the Supervisor told me the host was down
before I had entered my password.  Using JMS:17 requires a password go get
the host down message.  It appears that transparent login to 940NET is still
in effect.

The following hosts are transparent: 1,2,4,6,8,9,11,12,13,14,16,20.
The following hosts are normal: 3,7,10,15,17,18,19,21.

Someone needs to tell the Supervisors that hosts 14 and 16 are normal hosts,
not transparent.   /Joe

(end forwarded message)

Is this behavior in the group/class list, or is it a kludge in the Supervisor?
If the latter, how long will it take to get new code genned and loaded in
all the SUPs?    ?joe
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 16 Sep 87 16:34:05 PDT 
To: Gazel Tan <GAZEL@B39> 
Subject: Host 14 is still on 940NET. 

(begin forwarded message)
----------------------------------------------------------------
Received: from D35.Tymnet by B39.Tymnet; Wed, 16 Sep 87 16:29:34 PDT
Return-path: <JMS@X32> 
From: Joe Smith <JMS@X32> 
Date: Wed, 16 Sep 87 18:25:47 CDT 
To: wrs 
Subject: Host 14 is still on 940NET. 

(begin forwarded message)

Received: from D35.Tymnet by D35.Tymnet; Sat, 29 Aug 87 19:10:43 PDT
Return-path: <JMS@D35> 
From: Joe Smith <JMS@D35> 
Date: Sat, 29 Aug 87 20:52:47 CDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Landa Morris <IPC.L/Morris@Ontyme>, Jan Krivanec
	<IPC.J/Krivanec@Ontyme>, Vida Stafford <TYMOPS.V/Stafford@Ontyme> 
Subject: Host 14 is still on 940NET. 

When I tried to log into JMS:14, the Supervisor told me the host was down
before I had entered my password.  Using JMS:17 requires a password go get
the host down message.  It appears that transparent login to 940NET is still
in effect.

The following hosts are transparent: 1,2,4,6,8,9,11,12,13,14,16,20.
The following hosts are normal: 3,7,10,15,17,18,19,21.

Someone needs to tell the Supervisors that hosts 14 and 16 are normal hosts,
not transparent.   /Joe

(end forwarded message)

Is this behavior in the group/class list, or is it a kludge in the Supervisor?
If the latter, how long will it take to get new code genned and loaded in
all the SUPs?    ?joe
----------------------------------------------------------------
(end forwarded message)

From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 16 Sep 87 16:34:23 PDT 
To: <JMS@X32> 
Subject: Re: Host 14 is still on 940NET. 
In-reply-to: your message of Wed, 16 Sep 87 18:25:47 CDT

I don't know, I've forwarded your mail to Gazel.  If you don't hear from
her, let me know and I'll follow up.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 17 Sep 87 3:25:50 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Subject: Re: "REMINDER OF THURS'S MEETING 2:00"... 
In-reply-to: your message of Tue, 15 Sep 87 8:54:11 PDT

OK - 2:00 my office.  Needs to be short.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 17 Sep 87 3:27:57 PDT 
To: oserang 
Subject: CD's 

They're in an interoffice envelope on your shelf above the phone books.
Check out the Philip Glass.  I loved the Patric Moraz...clearly on my
list!  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 17 Sep 87 6:24:56 PDT 
To: <akselrod@calvin> 
Subject: Re: Public addresses 
In-reply-to: <8709141623.AA03015@calvin.Tymnet.com> of Mon, 14 Sep 87 09:23:21
	PDT

Hi Mark, having thought things over and talked with Dave and Thomas, I
can say with confidence that your suggestion is okay with me.  I've
updated the document in the RID to show a 1 to 1 correspondence between
public, Tadpole logical, and Tadpole physical addresses.  Public addresses
will have 16 MB resolution ad per Pat's request.  Thanks -Bill
Received: from D35.Tymnet by B39.Tymnet; Thu, 17 Sep 87 10:51:09 PDT
Return-path: <JMS@X32> 
From: Joe Smith <JMS@X32> 
Date: Thu, 17 Sep 87 12:50:30 CDT 
To: wrs 
Subject: Returned MAIL 

(begin forwarded message)

Received: from B39.Tymnet by D35.Tymnet; Thu, 17 Sep 87 3:25:04 PDT
Return-path: <WRS@B39.Tymnet> 
From: Postmaster@B39.Tymnet 
Date: Thu, 17 Sep 87 0:02:17 UT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Returned MAIL 
Resent-from: William R. Soley <WRS@B39.Tymnet> 
Resent-date: Thu, 17 Sep 87 3:23:29 PDT 
Resent-to: jms@35 

The following message was not delivered because:
No router table entry for "32.Q".



----------
RCPT To: <JMS@X32> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 16 Sep 87 16:34:23 PDT 
To: <JMS@X32> 
Bcc: WRS@B39.Tymnet 
Subject: Re: Host 14 is still on 940NET. 
In-reply-to: your message of Wed, 16 Sep 87 18:25:47 CDT

I don't know, I've forwarded your mail to Gazel.  If you don't hear from
her, let me know and I'll follow up.  -Bill

(end forwarded message)

I got voice-mail from Karen in Validations, she said the SUP code will be
changed in about a week.

No router table entry for "32.Q" or "14.Q" or "17.Q"
Received: from tymix.Tymnet by B39.Tymnet; Thu, 17 Sep 87 12:00:46 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07547; Thu, 17 Sep 87 12:05:04 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12134;
	Thu, 17 Sep 87 12:04:52 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA00267; Thu, 17 Sep 87
	12:04:48 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Thu, 17 Sep 87 12:04:48 PDT 
To: wrs@calvin 
Message-id: <8709171904.AA00267@calvin.Tymnet.com> 
Subject: Reading/Writing local ram through VME 

Mike Mo got the word from Tadpole that this is NOT a design
feature of the GP.

John Coons made a suggestion that the serial number of the
board (each board is supposed to have a unique serial number
in the EEPROM) could be used to find out where in memory
the board is located.  The board could write it's serial
number in a known location where other boards could read it.
Each board could assemble a table, also in a known location,
as it read through memory of the board's serial numbers and
their VME addresses.  A board could then read another board's
table, find it's own serial number and address.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Thu, 17 Sep 87 12:06:01 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07563; Thu, 17 Sep 87 12:10:21 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12151;
	Thu, 17 Sep 87 12:10:10 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA00310; Thu, 17 Sep 87
	12:10:06 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Thu, 17 Sep 87 12:10:06 PDT 
To: wrs@calvin 
Message-id: <8709171910.AA00310@calvin.Tymnet.com> 
Subject: ISIS integration needs 

Bill,

  Dan indicates that the ISIS kernel group could use a version of the
Rodney Kernel with IPCF facilities by 10/23.  I need to talk to you
about whether we can focus development and testing to deliver a
stripped-down version of Rodney by 10/23.  We won't need bootstrap,
session stuff, bus error handling, but will need IPCF with
posssibly a faked GLOB.  When can we get together?

PAt
Received: from F29.Tymnet by B39.Tymnet; Thu, 17 Sep 87 15:19:57 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 17 Sep 87 15:21:30 PDT
Return-path: <TNS.K/NILSSON@EMSNTD.Ontyme.Tymnet> 
From: TNS.K/NILSSON@EMSNTD.Ontyme.Tymnet 
Date: 17 SEP 87 15:03:58 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: (24 names) 
Message-id: S21439@Ontyme.Tymnet 
Subject: TELEVERKET PROPOSAL 

=======================  M E M O R A N D U M  =========================

                                                       MCDONNELL DOUGLAS
DATE>      18 SEP 87  00:46                           INFORMATION SYSTEMS

TO>         ROB ALBERTSON
            BRANT CORENSON
            BOB BARBOUR
            DENIS MARCHAL
            BOB GRANDEY
            ANNA CSABA
            BILL SCHRATER
            BOB SMITH
            B.J. CHANG
            GAZEL TAN
            DAVE CHANG
            BILL SOLEY
            DENNIS ERNST
            ADELE NEWMAN
            DAN LASATER
            DAVE BLISS
            RON WHITLOCK
            ROMOLO RAFFO
            KEN JONES
            MATT O'MALLEY
            DAVID ROBINSON
            STEVEN SMITH
            JIM DUNCAN

COPIES>     WARREN PRINCE
            PHIL KRETER
            MIKE CADY
            CLINT DEGABRIELLE
            AL FENN
            KEN HOLCOMB
            JOHN THORNBERG
            BOB TREHIN
            CYRIL MALM

FROM>      THE SWEDISH TEAM
            - KJELL NILSSON
            - STEFAN NICOU
            - LARS MOLINDER
         
            - CHUCK YOUNGER


SUBJECT>   TELEVERKET PROPOSAL


-----------------------------------------------------------------------


THE PROPOSAL FOR THE DATAPAK SECOND GENERATION EQUIPMENT IS NOW
DELIVERED TO TELEVERKET. THANKS TO YOUR HELP WE SUCCEEDED TO DELIVER 
THE PROPOSAL IN TIME. YOU HAVE ALL BEEN EXTREMLY HELPFUL AND COOPERATIVE
TO PROVIDE MDIS-SWEDEN WITH ALL NECESSARY INFORMATION TO BE ABLE TO
MEET TELEVERKET REQUIREMENTS AND PUTTING THE PROPOSAL TOGETHER.

FURTHER ON WE WILL BE INFORMING YOU HOW THE EVALUATION PROCESS
IS PROGRESSING.

BEST REGARDS

KJELL, LARS, STEFAN AND CHUCK
Received: from F29.Tymnet by B39.Tymnet; Thu, 17 Sep 87 16:00:04 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 17 Sep 87 15:46:27 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 17 SEP 87 15:42:52 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I87023@Ontyme.Tymnet 
Subject: NEW ADDRESS/TELEPHONE NUMBERS: 

                                     MEMORANDUM
                               NETWORK SYSTEMS COMPANY
                       QUALITY SERVICES AND TECHNICAL SUPPORT

          TO:       DISTRIBUTION                       CC:  DON JOHNSON
          FROM:     GLORIA KOLEK                            GARY WALKER
          DATE:     11 SEPTEMBER 1987                       ROURKE McCUSKER
          SUBJECT:  NEW ADDRESS/TELEPHONE NUMBERS:
                    (QSATS) N.T.S., S.T.S, Q.A.
          _________________________________________________________________

          NEW ADDRESS FOR U.S. MAIL DELIVERY:
          McDONNELL DOUGLAS CENTER
          (DEPT OR EMPLOYEE NAME + MAIL STOP)  HTS, STS, QA: M.S. F-2
          2560 N. FIRST STREET
          P. O. BOX 49019
          SAN JOSE, CA 95161-9019

          NEW ADDRESS FOR PRIVATE DELIVERY SERVICE (UPS, FED-EX, DHL, ETC)
          McDONNELL DOUGLAS CENTER
          (DEPT OR EMPLOYEE NAME + MAIL STOP)  HTS, STS, QA: M.S. F-2
          2560 N. FIRST STREET
          SAN JOSE, CA 95131

          TELEPHONE LISTING FOR QSATS (HTS, STS, QA):
          AREA CODE: 408      PREFIX NUMBER FOR ALL EXTENSIONS: 922

          NAME________________     DEPARTMENT__________________  EXTENSION
          AKELLA, RAM              QUALITY ASSURANCE             7461
          ARNETT, MARY             QUALITY ASSURANCE             7462
          BUBSEY, GAIL             STS (INTERFACE SUPPORT)       7463
          BURROUGH, JOHN           STS (INTERFACE SUPPORT)       7464
          CATTANEO, AUGUST         NTS (H/W TECH. SUPPORT)       7466
          CHEN, BEN                STS (NETWORK SUPPORT)         7468
          CHENG, HENRY             QUALITY ASSURANCE             7470
          CHIANG, SHIRLEY          STS (INTERFACE SUPP, MGR)     7471
          CHU, PING                STS (INTERFACE SUPPORT)       7472
          CLARIDGE, JON            QUALITY ASSURANCE             7473
          CURRI, LOUISE            STS (STS TECH. ANALYST)       7477
          DIAS, LARRY              QUALITY ASSURANCE             7478
          ELLENBERG, DOUG          STS (APPLICATIONS SUPP, MGR)  7480
          FISTE, TOD               STS (INTERFACE SUPPORT)       7484
          HO, RICHARD              STS (INTERFACE SUPPORT)       7486
          HOUGE, DIANA             QUALITY ASSURANCE             7487
          HUSSEY, DAVE             QUALITY ASSURANCE             7489
          JOHNSON, DON             QSATS (NTS, QA, MANAGER       6222
          KOCHMANN, TIM            NTS (H/W TECH. SUPPORT)       7493
          KOLEK, GLORIA            QSATS (NTS, STS, QA, SECY)    7495
          KRUMVIEDE, PAUL          NTS (H/W, S/W DIAG. SUPPORT)  7496
          LANGDON, CARL            STS (NETWORK SUPP, MGR)       7497
          LIU, JEFF                STS (APPLICATIONS SUPPORT)    7498
          LUCAS, JERRY             STS (NETWORK SUPPORT)         7499
          MATUCHA, DEAN            QUALITY ASSURANCE             7500
          McKIERNAN, JOHN          STS (INTERFACE SUPPORT)       7501












          MISTRY, PRAVIN           QUALITY ASSURANCE             7502
          MOORING, ED              NTS (H/W, S/W, DIAG. SUPPORT) 7504
          NARAYAN, KRISHNAN        STS (APPLICATIONS SUPPORT)    7507
          NELSON, PHIL             NTS (H/W TECH. SUPPORT)       7508
          NILLUKA, SANDRA          STS (NETWORK SUPPORT)         7510
          REILLY, JOHN             STS (NETWORK SUPPORT)         7447
          RICHARDS, STAN           NTS (H/W TECH. SUPPORT)       7513
          ROWE, CLIVE              STS (NETWORK SUPPORT)         7514
          SHARMA, ALOK             STS (INTERFACE SUPPORT)       7516
          SHEPHERD, BRUCE          NTS (H/W TECH. SUPPORT)       7517
          SHUTICH, JOHN            NTS (H/W TECH. SUPPORT)       7069
          STEIN, JIM               NTS (H/W TECH. SUPPORT, MGR)  7518
          TURNER, TOM              QUALITY ASSURANCE             7520
          VIVIER, RON              NTS (H/W S/W DIAG SUPP, MGR)  7521
          WALKER, GARY             STS (APPL,NW,INTERFACE, MGR)  7523
          WANG, JAMES              STS (NETWORK SUPPORT)         7524
          WARNER, RUTH             QUALITY ASSURANCE             7525
          YEH, TED                 STS (APPLICATIONS SUPPORT)    7528


          NOTE #1:
          FIELD ENGINEERING HANDBOOK IS COMPLETE AND AVAILABLE TO ORDER.
          PLEASE CONTACT YOUR MANAGER FOR FURTHER DETAILS.


          NOTE #2:
          DUE TO "CIRCUMSTANCES BEYOND OUR CONTROL", OUR MOVE HAS BEEN
          DELAYED FOR ONE WEEK.  WE WERE ORIGINALLY SCHEDULED TO MOVE ON
          FRIDAY, 11 SEPTEMBER 1987; OUR NEW MOVING DATE IS TARGETED FOR
          FRIDAY, 18 SEPTEMBER 1987.  ANY AIL BY U.S. POSTAL SERVICE OR
          PRIVATE MAIL CARRIER WILL BE DELIVERED TO NEW MAILING ADDRESS
          AND REROUTED TO ORCHARD PARKWAY.

          PRESENT TELEPHONE NUMBERS FOR NTS,  STS AND QA  ARE  STILL  VALID
          UNTIL FURTHER NOTICE.

          WHEN  TELEPHONE  NUMBERS  ARE  CHANGED,  AND  YOU  PHONE  OUR OLD
          NUMBERS, THERE WILL BE A RECORDED MESSAGE STATING THAT THESE
          NUMBERS HAVE BEEN DISCONNECTED AND YOU WILL BE GIVEN A NEW NUMBER
          TO USE.

          VOICE MAIL WILL REMAIN ACTIVATED ON OLD TELEPHONE NUMBERS FOR ONE
         WEEK AFTER TELEPHONE NUMBERS ARE CHANGED.

          IF IN DOUBT, PLEASE CALL CORPORATE TELEPHONE OPERATOR AT
          (408) 922-0250.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 2 Jul 87 23:35:07 PDT 
To: ebuell@calvin (Eric Buell) 
Subject: Re: Process trees 
In-reply-to: <8706301709.AA24778@calvin.Tymnet.com> of Tue, 30 Jun 87 10:09:27
	PDT

I believe the process tree elimination will not affect you.  If I remember
correctly from earlier discussions, you will have one main process for all
TSAP/SSAP, plus one process per TSAP/SSAP for down-level traffic, and one
process per TSAP/SSAP for up-level traffic.  The main process would do a
pair of k_sprout() calls for each new connection to setup the traffic
handling processes.  When the connection is closed, the two traffic
handling processes for that connection would be terminated by [somebody] as
part of cleaning up the connection.  This could be done either by returning
from the main function of each of the processes, or by calling
k_terminate().  This would work for both the old and new scheme of
processes.  I should emphasize that you would have to keep track of the PID
of each process and when you want to do the terminate, call k_terminate()
once for each process.  (Except if k_terminate() is called from within the
process being terminated, the PID is not required.)

I wanted to make sure there is no misunderstanding here before I delete the
tree feature, but to my knowledge, nobody was actually going to use it.
Is my summary correct, above?

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 3 Jul 87 14:06:20 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07434; Fri, 3 Jul 87 14:10:09 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA22545; Fri, 3 Jul 87 14:10:04
	PDT
Return-path: <coons@opus> 
From: coons@opus (John Coons) 
Date: Fri, 3 Jul 87 14:10:04 PDT 
To: wrs@b39 
Message-id: <8707032110.AA22545@opus.Tymnet.com> 
Subject: UNIX documentation 




Bill,

I too am amazed that UNIX documentation has not been copied and distributed
to all, as I found out just yesterday that the library was unwilling to let
people make copies of the docs they have due to copyright notices.
However, we have ON-LINE copies of most man pages which include no notices.
Do you know an easy way to to do a "man -t" of all of man(1), for example,
without having to do it separately for every command? Then we could copy and
distribute these to everyone at negligible cost.

Let me know ...

John

Received: from D35.Tymnet by B39.Tymnet; Sun, 5 Jul 87 8:32:38 PDT
Return-path: <LINDLEY@D35.Tymnet> 
From: LINDLEY@D35.Tymnet 
Date: Wed, 1 Jul 87 16:26:40 PDT 
To: wrs 
Subject: New Soley 

Bill:
What is the latest on the Soley household.  I presume the baby arrived--
and life is a bit hectic.  Please let me know name, etc.

Thanks.
-Lois
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 5 Jul 87 15:22:45 PDT 
To: LINDLEY@D35.Tymnet 
Subject: Re: New Soley 
In-reply-to: your message of Wed, 1 Jul 87 16:26:40 PDT

Well the name is either
  William Christian Soley
or
  Ester Ann Soley
but we won't know until its born.  We're still waiting.
It was due June 21 or June 28 depending on who you ask.
I'll let you know.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Tue, 7 Jul 87 13:59:43 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 7 Jul 87 13:46:24 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 07 JUL 87 13:10:10 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I55491@Ontyme.Tymnet 
Subject: "M E M O R A N D U M TYMNET NETWORK"... 

                            M E M O R A N D U M

                                                        [] TYMNET - NETWORK
                                                           SYSTEMS SUPPORT
DATE>      01 JUL 87  11:32

TO>        *** ALLSALES/TECH
           NTD.SUP

COPIES>    Gloria Kolek
           ** TECHSTAFF
           Beverly Atchison
           Rosie Hull
           Tom Turner
           Fin.Fixedassets

FROM>      Diana Houge and Tom Turner
           Quality Assurance Document Control


SUBJECT>   DNDSPEC Procedures


-----------------------------------------------------------------------



    
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

              DNDSPEC DOCUMENT TRANSFER PROCEDURES
                    AND DIRECTORY MAINTENANCE

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


The  DNDSPEC directory which contains development documents  will 
now  reside on system 38 instead of system 33.   This system move 
is  a result of the recent TECHREF directory move from system  56 
to system 38.

Only ERS documents will be transferred from the DNDSPEC system 38 
directory  into  the  TECHREF  system  38  directory.  All  other 
development  documents will stay in the DNDSPEC directory.   This 
will  be  managed  by  the  Quality  Assurance  Document  Control 
Coordinator  (Tom Turner; 408-432-5025).

When  a  document  is transferred from  DNDSPEC  system  39  (NTD 
environment)   to  DNDSPEC  system  38  (TYMNET  environment)   a 
notification  memo will be sent out.   Those to be informed  are:  
Software Technical Services, Hardware Technical Services, Quality 
Assurance,  Product  Marketing and Fixed  Assets.   The  ontyme's 
included are:  

               NTD.B/ATCHISON
               Teresa Fassbender (for distribution to Product Mktg.)
                    (ontyme to be assigned soon)
               FIN.FIXEDASSETS
               QSATS.D/HOUGE
               NTD.R/HULL
               QSATS.G/KOLEK  (for distribution to HTS and QA)
               ** TECHSTAFF   (for distribution to STS)
               QSATS.T/TURNER               

When  an  ERS document is transferred from DNDSPEC system  38  to 
TECHREF system 38,  a notification memo will be sent to All Sales 
and All Tech.
Received: from F38.Tymnet by B39.Tymnet; Tue, 7 Jul 87 14:07:43 PDT
Return-path: <MKTGINFO@F38.Tymnet> 
From: MKTGINFO@F38.Tymnet 
Date: Tue, 7 Jul 87 14:01:56 PDT 
To: WRS 
Subject: ACCESS# AGAIN! 

MAYBE I CAN TYPE A LITTLE SLOWER.  I AM READY TO RELEASE THE NEW ACCESS#
PROGRAM.  I WOULD LIKE IT TO HAVE THE SAV SAME KIND OF PROTECTION THAT THE
INFORMATION SYSTEM HAS, NAMELY THE LOGOUT ON DISCONNECT PROTECTION.  THE 
DIFFERENCE IN THIS NEW APPLICATION IS THAT IT RUNS IN MAGNUM.  I KNOW HOW
TO SET UP THE MAGNUM TO AUTOLOAD ONCE THE "FTMAG" COMMAND IS ISSUED.  I
WOULD RATHER NOT STORE ANY OF THE RELATIONS OR THE ACCESS# PROCEDURE UNDER
THE USERNAME ACCESS#, BUT RATHER USE THE SCHEMA AND RELATIONS UNDER THE
USERNAME DDD ON SYSTEM 38.  ACCESS# IS CURRENTLY UNDER SYSTEM 74, BUT I
CAN GET THE ACCESS# LOCATION CHANGED.  IS IT POSSIBLE TO MODIFY THE
SECURE PROGRAM TO FUNCTION IN THIS ENVIRONMENT?  I NOTICED IN ITS DOCUMENTA-
TION THAT YU DISABLED LOWER CASE DUE TO ITS USE WITH FORTRAN.  I WILL NEED
LOWER AND UPPER CASE ENABLED FOR THIS APPLICATION.  IF YOU HAVE QUESTIONS/
COMMENTS/ETC PLEASE DON'T HESITATE.

THANKS AND THANKS AGAIN!!
LYNN LAWRENCE (408)432-5024
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 8 Jul 87 13:56:37 PDT 
To: IPC.C/BLOCK@EMSNTD.Ontyme.Tymnet 
Subject: Re: Can't reach you using... 
In-reply-to: A78005@Ontyme.Tymnet of 07 JUL 87 07:58:27

You have to put the line
	To: WRS@B39.Tymnet (TUMS)
in the first line of the Ontyme (or PROFS) message and then send it
to TUMS.GATEWAY (or TUMSGATE).  However, if you are on Ontyme, it
is much simpler to just send to NTD.WRS since that mail is automatically
forwarded to me at WRS@B39.Tymnet.  I'm looking in to the disconnect/
reconnect problem now.  Will get back to you shortly.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 3:46:38 PDT 
To: IPC.C/BLOCK@EMSNTD.Ontyme.Tymnet 
Subject: Re: Loop In Tumsgate service machine 
In-reply-to: A77999@Ontyme.Tymnet of 07 JUL 87 07:46:01

It getting these error message (some of each) while running its exec:

DMSFNS908T FILE SYSTEM ERROR DETECTED. VIRTUAL ADDR '191'. REASON CODE '05'.
DMKDSP450W CP ENTERED; DISABLED WAIT PSW '00020000 00307404'

DMSITP144T ADDRESSING EXCEPTION OCCURRED AT 309794 IN ROUTINE FINIS WHILE 'UFDBUSY' = 00, RE IPL CMS.
DMKDSP450W CP ENTERED; DISABLED WAIT PSW '00020000 5031FBA2'

They don;'t mean anything to me and I gave away all my CMS manuals.
Do they mean anything to you?  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 3:48:37 PDT 
To: IPC.C/BLOCK@EMSNTD.Ontyme.Tymnet 
Subject: Re: Loop In Tumsgate service machine 
In-reply-to: A77999@Ontyme.Tymnet of 07 JUL 87 07:46:01

Also, I noticed that somebody modified the PROFILE EXEC on 7/3.  There
don't seem to be any changes to it, however.  Probably somebody in SL
since it was at 8:08 and nobody would be up here, yet.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 3:58:16 PDT 
To: MKTGINFO@F38.Tymnet 
Subject: Re: ACCESS# AGAIN! 
In-reply-to: your message of Tue, 7 Jul 87 14:01:56 PDT

ACCESS# is going to be a little problem.  The "#" is illegal in
user names (actually some programs don't care and some do).  We
will have to talk on the phone to figure something out.  Is it
going to run on only one system (74)?  I don't see DDD valid there.
Is it going to have a password?  (since its published, why bother)
It may be wise to get rid of the "#" by setting up ACCESS# to
just print a message like
	The name of this service has been changed from "ACCESS#" 
	to "ACCESS".  Please type "ACCESS" at the "please log in:".
	please log in:
This is just a thought.  Anyway, let me know when everything is
ready to cutover and I'll figure out some way to do it, even with
the "#".  Its actually not much of a problem.  -Bill
   home: 408-866-0443 afternoon or evening, please
   work: give up
Received: from tymix.Tymnet by B39.Tymnet; Thu, 9 Jul 87 18:20:14 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23245; Thu, 9 Jul 87 18:24:31 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA03587; Thu, 9 Jul 87 18:24:35
	PDT
Return-path: <krumvp@opus> 
From: krumvp@opus (Paul Krumviede) 
Date: Thu, 9 Jul 87 18:24:35 PDT 
To: wrs@39.Tymnet.com 
Message-id: <8707100124.AA03587@opus.Tymnet.com> 
Subject: SFTP 

Bill,
  Would it be possible to install SFTP on system 26?  I have been working
on moving the MXP source to a Sun, and would like to be able to get the
results on 26 without going via 39.  I noticed that the SFTP.SAV on 26
is older and different than on 39, but was reluctant to move it myself.
If it is OK, I can move it (I may also want it elsewhere, but not now).

thanks,
Paul
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 18:21:13 PDT 
To: kopfj@opus (John Kopf) 
Subject: Re: Bill: will you be in the office soon for consultation? 
In-reply-to: <8707092314.AA03145@opus.Tymnet.com> of Thu, 9 Jul 87 16:14:36 PDT

By appointment only.  How about tomorrow first thing after lunch.  I have
an 11:30 appointment that will probably last to about 1:30 and can drop by
at that time.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 18:24:38 PDT 
To: krumvp@opus (Paul Krumviede) 
Subject: Re: SFTP 
In-reply-to: <8707100124.AA03587@opus.Tymnet.com> of Thu, 9 Jul 87 18:24:35 PDT

Yes, that's okay to move SFTP to 26, and almost anywhere else.  The master
system is 39 so that should be the correct version to use.  Please move
  SFTP.SAV and SFTP.DOC
please do not move the sources.  Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 10 Jul 87 10:54:38 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA26608; Fri, 10 Jul 87 09:00:38 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06742;
	Fri, 10 Jul 87 09:00:04 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA28454; Fri, 10 Jul 87
	09:00:43 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Fri, 10 Jul 87 09:00:43 PDT 
To: wrs@calvin 
Message-id: <8707101600.AA28454@calvin.Tymnet.com> 
Subject: Re: boot.mm 

Please go ahead and update boot.mm, kernel.mm, and ipcf.mm.  I talked
with Steve about the boot looking for alternate file servers and
we agreed that the boot would search only for SFS0 through SFS9
in order to keep the boot simple. This should be documented in
boot.mm.

On another issue raised at the review, how will the kernel
(or kernel processes like the logger) check pointers passed
as arguments?  We may need a scheme like that used for VME
accesses or a routine like rkCheckPublic.

If the pointer is checked and found invalid then a new
RERR code must be created.

Have you thought about how rodney should handle crashes?
Specifically, what errors should be treated as recoverable
or non-recoverable?  Should an internal process crash
Rodney if an internal consistency check fails or
log the problem and re-initalize?

Any new additions to the family?

Pat
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 10 Jul 87 10:59:20 PDT 
To: driscoll@calvin (Pat Driscoll) 
Subject: Re: boot.mm 
In-reply-to: <8707101600.AA28454@calvin.Tymnet.com> of Fri, 10 Jul 87 09:00:43
	PDT

Actually the water just broke and we're getting contractions at 20 min,
so...this must be the weekend.
Boot can scan for "*:SFS?" and take the minimum function of the result or
it could do repeating lookups, but repeating lookups use alot of VME
resources (ten times the cost of a scan).  Either way, I'll say its 0-9
in the document, the rest is an internal issue.

I'm not sure the kernel needs to check pointers, it will find out if
they are invalid when it references them.  Data structure inconcistencies
have always been the most severe type of crash on the Tymcom-X.  By the
time we noticed that a linked list looped, or something like that, there
were usually so many other things screwed up that there was no way to
recover.  If you consider what has to go wrong to damage, a data base
for example, it usually involves some module that shouldn't be accessing
it at all scribbling in the wrong place.  Assuming the code that updates
the data base is correct (which is reasonable since it is usually very
simple and can be subjected to formal proofs) then its only a hardware
failure or another section of code that can screw it up.  Reinitializing
the victim is usually fruitless and there is no way to determine who
actually did the corruption.  I strongly suggest crash on data structure
inconsistency.  If its a fixable problem, the crash will fix it and
make useful information available to support staff, not crashing may
cause data integrity errors (undetected) and just have things limp along.
There are also few cases where you can reinitialize part of the kernel
data base without seriously confusing the patron.  For example we can
not reinitialize the symbol table or the TSAP data base because there
is no way to recover the data from the patron and other processes.  I
also think it will be a very rare thing in Rodney, running in ROM on
a single board computer.  Enough soap box.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Mon, 13 Jul 87 16:41:39 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 13 Jul 87 16:35:10 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 13 JUL 87 16:24:47 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I57950@Ontyme.Tymnet 
Subject: Bay Area Consolidation 

     TO:       All South Bay Employees

     FROM:     John Baumgarten

     DATE:     July 10, 1987

     SUBJECT:  Bay Area Consolidation
     =================================================================

     As you are all aware, most Bay Area ISG Operations are to move
     soon to the New McDonnell Douglas Center on North 1st Street and
     Trimble Road in San Jose.

     Many of you will have questions about the move that you will want
     answered.  Please direct all questions to your respective
     building's move coordinator.  Your move coordinators are listed
     below:
                                        Current         Move
     Group                              Building     Coordinator     Extension
     --------------------------------------------------------------------------
     Network Interface Applications      Bubb F      S. McFarland &    7349
     Pilot Operations                    Bubb G      Mike Rude         6160
     Product Support Engineering         Bubb G            "
     Network Systems Architecture        Bubb D            "
     Consulting and Support Services     Bubb D            "
     NTD Administration                  Bubb D            "
     Hardware Systems Engineering        Bubb H            "
     Network Systems Architecture        Bubb H            "
     NTD Administration                  Bubb H            "
     Tymnet Access Interface (TAI)       VG2A              "
     Network Analysis & Design           Bubb H            "

     Finance/Administration              VG1 & 2     Ellen Martino &   6521
     MIS                                 VG1B        Fred Canning      6382
     Purchasing                          VG1B              "
     Information Processing Co. (IPC)    VG2A        Nicki Marsh       7971
     Field Service Co (FSC)              Liberty     Craig Fletcher    116-2403
     Health Systems/Physicians Systems   Ironsides   Craig Smith       986-9644

     Network Operations                  OP          Steve Marquis     5026
     Facilities                          OP/VG1B     Gina Zwaagstra    6436
     Applied Communications Systems Co   VG2B/VG3    Debbie Eldridge   6294

     Marketing                           OP          Judie Sassali     5060
     Marketing                           Bubb F      Alice Leonard     5358
     Marketing                           VG1A              "
     Human Resources                     VG1A              "
     Business Development & Planning     VG1A        Toni Agnelli      8113
     Marketing & Planning                TTE               "
     NSC Headquarters                    VG1B        Carolee Neff      7682
     Legal                               VG1 & 2           "

     Payment Systems Co (PSC)            Liberty     Cynthia Oswald
     Payment Systems Co (PSC)            VG1 & 2           "
     Computer Systems Co (CSC)           VG2B              "
Received: from tymix.Tymnet by B39.Tymnet; Wed, 15 Jul 87 9:29:35 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13292; Wed, 15 Jul 87 09:34:02 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27994;
	Wed, 15 Jul 87 09:32:45 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA17778; Wed, 15 Jul 87
	09:33:34 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 15 Jul 87 09:33:34 PDT 
To: wrs@calvin 
Message-id: <8707151633.AA17778@calvin.Tymnet.com> 
Subject: Edit of rid/ipcf.mm 

Bill,

You've had ipcf.mm out for editing since 07/01/87.  Is it finished
or can it be returned?

PAt
Received: from F29.Tymnet by B39.Tymnet; Thu, 16 Jul 87 9:50:03 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 16 Jul 87 9:49:23 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 16 JUL 87 08:52:35 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I59232@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS NETWORK SYSTEMS"... 



                 MCDONNELL DOUGLAS NETWORK SYSTEMS COMPANY

                     T Y M N E T    O P E R A T I O N S

                            M E M O R A N D U M



DATE>      14 JUL 87  08:58

TO>        ALL BAY AREA EMPLOYEES

COPIES>	    MARK LAUVER    

FROM>      JOHN BAUMGARTEN


SUBJECT>   NEW OPEN OFFICE FURNITURE


-----------------------------------------------------------------------


YOU ARE WELCOME TO COME SEE THE NEW STEELCASE OPEN OFFICE FURNITURE
"MOCK-UPS" LOCATED ON THE FIRST FLOOR NEAR MANAGEMENT & PROFESSIONAL 
DEVELOPMENT DEPT. BEHIND THE TRAINING ROOM IN VG2 (20705 VALLEY GREEN).

THESE MOCK-UPS WILL GIVE YOU A GENERAL IDEA OF THE TYPE OF CUBICLE WHICH 
WILL BE IN THE NEW MCDONNELL DOUGLAS CENTER.
Received: from tymix.Tymnet by B39.Tymnet; Mon, 20 Jul 87 10:23:09 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA29387; Mon, 20 Jul 87 10:27:38 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06362; Mon, 20 Jul 87
	10:26:01 PDT
Return-path: <gauci@hobbes> 
From: gauci@hobbes (Ed Gauci) 
Date: Mon, 20 Jul 87 10:26:01 PDT 
To: wrs@hobbes 
Message-id: <8707201726.AA06362@hobbes.Tymnet.com> 
Subject: ipcf 

Bill,
   does ipcfDeassign clean up IPCF directory entries that belong to the
deassigned TSAP?

Ed.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 20 Jul 87 16:46:37 PDT 
To: NTD.D/BLISS@EMSNTD.Ontyme.Tymnet 
Subject: Re: Meeting, Jul. 7 
In-reply-to: I49364@Ontyme.Tymnet of 19 JUN 87 10:53:53

This meeting (re: Tadpoll mods, throughput, etc.) has been rescheduled
so much I'm not sure what the final date is, if any.  I can't find it
in my calander.  Please advise.  Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 21 Jul 87 8:27:48 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA03289; Tue, 21 Jul 87 08:29:43 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA13294; Tue, 21 Jul 87
	08:29:36 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Tue, 21 Jul 87 08:29:36 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707211529.AA13294@calvin.Tymnet.com> 
Subject: Re: object file 

Bill,

  Pamela Whitehurst is responsible for developing the E.OUT to
X409 conversion.  She should know about start addresses.  Will
you be in today to talk about schedules?

Pat
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 21 Jul 87 12:38:07 PDT 
To: driscoll@calvin (Pat Driscoll) 
Subject: Re: object file 
In-reply-to: <8707211529.AA13294@calvin.Tymnet.com> of Tue, 21 Jul 87 08:29:36
	PDT

Thanks (re: Pam).  If I come in today, it will impact my schedule.  Since
I promised to have the kernel stuff ready to plug into the simulator by
Wednesday, I think I'd better pass on appointments until Thursday if at
all possible.  Maybe Wednesday afternoon but I won't know until tonight.
Is it okay to delay talking about schedules until then?  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 22 Jul 87 12:01:41 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00365; Wed, 22 Jul 87 12:03:44 PDT
Received: by eunix.Tymnet (5.51/4.7) id AA00594; Wed, 22 Jul 87 12:03:29 PDT
Return-path: <kopfj@eunix> 
From: kopfj@eunix (John Kopf) 
Date: Wed, 22 Jul 87 12:03:29 PDT 
To: skywalker@eunix 
Message-id: <8707221903.AA00594@eunix.Tymnet> 
Subject: ~r sky_doc/iix 


From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 22 Jul 87 13:37:12 PDT 
To: kopfj@eunix (John Kopf) 
Subject: Re: ~r sky_doc/iix 
In-reply-to: <8707221903.AA00594@eunix.Tymnet> of Wed, 22 Jul 87 12:03:29 PDT

"~r" doesn't work on subjects.  Either answer subject and then do the "~r"
or use
	mail skywalker < sky_doc/iix
-Bill

PS - please resend the message
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 22 Jul 87 13:51:18 PDT 
To: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Subject: Re: That topic again 
In-reply-to: <8707221951.AA27194@oliven.ATC.OLIVETTI.COM> of Wed, 22 Jul 87
	12:51:29 PDT

Well, yet another somewhat biased, but mostly accurate opinion.  One thing
I have noticed about the circumcision issue is that every discussion I've
heard quotes different "facts" and consists of about 66% emotional bias.
I don't really disagree with anything he said, just his style (and that of
the only reference he quoted).  Anyway, its past the 8th day, so the point
is mute.  I only feel unconfortable with the decision because I feel I've
been unable to collect accurate information, however I have certainly
collected alot of dubious information.  Sigh.  One last note on your comment
about being more sensitive when circumcised, there were two cases quoted in
one of the books we read of adults being circumcised by choice.  One was
very happy and one felt it ruined his sex life, but they both agreed that
it made them less sensitive.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 22 Jul 87 20:36:51 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01807; Wed, 22 Jul 87 20:39:04 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA26057; Wed, 22 Jul 87
	20:37:36 PDT
Return-path: <julies@hobbes> 
From: julies@hobbes (Julie Stier) 
Date: Wed, 22 Jul 87 20:37:36 PDT 
To: wrs@hobbes 
Message-id: <8707230337.AA26057@hobbes.Tymnet.com> 
Subject: Re: DP printer broken 

I am sorry for all this inconvienience with the printer.  Steve will look
into it tonight.  I believe it works fine from all locations but opus.
If that is incorrect, please let me know.  I know that problems are
not attended to quickly during this period of time.  We are also
desparately trying to get the new building up too (which of course, true
to nature, is not without it's own unique set of experiences.

julie

From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 23 Jul 87 5:08:49 PDT 
To: julies@hobbes (Julie Stier) 
Subject: Re: DP printer broken 
In-reply-to: <8707230337.AA26057@hobbes.Tymnet.com> of Wed, 22 Jul 87 20:37:36
	PDT

It does not work from hobbes or calvin, either.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 23 Jul 87 18:36:57 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06005; Thu, 23 Jul 87 18:39:16 PDT
Received: by mac.tymnet.com (3.2/SMI-3.2) id AA01243; Thu, 23 Jul 87 18:34:43
	PDT
Return-path: <seltzer@mac> 
From: seltzer@mac (Les Seltzer) 
Date: Thu, 23 Jul 87 18:34:43 PDT 
To: wrs@mac 
Message-id: <8707240134.AA01243@mac.tymnet.com> 
Subject: UNITAP 

Bill,

Where is the new version of UNITAP on system 39?
The version in my directory is years old.

Les
	
Received: from F38.Tymnet by B39.Tymnet; Fri, 24 Jul 87 10:41:11 PDT
Return-path: <DDD@F38.Tymnet> 
From: DDD@F38.Tymnet 
Date: Fri, 24 Jul 87 10:39:24 PDT 
To: WRS 
Subject: ACCESS# 

BILL,

SINCE SUCH A LARGE AMOUNT OF PROBLEMS EXISTED WITH THE ACCESS#USERNAME, I HAVE
DECIDED TO PROD
PROCEED AS FOO
 
FOLLOWS:

I HAVE A USERNAME CALLED ACCESSNUM ON SYSTEM 38.  UNDER THAT USERNAEME THE
REQUIRED MAGNUM PROGRAM PLUS RELATIONS WILL BE RESIDENT.  I HAVE SET O UP
THE MAGNUM TO LOAD AND RUN AUTOMATICALLY.  THE QUESTION IS WHETHER THE
SECURE PROGRAM COULD BE INPLEMENTED IN AN I  ENVIRONMENT SUCH AS THIS.

LET ME KNOW IF YOU HAVE ANY IDEAS ON THIS.

THANKS
LYNN LAWRENCE
Received: from tymix.Tymnet by B39.Tymnet; Fri, 24 Jul 87 17:21:03 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA10240; Fri, 24 Jul 87 17:23:24 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12397;
	Fri, 24 Jul 87 17:21:48 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14142; Fri, 24 Jul 87
	17:22:52 PDT
Return-path: <dave@calvin> 
From: dave@calvin (Dave Smith) 
Date: Fri, 24 Jul 87 17:22:52 PDT 
To: wrs@calvin 
Message-id: <8707250022.AA14142@calvin.Tymnet.com> 
Subject: the Rodney utcGet() function 

Bill,
In the description for the Rodney utcGet() function, you say that
the slow and fast clock values that are returned are "consistent"
with each other.  What does this mean?  Specifically, does this mean
they are synchronized with each other?  Alfred would like to be able
to get values that are synchronized (or enough info to synchronous
the two itself) for the printing of timestamps to the millisecond
level.

From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 24 Jul 87 17:05:13 PDT 
To: DDD@F38.Tymnet 
Cc: Wayne D. Bartlett <BARTLETW@B39.Tymnet> 
Subject: Re: ACCESS# 
In-reply-to: your message of Fri, 24 Jul 87 10:39:24 PDT

I am concerned about the use of MAGNUM for several reasons.  First it is
not a supported product.  Second it is prone to altering the environment
and possibly defeating the security measures.  Third, there is nobody to
work with to verify or correct these problems.  Fourth, it is very
expensive.  I would consider volunteering my services to write a SAIL
program to provide the required services.  I believe this would be a
relatively minor effort which would have a considerable payoff.  The
second suggestion is to incorporate it into the INFORMATION username.
If it was not in MAGNUM, this would be simple and would reduce the
security exposure by having only one environment to worry about.  Please
let me know your thoughts on this.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 24 Jul 87 18:14:14 PDT 
To: dave@calvin (Dave Smith) 
Subject: Re: the Rodney utcGet() function 
In-reply-to: <8707250022.AA14142@calvin.Tymnet.com> of Fri, 24 Jul 87 17:22:52
	PDT

They are synchronized in the sense that the expression
	utc * 1000000 + fastc % 1000000
whould yield the number of microseconds past the origin date if the
machine actually had a word size long enough to compute it.  Put
another way, at the theoretical instant when utc is incrimented,
then (fastc % 1000000 == 0).  Is this what you need?
-Bill

PS - don't forget to do something to make reading the pair atomic or
you will get occasional off by 1 second errors.  You could use MOVEM.L
or a read,read,compare loop.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 25 Jul 87 4:32:49 PDT 
To: DDD@F38.Tymnet 
Subject: Re: ACCESS# 
In-reply-to: your message of Fri, 24 Jul 87 10:39:24 PDT

Lynn,
I've checked out the ACCESS.NUM program on 38, and I've found several
problems with the data that we should talk about.  I also finished a
SAIL program which is functionally similar to the MAGNUM program, but
since it uses the same data, it suffers from the same problems which
I will mention.  In short, I've found areas that have local access which
are not in the file, and I've found cases where the number listed is
out of the area even though a number exists in the same central office.
Examples:
	408-980 says closest is 415-Fremont, but what about 409-980-8100?
	716-745 says no local, but 716-285 is local.
-Bill
Received: from F29.Tymnet by B39.Tymnet; Sun, 26 Jul 87 5:21:39 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Sun, 26 Jul 87 5:08:38 PDT
Return-path: <ENS.J/OLIVETO@EMSNTD.Ontyme.Tymnet> 
From: ENS.J/OLIVETO@EMSNTD.Ontyme.Tymnet 
Date: 26 JUL 87 04:51:53 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: (30 names) 
Message-id: I63490@Ontyme.Tymnet 
Subject: TYMNET Numbering Plan 
TYMNETNumberingPlan: 

           TYMNET - McDonnell Douglas Network Systems Company
             External Network [ExNet]  -  Technical Services

           Tel# 703/356-6995   Telex# 82693 UF   Fax# 703/448-0346
                   Drop: 310600024915   or   TYMINTLVA

DATE>      25 JUL 87  23:39

TO>        Gary Stoy

COPIES>    See Distribution

FROM>      Jeff Oliveto


SUBJECT>   TYMNET Numbering Plan


-----------------------------------------------------------------------


     External Network Services has authored a white paper dicussion
on TYMNET's current X.121 numbering plan.  It contains suggestions to
simplify the current numbering plan and recommendations for needed
expansion.  The file is located on INTLTECH:25,33,38  under the name
NWX121.TNP.  

     For those that will be attending the meeting on Tuesday 7/28 at
10:00, please bring this document with you as it will be the basis of
TYMNET's discussion.  I apologize for the short time frame in which to
review the proposal.

- Jeff -
Received: from tymix.Tymnet by B39.Tymnet; Mon, 27 Jul 87 9:49:03 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14907; Mon, 27 Jul 87 09:51:35 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04506; Mon, 27 Jul 87
	09:49:50 PDT
Return-path: <dave@hobbes> 
From: dave@hobbes (Dave Smith) 
Date: Mon, 27 Jul 87 09:49:50 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707271649.AA04506@hobbes.Tymnet.com> 
Subject: Re: the Rodney utcGet() function 

Thanks, that's exactly what I need.  What happens to fastc during
seconds that are half-length or double-length for correction?
Also, my copy of the Rodney doc does not show fastc anywhere in the
glob, may I assume that it will be placed right next to utc?

Dave
Received: from tymix.Tymnet by B39.Tymnet; Tue, 28 Jul 87 11:21:46 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18810; Tue, 28 Jul 87 11:25:51 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05891;
	Tue, 28 Jul 87 11:24:16 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA09443; Tue, 28 Jul 87
	11:25:09 PDT
Return-path: <hsing@calvin> 
From: hsing@calvin (Yu-liang Hsing) 
Date: Tue, 28 Jul 87 11:25:09 PDT 
To: wrs@calvin 
Message-id: <8707281825.AA09443@calvin.Tymnet.com> 
Subject: kernel scheduler 

I have 2 questions regarding to the kernel scheduler:
  1. While playin your testing program with the dbx, I found that the dbx
     was acting funny.  Although I ignored the ALRM, I am not sure if this
     has anything to do with the setjmp and longjmp.

  2. Is there a possibility that one may want to cancel a sleeping process?
     If yes, then k_sleep needs to return the reqid.

Yu-Liang
Received: from F29.Tymnet by B39.Tymnet; Wed, 29 Jul 87 9:40:19 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 29 Jul 87 9:28:14 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 29 JUL 87 08:42:27 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I64833@Ontyme.Tymnet 
Subject: TTE Course Schedule 

                            M E M O R A N D U M

                          NETWORK SYSTEMS COMPANY
                         NETWORK SYSTEMS MARKETING


DATE>      24 JUL 87  08:29

TO>        The World

COPIES>    

FROM>      Tymnet Training and Education


SUBJECT>   TTE Course Schedule


-----------------------------------------------------------------------


TTE is pleased to announce the 4Q schedule and additions to the 3Q
schedule of courses.

Please note the following changes in the curriculum.
    1.  Addition of 2 days to TTE 235 Async Tymcom Consat Public Outdial
        so that we can adequately cover consat material.
    2.  Pilot test of a new course TTE 255 Advanced X.25.  This pilot
        will be held at TTE September 14-18.  To enroll select the 
        "special" option in the list of courses.  This pilot is restricted
        to MDC employees.
    3.  Addition of a half day course on the use of OnTyme.  The first
        offering will be for Field Service only on Friday afternoon
        October 2.  The course will be repeated on Monday afternoon
        November 23 and is open to anyone.

Addition to 3Q schedule -
TTE 255 (special) Advanced X.25                   5 days    Sep 14-18

                      4th Quarter Schedule
All Classes will be held at TTE in San Jose unless indicated.

Introductory Track
100 - Basic Data Communications                   3 days    Nov 4-6
101 - Tymnet Concepts (internal)                  2         Dec 7-8
102 - Network Operations (internal)               8         Dec 9-18
501 - Tymnet Concepts (private Nets)              2         Nov 9-10
502 - Network Operations                          8         Nov 11-20
520 - Network Design - Tops (internals only)      10        Nov 2-13 Vienna

Protocol/Interface Track
210 - IBM Data COmmunications                     5         Oct12-16
                                                            Dec 7-11
215 - Generic X.25                                5         Nov 2-6
225 - TYMNET X.25                                 5         Nov 9-13
230 - TYMNET 3270/CMT                             5         Oct 19-23
235 - Async Tymcom Consat Public Outdial          7*        Nov 16-24
240 - TYMNET SNA                                  5         Oct 26-30
                                                            Dec 14-18

Software Track
400 - Introduction to ISIS Software               5         Oct 19-23
                                                            Nov 30-Dec 4
430 - Assembly Language Programming               5         Oct 12-16
440 - TYMNET Engine Programming                   5         Oct 19-23
442 - ISIS Slot Programming                       10        Oct 26-Nov 6
445 - System Code Generation                      5         Dec 7-11

Hardware Track
310 - Communications Concepts and Operations      15        Oct5-23
330 - TYMNET Hardware Maintenance and
        Troubleshooting                           15        Oct 26-Nov 13
345 - Magnetic Tape and Disk Maintanance          5         Nov 16-20

OTHER -
CSS - Customer Satisfaction Skills                1 1/2     Oct 1-2
OnTyme                                            1/2       Oct 2 P.M
                                                            Nov 23 P.M
BPS - Basic Products School                       3         Nov 18-20

To enroll in these classes or to find out more about them use OnTyme
   :EXEC *** TTE.ENROLL
The OnTyme system will be updated to reflect this schedule shortly.
We look forward to seeing you in class.
Received: from F29.Tymnet by B39.Tymnet; Wed, 29 Jul 87 9:40:28 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 29 Jul 87 9:28:38 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 29 JUL 87 08:49:08 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I64843@Ontyme.Tymnet 
Subject: TTE 255 Advanced Tymnet X.25 

                            M E M O R A N D U M

                          NETWORK SYSTEMS COMPANY
                         NETWORK SYSTEMS MARKETING


DATE>      24 JUL 87  15:11

TO>        All Employees

COPIES>    

FROM>      Tymnet Trainig and Education


SUBJECT>   TTE 255 Advanced Tymnet X.25


-----------------------------------------------------------------------


TTE is pleased to announce the piloting of TTE 255, Advanced Tymnet
X.25 Implementation and Troubleshooting, to be held at the TTE, 
San Jose facility, September 14-18, 1987

The course has bees designed and is intended for Tymnet personnel who
have taken TTE 225 and have had a minimum of six months experience
with Tymnet X.25 implementation and/or support.

The course will be delivered by experienced support and field personnel
to include Michelle Willhauck, Jerry Lucas, Dan Lane, Patty Dixon,
Ed Moorine and Chris Nicoll.

The following is a brief topical outline of the course:

Day One -
    o    Accelerated Concepts
         a.  New features in next version of code.
         b.  Tymfile parameters not commonly used, ie; xcomO3.dO2
         c.  Shared X.25

    o    International Topics
   
         a.  X.75 Gateway Basic Considerations
         b.  Modulo 128 Frames
         c.  Satellite Considerations

                                                                Page  2

    o    X.25 Interactions with ISIS

Day Two and Three -
    o    Xcom Internal Specifics

         a.  IIX Protocol
         b.  Consat Pad Interaction with X.25 (specifically IIX)
         c.  X.25 PVC
         d.  Discussion of multi-base, multi-targeted host limitations

    o    General Troubleshooting Techniques

         a.  Analyzing Dumps (i.e., what to look at)
             1.  Error Tables
             2.  Crash Tables
             3.  Reading memory locations
             4.  Debug using traces
         b.  Setting Traps
         c.  Recognition of common problems and their causes.
             1.  T-1 timer expirations
             2.  SIO related problems
             3.  Discussions of known vendor problems (i.e., NPSi, COMTEN)
             4.  Packet Level reset
         d.  Response Time Problem
             1.  Backpressure logic (i.e., recognize who is doing the
                 backpressure)
             2.  Basic fixes (i.e., TPUT class, available patches, PAD
                 forward, window size etc.)

Day Four and Five -
    o  Code Structure
       a.  High-level diagram and interactions of modules
       b.  Discuss each module individually
           1.  dispatcher
           2.  record make
           3.  record teardown
           4.  Q-bit packets
           5.  foreground
       c.  Discuss ring handling routines
       d.  Buffering logic
       e.  Hypothetical transactions (i.e., code walkthrough
           1.  Get needle -  send out call request packet
           2.  Get call request packet - process and send call accept or 
               clear
       f.  Research labs
           Instructor to hand out RFI's submitted from the field.
           Student to research code and formulate answers.  discuss
           solution provided by HQ.
       g.  Final Lab
           Students to write a simple patch followed by general
           discussion of proposed solutions.

Class size is limited to sixteen, to ensure that you enroll early
through OnTyme access   :EXEC *** TTE.ENROLL
Using "Special" selection for course registration, or by contacting 
Cheryl McGowan, TTE Registrar at (408) 432-5042

We look forward to a very beneficial and successful class.
Received: from F29.Tymnet by B39.Tymnet; Wed, 29 Jul 87 9:40:36 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 29 Jul 87 9:28:56 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 29 JUL 87 08:59:36 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I64856@Ontyme.Tymnet 
Subject: Removal of Trash for MOve 

     TO:       All South Bay MDC Personnel

     FROM:     Dick Denesha

     DATE:     July 27, 1987

     SUBJECT:  Removal of Trash for MOve
     =================================================================

     TRASH, TRASH and more TRASH.  Here is how we are going to get rid
     of it!!.

     Carts marked with TRASH will be placed in each building at
     various locations.  They may be used by anyone, and Facilities
     Maintenance or the janitor will empty them as needed.  If you
     need immediate service, call 446-8048 for action.  PLEASE! do not
     put any WET trash into these carts.

     Now is a good time to get rid of outdated phone books and
     directories.  There will be a limited number of new San Jose
     books available for the Fremont movees, so everyone else please
     bring your 1987 books with you.

     The ELECTRICAL POWER STRIPS, EXTENSION CORDS, HEATERS AND FANS
     will not be needed or allowed (except in special circumstances)
     in the new buildings.  Please leave them on your desk for pickup
     by Facilities Maintenance.

     If you have any questions, please contact your local maintenance
     person:

     VALLEY GREEN        IGNACIO GUERRERO              446-6008

     BUBB RD.            BILL HUNT                     446-6040

     LIBERTY             YASUO KOTAKA                  415/794-2550

     OPW                 SLIM DAVENPORT                432-5224

     EMERGENCY FROM ANY LOCATION - 408-446-8048
Received: from F38.Tymnet by B39.Tymnet; Wed, 29 Jul 87 11:06:13 PDT
Return-path: <DDD@F38.Tymnet> 
From: DDD@F38.Tymnet 
Date: Wed, 29 Jul 87 10:57:18 PDT 
To: WRS 
Subject: B ACCESS# 

BU
BILL,

I JUST RE-READ YOUR TUMS MESSAGE.  DOES THIS MEAN THAT ONE
ONCE I RE-CONSTUCT THE MAGNUM DATA THE SAIL PROGRAM CAN TAKE ITS PLACE?
I GUS
GUESS I MISSED THAT THE FIRST TIME THROUGH.  I'LL CALL YOU TONITE (7/29)
AND WE CAN TALK ABOUT THIS FURTHER.

THANKS MUCHO,

LYNN
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 29 Jul 87 14:33:35 PDT 
To: DDD@F38.Tymnet 
Subject: Re: B ACCESS# 
In-reply-to: your message of Wed, 29 Jul 87 10:57:18 PDT

Ok tonight is good because the machine with all my assigned work on
it is down for the move to N 1st so I'm available more than normal.
I'll look for your call.  408-866-0443
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 20:32:07 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01362; Thu, 6 Aug 87 20:23:30 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15459; Thu,
	6 Aug 87 20:24:30 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01355; Thu, 6 Aug 87 20:23:21 PDT
Received: by oliveb.ATC.OLIVETTI.COM (smail2.3) id AA07105; 3 Aug 87 12:40:29
	PDT (Mon)
Received: from hpda.UUCP by hplabs.HP.COM with TCP ; Mon, 3 Aug 87 12:36:32 pdt
Received: from hpcltjb. by hpda.UUCP (13.1/3.5) id AA23334; Mon, 3 Aug 87
	12:35:27 pdt
Received: from hpcltjb by hpcltjb; Mon, 3 Aug 87 12:35:49 pdt
Return-path: <oliveb!hplabs!hpcltjb!hpda!brengle@tymix.Tymnet> 
From: Tim Brengle <hplabs!hpcltjb!hpda!brengle> 
Date: Mon, 03 Aug 87 12:35:44 PDT 
To: Bill Soley <tymix!wrs> 
Message-id: <8708031935.AA23334@hpda.UUCP> 
Subject: Hello! 
X-Mailer: mh6.5

Hello, Bill.

Congratulations on the new addition to your family!  How does it feel to be
a Dad?  Getting any sleep?

We are expecting our third (yes, THIRD) sometime late in December.
Marshall is 4 and our daughter Meredith is 2.

We moved to a house in Campbell near Hacienda and Winchester.  Where are
you all located?  I saw the return address in Campbell...

Say "Hi!" to Becky for us.

				Tim Brengle
				...!hplabs!brengle
				brengle@hplabs.hp.com
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 6 Aug 87 21:57:36 PDT 
To: driscoll@calvin (Pat Driscoll) 
Subject: Re: test message 
In-reply-to: <8708062233.AA12061@calvin.Tymnet.com> of Thu, 6 Aug 87 15:33:23
	PDT

----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Thu, 6 Aug 87 19:17:16 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00819; Thu, 6 Aug 87 19:16:36 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA14460;
	Thu, 6 Aug 87 15:35:21 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA12061; Thu, 6 Aug 87 15:33:23
	PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Thu, 6 Aug 87 15:33:23 PDT 
To: wrs@calvin 
Message-id: <8708062233.AA12061@calvin.Tymnet.com> 
Subject: test message 

This is a test message.  Please acknowledge.
Pat
----------------------------------------------------------------
Test received.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 6 Aug 87 21:59:12 PDT 
To: coons@opus (John Coons) 
Cc: julies@opus 
Subject: Re: TUMS 
In-reply-to: <8708060321.AA10269@opus.Tymnet.com> of Wed, 5 Aug 87 20:21:58 PDT

TUMS was unble to deliver to the Unix systems (including opus) since all
mail into and out of the Unix world is routed through tymix and tymix has been
down due to the move.  All the mail bound to Unix from TUMS (including
Ontyme) has been queued and should be delivered to you by now.  Unix on the
otherhand returns any mail that hasn't been delivered in 3 days to the sender
so mail sent from a Unix system to TUMS (inclusing Ontyme) has likely been
returned to its sender on Unix.  If it isn't returned, it is probably delivered
by now (i.e. mailsent within the last 3 days).  Let me know if there is any
further problem.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 6 Aug 87 22:09:41 PDT 
To: Tim Brengle <oliveb!hplabs!hpcltjb!hpda!brengle@tymix.Tymnet> 
Subject: Re: Hello! 
In-reply-to: <8708031935.AA23334@hpda.UUCP> of Mon, 03 Aug 87 12:35:44 PDT

Amazing.  We are on Archer Way which is south off of Westmont about 2
blocks west of San Thomas Aquino.  Three, eh?  Amazing.  Did you always
plan it that way?  Anyway, you'll all have to drop in and see us some
time.  Telephone might be more efficient: 866-0443.  Tymnet's moved from
Bubb Road to N. 1st and Trimble: 922-6210.  Oh well.  Three!  -Bill

PS - tymix is our Usenet gateway and has been down the last week due to
the move so things should be faster now...
Received: from F38.Tymnet by B39.Tymnet; Sat, 8 Aug 87 12:35:08 PDT
Return-path: <DDD@F38.Tymnet> 
From: DDD@F38.Tymnet 
Date: Sat, 8 Aug 87 12:33:38 PDT 
To: WRS 
Subject: ACCESS# 

I DON'T KNOW IF YOU GOT THAT LAST FRAGMENT OF TEXT.  IN SUMMARY:
I AM ADDING A SPEED FIELD TO THE FIELDS TO BE PRINTED OUT BY THE
ACCESS# PROGRAM.  I WILL TRY TO HAVE THE NEW DATA IN FILE FORM BY
8/10.

LYNN
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 10 Aug 87 13:07:02 PDT 
To: DDD@F38.Tymnet 
Subject: Re: ACCESS# 
In-reply-to: your message of Sat, 8 Aug 87 12:33:38 PDT

Ok - I've gotten real busy, but we should still be able to get this
set up.  I won't have time to do anything with the MAGNUM procedure
except maybe make some suggestions.  They'd be better on the phone
or in person.  Anyway, by "file form" do you mean the little file I
mentioned last time "ACCESS.DAT"?  -Bill
My new office # is 922-6210, mail stop B4.
Received: from X930.Tymnet by B39.Tymnet; Mon, 17 Aug 87 19:59:13 PDT
Return-path: <JMS@X930.Tymnet> 
From: JMS@X930.Tymnet 
Date: Mon, 17 Aug 87 19:55:52 PDT 
To: wrs 
Subject: "What sort of a system is HOBBES?"... 

What sort of a system is HOBBES?  How many systems are connected to ARPAnet
as "xxxxxx.TYMNET.COM"?  (I assume OFFICE-1 and OFFICE-4 are included.)
By the way, we have our 2nd 2020 system up, the one with 3 RP06 disks.
Since it is using X95's old base, the original X95 is not in the network.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 19 Aug 87 11:25:05 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00487; Wed, 19 Aug 87 11:29:24 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23912; Wed,
	19 Aug 87 11:28:57 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00480; Wed, 19 Aug 87 11:29:09 PDT
Received: by oliveb.ATC.OLIVETTI.COM (smail2.3) id AA20946; 18 Aug 87 19:54:16
	PDT (Tue)
Received: from snail.sun.com by sun.Sun.COM (4.0/SMI-4.0) id AA26508; Tue, 18
	Aug 87 19:12:07 PDT
Received: from Sun.COM (arpa-dev) by snail.sun.com (4.0/SMI-3.2) id AA03851;
	Tue, 18 Aug 87 19:05:06 PDT
Received: from seismo.CSS.GOV by Sun.COM (4.0/SMI-3.2) id AA01460; Tue, 18 Aug
	87 19:07:27 PDT
Received: from mcnc.mcnc.org by seismo.CSS.GOV (5.54/1.14) id AA05742; Tue, 18
	Aug 87 22:11:27 EDT
Received: by mcnc.mcnc.org (5.54/MCNC/1-2-87) id AA16783; Tue, 18 Aug 87
	22:11:15 EDT
Received: by convex (4.12/4.7) id AA11946; Tue, 18 Aug 87 19:07:52 cdt
Received: by smu (4.12/4.7) id AA29523; Tue, 18 Aug 87 18:39:28 cdt
Received: by txsil.UUCP (4.12/4.7) id AA25053; Tue, 18 Aug 87 17:13:11 cdt
Return-path: <oliveb!sun!seismo!mcnc.org!convex!smu!txsil!gregl@tymix.Tymnet> 
From: sun!seismo!mcnc.org!convex!smu!txsil!gregl (Greg Lyons) 
Date: Tue, 18 Aug 87 17:13:11 cdt 
To: relay.cs.net!convex!sun!oliveb!tymix!wrs@smu 
Message-id: <8708182213.AA25053@txsil.UUCP> 
Subject: ICON 


I made it back to Dallas safely.  It only took 56 hours once I got on the
road.  Had to have Constant Velocity joints (what ever those are) replaced
before I could start.  The most interesting part of the trip was a brief hail
storm in New Mexico.  I had a very full car - no room for hitch hikers - but
decided to stop anyway when I saw two figures being pelted with hunks of ice.
Managed to move enough stuff from passenger seat to the back so that the two
of them could just squeeze in out of the storm.  Good thing they were small.
It turned out to be a couple of Navaho girls;  cousins hitching back to the
reservation from Phoenix.  Went to a resturant for lunch;  They decided not
to try for a more spacious ride so I took them all the way to the reservation.
It was a good cross-cultural experience.  I learned, and have since forgotten,
a bit of Navaho.  Also learned a lot about relating to 'minority' people.
Though I have had some experience and a lot of training I have never had an
opportunity quite like this one before.  It is hard to communicate very much
in a couple of hours but we did amazingly well.

Oops got to go soon and I wanted to give you some ICON addresses:

ARPA:	icon-project@arizona.EDU
UUCP:	...{allegra, ihnp4, noao}!arizona!icon-project

FTP from /usr/ftp/icon on arizona.EDU

That out to do it.  I have postal address if you need it.

I'll try to get on birdix later.

see you.

-Greg

Received: from D55.Tymnet by B39.Tymnet; Wed, 19 Aug 87 12:36:37 PDT
Return-path: <BRUCES@D55.Tymnet> 
From: BRUCES@D55.Tymnet 
Date: Wed, 19 Aug 87 12:41:52 PDT 
To: Mail Wizard <MAIL> 
Subject: TUMS-10 bug report (v45000000 of 23-MAY-87) 

I seem to be having some trouble forwarding a message to an OnTyme
user, i get the following message retruned from the System:

  No router table entry for "ONTYME.Q"

Thanks for your help,
Bruce Shepherd  (ext 5161 - OP)
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 20 Aug 87 14:58:07 PDT 
To: <oliveb!sun!seismo!mcnc.org!convex!smu!txsil!gregl@tymix.Tymnet> 
Subject: Re: ICON 
In-reply-to: <8708182213.AA25053@txsil.UUCP> of Tue, 18 Aug 87 17:13:11 cdt

Thanks for the info on icon, the latest manual for 4.3 BSD includes a
couple documents on icon, but I can't find it on the system.  It might
be that it is distributed along with 4.3 BSD.  I'll look for your mail
on Birdix - remember its ^z, not ^d.  I really enjoyed talking with you
while you were here.  It will definately be too long before we have that
opportunity again.  The other ways are just not the same (mail, email,
even telephone).  Oh well.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 25 Aug 87 12:46:49 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05312; Tue, 25 Aug 87 12:52:14 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA11736; Tue,
	25 Aug 87 12:52:05 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA12595; Tue, 25 Aug 87 12:52:09 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 25 Aug 87 12:52:09 PDT 
To: wrs@f104 
Message-id: <8708251952.AA12595@f104.sun.com> 
Subject: Logger background 

Guess what? I not only need an initialization call l_bkgInit, but I need
to sprout the process in the first place!
Can you please add a k_sprout of l_bkgMain in the appropriate place.
Ron
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 25 Aug 87 13:43:44 PDT 
To: <rmurphy@f104> 
Subject: Re: Logger background 
In-reply-to: <8708251952.AA12595@f104.sun.com> of Tue, 25 Aug 87 12:52:09 PDT

Unless I misunderstand, can't you do the sprout from l_bkgInit()?  -Bill
Received: from F29.Tymnet by B39.Tymnet; Fri, 28 Aug 87 13:36:05 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 28 Aug 87 13:26:48 PDT
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Fri, 28 Aug 87 13:07:47 PDT
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Fri, 28 Aug 87 13:06:49 PDT
Return-path: <M108013@SLVMCL1.McAuto.Tymnet> 
From: Carol Block (M108013) L243 301-2W Ext. 33729
	<M108013@SLVMCL1.McAuto.Tymnet> 
Date: 08/28/87 15:05:02 
To: NTD.W/Soley@Ontyme.Tymnet, NTD.W/Soley@Ontyme.Tymnet,
	NTD.W/Soley@Ontyme.Tymnet 
Subject: Test of multiple PROFS notes to Ontyme Users 

Bill, I have a hunch that you will receive three notes from this test. The
check sum depending on how you do it may find a difference in the last line of
the note PROFS creates. It has a hex field that can be used for number of
copies of the note. It is not always consistant, so you cant use it to
eliminate duplicate notes, but it may be enough to fake out your newest mod to
identify copies. I thought of this after your last phone conversation and
thought I had better do a test and not leave it to the user that reported the
problem.
Have a good day,
Carol
Received: from F29.Tymnet by B39.Tymnet; Fri, 28 Aug 87 15:38:27 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 28 Aug 87 15:36:32 PDT
Received: from F29.Tymnet by EMSTUMS.Ontyme.Tymnet; Fri, 28 Aug 87 15:12:58 PDT
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Fri, 28 Aug 87 14:50:47 PDT
Return-path: <M108013@SLVMCL1.McAuto.Tymnet> 
From: Carol Block (M108013) L243 301-2W Ext. 33729
	<M108013@SLVMCL1.McAuto.Tymnet> 
Date: 08/28/87 16:49:41 
To: NTD.W/Soley@Ontyme.Tymnet 
Subject: Success then 

Thanks Bill, for responding to my note. I just had the concern about the hex
field in the last line.....you are correct, to check sum the rest of the note
is valid. By the way....when you send me an ONTYME note you have included in
the FROM: your name and userid...how as an ONTYME user did you do that?
Have a good day,
Carol
Received: from tymix.Tymnet by B39.Tymnet; Mon, 31 Aug 87 18:55:33 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01218; Sun, 30 Aug 87 16:49:40 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA14067;
	Mon, 31 Aug 87 16:49:22 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA09012; Mon, 31 Aug 87
	16:49:16 PDT
Return-path: <alexting@calvin> 
From: alexting@calvin (Alex Ting) 
Date: Mon, 31 Aug 87 16:49:16 PDT 
To: ako@calvin, feldman@calvin, hrivers@calvin, jwillott@calvin, kopfj@calvin,
	seltzer@calvin, wrs@calvin 
Message-id: <8708312349.AA09012@calvin.Tymnet.com> 
Subject: UNIX doc tools meeting 



	You are cordially invited to attend a UNIX documentation
   tools meeting on September 9, Wednesday, 2:00 PM, in the small
   conference room on third floor in Bldg. B. The purpose of this
   meeting is to discuss what UNIX utilities are needed to prepare
   NTD documents and, to gather experienced users' opinions on what 
   macros should be included in a psroff/MM package for standard
   NTD documentation styles, such as internal memoranda and 
   development documents. We will discuss the "STDDFS.MM" macro 
   package and its various versions. We will also discuss other
   topics such as index, change bar, and graphs. Yes, we will 
   also like to be able to update the status of our efforts to
   upgrade DWB software to release 2.0 by the meeting time.
	 Please send me a message indicating whether you can 
   attend this meeting or not.


   Alex

From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 1 Sep 87 16:17:55 PDT 
To: <feldman@comet> 
Subject: Re: su, .login 
In-reply-to: <8709012310.AA07112@comet.Tymnet.com> of Tue, 1 Sep 87 16:10:19
	PDT

YP is replicated, in't it?  So it only requires the network to be up to
distribute updates.  So a user on calvin can still "su" if hobbes is down.
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 11:08:36 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02767; Wed, 9 Sep 87 11:09:19 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23665; Wed, 9 Sep 87 11:08:55
	PDT
Return-path: <julies@hobbes> 
From: julies@hobbes (Julie Stier) 
Date: Wed, 9 Sep 87 11:08:55 PDT 
To: microtec@hobbes 
Message-id: <8709091808.AA23665@hobbes.Tymnet.com> 
Subject: Clock Wrapping and MCC68k 


For your information in using Microtec C compiler:


				    MEMO

To:	Dan Lasater

From:	Dave Smith

Date:	Sept 3, 1987

Subj:	Clock wrapping in C

=============================================================================

There is a problem in C programs on the 68020 when the usual relational
operators >, >=, <, <= are used to compare two values of a clock or other
variable that can wrap from positive to negative and back to positive.
The problem is that if the variables are declared to be unsigned, then the
test fails when they wrap from negative to positive and if they are declared
signed, the test fails when they wrap from positive to negative.

The reason this is a problem on the 68020 and wasn't on the Engine is that
the Microtec C compiler uses the 68020 BGT, BGE, BLT, and BLE instructions
for signed compares.  These instructions take into account the value of the
overflow flag as well as the sign bit when deciding when to branch (see the
appendix below).  The Engine did not have an overflow flag, so this was never
even a consideration.  The 68020 also has instructions that test only the
sign bit, but there seems to be no compiler-independent way to get the
compiler to use them.

The solution is to use assembler routines to compare the values of two clocks
or other such variables.  The following routines are defined in the file
/usr/hobbes/skyisis/src/lib/clkcmp.s under SCCS for the Microtec assembler
and are also available for the Sun assembler:

	clkgt(arg1, arg2)
	clkge(arg1, arg2)
	clklt(arg1, arg2)
	clkle(arg1, arg2)

They each return 1 if the first argument has the indicated relationship with
respect to the second argument, or 0 if it does not.  Note that the equal and
not-equal tests do not have this problem and can use the regular test
operators.

Please make all Skywalker developers aware of this problem and the solution.



APPENDIX:  Clock wrapping transition examples (for those who care)

Pos-to-neg: 80000001 - 7FFFFFFE = 3, N=0, V=1, C=0
Neg-to-pos: 00000001 - FFFFFFFE = 3, N=0, V=0, C=1

In both cases, the first value is larger than the second for a continuously
incrementing clock.  The Negative flag indicates this, but the oVerflow
flag is on in the first case and that causes the BGT, BGE, BLT, and BLE
instructions to consider the Negative flag wrong, thus signed compares
do not work; the BPL and BMI instructions ignore the oVerflow flag and
would work fine.  The Carry flag is wrong in the second case, so unsigned
compares do not work.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 9 Sep 87 15:38:58 PDT 
To: <unet!billp@tymix.Tymnet> 
Subject: Re: new bioya phone #'s 
In-reply-to: <8709081514.AA01663@billp.unet.uucp> of Tue, 8 Sep 87 08:14:26 PDT

I have a new PROM containing the following changes:

ADD	408-236; SAN JOSE  
ADD	408-432; SAN JOSE  
ADD	408-437; SAN JOSE  
DEL	408-555; DIR ASST    
ADD	408-562; SAN JOSE  
ADD	408-623; SAN JUAN  
ADD	408-761; WATSONVL  
ADD	408-879; CAMPBELL  
ADD	408-922; SAN JOSE  
ADD	408-924; SAN JOSE  
DEL	415-261; OAKLAND     
DEL	415-265; BERKELEY    
DEL	415-268; OAKLAND     
DEL	415-271; OAKLAND     
DEL	415-272; OAKLAND     
DEL	415-273; OAKLAND     
DEL	415-339; OAKLAND     
DEL	415-353; MT VIEW     
ADD	415-354; PALO ALTO 
DEL	415-420; OAKLAND     
DEL	415-428; OAKLAND     
DEL	415-436; OAKLAND     
DEL	415-437; OAKLAND     
DEL	415-444; OAKLAND     
DEL	415-446; OAKLAND     
DEL	415-448; OAKLAND     
DEL	415-451; OAKLAND     
DEL	415-452; OAKLAND     
DEL	415-464; OAKLAND     
DEL	415-465; OAKLAND     
DEL	415-466; OAKLAND     
DEL	415-482; OAKLAND     
DEL	415-486; BERKELEY    
DEL	415-521; ALAMEDA     
DEL	415-522; ALAMEDA     
DEL	415-523; ALAMEDA     
DEL	415-524; BERKELEY    
DEL	415-525; BERKELEY    
DEL	415-526; BERKELEY    
DEL	415-527; BERKELEY    
DEL	415-528; BERKELEY    
DEL	415-529; RICHMOND    
DEL	415-530; OAKLAND     
DEL	415-531; OAKLAND     
DEL	415-532; OAKLAND     
DEL	415-533; OAKLAND     
DEL	415-534; OAKLAND     
DEL	415-535; OAKLAND     
DEL	415-536; OAKLAND     
DEL	415-539; OAKLAND     
DEL	415-540; BERKELEY    
DEL	415-547; OAKLAND     
DEL	415-548; BERKELEY    
DEL	415-549; BERKELEY    
DEL	415-555; DIR ASST    
ADD	415-551; DANVILLE  
ADD	415-578; SAN MATEO 
DEL	415-642; BERKELEY    
DEL	415-643; BERKELEY    
DEL	415-644; BERKELEY    
DEL	415-645; OAKLAND     
DEL	415-649; BERKELEY    
DEL	415-652; OAKLAND     
DEL	415-653; OAKLAND     
DEL	415-654; OAKLAND     
DEL	415-655; OAKLAND     
DEL	415-658; OAKLAND     
ADD	415-667; OAKLAND   
ADD	415-678; OAKLAND   
ADD	415-683; FREMONT   
ADD	415-691; MOUNTAINVW
ADD	415-696; SAN MATEO 
ADD	415-723; PALO ALTO 
ADD	415-725; PALO ALTO 
ADD	415-732; HAYWARD   
ADD	415-745; FRMT NWRK 
DEL	415-748; ALAMEDA     
DEL	415-762; OAKLAND     
DEL	415-763; OAKLAND     
DEL	415-769; ALAMEDA     
DEL	415-832; OAKLAND     
DEL	415-834; OAKLAND     
DEL	415-835; OAKLAND     
DEL	415-836; OAKLAND     
DEL	415-839; OAKLAND     
DEL	415-840; OAKLAND     
DEL	415-841; BERKELEY    
DEL	415-843; BERKELEY    
DEL	415-845; BERKELEY    
DEL	415-848; BERKELEY    
DEL	415-849; BERKELEY    
DEL	415-860; BERKELEY    
DEL	415-865; ALAMEDA     
DEL	415-869; ALAMEDA     
DEL	415-874; OAKLAND     
DEL	415-891; OAKLAND     
DEL	415-893; OAKLAND     
Received: from tymix.Tymnet by B39.Tymnet; Thu, 10 Sep 87 14:21:32 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07377; Thu, 10 Sep 87 14:22:21 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01475; Thu, 10 Sep 87
	14:22:17 PDT
Return-path: <dave@hobbes> 
From: dave@hobbes (Dave Smith) 
Date: Thu, 10 Sep 87 14:22:17 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8709102122.AA01475@hobbes.Tymnet.com> 
Subject: Re: Clock Wrapping and MCC68k 

Bill,
The problem is NOT a compiler bug; the compiler should do what it does in
most instances, since that will give the correct result for an arithmetic
compare even if there is an overflow.  The problem is that a comparison of
a clock is not strictly an arithmetic or a logical comparison.
Your first "work around" will work only if the compiler generates a MOVE
instruction to save the result of the subtract; a smart compiler (I'm not
sure if ours is smart enough) might assign the variable CompareKludge to
the same register that the subtract is done in and no MOVE would be
necessary.
Your second "work around" requires that you know the length of the variables
being compared to get the right value of SignBit.  Using a subroutine
eliminates this problem (at least for char, short, int, & long).  It
also makes it very explicit that an unusual type of comparison is being
performed.
Dave.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 10 Sep 87 14:24:40 PDT 
To: <dave@hobbes> 
Subject: Re: Clock Wrapping and MCC68k 
In-reply-to: <8709102122.AA01475@hobbes.Tymnet.com> of Thu, 10 Sep 87 14:22:17
	PDT

I agree that (a < b) behaving as indicated is not a bug, however, I was
refering to ((a - b) < 0) being a bug as indicated.  You might argue that
in the case of overflow, the results need not be defined, but when you
add the variable to save the partial result, ((c = a - b) < 0), it is
very clear that this should produce the same result (no matter what) as
the case without the c =, which it does not.

I agree my suggested work arounds are obscure, but the case
	(kludge = a - b) < 0
should always work assuming that MICROTEC does not change their optomizer
without fixing the bug first.  A hypothetical other compiler can be
assumed to not have the bug so having the register "assigned" would not
matter.  That beside the point, since I went and looked up my bug
report, I remembered a still better work around:
	long zero = 0L;
	... (a - b) < zero ...

The subroutine call is clearly the cleanest style, but it is relatively
expensive and since this is usually a problem for clocks and is running
at interrupt level, that may be a concern.  Perhaps a macro could give
the best of both?

-Bill

PS - I suppose we should finalize on something so if you want to publish
a summary to the mailing list, feel free to do so and I'll shut up.  Also
below is a copy of my bug report to MICROTEC (and SUN).

wrs@hobbes% cat bug.c
/*
 * Incorrect and inconsistent results are produced when the value of an
 * expression whose evaluation causes overflow is compared with zero as
 * demonstrated by the following program.  The generated code sequence is
 *	move.l	_a,d0
 *	sub.l	_b,d0
 *	bge	label
 * but should be either
 *	move.l	_a,d0
 *	sub.l	_b,d0
 *	bpl	label
 * or
 *	move.l	_a,d0
 *	sub.l	_b,d0
 *	cmp.l	d0,#0
 *	bge	label
 * of which the first form is obviously prefered.  The code generator
 * appearently assumes that
 *	((a - b) < 0) == (a < b)
 * which is only true when -infinity <= (a - b) <= +infinity and therefore
 * does not cause overflow.
 */

int             a = 0x7fffffff,
		b = 0x80000001,
		c,
		zero = 0;

#define show(x)	printf("%16s = 0x%08X\n", "x", x)
#define test(x)	printf("%16s = %s\n", "x", (x)?"TRUE":"FALSE")

void
main()
{
	show(a);
	show(b);
	show(zero);
	show(a - b);
	test(a < b);
	test((a - b) < 0);
	test((a - b) < zero);
	test((c = a - b) < 0);
}
wrs@hobbes% bug
               a = 0x7FFFFFFF
               b = 0x80000001
            zero = 0x00000000
           a - b = 0xFFFFFFFE
           a < b = FALSE
     (a - b) < 0 = FALSE
  (a - b) < zero = TRUE
 (c = a - b) < 0 = TRUE
wrs@hobbes%
Received: from tymix.Tymnet by B39.Tymnet; Fri, 11 Sep 87 9:09:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA10792; Fri, 11 Sep 87 09:10:18 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06680;
	Fri, 11 Sep 87 09:10:12 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11107; Fri, 11 Sep 87
	09:10:11 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Fri, 11 Sep 87 09:10:11 PDT 
To: wrs@calvin 
Message-id: <8709111610.AA11107@calvin.Tymnet.com> 
Subject: Meeting 

Bill,

  Did you get my mail regarding the meeting today? 10am in the
third floor small conference room regarding addressing?

Pat
Received: from F29.Tymnet by B39.Tymnet; Fri, 11 Sep 87 9:32:57 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 11 Sep 87 9:25:49 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 11 SEP 87 09:11:42 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I84175@Ontyme.Tymnet 
Subject: MOVE BY HTS, Q.A., STS 

                                     MEMORANDUM
                               NETWORK SYSTEMS COMPANY
                       QUALITY SERVICES AND TECHNICAL SUPPORT

          TO:       DISTRIBUTION        CC:  DON JOHNSON
          FROM:     GLORIA KOLEK
          DATE:     10 SEPTEMBER 1987
          SUBJECT:  MOVE BY HTS, Q.A., STS
          _________________________________________________________________

          DUE TO 'CIRCUMSTANCES BEYOND OUR CONTROL', OUR MOVE HAS BEEN
          DELAYED FOR ONE WEEK.  WE WERE ORIGINALLY SCHEDULED TO MOVE ON
          FRIDAY, 11 SEPTEMBER 1987.  NEW DATE IS TARGETED FOR 18/09/87.

          ANY MAIL BY U. S. POSTAL SERVICE OR PRIVATE CARRIER WILL BE DE-
          LIVERED TO NEW MAILING ADDRESS AND REROUTED TO ORCHARD PARKWAY.

          NEW ADDRESS FOR U.S. MAIL DELIVERY:
          McDONNELL DOUGLAS CENTER
          (DEPT OR EMPLOYEE NAME + MAIL STOP)  (HTS, Q.A., STS: M.S. F-2)
          2560 N. FIRST STREET
          P. O. BOX 49019
          SAN JOSE, CA 95161-9019

          NEW ADDRESS FOR PRIVATE DELIVERY SERVICE (UPS, FED-EX, DHL,ETC.)
          McDONNELL DOUGLAS CENTER
          (DEPT OR EMPLOYEE NAME + MAIL STOP)  (HTS, Q.A., STS: M.S. F-2)
          2560 N. FIRST STREET
          SAN JOSE, CA 95131

          OUR PRESENT TELEPHONE  NUMBERS  ARE  STILL  VALID  UNTIL  FURTHER
          NOTICE.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 11 Sep 87 16:23:32 PDT 
To: <driscoll@calvin> 
Subject: Re: Meeting 
In-reply-to: <8709111610.AA11107@calvin.Tymnet.com> of Fri, 11 Sep 87 09:10:11
	PDT

No, I didn't get your mail, but I see it in the log (Thu 10:06 AM).  Some
errant program on system 70 sent me slightly over 6 megabytes of garbage
yesterday and it probably got lost in the shuffle.  I tried to go through
the file with GREP to see if there were any legitimate messages
interspersed but I must have overlooked yours.  I did get a message from
Mark - at first glance I see no problem with his suggestion.

I've been trying to get caught up this week on the clock internal design
and have clearly underestimated the size of the task.  None the less it
will be done by Monday morning (barring natural disaster or nuclear war).
The implementation is probably overestimated so it may balance out in the
long run.

Have a nice weekend.
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 14 Sep 87 9:18:30 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18979; Mon, 14 Sep 87 09:19:50 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA17615;
	Mon, 14 Sep 87 09:19:44 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA02997; Mon, 14 Sep 87
	09:19:45 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Mon, 14 Sep 87 09:19:45 PDT 
To: wrs@calvin 
Message-id: <8709141619.AA02997@calvin.Tymnet.com> 
Subject: Boot review 

Bill,

  Mike has updated design documents and will be ready for a code
walk-through this week.  Would you review the updated design
document?  I'll let you know when the code will be ready.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Mon, 14 Sep 87 9:22:16 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18985; Mon, 14 Sep 87 09:23:27 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA17656;
	Mon, 14 Sep 87 09:23:20 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA03015; Mon, 14 Sep 87
	09:23:21 PDT
Return-path: <akselrod@calvin> 
From: akselrod@calvin (Mark Akselrod) 
Date: Mon, 14 Sep 87 09:23:21 PDT 
To: wrs@calvin 
Message-id: <8709141623.AA03015@calvin.Tymnet.com> 
Subject: Public addresses 

Hello, Bill.
We had a meeting on Friday regarding the hardware change and you proposals
on the public addresses. I also sent you a mail on Friday, but since I
have no response it sounds like it did not reach you. Please, disregard
this mail if you got the first one.

What I whould like to suggest instead of your proposal is to have our
pulic address equal to Tadpole physical (as per recently received
documentation). Notice, this is not the proposal to do away with the
concept of the public address, just make it equal to Tadpole physical
for Release I. This may have some advantages. For example, the monitor
should work since RAM is not relocated this way. Anything else in Rodney
related to pre-MMU RAM usage could also be simplified.

At any rate, I would like to understand the reasons why a lot of things
should be relocated if my proposal is unacceptable. Before we had
problems with the location of Plessey VME extended and 1M resolution
requirement. Since both went away at the moment, I do not see why the
relocation is necessary or what could be the advantage of doing that.

      Best regards
               Mark Akselrod
Received: from tymix.Tymnet by B39.Tymnet; Mon, 14 Sep 87 15:38:28 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19942; Mon, 14 Sep 87 15:39:45 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA22213; Mon,
	14 Sep 87 15:39:36 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA09354; Mon, 14 Sep 87 15:38:52 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Mon, 14 Sep 87 15:38:52 PDT 
To: wrs@f104 
Message-id: <8709142238.AA09354@f104.sun.com> 
Subject: stack size in k_sprout 

How can I estimate the stack size needed by my background process?  Is
there a compile option that will tell me this?

Ron


Received: from tymix.Tymnet by B39.Tymnet; Mon, 14 Sep 87 15:39:40 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19947; Mon, 14 Sep 87 15:40:52 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA22451; Mon, 14 Sep 87
	15:40:41 PDT
Return-path: <tfok@hobbes> 
From: tfok@hobbes (Thomas Fok) 
Date: Mon, 14 Sep 87 15:40:41 PDT 
To: wrs@hobbes 
Cc: driscoll@hobbes 
Message-id: <8709142240.AA22451@hobbes.Tymnet.com> 
Subject: Question I have 


Bill, I have the following questions:

(1) Is it necesaary to do a RETRY in handling bus error routine ?

    I have been questioning Mike Mo whether GP will get a bus error when

    it wants to talk to another GP, while the bus is busy ( or saturated ).

    His answer is NO, and he also get a confirmation from the vendor.

    So, why do we need to do a retry ?  
    
    If the device is not there ( or hardware/memory problems ) which causes 
    
    the bus error, retry is not going to help. If the bus error happens to be 
    
    a VME data reference, we may start ACCUSSING the non-responding GP/IOCP.
    
(2) The interface to your bus error handling routines is not clear to me.

    Need to discuss this more.

    - rkBerrBegin
    - rkBerrEnd
    - BusError


(3) The final MEMORY/IO map for RODNEY/ISIS.





Received: from tymix.Tymnet by B39.Tymnet; Mon, 14 Sep 87 17:40:24 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA20167; Mon, 14 Sep 87 17:41:43 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23429;
	Mon, 14 Sep 87 17:41:33 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA08701; Mon, 14 Sep 87
	17:41:32 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 14 Sep 87 17:41:32 PDT 
To: wrs@calvin 
Message-id: <8709150041.AA08701@calvin.Tymnet.com> 
Subject: "Bill, any suggestions about the"... 

Bill,
any suggestions about the recommended values for stack_size
in k_sprout() ?  this is the very last item for me to take care of
before trying the lint.
also do you know anything about the so called naive-C-checker ?
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 14 Sep 87 18:39:32 PDT 
To: <rmurphy@f104> 
Subject: Re: stack size in k_sprout 
In-reply-to: <8709142238.AA09354@f104.sun.com> of Mon, 14 Sep 87 15:38:52 PDT

There is no good way, the calls program helps you do it by hand by
showing you how things nest.  Its in ~wrs/bin and there is a manual
in ~wrs/man (put it in your MANPATH and do a man calls).  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 14 Sep 87 19:22:19 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA20318; Mon, 14 Sep 87 19:23:39 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23732; Mon, 14 Sep 87
	19:23:31 PDT
Return-path: <bechlyt@hobbes> 
From: bechlyt@hobbes (Tom Bechly) 
Date: Mon, 14 Sep 87 19:23:31 PDT 
To: wrs@hobbes 
Message-id: <8709150223.AA23732@hobbes.Tymnet.com> 
Subject: board name 

How is the GP boardname defined.  The Rodney Arch spec has it as
CPP0, CPP1, etc., this doesn't look right.

thanks,

tom
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Sep 87 5:12:08 PDT 
To: <tfok@hobbes> 
Cc: driscoll@hobbes 
Subject: Re: Question I have 
In-reply-to: <8709142240.AA22451@hobbes.Tymnet.com> of Mon, 14 Sep 87 15:40:41
	PDT

Okay - how about immediately after lunch today?  That way that's been going
I'd suggest 2:00, my office?  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Sep 87 5:14:41 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, any suggestions about the"... 
In-reply-to: <8709150041.AA08701@calvin.Tymnet.com> of Mon, 14 Sep 87 17:41:32
	PDT

Stack size is a problem.  Only you can determine that.  I suggest using the
program ~wrs/bin/calls to analize the nesting of your code and then go in
and add up (by hand) the sizes of variables and stuff allocated by each fn.
Come by in the afternoon and I can shed some more light on this terribly
painful subject.  I have a 2:00 so 3 or 3:30 should be good.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Sep 87 5:17:17 PDT 
To: <bechlyt@hobbes> 
Subject: Re: board name 
In-reply-to: <8709150223.AA23732@hobbes.Tymnet.com> of Mon, 14 Sep 87 19:23:31
	PDT

That has not been speced.  I have no good opinion, all I can suggest is
GP00, GP02, ... GP99, any suggestions?  What does Pat think?  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Sep 87 22:54:59 PDT 
To: <gauci@hobbes> 
Subject: Re: testing simulations 
In-reply-to: <8709151806.AA26109@hobbes.Tymnet.com> of Tue, 15 Sep 87 11:06:36
	PDT

See Yu-Liang Hsing for IPCF testbed on Unix.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Wed, 16 Sep 87 14:44:38 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 16 Sep 87 14:44:24 PDT
Return-path: <QSATS.D/ELLENBERG@EMSNTD.Ontyme.Tymnet> 
From: QSATS.D/ELLENBERG@EMSNTD.Ontyme.Tymnet 
Date: 16 SEP 87 14:18:03 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: M19398@Ontyme.Tymnet 
Subject: Proposal 

            PROPOSAL FOR PROVIDING ENHANCED SECURITY IN THE
            -----------------------------------------------

     STRUCTURE AND ACCESS CAPABILITIES OF DIRECTORIES ON THE PDP 10
     --------------------------------------------------------------

                          CODE GENNING SYSTEMS
                          --------------------






















|                           September 3, 1987

                                   by

                          Douglas G. Ellenberg













                | - Denotes changes from last document.


Contributions:

Individuals who contributed to the document and their organizations.


Quality Assurance    Hardware Technical Support    Network Technology Division

 Diana Houge             Ed Mooring                     Romolo Raffo
|                        Paul Krumviede                 William Soley
                         Ron Vivier


Security             Field Service Company

Wayne Bartlett           Dennis Coffey
                         Carl Baltrunas








Overview:

Security is a major concern in all areas of the Network.  From
restricting access to the Network to restricting the range of
capabilities a user has once a successful connection is made to a
host, security is a concern.  This proposal seeks to address one part
of the security picture.

Today, all users on the PDP Code Genning Systems have the capability to
| access the NAD program in the (SYS) directory.  This program has read
| file license (RF) assigned to it.  With RF license, any user has the
| capability of accessing any file on the PDP through the NAD program.

| What this proposal seeks to control is really two fold.  First, it will
| restrict which users are allowed to use the NAD program.  This will
| restrict users without RF license from accessing files on the PDP they
| should not access.  Secondly, users who are allowed to access the NAD
| program and gen code but should not create code listing will be
| restricted from performing the listing function.


Goals and Compromises:

The ultimate goal for the implementation of restricted access was to
disallow access to the (SOURCE) and (BETATEST) directories from all
users except those that require access.  Those requiring access include
all field and Headquarters support people; Project Implementation; the
Field Service Organization; Customers who, by contract, are allowed
access; the Headquarters Quality Assurance group; and the development
organization.  Groups that required access only to a limited set of
files in the (SOURCE) and (BETATEST) directories would then be
restricted on a file by file basis.  For example, Customer A has in
their contract a license to gen X.25, Node Code and ISIS.  They would be
allowed access to only the source for this code and be barred access to
the remaining source code files.

Unfortunately, barring access down to a per file per user basis is not
possible under current TYMCOM-X limitations.  This capability would
require modifications to the Operating System.  What is possible is to
restrict access to the (SOURCE) and (BETATEST) directories as a whole
and restrict access to some of the capabilities of the NAD program. 
Thus, restricting access on a more limited basis is possible.

After the restructuring of the PDP systems, as will be documented,
restriction to the (SOURCE) and (BETATEST) directories will be on
a per user basis.  Certain users will be barred from being able to
create listings of the source code and will be able to only write files
into their own directories.  They will not be able to over-write files
| of any other users and will be restricted from accessing files that are
| protected and not in the (SOURCE) or (BETATEST) directories.



Restructuring with Restrictions:

There are copies of the code genning program, NAD, on all PDP Code Genning
Systems.  One NAD is the aforementioned copy in the (SYS) directory.
| The second NAD is the copy in the (SOURCE) directory.  Both of these
| copies are identical.  They are both assigned Read File license.

There is also an important feature of NAD that is part of this
restructuring.  If a user lacks TYM license, NAD will not allow that
user to create listings of the source code.  Currently, most users that
gen code have TYM license.  They are able to both gen code and create
listings of the source code.

The changes proposed to create tighter controls on the capabilities of
users on the PDP Code Genning Systems are:

| 1.  Make modifications to the NAD program so the NAD program in the
|     (SYS) directory will only be a verification program for access
|     control.  The NAD program in (SYS) will verify the user attempting
|     to call the NAD program is validated on a GOODGUY list.  If valid,
|     the user will be routed to the real NAD program in the (SOURCE)
|     directory.  The (SYS)NAD program will be given ALL RD RD
|     restrictions.  (NOTE:  This is a common practice for programs on the
|     PDP's today.)
| 
| 2.  Place protections on NAD in the (SOURCE) directory so it can not be
|     called directly unless the user can override the restrictions with
|     their own license.  This is will prevent users (such as customers)
|     without proper license from accessing the (SOURCE) NAD program
|     directly.
| 
| 3.  Modify the NAD program in the (SOURCE) directory to set Read File
|     license selectively.  When running NAD, the NAD program will set
|     Read File license if it is attempting to read a file in the
|     (SOURCE) or (BETATEST) directories.  If it is instructed to read a
|     file in any other directory, it will not set Read File license.
|     This will require any file that needs to be read by NAD to have the
|     proper protection to allow it to be read by all users.  This
|     includes files such as patches in other directories or special
|     source files in other directories.  The purpose of this is to not
|     allow users to use the Read File license of NAD to access files they
|     should not have access to.
| 
| 4.  Each customer will be placed in their own GAN.  This will restrict
|     them from being able to look at files in any directory but ones in
|     their own GAN.

| 5.  Remove TYM license from users who should not be able to create
|     listings of the source code.
| 
| 6.  The (SOURCE) and (BETATEST) directories should have directory access
|     protection of  SHARABLE and LISTABLE only.  They currently have
|     SHARABLE, LISTABLE and NEW FILES. 



| When these changes are complete, users not on the NAD GOODGUY list and
| without proper license will not have access to the NAD program.  Without
| the TYM license, they would be unable to create listings of the source
code.  Without the additional licenses on the NAD program they would be
unable to over-write files not in their own directory.  With directory 
access set, users will be prevented from writing into the (SOURCE) and
(BETATEST) directories.

With the current protections placed on all (SOURCE) and (BETATEST)
source code, users without the proper license will not have access to
the files in these directories.  They would be restricted from even
viewing the source files.

| Without proper license, users will not be able to view other users files
| in directories under different GANs.


Concerns and Questions:

PEAK and VUE, the text editors on the PDP's, require modification if they
are to be used by users that do not have TYM license.

Who will be tracking the change of GANs for the customers?

Who will identify the customers requiring these changes?

| An access list will have to be maintained for the (SYS)NAD program.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 16 Sep 87 15:33:08 PDT 
To: <JMS@X32.Tymnet> 
Subject: Re: "Two people have asked do you get"... 
In-reply-to: your message of Wed, 16 Sep 87 14:07:57 PDT

The project was canceled before anyone wrote TUMS.HLP, so...  there is
actually no documentation.  If he wants his Ontymes forwarded, he just
needs to send me his ontyme name, password and PDP-10 user name.  It is
somewhat expensive, so I don't want to do a whole bunch, but a few is OK.
There is no interface at this time to the 370 except through PROFS which
I think they are not running.  If he is in the MDC PROFS system, he can
send mail to the PDP-10 via PROFS by sending a NOTE (must be a note) to
the address TUMSGATE SLVMCL1 and including the real destination address
in the first line of the note as follows (for example):

To: Bill SOley <WRS@B39.Tymnet> (TUMS)

The "To:" must begin in column 1 and the (TUMS) must not have any
trailing blanks.  He can send to multiple addresses this way, by
just seperating them with commas or using multiple To: lines or both.
He can also leave out the name and <>...

To: WRS@B39.Tymnet (TUMS)

Oh well.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 16 Sep 87 15:50:34 PDT 
To: <QSATS.D/ELLENBERG@EMSNTD.Ontyme.Tymnet> 
Cc: Carl A. Baltrunas <Carl@X930.Tymnet>, Wayne D. Bartlett
	<BARTLETW@B39.Tymnet> 
Subject: Re: Proposal 
In-reply-to: M19398@Ontyme.Tymnet of 16 SEP 87 14:18:03

Doug, here is your document back.  It looks very good.  I've marked
a couple of things - some typeos and a couple clearifications.  All
my marks are enclosed in [].  Thanks -Bill

Received: from F29.Tymnet by B39.Tymnet; Wed, 16 Sep 87 14:44:38 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 16 Sep 87 14:44:24 PDT
Return-path: <QSATS.D/ELLENBERG@EMSNTD.Ontyme.Tymnet> 
From: QSATS.D/ELLENBERG@EMSNTD.Ontyme.Tymnet 
Date: 16 SEP 87 14:18:03 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: M19398@Ontyme.Tymnet 
Subject: Proposal 

            PROPOSAL FOR PROVIDING ENHANCED SECURITY IN THE
            -----------------------------------------------

     STRUCTURE AND ACCESS CAPABILITIES OF DIRECTORIES ON THE PDP 10
     --------------------------------------------------------------

                          CODE GENNING SYSTEMS
                          --------------------






















|                           September 3, 1987

                                   by

                          Douglas G. Ellenberg













                | - Denotes changes from last document.


Contributions:

Individuals who contributed to the document and their organizations.


Quality Assurance    Hardware Technical Support    Network Technology Division

 Diana Houge             Ed Mooring                     Romolo Raffo
|                        Paul Krumviede                 William Soley
                         Ron Vivier


Security             Field Service Company

Wayne Bartlett           Dennis Coffey
                         Carl Baltrunas








Overview:

Security is a major concern in all areas of the Network.  From
restricting access to the Network to restricting the range of
capabilities a user has once a successful connection is made to a
host, security is a concern.  This proposal seeks to address one part
of the security picture.

Today, all users on the PDP Code Genning Systems have the capability to
| access the NAD program in the (SYS) directory.  This program has read
| file license (RF) assigned to it.  With RF license, any user has the
| capability of accessing any file on the PDP through the NAD program.

| What this proposal seeks to control is really two fold.  First, it will
| restrict which users are allowed to use the NAD program.  This will
| restrict users without RF license from accessing files on the PDP they
| should not access.  Secondly, users who are allowed to access the NAD
| program and gen code but should not create code listing will be
| restricted from performing the listing function.

[Thirdly, those who do have access to NAD will only be able to assemble
files from (SOURCE), (BETATEST) or directories that they normally have
access to without license.]

Goals and Compromises:

The ultimate goal for the implementation of restricted access was to
disallow access to the (SOURCE) and (BETATEST) directories from all
users except those that require access.  Those requiring access include
all field and Headquarters support people; Project Implementation; the
Field Service Organization; Customers who, by contract, are allowed
access; the Headquarters Quality Assurance group; and the development
organization.  Groups that required access only to a limited set of
files in the (SOURCE) and (BETATEST) directories would then be
restricted on a file by file basis.  For example, Customer A has in
their contract a license to gen X.25, Node Code and ISIS.  They would be
allowed access to only the source for this code and be barred access to
the remaining source code files.

Unfortunately, barring access down to a per file per user basis is not
possible under current TYMCOM-X limitations.  This capability would
require modifications to the Operating System.  What is possible is to
restrict access to the (SOURCE) and (BETATEST) directories as a whole
and restrict access to some of the capabilities of the NAD program. 
Thus, restricting access on a more limited basis is possible.

After the restructuring of the PDP systems, as will be documented,
restriction to the (SOURCE) and (BETATEST) directories will be on
a per user basis.  Certain users will be barred from being able to
create listings of the source code and will be able to only write files
into their own directories.  They will not be able to over-write files
| of any other users and will be restricted from accessing files that are
| protected and not in the (SOURCE) or (BETATEST) directories.



Restructuring with Restrictions:

There are [two] copies of the code genning program, NAD, on all PDP Code
Genning Systems.  One NAD is the aforementioned copy in the (SYS) directory.
| The second NAD is the copy in the (SOURCE) directory.  Both of these
| copies are identical.  They are both assigned Read File license.

There is also an important feature of NAD that is part of this
restructuring.  If a user lacks TYM license, NAD will not allow that
user to create listings of the source code.  Currently, most users that
gen code have TYM license.  They are able to both gen code and create
listings of the source code.

The changes proposed to create tighter controls on the capabilities of
users on the PDP Code Genning Systems are:

| 1.  Make modifications to the NAD program so the NAD program in the
|     (SYS) directory will only be a verification program for access
|     control.  The NAD program in (SYS) will verify the user attempting
|     to call the NAD program is validated on a GOODGUY list.  If valid,
|     the user will be routed to the real NAD program in the (SOURCE)
|     directory.  The (SYS)NAD program will be given ALL [RUN RUN]
|     restrictions.  (NOTE:  This is a common practice for programs on the
|     PDP's today.)
| 
| 2.  Place protections on NAD in the (SOURCE) directory so it can not be
|     called directly unless the user can override the restrictions with
|     their own license.  This [] will prevent users (such as customers)
|     without proper license from accessing the (SOURCE) NAD program
|     directly.
| 
| 3.  Modify the NAD program in the (SOURCE) directory to set Read File
|     license selectively.  When running NAD, the NAD program will set
|     Read File license if it is attempting to read a file in the
|     (SOURCE) or (BETATEST) directories.  If it is instructed to read a
|     file in any other directory, it will not set Read File license.
|     This will require any file [not in SOURCE or BETATEST] that needs
|     to be read by NAD to have the
|     proper protection to allow it to be read by all users.  This
|     includes files such as patches in other directories or special
|     source files in other directories.  The purpose of this is to not
|     allow users to use the Read File license of NAD to access files they
|     should not have access to.
| 
| 4.  Each customer will be placed in their own GAN.  This will restrict
|     them from being able to look at files in any directory but ones in
|     their own GAN.

| 5.  Remove TYM license from users who should not be able to create
|     listings of the source code.
| 
| 6.  The (SOURCE) and (BETATEST) directories should have directory access
|     protection of  SHARABLE and LISTABLE only.  They currently have
|     SHARABLE, LISTABLE and NEW FILES. 



| When these changes are complete, users not on the NAD GOODGUY list and
| without proper license will not have access to the NAD program.  Without
| the TYM license, they would be unable to create listings of the source
code.  Without the additional licenses on the NAD program they would be
unable to over-write files not in their own directory.  With directory 
access set, users will be prevented from writing into the (SOURCE) and
(BETATEST) directories.

With the current protections placed on all (SOURCE) and (BETATEST)
source code, users without the proper license will not have access to
the files in these directories.  They would be restricted from even
viewing the source files.

| Without proper license, users will not be able to view other users files
| in directories under different GANs.


Concerns and Questions:

PEAK and VUE, the text editors on the PDP's, require modification if they
are to be used by users that do not have TYM license.

Who will be tracking the change of GANs for the customers?

Who will identify the customers requiring these changes?

| An access list will have to be maintained for the (SYS)NAD program.
Received: from tymix.Tymnet by B39.Tymnet; Thu, 17 Sep 87 19:43:24 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA10783; Thu, 17 Sep 87 19:47:42 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15687;
	Thu, 17 Sep 87 19:47:29 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04657; Thu, 17 Sep 87
	19:47:25 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Thu, 17 Sep 87 19:47:25 PDT 
To: wrs@calvin 
Message-id: <8709180247.AA04657@calvin.Tymnet.com> 
Subject: "Bill I got a C problem: when I"... 

Bill
I got a C problem:
when I declare - static char bb[20];
and later say bb[20] = "ABCD" the compiler does no like it.
I know it would probably prefer -
char *bb
and later - bb = "ABCD" but I need real string rather then pointers
since later I want to append something at the end of that string (add
a few characters).
what is a "safe" way to initialize a string?
Mike J. (thanks)
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 17 Sep 87 21:59:31 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill I got a C problem: when I"... 
In-reply-to: <8709180247.AA04657@calvin.Tymnet.com> of Thu, 17 Sep 87 19:47:25
	PDT

This is the hardest part of C to master, but once mastered, it is the
most powerful.  'C' works on data types.  There are three data types
relavent to our discussion:
	char	c;		/* character */
	char	*pc;		/* pointer to character */
	char	ac[10];		/* array of characters */
It is very important to understand their differences.  The character is
the simplest.  When you declare a character, the compiler allocates one
byte of memory and gives it the name declared.  In assembler, this would
look like:
	c:	dc.b	0
When you declare a pointer to a character, the compiler allocates a long
word of memory and gives it the name desired.  In assembler, this would
look like:
	pc:	dc.l	0
Finally, when you declare an array of characters, the compiler allocates
space for all the elements of the array, and gives the first element the
name desired.  In assembler, this would look like:
	ac:	ds.b	10

It gets complicated, but 'C' keeps track of the type of each identifier
in its symbol table, so whenever "c" appears in an expression, it knows
that it is a location in memory that contains a character.  Likewise, it
knows that "pc" is a location in memory that contains the address of a
location in memory that contains a character.  Finally, it knows that
"ac" is a location in memory that contains the first character of an
array of characters (but it doesn't keep track of how many).

Now, if you want to use "pc", you have to give it some value.  That all
depends on what you're using it for.  Remember that it's just a pointer
and it doesn't have in itself any space to store a character.  If you
want to, for example, build a string that contains the concatenation
of two strings, lets say "abcd" and "efgh", you might do this:

	char	buffer[9];	/* room for both strings plus a null */
	strcpy(buffer, "abcd");	/* copy abcd into buffer */
	strcat(buffer, "efgh");	/* concatenate efgh to the end */

Now, "abcd" is just like a character array that is initialized to
'a', 'b', 'c', 'd', '\0' at compile time.  In particular it is
a pointer to an area in ROM that contains the character 'a'.  In
the next location in ROM is 'b', then 'c', and so on.  So if you
had a pointer to the first character of an array that contained
one of the above strings, it could be used in the same way:

	char	abcd[] = "abcd";
	char	buffer[9];	/* room for both strings plus a null */
	strcpy(buffer, abcd);	/* copy abcd into buffer */
	strcat(buffer, "efgh");	/* concatenate efgh to the end */

Of course, when you do this, abcd[] is probably something that is
passed as an argument to a function or out of some data structure
or something.  I just used an initialized array for an example.
You must be careful to make sure there is enough space for the string
that is being copied in.  For example, in the above example, if abcd
actually pointed to a string of length 5, lets say "abcdx", then you
would end up with buffer containing 'a', 'b', 'c', 'd', 'x', 'e',
'f', 'g', 'h' and the null would be stored in whatever variable happened
to be allocated next in memory (you can't predict).  This is a very
common bug and you need to be very careful.  "C" does not ever do any
array bounds checking.  

Getting more to your message:
----------------------------------------------------------------
>  when I declare - static char bb[20];
>  and later say bb[20] = "ABCD" the compiler does no like it.

I don't blame it, "bb" is an array of characters, "bb[20]" is
the character after the last character of the array (illegal),
and '"ABCD"' is a pointer to a character.  So you're trying to
assign a pointer to a character - type mismatch!

>  I know it would probably prefer -
>  char *bb
>  and later - bb = "ABCD" but I need real string rather then pointers
>  since later I want to append something at the end of that string (add
>  a few characters).

Well, this is syntactically correct, but if you try to append to it
you'll actually be appending to the constant string "ABCD" which is
stored in ROM so that won't work.  I think what you want is

	char	buffer[123];
	strcpy(buffer, "ABCD");
	strcat(buffer, whatever);

"C" Won't copy strings (structs, yes, but not strings)  You have to
write a loop or use a library sunction like strcat.  See the Microtec
manual for a writeup of strcpy, strcat, etc.

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 18 Sep 87 0:32:43 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12876; Fri, 18 Sep 87 00:37:09 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16532;
	Fri, 18 Sep 87 00:36:57 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04943; Fri, 18 Sep 87
	00:36:52 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 18 Sep 87 00:36:52 PDT 
To: wrs@calvin 
Message-id: <8709180736.AA04943@calvin.Tymnet.com> 
Subject: "Bill, thanks for your very thorough"... 

Bill, thanks for your very thorough explanation ! Good-night Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 18 Sep 87 0:45:03 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12936; Fri, 18 Sep 87 00:49:29 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16539;
	Fri, 18 Sep 87 00:49:17 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04950; Fri, 18 Sep 87
	00:49:13 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 18 Sep 87 00:49:13 PDT 
To: wrs@calvin 
Message-id: <8709180749.AA04950@calvin.Tymnet.com> 
Subject: "by the way, I have seen examples"... 

by the way, I have seen examples of declarations like that:
static char qq[10] = "hello !"
so I reckoned the same can be done in the flow of the program - and
this is apparently not so. (page 387 "C primer"). so what such a
declaration actually does - loads array or initialize pointer ?
a-bit-confused.....Mike J.
Received: from F29.Tymnet by B39.Tymnet; Fri, 18 Sep 87 10:19:53 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 18 Sep 87 10:23:33 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 18 SEP 87 10:11:47 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I87227@Ontyme.Tymnet 
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
Received: from F29.Tymnet by B39.Tymnet; Fri, 18 Sep 87 15:07:45 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 18 Sep 87 15:10:36 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 18 SEP 87 15:08:07 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I87451@Ontyme.Tymnet 
Subject: "MCDONNELL DOUGLAS NETWORK SYSTEMS"... 



                             MCDONNELL DOUGLAS
                          NETWORK SYSTEMS COMPANY
                          INTEROFFICE MEMORANDUM



     TO:       ALL MDC CORP AMERICAN EXPRESS CARD HOLDERS

     FROM:     LINDA SERVELLO

     SUBJECT:  ISSUANCE OF NEW AMERICAN EXPRESS CARDS
                                                                     



     WE ARE IN THE PROCESS OF CONSOLIDATING OUR AMERICAN EXPRESS
     CARDS INTO ONE BILLING CYCLE.  SOME EMPLOYEES WILL BE RECEIVING
     A NEW AMERICAN EXPRESS CARD IN THE NEAR FUTURE.

     UNTIL THEN PLEASE CONTINUE TO USE YOUR CURRENT CARD AS NORMAL.
     PLEASE DISREGARD ANY MESSAGES ON YOUR MONTHLY STATEMENTS
     REGARDING CANCELLATION OF YOUR CARD.  YOUR CARD HAS NOT BEEN
     CANCELLED.

     IF YOU HAVE ANY QUESTIONS, PLEASE CALL PAM SUTER AT (408)
     922-6617.  WE ARE SORRY FOR ANY INCONVENIENCE YOU MAY HAVE BEEN
     CAUSED.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 18 Sep 87 15:31:47 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18252; Fri, 18 Sep 87 15:36:11 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA20394;
	Fri, 18 Sep 87 15:35:56 PDT
Received: from comet.Tymnet.com by calvin.Tymnet.com (3.2/SMI-3.2) id AA04696;
	Fri, 18 Sep 87 15:35:50 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA00844; Fri, 18 Sep 87 15:36:03
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Fri, 18 Sep 87 15:36:03 PDT 
To: wrs@calvin 
Message-id: <8709182236.AA00844@comet.Tymnet.com> 
Subject: Re: /usr/adm/wtmp munged! 

last is upset because wtmp isn't a multiple of 36 bytes large, so the
seeks to find beginnings of records didn't.  Probably it got
munged a while ago, or during the crash around noon.
I cleared it.
	Steve
Received: from tymix.Tymnet by B39.Tymnet; Fri, 18 Sep 87 15:35:39 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18273; Fri, 18 Sep 87 15:40:05 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA20444;
	Fri, 18 Sep 87 15:39:51 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04770; Fri, 18 Sep 87
	15:39:44 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 18 Sep 87 15:39:44 PDT 
To: wrs@calvin 
Message-id: <8709182239.AA04770@calvin.Tymnet.com> 
Subject: "Bill, another headache: copyhandle"... 

Bill,
another headache:

copyhandle = (struct copy) handle;

where copyhandle is a structure of type copy and handle is structure
Ipcfhandle- does not compile.
??
Mike J.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 18 Sep 87 15:41:45 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, another headache: copyhandle"... 
In-reply-to: <8709182239.AA04770@calvin.Tymnet.com> of Fri, 18 Sep 87 15:39:44
	PDT

Both structures have to be the same type.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 18 Sep 87 15:43:23 PDT 
To: <mjastrz@calvin> 
Subject: Re: "by the way, I have seen examples"... 
In-reply-to: <8709180749.AA04950@calvin.Tymnet.com> of Fri, 18 Sep 87 00:49:13
	PDT

char qq[10] = "hello"; 
is a kludge to make life easier ... it really does
char qq[10] = {'h', 'e', 'l', 'l', 'o', '\0'};
however,
char *qq = "hello";
initializes qq with the address of 'h' 'e' ... 'o' '\0'

BEWARE though that initialized anything will go in ROM so if you do
	char qq[10] = "hello";
and then try to append to it with
	strcat(qq, " there");
you'll just get a bus error.

-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 1 Jul 87 14:32:54 PDT 
To: driscoll@calvin 
Subject: module symbols 

/*
 * Pat- Please review before I distribute. -Bill
 */

Naming symbols inside Rodney should follow some agreed upon convention to
make things more consistent and less confusing.  The following is favored
by Pat and myself:

System calls begin with some functional group identifier as a prefix, but
do not have an underscore, for example:
	slgReport, ipcfEnter, symEnter, etc.

Symbols which are internal to a module (and declared static) can be named
as appropriate.  Symbols which are global to Rodney, but not available to
the patron, should be named with a short identifer of the module, followed
by an underscore, for example:
	k_reset, l_crash, s_zap, etc.
The modules would be:
	a_	agent
	b_	boot
	i_	ipcf
	k_	kernel
	l_	logger
	s_	session
	ss_	skysap
One character should be used unless it conflicts or is confusing.  The
first character following the _ is capitalized as if the prefix was not
present, for example:
	int k_scaler;
	struct k_Structure
	#define k_CONSTANT

Things which transcend modules or are frequently used and well known need
not have a module prefix, for example:
	glob, RERR_FOO, etc.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 2 Jul 87 18:14:49 PDT 
To: NTD.A/FENN@Ontyme 
Cc: NTD.D/ALCORN@Ontyme, NTD.W/EUSKE@Ontyme, NTD.K/HOLCOMB@Ontyme,
	NTD.M/RUDE@Ontyme, coons@opus, driscoll@calvin, dlasater@b39,
	seltzer@mac 
Subject: Sun Unix documentation 

Do you think having complete documentation about the Unix tools and
environment readily available at one's fingertips might improve a
programmer's productivity by ... say four tenths of one percent?

It is certainly not hard to imagine that such a resource would be able
to save the average programmer 9.6 minutes per week.  In addition to
reducing unproductive time spent wandering about looking for the right
document, usually interrupting other people to ask questions, there is
also the less tangible but more important aspect of producing higher
quality work and reducing support costs.

I suggest we order a complete Sun manual for each programmer.  The
cost may seem high, but it is clearly recovered in under a year.  In
the Skywalker project many programmers are using Unix for the first
time and I believe their learning curve to be a significant risk to
meeting the schedule.  Readily available documentation will reduce
that risk.

-Bill

0.4% of $50,000 (fully burdened cost) = $200 (per manual set)
0.4% of 2080 hours/year = 9.6 minutes/week
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 5 Jul 87 16:25:10 PDT 
To: submit 
Subject: Hold-Restart 

So, what actually causes "Hold-Restart"?  How do I find out what went wrong?
How do I make a job automatically restart upon any failure?

Then manual says Hold-Restart "job aborted with restart option enabled".
This does not tell me anything useful.  I found that the MAILER job had
gone into this state on system 35, and apearently intended to stay there
forever and never retry.  This is VERY UN-GOOD!  Please help resolve this.
-Bill
Received: from D65.Tymnet by B39.Tymnet; Wed, 8 Jul 87 12:26:37 PDT
Return-path: <STHOMPSON@d65.Tymnet> 
From: SUSAN THOMPSON <STHOMPSON@d65.Tymnet> 
Date: Wed, 8 Jul 87 12:29:25 PDT 
To: Mail Wizard <MAIL> 
Subject: TUMS-10 bug report (v45000000 of 23-MAY-87) 

HELLLLLLLP--For some reason, I keep getting the same 50-odd messages
over and over again since yesterday.  I was on vacation 06/19--07/06 and
only read my tums yesterday.

I keep getting these messages even if I delete my tums.msg and tums.tix.

Please let me know what can be done about this.  Thank you, Sue.


From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 6:21:48 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 5 Jul 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

45	16	1915	1104

46	16	1915	1104

20	2

31	4

32	8

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 9 Jul 87 19:12:20 PDT 
To: slinf@tmstlx 
Subject: addresses 

@@A001 CAMPBELL CA
TLX 714993999 EGELECTR
@@BCrabb, Dennis WB0GCI
115 N 14th St
Denison, Ia 51442

Maurer, John NA0S
RR#1
Ames, Ia 50010

Long, Scott WD6NSD
28-A S. Westmoor
Newark, OH 43055

Grant, Gregory M. N8FSP
262 W. Pleasent St.
Hillsboro, OH 45133

Nations, Daryl
Sandlapper
4184 Fernandina Rd
Columbia, SC 29210

Lagan, J. Edmund
c/o Nutec, Inc.
620 Comet Dr
Nashville, Tn 37209

Andrewjeski, Dean AD9W
Rib Mountain Repeater Assn
P.O.Box 323
Wausau, Wi 54401

Jordan, William F., Ph.D., C.P.A., AE4S
422 North Ride
Tallahassee, Fl 32303

APW Electronics, Ltd.
777 St. Clair St
Chatham, Ontario N7M5S7

I-40 Comm, Attn: Kim
2805 So Service Rd
West Memphis, Ar 72301
501-732-3327

Hazelton, Gary
67332 32nd St
Lawton, Mi 49065

Strones, Mike
10 Sorrento Rd
Wallingford, Ct

Schizdknecht, Ray
120 Undercliff Rd
Montclair, NJ 07042

McGarvey, Wm.
734 E Roger Rd #253
Tuscon, Az 85719

Leal, Bill
2297 Parkwood
Windsor, Ontario N8W2K1

Joyner, Reece
610 Knipp Av
Mascoutah, Il 62258-1309

Salemme, Vince KB6B^@
326 Michell Ct
Livermore, Ca 94550

Ahrens, Rich W3WJL
771 Beach St
Reading, Pa 19605

Cox, Edward W.
222 Shady Hollow
Casselberry, Fl 32707

Leuit, Mark
P.O.Box 438
Hughsowville, NY 12537-0438

Born, Mike
535 Ramblewood Ct
Lake Zurich, Il 60047

Rowland, Keith N8GQV
Bald Mountain Wireless
398 W Iroquois
Pontiac, Mi 48053
313-338-8247

Ligon, Ronny
11212 Salt Cedar
Austin, Tx 78750
512-250-1458

Colvett, Dave WB4EDS
P.O.Box 193
Alamo, Tn 38001
901-696-2580

Stafford, Rod KB6ZV
5155 Shadow Estates
San Jose, Ca 95135

Kadvan, Andy KA8R
1835 Lynn Mar Av
Poland, Oh 44514

Begolka, Donald N9EZJ
RR#1 Box 236
Atlanta, Il 61723

Gooch, Jay W9YH
E.E.Building VI
Urbana, Il 61801

Slupko, Tony K6ELQ
Sulphur Mtn Rpt Assoc
c/o K6VK (Bob) - see call book

Whitehouse, Glen KI6W
1 Pinnacle Rd
Amherst, NH 03031

Vrooman, Larry WB9DFD
3513 N Linden
Muncie, In 47304
Received: from tymix.Tymnet by B39.Tymnet; Fri, 10 Jul 87 12:54:32 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA28250; Fri, 10 Jul 87 12:39:48 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA00755; Fri, 10 Jul 87
	12:39:55 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Fri, 10 Jul 87 12:39:55 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707101939.AA00755@calvin.Tymnet.com> 
Subject: Re: boot.mm 

Bill,

  Good luck on the delivery.  There are no words sufficient to
describe the event.

  On checking pointers, if an MMU violation occurs because the
Patron gave a Rodney process a bad pointer, will the Rodney
process be able to recover and return a Rodney error code?
How will the MMU error handling code know that a process
was using a tainted pointer provided by some other process
as opposed to corrupting it's own pointer?

  Your comments on crashing on detecting internal data structure
inconsistencies make sense.  My comment about checking provided
pointers follows an additional philosophy, i.e. never (almost)
crashing on external error events, such as protocol violations,
bad function call codes or parameters, etc.

  Off of my soapbox now.

Pat
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 11 Jul 87 5:11:54 PDT 
To: wrs/baby 
Subject: William Christian Soley 

The latest addition to the Soley family: William Christian Soley
Born at home, 11-Jul-87 02:10 PDT, 9.25 lbs.
Both Becky and the baby are in excellent health.

-Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 16:20:06 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05275; Mon, 13 Jul 87 15:30:52 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA03566; Mon, 13 Jul 87 15:30:25
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Mon, 13 Jul 87 15:30:25 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707132230.AA03566@comet.Tymnet.com> 
Subject: Re: Lynn Gold 

i think i saw some netnews from her once at figmo@ingres.berkeley.edu
	steve
Received: from tymix.Tymnet by B39.Tymnet; Mon, 13 Jul 87 16:20:39 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05151; Mon, 13 Jul 87 15:01:31 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16588; Mon,
	13 Jul 87 15:00:21 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05144; Mon, 13 Jul 87 15:01:24 PDT
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA29484; Mon,
	13 Jul 87 14:01:18 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA11632; Mon,
	13 Jul 87 14:01:13 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Mon, 13 Jul 87 14:01:13 PDT 
To: oliveb!tymix!wrs 
Message-id: <8707132101.AA11632@oliven.ATC.OLIVETTI.COM> 
Subject: mark's address 


I see an address - Crispin@SUMEX-AIM.STANFORD.EDU
		or MRC@PANDA.COM

Hope this helps...

Gary
Received: from tymix.Tymnet by B39.Tymnet; Tue, 14 Jul 87 8:18:56 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA08273; Tue, 14 Jul 87 08:23:24 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11859; Tue, 14 Jul 87
	08:23:01 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Tue, 14 Jul 87 08:23:01 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707141523.AA11859@calvin.Tymnet.com> 
Subject: Re: File Design review 

Bill,

We'll proceed with the design review. Is there a possibility of getting
some time with you this week? I'd like to publish the updated RED. Are
your sections up to date?

Welcome to the world of parenting.

Pat
Received: from F29.Tymnet by B39.Tymnet; Tue, 14 Jul 87 9:42:59 PDT
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Tue, 14 Jul 87 9:47:01 PDT
Return-path: <M108013@SLVMCL1.McAuto.Tymnet> 
From: Carol Block <M108013@SLVMCL1.McAuto.Tymnet> 
Date: 07/14/87 11:46:52 
To: WRS@C39.TYMNET 
Subject: TUMSGATE Problems 

  Bill I the messges you sent me indicate that the 191 disk was having
problems which I think I caused. I was trying to look at the TUMSGATE NETLOG to
determine how many copies of a memo was being sent out through TUMSGSGATE. The
file is A0 so I linked it MW, which has been ok in the past if done VERY
carefully. It certainly is not a recommended procedure and we do it only when
we are sure that we do not make changes on the disk at the same time the other
user is writing to the disk. I suspect since the advent of Release 4 of CMS
that the "rules are changed" making this an absolute no no since the log file
was messed up. I renamed it yesterday and restarted a new file but have not
dared to look to see if everything is back to normal. Could look at the file
and let me know if it is functioning properly again?
  Is there a problem making the NETLOG an A1 file for debugging purposes? Also
wondered if you could give me a "safe" way of interrupting TUMSGATE and
looking for problems. How could I determine from this side if the id is busy
handling mail going either direction? That would be a handy feature.
  I suspect the looping problem was the mixed up 191 disk, but the multiple
copies of a message is still with us.  I will forward a note from Ed Roop
explaining the problem.  Have you been able to look into this one further.
   I would like your help in getting smarter about TUMSGATE so I go about
debugging things safely.  Any suggestions or instructions would be welcome.
Thankyou,
Carol
Received: from F29.Tymnet by B39.Tymnet; Tue, 14 Jul 87 11:33:27 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 14 Jul 87 11:29:14 PDT
Return-path: <NTD.A/NEWMAN@EMSNTD.Ontyme.Tymnet> 
From: NTD.A/NEWMAN@EMSNTD.Ontyme.Tymnet 
Date: 14 JUL 87 11:06:23 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I58231@Ontyme.Tymnet 
Subject: congrats 

Bill,

    CONGRATULATIONS!

    Just read your message (been on vacation for a few days).  You must
be very pleased and excited.  I did not want to call and risk waking
anyone up, so please pass my best wishes along to Becky.  Hope everything
continues to go smoothly.

//Adele
Received: from F29.Tymnet by B39.Tymnet; Tue, 14 Jul 87 11:33:31 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 14 Jul 87 11:29:20 PDT
Return-path: <CNS.ERMAIL@EMSNTD.Ontyme.Tymnet> 
From: CNS.ERMAIL@EMSNTD.Ontyme.Tymnet 
Date: 14 JUL 87 11:21:17 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: J94610@Ontyme.Tymnet 
Subject: "To: Bill Soley From: Joe Costantini"... 

To:  Bill Soley
From:  Joe Costantini

Congratulations and best wishes to you and your family.

Joe C.
Received: from F29.Tymnet by B39.Tymnet; Tue, 14 Jul 87 13:40:21 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 14 Jul 87 13:44:02 PDT
Return-path: <NETS.G/STOY@EMSNTD.Ontyme.Tymnet> 
From: NETS.G/STOY@EMSNTD.Ontyme.Tymnet 
Date: 14 JUL 87 13:22:27 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: (21 names) 
Message-id: I58342@Ontyme.Tymnet 
Subject: PUBLIC NETWORK - X.121 NUMBERING PLAN 
PUBLICNETWORK-X.121NUMBERINGPLAN: 

     ********************************************************************
                           M E M O R A N D U M
     
                                                  [ ] T Y M N E T
                                                    Network Systems Group
     
     TO:  DISTRIBUTION
     
     CC:  MIKE CADY      RAJA RAJARAMAN
          NEIL SULLIVAN  KEN HOLCOMB
     
     FR:  GARY R. STOY
     
     RE:  PUBLIC NETWORK - X.121 NUMBERING PLAN
     
     ********************************************************************
     
          A meeting is planned to discuss the Public Network - X.121
     numbering plan.  This meeting is planned for 28 July 1987 at Tymnet
     Headquarters in San Jose.
     
                    PLACE:    ORCHARD PARKWAY
                              CONFERENCE ROOM A
                    DATE:     28 JULY 1987
                    TIME:     1000 - 1400
     
          The agenda is being formalized at this time and has been
     proposed to include the following items:
     
          - Correct deficiencies in current numbering plan.
          - Plan for additional internetworking capabilities such as:
               o  Telex, maritime and mobile services
               o  Packet Switch to Voice circuit switching
               o  Packet Switch to ISDN
          - Plan for the implementation of changes to the current and
            future X.121 numbering plan.
     
          This meeting is to include members of External Network services
     (ENS), Network Technology Development (NTD), Network Planning,
     Validations, Network Security, and Marketing.  It is intended to
     have the persons listed in the distribution list below to attend
     this meeting for their valuable input.
     
                              GARY
     
     
     
     
     
                              Distribution                              ____________
               Wayne Bartlett           Jeff Oliveto
               Edsel Budar              Steve Poppe
               Mary Chalmers            Omar Serang
               David Chang              Bill Soley
               Pat Driscoll             Dean Spieller
               Dennis Ernst             Rick Sprenkel
               Bob Fultz                Vida Stafford
               Dennis Jonas             Gazel Tan
               Allyn Morriss
Received: from F29.Tymnet by B39.Tymnet; Tue, 14 Jul 87 14:15:32 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 14 Jul 87 14:15:34 PDT
Return-path: <NTD.D/ALCORN@EMSNTD.Ontyme.Tymnet> 
From: NTD.D/ALCORN@EMSNTD.Ontyme.Tymnet 
Date: 14 JUL 87 14:05:26 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I58388@Ontyme.Tymnet 
Subject: NTD MOVE TO NEW FACILITIES 
NTDMOVETONEWFACILITIES: 

 
 
                            M E M O R A N D U M
 
                                             [] T Y M N E T, I N C
                                             Network Technology Development
 
DATE>    July 14, 1987
 
TO>      All NTD Employees
 
COPIES>  Mark Lauver, Al Fenn
 
FROM>    Dave Alcorn (for Mike Rude)
 
SUBJECT> NTD MOVE TO NEW FACILITIES
 
---------------------------------------------------------------------------
The results of the most recent move negotiations from this morning are as
follows:
 
NTD is expected to be moved in two phases.  The first phase will be for
those people destined for floors 3, 4, and 5 of the new building.
 
The move date for these three floors is Friday evening, July 31st.  Four
hours have been recommended for Friday afternoon to pack boxes.  However,
we have requested that boxes be delivered no later than Thursday, July
30th.
 
The second phase of the move will be for those people destined for floors 1
and 2.  The move date for this portion of NTD is Friday evening, August
14th.  Again, boxes have been requested for no later than Thursday, August
13th.
 
A variety of options have been looked at to reduce employee disruption as
much as possible.  However, it appears necessary to move some of the steel
case furniture work surfaces in advance of the main move.  This requirement
will impact 48 people in Building F and 7 people using freestanding steel
case work surfaces in Building D.  It is intended to move only work sur-
faces early, not partitions or shelving.  Cubicle lighting, power, and ter-
minal connections should not be affected.  It is the intent to replace the
work surface with a temporary desk until the main move on July 31st.
 
Unfortunately the early moving of the work surfaces requires more immediate
attention.  Work surfaces are to be removed on the evening of July 16th
which requires that those employees affected have their possessions removed
from the work surfaces and its associated drawers by the afternoon of July
16th.  Boxes have been requested to arrive late today or tomorrow.  Ter-
minals and phones need not be removed from the work surface.
 
In regard to the early removal and painting of other office furniture, a
detailed plan is not yet available as to which offices and pieces of furni-
ture are affected.  However, we have been assured that if we deem such
activity to be too disruptive, we can delay the painting until after the
move.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 16 Jul 87 14:58:47 PDT 
To: feldman@comet 
Subject: sources and hosts 

Oh where oh where did the sources go?  I can't find vfontedpr anywhere.
Used to be on minimac which is now lost and gone forever which brings
me to my second question: should I make any other changes to the host
tables when I take minimac out?  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Thu, 16 Jul 87 15:25:32 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18544; Thu, 16 Jul 87 15:29:59 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA05893; Thu, 16 Jul 87 15:29:28
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Thu, 16 Jul 87 15:29:28 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707162229.AA05893@comet.Tymnet.com> 
Subject: Re: sources and hosts 

the sources are currently (for the rest of the week) on "bigmac", which
is the hardware which was tymix.  Don't add it to the list; it won't
be around very long.  we'll eventually have 3 new names for the 3 new
servers, but not until we move.
	steve
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 20 Jul 87 15:13:45 PDT 
To: driscoll@calvin 
Subject: boot matrix 

Do you have a copy of the boot matrix we made on the board a while back?
I think somebody was going to put it in the GED if it isn't already.
-Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 20 Jul 87 15:47:32 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00735; Mon, 20 Jul 87 15:51:55 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA08838;
	Mon, 20 Jul 87 15:50:17 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11747; Mon, 20 Jul 87
	15:51:14 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Mon, 20 Jul 87 15:51:14 PDT 
To: wrs@calvin 
Message-id: <8707202251.AA11747@calvin.Tymnet.com> 
Subject: Schedules 

Bill,

  Could I see you tomorrow about Kernel schedules?  I'll be
around all day except from 2-3:30 pm when there's a meeting
scheduled with John Coons and Pat Manley regarding integration
of disk bringup tools and diagnostics into Rodney.  You might
be interested in attending.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Mon, 20 Jul 87 16:11:15 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00859; Mon, 20 Jul 87 16:15:37 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11954; Mon, 20 Jul 87
	16:14:54 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Mon, 20 Jul 87 16:14:54 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8707202314.AA11954@calvin.Tymnet.com> 
Subject: Re: boot matrix 

Bill,

  I've got a copy from the blackboard.  Haven't had the time to
make it into a nice table for RED yet.

Pat
Received: from F29.Tymnet by B39.Tymnet; Mon, 20 Jul 87 17:33:04 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 20 Jul 87 17:31:51 PDT
Return-path: <NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet> 
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 20 JUL 87 16:50:37 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.A/FENN@Ontyme.Tymnet, NTD.K/HOLCOMB@Ontyme.Tymnet,
	NTD.D/RAWSON@Ontyme.Tymnet, NTD.D/LASATER@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	NTD.D/ALCORN@Ontyme.Tymnet, NTD.J/COONS@Ontyme.Tymnet,
	NTD.B/CHANG@Ontyme.Tymnet, NTD.W/EUSKE@Ontyme.Tymnet,
	NTD.J/KOPF@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.M/STIMAC@Ontyme.Tymnet, NTD.D/BLISS@Ontyme.Tymnet,
	NTD.A/MORRISS@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet 
Message-id: I60999@Ontyme.Tymnet 
Subject: Skywalker Weekly Project Review 

 
                      Tymnet/Network Technology Development
                          PROGRAM MANAGEMENT DEPARTMENT

 
                               M E M O R A N D U M 

 
                                                                []  ISG
 
DATE>      20 JUL 87  16:48

TO>        Distribution List (See CC List above)

COPIES>    Al Fenn
           Ken Holcomb

FROM>      Allyn Morriss


SUBJECT>   Skywalker Weekly Project Review


-----------------------------------------------------------------------


The following are the minutes from the Skywalker project status meeting
held Thursday July 16 at 1:30PM.
 
NEXT MEETING
------------
 
    Thursday July 23 at 1:30PM - Building D Conference Room
 
ATTENDEES
---------
 
    Dave Bliss     John Coons     Pat Driscoll     Dan Lasater    Gazel Tan
    Mike Stimac    Allyn Morriss
 
OLD BUSINESS
------------
 
1.  Status of the Tadpole requisition was believed signed by Mike Rude for
    Al Fenn and submitted to purchasing. John Coons was not sure if
    requisition was to be handled by Barbara Veigh or sent direct to VG1
                                                                Page  2

    Purchasing.
 
2.  John Coons has hired one consultant for IOCP bring up support.
    Another consultant was mentioned as additional requirement.
 
3.  No further status to report on Indian Hills coding style.
    Bill Soley has been kept busy with domestic matters.
 
4.  Regarding testing, most of the Skywalker team attended a Brown-bag
    presentation on test methodology given by Charlotte Hollander on 7/13.
    Several useful ideas were found to be applicable but current scheduling
    does not make allowance for the time needed to test with this level
    of vigor using this procedure. Suggest a further discussion of this
    and sizing of the schedule impact if methods are adopted.
 
5.  Action on 68851 PMMU was completed by Bliss with copy of issues sent
    to Dan Lasater.
 
6.  Concerning a/NTDDOC/SKY for storage of Skywalker documents under UNIX,
    Allyn Morriss took an action item to meet with Alex Ting for better
    understanding of what would be involved. Broader issue of document
    transmittal and control was mentioned and its similaries with software
    library service by Allyn. This action clearly falls in Sheldon Cohen's
    area and procedures will be drafted after clearer picture is known.
 
7.  To restate from last week's minutes, software development has requested
    a test bed on 4th Floor with 2 GPs in the mini-lab. Engineering has
    no problem and Sandy McFarland has been apprised.
 
8.  Schedules were requested of those not having them available last week.
    Those received were placed in Ken Holcomb's office by Allyn Morriss.
 
9.  Regarding SUN documentation, Mike Stimac has prepared a memo stating
    the subset document requirements he recommends for developers.
    Decision to stock each floor of new facility with complete set rests
    with Al Fenn. See additional comments under New Business.
 
10. Export Compliance. See New Business.
 
11. Nothing to report on Microtec Compiler vs SUN. John Kofp was
    to work with Mike Stimac to identify differences, document and provide
    solutions.
 
12. Security on Eunix as reported by John Kopf was discussed.
    Mike Stimac had not received memo from Ken detailing John's concerns
    but both Dan Lasater and John Coons made it clear that all command
    capability is to be eliminated from the logon prompt on the VAX
    implementation of UNIX.
 
13. Complete Skywalker Documents list NOT INCLUDED here.
 
NEW BUSINESS
------------
 
1.  Dave Bliss met with Pleasey to assess them as alternative to Tadpole.
    Pleasey is the more experience manufacturer but has restrictive
    manufacturer's rights. Tadpole contracts out much of its manufacturing
    and is less restrictive as to rights.
                                                                Page  3

 
2.  Update on export compliance draws attention to potential serious problem.
    Exporting of 32-bit technology is Okay for Western-allied countries.
    Sweden is regarded as 'neutral' country. This needs further investigation
    as pertains to Televerket.
 
3.  Bliss met with Bill Schrader and in the course of their meeting was
    shown a Skywalker Gantt chart that indicates 'Packaging
    Complete' on 8/87. Dave thinks the chart came from Mike Rebmann.
 
4.  Coons spoke of Consultant's comments on physical port & IOC board
    mapping. More discussion is suggested since John hurriedly left for
    another meeting at this point saying he would speak further w/ Dave Bliss.
 
5.  Pat had serious concerns over the readiness of test beds.
 
6.  Pat brought up the requirement for human interface to Skywalker utilities.
    File formatter and Logger were mentioned as examples. This was eliminated
    earlier from Alfred spec.
 
7.  Dan is worried about Laser printer compatibility between Xerox and Data
    Products. Xerox is not post-script and picture draw is a must.
    Another printer may be required.
 
8.  Dan called attention to Phase 2 development plans for Skywalker as
    relates to decisions needed for Engine enhancements as tradeoff or what
    might have to be implemented first on the Engine vs. later on Skywalker.
    ISIS work assignments in the near future will depend upon enhancements
    for the Engine vs ISIS68 for Skywalker. Seems like this has broader
    planning implications beyond ISIS development.
 
    Group felt that NTD should begin collecting ideas for Phase 2
    independent of Marketing to be assured that we have done our homework
    before waiting for discussions with Marketing.
 
9.  Mike Stimac says document reproduction rights is expected from SUN
    Microsystems this week. Group says need at least 50 copies of core documents
    to meet immediate needs of Skywalker developers. Eventually, 200 copies
    will be needed. Group would like control over who has what so updates
    can be distributed most effectively. Sounds like a NTD Library
    function although Group seemed to be looking at me to support this.
 
11. Document review status:
 
    For signoff:
 
    Alfred68K ...APPROVED
    ISIS68K Kernel Kernel Internal Design - Defer a week.
    ED68K Dispatcher Exerciser 7/9/87...Need for formal signoff is questionable.
    Dispatcher68K DO, 7/9/87...Rawson had serious reservations
    Dispatcher68K GED, 7/9/87...Rawson had serious reservations
    Dispatcher68K GID, 7/9/87...Rawson had serious reservations
 
    New documents
 
    XCOM Code Translation GED, 7/8/87...Newman unavailable for comment
    XCOM Code Translation GID, 7/8/87...Newman unavailable for comment
    NAD69 68020 Object Code Generation GED, 7/8/87...Stimac to check w/ Pam W.
                                                                Page  4

    NAD69 68020 Object Code Generation GED, 7/8/87...Stimac to check w/ Pam W.
    ISIS68K External Design...Decision on need for small group review this week.
    ISIS68K Internal Design...Adequate so far, awaiting more input by 7/21.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 20 Jul 87 18:20:56 PDT 
To: driscoll@calvin 
Subject: object file 

Who is the object file format guru?  I need to talk to him/her about start
addresses.  Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 20 Jul 87 21:33:37 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01707; Mon, 20 Jul 87 21:37:47 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10927; Mon,
	20 Jul 87 21:36:15 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01700; Mon, 20 Jul 87 21:37:40 PDT
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA08168; Mon,
	20 Jul 87 16:33:10 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA11128; Mon,
	20 Jul 87 16:33:03 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Mon, 20 Jul 87 16:33:03 PDT 
To: oliveb!tymix!wrs 
Message-id: <8707202333.AA11128@oliven.ATC.OLIVETTI.COM> 
Subject: forwarded msg. 


Path: oliveb!sun!gravity!klein
From: klein%gravity@Sun.COM (Mike Klein)
Newsgroups: rec.autos,misc.consumers

Subject: What "Caution: Baby Aboard" really means
--------------------------------------------------
Organization: Sun Microsystems, Mountain View, CA
Xref: oliveb rec.autos:3755 misc.consumers:3118

As the proud father of a new arrival, I now finally know what these
signs really mean:

	Caution: driver exhausted and distracted.  Stay clear.
--
Mike Klein		klein@Sun.COM
Sun Microsystems, Inc.	{ucbvax,hplabs,ihnp4,seismo}!sun!klein
Mountain View, CA
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 22 Jul 87 18:21:09 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 12 Jul 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

45	16	1915	1104

46	22	1915	1104

20	1

31	2

14	8

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 22 Jul 87 18:21:09 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 19 Jul 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

45	8	1915	1104

46	4	1915	1104

14	32

------------------------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Thu, 23 Jul 87 8:44:56 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04223; Thu, 23 Jul 87 08:47:11 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA28612;
	Thu, 23 Jul 87 08:45:41 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA05243; Thu, 23 Jul 87
	08:46:43 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Thu, 23 Jul 87 08:46:43 PDT 
To: wrs@hobbes 
Message-id: <8707231546.AA05243@calvin.Tymnet.com> 
Subject: Re: rid/dict.mm 

Bill,

It's the source for a data dictionary TBL table.  Leave it
in RID without putting it into SCCS.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Thu, 23 Jul 87 13:50:56 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04974; Thu, 23 Jul 87 13:53:12 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA02276; Thu, 23 Jul 87
	13:51:41 PDT
Return-path: <alanlo@hobbes> 
From: alanlo@hobbes (Alan Lo) 
Date: Thu, 23 Jul 87 13:51:41 PDT 
To: wrs@hobbes 
Message-id: <8707232051.AA02276@hobbes.Tymnet.com> 
Subject: setjmp/longjmp 


Bill,

I understand that you have already written the setjmp and longjmp
function for skywalker. May we borrow it? Please let me know 
where they are.

Alan Lo
Received: from F29.Tymnet by B39.Tymnet; Wed, 5 Aug 87 11:34:44 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 5 Aug 87 11:26:46 PDT
Return-path: <NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 05 AUG 87 07:52:54 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.D/RAWSON@Ontyme.Tymnet, NTD.D/LASATER@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.P/DRISCOLL@Ontyme.Tymnet, NTD.D/ALCORN@Ontyme.Tymnet,
	NTD.J/COONS@Ontyme.Tymnet, NTD.BJC@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.J/KOPF@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NTD.M/STIMAC@Ontyme.Tymnet,
	NTD.D/BLISS@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.A/FENN@Ontyme.Tymnet, NTD.K/HOLCOMB@Ontyme.Tymnet 
Message-id: I67771@Ontyme.Tymnet 
Subject: REVISED SKYWALKER MEETING SCHEDULE 
NextWeeklySkywalkerMeeting: 

                                  MCDONNELL DOUGLAS
                              INFORMATION SYSTEMS GROUP
                               INTEROFFICE MEMORANDUM
 
          Network Systems Company            Network Technology Development
          Tymnet                                            No:CS870803.003
                                                 Date:August 5, 1987  15:50
 
          SUBJECT>       Next Weekly Skywalker Meeting
 
          TO>            Dick Rawson              BJC
                         Dan Lasater              Bill Euske
                         Gazel Tan                John Kopf
                         Bill Soley               Mike Rude
                         Pat Driscoll             Michael Stimac
                         Dave Alcorn              Dave Bliss
                         John Coons               Allyn Morriss
 
          COPIES>        Al Fenn
 
          FROM>          Ken Holcomb
          -----------------------------------------------------------------
 
          Due  to  a  meeting  conflict,  I would like to schedule the next
          weekly Skywalker meeting for Thursday, August 6, at 1:30 PM in
          conference  room  505  on  the  third  floor of Bldg B, South Bay
          Center. One item of significant importance  is the  need for test
          beds  with  respect  to  your  schedule.    Please be prepared to
          discuss this on Friday.
 
          Ken.........
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 5 Aug 87 11:42:11 PDT 
To: rmurphy@f104 
Subject: license in files 

Putting your oper password in command files is no laughing
matter, Santa Claus.  I fixed your PERP job on 37 so it uses
the license feature in SUBMIT and took your name and password
out of the command file (TEST).  Of course, your TMCS pass-
word is still there, but I don't know what to do about that.
I hope I didn't screw anything up, so you may want to check
it.  

What brought this all on was a problem with the mail the job
was sending caliming it got an error.  Anyway, you may want
to create a MAIL.FWD on 37 as well so you'll get you PERP
mail.

Thanks -Bill
Received: from F29.Tymnet by B39.Tymnet; Fri, 7 Aug 87 1:56:09 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Fri, 7 Aug 87 1:56:19 PDT
Return-path: <DAUL@OFFICE-1.ARPA> 
From: William Daul / McDonnell-Douglas / APD-ASD <WBD.MDC@OFFICE-1.ARPA> 
Date: 6 Aug 87 23:43 PDT 
To: ASD.MDC@OFFICE-1.ARPA, ALG.MDC@OFFICE-1.ARPA, AWB.MDC@OFFICE-1.ARPA,
	M166087@SLVM307.McAuto.Tymnet, M189125@SLVM307.McAuto.Tymnet,
	ipc.n/marsh%Ontyme.Tymnet@OFFICE-1.ARPA, WRS.MDC@OFFICE-1.ARPA,
	RBC.COR@OFFICE-1.ARPA 
Cc: KB003568@CSWDCMS1.McAuto.Tymnet,
	CSUK.D/Vachell%Ontyme.Tymnet@OFFICE-1.ARPA,
	hrd.trng%Ontyme.Tymnet@OFFICE-1.ARPA,
	ntd.k/jones%Ontyme.Tymnet@OFFICE-1.ARPA 
Message-id: <MDC-WBD-BY52E@OFFICE-1> 
Subject: Unbelievable MDC Pro-Space Publicity Story!!! 
Author: Will Martin -- AMXAL-RI <wmartin@almsa-1.arpa>

Date:     Mon, 27 Jul 87 10:26:24 CDT

From: Will Martin -- AMXAL-RI <wmartin@almsa-1.arpa>

Subject:  Pro-Space Publicity

I was just looking through a back issue (May '87) of GOVERNMENT EXECUTIVE 
magazine (I'm no executive, myself -- this was an extra copy discarded by the 
library in our building). Anyway, I would recommend that Space readers try to 
locate a copy of this issue, as there are a couple articles in it of particular
interest to you. One is on the management of the NASA Space Station program, 
but the primary one, and why I am posting this with the subject above, is an 
editorial column about how ABC and CBS refused to air a McDonnell 
Douglas-sponsored ad, prepared by J. Walter Thompson, in favor of the 
development of the US Space Station which dramatically displayed our current 
inferiority to the Soviets in space. It's on page 56, and pictures from the ad 
are on page 57. I'll quote a bit:

"... The video portion begins with a a shot of outer space and gradually, as a 
space station comes into view, the voice-over narrator says that 'Right now, in
space' medicines are being developed to cure disease, new materials are being 
developed that could revolutionize the quality of products manufactured on 
Earth and scientific discoveries are being made that could change history.

By the end of the script, a Battlestar Galactica-sized space ship fills the 
screen -- with a red star and the letters CCCP on it. Says the narrator, 
'Shouldn't we be there too? America needs the Space Station.'"

"...Budget limits said they could afford to buy time on only one Sunday morning
talk show and a few local-TV nightime news slots. [This explains why I had 
never seen this ad nor heard of it before now.-WM] The networks made that 
decision easy for them. Only NBC said it would run the ad. ABC and CBS turned 
it down, said it was 'too controversial'."

[About a half-page of discussion omitted, ending with comments on the role of 
the media to censor ads for various reasons.] "... When they deny expression of
'controversial' opinions they don't like, this Republic has a heap of trouble, 
folks." (editorial by C. W. Borklund, Editor-In-Chief)

I think the point here, related to recent discussion on this list, is that 
there HAVE been efforts out there to publicize the pro-space concerns voiced by
many of this list's participants. But much of this effort has been stifled and 
suppressed by enemies of the pro-space viewpoint. If someone with the clout of 
McDonnell Douglas can't get a professionally-produced ad on TV, what chance do 
private persons have of spreading the word with some kind of newsletter or 
lobbying effort? (Though the budget limits mentioned above indicate that McD-D 
wasn't putting much corporate might behind this -- I wonder if the networks 
would have been so quick to refuse a "media blitz" campaign, where McD-D would 
be buying a LOT of ads, not just a few?)

Anyway, I think a lot of the past weeks' posters/participants would find this 
editorial and the series of stills from the ad of great interest. Maybe McD-D 
would donate copies to other groups for dissemination in various ways, like 
local cable public-access channels? Be a shame to waste the production costs...

Regards,

Will Martin

   wmartin@ALMSA-1.ARPA   (on USENET try ...!seismo!wmartin@ALMSA-1.ARPA )

------------------------------

Received: from F29.Tymnet by B39.Tymnet; Fri, 7 Aug 87 15:44:29 PDT
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Fri, 7 Aug 87 15:39:11 PDT
Return-path: <ACS.T/HACKWOOD@EMSTUMS.Ontyme.Tymnet> 
From: ACS.T/HACKWOOD@EMSTUMS.Ontyme.Tymnet 
Date: 07 AUG 87 22:32:50 
To: TUMS.SUP@EMSTUMS.Ontyme.Tymnet 
Message-id: A86978@Ontyme.Tymnet 
Subject: REASON ONTYME WAS DOWN 
HOSTDOWNTIME: 

                      APPLIED COMMUNICATIONS SYSTEMS
 
                           M E M O R A N D U M
 
 
                                                      []  ISG     
 
DATE>      07 AUG 87  15:30

TO>        ALL SUPERVISORS ON A:832

COPIES>    

FROM>      TOM HACKWOOD
            ONTYME SUPPORT


SUBJECT>   HOST DOWN TIME


-----------------------------------------------------------------------


On August 5, there was damage done to the filing structure of host A:832
about an hour before the daily backup was made.  The next day OnTyme
support attempted to repair the damage.  At about noon on August 6 it was
apparent that the host was going to have to be completely restored, as the
errors found in the database were not recoverable.
 
From 1300 hours on August 6 until about 0900 hours on August 7 all messages
were restored, with the first message being from July 22.  At the same
time, it was attempted to restore all files from July 22.
 
The result is as follows:
 
  o All messages have been restored, but they are in an unread state.  The
    messages that are on the system now that are older than 14 days will be
    purged on the night of August 8.  Messages that were originally sent
    through InterOntyme will be resent, with resulting errors reported by
    SYSTEM.ONTYME II.
 
  o Most files have been restored.  It is known that 18 files that were
    backed up on July 30 are lost.  In addition, any changes to files
    between 1700 hrs PDT (2000 hrs EDT) on August 4 to 1700 hrs PDT (2000
    hrs EDT) on August 6 are lost.  All messages and files are in the order
    they were restored (by date).
 
                                                                Page  2

We are sorry for the inconvience this has caused you.
 
Tom Hackwood
OnTyme Support
Received: from F29.Tymnet by B39.Tymnet; Fri, 7 Aug 87 17:24:24 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 7 Aug 87 17:19:55 PDT
Return-path: <NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 07 AUG 87 14:15:59 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I69249@Ontyme.Tymnet 
Subject: REVISION/TECHNICAL LIBRARY INFO 
REVISION-TECHNICALLIBRARYMEMO: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      07 AUG 87  14:14                  Network Technology Development

TO>        NTD PERSONNEL

COPIES>    

FROM>      KEN HOLCOMB


SUBJECT>   REVISION-TECHNICAL LIBRARY MEMO


-----------------------------------------------------------------------



THERE IS A TEMPORARY MINI-LIBRARY ON THE FIFTH FLOOR, BUILDING B.
AVAILABLE ARE SKYWALKER MATERIAL, SUN UNIX DOCUMENTATION, UNIX AND
C BOOKS, AND ABOUT 20 PERIODICALS.  (THERE IS ALSO A CUBICLE "READING
ROOM" - #325).  DOROTHY'S OFFICE IS #355, AND MARY'S IS #345.  SEND
YOUR REQUESTS VIA INTEROFFICE MAIL TO TECHNICAL LIBRARY, B-5, B-345,
OR BY ONTYME (NTD.D/SANDS OR NTD.M/PROFFITT).
 
***FOR THOSE OF YOU WHO READ THE FIRST MEMO ISSUED ON THIS SUBJECT
(#I69113), THE PHONE NUMBER WAS LISTED IN ERROR.
Received: from F29.Tymnet by B39.Tymnet; Thu, 13 Aug 87 15:44:13 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 13 Aug 87 15:38:13 PDT
Return-path: <NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet> 
From: NTD.K/HOLCOMB@EMSNTD.Ontyme.Tymnet 
Date: 13 AUG 87 11:28:10 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I71297@Ontyme.Tymnet 
Subject: Skywalker Review Mtg Notes 
SkywalkerWeeklyProjectReview: 

 
 
 
 
 
 
 
                                  MCDONNELL DOUGLAS
                              INFORMATION SYSTEMS GROUP
                               INTEROFFICE MEMORANDUM
 
          Network Systems Company            Network Technology Development
          Tymnet                                           No:CSS870813.001
                                                Date:August 13, 1987  11:21
 
          SUBJECT>       Skywalker Weekly Project Review 
 
          TO>            Dick Rawson              BJC
                         Dan Lasater              Bill Euske
                         Gazel Tan                John Kopf
                         Bill Soley               Mike Rude
                         Pat Driscoll             Michael Stimac
                         Dave Alcorn              Dave Bliss
                         John Coons
 
          COPIES>        Al Fenn
 
          FROM>          Ken Holcomb
          -----------------------------------------------------------------
 
          Old Business
 
          The status of the Tadpole requisition for Unix was signed by Mike
          Rude for Al Fenn and submitted to purchasing. John Coons  was not
          sure if  the requisition  was to  be handled  by Barbara Veigh or
          sent direct to VG1 Purchasing. Dave  Bliss indicates  that we can
          expect delivery from Tadpole around the end of August.
 
          The need  for another  consultant for  IOCP bring  up support was
          discussed. A decision was  made to  pursue this.  A consultant is
          scheduled to start 4-August. The consultant did come on board and
          is working half-time for two weeks and goes full time thereafter.
 
          It was decided to return the { as per Indian  Hills coding style.
          Dan Lasater  to update  coding procedures.  Check on status until
          complete. Dan stated that  the coding  procedures document should
          be updated on Monday if the online document can be found.
 
          Concerning a/NTDDOC/SKY  for storage of Skywalker documents under
          UNIX, Allyn Morriss took an action  item to  meet with  Alex Ting
          for  better  understanding  of  what  would  be  involved.  Allyn
          mentioned the broader issue  of document  transmittal and control
          and  its  similarities  with software library service. Procedures
          will be drafted after a  clearer  picture  is  known.  No further
          progress to report.
 
 
 
 
                                          1

 
 
 
 
 
          Reviewed controlled documents for approval.
 
               o    Rodney  Architectural  Specification 3/12/87 - Approved
                    and distributed to NSC. Needs revision.
               o    Skywalker Project Plan,  Version  2.10  -  Approved and
                    distributed to NSC. Needs revision.
               o    Skywalker  VMEbus  Interface  Gate  Array  HFS,  Rev D-
                    Approved and distributed to NSC.
               o    Skywalker IOC HFS, Rev  D.2 -  Approved and distributed
                    to NSC.
               o    NAD68  68020   Object  Code   Generation  GED  3/12/87-
                    Approved  and  distributed  to   NSC.  Revised  7/9/87,
                    approved and distributed to NSC.
               o    NAD68  68020   Object  Code   Generation  GID  4/23/87-
                    Approved and distributed to NSC.
               o    NAD68  68020  Object  Code   Optimization  GED  6/8/87-
                    Approved and distributed to NSC.
               o    NAD68  68020   Object  Code  Optimization  GID  7/8/87-
                    Approved and distributed to NSC.
               o    NAD68  68020  source  code   instructions  GED  7/1/87-
                    Approved and distributed to NSC.
               o    Alfred-68K  GED  6/25/87.  Approved  and distributed to
                    NSC.
               o    ISIS-68K External Specification 7/14/87  - Approved and
                    distributed to NSC.
               o    Skywalker   I/O   Controller   GED,  Rev  1.1  4/27/87-
                    Approved and distributed to NSC.
               o    Rodney External Specification 6/25/87   - Design Review
                    7/7/87, Unapproved.
               o    Dispatcher-68K DO 6/10/87 - Unapproved, do not dist.
               o    Dispatcher-68K GED 6/10/87 - Unapproved, do not dist.
               o    Dispatcher-68K GID 6/10/87 - Unapproved, do not dist.
               o    ED68K Dispatcher-68K  Exerciser 6/10/87  - Approved, do
                    not dist.
               o    ISIS-68K  Kernel  Kernel  Internal  Interfaces 6/29/87-
                    Approved and distributed to NSC.
               o    Skywalker  CPU   HFS  Rev  B  6/8/87  -  Approved,  and
                    distributed.
               o    Skywalker Hardware Documentation Plan,  Rev D.1 6/9/87-
                    Approved, do not dist.
               o    Skywalker IOC  Test Plan, Rev B.1 6/9/87 - Approved, do
                    not dist.
               o    Skywalker Serial I/O Hardware Functional Specification,
                    Rev A.13 7/31/87 - Approved and distributed to NSC.
               o    Skywalker  IOC  HFS,  Rev  E.1  6/9/87  -  Approved and
                    distributed to NSC.
               o    Skywalker CPU Test Plan,  Rev B  6/15/87 -  Approve, do
                    not dist.
               o    ISIS-68K KIO Internal Interfaces 7/13/87 - Approved and
                    distributed to NSC.
 
 
                                          2

 
 
 
 
               o    Node Code SCLP GED  6/16/87 -  Approved and distributed
                    to NSC.
               o    Node Code SCLP GID 8/6/87 - Unapproved.
               o    XCOM  Code   Translation  for  Skywalker  GID,  7/8/87-
                    Unapproved.
               o    XCOM  Code  Translation  for   Skywalker  GED,  7/8/87-
                    Unapproved.
               o    ISIS-68K  I/O  Subsystem  Internal Interfaces, 7/10/87-
                    Approved and distributed to NSC.
               o    Skywalker    System     Level    Hardware    Functional
                    Specification, Rev  C 7/29/87. Approved and distributed
                    to NSC.
 
          Televerket update on September 15th. BJ  to coordinate  with core
          team to plan presentation format and direction.
 
          Export  Compliance.  Stan  King  has been assigned this issue. We
          will check on the  status each  week. There  may be  some concern
          over exportation  of 32-bit  technology to a neutral country such
          as Sweden. This needs further investigation. Most of the form has
          been filled out. a couple of questions have arisen. Dave Bliss to
          get them to Ken. Ken has  organized  a  meeting  with  the Export
          Compliance people in Irvine on August 19.
 
          There appears  to be  some vaguely  defined problem  with how the
          Microtec Compiler incorporates H files.   John Kopf  to work with
          Michael  Stimac  to  identify  differences,  document  them,  and
          provide solutions if  possible.  Pam  Whitehurst  talked  to John
          Kopf.  Now  a  list  of  issues exist. The Microtec approach to a
          makefile is  incompatible with  other compilers.  Mike Stimac has
          agreed to  petition Microtec  to comply. Additional problems with
          the Microtec compiler were discussed with  in-line assembly code.
          There  are  some  problems  with  5.3 to 6.0 tools. All are being
          documented  and  forwarded  to  Julie  Stier  to  communicate  to
          Microtec. Ken  to check  with Julie  on forwarding of problems to
          Microtec. Julie has been doing this,  although she  currently has
          two problems which have not been sent on to Microtec.
 
          There appears to be an issues with the test beds. There is a plan
          in place. There is  weekly meeting  in Engineering  to review the
          status.  The  issue  on  disk  drives  on  CMF test beds has some
          financial impacts as well as  significant  impact  on  the Rodney
          schedule.  The  existing  drives  will be replaced with Skywalker
          drives. Susan Dyer will deal with  the financial  paper work. The
          current test  bed requirement was reviewed. It was broken down as
          follows.
 
 
 
 
 
 
 
                                          3

 
 
 
 
 
          Test Bed  Group     Date Required  Configuration
 
             #1     IOCP         9/15/87     02 GPS
                                             01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
             #2     IOCP         9/15/87     02 GPS
                                             01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
             #3     DIAGNOSTIC   8/6/87      01 File Server & Dsk
                                10/15/87     01 GP
                                             01 IOCP 4
                                             01 IOCP 8
             #4     RODNEY       9/15/87     02 GPS
                                10/01/87     01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
             #5     RODNEY      10/01/87     02 GPS
                                             01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
             #6     RODNEY      10/01/87     02 GPS
                                             01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
             #7     RODNEY      10/15/87     02 GPS
                                             01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
             #8     NODE        11/15/87     02 GPS
                                             01 File Server & Dsk
                                             03 IOCP 4
                                             01 IOCP 8
             #9     ISIS        10/01/87     02 GPS
                                             01 File Server & Dsk
                                10/07/87     01 IOCP 4
                                11/18/87     01 IOCP 8
             #10    ISIS        11/18/87     02 GPS
                                             01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
             #11    ISIS        11/18/87     02 GPS
                                             01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
             #12    ISIS        11/18/87     02 GPS
                                             01 File Server & Dsk
                                             01 IOCP 4
                                             01 IOCP 8
 
 
                                          4

 
 
 
 
          NAD testing would be done on test bed 1 or 2.
 
          BITS testing would be done on 4,5,6 or 7.
 
          XCOM testing requirements were not identified.
 
          There was a discussion on the requirement for  human interface to
          Skywalker utilities which have been eliminated from Alfred. These
          features are needed. Bits can  be  modified  to  add directories.
          There is no good solution for reading the log. Rodney to spec out
          file utilities additions to deliver after release 1.
 
          NTD needs to collect ideas for  phase two  of Skywalker.  BJC has
          agreed to be the clearing house for these ideas. We have received
          no input from IOCP, XCOM, BITS or NAD.
 
          Check with Engineering on the cost  update information requested.
          Ontyme with  update sent  to Marketing  on 30  July. Betty Jo has
          worked  out  some  sample  configurations  which   indicate  that
          materials cost  continue to come down from our original estimates
          and that we are very much inline with the Marketing Requirements.
 
          Mary Arnett  provided  NTD  with  a  Skywalker  Support  plan for
          Skywalker. I asked for comments and have received none.
 
          A new  revision of the Serial I/O Breakout HFS was distributed. A
          discussion of X.422  for  Skywalker.  It  was  decided  that DB25
          connectors  would  be  used  rather  than  some  form  of  37 pin
          connectors.  Version A.13 was approved.
 
          Pat  Driscoll   agreed  to   provide  libraries   for  the  X.409
          implementation being implemented in Rodney for other developers.
 
          Les Seltzer asked how other developers who were not involved with
          the original design  of  various  Skywalker  protocols  could get
          interpretations of  the designs. A procedure was worked out where
          the developers  would review  the documentation  and consult with
          Pat or Dan on any question. Dan further offered the suggestion to
          review the Video tape he made regarding Skywalker architecture.
 
          Schedule updated due to Ken by 14 August.
 
          New Business
 
          Review old business.
 
          Review documentation schedule.
 
          Update schedule.
 
          Next meeting August 20, RM 3-505.
 
 
                                          5
Received: from F29.Tymnet by B39.Tymnet; Fri, 14 Aug 87 12:03:45 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 14 Aug 87 11:51:49 PDT
Return-path: <NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet> 
From: NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet 
Date: 14 AUG 87 11:02:13 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I71850@Ontyme.Tymnet 
Subject: SUN UNIX DOCUMENTATION 
SUNUNIXDOCUMENTATION: 

 
 
 
 
 
                        MCDONNELL DOUGLAS
                    INFORMATION SYSTEMS GROUP
                     INTEROFFICE MEMORANDUM
 
Network Systems Company            Network Technology Development
Tymnet                                 Date:  14 August 1987
 
 
SUBJECT>  SUN UNIX DOCUMENTATION
 
TO>       NTD Personnel
 
FROM>     Dorothy Sands, Librarian
-----------------------------------------------------------------
 
Please be advised I have received 300 binders of the Sun Unix
Documentation.  They are currently available in the Library on
the 5th floor of Building B (#355).
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 17 Aug 87 8:35:22 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 26 Jul 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

46	36	1915	2302

31	2

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 17 Aug 87 8:35:22 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 2 Aug 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

46	30	1915	2302

31	3

20	8

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 17 Aug 87 8:35:22 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 9 Aug 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

20	16

46	16	1915	2302

31	8

32	12

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 17 Aug 87 8:35:22 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 16 Aug 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

46	42	1915	2302

30	4	1915	2302

31	4

20	2

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 17 Aug 87 8:35:42 PDT 
To: ntd.s/black@ontyme 
Subject: move 

The time sheets I just sent you report move to activity code 20.
If that's not correct, please change all my 20's to whatever is
appropriate.  I seem to remember Ken saying that's what we were
supposed to use, but I'm not certain.  Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 17 Aug 87 11:11:34 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA15491; Sun, 16 Aug 87 19:13:41 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12199; Sun,
	16 Aug 87 19:13:23 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA15464; Sun, 16 Aug 87 19:13:27 PDT
Return-path: <MAILER-DAEMON@tymix> 
From: MAILER-DAEMON@tymix (Mail Delivery Subsystem) 
Date: Sun, 16 Aug 87 19:13:27 PDT 
To: <wrs@hobbes> 
Message-id: <8708170213.AA15464@tymix.Tymnet> 
Subject: Returned mail: Cannot send message for 3 days 

   ----- Transcript of session follows -----
451 <WRS@39.Tymnet.com>... reply: read error
<WRS@39.Tymnet.com>... reply: read error

   ----- Unsent message follows -----
Received: by tymix.Tymnet (5.51/4.7)
	id AA03512; Thu, 13 Aug 87 19:05:04 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2)
	id AA05449; Thu, 13 Aug 87 19:04:52 PDT
Date: Thu, 13 Aug 87 19:04:52 PDT
From: wrs@hobbes (Bill Soley)
Message-Id: <8708140204.AA05449@hobbes.Tymnet.com>
To: wrs
Subject: dfx2ps

Extra \\! at begining sometimes.  Why?  So, fix it already!
Received: from tymix.Tymnet by B39.Tymnet; Mon, 17 Aug 87 11:12:52 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA10647; Sat, 15 Aug 87 08:09:05 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10419; Sat, 15 Aug 87
	08:08:51 PDT
Return-path: <julies@hobbes> 
From: julies@hobbes (Julie Stier) 
Date: Sat, 15 Aug 87 08:08:51 PDT 
To: wrs@hobbes 
Message-id: <8708151508.AA10419@hobbes.Tymnet.com> 
Subject: ontyme errors 


Bill,

Would you know any reason for tymix givving errors on ontyme messages?
The major user name seen is your's, ntd.g/bubsy and ntd.d/ellenburg
(neither of which are in ntd).  I know you were working on tums, and I'm
not sure if this is related.  Dave Hopkins showed me a printout of very
frequent error messages.  Sorry I am not more explicit about the
type of error, that would have to wait til monday.

julie

Received: from tymix.Tymnet by B39.Tymnet; Tue, 18 Aug 87 13:01:12 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA21734; Tue, 18 Aug 87 13:06:13 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA20035;
	Tue, 18 Aug 87 13:05:52 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA13375; Tue, 18 Aug 87
	13:06:08 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Tue, 18 Aug 87 13:06:08 PDT 
To: wrs@calvin 
Message-id: <8708182006.AA13375@calvin.Tymnet.com> 
Subject: Libraries 

Bill,

 Please contact Ms. Tsay, on your floor in cubicle number 415, and
provide her directions on which library routines she needs to work
on.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Tue, 18 Aug 87 16:24:49 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22450; Tue, 18 Aug 87 16:29:51 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA21353;
	Tue, 18 Aug 87 16:29:30 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA11145; Tue, 18 Aug 87 16:29:36
	PDT
Return-path: <wrs@opus> 
From: wrs@opus (Bill Soley) 
Date: Tue, 18 Aug 87 16:29:36 PDT 
To: wrs@opus 
Message-id: <8708182329.AA11145@opus.Tymnet.com> 
Subject: phone list 

Cheryl hedin C1 switch room - phone list on dBase-III floppy.
Received: from F29.Tymnet by B39.Tymnet; Wed, 19 Aug 87 10:16:26 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 19 Aug 87 10:19:57 PDT
Return-path: <NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet> 
From: NTD.J/SARGENT@EMSNTD.Ontyme.Tymnet 
Date: 19 AUG 87 08:29:05 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I73465@Ontyme.Tymnet 
Subject: SANDY MCFARLAND'S ABSENCE 
SANDYMcFARLAND: 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      19 AUG 87  08:28                  Network Technology Development

TO>        NTD PERSONNEL

COPIES>    

FROM>      JOY SARGENT


SUBJECT>   SANDY McFARLAND


-----------------------------------------------------------------------


THIS IS TO ADVISE YOU THAT SANDY McFARLAND WILL BE OUT OF THE OFFICE
UNTIL MONDAY, AUGUST 24.  IN THE INTERIM, PLEASE USE THE FOLLOWING
GUIDELINES:

1)  NETWORK PROBLEMS. . .CALL KAL KALENDA AT X 6147.
2)  HOST PROBLEMS. . .CALL JULIE STIER AT X 6311
3)  ANYTHING ELSE, REFER TO KEN HOLCOMB AT X 6614.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 19 Aug 87 10:27:11 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22591; Tue, 18 Aug 87 17:44:38 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA21743; Tue,
	18 Aug 87 17:44:16 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA08010; Tue, 18 Aug 87 17:44:29 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 18 Aug 87 17:44:29 PDT 
To: wrs@f104 
Message-id: <8708190044.AA08010@f104.sun.com> 
Subject: Debugging the scheduler 

I need a way to disable the background (disable activation of processes on 
the run queue) in the scheduler upon command, for debugging purposes. 
Since I am using dbx and I already have conditional-compilation code in
the logger for debugging purposes, one way to do this would be to have
conditional debugging code in the scheduler which exits the scheduling
routine immediately if a boolean (also conditionally compiled) is on.  This
boolean could be set and unset by dbx.  Can you think of a simpler approach?
If not, could you add such code?
Thanks,
Ron

Received: from tymix.Tymnet by B39.Tymnet; Wed, 19 Aug 87 17:32:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02942; Wed, 19 Aug 87 17:37:07 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27842;
	Wed, 19 Aug 87 17:36:44 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AB15456; Wed, 19 Aug 87 17:36:46
	PDT
Return-path: <MAILER-DAEMON@opus> 
From: MAILER-DAEMON@opus (Mail Delivery Subsystem) 
Date: Wed, 19 Aug 87 17:36:46 PDT 
To: wrs@opus 
Message-id: <8708200036.AB15456@opus.Tymnet.com> 
Subject: Returned mail: User unknown 

   ----- Transcript of session follows -----
550 system... User unknown

   ----- Unsent message follows -----
Return-Path: <wrs>
Received: by opus.Tymnet.com (3.2/SMI-3.2)
	id AA15454; Wed, 19 Aug 87 17:36:46 PDT
Date: Wed, 19 Aug 87 17:36:46 PDT
From: wrs (Bill Soley)
Message-Id: <8708200036.AA15454@opus.Tymnet.com>
To: system
Subject: phone data base

The /usr/public/phones.tym data base, used by the phone command, has been
updated with a list of all N.1St phones (even people that aren't here yet).
The fields are: last name, first name, extension, office location, mail
stop, cost code, division name.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 19 Aug 87 22:16:51 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA04021; Wed, 19 Aug 87 22:21:59 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA00265; Wed, 19 Aug 87
	22:21:37 PDT
Return-path: <jwillott@hobbes> 
From: jwillott@hobbes (Jim Willott) 
Date: Wed, 19 Aug 87 22:21:37 PDT 
To: wrs@hobbes 
Message-id: <8708200521.AA00265@hobbes.Tymnet.com> 
Subject: Sample AutoCAD DXF File 

Bill,

I placed a sample AutoCAD DXF file in opus:/usr/opus/eng/jwillott.
The file name is sample.dxf.  Copy of DXF file format comming under
separate cover along with a hard copy of the sample drawing.  This
file uses shapes which could be a problem.  Lots of luck.

Jim

Received: from tymix.Tymnet by B39.Tymnet; Thu, 20 Aug 87 15:17:59 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06440; Thu, 20 Aug 87 15:22:27 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA04888; Thu,
	20 Aug 87 15:22:02 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA06436; Thu, 20 Aug 87 15:22:20 PDT
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA26939; Thu,
	20 Aug 87 15:08:24 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA14822; Thu,
	20 Aug 87 15:08:13 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Thu, 20 Aug 87 15:08:13 PDT 
To: oliveb!tymix!wrs 
Message-id: <8708202208.AA14822@oliven.ATC.OLIVETTI.COM> 
Subject: more stuff 

Path: oliveb!intelca!amd!amdcad!ames!hao!gatech!mcnc!unc!shb
From: shb@unc.cs.unc.edu (Sandra Bloomberg)
Newsgroups: misc.kids,sci.med
Subject: Old English furniture polish
Message-ID: <990@unc.cs.unc.edu>
Date: 15 Aug 87 17:58:40 GMT
Organization: CS Dept., U. of N. Carolina, Chapel Hill
Lines: 26
Xref: oliveb misc.kids:2006 sci.med:2843
Posted: Sat Aug 15 10:58:40 1987


(I do not subscribe to sci.med, so forgive if this is an old topic. 
I Xposted since sci.med readers may have answers.)

Recent postings on product safety reminded me:

If you have or soon will have small children, throw away (not
just lock up) your Old English Furniture Polish.
I have been told (by a pediatrician and a pathologist (hi, bill!)) that
medical literature indicates this is an extremely frequent cause of
fatal poisoning in children. Evidently ipecac is no help because the
stuff is even more dangerous coming back out than staying in.

I heard of a child drinking some recently while the family was moving;
this was the kind of family that goes to the new house to install
childproof locks on the cabinets BEFORE moving in, and somehow the
child still got to it.

I don't know if it is just the scratch cover kind or all varieties,
but it is the Old English brand in particular. I also do not know if
the brand is more lethal, or just if packaging makes it more desirable
to children. (I would be interested if anyone does know, or has
information more accurate than my hearsay. Do any pediatricians
tell parents about this product?)

Sandra Bloomberg


Path: oliveb!pyramid!amdahl!ames!hao!noao!arizona!cascabel!lorrain
From: lorrain@cascabel.UUCP (Lorrain Nilsen)
Newsgroups: misc.kids
Subject: Re: Bumper Pad Warning  -- and product safety
Message-ID: <204@cascabel.UUCP>
Date: 12 Aug 87 07:02:41 GMT
References: <2174@drivax.UUCP>
Organization: Mom 'n Pop Software Shop, Tucson, AZ
Lines: 58

> PLEASE,  if you must use bumper pads,  please do not use the kind 
> that  ties.   The strings may not look long enough to do any harm 
> to  your  child  but  do you really want  to  take  that  chance?  
> Believe me, a few bruises on the head are a small price to pay if 
> it will save your child's life.
> 
> Please consider what I've had to say.  I've debated long and hard 
> about  posting  this to the net but it is a danger  that  parents 
> must  be  made aware of.   If I only reach one person  with  this 
> 
> Lynn

Thanks for sharing your story and warning.  We removed the bumper pads
from our son's crib when he was seven weeks old due to a close-call we had.
Recently, I had been considering re-installing them, but now I think
I'll throw them out.  For those who might benefit, I'll tell what happened 
when our son was seven weeks old.  I had left him in his crib for a while
while I was doing some housework.  After a short time, I went to check on
him and found that he had inched up to one of the corners of his crib and
had gotten his face pressed so tightly against the bumper pad that he 
couldn't breathe.  Babies that small can't move backwards, so he was
trapped.  We decided to remove the pads until he was more mobile.  

I've done some checking around on product safety, and found a toll-free
number you can call to get free information about Nursery safety, crib
safety, toy safety, or any kind of product safety you want.  The number
is for the U.S. Consumer Product Safety Commission.  It is: 
800-638-CPSC.  You can also write to: 
	
		U.S. Consumer Product Safety Commission
		Washington, D.C.  20207	

The above phone number and address are also the ones to use if you want
to report a product hazard or product-related injury.  

I also found a separate address and phone numbers for information about
toy safety:  
		TOYS
		Washington, D.C.  20207

		ph:  800-638-8326  Maryland only: 800-492-8363
		Alaska, Hawaii, Puerto Rico, Virgin Islands: 800-638-8333

I called the CPSC when we decided to build our own crib, and found them 
very helpful.  We received the information in less than two weeks.  
They also sent a lot of information we hadn't asked for, but which proved
to be quite valuable.  We got a booklet called "The Safe Nursery", 
which describes unsafe features in baby furniture and accessories, 
and includes a safety checklist to use as a guide when buying new or 
secondhand nursery equipment.  We also received a story book called 
"Little Leon the Lizard" which contains several stories to read to 
preschoolers.  The stories teach children the importance of not playing 
with broken toys, not climbing into chests or old refrigerators, etc.  


I hope someone finds the above information useful.

Lorrain
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 25 Aug 87 13:44:32 PDT 
To: unet!billp@tymix.Tymnet (Bill Putney) 
Subject: Catalyst 

Reminder to check on Sun's Catalyst program.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 26 Aug 87 1:30:37 PDT 
To: unet!billp@tymix.Tymnet (Bill Putney) 
Subject: Panasonic PBX 

You mentioned that somebody had a Panasonic 3/8 PBX at their house.  I
wonder if you could get a xerox of the manual for me so I can see if it
meets my requirements.  Thanks much.  -Bill
From: Richard Teng <richteng@B39.Tymnet> 
Date: Wed, 26 Aug 87 10:20:11 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: data structures book 

Bill,

I don't remember you lending me any text book.  (I checked anyway and couldn't
find any of yours.)  However, I do have a copy (my own) of a book which I'd be
glad to lend you if you need it.  It's also called "data structures", and the
authors are AHO, HOPCROFT, and ULLMAN.  Let me know.

- rich -
Received: from F29.Tymnet by B39.Tymnet; Fri, 28 Aug 87 9:07:36 PDT
Received: from OFFICE-1.ARPA by F29.Tymnet; Fri, 28 Aug 87 9:13:20 PDT
Return-path: <STONE@OFFICE-1.ARPA> 
From: Duane Stone / McDonnell Douglas <DLS.MDC@OFFICE-1.ARPA> 
Date: 28 Aug 87 12:06 EDT 
To: WBD.MDC@OFFICE-1.ARPA 
Cc: ASD.MDC@OFFICE-1.ARPA, RBC.COR@OFFICE-1.ARPA, DCE.MDC@OFFICE-1.ARPA,
	WRS.MDC@OFFICE-1.ARPA, TDC.MDC@OFFICE-1.ARPA 
Message-id: <MDC-DLS-C32LP@OFFICE-1> 
Subject: Re: Frwd: seismo is retiring 
In-reply-to: MDC-WBD-C3122

Although we are cautioned not to regard the new machine as a general purpose 
gateway, does this mean that we (Augment users) can substitute @uunet.uu.net 
for @uucp.css.gov after 1 Sep in the hopes that mail will go through??  Is 
there anyway of telling which UUCP hosts have registered with uunet?  (maybe 
they ave a NIC?? that can be queried??)

Is uunet.uu.net now known to our host tables??  If not, it probably should be. 
I've seen incoming mail with that domain recently.

   Stoney

Received: from tymix.Tymnet by B39.Tymnet; Tue, 1 Sep 87 17:34:29 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA05484; Tue, 1 Sep 87 17:38:51 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA22891; Tue, 1 Sep 87 17:38:29
	PDT
Return-path: <julies@hobbes> 
From: julies@hobbes (Julie Stier) 
Date: Tue, 1 Sep 87 17:38:29 PDT 
To: microtec@hobbes 
Message-id: <8709020038.AA22891@hobbes.Tymnet.com> 
Subject: Reported bugs 


Following is a list of bugs so far reported to microtec:

Microtec MCC68k version 5.2A - Unix bsd 4.3

* Inconsistent handling of include files between the cc compiler and
the mcc68k compiler which involves specifying the path in the source
code for the include files in some circumstances.  The result 
hampers portability.  The bug was submitted by John Kopf. 


Microtec MCC68k version 6.1A - Unix bsd 4.2

* Version 6.1A does not handle comment statements properly in some 
cases, causing incompatibility problems with source code which assembles
properly with version 5.2A.  In version 6.1A, if the asterisk is not in 
column 1, an ERROR:(528) Invalid Opcode  is gererated and NOP's are
inserted into the object file.  The bug was submitted by John Coons.


* Incorrect and inconsistent results are produced when the value of 
an expression whose evaluation causes overflow is compared with zero.
This bug was submitted by Bill Soley.


* The compiler generates a short branch around some in-line
assembly code when it should generate a long jump.  This bug was
discovered by Ed Gauci and submitted by Pam Whitehurst.



For all of the above problems I have more details on the problem and
most have source code, output and assembly listings available to 
examine.


Julie


Received: from tymix.Tymnet by B39.Tymnet; Wed, 2 Sep 87 11:19:57 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07950; Wed, 2 Sep 87 11:24:26 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27880;
	Wed, 2 Sep 87 11:24:22 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA13135; Wed, 2 Sep 87 11:24:21
	PDT
Return-path: <dfields@opus> 
From: dfields@opus (David Fields) 
Date: Wed, 2 Sep 87 11:24:21 PDT 
To: wrs@opus 
Message-id: <8709021824.AA13135@opus.Tymnet.com> 
Subject: Mail 

OK, here's the information:

	Ontyme name:	NTD.D/FIELDS
	Password:	fine mess
	Send to:	Opus
	Username:	dfields

Thanksabunch - Let me know if there's anything else you need.

Dave
Received: from tymix.Tymnet by B39.Tymnet; Wed, 2 Sep 87 11:32:57 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07990; Wed, 2 Sep 87 11:37:33 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27996; Wed, 2 Sep 87 11:37:30
	PDT
Return-path: <davarj@hobbes> 
From: davarj@hobbes (Jonathan Davar) 
Date: Wed, 2 Sep 87 11:37:30 PDT 
To: wrs@hobbes 
Message-id: <8709021837.AA27996@hobbes.Tymnet.com> 
Subject: ? 


If you aren't at the meeting today, please call me at 922-6092
concerning the reference to setjmp.h in kernel.h, asap. My files will
not compile!!

Thanks ,

Jonathan.

  From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 7 Sep 87 22:57:03 PDT 
To: Wayne D. Bartlett <BARTLETW@B39.Tymnet>, Carl A. Baltrunas
	<Carl@X930.Tymnet> 
Subject: (SYS)NAD 

(SYS)NAD is done.  It just needs to be installed.
I guess it can get transmitted by Carl when he does (SOURCE)NAD
or whatevr the procedure is.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 8 Sep 87 11:15:58 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00401; Tue, 8 Sep 87 11:20:36 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA17268; Tue,
	8 Sep 87 11:20:32 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00397; Tue, 8 Sep 87 11:20:29 PDT
Received: by billp.unet.uucp (3.2/SMI-3.0DEV3SLAVE1) id AA01663; Tue, 8 Sep 87
	08:14:26 PDT
Return-path: <unet!billp@tymix.Tymnet> 
From: unet!billp (Bill W. Putney) 
Date: Tue, 8 Sep 87 08:14:26 PDT 
To: :wrs@tymix 
Message-id: <8709081514.AA01663@billp.unet.uucp> 
Subject: new bioya phone #'s 

To: BILL PUTNEY                         Memo: 5357586064NET51
                                        Date: Mon  7 Sep 1987  6:59 PM PST
                                        From: FRANK DUARTE
                                        Dept: FLDSVC
                                        Tel:  415/366-4400 X266
                                        Adr:  400 PENOBSCOT DRIVE

Subject: PHONE PATCH ACCESS VIA WB6RFW!

HI BILL:

       
THE OTHER DAY I TRIED TO MAKE A PHONE PATCH TO STANFORD UNIVERSITY.
STANFORD HAS A NEW PREFEX IN EXISTANCE FOR ABOUT A YEAR.  IT IS
    
(415) 725-XXXX.

IT WOULD BE NICE TO HAVE THIS AS VALID CALL LOCATION.
THANKS.
   
73 FRANK

Received: from tymix.Tymnet by B39.Tymnet; Wed, 9 Sep 87 10:48:27 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02710; Wed, 9 Sep 87 10:49:28 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23557; Wed,
	9 Sep 87 10:49:25 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA05809; Wed, 9 Sep 87 10:48:32 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 9 Sep 87 10:48:32 PDT 
To: wrs@hobbes 
Message-id: <8709091748.AA05809@f104.sun.com> 
Subject: Re: x409 

No, apparently since it was a full moon last night the problem has cleared
up.  I haven't changed anything but it works for me too...
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 10 Sep 87 12:53:11 PDT 
To: Carl A. Baltrunas <Carl@X930.Tymnet> 
Subject: PERPOPER 

I just got about 2500 pages (disk) of mail from system 70 that was
returned to PERPOPER because some bimbo never reads his mail but has
6,123,472 perp jobs that run every 4 seconds.  Anyway, I noticed that
systems 29, 55 and 70 didn't have a mail.fwd in (perpoper) so I created
an empty one.  This will send all the mail to the bit bucket rather
than me.  I noticed on the other systems, you had it going to a file,
but I wasn't sure that was a good idea given the huge volume of stuff
that seems to be happening.  Anyway, change it if you want, just PLEASE
make sure it goes somewhere other than to me.  Wasn't sure you knew
about the empty mail.fwd feature.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 10 Sep 87 13:06:29 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 23 Aug 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

45	23	1915	2302

30	12	1915	2302

31	6

20	2

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 10 Sep 87 13:06:30 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 30 Aug 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

45	12	1915	2302

30	21	1915	2302

31	8

20	2

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 10 Sep 87 13:06:30 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 6 Sep 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

45	4	1915	2302

30	24	1915	2302

31	8

20	2

------------------------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Thu, 10 Sep 87 13:56:43 PDT
Return-path: <Postmaster@F29.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 10 Sep 87 13:55:28 PDT 
To: Postmaster@F29.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Error response(s) from SLVMCL1.McAuto.Tymnet ...
SNDMSG000R Transmit message, end with '.':
SNDMSG001R Transmit recipients, end with '.':
DMSDDL545E MISSING OPERAND(S).
SNDMSG003W Send to at SLVMCL1 failed due to error 5
SNDMSG004W Send failed to 1 recipients.
SNDMSG005E Note sent to 0 recipients.
R; T=0.08/0.27 15:55:08



----------
RCPT To: <@.McAuto.Tymnet> 
From: Postmaster@F29.Tymnet 
Date: Thu, 10 Sep 87 13:53:05 PDT 
To: @.McAuto.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No router table entry for "TYMNE.Q".



----------
RCPT To: <2336696@tms.tymne.Tymnet> 
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Thu, 10 Sep 87 13:52:51 PDT
From: @.McAuto.Tymnet 
To: 2336696@tms.tymne.Tymnet 
Subject: twx to litton systems, inc. gcsd, woodland hills, ca. 
ATTN: terre wilkinson - contracts admn.
SUBJ: av-8b program-cancel stop work twx acj987-8

1.  seller is hereby instructed to disregard stop work twx acj987-8
and is to continue work as required by po k60027.

2.  seller is requested to acknowledge this msg and confirm work
is proceeding as required.


a.c. jenkins, mcair
purchasing agent

From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 10 Sep 87 14:11:11 PDT 
To: A.C.Jenkins <M095046@SLVM307.MCAUTO.TYMNET> 
Subject: Returned MAIL 

I'm the acting postmaster for TUMS.  The following message which you sent
to 2336696@TMS was not delivered because you mistyped TYMNET.  I've sent a
copy with that corrected so it should be okay.  The concern, however, is
why TUMS was not able to see your return address (notice the "From:
@.McAuto.Tymnet" in your message).  I'd like to try to trace this problem
down and fix it with your cooperation.

Please send me a note the same way you sent it to 2336696@tms only use
my address <wrs@b39.tymnet> in its place.  If there is anything that you
can think of unusual about the way you sent that message, please include
that in the message.  In particular, is anything unusual about your
signature line or from line in your PROFS profile?

You can reach me at 408-922-6210 if necessary.  Thanks for using TUMS
and for your assistance.

Bill Soley
Tymnet, MDNSC


(begin forwarded message)
----------------------------------------------------------------
From: Postmaster@F29.Tymnet 
Date: Thu, 10 Sep 87 13:53:05 PDT 
To: @.McAuto.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
No router table entry for "TYMNE.Q".



----------
RCPT To: <2336696@tms.tymne.Tymnet> 
Received: From SLVMCL1.McAuto.Tymnet by F29.Tymnet; Thu, 10 Sep 87 13:52:51 PDT
From: @.McAuto.Tymnet 
To: 2336696@tms.tymne.Tymnet 
Subject: twx to litton systems, inc. gcsd, woodland hills, ca. 
ATTN: terre wilkinson - contracts admn.
SUBJ: av-8b program-cancel stop work twx acj987-8

1.  seller is hereby instructed to disregard stop work twx acj987-8
and is to continue work as required by po k60027.

2.  seller is requested to acknowledge this msg and confirm work
is proceeding as required.


a.c. jenkins, mcair
purchasing agent
----------------------------------------------------------------
(end forwarded message)
Received: from F29.Tymnet by B39.Tymnet; Thu, 10 Sep 87 16:18:37 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Thu, 10 Sep 87 16:12:31 PDT
Return-path: <NETS.G/STOY@EMSNTD.Ontyme.Tymnet> 
From: NETS.G/STOY@EMSNTD.Ontyme.Tymnet 
Date: 10 SEP 87 15:54:53 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: (18 names) 
Message-id: I83926@Ontyme.Tymnet 
Subject: 28JUL87 - MEETING MINUTES - TYMNET X.121 NUMBERING PLAN 
PUBLICNETWORK-X.121NUMBERINGPLAN: 

SUBJ: 28JUL87 - MEETING MINUTES - TYMNET X.121 NUMBERING PLAN


     ********************************************************************
                           M E M O R A N D U M
     
                                                  [ ] T Y M N E T
                                                    Network Systems Group
     
     TO:  DISTRIBUTION
     
     CC:  MIKE CADY      RAJA RAJARAMAN
          NEIL SULLIVAN  KEN HOLCOMB
     
     FR:  GARY R. STOY
     
     RE:  PUBLIC NETWORK - X.121 NUMBERING PLAN
          MEETING MINUTES - 28 JULY 1987
     
     ********************************************************************
     
          The Public Network X.121 Numbering Plan meeting was started off
     with a brief introduction by Jeff Oliveto on what the numbering plan
     is and how it affects Tymnet.  The following is a brief description
     of some of the discussion items:
     
        o Current numbering  plan (ADR1)  has duplication of purpose
          between  addressing   formats,  specifically  between  the
          "determination digits", `0' and `9'.
          
        o ADR1 Numbering Plan specifies that when the "determination
          digit" is  `0', the  X.25 called address (or NUA), the NUA
          is to  be included  in Group  19.   This group  offers  no
          password option,  no priority  targeting,  and  no  multi-
          targeting.   This means  that the username that is sent to
          the  Supervisor  is  under  the  originating  host's  CID.
          During  this  call  set  up  arrangement,  the  Supervisor
          assumes that the call is caller paid, even if the call was
          set up with the X.25 Reverse Charge facility.
          
        o ADR1 numbering plan specifies that when the "determination
          digit" is  `7', the call is an origination only NUA.  This
          was used  for nodes  that did  not report originating host
          numbers.   This apparently  has no  practical use  anymore
          since this was reserved for Varian originated calls.
          
        o ADR1 Numbering Plan reserved the "determination digit" `8'
          for PNIC's  (Private Network)  addressing.  This was to be
          assigned for  a new  feature of  XCOM that  is not due for
          release until  Mid-88.   This  appears  to  be  a  useless
          assignment of this digit with the ADR2 numbering plan.  It
          was suggested  that we  enhance the  function of  the  `0'
          option in  the NTN rather then reserving the `8' digit for
          PNIC's.
          
        o It was  mentioned that  the NTN  `9' option  was the  most
          secure in the present numbering scheme and that some Sales
          education was needed for NUN's.
          
        o It was  implied by  Jeff Oliveto that with the change-over
          from the  ADR1 to  the ADR2  numbering plan,  the password
          protection for  NUN's would  be lost.   This is apparently
 
 
 
 
 
 
 
           one of  the main  security features that banking customers
          enjoy about Tymnet.
          
        o It was  mentioned by  Bill Soley that a NUI identifies the
          calling party.   He  also mentioned  that we don't get the
          NUI all the time and we can't transport the NUI uniquely.
          
        o There was mention of nationalized calling cards and if the
          X.121 system  would handle it very well.  Would we need to
          prompt for  the called  user field  in order  to request a
          password or PIN.
          
        o It was  mentioned that  it would  be desirable  to have  a
          macro or  some other method of identifying a traffic class
          of service.
          
     
     The discussion changed to the actual plan for Tymnet style
     numbering.  It turned out that there were many ideas concerning a
     practical numbering plan.  Some of the questions that were brought
     up were:
     
          o    How many digits are actually necessary for host
          addressing and sub-host addressing?
          
          o    What would be a practical method for assigning
          block's of numbers to customers?
          
          o    If we were to get rid of the NTN `7' option
          (origination only), what will that effect?
          
          o    Could we define a PNIC upfront by a `$'?
          
          o    Would the plan for implementation allow for down
          revision versions of Node Code.
          
     
     The following is the closest that the group came to defining a
     numbering plan.  Another meeting will be planned for the future for
     additional discussion.
     
          DNIC - 0hhhhhssss (99,000 Hosts)
                 1hhhhhssss (100K and up, Reserved for future)
                 2nnnnnssss (Reserved for various Hunt Groups)
                 3nnnnnssss (Reserved for various Hunt Groups)
                 4nnnnnssss (Reserved for various Hunt Groups)
                 5nnnnnssss (Reserved for various Hunt Groups)/
                            (also the possibility of various WAN
                            applications (as below))
                 6nnnssssss (Telenet type addressing w/o sub-addressing)
                 7nnsssssss (Telenet type addressing with 1-digit sub-
                 addressing)
                 8          (Possible future elimination of PNIC
                            strategy)
                 9nnnn      NUN
                 
     
     Various types of numbering schemes were addressed but need further
     analysis.  An example of such a scheme is shown below:
     
 
 
 
 
 
 
                          DNIC - 34nnnnssss
                                35nnnsssss
                                36nnssssss
                                37nsssssss
     
     There was a concern on the distinction of abbreviated host
     addressing and how it could be effect by the new numbering scheme.
     Needless to say, the new scheme can not be a problem for this type
     of addressing.
     
     This is the best account that I could make on the meeting that was
     held 28 July 1987.  I welcome any comments, corrections, and most of
     all , additions to supplement these minutes.  This will serve as the
     ground work for future discussion on the future plans for Tymnet
     X.121 numbering.
     
     
                              GARY
     
     
                         DISTRIBUTION                         ____________
               Wayne Bartlett      Steve Poppe
               Edsel Budar         Patty Quevedo
               David Chang         Bill Soley
               Dennis Ernst        Rick Sprenkel
               Bob Fultz           Vida Stafford
               Jeff Oliveto        Gazel Tan
Received: from tymix.Tymnet by B39.Tymnet; Fri, 11 Sep 87 9:57:54 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA10962; Fri, 11 Sep 87 09:59:07 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07085;
	Fri, 11 Sep 87 09:59:01 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA11665; Fri, 11 Sep 87
	09:59:01 PDT
Return-path: <hsing@calvin> 
From: hsing@calvin (Yu-liang Hsing) 
Date: Fri, 11 Sep 87 09:59:01 PDT 
To: wrs@calvin 
Message-id: <8709111659.AA11665@calvin.Tymnet.com> 
Subject: IPCF code walk through 

Could we schedule a time, at your conveience, to walk through the IPCF code
and talk about the system initialization.
Yu-Liang
Received: from tymix.Tymnet by B39.Tymnet; Fri, 11 Sep 87 12:56:26 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA11419; Fri, 11 Sep 87 12:57:29 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09414; Fri, 11 Sep 87
	12:57:26 PDT
Return-path: <akselrod@hobbes> 
From: akselrod@hobbes (Mark Akselrod) 
Date: Fri, 11 Sep 87 12:57:26 PDT 
To: wrs@hobbes 
Message-id: <8709111957.AA09414@hobbes.Tymnet.com> 
Subject: New proposal on the public address 

Hello, Bill.
We had a meeting today this morning and it sounds like at least some
changes in public address structure are inevitable. As far as 16M boundary
is concerned, there were some concerns, Pat will probably inform you about
it. So, I will make this boundary changebale. Of course, if the boundary
is less than 16M, the MMU tables will take much more room (practically
in reverse proportion with the value of the boundary).

Now, suppose the changes to public space will have to be made. My question
is whether you have any specific reason for the public space structure
which you proposed. Moreover, I wonder whether you have any objection
to make the public space structure equal to the new Tadpole physical
address structure. I personnaly do not see anything wrong with it.

Notice, I do not propose to do away with the concept of the public address.
However, Release I could be simplified if physical and public are basically
equal. For one thing, if we do not relocate RAM we will not have any problems
to use the monitor for our debugging. Also, it might be (just might be)
that Rodney is able to disable MMU (or work without MMU) for Release I.
Anyway, I do not see any serious advantage to move thing around if the
whole addressable range is not decreased.

Please, let me know if I am wrong and there is a reason for relocating
which I do not see.

      Best Regards
                    Mark
Received: from tymix.Tymnet by B39.Tymnet; Fri, 11 Sep 87 17:22:23 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12335; Fri, 11 Sep 87 17:23:37 PDT
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13012;
	Fri, 11 Sep 87 17:23:33 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA01598; Fri, 11 Sep 87 17:23:32
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Fri, 11 Sep 87 17:23:32 PDT 
To: julies@comet 
Cc: wrs@comet 
Message-id: <8709120023.AA01598@comet.Tymnet.com> 
Subject: tbl should be better 

I belive that I have patched the psroff header file so that tables
come out looking nice and pretty with all the corners lined up now.
	Steve
Received: from tymix.Tymnet by B39.Tymnet; Sun, 13 Sep 87 12:39:52 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA17379; Sun, 13 Sep 87 12:41:08 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA15955;
	Sun, 13 Sep 87 12:41:02 PDT
Received: from comet.Tymnet.com by calvin.Tymnet.com (3.2/SMI-3.2) id AA26006;
	Sun, 13 Sep 87 12:41:04 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA02851; Sun, 13 Sep 87 12:41:02
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Sun, 13 Sep 87 12:41:02 PDT 
To: wrs@calvin 
Message-id: <8709131941.AA02851@comet.Tymnet.com> 
Subject: Re: lpr -Pdp 

I talked on Friday to someone at Adobe who said that there were race
conditions in pscomm which would cause it to hang sometimes.  He
sent me a new version which I'm about to try.  It should not have been
turned off.  Maybe we need to rig it up so that you get 120V through
your body if you fiddle with the printer without knowing what you're
doing.  I've seen it "hung" several times recently because it wasn't
online or was powered off.
	Steve
Received: from tymix.Tymnet by B39.Tymnet; Sun, 13 Sep 87 13:27:55 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA17422; Sun, 13 Sep 87 13:29:14 PDT
Received: from clouseau.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id
	AA16068; Sun, 13 Sep 87 13:29:08 PDT
Received: by clouseau.Tymnet.com (3.2/SMI-3.2) id AA01406; Sun, 13 Sep 87
	13:28:48 PDT
Return-path: <feldman@clouseau> 
From: feldman@clouseau (Steve Feldman) 
Date: Sun, 13 Sep 87 13:28:48 PDT 
To: foil@clouseau, julies@clouseau, wrs@clouseau 
Message-id: <8709132028.AA01406@clouseau.Tymnet.com> 
Subject: new pscomm installed 

I have installed the new version of "pscomm".  It seems to be working ok,
but we'll have to wait and see if the hanging problem has really gone away.
	Steve
Received: from F29.Tymnet by B39.Tymnet; Tue, 15 Sep 87 4:19:39 PDT
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Tue, 15 Sep 87 4:19:07 PDT
Return-path: <IPC.R/DANIELS@EMSTUMS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTUMS.Ontyme.Tymnet 
Date: 15 SEP 87 09:09:06 
To: TUMS.SUP@EMSTUMS.Ontyme.Tymnet 
Message-id: A12012@Ontyme.Tymnet 
Subject: "THE FOLLOWING ERROR PRINTED OUT"... 

THE FOLLOWING ERROR PRINTED OUT ON F29:

?TUMS: ERROR LOGMFP IN MODULE SMTPX, MESSAGE
IS (LOG0S) LOG FAILURE !CHMFP AC/ 777000015
PLEASE NOTIFY TUMS.SUP VIA ONTYME.
MON 14 SEP 87 15;34;25 PDT
Received: from tymix.Tymnet by B39.Tymnet; Tue, 15 Sep 87 8:59:46 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23431; Tue, 15 Sep 87 09:00:42 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA24332;
	Tue, 15 Sep 87 09:00:33 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA10654; Tue, 15 Sep 87
	09:00:32 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Tue, 15 Sep 87 09:00:32 PDT 
To: wrs@calvin 
Message-id: <8709151600.AA10654@calvin.Tymnet.com> 
Subject: Memory mapping issues 

Bill,

  At our meeting on Friday with Mark, Dave, John, and Thomas the
following issues were raised:

1) 16meg boundary may be acceptable if first 16 meg is used for
   non-Rodney (i.e. off the shelf) boards which only support
    D16 and standard addressing.  No problem for GP ISIS to
   access boards in this area.

2) Mark proposed direct physical to logical map for the GP.
   This would make bringup much easier (MMU is not needed initially)
   and would allow use of on-board monitor without modification.
   Is this acceptable?

3) If the 16meg boundary were ever changed back to a smaller boundary
   then MMU tables would expand greatly.  I don't see any reason which
   the boundary should shrink.  16meg allows 64 processors in a cage.
   The Tadpole board can have it's base address set a increments of
   2 meg.

4) The Tadpole GP may NOT be able to read it's own ram through the VME
   bus.  I haven't had a chance to confirm this with Mike Mo, but will
   do so today.

Comments?

P.S. Mark would like to settle MMU issues ASAP.

Pat
Received: from F29.Tymnet by B39.Tymnet; Wed, 16 Sep 87 1:05:29 PDT
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Wed, 16 Sep 87 1:05:40 PDT
Return-path: <IPC.R/DANIELS@EMSTUMS.Ontyme.Tymnet> 
From: IPC.R/DANIELS@EMSTUMS.Ontyme.Tymnet 
Date: 16 SEP 87 07:42:34 
To: TUMS.SUP@EMSTUMS.Ontyme.Tymnet 
Message-id: A12456@Ontyme.Tymnet 
Subject: "The following error printed out"... 

The following error printed out on F29:

?TUMS:  error LOGDEL in module MAILER, message is
log failure RENAME 9MAIL)'202120604357.LOG
Please notify TUMS.SUP via Ontyme.
Tue, 15 SEP 87 23:24:10 PDT.

Thanks Rick Daniels
Received: from F29.Tymnet by B39.Tymnet; Wed, 16 Sep 87 2:45:36 PDT
Received: from EMSTUMS.Ontyme.Tymnet by F29.Tymnet; Wed, 16 Sep 87 2:45:49 PDT
Return-path: <OPERA.L/WALLING@EMSTUMS.Ontyme.Tymnet> 
From: OPERA.L/WALLING@EMSTUMS.Ontyme.Tymnet 
Date: 16 SEP 87 09:23:36 
To: TUMS.SUP@EMSTUMS.Ontyme.Tymnet 
Message-id: A12465@Ontyme.Tymnet 
Subject: F29 

THE FOLLOWING ERROR PRINTED OUT ON F29:
?TUMS:  ERROR LOGDEL IN MODULE MAILER, MESSAGE 
IS LOG FAILURE RENAME 9MAIL)'202120604357.LOG
TUE,  15 SEP 87 23:24:10 PDT.
THANK YOU, FRMT OPS/LYN
From: Postmaster@B39.Tymnet 
Date: Wed, 16 Sep 87 15:41:05 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No router table entry for "32.Q".



----------
RCPT To: <JMS@X32.Tymnet> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 16 Sep 87 15:33:08 PDT 
To: <JMS@X32.Tymnet> 
Bcc: WRS@B39.Tymnet 
Subject: Re: "Two people have asked do you get"... 
In-reply-to: your message of Wed, 16 Sep 87 14:07:57 PDT

The project was canceled before anyone wrote TUMS.HLP, so...  there is
actually no documentation.  If he wants his Ontymes forwarded, he just
needs to send me his ontyme name, password and PDP-10 user name.  It is
somewhat expensive, so I don't want to do a whole bunch, but a few is OK.
There is no interface at this time to the 370 except through PROFS which
I think they are not running.  If he is in the MDC PROFS system, he can
send mail to the PDP-10 via PROFS by sending a NOTE (must be a note) to
the address TUMSGATE SLVMCL1 and including the real destination address
in the first line of the note as follows (for example):

To: Bill SOley <WRS@B39.Tymnet> (TUMS)

The "To:" must begin in column 1 and the (TUMS) must not have any
trailing blanks.  He can send to multiple addresses this way, by
just seperating them with commas or using multiple To: lines or both.
He can also leave out the name and <>...

To: WRS@B39.Tymnet (TUMS)

Oh well.  -Bill
From: Postmaster@B39.Tymnet 
Date: Thu, 17 Sep 87 0:02:17 UT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No router table entry for "32.Q".



----------
RCPT To: <JMS@X32> 
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 16 Sep 87 16:34:23 PDT 
To: <JMS@X32> 
Bcc: WRS@B39.Tymnet 
Subject: Re: Host 14 is still on 940NET. 
In-reply-to: your message of Wed, 16 Sep 87 18:25:47 CDT

I don't know, I've forwarded your mail to Gazel.  If you don't hear from
her, let me know and I'll follow up.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 18 Sep 87 11:15:52 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA16670; Fri, 18 Sep 87 11:20:17 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18495;
	Fri, 18 Sep 87 11:20:05 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA06810; Fri, 18 Sep 87
	11:19:59 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Fri, 18 Sep 87 11:19:59 PDT 
To: wrs@calvin 
Message-id: <8709181819.AA06810@calvin.Tymnet.com> 
Subject: dfx2ps 

I tried to make address in RED and the shell couldn't find dfx2ps, which
I believe is symbolically linked to your directory.  Help!

Pat
Received: from F29.Tymnet by B39.Tymnet; Fri, 18 Sep 87 15:59:52 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 18 Sep 87 15:55:54 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 18 SEP 87 15:14:02 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I87461@Ontyme.Tymnet 
Subject: NEW ADDRESS/TELEPHONE NUMBERS: 

                                   MEMORANDUM
                               NETWORK SYSTEMS COMPANY
                       QUALITY SERVICES AND TECHNICAL SUPPORT

          TO:       DISTRIBUTION                       CC:  DON JOHNSON
          FROM:     GLORIA KOLEK                            GARY WALKER
          DATE:     11 SEPTEMBER 1987                       ROURKE McCUSKER
          SUBJECT:  NEW ADDRESS/TELEPHONE NUMBERS:
                    (QSATS) N.T.S., S.T.S, Q.A.
          _________________________________________________________________

          NEW ADDRESS FOR U.S. MAIL DELIVERY:
          McDONNELL DOUGLAS CENTER
          (DEPT OR EMPLOYEE NAME + MAIL STOP)  HTS, STS, QA: M.S. F-21
          2560 N. FIRST STREET
          P. O. BOX 49019
          SAN JOSE, CA 95161-9019

          NEW ADDRESS FOR PRIVATE DELIVERY SERVICE (UPS, FED-EX, DHL, ETC)
          McDONNELL DOUGLAS CENTER
          (DEPT OR EMPLOYEE NAME + MAIL STOP)  HTS, STS, QA: M.S. F-21
          2560 N. FIRST STREET
          SAN JOSE, CA 95131

          TELEPHONE LISTING FOR QSATS (HTS, STS, QA):
          AREA CODE: 408      PREFIX NUMBER FOR ALL EXTENSIONS: 922

          NAME________________     DEPARTMENT__________________  EXTENSION
          AKELLA, RAM              QUALITY ASSURANCE             7461
          ARNETT, MARY             QUALITY ASSURANCE             7462
          BUBSEY, GAIL             STS (INTERFACE SUPPORT)       7463
          BURROUGH, JOHN           STS (INTERFACE SUPPORT)       7464
          CATTANEO, AUGUST         NTS (H/W TECH. SUPPORT)       7466
          CHANG, VERA              QUALITY ASSURANCE             7469
          CHEN, BEN                STS (NETWORK SUPPORT)         7468
          CHEN, CATHY              QUALITY ASSURANCE             7469
          CHENG, HENRY             QUALITY ASSURANCE             7470
          CHIANG, SHIRLEY          STS (INTERFACE SUPP, MGR)     7471
          CHU, PING                STS (INTERFACE SUPPORT)       7472
          CLARIDGE, JON            QUALITY ASSURANCE             7473
          CURRI, LOUISE            STS (STS TECH. ANALYST)       7477
          DAZZI, ALAIN             NTS (H/W SUPPORT)             7669
          DIAS, LARRY              QUALITY ASSURANCE             7478
          ELLENBERG, DOUG          STS (APPLICATIONS SUPP, MGR)  7480
          FANG, DAN                STS (INTERFACE SUPPORT)       7481
          FISTE, TOD               STS (INTERFACE SUPPORT)       7484
          HEALY, ANGELA            STS (NETWORK SUPPORT)         6741
          HO, RICHARD              STS (INTERFACE SUPPORT)       7486
          HOUGE, DIANA             QUALITY ASSURANCE             7487
          HUSSEY, DAVE             QUALITY ASSURANCE             7489
          JOHNSON, DON             QSATS (MANAGER: NTS & QA)     6222
          KOCHMANN, TIM            NTS (H/W TECH. SUPPORT)       7493
          KOLEK, GLORIA            QSATS (NTS, STS, QA, SECY)    7495
          KRUMVIEDE, PAUL          NTS (H/W, S/W DIAG. SUPPORT)  7496












          LANGDON, CARL            STS (NETWORK SUPP, MGR)       7497
          LIU, JEFF                STS (APPLICATIONS SUPPORT)    7498
          LUCAS, JERRY             STS (NETWORK SUPPORT)         7499
          MATUCHA, DEAN            QUALITY ASSURANCE             7500
          McCUSKER, ROURKE         QUALITY ASSURANCE (MANAGER)   7576
          McKIERNAN, JOHN          STS (INTERFACE SUPPORT)       7501
          MISTRY, PRAVIN           QUALITY ASSURANCE             7502
          MOORING, ED              NTS (H/W, S/W, DIAG. SUPP)    7504
          MORDEN, JEFF             STS (INTERFACE SUPPORT)       7506
          NARAYAN, KRISHNAN        STS (APPLICATIONS SUPPORT)    7507
          NELSON, PHIL             NTS (H/W TECH. SUPPORT)       7508
          NILLUKA, SANDRA          STS (NETWORK SUPPORT)         7510
          REILLY, JOHN             STS (NETWORK SUPPORT)         7447
          RICHARDS, STAN           NTS (H/W TECH. SUPPORT)       7513
          ROWE, CLIVE              STS (NETWORK SUPPORT)         7514
          SHARMA, ALOK             STS (INTERFACE SUPPORT)       7516
          SHEPHERD, BRUCE          NTS (H/W TECH. SUPPORT)       7517
          SHUTICH, JOHN            NTS (H/W TECH. SUPPORT)       7069
          STEIN, JIM               NTS (H/W TECH. SUPPORT, MGR)  7518
          TURNER, TOM              QUALITY ASSURANCE             7520
          VIVIER, RON              NTS (H/W S/W DIAG SUPP, MGR)  7521
          WALKER, GARY             STS (MANAGER: APPL,N/W,I/F)   7523
          WANG, JAMES              STS (NETWORK SUPPORT)         7524
          WARNER, RUTH             QUALITY ASSURANCE             7525
          YEH, TED                 STS (APPLICATIONS SUPPORT)    7528

          ONTYME LISTINGS WILL REMAIN THE SAME: QSATS.-/----


          NOTE #1:
          FIELD ENGINEERING HANDBOOK IS COMPLETE AND AVAILABLE TO ORDER.
          PLEASE CONTACT YOUR MANAGER FOR FURTHER DETAILS.


          NOTE #2:
          DUE TO "CIRCUMSTANCES BEYOND OUR CONTROL",  OUR MOVE WAS  DELAYED
          FOR ONE WEEK.  WE WERE ORIGINALLY SCHEDULED TO MOVE ON FRIDAY, 11
          SEPTEMBER 1987;  OUR NEW MOVING DATE IS TARGETED FOR  FRIDAY,  18
          SEPTEMBER 1987.   ANY MAIL BY U.S. POSTAL SERVICE OR PRIVATE MAIL
          CARRIER WILL BE DELIVERED TO NEW MAILING ADDRESS AND REROUTED  TO
          ORCHARD PARKWAY.

          PRESENT  TELEPHONE  NUMBERS  FOR NTS,  STS AND QA ARE STILL VALID
          UNTIL FURTHER NOTICE.

          WHEN TELEPHONE  NUMBERS  ARE  CHANGED,  AND  YOU  PHONE  OUR  OLD
          NUMBERS, THERE WILL BE A RECORDED MESSAGE STATING THAT THESE
          NUMBERS HAVE BEEN DISCONNECTED AND YOU WILL BE GIVEN A NEW NUMBER
          TO USE.

          IF IN DOUBT, PLEASE CALL CORPORATE TELEPHONE OPERATOR AT
          (408) 922-0250.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 18 Sep 87 16:09:27 PDT 
To: <dfields@opus> 
Subject: Re: Mail 
In-reply-to: <8709021824.AA13135@opus.Tymnet.com> of Wed, 2 Sep 87 11:24:21 PDT

You ontymes should now be forwarded.  Sorry for the delay, your message
got lost in the volume and I just now saw it when going through to delete
old mail.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 18 Sep 87 17:33:48 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19083; Fri, 18 Sep 87 17:38:11 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA21401;
	Fri, 18 Sep 87 17:37:57 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA06341; Fri, 18 Sep 87
	17:37:52 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 18 Sep 87 17:37:52 PDT 
To: wrs@calvin 
Message-id: <8709190037.AA06341@calvin.Tymnet.com> 
Subject: "Bill, If I understand you correctly"... 

Bill,
If I understand you correctly if something is initialized during
the definition then it goes into ROM?
how about automatic variables inside functions.
please give me a clue how to tell if a thing will go into ROM/RAM.
Mike
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 18 Sep 87 18:19:56 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, If I understand you correctly"... 
In-reply-to: <8709190037.AA06341@calvin.Tymnet.com> of Fri, 18 Sep 87 17:37:52
	PDT

I believe that all initialized static variables go in ROM.  The MICROTEC
manual say on page 5-24 that the -r option places "initialized local data
in a seperate section so that it can be placed in ROM".  I guess it means
static when it says local.  We need to use -r and -S in order to have that
kind of stuff work properly.  Without it, an initialized array would go in
RAM, but it wouldn't get initialized.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 18 Sep 87 18:27:54 PDT 
To: dchang 
Subject: garbaged TUMS 

You didn't exactly lose anything, I don't think.  Best as I can
tell, Omar sent you a message with a bunch of ^D's in it and so
it got broken up into lots of tiny messages which didn't have
message headers.  Maybe he can send it again without the ^D's.
-Bill  (this is just a guess from looking at your TUMS.MSG with
the editor)
From: Scott Wedel <SWEDEL@B39.Tymnet> 
Date: Sat, 19 Sep 87 3:05:58 PDT 
To: John Kopf <KopfJ@B39.Tymnet>, Romolo Raffo <NTD.R/RAFFO@Ontyme.Tymnet>,
	Dick Rawson <NTD.D/RAWSON@Ontyme.Tymnet>, Adele Newman
	<NTD.A/NEWMAN@Ontyme.Tymnet>, Dave Smith <NTD.D/SMITH@Ontyme.Tymnet>,
	dchang, Gazel Tan <GAZEL@B39>, Paul Chu <NTD.P/CHU@Ontyme.Tymnet>,
	Diana Tai <NTD.D/TAI@Ontyme.Tymnet>, Bill Soley <WRS@B39.Tymnet>, Rich
	Teng <RICHTENG@B39.Tymnet>, Ashok Chippa <ACHIPPA@B39>, Henry Rivers
	<NTD.H/RIVERS@Ontyme.Tymnet>, Nancy Holt
	<NTD.N/BURRIESCI@Ontyme.Tymnet>, Dennis Jonas <JONAS@B39>, Scott Wedel
	<NTD.S/WEDEL@Ontyme.Tymnet>, Wayne Bartlett
	<TYM.W/BARTLETT@Ontyme.Tymnet> 
Subject: Proposed change to LOG II 

Since Omar has gone on to other documents, I have taken over maintenance
of the LOG II GED.  Thus, from now on please direct your comments, suggestions
and questions about the document to me.  Also, I have decided to notify
the affected parties of any proposed changes to the document.

Proposed Change:  I suggest an alternate sequence of LOG II ISIS messages
and LOG II message objects (MOs) to initiate a circuit.  I suggest that
if the originating interface sends an ICRM ISIS message that Node Code
will return the ASLP.  The ICRM MO will then be sent by the originating
interface in the initial ICRS ISIS msg.

           Current LOG II                       Proposed Alternative
 Slot       Node Code    Supervisor       Slot        Node Code   Supervisor
 ----       ---------    ----------       ----        ---------   ----------
ISIS ICRM +                               ISIS ICRM
ICRM MO                                   on port 0 --->
on port 0 -->                                    ASLP
       ASLP        NCRM--->                 <--assigns port
  <-assigns port                         ISIS ICRS +
                   <---RCRM              ICRM MO--->
      ISIS RCRI+                                            NCRM-->
  <---RCRI MOs                                              <---RCRM
ISIS ICRS +                                  ISIS RCRI+
ICRS MO---->                              <--RCRI MO
                  NCRM--->                ISIS ICRS+
and so on until succesfull logon          ICRS MO--->
                                                           NCRM--->
                                          and soon until sucessful logon

The proposed alternative would allow slots to recieve an ASLP and hence
an assigned dispatcher port before sending logon data to the supe.  The
current LOG II requires all of the initial logon data be sent on the
initial message (ICRM) which is sent on port 0.  Please note that the
current LOGII method of initiating a circuit remains unchanged.  This
change is to allow an alternate method.  Also, this alternate method
is invisible to supe and ISIS (unless ISIS examines the data within the
ICRM and ICRS messages). 
                                                sdw
Received: from X32.Tymnet by B39.Tymnet; Sun, 20 Sep 87 21:28:35 PDT
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl@X32.Tymnet> 
Date: Sun, 20 Sep 87 21:28:58 PDT 
To: Monitor Group <CARL@X32>, Osman Guven <Osman@X32.Tymnet>, Joe Smith
	<JMS@X32.Tymnet> 
Cc: Craig Fletcher <fletcherc@X32.Tymnet>, Dennis Coffey <dencoff@X32.Tymnet>,
	William R Soley <WRS@B39.Tymnet> 
Subject: re:FINDIT %10 

New FINDIT %10 to be distributed soon...  Now includes ARGPAR.

The only visible change to anyone using FINDIT the old way is that the
line "End of SAIL execution." is no longer printed in any case, and some
programs such as RDIST may look for such a line to terminate processing.
The prompt has NOT been changed, so programs waiting for a second prompt
should not be affected.

/Carl
Received: from tymix.Tymnet by B39.Tymnet; Mon, 21 Sep 87 11:26:10 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12054; Mon, 21 Sep 87 11:30:35 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA06675;
	Mon, 21 Sep 87 11:30:17 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA13518; Mon, 21 Sep 87
	11:30:08 PDT
Return-path: <akselrod@calvin> 
From: akselrod@calvin (Mark Akselrod) 
Date: Mon, 21 Sep 87 11:30:08 PDT 
To: wrs@calvin 
Message-id: <8709211830.AA13518@calvin.Tymnet.com> 
Subject: public address memo 

Hello, Bill.
Pat showed me your change to "Public Adddresses" section. I just wanted to
make a couple of remarks about it. First, there are a couple of paragraphs
there where you atlk about address translation performed by MMU. I guess
they have to be changed too. Second, I would like to suggest that any
references to "subject to change" are removed from our documents. I am
kind of nervous about this type of things.
      Best regards
               Mark
Received: from tymix.Tymnet by B39.Tymnet; Mon, 21 Sep 87 13:03:13 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12827; Mon, 21 Sep 87 13:07:41 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07228;
	Mon, 21 Sep 87 13:07:22 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14285; Mon, 21 Sep 87
	13:07:12 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 21 Sep 87 13:07:12 PDT 
To: wrs@calvin 
Message-id: <8709212007.AA14285@calvin.Tymnet.com> 
Subject: "Bill, when I say in the beginning"... 

Bill,
when I say in the beginning - void BootMain();
and later -
void 
BootMain()
{....
}
- for some reason it complains that I am redefining BootMain().
The problem is even more mysterious because it complains only about
the very first function from the list of function types that 
are being declared in the beginning of the file. by the way-
around - void Bootmain(); it also points to void as a syntax
error ???
Mike 
Received: from tymix.Tymnet by B39.Tymnet; Mon, 21 Sep 87 14:08:26 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13246; Mon, 21 Sep 87 14:12:32 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07907;
	Mon, 21 Sep 87 14:12:11 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA10257; Mon, 21 Sep 87 14:12:25
	PDT
Return-path: <coons@opus> 
From: coons@opus (John Coons) 
Date: Mon, 21 Sep 87 14:12:25 PDT 
To: coons@hobbes, dlasater@b39, wrs@hobbes 
Message-id: <8709212112.AA10257@opus.Tymnet.com> 
Subject: Re: Applied Microsystems source debugger 

Bill,

Source-level debug support for the Sun is not yet available ... it should
have been completed by now (drivers being written by A/M) but is delayed.
Their salesman said something to the effect of "we're mainly a hardware
company and the software projects are not as well managed ..."

We're requisitioning 1 IBM/PC software package for evaluation (10 day).
At the end of that period, we either buy or give back.
Would you like to partcipate in the evaluation?

Regards,

John
Received: from tymix.Tymnet by B39.Tymnet; Mon, 21 Sep 87 15:21:33 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13711; Mon, 21 Sep 87 15:25:59 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA08762;
	Mon, 21 Sep 87 15:25:40 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA15826; Mon, 21 Sep 87
	15:25:30 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 21 Sep 87 15:25:30 PDT 
To: wrs@calvin 
Message-id: <8709212225.AA15826@calvin.Tymnet.com> 
Subject: "problem has been solved- there"... 

"void" problem has been solved- there was a bug in x409.h.
Mike
Received: from tymix.Tymnet by B39.Tymnet; Mon, 21 Sep 87 15:44:02 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13745; Mon, 21 Sep 87 15:48:30 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA08982;
	Mon, 21 Sep 87 15:48:12 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA16090; Mon, 21 Sep 87
	15:48:02 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 21 Sep 87 15:48:02 PDT 
To: wrs@calvin 
Message-id: <8709212248.AA16090@calvin.Tymnet.com> 
Subject: "Bill, Structure soft is no longer"... 

Bill,
Structure soft is no longer in your glob, that is why I will
remove productName and minor/majorVersion from the list of arguments
to rkPatronUp(). there will be no arguments.
Mike
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 21 Sep 87 15:51:59 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: IBM PC 

Bill,

What do you plan to do with you company IBM PC when you get your SUN?

I've found a used PC that I can hopefully get for a reasonable price
but I'd rather have a company PC at home, since it is just for company work.
Let me know if yours is going to end up being a spare.

Thanks....Omar


From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 21 Sep 87 16:18:35 PDT 
To: <NSMS.B/CORENSON@EMSNTD.Ontyme.Tymnet> 
Subject: Re: Please call 
In-reply-to: I87563@Ontyme.Tymnet of 20 SEP 87 13:50:29

My home number is 866-0443 (and its even listed).  I've left a couple
messages for you on your service and never got any answer on your home
number.  Actually I want to talk to you about outside of work stuff as
well.  Such a coincidence.  PS - who does Steve Kim work for?  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 21 Sep 87 16:36:56 PDT 
To: Omar A. Serang <OSerang@B39.Tymnet> 
Subject: Re: IBM PC 
In-reply-to: your message of Mon, 21 Sep 87 15:51:59 PDT

It'll be a spare when the Company replaces it with a Sun, or something.
I'll let you know.  Thanks -Bill
Received: from tymix.Tymnet by B39.Tymnet; Mon, 21 Sep 87 16:51:23 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13983; Mon, 21 Sep 87 16:55:52 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09799;
	Mon, 21 Sep 87 16:55:34 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA16896; Mon, 21 Sep 87
	16:55:24 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 21 Sep 87 16:55:24 PDT 
To: wrs@calvin 
Message-id: <8709212355.AA16896@calvin.Tymnet.com> 
Subject: "Bill, please add to your enum"... 

Bill,
please add to your enum RodneyError:
RERR_BOOT_QUIT and RERR_BOOT_RETRY.
They will be used in the BootRsp message to describe the final state
of Boot.
Mike J.
Received: from tymix.Tymnet by B39.Tymnet; Mon, 21 Sep 87 17:20:14 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14033; Mon, 21 Sep 87 17:24:46 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA10125;
	Mon, 21 Sep 87 17:24:28 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA17176; Mon, 21 Sep 87
	17:24:18 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 21 Sep 87 17:24:18 PDT 
To: wrs@calvin 
Message-id: <8709220024.AA17176@calvin.Tymnet.com> 
Subject: "Bill, remove from rodney.hreferences"... 

Bill,
remove from rodney.hreferences to rkPatronUp(), rkSetBoot() and
rkGetBoot(). they exist in boot.h.
thanks
Mike J.
From: Scott Wedel <SWEDEL@B39.Tymnet> 
Date: Mon, 21 Sep 87 22:36:25 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: LOADII weirdness 

Bill,
I don't know who to tell about some unexpected behaivor from loadii and I
guessed you.

When I gfd to Albert Lin's directory and type LOADII then LOADII logs me
out with the msg 'LOADII MAY NOT BE COPIED OR USED WITH AUX CIR'.  But,
I did not use aux circuit.  Albert has protection on his directory and
this does not happen if I type 'r loadii'.  Just thought that someone might
care or wonder.
                                                sdw
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 22 Sep 87 4:51:15 PDT 
To: Scott Wedel <SWEDEL@B39.Tymnet> 
Subject: Re: LOADII weirdness 
In-reply-to: your message of Mon, 21 Sep 87 22:36:25 PDT

When you GFD to ALIN, and type "LOADII" it runs (ALIN)LOADII since the
GFD directory appears on your search list before (SYS).  When you use
"R LOADII" that explicitly runs (SYS)LOADII.  The LOADII program checks
to see where it was run from and complains if it was not from SYS.  I
don't know why Albert has a copy of LOADII, but that's the problem.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 22 Sep 87 9:49:42 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA16779; Tue, 22 Sep 87 09:53:50 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA13141;
	Tue, 22 Sep 87 09:53:30 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA12573; Tue, 22 Sep 87 09:53:46
	PDT
Return-path: <rongchu@opus> 
From: rongchu@opus (Rong Chu) 
Date: Tue, 22 Sep 87 09:53:46 PDT 
To: wrs@hobbes 
Message-id: <8709221653.AA12573@opus.Tymnet.com> 
Subject: Patron scheduler for testing 

Bill:

	For testing purpose,  I need a minimum size patron scheduler.
	Please take a look the following routine and give me an advisor.


	main()
	{
		sysInit();		/* Power up or reset system init */
		patroninit();		/* Patron initialization */
		while (1) {
			rkbgRun();	/* Rodney background */
			patronbkgd();	/* Patron background */
		}
	}

	Does this routine satisfy the requirement for Rodney?
	Thank you for your attention and help !

                      - from Rong Chu   x6086
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Tue, 22 Sep 87 10:57:23 PDT 
To: WRS@C39.Tymnet 
Subject: "skywalker security, etc not sure"... 

skywalker security, etc - not sure if Omar sent you the following - also
you may want to review the doc. ( i would appreciate any comments )
----------------------------------------------------------------
Received: from F29.Tymnet by B39.Tymnet; Fri, 18 Sep 87 13:18:03 PDT
Received: from EMSTYM.Ontyme.Tymnet by F29.Tymnet; Fri, 18 Sep 87 13:22:19 PDT
Received: from F29.Tymnet by EMSNTD.Ontyme.Tymnet; Fri, 18 Sep 87 13:13:59 PDT
Received: from B39.Tymnet by F29.Tymnet; Fri, 18 Sep 87 13:13:38 PDT
Return-path: <OSerang@B39.Tymnet> 
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Fri, 18 Sep 87 13:04:50 PDT 
To: Wayne Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet> 
Subject: Password Expiration date in MUDII 

Wayne,

I asked Romolo Raffo about the password expiration date in the MUDII.
This was included in response to a request from DuPont.  The idea was
to have the Supervisor detect when a user's password was expired and
to route the circuit to NETVAL rather than the user specified destination.
This would force users to change their password.

I have re-designed the Skywalker access license bits scheme.  Gazel
will call a meeting in the near future to discuss this issue.  If
you would like to preview the proposed design, it is on (SUPDEV:39)
SKYLIC.DOC.  The design will be reviewed on Monday Sept 21, by the
Skywalker team, so it is not finalized, though I don't anticipate
any changes.

Thanks...Omar
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Tue, 22 Sep 87 11:54:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA17133; Tue, 22 Sep 87 11:58:15 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16059;
	Tue, 22 Sep 87 11:57:55 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA13191; Tue, 22 Sep 87 11:58:11
	PDT
Return-path: <tsay@opus> 
From: tsay@opus (Hai-Fang Tsay) 
Date: Tue, 22 Sep 87 11:58:11 PDT 
To: wrs@hobbes 
Message-id: <8709221858.AA13191@opus.Tymnet.com> 
Subject: RSC 

Bill:
	I need to get your advice on RODNEY system call interface.

     1. All the rodney system call functions have various number of
	arguments and they return different data type. 
	I have no way to build up a system call table which can be
	used  to pass control to the specified system call in trap
	handler routine.
     2. Where can I find definition of ENUM Rsi_event which is used 
	in function rsiSet() and rsiQuery() ?

					Hai-Fang
Received: from tymix.Tymnet by B39.Tymnet; Tue, 22 Sep 87 13:57:00 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18111; Tue, 22 Sep 87 14:01:10 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA17156;
	Tue, 22 Sep 87 14:00:50 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA04216; Tue, 22 Sep 87
	14:00:39 PDT
Return-path: <akselrod@calvin> 
From: akselrod@calvin (Mark Akselrod) 
Date: Tue, 22 Sep 87 14:00:39 PDT 
To: wrs@calvin 
Message-id: <8709222100.AA04216@calvin.Tymnet.com> 
Subject: Public addresses and MMU 

Hello, Bill.
Sorry to bother you again. I would like just to check with you one place
in your revised document. At the bottom of page 3 it states that modifying
MMU for VME-to-local references is "the only way that a Tadpole board
can verify its own VME address ....". This is, of course, true if you mean
"double check" by "verify". Of course, MMU cannot be modified until
the board own address is known by means independent from MMU. It could be
better to change this place in the document, so that this distinction
is more apparent.
          Best regards
                  Mark
Received: from tymix.Tymnet by B39.Tymnet; Tue, 22 Sep 87 15:58:49 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18291; Tue, 22 Sep 87 16:02:59 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18646;
	Tue, 22 Sep 87 16:02:38 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA07457; Tue, 22 Sep 87
	16:02:26 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 22 Sep 87 16:02:26 PDT 
To: wrs@calvin 
Message-id: <8709222302.AA07457@calvin.Tymnet.com> 
Subject: "Bill, in case my previous msgs"... 

Bill,
in case my previous msgs got lost:
please remove from rodney.h references to rkPatronUp(), rkSetBoot(0...
...etc. they are all included in boot.h.

also please add to RodneyError:
RERR_BOOT_QUIT  and  RERR_BOOT_RETRY
they are possible (including _OKAY) errorCode-s in the boot response
message.

also since glob.soft no longer exists rkPatronUp() will have NO
arguments. is it ok with you? (I have no alternatives)

thanks
Mike
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 0:59:45 PDT 
To: <oliveb!gnome@tymix.Tymnet> 
Subject: Re: forwarded stuff 
In-reply-to: <8709230051.AA23223@oliven.ATC.OLIVETTI.COM> of Tue, 22 Sep 87
	17:51:53 PDT

Thanks for the Electric Dreams.  By the way, you get shotime, I'd like to
archive it its on 3 times, yet.  If not, I think Putney does, but not sure
if he has 4 head although not a big deal.  -Bill
Received: from X32.Tymnet by B39.Tymnet; Wed, 23 Sep 87 1:34:32 PDT
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Wed, 23 Sep 87 1:33:52 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: Meeting 

Did you both decide on a meeting time?
  I've not done anything more since our last meeting with SETOP.  I have
however, completed NAD and PEAK.  PEAK is on (SYS) on all PDP_10 systems.
NAD has been "tested" and still resides in (CARL:39)NAD.SAV for more
intensive testing (if anyone feels it necessary, or if some QA procedure
exists for NAD before it is released... however, I doubt it).

  I've not heard back from Dennis Ernst about transmitting it.  I guess
I cando it, but I'd prefer someone in NTD since there are "procedures".

Let me know ... CARL@D35.Tymnet ... if we need to have a review meeting
at this point (soetime this week).

/Carl
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Wed, 23 Sep 87 8:56:29 PDT 
To: <Carl@X32.Tymnet> 
Cc: WRS@C39.Tymnet 
Subject: Re: Meeting 
In-reply-to: your message of Wed, 23 Sep 87 1:33:52 PDT

re the meeting - not this week - but how aboutnext week on thurs -
primary topic "new setop" - plus followup on action re nad etc.
how about tues @ 2:00 or thurs @ 2:00 - or we could meet fot lunch,
and follow with the meeting - also - Bill- can you join????
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Wed, 23 Sep 87 9:17:22 PDT 
To: <QSATS.D/ELLENBERG@EMSTYM.Ontyme.Tymnet> 
Cc: WRS@C39.Tymnet, CARL@X930, INTL.VIDA@ONTYME.TYMNET,
	TYMOPS.DJM@ONTYME.TYMNET 
Subject: Re: Proposal 
In-reply-to: M19398@Ontyme.Tymnet of 16 SEP 87 14:18:03


Great news - PEAK now requires no tymlicense!!! and the NAD revisions
are completed - SEE ATTACHED!!!! Can you get things implemented (with 
help, of course, but you be the "program mgr") We (validations, Vida)
will get tymlicense removed if you tell us the usernames to change, 
and this also relates to the "good guy" names to go on the new GAN, etc.
PLEASE LET ME KNOW WHAT THE PLAN AND SCHEDULE ARE!!!!!!
And one more thing - the loading of NAD (see Carl's memo below)
----------------------------------------------------------------
Received: from X32.Tymnet by B39.Tymnet; Tue, 22 Sep 87 18:04:09 PDT
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Tue, 22 Sep 87 18:05:49 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: PEAK removeal of TYM check 

I've found the proper sources for PEAK and generated a new version that
no longer checks for TYM-Proprietary.  It is installed on all systems.

/Carl
----------------------------------------------------------------
Received: from X32.Tymnet by B39.Tymnet; Wed, 23 Sep 87 1:34:32 PDT
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Wed, 23 Sep 87 1:33:52 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: Meeting 

Did you both decide on a meeting time?
  I've not done anything more since our last meeting with SETOP.  I have
however, completed NAD and PEAK.  PEAK is on (SYS) on all PDP_10 systems.
NAD has been "tested" and still resides in (CARL:39)NAD.SAV for more
intensive testing (if anyone feels it necessary, or if some QA procedure
exists for NAD before it is released... however, I doubt it).

  I've not heard back from Dennis Ernst about transmitting it.  I guess
I cando it, but I'd prefer someone in NTD since there are "procedures".

Let me know ... CARL@D35.Tymnet ... if we need to have a review meeting
at this point (soetime this week).

/Carl
----------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 9:46:40 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22434; Wed, 23 Sep 87 09:50:58 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA10026; Wed, 23 Sep 87
	09:50:55 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 23 Sep 87 09:50:55 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8709231650.AA10026@calvin.Tymnet.com> 
Subject: Re: public address memo 

Bill,

 I will publish the public address section today, unless you object,
at the core team meeting.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 10:16:23 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22540; Wed, 23 Sep 87 10:20:36 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23271;
	Wed, 23 Sep 87 10:20:32 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA10425; Wed, 23 Sep 87
	10:20:25 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 23 Sep 87 10:20:25 PDT 
To: wrs@calvin 
Message-id: <8709231720.AA10425@calvin.Tymnet.com> 
Subject: RED 

Bill,

  Has rkShare gone away? If so then RSI_NOTIFY in boot.mm needs
modification.  Also my notes shows that rkPatronUp has lost
its three arguments, productName, majorVersion, minorVersion.
Is this ok?  Also rkConfess seems to have disappeared.

Pat
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 10:25:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22568; Wed, 23 Sep 87 10:29:20 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23504;
	Wed, 23 Sep 87 10:29:15 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA10574; Wed, 23 Sep 87
	10:29:12 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 23 Sep 87 10:29:12 PDT 
To: wrs@calvin 
Message-id: <8709231729.AA10574@calvin.Tymnet.com> 
Subject: Makefile 

While you're editing the makefile, delete debug.r\.  Thanks.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 11:05:55 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22673; Wed, 23 Sep 87 11:10:04 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA24245; Wed,
	23 Sep 87 11:09:59 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA22666; Wed, 23 Sep 87 11:09:55 PDT
Received: by oliveb.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA10969; Wed, 23
	Sep 87 10:48:51 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA29252; Wed,
	23 Sep 87 10:48:45 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.OLIVETTI.COM (Gary) 
Date: Wed, 23 Sep 87 10:48:45 PDT 
To: oliveb!tymix!wrs 
Message-id: <8709231748.AA29252@oliven.ATC.OLIVETTI.COM> 
Subject: Showtime, etc 


Well, no, actually, I don't get any premium services and Showtime is
definately one.  Another thing is that my deck (2900) is getting flakey.
The night before last, it lost chroma lock on playback (rainbows and then
B/W) -- I dug out the schematics for it last night but it didn't fail.
Unfortunately, I don't know if it affects record operation (it shouldn't -
maybe).

I hope I don't find that all the up-coming Headroom shows I tape have
no color-burst!


How are you doing?   Are you eating?

Gary
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 13:05:36 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, in case my previous msgs"... 
In-reply-to: <8709222302.AA07457@calvin.Tymnet.com> of Tue, 22 Sep 87 16:02:26
	PDT

Okay - will do.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 13:07:50 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: <Carl@X32.Tymnet> 
Subject: Re: Meeting 
In-reply-to: your message of Wed, 23 Sep 87 8:56:29 PDT

I guess 2pm Thu Oct 1 is okay.  I may have to cancel if things get too
hairy as I expect they might.  The whole next month is going to be
pretty bad.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 13:10:21 PDT 
To: <driscoll@calvin> 
Subject: Re: public address memo 
In-reply-to: <8709231650.AA10026@calvin.Tymnet.com> of Wed, 23 Sep 87 09:50:55
	PDT

That's okay - there is a correction, but its only wording so go ahear.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 13:11:17 PDT 
To: <driscoll@calvin> 
Subject: Re: RED 
In-reply-to: <8709231720.AA10425@calvin.Tymnet.com> of Wed, 23 Sep 87 10:20:25
	PDT

Yes, rkShare is gone.  I took it out this weekend.  Yes, on patron up,
the patron is now obligated to put that in the symbol table.  That needs
to be defined, but ... (panic feeling) ... oh well.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 13:14:35 PDT 
To: <driscoll@calvin> 
Subject: Re: Makefile 
In-reply-to: <8709231729.AA10574@calvin.Tymnet.com> of Wed, 23 Sep 87 10:29:12
	PDT

Ooops, will do.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 13:15:32 PDT 
To: <oliveb!gnome@tymix.Tymnet> 
Subject: Re: Showtime, etc 
In-reply-to: <8709231748.AA29252@oliven.ATC.OLIVETTI.COM> of Wed, 23 Sep 87
	10:48:45 PDT

Yea, I'm eating.  Almost as good as when she's home which is a surprise.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 13:11:17 PDT 
To: <driscoll@calvin> 
Subject: Re: RED 
In-reply-to: <8709231720.AA10425@calvin.Tymnet.com> of Wed, 23 Sep 87 10:20:25
	PDT

Yes, rkShare is gone.  I took it out this weekend.  Yes, on patron up,
the patron is now obligated to put that in the symbol table.  That needs
to be defined, but ... (panic feeling) ... oh well.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 13:19:18 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23069; Wed, 23 Sep 87 13:23:35 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA12201; Wed, 23 Sep 87
	13:23:33 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 23 Sep 87 13:23:33 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8709232023.AA12201@calvin.Tymnet.com> 
Subject: Re: RED 

Does rkConfess still exist?
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 13:17:32 PDT 
To: oliveb!sun!spedick@tymix 
Subject: Catalyst 

Hi, Mike.
I've been trying to get ahold of Alice Hausladen about the Catalyst
application as it says to do in the application if I have questions.
I'm hoping to talk Tymnet into marketing the X.PC stuff for me and
so would want to have their name appear in the Catalyst catalog, but
I'm still negotiating that with Tymnet.  My questions are
 1. does that still meet requirements?
 2. can I apply now, but hold publication until negotiation final?
If these things turn out to be a problem, I will just submit the
application using another product that isn't so complicated politically,
however, I really wanted to do X.PC first for many reasons.  Maybe
you can get answers to these, or light a fire under Alice so she'll
return my calls.  Thank you very much.  -Bill

PS - I was told to order a workstation for the office today.  I
wonder what finally softened Al up?  Rumor has it we'll get 30 this
year which is pretty good since they're not in the budget!
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 13:28:03 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23099; Wed, 23 Sep 87 13:32:20 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA26301;
	Wed, 23 Sep 87 13:32:16 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA12302; Wed, 23 Sep 87
	13:32:15 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 23 Sep 87 13:32:15 PDT 
To: wrs@calvin 
Message-id: <8709232032.AA12302@calvin.Tymnet.com> 
Subject: rkShare 

Does the patron ever return memory to Rodney?
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 17:28:01 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA24296; Wed, 23 Sep 87 17:32:20 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01583; Wed,
	23 Sep 87 17:32:17 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA15196; Wed, 23 Sep 87 17:31:39 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 23 Sep 87 17:31:39 PDT 
To: wrs@hobbes 
Message-id: <8709240031.AA15196@f104.sun.com> 
Subject: Re: struct portability warning 

Are we under any constraint to ensure that our allocated structures
(e.g. an IPCF header) start on a word boundary?  E.g. if an malloc
is done, theoretically it could start at any character position; any problem?
It also might start at any position as a result of kludges which copy
the structure to a character array.

Yours Inquisitively,
Ron
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 18:05:17 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA24311; Wed, 23 Sep 87 17:47:10 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01678; Wed,
	23 Sep 87 17:47:07 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA15229; Wed, 23 Sep 87 17:46:29 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 23 Sep 87 17:46:29 PDT 
To: wrs@f104 
Message-id: <8709240046.AA15229@f104.sun.com> 
Subject: ipcf.mm 

I think there is a typo in the ipcfDeassign section of ipcf.mm.  If the
specified tsap is not assigned to the caller, RERR_TSAPI is returned, not
RERR_TSAP.  
I also happened to notice that the description for key under the struct 
IpcfHdr declaration has a sentence fragment in it...
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 18:05:44 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA24267; Wed, 23 Sep 87 17:15:36 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01164;
	Wed, 23 Sep 87 17:15:31 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14328; Wed, 23 Sep 87
	17:15:30 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 23 Sep 87 17:15:30 PDT 
To: wrs@calvin 
Message-id: <8709240015.AA14328@calvin.Tymnet.com> 
Subject: BST_PATRON_START reminder 

Remember to change BST_PATRON_INIT to BST_PATRON_START.
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 18:35:21 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23496; Wed, 23 Sep 87 15:10:05 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27775; Wed,
	23 Sep 87 15:10:02 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23492; Wed, 23 Sep 87 15:09:59 PDT
Received: by oliveb.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA16307; Wed, 23
	Sep 87 15:01:40 PDT
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/03-10-87) id AA01039; Wed,
	23 Sep 87 15:01:31 PDT
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.OLIVETTI.COM (Gary) 
Date: Wed, 23 Sep 87 15:01:31 PDT 
To: oliveb!tymix!wrs 
Message-id: <8709232201.AA01039@oliven.ATC.OLIVETTI.COM> 
Subject: Talking about eating! 


We have GOT to get some barbecue from Sunset BBQ in (now) Palo Alto.
They used to be in MV but moved...   Yum.

I have Hypercard running (well, available) on my mac.  I don't know
if I like it or not...  Oh well.

What's up?
Gary



From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 23:17:04 PDT 
To: <driscoll@calvin> 
Subject: Re: rkShare 
In-reply-to: <8709232032.AA12302@calvin.Tymnet.com> of Wed, 23 Sep 87 13:32:15
	PDT

The patron owns everything its given at RSI_STARTUP.  It may, if it
can, give some of it back, when requested to do so via RSI_PALLOC.
Rodney never gives back what it gets from PALLOC.  When the patron
restarts, Rodney restarts and immediately returns ownership of the
memory back to the patron without having modified it (although it may
have run a non-destructive memory test which tests one location at a
time and restores it to its original value).  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 23:23:07 PDT 
To: <rmurphy@f104> 
Subject: Re: struct portability warning 
In-reply-to: <8709240031.AA15196@f104.sun.com> of Wed, 23 Sep 87 17:31:39 PDT

malloc() always allocates a worst-case-aligned block.  That is, if on a
32 bit machine that doesn't care about double word alignment, it aligns
to a 32 bit boundary.  So no need to worry.  Now if you copy it to a char
array, I think the 68020 will do the right thing, but it won't be fast.
I'm not sure what the 68010 does, but I'm pretty sure that it requires
any word or long reference be to an even location.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 23 Sep 87 23:26:28 PDT 
To: <rmurphy@f104> 
Subject: Re: ipcf.mm 
In-reply-to: <8709240046.AA15229@f104.sun.com> of Wed, 23 Sep 87 17:46:29 PDT

Thanks will fix...
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 21 Sep 87 16:25:00 PDT 
To: <coons@opus> 
Cc: coons@hobbes, dlasater@b39 
Subject: Re: Applied Microsystems source debugger 
In-reply-to: <8709212112.AA10257@opus.Tymnet.com> of Mon, 21 Sep 87 14:12:25
	PDT

Ah.  I understand about software project management.  Its not just hardware
people that have problems with software.  I've decided that software is
clearly an art (I agree with Knuth, et. al.).  You can't schedule creativity.
If you try, it may appear to work at first, but what you get won't be a
product of a creative effort, it'll just be another liability.

By the way, this isn't a comment about Skywalker project management, quite
the contrary, its a comment about our past style of project management which
has become appearent when contrasted with Skywalker.  I guess I can take
this chance to mention that I'm consistently amazed by how Skywalker is
being managed, and if we follow through with this attitude, we're going to
have one hell of a good product!

Thanks for the reply on the emulator.
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 21 Sep 87 16:46:56 PDT 
To: driscoll@calvin, ntd.m/mo@ontyme 
Subject: Tadpole 

I'm in contact with Tadpole (at Mike Mo's request) about the timing and
self-addressing issues and we should be getting some resolution in the
next 1-2 days.  (hopefully satisfactory)  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 22 Sep 87 4:59:01 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 13 Sep 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

12	8

45	16	1915	2302

30	2	1915	2302

31	5

21	1

------------------------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 22 Sep 87 4:59:01 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 20 Sep 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

45	26	1915	2302

30	2	1915	2302

31	4

20	1

------------------------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Tue, 22 Sep 87 16:35:15 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18380; Tue, 22 Sep 87 16:34:48 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA19003; Tue, 22 Sep 87
	16:34:27 PDT
Return-path: <wrs@hobbes> 
From: wrs@hobbes (Bill Soley) 
Date: Tue, 22 Sep 87 16:34:27 PDT 
To: wrs@hobbes 
Message-id: <8709222334.AA19003@hobbes.Tymnet.com> 
Subject: rob albertson 2:00 wed 9 23 


Received: from X32.Tymnet by B39.Tymnet; Tue, 22 Sep 87 18:04:09 PDT
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Tue, 22 Sep 87 18:05:49 PDT 
To: Wayne D Bartlett <BARTLETW@B39.Tymnet>, William R Soley <WRS@B39.Tymnet> 
Subject: PEAK removeal of TYM check 

I've found the proper sources for PEAK and generated a new version that
no longer checks for TYM-Proprietary.  It is installed on all systems.

/Carl
Received: from tymix.Tymnet by B39.Tymnet; Wed, 23 Sep 87 17:19:48 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA24282; Wed, 23 Sep 87 17:24:06 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01494;
	Wed, 23 Sep 87 17:24:00 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA14419; Wed, 23 Sep 87
	17:23:57 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Wed, 23 Sep 87 17:23:57 PDT 
To: wrs@calvin 
Message-id: <8709240023.AA14419@calvin.Tymnet.com> 
Subject: Makefile 

Please return the Makefile ASAP.
Pat
Received: from D35.Tymnet by B39.Tymnet; Thu, 24 Sep 87 10:45:39 PDT
Return-path: <JMS@D35.Tymnet> 
From: Joe Smith <JMS> 
Date: Thu, 24 Sep 87 12:30:29 CDT 
To: carl, wrs 
Subject: OPER getting duplicate mail on F33. 

I have hardcopy of the following, which was printed from the OPR terminal:

please log in: OPER:33;;
tymshare  1713  23-sep-1987
[mail waiting]
.O MOST
PEREZC
.
!  [mail from Carl A Baltrunas <Carl@D35.Tymnet>]
!  [mail from OPER@F33.Tymnet]
!  [mail from JMS@F33.Tymnet]
!  [mail from Carl A Baltrunas <Carl@D35.Tymnet>]
!  [mail from OPER@F33.Tymnet]
!  [mail from JMS@F33.Tymnet]
!  [mail from Carl A Baltrunas <Carl@D35.Tymnet>]
!  [mail from OPER@F33.Tymnet]
!  [mail from JMS@F33.Tymnet]
!
!DAYTIME
23-SEP-87 19:04:01 PDT
!RDMAIL -H
Carl A Baltrunas <Carl@D35.Tymnet>    Wed, 23 Sep 87 13:37:17 PDT
OPER@F33.Tymnet                       Wed, 23 Sep 87 13:31:22 PDT
JMS@F33.Tymnet                        Wed, 23 Sep 87 13:36:30 PDT


The file (MAIL:33)LOG shows some strangeness going on - it says 
"REQ: <OPER>" every hour!  I tried to clean up a bit by modifying the
SUBMIT jobs for VWALKER and RKARLSON to not send mail and renamed 32.Q to
35.Q after editting it.			/Joe
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 24 Sep 87 12:23:15 PDT 
To: <JMS@D35.Tymnet> 
Cc: carl 
Subject: Re: OPER getting duplicate mail on F33. 
In-reply-to: your message of Thu, 24 Sep 87 12:30:29 CDT

I checked out 33 and whatever it was is gone now.  You must have fixed it.
Its not clearwhat it was.  Some program was appearently running, opening
a file, and dieing without closing it (this is detected and the mail is
reprocessed by MAILER thus causing the REQ).  The idea being that when a
program dies without cleaning up, you can't tell if it has delivered the
mail it was working on, so to be safe, you deliver it again.  -Bill
Received: from D35.Tymnet by B39.Tymnet; Thu, 24 Sep 87 12:58:09 PDT
Return-path: <Carl@D35.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Thu, 24 Sep 87 13:00:58 PDT 
To: Joe Smith <JMS>, William R Soley <WRS@B39.Tymnet> 
Subject: Re: OPER getting duplicate mail on F33 

Bill,
  This problem seems to be occurring off and on... Both Rick Daniels and
Osman have seen it on separate occasions... then it goes away?  Is (MAIL)
up to date on system 33?

/Carl
Received: from tymix.Tymnet by B39.Tymnet; Thu, 24 Sep 87 16:59:04 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA27923; Thu, 24 Sep 87 17:03:27 PDT
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07088;
	Thu, 24 Sep 87 17:03:21 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA02129; Thu, 24 Sep 87 16:51:07
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Thu, 24 Sep 87 16:51:07 PDT 
To: wrs@comet 
Message-id: <8709242351.AA02129@comet.Tymnet.com> 
Subject: ontyme-tums addition 

Please have NTD.CONFERENCE forward to ntdconf@tymix.
Thanks,
	Steve
Received: from tymix.Tymnet by B39.Tymnet; Thu, 24 Sep 87 18:43:58 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA28084; Thu, 24 Sep 87 18:47:48 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07598;
	Thu, 24 Sep 87 18:47:46 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA22543; Thu, 24 Sep 87
	18:47:44 PDT
Return-path: <hsing@calvin> 
From: hsing@calvin (Yu-liang Hsing) 
Date: Thu, 24 Sep 87 18:47:44 PDT 
To: wrs@calvin 
Message-id: <8709250147.AA22543@calvin.Tymnet.com> 
Subject: interrupt handling 

Hi Bill,
Do we need to handle context switching at the beginning and end of our
interrupt service routine, and how to set up the interrupt vector?
Received: from F29.Tymnet by B39.Tymnet; Fri, 25 Sep 87 9:19:43 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 25 Sep 87 9:11:31 PDT
Return-path: <NTD.S/BLACK@EMSNTD.Ontyme.Tymnet> 
From: NTD.S/BLACK@EMSNTD.Ontyme.Tymnet 
Date: 25 SEP 87 09:01:26 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I90051@Ontyme.Tymnet 
Subject: time accounting code - mdc video tape 
ACTIVITYCODEFORMDCVIDEOTAPE: 

                         M E M O R A N D U M

                                     [] T Y M N E T / MCDONNELL DOUGLAS
                                             Network Technology Development


DATE>      24 SEP 87  13:29

TO>        TIME ACCOUNTING PERSONNEL

COPIES>    ROSIE HULL

FROM>      MITCHELL AI-CHANG


SUBJECT>   ACTIVITY CODE FOR MDC VIDEO TAPE


-----------------------------------------------------------------------


PLEASE LOG ALL TIME FOR THE VIEWING OF MDC VIDEO TAPE UNDER
ACTIVITY CODE 30.
Received: from tymix.Tymnet by B39.Tymnet; Fri, 25 Sep 87 10:06:39 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00799; Fri, 25 Sep 87 10:10:33 PDT
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA08686;
	Fri, 25 Sep 87 10:10:30 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA02600; Fri, 25 Sep 87 10:10:27
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Fri, 25 Sep 87 10:10:27 PDT 
To: wrs@comet 
Message-id: <8709251710.AA02600@comet.Tymnet.com> 
Subject: ontyme forward 

i forgot to give you the password before.
please forward "ntd.conference" to "ntdconf@tymix", password is "ntd.announce"
	steve
Received: from tymix.Tymnet by B39.Tymnet; Fri, 25 Sep 87 11:46:07 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA00977; Fri, 25 Sep 87 11:50:03 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09158; Fri,
	25 Sep 87 11:49:59 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA16270; Fri, 25 Sep 87 11:49:55 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Fri, 25 Sep 87 11:49:55 PDT 
To: wrs@f104 
Message-id: <8709251849.AA16270@f104.sun.com> 
Subject: symbol names 

Did we decide for sure that symbol names are to be lower case?  Steve
still is using upper case names in his sfs documentation and we need to
let him know if so.
 

Received: from tymix.Tymnet by B39.Tymnet; Fri, 25 Sep 87 12:10:21 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01058; Fri, 25 Sep 87 12:13:56 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09229;
	Fri, 25 Sep 87 12:13:53 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA26539; Fri, 25 Sep 87
	12:13:51 PDT
Return-path: <driscoll@calvin> 
From: driscoll@calvin (Pat Driscoll) 
Date: Fri, 25 Sep 87 12:13:51 PDT 
To: wrs@calvin 
Message-id: <8709251913.AA26539@calvin.Tymnet.com> 
Subject: RED 

When I make red, I get a message "need description of glob fields".
What does this mean?
Pat
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 25 Sep 87 12:48:54 PDT 
To: <feldman@comet> 
Subject: Re: ontyme-tums addition 
In-reply-to: <8709242351.AA02129@comet.Tymnet.com> of Thu, 24 Sep 87 16:51:07
	PDT

Should I change the password for NTD.CONFERENCE to something appropriate
or else I need to know what it is now.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 25 Sep 87 12:50:16 PDT 
To: <hsing@calvin> 
Subject: Re: interrupt handling 
In-reply-to: <8709250147.AA22543@calvin.Tymnet.com> of Thu, 24 Sep 87 18:47:44
	PDT

I'm not sure I understand the question.  Which interrupt are we talking
about?  On the real hardware, you just setup the vector with the address
of the function that is the interrupt service routine.  There is a special
option on the MicroTec compiler to generate special code for interrupt
routines (basically it just saves all the registers).  The context switch,
if any is handled by the hardware assuming the new status word is setup
right in the vector.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 25 Sep 87 12:52:45 PDT 
To: <feldman@comet> 
Subject: Re: ontyme forward 
In-reply-to: <8709251710.AA02600@comet.Tymnet.com> of Fri, 25 Sep 87 10:10:27
	PDT

Okay NTD.CONFERENCE is setup...Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 25 Sep 87 12:58:53 PDT 
To: <rmurphy@f104> 
Cc: feldman@comet, driscoll@calvin 
Subject: Re: symbol names 
In-reply-to: <8709251849.AA16270@f104.sun.com> of Fri, 25 Sep 87 11:49:55 PDT

I vote for lower case.  Any comments?
-Bill

----------------------------------------------------------------
From: rmurphy@f104 (Ron Murphy) 
Date: Fri, 25 Sep 87 11:49:55 PDT 
Subject: symbol names 

Did we decide for sure that symbol names are to be lower case?  Steve
still is using upper case names in his sfs documentation and we need to
let him know if so.
----------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 25 Sep 87 13:00:57 PDT 
To: <driscoll@calvin> 
Subject: Re: RED 
In-reply-to: <8709251913.AA26539@calvin.Tymnet.com> of Fri, 25 Sep 87 12:13:51
	PDT

Its just a reminder to me to add descriptions of the glob fields in some
more organized and complete manner.  If they'd only stand still long
enough.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Fri, 25 Sep 87 13:07:03 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01155; Fri, 25 Sep 87 13:10:39 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA02799; Fri, 25 Sep 87 13:10:29
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Fri, 25 Sep 87 13:10:29 PDT 
To: wrs@b39.Tymnet.com 
Cc: driscoll@calvin, rmurphy@f104 
Message-id: <8709252010.AA02799@comet.Tymnet.com> 
Subject: Re: symbol names 

i vote for lower case too.  but then what do i know?  after all, i like unix.
	steve
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 25 Sep 87 13:31:02 PDT 
To: <feldman@comet> 
Cc: driscoll@calvin, rmurphy@f104 
Subject: Re: symbol names 
In-reply-to: <8709252010.AA02799@comet.Tymnet.com> of Fri, 25 Sep 87 13:10:29
	PDT

Lower case, then.  -Bil
Received: from F74.Tymnet by B39.Tymnet; Fri, 25 Sep 87 17:05:11 PDT
Return-path: <MKTGINFO@F74.Tymnet> 
From: MKTGINFO@F74.Tymnet 
Date: Fri, 25 Sep 87 17:02:43 PDT 
To: WRS 
Subject: OLD ACCESS# PROGRAM ON SYSTEM 74 

BILL,

THE OLD ACCESS#:74 PROGRAM IS HAVING SOME SECURITY PROBLEMS.  IT IS POSSIBLE,
TO DROP OUT OF THE PROGRAM IF YOU HIT SEVERAL ESC'S AFTER THE PROGRAM LOADS.
THE FORTRAN HAS THE ONINT STATEMENT TO COUNTERACT THIS, AND ONE ESC WILL
CAUSE AN EXIT, BUT MANY ESC'S SEND THE USER TO THE XEXEC PROMPT.  WHAT CAN
I DO ABOUT THIS?  ANY INFO WOULD BE HELPFUL.  PLEASE REPLY TO DDD:38, AS I
AM RARELY ON 74.
THANKS,

LYNN LAWRENCE
Received: from tymix.Tymnet by B39.Tymnet; Fri, 25 Sep 87 17:18:06 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01739; Fri, 25 Sep 87 17:21:39 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA12161;
	Fri, 25 Sep 87 17:21:36 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA29623; Fri, 25 Sep 87
	17:21:34 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 25 Sep 87 17:21:34 PDT 
To: wrs@calvin 
Message-id: <8709260021.AA29623@calvin.Tymnet.com> 
Subject: "Bill, I want to make sure I understand"... 

Bill,
I want to make sure I understand what is being put on the stack
for an estimation of sprout-stack.
Only the automatic variables and perhaps return addresses are put
on the stack. This includes automatic structures, unions etc.
Is it so?
I am not sure about static v., but externals shouldn't go on stack.

Mike
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 27 Sep 87 15:42:47 PDT 
To: <feldman@comet> 
Subject: Re: battery backed up ram on tadpole 
In-reply-to: <8709252239.AA03159@comet.Tymnet.com> of Fri, 25 Sep 87 15:39:46
	PDT

The bbram is tempting.  I'm not sure what could be saved there given that
it is subject to erant programs scribbling on it without notice.  It is
clearly something that should be kept in mind.  It would be great if we
could use it for the serial number, but that seems too risky unless we used
some extreemly clever ecc to significantly decrease the chance of an
unrecoverable error.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 27 Sep 87 15:46:33 PDT 
To: <MKTGINFO@F74.Tymnet> 
Cc: ddd@38 
Subject: Re: OLD ACCESS# PROGRAM ON SYSTEM 74 
In-reply-to: your message of Fri, 25 Sep 87 17:02:43 PDT

I can correct the security problem, but I think the time would be better
spent installing the new (SAIL) program.  What do you think?  _bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sun, 27 Sep 87 15:48:03 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, I want to make sure I understand"... 
In-reply-to: <8709260021.AA29623@calvin.Tymnet.com> of Fri, 25 Sep 87 17:21:34
	PDT

On the stack go any auto declared inside a function.  Its stacked when
the function is invoked.  Also any formal parameter (argument) to the
function, also when its invoked.  Statics and externals do not.  malloc()
memory does not.  Watch out for alignment, for example, the following
function...
	fn()
	{
		long x;
		char c;
		long y;
		char d;
		long z;
	}
could take 20 bytes of stack depending on how clever the compiler is since
it must always force long word alignment.  If you always declare in
descending order of size you are most likely to get optimal packing.  Also
be aware that when evaluation complex expressions, the compiler may push
temporary results on the stack if it runs out of registers to keep them in.
There is no way to predict this.  *SIGH*  If you suspect it, you just have
to look at the code.  -Bill
Received: from F38.Tymnet by B39.Tymnet; Mon, 28 Sep 87 10:07:44 PDT
Return-path: <DDD@F38.Tymnet> 
From: DDD@F38.Tymnet 
Date: Mon, 28 Sep 87 9:54:06 PDT 
To: WRS 
Subject: OLD ACCESS# 

BILL,

YOU ARE CERTAINLY MORE THAN CORRECT ABOUT INSTALLING THE NEW SAIL PROGRAM.
THE PROBLEM I AM HAVING IS GETTING A VALID DATA FILE FOR THE PROGRAM TO 
RUN OFF (GIGO).  THIS IS PROVING MORE AND MORE DIFFICULT WITH INCREASED
SYSTEM LOAD PLUS THE ENORMOUS AMOUNT OF DATA TO BE PROCESSED.  I HAVE TRIED
WRITING PROCEDURES WHICH WILL AUTOMATICALLY THROW OUT AREA CO AND PREFIX
COMBINATIONS WHICH ARE OBVIOUSLY NOT PERTINENT.  EVEN WHEN I DO GET A RUN TO
COMPLETE, I HAVE NO WAY OF IDENTIFYING INACCURATE DATA UNTIL IT SHOWS UP
DURING A PROGRAM RUN.  SINCE THIS IS THE FIRST TIME WE ARE OFFERING ANYTHING
BASED ON CCMI DIRECTLY TO OUR CUSTOMERS, IT NEEDS TO BE CLEANER.

SO, MY CURRENT PLAN IS TO CREATE SOME FLAT FILES AND PROCESS THEM IN TURBO C.
THIS WAY, IF THERE ARE OBVIOUS ERRORS, THE TIME TO REPROCESS WILL BE SHORTER.

THE FUTURE PLAN IS TO REWRITE DDD IN FOCUS ON THE IBM 370...

SO, I GUESS MY IDEA OF FIXING THE SECURITY FOR THE OLD ACCESS PROGRAM
IS A "QUICK FIX" FOR NOW.  SOMEONE WENT IN AND CHANGED THE PASSWORD
LAST FRIDAY TO THE ACCESS# USERNAME.  I LOGGED ON AFTER THE PASSWORD
WAS RESET AND FOUND OUT THAT IT WAS POSSIBLE TO ESC OUT OF THE PROGRAM.

ANY IDEAS/COMMENTS ARE WELCOME.

THANKS AGAIN,

LYNN LAWRENCE
Received: from tymix.Tymnet by B39.Tymnet; Mon, 28 Sep 87 10:36:34 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07472; Sun, 27 Sep 87 21:06:17 PDT
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA14437; Sun,
	27 Sep 87 21:06:14 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07468; Sun, 27 Sep 87 21:06:11 PDT
Return-path: <uucp@tymix> 
From: uucp@tymix (UNIX-to-UNIX Copy) 
Date: Sun, 27 Sep 87 21:06:11 PDT 
To: wrs@tymix 
Message-id: <8709280406.AA07468@tymix.Tymnet> 
Subject: file unet!/c/sdd/wrs/shwin -- remote access to path/file denied 


Received: from tymix.Tymnet by B39.Tymnet; Mon, 28 Sep 87 10:37:16 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA07253; Sun, 27 Sep 87 17:15:45 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA14284; Sun,
	27 Sep 87 17:15:42 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA17932; Sun, 27 Sep 87 17:15:42 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Sun, 27 Sep 87 17:15:42 PDT 
To: wrs@f104 
Message-id: <8709280015.AA17932@f104.sun.com> 
Subject: tsap_service_routine associated with ipcfAssign 

ipcf.mm says that the tsapKey passed ipcf in an assign will be handed
back to the tsap_service_routine when this routine is called.  If so,
I would advise making it available by reference (long *tsapKeyp)...
Ron


From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 28 Sep 87 11:50:47 PDT 
To: <rmurphy@f104> 
Subject: Re: tsap_service_routine associated with ipcfAssign 
In-reply-to: <8709280015.AA17932@f104.sun.com> of Sun, 27 Sep 87 17:15:42 PDT

Why?
Received: from tymix.Tymnet by B39.Tymnet; Mon, 28 Sep 87 12:05:07 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA09875; Mon, 28 Sep 87 12:08:42 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA18449; Mon, 28 Sep 87 12:08:40 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Mon, 28 Sep 87 12:08:40 PDT 
To: WRS@B39.Tymnet 
Message-id: <8709281908.AA18449@f104.sun.com> 
Subject: Re: tsap_service_routine associated with ipcfAssign 

Why indeed.  I take it back (the key would be constant).
Received: from tymix.Tymnet by B39.Tymnet; Mon, 28 Sep 87 13:51:56 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA10321; Mon, 28 Sep 87 13:55:33 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA18109;
	Mon, 28 Sep 87 13:55:31 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA08095; Mon, 28 Sep 87
	13:55:29 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Mon, 28 Sep 87 13:55:29 PDT 
To: wrs@calvin 
Message-id: <8709282055.AA08095@calvin.Tymnet.com> 
Subject: "Bill, how about BST_PATRON_INIT"... 

Bill,
how about BST_PATRON_INIT or BST_PATRON_START. please tell me
which name you prefer. I guess Pat wants _START but _INIT is still
in the include and I get a compile error.
Mike
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Mon, 28 Sep 87 14:19:35 PDT 
To: CARL@D35, WRS@C39.Tymnet 
Subject: "this confirms meeting this thurs"... 

this confirms meeting this thurs 10/1 @2:00 - Bill's office (thats to
be sure we have Bill there)
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 28 Sep 87 15:27:47 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, how about BST_PATRON_INIT"... 
In-reply-to: <8709282055.AA08095@calvin.Tymnet.com> of Mon, 28 Sep 87 13:55:29
	PDT

Sorry, I changed it then installed the old file.  Its fixed now.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 28 Sep 87 15:28:39 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: CARL@D35 
Subject: Re: "this confirms meeting this thurs"... 
In-reply-to: your message of Mon, 28 Sep 87 14:19:35 PDT

Confirmed but it will be very brief!  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 28 Sep 87 15:29:58 PDT 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: CARL@D35, qsats.d/ellenberg@ontyme 
Subject: Re: "NAD In case you have not heard, "... 
In-reply-to: your message of Mon, 28 Sep 87 14:24:22 PDT

Doug, I've moved my version of NAD to (WRS:33)SYSNAD.* but it should
be installed with the name NAD as follows.

The file (WRS)SYSNAD.SAV goes in (SYS)NAD.SAV with RF license and ALL RUN RUN.
The file (WRS)SYSNAD.LIS goes in (SOURCE)NAD.LIS with no special protection.

NAD.LIS is a demo.  It has comments in it (which you may want to remove
to prevent someone from being tempted to edit it).  Let me know if there
are any questions.  -Bill
Received: from F29.Tymnet by B39.Tymnet; Mon, 28 Sep 87 15:54:32 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 28 Sep 87 15:56:42 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 28 SEP 87 15:50:44 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I90975@Ontyme.Tymnet 
Subject: KEN HOLCOMB-OUT OF OFFICE 
OUTOFOFFICE: 

                         M E M O R A N D U M

                                     [] T Y M N E T / MCDONNELL DOUGLAS
                                             Network Technology Development


DATE>      25 SEP 87  10:30

TO>        ALL USERS

COPIES>    

FROM>      KEN HOLCOMB


SUBJECT>   OUT OF OFFICE


-----------------------------------------------------------------------


BJ CHANG AND I WILL BE IN HONG KONG ALL OF NEXT WEEK.  RON SALTGAVER
WILL BE ACTING IN OUR BEHALF.  PLEASE CONTACT HIM IF ANYTHING ARISES
THAT NEEDS IMMEDIATE ATTENTION.
Received: from F29.Tymnet by B39.Tymnet; Mon, 28 Sep 87 15:54:48 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Mon, 28 Sep 87 15:57:04 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 28 SEP 87 15:52:34 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I90977@Ontyme.Tymnet 
Subject: CHG OF ONTYME ACCTS FROM TNS.SUP 
CHANGEOFONTYMEACCOUNT: 

=======================  M E M O R A N D U M  =========================

                                                       MCDONNELL DOUGLAS
DATE>      28 SEP 87  09:33                           INFORMATION SYSTEMS

TO>        ALL

COPIES>    

FROM>      CORINNE ZERBIB


SUBJECT>   CHANGE OF ONTYME ACCOUNT


-----------------------------------------------------------------------



     
                        ***  A T T E N T I O N   ***    
     
     
DUE TO ORGANIZATIONAL CHANGES, PLEASE NOTE THE CHANGE OF ONTYME  
ACCOUNT FOR THE FOLLOWING PEOPLE.  
     
THE TRANSFER WILL BE EFFECTIVE OCTOBER, 1ST 1987. 
AS OF THIS DATE, STOP TO SEND MEMO'S TO TNS.XXXX  
     
PLEASE UPDATE YOUR DISTRIBUTION LISTS ACCORDINGLY.


THANKING YOU FOR YOUR COOPERATION. 
     
BEST REGARDS,  
CORINNE   
     
     
     
     
     
     

SWEDEN    
------    
     
OLD USERNAME                    NEW USERNAME   
- - - - - - -                   - - - - - - - 
     
TNS.D/WAGNER                    MDISWE.D/WAGNER
TNS.C/MALM                      MDISWE.D/WAGNER  
TNS.B/NASLUND                   MDISWE.B/NASLUND   
TNS.M/ROSEN                     MDISWE.M/ROSEN  
TNS.A/WENNSTROM                 MDISWE.A/WENNSTROM    
TNS.BM/NILSSON                  MDISWE.BM/NILSSON 
TNS.A/OSMARK                    MDISWE.A/OSMARK
TNS.A/FREND                     MDISWE.A/FREND  
TNS.S/MOHLIN                    MDISWE.S/MOHLIN
TNS.L/MOLINDER                  MDISWE.L/MOLINDER 
TNS.J/LINDBACK                  MDISWE.J/LINDBACK 
TNS.J/BERGMAN                   MDISWE.J/BERGMAN   
TNS.K/NILSSON                   MDISWE.K/NILSSON   
TNS.C/YOUNGER                   MDISWE.C/YOUNGER   
TNS.T/POTURAJ                   MDISWE.T/POTURAJ   
TNS.J/WIDEGRAN                  MDISWE.J/WIDGRAN  
TNS.Y/BERGLUND                  MDISWE.Y/BERGLUND 
TNS.M/SICKING                   MDISWE.M/SICKING   
     
TNS.M/HAAG                      MDICUST.M/HAAG   
TNS.L/SODERMAN                  MDICUST.L/SODERMAN
TNS.L/EKSTROM                   MDICUST.L/EKSTROM  
TNS.A/HAGGMAN                   MDICUST.A/HAGGMAN  
TNS.JOVE                        MDICUST.JOVE  
TNS.L/ERIKSSON                  MDICUST.L/ERIKSSON
TNS.HAGENMALM                   MDICUST.HAGENMALM  
TNS.LAETMAA                     MDICUST.LAETMAA 
TNS.LARSSON                     MDICUST.LARSSON 
TNS.O/SJOSTROM                  MDICUST.O/SJOSTROM
TNS.ERIKSSON                    MDICUST.ERIKSSON    
TNS.ERMAN                       MDICUST.ERMAN
TNS.MONA                        MDICUST.MONA  
TNS.SWOPS                       MDICUST.SWOPS
TNS.TELI                        MDICUST.TELI  
TNS.MALMOE                      MDICUST.MALMOE   
TNS.JOHANSSON                   MDICUST.JOHANSSON  
TNS.LUNDIN                      MDICUST.LUNDIN   
TNS.GOTEBORG                    MDICUST.GOTEBORG    
TNS.AXELSSON                    MDICUST.AXELSSON    
TNS.R/STROMBERG                 MDICUST.R/STROMBERG   
TNS.STROMBER                    MDICUST.STROMBERG   
TNS.LEVIN                       MDICUST.LEVIN
TNS.WARGNER                     MDICUST.WARGNER 
TNS.ISAKSSON                    MDICUST.ISAKSSON    
TNS.BORG                        MDICUST.BORG  
TNS.MARTENSSON                  MDICUST.MARTENSSON
TNS.SWHQ                        MDICUST.SWHQ  
TNS.HALLDEN                     MDICUST.HALLDEN 
TNS.P/STRINBERG                 MDICUST.P/STRINBERG   
TNS.OLOFSSON                    MDICUST.OLOFSSON    
TNS.G/BRADING                   MDICUST.G/BRADING  
     
TNS.A/ANDREASEN                 MDICUST.A/ANDREASEN   
TNS.SKOLAN                      MDICUST.SKOLAN   
TNS.SCANDUTCH                   MDICUST.SCANDUTCH  
     


     
     
     
FRANCE    
------    
     
OLD USERNAME			 NEW USERNAME
- - - - - - -			 - - - - - - -

TNS.ORDERS                      MDISF.ORDERS
TNS.JC/JACOTIN                  MDISF.JC/JACOTIN  
TNS.A/MARTINEZ                  MDISF.A/MARTINEZ  
TNS.F/HANNEBI                   MDISF.F/HANNEBI    
TNS.JUTTEAU                     MDISF.JUTTEAU   
TNS.JP/RUIZ                     MDISF.JP/RUIZ   
TNS.CAZAU                       MDISF.CAZAU  
TNS.C/ZERBIB                    MDISF.C/ZERBIB 
TNS.CONTESSE                    MDISF.CONTESSE 
TNS.NCS                         MDISF.NCS
TNS.VERONIQUE                   MDISF.V/VERAN 
TNS.C/RAUCOURT                  MDISF.C/RAUCOURT  
TNS.MC/LEMEUR                   MDISF.MC/LEMEUR    
TNS.A/BRIDARD                   MDISF.A/BRIDARD    
TNS.D/LAHAIX                    MDISF.D/LAHAIX 
     
     
TNS.E/ESPOUY                    TSG.E/ESPOUY   
TNS.TRAIN                       TSG.TRAIN    
TNS.G/SANTHA                    TSG.G/SANTHA   
TNS.H/VU                        TSG.H/VU 
TNS.CS                          TSG.CS    
TNS.M/BEELDENS                  TSG.M/BEELDENS    
TNS.P/JOHNSON                   TSG.P/JOHNSON 
TNS.GWENAELLE                   TSG.GWENAELLE 
TNS.D/LANCRENON                 TSG.D/LANCRENON  
TNS.P/GONZALEZ                  TSG.P/GONZALEZ    
TNS.PROJ                        TSG.PROJ 
TNS.S/FISBEIN                   TSG.S/FISBEIN 
TNS.OPS                         TSG.OPS  
TNS.JP/COTILLON                 TSG.JP/COTILLON  
TNS.JOANNA                      TSG.JOANNA  
TNS.M/HEY                       TSG.M/HEY    
TNS.P/BOUCACHARD                TSG.P/BOUCACHARD 
TNS.R/DUMAS                     TSG.R/DUMAS
TNS.RETHO                       TSG.RETHO    
TNS.M/GRANOTIER                 TSG.M/GRANOTIER  
TNS.LABP                        TSG.LABP 
TNS.NIRRFI                      TSG.NIRRFI  
TNS.P/FENEZ                     TSG.P/FENEZ
TNS.D/MARCHAND                  TSG.D/MARCHAND    
TNS.DULAM                       TSG.DULAM    
     

     
     
SWITZERLAND    
-----------    

OLD USERNAME			 NEW USERNAME
- - - - - - -			 - - - - - - -

     
TNS.R/ZURBRIGGEN                ISID.R/ZURBRIGGEN
TNS.E/BITZER                    ISID.E/BITZER  
TNS.MACNULTY                    ISID.MACNULTY  
     

TNS.SBC                         MDICUST.SBC   
TNS.SBCTRIP                     MDICUST.SBCTRIP 
     
     
ITALY
-----
     
TNS.ITCMARKT                    MDICUST.ITCMARKT    
TNS.SMAU                        MDICUST.SMAU  
TNS.SIRTI                       MDICUST.SIRTI
TNS.ITCMATS                     MDICUST.ITCMATS 
TNS.AUTOSTRADA                  MDICSUT.AUTOSTRADA
TNS.ITCMAINT                    MDICSUT.ITCMAINT    
TNS.ITCPLAN                     MDICUST.ITCPLAN 
TNS.AVALENTINI                  MDICUST.AVALENTINI
     
     
LEBANON   
-------   
     
TNS.DM/CHARLES                  MDICUST.DM/CHARLES
TNS.DM/PARIS                    MDICUST.DM/PARIS    
TNS.DM/JEAN                     MDICUST.DM/JEAN 
TNS.DM/TEST                     MDICUST.DM/TEST 
TNS.DM/BEYROUTH                 MDICUST.DM/BEYROUTH   
TNS.DM/SIOUFI                   MDICUST.DM/SIOUFI  
TNS.T/BOUNADER                  MDICUST.T/BOUNADER
TNS.C/MACRIDIS                  MDICUST.C/MACRIDIS
Received: from tymix.Tymnet by B39.Tymnet; Tue, 29 Sep 87 4:44:55 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12852; Tue, 29 Sep 87 04:48:31 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA23242; Tue, 29 Sep 87
	04:48:15 PDT
Return-path: <wrs@hobbes> 
From: wrs@hobbes (Bill Soley) 
Date: Tue, 29 Sep 87 04:48:15 PDT 
To: rodney@hobbes 
Message-id: <8709291148.AA23242@hobbes.Tymnet.com> 
Subject: fastc 

I've changed the name of glob.fastc to glob.usec since it never was
anything to do with fastc.  Sorry about the inconvenience.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Tue, 29 Sep 87 13:46:03 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13825; Tue, 29 Sep 87 13:49:35 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA26462; Tue,
	29 Sep 87 13:49:32 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA19472; Tue, 29 Sep 87 13:49:35 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 29 Sep 87 13:49:35 PDT 
To: wrs@f104 
Message-id: <8709292049.AA19472@f104.sun.com> 
Subject: ipcf send 

Does ipcf send still have a second parameter for the done timeout as we
discussed?  I am not using this (since dones are meaningless to the
logger) but I still have the parameter coded in the logger since I didn't
hear anything about a change since our discussion in July about a done
timeout.

Ron


Received: from tymix.Tymnet by B39.Tymnet; Tue, 29 Sep 87 14:16:02 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13908; Tue, 29 Sep 87 14:19:37 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27009; Tue, 29 Sep 87
	14:19:33 PDT
Return-path: <bechlyt@hobbes> 
From: bechlyt@hobbes (Tom Bechly) 
Date: Tue, 29 Sep 87 14:19:33 PDT 
To: wrs@hobbes 
Message-id: <8709292119.AA27009@hobbes.Tymnet.com> 
Subject: h files 

i couldn't find an h file that contained the enum Sym_Type.  if it is not
available could it be made available.

thanks,

tom
Received: from tymix.Tymnet by B39.Tymnet; Tue, 29 Sep 87 14:18:54 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13915; Tue, 29 Sep 87 14:22:26 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA27055; Tue, 29 Sep 87
	14:22:22 PDT
Return-path: <bechlyt@hobbes> 
From: bechlyt@hobbes (Tom Bechly) 
Date: Tue, 29 Sep 87 14:22:22 PDT 
To: wrs@hobbes 
Message-id: <8709292122.AA27055@hobbes.Tymnet.com> 
Subject: h files 

the file /usr/sky/rodney/v1/include/glob.h does not seem to have a field
containing the vme Id.  is it going to have one.

thanks,

tom
Received: from tymix.Tymnet by B39.Tymnet; Tue, 29 Sep 87 17:44:12 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA14788; Tue, 29 Sep 87 17:47:46 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01511; Tue,
	29 Sep 87 17:47:43 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00417; Tue, 29 Sep 87 17:47:45 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Tue, 29 Sep 87 17:47:45 PDT 
To: wrs@f104 
Message-id: <8709300047.AA00417@f104.sun.com> 
Subject: varargs in microtec 

I'm interested in writing microtec c code that uses the "varargs" c facility.
Apparently this facility is not provided by microtec.  I was wondering if
microtec manages its stack the same way that the sun's compiler does, so
that we could use the sun's vararg definitions for microtec.  Do you
happen to know the answer?


Received: from F29.Tymnet by B39.Tymnet; Tue, 29 Sep 87 22:39:55 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Tue, 29 Sep 87 22:34:30 PDT
Return-path: <NSMS.B/CORENSON@EMSNTD.Ontyme.Tymnet> 
From: NSMS.B/CORENSON@EMSNTD.Ontyme.Tymnet 
Date: 29 SEP 87 22:27:50 
To: NTD.WRS@EMSNTD.Ontyme.Tymnet 
Message-id: I91593@Ontyme.Tymnet 
Subject: Problem issuing commands in Birdix 

 
 
 
 
 
            TO: Bill Soley
            
            FR: Brant Corenson
            
            DT: 9/29 - 22:24
            
            Bill,
            
            I can log in successfully to Birdix.  However, after issuing
            a command, I lose my link to Bubb and am forced to re-enter
            the the gateway.  What is causing me this grief?
            
            Brant
Received: from F29.Tymnet by B39.Tymnet; Wed, 30 Sep 87 8:40:24 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 30 Sep 87 8:43:16 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 30 SEP 87 08:39:52 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I91759@Ontyme.Tymnet 
Subject: "Management Professional Development"... 

                     Management & Professional Development

                              M E M O R A N D U M


  TO:       All Employees (Please post)

  FROM:     Stephen Reynolds, Manager
            Management & Professional Development (M&PD) San Jose

  SUBJECTS: .4Q87 M&PD-San Jose Training Schedule
            .New M&PD-San Jose Address and Phone Numbers

  DATE:     September 24, 1987

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  BAY AREA EMPLOYEES   
  -4Q87 M&PD-San Jose schedules, enrollment forms, and course descriptions
  were sent to all A-F managers (Sr. Section Manager and above) on 
  September 11.  See your manager for a copy.
      -Registrations are due back to M&PD-San Jose by September 28.

  EMPLOYEES OUTSIDE THE BAY AREA
  -4Q87 M&PD San Jose schedule, enrollment forms and course desctiptions
  will be sent to all District Managers and above during the week of
  September 28 for courses in San Jose, Vienna/Rockville,
  Rosemont/Mt. Prospect, and San Diego.  See your manager for a copy.
      -Registrations are due back to M&PD-San Jose by October 12.

  FOR INFORMATION:
  - Tymnet Training & Education, call Cheryl McGowan at 408/922-7617
      (408/432-5042 until Oct. 5).

  - M&PD-St. Louis, call Linda Zink at 314/232-9304.

  - M&PD-Irvine, call Bonnie Watkins at 714/250-1000 ext.7486.

  - Denver (MDCIS) programs, call Kathy Padus at 303/220-6044.

*************  NEW M&PD-SAN JOSE ADDRESS  **************************

  Mail:  McDonnell Douglas Training Center
         2665 North First Street
         San Jose, CA  95134

  Interoffice:  CA59 Bldg T

  Ontyme:  HRD.TRNG

  Phone:  Name                      After 10/6/87              Until 10/6/87
          Stephen Reynolds..........408/922-7654............... 408/435-7742
          Kathleen Corcoran.........408/922-7651................408/432-5068
          LaVonne Hage..............408/922-7652................408/432-5292
          Belinda Quesada...........408/922-7653................408/435-7740
Received: from F29.Tymnet by B39.Tymnet; Wed, 30 Sep 87 9:40:31 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 30 Sep 87 9:38:05 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 30 SEP 87 08:43:44 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I91763@Ontyme.Tymnet 
Subject: Change form TNS Account to TSG Account 

PLEASE DISTRIBUTE THE FOLLOWING MESSAGE TO "THE WHOLE WORLD"

Many thanks,


=======================  M E M O R A N D U M  =========================

                                                       MCDONNELL DOUGLAS
DATE>      24 SEP 87  15:21                           INFORMATION SYSTEMS

TO>        Anyone sending ontymes to a TNS account

COPIES>    

FROM>      Du Lam
            Tymnet Support Group


SUBJECT>   Change form TNS Account to TSG Account


-----------------------------------------------------------------------



                              CHANGE OF ACCOUNT
                              =================

Please be advised that as of 1st October 1987, the ontyme account for
TSG (Tymnet Support Group) will be changed from "TNS" to "TSG".  Below
is the list of our usernames involved in the change :

Name                        Old                      New
----                        ---                      ---

Eliane Espouy               TNS.E/ESPOUY             TSG.E/ESPOUY
Gnanamoorthy Santhakumar    TNS.G/SANTHA             TSG.G/SANTHA
Hoang Dung Vu               TNS.H/VU                 TSG.H/VU
Martine Beeldens            TNS.M/BEELDENS           TSG.M/BEELDENS
Paul Johnson                TNS.P/JOHNSON            TSG.P/JOHNSON
Gwenaelle Le Martret        TNS.GWENAELLE            TSG.GWENAELLE
Dominique Lancrenon         TNS.D/LANCRENON          TSG.D/LANCRENON
Pierre Gonzalez             TNS.P/GONZALEZ           TSG.P/GONZALEZ
Stephane Fisbein            TNS.S/FISBEIN            TSG.S/FISBEIN
Jean-Paul Cotillon          TNS.JP/COTILLON          TSG.JP/COTILLON
Joanna Delahaye             TNS.JOANNA               TSG.JOANNA
Martin Hey                  TNS.M/HEY                TSG.M/HEY
Philippe Boucachard         TNS.P/BOUCACHARD         TSG.P/BOUCACHARD
Richard Dumas               TNS.R/DUMAS              TSG.R/DUMAS
Jean-Francois Retho         TNS.RETHO                TSG.RETHO
Michel Granotier            TNS.M/GRANOTIER          TSG.M/GRANOTIER
Pierre Fenez                TNS.P/FENEZ              TSG.P/FENEZ
Dominique Marchand          TNS.D/MARCHAND           TSG.D/MARCHAND
Du Lam                      TNS.DULAM                TSG.DULAM

                            TNS.TRAIN                TSG.TRAIN
                            TNS.CS                   TSG.CS
                            TNS.PROJ                 TSG.PROJ
                            TNS.OPS                  TSG.OPS
                            TNS.LABP                 TSG.LABP
                            TNS.NIRRFI               TSG.NIRRFI

Please update your distribution lists and send your mails as of
1st October 1987 to the new ontyme username.

Thank you,

Du Lam
TYMNET SUPPORT GROUP
Received: from F29.Tymnet by B39.Tymnet; Wed, 30 Sep 87 9:40:45 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 30 Sep 87 9:38:37 PDT
Return-path: <NTD.SUP@EMSNTD.Ontyme.Tymnet> 
From: NTD.SUP@EMSNTD.Ontyme.Tymnet 
Date: 30 SEP 87 08:51:40 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I91769@Ontyme.Tymnet 
Subject: CHANGE OF ONTYME ACCOUNT 

=======================  M E M O R A N D U M  =========================

                                                       MCDONNELL DOUGLAS
DATE>      28 SEP 87  09:33                           INFORMATION SYSTEMS

TO>        ALL

COPIES>    

FROM>      CORINNE ZERBIB


SUBJECT>   CHANGE OF ONTYME ACCOUNT


-----------------------------------------------------------------------



     
                        ***  A T T E N T I O N   ***    
     
     
DUE TO ORGANIZATIONAL CHANGES, PLEASE NOTE THE CHANGE OF ONTYME  
ACCOUNT FOR THE FOLLOWING PEOPLE.  
     
THE TRANSFER WILL BE EFFECTIVE OCTOBER, 1ST 1987. 
AS OF THIS DATE, STOP TO SEND MEMO'S TO TNS.XXXX  
     
PLEASE UPDATE YOUR DISTRIBUTION LISTS ACCORDINGLY.


THANKING YOU FOR YOUR COOPERATION. 
     
BEST REGARDS,  
CORINNE   
     
     
     
     
     
     

SWEDEN    
------    
     
OLD USERNAME                    NEW USERNAME   
- - - - - - -                   - - - - - - - 
     
TNS.D/WAGNER                    MDISWE.D/WAGNER
TNS.C/MALM                      MDISWE.D/WAGNER  
TNS.B/NASLUND                   MDISWE.B/NASLUND   
TNS.M/ROSEN                     MDISWE.M/ROSEN  
TNS.A/WENNSTROM                 MDISWE.A/WENNSTROM    
TNS.BM/NILSSON                  MDISWE.BM/NILSSON 
TNS.A/OSMARK                    MDISWE.A/OSMARK
TNS.A/FREND                     MDISWE.A/FREND  
TNS.S/MOHLIN                    MDISWE.S/MOHLIN
TNS.L/MOLINDER                  MDISWE.L/MOLINDER 
TNS.J/LINDBACK                  MDISWE.J/LINDBACK 
TNS.J/BERGMAN                   MDISWE.J/BERGMAN   
TNS.K/NILSSON                   MDISWE.K/NILSSON   
TNS.C/YOUNGER                   MDISWE.C/YOUNGER   
TNS.T/POTURAJ                   MDISWE.T/POTURAJ   
TNS.J/WIDEGRAN                  MDISWE.J/WIDGRAN  
TNS.Y/BERGLUND                  MDISWE.Y/BERGLUND 
TNS.M/SICKING                   MDISWE.M/SICKING   
     
TNS.M/HAAG                      MDICUST.M/HAAG   
TNS.L/SODERMAN                  MDICUST.L/SODERMAN
TNS.L/EKSTROM                   MDICUST.L/EKSTROM  
TNS.A/HAGGMAN                   MDICUST.A/HAGGMAN  
TNS.JOVE                        MDICUST.JOVE  
TNS.L/ERIKSSON                  MDICUST.L/ERIKSSON
TNS.HAGENMALM                   MDICUST.HAGENMALM  
TNS.LAETMAA                     MDICUST.LAETMAA 
TNS.LARSSON                     MDICUST.LARSSON 
TNS.O/SJOSTROM                  MDICUST.O/SJOSTROM
TNS.ERIKSSON                    MDICUST.ERIKSSON    
TNS.ERMAN                       MDICUST.ERMAN
TNS.MONA                        MDICUST.MONA  
TNS.SWOPS                       MDICUST.SWOPS
TNS.TELI                        MDICUST.TELI  
TNS.MALMOE                      MDICUST.MALMOE   
TNS.JOHANSSON                   MDICUST.JOHANSSON  
TNS.LUNDIN                      MDICUST.LUNDIN   
TNS.GOTEBORG                    MDICUST.GOTEBORG    
TNS.AXELSSON                    MDICUST.AXELSSON    
TNS.R/STROMBERG                 MDICUST.R/STROMBERG   
TNS.STROMBER                    MDICUST.STROMBERG   
TNS.LEVIN                       MDICUST.LEVIN
TNS.WARGNER                     MDICUST.WARGNER 
TNS.ISAKSSON                    MDICUST.ISAKSSON    
TNS.BORG                        MDICUST.BORG  
TNS.MARTENSSON                  MDICUST.MARTENSSON
TNS.SWHQ                        MDICUST.SWHQ  
TNS.HALLDEN                     MDICUST.HALLDEN 
TNS.P/STRINBERG                 MDICUST.P/STRINBERG   
TNS.OLOFSSON                    MDICUST.OLOFSSON    
TNS.G/BRADING                   MDICUST.G/BRADING  
     
TNS.A/ANDREASEN                 MDICUST.A/ANDREASEN   
TNS.SKOLAN                      MDICUST.SKOLAN   
TNS.SCANDUTCH                   MDICUST.SCANDUTCH  
     


     
     
     
FRANCE    
------    
     
OLD USERNAME			 NEW USERNAME
- - - - - - -			 - - - - - - -

TNS.ORDERS                      MDISF.ORDERS
TNS.JC/JACOTIN                  MDISF.JC/JACOTIN  
TNS.A/MARTINEZ                  MDISF.A/MARTINEZ  
TNS.F/HANNEBI                   MDISF.F/HANNEBI    
TNS.JUTTEAU                     MDISF.JUTTEAU   
TNS.JP/RUIZ                     MDISF.JP/RUIZ   
TNS.CAZAU                       MDISF.CAZAU  
TNS.C/ZERBIB                    MDISF.C/ZERBIB 
TNS.CONTESSE                    MDISF.CONTESSE 
TNS.NCS                         MDISF.NCS
TNS.VERONIQUE                   MDISF.V/VERAN 
TNS.C/RAUCOURT                  MDISF.C/RAUCOURT  
TNS.MC/LEMEUR                   MDISF.MC/LEMEUR    
TNS.A/BRIDARD                   MDISF.A/BRIDARD    
TNS.D/LAHAIX                    MDISF.D/LAHAIX 
     
     
TNS.E/ESPOUY                    TSG.E/ESPOUY   
TNS.TRAIN                       TSG.TRAIN    
TNS.G/SANTHA                    TSG.G/SANTHA   
TNS.H/VU                        TSG.H/VU 
TNS.CS                          TSG.CS    
TNS.M/BEELDENS                  TSG.M/BEELDENS    
TNS.P/JOHNSON                   TSG.P/JOHNSON 
TNS.GWENAELLE                   TSG.GWENAELLE 
TNS.D/LANCRENON                 TSG.D/LANCRENON  
TNS.P/GONZALEZ                  TSG.P/GONZALEZ    
TNS.PROJ                        TSG.PROJ 
TNS.S/FISBEIN                   TSG.S/FISBEIN 
TNS.OPS                         TSG.OPS  
TNS.JP/COTILLON                 TSG.JP/COTILLON  
TNS.JOANNA                      TSG.JOANNA  
TNS.M/HEY                       TSG.M/HEY    
TNS.P/BOUCACHARD                TSG.P/BOUCACHARD 
TNS.R/DUMAS                     TSG.R/DUMAS
TNS.RETHO                       TSG.RETHO    
TNS.M/GRANOTIER                 TSG.M/GRANOTIER  
TNS.LABP                        TSG.LABP 
TNS.NIRRFI                      TSG.NIRRFI  
TNS.P/FENEZ                     TSG.P/FENEZ
TNS.D/MARCHAND                  TSG.D/MARCHAND    
TNS.DULAM                       TSG.DULAM    
     

     
     
SWITZERLAND    
-----------    

OLD USERNAME			 NEW USERNAME
- - - - - - -			 - - - - - - -

     
TNS.R/ZURBRIGGEN                ISID.R/ZURBRIGGEN
TNS.E/BITZER                    ISID.E/BITZER  
TNS.MACNULTY                    ISID.MACNULTY  
     

TNS.SBC                         MDICUST.SBC   
TNS.SBCTRIP                     MDICUST.SBCTRIP 
     
     
ITALY
-----
     
TNS.ITCMARKT                    MDICUST.ITCMARKT    
TNS.SMAU                        MDICUST.SMAU  
TNS.SIRTI                       MDICUST.SIRTI
TNS.ITCMATS                     MDICUST.ITCMATS 
TNS.AUTOSTRADA                  MDICSUT.AUTOSTRADA
TNS.ITCMAINT                    MDICSUT.ITCMAINT    
TNS.ITCPLAN                     MDICUST.ITCPLAN 
TNS.AVALENTINI                  MDICUST.AVALENTINI
     
     
LEBANON   
-------   
     
TNS.DM/CHARLES                  MDICUST.DM/CHARLES
TNS.DM/PARIS                    MDICUST.DM/PARIS    
TNS.DM/JEAN                     MDICUST.DM/JEAN 
TNS.DM/TEST                     MDICUST.DM/TEST 
TNS.DM/BEYROUTH                 MDICUST.DM/BEYROUTH   
TNS.DM/SIOUFI                   MDICUST.DM/SIOUFI  
TNS.T/BOUNADER                  MDICUST.T/BOUNADER
TNS.C/MACRIDIS                  MDICUST.C/MACRIDIS
Received: from tymix.Tymnet by B39.Tymnet; Wed, 30 Sep 87 10:14:22 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18292; Wed, 30 Sep 87 10:17:55 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA05538;
	Wed, 30 Sep 87 10:17:52 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA14594; Wed, 30 Sep 87 10:17:51
	PDT
Return-path: <dfields@opus> 
From: dfields@opus (David Fields) 
Date: Wed, 30 Sep 87 10:17:51 PDT 
To: wrs@opus 
Message-id: <8709301717.AA14594@opus.Tymnet.com> 
Subject: Ontyme - UNIX hookup 

Bill:
	The Ontyme-UNIX connection you established for me worked fine
for a few days, but now seems to be broken.  The messages reached my
Ontyme username all right, but were not forwarded to opus.  Can you
fix it, or is it someone else's problem?
	In case you need the information again:

	ONTYME ID:	NTD.D/FIELDS
	ONTYME KEY:	FINE MESS
	DEST. HOST:	opus
	USERNAME:	dfields


					Thanks,

					...Dave Fields
Received: from tymix.Tymnet by B39.Tymnet; Wed, 30 Sep 87 11:31:30 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18383; Wed, 30 Sep 87 11:35:04 PDT
Received: from f104.sun.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA07107; Wed,
	30 Sep 87 11:35:00 PDT
Received: by f104.sun.com (3.2/SMI-3.2) id AA00903; Wed, 30 Sep 87 11:35:03 PDT
Return-path: <rmurphy@f104> 
From: rmurphy@f104 (Ron Murphy) 
Date: Wed, 30 Sep 87 11:35:03 PDT 
To: wrs@f104 
Message-id: <8709301835.AA00903@f104.sun.com> 
Subject: wildcard symbols 

ipcf.mm says that no wild card characters may be present in a symbol name
but there is no error code mentioned in symEnter for this error.  Will the
restriction be enforced?

 

From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 30 Sep 87 12:19:04 PDT 
To: <mjastrz@calvin> 
Subject: Re: "Bill, Pat asked me to review the"... 
In-reply-to: <8709292025.AA15275@calvin.Tymnet.com> of Tue, 29 Sep 87 13:25:01
	PDT

rkBootStrap should not return to the patron unless the only
action specified is dump.  If there is going to be a reload
or even a restart, it should never return.  As far as the other
stuff, I'll make a copy of the current BOOT and check it.  I'm
not at all sure what you are saying.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 30 Sep 87 12:24:28 PDT 
To: <rmurphy@f104> 
Subject: Re: ipcf send 
In-reply-to: <8709292049.AA19472@f104.sun.com> of Tue, 29 Sep 87 13:49:35 PDT

No, ipcfSend should only have 1 parameter, now.
Check with Yu-Liang for sure.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 30 Sep 87 12:29:54 PDT 
To: <bechlyt@hobbes> 
Subject: Re: h files 
In-reply-to: <8709292119.AA27009@hobbes.Tymnet.com> of Tue, 29 Sep 87 14:19:33
	PDT

Its in include/sym.h as of 1 minute ago.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 30 Sep 87 12:35:01 PDT 
To: <bechlyt@hobbes> 
Subject: Re: h files 
In-reply-to: <8709292122.AA27055@hobbes.Tymnet.com> of Tue, 29 Sep 87 14:22:22
	PDT

Its there now... thanks for reminder.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 30 Sep 87 12:37:50 PDT 
To: <rmurphy@f104> 
Subject: Re: varargs in microtec 
In-reply-to: <8709300047.AA00417@f104.sun.com> of Tue, 29 Sep 87 17:47:45 PDT

I think its the same.  There might be something in chapter 5 of the MicroTec
C Reference Manual.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 30 Sep 87 12:42:47 PDT 
To: <NSMS.B/CORENSON@EMSNTD.Ontyme.Tymnet> 
Subject: Re: Problem issuing commands in Birdix 
In-reply-to: I91593@Ontyme.Tymnet of 29 SEP 87 22:27:50

Hmmm.  It works ok for me.  Should go like

please log in: bubb
please log in: birdix
Login: enlon
% passwd
new password: whatever
% cat resume.wrs
blah blah
% exit
please log in:
(hang up)

Give me a call this evening if it still doesn't work.  -Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Wed, 30 Sep 87 13:02:20 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: CDS 

BILL,

MORE CDS IN YOUR MAILBOX!

THANKS...OMAR

From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 30 Sep 87 13:08:08 PDT 
To: <dfields@opus> 
Subject: Re: Ontyme - UNIX hookup 
In-reply-to: <8709301717.AA14594@opus.Tymnet.com> of Wed, 30 Sep 87 10:17:51
	PDT

Somebody from another division (!&@^%#^&!$) attempted to add semebody
to the polling file and in so doing deleted 5 other people.  You were
one.  You're back so everything should be fine now.  Sorry.  -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 30 Sep 87 13:09:49 PDT 
To: <rmurphy@f104> 
Subject: Re: wildcard symbols 
In-reply-to: <8709301835.AA00903@f104.sun.com> of Wed, 30 Sep 87 11:35:03 PDT

Not enforced.  -Bill
Received: from tymix.Tymnet by B39.Tymnet; Wed, 30 Sep 87 13:53:20 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA18900; Wed, 30 Sep 87 13:56:57 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09958;
	Wed, 30 Sep 87 13:56:54 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA22375; Wed, 30 Sep 87
	13:56:53 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Wed, 30 Sep 87 13:56:53 PDT 
To: wrs@calvin 
Message-id: <8709302056.AA22375@calvin.Tymnet.com> 
Subject: "Bill, give me a call I will catch"... 

Bill,
give me a call (or I will catch you later) so we could talk about
boot. today I am in ntd at 6700.
mike
Received: from tymix.Tymnet by B39.Tymnet; Fri, 25 Sep 87 12:39:27 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA01078; Fri, 25 Sep 87 12:43:05 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA09317;
	Fri, 25 Sep 87 12:43:03 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA26642; Fri, 25 Sep 87
	12:43:01 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Fri, 25 Sep 87 12:43:01 PDT 
To: wrs@calvin 
Message-id: <8709251943.AA26642@calvin.Tymnet.com> 
Subject: "Bill, please change BST_PATRON_INIT"... 

Bill, please change BST_PATRON_INIT to ..._START or let me know if you
insist on .._INIT.
Mike
Received: from tymix.Tymnet by B39.Tymnet; Tue, 29 Sep 87 13:21:42 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA13769; Tue, 29 Sep 87 13:25:06 PDT
Received: from calvin.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA26066;
	Tue, 29 Sep 87 13:25:03 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA15275; Tue, 29 Sep 87
	13:25:01 PDT
Return-path: <mjastrz@calvin> 
From: mjastrz@calvin (Mike Jastrzebski) 
Date: Tue, 29 Sep 87 13:25:01 PDT 
To: wrs@calvin 
Message-id: <8709292025.AA15275@calvin.Tymnet.com> 
Subject: "Bill, Pat asked me to review the"... 

Bill, Pat asked me to review the boot part of Rodney doc and I came
across a few points that are still a bit nebulous to me.
rkBootstrap() will return to the patron caller (the doc was modified
in this area), so I think Patron will have to dismiss or sit idle
in anticipation of being halted and/or reloaded.
There is also a statement that got deleted on my request since it has
no bearing in the current code: "if action 1 is requested (use of res.
memory) and there is no start address action 2 (waiting for file)
will take over" - well, currently I don't do any checking on the start
address but even when I do it I still have problems with this statement.
First of all if Patron was ever loaded (it must have been!) the old
start address must be still around and if for some reason it is unwise
to use it then I don't see any point of having action 1 around in the
first place. Pat insisted on deleting all statem. which are incompatible
with current state of affairs, but since this particular area is self-
contradictory (in my understanding) I don't see a chance it will ever
be re-instated unless you shed some more light on it.
Mike
Received: from tymix.Tymnet by B39.Tymnet; Thu, 1 Oct 87 11:10:51 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19499; Wed, 30 Sep 87 19:16:25 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA18151; Wed, 30 Sep 87 19:16:22
	PDT
Return-path: <dfields@opus> 
From: dfields@opus (David Fields) 
Date: Wed, 30 Sep 87 19:16:22 PDT 
To: WRS@B39.Tymnet.com 
Message-id: <8710010216.AA18151@opus.Tymnet.com> 
Subject: Re: Ontyme - UNIX hookup 

Thanks
	...Dave
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 28 Sep 87 0:59:14 PDT 
To: Starr Black <NTD.S/BLACK@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Subject: time accounting worksheet 
Week-Ending-Date: Sun, 27 Sep 87

			   TIME ACCOUNTING WORKSHEET
		     Tymnet Network Technology Development


							Cost code: 4119

Employee# 211711	Employee: William R. Soley	Mgr: William J. Euske

------------------------------------------------------------------------------
ActCode	Hours	PIR	PROJ	TaskID	Comments
------------------------------------------------------------------------------

30	1

46	14	1915	2302

30	8	1915	2302

21	1

------------------------------------------------------------------------------
Received: from tymix.Tymnet by B39.Tymnet; Thu, 1 Oct 87 11:11:32 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA19461; Wed, 30 Sep 87 18:45:43 PDT
Received: by hobbes.Tymnet.com (3.2/SMI-3.2) id AA16297; Wed, 30 Sep 87
	18:45:41 PDT
Return-path: <wrs@hobbes> 
From: wrs@hobbes (Bill Soley) 
Date: Wed, 30 Sep 87 18:45:41 PDT 
To: ntd.s/black@ontyme, wrs@hobbes 
Message-id: <8710010145.AA16297@hobbes.Tymnet.com> 
Subject: inventory 

#			SUPPLIMENTAL INVENTORY FORM
# 
# contact:	Bill Soley
# cost center:	4119
# last update:	29-Sep-87
# phone:	6210
+
-loc-	-description-	-manufacturer-	-model-		-serial-	-tag-
=
home	Computer	IBM		PC-XT		5101092		222689
home	Counter		Hewlett-Packard	5315A		2032A04117	222737
home	Engine		Tymnet		650005-001	0290		222353
home	Modem		Paradyne	MP-9600		11880		no tag
home	Modem		Penril		21A91A1 (912)	2084		no tag
home	Oscilloscope	Hewlett-Packard	1722B		0234A01131	222738
home	Pulse Gen	Datapulse	101		38539		222736
home	Terminal	Hewlett-Packard	2621P (444)	1920A01592	222734
home	Terminal	Lear Siegler 	ADM-3A (430)	52331		222735
work	Terminal	Hewlett-Packard	2621P (444)	1912A01258	222688
work	VME 68000 CPU	Force Computers	SYS68K/CPU-2F	200204001786	no tag
work	VME 68000 CPU	Force Computers	SYS68K/CPU-2F	200204001772	no tag
work	VME 68000 CPU	Force Computers	SYS68K/CPU-2F	200204001779	no tag
work	VME 68000 CPU	Force Computers	SYS68K/CPU-2F	200204001782	no tag
work	VME 68000 CPU	Force Computers	SYS68K/CPU-2F	210204001758	no tag
work	VME arbiter	Force Computers	SYS68K/ASCU-2	320B202A0440	no tag
work	VME cage	Force Computers	SYS68K/CHAS-12	2G00032		no tag
work	VME pwr sply	Force Computers	SYS68K/PWR-09A	VP00755		no tag
   zS1è