Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Tue, 26 Nov 85 18:07:50 UT
From: ATE.K/JONES@Ontyme.Tymnet 
Date: 26 NOV 85 09:48:25 
To: NTD.WRS@Ontyme.Tymnet 
Message-id: J81231@Ontyme.Tymnet 
Subject: PROFS Documentation 

Subject:  PROFS Documentation

     To:  Soley, Bill

 Attach:

I have forwarded copies of sections 2 and 5 from the document on
using PROFS in line mode.  I have also ordered another copy of the
document.  I have kept original on chance that I may be trying some
of the other things, but if you decide you need it I can get it to
you.

I am sending the copies through inter-dept mail.

If you look closer at the project and have any other opinions let
me know.

Talked to salesman on the Chrysler thing.  He said that people that
they deal with there were unable to get to their management with their
presentation and recommendation last Thursday.  He couldn't tell me when
they were going to but he expected it would be this week.  The Chrysler
people told him that they were recommending another vendor.

I wouldn`t advise investing any time hacking on this project now.
Next Monday and Tuesday, I am attending a meeting in St. Louis called
by ISG IRM people regarding PROFS/OnTyme connection.  They perceive
an internal need and want to stimulate its development.  I also talk
to Stuart Cain frequently and he seems to have an interest in TUMS and
has told me that he will be meeting with you.

[PC ID 20:se301a022:59524]
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 26 Nov 85 14:54:31 PST 
To: ATE.K/JONES@Ontyme.Tymnet 
Subject: Re: PROFS Documentation 
In-reply-to: J81231@Ontyme.Tymnet of 26 NOV 85 09:48:25

Okay.  So far I've found that the MDC VM system is configured to reject
circuits with a terminal-ID of Aux-Circuit so I can't connect to it from
a PDP-10, although I've had no trouble from a terminal.  I am in the
process of finding the right person to reconfigure their X.25 base to
allow these types of circuits.  I have a considerable interest in getting
the interface with TUMS working, anyway, just for TUMS sake, but will
consider it a slightly lower priority unless I hear further.  As soon as
the X.25 base is reconfigured, I expect I can have things operating in
"alpha test" in about a week.

I will be on vacation Nov 28 through Dec 8 (first week of December) so I
won't be able to get to it before then, anyway.  Hopefully, the node can
be reconfigured durring that time.  I'm planning to meet with Stuart on
Dec 11, but that's still tenative.

-Bill
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 26 Nov 85 17:20:04 PST 
To: ATE.K/JONES@Ontyme.Tymnet 
Subject: Re: PROFS Logon 
In-reply-to: J80733@Ontyme.Tymnet of 25 NOV 85 11:07:52

Ken, I've tracked down the problem with Aux-Circuits as far as possible
without talking to someone at McAuto about their end of the interface.
It appears that we are sending them a "Call Request" packet with the
"Protocol ID" field indicating a "non-PAD-Call".  They accept the call
and then immediately clear it with error code 96.  The exact message is
	host: call cleared (c 0, d 96): dte originated
The error code 96 is reserved for use by CCITT, but unassigned.  Could
you possibly get me the name of someone I can talk to about this problem
at the MDC end?  Thanks -Bill
From: William R. Soley <WRS@C39.Tymnet> 
Date: Mon, 9 Dec 85 13:08:38 PST 
To: ATE.K/JONES@Ontyme.Tymnet 
Subject: Re: PROFS Documentation 
In-reply-to: J81231@Ontyme.Tymnet of 26 NOV 85 09:48:25

Well, I'm back from vacation (Kings Canyon/Sequoia National Park) and
feel good - ready to go!  Found the manual pages for PROFS burried in
the 9.5 inches of mail waiting for me.  I can't find anything in what
you sent me on sending mail, only reading and replying.  I have pages
5-3 through 5-14 which describes only the OPENMAIL command and sub-
commands.  Please check to see if there is something on sending notes
that was ommited by accident.

I talked to Cliff Achord before leaving and they have fixed the problem
with the X.25 interface so it will now accept aux-circuits.  I have
not been in touch with them since I returned, but I did try it and
since it works, I assume they fixed it while I was gone.  

What system will Chrysler be running PROFS on?  If its an X.25 interface
then things will be easily ported from MDC to Chrysler, otherwise, it
may be tricky.  

The word has come down from Warren that this is a high priority for me,
so I can devote whatever it takes over the next couple weeks to get it
up and running.  Regardless of Chrysler, I feel the benefit from a MDC
interface is enough to justify the time.

Thanks -Bill
Received: From EMSNTD.Ontyme.Tymnet by C39.Tymnet; Tue, 10 Dec 85 19:09:15 UT
From: ATE.K/JONES@Ontyme.Tymnet 
Date: 10 DEC 85 10:24:46 
To: NTD.WRS@Ontyme.Tymnet 
Message-id: J85952@Ontyme.Tymnet 
Subject: PROFS Documentation 

Subject:  PROFS Documentation

     To:  Soley, Bill

 Attach:

Bill, I dropped copy of line mode usage manual by your office
yesterday afternoon.

Status Update:

Chrysler salesman says that they are still dragging out the decision.
They told him that they are considering having two vendors perform
the pilot.  (Hopefully we will be one)  Clint DeGabrielle met with
decision maker and said that we would provide a PROFS/OT interconnection
and that we could provide it in February.  I asked salesman what the
expectation was and he thought the expectation was that if Chrysler made
their decision by Dec 31, then we could have interconnection for pilot
by February 1.

I have looked at about a half dozen alternatives to providing this
interconnection and it is my opinion that TUMS is the only approach
with any hope of functionality by February.

I will try to see if someone can't get us better information on
Chrysler's requirements.

Cheers, KJJ!

[PC ID 20:se301a022:25584]
   