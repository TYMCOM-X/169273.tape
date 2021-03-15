Received: from C930.Tymnet by 39.Tymnet; Fri, 1 Feb 85 10:12:04 PST
Return-path: WILKES@C930.Tymnet 
From: WILKES@C930.Tymnet 
Date: Fri, 1 Feb 85 10:11:57 PST 
To: dernst@39 
Subject: micronode eats form feeds and spits out line feeds instead 

just a reminder.
i am implementing a line printer driver using a tty line on the 
unode, and found that the unode converts my FF into 6 LFs...  remember??
have gotten side tracked because I have found that some of the functions
of the TYMOP jsys appear to be broken (i wanted to try setting the line
to "transparent" mode, for instance...  get date and time from Tymnet,
for instance, etc.)
At any rate, the problem still exists (FF turning into 6 LFs), and I
wanted to touch base with you.  I check into system 930 every few days
to read mail, so you can reply to me theree, if you wish.  or i get
mail on office-4 (augment ident is JDW.TYM for me).
/John Wilkes
p.s. FF -> 6LF shows up only when i build circuit via :TRx@xxx
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sun, 10 Feb 85 1:00:14 PST 
To: dernst 
Subject: X.25/75 

Whats the best document for somebody who doesn't know anything about X.25
(like me) to learn about how the inter-net gateways work?  Also would like
some info on addressing, including a list of assigned DNIC's.  Can you
give me any pointers?  Thanks -Bill
Received: from C930.Tymnet by C39.Tymnet; Sat, 30 Mar 85 22:14:00 PST
Return-path: <CARL@930.Tymnet> 
From: Carl A Baltrunas <CARL@930.Tymnet> 
Date: Sat, 30 Mar 85 22:08:39 PST 
To: dernst@39 
Subject: Micronode/Base Info 

Dennis,
  On the micronode, it seems that when it crashes, it will re-synch with
the KMC-11/KS-10 code currently running... However, it does not do that
for the F3.  I want to know what you know about the interaction that goes
on between the base and host.
  On initial synch, the host does this things with sending 0's and waiting
for an acknoledgement of some kind [F3].... (I suspect the KMC is similar)
  When the micronode crashes and starts up again, i suspect that it no
longer has the correct IRING/ORING information... however, it should be
able to read the proper locations (150-165 for KI/KL/KS, 2000-2015? for the
poor F3) to find out what IRP620/IRPPDP,ORP620/ORPPDP should be... now,
it seems there is a "the base crash & has reloaded msg" that the KS/KMC
does something with... is this correct?
My question is, do you send a similar message to the F3?  Can you?
If I got such a message, then ran through the resynch code (all that sending
of zeros, etc.) (possibly even resetting IRP620 & ORP620 to 0) would that
"should that" work to resynch on the F3?  Or is there some F3 microcode
problem that you know of?

/Carl
Received: from C930.Tymnet by C39.Tymnet; Wed, 3 Apr 85 0:51:42 PST
Return-path: <TXSSUP@C930.Tymnet> 
From: TXSSUP@C930.Tymnet 
Date: Wed, 3 Apr 85 0:37:02 PST 
To: dernst@c39.tymnet 
Subject: Base action, resynch 

Dennis,
  I more or less understand that the F3 microcode is what the PDP-11 is
talking to, (or now, as you say, it simulates a KMC-11).  I am still very
much confused about how to make the F3 resynch with the micronode base.

  Let me explain that I know how the F3 originally syncs with the base by
resetting everything then shoving 300 zeroes at it, then putting it into
"auto" mode as you say.  That's a single routine in the monitor...
Where I'm confused is, how does it know that it's supposed to re-synch in
the first place?  Does the base send a message that it has crashed?  (On
the 2020, it seems to do something like that.)  If it can send that message
then why can't it just use the ring pointers as-is and assumeing that the
F3 sends circuit zappers for all connected ports, continue where it left off?

  If I can determine somehow that the base has gone away (directly, or by
keeping track of when it last serviced the rings) I can then zap all the
ports and reset everything and restart. -- waiting for it to synch --

  Better yet, if I can detect that it has come back to life, I can do other
things while I wait for it to come back...  send me mail, or give me a call.
I'll be at home (408) 945-4314 or [maybe] at the office late afternoon ...
116-2515.

/Carl
From: DERNST@C39.Tymnet 
Date: Thu, 18 Apr 85 11:29:00 PST 
To: carl@930, dernst 
Subject: F3 bases 

CARL,
I DIDN'T GET YOUR INFO ABOUT TUMS, SENDMAIL, ETC.  IT ALSO SEEMS
LIKE YOU DIDN'T GET MY LAST MESSAGE EITHER.

AS STATED BEFORE, A MICRONODE ON AN F3 DOES NOT READ THE RINGS,
THIS I DONE BY THE F3 MICROCODE.  THE INTERFACE TO THE F3
FROM THE MICRO-NODE IS JUST A 16 BIT REGISTER WITH 2 CONTROL BITS.
THE CODE YOU WANT TO LOOK AT IS THE TYMSRV.MAC CODE IN TENEX WHICH
HAS BEEN DOING RECOVERY FROM NODE AND HOST CRASHES FOR SEVERAL YEARS.
I DO HAVE A PAPER COPY OF THAT IF YOU CAN'T GET ONE ONLINE.

DENNIS
Received: from C930.Tymnet by C39.Tymnet; Thu, 18 Apr 85 23:00:25 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 18 Apr 85 22:30:54 PST 
To: DERNST@C39.Tymnet 
Cc: dernst 
Subject: Re: F3 bases 
In-reply-to: your message of Thu, 18 Apr 85 11:29:00 PST

Dennis,
  Sorry about the confusion... missing messages, etc.   I realize that
I seem to mis-comprehending something about the F3 bases, and when I
do understand it, it will knock me over, but...
  I know HOW we are currently synchronizing with the base when the host
comes up... it assumes the base is UP & RUNNING and it does the thing
with the 300 zeroes.  I'm assuming that that is how we would re-synch
by resetting the interface & sending the 300 zeroes and waiting for them
to be accepted (returned?).  What I'm having difficulty understanding
[WITHOUT SEEING THE TYMSRV SOURCES] is how the TENEX F3s know that the
base went away & they need to re-synch.  That's where I'm confused.
I'd like to see your paper copy of TYMSRV.MAC, thank you.  I can only
expect that they  use some kind of timeout & I'd like to know exactly
what they do to determine the base has gone, (or come back up).
/Carl

PS: Part of why I'm interested in this is not only to get F3's to make
    a graceful recovery from base crashes/glitches, but to be able to
    come up and handle the resynch at once-a-tick time instead of
    holding everything up until the base resynchs.
From: DERNST@C39.Tymnet 
Date: Fri, 19 Apr 85 9:26:19 PST 
To: carl@930 
Subject: F3 bases 

Carl,
    The TENEX code times out if it doesn't receive a message
in 60 seconds. The micronode code sends a type 1 message every 10
seconds if there is nothing to send.  If you did the same you
could detect the crash of the node.  The 300 zeros are to make
sure we haven't hung on a partial message.  The zeros are ignored
if they are not part of a message.  After the 300 zeros, then you
should go to automatic mode (resetting the ring pointers if the
micro-code doesn't) and just wait.  The reason we may have
gone down in the first place may have been a bad message from
the F3 which makes us do a one minute timeout, so make your code be
patient.  You do have to send the "specify host ports" 41 message
as the first message.  This is actually what gets to stop throwing
stuff away.  After that everything should be back up.

Dennis
Received: from C930.Tymnet by C39.Tymnet; Fri, 19 Apr 85 11:10:41 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 19 Apr 85 11:04:09 PST 
To: DERNST@C39.Tymnet 
Subject: Re: F3 bases 
In-reply-to: your message of Fri, 19 Apr 85 9:26:19 PST

Dennis,
  Thanks!!!!!  That is exactly what I wanted to know.  Do you know of
any reason why sending each of those 300 zeroes can't be done at the
60th of a second clock tick.  I tried it, and it didn't seem to work.
/Carl
From: DERNST@C39.Tymnet 
Date: Fri, 19 Apr 85 12:06:01 PST 
To: CARL@930 
Subject: f3 

No, timing isn't that important.
Dennis
Received: from X930.Tymnet by C39.Tymnet; Tue, 19 Nov 85 23:01:06 PST
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> (Liberty Street in Fremont 116-2512) 
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
Received: from X930.Tymnet by B39.Tymnet; Sat, 26 Apr 86 19:11:32 PST
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
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
Received: from X930.Tymnet by B39.Tymnet; Wed, 4 Jun 86 13:38:19 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
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
Received: from X930.Tymnet by B39.Tymnet; Thu, 19 Jun 86 15:01:05 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 19 Jun 86 14:42:57 PDT 
To: Dennis Ernst <DErnst@C39.Tymnet> 
Subject: Re: (MAIL:62)930.SAV has problems. 

(begin forwarded message)

Received: from B39.Tymnet by X930.Tymnet; Thu, 19 Jun 86 14:06:03 PDT
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

(end forwarded message)

I have fixed LOGINN to ignore all control characters in username except
CR.  This means it now allows ^A,^B,^C,^D,^I,^V in the login string.
These characters really should be filtered out by the PDP11.
Page 12 of CIRCUIT.DOC mentions ^A,^B,^C,^D,^I,^H,^K,^P,^R,^U,^V,^W,^X,^[.
Do the PDP11s handle all of those characters?	  /Joe
Received: from X930.Tymnet by B39.Tymnet; Thu, 19 Jun 86 15:33:49 PDT
Return-path: <JMS@X930.Tymnet> 
From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 19 Jun 86 15:30:21 PDT 
To: Dennis Ernst <DErnst@C39.Tymnet> 
Subject: Addressable ports. 

How do addressable ports work on PDP11 bases?  Does the code we are currently
running allow us to build a circuit from the host to a particular line on the
base?  If so, does the line need to be in the "please log in" state or the
"please see your rep..." disconnected state.

/Joe
Received: from X930.Tymnet by B39.Tymnet; Sat, 21 Jun 86 23:15:58 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
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
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Jul 86 1:48:55 PDT 
To: dernst 
Subject: meeting 

Did you get my message that I'd like to meet today at 9:30 as opposed to
the originally planned 10:30?  (rhetorical)  I'll come by your office.
I hope it fits in to your schedule, I have another meeting at 11:00.
Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 16 Sep 86 3:29:56 PDT 
To: dernst 
Cc: dernst@36, dernst@57 
Subject: swb 

I won't be in before I go to the airport.  If you need to talk about the
trip, call Dave Chang or call me at home (408)866-0443.  -Bill
Received: from X62.Tymnet by B39.Tymnet; Sun, 9 Nov 86 0:15:26 PST
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
Received: from X930.Tymnet by B39.Tymnet; Mon, 10 Nov 86 22:42:32 PST
Return-path: <CARL@X930.Tymnet> 
From: CARL@X930.Tymnet 
Date: Mon, 10 Nov 86 22:38:38 PST 
To: dernst@b39 
Subject: "Dennis, Hmmmm... I thought that"... 

Dennis,
   Hmmmm...  I thought that you said that SV2770.IMG would be out on 57
later this afternoon... It apparently never made it...  I got impatient
and copied (CURR11:39)SV2770.IMG to (CARL:930) and reloaded it.

It seems to have set the highest line to 1200 baud instead of 9600 baud.
i didn't have time to try out the other 3 libut assume that something
may not be right...

Did you finish?  (not on 57? so maybe you have to make some more changes?)
What I copied from 39 works fine, FF and all that... just at 1200 baud.

  I'll give you a call this next afternoon after our monitor group meeting.

/Carl
From: CARL@B39.Tymnet 
Date: Mon, 10 Nov 86 22:45:48 PST 
To: dernst 
Subject: "I see... You switched them 1200"... 

...Oh I see...
  You switched them 1200 on the last 2, and 9600 on the next to last 2.

that's OK. As long as I know that that is what it is configured for. They
seem to work fine at 1200.  I'l try the 9600 line tomorrow to see what it
looks like.

/Carl
Received: from X930.Tymnet by B39.Tymnet; Fri, 6 Feb 87 19:49:02 PST
Return-path: <JMS@X930.Tymnet> 
From: JMS@X930.Tymnet 
Date: Fri, 6 Feb 87 19:47:07 PST 
To: DERNST 
Subject: "TRW has a 2020 connected to TRWNET"... 

TRW has a 2020 connected to TRWNET and would like to upgrade the code
running in their microTYMbase.  They would like to go thru the proper
channels - who should they contact?
		/Joe
Received: from X62.Tymnet by B39.Tymnet; Sun, 8 Feb 87 2:14:58 PST
Return-path: <CARL@X62.Tymnet> 
From: CARL@X62.Tymnet 
Date: Sun, 8 Feb 87 2:08:02 PST 
To: dernst@b39 
Subject: "Re: Our micronet... I don't know"... 

Re: Our micronet...

I don't know if I ever got around to sending you mail about this... but
we spent about 4 hours a week or so ago looking at the address and vector
jumpers/switches on the various boards and looked at the data lines with
a scope and VOILA, 6760 is now talking to 2346!

One problem... the code in 6760 does not know how to build a circuit thru
2346 to get to :62 on 10115 or to :930 on 6657, although we can build any
circuits we want from :62 or :930 to 95 and they correctly take the right
path through 2346 (when it's up).

We tried the code we have on SYS, the code in DERNST, the code on :57 and
the code on :39 from wherever we could find the .BND or .IMG files.  None
of them seem to work right.

From a port on 6760 we can NOT build a circuit to :930 or :62 via 2346 at
all.  We suspect the routing table is wrong... but could not find anything
wrong in your sources on any of the systems.

Could you please take a look?

/Carl
From: WWANG@B39.Tymnet 
Date: Wed, 11 Feb 87 20:10:10 PST 
To: oserang@39, dchang@39, dernst@39, richteng@39, achippa@39, gazel@39,
	wwang@39 
Subject: "TO Omar From: W. Wang CC David, "... 

  TO  :  Omar
  From:  W. Wang
  CC  :  David, Rich, Ashok, Gazel, Dennis
  Subj.  X25 login string
   
  If you look into the hostname GED you will know that the syntax you 
addressed is far from the one defined in the GED. The SUP should detect
the DNIC before any of the following three different kinds of host spec's,
	0 + hhhhh + (ssss)
	9 + xxx...
	8 + hhhhh + (ssss)

  These Tymnet-unique cases are already finalized unless you have good
reasons. 
From: Richard Teng <richteng@B39.Tymnet> 
Date: Tue, 31 Mar 87 17:15:18 PST 
To: dernst 
Subject: Transit Field in login string 

Dennis, Bill:

I realize that this is rather late in the game for revisions, but I was
wondering if there was any compelling reason (e.g., CCITT compatiblity)
for having a transit field per se in the login string.  It seems to me
that this has several drawbacks.  Consider the login string:

	JSMITH=2000:-3000(408)446-6000

The Sup cares about the syntax in the destination field only when there
is no transit field (i.e., when the Sup actually targets to the
specified destination).  The syntax diagram reflects this.  However,
when there is a transit field, the Sup merely passes the destination
field on to the next network.  The next network might not accept this
syntax (with dashes, parens, etc.)  or our syntax might be too
restrictive.  In short, it does not make sense to impose Tymnet syntax
on a field which is interpreted elsewhere.
	  
You might argue that the destination field syntax should not be checked
whenever there is a transit field.  But this is very confusing and makes
the syntax diagram misleading.  You might also argue that the user
should use the extension field when he wants to pass an arbitrary
string.  But then why have the transit field at all?  Why not always
force data for other networks to be put in the extension field?

Another drawback is that the transit field horribly confuses
terminology.  It has been customary to use the term "destination" to
refer to a host within the Sup's network.  And other terms are naturally
spawned off - destination name (as opposed to a username), destination
host list, etc.  But these can be easily confused with the items in the
destination field of the login string.  In my ADRII GED (New
Addressing), I end up using awkward terms such as "local destination
specification" to refer to the specification relevant to Sup targetting
(i.e., the transit field if present, else the destination field).  I
admit this latter problem seems very minor, but I think we should not
underestimate the confusion that can be caused by poor terminology.

In light of this, I suggest that we drop the transit field entirely
and simply tell our customers to use the extension field.  We may
eventually provide a syntax chart for extensions which are passed to
Tymnet-technology networks.  But we should emphasize that the contents
of the field should be a function of the neighboring network, not the
current one.

Please give me your thoughts on this matter.  If we must conform to
international standards, perhaps we can do so in a better way.

- Rich -

P.S.  Bill... have you thought about the priority stop issue?

From: CARL@B39.Tymnet 
Date: Mon, 27 Apr 87 15:48:46 PDT 
To: dernst 
Cc: carl 
Subject: uNode questions 


Q1: 6657 has an extra DUV-11 card in it's current configuration.  Is it
    possible to have two TYMNET connections in that box and have the 2nd
    connection to node 3143 (for access to our hosts when 3146 is down)?
    I'm not asking about passthroughs and such, just 2 connections on the
    same micronode to the real world.

Q2: Is the DL11V-J card necessary in the configuration for the LSI-11 boxes?
    Currently they can be configured in one box with KD11-HA, MSV11-D,
    DUV-11, DLV11-J and DRV-11 that will allow one host connection and one
    network connection.  Can we replace the DLV11-J card with a DRV-11 and
    still run (without a console)?

Q3: About the dual DR-11 card in node 2770.  Do we know for a fact that it
    is broken?  IS there any documentation on it, what switches are for
    each DR, will one of them work, etc...

Q4: (Assuming we will not immediately replace the dual DR) Are we likely to
    be connecting the second DR11 in the 2020 to a second node, e.g. one to
    BUBB-NET?  If not, it makes sense to put it in the 11/04 and use it for
    the connection to host 118 (Yes or No?)

----------------------------
PS.  Please don't DO anything with the configuration as yet, I'd just like
     the answers to the questions for now.

    I'm planning on putting together an effective network configuration
    using as little of your time as is necessary...  Thanks for the help.

/Carl
From: DERNST@B39.Tymnet 
Date: Tue, 28 Apr 87 9:31:59 PDT 
To: CARL@B39.Tymnet 
Cc: carl 
Subject: Re: uNode questions 
In-reply-to: your message of Mon, 27 Apr 87 15:48:46 PDT

YOUR ANSWERS:
Q1:  YES, THAT WORKS VERY WELL.

Q2:  NO, THE LSI MICROCODE NEEDS TO TALK TO H
     THE CONSOLE ADDRESS.

Q3:  I AM PRETTY SURE.  I HAVE THE DOCUMENTATION.  HARDWARE SHOULD HAVE
     ONE AS WELL.

Q4:  YES, IF A GOOD DR-11C CAN BE FOUND OR YOU ARE WILLING TO GIVE UP
     8 PORT OF DZ-11 IN THE 11/04.

Received: from X930.Tymnet by B39.Tymnet; Wed, 29 Apr 87 12:50:40 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 29 Apr 87 12:29:36 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: Dual DR-11 board info 

Dennis,
  Could you please send me a copy of the ABEL dual DR-11 board documentation
that you have, including any hardware prints... if we can have the board
repaired, then we will not have to use a single DR-11 board.

  I will send you a copy of the current configuration and a copy of our new
proposed configuration as soon as I draw it up.  We've replaced the 11/03-LJ
box that was 2346 with an 11/03-SE & BA11-ME set since we were having so many
problems with the "LJ" box and no one seems to have time to find out what is
wrong with it... sigh.

/Carl
Received: from X930.Tymnet by B39.Tymnet; Thu, 30 Apr 87 19:29:33 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 30 Apr 87 19:30:28 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: Host numbers 

Dennis,
  Please don't bother to build the code for our nodes just yet.  I am
going to talk to validations, and will probably get a couple of 2-digit
host numbers for our hosts, thus freeing numbers 118, 930 and 934 for
use as host numbers for the DZ's and maybe the DZV's.  I'll let you know
the final host numbers as soon as I finish talking with Validations.

/Carl
From: CARL@B39.Tymnet 
Date: Fri, 1 May 87 17:26:52 PDT 
To: dernst 
Subject: mail.fwd cleanup 

Deleted MAIL.FWD from KJJ, JKATO and CCHIANG
changed B36 to B39 in CRUISING

Have a nice weekend.
From: Gazel Tan <GAZEL@C39.Tymnet> 
Date: Fri, 22 May 87 17:09:21 PDT 
To: kopfj, dernst 
Subject: austpac 


Hi gang,
David and Omar expect to have some write-ups ready for review by Tues am.
They will distribute - let's get together Tuesday  2pm to discuss the
subject matter as well as using the NSH, interfacing to it and impact on
NETVAL, etc...
Thanks .. and have a good weekend!
Gazel
----------------------------------------------------------------
Received: from X930.Tymnet by B39.Tymnet; Fri, 29 May 87 16:36:32 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
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
Received: from X930.Tymnet by B39.Tymnet; Wed, 15 Jul 87 2:15:39 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 15 Jul 87 2:13:07 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Cc: Joe Smith <JMS@X930.Tymnet>, Osman Guven <Osman@X930.Tymnet>, dencoff 
Subject: Micronode changes 

Dennis,
  I've sent you interoffice mail with a node map and hardware layout of
the new proposed micronet.  I've created a file (CURR11:930)RUFSC.MAC that
has the routing information (a la RULAB.MAC) with the nodes that are NOT
part of our local net removed.

  I put routing info in for the host numbers that I've chosen for the DZ's
and the (future) DZV's for comment... they can be commented out so that
they would go to TYMNET and back into the micronet where necessary.  We
can talk about that part when you and I talk on the phone.

  Please call me when you receive the inter-office mail.

/Carl
Received: from X930.Tymnet by B39.Tymnet; Wed, 15 Jul 87 15:48:15 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 15 Jul 87 15:43:47 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet>, Monitor Group <CARL@X930>, Osman Guven
	<Osman@X930.Tymnet>, Joe Smith <JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey <dencoff@X930.Tymnet>
	
Subject: PDP-11 Diagnostics 

Dennis,
  It is my impression that the node code loaded by NODLOD in *.IMG files
is an image copy made using LINK11 of code that runs in the PDP-11.  It
is also my impression that *.BND files are also possible to be built and
then loaded with LOADII.

  The hardware FE's have some standard diagnostics for PDP-11 based systems
which (on the 11's with disks or tapes) can be loaded and run.

  Do you know if the format of either the BND or IMG files is the same or
similar to the DEC standard diagnostics, and if it is reasonable to expect
that we could load these diagnostics from a TYMCOM-X system using NODLOD or
LOADII?

  If the formats are different, it may be possible (should be, anyway) to
convert them to either IMG or BND file format, and then load them into the
LSI's to run memory or other diagnostics in order to trace some of our
intermittant faults.

  I am asking, because this may be the method to use to try and diagnose
why the micronode at TRW keeps halting... the symptoms suggest a power-
supply problem, but I am unsure, and would like to try another route...
besides, it will give us a way to trace and fix the intermittant problems
we have with two of our micronodes.

/Carl
From: DERNST@B39.Tymnet 
Date: Thu, 16 Jul 87 11:08:12 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Cc: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet>, Craig Fletcher <fletcherc@X930.Tymnet>, Dennis
	Coffey <dencoff@X930.Tymnet> 
Subject: Re: PDP-11 Diagnostics 
In-reply-to: your message of Wed, 15 Jul 87 15:43:47 PDT

Carl,
  Once upon a time, we had a project to get the source
for the LSI-11 diags (yes, DEC sells them) and assemble them
using our PDP-11 assembler.  They could then be converted
to BND format.  However, since the normal method used by the
field people when the want to do diags is to read them from
mag tape using a portable TU-58, the project was dropped.  They are
also available on paper tape.  

I got your stuff in the mail.  Will call you when I have had a chance to 
look it over.

Dennis
Received: from X930.Tymnet by B39.Tymnet; Thu, 16 Jul 87 13:12:00 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 16 Jul 87 13:06:38 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: Changes to uNet 

Dennis,
  In addition to what you received in the mail, there a a couple of "trivial"
changes, as follows:

    o  The second DUV11 in node 2346 has been "assigned" to be the link to
       host BNET  (Network DUV11 driver)

    o  Another host number, for a DZV11 to be installed in the free slot in
       node 2346  (host #4285, I believe).  If the DLV11 can do full moden
       control, then the DLV11 would work as this host, but I believe that
       the DLV only has limited modem control... haven't really checked...

    o  Node YYYY has another host link; a DUV11 x.25 line into a microVax II.

  I have some questions regarding the host "devices" and the translations at
which points in the micronet versus host numbers for these devices.  WHen I
talk to you, I'll go over these changes and my questions.

  I believe that the routing map (CURR11)RUFSC.MAC is completely correct with
the exception of the host numbers for the DZ-11 and DZV-11s.

/Carl
Received: from X930.Tymnet by B39.Tymnet; Fri, 17 Jul 87 18:17:10 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Fri, 17 Jul 87 18:15:47 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: red herring 

Actually, it's more like "back to the drawing board"...

We were planning on "giving" one or both of our F3s to Tymnet training
as part of TrainNet.  It so happens that this is NOT going to occur.
So, the interconnect to TrainNet is now being removed from our plans.

I will regroup our systems and configuration to reflect this change and
send you a copy of the new configuration via interoffice mail.  This will
give us an option or two that we had talked about, such as possible host
numbers directly on BNET so that we can communicate with B39 without the
added gateway problems.  Possibly host 930 or something like that.

/Carl
Received: from X930.Tymnet by B39.Tymnet; Mon, 27 Jul 87 14:17:32 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Mon, 27 Jul 87 14:08:57 PDT 
To: Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff, Dennis Ernst <DErnst@B39> 
Subject: TXS micronet *.BND files are now on F38 and D37. 

Since (NEW11:57)ND6657.BND,ND6760.BND,ND2770.BND,ND0115.BND went away when
system 57 was shut down, I went to (NEW11:39) and telecopied the files to
(NEW11:38) and (NEW11:37).  This puts the required files on 3 consecutive
systems (37,38,39) in 3 locations (Dallas, Fremont, Bubbnet).  The TXS
micronet files in (CURR11), (OLD11), (DOC11), and (TOOLS11) have also been
copied from 39 to 38 and 37.

The ND####.BND files in (NEW11) on 62, 95, and 930 might not be correct, due
to changes in the routing table (which is not stable yet).

			/Joe
Received: from X930.Tymnet by B39.Tymnet; Mon, 27 Jul 87 14:37:58 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 27 Jul 87 14:29:49 PDT 
To: Monitor Group <CARL@X930>, Osman Guven <Osman@X930.Tymnet>, Joe Smith
	<JMS@X930.Tymnet> 
Cc: Craig Fletcher <fletcherc@X930.Tymnet>, Dennis Coffey
	<dencoff@X930.Tymnet>, Dennis Ernst <DErnst@B39.Tymnet> 
Subject: TXS-NET map 

Dennis,
  (CURR11:930)RUFSC.MAC  is complete, showing all host and node links
for our micronet.  We do not have a kernel host # on BNET for 2346
(or have verified that 2346 is available as a node on bubbnet) and have
not getten a host number for the KS on bubbnet yet... but that will not
be needed until late October most probably anyway.

  Let us know when the code is ready so we can start reconfiguration.

Thanks again...

/Carl
Received: from X930.Tymnet by B39.Tymnet; Mon, 27 Jul 87 14:47:06 PDT
Return-path: <JMS@X930> 
From: Joe Smith <JMS@X930> 
Date: Mon, 27 Jul 87 14:46:16 PDT 
To: Dennis Ernst <DErnst@B39>, Monitor Group <JMS>, Carl, Osman 
Cc: FletcherC, DenCoff 
Subject: TXSBND.WHR and TXSIMG.WHR 

(JMS:930)TXSBND.WHR shows where our ND####.BND files are.
(JMS:930)TXSIMG.WHR shows where our SV####.IMG files are.
The files are sorted by file name, checksum, then directory:system.
Received: from X930.Tymnet by B39.Tymnet; Tue, 28 Jul 87 21:19:24 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 28 Jul 87 21:19:53 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: NAD 

A little bird told me that you were the last person to do anything to the
NAD program... or at least it was rumored that you had...

Do you know who is responsible for said product, or anything about it's
internal workings.  I'm asking, since I'm concerned about how protective
NAD is about permitting read/write access to files from various user and
system directories.  NAD has WF license in certain copies.

Thank you for any assistance you can offer in this regard.

/Carl
From: DERNST@B39.Tymnet 
Date: Wed, 29 Jul 87 15:37:05 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: NAD 
In-reply-to: your message of Tue, 28 Jul 87 21:19:53 PDT

CARL, YOU CAME TO THE RIGHT PLACE.  NAD IS A REGULAR PROGRAM, EXCEPT
THAT IS PARTICULAR WHO IT TYPES ERROR MESSAGES OUT TO.  HF AND WF APPLIED TO IT
ARE PROBABLY NOT CORRECT.  TALK TO YOU ABOUT IT LATER.
DENNIS
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
Received: from X930.Tymnet by B39.Tymnet; Wed, 5 Aug 87 15:11:19 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Wed, 5 Aug 87 15:08:56 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: Moving? 

Are you finished moving yet???

How is Bldg A? ... or the commute?

Have you had any time to work on our reconfiguration of the uNet?
(we heard that we may be moving the week of Aug 24-28... so, I guess
I'm getting a little anxious...)

/Carl
Received: from X930.Tymnet by B39.Tymnet; Thu, 13 Aug 87 0:24:24 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Thu, 13 Aug 87 0:21:17 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: Dennis, 

  I have a meeting with Bill Soley at 1:30pm Thursday.  After that meeting
I will stop by your office with the diagrams and changes I've made to the
micro-net configurations.

  If you are not going to be available, please let me know, so I can try to
write up a description of what we need done, rather than having you try to
figure out my scribblings alone.

/Carl
From: CARL@B39.Tymnet 
Date: Thu, 20 Aug 87 1:17:39 PDT 
To: dernst 
Subject: uNet changes 

Hi...
   I realize that you are extremely tied up, but we are trying to reconfigure
before we move... and we only have 2 days left.  Monday they pull the plug on
our machines.

   I have edited copies of CG####.MAC and MC####.CMD and RUFSC.MAC in my
directory (CARL) on system B39.  I have also compiled these producing the
CG####.OBJ and RU####.OBJ files in my directory on B39.

   (I actually did all the work in (NEW11:95) but copied the results to B39).

   I would appreciate it if you could make the final changes to LK####.CMD
to include the modules required and link the resultant code...

   I'm pretty sure the CG####.MAC files are correct.  Please check these:
	CG2770.MAC -- did I do the LNKH macros correctly for :4117,:4284 ?
	CG6657.MAC -- did I do everythng necessary for 2 TYMNET links    ?
	CG6760.MAC -- same question as for 6657
	CG2346.MAC -- did I add the DZV-11s linex macros correctly?
   all therest should be OK.  I made sure each link has one P and one S
   and that the number of ports match...

    My only other question has to do with the LINK macros for type I network
links... the <node,nn.> at the end, eg <6657,12.> ... what is the "12."?


zIf all the stuff is OK, please copy (CARL:39)MC####.CMD+(CARL)CG####.*+
(carl)ru####.obj to (NEW11) and link them...  I don't want to overwrite
what you have in NEW11 without your OK.

/Carl
From: CARL@B39.Tymnet 
Date: Thu, 3 Sep 87 23:23:46 PDT 
To: dernst 
Subject: NAD sources 

Dennis you have:
   NAD01.BAK  & NAD02.BAK   which appear to be previous versions of
   NAD01.R06  & NAD01.R07   respectively... the difference between each
                            set (*.BAK and .R06/.R07) is the same.

   NAD01.R06 has a newer creation date than .R07 and the major difference is
the treatment of underline "_" between .R06 and .R07.  Which version should
I change?

/Carl
Received: from D35.Tymnet by B39.Tymnet; Thu, 3 Sep 87 23:33:15 PDT
Received: from B39.Tymnet by D35.Tymnet; Thu, 3 Sep 87 23:28:20 PDT
Return-path: <Carl@D35.Tymnet> 
From: CARL@B39.Tymnet 
Date: Thu, 3 Sep 87 23:07:50 PDT 
To: wrs, bartletw 
Subject: NAD %1.07 
Resent-from: Carl A Baltrunas <Carl> 
Resent-date: Thu, 3 Sep 87 23:37:00 PDT 
Resent-to: Dennis Ernst <DErnst@B39.Tymnet> 

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
From: CARL@B39.Tymnet 
Date: Thu, 10 Sep 87 23:36:20 PDT 
To: dernst 
Subject: NAD 

Dennis,
  I've made the changes that I mentioned to you the other day to a copy of
your file (DERNST)NAD01.R06 which resides in (CARL)NAD01.R06.

The changes are as follows:

 1)  Listing control is dependent soley on TP in .GTPRV for the user.
 2)  Input files in (SOURCE) or (BETATEST) may be read by anyone that
     can run NAD.  Listings or symbols in NIB files still require TP.
 3)  Command files may be read from directory (SOURCE) or (BETATEST) by
     anyone that can run NAD.

When the program is released to (SOURCE) it should have RF license and
be protected ALL RUN RUN.  It is NOT to be put on (SYS).

Bill Soley will replace the NAD on sys with a program which verifies who
the user is by GAN and/or USERNAME and run (SOURCE)NAD for that user who
is allowed to run NAD.   (If he hasn't done it already).

I didn't know whether to rename the file to NAD01.R07 or to change the
name in the source from .R06 or not.  I need someone to test the changes
more thoroughly.  (CARL)NAD###.DIF shows the differences between your
file .R06 and my file .R06 on system 39.

Thanks.

/Carl
Received: from F33.Tymnet by B39.Tymnet; Tue, 15 Sep 87 13:00:48 PDT
Return-path: <JMS@X32> 
From: Joe Smith <JMS@X32> 
Date: Tue, 15 Sep 87 12:58:16 PDT 
To: CARL@D35, DERNST@B39 
Subject: NEW11 etc copied from B39 to X32. 

CURR11, NEW11, TOOLS11, DERNST, and part of OLD11 have been copied to
system 32 (the test KI n Fremont).  They were restored with MODE OLD NEWEST.
			/Joe
Received: from D35.Tymnet by B39.Tymnet; Tue, 15 Sep 87 21:15:20 PDT
Return-path: <Carl@D35.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Tue, 15 Sep 87 21:15:13 PDT 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: NAD 

Have you had time to check what I did to NAD?
/Carl

Received: from D35.Tymnet by B39.Tymnet; Mon, 28 Sep 87 12:54:34 PDT
Return-path: <Carl@D35.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Mon, 28 Sep 87 12:29:34 PDT 
To: dernst@b39 
Subject: NAD 1.6 (or 1.7) 

What testing has been done to your 1.6 modifications?
(Exactly what were your changes supposed to fix, enhance)?

What has to be done for NAD to be given to QA?

(I don't care whether my changes are included in 1.6 or whether we
 change the version to 1.7 as long as the new version goes out soon.
 I'll be happy to do the legwork if you tell me what needs to be
 done and who needs to get what information.)

/Carl
Received: from D35.Tymnet by B39.Tymnet; Mon, 30 Nov 87 21:45:58 PST
Return-path: <Carl@D35.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Mon, 30 Nov 87 21:38:12 PST 
To: William R Soley <WRS@B39.Tymnet>, Wayne D Bartlett <BARTLETW@B39.Tymnet> 
Cc: Dennis Ernst <DErnst@B39.Tymnet>, Doug Ellenberg
	<QSATS.D/ELLENBERG@ONTYME.Tymnet> 
Subject: NAD %1.7 

Bill & Wayne,
  As per Doug's request, I have added directory (QSATS) to the list of valid
directories.  My previous version %1.6 is now (CARL:39)NAD1x6.SAV and the
new version %1.7 is now in (CARL:39)NAD.SAV.

  This version %1.7 does NOT include the changes made by Dennis Ernst to NAD
in his directory (dated 24-Jul-87) which changes the treatment of the "_"
character.  If Dennis wants his change incorporated into my version, I will
add it, but not without his confirmation.  Dennis?

  If my address for Doug is incorrect, please forward this message to him.

  The new version is ready for release/testing.  The only change between the
version now running as 1.6 and this version is the addition of QSATS to the
list of valid directories.

/Carl
From: DERNST@B39.Tymnet 
Date: Tue, 1 Dec 87 8:28:49 PST 
To: CARL@D35 
Subject: NAD 1.7 

CARL,
  THE "_" CHANGES WERE JUST STARTED IN MY V1.7.  THEY ARE NOT OFFICIAL OR
EVEN WELL THOUGHT OUT.   THERE HAVE BEEN ONE OR TWO NEW BUGS REPORTED
AND THEY WILL HAVE TO BE FIXED, BUT WE HAVE HAD THEM FOR 5 YEARS
NOW.  SO THERE DOESN'T SEEM TO BE A RUSH.

DENNIS
From: CARL@B39.Tymnet 
Date: Wed, 9 Dec 87 1:17:17 PST 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: NAD 1.7 

Dennis,

I have moved all copies of NAD from my directory back into the NAD directory.
Unless I notify you further, (NAD)NAD01.R06 and (NAD)NAD01.R07 are the new
base versions for any changes you wish to make.  NAD01.R06 is based on your
version of NAD01.R06 in directory DERNST:39.  NAD01.R07 contains additional
changes to .R06.  (NAD)NAD.SAV is the to-be-distributed version.  The files
(NAD)NAD1X6.SAV and (NAD)NAD1X5.SAV are the previous versions of NAD.SAV.

Please delete or rename your NAD01.R06 and rename your NAD01.R07 to avoid
confusion in the future.

Thanks again for you help in locating the sources.

/Carl
From: JMS@B39.Tymnet 
Date: Fri, 11 Dec 87 12:32:59 PST 
To: DERNST 
Subject: "I typed in the info on using the"... 

I typed in the info on using the status device from Carl's photocopy.
It is in (NEW11)XRAY11.HLP.  /Joe
Received: from X32.Tymnet by B39.Tymnet; Wed, 23 Dec 87 14:03:08 PST
Return-path: <Carl@X32.Tymnet> 
From: Carl A Baltrunas <Carl> 
Date: Wed, 23 Dec 87 13:53:15 PST 
To: Dennis Ernst <DErnst@B39.Tymnet> 
Subject: uNet circuits 

Dennis,
  We rebuilt the micronode code recently when we split node 2770 into two
physical boxes, 2770 and (for the moment) FRED.  Since the rebuild, when
we try to build a circuit to :TR04@2770 or :4284.04 (thru TYMNET) we find
that the circuit never succeeds.

  The old configuration had 2770 with 2 sync lines, 1 to TYMNET, the second
to the rest of the micronet (2770 <==> 2346).  We received a lot of errors
in this configuration, so we split them.  FRED took on the exact hardware
configuration that 2770 had, except we added a DRV link to the new 2770.

Fred is using 2770's old 11/03 NETCON box.  The DUV11 for TYMNET is installed
in the box, but the TYMNET line is not in the configuration...

2770 is in a new 11/03-SE box with CPU-MEMORY, 1 DUV11 to TYMNET, 1 DLV11 and
1 DRV11 connected to FRED.

Anyway... just prior to re-configuring 2770 and FRED, everything worked fine
except circuits kept getting zapped due to the two DUV11s stepping on each
other.  The new configuration solves that problem, but now we seem to have
trouble building circuits to the terminal devices on either 2770 or FRED.

Any ideas on what could cause that?

The sources are all in (NEW11:32) and (NEW11:14).  Thanks fo r your help...

/Carl
Received: from X17.Tymnet by B39.Tymnet; Fri, 29 Jan 88 12:14:18 PST
Return-path: <JMS@F29.TYMNET> 
From: Joe Smith <JMS@F29.TYMNET> 
Date: Fri, 29 Jan 88 12:14:42 PST 
To: dernst@39 
Subject: Host X17 and its base both crashed 

X17 got a KAF stopcode at 29-Jan 00:01 and its base, node 6657, got
Fatal node error TMP
Registers:
  000001  157100  157103  000006  131326  140250  143460  020104  000013
Stack:
143460:  077372  131654  157100  071740  000000  064634  157100  066410
143500:  131326  013640
Debug: 0 0 0 0 0 0 0 0
From: PEGGYH@B39.Tymnet 
Date: Mon, 28 Mar 88 11:20:21 PST 
To: dernst, peggyh, dernst 
Subject: "is this a test?"... 


is this a test?
From: PEGGYH@B39.Tymnet 
Date: Mon, 28 Mar 88 11:32:58 PST 
To: anewman, dernst, hrivers, jsoung, shsu, hueyma, mele, hudsong 
Subject: "Files were accidentally deleted"... 

Files (netmid)*.f44 were accidentally deleted Monday morning at 
approximately 9 am.  A batch request for restoration of these files
has been sent to operations.  My apologies for any inconvenience this
will cause you.

Peggyh

F.Y.I.  The access list restrictions for directory netmid do not seem
to be in effect at this time. 

Received: from tymix.Tymnet by B39.Tymnet; Thu, 7 Apr 88 12:02:26 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA23114; Thu, 7 Apr 88 11:59:51 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA16476; Thu, 7 Apr 88 11:59:46
	PDT
Return-path: <anewman@calvin> 
From: anewman@calvin (Adele M. Newman) 
Date: Thu, 7 Apr 88 11:59:46 PDT 
To: anewman@calvin, ernst@calvin, hudsong@opus, hueyma@calvin, jsoung@calvin,
	mele@hobbes, shsu@calvin 
Cc: dernst@B39, hueyma@B39, jsoung@B39, mele@B39, shsu@B39 
Message-id: <8804071859.AA16476@calvin.Tymnet.com> 
Subject: TVKT 

The complete dossier on Televerket support (escalations for 4.03) is now
sorted and in a large notebook on our community shelves.  At the end of
the book is a set of significant correspondence, including notes about
problems which seem to have no escalations attached.
//Adele
Received: from tymix.Tymnet by B39.Tymnet; Mon, 25 Apr 88 11:18:19 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02978; Mon, 25 Apr 88 11:15:12 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA26258; Mon, 25 Apr 88
	11:15:02 PDT
Return-path: <anewman@calvin> 
From: anewman@calvin (Adele M. Newman) 
Date: Mon, 25 Apr 88 11:15:02 PDT 
To: anewman@calvin, dernst@B39, ernst@calvin, hudsong@opus, hueyma@calvin,
	hueyma@B39, jsoung@calvin, jsoung@B39, mele@B39, mele@hobbes,
	shsu@calvin, shsu@B39 
Message-id: <8804251815.AA26258@calvin.Tymnet.com> 
Subject: REMINDER - section meeting today 

Section meeting at 3 PM in small conference room:
1)  NSR status for 4.03
2)  Testing for new patch file?
3)  What still needs to be incorporated in 5?

//Adele
Received: from tymix.Tymnet by B39.Tymnet; Mon, 2 May 88 12:44:22 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA24442; Mon, 2 May 88 12:47:04 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA12416; Mon, 2 May 88 12:46:56
	PDT
Return-path: <anewman@calvin> 
From: anewman@calvin (Adele M. Newman) 
Date: Mon, 2 May 88 12:46:56 PDT 
To: anewman@calvin, dernst@B39, ernst@calvin, hudsong@opus, hueyma@calvin,
	hueyma@B39, jsoung@calvin, jsoung@B39, mele@B39, mele@hobbes,
	shsu@calvin, shsu@B39 
Message-id: <8805021946.AA12416@calvin.Tymnet.com> 
Subject: section meeting today 

I'm still planning on a section meeting today -- but I have a meeting 
regarding the SWBT presentation at 2 PM, and it could run late.  I'll let
you know if we need to delay the section meeting.
//Adele
Received: from tymix.Tymnet by B39.Tymnet; Mon, 20 Jun 88 9:21:54 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA29631; Mon, 20 Jun 88 09:18:58 PDT
Received: by calvin.Tymnet.com (3.2/SMI-3.2) id AA03445; Mon, 20 Jun 88
	09:18:46 PDT
Return-path: <anewman@calvin> 
From: anewman@calvin (Adele M. Newman) 
Date: Mon, 20 Jun 88 09:18:46 PDT 
To: anewman@calvin, dernst@B39, ernst@calvin, hueyma@calvin, hueyma@B39,
	jlucas@calvin, jlucas@B39, jsoung@calvin, jsoung@B39, mele@B39,
	mele@hobbes, shsu@calvin, shsu@B39 
Message-id: <8806201618.AA03445@calvin.Tymnet.com> 
Subject: XCOM directories 

There are now directories for test plans and GID's under /xcom/vs5.00 and
also a new vs6 directory with subdirectories.
//Adele
  X #{