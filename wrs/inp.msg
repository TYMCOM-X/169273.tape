Received: from F29.Tymnet by B39.Tymnet; Fri, 19 Jun 87 16:13:35 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Fri, 19 Jun 87 15:23:24 PDT
Return-path: <NTD.D/CHANG@EMSNTD.Ontyme.Tymnet> 
From: NTD.D/CHANG@EMSNTD.Ontyme.Tymnet 
Date: 19 JUN 87 15:02:39 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: (19 names) 
Message-id: I49602@Ontyme.Tymnet 
Subject: Numbering Plan for the Public Net 
NumberingPlanforTYMNET: 

TO:  Gary Stoy, Dennis Ernst, Jeff Oliveto
FR:  David Chang
RE:  Numbering Plan for TYMNET
DA:  June 19, 1987
CC:  Bill Soley, Gazel Tan, Adele Newman, and interested parties

======================================================================

I have prepared a report on TYMNET's numbering plan...what I perceived
are its implementation and its problems.  Please review it since I
probably misinterpreted some of the details.  You may considered it as a
preliminary analysis for the project (PIR #2432) to define the numbering
plan for the public net.  I think that it is important to gather the
interested parties together and discuss the issues.  I understand that
Jeff (703-356-6995 will be in town next week (June 24-26).  Perhaps it
would be a good idea to set up a meeting.  Gary, would you be interested
in arranging it?  We need minimally yourself, Dennis, Jeff, Bill Soley,
and myself.  All other interested parties would be welcome.

NOTE:  PIR #2432 is directed to marketing...does that mean you, Gary?

........................................................................

(DCHANG:39)NUMBER.RPT

created 6/18/87


This report discusses the TYMNET Numbering Plan...its current
implementation, its possible problems, and its future.  


BACKGROUND

Tymnet's Numbering Plan follows the CCITT Recommendations X.121 in
defining the international data number for data terminals.  The
recommendations state that the X.121 address is a decimal number
composed of a 4-digit DNIC and a 1-10 digit NTN.  Hence, this number can
be from 5 to 14 digits long, with the DNIC identifying the specific
network.  The first digit of the DNIC is specified as follows:

	0 - reserved
	1 - reserved, but DNIC 111x is used for maritime mobile service

	2 - used for DNICs
	3 -      "
	4 -      "
	5 -      "
	6 -      "
	7 -      "

	8 - may be used for interworking with telex networks
	9 - may be used for interworking with telephone networks

To address a terminal in another network, the full address with the DNIC
is necessary.  However, it may be desirable that to address a terminal
within the same network with only the NTN (abbreviated addressing).

There are two ways (perhaps more) to support abbreviated addressing.
One way is to define the numbering plan of the network in such a way that
one can always distinguish between the full address and an abbreviated
address.  This would usually entail the detection of a valid DNIC by
examining the first digit.  For example, if the first digit of the NTN
is always 0 and if no DNIC ever begins with a 0, then the full and
abbreviated addresses can be easily distinguished.

A second way to support abbreviated addressing is to use an
international prefix which is defined locally for the network.  This
prefix is not part of the address, but is used to distinguish between a
full and abbreviated address.  For example, if an NTN never begins with
a 0, then the 0 may be used to prefix a full address.

Note that a 0 is used as examples for the two methods because a 0 is
reserved and undefined as a first digit of a DNIC.  SWB is planning to
use a prefix of 1 in method 2 to support abbreviated addressing.  TYMNET
is using a form of method 1 in its numbering plan.

TYMNET'S NUMBERING PLAN

Currently, TYMNET is using a numbering plan which was conceived with the
notion that DNICs will never begin with 0, 8, or 9.  According to the
document (TECHREF:56)X121.TYM by Jeff Oliveto or someone in his
organization, TYMNET internal addressing standard has the following
format:
			DNIC + d + xxxxxxxxx
where
	DNIC is 3106 (TYMNET's data network identification code)
	d is the determining digit, with the following meaning:
		0 - specifies a 5-digit host number with optional 1-4 digit
		    subhost number
			3106 + 0 + hhhhh + ssss
		    Currently, the X.25 interface detects the 0 and converts
		    to a login string "username:hhhhh.ssss".
		1 - reserved, undefined.
		2 -	"
		3 - 	"
		4 - 	"
		5 - 	"
		6 - 	"
		7 - specifies the 5-digit host number of the originating
		    interface
			3106 + 7 + hhhhh
		    This is probably used as the calling address.
		8 - specifies a 5-digit host number with a 1-4 digit
		    subhost number which should not be optional
			3106 + 8 + hhhhh + ssss
		    Currently, the X.25 interface detects the 8 and converts
		    it to a login string "8hhhhh:.ssss".  "8hhhhh" should be
		    a MUD entry with the destination host hhhhh.  NOTE:  THE 
		    DOCUMENT INDICATES THE SUBHOST AS OPTIONAL WHICH DEFEATS
		    THE PURPOSE OF THIS DETERMINING DIGIT, WHICH IS TO
		    DEFINE ONLY ONE MUD ENTRY FOR THE VARIOUS SUBHOST NUMBERS.
		9 - specifies a 5-digit host number with optional 1-4 digits
		    subhost number
		    	3106 + 9 + hhhhh + ssss
		    Currently, the X.25 interface detects the 9 and converts
		    it to a login string with username "9hhhhhssss".
		    "9hhhhhssss" should have a MUD entry indicating hhhhh as 
		    the destination host.  This means that for each
		    subhost number there would be a MUD entry, hence the
		    introduction of determining digit 8.  NOTE:  THE
		    DOCUMENT INDICATES THAT "hhhhh + ssss" IS ACTUALLY A 
		    5-9 DIGIT NUN "nnnnnnnnn".
	xxxxxxxxx specifies the 0 to 9 digits based on the determining digit

Note that the 5-digit host number "hhhhh" in the above definitions is of
fixed length.  However, it is not clear whether this host number is always
necessarily padded by zeroes to form the 5 digits.  According to Dennis
Ernst, at least for the case of determining digit 9, the 5-9 digit NUN
may actually be less that 5 digits.

THE PROBLEM

Currently, the X.121 Numbering Plan project for SWB is in progress.
This project, defined by the GEDs for LOG2, ADR2 (formally Hostnames),
and MUD2 (formally MUD Reformat), is attempting to provide a fundamental
mechanism to support a general X.121 addressing scheme.  A goal of
this project is to remove the support of a numbering plan from the local
interface, eg. XCOM, to the Supervisor, thereby centralizing the
analysis of an address.  At the conclusion of this project, the
Supervisor will support not only the host number in the destination
field but also symbolic hostname and X.121 address.  The X.121 address
is designated with a prefix "-", which is used to distinguish between a
number and a full address.  Each legal hostname and X.121 address will
have an associated MUD entry for validations.

This project will provide support for abbreviated addressing for SWB.
Since SWB uses a prefix of 1 to indicate international calling and no
local NTN ever begins with a 1, there is no problem.  Furthermore, since
the NTN is guaranteed to be 6 digits or greater, then there is no
problem distinguishing between the local NTN and a real 1-5 digit host
number in the destination field.

This project had not been designed to support abbreviated addressing for
TYMNET.  This means that all addresses must be specified with the DNIC.
All internal representation of the X.121 addresses are of the form
		- DNIC NTN
If the XCOM interface receives an address, it must determine whether or
not the DNIC is there.  However, that defeats the goal of removing
the address analysis from the local interfaces to the Supervisor.
As currently defined, the "-" indicates a full X.121 address, ie. the DNIC
follows the "-".  In order that the local interfaces not check for the DNIC,
they could just insert the "-" before all addresses, full or abbreviated,
during the login process.  Then it would be up to the Supervisor to
determine the appropriate action, relieving the local interfaces from
this task.  In essence, the Supervisor in TYMNET's public network would
be supporting abbreviated addressing.

It would be fine and dandy to implement this simple design change of
redefining the meaning of the "-" (ie. in the login message to the
Supervisor, "-" defines a full or abbreviated address, and in all other
instances, "-" defines a full address)...EXCEPT for the possibility of
locking ourselves to the current numbering plan.  Note that 0, 7, 8, and
9 are legal first digits for an NTN in TYMNET's numbering plan.  Supporting
abbreviated addressing with the current numbering plan means that the
Supervisor may receive a full or abbreviated address of the form
		- DNIC NTN    or    - NTN
Suppose the first digit after the "-" is a 7, is this the beginning of a
DNIC or NTN?  Suppose we wish to support telex and telephone networks in
the future, how do we interpret the 8 or 9 after the "-"?

THE QUESTIONS

* Is the current numbering plan adequate and expandable for the public
net?
* Should we devise another numbering plan that will address current and
future needs?
* Can we formalize TYMNET's numbering plan in a document with not only
details on definition but also usage?  [For example, the usage of the
0, 8, and 9 in the current plan is very similar...specifying a host
number and a subhost number.  There should be a more efficient way of
doing this.]
* Can we dispense with the current one, ie. how dependent are the
current customers on the current numbering plan?  Perhaps, we can devise
a way of migrating to a new one, once we come up with a new plan.
* Does TYMNET expect to plan to support telex and telephone networks in
the future?  maritime mobile services?
* How should the addresses be assigned, by whom, and via what mechanism?
* Does the public net wish to support abbreviated addressing at the
Supervisor level?

These questions should be addressed ASAP.  There is a project, PIR #2432
"Public Net Numbering Plan", which is directed to marketing, but which is
not yet scheduled.  It is probably desirable to begin this project now
and answer the aforementioned questions.

Until the above questions are addressed, the X.121 Addressing project
will proceed as specified in the GEDs (MUD2, ADR2, and LOG2).  In other
words, the Supervisor will support the current numbering plan and not
support abbreviated addressing for the public net.  That means that the
XCOM interface and perhaps other interfaces must analyze the address and
insert a default DNIC whenever appropriate.
Received: from F29.Tymnet by B39.Tymnet; Wed, 24 Jun 87 14:28:20 PDT
Received: from EMSNTD.Ontyme.Tymnet by F29.Tymnet; Wed, 24 Jun 87 14:23:22 PDT
Return-path: <ENS.J/OLIVETO@EMSNTD.Ontyme.Tymnet> 
From: ENS.J/OLIVETO@EMSNTD.Ontyme.Tymnet 
Date: 24 JUN 87 14:16:45 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: (19 names) 
Message-id: I51369@Ontyme.Tymnet 
Subject: PIR 2432 - Tymnet's Internal Numbering Plan (INP)/06240187 
PIR2432-Tymnet'sInternalNumberingPlan(INP)/06240187: 

          TYMNET - McDonnell Douglas Network Systems Company
            External Network [ExNet]  -  Technical Services

        Tel# 703/356-6995   Telex# 82693 UF   Fax# 703/448-0346
                  Drop: 310600024915   or   TYMINTLVA

DATE>      24 Jun 87  10:10

TO>        David Chang
           Gary Stoy

COPIES>    See Distribution

FROM>      Jeff Oliveto

SUBJECT>   PIR 2432 - Tymnet's Internal Numbering Plan (INP)/06240187

-----------------------------------------------------------------------

David;

     I would first like to clarify on the contents of your Ontyme I49602
dtd 19 June, Subj: Numbering Plan for Tymnet.  Then I will address your
questions, and finally put forward some matters for discussion.

CLARIFICATION:

1)  You comment that Tymnet's current INP was conceived with the
notion that DNIC's will never begin with 0, 8 or 9.  This is not
quite correct.  Actually, Tymnet's INP design did not consider
abbreviated X.121 addressing as a necessity since the Tymnet
interface would assure that a proper address-to-username mapping
would always be performed prior to sending a username to the
Supervisor.  

  2)  For PNIC's you state that a subhost number should NOT be
optional.  On Tymnet, where the intent is to allow the customer as much
flexibility as possible, a null address (similar to 31069) should be
allowed.

  3)  For NUN's, there is NO required format as you state.  NUN's
310690000 thru 310699999 are reserved for internal use (although little
have been assigned).  



TYMNET - ExNet Tech Services                           Page - 02

  4)  Whats "good" for SW Bell is not necessarily "good" for Tymnet.
Remember, SW Bell has to "share" their DNIC 3146 with other networks
and must allow identification of a LATA within the first 6 digits of
NTN.  Requiring NTN's to be 6 digits so that an NTN can be
distinguished from a host number has two flaws, first it assumes that
true abbreviated X.121 addressing is of no use (911, 611, or 411),
second it assumes that a Tymnet network will NEVER have more that
99,999 hosts.

  5)  You imply that it is undesirable for a local interface to perform
any X.121 address analysis.  I'm not sure I agree on this.  One of
our greatest attributes as a VAN is our flexibility at the interface
level.  This should be open for discussion.


YOUR QUESTIONS:

  1)  NO, the current INP is not adequate for Public Tymnet!

  2)  We should MODIFY the current INP to correct its deficiencies and
to allow additional inter-networking capabilities.

  3)  Yes, such a document would be desirable.  See my comments for
additional input.

  4)  We CAN NOT simply "dispense" with our current INP.  Changing an
address can be a traumatic experience for a host and Tymnet.  

   .  There will be front software to be regen'ed (many hosts
don't gen their FE software and might incur a cost in doing so).

   .  There is a user community to be notified.  If the user
community is international, this could be a formidable task.  It
could be particularly disastrous for Tymnet since most high volume
hosts have interfaces on Tymnet & Telenet.  If the foreign subscriber
could not get connected via Tymnet, they would simply start using
Telenet.  Returning customers to Tymnet might be an impossible
task.

   .  Of course there are Tymnet interfaces to be regen'ed.

  5)  Yes, Tymnet does plan to support packet switch to low speed
data circuit switch (Telex) addressing.  We currently have 3 Telex
DNIC's loaded in Target Profiles).  Yes, we plan to support packet
switch to voice (telco) circuit switching to multiple devices e.g.
aync, X.25/X.32, RJE?, SDLC? Fax?.  We also plan to support packet
switching to ISDN.



TYMNET - ExNet Tech Services                           Page - 03

  6)  Currently addresses are assigned and tracked via a separate
automated system (DTE).  There are plans to integrate INP assignments
into the new Network Profile Data Base and OER system under control
of Network Operations.  

  7)  I think this is your REAL question, Does Public Tymnet want to
support abbreviated addressing on the sup or as it currently does in
the interface?  A decision on this should not be reached until all
viewpoints have been discussed.


FOR DISCUSSION:

A.  Reassignment of X.121 Addresses -  As mentioned above, this can not
be done arbitrarily.  The only real restriction in reassignment is that
the NUA's... addresses where the d digit equals 0 (zero) are the only
addresses that CANNOT be reassigned.  Tymnet could always retarget an
NUN (d = 9) or PNIC (d = 8) to an "operator intercept" providing the
correct number.  This would be a simple program on a PC connected to
the network via X.25.

B.  In reality, most functions of Tymnet's NUA's, PNIC's & NUN's could be
provided by a single numeric "hostname only!" NUA.  Example:

        Host 2356 Tymnet Address = 3106002356ssss

                where hostname -3106002356 

Benefits:

        .  Multi/Priority targeting (NUN & PNIC features)

        .  10,000 Sub-host capability (NUA & PNIC features)

        .  Password protection (PNIC & NUN features)

        .  Overall Tymnet INP simplification

Limitations:

        .  only 99,999 Tymnet addressable hosts available

        .  7 origination only addresses not considered (is this a problem?)

        .  Some form of NUN's would still be required for international
inbound calls allowing separate "User Numbers" for individual user group
authentication, although the number required would be greatly reduced.



TYMNET - ExNet Tech Services                           Page - 04

Questions:

        1.  Is the "-" (dash) required as a part of the hostname?

        2.  Can the "-" or some other special character be used to
identify a particular traffic class at the specified destination.  

        3.  Can a "- " (dash/space) indicate an abbreviated address?

C.  What about DNIC's that begin with 9 indicating a PSTN address
request?  Example:

   X.121 Address -914089808100 (Tymnet San Jose Vadic rotary)

Would probably map to hostname -91408980.  But how does the
subscriber select their class of service.  Does the user want:

        .  public async outdial?  private?

        .  X.25/X.32 sync outdial?

        .  X780 sync outdial?

        .  Teletex? Fax? etc...

   X.121 Address -94418475915  (Tymnet/Mercury London V.21 rotary)

        .  Same questions apply

To just address the PSTN we would have to support such addresses as:

        91 + 3 digit NPA + 3 digit DCO for U.S & North America

        944 to identify U.K.

        933 to identify France

        9852 to identify Hong Kong

D.  For ISDN, X.121 numbering does not apply.  But, a question that
needs to be asked now is; How does Tymnet intend to map ISDN
numbering into its X.121 INP.

E.  CMTI and 3270 HIF still perform sub-addressing via the first 2
(two) digits of the username forcing many hosts to use an NUN verses
the desNUA.  PIR's are required to force these interfaces to
conform to the long establish last 2 digit format of sub-addressing.


I would like to propose a meeting between Public Tymnet and NTD the
week of 27 July.  In the meantime Tymnet can research this topic
further and put together a proposal for NTD's review.  Gary, David,
is this acceptable?

- Jeff -
   zhA