From: WWANG@B39.Tymnet 
Date: Wed, 19 Mar 86 15:20:34 PST 
To: wrs@39, dchang@39, wwang@39 
Subject: Hostname Project 


Bill:
  I would like to set a meeting to discuss the implementation scheme for
BOC table, among other things. This should last no more than an hour. Its
better if this can be done this week. Thanks.
-Weyyi
From: WWANG@B39.Tymnet 
Date: Tue, 25 Mar 86 7:53:59 PST 
To: wrs@39, dchang@39, wwang@39 
Subject: Hostnames 


Bill:
  I got the answser from Jeff Oliveto of the 2nd question we had yesterday:
yes, to determine the LATA's is the only purpose to address NNX code. Thanks.
-Weyyi
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 1 Apr 86 0:06:29 UT
From: ENS.J/OLIVETO@Ontyme.Tymnet 
Date: 31 MAR 86 12:03:20 
To: NTD.WRS@Ontyme.Tymnet 
Message-id: I42262@Ontyme.Tymnet 
Subject: New LATA Survey 

========================  M E M O R A N D U M  ========================
 
          TYMNET - McDonnell Douglas Network Systems Company
        External Network Services [ExNet]  -  Technical Support
     Phone: 703/356-6995    Telex: (23) 82693    Drop: 310600024915
 
DATE>      31 Mar 86  14:30
 
TO>        Bill Soley
 
COPIES>    Weyyi Wang
 
FROM>      Jeff Oliveto
 
SUBJECT>   New LATA Survey
 
-----------------------------------------------------------------------
 
     IRT  ur Ontyme  I41019 dtd 27  Mar 86 and our phonecon of 28 March
I am in agreement that  doing   any    sort  of   address   checking to
differentiate  between  a BOC  address  that is  in an NPA in more that
one LATA vs  one that  is not  would  not be  worth the overhead on the
SUP.  I am willing  to  validate a 10 digit  username (4 digit DNIC + 3
digit  DNPA + 3  digit DCO)  for  each  supported  Data Central Office.
 
      Let me know if  you need  any  additional input IRT this subject.
By the  way, I got  a copy  of  the  HostName GED  in the mail today. I
will review it for any comments.
 
- Jeff -
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 15 Apr 86 15:02:24 UT
From: ENS.J/OLIVETO@Ontyme.Tymnet 
Date: 15 APR 86 06:57:05 
To: NTD.WRS@Ontyme.Tymnet 
Cc: ENS.J/OLIVETO@Ontyme.Tymnet, NTD.B/ATCHISON@Ontyme.Tymnet,
	NTD.W/WANG@Ontyme.Tymnet, NTD.D/ERNST@Ontyme.Tymnet,
	NTD.J/NEIDRAUER@Ontyme.Tymnet, NTD.WRS@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NTD.W/EUSKE@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet 
Message-id: I49890@Ontyme.Tymnet 
Subject: HostName GED 

========================  M E M O R A N D U M  ========================
 
          TYMNET - McDonnell Douglas Network Systems Company
        External Network Services [ExNet]  -  Technical Support
  Phone: 703/356-6995   Telex: (23) 82693   Terminal Adr: 310600024915
 
DATE>      15 APR 86  09:40

TO>        Weyyi Wang

COPIES>    See Distribution

FROM>      Jeff Oliveto


SUBJECT>   HostName GED


-----------------------------------------------------------------------


     After discussions with Dennis Ernst of the XCOM code group it has
been found that it would more advantages to have only 5 digits of host
number after 31068 followed by 4 sub digits.  Example:

   3106 + 8 + 5 digit host number + 4 digits of sub addressing

   Host 1234 with a sub address of 0001 would = 3106 + 8 + 01234 + 0001

   There would be a hostname of 801234 with the possibility of a password.

The change was decided after examination of CCITT X.121 1984 recommendation
pertaining to PNIC's.  While it would be desirable to have 6 digits of host
number for expansion, Dennis feels that conforming with X.121 has greater
significance and that by the time Tymnet is ready to reach 100,000 hosts
the addressing standard used would most likely be E.164.

     Sorry for the last minute change, I was rushed to meet your March 24th
deadline and had not had quite enough time to get all comments IRT this
numbering plan change.  If you have any question please call.

- Jeff -
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Wed, 23 Apr 86 18:16:31 UT
From: ENS.J/OLIVETO@Ontyme.Tymnet 
Date: 23 APR 86 10:07:36 
To: NTD.WRS@Ontyme.Tymnet 
Cc: NTD.WRS@Ontyme.Tymnet, ENS.D/LANE@Ontyme.Tymnet,
	ENS.J/OLIVETO@Ontyme.Tymnet 
Message-id: I55045@Ontyme.Tymnet 
Subject: "Bill; Dan Lane from my group had"... 

Bill;

   Dan Lane from my group had an idea that you may be interested in viewing.
It is a possible meathod of speeding up the time from circuit request to 
circuit connect.  Dan terms it as Tymnet III.  

- Jeff -

-----------------------------------------------------------------------

General Description:

During a Tymnet logon, some high percentage of attempts in which the
username has been entered correctly will also have the password correct.
So, as soon as the sup receives the valid Tymnet username it could assume
the password will be correct and begin processing the path through the
network.

Implementation Ideas:

It would be possible for the sup to store the "hashed value" of the
password, and after receiving a username from a Consat, compute the
needle, and send the needle to the destination interface and the hashed
password to the Origination interface.  As the user is entering his 
password, the circuit would be built through the network.  The origination
interface could verify the password, and if valid, immediately send
a "call connected" or "host not available" message to the user.  If the 
password is never entered correctly, the Consat would have to Zap the
already built circuit.    This would be especially advantageous in 
networks where there are many usernames with many access positions.
It would also mean that no password is going through the
                                                                Page  2

network - only a hashed password.  

This implementation would depend on what percentage of correct usernames
entered are followed by a correct password.  The CCITT is in the process
of standarizing performance indicators of a network.  One of the possible
performance criteria will be call set up time.

The big disadvantage to this method is if the password is never entered 
correctly, network resources would be wasted setting up and clearing
the circuit.

I have not thought extensively about this idea, but thought that I would
throw it out for consideration.

Dan
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Wed, 30 Apr 86 21:05:30 UT
From: NTD.W/WANG@Ontyme.Tymnet 
Date: 30 APR 86 13:59:12 
To: NTD.WRS@Ontyme.Tymnet 
Message-id: I59206@Ontyme.Tymnet 
Subject: "TO Jeff Oliveto FROM Weyyi Wang"... 

	TO   :  Jeff Oliveto
	FROM :  Weyyi Wang
	CC   :  David Chang	Dennis Ernst	Bill Euske
		Jill Neidrauer	Mike Rude	Bill Soley
		Gazel Tan

	Subj.   Response to the suggestions of hostnames GED

	   After discussion with D. Ernst and W. Soley, here are the
	changes we made on hostnames GED:

	Item #1:
	   The DNICs (home networks DNICs or BOC DNICs) will be stored in
	the disk. Instead of using macros, the SUP reads DNICs from the
	disk during initialization. In addition, the networks operators
	can add/delete, thru Probe commands, the DNIC entries to support 
	the dynamic changes of X121 addressing.

	Item #2:
	   The DNICs is not going to be included in the hostname for
	home networking. For example, the hostname in the login string
	ENS:-31069012345 will be 9012345. The same implementation scheme
	will apply to the new proposal you made on home networking. For
	example, in the string of ENS:-31068012347777, the hostname will
	be 801234.

	   If an user logins to the network with username beginning with
	9, the same network security protection is applied as before, i.e.,
	the user will get a prompt for a password. On the other hand, if
	the user uses hostname -31069XXXXXX instead of host # to designate
	the destination, the security will be based upon the username, not 
	the hostname. No passwords are associated with the hostnames at this
	point. This should not have the impact on the networks access control.

	   Other than the changes listed above, I do not see any
	discrepancy in your memo. Please let me know if you have more
	suggestions.
    