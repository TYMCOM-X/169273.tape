Received: from EMSTUMS.Ontyme.Tymnet by B39.Tymnet; Tue, 8 Jul 86 22:29:12 UT
From: OTSUPA.ONTYME@Ontyme.Tymnet 
Date: 08 JUL 86 21:21:05 
To: TUMS.SUP@Ontyme.Tymnet 
Message-id: A02933@Ontyme.Tymnet 
Subject: OnTyme Version 22 

                      APPLIED COMMUNICATIONS SYSTEMS
 
                           M E M O R A N D U M
 
 
                                                      []  ISG     
 
DATE>      08 JUL 86  13:47

TO>        OnTyme Account Supervisors

COPIES>    

FROM>      Bill Bartlett
           OnTyme Marketing Support Manager


SUBJECT>   OnTyme Version 22


-----------------------------------------------------------------------


    Applied Communications Systems (ACS) is pleased to announce that
    version 22 of OnTyme will be deployed for customers beginning in
    August!  The following features and enhancements have been added.
 
    SUBJECT LINE.  On a :SEND or :REPLY the system will prompt for
    subject.  The subject text supplied by the sender will appear in
    the message header and all the message lists (:IN, :OUT, old,
    long, short, etc.).
 
    X.ONTYME.  This feature is an interface with which applications,
    PC "front ends", or intelligent terminal procedures may interact
    with OnTyme in a consistent, reliable, binary, machine-oriented
    format.
    
    PATHWAY.  This feature allows the user to log into any network
    host during an OnTyme session, capture data from the target host
    into the OnTyme workspace, transfer data between OnTyme and the
    network host, and execute commands on the host. The target host
    may reside in either the same network as OnTyme or in another
    network accessible through a gateway.
 
    XMODEM.  This feature is used for binary file transfer using the
    XMODEM protocol.
 
    SYSTEM GENERATED MESSAGES.  Version 22 of OnTyme will put a
    message in the :IN list of the sender regarding invalid recipients
    so they will no longer have to look through their :OUT (OLD) lists.
 
    RESERVED CHARACTERS. To facilitate future enhancements, six
    characters (!,@,#,$,%,&) will no longer be allowed in file names.
    These will be used later as 'wildcard' characters for displaying
    information with the :LIST command.
 
    :REPLY (cc)(msg#).  This command will send the contents of the
    workspace to the original recipients of msg#.
 
                                                                Page  2

    COMMAND SCANNER. The OnTyme code has been improved and made more
    efficient.  Many system messages have been grammatically improved
    and made more consistent.
 
    Version 22 is available on the Marketing test host so that you can
    become familiar with the new product features. To request a
    temporary id on the test host, send a request to ACS.ONTYME.
 
    More complete documentation is being written and included in user
    manuals, technical manuals, and *** files in OnTyme as
    appropriate.  Definitions of the enhancements in version 22 of
    OnTyme may be found in the public file *** ONTYME.NEWS. Please
    distribute this announcement within your organization.  
 
 
    Version 22 WARNING!
    -------------------
    All 'intelligent front end' processes should be adequately tested
    for compatibility with version 22 of OnTyme before August 1.  
 
    ACS will provide technical assistance to complete this test.  To
    provide the longest possible lead time for any necessary code
    conversions, please schedule these tests as soon as possible.  Ids
    on the Marketing test host are now available upon request from
    ACS.ONTYME. Please keep ACS.ONTYME informed of the results.
 
    There are two enhancements in version 22 of OnTyme that have the
    POTENTIAL of disrupting 'intelligent front end' processes such as
    OnTyme :EXEC procedures, PC based packages, and intelligent
    terminal procedures that match on the text of OnTyme messages or
    the format of the :SEND, :REPLY, :IN and :OUT commands.  
    
    First, the text of many of the more obscure (not frequently
    encountered) system and error messages has been revised for
    grammatical improvement and consistencey. The most common messages
    such as ACCEPTED and ALL MESSAGES READ have not been changed.  You
    may obtain a list of the text changes by sending a request to
    ACS.ONTYME.  In the event that an incompatibility is found between
    a front-end process and version 22, ACS will urge that the
    X.ONTYME protocol be adopted in the front-end to preclude any
    future problems.  Use of X.ONTYME will fix the problem once and
    for all.  Please :READ *** ONTYME22.XONTYME for further
    information about this protocol.
    
    Second, each OnTyme session will default to having subject line
    enabled.  Each :SEND or :REPLY command will be followed by a
    prompt for subject.  The format of the :IN and :OUT lists will
    change to show a subject line for each message.  The subject line
    feature may be disabled at the session level providing a very
    simple solution to any compatibility problems caused by subject
    line.  Please :READ *** ONTYME22.SUBJLINE for further information
    about the subject line feature.
 
    Please direct any questions about this announcement to ACS.ONTYME.
 
    Thank you.
    ACS.ONTYME
Received: from EMSBUB.Ontyme.Tymnet by B39.Tymnet; Wed, 9 Jul 86 17:06:03 UT
From: BUB.G/CONE@Ontyme.Tymnet 
Date: 09 JUL 86 08:12:04 
To: BUB.B/SOLEY@Ontyme.Tymnet (BUB.WRS@Ontyme.Tymnet) 
Message-id: Y00574@Ontyme.Tymnet 
Subject: OnTyme Version 22 

                      APPLIED COMMUNICATIONS SYSTEMS
 
                           M E M O R A N D U M
 
 
                                                      []  ISG     
 
DATE>      08 JUL 86  13:47
 
TO>        OnTyme Account Supervisors
 
COPIES>    
 
FROM>      Bill Bartlett
           OnTyme Marketing Support Manager
 
 
SUBJECT>   OnTyme Version 22
 
 
-----------------------------------------------------------------------
 
 
    Applied Communications Systems (ACS) is pleased to announce that
    version 22 of OnTyme will be deployed for customers beginning in
    August!  The following features and enhancements have been added.
 
    SUBJECT LINE.  On a :SEND or :REPLY the system will prompt for
    subject.  The subject text supplied by the sender will appear in
    the message header and all the message lists (:IN, :OUT, old,
    long, short, etc.).
 
    X.ONTYME.  This feature is an interface with which applications,
    PC "front ends", or intelligent terminal procedures may interact
    with OnTyme in a consistent, reliable, binary, machine-oriented
    format.
    
    PATHWAY.  This feature allows the user to log into any network
    host during an OnTyme session, capture data from the target host
    into the OnTyme workspace, transfer data between OnTyme and the
    network host, and execute commands on the host. The target host
    may reside in either the same network as OnTyme or in another
    network accessible through a gateway.
 
    XMODEM.  This feature is used for binary file transfer using the
    XMODEM protocol.
 
    SYSTEM GENERATED MESSAGES.  Version 22 of OnTyme will put a
    message in the :IN list of the sender regarding invalid recipients
    so they will no longer have to look through their :OUT (OLD) lists.
 
    RESERVED CHARACTERS. To facilitate future enhancements, six
    characters (!,@,#,$,%,&) will no longer be allowed in file names.
    These will be used later as 'wildcard' characters for displaying
    information with the :LIST command.
 
    :REPLY (cc)(msg#).  This command will send the contents of the
    workspace to the original recipients of msg#.
 
                                                                Page  2
 
    COMMAND SCANNER. The OnTyme code has been improved and made more
    efficient.  Many system messages have been grammatically improved
    and made more consistent.
 
    Version 22 is available on the Marketing test host so that you can
    become familiar with the new product features. To request a
    temporary id on the test host, send a request to ACS.ONTYME.
 
    More complete documentation is being written and included in user
    manuals, technical manuals, and *** files in OnTyme as
    appropriate.  Definitions of the enhancements in version 22 of
    OnTyme may be found in the public file *** ONTYME.NEWS. Please
    distribute this announcement within your organization.  
 
 
    Version 22 WARNING!
    -------------------
    All 'intelligent front end' processes should be adequately tested
    for compatibility with version 22 of OnTyme before August 1.  
 
    ACS will provide technical assistance to complete this test.  To
    provide the longest possible lead time for any necessary code
    conversions, please schedule these tests as soon as possible.  Ids
    on the Marketing test host are now available upon request from
    ACS.ONTYME. Please keep ACS.ONTYME informed of the results.
 
    There are two enhancements in version 22 of OnTyme that have the
    POTENTIAL of disrupting 'intelligent front end' processes such as
    OnTyme :EXEC procedures, PC based packages, and intelligent
    terminal procedures that match on the text of OnTyme messages or
    the format of the :SEND, :REPLY, :IN and :OUT commands.  
    
    First, the text of many of the more obscure (not frequently
    encountered) system and error messages has been revised for
    grammatical improvement and consistencey. The most common messages
    such as ACCEPTED and ALL MESSAGES READ have not been changed.  You
    may obtain a list of the text changes by sending a request to
    ACS.ONTYME.  In the event that an incompatibility is found between
    a front-end process and version 22, ACS will urge that the
    X.ONTYME protocol be adopted in the front-end to preclude any
    future problems.  Use of X.ONTYME will fix the problem once and
    for all.  Please :READ *** ONTYME22.XONTYME for further
    information about this protocol.
    
    Second, each OnTyme session will default to having subject line
    enabled.  Each :SEND or :REPLY command will be followed by a
    prompt for subject.  The format of the :IN and :OUT lists will
    change to show a subject line for each message.  The subject line
    feature may be disabled at the session level providing a very
    simple solution to any compatibility problems caused by subject
    line.  Please :READ *** ONTYME22.SUBJLINE for further information
    about the subject line feature.
 
    Please direct any questions about this announcement to ACS.ONTYME.
 
    Thank you.
    ACS.ONTYME
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 15 Jul 86 23:00:16 UT
From: ACS.G/EVANS@Ontyme.Tymnet 
Date: 15 JUL 86 15:43:47 
To: NTD.WRS@Ontyme.Tymnet 
Cc: NTD.WRS@Ontyme.Tymnet, ACS.ONTYME@Ontyme.Tymnet, ACS.G/EVANS@Ontyme.Tymnet,
	ACS.G/EVANS@Ontyme.Tymnet 
Message-id: A06784@Ontyme.Tymnet 
Subject: Your Msg# I94157 to ACS.ONTYME, request for test ID 

 
 
 
To:  William Soley
From: Glenn Evans
Subject: Your Msg# I94157 to ACS.ONTYME, request for test ID
 
 
Bill,
I have set up an ID on Host 2859 for you. It is a level 8 with
netlog, ID, KEY as follows:
 
      EMSBETA
      SUPZ.WRS
      HELLO HELLO
 
Please log in and change your key. Let me know if there are any
problems. List *** ONTYME22.ALL for files about version 22.
 
The documentation of system messages is hard-copy only so give
me your mailing address and I will send it to you. I will also
send you the documentation on the X.400-based machine interface to
Ontyme (X.ONTYME).
 
Call or OnTyme if you have any questions.
 
Cheers,
Glenn
Received: from F26.Tymnet by B39.Tymnet; Thu, 5 Nov 87 6:32:16 PST
Return-path: <GBRADING@F26.Tymnet> 
From: GBRADING@F26.Tymnet 
Date: Thu, 5 Nov 87 10:37:31 
To: postmaster 
Subject: Hi! 

I would like to have my Ontyme connected to TUMS if it's possible.
My username is GBRADING and my ontyme name is MDICUST.G/Brading.
My Ontyme system is EMSMDI if you are interested. 
My username exists on system 26 and 31!
My ontyme keytyme key is "GARGLE/BLASTER".
I hope it's possible to arrange. I HATE ontyme....
Regards
Gunnar Brading at televerket in Sweden.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 5 Nov 87 9:54:09 PST 
To: <GBRADING@F26.Tymnet> 
Subject: Re: Hi! 
In-reply-to: your message of Thu, 5 Nov 87 10:37:31

Gunnar, I've added your Ontyme name to the polling list bust I must mention
a few caveats:

1) TUMS is supported only in my spare time, so problems are not always
addressed very quickly.

2) TUMS logs into your ONTYME ID about 40 times per day depending on the
system load - if there is no mail, the session lasts only a few seconds,
but you'd be billed for the minimum connect time.  I don't know what the
rate structure is, but this could be expensive!

3) Your ONTYME password is in a file and thus readable by a number of
people who have the necessary license - hopefully none of your other
accounts has the same password.

Thank you for your interest in TUMS and please let me know if you have any
suggestions or problems - I'll do my best to respond as quickly as I can.

-Bill


PS - please let me know if you find this acceptable,
in particular, item 2 above.  Thanks.
Received: from F26.Tymnet by B39.Tymnet; Thu, 5 Nov 87 11:17:14 PST
Return-path: <GBRADING@F26.Tymnet> 
From: GBRADING@F26.Tymnet 
Date: Thu, 5 Nov 87 20:07:13 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: Hi! 
In-reply-to: your message of Thu, 5 Nov 87 9:54:09 PST

Thanks!!

There is no problems for me in any of the items.
I do not use the password in any other accounts so that's no problem
but thanks for reminding me!

I think TUMS seems to MUCH better than Ontyme and also seems to be
very nice to use.
I will tell you if I thinks of something to improve when I get's
more used to it.

Gunnar
  