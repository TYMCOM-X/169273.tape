Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Thu, 27 Mar 86 6:03:33 UT
From: NTD.R/TENG@Ontyme.Tymnet 
Date: 26 MAR 86 17:57:42 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I40410@Ontyme.Tymnet 
Subject: Proxy capability without passwords 

 
                           M E M O R A N D U M
 
 
Date:     March 26, 1986
 
To:       Bill Soley
 
From:     Richard Teng
 
Subject:  Proxy capability without passwords
 
---------------------------------------------------------------------------
 
Bill,
 
If we use your scheme of having a proxy bit in each access profile, we
might not need proxy passwords at all.
 
Suppose a person wanted to use vap V1 as a proxy host to log into hosts H1,
H2, or H3.  He would have to update his access profiles to look like this:
 
 
            | T A P |                    |
            | T U R |                    |
            | Y X O |   ORIGINATIONS     |  DESTINATIONS
            |     X |                    |
            |     Y |                    |
            |-------|--------------------|-----------------------
profile 1   | 1 0 0 |   . . .            |  V1, . . .
            |-------|--------------------|-----------------------
profile 2   | 0 0 1 |   V1               |  H1, H2, H3
            |-------|--------------------|-----------------------
profile 3   | . . . |   . . .            |  . . .
            |-------|--------------------|-----------------------
  . . .     |       |                    |
 
 
The Sup would allow the user to log into vap V1 as usual.  When V1 tries to
build a circuit to (say) H1, it sends the Sup a circuit request and
indicates that the circuit type is "proxy".  It does not send a password at
all (or it sends the null password).  By checking the MUD, the Sup verifies
that the user has given V1 proxy license to log into H1.  Consequently it
can accept the request without worrying about a password at all.
 
In order to misuse proxy license, a person would have to create an imposter
host with host number V1.  He could then log into any host allowed by the
MUD without having to receive proxy passwords from the Sup, or using proxy
passwords collected previously.  This scheme is therefore slightly less
secure than one in which proxy passwords are a function of the host
numbers.  However, it is probably better than the case where proxy
passwords can be stolen and used from any host.
 
This scheme easily applies to PVCs and eliminates the need for cleartext
passwords in memory.  (Also, the PVC would not have to carry on any dialogue
with the Sup in order to get a proxy password.)  I'm not sure how often
proxy capability could be used instead of aux circuits, however.
 
Please let me know if you see any gaping holes in this suggestion.
 
 
                                            Rich
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 27 Mar 86 11:45:04 PST 
To: NTD.R/TENG@Ontyme.Tymnet 
Subject: Re: Proxy capability without passwords 
In-reply-to: I40410@Ontyme.Tymnet of 26 MAR 86 17:57:42

My initial reaction is that we already have this implemented.  It seems
that our current AUX-CIRCUIT scheme is exactly as you proposed.  I think
it is good to be abl to restrict the capability by using the access profile,
but I think we want something more than AUX-CIRCUIT type functionality.
At the least, if we use a simple non-signature type proxy token, that is
one which is the same for all hosts, then the cryptography problem becomes
trivial and we have, combined with the access profile, a reasonable
compromise.  Before we give up, however, we need to be sure there is no
sufficiently efficient algorithm for creating the required signatures.
I need to get back in touch with Al on that, he is supposed to be thinking
about the problem.  He has said hes quite sure its possible, but his
concern is (1) does a small proxy have enough data to be secure?, (2) can
we find an algorithm to generate private keys fast enough to put on the
supervisor?

I suspect that we won't find a suitable algorithm at this time.  I thus
suggest that we make some compromise, but make sure that the hooks are
in the code, and more importantly in the protocol, to allow a smooth and
graceful transition to a full signature.

-Bill
