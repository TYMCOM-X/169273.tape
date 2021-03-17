Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 13 May 86 21:05:50 UT
From: NTD.W/EUSKE@Ontyme.Tymnet 
Date: 13 MAY 86 13:32:50 
To: NTD.WRS@Ontyme.Tymnet 
Cc: NTD.WRS@Ontyme.Tymnet, NTD.W/EUSKE@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.A/FENN@Ontyme.Tymnet, TYM.SU/BLU@Ontyme.Tymnet,
	TYM.RPR@Ontyme.Tymnet, NETS.D/PURPURA@Ontyme.Tymnet,
	ENS.NS@Ontyme.Tymnet 
Message-id: I66340@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Supporting SWB Numbering via Host Names - 5/2 Meeting 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      13 MAY 86  13:32                  Network Technology Development

TO>        Su Heisler

COPIES>    Bill Soley
           Mike Rude
           Al Fenn
           Rob Ritchie
           Dennis Purpura
           Neil SUllivan

FROM>      Bill Euske


SUBJECT>   Supporting SWB Numbering via Host Names - 5/2 Meeting


-----------------------------------------------------------------------


From a Technical perspective, Bill Soley's meeting with Jerry
Talken was very productive.  It did backfire in one respect,
however, we have a lot more work than we expected to fulfill
Southwestern Bell's needs (or desires).

With respect to supporting their needs long term via Host Names,
Bill Soley is writing up his understanding.  It will be available
shortly.  Until then, a gut feel estimate of about two man years
of work is a good round number to implement what they want.
Given this, we will have to re-evaluate Host Names and whether to
delay it by including SWB's needs first pass.

I do not know where Jerry got his information that we WILL NOT
support their needs, unless he also sees how far we (and anyone
else) would have to go and made an assumption.

With respect to supporting SIX DIGIT NUMBERING in the current
X implementation, I am forwarding that question for complete
clarification to Mike Rude.


                                                                Page  2


TO:  ALL CONCERNED
FR:  SU HEISLER
RE:  SWBT NUMBERING PLAN

APPARENTLY OUR MEETING OF MAY 2 BACKFIRED.  JERRY TALKEN JUST SENT AROUND
A LETTER SAYING WE DO NOT AND WILL NOT SUPPORT THEIR NUMBERING REQUESTS.
THIS IS NOT TRUE.  IN AN EFFORT TO STOP THIS RUMOR, I WOULD LIKE TO CONFIRM
OUR POSITION:  THE SIX DIGIT NUMBERING SWBT NEEDS IS AVAILABLE IN 
OUR CURRENT RELEASE OF X.25.  THE ENTIRE NUMBERING PLAN IS WHAT WE CALL
HOST NAMES AND IS APPARENTLY A RELATIVELY SHORT DEVELOPMENT EFFORT.

I WOULD LIKE TO SEND A LETTER TO THE MANAGERS TWO LEVELS ABOVE JERRY, 
BUT I NEED TO BE ABSOLUTELY SURE OF OU FACTS FIRST.

HELP

THANKS
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 13 May 86 17:33:44 PDT 
To: NTD.W/EUSKE@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Re: South West Bell 
In-reply-to: I66341@Ontyme.Tymnet of 13 MAY 86 13:33:30

Talken is probably not that far off.  From their point of view, having the
numbering plan supported by X.25 only is probably only slightly better than
not at all.  On the other hand, he received everything short of a promise
that we would implement the required functionality.  I believe this is an
attempt to "make waves".

I will try to expedite my reply to SWBT.  On the one hand it is more important
now than ever to be accurate.  It is also more important now than ever to be
quick.  I shall attempt to find a reasonable compromise between the two.  I
will try to distribute a draft for Tymnet people only by Monday 5/19.

-Bill
Received: from C36.Tymnet by B39.Tymnet; Mon, 19 May 86 11:26:06 PDT
Received: from tymix.Tymnet by C36.Tymnet; Mon, 19 May 86 11:24:30 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA00827;
	Mon, 19 May 86 05:11:53 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA18859; Mon, 19 May 86 05:11:47 pdt
Return-path: <wrs@eunix.Tymnet> 
From: wrs@eunix (Bill Soley) 
Date: Mon, 19 May 86 05:11:47 pdt 
To: ntd.w/euske@ontyme.Tymnet 
Cc: wrs@39.Tymnet 
Message-id: <8605191211.AA18859@eunix.Tymnet> 
Keywords: SWBT 
Subject: SWB meeting stuff 

Here is a memo saying what happened at the meeting.  It also has some
solutions thrown in, but most just minutes.  It is still a draft, please
let me know your comments and then I'll send it off to the dist list.
The other stuff is not going as well as I hoped, I'll have a draft of
it, though, this afternoon.  I have a meeting with Dennis Ernst at noon
to talk some things over so I can make some updates after that.  For
the most part it has come down to doing a partial design of subnets.
Since that's clearly a big job, the document will read "requires further
study" in that section, but I want to try and get as much written down
in the time I have as possible.  Oh well, here is the draft...

-Bill





                                        _M_E_M_O


            from: Bill Soley                     to: Bill Euske

            date: May 19, 1986               copies: Al Fenn
                                                     Su Heisler
         subject: May 2 Meeting at SWB               Dennis Purpura
                                                     Rob Ritchie
         status:: *D*R*A*F*T*                        Mike Rude
                                                     Neil Sullivan

          _________________________________________________________________


          This memo describes the significant points covered in the meeting
          I had with Jerry Talken, et. al. on May 2 at South Western Bell
          in Saint Louis.  The purpose of the meeting was to cover the
          technical requirements of the SWBT numbering plan and their rela-
          tionship to the host names project.  The meeting tended to focus
          around a draft technical memo from myself entitled _E_f_f_e_c_t _o_f _t_h_e
          _S_o_u_t_h _W_e_s_t_e_r_n _B_e_l_l _N_u_m_b_e_r_i_n_g _P_l_a_n _o_n _S_y_m_b_o_l_i_c _A_d_d_r_e_s_s_i_n_g _M_e_c_h_a_n_-
          _i_s_m_s _i_n _T_y_m_n_e_t.  The reader should be familiar with this techni-
          cal memo in order to understand the following points.

          1. suport of 28 LATAs on one supervisor

          It was stated as a requirement that SWB be able to support all of
          their 28 LATAs as virtual subnets under one supervisor.  With the
          current method of implementing virtual subnets, this would re-
          quire a minimum of 57 (2*28+1) access profile entries per user
          having a presubscribed IC.  Only 29 entries would be required for
          a non-presubscribed user.  I indicated that the current limita-
          tion is 9 entries per user and that I would look into the effort
          required to increase this to 64.

          SWB indicated concern over the administrative task of validating
          users having this many access profiles.  I suggested a front end
          program on a PC, or a customization of NETVAL to automatically
          generate the access profile.

          2. multiple RPOA facility required, eventually

          SWB indicated that the ability for a caller to specify more than
          one RPOA at call request would probably be required at some point
          in the future.  I said we did not have any work scheduled to sup-
          port this, but it should be easy provided that the regulatory
          restrictions are fairly reasonable.

          3. inhouse traffic to bypass IC

          SWB requires that specified SWB users be able to place inter-LATA
          calls to a SWB facility without transiting an IC.  I said this
          would require one of three procedures:
             o+ setup special inter-LATA gateways as an internal IC
             o+ specify the actual host number, not the X.121 address








                                        - 2 -



             o+ setup a special NTN for such calls
          None of these solutions appeared to be acceptable to SWB.  I said
          it might be possible to restructure the way the access profiles
          are setup in order to allow this to work more smoothly.

          Since the meeting, I have revised my plan for access profiles to
          allow these calls to be placed in the normal fashion using host
          names.  The privilege to place an inter-LATA call which does not
          transit an IC would be based on a combination of the users and
          hosts access profile.

          4. block public access calls lacking a NUI

          Since the NUI is expected to be a calling-card number, a call
          from a public access port without such should be blocked.  As
          long as the interfaces are configured to provide different origin
          host numbers for public access, this is no problem.

          5. clear call if no RPOA and preselected carrier unavailable

          Since IC preselect is implemented as priority targeting in the
          user profile, a side effect is that an inter-LATA call would be
          placed using the topologically nearest IC in the event the
          preselected IC is unavailable.  SWB pointed out that this would
          be a violation of regulations.  I said I saw no obvious solution
          to this problem other than to modify the supervisor code to deal
          with it specially.  I also indicated that it would not be a prob-
          lem if an RPOA was specified explicitly.

          6. intra-LATA calls transiting an IC

          SWB stated it would probably be required at some future time to
          be able to block an intra-LATA call from specifying an RPOA.  I
          said the only obvious way to implement this would be to change
          the host name behavoir to route RPOA or transit calls to an SWB
          destination based on the transit DNIC, called DNIC and the first
          six digits of the called NTN.  This would require a change in the
          planned implementation of host names.

          7. Tymsat call request syntax

          SWB described the desired syntax of a call request from a Tymsat
          port as follows:

              [3000+314+225+0881:][3106-][1+3000+]716+285+5110[;password]

          The symbols [] enclose optional input.  The symbol + is to im-
          prove readability.  The symbol - is literal.  The 3000 represents
          SWB DNIC.  The 3106 represents an IC DNIC specified as an RPOA.
          The 314+225+0881 is the caller's calling card.  The 716+285+5110
          is the called NTN.  The password specified matches the caller's
          calling card.  If the calling card is omited, the call is billed
          based on the originating port.  Calls without a calling card from
          public access ports must be blocked.








                                        - 3 -



          I indicated that the login format we were planning to accept for
          the purpose of X.121 calls is as follows:

              3000+314+225+0881:-[3106-]3000+716+285+5110[;password]

          It was agreed that requiring the user to enter the calling card
          number on each call was unreasonable.  The possibility of using a
          special flag in place of the calling card to indicate the call
          was to be billed based on the physical port would probably be ac-
          ceptable.  The flag discussed was *, resulting in:

              *:-[3106-]3000+716+285+5110

          There were still complaints about the required dash following the
          colon, and about the DNIC being required.  I indicated that these
          optional fields were such a deviation from our standard login
          syntax that it would require a custom modification to the inter-
          face (Tymsat).  If that were done, virtually any type of login
          syntax could be implemented.

          8. accounting

          The type of information available from RAM tapes was briefly dis-
          cussed.  I pointed out that for some types of calls, they may
          have to bill from the UUN as opposed to the NTN.  Likewise, other
          calls may require billing on host and port number.  I pointed out
          that since the UUN was a large number which they were at liberty
          to assign, they could do so in such a way as to minimize the
          postprocessing required.

          9. calling card authentication

          It was briefly mentioned that it would become a requirement at
          some point to be able to query a central NUI data base for cal-
          ling card authentication.  This would presumably have to be han-
          dled by the supervisor.  I indicated this would require custom
          development.

          10. calling address presence at X.75 gateways

          This topic was not directly discussed, but is noted here because
          it should be followed up on to make sure it is not a problem.  A
          mechanism or convention for translating the originating host and
          port into an NTN to be placed in the calling DTE address at the
          X.75 gateway needs to be established.

          Summary

          In summary, I feel the meeting was very successful in that it
          made both parties aware of technical issues that need attention.
          As a result of my enhanced understanding of SWB requirements and
          BOC requirements in general, I am now in a much better position
          to recommend implementation strategies for host names and closed
          subnets.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 20 May 86 16:06:20 UT
From: NTD.W/WANG@Ontyme.Tymnet 
Date: 20 MAY 86 08:21:58 
To: NTD.WRS@Ontyme.Tymnet 
Message-id: I70025@Ontyme.Tymnet 
Keywords: SWBT 
Subject: 64 access profile entries (64 APEs) 

	To  :  William R. Soley
	From:  Weyyi Wang
	CC  :  David Chang	Jill Neidrauer
	Subject: 64 access profile entries (64 APEs)


	    The new MUD format allows maximum of 256 bytes for each access
	profile entry.  In each access profile entry, the maximum size of
	origin lists is also 256 bytes.  Each access profile entry needs the
	minimum of 12 bytes (1 byte for APE size, 1 byte for origins-list size,
	2 bytes for access control, 4 bytes for class # and 4 bytes for group
	number).  Taking into account of 64 APEs, that means the minimum size
	for each user will be,
	
		64 * 12 + 22 (user header) + 2 (end mark) = 792 bytes

	    Let's assume that the SWB uses block size of 2K bytes, just
	like currently public net's MUD. That means at most 2 entries can
	occupy a MUD block.  The current implementation scheme does not allow
	splitting of MUD entries over MUD blocks. Also, the number of user
	entries is limited to 2 in an overflow block (the overflow blocks
	have the same size as the normal blocks).

	    I hope this can give you some idea of the impact on MUD database.
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 20 May 86 12:38:02 PDT 
To: NTD.W/WANG@Ontyme.Tymnet 
Cc: jmarcin@36, dchang@39 
Keywords: SWBT 
Subject: Re: 64 access profile entries (64 APEs) 
In-reply-to: I70025@Ontyme.Tymnet of 20 MAY 86 08:21:58

Thank you for your answer on that.  It occured to me the possibility of
a kludge (always possible).  Since there would only be a few different
access profiles (one for each IC preselect) we could assemble them into
the SUP memory somewhere and just put pointers to them in the users MUD
entries.  This would make them very short.  Any thoughts?  -Bill

We could even key it off of something like a special class.  Lets say
class 99:

	orig	dest
	c 99	g 6

would really mean to point to the 6th "canned" access profile in the SUP
memory.  -Bill
Received: from C57.Tymnet by B39.Tymnet; Tue, 20 May 86 19:27:01 PDT
Received: from tymix.Tymnet by C57.Tymnet; Tue, 20 May 86 19:24:45 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA08075;
	Tue, 20 May 86 18:53:59 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA01662; Tue, 20 May 86 18:54:25 pdt
Return-path: <wrs@eunix.Tymnet> 
From: wrs@eunix (Bill Soley) 
Date: Tue, 20 May 86 18:54:25 pdt 
To: ens.ns@ontyme.Tymnet, nss.m/cady@ontyme.Tymnet, ntd.a/fenn@ontyme.Tymnet,
	ntd.bjc@ontyme.Tymnet, ntd.j/hainaut@ontyme.Tymnet,
	ntd.m/rude@ontyme.Tymnet, tym.d/purpura@ontyme.Tymnet,
	tym.rpr@ontyme.Tymnet, tym.s/heisler@ontyme.Tymnet, wrs@b39.Tymnet 
Message-id: <8605210154.AA01662@eunix.Tymnet> 
Keywords: SWBT 
Subject: "MEMO from: Bill Soley to: Bill"... 




                                        MEMO


            from: Bill Soley                     to: Bill Euske

            date: May 20, 1986               copies: Mike Cady
                                                     B.J. Chang
         subject: Southwestern Bell                  Al Fenn
                  Status                             John Hainaut
                                                     Su Heisler
                                                     Dennis Purpura
                                                     Rob Ritchie
                                                     Mike Rude
                                                     Neil Sullivan

          _________________________________________________________________


          Attached is a memo describing the meeting I had with Southwestern
          Bell on May 2, in Saint Louis.

          I am currently working on a recommendation describing the various
          alternatives for meeting SWBT requirements.  The document will
          present several possible implementations and indicate the advan-
          tages and disadvantages of each.  Both long term and short term
          solutions will be presented.  The author's opinion of the most
          desirable solutions will be mentioned.

          Preparation of this document requires careful consideration of
          the interference between the various features.  Although we nor-
          mprovide most of the features required, they have never been
          provided in the combination required by SWB.  Some of them appear
          to be mutually exculsive.  In view of this complexity, I expect
          the document to be ready by June 30.

          It will certainly recommend the use of host names as the solution
          to the numbering plan requirements.  It will most likely recom-
          mend an implementation of subnets as a solution to the LATA con-
          finement requirements.  I have recently received a copy of Techn-
          ical Advisory 301 (about 650 pages) which I will use along with
          the RFP and notes from the meeting as a basis for the recommenda-
          tions.

          Some tentative decisions have been made before I bacame involved.
          I plan to work with Neil Sullivan to make sure none of the rea-
          soning that took place early on is lost in the process.  There
          will no doubt be some questions along the way which I will chan-
          nel through Neil.



                                        MEMO


            from: Bill Soley                     to: Bill Euske

            date: May 19, 1986               copies: Mike Cady
                                                     B.J. Chang
         subject: May 2 Meeting at SWBT              Al Fenn
                  (trip report)                      John Hainaut
                                                     Su Heisler
                                                     Dennis Purpura
                                                     Rob Ritchie
                                                     Mike Rude
                                                     Neil Sullivan

          _________________________________________________________________


          This memo describes the significant points covered in the meeting
          I attended on May 2 at Southwestern Bell in Saint Louis.  The
          following individuals were present at the meeting:

          Su Heisler       MDNSC   SWBT Account Manager
          Jerry Talken     SWBT    Staff Manager - Switched Services
                                   Technical Planning
          Wayne Sluhan     SWBT    Manager - Packet Switching
          Bill Soley       MDNSC   Staff Scientist - Network Systems
                                   and Architecture
          Tommy Williams   SWBT    District Manager - Data Switching


          The purpose of the meeting was to cover the technical require-
          ments of the SWBT numbering plan and their relationship to the
          host names project.  The meeting tended to focus around a draft
          technical memo from myself entitled Effect of the Southwestern
          Bell Numbering Plan on Symbolic Addressing Mechanisms in Tymnet.
          The reader should be familiar with this technical memo in order
          to understand the following points.

          1. support of 28 LATAs on one supervisor

          It was stated as a requirement that SWBT be able to support all
          of their 28 LATAs as virtual subnets under one supervisor.  With
          the current method of implementing virtual subnets, this would
          require a minimum of 57 (2*28+1) access profile entries per user
          having a presubscribed IC.  Only 29 entries would be required for
          a non-presubscribed user.  I indicated that the current limita-
          tion is 9 entries per user and that I would look into the effort
          required to increase this to 64.

          SWBT indicated concern over the administrative task of validating
          users having this many access profiles.  I suggested a front end
          program on a PC, or a customization of NETVAL to automatically
          generate the access profile.

          2. multiple RPOA facility required, eventually

                                        - 2 -



          SWBT indicated that the ability for a caller to specify more than
          one RPOA at call request would probably be required at some point
          in the future.  I said we would be supporting this in the future,
          but did not believe the work was currently scheduled.

          3. inhouse traffic to bypass IC

          SWBT requires that specified SWBT users be able to place inter-
          LATA calls to a SWBT facility without transiting an IC.  I said
          this would require one of three procedures:
             + setup special inter-LATA gateways as an internal IC
             + specify the actual host number, not the X.121 address
             + setup a special NTN for such calls
          None of these solutions appeared to be acceptable to SWBT.  I
          said it might be possible to restructure the way the access pro-
          files are setup in order to allow this to work more smoothly.

          Since the meeting, I have revised my plan for access profiles to
          allow these calls to be placed in the normal fashion using host
          names.  The privilege to place an inter-LATA call which does not
          transit an IC would be based on a combination of the user's and
          host's access profile.  This would require a simple modification
          to the planned interpretation of host name access profiles.

          4. block public access calls lacking a NUI

          Since the NUI is expected to be a calling-card number, a call
          from a public access port without such should be blocked.  As
          long as the interfaces are configured to provide different origin
          host numbers for public access, this will work under host names
          as planned.

          5. clear call if no RPOA and preselected carrier unavailable

          Since IC preselect is implemented as priority targeting in the
          user profile, a side effect is that an inter-LATA call would be
          placed using the topologically nearest IC in the event the
          preselected IC is unavailable.  SWBT pointed out that this would
          be a violation of regulations.  I said I saw no obvious solution
          to this problem other than to modify the supervisor code to deal
          with it specially.  It is not a problem if an RPOA is specified
          explicitly.

          6. intra-LATA calls transiting an IC

          SWBT stated it would probably be required at some future time to
          be able to block an intra-LATA call from specifying an RPOA.  I
          said the only obvious way to implement this would be to change
          the host name behavior to route RPOA or transit calls to an SWBT
          destination based on the transit DNIC, called DNIC and the first
          six digits of the called NTN.  This would require a change in the
          planned implementation of host names.

          7. Tymsat call request syntax

                                        - 3 -



          SWBT described the desired syntax of a call request from a Tymsat
          port as follows:

              [3000+314+225+0881:][3106-][1+3000+]703+285+5110[;password]

          The symbols [] enclose optional input.  The symbol + is to im-
          prove readability.  The symbol - is literal.  The 3000 represents
          SWBT DNIC.  The 3106 represents an IC DNIC specified as an RPOA.
          The 314+225+0881 is the caller's calling card.  The 703+285+5110
          is the called NTN.  The password specified matches the caller's
          calling card.  If the calling card is omited, the call is billed
          based on the originating port.  Calls without a calling card from
          public access ports must be blocked.

          I indicated that the login format we were planning to accept for
          the purpose of X.121 calls is as follows:

              3000+314+225+0881:[-3106]-3000+703+285+5110[;password]

          It was agreed that requiring the user to enter the calling card
          number on each call was unreasonable.  The possibility of using a
          special flag in place of the calling card to indicate the call
          was to be billed based on the physical port would probably be ac-
          ceptable.  The flag discussed was *, resulting in:

              *:[-3106]-3000+703+285+5110

          There were still complaints about the required dash following the
          colon, and about the DNIC being required.  I indicated that these
          optional fields were such a deviation from our standard login
          syntax that it would require a custom modification to the inter-
          face (Tymsat).  If that were done, virtually any type of login
          syntax could be implemented.  The customization could be done in
          the supervisor with some loss of flexability.

          8. accounting

          We briefly discussed the information available for billing.  I
          said we did not plan to put the host name in the accounting, but
          only the UUN of th host name.  Calls which did not have a NUI
          (calling card) which were to be billed to the owner of the port
          would not see the calling NTN in accounting, only the originating
          host and port number.  I pointed out that since the UUN was a
          large number which they were at liberty to assign, they could do
          so in such a way as to minimize the postprocessing required.

          9. calling card authentication

          It was briefly mentioned that it would become a requirement at
          some point to be able to query a central data base for calling
          card authentication.  This would presumably have to be handled by
          the supervisor.  I indicated this would require significant cus-
          tom development.

                                        - 4 -



          10. calling address presence at X.75 gateways

          This topic was not directly discussed, but is noted here because
          it should be followed up on to make sure it is not a problem.  A
          mechanism or convention for translating the originating host and
          port into an NTN to be placed in the calling DTE address at the
          X.75 gateway needs to be established.  The originating interface
          should ultimately perform this function based on data in its con-
          figuration tables.

          Summary

          In summary, I feel the meeting was very successful in that it
          made both parties aware of technical issues that need attention.
          As a result of my enhanced understanding of SWBT requirements and
          BOC requirements in general, I am now in a much better position
          to recommend implementation strategies for host names and closed
          subnets.
Received: from C36.Tymnet by B39.Tymnet; Tue, 27 May 86 8:29:00 PDT
Return-path: <JMARCIN@C36.Tymnet> 
From: JMARCIN@C36.Tymnet 
Date: Tue, 27 May 86 7:52:22 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Cc: wwang@39, dchang@39 
Keywords: SWBT 
Subject: Re: 64 access profile entries 
In-reply-to: your message of Tue, 20 May 86 1:19:12 PDT


Bill:

If Netval imposes any restrictions on the number of access profile entries
or sizes other than the inherent restrictions set by size fields, they
will be able to be changed modified the Netval parameter in a Pascal souce
file, re-compiling and re-loading...a simple procedure.


Jill
From: William R. Soley <WRS@C39.Tymnet> 
Date: Thu, 5 Jun 86 18:15:30 PDT 
To: Bill Euske <NTD.W/Euske@Ontyme.Tymnet> 
Keywords: SWBT 
Subject: SWBT response 

The Roman-numbered items below refer to the 5 major points in the attachment
to Don Mitchell's letter to Clint DeGabrielle dated May 20, 1986.  All items
are covered by the planned implementation of Host Names combined with an
extension of access profile size to 64 entries in addition to some minor
customization.  The paper being prepared for the end of June will present
details of the plans and some alternatives.  For the purpose of this estimate,
the minimum alternatives providing the required service were chosen.

I. SWBT Numbering Plan assignment: requires customization, 4 man-months
    * to provide originating DTE from Tymsat, and port addressing by assigned
	DTE on calls to Tymsat, 3 man-months, 4 elapsed months (Async Unit)
    * if the customer needs to utilize interfaces or VAPs other than the
	Consat, X.25, and X.75 then these must be considered seprately
    * to customize supervisor parsing of DNIC/NTN to infer the LATA from the
	first six digits of the NTN: requires customization, 1 man-month,
	1 elapsed month (Sup Unit)

II. IC access on equal basis: requires host-names

III. Inter-LATA call recognition: requires host-names

IV. Roamer service: requires either 64 profile entries or subnets

V. Abbreviated addressing: requires host-names, and customization, 1 man-month
    * sup must recognize abbreviated addresses: requires customization, 1
	man-month, 1 elapsed month (Sup Unit)

------------------------------------------------------------------------

Host-Names: 
Sup unit estimate is 10 man-months as currently specified, without allowing
symbolic addresses in access profile entries.  Expect elapsed time of 9 months
with current schedules, two programmers.  Concurrently NETVAL development is
expected to take 6 elapsed months and 9 man-months.

64 profile entries per user:
This project should not require any development, other than the new MUD
format.  Some time should be allowed for testing since we've never tried
more than 9 due to old NETVAL restrictions: requires testing, 1 man-month,
1 elapsed month.

Subnets:
To implement subnets would require about 4 man-months, and 4 elapsed months
of sup unit development which must follow host-names sequentially.  This
project is not required.

------------------------------------------------------------------------


Total man-months:

host-names	19
64 entries	 1
customization	 5
		--
total (MM)	25


Total elapsed months: (critical path)

		SUP	NETVAL	ASYNC

host-names	10	6
64 entries	 1
Sup cust.	 1
Consat cust.			1
		---------------------
total (CP)	12
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Fri, 6 Jun 86 17:04:54 UT
From: NTD.W/EUSKE@Ontyme.Tymnet 
Date: 06 JUN 86 09:11:39 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I80382@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Estimates for meeting SWB's Numbering Requirements 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      06 JUN 86  09:05                  Network Technology Development

TO>        Neil Sullivan

COPIES>    Clint DeGabrielle
           Al Fenn
           Bill Soley
           Mike Rude
           Ken Holcomb

FROM>      Bill Euske


SUBJECT>   Estimates for meeting SWB's Numbering Requirements


-----------------------------------------------------------------------


Since my visit to SWB on 4/25, and Bill Soley's subsequent
meeting with Jerry Talken on 5/2, we have been working on a plan
to address SWB's Numbering requirements in our numbering and
addressing research known as "Host Names".  Bill's 5/2 visit was
eye opening, and we gained a greater appreciation for the
complexity of SWB's requirements.   

As you know, we had committed to produce a report containing
alternative implementations and costs for later this month.  The
letter you brought to my attention Tuesday requesting a
commitment by 6/9 will not stop this work, but clearly we must
deliver to you something, call it a "best guess", if Tymnet is to
respond in the time frame request.  


Man-month effort:               Critical Path for Delivery ARO:
------------------              -----------------------------------
host-names      19                             SUP   NETVAL   ASYNC
64 entries       1              host-names      10      6
customization    5              64 entries       1
                ==              Sup cust.        1
total (MM)      25              Consat cust.                    1
                                               ====================
                                                12

Please note that the above delivery is ARO and is subject to priorization
and associated staffing constraints.

A detailed analysis of these estimates is attached.
                                                               Page  2

The Roman-numbered items below refer to the 5 major points in the attachment
to Don Mitchell's letter to Clint DeGabrielle dated May 20, 1986.  All items
are covered by the planned implementation of Host Names combined with an
extension of access profile size to 64 entries in addition to some minor
customization.  The paper being prepared for the end of June will present
details of the plans and some alternatives.  For the purpose of this estimate,
the minimum alternatives providing the required service were chosen.

I. SWBT Numbering Plan assignment: requires customization, 4 man-months
    * to provide originating DTE from Tymsat, and port addressing by assigned
        DTE on calls to Tymsat, 3 man-months, 4 elapsed months (Async Unit)
    * if the customer needs to utilize interfaces or VAPs other than the
        Consat, X.25, and X.75 then these must be considered separately
    * to customize supervisor parsing of DNIC/NTN to infer the LATA from the
        first six digits of the NTN: requires customization, 1 man-month,
        1 elapsed month (Sup Unit)

II. IC access on equal basis: requires host-names

III. Inter-LATA call recognition: requires host-names

IV. Roamer service: requires either 64 profile entries or subnets

V. Abbreviated addressing: requires host-names, and customization, 1 man-month
    * sup must recognize abbreviated addresses: requires customization, 1
        man-month, 1 elapsed month (Sup Unit)

------------------------------------------------------------------------

Host-Names: 
Sup unit estimate is 10 man-months as currently specified, without allowing
symbolic addresses in access profile entries.  Expect elapsed time of 9 months
with current schedules, two programmers.  Concurrently NETVAL development is
expected to take 6 elapsed months and 9 man-months.

64 profile entries per user:
This project should not require any development, other than the new MUD
format.  Some time should be allowed for testing since we've never tried
more than 9 due to old NETVAL restrictions: requires testing, 1 man-month,
1 elapsed month.

Subnets:
To implement subnets would require about 4 man-months, and 4 elapsed months
of sup unit development which must follow host-names sequentially.  This
project is not required.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Mon, 9 Jun 86 20:09:31 UT
From: NTD.W/EUSKE@Ontyme.Tymnet 
Date: 09 JUN 86 12:29:55 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I81342@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Numbering Plan Commitment to SWB 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      09 JUN 86  12:24                  Network Technology Development

TO>        Su Heisler

COPIES>    Dennis Purpura
           Forrest Hicks
           Rob Ritchie
           Al Fenn
           Neil Sullivan
           Bill Soley

FROM>      Bill Euske


SUBJECT>   Numbering Plan Commitment to SWB


-----------------------------------------------------------------------


Thank you for copying me on your letter to Jeff Stern.  As I understand
it, Neil Sullivan was assigned by Clint to follow up on the 5/20 letter
to Clint by SWB requesting a response by 6/9.   Neil brought this to my
attention last Tuesday (6/3).  

Some good news is that despite not having enought time to complete our
analysis, NTD provided Neil development man months and delivery ARO
estimates on Friday (6/6).
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Wed, 30 Jul 86 16:07:11 PDT
From: NTD.P/DRISCOLL@Ontyme.Tymnet 
Date: 30 JUL 86 15:50:33 
To: NTD.W/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.W/SOLEY@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.D/ERNST@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet,
	NTD.P/DRISCOLL@Ontyme.Tymnet 
Message-id: I04667@Ontyme.Tymnet 
Subject: "Tymnet/Network Technology Development"... 

 
 
 
 
 
 
	              Tymnet/Network Technology Development
	              Network Interface Development Section
	                                 
	                                 
	                       M E M O R A N D U M
	                                 
	                                 
	                                                        [] ISG
	
	Date:     July 30, 1986
	
	To:       W. Soley
	
	Copies:   D. Ernst
	          A. Newman
	          M. Rude
	
	From:     Pat Driscoll
	
	Subject:  Southwest Bell X.25 Development
	
	
	-----------------------------------------------------------------
	
	Some of the functionality  required to support SWB  addressing is
	already  incorporated  into XCOM 4.0,  which just  went into beta
	test.   Assuming that  you follow  the approach  outlined in your
	draft  document,   dated  July  15,   1986,  Recommendations  for
	Implementation of  the Numbering Plan  for the  Southwestern Bell
	Public  Packet  Switched  Network,   the  additional  development
	required is fairly small, from one to two person-months.
	
	There  are  no  conflicts  with  existing  X.25  projects,  as no
	projects are  currently  scheduled.   The  target  date  for beta
	release,  May 1,  1987,  is consistent with the X.25 product plan
	dated June 23, 1986.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 6 Aug 86 15:52:04 PDT 
To: Sue Heisler <Tym.SU/BLU@Ontyme.Tymnet> 
Cc: Bill Euske <NTD.W/Euske@Ontyme.Tymnet> 
Subject: conference call with SWBT 

This is a summary of the conference call which took place today with SWBT.
On the line were:

	Jerry Talken	(SWBT)
	Su Heisler	(Tymnet)
	Bill Soley	(Tymnet)

Most of the time was spent answering Jerry's questions about the simulation
in Appendix A of "Recommendations for Implementation of the Numbering Plan
for the Southwestern Bell Public Packet Switched Network".  The following
points were brought up and are worthy of note:

1.  Intra-LATA calls for which the user has specified an RPOA should be
    blocked or completed based on the originating LATA.  I believe this
    change to be minor and plan to rerun the simulation when the written
    comments have been received (see #4).

2.  Use of MENU SERVER for a speed-call-like feature may be too expensive
    without call redirect and disk based directory.  Suggest use of seperate
    user names of the form:
	user*mnemonic
    to be homed to different hosts.

3.  1+8+ (Telex) and 1+9+ (ISDN/Voice) will be treated as if a DNIC, thus
    causing routing based on the first four digits following the "1".

4.  Jerry will mail written comments to Su on Friday.

5.  Revised simulation will include a description of the default call
    type to make the exceptions shown in parenthesis more meaningful.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 29 Aug 86 14:34:13 PDT 
To: Su Heisler <TYM.SU/BLU@Ontyme> 
Cc: Bill Euske <NTD.W/EUSKE@Ontyme>, Mike Cady <NETS.M/CADY@Ontyme>, Dennis
	Purpura <NETS.D/PURPURA@Ontyme>, Rob Ritchie <TYM.RPR@Ontyme> 
Subject: Response to SWBT Comments 

The following is a response to comments prepared by Jerry Talken dated
August 8, 1986.  I will cover the points in sequence.  Please let me
know of any further concerns or questions.  I expect a meeting should be
setup when we feel we are ready to finalize some of the issues which
have been left to "general customization".  The most significant of
which is probably the user login syntax.  Thank you -Bill

1.  I believe a GED has been sent to Su Heisler for distribution to SWBT.

2.  All of the addressing schemes mentioned in the comments under (2.) are
within the constraints of the proposed implementation.  Since there is some
uncertainty as to exactly what will be desired in the future in view of
evolving standards, I would suggest a supervisor based implementation of
the abbreviated address scheme with any exceptions that develop being
handled at the interface.  This will present a uniform login procedure
across all types of interfaces.

3.  Any abbreviation scheme which is uniform across all interfaces is
probably best implemented in the supervisor.  Any scheme which is unique
to some subset of all interfaces should be implemented in the interface.
The objective is to avoid interface-specific code in the supervisor.  The
impact on SWBT is as follows:
 *  All users are subject to schemes implemented on the supervisor, while
    schemes in the interface may select specific users.
 *  Implementation on the supervisor requires modification of less code.

4.  The example shown in the comments is not correct.  There is no colon
after the options field.  To be more precise, here is the exact syntax
in Backus-Naur Form (BNF):

<login-string>	::=  <option-field> <user-field> <host-field> <password-field>
<option-field>	::=  <NULL>  |  <options>
<options>	::=  <control-char>  |  <control-char> <options>
<user-field>	::=  <user-name>  |  <X.121-spec>
<user-name>	::=  <alpha-char>  |  <alpha-char> <non-term-chars>
<host-field>	::=  <NULL>  |  ":" <host-spec>
<host-spec>	::=  <host-number>  |  <host-name>  |  <X.121-spec>
<host-number>	::=  "*"  |  <num-chars>  |  <num-chars> "." <num-chars>
<host-name>	::=  <alpha-char>  |  <alpha-char> <non-term-chars>
<X.121-spec>	::=  "-" <DNIC> <NTN>  |  "-" <RPOA> "-" <DNIC> <NTN>
<DNIC>		::=  <num-char> <num-char> <num-char> <num-char>
<NTN>		::=  <num-chars>
<RPOA>		::=  <num-char> <num-char> <num-char> <num-char>

<non-term-chars>::=  <non-term-char>  |  <non-term-char> <non-term-chars>
<num-chars>	::=  <num-char>  |  <num-char> <num-chars>

<control-char>	::=  <CONTROL-A>-<CONTROL-Z>
<non-term-char>	::=  <alpha-num-char>  |  "*"  |  "."  |  "-"  |  "#"
<alpha-num-char>::=  <alpha-char>  |  <num-char>
<alpha-char>	::=  "A"-"Z"
<num-char>	::=  "0"-"9"

This represents the standard syntax.  SWBT customization would alter the
syntax slightly.  Differences include:
 *  Implied DNIC with a leading 1 to flag a DNIC follows.
 *  Extraneous "-" "(" ")" "+" " " ignored.

5.  Options are simply control characters.  There is no change proposed
from the existing options (e.g. ^I=interactive, ^R=flow-control, etc.).

6.  The ability to assign arbitrary addresses to ports has existed only
for incoming calls.  The address currently generated by Tymnet for an
outgoing call is a function of the host and port numbers.  To allow an
arbitrary address for outgoing calls, it will be necessary to assemble
a table of addresses into each interface in addition to the central
validation in the Master User Directory (MUD).  Perhaps the trade-off
involved here requires more discussion.

7.  This falls out of the way a host name is validated.

8.  If only one PIN per account were to be allowed, there would be no
restriction on the character set used in the PIN.  If more than one PIN
per account is required, the PIN would be limited to the characters which
are legal in a user name.  This includes all alphanumeric characters and
some punctuation.

9.  Development cost and time for Subnet Confinement Facility (SCF) and
a PC based validation aid are not included in the $150,000 figure.

It is correct the two user access profile entries are required for each
LATA as a result of IC preselect.  It is also correct that the maximum
is 58 entries.  Section 4.5 and Table 4 state that 57 is the typical
size for a normal user, not the maximum.  We will be able to handle at
least 64 access profile entries which would allow for an expansion of
3 more LATAs.

There is no way planned to force a user to specify an RPOA.  One way
which comes to mind is to define a host, NEED-RPOA, and give it a
message, "Error, no carrier preselected, please specify RPOA.".  The
NEED-RPOA host would then be specified as if it were the preselected
carrier's gateway in the user access profile.  It would also appear
in all gateway group definitions.  Note that the host would not need
to actually exist, but only have a number assigned.

10.  Yes, since a separate host name is validated for each LATA for
each RPOA, the intra-LATA-with-RPOA blocking capability can be enabled
or disabled based on the LATA or the carrier, or both.

11.  I believe this is what is intended, but I will have to check with
the X.25/X.75 interface people.  Should X.25 users have the abbreviated
form or the canonical form?
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Wed, 15 Oct 86 19:08:40 UT
From: QSATS.S/CHIANG@Ontyme.Tymnet 
Date: 15 OCT 86 11:30:41 
To: NTD.W/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.D/CHANG@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.W/SOLEY@Ontyme.Tymnet, TYM.SU/BLU@Ontyme.Tymnet,
	NSCHQ.LAW@Ontyme.Tymnet, NTD.W/EUSKE@Ontyme.Tymnet,
	QSATS.S/CHIANG@Ontyme.Tymnet 
Message-id: M33854@Ontyme.Tymnet 
Subject: RESPONSE TO MEMO #I40521 - SWB'S ACCOUNTING QUESTIONS 

                            M E M O R A N D U M

                                                        [] TYMNET - NETWORK
                                                           SYSTEMS SUPPORT
DATE>      15 OCT 86  11:26

TO>        DAVE CHANG

COPIES>    SUE HEISLER
           JACK ORLOVE
           BILL SOLELY
           GAZEL TAN
           BILL EUSKE

FROM>      SHIRLEY CHIANG


SUBJECT>   RESPONSE TO MEMO #I40521 - SWB'S ACCOUNTING QUESTIONS


-----------------------------------------------------------------------


Dave,

    I've contacted Reggie Dampier a few times since you brought to
my attention that there was some problems in SWB's accounting data.
Reggie is the head of MIS department at SWB and is the person responsible
over the entire data base generation based on RAM's accounting tapes.
He and I have been working very closely in the past year to straighten
out problems in that area.  However, according to him, there has not been
any complaints about the problems stated in your meeting mimutes that
he is aware of, nor does he detect any thing inappropriate with the
origination/destination physical port reporting in the accounting data.

    As I had explained to Reggie and his organization earlier this year
in a two-day meeting with them at St. Louis,  the origination physical
port number is reported by Consat and 3270 TIF and each of these two ter-
minal interfaces has its own definition of so called "physical port number."
The destination physical port number is ONLY reported by Tymcom interface.
None of the other host interfaces such as 3270 HIF, X.25 or SNA HIF is
currently implemented to report this information.  Originally SWB had
designed their accounting data base keyed off by the session number
and the orig./dest. physical port number with the assumption that the
later would always be present in Tymnet's raw accounting file.
Well, it turned out to be a very wrong assumption since Tymnet's accounting
                                                                Page  2

document does not indicate to the reader that the physical port number
is only reported by very few interfaces and thereby should not be expected
on every session.

    Anyhow, SWB has since re-designed their applications with much better 
apprehension of Tymnet's accounting system.

    I've also talked to Sue Heisler to pursue the concern, she could not
recall who brought it to her and exactly what the concern was about.
She suggested that we close the loop.  Should there be problem again,
she will immediately inform me.

    Let me know if there is anything else I can be of help.


Regards,

Shirley Chiang
HQ S/W Tech. Services
Received: from C26.Tymnet by B39.Tymnet; Thu, 31 Jul 86 12:19:41 PDT
Received: from EMSNTD.Ontyme.Tymnet by C26.Tymnet; Thu, 31 Jul 86 19:06:00 UT
Return-path: <NTD.W/EUSKE@Ontyme.Tymnet> 
From: NTD.W/EUSKE@Ontyme.Tymnet 
Date: 31 JUL 86 11:49:38 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I05032@Ontyme.Tymnet 
Subject: Feasibility of changing SWB Numbering Plan ARO Time 

                         M E M O R A N D U M

                                          [] T Y M N E T, I N C
DATE>      31 JUL 86  11:46                  Network Technology Development

TO>        Mike Cady

COPIES>    Al Fenn      Dan Lasater
           Mike Rude    Pat Driscoll
           Ken Holcomb  Bill Soley
           Gazel Tan    Dave Chang

FROM>      Bill Euske


SUBJECT>   Feasibility of changing SWB Numbering Plan ARO Time


-----------------------------------------------------------------------


After analysis, this is our opinion on requests to change
delivery of the SWB Numbering Plan to 6 or 12 calendar months:

For the 6 month case, I regret to inform you that this is impossible.
The project is too complex.

For the 12 month case, we are confused.  We originally quoted to
Neil Sullivan a critical path time of 12 months and 25 man months
of development.  This is possible IF priorities are established
to support this.  We have not done a specific analysis on the
changes to current JPPC priorities for this case, but a flavor
can be gained from the next case.  

Since six months was impossible, we also determined the minimum
time required to complete this project.  This turned out to be
nine (9) months.  To achieve this, priorities in the following
product areas will have to be changed:

  Supervisor  -PIR 1899, New Slave Protocols must be delayed by
               three months.

  Netval      -PIR 1541, Selection Messages must be delayed until
               after completion of SWB.
              -PIR 2170, Force Password Change must be suspended 
                                                                Page  2

               until after completion of SWB.

  Consat      -PIR 2043, 1984 PAD Parameters must be delayed until
               after completion of SWB.
              -PIR 1525, XOM Inactivity Time must be delayed until
               after completion of SWB.

  X.25        -No change in priorities currently needed.

We feel that speeding up the project to 9 calendar months will
probably increase our cost by 3 man months, mostly in
administrative and planning costs.

Please note that these times are to Betatest and assume no major
deviation from Bill Soley's technical analysis: "Recommendations
for Implementation of the Numbering Plan for the Southwestern
Bell Public Packet Switched Network" dated 7/15/86.   This
document was hand delivered by Bill to Steve Schaff of SWB on
7/16/86 at their recent Bellcore consulting visit.  We have not
yet received any feedback on this analysis.  

I suspect that if all priorities for this are established, the
soonest we could change direction and start this project is 9/1/86.
This would produce a 6/1/87 Betatest start date.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Mon, 8 Sep 86 23:18:44 UT
From: NTD.A/MORRISS@Ontyme.Tymnet 
Date: 08 SEP 86 15:03:44 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.B/SOLEY@Ontyme.Tymnet, NTD.R/RAFFO@Ontyme.Tymnet,
	NTD.J/NEIDRAUER@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.D/ERNST@Ontyme.Tymnet,
	NTD.C/BLACK@Ontyme.Tymnet, NTD.K/HOLCOMB@Ontyme.Tymnet,
	NTD.B/EUSKE@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet 
Message-id: I22732@Ontyme.Tymnet 
Keywords: SWBT 
Subject: SWB Numbering Plan - PIR 2214 
SWB-NumberingPlan(PIR2214): 

 
                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      08 SEP 86  14:51

TO>        Attendees

COPIES>    Bill Euske
           Ken Holcomb

FROM>      Allyn Morriss


SUBJECT>   SWB - Numbering Plan (PIR 2214)


-----------------------------------------------------------------------


These are the minutes from the meeting held last Friday in Building C
regarding SWB's numbering plan.
 
In attendance were:
 
     Bill Soley (X6210) - Staff Scientist
     Jill Neidrauer (X6330) - Network Validations systems
     Romolo Raffo (X6338) - Network Tools
     David Chang (X7374) - Network Supervisor Unit
     Dennis Ernst (X6259) - Staff Scientist
     Adele Newman (X7410) - X.25 Unit
     Chris Black (X8808) - Async Unit (ABSENT)
 
The objectives for the meeting were:
 
1. Have all parties meet to re-assess their portions of the work
   required to implement SWB's new Numbering Plan. This would
   involve identifying existing projects and initiation of any
 
2. Having identified the projects, provide man-power estimates and
   to have each Unit propose a schedule which insures earliest completion
   while also pointing out the impact that such a schedule would have
   on other projects within their organizations.
 
3. I will be coordinating this effort and will work to get the work
   clearly defined as to content and schedule along with known impact.
 
4. Bill Soley's document 'Recommendations for Implemention of the
   Numbering Plan' dated 7/15/86 was sited as the requirements
   response to SWB's needs document dated 3/18/86 and PIR 2214.
 
The actions that resulted were:
 
1. Bill Soley will re-convene with SWB on 9/17 to review his document
   for changes in the specification. Bill will update and distribute
                                                                Page  2

   a revised copy upon his return.
 
2. Allyn will pull togther all relevant correspondence and produce a
   project schedule from the database to review individually with the
   managers of each Unit. This to be completed early this week.
 
3. Each Unit will define any new projects required to do the work that
   are not yet present in NTD's projects database. All projects are
   to be reviewed as to estimate and earliest scheduling. Other
   projects are to be looked at for potential impact resulting from
   SWB projects.
 
4. Romolo will work closely with Jill to understand the status of the
   work in Validations since he will become acting manager after
   Jill leaves on 9/18.
 
5. Allyn is to supply Adele with copy of Bill's document to allow her
   to re-assess the work in the X.25/X.121 Unit. Allyn will meet with
   Adele to assist her with what's required of her, where needed.
   Allyn will also provide David with a copy of the PIR so that he
   can initiate a project in the SUP Unit to cover his work on this
   SWB implementation. Allyn will make extra copies for those that
   need Bill's document.
 
6. Since Chris was unable to attend meeting, Allyn will meet with
   Chris and explain what is required of him.
 
7. Allyn will communicate with Marketing the proposed development
   plan and ask for acceptance. Marketing will no doubt be required to
   prioritize any new projects and possibly re-prioritize existing
   ones.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
From: David Chang <DChang@B39.Tymnet> 
Date: Mon, 15 Sep 86 13:52:13 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: SWB Meeting 

Bill,

I tried to catch you before you left, but missed.  I would like some
idea what the topic is about in this meeting.  Could you give me a 
short explanation of the problem remaining.  Thanks.

David

Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Thu, 18 Sep 86 2:06:58 UT
From: NTD.A/MORRISS@Ontyme.Tymnet 
Date: 17 SEP 86 18:26:43 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.A/MORRISS@Ontyme.Tymnet, NTD.B/EUSKE@Ontyme.Tymnet,
	NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.R/RAFFO@Ontyme.Tymnet, NTD.J/NEIDRAUER@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.C/BLACK@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet 
Message-id: I27551@Ontyme.Tymnet 
Subject: SWB Numbering Plan - Preliminary Schedule 

SUBJ: SWB Numbering Plan - Preliminary Schedule


 
                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      17 SEP 86  18:12

TO>        Distribution

COPIES>    Ken Holcomb
           Bill Euske
           Mike Rude

FROM>      Allyn Morriss


SUBJECT>   SWB Numbering Plan - Preliminary Schedule


-----------------------------------------------------------------------


After some review of projects relating to Southwestern Bell's PIR 2214
describing a new numbering plan, I have the following to report:
 
1. The acceptable time-frame to implement is a 12-month period starting
   October 1, 1986.
 
2. The ASYNC and X.25 development required will be discussed later
   today and be available tomorrow in an updated memo of this same kind.
 
3. The three (3) other areas affected are NETVAL, Node Code and Supervisor.
 
4. There does not appear to be any dependencies upon the work of each of
   these Units until integration testing.
 
5. Bill Soley left for St. Louis today for a meeting with SWB to gain
   acceptance to Bill's document on recommended development implementation.
   Any significant revisions will be cause to re-think the proposed
   schedule and alter accordingly.
 
 
Attached is the list of projects identified to meet SWB's requirements.
I have included any projects that are either in progress or that
the SWB projects depend Any information I do not have as yet is clearly
shown. Any project impact resulting from the proposed schedule is noted.
 
Please review and give me your comments. When all information is known and
Bill and David Chang return, we will meet again to discuss.
 
Allyn Morriss (X6851)
Manager, Program Management
NTD.A/MORRISS
                                                                Page  2

                       Southwestern Bell X.121 Numbering Plan
                            Preliminary Project Schedule
                                      9/16/86
 
 
PIR-Project  Project Name                Unit      Start     End      Note
-------------------------------------------------------------------------------
 
2214-1628   Study/Specs for SWB          Soley    4/1/86    7/15/86      1
 
1311-1290   New MUD Format               NETVAL   10/1/87   4/30/87      2
1553-1484   NETVAL mods - Host Names     NETVAL   4/10/87   8/27/87      2
 
1311-0081   New MUD Format               SUP      7/1/84   12/31/86      3
1553-0241   SUP mods - Host Names        SUP      4/1/85   12/31/86      3
2214-1714   X.121 Numbering Plan         SUP      9/15/86  12/31/86      3
 
 
1553-0582   Host Names Option            NODE                            4
 
2214-1534   X.121 Numbering Plan         X.25     4/1/87    6/30/87
 
2214-1713   X.121 Numbering Plan         ASYNC    5/1/87    7/31/87      5
 
2214-new    Integration Testing          ALL      9/1/86    10/31/86     6
 
NOTES
 
1. Completed recommendation document subject to SWB approval.
 
2. Does not include deployment into Public Net until after Host Names.
   Development of Host Names made to precede deployment of new MUD format.
   Project includes time to develop conversion from old to new format.
   Estimate new MUD format to be ready for SUP testing early 2/87.
 
3. Does not include time needed for integration testing.
 
4. The work required in the Node Code Unit will be in support of SUP
   changes and is not anticipated to be extensive. Assurance has been
   given that although some clarification is required, the work
   can be done within the 12-month period requested.
 
5. ASYNC work estimated for Release 8.00 of ConSat expected 7/87.
 
6. Estimated 2-month NTD testing at conclusion of NETVAL Host Names.
   Beta testing, thereafter.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Fri, 19 Sep 86 5:07:00 UT
From: NTD.A/MORRISS@Ontyme.Tymnet 
Date: 18 SEP 86 22:04:10 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.A/MORRISS@Ontyme.Tymnet, NTD.B/EUSKE@Ontyme.Tymnet,
	NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.R/RAFFO@Ontyme.Tymnet, NTD.J/NEIDRAUER@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.C/BLACK@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet 
Message-id: I28185@Ontyme.Tymnet 
Subject: SWB X.121 Numbering Plan - Preliminary Schedule 

SUBJ: SWB X.121 Numbering Plan - Preliminary Schedule


 
                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      18 SEP 86  22:02

TO>        Distribution List (See CC List)

COPIES>    Mike Rude
           Bill Euske
           Ken Holcomb

FROM>      Allyn Morriss


SUBJECT>   SWB X.121 Numbering Plan - Preliminary Schedule


-----------------------------------------------------------------------


I have made some slight changes to the SWB Numbering Plan schedule being
proposed for a 12-month development period. Note that Host Names
implemention for NETVAL is clearly the critical path leading to a 2-month
integration test period. All other work fits within the desired twelve months.
months. Planning for and providing the facilities needed is a critical issue
if we are to complete our end.
 
Please review and give me your initial reaction by tomorrow afternoon.
When Bill Soley and David Chang return, we will meet next week to discuss
any changes to the attached schedule.
 
Allyn Morriss (X6851)
Manager, Program Management
NTD.A/MORRISS
                                                                Page  2

                       Southwestern Bell X.121 Numbering Plan
                            Preliminary Project Schedule
                                      9/18/86
 
 
PIR-Project  Project Name                Unit      Start     End      Note
-------------------------------------------------------------------------------
 
2214-1628   Study/Specs for SWB          Soley    4/1/86    7/15/86      1
 
1311-1290   New MUD Format               NETVAL   10/1/86   4/30/87      2
1553-1484   NETVAL mods - Host Names     NETVAL   4/10/87   8/27/87      2
 
1311-0081   New MUD Format               SUP      7/1/84   12/31/86      3
1553-0241   SUP mods - Host Names        SUP      4/1/85   12/31/86      3
2214-1714   X.121 Numbering Plan         SUP      9/15/86  12/31/86      3
 
 
1553-0582   Host Names Option            NODE                            4
 
2214-1534   X.121 Numbering Plan         X.25     4/1/87    6/30/87
 
2214-1713   X.121 Numbering Plan         ASYNC    5/1/87    7/31/87      5
 
2214-new    Integration Testing          ALL      9/1/86    10/31/86     6
 
NOTES
 
1. Completed recommendation document subject to SWB approval.
 
2. Does not include deployment into Public Net until after Host Names.
   Development of Host Names made to precede deployment of new MUD format.
   Project includes time to develop conversion from old to new format.
   Estimate new MUD format to be ready for SUP testing early 2/87.
 
3. Does not include time needed for integration testing.
 
4. The work required in the Node Code Unit will be in support of SUP
   changes and is not anticipated to be extensive. Assurance has been
   given that although some clarification is required, the work
   can be done within the 12-month period requested.
 
5. ASYNC work estimated for Release 8.00 of ConSat expected 7/87.
 
6. Estimated 2-month NTD testing at conclusion of NETVAL Host Names.
   Beta testing, thereafter.
 
   This test period extends the develpment beyond the 12-month desired
   target to 13 before deployment for SWB testing.
From: David Chang <DChang@B39.Tymnet> 
Date: Tue, 23 Sep 86 15:13:06 PDT 
To: Bill Euske <BUB.W/Euske@EMSBUB.Ontyme.Tymnet>, Gazel Tan <GAZEL@B39> 
Cc: Bill Soley <WRS@B39.Tymnet> 
Subject: Report on SWB Meeting on 17SEP86 

TO:  Bill Euske and Gazel Tan
FR:  Bill Soley and David Chang
RE:  SWB Meeting on the Numbering Plan
DA:  Sepember 18, 1986
Distribution List:
	Allyn Morriss		Chris Black		Pat Driscoll
	Adele Newman		Dennis Ernst		Romolo Raffo
	Su Heisler		Neil Sullivan		Mike Cady
	Dennis Purpura		Rob Ritchie

=============================================================================
!!!  THIS IS FOR YOUR REVIEW BEFORE DISTRIBUTION TO VARIOUS PARTIES.   !!!


This is a report detailing the events and conclusions of the meeting with
SWB on September 17, 1986.  The objective of the meeting was to finalize
the requirements for the SWB Numbering Plan.  In particular, it was
unclear whether addressing by ports was necessary or addressing by hosts
was sufficient.  

1. The meeting was chaired by Jerry Talken of SWB.  There were several
SWB representatives.  TYMNET sales rep Su Heisler was also present.

2. Several problems were identified:

   a. SWB would like to somehow identify the physical port at the
origination and destination for each session.  Either an accounting log
containing the physical port numbers or a one-to-one correspondence
between NTN and physical port would be satisfactory.
COMMENT:  Currently, X.25 provides this information, but SWB cannot get
the information for the CONSAT.  The physical port numbers should be in
the accounting log already (see action items 1 and 2).  The capability of
assigning separate NTN for each physical port requires special
customization which is not anticipated at this time (need to supply the
NTN in the call request protocol).

   b. SWB would NOT like to reload a slot in order to add a new
customer.  Traditionally, the method of adding a host number to a slot
is by reconfiguring and reloading.  This means that current sessions
will have to be torn down, ie. disrupting customer service.
COMMENT:  X.25 interface has an operations monitor (XOM) which
allows a user to dynamically change certain parameters.  It is hoped
that the XOM facilitates the addition of a new host number (see action
item 3).  The CONSAT interface will need such a facility.

   c. SWB envisions the need for increasing the number of addressable
entities.
COMMENT:  TYMNET may need to support the 32-bit host numbers or SWB may
utilize a multi-network system.  This is for the future.  (see below)

   d. SWB indicated that our previous understanding was incorrect, ie.
an intra-LATA call should have the "best" path, even if it traverses
another LATA.  This means that the Subnet Confinement Facility (SCF)
is not a critical item.  However, SWB would eventually want it.
COMMENT:  This facility was a suggestion for future development.
However, Bill believes that this may be illegal and should be presented
to our legal department for an evaluation.  Perhaps, a statement from
SWB allowing intra-LATA calls through another LATA would absolve us of
any legal impropiety.

   e. SWB had concerns about collect calls, reverse charge, and sent
pay.
COMMENT:  No resolutions.  Problems to be addressed in the future.

   f. SWB would like the login syntax to look like this:
		(NUI;PWD:)1+IDN(:RPOA)
			  or NTN
in order to show the caller and callee.
COMMENT:  During this discussion, Bill indicated that our syntax has the
PWD at the end:
		(NUI:)1+IDN(:RPOA);PWD
		      or NTN
Probably the RPOA is also not in the right place, but the complete and
exact syntax is spelled out clearly in Bill's memo dated 29AUG86, which
is believed to be acceptable.  However, this syntax does not accomodate
"abbreviated addressing" (whereby the address is either NTN or
1+DNIC+NTN) nor "digit absorbing" (whereby "+,-,(,),blank" embedded in the
address are discarded), both of which are requested (or required?) by
SWB.

   g. SWB expects TYMNET to handle up to 15-digit addresses (prefix "1"
+ 14-digit IDN):
		 1 + 2XXX   \
		 1 + 3XXX    \
		 1 + 4XXX     \	Call to another network
		 1 + 5XXX     /
		 1 + 6XXX    /
		 1 + 7XXX   /
		 1 + 8XXX	Call to a telex network
		 1 + 9XXX	Call to ISDN
SWB also expects the following address syntax (where IDN = DNIC + NTN):
		X.75		14-digit IDN
		X.25		optional prefix of "1" + 14-digit IDN
	"dumb" terminals	{prefix + IDN} or NTN ("abbrev. addr")
COMMENT:  TYMNET needs to ensure that the interfaces can handle these
addresses, especially the prefix.  However, although its implementation
is not a prblem, it is not recommended (not conveyed to SWB at meeting)
because of the inconsistency between X.25 and CONSAT (ie. the address
"31069" is a DNIC+NTN at  X.25, but a NTN at the CONSAT).  This
situation may lead to user confusion because of the illusion of
"abbreviated addressing" at the X.25 interface.  This
"non-recommendation" was not conveyed to SWB because of their enthusiasm
for this addressing scheme.

   h. SWB expects to have shared DNIC (two networks with the same DNIC)
which is anticipated to be 3146.
COMMENT:  TYMNET needs to determine whether the entire NTN is necessary
for addressing or a portion (eg. 6 digits) is sufficient.  A scheme to
handle shared DNIC must be incorporated into the Numbering Plan.

   i. SWB expects to incorporate other interfaces into their Numbering
Plan.  These interfaces include 3270BSC, SDLC, X.PC, Menu Server, CMT,
and maybe MUX and UTS.  Perhaps all possible TYMNET interfaces would be
incorporated in the future.
COMMENT:  Originally, the expectation was that only the CONSAT and X.25
would be involved, so that customization in only those interfaces would
be feasible.  The problem is to provide a mechanism by which the calling
address is sent to the destination for call back.  TYMNET needs to come
up with a scheme to handle this need via modifications in just the Sup
or modifications in all the various interfaces.

   j. SWB would like in the long term to have the calling NTN in the
accounting data.
COMMENT:  Is this necessary?  Should we do it?  This is just a
convenient item for billing, since the physical port numbers are
available and adequate.  This item is left for future discussion.

3. In response to SWB's needs, Bill came up with three proposals:

	PROPOSAL A.  "Original Proposal"
	This is the original proposal as recommended by Bill in his
	draft dated July 15, 1986.  Basically, the NTN address would be 
	translated by the SUP to a host number and translated by the
	interface to a port number.  This double translation requires
	customization in at least the CONSAT.

	PROPOSAL B.  "Embedded Host Number"
	This proposal was concocted by Bill during the meeting to
	address the issue of special customization for each of the
	interfaces.  Bascially, since the host number field is 
	currently 14 usable bits (which mean 16,383 unique host
	numbers), the Sup could use the last 5 digits of the NTN
	(with the most significant digit being one of two possible
	values) to determine the host number (ie. the NTN contains the
	host number).  Consequently, the various interfaces would not
	have to be customized.

	PROPOSAL C.  "32-bit Hosts"
	This proposal was to address the long term requirement of
	sufficient host numbers to meet SWB needs.  The most that SWB
	would ever need is one NTN per host number, which means that
	TYMNET need to increase the host number field, probably to 32 
	bits eventually.  In this case, the NTN is the host number and
	it would be sparsely used within the entire range.

In order to evaluate the various proposals, Bill formulated the
following table:

		PROPOSAL A		PROPOSAL B 		PROPOSAL C

TERM		short-term		short-term 		long-term
[This entry is actually the development feasibility or lead time for
implementation.]
[short-term means within a year]

ADDRESS PORTS?	Yes			No * 			No *
[* = unless we have one host number for each port]
		
# NTN		16,383 * P		16,383	**		very large
[This entry is actually the # of addressable entities.]
[P = average # of ports per host]
[** = relatively small # of addressable entities unless no distinction
between host numbers and port numbers (ie. one host # for many ports) is
OK.  This restriction may be slightly alleviated by having the Sup
support 15-bit host numbers.]

# PORTS		16,383 * P		16,383 * P		very large
[This entry is the maximum number of physical ports allowed.]

RELOAD X.25?	No			No			No
RELOAD CONSAT?	Yes			Yes			Yes
[This entry is concerned with reloading if a parameter is changed]

SLOT DATA	NTN,host#,port#		host#			host#
[This entry indicates the type of parameters that may need dynamic
modifications.]

{RESTRICTED ASSIGNMENTS OF NTN}
		No			Yes			No
[This entry was not mentioned in the meeting.]

4. Conclusions:

   a. SWB was very anxious to get the show on the road, while keeping
with the advertised price of 12 months and $150,000.  Hence, SWB would
like to stick with Proposal A, although some of the SWB representatives
preferred Proposal B.  Perhaps, Proposal A is in the best interest of
SWB because of the following:  more addressable entities and less
restriction on NTN assignments.

   b. Proposal A, as with the other proposals, needs to address the two
main concerns:  shared DNIC and "other interfaces".  Bill will be
looking into how Proposal A should be modified to accomodate these
concerns.  It may be necssary to utilize certain elements of Proposal B
to handle "other interfaces".

   c. It was agreed that a "phased approach" is needed to satisfy all of
SWB's requirements:
		(1) Phase 1  short-term needs (Proposal A)
		(2) Phase 2  intermediate-term needs (see next subsection)
		(3) Phase 3  long-term needs (?)
SWB is probably expecting some kind of plan or proposal for these
phases, but not necessarily before the start of Phase 1.  For long-term
needs, 32-bit host numbers or multi-network (one net per LATA), or
some combination, may address the insufficient host numbers and/or
addressable entities, but a new naming convention may be necessary to
identify origination/destination ports at call setup.

   d. Some of the requirements for Phase 2 are prioritized:
		PC-based, user-friendly, customized validations facility
		Consat operations monitor (to change parameters)
		Support 3270 BSC
		Support X.PC	    (put NTN in calling addr field)
		Support menu server (put NTN in calling addr field)
		Support CMT	    (put NTN in calling addr field)
		Support SDLC
		Support MUX	    (put NTN in calling addr field)
		Subnet confinement facility (SCF)
		NUI facility for X.75 (pass NUI in call request packet)
		NUI facility 	      (pass NUI in call request packet)
		User friendly (speed) call (currently, called adr has
					    garbage)

5. Action items:

   1. Does the Sup know about the physical port of the originator? [David]
      ANSWER:  No, only the logical port number.  However, both the
origination and destination physical port numbers are reported to the
accounting log by the CONSAT.

   2. Why SWB accounting log not have the physical port number? [Su]

   3. Does the X.25 XOM allow addition of new host numbers? [Ernst,
Newman]

   4. When is the original proposal going to be modified to include
"shared DNIC" and "other interfaces"? [Bill]

   5. Does X.25 process NUI? [Ernst, Newman]

   6. Can intermediate nodes to accounting be turned on/off via PROBE?
[David]
      ANSWER:  No, but this can be implemented.
From: David Chang <DChang@B39.Tymnet> 
Date: Tue, 23 Sep 86 15:13:06 PDT 
To: Bill Euske <BUB.W/Euske@EMSBUB.Ontyme.Tymnet>, Gazel Tan <GAZEL@B39> 
Cc: Bill Soley <WRS@B39.Tymnet> 
Subject: Report on SWB Meeting on 17SEP86 
Resent-from: William R. Soley <WRS@B39.Tymnet> 
Resent-date: Fri, 10 Oct 86 13:46:46 PDT 
Resent-to: tym.su/blu@ontyme 

TO:  Bill Euske and Gazel Tan
FR:  Bill Soley and David Chang
RE:  SWB Meeting on the Numbering Plan
DA:  Sepember 18, 1986
Distribution List:
	Allyn Morriss		Chris Black		Pat Driscoll
	Adele Newman		Dennis Ernst		Romolo Raffo
	Su Heisler		Neil Sullivan		Mike Cady
	Dennis Purpura		Rob Ritchie

=============================================================================
!!!  THIS IS FOR YOUR REVIEW BEFORE DISTRIBUTION TO VARIOUS PARTIES.   !!!


This is a report detailing the events and conclusions of the meeting with
SWB on September 17, 1986.  The objective of the meeting was to finalize
the requirements for the SWB Numbering Plan.  In particular, it was
unclear whether addressing by ports was necessary or addressing by hosts
was sufficient.  

1. The meeting was chaired by Jerry Talken of SWB.  There were several
SWB representatives.  TYMNET sales rep Su Heisler was also present.

2. Several problems were identified:

   a. SWB would like to somehow identify the physical port at the
origination and destination for each session.  Either an accounting log
containing the physical port numbers or a one-to-one correspondence
between NTN and physical port would be satisfactory.
COMMENT:  Currently, X.25 provides this information, but SWB cannot get
the information for the CONSAT.  The physical port numbers should be in
the accounting log already (see action items 1 and 2).  The capability of
assigning separate NTN for each physical port requires special
customization which is not anticipated at this time (need to supply the
NTN in the call request protocol).

   b. SWB would NOT like to reload a slot in order to add a new
customer.  Traditionally, the method of adding a host number to a slot
is by reconfiguring and reloading.  This means that current sessions
will have to be torn down, ie. disrupting customer service.
COMMENT:  X.25 interface has an operations monitor (XOM) which
allows a user to dynamically change certain parameters.  It is hoped
that the XOM facilitates the addition of a new host number (see action
item 3).  The CONSAT interface will need such a facility.

   c. SWB envisions the need for increasing the number of addressable
entities.
COMMENT:  TYMNET may need to support the 32-bit host numbers or SWB may
utilize a multi-network system.  This is for the future.  (see below)

   d. SWB indicated that our previous understanding was incorrect, ie.
an intra-LATA call should have the "best" path, even if it traverses
another LATA.  This means that the Subnet Confinement Facility (SCF)
is not a critical item.  However, SWB would eventually want it.
COMMENT:  This facility was a suggestion for future development.
However, Bill believes that this may be illegal and should be presented
to our legal department for an evaluation.  Perhaps, a statement from
SWB allowing intra-LATA calls through another LATA would absolve us of
any legal impropiety.

   e. SWB had concerns about collect calls, reverse charge, and sent
pay.
COMMENT:  No resolutions.  Problems to be addressed in the future.

   f. SWB would like the login syntax to look like this:
		(NUI;PWD:)1+IDN(:RPOA)
			  or NTN
in order to show the caller and callee.
COMMENT:  During this discussion, Bill indicated that our syntax has the
PWD at the end:
		(NUI:)1+IDN(:RPOA);PWD
		      or NTN
Probably the RPOA is also not in the right place, but the complete and
exact syntax is spelled out clearly in Bill's memo dated 29AUG86, which
is believed to be acceptable.  However, this syntax does not accomodate
"abbreviated addressing" (whereby the address is either NTN or
1+DNIC+NTN) nor "digit absorbing" (whereby "+,-,(,),blank" embedded in the
address are discarded), both of which are requested (or required?) by
SWB.

   g. SWB expects TYMNET to handle up to 15-digit addresses (prefix "1"
+ 14-digit IDN):
		 1 + 2XXX   \
		 1 + 3XXX    \
		 1 + 4XXX     \	Call to another network
		 1 + 5XXX     /
		 1 + 6XXX    /
		 1 + 7XXX   /
		 1 + 8XXX	Call to a telex network
		 1 + 9XXX	Call to ISDN
SWB also expects the following address syntax (where IDN = DNIC + NTN):
		X.75		14-digit IDN
		X.25		optional prefix of "1" + 14-digit IDN
	"dumb" terminals	{prefix + IDN} or NTN ("abbrev. addr")
COMMENT:  TYMNET needs to ensure that the interfaces can handle these
addresses, especially the prefix.  However, although its implementation
is not a prblem, it is not recommended (not conveyed to SWB at meeting)
because of the inconsistency between X.25 and CONSAT (ie. the address
"31069" is a DNIC+NTN at  X.25, but a NTN at the CONSAT).  This
situation may lead to user confusion because of the illusion of
"abbreviated addressing" at the X.25 interface.  This
"non-recommendation" was not conveyed to SWB because of their enthusiasm
for this addressing scheme.

   h. SWB expects to have shared DNIC (two networks with the same DNIC)
which is anticipated to be 3146.
COMMENT:  TYMNET needs to determine whether the entire NTN is necessary
for addressing or a portion (eg. 6 digits) is sufficient.  A scheme to
handle shared DNIC must be incorporated into the Numbering Plan.

   i. SWB expects to incorporate other interfaces into their Numbering
Plan.  These interfaces include 3270BSC, SDLC, X.PC, Menu Server, CMT,
and maybe MUX and UTS.  Perhaps all possible TYMNET interfaces would be
incorporated in the future.
COMMENT:  Originally, the expectation was that only the CONSAT and X.25
would be involved, so that customization in only those interfaces would
be feasible.  The problem is to provide a mechanism by which the calling
address is sent to the destination for call back.  TYMNET needs to come
up with a scheme to handle this need via modifications in just the Sup
or modifications in all the various interfaces.

   j. SWB would like in the long term to have the calling NTN in the
accounting data.
COMMENT:  Is this necessary?  Should we do it?  This is just a
convenient item for billing, since the physical port numbers are
available and adequate.  This item is left for future discussion.

3. In response to SWB's needs, Bill came up with three proposals:

	PROPOSAL A.  "Original Proposal"
	This is the original proposal as recommended by Bill in his
	draft dated July 15, 1986.  Basically, the NTN address would be 
	translated by the SUP to a host number and translated by the
	interface to a port number.  This double translation requires
	customization in at least the CONSAT.

	PROPOSAL B.  "Embedded Host Number"
	This proposal was concocted by Bill during the meeting to
	address the issue of special customization for each of the
	interfaces.  Bascially, since the host number field is 
	currently 14 usable bits (which mean 16,383 unique host
	numbers), the Sup could use the last 5 digits of the NTN
	(with the most significant digit being one of two possible
	values) to determine the host number (ie. the NTN contains the
	host number).  Consequently, the various interfaces would not
	have to be customized.

	PROPOSAL C.  "32-bit Hosts"
	This proposal was to address the long term requirement of
	sufficient host numbers to meet SWB needs.  The most that SWB
	would ever need is one NTN per host number, which means that
	TYMNET need to increase the host number field, probably to 32 
	bits eventually.  In this case, the NTN is the host number and
	it would be sparsely used within the entire range.

In order to evaluate the various proposals, Bill formulated the
following table:

		PROPOSAL A		PROPOSAL B 		PROPOSAL C

TERM		short-term		short-term 		long-term
[This entry is actually the development feasibility or lead time for
implementation.]
[short-term means within a year]

ADDRESS PORTS?	Yes			No * 			No *
[* = unless we have one host number for each port]
		
# NTN		16,383 * P		16,383	**		very large
[This entry is actually the # of addressable entities.]
[P = average # of ports per host]
[** = relatively small # of addressable entities unless no distinction
between host numbers and port numbers (ie. one host # for many ports) is
OK.  This restriction may be slightly alleviated by having the Sup
support 15-bit host numbers.]

# PORTS		16,383 * P		16,383 * P		very large
[This entry is the maximum number of physical ports allowed.]

RELOAD X.25?	No			No			No
RELOAD CONSAT?	Yes			Yes			Yes
[This entry is concerned with reloading if a parameter is changed]

SLOT DATA	NTN,host#,port#		host#			host#
[This entry indicates the type of parameters that may need dynamic
modifications.]

{RESTRICTED ASSIGNMENTS OF NTN}
		No			Yes			No
[This entry was not mentioned in the meeting.]

4. Conclusions:

   a. SWB was very anxious to get the show on the road, while keeping
with the advertised price of 12 months and $150,000.  Hence, SWB would
like to stick with Proposal A, although some of the SWB representatives
preferred Proposal B.  Perhaps, Proposal A is in the best interest of
SWB because of the following:  more addressable entities and less
restriction on NTN assignments.

   b. Proposal A, as with the other proposals, needs to address the two
main concerns:  shared DNIC and "other interfaces".  Bill will be
looking into how Proposal A should be modified to accomodate these
concerns.  It may be necssary to utilize certain elements of Proposal B
to handle "other interfaces".

   c. It was agreed that a "phased approach" is needed to satisfy all of
SWB's requirements:
		(1) Phase 1  short-term needs (Proposal A)
		(2) Phase 2  intermediate-term needs (see next subsection)
		(3) Phase 3  long-term needs (?)
SWB is probably expecting some kind of plan or proposal for these
phases, but not necessarily before the start of Phase 1.  For long-term
needs, 32-bit host numbers or multi-network (one net per LATA), or
some combination, may address the insufficient host numbers and/or
addressable entities, but a new naming convention may be necessary to
identify origination/destination ports at call setup.

   d. Some of the requirements for Phase 2 are prioritized:
		PC-based, user-friendly, customized validations facility
		Consat operations monitor (to change parameters)
		Support 3270 BSC
		Support X.PC	    (put NTN in calling addr field)
		Support menu server (put NTN in calling addr field)
		Support CMT	    (put NTN in calling addr field)
		Support SDLC
		Support MUX	    (put NTN in calling addr field)
		Subnet confinement facility (SCF)
		NUI facility for X.75 (pass NUI in call request packet)
		NUI facility 	      (pass NUI in call request packet)
		User friendly (speed) call (currently, called adr has
					    garbage)

5. Action items:

   1. Does the Sup know about the physical port of the originator? [David]
      ANSWER:  No, only the logical port number.  However, both the
origination and destination physical port numbers are reported to the
accounting log by the CONSAT.

   2. Why SWB accounting log not have the physical port number? [Su]

   3. Does the X.25 XOM allow addition of new host numbers? [Ernst,
Newman]

   4. When is the original proposal going to be modified to include
"shared DNIC" and "other interfaces"? [Bill]

   5. Does X.25 process NUI? [Ernst, Newman]

   6. Can intermediate nodes to accounting be turned on/off via PROBE?
[David]
      ANSWER:  No, but this can be implemented.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Wed, 15 Oct 86 0:09:18 UT
From: NTD.D/CHANG@Ontyme.Tymnet 
Date: 14 OCT 86 17:07:58 
To: NTD.W/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.W/SOLEY@Ontyme.Tymnet, QSATS.S/CHIANG@Ontyme.Tymnet,
	TYM.SU/BLU@Ontyme.Tymnet, NSCHQ.LAW@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.D/CHANG@Ontyme.Tymnet 
Message-id: I40521@Ontyme.Tymnet 
Subject: SWB Action Items Follow-up 
SWBNumberingPlanProjectActionItems: 

TO:  Bill Soley, Shirley Chiang, Su Heisler, Jack Orlove
FR:  David Chang
RE:  SWB Numbering Plan Project Action Items
DA:  October 14, 1986
CC:  Bill Euske, Gazel Tan

===================================================================

I am trying to follow up on the action items as listed in the ONTYME
#I31876 concerning the meeting on SWB Numbering Plan project.  I have
not heard anything from you after the initial contact, and I am
wondering whether or not there were any progress.  Please let me know
via ONTYME or call me at 408-446-7374.  Thank you.
From: David Chang <DChang@B39.Tymnet> 
Date: Tue, 14 Oct 86 17:23:19 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: SWB 

Bill,

I am sorry...but I forgot to copy you on the final report on our meeting
with SWB.  The report is in (DCHANG:39)SWB.RPT, and I just sent a message
asking for everyone's response to the action items.  Have you revised the
SWB specs?

David

PS.  Are you feeling better?  I noticed that you were sick before I went
     to the Phase I program in St. Louis last week.
From: Dave Chang <DCHANG@39> 
Date: Thu, 23 Oct 86 13:20:37 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWBT report 


TO:  Bill Euske and Gazel Tan
FR:  Bill Soley and David Chang
RE:  SWB Meeting on the Numbering Plan
DA:  Sepember 26, 1986
Distribution List:
	Allyn Morriss		Chris Black		Pat Driscoll
	Adele Newman		Dennis Ernst		Romolo Raffo
	Dan Lasater		Dave Smith		Shirley Chiang
	Su Heisler		Neil Sullivan		Mike Cady
	Dennis Purpura		Rob Ritchie		Jack Orlove

=============================================================================


This is a report detailing the events and conclusions of the meeting with
SWB on September 17, 1986.  The objective of the meeting was to finalize
the requirements for the SWB Numbering Plan.  In particular, it was
unclear whether addressing by ports was necessary or addressing by hosts
was sufficient.  

1. The meeting was chaired by Jerry Talken of SWB.  There were several
SWB representatives.  TYMNET sales rep Su Heisler (314-878-9855) was
also present.

2. Several problems were discussed:

   a. SWB would like to somehow identify the physical port at the
origination and destination for each session.  Either an accounting log
containing the physical port numbers or a one-to-one correspondence
between NTN and physical port would be satisfactory.
COMMENT:  Currently, X.25 provides this information, but SWB cannot get
the information for the CONSAT.  The physical port numbers should be in
the accounting log already (see action item 1).  The capability of
assigning separate NTN for each physical port requires special
customization which is not anticipated at this time (need to supply the
NTN in the call request protocol).

   b. SWB would NOT like to reload a slot in order to add a new
customer.  Traditionally, the method of adding a host number to a slot
is by reconfiguring and reloading.  This means that current sessions
will have to be torn down, ie. disrupting customer service.
COMMENT:  X.25 interface has an operations monitor (XOM) which
allows a user to dynamically change certain parameters.  It is hoped
that the XOM facilitates the addition of a new host number (see action
item 2).  The CONSAT interface will need such a facility.  (see action
item 3) 

   c. SWB envisions the need for increasing the number of addressable
entities.
COMMENT:  TYMNET may need to support the 32-bit host numbers or SWB may
utilize a multi-network system.  This is for the future.  (see below)

   d. SWB indicated that our previous understanding was incorrect, ie.
an intra-LATA call should have the "best" path, even if it traverses
another LATA.  This means that the Subnet Confinement Facility (SCF)
is not a critical item.  However, SWB would eventually want it.

COMMENT:  This facility was a suggestion for future development.
However, Bill believes that this may be ILLEGAL and should be presented
to our legal department for an evaluation.  Perhaps, a statement from
SWB allowing intra-LATA calls through another LATA would absolve us of
any legal impropiety.  (see action item 4)

   e. SWB had concerns about collect calls, reverse charge, and sent
pay.
COMMENT:  No resolutions.  Problems to be addressed in the future.

   f. SWB would like the login syntax to look like this:
		(NUI;PWD:)1+IDN(:RPOA)
			  or NTN
in order to show the caller and callee.
COMMENT:  During this discussion, Bill indicated that our syntax has the
PWD at the end:
		(NUI:)1+IDN(:RPOA);PWD
		      or NTN
Probably the "(:RPOA)" is also not in the right place, but the complete and
exact syntax is spelled out clearly in Bill's memo dated 29AUG86, which
is believed to be acceptable.  However, this syntax does not accomodate
"abbreviated addressing" (whereby the address is either NTN or 1+DNIC+NTN)
nor "character absorbing" (whereby "+,-,(,),blank" embedded in the
address are discarded).  The former is required and the latter is
requested.  (see action item 5)

   g. SWB expects TYMNET to handle up to 15-digit addresses (prefix "1"
+ 14-digit IDN):
		 1 + 2XXX   \
		 1 + 3XXX    \
		 1 + 4XXX     \	Call to another network
		 1 + 5XXX     /
		 1 + 6XXX    /
		 1 + 7XXX   /
		 1 + 8XXX	Call to a telex network
		 1 + 9XXX	Call to ISDN
SWB also expects the following address syntax (where IDN = DNIC + NTN):
		X.75		14-digit IDN
		X.25		optional prefix of "1" + 14-digit IDN
	"dumb" terminals	{prefix + IDN} or NTN ("abbrev. addr")
COMMENT:  TYMNET needs to ensure that the interfaces can handle these
addresses, especially the prefix.  However, although its implementation
is not a problem, it is not recommended (not conveyed to SWB at meeting)
because of the inconsistency between X.25 and CONSAT (ie. the address
"31069" is a DNIC+NTN at X.25, but a NTN at the CONSAT).  This
situation may lead to user confusion because of the illusion of
"abbreviated addressing" at the X.25 interface.  (see action item 6)

   h. SWB expects to have shared DNIC (two networks with the same DNIC)
which is anticipated to be 3146.
COMMENT:  TYMNET needs to determine whether the entire NTN is necessary
for addressing or a portion (eg. 6 digits) is sufficient.  A scheme to
handle shared DNIC must be incorporated into the Numbering Plan.  (see
action item 5)


   i. SWB expects to incorporate other interfaces into their Numbering
Plan.  These interfaces include 3270BSC, SDLC, X.PC, Menu Server, CMT,
and maybe MUX and UTS.  Perhaps all possible TYMNET interfaces would be
incorporated in the future.
COMMENT:  Originally, the expectation was that only the CONSAT and X.25
would be involved, so that customization in only those interfaces would
be feasible.  The problem is to provide a mechanism by which the calling
address is sent to the destination for call back.  TYMNET needs to come
up with a scheme to handle this need via modifications in just the Sup
or modifications in all the various interfaces.  (see action items 5,8)

   j. SWB would like in the long term to have the calling NTN in the
accounting data.
COMMENT:  Is this necessary?  Should we do it?  This is just a
convenient item for billing, since the physical port numbers are
available and adequate.  This item is left for future discussion.

3. In response to SWB's needs, Bill came up with three proposals:

	PROPOSAL A.  "Original Proposal"
	This is the original proposal as recommended by Bill in his
	draft dated July 15, 1986.  Basically, the NTN address would be 
	translated by the SUP to a host number and translated by the
	interface to a port number.  This double translation requires
	customization in at least the CONSAT.

	PROPOSAL B.  "Embedded Host Number"
	This proposal was concocted by Bill during the meeting to
	address the issue of special customization for each of the
	interfaces.  Bascially, since the host number field is 
	currently 14 usable bits (which mean 16,383 unique host
	numbers), the Sup could use the last 5 digits of the NTN
	(with the most significant digit being one of two possible
	values) to determine the host number (ie. the NTN contains the
	host number).  Consequently, the various interfaces would not
	have to be customized.

	PROPOSAL C.  "32-bit Hosts"
	This proposal was to address the long term requirement of
	sufficient host numbers to meet SWB needs.  The most that SWB
	would ever need is one NTN per host number, which means that
	TYMNET need to increase the host number field, probably to 32 
	bits eventually.  In this case, the NTN is the host number and
	it would be sparsely used within the entire range.

In order to evaluate the various proposals, Bill formulated the
following table:

		PROPOSAL A		PROPOSAL B 		PROPOSAL C

TERM		short-term		short-term 		long-term
[This entry is actually the development feasibility or lead time for
implementation.]
[short-term means within a year]

ADDRESS PORTS?	Yes			No * 			No *
[* = unless we have one host number for each port]
	
# NTN		16,383 * P		16,383	**		very large
[This entry is actually the # of addressable entities.]
[P = average # of ports per host]
[** = relatively small # of addressable entities unless no distinction
between host numbers and port numbers (ie. one host # for many ports) is
OK.  This restriction may be slightly alleviated by having the Sup
support 15-bit host numbers.]

# PORTS		16,383 * P		16,383 * P		very large
[This entry is the maximum number of physical ports allowed.]

RELOAD X.25?	No			No			No
RELOAD CONSAT?	Yes			Yes			Yes
[This entry is concerned with reloading if a parameter is changed.]

SLOT DATA	NTN,host#,port#		host#			host#
[This entry indicates the type of parameters that may need dynamic
modifications.]

{RESTRICTED ASSIGNMENTS OF NTN}
		No			Yes			No
[This entry was not mentioned in the meeting.]

4. Conclusions:

   a. SWB was very anxious to get the show on the road, while keeping
with the advertised price of 12 months and $150,000.  Hence, SWB would
like to stick with Proposal A, although some of the SWB representatives
preferred Proposal B.  Perhaps, Proposal A is in the best interest of
SWB because of the following:  more addressable entities and less
restriction on NTN assignments.

   b. Proposal A, as with the other proposals, needs to address the two
main concerns:  shared DNIC and "other interfaces".  Bill will be
looking into how Proposal A should be modified to accomodate these
concerns.  It may be necessary to utilize certain elements of Proposal B
to handle "other interfaces".

   c. It was agreed that a "phased approach" is needed to satisfy all of
SWB's requirements:
		(1) Phase 1  short-term needs (Proposal A)
		(2) Phase 2  intermediate-term needs (see next subsection)
		(3) Phase 3  long-term needs (?)
SWB is probably expecting some kind of plan or proposal for these
phases, but not necessarily before the start of Phase 1.  For long-term
needs, 32-bit host numbers or multi-network (one net per LATA), or
some combination, may address the insufficient host numbers and/or
addressable entities, but a new naming convention may be necessary to
identify origination/destination ports at call setup.

   d. Some of the requirements for Phase 2 are prioritized:
		PC-based, user-friendly, customized validations facility
		Consat operations monitor (to change parameters)
		Support 3270 BSC
		Support X.PC	    (put NTN in calling addr field)
		Support menu server (put NTN in calling addr field)
		Support CMT	    (put NTN in calling addr field)
		Support SDLC
		Support MUX	    (put NTN in calling addr field)
		Subnet confinement facility (SCF)
		NUI facility for X.75 (pass NUI in call request packet)
		NUI facility for X.25 (pass NUI in call request packet)
		User friendly (speed) call (currently, called adr has
					    garbage)

5. Action items:

   1.  Need to investigate why SWB is not getting the physical port number
in their accounting log.  Both the origination and destination physical
port numbers should be reported to the accounting log.  [Shirley Chiang,
Su Heisler]

   2.  Need to verify that the X.25 XOM allows addition of new host
numbers and that X.25 processes the NUI.  [Dennis Ernst, Adele Newman]
RESULT:  As long as the lines are configured in, then the XOM will allow
the dynamic specification of new host numbers.  The NUI defined as
"username;password" is processed in version 4.00 and above.

   3.  SWB needs to submit a PIR for the CONSAT Operations Monitor.
[Su Heisler]

   4.  Need to investigate whether or not there is a legality
issue concerning the traversing of another LATA in intra-LATA calls.
[Jack Orlove]

   5.  Need to have an updated edition of the specifications for the
SWB Numbering Plan, which will include the handling of shared DNIC,
abbreviated addressing, and character absorbing, as well as a general
scheme to support the CONSAT for Phase 1 and other interfaces in the
future.  [Bill Soley]
COMMENT:  A forthcoming memo to Mike Cady will include this lastest
draft, which should be presented to SWB as the technical specifications
for the Numbering Plan project.

   6.  Need to convey NTD's recommendation to SWB that X.25 and "dumb
terminals" should both expect abbreviated addressing to avoid confusion.
[Su Heisler]

   7.  Need to check whether or not intermediate node information to the
accounting log can be turned on/off via PROBE.  [David Chang]
RESULT:  No, but this can be implemented.

   8.  Need to involve the ISIS Development unit in the SWB Numbering
Plan project.  In order to provide a feasible general scheme to handle
"other interfaces", unanticipated development in ISIS is now expected.
[Allyn Morriss, Dave Smith, Dan Lasater]
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 23 Oct 86 16:02:50 PDT 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWB...is it legal? 

Bill,

I will be talking to Jack Orlove of our legal dept concerning the legal
issue of the SWB Numbering Plan.  As I understand it, you think that it
may be illegal to make an intra-LATA call through another LATA...correct?
If so, why?  Is there some reason why you think so?  If I am incorrect, what
is the problem and why?  I would appreciate some response so that I could
talk to Jack with some hint of intelligence.  Thanks.

David
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Thu, 23 Oct 86 23:08:58 UT
From: NTD.D/CHANG@Ontyme.Tymnet 
Date: 23 OCT 86 16:01:09 
To: NTD.W/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.P/DRISCOLL@Ontyme.Tymnet, NTD.D/ERNST@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.O/SERANG@Ontyme.Tymnet, NTD.W/SOLEY@Ontyme.Tymnet,
	NTD.D/CHANG@Ontyme.Tymnet 
Message-id: I45303@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Meeting on Calling Address 
HowWeShouldHandleTheCallingAddress: 

TO:  Pat Driscoll	Dennis Ernst		Adele Newman
     Gazel Tan		Omar Serang
FR:  David Chang
CC:  Bill Soley
RE:  How We Should Handle The Calling Address
DA:  October 23, 1986

==================================================================

Bill Soley and I would like to discuss certain issues concerning how
TYMNET should handle the calling address.  In particular, what should
the calling address be and what other information should be passed in
the needle to the destination (concerning the origination).  A meeting
is set up for Tuesday, Oct 28, at 2pm in conference room 1 of building
D.  Your attendance is necessary to resolve the issues.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 27 Oct 86 11:46:53 PST 
To: dchang 
Keywords: SWBT 
Subject: SWB 

Su just called and asked if we've received the final version of Jerry's
numbering plan.  Should have come in about Thursday.  I said we haven't
so she is Federal Expressing another copy.  Have you gotten a copy?
Also have a conference call scheduled for Friday 10/31 at 11:00 in my
office to talk about a couple of questions like "the case of the missing
NUI" and "9XXX: ISDN or Voice?".  If you could be available, we can both
get on line.  She is also concerned about getting time and $ estimates on
the other phases.  Since these don't have much to do with the SUP or NODECODE,
we should talk to Euske about that.  How did it go with Orlove?  I'll try
to call this afternoon.  I'm off to Stacey's to get a PROLOG book so I'll
be back about 1:30 if you want to try me at home.  -Bill
From: David Chang <DChang@B39.Tymnet> 
Date: Mon, 27 Oct 86 13:34:17 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWB Misc 

Bill,

I did not get anything from Jerry.  I will be available for the conference
call on Friday.  Orlove seemed to be helpful...said he will follow up on
the legality question with Steve Bell.  I plan to wait a couple of days and
check on progress.  I found Euske and asked him about the Phase 2.  The
conclusion was to give it to Allyn Morriss.  However, we need to have a
well-defined calling adr design (I believe) before he can seek estimates from
the managers of the other interfaces.  Allyn will also be asking you about
SCF and the last item in the list (ie. user-friendly call, etc.).  Finally,
who should be doing the pc-based validations facility, them or us?  We might
want to discuss these issues with Allyn.  I also told Allyn that the outcome
of tomorrow's meeting should be the definition of calling addressing and
soon afterwards the final spec for SWB.

David

Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 28 Oct 86 17:19:29 UT
From: TYM.SU/BLU@Ontyme.Tymnet 
Date: 28 OCT 86 08:44:03 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: TYM.SU/BLU@Ontyme.Tymnet, NTD.B/EUSKE@Ontyme.Tymnet,
	NTD.B/SOLEY@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet 
Message-id: I47683@Ontyme.Tymnet 
Keywords: SWBT, SWBT 
Subject: conference call w/ SWBT 
swbtnumberingplan: 

to:  bill soley
     david chang
fr:  su heisler
re:  swbt numbering plan
cc:  bill euske
     
I talked to Jerry Talken and we will be calling you on Friday, 10/31 
at 11:00 am your time.  We need to discuss item 2.g from the notes
you put out from our September 17 meeting.

Also, Jerry has given me further informtion about "dial out" addressing.
In my simple mindI understand this as addressing for locations outside
of the SWBT packet network--regardless of destination.  You've examined
most of this so there is some review.

A user typing a "1" will always be telling the network he wants to go
outside of the SWBT packet net.  

If the second digit is "1", the user will be going to the mobile maratime net
                       "2 - 7", the user will be going to another packet net
                       "8", the user will be going to the telex net
                       "9", the user will be going either to an ISDN net or
                            to the voice net.
If the second through fifth digits are "9001" always put the call through to 
ISDN.  If the third digit does not equal "0", then expect to receive one to
three digits of the telephone country code.  In the short term, however, SWBT
expects to only receive second and third digits of "91" indicating a call to
the North American voice network.  They don't want to have to pad the fourth
and fifth digits if they don't have to, but instead want us to recognize the
"91" and read the next seven or ten digits as a telephone number.  They will
accept padding to "9100" if necessary.  

Does this make sense?  I hope so, but regardless, this should also
be mentioned in our conference call.

thanks

talk to you friday.
From: David Chang <DChang@B39.Tymnet> 
Date: Tue, 28 Oct 86 18:20:10 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT, SWBT 
Subject: SWB specs 

Bill,

This is just a reminder...the specs that you are going to update should
not only include abbreviated addressing, character absorbing, and a 
calling address scheme, but also shared DNIC handling.  There may be
others, but I just noticed that we have (or have we?) not discussed
shared DNIC.

David
From: David Chang <DChang@B39.Tymnet> 
Date: Wed, 29 Oct 86 17:52:04 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: String Processing in APE 

Bill,

I am writing about the string processing in the access profile per our meeting
but got stuck on a few issues.  Could you enlighten me?

1. A string in the origination can be used for DNIC blocking.  Is there any
other usage?

2. A string in the destination can be used to specify a destination in the
next network.  How is this different from the current usage of transparent
login?

3. Are there other usages of destination strings?

Thanks,

David
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Tue, 4 Nov 86 12:13:30 PST 
To: dchang, Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWB document review 

Bill,

I have reviewed the SWBT numbering plan document.

Comments:

4.1. I was a little unclear about the disk record size discussion.

4.2. to 4.7.  Is this stuff all Access Profiles construction?
I guess I'm a little unfamiliar to the concepts being discussed so it
was a bit unclear.

5.  "improvement in path computation time of 48".  Is this 48 percent?

Thanks...Omar

From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 6 Nov 86 14:00:27 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: string processing 

Bill,

I just finished writing a report for Holcomb which he needs tomorrow.
Would you review it for me ASAP since I am still uncertain of the details?
Thanks.

David

			    P R O P O S A L




		PROCESSING STRINGS IN THE ACCESS PROFILES

			(by David Chang 31OCT86)



A.  INTRODUCTION

	Access profiles in the Master User Directory (MUD) define the
allowable origination and destination combinations for each user or hostname.
Currently, originations can be classes, hosts, or nodes and destinations
can be groups or hosts.  More flexibility may be incorporated if both
originations and destinations can be defined as strings.  The
origination string may specify the calling address of a call request,
and the destination string may specify a hostname as the desired
destination.

B.  ORIGINATION STRING

	A string in the origination of an access profile entry (APE) may
used for validation of the calling symbolic address in a call request.
This origination string must successfully match the ORIGINAL calling
address in the incoming call in order to use the corresponding destinations
in the circuit building process.  If the match is unsuccessful, then the
next access profile, or origination/destination pair, is analyzed.  For
convenience, wild cards may be allowed to represent a "general address".

	Original calling address means the actual address of the user at
the original network.  Somehow this address must be passed through all
the transit networks for analysis by the current network.  It may not be
feasible to specify all possible transit networks in the origination
string nor is it clear whether such specification is useful.  In any
case, only the originating address is being considered here.

	One of the uses of origination string is "DNIC blocking", where
a call from a particular DNIC is prohibited access.  Suppose we have
networks A, B, C, D with DNIC's 4321, 4567, 3146, and 3106 respectively.
Suppose we are network C and we wish to block all calls from network A
to network D, regardless of any number of transit network B.  An entry
in the access profile for hostname "-31069876543210" in network C may
contain origination [~"-4321*"] with destination [g3106] where
	~ denotes exception, or negative, string (in this case)
	" denotes the string in the entry
	* denotes wild cards, ie. anything that follows is permitted
	g3106 denotes a group of gateway hosts to network D
This entry indicates that access to network D is permitted if the
calling address does not have 4321 as the originating DNIC.  If there
were no other entries for this hostname, then calls from A would effectively
be blocked from going to D via C.  However, different entries may be
desired to specify certain destinations for various origins.  If this 
hostname had many entries, then each of them must contain the exception
string in the origination.  This would be too costly in terms of
processing and to cumbersome in terms of maintenance of the profiles.

	A more aethestic solution is to define a new item, called a
"null" destination, which is acts as an "answered" host in the MUD
processing, but results in the call being cleared.  The access profile
entry would have the origination as ["4321*"] to match all calls from A
and the destination as [null].  For extra insurance, this should be the
first entry and perhaps with the "priority stop" bit set (ie. do not
search further for other priority targets).  This method provides for a
cleaner solution to the problem of DNIC blocking.

	In order to support DNIC blocking, several modifications are
necessary:
1. TYMNET must support calling address whereby the ultimate "caller"
address must be identifiable.  This means that the user (or the
interface) must be able to specify the calling address, that the
original calling address must be able to be passed through all transit
networks, and that the nodes must be able to accomodate large needle
messages that contain the source, transit (?) and destination addresses.
2. TYMNET must support strings in the MUD entries.  NETVAL must be able
to allow the user to enter strings into access profile entries and the
Supervisor must be able to perform string comparisons.  This support
must also include exception strings and null destinations.

C. DESTINATION STRING

	A string in the destination of the access profile may be used to
designate the specific destination in the targeted network.  In other
words, this destination string is actually a hostname.  The main benefit
of this feature is the possibility of using a user-friendly, symbolic
address to specify a port of the destination host in the local network
or to specify the desired "host" in a foreign network.

	In general, whenever a username is used with a hostname in a
login string, intersection processing must be done to find the desired
destination.  If the hostname is declared as a valid destination in the
access profile for the username, then the intersection processing is
eliminated.  Of course, string comparisons must be done, but this is less
CPU intensive.  Furthermore, the homed destination string can be used as
the called address sent to the destination.  For the homed or non-homed
destination, the entire login string is still sent to the destination in
a Tymnet network.  In either case, some information is available which
may or may not be used to further select a port or host.

	Port selection in the local network may be accomplished by the
usage of the homed destination string.  Suppose the user wishes to login
with the following string:  "USER:P2" by which he wishes to get to host
333 port 2.  The access profiles may be configured as:

	USER			P2			H333P2
    origin  dest	   origin  dest		     origin  dest
    class0  "P2"	   class0  !"H333P2"	     class0  !host333

where	class0	means all originations are valid
	prefix !  means a home destination

Here the string "P2" in the login string matches the destination string,
so no intersection processing is required.  Checking hostname P2, the search
is for some valid home destination, (in this case, the string "H333P2").
Checking the homed hostname H333P2, the valid home destination is host 333.
Hence the call routed to host 333 with the called address of "H333P2".
At this host, the called address may be parsed to get the information that
port 2 is desired.  Assume that all the originations were verified and
that other entries did not relate to this login.

	Host selection in a foreign network is accomplished
similarly.  Suppose a user wishes to access his second bank with the
login string "USER:BANK2" and that this bank is in some foreign network.
The following access profiles may be setup:

	USER			BANK2			    -3111
   origin  dest		   origin  dest			origin  dest
   class0  "BANK2"	   class0  !"-31114151234567"	class0  !host45

As before, the string "BANK2" matches the destination string in the
username profile.  In the search for the destination of hostname BANK2,
the homed destination X.121 address "-31114151234567" is found to be
valid.  Since the DNIC 3111 is foreign (assuming the local DNIC is
3106), then the hostname -3111 is searched.  In the case, the final
destination is homed host 45, which should be the X.75 gateway host to
the foreign network with DNIC 3111.  The called address "31114151234567"
is passed to the foreign network, which targets the call to the desired
address 4151234567.  Note that a single gateway may serve several
networks and this method allows targets to different networks via the
same gateway, providing further control of traffic to foreign networks.
However, this search of hostnames may go on forever, so some limit must
be imposed.

	In addition to the modifications for origination string, other
modifications are needed to support port/host selection via the
destination string:
1. TYMNET must pass the login string as specified by the user in the
needle, as well as the called address.  This called address should be the
homed destination string of the second to the last APE lookup and should
correspond to the address for the ultimate destination.  Furthermore,
some limitation should be imposed on the number of hostname lookups,
probably via a Tymfile parameter.  Destination interfaces must be able
to interpret the hostname field in the login string of the needle.

D. CONCLUSION

String definition in the access profile is a logical extension to the
resolution of the MUD entries.  Currently, the access profile can only
describe hosts and nodes.  With the string capabililty, the access
profile will be able to additionally describe possibly ports (in Tymnet
network), foreign networks (for DNIC blocking), and addresses.  This is
a powerful feature that allows for the control of traffic to and from
foreign networks, which is requested by Televerket (STA).
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 6 Nov 86 20:31:15 PST 
To: David Chang <DChang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: string processing 
In-reply-to: your message of Thu, 6 Nov 86 14:00:27 PST

Dave,

Thank you for the oppertunity to review your document.  Below are some
comments.  If you want to get together to talk, I should be available
in the afternoon.  In my comments below, I've quoted the first line of
each paragraph to make it easier to match my comments up with your text.
My comments are just that - comments - and do not necessarily represent
anything that needs to be changed.

Some of my comments here may contradict previous discussions or notes.
There is nothing like trying to write things down to bring out all the
inconsistencies.  I appologize if its been a little disorganized.  Now
that the SWBT Recommendation is done, I look forward to being able to
spend more time thinking about this.

I suggest the use of "symbolic address" in place of "strings" in the title
and most places where you aren't actually talking about the specific data
structure.  Although "strings" is correct, the reader may lose track of
what they really mean.  Note that a host name is not always a symbolic
address, but may only be part of one (e.g. just the DNIC), so the two
terms are not interchangable.

"A.  INTRODUCTION"
"A1	Access profiles in the Master User Directory (MUD) define the..."

I think it is more correct to say "the destination symbolic address may
specify a host name or X.121 address".

You might want to add, here, that this uses a new needle field, and to
say a little about the semantics of that field.

"B.  ORIGINATION STRING"
"B1	A string in the origination of an access profile entry (APE) may..."
"B2	Original calling address means the actual address of the user at..."

I don't believe we want any transit specifications in the origination-address
field.  [It isn't even clear they really belong in the destination-address
field.  When needle format time comes, we may want to consider having seperate
fields for the RPOA and destination-address.]

"B3	One of the uses of origination string is "DNIC blocking", where..."

If we are looking at the world through the eyes of network C (3146), we
should never see a hostname of "-31069876543210".  Since 3106 is another
network, the hostname would consist of only the DNIC: "-3106".

	For example, if the administration of network C (3146) wanted to
	block all calls from network A (4321) to network D (3106) from
	transiting their network, they could validate the host name "-3106"
	with the following access profile:

		Name: -3106 (host)
		origins	   |     destinations
		-----------|-----------------
		~"-4321*"  |  (home)Group3106

	When a call comes from network A, DNIC=4321.  The calling DTE
	address from the X.75 call request packet is placed in the
	origination address field of the needle by the X.75 gateway:
	-43219999999.  Likewise, the called DTE address is placed in the
	destination address field: -31068888888.  The user name is
	provided either from the NUI, if present, or is defaulted by the
	supervisor, based on the origination symbolic address.  Assuming
	there is no NUI, the user name would automatically be -4321.

	As far as this example is concerned, the contents of the access
	profile for -4321 is not important.  It can not be used to achieve
	the kind of blocking desired here since it can be overriden by a
	NUI.

"B4	A more aethestic solution is to define a new item, called a..."
"B5	In order to support DNIC blocking, several modifications are..."
"C. DESTINATION STRING"
"C1	A string in the destination of the access profile may be used to..."
"C2	In general, whenever a username is used with a hostname in a..."

Actually, the supervisor must still do an intersection even when strings are
found in the destination field.  In the case where both strings and hosts
or groups appear in the profile, the hosts and groups must still undergo
intersection.

"C3	Port selection in the local network may be accomplished by the..."

Actually, there is no need to put the string !"H333P2" in the profile of
"P2", here.  Port selection is a function performed in the interface and it
does not require the use of a symbolic address in an access profile.  The
example here is just an alias.

"C4	Host selection in a foreign network is accomplished..."

This is just an alias, as well, but there is considerably more justification
for it since it allows a user-friendly name such as BANK2 as opposed to
-31114151234567.  The extra overhead (disk) can possibly be justified on
the part of the network administration by reducing the "misdialed" call rate.
Misdialed calls create more overhead, and no revenue, possibly even requiring
a manual credit of the customer's bill.

These two examples are really identical.  It makes no difference if the
ultimate destination is in the home network or not.  You might just show
the case where the call is to the foreign network, and note that it can
just as easily handle domestic destinations.

"C5	In addition to the modifications for origination string, other..."

I've tried (but probably slipped a few times) to not use the words
"called (or calling) [DTE] address" unless I'm talking about the field
of an X.25 or X.75 call request.  In place, I've been using "origination
(or destination) [symbolic] address" which may be a DTE address or a
host name.  Without the word "symbolic" it may also be a host number.

"D. CONCLUSION"
"D1	String definition in the access profile is a logical extension to..."

----------------

May want to mention the types of wild cards:
	=	matches either "-" or "-####-"
	*	matches any number of characters (maybe none)
	?	matches any single character

We probably won't support embedded "*".

In general, I think its a very good summary.  You have said alot in a
little space - something the readers will appreciate.

-Bill
From: David Chang <DChang@B39.Tymnet> 
Date: Fri, 7 Nov 86 18:53:48 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: symbolic address report 

Bill,

I was in meetings all day today and did not study your comments in depth.
I just looked at it in some detail and find that I probably will have
some questions.  I still need to study it some more.  Could we get together
sometime mid-week?

David
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Mon, 10 Nov 86 23:08:29 UT
From: TYM.SU/BLU@Ontyme.Tymnet 
Date: 10 NOV 86 14:34:03 
To: NTD.W/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Message-id: I54940@Ontyme.Tymnet 
Keywords: SWBT 
Subject: "To: Bill Soley Fr: Su Heisler"... 
10DigitNumbering: 

     To:  Bill Soley
     
     Fr:  Su Heisler
     
     Re:  10 Digit Numbering
     
     CC:  Al Fenn
          Bill Euske
          Mike Cady
          Forrest Hicks
          Rob Ritchie
     
     
     I want  to take this opportunity to say Thank You for a job well done in
     turning around  the final draft of our 10-Digit Numbering Plan Response.
     I appreciate  the effort  you put into the document and also the way you
     turned the document around so quickly.
     
     We're very excited about this project and appreciate your effort.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 18 Nov 86 16:07:51 UT
From: TYM.SU/BLU@Ontyme.Tymnet 
Date: 18 NOV 86 07:47:33 
To: NTD.W/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.W/SOLEY@Ontyme.Tymnet, NTD.W/EUSKE@Ontyme.Tymnet,
	TYM.F/HICKS@Ontyme.Tymnet, TYM.SU/BLU@Ontyme.Tymnet 
Message-id: I58262@Ontyme.Tymnet 
Keywords: SWBT 
Subject: NUMBERING PLAN 
NUMBERINGPLAN: 

TO:  BILL SOLEY
FR:  SU HEISLER
RE:  NUMBERING PLAN

CC:  BILL EUSKE
     FORREST HICKS

CAN YOU PLEASE BE AVAILABLE IN CUPERTINO THURSDAY  11/20 FOR A 
POSSIBLE CONFERENCE CALL?  JERRY STILL HAS QUESTIONS AND IS COMING HERE
TO EXPLAIN THEM TO ME.  I WOULD LIKE TO BE ABLE TO PHONE YOU  AFTER 11 A M
YOUR TIME IF I NEED TO.

PLEASE RESPOND VIA ONTYME AND LET ME KNOW IF YOU CAN BE THERE.
IF YOU CON'T, WE'LL WING IT.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 18 Nov 86 12:24:34 PST 
To: Su Heisler <TYM.SU/BLU@Ontyme.Tymnet> 
Cc: Dave Chang <DChang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: NUMBERING PLAN 
In-reply-to: I58262@Ontyme.Tymnet of 18 NOV 86 07:47:33

>	From: TYM.SU/BLU@Ontyme.Tymnet 
>	Date: 18 NOV 86 07:47:33 
>
>	Can you please be available in Cupertino Thursday 11/20 for a
>	possible conference call?  Jerry still has questions and is coming
>	here to explain them to me.  I would like to be able to phone you
>	after 11 AM your time if I need to.

Su,

I'll plan to be in my office between 11:00 and 11:30 on Thursday.  If it
looks like it might be later than that, give me a call and I can adjust
lunch.  Dave will probably be there as well.  Please call even if it turns
out that you don't need us to answer the questions so we will know not to
wait any longer for the call.

-Bill
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Fri, 21 Nov 86 18:11:42 UT
From: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Date: 21 NOV 86 09:53:37 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I60177@Ontyme.Tymnet 
Keywords: SWBT 
Subject: NUMBERING PLAN 
NumberingPlan: 

     To:  Bill Soley
          David Chang
     
     Fr:  Su Heisler
     
     Re:  Numbering Plan
     
     cc:  Bill Euske
          Mike Cady
          Forrest Hicks
     
     
     Here are  52 items  that Jerry  feels are specified in his document.  He
     has marked  the items  he's confident  are or  are not  addressed by our
     phase 1 document.  Please review these items and
     
     1.  Tell us if Jerry's assessment's are accurate,
     2.  Fill in the blanks where you can,
     3.  Make corrections where necessary.
     
     Thanks for your support as always.
     
     
                                                               PROVIDED
                                                                 in
         FEATURE                                               PHASE 1
         -------                                               --------
     
     1.  TERM TO HOST CALLING SUPPORTED                           YES
     2.  TERM TO TERM CALLING SUPPORTED
     3.  HOST TO HOST CALLING SUPPORTED                           YES
     4.  HOST TO TERM CALLING SUPPORTED
     5.  AN NTN CAN BE ASSOCIATED WITH                            YES
         MULTIPLE PORTS.
     6.  COMMERCIAL TRAFFIC IS CONFINED TO A                      YES
         LATA, WHILE OCS IS NOT.  (OCS TRAFFIC
         MUST ORIGINATE AND TERMINATE WITHIN A
         LATA BOUNDARY ENROUTE.)
     7.  DNIC SHARING SUPPORTED, INCLUDES                         YES
         DNPA-DCO INTRA-INTERLATA ROUTING
         TABLES.
     8.  INTRADNIC CALLS ARE MADE ON A 10-
         DIGIT NTN BASIS (FULL IDN IS NOT
         ENTERED).  THESE CALLS MAY BE INTRA-
         OR INTERLATA, INTRA- OR INTERNET WORK.
     9.  INTERDNIC CALLS ARE MADE BY ENTERING 1+                  YES
         IDN.
     10. 1+9+1+10D ENTERED TO PLACE A CALL FROM THE               YES
         PPSN TO PSTN.
     11. 1+9001+10D ENTERED TO PLACE A CALL FROM THE              YES
         PPSN TO ISDN.
     12. 1+8+TDC+NTN ENTERED TO PLACE A CALL FROM THE             YES
         PPSN TO TELEX.
     13. PRIVATE SUBSCRIBER NETWORK CODE SUPPORTED                YES
     14. CHARACTERS - () + AND BLANK ARE DISCARDED                YES
         IF EMBEDDED IN NTN OR IDN
     15. 10-DIGIT NUI SUPPORTED                                   YES









     16. PASSWORD (USING a-z, A-Z, 0-9, AND SPECIAL               YES
         CHARACTERS) SUPPORTED.
     17. ROAMER SERVICE SUPPORTED                                 YES
         INCLUDING PRESUBSCRIPTION PER LATA, AND
         DELIVERY OF ALL TO IC POP IN ORIGINATING LATA
     18. TRANSMIT NUI OVER X.75 FACILITY, UNLESS                   NO
         BLOCKED.
     19. TRANSMIT NUI OVER X.25 FACILITY, UNLESS                   NO
         BLOCKED
     20. CUG SERVICE SUPPORTED
     21. SENT-PAID CALLING SUPPORTED
     22. AUTOMATIC PLACEMENT OF CALLS TO SPECIFIC                  NO
         DNPA OR DNPA-DCO'S ON REVERSE CHARGE BASIS
     23. CALL SCREENING ON X.75 FACILITIES ON                      NO
         INCOMING CALLS
     24. RPOA SUPPORTED                                           YES
     25. TRANSIT NETWORK PRESELECTION                             YES
     26. RPOA FORCED ON INTERLATA CALLS IF CUSTOMER               YES
         NOT PRESUBSCRIBED
     27. RETURN MESSAGE TO ORIGINATING CUSTOMER,                  YES
         AND CLEAR CALL IF PRESUBSCRIBED TRANSIT
         NETWORK IS NOT AVAILABLE ON INTERLATA CALL
         REQUEST
     28. USER FRIENDLY (SPEED) CALLING                             NO
     29. MENU DRIVEN CALLING ARRANGEMENT                           NO
     30. ANY NTN MAY BE ASSOCIATED WITH ANY PORT                  YES
         (NO PREDETERMINED/FIXED ASSOCIATION OF
         NTNs/PORTS
     31. A PORT MAY BE ASSIGNED ONE OR MORE NTNs                  YES
     32. ONE OR MORE NTNs  MAY BE ASSOCIATED WITH                 YES
         A PORT
     33. THE FOLLOWING INTERFACES ARE SUPPORTED:
             X.25                                                 YES
             X.28                                                 YES
             ASYNC                                                YES
             BISYNC                                                NO
             SDLC                                                  NO
             HDLC                                                  NO
             X.75                                                 YES
     34. AN INTERFACE CAN BE ASSIGNED 10 SEQUENTIAL               YES
         NTNs, BEGINNING WITH EPN XXX0
     35. AN INTERFACE CAN BE ASSIGNED 100 SEQUENTIAL              YES
         NTNs, BEGINNING WITH EPN XXOO
     36. AN INTERFACE CAN BE ASSIGNED MULTIPLE BLOCKS             YES
         OF 10 OR 100 NTNs -- WHERE THE BLOCKS MAY BE
         CONTIGUOUS OR NON-CONTIGUOUS
     37. AN INTERFACE CAN BE PART OF A HUNT GROUP                 YES
         WHICH IS ASSIGNED A SINGLE NTN
     38. AN INTERFACE CAN BE PART OF A HUNT GROUP                 YES
         WITH INDIVIDUAL LINE ADDRESSING
     39. INTRALATA/INTRADNIC CALLS TO AN IC MAY BE                YES
         BLOCKED -- OR ALLOWED TO COMPLETE ON A
         LATA BASIS
     40. IF A CALL REQUEST (EXCEPT DIRECT CALLS)                  YES
         DOES NOT CONTAIN A CALLED ADDRESS THE CALL
         IS CLEARED.
     41. IF A CALL REQUEST IS RECEIVED ON A DIRECT









         CALL INTERFACE WITHOUT A CALLED ADDRESS,
         THE APPROPRIATE ADDRESS IS INSERTED.
     42. THE FORMAT OF CALLED ADDRESSES (10-DIGIT NTN
         OR 1+DNIC+NTN) IS VERIFIED -- AND THE CALL IS
         CLEARED IF FORMAT IS INCORRECT.
     43. THE CALLING ADDRESS ON A DIRECT ACCESS INTERFACE
         IS VERIFIED, AND THE CALL IS CLEARED IF IT IS
         NOT ONE ASSIGNED TO THE PORT.
     44. IF THE CALLING ADDRESS IS NOT PRESENT ON A CALL
         REQUEST, THE APPROPRIATE ADDRESS FOR THE PORT
         IS INSERTED.
     45. NUI/PASSWORD MAY BE ENTERED ON AN OPTIONAL               YES
         BASIS FROM A DIRECT ACCESS INTERFACE.
     46. NUI/PASSWORD MAY BE ENTERED ON AN OPTIONAL               YES
         BASIS FROM A DIAL ACCESS INTERFACE.
     47. NUI/PASSWORD MAY BE MANDATORY ENTRIES ON
         SPECIFIC CALLS.  (E.G., REVERSE CHARGED CALLS
         ORIGINATED FROM DIAL ACCESS INTERFACE, OCS, ETC.)
     48. THE CALLED ADDRESS SENT ON A CALL TO A DIRECT
         ACCESS LINE SHALL BE A 14-DIGIT IDN ASSIGNED TO
         THAT INTERFACE.  (A PREFIX IS NOT TRANSMITTED
         THE DNIC IS INSERTED IF NOT PRESENT.)
     49. THE CALLING ADDRESS SENT TO AN INTERFACE SHALL BE
         9001+NXX+NXX+XXXX IF THE CALL ORIGINATED ON AN ISDN
     50. IF A CALL IS REDIRECTED, THE CALLED ADDRESS IN THE
         INCOMING CALL PACKET SHALL BE AN ADDRESS ASSIGNED TO
         THE ALTERNATE DTE.  AN ADDRESS ASSIGNED TO THE
         ORIGINALLY CALLED DTE SHALL BE PRESENT IN THE CALL
         REDIRECTION NOTIFICATION FACILITY
     51. IF AN X.25 CALL ACCEPTED PACKET CONTAINS THE
         CALLING OR CALLED ADDRESS, THE CALL SHALL BE
         CLEARED IF THEY DO NOT CORRESPOND TO THE
         ADDRESSES SUPPLIED IN THE INCOMING CALL PACKET.
     52. THE CALLING AND CALLED IDN (WITHOUT PREFIX) SHALL        YES
         ALWAYS BE PROVIDED ON X.75 INTERFACES.  DNIC IS
         INSERTED IF NOT PRESENT
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Wed, 26 Nov 86 14:27:32 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWB Syntax Diagram 

Bill,

Weyyi and I are meeting on Sunday to finalize the Supervisor
Login parsing for the SWB and Host Names projects.  If possible
I could really use a finalized Syntax diagram to work from.

Just leave it in my mailbox, or send mail regarding it's disposition.

Thanks...Omar

From: David Chang <DChang@B39.Tymnet> 
Date: Wed, 3 Dec 86 16:58:33 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: No NUI issue 

Bill,

If the NUI is not present, then we use the calling address as the NUI.
Does that mean that we need to set up a MUD entry for every possible
calling address including those from foreign nets?  Or are we saying
that we use the calling address as the NUI only in the needle and such,
but that we do not do any MUD lookup on the pseudo NUI (nor any passwork
check)?

David
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 3 Dec 86 18:35:49 PST 
To: David Chang <DChang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: No NUI issue 
In-reply-to: your message of Wed, 3 Dec 86 16:58:33 PST

If there is no user-name (NUI), then the calling-address is used to construct
a name to be looked up.  The constructed name is not passed in the user-name
field, it is only used locally by the supervisor.

As with any user-name, if it begins with a "-" (X.121) then it is subject
to truncation before lookup in the MUD based on the DNIC.  In this way, if
we get a call from an X.75 gateway as follows...

calling-address		-31117654321
NUI			none

Then we will construct a user-name of -31117654321.  The user-name will then
be truncated to -3111 (assuming that 3111 is a non-sharred DNIC) and looked
up in the MUD.  No password validation will be performed.

Because of the truncation, it will only be necessary to provide one user-name
per foreign network, as opposed to one per foreign address.

-Bill
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Thu, 4 Dec 86 18:20:41 UT
From: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Date: 04 DEC 86 09:58:59 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I64810@Ontyme.Tymnet 
Keywords: SWBT 
Subject: NUMBERING PLAN 

TO:  BILL SOLEY
FR:  SU HEISLER
RE:  NUMBERING PLAN

I'M NOT SURE ABOUT THIS ONE, BUT BEAR WITH ME.....

DID YOU PROMISE TO UPDATE YOUR DOCUMENT, IN PARTICULAR A CERTAIN
TABLE AND FIGURE ONE AND GET THE DOCUMENT BACK TO US?  I'M THINKING
THAT YOU DID, BUT I MAY BE WRONG.  

IN THE MEANTIME, I RAN INTO JERRY IN THE HALL AND HE HAD SOME
COMMENT ABOUT THAT RPOA TABLE.  DID HE CALL YOU?  IF NOT, WHY DON'T
YOU GIVE HIM A CALL AT 314/235-1858.  IT SOUNDED LIKE HE CHANGED
HIS MIND ABOUT ONE OF THE RPOA TABLES, BUT YOU TWO HAD BETTER SORT
IT OUT.

LET ME KNOW EITHER BY PHONE OR ONTYME.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 4 Dec 86 14:29:08 PST 
To: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Keywords: SWBT 
Subject: Re: NUMBERING PLAN 
In-reply-to: I64810@Ontyme.Tymnet of 04 DEC 86 09:58:59

Hi Su,

I did promise to update the document when all the terminology was finlized
and to send out a copy of the new syntax drawing.  These things are still
up in the air a little, and may be for the next month.  Its not worth doing
a draft of the document with the terminology, but it might be helpful to
Jerry to see a draft of the syntax drawing.  I will send that out in the
next week.

Its too late to call him now, but I will go over the RPOA table, and give
him a call tomorrow.  I'll keep you posted.  These things should not need
to hold up the contract, so I'm giving them slightly lower priority than
the list of phase one items.  Dennis Ernst has been out of town so far.

Thanks
-Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 4 Dec 86 14:41:17 PST 
To: dchang 
Keywords: SWBT 
Subject: NUMBERING PLAN 

(begin forwarded messages)
----------------------------------------------------------------
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Thu, 4 Dec 86 18:20:41 UT
From: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Date: 04 DEC 86 09:58:59 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I64810@Ontyme.Tymnet 
Subject: NUMBERING PLAN 

TO:  BILL SOLEY
FR:  SU HEISLER
RE:  NUMBERING PLAN

I'M NOT SURE ABOUT THIS ONE, BUT BEAR WITH ME.....

DID YOU PROMISE TO UPDATE YOUR DOCUMENT, IN PARTICULAR A CERTAIN
TABLE AND FIGURE ONE AND GET THE DOCUMENT BACK TO US?  I'M THINKING
THAT YOU DID, BUT I MAY BE WRONG.  

IN THE MEANTIME, I RAN INTO JERRY IN THE HALL AND HE HAD SOME
COMMENT ABOUT THAT RPOA TABLE.  DID HE CALL YOU?  IF NOT, WHY DON'T
YOU GIVE HIM A CALL AT 314/235-1858.  IT SOUNDED LIKE HE CHANGED
HIS MIND ABOUT ONE OF THE RPOA TABLES, BUT YOU TWO HAD BETTER SORT
IT OUT.

LET ME KNOW EITHER BY PHONE OR ONTYME.
----------------------------------------------------------------
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 4 Dec 86 14:29:08 PST 
To: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Subject: Re: NUMBERING PLAN 
In-reply-to: I64810@Ontyme.Tymnet of 04 DEC 86 09:58:59

Hi Su,

I did promise to update the document when all the terminology was finlized
and to send out a copy of the new syntax drawing.  These things are still
up in the air a little, and may be for the next month.  Its not worth doing
a draft of the document with the terminology, but it might be helpful to
Jerry to see a draft of the syntax drawing.  I will send that out in the
next week.

Its too late to call him now, but I will go over the RPOA table, and give
him a call tomorrow.  I'll keep you posted.  These things should not need
to hold up the contract, so I'm giving them slightly lower priority than
the list of phase one items.  Dennis Ernst has been out of town so far.

Thanks
-Bill
----------------------------------------------------------------
(end forwarded messages)

Dave,
FYI...can't tell if you were on original copy list.
-Bill
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 4 Dec 86 18:23:03 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWB misc 

Bill,

Thanks for a copy of Su's memo...I was not copied (or at least, I did not
receive anything yet).  I left a note with Ernst about the questions, and
I talked to Adele about them.  I hope to hear something early next week.
Are you planning to talk to Jerry?  I also need access to your acronym list
to put into my glossary.  We had a talk with Romolo concerning the new MUD,
and we discussed a little bit the SWB customization.  He thinks that similar
customization in NETVAL for SWB would be useful, eg. checking syntax of
the destination name.

David
From: David Chang <DChang@B39.Tymnet> 
Date: Fri, 5 Dec 86 18:16:42 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Misc SWB and addressing questions 

Bill,

I have several questions for you...

1. I would like to include your acronyms in my glossary.  May I copy it
from your file?

2. More additions to the glossary...what do you think?

NATIVE - pertaining to the local network or the network under
consideration.

NATIVE DNIC - the DNIC of the native network; there may be several for
a network.

DOMINANT DNIC - the default DNIC of a network that is used in the absence
of a specfied DNIC in a X.121 address; only one per network.  [Ex: 3106
for Tymnet]

RECESSIVE DNIC - the non-dominant DNIC of a network with several DNIC's.
[Ex: the DNIC for RCA which is within Tymnet public net]

SHARED DNIC - a DNIC that is used by more than one network.

FOREIGN DNIC - a DNIC of a non-native network.

3. We had talked about an APE with "host 5, ~host 6" which on the
surface seems meaningless.  I think that we resolve to say that it means
host 5.  But in the Sup, currently if we encounter something like this,
whatever is found first will be used.  In this case, host 5 is used, but
if the order was reversed "~host 6, host 5", then I think that the Sup
will match with any host not 6 before it sees host 5 as an item.  Any
comments?

4. By the same token, the old processing allowed the host numbers and
the groups to be in any order, and thus a customer may or may not take
this fact into consideration when defining his APE.  With the new MUD,
it is required that the host list is ordered and comes before the list
of groups.  This is subtle change which the customer may not notice.  Do
you think it is okay?

5. Your answer concerning no NUI prompted more questions.  If the
origination address is not X.121 and there is no username, then we
cannot do any truncation.  Do we clear the call at this point? 
Or are we saying that all origination address must be X.121?

6. Suppose that we have no NUI and we have a X.121 origination address,
then it seems that we must lookup the DNIC table to see if it is native
and/or shared and then decide what to do?!

NATIVE	SHARED	PERFORM
 Yes	 Yes	Lookup the entire username or DNIC with m digits?
 Yes	 No	Lookup the entire username or DNIC with m digits?
 No	 Yes	Lookup the entire username or DNIC with m digits?
 No	 No	Lookup the truncated address.

7. Did you know that Tymnet has a pseudo-numbering plan?  According to a
X.25 document, we have after the 3106 DNIC:
	0 + hhhhh + ssss  => host number with optional subhost
	9 + nnnnnnnnn	  => Network User Number (NUN) as username;
			     NTN as called address
	7 + hhhhh	  => host number as calling address
	77+ nnnn + pppp	  => node # and port # as calling address

8. Did you talk to Jerry?  Are you waiting for the completion of
glossary that I am working on?  I am considering the glossary as a
working document for the specs on orig/des addressing and symbolic
addressing.

Thanks,

David
From: David Chang <DChang@B39.Tymnet> 
Date: Fri, 5 Dec 86 18:42:37 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Deleting usernames 

Bill,

You previously showed me a list of possible usernames to be deleted.  Are
you handling this, or do you want us to pursue this.  I believe that
@, [, and : must be excluded from the username syntax, but are there others?
I have forgotten.

Omar indicated that you are preparing yet another version of the syntax
diagram to include all permitted punctation...true or false? and when?

I will try to corner Ernst early part of next week.

Have a nice weekend!

David
From: David Chang <DChang@B39.Tymnet> 
Date: Fri, 5 Dec 86 18:52:47 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: more on swb 

Bill,

I just thought of more questions...

1. If the transit address is begin with a ":", then we cannot tell whether
we have a transit address or a SWB NTN destination address or a host number
until we find the second ":" or more digits.  Perhaps, a different indication
would be more appropriate??

2. In you figure 2 diagram, you have a question "NTN ours".  How is this
checked?

David
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 8 Dec 86 22:57:34 PST 
To: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Keywords: SWBT 
Subject: Re: numbering plan features 
In-reply-to: I65112@Ontyme.Tymnet of 04 DEC 86 15:06:11

Just going through the old mail and came across your note about Jerry's
list.  Its still being held up by Dennis Ernst.  I think I mentioned this
when we talked on the phone, but in case I didn't (lovely ladies aren't
the only absent minded ones).  Will let you know.  -Bi
From: David Chang <DChang@B39.Tymnet> 
Date: Wed, 10 Dec 86 17:16:29 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: swb questions 

Bill,

I finally talked to Ernst and I have some answers, as well as some HUH's.
Would like us go over them, or perhaps I could just send a brief reply with
our comments to SWB?

I just thought of something...if hostnames are defined to start with a lettera
and usernames may start with punctuations and digits, then during migration
how do we reconcile with the current usernames to be both usernames and
hostnames in the new MUD?

David
From: David Chang <DChang@B39.Tymnet> 
Date: Wed, 10 Dec 86 18:14:30 PST 
To: Omar Serang <OSERANG@B39.Tymnet> 
Keywords: SWBT 
Subject: glossary 

Omar,

The glossary is "finished".  See (DCHANG:39)GLOSSAR.RPT.  Please make sure
your report uses the same terms.

David
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 10 Dec 86 18:18:55 PST 
To: David Chang <DChang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: swb questions 
In-reply-to: your message of Wed, 10 Dec 86 17:16:29 PST

I suppose we should talk first.  If you're still there, give me a call at
home, otherwise, I'll see you tomorrow or call.  I'll send you the original
message if you just want to "edit" in our answers.  -Bill

Oh on the hostname/ username punctuation thing.  It just means that people
with usernames that are not legal hostnames because they begin with some
bogus character, they won't be able to use them as hostnames.  That's what
they get for being "creative".  -Bill
From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Thu, 11 Dec 86 14:39:50 PST 
To: WRS@C39.Tymnet 
Cc: INTL.VIDA@ONTYME.TYMNET, NTD.D/CHANG@ONTYME.TYMNET 
Keywords: SWBT 
Subject: "SPECIAL CHARACTERS USER NAMES-"... 

SPECIAL CHARACTERS - USER NAMES- DAVID' MEMO.  IF NETVAL V 3.07
LIVES UP TO ITS SPEC (ERS 10/30 - PAGE 6), THE ONLY  CHARACTER IN
THE MEMO THAT IS ALLOWED NOW IS THE BLANK(SPACE). VIDA WILL 
DO AN EXPERIMENT TO VERIFY. IF THAT WORKS, ONLY NEED 1)ELIMINATE 
BLANK (NETVAL MOD) AND CHANGE CURRENT "BAD" USERNAMES. THAT WILL 
TAKE A BIT OF TIME - BUT NO BIG DEAL. (WHAT IS `023 AND `030 _
AT TOP - THE FIRST 2 ITEMS???)
ANY AMPLIFICATION ON PURPOSE OF RESERVING, OR WHY THESE CHARACTERS??
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 11 Dec 86 15:09:54 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: INTL.VIDA@ONTYME.TYMNET, NTD.D/CHANG@ONTYME.TYMNET 
Keywords: SWBT 
Subject: Re: "SPECIAL CHARACTERS USER NAMES-"... 
In-reply-to: your message of Thu, 11 Dec 86 14:39:50 PST

\023 is control-S
\030 is control-X
Since the supervisor interprets these as control messages, they are
clearly mistakes, and can be deleted without any concern for a
current user.  The actual task of deleting them, however, may need
help from a NETVAL wizard.  I will attempt to get a full dump of the
MUD this weekend so we can be sure about the extent of usernames
which violate the policy.  There are probably about 5000 names that
were missing from the file I used to generate that list, so there
may be one or two other bogus names.

The purpose of the exercise is to reserve special characters to be used
for other options that are being added to the login sequence.  While
we are at it, we are also reserving some characters which we feel we
might need in the future for special options.

-Bill
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 11 Dec 86 16:17:30 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWB questions 

Bill,

I did not hear from you, so I edited the file (DCHANG:39)SWBT.MEM with
the answers to Jerry's questions.  Please check it out.  If everything
is fine, then you could send it out (or I can tomorrow after you say 
that it is okay).  However, question #29 was not answered, since I
suppose that Su should be able to answer it.  

David
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 11 Dec 86 16:53:47 PST 
To: Su Heisler <TYM.SU/BLU@Ontyme.Tymnet> 
Cc: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWB questions and Glossary 

Su,

Here are the answers and comments to questions submitted by Jerry of SWB.
The answers and comments are enclosed in [].  A [???] means that the question
was NOT understood.

Appended to this memo after the questions is the glosssary of terms.

Regards,
David

     
     
                                                               PROVIDED
                                                                 in
         FEATURE                                               PHASE 1
         -------                                               --------
     
     1.  TERM TO HOST CALLING SUPPORTED                           YES
     2.  TERM TO TERM CALLING SUPPORTED			[yes]
     3.  HOST TO HOST CALLING SUPPORTED                           YES
     4.  HOST TO TERM CALLING SUPPORTED			[yes]
     5.  AN NTN CAN BE ASSOCIATED WITH                            YES
         MULTIPLE PORTS.
     6.  COMMERCIAL TRAFFIC IS CONFINED TO A                      YES
         LATA, WHILE OCS IS NOT.  (OCS TRAFFIC
         MUST ORIGINATE AND TERMINATE WITHIN A
         LATA BOUNDARY ENROUTE.)
     7.  DNIC SHARING SUPPORTED, INCLUDES                         YES
         DNPA-DCO INTRA-INTERLATA ROUTING
         TABLES.
     8.  INTRADNIC CALLS ARE MADE ON A 10-		[yes]
         DIGIT NTN BASIS (FULL IDN IS NOT
         ENTERED).  THESE CALLS MAY BE INTRA-
         OR INTERLATA, INTRA- OR INTERNET WORK.
     9.  INTERDNIC CALLS ARE MADE BY ENTERING 1+                  YES
         IDN.
     10. 1+9+1+10D ENTERED TO PLACE A CALL FROM THE               YES
         PPSN TO PSTN.
     11. 1+9001+10D ENTERED TO PLACE A CALL FROM THE              YES
         PPSN TO ISDN.
     12. 1+8+TDC+NTN ENTERED TO PLACE A CALL FROM THE             YES
         PPSN TO TELEX.					[yes, but ignore case]
     13. PRIVATE SUBSCRIBER NETWORK CODE SUPPORTED                YES
     14. CHARACTERS - () + AND BLANK ARE DISCARDED                YES
         IF EMBEDDED IN NTN OR IDN
     15. 10-DIGIT NUI SUPPORTED                                   YES









     16. PASSWORD (USING a-z, A-Z, 0-9, AND SPECIAL               YES
         CHARACTERS) SUPPORTED.
     17. ROAMER SERVICE SUPPORTED               	[???]     YES
         INCLUDING PRESUBSCRIPTION PER LATA, AND
         DELIVERY OF ALL TO IC POP IN ORIGINATING LATA
     18. TRANSMIT NUI OVER X.75 FACILITY, UNLESS                   NO
         BLOCKED.
     19. TRANSMIT NUI OVER X.25 FACILITY, UNLESS                   NO
         BLOCKED
     20. CUG SERVICE SUPPORTED				[yes]
     21. SENT-PAID CALLING SUPPORTED			[yes]
     22. AUTOMATIC PLACEMENT OF CALLS TO SPECIFIC                  NO
         DNPA OR DNPA-DCO'S ON REVERSE CHARGE BASIS	[yes with collect call]
     23. CALL SCREENING ON X.75 FACILITIES ON           [???]      NO
         INCOMING CALLS
     24. RPOA SUPPORTED                                           YES
     25. TRANSIT NETWORK PRESELECTION                             YES
     26. RPOA FORCED ON INTERLATA CALLS IF CUSTOMER               YES
         NOT PRESUBSCRIBED
     27. RETURN MESSAGE TO ORIGINATING CUSTOMER,                  YES
         AND CLEAR CALL IF PRESUBSCRIBED TRANSIT
         NETWORK IS NOT AVAILABLE ON INTERLATA CALL
         REQUEST
     28. USER FRIENDLY (SPEED) CALLING                             NO
     29. MENU DRIVEN CALLING ARRANGEMENT		[???]     NO
     30. ANY NTN MAY BE ASSOCIATED WITH ANY PORT                  YES
         (NO PREDETERMINED/FIXED ASSOCIATION OF
         NTNs/PORTS
     31. A PORT MAY BE ASSIGNED ONE OR MORE NTNs        [???]     YES
     32. ONE OR MORE NTNs  MAY BE ASSOCIATED WITH       [???]     YES
         A PORT
     33. THE FOLLOWING INTERFACES ARE SUPPORTED:
             X.25                                                 YES
             X.28                                                 YES
             ASYNC                                                YES
             BISYNC                                                NO
             SDLC                                                  NO
             HDLC                                                  NO
             X.75                                                 YES
     34. AN INTERFACE CAN BE ASSIGNED 10 SEQUENTIAL               YES
         NTNs, BEGINNING WITH EPN XXX0		[requires 10 MUD entries]
     35. AN INTERFACE CAN BE ASSIGNED 100 SEQUENTIAL              YES
         NTNs, BEGINNING WITH EPN XXOO		[requires 100 MUD entries]
     36. AN INTERFACE CAN BE ASSIGNED MULTIPLE BLOCKS             YES
         OF 10 OR 100 NTNs -- WHERE THE BLOCKS MAY BE
         CONTIGUOUS OR NON-CONTIGUOUS
     37. AN INTERFACE CAN BE PART OF A HUNT GROUP                 YES
         WHICH IS ASSIGNED A SINGLE NTN
     38. AN INTERFACE CAN BE PART OF A HUNT GROUP                 YES
         WITH INDIVIDUAL LINE ADDRESSING
     39. INTRALATA/INTRADNIC CALLS TO AN IC MAY BE                YES
         BLOCKED -- OR ALLOWED TO COMPLETE ON A
         LATA BASIS
     40. IF A CALL REQUEST (EXCEPT DIRECT CALLS)                  YES
         DOES NOT CONTAIN A CALLED ADDRESS THE CALL
         IS CLEARED.
     41. IF A CALL REQUEST IS RECEIVED ON A DIRECT	[no, requires









         CALL INTERFACE WITHOUT A CALLED ADDRESS,	symbolic addressing]
         THE APPROPRIATE ADDRESS IS INSERTED.
     42. THE FORMAT OF CALLED ADDRESSES (10-DIGIT NTN	[yes, name not in MUD]
         OR 1+DNIC+NTN) IS VERIFIED -- AND THE CALL IS
         CLEARED IF FORMAT IS INCORRECT.
     43. THE CALLING ADDRESS ON A DIRECT ACCESS INTERFACE	[yes]
         IS VERIFIED, AND THE CALL IS CLEARED IF IT IS
         NOT ONE ASSIGNED TO THE PORT.
     44. IF THE CALLING ADDRESS IS NOT PRESENT ON A CALL	[yes]
         REQUEST, THE APPROPRIATE ADDRESS FOR THE PORT
         IS INSERTED.
     45. NUI/PASSWORD MAY BE ENTERED ON AN OPTIONAL               YES
         BASIS FROM A DIRECT ACCESS INTERFACE.
     46. NUI/PASSWORD MAY BE ENTERED ON AN OPTIONAL               YES
         BASIS FROM A DIAL ACCESS INTERFACE.
     47. NUI/PASSWORD MAY BE MANDATORY ENTRIES ON		[yes]
         SPECIFIC CALLS.  (E.G., REVERSE CHARGED CALLS
         ORIGINATED FROM DIAL ACCESS INTERFACE, OCS, ETC.)
     48. THE CALLED ADDRESS SENT ON A CALL TO A DIRECT		[yes]
         ACCESS LINE SHALL BE A 14-DIGIT IDN ASSIGNED TO
         THAT INTERFACE.  (A PREFIX IS NOT TRANSMITTED
         THE DNIC IS INSERTED IF NOT PRESENT.)
     49. THE CALLING ADDRESS SENT TO AN INTERFACE SHALL BE	[no, no
         9001+NXX+NXX+XXXX IF THE CALL ORIGINATED ON AN ISDN	ISDN interface]
     50. IF A CALL IS REDIRECTED, THE CALLED ADDRESS IN THE	[no, no specs,
         INCOMING CALL PACKET SHALL BE AN ADDRESS ASSIGNED TO	non-CCITT]
         THE ALTERNATE DTE.  AN ADDRESS ASSIGNED TO THE
         ORIGINALLY CALLED DTE SHALL BE PRESENT IN THE CALL
         REDIRECTION NOTIFICATION FACILITY
     51. IF AN X.25 CALL ACCEPTED PACKET CONTAINS THE		[yes, *]
         CALLING OR CALLED ADDRESS, THE CALL SHALL BE
         CLEARED IF THEY DO NOT CORRESPOND TO THE
         ADDRESSES SUPPLIED IN THE INCOMING CALL PACKET.
     52. THE CALLING AND CALLED IDN (WITHOUT PREFIX) SHALL        YES
         ALWAYS BE PROVIDED ON X.75 INTERFACES.  DNIC IS
         INSERTED IF NOT PRESENT


* If the call accepted contain CLAMN (called line address modified
  notification), then we will not clear the call if the called addresses
  are different.


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(DCHANG:39)GLOSSA.RPT
DECEMBER 10, 1986






			G L O S S A R Y




	This is a glossary of Tymnet network terminology.  The first
section refers to CCITT terms, the second refers to Tymnet terms, and
the third refers to acronyms.  This is intended as a working document,
and thus subject to changes and additions.


CCITT Terminology

CALL REQUEST - an X.25 packet requesting to setup a call and containing
the calling DTE address and the called DTE address (in addition to other
various X.25 fields).

CALLING (DTE) ADDRESS - an address (usually X.121) in packed binary
coded decimal digits (max 15) specifying the original requestor of a call.  

CALLED (DTE) ADDRESS - an address (usually X.121) in packed binary coded
decimal digits (max 15) specifying the requested destination of a call.

X.121 ADDRESS - an address containing a 4-digit (decimal) DNIC followed
by 0 to 10 digits of NTN.
 

TYMNET Terminology

ACCESS PROFILE ENTRY (APE) - an entry in an access profile represented
by a pair of origination and destination lists.

ADDRESS - a symbolic representation of the origination or destination of
a circuit.  [Currently, X.21 address or hostname, but not host number]

CIRCUIT REQUEST - a node-to-sup message (0C) indicating a request to
setup a circuit and containing the login string and origination
information, eg. address, host, node, physical port.

CLASS - a set of hosts or nodes, either all inclusions or all
exclusions, in the origination list.

DESTINATION ADDRESS - the address indicating the destination of a
circuit; the prefixed (with a "-") X.121 address that is derived from
the called DTE address of an incoming call (call request) at a X.25/X.75
interface; the resultant destination address.

DESTINATION LIST - a list of possible destination items (targets),
eg. host, name, group.

DESTINATION NAME - a name of an access profile in the MUD that describes
a destination, either hostname or X.121 name.  [Formerly, destination
name was called "hostname".]

DNIC TABLE - a data base in the Supervisor that defines the
characteristics of all DNIC's relevant to the network.
 
EXCLUSION - an item or a representation of a set of items in the
destination list that will result in a circuit request failure if
specified in the destination field of the login string.  [Previously,
called "exception" or "negative".]

EXTENSION - the supplementary information in the extension field of the
login string that is sent in the needle to the destination for possible
further refinement of the ultimate destination (eg. telephone number for
out-dial, port number for port selection, or call user data for X.25 call
request packet).  [Currently, no specs for its implementation].

FOREIGN - pertaining to a network outside the native network.

FOREIGN DNIC - a DNIC of a non-native (ie. foreign) network.

FULL ADDRESS - an address that contains a network identifier (eg. DNIC).
[Currently, only X.121 address is a full address; in the future, perhaps
"39@FOO" or "TYMNET~SYS57" may be used.]

GROUP - a set of hosts, either all inclusions or exclusions, in the
destination list.

HOME ADDRESS - an address in the destination list of an access profile
that is designated "home" and is used in MUD lookups to determine the
resultant destination address.

HOSTNAME - a destination name for an access profile in the MUD that
may be specified by a string of characters beginning with a letter;
a non-X.121 address.

INCLUSION - an item or a representation of a set of items in the
destination list that may result in a circuit request success if
specified in the destination field of the login string.  [Previously,
called "positive".]

IN-PLACE - hosts or nodes specifically listed in the origination or
destination lists of an APE, ie. not part of a class or group.

LOGIN STRING - a string of characters containing the option, user(name),
transit, destination, and extension fields for the purpose of requesting
a circuit (terminated by a CR or ";").

MUD ENTRY (or ACCESS PROFILE) - the access profile containing set of
origination/destination lists for a name (username or destination
name) in the MUD.

NAME - a designation validated via NETVAL that defines and specifies
a particular MUD entry, ie. a username and/or destination name.
				NAME
		__________________|___________________
		|				     |
	    USERNAME			     DESTINATION NAME
	     /    \				/	  \
	NON-X.121  X.121		  HOSTNAME	X.121 NAME

NATIVE - pertaining to the local network, ie. the network under
consideration.

NATIVE DNIC - a DNIC of the native network.

NEEDLE - the Sup-to-node message (09) which is used to thread a circuit
and which contains the appropriate origination and destination
information (eg. host, node, address) of the circuit; the ISIS message
with similar function and information.

ORIGINATION ADDRESS - the address indicating the origination of a circuit;
the prefixed (with a "-") X.121 address that is derived from the calling
DTE address of an incoming call (call request) at a X.25/X.75 interface.

ORIGINATION LIST - a list of allowable origination items (origins),
eg. node, host, name, class.

PRIMARY DNIC - the default DNIC of a network that is used in the absence
of specified DNIC in a X.121 address; only one per network.  [Ex: 3106
is the primary DNIC for Tymnet.]

RESULTANT - after all login processing.

RESULTANT DESTINATION - the final address or hosts resulting from all
MUD processing to determine the termination point of the requested
circuit.

SECONDARY DNIC - the non-primary DNIC of a netowrk with more than one
DNIC defined.  [Ex: the DNIC for RCA is a secondary DNIC within the
Tymnet public network.]

SHARED DNIC - a DNIC that is used by more than one network.

SPECIFIED - present in the login string.

SPECIFIED DESTINATION - the information (address or hosts with possibly a
subhost number) in the destination field of the login
string describing the desired destination of the circuit being
requested.

SPECIFIED TRANSIT - the information [currently, RPOA] in the transit
field of the login string describing the desired transit network of
the circuit to be built toward the specified destination.

SUBNET - a set of nodes which are controlled by and interconnected to,
but logically isolated from, a hierarchically superior network.

USERNAME - a name in the username field of a login string that
represents the user, ie. the person who is making a circuit request;
the NUI in a X.25 packet that is used in the username field; MUD lookup
is dependent of type (non-X.121, X.121).

X.121 ADDRESS - a CCITT X.121 address whose presence in TYMNET is
indicated by a prefix dash "-".

X.121 NAME - a destination name for an access profile in the MUD that
may be specified by a string of digits preceded by a "-" and that may be
derived from an X.121 address (derivation is specific to the network).

X.121 USERNAME - a username that begins with a "-" and followed by
decimal digits; MUD lookup depends on specifications in the DNIC table.
[Non-X.121 username utilizes the entire name for the lookup.]

Acronyms

APE	Access Profile Entry
Bellcore	Bell Communications Research, Inc.
BOC	Bell Operating Company
CC	Country Code
CCITT	Consultative Committee on International Telegraph and Telephone
CMT	Character Mode Terminal interface
CUG	Closed User Group
DCO	Data Central Office code
DNIC	Data Network Identification Code
DNPA	Data Numbering Plan Area code
DTE	Data Terminal Equipment
EPN	End Point Number
GED	General External Design
IC	Inter-LATA Carrier
IDN	International Data Number [DNIC + NTN]
IIX	Interface Information Exchange
ISDN	Integrated Services Digital Network
LATA	Local Access and Transport Area
MDNSC	McDonnell Douglas Network Systems Company (Tymnet\(rg)
MUD	Master User Directory
NETVAL	NETwork VALidations program
NN	National Number
NTD	Network Technology Development (Division of MDNSC)
NTN	Network Terminal Number [DNPA + DCO + EPN]
NUI	Network User Identification
PAD	Packet Assembler and Disassembler
PC	Personal Computer
PIN	Personal Identification Number
PPSN	Public Packet Switches Network
PSN	Private Subscriber Network
RPOA	Recognized Private Operating Agency (ie. TNIC)
SCF	Subnet Confinement Facility
SN	Subscriber Number
SWBT	SouthWestern Bell Telephone
TA	Technical Advisory
TCC	Telephone Country Code
TNIC	Transit Network Identification Code (ie. RPOA)
UUN	Universal User Number
VAP	Value Added Passthrough
VSA	Virtual Subnet Arrangement
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 11 Dec 86 17:15:29 PST 
To: Romolo Raffo <RAFFO@B36.Tymnet> 
Cc: Bill Soley <WRS@B39.Tymnet>, Wayne Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet>,
	Weyyi Wang <WWANG@B39.Tymnet>, Omar Serang <OSERANG@B39.Tymnet>, Rich
	Teng <RICHTENG@B39.Tymnet>, Gazel Tan <GAZEL@B39> 
Keywords: SWBT 
Subject: Punctuation Characters in Usernames 

TO:  Romolo Raffo
FR:  David Chang
RE:  Punctuation Characters in Usernames
CC:  Bill Soley, Wayne Bartlett, Weyyi Wang, Omar Serang, Rich Teng
DA:  December 11, 1986

==========================================================================

I am proposing that NETVAL restrict usage of certain punctuation
characters in usernames so that we can reserve them for special
purposes in the future.  In particular, I suggest that legal
non-alphanumerics in the usernames should be:
		# $ * & , . - /
in order to be flexible and compatible with current usage.  At present,
there are a few characters:
		blank " % ' + ; @ [ cntl-chars
that are possibly in use and are being investigated by Wayne for
deletion.  I would like to "illegalize" the following characters:
		blank " % ' + ; @ [ ] ( ) < > ! ^ = ~ { } | \ ? :
as well as all control characters.

Furthermore, the destination names (formerly called "hostnames") will be
of two types: hostnames that begin with a letter, and X.121 names that
begin with a "-".  The same restriction should be applied for these
names.

I am putting this suggestion forth for discussion to see whether we
agree, whether it can be implemented, what are the ramifications, and
when should we impose these restrictions.

From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 12 Dec 86 8:35:22 PST 
To: David Chang <DChang@B39.Tymnet> 
Cc: Romolo Raffo <RAFFO@B36.Tymnet>, Bill Soley <WRS@B39.Tymnet>, Wayne
	Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet>, Weyyi Wang <WWANG@B39.Tymnet>,
	Omar Serang <OSERANG@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>,
	Gazel Tan <GAZEL@B39>, INTL.VIDA@ONTYME.TYMNET 
Keywords: SWBT 
Subject: Re: Punctuation Characters in Usernames 
In-reply-to: your message of Thu, 11 Dec 86 17:15:29 PST

A FOOTNOTE - BY MY UNDERSTANDING - SUBJECT REQUEST ONLY INVOLVES
ONE CHANGE - NAMELY - OUTLAW USE OF BLANK IN USERNAME - FINE BY ME
From: David Chang <DChang@B39.Tymnet> 
Date: Fri, 12 Dec 86 11:57:36 PST 
To: Wayne Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet> 
Cc: Bill Soley <WRS@B39.Tymnet>, Vida <INTL.VIDA@Ontyme.Tymnet>, Gazel Tan
	<GAZEL@B39> 
Keywords: SWBT 
Subject: Punctuations 

Wayne,

It was my understanding from our conference call with you and Vida that
you will be looking into the usernames currently using the punctuations
that we would like to reserve for special meanings in the future.
Hopefully, we could delete or convert these usernames in order to
conform with our new restrictions.  If this is possible, then we could
request Romolo to incorporate the restrictions into NETVAL so that
future usernames will also conform with the restrictions.

I hope that our understandings coincide.  Not only are we looking at the
blanks, but also all the other punctuations indicated by the * in my
note to you.  If our understandings differ,  please let me know so that
we could sort this out.

Thanks,

David
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 19 Dec 86 1:10:42 PST 
To: Dave Chang <DChang@B39.Tymnet>, Wayne D. Bartlett <BARTLETW@B39.Tymnet>,
	Vida Stafford <TYMOPS.V/Stafford@Ontyme.Tymnet> 
Keywords: SWBT 
Subject: illegal user name report 

The following report shows all 116 illegal user names found in the public
network MUD.  The list uses the following conventions to display names which
may otherwise be unprintable:
    ^x   = control x
    _    = blank
All other characters are exactly as they appear.

The policy for validation programs should be to ALLOW any of the following:
	#$&*-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
with the exception that the characters
	-.
may not be used at the beginning of a user name.

The following files exist on Bubbnet system 39:
	(WRS)MUDANL.MSG		this message
	(WRS)MUDANL.RPT		the report (included below)
	(WRS)MUDANL.LIS		just the list of invalid usernames
They should be listed on 132 column paper, as should this message.

Thank you.
-Bill
'OCLCYSQ
OLYMPIA,04
IDJL^x^x052DC
DBUG,06
WDNUSER,03
JARVIS,GW
TADAMS,02
NET,01
NET,02
NET,03
NET,04
NET,05
NET,06
NET,07
NET,08
NET,09
OBS3549[
__BANDMAIL
EMSACD,INC
MPAHL,02
NET,10
NET,11
WDNUSER,08
AERO%FOIL
________DEMO
KALE@WIFE
"V
MCCLPLAN___I
T.+OAK+MAIN+
UBAYTEST,25
DEMOV7_____K
____ET11
*4RAW_ACCT
ATLANTA,03
WDNUSER,01
WDNUSER,11
COAT!RACK
DE+MOTION
MAD%HOUSE
OCLCKUK%
BRUSH%OFF
WDNUSER,06
_BKMAIL
TLINK,02
_CHMAIL
ID=EOGRAM
A_B_C
FHWA08HAMSD@
C25+HB072
DSHEARER,02
CDW2RPTS___Q
IDEAL!ISE
XEN%OGENY
MIKLOVIK,02
FAL@SETTO
NIAGRA,05
MINN,05
___M3MAIL
UNIONBAY1,25
UNIONBAY2,25
UNIONBAY3,25
UNIONBAY4,25
UNIONBAY5,25
UNIONBAY6,25
BTEST;
AD@JUSTOR
BIGELOW,02
DAIABCV2___P
WDNUSER,04
'IPBN260NSS
ARTS,02
JFERRIS,02
BAKER,11
%AFTS01
NOMAIL__1IS
@AGEDGE0054
EDGEBK151^s3
KEEP@SAKE
ALOE,02
GONEIL,02
KER+CHIEF
RLOVE,02
EBEAN,02
GVANLIEW,02
WDNUSER,02
__CPMAIL
SCVAX,02
SHUSTON,02
BJ'STOM##
GROLOFF,02
CLAM+BAKE
$%
RPWARD,02
DAIRY@MAN
____NABGCSDM
HIDEA+WAY
CRESCENTTOM%
VMONELL,02
ROUSES,05
MONEY?#1
PORTLAND,01
OR23112@
WDNUSER,05
WDNUSER,15
INT'L
%WAYNE
__CLCLINCMAI
WDN,86
EDGE^hK22019
HALF@BACK
%AFTS02
RCHINET,06
GEOLO%GER
RMCB21'
WINDSOR,05
TBS)P1
MUDANL summary, legal characters: "#$&*-./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

char	first	middle
 ^h	0	1	; EDGE^hK22019
 ^s	0	1	; EDGEBK151^s3
 ^x	0	2	; IDJL^x^x052DC
   	9	37	__BANDMAIL ________DEMO ____ET11 _BKMAIL _CHMAIL ___M3MAIL __CPMAIL ____NABGCSDM __CLCLINCMAI; __BANDMAIL _
  !	0	2	; COAT!RACK IDEAL!ISE
  "	1	0	"V;
  #	142	467	##TOM## #DRPC2J ##TOM5608 ##ROLM2## ##SECPAC## ##ADMIN## ##GEN-TOM ##1227 ##4978 ##4706 ##PAT ##2980 ##2981
  $	67	117	$HQ25940 $DT2 $SZ017330 $ADVISOR $HQ25990 $WT00180 $WT00280 $WT00380 $WT00480 $WT00580 $ET60180 $ET60280 $E
  %	3	8	%AFTS01 %WAYNE %AFTS02; AERO%FOIL MAD%HOUSE OCLCKUK% BRUSH%OFF XEN%OGENY $% CRESCENTTOM% GEOLO%GER
  &	0	359	; C&L.AUDL06 C&L.175.EPAS C&L.AUDL26 C&L.AUDL36 C&L.AUDL56 C&L.AUDL86 S&P02 C&L.CAAGSYRA C&L.672.MCSC C&L.2
  '	2	3	'OCLCYSQ 'IPBN260NSS; BJ'STOM## INT'L RMCB21'
  )	0	1	; TBS)P1
  *	44	19	*6FT *6LT *6LC *6NEWS *4RAW_ACCT *6LM *2TTY *2TRR * *6QR *4721477 *6LA *6635562 *6LF *7710114 *7MARC *6WSD2
  +	0	8	; T.+OAK+MAIN+ DE+MOTION C25+HB072 KER+CHIEF CLAM+BAKE HIDEA+WAY
  ,	0	58	; OLYMPIA,04 DBUG,06 WDNUSER,03 JARVIS,GW TADAMS,02 NET,01 NET,02 NET,03 NET,04 NET,05 NET,06 NET,07 NET,08
  -	0	766	; AO-OKE AO-OKN AO-OKW WEA-NPB FAAWP-MIS400 N42-9396 N32-7524 N58-6423 N27-8924 N23-4601 FAAAC-IAPASP AO-ME
  .	0	3798	; TRT.2284 FO887.YARDLY CLEVELAND.TI PFE.MST.CZS I75.9337 T.AUSJONES C&L.AUDL06 C&L.175.EPAS C&L.AUDL26 C&L
  /	1	454	/A1/; OMN/AMA 81534/FMEM1 DEC/LAX TDFPHL3/MX PEO/LBL AFA/ALM TDFPHX3/MX PAUL/MX SCRI/SOBELL N/AARG293N/A SR
  0	2717	54646	00058100 01SPA1210GB 01SPA1211GB 01SPA1215GB 01SPA1219GB 060PLX012TJH 0029180002 001CTL030TO 07FPRS 01CY116
  1	634	43687	16260TX 126C860 1642 101C835 1X25MINN 1WUI200FJ 151401 1467 1WUI001LAT 1WUI455BP 1WUI465BP 139901 1WUI485BP
  2	332	31393	2FRPT2040BA 21034 26JP24041OC 26JP24043OC 26JP24049OC 2406 2426 2466 2721RCA1 2CAT840905 2X25NYPDC 2470WALS
  3	225	23615	3CNL001IT 3106002060 3704CHAN 3610 39S914 30801AD 3OTH3019BA 35CFON5238 3763BOSTON 30WOODSTOCK 3410DOUGLA 3
  4	153	20044	4CAT863605 4CTI 49C81I 4CAT863705 43IXSA9162 4CAT861005 4CAT863805 40CARES9086 4CBCC05S 4SCOREX 41CARES9086
  5	122	19745	52CATDN003 52CATHK006 52BCC40S 52CATAU001 52CATFR004 52CATIT002 5207501 50$BONDS 59XPSD3380 5ABCC69S 505300
  6	32	16589	668RCA1 669RCA1 65FRANCEHQ 66FRANCEHQ 6IS/DAL 651MUS 6SEEDSMPDC 61UA08045 60WBSG9086 615PUP 63FRAMPHILL 64F
  7	356	15325	71201034C 71201134T 71201082C 71201182C 707TEJ 71201035C 71201135C 71201083C 71201183C 70803796RT 70C828 70
  8	153	14904	81534/FMEM1 852E9ZAY 866VJ2HM 857AREN010 8510AREN007 857F4LMR 81534/FBOS1 877FTH7Z 81534/FRCH1 859AREN005 8
  9	609	14431	99213321 99201081 95000311 90465226 90014602 90465274 90552512 97881 901132 907884 9FDP 904137 908139 90161
  ;	0	1	; BTEST;
  =	0	1	; ID=EOGRAM
  ?	0	1	; MONEY?#1
  @	1	8	@AGEDGE0054; KALE@WIFE FHWA08HAMSD@ FAL@SETTO AD@JUSTOR KEEP@SAKE DAIRY@MAN OR23112@ HALF@BACK
  A	4472	43601	ALB13 AO-OKE AND11 ADC#1 ASNL001CS AZRO021348 ASO81 AO-OKN AYN501B ASEAHAFO ANS86 AO-OKW AFTAC16 AKLEINMAN 
  B	3231	13157	BU948S BAYCD153 BQRB036728 BUSPLN BEKINSCOMP BCE5PCO BMCSDEMOCA BOS00 BENHAM02 BU296S BU221S BSTRONG BEKINS
  C	6536	38997	CCCATL COPHMC CANDM CATRACHO CORT385NL CLEVELAND.TI CHIC16061255 CHIC16060257 COLORS CAB1E0 COLOSCI CSIDT83
  D	5187	17248	DA1OP DEN03011 D502238NE DOBDA09400TY DACNA549 DUJ011 DS DIA2419 DDTCWLONDON DHUB3426 DHUB3526 DEC/LAX DUPE
  E	5438	27362	EMSMTC ENFEOPS4 ES516DB EMSTYM ER897CSEA EMSMTS ER169X EMSSUPC ES119RN ENARG028NET EE276 ES219RN ENARG428NE
  F	4042	7066	FHWA10RGHST FTS10098A FTS10023A FO887.YARDLY FHWA09HDAHI F22 FG04605 FUT1 FTS10071A FREMONTDOC FRESSEN FHAC
  G	2814	9458	GELCOMIA9 GSG9BRA GS10LECA GIBAH05304TT GSG2MC062334 GSG1119 GL101 GS10GLCA01 GOODSHP GSG9ENM GS10GRCA01 GE
  H	2508	9479	HARTDIAL HOU90999 HAKUTO22 HOLCMT HNDDTMO HACATC HNICOLI HOUSROMCO HKDDA HACATI HS24 HACATO HGPHERBALIFE HU
  I	2617	20085	IUCBREW I75.9337 INDCOMH IMMBOX INRCA238RA IFARGTESTC IDDG501DC IURYPIE IPMIDEMORP IFARG098C ICT00062 IDPR0
  J	719	4488	JJESDP02 JWJJDOE JUSTTESTING JHFPEMAMIT1 JC101 JESHORE JUSTTESTING2 JWAXTEST1 JHUNTER JIMKING JHARRIS JFOST
  K	735	5074	K0 KCSHN KBOWERS KMTP013WCL KTBOSTTS KOPC001DAK KOPC011DAK KMEGGOE KARASEK KRAFTMENU KDDVIARCA KPRIEST KKIA
  L	2789	21214	LROA383WNRS LRJP21510RS LEWIS LEXIS LOPC06060041 LROA108WNRS LROA128WNRS LROA168WNRS LWEINER LSDGIIC01 LSDG
  M	4939	20418	MASONREP3 MASONREP4 MASONREP5 MASONREP6 MOXON MASONREP7 MBTC03 MASONREP8 MMESIMAIL MASONREP9 MASSHSP MN114 
  N	3833	21782	NYJP23890TI NWBJ9L02 N1PADEE N1KEITH NRF28050A NRF28150A NRF28350A NRF28650A NEMOBOX NETEL NELGNR1 NAPLANT 
  O	2781	17468	OILTEST OCLCANW OCLCAPW OTC2405S25 OCLCZAB OTC2405S75 OCLCZCB OTMN004JY OMN/AMA ORPORT OVLAEDI OVGHOSP OVER
  P	8643	17307	PCATAZE PCEE51K P03PRIN PCSMAIL P02CHEM PCFI7RB PCGS2N1 PCMR2J2 PCSA2DC PR255 PMD5PCO PR355 PCMK2L9 PR455 P
  Q	309	1915	QTMN002JY QUICKEL QD00021 QD00031 QDA0001 QALIBSYM QDB0001 QDA0011 QDA0021 QDA0041 Q7CNA346 QOUTRON04 Q6WIT
  R	5029	28209	RNP63527 R61500 R001 RAPRCHS2 RVDOE RCCC0019ET RAPR225 RCAA00001PT RCAA00002PT RCAA00003PT RCRMZLX2ET RSHAW
  S	7249	40598	SBZ101DP SFO07406 SFO07506 SPC4951 STVC03481371 SHOWASF SDOA409WNC1 SEEYAS SDOA419WNC1 STCCAT92 SICS SOUTHE
  T	10602	27380	TFSC12 TFSC13 TFSC14 TFSC15 TRT.2284 TSAA47592RC TRNETM TYMNET2545 TEXAS TYM4000 TESTTL58 TSDDESIGN TSDQVC 
  U	4455	10749	USGM785IS2 UTX00054 UTX00154 U191GEZ U129TSTJ U272ALR UAEMAIL U125GXV U133GWX UCSLX USA223COMP UGFMFILE USA
  V	981	5324	V3FTORD VI525 VI725 VI373 VI473 VTRIMAR VI673 VPSALES V3DETIVB VMMI001NVM VS590I V3CCL VADATS.OOC V3FESA V4
  W	1544	7144	WESTVALC WEA-NPB WRD4512 WRD4522 WESTDCMAIL WDC07364 WESTPUBMN WYLEDI WRD4387 WLCOM0 WLCOM1 WLCOM2 WUICAL W
  X	1211	5392	XDPDL010 XX05000022YY XDRA8106 X2506219 XOPERBAH X2H2SC X2H2SF X2H2SH X2H2SJ X2505468 X75.3210 X2H2SP X2507
  Y	98	5927	YRKHOSP Y9TIR YASUMA YAMAHAMAIL YOKOHAMA YPGS21301 YPR5EM YOKOSA YONEKURA1 YR1MAIL YOLOCTY YPGS20801 YOKOSU
  Z	274	1297	ZZ1220 ZPCOSAC ZZ2700 ZEZ9LM ZBJHD01 ZHG7BQ ZCBDA00300TY ZHR7BZ ZZ1230 ZIROH Z2541010000X ZFK1XY ZYMCOM2I Z
  [	0	1	; OBS3549[

116 illegal entries out of 98639 total entries.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Fri, 19 Dec 86 22:08:15 UT
From: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Date: 19 DEC 86 13:56:00 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I71338@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Numbering Plan 
numberingplan: 
SWBNumberingPlanContract: 

to:  David Chang
fr:  Su Heisler
re:  numbering plan
cc:  Neil Sullivan
     Bill Soley

We do not have a signed agreement for the Numbering Plan from swbt yet.  I
expect it before 12/31/86.  SWBT is requesting a delivery date of 1/1/88.  
We will be receiving a letter of agreement which I will forward to all
of you for approval.  In the meantime, I suggest we do everything we can
to get ready for this project so we can meet the requested date.




Received: from B39.Tymnet by EMSNTD.Ontyme.Tymnet; Tue, 16 Dec 86 11:50:47 PST
From: David Chang <DChang@B39.Tymnet> 
Date: Tue, 16 Dec 86 11:44:43 PST 
To: Su Heisler <TYM.SU/BLU@Ontyme.Tymnet> 
Cc: Bill Soley <WRS@B39.Tymnet> 
Subject: SWB Numbering Plan Contract 

Su,

I had heard a rumor that the SWB Numbering Plan contract was to be signed
on December 15, 1986.  However, I did not hear any followup.  I am requesting
status on this project.  Who is waiting for what??

Thank you.

David
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Mon, 29 Dec 86 22:25:12 UT
From: NETS.M/CADY@EMSNTD.Ontyme.Tymnet 
Date: 29 DEC 86 13:41:34 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I72934@Ontyme.Tymnet 
Keywords: SWBT 
Subject: NUMBERING PLAN FOR SWBT 

TO:  NEIL SULLIVAN
FR:  SU HEISLER
RE:  NUMBERING PLAN FOR SWBT
CC:  FORREST HICKS
     MIKE CADY
     PAUL HIDY

I NEED TO TALK TO YOU ABOUT THIS PROJECT.  I EXPECT THE LETTER REQUESTING
DELIVERY OF THIS SOFTWARE IN A COUPLE OF DAYS.  IT SHOULD BE BASICALLY WHAT
WE'VE AGREED TO WITH ONE EXCEPTION --  SWBT IS GOING TO REQUEST SUPPORT
OF THE NUMBERING PLAN FOR THE BSC PROTOCOL.  THIS IS NOT SOMETHING WE'VE 
AGREED TO DO IN THE PAST.  HOWEVER, THIS REQUEST DOES PRESENT SOME INTERESTING
ALTERNATIVES:

1.  IF WE DON'T WANT TO PURSUE SUPPORT AT THIS TIME WE MERELY SAY NO AND
STICK WITH THE STATED 12 MONTH ARO AND $120K.

2.  IF WE WANT MORE TIME FOR THE ENTIRE PROJECT WE SAY WE'LL STUDY THE 
REQUEST BUT IT WILL DELAY THE MAIN PROJECT.  NO COMMITMENT ON TIME OR 
PRICE MADE OR RECINDED.

3.  IF WE KNOW WE WANT TO GET INTO THIS PORTION BECAUSE IT IS RELATIVELY
MINOR, WE AGREE TO DO THE WORK AT DIFFERENT ARO TIME AND PRICES.  

4.  IF WE WANT TO BE REALLY GOOD GUYS, WE DO THE WORK FOR THE SAME PRICE
IN THE SAME TIMEFRAME.

5.  IF IT WOULD BENEFIT US IN SOME OTHER WAY TO ACCEPT THIS PROJECT, WE
DO SO AND ADD TIME AND COSTS TO THE QUOTE ON THE TABLE.  OTHER BENEFITS
MIGHT INCLUDE THE AUTHORIZATION FOR MORE NTD PEOPLE, INCREASED 1987 REVENUE
OR INCREASED SUPPORT PERSONNELL.

I DON'T KNOW WHAT OUR POSITION SHOULD BE.  I SUGGEST POSITIONS 1 AND 3 AS
THE MOST LIKELY.  SWBT IS REQUESTING THIS BECAUSE ASYNC, X.25 AND BSC USERS
ARE ON THE NET TODAY.  TO HAVE ONE LOGON PROCEDURE FOR ASYNC AND X.25 AND 
ANOTHER FOR BSC WOULD BE CONFUSING AND DIFFICULT TO ADMINISTER.

PLEASE LET ME KNOW WHAT YOU THINK.  AS IS SAID, I EXPECT THE LETTER NO 
LATER THAN JAN 9.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 29 Dec 86 16:51:23 PST 
To: dchang 
Keywords: SWBT 
Subject: NUMBERING PLAN FOR SWBT 

(begin forwarded message)
----------------------------------------------------------------
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Mon, 29 Dec 86 22:25:12 UT
From: NETS.M/CADY@EMSNTD.Ontyme.Tymnet 
Date: 29 DEC 86 13:41:34 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I72934@Ontyme.Tymnet 
Subject: NUMBERING PLAN FOR SWBT 

TO:  NEIL SULLIVAN
FR:  SU HEISLER
RE:  NUMBERING PLAN FOR SWBT
CC:  FORREST HICKS
     MIKE CADY
     PAUL HIDY

I NEED TO TALK TO YOU ABOUT THIS PROJECT.  I EXPECT THE LETTER REQUESTING
DELIVERY OF THIS SOFTWARE IN A COUPLE OF DAYS.  IT SHOULD BE BASICALLY WHAT
WE'VE AGREED TO WITH ONE EXCEPTION --  SWBT IS GOING TO REQUEST SUPPORT
OF THE NUMBERING PLAN FOR THE BSC PROTOCOL.  THIS IS NOT SOMETHING WE'VE 
AGREED TO DO IN THE PAST.  HOWEVER, THIS REQUEST DOES PRESENT SOME INTERESTING
ALTERNATIVES:

1.  IF WE DON'T WANT TO PURSUE SUPPORT AT THIS TIME WE MERELY SAY NO AND
STICK WITH THE STATED 12 MONTH ARO AND $120K.

2.  IF WE WANT MORE TIME FOR THE ENTIRE PROJECT WE SAY WE'LL STUDY THE 
REQUEST BUT IT WILL DELAY THE MAIN PROJECT.  NO COMMITMENT ON TIME OR 
PRICE MADE OR RECINDED.

3.  IF WE KNOW WE WANT TO GET INTO THIS PORTION BECAUSE IT IS RELATIVELY
MINOR, WE AGREE TO DO THE WORK AT DIFFERENT ARO TIME AND PRICES.  

4.  IF WE WANT TO BE REALLY GOOD GUYS, WE DO THE WORK FOR THE SAME PRICE
IN THE SAME TIMEFRAME.

5.  IF IT WOULD BENEFIT US IN SOME OTHER WAY TO ACCEPT THIS PROJECT, WE
DO SO AND ADD TIME AND COSTS TO THE QUOTE ON THE TABLE.  OTHER BENEFITS
MIGHT INCLUDE THE AUTHORIZATION FOR MORE NTD PEOPLE, INCREASED 1987 REVENUE
OR INCREASED SUPPORT PERSONNELL.

I DON'T KNOW WHAT OUR POSITION SHOULD BE.  I SUGGEST POSITIONS 1 AND 3 AS
THE MOST LIKELY.  SWBT IS REQUESTING THIS BECAUSE ASYNC, X.25 AND BSC USERS
ARE ON THE NET TODAY.  TO HAVE ONE LOGON PROCEDURE FOR ASYNC AND X.25 AND 
ANOTHER FOR BSC WOULD BE CONFUSING AND DIFFICULT TO ADMINISTER.

PLEASE LET ME KNOW WHAT YOU THINK.  AS IS SAID, I EXPECT THE LETTER NO 
LATER THAN JAN 9.
----------------------------------------------------------------
(end forwarded message)

From: David Chang <DChang@B39.Tymnet> 
Date: Wed, 31 Dec 86 16:07:30 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Cc: Omar Serang <OSERANG@B39.Tymnet> 
Keywords: SWBT 
Subject: swb 

Bill,

Since I will be gone during the month of January, Omar will be taking
my place concerning any technical aspects of the SWB project.  Please
copy him in all your correspondence.  Thanks.

David
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 6 Jan 87 19:09:59 UT
From: NTD.W/EUSKE@EMSNTD.Ontyme.Tymnet 
Date: 06 JAN 87 10:15:46 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I74759@Ontyme.Tymnet 
Keywords: SWBT 
Subject: SWB Num Plan 
NUMBERINGPLANFORSWBT: 

Gazel/Bill

Are all the SWB projects estimated, scheduled, and ready to start.
How about BSC (attached).

Bill E.
========================================================================


TO:  NEIL SULLIVAN
FR:  SU HEISLER
RE:  NUMBERING PLAN FOR SWBT
CC:  FORREST HICKS
     MIKE CADY
     PAUL HIDY

I NEED TO TALK TO YOU ABOUT THIS PROJECT.  I EXPECT THE LETTER REQUESTING
DELIVERY OF THIS SOFTWARE IN A COUPLE OF DAYS.  IT SHOULD BE BASICALLY WHAT
WE'VE AGREED TO WITH ONE EXCEPTION --  SWBT IS GOING TO REQUEST SUPPORT
OF THE NUMBERING PLAN FOR THE BSC PROTOCOL.  THIS IS NOT SOMETHING WE'VE 
AGREED TO DO IN THE PAST.  HOWEVER, THIS REQUEST DOES PRESENT SOME INTERESTING
ALTERNATIVES:

1.  IF WE DON'T WANT TO PURSUE SUPPORT AT THIS TIME WE MERELY SAY NO AND
STICK WITH THE STATED 12 MONTH ARO AND $120K.

2.  IF WE WANT MORE TIME FOR THE ENTIRE PROJECT WE SAY WE'LL STUDY THE 
REQUEST BUT IT WILL DELAY THE MAIN PROJECT.  NO COMMITMENT ON TIME OR 
PRICE MADE OR RECINDED.

3.  IF WE KNOW WE WANT TO GET INTO THIS PORTION BECAUSE IT IS RELATIVELY
MINOR, WE AGREE TO DO THE WORK AT DIFFERENT ARO TIME AND PRICES.  

4.  IF WE WANT TO BE REALLY GOOD GUYS, WE DO THE WORK FOR THE SAME PRICE
IN THE SAME TIMEFRAME.

5.  IF IT WOULD BENEFIT US IN SOME OTHER WAY TO ACCEPT THIS PROJECT, WE
DO SO AND ADD TIME AND COSTS TO THE QUOTE ON THE TABLE.  OTHER BENEFITS
MIGHT INCLUDE THE AUTHORIZATION FOR MORE NTD PEOPLE, INCREASED 1987 REVENUE
OR INCREASED SUPPORT PERSONNELL.

I DON'T KNOW WHAT OUR POSITION SHOULD BE.  I SUGGEST POSITIONS 1 AND 3 AS
THE MOST LIKELY.  SWBT IS REQUESTING THIS BECAUSE ASYNC, X.25 AND BSC USERS
ARE ON THE NET TODAY.  TO HAVE ONE LOGON PROCEDURE FOR ASYNC AND X.25 AND 
ANOTHER FOR BSC WOULD BE CONFUSING AND DIFFICULT TO ADMINISTER.

PLEASE LET ME KNOW WHAT YOU THINK.  AS IS SAID, I EXPECT THE LETTER NO 
LATER THAN JAN 9.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 6 Jan 87 16:08:14 PST 
To: NTD.W/EUSKE@EMSNTD.Ontyme.Tymnet 
Cc: Gazel Tan <GAZEL@B39> 
Keywords: SWBT 
Subject: Re: SWB Num Plan 
In-reply-to: I74759@Ontyme.Tymnet of 06 JAN 87 10:15:46

To my knowledge, all the sup stuff is scheduled.  As for the other units:
async, x.25 and they've just brought up BSC which was not originally part
of phase I, I don't know the status.  I'm sure BSC knows nothing of this.
This whole thing really needs a coordinator/manager to get all the units
together and keep them informed.  Allyn can help with scheduling, but I'm
not sure that's enough.

Pat Driscoll, Dennis Ernst and Adele Newman have all been informed about
what needs to be done on the X.25 end and have promised its all under control.

Chris Black did likewise for async, but that may have been lost in the
shuffle.

BSC, as I say, was never planned to be part of phase I.

-Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Wed, 7 Jan 87 10:25:46 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Syntax Change 

Bill,

Thanks, it's the first office I've had and it's in a good neighborhood!

This afternoon around 1400 should be fine, whenever.

Thanks...Omar

From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 12 Jan 87 10:44:23 PST 
To: wwang, dchang, Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Logon parsing, transit and options fields 

Weyyi,

After discussion with Bill Soley last week regarding the new logon parsing
the following was agreed upon:

The equal sign "=" will be used as we discussed, to precede a transit field
entry, and the colon ":" is only used for the start of the destination field.
The transit field entries may be of the forms we discussed, "-DNIC" indicating
the RPOA, "xxxxx" a one to five digit gateway host number, and "hostname", only
the RPOA portion being currently supported.

The options field is delimited by opening and closing parenthese as
"(options)".  Apparently this has been Bill Soley's intention all along
but I missed this point and wanted to make sure it is clarified.  The options
field may contain any arrangement of legal characters, presumably fairly
large, though I'm not sure how many characters maximum.  Their interpretation
will allow the user to specify new logon information and old style options
without the use of control characters, in a manner that is transparent to
old interfaces, passing as normal logon characters.  This will allow old
interfaces to take advantage of new features, by specifying the new
information as logon characters in the options field.  For the first
pass we will probably just gather up the characters and keep them in the
logon list to be sent out with the needle bound for the circuit termination.

Bill will redraw a syntax diagram to illustrate these points and I will copy
everybody when I receive it.

Thanks...Omar

Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 13 Jan 87 21:10:04 UT
From: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Date: 13 JAN 87 13:05:49 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: ENS.NS@Ontyme.Tymnet, TYM.SU/BLU@Ontyme.Tymnet, NTD.W/EUSKE@Ontyme.Tymnet,
	NTD.W/SOLEY@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet 
Message-id: I77875@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Numbering Plan for SWBT 
NumberingPlan: 

SUBJ: Numbering Plan for SWBT


 
 
 
 
 
     
     
     This is  a retype  of an  ontyme sent  to Neil  Sullivan on December 29,
     Ontyme #I72734.
     
     To:  Neil Sullivan
     Fr:  Su Heisler
     Re:  Numbering Plan
     Cc:  Forrest Hicks
          Mike Cady
          Paul Hidy
     
     I need  to talk  to you  about  this  project.    I  expect  the  letter
     requesting delivery  of this software in a couple of days.  It should be
     basically what  we've agreed  to with  one exception -- SWBT is going to
     request support of the Numbering Plan for the BSC protocol.  This is not
     something we've  agreed to  do in  the past.  However, this request does
     present some interesting alternatives:
     
     1.  If we don't want to pursue support at this time we merely say no and
     stick with the stated 12 month aro delivery for $150K.
     
     2.   If we  want more time for the entire project we say we'll study the
     request but  it will  delay the  main project.  No commitment on time or
     price made or recinded.
     
     3.  If we know we want to get into this portion because it is relatively
     minor, we agree to do the work at different aro time and delivery price.
     
     4.  If we want to be really good guys, we do the work for the same price
     in the same timeframe.
     
     5.   If it would benefit us in some other way to accept this project, we
     do so  and add time and costs to the quote on the table.  Other benefits
     might include  the authorization  for more  NTD people,  increased  1987
     revenue or increased support personnel.
     
     I don't  know what  our position should be.  I suggest positions 1 and 3
     as the most likely.  SWBT is requesting this because async, X.25 and BSC
     users are  on the  net today.  To have one logon procedure for async and
     X.25 and another for BSC would be confusing and difficult to administer.
     
     Please let  me know  what you  think.  As I said, I expect the letter no
     later than Jan 9.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 13 Jan 87 16:03:32 PST 
To: Gazel Tan <GAZEL@B39> 
Keywords: SWBT 
Subject: SWBT contract 

The SWBT contract is supposed to be here today and I'll be in late.
If you get a chance, please leave a copy in my mailbox.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 14 Jan 87 14:28:00 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: David Chang <DChang@B39.Tymnet>, Romolo Raffo <RAFFO@B36.Tymnet>, Wayne
	Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet>, Weyyi Wang <WWANG@B39.Tymnet>,
	Omar Serang <OSERANG@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>,
	Gazel Tan <GAZEL@B39>, INTL.VIDA@ONTYME.TYMNET,
	NSMS.B/FULTZ@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Re: Punctuation Characters in Usernames 
In-reply-to: NSMS.B/FULTZ@Ontyme.Tymnet message of 10 JAN 87 17:20:18

The reason we are restricting the use of punctuation in user names is
because we are expanding the login string syntax.  Other delimiters are
required for other fields such as transit hosts, X.121 addresses, RPOA,
call user data, options and probably more in the future.

In particular, the character `blank' is possibly negotiable as an embedded
character, but not as a leading or trailing character.  The character `@'
is used as an address field delimiter by many other networks -- it is in
the interest of providing compatability in the future (as internetworking
becomes more practical) that we are reserving this character at this time.

I'm sure `@' should not be a problem, since there are currently only 9 user
names in the public network which contain an `@'.  All of these are inhouse
(ih), and most are in the same GAN (50325).

        HALF@BACK       50325,505142    ih
        FHWA08HAMSD@    36652,561602    ih
        KEEP@SAKE       50325,611372    ih
        AD@JUSTOR       50325,505612    ih
        DAIRY@MAN       50325,611356    ih
        @AGEDGE0054     47255,532576    ih
        KALE@WIFE       50325,560141    ih
        FAL@SETTO       50325,505123    ih
        OR23112@        11216,554642    ih

Bob mentioned that "If we recommend using non-alpha characters, we should
have a full complement to choose from."  Do we in fact so recommend?  If so
why?  If the purpose is for security, that function is to be provided by
the password which will continue to be relatively unrestricted.

Comments please.

-Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Fri, 16 Jan 87 13:53:43 PST 
To: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>, Ken Holcomb
	<NTD.K/HOLCOMB@Ontyme.Tymnet>, Mike Rude <NTD.M/Rude@Ontyme.Tymnet>,
	Betty Jo (BJ) Chang <NTD.B/CHANG@Ontyme.Tymnet>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme.Tymnet>, Dennis Ernst
	<NTD.D/ERNST@Ontyme.Tymnet>, John Kopf <KopfJ@B39.Tymnet>, Dan Lasater
	<DLasater@B39.Tymnet>, Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>,
	Romolo Raffo <NTD.R/RAFFO@Ontyme.Tymnet>, Dick Rawson
	<NTD.D/RAWSON@Ontyme.Tymnet>, Adele Newman
	<NTD.A/NEWMAN@Ontyme.Tymnet>, Dave Smith <NTD.D/SMITH@Ontyme.Tymnet>,
	DChang, Gazel Tan <GAZEL@B39>, Paul Chu <NTD.P/CHU@Ontyme.Tymnet>, Su
	Heisler <TYM.SU/BLU@Ontyme.Tymnet>, Bill Soley <WRS@B39.Tymnet> 
Keywords: TUMS, SWBT 
Subject: SWB contract review 

Hello Folks,

The SWB alleged contract has been reviewed and the document is located
on (OSERANG:39)SWBREV.DOC.  The document is also appended to this TUMS.
This refers to the SWB alleged contract of Jan. 8, 1987.

Thanks...Omar

                                                               January 16, 1987








                                    SWBREV
                          SWB Alleged Contract Review


                            Author:  Omar A. Serang
                          NETWORK TECHNOLOGY DIVISION
                               January 16, 1987










     ********************************************************************
     ***                  PROPRIETARY INfORMATION                     ***
     ***                                                              ***
     ***  MDC-TYMNET's proprietary  rights are included  in the       ***
     ***  information disclosed herein.  The recipient, by receiving  ***
     ***  this document,  agrees that neither this document nor  the  ***
     ***  information disclosed herein nor any part thereof shall be  ***
     ***  reproduced  or  transferred to other documents or used  or  ***
     ***  disclosed  to  others for manufacturing or for  any  other  ***
     ***  purpose  except as specifically authorized in  writing  by  ***
     ***  MDC-TYMNET.                                                 ***
     ********************************************************************



















                            Review of SWB alleged contract               SWBREV
                                                               January 16, 1987







                                   TABLE OF CONTENTS

        Section                                                            Page



        1.   Review of Attachment A                                           1


        2.   Review of Attachment B                                          18







































                            Review of SWB alleged contract               SWBREV
        Review of Attachment A                                 January 16, 1987






                              1 -  Review of Attachment A

        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        01  R  N  N  Y  N

                         Terminal  to host  calling is  currently included
                         for  all   supported  interfaces   and  supported
                         terminals.  Types of terminals and hosts  must be
                         explicitly specified by SWB.

        02  R  N  N  Y  N

                         Terminal   to  terminal   calling   is  currently
                         included   for  all   supported   interfaces  and
                         supported terminals.  Types of terminals  must be
                         explicitly specified by SWB.

        03  R  N  N  Y  N

                         Host to  host calling  is currently  included for
                         all supported interfaces.  Types of hosts must be
                         explicitly specified by SWB.

        04  R  N  N  Y  N

                         Host  to terminal  calling is  currently included
                         for  all   supported  interfaces   and  supported
                         terminals.  Types of terminals must be explicitly
                         specified by SWB.














        SWBREV              Review of SWB alleged contract                    1
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        05  N  N  N  Y  Y

                         An NTN can  be associated with multiple  ports in
                         the supported terminal interfaces.   For CONSATs,
                         specification  of NTN  is done  in  the interface
                         slot Tymfile and  allows an NTN to  be associated
                         with  a  range  of ports.   The  port  ranges may
                         overlap. An NTN  can be associated  with multiple
                         lines in  the supported X.25/75  interfaces.  For
                         X.25,  specification  of  NTN  is  done   in  the
                         interface slot  Tymfile and allows  an NTN  to be
                         associated  with  a  range  of  lines.   The line
                         ranges may overlap.

        06  R  N  N  Y  Y

                         Commercial  traffic  users  within  a   LATA  are
                         confined by Access Profiles (AP) configuration to
                         allow calls that originate and terminate within a
                         LATA and use no extra-LATA carrier as  a transit.
                         Official   usernames   are   set   up   with   AP
                         configurations that allow official circuits to be
                         built that use a carrier outside of the LATA, but
                         still restricts  the origination  and termination
                         points within the LATA.

        07  N  N  N  Y  Y

                         DNIC sharing  is supported  and hostnames  and AP
                         entires   are   used   to    implement   DNPA-DCO
                         intra/inter-LATA routing.















        SWBREV              Review of SWB alleged contract                    2
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        08  N  N  N  Y  Y

                         The  SWB   login  parsing   supports  abbreviated
                         addressing that does not require the requestor to
                         specify  the  DNIC  within  the  destination  for
                         intra-DNIC calls.   The Supervisor uses  the DNIC
                         specified  in the  origination address  (OADR) of
                         the call request, or the Supervisor  native DNIC,
                         to  form the  full 14-digit  address from  the 10
                         digits NTN.  Also implemented is the prefixing of
                         the NTN with 1, where the 1 is ignored an the NTN
                         is expanded a described.

        09  N  N  N  Y  Y

                         The  SWB   login  parsing   supports  abbreviated
                         addressing that does not require the requestor to
                         specify  the  DNIC  within  the  destination  for
                         intra-DNIC calls.   The Supervisor uses  the DNIC
                         specified  in the  origination address  (OADR) of
                         the call request, or the Supervisor  native DNIC,
                         to  form the  full 14-digit  address from  the 10
                         digits NTN.  Also implemented is the prefixing of
                         the NTN with 1, where the 1 is ignored an the NTN
                         is expanded a described.

        10  N  N  W  Y  Y

                         The  implementation   of  DNIC   handling  allows
                         hostnames  entries to  be created  to  handle the
                         1+9+1+NTN,  the 1+9001+NTN,  and  the 1+8+TDC+NTN
                         forms  of  addressing  as  special   case  DNICs.
                         HOWEVER, though the  addressing can occur,  we do
                         not support PSTN, ISDN and TELEX interfaces.












        SWBREV              Review of SWB alleged contract                    3
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        11  N  N  W  Y  Y

                         The  implementation   of  DNIC   handling  allows
                         hostnames  entries to  be created  to  handle the
                         1+9+1+NTN,  the 1+9001+NTN,  and  the 1+8+TDC+NTN
                         forms  of  addressing  as  special   case  DNICs.
                         HOWEVER, though the  addressing can occur,  we do
                         not support PSTN, ISDN and TELEX interfaces.

        12  N  N  W  Y  Y

                         The  implementation   of  DNIC   handling  allows
                         hostnames  entries to  be created  to  handle the
                         1+9+1+NTN,  the 1+9001+NTN,  and  the 1+8+TDC+NTN
                         forms  of  addressing  as  special   case  DNICs.
                         HOWEVER, though the  addressing can occur,  we do
                         not support PSTN, ISDN and TELEX interfaces.

        13  R  C  W  N  Y

                         Not   interpreted.   Discarded   without  further
                         clarification.

        14  N  N  N  Y  Y

                         The SWB login string parsing  supported character
                         absorption which ignores the characters -,  (, ),
                         and + when embedded in an NTN or IDN.

        15  N  N  N  Y  Y

                         12 character  usernames are  currently supported,
                         which satisfies the  requirements for a  10 digit
                         NUI.  There may be some changes necessary  in the
                         X.25/75 interfaces.











        SWBREV              Review of SWB alleged contract                    4
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        1N}=d`a"‰&t1Ù,"XAsq	`eMWDO?T(Kô"P*:(        A9kë`2I%r1ó1 )0îC`Äu&&1wb˜jd&D8=h(`Äx0Rh@Â|?B   Õ
#±G	 =4"AêB 	X   ! @H" FA (@ ` RZ    @Pj8
‡c*V< Ç  L` &@#T	Ö4XÄt 3E.wÁH MSB{ÅC%srÉ.@ ¢S12Ì,:v4D)8c`Ms;4""&b?$x$7[$z<ÑH\ z oh91   v/CˆG  Që ÁO*lKHH. )&"tDÑqb8qÑ0	@$bC2(2xD] HœGX (êaO_M Bë U≥`umƒAH `Ap»
Kh0*a@&      D     @Sa P@∑&H@)@9 	< 	.i` *jQ ;g6%7S  zN \4CHp	w  ≈Ha>A!Fúab`-†@0fajE+$  ÅJSA0à4 ®xÖ}A::Q0‰ hAd eT          cmdsize  :  integer;
                          minabb   :  integer;
                          minauth  :  integer;
                          prgname  :  cmdname;
                          seqprg   :  boolean;
                          allowfr  :  boolean;
                          keyoffset,
                          numkey   :  integer
                        end;
!
const

keyword_offset = 0;

alias_keyword_offset       = keyword_offset;
  alias_quasi_keyword      = 1;
  alias_add_keyword        = 1 + alias_quasi_keyword;
  alias_delete_keyword     = 1 + alias_add_keyword;
alias_keyword_count        = alE INTERPRETED 
                 AS '40' - 5A'.
             
             RULE 12. '7B' - '7D' ARE INTERPRETED AS '1B' CAUSING ESCAPE.
             
             RULE 13. '7E' IS INTERPRETED AS '1D' CAUSING ESCAPE.
             
             RULE 14. '7F' IS INTERPRETED AS '00' AND IS IGNORED.
             







        SWBREV              Review of SWB alleged contract                    5
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        17  N  N  W  Y  Y

                         A default RPOA may  be specified for a  user, for
                         inter-LATA  calls,   and  may  be   defaulted  to
                         different  RPOAs  depending upon  which  LATA the
                         call originates.   The default can  be overridden
                         in   the   login  string   transit   field.   All
                         implementation  is  done  in   AP  configuration.
                         WARNING - The state of the Supervisor  MUD design
                         and APX may  not properly take this  into account
                         at this time.

        18  R  N  N  Y  Y

                         NUI   is  currently   transmitted   over  X.25/75
                         facilities.  NUI blocking will be  implemented at
                         the interface slots, which could replace  NUIs or
                         reject  calls  based  upon  incoming  NUIs.   The
                         request is  still unclear regarding  the expected
                         blocking.

        19  R  N  N  Y  Y

                         NUI   is  currently   transmitted   over  X.25/75
                         facilities.  NUI blocking will be  implemented at
                         the interface slots, which could replace  NUIs or
                         reject  calls  based  upon  incoming  NUIs.   The
                         request is  still unclear regarding  the expected
                         blocking.

















        SWBREV              Review of SWB alleged contract                    6
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        20  N  N  N  Y  N

                         CUG is currently implemented using end to end IIX
                         exchange   of   information   by    the   X.25/75
                         interfaces.

        21  R  Y  N  Y  N

                         The request for  "SENT PAID" calling seems  to be
                         satisfied  by our  current normal  call charging.
                         Clarification is requested as to why this item is
                         indicated as not  supported in Phase I,  since it
                         seems that it is currently  implemented.  Perhaps
                         the request is being misinterpreted.

        22  N  Y  W  Y  Y

                         The  current   design  includes   provisions  for
                         collect    calling.    The    implementation   of
                         user/hostnames  that  place  collect  calls  as a
                         default  should be  clarified for  the Supervisor
                         design, and the issue of reject priority  must be
                         clarified.    Also,  this   is   currently  being
                         implemented and it is unclear why the  request is
                         indicated as not being supported in Phase I.





















        SWBREV              Review of SWB alleged contract                    7
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        23  R  N  N  Y  Y

                         The  screening   of  X.25/75  calls   based  upon
                         facilities  data   will  all   be  done   by  the
                         interfaces.   Some  clarification   upon  whether
                         "utilities"   is   really   meant    instead   of
                         "facilities".

        24  N  N  N  Y  Y

                         The use of RPOA is specified in the  login string
                         transit field.

        25  N  N  W  Y  Y

                         The  use  of a  default  is specified  in  the AP
                         configuration  for  the user/host  names  and the
                         default  may be  overridden by  a  user specified
                         RPOA in the login string transit  field.  WARNING
                         - The state of the Supervisor MUD design  and APX
                         may not properly take default RPOAs  into account
                         at this time.
























        SWBREV              Review of SWB alleged contract                    8
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        26  N  N  W  Y  Y

                         The  default  RPOA   is   specified  in   the  AP
                         configuration   for  the   user/host   names  and
                         indicates  a   non-existent  RPOA   gateway  host
                         number, and the  host down message for  that host
                         number  requests the  user to  enter  an explicit
                         RPOA in  the login  request transit  field.  This
                         forces non presubscribed users to specify an RPOA
                         for inter-LATA calls.

        27  N  N  N  Y  N

                         The host  down messages are  used to  indicate to
                         gateway  host  requestors  the  reason  that  the
                         presubscribed   RPOA   is   unavailable,   before
                         clearing the call.

        28  N  N  N  Y  Y

                         The use of strings  in AP entries will  allow the
                         use  of nicknames  for addresses,  thus providing
                         speed calling.

        29  N  N  N  Y  Y

                         The Menu Server code will be modified to  use the
                         new logon design (LOGII) to provide a menu driven
                         logon feature.

















        SWBREV              Review of SWB alleged contract                    9
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        30  N  N  N  Y  Y

                         An NTN can  be associated with multiple  ports in
                         the supported terminal interfaces.   For CONSATs,
                         specification  of NTN  is done  in  the interface
                         slot Tymfile and  allows an NTN to  be associated
                         with  a  range  of ports.   The  port  ranges may
                         overlap. An NTN  can be associated  with multiple
                         lines in  the supported X.25/75  interfaces.  For
                         X.25,  specification  of  NTN  is  done   in  the
                         interface slot  Tymfile and allows  an NTN  to be
                         associated  with  a  range  of  lines.   The line
                         ranges  may overlap.   Since all  origination NTN
                         addresses  are  simply coded  into  the interface
                         assembly,  any  NTN may  be  associated  with any
                         physical port.  This should only be done for NTNs
                         within  a  LATA  to  prevent  OADR administrative
                         problems and confusion.

        31  N  N  N  Y  Y

                         An NTN can  be associated with multiple  ports in
                         the supported terminal interfaces.   For CONSATs,
                         specification  of NTN  is done  in  the interface
                         slot Tymfile and  allows an NTN to  be associated
                         with  a  range  of ports.   The  port  ranges may
                         overlap. An NTN  can be associated  with multiple
                         lines in  the supported X.25/75  interfaces.  For
                         X.25,  specification  of  NTN  is  done   in  the
                         interface slot  Tymfile and allows  an NTN  to be
                         associated  with  a  range  of  lines.   The line
                         ranges  may  overlap.  Since  the  port  and line
                         ranges  may   overlap,  a  port   is  effectively
                         assigned more than one NTN.












        SWBREV              Review of SWB alleged contract                   10
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        32  N  N  N  Y  Y

                         An NTN can  be associated with multiple  ports in
                         the supported terminal interfaces.   For CONSATs,
                         specification  of NTN  is done  in  the interface
                         slot Tymfile and  allows an NTN to  be associated
                         with  a  range  of ports.   The  port  ranges may
                         overlap. An NTN  can be associated  with multiple
                         lines in  the supported X.25/75  interfaces.  For
                         X.25,  specification  of  NTN  is  done   in  the
                         interface slot  Tymfile and allows  an NTN  to be
                         associated  with  a  range  of  lines.   The line
                         ranges  may  overlap.  Since  the  port  and line
                         ranges may overlap, more than NTN is  assigned to
                         one port.

        33  N  Y  W  Y  Y

                         The  BISYNC  3270  interface  was  not originally
                         intended to be part of phase I,  but conceptually
                         poses  no  problems for  integration  to  the new
                         logon  session format  (LOGII).  Once  the BISYNC
                         uses  LOGII   all  pertinent  services   will  be
                         provided.  All other interfaces are  in agreement
                         for phase I provision as indicated.





















        SWBREV              Review of SWB alleged contract                   11
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        34  N  N  W  Y  Y

                         The  assignment  of  NTNs  can  be  done  in  any
                         arbitrary fashion since the are  simply character
                         strings representing MUD entries.   HOWEVER, each
                         NTN  assignment requires  a MUD  entry,  so large
                         unfilled  ranges  of  NTNs  would  lead  to  very
                         inefficient use of the MUD space.

        35  N  N  W  Y  Y

                         The  assignment  of  NTNs  can  be  done  in  any
                         arbitrary fashion since the are  simply character
                         strings representing MUD entries.   HOWEVER, each
                         NTN  assignment requires  a MUD  entry,  so large
                         unfilled  ranges  of  NTNs  would  lead  to  very
                         inefficient use of the MUD space.

        36  N  N  W  Y  Y

                         The  assignment  of  NTNs  can  be  done  in  any
                         arbitrary fashion since the are  simply character
                         strings representing MUD entries.   HOWEVER, each
                         NTN  assignment requires  a MUD  entry,  so large
                         unfilled  ranges  of  NTNs  would  lead  to  very
                         inefficient use of the MUD space.

        37  N  N  N  Y  Y

                         Several  interface hosts  may be  specified  as a
                         possible  targets  for  a single  NTN  in  the AP
                         configuration.   Also,  the  interface  slots may
                         have multiple  ports or  lines associated  with a
                         single  NTN.   This supports  a  hunt  group with
                         individual line addressing based upon the NTN.











        SWBREV              Review of SWB alleged contract                   12
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        38  N  N  N  Y  Y

                         Several  interface hosts  may be  specified  as a
                         possible  targets  for  a single  NTN  in  the AP
                         configuration.   Also,  the  interface  slots may
                         have multiple  ports or  lines associated  with a
                         single  NTN.   This supports  a  hunt  group with
                         individual line addressing based upon the NTN.

        39  N  N  W  Y  Y

                         If the requestor  specifies a destination  in the
                         same  LATA  as the  origination,  yet  requests a
                         transit RPOA to route  the call, the call  may be
                         blocked   or   allowed,   depending    upon   the
                         origination   LATA.    This  is   done   with  AP
                         configuration.    WARNING  -   Multi-based  hosts
                         present  a  problem since  the  same  host number
                         could  reside  in  different  LATAs.   This  is a
                         potential  can of  worms,  and what  SWB  is told
                         regarding  Multi-basd hosts  should  be carefully
                         preconsidered.

        40  N  N  N  Y  Y

                         The  accepting  or  rejecting  of  non-PVC  calls
                         depending upon  presence a  called address  is an
                         interface configured option.  Interfaces  need to
                         be able  to specify  this option  as part  of the
                         Tymfile.
















        SWBREV              Review of SWB alleged contract                   13
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        41  N  N  N  Y  Y

                         The  X.25  interface currently  inserts  a called
                         address for  PVC calls that  do not  specify one.
                         The  CONSAT and  other interfaces  will  have the
                         same  feature  implemented,  in  a  user friendly
                         manner.

        42  N  N  N  Y  Y

                         The ultimate criterion an address is  presence in
                         the MUD.   The interfaces may  check the  form of
                         addresses and  clear the  call if  necessary, but
                         ultimately, if the address is not present  in the
                         MUD, it is de facto an invalid format.   The X.25
                         interface  will  need  some  adjustment  to  it's
                         present  address  checking  to  handle   the  new
                         addressing requirements.

        43  N  N  N  Y  Y

                         The X.25  interfaces perform  end to  end calling
                         address   verification  using   IIX,   and  other
                         interfaces have  the calling  addresses assembled
                         in, providing implicit verification.

        44  N  N  N  Y  Y

                         The  originating  interfaces  always  provide  an
                         origination address if the call request  does not
                         specify one.















        SWBREV              Review of SWB alleged contract                   14
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        45  N  N  N  Y  Y

                         Passwords  are  optional  in  the  MUD   for  any
                         username,   providing   complete   flexiblity  in
                         requiring  passwords  with  direct  access,  dial
                         access and other special types of calls.

        46  N  N  N  Y  Y

                         Passwords  are  optional  in  the  MUD   for  any
                         username,   providing   complete   flexiblity  in
                         requiring  passwords  with  direct  access,  dial
                         access and other special types of calls.

        47  N  N  N  Y  Y

                         Passwords  are  optional  in  the  MUD   for  any
                         username,   providing   complete   flexiblity  in
                         requiring  passwords  with  direct  access,  dial
                         access and other special types of calls.

        48  N  N  N  Y  Y

                         The  called address  is sent  to  the destination
                         interface  in full  IDN form,  without prefixing,
                         and is checked by the destination to  verify that
                         the destination matches the called  address.  The
                         call is rejected if they do not match.


















        SWBREV              Review of SWB alleged contract                   15
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        49  N  N  W  Y  Y

                         Calling addresses of the  form 9001+NXX+NXX+XXXX,
                         can be associated with ISDN interfaces by using a
                         special  case  DNIC,  HOWEVER  there  is  no ISDN
                         interface supported,  even though  the addressing
                         form is supported.

        50  N  Y  W  Y  Y

                         The Supervisor sends the called address specified
                         in  the  login  string  and  the  called  address
                         resulting  from  nickname  translation   or  call
                         redirection,  to the  termination  interface.  If
                         redirection  occured   in  the   Supervisor  call
                         routing  the termination  interface  is notified.
                         WARNING  - Since  strings in  AP entries  are not
                         supported in  Phase I, this  feature is  also not
                         supported in Phase I.

        51  N  N  N  Y  Y

                         The X.25/75 interfaces provide calling and called
                         address verification in the call accepted packet,
                         using end  to end  IIX communications.   Call Re-
                         direction  is  handled  differently  since  a re-
                         directed  calling  address  does  not  match  the
                         original calling address.


















        SWBREV              Review of SWB alleged contract                   16
        Review of Attachment A                                 January 16, 1987


        ITEM NUMBER
        |   REQUEST CLARIFICATION FROM SWB
        |   |  CHANGE OF YES/NO STATUS
        |   |  |  WARNING
        |   |  |  |  ACCEPT THIS ITEM WITH THE ADDITIONAL SPECIFICATIONS
        |   |  |  |  |  CODE CHANGES ARE NECESSARY
        |   |  |  |  |  |

        52  N  N  N  Y  Y

                         The Supervisor always provides calling and called
                         addresses  in  full 14  digit  IDN  form, without
                         prefixes.

        53  R  N  N  N  N

                         Clarification  is  requested  from  SWB regarding
                         what  is  meant by  calling/credit  card service.
                         The  interaction  between  NUI  and  calling card
                         needs   to   be   specified.     Depending   upon
                         interpretation,  this  feature  may  currently be
                         implemented as username/password.
































        SWBREV              Review of SWB alleged contract                   17
        Review of Attachment B                                 January 16, 1987






                              2 -  Review of Attachment B


             The login  syntax is fully  specified in the  attached syntax
        diagram.   The basic  form  is "(options)username=RPOA:Destination
        address;password".   and   differs  syntactically  from   the  SWB
        specification.











































        SWBREV              Review of SWB alleged contract                   18

From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 16 Jan 87 17:06:35 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: David Chang <DChang@B39.Tymnet>, Romolo Raffo <RAFFO@B36.Tymnet>, Wayne
	Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet>, Weyyi Wang <WWANG@B39.Tymnet>,
	Omar Serang <OSERANG@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>,
	Gazel Tan <GAZEL@B39>, INTL.VIDA@ONTYME.TYMNET 
Keywords: SWBT 
Subject: Re: Punctuation Characters in Usernames 
In-reply-to: your message of Fri, 16 Jan 87 14:25:49 PST

Re: PIR for username character restriction...
its all covered by the hostnames and Southwest Bell PIR's
-Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 19 Jan 87 9:10:08 PST 
To: DChang, Rich Teng <RICHTENG@B39.Tymnet>, Weyyi Wang <WWANG@B39.Tymnet>,
	Gazel Tan <GAZEL@B39>, Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Logon Syntax 

Hello Folks,

I would like to have a meeting with Rich, Weyyi, Bill and myself this
week to review and "finalize" the logon syntax for both SWB and Tymnet.

The objective of the meeting is to work from the current syntax diagram
to provide an accompanying text explanation and to isolate the SWB and
Tymnet specific issues, resulting in two separate syntax diagrams and
text explanations.

I will schedule the meeting for Tuesday, Jan. 20,at 1430 in the front
conference room.  I anticipate the meeting will take up to two hours.

Please respond today (Monday) if you have any schedule conflicts so
that I may re-schedule as soon as possible.  It is imperative that
we resolve these issues, since we must be able to communicate this
information clearly to both the customer and throughout the affected
development units.

Thanks...Omar

From: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Date: Fri, 23 Jan 87 16:10:20 PST 
To: David Chang <DChang@B39.Tymnet> 
Cc: Romolo Raffo <RAFFO@B36.Tymnet>, Bill Soley <WRS@B39.Tymnet>, Wayne
	Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet>, Weyyi Wang <WWANG@B39.Tymnet>,
	Omar Serang <OSERANG@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>,
	Gazel Tan <GAZEL@B39>, WRS@C39.Tymnet, INTL.VIDA@ONTYME.TYMNET 
Keywords: SWBT 
Subject: Re: Punctuation Characters in Usernames 
In-reply-to: your message of Thu, 11 Dec 86 17:15:29 PST

PROGRESS REPORT AND QUESTION.
PROGRESS:43 NAMES CAN DE REMOVED IMMEDIATELY, BUT WE NEED NANCY'S HELP-
THEY ARE THE ONES WITH CURRENTLY "ILLEGAL"CHARACTERS THAT WE CANNOT
REACH. THAT LEAVES 73, OF WHICH THERE ARE 58 WITH A COMMA. THAT BRINGS
UP THE QUESTION. YOU DID NOT EXCLUDE THE COMMA, BUT BILL DID INCLUDE
THOSE NAMES ON THE "BAD GUY" LIST OF NAMES. DO WE WANT TO EXCLUDE THE
COMMA??? ONE CUSTOMER USES IT EXTENSIVELY (PROBABLY 50+ BELONG TO HIM).
IF COMMA IS OK, THAT ONLY LEAVES 15 NAMES BELONGING TO ~ 10 CUSTOMERS.
WE (VIDA AND I ) AWAIT AN ANSWER BEFORE WE PROCEED TO GO TO THEM.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 23 Jan 87 19:37:41 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: David Chang <DChang@B39.Tymnet>, Romolo Raffo <RAFFO@B36.Tymnet>, Wayne
	Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet>, Weyyi Wang <WWANG@B39.Tymnet>,
	Omar Serang <OSERANG@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>,
	Gazel Tan <GAZEL@B39>, INTL.VIDA@ONTYME.TYMNET 
Keywords: SWBT 
Subject: Re: Punctuation Characters in Usernames 
In-reply-to: your message of Fri, 23 Jan 87 16:10:20 PST

On comma, I suggest we restrict it to prevent further names from being
validated having a comma, but let the existing names remain.  Are there
any objections to this?  It might make the parsing more difficult since
comma is used in the destination field.  What do you think, Weyyi? Omar?
-Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Sat, 24 Jan 87 9:52:05 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Punctuation characters 

Bill,

I say we get the commas out of the usernames now.  This will prevent
any possible future conflicts and will make the login parsing more
straightforward.  The user that uses all of the commas can use one of
the non-illegal characters as a substitute.

Thanks...Omar

From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 24 Jan 87 22:57:23 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: David Chang <DChang@B39.Tymnet>, Romolo Raffo <RAFFO@B36.Tymnet>, Wayne
	Bartlett <TYM.W/BARTLETT@Ontyme.Tymnet>, Weyyi Wang <WWANG@B39.Tymnet>,
	Omar Serang <OSERANG@B39.Tymnet>, Rich Teng <RICHTENG@B39.Tymnet>,
	Gazel Tan <GAZEL@B39>, INTL.VIDA@ONTYME.TYMNET 
Keywords: SWBT 
Subject: Re: Punctuation Characters in Usernames 
In-reply-to: your message of Fri, 23 Jan 87 16:10:20 PST

As Omar suggests (below), eliminating the comma laced user names now will
simplify things and prevent a possible ``crisis'' campaign to eliminate them
at some future time.  Let's see if we can get the customer to cooperate.

-Bill

(begin forwarded message)
----------------------------------------------------------------
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Sat, 24 Jan 87 9:52:05 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Subject: Punctuation characters 

Bill,

I say we get the commas out of the usernames now.  This will prevent
any possible future conflicts and will make the login parsing more
straightforward.  The user that uses all of the commas can use one of
the non-illegal characters as a substitute.

Thanks...Omar

----------------------------------------------------------------
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Wed, 28 Jan 87 17:17:27 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: login syntax 

Bill,

I need the login syntax complete diagram to generate the examples generation
programming.

Thanks...Omar

From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 29 Jan 87 13:25:30 PST 
To: Omar A. Serang <OSerang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: login syntax 
In-reply-to: your message of Wed, 28 Jan 87 17:17:27 PST

Page 1 is done with all the changes we talked about, page 2 is done, but
needs to be checked, I  think its okay.  It will probably be the weekend
before I get time to plot it unless you consider it a crisis in which case
I can probably get out of the meetings for a while, but I'd prefer not to
if Monday is okay.  Sorry for this unpredictable interruption.  -Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Fri, 30 Jan 87 9:41:48 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Login Syntax 

Bill,

Not to worry, we are not at the crisis stage.  If I can get page one early
next week that will be fine.  Is there any plans for page 2?  If not, you
and I can clarify the page 2 contents for the examples generation.  As soon
as we can finalize this pass (with all items being resolved to circles), I
will not need to involve you any further.

Thanks...Omar

From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Fri, 30 Jan 87 10:55:25 PST 
To: Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>, Ken Holcomb
	<NTD.K/HOLCOMB@Ontyme.Tymnet>, Mike Rude <NTD.M/Rude@Ontyme.Tymnet>,
	Betty Jo (BJ) Chang <NTD.B/CHANG@Ontyme.Tymnet>, Pat Driscoll
	<NTD.P/DRISCOLL@Ontyme.Tymnet>, Dennis Ernst
	<NTD.D/ERNST@Ontyme.Tymnet>, John Kopf <KopfJ@B39.Tymnet>, Dan Lasater
	<DLasater@B39.Tymnet>, Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>,
	Romolo Raffo <NTD.R/RAFFO@Ontyme.Tymnet>, Dick Rawson
	<NTD.D/RAWSON@Ontyme.Tymnet>, Adele Newman
	<NTD.A/NEWMAN@Ontyme.Tymnet>, Dave Smith <NTD.D/SMITH@Ontyme.Tymnet>,
	DChang, Gazel Tan <GAZEL@B39>, Paul Chu <NTD.P/CHU@Ontyme.Tymnet>, Su
	Heisler <TYM.SU/BLU@Ontyme.Tymnet>, Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: LOGII GED 

Hello Folks,

The LOGII GED is ready for final review, incorporating the results
of the previous design review interviews and meeting.  Since the
document has grown 75%, the parts that are THE SAME as the previous
document are outlined, and all UNMARKED PORTIONS ARE UPDATED PORTIONS.

Please help make the most of your prior contributions and give this
document a strict review.  I will check with you each personally
if I do not get a response during the next month, just to make
sure that there is concensus on the design.

The document is available on (OSERANG:39)LG2GED.DOC and marked up
copies are being sent by mail.

Thanks...Omar

From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Fri, 30 Jan 87 11:06:52 PST 
To: Gazel Tan <GAZEL@B39>, Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>, Al Fenn
	<NTD.A/Fenn@Ontyme.Tymnet>, Bill Soley <WRS@B39.Tymnet>, DCHANG, Neil
	Sullivan <ENS.NS@Ontyme.Tymnet> 
Keywords: SWBT 
Subject: Intra-LATA Legality 

Hello Folks,

After some cursory research and investigation into the matter of
intra-LATA legality, Steven Bell of Squire, Sanders and Dempsey,
a law firm under retainer by Tymnet, has written up a letter
stating the legal position on the matter.

The issue of intra-LATA legality involves a circuit with endpoints
confined within a LATA, but with passthrough nodes that reside outside
the LATA.  As stated by Mr. Bell, there has been no clear guidance
on the issue, and at this point Tymnet has taken a stance of allowing
the afore-mentioned scenario to occur, legally.  However, there may
be a time in which Tymnet would take a different standpoint, if
the line between passthrough and processing is crossed.

The scenario presented above indicates the the packet data is actually
confined on an endpoint basis to within a LATA, but may PASS THROUGH a
piece of hardware that is not within the LATA, as part of the route from
origin to destination.  Now if the data was further processed on the
hardware outside of the LATA, like some value added service was provided
by the extra-LATA hardware, Tymnet's standpoint on the issue could be
completely reversed.

In summary, the indication is that a message which originates and terminates
within the same LATA is an intra-LATA communication, even if it briefly and
incidentally transits another LATA.  The issue of transport and interpration
of this data outside the LATA must be watched carefully.

A copy of Mr. Bell's letter is being sent in the mail.

THIS COMMUNICATION IS OF AN EXTREMELY SENSITIVE NATURE AND IS
NOT TO BE GENERALLY DISTRIBUTED.

Thanks...Omar

From: Gazel Tan <GAZEL@C39.Tymnet> 
Date: Fri, 30 Jan 87 16:50:39 PST 
To: Wayne D. Bartlett <BARTLETW@C39.Tymnet> 
Cc: WRS@C39.Tymnet 
Keywords: SWBT 
Subject: Re: "PER BILL'S LAST MEMO, CAN YOU"... 
In-reply-to: your message of Tue, 27 Jan 87 8:45:50 PST

Wayne,

All the work for SWB is under PIR #2214.  We also have a PIR (#1553)
to implement Host names.  As fo the comma, it will be part of the
implementation of Host Names and will be addressed in the GEDs.

Gazel
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 2 Feb 87 10:10:02 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Login Syntax 

Bill,

I looked over the logon syntax, and it looks great.  In the transit field
why is there optional host#, host#, yet not optional hostname, hostname
or -RPOA, -RPOA, -RPOA?  Currently we are only supporting single -RPOA
entries, yet the syntax should provide for multiple -RPOAs as well as
multiple hostnames, seperated by commas.

I will get the examples generation program going right away, it shouldn't
take too long.

Thanks...Omar

From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Wed, 4 Feb 87 10:04:14 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: LOGON SYNTAX 

Bill,

I noticed as I'm doing my example generation that there are no loop counts
indicated for subjects like username (how many characters min and max)?
Is this intentional, since it is not really a syntax limitation or do we
need to indicate the number of items are minimum and maximum.  I will be
using some values for the program (obviously).

Thanks...Omar

Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Fri, 6 Feb 87 16:08:06 UT
From: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Date: 06 FEB 87 07:17:25 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.W/EUSKE@Ontyme.Tymnet, NTD.W/SOLEY@Ontyme.Tymnet,
	NTD.O/SERANG@Ontyme.Tymnet, TYM.SU/BLU@Ontyme.Tymnet,
	NETS.M/CADY@Ontyme.Tymnet, NETS.J/STERN@Ontyme.Tymnet,
	TYM.F/HICKS@Ontyme.Tymnet 
Message-id: I89500@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Numbering Plan Conference Call request #2 
NumberingPlan: 

to:  Bill Euske
     Bill Soley
     Omar Serang
fr:  Su Heisler
re:  Numbering Plan
cc:  Mike Cady
     Jeff Stern
     Forrest Hicks

Can one of you be available to explain the Logon Syntax Diagram to Jerry 
Talken and me on Monday, February 9 at 1 p.m. CA time?  I expect 
this will be a very short call, we just don't understand your loops.

Please respond this morning if possible.  I need to make an appointment
or cancel one based on your response.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 7 Feb 87 1:12:54 PST 
To: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Cc: NTD.W/EUSKE@Ontyme.Tymnet, OSERANG@39.Tymnet, NETS.M/CADY@Ontyme.Tymnet,
	NETS.J/STERN@Ontyme.Tymnet, TYM.F/HICKS@Ontyme.Tymnet, DCHANG@39.Tymnet
	
Keywords: SWBT 
Subject: Re: Numbering Plan Conference Call request #2 
In-reply-to: I89500@Ontyme.Tymnet of 06 FEB 87 07:17:25

I suggest we discuss a more current copy of the login syntax than the one
you have.  I will get the current syntax drawing plotted out and mailed to
you as soon as it final.  It is currently being reviewed, and there is at
least one change so far that needs to be made.  One of us will get back to
you as to when it can be sent out.  Thanks -Bill
From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 7 Feb 87 1:31:10 PST 
To: dchang, oserang 
Keywords: SWBT 
Subject: Su's request 

I think we should make sure this next copy of the login syntax is truely
the final one.  How long do we think we need to be certain there are no
more changes to be made?  It will take me about 2 days to make the changes
I have noted so far.  We will have to agree on maximum field sizes for
each of the fields.  What do you think?  -Bill

----------------------------------------------------------------
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Fri, 6 Feb 87 16:08:06 UT
From: TYM.SU/BLU@EMSNTD.Ontyme.Tymnet 
Date: 06 FEB 87 07:17:25 
To: NTD.W/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.W/EUSKE@Ontyme.Tymnet, NTD.W/SOLEY@Ontyme.Tymnet,
	NTD.O/SERANG@Ontyme.Tymnet, TYM.SU/BLU@Ontyme.Tymnet,
	NETS.M/CADY@Ontyme.Tymnet, NETS.J/STERN@Ontyme.Tymnet,
	TYM.F/HICKS@Ontyme.Tymnet 
Message-id: I89500@Ontyme.Tymnet 
Subject: Numbering Plan Conference Call request #2 
NumberingPlan: 

to:  Bill Euske
     Bill Soley
     Omar Serang
fr:  Su Heisler
re:  Numbering Plan
cc:  Mike Cady
     Jeff Stern
     Forrest Hicks

Can one of you be available to explain the Logon Syntax Diagram to Jerry 
Talken and me on Monday, February 9 at 1 p.m. CA time?  I expect 
this will be a very short call, we just don't understand your loops.

Please respond this morning if possible.  I need to make an appointment
or cancel one based on your response.
------------------------------------------------
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Sat, 7 Feb 87 22:07:28 UT
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 07 FEB 87 14:00:18 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.A/MORRISS@Ontyme.Tymnet, NTD.B/EUSKE@Ontyme.Tymnet,
	NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.D/SMITH@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.R/RAFFO@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.B/SOLEY@Ontyme.Tymnet, NTD.P/CHU@Ontyme.Tymnet,
	TYM.SU/BLU@Ontyme.Tymnet, NETS.R/ALBERTSON@Ontyme.Tymnet 
Message-id: I90056@Ontyme.Tymnet 
Keywords: SWBT 
Subject: SWB Actions 
SWBActions: 

                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      07 FEB 87  13:58

TO>        Sue Heisler

COPIES>    Distribution List (See CC List)

FROM>      Allyn Morriss


SUBJECT>   SWB Actions


-----------------------------------------------------------------------


In response to your concerns last Friday:
 
1. The GED for LOGII has been placed on (DNDSPEC:33) for your review.
   A copy of this may also be place in (TECHREF:56) by Marketing.
   A filename is LG2GED.DOC.
 
2. A written explanation of how to intrepret the schematic has been
   prepared by Omar Serang. It includes diagram symbols with text to
   describe same. This need to be either FAXed to you or sent Federal
   Express.
 
   Please give me:
 
   - Mailing address for Fed/Exp  and/or
 
   - FAX telephone number.
 
3. I will speak to Romulo about RAM format impact (if any) on Monday.
   I will message you with results.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 9 Feb 87 9:47:06 PST 
To: Bill Soley <WRS@B39.Tymnet>, dchang, Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet>
	
Keywords: SWBT 
Subject: Logon Syntax 

Hello Folks,

Regarding the SWB logon syntax;  I hope that the syntax explanation will
hold them off for a couple of days to allow us to finalize the diagram.

As far as I know, the syntax is good as it stands, we must simply finalize
loop counts and include a loop for the transit field to allow multiple
entries.  A "Note 3" will indicate that the loop count for the overall
transit field loop is currently only 1, which will also be indicated in
the loop count fields.  I will attempt to come up with meaningful loop counts,
or at least character counts, for each field.

Thanks...Omar

From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 9 Feb 87 10:02:57 PST 
To: Bill Soley <WRS@B39.Tymnet>, dchang 
Keywords: SWBT 
Subject: Logon Syntax 

Bill,

I will research the loop counts for the logon syntax.

The IGNORES description has a bug.

We need a description for NODE-NUMBER.

We should indicate a loop to allow multiple Transit Field entries
of the form =RPOA=RPOA=HOSTNAME=HOSTNUMBER.  The loop count will
indicate only one entry is allowed for now.  A NOTE 3 indicating this
fact may be helpful.

I don't think there are any more problems with it that I can see, although
I am canvasing the Sup unit for input.  I will come up with the loop counts
by Tuesday afternoon.  Please let me know when we can discuss the final
diagram before you go to print, to insure agreement.

As always, any time frame that is suitable to you;  I am flexible.

Thanks...Omar

Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Mon, 9 Feb 87 18:11:34 UT
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 09 FEB 87 09:49:47 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.A/MORRISS@Ontyme.Tymnet, NTD.B/EUSKE@Ontyme.Tymnet,
	NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.D/SMITH@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.R/RAFFO@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.B/SOLEY@Ontyme.Tymnet, NTD.P/CHU@Ontyme.Tymnet,
	TYM.SU/BLU@Ontyme.Tymnet, NETS.R/ALBERTSON@Ontyme.Tymnet 
Message-id: I90302@Ontyme.Tymnet 
Keywords: SWBT 
Subject: SWB Actions 0 Restated 
SWBActions-RESTATED: 

                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      09 FEB 87  09:48

TO>        Sue Heisler

COPIES>    Distribution List (See CC List)

FROM>      Allyn Morriss


SUBJECT>   SWB Actions - RESTATED


-----------------------------------------------------------------------


In response to your concerns last Friday:
 
1. The GED for LOGII has been placed on (DNDSPEC:33) for your review.
   A copy of this may also be place in (TECHREF:56) by Marketing.
   The filename is LG2GED.DOC.
 
2. A written explanation of how to intrepret the schematic has been
   prepared by Omar Serang. It includes diagram symbols with text to
   describe same. This needs to be either FAXed to you or sent Federal
   Express.
 
   Please give me:
 
   - Mailing address for Fed/Exp  and/or
 
   - FAX telephone number.
 
3. I will speak to Romulo about RAM format impact (if any) on Monday.
   I will message you with results.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
From: David Chang <DChang@B39.Tymnet> 
Date: Mon, 9 Feb 87 17:48:28 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: SWB misc 

Bill,

I have given my comments on the logon syntax to Omar.  I also talked to
Allyn to check with Su to see if the conference call with Jerry was
necessary.  Hopefully, the explanation from Omar was satisfactory.  
Apparently, as of noon today, the explanation was not sent.  Are we sure
we are in the communications business?

Hope that you are not holed up too long.  One does need air once awhile,
you know.

David
From: David Chang <DChang@B39.Tymnet> 
Date: Tue, 10 Feb 87 17:21:57 PST 
To: Gazel Tan <GAZEL@B39> 
Cc: Omar Serang <OSERANG@B39.Tymnet>, Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: cost tables 

TO:  Gazel Tan
FR:  David Chang
RE:  Comments on Recommended New Cost Tables by Farhangian
DA:  Feb 9, 1987
CC:  Omar Serang, Bill Soley

=========================================================================

I have reviewed the recommended changes to the supervisor cost tables
by Keyvan Farhangian dated 5FEB87.  My only comment is that if the cost
tables are changes to "allow some batch traffic to travel through high
or medium bandwidth terrestrial links even if alternative satellite
links are available", then I am concerned about building such circuits
during non-prime hours and then impacting interactive circuits being
built later during prime time.  Unless we have a means of dynamically
diverting traffic depending on load, I feel that a closer examination on
this subject may be needed.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 11 Feb 87 22:42:26 PST 
To: Omar A. Serang <OSerang@B39.Tymnet> 
Cc: dchang, Bill Euske <NTD.W/EUSKE@Ontyme.Tymnet> 
Keywords: SWBT 
Subject: Re: Logon Syntax 
In-reply-to: your message of Mon, 9 Feb 87 9:47:06 PST

I suggest character counts, loop counts are probably confusing, particularly
without a copy of the drawing.  -Bill
(let me know and I'll prepare a draft and then sched a meeting to review)
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 16 Feb 87 8:44:04 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Logon Syntax 

Hello Bill,

Please let me know when we can mee to finalize the Logon Syntax chart.

Thanks...Omar

From: William R. Soley <WRS@B39.Tymnet> 
Date: Wed, 18 Feb 87 15:00:18 PST 
To: Omar A. Serang <OSerang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: Logon Syntax 
In-reply-to: your message of Mon, 16 Feb 87 8:44:04 PST

Friday afternoon?  I have a lunch meeting, so lets say 1:30?  -Bill
(sorry for the delays - I've been kind of brain damaged by Skywalke
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 19 Feb 87 11:45:15 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: double colon option 

Bill,
Are we or should we advertise the double colon option in the syntax diagram?
What is the future of this option?  Phase out, status quo, or expansion?
David
From: William R. Soley <WRS@B39.Tymnet> 
Date: Fri, 20 Feb 87 3:42:41 PST 
To: David Chang <DChang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: double colon option 
In-reply-to: your message of Thu, 19 Feb 87 11:45:15 PST

Definately not.  We need to keep it, status quo.  I'm concerned about testing
though, we should talk about it sometime.  -Bill
From: Omar A. Serang <OSerang@B39.Tymnet> 
Date: Mon, 23 Feb 87 9:14:54 PST 
To: Bill Soley <WRS@B39.Tymnet>, dchang 
Keywords: SWBT 
Subject: Logon syntax 

Bill,

Do we have a time for today?  I have meetings at 1100-1230, and 1500-1630.

Thanks...Omar

From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 23 Feb 87 11:15:25 PST 
To: Omar A. Serang <OSerang@B39.Tymnet> 
Cc: dchang 
Keywords: SWBT 
Subject: Re: Logon syntax 
In-reply-to: your message of Mon, 23 Feb 87 9:14:54 PST

How about right after the (1500) Cost Table meeting, whenever that breaks up.
Or we could just make it for 1600 if any non-cost-table people are involved.
-Bill
From: David Chang <DChang@B39.Tymnet> 
Date: Mon, 23 Feb 87 17:41:49 PST 
To: Omar Serang <OSERANG@B39.Tymnet> 
Cc: Gazel Tan <GAZEL@B39>, Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: Supervisor cost in the needle dump 

Omar,

I just talked to Edsel Budar and Dennis Jonas.  Both are very receptive
of having the supervisor cost in the needle dump data.  According to
Edsel, nobody else is analyzing this data, so that there is no problem
with including the cost in the dump for version 43.  As we have discussed,
please do it when you plan to do the NSR concerning the history log and
needle dump.

David
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 24 Feb 87 18:08:40 UT
From: NTD.G/CONE@EMSNTD.Ontyme.Tymnet 
Date: 24 FEB 87 09:27:41 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I97411@Ontyme.Tymnet 
Keywords: SWBT 
Subject: SKYWALKER project meeting minutes. 

 
	 23 February 1987		       skymin.223
 
	 To: Distribution
	 From: BJ
	 Subject: SKYWALKER project meeting minutes.
 
	 Here are the minutes from the status review meeting
	 of 20 February.
 
	 Agenda:
 
	 I.  Old items
	 II. New items
	 III.Other new items
 
 
	 I. OLD ITEMS
 
	 Old items were reviewed, including action items from previous
	 meeting.
 
	 1a. The first UNIX training class was completed, another
	 is scheduled. Michael's staff did an excellant job of
	 presenting to 22 class participants.
 
	 1b. Project schedule updates for firmware and diagnostics
	 will be available next week (diagnostics) and after final
	 Rodney review (firmware).
 
	 1c. Engine/68k debugger proposal will be provided on 27 Feb.
 
	 1e. Mechanical engineering  concept review will be held
	 monday 23 Feb at 9:30.
 
	 1f. IOC/HW functional specification review scheduled for
	 25 Feb. Goal is to complete internal reviw by 4 march.
 
	 2a. Update to draft project plan (first submitted for
	 core team review in early january) is distributed for
	 review. Note that there have been a number of changes to
	 this document. Review comments due Thursday 26 Feb.
 
	 2b.  SCCS class will be prepared by Michael's staff. Michael
	 will send out class description, 1 rep from each of hte
	 development groups should attend class, class will be
	 scheduled for week of 1 march.
 
	 2e. Unix system support. This issue was raised at Al's staff
	 meeting. Decision to defer further action until we get more
	 experience with the environment.
 
	 2f.  Trade shows.  QA needs to be involved.  Mktg has asked
	 us to build 12 machines over and above our own use .  QA has
	 asked for a SKYWALKER machine from pilot as well.  Ken is to
	 request that Rebmann specify what level of functionality is
	 reuqired for the trade shows.
 
 
	 II. NEW ITEMS
 
	 New items include:
 
	 1.  Review of project plan
	 2.  Review of change control form
	 3.  Project schedule issues
	 4.  RODNEY issues
 
 
	 1  Revised project plan distributed for review. Comments
	 due back by 26 February. Note that a number of changes have
	 been made to this document.
 
	 2.  Document change control form was distributed for review.
	 and approval of change to "Proposal".
 
	 3.  Players are reminded that project schedule updates
	 are expected monday 1 march.
 
	 4. Rodney walkthrough on 19 Feb resulted in a number of
	 action items for SKYWALKER core team. These are noted below.
 
	 RODNEY issues	are action items defined in the
	 meeting that need resources allocated to them.
 
	 1.  Long word boundary concerns on IOC We need resource
	 assignment to study this with reference to Gate array,
	 Compiler, and Linker.
 
	 ==> Jim Wollot assigned to evaluate this.
 
	 2.  Console (TTY) port loading We need resource assignment to
	 prepare proposal for integration into RODNEY document.
 
	 ==> Kevin assigned to this
 
	 3.  ASYNC I/O We need resource allocation for ASYNC I/O
	 study.  This study includes evaluation of one or more
	 strategies for handling autobaud, 19.2, and SIO async
	 emulation in SKYWALKER, and evaluation of implications of
	 strategies on CONSAT , SIO Emulation, and Firmware
	 development.
 
	 ==> Ernst, Chu, Kopf, Jonas assigned to this
 
	 4.  DISK Controller We need to expeditiously make a decision
	 on a controller for SKYWALKER because the controller
	 characteristics impact design and hence schedule issues.
 
	 ==> Soley and one of John's guys assigned to this.
 
	 5.  Utilities for File and disk system We need resource
	 allocation to define utilities required to support file and
	 disk system for RODNEY
 
	 Comment: Investigate single board computer with disk
	 controller. Consider build or buy decision before committing
	 internal development of a file system.
 
	 ==> Same as 4 above.
 
	 6.  Down line load protocol We need resource allocation to
	 evaluate and define a DLL protocol for SKYWALKER.
 
	 ==> Dave Comstock and one of Mike Rude's guys assigned to
	 this.
 
	 7.  PORT numbers/accounting We need Mike Rebmann to put
	 together a taskforce to define accounting requirements for
	 SKYWALKER
 
	 ==> Ken and BJ to bring up with Rebmann on wednesday
 
	 8.  Administration of penal codes etc.  We need an
	 administrator for penal codes, for message IDs to RODNEY's
	 log, and for IPCF message types.
 
	 ==> Ken
 
	 9.  We need to address MAXCAGE and Model# use with marketing
 
	  ==> Rebmann (Ken and BJ)
 
	 10.  We decided to eliminate non-RODNEY design issues from
	 RODNEY document.  We need to insure that these are instead
	 expressed as design input to other SKYWALKER development
	 activites.
 
	 III. Other new items
 
	 1. Availability/requirements for test beds need to be defined
	 for engineering.
 
	 Project schedules for each group should identify what test
	 facilities they require and WHEN they are required such that
	 we can share resources.
 
	 It is very important that we minimize the number of test bed
	 facilities we have to support during the development process
 
	 ==> Everyone. Include this issue in project schedule updates.
 
	 2. Other new items.
 
	 1. Dan presented the resource re-allocation issue, making
	 specific recommendations regarding resource re-allocation
	 in his area, and requests for resources from other groups.
 
	 Al reported that the only two projects in the division that
	 were not automatically candidates for resource sharing are
	 TSI and SWBELL numbering plan. He agreed to address resource
	 reallocation issues presented to him.
 
	 We agreed to meet on Wednesday AM (9am conference room in
	 BLDG D) to identify specific requirements and to apply
	 (reallocate) resources to meet these requirements.
 
	 2. Dave B distributed copies of memos regarding IOC HW
	 and firmware concerns to be resolved prior to design approval
	 on the IOC.
 
	 3. Weekly status review meetings will, beginning next week,
	 include Mr. Rebmann.
 
	  4. Next week's meeting is scheduled for Thursday 26 Feb,
	  1:30 pm.
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Wed, 4 Mar 87 0:11:14 UT
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 03 MAR 87 15:50:17 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I00944@Ontyme.Tymnet 
Keywords: SWBT 
Subject: SWB Meeting - Our Preparation 

                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      03 MAR 87  15:30

TO>        Distribution List (** NUMPLAN) - Attachment to Follow

COPIES>    Ken Holcomb
           Mike Rude
           Bill Euske
           Bill Soley
           Dan Lasater
           Pat Driscoll

FROM>      Allyn Morriss
           Gazel Tan


SUBJECT>   SWB Meeting(s) - Our Preparation


-----------------------------------------------------------------------


There will be a meeting at Orchard Parkway in the afternoon on March 10.
The purpose of the meeting will be to meet with Sue Heisler and Jeff Stern
to discuss a game plan for the meeting with SWB on 3/18 in St. Louis.
 
Gazel and I will be preparing for this meeting with your input. We will
also determine who will be needed on the 10th. I do not expect that all
of you are required to be there. Take note of the date, however, just in
case.
 
In my view, what is needed for the meeting is:
 
1. Set of documents to date.
 
   - Requirements document from SWB and any supplements thereafter.
   - Requirements specification from NTD that addresses SWB's request.
   - Individual design documents - written or planned.
   - Other related documents and correspondence.
 
   These will be the documents we want SWB to focus upon in dealing with us.
   I suspect that some writings may be separate documents that are really 
   addendums or re-statements to parent documents. This needs to be
   cleaned up so as to consolidate and bring up-to-date our requirements
   cleaned up by consolidating these into a cohesive set. We will need to
   identify these as to title and date and let everyone know.
 
   This will be a step towards reaching agreement (or seeing where there
                                                                Page  2

   is disagreement) with SWB on what it is that we are to do and the way we
   intend to implement. This will be a major breakthrough if we can get
   this accomplished.
 
2. Scheduling information is needed from each of you on your separate
   projects. This needs to be broken down into development phases. If your
   project depends upon completion of somebody else's, merely indicate
   which project(s) these are and durations for the phases in your project.
   I have attached a report to mark up your reply on this. I will use your
   input as a worksheet to enable creation of an integrated schedule with
   all project work shown.
 
   After preparing the integrated schedule, it will become apparent which
   dates will mark milestones. We can discuss which are external and thus
   ones we want to make known to SWB. Other milestones will be for our
   use in tracking the overall effort.
 
   NOTE: If there are any missing projects, please initate these with Rosie
         Hull so we can complete the picture of all NTD work.
 
3. Consideration should be given to post-implementation activities, such as
   integration testing and final documentation. These should be reflected
   in the schedule mentioned above. Particular attention should be focused
   on people and testbeds needed to perform final checkout operations.
   These are resources that may not be planned from your specific area.
   If that is the situation, I will need to know this so planning
   can take place to fill the gap when the time occurs.
 
4. Another area is clarification of requirements. If any of you are unsure
   of what is required in your specific area, please draft a set of questions
   and OnTyme the group with these ASAP. By the 'group' I mean the people 
   listed in OnTyme file ** NUMPLAN.
 
Please respond to the above this week and message Gazel and I on any concerns
you may have.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Wed, 4 Mar 87 1:13:04 UT
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 03 MAR 87 16:14:47 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.P/CHU@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.R/RAFFO@Ontyme.Tymnet 
Message-id: I00964@Ontyme.Tymnet 
Keywords: SWBT 
Subject: SWB Numbering Plan - Meeting & Our Preparation 

SUBJ: SWB Numbering Plan - Meeting & Our Preparation

                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      03 MAR 87  15:30

TO>        Distribution List (** NUMPLAN) - Attachment to Follow

COPIES>    Ken Holcomb
           Mike Rude
           Bill Euske
           Bill Soley
           Dan Lasater
           Pat Driscoll

FROM>      Allyn Morriss
           Gazel Tan


SUBJECT>   SWB Meeting(s) - Our Preparation


-----------------------------------------------------------------------


There will be a meeting at Orchard Parkway in the afternoon on March 10.
The purpose of the meeting will be to meet with Sue Heisler and Jeff Stern
to discuss a game plan for the meeting with SWB on 3/18 in St. Louis.
 
Gazel and I will be preparing for this meeting with your input. We will
also determine who will be needed on the 10th. I do not expect that all
of you are required to be there. Take note of the date, however, just in
case.
 
In my view, what is needed for the meeting is:
 
1. Set of documents to date.
 
   - Requirements document from SWB and any supplements thereafter.
   - Requirements specification from NTD that addresses SWB's request.
   - Individual design documents - written or planned.
   - Other related documents and correspondence.
 
   These will be the documents we want SWB to focus upon in dealing with us.
   I suspect that some writings may be separate documents that are really 
   addendums or re-statements to parent documents. This needs to be
   cleaned up by consolidating these into a cohesive set. We will need to
   identify these as to title and date and let everyone know.
 
   This will be a step towards reaching agreement (or seeing where there
                                                                Page  2

   is disagreement) with SWB on what it is that we are to do and the way we
   intend to implement. This will be a major breakthrough if we can get
   this accomplished.
 
2. Scheduling information is needed from each of you on your separate
   projects. This needs to be broken down into development phases. If your
   project depends upon completion of somebody else's, merely indicate
   which project(s) these are and durations for the phases in your project.
   I have attached a report to mark up your reply on this. I will use your
   input as a worksheet to enable creation of an integrated schedule with
   all project work shown.
 
   After preparing the integrated schedule, it will become apparent which
   dates will mark milestones. We can discuss which are external and thus
   ones we want to make known to SWB. Other milestones will be for our
   use in tracking the overall effort.
 
   NOTE: If there are any missing projects, please initate these with Rosie
         Hull so we can complete the picture of all NTD work.
 
3. Consideration should be given to post-implementation activities, such as
   integration testing and final documentation. These should be reflected
   in the schedule mentioned above. Particular attention should be focused
   on people and testbeds needed to perform final checkout operations.
   These are resources that may not be planned from your specific area.
   If that is the situation, I will need to know this so planning
   can take place to fill the gap when the time occurs.
 
4. Another area is clarification of requirements. If any of you are unsure
   of what is required in your specific area, please draft a set of questions
   and OnTyme the group with these ASAP. By the 'group' I mean the people 
   listed in OnTyme file ** NUMPLAN.
 
Please respond to the above this week and message Gazel and I on any concerns
you may have.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Fri, 6 Mar 87 3:17:13 UT
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 05 MAR 87 19:04:24 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: TYM.SU/BLU@Ontyme.Tymnet, TYM.F/HICKS@Ontyme.Tymnet,
	NETS.J/STERN@Ontyme.Tymnet, NETS.P/HIDY@Ontyme.Tymnet,
	NETS.M/CADY@Ontyme.Tymnet, NTD.K/HOLCOMB@Ontyme.Tymnet,
	NTD.A/MORRISS@Ontyme.Tymnet, NTD.W/EUSKE@Ontyme.Tymnet,
	NTD.B/SOLEY@Ontyme.Tymnet, NTD.D/LASATER@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.P/CHU@Ontyme.Tymnet,
	NTD.D/CHANG@Ontyme.Tymnet, NTD.M/RUDE@Ontyme.Tymnet,
	NTD.P/DRISCOLL@Ontyme.Tymnet, NTD.A/NEWMAN@Ontyme.Tymnet,
	NTD.R/RAFFO@Ontyme.Tymnet 
Message-id: I02287@Ontyme.Tymnet 
Keywords: SWBT 
Subject: March 10th SWB Meeting At O/P 
March10thSWBMeetingAtO/P: 

SUBJ: March 10th SWB Meeting At O/P

                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      05 MAR 87  19:03

TO>        Jeff Stern
           Sue Heisler

COPIES>    Distribution List (** SWB)

FROM>      Allyn Morriss


SUBJECT>   March 10th SWB Meeting At O/P


-----------------------------------------------------------------------


Received your anouncement of the meeting on 3/10 1:00PM. Please be
prepared with documented list of the 'apparent inconsistensies' between 
the development documents. 
 
We are preparing a document set and integrated schedule to go over 
with you. We will be working to identify milestones as well. The first of
these may very well be reaching agreement on the documents. We will be
requiring a SWB signoff as evidence to such agreement.
 
See you Tuesday.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Mon, 9 Mar 87 21:36:58 UT
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 09 MAR 87 12:24:26 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.P/CHU@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.R/RAFFO@Ontyme.Tymnet,
	NTD.O/SERANG@Ontyme.Tymnet 
Message-id: I03254@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Request for SWB Scheduling Information 
RequestforSWBSchedulingInformation: 

                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      09 MAR 87  12:23

TO>        Distribution (** NUMPLAN)

COPIES>    Ken Holcomb

FROM>      Allyn Morriss


SUBJECT>   Request for SWB Scheduling Information


-----------------------------------------------------------------------


Last Wednesday, Gazel Tan distributed project sheets to all of you asking
for a phase breakdown schedule for each. At this date, I have only heard from
David Chang. David has also responded to an earlier memo requesting that
questions and concerns be documented by you as relates to SWB numbering
plan projects.
 
Please respond either later today or early tomorrow. The meeting with Jeff
Stern and Sue Heisler is tomorrow afternoon. I want to be prepared with an
integrated schedule and potential milestones.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 12 Mar 87 16:05:50 PST 
To: Bill Soley <WRS@B39.Tymnet> 
Keywords: SWBT 
Subject: swb misc 

Bill,

When you have time, please answer the following:

1. What is "transit network preselection, and how are we going to support
it for SWB?  I think that we are going to do this via a MUD entry of the
default gateway for a destination name.

2. What is the status of Subnet Confinement Facility?  I need to give
an estimate of man-months for design, implementation, etc.  I believe that
you will be providing the design, right?!

3. Do you know anything about "call screening"?

4. How's it going in Peter Pan world?

David
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 12 Mar 87 16:37:59 PST 
To: David Chang <DChang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: swb misc 
In-reply-to: your message of Thu, 12 Mar 87 16:05:50 PST

1. transit network preselect is done in the access profile by a complicated
matrix between the user's access profile and the host's.  I can give a
tutorial some time, but I don't know when.  It does not need any code in
and of itself. 

2. subnet confinement facility is something that is not well designed.
All of my comments are in the Recommendation document.  I will be glad
to participate in the design, but I'm not sure what priorities will be.
Have they ordered it?  Its not part of phase I.

3. I don't know anything about call screening.  I think it is something
done at the X.25 interface, maybe.

4. Everything's turning up peanut butter.  I think we'll have a nice GED
by the 2nd week of April for the ROM based services.  I still haven't
gotten used to getting up at 8:00, but I'm having fun none the less.
I am worried about you sounding a little discouraged about not being
involved right now.  All I can say is that I think there is more than
enough interesting work coming down the road for the supervisor when
we are ready to port it over.  I think the network is going to undergo
alot of change at the architectural level as a result of Skywalker.  I
mean stuff that people don't even think about now - its just opens up
alot of possibilities.  Skywalker is interesting, but only because its
something new.  That will wear off, probably before the work is done.
The supervisor, however, is clearly the most fascinating and challenging
part of the network.  That won't wear off.

-Bill
Received: from B10.Tymnet by B39.Tymnet; Wed, 25 Mar 87 18:17:55 PST
Received: from EMSNTD.Ontyme.Tymnet by B10.Tymnet; Thu, 26 Mar 87 3:07:32 UT
Return-path: <NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet> 
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 25 MAR 87 17:29:04 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: (24 names) 
Message-id: I11290@Ontyme.Tymnet 
Keywords: SWBT 
Subject: SWB Numbering Plan - Action Items From Meeting 3/18 
ActionItems-SWBMeeting3/18/87: 

                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      25 MAR 87  17:25

TO>        Distribution List (*** SWB)

COPIES>    NTD Project Members (** NUMPLAN)

FROM>      Allyn Morriss


SUBJECT>   Action Items - SWB Meeting 3/18/87


-----------------------------------------------------------------------


The following is the list of action items resulting from the meeting
in St. Louis with SouthWestern Bell - 3/18/87.
 
    Action Items.                                                Responsibility
    ------------                                                 --------------
 
1.  Review our ability to furnish SWB with RASP accounting       Ken Holcomb
    tape during Bubbnet testing.
 
2.  Obtain from Bill Soley a description of the PC-based         Bill Soley
    validation application he eluded to during his visit to      Ken Holcomb
    St. Louis last year.
 
3.  Review for acceptability and applicability the monitoring    SWB
    features in version 6.02 of CONSAT.
 
4.  Provide estimates for all Phase 2 development work           NTD
    discussed at meeting (Re: PIR 2379)
 
5.  SWB requested document describing enhanced NETVAL process.   Romulo
 
6.  Program Manager to be assigned to coordinated SWB            SWB - 4/8
    project activities.
 
7.  Review/approval by SWB on SUPervisor GED for Numbering       SWB - 4/8
                                                                Page  2

    Plan. (LOG-II)
 
8.  Review/approval of 'Implementation Recommendations of        SWB
    the Numbering Plan' dated 11/86 as baseline document.
 
9.  Date for next meeting on 6/23 to be confirmed.               SWB
 
10. Decision on whether to accept Tymnet's Project Mgt.          SWB
 
11. Review and response to Installation Plan requirements.       SWB
 
12. Review and response to Acceptance Plan requirements.         SWB
 
13. Review of Jerry Talkin's proposed changes to login           Gazel - 3/31
    syntax & overlay of X.28 with numbering plan.                Conf call
 
IMPORTANT NOTE:
 
In addition to the above actions, after the meeting, it was learned that
SWB has many ATCs deployed and providing support for the Numbering Plan
for these machines has not be considered for implementation in Phase I.
A careful look at this is required by Marketing/Sales to assess this
additional requirement.
 
Also: LOG II Revised GED regarding SUP design of the implementation
      of the Numbering Plan resides in DNDSPEC:33 as file LOGII.GED.
 
      OnTyme file *** SWB is being redistributed to all internal systems
      to correct an oversight.
 
If there is anything I missed, please advise.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
From: David Chang <DChang@B39.Tymnet> 
Date: Fri, 27 Mar 87 14:15:33 PST 
To: Bill Soley <WRS@B39.Tymnet>, Dennis Ernst <NTD.D/ERNST@Ontyme.Tymnet> 
Keywords: SWBT 
Subject: closed user group and ISDN 

Bill and Dennis,

I am trying to get information for the SWB Numbering Plan project
concerning closed user groups and ISDN.  According to the X.25
recommendations (sec. 6.14), closed user groups enable "users to form
groups of DTEs to and/or from which access is restricted".  This sounds
like that we support CUG via the MUD.  However,  the recommendation also
states that "the DTE may explicitly indicate in the call request packet
the CUG selected by using the closed user group selection facility or
the closed user group with outgoing access selection faciltiy".  May we
say that we support these facilities via the destination field of the
login string or are these facilities special X.25 protocol messages?
More to the point, I am asking whether we are providing CUG service to
SWB when the X.121 Numbering Plan project is completed?  Furthermore,
Adele mentioned that single CUG is supported at the interface level.
What does that mean?

I am also inquiring about ISDN.  Will we support:  "The calling address
sent to an interface shall be 9001+NXX+NXX+XXXX if the call originated
on an ISDN".  Originally, we had said "NO" because we had no specs for
ISDN.  However, can we not say that an ISDN interface can insert any
originating address in the login request, including one with the requested
pseudo-DNIC of 9001?

Thanks.

David

From: William R. Soley <WRS@B39.Tymnet> 
Date: Sat, 28 Mar 87 2:31:34 PST 
To: David Chang <DChang@B39.Tymnet> 
Cc: Dennis Ernst <NTD.D/ERNST@Ontyme.Tymnet> 
Keywords: SWBT 
Subject: Re: closed user group and ISDN 
In-reply-to: your message of Fri, 27 Mar 87 14:15:33 PST

I understand that CUG's are supported to some degree within the X.25
interface (and possibly X.28).  We have no plan to support them at the
supervisor level.  They are kind of another way of meeting some of the
requirements met by access profiles.  So I think the answer is that
CUGs will be supported only for calls with both endpoints on X.25 .

Re: ISDN.  Since I have no idea how they plan to interface to ISDN, I
can't really comment except to say that if they build an interface that
does what they want, then it will do what they want.  I would guess they
could use an X.25 interface and just program it to think its DNIC is
9001 -- any comment on this, Dennis?

Sorry I'm not more help on these.
-Bill
Received: from B10.Tymnet by B39.Tymnet; Mon, 30 Mar 87 15:34:20 PST
Received: from EMSNTD.Ontyme.Tymnet by B10.Tymnet; Mon, 30 Mar 87 23:09:50 UT
Return-path: <NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet> 
From: NTD.A/MORRISS@EMSNTD.Ontyme.Tymnet 
Date: 30 MAR 87 14:24:21 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.P/CHU@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.R/RAFFO@Ontyme.Tymnet,
	NTD.O/SERANG@Ontyme.Tymnet 
Message-id: I13249@Ontyme.Tymnet 
Keywords: SWBT 
Subject: Minutes - SWB Numbering Plan Meeting 3/27 
Minutes-SWBNumberingPlanMeeting3/27/87: 

                      TYMNET/NETWORK TECHNOLOGY DEVELOPMENT
                           PROGRAM MANAGEMENT SECTION
 
                               M E M O R A N D U M 
 
                                                                []  ISG
 
DATE>      30 MAR 87  14:18

TO>        Attendees

COPIES>    Distribution List (** NUMPLAN)

FROM>      Allyn Morriss


SUBJECT>   Minutes - SWB Numbering Plan Meeting 3/27/87


-----------------------------------------------------------------------


ATTENDEES:   Gazel Tan          Adele Newman          David Chang
             Paul Chu           Nancy Holt            Allyn Morriss
 
The following are the minutes of last Friday's meeting:
 
GENERAL.
 
1. Gazel and Allyn discussed the 3/18 meeting in St. Louis with SWB.
 
2. Allyn had copied all project members with their phase breakdown
   schedules received earlier in the month, and pointed out the Tellaplan
   Schedule and Pert Chart provided at SWB meeting.
 
3. Gazel distributed copies of SWB requirements document for review during
   meeting along with X.28 CCITT standard format specification.
 
REGARDING SCHEDULES.
 
1. Everyone is to develop a Pert Chart using SuperProject for their
   Unit's work on the SWB Numbering Plan - Phase I.
 
2. Diskettes are to be given to Allyn for consolidation. Allyn agreed to
   prepare high-level Pert and Tellaplan Gantt to show how schedules are
   integrated.
 
                                                                Page  2

3. Schedules should be developed by next meeting.
 
REGARDING DOCUMENTS.
 
1. Gazel is to identify all the documents that are to be prepared
   for this Project and give copy to Allyn.
 
2. It was agreed that all documents would be reviewed internally and
   that five (5) days should be planned to complete this before delivery
   to Marketing for customer signoff.
 
3. Documents sent to Marketing for review and approval by customer are
   to be given ten (10) working days for response to complete.
   Allyn is to handle distribution and response.
 
4. The first document to made available to Marketing (Jeff Stern) will
   be the SUP Login Processing GED. It was ready later in afternoon and
   was sent to Jeff this morning.
 
5. Question was raised about requirement to provide ERS updates
   Marketing and customer. Allyn to investigate.
   Point was raised during discussion concerning NETVAL documentation
   of changes to validation procedures. SWB has requirement to know of these
   as soon as available.
 
REGARDING the review of SWB's Requirements Document.
 
1. All 53 items were reviewed with special attention given to those
   marked as YES, indicating SWB's intention for Phase I implementation.
 
2. David and Adele agreed to meet at 2PM this Wednesday to clarify
   X.25 requirements in items 37 & 38.
 
3. Item 41 is marked as YES but implementation depends upon symbolic
   addressing capability. There is at present a PIR for Televerket to
   develop this. SWB apparently overlooked our original response to
   their itemized requirements early this year. Bill Soley and Dave
   to re-examine.
 
4. Item 50 was also found to be in conflict with NTD's original
   assessment of requirements list.
 
5. Item 51 was questionable for next release of XCOM. Adele to re-assess.
 
REGARDING PROJECT MEETINGS.
 
1. Agreed to monthly meetings to review progress and problems.
   Meetings will be more frequent, if required. Frequent meetings
   are anticipated as integration testing nears.
 
2. NEXT MEETING is scheduled for April 24 - 10AM.
 
3. Progress reporting will be monthly and occur after monthly status meeting.
 
Please let me know if I overlooked anything.
 
Regards,
Allyn Morriss (408/446-6851)
Manager, NTD/Program Management
NTD.A/MORRISS
Received: from B36.Tymnet by B39.Tymnet; Thu, 2 Apr 87 2:05:16 PST
Received: from tymix.Tymnet by B36.Tymnet; Thu, 2 Apr 87 0:55:52 PST
Received: by tymix.Tymnet (5.51/4.7) id AA12198; Thu, 2 Apr 87 00:54:52 PST
Return-path: <wrs@tymix.Tymnet> 
From: wrs@tymix.Tymnet (Bill Soley) 
Date: Thu, 2 Apr 87 00:54:52 PST 
To: dchang@39, wrs@39 
Message-id: <8704020854.AA12198@tymix.Tymnet> 
Keywords: SWBT 
Subject: LOGII and X.409 

I am sorry to bring this up so late in the game.  As part of the Skywalker
design, we are looking at X.409.  This spec describes exactly what we are
doing in the SMO.  I would like to suggest we adapt the X.409 in place of
the SMO if for no other reason than it is very well documented and it
would save us time and confusion in that regard.  Conceptually X.409 is
identical to SMOs and even the syntax is very similar.  The overhead
seems to be comparable, as well.  It is slightly more general.  Sometime
soon we should chat about this, although, perhaps its too late.  -Bill
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 2 Apr 87 10:14:58 PST 
To: wrs@tymix.Tymnet (Bill Soley) 
Cc: wrs@39 
Keywords: SWBT 
Subject: Re: LOGII and X.409 
In-reply-to: <8704020854.AA12198@tymix.Tymnet> of Thu, 2 Apr 87 00:54:52 PST

Bill,

Yes, indeed, I wish to talk to you about X.409.  When can we talk?

David
Received: from B36.Tymnet by B39.Tymnet; Thu, 2 Apr 87 10:39:12 PST
Received: from tymix.Tymnet by B36.Tymnet; Thu, 2 Apr 87 10:38:55 PST
Received: by tymix.Tymnet (5.51/4.7) id AA14616; Thu, 2 Apr 87 10:37:53 PST
Return-path: <DChang@B39.Tymnet> 
From: David Chang <DChang@B39.Tymnet> 
Date: Thu, 2 Apr 87 10:14:58 PST 
To: wrs@tymix.Tymnet (Bill Soley) 
Cc: wrs@39 
Message-id: <8704021837.AA14616@tymix.Tymnet> 
Keywords: SWBT 
Subject: Re: LOGII and X.409 
In-Reply-To: <8704020854.AA12198@tymix.Tymnet> of Thu, 2 Apr 87 00:54:52 PST

Bill,

Yes, indeed, I wish to talk to you about X.409.  When can we talk?

David
From: William R. Soley <WRS@B39.Tymnet> 
Date: Thu, 2 Apr 87 13:29:22 PST 
To: David Chang <DChang@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: LOGII and X.409 
In-reply-to: <8704021837.AA14616@tymix.Tymnet> of Thu, 2 Apr 87 10:14:58 PST

Well, I might be able to come by this afternoon, but I'm not sure yet.
I have to see how the writting goes, I might get done early.  I suggest
you might briefly look at X.409 in the Red Book in the library.  Most of
the paper describes a formal notation for defining the data structure.
There exist compilers that input the formal notation and output parsers
and pretty documents.  Ken Jones supposedly has one for the PC that he
is going to give me source code for (in C).  At any rate, concentrate
on the example on page 87 and 88 - it shows the real bits.  I can then
narrate a little and it should all become obvious.

When I first looked at it, it made no sense at all, then Romalo spent
5 minutes telling me a few things and now it all makes perfect sense.
CCITT documents are that way.

-Bill
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 14 Apr 87 1:58:22 UT
From: NTD.P/CHU@EMSNTD.Ontyme.Tymnet 
Date: 13 APR 87 12:30:19 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Message-id: I19766@Ontyme.Tymnet 
Keywords: SWBT 
Subject: GED for CONSAT Numbering Plan 

                         M E M O R A N D U M

                                          [] Network Technology Division
                                             Async Development Section
DATE>      13 APR 10:00

  TO>      ALLYN MORRISS

COPIES>    GAZEL TAN
           DAN LASATER
           DEBORAH GRAHAM
           ADELE NEWMAN
           DENNIS ERNST
           DAVID CHANG
           OMAR SERANG
           RICH TENG

FROM>      PAUL CHU

SUBJECT>   CONSAT GED FOR NEW LOGIN SYNTAX(RE: SWB NUMBERING PLAN)

-----------------------------------------------------------------------

Allyn,

The GED for CONSAT PIR# 2214 is ready for NTD internal review.
The file is in (DNDSPEC:39)CSLGII.GED.

I will make copies and distribute them to the distribution list
copied above.

The review period starts from 04/14/87 to 04/16/87.

The final GED will be delivered to Gazel Tan for customer's
review beginning 04/20/87.  The expected customer turn around time
is two weeks from the deliver date.

Regards,


Paul Chu
Async Development
Received: from EMSNTD.Ontyme.Tymnet by B39.Tymnet; Tue, 14 Apr 87 1:58:49 UT
From: NTD.P/CHU@EMSNTD.Ontyme.Tymnet 
Date: 13 APR 87 15:48:54 
To: NTD.B/SOLEY@EMSNTD.Ontyme.Tymnet (NTD.WRS) 
Cc: NTD.K/HOLCOMB@Ontyme.Tymnet, NTD.A/MORRISS@Ontyme.Tymnet,
	NTD.W/EUSKE@Ontyme.Tymnet, NTD.B/SOLEY@Ontyme.Tymnet,
	NTD.D/LASATER@Ontyme.Tymnet, NTD.G/TAN@Ontyme.Tymnet,
	NTD.P/CHU@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet,
	NTD.M/RUDE@Ontyme.Tymnet, NTD.P/DRISCOLL@Ontyme.Tymnet,
	NTD.A/NEWMAN@Ontyme.Tymnet, NTD.R/RAFFO@Ontyme.Tymnet,
	NTD.O/SERANG@Ontyme.Tymnet 
Message-id: I19971@Ontyme.Tymnet 
Keywords: SWBT 
Subject: PAD AND X.25 IIX EXCHANGES 
IIXEXCHANGESBETWEENPADANDX.25AFTERNEEDLEMESSAGE: 

                         M E M O R A N D U M

                                          [] Network Technology Division
                                             Async Development Section
DATE>      13 APR 16:00

  TO>      ADELE NEWMAN

COPIES>    DAN LASATER
           GAZEL TAN
           DAVID CHANG
           OMAR SERANG

FROM>      PAUL CHU

SUBJECT>   IIX EXCHANGES BETWEEN PAD AND X.25 AFTER NEEDLE MESSAGE

-----------------------------------------------------------------------

Adele,

It is my belief than PAD and X.25 should be in strict agreement
as to how we should handle the IIX exchanges after the circuit
request has been processed and the needle is built to the X.25 slot.

Since the NEEDLE message will provide some useful information, I feel
that we should define and confirm the IIX messages currently being
exchanged between PAD and X.25.
After the agreement on the necessary exchanges, we then should look at 
the NEEDLE format and define what information we should take from the
NEEDLE and determine if the IIX exchanges should be eliminated or not.
Of course, there are conditions when either PAD or X.25 is not
LOGII compatible and that we syill have to go through the regular
IIX exchanges.

I like to solicit your input on this and, if possible, set up
a meeting with you to define the scope of the exchange sequences.

Regards,


Paul Chu
Async Development
From: David Chang <DChang@B39.Tymnet> 
Date: Tue, 14 Apr 87 17:53:07 PDT 
To: Gazel Tan <GAZEL@B39>, Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>, Bill
	Soley <WRS@B39.Tymnet> 
Cc: Rich Teng <RICHTENG@B39.Tymnet>, Omar Serang <OSERANG@B39.Tymnet> 
Keywords: SWBT 
Subject: Questions for SWB 
RRCPTTo: !WRS@C39.Tymnet

Gazel, Allyn, Bill:

I have listed below some of the questions that perhaps should be posed to
SWB.  Maybe Bill has some of the answers.  If not, then I would like these
questions be presented to SWB as soon as possible.

QUESTIONS FOR SWB:

1. [4/7/87] Are all native DNIC's in SWBNET synonymous?
Here "native" means the DNIC's associated with the network one is
referring to.  "Synonymous" means native DNIC's that are essentially
equivalent, ie. if 3111 and 3222 are native DNICs of a network, then
they are also synonymous when any address of the form 3111xxx... and
3222xxx... refer to the same location.  In the public net, all the
DNIC's are synonymous.

2. [4/7/97] Does SWB expect to use TYMNET II gateways in Phase 1?
SWB is expecting to traverse transit networks, but it is not known
whether they will be using X.75 or T-II gateways.  If so, then work
should be planned (if not already).

3. [4/10/87] Does the link in SWBNET between the X.25 interface and the
DTEs carry abbreviated addresses (like NTD and 1+IDN) or full addresses
(like IDN only)?
We are assuming (unless told otherwise) that the abbreviated addresses
are used mainly at the CONSAT PAD in the login string, for user
convenience.  If the DTE at the X.25 interface allows the user to do the
same, then this DTE is smart enough to do the conversion.  In essence,
the X.25 will never see the abbreviated addressing, since all X.121
adddressing internal to the network will be prefixed with a "-".  Only
the login string may have the abbreviated addressing in SWBNET.

4. [4/14/87] When specifying the RPOA as a transit network, does SWB
require the prefix "1"?
We are assuming "NO".  In fact, we require the prefix "-".  If the
prefix was "1". then we cannot differentiate between the RPOA and a
transit (gateway) host number.  Our assumption is depicted in the syntax
diagram, but SWB may have missed it.

5. [4/14/87] Will SWB use an X.121 name in the user field?  How?
In the syntax diagram, the X.121 name as a username has a fixed prefix
"-", DNIC, and 1 to 10 digits of NTN.  Will SWB require a prefix of "1"?
Whenever we encounter an X.121 address or name, we will lookup the DNIC
table to determine the number of NTN digits to be used in the lookup.
Will that satisfy SWB, or do they care?  Actually, the syntax diagram
requires 1 to 10 digits of NTN in the X.121 name, but we could just
lookup in the DNIC and reject the call if the name is not in the MUD.
Currently, we will follow the latest syntax diagram by Bill Soley, ie.
the X.121 name is prefixed by a "-", must have 1 to 10 digits of NTN,
and will be processed via the DNIC table as in the case of the
destination field.
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 14 Apr 87 23:01:40 PDT 
To: David Chang <DChang@B39.Tymnet> 
Cc: Gazel Tan <GAZEL@B39>, Allyn Morriss <NTD.A/MORRISS@Ontyme.Tymnet>, Rich
	Teng <RICHTENG@B39.Tymnet>, Omar Serang <OSERANG@B39.Tymnet> 
Keywords: SWBT 
Subject: Re: Questions for SWB 
In-reply-to: your message of Tue, 14 Apr 87 17:53:07 PDT

1. [4/7/87] Are all native DNIC's in SWBNET synonymous?

No, that's just a kludge in the way its implemented, now.  Since in the new
scheme, the DNIC is looked up as part of the name in the MUD, they will
have distinct address spaces (as they should).  If anyone is counting on it
behaving like it does now, they will have to validate more than one host
name.  As for the names that might be generated algorithmically according
to the TYMNET numbering plan, that is a matter for further study.

2. [4/7/97] Does SWB expect to use TYMNET II gateways in Phase 1?

I think they have to use X.75 for interface to carriers, even if they are
TYMNET technology.  Their LATA's are only virtual subnets, so there are
no gateways.  Of course there will be TYMNET-II gateways to Tymnet for the
purpose of down-line loads.

3. [4/10/87] Does the link in SWBNET between the X.25 interface and the
DTEs carry abbreviated addresses (like NTD and 1+IDN) or full addresses
(like IDN only)?

The address provided to X.25 and X.75 interfaces should always be the
full IDN - without the "1".  There was still some debate, last I checked,
as to weather or not they wanted to accept abbreviated addresses at the
X.25 interface (never at X.75).  This is only because they don't understand
the data communication world - it is clear that accepting abbreviated
addresses at the X.25 interface would be a mistake.  At any rate, I
suppose you should check with them to be sure, unless you feel comfortable
doing it right even if they want it done wrong.  I can provide a more
convincing argument than presented here if I must, but I don't really
have the time.

4. [4/14/87] When specifying the RPOA as a transit network, does SWB
require the prefix "1"?

No.

5. [4/14/87] Will SWB use an X.121 name in the user field?  How?

What is an X.121 name?  Assuming you mean a name beginning with a "-",
then what you said is the correct behavoir...

> Whenever we encounter an X.121 address or name, we will lookup the DNIC
> table to determine the number of NTN digits to be used in the lookup.
> Will that satisfy SWB, or do they care?  Actually, the syntax diagram
> requires 1 to 10 digits of NTN in the X.121 name, but we could just
> lookup in the DNIC and reject the call if the name is not in the MUD.
> Currently, we will follow the latest syntax diagram by Bill Soley, ie.
> the X.121 name is prefixed by a "-", must have 1 to 10 digits of NTN,
> and will be processed via the DNIC table as in the case of the
> destination field.

If you allow the X.121 in the user field to be abbreviated as with 1 and
such in place of the "-", then you'll have serious problems in the
transition since numeric names are now legal and have different meanings
by convention.

------------------------------------------------------------------------
I hope all this helps.
-Bill
  ^lI&