Received: from EMSNTD.Ontyme.Tymnet by C39.Tymnet; Sat, 1 Mar 86 3:02:38 UT
From: NTD.D/CHANG@Ontyme.Tymnet 
Date: 28 FEB 86 18:52:02 
To: NTD.B/SOLEY@Ontyme.Tymnet (NTD.WRS@Ontyme.Tymnet) 
Cc: NTD.B/SOLEY@Ontyme.Tymnet, NTD.W/SOLEY@Ontyme.Tymnet,
	NTD.D/RAWSON@Ontyme.Tymnet, NTD.J/KOPF@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet 
Message-id: I27483@Ontyme.Tymnet 
Keywords: SUP 
Subject: Load Leveling Through Paths With Same Costs 

TO:  Bill Soley
FR:  David Chang
RE:  Load Leveling Through Paths With Same Costs
DA:  February 28, 1986
CC:  Dick Rawson, John Kopf, Gazel Tan

====================================================================

Since you have been, are, or will be working on the Least Cost First 
algorithm, I thought that I would ask for your comments on the
following proposal.  In particular, I am concerned about the randomness
of the solution.  If I remember my Probability/Statistics correctly, it
seems that I am not getting the desired equal distribution.  For
example, if there are four available paths (A, B, C, D) and the
comparisons are made a pair at a time, then the probablity distribution
for the paths would be 1/8, 1/8, 1/4, 1/2, instead of the desired 1/4,
1/4, 1/4, 1/4.  I am wondering whether a better solution is available.


____________________________________________________________________

(DCHANG:39)P2001.RPT

RESPONSE TO PIR #2001 (LOAD LEVELING THRU TYMNET T201 PROTOCOL)
by David Chang (28FEB86)

OBJECTIVE: To load level several possible paths with the same cost.  For
each login request, the path with the least cost is chosen for the
circuit.  However, if there are several possible paths with the same
cost, then the first path is selected.  This results in the other paths
not being used until the first path is overloaded.  It is desirable to
insure that all possible paths are utilized equally.

ANALYSIS: Currently, the pathfinding routine (SPROUT) in the Supervisor
calculates the cost of a node to a destination as a tree is "sprouting"
toward the originating node of the login request.  As the tree is being
built, the least cost path from a node to the destination is selected.
If more than two paths with the same cost exist for a node, then the
first one encountered is selected.  When the tree is finally built, only
the information representing resulting path is retained.  Since the
decisions are made during the tree sprouting, it is not possible to choose
another path after the tree has sprouted.  Since the same decisions are
being made, the resulting path is always the same (given the similar
conditions).

This decision made during the tree sprouting is a simple one: choose the
previous found path if the costs are the same.  The cost comparisons are
done between two paths at a time.  Simply alternating the decision (ie.
one time choose the previous path and the next time choose the new path)
would only result in load leveling between two paths.  If more are
available, then they are not utilized.

Perhaps the simplest solution is to make the decision in a random
fashion.  In a truly random environment, the load-leveling among
multiple paths should be accomplished.  To obtain randomness, the least
significant bit of the FASTC time may be used.  The advantage of this
solution is simplicity without redesigning the the pathfinding scheme.
The disadvantage is more processing time to make a decision.  Since the
public net uses the microcode to do the sprouting, this proposal should
first be deployed for the private networks (Southwestern Bell requested
this feature).
From: William R. Soley <WRS@C39.Tymnet> 
Date: Sat, 1 Mar 86 20:48:45 PST 
To: NTD.D/CHANG@Ontyme.Tymnet 
Cc: NTD.D/RAWSON@Ontyme.Tymnet, NTD.J/KOPF@Ontyme.Tymnet,
	NTD.G/TAN@Ontyme.Tymnet, NTD.D/CHANG@Ontyme.Tymnet 
Keywords: SUP 
Subject: Re: Load Leveling Through Paths With Same Costs 
In-reply-to: I27483@Ontyme.Tymnet of 28 FEB 86 18:52:02

The alternative of the coin flip is actually more efficient.  A background
process would (asynchronously) rotate links through the neighbor table for
each node.  The algorithm is biased toward the front of the table.  Although
this could take a considerable amount of CPU, it would be a low priority
process and would not run during heavy load.  It could be triggered on elapsed
time or on number of circuits built or just about anything else.  I believe
number of circuits built may be the best trigger.  For example, a circular
pointer could be kept to the node table.  Each time a circuit is built, the
pointer is advanced to the next node and the neighbor table for that node is
rotated.  If we had infinite amounts of CPU, we would rotate the table of
each node in the circuit each time a circuit is built.  This is also a
statistical approach, but I believe it should be quite fair.

You are correct about the biasing resulting from cascaded coin flips.  It
does apply to that solution -- that and the CPU requirement make it a poor
choice.

-Bill
   