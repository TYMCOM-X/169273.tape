Received: from X32.Tymnet by B39.Tymnet; Thu, 5 Nov 87 18:27:15 PST
Return-path: <JMS@X32.Tymnet> 
From: Joe Smith <JMS> 
Date: Thu, 5 Nov 87 18:26:46 PST 
To: wrs@b39 
Subject: Modification to RDIST. 

Problem 1: If (SPL)FINDIT is damaged or unaccessable, RDIST waits forever
in auxSYNC.  Fix: Add timeout values to auxSYNC calls.

Problem 2: If the circuit is zapped before FINDIT finishes its report,
RDIST indicates the remaining files as being missing on slave, and needlessly
TELECOpies the rest of the directory.

File 1)	DSK:RDIST.3	created: 2229 14-SEP-87
File 2)	DSK:RDIST.SAI	created: 1824 05-NOV-87

1)1	require '1 lsh 24 lor '003 version;	comment version 1(3);
1)	require "(SAILIB)SAIL.DEF" source!file;
****
2)1	require '1 lsh 24 lor '004 version;	comment version 1(4);
2)	require "(SAILIB)SAIL.DEF" source!file;
**************
1)6	    auxSync( #lf& "Find file: " );
1)	    auxOut( NAMES&#cr );
1)	    auxSync( #lf );
1)	    while true do case auxRead( S, datum(TIMEOUT) ) of begin
****
2)6	    auxSync( #lf& "Find file: ", 60 );
2)	    auxOut( NAMES&#cr );
2)	    auxSync( #lf, 30 );
2)	    while true do case auxRead( S, datum(TIMEOUT) ) of begin
**************
1)9	    if not props(LISTONLY) then print( "Checking host ", HOST, crlf );
****
2)9	    boolean Zapped;
2)	    if not props(LISTONLY) then print( "Checking host ", HOST, crlf );
**************
1)9	    auxZap;
1)	      foreach SIV | SIV in MissingSet do begin
****
2)9	    Zapped _ !lh(auxPort);	! Set flag if premature zap;
2)	    auxZap;
2)	    if Zapped then print ("Check incomplete for host ", HOST, crlf) else
2)	      foreach SIV | SIV in MissingSet do begin
**************

For your info; in case you want to add this to version 103.    /Joe
    