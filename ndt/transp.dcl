COMMENT		transport medium code	TRANSP
;

external integer procedure X!BLOCK( integer data!class; string data );
COMMENT	Queue a block for transmission to the other end.  This may
	wait for some response from the other end to allow room in the 
	pipe.  X!BLOCK returns a serial number of the block for use 
	with X!AWAIT to insure reception by the far end.
;

external procedure X!AWAIT( integer block!identifier );
COMMENT	Returns only after the identfied block has been acknowledged.
	This allows synchronization between two ends of the circuit.
	X!AWAIT(X!BLOCK(c, s)) waits for that message to get there.
	s# := X!BLOCK(c,s): X!BLOCK(c1,s1): X!BLOCK(c2,s2): X!AWAIT(s#)
	Will send three blocks and wait for the first one to be 
	received.  [Note: some implementations may wait for all]
;

COMMENT	You provide R!BLOCK(data!class, "data") which is called once
	for each successful transmission block received.
;

external boolean procedure do!some!input( boolean allow!input!wait );
COMMENT	should be pretty obvious: read from source and call R!BLOCK
	as noticed. allow!input!wait on to do blocking I/O.
;
 