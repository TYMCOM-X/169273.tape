  define fsm#tran0=4, fsm#recog0=5;
record!class buffer( integer room, bytePointer; r!p(buffer) next );
  define #room=1, #bytptr=2, #next=3;

r!p(buffer) forInterrupt;	! buffer(s) currently for output;
integer auxArg;			! for ease/speed: holds port,,!axI8S;
string	forceOutput;		! call forceStore to overwrite this;
				! and forceRead to look at it;
integer	forcePending;	! used by causeInputInterrupt / byteShovel;
! byteShovel sometimes needs an interrupt at a Nice, clean time ;
! (eg: no STRING GC active). it does this by making a deferred ;
! interrupt procedure (cause...) cause the interrupt that normally only;
! happens on character arrival and such to go off (ie a good time);
! this variable is to keep too many from being done (-1 if no pending);

safe integer array matchB[0:8]; ! block for DFR1IN of match d-interrupt;

r!p(fsm) WatchThis	! machine for the search;
;
integer	recogState	! current state of the FSM (for interrupt);
,	matchPoints	! incremented once per recognition (serial #);
,	matchRoom	!-1 per entry in "match" queue, +1 when gotten;
;			! Init to total interrupts allowed in pipe - 1;
integer safe array causeBlock[0:2]	! 0: DFR1IN ptr for cause;
;
r!p(buffer) onMatch	! so linked list doesn't disappear on a match;
;			! !!! tricky to set/clear, look out;


PROCEDURE byteShovel;
IF NOT currentBuffer THEN RETURN ELSE
WHILE TRUE
   DO BEGIN "trystr"	LABEL leave;
			INTEGER room;

		PROCEDURE xDefer;
		BEGIN
		 IF increment( forcePending ) LEQ 0 AND causeBlock[0]
		  THEN DFR1IN( location( causeBlock[0] ) );
		 GO leave;
		END;

		PROCEDURE getRoom;
		BEGIN
		 WHILE currentBuffer_ currentBuffer:next
		   DO IF 0 < room_ currentBuffer:room THEN RETURN;
		 xDefer;
		END;

	IF 0 GEQ room_ currentBuffer:room THEN getRoom;
	IF NOT clearlyNull
	 THEN IF inStrngGc 
		THEN xDefer
		ELSE BEGIN
			WHILE LENGTH( forced )
			   DO BEGIN	integer slice;
				slice_ room MIN LENGTH( forced );
				store( forced[1 FOR slice] );
				forced_ forced[ slice+1 TO INF ];
				IF 0 GEQ decrement( Room, slice )
				 THEN getRoom
				 ELSE currentBuffer:room_ room;
			      END;
			makeClearlyNull;
		      END;
	IF auxArg=0 OR matchRoom<0 THEN RETURN;
	<< set up FSM ACs >>
	WHILE reads( Char )
	   DO BEGIN "a char"
		store( Char );
		IF recognize( Char )
		 THEN BEGIN "found"
			currentBuffer:room_ decrement( Room );
			IF NOT onMatch THEN onMatch_ currentBuffer;
			<< set down FSM ACs >>
			increment( matchPoints );
			deferBlock[1]_ topTarget;
			deferBlock[2]_ currentBuffer;
			deferBlock[3]_ currentBuffer:Room;
			deferBlock[4]_ currentBuffer:BytePointer;
			deferBlock[5]_ matchPoints;
			decrement( matchRoom );
			DFR1IN( location( deferBlock[0] ) );
			CONTINUE "tryStr";
		      END "found";
		IF 0 GEQ decrement( Room )
		 THEN BEGIN << set down FSM ACs >> ; getRoom; END;
	      END "a char";
	<< set down FSM ACs >>
	currentBuffer:room_ room;
	.AXLDE;
LEAVE:	RETURN;
      END "tryStr";

    