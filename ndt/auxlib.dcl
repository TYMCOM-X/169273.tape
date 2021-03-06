COMMENT
Global variables:
 AUXTRAce	set to true to trace the circuit traffic coming in
 AUXPORt	The port used by these routines for the i/o
 AUXDEAd	for saving procedure to call on a zap (itemvar)

 <is.real>	:= CHECKPort
 <port>		:= CREAUX ( "user:sysno" )
 <character>	:= AUXIN ( timeout!in!seconds(0) )
		   AUXOUT ( "text to stuff down the pipe" )
 <why.done>	:= AUXREAd ( @"get.text", timeout!in!secs(0) )
 <is.in.sync>	:= AUXSYNc ( "sync.string", timeout!in!secs(0) )
		   AUXZAP
 <octal_port_list_owned_by_job>	:= AUXLISt;
;

external boolean AUXTRACE
;			COMMENT set to enable character trace on input;
external integer AUXPORT
;			COMMENT port number, or lh<>0 if port gone;
external itemvar AUXDEAD
;			COMMENT assign procedure to call on port zap;


external integer procedure CREAUX ( string user!name!colon!system );
external boolean procedure CheckPort;
external	 procedure AuxOut ( string S );
external integer procedure AuxIn ( integer timeout!in!seconds(0) );
external	 procedure AuxZap;
external boolean procedure AuxSync ( string sync
				; integer timeout!in!seconds(0) );
external integer procedure AuxRead ( reference string TEXT
				; integer timeout!in!seconds(0) );
external string  procedure AuxList;
external	 procedure port!set( integer port(0) );

COMMENT return values for AuxRead: ;
define	Aux!Zap = 0
,	Aux!Line = 1
,	Aux!Prompt = 2
,	Aux!Timeout = 3
;



  