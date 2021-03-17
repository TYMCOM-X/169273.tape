external boolean auxTrace;	comment set to enable character trace on input;
external boolean auxEcho;	comment if need local echo;
external boolean auxRNC;	comment set to ignore controls on auxRead;
external integer auxPort;	comment port number, or lh<>0 if port gone;
external itemvar auxDEAD;	comment assign procedure to call on port zap;
external string auxUser;	comment username;
external string auxHost;	comment host;
external string auxError;	comment error message;
external integer procedure CREaux (
    string LogString;
    boolean ErrorOkay(false) );
external integer procedure GETLCH( integer PORT );
external boolean procedure CheckPort;
external procedure auxOut( string S );
external boolean procedure AuxWait( integer TIMEOUT(0) );
external integer procedure auxIn (
    integer TIM(0) );
external procedure auxZap;
external boolean procedure auxSync (
    string CHS;
    integer TIM(0) );

define aux!Zap = 0;
define aux!Line = 1;
define aux!Prompt = 2;
define aux!Timeout = 3;
external integer procedure auxRead (
    reference string S;
    integer TIM(0), PTIM(3) );
