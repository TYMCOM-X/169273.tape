
require "  XEXINT - XEXEC Interrupt Handler " message;

define
	Int!ZAP      = 10	! channel for zapper interrupt ;
,	Int!LOS      = 12	! channel for lost chars interrupt ;
,	Int!IOW      = 14	! channel for i/o wait interrupt ;
,	Int!ROM      = 16	! channel for i/o room interrupt ;
,	Int!CHR      = 18	! channel for character interrupt ;
,	Int!ORG      = 20	! channel for orange ball interrupt ;

,	Int!TIM      = 30	! channel for timer (SETTIM) interrupt ;
,	Int!NTQ      = 32	! channel for notice to quit ;
,	Int!ESC      = 32	! channel for attention interrupt ;
;


External boolean	comment interrupt causes  ;
    NTQ!		comment notice-to-quit ;
,   TIM!		comment SETTIM timer ;
,   CHR!		comment break character ;
,   IOW!		comment i/o wait ;
,   ROM!		comment i/o room available ;
,   LOS!		comment characters lost ;
,   ORG!		comment orange balls ;
,   ZAP!		comment circuit zaps ;
;

External simple procedure IntMak( integer intChan );
External simple procedure intZap( integer Port );
External simple procedure intPrt( integer Port );
External simple procedure intIni;
External simple procedure intDis;

require "XEXINT" library;
