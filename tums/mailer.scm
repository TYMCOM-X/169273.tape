File 1)	DSK:MAILER.BAK	created: 1332 30-SEP-87
File 2)	DSK:MAILER.SAI	created: 1531 21-NOV-88

1)1	require !xwd( '1,'11 ) version;
1)	! v1.11	30-Sep-87 WRS	discard expired postmaster messages;
****
2)1	require !xwd( '1,'12 ) version;
2)	! v1.12 21-Nov-88 JMS	fix garbage from "?System date error" message;
2)	! v1.11	30-Sep-87 WRS	discard expired postmaster messages;
**************
1)8	    UFDname := cv6str(L['27])& cv6str(L['30]);
1)	    start!code
****
2)8	    UFDname := if L['27] then cv6str(L['27])&cv6str(L['30]) else cv6str(PPN);
2)	    start!code
**************
1)9	    if AGE > EXPIRE then
1)		cprint( logChan, 
****
2)9	    if AGE > EXPIRE then begin
2)		checkLogout( UFD[0] );		! Set up UFDname;
2)		cprint( logChan,
**************
1)9		    "?System date = ",TODAY,", File date = ",!lh(UFD[2])& crlf );
1)	    if AGE = EXPIRE then begin
****
2)9		    "?System date = ",TODAY,", File date = ",!lh(UFD[2]), crlf );
2)	    end;
2)	    if AGE = EXPIRE then begin
**************
   