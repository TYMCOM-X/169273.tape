
Simple procedure VerifySlot( Integer JobSlot );
begin "verify"

    if ( QRS( JobSlot ) )		! anything in slot? ;
     then begin "valid slot"

	arrclr( Rec );			! clear record and set sequence ;
	Dpb( Seq_ LPStat( QRS(JobSlot) ), PPStat(Rec) );

	if ( GetRec( Rec[E!Runtime] ) )	! does it exist in the queue? ;
	 then begin "check uid"		! yes, so check it out ;

	    if ( QRU(JobSlot) = Gettab(QRF(JobSlot),!gtUID) )
	     then return;		! if UID = job's UID, return ok ;

	    ReSchedule( JobSlot, Sys$Abort );

	 end "check uid"
	 else begin "invalid data"	! nothing in slot, free up seq # ;

	    Prune( Seq );		! prune JobPri tree ;

	    SlotAdr_ QueRun + E!Length * ( JobSlot - 1 );
	    Memory[SlotAdr]_ 0;		! clear out slot ;
	    ArrBlt( Memory[SlotAdr+1], Memory[SlotAdr], E!Length );

	    QRS(JobSlot)_ QRU(JobSlot)_ 0;	! cleanup database ;

	 end "invalid data";

     end "valid slot";

end "verify";

   