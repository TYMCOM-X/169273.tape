0010?	external record!pointer (fsm) procedure makFsm( integer array equivs;
   0020?				reference record!pointer (any!class) find );
0030?	
  0040?	external record!pointer (any!class) procedure useFsm(
    0050?				record!pointer (fsm) state;
  0060?				reference integer count, bytepointer );
0070?	
  0080?	external record!pointer (any!class) procedure useFst(
    0090?				record!pointer (fsm) state;
  0100?				reference string datastr );
      ?  0110?	external r!p (fsm) procedure mksfsm( integer array equivs;
    0120?				ref# set targetSet; boolean usePnames );
    0125?	
                                          0130?	external set procedure ussFsm( r!p (fsm) state;
0140?				ref# integer count, bytepointer );
0160?	
  0170?	external set procedure ussFss( r!p (fsm) state; ref# string dataStr );
  