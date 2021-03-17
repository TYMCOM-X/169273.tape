PROGRAM TEST81 OPTIONS DUMP;

(*  RECORD CONSTRUCTORS  *)

TYPE
    R = RECORD
	F1, F2: CHAR;
	CASE F3: INTEGER OF
	    1,3,7..10,20:
		( F4A,F5A: STRING [5] );
	    2,4,5,11..15:
		( F4B,F5B: INTEGER );
	    0:
		( F4C: CHAR;
		  CASE CHAR OF
		    '0'..'9': (F6A:INTEGER);
		    'A'..'Z': (F6B: CHAR);
		    OTHERS: (F6C:BOOLEAN))
    END;

VAR X: R;

BEGIN
    X := ('A','B',6);
    X := ('0','1',1,'MINIMAX','ALPHA');
    X := ('','LEN',8,'FOR','FIL');
    X := (' ',' ',2,3,7);
    X := ('+','-',0,'&','0',20);
    X := ('*','/',0,'','Z','X');
    X := ('$','$',0,'$','$',TRUE);
END.
