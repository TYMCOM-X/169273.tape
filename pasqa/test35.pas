PROGRAM TEST28 OPTIONS DUMP;

 TYPE
   INT = 0..25;
   R = RECORD
	 F1, F2: 0..4;
	 CASE B3: BOOLEAN OF
	   FALSE:( C: CHAR;
		   CASE INT OF
		     1, 12..20: ( XYZ: STRING );
		     OTHERS: ( S: PACKED ARRAY[1..4] OF CHAR ) )
       END;

  VAR P: R;
      B: BOOLEAN;


BEGIN
  P := (0,4,TRUE);
  P := (1,3,FALSE,'Z',1,'');
  P := (2,5,FALSE,'',20,'ALPHA','BETA');
  P := (4,4,B,'A');
  P := (0,0);
  P := (1,3,TRUE,FALSE);
  P := (2,2,FALSE,'A',0,'ALPHABET');
END.
