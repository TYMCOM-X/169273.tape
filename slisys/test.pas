PROGRAM INFLATION;
	CONST
		N=10;
	VAR
		I:INTEGER; W,X,Y:REAL;
	BEGIN
	I:=0; W:=1.0; X:=1.0; Y:=1.0;
	REPEAT	I:=I+1;
		W:=W*1.07;
		X:=X*1.08;
		Y:=Y*1.10;
		WRITELN(I,W,X,Y)
	UNTIL I=N
	END.
   