PROCEDURE PROGRAMHEADING (*$Y+*);

VAR
    CP: CTP;
    GLOBALSIZE: ADDRRANGE;
    FILEINX: 0..4;
    FILES: ARRAY [1..4] OF ALFA;

BEGIN
  INSYMBOL;
  GLOBALSIZE := DAPADDR + 2*MAXFILES + 6 (* SPACE FOR LUNTAB AND TTY IOSB *);
  MAIN := MAIN AND NOT OFFSWITCH['M'] OR ONSWITCH['M'];
  IF DEBUG THEN
    ONSWITCH['D'] := TRUE
  ELSE
    OFFSWITCH['D'] := TRUE;
  IF FREQUENCE THEN
    ONSWITCH['Q'] := TRUE
  ELSE
    OFFSWITCH['Q'] := TRUE;
  FILES[1] := 'INPUT     ';
  FILES[2] := 'OUTPUT    ';
  FILES[3] := 'TTY       ';
  FILES[4]:= 'TTYOUTPUT ';
  INPUTPTR := NIL;
  OUTPUTPTR := NIL;
  TTYINPTR := NIL;
  TTYOUTPTR := NIL;
  IF SY=IDENT THEN BEGIN (* CHECK FOR AND EAT PROGRAM/MODULE HEADING *)
    IF ID='PROGRAM   ' THEN BEGIN
      MAIN:= TRUE;
      INSYMBOL;
      IF SY=IDENT THEN PSECT:= ID
      ELSE ERROR(2);
      INSYMBOL;
      IF SY=LPARENT THEN BEGIN (* EAT PARAMETERLIST *)
	REPEAT
	  INSYMBOL;
	  IF SY=IDENT THEN INSYMBOL
	UNTIL SY<>COMMA;
	IF SY=RPARENT THEN INSYMBOL
	ELSE ERROR(4)
      END;
      IF SY<>SEMICOLON THEN ERROR(14)
      ELSE INSYMBOL
    END
    ELSE IF ID='MODULE    ' THEN BEGIN
      MAIN:= FALSE;
      INSYMBOL;
      IF SY<>IDENT THEN ERROR(2);
      INSYMBOL;
      IF SY<>SEMICOLON THEN ERROR(14)
      ELSE INSYMBOL
    END
  END;
  FILEINX:= 4; (* DEFINE STANDARD FILES *)
  REPEAT
    NEW(CP,VARS);
    WITH CP^ DO BEGIN
      NAME:= FILES[FILEINX];
      IDTYPE := TEXTPTR;
      SELFCTP := 0;
      VKIND := ACTUAL;
      VCLASS := DEFAULTSY;
      NEXT := NIL;
      GLOBALSIZE := GLOBALSIZE + FILESIZECORR + TEXTBUFFSIZE + 4;
      IF FILEINX > 2 THEN
      GLOBALSIZE := GLOBALSIZE - FDBSIZE;
      VADDR := GLOBALSIZE - 2;
      ENTERID(CP);
      CASE FILEINX OF
	1:
	  INPUTPTR := CP;
	2:
	  OUTPUTPTR := CP;
	3:
	  TTYINPTR := CP;
	4:
	  TTYOUTPTR := CP
      END;
      FILEINX := FILEINX - 1;
    END (* WITH CP^ *);
  UNTIL FILEINX < 1;
  DATASIZE := GLOBALSIZE ;
  IF FREQUENCE AND (OUTPUTPTR = NIL) THEN
    ERROR(930);
  NOIO := NOIO AND NOT OFFSWITCH['N'] OR ONSWITCH['N'];
  EXTSET := EXTSET AND NOT OFFSWITCH['E'] OR ONSWITCH['E'];
  FLTSET := FLTSET AND NOT OFFSWITCH['G'] OR ONSWITCH['G'];
  FPPUNIT:=FPPUNIT AND NOT OFFSWITCH['F'] OR ONSWITCH['F'];
  PRCODE := PRCODE AND NOT OFFSWITCH['C'] OR ONSWITCH['C'];
  LIST := LIST AND NOT OFFSWITCH['L'] OR ONSWITCH['L'];
  WARNINGS := WARNINGS AND NOT OFFSWITCH['W'] OR ONSWITCH['W'];
  PSECTGEN := PSECTGEN AND NOT OFFSWITCH['Y'] OR ONSWITCH['Y'];
  CONDCOMP := CONDCOMP AND NOT ONSWITCH['X'];
  HEAPCHECK := HEAPCHECK AND NOT OFFSWITCH['T'] OR ONSWITCH['T'];
  FREQUENCE := (FREQUENCE AND NOT OFFSWITCH['Q'] OR ONSWITCH['Q'])
               AND NOT NOIO;
  RUNTMCHECK := RUNTMCHECK AND NOT OFFSWITCH['R'] OR ONSWITCH['R'];
  EXTSET := EXTSET OR FLTSET OR FPPUNIT ;
  TRACE := TRACE AND NOT OFFSWITCH['S'] OR ONSWITCH['S'];
  IF FREQUENCE OR TRACE OR DEBUG THEN BEGIN
    HEAPCHECK := TRUE;
    ONSWITCH['T'] := TRUE
  END;
  IF DEBUG THEN BEGIN
    PSECTGEN := FALSE;
    OFFSWITCH['Y'] := TRUE;
    ONSWITCH['Y'] := FALSE;
  END;
  OFFSWITCH['D'] := NOT ONSWITCH['D'];
  OFFSWITCH['S'] := NOT ONSWITCH['S'];
  OFFSWITCH['Q'] := NOT ONSWITCH['Q'];
  LASTLINE.LLADDR := 0;
  LASTLINE.LLPSECT := PSECT;
  IF FREQUENCE THEN
    ONSWITCH['Q'] := TRUE;
  IF DEBUG THEN
    LC := -202 (*WORKING SPACE FOR DEBUGGER*);
END (* PROGRAMHEADING *);

PROCEDURE ENTERSTANDARD (*$Y+*);

CONST
    NUMBERNAMES = 61;
    CONSTOFFSET = 48;	(* INDEX IN NAME TABLE TO FIRST CONSTANT *)
    IOCONSTOFFSET = 54; (* INDEX IN NAME TABLE TO FIRST IO CONSTANT *)
    FIRSTPROC = 5;	(* INDEX TO FIRST STANDARD PROC NAME *)
    LASTPROC = 23;	(* INDEX TO LAST PROC *)
    PROCBREAK = 19;	(* INDEX OF FIRST PROC FOR PREDECLARATION LEVEL >= 1 *)
    FIRSTFUNC = 24;
    LASTFUNC = 47;
    FUNCBREAK = 37;

VAR
    CP,CP1: CTP;
    I: INTEGER;
    LVP: CSP;
    SP: STP;
    NA: ARRAY [1..NUMBERNAMES] OF ALFA;
    VAL: ARRAY [0..8] OF INTEGER;

BEGIN (*TYPE UNDERLIEING:*)
(* ENTER STANDARD TYPES *)
(************************)
(* INITPROCEDURE *) (*STANDARDNAMES*)
  BEGIN
    NA[ 1] := 'FALSE     ';
    NA[ 2] := 'TRUE      ';
    NA[ 3] := 'INPUT     ';
    NA[ 4] := 'OUTPUT    ';
    NA[ 5] := 'GET       ';
    NA[ 6] := 'PAGE      ';
    NA[ 7] := 'PUT       ';
    NA[ 8] := 'BREAK     ';
    NA[ 9] := 'RESET     ';
    NA[10] := 'REWRITE   ';
    NA[11] := 'READ      ';
    NA[12] := 'READLN    ';
    NA[13] := 'WRITE     ';
    NA[14] := 'WRITELN   ';
    NA[15] := 'NEW       ';
    NA[16] := 'MARK      ';
    NA[17] := 'RELEASE   ';
    NA[18] := 'DISPOSE   ';
    NA[19] := 'HALT      ';
    NA[20] := 'PACK      ';
    NA[21] := 'UNPACK    ';
    NA[22] := 'DATE      ';
    NA[23] := 'TIME      ';
    NA[24] := 'ABS       ';
    NA[25] := 'SQR       ';
    NA[26] := 'TRUNC     ';
    NA[27] := 'ODD       ';
    NA[28] := 'ORD       ';
    NA[29] := 'CHR       ';
    NA[30] := 'PRED      ';
    NA[31] := 'SUCC      ';
    NA[32] := 'EOF       ';
    NA[33] := 'EOLN      ';
    NA[34] := 'IORESULT  ';
    NA[35] := 'ROUND     ';
    NA[36] := 'RUNTIME   ';
    NA[37] := 'SPLITREAL ';
    NA[38] := 'TWOPOW    ';
    NA[39] := 'SIN       ';
    NA[40] := 'COS       ';
    NA[41] := 'ARCTAN    ';
    NA[42] := 'EXP       ';
    NA[43] := 'LN        ';
    NA[44] := 'SQRT      ';
    NA[45] := 'SIZE      ';
    NA[46] := 'PTR       ';
    NA[47] := 'ADDRESS   ';
    NA[48] := 'ALFALENG  ';
    NA[49] := 'MAXINT    ';
    NA[50] := 'MININT    ';
    NA[51] := 'MAXREAL   ';
    NA[52] := 'SMALLREAL ';
    NA[53] := 'MINREAL   ';
    NA[54] := 'RANDOM    ';
    NA[55] := 'UPDATE    ';
    NA[56] := 'APPEND    ';
    NA[57] := 'TEMPORARY ';
    NA[58] := 'INSERT    ';
    NA[59] := 'SHARED    ';
    NA[60] := 'SPOOL     ';
    NA[61] := 'BLOCK     ';
    VAL[0] := ALFALENG;
    VAL[1] := 32767;
    VAL[2] := 100000B;
    VAL[3] := 077777B;
    VAL[4] := 032400B;
    VAL[5] := 000001B;
    VAL[6] := 177777B;
    VAL[7] := 000000B;
    VAL[8] := 000000B;
  END (*STANDARDNAMES*);
  NEW(INTPTR,SCALAR,STANDARD); (*INTEGER*)
  INTPTR^.SIZE := 2;
  INTPTR^.SELFSTP := 0;
  NEW(REALPTR,SCALAR,STANDARD); (*REAL*)
  REALPTR^.SIZE := 4;
  REALPTR^.SELFSTP := 0;
  NEW(CHARPTR,SCALAR,STANDARD); (*CHAR*)
  CHARPTR^.SIZE := 2;
  CHARPTR^.SELFSTP := 0;
  NEW(BOOLPTR,SCALAR,DECLARED); (*BOOLEAN*)
  BOOLPTR^.SIZE := 2;
  BOOLPTR^.SELFSTP := 0;
  NEW(NILPTR,POINTER); (*NIL*)
  WITH NILPTR^ DO BEGIN
    ELTYPE := NIL;
    SIZE := 2;
    SELFSTP := 0;
  END;
  BEGIN
    NEW(TEXTPTR,FILES); (*TEXT*)
    WITH TEXTPTR^ DO BEGIN
      FILTYPE := CHARPTR;
      SIZE := 2;
      SELFSTP := 0
    END;
  END;


  (* ENTER STANDARD NAMES *)
  (************************)


  NEW(CP,TYPES); (*INTEGER*)
  WITH CP^ DO BEGIN
    NAME := 'INTEGER   ';
    IDTYPE := INTPTR
  END;
  ENTERID(CP);
  NEW(CP,TYPES); (*REAL*)
  WITH CP^ DO BEGIN
    NAME := 'REAL      ';
    IDTYPE := REALPTR
  END;
  ENTERID(CP);
  NEW(CP,TYPES); (*CHAR*)
  WITH CP^ DO BEGIN
    NAME := 'CHAR      ';
    IDTYPE := CHARPTR
  END;
  ENTERID(CP);
  NEW(CP,TYPES); (*BOOLEAN*)
  WITH CP^ DO BEGIN
    NAME := 'BOOLEAN   ';
    IDTYPE := BOOLPTR
  END;
  ENTERID(CP);
  NEW(CP,KONST); (*NIL*)
  WITH CP^ DO BEGIN
    NAME := 'NIL       ';
    IDTYPE := NILPTR;
    NEXT := NIL;
    VALUES.IVAL := 0
  END;
  ENTERID(CP);
  IF DEFLEVEL >= 2 THEN BEGIN
    NEW(CP,TYPES);
    WITH CP^ DO BEGIN
      NAME := 'TEXT      ';
      IDTYPE := TEXTPTR;
    END;
    ENTERID(CP);
    FOR I := 0 TO 5 DO BEGIN
      NEW(CP,KONST); (*ALFALENG*)
      WITH CP^ DO BEGIN
	NAME := NA[I+CONSTOFFSET];
	NEXT := NIL;
	IF I < 3 THEN BEGIN
	  IDTYPE := INTPTR;
	  VALUES.IVAL := VAL[I]
	END
	ELSE BEGIN
	  IDTYPE := REALPTR;
	  (*$Z+*)
	  NEW(CP1,KONST); (*SOLVE PROBLEM WITH NEW(LVP)*) (*$Z-*)
	  NEW(LVP,REEL);
	  LVP^.HEAD := VAL[I];
	  LVP^.TAIL := VAL[I+3];
	  VALUES.VALP := LVP;
	END;
      END;
      ENTERID(CP);
    END;
  END;
  IF DEFLEVEL >= 3 THEN BEGIN
    NEW(IOSPECPTR,SCALAR,DECLARED);
    IOSPECPTR^.SIZE := 2;
    NEW(CP,TYPES);
    WITH CP^ DO BEGIN
      NAME := 'IOSPEC    ';
      IDTYPE := IOSPECPTR;
    END;
    ENTERID ( CP );
    CP1 := NIL;
    FOR I := 0 TO 7 DO BEGIN
      NEW( CP,KONST );
      WITH CP^ DO BEGIN
	NAME := NA[I+IOCONSTOFFSET];
	IDTYPE := IOSPECPTR;
	NEXT := CP1;
	VALUES.IVAL := I;
      END;
      ENTERID( CP );
      CP1 := CP;
    END;
    IOSPECPTR^.FCONST := CP;
  END (* IF DEFLEVEL >= 3 *);
  IF DEFLEVEL >= 2 THEN BEGIN
    NEW( SP,SUBRANGE );
    WITH SP^ DO BEGIN
      SIZE := 2;
      SELFSTP := 0;
      RANGETYPE := CHARPTR;
      MIN.IVAL := 0;
      MAX.IVAL := 127;
    END;
    NEW( CP,TYPES );
    WITH CP^ DO BEGIN
      NAME := 'ASCII     ';
      IDTYPE := SP;
      SELFCTP := 0;
    END;
    NEW( SP,SUBRANGE );
    SP^ := CP^.IDTYPE^;
    SP^.MAX.IVAL := 255;
    NEW( CP1,TYPES );
    WITH CP1^ DO BEGIN
      NAME := 'BYTE      ';
      IDTYPE := SP;
      SELFCTP := 0;
    END;
    ENTERID( CP );
    ENTERID( CP1 );
  END (*  IF DEFLEVEL >= 2   *);
  CP1 := NIL;
  FOR I := 1 TO 2 DO BEGIN
    NEW(CP,KONST); (*FALSE,TRUE*)
    WITH CP^ DO BEGIN
      NAME := NA[I];
      IDTYPE := BOOLPTR;
      NEXT := CP1;
      VALUES.IVAL := I - 1
    END;
    ENTERID(CP);
    CP1 := CP
  END;
  BOOLPTR^.FCONST := CP;
  FOR I := FIRSTPROC TO LASTPROC DO
    IF (I<PROCBREAK) OR (DEFLEVEL>=1) THEN BEGIN
      NEW(CP,PROC,STANDARD); (*GET,GETLN,PUT,PUTLN,RESET*)
      WITH CP^ DO (*REWRITE,READ,READLN,WRITE,WRITELN,*)
      BEGIN
	NAME := NA[I];
	IDTYPE := NIL; (*NEW,MARK,RELEASE,DISPOSE*)
	NEXT := NIL;
	KEY := I - FIRSTPROC+1; (*SETCONTENTS*)
      END;
      (*MARK,RELEASE*)
      ENTERID(CP)
    END;
  FOR I := FIRSTFUNC TO LASTFUNC DO
    IF (I<FUNCBREAK) OR (DEFLEVEL>=1) THEN BEGIN
      NEW(CP,FUNC,STANDARD);
      WITH CP^ DO BEGIN
	NAME := NA[I];
	IDTYPE := NIL;
	NEXT := NIL;
	KEY := I - FIRSTFUNC+1;
      END;
      ENTERID(CP);
    END;
  NEW(CP,VARS); (*PARAMETER OF PREDECLARED FUNCTIONS*)
  WITH CP^ DO BEGIN
    NAME := '          ';
    IDTYPE := REALPTR;
    VKIND := ACTUAL;
    VCLASS := DEFAULTSY;
    NEXT := NIL;
    VLEV := 1;
    VADDR := 0
  END;


  (* ENTER UNDECLARED *)
  (********************)

  NEW(UTYPPTR,TYPES);
  WITH UTYPPTR^ DO BEGIN
    NAME := '          ';
    IDTYPE := NIL
  END;
  NEW(UCSTPTR,KONST);
  WITH UCSTPTR^ DO BEGIN
    NAME := '          ';
    IDTYPE := NIL;
    NEXT := NIL;
    VALUES.IVAL := 0
  END;
  NEW(UVARPTR,VARS);
  WITH UVARPTR^ DO BEGIN
    NAME := '          ';
    IDTYPE := NIL;
    VKIND := ACTUAL;
    VCLASS := DEFAULTSY;
    NEXT := NIL;
    VLEV := 0;
    VADDR := 0
  END;
  NEW(UFLDPTR,FIELD);
  WITH UFLDPTR^ DO BEGIN
    NAME := '          ';
    IDTYPE := NIL;
    NEXT := NIL;
    FLDADDR := 0
  END;
  NEW(UPRCPTR,PROC,DECLARED,ACTUAL);
  WITH UPRCPTR^ DO BEGIN
    NAME := '          ';
    IDTYPE := NIL;
    DECLPLACE := INTERNAL;
    NEXT := NIL;
    EXTNAME := NIL;
    PFLEV := 0;
    PFADDR :=0;
  END;
  NEW(UFCTPTR,FUNC,DECLARED,ACTUAL);
  WITH UFCTPTR^ DO BEGIN
    NAME := '          ';
    IDTYPE := NIL;
    NEXT := NIL;
    DECLPLACE := INTERNAL;
    EXTNAME := NIL;
    PFLEV := 0;
    PFADDR := 0;
  END;
  (* ADJUST INPUT ETC BECAUSE PRORAMHEADING IS CALLED BEFORE ENTERST *)
  IF INPUTPTR <> NIL THEN
    INPUTPTR^.IDTYPE := TEXTPTR;
  IF OUTPUTPTR <> NIL THEN
    OUTPUTPTR^.IDTYPE := TEXTPTR;
  IF TTYINPTR <> NIL THEN
    TTYINPTR^.IDTYPE := TEXTPTR;
  IF TTYOUTPTR <> NIL THEN
    TTYOUTPTR^.IDTYPE := TEXTPTR;
END (*ENTERUNDECL*);
