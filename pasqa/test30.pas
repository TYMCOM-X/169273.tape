PROGRAM TEST30 OPTIONS DUMP;
VAR P1, P2: PROCEDURE ( BOOLEAN; VAR BOOLEAN);
    F1, F2: FUNCTION ( BOOLEAN ): CHAR;

  PROCEDURE PROC1 ( B1: BOOLEAN;  VAR B2: BOOLEAN );
  BEGIN
  END;

  PROCEDURE PROC2 ( VAR B1: BOOLEAN; VAR B2: BOOLEAN );
  BEGIN
  END;

  FUNCTION FUNC1 ( B1: BOOLEAN ): CHAR;
  BEGIN
    FUNC1 := 'X';
  END;

  FUNCTION FUNC2 ( B1: BOOLEAN ): CHAR;
  BEGIN
    IF B1
      THEN FUNC2 := 'X'
      ELSE FUNC2 := 'Y';
    F1 := FUNC1;
    F2 := FUNC2;
  END;

BEGIN
  P1 := PROC1;
  P2 := PROC2;
  WRITELN (FUNC1(TRUE),FUNC2(FALSE));
  WRITELN (F1(TRUE),F2(FALSE));
END.
 