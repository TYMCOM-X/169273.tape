PROGRAM TEST47 OPTIONS DUMP;

(*  TEST OF SELECTED STANDARD FUNCTIONS.  OPERATOR GENERATION, FOLDING,
    AND ERROR CHECKING ARE TESTED FOR THE FOLLOWING FUNCTIONS:

	ABS
	ODD
	EOLN
	EOF
	TRUNC
	ROUND
	ORD
	CHR
	SUCC
	PRED								*)

TYPE COLOR = ( RED, GREEN, BLUE );

PROCEDURE P;
VAR I: INTEGER;
    F: 1 .. 100 BY 0.01;
    R: REAL;
    P: BOOLEAN;
    C: CHAR;
    S: COLOR;
    B: FILE OF BOOLEAN;
BEGIN

    (*  THE FOLLOWING CALLS ARE ALL LEGAL  *)

    I := ABS (I);
    F := ABS (F);
    R := ABS (R);
    P := ODD (I);
    P := EOLN;
    P := EOLN (TTY);
    P := EOF;
    P := EOF (TTY);
    I := TRUNC (F);
    I := ROUND (F);
    I := TRUNC (R);
    I := ROUND (R);
    I := TRUNC (I); (* THIS IS A SPECIAL CASE *)
    I := ROUND (I); (* THIS IS A SPECIAL CASE *)
    I := ORD (C);
    I := ORD (P);
    I := ORD (S);
    I := ORD (I); (* THIS IS A SPECIAL CASE *)
    C := CHR (I);
    I := SUCC (I);
    C := SUCC (C);
    P := SUCC (P);
    S := SUCC (S);
    I := PRED (I);
    C := PRED (C);
    P := PRED (P);
    S := PRED (S);

    (*  THE FOLLOWING CALLS HAVE INCORRECT NUMBERS OF PARAMETERS  *)

    I := ABS;
    I := ABS (I,C);
    P := ODD ();
    P := ODD (I,C);
    P := EOLN (INPUT,TTY);
    P := EOF (OUTPUT,TTY);
    I := TRUNC ();
    I := TRUNC (F,R);
    I := ROUND;
    I := ROUND (F,R);
    I := ORD ();
    I := ORD (I,J);
    C := CHR;
    C := CHR (I,I);
    C := SUCC;
    C := SUCC (P,I);
    P := PRED;
    P := PRED (I,C);

    (*  THE FOLLOWING CALLS HAVE PARAMETERS OF UNACCEPTABLE TYPES  *)

    I := ABS (P);
    P := ODD (S);
    P := ODD (R);
    P := EOLN (B);
    P := EOLN (F);
    P := EOF (P);
    I := TRUNC (S);
    I := ROUND (C);
    I := ORD (F);
    I := ORD (B);
    C := CHR (P);
    R := SUCC (R);
    F := PRED (F);

END (* P *);

CONST

    (*  THE FOLLOWING CALLS CAN ALL BE FOLDED  *)

    C1 = ABS (-7);
    C2 = ABS (6);
    C3 = ODD (3);
    C4 = ODD (6);
    C5 = ORD ('A');
    C6 = ORD (TRUE);
    C7 = ORD (RED);
    C8 = CHR (64);
    C9 = SUCC (FALSE);
    C10 = SUCC (3);
    C11 = PRED (BLUE);
    C12 = PRED ('B');

    (*  THE FOLLOWING CONSTANT FUNCTION CALLS CONTAIN MISCELLANEOUS ERRORS  *)

    D1 = SUCC (TRUE);
    D2 = PRED (RED);

BEGIN END.
    