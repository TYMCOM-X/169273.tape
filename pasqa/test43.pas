PROGRAM TEST43 OPTIONS DUMP;

(*  OUT-OF-RANGE SCALAR CONSTANTS AND DIVISION BY ZERO  *)

TYPE
    T1 = 0 .. 10;
    T2 = 5 .. 15;

CONST
    C1 = 5 DIV 3;
    C2: T1 = C1;
    C3: T2 = C2;
    C4: T1 = 11;
    C5 = C1 DIV 0;

BEGIN  END.
