PROGRAM TEST45 OPTIONS DUMP;

(*  CONSTANT SUBSTRING TEST  *)

CONST
    C1 = '0123456789';
    C2 = 'ALPHABETICAL';
    C3 = 'MISCELLANEOUS';
    C4 = ' ';

    D1 = C1 [5];
    D2 = C1 [3:4];
    D3 = C2 [1:5] || C3 [7:7];
    D4 = C2 || C4 || C3;
    D5 = C1 [1:10];
    D6 = C1 [11:0];

    E1 = C1 [0];
    E2 = C1 [11];
    E3 = C1 [6:6];

BEGIN END.
 