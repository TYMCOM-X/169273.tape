PROGRAM TEST16 OPTIONS DUMP;

VAR A1: ARRAY [1..10] OF CHAR;
    A2: ARRAY ['A'..'J',1..10] OF STRING [10];
    C1, C2: CHAR;
    I, J: 1 .. 10;

BEGIN
    A1 := A2 [C1];
    A1 [I] := 'C';
    A2 [A1[J],3] := 'ABCDEF';
    A2 [A2[C2,3,3],J] := '';
    C2 := A2 ['A',1,3:5];
    C2 := A2 ['A',1,3:5,7];
    C2 := C1 [3];
    C2 := A1 ['A'];
    C2 := A1 [1,3];
END.
 