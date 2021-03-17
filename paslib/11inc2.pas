$OPTIONS SPECIAL, NOCHECK

CONST
    SIZEOFFILEBLOCK := 53B;
    OFFSET1 := 1B;
    CORRECT1 := 25B;
    OFFSET2 := 7B;
    CORRECT2 := 11B;
    OFFSET3 := 11B;
    CORRECT3 := 14B;
    OFFSET4 := 13B;
    CORRECT4 := 14B;
    OFFSET5 := 51B;
    CORRECT5 := 25B;

TYPE
    HALFWORD = 0..777777B;
    FAKEFILEBLOCK = PACKED ARRAY[0..SIZEOFFILEBLOCK] OF HALFWORD;

EXTERNAL VAR
    INPUT: FAKEFILEBLOCK;

PUBLIC PROCEDURE SAVEINPUT (VAR BLOCK: FAKEFILEBLOCK);

BEGIN
  BLOCK := INPUT;
END;

PUBLIC PROCEDURE INITINPUT (VAR BLOCK: FAKEFILEBLOCK);

VAR
    ADDR: HALFWORD;

BEGIN
  INPUT := BLOCK;
  ADDR := ORD(ADDRESS(INPUT));
  INPUT[OFFSET1] := ADDR+CORRECT1;
  INPUT[OFFSET2] := ADDR+CORRECT2;
  INPUT[OFFSET3] := ADDR+CORRECT3;
  INPUT[OFFSET4] := ADDR+CORRECT4;
  INPUT[OFFSET5] := ADDR+CORRECT5;
END;

PUBLIC PROCEDURE OLDINPUT (BLOCK: FAKEFILEBLOCK);

BEGIN
  INPUT := BLOCK;
END.
