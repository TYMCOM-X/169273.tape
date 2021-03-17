$OPTIONS SPECIAL, NOCHECK

CONST
    SIZEOFFILEBLOCK := 53B; (* 0..53B HALFWORDS *)

TYPE
    FILEPTR = ^FILERECORD;
    HALFWORD = 0..777777B;
    FAKEFILEBLOCK = PACKED ARRAY[0..SIZEOFFILEBLOCK] OF HALFWORD;
    FILERECORD = RECORD
      PREVIOUSFILE: FILEPTR;
      FILEBLOCK: FAKEFILEBLOCK
    END;

VAR
    CURRENTFILE: FILEPTR := NIL;
    BASICFILEBLOCK: FAKEFILEBLOCK := ( 0,0,0,0,0,0,50000B,0,76000B,0,77000B,0,
      56000B,0,57000B,0,71000B,0,0,0, 446353B,0,0,0,460000B,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,777777B,0,0,0);
    TEMPFILEBLOCK: FAKEFILEBLOCK;

EXTERNAL PROCEDURE SAVEINPUT (VAR FAKEFILEBLOCK);

EXTERNAL PROCEDURE INITINPUT (VAR FAKEFILEBLOCK);

EXTERNAL PROCEDURE OLDINPUT (VAR FAKEFILEBLOCK);

EXTERNAL PROCEDURE REPLACEINPUT (VAR FAKEFILEBLOCK);

EXTERNAL PROCEDURE RESTOREINPUT (VAR FAKEFILEBLOCK);

EXTERNAL PROCEDURE REMOVEINPUT;

PUBLIC FUNCTION INCLUDE (FILENAME: STRING): BOOLEAN;

VAR
    F: FILEPTR;

BEGIN
  SAVEINPUT (TEMPFILEBLOCK);
  INITINPUT (BASICFILEBLOCK);
  RESET (INPUT,'.PAS '||FILENAME);
  IF EOF (INPUT) THEN BEGIN
    INCLUDE := FALSE;
    RESTOREINPUT (TEMPFILEBLOCK);
  END
  ELSE BEGIN
    INCLUDE := TRUE;
    NEW (F);
    WITH F^ DO BEGIN
      PREVIOUSFILE := CURRENTFILE;
      CURRENTFILE := F;
      FILEBLOCK := TEMPFILEBLOCK;
      REPLACEINPUT (FILEBLOCK);
    END
  END
END;

PUBLIC PROCEDURE CLOSEINCLUDEFILE;

VAR F: FILEPTR;

BEGIN
  IF CURRENTFILE=NIL THEN (* SHOULD NOT OCCUR *)
  ELSE BEGIN
    CLOSE (INPUT);
    F := CURRENTFILE;
    CURRENTFILE := CURRENTFILE^.PREVIOUSFILE;
    REMOVEINPUT;
    OLDINPUT (F^.FILEBLOCK);
    RESTOREINPUT (F^.FILEBLOCK);
    DISPOSE (F);
  END;
END.
 