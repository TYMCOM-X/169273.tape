PROGRAM file_input;
(*copy a file to temporary*)

CONST tmp_fil = '###DAT.TMP';

VAR inam, onam: FILE_NAME; lin: STRING[132]; lincnt: INTEGER;

BEGIN
  OPEN(TTY);
  REWRITE(TTYOUTPUT);
  WRITE(TTYOUTPUT, 'Enter input file name: '); BREAK(TTYOUTPUT);
  READLN(TTY); READ(TTY, inam);
  onam := tmp_fil;
  RESET(INPUT, inam);
  IF IOSTATUS = IO_OK THEN BEGIN
    REWRITE(OUTPUT, onam);
    IF IOSTATUS = IO_OK THEN BEGIN
      lincnt := 0;
      WHILE NOT EOF DO BEGIN
        READLN(lin); WRITELN(lin);
	IF EOPAGE THEN PAGE;
	lincnt := lincnt + 1
      END;
      WRITE(TTYOUTPUT, FILENAME(INPUT), ' ==> ', FILENAME(OUTPUT));
      WRITELN(TTYOUTPUT, ' ', lincnt: 0, ' lines');
      CLOSE(OUTPUT)
    END
    ELSE WRITELN(TTY, '? can not open output file: "', onam, '"');
    CLOSE(INPUT)
  END
  ELSE WRITELN(TTY, '? can not open input file: "', inam, '"')
END.
 